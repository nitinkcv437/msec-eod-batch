       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWB200.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  11/03/1997.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWB200                                            *
      * TITLE      : SWIFT INBOUND STATUS AND CONFIRMATION PARSER      *
      * JOB        : MSSWD020  STEP010                                 *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   READS THE CUSTODIAN'S ISO 15022 MESSAGES RECEIVED OVERNIGHT  *
      *   AND POSTS THEM TO THE SETTLEMENT INSTRUCTION MASTER:         *
      *     MT548  SETTLEMENT STATUS AND PROCESSING ADVICE             *
      *            :25D::MTCH//MACH / NMAT     MATCHED / UNMATCHED     *
      *            :25D::SETT//PEND / PENF     PENDING (+ :24B: REASON)*
      *            :25D::IPRC//REJT            REJECTED (+ REASON)     *
      *            :25D::IPRC//PACK            ACKNOWLEDGED (NO EVENT) *
      *            :25D::IPRC//CAND            CANCELLATION COMPLETED  *
      *     MT544-MT547  SETTLEMENT CONFIRMATIONS                      *
      *            :36B::ESTT// SETTLED QUANTITY, :19A::ESTT// AMOUNT, *
      *            :98A::ESET// EFFECTIVE SETTLEMENT DATE.  LESS THAN  *
      *            THE OPEN QUANTITY = PARTIAL SETTLEMENT (PS).        *
      *   THE INSTRUCTION IS FOUND BY THE RELATED REFERENCE            *
      *   (:20C::RELA// IN THE LINK BLOCK) = OUR SENDER REFERENCE.     *
      *   UNKNOWN REFERENCES ARE REPORTED AS EVENT UR (RC 4).          *
      *   ONE STATUS EVENT (SWSTAT) IS WRITTEN PER STATUS / CONFIRM.   *
      *                                                                *
      *   MESSAGE LAYOUT: LINE 1 '{1:...}{2:O5NN...}{4:', TAG LINES    *
      *   ':TAG:VALUE' OR ':TAG::QUAL//VALUE', BLOCKS :16R:/:16S:,     *
      *   LAST LINE '-}'.  NUMBERS USE THE ISO DECIMAL COMMA.          *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETERS SWP200A (MAXERR=NNNN)            *
      *          MSGIN     MSEC.PROD.SW.INMSG.RAW(0)      (SWMSG)      *
      * UPDATE : SWINSTR   MSEC.PROD.SW.INSTR.KSDS        (SWINSTR)    *
      * OUTPUT : STATOUT   MSEC.PROD.SW.STATEVT(+1)       (SWSTAT)     *
      * CALLS  : CMASM01 CMU050 CMU060 CMU080                          *
      *                                                                *
      * RETURN CODES: 0 CLEAN                                          *
      *               4 UNKNOWN REFERENCES, FORMAT ERRORS OR           *
      *                 UNSUPPORTED MESSAGE TYPES                      *
      *               8 MORE FORMAT ERRORS THAN MAXERR                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1997-11-03 DWB  CHG03390  ORIGINAL - ISO 15022 MIGRATION       *
      *                           (REPLACES MT530/MT532 PARSER)        *
      * 1998-11-02 TLM  CHG04471  Y2K - :98A: CCYYMMDD                 *
      * 1999-06-21 TLM  CHG05230  PARTIAL SETTLEMENT (PS)              *
      * 2001-04-09 KAP  CHG08820  DECIMALIZATION - 4 DECIMALS ON QTY   *
      * 2004-09-13 KAP  CHG12507  BLOCK TRACKING - SETTRAN BLOCK OF    *
      *                           MT548 CARRIES ITS OWN :25D: TAGS     *
      * 2006-02-06 KAP  CHG14415  CUSTODIAN SENDS WHOLE NUMBERS        *
      *                           WITHOUT COMMA - ACCEPTED             *
      * 2009-12-14 SPA  CHG19002  EUROCLEAR PENF, NEGATIVE AMOUNTS (N) *
      * 2014-05-05 SPA  CHG27115  IPRC//CAND CLOSES CANC AND ORIGINAL  *
      * 2019-05-13 MHC  CHG34410  MAXERR PARAMETER, RC 8               *
      * 2024-02-12 NVR  CHG41007  T+1 REVIEW - NO CHANGE               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD      ASSIGN TO SYSIN
                                FILE STATUS IS WS-PARMCARD-FS.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT MSGIN-FILE    ASSIGN TO MSGIN
                                FILE STATUS IS WS-MSGIN-FS.
           SELECT SWINSTR-FILE  ASSIGN TO SWINSTR
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD KEY IS SWI-SENDER-REF
                                FILE STATUS IS WS-SWINSTR-FS.
           SELECT STATOUT-FILE  ASSIGN TO STATOUT
                                FILE STATUS IS WS-STATOUT-FS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  MSGIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MSGIN-REC                   PIC X(120).
       FD  SWINSTR-FILE.
       COPY SWINSTR.
       FD  STATOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SWSTAT.
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWB200'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-MSGIN-FS             PIC X(02).
               88  MSGIN-OK                      VALUE '00'.
               88  MSGIN-EOF                     VALUE '10'.
           05  WS-SWINSTR-FS           PIC X(02).
               88  SWINSTR-OK                    VALUE '00'.
               88  SWINSTR-NOTFND                VALUE '23'.
           05  WS-STATOUT-FS           PIC X(02).
               88  STATOUT-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-INPUT               VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-COMPLETE-SW          PIC X(01)  VALUE 'N'.
               88  WS-MSG-COMPLETE               VALUE 'Y'.
           05  WS-FOUND-SW             PIC X(01)  VALUE 'N'.
               88  WS-INSTR-FOUND                VALUE 'Y'.
           05  WS-UPDATE-SW            PIC X(01)  VALUE 'N'.
               88  WS-INSTR-CHANGED              VALUE 'Y'.
           05  WS-NUM-OK-SW            PIC X(01)  VALUE 'N'.
               88  WS-NUM-OK                     VALUE 'Y'.
      *
       01  WS-MAX-ERRORS               PIC 9(04)  VALUE 0050.
       01  WS-PARM-KEYWORD             PIC X(20).
       01  WS-PARM-VALUE               PIC X(20).
      *
       COPY SWMSG.
      *
      *----------------------------------------------------------------*
      * CURRENT MESSAGE TEXT                                           *
      *----------------------------------------------------------------*
       01  WS-MESSAGE.
           05  WS-MSG-SEQ              PIC 9(08).
           05  WS-MSG-REC-TYPE         PIC X(03).
           05  WS-MSG-LINES            PIC S9(04) COMP VALUE ZERO.
           05  WS-MSG-TEXT             PIC X(105) OCCURS 80 TIMES.
       01  WS-MSG-MAX-LINES            PIC S9(04) COMP VALUE 80.
      *
      *----------------------------------------------------------------*
      * FIELDS OF THE PARSED MESSAGE                                   *
      *----------------------------------------------------------------*
       01  WS-PARSED.
           05  WS-P-MT                 PIC X(03).
           05  WS-P-CUST-REF           PIC X(16).
           05  WS-P-RELA               PIC X(16).
           05  WS-P-FUNCTION           PIC X(04).
           05  WS-P-ESET-DATE          PIC X(08).
           05  WS-P-QTY-TYPE           PIC X(04).
           05  WS-P-QTY-TEXT           PIC X(35).
           05  WS-P-AMT-TEXT           PIC X(35).
           05  WS-P-STAT-COUNT         PIC S9(04) COMP.
           05  WS-P-STAT-ENTRY         OCCURS 5 TIMES.
               10  WS-P-STAT-QUAL      PIC X(04).
               10  WS-P-STAT-CODE      PIC X(04).
               10  WS-P-STAT-REASON    PIC X(04).
       01  WS-STAT-SUB                 PIC S9(04) COMP.
      *
      *----------------------------------------------------------------*
      * TAG SPLITTING                                                  *
      *----------------------------------------------------------------*
       01  WS-TAG-WORK.
           05  WS-LINE                 PIC X(105).
           05  WS-FLD-1                PIC X(10).
           05  WS-FLD-TAG              PIC X(04).
           05  WS-FLD-3                PIC X(100).
           05  WS-FLD-4                PIC X(100).
           05  WS-FLD-COUNT            PIC S9(04) COMP.
           05  WS-QUAL                 PIC X(04).
           05  WS-QUAL-DELIM           PIC X(02).
           05  WS-PART-2               PIC X(100).
           05  WS-PART-2-DELIM         PIC X(02).
           05  WS-PART-3               PIC X(100).
           05  WS-HDR-POS              PIC S9(04) COMP.
      *
      *    BLOCK STACK (:16R: PUSH, :16S: POP)
       01  WS-BLOCK-STACK.
           05  WS-BLK-DEPTH            PIC S9(04) COMP VALUE ZERO.
           05  WS-BLK-NAME             PIC X(08) OCCURS 6 TIMES.
       01  WS-CUR-BLOCK                PIC X(08).
       01  WS-PARENT-BLOCK             PIC X(08).
      *
      *----------------------------------------------------------------*
      * DECIMAL COMMA CONVERSION                                       *
      *----------------------------------------------------------------*
       01  WS-NUM-WORK.
           05  WS-NUM-TEXT             PIC X(35).
           05  WS-NUM-NEG-SW           PIC X(01).
           05  WS-NUM-COMMAS           PIC S9(04) COMP.
           05  WS-NUM-INT-TXT          PIC X(18).
           05  WS-NUM-INT-LEN          PIC S9(04) COMP.
           05  WS-NUM-FRAC-TXT         PIC X(04).
           05  WS-NUM-FRAC-LEN         PIC S9(04) COMP.
           05  WS-NUM-INT              PIC 9(15).
           05  WS-NUM-FRAC             PIC 9(04).
           05  WS-NUM-VALUE            PIC S9(15)V9(04) COMP-3.
       01  WS-SETTLED-QTY              PIC S9(11)V9(04) COMP-3.
       01  WS-SETTLED-AMT              PIC S9(15)V99    COMP-3.
       01  WS-SETTLED-CCY              PIC X(03).
       01  WS-OPEN-QTY                 PIC S9(11)V9(04) COMP-3.
      *
      *
      *----------------------------------------------------------------*
      * CUSTODIAN REFERENCES SEEN IN THIS FILE - A MESSAGE SENT TWICE  *
      * (GATEWAY RETRANSMISSION, PDE) IS APPLIED ONCE.  HASHED TABLE.  *
      *----------------------------------------------------------------*
       01  WS-SEEN-TABLE.
           05  WS-SEEN-SLOTS           PIC S9(08) COMP VALUE 20011.
           05  WS-SEEN-USED            PIC S9(08) COMP VALUE ZERO.
           05  WS-SEEN-ENTRY           OCCURS 20011 TIMES.
               10  WS-SEEN-REF         PIC X(16).
       01  WS-HASH-WORK.
           05  WS-HASH                 PIC S9(09) COMP.
           05  WS-HASH-POS             PIC S9(04) COMP.
           05  WS-HASH-SLOT            PIC S9(08) COMP.
           05  WS-HASH-PROBES          PIC S9(08) COMP.
           05  WS-HASH-SW              PIC X(01).
               88  WS-HASH-DUPLICATE             VALUE 'D'.
               88  WS-HASH-STORED                VALUE 'S'.
      *
      *----------------------------------------------------------------*
      * SETTLED AMOUNT CHECK - DIFFERENCE TO THE INSTRUCTED AMOUNT     *
      * (PRO RATA FOR PARTIALS) ABOVE THIS FRACTION IS REPORTED        *
      *----------------------------------------------------------------*
       01  WS-AMT-TOLERANCE            PIC V9(04) VALUE .0050.
       01  WS-AMT-CHECK.
           05  WS-EXPECTED-AMT         PIC S9(15)V99    COMP-3.
           05  WS-AMT-DIFF             PIC S9(15)V99    COMP-3.
           05  WS-AMT-LIMIT            PIC S9(15)V99    COMP-3.
      *
       01  WS-EVENT-WORK.
           05  WS-EV-CODE              PIC X(02).
           05  WS-EV-STATUS-CODE       PIC X(04).
           05  WS-EV-REASON            PIC X(04).
           05  WS-EV-APPLIED           PIC X(01).
           05  WS-NEW-STATUS           PIC X(02).
           05  WS-ORIG-REF             PIC X(16).
      *
       01  WS-COUNTERS.
           05  WS-LINES-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MSGS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MSG-ERRORS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MSG-UNSUPPORTED      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-SKIPPED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CNT-548              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CNT-CONFIRM          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-WRITTEN           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-MATCHED           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-UNMATCHED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-PENDING           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-REJECTED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-SETTLED           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-PARTIAL           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-CANCELLED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-UNKNOWN           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EV-NOT-APPLIED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACKS                 PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MT-MISMATCH          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DUPLICATES           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OVER-DELIVERY        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AMT-DIFFERENCES      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FUTURE-ESET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-UPDATED        PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-SETTLED-AMT-TOTAL    PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-SETTLED-QTY-TOTAL    PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-EV-QTY-TOTAL         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-SUB                  PIC S9(04) COMP.
           05  WS-ERR-TEXT             PIC X(50).
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
      *
       COPY CMDATEW.
       COPY CMJILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-MESSAGE THRU 2000-EXIT
               UNTIL WS-END-OF-INPUT
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
      * 1000 - INITIALIZE                                              *
      *================================================================*
       1000-INITIALIZE.
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'SWIFT INBOUND STATUS PARSER STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           CALL 'CMASM01' USING JI-JOB-INFO
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM  THRU 1100-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
      *
           PERFORM VARYING WS-HASH-SLOT FROM 1 BY 1
                     UNTIL WS-HASH-SLOT > WS-SEEN-SLOTS
               MOVE SPACES TO WS-SEEN-REF (WS-HASH-SLOT)
           END-PERFORM
      *
           OPEN INPUT MSGIN-FILE
           IF NOT MSGIN-OK
               MOVE 'MSGIN'            TO AB-DDNAME
               MOVE WS-MSGIN-FS        TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN I-O SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT STATOUT-FILE
           IF NOT STATOUT-OK
               MOVE 'STATOUT'          TO AB-DDNAME
               MOVE WS-STATOUT-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           PERFORM 8000-READ-LINE      THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARM.
           READ PARMCARD
               AT END
                   SET WS-PARM-EOF     TO TRUE
                   GO TO 1100-EXIT
           END-READ
           IF PARM-CARD-REC (1:1) = '*'
           OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           IF WS-PARM-KEYWORD = 'MAXERR'
           AND WS-PARM-VALUE (1:4) NUMERIC
               MOVE WS-PARM-VALUE (1:4) TO WS-MAX-ERRORS
           ELSE
               DISPLAY 'SWB200 - PARAMETER IGNORED: '
                       PARM-CARD-REC (1:40)
           END-IF.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - COLLECT ONE MESSAGE: '{1:' ... '-}'                     *
      *================================================================*
       2000-PROCESS-MESSAGE.
      *    TEXT OUTSIDE A MESSAGE IS SKIPPED UP TO THE NEXT HEADER
           IF SWM-TEXT (1:3) NOT = '{1:'
               ADD 1                   TO WS-LINES-SKIPPED
               PERFORM 8000-READ-LINE  THRU 8000-EXIT
               GO TO 2000-EXIT
           END-IF
      *
           MOVE SWM-MSG-SEQ            TO WS-MSG-SEQ
           MOVE SWM-MSG-TYPE           TO WS-MSG-REC-TYPE
           MOVE ZERO                   TO WS-MSG-LINES
           MOVE 'N'                    TO WS-COMPLETE-SW
           PERFORM 2100-ADD-LINE       THRU 2100-EXIT
           PERFORM 8000-READ-LINE      THRU 8000-EXIT
           PERFORM UNTIL WS-MSG-COMPLETE
                      OR WS-END-OF-INPUT
                      OR SWM-MSG-SEQ NOT = WS-MSG-SEQ
                      OR SWM-TEXT (1:3) = '{1:'
               PERFORM 2100-ADD-LINE   THRU 2100-EXIT
               IF SWM-TEXT (1:2) = '-}'
                   SET WS-MSG-COMPLETE TO TRUE
               END-IF
               PERFORM 8000-READ-LINE  THRU 8000-EXIT
           END-PERFORM
      *
           ADD 1                       TO WS-MSGS-READ
           IF NOT WS-MSG-COMPLETE
               MOVE 'MESSAGE INCOMPLETE - NO -} TRAILER' TO WS-ERR-TEXT
               PERFORM 6900-FORMAT-ERROR THRU 6900-EXIT
               GO TO 2000-EXIT
           END-IF
      *
           PERFORM 3000-PARSE-MESSAGE  THRU 3000-EXIT
           IF WS-P-CUST-REF NOT = SPACES
               PERFORM 3900-CHECK-DUPLICATE THRU 3900-EXIT
               IF WS-HASH-DUPLICATE
                   ADD 1               TO WS-DUPLICATES
                   DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' MT' WS-P-MT
                           ' CUSTODIAN REF ' WS-P-CUST-REF
                           ' ALREADY RECEIVED - IGNORED'
                   GO TO 2000-EXIT
               END-IF
           END-IF
           PERFORM 4000-APPLY-MESSAGE  THRU 4000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-ADD-LINE.
           IF WS-MSG-LINES < WS-MSG-MAX-LINES
               ADD 1                   TO WS-MSG-LINES
               MOVE SWM-TEXT           TO WS-MSG-TEXT (WS-MSG-LINES)
           END-IF.
       2100-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - PARSE HEADER AND TAGS                                   *
      *================================================================*
       3000-PARSE-MESSAGE.
           INITIALIZE WS-PARSED
           MOVE ZERO                   TO WS-BLK-DEPTH
           MOVE SPACES                 TO WS-CUR-BLOCK WS-PARENT-BLOCK
      *
      *    MESSAGE TYPE FROM THE APPLICATION HEADER {2:O548...
           MOVE ZERO                   TO WS-HDR-POS
           INSPECT WS-MSG-TEXT (1) TALLYING WS-HDR-POS
               FOR CHARACTERS BEFORE INITIAL '{2:'
           IF WS-HDR-POS < 95
               MOVE WS-MSG-TEXT (1) (WS-HDR-POS + 5:3) TO WS-P-MT
           ELSE
               MOVE WS-MSG-REC-TYPE    TO WS-P-MT
           END-IF
           IF WS-P-MT NOT NUMERIC
               MOVE WS-MSG-REC-TYPE    TO WS-P-MT
           END-IF
      *
           PERFORM VARYING WS-SUB FROM 2 BY 1
                     UNTIL WS-SUB NOT < WS-MSG-LINES
               MOVE WS-MSG-TEXT (WS-SUB) TO WS-LINE
               IF WS-LINE (1:1) = ':'
                   PERFORM 3100-SPLIT-TAG THRU 3100-EXIT
                   PERFORM 3200-TAKE-FIELD THRU 3200-EXIT
               END-IF
           END-PERFORM.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3100 - ':25D::MTCH//MACH' -> TAG 25D, QUAL MTCH, PART-2 MACH   *
      *        ':36B::ESTT//UNIT/100,' -> PART-2 UNIT, PART-3 100,     *
      *        ':16R:GENL'        -> TAG 16R, WS-FLD-3 GENL            *
      *----------------------------------------------------------------*
       3100-SPLIT-TAG.
           MOVE SPACES                 TO WS-FLD-1 WS-FLD-TAG WS-FLD-3
                                          WS-FLD-4 WS-QUAL
                                          WS-QUAL-DELIM WS-PART-2
                                          WS-PART-2-DELIM WS-PART-3
           MOVE ZERO                   TO WS-FLD-COUNT
           UNSTRING WS-LINE DELIMITED BY ':'
               INTO WS-FLD-1 WS-FLD-TAG WS-FLD-3 WS-FLD-4
               TALLYING IN WS-FLD-COUNT
           END-UNSTRING
           IF WS-FLD-3 = SPACES AND WS-FLD-4 NOT = SPACES
               UNSTRING WS-FLD-4 DELIMITED BY '//' OR '/'
                   INTO WS-QUAL      DELIMITER IN WS-QUAL-DELIM
                        WS-PART-2    DELIMITER IN WS-PART-2-DELIM
                        WS-PART-3
               END-UNSTRING
           END-IF.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3200 - KEEP THE FIELDS WE USE, BY BLOCK                        *
      *----------------------------------------------------------------*
       3200-TAKE-FIELD.
           EVALUATE WS-FLD-TAG
               WHEN '16R'
                   PERFORM 3300-PUSH-BLOCK THRU 3300-EXIT
               WHEN '16S'
                   PERFORM 3400-POP-BLOCK  THRU 3400-EXIT
               WHEN '20C'
                   IF WS-QUAL = 'SEME' AND WS-CUR-BLOCK = 'GENL'
                       MOVE WS-PART-2 (1:16) TO WS-P-CUST-REF
                   END-IF
                   IF (WS-QUAL = 'RELA' OR 'PREV')
                   AND WS-CUR-BLOCK = 'LINK'
                   AND WS-PARENT-BLOCK = 'GENL'
                       MOVE WS-PART-2 (1:16) TO WS-P-RELA
                   END-IF
               WHEN '23G'
                   IF WS-CUR-BLOCK = 'GENL'
                       MOVE WS-FLD-3 (1:4) TO WS-P-FUNCTION
                   END-IF
               WHEN '25D'
                   IF WS-CUR-BLOCK = 'STAT'
                   AND WS-P-STAT-COUNT < 5
                       ADD 1           TO WS-P-STAT-COUNT
                       MOVE WS-QUAL    TO
                            WS-P-STAT-QUAL (WS-P-STAT-COUNT)
                       MOVE WS-PART-2 (1:4) TO
                            WS-P-STAT-CODE (WS-P-STAT-COUNT)
                       MOVE SPACES     TO
                            WS-P-STAT-REASON (WS-P-STAT-COUNT)
                   END-IF
               WHEN '24B'
                   IF WS-CUR-BLOCK = 'REAS'
                   AND WS-PARENT-BLOCK = 'STAT'
                   AND WS-P-STAT-COUNT > ZERO
                       IF WS-P-STAT-REASON (WS-P-STAT-COUNT) = SPACES
                           MOVE WS-PART-2 (1:4) TO
                                WS-P-STAT-REASON (WS-P-STAT-COUNT)
                       END-IF
                   END-IF
               WHEN '98A'
                   IF WS-QUAL = 'ESET' AND WS-CUR-BLOCK = 'TRADDET'
                       MOVE WS-PART-2 (1:8) TO WS-P-ESET-DATE
                   END-IF
               WHEN '36B'
                   IF WS-QUAL = 'ESTT' AND WS-CUR-BLOCK = 'FIAC'
                       MOVE WS-PART-2 (1:4) TO WS-P-QTY-TYPE
                       MOVE WS-PART-3 (1:35) TO WS-P-QTY-TEXT
                   END-IF
               WHEN '19A'
                   IF WS-QUAL = 'ESTT' AND WS-CUR-BLOCK = 'AMT'
                       MOVE WS-PART-2 (1:35) TO WS-P-AMT-TEXT
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       3200-EXIT.
           EXIT.
      *
       3300-PUSH-BLOCK.
           IF WS-BLK-DEPTH < 6
               ADD 1                   TO WS-BLK-DEPTH
               MOVE WS-FLD-3 (1:8)     TO WS-BLK-NAME (WS-BLK-DEPTH)
           ELSE
               MOVE 'BLOCKS NESTED DEEPER THAN 6' TO WS-ERR-TEXT
               DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' ' WS-ERR-TEXT
           END-IF
           PERFORM 3500-SET-CURRENT    THRU 3500-EXIT.
       3300-EXIT.
           EXIT.
      *
       3400-POP-BLOCK.
           IF WS-BLK-DEPTH > ZERO
               IF WS-BLK-NAME (WS-BLK-DEPTH) NOT = WS-FLD-3 (1:8)
                   DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ
                           ' :16S:' WS-FLD-3 (1:8)
                           ' CLOSES ' WS-BLK-NAME (WS-BLK-DEPTH)
               END-IF
               SUBTRACT 1              FROM WS-BLK-DEPTH
           END-IF
           PERFORM 3500-SET-CURRENT    THRU 3500-EXIT.
       3400-EXIT.
           EXIT.
      *
       3500-SET-CURRENT.
           MOVE SPACES                 TO WS-CUR-BLOCK WS-PARENT-BLOCK
           IF WS-BLK-DEPTH > ZERO
               MOVE WS-BLK-NAME (WS-BLK-DEPTH) TO WS-CUR-BLOCK
           END-IF
           IF WS-BLK-DEPTH > 1
               MOVE WS-BLK-NAME (WS-BLK-DEPTH - 1) TO WS-PARENT-BLOCK
           END-IF.
       3500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3900 - CUSTODIAN REFERENCE ALREADY SEEN IN THIS FILE?          *
      *        HASH = SUM OF CHARACTER VALUE X POSITION, OPEN          *
      *        ADDRESSING WITH LINEAR PROBING                          *
      *----------------------------------------------------------------*
       3900-CHECK-DUPLICATE.
           MOVE ZERO                   TO WS-HASH
           PERFORM VARYING WS-HASH-POS FROM 1 BY 1
                     UNTIL WS-HASH-POS > 16
               COMPUTE WS-HASH = WS-HASH +
                   FUNCTION ORD (WS-P-CUST-REF (WS-HASH-POS:1))
                   * WS-HASH-POS
           END-PERFORM
           COMPUTE WS-HASH-SLOT =
                   FUNCTION MOD (WS-HASH WS-SEEN-SLOTS) + 1
           MOVE ZERO                   TO WS-HASH-PROBES
           MOVE SPACE                  TO WS-HASH-SW
           PERFORM UNTIL WS-HASH-SW NOT = SPACE
               EVALUATE TRUE
                   WHEN WS-SEEN-REF (WS-HASH-SLOT) = SPACES
                       MOVE WS-P-CUST-REF TO WS-SEEN-REF (WS-HASH-SLOT)
                       ADD 1           TO WS-SEEN-USED
                       SET WS-HASH-STORED TO TRUE
                   WHEN WS-SEEN-REF (WS-HASH-SLOT) = WS-P-CUST-REF
                       SET WS-HASH-DUPLICATE TO TRUE
                   WHEN OTHER
                       ADD 1           TO WS-HASH-PROBES
                       ADD 1           TO WS-HASH-SLOT
                       IF WS-HASH-SLOT > WS-SEEN-SLOTS
                           MOVE 1      TO WS-HASH-SLOT
                       END-IF
                       IF WS-HASH-PROBES NOT < WS-SEEN-SLOTS
                           MOVE 1007   TO AB-ABEND-CODE
                           MOVE '3900-CHECK-DUPLICATE'
                                       TO AB-PARAGRAPH
                           MOVE WS-P-CUST-REF TO AB-KEY
                           MOVE 'CUSTODIAN REFERENCE TABLE FULL'
                                       TO AB-MESSAGE
                           PERFORM 9999-ABEND THRU 9999-EXIT
                       END-IF
               END-EVALUATE
           END-PERFORM.
       3900-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - POST THE MESSAGE TO THE INSTRUCTION MASTER              *
      *================================================================*
       4000-APPLY-MESSAGE.
           EVALUATE WS-P-MT
               WHEN '548'
                   ADD 1               TO WS-CNT-548
               WHEN '544' WHEN '545' WHEN '546' WHEN '547'
                   ADD 1               TO WS-CNT-CONFIRM
               WHEN OTHER
                   ADD 1               TO WS-MSG-UNSUPPORTED
                   MOVE 4              TO WS-RETURN-CODE
                   DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' MT' WS-P-MT
                           ' NOT SUPPORTED - SKIPPED'
                   GO TO 4000-EXIT
           END-EVALUATE
      *
           PERFORM 4050-READ-INSTRUCTION THRU 4050-EXIT
           IF NOT WS-INSTR-FOUND
               PERFORM 4900-UNKNOWN-REF THRU 4900-EXIT
               GO TO 4000-EXIT
           END-IF
      *
           MOVE 'N'                    TO WS-UPDATE-SW
           IF WS-P-MT = '548'
               IF WS-P-STAT-COUNT = ZERO
                   MOVE 'MT548 WITHOUT :25D: STATUS' TO WS-ERR-TEXT
                   PERFORM 6900-FORMAT-ERROR THRU 6900-EXIT
                   GO TO 4000-EXIT
               END-IF
               PERFORM 4200-APPLY-STATUS THRU 4200-EXIT
                   VARYING WS-STAT-SUB FROM 1 BY 1
                     UNTIL WS-STAT-SUB > WS-P-STAT-COUNT
           ELSE
               PERFORM 4100-APPLY-SETTLEMENT THRU 4100-EXIT
           END-IF
      *
           IF WS-INSTR-CHANGED
               MOVE DC-BUS-DATE        TO SWI-LAST-STATUS-DATE
               MOVE WS-P-MT            TO SWI-LAST-STATUS-MSG
               IF WS-P-CUST-REF NOT = SPACES
               AND SWI-FUNCTION = 'NEWM'
                   MOVE WS-P-CUST-REF  TO SWI-CUST-REF
               END-IF
               MOVE JI-JOBNAME         TO SWI-LAST-UPD-JOB
               PERFORM 8200-REWRITE-INSTR THRU 8200-EXIT
               ADD 1                   TO WS-INSTR-UPDATED
           END-IF.
       4000-EXIT.
           EXIT.
      *
       4050-READ-INSTRUCTION.
           MOVE 'N'                    TO WS-FOUND-SW
           IF WS-P-RELA = SPACES
           OR WS-P-RELA = 'SEQCONTROL'
               GO TO 4050-EXIT
           END-IF
           MOVE WS-P-RELA              TO SWI-SENDER-REF
           READ SWINSTR-FILE
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   SET WS-INSTR-FOUND  TO TRUE
               WHEN SWINSTR-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE WS-P-RELA      TO AB-KEY
                   MOVE '4050-READ-INSTRUCTION' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       4050-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4100 - MT544-547 SETTLEMENT CONFIRMATION                       *
      *----------------------------------------------------------------*
       4100-APPLY-SETTLEMENT.
           IF (WS-P-MT = '544' OR '545') AND SWI-MSG-TYPE NOT = '541'
           OR (WS-P-MT = '546' OR '547') AND SWI-MSG-TYPE NOT = '543'
               ADD 1                   TO WS-MT-MISMATCH
               DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' MT' WS-P-MT
                       ' RELATES TO MT' SWI-MSG-TYPE ' ' WS-P-RELA
           END-IF
      *
           MOVE WS-P-QTY-TEXT          TO WS-NUM-TEXT
           PERFORM 5000-DECIMAL-COMMA  THRU 5000-EXIT
           IF NOT WS-NUM-OK OR WS-P-QTY-TEXT = SPACES
               MOVE 'SETTLED QUANTITY :36B::ESTT// INVALID'
                                       TO WS-ERR-TEXT
               PERFORM 6900-FORMAT-ERROR THRU 6900-EXIT
               GO TO 4100-EXIT
           END-IF
           MOVE WS-NUM-VALUE           TO WS-SETTLED-QTY
      *
           MOVE ZERO                   TO WS-SETTLED-AMT
           MOVE SPACES                 TO WS-SETTLED-CCY
           IF WS-P-AMT-TEXT NOT = SPACES
               IF WS-P-AMT-TEXT (1:1) = 'N'
                   MOVE WS-P-AMT-TEXT (2:3) TO WS-SETTLED-CCY
                   MOVE 'N'            TO WS-NUM-TEXT
                   MOVE WS-P-AMT-TEXT (5:31) TO WS-NUM-TEXT (2:34)
               ELSE
                   MOVE WS-P-AMT-TEXT (1:3) TO WS-SETTLED-CCY
                   MOVE WS-P-AMT-TEXT (4:32) TO WS-NUM-TEXT
               END-IF
               PERFORM 5000-DECIMAL-COMMA THRU 5000-EXIT
               IF NOT WS-NUM-OK
                   MOVE 'SETTLED AMOUNT :19A::ESTT// INVALID'
                                       TO WS-ERR-TEXT
                   PERFORM 6900-FORMAT-ERROR THRU 6900-EXIT
                   GO TO 4100-EXIT
               END-IF
               MOVE WS-NUM-VALUE       TO WS-SETTLED-AMT
               IF WS-SETTLED-CCY NOT = SWI-CCY
                   DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' CURRENCY '
                           WS-SETTLED-CCY ' NOT ' SWI-CCY ' '
                           WS-P-RELA
               END-IF
           END-IF
      *
           MOVE SPACES                 TO WS-EV-REASON
           MOVE 'SETT'                 TO WS-EV-STATUS-CODE
           IF SWI-SETTLED OR SWI-CANCELLED
               MOVE 'ST'               TO WS-EV-CODE
               MOVE 'N'                TO WS-EV-APPLIED
               PERFORM 6000-WRITE-EVENT THRU 6000-EXIT
               GO TO 4100-EXIT
           END-IF
      *
           COMPUTE WS-OPEN-QTY = SWI-QTY - SWI-SETTLED-QTY
           PERFORM 4150-CHECK-CONFIRMATION THRU 4150-EXIT
           ADD WS-SETTLED-QTY          TO SWI-SETTLED-QTY
           ADD WS-SETTLED-AMT          TO SWI-SETTLED-AMOUNT
           IF WS-SETTLED-QTY < WS-OPEN-QTY
               SET SWI-PARTIAL         TO TRUE
               MOVE 'PS'               TO WS-EV-CODE
               MOVE 'PART'             TO SWI-STATUS-CODE
           ELSE
               SET SWI-SETTLED         TO TRUE
               MOVE 'ST'               TO WS-EV-CODE
               MOVE 'SETT'             TO SWI-STATUS-CODE
           END-IF
           MOVE SPACES                 TO SWI-REASON-CODE
           IF WS-P-ESET-DATE NUMERIC
               MOVE WS-P-ESET-DATE     TO SWI-EFF-SETTLE-DATE
           ELSE
               MOVE DC-BUS-DATE        TO SWI-EFF-SETTLE-DATE
           END-IF
           MOVE ZERO                   TO SWI-FAIL-AGE
           SET WS-INSTR-CHANGED        TO TRUE
           ADD WS-SETTLED-AMT          TO WS-SETTLED-AMT-TOTAL
           ADD WS-SETTLED-QTY          TO WS-SETTLED-QTY-TOTAL
           MOVE 'Y'                    TO WS-EV-APPLIED
           PERFORM 6000-WRITE-EVENT    THRU 6000-EXIT.
       4100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4150 - PLAUSIBILITY OF A CONFIRMATION (WARNINGS ONLY - THE     *
      *        CUSTODIAN'S BOOKS ARE THE RECORD OF SETTLEMENT)         *
      *----------------------------------------------------------------*
       4150-CHECK-CONFIRMATION.
           IF WS-SETTLED-QTY > WS-OPEN-QTY
               ADD 1                   TO WS-OVER-DELIVERY
               DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' ' WS-P-RELA
                       ' SETTLED QUANTITY ABOVE OPEN QUANTITY'
           END-IF
           IF WS-P-ESET-DATE NUMERIC
               IF WS-P-ESET-DATE > DC-BUS-DATE
                   ADD 1               TO WS-FUTURE-ESET
                   DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' ' WS-P-RELA
                           ' EFFECTIVE SETTLE DATE ' WS-P-ESET-DATE
                           ' AFTER BUSINESS DATE'
               END-IF
           END-IF
           IF SWI-QTY = ZERO OR WS-P-AMT-TEXT = SPACES
               GO TO 4150-EXIT
           END-IF
           COMPUTE WS-EXPECTED-AMT ROUNDED =
                   SWI-AMOUNT * WS-SETTLED-QTY / SWI-QTY
           COMPUTE WS-AMT-DIFF = WS-SETTLED-AMT - WS-EXPECTED-AMT
           IF WS-AMT-DIFF < ZERO
               COMPUTE WS-AMT-DIFF = ZERO - WS-AMT-DIFF
           END-IF
           COMPUTE WS-AMT-LIMIT = WS-EXPECTED-AMT * WS-AMT-TOLERANCE
           IF WS-AMT-DIFF > WS-AMT-LIMIT
               ADD 1                   TO WS-AMT-DIFFERENCES
               DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' ' WS-P-RELA
                       ' SETTLED AMOUNT DIFFERS FROM INSTRUCTION BY '
                       'MORE THAN 0.5 PCT'
           END-IF.
       4150-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4200 - ONE :25D: STATUS OF AN MT548                            *
      *----------------------------------------------------------------*
       4200-APPLY-STATUS.
           MOVE WS-P-STAT-CODE (WS-STAT-SUB)   TO WS-EV-STATUS-CODE
           MOVE WS-P-STAT-REASON (WS-STAT-SUB) TO WS-EV-REASON
           MOVE ZERO                   TO WS-SETTLED-QTY WS-SETTLED-AMT
           MOVE SPACES                 TO WS-EV-CODE
           EVALUATE WS-P-STAT-QUAL (WS-STAT-SUB)
                ALSO WS-P-STAT-CODE (WS-STAT-SUB)
               WHEN 'MTCH' ALSO 'MACH'
                   MOVE 'MA'           TO WS-EV-CODE
               WHEN 'MTCH' ALSO 'NMAT'
                   MOVE 'NM'           TO WS-EV-CODE
               WHEN 'SETT' ALSO 'PEND'
               WHEN 'SETT' ALSO 'PENF'
                   MOVE 'PE'           TO WS-EV-CODE
               WHEN 'IPRC' ALSO 'REJT'
                   MOVE 'RJ'           TO WS-EV-CODE
               WHEN 'IPRC' ALSO 'CAND'
                   MOVE 'CX'           TO WS-EV-CODE
               WHEN 'IPRC' ALSO 'PACK'
                   ADD 1               TO WS-ACKS
                   IF SWI-SENT
                       MOVE 'PACK'     TO SWI-STATUS-CODE
                       SET WS-INSTR-CHANGED TO TRUE
                   END-IF
                   GO TO 4200-EXIT
               WHEN OTHER
                   STRING 'STATUS ' WS-P-STAT-QUAL (WS-STAT-SUB) '//'
                          WS-P-STAT-CODE (WS-STAT-SUB)
                          ' NOT RECOGNISED'
                          DELIMITED BY SIZE INTO WS-ERR-TEXT
                   PERFORM 6900-FORMAT-ERROR THRU 6900-EXIT
                   GO TO 4200-EXIT
           END-EVALUATE
      *
           MOVE 'Y'                    TO WS-EV-APPLIED
           EVALUATE TRUE
               WHEN SWI-SETTLED
               WHEN SWI-CANCELLED
                   MOVE 'N'            TO WS-EV-APPLIED
               WHEN WS-EV-CODE = 'CX'
                   PERFORM 4300-CANCEL-COMPLETED THRU 4300-EXIT
               WHEN SWI-PARTIAL AND WS-EV-CODE NOT = 'RJ'
      *            PARTIALLY SETTLED - THE REST STAYS PS
                   MOVE WS-EV-STATUS-CODE TO SWI-STATUS-CODE
                   MOVE WS-EV-REASON   TO SWI-REASON-CODE
                   SET WS-INSTR-CHANGED TO TRUE
               WHEN OTHER
                   MOVE WS-EV-CODE     TO SWI-STATUS
                   MOVE WS-EV-STATUS-CODE TO SWI-STATUS-CODE
                   MOVE WS-EV-REASON   TO SWI-REASON-CODE
                   SET WS-INSTR-CHANGED TO TRUE
           END-EVALUATE
           PERFORM 6000-WRITE-EVENT    THRU 6000-EXIT.
       4200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4300 - CANCELLATION COMPLETED: THE CANC ROW AND THE NEWM ROW   *
      *        IT CANCELLED (SWI-CUST-REF OF A CANC ROW) BECOME CX     *
      *----------------------------------------------------------------*
       4300-CANCEL-COMPLETED.
           SET SWI-CANCELLED           TO TRUE
           MOVE 'CAND'                 TO SWI-STATUS-CODE
           SET WS-INSTR-CHANGED        TO TRUE
           IF SWI-FUNCTION NOT = 'CANC'
           OR SWI-CUST-REF = SPACES
               GO TO 4300-EXIT
           END-IF
      *    WRITE THE CANC ROW NOW - THE ORIGINAL IS READ INTO THE SAME
      *    RECORD AREA
           MOVE DC-BUS-DATE            TO SWI-LAST-STATUS-DATE
           MOVE WS-P-MT                TO SWI-LAST-STATUS-MSG
           MOVE JI-JOBNAME             TO SWI-LAST-UPD-JOB
           PERFORM 8200-REWRITE-INSTR  THRU 8200-EXIT
           MOVE SWI-CUST-REF           TO WS-ORIG-REF
           MOVE WS-ORIG-REF            TO SWI-SENDER-REF
           READ SWINSTR-FILE
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   IF NOT SWI-CANCELLED
                       SET SWI-CANCELLED TO TRUE
                       MOVE 'CAND'     TO SWI-STATUS-CODE
                       MOVE DC-BUS-DATE TO SWI-LAST-STATUS-DATE
                       MOVE WS-P-MT    TO SWI-LAST-STATUS-MSG
                       MOVE JI-JOBNAME TO SWI-LAST-UPD-JOB
                       PERFORM 8200-REWRITE-INSTR THRU 8200-EXIT
                   END-IF
               WHEN SWINSTR-NOTFND
                   DISPLAY 'SWB200 - CANCELLED INSTRUCTION '
                           WS-ORIG-REF ' NOT ON MASTER'
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE WS-ORIG-REF    TO AB-KEY
                   MOVE '4300-CANCEL-COMPLETED' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
      *    RE-POSITION ON THE CANC ROW FOR THE EVENT AND THE FINAL
      *    REWRITE IN 4000
           MOVE WS-P-RELA              TO SWI-SENDER-REF
           READ SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-P-RELA          TO AB-KEY
               MOVE '4300-CANCEL-COMPLETED' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           SET WS-INSTR-CHANGED        TO TRUE.
       4300-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4900 - RELATED REFERENCE NOT ON THE MASTER                     *
      *----------------------------------------------------------------*
       4900-UNKNOWN-REF.
           ADD 1                       TO WS-EV-UNKNOWN
           MOVE 4                      TO WS-RETURN-CODE
           DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' MT' WS-P-MT
                   ' UNKNOWN REFERENCE [' WS-P-RELA ']'
                   ' CUSTODIAN REF ' WS-P-CUST-REF
           INITIALIZE SWS-STATUS-REC
           MOVE DC-BUS-DATE            TO SWS-BUS-DATE
           MOVE WS-MSG-SEQ             TO SWS-MSG-SEQ
           MOVE WS-P-MT                TO SWS-MSG-TYPE
           MOVE WS-P-RELA              TO SWS-RELATED-REF
           MOVE WS-P-CUST-REF          TO SWS-CUST-REF
           SET SWS-EV-UNKNOWN-REF      TO TRUE
           IF WS-P-STAT-COUNT > ZERO
               MOVE WS-P-STAT-CODE (1)   TO SWS-STATUS-CODE
               MOVE WS-P-STAT-REASON (1) TO SWS-REASON-CODE
           ELSE
               MOVE 'SETT'             TO SWS-STATUS-CODE
               MOVE SPACES             TO SWS-REASON-CODE
           END-IF
           MOVE ZERO                   TO SWS-QTY SWS-AMOUNT
           MOVE WS-P-QTY-TEXT          TO WS-NUM-TEXT
           IF WS-P-QTY-TEXT NOT = SPACES
               PERFORM 5000-DECIMAL-COMMA THRU 5000-EXIT
               IF WS-NUM-OK
                   MOVE WS-NUM-VALUE   TO SWS-QTY
               END-IF
           END-IF
           IF WS-P-ESET-DATE NUMERIC
               MOVE WS-P-ESET-DATE     TO SWS-EFF-DATE
           ELSE
               MOVE ZERO               TO SWS-EFF-DATE
           END-IF
           MOVE 'N'                    TO SWS-APPLIED-FLAG
           PERFORM 6100-PUT-EVENT      THRU 6100-EXIT.
       4900-EXIT.
           EXIT.
      *
      *================================================================*
      * 5000 - ISO 15022 NUMBER '1234,56' -> WS-NUM-VALUE              *
      *        WHOLE NUMBERS WITHOUT A COMMA ARE ACCEPTED (CHG14415).  *
      *        'N' IN FRONT = NEGATIVE.  MORE THAN 4 DECIMALS ARE      *
      *        DROPPED.                                                *
      *================================================================*
       5000-DECIMAL-COMMA.
           MOVE 'N'                    TO WS-NUM-OK-SW
           MOVE ZERO                   TO WS-NUM-VALUE
           MOVE 'N'                    TO WS-NUM-NEG-SW
           IF WS-NUM-TEXT (1:1) = 'N'
               MOVE 'Y'                TO WS-NUM-NEG-SW
               MOVE WS-NUM-TEXT (2:34) TO WS-NUM-TEXT
           END-IF
           MOVE ZERO                   TO WS-NUM-COMMAS
           INSPECT WS-NUM-TEXT TALLYING WS-NUM-COMMAS FOR ALL ','
           IF WS-NUM-COMMAS > 1
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES                 TO WS-NUM-INT-TXT WS-NUM-FRAC-TXT
           MOVE ZERO                   TO WS-NUM-INT-LEN WS-NUM-FRAC-LEN
           UNSTRING WS-NUM-TEXT DELIMITED BY ',' OR SPACE
               INTO WS-NUM-INT-TXT  COUNT IN WS-NUM-INT-LEN
                    WS-NUM-FRAC-TXT COUNT IN WS-NUM-FRAC-LEN
           END-UNSTRING
           IF WS-NUM-INT-LEN = ZERO OR WS-NUM-INT-LEN > 15
               GO TO 5000-EXIT
           END-IF
           IF WS-NUM-INT-TXT (1:WS-NUM-INT-LEN) NOT NUMERIC
               GO TO 5000-EXIT
           END-IF
           MOVE WS-NUM-INT-TXT (1:WS-NUM-INT-LEN) TO WS-NUM-INT
           IF WS-NUM-COMMAS = ZERO
               MOVE SPACES             TO WS-NUM-FRAC-TXT
           END-IF
           INSPECT WS-NUM-FRAC-TXT REPLACING ALL SPACE BY ZERO
           IF WS-NUM-FRAC-TXT NOT NUMERIC
               GO TO 5000-EXIT
           END-IF
           MOVE WS-NUM-FRAC-TXT        TO WS-NUM-FRAC
           COMPUTE WS-NUM-VALUE = WS-NUM-INT + (WS-NUM-FRAC / 10000)
           IF WS-NUM-NEG-SW = 'Y'
               COMPUTE WS-NUM-VALUE = ZERO - WS-NUM-VALUE
           END-IF
           SET WS-NUM-OK               TO TRUE.
       5000-EXIT.
           EXIT.
      *
      *================================================================*
      * 6000 - STATUS EVENT FOR THE CURRENT INSTRUCTION                *
      *================================================================*
       6000-WRITE-EVENT.
           INITIALIZE SWS-STATUS-REC
           MOVE DC-BUS-DATE            TO SWS-BUS-DATE
           MOVE WS-MSG-SEQ             TO SWS-MSG-SEQ
           MOVE WS-P-MT                TO SWS-MSG-TYPE
           MOVE WS-P-RELA              TO SWS-RELATED-REF
           MOVE WS-P-CUST-REF          TO SWS-CUST-REF
           MOVE WS-EV-CODE             TO SWS-EVENT
           MOVE WS-EV-STATUS-CODE      TO SWS-STATUS-CODE
           MOVE WS-EV-REASON           TO SWS-REASON-CODE
           MOVE WS-SETTLED-QTY         TO SWS-QTY
           MOVE WS-SETTLED-AMT         TO SWS-AMOUNT
           IF WS-P-ESET-DATE NUMERIC
               MOVE WS-P-ESET-DATE     TO SWS-EFF-DATE
           ELSE
               MOVE DC-BUS-DATE        TO SWS-EFF-DATE
           END-IF
           MOVE WS-EV-APPLIED          TO SWS-APPLIED-FLAG
           IF WS-EV-APPLIED = 'N'
               ADD 1                   TO WS-EV-NOT-APPLIED
           END-IF
           EVALUATE WS-EV-CODE
               WHEN 'MA'   ADD 1       TO WS-EV-MATCHED
               WHEN 'NM'   ADD 1       TO WS-EV-UNMATCHED
               WHEN 'PE'   ADD 1       TO WS-EV-PENDING
               WHEN 'RJ'   ADD 1       TO WS-EV-REJECTED
               WHEN 'ST'   ADD 1       TO WS-EV-SETTLED
               WHEN 'PS'   ADD 1       TO WS-EV-PARTIAL
               WHEN 'CX'   ADD 1       TO WS-EV-CANCELLED
           END-EVALUATE
           PERFORM 6100-PUT-EVENT      THRU 6100-EXIT.
       6000-EXIT.
           EXIT.
      *
       6100-PUT-EVENT.
           WRITE SWS-STATUS-REC
           IF NOT STATOUT-OK
               MOVE 'STATOUT'          TO AB-DDNAME
               MOVE WS-STATOUT-FS      TO AB-FILE-STATUS
               MOVE WS-P-RELA          TO AB-KEY
               MOVE '6100-PUT-EVENT'   TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-EV-WRITTEN
           ADD SWS-QTY                 TO WS-EV-QTY-TOTAL.
       6100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 6900 - MESSAGE CANNOT BE APPLIED                               *
      *----------------------------------------------------------------*
       6900-FORMAT-ERROR.
           ADD 1                       TO WS-MSG-ERRORS
           MOVE 4                      TO WS-RETURN-CODE
           DISPLAY 'SWB200 - MSG ' WS-MSG-SEQ ' MT' WS-MSG-REC-TYPE
                   ' NOT APPLIED: ' WS-ERR-TEXT
           MOVE SPACES                 TO WS-ERR-TEXT.
       6900-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O                                                     *
      *================================================================*
       8000-READ-LINE.
           READ MSGIN-FILE INTO SWM-MSG-LINE
           EVALUATE TRUE
               WHEN MSGIN-OK
                   ADD 1               TO WS-LINES-READ
               WHEN MSGIN-EOF
                   SET WS-END-OF-INPUT TO TRUE
                   MOVE HIGH-VALUES    TO SWM-MSG-LINE
               WHEN OTHER
                   MOVE 'MSGIN'        TO AB-DDNAME
                   MOVE WS-MSGIN-FS    TO AB-FILE-STATUS
                   MOVE '8000-READ-LINE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
       8200-REWRITE-INSTR.
           REWRITE SWI-INSTR-REC
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE '8200-REWRITE-INSTR' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       8200-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE MSGIN-FILE
                 SWINSTR-FILE
                 STATOUT-FILE
           IF NOT MSGIN-OK OR NOT SWINSTR-OK OR NOT STATOUT-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-MSGIN-FS ' '
                      WS-SWINSTR-FS ' ' WS-STATOUT-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           IF WS-MSG-ERRORS > WS-MAX-ERRORS
               MOVE 8                  TO WS-RETURN-CODE
               DISPLAY 'SWB200 - FORMAT ERRORS ' WS-MSG-ERRORS
                       ' EXCEED MAXERR ' WS-MAX-ERRORS
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM CT-STAGE
           MOVE 'MSG-IN'               TO CT-COUNTER-NAME
           MOVE WS-MSGS-READ           TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
           MOVE WS-LINES-READ          TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'SETTLED'              TO CT-COUNTER-NAME
           COMPUTE CT-COUNT = WS-EV-SETTLED + WS-EV-PARTIAL
           MOVE WS-SETTLED-AMT-TOTAL   TO CT-AMOUNT
           MOVE WS-SETTLED-QTY-TOTAL   TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'PENDING'              TO CT-COUNTER-NAME
           MOVE WS-EV-PENDING          TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'UNKNOWN-REF'          TO CT-COUNTER-NAME
           MOVE WS-EV-UNKNOWN          TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'MATCHED'              TO CT-COUNTER-NAME
           MOVE WS-EV-MATCHED          TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'UNMATCHED'            TO CT-COUNTER-NAME
           MOVE WS-EV-UNMATCHED        TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'REJECTED'             TO CT-COUNTER-NAME
           MOVE WS-EV-REJECTED         TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'PARTIAL'              TO CT-COUNTER-NAME
           MOVE WS-EV-PARTIAL          TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'MSG-ERRORS'           TO CT-COUNTER-NAME
           COMPUTE CT-COUNT = WS-MSG-ERRORS + WS-MSG-UNSUPPORTED
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'DUPLICATES'           TO CT-COUNTER-NAME
           MOVE WS-DUPLICATES          TO CT-COUNT
           MOVE ZERO                   TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'STATEVT-OUT'          TO CT-COUNTER-NAME
           MOVE WS-EV-WRITTEN          TO CT-COUNT
           MOVE WS-EV-QTY-TOTAL        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RETURN-CODE = ZERO
               MOVE 'I'                TO AU-SEVERITY
           ELSE
               MOVE 'W'                TO AU-SEVERITY
           END-IF
           MOVE WS-MSGS-READ           TO WS-DISP-COUNT
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'INBOUND PARSER ENDED. MESSAGES ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* SWB200 - SWIFT INBOUND PARSER      ' DC-BUS-DATE
           DISPLAY '*************************************************'
           MOVE WS-LINES-READ          TO WS-DISP-COUNT
           DISPLAY '* LINES READ                : ' WS-DISP-COUNT
           MOVE WS-MSGS-READ           TO WS-DISP-COUNT
           DISPLAY '* MESSAGES READ             : ' WS-DISP-COUNT
           MOVE WS-CNT-548             TO WS-DISP-COUNT
           DISPLAY '*   MT548 STATUS            : ' WS-DISP-COUNT
           MOVE WS-CNT-CONFIRM         TO WS-DISP-COUNT
           DISPLAY '*   MT544-547 CONFIRMATIONS : ' WS-DISP-COUNT
           MOVE WS-MSG-UNSUPPORTED     TO WS-DISP-COUNT
           DISPLAY '*   UNSUPPORTED TYPES       : ' WS-DISP-COUNT
           MOVE WS-MSG-ERRORS          TO WS-DISP-COUNT
           DISPLAY '*   FORMAT ERRORS           : ' WS-DISP-COUNT
           MOVE WS-LINES-SKIPPED       TO WS-DISP-COUNT
           DISPLAY '* LINES OUTSIDE A MESSAGE   : ' WS-DISP-COUNT
           MOVE WS-EV-WRITTEN          TO WS-DISP-COUNT
           DISPLAY '* STATUS EVENTS WRITTEN     : ' WS-DISP-COUNT
           MOVE WS-EV-MATCHED          TO WS-DISP-COUNT
           DISPLAY '*   MATCHED          (MA)   : ' WS-DISP-COUNT
           MOVE WS-EV-UNMATCHED        TO WS-DISP-COUNT
           DISPLAY '*   UNMATCHED        (NM)   : ' WS-DISP-COUNT
           MOVE WS-EV-PENDING          TO WS-DISP-COUNT
           DISPLAY '*   PENDING          (PE)   : ' WS-DISP-COUNT
           MOVE WS-EV-REJECTED         TO WS-DISP-COUNT
           DISPLAY '*   REJECTED         (RJ)   : ' WS-DISP-COUNT
           MOVE WS-EV-SETTLED          TO WS-DISP-COUNT
           DISPLAY '*   SETTLED          (ST)   : ' WS-DISP-COUNT
           MOVE WS-EV-PARTIAL          TO WS-DISP-COUNT
           DISPLAY '*   PARTIAL          (PS)   : ' WS-DISP-COUNT
           MOVE WS-EV-CANCELLED        TO WS-DISP-COUNT
           DISPLAY '*   CANCELLED        (CX)   : ' WS-DISP-COUNT
           MOVE WS-EV-UNKNOWN          TO WS-DISP-COUNT
           DISPLAY '*   UNKNOWN REFERENCE (UR)  : ' WS-DISP-COUNT
           MOVE WS-EV-NOT-APPLIED      TO WS-DISP-COUNT
           DISPLAY '*   NOT APPLIED (FINAL)     : ' WS-DISP-COUNT
           MOVE WS-ACKS                TO WS-DISP-COUNT
           DISPLAY '* ACKNOWLEDGEMENTS (PACK)   : ' WS-DISP-COUNT
           MOVE WS-MT-MISMATCH         TO WS-DISP-COUNT
           DISPLAY '* MT / INSTRUCTION MISMATCH : ' WS-DISP-COUNT
           MOVE WS-DUPLICATES          TO WS-DISP-COUNT
           DISPLAY '* DUPLICATE MESSAGES        : ' WS-DISP-COUNT
           MOVE WS-OVER-DELIVERY       TO WS-DISP-COUNT
           DISPLAY '* OVER-DELIVERY WARNINGS    : ' WS-DISP-COUNT
           MOVE WS-AMT-DIFFERENCES     TO WS-DISP-COUNT
           DISPLAY '* AMOUNT DIFFERENCES > 0.5% : ' WS-DISP-COUNT
           MOVE WS-FUTURE-ESET         TO WS-DISP-COUNT
           DISPLAY '* FUTURE EFFECTIVE DATES    : ' WS-DISP-COUNT
           MOVE WS-SEEN-USED           TO WS-DISP-COUNT
           DISPLAY '* CUSTODIAN REFS HASHED     : ' WS-DISP-COUNT
           MOVE WS-INSTR-UPDATED       TO WS-DISP-COUNT
           DISPLAY '* INSTRUCTIONS UPDATED      : ' WS-DISP-COUNT
           MOVE WS-SETTLED-AMT-TOTAL   TO WS-DISP-AMT
           DISPLAY '* SETTLED AMOUNT (ALL CCY)  : ' WS-DISP-AMT
           DISPLAY '* RETURN CODE               : ' WS-RETURN-CODE
           DISPLAY '*************************************************'.
       9000-EXIT.
           EXIT.
      *
       9110-CALL-CMU080.
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9110-CALL-CMU080' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME    TO AB-KEY
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF.
       9110-EXIT.
           EXIT.
      *
       9910-OPEN-ERROR.
           MOVE 1001                   TO AB-ABEND-CODE
           MOVE '1000-INITIALIZE'      TO AB-PARAGRAPH
           STRING 'OPEN FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9910-EXIT.
           EXIT.
      *
       9920-IO-ERROR.
           MOVE 1002                   TO AB-ABEND-CODE
           STRING 'I/O ERROR ON ' AB-DDNAME ' STATUS '
                  AB-FILE-STATUS
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9920-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'SWB200 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWB200 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
