       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWB100.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  10/06/1997.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWB100                                            *
      * TITLE      : SWIFT MT541 / MT543 SETTLEMENT INSTRUCTION BUILD  *
      * JOB        : MSSWD010  STEP020  (IKJEFT01 - DB2 PLAN MSSWPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   FORMATS EVERY VALIDATED SETTLEMENT INSTRUCTION (SWB050) AS   *
      *   AN ISO 15022 MESSAGE FOR THE CUSTODIAN:                      *
      *     BUY  -> MT541 RECEIVE AGAINST PAYMENT                      *
      *     SELL -> MT543 DELIVER AGAINST PAYMENT                      *
      *     NEWM NEW INSTRUCTION, CANC CANCELLATION OF A PREVIOUS      *
      *     INSTRUCTION (LINKED WITH :20C::PREV//).                    *
      *   EVERY MESSAGE IS WRITTEN LINE BY LINE TO THE GATEWAY FILE    *
      *   (SWMSG, DIRECTION O) UNDER A MESSAGE SEQUENCE NUMBER TAKEN   *
      *   FROM THE SEQUENCE CONTROL ROW OF THE INSTRUCTION MASTER      *
      *   (KEY 'SEQCONTROL').  ONE INSTRUCTION MASTER ROW IS WRITTEN   *
      *   PER MESSAGE (STATUS SN = SENT); THE CUSTODIAN'S STATUS AND   *
      *   CONFIRMATIONS ARE POSTED TO IT BY SWB200.                    *
      *   AN INSTRUCTION ALREADY ON THE MASTER IS NOT SENT AGAIN       *
      *   (RERUN PROTECTION) UNLESS IT WAS REJECTED AND RESEND=Y.      *
      *   A CANCEL IS MATCHED TO THE INSTRUCTION IT CANCELS BY THE     *
      *   TRADE ID; OMS CANCELS CARRY THEIR OWN EXECUTION ID AND ARE   *
      *   MATCHED ON ACCOUNT / CUSIP / MT / TRADE DATE / QUANTITY      *
      *   AGAINST THE LAST 14 DAYS OF OPEN INSTRUCTIONS.               *
      *   BOND QUANTITIES ARE SENT AS FACE AMOUNT (FAMT), SHARES AS    *
      *   UNIT.  NUMBERS USE THE ISO 15022 DECIMAL COMMA (SWU010).     *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETERS SWP100A                          *
      *          INSTIN    MSEC.PROD.SW.INSTVAL(+1)       (TCSETIN)    *
      * UPDATE : SWINSTR   MSEC.PROD.SW.INSTR.KSDS        (SWINSTR)    *
      * OUTPUT : MSGOUT    MSEC.PROD.SW.OUTMSG(+1)        (SWMSG)      *
      * CALLS  : CMD010 CMU010 SWU010 CMASM01 CMU050 CMU060 CMU080     *
      *                                                                *
      * RETURN CODES: 0 CLEAN                                          *
      *               4 INSTRUCTIONS NOT SENT (ALREADY SENT, CANCEL    *
      *                 WITHOUT ORIGINAL) OR CONTROL ROW CREATED       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1997-10-06 DWB  CHG03390  ORIGINAL - ISO 15022 MIGRATION       *
      *                           (REPLACES MT521/MT523 FORMATTER)     *
      * 1998-02-09 DWB  CHG03512  INPUT FROM PRE-VALIDATION SWB050     *
      * 1998-11-02 TLM  CHG04471  Y2K - :98A: DATES CCYYMMDD           *
      * 2001-04-09 KAP  CHG08820  DECIMALIZATION - QTY VIA SWU010      *
      * 2003-01-27 KAP  CHG10244  SEQUENCE NUMBER KEPT ON INSTR MASTER *
      *                           (WAS A SEPARATE ONE-RECORD FILE)     *
      * 2005-08-30 KAP  CHG13391  :35B: ISIN + /US/CUSIP DESCRIPTION   *
      * 2006-03-13 KAP  CHG14620  FAMT FOR BONDS FROM SECURITY MASTER  *
      * 2009-12-14 SPA  CHG19002  EUROCLEAR PLACE OF SETTLEMENT        *
      * 2014-05-05 SPA  CHG27115  OMS CANCELS MATCHED ON ECONOMICS     *
      * 2016-10-03 SPA  CHG30112  :95R: DTC CONTRA PARTICIPANT         *
      * 2019-05-13 MHC  CHG34410  RESEND=Y FOR REJECTED INSTRUCTIONS   *
      * 2024-02-12 NVR  CHG41007  T+1 - CANCEL LOOKBACK 14 DAYS        *
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
           SELECT INSTIN-FILE   ASSIGN TO INSTIN
                                FILE STATUS IS WS-INSTIN-FS.
           SELECT SWINSTR-FILE  ASSIGN TO SWINSTR
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD KEY IS SWI-SENDER-REF
                                FILE STATUS IS WS-SWINSTR-FS.
           SELECT MSGOUT-FILE   ASSIGN TO MSGOUT
                                FILE STATUS IS WS-MSGOUT-FS.
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
       COPY CMDATEW.
       FD  INSTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCSETIN.
       FD  SWINSTR-FILE.
       COPY SWINSTR.
       FD  MSGOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SWMSG.
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWB100'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-INSTIN-FS            PIC X(02).
               88  INSTIN-OK                     VALUE '00'.
               88  INSTIN-EOF                    VALUE '10'.
           05  WS-SWINSTR-FS           PIC X(02).
               88  SWINSTR-OK                    VALUE '00'.
               88  SWINSTR-EOF                   VALUE '10'.
               88  SWINSTR-DUPKEY                VALUE '22'.
               88  SWINSTR-NOTFND                VALUE '23'.
           05  WS-MSGOUT-FS            PIC X(02).
               88  MSGOUT-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-INSTR               VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-BROWSE-SW            PIC X(01)  VALUE 'N'.
               88  WS-END-OF-BROWSE              VALUE 'Y'.
           05  WS-RESEND-SW            PIC X(01)  VALUE 'N'.
               88  WS-RESEND-REJECTED            VALUE 'Y'.
           05  WS-RESEND-THIS-SW       PIC X(01)  VALUE 'N'.
               88  WS-RESEND-THIS                VALUE 'Y'.
           05  WS-ORIG-FOUND-SW        PIC X(01)  VALUE 'N'.
               88  WS-ORIG-FOUND                 VALUE 'Y'.
           05  WS-CTL-CREATED-SW       PIC X(01)  VALUE 'N'.
               88  WS-CTL-CREATED                VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * GATEWAY ADDRESSES (SWP100A OVERRIDES)                          *
      *----------------------------------------------------------------*
       01  WS-GATEWAY.
           05  WS-SENDER-LT            PIC X(12)  VALUE 'MERDUS33AXXX'.
           05  WS-RECEIVER-LT          PIC X(12)  VALUE 'CUSTUS33XXXX'.
           05  WS-SESSION-ISN          PIC X(10)  VALUE '0000000000'.
       01  WS-PARM-KEYWORD             PIC X(20).
       01  WS-PARM-VALUE               PIC X(20).
      *
      *----------------------------------------------------------------*
      * SEQUENCE CONTROL ROW - DO NOT PURGE (SEE SWB300 HOUSEKEEPING)  *
      *----------------------------------------------------------------*
       01  WS-SEQ-CONTROL.
           05  WS-SEQ-CTL-KEY          PIC X(16)  VALUE 'SEQCONTROL'.
           05  WS-MSG-SEQ              PIC 9(08)  VALUE ZERO.
           05  WS-FIRST-SEQ            PIC 9(08)  VALUE ZERO.
           05  WS-SEQ-MAX              PIC 9(08)  VALUE 99999999.
      *
      *----------------------------------------------------------------*
      * DATA SOURCE SCHEME FOR :95R: CONTRA PARTICIPANT                *
      *----------------------------------------------------------------*
       01  WS-SCHEME-VALUES.
           05  FILLER  PIC X(10)  VALUE 'DTC DTCYID'.
           05  FILLER  PIC X(10)  VALUE 'FED USFW  '.
           05  FILLER  PIC X(10)  VALUE 'EUCLECLR  '.
       01  WS-SCHEME-TABLE REDEFINES WS-SCHEME-VALUES.
           05  WS-SCH-ENTRY            OCCURS 3 TIMES
                                       INDEXED BY WS-SCH-IDX.
               10  WS-SCH-DEPOSITORY   PIC X(04).
               10  WS-SCH-SCHEME       PIC X(06).
      *
      *----------------------------------------------------------------*
      * OPEN NEWM INSTRUCTIONS OF THE LAST 14 DAYS - FOR CANCELS THAT  *
      * DO NOT CARRY THE ORIGINAL TRADE ID (OMS)                       *
      *----------------------------------------------------------------*
       01  WS-OPEN-TABLE.
           05  WS-OPEN-COUNT           PIC S9(08) COMP VALUE ZERO.
           05  WS-OPEN-MAX             PIC S9(08) COMP VALUE 30000.
           05  WS-OPEN-ENTRY           OCCURS 30000 TIMES
                                       INDEXED BY WS-OPN-IDX.
               10  WS-OPN-REF          PIC X(16).
               10  WS-OPN-ACCT         PIC X(10).
               10  WS-OPN-CUSIP        PIC X(09).
               10  WS-OPN-MT           PIC X(03).
               10  WS-OPN-TRADE-DATE   PIC 9(08).
               10  WS-OPN-QTY          PIC S9(11)V9(04) COMP-3.
               10  WS-OPN-USED         PIC X(01).
       01  WS-LOOKBACK-DATE            PIC 9(08).
      *
      *----------------------------------------------------------------*
      * MESSAGE BEING BUILT                                            *
      *----------------------------------------------------------------*
       01  WS-MESSAGE.
           05  WS-MSG-LINE-COUNT       PIC S9(04) COMP VALUE ZERO.
           05  WS-MSG-LINE             PIC X(105) OCCURS 40 TIMES.
       01  WS-LINE                     PIC X(105).
       01  WS-MSG-FIELDS.
           05  WS-MT                   PIC X(03).
           05  WS-SEME                 PIC X(16).
           05  WS-SEME-LEN             PIC S9(04) COMP.
           05  WS-PREV-REF             PIC X(16).
           05  WS-PREV-LEN             PIC S9(04) COMP.
           05  WS-QTY-TYPE             PIC X(04).
           05  WS-QTY-TEXT             PIC X(35).
           05  WS-QTY-LEN              PIC S9(04) COMP.
           05  WS-AMT-TEXT             PIC X(35).
           05  WS-AMT-LEN              PIC S9(04) COMP.
           05  WS-AGENT-QUAL           PIC X(04).
           05  WS-PARTY-QUAL           PIC X(04).
           05  WS-SCHEME               PIC X(06).
           05  WS-FUNCTION             PIC X(04).
      *
       01  WS-TRIM-WORK.
           05  WS-TRIM-FIELD           PIC X(35).
           05  WS-TRIM-LEN             PIC S9(04) COMP.
      *
       01  WS-SECURITY-CACHE.
           05  WS-LAST-CUSIP           PIC X(09)  VALUE LOW-VALUES.
           05  WS-LAST-SEC-TYPE        PIC X(02)  VALUE SPACES.
           05  WS-LAST-SEC-FOUND       PIC X(01)  VALUE 'N'.
      *
       01  WS-COUNTERS.
           05  WS-INSTR-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MSG-WRITTEN          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEWM-SENT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-SENT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MT541-SENT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MT543-SENT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAMT-SENT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ALREADY-SENT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-RESENT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-NO-ORIG         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-SETTLED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-BY-ECON         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OPEN-LOADED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MASTER-ROWS          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SECM-LOOKUPS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SECM-NOTFOUND        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FMT-WARNINGS         PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-IN-AMT-TOTAL         PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-IN-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-AMT-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-SKIP-AMT-TOTAL       PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-SUB                  PIC S9(04) COMP.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DISP-SEQ             PIC Z(7)9.
           05  WS-SAVE-INSTR-REC       PIC X(250).
      *
       COPY CMSECMS.
       COPY CMSECLNK.
       COPY CMDTLNK.
       COPY SWFMLNK.
       COPY CMJILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-INSTR  THRU 2000-EXIT
               UNTIL WS-END-OF-INSTR
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
           READ DATECARD-FILE
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
           MOVE 'SWIFT INSTRUCTION BUILD STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           CALL 'CMASM01' USING JI-JOB-INFO
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1050-READ-PARM  THRU 1050-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
           DISPLAY 'SWB100 - SENDER ' WS-SENDER-LT
                   ' RECEIVER ' WS-RECEIVER-LT
                   ' RESEND REJECTED ' WS-RESEND-SW
      *
           OPEN INPUT INSTIN-FILE
           IF NOT INSTIN-OK
               MOVE 'INSTIN'           TO AB-DDNAME
               MOVE WS-INSTIN-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN I-O SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT MSGOUT-FILE
           IF NOT MSGOUT-OK
               MOVE 'MSGOUT'           TO AB-DDNAME
               MOVE WS-MSGOUT-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
      *
           PERFORM 1100-GET-SEQUENCE   THRU 1100-EXIT
           PERFORM 1200-LOAD-OPEN-INSTR THRU 1200-EXIT
           PERFORM 8000-READ-INSTR     THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1050-READ-PARM.
           READ PARMCARD
               AT END
                   SET WS-PARM-EOF     TO TRUE
                   GO TO 1050-EXIT
           END-READ
           IF PARM-CARD-REC (1:1) = '*'
           OR PARM-CARD-REC = SPACES
               GO TO 1050-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD
                                          WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'SENDER-LT'
                   MOVE WS-PARM-VALUE (1:12) TO WS-SENDER-LT
               WHEN 'RECEIVER-LT'
                   MOVE WS-PARM-VALUE (1:12) TO WS-RECEIVER-LT
               WHEN 'RESEND'
                   MOVE WS-PARM-VALUE (1:1)  TO WS-RESEND-SW
               WHEN OTHER
                   DISPLAY 'SWB100 - UNKNOWN PARAMETER IGNORED: '
                           PARM-CARD-REC (1:40)
           END-EVALUATE.
       1050-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1100 - LAST MESSAGE SEQUENCE NUMBER FROM THE CONTROL ROW.      *
      *        THE ROW IS REWRITTEN ONCE AT END OF RUN (9100).         *
      *----------------------------------------------------------------*
       1100-GET-SEQUENCE.
           MOVE WS-SEQ-CTL-KEY         TO SWI-SENDER-REF
           READ SWINSTR-FILE
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   MOVE SWI-MSG-SEQ    TO WS-MSG-SEQ
               WHEN SWINSTR-NOTFND
                   DISPLAY 'SWB100 - SEQUENCE CONTROL ROW NOT FOUND -'
                           ' CREATED, SEQUENCE STARTS AT 1'
                   INITIALIZE SWI-INSTR-REC
                   MOVE WS-SEQ-CTL-KEY TO SWI-SENDER-REF
                   MOVE 'CTL'          TO SWI-MSG-TYPE
                   MOVE ZERO           TO SWI-MSG-SEQ
                   MOVE DC-BUS-DATE    TO SWI-SENT-DATE
                                          SWI-LAST-STATUS-DATE
                   MOVE JI-JOBNAME     TO SWI-LAST-UPD-JOB
                   WRITE SWI-INSTR-REC
                   IF NOT SWINSTR-OK
                       MOVE 'SWINSTR'  TO AB-DDNAME
                       MOVE WS-SWINSTR-FS TO AB-FILE-STATUS
                       MOVE WS-SEQ-CTL-KEY TO AB-KEY
                       MOVE '1100-GET-SEQUENCE' TO AB-PARAGRAPH
                       PERFORM 9920-IO-ERROR THRU 9920-EXIT
                   END-IF
                   MOVE ZERO           TO WS-MSG-SEQ
                   SET WS-CTL-CREATED  TO TRUE
                   MOVE 4              TO WS-RETURN-CODE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE WS-SEQ-CTL-KEY TO AB-KEY
                   MOVE '1100-GET-SEQUENCE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           MOVE WS-MSG-SEQ             TO WS-FIRST-SEQ
           MOVE WS-MSG-SEQ             TO WS-DISP-SEQ
           DISPLAY 'SWB100 - LAST MESSAGE SEQUENCE ON FILE '
                   WS-DISP-SEQ.
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1200 - LOAD THE OPEN NEWM INSTRUCTIONS OF THE LAST 14 DAYS     *
      *----------------------------------------------------------------*
       1200-LOAD-OPEN-INSTR.
           MOVE 'ADDC'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE DC-BUS-DATE            TO DT-DATE-1
           MOVE -14                    TO DT-DAYS
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '1200-LOAD-OPEN-INSTR' TO AB-PARAGRAPH
               MOVE 'CMU010 ADDC FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE DT-RESULT-DATE         TO WS-LOOKBACK-DATE
      *
           MOVE LOW-VALUES             TO SWI-SENDER-REF
           START SWINSTR-FILE KEY IS NOT LESS THAN SWI-SENDER-REF
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   MOVE 'N'            TO WS-BROWSE-SW
               WHEN SWINSTR-NOTFND
                   SET WS-END-OF-BROWSE TO TRUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE '1200-LOAD-OPEN-INSTR' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           PERFORM UNTIL WS-END-OF-BROWSE
               READ SWINSTR-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN SWINSTR-OK
                       ADD 1           TO WS-MASTER-ROWS
                       PERFORM 1210-ADD-OPEN-ENTRY THRU 1210-EXIT
                   WHEN SWINSTR-EOF
                       SET WS-END-OF-BROWSE TO TRUE
                   WHEN OTHER
                       MOVE 'SWINSTR'  TO AB-DDNAME
                       MOVE WS-SWINSTR-FS TO AB-FILE-STATUS
                       MOVE '1200-LOAD-OPEN-INSTR' TO AB-PARAGRAPH
                       PERFORM 9920-IO-ERROR THRU 9920-EXIT
               END-EVALUATE
           END-PERFORM
           MOVE WS-OPEN-COUNT          TO WS-OPEN-LOADED
           MOVE WS-OPEN-COUNT          TO WS-DISP-COUNT
           DISPLAY 'SWB100 - OPEN INSTRUCTIONS LOADED FOR CANCEL '
                   'MATCHING ' WS-DISP-COUNT ' SINCE ' WS-LOOKBACK-DATE.
       1200-EXIT.
           EXIT.
      *
       1210-ADD-OPEN-ENTRY.
           IF SWI-SENDER-REF = WS-SEQ-CTL-KEY
               GO TO 1210-EXIT
           END-IF
           IF SWI-FUNCTION NOT = 'NEWM'
           OR SWI-CANCELLED
           OR SWI-TRADE-DATE < WS-LOOKBACK-DATE
               GO TO 1210-EXIT
           END-IF
           PERFORM 1220-STORE-ENTRY    THRU 1220-EXIT.
       1210-EXIT.
           EXIT.
      *
       1220-STORE-ENTRY.
           IF WS-OPEN-COUNT NOT < WS-OPEN-MAX
               MOVE 1007               TO AB-ABEND-CODE
               MOVE '1220-STORE-ENTRY' TO AB-PARAGRAPH
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE 'OPEN INSTRUCTION TABLE FULL - WS-OPEN-MAX'
                                       TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           ADD 1                       TO WS-OPEN-COUNT
           SET WS-OPN-IDX              TO WS-OPEN-COUNT
           MOVE SWI-SENDER-REF         TO WS-OPN-REF (WS-OPN-IDX)
           MOVE SWI-ACCT-NO            TO WS-OPN-ACCT (WS-OPN-IDX)
           MOVE SWI-CUSIP              TO WS-OPN-CUSIP (WS-OPN-IDX)
           MOVE SWI-MSG-TYPE           TO WS-OPN-MT (WS-OPN-IDX)
           MOVE SWI-TRADE-DATE         TO WS-OPN-TRADE-DATE (WS-OPN-IDX)
           MOVE SWI-QTY                TO WS-OPN-QTY (WS-OPN-IDX)
           MOVE 'N'                    TO WS-OPN-USED (WS-OPN-IDX).
       1220-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - ONE VALIDATED INSTRUCTION                               *
      *================================================================*
       2000-PROCESS-INSTR.
           ADD 1                       TO WS-INSTR-READ
           ADD SI-SETTLE-AMOUNT        TO WS-IN-AMT-TOTAL
           ADD SI-QTY                  TO WS-IN-QTY-HASH
           MOVE SI-MSG-TYPE (3:3)      TO WS-MT
           MOVE SI-FUNCTION            TO WS-FUNCTION
           MOVE SPACES                 TO WS-PREV-REF
           MOVE ZERO                   TO WS-PREV-LEN
      *
           IF SI-CANCEL-INSTR
               PERFORM 2200-CANCEL-INSTR THRU 2200-EXIT
           ELSE
               PERFORM 2100-NEW-INSTR  THRU 2100-EXIT
           END-IF
      *
           PERFORM 8000-READ-INSTR     THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2100 - NEWM: SENDER REFERENCE = TRADE ID                       *
      *----------------------------------------------------------------*
       2100-NEW-INSTR.
           MOVE 'N'                    TO WS-RESEND-THIS-SW
           MOVE SI-TRADE-ID            TO WS-SEME
           MOVE SI-TRADE-ID            TO SWI-SENDER-REF
           READ SWINSTR-FILE
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   IF SWI-REJECTED AND WS-RESEND-REJECTED
                       SET WS-RESEND-THIS TO TRUE
                   ELSE
                       ADD 1           TO WS-ALREADY-SENT
                       ADD SI-SETTLE-AMOUNT TO WS-SKIP-AMT-TOTAL
                       MOVE 4          TO WS-RETURN-CODE
                       DISPLAY 'SWB100 - ALREADY SENT, NOT RESENT: '
                               SI-TRADE-ID ' STATUS ' SWI-STATUS
                               ' SEQ ' SWI-MSG-SEQ
                       GO TO 2100-EXIT
                   END-IF
               WHEN SWINSTR-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE SI-TRADE-ID    TO AB-KEY
                   MOVE '2100-NEW-INSTR' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
      *
           PERFORM 3000-BUILD-MESSAGE  THRU 3000-EXIT
           PERFORM 4000-WRITE-MESSAGE  THRU 4000-EXIT
           IF WS-RESEND-THIS
               PERFORM 4200-REWRITE-RESENT THRU 4200-EXIT
               ADD 1                   TO WS-RESENT
           ELSE
               PERFORM 4100-WRITE-MASTER-ROW THRU 4100-EXIT
      *        SAME-DAY CANCELS MUST FIND IT
               PERFORM 1220-STORE-ENTRY THRU 1220-EXIT
           END-IF
           ADD 1                       TO WS-NEWM-SENT.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2200 - CANC: OWN REFERENCE = TRADE ID + 'C', PREV = ORIGINAL   *
      *----------------------------------------------------------------*
       2200-CANCEL-INSTR.
           MOVE SI-TRADE-ID            TO WS-TRIM-FIELD
           PERFORM 8900-TRIM-LENGTH    THRU 8900-EXIT
           MOVE SPACES                 TO WS-SEME
           IF WS-TRIM-LEN > 15
               MOVE 15                 TO WS-TRIM-LEN
           END-IF
           STRING SI-TRADE-ID (1:WS-TRIM-LEN) 'C'
                  DELIMITED BY SIZE INTO WS-SEME
           END-STRING
      *
           MOVE WS-SEME                TO SWI-SENDER-REF
           READ SWINSTR-FILE
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   ADD 1               TO WS-ALREADY-SENT
                   ADD SI-SETTLE-AMOUNT TO WS-SKIP-AMT-TOTAL
                   MOVE 4              TO WS-RETURN-CODE
                   DISPLAY 'SWB100 - CANCEL ALREADY SENT: ' WS-SEME
                   GO TO 2200-EXIT
               WHEN SWINSTR-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE WS-SEME        TO AB-KEY
                   MOVE '2200-CANCEL-INSTR' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
      *
           PERFORM 2210-FIND-ORIGINAL  THRU 2210-EXIT
           IF NOT WS-ORIG-FOUND
               ADD 1                   TO WS-CANC-NO-ORIG
               ADD SI-SETTLE-AMOUNT    TO WS-SKIP-AMT-TOTAL
               MOVE 4                  TO WS-RETURN-CODE
               DISPLAY 'SWB100 - CANCEL ' SI-TRADE-ID
                       ' - NO OPEN INSTRUCTION FOUND, NOT SENT'
                       ' (MANUAL CANCEL AT CUSTODIAN)'
               GO TO 2200-EXIT
           END-IF
      *    SWI-INSTR-REC NOW HOLDS THE ORIGINAL
           IF SWI-SETTLED
               ADD 1                   TO WS-CANC-SETTLED
               ADD SI-SETTLE-AMOUNT    TO WS-SKIP-AMT-TOTAL
               MOVE 4                  TO WS-RETURN-CODE
               DISPLAY 'SWB100 - CANCEL ' SI-TRADE-ID
                       ' - ORIGINAL ' WS-PREV-REF
                       ' ALREADY SETTLED, NOT SENT'
               GO TO 2200-EXIT
           END-IF
      *
           PERFORM 3000-BUILD-MESSAGE  THRU 3000-EXIT
           PERFORM 4000-WRITE-MESSAGE  THRU 4000-EXIT
           PERFORM 4300-CANCEL-ORIGINAL THRU 4300-EXIT
           PERFORM 4100-WRITE-MASTER-ROW THRU 4100-EXIT
           ADD 1                       TO WS-CANC-SENT.
       2200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2210 - THE INSTRUCTION BEING CANCELLED.  MANUAL CANCELS AND    *
      *        CORRECTION REVERSALS CARRY THE ORIGINAL TRADE ID; OMS   *
      *        CANCELS ARE MATCHED ON THE ECONOMICS (CHG27115).        *
      *----------------------------------------------------------------*
       2210-FIND-ORIGINAL.
           MOVE 'N'                    TO WS-ORIG-FOUND-SW
           MOVE SI-TRADE-ID            TO SWI-SENDER-REF
           READ SWINSTR-FILE
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   IF SWI-FUNCTION = 'NEWM'
                   AND NOT SWI-CANCELLED
                       MOVE SI-TRADE-ID TO WS-PREV-REF
                       SET WS-ORIG-FOUND TO TRUE
                       PERFORM 2230-MARK-TABLE-USED THRU 2230-EXIT
                       GO TO 2210-EXIT
                   END-IF
               WHEN SWINSTR-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE SI-TRADE-ID    TO AB-KEY
                   MOVE '2210-FIND-ORIGINAL' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
      *
           SET WS-OPN-IDX              TO 1
           SEARCH WS-OPEN-ENTRY VARYING WS-OPN-IDX
               AT END
                   GO TO 2210-EXIT
               WHEN WS-OPN-IDX > WS-OPEN-COUNT
                   GO TO 2210-EXIT
               WHEN WS-OPN-USED (WS-OPN-IDX) = 'N'
                AND WS-OPN-ACCT (WS-OPN-IDX) = SI-ACCT-NO
                AND WS-OPN-CUSIP (WS-OPN-IDX) = SI-CUSIP
                AND WS-OPN-MT (WS-OPN-IDX) = WS-MT
                AND WS-OPN-TRADE-DATE (WS-OPN-IDX) = SI-TRADE-DATE
                AND WS-OPN-QTY (WS-OPN-IDX) = SI-QTY
                   MOVE 'Y'            TO WS-OPN-USED (WS-OPN-IDX)
                   MOVE WS-OPN-REF (WS-OPN-IDX) TO WS-PREV-REF
           END-SEARCH
      *
           MOVE WS-PREV-REF            TO SWI-SENDER-REF
           READ SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-PREV-REF        TO AB-KEY
               MOVE '2210-FIND-ORIGINAL' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           SET WS-ORIG-FOUND           TO TRUE
           ADD 1                       TO WS-CANC-BY-ECON.
       2210-EXIT.
           EXIT.
      *
       2230-MARK-TABLE-USED.
           SET WS-OPN-IDX              TO 1
           SEARCH WS-OPEN-ENTRY VARYING WS-OPN-IDX
               AT END
                   CONTINUE
               WHEN WS-OPN-IDX > WS-OPEN-COUNT
                   CONTINUE
               WHEN WS-OPN-REF (WS-OPN-IDX) = SI-TRADE-ID
                   MOVE 'Y'            TO WS-OPN-USED (WS-OPN-IDX)
           END-SEARCH.
       2230-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - BUILD THE MESSAGE TEXT (WS-MSG-LINE TABLE)              *
      *================================================================*
       3000-BUILD-MESSAGE.
           MOVE ZERO                   TO WS-MSG-LINE-COUNT
           PERFORM 3100-SECURITY-TYPE  THRU 3100-EXIT
           PERFORM 3200-FORMAT-NUMBERS THRU 3200-EXIT
           IF WS-MT = '541'
               MOVE 'DEAG'             TO WS-AGENT-QUAL
               MOVE 'BUYR'             TO WS-PARTY-QUAL
           ELSE
               MOVE 'REAG'             TO WS-AGENT-QUAL
               MOVE 'SELL'             TO WS-PARTY-QUAL
           END-IF
      *
      *    BASIC / APPLICATION HEADER
           MOVE SPACES                 TO WS-LINE
           STRING '{1:F01' WS-SENDER-LT WS-SESSION-ISN
                  '}{2:I' WS-MT WS-RECEIVER-LT 'N}{4:'
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
      *
      *    GENERAL INFORMATION
           MOVE ':16R:GENL'            TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE WS-SEME                TO WS-TRIM-FIELD
           PERFORM 8900-TRIM-LENGTH    THRU 8900-EXIT
           MOVE WS-TRIM-LEN            TO WS-SEME-LEN
           MOVE SPACES                 TO WS-LINE
           STRING ':20C::SEME//' WS-SEME (1:WS-SEME-LEN)
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':23G:' WS-FUNCTION DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           IF SI-CANCEL-INSTR
               MOVE ':16R:LINK'        TO WS-LINE
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
               MOVE WS-PREV-REF        TO WS-TRIM-FIELD
               PERFORM 8900-TRIM-LENGTH THRU 8900-EXIT
               MOVE WS-TRIM-LEN        TO WS-PREV-LEN
               MOVE SPACES             TO WS-LINE
               STRING ':20C::PREV//' WS-PREV-REF (1:WS-PREV-LEN)
                      DELIMITED BY SIZE INTO WS-LINE
               END-STRING
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
               MOVE ':16S:LINK'        TO WS-LINE
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
           END-IF
           MOVE ':16S:GENL'            TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
      *
      *    TRADE DETAILS
           MOVE ':16R:TRADDET'         TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':98A::SETT//' SI-SETTLE-DATE
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':98A::TRAD//' SI-TRADE-DATE
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           IF SI-ISIN = SPACES
               MOVE SPACES             TO WS-LINE
               STRING ':35B:/US/' SI-CUSIP
                      DELIMITED BY SIZE INTO WS-LINE
               END-STRING
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
           ELSE
               MOVE SPACES             TO WS-LINE
               STRING ':35B:ISIN ' SI-ISIN
                      DELIMITED BY SIZE INTO WS-LINE
               END-STRING
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
               MOVE SPACES             TO WS-LINE
               STRING '/US/' SI-CUSIP DELIMITED BY SIZE INTO WS-LINE
               END-STRING
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
           END-IF
           MOVE ':16S:TRADDET'         TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
      *
      *    FINANCIAL INSTRUMENT / ACCOUNT
           MOVE ':16R:FIAC'            TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':36B::SETT//' WS-QTY-TYPE '/'
                  WS-QTY-TEXT (1:WS-QTY-LEN)
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':97A::SAFE//' SI-SAFEKEEPING-ACCT
                  DELIMITED BY SPACE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE ':16S:FIAC'            TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
      *
      *    SETTLEMENT DETAILS
           MOVE ':16R:SETDET'          TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE ':22F::SETR//TRAD'     TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           PERFORM 3300-SETTLEMENT-PARTIES THRU 3300-EXIT
           MOVE ':16R:AMT'             TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':19A::SETT//' WS-AMT-TEXT (1:WS-AMT-LEN)
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE ':16S:AMT'             TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE ':16S:SETDET'          TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE '-}'                   TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3100 - SECURITY TYPE: BONDS ARE INSTRUCTED IN FACE AMOUNT.     *
      *        UNKNOWN CUSIP: FED BOOK-ENTRY IS ALWAYS FACE AMOUNT.    *
      *----------------------------------------------------------------*
       3100-SECURITY-TYPE.
           IF SI-CUSIP NOT = WS-LAST-CUSIP
               MOVE SI-CUSIP           TO WS-LAST-CUSIP
               MOVE 'GET '             TO SL-FUNCTION
               MOVE SI-CUSIP           TO SL-KEY-CUSIP
               MOVE SPACES             TO SL-KEY-ISIN SL-KEY-SYMBOL
               CALL 'CMD010' USING SL-SECURITY-PARMS
               ADD 1                   TO WS-SECM-LOOKUPS
               EVALUATE TRUE
                   WHEN SL-FOUND
                       MOVE SL-SEC-DATA TO SEC-MASTER-REC
                       MOVE SEC-TYPE   TO WS-LAST-SEC-TYPE
                       MOVE 'Y'        TO WS-LAST-SEC-FOUND
                   WHEN SL-NOT-FOUND
                       MOVE SPACES     TO WS-LAST-SEC-TYPE
                       MOVE 'N'        TO WS-LAST-SEC-FOUND
                       ADD 1           TO WS-SECM-NOTFOUND
                   WHEN OTHER
                       MOVE SL-SQLCODE TO AB-SQLCODE
                       MOVE SI-CUSIP   TO AB-KEY
                       MOVE 1003       TO AB-ABEND-CODE
                       MOVE '3100-SECURITY-TYPE' TO AB-PARAGRAPH
                       MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                       PERFORM 9999-ABEND THRU 9999-EXIT
               END-EVALUATE
           END-IF
           MOVE 'UNIT'                 TO WS-QTY-TYPE
           IF WS-LAST-SEC-FOUND = 'Y'
               IF WS-LAST-SEC-TYPE = 'CB' OR 'MU' OR 'GV'
                   MOVE 'FAMT'         TO WS-QTY-TYPE
               END-IF
           ELSE
               IF SI-DEPOSITORY = 'FED '
                   MOVE 'FAMT'         TO WS-QTY-TYPE
               END-IF
           END-IF.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3200 - QUANTITY AND AMOUNT IN ISO 15022 FORMAT                 *
      *----------------------------------------------------------------*
       3200-FORMAT-NUMBERS.
           MOVE 'QTY '                 TO FM-FUNCTION
           MOVE SI-QTY                 TO FM-QTY-IN
           MOVE SPACES                 TO FM-CCY
           CALL 'SWU010' USING FM-PARMS
           IF FM-RETURN-CODE > 04
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '3200-FORMAT-NUMBERS' TO AB-PARAGRAPH
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE 'SWU010 QTY FORMAT FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           IF FM-WARNING
               ADD 1                   TO WS-FMT-WARNINGS
           END-IF
           MOVE FM-TEXT                TO WS-QTY-TEXT
           MOVE FM-TEXT-LEN            TO WS-QTY-LEN
      *
           MOVE 'AMT '                 TO FM-FUNCTION
           MOVE SI-CCY                 TO FM-CCY
           MOVE SI-SETTLE-AMOUNT       TO FM-AMT-IN
           CALL 'SWU010' USING FM-PARMS
           IF FM-RETURN-CODE > 04
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '3200-FORMAT-NUMBERS' TO AB-PARAGRAPH
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE 'SWU010 AMT FORMAT FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           IF FM-WARNING
               ADD 1                   TO WS-FMT-WARNINGS
           END-IF
           MOVE FM-TEXT                TO WS-AMT-TEXT
           MOVE FM-TEXT-LEN            TO WS-AMT-LEN.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3300 - SETTLEMENT PARTIES: PLACE OF SETTLEMENT, CONTRA         *
      *        PARTICIPANT (IF KNOWN), BUYER / SELLER (OUR AGENT)      *
      *----------------------------------------------------------------*
       3300-SETTLEMENT-PARTIES.
           MOVE ':16R:SETPRTY'         TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':95P::PSET//' SI-PLACE-BIC
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE ':16S:SETPRTY'         TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
      *
           IF SI-CONTRA NOT = SPACES
               MOVE 'DTCYID'           TO WS-SCHEME
               SET WS-SCH-IDX          TO 1
               SEARCH WS-SCH-ENTRY
                   AT END
                       CONTINUE
                   WHEN WS-SCH-DEPOSITORY (WS-SCH-IDX) = SI-DEPOSITORY
                       MOVE WS-SCH-SCHEME (WS-SCH-IDX) TO WS-SCHEME
               END-SEARCH
               MOVE ':16R:SETPRTY'     TO WS-LINE
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
               MOVE SPACES             TO WS-LINE
               STRING ':95R::' WS-AGENT-QUAL '/'
                      WS-SCHEME DELIMITED BY SPACE
                      '/' SI-CONTRA DELIMITED BY SIZE
                      INTO WS-LINE
               END-STRING
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
               MOVE ':16S:SETPRTY'     TO WS-LINE
               PERFORM 3900-ADD-LINE   THRU 3900-EXIT
           END-IF
      *
           MOVE ':16R:SETPRTY'         TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE SPACES                 TO WS-LINE
           STRING ':95P::' WS-PARTY-QUAL '//' SI-AGENT-BIC
                  DELIMITED BY SIZE INTO WS-LINE
           END-STRING
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT
           MOVE ':16S:SETPRTY'         TO WS-LINE
           PERFORM 3900-ADD-LINE       THRU 3900-EXIT.
       3300-EXIT.
           EXIT.
      *
       3900-ADD-LINE.
           IF WS-MSG-LINE-COUNT NOT < 40
               MOVE 1007               TO AB-ABEND-CODE
               MOVE '3900-ADD-LINE'    TO AB-PARAGRAPH
               MOVE WS-SEME            TO AB-KEY
               MOVE 'MORE THAN 40 LINES IN ONE MESSAGE' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           ADD 1                       TO WS-MSG-LINE-COUNT
           MOVE WS-LINE          TO WS-MSG-LINE (WS-MSG-LINE-COUNT)
           MOVE SPACES                 TO WS-LINE.
       3900-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - WRITE THE MESSAGE UNDER THE NEXT SEQUENCE NUMBER        *
      *================================================================*
       4000-WRITE-MESSAGE.
           IF WS-MSG-SEQ NOT < WS-SEQ-MAX
               MOVE 1                  TO WS-MSG-SEQ
               DISPLAY 'SWB100 - MESSAGE SEQUENCE WRAPPED TO 1'
           ELSE
               ADD 1                   TO WS-MSG-SEQ
           END-IF
           PERFORM VARYING WS-SUB FROM 1 BY 1
                     UNTIL WS-SUB > WS-MSG-LINE-COUNT
               MOVE WS-MSG-SEQ         TO SWM-MSG-SEQ
               MOVE WS-SUB             TO SWM-LINE-NO
               MOVE WS-MT              TO SWM-MSG-TYPE
               SET SWM-OUTBOUND        TO TRUE
               MOVE WS-MSG-LINE (WS-SUB) TO SWM-TEXT
               WRITE SWM-MSG-LINE
               IF NOT MSGOUT-OK
                   MOVE 'MSGOUT'       TO AB-DDNAME
                   MOVE WS-MSGOUT-FS   TO AB-FILE-STATUS
                   MOVE WS-SEME        TO AB-KEY
                   MOVE '4000-WRITE-MESSAGE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
               END-IF
               ADD 1                   TO WS-LINES-WRITTEN
           END-PERFORM
           ADD 1                       TO WS-MSG-WRITTEN
           ADD SI-SETTLE-AMOUNT        TO WS-OUT-AMT-TOTAL
           ADD SI-QTY                  TO WS-OUT-QTY-HASH
           IF WS-MT = '541'
               ADD 1                   TO WS-MT541-SENT
           ELSE
               ADD 1                   TO WS-MT543-SENT
           END-IF
           IF WS-QTY-TYPE = 'FAMT'
               ADD 1                   TO WS-FAMT-SENT
           END-IF.
       4000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4100 - NEW INSTRUCTION MASTER ROW, STATUS SN                   *
      *----------------------------------------------------------------*
       4100-WRITE-MASTER-ROW.
           INITIALIZE SWI-INSTR-REC
           MOVE WS-SEME                TO SWI-SENDER-REF
           MOVE WS-MT                  TO SWI-MSG-TYPE
           MOVE SI-FUNCTION            TO SWI-FUNCTION
           SET SWI-SENT                TO TRUE
           MOVE SI-ACCT-NO             TO SWI-ACCT-NO
           MOVE SI-CUSIP               TO SWI-CUSIP
           MOVE SI-ISIN                TO SWI-ISIN
           MOVE SI-TRADE-DATE          TO SWI-TRADE-DATE
           MOVE SI-SETTLE-DATE         TO SWI-SETTLE-DATE
           MOVE SI-QTY                 TO SWI-QTY
           MOVE SI-SETTLE-AMOUNT       TO SWI-AMOUNT
           MOVE SI-CCY                 TO SWI-CCY
           MOVE SI-DEPOSITORY          TO SWI-DEPOSITORY
           MOVE DC-BUS-DATE            TO SWI-SENT-DATE
                                          SWI-LAST-STATUS-DATE
           MOVE WS-MSG-SEQ             TO SWI-MSG-SEQ
           MOVE WS-MT                  TO SWI-LAST-STATUS-MSG
           MOVE SPACES                 TO SWI-STATUS-CODE
                                          SWI-REASON-CODE
           MOVE ZERO                   TO SWI-SETTLED-QTY
                                          SWI-SETTLED-AMOUNT
                                          SWI-EFF-SETTLE-DATE
                                          SWI-FAIL-AGE
                                          SWI-CLOSEOUT-DATE
           MOVE 'N'                    TO SWI-BUYIN-FLAG
      *    CANC ROWS KEEP THE REFERENCE OF THE CANCELLED INSTRUCTION
           IF SI-CANCEL-INSTR
               MOVE WS-PREV-REF        TO SWI-CUST-REF
           ELSE
               MOVE SPACES             TO SWI-CUST-REF
           END-IF
           MOVE JI-JOBNAME             TO SWI-LAST-UPD-JOB
           WRITE SWI-INSTR-REC
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-SEME            TO AB-KEY
               MOVE '4100-WRITE-MASTER-ROW' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       4100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4200 - REJECTED INSTRUCTION SENT AGAIN (RESEND=Y)              *
      *----------------------------------------------------------------*
       4200-REWRITE-RESENT.
           MOVE SI-TRADE-ID            TO SWI-SENDER-REF
           READ SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE '4200-REWRITE-RESENT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           SET SWI-SENT                TO TRUE
           MOVE SI-QTY                 TO SWI-QTY
           MOVE SI-SETTLE-AMOUNT       TO SWI-AMOUNT
           MOVE SI-SETTLE-DATE         TO SWI-SETTLE-DATE
           MOVE DC-BUS-DATE            TO SWI-SENT-DATE
                                          SWI-LAST-STATUS-DATE
           MOVE WS-MSG-SEQ             TO SWI-MSG-SEQ
           MOVE WS-MT                  TO SWI-LAST-STATUS-MSG
           MOVE SPACES                 TO SWI-STATUS-CODE
                                          SWI-REASON-CODE
           MOVE JI-JOBNAME             TO SWI-LAST-UPD-JOB
           REWRITE SWI-INSTR-REC
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE '4200-REWRITE-RESENT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           DISPLAY 'SWB100 - REJECTED INSTRUCTION RESENT: '
                   SI-TRADE-ID ' NEW SEQ ' WS-MSG-SEQ.
       4200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4300 - THE CANCELLED INSTRUCTION IS CLOSED WHEN THE CANC GOES  *
      *        OUT - THE CUSTODIAN CONFIRMS BY EXCEPTION ONLY          *
      *----------------------------------------------------------------*
       4300-CANCEL-ORIGINAL.
           MOVE WS-PREV-REF            TO SWI-SENDER-REF
           READ SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-PREV-REF        TO AB-KEY
               MOVE '4300-CANCEL-ORIGINAL' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           SET SWI-CANCELLED           TO TRUE
           MOVE 'CANC'                 TO SWI-STATUS-CODE
           MOVE DC-BUS-DATE            TO SWI-LAST-STATUS-DATE
           MOVE JI-JOBNAME             TO SWI-LAST-UPD-JOB
           REWRITE SWI-INSTR-REC
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-PREV-REF        TO AB-KEY
               MOVE '4300-CANCEL-ORIGINAL' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       4300-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O AND UTILITIES                                       *
      *================================================================*
       8000-READ-INSTR.
           READ INSTIN-FILE
           EVALUATE TRUE
               WHEN INSTIN-OK
                   CONTINUE
               WHEN INSTIN-EOF
                   SET WS-END-OF-INSTR TO TRUE
               WHEN OTHER
                   MOVE 'INSTIN'       TO AB-DDNAME
                   MOVE WS-INSTIN-FS   TO AB-FILE-STATUS
                   MOVE '8000-READ-INSTR' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *    SIGNIFICANT LENGTH OF WS-TRIM-FIELD (0 WHEN BLANK)
       8900-TRIM-LENGTH.
           MOVE 35                     TO WS-TRIM-LEN
           PERFORM UNTIL WS-TRIM-LEN < 1
                      OR WS-TRIM-FIELD (WS-TRIM-LEN:1) NOT = SPACE
               SUBTRACT 1              FROM WS-TRIM-LEN
           END-PERFORM
           IF WS-TRIM-LEN < 1
               MOVE 1                  TO WS-TRIM-LEN
           END-IF.
       8900-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           PERFORM 9100-UPDATE-SEQUENCE THRU 9100-EXIT
           CLOSE INSTIN-FILE
                 SWINSTR-FILE
                 MSGOUT-FILE
           IF NOT INSTIN-OK OR NOT SWINSTR-OK OR NOT MSGOUT-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-INSTIN-FS ' '
                      WS-SWINSTR-FS ' ' WS-MSGOUT-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'INSTR-IN'             TO CT-COUNTER-NAME
           MOVE WS-INSTR-READ          TO CT-COUNT
           MOVE WS-IN-AMT-TOTAL        TO CT-AMOUNT
           MOVE WS-IN-QTY-HASH         TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'MSG-OUT'              TO CT-COUNTER-NAME
           MOVE WS-MSG-WRITTEN         TO CT-COUNT
           MOVE WS-OUT-AMT-TOTAL       TO CT-AMOUNT
           MOVE WS-OUT-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CANC-OUT'             TO CT-COUNTER-NAME
           MOVE WS-CANC-SENT           TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'LINES-OUT'            TO CT-COUNTER-NAME
           MOVE WS-LINES-WRITTEN       TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'INSTR-SKIPPED'        TO CT-COUNTER-NAME
           COMPUTE CT-COUNT = WS-ALREADY-SENT + WS-CANC-NO-ORIG
                            + WS-CANC-SETTLED
           MOVE WS-SKIP-AMT-TOTAL      TO CT-AMOUNT
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
           MOVE WS-MSG-WRITTEN         TO WS-DISP-COUNT
           MOVE WS-MSG-SEQ             TO WS-DISP-SEQ
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'SWIFT BUILD ENDED. MESSAGES ' WS-DISP-COUNT
                  ' LAST SEQ ' WS-DISP-SEQ
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* SWB100 - SWIFT INSTRUCTION BUILD   ' DC-BUS-DATE
           DISPLAY '*************************************************'
           MOVE WS-INSTR-READ          TO WS-DISP-COUNT
           DISPLAY '* INSTRUCTIONS READ         : ' WS-DISP-COUNT
           MOVE WS-MSG-WRITTEN         TO WS-DISP-COUNT
           DISPLAY '* MESSAGES WRITTEN          : ' WS-DISP-COUNT
           MOVE WS-MT541-SENT          TO WS-DISP-COUNT
           DISPLAY '*   MT541                   : ' WS-DISP-COUNT
           MOVE WS-MT543-SENT          TO WS-DISP-COUNT
           DISPLAY '*   MT543                   : ' WS-DISP-COUNT
           MOVE WS-NEWM-SENT           TO WS-DISP-COUNT
           DISPLAY '*   NEWM                    : ' WS-DISP-COUNT
           MOVE WS-CANC-SENT           TO WS-DISP-COUNT
           DISPLAY '*   CANC                    : ' WS-DISP-COUNT
           MOVE WS-CANC-BY-ECON        TO WS-DISP-COUNT
           DISPLAY '*     MATCHED ON ECONOMICS  : ' WS-DISP-COUNT
           MOVE WS-FAMT-SENT           TO WS-DISP-COUNT
           DISPLAY '*   FACE AMOUNT (FAMT)      : ' WS-DISP-COUNT
           MOVE WS-RESENT              TO WS-DISP-COUNT
           DISPLAY '*   REJECTED AND RESENT     : ' WS-DISP-COUNT
           MOVE WS-LINES-WRITTEN       TO WS-DISP-COUNT
           DISPLAY '* MESSAGE LINES WRITTEN     : ' WS-DISP-COUNT
           MOVE WS-ALREADY-SENT        TO WS-DISP-COUNT
           DISPLAY '* ALREADY SENT (SKIPPED)    : ' WS-DISP-COUNT
           MOVE WS-CANC-NO-ORIG        TO WS-DISP-COUNT
           DISPLAY '* CANCEL WITHOUT ORIGINAL   : ' WS-DISP-COUNT
           MOVE WS-CANC-SETTLED        TO WS-DISP-COUNT
           DISPLAY '* CANCEL OF SETTLED INSTR   : ' WS-DISP-COUNT
           MOVE WS-MASTER-ROWS         TO WS-DISP-COUNT
           DISPLAY '* MASTER ROWS AT START      : ' WS-DISP-COUNT
           MOVE WS-SECM-NOTFOUND       TO WS-DISP-COUNT
           DISPLAY '* CUSIP NOT ON SEC MASTER   : ' WS-DISP-COUNT
           MOVE WS-FIRST-SEQ           TO WS-DISP-SEQ
           DISPLAY '* FIRST SEQUENCE USED AFTER : ' WS-DISP-SEQ
           MOVE WS-MSG-SEQ             TO WS-DISP-SEQ
           DISPLAY '* LAST SEQUENCE USED        : ' WS-DISP-SEQ
           MOVE WS-OUT-AMT-TOTAL       TO WS-DISP-AMT
           DISPLAY '* AMOUNT INSTRUCTED         : ' WS-DISP-AMT
           DISPLAY '* RETURN CODE               : ' WS-RETURN-CODE
           DISPLAY '*************************************************'.
       9000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 9100 - STORE THE LAST SEQUENCE NUMBER USED                     *
      *----------------------------------------------------------------*
       9100-UPDATE-SEQUENCE.
           MOVE WS-SEQ-CTL-KEY         TO SWI-SENDER-REF
           READ SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-SEQ-CTL-KEY     TO AB-KEY
               MOVE '9100-UPDATE-SEQUENCE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           MOVE WS-MSG-SEQ             TO SWI-MSG-SEQ
           MOVE DC-BUS-DATE            TO SWI-SENT-DATE
                                          SWI-LAST-STATUS-DATE
           MOVE JI-JOBNAME             TO SWI-LAST-UPD-JOB
           REWRITE SWI-INSTR-REC
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE WS-SEQ-CTL-KEY     TO AB-KEY
               MOVE '9100-UPDATE-SEQUENCE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       9100-EXIT.
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
           DISPLAY 'SWB100 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWB100 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
