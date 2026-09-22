       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWR110.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  11/18/2002.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWR110                                            *
      * TITLE      : SWIFT OUTBOUND MESSAGE JOURNAL                    *
      * JOB        : MSSWD010  STEP030                                 *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   READS BACK THE GATEWAY FILE WRITTEN BY SWB100 AND PRINTS     *
      *   ONE LINE PER MESSAGE AS IT WILL BE SEEN BY THE CUSTODIAN:    *
      *   SEQUENCE, MT, FUNCTION, SENDER REFERENCE, LINKED REFERENCE,  *
      *   ISIN, SETTLE DATE, QUANTITY, AMOUNT, PLACE OF SETTLEMENT.    *
      *   THE FIELDS ARE TAKEN FROM THE MESSAGE TEXT (NOT FROM THE     *
      *   INSTRUCTION FILE) SO THE JOURNAL PROVES WHAT WAS SENT.       *
      *   STRUCTURE CHECKS: BLOCK HEADER ON LINE 1, CONSECUTIVE LINE   *
      *   NUMBERS, '-}' TRAILER, CONTIGUOUS MESSAGE SEQUENCE NUMBERS.  *
      *   CANCELLATIONS (DETAIL=CANC) OR ALL MESSAGES (DETAIL=ALL) ARE *
      *   ALSO PRINTED IN FULL.  TOTALS BY MT/FUNCTION AND CURRENCY.   *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETERS SWP110A (DETAIL=)                *
      *          MSGIN     MSEC.PROD.SW.OUTMSG(+1)        (SWMSG)      *
      * OUTPUT : RPTFILE   JOURNAL SWR110 (FB 133 ASA)                 *
      * CALLS  : SWU010 CMU050 CMU060 CMU080 CMASM02                   *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 STRUCTURE ERROR OR SEQUENCE GAP       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 2002-11-18 KAP  CHG09977  ORIGINAL - REPLACES GATEWAY PRINTOUT *
      * 2005-08-30 KAP  CHG13391  ISIN COLUMN                          *
      * 2009-12-14 SPA  CHG19002  TOTALS BY CURRENCY                   *
      * 2014-05-05 SPA  CHG27115  PREV REFERENCE, DETAIL=CANC          *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-Z15.
       OBJECT-COMPUTER.  IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD      ASSIGN TO SYSIN
                                FILE STATUS IS WS-PARMCARD-FS.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT MSGIN-FILE    ASSIGN TO MSGIN
                                FILE STATUS IS WS-MSGIN-FS.
           SELECT RPTFILE       ASSIGN TO RPTFILE
                                FILE STATUS IS WS-RPTFILE-FS.
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
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWR110'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-MSGIN-FS             PIC X(02).
               88  MSGIN-OK                      VALUE '00'.
               88  MSGIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-FS           PIC X(02).
               88  RPTFILE-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-LINES               VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-DETAIL-OPT           PIC X(04)  VALUE 'CANC'.
               88  WS-DETAIL-NONE                VALUE 'NONE'.
               88  WS-DETAIL-CANC                VALUE 'CANC'.
               88  WS-DETAIL-ALL                 VALUE 'ALL '.
           05  WS-STRUCT-SW            PIC X(01)  VALUE 'N'.
               88  WS-STRUCT-ERROR               VALUE 'Y'.
      *
       01  WS-PARM-KEYWORD             PIC X(20).
       01  WS-PARM-VALUE               PIC X(20).
      *
       COPY SWMSG.
      *
      *----------------------------------------------------------------*
      * CURRENT MESSAGE                                                *
      *----------------------------------------------------------------*
       01  WS-MESSAGE.
           05  WS-CUR-SEQ              PIC 9(08).
           05  WS-CUR-MT               PIC X(03).
           05  WS-CUR-LINES            PIC S9(04) COMP VALUE ZERO.
           05  WS-CUR-LINE-TEXT        PIC X(105) OCCURS 60 TIMES.
       01  WS-MSG-FIELDS.
           05  WS-F-FUNCTION           PIC X(04).
           05  WS-F-SEME               PIC X(16).
           05  WS-F-PREV               PIC X(16).
           05  WS-F-ISIN               PIC X(12).
           05  WS-F-SETTLE             PIC X(08).
           05  WS-F-QTY-TYPE           PIC X(04).
           05  WS-F-QTY-TEXT           PIC X(35).
           05  WS-F-CCY                PIC X(03).
           05  WS-F-AMT-TEXT           PIC X(35).
           05  WS-F-PSET               PIC X(11).
           05  WS-F-QTY                PIC S9(15)V9(04) COMP-3.
           05  WS-F-AMT                PIC S9(15)V9(04) COMP-3.
           05  WS-F-TRAILER-SW         PIC X(01).
               88  WS-F-HAS-TRAILER              VALUE 'Y'.
       01  WS-TAG-WORK.
           05  WS-TAG-LINE             PIC X(105).
           05  WS-TAG-EMPTY            PIC X(10).
           05  WS-TAG                  PIC X(04).
           05  WS-TAG-VALUE            PIC X(100).
           05  WS-TAG-QUAL             PIC X(04).
           05  WS-TAG-DATA             PIC X(95).
           05  WS-TAG-SUB1             PIC X(35).
           05  WS-TAG-SUB2             PIC X(35).
      *
      *----------------------------------------------------------------*
      * TOTALS BY MT / FUNCTION AND BY CURRENCY                        *
      *----------------------------------------------------------------*
       01  WS-TYPE-TOTALS.
           05  WS-TT-ENTRY             OCCURS 4 TIMES.
               10  WS-TT-MT            PIC X(03).
               10  WS-TT-FUNC          PIC X(04).
               10  WS-TT-COUNT         PIC S9(07) COMP-3.
       01  WS-TT-INIT-VALUES           PIC X(28)
                                VALUE '541NEWM541CANC543NEWM543CANC'.
       01  WS-TT-SUB                   PIC S9(04) COMP.
       01  WS-CCY-TOTALS.
           05  WS-CT-COUNT-USED        PIC S9(04) COMP VALUE ZERO.
           05  WS-CT-ENTRY             OCCURS 12 TIMES
                                       INDEXED BY WS-CT-IDX.
               10  WS-CT-CCY           PIC X(03).
               10  WS-CT-MSGS          PIC S9(07) COMP-3.
               10  WS-CT-RCV-AMT       PIC S9(15)V99 COMP-3.
               10  WS-CT-DLV-AMT       PIC S9(15)V99 COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-LINES-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MSGS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MSGS-PRINTED-FULL    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STRUCT-ERRORS        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEQ-GAPS             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NUM-ERRORS           PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-SEQ-WORK.
           05  WS-FIRST-SEQ            PIC 9(08)  VALUE ZERO.
           05  WS-LAST-SEQ             PIC 9(08)  VALUE ZERO.
           05  WS-EXPECT-SEQ           PIC 9(08)  VALUE ZERO.
       01  WS-WORK-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-SUB                  PIC S9(04) COMP.
           05  WS-EXPECT-LINE          PIC 9(03).
           05  WS-TOTAL-AMT            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-ERR-TEXT             PIC X(40).
      *
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(10)  VALUE ' MSG SEQ'.
           05  FILLER  PIC X(04)  VALUE 'MT'.
           05  FILLER  PIC X(05)  VALUE 'FUNC'.
           05  FILLER  PIC X(17)  VALUE 'SENDER REF'.
           05  FILLER  PIC X(17)  VALUE 'PREVIOUS REF'.
           05  FILLER  PIC X(13)  VALUE 'ISIN'.
           05  FILLER  PIC X(11)  VALUE 'SETTLE'.
           05  FILLER  PIC X(05)  VALUE 'TYPE'.
           05  FILLER  PIC X(18)  VALUE '         QUANTITY'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(16)  VALUE '         AMOUNT'.
           05  FILLER  PIC X(12)  VALUE 'PSET'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(10)  VALUE ' --------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(13)  VALUE '------------'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(18)  VALUE ' ----------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(16)  VALUE ' --------------'.
           05  FILLER  PIC X(12)  VALUE '-----------'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-SEQ                  PIC 9(08).
           05  FILLER                  PIC X(01).
           05  DL-MT                   PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-FUNC                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-SEME                 PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-PREV                 PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-ISIN                 PIC X(12).
           05  FILLER                  PIC X(01).
           05  DL-SETTLE               PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-QTY-TYPE             PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-AMT                  PIC ZZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-PSET                 PIC X(11).
       01  WS-TEXT-LINE.
           05  TX-CC                   PIC X(01).
           05  FILLER                  PIC X(12).
           05  TX-LINE-NO              PIC 9(03).
           05  FILLER                  PIC X(02).
           05  TX-TEXT                 PIC X(105).
           05  FILLER                  PIC X(10).
       01  WS-ERROR-LINE.
           05  EL-CC                   PIC X(01).
           05  FILLER                  PIC X(10).
           05  FILLER                  PIC X(12)  VALUE '*** ERROR: '.
           05  EL-TEXT                 PIC X(40).
           05  FILLER                  PIC X(10)  VALUE ' MSG SEQ '.
           05  EL-SEQ                  PIC 9(08).
           05  FILLER                  PIC X(52).
       01  WS-TT-LINE.
           05  TT-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  FILLER                  PIC X(03)  VALUE 'MT'.
           05  TT-MT                   PIC X(03).
           05  FILLER                  PIC X(02).
           05  TT-FUNC                 PIC X(04).
           05  FILLER                  PIC X(05).
           05  TT-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(99).
       01  WS-CT-HEAD.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(05)  VALUE SPACES.
           05  FILLER  PIC X(08)  VALUE 'CCY'.
           05  FILLER  PIC X(12)  VALUE '    MESSAGES'.
           05  FILLER  PIC X(24)  VALUE '      RECEIVE (MT541)'.
           05  FILLER  PIC X(24)  VALUE '      DELIVER (MT543)'.
           05  FILLER  PIC X(59)  VALUE SPACES.
       01  WS-CT-LINE.
           05  CT-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  CT-CCY                  PIC X(03).
           05  FILLER                  PIC X(05).
           05  CT-MSGS                 PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  CT-RCV                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  CT-DLV                  PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(64).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  TL-LABEL                PIC X(36).
           05  TL-VALUE                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(80).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO MESSAGES SENT FOR THIS BUSINESS DATE ***'.
      *
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY SWFMLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-MESSAGE THRU 2000-EXIT
               UNTIL WS-END-OF-LINES
           PERFORM 7000-PRINT-TOTALS   THRU 7000-EXIT
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
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'MESSAGE JOURNAL STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM  THRU 1100-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
      *
           PERFORM VARYING WS-TT-SUB FROM 1 BY 1 UNTIL WS-TT-SUB > 4
               MOVE WS-TT-INIT-VALUES ((WS-TT-SUB - 1) * 7 + 1:3)
                                       TO WS-TT-MT (WS-TT-SUB)
               MOVE WS-TT-INIT-VALUES ((WS-TT-SUB - 1) * 7 + 4:4)
                                       TO WS-TT-FUNC (WS-TT-SUB)
               MOVE ZERO               TO WS-TT-COUNT (WS-TT-SUB)
           END-PERFORM
      *
           OPEN INPUT MSGIN-FILE
           IF NOT MSGIN-OK
               MOVE 'MSGIN'            TO AB-DDNAME
               MOVE WS-MSGIN-FS        TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP (1:10)    TO RPT-H1-RUN-DATE
           MOVE 'SWR110'               TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'SWIFT OUTBOUND MESSAGE JOURNAL - MT541 / MT543'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-DATE            TO WS-DATE-IN
           PERFORM 8300-EDIT-DATE      THRU 8300-EXIT
           MOVE WS-DATE-EDIT           TO RPT-H2-BUS-DATE
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
           PERFORM 8000-READ-LINE      THRU 8000-EXIT
           IF WS-END-OF-LINES
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2                   TO RPT-LINE-COUNT
           END-IF.
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
           IF WS-PARM-KEYWORD = 'DETAIL'
               MOVE WS-PARM-VALUE (1:4) TO WS-DETAIL-OPT
           ELSE
               DISPLAY 'SWR110 - UNKNOWN PARAMETER IGNORED: '
                       PARM-CARD-REC (1:40)
           END-IF.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - COLLECT THE LINES OF ONE MESSAGE, THEN REPORT IT        *
      *================================================================*
       2000-PROCESS-MESSAGE.
           MOVE SWM-MSG-SEQ            TO WS-CUR-SEQ
           MOVE SWM-MSG-TYPE           TO WS-CUR-MT
           MOVE ZERO                   TO WS-CUR-LINES
           MOVE 'N'                    TO WS-STRUCT-SW
           MOVE 1                      TO WS-EXPECT-LINE
           PERFORM UNTIL WS-END-OF-LINES
                      OR SWM-MSG-SEQ NOT = WS-CUR-SEQ
               IF SWM-LINE-NO NOT = WS-EXPECT-LINE
               AND NOT WS-STRUCT-ERROR
                   MOVE 'LINE NUMBERS NOT CONSECUTIVE' TO WS-ERR-TEXT
                   PERFORM 6900-STRUCTURE-ERROR THRU 6900-EXIT
               END-IF
               ADD 1                   TO WS-EXPECT-LINE
               IF WS-CUR-LINES < 60
                   ADD 1               TO WS-CUR-LINES
                   MOVE SWM-TEXT
                            TO WS-CUR-LINE-TEXT (WS-CUR-LINES)
               ELSE
                   IF NOT WS-STRUCT-ERROR
                       MOVE 'MORE THAN 60 LINES' TO WS-ERR-TEXT
                       PERFORM 6900-STRUCTURE-ERROR THRU 6900-EXIT
                   END-IF
               END-IF
               PERFORM 8000-READ-LINE  THRU 8000-EXIT
           END-PERFORM
      *
           ADD 1                       TO WS-MSGS-READ
           PERFORM 2100-CHECK-SEQUENCE THRU 2100-EXIT
           PERFORM 3000-EXTRACT-FIELDS THRU 3000-EXIT
           PERFORM 4000-PRINT-MESSAGE  THRU 4000-EXIT
           PERFORM 5000-ACCUMULATE     THRU 5000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-CHECK-SEQUENCE.
           IF WS-MSGS-READ = 1
               MOVE WS-CUR-SEQ         TO WS-FIRST-SEQ
           ELSE
               IF WS-CUR-SEQ NOT = WS-EXPECT-SEQ
                   ADD 1               TO WS-SEQ-GAPS
                   MOVE 'MESSAGE SEQUENCE GAP OR REPEAT' TO WS-ERR-TEXT
                   PERFORM 6950-PRINT-ERROR THRU 6950-EXIT
               END-IF
           END-IF
           MOVE WS-CUR-SEQ             TO WS-LAST-SEQ
           IF WS-CUR-SEQ = 99999999
               MOVE 1                  TO WS-EXPECT-SEQ
           ELSE
               COMPUTE WS-EXPECT-SEQ = WS-CUR-SEQ + 1
           END-IF.
       2100-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - PICK THE REPORTED FIELDS OUT OF THE MESSAGE TEXT        *
      *================================================================*
       3000-EXTRACT-FIELDS.
           MOVE SPACES                 TO WS-F-FUNCTION WS-F-SEME
                                          WS-F-PREV WS-F-ISIN
                                          WS-F-SETTLE WS-F-QTY-TYPE
                                          WS-F-QTY-TEXT WS-F-CCY
                                          WS-F-AMT-TEXT WS-F-PSET
           MOVE ZERO                   TO WS-F-QTY WS-F-AMT
           MOVE 'N'                    TO WS-F-TRAILER-SW
           IF WS-CUR-LINE-TEXT (1) (1:3) NOT = '{1:'
               MOVE 'NO BLOCK HEADER ON LINE 1' TO WS-ERR-TEXT
               PERFORM 6900-STRUCTURE-ERROR THRU 6900-EXIT
           END-IF
           IF WS-CUR-LINE-TEXT (WS-CUR-LINES) (1:2) = '-}'
               SET WS-F-HAS-TRAILER    TO TRUE
           ELSE
               MOVE 'NO -} TRAILER'    TO WS-ERR-TEXT
               PERFORM 6900-STRUCTURE-ERROR THRU 6900-EXIT
           END-IF
           PERFORM VARYING WS-SUB FROM 2 BY 1
                     UNTIL WS-SUB > WS-CUR-LINES
               IF WS-CUR-LINE-TEXT (WS-SUB) (1:1) = ':'
                   MOVE WS-CUR-LINE-TEXT (WS-SUB) TO WS-TAG-LINE
                   PERFORM 3100-SPLIT-TAG THRU 3100-EXIT
                   PERFORM 3200-PICK-FIELD THRU 3200-EXIT
               END-IF
           END-PERFORM
      *
           IF WS-F-QTY-TEXT NOT = SPACES
               MOVE 'NUM '             TO FM-FUNCTION
               MOVE WS-F-QTY-TEXT      TO FM-TEXT
               CALL 'SWU010' USING FM-PARMS
               IF FM-RETURN-CODE > 04
                   ADD 1               TO WS-NUM-ERRORS
                   MOVE 'QUANTITY NOT A VALID NUMBER' TO WS-ERR-TEXT
                   PERFORM 6950-PRINT-ERROR THRU 6950-EXIT
               ELSE
                   MOVE FM-NUM-OUT     TO WS-F-QTY
               END-IF
           END-IF
           IF WS-F-AMT-TEXT NOT = SPACES
               MOVE 'NUM '             TO FM-FUNCTION
               MOVE WS-F-AMT-TEXT      TO FM-TEXT
               CALL 'SWU010' USING FM-PARMS
               IF FM-RETURN-CODE > 04
                   ADD 1               TO WS-NUM-ERRORS
                   MOVE 'AMOUNT NOT A VALID NUMBER' TO WS-ERR-TEXT
                   PERFORM 6950-PRINT-ERROR THRU 6950-EXIT
               ELSE
                   MOVE FM-NUM-OUT     TO WS-F-AMT
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.
      *
      *    ':20C::SEME//REF' -> TAG 20C, QUAL SEME, DATA REF
      *    ':23G:NEWM'       -> TAG 23G, DATA NEWM
       3100-SPLIT-TAG.
           MOVE SPACES                 TO WS-TAG-EMPTY WS-TAG
                                          WS-TAG-VALUE WS-TAG-QUAL
                                          WS-TAG-DATA
           UNSTRING WS-TAG-LINE DELIMITED BY ':'
               INTO WS-TAG-EMPTY WS-TAG WS-TAG-VALUE
           END-UNSTRING
           IF WS-TAG-VALUE = SPACES
      *        QUALIFIED FIELD - THE VALUE FOLLOWS THE SECOND COLON
               UNSTRING WS-TAG-LINE DELIMITED BY '::'
                   INTO WS-TAG-EMPTY WS-TAG-VALUE
               END-UNSTRING
               UNSTRING WS-TAG-VALUE DELIMITED BY '/'
                   INTO WS-TAG-QUAL
               END-UNSTRING
               IF WS-TAG-VALUE (5:2) = '//'
                   MOVE WS-TAG-VALUE (7:94) TO WS-TAG-DATA
               ELSE
                   MOVE WS-TAG-VALUE (6:95) TO WS-TAG-DATA
               END-IF
           ELSE
               MOVE WS-TAG-VALUE       TO WS-TAG-DATA
           END-IF.
       3100-EXIT.
           EXIT.
      *
       3200-PICK-FIELD.
           EVALUATE WS-TAG ALSO WS-TAG-QUAL
               WHEN '23G' ALSO ANY
                   MOVE WS-TAG-DATA (1:4) TO WS-F-FUNCTION
               WHEN '20C' ALSO 'SEME'
                   MOVE WS-TAG-DATA (1:16) TO WS-F-SEME
               WHEN '20C' ALSO 'PREV'
                   MOVE WS-TAG-DATA (1:16) TO WS-F-PREV
               WHEN '98A' ALSO 'SETT'
                   MOVE WS-TAG-DATA (1:8) TO WS-F-SETTLE
               WHEN '35B' ALSO ANY
                   IF WS-TAG-DATA (1:5) = 'ISIN '
                       MOVE WS-TAG-DATA (6:12) TO WS-F-ISIN
                   END-IF
               WHEN '36B' ALSO 'SETT'
                   MOVE SPACES         TO WS-TAG-SUB1 WS-TAG-SUB2
                   UNSTRING WS-TAG-DATA DELIMITED BY '/'
                       INTO WS-TAG-SUB1 WS-TAG-SUB2
                   END-UNSTRING
                   MOVE WS-TAG-SUB1 (1:4) TO WS-F-QTY-TYPE
                   MOVE WS-TAG-SUB2    TO WS-F-QTY-TEXT
               WHEN '19A' ALSO 'SETT'
                   MOVE WS-TAG-DATA (1:3) TO WS-F-CCY
                   MOVE WS-TAG-DATA (4:32) TO WS-F-AMT-TEXT
               WHEN '95P' ALSO 'PSET'
                   MOVE WS-TAG-DATA (1:11) TO WS-F-PSET
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       3200-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - JOURNAL LINE (+ FULL TEXT FOR CANC / DETAIL=ALL)        *
      *================================================================*
       4000-PRINT-MESSAGE.
           MOVE SPACES                 TO WS-DETAIL-LINE
           MOVE ' '                    TO DL-CC
           MOVE WS-CUR-SEQ             TO DL-SEQ
           MOVE WS-CUR-MT              TO DL-MT
           MOVE WS-F-FUNCTION          TO DL-FUNC
           MOVE WS-F-SEME              TO DL-SEME
           MOVE WS-F-PREV              TO DL-PREV
           MOVE WS-F-ISIN              TO DL-ISIN
           IF WS-F-SETTLE NUMERIC
               MOVE WS-F-SETTLE        TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE  THRU 8300-EXIT
               MOVE WS-DATE-EDIT       TO DL-SETTLE
           ELSE
               MOVE WS-F-SETTLE        TO DL-SETTLE
           END-IF
           MOVE WS-F-QTY-TYPE          TO DL-QTY-TYPE
           MOVE WS-F-QTY               TO DL-QTY
           MOVE WS-F-CCY               TO DL-CCY
           MOVE WS-F-AMT               TO DL-AMT
           MOVE WS-F-PSET              TO DL-PSET
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
      *
           IF WS-DETAIL-ALL
           OR (WS-DETAIL-CANC AND WS-F-FUNCTION = 'CANC')
               ADD 1                   TO WS-MSGS-PRINTED-FULL
               PERFORM VARYING WS-SUB FROM 1 BY 1
                         UNTIL WS-SUB > WS-CUR-LINES
                   MOVE SPACES         TO WS-TEXT-LINE
                   MOVE ' '            TO TX-CC
                   MOVE WS-SUB         TO TX-LINE-NO
                   MOVE WS-CUR-LINE-TEXT (WS-SUB) TO TX-TEXT
                   MOVE WS-TEXT-LINE   TO WS-DETAIL-LINE
                   PERFORM 8100-WRITE-LINE THRU 8100-EXIT
               END-PERFORM
               MOVE SPACES             TO WS-DETAIL-LINE
               MOVE ' '                TO DL-CC
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
           END-IF.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
      * 5000 - TOTALS                                                  *
      *================================================================*
       5000-ACCUMULATE.
           PERFORM VARYING WS-TT-SUB FROM 1 BY 1 UNTIL WS-TT-SUB > 4
               IF WS-TT-MT (WS-TT-SUB) = WS-CUR-MT
               AND WS-TT-FUNC (WS-TT-SUB) = WS-F-FUNCTION
                   ADD 1               TO WS-TT-COUNT (WS-TT-SUB)
               END-IF
           END-PERFORM
           IF WS-F-FUNCTION = 'CANC'
               GO TO 5000-EXIT
           END-IF
           SET WS-CT-IDX               TO 1
           SEARCH WS-CT-ENTRY
               AT END
                   DISPLAY 'SWR110 - CURRENCY TABLE FULL, NOT TOTALLED '
                           WS-F-CCY
               WHEN WS-CT-IDX > WS-CT-COUNT-USED
                   ADD 1               TO WS-CT-COUNT-USED
                   SET WS-CT-IDX       TO WS-CT-COUNT-USED
                   MOVE WS-F-CCY       TO WS-CT-CCY (WS-CT-IDX)
                   MOVE ZERO           TO WS-CT-MSGS (WS-CT-IDX)
                                          WS-CT-RCV-AMT (WS-CT-IDX)
                                          WS-CT-DLV-AMT (WS-CT-IDX)
                   PERFORM 5100-ADD-CCY THRU 5100-EXIT
               WHEN WS-CT-CCY (WS-CT-IDX) = WS-F-CCY
                   PERFORM 5100-ADD-CCY THRU 5100-EXIT
           END-SEARCH.
       5000-EXIT.
           EXIT.
      *
       5100-ADD-CCY.
           ADD 1                       TO WS-CT-MSGS (WS-CT-IDX)
           IF WS-CUR-MT = '541'
               ADD WS-F-AMT            TO WS-CT-RCV-AMT (WS-CT-IDX)
           ELSE
               ADD WS-F-AMT            TO WS-CT-DLV-AMT (WS-CT-IDX)
           END-IF
           ADD WS-F-AMT                TO WS-TOTAL-AMT.
       5100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 6900 - STRUCTURE ERRORS (ONE REPORTED PER MESSAGE)             *
      *----------------------------------------------------------------*
       6900-STRUCTURE-ERROR.
           IF NOT WS-STRUCT-ERROR
               ADD 1                   TO WS-STRUCT-ERRORS
               SET WS-STRUCT-ERROR     TO TRUE
           END-IF
           PERFORM 6950-PRINT-ERROR    THRU 6950-EXIT.
       6900-EXIT.
           EXIT.
      *
       6950-PRINT-ERROR.
           MOVE 4                      TO WS-RETURN-CODE
           MOVE SPACES                 TO WS-ERROR-LINE
           MOVE ' '                    TO EL-CC
           MOVE '*** ERROR: '          TO WS-ERROR-LINE (12:12)
           MOVE WS-ERR-TEXT            TO EL-TEXT
           MOVE ' MSG SEQ '            TO WS-ERROR-LINE (64:10)
           MOVE WS-CUR-SEQ             TO EL-SEQ
           MOVE WS-ERROR-LINE          TO WS-DETAIL-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT.
       6950-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - SUMMARY PAGE                                            *
      *================================================================*
       7000-PRINT-TOTALS.
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE '0'                    TO TL-CC
           MOVE 'MESSAGES IN JOURNAL'  TO TL-LABEL
           MOVE WS-MSGS-READ           TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'MESSAGE LINES'        TO TL-LABEL
           MOVE WS-LINES-READ          TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'FIRST MESSAGE SEQUENCE' TO TL-LABEL
           MOVE WS-FIRST-SEQ           TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'LAST MESSAGE SEQUENCE' TO TL-LABEL
           MOVE WS-LAST-SEQ            TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'SEQUENCE GAPS'        TO TL-LABEL
           MOVE WS-SEQ-GAPS            TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'MESSAGES WITH STRUCTURE ERRORS' TO TL-LABEL
           MOVE WS-STRUCT-ERRORS       TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'INVALID NUMBERS'      TO TL-LABEL
           MOVE WS-NUM-ERRORS          TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
           MOVE 'MESSAGES PRINTED IN FULL' TO TL-LABEL
           MOVE WS-MSGS-PRINTED-FULL   TO TL-VALUE
           PERFORM 7900-WRITE-TOTAL    THRU 7900-EXIT
      *
           PERFORM VARYING WS-TT-SUB FROM 1 BY 1 UNTIL WS-TT-SUB > 4
               MOVE SPACES             TO WS-TT-LINE
               IF WS-TT-SUB = 1
                   MOVE '0'            TO TT-CC
               ELSE
                   MOVE ' '            TO TT-CC
               END-IF
               MOVE 'MT'               TO WS-TT-LINE (7:2)
               MOVE WS-TT-MT (WS-TT-SUB)    TO TT-MT
               MOVE WS-TT-FUNC (WS-TT-SUB)  TO TT-FUNC
               MOVE WS-TT-COUNT (WS-TT-SUB) TO TT-COUNT
               WRITE RPT-RECORD FROM WS-TT-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-PERFORM
      *
           WRITE RPT-RECORD FROM WS-CT-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           PERFORM VARYING WS-CT-IDX FROM 1 BY 1
                     UNTIL WS-CT-IDX > WS-CT-COUNT-USED
               MOVE SPACES             TO WS-CT-LINE
               MOVE ' '                TO CT-CC
               MOVE WS-CT-CCY (WS-CT-IDX)     TO CT-CCY
               MOVE WS-CT-MSGS (WS-CT-IDX)    TO CT-MSGS
               MOVE WS-CT-RCV-AMT (WS-CT-IDX) TO CT-RCV
               MOVE WS-CT-DLV-AMT (WS-CT-IDX) TO CT-DLV
               WRITE RPT-RECORD FROM WS-CT-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-PERFORM
           WRITE RPT-RECORD FROM RPT-END-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT.
       7000-EXIT.
           EXIT.
      *
       7900-WRITE-TOTAL.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE ' '                    TO TL-CC.
       7900-EXIT.
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
                   SET WS-END-OF-LINES TO TRUE
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
       8100-WRITE-LINE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-DETAIL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *
       8200-HEADINGS.
           ADD 1                       TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT         TO RPT-H1-PAGE
           WRITE RPT-RECORD FROM RPT-HEADING-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM RPT-HEADING-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM WS-COL-HEAD-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM WS-COL-HEAD-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           MOVE 5                      TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *
       8300-EDIT-DATE.
           MOVE WS-DI-CCYY             TO WS-DE-CCYY
           MOVE WS-DI-MM               TO WS-DE-MM
           MOVE WS-DI-DD               TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *
       8900-CHECK-WRITE.
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE '8900-CHECK-WRITE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       8900-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE MSGIN-FILE RPTFILE
           IF NOT MSGIN-OK OR NOT RPTFILE-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CLOSE ERROR'      TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM CT-STAGE
           MOVE 'MSG-IN'               TO CT-COUNTER-NAME
           MOVE WS-MSGS-READ           TO CT-COUNT
           MOVE WS-TOTAL-AMT           TO CT-AMOUNT
           MOVE ZERO                   TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'LINES-IN'             TO CT-COUNTER-NAME
           MOVE WS-LINES-READ          TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
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
           STRING 'MESSAGE JOURNAL ENDED. MESSAGES ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           MOVE WS-MSGS-READ           TO WS-DISP-COUNT
           DISPLAY 'SWR110 MESSAGES READ        : ' WS-DISP-COUNT
           MOVE WS-LINES-READ          TO WS-DISP-COUNT
           DISPLAY 'SWR110 LINES READ           : ' WS-DISP-COUNT
           MOVE WS-STRUCT-ERRORS       TO WS-DISP-COUNT
           DISPLAY 'SWR110 STRUCTURE ERRORS     : ' WS-DISP-COUNT
           MOVE WS-SEQ-GAPS            TO WS-DISP-COUNT
           DISPLAY 'SWR110 SEQUENCE GAPS        : ' WS-DISP-COUNT
           DISPLAY 'SWR110 PAGES                : ' RPT-PAGE-COUNT
           DISPLAY 'SWR110 RETURN CODE          : ' WS-RETURN-CODE.
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
           DISPLAY 'SWR110 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWR110 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
