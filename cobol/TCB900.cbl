       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCB900.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/08/2002.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCB900                                            *
      * TITLE      : TRADE CAPTURE CONTROL TOTAL RECONCILIATION        *
      * JOB        : MSTCD070   STEP040                                *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   READS THE CONTROL TOTALS POSTED TODAY BY EVERY TRADE CAPTURE *
      *   STEP (THROUGH CMU080 'GET ') AND PROVES THAT NO TRADE WAS    *
      *   LOST OR CREATED BETWEEN STAGES:                              *
      *     R1-R3  EACH VALIDATOR   RECORDS-IN = VALID-OUT + REJECT-OUT*
      *     R4     SUM OF VALIDATOR VALID-OUT  = TCB200 TRADES-IN      *
      *     R5     TCB200 TRADES-IN = TRADES-OUT + REJECT-OUT          *
      *     R6     TCB200 TRADES-OUT = TCB300 TRADES-IN                *
      *     R7     TCB300 TRADES-IN + CORR-REVERSAL                    *
      *                             = TRADES-OUT + REJECT-OUT          *
      *     R8     TCB300 TRADES-OUT = TCB500 TRADES-IN                *
      *     R9     TCB500 SRACTV-OUT = 2 X TCB500 TRADES-IN            *
      *   QUANTITY HASH AND AMOUNT ARE PROVED WHERE THE SAME RECORDS   *
      *   ARE COUNTED ON BOTH SIDES (R4 QTY, R6 AND R8 QTY + AMOUNT).  *
      *   ANY BREAK OR MISSING COUNTER SETS RETURN CODE 4.  THE JOB    *
      *   CONTINUES - OPERATIONS REVIEW THE REPORT (RUNBOOK TC-07).    *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          CTLTOTS   MSEC.PROD.CM.CTLTOTS(0) VIA CMU080          *
      * OUTPUT : RPTFILE   RECONCILIATION REPORT FBA 133 (CMRPTHD)     *
      * CALLS  : CMU050 CMU060 CMU080                                  *
      *                                                                *
      * RETURN CODES: 0 IN BALANCE  4 BREAKS / MISSING COUNTERS        *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 2002-03-08 KAP  CHG09930  ORIGINAL                             *
      * 2006-06-12 KAP  CHG15008  DUPLICATE REJECTS IN TCB300 COUNTS   *
      * 2012-10-01 SPA  CHG23466  CORRECTION REVERSALS (R7)            *
      * 2014-11-17 SPA  CHG27740  SRACTV LEG CHECK (R9)                *
      * 2019-03-11 NVR  CHG35510  RC 4 INSTEAD OF ABEND 1004 ON BREAK  *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-Z15.
       OBJECT-COMPUTER.  IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT RPTFILE       ASSIGN TO RPTFILE
                                FILE STATUS IS WS-RPTFILE-FS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-LINE                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCB900'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-RPTFILE-FS           PIC X(02).
               88  RPTFILE-OK                    VALUE '00'.
      *
      *----------------------------------------------------------------*
      * COUNTERS TO FETCH - STAGE(8) COUNTER(16)                       *
      *----------------------------------------------------------------*
       01  WS-COUNTER-LIST-VALUES.
           05  FILLER PIC X(24) VALUE 'TCB100  RECORDS-IN      '.
           05  FILLER PIC X(24) VALUE 'TCB100  VALID-OUT       '.
           05  FILLER PIC X(24) VALUE 'TCB100  REJECT-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB110  RECORDS-IN      '.
           05  FILLER PIC X(24) VALUE 'TCB110  VALID-OUT       '.
           05  FILLER PIC X(24) VALUE 'TCB110  REJECT-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB120  RECORDS-IN      '.
           05  FILLER PIC X(24) VALUE 'TCB120  VALID-OUT       '.
           05  FILLER PIC X(24) VALUE 'TCB120  REJECT-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB200  TRADES-IN       '.
           05  FILLER PIC X(24) VALUE 'TCB200  TRADES-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB200  REJECT-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB300  TRADES-IN       '.
           05  FILLER PIC X(24) VALUE 'TCB300  TRADES-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB300  REJECT-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB300  CORR-REVERSAL   '.
           05  FILLER PIC X(24) VALUE 'TCB500  TRADES-IN       '.
           05  FILLER PIC X(24) VALUE 'TCB500  SRACTV-OUT      '.
           05  FILLER PIC X(24) VALUE 'TCB500  SETLINST-OUT    '.
       01  WS-COUNTER-LIST REDEFINES WS-COUNTER-LIST-VALUES.
           05  WS-CL-ENTRY             OCCURS 19 TIMES.
               10  WS-CL-STAGE         PIC X(08).
               10  WS-CL-COUNTER       PIC X(16).
       01  WS-CL-MAX                   PIC S9(04) COMP VALUE +19.
      *
      *    VALUES RETURNED - SAME SUBSCRIPT AS THE LIST
       01  WS-COUNTER-VALUES.
           05  WS-CV-ENTRY             OCCURS 19 TIMES.
               10  WS-CV-FOUND         PIC X(01).
               10  WS-CV-COUNT         PIC S9(09)       COMP-3.
               10  WS-CV-AMOUNT        PIC S9(15)V99    COMP-3.
               10  WS-CV-QTY           PIC S9(15)V9(04) COMP-3.
      *
      *    SYMBOLIC SUBSCRIPTS
       01  WS-SUBSCRIPTS.
           05  C-TCB100-IN             PIC S9(04) COMP VALUE +1.
           05  C-TCB100-VALID          PIC S9(04) COMP VALUE +2.
           05  C-TCB100-REJ            PIC S9(04) COMP VALUE +3.
           05  C-TCB110-IN             PIC S9(04) COMP VALUE +4.
           05  C-TCB110-VALID          PIC S9(04) COMP VALUE +5.
           05  C-TCB110-REJ            PIC S9(04) COMP VALUE +6.
           05  C-TCB120-IN             PIC S9(04) COMP VALUE +7.
           05  C-TCB120-VALID          PIC S9(04) COMP VALUE +8.
           05  C-TCB120-REJ            PIC S9(04) COMP VALUE +9.
           05  C-TCB200-IN             PIC S9(04) COMP VALUE +10.
           05  C-TCB200-OUT            PIC S9(04) COMP VALUE +11.
           05  C-TCB200-REJ            PIC S9(04) COMP VALUE +12.
           05  C-TCB300-IN             PIC S9(04) COMP VALUE +13.
           05  C-TCB300-OUT            PIC S9(04) COMP VALUE +14.
           05  C-TCB300-REJ            PIC S9(04) COMP VALUE +15.
           05  C-TCB300-REV            PIC S9(04) COMP VALUE +16.
           05  C-TCB500-IN             PIC S9(04) COMP VALUE +17.
           05  C-TCB500-LEGS           PIC S9(04) COMP VALUE +18.
           05  C-TCB500-INSTR          PIC S9(04) COMP VALUE +19.
       01  WS-SUB                      PIC S9(04) COMP.
       01  WS-SUB-LEFT                 PIC S9(04) COMP.
       01  WS-SUB-RIGHT                PIC S9(04) COMP.
      *
       01  WS-CHECK-WORK.
           05  WS-CHK-ID               PIC X(03).
           05  WS-CHK-DESC             PIC X(50).
           05  WS-CHK-MEASURE          PIC X(05).
           05  WS-CHK-LEFT             PIC S9(15)V9(04) COMP-3.
           05  WS-CHK-RIGHT            PIC S9(15)V9(04) COMP-3.
           05  WS-CHK-DIFF             PIC S9(15)V9(04) COMP-3.
           05  WS-CHK-MISSING-SW       PIC X(01).
               88  WS-CHK-HAS-MISSING            VALUE 'Y'.
       01  WS-TOTALS.
           05  WS-CHECKS-RUN           PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-CHECKS-OK            PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-CHECKS-BROKEN        PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-CHECKS-INCOMPLETE    PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-COUNTERS-MISSING     PIC S9(05) COMP-3 VALUE ZERO.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
       01  WS-EDIT-DATE.
           05  WS-ED-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  WS-ED-DD                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  WS-ED-CCYY              PIC 9(04).
      *
       COPY CMDATEW.
       COPY CMRPTHD.
      *
       01  RPT-SECTION-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  RPT-SEC-TEXT            PIC X(60).
           05  FILLER                  PIC X(72)  VALUE SPACES.
       01  RPT-CTR-HDR.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  FILLER                  PIC X(10)  VALUE 'STAGE'.
           05  FILLER                  PIC X(18)  VALUE 'COUNTER'.
           05  FILLER                  PIC X(15)  VALUE
               '          COUNT'.
           05  FILLER                  PIC X(24)  VALUE
               '                  AMOUNT'.
           05  FILLER                  PIC X(26)  VALUE
               '                  QTY HASH'.
           05  FILLER                  PIC X(10)  VALUE '   STATUS'.
           05  FILLER                  PIC X(25)  VALUE SPACES.
       01  RPT-CTR-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-CT-STAGE            PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-CT-COUNTER          PIC X(16).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-CT-COUNT            PIC ZZZ,ZZZ,ZZ9-.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  RPT-CT-AMOUNT           PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-CT-QTY              PIC ZZZ,ZZZ,ZZZ,ZZ9.9999-.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  RPT-CT-STATUS           PIC X(07).
           05  FILLER                  PIC X(29)  VALUE SPACES.
       01  RPT-CHK-HDR.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  FILLER                  PIC X(04)  VALUE 'ID'.
           05  FILLER                  PIC X(46)  VALUE 'CHECK'.
           05  FILLER                  PIC X(06)  VALUE 'ON'.
           05  FILLER                  PIC X(20)  VALUE
               '         LEFT SIDE'.
           05  FILLER                  PIC X(20)  VALUE
               '        RIGHT SIDE'.
           05  FILLER                  PIC X(20)  VALUE
               '        DIFFERENCE'.
           05  FILLER                  PIC X(12)  VALUE 'RESULT'.
       01  RPT-CHK-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-CK-ID               PIC X(03).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-CK-DESC             PIC X(45).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-CK-MEASURE          PIC X(05).
           05  RPT-CK-LEFT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-CK-RIGHT            PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-CK-DIFF             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-CK-RESULT           PIC X(12).
       01  RPT-SUMMARY-LINE.
           05  RPT-SM-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-SM-LABEL            PIC X(40).
           05  RPT-SM-VALUE            PIC ZZ,ZZ9.
           05  FILLER                  PIC X(82)  VALUE SPACES.
       01  RPT-VERDICT-LINE.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-VD-TEXT             PIC X(80).
           05  FILLER                  PIC X(48)  VALUE SPACES.
      *
       COPY CMCTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-FETCH-COUNTERS
           PERFORM 3000-PRINT-COUNTERS
           PERFORM 4000-RUN-CHECKS
           PERFORM 6000-PRINT-SUMMARY
           PERFORM 9000-TERMINATE
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED FOR DATECARD' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'TC CONTROL TOTAL RECONCILIATION STARTED'
                                       TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN OUTPUT RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED FOR RPTFILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
           MOVE 'TCB900'               TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'TRADE CAPTURE CONTROL TOTAL RECONCILIATION'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-MM              TO WS-ED-MM
           MOVE DC-BUS-DD              TO WS-ED-DD
           MOVE DC-BUS-CCYY            TO WS-ED-CCYY
           MOVE WS-EDIT-DATE           TO RPT-H2-BUS-DATE
           MOVE DC-CAL-DATE(5:2)       TO WS-ED-MM
           MOVE DC-CAL-DATE(7:2)       TO WS-ED-DD
           MOVE DC-CAL-DATE(1:4)       TO WS-ED-CCYY
           MOVE WS-EDIT-DATE           TO RPT-H1-RUN-DATE
           MOVE 99                     TO RPT-LINE-COUNT
           PERFORM 7000-CHECK-PAGE.
      *
      *================================================================*
      * 2000 - FETCH EVERY COUNTER FROM CTLTOTS THROUGH CMU080         *
      *================================================================*
       2000-FETCH-COUNTERS.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > WS-CL-MAX
               INITIALIZE CT-CONTROL-PARMS
               MOVE 'GET '             TO CT-FUNCTION
               MOVE DC-BUS-DATE        TO CT-BUS-DATE
               MOVE WS-PROGRAM-ID      TO CT-PROGRAM
               MOVE WS-CL-STAGE (WS-SUB)   TO CT-STAGE
               MOVE WS-CL-COUNTER (WS-SUB) TO CT-COUNTER-NAME
               CALL 'CMU080' USING CT-CONTROL-PARMS
               EVALUATE TRUE
                   WHEN CT-OK
                       MOVE 'Y'        TO WS-CV-FOUND (WS-SUB)
                       MOVE CT-COUNT   TO WS-CV-COUNT (WS-SUB)
                       MOVE CT-AMOUNT  TO WS-CV-AMOUNT (WS-SUB)
                       MOVE CT-QTY-HASH TO WS-CV-QTY (WS-SUB)
                   WHEN CT-NOT-FOUND
                       MOVE 'N'        TO WS-CV-FOUND (WS-SUB)
                       MOVE ZERO       TO WS-CV-COUNT (WS-SUB)
                                          WS-CV-AMOUNT (WS-SUB)
                                          WS-CV-QTY (WS-SUB)
                       ADD 1           TO WS-COUNTERS-MISSING
                       DISPLAY 'TCB900 - COUNTER NOT POSTED: '
                               WS-CL-STAGE (WS-SUB) ' '
                               WS-CL-COUNTER (WS-SUB)
                   WHEN OTHER
                       MOVE 'CTLTOTS'  TO AB-DDNAME
                       MOVE 1010       TO AB-ABEND-CODE
                       MOVE '2000-FETCH-COUNTERS' TO AB-PARAGRAPH
                       MOVE CT-COUNTER-NAME TO AB-KEY
                       MOVE 'CMU080 GET FAILED' TO AB-MESSAGE
                       PERFORM 9999-ABEND
               END-EVALUATE
           END-PERFORM.
      *
      *================================================================*
      * 3000 - LIST THE COUNTERS                                       *
      *================================================================*
       3000-PRINT-COUNTERS.
           MOVE 'CONTROL TOTALS POSTED TODAY (CTLTOTS)' TO RPT-SEC-TEXT
           MOVE RPT-SECTION-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE RPT-CTR-HDR            TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           ADD 3                       TO RPT-LINE-COUNT
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > WS-CL-MAX
               PERFORM 7000-CHECK-PAGE
               MOVE WS-CL-STAGE (WS-SUB)   TO RPT-CT-STAGE
               MOVE WS-CL-COUNTER (WS-SUB) TO RPT-CT-COUNTER
               MOVE WS-CV-COUNT (WS-SUB)   TO RPT-CT-COUNT
               MOVE WS-CV-AMOUNT (WS-SUB)  TO RPT-CT-AMOUNT
               MOVE WS-CV-QTY (WS-SUB)     TO RPT-CT-QTY
               IF WS-CV-FOUND (WS-SUB) = 'Y'
                   MOVE 'POSTED'       TO RPT-CT-STATUS
               ELSE
                   MOVE 'MISSING'      TO RPT-CT-STATUS
               END-IF
               MOVE RPT-CTR-LINE       TO RPT-LINE
               PERFORM 7100-WRITE-LINE
               ADD 1                   TO RPT-LINE-COUNT
           END-PERFORM.
      *
      *================================================================*
      * 4000 - RECONCILIATION RULES                                    *
      *================================================================*
       4000-RUN-CHECKS.
           PERFORM 7000-CHECK-PAGE
           MOVE 'STAGE TO STAGE PROOF' TO RPT-SEC-TEXT
           MOVE RPT-SECTION-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE RPT-CHK-HDR            TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           ADD 3                       TO RPT-LINE-COUNT
      *
      *    R1-R3  VALIDATOR IN = VALID + REJECT
           MOVE 'R1 '                  TO WS-CHK-ID
           MOVE 'TCB100 OMS DETAILS = VALID + REJECTED'
                                       TO WS-CHK-DESC
           PERFORM 4100-VALIDATOR-CHECK-100
           MOVE 'R2 '                  TO WS-CHK-ID
           MOVE 'TCB110 BOND TICKETS = VALID + REJECTED'
                                       TO WS-CHK-DESC
           PERFORM 4110-VALIDATOR-CHECK-110
           MOVE 'R3 '                  TO WS-CHK-ID
           MOVE 'TCB120 MANUAL CARDS = VALID + REJECTED'
                                       TO WS-CHK-DESC
           PERFORM 4120-VALIDATOR-CHECK-120
      *
      *    R4  VALID OUT (ALL FEEDS) = ENRICHMENT IN
           MOVE 'R4 '                  TO WS-CHK-ID
           MOVE 'VALIDATORS VALID-OUT = TCB200 TRADES-IN'
                                       TO WS-CHK-DESC
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           COMPUTE WS-CHK-LEFT = WS-CV-COUNT (C-TCB100-VALID)
                               + WS-CV-COUNT (C-TCB110-VALID)
                               + WS-CV-COUNT (C-TCB120-VALID)
           MOVE WS-CV-COUNT (C-TCB200-IN) TO WS-CHK-RIGHT
           MOVE C-TCB100-VALID         TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB110-VALID         TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB120-VALID         TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB200-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE
           MOVE 'QTY  '                TO WS-CHK-MEASURE
           COMPUTE WS-CHK-LEFT = WS-CV-QTY (C-TCB100-VALID)
                               + WS-CV-QTY (C-TCB110-VALID)
                               + WS-CV-QTY (C-TCB120-VALID)
           MOVE WS-CV-QTY (C-TCB200-IN) TO WS-CHK-RIGHT
           PERFORM 5000-COMPARE
      *
      *    R5  ENRICHMENT IN = OUT + REJECT
           MOVE 'R5 '                  TO WS-CHK-ID
           MOVE 'TCB200 TRADES-IN = TRADES-OUT + REJECT-OUT'
                                       TO WS-CHK-DESC
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           MOVE WS-CV-COUNT (C-TCB200-IN) TO WS-CHK-LEFT
           COMPUTE WS-CHK-RIGHT = WS-CV-COUNT (C-TCB200-OUT)
                                + WS-CV-COUNT (C-TCB200-REJ)
           MOVE C-TCB200-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB200-OUT           TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE
      *
      *    R6  ENRICHMENT OUT = CANCEL/CORRECT IN
           MOVE 'R6 '                  TO WS-CHK-ID
           MOVE 'TCB200 TRADES-OUT = TCB300 TRADES-IN'
                                       TO WS-CHK-DESC
           MOVE C-TCB200-OUT           TO WS-SUB
           MOVE C-TCB300-IN            TO WS-SUB-RIGHT
           PERFORM 4500-STAGE-HANDOFF
      *
      *    R7  CANCEL/CORRECT IN + REVERSALS = OUT + REJECT
           MOVE 'R7 '                  TO WS-CHK-ID
           MOVE 'TCB300 IN + REVERSALS = OUT + REJECT-OUT'
                                       TO WS-CHK-DESC
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           COMPUTE WS-CHK-LEFT = WS-CV-COUNT (C-TCB300-IN)
                               + WS-CV-COUNT (C-TCB300-REV)
           COMPUTE WS-CHK-RIGHT = WS-CV-COUNT (C-TCB300-OUT)
                                + WS-CV-COUNT (C-TCB300-REJ)
           MOVE C-TCB300-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB300-OUT           TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE
      *
      *    R8  CANCEL/CORRECT OUT = EXTRACT IN
           MOVE 'R8 '                  TO WS-CHK-ID
           MOVE 'TCB300 TRADES-OUT = TCB500 TRADES-IN'
                                       TO WS-CHK-DESC
           MOVE C-TCB300-OUT           TO WS-SUB
           MOVE C-TCB500-IN            TO WS-SUB-RIGHT
           PERFORM 4500-STAGE-HANDOFF
      *
      *    R9  TWO LEGS PER TRADE
           MOVE 'R9 '                  TO WS-CHK-ID
           MOVE 'TCB500 SRACTV LEGS = 2 X TRADES-IN'
                                       TO WS-CHK-DESC
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           MOVE WS-CV-COUNT (C-TCB500-LEGS) TO WS-CHK-LEFT
           COMPUTE WS-CHK-RIGHT = WS-CV-COUNT (C-TCB500-IN) * 2
           MOVE C-TCB500-LEGS          TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB500-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE.
      *
       4100-VALIDATOR-CHECK-100.
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           MOVE WS-CV-COUNT (C-TCB100-IN) TO WS-CHK-LEFT
           COMPUTE WS-CHK-RIGHT = WS-CV-COUNT (C-TCB100-VALID)
                                + WS-CV-COUNT (C-TCB100-REJ)
           MOVE C-TCB100-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB100-VALID         TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE.
      *
       4110-VALIDATOR-CHECK-110.
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           MOVE WS-CV-COUNT (C-TCB110-IN) TO WS-CHK-LEFT
           COMPUTE WS-CHK-RIGHT = WS-CV-COUNT (C-TCB110-VALID)
                                + WS-CV-COUNT (C-TCB110-REJ)
           MOVE C-TCB110-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB110-VALID         TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE.
      *
       4120-VALIDATOR-CHECK-120.
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           MOVE WS-CV-COUNT (C-TCB120-IN) TO WS-CHK-LEFT
           COMPUTE WS-CHK-RIGHT = WS-CV-COUNT (C-TCB120-VALID)
                                + WS-CV-COUNT (C-TCB120-REJ)
           MOVE C-TCB120-IN            TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE C-TCB120-VALID         TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           PERFORM 5000-COMPARE.
      *
      *----------------------------------------------------------------*
      * 4500 - SAME RECORDS ON BOTH SIDES: COUNT, AMOUNT, QTY HASH     *
      *        WS-SUB = SENDING COUNTER, WS-SUB-RIGHT = RECEIVING      *
      *----------------------------------------------------------------*
       4500-STAGE-HANDOFF.
           MOVE 'N'                    TO WS-CHK-MISSING-SW
           PERFORM 4900-NOTE-MISSING
           MOVE WS-SUB                 TO WS-SUB-LEFT
           MOVE WS-SUB-RIGHT           TO WS-SUB
           PERFORM 4900-NOTE-MISSING
           MOVE 'COUNT'                TO WS-CHK-MEASURE
           MOVE WS-CV-COUNT (WS-SUB-LEFT)  TO WS-CHK-LEFT
           MOVE WS-CV-COUNT (WS-SUB-RIGHT) TO WS-CHK-RIGHT
           PERFORM 5000-COMPARE
           MOVE 'AMT  '                TO WS-CHK-MEASURE
           MOVE WS-CV-AMOUNT (WS-SUB-LEFT)  TO WS-CHK-LEFT
           MOVE WS-CV-AMOUNT (WS-SUB-RIGHT) TO WS-CHK-RIGHT
           PERFORM 5000-COMPARE
           MOVE 'QTY  '                TO WS-CHK-MEASURE
           MOVE WS-CV-QTY (WS-SUB-LEFT)  TO WS-CHK-LEFT
           MOVE WS-CV-QTY (WS-SUB-RIGHT) TO WS-CHK-RIGHT
           PERFORM 5000-COMPARE.
      *
       4900-NOTE-MISSING.
           IF WS-CV-FOUND (WS-SUB) NOT = 'Y'
               SET WS-CHK-HAS-MISSING  TO TRUE
           END-IF.
      *
      *================================================================*
      * 5000 - COMPARE AND PRINT ONE CHECK LINE                        *
      *================================================================*
       5000-COMPARE.
           ADD 1                       TO WS-CHECKS-RUN
           COMPUTE WS-CHK-DIFF = WS-CHK-LEFT - WS-CHK-RIGHT
           PERFORM 7000-CHECK-PAGE
           MOVE WS-CHK-ID              TO RPT-CK-ID
           MOVE WS-CHK-DESC            TO RPT-CK-DESC
           MOVE WS-CHK-MEASURE         TO RPT-CK-MEASURE
           MOVE WS-CHK-LEFT            TO RPT-CK-LEFT
           MOVE WS-CHK-RIGHT           TO RPT-CK-RIGHT
           MOVE WS-CHK-DIFF            TO RPT-CK-DIFF
           EVALUATE TRUE
               WHEN WS-CHK-HAS-MISSING
                   MOVE 'INCOMPLETE'   TO RPT-CK-RESULT
                   ADD 1               TO WS-CHECKS-INCOMPLETE
               WHEN WS-CHK-DIFF = ZERO
                   MOVE 'OK'           TO RPT-CK-RESULT
                   ADD 1               TO WS-CHECKS-OK
               WHEN OTHER
                   MOVE '** BREAK **'  TO RPT-CK-RESULT
                   ADD 1               TO WS-CHECKS-BROKEN
                   DISPLAY 'TCB900 - BREAK ' WS-CHK-ID ' '
                           WS-CHK-MEASURE ' ' WS-CHK-DESC
           END-EVALUATE
           MOVE RPT-CHK-LINE           TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           ADD 1                       TO RPT-LINE-COUNT.
      *
      *================================================================*
      * 6000 - SUMMARY AND VERDICT                                     *
      *================================================================*
       6000-PRINT-SUMMARY.
           IF RPT-LINE-COUNT + 10 > RPT-LINES-PER-PAGE
               MOVE 99                 TO RPT-LINE-COUNT
               PERFORM 7000-CHECK-PAGE
           END-IF
           MOVE '0'                    TO RPT-SM-CC
           MOVE 'CHECKS PERFORMED'     TO RPT-SM-LABEL
           MOVE WS-CHECKS-RUN          TO RPT-SM-VALUE
           MOVE RPT-SUMMARY-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE ' '                    TO RPT-SM-CC
           MOVE 'CHECKS IN BALANCE'    TO RPT-SM-LABEL
           MOVE WS-CHECKS-OK           TO RPT-SM-VALUE
           MOVE RPT-SUMMARY-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE 'CHECKS OUT OF BALANCE' TO RPT-SM-LABEL
           MOVE WS-CHECKS-BROKEN       TO RPT-SM-VALUE
           MOVE RPT-SUMMARY-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE 'CHECKS INCOMPLETE (COUNTER MISSING)' TO RPT-SM-LABEL
           MOVE WS-CHECKS-INCOMPLETE   TO RPT-SM-VALUE
           MOVE RPT-SUMMARY-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE 'COUNTERS NOT POSTED'  TO RPT-SM-LABEL
           MOVE WS-COUNTERS-MISSING    TO RPT-SM-VALUE
           MOVE RPT-SUMMARY-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
      *
           IF WS-CHECKS-BROKEN = ZERO
           AND WS-CHECKS-INCOMPLETE = ZERO
               MOVE '*** TRADE CAPTURE IS IN BALANCE ***'
                                       TO RPT-VD-TEXT
               MOVE ZERO               TO WS-RETURN-CODE
           ELSE
               MOVE SPACES             TO RPT-VD-TEXT
               STRING '*** TRADE CAPTURE OUT OF BALANCE - SEE '
                      'RUNBOOK TC-07 BEFORE RELEASING SR ***'
                      DELIMITED BY SIZE INTO RPT-VD-TEXT
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
           MOVE RPT-VERDICT-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE
           MOVE RPT-END-LINE           TO RPT-LINE
           PERFORM 7100-WRITE-LINE.
      *
      *================================================================*
      * 7000 - PAGE CONTROL                                            *
      *================================================================*
       7000-CHECK-PAGE.
           IF RPT-LINE-COUNT >= RPT-LINES-PER-PAGE
               ADD 1                   TO RPT-PAGE-COUNT
               MOVE RPT-PAGE-COUNT     TO RPT-H1-PAGE
               MOVE RPT-HEADING-1      TO RPT-LINE
               PERFORM 7100-WRITE-LINE
               MOVE RPT-HEADING-2      TO RPT-LINE
               PERFORM 7100-WRITE-LINE
               MOVE RPT-BLANK-LINE     TO RPT-LINE
               PERFORM 7100-WRITE-LINE
               MOVE 4                  TO RPT-LINE-COUNT
           END-IF.
      *
       7100-WRITE-LINE.
           WRITE RPT-LINE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '7100-WRITE-LINE'  TO AB-PARAGRAPH
               MOVE 'WRITE FAILED FOR RPTFILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED FOR RPTFILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
           INITIALIZE CT-CONTROL-PARMS
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'RECON-BREAKS'         TO CT-COUNTER-NAME
           COMPUTE CT-COUNT = WS-CHECKS-BROKEN + WS-CHECKS-INCOMPLETE
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           MOVE SPACES                 TO AU-MESSAGE
           IF WS-RETURN-CODE = ZERO
               MOVE 'I'                TO AU-SEVERITY
               MOVE 'TC CONTROL TOTALS IN BALANCE' TO AU-MESSAGE
           ELSE
               MOVE 'W'                TO AU-SEVERITY
               MOVE 'TC CONTROL TOTALS OUT OF BALANCE - SEE TCB900'
                                       TO AU-MESSAGE
           END-IF
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY 'TCB900 - CONTROL TOTAL RECONCILIATION ' DC-BUS-DATE
           DISPLAY 'TCB900 - CHECKS RUN        ' WS-CHECKS-RUN
           DISPLAY 'TCB900 - CHECKS OK         ' WS-CHECKS-OK
           DISPLAY 'TCB900 - BREAKS            ' WS-CHECKS-BROKEN
           DISPLAY 'TCB900 - INCOMPLETE        ' WS-CHECKS-INCOMPLETE
           DISPLAY 'TCB900 - COUNTERS MISSING  ' WS-COUNTERS-MISSING
           DISPLAY 'TCB900 - RETURN CODE       ' WS-RETURN-CODE.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'TCB900 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'TCB900 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
