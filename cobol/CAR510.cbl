      *================================================================*
      * PROGRAM    : CAR510                                            *
      * DESCRIPTION: CORPORATE ACTION DIVIDEND ACCRUAL REPORT.         *
      *              SECTION 1 - ACCRUALS BY EVENT WITH DAYS TO PAY    *
      *                          AND AGING BUCKET, BUCKET TOTALS       *
      *              SECTION 2 - GL ACCOUNT SUMMARY OF THE ACCRUAL     *
      *                          JOURNAL, DEBITS = CREDITS CHECK AND   *
      *                          PROOF AGAINST THE EVENT SUMMARY       *
      *----------------------------------------------------------------*
      * JOB        : MSCAD050  STEP020                                 *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              ACCSUMIN  ACCRUAL SUMMARY (CAACRSM)               *
      *                        MSEC.PROD.CA.ACCRSUM(+1)                *
      *              ACCRGL    ACCRUAL GL JOURNAL (SRGLJNL)            *
      *                        MSEC.PROD.CA.ACCRUAL(+1)                *
      * OUTPUT     : RPTFILE   REPORT, FB 133 WITH ASA CONTROL         *
      * CALLS      : CMU010 (DAYS TO PAY)  CMU050 CMU060 CMU080        *
      *              CMASM02                                           *
      * RETURN CODE: 00 CLEAN   04 JOURNAL OUT OF BALANCE OR PROOF     *
      *              DIFFERENCE                                        *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2003-03-17 KAP  ORIGINAL                              CHG10877 *
      * 2006-01-16 KAP  AUTO-REVERSING NOTE ON REPORT         CHG14720 *
      * 2011-06-20 SPA  STANDARD HEADINGS CMRPTHD, CMU060/080 CHG21877 *
      * 2011-09-12 SPA  USD EQUIVALENT COLUMN                 CHG22410 *
      * 2016-11-07 MFO  AGING BUCKETS FOR FINANCE             CHG30390 *
      * 2021-03-01 MFO  RECOMPILED ENTERPRISE COBOL 6.3       CHG36620 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAR510.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  2003-03-17.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE   ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT ACCSUMIN-FILE   ASSIGN TO ACCSUMIN
                  FILE STATUS IS WS-ACCSUMIN-STATUS.
           SELECT ACCRGL-FILE     ASSIGN TO ACCRGL
                  FILE STATUS IS WS-ACCRGL-STATUS.
           SELECT REPORT-FILE     ASSIGN TO RPTFILE
                  FILE STATUS IS WS-REPORT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  ACCSUMIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CAACRSM.
      *
       FD  ACCRGL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY SRGLJNL.
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAR510'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-ACCSUMIN-STATUS      PIC X(02) VALUE '00'.
               88  ACCSUMIN-OK                   VALUE '00'.
               88  ACCSUMIN-EOF                  VALUE '10'.
           05  WS-ACCRGL-STATUS        PIC X(02) VALUE '00'.
               88  ACCRGL-OK                     VALUE '00'.
               88  ACCRGL-EOF                    VALUE '10'.
           05  WS-REPORT-STATUS        PIC X(02) VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-SUM-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-SUM-EOF                    VALUE 'Y'.
           05  WS-GL-EOF-SW            PIC X(01) VALUE 'N'.
               88  WS-GL-EOF                     VALUE 'Y'.
           05  WS-SECTION              PIC X(01) VALUE '1'.
               88  WS-SECTION-EVENTS             VALUE '1'.
               88  WS-SECTION-GL                 VALUE '2'.
               88  WS-SECTION-END                VALUE '3'.
      *
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
       01  WS-HOLD-LINE                PIC X(133).
       01  WS-DAYS-TO-PAY              PIC S9(07) COMP-3.
       01  WS-BKT                      PIC S9(04) COMP.
      *
      *----------------------------------------------------------------*
      * AGING BUCKETS                                                  *
      *----------------------------------------------------------------*
       01  WS-BUCKET-TABLE.
           05  WS-BUCKET               OCCURS 3 TIMES.
               10  WS-BK-LABEL         PIC X(07).
               10  WS-BK-HIGH-DAYS     PIC S9(05) COMP-3.
               10  WS-BK-EVENTS        PIC S9(07) COMP-3.
               10  WS-BK-USD           PIC S9(15)V99 COMP-3.
      *
      *----------------------------------------------------------------*
      * GL ACCOUNT TABLE                                               *
      *----------------------------------------------------------------*
       01  WS-GL-MAX                   PIC S9(04) COMP VALUE +50.
       01  WS-GL-COUNT                 PIC S9(04) COMP VALUE ZERO.
       01  WS-GL-TABLE.
           05  WS-GL-ENTRY             OCCURS 50 TIMES
                                       INDEXED BY GL-IDX.
               10  WS-GT-ACCOUNT       PIC X(10).
               10  WS-GT-COST-CTR      PIC X(06).
               10  WS-GT-LINES         PIC S9(07)    COMP-3.
               10  WS-GT-DR            PIC S9(15)V99 COMP-3.
               10  WS-GT-CR            PIC S9(15)V99 COMP-3.
      *
       01  WS-TOTALS.
           05  WS-SUM-READ             PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-GL-READ              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-ENTL             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-ACCRUED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-SKIPPED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-USD              PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-AMT              PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-DR               PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-CR               PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-DR-USD           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-NOT-REVERSING    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NET                  PIC S9(15)V99 COMP-3 VALUE ZERO.
      *
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-OUT                 PIC X(10).
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
           COPY CMRPTHD.
      *
       01  WS-SECTION-LINE.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  WS-SL-TEXT              PIC X(100).
           05  FILLER                  PIC X(31) VALUE SPACES.
      *
       01  WS-EVT-COL-1.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(13) VALUE 'EVENT ID'.
           05  FILLER                  PIC X(10) VALUE 'CUSIP'.
           05  FILLER                  PIC X(04) VALUE 'TYP'.
           05  FILLER                  PIC X(11) VALUE 'PAY DATE'.
           05  FILLER                  PIC X(06) VALUE ' DAYS'.
           05  FILLER                  PIC X(08) VALUE 'BUCKET'.
           05  FILLER                  PIC X(04) VALUE 'CCY'.
           05  FILLER                  PIC X(07) VALUE '  ENTL'.
           05  FILLER                  PIC X(07) VALUE ' ACCRD'.
           05  FILLER                  PIC X(05) VALUE 'SKIP'.
           05  FILLER                  PIC X(16) VALUE
               '  ACCRUAL AMOUNT'.
           05  FILLER                  PIC X(16) VALUE
               '     USD EQUIV'.
           05  FILLER                  PIC X(11) VALUE 'DR GL'.
           05  FILLER                  PIC X(14) VALUE 'CR GL'.
       01  WS-DASH-LINE.
           05  FILLER                  PIC X(01) VALUE ' '.
           05  FILLER                  PIC X(132) VALUE ALL '-'.
      *
       01  WS-EVT-DETAIL.
           05  ED-CC                   PIC X(01).
           05  ED-EVENT-ID             PIC X(12).
           05  FILLER                  PIC X(01).
           05  ED-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  ED-TYPE                 PIC X(03).
           05  FILLER                  PIC X(01).
           05  ED-PAY-DATE             PIC X(10).
           05  FILLER                  PIC X(01).
           05  ED-DAYS                 PIC ZZZ9-.
           05  FILLER                  PIC X(01).
           05  ED-BUCKET               PIC X(07).
           05  FILLER                  PIC X(01).
           05  ED-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  ED-ENTL                 PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  ED-ACCRUED              PIC ZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  ED-SKIPPED              PIC ZZZ9.
           05  FILLER                  PIC X(01).
           05  ED-AMOUNT               PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  ED-USD                  PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  ED-DR-GL                PIC X(10).
           05  FILLER                  PIC X(01).
           05  ED-CR-GL                PIC X(10).
           05  FILLER                  PIC X(03).
      *
       01  WS-BUCKET-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  BL-LABEL                PIC X(30).
           05  FILLER                  PIC X(08) VALUE 'EVENTS: '.
           05  BL-EVENTS               PIC ZZ,ZZ9.
           05  FILLER                  PIC X(12) VALUE '   USD AMT: '.
           05  BL-USD                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(52) VALUE SPACES.
      *
       01  WS-GL-COL-1.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  FILLER                  PIC X(12) VALUE 'GL ACCOUNT'.
           05  FILLER                  PIC X(08) VALUE 'CC'.
           05  FILLER                  PIC X(08) VALUE ' LINES'.
           05  FILLER                  PIC X(21) VALUE
               '              DEBITS'.
           05  FILLER                  PIC X(21) VALUE
               '             CREDITS'.
           05  FILLER                  PIC X(21) VALUE
               '                 NET'.
           05  FILLER                  PIC X(36) VALUE SPACES.
      *
       01  WS-GL-DETAIL.
           05  GD-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  GD-ACCOUNT              PIC X(10).
           05  FILLER                  PIC X(02).
           05  GD-COST-CTR             PIC X(06).
           05  FILLER                  PIC X(02).
           05  GD-LINES                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  GD-DR                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  GD-CR                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  GD-NET                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(38).
      *
       01  WS-MSG-LINE.
           05  ML-CC                   PIC X(01).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  ML-TEXT                 PIC X(100).
           05  FILLER                  PIC X(27) VALUE SPACES.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZZ,ZZ9.
       01  WS-EDIT-AMT                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-EDIT-AMT2                PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
           COPY CMDATEW.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMDTLNK.
           COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-EVENT-SECTION
           PERFORM 3000-GL-SECTION
           PERFORM 4000-PROOF
           PERFORM 9000-TERMINATE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           INITIALIZE AB-ABEND-PARMS
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS   TO AB-FILE-STATUS
               MOVE 'DATECARD'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON DATE CARD' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1005                 TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS   TO AB-FILE-STATUS
               MOVE 'DATECARD'           TO AB-DDNAME
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                   TO AU-FUNCTION
           MOVE WS-PROGRAM-ID            TO AU-PROGRAM
           MOVE 'START'                  TO AU-EVENT
           MOVE 'I'                      TO AU-SEVERITY
           MOVE DC-BUS-DATE              TO AU-BUS-DATE
           MOVE SPACES                   TO AU-KEY
           MOVE 'CA ACCRUAL REPORT STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT ACCSUMIN-FILE
           IF WS-ACCSUMIN-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-ACCSUMIN-STATUS   TO AB-FILE-STATUS
               MOVE 'ACCSUMIN'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON ACCRUAL SUMMARY' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           OPEN INPUT ACCRGL-FILE
           IF WS-ACCRGL-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-ACCRGL-STATUS     TO AB-FILE-STATUS
               MOVE 'ACCRGL'             TO AB-DDNAME
               MOVE 'OPEN FAILED ON ACCRUAL JOURNAL' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS     TO AB-FILE-STATUS
               MOVE 'RPTFILE'            TO AB-DDNAME
               MOVE 'OPEN FAILED ON REPORT FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
      *    AGING BUCKETS - DAYS FROM BUSINESS DATE TO PAY DATE
           MOVE '0-5    '   TO WS-BK-LABEL (1)
           MOVE 5           TO WS-BK-HIGH-DAYS (1)
           MOVE '6-30   '   TO WS-BK-LABEL (2)
           MOVE 30          TO WS-BK-HIGH-DAYS (2)
           MOVE 'OVER 30'   TO WS-BK-LABEL (3)
           MOVE 99999       TO WS-BK-HIGH-DAYS (3)
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 3
               MOVE ZERO TO WS-BK-EVENTS (WS-BKT)
                            WS-BK-USD (WS-BKT)
           END-PERFORM
      *
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE WS-PROGRAM-ID            TO RPT-H1-REPORT-ID
                                            RPT-H2-PROGRAM
           MOVE TS-TIMESTAMP(1:10)       TO RPT-H1-RUN-DATE
           MOVE 'CORPORATE ACTION DIVIDEND ACCRUAL REPORT'
                                         TO RPT-H2-TITLE
           MOVE DC-BUS-DATE              TO WS-DATE-IN
           PERFORM 7000-FORMAT-DATE
           MOVE WS-DATE-OUT              TO RPT-H2-BUS-DATE
           .
      *
      *================================================================*
      * SECTION 1 - ACCRUALS BY EVENT                                  *
      *================================================================*
       2000-EVENT-SECTION.
           SET WS-SECTION-EVENTS         TO TRUE
           MOVE 'SECTION 1 - DIVIDEND ACCRUALS BY EVENT (AUTO-REVERSING'
             TO WS-SL-TEXT
           MOVE ', REBOOKED DAILY UNTIL PAY DATE)' TO WS-SL-TEXT(55:)
           PERFORM 7100-PAGE-HEADING
      *
           PERFORM 8000-READ-SUMMARY
           IF WS-SUM-EOF
               MOVE '0'                  TO ML-CC
               MOVE 'NO ACCRUALS BOOKED TODAY' TO ML-TEXT
               MOVE WS-MSG-LINE          TO REPORT-REC
               PERFORM 8200-WRITE-LINE
           END-IF
      *
           PERFORM UNTIL WS-SUM-EOF
               PERFORM 2100-EVENT-LINE
               PERFORM 8000-READ-SUMMARY
           END-PERFORM
      *
           MOVE WS-DASH-LINE             TO REPORT-REC
           PERFORM 8200-WRITE-LINE
           MOVE SPACES                   TO WS-EVT-DETAIL
           MOVE '0'                      TO ED-CC
           MOVE 'TOTAL'                  TO ED-EVENT-ID
           MOVE WS-TOT-ENTL              TO ED-ENTL
           MOVE WS-TOT-ACCRUED           TO ED-ACCRUED
           MOVE WS-TOT-SKIPPED           TO ED-SKIPPED
           MOVE WS-TOT-USD               TO ED-USD
           MOVE WS-EVT-DETAIL            TO REPORT-REC
           PERFORM 8200-WRITE-LINE
      *
           PERFORM VARYING WS-BKT FROM 1 BY 1 UNTIL WS-BKT > 3
               MOVE SPACES               TO BL-LABEL
               IF WS-BKT = 1
                   MOVE '0'              TO BL-CC
               ELSE
                   MOVE ' '              TO BL-CC
               END-IF
               STRING 'DAYS TO PAY ' WS-BK-LABEL (WS-BKT)
                      DELIMITED BY SIZE INTO BL-LABEL
               MOVE WS-BK-EVENTS (WS-BKT) TO BL-EVENTS
               MOVE WS-BK-USD (WS-BKT)    TO BL-USD
               MOVE WS-BUCKET-LINE        TO REPORT-REC
               PERFORM 8200-WRITE-LINE
           END-PERFORM
           .
      *
       2100-EVENT-LINE.
           MOVE SPACES                   TO WS-EVT-DETAIL
           MOVE ' '                      TO ED-CC
           MOVE ACS-EVENT-ID             TO ED-EVENT-ID
           MOVE ACS-CUSIP                TO ED-CUSIP
           MOVE ACS-EVENT-TYPE           TO ED-TYPE
           MOVE ACS-PAY-DATE             TO WS-DATE-IN
           PERFORM 7000-FORMAT-DATE
           MOVE WS-DATE-OUT              TO ED-PAY-DATE
      *
           MOVE 'DIFC'                   TO DT-FUNCTION
           MOVE 'NYSE'                   TO DT-CALENDAR
           MOVE DC-BUS-DATE              TO DT-DATE-1
           MOVE ACS-PAY-DATE             TO DT-DATE-2
           CALL 'CMU010' USING DT-DATE-PARMS
           IF DT-OK
               MOVE DT-RESULT-NUM        TO WS-DAYS-TO-PAY
           ELSE
               MOVE ZERO                 TO WS-DAYS-TO-PAY
           END-IF
           MOVE WS-DAYS-TO-PAY           TO ED-DAYS
      *
           PERFORM VARYING WS-BKT FROM 1 BY 1
                   UNTIL WS-BKT > 2
                      OR WS-DAYS-TO-PAY NOT > WS-BK-HIGH-DAYS (WS-BKT)
               CONTINUE
           END-PERFORM
           MOVE WS-BK-LABEL (WS-BKT)     TO ED-BUCKET
           ADD 1                         TO WS-BK-EVENTS (WS-BKT)
           ADD ACS-ACCRUAL-USD           TO WS-BK-USD (WS-BKT)
      *
           MOVE ACS-CCY                  TO ED-CCY
           MOVE ACS-ENTL-COUNT           TO ED-ENTL
           MOVE ACS-ACCRUED-COUNT        TO ED-ACCRUED
           MOVE ACS-SKIP-COUNT           TO ED-SKIPPED
           MOVE ACS-ACCRUAL-AMT          TO ED-AMOUNT
           MOVE ACS-ACCRUAL-USD          TO ED-USD
           MOVE ACS-DR-GL-ACCOUNT        TO ED-DR-GL
           MOVE ACS-CR-GL-ACCOUNT        TO ED-CR-GL
           MOVE WS-EVT-DETAIL            TO REPORT-REC
           PERFORM 8200-WRITE-LINE
      *
           ADD ACS-ENTL-COUNT            TO WS-TOT-ENTL
           ADD ACS-ACCRUED-COUNT         TO WS-TOT-ACCRUED
           ADD ACS-SKIP-COUNT            TO WS-TOT-SKIPPED
           ADD ACS-ACCRUAL-USD           TO WS-TOT-USD
           ADD ACS-ACCRUAL-AMT           TO WS-TOT-AMT
           .
      *
      *================================================================*
      * SECTION 2 - GL ACCOUNT SUMMARY OF THE ACCRUAL JOURNAL          *
      *================================================================*
       3000-GL-SECTION.
           PERFORM 8100-READ-GL
           PERFORM UNTIL WS-GL-EOF
               PERFORM 3100-ACCUMULATE-GL
               PERFORM 8100-READ-GL
           END-PERFORM
      *
           SET WS-SECTION-GL             TO TRUE
           MOVE 'SECTION 2 - ACCRUAL JOURNAL BY GL ACCOUNT (TXN CACR)'
                                         TO WS-SL-TEXT
           PERFORM 7100-PAGE-HEADING
      *
           PERFORM VARYING GL-IDX FROM 1 BY 1
                   UNTIL GL-IDX > WS-GL-COUNT
               MOVE SPACES               TO WS-GL-DETAIL
               MOVE ' '                  TO GD-CC
               MOVE WS-GT-ACCOUNT (GL-IDX)  TO GD-ACCOUNT
               MOVE WS-GT-COST-CTR (GL-IDX) TO GD-COST-CTR
               MOVE WS-GT-LINES (GL-IDX) TO GD-LINES
               MOVE WS-GT-DR (GL-IDX)    TO GD-DR
               MOVE WS-GT-CR (GL-IDX)    TO GD-CR
               COMPUTE WS-NET = WS-GT-DR (GL-IDX) - WS-GT-CR (GL-IDX)
               MOVE WS-NET               TO GD-NET
               MOVE WS-GL-DETAIL         TO REPORT-REC
               PERFORM 8200-WRITE-LINE
           END-PERFORM
      *
           MOVE WS-DASH-LINE             TO REPORT-REC
           PERFORM 8200-WRITE-LINE
           MOVE SPACES                   TO WS-GL-DETAIL
           MOVE ' '                      TO GD-CC
           MOVE 'TOTAL'                  TO GD-ACCOUNT
           MOVE WS-GL-READ               TO GD-LINES
           MOVE WS-TOT-DR                TO GD-DR
           MOVE WS-TOT-CR                TO GD-CR
           COMPUTE WS-NET = WS-TOT-DR - WS-TOT-CR
           MOVE WS-NET                   TO GD-NET
           MOVE WS-GL-DETAIL             TO REPORT-REC
           PERFORM 8200-WRITE-LINE
           .
      *
       3100-ACCUMULATE-GL.
           IF NOT GLJ-AUTO-REVERSE
               ADD 1                     TO WS-TOT-NOT-REVERSING
           END-IF
           IF GLJ-DEBIT
               ADD GLJ-AMOUNT            TO WS-TOT-DR
               ADD GLJ-AMOUNT-USD        TO WS-TOT-DR-USD
           ELSE
               ADD GLJ-AMOUNT            TO WS-TOT-CR
           END-IF
      *
           SET GL-IDX                    TO 1
           SEARCH WS-GL-ENTRY
               AT END
                   PERFORM 3200-ADD-GL-ACCOUNT
               WHEN GL-IDX > WS-GL-COUNT
                   PERFORM 3200-ADD-GL-ACCOUNT
               WHEN WS-GT-ACCOUNT (GL-IDX) = GLJ-GL-ACCOUNT
                AND WS-GT-COST-CTR (GL-IDX) = GLJ-COST-CENTER
                   CONTINUE
           END-SEARCH
      *
           ADD 1                         TO WS-GT-LINES (GL-IDX)
           IF GLJ-DEBIT
               ADD GLJ-AMOUNT            TO WS-GT-DR (GL-IDX)
           ELSE
               ADD GLJ-AMOUNT            TO WS-GT-CR (GL-IDX)
           END-IF
           .
      *
       3200-ADD-GL-ACCOUNT.
           IF WS-GL-COUNT NOT < WS-GL-MAX
               MOVE '3200-ADD-GL-ACCOUNT' TO AB-PARAGRAPH
               MOVE 1007                 TO AB-ABEND-CODE
               MOVE SPACES               TO AB-FILE-STATUS
               MOVE 'ACCRGL'             TO AB-DDNAME
               MOVE GLJ-GL-ACCOUNT       TO AB-KEY
               MOVE 'MORE THAN 50 GL ACCOUNTS IN ACCRUAL JOURNAL'
                                         TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           ADD 1                         TO WS-GL-COUNT
           SET GL-IDX                    TO WS-GL-COUNT
           MOVE GLJ-GL-ACCOUNT           TO WS-GT-ACCOUNT (GL-IDX)
           MOVE GLJ-COST-CENTER          TO WS-GT-COST-CTR (GL-IDX)
           MOVE ZERO                     TO WS-GT-LINES (GL-IDX)
                                            WS-GT-DR (GL-IDX)
                                            WS-GT-CR (GL-IDX)
           .
      *
      *================================================================*
      * BALANCE AND PROOF                                              *
      *================================================================*
       4000-PROOF.
           MOVE '0'                      TO ML-CC
           IF WS-TOT-DR = WS-TOT-CR
               MOVE 'JOURNAL IN BALANCE - DEBITS EQUAL CREDITS'
                                         TO ML-TEXT
           ELSE
               MOVE '*** JOURNAL OUT OF BALANCE - DEBITS NOT EQUAL '
                 & 'CREDITS ***'         TO ML-TEXT
               MOVE 4                    TO WS-RETURN-CODE
           END-IF
           MOVE WS-MSG-LINE              TO REPORT-REC
           PERFORM 8200-WRITE-LINE
      *
           MOVE ' '                      TO ML-CC
           MOVE SPACES                   TO ML-TEXT
           MOVE WS-TOT-AMT               TO WS-EDIT-AMT
           MOVE WS-TOT-DR                TO WS-EDIT-AMT2
           IF WS-TOT-AMT = WS-TOT-DR
               STRING 'EVENT SUMMARY ' WS-EDIT-AMT
                      ' AGREES WITH JOURNAL DEBITS ' WS-EDIT-AMT2
                      DELIMITED BY SIZE INTO ML-TEXT
           ELSE
               STRING '*** EVENT SUMMARY ' WS-EDIT-AMT
                      ' DOES NOT AGREE WITH DEBITS ' WS-EDIT-AMT2
                      DELIMITED BY SIZE INTO ML-TEXT
               MOVE 4                    TO WS-RETURN-CODE
           END-IF
           MOVE WS-MSG-LINE              TO REPORT-REC
           PERFORM 8200-WRITE-LINE
      *
           IF WS-TOT-NOT-REVERSING > ZERO
               MOVE SPACES               TO ML-TEXT
               MOVE WS-TOT-NOT-REVERSING TO WS-DISP-COUNT
               STRING '*** ' WS-DISP-COUNT
                      ' JOURNAL LINES NOT FLAGGED AUTO-REVERSING'
                      DELIMITED BY SIZE INTO ML-TEXT
               MOVE WS-MSG-LINE          TO REPORT-REC
               PERFORM 8200-WRITE-LINE
               MOVE 4                    TO WS-RETURN-CODE
           END-IF
      *
           SET WS-SECTION-END            TO TRUE
           MOVE RPT-END-LINE             TO REPORT-REC
           PERFORM 8200-WRITE-LINE
           .
      *
      *================================================================*
      * UTILITIES                                                      *
      *================================================================*
       7000-FORMAT-DATE.
           IF WS-DATE-IN = ZERO OR WS-DATE-IN NOT NUMERIC
               MOVE SPACES               TO WS-DATE-OUT
           ELSE
               STRING WS-DI-CCYY '-' WS-DI-MM '-' WS-DI-DD
                      DELIMITED BY SIZE INTO WS-DATE-OUT
           END-IF
           .
      *
       7100-PAGE-HEADING.
           ADD 1                         TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT           TO RPT-H1-PAGE
           MOVE RPT-HEADING-1            TO REPORT-REC
           PERFORM 8300-PUT-LINE
           MOVE RPT-HEADING-2            TO REPORT-REC
           PERFORM 8300-PUT-LINE
           MOVE WS-SECTION-LINE          TO REPORT-REC
           PERFORM 8300-PUT-LINE
           MOVE 4                        TO RPT-LINE-COUNT
           EVALUATE TRUE
               WHEN WS-SECTION-EVENTS
                   MOVE WS-EVT-COL-1     TO REPORT-REC
                   PERFORM 8300-PUT-LINE
                   MOVE WS-DASH-LINE     TO REPORT-REC
                   PERFORM 8300-PUT-LINE
                   ADD 3                 TO RPT-LINE-COUNT
               WHEN WS-SECTION-GL
                   MOVE WS-GL-COL-1      TO REPORT-REC
                   PERFORM 8300-PUT-LINE
                   MOVE WS-DASH-LINE     TO REPORT-REC
                   PERFORM 8300-PUT-LINE
                   ADD 3                 TO RPT-LINE-COUNT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
      *
       8000-READ-SUMMARY.
           READ ACCSUMIN-FILE
           EVALUATE TRUE
               WHEN ACCSUMIN-OK
                   ADD 1                 TO WS-SUM-READ
               WHEN ACCSUMIN-EOF
                   SET WS-SUM-EOF        TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-SUMMARY'  TO AB-PARAGRAPH
                   MOVE 1002                 TO AB-ABEND-CODE
                   MOVE WS-ACCSUMIN-STATUS   TO AB-FILE-STATUS
                   MOVE 'ACCSUMIN'           TO AB-DDNAME
                   MOVE 'READ FAILED ON ACCRUAL SUMMARY' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           .
      *
       8100-READ-GL.
           READ ACCRGL-FILE
           EVALUATE TRUE
               WHEN ACCRGL-OK
                   ADD 1                 TO WS-GL-READ
               WHEN ACCRGL-EOF
                   SET WS-GL-EOF         TO TRUE
               WHEN OTHER
                   MOVE '8100-READ-GL'       TO AB-PARAGRAPH
                   MOVE 1002                 TO AB-ABEND-CODE
                   MOVE WS-ACCRGL-STATUS     TO AB-FILE-STATUS
                   MOVE 'ACCRGL'             TO AB-DDNAME
                   MOVE 'READ FAILED ON ACCRUAL JOURNAL' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           .
      *
      *    BODY LINE WITH PAGE OVERFLOW
       8200-WRITE-LINE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               MOVE REPORT-REC           TO WS-HOLD-LINE
               PERFORM 7100-PAGE-HEADING
               MOVE WS-HOLD-LINE         TO REPORT-REC
           END-IF
           PERFORM 8300-PUT-LINE
           ADD 1                         TO RPT-LINE-COUNT
           IF REPORT-REC(1:1) = '0'
               ADD 1                     TO RPT-LINE-COUNT
           END-IF
           .
      *
       8300-PUT-LINE.
           WRITE REPORT-REC
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '8300-PUT-LINE'      TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS     TO AB-FILE-STATUS
               MOVE 'RPTFILE'            TO AB-DDNAME
               MOVE 'WRITE FAILED ON REPORT FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           ADD 1                         TO WS-LINES-WRITTEN
           .
      *
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE ACCSUMIN-FILE
           IF WS-ACCSUMIN-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-ACCSUMIN-STATUS   TO AB-FILE-STATUS
               MOVE 'ACCSUMIN'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ACCRUAL SUMMARY' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE ACCRGL-FILE
           IF WS-ACCRGL-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-ACCRGL-STATUS     TO AB-FILE-STATUS
               MOVE 'ACCRGL'             TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ACCRUAL JOURNAL' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE REPORT-FILE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS     TO AB-FILE-STATUS
               MOVE 'RPTFILE'            TO AB-DDNAME
               MOVE 'CLOSE FAILED ON REPORT FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
           MOVE 'POST'                   TO CT-FUNCTION
           MOVE DC-BUS-DATE              TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID            TO CT-PROGRAM
           MOVE 'CAR510'                 TO CT-STAGE
           MOVE 'ACCRUAL-IN'             TO CT-COUNTER-NAME
           MOVE WS-GL-READ               TO CT-COUNT
           MOVE WS-TOT-DR                TO CT-AMOUNT
           MOVE ZERO                     TO CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1010                 TO AB-ABEND-CODE
               MOVE 'CTLTOTS'            TO AB-DDNAME
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           MOVE 'CLOS'                   TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* CAR510  CA ACCRUAL REPORT     - RUN STATISTICS *'
           DISPLAY '*************************************************'
           MOVE WS-SUM-READ              TO WS-DISP-COUNT
           DISPLAY ' SUMMARY RECORDS READ     : ' WS-DISP-COUNT
           MOVE WS-GL-READ               TO WS-DISP-COUNT
           DISPLAY ' JOURNAL LINES READ       : ' WS-DISP-COUNT
           MOVE WS-GL-COUNT              TO WS-DISP-COUNT
           DISPLAY ' GL ACCOUNTS              : ' WS-DISP-COUNT
           MOVE WS-TOT-DR                TO WS-EDIT-AMT
           DISPLAY ' TOTAL DEBITS             : ' WS-EDIT-AMT
           MOVE WS-TOT-CR                TO WS-EDIT-AMT
           DISPLAY ' TOTAL CREDITS            : ' WS-EDIT-AMT
           MOVE RPT-PAGE-COUNT           TO WS-DISP-COUNT
           DISPLAY ' REPORT PAGES             : ' WS-DISP-COUNT
           MOVE WS-LINES-WRITTEN         TO WS-DISP-COUNT
           DISPLAY ' REPORT LINES             : ' WS-DISP-COUNT
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE
      *
           MOVE 'WRIT'                   TO AU-FUNCTION
           MOVE 'END'                    TO AU-EVENT
           MOVE 'I'                      TO AU-SEVERITY
           MOVE SPACES                   TO AU-KEY
           MOVE 'CA ACCRUAL REPORT ENDED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                   TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
           .
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID            TO AB-PROGRAM
           DISPLAY 'CAR510 - ABENDING: ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                       TO RETURN-CODE
           GOBACK
           .
