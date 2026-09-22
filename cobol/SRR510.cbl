       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR510.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MAY 2013.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR510                                            *
      * DESCRIPTION: STOCK RECORD BREAK REPORT.                        *
      *              ONE BLOCK PER CUSIP IN BREAK: THE BREAK LINES     *
      *              (SETTLED, TRADE DATE, FIRM SHORT) WITH OWNER,     *
      *              FIRM AND LOCATION QUANTITIES, DIFFERENCE, VALUE   *
      *              AND AGE, FOLLOWED BY EVERY POSITION ROW OF THE    *
      *              CUSIP.  SUMMARY BY BREAK TYPE AND AGE.            *
      *              REPLACES THE 1988 SRB500 PRINT (SYSOUT ONLY).     *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD060 / STEP050                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              BRKDTL   - MSEC.PROD.SR.BREAKS.DETAIL(+1)(SRBRKD) *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2013-05-06 SPA  ORIGINAL                              CHG24790 *
      * 2016-10-03 SPA  ROLE COLUMN (OMNIBUS)                 CHG30112 *
      * 2019-02-25 MHC  CONTROL TOTALS                        CHG33410 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT BRKDTL-FILE    ASSIGN TO BRKDTL
                  FILE STATUS IS WS-BRKDTL-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  BRKDTL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BRKDTL-REC                  PIC X(150).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR510'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-BRKDTL-STATUS        PIC X(02)  VALUE '00'.
               88  BRKDTL-OK                      VALUE '00'.
               88  BRKDTL-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-DETAIL                  VALUE 'Y'.
           05  WS-DTL-HEAD-SW          PIC X(01)  VALUE 'N'.
               88  DETAIL-HEAD-PRINTED            VALUE 'Y'.
       01  WS-LAST-CUSIP               PIC X(09)  VALUE LOW-VALUES.
      *----------------------------------------------------------------*
      * SUMMARY TABLES                                                 *
      *----------------------------------------------------------------*
       01  WS-BRK-TYPE-VALUES.
           05  FILLER  PIC X(14)  VALUE 'SDSETTLED     '.
           05  FILLER  PIC X(14)  VALUE 'TDTRADE DATE  '.
           05  FILLER  PIC X(14)  VALUE 'FSFIRM SHORT  '.
       01  WS-BRK-TYPE-TABLE REDEFINES WS-BRK-TYPE-VALUES.
           05  WS-BT-ENTRY OCCURS 3 TIMES INDEXED BY BT-IDX.
               10  WS-BT-CODE          PIC X(02).
               10  WS-BT-NAME          PIC X(12).
       01  WS-BRK-TYPE-TOTALS.
           05  WS-BTT OCCURS 3 TIMES.
               10  WS-BTT-COUNT        PIC S9(07)       COMP-3.
               10  WS-BTT-NEW          PIC S9(07)       COMP-3.
               10  WS-BTT-MV           PIC S9(15)V99    COMP-3.
       01  WS-AGE-BUCKETS.
           05  WS-AGE-CNT OCCURS 4 TIMES PIC S9(07)     COMP-3.
       01  WS-AGE-NAMES                PIC X(48)  VALUE
           '1 DAY       2 - 5 DAYS  6 - 10 DAYS 11 DAYS +   '.
       01  WS-AGE-NAME-TABLE REDEFINES WS-AGE-NAMES.
           05  WS-AGE-NAME OCCURS 4 TIMES PIC X(12).
       01  WS-SUB                      PIC S9(04) COMP.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HDR-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DTL-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CUSIP-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-MV               PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(11)  VALUE ' CUSIP'.
           05  FILLER  PIC X(03)  VALUE 'ST'.
           05  FILLER  PIC X(16)  VALUE 'BREAK TYPE'.
           05  FILLER  PIC X(18)  VALUE '        OWNER QTY'.
           05  FILLER  PIC X(18)  VALUE '         FIRM QTY'.
           05  FILLER  PIC X(18)  VALUE '     LOCATION QTY'.
           05  FILLER  PIC X(18)  VALUE '       DIFFERENCE'.
           05  FILLER  PIC X(16)  VALUE '  VALUE USD'.
           05  FILLER  PIC X(04)  VALUE 'AGE'.
           05  FILLER  PIC X(10)  VALUE ' OWN/LOC'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(11)  VALUE ' ---------'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(16)  VALUE '-------------'.
           05  FILLER  PIC X(18)  VALUE '-----------------'.
           05  FILLER  PIC X(18)  VALUE '-----------------'.
           05  FILLER  PIC X(18)  VALUE '-----------------'.
           05  FILLER  PIC X(18)  VALUE '-----------------'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(10)  VALUE ' ---------'.
       01  WS-BREAK-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  BL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  BL-SEC-TYPE             PIC X(02).
           05  FILLER                  PIC X(01).
           05  BL-TYPE                 PIC X(02).
           05  FILLER                  PIC X(01).
           05  BL-TYPE-NAME            PIC X(12).
           05  FILLER                  PIC X(01).
           05  BL-OWNER                PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-FIRM                 PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-LOCATION             PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-DIFF                 PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-MV                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  BL-AGE                  PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  BL-OWN-CNT              PIC ZZZ9.
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  BL-LOC-CNT              PIC ZZZ9.
           05  FILLER                  PIC X(01).
       01  WS-DTL-HEAD.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(12)  VALUE SPACES.
           05  FILLER                  PIC X(29)  VALUE
               'ROLE     ACCOUNT    LOC  TY'.
           05  FILLER                  PIC X(18)  VALUE
               '       TRADE DATE'.
           05  FILLER                  PIC X(18)  VALUE
               '          SETTLED'.
           05  FILLER                  PIC X(18)  VALUE
               '          PEND IN'.
           05  FILLER                  PIC X(18)  VALUE
               '         PEND OUT'.
           05  FILLER                  PIC X(19)  VALUE
               ' LAST ACTV'.
       01  WS-DTL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(12).
           05  DL-ROLE                 PIC X(08).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-LOC                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-ACCT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-TD                   PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-SD                   PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-PEND-IN              PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-PEND-OUT             PIC -ZZZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-LAST-ACTV            PIC X(10).
           05  FILLER                  PIC X(10).
       01  WS-SUMMARY-HEAD.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(132) VALUE
               ' SUMMARY BY BREAK TYPE                 COUNT       NEW
      -        '         VALUE USD'.
       01  WS-SUMMARY-LINE.
           05  SL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  SL-NAME                 PIC X(20).
           05  FILLER                  PIC X(08).
           05  SL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  SL-NEW                  PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  SL-MV                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(60).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** STOCK RECORD IN BALANCE - NO BREAKS ***'.
       COPY SRBRKD.
       COPY SRBREAK.
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-RECORD UNTIL END-OF-DETAIL.
           PERFORM 4000-SUMMARY.
           PERFORM 9000-TERMINATE.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'BREAK REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT BRKDTL-FILE.
           IF WS-BRKDTL-STATUS NOT = '00'
               MOVE 'BRKDTL' TO AB-DDNAME
               MOVE WS-BRKDTL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           INITIALIZE WS-BRK-TYPE-TOTALS WS-AGE-BUCKETS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'STOCK RECORD BREAK REPORT' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS.
           PERFORM 8000-READ-DETAIL.
           IF END-OF-DETAIL
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           EVALUATE TRUE
               WHEN BKD-BREAK-HEADER
                   PERFORM 2100-PRINT-BREAK
               WHEN BKD-POSITION-DETAIL
                   PERFORM 2200-PRINT-POSITION
               WHEN OTHER
                   DISPLAY 'SRR510 UNKNOWN RECORD TYPE ' BKD-REC-TYPE
           END-EVALUATE.
           PERFORM 8000-READ-DETAIL.
      *----------------------------------------------------------------*
       2100-PRINT-BREAK.
      *----------------------------------------------------------------*
           ADD 1 TO WS-HDR-CNT.
           MOVE BKD-BREAK-IMAGE TO BRK-BREAK-REC (1:76).
           IF BKD-CUSIP NOT = WS-LAST-CUSIP
               ADD 1 TO WS-CUSIP-CNT
               MOVE BKD-CUSIP TO WS-LAST-CUSIP
               MOVE 'N' TO WS-DTL-HEAD-SW
               MOVE '0' TO BL-CC
               IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
                   PERFORM 8200-HEADINGS
               END-IF
               ADD 1 TO RPT-LINE-COUNT
           ELSE
               MOVE ' ' TO BL-CC
           END-IF.
           MOVE BRK-CUSIP          TO BL-CUSIP.
           MOVE BRK-SEC-TYPE       TO BL-SEC-TYPE.
           MOVE BRK-TYPE           TO BL-TYPE.
           MOVE BRK-OWNER-QTY      TO BL-OWNER.
           MOVE BRK-FIRM-QTY       TO BL-FIRM.
           MOVE BRK-LOCATION-QTY   TO BL-LOCATION.
           MOVE BRK-DIFFERENCE     TO BL-DIFF.
           MOVE BRK-MKT-VALUE-USD  TO BL-MV.
           MOVE BRK-AGE-DAYS       TO BL-AGE.
           MOVE BRK-OWNER-COUNT    TO BL-OWN-CNT.
           MOVE BRK-LOCATION-COUNT TO BL-LOC-CNT.
           SET BT-IDX TO 1.
           SEARCH WS-BT-ENTRY
               AT END
                   MOVE 'UNKNOWN' TO BL-TYPE-NAME
                   MOVE 0 TO WS-SUB
               WHEN WS-BT-CODE (BT-IDX) = BRK-TYPE
                   MOVE WS-BT-NAME (BT-IDX) TO BL-TYPE-NAME
                   SET WS-SUB TO BT-IDX
           END-SEARCH.
           IF WS-SUB > ZERO
               ADD 1 TO WS-BTT-COUNT (WS-SUB)
               ADD BRK-MKT-VALUE-USD TO WS-BTT-MV (WS-SUB)
               IF BRK-AGE-DAYS NOT > 1
                   ADD 1 TO WS-BTT-NEW (WS-SUB)
               END-IF
           END-IF.
           EVALUATE TRUE
               WHEN BRK-AGE-DAYS NOT > 1   ADD 1 TO WS-AGE-CNT (1)
               WHEN BRK-AGE-DAYS NOT > 5   ADD 1 TO WS-AGE-CNT (2)
               WHEN BRK-AGE-DAYS NOT > 10  ADD 1 TO WS-AGE-CNT (3)
               WHEN OTHER                  ADD 1 TO WS-AGE-CNT (4)
           END-EVALUATE.
           ADD BRK-MKT-VALUE-USD TO WS-TOT-MV.
           WRITE RPT-RECORD FROM WS-BREAK-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       2200-PRINT-POSITION.
      *----------------------------------------------------------------*
           ADD 1 TO WS-DTL-CNT.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
               MOVE 'N' TO WS-DTL-HEAD-SW
           END-IF.
           IF NOT DETAIL-HEAD-PRINTED
               WRITE RPT-RECORD FROM WS-DTL-HEAD
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
               MOVE 'Y' TO WS-DTL-HEAD-SW
           END-IF.
           MOVE SPACES TO WS-DTL-LINE.
           MOVE ' ' TO DL-CC.
           EVALUATE TRUE
               WHEN BKD-ROLE-CLIENT    MOVE 'CLIENT'   TO DL-ROLE
               WHEN BKD-ROLE-FIRM      MOVE 'FIRM'     TO DL-ROLE
               WHEN BKD-ROLE-LOCATION  MOVE 'LOCATION' TO DL-ROLE
               WHEN OTHER              MOVE '?'        TO DL-ROLE
           END-EVALUATE.
           MOVE BKD-ACCT-NO        TO DL-ACCT.
           MOVE BKD-LOCATION       TO DL-LOC.
           MOVE BKD-ACCT-TYPE      TO DL-ACCT-TYPE.
           MOVE BKD-TD-QTY         TO DL-TD.
           MOVE BKD-SD-QTY         TO DL-SD.
           MOVE BKD-PEND-IN-QTY    TO DL-PEND-IN.
           MOVE BKD-PEND-OUT-QTY   TO DL-PEND-OUT.
           IF BKD-LAST-ACTV-DATE NUMERIC
           AND BKD-LAST-ACTV-DATE > ZERO
               MOVE BKD-LAST-ACTV-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE
               MOVE WS-DATE-EDIT TO DL-LAST-ACTV
           END-IF.
           WRITE RPT-RECORD FROM WS-DTL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *================================================================*
       4000-SUMMARY.
      *================================================================*
           IF RPT-LINE-COUNT + 14 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-SUMMARY-HEAD.
           PERFORM 8900-CHECK-WRITE.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 3
               MOVE SPACES TO WS-SUMMARY-LINE
               IF WS-SUB = 1
                   MOVE '0' TO SL-CC
               ELSE
                   MOVE ' ' TO SL-CC
               END-IF
               MOVE WS-BT-NAME (WS-SUB)   TO SL-NAME
               MOVE WS-BTT-COUNT (WS-SUB) TO SL-COUNT
               MOVE WS-BTT-NEW (WS-SUB)   TO SL-NEW
               MOVE WS-BTT-MV (WS-SUB)    TO SL-MV
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE
           END-PERFORM.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4
               MOVE SPACES TO WS-SUMMARY-LINE
               IF WS-SUB = 1
                   MOVE '0' TO SL-CC
               ELSE
                   MOVE ' ' TO SL-CC
               END-IF
               STRING 'AGE ' WS-AGE-NAME (WS-SUB)
                      DELIMITED BY SIZE INTO SL-NAME
               MOVE WS-AGE-CNT (WS-SUB) TO SL-COUNT
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE
           END-PERFORM.
           MOVE SPACES TO WS-SUMMARY-LINE.
           MOVE '0' TO SL-CC.
           MOVE 'CUSIPS IN BREAK' TO SL-NAME.
           MOVE WS-CUSIP-CNT TO SL-COUNT.
           MOVE WS-TOT-MV TO SL-MV.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
       8000-READ-DETAIL.
      *================================================================*
           READ BRKDTL-FILE INTO BKD-DETAIL-REC.
           EVALUATE TRUE
               WHEN BRKDTL-OK
                   CONTINUE
               WHEN BRKDTL-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'BRKDTL' TO AB-DDNAME
                   MOVE WS-BRKDTL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE.
           MOVE 5 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE.
      *----------------------------------------------------------------*
           MOVE WS-DI-CCYY TO WS-DE-CCYY.
           MOVE WS-DI-MM   TO WS-DE-MM.
           MOVE WS-DI-DD   TO WS-DE-DD.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE BRKDTL-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'SRR510'        TO CT-STAGE.
           MOVE 'BREAKS-IN'     TO CT-COUNTER-NAME.
           MOVE WS-HDR-CNT      TO CT-COUNT.
           MOVE WS-TOT-MV       TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR510 DETAIL RECORDS READ : ' WS-READ-CNT.
           DISPLAY 'SRR510 BREAKS REPORTED     : ' WS-HDR-CNT.
           DISPLAY 'SRR510 CUSIPS IN BREAK     : ' WS-CUSIP-CNT.
           DISPLAY 'SRR510 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'BREAK REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SRR510 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
