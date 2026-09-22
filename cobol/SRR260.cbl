       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR260.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JUNE 1991.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR260                                            *
      * DESCRIPTION: SETTLEMENT FAILS AGING REPORT.                    *
      *              LISTS EVERY CUSTOMER / FIRM LEG THAT FAILED TO    *
      *              SETTLE, OLDEST CONTRACTUAL SETTLE DATE FIRST,     *
      *              WITH THE NUMBER OF BUSINESS DAYS OUTSTANDING.     *
      *              SUMMARY BY AGING BUCKET AND RECEIVE / DELIVER.    *
      *              STREET (LOCATION) LEGS ARE COUNTED ONLY.          *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD030 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              FAILIN   - MSEC.PROD.SR.FAILS(+1)        (SRFAIL) *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1991-06-03 DWB  ORIGINAL                              CHG00987 *
      * 1995-06-07 LFM  T+3 BUCKETS                           CHG02140 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2008-01-14 SPA  REG SHO CLOSE-OUT BUCKET (13+ DAYS)   CHG17720 *
      * 2017-09-05 MHC  MARKET VALUE COLUMN                   CHG31388 *
      * 2024-05-20 NVR  T+1                                   CHG40551 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT FAILIN-FILE    ASSIGN TO FAILIN
                  FILE STATUS IS WS-FAILIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  FAILIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRFAIL.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR260'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-FAILIN-STATUS        PIC X(02)  VALUE '00'.
               88  FAILIN-OK                      VALUE '00'.
               88  FAILIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-EOF-SW                   PIC X(01)  VALUE 'N'.
           88  END-OF-FAILS                       VALUE 'Y'.
      *----------------------------------------------------------------*
      * AGING BUCKETS (BUSINESS DAYS)                                  *
      *----------------------------------------------------------------*
       01  WS-BUCKET-VALUES.
           05  FILLER  PIC X(21)  VALUE '001001 1 DAY         '.
           05  FILLER  PIC X(21)  VALUE '002003 2 - 3 DAYS    '.
           05  FILLER  PIC X(21)  VALUE '004005 4 - 5 DAYS    '.
           05  FILLER  PIC X(21)  VALUE '006012 6 - 12 DAYS   '.
           05  FILLER  PIC X(21)  VALUE '013999 13 DAYS +     '.
       01  WS-BUCKET-TABLE REDEFINES WS-BUCKET-VALUES.
           05  WS-BKT OCCURS 5 TIMES INDEXED BY BK-IDX.
               10  WS-BKT-LOW          PIC 9(03).
               10  WS-BKT-HIGH         PIC 9(03).
               10  WS-BKT-NAME         PIC X(15).
       01  WS-BUCKET-TOTALS.
           05  WS-BT OCCURS 5 TIMES.
               10  WS-BT-RCV-CNT       PIC S9(07)       COMP-3.
               10  WS-BT-RCV-AMT       PIC S9(15)V99    COMP-3.
               10  WS-BT-DLV-CNT       PIC S9(07)       COMP-3.
               10  WS-BT-DLV-AMT       PIC S9(15)V99    COMP-3.
               10  WS-BT-MV            PIC S9(15)V99    COMP-3.
       01  WS-BK-SUB                   PIC S9(04) COMP.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STREET-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-AMT              PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-MV               PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-ABS-AMT              PIC S9(15)V99    COMP-3.
           05  WS-OLDEST-DAYS          PIC S9(03)       COMP-3
                                                     VALUE ZERO.
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
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(12)  VALUE ' SETTLE DT'.
           05  FILLER  PIC X(11)  VALUE 'TRADE DT'.
           05  FILLER  PIC X(17)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(03)  VALUE 'LG'.
           05  FILLER  PIC X(11)  VALUE 'ACCOUNT'.
           05  FILLER  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER  PIC X(05)  VALUE 'LOC'.
           05  FILLER  PIC X(04)  VALUE 'TYP'.
           05  FILLER  PIC X(15)  VALUE '      QUANTITY'.
           05  FILLER  PIC X(16)  VALUE '   CONTRACT AMT'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(16)  VALUE '  MKT VALUE USD'.
           05  FILLER  PIC X(05)  VALUE 'DAYS'.
           05  FILLER  PIC X(03)  VALUE 'N'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(12)  VALUE ' ----------'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(15)  VALUE '--------------'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(03)  VALUE '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-SETTLE-DATE          PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-TRADE-DATE           PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-LEG                  PIC 9(01).
           05  FILLER                  PIC X(02).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  DL-LOC                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-TYPE                 PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC -ZZZZZZZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-AMT                  PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-MV                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-DAYS                 PIC ZZZ9.
           05  FILLER                  PIC X(01).
           05  DL-NEW                  PIC X(01).
           05  FILLER                  PIC X(01).
       01  WS-SUMMARY-HEAD.
           05  FILLER  PIC X(01)  VALUE '-'.
           05  FILLER  PIC X(19)  VALUE ' AGING BUCKET'.
           05  FILLER  PIC X(12)  VALUE ' RCV FAILS'.
           05  FILLER  PIC X(22)  VALUE '     RECEIVE AMOUNT'.
           05  FILLER  PIC X(12)  VALUE ' DLV FAILS'.
           05  FILLER  PIC X(22)  VALUE '     DELIVER AMOUNT'.
           05  FILLER  PIC X(22)  VALUE '   MARKET VALUE USD'.
           05  FILLER  PIC X(23)  VALUE SPACES.
       01  WS-SUMMARY-LINE.
           05  SL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  SL-NAME                 PIC X(15).
           05  FILLER                  PIC X(03).
           05  SL-RCV-CNT              PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  SL-RCV-AMT              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  SL-DLV-CNT              PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  SL-DLV-AMT              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  SL-MV                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(26).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  TL-LABEL                PIC X(40).
           05  TL-VALUE                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  TL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(58).
       01  WS-NO-FAILS-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO SETTLEMENT FAILS FOR THIS BUSINESS DATE ***'.
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
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-FAIL THRU 2000-EXIT
               UNTIL END-OF-FAILS.
           PERFORM 3000-PRINT-SUMMARY THRU 3000-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
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
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'FAILS AGING REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT FAILIN-FILE.
           IF WS-FAILIN-STATUS NOT = '00'
               MOVE 'FAILIN' TO AB-DDNAME
               MOVE WS-FAILIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM VARYING WS-BK-SUB FROM 1 BY 1 UNTIL WS-BK-SUB > 5
               MOVE ZERO TO WS-BT-RCV-CNT (WS-BK-SUB)
                            WS-BT-RCV-AMT (WS-BK-SUB)
                            WS-BT-DLV-CNT (WS-BK-SUB)
                            WS-BT-DLV-AMT (WS-BK-SUB)
                            WS-BT-MV      (WS-BK-SUB)
           END-PERFORM.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE 'SRR260'       TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'SETTLEMENT FAILS AGING REPORT' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-FAIL THRU 8000-EXIT.
           IF END-OF-FAILS
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               WRITE RPT-RECORD FROM WS-NO-FAILS-LINE
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-FAIL.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF FLR-ACCT-TYPE = 'ST'
               ADD 1 TO WS-STREET-CNT
               GO TO 2000-READ
           END-IF.
           PERFORM 2100-FIND-BUCKET THRU 2100-EXIT.
           IF FLR-CASH < ZERO
               COMPUTE WS-ABS-AMT = FLR-CASH * -1
           ELSE
               MOVE FLR-CASH TO WS-ABS-AMT
           END-IF.
           IF FLR-QTY > ZERO
               ADD 1          TO WS-BT-RCV-CNT (WS-BK-SUB)
               ADD WS-ABS-AMT TO WS-BT-RCV-AMT (WS-BK-SUB)
           ELSE
               ADD 1          TO WS-BT-DLV-CNT (WS-BK-SUB)
               ADD WS-ABS-AMT TO WS-BT-DLV-AMT (WS-BK-SUB)
           END-IF.
           ADD FLR-MKT-VALUE-USD TO WS-BT-MV (WS-BK-SUB) WS-TOT-MV.
           ADD WS-ABS-AMT TO WS-TOT-AMT.
           IF FLR-FAIL-DAYS > WS-OLDEST-DAYS
               MOVE FLR-FAIL-DAYS TO WS-OLDEST-DAYS
           END-IF.
           IF FLR-NEW-FAIL
               ADD 1 TO WS-NEW-CNT
           END-IF.
           PERFORM 2200-PRINT-DETAIL THRU 2200-EXIT.
       2000-READ.
           PERFORM 8000-READ-FAIL THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-FIND-BUCKET.
      *----------------------------------------------------------------*
           MOVE 5 TO WS-BK-SUB.
           PERFORM VARYING BK-IDX FROM 1 BY 1 UNTIL BK-IDX > 5
               IF FLR-FAIL-DAYS NOT < WS-BKT-LOW (BK-IDX)
               AND FLR-FAIL-DAYS NOT > WS-BKT-HIGH (BK-IDX)
                   SET WS-BK-SUB TO BK-IDX
               END-IF
           END-PERFORM.
      *    ZERO DAYS (FAILED ON CONTRACT DATE) GOES IN THE 1 DAY BUCKET
           IF FLR-FAIL-DAYS < 1
               MOVE 1 TO WS-BK-SUB
           END-IF.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PRINT-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES           TO WS-DETAIL-LINE.
           MOVE ' '              TO DL-CC.
           MOVE FLR-SETTLE-DATE  TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT     TO DL-SETTLE-DATE.
           MOVE FLR-TRADE-DATE   TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT     TO DL-TRADE-DATE.
           MOVE FLR-REF          TO DL-REF.
           MOVE FLR-LEG-NO       TO DL-LEG.
           MOVE FLR-ACCT-NO      TO DL-ACCT.
           MOVE FLR-CUSIP        TO DL-CUSIP.
           MOVE FLR-LOCATION     TO DL-LOC.
           MOVE FLR-ACT-TYPE     TO DL-TYPE.
           MOVE FLR-QTY          TO DL-QTY.
           MOVE FLR-CASH         TO DL-AMT.
           MOVE FLR-CCY          TO DL-CCY.
           MOVE FLR-MKT-VALUE-USD TO DL-MV.
           MOVE FLR-FAIL-DAYS    TO DL-DAYS.
           IF FLR-NEW-FAIL
               MOVE '*' TO DL-NEW
           END-IF.
           PERFORM 8100-WRITE-DETAIL THRU 8100-EXIT.
           ADD 1 TO WS-PRINT-CNT.
       2200-EXIT.
           EXIT.
      *================================================================*
       3000-PRINT-SUMMARY.
      *================================================================*
           IF RPT-LINE-COUNT + 14 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-SUMMARY-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
           PERFORM VARYING WS-BK-SUB FROM 1 BY 1 UNTIL WS-BK-SUB > 5
               MOVE SPACES TO WS-SUMMARY-LINE
               IF WS-BK-SUB = 1
                   MOVE '0' TO SL-CC
               ELSE
                   MOVE ' ' TO SL-CC
               END-IF
               MOVE WS-BKT-NAME   (WS-BK-SUB) TO SL-NAME
               MOVE WS-BT-RCV-CNT (WS-BK-SUB) TO SL-RCV-CNT
               MOVE WS-BT-RCV-AMT (WS-BK-SUB) TO SL-RCV-AMT
               MOVE WS-BT-DLV-CNT (WS-BK-SUB) TO SL-DLV-CNT
               MOVE WS-BT-DLV-AMT (WS-BK-SUB) TO SL-DLV-AMT
               MOVE WS-BT-MV      (WS-BK-SUB) TO SL-MV
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1 TO RPT-LINE-COUNT
           END-PERFORM.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE '0' TO TL-CC.
           MOVE 'CUSTOMER / FIRM LEGS FAILING' TO TL-LABEL.
           MOVE WS-PRINT-CNT TO TL-VALUE.
           MOVE WS-TOT-AMT TO TL-AMOUNT.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
           MOVE 'NEW FAILS TODAY' TO TL-LABEL.
           MOVE WS-NEW-CNT TO TL-VALUE.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
           MOVE 'STREET SIDE LEGS (NOT LISTED)' TO TL-LABEL.
           MOVE WS-STREET-CNT TO TL-VALUE.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
           MOVE 'OLDEST FAIL (BUSINESS DAYS)' TO TL-LABEL.
           MOVE WS-OLDEST-DAYS TO TL-VALUE.
           MOVE WS-TOT-MV TO TL-AMOUNT.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-FAIL.
      *================================================================*
           READ FAILIN-FILE.
           EVALUATE TRUE
               WHEN FAILIN-OK
                   CONTINUE
               WHEN FAILIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'FAILIN' TO AB-DDNAME
                   MOVE WS-FAILIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-WRITE-DETAIL.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 5 TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE.
      *----------------------------------------------------------------*
           MOVE WS-DI-CCYY TO WS-DE-CCYY.
           MOVE WS-DI-MM   TO WS-DE-MM.
           MOVE WS-DI-DD   TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE FAILIN-FILE RPTFILE.
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRR260'       TO CT-STAGE.
           MOVE 'FAILS-IN'     TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT    TO CT-COUNT.
           MOVE WS-TOT-AMT     TO CT-AMOUNT.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'FAILS-PRINTED' TO CT-COUNTER-NAME.
           MOVE WS-PRINT-CNT   TO CT-COUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR260 FAIL RECORDS READ   : ' WS-READ-CNT.
           DISPLAY 'SRR260 FAIL LINES PRINTED  : ' WS-PRINT-CNT.
           DISPLAY 'SRR260 STREET LEGS SKIPPED : ' WS-STREET-CNT.
           DISPLAY 'SRR260 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE 'FAILS AGING REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SRR260 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
