       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR310.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  APRIL 2015.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR310                                            *
      * DESCRIPTION: DAILY CASH ACTIVITY REPORT.                       *
      *              ONE LINE PER CASH MOVEMENT POSTED BY SRB300,      *
      *              GROUPED BY ACCOUNT AND CURRENCY, WITH TRADE-DATE  *
      *              AND SETTLE-DATE MOVEMENT AND CLOSING BALANCES.    *
      *              GRAND TOTALS BY CURRENCY.                         *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD040 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              CASHIN   - MSEC.PROD.SR.CASHACT.SORTED(+1)        *
      *                         (SRCSHA, BY ACCOUNT/CCY/BASIS)         *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2015-04-20 SPA  ORIGINAL - REPLACES CSH-LISTING SAS   CHG28004 *
      * 2019-02-25 MHC  CONTROL TOTALS                        CHG33410 *
      * 2024-05-20 NVR  T+1 - SETTLE DATE COLUMN              CHG40551 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CASHIN-FILE    ASSIGN TO CASHIN
                  FILE STATUS IS WS-CASHIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  CASHIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CASHIN-REC                  PIC X(150).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR310'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-CASHIN-STATUS        PIC X(02)  VALUE '00'.
               88  CASHIN-OK                      VALUE '00'.
               88  CASHIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-CASH                    VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-RECORD                   VALUE 'Y'.
       01  WS-GROUP-KEY.
           05  WS-GK-ACCT              PIC X(10).
           05  WS-GK-CCY               PIC X(03).
       01  WS-PREV-GROUP-KEY           PIC X(13)  VALUE LOW-VALUES.
       01  WS-GROUP-TOTALS.
           05  WS-GT-COUNT             PIC S9(07)       COMP-3.
           05  WS-GT-TD-MOVE           PIC S9(15)V99    COMP-3.
           05  WS-GT-SD-MOVE           PIC S9(15)V99    COMP-3.
           05  WS-GT-CLOSE-TD          PIC S9(15)V99    COMP-3.
           05  WS-GT-CLOSE-SD          PIC S9(15)V99    COMP-3.
       01  WS-CCY-TOTALS.
           05  WS-CCY-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-CCY-ENTRY OCCURS 20 TIMES INDEXED BY CX.
               10  WS-CX-CCY           PIC X(03).
               10  WS-CX-COUNT         PIC S9(07)       COMP-3.
               10  WS-CX-ACCTS         PIC S9(07)       COMP-3.
               10  WS-CX-TD-MOVE       PIC S9(15)V99    COMP-3.
               10  WS-CX-SD-MOVE       PIC S9(15)V99    COMP-3.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GROUP-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AMT-HASH             PIC S9(15)V99    COMP-3
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
           05  FILLER  PIC X(12)  VALUE ' ACCOUNT'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(03)  VALUE 'BS'.
           05  FILLER  PIC X(03)  VALUE 'SR'.
           05  FILLER  PIC X(17)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(02)  VALUE 'L'.
           05  FILLER  PIC X(04)  VALUE 'TYP'.
           05  FILLER  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER  PIC X(11)  VALUE 'SETTLE DT'.
           05  FILLER  PIC X(20)  VALUE '             AMOUNT'.
           05  FILLER  PIC X(20)  VALUE '     TD BAL AFTER'.
           05  FILLER  PIC X(20)  VALUE '     SD BAL AFTER'.
           05  FILLER  PIC X(06)  VALUE 'N'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(12)  VALUE ' ----------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(02)  VALUE '-'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(20)  VALUE '-------------------'.
           05  FILLER  PIC X(20)  VALUE '-------------------'.
           05  FILLER  PIC X(20)  VALUE '-------------------'.
           05  FILLER  PIC X(06)  VALUE '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-BASIS                PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-SOURCE               PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-LEG                  PIC 9(01).
           05  FILLER                  PIC X(01).
           05  DL-TYPE                 PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  DL-SETTLE               PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-TD-AFTER             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-SD-AFTER             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-NEW                  PIC X(01).
           05  FILLER                  PIC X(05).
       01  WS-GROUP-LINE.
           05  GL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  FILLER                  PIC X(08)  VALUE 'ACCOUNT '.
           05  GL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  GL-CCY                  PIC X(03).
           05  FILLER                  PIC X(05)  VALUE ' MOV '.
           05  GL-COUNT                PIC ZZZ9.
           05  FILLER                  PIC X(05)  VALUE '  TD '.
           05  GL-TD-MOVE              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(05)  VALUE '  SD '.
           05  GL-SD-MOVE              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(10)  VALUE ' CLOSE TD '.
           05  GL-CLOSE-TD             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04)  VALUE ' SD '.
           05  GL-CLOSE-SD             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-CCY-HEAD.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(132) VALUE
               ' CURRENCY TOTALS'.
       01  WS-CCY-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  CL-CCY                  PIC X(03).
           05  FILLER                  PIC X(12)  VALUE '  ACCOUNTS: '.
           05  CL-ACCTS                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(13)  VALUE '  MOVEMENTS: '.
           05  CL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(06)  VALUE '   TD '.
           05  CL-TD-MOVE              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(06)  VALUE '   SD '.
           05  CL-SD-MOVE              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(35).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO CASH ACTIVITY FOR THIS BUSINESS DATE ***'.
       COPY SRCSHA.
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
           PERFORM 2000-PROCESS-RECORD UNTIL END-OF-CASH.
           IF NOT FIRST-RECORD
               PERFORM 3000-GROUP-BREAK
           END-IF.
           PERFORM 4000-CURRENCY-TOTALS.
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
           MOVE 'CASH ACTIVITY REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT CASHIN-FILE.
           IF WS-CASHIN-STATUS NOT = '00'
               MOVE 'CASHIN' TO AB-DDNAME
               MOVE WS-CASHIN-STATUS TO AB-FILE-STATUS
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
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'DAILY CASH ACTIVITY BY ACCOUNT AND CURRENCY'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           INITIALIZE WS-GROUP-TOTALS.
           PERFORM 8000-READ-CASH.
           IF END-OF-CASH
               PERFORM 8200-HEADINGS
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
           END-IF.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           ADD CSA-AMOUNT TO WS-AMT-HASH.
           MOVE CSA-ACCT-NO TO WS-GK-ACCT.
           MOVE CSA-CCY     TO WS-GK-CCY.
           EVALUATE TRUE
               WHEN FIRST-RECORD
                   MOVE 'N' TO WS-FIRST-SW
                   MOVE WS-GROUP-KEY TO WS-PREV-GROUP-KEY
               WHEN WS-GROUP-KEY NOT = WS-PREV-GROUP-KEY
                   PERFORM 3000-GROUP-BREAK
                   MOVE WS-GROUP-KEY TO WS-PREV-GROUP-KEY
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
           ADD 1 TO WS-GT-COUNT.
           IF CSA-TRADE-DATE-CASH
               ADD CSA-AMOUNT TO WS-GT-TD-MOVE
               IF CSA-AFTER-SD-BAL NOT = CSA-BEFORE-SD-BAL
                   ADD CSA-AMOUNT TO WS-GT-SD-MOVE
               END-IF
           ELSE
               ADD CSA-AMOUNT TO WS-GT-SD-MOVE
           END-IF.
           MOVE CSA-AFTER-TD-BAL TO WS-GT-CLOSE-TD.
           MOVE CSA-AFTER-SD-BAL TO WS-GT-CLOSE-SD.
           PERFORM 2100-PRINT-DETAIL.
           PERFORM 8000-READ-CASH.
      *----------------------------------------------------------------*
       2100-PRINT-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES           TO WS-DETAIL-LINE.
           MOVE ' '              TO DL-CC.
           MOVE CSA-ACCT-NO      TO DL-ACCT.
           MOVE CSA-CCY          TO DL-CCY.
           EVALUATE TRUE
               WHEN CSA-TRADE-DATE-CASH   MOVE 'TD' TO DL-BASIS
               WHEN CSA-SETTLE-DATE-CASH  MOVE 'SD' TO DL-BASIS
               WHEN OTHER                 MOVE '??' TO DL-BASIS
           END-EVALUATE.
           MOVE CSA-SOURCE       TO DL-SOURCE.
           MOVE CSA-REF          TO DL-REF.
           MOVE CSA-LEG-NO       TO DL-LEG.
           MOVE CSA-ACT-TYPE     TO DL-TYPE.
           MOVE CSA-CUSIP        TO DL-CUSIP.
           IF CSA-SETTLE-DATE NUMERIC AND CSA-SETTLE-DATE > ZERO
               MOVE CSA-SETTLE-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE
               MOVE WS-DATE-EDIT TO DL-SETTLE
           END-IF.
           MOVE CSA-AMOUNT       TO DL-AMOUNT.
           MOVE CSA-AFTER-TD-BAL TO DL-TD-AFTER.
           MOVE CSA-AFTER-SD-BAL TO DL-SD-AFTER.
           IF CSA-NEW-BALANCE
               MOVE '*' TO DL-NEW
           END-IF.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *================================================================*
       3000-GROUP-BREAK.
      *================================================================*
           ADD 1 TO WS-GROUP-CNT.
           MOVE ' '                      TO GL-CC.
           MOVE WS-PREV-GROUP-KEY (1:10) TO GL-ACCT.
           MOVE WS-PREV-GROUP-KEY (11:3) TO GL-CCY.
           MOVE WS-GT-COUNT              TO GL-COUNT.
           MOVE WS-GT-TD-MOVE            TO GL-TD-MOVE.
           MOVE WS-GT-SD-MOVE            TO GL-SD-MOVE.
           MOVE WS-GT-CLOSE-TD           TO GL-CLOSE-TD.
           MOVE WS-GT-CLOSE-SD           TO GL-CLOSE-SD.
           IF RPT-LINE-COUNT + 2 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-GROUP-LINE.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-BLANK-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
           PERFORM 3100-ADD-CURRENCY.
           INITIALIZE WS-GROUP-TOTALS.
      *----------------------------------------------------------------*
       3100-ADD-CURRENCY.
      *----------------------------------------------------------------*
           SET CX TO 1.
           SEARCH WS-CCY-ENTRY
               AT END
                   DISPLAY 'SRR310 CURRENCY TABLE FULL'
               WHEN CX > WS-CCY-USED
                   ADD 1 TO WS-CCY-USED
                   MOVE WS-PREV-GROUP-KEY (11:3) TO WS-CX-CCY (CX)
                   MOVE ZERO TO WS-CX-COUNT (CX) WS-CX-ACCTS (CX)
                                WS-CX-TD-MOVE (CX) WS-CX-SD-MOVE (CX)
                   PERFORM 3110-ACCUM-CURRENCY
               WHEN WS-CX-CCY (CX) = WS-PREV-GROUP-KEY (11:3)
                   PERFORM 3110-ACCUM-CURRENCY
           END-SEARCH.
      *----------------------------------------------------------------*
       3110-ACCUM-CURRENCY.
      *----------------------------------------------------------------*
           ADD 1             TO WS-CX-ACCTS (CX).
           ADD WS-GT-COUNT   TO WS-CX-COUNT (CX).
           ADD WS-GT-TD-MOVE TO WS-CX-TD-MOVE (CX).
           ADD WS-GT-SD-MOVE TO WS-CX-SD-MOVE (CX).
      *================================================================*
       4000-CURRENCY-TOTALS.
      *================================================================*
           IF RPT-LINE-COUNT + WS-CCY-USED + 5 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-CCY-HEAD.
           PERFORM 8900-CHECK-WRITE.
           ADD 3 TO RPT-LINE-COUNT.
           PERFORM VARYING CX FROM 1 BY 1 UNTIL CX > WS-CCY-USED
               MOVE ' '                TO CL-CC
               MOVE WS-CX-CCY (CX)     TO CL-CCY
               MOVE WS-CX-ACCTS (CX)   TO CL-ACCTS
               MOVE WS-CX-COUNT (CX)   TO CL-COUNT
               MOVE WS-CX-TD-MOVE (CX) TO CL-TD-MOVE
               MOVE WS-CX-SD-MOVE (CX) TO CL-SD-MOVE
               WRITE RPT-RECORD FROM WS-CCY-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
           END-PERFORM.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
       8000-READ-CASH.
      *================================================================*
           READ CASHIN-FILE INTO CSA-CASH-ACTV-REC.
           EVALUATE TRUE
               WHEN CASHIN-OK
                   CONTINUE
               WHEN CASHIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'CASHIN' TO AB-DDNAME
                   MOVE WS-CASHIN-STATUS TO AB-FILE-STATUS
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
           CLOSE CASHIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'SRR310'        TO CT-STAGE.
           MOVE 'CASHACT-IN'    TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-AMT-HASH     TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR310 CASH ACTIVITY RECORDS : ' WS-READ-CNT.
           DISPLAY 'SRR310 ACCOUNT/CCY GROUPS    : ' WS-GROUP-CNT.
           DISPLAY 'SRR310 PAGES                 : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'CASH ACTIVITY REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SRR310 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
