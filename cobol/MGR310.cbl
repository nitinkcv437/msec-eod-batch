       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGR310.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  DECEMBER 2009.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGR310                                            *
      * DESCRIPTION: MARGIN INTEREST REPORT.                           *
      *              PART 1  DAILY ACCRUAL BY ACCOUNT (MG.INTACCR)     *
      *              PART 2  SUMMARY BY SPREAD TIER                    *
      *              PART 3  MONTH-END CHARGE - GL LINES (MG.INTCHG)   *
      *                      WITH THE DEBIT = CREDIT PROOF.  ON A      *
      *                      DAILY CYCLE THE GL FILE IS EMPTY AND      *
      *                      PART 3 SAYS SO.                           *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD030 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              ACCRIN   - MSEC.PROD.MG.INTACCR(+1)        (MGINT)*
      *              GLIN     - MSEC.PROD.MG.INTCHG(+1)       (SRGLJNL)*
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  MONTH-END GL LINES OUT OF BALANCE              *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2009-12-14 SPA  ORIGINAL - REPLACES MGB300 SYSOUT     CHG19002 *
      * 2011-06-20 SPA  CONTROL TOTALS                        CHG21877 *
      * 2017-02-13 MHC  WAIVED CHARGES FLAGGED                CHG31022 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT ACCRIN-FILE    ASSIGN TO ACCRIN
                  FILE STATUS IS WS-ACCRIN-STATUS.
           SELECT GLIN-FILE      ASSIGN TO GLIN
                  FILE STATUS IS WS-GLIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  ACCRIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACCRIN-REC                  PIC X(120).
       FD  GLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLIN-REC                    PIC X(150).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGR310'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-ACCRIN-STATUS        PIC X(02)  VALUE '00'.
               88  ACCRIN-OK                      VALUE '00'.
               88  ACCRIN-EOF                     VALUE '10'.
           05  WS-GLIN-STATUS          PIC X(02)  VALUE '00'.
               88  GLIN-OK                        VALUE '00'.
               88  GLIN-EOF                       VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-ACCR-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCRUALS                VALUE 'Y'.
           05  WS-GL-EOF-SW            PIC X(01)  VALUE 'N'.
               88  END-OF-GL                      VALUE 'Y'.
           05  WS-PART-SW              PIC X(01)  VALUE '1'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-TIER-TOTALS.
           05  WS-TT OCCURS 5 TIMES.
               10  WS-TT-COUNT         PIC S9(07)       COMP-3.
               10  WS-TT-DEBIT         PIC S9(15)V99    COMP-3.
               10  WS-TT-INTEREST      PIC S9(13)V99    COMP-3.
               10  WS-TT-RATE          PIC S9(03)V9(06) COMP-3.
       01  WS-SUB                      PIC S9(04) COMP  VALUE ZERO.
       01  WS-TOTALS.
           05  WS-ACCR-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-DEBIT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-DAILY            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-MTD              PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-WAIVED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GL-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GL-DR                PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GL-CR                PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GL-DIFF              PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GL-ACCTS             PIC S9(09) COMP-3 VALUE ZERO.
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
      * REPORT LINES - PART 1                                          *
      *----------------------------------------------------------------*
       01  WS-PART-LINE.
           05  PL-CC                   PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  PL-TEXT                 PIC X(60).
           05  FILLER                  PIC X(71)  VALUE SPACES.
       01  WS-ACCR-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(12)  VALUE 'ACCOUNT'.
           05  FILLER  PIC X(20)  VALUE '       DEBIT BALANCE'.
           05  FILLER  PIC X(12)  VALUE '   BASE RATE'.
           05  FILLER  PIC X(12)  VALUE '      SPREAD'.
           05  FILLER  PIC X(12)  VALUE '   EFF. RATE'.
           05  FILLER  PIC X(06)  VALUE '  TIER'.
           05  FILLER  PIC X(06)  VALUE '  DAYS'.
           05  FILLER  PIC X(17)  VALUE '   INTEREST TODAY'.
           05  FILLER  PIC X(17)  VALUE '    MONTH TO DATE'.
           05  FILLER  PIC X(08)  VALUE '  POSTED'.
           05  FILLER  PIC X(10)  VALUE SPACES.
       01  WS-ACCR-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(12)  VALUE '----------'.
           05  FILLER  PIC X(20)  VALUE '   -----------------'.
           05  FILLER  PIC X(12)  VALUE '  ----------'.
           05  FILLER  PIC X(12)  VALUE '  ----------'.
           05  FILLER  PIC X(12)  VALUE '  ----------'.
           05  FILLER  PIC X(06)  VALUE '  ----'.
           05  FILLER  PIC X(06)  VALUE '  ----'.
           05  FILLER  PIC X(17)  VALUE '   --------------'.
           05  FILLER  PIC X(17)  VALUE '   --------------'.
           05  FILLER  PIC X(08)  VALUE '  ------'.
           05  FILLER  PIC X(10)  VALUE SPACES.
       01  WS-ACCR-LINE.
           05  AL-CC                   PIC X(01).
           05  AL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(02).
           05  AL-DEBIT                PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  AL-BASE                 PIC ZZ9.999999.
           05  FILLER                  PIC X(02).
           05  AL-SPREAD               PIC ZZ9.999999.
           05  FILLER                  PIC X(02).
           05  AL-RATE                 PIC ZZ9.999999.
           05  FILLER                  PIC X(05).
           05  AL-TIER                 PIC 9.
           05  FILLER                  PIC X(03).
           05  AL-DAYS                 PIC ZZ9.
           05  FILLER                  PIC X(03).
           05  AL-DAILY                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  AL-MTD                  PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(05).
           05  AL-POSTED               PIC X(01).
           05  FILLER                  PIC X(14).
       01  WS-ACCR-TOTAL-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE 'TOTAL'.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  ATL-DEBIT               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(07)  VALUE SPACES.
           05  FILLER                  PIC X(10)  VALUE 'ACCOUNTS: '.
           05  ATL-COUNT               PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(18)  VALUE SPACES.
           05  ATL-DAILY               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  ATL-MTD                 PIC ZZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(29)  VALUE SPACES.
      *----------------------------------------------------------------*
      * PART 2 - TIERS                                                 *
      *----------------------------------------------------------------*
       01  WS-TIER-HEAD.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(53)  VALUE
               '   TIER   ACCOUNTS          DEBIT BALANCE    RATE (LA'.
           05  FILLER                  PIC X(79)  VALUE
               'ST)     INTEREST TODAY'.
       01  WS-TIER-LINE.
           05  TR-CC                   PIC X(01).
           05  FILLER                  PIC X(06).
           05  TR-TIER                 PIC 9.
           05  FILLER                  PIC X(04).
           05  TR-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(04).
           05  TR-DEBIT                PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(05).
           05  TR-RATE                 PIC ZZ9.999999.
           05  FILLER                  PIC X(04).
           05  TR-INTEREST             PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(59).
      *----------------------------------------------------------------*
      * PART 3 - GL                                                    *
      *----------------------------------------------------------------*
       01  WS-GL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(18)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(05)  VALUE 'LINE'.
           05  FILLER  PIC X(06)  VALUE 'TXN'.
           05  FILLER  PIC X(12)  VALUE 'GL ACCOUNT'.
           05  FILLER  PIC X(08)  VALUE 'CC'.
           05  FILLER  PIC X(04)  VALUE 'D/C'.
           05  FILLER  PIC X(20)  VALUE '              AMOUNT'.
           05  FILLER  PIC X(04)  VALUE ' CCY'.
           05  FILLER  PIC X(13)  VALUE '  ACCOUNT'.
           05  FILLER  PIC X(42)  VALUE 'DESCRIPTION'.
       01  WS-GL-LINE.
           05  GL-CC                   PIC X(01).
           05  GL-REF                  PIC X(16).
           05  FILLER                  PIC X(02).
           05  GL-LINE-NO              PIC ZZ9.
           05  FILLER                  PIC X(02).
           05  GL-TXN                  PIC X(04).
           05  FILLER                  PIC X(02).
           05  GL-ACCOUNT              PIC X(10).
           05  FILLER                  PIC X(02).
           05  GL-COST-CTR             PIC X(06).
           05  FILLER                  PIC X(03).
           05  GL-DR-CR                PIC X(01).
           05  FILLER                  PIC X(02).
           05  GL-AMOUNT               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  GL-CCY                  PIC X(03).
           05  FILLER                  PIC X(02).
           05  GL-ACCT-NO              PIC X(10).
           05  FILLER                  PIC X(03).
           05  GL-DESC                 PIC X(30).
           05  FILLER                  PIC X(11).
       01  WS-GL-PROOF-LINE.
           05  GP-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  GP-LABEL                PIC X(30).
           05  GP-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  GP-TEXT                 PIC X(40).
           05  FILLER                  PIC X(36)  VALUE SPACES.
       01  WS-NO-GL-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NOT A MONTH-END CYCLE - NO INTEREST CHARGED ***'.
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO DEBIT BALANCES ACCRUED TODAY ***'.
       COPY MGINT.
       COPY SRGLJNL.
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
           PERFORM 2000-ACCRUAL-DETAIL UNTIL END-OF-ACCRUALS.
           PERFORM 2500-ACCRUAL-TOTAL.
           PERFORM 3000-TIER-SUMMARY.
           PERFORM 4000-MONTH-END-GL.
           PERFORM 9000-TERMINATE.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
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
           MOVE 'MARGIN INTEREST REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT ACCRIN-FILE.
           IF WS-ACCRIN-STATUS NOT = '00'
               MOVE 'ACCRIN' TO AB-DDNAME
               MOVE WS-ACCRIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT GLIN-FILE.
           IF WS-GLIN-STATUS NOT = '00'
               MOVE 'GLIN' TO AB-DDNAME
               MOVE WS-GLIN-STATUS TO AB-FILE-STATUS
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
           INITIALIZE WS-TIER-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'MARGIN INTEREST - DAILY ACCRUAL / MONTH-END CHARGE'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           MOVE '1' TO WS-PART-SW.
           PERFORM 8200-HEADINGS.
           MOVE 'PART 1 - DAILY ACCRUAL BY ACCOUNT' TO PL-TEXT.
           PERFORM 8250-PART-LINE.
           PERFORM 8000-READ-ACCRUAL.
           IF END-OF-ACCRUALS
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
      *================================================================*
      * PART 1                                                         *
      *================================================================*
       2000-ACCRUAL-DETAIL.
           ADD 1 TO WS-ACCR-CNT.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES               TO WS-ACCR-LINE.
           MOVE ' '                  TO AL-CC.
           MOVE MGI-ACCT-NO          TO AL-ACCT.
           MOVE MGI-DEBIT-BALANCE    TO AL-DEBIT.
           MOVE MGI-BASE-RATE        TO AL-BASE.
           MOVE MGI-SPREAD           TO AL-SPREAD.
           MOVE MGI-EFFECTIVE-RATE   TO AL-RATE.
           MOVE MGI-TIER             TO AL-TIER.
           MOVE MGI-DAYS             TO AL-DAYS.
           MOVE MGI-DAILY-INTEREST   TO AL-DAILY.
           MOVE MGI-MTD-INTEREST     TO AL-MTD.
           MOVE MGI-POSTED-FLAG      TO AL-POSTED.
           WRITE RPT-RECORD FROM WS-ACCR-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
           ADD MGI-DEBIT-BALANCE  TO WS-TOT-DEBIT.
           ADD MGI-DAILY-INTEREST TO WS-TOT-DAILY.
           ADD MGI-MTD-INTEREST   TO WS-TOT-MTD.
           IF MGI-POSTED-FLAG = 'W'
               ADD 1 TO WS-WAIVED-CNT
           END-IF.
           IF MGI-TIER NUMERIC AND MGI-TIER > ZERO AND MGI-TIER < 6
               MOVE MGI-TIER TO WS-SUB
               ADD 1                  TO WS-TT-COUNT (WS-SUB)
               ADD MGI-DEBIT-BALANCE  TO WS-TT-DEBIT (WS-SUB)
               ADD MGI-DAILY-INTEREST TO WS-TT-INTEREST (WS-SUB)
               MOVE MGI-EFFECTIVE-RATE TO WS-TT-RATE (WS-SUB)
           END-IF.
           PERFORM 8000-READ-ACCRUAL.
      *----------------------------------------------------------------*
       2500-ACCRUAL-TOTAL.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT + 2 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE WS-TOT-DEBIT  TO ATL-DEBIT.
           MOVE WS-ACCR-CNT   TO ATL-COUNT.
           MOVE WS-TOT-DAILY  TO ATL-DAILY.
           MOVE WS-TOT-MTD    TO ATL-MTD.
           WRITE RPT-RECORD FROM WS-ACCR-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
      *================================================================*
      * PART 2                                                         *
      *================================================================*
       3000-TIER-SUMMARY.
           IF RPT-LINE-COUNT + 12 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE 'PART 2 - SUMMARY BY SPREAD TIER' TO PL-TEXT.
           PERFORM 8250-PART-LINE.
           WRITE RPT-RECORD FROM WS-TIER-HEAD.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
               MOVE SPACES              TO WS-TIER-LINE
               MOVE ' '                 TO TR-CC
               MOVE WS-SUB              TO TR-TIER
               MOVE WS-TT-COUNT (WS-SUB)    TO TR-COUNT
               MOVE WS-TT-DEBIT (WS-SUB)    TO TR-DEBIT
               MOVE WS-TT-RATE (WS-SUB)     TO TR-RATE
               MOVE WS-TT-INTEREST (WS-SUB) TO TR-INTEREST
               WRITE RPT-RECORD FROM WS-TIER-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
           END-PERFORM.
      *================================================================*
      * PART 3 - MONTH-END GL LINES AND PROOF                          *
      *================================================================*
       4000-MONTH-END-GL.
           MOVE '3' TO WS-PART-SW.
           PERFORM 8200-HEADINGS.
           MOVE 'PART 3 - MONTH-END INTEREST CHARGE (GL TXN MINT)'
                                   TO PL-TEXT.
           PERFORM 8250-PART-LINE.
           PERFORM 8100-READ-GL.
           IF END-OF-GL
               WRITE RPT-RECORD FROM WS-NO-GL-LINE
               PERFORM 8900-CHECK-WRITE
           ELSE
               PERFORM 4100-GL-DETAIL UNTIL END-OF-GL
               PERFORM 4200-GL-PROOF
           END-IF.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *----------------------------------------------------------------*
       4100-GL-DETAIL.
      *----------------------------------------------------------------*
           ADD 1 TO WS-GL-CNT.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES              TO WS-GL-LINE.
           MOVE ' '                 TO GL-CC.
           MOVE GLJ-REF             TO GL-REF.
           MOVE GLJ-LINE-NO         TO GL-LINE-NO.
           MOVE GLJ-TXN-CODE        TO GL-TXN.
           MOVE GLJ-GL-ACCOUNT      TO GL-ACCOUNT.
           MOVE GLJ-COST-CENTER     TO GL-COST-CTR.
           MOVE GLJ-DR-CR           TO GL-DR-CR.
           MOVE GLJ-AMOUNT          TO GL-AMOUNT.
           MOVE GLJ-CCY             TO GL-CCY.
           MOVE GLJ-ACCT-NO         TO GL-ACCT-NO.
           MOVE GLJ-DESC            TO GL-DESC.
           WRITE RPT-RECORD FROM WS-GL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
           EVALUATE TRUE
               WHEN GLJ-DEBIT
                   ADD GLJ-AMOUNT-USD TO WS-GL-DR
                   ADD 1 TO WS-GL-ACCTS
               WHEN GLJ-CREDIT
                   ADD GLJ-AMOUNT-USD TO WS-GL-CR
               WHEN OTHER
                   DISPLAY 'MGR310 GL LINE WITHOUT D/C ' GLJ-REF
                   MOVE 4 TO WS-RETURN-CODE
           END-EVALUATE.
           PERFORM 8100-READ-GL.
      *----------------------------------------------------------------*
       4200-GL-PROOF.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT + 8 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE 'TOTAL DEBITS  (USD)'   TO GP-LABEL.
           MOVE WS-GL-DR                TO GP-AMOUNT.
           MOVE SPACES                  TO GP-TEXT.
           WRITE RPT-RECORD FROM WS-GL-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE 'TOTAL CREDITS (USD)'   TO GP-LABEL.
           MOVE WS-GL-CR                TO GP-AMOUNT.
           WRITE RPT-RECORD FROM WS-GL-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE.
           COMPUTE WS-GL-DIFF = WS-GL-DR - WS-GL-CR.
           MOVE 'DIFFERENCE'            TO GP-LABEL.
           MOVE WS-GL-DIFF              TO GP-AMOUNT.
           IF WS-GL-DIFF = ZERO
               MOVE 'DEBITS = CREDITS - IN BALANCE' TO GP-TEXT
           ELSE
               MOVE '*** OUT OF BALANCE - DO NOT RELEASE ***'
                                        TO GP-TEXT
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           WRITE RPT-RECORD FROM WS-GL-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE 'ACCOUNTS CHARGED'      TO GP-LABEL.
           MOVE WS-GL-ACCTS             TO GP-AMOUNT.
           MOVE SPACES                  TO GP-TEXT.
           WRITE RPT-RECORD FROM WS-GL-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 8 TO RPT-LINE-COUNT.
      *================================================================*
       8000-READ-ACCRUAL.
      *================================================================*
           READ ACCRIN-FILE INTO MGI-ACCRUAL-REC.
           EVALUATE TRUE
               WHEN ACCRIN-OK
                   CONTINUE
               WHEN ACCRIN-EOF
                   MOVE 'Y' TO WS-ACCR-EOF-SW
               WHEN OTHER
                   MOVE 'ACCRIN' TO AB-DDNAME
                   MOVE WS-ACCRIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8100-READ-GL.
      *----------------------------------------------------------------*
           READ GLIN-FILE INTO GLJ-JOURNAL-REC.
           EVALUATE TRUE
               WHEN GLIN-OK
                   CONTINUE
               WHEN GLIN-EOF
                   MOVE 'Y' TO WS-GL-EOF-SW
               WHEN OTHER
                   MOVE 'GLIN' TO AB-DDNAME
                   MOVE WS-GLIN-STATUS TO AB-FILE-STATUS
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
           IF WS-PART-SW = '3'
               WRITE RPT-RECORD FROM WS-GL-HEAD-1
               PERFORM 8900-CHECK-WRITE
               MOVE 4 TO RPT-LINE-COUNT
           ELSE
               WRITE RPT-RECORD FROM WS-ACCR-HEAD-1
               PERFORM 8900-CHECK-WRITE
               WRITE RPT-RECORD FROM WS-ACCR-HEAD-2
               PERFORM 8900-CHECK-WRITE
               MOVE 5 TO RPT-LINE-COUNT
           END-IF.
      *----------------------------------------------------------------*
       8250-PART-LINE.
      *----------------------------------------------------------------*
           WRITE RPT-RECORD FROM WS-PART-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 3 TO RPT-LINE-COUNT.
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
           CLOSE ACCRIN-FILE GLIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'MGR310'        TO CT-STAGE.
           MOVE 'ACCRUAL-IN'    TO CT-COUNTER-NAME.
           MOVE WS-ACCR-CNT     TO CT-COUNT.
           MOVE WS-TOT-DAILY    TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE 'GLJ-IN'        TO CT-COUNTER-NAME.
           MOVE WS-GL-CNT       TO CT-COUNT.
           MOVE WS-GL-DR        TO CT-AMOUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'MGR310 ACCRUALS READ       : ' WS-ACCR-CNT.
           DISPLAY 'MGR310 WAIVED (FLAG W)     : ' WS-WAIVED-CNT.
           DISPLAY 'MGR310 GL LINES READ       : ' WS-GL-CNT.
           DISPLAY 'MGR310 GL DEBITS           : ' WS-GL-DR.
           DISPLAY 'MGR310 GL CREDITS          : ' WS-GL-CR.
           DISPLAY 'MGR310 PAGES               : ' RPT-PAGE-COUNT.
           DISPLAY 'MGR310 RETURN CODE         : ' WS-RETURN-CODE.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'MARGIN INTEREST REPORT ENDED' TO AU-MESSAGE.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'MGR310 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
