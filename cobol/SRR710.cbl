       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR710.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  FEBRUARY 1990.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR710                                            *
      * DESCRIPTION: CLIENT STATEMENT PRINT.                           *
      *              PRINTS ONE STATEMENT PER ACCOUNT FROM THE SORTED  *
      *              STATEMENT EXTRACT.  EVERY STATEMENT STARTS ON A   *
      *              NEW PAGE WITH THE NAME AND ADDRESS BLOCK; LONG    *
      *              STATEMENTS CONTINUE ON FOLLOWING PAGES WITH A     *
      *              CONTINUATION HEADER.  SECTIONS:                   *
      *                SECURITY POSITIONS                              *
      *                CASH BALANCES (BY CURRENCY)                     *
      *                ACCOUNT SUMMARY                                 *
      *              OUTPUT GOES TO THE PRINT VENDOR TRANSMISSION.     *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRM010 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              STMTIN   - MSEC.PROD.SR.STMTEXT.SORTED(+1)        *
      *                         (SRSTMT BY ACCOUNT / SEQUENCE)         *
      * OUTPUT     : RPTFILE  - STATEMENTS, FB 133 ASA                 *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0, 4 IF A STATEMENT HAS NO TOTAL RECORD           *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1990-02-12 RJK  ORIGINAL                                       *
      * 1993-07-19 DWB  CONTINUATION HEADER ON OVERFLOW       CHG01655 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-07-16 KAP  DECIMALIZATION - 4 DECIMAL PRICES     CHG08811 *
      * 2009-12-14 SPA  MULTI-CURRENCY CASH SECTION           CHG19002 *
      * 2014-06-30 SPA  ESTIMATED INCOME COLUMN, DISCLOSURES  CHG26011 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT STMTIN-FILE    ASSIGN TO STMTIN
                  FILE STATUS IS WS-STMTIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  STMTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTIN-REC                  PIC X(200).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR710'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-STMTIN-STATUS        PIC X(02)  VALUE '00'.
               88  STMTIN-OK                      VALUE '00'.
               88  STMTIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-STATEMENTS              VALUE 'Y'.
           05  WS-IN-STMT-SW           PIC X(01)  VALUE 'N'.
               88  STATEMENT-OPEN                 VALUE 'Y'.
           05  WS-SECTION-SW           PIC X(01)  VALUE SPACE.
               88  IN-POSITION-SECTION            VALUE 'P'.
               88  IN-CASH-SECTION                VALUE 'C'.
               88  NO-SECTION                     VALUE ' '.
           05  WS-TOTAL-SEEN-SW        PIC X(01)  VALUE 'N'.
               88  TOTAL-SEEN                     VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * SAVED ACCOUNT HEADER (REPRINTED ON CONTINUATION PAGES)         *
      *----------------------------------------------------------------*
       01  WS-SAVED-HEADER.
           05  WS-SH-ACCT              PIC X(10).
           05  WS-SH-NAME              PIC X(40).
           05  WS-SH-ADDR-1            PIC X(30).
           05  WS-SH-ADDR-2            PIC X(30).
           05  WS-SH-BRANCH            PIC X(03).
           05  WS-SH-REP               PIC X(04).
           05  WS-SH-CCY               PIC X(03).
           05  WS-SH-PERIOD            PIC 9(08).
       01  WS-STMT-PAGE                PIC S9(03) COMP-3 VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STMT-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POS-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-TOTAL-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ORPHAN-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOTAL-EQUITY         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-EDIT.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  WS-DE-DD                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  WS-DE-CCYY              PIC 9(04).
      *----------------------------------------------------------------*
      * STATEMENT LINES                                                *
      *----------------------------------------------------------------*
       01  WS-ADDR-LINE.
           05  AL-CC                   PIC X(01).
           05  FILLER                  PIC X(09).
           05  AL-TEXT                 PIC X(40).
           05  FILLER                  PIC X(30).
           05  AL-LABEL                PIC X(18).
           05  AL-VALUE                PIC X(20).
           05  FILLER                  PIC X(15).
       01  WS-SECTION-LINE.
           05  SC-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  SC-TEXT                 PIC X(60).
           05  FILLER                  PIC X(71).
       01  WS-POS-HEAD-1.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(11)  VALUE ' CUSIP'.
           05  FILLER  PIC X(31)  VALUE 'DESCRIPTION'.
           05  FILLER  PIC X(17)  VALUE '        QUANTITY'.
           05  FILLER  PIC X(13)  VALUE '       PRICE'.
           05  FILLER  PIC X(16)  VALUE '   MARKET VALUE'.
           05  FILLER  PIC X(16)  VALUE '     COST BASIS'.
           05  FILLER  PIC X(16)  VALUE ' UNREALIZED G/L'.
           05  FILLER  PIC X(12)  VALUE ' EST INCOME'.
       01  WS-POS-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(11)  VALUE ' ---------'.
           05  FILLER  PIC X(31)  VALUE
               '------------------------------'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(13)  VALUE '------------'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
           05  FILLER  PIC X(12)  VALUE '-----------'.
       01  WS-POS-LINE.
           05  PL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  PL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  PL-DESC                 PIC X(30).
           05  FILLER                  PIC X(01).
           05  PL-QTY                  PIC -ZZZ,ZZZ,ZZ9.999.
           05  FILLER                  PIC X(01).
           05  PL-PRICE                PIC ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  PL-MV                   PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  PL-COST                 PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  PL-UNRLZD               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  PL-INCOME               PIC -ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04).
       01  WS-CASH-HEAD.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(10)  VALUE ' CURRENCY'.
           05  FILLER  PIC X(22)  VALUE '             BALANCE'.
           05  FILLER  PIC X(22)  VALUE '   DIVIDENDS YTD'.
           05  FILLER  PIC X(22)  VALUE '  TAX WITHHELD YTD'.
           05  FILLER  PIC X(56)  VALUE SPACES.
       01  WS-CASH-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(04).
           05  CL-CCY                  PIC X(03).
           05  FILLER                  PIC X(03).
           05  CL-BALANCE              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  CL-INCOME               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  CL-WHT                  PIC -Z,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(61).
       01  WS-SUMMARY-LINE.
           05  SL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  SL-LABEL                PIC X(40).
           05  SL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(68).
       01  WS-DISCLOSURE-1.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(05)  VALUE SPACES.
           05  FILLER                  PIC X(48)  VALUE
               'MARKET VALUES ARE BASED ON CLOSING PRICES AS OF '.
           05  FILLER                  PIC X(79)  VALUE
               'THE STATEMENT DATE AND ARE STATED IN U.S. DOLLARS.'.
       01  WS-DISCLOSURE-2.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(05)  VALUE SPACES.
           05  FILLER                  PIC X(48)  VALUE
               'ESTIMATED INCOME IS AN ESTIMATE ONLY AND IS NOT '.
           05  FILLER                  PIC X(79)  VALUE
               'GUARANTEED.  PLEASE REVIEW THIS STATEMENT PROMPTLY.'.
       01  WS-DISCLOSURE-3.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(05)  VALUE SPACES.
           05  FILLER                  PIC X(127) VALUE
               'MERIDIAN SECURITIES LLC - MEMBER FINRA / SIPC'.
       COPY SRSTMT.
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
           PERFORM 2000-PROCESS-RECORD THRU 2000-EXIT
               UNTIL END-OF-STATEMENTS.
           IF STATEMENT-OPEN
               PERFORM 3900-END-STATEMENT THRU 3900-EXIT
           END-IF.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
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
           MOVE 'STATEMENT PRINT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT STMTIN-FILE.
           IF WS-STMTIN-STATUS NOT = '00'
               MOVE 'STMTIN' TO AB-DDNAME
               MOVE WS-STMTIN-STATUS TO AB-FILE-STATUS
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
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'CLIENT ACCOUNT STATEMENT' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-STMT THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           EVALUATE TRUE
               WHEN STM-ACCT-HEADER
                   IF STATEMENT-OPEN
                       PERFORM 3900-END-STATEMENT THRU 3900-EXIT
                   END-IF
                   PERFORM 3000-START-STATEMENT THRU 3000-EXIT
               WHEN NOT STATEMENT-OPEN
               WHEN STM-ACCT-NO NOT = WS-SH-ACCT
                   ADD 1 TO WS-ORPHAN-CNT
                   DISPLAY 'SRR710 RECORD WITHOUT HEADER - ACCOUNT '
                           STM-ACCT-NO ' TYPE ' STM-REC-TYPE
               WHEN STM-POSITION-LINE
                   PERFORM 3100-POSITION-LINE THRU 3100-EXIT
               WHEN STM-CASH-LINE
                   PERFORM 3200-CASH-LINE THRU 3200-EXIT
               WHEN STM-ACCT-TOTAL
                   PERFORM 3300-ACCOUNT-SUMMARY THRU 3300-EXIT
               WHEN OTHER
                   DISPLAY 'SRR710 UNKNOWN RECORD TYPE ' STM-REC-TYPE
           END-EVALUATE.
           PERFORM 8000-READ-STMT THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *================================================================*
      * NEW STATEMENT - NEW PAGE, NAME AND ADDRESS                     *
      *================================================================*
       3000-START-STATEMENT.
           MOVE 'Y' TO WS-IN-STMT-SW.
           MOVE 'N' TO WS-TOTAL-SEEN-SW.
           SET NO-SECTION TO TRUE.
           ADD 1 TO WS-STMT-CNT.
           MOVE ZERO TO WS-STMT-PAGE.
           MOVE STM-ACCT-NO     TO WS-SH-ACCT.
           MOVE STM-NAME        TO WS-SH-NAME.
           MOVE STM-ADDR-1      TO WS-SH-ADDR-1.
           MOVE STM-ADDR-2      TO WS-SH-ADDR-2.
           MOVE STM-BRANCH      TO WS-SH-BRANCH.
           MOVE STM-REP         TO WS-SH-REP.
           MOVE STM-BASE-CCY    TO WS-SH-CCY.
           MOVE STM-PERIOD-END  TO WS-SH-PERIOD.
           PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-POSITION-LINE.
      *----------------------------------------------------------------*
           IF NOT IN-POSITION-SECTION
               SET IN-POSITION-SECTION TO TRUE
               PERFORM 3150-POSITION-HEADINGS THRU 3150-EXIT
           END-IF.
           MOVE SPACES           TO WS-POS-LINE.
           MOVE ' '              TO PL-CC.
           MOVE STM-CUSIP        TO PL-CUSIP.
           MOVE STM-SEC-DESC     TO PL-DESC.
           MOVE STM-QTY          TO PL-QTY.
           MOVE STM-PRICE        TO PL-PRICE.
           MOVE STM-MKT-VALUE    TO PL-MV.
           MOVE STM-COST-BASIS   TO PL-COST.
           MOVE STM-UNRLZD-PL    TO PL-UNRLZD.
           MOVE STM-EST-INCOME   TO PL-INCOME.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT
               PERFORM 3150-POSITION-HEADINGS THRU 3150-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-POS-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT WS-POS-CNT.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3150-POSITION-HEADINGS.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT + 5 > RPT-LINES-PER-PAGE
               PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT
           END-IF.
           MOVE SPACES TO WS-SECTION-LINE.
           MOVE '0'    TO SC-CC.
           MOVE 'SECURITY POSITIONS' TO SC-TEXT.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-POS-HEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-POS-HEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 4 TO RPT-LINE-COUNT.
       3150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CASH-LINE.
      *----------------------------------------------------------------*
           IF NOT IN-CASH-SECTION
               SET IN-CASH-SECTION TO TRUE
               PERFORM 3250-CASH-HEADINGS THRU 3250-EXIT
           END-IF.
           MOVE SPACES              TO WS-CASH-LINE.
           MOVE ' '                 TO CL-CC.
           MOVE STM-CASH-CCY        TO CL-CCY.
           MOVE STM-CASH-BALANCE    TO CL-BALANCE.
           MOVE STM-CASH-INCOME-YTD TO CL-INCOME.
           MOVE STM-CASH-WHT-YTD    TO CL-WHT.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT
               PERFORM 3250-CASH-HEADINGS THRU 3250-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-CASH-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT WS-CASH-CNT.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3250-CASH-HEADINGS.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT + 4 > RPT-LINES-PER-PAGE
               PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT
           END-IF.
           MOVE SPACES TO WS-SECTION-LINE.
           MOVE '0'    TO SC-CC.
           MOVE 'CASH BALANCES' TO SC-TEXT.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-CASH-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
       3250-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-ACCOUNT-SUMMARY.
      *----------------------------------------------------------------*
           MOVE 'Y' TO WS-TOTAL-SEEN-SW.
           SET NO-SECTION TO TRUE.
           IF RPT-LINE-COUNT + 12 > RPT-LINES-PER-PAGE
               PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT
           END-IF.
           MOVE SPACES TO WS-SECTION-LINE.
           MOVE '0'    TO SC-CC.
           MOVE 'ACCOUNT SUMMARY (U.S. DOLLARS)' TO SC-TEXT.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-SUMMARY-LINE.
           MOVE '0'    TO SL-CC.
           MOVE 'TOTAL MARKET VALUE OF SECURITIES' TO SL-LABEL.
           MOVE STM-TOT-MKT-VALUE TO SL-AMOUNT.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-SUMMARY-LINE.
           MOVE ' '    TO SL-CC.
           MOVE 'TOTAL CASH' TO SL-LABEL.
           MOVE STM-TOT-CASH TO SL-AMOUNT.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-SUMMARY-LINE.
           MOVE '0'    TO SL-CC.
           MOVE 'TOTAL ACCOUNT VALUE' TO SL-LABEL.
           MOVE STM-TOT-EQUITY TO SL-AMOUNT.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-SUMMARY-LINE.
           MOVE ' '    TO SL-CC.
           MOVE 'NUMBER OF POSITIONS' TO SL-LABEL.
           MOVE STM-TOT-POSITIONS TO SL-AMOUNT.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 8 TO RPT-LINE-COUNT.
           ADD STM-TOT-EQUITY TO WS-TOTAL-EQUITY.
       3300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * END OF A STATEMENT - DISCLOSURES                               *
      *----------------------------------------------------------------*
       3900-END-STATEMENT.
           IF NOT TOTAL-SEEN
               ADD 1 TO WS-NO-TOTAL-CNT
               MOVE 4 TO WS-RETURN-CODE
               DISPLAY 'SRR710 STATEMENT WITHOUT TOTAL RECORD - '
                       WS-SH-ACCT
           END-IF.
           IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
               PERFORM 8200-STATEMENT-PAGE THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-DISCLOSURE-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-DISCLOSURE-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-DISCLOSURE-3.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 5 TO RPT-LINE-COUNT.
           MOVE 'N' TO WS-IN-STMT-SW.
       3900-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-STMT.
           READ STMTIN-FILE INTO STM-STATEMENT-REC.
           EVALUATE TRUE
               WHEN STMTIN-OK
                   CONTINUE
               WHEN STMTIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'STMTIN' TO AB-DDNAME
                   MOVE WS-STMTIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STATEMENT PAGE HEADING.  PAGE 1 HAS THE FULL NAME / ADDRESS    *
      * BLOCK, LATER PAGES A ONE LINE CONTINUATION HEADER.             *
      *----------------------------------------------------------------*
       8200-STATEMENT-PAGE.
           ADD 1 TO WS-STMT-PAGE RPT-PAGE-COUNT.
           MOVE WS-STMT-PAGE TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 2 TO RPT-LINE-COUNT.
           MOVE SPACES TO WS-ADDR-LINE.
           IF WS-STMT-PAGE = 1
               MOVE '-'            TO AL-CC
               MOVE WS-SH-NAME     TO AL-TEXT
               MOVE 'ACCOUNT NUMBER: ' TO AL-LABEL
               MOVE WS-SH-ACCT     TO AL-VALUE
               PERFORM 8250-WRITE-ADDR THRU 8250-EXIT
               MOVE SPACES TO WS-ADDR-LINE
               MOVE ' '            TO AL-CC
               MOVE WS-SH-ADDR-1   TO AL-TEXT
               MOVE 'STATEMENT DATE: ' TO AL-LABEL
               MOVE WS-SH-PERIOD   TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE THRU 8300-EXIT
               MOVE WS-DATE-EDIT   TO AL-VALUE
               PERFORM 8250-WRITE-ADDR THRU 8250-EXIT
               MOVE SPACES TO WS-ADDR-LINE
               MOVE ' '            TO AL-CC
               MOVE WS-SH-ADDR-2   TO AL-TEXT
               MOVE 'BRANCH / REP:   ' TO AL-LABEL
               STRING WS-SH-BRANCH ' / ' WS-SH-REP
                      DELIMITED BY SIZE INTO AL-VALUE
               PERFORM 8250-WRITE-ADDR THRU 8250-EXIT
               MOVE SPACES TO WS-ADDR-LINE
               MOVE ' '            TO AL-CC
               MOVE 'BASE CURRENCY:  ' TO AL-LABEL
               MOVE WS-SH-CCY      TO AL-VALUE
               PERFORM 8250-WRITE-ADDR THRU 8250-EXIT
               ADD 2 TO RPT-LINE-COUNT
           ELSE
               MOVE '0'            TO AL-CC
               MOVE WS-SH-NAME     TO AL-TEXT
               MOVE 'ACCOUNT (CONT):  ' TO AL-LABEL
               MOVE WS-SH-ACCT     TO AL-VALUE
               PERFORM 8250-WRITE-ADDR THRU 8250-EXIT
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8250-WRITE-ADDR.
      *----------------------------------------------------------------*
           WRITE RPT-RECORD FROM WS-ADDR-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8250-EXIT.
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
           CLOSE STMTIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'SRR710'        TO CT-STAGE.
           MOVE 'STMT-RECS-IN'  TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-TOTAL-EQUITY TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'STMTS-PRINTED' TO CT-COUNTER-NAME.
           MOVE WS-STMT-CNT     TO CT-COUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR710 EXTRACT RECORDS READ : ' WS-READ-CNT.
           DISPLAY 'SRR710 STATEMENTS PRINTED   : ' WS-STMT-CNT.
           DISPLAY 'SRR710 POSITION LINES       : ' WS-POS-CNT.
           DISPLAY 'SRR710 CASH LINES           : ' WS-CASH-CNT.
           DISPLAY 'SRR710 PAGES                : ' RPT-PAGE-COUNT.
           DISPLAY 'SRR710 WITHOUT TOTAL        : ' WS-NO-TOTAL-CNT.
           DISPLAY 'SRR710 ORPHAN RECORDS       : ' WS-ORPHAN-CNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'STATEMENT PRINT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SRR710 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
