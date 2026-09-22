       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB700.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JANUARY 1990.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB700                                            *
      * DESCRIPTION: MONTH-END CLIENT STATEMENT EXTRACT.               *
      *              READS THE ACCOUNT MASTER IN KEY ORDER AND, FOR    *
      *              EVERY CLIENT ACCOUNT WHOSE STATEMENT CYCLE IS DUE *
      *              (MONTHLY ALWAYS, QUARTERLY ON QUARTER / YEAR END) *
      *              WRITES:                                           *
      *                'A' ACCOUNT HEADER (NAME, ADDRESS, BRANCH/REP)  *
      *                'P' ONE PER OPEN POSITION (FROM THE POSITION    *
      *                    MASTER AS VALUED BY SRB400)                 *
      *                'C' ONE PER CASH BALANCE CURRENCY               *
      *                'T' ACCOUNT TOTAL (USD)                         *
      *              SECURITY DESCRIPTIONS FROM CMD010.                *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRM010 / STEP010  (IKJEFT01 - DB2 PLAN MSSRPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD (CYCLE TYPE)        *
      *              SYSIN    - OPTIONAL  FORCE=M / FORCE=Q (RERUNS)   *
      *              ACCTMAST - ACCOUNT MASTER KSDS (SEQUENTIAL)       *
      *              POSMAST  - POSITION MASTER KSDS (BROWSE BY ACCT)  *
      *              CASHBAL  - CASH BALANCE KSDS (BROWSE BY ACCT)     *
      * OUTPUT     : STMTOUT  - MSEC.PROD.SR.STMTEXT(+1)      (SRSTMT) *
      * CALLS      : CMD010, CMU040, CMU010, CMU050, CMU060, CMU080    *
      * RETURN CODE: 0, 4 WHEN RUN ON A NON MONTH-END DATE (NO OUTPUT) *
      *              OR FX RATES MISSING                               *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1990-01-15 RJK  ORIGINAL                                       *
      * 1994-02-11 DWB  INSTITUTIONAL ADDRESS (AGENT / CUST)  CHG01801 *
      * 1996-04-22 DWB  DESCRIPTIONS FROM DB2 (CMD010)        CHG02215 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2003-09-08 KAP  QUARTERLY STATEMENT CYCLE             CHG11562 *
      * 2009-12-14 SPA  MULTI-CURRENCY CASH, USD TOTALS       CHG19002 *
      * 2014-06-30 SPA  ESTIMATED ANNUAL INCOME (BONDS)       CHG26011 *
      * 2016-10-03 SPA  OMNIBUS ACCOUNTS                      CHG30112 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT PARMCARD       ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS ACCTMAST-KEY
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT POSMAST-FILE   ASSIGN TO POSMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS POS-KEY
                  FILE STATUS IS WS-POSMAST-STATUS.
           SELECT CASHBAL-FILE   ASSIGN TO CASHBAL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CSH-KEY
                  FILE STATUS IS WS-CASHBAL-STATUS.
           SELECT STMTOUT-FILE   ASSIGN TO STMTOUT
                  FILE STATUS IS WS-STMTOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARMCARD-REC                PIC X(80).
       FD  ACCTMAST-FILE.
       01  ACCTMAST-REC.
           05  ACCTMAST-KEY            PIC X(10).
           05  FILLER                  PIC X(290).
       FD  POSMAST-FILE.
       COPY SRPOSN.
       FD  CASHBAL-FILE.
       COPY SRCASH.
       FD  STMTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTOUT-REC                 PIC X(200).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB700'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-EOF                   VALUE '10'.
           05  WS-POSMAST-STATUS       PIC X(02)  VALUE '00'.
               88  POSMAST-OK                     VALUE '00' '02'.
               88  POSMAST-EOF                    VALUE '10'.
               88  POSMAST-NOTFND                 VALUE '23'.
           05  WS-CASHBAL-STATUS       PIC X(02)  VALUE '00'.
               88  CASHBAL-OK                     VALUE '00' '02'.
               88  CASHBAL-EOF                    VALUE '10'.
               88  CASHBAL-NOTFND                 VALUE '23'.
           05  WS-STMTOUT-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-ACCT-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCOUNTS                VALUE 'Y'.
           05  WS-POS-DONE-SW          PIC X(01)  VALUE 'N'.
               88  POSITIONS-DONE                 VALUE 'Y'.
           05  WS-CASH-DONE-SW         PIC X(01)  VALUE 'N'.
               88  CASH-DONE                      VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-CYCLE-SW             PIC X(01)  VALUE SPACE.
               88  RUN-MONTHLY                    VALUE 'M'.
               88  RUN-QUARTERLY                  VALUE 'Q'.
               88  RUN-NONE                       VALUE ' '.
           05  WS-DUE-SW               PIC X(01)  VALUE 'N'.
               88  STATEMENT-DUE                  VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-PARM-CARD                PIC X(80).
       01  WS-PERIOD-END               PIC 9(08)  VALUE ZERO.
       01  WS-PERIOD-START             PIC 9(08)  VALUE ZERO.
      *----------------------------------------------------------------*
      * PER ACCOUNT WORK                                               *
      *----------------------------------------------------------------*
       01  WS-ACCT-WORK.
           05  WS-SEQ-NO               PIC 9(05).
           05  WS-TOT-MV               PIC S9(15)V99    COMP-3.
           05  WS-TOT-CASH             PIC S9(15)V99    COMP-3.
           05  WS-TOT-POSITIONS        PIC S9(05)       COMP-3.
           05  WS-CASH-USD             PIC S9(15)V99    COMP-3.
           05  WS-EST-INCOME           PIC S9(13)V99    COMP-3.
       01  WS-CUR-ACCT                 PIC X(10).
      *----------------------------------------------------------------*
      * SECURITY DESCRIPTION CACHE                                     *
      *----------------------------------------------------------------*
       01  WS-SEC-CACHE.
           05  WS-SC-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-SC-ENTRY OCCURS 3000 TIMES INDEXED BY SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-DESC          PIC X(40).
               10  WS-SC-TYPE          PIC X(02).
               10  WS-SC-TYPE-DATA     PIC X(60).
               10  WS-SC-FACTOR        PIC S9(05)V9(04) COMP-3.
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-ACCT-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-NOT-CLIENT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-CLOSED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-NOT-DUE         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STMT-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MONTHLY-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-QUARTERLY-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EMPTY-STMT-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REC-OUT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POS-LINE-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-LINE-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-DESC-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-ERR-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BAD-VALUE-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GRAND-MV             PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GRAND-CASH           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       COPY SRSTMT.
       COPY CMACCT.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMSECLNK.
       COPY CMFXLNK.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           IF NOT RUN-NONE
               PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
                   UNTIL END-OF-ACCOUNTS
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
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
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
           MOVE 'STATEMENT EXTRACT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *    ---- WHICH CYCLES ARE DUE -------------------------------
           EVALUATE TRUE
               WHEN DC-QTR-END
                   SET RUN-QUARTERLY TO TRUE
               WHEN DC-MONTH-END
                   SET RUN-MONTHLY TO TRUE
               WHEN OTHER
                   SET RUN-NONE TO TRUE
           END-EVALUATE.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           IF RUN-NONE
               DISPLAY 'SRB700 CYCLE TYPE ' DC-CYCLE-TYPE
                       ' - NOT A STATEMENT DATE, NOTHING EXTRACTED'
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           PERFORM 1200-PERIOD-DATES THRU 1200-EXIT.
           PERFORM 1300-OPEN-FILES THRU 1300-EXIT.
           IF NOT RUN-NONE
               PERFORM 8000-READ-ACCOUNT THRU 8000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FORCE=M OR FORCE=Q FOR A RERUN OUTSIDE THE CALENDAR            *
      *----------------------------------------------------------------*
       1100-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               GO TO 1100-EXIT
           END-IF.
           PERFORM UNTIL END-OF-PARMS
               READ PARMCARD INTO WS-PARM-CARD
                   AT END
                       MOVE 'Y' TO WS-PARM-EOF-SW
                   NOT AT END
                       IF WS-PARM-CARD (1:6) = 'FORCE='
                           DISPLAY 'SRB700 CONTROL CARD: '
                                   WS-PARM-CARD (1:20)
                           EVALUATE WS-PARM-CARD (7:1)
                               WHEN 'M'  SET RUN-MONTHLY TO TRUE
                               WHEN 'Q'  SET RUN-QUARTERLY TO TRUE
                               WHEN OTHER
                                   DISPLAY 'SRB700 INVALID FORCE= '
                                           'VALUE IGNORED'
                           END-EVALUATE
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE PARMCARD.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STATEMENT PERIOD - CALENDAR MONTH OF THE BUSINESS DATE         *
      *----------------------------------------------------------------*
       1200-PERIOD-DATES.
           MOVE 'EOM '       TO DT-FUNCTION.
           MOVE SPACES       TO DT-CALENDAR.
           MOVE DC-BUS-DATE  TO DT-DATE-1.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE DT-RESULT-DATE TO WS-PERIOD-END
           ELSE
               MOVE DC-BUS-DATE TO WS-PERIOD-END
           END-IF.
           MOVE DC-BUS-DATE TO WS-PERIOD-START.
           MOVE '01' TO WS-PERIOD-START (7:2).
       1200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1300-OPEN-FILES.
      *----------------------------------------------------------------*
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT CASHBAL-FILE.
           IF WS-CASHBAL-STATUS NOT = '00'
               MOVE 'CASHBAL' TO AB-DDNAME
               MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT STMTOUT-FILE.
           IF WS-STMTOUT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO AB-DDNAME
               MOVE WS-STMTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       1300-EXIT.
           EXIT.
      *================================================================*
      * ONE ACCOUNT                                                    *
      *================================================================*
       2000-PROCESS-ACCOUNT.
           ADD 1 TO WS-ACCT-READ.
           PERFORM 2100-CHECK-DUE THRU 2100-EXIT.
           IF STATEMENT-DUE
               MOVE ACCT-NO TO WS-CUR-ACCT
               MOVE ZERO    TO WS-SEQ-NO WS-TOT-MV WS-TOT-CASH
                               WS-TOT-POSITIONS
               PERFORM 3000-WRITE-HEADER THRU 3000-EXIT
               PERFORM 4000-POSITIONS THRU 4000-EXIT
               PERFORM 5000-CASH THRU 5000-EXIT
               PERFORM 6000-WRITE-TOTAL THRU 6000-EXIT
               ADD 1 TO WS-STMT-CNT
           END-IF.
           PERFORM 8000-READ-ACCOUNT THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CLIENT ACCOUNTS ONLY.  CLOSED ACCOUNTS GET A FINAL STATEMENT   *
      * IN THE MONTH THEY CLOSE.  NO CYCLE CODE = MONTHLY (PRE-2003).  *
      *----------------------------------------------------------------*
       2100-CHECK-DUE.
           MOVE 'N' TO WS-DUE-SW.
           IF NOT ACCT-RETAIL
           AND NOT ACCT-INSTITUTIONAL
           AND NOT ACCT-OMNIBUS
               ADD 1 TO WS-ACCT-NOT-CLIENT
               GO TO 2100-EXIT
           END-IF.
           IF ACCT-CLOSED
               IF ACCT-CLOSE-DATE NOT NUMERIC
               OR ACCT-CLOSE-DATE < WS-PERIOD-START
                   ADD 1 TO WS-ACCT-CLOSED
                   GO TO 2100-EXIT
               END-IF
           END-IF.
           EVALUATE TRUE
               WHEN ACCT-STMT-QUARTERLY
                   IF RUN-QUARTERLY
                       MOVE 'Y' TO WS-DUE-SW
                       ADD 1 TO WS-QUARTERLY-CNT
                   ELSE
                       ADD 1 TO WS-ACCT-NOT-DUE
                   END-IF
               WHEN OTHER
                   MOVE 'Y' TO WS-DUE-SW
                   ADD 1 TO WS-MONTHLY-CNT
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *================================================================*
      * 'A' ACCOUNT HEADER                                             *
      *================================================================*
       3000-WRITE-HEADER.
           MOVE SPACES           TO STM-STATEMENT-REC.
           MOVE 'A'              TO STM-REC-TYPE.
           MOVE ACCT-NAME        TO STM-NAME.
           EVALUATE TRUE
               WHEN ACCT-RETAIL
                   MOVE ACCT-RTL-ADDR-1 TO STM-ADDR-1
                   STRING ACCT-RTL-ADDR-2 DELIMITED BY '  '
                          ' '             DELIMITED BY SIZE
                          ACCT-RTL-STATE  DELIMITED BY SIZE
                          ' '             DELIMITED BY SIZE
                          ACCT-RTL-ZIP (1:5) DELIMITED BY SIZE
                          INTO STM-ADDR-2
               WHEN OTHER
                   STRING 'C/O AGENT ' DELIMITED BY SIZE
                          ACCT-INST-AGENT DELIMITED BY SIZE
                          INTO STM-ADDR-1
                   STRING 'CUSTODY A/C ' DELIMITED BY SIZE
                          ACCT-INST-CUST-ACCT DELIMITED BY SIZE
                          INTO STM-ADDR-2
           END-EVALUATE.
           MOVE ACCT-BRANCH      TO STM-BRANCH.
           MOVE ACCT-REP         TO STM-REP.
           MOVE ACCT-BASE-CCY    TO STM-BASE-CCY.
           IF STM-BASE-CCY = SPACES
               MOVE 'USD' TO STM-BASE-CCY
           END-IF.
           PERFORM 8500-WRITE-STMT THRU 8500-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
      * 'P' POSITIONS - BROWSE THE POSITION MASTER FROM ACCOUNT KEY    *
      *================================================================*
       4000-POSITIONS.
           MOVE 'N' TO WS-POS-DONE-SW.
           MOVE LOW-VALUES TO POS-KEY.
           MOVE WS-CUR-ACCT TO POS-ACCT-NO.
           START POSMAST-FILE KEY IS NOT LESS THAN POS-KEY.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   CONTINUE
               WHEN POSMAST-NOTFND
                   MOVE 'Y' TO WS-POS-DONE-SW
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '4000-POSITIONS' TO AB-PARAGRAPH
                   MOVE WS-CUR-ACCT TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM UNTIL POSITIONS-DONE
               READ POSMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN POSMAST-OK
                       IF POS-ACCT-NO NOT = WS-CUR-ACCT
                           MOVE 'Y' TO WS-POS-DONE-SW
                       ELSE
                           IF POS-TD-QTY NOT = ZERO
                               PERFORM 4100-POSITION-LINE
                                   THRU 4100-EXIT
                           END-IF
                       END-IF
                   WHEN POSMAST-EOF
                       MOVE 'Y' TO WS-POS-DONE-SW
                   WHEN OTHER
                       MOVE 'POSMAST' TO AB-DDNAME
                       MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '4000-POSITIONS' TO AB-PARAGRAPH
                       MOVE WS-CUR-ACCT TO AB-KEY
                       MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-PERFORM.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-POSITION-LINE.
      *----------------------------------------------------------------*
           PERFORM 4200-GET-SECURITY THRU 4200-EXIT.
           MOVE SPACES             TO STM-STATEMENT-REC.
           MOVE 'P'                TO STM-REC-TYPE.
           MOVE POS-CUSIP          TO STM-CUSIP.
           MOVE WS-SC-DESC (SC-IDX) TO STM-SEC-DESC.
           MOVE POS-TD-QTY         TO STM-QTY.
           IF POS-MKT-PRICE NUMERIC
               MOVE POS-MKT-PRICE  TO STM-PRICE
           ELSE
               MOVE ZERO           TO STM-PRICE
               ADD 1 TO WS-BAD-VALUE-CNT
           END-IF.
           IF POS-MKT-VALUE-USD NUMERIC
               MOVE POS-MKT-VALUE-USD TO STM-MKT-VALUE
           ELSE
               MOVE ZERO           TO STM-MKT-VALUE
               ADD 1 TO WS-BAD-VALUE-CNT
           END-IF.
           IF POS-COST-BASIS NUMERIC
               MOVE POS-COST-BASIS TO STM-COST-BASIS
           ELSE
               MOVE ZERO           TO STM-COST-BASIS
           END-IF.
           IF POS-UNRLZD-PL NUMERIC
               MOVE POS-UNRLZD-PL  TO STM-UNRLZD-PL
           ELSE
               MOVE ZERO           TO STM-UNRLZD-PL
           END-IF.
           PERFORM 4300-ESTIMATED-INCOME THRU 4300-EXIT.
           MOVE WS-EST-INCOME      TO STM-EST-INCOME.
           PERFORM 8500-WRITE-STMT THRU 8500-EXIT.
           ADD STM-MKT-VALUE TO WS-TOT-MV.
           ADD 1 TO WS-TOT-POSITIONS WS-POS-LINE-CNT.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SECURITY DESCRIPTION (CACHED).  SC-IDX POINTS AT THE ENTRY.    *
      *----------------------------------------------------------------*
       4200-GET-SECURITY.
           SET SC-IDX TO 1.
           SEARCH WS-SC-ENTRY
               AT END
                   PERFORM 4250-LOAD-SECURITY THRU 4250-EXIT
               WHEN SC-IDX > WS-SC-USED
                   PERFORM 4250-LOAD-SECURITY THRU 4250-EXIT
               WHEN WS-SC-CUSIP (SC-IDX) = POS-CUSIP
                   CONTINUE
           END-SEARCH.
       4200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4250-LOAD-SECURITY.
      *----------------------------------------------------------------*
           IF WS-SC-USED < 3000
               ADD 1 TO WS-SC-USED
           END-IF.
           SET SC-IDX TO WS-SC-USED.
           MOVE POS-CUSIP TO WS-SC-CUSIP (SC-IDX).
           MOVE 'GET '    TO SL-FUNCTION.
           MOVE POS-CUSIP TO SL-KEY-CUSIP.
           MOVE SPACES    TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA   TO SEC-MASTER-REC
                   MOVE SEC-DESC      TO WS-SC-DESC (SC-IDX)
                   MOVE SEC-TYPE      TO WS-SC-TYPE (SC-IDX)
                   MOVE SEC-TYPE-DATA TO WS-SC-TYPE-DATA (SC-IDX)
                   IF SEC-PRICE-FACTOR NUMERIC
                       MOVE SEC-PRICE-FACTOR TO WS-SC-FACTOR (SC-IDX)
                   ELSE
                       MOVE ZERO TO WS-SC-FACTOR (SC-IDX)
                   END-IF
               WHEN SL-NOT-FOUND
                   ADD 1 TO WS-NO-DESC-CNT
                   MOVE '*** DESCRIPTION NOT AVAILABLE ***'
                                      TO WS-SC-DESC (SC-IDX)
                   MOVE POS-SEC-TYPE  TO WS-SC-TYPE (SC-IDX)
                   MOVE SPACES        TO WS-SC-TYPE-DATA (SC-IDX)
                   MOVE ZERO          TO WS-SC-FACTOR (SC-IDX)
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '4250-LOAD-SECURITY' TO AB-PARAGRAPH
                   MOVE SL-SQLCODE TO AB-SQLCODE
                   MOVE POS-CUSIP TO AB-KEY
                   MOVE 'CMD010 SECURITY MASTER ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       4250-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ESTIMATED ANNUAL INCOME - BONDS ONLY: FACE X COUPON / 100      *
      *----------------------------------------------------------------*
       4300-ESTIMATED-INCOME.
           MOVE ZERO TO WS-EST-INCOME.
           MOVE WS-SC-TYPE (SC-IDX)      TO SEC-TYPE.
           MOVE WS-SC-TYPE-DATA (SC-IDX) TO SEC-TYPE-DATA.
           IF SEC-FIXED-INCOME
               IF SEC-COUPON-RATE NUMERIC
                   COMPUTE WS-EST-INCOME ROUNDED =
                       POS-TD-QTY * SEC-COUPON-RATE / 100
                       ON SIZE ERROR
                           MOVE ZERO TO WS-EST-INCOME
                   END-COMPUTE
               END-IF
           END-IF.
       4300-EXIT.
           EXIT.
      *================================================================*
      * 'C' CASH - BROWSE THE CASH BALANCE FILE FROM ACCOUNT KEY       *
      *================================================================*
       5000-CASH.
           MOVE 'N' TO WS-CASH-DONE-SW.
           MOVE LOW-VALUES  TO CSH-KEY.
           MOVE WS-CUR-ACCT TO CSH-ACCT-NO.
           START CASHBAL-FILE KEY IS NOT LESS THAN CSH-KEY.
           EVALUATE TRUE
               WHEN CASHBAL-OK
                   CONTINUE
               WHEN CASHBAL-NOTFND
                   MOVE 'Y' TO WS-CASH-DONE-SW
               WHEN OTHER
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5000-CASH' TO AB-PARAGRAPH
                   MOVE WS-CUR-ACCT TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM UNTIL CASH-DONE
               READ CASHBAL-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN CASHBAL-OK
                       IF CSH-ACCT-NO NOT = WS-CUR-ACCT
                           MOVE 'Y' TO WS-CASH-DONE-SW
                       ELSE
                           PERFORM 5100-CASH-LINE THRU 5100-EXIT
                       END-IF
                   WHEN CASHBAL-EOF
                       MOVE 'Y' TO WS-CASH-DONE-SW
                   WHEN OTHER
                       MOVE 'CASHBAL' TO AB-DDNAME
                       MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '5000-CASH' TO AB-PARAGRAPH
                       MOVE WS-CUR-ACCT TO AB-KEY
                       MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-PERFORM.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5100-CASH-LINE.
      *----------------------------------------------------------------*
           MOVE SPACES             TO STM-STATEMENT-REC.
           MOVE 'C'                TO STM-REC-TYPE.
           MOVE CSH-CCY            TO STM-CASH-CCY.
           MOVE CSH-TD-BALANCE     TO STM-CASH-BALANCE.
           MOVE CSH-INCOME-YTD     TO STM-CASH-INCOME-YTD.
           MOVE CSH-WHT-YTD        TO STM-CASH-WHT-YTD.
           PERFORM 8500-WRITE-STMT THRU 8500-EXIT.
           ADD 1 TO WS-CASH-LINE-CNT.
      *    TOTAL CASH IN USD
           IF CSH-CCY = 'USD' OR CSH-CCY = SPACES
               MOVE CSH-TD-BALANCE TO WS-CASH-USD
           ELSE
               MOVE CSH-CCY        TO FX-FROM-CCY
               MOVE 'USD'          TO FX-TO-CCY
               MOVE DC-BUS-DATE    TO FX-RATE-DATE
               MOVE CSH-TD-BALANCE TO FX-AMOUNT-IN
               CALL 'CMU040' USING FX-CONVERT-PARMS
               EVALUATE TRUE
                   WHEN FX-OK
                   WHEN FX-STALE-RATE
                       MOVE FX-AMOUNT-OUT TO WS-CASH-USD
                   WHEN FX-RATE-NOT-FOUND
                       ADD 1 TO WS-FX-ERR-CNT
                       MOVE ZERO TO WS-CASH-USD
                       IF WS-RETURN-CODE < 4
                           MOVE 4 TO WS-RETURN-CODE
                       END-IF
                   WHEN OTHER
                       MOVE 1003 TO AB-ABEND-CODE
                       MOVE '5100-CASH-LINE' TO AB-PARAGRAPH
                       MOVE CSH-KEY TO AB-KEY
                       MOVE FX-MESSAGE TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-IF.
           ADD WS-CASH-USD TO WS-TOT-CASH.
       5100-EXIT.
           EXIT.
      *================================================================*
      * 'T' ACCOUNT TOTAL                                              *
      *================================================================*
       6000-WRITE-TOTAL.
           MOVE SPACES             TO STM-STATEMENT-REC.
           MOVE 'T'                TO STM-REC-TYPE.
           MOVE WS-TOT-MV          TO STM-TOT-MKT-VALUE.
           MOVE WS-TOT-CASH        TO STM-TOT-CASH.
           COMPUTE STM-TOT-EQUITY = WS-TOT-MV + WS-TOT-CASH.
           MOVE WS-TOT-POSITIONS   TO STM-TOT-POSITIONS.
           PERFORM 8500-WRITE-STMT THRU 8500-EXIT.
           ADD WS-TOT-MV   TO WS-GRAND-MV.
           ADD WS-TOT-CASH TO WS-GRAND-CASH.
           IF WS-TOT-POSITIONS = ZERO AND WS-TOT-CASH = ZERO
               ADD 1 TO WS-EMPTY-STMT-CNT
           END-IF.
       6000-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-ACCOUNT.
           READ ACCTMAST-FILE NEXT RECORD INTO ACCT-MASTER-REC.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   CONTINUE
               WHEN ACCTMAST-EOF
                   MOVE 'Y' TO WS-ACCT-EOF-SW
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-ACCOUNT' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-WRITE-STMT.
      *----------------------------------------------------------------*
           ADD 1 TO WS-SEQ-NO.
           MOVE WS-CUR-ACCT   TO STM-ACCT-NO.
           MOVE WS-SEQ-NO     TO STM-SEQ-NO.
           MOVE WS-PERIOD-END TO STM-PERIOD-END.
           WRITE STMTOUT-REC FROM STM-STATEMENT-REC.
           IF WS-STMTOUT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO AB-DDNAME
               MOVE WS-STMTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8500-WRITE-STMT' TO AB-PARAGRAPH
               MOVE WS-CUR-ACCT TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-REC-OUT-CNT.
       8500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8600-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB700'       TO CT-STAGE.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '8600-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8600-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE ACCTMAST-FILE POSMAST-FILE CASHBAL-FILE.
           CLOSE STMTOUT-FILE.
           IF WS-STMTOUT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO AB-DDNAME
               MOVE WS-STMTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'ACCTS-READ'     TO CT-COUNTER-NAME.
           MOVE WS-ACCT-READ     TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'STMTS-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-STMT-CNT      TO CT-COUNT.
           COMPUTE CT-AMOUNT = WS-GRAND-MV + WS-GRAND-CASH.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'STMT-RECS-OUT'  TO CT-COUNTER-NAME.
           MOVE WS-REC-OUT-CNT   TO CT-COUNT.
           MOVE WS-GRAND-MV      TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* SRB700 - MONTH-END STATEMENT EXTRACT         *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' CYCLE TYPE / RUN         : ' DC-CYCLE-TYPE ' / '
                   WS-CYCLE-SW.
           DISPLAY ' STATEMENT PERIOD         : ' WS-PERIOD-START
                   ' - ' WS-PERIOD-END.
           MOVE WS-ACCT-READ TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS READ            : ' WS-DISP-CNT.
           MOVE WS-ACCT-NOT-CLIENT TO WS-DISP-CNT.
           DISPLAY '   FIRM / STREET SKIPPED  : ' WS-DISP-CNT.
           MOVE WS-ACCT-CLOSED TO WS-DISP-CNT.
           DISPLAY '   CLOSED SKIPPED         : ' WS-DISP-CNT.
           MOVE WS-ACCT-NOT-DUE TO WS-DISP-CNT.
           DISPLAY '   CYCLE NOT DUE          : ' WS-DISP-CNT.
           MOVE WS-STMT-CNT TO WS-DISP-CNT.
           DISPLAY ' STATEMENTS EXTRACTED     : ' WS-DISP-CNT.
           MOVE WS-MONTHLY-CNT TO WS-DISP-CNT.
           DISPLAY '   MONTHLY                : ' WS-DISP-CNT.
           MOVE WS-QUARTERLY-CNT TO WS-DISP-CNT.
           DISPLAY '   QUARTERLY              : ' WS-DISP-CNT.
           MOVE WS-EMPTY-STMT-CNT TO WS-DISP-CNT.
           DISPLAY '   NO POSITIONS OR CASH   : ' WS-DISP-CNT.
           MOVE WS-POS-LINE-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITION LINES           : ' WS-DISP-CNT.
           MOVE WS-CASH-LINE-CNT TO WS-DISP-CNT.
           DISPLAY ' CASH LINES               : ' WS-DISP-CNT.
           MOVE WS-REC-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' RECORDS WRITTEN          : ' WS-DISP-CNT.
           MOVE WS-NO-DESC-CNT TO WS-DISP-CNT.
           DISPLAY ' SECURITIES NOT ON MASTER : ' WS-DISP-CNT.
           MOVE WS-BAD-VALUE-CNT TO WS-DISP-CNT.
           DISPLAY ' UNVALUED POSITIONS       : ' WS-DISP-CNT.
           MOVE WS-FX-ERR-CNT TO WS-DISP-CNT.
           DISPLAY ' FX RATES MISSING         : ' WS-DISP-CNT.
           MOVE WS-GRAND-MV TO WS-DISP-AMT.
           DISPLAY ' TOTAL MARKET VALUE USD   : ' WS-DISP-AMT.
           MOVE WS-GRAND-CASH TO WS-DISP-AMT.
           DISPLAY ' TOTAL CASH USD           : ' WS-DISP-AMT.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'STATEMENT EXTRACT ENDED' TO AU-MESSAGE.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
           ELSE
               MOVE 'I' TO AU-SEVERITY
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'SRB700 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB700 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB700 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
