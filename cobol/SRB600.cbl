       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB600.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JULY 1991.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB600                                            *
      * DESCRIPTION: GENERAL LEDGER JOURNAL BUILD.                     *
      *              READS THE POSTING JOURNAL SORTED BY SOURCE /      *
      *              REFERENCE / LEG AND BUILDS BALANCED DEBIT AND     *
      *              CREDIT LINES FOR EVERY CUSTOMER / FIRM LEG:       *
      *                PRINCIPAL OR CASH - GL MAP BY TXN CODE,         *
      *                                    ACCOUNT TYPE, SECURITY TYPE *
      *                COMMISSION (TCOM) - TRADES                      *
      *                FEES       (TFEE) - TRADES                      *
      *              STREET (LOCATION) LEGS ARE MEMO ONLY - NO GL.     *
      *              COST CENTER = MAP COST CENTER PREFIX + BRANCH.    *
      *              DIVIDEND ACCRUAL LINES FROM CAB500 ARE MERGED IN  *
      *              UNCHANGED.  EVERY REFERENCE MUST BALANCE.         *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD070 / STEP020  (IKJEFT01 - DB2 PLAN MSSRPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              JRNLIN   - MSEC.PROD.SR.POSTJRNL.SORTED(+1)       *
      *                         (SRPSTJ, BY SOURCE/REF/LEG)            *
      *              ACCRIN   - MSEC.PROD.CA.ACCRUAL(0)     (SRGLJNL)  *
      *              ACCTMAST - ACCOUNT MASTER KSDS (BRANCH)           *
      * OUTPUT     : GLJOUT   - MSEC.PROD.SR.GLJRNL(+1)     (SRGLJNL)  *
      *              GLSUMM   - MSEC.PROD.SR.GLSUMM(+1)     (SRGLSUM)  *
      * CALLS      : CMD050, CMU040, CMU050, CMU060, CMU080            *
      * RETURN CODE: 0 CLEAN, 4 UNMAPPED CODES (SUSPENSE) OR ACCRUAL   *
      *              REFERENCES OUT OF BALANCE, 8 TOTAL DR NOT = CR    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1991-07-22 DWB  ORIGINAL                              CHG00987 *
      * 1996-04-22 DWB  GL MAP FROM DB2 (CMD050)              CHG02215 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-07-16 KAP  COMMISSION AND FEE LINES              CHG08811 *
      * 2004-02-09 KAP  COST CENTER FROM BRANCH               CHG11890 *
      * 2009-12-14 SPA  USD AMOUNT VIA CMU040                 CHG19002 *
      * 2010-04-05 SPA  CORPORATE ACTION CODES                CHG19870 *
      * 2012-11-19 SPA  MERGE CA DIVIDEND ACCRUALS            CHG23904 *
      * 2019-02-25 MHC  SUMMARY FILE FOR SRR610               CHG33410 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT JRNLIN-FILE    ASSIGN TO JRNLIN
                  FILE STATUS IS WS-JRNLIN-STATUS.
           SELECT ACCRIN-FILE    ASSIGN TO ACCRIN
                  FILE STATUS IS WS-ACCRIN-STATUS.
           SELECT GLJOUT-FILE    ASSIGN TO GLJOUT
                  FILE STATUS IS WS-GLJOUT-STATUS.
           SELECT GLSUMM-FILE    ASSIGN TO GLSUMM
                  FILE STATUS IS WS-GLSUMM-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCTMAST-KEY
                  FILE STATUS IS WS-ACCTMAST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  JRNLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  JRNLIN-REC                  PIC X(300).
       FD  ACCRIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACCRIN-REC                  PIC X(150).
       FD  GLJOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLJOUT-REC                  PIC X(150).
       FD  GLSUMM-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLSUMM-REC                  PIC X(100).
       FD  ACCTMAST-FILE.
       01  ACCTMAST-REC.
           05  ACCTMAST-KEY            PIC X(10).
           05  FILLER                  PIC X(290).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB600'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-JRNLIN-STATUS        PIC X(02)  VALUE '00'.
               88  JRNLIN-OK                      VALUE '00'.
               88  JRNLIN-EOF                     VALUE '10'.
           05  WS-ACCRIN-STATUS        PIC X(02)  VALUE '00'.
               88  ACCRIN-OK                      VALUE '00'.
               88  ACCRIN-EOF                     VALUE '10'.
           05  WS-GLJOUT-STATUS        PIC X(02)  VALUE '00'.
           05  WS-GLSUMM-STATUS        PIC X(02)  VALUE '00'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
       01  WS-SWITCHES.
           05  WS-JRNL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-JOURNAL                 VALUE 'Y'.
           05  WS-ACCR-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCRUALS                VALUE 'Y'.
           05  WS-TRADE-SW             PIC X(01)  VALUE 'N'.
               88  TRADE-ACTIVITY                 VALUE 'Y'.
           05  WS-FIRST-ACCR-SW        PIC X(01)  VALUE 'Y'.
               88  FIRST-ACCRUAL                  VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * REFERENCE CONTROL                                              *
      *----------------------------------------------------------------*
       01  WS-CUR-REF-KEY.
           05  WS-CRK-SOURCE           PIC X(02).
           05  WS-CRK-REF              PIC X(16).
       01  WS-PREV-REF-KEY             PIC X(18)  VALUE LOW-VALUES.
       01  WS-REF-TOTALS.
           05  WS-RT-DR                PIC S9(15)V99    COMP-3.
           05  WS-RT-CR                PIC S9(15)V99    COMP-3.
           05  WS-RT-LINE-NO           PIC 9(03).
           05  WS-RT-LINES             PIC S9(05)       COMP-3.
      *----------------------------------------------------------------*
      * AMOUNT WORK                                                    *
      *----------------------------------------------------------------*
       01  WS-AMOUNT-WORK.
           05  WS-PRINCIPAL            PIC S9(15)V99    COMP-3.
           05  WS-COMMISSION           PIC S9(15)V99    COMP-3.
           05  WS-FEES                 PIC S9(15)V99    COMP-3.
           05  WS-CHARGES              PIC S9(15)V99    COMP-3.
           05  WS-ABS-CASH             PIC S9(15)V99    COMP-3.
           05  WS-ABS-QTY              PIC S9(11)V9(04) COMP-3.
           05  WS-PRICE-MULT           PIC S9(01)V9(04) COMP-3.
           05  WS-LINE-AMOUNT          PIC S9(15)V99    COMP-3.
           05  WS-LINE-TXN-CODE        PIC X(04).
           05  WS-LINE-DESC            PIC X(30).
       01  WS-ACCT-WORK.
           05  WS-LAST-ACCT            PIC X(10)  VALUE LOW-VALUES.
           05  WS-LAST-BRANCH          PIC X(03)  VALUE '000'.
       01  WS-SUSPENSE-GL              PIC X(10)  VALUE 'SUSP999999'.
       01  WS-DEFAULT-CC-PREFIX        PIC X(03)  VALUE '100'.
      *----------------------------------------------------------------*
      * GL MAP CACHE (TXN/ACCT TYPE/SEC TYPE -> DR/CR/CC)              *
      *----------------------------------------------------------------*
       01  WS-MAP-CACHE.
           05  WS-MAP-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-MAP-ENTRY OCCURS 500 TIMES INDEXED BY MP-IDX.
               10  WS-MP-KEY           PIC X(08).
               10  WS-MP-FOUND         PIC X(01).
               10  WS-MP-DR            PIC X(10).
               10  WS-MP-CR            PIC X(10).
               10  WS-MP-CC            PIC X(06).
               10  WS-MP-DESC          PIC X(30).
       01  WS-MAP-KEY.
           05  WS-MK-TXN               PIC X(04).
           05  WS-MK-ACCT-TYPE         PIC X(02).
           05  WS-MK-SEC-TYPE          PIC X(02).
       01  WS-MAP-RESULT.
           05  WS-MR-FOUND             PIC X(01).
               88  MAP-FOUND                      VALUE 'Y'.
           05  WS-MR-DR                PIC X(10).
           05  WS-MR-CR                PIC X(10).
           05  WS-MR-CC                PIC X(06).
           05  WS-MR-DESC              PIC X(30).
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-JRNL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-NO-AMT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCR-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-OUT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GEN-LINES            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REFS-OUT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REFS-UNBAL           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNMAPPED-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COMM-LINES           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FEE-LINES            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DERIVED-CHG-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-ACCT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-ERR-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMD050-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SUMM-CNT             PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-TOTAL-DR             PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOTAL-CR             PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOTAL-DR-USD         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-JRNL-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-ACCR-AMT-HASH        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       COPY SRPSTJ.
       COPY SRACTV.
       COPY SRACTX.
       COPY SRGLJNL.
       COPY SRGLSUM.
       COPY CMACCT.
       COPY CMDATEW.
       COPY CMGMLNK.
       COPY CMFXLNK.
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
           PERFORM 2000-PROCESS-JOURNAL THRU 2000-EXIT
               UNTIL END-OF-JOURNAL.
           IF WS-PREV-REF-KEY NOT = LOW-VALUES
               PERFORM 3000-REFERENCE-END THRU 3000-EXIT
           END-IF.
           PERFORM 5000-MERGE-ACCRUALS THRU 5000-EXIT.
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
           MOVE 'GL JOURNAL BUILD STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT JRNLIN-FILE.
           IF WS-JRNLIN-STATUS NOT = '00'
               MOVE 'JRNLIN' TO AB-DDNAME
               MOVE WS-JRNLIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT ACCRIN-FILE.
           IF WS-ACCRIN-STATUS NOT = '00'
               MOVE 'ACCRIN' TO AB-DDNAME
               MOVE WS-ACCRIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT GLJOUT-FILE.
           IF WS-GLJOUT-STATUS NOT = '00'
               MOVE 'GLJOUT' TO AB-DDNAME
               MOVE WS-GLJOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT GLSUMM-FILE.
           IF WS-GLSUMM-STATUS NOT = '00'
               MOVE 'GLSUMM' TO AB-DDNAME
               MOVE WS-GLSUMM-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           INITIALIZE WS-REF-TOTALS.
           PERFORM 8000-READ-JOURNAL THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * ONE POSTING JOURNAL LEG                                        *
      *================================================================*
       2000-PROCESS-JOURNAL.
           ADD 1 TO WS-JRNL-READ.
           MOVE PSJ-ACTIVITY TO ACT-ACTIVITY-REC ACX-ACTIVITY-EXT.
           ADD ACT-CASH-CHANGE TO WS-JRNL-CASH-HASH.
           MOVE ACT-SOURCE TO WS-CRK-SOURCE.
           MOVE ACT-REF    TO WS-CRK-REF.
           IF WS-CUR-REF-KEY NOT = WS-PREV-REF-KEY
               IF WS-PREV-REF-KEY NOT = LOW-VALUES
                   PERFORM 3000-REFERENCE-END THRU 3000-EXIT
               END-IF
               MOVE WS-CUR-REF-KEY TO WS-PREV-REF-KEY
           END-IF.
      *    STREET SIDE IS A MEMO ENTRY ON THE STOCK RECORD ONLY
           IF ACT-ACCT-TYPE = 'ST'
               ADD 1 TO WS-JRNL-STREET
               GO TO 2000-NEXT
           END-IF.
           PERFORM 2050-GET-BRANCH THRU 2050-EXIT.
           EVALUATE ACT-TYPE
               WHEN 'BUY' WHEN 'SEL' WHEN 'SSL'
               WHEN 'BCV' WHEN 'XBY' WHEN 'XSL'
                   MOVE 'Y' TO WS-TRADE-SW
                   PERFORM 2100-TRADE-AMOUNTS THRU 2100-EXIT
               WHEN OTHER
                   MOVE 'N' TO WS-TRADE-SW
                   PERFORM 2200-OTHER-AMOUNTS THRU 2200-EXIT
           END-EVALUATE.
           IF WS-PRINCIPAL = ZERO AND WS-COMMISSION = ZERO
           AND WS-FEES = ZERO
               ADD 1 TO WS-JRNL-NO-AMT
               GO TO 2000-NEXT
           END-IF.
      *    ---- MAIN LINE PAIR --------------------------------------
           IF WS-PRINCIPAL NOT = ZERO
               MOVE ACT-GL-TXN-CODE TO WS-LINE-TXN-CODE
               MOVE WS-PRINCIPAL    TO WS-LINE-AMOUNT
               PERFORM 4000-BUILD-LINE-PAIR THRU 4000-EXIT
           END-IF.
      *    ---- COMMISSION ------------------------------------------
           IF WS-COMMISSION NOT = ZERO
               MOVE 'TCOM'          TO WS-LINE-TXN-CODE
               MOVE WS-COMMISSION   TO WS-LINE-AMOUNT
               PERFORM 4000-BUILD-LINE-PAIR THRU 4000-EXIT
               ADD 1 TO WS-COMM-LINES
           END-IF.
      *    ---- FEES ------------------------------------------------
           IF WS-FEES NOT = ZERO
               MOVE 'TFEE'          TO WS-LINE-TXN-CODE
               MOVE WS-FEES         TO WS-LINE-AMOUNT
               PERFORM 4000-BUILD-LINE-PAIR THRU 4000-EXIT
               ADD 1 TO WS-FEE-LINES
           END-IF.
       2000-NEXT.
           PERFORM 8000-READ-JOURNAL THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BRANCH FOR THE COST CENTER (ACCOUNTS COME IN REF ORDER, NOT    *
      * ACCOUNT ORDER - ONLY THE LAST ONE IS REMEMBERED)               *
      *----------------------------------------------------------------*
       2050-GET-BRANCH.
           IF ACT-ACCT-NO = WS-LAST-ACCT
               GO TO 2050-EXIT
           END-IF.
           MOVE ACT-ACCT-NO TO WS-LAST-ACCT ACCTMAST-KEY.
           READ ACCTMAST-FILE INTO ACCT-MASTER-REC.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE ACCT-BRANCH TO WS-LAST-BRANCH
               WHEN ACCTMAST-NOTFND
                   MOVE '000' TO WS-LAST-BRANCH
                   ADD 1 TO WS-NO-ACCT-CNT
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2050-GET-BRANCH' TO AB-PARAGRAPH
                   MOVE ACT-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2050-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TRADES: PRINCIPAL, COMMISSION, FEES.                           *
      * CHARGES CARRIED BY TCB500 IN THE ACTIVITY EXTENSION ARE USED   *
      * WHEN PRESENT, OTHERWISE THE DIFFERENCE BETWEEN NET CASH AND    *
      * PRINCIPAL IS BOOKED AS COMMISSION (CHG08811).                  *
      *----------------------------------------------------------------*
       2100-TRADE-AMOUNTS.
           MOVE ZERO TO WS-PRINCIPAL WS-COMMISSION WS-FEES.
           IF ACT-QTY-CHANGE < ZERO
               COMPUTE WS-ABS-QTY = ACT-QTY-CHANGE * -1
           ELSE
               MOVE ACT-QTY-CHANGE TO WS-ABS-QTY
           END-IF.
           IF ACT-CASH-CHANGE < ZERO
               COMPUTE WS-ABS-CASH = ACT-CASH-CHANGE * -1
           ELSE
               MOVE ACT-CASH-CHANGE TO WS-ABS-CASH
           END-IF.
      *    PRINCIPAL = QTY X PRICE X FACTOR.  WHEN NO PRICE IS
      *    CARRIED FALL BACK TO THE COST (NET) AMOUNT.
           IF ACT-SEC-TYPE = 'CB' OR 'MU' OR 'GV'
               MOVE .0100 TO WS-PRICE-MULT
           ELSE
               MOVE 1.0000 TO WS-PRICE-MULT
           END-IF.
           IF ACT-PRICE > ZERO
               COMPUTE WS-PRINCIPAL ROUNDED =
                   WS-ABS-QTY * ACT-PRICE * WS-PRICE-MULT
           ELSE
               IF ACT-COST-CHANGE < ZERO
                   COMPUTE WS-PRINCIPAL = ACT-COST-CHANGE * -1
               ELSE
                   MOVE ACT-COST-CHANGE TO WS-PRINCIPAL
               END-IF
           END-IF.
           IF WS-PRINCIPAL = ZERO
               MOVE WS-ABS-CASH TO WS-PRINCIPAL
           END-IF.
           IF ACX-COMMISSION NUMERIC AND ACX-FEES NUMERIC
           AND (ACX-COMMISSION NOT = ZERO OR ACX-FEES NOT = ZERO)
               MOVE ACX-COMMISSION TO WS-COMMISSION
               MOVE ACX-FEES       TO WS-FEES
               GO TO 2100-EXIT
           END-IF.
           EVALUATE ACT-TYPE
               WHEN 'BUY' WHEN 'BCV' WHEN 'XSL'
                   COMPUTE WS-CHARGES = WS-ABS-CASH - WS-PRINCIPAL
               WHEN OTHER
                   COMPUTE WS-CHARGES = WS-PRINCIPAL - WS-ABS-CASH
           END-EVALUATE.
           IF WS-CHARGES > ZERO AND WS-ABS-CASH NOT = ZERO
               MOVE WS-CHARGES TO WS-COMMISSION
               ADD 1 TO WS-DERIVED-CHG-CNT
           END-IF.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CORPORATE ACTIONS / ADJUSTMENTS: CASH, ELSE COST               *
      *----------------------------------------------------------------*
       2200-OTHER-AMOUNTS.
           MOVE ZERO TO WS-PRINCIPAL WS-COMMISSION WS-FEES.
           EVALUATE TRUE
               WHEN ACT-CASH-CHANGE NOT = ZERO
                   IF ACT-CASH-CHANGE < ZERO
                       COMPUTE WS-PRINCIPAL = ACT-CASH-CHANGE * -1
                   ELSE
                       MOVE ACT-CASH-CHANGE TO WS-PRINCIPAL
                   END-IF
               WHEN ACT-COST-CHANGE NOT = ZERO
                   IF ACT-COST-CHANGE < ZERO
                       COMPUTE WS-PRINCIPAL = ACT-COST-CHANGE * -1
                   ELSE
                       MOVE ACT-COST-CHANGE TO WS-PRINCIPAL
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       2200-EXIT.
           EXIT.
      *================================================================*
      * END OF A REFERENCE - PROVE IT                                  *
      *================================================================*
       3000-REFERENCE-END.
           IF WS-RT-LINES > ZERO
               ADD 1 TO WS-REFS-OUT
               IF WS-RT-DR NOT = WS-RT-CR
                   ADD 1 TO WS-REFS-UNBAL
                   MOVE 8 TO WS-RETURN-CODE
                   MOVE SPACES           TO GLS-SUMMARY-REC
                   MOVE 'X'              TO GLS-REC-TYPE
                   MOVE DC-BUS-DATE      TO GLS-BUS-DATE
                   MOVE WS-PREV-REF-KEY (1:2)  TO GLS-SOURCE
                   MOVE WS-PREV-REF-KEY (3:16) TO GLS-REF
                   MOVE WS-RT-DR         TO GLS-DR-AMOUNT
                   MOVE WS-RT-CR         TO GLS-CR-AMOUNT
                   PERFORM 8300-WRITE-SUMMARY THRU 8300-EXIT
               END-IF
           END-IF.
           INITIALIZE WS-REF-TOTALS.
       3000-EXIT.
           EXIT.
      *================================================================*
      * ONE DEBIT AND ONE CREDIT LINE FOR WS-LINE-TXN-CODE /           *
      * WS-LINE-AMOUNT                                                 *
      *================================================================*
       4000-BUILD-LINE-PAIR.
           MOVE WS-LINE-TXN-CODE TO WS-MK-TXN.
           MOVE ACT-ACCT-TYPE    TO WS-MK-ACCT-TYPE.
           MOVE ACT-SEC-TYPE     TO WS-MK-SEC-TYPE.
           PERFORM 4100-GET-GL-MAP THRU 4100-EXIT.
           IF NOT MAP-FOUND
               ADD 1 TO WS-UNMAPPED-CNT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               MOVE WS-SUSPENSE-GL TO WS-MR-DR WS-MR-CR
               MOVE SPACES         TO WS-MR-CC
               MOVE 'UNMAPPED - SUSPENSE' TO WS-MR-DESC
               MOVE SPACES           TO GLS-SUMMARY-REC
               MOVE 'U'              TO GLS-REC-TYPE
               MOVE DC-BUS-DATE      TO GLS-BUS-DATE
               MOVE ACT-SOURCE       TO GLS-SOURCE
               MOVE ACT-REF          TO GLS-REF
               MOVE WS-MK-TXN        TO GLS-TXN-CODE
               MOVE WS-MK-ACCT-TYPE  TO GLS-ACCT-TYPE
               MOVE WS-MK-SEC-TYPE   TO GLS-SEC-TYPE
               MOVE WS-LINE-AMOUNT   TO GLS-DR-AMOUNT GLS-CR-AMOUNT
               PERFORM 8300-WRITE-SUMMARY THRU 8300-EXIT
           END-IF.
           MOVE SPACES              TO GLJ-JOURNAL-REC.
           MOVE DC-BUS-DATE         TO GLJ-BUS-DATE.
           MOVE ACT-SOURCE          TO GLJ-SOURCE.
           MOVE ACT-REF             TO GLJ-REF.
           MOVE WS-LINE-TXN-CODE    TO GLJ-TXN-CODE.
           PERFORM 4200-COST-CENTER THRU 4200-EXIT.
           MOVE WS-LINE-AMOUNT      TO GLJ-AMOUNT.
           MOVE ACT-CCY             TO GLJ-CCY.
           IF GLJ-CCY = SPACES
               MOVE 'USD' TO GLJ-CCY
           END-IF.
           PERFORM 4300-USD-AMOUNT THRU 4300-EXIT.
           MOVE ACT-ACCT-NO         TO GLJ-ACCT-NO.
           MOVE ACT-CUSIP           TO GLJ-CUSIP.
           IF WS-MR-DESC NOT = SPACES
               MOVE WS-MR-DESC      TO GLJ-DESC
           ELSE
               MOVE ACT-DESC        TO GLJ-DESC
           END-IF.
           MOVE SPACE               TO GLJ-REVERSAL-FLAG.
      *    ---- DEBIT -----------------------------------------------
           ADD 1 TO WS-RT-LINE-NO.
           MOVE WS-RT-LINE-NO       TO GLJ-LINE-NO.
           MOVE WS-MR-DR            TO GLJ-GL-ACCOUNT.
           MOVE 'D'                 TO GLJ-DR-CR.
           PERFORM 8200-WRITE-GL-LINE THRU 8200-EXIT.
           ADD WS-LINE-AMOUNT TO WS-RT-DR.
      *    ---- CREDIT ----------------------------------------------
           ADD 1 TO WS-RT-LINE-NO.
           MOVE WS-RT-LINE-NO       TO GLJ-LINE-NO.
           MOVE WS-MR-CR            TO GLJ-GL-ACCOUNT.
           MOVE 'C'                 TO GLJ-DR-CR.
           PERFORM 8200-WRITE-GL-LINE THRU 8200-EXIT.
           ADD WS-LINE-AMOUNT TO WS-RT-CR.
           ADD 2 TO WS-RT-LINES WS-GEN-LINES.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GL MAP LOOKUP WITH CACHE.  THE MOST-SPECIFIC-ROW RULE IS IN    *
      * CMD050, NOT HERE.                                              *
      *----------------------------------------------------------------*
       4100-GET-GL-MAP.
           SET MP-IDX TO 1.
           SEARCH WS-MAP-ENTRY
               AT END
                   PERFORM 4150-CALL-CMD050 THRU 4150-EXIT
               WHEN MP-IDX > WS-MAP-USED
                   PERFORM 4150-CALL-CMD050 THRU 4150-EXIT
               WHEN WS-MP-KEY (MP-IDX) = WS-MAP-KEY
                   MOVE WS-MP-FOUND (MP-IDX) TO WS-MR-FOUND
                   MOVE WS-MP-DR (MP-IDX)    TO WS-MR-DR
                   MOVE WS-MP-CR (MP-IDX)    TO WS-MR-CR
                   MOVE WS-MP-CC (MP-IDX)    TO WS-MR-CC
                   MOVE WS-MP-DESC (MP-IDX)  TO WS-MR-DESC
           END-SEARCH.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4150-CALL-CMD050.
      *----------------------------------------------------------------*
           MOVE WS-MK-TXN       TO GM-TXN-CODE.
           MOVE WS-MK-ACCT-TYPE TO GM-ACCT-TYPE.
           MOVE WS-MK-SEC-TYPE  TO GM-SEC-TYPE.
           CALL 'CMD050' USING GM-GLMAP-PARMS.
           ADD 1 TO WS-CMD050-CALLS.
           EVALUATE TRUE
               WHEN GM-FOUND
                   MOVE 'Y'               TO WS-MR-FOUND
                   MOVE GM-DR-GL-ACCOUNT  TO WS-MR-DR
                   MOVE GM-CR-GL-ACCOUNT  TO WS-MR-CR
                   MOVE GM-COST-CENTER    TO WS-MR-CC
                   MOVE GM-DESC           TO WS-MR-DESC
               WHEN GM-NOT-FOUND
                   MOVE 'N'    TO WS-MR-FOUND
                   MOVE SPACES TO WS-MR-DR WS-MR-CR WS-MR-CC
                                  WS-MR-DESC
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '4150-CALL-CMD050' TO AB-PARAGRAPH
                   MOVE GM-SQLCODE TO AB-SQLCODE
                   MOVE WS-MAP-KEY TO AB-KEY
                   MOVE 'CMD050 GL ACCOUNT MAP ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF WS-MAP-USED < 500
               ADD 1 TO WS-MAP-USED
               SET MP-IDX TO WS-MAP-USED
               MOVE WS-MAP-KEY  TO WS-MP-KEY (MP-IDX)
               MOVE WS-MR-FOUND TO WS-MP-FOUND (MP-IDX)
               MOVE WS-MR-DR    TO WS-MP-DR (MP-IDX)
               MOVE WS-MR-CR    TO WS-MP-CR (MP-IDX)
               MOVE WS-MR-CC    TO WS-MP-CC (MP-IDX)
               MOVE WS-MR-DESC  TO WS-MP-DESC (MP-IDX)
           END-IF.
       4150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * COST CENTER = FIRST 3 OF THE MAP COST CENTER + BRANCH          *
      *----------------------------------------------------------------*
       4200-COST-CENTER.
           IF WS-MR-CC = SPACES
               MOVE WS-DEFAULT-CC-PREFIX TO GLJ-COST-CENTER (1:3)
           ELSE
               MOVE WS-MR-CC (1:3)       TO GLJ-COST-CENTER (1:3)
           END-IF.
           MOVE WS-LAST-BRANCH           TO GLJ-COST-CENTER (4:3).
       4200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4300-USD-AMOUNT.
      *----------------------------------------------------------------*
           IF GLJ-CCY = 'USD'
               MOVE GLJ-AMOUNT TO GLJ-AMOUNT-USD
               GO TO 4300-EXIT
           END-IF.
           MOVE GLJ-CCY     TO FX-FROM-CCY.
           MOVE 'USD'       TO FX-TO-CCY.
           MOVE DC-BUS-DATE TO FX-RATE-DATE.
           MOVE GLJ-AMOUNT  TO FX-AMOUNT-IN.
           CALL 'CMU040' USING FX-CONVERT-PARMS.
           EVALUATE TRUE
               WHEN FX-OK
               WHEN FX-STALE-RATE
                   MOVE FX-AMOUNT-OUT TO GLJ-AMOUNT-USD
               WHEN FX-RATE-NOT-FOUND
                   ADD 1 TO WS-FX-ERR-CNT
                   MOVE ZERO TO GLJ-AMOUNT-USD
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '4300-USD-AMOUNT' TO AB-PARAGRAPH
                   MOVE GLJ-CCY TO AB-KEY
                   MOVE FX-MESSAGE TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       4300-EXIT.
           EXIT.
      *================================================================*
      * CA DIVIDEND ACCRUALS - COPIED UNCHANGED, BALANCE PROVED PER    *
      * REFERENCE (THE FILE IS WRITTEN BY CAB500 IN REFERENCE ORDER)   *
      *================================================================*
       5000-MERGE-ACCRUALS.
           INITIALIZE WS-REF-TOTALS.
           MOVE LOW-VALUES TO WS-PREV-REF-KEY.
           PERFORM 8100-READ-ACCRUAL THRU 8100-EXIT.
           PERFORM UNTIL END-OF-ACCRUALS
               ADD 1 TO WS-ACCR-READ
               MOVE GLJ-SOURCE TO WS-CRK-SOURCE
               MOVE GLJ-REF    TO WS-CRK-REF
               IF WS-CUR-REF-KEY NOT = WS-PREV-REF-KEY
                   IF WS-PREV-REF-KEY NOT = LOW-VALUES
                       PERFORM 5100-ACCRUAL-REF-END THRU 5100-EXIT
                   END-IF
                   MOVE WS-CUR-REF-KEY TO WS-PREV-REF-KEY
               END-IF
               ADD GLJ-AMOUNT TO WS-ACCR-AMT-HASH
               PERFORM 8200-WRITE-GL-LINE THRU 8200-EXIT
               IF GLJ-DEBIT
                   ADD GLJ-AMOUNT TO WS-RT-DR
               ELSE
                   ADD GLJ-AMOUNT TO WS-RT-CR
               END-IF
               ADD 1 TO WS-RT-LINES
               PERFORM 8100-READ-ACCRUAL THRU 8100-EXIT
           END-PERFORM.
           IF WS-PREV-REF-KEY NOT = LOW-VALUES
               PERFORM 5100-ACCRUAL-REF-END THRU 5100-EXIT
           END-IF.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5100-ACCRUAL-REF-END.
      *----------------------------------------------------------------*
           ADD 1 TO WS-REFS-OUT.
           IF WS-RT-DR NOT = WS-RT-CR
               ADD 1 TO WS-REFS-UNBAL
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               DISPLAY 'SRB600 ACCRUAL REF OUT OF BALANCE '
                       WS-PREV-REF-KEY
               MOVE SPACES           TO GLS-SUMMARY-REC
               MOVE 'X'              TO GLS-REC-TYPE
               MOVE DC-BUS-DATE      TO GLS-BUS-DATE
               MOVE WS-PREV-REF-KEY (1:2)  TO GLS-SOURCE
               MOVE WS-PREV-REF-KEY (3:16) TO GLS-REF
               MOVE 'CACR'           TO GLS-TXN-CODE
               MOVE WS-RT-DR         TO GLS-DR-AMOUNT
               MOVE WS-RT-CR         TO GLS-CR-AMOUNT
               PERFORM 8300-WRITE-SUMMARY THRU 8300-EXIT
           END-IF.
           INITIALIZE WS-REF-TOTALS.
       5100-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-JOURNAL.
           READ JRNLIN-FILE INTO PSJ-JOURNAL-REC.
           EVALUATE TRUE
               WHEN JRNLIN-OK
                   CONTINUE
               WHEN JRNLIN-EOF
                   MOVE 'Y' TO WS-JRNL-EOF-SW
               WHEN OTHER
                   MOVE 'JRNLIN' TO AB-DDNAME
                   MOVE WS-JRNLIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-JOURNAL' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-ACCRUAL.
      *----------------------------------------------------------------*
           READ ACCRIN-FILE INTO GLJ-JOURNAL-REC.
           EVALUATE TRUE
               WHEN ACCRIN-OK
                   IF GLJ-AMOUNT NOT NUMERIC
                       MOVE 'ACCRIN' TO AB-DDNAME
                       MOVE 1008 TO AB-ABEND-CODE
                       MOVE '8100-READ-ACCRUAL' TO AB-PARAGRAPH
                       MOVE GLJ-REF TO AB-KEY
                       MOVE 'ACCRUAL AMOUNT NOT NUMERIC' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   END-IF
               WHEN ACCRIN-EOF
                   MOVE 'Y' TO WS-ACCR-EOF-SW
               WHEN OTHER
                   MOVE 'ACCRIN' TO AB-DDNAME
                   MOVE WS-ACCRIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-ACCRUAL' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-WRITE-GL-LINE.
      *----------------------------------------------------------------*
           WRITE GLJOUT-REC FROM GLJ-JOURNAL-REC.
           IF WS-GLJOUT-STATUS NOT = '00'
               MOVE 'GLJOUT' TO AB-DDNAME
               MOVE WS-GLJOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-WRITE-GL-LINE' TO AB-PARAGRAPH
               MOVE GLJ-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-LINES-OUT.
           IF GLJ-DEBIT
               ADD GLJ-AMOUNT     TO WS-TOTAL-DR
               ADD GLJ-AMOUNT-USD TO WS-TOTAL-DR-USD
           ELSE
               ADD GLJ-AMOUNT     TO WS-TOTAL-CR
           END-IF.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-WRITE-SUMMARY.
      *----------------------------------------------------------------*
           WRITE GLSUMM-REC FROM GLS-SUMMARY-REC.
           IF WS-GLSUMM-STATUS NOT = '00'
               MOVE 'GLSUMM' TO AB-DDNAME
               MOVE WS-GLSUMM-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8300-WRITE-SUMMARY' TO AB-PARAGRAPH
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-SUMM-CNT.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB600'       TO CT-STAGE.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '8500-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8500-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           IF WS-TOTAL-DR NOT = WS-TOTAL-CR
               MOVE 8 TO WS-RETURN-CODE
               DISPLAY 'SRB600 *** GL JOURNAL OUT OF BALANCE ***'
           END-IF.
      *    ---- RUN TOTALS RECORD - ALWAYS LAST --------------------
           MOVE SPACES           TO GLS-SUMMARY-REC.
           MOVE 'T'              TO GLS-REC-TYPE.
           MOVE DC-BUS-DATE      TO GLS-BUS-DATE.
           MOVE WS-JRNL-READ     TO GLS-JRNL-IN.
           MOVE WS-ACCR-READ     TO GLS-ACCR-IN.
           MOVE WS-LINES-OUT     TO GLS-LINES-OUT.
           MOVE WS-REFS-OUT      TO GLS-REFS-OUT.
           MOVE WS-REFS-UNBAL    TO GLS-REFS-UNBAL.
           MOVE WS-UNMAPPED-CNT  TO GLS-UNMAPPED-CNT.
           MOVE WS-TOTAL-DR      TO GLS-TOTAL-DR.
           MOVE WS-TOTAL-CR      TO GLS-TOTAL-CR.
           PERFORM 8300-WRITE-SUMMARY THRU 8300-EXIT.
           CLOSE JRNLIN-FILE ACCRIN-FILE ACCTMAST-FILE.
           CLOSE GLJOUT-FILE.
           IF WS-GLJOUT-STATUS NOT = '00'
               MOVE 'GLJOUT' TO AB-DDNAME
               MOVE WS-GLJOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE GLSUMM-FILE.
           MOVE 'JRNL-IN'        TO CT-COUNTER-NAME.
           MOVE WS-JRNL-READ     TO CT-COUNT.
           MOVE WS-JRNL-CASH-HASH TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'ACCRUAL-IN'     TO CT-COUNTER-NAME.
           MOVE WS-ACCR-READ     TO CT-COUNT.
           MOVE WS-ACCR-AMT-HASH TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'GLJ-OUT'        TO CT-COUNTER-NAME.
           MOVE WS-LINES-OUT     TO CT-COUNT.
           MOVE WS-TOTAL-DR      TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'GLJ-CREDITS'    TO CT-COUNTER-NAME.
           MOVE WS-LINES-OUT     TO CT-COUNT.
           MOVE WS-TOTAL-CR      TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'GLJ-UNBALANCED' TO CT-COUNTER-NAME.
           MOVE WS-REFS-UNBAL    TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'GLJ-UNMAPPED'   TO CT-COUNTER-NAME.
           MOVE WS-UNMAPPED-CNT  TO CT-COUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* SRB600 - GL JOURNAL BUILD                    *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-JRNL-READ TO WS-DISP-CNT.
           DISPLAY ' JOURNAL LEGS READ        : ' WS-DISP-CNT.
           MOVE WS-JRNL-STREET TO WS-DISP-CNT.
           DISPLAY '   STREET (MEMO) LEGS     : ' WS-DISP-CNT.
           MOVE WS-JRNL-NO-AMT TO WS-DISP-CNT.
           DISPLAY '   NO AMOUNT              : ' WS-DISP-CNT.
           MOVE WS-GEN-LINES TO WS-DISP-CNT.
           DISPLAY ' GL LINES GENERATED       : ' WS-DISP-CNT.
           MOVE WS-COMM-LINES TO WS-DISP-CNT.
           DISPLAY '   COMMISSION PAIRS       : ' WS-DISP-CNT.
           MOVE WS-FEE-LINES TO WS-DISP-CNT.
           DISPLAY '   FEE PAIRS              : ' WS-DISP-CNT.
           MOVE WS-DERIVED-CHG-CNT TO WS-DISP-CNT.
           DISPLAY '   CHARGES DERIVED        : ' WS-DISP-CNT.
           MOVE WS-ACCR-READ TO WS-DISP-CNT.
           DISPLAY ' ACCRUAL LINES MERGED     : ' WS-DISP-CNT.
           MOVE WS-LINES-OUT TO WS-DISP-CNT.
           DISPLAY ' GL LINES WRITTEN         : ' WS-DISP-CNT.
           MOVE WS-REFS-OUT TO WS-DISP-CNT.
           DISPLAY ' REFERENCES               : ' WS-DISP-CNT.
           MOVE WS-REFS-UNBAL TO WS-DISP-CNT.
           DISPLAY ' REFERENCES OUT OF BALANCE: ' WS-DISP-CNT.
           MOVE WS-UNMAPPED-CNT TO WS-DISP-CNT.
           DISPLAY ' UNMAPPED (SUSPENSE)      : ' WS-DISP-CNT.
           MOVE WS-NO-ACCT-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS NOT ON MASTER   : ' WS-DISP-CNT.
           MOVE WS-FX-ERR-CNT TO WS-DISP-CNT.
           DISPLAY ' FX RATES MISSING         : ' WS-DISP-CNT.
           MOVE WS-CMD050-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMD050 CALLS             : ' WS-DISP-CNT.
           MOVE WS-TOTAL-DR TO WS-DISP-AMT.
           DISPLAY ' TOTAL DEBITS             : ' WS-DISP-AMT.
           MOVE WS-TOTAL-CR TO WS-DISP-AMT.
           DISPLAY ' TOTAL CREDITS            : ' WS-DISP-AMT.
           MOVE WS-TOTAL-DR-USD TO WS-DISP-AMT.
           DISPLAY ' TOTAL DEBITS USD         : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
                   MOVE 'GL JOURNAL OUT OF BALANCE' TO AU-MESSAGE
               WHEN WS-RETURN-CODE = 4
                   MOVE 'W' TO AU-SEVERITY
                   MOVE 'GL JOURNAL BUILD ENDED WITH WARNINGS'
                                   TO AU-MESSAGE
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
                   MOVE 'GL JOURNAL BUILD ENDED' TO AU-MESSAGE
           END-EVALUATE.
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
           DISPLAY 'SRB600 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB600 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB600 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
