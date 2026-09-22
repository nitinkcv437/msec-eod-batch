       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB400.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  APRIL 1996.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB400                                            *
      * DESCRIPTION: DAILY POSITION VALUATION.                         *
      *              READS THE POSITION MASTER SEQUENTIALLY AND        *
      *              REVALUES EVERY POSITION IN PLACE:                 *
      *                PRICE     - CMD020 'GETL' (LATEST ON OR BEFORE  *
      *                            THE BUSINESS DATE); STALE WHEN THE  *
      *                            PRICE DATE IS BEFORE THE PREVIOUS   *
      *                            BUSINESS DAY                        *
      *                MKT VALUE - QTY X PRICE X PRICE FACTOR          *
      *                USD VALUE - CMU040                              *
      *                UNREALIZED P&L = MKT VALUE - COST BASIS         *
      *                ACCRUED   - CMU030 FOR FIXED INCOME (INFO ONLY) *
      *              ONE VALUATION DETAIL RECORD IS WRITTEN FOR EVERY  *
      *              OPEN CUSTOMER / FIRM POSITION.                    *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD050 / STEP010  (IKJEFT01 - DB2 PLAN MSSRPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              ACCTMAST - ACCOUNT MASTER KSDS (BRANCH / REP)     *
      * IN/OUT     : POSMAST  - MSEC.PROD.SR.POSITION.KSDS    (SRPOSN) *
      * OUTPUT     : VALOUT   - MSEC.PROD.SR.VALUATION(+1)   (SRVALUE) *
      * CALLS      : CMD010, CMD020, CMU030, CMU040, CMU050, CMU060,   *
      *              CMU080, CMASM01                                   *
      * RETURN CODE: 0 CLEAN, 4 STALE / MISSING PRICES OR FX RATES     *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1996-04-22 DWB  ORIGINAL - PRICES FROM DB2            CHG02215 *
      * 1996-11-18 DWB  MUTUAL FUNDS PRICED WITH BONDS (NAV)  CHG02488 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 1999-03-08 TLM  REPAIR OF UNINITIALIZED VALUE FIELDS  CHG04655 *
      * 2001-07-16 KAP  DECIMALIZATION - PRICE 8 DECIMALS     CHG08811 *
      * 2003-06-02 KAP  ACCRUED INTEREST VIA CMU030           CHG11244 *
      * 2009-12-14 SPA  USD VALUE VIA CMU040                  CHG19002 *
      * 2011-08-29 SPA  STALE PRICE WARNING RC 4              CHG22190 *
      * 2014-01-13 SPA  SECURITY / PRICE CACHE                CHG25507 *
      * 2016-03-21 MHC  ACCRUED ESTIMATE FOR SHORT 1ST CPN    CHG29921 *
      * 2024-05-20 NVR  T+1 - ACCRUED TO NEXT BUSINESS DAY    CHG40551 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT POSMAST-FILE   ASSIGN TO POSMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS POS-KEY
                  FILE STATUS IS WS-POSMAST-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCTMAST-KEY
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT VALOUT-FILE    ASSIGN TO VALOUT
                  FILE STATUS IS WS-VALOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  POSMAST-FILE.
       COPY SRPOSN.
       FD  ACCTMAST-FILE.
       01  ACCTMAST-REC.
           05  ACCTMAST-KEY            PIC X(10).
           05  FILLER                  PIC X(290).
       FD  VALOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  VALOUT-REC                  PIC X(200).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB400'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-POSMAST-STATUS       PIC X(02)  VALUE '00'.
               88  POSMAST-OK                     VALUE '00' '02'.
               88  POSMAST-EOF                    VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-VALOUT-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-POSITIONS               VALUE 'Y'.
           05  WS-SEC-FOUND-SW         PIC X(01)  VALUE 'N'.
               88  SECURITY-FOUND                 VALUE 'Y'.
           05  WS-PRICE-FOUND-SW       PIC X(01)  VALUE 'N'.
               88  PRICE-FOUND                    VALUE 'Y'.
           05  WS-STALE-SW             PIC X(01)  VALUE 'N'.
               88  PRICE-IS-STALE                 VALUE 'Y'.
           05  WS-REPAIRED-SW          PIC X(01)  VALUE 'N'.
               88  RECORD-REPAIRED                VALUE 'Y'.
           05  WS-MATURED-SW           PIC X(01)  VALUE 'N'.
               88  BOND-MATURED                   VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * VALUATION WORK FIELDS                                          *
      *----------------------------------------------------------------*
       01  WS-VALUATION-WORK.
           05  WS-VAL-QTY              PIC S9(11)V9(04) COMP-3.
           05  WS-VAL-PRICE            PIC S9(09)V9(08) COMP-3.
           05  WS-VAL-PRICE-DATE       PIC 9(08).
           05  WS-PRICE-FACTOR         PIC S9(05)V9(04) COMP-3.
           05  WS-UNIT-PRICE           PIC S9(09)V9(06) COMP-3.
           05  WS-MKT-VALUE            PIC S9(15)V99    COMP-3.
           05  WS-FX-RATE              PIC S9(05)V9(08) COMP-3.
           05  WS-MKT-VALUE-USD        PIC S9(15)V99    COMP-3.
           05  WS-UNRLZD-PL            PIC S9(15)V99    COMP-3.
           05  WS-ACCRUED              PIC S9(13)V99    COMP-3.
           05  WS-ACCR-EST             PIC S9(13)V99    COMP-3.
           05  WS-FACE                 PIC S9(13)V99    COMP-3.
           05  WS-VAL-CCY              PIC X(03).
       01  WS-PAR-PRICE                PIC S9(09)V9(08) COMP-3
                                                     VALUE +100.
       01  WS-BOND-FACTOR-DEFAULT      PIC S9(05)V9(04) COMP-3
                                                     VALUE +.0100.
       01  WS-EQUITY-FACTOR-DEFAULT    PIC S9(05)V9(04) COMP-3
                                                     VALUE +1.0000.
       01  WS-ACCR-BASIS-DAYS          PIC S9(03)       COMP-3
                                                     VALUE +360.
      *----------------------------------------------------------------*
      * SECURITY / PRICE CACHE (CHG25507).  POSITIONS ARE IN ACCOUNT   *
      * ORDER SO THE SAME CUSIP COMES BACK MANY TIMES.                 *
      *----------------------------------------------------------------*
       01  WS-CACHE-AREA.
           05  WS-CACHE-USED           PIC S9(04) COMP  VALUE ZERO.
           05  WS-CACHE-MAX            PIC S9(04) COMP  VALUE +3000.
           05  WS-CACHE-ENTRY OCCURS 3000 TIMES INDEXED BY CA-IDX.
               10  WS-CE-CUSIP         PIC X(09).
               10  WS-CE-SEC-FOUND     PIC X(01).
               10  WS-CE-SEC-TYPE      PIC X(02).
               10  WS-CE-SEC-CCY       PIC X(03).
               10  WS-CE-FACTOR        PIC S9(05)V9(04) COMP-3.
               10  WS-CE-TYPE-DATA     PIC X(60).
               10  WS-CE-PRICE-FOUND   PIC X(01).
               10  WS-CE-PRICE         PIC S9(09)V9(08) COMP-3.
               10  WS-CE-PRICE-DATE    PIC 9(08).
               10  WS-CE-PRICE-CCY     PIC X(03).
       01  WS-LAST-ACCT                PIC X(10)  VALUE LOW-VALUES.
       01  WS-LAST-ACCT-FOUND          PIC X(01)  VALUE 'N'.
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-VALUED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAT-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-VALOUT-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STREET-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STALE-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-PRICE-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-SEC-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-FX-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STALE-FX-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-ACCT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REPAIR-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MATURED-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCR-OK-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCR-EST-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCR-ERR-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OTHER-TYPE-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EQ-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FI-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMD010-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMD020-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMU040-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-TOT-MV-USD           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-OWNER-MV-USD     PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-UNRLZD           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-ACCRUED          PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-QTY              PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-ABS-QTY              PIC S9(11)V9(04) COMP-3.
       COPY SRVALUE.
       COPY CMACCT.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMSECLNK.
       COPY CMPRLNK.
       COPY CMFXLNK.
       COPY CMAILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMJILNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-POSITION THRU 2000-EXIT
               UNTIL END-OF-POSITIONS.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           CALL 'CMASM01' USING JI-JOB-INFO.
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
           OR DC-PREV-BUS-DATE NOT NUMERIC
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
           MOVE 'POSITION VALUATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN I-O POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
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
           OPEN OUTPUT VALOUT-FILE.
           IF WS-VALOUT-STATUS NOT = '00'
               MOVE 'VALOUT' TO AB-DDNAME
               MOVE WS-VALOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-POSITION THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * ONE POSITION                                                   *
      *================================================================*
       2000-PROCESS-POSITION.
           ADD 1 TO WS-READ-CNT.
           PERFORM 2100-REPAIR-PACKED-FIELDS THRU 2100-EXIT.
           IF POS-TD-QTY = ZERO
               PERFORM 2150-ZERO-VALUATION THRU 2150-EXIT
               GO TO 2000-REWRITE
           END-IF.
           MOVE POS-TD-QTY TO WS-VAL-QTY.
           PERFORM 2200-GET-SECURITY THRU 2200-EXIT.
           PERFORM 2300-GET-PRICE THRU 2300-EXIT.
           PERFORM 2400-VALUE-BY-TYPE THRU 2400-EXIT.
           PERFORM 2500-CONVERT-USD THRU 2500-EXIT.
           COMPUTE WS-UNRLZD-PL = WS-MKT-VALUE - POS-COST-BASIS.
           MOVE WS-VAL-PRICE      TO POS-MKT-PRICE.
           MOVE WS-VAL-PRICE-DATE TO POS-PRICE-DATE.
           MOVE WS-MKT-VALUE      TO POS-MKT-VALUE.
           MOVE WS-FX-RATE        TO POS-FX-RATE.
           MOVE WS-MKT-VALUE-USD  TO POS-MKT-VALUE-USD.
           MOVE WS-UNRLZD-PL      TO POS-UNRLZD-PL.
           ADD 1 TO WS-VALUED-CNT.
           ADD WS-MKT-VALUE-USD TO WS-TOT-MV-USD.
           IF POS-ACCT-TYPE = 'ST'
               ADD 1 TO WS-STREET-CNT
           ELSE
               PERFORM 2800-WRITE-VALUATION THRU 2800-EXIT
           END-IF.
       2000-REWRITE.
           PERFORM 8100-REWRITE-POSITION THRU 8100-EXIT.
           PERFORM 8000-READ-POSITION THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 1999-03-08 TLM - NEW POSITIONS ARRIVE FROM SRB200 AND THE      *
      * CONVERSION LOADS WITH THE VALUATION FIELDS NOT SET.  MAKE THEM *
      * ZERO BEFORE ANY ARITHMETIC (S0C7).                             *
      *----------------------------------------------------------------*
       2100-REPAIR-PACKED-FIELDS.
           MOVE 'N' TO WS-REPAIRED-SW.
           IF POS-MKT-PRICE NOT NUMERIC
               MOVE ZERO TO POS-MKT-PRICE
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-PRICE-DATE NOT NUMERIC
               MOVE ZERO TO POS-PRICE-DATE
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-MKT-VALUE NOT NUMERIC
               MOVE ZERO TO POS-MKT-VALUE
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-FX-RATE NOT NUMERIC
               MOVE ZERO TO POS-FX-RATE
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-MKT-VALUE-USD NOT NUMERIC
               MOVE ZERO TO POS-MKT-VALUE-USD
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-UNRLZD-PL NOT NUMERIC
               MOVE ZERO TO POS-UNRLZD-PL
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-COST-BASIS NOT NUMERIC
               MOVE ZERO TO POS-COST-BASIS
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-AVG-COST NOT NUMERIC
               MOVE ZERO TO POS-AVG-COST
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-REALIZED-PL-YTD NOT NUMERIC
               MOVE ZERO TO POS-REALIZED-PL-YTD
               MOVE 'Y' TO WS-REPAIRED-SW
           END-IF.
           IF POS-TD-QTY NOT NUMERIC
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2100-REPAIR-PACKED-FIELDS' TO AB-PARAGRAPH
               MOVE POS-KEY TO AB-KEY
               MOVE 'TRADE DATE QUANTITY NOT NUMERIC' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF RECORD-REPAIRED
               ADD 1 TO WS-REPAIR-CNT
           END-IF.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2150-ZERO-VALUATION.
      *----------------------------------------------------------------*
           ADD 1 TO WS-FLAT-CNT.
           MOVE ZERO TO POS-MKT-VALUE POS-MKT-VALUE-USD POS-UNRLZD-PL.
           IF POS-FX-RATE = ZERO
               MOVE 1 TO POS-FX-RATE
           END-IF.
       2150-EXIT.
           EXIT.
      *================================================================*
      * SECURITY MASTER (CACHED)                                       *
      *================================================================*
       2200-GET-SECURITY.
           MOVE 'N' TO WS-SEC-FOUND-SW.
           SET CA-IDX TO 1.
           SEARCH WS-CACHE-ENTRY
               AT END
                   PERFORM 2210-LOAD-SECURITY THRU 2210-EXIT
               WHEN CA-IDX > WS-CACHE-USED
                   PERFORM 2210-LOAD-SECURITY THRU 2210-EXIT
               WHEN WS-CE-CUSIP (CA-IDX) = POS-CUSIP
                   CONTINUE
           END-SEARCH.
           INITIALIZE SEC-MASTER-REC.
           MOVE POS-CUSIP TO SEC-CUSIP.
           IF WS-CE-SEC-FOUND (CA-IDX) = 'Y'
               MOVE 'Y'                     TO WS-SEC-FOUND-SW
               MOVE WS-CE-SEC-TYPE (CA-IDX)  TO SEC-TYPE
               MOVE WS-CE-SEC-CCY (CA-IDX)   TO SEC-CCY
               MOVE WS-CE-FACTOR (CA-IDX)    TO SEC-PRICE-FACTOR
               MOVE WS-CE-TYPE-DATA (CA-IDX) TO SEC-TYPE-DATA
           ELSE
               ADD 1 TO WS-NO-SEC-CNT
               MOVE POS-SEC-TYPE TO SEC-TYPE
               MOVE POS-CCY      TO SEC-CCY
               MOVE ZERO         TO SEC-PRICE-FACTOR
           END-IF.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * NOT IN CACHE - CMD010 AND CMD020, THEN ADD.  IF THE CACHE IS   *
      * FULL THE LAST SLOT IS REUSED.                                  *
      *----------------------------------------------------------------*
       2210-LOAD-SECURITY.
           IF WS-CACHE-USED < WS-CACHE-MAX
               ADD 1 TO WS-CACHE-USED
           END-IF.
           SET CA-IDX TO WS-CACHE-USED.
           MOVE POS-CUSIP TO WS-CE-CUSIP (CA-IDX).
           MOVE 'GET '    TO SL-FUNCTION.
           MOVE POS-CUSIP TO SL-KEY-CUSIP.
           MOVE SPACES    TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           ADD 1 TO WS-CMD010-CALLS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA TO SEC-MASTER-REC
                   MOVE 'Y'              TO WS-CE-SEC-FOUND (CA-IDX)
                   MOVE SEC-TYPE         TO WS-CE-SEC-TYPE (CA-IDX)
                   MOVE SEC-CCY          TO WS-CE-SEC-CCY (CA-IDX)
                   MOVE SEC-TYPE-DATA    TO WS-CE-TYPE-DATA (CA-IDX)
                   IF SEC-PRICE-FACTOR NUMERIC
                       MOVE SEC-PRICE-FACTOR TO WS-CE-FACTOR (CA-IDX)
                   ELSE
                       MOVE ZERO TO WS-CE-FACTOR (CA-IDX)
                   END-IF
               WHEN SL-NOT-FOUND
                   MOVE 'N'    TO WS-CE-SEC-FOUND (CA-IDX)
                   MOVE SPACES TO WS-CE-SEC-TYPE (CA-IDX)
                                  WS-CE-SEC-CCY (CA-IDX)
                                  WS-CE-TYPE-DATA (CA-IDX)
                   MOVE ZERO   TO WS-CE-FACTOR (CA-IDX)
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '2210-LOAD-SECURITY' TO AB-PARAGRAPH
                   MOVE SL-SQLCODE TO AB-SQLCODE
                   MOVE POS-CUSIP TO AB-KEY
                   MOVE 'CMD010 SECURITY MASTER ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           MOVE 'GETL'      TO PL-FUNCTION.
           MOVE POS-CUSIP   TO PL-CUSIP.
           MOVE DC-BUS-DATE TO PL-PRICE-DATE.
           CALL 'CMD020' USING PL-PRICE-PARMS.
           ADD 1 TO WS-CMD020-CALLS.
           EVALUATE TRUE
               WHEN PL-FOUND
                   MOVE 'Y'            TO WS-CE-PRICE-FOUND (CA-IDX)
                   MOVE PL-PRICE       TO WS-CE-PRICE (CA-IDX)
                   MOVE PL-ACTUAL-DATE TO WS-CE-PRICE-DATE (CA-IDX)
                   MOVE PL-PRICE-CCY   TO WS-CE-PRICE-CCY (CA-IDX)
               WHEN PL-NOT-FOUND
                   MOVE 'N'  TO WS-CE-PRICE-FOUND (CA-IDX)
                   MOVE ZERO TO WS-CE-PRICE (CA-IDX)
                                WS-CE-PRICE-DATE (CA-IDX)
                   MOVE SPACES TO WS-CE-PRICE-CCY (CA-IDX)
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '2210-LOAD-SECURITY' TO AB-PARAGRAPH
                   MOVE PL-SQLCODE TO AB-SQLCODE
                   MOVE POS-CUSIP TO AB-KEY
                   MOVE 'CMD020 SECURITY PRICE ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2210-EXIT.
           EXIT.
      *================================================================*
      * PRICE AND STALE TEST                                           *
      *================================================================*
       2300-GET-PRICE.
           MOVE 'N' TO WS-PRICE-FOUND-SW WS-STALE-SW.
           IF WS-CE-PRICE-FOUND (CA-IDX) = 'Y'
               MOVE 'Y' TO WS-PRICE-FOUND-SW
               MOVE WS-CE-PRICE (CA-IDX)      TO WS-VAL-PRICE
               MOVE WS-CE-PRICE-DATE (CA-IDX) TO WS-VAL-PRICE-DATE
               IF WS-VAL-PRICE-DATE < DC-PREV-BUS-DATE
                   MOVE 'Y' TO WS-STALE-SW
               END-IF
           ELSE
      *        NO PRICE ON FILE - CARRY YESTERDAY'S PRICE, FLAG STALE
               ADD 1 TO WS-NO-PRICE-CNT
               MOVE POS-MKT-PRICE  TO WS-VAL-PRICE
               MOVE POS-PRICE-DATE TO WS-VAL-PRICE-DATE
               MOVE 'Y' TO WS-STALE-SW
           END-IF.
           IF PRICE-IS-STALE
               ADD 1 TO WS-STALE-CNT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           IF SEC-CCY = SPACES OR LOW-VALUES
               MOVE POS-CCY TO WS-VAL-CCY
           ELSE
               MOVE SEC-CCY TO WS-VAL-CCY
           END-IF.
           IF WS-VAL-CCY = SPACES
               MOVE 'USD' TO WS-VAL-CCY
           END-IF.
       2300-EXIT.
           EXIT.
      *================================================================*
      * VALUE BY SECURITY TYPE                                         *
      *================================================================*
       2400-VALUE-BY-TYPE.
           MOVE ZERO TO WS-ACCRUED WS-MKT-VALUE.
           MOVE 'N'  TO WS-MATURED-SW.
           EVALUATE SEC-TYPE
               WHEN 'EQ'
               WHEN 'PF'
               WHEN 'AD'
                   ADD 1 TO WS-EQ-CNT
                   PERFORM 3100-VALUE-EQUITY THRU 3100-EXIT
               WHEN 'MF'
               WHEN 'CB'
               WHEN 'MU'
               WHEN 'GV'
                   ADD 1 TO WS-FI-CNT
                   PERFORM 3200-VALUE-FIXED-INCOME THRU 3200-EXIT
               WHEN OTHER
                   ADD 1 TO WS-OTHER-TYPE-CNT
                   DISPLAY 'SRB400 UNKNOWN SECURITY TYPE ' SEC-TYPE
                           ' FOR ' POS-CUSIP ' - VALUED AS EQUITY'
                   PERFORM 3100-VALUE-EQUITY THRU 3100-EXIT
           END-EVALUATE.
       2400-EXIT.
           EXIT.
      *================================================================*
      * FX TO USD                                                      *
      *================================================================*
       2500-CONVERT-USD.
           IF WS-VAL-CCY = 'USD'
               MOVE 1            TO WS-FX-RATE
               MOVE WS-MKT-VALUE TO WS-MKT-VALUE-USD
               GO TO 2500-EXIT
           END-IF.
           MOVE WS-VAL-CCY   TO FX-FROM-CCY.
           MOVE 'USD'        TO FX-TO-CCY.
           MOVE DC-BUS-DATE  TO FX-RATE-DATE.
           MOVE WS-MKT-VALUE TO FX-AMOUNT-IN.
           CALL 'CMU040' USING FX-CONVERT-PARMS.
           ADD 1 TO WS-CMU040-CALLS.
           EVALUATE TRUE
               WHEN FX-OK
                   MOVE FX-RATE       TO WS-FX-RATE
                   MOVE FX-AMOUNT-OUT TO WS-MKT-VALUE-USD
               WHEN FX-STALE-RATE
                   ADD 1 TO WS-STALE-FX-CNT
                   MOVE FX-RATE       TO WS-FX-RATE
                   MOVE FX-AMOUNT-OUT TO WS-MKT-VALUE-USD
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN FX-RATE-NOT-FOUND
                   ADD 1 TO WS-NO-FX-CNT
                   MOVE ZERO TO WS-FX-RATE WS-MKT-VALUE-USD
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
                   DISPLAY 'SRB400 NO FX RATE FOR ' WS-VAL-CCY
                           ' POSITION ' POS-KEY
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '2500-CONVERT-USD' TO AB-PARAGRAPH
                   MOVE WS-VAL-CCY TO AB-KEY
                   MOVE FX-MESSAGE TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2500-EXIT.
           EXIT.
      *================================================================*
      * VALUATION DETAIL FOR CUSTOMER / FIRM POSITIONS                 *
      *================================================================*
       2800-WRITE-VALUATION.
           PERFORM 2850-GET-ACCOUNT THRU 2850-EXIT.
           MOVE SPACES            TO VAL-VALUATION-REC.
           MOVE DC-BUS-DATE       TO VAL-BUS-DATE.
           MOVE POS-ACCT-NO       TO VAL-ACCT-NO.
           MOVE POS-CUSIP         TO VAL-CUSIP.
           MOVE POS-LOCATION      TO VAL-LOCATION.
           IF WS-LAST-ACCT-FOUND = 'Y'
               MOVE ACCT-BRANCH   TO VAL-BRANCH
               MOVE ACCT-REP      TO VAL-REP
           ELSE
               MOVE '???'         TO VAL-BRANCH
               MOVE '????'        TO VAL-REP
           END-IF.
           MOVE POS-ACCT-TYPE     TO VAL-ACCT-TYPE.
           MOVE SEC-TYPE          TO VAL-SEC-TYPE.
           MOVE WS-VAL-CCY        TO VAL-CCY.
           MOVE WS-VAL-QTY        TO VAL-QTY.
           MOVE WS-VAL-PRICE      TO VAL-PRICE.
           MOVE WS-VAL-PRICE-DATE TO VAL-PRICE-DATE.
           MOVE WS-STALE-SW       TO VAL-PRICE-STALE-FLAG.
           MOVE WS-PRICE-FACTOR   TO VAL-PRICE-FACTOR.
           MOVE WS-MKT-VALUE      TO VAL-MKT-VALUE.
           MOVE WS-FX-RATE        TO VAL-FX-RATE.
           MOVE WS-MKT-VALUE-USD  TO VAL-MKT-VALUE-USD.
           MOVE POS-COST-BASIS    TO VAL-COST-BASIS.
           MOVE WS-UNRLZD-PL      TO VAL-UNRLZD-PL.
           MOVE WS-ACCRUED        TO VAL-ACCRUED-INT.
           WRITE VALOUT-REC FROM VAL-VALUATION-REC.
           IF WS-VALOUT-STATUS NOT = '00'
               MOVE 'VALOUT' TO AB-DDNAME
               MOVE WS-VALOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2800-WRITE-VALUATION' TO AB-PARAGRAPH
               MOVE POS-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-VALOUT-CNT.
           ADD WS-MKT-VALUE-USD TO WS-TOT-OWNER-MV-USD.
           ADD WS-UNRLZD-PL     TO WS-TOT-UNRLZD.
           ADD WS-ACCRUED       TO WS-TOT-ACCRUED.
           IF WS-VAL-QTY < ZERO
               COMPUTE WS-ABS-QTY = WS-VAL-QTY * -1
           ELSE
               MOVE WS-VAL-QTY TO WS-ABS-QTY
           END-IF.
           ADD WS-ABS-QTY TO WS-TOT-QTY.
       2800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2850-GET-ACCOUNT.
      *----------------------------------------------------------------*
           IF POS-ACCT-NO = WS-LAST-ACCT
               GO TO 2850-EXIT
           END-IF.
           MOVE POS-ACCT-NO TO WS-LAST-ACCT ACCTMAST-KEY.
           READ ACCTMAST-FILE INTO ACCT-MASTER-REC.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE 'Y' TO WS-LAST-ACCT-FOUND
               WHEN ACCTMAST-NOTFND
                   MOVE 'N' TO WS-LAST-ACCT-FOUND
                   ADD 1 TO WS-NO-ACCT-CNT
                   DISPLAY 'SRB400 POSITION ACCOUNT NOT ON MASTER '
                           POS-ACCT-NO
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2850-GET-ACCOUNT' TO AB-PARAGRAPH
                   MOVE POS-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2850-EXIT.
           EXIT.
      *================================================================*
      * EQUITY-LIKE: PRICE PER SHARE X FACTOR (PREFERREDS MAY BE .04)  *
      *================================================================*
       3100-VALUE-EQUITY.
           MOVE SEC-PRICE-FACTOR TO WS-PRICE-FACTOR.
           IF WS-PRICE-FACTOR = ZERO
               MOVE WS-EQUITY-FACTOR-DEFAULT TO WS-PRICE-FACTOR
           END-IF.
           PERFORM 3150-CALC-MARKET-VALUE THRU 3150-EXIT.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MARKET VALUE.  UNIT PRICE CARRIED TO 6 DECIMALS (CHG08811).    *
      *----------------------------------------------------------------*
       3150-CALC-MARKET-VALUE.
           COMPUTE WS-UNIT-PRICE = WS-VAL-PRICE * WS-PRICE-FACTOR.
           COMPUTE WS-MKT-VALUE ROUNDED = WS-VAL-QTY * WS-UNIT-PRICE
               ON SIZE ERROR
                   DISPLAY 'SRB400 MARKET VALUE OVERFLOW ' POS-KEY
                   MOVE ZERO TO WS-MKT-VALUE
           END-COMPUTE.
       3150-EXIT.
           EXIT.
      *================================================================*
      * FIXED INCOME AND FUNDS: PRICE IN PERCENT OF PAR (FACTOR .01),  *
      * MATURED BONDS AT PAR, ACCRUED INTEREST FROM CMU030.            *
      *================================================================*
       3200-VALUE-FIXED-INCOME.
           MOVE SEC-PRICE-FACTOR TO WS-PRICE-FACTOR.
           IF WS-PRICE-FACTOR = ZERO
               MOVE WS-BOND-FACTOR-DEFAULT TO WS-PRICE-FACTOR
           END-IF.
           IF SEC-MATURITY-DATE NUMERIC
               IF SEC-MATURITY-DATE > ZERO
               AND SEC-MATURITY-DATE < DC-BUS-DATE
                   MOVE 'Y' TO WS-MATURED-SW
                   ADD 1 TO WS-MATURED-CNT
                   MOVE WS-PAR-PRICE TO WS-VAL-PRICE
               END-IF
           END-IF.
           PERFORM 3150-CALC-MARKET-VALUE THRU 3150-EXIT.
           IF NOT BOND-MATURED
               PERFORM 3300-ACCRUED-INTEREST THRU 3300-EXIT
           END-IF.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ACCRUED INTEREST TO NEXT BUSINESS DAY (T+1 SETTLEMENT)         *
      *----------------------------------------------------------------*
       3300-ACCRUED-INTEREST.
           INITIALIZE AI-ACCRUAL-PARMS.
           MOVE SEC-DAYCOUNT       TO AI-DAYCOUNT.
           IF SEC-COUPON-RATE NUMERIC
               MOVE SEC-COUPON-RATE TO AI-COUPON-RATE
           ELSE
               MOVE ZERO TO AI-COUPON-RATE
           END-IF.
           IF SEC-COUPON-FREQ NUMERIC
               MOVE SEC-COUPON-FREQ TO AI-COUPON-FREQ
           END-IF.
           IF SEC-MATURITY-DATE NUMERIC
               MOVE SEC-MATURITY-DATE TO AI-MATURITY-DATE
           END-IF.
           IF SEC-ISSUE-DATE NUMERIC
               MOVE SEC-ISSUE-DATE TO AI-ISSUE-DATE
           END-IF.
           IF SEC-FIRST-CPN-DATE NUMERIC
               MOVE SEC-FIRST-CPN-DATE TO AI-FIRST-CPN-DATE
           END-IF.
           MOVE DC-NEXT-BUS-DATE   TO AI-SETTLE-DATE.
           MOVE WS-VAL-QTY         TO WS-FACE.
           MOVE WS-FACE            TO AI-FACE-AMOUNT.
           MOVE ZERO               TO AI-LAST-CPN-DATE
                                      AI-NEXT-CPN-DATE.
           CALL 'CMU030' USING AI-ACCRUAL-PARMS.
           EVALUATE TRUE
               WHEN AI-OK
                   ADD 1 TO WS-ACCR-OK-CNT
                   MOVE AI-ACCRUED-AMOUNT TO WS-ACCRUED
      *            2016-03-21 MHC SHORT FIRST COUPON RETURNS ZERO
                   IF WS-ACCRUED = ZERO AND AI-ACCRUAL-DAYS > ZERO
                       COMPUTE WS-ACCR-EST ROUNDED =
                           WS-FACE * AI-COUPON-RATE / 100
                           * AI-ACCRUAL-DAYS / WS-ACCR-BASIS-DAYS
                       MOVE WS-ACCR-EST TO WS-ACCRUED
                       ADD 1 TO WS-ACCR-EST-CNT
                   END-IF
               WHEN AI-AFTER-MATURITY
                   MOVE ZERO TO WS-ACCRUED
               WHEN OTHER
                   ADD 1 TO WS-ACCR-ERR-CNT
                   MOVE ZERO TO WS-ACCRUED
           END-EVALUATE.
       3300-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-POSITION.
           READ POSMAST-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   CONTINUE
               WHEN POSMAST-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-POSITION' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-REWRITE-POSITION.
      *----------------------------------------------------------------*
           REWRITE POS-POSITION-REC.
           IF NOT POSMAST-OK
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8100-REWRITE-POSITION' TO AB-PARAGRAPH
               MOVE POS-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB400'       TO CT-STAGE.
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
           CLOSE POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE ACCTMAST-FILE VALOUT-FILE.
           IF WS-VALOUT-STATUS NOT = '00'
               MOVE 'VALOUT' TO AB-DDNAME
               MOVE WS-VALOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'POSN-READ'      TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT      TO CT-COUNT.
           MOVE WS-TOT-MV-USD    TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'VALUE-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-VALOUT-CNT    TO CT-COUNT.
           MOVE WS-TOT-OWNER-MV-USD TO CT-AMOUNT.
           MOVE WS-TOT-QTY       TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'STALE-PRICE'    TO CT-COUNTER-NAME.
           MOVE WS-STALE-CNT     TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'NO-PRICE'       TO CT-COUNTER-NAME.
           MOVE WS-NO-PRICE-CNT  TO CT-COUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* SRB400 - POSITION VALUATION                  *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' STALE IF PRICED BEFORE   : ' DC-PREV-BUS-DATE.
           MOVE WS-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS READ           : ' WS-DISP-CNT.
           MOVE WS-VALUED-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS VALUED         : ' WS-DISP-CNT.
           MOVE WS-FLAT-CNT TO WS-DISP-CNT.
           DISPLAY ' ZERO QUANTITY            : ' WS-DISP-CNT.
           MOVE WS-STREET-CNT TO WS-DISP-CNT.
           DISPLAY ' STREET POSITIONS         : ' WS-DISP-CNT.
           MOVE WS-VALOUT-CNT TO WS-DISP-CNT.
           DISPLAY ' VALUATION RECORDS OUT    : ' WS-DISP-CNT.
           MOVE WS-EQ-CNT TO WS-DISP-CNT.
           DISPLAY '   EQUITY PRICING         : ' WS-DISP-CNT.
           MOVE WS-FI-CNT TO WS-DISP-CNT.
           DISPLAY '   FIXED INCOME / FUNDS   : ' WS-DISP-CNT.
           MOVE WS-OTHER-TYPE-CNT TO WS-DISP-CNT.
           DISPLAY '   UNKNOWN TYPE           : ' WS-DISP-CNT.
           MOVE WS-MATURED-CNT TO WS-DISP-CNT.
           DISPLAY '   MATURED AT PAR         : ' WS-DISP-CNT.
           MOVE WS-STALE-CNT TO WS-DISP-CNT.
           DISPLAY ' STALE PRICES             : ' WS-DISP-CNT.
           MOVE WS-NO-PRICE-CNT TO WS-DISP-CNT.
           DISPLAY ' NO PRICE ON FILE         : ' WS-DISP-CNT.
           MOVE WS-NO-SEC-CNT TO WS-DISP-CNT.
           DISPLAY ' NOT ON SECURITY MASTER   : ' WS-DISP-CNT.
           MOVE WS-NO-FX-CNT TO WS-DISP-CNT.
           DISPLAY ' NO FX RATE               : ' WS-DISP-CNT.
           MOVE WS-STALE-FX-CNT TO WS-DISP-CNT.
           DISPLAY ' STALE FX RATE            : ' WS-DISP-CNT.
           MOVE WS-NO-ACCT-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNT NOT ON MASTER    : ' WS-DISP-CNT.
           MOVE WS-REPAIR-CNT TO WS-DISP-CNT.
           DISPLAY ' RECORDS REPAIRED (S0C7)  : ' WS-DISP-CNT.
           MOVE WS-ACCR-OK-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCRUED CALCULATED       : ' WS-DISP-CNT.
           MOVE WS-ACCR-EST-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCRUED ESTIMATED        : ' WS-DISP-CNT.
           MOVE WS-ACCR-ERR-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCRUED ERRORS           : ' WS-DISP-CNT.
           MOVE WS-CMD010-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMD010 CALLS             : ' WS-DISP-CNT.
           MOVE WS-CMD020-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMD020 CALLS             : ' WS-DISP-CNT.
           MOVE WS-CMU040-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMU040 CALLS             : ' WS-DISP-CNT.
           MOVE WS-TOT-OWNER-MV-USD TO WS-DISP-AMT.
           DISPLAY ' OWNER MARKET VALUE USD   : ' WS-DISP-AMT.
           MOVE WS-TOT-UNRLZD TO WS-DISP-AMT.
           DISPLAY ' UNREALIZED P&L (LOCAL)   : ' WS-DISP-AMT.
           MOVE WS-TOT-ACCRUED TO WS-DISP-AMT.
           DISPLAY ' ACCRUED INTEREST         : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'POSITION VALUATION ENDED' TO AU-MESSAGE.
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
           DISPLAY 'SRB400 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB400 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB400 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
