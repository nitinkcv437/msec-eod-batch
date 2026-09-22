       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCB200.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/02/1988.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCB200                                            *
      * TITLE      : TRADE ENRICHMENT                                  *
      * JOB        : MSTCD040   STEP020 (IKJEFT01 - DB2 PLAN MSTCPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   READS THE MERGED, SORTED VALIDATED TRADES FROM ALL FEEDS     *
      *   (OMS, BOND DESK, MANUAL) AND COMPLETES EACH TRADE:           *
      *     - SECURITY ATTRIBUTES (SECURITY MASTER VIA CMD010, CACHED) *
      *     - ACCOUNT ATTRIBUTES  (BRANCH, REP, ACCOUNT TYPE)          *
      *     - SETTLEMENT DATE     (CMU020) WHEN NOT SUPPLIED           *
      *     - PRINCIPAL           QTY X PRICE X PRICE FACTOR           *
      *     - ACCRUED INTEREST    (CMU030) FOR FIXED INCOME            *
      *     - COMMISSION          (TCU210) UNLESS OVERRIDDEN           *
      *     - REGULATORY FEES     (TCU22E / TCU22F - SEE 3600)         *
      *     - NET AMOUNT          BUY  = PRIN + COMM + FEES + ACCRUED  *
      *                           SELL = PRIN - COMM - FEES + ACCRUED  *
      *     - FX RATE / USD NET   (CMU040) FOR NON-USD TRADES          *
      *     - SETTLEMENT LOCATION FROM SECURITY DEPOSITORY             *
      *   TRADES THAT CANNOT BE ENRICHED ARE WRITTEN TO THE REJECT     *
      *   FILE WITH AN E0XX CODE.  WARNINGS (W0XX) GO TO THE SAME      *
      *   FILE WITH SEVERITY W AND THE TRADE CONTINUES.                *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETER CARDS TCP200A                     *
      *          TRADEIN   MSEC.PROD.TC.TRADES.SORTED(+1) (TCTRADE)    *
      *                    SORTED ACCT / CUSIP (SEE MSTCD040 STEP010)  *
      *          ACCTMAST  MSEC.PROD.CM.ACCTMAST.KSDS     (CMACCT)     *
      * OUTPUT : TRADEOUT  MSEC.PROD.TC.TRADES.ENRICHED(+1) (TCTRADE)  *
      *          REJOUT    MSEC.PROD.TC.REJECTS.ENR(+1)   (TCREJCT)    *
      * CALLS  : CMD010 CMU010 CMU020 CMU030 CMU040 CMU050 CMU060      *
      *          CMU080 CMASM02 TCU210 TCU22E TCU22F                   *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 REJECTS / WARNINGS                    *
      *                                                                *
      * RESTART: RERUN THE WHOLE JOB MSTCD040 (SORT + ENRICHMENT).     *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1988-03-02 RJK            ORIGINAL - EQUITY ONLY, T+5          *
      * 1988-11-14 RJK            SEC FEE (CALL TCU220)                *
      * 1989-06-05 RJK            COMMISSION MOVED TO TCU210           *
      * 1990-02-19 RJK            BOND TRADES - ACCRUED INTEREST       *
      * 1991-05-06 RJK            BOND DESK FEED                       *
      * 1993-06-07 DWB            SETTLEMENT T+5 TO T+3 (CHG00790)     *
      * 1995-06-01 DWB  CHG01950  T+3 NOW FROM CMU020 SETTLE MODULE    *
      * 1995-07-17 DWB  CHG01877  OMS FEED REPLACES EXECUTION TAPE     *
      * 1996-04-22 DWB  CHG02215  SECURITY MASTER TO DB2 - CMD010      *
      * 1996-05-13 DWB  CHG02240  SECURITY CACHE (DB2 CPU)             *
      * 1998-11-02 TLM  CHG04471  Y2K - ALL DATES CCYYMMDD             *
      * 1999-01-04 TLM  CHG04588  EURO LEGACY CURRENCY CONVERSION      *
      * 1999-06-28 TLM  CHG04910  Y2K TEST DATE PARAMETER              *
      * 2001-04-09 KAP  CHG08814  DECIMALIZATION - FRACTIONS REMOVED   *
      * 2002-03-08 KAP  CHG09930  TAF FEE, REJECT SEVERITY             *
      * 2004-06-07 KAP  CHG12011  COMMISSION 5 PCT POLICY WARNING      *
      * 2004-10-18 KAP  CHG12670  WARNINGS TO REJECT FILE              *
      * 2005-07-11 KAP  CHG13520  REG SHO - SHORT SALE LOCATE FLAG     *
      * 2006-03-20 KAP  CHG14790  LARGE TRADE LIST FOR COMPLIANCE      *
      * 2005-02-14 KAP  CHG13002  ACCRUED OVERRIDE FROM BOND DESK      *
      * 2007-01-22 KAP  CHG16202  COMMISSION OVERRIDE FROM OMS         *
      * 2009-12-14 SPA  CHG19002  FX TO USD VIA CMU040, USD NET        *
      * 2011-11-14 SPA  CHG22418  FEE MODULE SPLIT TCU22E / TCU22F     *
      * 2012-05-07 SPA  CHG23112  PRINCIPAL CAPACITY - NO COMMISSION   *
      * 2013-10-21 SPA  CHG25390  SECURITY CACHE 500 TO 2000           *
      * 2016-10-03 SPA  CHG30112  HOUSE ACCOUNTS NOT CHARGED           *
      * 2019-03-11 NVR  CHG35510  SEQUENCE CHECK NOW A WARNING         *
      * 2022-09-12 NVR  CHG39740  EUROCLEAR LOCATION FOR NON-USD       *
      * 2024-02-12 NVR  CHG41007  T+1 - SETTLE DATE FROM CMU020 ONLY   *
      * 2024-05-20 NVR  CHG41388  T+1 GO-LIVE 2024-05-28               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD      ASSIGN TO SYSIN
                                FILE STATUS IS WS-PARMCARD-FS.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT TRADEIN-FILE  ASSIGN TO TRADEIN
                                FILE STATUS IS WS-TRADEIN-FS.
           SELECT ACCTMAST-FILE ASSIGN TO ACCTMAST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS RANDOM
                                RECORD KEY IS ACCT-NO
                                FILE STATUS IS WS-ACCTMAST-FS.
           SELECT TRADEOUT-FILE ASSIGN TO TRADEOUT
                                FILE STATUS IS WS-TRADEOUT-FS.
           SELECT REJOUT-FILE   ASSIGN TO REJOUT
                                FILE STATUS IS WS-REJOUT-FS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  TRADEIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRADEIN-REC                 PIC X(400).
      *
       FD  ACCTMAST-FILE.
       COPY CMACCT.
      *
       FD  TRADEOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRADEOUT-REC                PIC X(400).
      *
       FD  REJOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJOUT-REC                  PIC X(450).
      *
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)  VALUE
           'TCB200 WORKING STORAGE BEGINS'.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCB200'.
      *
      *----------------------------------------------------------------*
      * FILE STATUS                                                    *
      *----------------------------------------------------------------*
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
               88  PARMCARD-OK                   VALUE '00'.
           05  WS-DATECARD-FS          PIC X(02).
               88  DATECARD-OK                   VALUE '00'.
           05  WS-TRADEIN-FS           PIC X(02).
               88  TRADEIN-OK                    VALUE '00'.
               88  TRADEIN-EOF                   VALUE '10'.
           05  WS-ACCTMAST-FS          PIC X(02).
               88  ACCTMAST-OK                   VALUE '00'.
               88  ACCTMAST-NOTFND               VALUE '23'.
           05  WS-TRADEOUT-FS          PIC X(02).
               88  TRADEOUT-OK                   VALUE '00'.
           05  WS-REJOUT-FS            PIC X(02).
               88  REJOUT-OK                     VALUE '00'.
      *
      *----------------------------------------------------------------*
      * SWITCHES                                                       *
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-TRADES              VALUE 'Y'.
           05  WS-TRADE-STATUS-SW      PIC X(01)  VALUE 'G'.
               88  WS-TRADE-GOOD                 VALUE 'G'.
               88  WS-TRADE-REJECTED             VALUE 'R'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-FIRST-TRADE-SW       PIC X(01)  VALUE 'Y'.
               88  WS-FIRST-TRADE                VALUE 'Y'.
           05  WS-SEC-CACHE-FULL-SW    PIC X(01)  VALUE 'N'.
               88  WS-SEC-CACHE-FULL             VALUE 'Y'.
           05  WS-SEC-CACHE-HIT-SW     PIC X(01)  VALUE 'N'.
               88  WS-SEC-CACHE-HIT              VALUE 'Y'.
      *    PARAMETER DRIVEN
           05  WS-FEE-MODULE-SW        PIC X(01)  VALUE 'Y'.
               88  WS-FEES-ACTIVE                VALUE 'Y'.
           05  WS-SEQ-CHECK-SW         PIC X(01)  VALUE 'Y'.
               88  WS-SEQ-CHECK-ON               VALUE 'Y'.
           05  WS-TRACE-SW             PIC X(01)  VALUE 'N'.
               88  WS-TRACE-ON                   VALUE 'Y'.
           05  WS-CACHE-SW             PIC X(01)  VALUE 'Y'.
               88  WS-CACHE-ON                   VALUE 'Y'.
      *    EURO LEGACY CURRENCIES (DEM FRF ITL NLG BEF ESP) - THE
      *    CONVERSION WAS ONLY NEEDED THROUGH THE 2002 CHANGEOVER.
           05  WS-EURO-CONV-SW         PIC X(01)  VALUE 'N'.
               88  WS-EURO-CONV-ON               VALUE 'Y'.
      *    PRE-DECIMALIZATION PRICE FRACTIONS (256THS) - CHG08814
           05  WS-FRACTION-PRICE-SW    PIC X(01)  VALUE 'N'.
               88  WS-FRACTION-PRICES            VALUE 'Y'.
      *    Y2K CLOCK TEST - FORCES THE SETTLEMENT BASE DATE
           05  WS-Y2K-TEST-SW          PIC X(01)  VALUE 'N'.
               88  WS-Y2K-TEST-ON                VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * PARAMETERS (TCP200A)                                           *
      *----------------------------------------------------------------*
       01  WS-PARAMETERS.
           05  WS-PARM-SEQ-WARN-MAX    PIC 9(03)  VALUE 010.
           05  WS-PARM-Y2K-TEST-DATE   PIC 9(08)  VALUE ZERO.
           05  WS-PARM-TRACE-LIMIT     PIC 9(05)  VALUE 00100.
           05  WS-PARM-COMM-PCT-LIMIT  PIC 9(02)V99 VALUE 05.00.
           05  WS-PARM-COMM-PCT-X      REDEFINES WS-PARM-COMM-PCT-LIMIT
                                       PIC X(04).
           05  WS-PARM-LARGE-TRADE     PIC 9(11)  VALUE 01000000.
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(30).
           05  WS-PARM-VALUE           PIC X(30).
      *
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-TRADES-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRADES-WRITTEN       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRADES-REJECTED      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WARNINGS-WRITTEN     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEQ-ERRORS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-TRADES           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANCEL-TRADES        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CORRECT-TRADES       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETTLE-COMPUTED      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETTLE-SUPPLIED      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCRUED-COMPUTED     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCRUED-OVERRIDES    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COMM-COMPUTED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COMM-OVERRIDES       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COMM-HOUSE           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FEE-CALLS-E          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FEE-CALLS-F          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-CONVERSIONS       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-STALE             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-DB-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-CACHE-HITS       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-READS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-REUSED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRACE-COUNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SHORT-WARNINGS       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COMM-PCT-WARNINGS    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LARGE-TRADES         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CROSS-CCY-TRADES     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SHORT-IN-CASH-ACCT   PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HOUSE-TRADES         PIC S9(09) COMP-3 VALUE ZERO.
      *
       01  WS-TOTALS.
           05  WS-IN-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-IN-AMT-HASH          PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-NET-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-USD-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-REJ-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-TOT-PRINCIPAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-TOT-COMMISSION       PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-TOT-SEC-FEE          PIC S9(13)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-TOT-TAF-FEE          PIC S9(13)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-TOT-OTHER-FEES       PIC S9(13)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-TOT-ACCRUED          PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * STATISTICS BY SOURCE AND SECURITY TYPE                         *
      *----------------------------------------------------------------*
       01  WS-SOURCE-STATS-VALUES.
           05  FILLER                  PIC X(03)  VALUE 'OMS'.
           05  FILLER                  PIC X(22)  VALUE LOW-VALUES.
           05  FILLER                  PIC X(03)  VALUE 'FIX'.
           05  FILLER                  PIC X(22)  VALUE LOW-VALUES.
           05  FILLER                  PIC X(03)  VALUE 'MAN'.
           05  FILLER                  PIC X(22)  VALUE LOW-VALUES.
           05  FILLER                  PIC X(03)  VALUE '???'.
           05  FILLER                  PIC X(22)  VALUE LOW-VALUES.
       01  WS-SOURCE-STATS REDEFINES WS-SOURCE-STATS-VALUES.
           05  WS-SRC-ENTRY            OCCURS 4 TIMES
                                       INDEXED BY WS-SRC-IDX.
               10  WS-SRC-CODE         PIC X(03).
               10  WS-SRC-IN           PIC S9(09) COMP-3.
               10  WS-SRC-OUT          PIC S9(09) COMP-3.
               10  WS-SRC-REJ          PIC S9(09) COMP-3.
               10  WS-SRC-NET          PIC S9(11)V99 COMP-3.
      *
       01  WS-SECTYPE-STATS.
           05  WS-STY-ENTRY            OCCURS 10 TIMES
                                       INDEXED BY WS-STY-IDX.
               10  WS-STY-CODE         PIC X(02).
               10  WS-STY-COUNT        PIC S9(09) COMP-3.
               10  WS-STY-PRINCIPAL    PIC S9(15)V99 COMP-3.
       01  WS-STY-USED                 PIC S9(04) COMP VALUE ZERO.
      *
       01  WS-CCY-STATS.
           05  WS-CST-ENTRY            OCCURS 12 TIMES
                                       INDEXED BY WS-CST-IDX.
               10  WS-CST-CCY          PIC X(03).
               10  WS-CST-COUNT        PIC S9(09) COMP-3.
               10  WS-CST-NET          PIC S9(15)V99 COMP-3.
               10  WS-CST-USD          PIC S9(15)V99 COMP-3.
       01  WS-CST-USED                 PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * LARGE TRADE LIST (COMPLIANCE REQUEST 2006) - TEN LARGEST BY    *
      * USD NET, KEPT IN DESCENDING ORDER, DISPLAYED AT END OF JOB.    *
      *----------------------------------------------------------------*
       01  WS-LARGE-TRADE-TABLE.
           05  WS-LT-ENTRY             OCCURS 10 TIMES
                                       INDEXED BY WS-LT-IDX.
               10  WS-LT-TRADE-ID      PIC X(16).
               10  WS-LT-ACCT-NO       PIC X(10).
               10  WS-LT-CUSIP         PIC X(09).
               10  WS-LT-SIDE          PIC X(02).
               10  WS-LT-USD-NET       PIC S9(15)V99 COMP-3.
       01  WS-LT-USED                  PIC S9(04) COMP VALUE ZERO.
       01  WS-LT-SUB                   PIC S9(04) COMP.
       01  WS-LT-INS                   PIC S9(04) COMP.
       01  WS-LT-ABS                   PIC S9(15)V99 COMP-3.
      *
      *----------------------------------------------------------------*
      * SECURITY CACHE.  LOADED ON DEMAND FROM CMD010.  ENTRIES ARE    *
      * ADDED IN ARRIVAL ORDER (INPUT IS IN ACCOUNT ORDER, NOT CUSIP)  *
      * SO THE TABLE IS SEARCHED SERIALLY.  UNUSED SLOTS HIGH-VALUES.  *
      *----------------------------------------------------------------*
       01  WS-SEC-CACHE-MAX            PIC S9(04) COMP VALUE +2000.
       01  WS-SEC-CACHE-COUNT          PIC S9(04) COMP VALUE ZERO.
       01  WS-SEC-CACHE.
           05  WS-SC-ENTRY             OCCURS 2000 TIMES
                                       INDEXED BY WS-SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-SEC-DATA      PIC X(250).
      *
      *----------------------------------------------------------------*
      * SEQUENCE CHECK                                                 *
      *----------------------------------------------------------------*
       01  WS-SEQ-KEYS.
           05  WS-PREV-KEY.
               10  WS-PREV-ACCT        PIC X(10)  VALUE LOW-VALUES.
               10  WS-PREV-CUSIP       PIC X(09)  VALUE LOW-VALUES.
           05  WS-CURR-KEY.
               10  WS-CURR-ACCT        PIC X(10).
               10  WS-CURR-CUSIP       PIC X(09).
      *
      *----------------------------------------------------------------*
      * ACCOUNT HOLD AREA (LAST ACCOUNT READ - INPUT IS ACCT ORDER)    *
      *----------------------------------------------------------------*
       01  WS-LAST-ACCT-NO             PIC X(10)  VALUE LOW-VALUES.
       01  WS-LAST-ACCT-FOUND          PIC X(01)  VALUE 'N'.
           88  WS-LAST-ACCT-ON-FILE              VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * FEE MODULE NAME - BUILT AT RUN TIME FROM THE SECURITY TYPE     *
      *----------------------------------------------------------------*
       01  WS-FEE-PGM                  PIC X(08)  VALUE SPACES.
       01  WS-FEE-SUFFIX               PIC X(01)  VALUE SPACE.
           88  WS-FEE-EQUITY                     VALUE 'E'.
           88  WS-FEE-FIXED-INCOME               VALUE 'F'.
      *
      *----------------------------------------------------------------*
      * WORK FIELDS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-REJ-CODE             PIC X(04).
           05  WS-REJ-TEXT             PIC X(60).
           05  WS-WARN-CODE            PIC X(04).
           05  WS-WARN-TEXT            PIC X(60).
           05  WS-TOTAL-FEES           PIC S9(11)V99    COMP-3.
           05  WS-NET-WORK             PIC S9(15)V99    COMP-3.
           05  WS-PRIN-WORK            PIC S9(15)V99    COMP-3.
           05  WS-AMT-WORK             PIC S9(15)V99    COMP-3.
           05  WS-FACE-WORK            PIC S9(13)V99    COMP-3.
           05  WS-ENRICH-TS            PIC X(26).
           05  WS-SETTLE-BASE-DATE     PIC 9(08).
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-SUB                  PIC S9(04) COMP.
           05  WS-COMM-PCT             PIC S9(05)V9(04) COMP-3.
      *
      *    OBSOLETE - FRACTIONAL PRICE WORK AREA (PRE 2001)
       01  WS-FRACTION-WORK.
           05  WS-FRAC-WHOLE           PIC 9(05).
           05  WS-FRAC-NUMER           PIC 9(03).
           05  WS-FRAC-DENOM           PIC 9(03).
           05  WS-FRAC-PRICE           PIC S9(05)V9(08) COMP-3.
      *
      *    OBSOLETE - EURO LEGACY CURRENCY FIXED RATES (1999-01-01)
       01  WS-EURO-LEGACY-VALUES.
           05  FILLER  PIC X(13)  VALUE 'DEM0019558300'.
           05  FILLER  PIC X(13)  VALUE 'FRF0065595700'.
           05  FILLER  PIC X(13)  VALUE 'ITL1936270000'.
           05  FILLER  PIC X(13)  VALUE 'NLG0022037100'.
           05  FILLER  PIC X(13)  VALUE 'BEF0403399000'.
           05  FILLER  PIC X(13)  VALUE 'ESP1663860000'.
       01  WS-EURO-LEGACY-TABLE REDEFINES WS-EURO-LEGACY-VALUES.
           05  WS-EURO-LEG-ENTRY       OCCURS 6 TIMES
                                       INDEXED BY WS-EURO-IDX.
               10  WS-EURO-LEG-CCY     PIC X(03).
               10  WS-EURO-LEG-RATE    PIC 9(04)V9(06).
      *
      *----------------------------------------------------------------*
      * DISPLAY FIELDS                                                 *
      *----------------------------------------------------------------*
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-DISP-QTY             PIC ZZZ,ZZZ,ZZ9.9999-.
           05  WS-DISP-RATE            PIC ZZZZ9.99999999-.
      *
      *----------------------------------------------------------------*
      * RECORD AREAS                                                   *
      *----------------------------------------------------------------*
       COPY CMDATEW.
       COPY TCTRADE.
       COPY TCREJCT.
       COPY CMSECMS.
      *
      *----------------------------------------------------------------*
      * CALL INTERFACES                                                *
      *----------------------------------------------------------------*
       COPY CMSECLNK.
       COPY CMSTLNK.
       COPY CMAILNK.
       COPY CMFXLNK.
       COPY CMDTLNK.
       COPY TCCOMLNK.
       COPY TCFEELNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
      *
       01  FILLER                      PIC X(32)  VALUE
           'TCB200 WORKING STORAGE ENDS'.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-TRADE  THRU 2000-EXIT
               UNTIL WS-END-OF-TRADES
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
      * 1000 - INITIALIZATION                                          *
      *================================================================*
       1000-INITIALIZE.
           PERFORM 1010-READ-DATE-CARD THRU 1010-EXIT
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'TRADE ENRICHMENT STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           PERFORM 1100-READ-PARAMETERS THRU 1100-EXIT
           PERFORM 1200-OPEN-FILES     THRU 1200-EXIT
      *
           MOVE HIGH-VALUES            TO WS-SEC-CACHE
           MOVE ZERO                   TO WS-SEC-CACHE-COUNT
           PERFORM 1300-INIT-STATS     THRU 1300-EXIT
      *
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP           TO WS-ENRICH-TS
      *
           IF WS-Y2K-TEST-ON
               MOVE WS-PARM-Y2K-TEST-DATE TO WS-SETTLE-BASE-DATE
               DISPLAY 'TCB200 - *** Y2K TEST DATE IN EFFECT '
                       WS-SETTLE-BASE-DATE ' ***'
           ELSE
               MOVE DC-BUS-DATE        TO WS-SETTLE-BASE-DATE
           END-IF
      *
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1010-READ-DATE-CARD.
           OPEN INPUT DATECARD-FILE
           IF NOT DATECARD-OK
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF NOT DATECARD-OK
           OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1010-READ-DATE-CARD' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           CLOSE DATECARD-FILE
           IF NOT DATECARD-OK
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           DISPLAY 'TCB200 - BUSINESS DATE ' DC-BUS-DATE
                   ' CYCLE ' DC-CYCLE-TYPE
                   ' RUN ' DC-RUN-NUMBER.
       1010-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1100 - PARAMETERS  KEYWORD=VALUE                               *
      *   FEE-MODULES=Y|N     SEQUENCE-CHECK=Y|N    TRACE=Y|N          *
      *   TRACE-LIMIT=NNNNN   SEQ-WARN-MAX=NNN      SEC-CACHE=Y|N      *
      *   Y2K-TEST-DATE=CCYYMMDD  (TEST REGION ONLY)                   *
      *----------------------------------------------------------------*
       1100-READ-PARAMETERS.
           OPEN INPUT PARMCARD
           IF NOT PARMCARD-OK
               DISPLAY 'TCB200 - NO SYSIN PARAMETERS - DEFAULTS USED'
               GO TO 1100-EXIT
           END-IF
           PERFORM 1110-READ-PARM-CARD THRU 1110-EXIT
               UNTIL WS-PARM-EOF
           CLOSE PARMCARD
           DISPLAY 'TCB200 - FEE MODULES      : ' WS-FEE-MODULE-SW
           DISPLAY 'TCB200 - SEQUENCE CHECK   : ' WS-SEQ-CHECK-SW
           DISPLAY 'TCB200 - SECURITY CACHE   : ' WS-CACHE-SW
           DISPLAY 'TCB200 - TRACE            : ' WS-TRACE-SW
           DISPLAY 'TCB200 - COMM PCT LIMIT   : ' WS-PARM-COMM-PCT-LIMIT
           DISPLAY 'TCB200 - LARGE TRADE USD  : ' WS-PARM-LARGE-TRADE.
       1100-EXIT.
           EXIT.
      *
       1110-READ-PARM-CARD.
           READ PARMCARD
               AT END
                   SET WS-PARM-EOF     TO TRUE
                   GO TO 1110-EXIT
           END-READ
           IF PARM-CARD-REC(1:1) = '*'
           OR PARM-CARD-REC = SPACES
               GO TO 1110-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD
                                          WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'FEE-MODULES'
                   MOVE WS-PARM-VALUE(1:1) TO WS-FEE-MODULE-SW
               WHEN 'SEQUENCE-CHECK'
                   MOVE WS-PARM-VALUE(1:1) TO WS-SEQ-CHECK-SW
               WHEN 'TRACE'
                   MOVE WS-PARM-VALUE(1:1) TO WS-TRACE-SW
               WHEN 'TRACE-LIMIT'
                   IF WS-PARM-VALUE(1:5) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:5) TO WS-PARM-TRACE-LIMIT
                   END-IF
               WHEN 'SEQ-WARN-MAX'
                   IF WS-PARM-VALUE(1:3) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:3) TO WS-PARM-SEQ-WARN-MAX
                   END-IF
               WHEN 'SEC-CACHE'
                   MOVE WS-PARM-VALUE(1:1) TO WS-CACHE-SW
               WHEN 'COMM-PCT-LIMIT'
                   IF WS-PARM-VALUE(1:4) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:4)
                                       TO WS-PARM-COMM-PCT-X
                   END-IF
               WHEN 'LARGE-TRADE-USD'
                   IF WS-PARM-VALUE(1:11) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:11)
                                       TO WS-PARM-LARGE-TRADE
                   END-IF
               WHEN 'Y2K-TEST-DATE'
                   IF WS-PARM-VALUE(1:8) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:8)
                                       TO WS-PARM-Y2K-TEST-DATE
                       SET WS-Y2K-TEST-ON TO TRUE
                   END-IF
      *        WHEN 'EURO-CONVERSION'
      *            MOVE WS-PARM-VALUE(1:1) TO WS-EURO-CONV-SW
               WHEN OTHER
                   DISPLAY 'TCB200 - UNKNOWN PARAMETER IGNORED: '
                           PARM-CARD-REC(1:40)
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *
       1200-OPEN-FILES.
           OPEN INPUT TRADEIN-FILE
           IF NOT TRADEIN-OK
               MOVE 'TRADEIN'          TO AB-DDNAME
               MOVE WS-TRADEIN-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN INPUT ACCTMAST-FILE
           IF NOT ACCTMAST-OK
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT TRADEOUT-FILE
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT REJOUT-FILE
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF.
       1200-EXIT.
           EXIT.
      *
       1300-INIT-STATS.
           PERFORM VARYING WS-SRC-IDX FROM 1 BY 1
                   UNTIL WS-SRC-IDX > 4
               MOVE ZERO               TO WS-SRC-IN (WS-SRC-IDX)
                                          WS-SRC-OUT (WS-SRC-IDX)
                                          WS-SRC-REJ (WS-SRC-IDX)
                                          WS-SRC-NET (WS-SRC-IDX)
           END-PERFORM
           PERFORM VARYING WS-STY-IDX FROM 1 BY 1
                   UNTIL WS-STY-IDX > 10
               MOVE SPACES             TO WS-STY-CODE (WS-STY-IDX)
               MOVE ZERO               TO WS-STY-COUNT (WS-STY-IDX)
                                          WS-STY-PRINCIPAL (WS-STY-IDX)
           END-PERFORM
           MOVE ZERO                   TO WS-STY-USED
           PERFORM VARYING WS-CST-IDX FROM 1 BY 1
                   UNTIL WS-CST-IDX > 12
               MOVE SPACES             TO WS-CST-CCY (WS-CST-IDX)
               MOVE ZERO               TO WS-CST-COUNT (WS-CST-IDX)
                                          WS-CST-NET (WS-CST-IDX)
                                          WS-CST-USD (WS-CST-IDX)
           END-PERFORM
           MOVE ZERO                   TO WS-CST-USED
           PERFORM VARYING WS-LT-IDX FROM 1 BY 1
                   UNTIL WS-LT-IDX > 10
               MOVE SPACES             TO WS-LT-TRADE-ID (WS-LT-IDX)
                                          WS-LT-ACCT-NO (WS-LT-IDX)
                                          WS-LT-CUSIP (WS-LT-IDX)
                                          WS-LT-SIDE (WS-LT-IDX)
               MOVE ZERO               TO WS-LT-USD-NET (WS-LT-IDX)
           END-PERFORM
           MOVE ZERO                   TO WS-LT-USED.
       1300-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - ENRICH ONE TRADE                                        *
      *================================================================*
       2000-PROCESS-TRADE.
           ADD 1                       TO WS-TRADES-READ
           SET WS-TRADE-GOOD           TO TRUE
           MOVE SPACES                 TO WS-REJ-CODE
                                          WS-REJ-TEXT
           PERFORM 2050-COUNT-INPUT    THRU 2050-EXIT
           IF WS-SEQ-CHECK-ON
               PERFORM 2100-SEQUENCE-CHECK THRU 2100-EXIT
           END-IF
      *
           PERFORM 3000-SECURITY-DATA  THRU 3000-EXIT
           IF WS-TRADE-GOOD
               PERFORM 3100-ACCOUNT-DATA THRU 3100-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3200-SETTLEMENT-DATE THRU 3200-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3300-PRINCIPAL  THRU 3300-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3400-ACCRUED-INTEREST THRU 3400-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3500-COMMISSION THRU 3500-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3550-COMMISSION-POLICY THRU 3550-EXIT
           END-IF
           IF WS-TRADE-GOOD
           AND WS-FEES-ACTIVE
               PERFORM 3600-REGULATORY-FEES THRU 3600-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3700-NET-AMOUNT THRU 3700-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3800-FX-CONVERSION THRU 3800-EXIT
           END-IF
           IF WS-TRADE-GOOD
               PERFORM 3900-SETTLE-LOCATION THRU 3900-EXIT
           END-IF
      *
           IF WS-TRADE-GOOD
               PERFORM 4000-FINISH-TRADE THRU 4000-EXIT
               PERFORM 7100-WRITE-TRADE THRU 7100-EXIT
               PERFORM 5000-TRADE-STATISTICS THRU 5000-EXIT
           ELSE
               PERFORM 7000-WRITE-REJECT THRU 7000-EXIT
           END-IF
      *
           IF WS-TRACE-ON
               PERFORM 6500-TRACE-TRADE THRU 6500-EXIT
           END-IF
      *
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2050-COUNT-INPUT.
           ADD TRD-QTY                 TO WS-IN-QTY-HASH
           COMPUTE WS-AMT-WORK ROUNDED = TRD-QTY * TRD-PRICE
           ADD WS-AMT-WORK             TO WS-IN-AMT-HASH
           EVALUATE TRUE
               WHEN TRD-NEW
                   ADD 1               TO WS-NEW-TRADES
               WHEN TRD-CANCEL
                   ADD 1               TO WS-CANCEL-TRADES
               WHEN TRD-CORRECT
                   ADD 1               TO WS-CORRECT-TRADES
           END-EVALUATE
           SET WS-SRC-IDX              TO 1
           SEARCH WS-SRC-ENTRY
               AT END
                   SET WS-SRC-IDX      TO 4
               WHEN WS-SRC-CODE (WS-SRC-IDX) = TRD-SOURCE
                   CONTINUE
           END-SEARCH
           ADD 1                       TO WS-SRC-IN (WS-SRC-IDX).
       2050-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2100 - SEQUENCE CHECK ON ACCOUNT                               *
      *   THE ACCOUNT HOLD AREA ASSUMES ACCOUNT ORDER.  OUT OF         *
      *   SEQUENCE IS A WARNING SINCE CHG35510 - IT ONLY COSTS EXTRA   *
      *   ACCOUNT READS.  THE CUSIP WAS DROPPED FROM THE CHECK WHEN    *
      *   TXN TYPE WAS ADDED TO THE SORT KEY (CHG23380).               *
      *----------------------------------------------------------------*
       2100-SEQUENCE-CHECK.
           MOVE TRD-ACCT-NO            TO WS-CURR-ACCT
           MOVE TRD-CUSIP              TO WS-CURR-CUSIP
           IF WS-CURR-ACCT < WS-PREV-ACCT
               ADD 1                   TO WS-SEQ-ERRORS
               MOVE 'S'                TO TRD-WARN-FLAG (5)
               IF WS-SEQ-ERRORS NOT > WS-PARM-SEQ-WARN-MAX
                   DISPLAY 'TCB200 - SEQUENCE WARNING AT TRADE '
                           TRD-ID ' ACCOUNT ' WS-CURR-ACCT
                           ' PREV ' WS-PREV-ACCT
                   MOVE 'W005'         TO WS-WARN-CODE
                   MOVE SPACES         TO WS-WARN-TEXT
                   STRING 'OUT OF ACCOUNT SEQUENCE AFTER '
                          WS-PREV-ACCT
                          DELIMITED BY SIZE INTO WS-WARN-TEXT
                   PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
               END-IF
           END-IF
           MOVE WS-CURR-KEY            TO WS-PREV-KEY.
       2100-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - SECURITY DATA                                           *
      *================================================================*
       3000-SECURITY-DATA.
           MOVE 'N'                    TO WS-SEC-CACHE-HIT-SW
           IF WS-CACHE-ON
               PERFORM 3010-SEARCH-SEC-CACHE THRU 3010-EXIT
           END-IF
           IF NOT WS-SEC-CACHE-HIT
               PERFORM 3020-READ-SECURITY THRU 3020-EXIT
               IF WS-TRADE-REJECTED
                   GO TO 3000-EXIT
               END-IF
           END-IF
      *
           MOVE SEC-TYPE               TO TRD-SEC-TYPE
           IF TRD-SYMBOL = SPACES
               MOVE SEC-SYMBOL         TO TRD-SYMBOL
           END-IF
           IF SEC-PRICE-FACTOR NOT NUMERIC
           OR SEC-PRICE-FACTOR = ZERO
               MOVE 1                  TO TRD-PRICE-FACTOR
           ELSE
               MOVE SEC-PRICE-FACTOR   TO TRD-PRICE-FACTOR
           END-IF
           IF TRD-CCY = SPACES
               MOVE SEC-CCY            TO TRD-CCY
           END-IF
      *
           IF WS-FRACTION-PRICES
               PERFORM 3050-FRACTION-TO-DECIMAL THRU 3050-EXIT
           END-IF
      *
           PERFORM 3060-COUNT-SEC-TYPE THRU 3060-EXIT
      *
           IF TRD-SELL-SHORT
               PERFORM 3070-SHORT-SALE-CHECK THRU 3070-EXIT
           END-IF.
       3000-EXIT.
           EXIT.
      *
       3010-SEARCH-SEC-CACHE.
           SET WS-SC-IDX               TO 1
           SEARCH WS-SC-ENTRY
               AT END
                   SET WS-SEC-CACHE-FULL TO TRUE
               WHEN WS-SC-CUSIP (WS-SC-IDX) = TRD-CUSIP
                   MOVE WS-SC-SEC-DATA (WS-SC-IDX) TO SEC-MASTER-REC
                   SET WS-SEC-CACHE-HIT TO TRUE
                   ADD 1               TO WS-SEC-CACHE-HITS
               WHEN WS-SC-CUSIP (WS-SC-IDX) = HIGH-VALUES
                   CONTINUE
           END-SEARCH.
       3010-EXIT.
           EXIT.
      *
       3020-READ-SECURITY.
           MOVE 'GET '                 TO SL-FUNCTION
           MOVE TRD-CUSIP              TO SL-KEY-CUSIP
           MOVE SPACES                 TO SL-KEY-ISIN
                                          SL-KEY-SYMBOL
           CALL 'CMD010' USING SL-SECURITY-PARMS
           ADD 1                       TO WS-SEC-DB-CALLS
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA    TO SEC-MASTER-REC
               WHEN SL-NOT-FOUND
                   MOVE 'E001'         TO WS-REJ-CODE
                   STRING 'CUSIP ' TRD-CUSIP
                          ' NOT ON SECURITY MASTER AT ENRICHMENT'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 3020-EXIT
               WHEN OTHER
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE TRD-CUSIP      TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3020-READ-SECURITY' TO AB-PARAGRAPH
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE
      *    ADD TO THE CACHE (FIRST HIGH-VALUES SLOT) UNLESS FULL
           IF WS-CACHE-ON
           AND NOT WS-SEC-CACHE-FULL
               IF WS-SEC-CACHE-COUNT < WS-SEC-CACHE-MAX
                   ADD 1               TO WS-SEC-CACHE-COUNT
                   SET WS-SC-IDX       TO WS-SEC-CACHE-COUNT
                   MOVE TRD-CUSIP      TO WS-SC-CUSIP (WS-SC-IDX)
                   MOVE SEC-MASTER-REC TO WS-SC-SEC-DATA (WS-SC-IDX)
               ELSE
                   SET WS-SEC-CACHE-FULL TO TRUE
                   DISPLAY 'TCB200 - SECURITY CACHE FULL AT '
                           WS-SEC-CACHE-COUNT
                           ' ENTRIES - DB2 USED FOR REMAINDER'
               END-IF
           END-IF.
      *
      *    PRE-1996 VSAM SECURITY MASTER READ - REPLACED BY CMD010
      *    MOVE TRD-CUSIP              TO SEC-CUSIP
      *    READ SECMAST-FILE INTO SEC-MASTER-REC
      *    IF WS-SECMAST-FS = '23'
      *        MOVE 'E001'             TO WS-REJ-CODE
      *        SET WS-TRADE-REJECTED   TO TRUE
      *    END-IF
       3020-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3050 - FRACTIONAL PRICES (OLD EXECUTION TAPE)                  *
      *   PRICE CAME AS WHOLE + NUMERATOR / DENOMINATOR IN THE LOW     *
      *   ORDER DIGITS.  NOT USED SINCE DECIMALIZATION (CHG08814).     *
      *----------------------------------------------------------------*
       3050-FRACTION-TO-DECIMAL.
           MOVE TRD-PRICE              TO WS-FRAC-WHOLE
           COMPUTE WS-FRAC-NUMER =
                   (TRD-PRICE - WS-FRAC-WHOLE) * 1000
           MOVE 256                    TO WS-FRAC-DENOM
           IF WS-FRAC-DENOM > ZERO
               COMPUTE WS-FRAC-PRICE ROUNDED =
                       WS-FRAC-WHOLE + (WS-FRAC-NUMER / WS-FRAC-DENOM)
               MOVE WS-FRAC-PRICE      TO TRD-PRICE
           END-IF.
       3050-EXIT.
           EXIT.
      *
       3060-COUNT-SEC-TYPE.
           SET WS-STY-IDX              TO 1
           SEARCH WS-STY-ENTRY
               AT END
                   CONTINUE
               WHEN WS-STY-CODE (WS-STY-IDX) = TRD-SEC-TYPE
                   ADD 1               TO WS-STY-COUNT (WS-STY-IDX)
               WHEN WS-STY-CODE (WS-STY-IDX) = SPACES
                   MOVE TRD-SEC-TYPE   TO WS-STY-CODE (WS-STY-IDX)
                   MOVE 1              TO WS-STY-COUNT (WS-STY-IDX)
                   ADD 1               TO WS-STY-USED
           END-SEARCH.
       3060-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3070 - SHORT SALE LOCATE.  THE SECURITY MASTER CARRIES THE     *
      *   EASY-TO-BORROW INDICATOR FOR LISTED EQUITIES.  A SHORT SALE  *
      *   IN A SECURITY NOT ON THE LIST IS FLAGGED FOR THE STOCK LOAN  *
      *   DESK (REG SHO) - IT IS NOT REJECTED.                         *
      *----------------------------------------------------------------*
       3070-SHORT-SALE-CHECK.
           IF SEC-FIXED-INCOME
               GO TO 3070-EXIT
           END-IF
           IF SEC-SHORT-SALE-FLAG = 'N'
               ADD 1                   TO WS-SHORT-WARNINGS
               MOVE 'H'                TO TRD-WARN-FLAG (6)
               MOVE 'W006'             TO WS-WARN-CODE
               MOVE SPACES             TO WS-WARN-TEXT
               STRING 'SHORT SALE ' TRD-SYMBOL
                      ' NOT ON EASY-TO-BORROW LIST'
                      DELIMITED BY SIZE INTO WS-WARN-TEXT
               PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
           END-IF.
       3070-EXIT.
           EXIT.
      *
      *================================================================*
      * 3100 - ACCOUNT DATA (BRANCH, REP, TYPE)                        *
      *   INPUT IS IN ACCOUNT ORDER - THE LAST ACCOUNT RECORD IS KEPT  *
      *   IN THE FD AREA AND RE-USED FOR CONSECUTIVE TRADES.           *
      *================================================================*
       3100-ACCOUNT-DATA.
           IF TRD-ACCT-NO = WS-LAST-ACCT-NO
               ADD 1                   TO WS-ACCT-REUSED
           ELSE
               PERFORM 3110-READ-ACCOUNT THRU 3110-EXIT
           END-IF
           IF NOT WS-LAST-ACCT-ON-FILE
               MOVE 'E002'             TO WS-REJ-CODE
               STRING 'ACCOUNT ' TRD-ACCT-NO
                      ' NOT ON MASTER AT ENRICHMENT'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
               GO TO 3100-EXIT
           END-IF
           MOVE ACCT-TYPE              TO TRD-ACCT-TYPE
           MOVE ACCT-BRANCH            TO TRD-BRANCH
           MOVE ACCT-REP               TO TRD-REP
           PERFORM 3120-ACCOUNT-CHECKS THRU 3120-EXIT.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3120 - INFORMATIONAL ACCOUNT CHECKS (COUNTED, NOT REJECTED)    *
      *   - TRADE CURRENCY DIFFERS FROM ACCOUNT BASE CURRENCY          *
      *   - SHORT SALE IN A CASH ACCOUNT (MARGIN DEPT REPORT)          *
      *   - HOUSE ACCOUNT ACTIVITY                                     *
      *   THE DESK FIELD FOR HOUSE ACCOUNTS COMES FROM THE MASTER      *
      *   WHEN THE FEED DID NOT SUPPLY ONE.                            *
      *----------------------------------------------------------------*
       3120-ACCOUNT-CHECKS.
           IF ACCT-BASE-CCY NOT = SPACES
           AND ACCT-BASE-CCY NOT = TRD-CCY
               ADD 1                   TO WS-CROSS-CCY-TRADES
           END-IF
           IF TRD-SELL-SHORT
           AND ACCT-CASH-ACCT
               ADD 1                   TO WS-SHORT-IN-CASH-ACCT
               DISPLAY 'TCB200 - SHORT SALE IN CASH ACCOUNT '
                       TRD-ACCT-NO ' TRADE ' TRD-ID
           END-IF
           EVALUATE TRUE
               WHEN ACCT-FIRM-INVENTORY
               WHEN ACCT-STREET-SIDE
                   ADD 1               TO WS-HOUSE-TRADES
                   IF TRD-DESK = SPACES
                       MOVE ACCT-FIRM-DESK TO TRD-DESK
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       3120-EXIT.
           EXIT.
      *
       3110-READ-ACCOUNT.
           MOVE TRD-ACCT-NO            TO ACCT-NO
                                          WS-LAST-ACCT-NO
           READ ACCTMAST-FILE
           ADD 1                       TO WS-ACCT-READS
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE 'Y'            TO WS-LAST-ACCT-FOUND
               WHEN ACCTMAST-NOTFND
                   MOVE 'N'            TO WS-LAST-ACCT-FOUND
               WHEN OTHER
                   MOVE 'ACCTMAST'     TO AB-DDNAME
                   MOVE WS-ACCTMAST-FS TO AB-FILE-STATUS
                   MOVE TRD-ACCT-NO    TO AB-KEY
                   MOVE '3110-READ-ACCOUNT' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       3110-EXIT.
           EXIT.
      *
      *================================================================*
      * 3200 - SETTLEMENT DATE                                         *
      *   SUPPLIED BY THE FEED (BOND DESK, MANUAL) OR DERIVED BY       *
      *   CMU020 FROM SECURITY TYPE, CURRENCY AND MARKET.              *
      *================================================================*
       3200-SETTLEMENT-DATE.
           IF TRD-SETTLE-DATE IS NUMERIC
           AND TRD-SETTLE-DATE > ZERO
               ADD 1                   TO WS-SETTLE-SUPPLIED
               IF TRD-SETTLE-DATE < TRD-TRADE-DATE
                   MOVE 'E003'         TO WS-REJ-CODE
                   STRING 'SETTLE DATE ' TRD-SETTLE-DATE
                          ' BEFORE TRADE DATE ' TRD-TRADE-DATE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
               END-IF
               GO TO 3200-EXIT
           END-IF
      *
           INITIALIZE SD-SETTLE-PARMS
           MOVE TRD-TRADE-DATE         TO SD-TRADE-DATE
           MOVE TRD-SEC-TYPE           TO SD-SEC-TYPE
           MOVE TRD-CCY                TO SD-CCY
           MOVE TRD-MARKET             TO SD-MARKET
           MOVE ZERO                   TO SD-SETTLE-DAYS-OVR
           CALL 'CMU020' USING SD-SETTLE-PARMS
           EVALUATE TRUE
               WHEN SD-OK
                   MOVE SD-SETTLE-DATE TO TRD-SETTLE-DATE
                   ADD 1               TO WS-SETTLE-COMPUTED
               WHEN SD-TRADE-DATE-HOLIDAY
                   MOVE 'E003'         TO WS-REJ-CODE
                   STRING 'TRADE DATE ' TRD-TRADE-DATE
                          ' IS NOT A BUSINESS DAY'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
               WHEN OTHER
                   MOVE 'E003'         TO WS-REJ-CODE
                   MOVE SD-MESSAGE     TO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
           END-EVALUATE.
      *
      *    T+3 SETTLEMENT BEFORE CMU020 (CHG00790 / CHG01950)
      *    PERFORM 4500-CALC-T3-SETTLE THRU 4500-EXIT
       3200-EXIT.
           EXIT.
      *
      *================================================================*
      * 3300 - PRINCIPAL = QUANTITY X PRICE X PRICE FACTOR             *
      *   BONDS: QTY IS FACE, PRICE IS PERCENT OF PAR, FACTOR 0.01     *
      *================================================================*
       3300-PRINCIPAL.
           COMPUTE WS-PRIN-WORK ROUNDED =
                   TRD-QTY * TRD-PRICE * TRD-PRICE-FACTOR
               ON SIZE ERROR
                   MOVE 'E008'         TO WS-REJ-CODE
                   MOVE 'PRINCIPAL OVERFLOW - QTY X PRICE TOO LARGE'
                                       TO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 3300-EXIT
           END-COMPUTE
           IF WS-PRIN-WORK NOT > ZERO
               MOVE 'E008'             TO WS-REJ-CODE
               MOVE 'PRINCIPAL IS ZERO' TO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
               GO TO 3300-EXIT
           END-IF
           MOVE WS-PRIN-WORK           TO TRD-PRINCIPAL
           IF TRD-SRC-FIXED-INC
           OR SEC-FIXED-INCOME
               IF TRD-FACE-AMOUNT NOT NUMERIC
               OR TRD-FACE-AMOUNT = ZERO
                   MOVE TRD-QTY        TO TRD-FACE-AMOUNT
               END-IF
           ELSE
               MOVE ZERO               TO TRD-FACE-AMOUNT
           END-IF.
       3300-EXIT.
           EXIT.
      *
      *================================================================*
      * 3400 - ACCRUED INTEREST (FIXED INCOME ONLY)                    *
      *   THE BOND DESK MAY SUPPLY AN OVERRIDE (WARN FLAG 8 = 'A').    *
      *   ACCRUED IS TO THE SETTLEMENT DATE.                           *
      *================================================================*
       3400-ACCRUED-INTEREST.
           IF NOT SEC-FIXED-INCOME
               MOVE ZERO               TO TRD-ACCRUED-INT
               GO TO 3400-EXIT
           END-IF
           IF TRD-WARN-FLAG (8) = 'A'
               ADD 1                   TO WS-ACCRUED-OVERRIDES
               IF TRD-ACCRUED-INT NOT NUMERIC
                   MOVE ZERO           TO TRD-ACCRUED-INT
               END-IF
               GO TO 3400-EXIT
           END-IF
           IF SEC-COUPON-RATE NOT NUMERIC
           OR SEC-COUPON-RATE = ZERO
      *        ZERO COUPON / DISCOUNT NOTE - NO ACCRUAL
               MOVE ZERO               TO TRD-ACCRUED-INT
               GO TO 3400-EXIT
           END-IF
      *
           INITIALIZE AI-ACCRUAL-PARMS
           MOVE SEC-DAYCOUNT           TO AI-DAYCOUNT
           MOVE SEC-COUPON-RATE        TO AI-COUPON-RATE
           MOVE SEC-COUPON-FREQ        TO AI-COUPON-FREQ
           MOVE SEC-MATURITY-DATE      TO AI-MATURITY-DATE
           MOVE SEC-ISSUE-DATE         TO AI-ISSUE-DATE
           MOVE SEC-FIRST-CPN-DATE     TO AI-FIRST-CPN-DATE
           MOVE TRD-SETTLE-DATE        TO AI-SETTLE-DATE
           MOVE TRD-FACE-AMOUNT        TO AI-FACE-AMOUNT
           MOVE ZERO                   TO AI-LAST-CPN-DATE
                                          AI-NEXT-CPN-DATE
           CALL 'CMU030' USING AI-ACCRUAL-PARMS
           EVALUATE TRUE
               WHEN AI-OK
                   MOVE AI-ACCRUED-AMOUNT TO TRD-ACCRUED-INT
                   ADD 1               TO WS-ACCRUED-COMPUTED
               WHEN AI-AFTER-MATURITY
                   MOVE 'E006'         TO WS-REJ-CODE
                   STRING 'SETTLES AFTER MATURITY '
                          SEC-MATURITY-DATE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
               WHEN OTHER
                   MOVE 'E006'         TO WS-REJ-CODE
                   STRING 'CMU030 RC ' AI-RETURN-CODE ' '
                          AI-MESSAGE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
           END-EVALUATE.
       3400-EXIT.
           EXIT.
      *
      *================================================================*
      * 3500 - COMMISSION                                              *
      *   OMS OVERRIDE WINS.  HOUSE ACCOUNTS (FIRM INVENTORY, STREET)  *
      *   ARE NEVER CHARGED.  OTHERWISE TCU210 BY ACCOUNT SCHEDULE.    *
      *================================================================*
       3500-COMMISSION.
           IF TRD-COMM-OVERRIDDEN
               ADD 1                   TO WS-COMM-OVERRIDES
               IF TRD-COMMISSION NOT NUMERIC
                   MOVE ZERO           TO TRD-COMMISSION
               END-IF
               GO TO 3500-EXIT
           END-IF
           IF ACCT-FIRM-INVENTORY
           OR ACCT-STREET-SIDE
               MOVE ZERO               TO TRD-COMMISSION
               ADD 1                   TO WS-COMM-HOUSE
               GO TO 3500-EXIT
           END-IF
      *
           INITIALIZE CO-COMMISSION-PARMS
           MOVE ACCT-COMM-SCHED        TO CO-COMM-SCHED
           MOVE TRD-SEC-TYPE           TO CO-SEC-TYPE
           MOVE TRD-CAPACITY           TO CO-CAPACITY
           MOVE TRD-SIDE               TO CO-SIDE
           MOVE TRD-QTY                TO CO-QTY
           MOVE TRD-PRICE              TO CO-PRICE
           MOVE TRD-PRINCIPAL          TO CO-PRINCIPAL
           MOVE TRD-TRADE-DATE         TO CO-TRADE-DATE
           IF ACCT-INSTITUTIONAL
           OR ACCT-OMNIBUS
               MOVE ACCT-INST-COMM-RATE TO CO-INST-RATE-BPS
           ELSE
               MOVE ZERO               TO CO-INST-RATE-BPS
           END-IF
           CALL 'TCU210' USING CO-COMMISSION-PARMS
           EVALUATE TRUE
               WHEN CO-OK
                   MOVE CO-COMMISSION  TO TRD-COMMISSION
                   ADD 1               TO WS-COMM-COMPUTED
               WHEN CO-SCHED-DEFAULTED
                   MOVE CO-COMMISSION  TO TRD-COMMISSION
                   ADD 1               TO WS-COMM-COMPUTED
                   MOVE 'C'            TO TRD-WARN-FLAG (4)
                   MOVE 'W004'         TO WS-WARN-CODE
                   MOVE CO-MESSAGE     TO WS-WARN-TEXT
                   PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
               WHEN OTHER
                   MOVE 'E007'         TO WS-REJ-CODE
                   STRING 'TCU210 RC ' CO-RETURN-CODE ' '
                          CO-MESSAGE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
           END-EVALUATE.
       3500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3550 - COMMISSION REASONABLENESS (5% POLICY, CHG12011)         *
      *   A COMMISSION ABOVE THE LIMIT PERCENT OF PRINCIPAL IS FLAGGED *
      *   FOR SUPERVISORY REVIEW.  MINIMUM TICKET CHARGES ON SMALL     *
      *   TRADES ARE THE USUAL CAUSE.                                  *
      *----------------------------------------------------------------*
       3550-COMMISSION-POLICY.
           IF TRD-COMMISSION NOT > ZERO
           OR TRD-PRINCIPAL NOT > ZERO
               GO TO 3550-EXIT
           END-IF
           COMPUTE WS-COMM-PCT ROUNDED =
                   TRD-COMMISSION * 100 / TRD-PRINCIPAL
               ON SIZE ERROR
                   MOVE 99.9999        TO WS-COMM-PCT
           END-COMPUTE
           IF WS-COMM-PCT > WS-PARM-COMM-PCT-LIMIT
               ADD 1                   TO WS-COMM-PCT-WARNINGS
               MOVE 'M'                TO TRD-WARN-FLAG (7)
               MOVE 'W007'             TO WS-WARN-CODE
               MOVE SPACES             TO WS-WARN-TEXT
               MOVE TRD-COMMISSION     TO WS-DISP-AMT
               STRING 'COMMISSION ' WS-DISP-AMT
                      ' EXCEEDS PCT LIMIT'
                      DELIMITED BY SIZE INTO WS-WARN-TEXT
               PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
           END-IF.
       3550-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1988 IN-LINE COMMISSION - MOVED TO TCU210 IN 1989.  LEFT HERE  *
      * FOR THE AUDITORS (RECONSTRUCTION OF 1988 CONFIRMS).            *
      *----------------------------------------------------------------*
      *3590-OLD-COMMISSION.
      *    IF TRD-PRINCIPAL < 2500
      *        COMPUTE TRD-COMMISSION ROUNDED =
      *                TRD-PRINCIPAL * .0300
      *    ELSE
      *        IF TRD-PRINCIPAL < 10000
      *            COMPUTE TRD-COMMISSION ROUNDED =
      *                    75.00 + (TRD-PRINCIPAL - 2500) * .0200
      *        ELSE
      *            COMPUTE TRD-COMMISSION ROUNDED =
      *                    225.00 + (TRD-PRINCIPAL - 10000) * .0100
      *        END-IF
      *    END-IF
      *    IF TRD-COMMISSION < 20.00
      *        MOVE 20.00              TO TRD-COMMISSION
      *    END-IF
      *    IF ACCT-TYPE = 'EM'
      *        COMPUTE TRD-COMMISSION ROUNDED = TRD-COMMISSION * .50
      *    END-IF.
      *3590-EXIT.
      *    EXIT.
      *
      *================================================================*
      * 3600 - REGULATORY FEES                                         *
      *   THE FEE MODULE IS SELECTED BY SECURITY TYPE:                 *
      *     TCU22E  EQUITY, PREFERRED, ADR, FUNDS                      *
      *     TCU22F  CORPORATE, MUNICIPAL, GOVERNMENT BONDS             *
      *================================================================*
       3600-REGULATORY-FEES.
           MOVE ZERO                   TO TRD-SEC-FEE
                                          TRD-TAF-FEE
                                          TRD-OTHER-FEES
           EVALUATE TRUE
               WHEN SEC-FIXED-INCOME
                   MOVE 'F'            TO WS-FEE-SUFFIX
                   ADD 1               TO WS-FEE-CALLS-F
               WHEN OTHER
                   MOVE 'E'            TO WS-FEE-SUFFIX
                   ADD 1               TO WS-FEE-CALLS-E
           END-EVALUATE
           MOVE SPACES                 TO WS-FEE-PGM
           STRING 'TCU22' WS-FEE-SUFFIX
                  DELIMITED BY SIZE INTO WS-FEE-PGM
      *
           INITIALIZE FE-FEE-PARMS
           MOVE TRD-SEC-TYPE           TO FE-SEC-TYPE
           MOVE TRD-SIDE               TO FE-SIDE
           MOVE TRD-CAPACITY           TO FE-CAPACITY
           MOVE TRD-TRADE-DATE         TO FE-TRADE-DATE
           MOVE TRD-QTY                TO FE-QTY
           MOVE TRD-PRINCIPAL          TO FE-PRINCIPAL
           MOVE TRD-FACE-AMOUNT        TO FE-FACE-AMOUNT
      *
           CALL WS-FEE-PGM USING FE-FEE-PARMS
               ON EXCEPTION
                   MOVE 'E005'         TO WS-REJ-CODE
                   STRING 'FEE MODULE ' WS-FEE-PGM
                          ' COULD NOT BE LOADED'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 3600-EXIT
           END-CALL
      *
           EVALUATE TRUE
               WHEN FE-OK
               WHEN FE-NOT-APPLICABLE
                   MOVE FE-SEC-FEE     TO TRD-SEC-FEE
                   MOVE FE-TAF-FEE     TO TRD-TAF-FEE
                   MOVE FE-OTHER-FEES  TO TRD-OTHER-FEES
               WHEN OTHER
                   MOVE 'E005'         TO WS-REJ-CODE
                   STRING WS-FEE-PGM ' RC ' FE-RETURN-CODE ' '
                          FE-MESSAGE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
           END-EVALUATE.
      *
      *    BEFORE CHG22418 ONE MODULE HANDLED ALL TYPES
      *    CALL 'TCU220' USING FE-FEE-PARMS
       3600-EXIT.
           EXIT.
      *
      *================================================================*
      * 3700 - NET AMOUNT                                              *
      *   BUY  = PRINCIPAL + COMMISSION + FEES + ACCRUED               *
      *   SELL = PRINCIPAL - COMMISSION - FEES + ACCRUED               *
      *================================================================*
       3700-NET-AMOUNT.
           COMPUTE WS-TOTAL-FEES =
                   TRD-SEC-FEE + TRD-TAF-FEE + TRD-OTHER-FEES
           EVALUATE TRUE
               WHEN TRD-BUY-SIDE
                   COMPUTE WS-NET-WORK =
                           TRD-PRINCIPAL + TRD-COMMISSION
                         + WS-TOTAL-FEES + TRD-ACCRUED-INT
               WHEN TRD-SELL-SIDE
                   COMPUTE WS-NET-WORK =
                           TRD-PRINCIPAL - TRD-COMMISSION
                         - WS-TOTAL-FEES + TRD-ACCRUED-INT
               WHEN OTHER
                   MOVE 'E008'         TO WS-REJ-CODE
                   STRING 'SIDE [' TRD-SIDE '] CANNOT BE NETTED'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 3700-EXIT
           END-EVALUATE
           MOVE WS-NET-WORK            TO TRD-NET-AMOUNT.
       3700-EXIT.
           EXIT.
      *
      *================================================================*
      * 3800 - FX CONVERSION TO USD                                    *
      *================================================================*
       3800-FX-CONVERSION.
           IF TRD-CCY = 'USD'
               MOVE 1                  TO TRD-FX-RATE
               MOVE TRD-NET-AMOUNT     TO TRD-USD-NET-AMOUNT
               GO TO 3800-EXIT
           END-IF
           IF WS-EURO-CONV-ON
               PERFORM 3850-EURO-LEGACY-CCY THRU 3850-EXIT
           END-IF
      *
           INITIALIZE FX-CONVERT-PARMS
           MOVE TRD-CCY                TO FX-FROM-CCY
           MOVE 'USD'                  TO FX-TO-CCY
           MOVE TRD-TRADE-DATE         TO FX-RATE-DATE
           MOVE TRD-NET-AMOUNT         TO FX-AMOUNT-IN
           CALL 'CMU040' USING FX-CONVERT-PARMS
           EVALUATE TRUE
               WHEN FX-OK
                   MOVE FX-RATE        TO TRD-FX-RATE
                   MOVE FX-AMOUNT-OUT  TO TRD-USD-NET-AMOUNT
                   ADD 1               TO WS-FX-CONVERSIONS
               WHEN FX-STALE-RATE
                   MOVE FX-RATE        TO TRD-FX-RATE
                   MOVE FX-AMOUNT-OUT  TO TRD-USD-NET-AMOUNT
                   ADD 1               TO WS-FX-CONVERSIONS
                                          WS-FX-STALE
                   MOVE 'X'            TO TRD-WARN-FLAG (3)
                   MOVE 'W003'         TO WS-WARN-CODE
                   MOVE SPACES         TO WS-WARN-TEXT
                   STRING 'STALE ' TRD-CCY ' RATE FROM '
                          FX-RATE-ACTUAL-DATE
                          DELIMITED BY SIZE INTO WS-WARN-TEXT
                   PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
               WHEN FX-RATE-NOT-FOUND
                   MOVE 'E004'         TO WS-REJ-CODE
                   STRING 'NO ' TRD-CCY '/USD RATE FOR '
                          TRD-TRADE-DATE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
               WHEN OTHER
                   MOVE ZERO           TO AB-SQLCODE
                   MOVE TRD-CCY        TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3800-FX-CONVERSION' TO AB-PARAGRAPH
                   MOVE FX-MESSAGE     TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE.
      *
      *    PRE-CHG19002: IN-HOUSE RATE TABLE LOADED FROM FXTABLE DD
      *    SEARCH WS-FX-ENTRY
      *        AT END MOVE 'E004' TO WS-REJ-CODE
      *        WHEN WS-FX-CCY (WS-FX-IDX) = TRD-CCY
      *            COMPUTE TRD-USD-NET-AMOUNT ROUNDED =
      *                    TRD-NET-AMOUNT * WS-FX-RATE (WS-FX-IDX)
      *    END-SEARCH
       3800-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3850 - EURO LEGACY CURRENCY (1999-2001 ONLY)                   *
      *   NATIONAL CURRENCY AMOUNT RESTATED IN EUR AT THE IRREVOCABLE  *
      *   CONVERSION RATE, THEN CONVERTED TO USD AS EUR.               *
      *----------------------------------------------------------------*
       3850-EURO-LEGACY-CCY.
           SET WS-EURO-IDX             TO 1
           SEARCH WS-EURO-LEG-ENTRY
               AT END
                   CONTINUE
               WHEN WS-EURO-LEG-CCY (WS-EURO-IDX) = TRD-CCY
                   COMPUTE TRD-NET-AMOUNT ROUNDED =
                           TRD-NET-AMOUNT
                         / WS-EURO-LEG-RATE (WS-EURO-IDX)
                   MOVE 'EUR'          TO TRD-CCY
           END-SEARCH.
       3850-EXIT.
           EXIT.
      *
      *================================================================*
      * 3900 - SETTLEMENT LOCATION FROM SECURITY DEPOSITORY            *
      *================================================================*
       3900-SETTLE-LOCATION.
           EVALUATE TRUE
               WHEN SEC-DEPOSITORY NOT = SPACES
                   MOVE SEC-DEPOSITORY TO TRD-SETTLE-LOC
               WHEN SEC-GOVT-BOND
                   MOVE 'FED '         TO TRD-SETTLE-LOC
               WHEN TRD-CCY NOT = 'USD'
                   MOVE 'EUCL'         TO TRD-SETTLE-LOC
               WHEN OTHER
                   MOVE 'DTC '         TO TRD-SETTLE-LOC
           END-EVALUATE.
       3900-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - FINISH TRADE                                            *
      *================================================================*
       4000-FINISH-TRADE.
           SET TRD-ST-ENRICHED         TO TRUE
           MOVE WS-ENRICH-TS           TO TRD-ENRICH-TS
           MOVE SPACES                 TO TRD-REJECT-CODE
                                          TRD-REJECT-TEXT
           MOVE ZERO                   TO TRD-DUP-HASH.
       4000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4500 - T+3 SETTLEMENT DATE (1993 RULE)                         *
      *   TRADE DATE PLUS THREE NYSE BUSINESS DAYS.  GOVERNMENTS T+1   *
      *   ON THE FED CALENDAR.  REPLACED BY CMU020 IN CHG01950.        *
      *----------------------------------------------------------------*
       4500-CALC-T3-SETTLE.
           MOVE 'ADDB'                 TO DT-FUNCTION
           MOVE TRD-TRADE-DATE         TO DT-DATE-1
           MOVE ZERO                   TO DT-DATE-2
           IF SEC-GOVT-BOND
               MOVE 'FED '             TO DT-CALENDAR
               MOVE 1                  TO DT-DAYS
           ELSE
               MOVE 'NYSE'             TO DT-CALENDAR
               MOVE 3                  TO DT-DAYS
           END-IF
           CALL 'CMU010' USING DT-DATE-PARMS
           IF DT-OK
               MOVE DT-RESULT-DATE     TO TRD-SETTLE-DATE
               ADD 1                   TO WS-SETTLE-COMPUTED
           ELSE
               MOVE 'E003'             TO WS-REJ-CODE
               MOVE DT-MESSAGE         TO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
           END-IF
      *    CASH TRADES SETTLE SAME DAY
           IF TRD-MARKET = 'CASH'
               MOVE TRD-TRADE-DATE     TO TRD-SETTLE-DATE
           END-IF.
       4500-EXIT.
           EXIT.
      *
      *================================================================*
      * 5000 - PER TRADE STATISTICS (CURRENCY, LARGE TRADES)           *
      *================================================================*
       5000-TRADE-STATISTICS.
           SET WS-CST-IDX              TO 1
           SEARCH WS-CST-ENTRY
               AT END
                   DISPLAY 'TCB200 - CURRENCY STATISTICS TABLE FULL'
               WHEN WS-CST-CCY (WS-CST-IDX) = TRD-CCY
                   ADD 1               TO WS-CST-COUNT (WS-CST-IDX)
                   ADD TRD-NET-AMOUNT  TO WS-CST-NET (WS-CST-IDX)
                   ADD TRD-USD-NET-AMOUNT
                                       TO WS-CST-USD (WS-CST-IDX)
               WHEN WS-CST-CCY (WS-CST-IDX) = SPACES
                   MOVE TRD-CCY        TO WS-CST-CCY (WS-CST-IDX)
                   MOVE 1              TO WS-CST-COUNT (WS-CST-IDX)
                   MOVE TRD-NET-AMOUNT TO WS-CST-NET (WS-CST-IDX)
                   MOVE TRD-USD-NET-AMOUNT
                                       TO WS-CST-USD (WS-CST-IDX)
                   ADD 1               TO WS-CST-USED
           END-SEARCH
      *
           MOVE TRD-USD-NET-AMOUNT     TO WS-LT-ABS
           IF WS-LT-ABS < ZERO
               COMPUTE WS-LT-ABS = ZERO - WS-LT-ABS
           END-IF
           IF WS-LT-ABS < WS-PARM-LARGE-TRADE
               GO TO 5000-EXIT
           END-IF
           ADD 1                       TO WS-LARGE-TRADES
           PERFORM 5100-INSERT-LARGE-TRADE THRU 5100-EXIT.
       5000-EXIT.
           EXIT.
      *
      *    INSERTION INTO THE DESCENDING TOP-10 LIST
       5100-INSERT-LARGE-TRADE.
           MOVE ZERO                   TO WS-LT-INS
           PERFORM VARYING WS-LT-SUB FROM 1 BY 1
                   UNTIL WS-LT-SUB > WS-LT-USED
                      OR WS-LT-INS > ZERO
               IF WS-LT-ABS > WS-LT-USD-NET (WS-LT-SUB)
                   MOVE WS-LT-SUB      TO WS-LT-INS
               END-IF
           END-PERFORM
           IF WS-LT-INS = ZERO
               IF WS-LT-USED < 10
                   ADD 1               TO WS-LT-USED
                   MOVE WS-LT-USED     TO WS-LT-INS
               ELSE
                   GO TO 5100-EXIT
               END-IF
           ELSE
               IF WS-LT-USED < 10
                   ADD 1               TO WS-LT-USED
               END-IF
               PERFORM VARYING WS-LT-SUB FROM WS-LT-USED BY -1
                       UNTIL WS-LT-SUB NOT > WS-LT-INS
                   MOVE WS-LT-ENTRY (WS-LT-SUB - 1)
                                       TO WS-LT-ENTRY (WS-LT-SUB)
               END-PERFORM
           END-IF
           MOVE TRD-ID                 TO WS-LT-TRADE-ID (WS-LT-INS)
           MOVE TRD-ACCT-NO            TO WS-LT-ACCT-NO (WS-LT-INS)
           MOVE TRD-CUSIP              TO WS-LT-CUSIP (WS-LT-INS)
           MOVE TRD-SIDE               TO WS-LT-SIDE (WS-LT-INS)
           MOVE WS-LT-ABS              TO WS-LT-USD-NET (WS-LT-INS).
       5100-EXIT.
           EXIT.
      *
      *================================================================*
      * 6500 - TRACE (PARAMETER TRACE=Y)                               *
      *================================================================*
       6500-TRACE-TRADE.
           ADD 1                       TO WS-TRACE-COUNT
           IF WS-TRACE-COUNT > WS-PARM-TRACE-LIMIT
               GO TO 6500-EXIT
           END-IF
           DISPLAY 'TCB200 TRACE ' TRD-ID ' ' TRD-SOURCE ' '
                   TRD-TXN-TYPE ' ' TRD-ACCT-NO ' ' TRD-CUSIP ' '
                   TRD-SIDE ' ' TRD-SEC-TYPE
           IF WS-TRADE-REJECTED
               DISPLAY 'TCB200 TRACE    REJECTED ' WS-REJ-CODE ' '
                       WS-REJ-TEXT
               GO TO 6500-EXIT
           END-IF
           MOVE TRD-QTY                TO WS-DISP-QTY
           DISPLAY 'TCB200 TRACE    QTY       ' WS-DISP-QTY
           MOVE TRD-PRINCIPAL          TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    PRINCIPAL ' WS-DISP-AMT
           MOVE TRD-COMMISSION         TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    COMMISSION' WS-DISP-AMT
                   ' RULE ' CO-RULE-APPLIED
           MOVE TRD-SEC-FEE            TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    SEC FEE   ' WS-DISP-AMT
                   ' VIA ' WS-FEE-PGM
           MOVE TRD-TAF-FEE            TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    TAF FEE   ' WS-DISP-AMT
           MOVE TRD-OTHER-FEES         TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    OTHER FEES' WS-DISP-AMT
           MOVE TRD-ACCRUED-INT        TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    ACCRUED   ' WS-DISP-AMT
           MOVE TRD-NET-AMOUNT         TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    NET       ' WS-DISP-AMT
                   ' ' TRD-CCY
           MOVE TRD-FX-RATE            TO WS-DISP-RATE
           MOVE TRD-USD-NET-AMOUNT     TO WS-DISP-AMT
           DISPLAY 'TCB200 TRACE    USD NET   ' WS-DISP-AMT
                   ' @ ' WS-DISP-RATE
           DISPLAY 'TCB200 TRACE    SETTLE    ' TRD-SETTLE-DATE
                   ' AT ' TRD-SETTLE-LOC.
       6500-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - OUTPUT                                                  *
      *================================================================*
       7000-WRITE-REJECT.
           MOVE WS-REJ-CODE            TO TRD-REJECT-CODE
           MOVE WS-REJ-TEXT            TO TRD-REJECT-TEXT
           SET TRD-ST-REJECTED         TO TRUE
           MOVE WS-REJ-CODE            TO WS-WARN-CODE
           MOVE WS-REJ-TEXT            TO WS-WARN-TEXT
           PERFORM 7010-BUILD-REJECT   THRU 7010-EXIT
           MOVE 'E'                    TO REJ-SEVERITY
           WRITE REJOUT-REC            FROM REJ-REJECT-REC
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7000-WRITE-REJECT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-TRADES-REJECTED
           ADD TRD-QTY                 TO WS-REJ-QTY-HASH
           ADD 1                       TO WS-SRC-REJ (WS-SRC-IDX).
       7000-EXIT.
           EXIT.
      *
       7010-BUILD-REJECT.
           INITIALIZE REJ-REJECT-REC
           MOVE DC-BUS-DATE            TO REJ-BUS-DATE
           MOVE 'ENRICH'               TO REJ-STAGE
           MOVE WS-PROGRAM-ID          TO REJ-PROGRAM
           MOVE TRD-SOURCE             TO REJ-SOURCE
           MOVE TRD-ID                 TO REJ-TRADE-ID
           MOVE TRD-ACCT-NO            TO REJ-ACCT-NO
           MOVE TRD-CUSIP              TO REJ-CUSIP
           MOVE WS-WARN-CODE           TO REJ-CODE
           MOVE WS-WARN-TEXT           TO REJ-TEXT
           MOVE 300                    TO REJ-RAW-LENGTH
           MOVE TRD-TRADE-REC(1:300)   TO REJ-RAW-IMAGE.
       7010-EXIT.
           EXIT.
      *
       7050-WRITE-WARNING.
           PERFORM 7010-BUILD-REJECT   THRU 7010-EXIT
           MOVE 'W'                    TO REJ-SEVERITY
           WRITE REJOUT-REC            FROM REJ-REJECT-REC
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7050-WRITE-WARNING' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-WARNINGS-WRITTEN.
       7050-EXIT.
           EXIT.
      *
       7100-WRITE-TRADE.
           WRITE TRADEOUT-REC          FROM TRD-TRADE-REC
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7100-WRITE-TRADE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-TRADES-WRITTEN
           ADD 1                       TO WS-SRC-OUT (WS-SRC-IDX)
           ADD TRD-NET-AMOUNT          TO WS-SRC-NET (WS-SRC-IDX)
           ADD TRD-QTY                 TO WS-OUT-QTY-HASH
           ADD TRD-NET-AMOUNT          TO WS-OUT-NET-TOTAL
           ADD TRD-USD-NET-AMOUNT      TO WS-OUT-USD-TOTAL
           ADD TRD-PRINCIPAL           TO WS-TOT-PRINCIPAL
           ADD TRD-COMMISSION          TO WS-TOT-COMMISSION
           ADD TRD-SEC-FEE             TO WS-TOT-SEC-FEE
           ADD TRD-TAF-FEE             TO WS-TOT-TAF-FEE
           ADD TRD-OTHER-FEES          TO WS-TOT-OTHER-FEES
           ADD TRD-ACCRUED-INT         TO WS-TOT-ACCRUED
           SET WS-STY-IDX              TO 1
           SEARCH WS-STY-ENTRY
               AT END
                   CONTINUE
               WHEN WS-STY-CODE (WS-STY-IDX) = TRD-SEC-TYPE
                   ADD TRD-PRINCIPAL   TO WS-STY-PRINCIPAL (WS-STY-IDX)
           END-SEARCH.
       7100-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - READ NEXT VALIDATED TRADE                               *
      *================================================================*
       8000-READ-TRADE.
           READ TRADEIN-FILE INTO TRD-TRADE-REC
           EVALUATE TRUE
               WHEN TRADEIN-OK
                   CONTINUE
               WHEN TRADEIN-EOF
                   SET WS-END-OF-TRADES TO TRUE
               WHEN OTHER
                   MOVE 'TRADEIN'      TO AB-DDNAME
                   MOVE WS-TRADEIN-FS  TO AB-FILE-STATUS
                   MOVE '8000-READ-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATION                                             *
      *================================================================*
       9000-TERMINATE.
           PERFORM 9050-CLOSE-FILES    THRU 9050-EXIT
           IF WS-TRADES-REJECTED > ZERO
           OR WS-WARNINGS-WRITTEN > ZERO
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
           PERFORM 9100-POST-CONTROL-TOTALS THRU 9100-EXIT
           PERFORM 9200-WRITE-AUDIT-END THRU 9200-EXIT
           PERFORM 9300-DISPLAY-STATISTICS THRU 9300-EXIT.
       9000-EXIT.
           EXIT.
      *
       9050-CLOSE-FILES.
           CLOSE TRADEIN-FILE
           IF NOT TRADEIN-OK
               MOVE 'TRADEIN'          TO AB-DDNAME
               MOVE WS-TRADEIN-FS      TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE ACCTMAST-FILE
           IF NOT ACCTMAST-OK
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE TRADEOUT-FILE
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE REJOUT-FILE
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF.
       9050-EXIT.
           EXIT.
      *
       9100-POST-CONTROL-TOTALS.
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
      *
           MOVE 'TRADES-IN'            TO CT-COUNTER-NAME
           MOVE WS-TRADES-READ         TO CT-COUNT
           MOVE WS-IN-AMT-HASH         TO CT-AMOUNT
           MOVE WS-IN-QTY-HASH         TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'TRADES-OUT'           TO CT-COUNTER-NAME
           MOVE WS-TRADES-WRITTEN      TO CT-COUNT
           MOVE WS-OUT-NET-TOTAL       TO CT-AMOUNT
           MOVE WS-OUT-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'REJECT-OUT'           TO CT-COUNTER-NAME
           MOVE WS-TRADES-REJECTED     TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
           MOVE WS-REJ-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'WARN-OUT'             TO CT-COUNTER-NAME
           MOVE WS-WARNINGS-WRITTEN    TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
                                          CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'USD-NET-OUT'          TO CT-COUNTER-NAME
           MOVE WS-TRADES-WRITTEN      TO CT-COUNT
           MOVE WS-OUT-USD-TOTAL       TO CT-AMOUNT
           MOVE ZERO                   TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9100-EXIT.
           EXIT.
      *
       9110-CALL-CMU080.
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9110-CALL-CMU080' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME    TO AB-KEY
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF.
       9110-EXIT.
           EXIT.
      *
       9200-WRITE-AUDIT-END.
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'END'                  TO AU-EVENT
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
                                          AU-MESSAGE
           IF WS-RETURN-CODE = ZERO
               MOVE 'I'                TO AU-SEVERITY
           ELSE
               MOVE 'W'                TO AU-SEVERITY
           END-IF
           MOVE WS-TRADES-WRITTEN      TO WS-DISP-COUNT
           STRING 'ENRICHMENT ENDED. TRADES OUT ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           IF WS-SEQ-ERRORS > ZERO
               MOVE 'SEQWARN'          TO AU-EVENT
               MOVE 'W'                TO AU-SEVERITY
               MOVE SPACES             TO AU-MESSAGE
               MOVE WS-SEQ-ERRORS      TO WS-DISP-COUNT
               STRING 'INPUT OUT OF ACCOUNT SEQUENCE '
                      WS-DISP-COUNT ' TIMES'
                      DELIMITED BY SIZE INTO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9200-EXIT.
           EXIT.
      *
       9300-DISPLAY-STATISTICS.
           DISPLAY '**************************************************'
           DISPLAY '* TCB200 - TRADE ENRICHMENT STATISTICS           *'
           DISPLAY '**************************************************'
           DISPLAY '* BUSINESS DATE              : ' DC-BUS-DATE
           MOVE WS-TRADES-READ         TO WS-DISP-COUNT
           DISPLAY '* TRADES READ                : ' WS-DISP-COUNT
           MOVE WS-NEW-TRADES          TO WS-DISP-COUNT
           DISPLAY '*   NEW                      : ' WS-DISP-COUNT
           MOVE WS-CANCEL-TRADES       TO WS-DISP-COUNT
           DISPLAY '*   CANCEL                   : ' WS-DISP-COUNT
           MOVE WS-CORRECT-TRADES      TO WS-DISP-COUNT
           DISPLAY '*   CORRECTION               : ' WS-DISP-COUNT
           MOVE WS-TRADES-WRITTEN      TO WS-DISP-COUNT
           DISPLAY '* TRADES ENRICHED            : ' WS-DISP-COUNT
           MOVE WS-TRADES-REJECTED     TO WS-DISP-COUNT
           DISPLAY '* TRADES REJECTED            : ' WS-DISP-COUNT
           MOVE WS-WARNINGS-WRITTEN    TO WS-DISP-COUNT
           DISPLAY '* WARNINGS                   : ' WS-DISP-COUNT
           MOVE WS-SEQ-ERRORS          TO WS-DISP-COUNT
           DISPLAY '* SEQUENCE WARNINGS          : ' WS-DISP-COUNT
           DISPLAY '*------------------------------------------------*'
           MOVE WS-SETTLE-COMPUTED     TO WS-DISP-COUNT
           DISPLAY '* SETTLE DATES COMPUTED      : ' WS-DISP-COUNT
           MOVE WS-SETTLE-SUPPLIED     TO WS-DISP-COUNT
           DISPLAY '* SETTLE DATES SUPPLIED      : ' WS-DISP-COUNT
           MOVE WS-ACCRUED-COMPUTED    TO WS-DISP-COUNT
           DISPLAY '* ACCRUED COMPUTED (CMU030)  : ' WS-DISP-COUNT
           MOVE WS-ACCRUED-OVERRIDES   TO WS-DISP-COUNT
           DISPLAY '* ACCRUED OVERRIDES          : ' WS-DISP-COUNT
           MOVE WS-COMM-COMPUTED       TO WS-DISP-COUNT
           DISPLAY '* COMMISSIONS COMPUTED       : ' WS-DISP-COUNT
           MOVE WS-COMM-OVERRIDES      TO WS-DISP-COUNT
           DISPLAY '* COMMISSION OVERRIDES       : ' WS-DISP-COUNT
           MOVE WS-COMM-HOUSE          TO WS-DISP-COUNT
           DISPLAY '* HOUSE ACCOUNTS NOT CHARGED : ' WS-DISP-COUNT
           MOVE WS-FEE-CALLS-E         TO WS-DISP-COUNT
           DISPLAY '* FEE CALLS TCU22E           : ' WS-DISP-COUNT
           MOVE WS-FEE-CALLS-F         TO WS-DISP-COUNT
           DISPLAY '* FEE CALLS TCU22F           : ' WS-DISP-COUNT
           MOVE WS-FX-CONVERSIONS      TO WS-DISP-COUNT
           DISPLAY '* FX CONVERSIONS             : ' WS-DISP-COUNT
           MOVE WS-FX-STALE            TO WS-DISP-COUNT
           DISPLAY '*   STALE RATES              : ' WS-DISP-COUNT
           MOVE WS-SHORT-WARNINGS      TO WS-DISP-COUNT
           DISPLAY '* SHORT SALE LOCATE FLAGS    : ' WS-DISP-COUNT
           MOVE WS-COMM-PCT-WARNINGS   TO WS-DISP-COUNT
           DISPLAY '* COMMISSION POLICY FLAGS    : ' WS-DISP-COUNT
           MOVE WS-LARGE-TRADES        TO WS-DISP-COUNT
           DISPLAY '* LARGE TRADES               : ' WS-DISP-COUNT
           MOVE WS-CROSS-CCY-TRADES    TO WS-DISP-COUNT
           DISPLAY '* CROSS CURRENCY TRADES      : ' WS-DISP-COUNT
           MOVE WS-SHORT-IN-CASH-ACCT  TO WS-DISP-COUNT
           DISPLAY '* SHORT SALES IN CASH ACCTS  : ' WS-DISP-COUNT
           MOVE WS-HOUSE-TRADES        TO WS-DISP-COUNT
           DISPLAY '* HOUSE ACCOUNT TRADES       : ' WS-DISP-COUNT
           DISPLAY '*------------------------------------------------*'
           MOVE WS-SEC-DB-CALLS        TO WS-DISP-COUNT
           DISPLAY '* SECURITY DB2 CALLS         : ' WS-DISP-COUNT
           MOVE WS-SEC-CACHE-HITS      TO WS-DISP-COUNT
           DISPLAY '* SECURITY CACHE HITS        : ' WS-DISP-COUNT
           MOVE WS-SEC-CACHE-COUNT     TO WS-DISP-COUNT
           DISPLAY '* SECURITY CACHE ENTRIES     : ' WS-DISP-COUNT
           MOVE WS-ACCT-READS          TO WS-DISP-COUNT
           DISPLAY '* ACCOUNT MASTER READS       : ' WS-DISP-COUNT
           MOVE WS-ACCT-REUSED         TO WS-DISP-COUNT
           DISPLAY '* ACCOUNT READS SAVED        : ' WS-DISP-COUNT
           DISPLAY '*------------------------------------------------*'
           MOVE WS-TOT-PRINCIPAL       TO WS-DISP-AMT
           DISPLAY '* TOTAL PRINCIPAL            : ' WS-DISP-AMT
           MOVE WS-TOT-COMMISSION      TO WS-DISP-AMT
           DISPLAY '* TOTAL COMMISSION           : ' WS-DISP-AMT
           MOVE WS-TOT-SEC-FEE         TO WS-DISP-AMT
           DISPLAY '* TOTAL SEC FEE              : ' WS-DISP-AMT
           MOVE WS-TOT-TAF-FEE         TO WS-DISP-AMT
           DISPLAY '* TOTAL TAF                  : ' WS-DISP-AMT
           MOVE WS-TOT-OTHER-FEES      TO WS-DISP-AMT
           DISPLAY '* TOTAL OTHER FEES           : ' WS-DISP-AMT
           MOVE WS-TOT-ACCRUED         TO WS-DISP-AMT
           DISPLAY '* TOTAL ACCRUED INTEREST     : ' WS-DISP-AMT
           MOVE WS-OUT-NET-TOTAL       TO WS-DISP-AMT
           DISPLAY '* TOTAL NET (TRADE CCY)      : ' WS-DISP-AMT
           MOVE WS-OUT-USD-TOTAL       TO WS-DISP-AMT
           DISPLAY '* TOTAL NET (USD)            : ' WS-DISP-AMT
           DISPLAY '*------------------------------------------------*'
           DISPLAY '* BY SOURCE        IN        OUT    REJECT       *'
           PERFORM 9310-DISPLAY-SOURCE THRU 9310-EXIT
               VARYING WS-SRC-IDX FROM 1 BY 1
               UNTIL WS-SRC-IDX > 4
           DISPLAY '*------------------------------------------------*'
           DISPLAY '* BY SECURITY TYPE  COUNT        PRINCIPAL        *'
           PERFORM 9320-DISPLAY-SEC-TYPE THRU 9320-EXIT
               VARYING WS-STY-IDX FROM 1 BY 1
               UNTIL WS-STY-IDX > WS-STY-USED
           DISPLAY '*------------------------------------------------*'
           DISPLAY '* BY CURRENCY  COUNT   NET (CCY)   NET (USD)      *'
           PERFORM 9330-DISPLAY-CCY    THRU 9330-EXIT
               VARYING WS-CST-IDX FROM 1 BY 1
               UNTIL WS-CST-IDX > WS-CST-USED
           DISPLAY '*------------------------------------------------*'
           DISPLAY '* LARGEST TRADES (USD NET)                       *'
           PERFORM 9340-DISPLAY-LARGE  THRU 9340-EXIT
               VARYING WS-LT-IDX FROM 1 BY 1
               UNTIL WS-LT-IDX > WS-LT-USED
           DISPLAY '*------------------------------------------------*'
           DISPLAY '* RETURN CODE                : ' WS-RETURN-CODE
           DISPLAY '**************************************************'.
       9300-EXIT.
           EXIT.
      *
       9310-DISPLAY-SOURCE.
           IF WS-SRC-IN (WS-SRC-IDX) = ZERO
               GO TO 9310-EXIT
           END-IF
           MOVE WS-SRC-IN (WS-SRC-IDX)  TO WS-DISP-COUNT
           DISPLAY '*   ' WS-SRC-CODE (WS-SRC-IDX)
                   '   IN  ' WS-DISP-COUNT
           MOVE WS-SRC-OUT (WS-SRC-IDX) TO WS-DISP-COUNT
           DISPLAY '*         OUT ' WS-DISP-COUNT
           MOVE WS-SRC-REJ (WS-SRC-IDX) TO WS-DISP-COUNT
           DISPLAY '*         REJ ' WS-DISP-COUNT
           MOVE WS-SRC-NET (WS-SRC-IDX) TO WS-DISP-AMT
           DISPLAY '*         NET ' WS-DISP-AMT.
       9310-EXIT.
           EXIT.
      *
       9320-DISPLAY-SEC-TYPE.
           MOVE WS-STY-COUNT (WS-STY-IDX)     TO WS-DISP-COUNT
           MOVE WS-STY-PRINCIPAL (WS-STY-IDX) TO WS-DISP-AMT
           DISPLAY '*   ' WS-STY-CODE (WS-STY-IDX) ' '
                   WS-DISP-COUNT ' ' WS-DISP-AMT.
       9320-EXIT.
           EXIT.
      *
       9330-DISPLAY-CCY.
           MOVE WS-CST-COUNT (WS-CST-IDX) TO WS-DISP-COUNT
           DISPLAY '*   ' WS-CST-CCY (WS-CST-IDX) ' COUNT '
                   WS-DISP-COUNT
           MOVE WS-CST-NET (WS-CST-IDX)   TO WS-DISP-AMT
           DISPLAY '*       NET ' WS-DISP-AMT
           MOVE WS-CST-USD (WS-CST-IDX)   TO WS-DISP-AMT
           DISPLAY '*       USD ' WS-DISP-AMT.
       9330-EXIT.
           EXIT.
      *
       9340-DISPLAY-LARGE.
           MOVE WS-LT-USD-NET (WS-LT-IDX) TO WS-DISP-AMT
           DISPLAY '*   ' WS-LT-TRADE-ID (WS-LT-IDX) ' '
                   WS-LT-ACCT-NO (WS-LT-IDX) ' '
                   WS-LT-CUSIP (WS-LT-IDX) ' '
                   WS-LT-SIDE (WS-LT-IDX) ' ' WS-DISP-AMT.
       9340-EXIT.
           EXIT.
      *
      *================================================================*
      * 99XX - ERROR ROUTINES                                          *
      *================================================================*
       9910-OPEN-ERROR.
           MOVE 1001                   TO AB-ABEND-CODE
           MOVE '1000-INITIALIZE'      TO AB-PARAGRAPH
           STRING 'OPEN FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9910-EXIT.
           EXIT.
      *
       9920-IO-ERROR.
           MOVE 1002                   TO AB-ABEND-CODE
           STRING 'I/O ERROR ON ' AB-DDNAME ' STATUS '
                  AB-FILE-STATUS
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9920-EXIT.
           EXIT.
      *
       9930-CLOSE-ERROR.
           MOVE 1002                   TO AB-ABEND-CODE
           MOVE '9050-CLOSE-FILES'     TO AB-PARAGRAPH
           STRING 'CLOSE FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9930-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'TCB200 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'TCB200 - ' AB-MESSAGE
           DISPLAY 'TCB200 - TRADES READ SO FAR ' WS-TRADES-READ
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
