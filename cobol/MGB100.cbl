       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGB100.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JUNE 1994.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGB100                                            *
      * DESCRIPTION: DAILY MARGIN REQUIREMENT.                         *
      *              FOR EVERY MARGIN ACCOUNT (ACCT-MARGIN-TYPE 'M',   *
      *              STATUS ACTIVE OR RESTRICTED) THE PROGRAM:         *
      *                - VALUES THE ACCOUNT FROM THE STOCK RECORD      *
      *                  (POSITION MASTER, VALUED BY SRB400) AND THE   *
      *                  CASH BALANCES (TD BALANCE, ALL CURRENCIES,    *
      *                  CONVERTED TO USD THROUGH CMU040)              *
      *                - APPLIES THE HOUSE RULE TABLE (MGRULES) TO     *
      *                  EACH POSITION: NON-MARGINABLE, HOUSE LONG,    *
      *                  HOUSE SHORT BY SECURITY TYPE AND PRICE BAND / *
      *                  MATURITY BUCKET, MOST SPECIFIC RULE FIRST     *
      *                - COMPUTES REG T, HOUSE MAINTENANCE, ISSUER     *
      *                  CONCENTRATION ADD-ON AND MINIMUM EQUITY       *
      *                - SETS THE MARGIN STATUS G / H / T / M          *
      *              ONE REQUIREMENT RECORD PER ACCOUNT (MG.REQ) AND   *
      *              ONE DETAIL RECORD PER POSITION (MG.POSREQ).       *
      *              MG.REQ FEEDS THE CALL PROCESS (MGB200), MARGIN    *
      *              INTEREST (MGB300) AND THE RESERVE FORMULA.        *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD010 / STEP010  (IKJEFT01 - DB2 PLAN MSMGPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - PARAMETERS MGP100A                     *
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS     (CMACCT)*
      *              POSMAST  - MSEC.PROD.SR.POSITION.KSDS     (SRPOSN)*
      *              CASHBAL  - MSEC.PROD.SR.CASHBAL.KSDS      (SRCASH)*
      *              MGRULES  - MSEC.PROD.MG.RULES.KSDS        (MGRULE)*
      * OUTPUT     : REQOUT   - MSEC.PROD.MG.REQ(+1)            (MGREQ)*
      *              POSREQ   - MSEC.PROD.MG.POSREQ(+1)        (MGPREQ)*
      * CALLS      : CMD010 (SECURITY MASTER - TYPE, ISSUER, MATURITY) *
      *              CMU040 (FX TO USD), CMU050, CMU060, CMU080,       *
      *              CMASM01                                           *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  STALE / UNVALUED POSITIONS, SECURITY NOT ON    *
      *                 MASTER, NO RULE FOUND, NO FX RATE              *
      *              8  A FIRM-WIDE RULE (RT / MN / CN) IS MISSING FROM*
      *                 MGRULES - STANDARD VALUES USED, REVIEW OUTPUT  *
      *----------------------------------------------------------------*
      * REQUIREMENT FORMULAS (MARGIN DEPT PROCEDURE MD-114)            *
      *   LONG MV   = SUM MV USD OF POSITIONS WITH TD QTY > 0          *
      *   SHORT MV  = SUM ABS(MV USD) OF POSITIONS WITH TD QTY < 0     *
      *   CASH      = SUM TD BALANCE (USD)  DEBIT = -CASH IF CASH < 0  *
      *   EQUITY    = LONG MV - SHORT MV + CASH                        *
      *   HOUSE REQ = SUM OF POSITION REQUIREMENTS (HL / HS / NM)      *
      *   REG T REQ = RT PCT X MARGINABLE LONG MV + RT PCT X SHORT MV  *
      *   CONC ADD  = CN PCT X LARGEST ISSUER MV WHEN THE LARGEST      *
      *               ISSUER IS MORE THAN CN THRESHOLD OF LONG MV      *
      *   EXCESS    = EQUITY - HOUSE REQ - CONC ADD                    *
      *   SMA       = EQUITY - REG T REQ (NOT LESS THAN ZERO)          *
      *   STATUS    H EQUITY < HOUSE + ADD-ON                          *
      *             T EQUITY < REG T                                   *
      *             M DEBIT > 0 AND EQUITY < MINIMUM (MN)              *
      *             G OTHERWISE                                        *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1994-06-13 DWB  ORIGINAL - REPLACES MARGIN SYSTEM     CHG00977 *
      *                 TAPE MGN0100 (SERVICE BUREAU)                  *
      * 1994-11-07 DWB  SHORT POSITIONS - HOUSE SHORT RULES   CHG01020 *
      * 1995-03-20 RJK  NON-MARGINABLE (LOW PRICED) SECURITIES CHG01288*
      * 1996-09-30 LFM  CALLS AND INTEREST SPLIT TO MGB200 /  CHG02390 *
      *                 MGB300 - REQUIREMENT FILE MG.REQ               *
      * 1997-05-12 RJK  MINIMUM EQUITY RULE MN                CHG03511 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-04-09 KAP  DECIMALIZATION - PER SHARE MINIMUMS   CHG08201 *
      * 2003-10-20 KAP  BOND MATURITY BUCKETS FROM SEC MASTER CHG11702 *
      * 2009-12-14 SPA  USD MARKET VALUE, MULTI-CCY CASH      CHG19002 *
      * 2011-02-28 SPA  ISSUER CONCENTRATION ADD-ON           CHG21340 *
      * 2011-08-29 SPA  STALE PRICE FLAG                      CHG22190 *
      * 2014-01-13 SPA  SECURITY CACHE                        CHG25507 *
      * 2016-10-03 SPA  POSITION DETAIL FILE MG.POSREQ        CHG30188 *
      * 2019-04-15 MHC  RULE EFFECTIVE DATE, RULE USAGE STATS CHG33702 *
      * 2022-01-10 MHC  TRACE-ACCT PARAMETER FOR MARGIN DEPT  CHG37020 *
      * 2024-05-20 NVR  T+1 REVIEW - TD QTY STAYS THE BASIS   CHG40551 *
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
                  RECORD KEY IS ACCT-NO
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
           SELECT MGRULES-FILE   ASSIGN TO MGRULES
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS MGR-KEY
                  FILE STATUS IS WS-MGRULES-STATUS.
           SELECT REQOUT-FILE    ASSIGN TO REQOUT
                  FILE STATUS IS WS-REQOUT-STATUS.
           SELECT POSREQ-FILE    ASSIGN TO POSREQ
                  FILE STATUS IS WS-POSREQ-STATUS.
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
           COPY CMACCT.
       FD  POSMAST-FILE.
           COPY SRPOSN.
       FD  CASHBAL-FILE.
           COPY SRCASH.
       FD  MGRULES-FILE.
           COPY MGRULE.
       FD  REQOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REQOUT-REC                  PIC X(250).
       FD  POSREQ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGPREQ.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGB100'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
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
           05  WS-MGRULES-STATUS       PIC X(02)  VALUE '00'.
               88  MGRULES-OK                     VALUE '00'.
               88  MGRULES-EOF                    VALUE '10'.
               88  MGRULES-NOTFND                 VALUE '23'.
           05  WS-REQOUT-STATUS        PIC X(02)  VALUE '00'.
           05  WS-POSREQ-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-ACCT-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCOUNTS                VALUE 'Y'.
           05  WS-POSN-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCT-POSITIONS          VALUE 'Y'.
           05  WS-CASH-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCT-CASH               VALUE 'Y'.
           05  WS-RULE-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-RULES                   VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-IN-SCOPE-SW          PIC X(01)  VALUE 'N'.
               88  ACCOUNT-IN-SCOPE               VALUE 'Y'.
           05  WS-RULE-FOUND-SW        PIC X(01)  VALUE 'N'.
               88  RULE-FOUND                     VALUE 'Y'.
           05  WS-SEC-FOUND-SW         PIC X(01)  VALUE 'N'.
               88  SECURITY-FOUND                 VALUE 'Y'.
           05  WS-TRACE-SW             PIC X(01)  VALUE 'N'.
               88  TRACE-THIS-ACCOUNT             VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * PARAMETERS (SYSIN MGP100A)                                     *
      *----------------------------------------------------------------*
       01  WS-PARMS.
           05  WS-PARM-STALE-CHECK     PIC X(01)  VALUE 'Y'.
               88  STALE-CHECK-ON                 VALUE 'Y'.
           05  WS-PARM-TRACE-ACCT      PIC X(10)  VALUE SPACES.
           05  WS-PARM-ZERO-MV-DETAIL  PIC X(01)  VALUE 'N'.
               88  WRITE-ZERO-MV-DETAIL           VALUE 'Y'.
           05  WS-PARM-RULE-DATE       PIC 9(08)  VALUE ZERO.
           05  WS-PARM-CARD-CNT        PIC S9(04) COMP  VALUE ZERO.
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(30).
       01  WS-RULE-EFF-DATE            PIC 9(08)  VALUE ZERO.
      *----------------------------------------------------------------*
      * FIRM-WIDE RULES.  THE VALUES BELOW ARE THE REG T / NYSE 431    *
      * STANDARD AND ARE USED ONLY WHEN THE ROW IS MISSING FROM        *
      * MGRULES (RC 8 - MARGIN DEPT MUST RELOAD THE TABLE).            *
      *----------------------------------------------------------------*
       01  WS-FIRM-RULES.
           05  WS-RT-PCT               PIC S9(03)V9(04) COMP-3
                                                     VALUE +50.0000.
           05  WS-MN-AMOUNT            PIC S9(11)V99    COMP-3
                                                     VALUE +2000.00.
           05  WS-CN-PCT               PIC S9(03)V9(04) COMP-3
                                                     VALUE +10.0000.
           05  WS-CN-THRESHOLD         PIC S9(03)V9(04) COMP-3
                                                     VALUE +25.0000.
           05  WS-RT-FOUND-SW          PIC X(01)  VALUE 'N'.
           05  WS-MN-FOUND-SW          PIC X(01)  VALUE 'N'.
           05  WS-CN-FOUND-SW          PIC X(01)  VALUE 'N'.
       01  WS-FIRM-RULE-KEYS.
           05  WS-RT-KEY               PIC X(08)  VALUE 'RT******'.
           05  WS-MN-KEY               PIC X(08)  VALUE 'MN******'.
           05  WS-CN-KEY               PIC X(08)  VALUE 'CN******'.
       01  WS-NO-RULE-PCT              PIC S9(03)V9(04) COMP-3
                                                     VALUE +100.0000.
       01  WS-LOW-PRICE-LIMIT          PIC S9(09)V9(08) COMP-3
                                                     VALUE +5.
      *----------------------------------------------------------------*
      * RULE TABLE - LOADED FROM MGRULES AT START OF RUN.  ROWS WITH   *
      * AN EFFECTIVE DATE AFTER THE BUSINESS DATE ARE NOT LOADED.      *
      *----------------------------------------------------------------*
       01  WS-RULE-TABLE.
           05  WS-RULE-COUNT           PIC S9(04) COMP  VALUE ZERO.
           05  WS-RULE-MAX             PIC S9(04) COMP  VALUE +500.
           05  WS-RULE-ENTRY OCCURS 500 TIMES INDEXED BY RL-IDX.
               10  WS-RL-KEY.
                   15  WS-RL-RULE-TYPE PIC X(02).
                   15  WS-RL-SEC-TYPE  PIC X(02).
                   15  WS-RL-QUALIFIER PIC X(04).
               10  WS-RL-PCT           PIC S9(03)V9(04) COMP-3.
               10  WS-RL-PER-SHARE     PIC S9(05)V99    COMP-3.
               10  WS-RL-AMOUNT        PIC S9(11)V99    COMP-3.
               10  WS-RL-THRESHOLD     PIC S9(03)V9(04) COMP-3.
               10  WS-RL-EFF-DATE      PIC 9(08).
               10  WS-RL-DESC          PIC X(30).
               10  WS-RL-USED-CNT      PIC S9(07)       COMP-3.
       01  WS-RULE-STATS.
           05  WS-RULES-READ           PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-RULES-FUTURE         PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-RULES-BAD            PIC S9(05) COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * RULE LOOKUP                                                    *
      *----------------------------------------------------------------*
       01  WS-LOOKUP-KEY.
           05  WS-LK-RULE-TYPE         PIC X(02).
           05  WS-LK-SEC-TYPE          PIC X(02).
           05  WS-LK-QUALIFIER         PIC X(04).
       01  WS-LOOKUP-SAVE              PIC X(08).
       01  WS-LOOKUP-LEVELS            PIC 9(01)  VALUE 3.
       01  WS-LOOKUP-LEVEL             PIC 9(01)  VALUE ZERO.
       01  WS-FOUND-RULE.
           05  WS-FR-KEY               PIC X(08).
           05  WS-FR-PCT               PIC S9(03)V9(04) COMP-3.
           05  WS-FR-PER-SHARE         PIC S9(05)V99    COMP-3.
           05  WS-FR-AMOUNT            PIC S9(11)V99    COMP-3.
           05  WS-FR-THRESHOLD         PIC S9(03)V9(04) COMP-3.
       01  WS-ANY-QUALIFIER            PIC X(04)  VALUE '****'.
       01  WS-ANY-SEC-TYPE             PIC X(02)  VALUE '**'.
      *----------------------------------------------------------------*
      * SECURITY CACHE (CHG25507)                                      *
      *----------------------------------------------------------------*
       01  WS-SEC-CACHE.
           05  WS-SC-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-SC-MAX               PIC S9(04) COMP  VALUE +4000.
           05  WS-SC-ENTRY OCCURS 4000 TIMES INDEXED BY SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-FOUND         PIC X(01).
               10  WS-SC-SEC-TYPE      PIC X(02).
               10  WS-SC-ISSUER-ID     PIC X(06).
               10  WS-SC-MATURITY      PIC 9(08).
               10  WS-SC-STATUS        PIC X(01).
      *----------------------------------------------------------------*
      * ACCOUNT ACCUMULATORS                                           *
      *----------------------------------------------------------------*
       01  WS-ACCOUNT-WORK.
           05  WS-AW-ACCT-NO           PIC X(10).
           05  WS-AW-LONG-MV           PIC S9(15)V99    COMP-3.
           05  WS-AW-SHORT-MV          PIC S9(15)V99    COMP-3.
           05  WS-AW-MARG-LONG-MV      PIC S9(15)V99    COMP-3.
           05  WS-AW-NONMARG-MV        PIC S9(15)V99    COMP-3.
           05  WS-AW-CASH-USD          PIC S9(15)V99    COMP-3.
           05  WS-AW-DEBIT             PIC S9(15)V99    COMP-3.
           05  WS-AW-CREDIT            PIC S9(15)V99    COMP-3.
           05  WS-AW-EQUITY            PIC S9(15)V99    COMP-3.
           05  WS-AW-HOUSE-REQ         PIC S9(15)V99    COMP-3.
           05  WS-AW-REGT-LONG         PIC S9(15)V99    COMP-3.
           05  WS-AW-REGT-SHORT        PIC S9(15)V99    COMP-3.
           05  WS-AW-REGT-REQ          PIC S9(15)V99    COMP-3.
           05  WS-AW-CONC-ADDON        PIC S9(15)V99    COMP-3.
           05  WS-AW-TOTAL-HOUSE       PIC S9(15)V99    COMP-3.
           05  WS-AW-EXCESS            PIC S9(15)V99    COMP-3.
           05  WS-AW-SMA               PIC S9(15)V99    COMP-3.
           05  WS-AW-POSN-CNT          PIC S9(05)       COMP-3.
           05  WS-AW-CASH-CNT          PIC S9(05)       COMP-3.
           05  WS-AW-LARGEST-ISSUER    PIC X(06).
           05  WS-AW-LARGEST-MV        PIC S9(15)V99    COMP-3.
           05  WS-AW-LARGEST-PCT       PIC S9(03)V9(04) COMP-3.
           05  WS-AW-STATUS            PIC X(01).
           05  WS-AW-STALE-FLAG        PIC X(01).
      *----------------------------------------------------------------*
      * ISSUER TABLE FOR THE ACCOUNT BEING PROCESSED (CHG21340)        *
      *----------------------------------------------------------------*
       01  WS-ISSUER-TABLE.
           05  WS-IT-COUNT             PIC S9(04) COMP  VALUE ZERO.
           05  WS-IT-MAX               PIC S9(04) COMP  VALUE +400.
           05  WS-IT-ENTRY OCCURS 400 TIMES INDEXED BY IT-IDX.
               10  WS-IT-ISSUER        PIC X(06).
               10  WS-IT-LONG-MV       PIC S9(15)V99    COMP-3.
      *----------------------------------------------------------------*
      * POSITION WORK                                                  *
      *----------------------------------------------------------------*
       01  WS-POSITION-WORK.
           05  WS-PW-QTY               PIC S9(11)V9(04) COMP-3.
           05  WS-PW-ABS-QTY           PIC S9(11)V9(04) COMP-3.
           05  WS-PW-PRICE             PIC S9(09)V9(08) COMP-3.
           05  WS-PW-MV-USD            PIC S9(15)V99    COMP-3.
           05  WS-PW-ABS-MV            PIC S9(15)V99    COMP-3.
           05  WS-PW-SEC-TYPE          PIC X(02).
           05  WS-PW-ISSUER            PIC X(06).
           05  WS-PW-MATURITY          PIC 9(08).
           05  WS-PW-MAT-DIFF          PIC S9(09)       COMP-3.
           05  WS-PW-QUALIFIER         PIC X(04).
           05  WS-PW-PCT               PIC S9(03)V9(04) COMP-3.
           05  WS-PW-PCT-REQ           PIC S9(15)V99    COMP-3.
           05  WS-PW-SHARE-REQ         PIC S9(15)V99    COMP-3.
           05  WS-PW-REQ               PIC S9(15)V99    COMP-3.
           05  WS-PW-RULE-KEY          PIC X(08).
           05  WS-PW-SIDE              PIC X(01).
               88  PW-LONG                        VALUE 'L'.
               88  PW-SHORT                       VALUE 'S'.
           05  WS-PW-PER-SHARE-SW      PIC X(01).
           05  WS-PW-MARGINABLE-SW     PIC X(01).
               88  PW-MARGINABLE                  VALUE 'Y'.
               88  PW-NON-MARGINABLE              VALUE 'N'.
           05  WS-PW-STALE-SW          PIC X(01).
      *----------------------------------------------------------------*
      * MATURITY BUCKETS - YEARS TO MATURITY FROM CCYYMMDD DIFFERENCE  *
      *----------------------------------------------------------------*
       01  WS-MATURITY-LIMITS.
           05  WS-MAT-1-YEAR           PIC S9(09) COMP-3 VALUE +10000.
           05  WS-MAT-5-YEARS          PIC S9(09) COMP-3 VALUE +50000.
           05  WS-MAT-10-YEARS         PIC S9(09) COMP-3 VALUE +100000.
      *----------------------------------------------------------------*
      * CASH                                                           *
      *----------------------------------------------------------------*
       01  WS-CASH-WORK.
           05  WS-CW-BAL-LOCAL         PIC S9(15)V99    COMP-3.
           05  WS-CW-BAL-USD           PIC S9(15)V99    COMP-3.
       01  WS-START-KEYS.
           05  WS-POS-START-KEY.
               10  WS-PSK-ACCT         PIC X(10).
               10  WS-PSK-REST         PIC X(13).
           05  WS-CSH-START-KEY.
               10  WS-CSK-ACCT         PIC X(10).
               10  WS-CSK-REST         PIC X(03).
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-ACCT-READ-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-IN-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FIRM-SKIP-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-ACCT-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STATUS-SKIP-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REQ-OUT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSREQ-OUT-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-READ-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-ZERO-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-LONG-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-SHORT-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-NM-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-UNVALUED-CNT    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-STALE-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PER-SHARE-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-SEC-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-RULE-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-READ-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-FX-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STALE-FX-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STALE-ACCT-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DEFICIT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STAT-G-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STAT-H-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STAT-T-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STAT-M-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CONC-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ISSUER-OVFL-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMD010-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMU040-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-TOT-LONG-MV          PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-SHORT-MV         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-EQUITY           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-DEBIT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-CREDIT           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-HOUSE-REQ        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-REGT-REQ         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-CONC-ADDON       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-DEFICIT          PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-POSREQ-MV        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-POSREQ-REQ       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-POSREQ-QTY       PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * BRANCH TOTALS FOR THE BRANCH MANAGER SUMMARY (SYSOUT) - KEPT   *
      * FROM THE 1994 LISTING, THE MARGIN DEPT STILL READS IT          *
      *----------------------------------------------------------------*
       01  WS-BRANCH-TABLE.
           05  WS-BT-COUNT             PIC S9(04) COMP  VALUE ZERO.
           05  WS-BT-MAX               PIC S9(04) COMP  VALUE +100.
           05  WS-BT-ENTRY OCCURS 100 TIMES INDEXED BY BT-IDX.
               10  WS-BT-BRANCH        PIC X(03).
               10  WS-BT-ACCTS         PIC S9(07)       COMP-3.
               10  WS-BT-CALLS         PIC S9(07)       COMP-3.
               10  WS-BT-WARN          PIC S9(07)       COMP-3.
               10  WS-BT-EQUITY        PIC S9(15)V99    COMP-3.
               10  WS-BT-DEBIT         PIC S9(15)V99    COMP-3.
               10  WS-BT-DEFICIT       PIC S9(15)V99    COMP-3.
       01  WS-BT-OVFL-CNT              PIC S9(07) COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * EARLY WARNING - EXCESS UNDER 5 PCT OF EQUITY, NOT YET IN CALL  *
      *----------------------------------------------------------------*
       01  WS-WARN-PCT                 PIC S9(03)V99    COMP-3
                                                     VALUE +5.00.
       01  WS-WARN-LIMIT               PIC S9(15)V99    COMP-3.
       01  WS-WARN-CNT                 PIC S9(07)       COMP-3
                                                     VALUE ZERO.
       01  WS-WARN-SHOWN               PIC S9(07)       COMP-3
                                                     VALUE ZERO.
       01  WS-WARN-SHOW-MAX            PIC S9(07)       COMP-3
                                                     VALUE +25.
       01  WS-RESTRICTED-CNT           PIC S9(07)       COMP-3
                                                     VALUE ZERO.
       01  WS-NO-ACTIVITY-CNT          PIC S9(07)       COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * CONTROL PROOF - DETAIL FILE AGAINST REQUIREMENT FILE           *
      *----------------------------------------------------------------*
       01  WS-PROOF-WORK.
           05  WS-PROOF-NET-MV         PIC S9(15)V99    COMP-3.
           05  WS-PROOF-DIFF           PIC S9(15)V99    COMP-3.
           05  WS-PROOF-REQ-DIFF       PIC S9(15)V99    COMP-3.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-PCT             PIC -ZZ9.9999.
           05  WS-DISP-QTY             PIC -ZZZ,ZZZ,ZZ9.9999.
           05  WS-DISP-SHR             PIC ZZ,ZZ9.99.
           05  WS-DISP-SMALL-1         PIC ZZZ,ZZ9.
           05  WS-DISP-SMALL-2         PIC ZZZ,ZZ9.
       COPY MGREQ.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMSECLNK.
       COPY CMFXLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMJILNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-ACCOUNT THRU 2000-EXIT
               UNTIL END-OF-ACCOUNTS.
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
           OR DC-BUS-DATE NOT NUMERIC
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
           MOVE 'MARGIN REQUIREMENT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           OPEN INPUT MGRULES-FILE.
           IF WS-MGRULES-STATUS NOT = '00'
               MOVE 'MGRULES' TO AB-DDNAME
               MOVE WS-MGRULES-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 1200-LOAD-RULES THRU 1200-EXIT.
           CLOSE MGRULES-FILE.
           PERFORM 1300-FIRM-RULES THRU 1300-EXIT.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT CASHBAL-FILE.
           IF WS-CASHBAL-STATUS NOT = '00'
               MOVE 'CASHBAL' TO AB-DDNAME
               MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT REQOUT-FILE.
           IF WS-REQOUT-STATUS NOT = '00'
               MOVE 'REQOUT' TO AB-DDNAME
               MOVE WS-REQOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT POSREQ-FILE.
           IF WS-POSREQ-STATUS NOT = '00'
               MOVE 'POSREQ' TO AB-DDNAME
               MOVE WS-POSREQ-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-ACCOUNT THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PARAMETER CARDS  KEYWORD=VALUE, '*' IN COLUMN 1 = COMMENT      *
      *----------------------------------------------------------------*
       1100-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF NOT PARMCARD-OK
               DISPLAY 'MGB100 NO SYSIN PARAMETERS - DEFAULTS USED'
               GO TO 1100-EXIT
           END-IF.
           PERFORM 1110-READ-PARM-CARD THRU 1110-EXIT
               UNTIL END-OF-PARMS.
           CLOSE PARMCARD.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-READ-PARM-CARD.
      *----------------------------------------------------------------*
           READ PARMCARD.
           IF PARMCARD-EOF
               MOVE 'Y' TO WS-PARM-EOF-SW
               GO TO 1110-EXIT
           END-IF.
           IF NOT PARMCARD-OK
               MOVE 'SYSIN' TO AB-DDNAME
               MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '1110-READ-PARM-CARD' TO AB-PARAGRAPH
               MOVE 'READ FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF PARMCARD-REC (1:1) = '*' OR PARMCARD-REC = SPACES
               GO TO 1110-EXIT
           END-IF.
           ADD 1 TO WS-PARM-CARD-CNT.
           DISPLAY 'MGB100 PARM: ' PARMCARD-REC (1:60).
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARMCARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING.
           EVALUATE WS-PARM-KEYWORD
               WHEN 'STALE-CHECK'
                   MOVE WS-PARM-VALUE (1:1) TO WS-PARM-STALE-CHECK
               WHEN 'TRACE-ACCT'
                   MOVE WS-PARM-VALUE (1:10) TO WS-PARM-TRACE-ACCT
               WHEN 'ZERO-MV-DETAIL'
                   MOVE WS-PARM-VALUE (1:1) TO WS-PARM-ZERO-MV-DETAIL
               WHEN 'RULE-DATE'
                   IF WS-PARM-VALUE (1:8) IS NUMERIC
                       MOVE WS-PARM-VALUE (1:8) TO WS-PARM-RULE-DATE
                   ELSE
                       DISPLAY 'MGB100 RULE-DATE NOT NUMERIC - IGNORED'
                   END-IF
               WHEN OTHER
                   DISPLAY 'MGB100 UNKNOWN PARAMETER IGNORED: '
                           WS-PARM-KEYWORD
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *================================================================*
      * LOAD THE RULE TABLE.  THE KSDS IS READ IN KEY ORDER; THE       *
      * EFFECTIVE DATE TEST USES THE BUSINESS DATE UNLESS RULE-DATE    *
      * IS GIVEN (WHAT-IF RUNS FOR THE MARGIN DEPT - CHG33702).        *
      *================================================================*
       1200-LOAD-RULES.
           IF WS-PARM-RULE-DATE > ZERO
               MOVE WS-PARM-RULE-DATE TO WS-RULE-EFF-DATE
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'RULEDATE'     TO AU-EVENT
               MOVE 'W'            TO AU-SEVERITY
               MOVE WS-PARM-RULE-DATE TO AU-KEY
               MOVE 'MARGIN RULES SELECTED AS OF OVERRIDE DATE'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           ELSE
               MOVE DC-BUS-DATE TO WS-RULE-EFF-DATE
           END-IF.
           MOVE LOW-VALUES TO MGR-KEY.
           START MGRULES-FILE KEY IS NOT LESS THAN MGR-KEY.
           EVALUATE TRUE
               WHEN MGRULES-OK
                   CONTINUE
               WHEN MGRULES-NOTFND
                   MOVE 'MGRULES' TO AB-DDNAME
                   MOVE WS-MGRULES-STATUS TO AB-FILE-STATUS
                   MOVE 1008 TO AB-ABEND-CODE
                   MOVE '1200-LOAD-RULES' TO AB-PARAGRAPH
                   MOVE 'MARGIN RULE TABLE IS EMPTY' TO AB-MESSAGE
                   GO TO 9999-ABEND
               WHEN OTHER
                   MOVE 'MGRULES' TO AB-DDNAME
                   MOVE WS-MGRULES-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '1200-LOAD-RULES' TO AB-PARAGRAPH
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 1210-READ-RULE THRU 1210-EXIT
               UNTIL END-OF-RULES.
           DISPLAY 'MGB100 RULES LOADED  : ' WS-RULE-COUNT
                   '  NOT YET EFFECTIVE : ' WS-RULES-FUTURE
                   '  INVALID : ' WS-RULES-BAD.
       1200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1210-READ-RULE.
      *----------------------------------------------------------------*
           READ MGRULES-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN MGRULES-OK
                   CONTINUE
               WHEN MGRULES-EOF
                   MOVE 'Y' TO WS-RULE-EOF-SW
                   GO TO 1210-EXIT
               WHEN OTHER
                   MOVE 'MGRULES' TO AB-DDNAME
                   MOVE WS-MGRULES-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '1210-READ-RULE' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           ADD 1 TO WS-RULES-READ.
           IF MGR-PCT NOT NUMERIC
           OR MGR-PER-SHARE-MIN NOT NUMERIC
           OR MGR-AMOUNT NOT NUMERIC
           OR MGR-THRESHOLD-PCT NOT NUMERIC
           OR MGR-EFF-DATE NOT NUMERIC
               ADD 1 TO WS-RULES-BAD
               DISPLAY 'MGB100 RULE ' MGR-KEY ' HAS INVALID DATA - '
                       'NOT LOADED'
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               GO TO 1210-EXIT
           END-IF.
           IF MGR-EFF-DATE > WS-RULE-EFF-DATE
               ADD 1 TO WS-RULES-FUTURE
               DISPLAY 'MGB100 RULE ' MGR-KEY ' EFFECTIVE '
                       MGR-EFF-DATE ' - NOT YET IN FORCE'
               GO TO 1210-EXIT
           END-IF.
           IF WS-RULE-COUNT NOT < WS-RULE-MAX
               MOVE 'MGRULES' TO AB-DDNAME
               MOVE 1007 TO AB-ABEND-CODE
               MOVE '1210-READ-RULE' TO AB-PARAGRAPH
               MOVE MGR-KEY TO AB-KEY
               MOVE 'RULE TABLE OVERFLOW - INCREASE WS-RULE-MAX'
                                    TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-RULE-COUNT.
           SET RL-IDX TO WS-RULE-COUNT.
           MOVE MGR-KEY           TO WS-RL-KEY (RL-IDX).
           MOVE MGR-PCT           TO WS-RL-PCT (RL-IDX).
           MOVE MGR-PER-SHARE-MIN TO WS-RL-PER-SHARE (RL-IDX).
           MOVE MGR-AMOUNT        TO WS-RL-AMOUNT (RL-IDX).
           MOVE MGR-THRESHOLD-PCT TO WS-RL-THRESHOLD (RL-IDX).
           MOVE MGR-EFF-DATE      TO WS-RL-EFF-DATE (RL-IDX).
           MOVE MGR-DESC          TO WS-RL-DESC (RL-IDX).
           MOVE ZERO              TO WS-RL-USED-CNT (RL-IDX).
       1210-EXIT.
           EXIT.
      *================================================================*
      * FIRM-WIDE RULES RT / MN / CN.  A MISSING ROW IS NOT FATAL -    *
      * THE STANDARD VALUE IS KEPT AND THE STEP ENDS RC 8 (CHG03511).  *
      *================================================================*
       1300-FIRM-RULES.
           MOVE WS-RT-KEY TO WS-LOOKUP-KEY.
           PERFORM 7100-SEARCH-RULE THRU 7100-EXIT.
           IF RULE-FOUND
               MOVE 'Y' TO WS-RT-FOUND-SW
               MOVE WS-FR-PCT TO WS-RT-PCT
           END-IF.
           MOVE WS-MN-KEY TO WS-LOOKUP-KEY.
           PERFORM 7100-SEARCH-RULE THRU 7100-EXIT.
           IF RULE-FOUND
               MOVE 'Y' TO WS-MN-FOUND-SW
               MOVE WS-FR-AMOUNT TO WS-MN-AMOUNT
           END-IF.
           MOVE WS-CN-KEY TO WS-LOOKUP-KEY.
           PERFORM 7100-SEARCH-RULE THRU 7100-EXIT.
           IF RULE-FOUND
               MOVE 'Y' TO WS-CN-FOUND-SW
               MOVE WS-FR-PCT       TO WS-CN-PCT
               MOVE WS-FR-THRESHOLD TO WS-CN-THRESHOLD
           END-IF.
           IF WS-RT-FOUND-SW = 'N' OR WS-MN-FOUND-SW = 'N'
           OR WS-CN-FOUND-SW = 'N'
               DISPLAY 'MGB100 *** FIRM RULE MISSING FROM MGRULES - '
                       'STANDARD VALUES USED ***'
               DISPLAY 'MGB100     RT FOUND ' WS-RT-FOUND-SW
                       '  MN FOUND ' WS-MN-FOUND-SW
                       '  CN FOUND ' WS-CN-FOUND-SW
               MOVE 8 TO WS-RETURN-CODE
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'NORULE'       TO AU-EVENT
               MOVE 'E'            TO AU-SEVERITY
               MOVE 'RT/MN/CN'     TO AU-KEY
               MOVE 'FIRM MARGIN RULE MISSING - DEFAULTS APPLIED'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF.
           MOVE WS-RT-PCT TO WS-DISP-PCT.
           DISPLAY 'MGB100 REG T PCT      : ' WS-DISP-PCT.
           MOVE WS-MN-AMOUNT TO WS-DISP-AMT.
           DISPLAY 'MGB100 MINIMUM EQUITY : ' WS-DISP-AMT.
           MOVE WS-CN-THRESHOLD TO WS-DISP-PCT.
           DISPLAY 'MGB100 CONC THRESHOLD : ' WS-DISP-PCT.
           MOVE WS-CN-PCT TO WS-DISP-PCT.
           DISPLAY 'MGB100 CONC ADD-ON PCT: ' WS-DISP-PCT.
       1300-EXIT.
           EXIT.
      *================================================================*
      * ONE ACCOUNT MASTER RECORD                                      *
      *================================================================*
       2000-PROCESS-ACCOUNT.
           ADD 1 TO WS-ACCT-READ-CNT.
           PERFORM 2100-CHECK-SCOPE THRU 2100-EXIT.
           IF NOT ACCOUNT-IN-SCOPE
               GO TO 2000-NEXT
           END-IF.
           ADD 1 TO WS-ACCT-IN-CNT.
           PERFORM 2200-INIT-ACCOUNT THRU 2200-EXIT.
           PERFORM 3000-ACCOUNT-POSITIONS THRU 3000-EXIT.
           PERFORM 4000-ACCOUNT-CASH THRU 4000-EXIT.
           PERFORM 5000-ACCOUNT-REQUIREMENT THRU 5000-EXIT.
           PERFORM 6000-WRITE-REQUIREMENT THRU 6000-EXIT.
       2000-NEXT.
           PERFORM 8000-READ-ACCOUNT THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SCOPE: MARGIN ACCOUNTS, ACTIVE OR RESTRICTED.  HOUSE AND       *
      * STREET ACCOUNTS ARE NEVER MARGINED.                            *
      *----------------------------------------------------------------*
       2100-CHECK-SCOPE.
           MOVE 'N' TO WS-IN-SCOPE-SW.
           IF ACCT-NO (1:1) < '0'
               ADD 1 TO WS-FIRM-SKIP-CNT
               GO TO 2100-EXIT
           END-IF.
           IF NOT ACCT-MARGIN-ACCT
               ADD 1 TO WS-CASH-ACCT-CNT
               GO TO 2100-EXIT
           END-IF.
           IF NOT ACCT-ACTIVE AND NOT ACCT-RESTRICTED
               ADD 1 TO WS-STATUS-SKIP-CNT
               GO TO 2100-EXIT
           END-IF.
           MOVE 'Y' TO WS-IN-SCOPE-SW.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-INIT-ACCOUNT.
      *----------------------------------------------------------------*
           INITIALIZE WS-ACCOUNT-WORK.
           MOVE ACCT-NO TO WS-AW-ACCT-NO.
           MOVE 'N'     TO WS-AW-STALE-FLAG.
           MOVE SPACES  TO WS-AW-LARGEST-ISSUER.
           MOVE ZERO    TO WS-IT-COUNT.
           IF WS-PARM-TRACE-ACCT NOT = SPACES
           AND WS-PARM-TRACE-ACCT = ACCT-NO
               MOVE 'Y' TO WS-TRACE-SW
               DISPLAY 'MGB100 TRACE ======== ACCOUNT ' ACCT-NO
                       ' ' ACCT-NAME
           ELSE
               MOVE 'N' TO WS-TRACE-SW
           END-IF.
       2200-EXIT.
           EXIT.
      *================================================================*
      * POSITIONS OF THE ACCOUNT - START ON THE ACCOUNT NUMBER AND     *
      * READ FORWARD UNTIL THE ACCOUNT CHANGES.                        *
      *================================================================*
       3000-ACCOUNT-POSITIONS.
           MOVE 'N'         TO WS-POSN-EOF-SW.
           MOVE ACCT-NO     TO WS-PSK-ACCT.
           MOVE LOW-VALUES  TO WS-PSK-REST.
           MOVE WS-POS-START-KEY TO POS-KEY.
           START POSMAST-FILE KEY IS NOT LESS THAN POS-KEY.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   CONTINUE
               WHEN POSMAST-NOTFND
                   MOVE 'Y' TO WS-POSN-EOF-SW
                   GO TO 3000-EXIT
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '3000-ACCOUNT-POSITIONS' TO AB-PARAGRAPH
                   MOVE ACCT-NO TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 8100-READ-POSITION THRU 8100-EXIT.
           PERFORM 3100-PROCESS-POSITION THRU 3100-EXIT
               UNTIL END-OF-ACCT-POSITIONS.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-PROCESS-POSITION.
      *----------------------------------------------------------------*
           ADD 1 TO WS-POSN-READ-CNT.
           IF POS-TD-QTY NOT NUMERIC
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3100-PROCESS-POSITION' TO AB-PARAGRAPH
               MOVE POS-KEY TO AB-KEY
               MOVE 'TRADE DATE QUANTITY NOT NUMERIC' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF POS-TD-QTY = ZERO
               ADD 1 TO WS-POSN-ZERO-CNT
               GO TO 3100-NEXT
           END-IF.
           PERFORM 3150-POSITION-VALUE THRU 3150-EXIT.
           PERFORM 3200-GET-SECURITY THRU 3200-EXIT.
           PERFORM 3300-QUALIFIER THRU 3300-EXIT.
           IF WS-PW-QTY > ZERO
               MOVE 'L' TO WS-PW-SIDE
               ADD 1 TO WS-POSN-LONG-CNT
               PERFORM 3400-LONG-REQUIREMENT THRU 3400-EXIT
           ELSE
               MOVE 'S' TO WS-PW-SIDE
               ADD 1 TO WS-POSN-SHORT-CNT
               PERFORM 3500-SHORT-REQUIREMENT THRU 3500-EXIT
           END-IF.
           ADD 1 TO WS-AW-POSN-CNT.
           ADD WS-PW-REQ TO WS-AW-HOUSE-REQ.
           IF TRACE-THIS-ACCOUNT
               PERFORM 3900-TRACE-POSITION THRU 3900-EXIT
           END-IF.
           IF WS-PW-MV-USD NOT = ZERO OR WRITE-ZERO-MV-DETAIL
               PERFORM 3800-WRITE-POSREQ THRU 3800-EXIT
           END-IF.
       3100-NEXT.
           PERFORM 8100-READ-POSITION THRU 8100-EXIT.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * VALUE FROM THE STOCK RECORD.  SRB400 SETS THE VALUATION FIELDS *
      * - A POSITION IT HAS NOT REACHED CARRIES NO VALUE (TREATED AS   *
      * ZERO AND FLAGGED STALE).                                       *
      *----------------------------------------------------------------*
       3150-POSITION-VALUE.
           MOVE POS-TD-QTY TO WS-PW-QTY.
           IF WS-PW-QTY < ZERO
               COMPUTE WS-PW-ABS-QTY = WS-PW-QTY * -1
           ELSE
               MOVE WS-PW-QTY TO WS-PW-ABS-QTY
           END-IF.
           MOVE 'N' TO WS-PW-STALE-SW.
           IF POS-MKT-VALUE-USD NUMERIC
               MOVE POS-MKT-VALUE-USD TO WS-PW-MV-USD
           ELSE
               MOVE ZERO TO WS-PW-MV-USD
               MOVE 'Y'  TO WS-PW-STALE-SW
               ADD 1 TO WS-POSN-UNVALUED-CNT
           END-IF.
           IF POS-MKT-PRICE NUMERIC
               MOVE POS-MKT-PRICE TO WS-PW-PRICE
           ELSE
               MOVE ZERO TO WS-PW-PRICE
               MOVE 'Y'  TO WS-PW-STALE-SW
           END-IF.
           IF STALE-CHECK-ON
               IF POS-PRICE-DATE NOT NUMERIC
                   MOVE 'Y' TO WS-PW-STALE-SW
               ELSE
                   IF POS-PRICE-DATE < DC-BUS-DATE
                       MOVE 'Y' TO WS-PW-STALE-SW
                   END-IF
               END-IF
           END-IF.
           IF WS-PW-STALE-SW = 'Y'
               ADD 1 TO WS-POSN-STALE-CNT
               MOVE 'Y' TO WS-AW-STALE-FLAG
           END-IF.
           IF WS-PW-MV-USD < ZERO
               COMPUTE WS-PW-ABS-MV = WS-PW-MV-USD * -1
           ELSE
               MOVE WS-PW-MV-USD TO WS-PW-ABS-MV
           END-IF.
       3150-EXIT.
           EXIT.
      *================================================================*
      * SECURITY MASTER - TYPE, ISSUER, MATURITY (CACHED)              *
      *================================================================*
       3200-GET-SECURITY.
           MOVE 'N' TO WS-SEC-FOUND-SW.
           SET SC-IDX TO 1.
           SEARCH WS-SC-ENTRY
               AT END
                   PERFORM 3210-LOAD-SECURITY THRU 3210-EXIT
               WHEN SC-IDX > WS-SC-USED
                   PERFORM 3210-LOAD-SECURITY THRU 3210-EXIT
               WHEN WS-SC-CUSIP (SC-IDX) = POS-CUSIP
                   CONTINUE
           END-SEARCH.
           IF WS-SC-FOUND (SC-IDX) = 'Y'
               MOVE 'Y'                     TO WS-SEC-FOUND-SW
               MOVE WS-SC-SEC-TYPE (SC-IDX)  TO WS-PW-SEC-TYPE
               MOVE WS-SC-ISSUER-ID (SC-IDX) TO WS-PW-ISSUER
               MOVE WS-SC-MATURITY (SC-IDX)  TO WS-PW-MATURITY
           ELSE
               MOVE POS-SEC-TYPE            TO WS-PW-SEC-TYPE
               MOVE SPACES                  TO WS-PW-ISSUER
               MOVE ZERO                    TO WS-PW-MATURITY
               ADD 1 TO WS-NO-SEC-CNT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
      *    NO ISSUER ON THE MASTER - USE THE CUSIP ISSUER NUMBER
           IF WS-PW-ISSUER = SPACES OR LOW-VALUES
               MOVE POS-CUSIP (1:6) TO WS-PW-ISSUER
           END-IF.
           IF WS-PW-SEC-TYPE = SPACES
               MOVE POS-SEC-TYPE TO WS-PW-SEC-TYPE
           END-IF.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3210-LOAD-SECURITY.
      *----------------------------------------------------------------*
           IF WS-SC-USED < WS-SC-MAX
               ADD 1 TO WS-SC-USED
           END-IF.
           SET SC-IDX TO WS-SC-USED.
           MOVE POS-CUSIP TO WS-SC-CUSIP (SC-IDX).
           MOVE 'GET '    TO SL-FUNCTION.
           MOVE POS-CUSIP TO SL-KEY-CUSIP.
           MOVE SPACES    TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           ADD 1 TO WS-CMD010-CALLS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA  TO SEC-MASTER-REC
                   MOVE 'Y'          TO WS-SC-FOUND (SC-IDX)
                   MOVE SEC-TYPE     TO WS-SC-SEC-TYPE (SC-IDX)
                   MOVE SEC-ISSUER-ID TO WS-SC-ISSUER-ID (SC-IDX)
                   MOVE SEC-STATUS   TO WS-SC-STATUS (SC-IDX)
                   MOVE ZERO         TO WS-SC-MATURITY (SC-IDX)
                   IF SEC-FIXED-INCOME
                       IF SEC-MATURITY-DATE NUMERIC
                           MOVE SEC-MATURITY-DATE
                                     TO WS-SC-MATURITY (SC-IDX)
                       END-IF
                   END-IF
               WHEN SL-NOT-FOUND
                   MOVE 'N'    TO WS-SC-FOUND (SC-IDX)
                   MOVE SPACES TO WS-SC-SEC-TYPE (SC-IDX)
                                  WS-SC-ISSUER-ID (SC-IDX)
                                  WS-SC-STATUS (SC-IDX)
                   MOVE ZERO   TO WS-SC-MATURITY (SC-IDX)
                   DISPLAY 'MGB100 SECURITY NOT ON MASTER ' POS-CUSIP
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '3210-LOAD-SECURITY' TO AB-PARAGRAPH
                   MOVE SL-SQLCODE TO AB-SQLCODE
                   MOVE POS-CUSIP TO AB-KEY
                   MOVE 'CMD010 SECURITY MASTER ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       3210-EXIT.
           EXIT.
      *================================================================*
      * RULE QUALIFIER: PRICE BAND FOR EQUITIES, MATURITY BUCKET FOR   *
      * BONDS (YEARS FROM THE CCYYMMDD DIFFERENCE), '****' OTHERWISE.  *
      *================================================================*
       3300-QUALIFIER.
           MOVE WS-ANY-QUALIFIER TO WS-PW-QUALIFIER.
           EVALUATE WS-PW-SEC-TYPE
               WHEN 'EQ'
               WHEN 'PF'
               WHEN 'AD'
                   IF WS-PW-PRICE < WS-LOW-PRICE-LIMIT
                       MOVE 'LT05' TO WS-PW-QUALIFIER
                   ELSE
                       MOVE 'GE05' TO WS-PW-QUALIFIER
                   END-IF
               WHEN 'CB'
               WHEN 'MU'
               WHEN 'GV'
                   IF WS-PW-MATURITY = ZERO
                       MOVE 'M99 ' TO WS-PW-QUALIFIER
                   ELSE
                       COMPUTE WS-PW-MAT-DIFF =
                           WS-PW-MATURITY - DC-BUS-DATE
                       EVALUATE TRUE
                           WHEN WS-PW-MAT-DIFF < WS-MAT-1-YEAR
                               MOVE 'M01 ' TO WS-PW-QUALIFIER
                           WHEN WS-PW-MAT-DIFF < WS-MAT-5-YEARS
                               MOVE 'M05 ' TO WS-PW-QUALIFIER
                           WHEN WS-PW-MAT-DIFF < WS-MAT-10-YEARS
                               MOVE 'M10 ' TO WS-PW-QUALIFIER
                           WHEN OTHER
                               MOVE 'M99 ' TO WS-PW-QUALIFIER
                       END-EVALUATE
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       3300-EXIT.
           EXIT.
      *================================================================*
      * LONG POSITION                                                  *
      *   1. NON-MARGINABLE (NM) - TYPE + QUALIFIER, THEN TYPE + ANY.  *
      *      NO FALL-BACK TO '**' - EVERYTHING WOULD BE NON-MARGINABLE *
      *   2. HOUSE LONG (HL) - TYPE + QUALIFIER, TYPE + ANY, '**' ANY  *
      *================================================================*
       3400-LONG-REQUIREMENT.
           MOVE 'N'  TO WS-PW-PER-SHARE-SW.
           MOVE 'Y'  TO WS-PW-MARGINABLE-SW.
           MOVE ZERO TO WS-PW-SHARE-REQ.
           MOVE 'NM'            TO WS-LK-RULE-TYPE.
           MOVE WS-PW-SEC-TYPE  TO WS-LK-SEC-TYPE.
           MOVE WS-PW-QUALIFIER TO WS-LK-QUALIFIER.
           MOVE 2 TO WS-LOOKUP-LEVELS.
           PERFORM 7000-FIND-RULE THRU 7000-EXIT.
           IF RULE-FOUND
               MOVE 'N' TO WS-PW-MARGINABLE-SW
               ADD 1 TO WS-POSN-NM-CNT
               ADD WS-PW-MV-USD TO WS-AW-NONMARG-MV
           ELSE
               MOVE 'HL'            TO WS-LK-RULE-TYPE
               MOVE WS-PW-SEC-TYPE  TO WS-LK-SEC-TYPE
               MOVE WS-PW-QUALIFIER TO WS-LK-QUALIFIER
               MOVE 3 TO WS-LOOKUP-LEVELS
               PERFORM 7000-FIND-RULE THRU 7000-EXIT
               ADD WS-PW-MV-USD TO WS-AW-MARG-LONG-MV
           END-IF.
           IF RULE-FOUND
               MOVE WS-FR-KEY TO WS-PW-RULE-KEY
               MOVE WS-FR-PCT TO WS-PW-PCT
           ELSE
               PERFORM 3450-NO-RULE THRU 3450-EXIT
           END-IF.
           COMPUTE WS-PW-PCT-REQ ROUNDED =
               WS-PW-MV-USD * WS-PW-PCT / 100.
           MOVE WS-PW-PCT-REQ TO WS-PW-REQ.
           ADD WS-PW-MV-USD TO WS-AW-LONG-MV.
           PERFORM 3700-ADD-ISSUER THRU 3700-EXIT.
       3400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * NO RULE AT ANY LEVEL - FULL VALUE REQUIRED UNTIL THE MARGIN    *
      * DEPT ADDS A ROW                                                *
      *----------------------------------------------------------------*
       3450-NO-RULE.
           ADD 1 TO WS-NO-RULE-CNT.
           MOVE WS-LOOKUP-SAVE  TO WS-PW-RULE-KEY.
           MOVE WS-NO-RULE-PCT  TO WS-PW-PCT.
           DISPLAY 'MGB100 NO MARGIN RULE FOR ' WS-LOOKUP-SAVE
                   ' ACCOUNT ' POS-ACCT-NO ' CUSIP ' POS-CUSIP.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       3450-EXIT.
           EXIT.
      *================================================================*
      * SHORT POSITION - HOUSE SHORT (HS).  SHORT EQUITIES: THE        *
      * GREATER OF PCT X MV AND PER SHARE MINIMUM X SHARES (CHG08201). *
      *================================================================*
       3500-SHORT-REQUIREMENT.
           MOVE 'N'  TO WS-PW-PER-SHARE-SW.
           MOVE 'Y'  TO WS-PW-MARGINABLE-SW.
           MOVE ZERO TO WS-PW-SHARE-REQ.
           MOVE 'HS'            TO WS-LK-RULE-TYPE.
           MOVE WS-PW-SEC-TYPE  TO WS-LK-SEC-TYPE.
           MOVE WS-PW-QUALIFIER TO WS-LK-QUALIFIER.
           MOVE 3 TO WS-LOOKUP-LEVELS.
           PERFORM 7000-FIND-RULE THRU 7000-EXIT.
           IF RULE-FOUND
               MOVE WS-FR-KEY TO WS-PW-RULE-KEY
               MOVE WS-FR-PCT TO WS-PW-PCT
           ELSE
               PERFORM 3450-NO-RULE THRU 3450-EXIT
               MOVE ZERO TO WS-FR-PER-SHARE
           END-IF.
           COMPUTE WS-PW-PCT-REQ ROUNDED =
               WS-PW-ABS-MV * WS-PW-PCT / 100.
           MOVE WS-PW-PCT-REQ TO WS-PW-REQ.
           IF WS-PW-SEC-TYPE = 'EQ' OR 'PF' OR 'AD'
               IF WS-FR-PER-SHARE > ZERO
                   COMPUTE WS-PW-SHARE-REQ ROUNDED =
                       WS-PW-ABS-QTY * WS-FR-PER-SHARE
                   IF WS-PW-SHARE-REQ > WS-PW-PCT-REQ
                       MOVE WS-PW-SHARE-REQ TO WS-PW-REQ
                       MOVE 'Y' TO WS-PW-PER-SHARE-SW
                       ADD 1 TO WS-PER-SHARE-CNT
                   END-IF
               END-IF
           END-IF.
           ADD WS-PW-ABS-MV TO WS-AW-SHORT-MV.
       3500-EXIT.
           EXIT.
      *================================================================*
      * ISSUER CONCENTRATION - LONG MV BY ISSUER WITHIN THE ACCOUNT    *
      *================================================================*
       3700-ADD-ISSUER.
           SET IT-IDX TO 1.
           SEARCH WS-IT-ENTRY
               AT END
                   PERFORM 3710-NEW-ISSUER THRU 3710-EXIT
               WHEN IT-IDX > WS-IT-COUNT
                   PERFORM 3710-NEW-ISSUER THRU 3710-EXIT
               WHEN WS-IT-ISSUER (IT-IDX) = WS-PW-ISSUER
                   ADD WS-PW-MV-USD TO WS-IT-LONG-MV (IT-IDX)
           END-SEARCH.
       3700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3710-NEW-ISSUER.
      *----------------------------------------------------------------*
           IF WS-IT-COUNT NOT < WS-IT-MAX
               ADD 1 TO WS-ISSUER-OVFL-CNT
               DISPLAY 'MGB100 ISSUER TABLE FULL - ACCOUNT '
                       POS-ACCT-NO ' ISSUER ' WS-PW-ISSUER
                       ' NOT IN CONCENTRATION TEST'
               GO TO 3710-EXIT
           END-IF.
           ADD 1 TO WS-IT-COUNT.
           SET IT-IDX TO WS-IT-COUNT.
           MOVE WS-PW-ISSUER TO WS-IT-ISSUER (IT-IDX).
           MOVE WS-PW-MV-USD TO WS-IT-LONG-MV (IT-IDX).
       3710-EXIT.
           EXIT.
      *================================================================*
      * POSITION REQUIREMENT DETAIL (MG.POSREQ)                        *
      *================================================================*
       3800-WRITE-POSREQ.
           MOVE SPACES             TO MPQ-POSREQ-REC.
           MOVE DC-BUS-DATE        TO MPQ-BUS-DATE.
           MOVE POS-ACCT-NO        TO MPQ-ACCT-NO.
           MOVE POS-CUSIP          TO MPQ-CUSIP.
           MOVE WS-PW-SEC-TYPE     TO MPQ-SEC-TYPE.
           MOVE WS-PW-ISSUER       TO MPQ-ISSUER-ID.
           MOVE WS-PW-QTY          TO MPQ-QTY.
           MOVE WS-PW-PRICE        TO MPQ-PRICE.
           MOVE WS-PW-MV-USD       TO MPQ-MKT-VALUE-USD.
           MOVE WS-PW-RULE-KEY     TO MPQ-RULE-KEY.
           MOVE WS-PW-PCT          TO MPQ-REQ-PCT.
           MOVE WS-PW-REQ          TO MPQ-REQ-AMOUNT.
           MOVE WS-PW-PER-SHARE-SW TO MPQ-PER-SHARE-FLAG.
           MOVE WS-PW-MARGINABLE-SW TO MPQ-MARGINABLE-FLAG.
           WRITE MPQ-POSREQ-REC.
           IF WS-POSREQ-STATUS NOT = '00'
               MOVE 'POSREQ' TO AB-DDNAME
               MOVE WS-POSREQ-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '3800-WRITE-POSREQ' TO AB-PARAGRAPH
               MOVE POS-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-POSREQ-OUT-CNT.
           ADD WS-PW-MV-USD  TO WS-TOT-POSREQ-MV.
           ADD WS-PW-REQ     TO WS-TOT-POSREQ-REQ.
           ADD WS-PW-ABS-QTY TO WS-TOT-POSREQ-QTY.
       3800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3900-TRACE-POSITION.
      *----------------------------------------------------------------*
           MOVE WS-PW-QTY TO WS-DISP-QTY.
           DISPLAY 'MGB100 TRACE POSN ' POS-CUSIP ' ' POS-LOCATION
                   ' ' WS-PW-SEC-TYPE ' ' WS-PW-SIDE
                   ' QTY ' WS-DISP-QTY.
           MOVE WS-PW-MV-USD TO WS-DISP-AMT.
           DISPLAY 'MGB100 TRACE      MV USD ' WS-DISP-AMT
                   ' STALE ' WS-PW-STALE-SW
                   ' ISSUER ' WS-PW-ISSUER
                   ' QUAL ' WS-PW-QUALIFIER.
           MOVE WS-PW-PCT TO WS-DISP-PCT.
           MOVE WS-PW-REQ TO WS-DISP-AMT.
           DISPLAY 'MGB100 TRACE      RULE ' WS-PW-RULE-KEY
                   ' PCT ' WS-DISP-PCT ' REQ ' WS-DISP-AMT
                   ' PER-SHR ' WS-PW-PER-SHARE-SW
                   ' MARGINABLE ' WS-PW-MARGINABLE-SW.
       3900-EXIT.
           EXIT.
      *================================================================*
      * CASH - TD BALANCE OF EVERY CURRENCY, IN USD                    *
      *================================================================*
       4000-ACCOUNT-CASH.
           MOVE 'N'         TO WS-CASH-EOF-SW.
           MOVE ACCT-NO     TO WS-CSK-ACCT.
           MOVE LOW-VALUES  TO WS-CSK-REST.
           MOVE WS-CSH-START-KEY TO CSH-KEY.
           START CASHBAL-FILE KEY IS NOT LESS THAN CSH-KEY.
           EVALUATE TRUE
               WHEN CASHBAL-OK
                   CONTINUE
               WHEN CASHBAL-NOTFND
                   GO TO 4000-EXIT
               WHEN OTHER
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '4000-ACCOUNT-CASH' TO AB-PARAGRAPH
                   MOVE ACCT-NO TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 8200-READ-CASH THRU 8200-EXIT.
           PERFORM 4100-CASH-BALANCE THRU 4100-EXIT
               UNTIL END-OF-ACCT-CASH.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-CASH-BALANCE.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CASH-READ-CNT.
           ADD 1 TO WS-AW-CASH-CNT.
           IF CSH-TD-BALANCE NUMERIC
               MOVE CSH-TD-BALANCE TO WS-CW-BAL-LOCAL
           ELSE
               MOVE ZERO TO WS-CW-BAL-LOCAL
               DISPLAY 'MGB100 CASH BALANCE NOT NUMERIC ' CSH-KEY
           END-IF.
           IF WS-CW-BAL-LOCAL = ZERO
               GO TO 4100-NEXT
           END-IF.
           IF CSH-CCY = 'USD' OR CSH-CCY = SPACES
               MOVE WS-CW-BAL-LOCAL TO WS-CW-BAL-USD
           ELSE
               PERFORM 4200-CONVERT-CASH THRU 4200-EXIT
           END-IF.
           ADD WS-CW-BAL-USD TO WS-AW-CASH-USD.
           IF TRACE-THIS-ACCOUNT
               MOVE WS-CW-BAL-USD TO WS-DISP-AMT
               DISPLAY 'MGB100 TRACE CASH ' CSH-CCY ' USD '
                       WS-DISP-AMT
           END-IF.
       4100-NEXT.
           PERFORM 8200-READ-CASH THRU 8200-EXIT.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4200-CONVERT-CASH.
      *----------------------------------------------------------------*
           MOVE CSH-CCY          TO FX-FROM-CCY.
           MOVE 'USD'            TO FX-TO-CCY.
           MOVE DC-BUS-DATE      TO FX-RATE-DATE.
           MOVE WS-CW-BAL-LOCAL  TO FX-AMOUNT-IN.
           CALL 'CMU040' USING FX-CONVERT-PARMS.
           ADD 1 TO WS-CMU040-CALLS.
           EVALUATE TRUE
               WHEN FX-OK
                   MOVE FX-AMOUNT-OUT TO WS-CW-BAL-USD
               WHEN FX-STALE-RATE
                   ADD 1 TO WS-STALE-FX-CNT
                   MOVE FX-AMOUNT-OUT TO WS-CW-BAL-USD
                   MOVE 'Y' TO WS-AW-STALE-FLAG
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN FX-RATE-NOT-FOUND
                   ADD 1 TO WS-NO-FX-CNT
                   MOVE ZERO TO WS-CW-BAL-USD
                   MOVE 'Y' TO WS-AW-STALE-FLAG
                   DISPLAY 'MGB100 NO FX RATE ' CSH-CCY
                           ' - CASH IGNORED ' CSH-KEY
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 1010 TO AB-ABEND-CODE
                   MOVE '4200-CONVERT-CASH' TO AB-PARAGRAPH
                   MOVE CSH-KEY TO AB-KEY
                   MOVE FX-MESSAGE TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       4200-EXIT.
           EXIT.
      *================================================================*
      * ACCOUNT REQUIREMENT AND STATUS                                 *
      *================================================================*
       5000-ACCOUNT-REQUIREMENT.
           IF WS-AW-CASH-USD < ZERO
               COMPUTE WS-AW-DEBIT = WS-AW-CASH-USD * -1
               MOVE ZERO TO WS-AW-CREDIT
           ELSE
               MOVE ZERO TO WS-AW-DEBIT
               MOVE WS-AW-CASH-USD TO WS-AW-CREDIT
           END-IF.
           COMPUTE WS-AW-EQUITY =
               WS-AW-LONG-MV - WS-AW-SHORT-MV + WS-AW-CASH-USD.
           PERFORM 5100-REG-T THRU 5100-EXIT.
           PERFORM 5200-CONCENTRATION THRU 5200-EXIT.
           COMPUTE WS-AW-TOTAL-HOUSE =
               WS-AW-HOUSE-REQ + WS-AW-CONC-ADDON.
           COMPUTE WS-AW-EXCESS = WS-AW-EQUITY - WS-AW-TOTAL-HOUSE.
           COMPUTE WS-AW-SMA = WS-AW-EQUITY - WS-AW-REGT-REQ.
           IF WS-AW-SMA < ZERO
               MOVE ZERO TO WS-AW-SMA
           END-IF.
           PERFORM 5300-SET-STATUS THRU 5300-EXIT.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * REG T: RT PCT OF MARGINABLE LONGS PLUS RT PCT OF SHORTS        *
      *----------------------------------------------------------------*
       5100-REG-T.
           COMPUTE WS-AW-REGT-LONG ROUNDED =
               WS-AW-MARG-LONG-MV * WS-RT-PCT / 100.
           COMPUTE WS-AW-REGT-SHORT ROUNDED =
               WS-AW-SHORT-MV * WS-RT-PCT / 100.
           COMPUTE WS-AW-REGT-REQ =
               WS-AW-REGT-LONG + WS-AW-REGT-SHORT.
       5100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CONCENTRATION (CHG21340): LARGEST ISSUER AS PCT OF LONG MV     *
      *----------------------------------------------------------------*
       5200-CONCENTRATION.
           MOVE ZERO TO WS-AW-CONC-ADDON WS-AW-LARGEST-MV
                        WS-AW-LARGEST-PCT.
           MOVE SPACES TO WS-AW-LARGEST-ISSUER.
           IF WS-IT-COUNT = ZERO OR WS-AW-LONG-MV NOT > ZERO
               GO TO 5200-EXIT
           END-IF.
           PERFORM VARYING IT-IDX FROM 1 BY 1
                   UNTIL IT-IDX > WS-IT-COUNT
               IF WS-IT-LONG-MV (IT-IDX) > WS-AW-LARGEST-MV
                   MOVE WS-IT-LONG-MV (IT-IDX) TO WS-AW-LARGEST-MV
                   MOVE WS-IT-ISSUER (IT-IDX)  TO WS-AW-LARGEST-ISSUER
               END-IF
           END-PERFORM.
           COMPUTE WS-AW-LARGEST-PCT ROUNDED =
               WS-AW-LARGEST-MV / WS-AW-LONG-MV * 100
               ON SIZE ERROR
                   MOVE 999.9999 TO WS-AW-LARGEST-PCT
           END-COMPUTE.
           IF WS-AW-LARGEST-PCT > WS-CN-THRESHOLD
               COMPUTE WS-AW-CONC-ADDON =
                   WS-AW-LARGEST-MV * WS-CN-PCT / 100
               ADD 1 TO WS-CONC-CNT
               IF TRACE-THIS-ACCOUNT
                   MOVE WS-AW-CONC-ADDON TO WS-DISP-AMT
                   DISPLAY 'MGB100 TRACE CONC ISSUER '
                           WS-AW-LARGEST-ISSUER ' ADD-ON ' WS-DISP-AMT
               END-IF
           END-IF.
       5200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STATUS - HOUSE FIRST, THEN REG T, THEN MINIMUM EQUITY          *
      *----------------------------------------------------------------*
       5300-SET-STATUS.
           EVALUATE TRUE
               WHEN WS-AW-EQUITY < WS-AW-TOTAL-HOUSE
                   MOVE 'H' TO WS-AW-STATUS
                   ADD 1 TO WS-STAT-H-CNT
               WHEN WS-AW-EQUITY < WS-AW-REGT-REQ
                   MOVE 'T' TO WS-AW-STATUS
                   ADD 1 TO WS-STAT-T-CNT
               WHEN WS-AW-DEBIT > ZERO
                AND WS-AW-EQUITY < WS-MN-AMOUNT
                   MOVE 'M' TO WS-AW-STATUS
                   ADD 1 TO WS-STAT-M-CNT
               WHEN OTHER
                   MOVE 'G' TO WS-AW-STATUS
                   ADD 1 TO WS-STAT-G-CNT
           END-EVALUATE.
           IF WS-AW-STATUS NOT = 'G'
               ADD 1 TO WS-DEFICIT-CNT
               IF WS-AW-EXCESS < ZERO
                   SUBTRACT WS-AW-EXCESS FROM WS-TOT-DEFICIT
               END-IF
           END-IF.
           IF WS-AW-STALE-FLAG = 'Y'
               ADD 1 TO WS-STALE-ACCT-CNT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           IF TRACE-THIS-ACCOUNT
               MOVE WS-AW-EQUITY TO WS-DISP-AMT
               DISPLAY 'MGB100 TRACE EQUITY      ' WS-DISP-AMT
               MOVE WS-AW-HOUSE-REQ TO WS-DISP-AMT
               DISPLAY 'MGB100 TRACE HOUSE REQ   ' WS-DISP-AMT
               MOVE WS-AW-REGT-REQ TO WS-DISP-AMT
               DISPLAY 'MGB100 TRACE REG T REQ   ' WS-DISP-AMT
               MOVE WS-AW-EXCESS TO WS-DISP-AMT
               DISPLAY 'MGB100 TRACE EXCESS      ' WS-DISP-AMT
                       ' STATUS ' WS-AW-STATUS
           END-IF.
       5300-EXIT.
           EXIT.
      *================================================================*
      * REQUIREMENT RECORD (MG.REQ)                                    *
      *================================================================*
       6000-WRITE-REQUIREMENT.
           MOVE SPACES               TO MRQ-REQUIREMENT-REC.
           MOVE DC-BUS-DATE          TO MRQ-BUS-DATE.
           MOVE ACCT-NO              TO MRQ-ACCT-NO.
           MOVE ACCT-TYPE            TO MRQ-ACCT-TYPE.
           MOVE ACCT-BRANCH          TO MRQ-BRANCH.
           MOVE ACCT-REP             TO MRQ-REP.
           MOVE WS-AW-LONG-MV        TO MRQ-LONG-MV.
           MOVE WS-AW-SHORT-MV       TO MRQ-SHORT-MV.
           MOVE WS-AW-CASH-USD       TO MRQ-CASH-BALANCE.
           MOVE WS-AW-DEBIT          TO MRQ-DEBIT-BALANCE.
           MOVE WS-AW-CREDIT         TO MRQ-CREDIT-BALANCE.
           MOVE WS-AW-EQUITY         TO MRQ-EQUITY.
           MOVE WS-AW-REGT-REQ       TO MRQ-REGT-REQ.
           MOVE WS-AW-HOUSE-REQ      TO MRQ-HOUSE-REQ.
           MOVE WS-AW-CONC-ADDON     TO MRQ-CONC-ADDON.
           MOVE WS-AW-EXCESS         TO MRQ-EXCESS.
           MOVE WS-AW-SMA            TO MRQ-SMA.
           MOVE WS-AW-NONMARG-MV     TO MRQ-NONMARG-MV.
           MOVE WS-AW-LARGEST-ISSUER TO MRQ-LARGEST-ISSUER.
           MOVE WS-AW-LARGEST-PCT    TO MRQ-LARGEST-PCT.
           MOVE WS-AW-POSN-CNT       TO MRQ-POSITION-COUNT.
           MOVE WS-AW-STATUS         TO MRQ-STATUS.
           MOVE WS-AW-STALE-FLAG     TO MRQ-PRICE-STALE-FLAG.
           WRITE REQOUT-REC FROM MRQ-REQUIREMENT-REC.
           IF WS-REQOUT-STATUS NOT = '00'
               MOVE 'REQOUT' TO AB-DDNAME
               MOVE WS-REQOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '6000-WRITE-REQUIREMENT' TO AB-PARAGRAPH
               MOVE ACCT-NO TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-REQ-OUT-CNT.
           ADD WS-AW-LONG-MV     TO WS-TOT-LONG-MV.
           ADD WS-AW-SHORT-MV    TO WS-TOT-SHORT-MV.
           ADD WS-AW-EQUITY      TO WS-TOT-EQUITY.
           ADD WS-AW-DEBIT       TO WS-TOT-DEBIT.
           ADD WS-AW-CREDIT      TO WS-TOT-CREDIT.
           ADD WS-AW-HOUSE-REQ   TO WS-TOT-HOUSE-REQ.
           ADD WS-AW-REGT-REQ    TO WS-TOT-REGT-REQ.
           ADD WS-AW-CONC-ADDON  TO WS-TOT-CONC-ADDON.
           IF ACCT-RESTRICTED
               ADD 1 TO WS-RESTRICTED-CNT
           END-IF.
           IF WS-AW-POSN-CNT = ZERO AND WS-AW-CASH-USD = ZERO
               ADD 1 TO WS-NO-ACTIVITY-CNT
           END-IF.
           PERFORM 6100-EARLY-WARNING THRU 6100-EXIT.
           PERFORM 6200-BRANCH-TOTALS THRU 6200-EXIT.
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EARLY WARNING - GOOD ORDER BUT THE EXCESS IS UNDER 5 PCT OF    *
      * THE EQUITY.  LISTED FOR THE MARGIN DEPT (FIRST 25 ONLY).       *
      *----------------------------------------------------------------*
       6100-EARLY-WARNING.
           MOVE 'N' TO WS-PW-STALE-SW.
           IF WS-AW-STATUS NOT = 'G' OR WS-AW-EQUITY NOT > ZERO
               GO TO 6100-EXIT
           END-IF.
           IF WS-AW-DEBIT = ZERO
               GO TO 6100-EXIT
           END-IF.
           COMPUTE WS-WARN-LIMIT ROUNDED =
               WS-AW-EQUITY * WS-WARN-PCT / 100.
           IF WS-AW-EXCESS NOT < WS-WARN-LIMIT
               GO TO 6100-EXIT
           END-IF.
           ADD 1 TO WS-WARN-CNT.
           MOVE 'Y' TO WS-PW-STALE-SW.
           IF WS-WARN-SHOWN < WS-WARN-SHOW-MAX
               ADD 1 TO WS-WARN-SHOWN
               MOVE WS-AW-EXCESS TO WS-DISP-AMT
               DISPLAY 'MGB100 EARLY WARNING ' ACCT-NO ' ' ACCT-BRANCH
                       ' ' ACCT-REP ' EXCESS ' WS-DISP-AMT
           END-IF.
       6100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6200-BRANCH-TOTALS.
      *----------------------------------------------------------------*
           SET BT-IDX TO 1.
           SEARCH WS-BT-ENTRY
               AT END
                   ADD 1 TO WS-BT-OVFL-CNT
                   GO TO 6200-EXIT
               WHEN BT-IDX > WS-BT-COUNT
                   IF WS-BT-COUNT NOT < WS-BT-MAX
                       ADD 1 TO WS-BT-OVFL-CNT
                       GO TO 6200-EXIT
                   END-IF
                   ADD 1 TO WS-BT-COUNT
                   MOVE ACCT-BRANCH TO WS-BT-BRANCH (BT-IDX)
                   MOVE ZERO TO WS-BT-ACCTS (BT-IDX)
                                WS-BT-CALLS (BT-IDX)
                                WS-BT-WARN (BT-IDX)
                                WS-BT-EQUITY (BT-IDX)
                                WS-BT-DEBIT (BT-IDX)
                                WS-BT-DEFICIT (BT-IDX)
               WHEN WS-BT-BRANCH (BT-IDX) = ACCT-BRANCH
                   CONTINUE
           END-SEARCH.
           ADD 1            TO WS-BT-ACCTS (BT-IDX).
           ADD WS-AW-EQUITY TO WS-BT-EQUITY (BT-IDX).
           ADD WS-AW-DEBIT  TO WS-BT-DEBIT (BT-IDX).
           IF WS-AW-STATUS NOT = 'G'
               ADD 1 TO WS-BT-CALLS (BT-IDX)
               IF WS-AW-EXCESS < ZERO
                   SUBTRACT WS-AW-EXCESS FROM WS-BT-DEFICIT (BT-IDX)
               END-IF
           END-IF.
           IF WS-PW-STALE-SW = 'Y'
               ADD 1 TO WS-BT-WARN (BT-IDX)
           END-IF.
       6200-EXIT.
           EXIT.
      *================================================================*
      * RULE LOOKUP - MOST SPECIFIC FIRST.                             *
      *   LEVEL 1  TYPE + SEC TYPE + QUALIFIER                         *
      *   LEVEL 2  TYPE + SEC TYPE + '****'                            *
      *   LEVEL 3  TYPE + '**'     + '****'                            *
      * WS-LOOKUP-LEVELS LIMITS HOW FAR WE FALL BACK.                  *
      *================================================================*
       7000-FIND-RULE.
           MOVE WS-LOOKUP-KEY TO WS-LOOKUP-SAVE.
           MOVE 1 TO WS-LOOKUP-LEVEL.
           PERFORM 7100-SEARCH-RULE THRU 7100-EXIT.
           IF RULE-FOUND
               GO TO 7000-EXIT
           END-IF.
           IF WS-LOOKUP-LEVELS < 2
               GO TO 7000-EXIT
           END-IF.
           IF WS-LK-QUALIFIER NOT = WS-ANY-QUALIFIER
               MOVE 2 TO WS-LOOKUP-LEVEL
               MOVE WS-ANY-QUALIFIER TO WS-LK-QUALIFIER
               PERFORM 7100-SEARCH-RULE THRU 7100-EXIT
               IF RULE-FOUND
                   GO TO 7000-EXIT
               END-IF
           END-IF.
           IF WS-LOOKUP-LEVELS < 3
               GO TO 7000-EXIT
           END-IF.
           MOVE 3 TO WS-LOOKUP-LEVEL.
           MOVE WS-ANY-SEC-TYPE  TO WS-LK-SEC-TYPE.
           MOVE WS-ANY-QUALIFIER TO WS-LK-QUALIFIER.
           PERFORM 7100-SEARCH-RULE THRU 7100-EXIT.
       7000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7100-SEARCH-RULE.
      *----------------------------------------------------------------*
           MOVE 'N' TO WS-RULE-FOUND-SW.
           IF WS-RULE-COUNT = ZERO
               GO TO 7100-EXIT
           END-IF.
           SET RL-IDX TO 1.
           SEARCH WS-RULE-ENTRY
               AT END
                   CONTINUE
               WHEN RL-IDX > WS-RULE-COUNT
                   CONTINUE
               WHEN WS-RL-KEY (RL-IDX) = WS-LOOKUP-KEY
                   MOVE 'Y'                    TO WS-RULE-FOUND-SW
                   MOVE WS-RL-KEY (RL-IDX)       TO WS-FR-KEY
                   MOVE WS-RL-PCT (RL-IDX)       TO WS-FR-PCT
                   MOVE WS-RL-PER-SHARE (RL-IDX) TO WS-FR-PER-SHARE
                   MOVE WS-RL-AMOUNT (RL-IDX)    TO WS-FR-AMOUNT
                   MOVE WS-RL-THRESHOLD (RL-IDX) TO WS-FR-THRESHOLD
                   ADD 1 TO WS-RL-USED-CNT (RL-IDX)
           END-SEARCH.
       7100-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-ACCOUNT.
           READ ACCTMAST-FILE NEXT RECORD.
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
       8100-READ-POSITION.
      *----------------------------------------------------------------*
           READ POSMAST-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   IF POS-ACCT-NO NOT = WS-AW-ACCT-NO
                       MOVE 'Y' TO WS-POSN-EOF-SW
                   END-IF
               WHEN POSMAST-EOF
                   MOVE 'Y' TO WS-POSN-EOF-SW
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-POSITION' TO AB-PARAGRAPH
                   MOVE WS-AW-ACCT-NO TO AB-KEY
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-READ-CASH.
      *----------------------------------------------------------------*
           READ CASHBAL-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN CASHBAL-OK
                   IF CSH-ACCT-NO NOT = WS-AW-ACCT-NO
                       MOVE 'Y' TO WS-CASH-EOF-SW
                   END-IF
               WHEN CASHBAL-EOF
                   MOVE 'Y' TO WS-CASH-EOF-SW
               WHEN OTHER
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8200-READ-CASH' TO AB-PARAGRAPH
                   MOVE WS-AW-ACCT-NO TO AB-KEY
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'MGB100'       TO CT-STAGE.
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
           CLOSE ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE CASHBAL-FILE.
           IF WS-CASHBAL-STATUS NOT = '00'
               MOVE 'CASHBAL' TO AB-DDNAME
               MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE REQOUT-FILE.
           IF WS-REQOUT-STATUS NOT = '00'
               MOVE 'REQOUT' TO AB-DDNAME
               MOVE WS-REQOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE POSREQ-FILE.
           IF WS-POSREQ-STATUS NOT = '00'
               MOVE 'POSREQ' TO AB-DDNAME
               MOVE WS-POSREQ-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 9050-CONTROL-PROOF THRU 9050-EXIT.
           PERFORM 9100-POST-TOTALS THRU 9100-EXIT.
           PERFORM 9200-DISPLAY-STATS THRU 9200-EXIT.
           PERFORM 9300-RULE-USAGE THRU 9300-EXIT.
           PERFORM 9400-BRANCH-SUMMARY THRU 9400-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MARGIN REQUIREMENT ENDED' TO AU-MESSAGE.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
               WHEN WS-RETURN-CODE > ZERO
                   MOVE 'W' TO AU-SEVERITY
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
           END-EVALUATE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * THE POSITION DETAIL MUST ADD UP TO THE ACCOUNT RECORDS:        *
      *   SUM DETAIL MV  = LONG MV - SHORT MV                          *
      *   SUM DETAIL REQ = HOUSE REQUIREMENT (EXCLUDING CONC ADD-ON)   *
      * ZERO-VALUE POSITIONS ARE NOT ON THE DETAIL BUT ADD NOTHING.    *
      *----------------------------------------------------------------*
       9050-CONTROL-PROOF.
           COMPUTE WS-PROOF-NET-MV = WS-TOT-LONG-MV - WS-TOT-SHORT-MV.
           COMPUTE WS-PROOF-DIFF = WS-PROOF-NET-MV - WS-TOT-POSREQ-MV.
           COMPUTE WS-PROOF-REQ-DIFF =
               WS-TOT-HOUSE-REQ - WS-TOT-POSREQ-REQ.
           IF WS-PROOF-DIFF NOT = ZERO OR WS-PROOF-REQ-DIFF NOT = ZERO
               MOVE WS-PROOF-DIFF TO WS-DISP-AMT
               DISPLAY 'MGB100 *** CONTROL PROOF FAILED - MV DIFF '
                       WS-DISP-AMT
               MOVE WS-PROOF-REQ-DIFF TO WS-DISP-AMT
               DISPLAY 'MGB100 ***                        REQ DIFF '
                       WS-DISP-AMT
               MOVE 8 TO WS-RETURN-CODE
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'CTLPROOF'     TO AU-EVENT
               MOVE 'E'            TO AU-SEVERITY
               MOVE 'MG.POSREQ'    TO AU-KEY
               MOVE 'POSITION DETAIL DOES NOT AGREE WITH MG.REQ'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           ELSE
               DISPLAY 'MGB100 CONTROL PROOF - DETAIL AGREES WITH '
                       'REQUIREMENTS'
           END-IF.
       9050-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CONTROL TOTALS: ACCTS-IN, REQ-OUT, DEFICIT-ACCTS, POSREQ-OUT   *
      *----------------------------------------------------------------*
       9100-POST-TOTALS.
           MOVE 'ACCTS-IN'       TO CT-COUNTER-NAME.
           MOVE WS-ACCT-IN-CNT   TO CT-COUNT.
           MOVE WS-TOT-EQUITY    TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'REQ-OUT'        TO CT-COUNTER-NAME.
           MOVE WS-REQ-OUT-CNT   TO CT-COUNT.
           MOVE WS-TOT-HOUSE-REQ TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'DEFICIT-ACCTS'  TO CT-COUNTER-NAME.
           MOVE WS-DEFICIT-CNT   TO CT-COUNT.
           MOVE WS-TOT-DEFICIT   TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'POSREQ-OUT'     TO CT-COUNTER-NAME.
           MOVE WS-POSREQ-OUT-CNT TO CT-COUNT.
           MOVE WS-TOT-POSREQ-MV TO CT-AMOUNT.
           MOVE WS-TOT-POSREQ-QTY TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
       9100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9200-DISPLAY-STATS.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'.
           DISPLAY '* MGB100 - DAILY MARGIN REQUIREMENT            *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' RULE EFFECTIVE DATE      : ' WS-RULE-EFF-DATE.
           MOVE WS-ACCT-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS READ            : ' WS-DISP-CNT.
           MOVE WS-FIRM-SKIP-CNT TO WS-DISP-CNT.
           DISPLAY '   HOUSE / STREET SKIPPED : ' WS-DISP-CNT.
           MOVE WS-CASH-ACCT-CNT TO WS-DISP-CNT.
           DISPLAY '   CASH ACCOUNTS SKIPPED  : ' WS-DISP-CNT.
           MOVE WS-STATUS-SKIP-CNT TO WS-DISP-CNT.
           DISPLAY '   CLOSED / DECEASED      : ' WS-DISP-CNT.
           MOVE WS-ACCT-IN-CNT TO WS-DISP-CNT.
           DISPLAY ' MARGIN ACCOUNTS          : ' WS-DISP-CNT.
           MOVE WS-REQ-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' REQUIREMENT RECORDS OUT  : ' WS-DISP-CNT.
           MOVE WS-STAT-G-CNT TO WS-DISP-CNT.
           DISPLAY '   G  GOOD ORDER          : ' WS-DISP-CNT.
           MOVE WS-STAT-H-CNT TO WS-DISP-CNT.
           DISPLAY '   H  HOUSE CALL          : ' WS-DISP-CNT.
           MOVE WS-STAT-T-CNT TO WS-DISP-CNT.
           DISPLAY '   T  REG T CALL          : ' WS-DISP-CNT.
           MOVE WS-STAT-M-CNT TO WS-DISP-CNT.
           DISPLAY '   M  MINIMUM EQUITY      : ' WS-DISP-CNT.
           MOVE WS-CONC-CNT TO WS-DISP-CNT.
           DISPLAY ' CONCENTRATION ADD-ONS    : ' WS-DISP-CNT.
           MOVE WS-STALE-ACCT-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS FLAGGED STALE   : ' WS-DISP-CNT.
           MOVE WS-POSN-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS READ           : ' WS-DISP-CNT.
           MOVE WS-POSN-ZERO-CNT TO WS-DISP-CNT.
           DISPLAY '   ZERO QUANTITY          : ' WS-DISP-CNT.
           MOVE WS-POSN-LONG-CNT TO WS-DISP-CNT.
           DISPLAY '   LONG                   : ' WS-DISP-CNT.
           MOVE WS-POSN-SHORT-CNT TO WS-DISP-CNT.
           DISPLAY '   SHORT                  : ' WS-DISP-CNT.
           MOVE WS-POSN-NM-CNT TO WS-DISP-CNT.
           DISPLAY '   NON-MARGINABLE         : ' WS-DISP-CNT.
           MOVE WS-PER-SHARE-CNT TO WS-DISP-CNT.
           DISPLAY '   PER SHARE MINIMUM USED : ' WS-DISP-CNT.
           MOVE WS-POSN-UNVALUED-CNT TO WS-DISP-CNT.
           DISPLAY '   NOT VALUED (NO MV)     : ' WS-DISP-CNT.
           MOVE WS-POSN-STALE-CNT TO WS-DISP-CNT.
           DISPLAY '   STALE PRICE            : ' WS-DISP-CNT.
           MOVE WS-POSREQ-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITION DETAIL OUT      : ' WS-DISP-CNT.
           MOVE WS-NO-SEC-CNT TO WS-DISP-CNT.
           DISPLAY ' NOT ON SECURITY MASTER   : ' WS-DISP-CNT.
           MOVE WS-NO-RULE-CNT TO WS-DISP-CNT.
           DISPLAY ' NO RULE (100 PCT)        : ' WS-DISP-CNT.
           MOVE WS-ISSUER-OVFL-CNT TO WS-DISP-CNT.
           DISPLAY ' ISSUER TABLE OVERFLOW    : ' WS-DISP-CNT.
           MOVE WS-CASH-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' CASH BALANCES READ       : ' WS-DISP-CNT.
           MOVE WS-NO-FX-CNT TO WS-DISP-CNT.
           DISPLAY ' NO FX RATE               : ' WS-DISP-CNT.
           MOVE WS-STALE-FX-CNT TO WS-DISP-CNT.
           DISPLAY ' STALE FX RATE            : ' WS-DISP-CNT.
           MOVE WS-CMD010-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMD010 CALLS             : ' WS-DISP-CNT.
           MOVE WS-CMU040-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMU040 CALLS             : ' WS-DISP-CNT.
           MOVE WS-TOT-LONG-MV TO WS-DISP-AMT.
           DISPLAY ' LONG MARKET VALUE        : ' WS-DISP-AMT.
           MOVE WS-TOT-SHORT-MV TO WS-DISP-AMT.
           DISPLAY ' SHORT MARKET VALUE       : ' WS-DISP-AMT.
           MOVE WS-TOT-DEBIT TO WS-DISP-AMT.
           DISPLAY ' DEBIT BALANCES           : ' WS-DISP-AMT.
           MOVE WS-TOT-CREDIT TO WS-DISP-AMT.
           DISPLAY ' CREDIT BALANCES          : ' WS-DISP-AMT.
           MOVE WS-TOT-EQUITY TO WS-DISP-AMT.
           DISPLAY ' EQUITY                   : ' WS-DISP-AMT.
           MOVE WS-TOT-HOUSE-REQ TO WS-DISP-AMT.
           DISPLAY ' HOUSE REQUIREMENT        : ' WS-DISP-AMT.
           MOVE WS-TOT-CONC-ADDON TO WS-DISP-AMT.
           DISPLAY ' CONCENTRATION ADD-ON     : ' WS-DISP-AMT.
           MOVE WS-TOT-REGT-REQ TO WS-DISP-AMT.
           DISPLAY ' REG T REQUIREMENT        : ' WS-DISP-AMT.
           MOVE WS-TOT-DEFICIT TO WS-DISP-AMT.
           DISPLAY ' HOUSE DEFICIT            : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
       9200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * RULE USAGE - UNUSED ROWS ARE CANDIDATES FOR CLEAN-UP (CHG33702)*
      *----------------------------------------------------------------*
       9300-RULE-USAGE.
           DISPLAY ' RULE USAGE   KEY      PCT        PER SHR   USED'.
           PERFORM VARYING RL-IDX FROM 1 BY 1
                   UNTIL RL-IDX > WS-RULE-COUNT
               MOVE WS-RL-PCT (RL-IDX)      TO WS-DISP-PCT
               MOVE WS-RL-USED-CNT (RL-IDX) TO WS-DISP-CNT
               MOVE WS-RL-PER-SHARE (RL-IDX) TO WS-DISP-SHR
               DISPLAY '              ' WS-RL-KEY (RL-IDX) ' '
                       WS-DISP-PCT ' ' WS-DISP-SHR
                       ' ' WS-DISP-CNT
           END-PERFORM.
       9300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BRANCH MANAGER SUMMARY (TABLE IN ORDER OF FIRST APPEARANCE)    *
      *----------------------------------------------------------------*
       9400-BRANCH-SUMMARY.
           DISPLAY ' '.
           DISPLAY ' BRANCH  ACCOUNTS  IN CALL  WARNING'
                   '              EQUITY             DEFICIT'.
           PERFORM VARYING BT-IDX FROM 1 BY 1
                   UNTIL BT-IDX > WS-BT-COUNT
               MOVE WS-BT-EQUITY (BT-IDX)  TO WS-DISP-AMT
               MOVE WS-BT-ACCTS (BT-IDX)   TO WS-DISP-CNT
               MOVE WS-BT-CALLS (BT-IDX)   TO WS-DISP-SMALL-1
               MOVE WS-BT-WARN (BT-IDX)    TO WS-DISP-SMALL-2
               DISPLAY ' ' WS-BT-BRANCH (BT-IDX) '   '
                       WS-DISP-CNT ' '
                       WS-DISP-SMALL-1 '  '
                       WS-DISP-SMALL-2 ' '
                       WS-DISP-AMT WITH NO ADVANCING
               MOVE WS-BT-DEFICIT (BT-IDX) TO WS-DISP-AMT
               DISPLAY ' ' WS-DISP-AMT
           END-PERFORM.
           IF WS-BT-OVFL-CNT > ZERO
               DISPLAY ' BRANCH TABLE FULL - ACCOUNTS NOT SUMMARIZED: '
                       WS-BT-OVFL-CNT
           END-IF.
           MOVE WS-WARN-CNT TO WS-DISP-CNT.
           DISPLAY ' EARLY WARNING ACCOUNTS   : ' WS-DISP-CNT.
           MOVE WS-RESTRICTED-CNT TO WS-DISP-CNT.
           DISPLAY ' RESTRICTED ACCOUNTS      : ' WS-DISP-CNT.
           MOVE WS-NO-ACTIVITY-CNT TO WS-DISP-CNT.
           DISPLAY ' NO POSITIONS AND NO CASH : ' WS-DISP-CNT.
       9400-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'MGB100 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'MGB100 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'MGB100 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
