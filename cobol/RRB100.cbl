       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRB100.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 1995.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRB100                                            *
      * DESCRIPTION: CUSTOMER RESERVE FORMULA COMPUTATION.             *
      *              BUILDS THE CREDIT AND DEBIT ITEMS OF THE CUSTOMER *
      *              RESERVE FORMULA FROM THE BOOKS AND RECORDS AND    *
      *              COMPARES THE REQUIREMENT WITH THE AMOUNT ON       *
      *              DEPOSIT IN THE SPECIAL RESERVE BANK ACCOUNT.      *
      *                                                                *
      *              CREDITS                                           *
      *               C01 FREE CREDIT BALANCES - CLIENT CASH ACCOUNTS  *
      *                   (POSITIVE SETTLED CASH, ALL CCYS IN USD)     *
      *               C02 CREDIT BALANCES - MARGIN ACCOUNTS (MG.REQ)   *
      *               C03 DIVIDENDS / ENTITLEMENTS PAYABLE (ENTLMAST   *
      *                   STATUS CA, NET CASH, CLIENT ACCOUNTS)        *
      *               C04 FAILS TO RECEIVE (SR.FAILS BUY / BCV)        *
      *              DEBITS                                            *
      *               D01 CUSTOMER DEBIT BALANCES (MG.REQ) - SECURED   *
      *                   ONLY, I.E. ACCOUNT EQUITY > 0                *
      *               D02 FAILS TO DELIVER (SR.FAILS SEL / SSL) NOT    *
      *                   OLDER THAN 30 DAYS                           *
      *               D03 ONE PERCENT REDUCTION OF AGGREGATE DEBITS    *
      *              SUMMARY                                           *
      *               S01 TOTAL CREDITS   S02 TOTAL DEBITS             *
      *               S03 RESERVE REQUIREMENT  MAX(0, S01 - S02)       *
      *               S04 AMOUNT ON DEPOSIT (SYSIN RESERVE-BANK=)      *
      *               S05 EXCESS / (DEFICIENCY)  S04 - S03             *
      *                                                                *
      *              FIRM AND STREET ACCOUNTS ARE NOT CUSTOMERS AND    *
      *              ARE LEFT OUT OF EVERY LINE.                       *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRRD010 / STEP010  (IKJEFT01 - DB2 PLAN MSRRPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              SYSIN    - CONTROL CARDS RRP010A                  *
      *                         RESERVE-BANK=NNNNNNNNNNNNN.NN          *
      *                         TRACE=Y  (DISPLAY EVERY ITEM)          *
      *              CASHBAL  - MSEC.PROD.SR.CASHBAL.KSDS  (SRCASH)    *
      *              MGREQIN  - MSEC.PROD.MG.REQ(0)        (MGREQ)     *
      *              ENTLMAST - MSEC.PROD.CA.ENTLMAST.KSDS (CAENTL)    *
      *              FAILIN   - MSEC.PROD.SR.FAILS(0)      (SRFAIL)    *
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS (CMACCT)    *
      * OUTPUT     : RESVOUT  - MSEC.PROD.RR.RESERVE(+1)   (RRFORM)    *
      * CALLS      : CMU040, CMU050, CMU060, CMU080                    *
      * RETURN CODE: 0 CLEAN                                           *
      *              4 DEFICIENCY, NO RESERVE-BANK CARD, MARGIN FILE   *
      *                NOT FROM TODAY, FX RATE MISSING/STALE, ACCOUNT  *
      *                NOT ON MASTER                                   *
      *              8 NEGATIVE REQUIREMENT LINE (SHOULD NOT HAPPEN)   *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1995-03-13 DWB  ORIGINAL - WEEKLY FORMULA (FRIDAY)    CHG01880 *
      * 1997-06-30 DWB  DAILY COMPUTATION                     CHG03115 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-04-09 KAP  DECIMALIZATION - FAILS AT MARKET      CHG08240 *
      * 2004-09-20 KAP  1 PCT REDUCTION AS SEPARATE LINE D03  CHG12650 *
      * 2009-12-14 SPA  MULTI-CURRENCY CASH VIA CMU040        CHG19002 *
      * 2012-11-19 SPA  MARGIN BALANCES FROM MG.REQ - WAS THE CHG23911 *
      *                 MARGIN EXTRACT TAPE                            *
      * 2016-10-03 SPA  DIVIDENDS PAYABLE FROM ENTLMAST       CHG30112 *
      * 2019-08-12 MHC  RESERVE-BANK CARD 13.2, TRACE CARD    CHG34020 *
      * 2024-05-20 NVR  T+1 - FAIL DAYS NOW BUSINESS DAYS,    CHG40551 *
      *                 30 DAY LIMIT KEPT PER COMPLIANCE               *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD       ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CASHBAL-FILE   ASSIGN TO CASHBAL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS CASHBAL-KEY
                  FILE STATUS IS WS-CASHBAL-STATUS.
           SELECT MGREQIN-FILE   ASSIGN TO MGREQIN
                  FILE STATUS IS WS-MGREQIN-STATUS.
           SELECT ENTLMAST-FILE  ASSIGN TO ENTLMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS ENTLMAST-KEY
                  FILE STATUS IS WS-ENTLMAST-STATUS.
           SELECT FAILIN-FILE    ASSIGN TO FAILIN
                  FILE STATUS IS WS-FAILIN-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCTMAST-KEY
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT RESVOUT-FILE   ASSIGN TO RESVOUT
                  FILE STATUS IS WS-RESVOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  CASHBAL-FILE.
       01  CASHBAL-REC.
           05  CASHBAL-KEY             PIC X(13).
           05  FILLER                  PIC X(137).
       FD  MGREQIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  MGREQIN-REC                 PIC X(250).
       FD  ENTLMAST-FILE.
       01  ENTLMAST-REC.
           05  ENTLMAST-KEY            PIC X(26).
           05  FILLER                  PIC X(224).
       FD  FAILIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FAILIN-REC                  PIC X(150).
       FD  ACCTMAST-FILE.
       01  ACCTMAST-REC.
           05  ACCTMAST-KEY            PIC X(10).
           05  FILLER                  PIC X(290).
       FD  RESVOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RESVOUT-REC                 PIC X(100).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRB100'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-CASHBAL-STATUS       PIC X(02)  VALUE '00'.
               88  CASHBAL-OK                     VALUE '00'.
               88  CASHBAL-EOF                    VALUE '10'.
           05  WS-MGREQIN-STATUS       PIC X(02)  VALUE '00'.
               88  MGREQIN-OK                     VALUE '00'.
               88  MGREQIN-EOF                    VALUE '10'.
           05  WS-ENTLMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ENTLMAST-OK                    VALUE '00'.
               88  ENTLMAST-EOF                   VALUE '10'.
           05  WS-FAILIN-STATUS        PIC X(02)  VALUE '00'.
               88  FAILIN-OK                      VALUE '00'.
               88  FAILIN-EOF                     VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-RESVOUT-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-CASH-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-CASHBAL                 VALUE 'Y'.
           05  WS-MGREQ-EOF-SW         PIC X(01)  VALUE 'N'.
               88  END-OF-MGREQ                   VALUE 'Y'.
           05  WS-ENTL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ENTLMAST                VALUE 'Y'.
           05  WS-FAIL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-FAILS                   VALUE 'Y'.
           05  WS-BANK-CARD-SW         PIC X(01)  VALUE 'N'.
               88  BANK-CARD-READ                 VALUE 'Y'.
           05  WS-TRACE-SW             PIC X(01)  VALUE 'N'.
               88  TRACE-ON                       VALUE 'Y'.
           05  WS-CUSTOMER-SW          PIC X(01)  VALUE 'Y'.
               88  CUSTOMER-ACCOUNT               VALUE 'Y'.
               88  FIRM-OR-STREET                 VALUE 'N'.
           05  WS-ACCT-FOUND-SW        PIC X(01)  VALUE 'N'.
               88  ACCT-ON-MASTER                 VALUE 'Y'.
           05  WS-MGREQ-DATE-SW        PIC X(01)  VALUE 'N'.
               88  MGREQ-DATE-WARNED              VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * CONTROL CARD WORK                                              *
      *----------------------------------------------------------------*
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(30).
           05  WS-PARM-VALUE-R REDEFINES WS-PARM-VALUE.
               10  WS-PV-DOLLARS       PIC X(13).
               10  WS-PV-POINT         PIC X(01).
               10  WS-PV-CENTS         PIC X(02).
               10  WS-PV-REST          PIC X(14).
           05  WS-PV-AMOUNT-X.
               10  WS-PVA-DOLLARS      PIC X(13).
               10  WS-PVA-CENTS        PIC X(02).
           05  WS-PV-AMOUNT REDEFINES WS-PV-AMOUNT-X
                                       PIC 9(13)V99.
       01  WS-RESERVE-BANK-AMT         PIC S9(15)V99 COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * FORMULA LINES.  LINE NUMBERS AND DESCRIPTIONS ARE PART OF THE  *
      * FILED COMPUTATION - DO NOT RENUMBER WITHOUT COMPLIANCE SIGNOFF *
      * (SEE CHG12650).                                                *
      *----------------------------------------------------------------*
       01  WS-FORMULA-VALUES.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'C'.
               10  FILLER  PIC 9(02)  VALUE 01.
               10  FILLER  PIC X(40)  VALUE
                   'FREE CREDIT BALANCES - CASH ACCOUNTS'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'C'.
               10  FILLER  PIC 9(02)  VALUE 02.
               10  FILLER  PIC X(40)  VALUE
                   'CREDIT BALANCES - MARGIN ACCOUNTS'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'C'.
               10  FILLER  PIC 9(02)  VALUE 03.
               10  FILLER  PIC X(40)  VALUE
                   'DIVIDENDS AND ENTITLEMENTS PAYABLE'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'C'.
               10  FILLER  PIC 9(02)  VALUE 04.
               10  FILLER  PIC X(40)  VALUE
                   'SECURITIES FAILED TO RECEIVE'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'D'.
               10  FILLER  PIC 9(02)  VALUE 01.
               10  FILLER  PIC X(40)  VALUE
                   'CUSTOMER DEBIT BALANCES - SECURED'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'D'.
               10  FILLER  PIC 9(02)  VALUE 02.
               10  FILLER  PIC X(40)  VALUE
                   'FAILS TO DELIVER - 30 DAYS OR LESS'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'D'.
               10  FILLER  PIC 9(02)  VALUE 03.
               10  FILLER  PIC X(40)  VALUE
                   'LESS 1 PCT OF AGGREGATE DEBIT ITEMS'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'S'.
               10  FILLER  PIC 9(02)  VALUE 01.
               10  FILLER  PIC X(40)  VALUE
                   'TOTAL CREDIT ITEMS'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'S'.
               10  FILLER  PIC 9(02)  VALUE 02.
               10  FILLER  PIC X(40)  VALUE
                   'TOTAL DEBIT ITEMS'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'S'.
               10  FILLER  PIC 9(02)  VALUE 03.
               10  FILLER  PIC X(40)  VALUE
                   'RESERVE REQUIREMENT'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'S'.
               10  FILLER  PIC 9(02)  VALUE 04.
               10  FILLER  PIC X(40)  VALUE
                   'AMOUNT ON DEPOSIT - RESERVE BANK ACCOUNT'.
           05  FILLER.
               10  FILLER  PIC X(01)  VALUE 'S'.
               10  FILLER  PIC 9(02)  VALUE 05.
               10  FILLER  PIC X(40)  VALUE
                   'EXCESS / (DEFICIENCY)'.
       01  WS-FORMULA-TABLE REDEFINES WS-FORMULA-VALUES.
           05  WS-FL-ENTRY             OCCURS 12 TIMES.
               10  WS-FL-SECTION       PIC X(01).
               10  WS-FL-LINE-NO       PIC 9(02).
               10  WS-FL-DESC          PIC X(40).
       01  WS-FORMULA-AMOUNTS.
           05  WS-FA-ENTRY             OCCURS 12 TIMES.
               10  WS-FA-AMOUNT        PIC S9(15)V99    COMP-3.
               10  WS-FA-COUNT         PIC S9(07)       COMP-3.
       01  WS-FL-MAX                   PIC S9(04) COMP  VALUE 12.
       01  WS-FL-SUB                   PIC S9(04) COMP  VALUE ZERO.
      *    ---- TABLE POSITIONS OF THE LINES ------------------------
       01  WS-LINE-INDEXES.
           05  WS-IX-C01               PIC S9(04) COMP  VALUE 1.
           05  WS-IX-C02               PIC S9(04) COMP  VALUE 2.
           05  WS-IX-C03               PIC S9(04) COMP  VALUE 3.
           05  WS-IX-C04               PIC S9(04) COMP  VALUE 4.
           05  WS-IX-D01               PIC S9(04) COMP  VALUE 5.
           05  WS-IX-D02               PIC S9(04) COMP  VALUE 6.
           05  WS-IX-D03               PIC S9(04) COMP  VALUE 7.
           05  WS-IX-S01               PIC S9(04) COMP  VALUE 8.
           05  WS-IX-S02               PIC S9(04) COMP  VALUE 9.
           05  WS-IX-S03               PIC S9(04) COMP  VALUE 10.
           05  WS-IX-S04               PIC S9(04) COMP  VALUE 11.
           05  WS-IX-S05               PIC S9(04) COMP  VALUE 12.
      *----------------------------------------------------------------*
      * FORMULA CONSTANTS                                              *
      *----------------------------------------------------------------*
       01  WS-FORMULA-CONSTANTS.
           05  WS-DEBIT-REDUCTION-PCT  PIC SV9(04) COMP-3 VALUE .0100.
           05  WS-FAIL-AGE-LIMIT       PIC S9(03)  COMP-3 VALUE +30.
           05  WS-BASE-CCY             PIC X(03)          VALUE 'USD'.
      *----------------------------------------------------------------*
      * AMOUNT WORK                                                    *
      *----------------------------------------------------------------*
       01  WS-AMOUNT-WORK.
           05  WS-ITEM-AMOUNT          PIC S9(15)V99    COMP-3.
           05  WS-ITEM-USD             PIC S9(15)V99    COMP-3.
           05  WS-ITEM-CCY             PIC X(03).
           05  WS-AGG-DEBITS           PIC S9(15)V99    COMP-3.
           05  WS-REDUCTION            PIC S9(15)V99    COMP-3.
           05  WS-NET-REQ              PIC S9(15)V99    COMP-3.
           05  WS-TRACE-LINE           PIC X(03).
       01  WS-ACCT-WORK.
           05  WS-CHK-ACCT             PIC X(10).
           05  WS-LAST-ACCT            PIC X(10)  VALUE LOW-VALUES.
           05  WS-LAST-MARGIN-TYPE     PIC X(01)  VALUE SPACE.
           05  WS-LAST-FOUND-SW        PIC X(01)  VALUE 'N'.
      *----------------------------------------------------------------*
      * STATISTICS                                                     *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-PARM-CARDS           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CASH-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-FIRM            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-MARGIN          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-ZERO            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-DEBIT-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-DEBIT-AMT       PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-CASH-HASH            PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-MGREQ-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MGREQ-FIRM           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MGREQ-OLD-DATE       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MGREQ-UNSEC-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MGREQ-UNSEC-AMT      PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-MGREQ-HASH           PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-ENTL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-NOT-CA          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-FIRM            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-NO-CASH         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-HASH            PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-FAIL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAIL-FIRM            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAIL-AGED-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAIL-AGED-AMT        PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-FAIL-OTHER           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAIL-AT-CONTRACT     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAIL-HASH            PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-ACCT-NOT-FOUND       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-CALLS             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-STALE             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-MISSING           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-OUT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CREDIT-ITEMS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DEBIT-ITEMS          PIC S9(09) COMP-3 VALUE ZERO.
       COPY RRFORM.
       COPY SRCASH.
       COPY MGREQ.
       COPY CAENTL.
       COPY SRFAIL.
       COPY CMACCT.
       COPY CMDATEW.
       COPY CMFXLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-CASH-BALANCES THRU 2000-EXIT
               UNTIL END-OF-CASHBAL.
           PERFORM 3000-MARGIN-BALANCES THRU 3000-EXIT
               UNTIL END-OF-MGREQ.
           PERFORM 4000-ENTITLEMENTS THRU 4000-EXIT
               UNTIL END-OF-ENTLMAST.
           PERFORM 5000-FAILS THRU 5000-EXIT
               UNTIL END-OF-FAILS.
           PERFORM 6000-COMPUTE-RESERVE THRU 6000-EXIT.
           PERFORM 7000-WRITE-FORMULA THRU 7000-EXIT.
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
           MOVE 'CUSTOMER RESERVE COMPUTATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           INITIALIZE WS-FORMULA-AMOUNTS.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           IF NOT BANK-CARD-READ
               DISPLAY 'RRB100 W - NO RESERVE-BANK CARD, DEPOSIT = 0'
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               MOVE 'WRIT'     TO AU-FUNCTION
               MOVE 'NOBANK'   TO AU-EVENT
               MOVE 'W'        TO AU-SEVERITY
               MOVE 'RESERVE-BANK CARD MISSING - DEPOSIT TAKEN AS ZERO'
                               TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
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
           OPEN INPUT MGREQIN-FILE.
           IF WS-MGREQIN-STATUS NOT = '00'
               MOVE 'MGREQIN' TO AB-DDNAME
               MOVE WS-MGREQIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT ENTLMAST-FILE.
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE 'ENTLMAST' TO AB-DDNAME
               MOVE WS-ENTLMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT FAILIN-FILE.
           IF WS-FAILIN-STATUS NOT = '00'
               MOVE 'FAILIN' TO AB-DDNAME
               MOVE WS-FAILIN-STATUS TO AB-FILE-STATUS
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
           OPEN OUTPUT RESVOUT-FILE.
           IF WS-RESVOUT-STATUS NOT = '00'
               MOVE 'RESVOUT' TO AB-DDNAME
               MOVE WS-RESVOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-CASHBAL THRU 8000-EXIT.
           PERFORM 8100-READ-MGREQ THRU 8100-EXIT.
           PERFORM 8200-READ-ENTLMAST THRU 8200-EXIT.
           PERFORM 8300-READ-FAIL THRU 8300-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CONTROL CARDS.  '*' IN COLUMN 1 = COMMENT.                     *
      *   RESERVE-BANK=NNNNNNNNNNNNN.NN  BALANCE OF THE SPECIAL        *
      *                RESERVE BANK ACCOUNT AT THE CLOSE (TREASURY     *
      *                UPDATES THE MEMBER EVERY MORNING - RUNBOOK      *
      *                RR-010)                                         *
      *   TRACE=Y      DISPLAY EVERY FORMULA ITEM (AUDIT REQUESTS)     *
      *----------------------------------------------------------------*
       1100-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               MOVE 'SYSIN' TO AB-DDNAME
               MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1100-READ-PARMS' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
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
           ADD 1 TO WS-PARM-CARDS.
           IF PARM-CARD-REC (1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1110-EXIT
           END-IF.
           DISPLAY 'RRB100 CARD: ' PARM-CARD-REC (1:72).
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING.
           EVALUATE WS-PARM-KEYWORD
               WHEN 'RESERVE-BANK'
                   PERFORM 1120-EDIT-BANK-AMOUNT THRU 1120-EXIT
               WHEN 'TRACE'
                   IF WS-PARM-VALUE (1:1) = 'Y'
                       MOVE 'Y' TO WS-TRACE-SW
                   END-IF
               WHEN OTHER
                   DISPLAY 'RRB100 W - UNKNOWN CONTROL CARD IGNORED'
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * RESERVE-BANK=0000012345678.90  (13 DIGITS . 2 DIGITS, CHG34020)*
      * THE CARD IS THE FILED DEPOSIT - A BAD CARD STOPS THE RUN.      *
      *----------------------------------------------------------------*
       1120-EDIT-BANK-AMOUNT.
           IF WS-PV-DOLLARS NOT NUMERIC
           OR WS-PV-POINT NOT = '.'
           OR WS-PV-CENTS NOT NUMERIC
           OR WS-PV-REST NOT = SPACES
               MOVE 'SYSIN' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '1120-EDIT-BANK-AMOUNT' TO AB-PARAGRAPH
               MOVE WS-PARM-VALUE TO AB-KEY
               MOVE 'RESERVE-BANK CARD NOT NNNNNNNNNNNNN.NN'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF BANK-CARD-READ
               DISPLAY 'RRB100 W - SECOND RESERVE-BANK CARD REPLACES '
                       'THE FIRST'
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           MOVE WS-PV-DOLLARS TO WS-PVA-DOLLARS.
           MOVE WS-PV-CENTS   TO WS-PVA-CENTS.
           MOVE WS-PV-AMOUNT  TO WS-RESERVE-BANK-AMT.
           MOVE 'Y' TO WS-BANK-CARD-SW.
       1120-EXIT.
           EXIT.
      *================================================================*
      * C01 - FREE CREDIT BALANCES IN CLIENT CASH ACCOUNTS.            *
      * MARGIN ACCOUNTS ARE TAKEN FROM MG.REQ (C02 / D01) SO THEY ARE  *
      * SKIPPED HERE.  ONLY THE SETTLED BALANCE IS A FREE CREDIT.      *
      *================================================================*
       2000-CASH-BALANCES.
           ADD 1 TO WS-CASH-READ.
           ADD CSH-SD-BALANCE TO WS-CASH-HASH.
           MOVE CSH-ACCT-NO TO WS-CHK-ACCT.
           PERFORM 7500-CLASSIFY-ACCOUNT THRU 7500-EXIT.
           IF FIRM-OR-STREET
               ADD 1 TO WS-CASH-FIRM
               GO TO 2000-NEXT
           END-IF.
           PERFORM 7600-GET-ACCOUNT THRU 7600-EXIT.
           IF WS-LAST-MARGIN-TYPE = 'M'
               ADD 1 TO WS-CASH-MARGIN
               GO TO 2000-NEXT
           END-IF.
           IF CSH-SD-BALANCE = ZERO
               ADD 1 TO WS-CASH-ZERO
               GO TO 2000-NEXT
           END-IF.
      *    DEBIT BALANCES IN CASH ACCOUNTS ARE UNSECURED UNTIL THE
      *    TRADE SETTLES - NOT A FORMULA DEBIT (COMPLIANCE 1997)
           IF CSH-SD-BALANCE < ZERO
               ADD 1 TO WS-CASH-DEBIT-CNT
               ADD CSH-SD-BALANCE TO WS-CASH-DEBIT-AMT
               GO TO 2000-NEXT
           END-IF.
           MOVE CSH-SD-BALANCE TO WS-ITEM-AMOUNT.
           MOVE CSH-CCY        TO WS-ITEM-CCY.
           PERFORM 7700-CONVERT-TO-USD THRU 7700-EXIT.
           MOVE WS-IX-C01 TO WS-FL-SUB.
           MOVE 'C01'     TO WS-TRACE-LINE.
           PERFORM 7800-ADD-TO-LINE THRU 7800-EXIT.
       2000-NEXT.
           PERFORM 8000-READ-CASHBAL THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *================================================================*
      * C02 / D01 - MARGIN ACCOUNTS FROM THE MARGIN REQUIREMENT FILE.  *
      * A DEBIT IS A FORMULA DEBIT ONLY WHEN IT IS SECURED, I.E. THE   *
      * ACCOUNT HAS POSITIVE EQUITY.  UNSECURED DEBITS ARE A CHARGE    *
      * TO NET CAPITAL (NOT COMPUTED HERE).                            *
      *================================================================*
       3000-MARGIN-BALANCES.
           ADD 1 TO WS-MGREQ-READ.
           ADD MRQ-CASH-BALANCE TO WS-MGREQ-HASH.
           IF MRQ-BUS-DATE NOT = DC-BUS-DATE
               ADD 1 TO WS-MGREQ-OLD-DATE
               IF NOT MGREQ-DATE-WARNED
                   MOVE 'Y' TO WS-MGREQ-DATE-SW
                   PERFORM 3100-MGREQ-DATE-WARNING THRU 3100-EXIT
               END-IF
           END-IF.
           MOVE MRQ-ACCT-NO TO WS-CHK-ACCT.
           PERFORM 7500-CLASSIFY-ACCOUNT THRU 7500-EXIT.
           IF FIRM-OR-STREET
               ADD 1 TO WS-MGREQ-FIRM
               GO TO 3000-NEXT
           END-IF.
           IF MRQ-CREDIT-BALANCE > ZERO
               MOVE MRQ-CREDIT-BALANCE TO WS-ITEM-USD
               MOVE WS-IX-C02 TO WS-FL-SUB
               MOVE 'C02'     TO WS-TRACE-LINE
               PERFORM 7800-ADD-TO-LINE THRU 7800-EXIT
           END-IF.
           IF MRQ-DEBIT-BALANCE > ZERO
               IF MRQ-EQUITY > ZERO
                   MOVE MRQ-DEBIT-BALANCE TO WS-ITEM-USD
                   MOVE WS-IX-D01 TO WS-FL-SUB
                   MOVE 'D01'     TO WS-TRACE-LINE
                   PERFORM 7800-ADD-TO-LINE THRU 7800-EXIT
               ELSE
                   ADD 1 TO WS-MGREQ-UNSEC-CNT
                   ADD MRQ-DEBIT-BALANCE TO WS-MGREQ-UNSEC-AMT
               END-IF
           END-IF.
       3000-NEXT.
           PERFORM 8100-READ-MGREQ THRU 8100-EXIT.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * THE MARGIN FILE IS THE LATEST GENERATION, WHICHEVER DAY THE    *
      * MARGIN JOB LAST RAN.  WARN ONCE AND CARRY ON.                  *
      *----------------------------------------------------------------*
       3100-MGREQ-DATE-WARNING.
           DISPLAY 'RRB100 W - MG.REQ BUSINESS DATE ' MRQ-BUS-DATE
                   ' NOT = ' DC-BUS-DATE.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'MGREQOLD'     TO AU-EVENT.
           MOVE 'W'            TO AU-SEVERITY.
           MOVE MRQ-BUS-DATE   TO AU-KEY.
           MOVE 'MARGIN REQUIREMENT FILE IS NOT FROM THE BUSINESS DATE'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       3100-EXIT.
           EXIT.
      *================================================================*
      * C03 - DIVIDENDS AND OTHER ENTITLEMENTS CALCULATED BUT NOT YET  *
      * PAID (ENTLMAST STATUS CA).  NET CASH ONLY - CASH IN LIEU IS    *
      * PAID THROUGH THE AGENT AND IS NOT OWED BY THE FIRM (CHG30112). *
      *================================================================*
       4000-ENTITLEMENTS.
           ADD 1 TO WS-ENTL-READ.
           IF NOT ENT-CALCULATED
               ADD 1 TO WS-ENTL-NOT-CA
               GO TO 4000-NEXT
           END-IF.
           MOVE ENT-ACCT-NO TO WS-CHK-ACCT.
           PERFORM 7500-CLASSIFY-ACCOUNT THRU 7500-EXIT.
           IF FIRM-OR-STREET
               ADD 1 TO WS-ENTL-FIRM
               GO TO 4000-NEXT
           END-IF.
           IF ENT-NET-CASH NOT > ZERO
               ADD 1 TO WS-ENTL-NO-CASH
               GO TO 4000-NEXT
           END-IF.
           ADD ENT-NET-CASH TO WS-ENTL-HASH.
           MOVE ENT-NET-CASH TO WS-ITEM-AMOUNT.
           MOVE ENT-CCY      TO WS-ITEM-CCY.
           PERFORM 7700-CONVERT-TO-USD THRU 7700-EXIT.
           MOVE WS-IX-C03 TO WS-FL-SUB.
           MOVE 'C03'     TO WS-TRACE-LINE.
           PERFORM 7800-ADD-TO-LINE THRU 7800-EXIT.
       4000-NEXT.
           PERFORM 8200-READ-ENTLMAST THRU 8200-EXIT.
       4000-EXIT.
           EXIT.
      *================================================================*
      * C04 / D02 - SETTLEMENT FAILS OF CLIENT ACCOUNTS.               *
      *   RECEIVE SIDE (BUY, BCV)  - CREDIT: THE FIRM OWES THE CLIENT  *
      *                              THE SECURITIES                    *
      *   DELIVER SIDE (SEL, SSL)  - DEBIT, ONLY WHILE 30 DAYS OR LESS *
      * VALUE = MARKET VALUE FROM SRB250.  A FAIL WITHOUT A MARKET     *
      * VALUE (POSITION NOT YET VALUED) IS TAKEN AT CONTRACT VALUE.    *
      *================================================================*
       5000-FAILS.
           ADD 1 TO WS-FAIL-READ.
           MOVE FLR-ACCT-NO TO WS-CHK-ACCT.
           PERFORM 7500-CLASSIFY-ACCOUNT THRU 7500-EXIT.
           IF FIRM-OR-STREET
               ADD 1 TO WS-FAIL-FIRM
               GO TO 5000-NEXT
           END-IF.
           IF FLR-MKT-VALUE-USD NOT NUMERIC
               MOVE ZERO TO FLR-MKT-VALUE-USD
           END-IF.
           IF FLR-MKT-VALUE-USD = ZERO
               ADD 1 TO WS-FAIL-AT-CONTRACT
               IF FLR-CASH < ZERO
                   COMPUTE WS-ITEM-AMOUNT = FLR-CASH * -1
               ELSE
                   MOVE FLR-CASH TO WS-ITEM-AMOUNT
               END-IF
               MOVE FLR-CCY TO WS-ITEM-CCY
               PERFORM 7700-CONVERT-TO-USD THRU 7700-EXIT
           ELSE
               IF FLR-MKT-VALUE-USD < ZERO
                   COMPUTE WS-ITEM-USD = FLR-MKT-VALUE-USD * -1
               ELSE
                   MOVE FLR-MKT-VALUE-USD TO WS-ITEM-USD
               END-IF
           END-IF.
           ADD WS-ITEM-USD TO WS-FAIL-HASH.
           EVALUATE FLR-ACT-TYPE
               WHEN 'BUY'
               WHEN 'BCV'
                   MOVE WS-IX-C04 TO WS-FL-SUB
                   MOVE 'C04'     TO WS-TRACE-LINE
                   PERFORM 7800-ADD-TO-LINE THRU 7800-EXIT
               WHEN 'SEL'
               WHEN 'SSL'
                   IF FLR-FAIL-DAYS > WS-FAIL-AGE-LIMIT
                       ADD 1 TO WS-FAIL-AGED-CNT
                       ADD WS-ITEM-USD TO WS-FAIL-AGED-AMT
                   ELSE
                       MOVE WS-IX-D02 TO WS-FL-SUB
                       MOVE 'D02'     TO WS-TRACE-LINE
                       PERFORM 7800-ADD-TO-LINE THRU 7800-EXIT
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-FAIL-OTHER
           END-EVALUATE.
       5000-NEXT.
           PERFORM 8300-READ-FAIL THRU 8300-EXIT.
       5000-EXIT.
           EXIT.
      *================================================================*
      * THE COMPUTATION                                                *
      *================================================================*
       6000-COMPUTE-RESERVE.
      *    ---- D03 1 PCT OF AGGREGATE DEBIT ITEMS (NEGATIVE LINE) ---
           COMPUTE WS-AGG-DEBITS = WS-FA-AMOUNT (WS-IX-D01)
                                 + WS-FA-AMOUNT (WS-IX-D02).
           COMPUTE WS-REDUCTION ROUNDED =
                   WS-AGG-DEBITS * WS-DEBIT-REDUCTION-PCT.
           COMPUTE WS-FA-AMOUNT (WS-IX-D03) = WS-REDUCTION * -1.
           MOVE ZERO TO WS-FA-COUNT (WS-IX-D03).
      *    ---- S01 TOTAL CREDITS ------------------------------------
           MOVE ZERO TO WS-FA-AMOUNT (WS-IX-S01)
                        WS-FA-COUNT (WS-IX-S01).
           PERFORM VARYING WS-FL-SUB FROM 1 BY 1
                   UNTIL WS-FL-SUB > WS-FL-MAX
               IF WS-FL-SECTION (WS-FL-SUB) = 'C'
                   ADD WS-FA-AMOUNT (WS-FL-SUB)
                                   TO WS-FA-AMOUNT (WS-IX-S01)
                   ADD WS-FA-COUNT (WS-FL-SUB)
                                   TO WS-FA-COUNT (WS-IX-S01)
               END-IF
           END-PERFORM.
      *    ---- S02 TOTAL DEBITS (NET OF THE REDUCTION) --------------
           MOVE ZERO TO WS-FA-AMOUNT (WS-IX-S02)
                        WS-FA-COUNT (WS-IX-S02).
           PERFORM VARYING WS-FL-SUB FROM 1 BY 1
                   UNTIL WS-FL-SUB > WS-FL-MAX
               IF WS-FL-SECTION (WS-FL-SUB) = 'D'
                   ADD WS-FA-AMOUNT (WS-FL-SUB)
                                   TO WS-FA-AMOUNT (WS-IX-S02)
                   ADD WS-FA-COUNT (WS-FL-SUB)
                                   TO WS-FA-COUNT (WS-IX-S02)
               END-IF
           END-PERFORM.
           MOVE WS-FA-COUNT (WS-IX-S01) TO WS-CREDIT-ITEMS.
           MOVE WS-FA-COUNT (WS-IX-S02) TO WS-DEBIT-ITEMS.
      *    ---- S03 REQUIREMENT = EXCESS OF CREDITS OVER DEBITS ------
           COMPUTE WS-NET-REQ = WS-FA-AMOUNT (WS-IX-S01)
                              - WS-FA-AMOUNT (WS-IX-S02).
           IF WS-NET-REQ < ZERO
               MOVE ZERO TO WS-FA-AMOUNT (WS-IX-S03)
           ELSE
               MOVE WS-NET-REQ TO WS-FA-AMOUNT (WS-IX-S03)
           END-IF.
           MOVE ZERO TO WS-FA-COUNT (WS-IX-S03).
      *    ---- S04 DEPOSIT ------------------------------------------
           MOVE WS-RESERVE-BANK-AMT TO WS-FA-AMOUNT (WS-IX-S04).
           IF BANK-CARD-READ
               MOVE 1 TO WS-FA-COUNT (WS-IX-S04)
           ELSE
               MOVE ZERO TO WS-FA-COUNT (WS-IX-S04)
           END-IF.
      *    ---- S05 EXCESS / (DEFICIENCY) ----------------------------
           COMPUTE WS-FA-AMOUNT (WS-IX-S05) =
                   WS-FA-AMOUNT (WS-IX-S04) - WS-FA-AMOUNT (WS-IX-S03).
           MOVE ZERO TO WS-FA-COUNT (WS-IX-S05).
           IF WS-FA-AMOUNT (WS-IX-S05) < ZERO
               PERFORM 6100-DEFICIENCY THRU 6100-EXIT
           END-IF.
           IF WS-FA-AMOUNT (WS-IX-S03) < ZERO
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * A DEFICIENCY MUST BE DEPOSITED BEFORE 10:00 ON THE NEXT        *
      * BUSINESS DAY AND TREASURY / COMPLIANCE NOTIFIED (RUNBOOK       *
      * RR-010).  THE JOB CONTINUES WITH RC 4.                         *
      *----------------------------------------------------------------*
       6100-DEFICIENCY.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           MOVE WS-FA-AMOUNT (WS-IX-S05) TO WS-DISP-AMT.
           DISPLAY 'RRB100 *** RESERVE DEFICIENCY ' WS-DISP-AMT
                   ' - DEPOSIT BY ' DC-NEXT-BUS-DATE ' 10:00 ***'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'RSVDEFIC'     TO AU-EVENT.
           MOVE 'W'            TO AU-SEVERITY.
           MOVE WS-DISP-AMT    TO AU-KEY.
           MOVE 'CUSTOMER RESERVE DEFICIENCY - NOTIFY TREASURY'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       6100-EXIT.
           EXIT.
      *================================================================*
      * ONE RRFORM RECORD PER FORMULA LINE, IN FILED ORDER             *
      *================================================================*
       7000-WRITE-FORMULA.
           PERFORM VARYING WS-FL-SUB FROM 1 BY 1
                   UNTIL WS-FL-SUB > WS-FL-MAX
               MOVE SPACES                    TO RRF-FORMULA-REC
               MOVE DC-BUS-DATE               TO RRF-BUS-DATE
               MOVE WS-FL-SECTION (WS-FL-SUB) TO RRF-SECTION
               MOVE WS-FL-LINE-NO (WS-FL-SUB) TO RRF-LINE-NO
               MOVE WS-FL-DESC (WS-FL-SUB)    TO RRF-DESC
               MOVE WS-FA-AMOUNT (WS-FL-SUB)  TO RRF-AMOUNT
               MOVE WS-FA-COUNT (WS-FL-SUB)   TO RRF-ITEM-COUNT
               PERFORM 8500-WRITE-FORMULA THRU 8500-EXIT
           END-PERFORM.
       7000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CUSTOMER OR FIRM.  CUSTOMER ACCOUNTS ARE ALL-NUMERIC; FIRM     *
      * INVENTORY AND STREET ACCOUNTS START WITH A LETTER AND SORT     *
      * LOW (PAB ACCOUNTS ARE COMPUTED BY THE CONTROLLERS - NOT HERE). *
      *----------------------------------------------------------------*
       7500-CLASSIFY-ACCOUNT.
           IF WS-CHK-ACCT (1:1) < '0'
               MOVE 'N' TO WS-CUSTOMER-SW
           ELSE
               MOVE 'Y' TO WS-CUSTOMER-SW
           END-IF.
       7500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ACCOUNT MASTER - MARGIN TYPE.  CASHBAL IS IN ACCOUNT ORDER SO  *
      * ONLY THE LAST ACCOUNT IS KEPT.  NOT ON FILE = CASH ACCOUNT.    *
      *----------------------------------------------------------------*
       7600-GET-ACCOUNT.
           IF WS-CHK-ACCT = WS-LAST-ACCT
               GO TO 7600-EXIT
           END-IF.
           MOVE WS-CHK-ACCT TO WS-LAST-ACCT ACCTMAST-KEY.
           READ ACCTMAST-FILE INTO ACCT-MASTER-REC.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE 'Y'              TO WS-LAST-FOUND-SW
                   MOVE ACCT-MARGIN-TYPE TO WS-LAST-MARGIN-TYPE
               WHEN ACCTMAST-NOTFND
                   MOVE 'N'              TO WS-LAST-FOUND-SW
                   MOVE 'C'              TO WS-LAST-MARGIN-TYPE
                   ADD 1 TO WS-ACCT-NOT-FOUND
                   DISPLAY 'RRB100 W - ACCOUNT NOT ON MASTER '
                           WS-CHK-ACCT
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '7600-GET-ACCOUNT' TO AB-PARAGRAPH
                   MOVE WS-CHK-ACCT TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       7600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * WS-ITEM-AMOUNT IN WS-ITEM-CCY -> WS-ITEM-USD (CMU040, RATE OF  *
      * THE BUSINESS DATE).  NO RATE: THE AMOUNT IS TAKEN AT PAR AND   *
      * THE RUN ENDS RC 4 SO TREASURY CAN REVIEW (CHG19002).           *
      *----------------------------------------------------------------*
       7700-CONVERT-TO-USD.
           IF WS-ITEM-CCY = WS-BASE-CCY OR WS-ITEM-CCY = SPACES
               MOVE WS-ITEM-AMOUNT TO WS-ITEM-USD
               GO TO 7700-EXIT
           END-IF.
           ADD 1 TO WS-FX-CALLS.
           MOVE WS-ITEM-CCY    TO FX-FROM-CCY.
           MOVE WS-BASE-CCY    TO FX-TO-CCY.
           MOVE DC-BUS-DATE    TO FX-RATE-DATE.
           MOVE WS-ITEM-AMOUNT TO FX-AMOUNT-IN.
           CALL 'CMU040' USING FX-CONVERT-PARMS.
           EVALUATE TRUE
               WHEN FX-OK
                   MOVE FX-AMOUNT-OUT TO WS-ITEM-USD
               WHEN FX-STALE-RATE
                   ADD 1 TO WS-FX-STALE
                   MOVE FX-AMOUNT-OUT TO WS-ITEM-USD
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN FX-RATE-NOT-FOUND
                   ADD 1 TO WS-FX-MISSING
                   MOVE WS-ITEM-AMOUNT TO WS-ITEM-USD
                   DISPLAY 'RRB100 W - NO FX RATE FOR ' WS-ITEM-CCY
                           ' - TAKEN AT PAR'
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '7700-CONVERT-TO-USD' TO AB-PARAGRAPH
                   MOVE WS-ITEM-CCY TO AB-KEY
                   MOVE FX-MESSAGE TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       7700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ADD WS-ITEM-USD TO FORMULA LINE WS-FL-SUB                      *
      *----------------------------------------------------------------*
       7800-ADD-TO-LINE.
           ADD WS-ITEM-USD TO WS-FA-AMOUNT (WS-FL-SUB).
           ADD 1           TO WS-FA-COUNT (WS-FL-SUB).
           IF TRACE-ON
               MOVE WS-ITEM-USD TO WS-DISP-AMT
               DISPLAY 'RRB100 TRACE ' WS-TRACE-LINE ' '
                       WS-CHK-ACCT ' ' WS-DISP-AMT
           END-IF.
       7800-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-CASHBAL.
           READ CASHBAL-FILE INTO CSH-CASH-REC.
           EVALUATE TRUE
               WHEN CASHBAL-OK
                   IF CSH-SD-BALANCE NOT NUMERIC
                       MOVE ZERO TO CSH-SD-BALANCE
                   END-IF
               WHEN CASHBAL-EOF
                   MOVE 'Y' TO WS-CASH-EOF-SW
               WHEN OTHER
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-CASHBAL' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-MGREQ.
      *----------------------------------------------------------------*
           READ MGREQIN-FILE INTO MRQ-REQUIREMENT-REC.
           EVALUATE TRUE
               WHEN MGREQIN-OK
                   CONTINUE
               WHEN MGREQIN-EOF
                   MOVE 'Y' TO WS-MGREQ-EOF-SW
               WHEN OTHER
                   MOVE 'MGREQIN' TO AB-DDNAME
                   MOVE WS-MGREQIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-MGREQ' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-READ-ENTLMAST.
      *----------------------------------------------------------------*
           READ ENTLMAST-FILE INTO ENT-ENTITLEMENT-REC.
           EVALUATE TRUE
               WHEN ENTLMAST-OK
                   IF ENT-NET-CASH NOT NUMERIC
                       MOVE ZERO TO ENT-NET-CASH
                   END-IF
               WHEN ENTLMAST-EOF
                   MOVE 'Y' TO WS-ENTL-EOF-SW
               WHEN OTHER
                   MOVE 'ENTLMAST' TO AB-DDNAME
                   MOVE WS-ENTLMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8200-READ-ENTLMAST' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-READ-FAIL.
      *----------------------------------------------------------------*
           READ FAILIN-FILE INTO FLR-FAIL-REC.
           EVALUATE TRUE
               WHEN FAILIN-OK
                   CONTINUE
               WHEN FAILIN-EOF
                   MOVE 'Y' TO WS-FAIL-EOF-SW
               WHEN OTHER
                   MOVE 'FAILIN' TO AB-DDNAME
                   MOVE WS-FAILIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8300-READ-FAIL' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-WRITE-FORMULA.
      *----------------------------------------------------------------*
           WRITE RESVOUT-REC FROM RRF-FORMULA-REC.
           IF WS-RESVOUT-STATUS NOT = '00'
               MOVE 'RESVOUT' TO AB-DDNAME
               MOVE WS-RESVOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8500-WRITE-FORMULA' TO AB-PARAGRAPH
               MOVE RRF-DESC TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-LINES-OUT.
       8500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8600-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RRB100'       TO CT-STAGE.
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
           CLOSE CASHBAL-FILE MGREQIN-FILE ENTLMAST-FILE
                 FAILIN-FILE ACCTMAST-FILE.
           CLOSE RESVOUT-FILE.
           IF WS-RESVOUT-STATUS NOT = '00'
               MOVE 'RESVOUT' TO AB-DDNAME
               MOVE WS-RESVOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
      *    ---- CONTROL TOTALS ---------------------------------------
           MOVE 'CASHBAL-IN'     TO CT-COUNTER-NAME.
           MOVE WS-CASH-READ     TO CT-COUNT.
           MOVE WS-CASH-HASH     TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'MGREQ-IN'       TO CT-COUNTER-NAME.
           MOVE WS-MGREQ-READ    TO CT-COUNT.
           MOVE WS-MGREQ-HASH    TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'ENTL-IN'        TO CT-COUNTER-NAME.
           MOVE WS-ENTL-READ     TO CT-COUNT.
           MOVE WS-ENTL-HASH     TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'FAILS-IN'       TO CT-COUNTER-NAME.
           MOVE WS-FAIL-READ     TO CT-COUNT.
           MOVE WS-FAIL-HASH     TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'CREDITS'        TO CT-COUNTER-NAME.
           MOVE WS-CREDIT-ITEMS  TO CT-COUNT.
           MOVE WS-FA-AMOUNT (WS-IX-S01) TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'DEBITS'         TO CT-COUNTER-NAME.
           MOVE WS-DEBIT-ITEMS   TO CT-COUNT.
           MOVE WS-FA-AMOUNT (WS-IX-S02) TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           MOVE 'RESERVE-OUT'    TO CT-COUNTER-NAME.
           MOVE WS-LINES-OUT     TO CT-COUNT.
           MOVE WS-FA-AMOUNT (WS-IX-S05) TO CT-AMOUNT.
           PERFORM 8600-POST-TOTAL THRU 8600-EXIT.
           PERFORM 9100-DISPLAY-STATISTICS THRU 9100-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
                   MOVE 'RESERVE COMPUTATION ENDED IN ERROR'
                                   TO AU-MESSAGE
               WHEN WS-RETURN-CODE = 4
                   MOVE 'W' TO AU-SEVERITY
                   MOVE 'RESERVE COMPUTATION ENDED WITH WARNINGS'
                                   TO AU-MESSAGE
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
                   MOVE 'RESERVE COMPUTATION ENDED' TO AU-MESSAGE
           END-EVALUATE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9100-DISPLAY-STATISTICS.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'.
           DISPLAY '* RRB100 - CUSTOMER RESERVE COMPUTATION        *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-PARM-CARDS TO WS-DISP-CNT.
           DISPLAY ' CONTROL CARDS READ       : ' WS-DISP-CNT.
           MOVE WS-CASH-READ TO WS-DISP-CNT.
           DISPLAY ' CASH BALANCES READ       : ' WS-DISP-CNT.
           MOVE WS-CASH-FIRM TO WS-DISP-CNT.
           DISPLAY '   FIRM / STREET          : ' WS-DISP-CNT.
           MOVE WS-CASH-MARGIN TO WS-DISP-CNT.
           DISPLAY '   MARGIN ACCOUNTS        : ' WS-DISP-CNT.
           MOVE WS-CASH-ZERO TO WS-DISP-CNT.
           DISPLAY '   ZERO SETTLED BALANCE   : ' WS-DISP-CNT.
           MOVE WS-CASH-DEBIT-CNT TO WS-DISP-CNT.
           DISPLAY '   CASH ACCOUNT DEBITS    : ' WS-DISP-CNT.
           MOVE WS-CASH-DEBIT-AMT TO WS-DISP-AMT.
           DISPLAY '   CASH ACCT DEBIT AMOUNT : ' WS-DISP-AMT.
           MOVE WS-MGREQ-READ TO WS-DISP-CNT.
           DISPLAY ' MARGIN REQUIREMENTS READ : ' WS-DISP-CNT.
           MOVE WS-MGREQ-FIRM TO WS-DISP-CNT.
           DISPLAY '   FIRM / STREET          : ' WS-DISP-CNT.
           MOVE WS-MGREQ-OLD-DATE TO WS-DISP-CNT.
           DISPLAY '   NOT FROM BUSINESS DATE : ' WS-DISP-CNT.
           MOVE WS-MGREQ-UNSEC-CNT TO WS-DISP-CNT.
           DISPLAY '   UNSECURED DEBITS       : ' WS-DISP-CNT.
           MOVE WS-MGREQ-UNSEC-AMT TO WS-DISP-AMT.
           DISPLAY '   UNSECURED DEBIT AMOUNT : ' WS-DISP-AMT.
           MOVE WS-ENTL-READ TO WS-DISP-CNT.
           DISPLAY ' ENTITLEMENTS READ        : ' WS-DISP-CNT.
           MOVE WS-ENTL-NOT-CA TO WS-DISP-CNT.
           DISPLAY '   NOT STATUS CA          : ' WS-DISP-CNT.
           MOVE WS-ENTL-FIRM TO WS-DISP-CNT.
           DISPLAY '   FIRM / STREET          : ' WS-DISP-CNT.
           MOVE WS-ENTL-NO-CASH TO WS-DISP-CNT.
           DISPLAY '   NO NET CASH            : ' WS-DISP-CNT.
           MOVE WS-FAIL-READ TO WS-DISP-CNT.
           DISPLAY ' FAILS READ               : ' WS-DISP-CNT.
           MOVE WS-FAIL-FIRM TO WS-DISP-CNT.
           DISPLAY '   FIRM / STREET          : ' WS-DISP-CNT.
           MOVE WS-FAIL-AGED-CNT TO WS-DISP-CNT.
           DISPLAY '   DELIVER OVER 30 DAYS   : ' WS-DISP-CNT.
           MOVE WS-FAIL-AGED-AMT TO WS-DISP-AMT.
           DISPLAY '   OVER 30 DAYS AMOUNT    : ' WS-DISP-AMT.
           MOVE WS-FAIL-AT-CONTRACT TO WS-DISP-CNT.
           DISPLAY '   AT CONTRACT VALUE      : ' WS-DISP-CNT.
           MOVE WS-FAIL-OTHER TO WS-DISP-CNT.
           DISPLAY '   OTHER ACTIVITY TYPES   : ' WS-DISP-CNT.
           MOVE WS-ACCT-NOT-FOUND TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS NOT ON MASTER   : ' WS-DISP-CNT.
           MOVE WS-FX-CALLS TO WS-DISP-CNT.
           DISPLAY ' FX CONVERSIONS           : ' WS-DISP-CNT.
           MOVE WS-FX-STALE TO WS-DISP-CNT.
           DISPLAY '   STALE RATES            : ' WS-DISP-CNT.
           MOVE WS-FX-MISSING TO WS-DISP-CNT.
           DISPLAY '   NO RATE (AT PAR)       : ' WS-DISP-CNT.
           DISPLAY '------------------------------------------------'.
           PERFORM VARYING WS-FL-SUB FROM 1 BY 1
                   UNTIL WS-FL-SUB > WS-FL-MAX
               MOVE WS-FA-AMOUNT (WS-FL-SUB) TO WS-DISP-AMT
               MOVE WS-FA-COUNT (WS-FL-SUB)  TO WS-DISP-CNT
               DISPLAY ' ' WS-FL-SECTION (WS-FL-SUB)
                       WS-FL-LINE-NO (WS-FL-SUB) ' '
                       WS-FL-DESC (WS-FL-SUB) (1:36) ' '
                       WS-DISP-CNT ' ' WS-DISP-AMT
           END-PERFORM.
           DISPLAY '------------------------------------------------'.
           MOVE WS-LINES-OUT TO WS-DISP-CNT.
           DISPLAY ' FORMULA LINES WRITTEN    : ' WS-DISP-CNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
       9100-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RRB100 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RRB100 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RRB100 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
