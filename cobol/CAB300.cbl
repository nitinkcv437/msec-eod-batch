      *================================================================*
      * PROGRAM    : CAB300                                            *
      * DESCRIPTION: CORPORATE ACTION ENTITLEMENT CALCULATION.         *
      *              READS THE RECORD DATE ELIGIBILITY SNAPSHOT (ONE   *
      *              RECORD PER HOLDER POSITION, SORTED BY EVENT AND   *
      *              ACCOUNT) AND CALCULATES EACH HOLDER'S CASH AND/OR *
      *              SHARE ENTITLEMENT ACCORDING TO THE EVENT TERMS.   *
      *              WRITES THE DAILY ENTITLEMENT FILE AND THE         *
      *              ENTITLEMENT MASTER, POSTS EVENT TOTALS TO THE     *
      *              EVENT MASTER AND SETS THE EVENT STATUS EL -> EN.  *
      *----------------------------------------------------------------*
      * JOB        : MSCAD030  STEP010  (IKJEFT01 - PLAN MSCAPLN)      *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              SYSIN     CONTROL CARD CAP300A                    *
      *              ELIGIN    ELIGIBILITY SNAPSHOT (CAELIG)           *
      *                        MSEC.PROD.CA.ELIG(0)                    *
      *              CAEVENT   EVENT MASTER KSDS (I-O)                 *
      *              ACCTMAST  ACCOUNT MASTER KSDS (RANDOM)            *
      * OUTPUT     : ENTLOUT   ENTITLEMENTS (CAENTL)                   *
      *                        MSEC.PROD.CA.ENTL(+1)                   *
      *              ENTLMAST  ENTITLEMENT MASTER KSDS (I-O)           *
      * CALLS      : CMD010 (SECURITY)  CMD040 (WITHHOLDING RATE)      *
      *              CMU050 CMU060 CMU080 CMASM01                      *
      *----------------------------------------------------------------*
      * CALCULATION RULES (SEE CA OPERATIONS MANUAL SECTION 4)         *
      *   CDV  GROSS = ELIGIBLE QTY X RATE, TRUNCATED TO THE CENT.     *
      *        WITHHOLDING = GROSS X RATE, ROUNDED HALF UP.            *
      *        NET = GROSS - WITHHOLDING.  FIRM ACCOUNTS ARE EXEMPT.   *
      *   SDV  NEW SHARES = QTY X NEW / OLD                            *
      *   SPL  NEW SHARES = QTY X NEW / OLD - QTY (ADDITIONAL SHARES)  *
      *   RSP  NEW SHARES = QTY X NEW / OLD (REPLACES THE POSITION)    *
      *        FRACTIONS: C CASH IN LIEU AT CIL PRICE (ROUNDED)        *
      *                   D ROUND DOWN   U ROUND UP                    *
      *        REVERSE SPLIT FRACTIONS ARE ALWAYS CASH IN LIEU.        *
      *   MRG  CASH = QTY X MERGER RATE, ROUNDED.  NO WITHHOLDING.     *
      *----------------------------------------------------------------*
      * RETURN CODES:                                                  *
      *   00 CLEAN                                                     *
      *   04 WITHHOLDING RATE NOT FOUND (DEFAULT APPLIED), EVENT NOT   *
      *      ON FILE / WRONG STATUS, PAID ENTITLEMENT NOT REPLACED     *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1994-02-14 DWB  ORIGINAL - CDV, SDV, SPL                       *
      * 1994-03-02 DWB  REVERSE SPLITS                        CHG00412 *
      * 1995-06-12 RJK  CASH IN LIEU OF FRACTIONS             CHG01102 *
      * 1996-08-05 RJK  DUE BILL FLAG                         CHG02731 *
      * 1997-03-17 RJK  GROSS TRUNCATED PER PAYING AGENT      CHG03390 *
      *                 CONVENTION - DO NOT ROUND                      *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2001-04-09 KAP  DECIMALIZATION - CIL PRICE 6 DEC      CHG08260 *
      * 2003-05-19 KAP  FOREIGN HOLDER WITHHOLDING (W-8BEN)   CHG11020 *
      * 2004-11-01 KAP  ENTITLEMENT MASTER KSDS               CHG12877 *
      * 2008-02-25 KAP  CASH MERGER (MRG)                     CHG17444 *
      * 2011-06-20 SPA  AUDIT AND CONTROL TOTALS              CHG21877 *
      * 2014-09-08 SPA  WITHHOLDING TABLE MOVED TO DB2 WHT_RATE        *
      *                 (CMD040) - INTERNAL TABLE REMOVED     CHG27115 *
      * 2016-02-22 MFO  DEFAULT FOREIGN RATE FROM CONTROL CARDCHG29980 *
      * 2020-06-29 MFO  RERUN - REPLACE CALCULATED ROWS ONLY  CHG35512 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAB300.
       AUTHOR.        D W BRANDT.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  02/14/94.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE   ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
      *
           SELECT PARMCARD        ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
      *
           SELECT ELIGIN-FILE     ASSIGN TO ELIGIN
                  FILE STATUS IS WS-ELIGIN-STATUS.
      *
           SELECT CAEVENT-FILE    ASSIGN TO CAEVENT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CAE-EVENT-ID
                  FILE STATUS IS WS-CAEVENT-STATUS.
      *
           SELECT ACCTMAST-FILE   ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCT-NO
                  FILE STATUS IS WS-ACCTMAST-STATUS.
      *
           SELECT ENTLOUT-FILE    ASSIGN TO ENTLOUT
                  FILE STATUS IS WS-ENTLOUT-STATUS.
      *
           SELECT ENTLMAST-FILE   ASSIGN TO ENTLMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS EM-KEY
                  FILE STATUS IS WS-ENTLMAST-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARMCARD-REC                PIC X(80).
      *
       FD  ELIGIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CAELIG.
      *
       FD  CAEVENT-FILE.
           COPY CAEVENT.
      *
       FD  ACCTMAST-FILE.
           COPY CMACCT.
      *
       FD  ENTLOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ENTLOUT-REC                 PIC X(250).
      *
       FD  ENTLMAST-FILE.
       01  EM-MASTER-REC.
           05  EM-KEY                  PIC X(26).
           05  EM-DATA                 PIC X(224).
      *
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
           VALUE 'CAB300 WORKING STORAGE BEGINS'.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAB300'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-ELIGIN-STATUS        PIC X(02) VALUE '00'.
           05  WS-CAEVENT-STATUS       PIC X(02) VALUE '00'.
               88  CAEVENT-OK                    VALUE '00'.
               88  CAEVENT-NOTFND                VALUE '23'.
           05  WS-ACCTMAST-STATUS      PIC X(02) VALUE '00'.
               88  ACCTMAST-OK                   VALUE '00'.
               88  ACCTMAST-NOTFND               VALUE '23'.
           05  WS-ENTLOUT-STATUS       PIC X(02) VALUE '00'.
           05  WS-ENTLMAST-STATUS      PIC X(02) VALUE '00'.
               88  ENTLMAST-OK                   VALUE '00'.
               88  ENTLMAST-DUPKEY               VALUE '22'.
      *
       01  WS-SWITCHES.
           05  WS-ELIG-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-ELIG-EOF                   VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-EVENT-SKIP-SW        PIC X(01) VALUE 'N'.
               88  WS-EVENT-SKIP                 VALUE 'Y'.
           05  WS-EVENT-ACTIVE-SW      PIC X(01) VALUE 'N'.
               88  WS-EVENT-ACTIVE               VALUE 'Y'.
           05  WS-WHT-CACHE-SW         PIC X(01) VALUE 'N'.
               88  WS-WHT-IN-CACHE               VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * CONTROL CARD (CAP300A)                                         *
      * CAB300 DFLTWHT=0.3000 WHTDATE=P                                *
      *   DFLTWHT  RATE APPLIED TO FOREIGN HOLDERS WHEN NO WHT_RATE    *
      *            ROW IS FOUND                                        *
      *   WHTDATE  P = RATE EFFECTIVE ON PAY DATE, R = RECORD DATE     *
      *----------------------------------------------------------------*
       01  WS-PARM-CARD.
           05  PC-PROGRAM              PIC X(06).
           05  FILLER                  PIC X(01).
           05  PC-DFLTWHT-KW           PIC X(08).
           05  PC-DFLTWHT              PIC 9(01)V9(04).
           05  PC-DFLTWHT-X  REDEFINES PC-DFLTWHT
                                       PIC X(05).
           05  FILLER                  PIC X(01).
           05  PC-WHTDATE-KW           PIC X(08).
           05  PC-WHTDATE              PIC X(01).
           05  FILLER                  PIC X(50).
      *
       01  WS-PARMS.
           05  WS-DEFAULT-FGN-RATE     PIC S9(01)V9(04) COMP-3
                                                 VALUE +.3000.
           05  WS-WHT-DATE-OPT         PIC X(01) VALUE 'P'.
               88  WS-WHT-ON-PAY-DATE            VALUE 'P'.
               88  WS-WHT-ON-RECORD-DATE         VALUE 'R'.
      *
       01  WS-CURR-EVENT               PIC X(12) VALUE LOW-VALUES.
       01  WS-ISSUER-COUNTRY           PIC X(02) VALUE SPACES.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * CALCULATION WORK FIELDS                                        *
      *----------------------------------------------------------------*
       01  WS-CALC-WORK.
           05  WS-ELIG-QTY             PIC S9(11)V9(04) COMP-3.
           05  WS-GROSS-CASH           PIC S9(13)V99    COMP-3.
           05  WS-WHT-RATE             PIC S9(01)V9(04) COMP-3.
           05  WS-WHT-AMOUNT           PIC S9(13)V99    COMP-3.
           05  WS-NET-CASH             PIC S9(13)V99    COMP-3.
           05  WS-NEW-SHARES-X         PIC S9(11)V9(06) COMP-3.
           05  WS-WHOLE-SHARES         PIC S9(11)       COMP-3.
           05  WS-FRAC-SHARES          PIC S9(01)V9(06) COMP-3.
           05  WS-CIL-AMOUNT           PIC S9(11)V99    COMP-3.
           05  WS-TAX-STATUS           PIC X(01).
           05  WS-TAX-COUNTRY          PIC X(02).
      *
      *----------------------------------------------------------------*
      * WITHHOLDING RATE CACHE - CLEARED AT EACH EVENT                 *
      *----------------------------------------------------------------*
       01  WS-WHT-CACHE-CTL.
           05  WS-WC-COUNT             PIC S9(04) COMP VALUE ZERO.
           05  WS-WC-MAX               PIC S9(04) COMP VALUE +40.
           05  WS-WC-SUB               PIC S9(04) COMP VALUE ZERO.
       01  WS-WHT-CACHE.
           05  WS-WC-ENTRY             OCCURS 40 TIMES.
               10  WS-WC-COUNTRY       PIC X(02).
               10  WS-WC-STATUS        PIC X(01).
               10  WS-WC-RATE          PIC S9(01)V9(04) COMP-3.
               10  WS-WC-DEFAULTED     PIC X(01).
      *
      *----------------------------------------------------------------*
      * EVENT ACCUMULATORS                                             *
      *----------------------------------------------------------------*
       01  WS-EVENT-TOTALS.
           05  WS-EV-COUNT             PIC S9(07)       COMP-3.
           05  WS-EV-ELIG-QTY          PIC S9(13)V9(04) COMP-3.
           05  WS-EV-GROSS             PIC S9(15)V99    COMP-3.
           05  WS-EV-WHT               PIC S9(15)V99    COMP-3.
           05  WS-EV-NET               PIC S9(15)V99    COMP-3.
           05  WS-EV-CIL               PIC S9(15)V99    COMP-3.
           05  WS-EV-WHOLE             PIC S9(13)V9(04) COMP-3.
           05  WS-EV-NEW-EXACT         PIC S9(13)V9(06) COMP-3.
           05  WS-EV-SKIPPED           PIC S9(07)       COMP-3.
      *
      *----------------------------------------------------------------*
      * RUN COUNTERS                                                   *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-ELIG-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ELIG-SKIPPED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-WRITTEN         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EM-ADDED             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EM-REPLACED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EM-KEPT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EVENTS-DONE          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-SKIPPED       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-WHT-CALLS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WHT-DEFAULTED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FIRM-EXEMPT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-NOT-FOUND       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ELIG-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
           05  WS-NET-CASH-HASH        PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-SHARES-HASH          PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-QTY                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
       01  WS-DISP-RATE                PIC 9.9999.
      *
           COPY CAENTL.
           COPY CMDATEW.
           COPY CMSECMS.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMSECLNK.
           COPY CMWHLNK.
           COPY CMJILNK.
      *
       01  FILLER                      PIC X(32)
           VALUE 'CAB300 WORKING STORAGE ENDS'.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
      *
           PERFORM 2000-PROCESS-ELIG THRU 2000-EXIT
               UNTIL WS-ELIG-EOF.
      *
           IF WS-EVENT-ACTIVE
               PERFORM 3500-END-EVENT THRU 3500-EXIT.
      *
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
      *
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           INITIALIZE AB-ABEND-PARMS.
           CALL 'CMASM01' USING JI-JOB-INFO.
      *
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON DATE CARD' TO AB-MESSAGE
               GO TO 9999-ABEND.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DC-VALID-CARD
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1005                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE DATECARD-FILE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID    TO AU-PROGRAM.
           MOVE 'START'          TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE DC-BUS-DATE      TO AU-BUS-DATE.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA ENTITLEMENT CALCULATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           PERFORM 1100-READ-PARMCARD THRU 1100-EXIT.
      *
           OPEN INPUT ELIGIN-FILE.
           IF WS-ELIGIN-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ELIGIN-STATUS    TO AB-FILE-STATUS
               MOVE 'ELIGIN'            TO AB-DDNAME
               MOVE 'OPEN FAILED ON ELIGIBILITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN I-O CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ACCTMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ACCTMAST'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON ACCOUNT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN OUTPUT ENTLOUT-FILE.
           IF WS-ENTLOUT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ENTLOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ENTLOUT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON ENTITLEMENT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN I-O ENTLMAST-FILE.
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           PERFORM 8000-READ-ELIG THRU 8000-EXIT.
           IF WS-ELIG-EOF
               DISPLAY 'CAB300 - ELIGIBILITY FILE EMPTY - NO EVENTS'.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       1100-READ-PARMCARD.
      *----------------------------------------------------------------*
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'CAB300 - NO CONTROL CARD - DEFAULTS USED'
               GO TO 1100-EXIT.
       1100-READ-NEXT.
           READ PARMCARD INTO WS-PARM-CARD
               AT END
                   MOVE 'Y' TO WS-PARM-EOF-SW.
           IF WS-PARM-EOF
               GO TO 1100-CLOSE.
           IF WS-PARMCARD-STATUS NOT = '00'
               MOVE '1100-READ-PARMCARD' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-PARMCARD-STATUS  TO AB-FILE-STATUS
               MOVE 'SYSIN'             TO AB-DDNAME
               MOVE 'READ FAILED ON CONTROL CARD' TO AB-MESSAGE
               GO TO 9999-ABEND.
           IF PARMCARD-REC(1:1) = '*'
           OR PC-PROGRAM NOT = 'CAB300'
               GO TO 1100-READ-NEXT.
      *    RATE IS PUNCHED WITH A DECIMAL POINT - 0.3000
           IF PC-DFLTWHT-KW = 'DFLTWHT='
               IF PC-DFLTWHT-X(2:1) = '.'
               AND PC-DFLTWHT-X(1:1) IS NUMERIC
               AND PC-DFLTWHT-X(3:3) IS NUMERIC
                   COMPUTE WS-DEFAULT-FGN-RATE =
                       FUNCTION NUMVAL(PC-DFLTWHT-X)
               ELSE
                   DISPLAY 'CAB300 - DFLTWHT INVALID - DEFAULT KEPT'.
           IF PC-WHTDATE-KW = 'WHTDATE='
               IF PC-WHTDATE = 'P' OR 'R'
                   MOVE PC-WHTDATE TO WS-WHT-DATE-OPT.
           GO TO 1100-READ-NEXT.
       1100-CLOSE.
           CLOSE PARMCARD.
           MOVE WS-DEFAULT-FGN-RATE TO WS-DISP-RATE.
           DISPLAY 'CAB300 - DEFAULT FOREIGN WHT RATE ' WS-DISP-RATE
                   ' - RATE DATE OPTION ' WS-WHT-DATE-OPT.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
       2000-PROCESS-ELIG.
      *================================================================*
           IF ELG-EVENT-ID NOT = WS-CURR-EVENT
               IF WS-EVENT-ACTIVE
                   PERFORM 3500-END-EVENT THRU 3500-EXIT
                   PERFORM 3000-START-EVENT THRU 3000-EXIT
               ELSE
                   PERFORM 3000-START-EVENT THRU 3000-EXIT.
      *
           IF WS-EVENT-SKIP
               ADD 1 TO WS-ELIG-SKIPPED
               ADD 1 TO WS-EV-SKIPPED
               GO TO 2000-READ.
      *
           PERFORM 4000-PREPARE-HOLDER THRU 4000-EXIT.
      *
           EVALUATE TRUE
               WHEN CAE-CASH-DIV
                   PERFORM 5100-CALC-CASH-DIV THRU 5100-EXIT
               WHEN CAE-STOCK-DIV
                   PERFORM 5200-CALC-STOCK-DIV THRU 5200-EXIT
               WHEN CAE-FWD-SPLIT
                   PERFORM 5300-CALC-FWD-SPLIT THRU 5300-EXIT
               WHEN CAE-REV-SPLIT
                   PERFORM 5400-CALC-REV-SPLIT THRU 5400-EXIT
               WHEN CAE-CASH-MERGER
                   PERFORM 5500-CALC-MERGER THRU 5500-EXIT
               WHEN OTHER
                   MOVE '2000-PROCESS-ELIG' TO AB-PARAGRAPH
                   MOVE 1008                TO AB-ABEND-CODE
                   MOVE SPACES              TO AB-FILE-STATUS
                   MOVE 'CAEVENT'           TO AB-DDNAME
                   MOVE CAE-EVENT-ID        TO AB-KEY
                   MOVE 'UNSUPPORTED EVENT TYPE ON EVENT MASTER'
                                            TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
      *
           PERFORM 6000-BUILD-ENTITLEMENT THRU 6000-EXIT.
           PERFORM 6100-WRITE-ENTL-FILE THRU 6100-EXIT.
           PERFORM 6200-WRITE-ENTL-MASTER THRU 6200-EXIT.
           PERFORM 6300-ACCUMULATE THRU 6300-EXIT.
       2000-READ.
           PERFORM 8000-READ-ELIG THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * START OF A NEW EVENT ON THE ELIGIBILITY FILE                   *
      *================================================================*
       3000-START-EVENT.
           MOVE ELG-EVENT-ID TO WS-CURR-EVENT.
           MOVE 'Y' TO WS-EVENT-ACTIVE-SW.
           MOVE 'N' TO WS-EVENT-SKIP-SW.
           INITIALIZE WS-EVENT-TOTALS.
           MOVE ZERO TO WS-WC-COUNT.
      *
           MOVE ELG-EVENT-ID TO CAE-EVENT-ID.
           READ CAEVENT-FILE.
           IF CAEVENT-NOTFND
               DISPLAY 'CAB300 - EVENT ' ELG-EVENT-ID
                       ' NOT ON EVENT MASTER - SKIPPED'
               MOVE 'Y' TO WS-EVENT-SKIP-SW
               GO TO 3000-EXIT.
           IF NOT CAEVENT-OK
               MOVE '3000-START-EVENT'  TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE ELG-EVENT-ID        TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           IF CAE-ELIGIBLE-TAKEN
               NEXT SENTENCE
           ELSE
               IF CAE-ENTITLED AND DC-RERUN
                   DISPLAY 'CAB300 - RERUN - RECALCULATING '
                           CAE-EVENT-ID
               ELSE
                   DISPLAY 'CAB300 - EVENT ' CAE-EVENT-ID ' STATUS '
                           CAE-STATUS ' - NOT CALCULATED'
                   MOVE 'Y' TO WS-EVENT-SKIP-SW
                   GO TO 3000-EXIT.
      *
      *    ISSUER COUNTRY FOR WITHHOLDING
           MOVE 'GET '         TO SL-FUNCTION.
           MOVE CAE-CUSIP      TO SL-KEY-CUSIP.
           MOVE SPACES         TO SL-KEY-ISIN
                                  SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           IF SL-DB-ERROR
               MOVE '3000-START-EVENT'  TO AB-PARAGRAPH
               MOVE 1003                TO AB-ABEND-CODE
               MOVE SL-SQLCODE          TO AB-SQLCODE
               MOVE 'CMD010'            TO AB-DDNAME
               MOVE CAE-CUSIP           TO AB-KEY
               MOVE 'DB2 ERROR READING SECURITY MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           IF SL-FOUND
               MOVE SL-SEC-DATA TO SEC-MASTER-REC
               MOVE SEC-COUNTRY TO WS-ISSUER-COUNTRY
           ELSE
               DISPLAY 'CAB300 - SECURITY ' CAE-CUSIP
                       ' NOT FOUND - ISSUER COUNTRY US ASSUMED'
               MOVE 'US' TO WS-ISSUER-COUNTRY
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE.
           IF WS-ISSUER-COUNTRY = SPACES
               MOVE 'US' TO WS-ISSUER-COUNTRY.
      *
           DISPLAY 'CAB300 - CALCULATING ' CAE-EVENT-ID ' '
                   CAE-EVENT-TYPE ' CUSIP ' CAE-CUSIP
                   ' ISSUER ' WS-ISSUER-COUNTRY.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * END OF EVENT - POST TOTALS TO EVENT MASTER                     *
      *================================================================*
       3500-END-EVENT.
           MOVE 'N' TO WS-EVENT-ACTIVE-SW.
           IF WS-EVENT-SKIP
               ADD 1 TO WS-EVENTS-SKIPPED
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               MOVE WS-EV-SKIPPED TO WS-DISP-COUNT
               DISPLAY 'CAB300 - ' WS-CURR-EVENT
                       ' ELIGIBILITY RECORDS SKIPPED: ' WS-DISP-COUNT
               GO TO 3500-EXIT.
      *
           MOVE WS-CURR-EVENT TO CAE-EVENT-ID.
           READ CAEVENT-FILE.
           IF NOT CAEVENT-OK
               MOVE '3500-END-EVENT'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE WS-CURR-EVENT       TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           MOVE 'EN'                TO CAE-STATUS.
           COMPUTE CAE-ENTL-CASH-TOTAL = WS-EV-GROSS + WS-EV-CIL.
           MOVE WS-EV-WHOLE         TO CAE-ENTL-SHARE-TOTAL.
           MOVE DC-BUS-DATE         TO CAE-LAST-UPD-DATE.
           MOVE JI-JOBNAME          TO CAE-LAST-UPD-JOB.
           REWRITE CAE-EVENT-REC.
           IF NOT CAEVENT-OK
               MOVE '3500-END-EVENT'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE WS-CURR-EVENT       TO AB-KEY
               MOVE 'REWRITE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-DONE.
      *
           MOVE WS-EV-COUNT   TO WS-DISP-COUNT.
           DISPLAY '         ENTITLEMENTS           : ' WS-DISP-COUNT.
           MOVE WS-EV-ELIG-QTY TO WS-DISP-QTY.
           DISPLAY '         ELIGIBLE QUANTITY      : ' WS-DISP-QTY.
           MOVE WS-EV-GROSS   TO WS-DISP-AMT.
           DISPLAY '         GROSS CASH             : ' WS-DISP-AMT.
           MOVE WS-EV-WHT     TO WS-DISP-AMT.
           DISPLAY '         WITHHOLDING            : ' WS-DISP-AMT.
           MOVE WS-EV-NET     TO WS-DISP-AMT.
           DISPLAY '         NET CASH               : ' WS-DISP-AMT.
           MOVE WS-EV-CIL     TO WS-DISP-AMT.
           DISPLAY '         CASH IN LIEU           : ' WS-DISP-AMT.
           MOVE WS-EV-WHOLE   TO WS-DISP-QTY.
           DISPLAY '         WHOLE SHARES           : ' WS-DISP-QTY.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE 'ENTITLED'       TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE WS-CURR-EVENT    TO AU-KEY.
           MOVE 'EVENT ENTITLEMENTS CALCULATED - STATUS EN'
                                 TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       3500-EXIT.
           EXIT.
      *
      *================================================================*
      * HOLDER DATA - TAX STATUS AND COUNTRY                           *
      *================================================================*
       4000-PREPARE-HOLDER.
           IF ELG-ENTITLED-QTY NUMERIC
               MOVE ELG-ENTITLED-QTY TO WS-ELIG-QTY
           ELSE
               MOVE ZERO TO WS-ELIG-QTY.
           MOVE ELG-TAX-STATUS  TO WS-TAX-STATUS.
           MOVE ELG-TAX-COUNTRY TO WS-TAX-COUNTRY.
      *
      *    SNAPSHOT TAKEN BEFORE W-8BEN WAS ON FILE - USE ACCOUNT
           IF WS-TAX-STATUS = SPACE
           OR WS-TAX-COUNTRY = SPACES
               MOVE ELG-ACCT-NO TO ACCT-NO
               READ ACCTMAST-FILE
               IF ACCTMAST-OK
                   MOVE ACCT-TAX-STATUS  TO WS-TAX-STATUS
                   MOVE ACCT-TAX-COUNTRY TO WS-TAX-COUNTRY
               ELSE
                   IF ACCTMAST-NOTFND
                       ADD 1 TO WS-ACCT-NOT-FOUND
                   ELSE
                       MOVE '4000-PREPARE-HOLDER' TO AB-PARAGRAPH
                       MOVE 1002                TO AB-ABEND-CODE
                       MOVE WS-ACCTMAST-STATUS  TO AB-FILE-STATUS
                       MOVE 'ACCTMAST'          TO AB-DDNAME
                       MOVE ELG-ACCT-NO         TO AB-KEY
                       MOVE 'READ FAILED ON ACCOUNT MASTER'
                                                TO AB-MESSAGE
                       GO TO 9999-ABEND.
           IF WS-TAX-STATUS = SPACE
               MOVE 'D' TO WS-TAX-STATUS.
           IF WS-TAX-COUNTRY = SPACES
               MOVE 'US' TO WS-TAX-COUNTRY.
      *
           MOVE ZERO TO WS-GROSS-CASH
                        WS-WHT-RATE
                        WS-WHT-AMOUNT
                        WS-NET-CASH
                        WS-NEW-SHARES-X
                        WS-WHOLE-SHARES
                        WS-FRAC-SHARES
                        WS-CIL-AMOUNT.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
      * CASH DIVIDEND                                                  *
      *================================================================*
       5100-CALC-CASH-DIV.
      *    GROSS IS TRUNCATED TO THE CENT - SEE CHG03390
           COMPUTE WS-GROSS-CASH = WS-ELIG-QTY * CAE-RATE.
      *
           IF CAE-TAXABLE-FLAG = 'N'
               MOVE ZERO TO WS-WHT-RATE
           ELSE
               PERFORM 5600-GET-WHT-RATE THRU 5600-EXIT.
      *
           COMPUTE WS-WHT-AMOUNT ROUNDED = WS-GROSS-CASH * WS-WHT-RATE.
           COMPUTE WS-NET-CASH = WS-GROSS-CASH - WS-WHT-AMOUNT.
       5100-EXIT.
           EXIT.
      *
      *================================================================*
      * STOCK DIVIDEND - NEW SHARES = QTY X NEW / OLD                  *
      *================================================================*
       5200-CALC-STOCK-DIV.
           COMPUTE WS-NEW-SHARES-X =
               WS-ELIG-QTY * CAE-RATIO-NEW / CAE-RATIO-OLD.
           PERFORM 5700-APPLY-FRACTIONS THRU 5700-EXIT.
       5200-EXIT.
           EXIT.
      *
      *================================================================*
      * FORWARD SPLIT - ADDITIONAL SHARES = QTY X NEW / OLD - QTY      *
      *================================================================*
       5300-CALC-FWD-SPLIT.
           COMPUTE WS-NEW-SHARES-X =
               WS-ELIG-QTY * CAE-RATIO-NEW / CAE-RATIO-OLD
               - WS-ELIG-QTY.
           PERFORM 5700-APPLY-FRACTIONS THRU 5700-EXIT.
       5300-EXIT.
           EXIT.
      *
      *================================================================*
      * REVERSE SPLIT - NEW POSITION = QTY X NEW / OLD                 *
      * FRACTIONS ALWAYS CASH IN LIEU                                  *
      *================================================================*
       5400-CALC-REV-SPLIT.
           COMPUTE WS-NEW-SHARES-X =
               WS-ELIG-QTY * CAE-RATIO-NEW / CAE-RATIO-OLD.
           MOVE WS-NEW-SHARES-X TO WS-WHOLE-SHARES.
           COMPUTE WS-FRAC-SHARES = WS-NEW-SHARES-X - WS-WHOLE-SHARES.
           IF WS-FRAC-SHARES > ZERO
               COMPUTE WS-CIL-AMOUNT ROUNDED =
                   WS-FRAC-SHARES * CAE-CIL-PRICE.
       5400-EXIT.
           EXIT.
      *
      *================================================================*
      * CASH MERGER - CASH = QTY X MERGER RATE, ROUNDED                *
      *================================================================*
       5500-CALC-MERGER.
           COMPUTE WS-GROSS-CASH ROUNDED =
               WS-ELIG-QTY * CAE-MRG-CASH-RATE.
           MOVE ZERO          TO WS-WHT-RATE
                                 WS-WHT-AMOUNT.
           MOVE WS-GROSS-CASH TO WS-NET-CASH.
       5500-EXIT.
           EXIT.
      *
      *================================================================*
      * WITHHOLDING RATE                                               *
      *   FIRM ACCOUNTS (ALPHA ACCOUNT NUMBERS) ARE EXEMPT             *
      *   TAX STATUS E (EXEMPT) - NO WITHHOLDING                       *
      *   OTHERWISE WHT_RATE VIA CMD040, CACHED BY COUNTRY + STATUS    *
      *================================================================*
       5600-GET-WHT-RATE.
           IF ELG-ACCT-NO(1:1) < '0'
               MOVE ZERO TO WS-WHT-RATE
               ADD 1 TO WS-FIRM-EXEMPT
               GO TO 5600-EXIT.
      *
           IF WS-TAX-STATUS = 'E'
               MOVE ZERO TO WS-WHT-RATE
               GO TO 5600-EXIT.
      *
           MOVE 'N' TO WS-WHT-CACHE-SW.
           PERFORM 5610-SEARCH-CACHE THRU 5610-EXIT
               VARYING WS-WC-SUB FROM 1 BY 1
               UNTIL WS-WC-SUB > WS-WC-COUNT
                  OR WS-WHT-IN-CACHE.
           IF WS-WHT-IN-CACHE
               GO TO 5600-EXIT.
      *
           MOVE WS-TAX-COUNTRY     TO WH-HOLDER-COUNTRY.
           MOVE WS-ISSUER-COUNTRY  TO WH-ISSUER-COUNTRY.
           MOVE 'DV'               TO WH-INCOME-TYPE.
           MOVE WS-TAX-STATUS      TO WH-TAX-STATUS.
           IF WS-WHT-ON-RECORD-DATE
               MOVE CAE-RECORD-DATE TO WH-EFF-DATE
           ELSE
               MOVE CAE-PAY-DATE    TO WH-EFF-DATE.
           MOVE ZERO               TO WH-RATE.
           ADD 1 TO WS-WHT-CALLS.
           CALL 'CMD040' USING WH-WHTAX-PARMS.
      *
           IF WH-DB-ERROR
               MOVE '5600-GET-WHT-RATE' TO AB-PARAGRAPH
               MOVE 1003                TO AB-ABEND-CODE
               MOVE WH-SQLCODE          TO AB-SQLCODE
               MOVE 'CMD040'            TO AB-DDNAME
               STRING WS-TAX-COUNTRY WS-ISSUER-COUNTRY 'DV'
                      WS-TAX-STATUS DELIMITED BY SIZE INTO AB-KEY
               MOVE 'DB2 ERROR READING WHT_RATE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           IF WH-FOUND
               MOVE WH-RATE TO WS-WHT-RATE
           ELSE
               ADD 1 TO WS-WHT-DEFAULTED
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               IF WS-TAX-STATUS = 'F'
                   MOVE WS-DEFAULT-FGN-RATE TO WS-WHT-RATE
               ELSE
                   MOVE ZERO TO WS-WHT-RATE
               END-IF
               DISPLAY 'CAB300 - NO WHT_RATE ROW HOLDER '
                       WS-TAX-COUNTRY ' ISSUER ' WS-ISSUER-COUNTRY
                       ' STATUS ' WS-TAX-STATUS ' - DEFAULT APPLIED'
               MOVE 'WRIT'           TO AU-FUNCTION
               MOVE 'WHTDFLT'        TO AU-EVENT
               MOVE 'W'              TO AU-SEVERITY
               MOVE ELG-EVENT-ID     TO AU-KEY
               STRING 'NO WHT RATE FOR HOLDER ' WS-TAX-COUNTRY
                      ' STATUS ' WS-TAX-STATUS ' - DEFAULT USED'
                      DELIMITED BY SIZE INTO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           IF WS-WC-COUNT < WS-WC-MAX
               ADD 1 TO WS-WC-COUNT
               MOVE WS-TAX-COUNTRY TO WS-WC-COUNTRY (WS-WC-COUNT)
               MOVE WS-TAX-STATUS  TO WS-WC-STATUS (WS-WC-COUNT)
               MOVE WS-WHT-RATE    TO WS-WC-RATE (WS-WC-COUNT)
               IF WH-FOUND
                   MOVE 'N' TO WS-WC-DEFAULTED (WS-WC-COUNT)
               ELSE
                   MOVE 'Y' TO WS-WC-DEFAULTED (WS-WC-COUNT).
       5600-EXIT.
           EXIT.
      *
       5610-SEARCH-CACHE.
           IF WS-WC-COUNTRY (WS-WC-SUB) = WS-TAX-COUNTRY
           AND WS-WC-STATUS (WS-WC-SUB) = WS-TAX-STATUS
               MOVE 'Y' TO WS-WHT-CACHE-SW
               MOVE WS-WC-RATE (WS-WC-SUB) TO WS-WHT-RATE.
       5610-EXIT.
           EXIT.
      *
      *================================================================*
      * FRACTIONAL SHARES - SDV AND SPL                                *
      *================================================================*
       5700-APPLY-FRACTIONS.
           MOVE WS-NEW-SHARES-X TO WS-WHOLE-SHARES.
           COMPUTE WS-FRAC-SHARES = WS-NEW-SHARES-X - WS-WHOLE-SHARES.
           IF WS-FRAC-SHARES = ZERO
               GO TO 5700-EXIT.
           EVALUATE TRUE
               WHEN CAE-FRAC-CASH-IN-LIEU
                   COMPUTE WS-CIL-AMOUNT ROUNDED =
                       WS-FRAC-SHARES * CAE-CIL-PRICE
               WHEN CAE-FRAC-ROUND-UP
                   ADD 1 TO WS-WHOLE-SHARES
               WHEN CAE-FRAC-ROUND-DOWN
                   CONTINUE
               WHEN OTHER
      *            NO METHOD ON FILE - ROUND DOWN (PRE-1995 EVENTS)
                   CONTINUE
           END-EVALUATE.
       5700-EXIT.
           EXIT.
      *
      *================================================================*
      * BUILD THE ENTITLEMENT RECORD                                   *
      *================================================================*
       6000-BUILD-ENTITLEMENT.
           MOVE SPACES              TO ENT-ENTITLEMENT-REC.
           MOVE ELG-EVENT-ID        TO ENT-EVENT-ID.
           MOVE ELG-ACCT-NO         TO ENT-ACCT-NO.
           MOVE ELG-LOCATION        TO ENT-LOCATION.
           MOVE CAE-CUSIP           TO ENT-CUSIP.
           MOVE CAE-EVENT-TYPE      TO ENT-EVENT-TYPE.
           MOVE ELG-ACCT-TYPE       TO ENT-ACCT-TYPE.
           MOVE WS-TAX-STATUS       TO ENT-TAX-STATUS.
           MOVE WS-TAX-COUNTRY      TO ENT-TAX-COUNTRY.
           MOVE WS-ELIG-QTY         TO ENT-ELIGIBLE-QTY.
           MOVE WS-GROSS-CASH       TO ENT-GROSS-CASH.
           MOVE WS-WHT-RATE         TO ENT-WHT-RATE.
           MOVE WS-WHT-AMOUNT       TO ENT-WHT-AMOUNT.
           MOVE WS-NET-CASH         TO ENT-NET-CASH.
           MOVE CAE-CCY             TO ENT-CCY.
           MOVE WS-NEW-SHARES-X     TO ENT-NEW-SHARES.
           MOVE WS-WHOLE-SHARES     TO ENT-WHOLE-SHARES.
           MOVE WS-FRAC-SHARES      TO ENT-FRAC-SHARES.
           MOVE WS-CIL-AMOUNT       TO ENT-CIL-AMOUNT.
           IF CAE-STOCK-EVENT
               MOVE CAE-NEW-CUSIP   TO ENT-NEW-CUSIP
           ELSE
               MOVE SPACES          TO ENT-NEW-CUSIP.
           IF ELG-DUE-BILL-QTY NUMERIC
           AND ELG-DUE-BILL-QTY NOT = ZERO
               MOVE 'Y'             TO ENT-DUE-BILL-FLAG
           ELSE
               MOVE 'N'             TO ENT-DUE-BILL-FLAG.
           MOVE 'CA'                TO ENT-STATUS.
           MOVE CAE-PAY-DATE        TO ENT-PAY-DATE.
           MOVE DC-BUS-DATE         TO ENT-CALC-DATE.
           MOVE ZERO                TO ENT-PAID-DATE.
       6000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       6100-WRITE-ENTL-FILE.
      *----------------------------------------------------------------*
           WRITE ENTLOUT-REC FROM ENT-ENTITLEMENT-REC.
           IF WS-ENTLOUT-STATUS NOT = '00'
               MOVE '6100-WRITE-ENTL-FILE' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ENTLOUT'           TO AB-DDNAME
               MOVE ENT-KEY             TO AB-KEY
               MOVE 'WRITE FAILED ON ENTITLEMENT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-ENTL-WRITTEN.
       6100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * ENTITLEMENT MASTER.  ON A RERUN THE ROW ALREADY EXISTS -       *
      * REPLACE IT ONLY WHILE IT IS STILL CALCULATED (CA).             *
      *----------------------------------------------------------------*
       6200-WRITE-ENTL-MASTER.
           MOVE ENT-ENTITLEMENT-REC TO EM-MASTER-REC.
           WRITE EM-MASTER-REC.
           IF ENTLMAST-OK
               ADD 1 TO WS-EM-ADDED
               GO TO 6200-EXIT.
           IF NOT ENTLMAST-DUPKEY
               MOVE '6200-WRITE-ENTL-MASTER' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE ENT-KEY             TO AB-KEY
               MOVE 'WRITE FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           MOVE ENT-KEY TO EM-KEY.
           READ ENTLMAST-FILE.
           IF NOT ENTLMAST-OK
               MOVE '6200-WRITE-ENTL-MASTER' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE ENT-KEY             TO AB-KEY
               MOVE 'READ FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *    RECORD OFFSET 116 = ENT-STATUS
           IF EM-MASTER-REC(117:2) NOT = 'CA'
               ADD 1 TO WS-EM-KEPT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               DISPLAY 'CAB300 - ENTITLEMENT ' ENT-KEY ' STATUS '
                       EM-MASTER-REC(117:2) ' NOT REPLACED'
               GO TO 6200-EXIT.
           MOVE ENT-ENTITLEMENT-REC TO EM-MASTER-REC.
           REWRITE EM-MASTER-REC.
           IF NOT ENTLMAST-OK
               MOVE '6200-WRITE-ENTL-MASTER' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE ENT-KEY             TO AB-KEY
               MOVE 'REWRITE FAILED ON ENTITLEMENT MASTER'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EM-REPLACED.
       6200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       6300-ACCUMULATE.
      *----------------------------------------------------------------*
           ADD 1                TO WS-EV-COUNT.
           ADD WS-ELIG-QTY      TO WS-EV-ELIG-QTY.
           ADD WS-GROSS-CASH    TO WS-EV-GROSS.
           ADD WS-WHT-AMOUNT    TO WS-EV-WHT.
           ADD WS-NET-CASH      TO WS-EV-NET.
           ADD WS-CIL-AMOUNT    TO WS-EV-CIL.
           ADD WS-WHOLE-SHARES  TO WS-EV-WHOLE.
           ADD WS-NEW-SHARES-X  TO WS-EV-NEW-EXACT.
      *
           ADD WS-NET-CASH      TO WS-NET-CASH-HASH.
           ADD WS-CIL-AMOUNT    TO WS-NET-CASH-HASH.
           ADD WS-WHOLE-SHARES  TO WS-SHARES-HASH.
       6300-EXIT.
           EXIT.
      *
      *================================================================*
       8000-READ-ELIG.
      *================================================================*
           READ ELIGIN-FILE
               AT END
                   MOVE 'Y' TO WS-ELIG-EOF-SW
                   GO TO 8000-EXIT.
           IF WS-ELIGIN-STATUS NOT = '00'
               MOVE '8000-READ-ELIG'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ELIGIN-STATUS    TO AB-FILE-STATUS
               MOVE 'ELIGIN'            TO AB-DDNAME
               MOVE 'READ FAILED ON ELIGIBILITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-ELIG-READ.
           IF ELG-ENTITLED-QTY NUMERIC
               ADD ELG-ENTITLED-QTY TO WS-ELIG-QTY-HASH.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE ELIGIN-FILE.
           IF WS-ELIGIN-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ELIGIN-STATUS    TO AB-FILE-STATUS
               MOVE 'ELIGIN'            TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ELIGIBILITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ACCTMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ACCTMAST'          TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ACCOUNT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE ENTLOUT-FILE.
           IF WS-ENTLOUT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ENTLOUT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ENTITLEMENT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE ENTLMAST-FILE.
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           MOVE 'ELIG-IN'          TO CT-COUNTER-NAME.
           MOVE WS-ELIG-READ       TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT.
           MOVE WS-ELIG-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'ENTL-OUT'         TO CT-COUNTER-NAME.
           MOVE WS-ENTL-WRITTEN    TO CT-COUNT.
           MOVE WS-NET-CASH-HASH   TO CT-AMOUNT.
           MOVE WS-SHARES-HASH     TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'EVENTS-ENTL'      TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-DONE     TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT
                                      CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'WHT-DEFAULT'      TO CT-COUNTER-NAME.
           MOVE WS-WHT-DEFAULTED   TO CT-COUNT.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'CLOS'             TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *
           DISPLAY '*************************************************'.
           DISPLAY '* CAB300  CA ENTITLEMENTS       - RUN STATISTICS *'.
           DISPLAY '*************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-ELIG-READ        TO WS-DISP-COUNT.
           DISPLAY ' ELIGIBILITY RECS READ    : ' WS-DISP-COUNT.
           MOVE WS-ELIG-SKIPPED     TO WS-DISP-COUNT.
           DISPLAY ' ELIGIBILITY RECS SKIPPED : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-DONE      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS ENTITLED          : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-SKIPPED   TO WS-DISP-COUNT.
           DISPLAY ' EVENTS SKIPPED           : ' WS-DISP-COUNT.
           MOVE WS-ENTL-WRITTEN     TO WS-DISP-COUNT.
           DISPLAY ' ENTITLEMENTS WRITTEN     : ' WS-DISP-COUNT.
           MOVE WS-EM-ADDED         TO WS-DISP-COUNT.
           DISPLAY ' ENTLMAST ADDED           : ' WS-DISP-COUNT.
           MOVE WS-EM-REPLACED      TO WS-DISP-COUNT.
           DISPLAY ' ENTLMAST REPLACED        : ' WS-DISP-COUNT.
           MOVE WS-EM-KEPT          TO WS-DISP-COUNT.
           DISPLAY ' ENTLMAST NOT REPLACED    : ' WS-DISP-COUNT.
           MOVE WS-WHT-CALLS        TO WS-DISP-COUNT.
           DISPLAY ' WHT_RATE LOOKUPS         : ' WS-DISP-COUNT.
           MOVE WS-WHT-DEFAULTED    TO WS-DISP-COUNT.
           DISPLAY ' WHT RATE DEFAULTED       : ' WS-DISP-COUNT.
           MOVE WS-FIRM-EXEMPT      TO WS-DISP-COUNT.
           DISPLAY ' FIRM ACCOUNTS EXEMPT     : ' WS-DISP-COUNT.
           MOVE WS-ACCT-NOT-FOUND   TO WS-DISP-COUNT.
           DISPLAY ' ACCOUNTS NOT FOUND       : ' WS-DISP-COUNT.
           MOVE WS-NET-CASH-HASH    TO WS-DISP-AMT.
           DISPLAY ' NET CASH + CIL           : ' WS-DISP-AMT.
           MOVE WS-SHARES-HASH      TO WS-DISP-QTY.
           DISPLAY ' WHOLE SHARES             : ' WS-DISP-QTY.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID    TO AU-PROGRAM.
           MOVE 'END'            TO AU-EVENT.
           IF WS-RETURN-CODE > 0
               MOVE 'W'          TO AU-SEVERITY
           ELSE
               MOVE 'I'          TO AU-SEVERITY.
           MOVE DC-BUS-DATE      TO AU-BUS-DATE.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA ENTITLEMENT CALCULATION ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'           TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
       9100-POST-TOTAL.
           MOVE 'POST'           TO CT-FUNCTION.
           MOVE DC-BUS-DATE      TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID    TO CT-PROGRAM.
           MOVE 'CAB300'         TO CT-STAGE.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE '9100-POST-TOTAL'   TO AB-PARAGRAPH
               MOVE 1010                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'CTLTOTS'           TO AB-DDNAME
               MOVE CT-COUNTER-NAME     TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND.
       9100-EXIT.
           EXIT.
      *
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'CAB300 - ABENDING: ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
