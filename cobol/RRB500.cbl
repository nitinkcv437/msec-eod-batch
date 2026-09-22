       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRB500.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  04/19/1993.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRB500                                            *
      * TITLE      : NET CAPITAL - SECURITIES HAIRCUTS ON FIRM         *
      *              INVENTORY                                         *
      * JOB        : MSRRD040   STEP010 (IKJEFT01 - DB2 PLAN MSRRPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   COMPUTES THE HAIRCUT (PERCENTAGE DEDUCTION FROM TENTATIVE    *
      *   NET CAPITAL) ON EVERY FIRM INVENTORY POSITION:               *
      *     EQUITY / PREFERRED / ADR          15 PCT                   *
      *     MUTUAL FUNDS (REDEEMABLE)          9 PCT                   *
      *     GOVERNMENTS - BY TIME TO MATURITY  0 - 6 PCT  (12 BANDS)   *
      *     CORPORATE BONDS                    2 - 9 PCT  ( 5 BANDS)   *
      *     MUNICIPALS                         1 - 7 PCT  ( 4 BANDS)   *
      *     ANYTHING ELSE (NO MARKET)        100 PCT                   *
      *   SHORT POSITIONS TAKE THE SAME PERCENTAGE ON THE ABSOLUTE     *
      *   MARKET VALUE.                                                *
      *   UNDUE CONCENTRATION: WHEN THE LONG MARKET VALUE OF ONE       *
      *   ISSUER EXCEEDS 10 PCT OF TENTATIVE NET CAPITAL (SYSIN TNC=)  *
      *   AN ADDITIONAL 50 PCT OF THE BASE HAIRCUT IS TAKEN ON THE     *
      *   EXCESS.  GOVERNMENTS ARE NOT SUBJECT TO CONCENTRATION.       *
      *                                                                *
      *   PASS 1 LOADS THE FIRM INVENTORY POSITIONS WITH THEIR BASE    *
      *   HAIRCUT AND BUILDS THE ISSUER TOTALS; PASS 2 ADDS THE UNDUE  *
      *   CONCENTRATION AND WRITES ONE RRHCUT RECORD PER POSITION.     *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     TNC=NNNNNNNNNNNNN.NN (RRP040A)              *
      *                    CONC-EXEMPT=<ISSUER6>  NOT SUBJECT TO UNDUE *
      *                                CONCENTRATION (MAX 20 CARDS)    *
      *                    TRACE=Y     DISPLAY EVERY POSITION          *
      *          POSMAST   MSEC.PROD.SR.POSITION.KSDS    (SRPOSN)      *
      * OUTPUT : HCUTOUT   MSEC.PROD.RR.HAIRCUT(+1)      (RRHCUT)      *
      * CALLS  : CMD010 (TYPE, ISSUER, MATURITY)  CMU010 (DIFC)        *
      *          CMU050 CMU060 CMU080                                  *
      *                                                                *
      * RETURN CODES: 0 CLEAN                                          *
      *               4 NO TNC CARD (NO CONCENTRATION CHARGE),         *
      *                 POSITION NOT VALUED, SECURITY NOT ON FILE,     *
      *                 MATURED BOND IN INVENTORY, PRICE OLDER THAN    *
      *                 THE PREVIOUS BUSINESS DAY, BAD CONTROL CARD    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1993-04-19 RJK  CHG01512  ORIGINAL - HAIRCUT SCHEDULE          *
      * 1996-04-22 DWB  CHG02215  SECURITY MASTER NOW DB2 (CMD010)     *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD                 *
      * 2004-03-15 KAP  CHG12011  MUNICIPAL BANDS                      *
      * 2009-12-14 SPA  CHG19002  USD MARKET VALUE FROM POSITION       *
      * 2014-06-30 SPA  CHG27120  UNDUE CONCENTRATION (TNC CARD)       *
      * 2020-03-30 MHC  CHG34990  SHORTS ON ABSOLUTE MARKET VALUE      *
      * 2022-09-19 JLR  CHG37705  STALE PRICE CHECK, CONC-EXEMPT CARD, *
      *                           ACCOUNT SUMMARY                      *
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
           SELECT POSMAST-FILE  ASSIGN TO POSMAST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS SEQUENTIAL
                                RECORD KEY IS POSMAST-KEY
                                FILE STATUS IS WS-POSMAST-FS.
           SELECT HCUTOUT-FILE  ASSIGN TO HCUTOUT
                                FILE STATUS IS WS-HCUTOUT-FS.
      *
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
       FD  POSMAST-FILE.
       01  POSMAST-REC.
           05  POSMAST-KEY             PIC X(23).
           05  FILLER                  PIC X(177).
       FD  HCUTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  HCUTOUT-REC                 PIC X(150).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRB500'.
       01  WS-FILE-STATUS-AREA.
           05  WS-PARMCARD-FS          PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-DATECARD-FS          PIC X(02)  VALUE '00'.
           05  WS-POSMAST-FS           PIC X(02)  VALUE '00'.
               88  POSMAST-OK                     VALUE '00'.
               88  POSMAST-EOF                    VALUE '10'.
           05  WS-HCUTOUT-FS           PIC X(02)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                    VALUE 'Y'.
           05  WS-POSN-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-POSN-EOF                    VALUE 'Y'.
           05  WS-TNC-SW               PIC X(01)  VALUE 'N'.
               88  WS-TNC-GIVEN                   VALUE 'Y'.
           05  WS-TRACE-SW             PIC X(01)  VALUE 'N'.
               88  WS-TRACE-ON                    VALUE 'Y'.
           05  WS-EXEMPT-SW            PIC X(01)  VALUE 'N'.
               88  WS-ISSUER-EXEMPT               VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *
      *---- CONTROL CARD ---------------------------------------------*
       01  WS-PARM-KEYWORD             PIC X(20).
       01  WS-PARM-VALUE               PIC X(30).
       01  WS-PARM-VALUE-R REDEFINES WS-PARM-VALUE.
           05  WS-PV-DOLLARS           PIC X(13).
           05  WS-PV-POINT             PIC X(01).
           05  WS-PV-CENTS             PIC X(02).
           05  WS-PV-REST              PIC X(14).
       01  WS-PV-AMOUNT-X.
           05  WS-PVA-DOLLARS          PIC X(13).
           05  WS-PVA-CENTS            PIC X(02).
       01  WS-PV-AMOUNT REDEFINES WS-PV-AMOUNT-X
                                       PIC 9(13)V99.
       01  WS-TNC                      PIC S9(15)V99 COMP-3 VALUE ZERO.
       01  WS-PARM-ERRORS              PIC S9(05) COMP-3 VALUE ZERO.
      *---- ISSUERS EXEMPT FROM UNDUE CONCENTRATION (CHG37705) --------*
       01  WS-EXEMPT-TABLE.
           05  WS-EX-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-EX-ISSUER            PIC X(06)  OCCURS 20 TIMES
                                       INDEXED BY EX-IDX.
       01  WS-EX-MAX                   PIC S9(04) COMP  VALUE 20.
      *
      *---- HAIRCUT SCHEDULE (PERCENT) --------------------------------*
       01  WS-FLAT-RATES.
           05  WS-EQUITY-PCT           PIC S9(03)V9(04) COMP-3
                                                     VALUE +15.0000.
           05  WS-FUND-PCT             PIC S9(03)V9(04) COMP-3
                                                     VALUE +9.0000.
           05  WS-NO-MARKET-PCT        PIC S9(03)V9(04) COMP-3
                                                     VALUE +100.0000.
           05  WS-CONC-LIMIT-PCT       PIC S9(03)V9(04) COMP-3
                                                     VALUE +10.0000.
           05  WS-CONC-ADDON-PCT       PIC S9(03)V9(04) COMP-3
                                                     VALUE +50.0000.
      *    GOVERNMENTS: DAYS TO MATURITY LESS THAN / BAND / PERCENT
       01  WS-GOVT-VALUES.
           05  FILLER  PIC X(15)  VALUE '00091G03M000000'.
           05  FILLER  PIC X(15)  VALUE '00182G06M000050'.
           05  FILLER  PIC X(15)  VALUE '00273G09M000075'.
           05  FILLER  PIC X(15)  VALUE '00365G01Y000100'.
           05  FILLER  PIC X(15)  VALUE '00730G02Y000150'.
           05  FILLER  PIC X(15)  VALUE '01095G03Y000200'.
           05  FILLER  PIC X(15)  VALUE '01825G05Y000300'.
           05  FILLER  PIC X(15)  VALUE '03650G10Y000400'.
           05  FILLER  PIC X(15)  VALUE '05475G15Y000450'.
           05  FILLER  PIC X(15)  VALUE '07300G20Y000500'.
           05  FILLER  PIC X(15)  VALUE '09125G25Y000550'.
           05  FILLER  PIC X(15)  VALUE '99999G25+000600'.
       01  WS-GOVT-TABLE REDEFINES WS-GOVT-VALUES.
           05  WS-GOVT-ENTRY           OCCURS 12 TIMES.
               10  WS-GOVT-DAYS        PIC 9(05).
               10  WS-GOVT-BAND        PIC X(04).
               10  WS-GOVT-PCT         PIC 9(04)V99.
      *    CORPORATES
       01  WS-CORP-VALUES.
           05  FILLER  PIC X(15)  VALUE '00365C01Y000200'.
           05  FILLER  PIC X(15)  VALUE '01095C03Y000300'.
           05  FILLER  PIC X(15)  VALUE '01825C05Y000500'.
           05  FILLER  PIC X(15)  VALUE '03650C10Y000700'.
           05  FILLER  PIC X(15)  VALUE '99999C10+000900'.
       01  WS-CORP-TABLE REDEFINES WS-CORP-VALUES.
           05  WS-CORP-ENTRY           OCCURS 5 TIMES.
               10  WS-CORP-DAYS        PIC 9(05).
               10  WS-CORP-BAND        PIC X(04).
               10  WS-CORP-PCT         PIC 9(04)V99.
      *    MUNICIPALS (CHG12011)
       01  WS-MUNI-VALUES.
           05  FILLER  PIC X(15)  VALUE '00365M01Y000100'.
           05  FILLER  PIC X(15)  VALUE '01825M05Y000300'.
           05  FILLER  PIC X(15)  VALUE '03650M10Y000500'.
           05  FILLER  PIC X(15)  VALUE '99999M10+000700'.
       01  WS-MUNI-TABLE REDEFINES WS-MUNI-VALUES.
           05  WS-MUNI-ENTRY           OCCURS 4 TIMES.
               10  WS-MUNI-DAYS        PIC 9(05).
               10  WS-MUNI-BAND        PIC X(04).
               10  WS-MUNI-PCT         PIC 9(04)V99.
       01  WS-BAND-SUB                 PIC S9(04) COMP.
      *
      *---- FIRM INVENTORY TABLE (PASS 1) ----------------------------*
       01  WS-INVENTORY-TABLE.
           05  WS-IV-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-IV-ENTRY             OCCURS 3000 TIMES.
               10  WS-IV-ACCT          PIC X(10).
               10  WS-IV-CUSIP         PIC X(09).
               10  WS-IV-SEC-TYPE      PIC X(02).
               10  WS-IV-ISSUER        PIC X(06).
               10  WS-IV-BAND          PIC X(04).
               10  WS-IV-QTY           PIC S9(11)V9(04) COMP-3.
               10  WS-IV-MV            PIC S9(15)V99    COMP-3.
               10  WS-IV-PCT           PIC S9(03)V9(04) COMP-3.
               10  WS-IV-HAIRCUT       PIC S9(15)V99    COMP-3.
               10  WS-IV-CONC          PIC S9(15)V99    COMP-3.
               10  WS-IV-LONG-SHORT    PIC X(01).
       01  WS-IV-MAX                   PIC S9(04) COMP  VALUE 3000.
       01  WS-IV-SUB                   PIC S9(04) COMP.
      *
      *---- ISSUER TOTALS (LONG MARKET VALUE) -------------------------*
       01  WS-ISSUER-TABLE.
           05  WS-IS-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-IS-ENTRY             OCCURS 1000 TIMES
                                       INDEXED BY IS-IDX.
               10  WS-IS-ISSUER        PIC X(06).
               10  WS-IS-LONG-MV       PIC S9(15)V99    COMP-3.
               10  WS-IS-EXCESS        PIC S9(15)V99    COMP-3.
               10  WS-IS-POSITIONS     PIC S9(05)       COMP-3.
       01  WS-IS-MAX                   PIC S9(04) COMP  VALUE 1000.
       01  WS-CONC-THRESHOLD           PIC S9(15)V99    COMP-3.
      *
      *---- ACCOUNT SUMMARY -------------------------------------------*
       01  WS-ACCOUNT-TABLE.
           05  WS-AS-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-AS-ENTRY             OCCURS 50 TIMES
                                       INDEXED BY AS-IDX.
               10  WS-AS-ACCT          PIC X(10).
               10  WS-AS-POSNS         PIC S9(05)       COMP-3.
               10  WS-AS-MV            PIC S9(15)V99    COMP-3.
               10  WS-AS-HAIRCUT       PIC S9(15)V99    COMP-3.
               10  WS-AS-CONC          PIC S9(15)V99    COMP-3.
       01  WS-AS-MAX                   PIC S9(04) COMP  VALUE 50.
       01  WS-AS-SUB                   PIC S9(04) COMP.
       01  WS-DISP-CNT                 PIC ZZ,ZZ9.
       01  WS-DISP-AMT2                PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-AMT3                PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
      *---- POSITION WORK --------------------------------------------*
       01  WS-WORK.
           05  WS-SEC-TYPE             PIC X(02).
           05  WS-ISSUER               PIC X(06).
           05  WS-MATURITY             PIC 9(08).
           05  WS-DAYS-TO-MAT          PIC S9(07)       COMP-3.
           05  WS-BAND                 PIC X(04).
           05  WS-PCT                  PIC S9(03)V9(04) COMP-3.
           05  WS-ABS-MV               PIC S9(15)V99    COMP-3.
           05  WS-HAIRCUT              PIC S9(15)V99    COMP-3.
           05  WS-CONC-SHARE           PIC S9(15)V99    COMP-3.
      *
      *---- COUNTERS AND TOTALS ---------------------------------------*
       01  WS-COUNTERS.
           05  WS-POSN-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-CLIENT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-FLAT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-FIRM            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NOT-VALUED           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-NOT-FOUND        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MATURED              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-MARKET            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STALE-PRICE          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CONC-EXEMPTED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SHORTS               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CONC-ISSUERS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CONC-POSNS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HCUT-OUT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-LONG-MV          PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-SHORT-MV         PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-HAIRCUT          PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-CONC             PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-QTY              PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-NET-CAPITAL          PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
       01  WS-DISP-AMT                 PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
           COPY SRPOSN.
           COPY RRHCUT.
           COPY CMSECMS.
           COPY CMDATEW.
           COPY CMDTLNK.
           COPY CMSECLNK.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT.
           PERFORM 2000-LOAD-INVENTORY THRU 2000-EXIT
               UNTIL WS-POSN-EOF.
           PERFORM 5000-CONCENTRATION  THRU 5000-EXIT.
           PERFORM 6000-WRITE-HAIRCUTS THRU 6000-EXIT
               VARYING WS-IV-SUB FROM 1 BY 1
               UNTIL WS-IV-SUB > WS-IV-USED.
           PERFORM 9000-TERMINATE      THRU 9000-EXIT.
           MOVE WS-RETURN-CODE         TO RETURN-CODE.
           GOBACK.
      *
      *================================================================*
      * 1000 - INITIALIZE                                              *
      *================================================================*
       1000-INITIALIZE.
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'NET CAPITAL HAIRCUTS STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS NOT = '00'
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           PERFORM 1100-READ-PARM      THRU 1100-EXIT
               UNTIL WS-PARM-EOF
           CLOSE PARMCARD
           IF NOT WS-TNC-GIVEN
               DISPLAY 'RRB500 W - NO TNC CARD - UNDUE CONCENTRATION '
                       'NOT COMPUTED'
               IF WS-RETURN-CODE < 4
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
           END-IF
      *
           OPEN INPUT POSMAST-FILE
           IF WS-POSMAST-FS NOT = '00'
               MOVE 'POSMAST'          TO AB-DDNAME
               MOVE WS-POSMAST-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           OPEN OUTPUT HCUTOUT-FILE
           IF WS-HCUTOUT-FS NOT = '00'
               MOVE 'HCUTOUT'          TO AB-DDNAME
               MOVE WS-HCUTOUT-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           PERFORM 8000-READ-POSITION  THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
      *---- TNC=0000150000000.00  (13 DIGITS . 2 DIGITS) ------------*
       1100-READ-PARM.
           READ PARMCARD
           IF PARMCARD-EOF
               MOVE 'Y'                TO WS-PARM-EOF-SW
               GO TO 1100-EXIT
           END-IF
           IF NOT PARMCARD-OK
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-FS     TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '1100-READ-PARM'   TO AB-PARAGRAPH
               MOVE 'READ FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           IF PARM-CARD-REC(1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           DISPLAY 'RRB500 CARD: ' PARM-CARD-REC(1:72)
           MOVE SPACES                 TO WS-PARM-KEYWORD WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'TNC'
                   CONTINUE
               WHEN 'CONC-EXEMPT'
                   PERFORM 1150-EXEMPT-CARD THRU 1150-EXIT
                   GO TO 1100-EXIT
               WHEN 'TRACE'
                   IF WS-PARM-VALUE(1:1) = 'Y'
                       MOVE 'Y'        TO WS-TRACE-SW
                   END-IF
                   GO TO 1100-EXIT
               WHEN OTHER
                   PERFORM 1190-BAD-CARD THRU 1190-EXIT
                   GO TO 1100-EXIT
           END-EVALUATE
           IF WS-PV-DOLLARS NOT NUMERIC
           OR WS-PV-POINT NOT = '.'
           OR WS-PV-CENTS NOT NUMERIC
           OR WS-PV-REST NOT = SPACES
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE 1008               TO AB-ABEND-CODE
               MOVE '1100-READ-PARM'   TO AB-PARAGRAPH
               MOVE WS-PARM-VALUE      TO AB-KEY
               MOVE 'TNC CARD NOT NNNNNNNNNNNNN.NN' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           MOVE WS-PV-DOLLARS          TO WS-PVA-DOLLARS
           MOVE WS-PV-CENTS            TO WS-PVA-CENTS
           MOVE WS-PV-AMOUNT           TO WS-TNC
           MOVE 'Y'                    TO WS-TNC-SW.
       1100-EXIT.
           EXIT.
      *
      *---- CONC-EXEMPT=<ISSUER 6> - COMPLIANCE APPROVAL ON FILE ------*
       1150-EXEMPT-CARD.
           IF WS-PARM-VALUE(1:6) = SPACES
           OR WS-PARM-VALUE(7:) NOT = SPACES
           OR WS-EX-USED NOT < WS-EX-MAX
               PERFORM 1190-BAD-CARD   THRU 1190-EXIT
               GO TO 1150-EXIT
           END-IF
           ADD 1                       TO WS-EX-USED
           MOVE WS-PARM-VALUE(1:6)     TO WS-EX-ISSUER(WS-EX-USED).
       1150-EXIT.
           EXIT.
      *
       1190-BAD-CARD.
           ADD 1                       TO WS-PARM-ERRORS
           DISPLAY 'RRB500 W - CONTROL CARD IGNORED: '
                   PARM-CARD-REC(1:40)
           IF WS-RETURN-CODE < 4
               MOVE 4                  TO WS-RETURN-CODE
           END-IF.
       1190-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - PASS 1: FIRM INVENTORY POSITIONS AND BASE HAIRCUT       *
      *================================================================*
       2000-LOAD-INVENTORY.
           ADD 1                       TO WS-POSN-READ
      *    FIRM ACCTS SORT LOW - CLIENT ACCOUNTS ARE ALL NUMERIC
           IF POS-ACCT-NO(1:1) NOT < '0'
               ADD 1                   TO WS-POSN-CLIENT
               GO TO 2000-NEXT
           END-IF
      *    STREET (LOCATION) ROWS ARE NOT INVENTORY
           IF POS-ACCT-TYPE = 'ST'
               ADD 1                   TO WS-POSN-STREET
               GO TO 2000-NEXT
           END-IF
           IF POS-TD-QTY = ZERO
               ADD 1                   TO WS-POSN-FLAT
               GO TO 2000-NEXT
           END-IF
           ADD 1                       TO WS-POSN-FIRM
           IF WS-IV-USED NOT < WS-IV-MAX
               MOVE 1007               TO AB-ABEND-CODE
               MOVE '2000-LOAD-INVENTORY' TO AB-PARAGRAPH
               MOVE POS-KEY            TO AB-KEY
               MOVE 'MORE THAN 3000 FIRM POSITIONS' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           IF POS-MKT-VALUE-USD NOT NUMERIC
               ADD 1                   TO WS-NOT-VALUED
               MOVE ZERO               TO POS-MKT-VALUE-USD
               DISPLAY 'RRB500 W - POSITION NOT VALUED ' POS-KEY
               IF WS-RETURN-CODE < 4
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
           END-IF
      *    PRICE MUST BE FROM THE PREVIOUS CLOSE OR LATER (CHG37705)
           IF POS-PRICE-DATE NOT NUMERIC
           OR POS-PRICE-DATE < DC-PREV-BUS-DATE
               ADD 1                   TO WS-STALE-PRICE
               DISPLAY 'RRB500 W - STALE PRICE ' POS-KEY ' '
                       POS-PRICE-DATE
               IF WS-RETURN-CODE < 4
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
           END-IF
           PERFORM 2100-GET-SECURITY   THRU 2100-EXIT
           PERFORM 3000-DETERMINE-BAND THRU 3000-EXIT
      *    ---- BASE HAIRCUT ON ABSOLUTE MARKET VALUE ---------------
           IF POS-MKT-VALUE-USD < ZERO
               COMPUTE WS-ABS-MV = POS-MKT-VALUE-USD * -1
           ELSE
               MOVE POS-MKT-VALUE-USD  TO WS-ABS-MV
           END-IF
           COMPUTE WS-HAIRCUT ROUNDED = WS-ABS-MV * WS-PCT / 100
      *    ---- INTO THE TABLE --------------------------------------
           ADD 1                       TO WS-IV-USED
           MOVE WS-IV-USED             TO WS-IV-SUB
           MOVE POS-ACCT-NO            TO WS-IV-ACCT(WS-IV-SUB)
           MOVE POS-CUSIP              TO WS-IV-CUSIP(WS-IV-SUB)
           MOVE WS-SEC-TYPE            TO WS-IV-SEC-TYPE(WS-IV-SUB)
           MOVE WS-ISSUER              TO WS-IV-ISSUER(WS-IV-SUB)
           MOVE WS-BAND                TO WS-IV-BAND(WS-IV-SUB)
           MOVE POS-TD-QTY             TO WS-IV-QTY(WS-IV-SUB)
           MOVE POS-MKT-VALUE-USD      TO WS-IV-MV(WS-IV-SUB)
           MOVE WS-PCT                 TO WS-IV-PCT(WS-IV-SUB)
           MOVE WS-HAIRCUT             TO WS-IV-HAIRCUT(WS-IV-SUB)
           MOVE ZERO                   TO WS-IV-CONC(WS-IV-SUB)
           IF POS-TD-QTY < ZERO
               MOVE 'S'                TO WS-IV-LONG-SHORT(WS-IV-SUB)
               ADD 1                   TO WS-SHORTS
               ADD WS-ABS-MV           TO WS-TOT-SHORT-MV
           ELSE
               MOVE 'L'                TO WS-IV-LONG-SHORT(WS-IV-SUB)
               ADD WS-ABS-MV           TO WS-TOT-LONG-MV
               PERFORM 2200-ADD-ISSUER THRU 2200-EXIT
           END-IF
           ADD WS-HAIRCUT              TO WS-TOT-HAIRCUT
           IF WS-TRACE-ON
               MOVE WS-HAIRCUT         TO WS-DISP-AMT
               DISPLAY 'RRB500 TRACE ' POS-KEY ' ' WS-SEC-TYPE ' '
                       WS-BAND ' ' WS-DAYS-TO-MAT ' ' WS-DISP-AMT
           END-IF.
       2000-NEXT.
           PERFORM 8000-READ-POSITION  THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *---- SECURITY TYPE, ISSUER, MATURITY FROM THE MASTER ----------*
       2100-GET-SECURITY.
           MOVE 'GET '                 TO SL-FUNCTION
           MOVE POS-CUSIP              TO SL-KEY-CUSIP
           MOVE SPACES                 TO SL-KEY-ISIN SL-KEY-SYMBOL
           CALL 'CMD010' USING SL-SECURITY-PARMS
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA    TO SEC-MASTER-REC
                   MOVE SEC-TYPE       TO WS-SEC-TYPE
                   MOVE SEC-ISSUER-ID  TO WS-ISSUER
                   MOVE ZERO           TO WS-MATURITY
                   IF SEC-FIXED-INCOME
                       IF SEC-MATURITY-DATE NUMERIC
                           MOVE SEC-MATURITY-DATE TO WS-MATURITY
                       END-IF
                   END-IF
               WHEN SL-NOT-FOUND
                   ADD 1               TO WS-SEC-NOT-FOUND
                   MOVE POS-SEC-TYPE   TO WS-SEC-TYPE
                   MOVE SPACES         TO WS-ISSUER
                   MOVE ZERO           TO WS-MATURITY
                   DISPLAY 'RRB500 W - SECURITY NOT ON MASTER '
                           POS-CUSIP
                   IF WS-RETURN-CODE < 4
                       MOVE 4          TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '2100-GET-SECURITY' TO AB-PARAGRAPH
                   MOVE POS-CUSIP      TO AB-KEY
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *
      *---- LONG MARKET VALUE BY ISSUER -------------------------------*
       2200-ADD-ISSUER.
           IF WS-ISSUER = SPACES
               GO TO 2200-EXIT
           END-IF
           SET IS-IDX                  TO 1
           SEARCH WS-IS-ENTRY
               AT END
                   PERFORM 2210-NEW-ISSUER THRU 2210-EXIT
               WHEN IS-IDX > WS-IS-USED
                   PERFORM 2210-NEW-ISSUER THRU 2210-EXIT
               WHEN WS-IS-ISSUER(IS-IDX) = WS-ISSUER
                   ADD WS-ABS-MV       TO WS-IS-LONG-MV(IS-IDX)
                   ADD 1               TO WS-IS-POSITIONS(IS-IDX)
           END-SEARCH.
       2200-EXIT.
           EXIT.
      *
       2210-NEW-ISSUER.
           IF WS-IS-USED NOT < WS-IS-MAX
               MOVE 1007               TO AB-ABEND-CODE
               MOVE '2210-NEW-ISSUER'  TO AB-PARAGRAPH
               MOVE WS-ISSUER          TO AB-KEY
               MOVE 'MORE THAN 1000 ISSUERS' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           ADD 1                       TO WS-IS-USED
           MOVE WS-ISSUER              TO WS-IS-ISSUER(WS-IS-USED)
           MOVE WS-ABS-MV              TO WS-IS-LONG-MV(WS-IS-USED)
           MOVE ZERO                   TO WS-IS-EXCESS(WS-IS-USED)
           MOVE 1                      TO WS-IS-POSITIONS(WS-IS-USED).
       2210-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - HAIRCUT BAND AND PERCENT FOR THE SECURITY TYPE          *
      *================================================================*
       3000-DETERMINE-BAND.
           EVALUATE WS-SEC-TYPE
               WHEN 'EQ'
               WHEN 'PF'
               WHEN 'AD'
                   MOVE 'EQTY'         TO WS-BAND
                   MOVE WS-EQUITY-PCT  TO WS-PCT
               WHEN 'MF'
                   MOVE 'MFND'         TO WS-BAND
                   MOVE WS-FUND-PCT    TO WS-PCT
               WHEN 'GV'
                   PERFORM 3100-DAYS-TO-MATURITY THRU 3100-EXIT
                   PERFORM 3200-GOVT-BAND THRU 3200-EXIT
               WHEN 'CB'
                   PERFORM 3100-DAYS-TO-MATURITY THRU 3100-EXIT
                   PERFORM 3300-CORP-BAND THRU 3300-EXIT
               WHEN 'MU'
                   PERFORM 3100-DAYS-TO-MATURITY THRU 3100-EXIT
                   PERFORM 3400-MUNI-BAND THRU 3400-EXIT
               WHEN OTHER
                   ADD 1               TO WS-NO-MARKET
                   MOVE 'NMKT'         TO WS-BAND
                   MOVE WS-NO-MARKET-PCT TO WS-PCT
           END-EVALUATE.
       3000-EXIT.
           EXIT.
      *
      *---- CALENDAR DAYS FROM THE BUSINESS DATE TO MATURITY ----------*
       3100-DAYS-TO-MATURITY.
           IF WS-MATURITY = ZERO
               ADD 1                   TO WS-MATURED
               MOVE ZERO               TO WS-DAYS-TO-MAT
               DISPLAY 'RRB500 W - NO MATURITY DATE ' POS-CUSIP
               IF WS-RETURN-CODE < 4
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
               GO TO 3100-EXIT
           END-IF
           MOVE 'DIFC'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE DC-BUS-DATE            TO DT-DATE-1
           MOVE WS-MATURITY            TO DT-DATE-2
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '3100-DAYS-TO-MATURITY' TO AB-PARAGRAPH
               MOVE POS-CUSIP          TO AB-KEY
               MOVE DT-MESSAGE         TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           MOVE DT-RESULT-NUM          TO WS-DAYS-TO-MAT
           IF WS-DAYS-TO-MAT NOT > ZERO
               ADD 1                   TO WS-MATURED
               MOVE ZERO               TO WS-DAYS-TO-MAT
               DISPLAY 'RRB500 W - MATURED BOND IN INVENTORY '
                       POS-CUSIP ' ' WS-MATURITY
               IF WS-RETURN-CODE < 4
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
           END-IF.
       3100-EXIT.
           EXIT.
      *
       3200-GOVT-BAND.
           PERFORM VARYING WS-BAND-SUB FROM 1 BY 1
                   UNTIL WS-BAND-SUB > 11
                      OR WS-DAYS-TO-MAT < WS-GOVT-DAYS(WS-BAND-SUB)
               CONTINUE
           END-PERFORM
           IF WS-BAND-SUB > 12
               MOVE 12                 TO WS-BAND-SUB
           END-IF
           MOVE WS-GOVT-BAND(WS-BAND-SUB) TO WS-BAND
           MOVE WS-GOVT-PCT(WS-BAND-SUB)  TO WS-PCT.
       3200-EXIT.
           EXIT.
      *
       3300-CORP-BAND.
           PERFORM VARYING WS-BAND-SUB FROM 1 BY 1
                   UNTIL WS-BAND-SUB > 4
                      OR WS-DAYS-TO-MAT < WS-CORP-DAYS(WS-BAND-SUB)
               CONTINUE
           END-PERFORM
           MOVE WS-CORP-BAND(WS-BAND-SUB) TO WS-BAND
           MOVE WS-CORP-PCT(WS-BAND-SUB)  TO WS-PCT.
       3300-EXIT.
           EXIT.
      *
       3400-MUNI-BAND.
           PERFORM VARYING WS-BAND-SUB FROM 1 BY 1
                   UNTIL WS-BAND-SUB > 3
                      OR WS-DAYS-TO-MAT < WS-MUNI-DAYS(WS-BAND-SUB)
               CONTINUE
           END-PERFORM
           MOVE WS-MUNI-BAND(WS-BAND-SUB) TO WS-BAND
           MOVE WS-MUNI-PCT(WS-BAND-SUB)  TO WS-PCT.
       3400-EXIT.
           EXIT.
      *
      *================================================================*
      * 5000 - PASS 2: UNDUE CONCENTRATION (CHG27120)                  *
      *   THRESHOLD = 10 PCT OF TNC.  FOR AN ISSUER ABOVE IT, EACH     *
      *   LONG POSITION TAKES ITS SHARE OF THE EXCESS TIMES 50 PCT OF  *
      *   ITS OWN BASE PERCENT.  GOVERNMENTS ARE EXEMPT.               *
      *================================================================*
       5000-CONCENTRATION.
           IF NOT WS-TNC-GIVEN
               GO TO 5000-EXIT
           END-IF
           COMPUTE WS-CONC-THRESHOLD ROUNDED =
                   WS-TNC * WS-CONC-LIMIT-PCT / 100
           PERFORM VARYING IS-IDX FROM 1 BY 1
                   UNTIL IS-IDX > WS-IS-USED
               MOVE 'N'                TO WS-EXEMPT-SW
               IF WS-EX-USED > ZERO
                   SET EX-IDX          TO 1
                   SEARCH WS-EX-ISSUER
                       AT END
                           CONTINUE
                       WHEN WS-EX-ISSUER(EX-IDX) = WS-IS-ISSUER(IS-IDX)
                           MOVE 'Y'    TO WS-EXEMPT-SW
                   END-SEARCH
               END-IF
               IF WS-ISSUER-EXEMPT
               AND WS-IS-LONG-MV(IS-IDX) > WS-CONC-THRESHOLD
                   ADD 1               TO WS-CONC-EXEMPTED
                   DISPLAY 'RRB500 CONCENTRATION EXEMPT '
                           WS-IS-ISSUER(IS-IDX)
               END-IF
               IF WS-IS-LONG-MV(IS-IDX) > WS-CONC-THRESHOLD
               AND NOT WS-ISSUER-EXEMPT
                   COMPUTE WS-IS-EXCESS(IS-IDX) =
                           WS-IS-LONG-MV(IS-IDX) - WS-CONC-THRESHOLD
                   ADD 1               TO WS-CONC-ISSUERS
                   DISPLAY 'RRB500 CONCENTRATION ISSUER '
                           WS-IS-ISSUER(IS-IDX)
               END-IF
           END-PERFORM
           PERFORM 5100-POSITION-CONC  THRU 5100-EXIT
               VARYING WS-IV-SUB FROM 1 BY 1
               UNTIL WS-IV-SUB > WS-IV-USED.
       5000-EXIT.
           EXIT.
      *
       5100-POSITION-CONC.
           IF WS-IV-LONG-SHORT(WS-IV-SUB) NOT = 'L'
           OR WS-IV-SEC-TYPE(WS-IV-SUB) = 'GV'
           OR WS-IV-ISSUER(WS-IV-SUB) = SPACES
               GO TO 5100-EXIT
           END-IF
           SET IS-IDX                  TO 1
           SEARCH WS-IS-ENTRY
               AT END
                   GO TO 5100-EXIT
               WHEN WS-IS-ISSUER(IS-IDX) = WS-IV-ISSUER(WS-IV-SUB)
                   CONTINUE
           END-SEARCH
           IF WS-IS-EXCESS(IS-IDX) NOT > ZERO
               GO TO 5100-EXIT
           END-IF
      *    SHARE OF THE EXCESS, THEN 50 PCT OF THE BASE PERCENT
           COMPUTE WS-CONC-SHARE ROUNDED =
                   WS-IV-MV(WS-IV-SUB) * WS-IS-EXCESS(IS-IDX)
                                       / WS-IS-LONG-MV(IS-IDX)
           COMPUTE WS-IV-CONC(WS-IV-SUB) ROUNDED =
                   WS-CONC-SHARE * WS-IV-PCT(WS-IV-SUB) / 100
                                 * WS-CONC-ADDON-PCT / 100
           ADD WS-IV-CONC(WS-IV-SUB)   TO WS-TOT-CONC
           ADD 1                       TO WS-CONC-POSNS.
       5100-EXIT.
           EXIT.
      *
      *================================================================*
      * 6000 - ONE RRHCUT RECORD PER POSITION (POSITION KEY ORDER)     *
      *================================================================*
       6000-WRITE-HAIRCUTS.
           MOVE SPACES                 TO RRH-HAIRCUT-REC
           MOVE DC-BUS-DATE            TO RRH-BUS-DATE
           MOVE WS-IV-ACCT(WS-IV-SUB)  TO RRH-ACCT-NO
           MOVE WS-IV-CUSIP(WS-IV-SUB) TO RRH-CUSIP
           MOVE WS-IV-SEC-TYPE(WS-IV-SUB) TO RRH-SEC-TYPE
           MOVE WS-IV-ISSUER(WS-IV-SUB) TO RRH-ISSUER-ID
           MOVE WS-IV-BAND(WS-IV-SUB)  TO RRH-BUCKET
           MOVE WS-IV-QTY(WS-IV-SUB)   TO RRH-QTY
           MOVE WS-IV-MV(WS-IV-SUB)    TO RRH-MKT-VALUE-USD
           MOVE WS-IV-PCT(WS-IV-SUB)   TO RRH-HAIRCUT-PCT
           MOVE WS-IV-HAIRCUT(WS-IV-SUB) TO RRH-HAIRCUT-AMT
           MOVE WS-IV-CONC(WS-IV-SUB)  TO RRH-UNDUE-CONC-AMT
           MOVE WS-IV-LONG-SHORT(WS-IV-SUB) TO RRH-LONG-SHORT
           WRITE HCUTOUT-REC FROM RRH-HAIRCUT-REC
           IF WS-HCUTOUT-FS NOT = '00'
               MOVE 'HCUTOUT'          TO AB-DDNAME
               MOVE WS-HCUTOUT-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '6000-WRITE-HAIRCUTS' TO AB-PARAGRAPH
               MOVE RRH-CUSIP          TO AB-KEY
               MOVE 'WRITE FAILED'     TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           ADD 1                       TO WS-HCUT-OUT
           ADD WS-IV-QTY(WS-IV-SUB)    TO WS-TOT-QTY
           PERFORM 6100-ACCOUNT-SUMMARY THRU 6100-EXIT.
       6000-EXIT.
           EXIT.
      *
      *---- HAIRCUT BY FIRM ACCOUNT (DESK) FOR THE SYSOUT -------------*
       6100-ACCOUNT-SUMMARY.
           SET AS-IDX                  TO 1
           SEARCH WS-AS-ENTRY
               AT END
                   DISPLAY 'RRB500 W - ACCOUNT SUMMARY FULL '
                           WS-IV-ACCT(WS-IV-SUB)
               WHEN AS-IDX > WS-AS-USED
                   ADD 1               TO WS-AS-USED
                   MOVE WS-IV-ACCT(WS-IV-SUB) TO WS-AS-ACCT(WS-AS-USED)
                   MOVE 1              TO WS-AS-POSNS(WS-AS-USED)
                   MOVE WS-IV-MV(WS-IV-SUB) TO WS-AS-MV(WS-AS-USED)
                   MOVE WS-IV-HAIRCUT(WS-IV-SUB)
                                       TO WS-AS-HAIRCUT(WS-AS-USED)
                   MOVE WS-IV-CONC(WS-IV-SUB)
                                       TO WS-AS-CONC(WS-AS-USED)
               WHEN WS-AS-ACCT(AS-IDX) = WS-IV-ACCT(WS-IV-SUB)
                   ADD 1               TO WS-AS-POSNS(AS-IDX)
                   ADD WS-IV-MV(WS-IV-SUB) TO WS-AS-MV(AS-IDX)
                   ADD WS-IV-HAIRCUT(WS-IV-SUB) TO WS-AS-HAIRCUT(AS-IDX)
                   ADD WS-IV-CONC(WS-IV-SUB) TO WS-AS-CONC(AS-IDX)
           END-SEARCH.
       6100-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O                                                     *
      *================================================================*
       8000-READ-POSITION.
           READ POSMAST-FILE INTO POS-POSITION-REC
           EVALUATE TRUE
               WHEN POSMAST-OK
                   IF POS-TD-QTY NOT NUMERIC
                       MOVE ZERO       TO POS-TD-QTY
                   END-IF
               WHEN POSMAST-EOF
                   MOVE 'Y'            TO WS-POSN-EOF-SW
               WHEN OTHER
                   MOVE 'POSMAST'      TO AB-DDNAME
                   MOVE WS-POSMAST-FS  TO AB-FILE-STATUS
                   MOVE 1002           TO AB-ABEND-CODE
                   MOVE '8000-READ-POSITION' TO AB-PARAGRAPH
                   MOVE 'READ FAILED'  TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE POSMAST-FILE
           CLOSE HCUTOUT-FILE
           IF WS-HCUTOUT-FS NOT = '00'
               MOVE 'HCUTOUT'          TO AB-DDNAME
               MOVE WS-HCUTOUT-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED'     TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           COMPUTE WS-NET-CAPITAL = WS-TNC - WS-TOT-HAIRCUT
                                           - WS-TOT-CONC
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
           MOVE 'RRB500'               TO CT-STAGE
           MOVE 'POSN-IN'              TO CT-COUNTER-NAME
           MOVE WS-POSN-READ           TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           MOVE 'HAIRCUT-OUT'          TO CT-COUNTER-NAME
           MOVE WS-HCUT-OUT            TO CT-COUNT
           COMPUTE CT-AMOUNT = WS-TOT-HAIRCUT + WS-TOT-CONC
           MOVE WS-TOT-QTY             TO CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           DISPLAY '************************************************'
           DISPLAY '* RRB500 - NET CAPITAL HAIRCUTS                *'
           DISPLAY '************************************************'
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE
           DISPLAY ' POSITIONS READ           : ' WS-POSN-READ
           DISPLAY '   CLIENT                 : ' WS-POSN-CLIENT
           DISPLAY '   STREET / LOCATION      : ' WS-POSN-STREET
           DISPLAY '   FIRM - FLAT            : ' WS-POSN-FLAT
           DISPLAY '   FIRM INVENTORY         : ' WS-POSN-FIRM
           DISPLAY ' NOT VALUED               : ' WS-NOT-VALUED
           DISPLAY ' SECURITY NOT ON MASTER   : ' WS-SEC-NOT-FOUND
           DISPLAY ' MATURED / NO MATURITY    : ' WS-MATURED
           DISPLAY ' NO MARKET (100 PCT)      : ' WS-NO-MARKET
           DISPLAY ' STALE PRICES             : ' WS-STALE-PRICE
           DISPLAY ' CONTROL CARDS IGNORED    : ' WS-PARM-ERRORS
           DISPLAY ' CONCENTRATION EXEMPTED   : ' WS-CONC-EXEMPTED
           DISPLAY ' SHORT POSITIONS          : ' WS-SHORTS
           DISPLAY ' ISSUERS                  : ' WS-IS-USED
           DISPLAY ' CONCENTRATED ISSUERS     : ' WS-CONC-ISSUERS
           DISPLAY ' CONCENTRATION POSITIONS  : ' WS-CONC-POSNS
           DISPLAY ' HAIRCUT RECORDS WRITTEN  : ' WS-HCUT-OUT
           MOVE WS-TOT-LONG-MV         TO WS-DISP-AMT
           DISPLAY ' LONG MARKET VALUE        : ' WS-DISP-AMT
           MOVE WS-TOT-SHORT-MV        TO WS-DISP-AMT
           DISPLAY ' SHORT MARKET VALUE       : ' WS-DISP-AMT
           MOVE WS-TOT-HAIRCUT         TO WS-DISP-AMT
           DISPLAY ' BASE HAIRCUTS            : ' WS-DISP-AMT
           MOVE WS-TOT-CONC            TO WS-DISP-AMT
           DISPLAY ' UNDUE CONCENTRATION      : ' WS-DISP-AMT
           MOVE WS-TNC                 TO WS-DISP-AMT
           DISPLAY ' TENTATIVE NET CAPITAL    : ' WS-DISP-AMT
           MOVE WS-NET-CAPITAL         TO WS-DISP-AMT
           DISPLAY ' NET CAPITAL              : ' WS-DISP-AMT
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE
           DISPLAY '------------------------------------------------'
           DISPLAY ' ACCOUNT     POSNS        MARKET VALUE USD'
                   '             HAIRCUT       CONCENTRATION'
           PERFORM VARYING WS-AS-SUB FROM 1 BY 1
                   UNTIL WS-AS-SUB > WS-AS-USED
               MOVE WS-AS-POSNS(WS-AS-SUB)   TO WS-DISP-CNT
               MOVE WS-AS-MV(WS-AS-SUB)      TO WS-DISP-AMT2
               MOVE WS-AS-HAIRCUT(WS-AS-SUB) TO WS-DISP-AMT
               MOVE WS-AS-CONC(WS-AS-SUB)    TO WS-DISP-AMT3
               DISPLAY ' ' WS-AS-ACCT(WS-AS-SUB) ' ' WS-DISP-CNT ' '
                       WS-DISP-AMT2 ' ' WS-DISP-AMT(8:) ' '
                       WS-DISP-AMT3
           END-PERFORM
           DISPLAY '************************************************'
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           MOVE SPACES                 TO AU-KEY
           IF WS-RETURN-CODE > ZERO
               MOVE 'W'                TO AU-SEVERITY
               MOVE 'NET CAPITAL HAIRCUTS ENDED WITH WARNINGS'
                                       TO AU-MESSAGE
           ELSE
               MOVE 'I'                TO AU-SEVERITY
               MOVE 'NET CAPITAL HAIRCUTS ENDED' TO AU-MESSAGE
           END-IF
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9999 - ABEND                                                   *
      *================================================================*
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'RRB500 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH
           DISPLAY 'RRB500 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY
           DISPLAY 'RRB500 ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
