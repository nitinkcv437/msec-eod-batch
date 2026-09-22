       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCR510.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  10/12/1993.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCR510                                            *
      * TITLE      : DAILY TRADE BLOTTER BY BRANCH / REP / ACCOUNT     *
      * JOB        : MSTCD070   STEP030                                *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   LISTS EVERY FINAL TRADE OF THE DAY FOR BRANCH SUPERVISION.   *
      *   INPUT IS TC.TRADES.FINAL SORTED BY BRANCH, REP, ACCOUNT AND  *
      *   TRADE ID (MSTCD070 STEP020).                                 *
      *   BREAKS:  BRANCH (NEW PAGE)  REP  ACCOUNT.                    *
      *   WITHIN A REP THE HOUSE ACCOUNTS (FIRM INVENTORY / STREET)    *
      *   ARE LISTED FIRST WITH THEIR OWN SUBTOTAL, THEN THE CLIENT    *
      *   ACCOUNTS.  TOTALS ARE IN USD (TRD-USD-NET-AMOUNT).           *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     PARAMETER CARDS TCP510A                     *
      *          BLOTIN    MSEC.PROD.TC.BLOTTER.SORTED(+1) (TCTRADE)   *
      * OUTPUT : RPTFILE   REPORT FBA 133                (CMRPTHD)     *
      * CALLS  : CMU050 CMU060 CMU080                                  *
      *                                                                *
      * RETURN CODES: 0 ALWAYS (UNLESS ABEND)                          *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1993-10-12 DWB            ORIGINAL                             *
      * 1995-07-17 DWB  CHG01877  OMS TRADES, SOURCE COLUMN            *
      * 1998-11-02 TLM  CHG04471  Y2K - SETTLE DATE CCYYMMDD           *
      * 2001-04-09 KAP  CHG08814  DECIMAL PRICES (4 DEC ON REPORT)     *
      * 2002-03-08 KAP  CHG09930  FEES COLUMN (SEC + TAF + OTHER)      *
      * 2004-01-26 KAP  CHG11790  HOUSE ACCOUNT SECTION PER REP        *
      * 2009-12-14 SPA  CHG19002  TOTALS IN USD                        *
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
           SELECT BLOTIN-FILE   ASSIGN TO BLOTIN
                                FILE STATUS IS WS-BLOTIN-FS.
           SELECT RPTFILE       ASSIGN TO RPTFILE
                                FILE STATUS IS WS-RPTFILE-FS.
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
       FD  BLOTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BLOTIN-REC                  PIC X(400).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-LINE                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCR510'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-BLOTIN-FS            PIC X(02).
               88  BLOTIN-OK                     VALUE '00'.
               88  BLOTIN-EOF                    VALUE '10'.
           05  WS-RPTFILE-FS           PIC X(02).
               88  RPTFILE-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-FILE                VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  WS-FIRST-RECORD               VALUE 'Y'.
      *    SECTION WITHIN A REP: F = HOUSE ACCOUNTS, C = CLIENTS
           05  WS-SECTION-SW           PIC X(01)  VALUE 'F'.
               88  WS-IN-HOUSE-SECTION           VALUE 'F'.
               88  WS-IN-CLIENT-SECTION          VALUE 'C'.
           05  WS-HOUSE-OPT-SW         PIC X(01)  VALUE 'Y'.
               88  WS-HOUSE-SECTION-ON           VALUE 'Y'.
           05  WS-SHOW-CANCEL-SW       PIC X(01)  VALUE 'Y'.
               88  WS-SHOW-CANCELS               VALUE 'Y'.
           05  WS-HOUSE-HDR-SW         PIC X(01)  VALUE 'N'.
               88  WS-HOUSE-HDR-PRINTED          VALUE 'Y'.
       01  WS-PARM-KEYWORD             PIC X(30).
       01  WS-PARM-VALUE               PIC X(30).
      *
       01  WS-SAVE-KEYS.
           05  WS-SAVE-BRANCH          PIC X(03).
           05  WS-SAVE-REP             PIC X(04).
           05  WS-SAVE-ACCT            PIC X(10).
           05  WS-SAVE-ACCT-TYPE       PIC X(02).
      *
      *----------------------------------------------------------------*
      * ACCUMULATORS  (1=ACCOUNT 2=HOUSE 3=CLIENT 4=REP 5=BRANCH       *
      *                6=GRAND)                                        *
      *----------------------------------------------------------------*
       01  WS-ACCUMULATORS.
           05  WS-ACC                  OCCURS 6 TIMES.
               10  WS-ACC-COUNT        PIC S9(07)       COMP-3.
               10  WS-ACC-BUY-USD      PIC S9(15)V99    COMP-3.
               10  WS-ACC-SELL-USD     PIC S9(15)V99    COMP-3.
               10  WS-ACC-COMM         PIC S9(13)V99    COMP-3.
               10  WS-ACC-FEES         PIC S9(13)V99    COMP-3.
               10  WS-ACC-CXL          PIC S9(07)       COMP-3.
       01  WS-LVL                      PIC S9(04) COMP.
       01  WS-LVL-ACCOUNT              PIC S9(04) COMP VALUE 1.
       01  WS-LVL-HOUSE                PIC S9(04) COMP VALUE 2.
       01  WS-LVL-CLIENT               PIC S9(04) COMP VALUE 3.
       01  WS-LVL-REP                  PIC S9(04) COMP VALUE 4.
       01  WS-LVL-BRANCH               PIC S9(04) COMP VALUE 5.
       01  WS-LVL-GRAND                PIC S9(04) COMP VALUE 6.
      *
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-BRANCHES             PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-REPS                 PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-ACCOUNTS             PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(07) COMP-3 VALUE ZERO.
       01  WS-WORK.
           05  WS-FEES                 PIC S9(11)V99    COMP-3.
           05  WS-USD-NET              PIC S9(15)V99    COMP-3.
           05  WS-TOTAL-LABEL          PIC X(30).
           05  WS-EDIT-DATE.
               10  WS-ED-MM            PIC 9(02).
               10  FILLER              PIC X(01)  VALUE '/'.
               10  WS-ED-DD            PIC 9(02).
               10  FILLER              PIC X(01)  VALUE '/'.
               10  WS-ED-CCYY          PIC 9(04).
      *
       COPY CMDATEW.
       COPY TCTRADE.
       COPY CMRPTHD.
      *
       01  RPT-BRANCH-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(08)  VALUE 'BRANCH: '.
           05  RPT-BR-BRANCH           PIC X(03).
           05  FILLER                  PIC X(121) VALUE SPACES.
       01  RPT-REP-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  FILLER                  PIC X(05)  VALUE 'REP: '.
           05  RPT-RP-REP              PIC X(04).
           05  FILLER                  PIC X(120) VALUE SPACES.
       01  RPT-SECTION-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(06)  VALUE SPACES.
           05  RPT-SC-TEXT             PIC X(40).
           05  FILLER                  PIC X(86)  VALUE SPACES.
       01  RPT-ACCT-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(06)  VALUE SPACES.
           05  FILLER                  PIC X(09)  VALUE 'ACCOUNT: '.
           05  RPT-AC-ACCT             PIC X(10).
           05  FILLER                  PIC X(08)  VALUE '  TYPE: '.
           05  RPT-AC-TYPE             PIC X(02).
           05  FILLER                  PIC X(97)  VALUE SPACES.
       01  RPT-COLUMN-HDR-1.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  FILLER                  PIC X(17)  VALUE 'TRADE ID'.
           05  FILLER                  PIC X(03)  VALUE 'TX'.
           05  FILLER                  PIC X(03)  VALUE 'SD'.
           05  FILLER                  PIC X(16)  VALUE
               '       QUANTITY'.
           05  FILLER                  PIC X(09)  VALUE 'SYMBOL'.
           05  FILLER                  PIC X(13)  VALUE '       PRICE'.
           05  FILLER                  PIC X(16)  VALUE
               '      PRINCIPAL'.
           05  FILLER                  PIC X(10)  VALUE '     COMM'.
           05  FILLER                  PIC X(10)  VALUE '     FEES'.
           05  FILLER                  PIC X(16)  VALUE
               '     NET AMOUNT'.
           05  FILLER                  PIC X(04)  VALUE 'CCY'.
           05  FILLER                  PIC X(09)  VALUE 'SETTLE'.
           05  FILLER                  PIC X(05)  VALUE 'SRC'.
       01  RPT-DETAIL-LINE.
           05  RPT-DT-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-TRADE-ID         PIC X(16).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-TXN              PIC X(02).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-SIDE             PIC X(02).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-QTY              PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-SYMBOL           PIC X(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-PRICE            PIC ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-PRINCIPAL        PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-COMM             PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-FEES             PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-NET              PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-CCY              PIC X(03).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-SETTLE           PIC 9(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DT-SOURCE           PIC X(03).
           05  FILLER                  PIC X(02)  VALUE SPACES.
       01  RPT-TOTAL-LINE.
           05  RPT-TT-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(06)  VALUE SPACES.
           05  RPT-TT-LABEL            PIC X(30).
           05  FILLER                  PIC X(07)  VALUE ' TRADES'.
           05  RPT-TT-COUNT            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(06)  VALUE '  BUY '.
           05  RPT-TT-BUY              PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(06)  VALUE '  SEL '.
           05  RPT-TT-SELL             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(06)  VALUE '  COM '.
           05  RPT-TT-COMM             PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(06)  VALUE '  CXL '.
           05  RPT-TT-CXL              PIC ZZ,ZZ9.
       01  RPT-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(40)  VALUE SPACES.
           05  FILLER                  PIC X(50)  VALUE
               '*** NO FINAL TRADES FOR THIS BUSINESS DATE ***'.
           05  FILLER                  PIC X(42)  VALUE SPACES.
      *
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-TRADE  THRU 2000-EXIT
               UNTIL WS-END-OF-FILE
           PERFORM 3000-END-OF-REPORT  THRU 3000-EXIT
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE ZERO                   TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'TRADE BLOTTER STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM UNTIL WS-PARM-EOF
                   READ PARMCARD
                       AT END
                           SET WS-PARM-EOF TO TRUE
                       NOT AT END
                           PERFORM 1100-APPLY-PARM THRU 1100-EXIT
                   END-READ
               END-PERFORM
               CLOSE PARMCARD
           END-IF
      *
           OPEN INPUT BLOTIN-FILE
           IF NOT BLOTIN-OK
               MOVE 'BLOTIN'           TO AB-DDNAME
               MOVE WS-BLOTIN-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
      *
           PERFORM VARYING WS-LVL FROM 1 BY 1 UNTIL WS-LVL > 6
               PERFORM 6900-CLEAR-LEVEL THRU 6900-EXIT
           END-PERFORM
      *
           MOVE 'TCR510'               TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'DAILY TRADE BLOTTER BY BRANCH / REP / ACCOUNT'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-MM              TO WS-ED-MM
           MOVE DC-BUS-DD              TO WS-ED-DD
           MOVE DC-BUS-CCYY            TO WS-ED-CCYY
           MOVE WS-EDIT-DATE           TO RPT-H2-BUS-DATE
           MOVE DC-CAL-DATE(5:2)       TO WS-ED-MM
           MOVE DC-CAL-DATE(7:2)       TO WS-ED-DD
           MOVE DC-CAL-DATE(1:4)       TO WS-ED-CCYY
           MOVE WS-EDIT-DATE           TO RPT-H1-RUN-DATE
      *
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-APPLY-PARM.
           IF PARM-CARD-REC(1:1) = '*'
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'HOUSE-SECTION'
                   MOVE WS-PARM-VALUE(1:1) TO WS-HOUSE-OPT-SW
               WHEN 'SHOW-CANCELS'
                   MOVE WS-PARM-VALUE(1:1) TO WS-SHOW-CANCEL-SW
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - CONTROL BREAKS                                          *
      *================================================================*
       2000-PROCESS-TRADE.
           EVALUATE TRUE
               WHEN WS-FIRST-RECORD
                   MOVE 'N'            TO WS-FIRST-SW
                   PERFORM 2100-START-BRANCH THRU 2100-EXIT
                   PERFORM 2200-START-REP THRU 2200-EXIT
                   PERFORM 2300-START-ACCOUNT THRU 2300-EXIT
               WHEN TRD-BRANCH NOT = WS-SAVE-BRANCH
                   PERFORM 2530-END-ACCOUNT THRU 2530-EXIT
                   PERFORM 2520-END-REP THRU 2520-EXIT
                   PERFORM 2510-END-BRANCH THRU 2510-EXIT
                   PERFORM 2100-START-BRANCH THRU 2100-EXIT
                   PERFORM 2200-START-REP THRU 2200-EXIT
                   PERFORM 2300-START-ACCOUNT THRU 2300-EXIT
               WHEN TRD-REP NOT = WS-SAVE-REP
                   PERFORM 2530-END-ACCOUNT THRU 2530-EXIT
                   PERFORM 2520-END-REP THRU 2520-EXIT
                   PERFORM 2200-START-REP THRU 2200-EXIT
                   PERFORM 2300-START-ACCOUNT THRU 2300-EXIT
               WHEN TRD-ACCT-NO NOT = WS-SAVE-ACCT
                   PERFORM 2530-END-ACCOUNT THRU 2530-EXIT
                   PERFORM 2300-START-ACCOUNT THRU 2300-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           PERFORM 2400-DETAIL-LINE    THRU 2400-EXIT
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-START-BRANCH.
           MOVE TRD-BRANCH             TO WS-SAVE-BRANCH
           ADD 1                       TO WS-BRANCHES
           MOVE WS-LVL-BRANCH          TO WS-LVL
           PERFORM 6900-CLEAR-LEVEL    THRU 6900-EXIT
           MOVE 99                     TO RPT-LINE-COUNT
           PERFORM 7000-CHECK-PAGE     THRU 7000-EXIT.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2200 - NEW REP.  HOUSE ACCOUNTS ARE EXPECTED FIRST             *
      *        (FIRM ACCTS SORT LOW) SO THE SECTION STARTS AT 'F'.     *
      *----------------------------------------------------------------*
       2200-START-REP.
           MOVE TRD-REP                TO WS-SAVE-REP
           ADD 1                       TO WS-REPS
           MOVE WS-LVL-REP             TO WS-LVL
           PERFORM 6900-CLEAR-LEVEL    THRU 6900-EXIT
           MOVE WS-LVL-HOUSE           TO WS-LVL
           PERFORM 6900-CLEAR-LEVEL    THRU 6900-EXIT
           MOVE WS-LVL-CLIENT          TO WS-LVL
           PERFORM 6900-CLEAR-LEVEL    THRU 6900-EXIT
           IF WS-HOUSE-SECTION-ON
               SET WS-IN-HOUSE-SECTION TO TRUE
           ELSE
               SET WS-IN-CLIENT-SECTION TO TRUE
           END-IF
           MOVE 'N'                    TO WS-HOUSE-HDR-SW
           PERFORM 7000-CHECK-PAGE     THRU 7000-EXIT
           MOVE WS-SAVE-REP            TO RPT-RP-REP
           MOVE RPT-REP-LINE           TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           ADD 2                       TO RPT-LINE-COUNT.
       2200-EXIT.
           EXIT.
      *
       2300-START-ACCOUNT.
           IF WS-IN-HOUSE-SECTION
               IF TRD-ACCT-NO(1:1) IS NUMERIC
                   PERFORM 2350-END-HOUSE-SECTION THRU 2350-EXIT
               ELSE
                   IF NOT WS-HOUSE-HDR-PRINTED
                       PERFORM 7000-CHECK-PAGE THRU 7000-EXIT
                       MOVE '--- HOUSE ACCOUNTS (FIRM / STREET) ---'
                                       TO RPT-SC-TEXT
                       MOVE RPT-SECTION-LINE TO RPT-LINE
                       PERFORM 7100-WRITE-LINE THRU 7100-EXIT
                       ADD 1           TO RPT-LINE-COUNT
                       SET WS-HOUSE-HDR-PRINTED TO TRUE
                   END-IF
               END-IF
           END-IF
           MOVE TRD-ACCT-NO            TO WS-SAVE-ACCT
           MOVE TRD-ACCT-TYPE          TO WS-SAVE-ACCT-TYPE
           ADD 1                       TO WS-ACCOUNTS
           MOVE WS-LVL-ACCOUNT         TO WS-LVL
           PERFORM 6900-CLEAR-LEVEL    THRU 6900-EXIT
           IF RPT-LINE-COUNT + 4 > RPT-LINES-PER-PAGE
               MOVE 99                 TO RPT-LINE-COUNT
           END-IF
           PERFORM 7000-CHECK-PAGE     THRU 7000-EXIT
           MOVE TRD-ACCT-NO            TO RPT-AC-ACCT
           MOVE TRD-ACCT-TYPE          TO RPT-AC-TYPE
           MOVE RPT-ACCT-LINE          TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           MOVE RPT-COLUMN-HDR-1       TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           ADD 3                       TO RPT-LINE-COUNT.
       2300-EXIT.
           EXIT.
      *
      *    FIRST CLIENT ACCOUNT OF THE REP - CLOSE THE HOUSE SECTION
       2350-END-HOUSE-SECTION.
           IF WS-ACC-COUNT (WS-LVL-HOUSE) > ZERO
               MOVE WS-LVL-HOUSE       TO WS-LVL
               MOVE 'HOUSE ACCOUNTS SUBTOTAL' TO WS-TOTAL-LABEL
               MOVE '0'                TO RPT-TT-CC
               PERFORM 6000-PRINT-TOTAL THRU 6000-EXIT
           END-IF
           SET WS-IN-CLIENT-SECTION    TO TRUE
           PERFORM 7000-CHECK-PAGE     THRU 7000-EXIT
           MOVE '--- CLIENT ACCOUNTS ---' TO RPT-SC-TEXT
           MOVE RPT-SECTION-LINE       TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       2350-EXIT.
           EXIT.
      *
       2400-DETAIL-LINE.
           ADD 1                       TO WS-RECS-READ
           COMPUTE WS-FEES = TRD-SEC-FEE + TRD-TAF-FEE + TRD-OTHER-FEES
           MOVE TRD-USD-NET-AMOUNT     TO WS-USD-NET
           IF TRD-CCY = 'USD'
           AND WS-USD-NET = ZERO
               MOVE TRD-NET-AMOUNT     TO WS-USD-NET
           END-IF
           MOVE WS-LVL-ACCOUNT         TO WS-LVL
           PERFORM 6800-ACCUMULATE     THRU 6800-EXIT
           IF WS-IN-HOUSE-SECTION
               MOVE WS-LVL-HOUSE       TO WS-LVL
           ELSE
               MOVE WS-LVL-CLIENT      TO WS-LVL
           END-IF
           PERFORM 6800-ACCUMULATE     THRU 6800-EXIT
           IF TRD-CANCEL
           AND NOT WS-SHOW-CANCELS
               GO TO 2400-EXIT
           END-IF
           PERFORM 7000-CHECK-PAGE     THRU 7000-EXIT
           MOVE TRD-ID                 TO RPT-DT-TRADE-ID
           MOVE TRD-TXN-TYPE           TO RPT-DT-TXN
           MOVE TRD-SIDE               TO RPT-DT-SIDE
           MOVE TRD-QTY                TO RPT-DT-QTY
           MOVE TRD-SYMBOL             TO RPT-DT-SYMBOL
           MOVE TRD-PRICE              TO RPT-DT-PRICE
           MOVE TRD-PRINCIPAL          TO RPT-DT-PRINCIPAL
           MOVE TRD-COMMISSION         TO RPT-DT-COMM
           MOVE WS-FEES                TO RPT-DT-FEES
           MOVE TRD-NET-AMOUNT         TO RPT-DT-NET
           MOVE TRD-CCY                TO RPT-DT-CCY
           MOVE TRD-SETTLE-DATE        TO RPT-DT-SETTLE
           MOVE TRD-SOURCE             TO RPT-DT-SOURCE
           MOVE RPT-DETAIL-LINE        TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       2400-EXIT.
           EXIT.
      *
      *================================================================*
      * 2500 - END OF GROUP TOTALS                                     *
      *================================================================*
       2510-END-BRANCH.
           MOVE WS-LVL-BRANCH          TO WS-LVL
           STRING 'BRANCH ' WS-SAVE-BRANCH ' TOTAL'
                  DELIMITED BY SIZE INTO WS-TOTAL-LABEL
           MOVE '-'                    TO RPT-TT-CC
           PERFORM 6000-PRINT-TOTAL    THRU 6000-EXIT
           PERFORM 6700-ROLL-UP        THRU 6700-EXIT.
       2510-EXIT.
           EXIT.
      *
       2520-END-REP.
           IF WS-IN-HOUSE-SECTION
           AND WS-ACC-COUNT (WS-LVL-HOUSE) > ZERO
               MOVE WS-LVL-HOUSE       TO WS-LVL
               MOVE 'HOUSE ACCOUNTS SUBTOTAL' TO WS-TOTAL-LABEL
               MOVE '0'                TO RPT-TT-CC
               PERFORM 6000-PRINT-TOTAL THRU 6000-EXIT
           END-IF
           IF WS-ACC-COUNT (WS-LVL-CLIENT) > ZERO
               MOVE WS-LVL-CLIENT      TO WS-LVL
               MOVE 'CLIENT ACCOUNTS SUBTOTAL' TO WS-TOTAL-LABEL
               MOVE '0'                TO RPT-TT-CC
               PERFORM 6000-PRINT-TOTAL THRU 6000-EXIT
           END-IF
      *    REP TOTAL = HOUSE + CLIENT
           MOVE WS-LVL-HOUSE           TO WS-LVL
           PERFORM 6750-ADD-TO-REP     THRU 6750-EXIT
           MOVE WS-LVL-CLIENT          TO WS-LVL
           PERFORM 6750-ADD-TO-REP     THRU 6750-EXIT
           MOVE WS-LVL-REP             TO WS-LVL
           MOVE SPACES                 TO WS-TOTAL-LABEL
           STRING 'REP ' WS-SAVE-REP ' TOTAL'
                  DELIMITED BY SIZE INTO WS-TOTAL-LABEL
           MOVE '0'                    TO RPT-TT-CC
           PERFORM 6000-PRINT-TOTAL    THRU 6000-EXIT
           PERFORM 6700-ROLL-UP        THRU 6700-EXIT.
       2520-EXIT.
           EXIT.
      *
       2530-END-ACCOUNT.
           MOVE WS-LVL-ACCOUNT         TO WS-LVL
           MOVE SPACES                 TO WS-TOTAL-LABEL
           STRING 'ACCOUNT ' WS-SAVE-ACCT ' TOTAL'
                  DELIMITED BY SIZE INTO WS-TOTAL-LABEL
           MOVE ' '                    TO RPT-TT-CC
           PERFORM 6000-PRINT-TOTAL    THRU 6000-EXIT.
       2530-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - END OF REPORT                                           *
      *================================================================*
       3000-END-OF-REPORT.
           IF WS-RECS-READ = ZERO
               MOVE 99                 TO RPT-LINE-COUNT
               PERFORM 7000-CHECK-PAGE THRU 7000-EXIT
               MOVE RPT-NONE-LINE      TO RPT-LINE
               PERFORM 7100-WRITE-LINE THRU 7100-EXIT
           ELSE
               PERFORM 2530-END-ACCOUNT THRU 2530-EXIT
               PERFORM 2520-END-REP    THRU 2520-EXIT
               PERFORM 2510-END-BRANCH THRU 2510-EXIT
               MOVE WS-LVL-GRAND       TO WS-LVL
               MOVE 'GRAND TOTAL - ALL BRANCHES' TO WS-TOTAL-LABEL
               MOVE '-'                TO RPT-TT-CC
               PERFORM 6000-PRINT-TOTAL THRU 6000-EXIT
           END-IF
           MOVE RPT-END-LINE           TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 6000 - TOTAL LINE FOR LEVEL WS-LVL                             *
      *================================================================*
       6000-PRINT-TOTAL.
           PERFORM 7000-CHECK-PAGE     THRU 7000-EXIT
           MOVE WS-TOTAL-LABEL         TO RPT-TT-LABEL
           MOVE WS-ACC-COUNT (WS-LVL)  TO RPT-TT-COUNT
           MOVE WS-ACC-BUY-USD (WS-LVL) TO RPT-TT-BUY
           MOVE WS-ACC-SELL-USD (WS-LVL) TO RPT-TT-SELL
           MOVE WS-ACC-COMM (WS-LVL)   TO RPT-TT-COMM
           MOVE WS-ACC-CXL (WS-LVL)    TO RPT-TT-CXL
           MOVE RPT-TOTAL-LINE         TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           EVALUATE RPT-TT-CC
               WHEN '0'  ADD 2         TO RPT-LINE-COUNT
               WHEN '-'  ADD 3         TO RPT-LINE-COUNT
               WHEN OTHER ADD 1        TO RPT-LINE-COUNT
           END-EVALUATE
           MOVE SPACES                 TO WS-TOTAL-LABEL.
       6000-EXIT.
           EXIT.
      *
      *    ROLL LEVEL WS-LVL INTO THE NEXT HIGHER (REP > BRANCH > GRAND)
       6700-ROLL-UP.
           ADD WS-ACC-COUNT (WS-LVL)    TO WS-ACC-COUNT (WS-LVL + 1)
           ADD WS-ACC-BUY-USD (WS-LVL)  TO WS-ACC-BUY-USD (WS-LVL + 1)
           ADD WS-ACC-SELL-USD (WS-LVL) TO WS-ACC-SELL-USD (WS-LVL + 1)
           ADD WS-ACC-COMM (WS-LVL)     TO WS-ACC-COMM (WS-LVL + 1)
           ADD WS-ACC-FEES (WS-LVL)     TO WS-ACC-FEES (WS-LVL + 1)
           ADD WS-ACC-CXL (WS-LVL)      TO WS-ACC-CXL (WS-LVL + 1).
       6700-EXIT.
           EXIT.
      *
       6750-ADD-TO-REP.
           ADD WS-ACC-COUNT (WS-LVL)    TO WS-ACC-COUNT (WS-LVL-REP)
           ADD WS-ACC-BUY-USD (WS-LVL)  TO WS-ACC-BUY-USD (WS-LVL-REP)
           ADD WS-ACC-SELL-USD (WS-LVL) TO WS-ACC-SELL-USD (WS-LVL-REP)
           ADD WS-ACC-COMM (WS-LVL)     TO WS-ACC-COMM (WS-LVL-REP)
           ADD WS-ACC-FEES (WS-LVL)     TO WS-ACC-FEES (WS-LVL-REP)
           ADD WS-ACC-CXL (WS-LVL)      TO WS-ACC-CXL (WS-LVL-REP).
       6750-EXIT.
           EXIT.
      *
       6800-ACCUMULATE.
           ADD 1                       TO WS-ACC-COUNT (WS-LVL)
           IF TRD-CANCEL
               ADD 1                   TO WS-ACC-CXL (WS-LVL)
           END-IF
           IF TRD-BUY-SIDE
               ADD WS-USD-NET          TO WS-ACC-BUY-USD (WS-LVL)
           ELSE
               ADD WS-USD-NET          TO WS-ACC-SELL-USD (WS-LVL)
           END-IF
           ADD TRD-COMMISSION          TO WS-ACC-COMM (WS-LVL)
           ADD WS-FEES                 TO WS-ACC-FEES (WS-LVL).
       6800-EXIT.
           EXIT.
      *
       6900-CLEAR-LEVEL.
           MOVE ZERO                   TO WS-ACC-COUNT (WS-LVL)
                                          WS-ACC-BUY-USD (WS-LVL)
                                          WS-ACC-SELL-USD (WS-LVL)
                                          WS-ACC-COMM (WS-LVL)
                                          WS-ACC-FEES (WS-LVL)
                                          WS-ACC-CXL (WS-LVL).
       6900-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - PAGE CONTROL                                            *
      *================================================================*
       7000-CHECK-PAGE.
           IF RPT-LINE-COUNT < RPT-LINES-PER-PAGE
               GO TO 7000-EXIT
           END-IF
           ADD 1                       TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT         TO RPT-H1-PAGE
           MOVE RPT-HEADING-1          TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           MOVE RPT-HEADING-2          TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           MOVE WS-SAVE-BRANCH         TO RPT-BR-BRANCH
           MOVE RPT-BRANCH-LINE        TO RPT-LINE
           PERFORM 7100-WRITE-LINE     THRU 7100-EXIT
           MOVE 5                      TO RPT-LINE-COUNT.
       7000-EXIT.
           EXIT.
      *
       7100-WRITE-LINE.
           WRITE RPT-LINE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE '7100-WRITE-LINE'  TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-LINES-WRITTEN.
       7100-EXIT.
           EXIT.
      *
       8000-READ-TRADE.
           READ BLOTIN-FILE INTO TRD-TRADE-REC
           EVALUATE TRUE
               WHEN BLOTIN-OK
                   CONTINUE
               WHEN BLOTIN-EOF
                   SET WS-END-OF-FILE  TO TRUE
               WHEN OTHER
                   MOVE 'BLOTIN'       TO AB-DDNAME
                   MOVE WS-BLOTIN-FS   TO AB-FILE-STATUS
                   MOVE '8000-READ-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE BLOTIN-FILE
           IF NOT BLOTIN-OK
               MOVE 'BLOTIN'           TO AB-DDNAME
               MOVE WS-BLOTIN-FS       TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'RECORDS-IN'           TO CT-COUNTER-NAME
           MOVE WS-RECS-READ           TO CT-COUNT
           COMPUTE CT-AMOUNT = WS-ACC-BUY-USD (WS-LVL-GRAND)
                             + WS-ACC-SELL-USD (WS-LVL-GRAND)
           MOVE ZERO                   TO CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'TRADE BLOTTER ENDED. PAGES ' RPT-H1-PAGE
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY 'TCR510 - TRADE BLOTTER ' DC-BUS-DATE
           DISPLAY 'TCR510 - TRADES LISTED  ' WS-RECS-READ
           DISPLAY 'TCR510 - BRANCHES       ' WS-BRANCHES
           DISPLAY 'TCR510 - REPS           ' WS-REPS
           DISPLAY 'TCR510 - ACCOUNTS       ' WS-ACCOUNTS
           DISPLAY 'TCR510 - PAGES          ' RPT-PAGE-COUNT
           DISPLAY 'TCR510 - LINES          ' WS-LINES-WRITTEN.
       9000-EXIT.
           EXIT.
      *
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
           MOVE '9000-TERMINATE'       TO AB-PARAGRAPH
           STRING 'CLOSE FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9930-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'TCR510 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'TCR510 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
