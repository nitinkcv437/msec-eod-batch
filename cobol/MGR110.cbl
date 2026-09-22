       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGR110.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  OCTOBER 2003.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGR110                                            *
      * DESCRIPTION: DAILY MARGIN REQUIREMENT REPORT.                  *
      *              ONE LINE PER MARGIN ACCOUNT, GROUPED BY BRANCH    *
      *              AND REGISTERED REP.  WITHIN A REP THE ACCOUNTS IN *
      *              CALL STATUS COME FIRST (LARGEST DEFICIT FIRST),   *
      *              FOLLOWED BY THE ACCOUNTS IN GOOD ORDER.  REP,     *
      *              BRANCH AND FIRM TOTALS; STATUS SUMMARY.           *
      *              THE INPUT IS MG.REQ SORTED BY MSMGD010 STEP020    *
      *              (BRANCH, REP, STATUS DESCENDING, EXCESS, ACCOUNT).*
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD010 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              REQIN    - MSEC.PROD.MG.REQ.SORTED(+1)     (MGREQ)*
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2003-10-20 KAP  ORIGINAL - REPLACES MGB100 SYSOUT     CHG11702 *
      *                 LISTING                                        *
      * 2009-12-14 SPA  USD COLUMNS                           CHG19002 *
      * 2011-02-28 SPA  CONCENTRATION COLUMN                  CHG21340 *
      * 2011-08-29 SPA  STALE PRICE MARKER                    CHG22190 *
      * 2019-02-25 MHC  CONTROL TOTALS                        CHG33410 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT REQIN-FILE     ASSIGN TO REQIN
                  FILE STATUS IS WS-REQIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  REQIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REQIN-REC                   PIC X(250).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGR110'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-REQIN-STATUS         PIC X(02)  VALUE '00'.
               88  REQIN-OK                       VALUE '00'.
               88  REQIN-EOF                      VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-RECORD                   VALUE 'Y'.
           05  WS-GOOD-HEAD-SW         PIC X(01)  VALUE 'N'.
               88  GOOD-ORDER-HEAD-DONE           VALUE 'Y'.
       01  WS-PREV-KEYS.
           05  WS-PREV-BRANCH          PIC X(03)  VALUE LOW-VALUES.
           05  WS-PREV-REP             PIC X(04)  VALUE LOW-VALUES.
      *----------------------------------------------------------------*
      * TOTAL LEVELS  1 = REP   2 = BRANCH   3 = FIRM                  *
      *----------------------------------------------------------------*
       01  WS-TOTAL-TABLE.
           05  WS-TOT OCCURS 3 TIMES.
               10  WS-T-ACCTS          PIC S9(07)       COMP-3.
               10  WS-T-CALLS          PIC S9(07)       COMP-3.
               10  WS-T-LONG           PIC S9(15)V99    COMP-3.
               10  WS-T-SHORT          PIC S9(15)V99    COMP-3.
               10  WS-T-CASH           PIC S9(15)V99    COMP-3.
               10  WS-T-EQUITY         PIC S9(15)V99    COMP-3.
               10  WS-T-HOUSE          PIC S9(15)V99    COMP-3.
               10  WS-T-REGT           PIC S9(15)V99    COMP-3.
               10  WS-T-DEFICIT        PIC S9(15)V99    COMP-3.
       01  WS-LVL                      PIC S9(04) COMP  VALUE ZERO.
       01  WS-TOTAL-NAMES.
           05  FILLER                  PIC X(12)  VALUE 'REP TOTAL'.
           05  FILLER                  PIC X(12)  VALUE 'BRANCH TOTAL'.
           05  FILLER                  PIC X(12)  VALUE 'FIRM TOTAL'.
       01  WS-TOTAL-NAME-TABLE REDEFINES WS-TOTAL-NAMES.
           05  WS-TOTAL-NAME OCCURS 3 TIMES PIC X(12).
      *----------------------------------------------------------------*
      * STATUS SUMMARY                                                 *
      *----------------------------------------------------------------*
       01  WS-STATUS-VALUES.
           05  FILLER  PIC X(21)  VALUE 'HHOUSE MAINTENANCE   '.
           05  FILLER  PIC X(21)  VALUE 'TREG T               '.
           05  FILLER  PIC X(21)  VALUE 'MMINIMUM EQUITY      '.
           05  FILLER  PIC X(21)  VALUE 'GGOOD ORDER          '.
       01  WS-STATUS-TABLE REDEFINES WS-STATUS-VALUES.
           05  WS-ST-ENTRY OCCURS 4 TIMES INDEXED BY ST-IDX.
               10  WS-ST-CODE          PIC X(01).
               10  WS-ST-NAME          PIC X(20).
       01  WS-STATUS-TOTALS.
           05  WS-STT OCCURS 5 TIMES.
               10  WS-STT-COUNT        PIC S9(07)       COMP-3.
               10  WS-STT-EQUITY       PIC S9(15)V99    COMP-3.
               10  WS-STT-DEFICIT      PIC S9(15)V99    COMP-3.
       01  WS-SUB                      PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
       01  WS-WORK.
           05  WS-HOUSE-TOTAL          PIC S9(15)V99    COMP-3.
           05  WS-DEFICIT              PIC S9(15)V99    COMP-3.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINE-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STALE-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-EQUITY-HASH      PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(13)  VALUE 'ACCOUNT    SP'.
           05  FILLER  PIC X(15)  VALUE '        LONG MV'.
           05  FILLER  PIC X(15)  VALUE '       SHORT MV'.
           05  FILLER  PIC X(15)  VALUE '     CASH (USD)'.
           05  FILLER  PIC X(15)  VALUE '         EQUITY'.
           05  FILLER  PIC X(15)  VALUE '   HOUSE + CONC'.
           05  FILLER  PIC X(15)  VALUE '      REG T REQ'.
           05  FILLER  PIC X(15)  VALUE '   EXCESS/(DEF)'.
           05  FILLER  PIC X(07)  VALUE ' ISSUER'.
           05  FILLER  PIC X(04)  VALUE '   %'.
           05  FILLER  PIC X(03)  VALUE 'POS'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(13)  VALUE '---------- --'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(07)  VALUE ' ------'.
           05  FILLER  PIC X(04)  VALUE ' ---'.
           05  FILLER  PIC X(03)  VALUE '---'.
       01  WS-BRANCH-LINE.
           05  BR-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(08)  VALUE ' BRANCH '.
           05  BR-BRANCH               PIC X(03).
           05  FILLER                  PIC X(07)  VALUE '  REP  '.
           05  BR-REP                  PIC X(04).
           05  FILLER                  PIC X(110) VALUE SPACES.
       01  WS-SUBHEAD-LINE.
           05  SH-CC                   PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  SH-TEXT                 PIC X(40).
           05  FILLER                  PIC X(90)  VALUE SPACES.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(01).
           05  DL-STALE                PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-LONG                 PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-SHORT                PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-CASH                 PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-EQUITY               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-HOUSE                PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-REGT                 PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-EXCESS               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-ISSUER               PIC X(06).
           05  FILLER                  PIC X(01).
           05  DL-CONC-PCT             PIC ZZ9.
           05  DL-POSNS                PIC ZZ9.
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  TL-NAME                 PIC X(13).
           05  FILLER                  PIC X(01).
           05  TL-LONG                 PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-SHORT                PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-CASH                 PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-EQUITY               PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-HOUSE                PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-REGT                 PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-DEFICIT              PIC -ZZZZZZZZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-ACCTS                PIC ZZZZ9.
           05  FILLER                  PIC X(01).
           05  TL-CALLS                PIC ZZZZ9.
           05  FILLER                  PIC X(02).
       01  WS-TOTAL-NOTE-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(16)  VALUE SPACES.
           05  FILLER                  PIC X(50)  VALUE
               '(DEFICIT COLUMN = SUM OF HOUSE DEFICITS; LAST TWO '.
           05  FILLER                  PIC X(66)  VALUE
               'COLUMNS = ACCOUNTS, ACCOUNTS IN CALL)'.
       01  WS-SUMMARY-HEAD.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(44)  VALUE
               ' SUMMARY BY MARGIN STATUS                ACC'.
           05  FILLER                  PIC X(88)  VALUE
               'OUNTS          EQUITY USD      HOUSE DEFICIT'.
       01  WS-SUMMARY-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  SM-CODE                 PIC X(01).
           05  FILLER                  PIC X(02).
           05  SM-NAME                 PIC X(20).
           05  FILLER                  PIC X(12).
           05  SM-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  SM-EQUITY               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  SM-DEFICIT              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(45).
       01  WS-STALE-NOTE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  FILLER                  PIC X(40)  VALUE
               'ACCOUNTS VALUED WITH STALE PRICES (P=*):'.
           05  SN-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(82)  VALUE SPACES.
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO MARGIN ACCOUNTS ON THE REQUIREMENT FILE ***'.
       COPY MGREQ.
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
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-RECORD UNTIL END-OF-INPUT.
           IF NOT FIRST-RECORD
               MOVE 1 TO WS-LVL
               PERFORM 3000-PRINT-TOTAL
               MOVE 2 TO WS-LVL
               PERFORM 3000-PRINT-TOTAL
           END-IF.
           MOVE 3 TO WS-LVL.
           PERFORM 3000-PRINT-TOTAL.
           PERFORM 4000-SUMMARY.
           PERFORM 9000-TERMINATE.
           MOVE ZERO TO RETURN-CODE.
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
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MARGIN REQUIREMENT REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT REQIN-FILE.
           IF WS-REQIN-STATUS NOT = '00'
               MOVE 'REQIN' TO AB-DDNAME
               MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           INITIALIZE WS-TOTAL-TABLE WS-STATUS-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'DAILY MARGIN REQUIREMENT BY BRANCH / REP'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS.
           PERFORM 8000-READ-REQ.
           IF END-OF-INPUT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF FIRST-RECORD
               MOVE 'N' TO WS-FIRST-SW
               PERFORM 2100-NEW-BRANCH-REP
           ELSE
               IF MRQ-BRANCH NOT = WS-PREV-BRANCH
                   MOVE 1 TO WS-LVL
                   PERFORM 3000-PRINT-TOTAL
                   MOVE 2 TO WS-LVL
                   PERFORM 3000-PRINT-TOTAL
                   PERFORM 2100-NEW-BRANCH-REP
               ELSE
                   IF MRQ-REP NOT = WS-PREV-REP
                       MOVE 1 TO WS-LVL
                       PERFORM 3000-PRINT-TOTAL
                       PERFORM 2100-NEW-BRANCH-REP
                   END-IF
               END-IF
           END-IF.
           IF MRQ-IN-GOOD-ORDER AND NOT GOOD-ORDER-HEAD-DONE
               MOVE 'ACCOUNTS IN GOOD ORDER' TO SH-TEXT
               PERFORM 2300-SUBHEAD
               MOVE 'Y' TO WS-GOOD-HEAD-SW
           END-IF.
           PERFORM 2200-PRINT-DETAIL.
           PERFORM 8000-READ-REQ.
      *----------------------------------------------------------------*
       2100-NEW-BRANCH-REP.
      *----------------------------------------------------------------*
           MOVE MRQ-BRANCH TO WS-PREV-BRANCH BR-BRANCH.
           MOVE MRQ-REP    TO WS-PREV-REP BR-REP.
           MOVE 'N'        TO WS-GOOD-HEAD-SW.
           IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-BRANCH-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
           IF NOT MRQ-IN-GOOD-ORDER
               MOVE 'ACCOUNTS IN CALL STATUS' TO SH-TEXT
               PERFORM 2300-SUBHEAD
           END-IF.
      *----------------------------------------------------------------*
       2200-PRINT-DETAIL.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           COMPUTE WS-HOUSE-TOTAL = MRQ-HOUSE-REQ + MRQ-CONC-ADDON.
           MOVE SPACES              TO WS-DETAIL-LINE.
           MOVE ' '                 TO DL-CC.
           MOVE MRQ-ACCT-NO         TO DL-ACCT.
           MOVE MRQ-STATUS          TO DL-STATUS.
           IF MRQ-PRICE-STALE-FLAG = 'Y'
               MOVE '*' TO DL-STALE
               ADD 1 TO WS-STALE-CNT
           END-IF.
           MOVE MRQ-LONG-MV         TO DL-LONG.
           MOVE MRQ-SHORT-MV        TO DL-SHORT.
           MOVE MRQ-CASH-BALANCE    TO DL-CASH.
           MOVE MRQ-EQUITY          TO DL-EQUITY.
           MOVE WS-HOUSE-TOTAL      TO DL-HOUSE.
           MOVE MRQ-REGT-REQ        TO DL-REGT.
           MOVE MRQ-EXCESS          TO DL-EXCESS.
           MOVE MRQ-LARGEST-ISSUER  TO DL-ISSUER.
           MOVE MRQ-LARGEST-PCT     TO DL-CONC-PCT.
           MOVE MRQ-POSITION-COUNT  TO DL-POSNS.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT WS-LINE-CNT.
           IF MRQ-EXCESS < ZERO
               COMPUTE WS-DEFICIT = MRQ-EXCESS * -1
           ELSE
               MOVE ZERO TO WS-DEFICIT
           END-IF.
           PERFORM VARYING WS-LVL FROM 1 BY 1 UNTIL WS-LVL > 3
               ADD 1                TO WS-T-ACCTS (WS-LVL)
               IF NOT MRQ-IN-GOOD-ORDER
                   ADD 1            TO WS-T-CALLS (WS-LVL)
               END-IF
               ADD MRQ-LONG-MV      TO WS-T-LONG (WS-LVL)
               ADD MRQ-SHORT-MV     TO WS-T-SHORT (WS-LVL)
               ADD MRQ-CASH-BALANCE TO WS-T-CASH (WS-LVL)
               ADD MRQ-EQUITY       TO WS-T-EQUITY (WS-LVL)
               ADD WS-HOUSE-TOTAL   TO WS-T-HOUSE (WS-LVL)
               ADD MRQ-REGT-REQ     TO WS-T-REGT (WS-LVL)
               ADD WS-DEFICIT       TO WS-T-DEFICIT (WS-LVL)
           END-PERFORM.
           SET ST-IDX TO 1.
           SEARCH WS-ST-ENTRY
               AT END
                   MOVE 5 TO WS-SUB
               WHEN WS-ST-CODE (ST-IDX) = MRQ-STATUS
                   SET WS-SUB TO ST-IDX
           END-SEARCH.
           ADD 1          TO WS-STT-COUNT (WS-SUB).
           ADD MRQ-EQUITY TO WS-STT-EQUITY (WS-SUB).
           ADD WS-DEFICIT TO WS-STT-DEFICIT (WS-SUB).
           ADD MRQ-EQUITY TO WS-TOT-EQUITY-HASH.
      *----------------------------------------------------------------*
       2300-SUBHEAD.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT + 2 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-SUBHEAD-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *================================================================*
      * TOTAL LINE FOR LEVEL WS-LVL, THEN RESET THAT LEVEL             *
      *================================================================*
       3000-PRINT-TOTAL.
           IF RPT-LINE-COUNT + 3 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES                  TO WS-TOTAL-LINE.
           IF WS-LVL = 1
               MOVE ' ' TO TL-CC
           ELSE
               MOVE '0' TO TL-CC
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           MOVE WS-TOTAL-NAME (WS-LVL)  TO TL-NAME.
           MOVE WS-T-LONG (WS-LVL)      TO TL-LONG.
           MOVE WS-T-SHORT (WS-LVL)     TO TL-SHORT.
           MOVE WS-T-CASH (WS-LVL)      TO TL-CASH.
           MOVE WS-T-EQUITY (WS-LVL)    TO TL-EQUITY.
           MOVE WS-T-HOUSE (WS-LVL)     TO TL-HOUSE.
           MOVE WS-T-REGT (WS-LVL)      TO TL-REGT.
           MOVE WS-T-DEFICIT (WS-LVL)   TO TL-DEFICIT.
           MOVE WS-T-ACCTS (WS-LVL)     TO TL-ACCTS.
           MOVE WS-T-CALLS (WS-LVL)     TO TL-CALLS.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
           IF WS-LVL = 3
               WRITE RPT-RECORD FROM WS-TOTAL-NOTE-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           INITIALIZE WS-TOT (WS-LVL).
      *================================================================*
       4000-SUMMARY.
      *================================================================*
           IF RPT-LINE-COUNT + 12 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-SUMMARY-HEAD.
           PERFORM 8900-CHECK-WRITE.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4
               MOVE SPACES TO WS-SUMMARY-LINE
               IF WS-SUB = 1
                   MOVE '0' TO SM-CC
               ELSE
                   MOVE ' ' TO SM-CC
               END-IF
               MOVE WS-ST-CODE (WS-SUB)     TO SM-CODE
               MOVE WS-ST-NAME (WS-SUB)     TO SM-NAME
               MOVE WS-STT-COUNT (WS-SUB)   TO SM-COUNT
               MOVE WS-STT-EQUITY (WS-SUB)  TO SM-EQUITY
               MOVE WS-STT-DEFICIT (WS-SUB) TO SM-DEFICIT
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE
           END-PERFORM.
           IF WS-STT-COUNT (5) > ZERO
               MOVE SPACES TO WS-SUMMARY-LINE
               MOVE ' '    TO SM-CC
               MOVE '?'    TO SM-CODE
               MOVE 'UNKNOWN STATUS' TO SM-NAME
               MOVE WS-STT-COUNT (5)   TO SM-COUNT
               MOVE WS-STT-EQUITY (5)  TO SM-EQUITY
               MOVE WS-STT-DEFICIT (5) TO SM-DEFICIT
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE
           END-IF.
           MOVE WS-STALE-CNT TO SN-COUNT.
           WRITE RPT-RECORD FROM WS-STALE-NOTE.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
       8000-READ-REQ.
      *================================================================*
           READ REQIN-FILE INTO MRQ-REQUIREMENT-REC.
           EVALUATE TRUE
               WHEN REQIN-OK
                   CONTINUE
               WHEN REQIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'REQIN' TO AB-DDNAME
                   MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE.
           MOVE 5 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE.
      *----------------------------------------------------------------*
           MOVE WS-DI-CCYY TO WS-DE-CCYY.
           MOVE WS-DI-MM   TO WS-DE-MM.
           MOVE WS-DI-DD   TO WS-DE-DD.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE REQIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'MGR110'        TO CT-STAGE.
           MOVE 'REQ-IN'        TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-TOT-EQUITY-HASH TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'MGR110 REQUIREMENTS READ   : ' WS-READ-CNT.
           DISPLAY 'MGR110 DETAIL LINES        : ' WS-LINE-CNT.
           DISPLAY 'MGR110 STALE ACCOUNTS      : ' WS-STALE-CNT.
           DISPLAY 'MGR110 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'MARGIN REQUIREMENT REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'MGR110 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
