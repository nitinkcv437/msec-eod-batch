       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGR410.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  NOVEMBER 2015.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGR410                                            *
      * DESCRIPTION: MONTHLY MARGIN ACCOUNT SUMMARY REPORT.            *
      *              ONE LINE PER MARGIN ACCOUNT BY BRANCH / REP:      *
      *              MONTH-END STATUS AND EQUITY, DEBIT, AVERAGE DEBIT,*
      *              INTEREST CHARGED, CALL ACTIVITY, WATCH FLAG.      *
      *              BRANCH TOTALS, FIRM TOTAL AND A PROOF OF THE      *
      *              DETAIL AGAINST THE MGB400 TOTAL RECORD.           *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGM010 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              STMTIN   - MSEC.PROD.MG.MSTMT.SORTED(+1)  (MGMSTM)*
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  DETAIL DOES NOT AGREE WITH THE TOTAL RECORD    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2015-11-30 SPA  ORIGINAL                              CHG28844 *
      * 2018-07-30 MHC  WATCH COLUMN                          CHG32655 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT STMTIN-FILE    ASSIGN TO STMTIN
                  FILE STATUS IS WS-STMTIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  STMTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTIN-REC                  PIC X(200).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGR410'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-STMTIN-STATUS        PIC X(02)  VALUE '00'.
               88  STMTIN-OK                      VALUE '00'.
               88  STMTIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-ACCOUNT                  VALUE 'Y'.
           05  WS-TOTAL-SW             PIC X(01)  VALUE 'N'.
               88  TOTAL-RECORD-SEEN              VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-PREV-BRANCH              PIC X(03)  VALUE LOW-VALUES.
      *----------------------------------------------------------------*
      * 1 = BRANCH   2 = FIRM (FROM DETAIL)                            *
      *----------------------------------------------------------------*
       01  WS-TOTAL-TABLE.
           05  WS-TOT OCCURS 2 TIMES.
               10  WS-T-ACCTS          PIC S9(07)       COMP-3.
               10  WS-T-EQUITY         PIC S9(15)V99    COMP-3.
               10  WS-T-DEBIT          PIC S9(15)V99    COMP-3.
               10  WS-T-AVG-DEBIT      PIC S9(15)V99    COMP-3.
               10  WS-T-INTEREST       PIC S9(13)V99    COMP-3.
               10  WS-T-ISSUED         PIC S9(07)       COMP-3.
               10  WS-T-MET            PIC S9(07)       COMP-3.
               10  WS-T-LIQ            PIC S9(07)       COMP-3.
               10  WS-T-OPEN           PIC S9(07)       COMP-3.
               10  WS-T-WATCH          PIC S9(07)       COMP-3.
       01  WS-LVL                      PIC S9(04) COMP  VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINE-CNT             PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-MONTH-NAMES.
           05  FILLER  PIC X(18) VALUE 'JANFEBMARAPRMAYJUN'.
           05  FILLER  PIC X(18) VALUE 'JULAUGSEPOCTNOVDEC'.
       01  WS-MONTH-TABLE REDEFINES WS-MONTH-NAMES.
           05  WS-MONTH-NAME OCCURS 12 TIMES PIC X(03).
       01  WS-MONTH-WORK               PIC 9(06).
       01  WS-MONTH-WORK-R REDEFINES WS-MONTH-WORK.
           05  WS-MW-CCYY              PIC 9(04).
           05  WS-MW-MM                PIC 9(02).
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
           05  FILLER  PIC X(32)  VALUE 'ACCOUNT    NAME'.
           05  FILLER  PIC X(04)  VALUE 'S W'.
           05  FILLER  PIC X(16)  VALUE '          EQUITY'.
           05  FILLER  PIC X(15)  VALUE '   DEBIT (EOM)'.
           05  FILLER  PIC X(15)  VALUE '     AVG DEBIT'.
           05  FILLER  PIC X(05)  VALUE ' DAYS'.
           05  FILLER  PIC X(13)  VALUE '     INTEREST'.
           05  FILLER  PIC X(02)  VALUE ' F'.
           05  FILLER  PIC X(30)  VALUE
               '  CALLS ISS MET LIQ OPN EXT TY'.
       01  WS-BRANCH-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(08)  VALUE ' BRANCH '.
           05  BL-BRANCH               PIC X(03).
           05  FILLER                  PIC X(121) VALUE SPACES.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-NAME                 PIC X(20).
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-WATCH                PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-EQUITY               PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-DEBIT                PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-AVG-DEBIT            PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-DAYS                 PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-INTEREST             PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-INT-FLAG             PIC X(01).
           05  FILLER                  PIC X(08).
           05  DL-ISSUED               PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-MET                  PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-LIQ                  PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-OPEN                 PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-EXT                  PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-CALL-TYPE            PIC X(02).
           05  FILLER                  PIC X(02).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  TL-NAME                 PIC X(20).
           05  TL-ACCTS                PIC ZZ,ZZ9.
           05  FILLER                  PIC X(05).
           05  TL-EQUITY               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  TL-DEBIT                PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  TL-AVG-DEBIT            PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-INTEREST             PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(07).
           05  TL-ISSUED               PIC ZZZ9.
           05  TL-MET                  PIC ZZZ9.
           05  TL-LIQ                  PIC ZZZ9.
           05  TL-OPEN                 PIC ZZZ9.
           05  FILLER                  PIC X(02).
           05  TL-WATCH                PIC ZZZ9.
           05  FILLER                  PIC X(05).
       01  WS-PROOF-LINE.
           05  PF-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  PF-TEXT                 PIC X(60).
           05  FILLER                  PIC X(70)  VALUE SPACES.
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO MONTHLY SUMMARY - NOT A MONTH-END CYCLE ***'.
       COPY MGMSTM.
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
           IF NOT FIRST-ACCOUNT
               MOVE 1 TO WS-LVL
               PERFORM 3000-PRINT-TOTAL
           END-IF.
           IF WS-T-ACCTS (2) = ZERO
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
           END-IF.
           MOVE 2 TO WS-LVL.
           PERFORM 3000-PRINT-TOTAL.
           PERFORM 4000-PROOF.
           PERFORM 9000-TERMINATE.
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
           MOVE 'MONTHLY MARGIN REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT STMTIN-FILE.
           IF WS-STMTIN-STATUS NOT = '00'
               MOVE 'STMTIN' TO AB-DDNAME
               MOVE WS-STMTIN-STATUS TO AB-FILE-STATUS
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
           INITIALIZE WS-TOTAL-TABLE.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE DC-BUS-DATE (1:6) TO WS-MONTH-WORK.
           STRING 'MONTHLY MARGIN ACCOUNT SUMMARY - '
                  WS-MONTH-NAME (WS-MW-MM) ' ' WS-MW-CCYY
                  DELIMITED BY SIZE INTO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           MOVE WS-DI-CCYY     TO WS-DE-CCYY.
           MOVE WS-DI-MM       TO WS-DE-MM.
           MOVE WS-DI-DD       TO WS-DE-DD.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS.
           PERFORM 8000-READ-STMT.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF MMS-TOTAL-REC
               MOVE 'Y' TO WS-TOTAL-SW
           ELSE
               IF FIRST-ACCOUNT
                   MOVE 'N' TO WS-FIRST-SW
                   PERFORM 2100-NEW-BRANCH
               ELSE
                   IF MMS-BRANCH NOT = WS-PREV-BRANCH
                       MOVE 1 TO WS-LVL
                       PERFORM 3000-PRINT-TOTAL
                       PERFORM 2100-NEW-BRANCH
                   END-IF
               END-IF
               PERFORM 2200-PRINT-ACCOUNT
           END-IF.
           PERFORM 8000-READ-STMT.
      *----------------------------------------------------------------*
       2100-NEW-BRANCH.
      *----------------------------------------------------------------*
           MOVE MMS-BRANCH TO WS-PREV-BRANCH BL-BRANCH.
           IF RPT-LINE-COUNT + 5 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-BRANCH-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       2200-PRINT-ACCOUNT.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES                TO WS-DETAIL-LINE.
           MOVE ' '                   TO DL-CC.
           MOVE MMS-ACCT-NO           TO DL-ACCT.
           MOVE MMS-ACCT-NAME         TO DL-NAME.
           MOVE MMS-STATUS            TO DL-STATUS.
           MOVE MMS-WATCH-FLAG        TO DL-WATCH.
           MOVE MMS-EQUITY            TO DL-EQUITY.
           MOVE MMS-DEBIT-BALANCE     TO DL-DEBIT.
           MOVE MMS-AVG-DEBIT         TO DL-AVG-DEBIT.
           MOVE MMS-INT-DAYS          TO DL-DAYS.
           MOVE MMS-INT-CHARGED       TO DL-INTEREST.
           MOVE MMS-INT-FLAG          TO DL-INT-FLAG.
           MOVE MMS-CALLS-ISSUED      TO DL-ISSUED.
           MOVE MMS-CALLS-MET         TO DL-MET.
           MOVE MMS-CALLS-LIQ         TO DL-LIQ.
           MOVE MMS-CALLS-OPEN        TO DL-OPEN.
           MOVE MMS-EXTENSIONS        TO DL-EXT.
           MOVE MMS-LARGEST-CALL-TYPE TO DL-CALL-TYPE.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT WS-LINE-CNT.
           PERFORM VARYING WS-LVL FROM 1 BY 1 UNTIL WS-LVL > 2
               ADD 1                 TO WS-T-ACCTS (WS-LVL)
               ADD MMS-EQUITY        TO WS-T-EQUITY (WS-LVL)
               ADD MMS-DEBIT-BALANCE TO WS-T-DEBIT (WS-LVL)
               ADD MMS-AVG-DEBIT     TO WS-T-AVG-DEBIT (WS-LVL)
               ADD MMS-INT-CHARGED   TO WS-T-INTEREST (WS-LVL)
               ADD MMS-CALLS-ISSUED  TO WS-T-ISSUED (WS-LVL)
               ADD MMS-CALLS-MET     TO WS-T-MET (WS-LVL)
               ADD MMS-CALLS-LIQ     TO WS-T-LIQ (WS-LVL)
               ADD MMS-CALLS-OPEN    TO WS-T-OPEN (WS-LVL)
               IF MMS-ON-WATCH
                   ADD 1 TO WS-T-WATCH (WS-LVL)
               END-IF
           END-PERFORM.
      *================================================================*
       3000-PRINT-TOTAL.
      *================================================================*
           IF RPT-LINE-COUNT + 3 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES TO WS-TOTAL-LINE.
           IF WS-LVL = 1
               MOVE ' ' TO TL-CC
               STRING 'BRANCH ' WS-PREV-BRANCH ' TOTAL'
                      DELIMITED BY SIZE INTO TL-NAME
           ELSE
               MOVE '-' TO TL-CC
               MOVE 'FIRM TOTAL' TO TL-NAME
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
           MOVE WS-T-ACCTS (WS-LVL)     TO TL-ACCTS.
           MOVE WS-T-EQUITY (WS-LVL)    TO TL-EQUITY.
           MOVE WS-T-DEBIT (WS-LVL)     TO TL-DEBIT.
           MOVE WS-T-AVG-DEBIT (WS-LVL) TO TL-AVG-DEBIT.
           MOVE WS-T-INTEREST (WS-LVL)  TO TL-INTEREST.
           MOVE WS-T-ISSUED (WS-LVL)    TO TL-ISSUED.
           MOVE WS-T-MET (WS-LVL)       TO TL-MET.
           MOVE WS-T-LIQ (WS-LVL)       TO TL-LIQ.
           MOVE WS-T-OPEN (WS-LVL)      TO TL-OPEN.
           MOVE WS-T-WATCH (WS-LVL)     TO TL-WATCH.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
           IF WS-LVL = 1
               INITIALIZE WS-TOT (1)
           END-IF.
      *================================================================*
      * PROOF - DETAIL AGAINST THE MGB400 TOTAL RECORD                 *
      *================================================================*
       4000-PROOF.
           IF NOT TOTAL-RECORD-SEEN
               MOVE '*** MGB400 TOTAL RECORD MISSING ***' TO PF-TEXT
               MOVE 4 TO WS-RETURN-CODE
           ELSE
               IF WS-T-ACCTS (2) = WS-READ-CNT - 1
               AND WS-T-INTEREST (2) = MMS-INT-CHARGED
               AND WS-T-ISSUED (2) = MMS-CALLS-ISSUED
                   MOVE 'PROOF: DETAIL AGREES WITH MGB400 TOTAL RECORD'
                                    TO PF-TEXT
               ELSE
                   MOVE '*** PROOF FAILED - DETAIL NOT = MGB400 TOTAL'
                                    TO PF-TEXT
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
       8000-READ-STMT.
      *================================================================*
           READ STMTIN-FILE INTO MMS-MONTHLY-REC.
           EVALUATE TRUE
               WHEN STMTIN-OK
                   CONTINUE
               WHEN STMTIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'STMTIN' TO AB-DDNAME
                   MOVE WS-STMTIN-STATUS TO AB-FILE-STATUS
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
           MOVE 4 TO RPT-LINE-COUNT.
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
           CLOSE STMTIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'MGR410'        TO CT-STAGE.
           MOVE 'MSTMT-IN'      TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-T-INTEREST (2) TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'MGR410 RECORDS READ        : ' WS-READ-CNT.
           DISPLAY 'MGR410 ACCOUNTS PRINTED    : ' WS-LINE-CNT.
           DISPLAY 'MGR410 PAGES               : ' RPT-PAGE-COUNT.
           DISPLAY 'MGR410 RETURN CODE         : ' WS-RETURN-CODE.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'MONTHLY MARGIN REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'MGR410 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
