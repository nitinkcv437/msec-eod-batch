       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGR210.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 2004.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGR210                                            *
      * DESCRIPTION: MARGIN CALL REPORT.                               *
      *              PRINTS THE DAY'S CALL EVENTS FROM MGB200 IN FIVE  *
      *              SECTIONS - ONE PASS OF THE EVENT FILE PER SECTION:*
      *                1  NEW CALLS ISSUED TODAY                       *
      *                2  OPEN CALLS (AGED / EXTENDED)                 *
      *                3  CALLS MET                                    *
      *                4  LIQUIDATION (PAST DUE)                       *
      *                5  CANCELLED CALLS / REJECTED EXTENSIONS        *
      *              SECTION TOTALS AND A SUMMARY BY CALL TYPE.        *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD020 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              EVTIN    - MSEC.PROD.MG.CALLEVT(+1)       (MGCEVT)*
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2004-03-15 KAP  ORIGINAL                              CHG12230 *
      * 2004-09-27 KAP  CANCELLED CALLS SECTION               CHG12661 *
      * 2018-07-30 MHC  LIQUIDATED CALLS STILL OPEN SHOWN IN  CHG32655 *
      *                 SECTION 4 UNTIL MET                            *
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
           SELECT EVTIN-FILE     ASSIGN TO EVTIN
                  FILE STATUS IS WS-EVTIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  EVTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGCEVT.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGR210'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-EVTIN-STATUS         PIC X(02)  VALUE '00'.
               88  EVTIN-OK                       VALUE '00'.
               88  EVTIN-EOF                      VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-EVENTS                  VALUE 'Y'.
           05  WS-SELECT-SW            PIC X(01)  VALUE 'N'.
               88  EVENT-SELECTED                 VALUE 'Y'.
      *----------------------------------------------------------------*
      * SECTIONS                                                       *
      *----------------------------------------------------------------*
       01  WS-SECTION-NAMES.
           05  FILLER  PIC X(40)  VALUE
               'SECTION 1 - NEW CALLS ISSUED TODAY'.
           05  FILLER  PIC X(40)  VALUE
               'SECTION 2 - OPEN CALLS'.
           05  FILLER  PIC X(40)  VALUE
               'SECTION 3 - CALLS MET'.
           05  FILLER  PIC X(40)  VALUE
               'SECTION 4 - LIQUIDATION'.
           05  FILLER  PIC X(40)  VALUE
               'SECTION 5 - CANCELLED / EXTENSIONS'.
       01  WS-SECTION-TABLE REDEFINES WS-SECTION-NAMES.
           05  WS-SECTION-NAME OCCURS 5 TIMES PIC X(40).
       01  WS-SECTION                  PIC S9(04) COMP  VALUE ZERO.
       01  WS-SECTION-TOTALS.
           05  WS-SEC-TOT OCCURS 5 TIMES.
               10  WS-ST-COUNT         PIC S9(07)       COMP-3.
               10  WS-ST-CALL-AMT      PIC S9(15)V99    COMP-3.
               10  WS-ST-MET-AMT       PIC S9(15)V99    COMP-3.
               10  WS-ST-DEFICIT       PIC S9(15)V99    COMP-3.
      *----------------------------------------------------------------*
      * CALL TYPE SUMMARY (OPEN + NEW + LIQUIDATION)                   *
      *----------------------------------------------------------------*
       01  WS-TYPE-VALUES.
           05  FILLER  PIC X(22)  VALUE 'HMHOUSE MAINTENANCE   '.
           05  FILLER  PIC X(22)  VALUE 'RTREG T               '.
           05  FILLER  PIC X(22)  VALUE 'MEMINIMUM EQUITY      '.
       01  WS-TYPE-TABLE REDEFINES WS-TYPE-VALUES.
           05  WS-TY-ENTRY OCCURS 3 TIMES INDEXED BY TY-IDX.
               10  WS-TY-CODE          PIC X(02).
               10  WS-TY-NAME          PIC X(20).
       01  WS-TYPE-TOTALS.
           05  WS-TYT OCCURS 3 TIMES.
               10  WS-TYT-ACTIVE       PIC S9(07)       COMP-3.
               10  WS-TYT-AMOUNT       PIC S9(15)V99    COMP-3.
               10  WS-TYT-MET          PIC S9(07)       COMP-3.
       01  WS-SUB                      PIC S9(04) COMP  VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PASS-CNT             PIC S9(04) COMP   VALUE ZERO.
           05  WS-TOT-CALL-HASH        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-CC-YY REDEFINES WS-DI-CCYY.
               10  WS-DI-CC            PIC 9(02).
               10  WS-DI-YY            PIC 9(02).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
       01  WS-SHORT-DATE.
           05  WS-SD-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  WS-SD-DD                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '/'.
           05  WS-SD-YY                PIC 9(02).
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-SECTION-LINE.
           05  SC-CC                   PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  SC-NAME                 PIC X(40).
           05  FILLER                  PIC X(91)  VALUE SPACES.
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(28)  VALUE 'ACCOUNT    BR  REP  TY S'.
           05  FILLER  PIC X(24)  VALUE 'ISSUED   DUE      AGE X'.
           05  FILLER  PIC X(15)  VALUE '    CALL AMOUNT'.
           05  FILLER  PIC X(15)  VALUE '     AMOUNT MET'.
           05  FILLER  PIC X(15)  VALUE '    DEFICIT NOW'.
           05  FILLER  PIC X(15)  VALUE '         EQUITY'.
           05  FILLER  PIC X(20)  VALUE ' REMARKS'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(28)  VALUE '---------- --- ---- -- -'.
           05  FILLER  PIC X(24)  VALUE '-------- -------- --- -'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' --------------'.
           05  FILLER  PIC X(20)  VALUE ' -------------------'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-BRANCH               PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-REP                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-TYPE                 PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(01).
           05  FILLER                  PIC X(04).
           05  DL-ISSUED               PIC X(08).
           05  FILLER                  PIC X(01).
           05  DL-DUE                  PIC X(08).
           05  FILLER                  PIC X(01).
           05  DL-AGE                  PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-EXT                  PIC 9.
           05  FILLER                  PIC X(02).
           05  DL-CALL-AMT             PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-MET-AMT              PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-DEFICIT              PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-EQUITY               PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-REMARKS              PIC X(19).
       01  WS-SECTION-TOTAL-LINE.
           05  STL-CC                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(16)  VALUE
               'SECTION TOTAL  '.
           05  STL-COUNT               PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(07)  VALUE ' CALLS '.
           05  FILLER                  PIC X(12)  VALUE SPACES.
           05  STL-CALL-AMT            PIC -ZZZ,ZZZ,ZZ9.99.
           05  STL-MET-AMT             PIC -ZZZ,ZZZ,ZZ9.99.
           05  STL-DEFICIT             PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(35)  VALUE SPACES.
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NONE ***'.
       01  WS-SUMMARY-HEAD.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(47)  VALUE
               ' SUMMARY BY CALL TYPE            ACTIVE CALLS  '.
           05  FILLER                  PIC X(85)  VALUE
               '      ACTIVE AMOUNT    MET TODAY'.
       01  WS-SUMMARY-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  SM-CODE                 PIC X(02).
           05  FILLER                  PIC X(02).
           05  SM-NAME                 PIC X(20).
           05  FILLER                  PIC X(07).
           05  SM-ACTIVE               PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  SM-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(06).
           05  SM-MET                  PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(56).
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE SECTION.
      *================================================================*
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PRINT-SECTION
               VARYING WS-SECTION FROM 1 BY 1 UNTIL WS-SECTION > 5.
           PERFORM 4000-SUMMARY.
           PERFORM 9000-TERMINATE.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE SECTION.
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
           MOVE 'MARGIN CALL REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           INITIALIZE WS-SECTION-TOTALS WS-TYPE-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'MARGIN CALLS - NEW / OPEN / MET / LIQUIDATION'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS.
       1000-EXIT.
           EXIT.
      *================================================================*
      * ONE PASS OF THE EVENT FILE FOR SECTION WS-SECTION              *
      *================================================================*
       2000-PRINT-SECTION SECTION.
           ADD 1 TO WS-PASS-CNT.
           IF RPT-LINE-COUNT + 8 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE WS-SECTION-NAME (WS-SECTION) TO SC-NAME.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 3 TO RPT-LINE-COUNT.
           OPEN INPUT EVTIN-FILE.
           IF WS-EVTIN-STATUS NOT = '00'
               MOVE 'EVTIN' TO AB-DDNAME
               MOVE WS-EVTIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'N' TO WS-EOF-SW.
           PERFORM 8000-READ-EVENT.
           PERFORM UNTIL END-OF-EVENTS
               IF WS-SECTION = 1
                   ADD 1 TO WS-READ-CNT
               END-IF
               PERFORM 2100-SELECT-EVENT
               IF EVENT-SELECTED
                   PERFORM 2200-PRINT-EVENT
               END-IF
               PERFORM 8000-READ-EVENT
           END-PERFORM.
           CLOSE EVTIN-FILE.
           IF WS-ST-COUNT (WS-SECTION) = ZERO
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 2 TO RPT-LINE-COUNT
           ELSE
               PERFORM 3000-SECTION-TOTAL
           END-IF.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * WHICH EVENTS BELONG TO THE SECTION                             *
      *----------------------------------------------------------------*
       2100-SELECT-EVENT SECTION.
           MOVE 'N' TO WS-SELECT-SW.
           EVALUATE WS-SECTION
               WHEN 1
                   IF MCE-EVT-NEW
                       MOVE 'Y' TO WS-SELECT-SW
                   END-IF
               WHEN 2
                   IF (MCE-EVT-UPDATED OR MCE-EVT-EXTENDED)
                   AND (MCE-STATUS = 'O' OR 'X')
                       MOVE 'Y' TO WS-SELECT-SW
                   END-IF
               WHEN 3
                   IF MCE-EVT-MET
                       MOVE 'Y' TO WS-SELECT-SW
                   END-IF
               WHEN 4
                   IF MCE-EVT-LIQUIDATE
                   OR (MCE-EVT-UPDATED AND MCE-STATUS = 'L')
                       MOVE 'Y' TO WS-SELECT-SW
                   END-IF
               WHEN 5
                   IF MCE-EVT-CANCELLED OR MCE-EVT-EXT-REJECTED
                       MOVE 'Y' TO WS-SELECT-SW
                   END-IF
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PRINT-EVENT SECTION.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
               MOVE WS-SECTION-NAME (WS-SECTION) TO SC-NAME
               WRITE RPT-RECORD FROM WS-SECTION-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 3 TO RPT-LINE-COUNT
           END-IF.
           MOVE SPACES             TO WS-DETAIL-LINE.
           MOVE ' '                TO DL-CC.
           MOVE MCE-ACCT-NO        TO DL-ACCT.
           MOVE MCE-BRANCH         TO DL-BRANCH.
           MOVE MCE-REP            TO DL-REP.
           MOVE MCE-CALL-TYPE      TO DL-TYPE.
           MOVE MCE-STATUS         TO DL-STATUS.
           IF MCE-ISSUE-DATE NUMERIC AND MCE-ISSUE-DATE > ZERO
               MOVE MCE-ISSUE-DATE TO WS-DATE-IN
               PERFORM 8400-SHORT-DATE
               MOVE WS-SHORT-DATE  TO DL-ISSUED
           END-IF.
           IF MCE-DUE-DATE NUMERIC AND MCE-DUE-DATE > ZERO
               MOVE MCE-DUE-DATE   TO WS-DATE-IN
               PERFORM 8400-SHORT-DATE
               MOVE WS-SHORT-DATE  TO DL-DUE
           END-IF.
           MOVE MCE-AGE-BUS-DAYS   TO DL-AGE.
           MOVE MCE-EXTENSION-COUNT TO DL-EXT.
           MOVE MCE-CALL-AMOUNT    TO DL-CALL-AMT.
           MOVE MCE-AMOUNT-MET     TO DL-MET-AMT.
           MOVE MCE-CURR-DEFICIT   TO DL-DEFICIT.
           MOVE MCE-EQUITY         TO DL-EQUITY.
           MOVE MCE-MESSAGE        TO DL-REMARKS.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT WS-PRINT-CNT.
           ADD 1                 TO WS-ST-COUNT (WS-SECTION).
           ADD MCE-CALL-AMOUNT   TO WS-ST-CALL-AMT (WS-SECTION).
           ADD MCE-AMOUNT-MET    TO WS-ST-MET-AMT (WS-SECTION).
           IF MCE-CURR-DEFICIT > ZERO
               ADD MCE-CURR-DEFICIT TO WS-ST-DEFICIT (WS-SECTION)
           END-IF.
           PERFORM 2300-TYPE-TOTALS.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-TYPE-TOTALS SECTION.
      *----------------------------------------------------------------*
           SET TY-IDX TO 1.
           SEARCH WS-TY-ENTRY
               AT END
                   MOVE ZERO TO WS-SUB
               WHEN WS-TY-CODE (TY-IDX) = MCE-CALL-TYPE
                   SET WS-SUB TO TY-IDX
           END-SEARCH.
           IF WS-SUB = ZERO
               GO TO 2300-EXIT
           END-IF.
           EVALUATE WS-SECTION
               WHEN 1 WHEN 2 WHEN 4
                   ADD 1               TO WS-TYT-ACTIVE (WS-SUB)
                   ADD MCE-CALL-AMOUNT TO WS-TYT-AMOUNT (WS-SUB)
                   ADD MCE-CALL-AMOUNT TO WS-TOT-CALL-HASH
               WHEN 3
                   ADD 1               TO WS-TYT-MET (WS-SUB)
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       2300-EXIT.
           EXIT.
      *================================================================*
       3000-SECTION-TOTAL SECTION.
      *================================================================*
           IF RPT-LINE-COUNT + 2 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE WS-ST-COUNT (WS-SECTION)    TO STL-COUNT.
           MOVE WS-ST-CALL-AMT (WS-SECTION) TO STL-CALL-AMT.
           MOVE WS-ST-MET-AMT (WS-SECTION)  TO STL-MET-AMT.
           MOVE WS-ST-DEFICIT (WS-SECTION)  TO STL-DEFICIT.
           WRITE RPT-RECORD FROM WS-SECTION-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-SUMMARY SECTION.
      *================================================================*
           IF RPT-LINE-COUNT + 8 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-SUMMARY-HEAD.
           PERFORM 8900-CHECK-WRITE.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 3
               MOVE SPACES TO WS-SUMMARY-LINE
               IF WS-SUB = 1
                   MOVE '0' TO SM-CC
               ELSE
                   MOVE ' ' TO SM-CC
               END-IF
               MOVE WS-TY-CODE (WS-SUB)    TO SM-CODE
               MOVE WS-TY-NAME (WS-SUB)    TO SM-NAME
               MOVE WS-TYT-ACTIVE (WS-SUB) TO SM-ACTIVE
               MOVE WS-TYT-AMOUNT (WS-SUB) TO SM-AMOUNT
               MOVE WS-TYT-MET (WS-SUB)    TO SM-MET
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE
           END-PERFORM.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
       4000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-EVENT SECTION.
      *================================================================*
           READ EVTIN-FILE.
           EVALUATE TRUE
               WHEN EVTIN-OK
                   CONTINUE
               WHEN EVTIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'EVTIN' TO AB-DDNAME
                   MOVE WS-EVTIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-HEADINGS SECTION.
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
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE SECTION.
      *----------------------------------------------------------------*
           MOVE WS-DI-CCYY TO WS-DE-CCYY.
           MOVE WS-DI-MM   TO WS-DE-MM.
           MOVE WS-DI-DD   TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8400-SHORT-DATE SECTION.
      *----------------------------------------------------------------*
           MOVE WS-DI-MM   TO WS-SD-MM.
           MOVE WS-DI-DD   TO WS-SD-DD.
           MOVE WS-DI-YY   TO WS-SD-YY.
       8400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE SECTION.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE SECTION.
      *================================================================*
           CLOSE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'MGR210'        TO CT-STAGE.
           MOVE 'CALLEVT-IN'    TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-TOT-CALL-HASH TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'MGR210 EVENTS READ         : ' WS-READ-CNT.
           DISPLAY 'MGR210 LINES PRINTED       : ' WS-PRINT-CNT.
           DISPLAY 'MGR210 PASSES OF EVTIN     : ' WS-PASS-CNT.
           DISPLAY 'MGR210 NEW CALLS           : ' WS-ST-COUNT (1).
           DISPLAY 'MGR210 OPEN CALLS          : ' WS-ST-COUNT (2).
           DISPLAY 'MGR210 CALLS MET           : ' WS-ST-COUNT (3).
           DISPLAY 'MGR210 LIQUIDATION         : ' WS-ST-COUNT (4).
           DISPLAY 'MGR210 CANCELLED/EXT       : ' WS-ST-COUNT (5).
           DISPLAY 'MGR210 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'MARGIN CALL REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND SECTION.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'MGR210 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
