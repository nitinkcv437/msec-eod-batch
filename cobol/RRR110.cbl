       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRR110.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 1995.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRR110                                            *
      * DESCRIPTION: CUSTOMER RESERVE COMPUTATION REPORT.              *
      *              PRINTS THE FORMULA LINES WRITTEN BY RRB100 WITH   *
      *              THE PRIOR COMPUTATION AND THE CHANGE, RE-FOOTS    *
      *              THE CREDIT AND DEBIT SECTIONS AGAINST THE TOTAL   *
      *              LINES AND PROVES THE REQUIREMENT AND THE EXCESS / *
      *              DEFICIENCY.  THE SIGNED COPY GOES TO THE FINOP.   *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRRD010 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              RESVIN   - MSEC.PROD.RR.RESERVE(+1)    (RRFORM)   *
      *              PRIORIN  - MSEC.PROD.RR.RESERVE(0)     (RRFORM)   *
      *                         PRIOR COMPUTATION, MAY BE EMPTY        *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0 EXCESS, 4 DEFICIENCY OR NO PRIOR COMPUTATION,   *
      *              8 FORMULA DOES NOT FOOT / PROVE                   *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1995-03-20 DWB  ORIGINAL                              CHG01880 *
      * 1997-06-30 DWB  PRIOR DAY COLUMN (DAILY COMPUTATION)  CHG03115 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2004-09-20 KAP  LINE D03, FOOTING PROOF               CHG12650 *
      * 2019-08-12 MHC  COVERAGE PERCENT, DEPOSIT DEADLINE    CHG34020 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT RESVIN-FILE    ASSIGN TO RESVIN
                  FILE STATUS IS WS-RESVIN-STATUS.
           SELECT PRIORIN-FILE   ASSIGN TO PRIORIN
                  FILE STATUS IS WS-PRIORIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  RESVIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RRFORM.
       FD  PRIORIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PRIORIN-REC                 PIC X(100).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRR110'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-RESVIN-STATUS        PIC X(02)  VALUE '00'.
               88  RESVIN-OK                      VALUE '00'.
               88  RESVIN-EOF                     VALUE '10'.
           05  WS-PRIORIN-STATUS       PIC X(02)  VALUE '00'.
               88  PRIORIN-OK                     VALUE '00'.
               88  PRIORIN-EOF                    VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-FORMULA                 VALUE 'Y'.
           05  WS-PRIOR-EOF-SW         PIC X(01)  VALUE 'N'.
               88  END-OF-PRIOR                   VALUE 'Y'.
           05  WS-PRIOR-SW             PIC X(01)  VALUE 'N'.
               88  PRIOR-AVAILABLE                VALUE 'Y'.
           05  WS-CUR-SECTION          PIC X(01)  VALUE SPACE.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * FORMULA HELD IN CORE - 12 LINES, ROOM FOR 30                   *
      *----------------------------------------------------------------*
       01  WS-LINE-TABLE.
           05  WS-LT-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-LT-ENTRY             OCCURS 30 TIMES.
               10  WS-LT-KEY.
                   15  WS-LT-SECTION   PIC X(01).
                   15  WS-LT-LINE-NO   PIC 9(02).
               10  WS-LT-DESC          PIC X(40).
               10  WS-LT-AMOUNT        PIC S9(15)V99    COMP-3.
               10  WS-LT-COUNT         PIC S9(07)       COMP-3.
               10  WS-LT-PRIOR         PIC S9(15)V99    COMP-3.
               10  WS-LT-PRIOR-SW      PIC X(01).
       01  WS-LT-MAX                   PIC S9(04) COMP  VALUE 30.
       01  WS-SUB                      PIC S9(04) COMP  VALUE ZERO.
       01  WS-FOUND-SUB                PIC S9(04) COMP  VALUE ZERO.
       01  WS-PRIOR-KEY                PIC X(03).
       01  WS-PRIOR-DATE               PIC 9(08)  VALUE ZERO.
       01  WS-FORMULA-DATE             PIC 9(08)  VALUE ZERO.
      *----------------------------------------------------------------*
      * PROOF WORK                                                     *
      *----------------------------------------------------------------*
       01  WS-PROOF-WORK.
           05  WS-FOOT-CREDITS         PIC S9(15)V99    COMP-3.
           05  WS-FOOT-DEBITS          PIC S9(15)V99    COMP-3.
           05  WS-TOTAL-CREDITS        PIC S9(15)V99    COMP-3.
           05  WS-TOTAL-DEBITS         PIC S9(15)V99    COMP-3.
           05  WS-REQUIREMENT          PIC S9(15)V99    COMP-3.
           05  WS-DEPOSIT              PIC S9(15)V99    COMP-3.
           05  WS-EXCESS               PIC S9(15)V99    COMP-3.
           05  WS-CALC-REQ             PIC S9(15)V99    COMP-3.
           05  WS-CALC-EXCESS          PIC S9(15)V99    COMP-3.
           05  WS-CHANGE               PIC S9(15)V99    COMP-3.
           05  WS-COVERAGE-PCT         PIC S9(05)V99    COMP-3.
           05  WS-PROOF-ERRORS         PIC S9(04)       COMP  VALUE 0.
           05  WS-SECTION-TOTAL        PIC S9(15)V99    COMP-3.
           05  WS-SECTION-PRIOR        PIC S9(15)V99    COMP-3.
           05  WS-SECTION-COUNT        PIC S9(07)       COMP-3.
       01  WS-COUNTERS.
           05  WS-LINES-READ           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-PRIOR-READ           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-PRIOR-UNMATCHED      PIC S9(07) COMP-3 VALUE ZERO.
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
       01  WS-SUB-HEAD.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(28)  VALUE SPACES.
           05  FILLER  PIC X(27)  VALUE
               'PRIOR COMPUTATION DATED : '.
           05  SH-PRIOR-DATE       PIC X(10).
           05  FILLER  PIC X(67)  VALUE SPACES.
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(06)  VALUE ' LINE'.
           05  FILLER  PIC X(42)  VALUE 'DESCRIPTION'.
           05  FILLER  PIC X(10)  VALUE '    ITEMS'.
           05  FILLER  PIC X(25)  VALUE
               '          CURRENT'.
           05  FILLER  PIC X(25)  VALUE
               '            PRIOR'.
           05  FILLER  PIC X(24)  VALUE
               '           CHANGE'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(06)  VALUE ' ----'.
           05  FILLER  PIC X(42)  VALUE
               '----------------------------------------'.
           05  FILLER  PIC X(10)  VALUE ' --------'.
           05  FILLER  PIC X(25)  VALUE
               ' -----------------------'.
           05  FILLER  PIC X(25)  VALUE
               ' -----------------------'.
           05  FILLER  PIC X(24)  VALUE
               ' ----------------------'.
       01  WS-SECTION-LINE.
           05  SL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  SL-TEXT                 PIC X(60).
           05  FILLER                  PIC X(71).
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-SECTION              PIC X(01).
           05  DL-LINE-NO              PIC 99.
           05  FILLER                  PIC X(02).
           05  DL-DESC                 PIC X(40).
           05  FILLER                  PIC X(02).
           05  DL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  DL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-PRIOR                PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-CHANGE               PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
       01  WS-PROOF-LINE.
           05  PL-CC                   PIC X(01).
           05  FILLER                  PIC X(04).
           05  PL-LABEL                PIC X(42).
           05  PL-VALUE-1              PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  PL-VALUE-2              PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  PL-RESULT               PIC X(30).
           05  FILLER                  PIC X(04).
       01  WS-BOX-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(10).
           05  BL-TEXT                 PIC X(100).
           05  FILLER                  PIC X(22).
       01  WS-BOX-BORDER.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(80)  VALUE ALL '*'.
           05  FILLER                  PIC X(42)  VALUE SPACES.
       01  WS-MSG-WORK.
           05  WS-MSG-AMOUNT           PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-MSG-PCT              PIC ZZ,ZZ9.99.
           05  WS-MSG-DATE             PIC X(10).
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
       01  WS-PRIOR-FORMULA.
           05  WS-PF-BUS-DATE          PIC 9(08).
           05  WS-PF-SECTION           PIC X(01).
           05  WS-PF-LINE-NO           PIC 9(02).
           05  WS-PF-DESC              PIC X(40).
           05  WS-PF-AMOUNT            PIC S9(15)V99    COMP-3.
           05  WS-PF-ITEM-COUNT        PIC S9(07)       COMP-3.
           05  FILLER                  PIC X(36).
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-LOAD-FORMULA THRU 2000-EXIT
               UNTIL END-OF-FORMULA.
           PERFORM 2500-LOAD-PRIOR THRU 2500-EXIT
               UNTIL END-OF-PRIOR.
           PERFORM 3000-PRINT-FORMULA THRU 3000-EXIT.
           PERFORM 4000-PROOFS THRU 4000-EXIT.
           PERFORM 5000-RESULT-BOX THRU 5000-EXIT.
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
           MOVE 'RESERVE COMPUTATION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT RESVIN-FILE.
           IF WS-RESVIN-STATUS NOT = '00'
               MOVE 'RESVIN' TO AB-DDNAME
               MOVE WS-RESVIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT PRIORIN-FILE.
           IF WS-PRIORIN-STATUS NOT = '00'
               MOVE 'PRIORIN' TO AB-DDNAME
               MOVE WS-PRIORIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           INITIALIZE WS-PROOF-WORK.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'CUSTOMER RESERVE REQUIREMENT - FORMULA COMPUTATION'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-FORMULA THRU 8000-EXIT.
           IF END-OF-FORMULA
               MOVE 'RESVIN' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'NO FORMULA LINES - RRB100 OUTPUT EMPTY'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE RRF-BUS-DATE TO WS-FORMULA-DATE.
           IF RRF-BUS-DATE NOT = DC-BUS-DATE
               DISPLAY 'RRR110 W - FORMULA DATED ' RRF-BUS-DATE
                       ' BUSINESS DATE ' DC-BUS-DATE
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           PERFORM 8100-READ-PRIOR THRU 8100-EXIT.
           IF END-OF-PRIOR
               DISPLAY 'RRR110 W - NO PRIOR COMPUTATION ON FILE'
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           ELSE
               MOVE 'Y' TO WS-PRIOR-SW
               MOVE WS-PF-BUS-DATE TO WS-PRIOR-DATE
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
      * LOAD TODAY'S FORMULA INTO THE LINE TABLE                       *
      *================================================================*
       2000-LOAD-FORMULA.
           ADD 1 TO WS-LINES-READ.
           IF WS-LT-USED NOT < WS-LT-MAX
               MOVE 'RESVIN' TO AB-DDNAME
               MOVE 1007 TO AB-ABEND-CODE
               MOVE '2000-LOAD-FORMULA' TO AB-PARAGRAPH
               MOVE 'MORE THAN 30 FORMULA LINES' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-LT-USED.
           MOVE WS-LT-USED     TO WS-SUB.
           MOVE RRF-SECTION    TO WS-LT-SECTION (WS-SUB).
           MOVE RRF-LINE-NO    TO WS-LT-LINE-NO (WS-SUB).
           MOVE RRF-DESC       TO WS-LT-DESC (WS-SUB).
           MOVE RRF-AMOUNT     TO WS-LT-AMOUNT (WS-SUB).
           MOVE RRF-ITEM-COUNT TO WS-LT-COUNT (WS-SUB).
           MOVE ZERO           TO WS-LT-PRIOR (WS-SUB).
           MOVE 'N'            TO WS-LT-PRIOR-SW (WS-SUB).
           PERFORM 8000-READ-FORMULA THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *================================================================*
      * MATCH THE PRIOR COMPUTATION BY SECTION + LINE NUMBER           *
      *================================================================*
       2500-LOAD-PRIOR.
           ADD 1 TO WS-PRIOR-READ.
           MOVE WS-PF-SECTION TO WS-PRIOR-KEY (1:1).
           MOVE WS-PF-LINE-NO TO WS-PRIOR-KEY (2:2).
           MOVE ZERO TO WS-FOUND-SUB.
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-LT-USED
                      OR WS-FOUND-SUB > ZERO
               IF WS-LT-KEY (WS-SUB) = WS-PRIOR-KEY
                   MOVE WS-SUB TO WS-FOUND-SUB
               END-IF
           END-PERFORM.
           IF WS-FOUND-SUB > ZERO
               MOVE WS-PF-AMOUNT TO WS-LT-PRIOR (WS-FOUND-SUB)
               MOVE 'Y'          TO WS-LT-PRIOR-SW (WS-FOUND-SUB)
           ELSE
               ADD 1 TO WS-PRIOR-UNMATCHED
           END-IF.
           PERFORM 8100-READ-PRIOR THRU 8100-EXIT.
       2500-EXIT.
           EXIT.
      *================================================================*
      * PRINT THE FORMULA BY SECTION                                   *
      *================================================================*
       3000-PRINT-FORMULA.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           MOVE 'C' TO WS-CUR-SECTION.
           MOVE 'CREDIT BALANCES' TO SL-TEXT.
           PERFORM 3100-PRINT-SECTION THRU 3100-EXIT.
           MOVE WS-SECTION-TOTAL TO WS-FOOT-CREDITS.
           MOVE 'D' TO WS-CUR-SECTION.
           MOVE 'DEBIT BALANCES' TO SL-TEXT.
           PERFORM 3100-PRINT-SECTION THRU 3100-EXIT.
           MOVE WS-SECTION-TOTAL TO WS-FOOT-DEBITS.
           MOVE 'S' TO WS-CUR-SECTION.
           MOVE 'RESERVE COMPUTATION' TO SL-TEXT.
           PERFORM 3100-PRINT-SECTION THRU 3100-EXIT.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ONE SECTION: HEADER, LINES, SECTION TOTAL (NOT FOR SUMMARY)    *
      *----------------------------------------------------------------*
       3100-PRINT-SECTION.
           IF RPT-LINE-COUNT + 8 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE '0' TO SL-CC.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
           MOVE ZERO TO WS-SECTION-TOTAL WS-SECTION-PRIOR
                        WS-SECTION-COUNT.
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-LT-USED
               IF WS-LT-SECTION (WS-SUB) = WS-CUR-SECTION
                   PERFORM 3200-PRINT-DETAIL THRU 3200-EXIT
                   IF WS-CUR-SECTION NOT = 'S'
                       ADD WS-LT-AMOUNT (WS-SUB) TO WS-SECTION-TOTAL
                       ADD WS-LT-PRIOR (WS-SUB)  TO WS-SECTION-PRIOR
                       ADD WS-LT-COUNT (WS-SUB)  TO WS-SECTION-COUNT
                   END-IF
               END-IF
           END-PERFORM.
           IF WS-CUR-SECTION NOT = 'S'
               MOVE SPACES           TO WS-DETAIL-LINE
               MOVE ' '              TO DL-CC
               MOVE '     SECTION TOTAL' TO DL-DESC
               MOVE WS-SECTION-COUNT TO DL-COUNT
               MOVE WS-SECTION-TOTAL TO DL-AMOUNT
               IF PRIOR-AVAILABLE
                   MOVE WS-SECTION-PRIOR TO DL-PRIOR
                   COMPUTE WS-CHANGE =
                           WS-SECTION-TOTAL - WS-SECTION-PRIOR
                   MOVE WS-CHANGE TO DL-CHANGE
               END-IF
               PERFORM 8120-PRINT-LINE THRU 8120-EXIT
           END-IF.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-PRINT-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES                 TO WS-DETAIL-LINE.
           MOVE ' '                    TO DL-CC.
           MOVE WS-LT-SECTION (WS-SUB) TO DL-SECTION.
           MOVE WS-LT-LINE-NO (WS-SUB) TO DL-LINE-NO.
           MOVE WS-LT-DESC (WS-SUB)    TO DL-DESC.
           IF WS-LT-COUNT (WS-SUB) NOT = ZERO
               MOVE WS-LT-COUNT (WS-SUB) TO DL-COUNT
           END-IF.
           MOVE WS-LT-AMOUNT (WS-SUB)  TO DL-AMOUNT.
           IF WS-LT-PRIOR-SW (WS-SUB) = 'Y'
               MOVE WS-LT-PRIOR (WS-SUB) TO DL-PRIOR
               COMPUTE WS-CHANGE = WS-LT-AMOUNT (WS-SUB)
                                 - WS-LT-PRIOR (WS-SUB)
               MOVE WS-CHANGE TO DL-CHANGE
           END-IF.
           PERFORM 8120-PRINT-LINE THRU 8120-EXIT.
      *    KEEP THE SUMMARY LINES FOR THE PROOF
           IF WS-LT-SECTION (WS-SUB) = 'S'
               EVALUATE WS-LT-LINE-NO (WS-SUB)
                   WHEN 01
                       MOVE WS-LT-AMOUNT (WS-SUB) TO WS-TOTAL-CREDITS
                   WHEN 02
                       MOVE WS-LT-AMOUNT (WS-SUB) TO WS-TOTAL-DEBITS
                   WHEN 03
                       MOVE WS-LT-AMOUNT (WS-SUB) TO WS-REQUIREMENT
                   WHEN 04
                       MOVE WS-LT-AMOUNT (WS-SUB) TO WS-DEPOSIT
                   WHEN 05
                       MOVE WS-LT-AMOUNT (WS-SUB) TO WS-EXCESS
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-IF.
       3200-EXIT.
           EXIT.
      *================================================================*
      * FOOTING AND COMPUTATION PROOFS                                 *
      *================================================================*
       4000-PROOFS.
           IF RPT-LINE-COUNT + 10 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE SPACES TO WS-SECTION-LINE.
           MOVE '-'    TO SL-CC.
           MOVE 'PROOF OF COMPUTATION' TO SL-TEXT.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
      *    ---- CREDIT LINES FOOT TO S01 ------------------------------
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE '0'              TO PL-CC.
           MOVE 'CREDIT LINES FOOTED / LINE S01' TO PL-LABEL.
           MOVE WS-FOOT-CREDITS  TO PL-VALUE-1.
           MOVE WS-TOTAL-CREDITS TO PL-VALUE-2.
           IF WS-FOOT-CREDITS = WS-TOTAL-CREDITS
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DOES NOT FOOT ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF THRU 8150-EXIT.
      *    ---- DEBIT LINES FOOT TO S02 -------------------------------
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'DEBIT LINES FOOTED / LINE S02' TO PL-LABEL.
           MOVE WS-FOOT-DEBITS   TO PL-VALUE-1.
           MOVE WS-TOTAL-DEBITS  TO PL-VALUE-2.
           IF WS-FOOT-DEBITS = WS-TOTAL-DEBITS
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DOES NOT FOOT ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF THRU 8150-EXIT.
      *    ---- S03 = GREATER OF ZERO AND S01 - S02 -------------------
           COMPUTE WS-CALC-REQ = WS-TOTAL-CREDITS - WS-TOTAL-DEBITS.
           IF WS-CALC-REQ < ZERO
               MOVE ZERO TO WS-CALC-REQ
           END-IF.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'REQUIREMENT RECOMPUTED / LINE S03' TO PL-LABEL.
           MOVE WS-CALC-REQ      TO PL-VALUE-1.
           MOVE WS-REQUIREMENT   TO PL-VALUE-2.
           IF WS-CALC-REQ = WS-REQUIREMENT
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DOES NOT PROVE ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF THRU 8150-EXIT.
      *    ---- S05 = S04 - S03 ---------------------------------------
           COMPUTE WS-CALC-EXCESS = WS-DEPOSIT - WS-REQUIREMENT.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'EXCESS RECOMPUTED / LINE S05' TO PL-LABEL.
           MOVE WS-CALC-EXCESS   TO PL-VALUE-1.
           MOVE WS-EXCESS        TO PL-VALUE-2.
           IF WS-CALC-EXCESS = WS-EXCESS
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DOES NOT PROVE ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF THRU 8150-EXIT.
           IF WS-PROOF-ERRORS > ZERO
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
       4000-EXIT.
           EXIT.
      *================================================================*
      * RESULT BOX - EXCESS OR DEFICIENCY, COVERAGE                    *
      *================================================================*
       5000-RESULT-BOX.
           IF RPT-LINE-COUNT + 9 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           IF WS-REQUIREMENT > ZERO
               COMPUTE WS-COVERAGE-PCT ROUNDED =
                       WS-DEPOSIT * 100 / WS-REQUIREMENT
                   ON SIZE ERROR
                       MOVE 99999.99 TO WS-COVERAGE-PCT
               END-COMPUTE
           ELSE
               MOVE ZERO TO WS-COVERAGE-PCT
           END-IF.
           MOVE WS-BOX-BORDER TO RPT-RECORD.
           MOVE '-' TO RPT-RECORD (1:1).
           WRITE RPT-RECORD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-BOX-LINE.
           MOVE ' '    TO BL-CC.
           IF WS-EXCESS < ZERO
               COMPUTE WS-CHANGE = WS-EXCESS * -1
               MOVE WS-CHANGE TO WS-MSG-AMOUNT
               MOVE DC-NEXT-BUS-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE THRU 8300-EXIT
               MOVE WS-DATE-EDIT TO WS-MSG-DATE
               STRING '*** DEFICIENCY OF $' DELIMITED BY SIZE
                      WS-MSG-AMOUNT         DELIMITED BY SIZE
                      ' - DEPOSIT BY 10:00 ON '
                                            DELIMITED BY SIZE
                      WS-MSG-DATE           DELIMITED BY SIZE
                      ' ***'                DELIMITED BY SIZE
                   INTO BL-TEXT
               END-STRING
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           ELSE
               MOVE WS-EXCESS TO WS-MSG-AMOUNT
               STRING 'RESERVE BANK ACCOUNT IN EXCESS OF THE '
                                            DELIMITED BY SIZE
                      'REQUIREMENT BY $'    DELIMITED BY SIZE
                      WS-MSG-AMOUNT         DELIMITED BY SIZE
                   INTO BL-TEXT
               END-STRING
           END-IF.
           WRITE RPT-RECORD FROM WS-BOX-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-BOX-LINE.
           MOVE ' '    TO BL-CC.
           MOVE WS-COVERAGE-PCT TO WS-MSG-PCT.
           STRING 'DEPOSIT AS PERCENT OF REQUIREMENT: '
                                            DELIMITED BY SIZE
                  WS-MSG-PCT                DELIMITED BY SIZE
                  ' PCT'                    DELIMITED BY SIZE
               INTO BL-TEXT
           END-STRING.
           WRITE RPT-RECORD FROM WS-BOX-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-BOX-BORDER.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-BOX-LINE.
           MOVE '0'    TO BL-CC.
           STRING 'PREPARED BY: ______________________    '
                                            DELIMITED BY SIZE
                  'REVIEWED (FINOP): ______________________'
                                            DELIMITED BY SIZE
               INTO BL-TEXT
           END-STRING.
           WRITE RPT-RECORD FROM WS-BOX-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 9 TO RPT-LINE-COUNT.
       5000-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-FORMULA.
           READ RESVIN-FILE.
           EVALUATE TRUE
               WHEN RESVIN-OK
                   IF RRF-AMOUNT NOT NUMERIC
                       MOVE 'RESVIN' TO AB-DDNAME
                       MOVE 1008 TO AB-ABEND-CODE
                       MOVE '8000-READ-FORMULA' TO AB-PARAGRAPH
                       MOVE RRF-DESC TO AB-KEY
                       MOVE 'FORMULA AMOUNT NOT NUMERIC' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   END-IF
               WHEN RESVIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'RESVIN' TO AB-DDNAME
                   MOVE WS-RESVIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-FORMULA' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-PRIOR.
      *----------------------------------------------------------------*
           READ PRIORIN-FILE INTO WS-PRIOR-FORMULA.
           EVALUATE TRUE
               WHEN PRIORIN-OK
                   IF WS-PF-AMOUNT NOT NUMERIC
                       MOVE ZERO TO WS-PF-AMOUNT
                   END-IF
               WHEN PRIORIN-EOF
                   MOVE 'Y' TO WS-PRIOR-EOF-SW
               WHEN OTHER
                   MOVE 'PRIORIN' TO AB-DDNAME
                   MOVE WS-PRIORIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-PRIOR' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8120-PRINT-LINE.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8120-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8150-PRINT-PROOF.
      *----------------------------------------------------------------*
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           IF PRIOR-AVAILABLE
               MOVE WS-PRIOR-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE THRU 8300-EXIT
               MOVE WS-DATE-EDIT  TO SH-PRIOR-DATE
           ELSE
               MOVE 'NONE'        TO SH-PRIOR-DATE
           END-IF.
           WRITE RPT-RECORD FROM WS-SUB-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 6 TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE.
      *----------------------------------------------------------------*
           MOVE WS-DI-CCYY TO WS-DE-CCYY.
           MOVE WS-DI-MM   TO WS-DE-MM.
           MOVE WS-DI-DD   TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8900-CHECK-WRITE' TO AB-PARAGRAPH
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE RESVIN-FILE PRIORIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'RRR110'        TO CT-STAGE.
           MOVE 'RESERVE-IN'    TO CT-COUNTER-NAME.
           MOVE WS-LINES-READ   TO CT-COUNT.
           MOVE WS-EXCESS       TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY '************************************************'.
           DISPLAY '* RRR110 - CUSTOMER RESERVE REPORT             *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' FORMULA DATED            : ' WS-FORMULA-DATE.
           DISPLAY ' PRIOR COMPUTATION DATED  : ' WS-PRIOR-DATE.
           DISPLAY ' FORMULA LINES READ       : ' WS-LINES-READ.
           DISPLAY ' PRIOR LINES READ         : ' WS-PRIOR-READ.
           DISPLAY ' PRIOR LINES UNMATCHED    : ' WS-PRIOR-UNMATCHED.
           DISPLAY ' PROOF ERRORS             : ' WS-PROOF-ERRORS.
           MOVE WS-EXCESS TO WS-MSG-AMOUNT.
           IF WS-EXCESS < ZERO
               DISPLAY ' DEFICIENCY               : -' WS-MSG-AMOUNT
           ELSE
               DISPLAY ' EXCESS                   : ' WS-MSG-AMOUNT
           END-IF.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
                   MOVE 'RESERVE REPORT - FORMULA DOES NOT PROVE'
                                     TO AU-MESSAGE
               WHEN WS-EXCESS < ZERO
                   MOVE 'W' TO AU-SEVERITY
                   MOVE 'RESERVE REPORT - DEFICIENCY REPORTED'
                                     TO AU-MESSAGE
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
                   MOVE 'RESERVE COMPUTATION REPORT ENDED'
                                     TO AU-MESSAGE
           END-EVALUATE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RRR110 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RRR110 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
