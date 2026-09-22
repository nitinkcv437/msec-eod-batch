       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCR310.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  APRIL 2004.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCR310                                            *
      * DESCRIPTION: SETTLEMENT CASH RECONCILIATION REPORT.            *
      *              SECTION 1 - BANK ITEMS NOT MATCHED, AND ITEMS     *
      *                          MATCHED WITHIN TOLERANCE (R02) OR ON  *
      *                          THE DTC NET LINE (R03)                *
      *              SECTION 2 - BOOK CASH LEGS NOT MATCHED (LEGS OF   *
      *                          AN OUT-OF-BALANCE DTC NET ARE ONLY    *
      *                          COUNTED)                              *
      *              SECTION 3 - SUMMARY BY CURRENCY AND MATCHING RULE *
      *              SECTION 4 - CASH BREAKS OPEN IN THE BREAK MASTER  *
      *                          AFTER TODAY'S RCB300 (AGE, LEVEL)     *
      *              THE RESULT FILE HOLDS THE BANK ITEMS FIRST, THEN  *
      *              THE BOOK LEGS (RCB300 WRITES THEM IN THAT ORDER). *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD040 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              CASHREC  - MSEC.PROD.RC.CASHREC(+1)      (RCMATCH)*
      *              RCBRKMST - MSEC.PROD.RC.BRKMAST.KSDS     (RCBRKM) *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2004-04-19 KAP  ORIGINAL                              CHG12230 *
      * 2004-06-07 KAP  TOLERANCE MATCHES LISTED              CHG12301 *
      * 2008-09-15 SPA  DTC NET LINE AND LEG COUNTS           CHG17955 *
      * 2013-05-06 SPA  CURRENCY SUMMARY TABLE                CHG24790 *
      * 2019-02-25 MHC  CONTROL TOTALS                        CHG33410 *
      * 2020-09-14 NVR  SECTION 4 - OPEN CASH BREAKS (RCR410  CHG36977 *
      *                 ONLY SHOWS THEM THE NEXT MORNING)              *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CASHREC-FILE   ASSIGN TO CASHREC
                  FILE STATUS IS WS-CASHREC-STATUS.
           SELECT BRKMAST-FILE   ASSIGN TO RCBRKMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS RBM-KEY
                  FILE STATUS IS WS-BRKMAST-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  CASHREC-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CASHREC-IN-REC              PIC X(200).
       FD  BRKMAST-FILE.
       COPY RCBRKM.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCR310'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-CASHREC-STATUS       PIC X(02)  VALUE '00'.
               88  CASHREC-OK                     VALUE '00'.
               88  CASHREC-EOF                    VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
           05  WS-BRKMAST-STATUS       PIC X(02)  VALUE '00'.
               88  BRKMAST-OK                     VALUE '00' '02'.
               88  BRKMAST-EOF                    VALUE '10'.
               88  BRKMAST-NOT-FOUND              VALUE '23'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-CASHREC                 VALUE 'Y'.
           05  WS-BRK-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-CASH-BREAKS             VALUE 'Y'.
       01  WS-CS-COUNTERS.
           05  WS-CS-OPEN              PIC S9(07)    COMP-3 VALUE ZERO.
           05  WS-CS-NEW               PIC S9(07)    COMP-3 VALUE ZERO.
           05  WS-CS-ESCALATED         PIC S9(07)    COMP-3 VALUE ZERO.
           05  WS-CS-VALUE             PIC S9(15)V99 COMP-3 VALUE ZERO.
       01  WS-CUR-SECTION              PIC 9(01)  VALUE ZERO.
       01  WS-SECTION-LINES            PIC S9(07) COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * SUMMARY BY CURRENCY: BANK / BOOK, MATCHED BY RULE, UNMATCHED   *
      *----------------------------------------------------------------*
       01  WS-CCY-USED                 PIC S9(04) COMP  VALUE ZERO.
       01  WS-CCY-TABLE.
           05  WS-CY OCCURS 20 TIMES INDEXED BY CY-IDX.
               10  WS-CY-CCY           PIC X(03).
               10  WS-CY-BANK-CNT      PIC S9(07)    COMP-3.
               10  WS-CY-BANK-AMT      PIC S9(15)V99 COMP-3.
               10  WS-CY-BOOK-CNT      PIC S9(07)    COMP-3.
               10  WS-CY-BOOK-AMT      PIC S9(15)V99 COMP-3.
               10  WS-CY-RULE OCCURS 4 TIMES.
                   15  WS-CY-R-CNT     PIC S9(07)    COMP-3.
                   15  WS-CY-R-AMT     PIC S9(15)V99 COMP-3.
               10  WS-CY-UN-BANK       PIC S9(07)    COMP-3.
               10  WS-CY-UN-BANK-AMT   PIC S9(15)V99 COMP-3.
               10  WS-CY-UN-BOOK       PIC S9(07)    COMP-3.
               10  WS-CY-UN-BOOK-AMT   PIC S9(15)V99 COMP-3.
               10  WS-CY-NET-LEGS      PIC S9(07)    COMP-3.
       01  WS-RULE-SUB                 PIC S9(04) COMP.
       01  WS-SUB                      PIC S9(04) COMP.
       01  WS-RULE-NAMES               PIC X(48)  VALUE
           'R01 EXACT   R02 TOLER   R03 DTC NET NONE        '.
       01  WS-RULE-NAME-TABLE REDEFINES WS-RULE-NAMES.
           05  WS-RULE-NAME OCCURS 4 TIMES PIC X(12).
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BANK-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINTED-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UN-BANK-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UN-BOOK-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NET-LEG-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MT-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UN-AMT               PIC S9(15)V99 COMP-3 VALUE ZERO.
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
       01  WS-SECTION-TITLE.
           05  ST-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  ST-TEXT                 PIC X(131).
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(04)  VALUE ' ST'.
           05  FILLER  PIC X(05)  VALUE 'RULE'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(11)  VALUE 'VALUE DATE'.
           05  FILLER  PIC X(17)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(19)  VALUE '            AMOUNT'.
           05  FILLER  PIC X(17)  VALUE 'MATCHED TO'.
           05  FILLER  PIC X(19)  VALUE '     OTHER AMOUNT'.
           05  FILLER  PIC X(15)  VALUE '    DIFFERENCE'.
           05  FILLER  PIC X(04)  VALUE 'TYP'.
           05  FILLER  PIC X(17)  VALUE 'TEXT'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(04)  VALUE ' --'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(15)  VALUE ' -------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-RULE                 PIC X(03).
           05  FILLER                  PIC X(02).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-VALUE-DATE           PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-OTHER-REF            PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-OTHER-AMT            PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-DIFF                 PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-TYPE                 PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-TEXT                 PIC X(14).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  NL-TEXT                 PIC X(122).
       01  WS-SUMM-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(05)  VALUE ' CCY'.
           05  FILLER  PIC X(08)  VALUE '   BANK'.
           05  FILLER  PIC X(19)  VALUE '      BANK AMOUNT'.
           05  FILLER  PIC X(08)  VALUE '   BOOK'.
           05  FILLER  PIC X(19)  VALUE '      BOOK AMOUNT'.
           05  FILLER  PIC X(08)  VALUE '    R01'.
           05  FILLER  PIC X(08)  VALUE '    R02'.
           05  FILLER  PIC X(08)  VALUE '    R03'.
           05  FILLER  PIC X(08)  VALUE ' UN BNK'.
           05  FILLER  PIC X(19)  VALUE ' UNMATCHED BANK'.
           05  FILLER  PIC X(08)  VALUE ' UN BK'.
           05  FILLER  PIC X(14)  VALUE ' NET LEGS'.
       01  WS-SUMM-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  SM-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  SM-BANK-CNT             PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-BANK-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  SM-BOOK-CNT             PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-BOOK-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  SM-R01                  PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-R02                  PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-R03                  PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-UN-BANK              PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-UN-BANK-AMT          PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  SM-UN-BOOK              PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  SM-NET-LEGS             PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(04).
       01  WS-BRK-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(05)  VALUE ' CCY'.
           05  FILLER  PIC X(17)  VALUE 'ITEM'.
           05  FILLER  PIC X(04)  VALUE 'CAT'.
           05  FILLER  PIC X(11)  VALUE 'FIRST SEEN'.
           05  FILLER  PIC X(05)  VALUE ' AGE'.
           05  FILLER  PIC X(04)  VALUE 'ESC'.
           05  FILLER  PIC X(19)  VALUE '      BANK AMOUNT'.
           05  FILLER  PIC X(19)  VALUE '      BOOK AMOUNT'.
           05  FILLER  PIC X(19)  VALUE '       DIFFERENCE'.
           05  FILLER  PIC X(29)  VALUE 'COMMENT'.
       01  WS-BRK-LINE.
           05  BR-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  BR-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  BR-ITEM                 PIC X(16).
           05  FILLER                  PIC X(01).
           05  BR-CAT                  PIC X(02).
           05  FILLER                  PIC X(02).
           05  BR-FIRST-SEEN           PIC X(10).
           05  FILLER                  PIC X(01).
           05  BR-AGE                  PIC ZZZ9.
           05  FILLER                  PIC X(02).
           05  BR-ESC                  PIC 9.
           05  FILLER                  PIC X(01).
           05  BR-BANK                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  BR-BOOK                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  BR-DIFF                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  BR-COMMENT              PIC X(29).
       01  WS-COUNT-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  CL-LABEL                PIC X(40).
           05  CL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  CL-AMT                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(60)  VALUE SPACES.
       COPY RCMATCH.
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
           PERFORM 2000-PROCESS-RECORD UNTIL END-OF-CASHREC.
           PERFORM 3900-END-OF-SECTIONS.
           PERFORM 4000-SUMMARY.
           PERFORM 5000-OPEN-CASH-BREAKS.
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
           MOVE 'CASH RECONCILIATION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT CASHREC-FILE.
           IF WS-CASHREC-STATUS NOT = '00'
               MOVE 'CASHREC' TO AB-DDNAME
               MOVE WS-CASHREC-STATUS TO AB-FILE-STATUS
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
           INITIALIZE WS-CCY-TABLE.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'SETTLEMENT CASH RECONCILIATION' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           MOVE 1 TO WS-CUR-SECTION.
           PERFORM 8200-HEADINGS.
           PERFORM 8000-READ-CASHREC.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           PERFORM 2100-ACCUMULATE.
           IF RCM-BOOK-SIDE AND WS-CUR-SECTION = 1
               PERFORM 3800-CLOSE-SECTION
               MOVE 2 TO WS-CUR-SECTION
               PERFORM 8200-HEADINGS
           END-IF.
           EVALUATE TRUE
               WHEN RCM-BANK-SIDE
                   IF RCM-UNMATCHED OR RCM-MATCHED-TOLERANCE
                   OR RCM-MATCHED-NET
                       PERFORM 2500-PRINT-DETAIL
                   END-IF
               WHEN RCM-BOOK-SIDE
                   IF RCM-UNMATCHED
                       IF RCM-RULE-ID = 'NT '
                           ADD 1 TO WS-NET-LEG-CNT
                       ELSE
                           PERFORM 2500-PRINT-DETAIL
                       END-IF
                   END-IF
           END-EVALUATE.
           PERFORM 8000-READ-CASHREC.
      *----------------------------------------------------------------*
      * CURRENCY TABLE                                                 *
      *----------------------------------------------------------------*
       2100-ACCUMULATE.
           SET CY-IDX TO 1.
           SEARCH WS-CY
               AT END
                   DISPLAY 'RCR310 MORE THAN 20 CURRENCIES - '
                           RCM-CCY ' NOT SUMMARISED'
               WHEN CY-IDX > WS-CCY-USED
                   ADD 1 TO WS-CCY-USED
                   MOVE RCM-CCY TO WS-CY-CCY (CY-IDX)
                   PERFORM 2150-ADD-TO-CCY
               WHEN WS-CY-CCY (CY-IDX) = RCM-CCY
                   PERFORM 2150-ADD-TO-CCY
           END-SEARCH.
      *----------------------------------------------------------------*
       2150-ADD-TO-CCY.
      *----------------------------------------------------------------*
           EVALUATE RCM-RULE-ID
               WHEN 'R01'  MOVE 1 TO WS-RULE-SUB
               WHEN 'R02'  MOVE 2 TO WS-RULE-SUB
               WHEN 'R03'  MOVE 3 TO WS-RULE-SUB
               WHEN OTHER  MOVE 4 TO WS-RULE-SUB
           END-EVALUATE.
           IF RCM-BANK-SIDE
               ADD 1 TO WS-BANK-CNT WS-CY-BANK-CNT (CY-IDX)
               ADD RCM-AMOUNT TO WS-CY-BANK-AMT (CY-IDX)
               IF RCM-UNMATCHED
                   ADD 1 TO WS-UN-BANK-CNT WS-CY-UN-BANK (CY-IDX)
                   ADD RCM-AMOUNT TO WS-CY-UN-BANK-AMT (CY-IDX)
                                     WS-UN-AMT
               ELSE
                   ADD 1 TO WS-CY-R-CNT (CY-IDX WS-RULE-SUB)
                   ADD RCM-AMOUNT TO WS-CY-R-AMT (CY-IDX WS-RULE-SUB)
               END-IF
               IF RCM-MATCHED-TOLERANCE
                   ADD 1 TO WS-MT-CNT
               END-IF
           ELSE
               ADD 1 TO WS-BOOK-CNT WS-CY-BOOK-CNT (CY-IDX)
               ADD RCM-AMOUNT TO WS-CY-BOOK-AMT (CY-IDX)
               IF RCM-UNMATCHED
                   IF RCM-RULE-ID = 'NT '
                       ADD 1 TO WS-CY-NET-LEGS (CY-IDX)
                   ELSE
                       ADD 1 TO WS-UN-BOOK-CNT WS-CY-UN-BOOK (CY-IDX)
                       ADD RCM-AMOUNT TO WS-CY-UN-BOOK-AMT (CY-IDX)
                   END-IF
               END-IF
           END-IF.
      *----------------------------------------------------------------*
       2500-PRINT-DETAIL.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES             TO WS-DETAIL-LINE.
           MOVE ' '                TO DL-CC.
           MOVE RCM-MATCH-STATUS   TO DL-STATUS.
           MOVE RCM-RULE-ID        TO DL-RULE.
           MOVE RCM-CCY            TO DL-CCY.
           IF RCM-VALUE-DATE NUMERIC AND RCM-VALUE-DATE > ZERO
               MOVE RCM-VALUE-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE
               MOVE WS-DATE-EDIT   TO DL-VALUE-DATE
           END-IF.
           MOVE RCM-REF            TO DL-REF.
           MOVE RCM-AMOUNT         TO DL-AMOUNT.
           MOVE RCM-OTHER-REF      TO DL-OTHER-REF.
           IF RCM-OTHER-REF NOT = SPACES
               MOVE RCM-OTHER-AMOUNT TO DL-OTHER-AMT
               MOVE RCM-DIFF-AMOUNT  TO DL-DIFF
           END-IF.
           MOVE RCM-TYPE-CODE      TO DL-TYPE.
           MOVE RCM-TEXT           TO DL-TEXT.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT WS-SECTION-LINES WS-PRINTED-CNT.
      *================================================================*
       3800-CLOSE-SECTION.
      *================================================================*
           IF WS-SECTION-LINES = ZERO
               MOVE SPACES TO NL-TEXT
               IF WS-CUR-SECTION = 1
                   MOVE '*** EVERY BANK ITEM MATCHED EXACTLY ***'
                                    TO NL-TEXT
               ELSE
                   MOVE '*** EVERY BOOK CASH LEG MATCHED ***'
                                    TO NL-TEXT
               END-IF
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           MOVE ZERO TO WS-SECTION-LINES.
      *----------------------------------------------------------------*
       3900-END-OF-SECTIONS.
      *----------------------------------------------------------------*
           PERFORM 3800-CLOSE-SECTION.
           IF WS-CUR-SECTION = 1
               MOVE 2 TO WS-CUR-SECTION
               PERFORM 8200-HEADINGS
               PERFORM 3800-CLOSE-SECTION
           END-IF.
           IF WS-NET-LEG-CNT > ZERO
               MOVE SPACES TO WS-COUNT-LINE
               MOVE '0'    TO CL-CC
               MOVE 'LEGS IN AN OUT-OF-BALANCE DTC NET (NOT LISTED)'
                                        TO CL-LABEL
               MOVE WS-NET-LEG-CNT      TO CL-COUNT
               WRITE RPT-RECORD FROM WS-COUNT-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
      *================================================================*
      * SECTION 3 - SUMMARY BY CURRENCY                                *
      *================================================================*
       4000-SUMMARY.
           MOVE 3 TO WS-CUR-SECTION.
           PERFORM 8200-HEADINGS.
           PERFORM VARYING CY-IDX FROM 1 BY 1
                   UNTIL CY-IDX > WS-CCY-USED
               MOVE SPACES                   TO WS-SUMM-LINE
               MOVE ' '                      TO SM-CC
               MOVE WS-CY-CCY (CY-IDX)       TO SM-CCY
               MOVE WS-CY-BANK-CNT (CY-IDX)  TO SM-BANK-CNT
               MOVE WS-CY-BANK-AMT (CY-IDX)  TO SM-BANK-AMT
               MOVE WS-CY-BOOK-CNT (CY-IDX)  TO SM-BOOK-CNT
               MOVE WS-CY-BOOK-AMT (CY-IDX)  TO SM-BOOK-AMT
               MOVE WS-CY-R-CNT (CY-IDX 1)   TO SM-R01
               MOVE WS-CY-R-CNT (CY-IDX 2)   TO SM-R02
               MOVE WS-CY-R-CNT (CY-IDX 3)   TO SM-R03
               MOVE WS-CY-UN-BANK (CY-IDX)   TO SM-UN-BANK
               MOVE WS-CY-UN-BANK-AMT (CY-IDX) TO SM-UN-BANK-AMT
               MOVE WS-CY-UN-BOOK (CY-IDX)   TO SM-UN-BOOK
               MOVE WS-CY-NET-LEGS (CY-IDX)  TO SM-NET-LEGS
               WRITE RPT-RECORD FROM WS-SUMM-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
           END-PERFORM.
           IF WS-CCY-USED = ZERO
               MOVE SPACES TO NL-TEXT
               MOVE '*** NO BANK OR BOOK CASH ITEMS TODAY ***'
                                        TO NL-TEXT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
           END-IF.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE '0'    TO CL-CC.
           MOVE 'BANK ITEMS'                     TO CL-LABEL.
           MOVE WS-BANK-CNT                      TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BOOK CASH LEGS'                 TO CL-LABEL.
           MOVE WS-BOOK-CNT                      TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'MATCHED WITHIN TOLERANCE (R02)' TO CL-LABEL.
           MOVE WS-MT-CNT                        TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BANK ITEMS UNMATCHED / AMOUNT'  TO CL-LABEL.
           MOVE WS-UN-BANK-CNT                   TO CL-COUNT.
           MOVE WS-UN-AMT                        TO CL-AMT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BOOK LEGS UNMATCHED (LISTED)'   TO CL-LABEL.
           MOVE WS-UN-BOOK-CNT                   TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
      * SECTION 4 - OPEN CASH BREAKS (CLASS CS) AFTER TODAY'S RCB300.  *
      * THE MASTER IS OPTIONAL HERE - A RERUN OF THE REPORT WHILE THE  *
      * CLUSTER IS BEING RESTORED STILL PRINTS SECTIONS 1-3.           *
      *================================================================*
       5000-OPEN-CASH-BREAKS.
           MOVE 4 TO WS-CUR-SECTION.
           PERFORM 8200-HEADINGS.
           OPEN INPUT BRKMAST-FILE.
           IF WS-BRKMAST-STATUS NOT = '00'
           AND WS-BRKMAST-STATUS NOT = '97'
               MOVE SPACES TO NL-TEXT
               STRING '*** BREAK MASTER NOT AVAILABLE - STATUS '
                      WS-BRKMAST-STATUS ' ***'
                      DELIMITED BY SIZE INTO NL-TEXT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
           ELSE
               MOVE SPACES TO RBM-KEY
               MOVE 'CS'   TO RBM-BREAK-CLASS
               START BRKMAST-FILE KEY IS NOT LESS THAN RBM-KEY
               IF NOT BRKMAST-OK
                   MOVE 'Y' TO WS-BRK-EOF-SW
               END-IF
               PERFORM 5100-NEXT-CASH-BREAK UNTIL END-OF-CASH-BREAKS
               CLOSE BRKMAST-FILE
               PERFORM 5200-CASH-BREAK-TOTALS
           END-IF.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *----------------------------------------------------------------*
       5100-NEXT-CASH-BREAK.
      *----------------------------------------------------------------*
           READ BRKMAST-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN BRKMAST-EOF
                   MOVE 'Y' TO WS-BRK-EOF-SW
               WHEN NOT BRKMAST-OK
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'BREAK MASTER READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
               WHEN NOT RBM-CASH-BREAK
                   MOVE 'Y' TO WS-BRK-EOF-SW
               WHEN RBM-OPEN
                   PERFORM 5150-PRINT-CASH-BREAK
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      *----------------------------------------------------------------*
       5150-PRINT-CASH-BREAK.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CS-OPEN.
           ADD RBM-MKT-VALUE-USD TO WS-CS-VALUE.
           IF RBM-FIRST-SEEN-DATE = DC-BUS-DATE
               ADD 1 TO WS-CS-NEW
           END-IF.
           IF RBM-ESCALATION-LVL > ZERO
               ADD 1 TO WS-CS-ESCALATED
           END-IF.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES              TO WS-BRK-LINE.
           MOVE ' '                 TO BR-CC.
           MOVE RBM-CCY             TO BR-CCY.
           MOVE RBM-ITEM-ID         TO BR-ITEM.
           MOVE RBM-CATEGORY        TO BR-CAT.
           MOVE RBM-FIRST-SEEN-DATE TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT        TO BR-FIRST-SEEN.
           MOVE RBM-AGE-BUS-DAYS    TO BR-AGE.
           MOVE RBM-ESCALATION-LVL  TO BR-ESC.
           MOVE RBM-STREET-AMOUNT   TO BR-BANK.
           MOVE RBM-BOOK-AMOUNT     TO BR-BOOK.
           MOVE RBM-DIFF-AMOUNT     TO BR-DIFF.
           MOVE RBM-COMMENT         TO BR-COMMENT.
           WRITE RPT-RECORD FROM WS-BRK-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       5200-CASH-BREAK-TOTALS.
      *----------------------------------------------------------------*
           IF WS-CS-OPEN = ZERO
               MOVE SPACES TO NL-TEXT
               MOVE '*** NO OPEN CASH BREAKS ***' TO NL-TEXT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE
           END-IF.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE '0'    TO CL-CC.
           MOVE 'OPEN CASH BREAKS / ABS DIFFERENCE'  TO CL-LABEL.
           MOVE WS-CS-OPEN                        TO CL-COUNT.
           MOVE WS-CS-VALUE                       TO CL-AMT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE '  OPENED TODAY'                     TO CL-LABEL.
           MOVE WS-CS-NEW                         TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE '  ESCALATED (LEVEL 1 AND ABOVE)'    TO CL-LABEL.
           MOVE WS-CS-ESCALATED                   TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
       8000-READ-CASHREC.
      *================================================================*
           READ CASHREC-FILE INTO RCM-MATCH-REC.
           EVALUATE TRUE
               WHEN CASHREC-OK
                   ADD 1 TO WS-READ-CNT
               WHEN CASHREC-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'CASHREC' TO AB-DDNAME
                   MOVE WS-CASHREC-STATUS TO AB-FILE-STATUS
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
           MOVE SPACES TO ST-TEXT.
           EVALUATE WS-CUR-SECTION
               WHEN 1
                   MOVE 'SECTION 1 - BANK ITEMS: UNMATCHED, R02, R03'
                                   TO ST-TEXT
               WHEN 2
                   MOVE 'SECTION 2 - BOOK CASH LEGS NOT MATCHED'
                                   TO ST-TEXT
               WHEN 3
                   MOVE 'SECTION 3 - SUMMARY BY CURRENCY' TO ST-TEXT
               WHEN OTHER
                   MOVE 'SECTION 4 - OPEN CASH BREAKS (BREAK MASTER)'
                                   TO ST-TEXT
           END-EVALUATE.
           WRITE RPT-RECORD FROM WS-SECTION-TITLE.
           PERFORM 8900-CHECK-WRITE.
           IF WS-CUR-SECTION < 3
               WRITE RPT-RECORD FROM WS-COL-HEAD-1
               PERFORM 8900-CHECK-WRITE
               WRITE RPT-RECORD FROM WS-COL-HEAD-2
               PERFORM 8900-CHECK-WRITE
               MOVE 7 TO RPT-LINE-COUNT
           ELSE
               IF WS-CUR-SECTION = 3
                   WRITE RPT-RECORD FROM WS-SUMM-HEAD-1
               ELSE
                   WRITE RPT-RECORD FROM WS-BRK-HEAD-1
               END-IF
               PERFORM 8900-CHECK-WRITE
               MOVE 6 TO RPT-LINE-COUNT
           END-IF.
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
           CLOSE CASHREC-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'RCR310'        TO CT-STAGE.
           MOVE 'RECORDS-IN'    TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE ZERO            TO CT-AMOUNT CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE 'ITEMS-PRINTED' TO CT-COUNTER-NAME.
           MOVE WS-PRINTED-CNT  TO CT-COUNT.
           MOVE WS-UN-AMT       TO CT-AMOUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'RCR310 RESULT RECORDS READ : ' WS-READ-CNT.
           DISPLAY 'RCR310 ITEMS PRINTED       : ' WS-PRINTED-CNT.
           DISPLAY 'RCR310 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'CASH RECONCILIATION REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'RCR310 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
