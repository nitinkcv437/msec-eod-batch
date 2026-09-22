       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCR410.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  AUGUST 2002.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCR410                                            *
      * DESCRIPTION: RECONCILIATION BREAK AGING REPORT.                *
      *              READS THE WHOLE BREAK MASTER (KEY ORDER: CASH     *
      *              BREAKS 'CS' BEFORE POSITION BREAKS 'SP') AND      *
      *              PRINTS                                            *
      *              PART 1 - EVERY OPEN BREAK WITH FIRST / LAST SEEN, *
      *                       AGE, AGING BUCKET, ESCALATION AND OWNER  *
      *              PART 2 - BREAKS CLOSED TODAY                      *
      *              PART 3 - AGING MATRIX (CLASS / CATEGORY BY AGE    *
      *                       BUCKET) AND ESCALATION SUMMARY           *
      *              AGING BUCKETS: 0-2, 3-4, 5-9, 10+ BUSINESS DAYS   *
      *              (THE ESCALATION THRESHOLDS OF RCB400).            *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD030 / STEP050                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              RCBRKMST - MSEC.PROD.RC.BRKMAST.KSDS     (RCBRKM) *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      * NOTE       : RUNS IN MSRCD030 AFTER RCB400.  CASH BREAKS ARE   *
      *              THOSE LEFT BY THE PREVIOUS RUN OF MSRCD040.       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2002-08-19 KAP  ORIGINAL                              CHG10240 *
      * 2005-01-24 KAP  ESCALATION SUMMARY, CASH BREAKS       CHG12966 *
      * 2009-12-14 SPA  VALUE IN USD                          CHG19002 *
      * 2017-03-13 MHC  BUSINESS DAY BUCKETS                  CHG31555 *
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
           SELECT BRKMAST-FILE   ASSIGN TO RCBRKMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
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
       FD  BRKMAST-FILE.
       COPY RCBRKM.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCR410'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-BRKMAST-STATUS       PIC X(02)  VALUE '00'.
               88  BRKMAST-OK                     VALUE '00' '02'.
               88  BRKMAST-EOF                    VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-BRKMAST                 VALUE 'Y'.
       01  WS-PART                     PIC 9(01)  VALUE 1.
       01  WS-PART-LINES               PIC S9(07) COMP-3 VALUE ZERO.
       01  WS-CUR-CLASS                PIC X(02)  VALUE SPACES.
      *----------------------------------------------------------------*
      * AGING BUCKETS - SAME THRESHOLDS AS THE RCB400 ESCALATION       *
      *----------------------------------------------------------------*
       01  WS-BUCKET-VALUES.
           05  FILLER  PIC X(12)  VALUE '000   0 - 2 '.
           05  FILLER  PIC X(12)  VALUE '003   3 - 4 '.
           05  FILLER  PIC X(12)  VALUE '005   5 - 9 '.
           05  FILLER  PIC X(12)  VALUE '010    10 + '.
       01  WS-BUCKET-TABLE REDEFINES WS-BUCKET-VALUES.
           05  WS-BU-ENTRY OCCURS 4 TIMES.
               10  WS-BU-MIN-AGE       PIC 9(03).
               10  WS-BU-NAME          PIC X(09).
       01  WS-BUCKET                   PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * AGING MATRIX: 6 CATEGORIES (MS MB QD BU KU AD) X 4 BUCKETS     *
      *----------------------------------------------------------------*
       01  WS-CAT-VALUES.
           05  FILLER  PIC X(26)  VALUE 'SPMSPOSN MISSING AT STREET'.
           05  FILLER  PIC X(26)  VALUE 'SPMBPOSN MISSING IN BOOKS '.
           05  FILLER  PIC X(26)  VALUE 'SPQDPOSN QTY DIFFERENCE   '.
           05  FILLER  PIC X(26)  VALUE 'CSBUCASH BANK UNMATCHED   '.
           05  FILLER  PIC X(26)  VALUE 'CSKUCASH BOOK UNMATCHED   '.
           05  FILLER  PIC X(26)  VALUE 'CSADCASH AMOUNT DIFFERENCE'.
       01  WS-CAT-TABLE REDEFINES WS-CAT-VALUES.
           05  WS-CA-ENTRY OCCURS 6 TIMES INDEXED BY CA-IDX.
               10  WS-CA-CLASS         PIC X(02).
               10  WS-CA-CODE          PIC X(02).
               10  WS-CA-NAME          PIC X(22).
       01  WS-MATRIX.
           05  WS-MX-CAT OCCURS 7 TIMES.
               10  WS-MX-CELL OCCURS 4 TIMES.
                   15  WS-MX-COUNT     PIC S9(07)    COMP-3.
                   15  WS-MX-VALUE     PIC S9(15)V99 COMP-3.
               10  WS-MX-TOT-COUNT     PIC S9(07)    COMP-3.
               10  WS-MX-TOT-VALUE     PIC S9(15)V99 COMP-3.
       01  WS-CAT-SUB                  PIC S9(04) COMP.
       01  WS-SUB                      PIC S9(04) COMP.
       01  WS-SUB2                     PIC S9(04) COMP.
       01  WS-LEVEL-COUNTS.
           05  WS-LEVEL-CNT OCCURS 4 TIMES PIC S9(07) COMP-3.
      *----------------------------------------------------------------*
      * BREAKS CLOSED TODAY ARE HELD FOR PART 2                        *
      *----------------------------------------------------------------*
       01  WS-CLOSED-USED              PIC S9(04) COMP  VALUE ZERO.
       01  WS-CLOSED-TABLE.
           05  WS-CL OCCURS 500 TIMES.
               10  WS-CL-KEY           PIC X(22).
               10  WS-CL-CATEGORY      PIC X(02).
               10  WS-CL-FIRST-SEEN    PIC 9(08).
               10  WS-CL-AGE           PIC S9(03)    COMP-3.
               10  WS-CL-VALUE         PIC S9(15)V99 COMP-3.
               10  WS-CL-COMMENT       PIC X(40).
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OPEN-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLOSED-TODAY         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLOSED-OLD           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WRITTEN-OFF          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLOSED-DROPPED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OPEN-VALUE           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-OLDEST-AGE           PIC S9(05)    COMP-3 VALUE ZERO.
           05  WS-OLDEST-KEY           PIC X(22)  VALUE SPACES.
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
       01  WS-PART-TITLE.
           05  PT-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  PT-TEXT                 PIC X(131).
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(04)  VALUE ' CL'.
           05  FILLER  PIC X(05)  VALUE 'DEPO'.
           05  FILLER  PIC X(17)  VALUE 'ITEM'.
           05  FILLER  PIC X(04)  VALUE 'CAT'.
           05  FILLER  PIC X(11)  VALUE 'FIRST SEEN'.
           05  FILLER  PIC X(11)  VALUE 'LAST SEEN'.
           05  FILLER  PIC X(05)  VALUE ' AGE'.
           05  FILLER  PIC X(10)  VALUE 'BUCKET'.
           05  FILLER  PIC X(04)  VALUE 'ESC'.
           05  FILLER  PIC X(09)  VALUE 'OWNER'.
           05  FILLER  PIC X(19)  VALUE '       DIFFERENCE'.
           05  FILLER  PIC X(17)  VALUE '      VALUE'.
           05  FILLER  PIC X(16)  VALUE 'COMMENT'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(04)  VALUE ' --'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(17)  VALUE '----------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(05)  VALUE ' ---'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(09)  VALUE '--------'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(17)  VALUE ' ---------------'.
           05  FILLER  PIC X(16)  VALUE '---------------'.
       01  WS-BREAK-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  BL-CLASS                PIC X(02).
           05  FILLER                  PIC X(01).
           05  BL-DEPO                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  BL-ITEM                 PIC X(16).
           05  FILLER                  PIC X(01).
           05  BL-CAT                  PIC X(02).
           05  FILLER                  PIC X(02).
           05  BL-FIRST-SEEN           PIC X(10).
           05  FILLER                  PIC X(01).
           05  BL-LAST-SEEN            PIC X(10).
           05  FILLER                  PIC X(01).
           05  BL-AGE                  PIC ZZZ9.
           05  FILLER                  PIC X(01).
           05  BL-BUCKET               PIC X(09).
           05  FILLER                  PIC X(02).
           05  BL-ESC                  PIC 9.
           05  FILLER                  PIC X(02).
           05  BL-OWNER                PIC X(08).
           05  FILLER                  PIC X(01).
           05  BL-DIFF                 PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-VALUE                PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  BL-COMMENT              PIC X(18).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  NL-TEXT                 PIC X(122).
       01  WS-MATRIX-HEAD.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(28)  VALUE ' CATEGORY'.
           05  FILLER  PIC X(21)  VALUE '     0 - 2 DAYS'.
           05  FILLER  PIC X(21)  VALUE '     3 - 4 DAYS'.
           05  FILLER  PIC X(21)  VALUE '     5 - 9 DAYS'.
           05  FILLER  PIC X(21)  VALUE '    10 + DAYS'.
           05  FILLER  PIC X(20)  VALUE '     TOTAL'.
       01  WS-MATRIX-LINE.
           05  ML-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  ML-NAME                 PIC X(26).
           05  ML-CELL OCCURS 5 TIMES.
               10  ML-COUNT            PIC ZZZ9.
               10  FILLER              PIC X(01).
               10  ML-VALUE            PIC -ZZZ,ZZZ,ZZ9.
               10  FILLER              PIC X(03).
           05  FILLER                  PIC X(05).
       01  WS-COUNT-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  CL-LABEL                PIC X(40).
           05  CL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  CL-TEXT                 PIC X(79).
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
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-BREAK THRU 2000-EXIT
               UNTIL END-OF-BRKMAST.
           PERFORM 3000-CLOSED-TODAY THRU 3000-EXIT.
           PERFORM 4000-AGING-MATRIX THRU 4000-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
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
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
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
           MOVE 'BREAK AGING REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT BRKMAST-FILE.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           INITIALIZE WS-MATRIX WS-LEVEL-COUNTS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'RECONCILIATION BREAK AGING' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           MOVE 1 TO WS-PART.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           PERFORM 8000-READ-BRKMAST THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * ONE BREAK MASTER ROW                                           *
      *================================================================*
       2000-PROCESS-BREAK.
           EVALUATE TRUE
               WHEN RBM-OPEN
                   PERFORM 2100-OPEN-BREAK THRU 2100-EXIT
               WHEN RBM-CLOSED
                   IF RBM-CLOSED-DATE = DC-BUS-DATE
                       PERFORM 2500-HOLD-CLOSED THRU 2500-EXIT
                   ELSE
                       ADD 1 TO WS-CLOSED-OLD
                   END-IF
               WHEN RBM-WRITTEN-OFF
                   ADD 1 TO WS-WRITTEN-OFF
               WHEN OTHER
                   DISPLAY 'RCR410 UNKNOWN STATUS ' RBM-STATUS
                           ' ON ' RBM-KEY
           END-EVALUATE.
           PERFORM 8000-READ-BRKMAST THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-OPEN-BREAK.
      *----------------------------------------------------------------*
           ADD 1 TO WS-OPEN-CNT.
           ADD RBM-MKT-VALUE-USD TO WS-OPEN-VALUE.
           IF RBM-AGE-BUS-DAYS > WS-OLDEST-AGE
               MOVE RBM-AGE-BUS-DAYS TO WS-OLDEST-AGE
               MOVE RBM-KEY          TO WS-OLDEST-KEY
           END-IF.
           PERFORM 2200-FIND-BUCKET THRU 2200-EXIT.
           PERFORM 2300-ADD-TO-MATRIX THRU 2300-EXIT.
           IF RBM-ESCALATION-LVL NUMERIC
           AND RBM-ESCALATION-LVL < 4
               COMPUTE WS-SUB = RBM-ESCALATION-LVL + 1
               ADD 1 TO WS-LEVEL-CNT (WS-SUB)
           END-IF.
           IF RBM-BREAK-CLASS NOT = WS-CUR-CLASS
               MOVE RBM-BREAK-CLASS TO WS-CUR-CLASS
               IF RPT-LINE-COUNT + 3 > RPT-LINES-PER-PAGE
                   PERFORM 8200-HEADINGS THRU 8200-EXIT
               ELSE
                   MOVE SPACES TO WS-BREAK-LINE
                   MOVE ' '    TO BL-CC
                   WRITE RPT-RECORD FROM WS-BREAK-LINE
                   PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
                   ADD 1 TO RPT-LINE-COUNT
               END-IF
           END-IF.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE SPACES              TO WS-BREAK-LINE.
           MOVE ' '                 TO BL-CC.
           MOVE RBM-BREAK-CLASS     TO BL-CLASS.
           MOVE RBM-DEPOSITORY      TO BL-DEPO.
           MOVE RBM-ITEM-ID         TO BL-ITEM.
           MOVE RBM-CATEGORY        TO BL-CAT.
           MOVE RBM-FIRST-SEEN-DATE TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT        TO BL-FIRST-SEEN.
           MOVE RBM-LAST-SEEN-DATE  TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT        TO BL-LAST-SEEN.
           MOVE RBM-AGE-BUS-DAYS    TO BL-AGE.
           MOVE WS-BU-NAME (WS-BUCKET) TO BL-BUCKET.
           MOVE RBM-ESCALATION-LVL  TO BL-ESC.
           MOVE RBM-ASSIGNED-TO     TO BL-OWNER.
           IF RBM-POSITION-BREAK
               MOVE RBM-DIFF-QTY    TO BL-DIFF
           ELSE
               MOVE RBM-DIFF-AMOUNT TO BL-DIFF
           END-IF.
           MOVE RBM-MKT-VALUE-USD   TO BL-VALUE.
           MOVE RBM-COMMENT         TO BL-COMMENT.
           WRITE RPT-RECORD FROM WS-BREAK-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT WS-PART-LINES.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-FIND-BUCKET.
      *----------------------------------------------------------------*
           MOVE 1 TO WS-BUCKET.
           PERFORM VARYING WS-SUB FROM 2 BY 1 UNTIL WS-SUB > 4
               IF RBM-AGE-BUS-DAYS NOT < WS-BU-MIN-AGE (WS-SUB)
                   MOVE WS-SUB TO WS-BUCKET
               END-IF
           END-PERFORM.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CATEGORY ROW 7 = ANYTHING NOT IN THE TABLE                     *
      *----------------------------------------------------------------*
       2300-ADD-TO-MATRIX.
           MOVE 7 TO WS-CAT-SUB.
           SET CA-IDX TO 1.
           SEARCH WS-CA-ENTRY
               AT END
                   CONTINUE
               WHEN WS-CA-CLASS (CA-IDX) = RBM-BREAK-CLASS
                AND WS-CA-CODE (CA-IDX) = RBM-CATEGORY
                   SET WS-CAT-SUB TO CA-IDX
           END-SEARCH.
           ADD 1 TO WS-MX-COUNT (WS-CAT-SUB WS-BUCKET)
                    WS-MX-TOT-COUNT (WS-CAT-SUB).
           ADD RBM-MKT-VALUE-USD TO WS-MX-VALUE (WS-CAT-SUB WS-BUCKET)
                                    WS-MX-TOT-VALUE (WS-CAT-SUB).
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2500-HOLD-CLOSED.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CLOSED-TODAY.
           IF WS-CLOSED-USED NOT < 500
               ADD 1 TO WS-CLOSED-DROPPED
               GO TO 2500-EXIT
           END-IF.
           ADD 1 TO WS-CLOSED-USED.
           MOVE RBM-KEY             TO WS-CL-KEY (WS-CLOSED-USED).
           MOVE RBM-CATEGORY        TO WS-CL-CATEGORY (WS-CLOSED-USED).
           MOVE RBM-FIRST-SEEN-DATE TO
                                    WS-CL-FIRST-SEEN (WS-CLOSED-USED).
           MOVE RBM-AGE-BUS-DAYS    TO WS-CL-AGE (WS-CLOSED-USED).
           MOVE RBM-MKT-VALUE-USD   TO WS-CL-VALUE (WS-CLOSED-USED).
           MOVE RBM-COMMENT         TO WS-CL-COMMENT (WS-CLOSED-USED).
       2500-EXIT.
           EXIT.
      *================================================================*
      * PART 2 - BREAKS CLOSED TODAY                                   *
      *================================================================*
       3000-CLOSED-TODAY.
           IF WS-PART-LINES = ZERO
               MOVE SPACES TO NL-TEXT
               MOVE '*** NO OPEN BREAKS ***' TO NL-TEXT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
           MOVE 2 TO WS-PART.
           MOVE ZERO TO WS-PART-LINES.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           PERFORM VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-CLOSED-USED
               IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
                   PERFORM 8200-HEADINGS THRU 8200-EXIT
               END-IF
               MOVE SPACES                   TO WS-BREAK-LINE
               MOVE ' '                      TO BL-CC
               MOVE WS-CL-KEY (WS-SUB) (1:2) TO BL-CLASS
               MOVE WS-CL-KEY (WS-SUB) (3:4) TO BL-DEPO
               MOVE WS-CL-KEY (WS-SUB) (7:16) TO BL-ITEM
               MOVE WS-CL-CATEGORY (WS-SUB)  TO BL-CAT
               MOVE WS-CL-FIRST-SEEN (WS-SUB) TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE THRU 8300-EXIT
               MOVE WS-DATE-EDIT             TO BL-FIRST-SEEN
               MOVE 'CLOSED'                 TO BL-LAST-SEEN
               MOVE WS-CL-AGE (WS-SUB)       TO BL-AGE
               MOVE WS-CL-VALUE (WS-SUB)     TO BL-VALUE
               MOVE WS-CL-COMMENT (WS-SUB)   TO BL-COMMENT
               WRITE RPT-RECORD FROM WS-BREAK-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1 TO RPT-LINE-COUNT WS-PART-LINES
           END-PERFORM.
           IF WS-PART-LINES = ZERO
               MOVE SPACES TO NL-TEXT
               MOVE '*** NO BREAKS CLOSED TODAY ***' TO NL-TEXT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
           IF WS-CLOSED-DROPPED > ZERO
               MOVE SPACES TO WS-COUNT-LINE
               MOVE '0'    TO CL-CC
               MOVE 'CLOSED BREAKS NOT LISTED (TABLE FULL)'
                                          TO CL-LABEL
               MOVE WS-CLOSED-DROPPED     TO CL-COUNT
               WRITE RPT-RECORD FROM WS-COUNT-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
       3000-EXIT.
           EXIT.
      *================================================================*
      * PART 3 - AGING MATRIX AND ESCALATION SUMMARY                   *
      *================================================================*
       4000-AGING-MATRIX.
           MOVE 3 TO WS-PART.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           PERFORM VARYING WS-CAT-SUB FROM 1 BY 1 UNTIL WS-CAT-SUB > 7
               MOVE SPACES TO WS-MATRIX-LINE
               MOVE ' '    TO ML-CC
               IF WS-CAT-SUB < 7
                   STRING WS-CA-CLASS (WS-CAT-SUB) ' '
                          WS-CA-CODE (WS-CAT-SUB) ' '
                          WS-CA-NAME (WS-CAT-SUB)
                          DELIMITED BY SIZE INTO ML-NAME
               ELSE
                   MOVE 'OTHER'   TO ML-NAME
               END-IF
               PERFORM VARYING WS-SUB2 FROM 1 BY 1 UNTIL WS-SUB2 > 4
                   MOVE WS-MX-COUNT (WS-CAT-SUB WS-SUB2)
                                        TO ML-COUNT (WS-SUB2)
                   MOVE WS-MX-VALUE (WS-CAT-SUB WS-SUB2)
                                        TO ML-VALUE (WS-SUB2)
               END-PERFORM
               MOVE WS-MX-TOT-COUNT (WS-CAT-SUB) TO ML-COUNT (5)
               MOVE WS-MX-TOT-VALUE (WS-CAT-SUB) TO ML-VALUE (5)
               WRITE RPT-RECORD FROM WS-MATRIX-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-PERFORM.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE '0'    TO CL-CC.
           MOVE 'OPEN BREAKS' TO CL-LABEL.
           MOVE WS-OPEN-CNT   TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4
               MOVE SPACES TO WS-COUNT-LINE
               MOVE ' '    TO CL-CC
               COMPUTE WS-SUB2 = WS-SUB - 1
               EVALUATE WS-SUB2
                   WHEN 0
                       MOVE '  NOT ESCALATED' TO CL-LABEL
                   WHEN 1
                       MOVE '  LEVEL 1 - CAGE SUPERVISOR' TO CL-LABEL
                   WHEN 2
                       MOVE '  LEVEL 2 - OPERATIONS MANAGER'
                                                TO CL-LABEL
                   WHEN OTHER
                       MOVE '  LEVEL 3 - CONTROLLER / FINOP'
                                                TO CL-LABEL
               END-EVALUATE
               MOVE WS-LEVEL-CNT (WS-SUB) TO CL-COUNT
               WRITE RPT-RECORD FROM WS-COUNT-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-PERFORM.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'OLDEST OPEN BREAK (BUSINESS DAYS)' TO CL-LABEL.
           MOVE WS-OLDEST-AGE TO CL-COUNT.
           MOVE WS-OLDEST-KEY TO CL-TEXT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'CLOSED TODAY'            TO CL-LABEL.
           MOVE WS-CLOSED-TODAY           TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'CLOSED ON EARLIER DAYS (HISTORY)' TO CL-LABEL.
           MOVE WS-CLOSED-OLD             TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'WRITTEN OFF'             TO CL-LABEL.
           MOVE WS-WRITTEN-OFF            TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       4000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-BRKMAST.
      *================================================================*
           READ BRKMAST-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN BRKMAST-OK
                   ADD 1 TO WS-READ-CNT
               WHEN BRKMAST-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
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
           MOVE SPACES TO PT-TEXT.
           EVALUATE WS-PART
               WHEN 1
                   MOVE 'PART 1 - OPEN BREAKS (CASH, THEN POSITION)'
                                   TO PT-TEXT
               WHEN 2
                   MOVE 'PART 2 - BREAKS CLOSED TODAY' TO PT-TEXT
               WHEN OTHER
                   MOVE 'PART 3 - AGING MATRIX AND ESCALATION'
                                   TO PT-TEXT
           END-EVALUATE.
           WRITE RPT-RECORD FROM WS-PART-TITLE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           IF WS-PART < 3
               WRITE RPT-RECORD FROM WS-COL-HEAD-1
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               WRITE RPT-RECORD FROM WS-COL-HEAD-2
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               MOVE 7 TO RPT-LINE-COUNT
           ELSE
               WRITE RPT-RECORD FROM WS-MATRIX-HEAD
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               MOVE 6 TO RPT-LINE-COUNT
           END-IF.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE.
      *----------------------------------------------------------------*
           IF WS-DATE-IN NOT NUMERIC
               MOVE ZERO TO WS-DATE-IN
           END-IF.
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
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE BRKMAST-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'RCR410'        TO CT-STAGE.
           MOVE 'RECORDS-IN'    TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE ZERO            TO CT-AMOUNT CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE 'OPEN-BREAKS'   TO CT-COUNTER-NAME.
           MOVE WS-OPEN-CNT     TO CT-COUNT.
           MOVE WS-OPEN-VALUE   TO CT-AMOUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'RCR410 BREAK MASTER ROWS   : ' WS-READ-CNT.
           DISPLAY 'RCR410 OPEN BREAKS         : ' WS-OPEN-CNT.
           DISPLAY 'RCR410 CLOSED TODAY        : ' WS-CLOSED-TODAY.
           DISPLAY 'RCR410 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'BREAK AGING REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'RCR410 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
