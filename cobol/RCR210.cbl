       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCR210.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  FEBRUARY 1997.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCR210                                            *
      * DESCRIPTION: STREET POSITION RECONCILIATION - BREAK REPORT.    *
      *              ONE SECTION PER DEPOSITORY (DTC, EUROCLEAR, FED): *
      *              EVERY BREAK FOUND BY RCB200 WITH STREET AND BOOK  *
      *              QUANTITY, DIFFERENCE, VALUE, AND - FROM THE BREAK *
      *              MASTER UPDATED BY RCB400 - FIRST SEEN DATE, AGE,  *
      *              ESCALATION LEVEL AND OWNER.  DEPOSITORY TOTALS    *
      *              AND A FINAL SUMMARY BY BREAK CATEGORY.            *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD030 / STEP040                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              POSREC   - MSEC.PROD.RC.POSREC(+1)       (RCPREC) *
      *              RCBRKMST - MSEC.PROD.RC.BRKMAST.KSDS     (RCBRKM) *
      *                         (RANDOM READ)                          *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1997-02-10 DWB  ORIGINAL                              CHG02644 *
      * 1998-01-12 TLM  FED SECTION                           CHG03880 *
      * 1998-12-07 TLM  Y2K - 4 DIGIT YEARS IN HEADINGS       CHG04471 *
      * 2002-08-19 KAP  AGE AND OWNER FROM THE BREAK MASTER   CHG10240 *
      * 2003-10-06 KAP  EUROCLEAR SECTION, ISIN COLUMN        CHG11702 *
      * 2005-01-24 KAP  ESCALATION LEVEL COLUMN               CHG12966 *
      * 2009-12-14 SPA  VALUE IN USD                          CHG19002 *
      * 2013-05-06 SPA  COUNTS OF MATCHED ITEMS PER SECTION   CHG24790 *
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
           SELECT POSREC-FILE    ASSIGN TO POSREC
                  FILE STATUS IS WS-POSREC-STATUS.
           SELECT BRKMAST-FILE   ASSIGN TO RCBRKMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
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
       FD  POSREC-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RCPREC.
       FD  BRKMAST-FILE.
       COPY RCBRKM.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCR210'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-POSREC-STATUS        PIC X(02)  VALUE '00'.
               88  POSREC-OK                      VALUE '00'.
               88  POSREC-EOF                     VALUE '10'.
           05  WS-BRKMAST-STATUS       PIC X(02)  VALUE '00'.
               88  BRKMAST-OK                     VALUE '00' '02'.
               88  BRKMAST-NOT-FOUND              VALUE '23'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-POSREC                  VALUE 'Y'.
           05  WS-BRKMAST-OPEN-SW      PIC X(01)  VALUE 'N'.
               88  BRKMAST-IS-OPEN                VALUE 'Y'.
           05  WS-SECTION-SW           PIC X(01)  VALUE 'N'.
               88  SECTION-STARTED                VALUE 'Y'.
       01  WS-CUR-DEPO                 PIC X(04)  VALUE LOW-VALUES.
       01  WS-CUR-STMT-DATE            PIC 9(08)  VALUE ZERO.
      *----------------------------------------------------------------*
      * DEPOSITORY NAMES                                               *
      *----------------------------------------------------------------*
       01  WS-DEPO-NAME-VALUES.
           05  FILLER  PIC X(44)  VALUE
               'DTC DEPOSITORY TRUST COMPANY      STREETDTC0'.
           05  FILLER  PIC X(44)  VALUE
               'EUCLEUROCLEAR BANK                STREETEUC0'.
           05  FILLER  PIC X(44)  VALUE
               'FED FEDERAL RESERVE BOOK ENTRY    STREETFED0'.
       01  WS-DEPO-NAME-TABLE REDEFINES WS-DEPO-NAME-VALUES.
           05  WS-DN-ENTRY OCCURS 3 TIMES INDEXED BY DN-IDX.
               10  WS-DN-DEPO          PIC X(04).
               10  WS-DN-NAME          PIC X(30).
               10  WS-DN-ACCT          PIC X(10).
      *----------------------------------------------------------------*
      * BREAK CATEGORIES                                               *
      *----------------------------------------------------------------*
       01  WS-CAT-VALUES.
           05  FILLER  PIC X(24)  VALUE 'MSMISSING AT STREET     '.
           05  FILLER  PIC X(24)  VALUE 'MBMISSING IN BOOKS      '.
           05  FILLER  PIC X(24)  VALUE 'QDQUANTITY DIFFERENCE   '.
       01  WS-CAT-TABLE REDEFINES WS-CAT-VALUES.
           05  WS-CT-ENTRY OCCURS 3 TIMES INDEXED BY CT-IDX.
               10  WS-CT-CODE          PIC X(02).
               10  WS-CT-NAME          PIC X(22).
       01  WS-CAT-TOTALS.
           05  WS-CTT OCCURS 3 TIMES.
               10  WS-CTT-COUNT        PIC S9(07)       COMP-3.
               10  WS-CTT-NEW          PIC S9(07)       COMP-3.
               10  WS-CTT-MV           PIC S9(15)V99    COMP-3.
       01  WS-SUB                      PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * BREAKS BY SECURITY TYPE - MB BREAKS HAVE NO BOOK ROW           *
      *----------------------------------------------------------------*
       01  WS-SECTYPE-VALUES.
           05  FILLER  PIC X(24)  VALUE 'EQEQUITY                '.
           05  FILLER  PIC X(24)  VALUE 'PFPREFERRED             '.
           05  FILLER  PIC X(24)  VALUE 'ADADR                   '.
           05  FILLER  PIC X(24)  VALUE 'CBCORPORATE BOND        '.
           05  FILLER  PIC X(24)  VALUE 'MUMUNICIPAL BOND        '.
           05  FILLER  PIC X(24)  VALUE 'GVGOVERNMENT            '.
           05  FILLER  PIC X(24)  VALUE 'MFMUTUAL FUND           '.
           05  FILLER  PIC X(24)  VALUE '  NO BOOK POSITION      '.
       01  WS-SECTYPE-TABLE REDEFINES WS-SECTYPE-VALUES.
           05  WS-STY-ENTRY OCCURS 8 TIMES INDEXED BY STY-IDX.
               10  WS-STY-CODE         PIC X(02).
               10  WS-STY-NAME         PIC X(22).
       01  WS-SECTYPE-TOTALS.
           05  WS-STT OCCURS 9 TIMES.
               10  WS-STT-COUNT        PIC S9(07)       COMP-3.
               10  WS-STT-MV           PIC S9(15)V99    COMP-3.
       01  WS-STY-SUB                  PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * SECTION (DEPOSITORY) AND REPORT TOTALS                         *
      *----------------------------------------------------------------*
       01  WS-SECTION-TOTALS.
           05  WS-ST-ITEMS             PIC S9(07)       COMP-3.
           05  WS-ST-MATCHED           PIC S9(07)       COMP-3.
           05  WS-ST-BREAKS            PIC S9(07)       COMP-3.
           05  WS-ST-STREET-QTY        PIC S9(15)V9(04) COMP-3.
           05  WS-ST-BOOK-QTY          PIC S9(15)V9(04) COMP-3.
           05  WS-ST-BREAK-MV          PIC S9(15)V99    COMP-3.
       01  WS-REPORT-TOTALS.
           05  WS-RT-ITEMS             PIC S9(07)       COMP-3 VALUE 0.
           05  WS-RT-MATCHED           PIC S9(07)       COMP-3 VALUE 0.
           05  WS-RT-BREAKS            PIC S9(07)       COMP-3 VALUE 0.
           05  WS-RT-BREAK-MV          PIC S9(15)V99    COMP-3 VALUE 0.
           05  WS-RT-NO-VALUE          PIC S9(07)       COMP-3 VALUE 0.
           05  WS-RT-NOT-ON-MASTER     PIC S9(07)       COMP-3 VALUE 0.
           05  WS-RT-ESCALATED         PIC S9(07)       COMP-3 VALUE 0.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINTED-CNT          PIC S9(09) COMP-3 VALUE ZERO.
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
       01  WS-SECTION-HEAD.
           05  SH-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  FILLER                  PIC X(12)  VALUE 'DEPOSITORY: '.
           05  SH-DEPO                 PIC X(04).
           05  FILLER                  PIC X(03)  VALUE ' - '.
           05  SH-NAME                 PIC X(30).
           05  FILLER                  PIC X(16)  VALUE
               'BOOK ACCOUNT: '.
           05  SH-ACCT                 PIC X(10).
           05  FILLER                  PIC X(20)  VALUE
               '   STATEMENT AS OF: '.
           05  SH-STMT-DATE            PIC X(10).
           05  FILLER                  PIC X(26)  VALUE SPACES.
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(11)  VALUE ' CUSIP'.
           05  FILLER  PIC X(13)  VALUE 'ISIN'.
           05  FILLER  PIC X(04)  VALUE 'CAT'.
           05  FILLER  PIC X(19)  VALUE '       STREET QTY'.
           05  FILLER  PIC X(19)  VALUE '         BOOK QTY'.
           05  FILLER  PIC X(19)  VALUE '       DIFFERENCE'.
           05  FILLER  PIC X(17)  VALUE '      VALUE USD'.
           05  FILLER  PIC X(11)  VALUE 'FIRST SEEN'.
           05  FILLER  PIC X(04)  VALUE 'AGE'.
           05  FILLER  PIC X(04)  VALUE 'ESC'.
           05  FILLER  PIC X(11)  VALUE 'OWNER'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(11)  VALUE ' ---------'.
           05  FILLER  PIC X(13)  VALUE '------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(17)  VALUE ' ---------------'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(11)  VALUE '--------'.
       01  WS-BREAK-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  BL-CUSIP                PIC X(09).
           05  BL-FLAG                 PIC X(01).
           05  FILLER                  PIC X(01).
           05  BL-ISIN                 PIC X(12).
           05  FILLER                  PIC X(01).
           05  BL-CAT                  PIC X(02).
           05  FILLER                  PIC X(01).
           05  BL-STREET               PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-BOOK                 PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-DIFF                 PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  BL-MV                   PIC -ZZZ,ZZZ,ZZ9.99.
           05  BL-MV-FLAG              PIC X(01).
           05  FILLER                  PIC X(01).
           05  BL-FIRST-SEEN           PIC X(10).
           05  FILLER                  PIC X(01).
           05  BL-AGE                  PIC ZZ9.
           05  FILLER                  PIC X(02).
           05  BL-ESC                  PIC 9.
           05  FILLER                  PIC X(02).
           05  BL-OWNER                PIC X(08).
           05  FILLER                  PIC X(06).
       01  WS-SECTION-TOTAL-LINE.
           05  STL-CC                  PIC X(01).
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  STL-LABEL               PIC X(24).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  STL-STREET              PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  STL-BOOK                PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  STL-MV                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(38)  VALUE SPACES.
       01  WS-COUNT-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(03)  VALUE SPACES.
           05  CL-LABEL                PIC X(32).
           05  CL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  CL-LABEL-2              PIC X(20).
           05  CL-MV                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(47)  VALUE SPACES.
       01  WS-NO-BREAK-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO BREAKS FOR THIS DEPOSITORY ***'.
       01  WS-FOOTNOTE-1.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(50)  VALUE
               ' * AFTER THE CUSIP = ISIN NOT ON THE SECURITY MAST'.
           05  FILLER                  PIC X(50)  VALUE
               'ER, CUSIP TAKEN FROM THE ISIN.   N AFTER THE VALUE'.
           05  FILLER                  PIC X(32)  VALUE
               ' = NO BOOK VALUATION.'.
       01  WS-FOOTNOTE-2.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(50)  VALUE
               ' DIFFERENCE = STREET - BOOK.  BOOK = -(SETTLED QTY'.
           05  FILLER                  PIC X(50)  VALUE
               ') OF THE STREET ACCOUNT ON THE PRIOR-DAY CLOSE (BK'.
           05  FILLER                  PIC X(32)  VALUE
               'UP.POSITION).'.
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
           PERFORM 2000-PROCESS-RESULT UNTIL END-OF-POSREC.
           IF SECTION-STARTED
               PERFORM 3000-END-SECTION
           END-IF.
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
           MOVE 'POSITION BREAK REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT POSREC-FILE.
           IF WS-POSREC-STATUS NOT = '00'
               MOVE 'POSREC' TO AB-DDNAME
               MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *    THE BREAK MASTER ONLY ADDS AGE AND OWNER - REPORT RUNS
      *    WITHOUT IT (E.G. RERUN WHILE THE CLUSTER IS BEING RESTORED)
           OPEN INPUT BRKMAST-FILE.
           IF WS-BRKMAST-STATUS = '00' OR WS-BRKMAST-STATUS = '97'
               MOVE 'Y' TO WS-BRKMAST-OPEN-SW
           ELSE
               DISPLAY 'RCR210 BREAK MASTER NOT AVAILABLE (STATUS '
                       WS-BRKMAST-STATUS ') - NO AGE / OWNER COLUMNS'
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           INITIALIZE WS-CAT-TOTALS WS-SECTYPE-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'STREET POSITION RECONCILIATION - BREAKS'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-POSREC.
           IF END-OF-POSREC
               PERFORM 8200-HEADINGS
               MOVE SPACES TO WS-COUNT-LINE
               MOVE '0' TO CL-CC
               MOVE '*** NO RECONCILIATION RESULTS TODAY ***'
                                    TO CL-LABEL
               WRITE RPT-RECORD FROM WS-COUNT-LINE
               PERFORM 8900-CHECK-WRITE
           END-IF.
      *================================================================*
      * ONE RESULT - SECTION BREAK ON DEPOSITORY                       *
      *================================================================*
       2000-PROCESS-RESULT.
           IF RPR-DEPOSITORY NOT = WS-CUR-DEPO
               IF SECTION-STARTED
                   PERFORM 3000-END-SECTION
               END-IF
               PERFORM 2100-START-SECTION
           END-IF.
           ADD 1 TO WS-ST-ITEMS WS-RT-ITEMS.
           ADD RPR-STREET-QTY TO WS-ST-STREET-QTY.
           ADD RPR-BOOK-QTY   TO WS-ST-BOOK-QTY.
           IF RPR-IS-BREAK
               PERFORM 2200-PRINT-BREAK
           ELSE
               ADD 1 TO WS-ST-MATCHED WS-RT-MATCHED
           END-IF.
           PERFORM 8000-READ-POSREC.
      *----------------------------------------------------------------*
       2100-START-SECTION.
      *----------------------------------------------------------------*
           MOVE RPR-DEPOSITORY TO WS-CUR-DEPO.
           MOVE RPR-STMT-DATE  TO WS-CUR-STMT-DATE.
           MOVE 'Y' TO WS-SECTION-SW.
           INITIALIZE WS-SECTION-TOTALS.
           PERFORM 8200-HEADINGS.
      *----------------------------------------------------------------*
       2200-PRINT-BREAK.
      *----------------------------------------------------------------*
           ADD 1 TO WS-ST-BREAKS WS-RT-BREAKS WS-PRINTED-CNT.
           ADD RPR-MKT-VALUE-USD TO WS-ST-BREAK-MV WS-RT-BREAK-MV.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES             TO WS-BREAK-LINE.
           MOVE ' '                TO BL-CC.
           MOVE RPR-CUSIP          TO BL-CUSIP.
           IF RPR-ID-FLAG = 'N'
               MOVE '*'            TO BL-FLAG
               ADD 1 TO WS-RT-NOT-ON-MASTER
           END-IF.
           MOVE RPR-ISIN           TO BL-ISIN.
           MOVE RPR-RESULT         TO BL-CAT.
           MOVE RPR-STREET-QTY     TO BL-STREET.
           MOVE RPR-BOOK-QTY       TO BL-BOOK.
           MOVE RPR-DIFF-QTY       TO BL-DIFF.
           MOVE RPR-MKT-VALUE-USD  TO BL-MV.
           IF RPR-NO-VALUATION
               MOVE 'N'            TO BL-MV-FLAG
               ADD 1 TO WS-RT-NO-VALUE
           END-IF.
           PERFORM 2300-BREAK-MASTER-INFO.
           SET CT-IDX TO 1.
           SEARCH WS-CT-ENTRY
               AT END
                   CONTINUE
               WHEN WS-CT-CODE (CT-IDX) = RPR-RESULT
                   SET WS-SUB TO CT-IDX
                   ADD 1 TO WS-CTT-COUNT (WS-SUB)
                   ADD RPR-MKT-VALUE-USD TO WS-CTT-MV (WS-SUB)
                   IF BRKMAST-IS-OPEN AND BRKMAST-OK
                   AND RBM-AGE-BUS-DAYS = ZERO
                       ADD 1 TO WS-CTT-NEW (WS-SUB)
                   END-IF
           END-SEARCH.
           PERFORM 2250-ADD-SECURITY-TYPE.
           WRITE RPT-RECORD FROM WS-BREAK-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
      * SECURITY TYPE OF THE BOOK ROW - ROW 9 = TYPE NOT IN THE TABLE  *
      *----------------------------------------------------------------*
       2250-ADD-SECURITY-TYPE.
           MOVE 9 TO WS-STY-SUB.
           SET STY-IDX TO 1.
           SEARCH WS-STY-ENTRY
               AT END
                   CONTINUE
               WHEN WS-STY-CODE (STY-IDX) = RPR-SEC-TYPE
                   SET WS-STY-SUB TO STY-IDX
           END-SEARCH.
           ADD 1                 TO WS-STT-COUNT (WS-STY-SUB).
           ADD RPR-MKT-VALUE-USD TO WS-STT-MV (WS-STY-SUB).
      *----------------------------------------------------------------*
      * AGE / ESCALATION / OWNER FROM THE BREAK MASTER                 *
      *----------------------------------------------------------------*
       2300-BREAK-MASTER-INFO.
           MOVE '99' TO WS-BRKMAST-STATUS.
           IF NOT BRKMAST-IS-OPEN
               MOVE 'N/A'           TO BL-FIRST-SEEN
           ELSE
               MOVE SPACES          TO RBM-KEY
               MOVE 'SP'            TO RBM-BREAK-CLASS
               MOVE RPR-DEPOSITORY  TO RBM-DEPOSITORY
               MOVE RPR-CUSIP       TO RBM-ITEM-ID
               READ BRKMAST-FILE
               EVALUATE TRUE
                   WHEN BRKMAST-OK
                       MOVE RBM-FIRST-SEEN-DATE TO WS-DATE-IN
                       PERFORM 8300-EDIT-DATE
                       MOVE WS-DATE-EDIT        TO BL-FIRST-SEEN
                       MOVE RBM-AGE-BUS-DAYS    TO BL-AGE
                       MOVE RBM-ESCALATION-LVL  TO BL-ESC
                       MOVE RBM-ASSIGNED-TO     TO BL-OWNER
                       IF RBM-ESCALATION-LVL > ZERO
                           ADD 1 TO WS-RT-ESCALATED
                       END-IF
                   WHEN BRKMAST-NOT-FOUND
                       MOVE 'NOT ON BM'         TO BL-FIRST-SEEN
                   WHEN OTHER
                       MOVE 'RCBRKMST' TO AB-DDNAME
                       MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE RBM-KEY TO AB-KEY
                       MOVE 'BREAK MASTER READ FAILED' TO AB-MESSAGE
                       PERFORM 9999-ABEND
               END-EVALUATE
           END-IF.
      *================================================================*
      * DEPOSITORY TOTALS                                              *
      *================================================================*
       3000-END-SECTION.
           IF WS-ST-BREAKS = ZERO
               WRITE RPT-RECORD FROM WS-NO-BREAK-LINE
               PERFORM 8900-CHECK-WRITE
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           MOVE SPACES TO WS-SECTION-TOTAL-LINE.
           MOVE '0'    TO STL-CC.
           STRING 'TOTAL ' WS-CUR-DEPO DELIMITED BY SIZE INTO STL-LABEL.
           MOVE WS-ST-STREET-QTY TO STL-STREET.
           MOVE WS-ST-BOOK-QTY   TO STL-BOOK.
           MOVE WS-ST-BREAK-MV   TO STL-MV.
           WRITE RPT-RECORD FROM WS-SECTION-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'ITEMS COMPARED'  TO CL-LABEL.
           MOVE WS-ST-ITEMS       TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'MATCHED'         TO CL-LABEL.
           MOVE WS-ST-MATCHED     TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BREAKS'          TO CL-LABEL.
           MOVE WS-ST-BREAKS      TO CL-COUNT.
           MOVE 'BREAK VALUE USD'  TO CL-LABEL-2.
           MOVE WS-ST-BREAK-MV    TO CL-MV.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 4 TO RPT-LINE-COUNT.
      *================================================================*
      * REPORT SUMMARY BY CATEGORY                                     *
      *================================================================*
       4000-SUMMARY.
           MOVE 'SP' TO WS-CUR-DEPO.
           MOVE 'N' TO WS-SECTION-SW.
           PERFORM 8200-HEADINGS.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE '0'    TO CL-CC.
           MOVE 'SUMMARY - ALL DEPOSITORIES' TO CL-LABEL.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 3
               MOVE SPACES TO WS-COUNT-LINE
               IF WS-SUB = 1
                   MOVE '0' TO CL-CC
               ELSE
                   MOVE ' ' TO CL-CC
               END-IF
               STRING WS-CT-CODE (WS-SUB) ' ' WS-CT-NAME (WS-SUB)
                      DELIMITED BY SIZE INTO CL-LABEL
               MOVE WS-CTT-COUNT (WS-SUB) TO CL-COUNT
               MOVE 'VALUE USD'           TO CL-LABEL-2
               MOVE WS-CTT-MV (WS-SUB)    TO CL-MV
               WRITE RPT-RECORD FROM WS-COUNT-LINE
               PERFORM 8900-CHECK-WRITE
               MOVE SPACES TO WS-COUNT-LINE
               MOVE ' '    TO CL-CC
               MOVE '     OF WHICH NEW TODAY'  TO CL-LABEL
               MOVE WS-CTT-NEW (WS-SUB)   TO CL-COUNT
               WRITE RPT-RECORD FROM WS-COUNT-LINE
               PERFORM 8900-CHECK-WRITE
           END-PERFORM.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE '0'    TO CL-CC.
           MOVE 'ITEMS COMPARED'          TO CL-LABEL.
           MOVE WS-RT-ITEMS               TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'MATCHED'                 TO CL-LABEL.
           MOVE WS-RT-MATCHED             TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BREAKS'                  TO CL-LABEL.
           MOVE WS-RT-BREAKS              TO CL-COUNT.
           MOVE 'BREAK VALUE USD'         TO CL-LABEL-2.
           MOVE WS-RT-BREAK-MV            TO CL-MV.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BREAKS ESCALATED (LEVEL 1+)' TO CL-LABEL.
           MOVE WS-RT-ESCALATED           TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'BREAKS WITH NO BOOK VALUE'   TO CL-LABEL.
           MOVE WS-RT-NO-VALUE            TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           MOVE 'ISIN NOT ON SECURITY MASTER' TO CL-LABEL.
           MOVE WS-RT-NOT-ON-MASTER       TO CL-COUNT.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE '-'    TO CL-CC.
           MOVE 'BREAKS BY SECURITY TYPE' TO CL-LABEL.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           PERFORM VARYING WS-STY-SUB FROM 1 BY 1 UNTIL WS-STY-SUB > 9
               IF WS-STT-COUNT (WS-STY-SUB) > ZERO
                   MOVE SPACES TO WS-COUNT-LINE
                   MOVE ' '    TO CL-CC
                   IF WS-STY-SUB < 9
                       STRING '  ' WS-STY-CODE (WS-STY-SUB) ' '
                              WS-STY-NAME (WS-STY-SUB)
                              DELIMITED BY SIZE INTO CL-LABEL
                   ELSE
                       MOVE '  ?? OTHER TYPE' TO CL-LABEL
                   END-IF
                   MOVE WS-STT-COUNT (WS-STY-SUB) TO CL-COUNT
                   MOVE 'VALUE USD'                TO CL-LABEL-2
                   MOVE WS-STT-MV (WS-STY-SUB)     TO CL-MV
                   WRITE RPT-RECORD FROM WS-COUNT-LINE
                   PERFORM 8900-CHECK-WRITE
               END-IF
           END-PERFORM.
           WRITE RPT-RECORD FROM WS-FOOTNOTE-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM WS-FOOTNOTE-2.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
       8000-READ-POSREC.
      *================================================================*
           READ POSREC-FILE.
           EVALUATE TRUE
               WHEN POSREC-OK
                   ADD 1 TO WS-READ-CNT
               WHEN POSREC-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'POSREC' TO AB-DDNAME
                   MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
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
           MOVE 2 TO RPT-LINE-COUNT.
           IF SECTION-STARTED
               MOVE WS-CUR-DEPO TO SH-DEPO
               MOVE SPACES      TO SH-NAME SH-ACCT
               SET DN-IDX TO 1
               SEARCH WS-DN-ENTRY
                   AT END
                       MOVE 'UNKNOWN DEPOSITORY' TO SH-NAME
                   WHEN WS-DN-DEPO (DN-IDX) = WS-CUR-DEPO
                       MOVE WS-DN-NAME (DN-IDX) TO SH-NAME
                       MOVE WS-DN-ACCT (DN-IDX) TO SH-ACCT
               END-SEARCH
               MOVE WS-CUR-STMT-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE
               MOVE WS-DATE-EDIT TO SH-STMT-DATE
               WRITE RPT-RECORD FROM WS-SECTION-HEAD
               PERFORM 8900-CHECK-WRITE
               WRITE RPT-RECORD FROM WS-COL-HEAD-1
               PERFORM 8900-CHECK-WRITE
               WRITE RPT-RECORD FROM WS-COL-HEAD-2
               PERFORM 8900-CHECK-WRITE
               ADD 5 TO RPT-LINE-COUNT
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
           CLOSE POSREC-FILE RPTFILE.
           IF BRKMAST-IS-OPEN
               CLOSE BRKMAST-FILE
           END-IF.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'RCR210'        TO CT-STAGE.
           MOVE 'RECORDS-IN'    TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE ZERO            TO CT-AMOUNT CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE 'BREAKS-PRINTED' TO CT-COUNTER-NAME.
           MOVE WS-PRINTED-CNT  TO CT-COUNT.
           MOVE WS-RT-BREAK-MV  TO CT-AMOUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'RCR210 RESULTS READ        : ' WS-READ-CNT.
           DISPLAY 'RCR210 BREAKS PRINTED      : ' WS-PRINTED-CNT.
           DISPLAY 'RCR210 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'POSITION BREAK REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'RCR210 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
