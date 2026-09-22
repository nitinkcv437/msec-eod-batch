       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRR410.
       AUTHOR.        J L REYES.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JANUARY 2021.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRR410                                            *
      * DESCRIPTION: DAILY TAX ACTIVITY REPORT.                        *
      *              LISTS EVERY TRANSACTION RRB400 APPLIED TO THE TAX *
      *              YEAR-TO-DATE MASTER TODAY, BY FORM AND ACCOUNT,   *
      *              WITH THE ACCOUNT'S YEAR-TO-DATE BOXES READ BACK   *
      *              FROM THE MASTER AFTER THE UPDATE.  FORM TOTALS    *
      *              AND A SUMMARY BY TAX CATEGORY FOLLOW.             *
      *              TAX OPERATIONS USES THE REPORT TO SPOT-CHECK THE  *
      *              ACCUMULATION BEFORE YEAR END.                     *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRRD030 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              TAXIN    - TAX ACTIVITY SORTED BY FORM / ACCOUNT  *
      *                         / SOURCE / REF (&&TAXSRT, RRTAXA)      *
      *              TAXYTD   - MSEC.PROD.RR.TAXYTD.KSDS  (RRTAXY)     *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0 CLEAN, 4 YTD ROW NOT FOUND FOR AN ACCOUNT       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2021-01-11 JLR  ORIGINAL                              CHG35610 *
      * 2022-02-14 JLR  YTD BOXES AFTER EACH ACCOUNT          CHG36802 *
      * 2023-10-02 MHC  1042 SECTION SEPARATE PAGE            CHG39015 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT TAXIN-FILE     ASSIGN TO TAXIN
                  FILE STATUS IS WS-TAXIN-STATUS.
           SELECT TAXYTD-FILE    ASSIGN TO TAXYTD
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS RTX-KEY
                  FILE STATUS IS WS-TAXYTD-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  TAXIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RRTAXA.
       FD  TAXYTD-FILE.
       COPY RRTAXY.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRR410'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-TAXIN-STATUS         PIC X(02)  VALUE '00'.
               88  TAXIN-OK                       VALUE '00'.
               88  TAXIN-EOF                      VALUE '10'.
           05  WS-TAXYTD-STATUS        PIC X(02)  VALUE '00'.
               88  TAXYTD-OK                      VALUE '00'.
               88  TAXYTD-NOTFND                  VALUE '23'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-ACTIVITY                VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-RECORD                   VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * CONTROL BREAKS                                                 *
      *----------------------------------------------------------------*
       01  WS-PREV-FORM                PIC X(04)  VALUE LOW-VALUES.
       01  WS-PREV-ACCT                PIC X(10)  VALUE LOW-VALUES.
       01  WS-PREV-YEAR                PIC 9(04)  VALUE ZERO.
       01  WS-ACCT-TOTALS.
           05  WS-AT-COUNT             PIC S9(07)       COMP-3.
           05  WS-AT-AMOUNT            PIC S9(15)V99    COMP-3.
           05  WS-AT-PL                PIC S9(15)V99    COMP-3.
       01  WS-FORM-TOTALS.
           05  WS-FT-ACCTS             PIC S9(07)       COMP-3.
           05  WS-FT-COUNT             PIC S9(07)       COMP-3.
           05  WS-FT-AMOUNT            PIC S9(15)V99    COMP-3.
           05  WS-FT-PL                PIC S9(15)V99    COMP-3.
       01  WS-RUN-TOTALS.
           05  WS-RN-READ              PIC S9(09)       COMP-3 VALUE 0.
           05  WS-RN-ACCTS             PIC S9(09)       COMP-3 VALUE 0.
           05  WS-RN-AMOUNT            PIC S9(15)V99    COMP-3 VALUE 0.
           05  WS-RN-YTD-MISSING       PIC S9(09)       COMP-3 VALUE 0.
      *----------------------------------------------------------------*
      * SUMMARY BY FORM AND CATEGORY                                   *
      *----------------------------------------------------------------*
       01  WS-CATEGORY-VALUES.
           05  FILLER  PIC X(34)  VALUE
               'ODIV ORDINARY DIVIDENDS (NON-QUAL)'.
           05  FILLER  PIC X(34)  VALUE
               'QDIV QUALIFIED DIVIDENDS          '.
           05  FILLER  PIC X(34)  VALUE
               'FTAX FOREIGN TAX PAID             '.
           05  FILLER  PIC X(34)  VALUE
               '42GI 1042 GROSS INCOME            '.
           05  FILLER  PIC X(34)  VALUE
               '42TW 1042 TAX WITHHELD            '.
           05  FILLER  PIC X(34)  VALUE
               'CILP CASH IN LIEU PROCEEDS        '.
           05  FILLER  PIC X(34)  VALUE
               'MRGP CASH MERGER PROCEEDS         '.
           05  FILLER  PIC X(34)  VALUE
               'SALE GROSS PROCEEDS OF SALES      '.
       01  WS-CATEGORY-TABLE REDEFINES WS-CATEGORY-VALUES.
           05  WS-CAT-ENTRY            OCCURS 8 TIMES.
               10  WS-CAT-CODE         PIC X(04).
               10  FILLER              PIC X(01).
               10  WS-CAT-DESC         PIC X(29).
       01  WS-CATEGORY-TOTALS.
           05  WS-CT-FORM              OCCURS 2 TIMES.
               10  WS-CT-CAT           OCCURS 8 TIMES.
                   15  WS-CT-COUNT     PIC S9(07)       COMP-3.
                   15  WS-CT-AMOUNT    PIC S9(15)V99    COMP-3.
       01  WS-FORM-NAMES.
           05  FILLER                  PIC X(04)  VALUE '1042'.
           05  FILLER                  PIC X(04)  VALUE '1099'.
       01  WS-FORM-TABLE REDEFINES WS-FORM-NAMES.
           05  WS-FORM-NAME            PIC X(04)  OCCURS 2 TIMES.
       01  WS-FORM-SUB                 PIC S9(04) COMP.
       01  WS-EDIT-ACCTS               PIC ZZZ,ZZ9.
       01  WS-CAT-SUB                  PIC S9(04) COMP.
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
       01  WS-FORM-HEAD.
           05  FH-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  FILLER                  PIC X(06)  VALUE 'FORM '.
           05  FH-FORM                 PIC X(04).
           05  FILLER                  PIC X(03)  VALUE ' - '.
           05  FH-FORM-DESC            PIC X(40).
           05  FILLER                  PIC X(78)  VALUE SPACES.
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(12)  VALUE ' ACCOUNT'.
           05  FILLER  PIC X(04)  VALUE 'SR'.
           05  FILLER  PIC X(18)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(05)  VALUE 'TYP'.
           05  FILLER  PIC X(11)  VALUE 'CUSIP'.
           05  FILLER  PIC X(05)  VALUE 'CAT'.
           05  FILLER  PIC X(21)  VALUE '           AMOUNT USD'.
           05  FILLER  PIC X(21)  VALUE '         REALIZED P&L'.
           05  FILLER  PIC X(21)  VALUE '      ORIGINAL AMOUNT'.
           05  FILLER  PIC X(14)  VALUE ' CCY  ISS ST'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-SOURCE               PIC X(02).
           05  FILLER                  PIC X(02).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(02).
           05  DL-TYPE                 PIC X(03).
           05  FILLER                  PIC X(02).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(02).
           05  DL-CATEGORY             PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-PL                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-ORIG                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(02).
           05  DL-ISSUER               PIC X(02).
           05  FILLER                  PIC X(02).
           05  DL-SEC-TYPE             PIC X(02).
           05  FILLER                  PIC X(03).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  TL-LABEL                PIC X(46).
           05  TL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  TL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  TL-PL                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(31).
       01  WS-YTD-LINE-1.
           05  Y1-CC                   PIC X(01).
           05  FILLER                  PIC X(12)  VALUE SPACES.
           05  Y1-LABEL-1              PIC X(10).
           05  Y1-VALUE-1              PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  Y1-LABEL-2              PIC X(10).
           05  Y1-VALUE-2              PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  Y1-LABEL-3              PIC X(10).
           05  Y1-VALUE-3              PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  Y1-LABEL-4              PIC X(10).
           05  Y1-VALUE-4              PIC -ZZZ,ZZZ,ZZ9.99.
           05  Y1-VALUE-4-X REDEFINES Y1-VALUE-4
                                       PIC X(15).
           05  FILLER                  PIC X(14)  VALUE SPACES.
       01  WS-SUMMARY-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  SM-FORM                 PIC X(04).
           05  FILLER                  PIC X(03).
           05  SM-CODE                 PIC X(04).
           05  FILLER                  PIC X(02).
           05  SM-DESC                 PIC X(29).
           05  SM-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  SM-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(57).
       01  WS-MESSAGE-LINE.
           05  ML-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  ML-TEXT                 PIC X(127).
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
           PERFORM 8000-READ-ACTIVITY.
           IF END-OF-ACTIVITY
               MOVE SPACES TO WS-MESSAGE-LINE
               MOVE '0'    TO ML-CC
               MOVE '*** NO TAX-REPORTABLE ACTIVITY TODAY ***'
                           TO ML-TEXT
               PERFORM 8100-PRINT-MESSAGE
           END-IF.
           PERFORM UNTIL END-OF-ACTIVITY
               PERFORM 2000-PROCESS-ACTIVITY
               PERFORM 8000-READ-ACTIVITY
           END-PERFORM.
           IF NOT FIRST-RECORD
               PERFORM 3100-ACCOUNT-BREAK
               PERFORM 3200-FORM-BREAK
           END-IF.
           PERFORM 4000-CATEGORY-SUMMARY.
           PERFORM 9000-TERMINATE.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
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
           MOVE 'TAX ACTIVITY REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT TAXIN-FILE.
           IF WS-TAXIN-STATUS NOT = '00'
               MOVE 'TAXIN'            TO AB-DDNAME
               MOVE WS-TAXIN-STATUS    TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT TAXYTD-FILE.
           IF WS-TAXYTD-STATUS NOT = '00'
               MOVE 'TAXYTD'           TO AB-DDNAME
               MOVE WS-TAXYTD-STATUS   TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS  TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           INITIALIZE WS-ACCT-TOTALS WS-FORM-TOTALS WS-CATEGORY-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'DAILY TAX ACTIVITY - YEAR-TO-DATE ACCUMULATION'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           MOVE WS-DI-CCYY     TO WS-DE-CCYY.
           MOVE WS-DI-MM       TO WS-DE-MM.
           MOVE WS-DI-DD       TO WS-DE-DD.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
      *================================================================*
      * ONE TAX ACTIVITY RECORD                                        *
      *================================================================*
       2000-PROCESS-ACTIVITY.
           IF FIRST-RECORD
               MOVE 'N' TO WS-FIRST-SW
               PERFORM 3300-FORM-START
               PERFORM 3400-ACCOUNT-START
           ELSE
               IF RTA-FORM NOT = WS-PREV-FORM
                   PERFORM 3100-ACCOUNT-BREAK
                   PERFORM 3200-FORM-BREAK
                   PERFORM 3300-FORM-START
                   PERFORM 3400-ACCOUNT-START
               ELSE
                   IF RTA-ACCT-NO NOT = WS-PREV-ACCT
                       PERFORM 3100-ACCOUNT-BREAK
                       PERFORM 3400-ACCOUNT-START
                   END-IF
               END-IF
           END-IF.
           MOVE SPACES          TO WS-DETAIL-LINE.
           MOVE ' '             TO DL-CC.
           IF WS-AT-COUNT = ZERO
               MOVE RTA-ACCT-NO TO DL-ACCT
           END-IF.
           MOVE RTA-SOURCE      TO DL-SOURCE.
           MOVE RTA-REF         TO DL-REF.
           MOVE RTA-ACT-TYPE    TO DL-TYPE.
           MOVE RTA-CUSIP       TO DL-CUSIP.
           MOVE RTA-CATEGORY    TO DL-CATEGORY.
           MOVE RTA-AMOUNT-USD  TO DL-AMOUNT.
           IF RTA-REALIZED-PL NOT = ZERO
               MOVE RTA-REALIZED-PL TO DL-PL
           END-IF.
           IF RTA-ORIG-CCY NOT = 'USD'
               MOVE RTA-ORIG-AMOUNT TO DL-ORIG
           END-IF.
           MOVE RTA-ORIG-CCY    TO DL-CCY.
           MOVE RTA-ISSUER-CTRY TO DL-ISSUER.
           MOVE RTA-SEC-TYPE    TO DL-SEC-TYPE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
               MOVE RTA-ACCT-NO TO DL-ACCT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
           ADD 1               TO WS-AT-COUNT.
           ADD RTA-AMOUNT-USD  TO WS-AT-AMOUNT.
           ADD RTA-REALIZED-PL TO WS-AT-PL.
           ADD RTA-AMOUNT-USD  TO WS-RN-AMOUNT.
           PERFORM 2100-ADD-CATEGORY.
      *----------------------------------------------------------------*
       2100-ADD-CATEGORY.
      *----------------------------------------------------------------*
           IF RTA-FORM = '1042'
               MOVE 1 TO WS-FORM-SUB
           ELSE
               MOVE 2 TO WS-FORM-SUB
           END-IF.
           PERFORM VARYING WS-CAT-SUB FROM 1 BY 1
                   UNTIL WS-CAT-SUB > 8
               IF WS-CAT-CODE (WS-CAT-SUB) = RTA-CATEGORY
                   ADD 1 TO WS-CT-COUNT (WS-FORM-SUB WS-CAT-SUB)
                   ADD RTA-AMOUNT-USD
                       TO WS-CT-AMOUNT (WS-FORM-SUB WS-CAT-SUB)
               END-IF
           END-PERFORM.
      *================================================================*
      * CONTROL BREAKS                                                 *
      *================================================================*
       3100-ACCOUNT-BREAK.
           MOVE SPACES           TO WS-TOTAL-LINE.
           MOVE ' '              TO TL-CC.
           MOVE '  ACCOUNT TOTAL TODAY' TO TL-LABEL.
           MOVE WS-AT-COUNT      TO TL-COUNT.
           MOVE WS-AT-AMOUNT     TO TL-AMOUNT.
           MOVE WS-AT-PL         TO TL-PL.
           PERFORM 8150-PRINT-TOTAL.
           PERFORM 3150-PRINT-YTD.
           ADD 1            TO WS-FT-ACCTS WS-RN-ACCTS.
           ADD WS-AT-COUNT  TO WS-FT-COUNT.
           ADD WS-AT-AMOUNT TO WS-FT-AMOUNT.
           ADD WS-AT-PL     TO WS-FT-PL.
           INITIALIZE WS-ACCT-TOTALS.
      *----------------------------------------------------------------*
      * YEAR-TO-DATE BOXES AS NOW ON THE MASTER                        *
      *----------------------------------------------------------------*
       3150-PRINT-YTD.
           MOVE WS-PREV-ACCT TO RTX-ACCT-NO.
           MOVE WS-PREV-YEAR TO RTX-TAX-YEAR.
           MOVE WS-PREV-FORM TO RTX-FORM.
           READ TAXYTD-FILE.
           EVALUATE TRUE
               WHEN TAXYTD-OK
                   CONTINUE
               WHEN TAXYTD-NOTFND
                   ADD 1 TO WS-RN-YTD-MISSING
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
                   MOVE SPACES TO WS-MESSAGE-LINE
                   MOVE ' '    TO ML-CC
                   MOVE '      *** YEAR-TO-DATE ROW NOT ON THE MASTER'
                               TO ML-TEXT
                   PERFORM 8100-PRINT-MESSAGE
                   EXIT PARAGRAPH
               WHEN OTHER
                   MOVE 'TAXYTD'           TO AB-DDNAME
                   MOVE WS-TAXYTD-STATUS   TO AB-FILE-STATUS
                   MOVE 1002               TO AB-ABEND-CODE
                   MOVE RTX-KEY            TO AB-KEY
                   MOVE 'READ FAILED'      TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
           MOVE SPACES TO WS-YTD-LINE-1.
           MOVE ' '    TO Y1-CC.
           IF RTX-FORM = '1042'
               MOVE 'YTD INCOME' TO Y1-LABEL-1
               MOVE RTX-1042-GROSS-INCOME TO Y1-VALUE-1
               MOVE 'WITHHELD'   TO Y1-LABEL-2
               MOVE RTX-1042-TAX-WITHHELD TO Y1-VALUE-2
           ELSE
               MOVE 'YTD ORD'    TO Y1-LABEL-1
               MOVE RTX-ORD-DIVIDENDS  TO Y1-VALUE-1
               MOVE 'QUALIFIED'  TO Y1-LABEL-2
               MOVE RTX-QUAL-DIVIDENDS TO Y1-VALUE-2
           END-IF.
           MOVE 'FOR TAX'    TO Y1-LABEL-3.
           MOVE RTX-FOREIGN-TAX-PAID TO Y1-VALUE-3.
           MOVE 'TXNS'       TO Y1-LABEL-4.
           MOVE RTX-TXN-COUNT TO WS-EDIT-ACCTS.
           MOVE WS-EDIT-ACCTS TO Y1-VALUE-4-X.
           PERFORM 8160-PRINT-YTD.
           MOVE SPACES TO WS-YTD-LINE-1.
           MOVE ' '    TO Y1-CC.
           MOVE 'PROCEEDS'   TO Y1-LABEL-1.
           MOVE RTX-GROSS-PROCEEDS TO Y1-VALUE-1.
           MOVE 'COST'       TO Y1-LABEL-2.
           MOVE RTX-COST-BASIS TO Y1-VALUE-2.
           MOVE 'REAL P&L'   TO Y1-LABEL-3.
           MOVE RTX-REALIZED-PL TO Y1-VALUE-3.
           MOVE 'CIL+MRGR'   TO Y1-LABEL-4.
           COMPUTE Y1-VALUE-4 = RTX-CIL-PROCEEDS + RTX-MERGER-PROCEEDS.
           PERFORM 8160-PRINT-YTD.
      *----------------------------------------------------------------*
       3200-FORM-BREAK.
      *----------------------------------------------------------------*
           MOVE SPACES           TO WS-TOTAL-LINE.
           MOVE '0'              TO TL-CC.
           MOVE WS-FT-ACCTS      TO WS-EDIT-ACCTS.
           STRING 'FORM ' WS-PREV-FORM ' TOTAL - ACCOUNTS: '
                  WS-EDIT-ACCTS
                  DELIMITED BY SIZE INTO TL-LABEL
           END-STRING.
           MOVE WS-FT-COUNT      TO TL-COUNT.
           MOVE WS-FT-AMOUNT     TO TL-AMOUNT.
           MOVE WS-FT-PL         TO TL-PL.
           PERFORM 8150-PRINT-TOTAL.
           ADD 1 TO RPT-LINE-COUNT.
           INITIALIZE WS-FORM-TOTALS.
      *----------------------------------------------------------------*
       3300-FORM-START.
      *----------------------------------------------------------------*
           MOVE RTA-FORM TO WS-PREV-FORM FH-FORM.
           IF RTA-FORM = '1042'
               MOVE 'FOREIGN PERSON U.S. SOURCE INCOME' TO FH-FORM-DESC
           ELSE
               MOVE 'DIVIDENDS AND BROKER PROCEEDS' TO FH-FORM-DESC
           END-IF.
           PERFORM 8200-HEADINGS.
      *----------------------------------------------------------------*
       3400-ACCOUNT-START.
      *----------------------------------------------------------------*
           MOVE RTA-ACCT-NO  TO WS-PREV-ACCT.
           MOVE RTA-TAX-YEAR TO WS-PREV-YEAR.
           IF RPT-LINE-COUNT + 5 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
      *================================================================*
      * SUMMARY BY FORM AND TAX CATEGORY                               *
      *================================================================*
       4000-CATEGORY-SUMMARY.
           MOVE 'DAILY TAX ACTIVITY - SUMMARY BY CATEGORY'
                               TO RPT-H2-TITLE.
           PERFORM 8250-PAGE-HEADINGS.
           PERFORM VARYING WS-FORM-SUB FROM 1 BY 1
                   UNTIL WS-FORM-SUB > 2
               PERFORM VARYING WS-CAT-SUB FROM 1 BY 1
                       UNTIL WS-CAT-SUB > 8
                   MOVE SPACES TO WS-SUMMARY-LINE
                   MOVE ' '    TO SM-CC
                   IF WS-CAT-SUB = 1
                       MOVE '0' TO SM-CC
                       MOVE WS-FORM-NAME (WS-FORM-SUB) TO SM-FORM
                       ADD 1 TO RPT-LINE-COUNT
                   END-IF
                   MOVE WS-CAT-CODE (WS-CAT-SUB) TO SM-CODE
                   MOVE WS-CAT-DESC (WS-CAT-SUB) TO SM-DESC
                   MOVE WS-CT-COUNT (WS-FORM-SUB WS-CAT-SUB)
                                               TO SM-COUNT
                   MOVE WS-CT-AMOUNT (WS-FORM-SUB WS-CAT-SUB)
                                               TO SM-AMOUNT
                   WRITE RPT-RECORD FROM WS-SUMMARY-LINE
                   PERFORM 8900-CHECK-WRITE
                   ADD 1 TO RPT-LINE-COUNT
               END-PERFORM
           END-PERFORM.
           MOVE SPACES           TO WS-TOTAL-LINE.
           MOVE '-'              TO TL-CC.
           MOVE 'ALL FORMS - TRANSACTIONS / AMOUNT USD' TO TL-LABEL.
           MOVE WS-RN-READ       TO TL-COUNT.
           MOVE WS-RN-AMOUNT     TO TL-AMOUNT.
           PERFORM 8150-PRINT-TOTAL.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-ACTIVITY.
           READ TAXIN-FILE.
           EVALUATE TRUE
               WHEN TAXIN-OK
                   ADD 1 TO WS-RN-READ
                   IF RTA-AMOUNT-USD NOT NUMERIC
                       MOVE ZERO TO RTA-AMOUNT-USD
                   END-IF
                   IF RTA-REALIZED-PL NOT NUMERIC
                       MOVE ZERO TO RTA-REALIZED-PL
                   END-IF
               WHEN TAXIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'TAXIN'            TO AB-DDNAME
                   MOVE WS-TAXIN-STATUS    TO AB-FILE-STATUS
                   MOVE 1002               TO AB-ABEND-CODE
                   MOVE 'READ FAILED'      TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8100-PRINT-MESSAGE.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-MESSAGE-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8150-PRINT-TOTAL.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8160-PRINT-YTD.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-YTD-LINE-1.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           PERFORM 8250-PAGE-HEADINGS.
           IF WS-PREV-FORM NOT = LOW-VALUES
               WRITE RPT-RECORD FROM WS-FORM-HEAD
               PERFORM 8900-CHECK-WRITE
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8250-PAGE-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE.
           MOVE 2 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS  TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE TAXIN-FILE TAXYTD-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'RRR410'        TO CT-STAGE.
           MOVE 'TAXACT-IN'     TO CT-COUNTER-NAME.
           MOVE WS-RN-READ      TO CT-COUNT.
           MOVE WS-RN-AMOUNT    TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'RRR410 TAX ACTIVITY READ : ' WS-RN-READ.
           DISPLAY 'RRR410 ACCOUNTS          : ' WS-RN-ACCTS.
           DISPLAY 'RRR410 YTD ROWS MISSING  : ' WS-RN-YTD-MISSING.
           DISPLAY 'RRR410 AMOUNT USD        : ' WS-RN-AMOUNT.
           DISPLAY 'RRR410 RETURN CODE       : ' WS-RETURN-CODE.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'I'             TO AU-SEVERITY.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W'         TO AU-SEVERITY
           END-IF.
           MOVE 'TAX ACTIVITY REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'RRR410 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
