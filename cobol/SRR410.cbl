       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR410.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MAY 1996.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR410                                            *
      * DESCRIPTION: DAILY VALUATION REPORT BY ACCOUNT.                *
      *              ONE SECTION PER ACCOUNT WITH EVERY OPEN POSITION, *
      *              PRICE, MARKET VALUE IN USD, COST, UNREALIZED P&L  *
      *              AND ACCRUED INTEREST.  STALE PRICES MARKED '*'.   *
      *              ACCOUNT TOTALS, GRAND TOTALS AND A SUMMARY BY     *
      *              SECURITY TYPE.                                    *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD050 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              VALIN    - MSEC.PROD.SR.VALUATION(+1)   (SRVALUE) *
      *                         (WRITTEN IN POSITION KEY ORDER, I.E.   *
      *                         ACCOUNT / CUSIP / LOCATION - NO SORT)  *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0, 4 WHEN STALE PRICES WERE REPORTED              *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1996-05-13 DWB  ORIGINAL                              CHG02215 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-07-16 KAP  DECIMALIZATION - 6 DECIMAL PRICE      CHG08811 *
      * 2003-06-02 KAP  ACCRUED INTEREST COLUMN               CHG11244 *
      * 2009-12-14 SPA  USD MARKET VALUE, CCY COLUMN          CHG19002 *
      * 2011-08-29 SPA  STALE PRICE MARKER                    CHG22190 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT VALIN-FILE     ASSIGN TO VALIN
                  FILE STATUS IS WS-VALIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  VALIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRVALUE.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR410'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-VALIN-STATUS         PIC X(02)  VALUE '00'.
               88  VALIN-OK                       VALUE '00'.
               88  VALIN-EOF                      VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-VALUES                  VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-ACCOUNT                  VALUE 'Y'.
       01  WS-PREV-ACCT                PIC X(10)  VALUE LOW-VALUES.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * ACCOUNT AND GRAND TOTALS                                       *
      *----------------------------------------------------------------*
       01  WS-ACCT-TOTALS.
           05  WS-AT-POSITIONS         PIC S9(07)       COMP-3.
           05  WS-AT-MV-USD            PIC S9(15)V99    COMP-3.
           05  WS-AT-COST              PIC S9(15)V99    COMP-3.
           05  WS-AT-UNRLZD            PIC S9(15)V99    COMP-3.
           05  WS-AT-ACCRUED           PIC S9(13)V99    COMP-3.
           05  WS-AT-STALE             PIC S9(07)       COMP-3.
           05  WS-AT-NON-USD           PIC S9(07)       COMP-3.
       01  WS-GRAND-TOTALS.
           05  WS-GT-ACCOUNTS          PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-GT-POSITIONS         PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-GT-MV-USD            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GT-COST              PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GT-UNRLZD            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GT-ACCRUED           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-GT-STALE             PIC S9(07)       COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * SUMMARY BY SECURITY TYPE                                       *
      *----------------------------------------------------------------*
       01  WS-TYPE-NAME-VALUES.
           05  FILLER  PIC X(22)  VALUE 'EQCOMMON EQUITY       '.
           05  FILLER  PIC X(22)  VALUE 'PFPREFERRED           '.
           05  FILLER  PIC X(22)  VALUE 'ADADR                 '.
           05  FILLER  PIC X(22)  VALUE 'CBCORPORATE BOND      '.
           05  FILLER  PIC X(22)  VALUE 'MUMUNICIPAL BOND      '.
           05  FILLER  PIC X(22)  VALUE 'GVGOVERNMENT          '.
           05  FILLER  PIC X(22)  VALUE 'MFMUTUAL FUND         '.
           05  FILLER  PIC X(22)  VALUE '??OTHER               '.
       01  WS-TYPE-TABLE REDEFINES WS-TYPE-NAME-VALUES.
           05  WS-TT-ENTRY OCCURS 8 TIMES INDEXED BY TT-IDX.
               10  WS-TT-CODE          PIC X(02).
               10  WS-TT-NAME          PIC X(20).
       01  WS-TYPE-TOTALS.
           05  WS-TY OCCURS 8 TIMES.
               10  WS-TY-COUNT         PIC S9(07)       COMP-3.
               10  WS-TY-MV-USD        PIC S9(15)V99    COMP-3.
               10  WS-TY-UNRLZD        PIC S9(15)V99    COMP-3.
       01  WS-TY-SUB                   PIC S9(04) COMP.
       01  WS-READ-CNT                 PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-QTY-HASH                 PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
       01  WS-ABS-QTY                  PIC S9(11)V9(04) COMP-3.
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
           05  FILLER  PIC X(11)  VALUE ' CUSIP'.
           05  FILLER  PIC X(05)  VALUE 'LOC'.
           05  FILLER  PIC X(03)  VALUE 'ST'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(18)  VALUE '         QUANTITY'.
           05  FILLER  PIC X(16)  VALUE '         PRICE S'.
           05  FILLER  PIC X(19)  VALUE '   MKT VALUE USD'.
           05  FILLER  PIC X(19)  VALUE '       COST BASIS'.
           05  FILLER  PIC X(19)  VALUE '   UNREALIZED P&L'.
           05  FILLER  PIC X(18)  VALUE '      ACCRUED'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(11)  VALUE ' ---------'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(18)  VALUE '-----------------'.
           05  FILLER  PIC X(16)  VALUE '-------------- -'.
           05  FILLER  PIC X(19)  VALUE ' -----------------'.
           05  FILLER  PIC X(19)  VALUE '------------------'.
           05  FILLER  PIC X(19)  VALUE '------------------'.
           05  FILLER  PIC X(18)  VALUE '-------------'.
       01  WS-ACCT-HEAD-LINE.
           05  AH-CC                   PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  FILLER                  PIC X(09)  VALUE 'ACCOUNT: '.
           05  AH-ACCT                 PIC X(10).
           05  FILLER                  PIC X(10)  VALUE '  BRANCH: '.
           05  AH-BRANCH               PIC X(03).
           05  FILLER                  PIC X(07)  VALUE '  REP: '.
           05  AH-REP                  PIC X(04).
           05  FILLER                  PIC X(08)  VALUE '  TYPE: '.
           05  AH-TYPE                 PIC X(02).
           05  FILLER                  PIC X(78)  VALUE SPACES.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  DL-LOC                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-SEC-TYPE             PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-PRICE                PIC ZZZ,ZZ9.999999.
           05  DL-STALE                PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-MV-USD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-COST                 PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-UNRLZD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-ACCRUED              PIC -Z,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(05).
       01  WS-ACCT-TOTAL-LINE.
           05  AT-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  AT-LABEL                PIC X(20).
           05  AT-POSITIONS            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(11)  VALUE ' POSITIONS '.
           05  AT-STALE                PIC ZZZ9.
           05  FILLER                  PIC X(10)  VALUE ' STALE    '.
           05  AT-MV-USD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  AT-COST                 PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  AT-UNRLZD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  AT-ACCRUED              PIC -Z,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(07).
       01  WS-MIXED-CCY-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(24)  VALUE SPACES.
           05  FILLER                  PIC X(108) VALUE
               'NOTE: COST AND P&L INCLUDE NON-USD POSITIONS IN LOCAL CU
      -        'RRENCY'.
       01  WS-TYPE-HEAD.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(132) VALUE
               ' SUMMARY BY SECURITY TYPE'.
       01  WS-TYPE-LINE.
           05  TY-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  TY-CODE                 PIC X(02).
           05  FILLER                  PIC X(02).
           05  TY-NAME                 PIC X(20).
           05  TY-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  TY-MV-USD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  TY-UNRLZD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(56).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO OPEN POSITIONS VALUED FOR THIS DATE ***'.
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
           PERFORM 2000-PROCESS-VALUE THRU 2000-EXIT
               UNTIL END-OF-VALUES.
           IF NOT FIRST-ACCOUNT
               PERFORM 3000-ACCOUNT-BREAK THRU 3000-EXIT
           END-IF.
           PERFORM 4000-GRAND-TOTALS THRU 4000-EXIT.
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
           MOVE 'VALUATION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT VALIN-FILE.
           IF WS-VALIN-STATUS NOT = '00'
               MOVE 'VALIN' TO AB-DDNAME
               MOVE WS-VALIN-STATUS TO AB-FILE-STATUS
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
           PERFORM VARYING WS-TY-SUB FROM 1 BY 1 UNTIL WS-TY-SUB > 8
               MOVE ZERO TO WS-TY-COUNT (WS-TY-SUB)
                            WS-TY-MV-USD (WS-TY-SUB)
                            WS-TY-UNRLZD (WS-TY-SUB)
           END-PERFORM.
           INITIALIZE WS-ACCT-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'DAILY VALUATION BY ACCOUNT' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-VALUE THRU 8000-EXIT.
           IF END-OF-VALUES
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-VALUE.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF VAL-QTY < ZERO
               COMPUTE WS-ABS-QTY = VAL-QTY * -1
           ELSE
               MOVE VAL-QTY TO WS-ABS-QTY
           END-IF.
           ADD WS-ABS-QTY TO WS-QTY-HASH.
           IF FIRST-ACCOUNT
               MOVE 'N' TO WS-FIRST-SW
               MOVE VAL-ACCT-NO TO WS-PREV-ACCT
               PERFORM 2100-ACCOUNT-HEADER THRU 2100-EXIT
           ELSE
               IF VAL-ACCT-NO NOT = WS-PREV-ACCT
                   PERFORM 3000-ACCOUNT-BREAK THRU 3000-EXIT
                   MOVE VAL-ACCT-NO TO WS-PREV-ACCT
                   PERFORM 2100-ACCOUNT-HEADER THRU 2100-EXIT
               END-IF
           END-IF.
           PERFORM 2200-PRINT-DETAIL THRU 2200-EXIT.
           PERFORM 2300-ACCUMULATE THRU 2300-EXIT.
           PERFORM 8000-READ-VALUE THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-ACCOUNT-HEADER.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT + 4 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE VAL-ACCT-NO   TO AH-ACCT.
           MOVE VAL-BRANCH    TO AH-BRANCH.
           MOVE VAL-REP       TO AH-REP.
           MOVE VAL-ACCT-TYPE TO AH-TYPE.
           WRITE RPT-RECORD FROM WS-ACCT-HEAD-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PRINT-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES            TO WS-DETAIL-LINE.
           MOVE ' '               TO DL-CC.
           MOVE VAL-CUSIP         TO DL-CUSIP.
           MOVE VAL-LOCATION      TO DL-LOC.
           MOVE VAL-SEC-TYPE      TO DL-SEC-TYPE.
           MOVE VAL-CCY           TO DL-CCY.
           MOVE VAL-QTY           TO DL-QTY.
           MOVE VAL-PRICE         TO DL-PRICE.
           IF VAL-PRICE-STALE
               MOVE '*' TO DL-STALE
           END-IF.
           MOVE VAL-MKT-VALUE-USD TO DL-MV-USD.
           MOVE VAL-COST-BASIS    TO DL-COST.
           MOVE VAL-UNRLZD-PL     TO DL-UNRLZD.
           MOVE VAL-ACCRUED-INT   TO DL-ACCRUED.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               PERFORM 2100-ACCOUNT-HEADER THRU 2100-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-ACCUMULATE.
      *----------------------------------------------------------------*
           ADD 1                 TO WS-AT-POSITIONS.
           ADD VAL-MKT-VALUE-USD TO WS-AT-MV-USD.
           ADD VAL-COST-BASIS    TO WS-AT-COST.
           ADD VAL-UNRLZD-PL     TO WS-AT-UNRLZD.
           ADD VAL-ACCRUED-INT   TO WS-AT-ACCRUED.
           IF VAL-PRICE-STALE
               ADD 1 TO WS-AT-STALE
           END-IF.
           IF VAL-CCY NOT = 'USD'
               ADD 1 TO WS-AT-NON-USD
           END-IF.
           SET TT-IDX TO 1.
           SEARCH WS-TT-ENTRY
               AT END
                   MOVE 8 TO WS-TY-SUB
               WHEN WS-TT-CODE (TT-IDX) = VAL-SEC-TYPE
                   SET WS-TY-SUB TO TT-IDX
           END-SEARCH.
           ADD 1                 TO WS-TY-COUNT (WS-TY-SUB).
           ADD VAL-MKT-VALUE-USD TO WS-TY-MV-USD (WS-TY-SUB).
           ADD VAL-UNRLZD-PL     TO WS-TY-UNRLZD (WS-TY-SUB).
       2300-EXIT.
           EXIT.
      *================================================================*
       3000-ACCOUNT-BREAK.
      *================================================================*
           MOVE ' '              TO AT-CC.
           MOVE 'ACCOUNT TOTAL'  TO AT-LABEL.
           MOVE WS-AT-POSITIONS  TO AT-POSITIONS.
           MOVE WS-AT-STALE      TO AT-STALE.
           MOVE WS-AT-MV-USD     TO AT-MV-USD.
           MOVE WS-AT-COST       TO AT-COST.
           MOVE WS-AT-UNRLZD     TO AT-UNRLZD.
           MOVE WS-AT-ACCRUED    TO AT-ACCRUED.
           IF RPT-LINE-COUNT + 2 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-ACCT-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
           IF WS-AT-NON-USD > ZERO
               WRITE RPT-RECORD FROM WS-MIXED-CCY-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           ADD 1                TO WS-GT-ACCOUNTS.
           ADD WS-AT-POSITIONS  TO WS-GT-POSITIONS.
           ADD WS-AT-MV-USD     TO WS-GT-MV-USD.
           ADD WS-AT-COST       TO WS-GT-COST.
           ADD WS-AT-UNRLZD     TO WS-GT-UNRLZD.
           ADD WS-AT-ACCRUED    TO WS-GT-ACCRUED.
           ADD WS-AT-STALE      TO WS-GT-STALE.
           INITIALIZE WS-ACCT-TOTALS.
       3000-EXIT.
           EXIT.
      *================================================================*
       4000-GRAND-TOTALS.
      *================================================================*
           IF RPT-LINE-COUNT + 16 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE '0'              TO AT-CC.
           MOVE 'GRAND TOTAL'    TO AT-LABEL.
           MOVE WS-GT-POSITIONS  TO AT-POSITIONS.
           MOVE WS-GT-STALE      TO AT-STALE.
           MOVE WS-GT-MV-USD     TO AT-MV-USD.
           MOVE WS-GT-COST       TO AT-COST.
           MOVE WS-GT-UNRLZD     TO AT-UNRLZD.
           MOVE WS-GT-ACCRUED    TO AT-ACCRUED.
           WRITE RPT-RECORD FROM WS-ACCT-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-TYPE-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           PERFORM VARYING WS-TY-SUB FROM 1 BY 1 UNTIL WS-TY-SUB > 8
               IF WS-TY-COUNT (WS-TY-SUB) > ZERO
                   MOVE SPACES TO WS-TYPE-LINE
                   MOVE ' ' TO TY-CC
                   MOVE WS-TT-CODE (WS-TY-SUB)   TO TY-CODE
                   MOVE WS-TT-NAME (WS-TY-SUB)   TO TY-NAME
                   MOVE WS-TY-COUNT (WS-TY-SUB)  TO TY-COUNT
                   MOVE WS-TY-MV-USD (WS-TY-SUB) TO TY-MV-USD
                   MOVE WS-TY-UNRLZD (WS-TY-SUB) TO TY-UNRLZD
                   WRITE RPT-RECORD FROM WS-TYPE-LINE
                   PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               END-IF
           END-PERFORM.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           IF WS-GT-STALE > ZERO
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       4000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-VALUE.
      *================================================================*
           READ VALIN-FILE.
           EVALUATE TRUE
               WHEN VALIN-OK
                   CONTINUE
               WHEN VALIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'VALIN' TO AB-DDNAME
                   MOVE WS-VALIN-STATUS TO AB-FILE-STATUS
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
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 5 TO RPT-LINE-COUNT.
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
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE VALIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'SRR410'        TO CT-STAGE.
           MOVE 'VALUE-IN'      TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-GT-MV-USD    TO CT-AMOUNT.
           MOVE WS-QTY-HASH     TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR410 VALUATION RECORDS : ' WS-READ-CNT.
           DISPLAY 'SRR410 ACCOUNTS          : ' WS-GT-ACCOUNTS.
           DISPLAY 'SRR410 STALE PRICES      : ' WS-GT-STALE.
           DISPLAY 'SRR410 PAGES             : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'VALUATION REPORT ENDED' TO AU-MESSAGE.
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
           DISPLAY 'SRR410 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
