       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR210.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  NOVEMBER 1989.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR210                                            *
      * DESCRIPTION: POSITION REPORT BY BRANCH / REP / ACCOUNT.        *
      *              SECTION 1 - CLIENT ACCOUNTS, WITH ACCOUNT, REP    *
      *                          AND BRANCH TOTALS.                    *
      *              SECTION 2 - FIRM INVENTORY ACCOUNTS (FOR THE      *
      *                          TRADING DESKS), ACCOUNT TOTALS.       *
      *              THE FILE IS READ TWICE, ONCE PER SECTION.         *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD080 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              VALIN    - MSEC.PROD.SR.VALUE.BRSORT(+1)          *
      *                         (SRVALUE BY BRANCH/REP/ACCOUNT/CUSIP)  *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1989-11-06 RJK  ORIGINAL                                       *
      * 1992-04-13 DWB  FIRM INVENTORY SECTION FOR DESKS      CHG01288 *
      * 1996-05-13 DWB  INPUT FROM VALUATION FILE (SRB400)    CHG02215 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-07-16 KAP  DECIMALIZATION                        CHG08811 *
      * 2009-12-14 SPA  USD MARKET VALUE                      CHG19002 *
      * 2016-10-03 SPA  OMNIBUS ACCOUNTS IN CLIENT SECTION    CHG30112 *
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
       01  VALIN-REC                   PIC X(200).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR210'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-VALIN-STATUS         PIC X(02)  VALUE '00'.
               88  VALIN-OK                       VALUE '00'.
               88  VALIN-EOF                      VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-VALUES                  VALUE 'Y'.
           05  WS-SECTION-SW           PIC X(01)  VALUE 'C'.
               88  CLIENT-SECTION                 VALUE 'C'.
               88  FIRM-SECTION                   VALUE 'F'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-IN-SECTION               VALUE 'Y'.
           05  WS-SELECT-SW            PIC X(01)  VALUE 'N'.
               88  RECORD-SELECTED                VALUE 'Y'.
      *----------------------------------------------------------------*
      * CONTROL BREAK FIELDS                                           *
      *----------------------------------------------------------------*
       01  WS-PREV-KEYS.
           05  WS-PREV-BRANCH          PIC X(03)  VALUE LOW-VALUES.
           05  WS-PREV-REP             PIC X(04)  VALUE LOW-VALUES.
           05  WS-PREV-ACCT            PIC X(10)  VALUE LOW-VALUES.
           05  WS-PREV-ACCT-TYPE       PIC X(02)  VALUE SPACES.
       01  WS-LEVEL-TOTALS.
           05  WS-LVL OCCURS 4 TIMES.
      *        1 ACCOUNT  2 REP  3 BRANCH  4 SECTION
               10  WS-LV-POSITIONS     PIC S9(07)       COMP-3.
               10  WS-LV-ACCOUNTS      PIC S9(07)       COMP-3.
               10  WS-LV-MV-USD        PIC S9(15)V99    COMP-3.
               10  WS-LV-COST          PIC S9(15)V99    COMP-3.
               10  WS-LV-UNRLZD        PIC S9(15)V99    COMP-3.
       01  WS-LV-SUB                   PIC S9(04) COMP.
       01  WS-LV-NAMES                 PIC X(56)  VALUE
           'ACCOUNT TOTAL REP TOTAL     BRANCH TOTAL  SECTION TOTAL '.
       01  WS-LV-NAME-TABLE REDEFINES WS-LV-NAMES.
           05  WS-LV-NAME OCCURS 4 TIMES PIC X(14).
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLIENT-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FIRM-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PASS-CNT             PIC S9(04) COMP   VALUE ZERO.
           05  WS-TOT-MV-USD           PIC S9(15)V99    COMP-3
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
           05  FILLER  PIC X(05)  VALUE ' BR'.
           05  FILLER  PIC X(05)  VALUE 'REP'.
           05  FILLER  PIC X(11)  VALUE 'ACCOUNT'.
           05  FILLER  PIC X(03)  VALUE 'AT'.
           05  FILLER  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER  PIC X(03)  VALUE 'ST'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(18)  VALUE '         QUANTITY'.
           05  FILLER  PIC X(16)  VALUE '          PRICE'.
           05  FILLER  PIC X(19)  VALUE '   MKT VALUE USD'.
           05  FILLER  PIC X(19)  VALUE '       COST BASIS'.
           05  FILLER  PIC X(19)  VALUE '   UNREALIZED P&L'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(05)  VALUE ' ---'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(18)  VALUE '-----------------'.
           05  FILLER  PIC X(16)  VALUE '--------------- '.
           05  FILLER  PIC X(19)  VALUE '------------------'.
           05  FILLER  PIC X(19)  VALUE '------------------'.
           05  FILLER  PIC X(19)  VALUE '------------------'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-BRANCH               PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-REP                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-ACCT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
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
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  TL-NAME                 PIC X(14).
           05  FILLER                  PIC X(01).
           05  TL-KEY                  PIC X(10).
           05  FILLER                  PIC X(02).
           05  TL-ACCOUNTS             PIC ZZ,ZZ9.
           05  FILLER                  PIC X(06)  VALUE ' ACCTS'.
           05  TL-POSITIONS            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(07)  VALUE ' POSNS '.
           05  FILLER                  PIC X(08).
           05  TL-MV-USD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-COST                 PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-UNRLZD               PIC -ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(10).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO POSITIONS IN THIS SECTION ***'.
       COPY SRVALUE.
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
      *    ---- SECTION 1 - CLIENTS ---------------------------------
           SET CLIENT-SECTION TO TRUE.
           MOVE 'POSITIONS BY BRANCH / REP / ACCOUNT - CLIENTS'
                               TO RPT-H2-TITLE.
           PERFORM 2000-RUN-SECTION THRU 2000-EXIT.
      *    ---- SECTION 2 - FIRM INVENTORY --------------------------
           SET FIRM-SECTION TO TRUE.
           MOVE 'POSITIONS - FIRM INVENTORY ACCOUNTS'
                               TO RPT-H2-TITLE.
           PERFORM 2000-RUN-SECTION THRU 2000-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
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
           MOVE 'POSITION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
       1000-EXIT.
           EXIT.
      *================================================================*
      * ONE PASS OF THE FILE FOR THE CURRENT SECTION                   *
      *================================================================*
       2000-RUN-SECTION.
           ADD 1 TO WS-PASS-CNT.
           MOVE 'N' TO WS-EOF-SW.
           MOVE 'Y' TO WS-FIRST-SW.
           MOVE LOW-VALUES TO WS-PREV-BRANCH WS-PREV-REP WS-PREV-ACCT.
           PERFORM VARYING WS-LV-SUB FROM 1 BY 1 UNTIL WS-LV-SUB > 4
               PERFORM 3900-CLEAR-LEVEL THRU 3900-EXIT
           END-PERFORM.
           OPEN INPUT VALIN-FILE.
           IF WS-VALIN-STATUS NOT = '00'
               MOVE 'VALIN' TO AB-DDNAME
               MOVE WS-VALIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           PERFORM 8000-READ-SELECTED THRU 8000-EXIT.
           IF END-OF-VALUES
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
           PERFORM 2100-PROCESS-VALUE THRU 2100-EXIT
               UNTIL END-OF-VALUES.
           IF NOT FIRST-IN-SECTION
               PERFORM 3100-ACCOUNT-BREAK THRU 3100-EXIT
               IF CLIENT-SECTION
                   PERFORM 3200-REP-BREAK THRU 3200-EXIT
                   PERFORM 3300-BRANCH-BREAK THRU 3300-EXIT
               END-IF
               MOVE 4 TO WS-LV-SUB
               MOVE SPACES TO TL-KEY
               PERFORM 3800-PRINT-TOTAL THRU 3800-EXIT
           END-IF.
           CLOSE VALIN-FILE.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESS-VALUE.
      *----------------------------------------------------------------*
           IF FIRST-IN-SECTION
               MOVE 'N' TO WS-FIRST-SW
               PERFORM 2900-SAVE-KEYS THRU 2900-EXIT
           ELSE
               IF CLIENT-SECTION
                   EVALUATE TRUE
                       WHEN VAL-BRANCH NOT = WS-PREV-BRANCH
                           PERFORM 3100-ACCOUNT-BREAK THRU 3100-EXIT
                           PERFORM 3200-REP-BREAK THRU 3200-EXIT
                           PERFORM 3300-BRANCH-BREAK THRU 3300-EXIT
                       WHEN VAL-REP NOT = WS-PREV-REP
                           PERFORM 3100-ACCOUNT-BREAK THRU 3100-EXIT
                           PERFORM 3200-REP-BREAK THRU 3200-EXIT
                       WHEN VAL-ACCT-NO NOT = WS-PREV-ACCT
                           PERFORM 3100-ACCOUNT-BREAK THRU 3100-EXIT
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               ELSE
                   IF VAL-ACCT-NO NOT = WS-PREV-ACCT
                       PERFORM 3100-ACCOUNT-BREAK THRU 3100-EXIT
                   END-IF
               END-IF
               PERFORM 2900-SAVE-KEYS THRU 2900-EXIT
           END-IF.
           PERFORM 2200-PRINT-DETAIL THRU 2200-EXIT.
           ADD 1                 TO WS-LV-POSITIONS (1).
           ADD VAL-MKT-VALUE-USD TO WS-LV-MV-USD (1) WS-TOT-MV-USD.
           ADD VAL-COST-BASIS    TO WS-LV-COST (1).
           ADD VAL-UNRLZD-PL     TO WS-LV-UNRLZD (1).
           PERFORM 8000-READ-SELECTED THRU 8000-EXIT.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PRINT-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES            TO WS-DETAIL-LINE.
           MOVE ' '               TO DL-CC.
           MOVE VAL-BRANCH        TO DL-BRANCH.
           MOVE VAL-REP           TO DL-REP.
           MOVE VAL-ACCT-NO       TO DL-ACCT.
           MOVE VAL-ACCT-TYPE     TO DL-ACCT-TYPE.
           MOVE VAL-CUSIP         TO DL-CUSIP.
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
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2900-SAVE-KEYS.
      *----------------------------------------------------------------*
           MOVE VAL-BRANCH    TO WS-PREV-BRANCH.
           MOVE VAL-REP       TO WS-PREV-REP.
           MOVE VAL-ACCT-NO   TO WS-PREV-ACCT.
           MOVE VAL-ACCT-TYPE TO WS-PREV-ACCT-TYPE.
       2900-EXIT.
           EXIT.
      *================================================================*
      * CONTROL BREAKS - EACH LEVEL ROLLS INTO THE NEXT                *
      *================================================================*
       3100-ACCOUNT-BREAK.
           ADD 1 TO WS-LV-ACCOUNTS (1).
           MOVE 1 TO WS-LV-SUB.
           MOVE WS-PREV-ACCT TO TL-KEY.
           PERFORM 3800-PRINT-TOTAL THRU 3800-EXIT.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-REP-BREAK.
      *----------------------------------------------------------------*
           MOVE 2 TO WS-LV-SUB.
           MOVE SPACES TO TL-KEY.
           MOVE WS-PREV-REP TO TL-KEY.
           PERFORM 3800-PRINT-TOTAL THRU 3800-EXIT.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-BRANCH-BREAK.
      *----------------------------------------------------------------*
           MOVE 3 TO WS-LV-SUB.
           MOVE SPACES TO TL-KEY.
           MOVE WS-PREV-BRANCH TO TL-KEY.
           PERFORM 3800-PRINT-TOTAL THRU 3800-EXIT.
       3300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PRINT THE TOTAL FOR LEVEL WS-LV-SUB, ROLL IT UP, CLEAR IT.     *
      * IN THE FIRM SECTION ACCOUNTS ROLL STRAIGHT TO THE SECTION.     *
      *----------------------------------------------------------------*
       3800-PRINT-TOTAL.
           MOVE ' '                         TO TL-CC.
           MOVE WS-LV-NAME (WS-LV-SUB)      TO TL-NAME.
           MOVE WS-LV-ACCOUNTS (WS-LV-SUB)  TO TL-ACCOUNTS.
           MOVE WS-LV-POSITIONS (WS-LV-SUB) TO TL-POSITIONS.
           MOVE WS-LV-MV-USD (WS-LV-SUB)    TO TL-MV-USD.
           MOVE WS-LV-COST (WS-LV-SUB)      TO TL-COST.
           MOVE WS-LV-UNRLZD (WS-LV-SUB)    TO TL-UNRLZD.
           IF WS-LV-SUB > 1
               MOVE '0' TO TL-CC
           END-IF.
           IF RPT-LINE-COUNT + 2 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
           IF WS-LV-SUB < 4
               IF FIRM-SECTION
                   PERFORM 3850-ROLL-TO-SECTION THRU 3850-EXIT
               ELSE
                   ADD WS-LV-POSITIONS (WS-LV-SUB)
                                     TO WS-LV-POSITIONS (WS-LV-SUB + 1)
                   ADD WS-LV-ACCOUNTS (WS-LV-SUB)
                                     TO WS-LV-ACCOUNTS (WS-LV-SUB + 1)
                   ADD WS-LV-MV-USD (WS-LV-SUB)
                                     TO WS-LV-MV-USD (WS-LV-SUB + 1)
                   ADD WS-LV-COST (WS-LV-SUB)
                                     TO WS-LV-COST (WS-LV-SUB + 1)
                   ADD WS-LV-UNRLZD (WS-LV-SUB)
                                     TO WS-LV-UNRLZD (WS-LV-SUB + 1)
               END-IF
           END-IF.
           PERFORM 3900-CLEAR-LEVEL THRU 3900-EXIT.
       3800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3850-ROLL-TO-SECTION.
      *----------------------------------------------------------------*
           ADD WS-LV-POSITIONS (WS-LV-SUB) TO WS-LV-POSITIONS (4).
           ADD WS-LV-ACCOUNTS (WS-LV-SUB)  TO WS-LV-ACCOUNTS (4).
           ADD WS-LV-MV-USD (WS-LV-SUB)    TO WS-LV-MV-USD (4).
           ADD WS-LV-COST (WS-LV-SUB)      TO WS-LV-COST (4).
           ADD WS-LV-UNRLZD (WS-LV-SUB)    TO WS-LV-UNRLZD (4).
       3850-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3900-CLEAR-LEVEL.
      *----------------------------------------------------------------*
           MOVE ZERO TO WS-LV-POSITIONS (WS-LV-SUB)
                        WS-LV-ACCOUNTS (WS-LV-SUB)
                        WS-LV-MV-USD (WS-LV-SUB)
                        WS-LV-COST (WS-LV-SUB)
                        WS-LV-UNRLZD (WS-LV-SUB).
       3900-EXIT.
           EXIT.
      *================================================================*
      * READ THE NEXT RECORD BELONGING TO THE CURRENT SECTION.         *
      * FIRM ACCTS SORT LOW - FIRST BYTE BELOW '0' IS A FIRM ACCOUNT.  *
      *================================================================*
       8000-READ-SELECTED.
           MOVE 'N' TO WS-SELECT-SW.
           PERFORM UNTIL RECORD-SELECTED OR END-OF-VALUES
               READ VALIN-FILE INTO VAL-VALUATION-REC
               EVALUATE TRUE
                   WHEN VALIN-OK
                       IF WS-PASS-CNT = 1
                           ADD 1 TO WS-READ-CNT
                       END-IF
                       IF VAL-ACCT-NO (1:1) < '0'
                           IF FIRM-SECTION
                               MOVE 'Y' TO WS-SELECT-SW
                               ADD 1 TO WS-FIRM-CNT
                           END-IF
                       ELSE
                           IF CLIENT-SECTION
                               MOVE 'Y' TO WS-SELECT-SW
                               ADD 1 TO WS-CLIENT-CNT
                           END-IF
                       END-IF
                   WHEN VALIN-EOF
                       MOVE 'Y' TO WS-EOF-SW
                   WHEN OTHER
                       MOVE 'VALIN' TO AB-DDNAME
                       MOVE WS-VALIN-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE 'READ FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-PERFORM.
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
           CLOSE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'SRR210'        TO CT-STAGE.
           MOVE 'VALUE-IN'      TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-TOT-MV-USD   TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'FIRM-POSNS'    TO CT-COUNTER-NAME.
           MOVE WS-FIRM-CNT     TO CT-COUNT.
           MOVE ZERO            TO CT-AMOUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR210 VALUATION RECORDS : ' WS-READ-CNT.
           DISPLAY 'SRR210 CLIENT POSITIONS  : ' WS-CLIENT-CNT.
           DISPLAY 'SRR210 FIRM POSITIONS    : ' WS-FIRM-CNT.
           DISPLAY 'SRR210 PAGES             : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'POSITION REPORT ENDED' TO AU-MESSAGE.
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
           DISPLAY 'SRR210 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
