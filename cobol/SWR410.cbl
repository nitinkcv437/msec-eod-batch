       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWR410.
       AUTHOR.        T L MORGAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/22/1999.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWR410                                            *
      * TITLE      : STATEMENT OF PENDING TRANSACTIONS - PRINT         *
      * JOB        : MSSWD030  STEP070                                 *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   ONE STATEMENT PER DVP / RVP ACCOUNT (NEW PAGE), THREE        *
      *   SECTIONS: FAILING, PENDING, SETTLED TODAY.  INPUT IS THE     *
      *   SWB400 EXTRACT SORTED BY ACCOUNT / SECTION / SETTLE DATE /   *
      *   REFERENCE (SWS400A).  CONTINUATION HEADING ON OVERFLOW.      *
      *   ACCOUNT TOTALS BY SECTION, RUN TOTALS AT THE END.            *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          PSTMIN    MSEC.PROD.SW.PENDSTMT.SORTED(+1) (SWPSTM)   *
      * OUTPUT : RPTFILE   STATEMENTS SWR410 (FB 133 ASA)              *
      * CALLS  : CMU050 CMU060 CMU080 CMASM02                          *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1999-03-22 TLM  CHG04890  ORIGINAL - MT537 REPLACEMENT         *
      * 1999-06-21 TLM  CHG05230  FAILING SECTION                      *
      * 2004-01-12 KAP  CHG11650  BUY-IN NOTICE LINE                   *
      * 2024-05-20 NVR  CHG40551  T+1 WORDING OF THE NOTICE            *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT PSTMIN-FILE    ASSIGN TO PSTMIN
                  FILE STATUS IS WS-PSTMIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PSTMIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SWPSTM.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWR410'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PSTMIN-STATUS        PIC X(02)  VALUE '00'.
               88  PSTMIN-OK                      VALUE '00'.
               88  PSTMIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-EOF-SW                   PIC X(01)  VALUE 'N'.
           88  END-OF-STMT                        VALUE 'Y'.
       01  WS-CONTROL.
           05  WS-PREV-ACCT            PIC X(10)  VALUE LOW-VALUES.
           05  WS-PREV-SECTION         PIC X(01)  VALUE LOW-VALUES.
           05  WS-ACCT-PAGE            PIC S9(04) COMP-3 VALUE ZERO.
       01  WS-SECTION-TITLES.
           05  FILLER  PIC X(41)  VALUE
               'FFAILING - CONTRACTUAL SETTLE DATE PASSED'.
           05  FILLER  PIC X(41)  VALUE
               'PPENDING SETTLEMENT                      '.
           05  FILLER  PIC X(41)  VALUE
               'SSETTLED TODAY                           '.
       01  WS-SECTION-TABLE REDEFINES WS-SECTION-TITLES.
           05  WS-ST-ENTRY OCCURS 3 TIMES INDEXED BY ST-IDX.
               10  WS-ST-CODE          PIC X(01).
               10  WS-ST-TITLE         PIC X(40).
       01  WS-ACCT-TOTALS.
           05  WS-AC-SEC-CNT           PIC S9(07) COMP-3 OCCURS 3 TIMES.
       01  WS-RUN-TOTALS.
           05  WS-RUN-SEC-CNT          PIC S9(07) COMP-3 OCCURS 3 TIMES.
           05  WS-RUN-ACCTS            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-RUN-BUYIN            PIC S9(07) COMP-3 VALUE ZERO.
       01  WS-SEC-SUB                  PIC S9(04) COMP.
       01  WS-AGE-EDIT                 PIC ZZ9.
       01  WS-CLOSEOUT-TEXT            PIC X(22).
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-ACCT-LINE.
           05  AL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  FILLER                  PIC X(09)  VALUE 'ACCOUNT: '.
           05  AL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(05).
           05  AL-CONT                 PIC X(20).
           05  FILLER                  PIC X(87).
       01  WS-SECTION-LINE.
           05  SC-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  SC-TITLE                PIC X(40).
           05  FILLER                  PIC X(89).
       01  WS-COL-HEAD.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(05)  VALUE SPACES.
           05  FILLER  PIC X(17)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(05)  VALUE 'TYPE'.
           05  FILLER  PIC X(11)  VALUE 'TRADE'.
           05  FILLER  PIC X(11)  VALUE 'SETTLE'.
           05  FILLER  PIC X(13)  VALUE 'ISIN'.
           05  FILLER  PIC X(31)  VALUE 'DESCRIPTION'.
           05  FILLER  PIC X(17)  VALUE '   OPEN QUANTITY'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(15)  VALUE '       AMOUNT'.
           05  FILLER  PIC X(03)  VALUE 'ST'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-TYPE                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-TRADE                PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-SETTLE               PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-ISIN                 PIC X(12).
           05  FILLER                  PIC X(01).
           05  DL-DESC                 PIC X(30).
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-AMT                  PIC ZZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(02).
       01  WS-NOTE-LINE.
           05  NL-CC                   PIC X(01).
           05  FILLER                  PIC X(22).
           05  NL-TEXT                 PIC X(110).
       01  WS-ACCT-TOTAL-LINE.
           05  AT-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  FILLER                  PIC X(20)  VALUE
               'ACCOUNT SUMMARY:'.
           05  FILLER                  PIC X(09)  VALUE 'FAILING '.
           05  AT-F                    PIC ZZ,ZZ9.
           05  FILLER                  PIC X(11)  VALUE '   PENDING '.
           05  AT-P                    PIC ZZ,ZZ9.
           05  FILLER                  PIC X(17)  VALUE
               '   SETTLED TODAY '.
           05  AT-S                    PIC ZZ,ZZ9.
           05  FILLER                  PIC X(52).
       01  WS-DISCLAIMER-1.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(05)  VALUE SPACES.
           05  FILLER                  PIC X(50)  VALUE
               'THIS STATEMENT LISTS INSTRUCTIONS SENT TO OUR CUST'.
           05  FILLER                  PIC X(50)  VALUE
               'ODIAN ON YOUR BEHALF THAT HAVE NOT SETTLED, AND TH'.
           05  FILLER                  PIC X(27)  VALUE
               'OSE SETTLED TODAY.'.
       01  WS-DISCLAIMER-2.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(05)  VALUE SPACES.
           05  FILLER                  PIC X(50)  VALUE
               'PLEASE CONTACT SETTLEMENT OPERATIONS IF YOUR RECOR'.
           05  FILLER                  PIC X(50)  VALUE
               'DS DIFFER. FAILS ARE SUBJECT TO CLOSE-OUT UNDER SE'.
           05  FILLER                  PIC X(27)  VALUE
               'C RULE 204.'.
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  TL-LABEL                PIC X(36).
           05  TL-VALUE                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(80).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO PENDING TRANSACTIONS FOR THIS BUSINESS DATE ***'.
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
           PERFORM 2000-PROCESS-LINE THRU 2000-EXIT
               UNTIL END-OF-STMT.
           IF WS-READ-CNT > ZERO
               PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
           END-IF.
           PERFORM 3000-RUN-TOTALS THRU 3000-EXIT.
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
           MOVE 'PENDING STATEMENT PRINT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT PSTMIN-FILE.
           IF NOT PSTMIN-OK
               MOVE 'PSTMIN' TO AB-DDNAME
               MOVE WS-PSTMIN-STATUS TO AB-FILE-STATUS
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
           PERFORM VARYING WS-SEC-SUB FROM 1 BY 1 UNTIL WS-SEC-SUB > 3
               MOVE ZERO TO WS-RUN-SEC-CNT (WS-SEC-SUB)
           END-PERFORM.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE 'SWR410'       TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'STATEMENT OF PENDING TRANSACTIONS' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-STMT THRU 8000-EXIT.
           IF END-OF-STMT
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-LINE.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF PST-ACCT-NO NOT = WS-PREV-ACCT
               IF WS-READ-CNT > 1
                   PERFORM 2900-END-ACCOUNT THRU 2900-EXIT
               END-IF
               PERFORM 2100-START-ACCOUNT THRU 2100-EXIT
           END-IF.
           IF PST-SECTION NOT = WS-PREV-SECTION
               PERFORM 2200-START-SECTION THRU 2200-EXIT
           END-IF.
           PERFORM 2300-PRINT-DETAIL THRU 2300-EXIT.
           PERFORM 8000-READ-STMT THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-START-ACCOUNT.
      *----------------------------------------------------------------*
           MOVE PST-ACCT-NO TO WS-PREV-ACCT.
           MOVE LOW-VALUES TO WS-PREV-SECTION.
           MOVE ZERO TO WS-ACCT-PAGE.
           ADD 1 TO WS-RUN-ACCTS.
           PERFORM VARYING WS-SEC-SUB FROM 1 BY 1 UNTIL WS-SEC-SUB > 3
               MOVE ZERO TO WS-AC-SEC-CNT (WS-SEC-SUB)
           END-PERFORM.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-START-SECTION.
      *----------------------------------------------------------------*
           MOVE PST-SECTION TO WS-PREV-SECTION.
           IF RPT-LINE-COUNT + 4 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE SPACES TO WS-SECTION-LINE.
           MOVE '0' TO SC-CC.
           MOVE 'UNKNOWN SECTION' TO SC-TITLE.
           PERFORM VARYING ST-IDX FROM 1 BY 1 UNTIL ST-IDX > 3
               IF WS-ST-CODE (ST-IDX) = PST-SECTION
                   MOVE WS-ST-TITLE (ST-IDX) TO SC-TITLE
               END-IF
           END-PERFORM.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-PRINT-DETAIL.
      *----------------------------------------------------------------*
           EVALUATE PST-SECTION
               WHEN 'F'  MOVE 1 TO WS-SEC-SUB
               WHEN 'P'  MOVE 2 TO WS-SEC-SUB
               WHEN OTHER MOVE 3 TO WS-SEC-SUB
           END-EVALUATE.
           ADD 1 TO WS-AC-SEC-CNT (WS-SEC-SUB)
                    WS-RUN-SEC-CNT (WS-SEC-SUB).
           MOVE SPACES           TO WS-DETAIL-LINE.
           MOVE ' '              TO DL-CC.
           MOVE PST-SENDER-REF   TO DL-REF.
           IF PST-MSG-TYPE = '541'
               MOVE 'RVP'        TO DL-TYPE
           ELSE
               MOVE 'DVP'        TO DL-TYPE
           END-IF.
           MOVE PST-TRADE-DATE   TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT     TO DL-TRADE.
           MOVE PST-SETTLE-DATE  TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT     TO DL-SETTLE.
           MOVE PST-ISIN         TO DL-ISIN.
           MOVE PST-SEC-DESC     TO DL-DESC.
           MOVE PST-OPEN-QTY     TO DL-QTY.
           MOVE PST-CCY          TO DL-CCY.
           MOVE PST-AMOUNT       TO DL-AMT.
           MOVE PST-STATUS       TO DL-STATUS.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           ADD 1 TO WS-PRINT-CNT.
      *    NOTE LINE FOR FAILS: AGE, REASON, CLOSE-OUT, BUY-IN
           IF PST-FAILING
               MOVE SPACES TO WS-NOTE-LINE
               MOVE ' ' TO NL-CC
               MOVE SPACES TO WS-CLOSEOUT-TEXT
               IF PST-CLOSEOUT-DATE NOT = ZERO
                   MOVE PST-CLOSEOUT-DATE TO WS-DATE-IN
                   PERFORM 8300-EDIT-DATE THRU 8300-EXIT
                   STRING '  CLOSE-OUT ' WS-DATE-EDIT
                          DELIMITED BY SIZE INTO WS-CLOSEOUT-TEXT
               END-IF
               MOVE PST-FAIL-AGE TO WS-AGE-EDIT
               STRING 'FAILING ' WS-AGE-EDIT ' BUSINESS DAY(S)'
                      '  REASON ' PST-REASON-CODE
                      WS-CLOSEOUT-TEXT
                      DELIMITED BY SIZE INTO NL-TEXT
               IF PST-BUYIN-FLAG = 'Y'
                   MOVE '  *** BUY-IN NOTICE ISSUED ***'
                     TO NL-TEXT (70:30)
                   ADD 1 TO WS-RUN-BUYIN
               END-IF
               MOVE WS-NOTE-LINE TO WS-DETAIL-LINE
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
           END-IF.
           IF PST-SETTLED-TODAY
               MOVE SPACES TO WS-NOTE-LINE
               MOVE ' ' TO NL-CC
               MOVE PST-EFF-SETTLE-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE THRU 8300-EXIT
               STRING 'SETTLED ' WS-DATE-EDIT
                      DELIMITED BY SIZE INTO NL-TEXT
               MOVE WS-NOTE-LINE TO WS-DETAIL-LINE
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
           END-IF.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2900-END-ACCOUNT.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-ACCT-TOTAL-LINE.
           MOVE '0' TO AT-CC.
           MOVE 'ACCOUNT SUMMARY:' TO WS-ACCT-TOTAL-LINE (7:20).
           MOVE 'FAILING ' TO WS-ACCT-TOTAL-LINE (27:9).
           MOVE '   PENDING ' TO WS-ACCT-TOTAL-LINE (42:11).
           MOVE '   SETTLED TODAY ' TO WS-ACCT-TOTAL-LINE (59:17).
           MOVE WS-AC-SEC-CNT (1) TO AT-F.
           MOVE WS-AC-SEC-CNT (2) TO AT-P.
           MOVE WS-AC-SEC-CNT (3) TO AT-S.
           WRITE RPT-RECORD FROM WS-ACCT-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-DISCLAIMER-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-DISCLAIMER-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       2900-EXIT.
           EXIT.
      *================================================================*
       3000-RUN-TOTALS.
      *================================================================*
           MOVE SPACES TO WS-PREV-ACCT.
           MOVE 'RUN TOTALS' TO RPT-H2-TITLE.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE '0' TO TL-CC.
           MOVE 'ACCOUNTS WITH A STATEMENT' TO TL-LABEL.
           MOVE WS-RUN-ACCTS TO TL-VALUE.
           PERFORM 3100-WRITE-TOTAL THRU 3100-EXIT.
           MOVE 'FAILING INSTRUCTIONS' TO TL-LABEL.
           MOVE WS-RUN-SEC-CNT (1) TO TL-VALUE.
           PERFORM 3100-WRITE-TOTAL THRU 3100-EXIT.
           MOVE 'PENDING INSTRUCTIONS' TO TL-LABEL.
           MOVE WS-RUN-SEC-CNT (2) TO TL-VALUE.
           PERFORM 3100-WRITE-TOTAL THRU 3100-EXIT.
           MOVE 'SETTLED TODAY' TO TL-LABEL.
           MOVE WS-RUN-SEC-CNT (3) TO TL-VALUE.
           PERFORM 3100-WRITE-TOTAL THRU 3100-EXIT.
           MOVE 'BUY-IN NOTICES' TO TL-LABEL.
           MOVE WS-RUN-BUYIN TO TL-VALUE.
           PERFORM 3100-WRITE-TOTAL THRU 3100-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       3000-EXIT.
           EXIT.
      *
       3100-WRITE-TOTAL.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
       3100-EXIT.
           EXIT.
      *================================================================*
       8000-READ-STMT.
      *================================================================*
           READ PSTMIN-FILE.
           EVALUATE TRUE
               WHEN PSTMIN-OK
                   CONTINUE
               WHEN PSTMIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'PSTMIN' TO AB-DDNAME
                   MOVE WS-PSTMIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-WRITE-LINE.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               WRITE RPT-RECORD FROM WS-COL-HEAD
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           ADD 1 TO WS-ACCT-PAGE.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 3 TO RPT-LINE-COUNT.
           IF WS-PREV-ACCT NOT = SPACES AND NOT = LOW-VALUES
               MOVE SPACES TO WS-ACCT-LINE
               MOVE '0' TO AL-CC
               MOVE 'ACCOUNT: ' TO WS-ACCT-LINE (3:9)
               MOVE WS-PREV-ACCT TO AL-ACCT
               IF WS-ACCT-PAGE > 1
                   MOVE '(CONTINUED)' TO AL-CONT
               END-IF
               WRITE RPT-RECORD FROM WS-ACCT-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
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
           CLOSE PSTMIN-FILE RPTFILE.
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SWR410'       TO CT-STAGE.
           MOVE 'STMT-IN'      TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT    TO CT-COUNT.
           MOVE ZERO           TO CT-AMOUNT CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'STMTS-PRINTED' TO CT-COUNTER-NAME.
           MOVE WS-RUN-ACCTS   TO CT-COUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SWR410 STATEMENT LINES READ : ' WS-READ-CNT.
           DISPLAY 'SWR410 ACCOUNTS PRINTED     : ' WS-RUN-ACCTS.
           DISPLAY 'SWR410 PAGES                : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE 'PENDING STATEMENT PRINT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SWR410 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
