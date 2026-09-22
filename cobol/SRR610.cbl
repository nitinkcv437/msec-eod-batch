       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRR610.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  AUGUST 1991.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRR610                                            *
      * DESCRIPTION: GENERAL LEDGER JOURNAL SUMMARY AND PROOF.         *
      *              SECTION 1 - DEBITS AND CREDITS BY GL ACCOUNT AND  *
      *                          TRANSACTION CODE, GL ACCOUNT TOTALS,  *
      *                          GRAND TOTAL AND DR = CR PROOF.        *
      *              SECTION 2 - EXCEPTIONS FROM SRB600 (REFERENCES    *
      *                          OUT OF BALANCE, UNMAPPED CODES).      *
      *              SECTION 3 - CONTROL PROOF: SRB600 RUN TOTALS      *
      *                          AGAINST THE JOURNAL AS READ HERE.     *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD070 / STEP040                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              GLJIN    - MSEC.PROD.SR.GLJRNL.SORTED(+1)         *
      *                         (SRGLJNL BY GL ACCOUNT / TXN CODE)     *
      *              GLSUMM   - MSEC.PROD.SR.GLSUMM(+1)    (SRGLSUM)   *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0 IN BALANCE, 4 EXCEPTIONS, 8 OUT OF BALANCE OR   *
      *              CONTROL TOTALS DO NOT AGREE                       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1991-08-05 DWB  ORIGINAL                              CHG00987 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-07-16 KAP  COMMISSION / FEE CODES                CHG08811 *
      * 2012-11-19 SPA  ACCRUAL LINES (CACR)                  CHG23904 *
      * 2019-02-25 MHC  EXCEPTION AND CONTROL SECTIONS        CHG33410 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT GLJIN-FILE     ASSIGN TO GLJIN
                  FILE STATUS IS WS-GLJIN-STATUS.
           SELECT GLSUMM-FILE    ASSIGN TO GLSUMM
                  FILE STATUS IS WS-GLSUMM-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  GLJIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRGLJNL.
       FD  GLSUMM-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRGLSUM.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRR610'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-GLJIN-STATUS         PIC X(02)  VALUE '00'.
               88  GLJIN-OK                       VALUE '00'.
               88  GLJIN-EOF                      VALUE '10'.
           05  WS-GLSUMM-STATUS        PIC X(02)  VALUE '00'.
               88  GLSUMM-OK                      VALUE '00'.
               88  GLSUMM-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-JOURNAL                 VALUE 'Y'.
           05  WS-SUM-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-SUMMARY                 VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-LINE                     VALUE 'Y'.
           05  WS-TOTALS-SEEN-SW       PIC X(01)  VALUE 'N'.
               88  RUN-TOTALS-SEEN                VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-SECTION-TITLE            PIC X(60).
      *----------------------------------------------------------------*
      * CONTROL BREAK KEYS AND ACCUMULATORS                            *
      *----------------------------------------------------------------*
       01  WS-PREV-GL                  PIC X(10)  VALUE LOW-VALUES.
       01  WS-PREV-TXN                 PIC X(04)  VALUE LOW-VALUES.
       01  WS-TXN-DESC                 PIC X(20)  VALUE SPACES.
       01  WS-TXN-TOTALS.
           05  WS-TX-COUNT             PIC S9(07)       COMP-3.
           05  WS-TX-DR                PIC S9(15)V99    COMP-3.
           05  WS-TX-CR                PIC S9(15)V99    COMP-3.
       01  WS-GL-TOTALS.
           05  WS-GT-COUNT             PIC S9(07)       COMP-3.
           05  WS-GT-DR                PIC S9(15)V99    COMP-3.
           05  WS-GT-CR                PIC S9(15)V99    COMP-3.
       01  WS-RUN-TOTALS.
           05  WS-RN-COUNT             PIC S9(09)       COMP-3
                                                     VALUE ZERO.
           05  WS-RN-DR                PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-RN-CR                PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-RN-DR-USD            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-RN-CR-USD            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-RN-GL-ACCTS          PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-RN-EXCEPTIONS        PIC S9(07)       COMP-3
                                                     VALUE ZERO.
       01  WS-NET                      PIC S9(15)V99    COMP-3.
      *----------------------------------------------------------------*
      * SAVED SRB600 RUN TOTALS                                        *
      *----------------------------------------------------------------*
       01  WS-SRB600-TOTALS.
           05  WS-S6-JRNL-IN           PIC S9(09)       COMP-3.
           05  WS-S6-ACCR-IN           PIC S9(09)       COMP-3.
           05  WS-S6-LINES-OUT         PIC S9(09)       COMP-3.
           05  WS-S6-REFS-OUT          PIC S9(09)       COMP-3.
           05  WS-S6-REFS-UNBAL        PIC S9(09)       COMP-3.
           05  WS-S6-UNMAPPED          PIC S9(09)       COMP-3.
           05  WS-S6-TOTAL-DR          PIC S9(15)V99    COMP-3.
           05  WS-S6-TOTAL-CR          PIC S9(15)V99    COMP-3.
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
           05  FILLER  PIC X(13)  VALUE ' GL ACCOUNT'.
           05  FILLER  PIC X(06)  VALUE 'TXN'.
           05  FILLER  PIC X(20)  VALUE 'DESCRIPTION'.
           05  FILLER  PIC X(09)  VALUE '  LINES'.
           05  FILLER  PIC X(21)  VALUE '             DEBITS'.
           05  FILLER  PIC X(21)  VALUE '            CREDITS'.
           05  FILLER  PIC X(21)  VALUE '                NET'.
           05  FILLER  PIC X(21)  VALUE SPACES.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(13)  VALUE ' ----------'.
           05  FILLER  PIC X(06)  VALUE '----'.
           05  FILLER  PIC X(20)  VALUE '-------------------'.
           05  FILLER  PIC X(09)  VALUE '-------'.
           05  FILLER  PIC X(21)  VALUE '-------------------'.
           05  FILLER  PIC X(21)  VALUE '-------------------'.
           05  FILLER  PIC X(21)  VALUE '-------------------'.
           05  FILLER  PIC X(21)  VALUE SPACES.
       01  WS-TXN-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  TL-GL                   PIC X(10).
           05  FILLER                  PIC X(02).
           05  TL-TXN                  PIC X(04).
           05  FILLER                  PIC X(02).
           05  TL-DESC                 PIC X(20).
           05  TL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  TL-DR                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  TL-CR                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  TL-NET                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(23).
       01  WS-PROOF-LINE.
           05  PL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  PL-LABEL                PIC X(40).
           05  PL-VALUE-1              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  PL-VALUE-2              PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(03).
           05  PL-RESULT               PIC X(30).
           05  FILLER                  PIC X(13).
       01  WS-EXC-HEAD.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(07)  VALUE '  TYP'.
           05  FILLER  PIC X(04)  VALUE 'SR'.
           05  FILLER  PIC X(17)  VALUE 'REFERENCE'.
           05  FILLER  PIC X(05)  VALUE 'TXN'.
           05  FILLER  PIC X(03)  VALUE 'AT'.
           05  FILLER  PIC X(04)  VALUE 'ST'.
           05  FILLER  PIC X(21)  VALUE '             DEBITS'.
           05  FILLER  PIC X(21)  VALUE '            CREDITS'.
           05  FILLER  PIC X(50)  VALUE 'REASON'.
       01  WS-EXC-LINE.
           05  XL-CC                   PIC X(01).
           05  FILLER                  PIC X(02).
           05  XL-TYPE                 PIC X(03).
           05  FILLER                  PIC X(01).
           05  XL-SOURCE               PIC X(02).
           05  FILLER                  PIC X(02).
           05  XL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  XL-TXN                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  XL-ACCT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01).
           05  XL-SEC-TYPE             PIC X(02).
           05  FILLER                  PIC X(02).
           05  XL-DR                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  XL-CR                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  XL-TEXT                 PIC X(30).
           05  FILLER                  PIC X(21).
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
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-LINE THRU 2000-EXIT
               UNTIL END-OF-JOURNAL.
           IF NOT FIRST-LINE
               PERFORM 3100-TXN-BREAK THRU 3100-EXIT
               PERFORM 3200-GL-BREAK THRU 3200-EXIT
           END-IF.
           PERFORM 4000-GRAND-TOTAL THRU 4000-EXIT.
           PERFORM 5000-EXCEPTIONS THRU 5000-EXIT.
           PERFORM 6000-CONTROL-PROOF THRU 6000-EXIT.
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
           MOVE 'GL SUMMARY REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT GLJIN-FILE.
           IF WS-GLJIN-STATUS NOT = '00'
               MOVE 'GLJIN' TO AB-DDNAME
               MOVE WS-GLJIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT GLSUMM-FILE.
           IF WS-GLSUMM-STATUS NOT = '00'
               MOVE 'GLSUMM' TO AB-DDNAME
               MOVE WS-GLSUMM-STATUS TO AB-FILE-STATUS
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
           INITIALIZE WS-TXN-TOTALS WS-GL-TOTALS WS-SRB600-TOTALS.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'GENERAL LEDGER JOURNAL SUMMARY AND PROOF'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           PERFORM 8000-READ-GL-LINE THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * SECTION 1 - ONE GL LINE                                        *
      *================================================================*
       2000-PROCESS-LINE.
           IF FIRST-LINE
               MOVE 'N' TO WS-FIRST-SW
               MOVE GLJ-GL-ACCOUNT TO WS-PREV-GL
               MOVE GLJ-TXN-CODE   TO WS-PREV-TXN
               MOVE GLJ-DESC       TO WS-TXN-DESC
           ELSE
               IF GLJ-GL-ACCOUNT NOT = WS-PREV-GL
                   PERFORM 3100-TXN-BREAK THRU 3100-EXIT
                   PERFORM 3200-GL-BREAK THRU 3200-EXIT
                   MOVE GLJ-GL-ACCOUNT TO WS-PREV-GL
                   MOVE GLJ-TXN-CODE   TO WS-PREV-TXN
                   MOVE GLJ-DESC       TO WS-TXN-DESC
               ELSE
                   IF GLJ-TXN-CODE NOT = WS-PREV-TXN
                       PERFORM 3100-TXN-BREAK THRU 3100-EXIT
                       MOVE GLJ-TXN-CODE TO WS-PREV-TXN
                       MOVE GLJ-DESC     TO WS-TXN-DESC
                   END-IF
               END-IF
           END-IF.
           ADD 1 TO WS-TX-COUNT WS-RN-COUNT.
           IF GLJ-DEBIT
               ADD GLJ-AMOUNT     TO WS-TX-DR WS-RN-DR
               ADD GLJ-AMOUNT-USD TO WS-RN-DR-USD
           ELSE
               ADD GLJ-AMOUNT     TO WS-TX-CR WS-RN-CR
               ADD GLJ-AMOUNT-USD TO WS-RN-CR-USD
           END-IF.
           PERFORM 8000-READ-GL-LINE THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-TXN-BREAK.
      *----------------------------------------------------------------*
           MOVE SPACES       TO WS-TXN-LINE.
           MOVE ' '          TO TL-CC.
           IF WS-GT-COUNT = ZERO
               MOVE WS-PREV-GL TO TL-GL
           END-IF.
           MOVE WS-PREV-TXN  TO TL-TXN.
           MOVE WS-TXN-DESC  TO TL-DESC.
           MOVE WS-TX-COUNT  TO TL-COUNT.
           MOVE WS-TX-DR     TO TL-DR.
           MOVE WS-TX-CR     TO TL-CR.
           COMPUTE WS-NET = WS-TX-DR - WS-TX-CR.
           MOVE WS-NET       TO TL-NET.
           PERFORM 8100-PRINT-LINE THRU 8100-EXIT.
           ADD WS-TX-COUNT TO WS-GT-COUNT.
           ADD WS-TX-DR    TO WS-GT-DR.
           ADD WS-TX-CR    TO WS-GT-CR.
           INITIALIZE WS-TXN-TOTALS.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-GL-BREAK.
      *----------------------------------------------------------------*
           ADD 1 TO WS-RN-GL-ACCTS.
           MOVE SPACES       TO WS-TXN-LINE.
           MOVE ' '          TO TL-CC.
           MOVE '  ACCOUNT TOTAL' TO TL-DESC.
           MOVE WS-GT-COUNT  TO TL-COUNT.
           MOVE WS-GT-DR     TO TL-DR.
           MOVE WS-GT-CR     TO TL-CR.
           COMPUTE WS-NET = WS-GT-DR - WS-GT-CR.
           MOVE WS-NET       TO TL-NET.
           PERFORM 8100-PRINT-LINE THRU 8100-EXIT.
           WRITE RPT-RECORD FROM RPT-BLANK-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
           INITIALIZE WS-GL-TOTALS.
       3200-EXIT.
           EXIT.
      *================================================================*
      * GRAND TOTAL AND DR = CR PROOF                                  *
      *================================================================*
       4000-GRAND-TOTAL.
           IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE SPACES       TO WS-TXN-LINE.
           MOVE '0'          TO TL-CC.
           MOVE 'GRAND TOTAL' TO TL-DESC.
           MOVE WS-RN-COUNT  TO TL-COUNT.
           MOVE WS-RN-DR     TO TL-DR.
           MOVE WS-RN-CR     TO TL-CR.
           COMPUTE WS-NET = WS-RN-DR - WS-RN-CR.
           MOVE WS-NET       TO TL-NET.
           WRITE RPT-RECORD FROM WS-TXN-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES       TO WS-PROOF-LINE.
           MOVE '0'          TO PL-CC.
           MOVE 'PROOF - TOTAL DEBITS / TOTAL CREDITS' TO PL-LABEL.
           MOVE WS-RN-DR     TO PL-VALUE-1.
           MOVE WS-RN-CR     TO PL-VALUE-2.
           IF WS-RN-DR = WS-RN-CR
               MOVE 'IN BALANCE' TO PL-RESULT
           ELSE
               MOVE '*** OUT OF BALANCE ***' TO PL-RESULT
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES       TO WS-PROOF-LINE.
           MOVE ' '          TO PL-CC.
           MOVE 'MEMO - USD EQUIVALENT DEBITS / CREDITS' TO PL-LABEL.
           MOVE WS-RN-DR-USD TO PL-VALUE-1.
           MOVE WS-RN-CR-USD TO PL-VALUE-2.
           IF WS-RN-DR-USD NOT = WS-RN-CR-USD
               MOVE 'USD ROUNDING DIFFERENCE' TO PL-RESULT
           END-IF.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 4 TO RPT-LINE-COUNT.
       4000-EXIT.
           EXIT.
      *================================================================*
      * SECTION 2 - EXCEPTIONS FROM SRB600                             *
      *================================================================*
       5000-EXCEPTIONS.
           MOVE 'GL JOURNAL EXCEPTIONS' TO RPT-H2-TITLE.
           PERFORM 8250-SECTION-HEADINGS THRU 8250-EXIT.
           WRITE RPT-RECORD FROM WS-EXC-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
           PERFORM 8400-READ-SUMMARY THRU 8400-EXIT.
           PERFORM UNTIL END-OF-SUMMARY
               EVALUATE TRUE
                   WHEN GLS-RUN-TOTALS
                       MOVE 'Y' TO WS-TOTALS-SEEN-SW
                       MOVE GLS-JRNL-IN      TO WS-S6-JRNL-IN
                       MOVE GLS-ACCR-IN      TO WS-S6-ACCR-IN
                       MOVE GLS-LINES-OUT    TO WS-S6-LINES-OUT
                       MOVE GLS-REFS-OUT     TO WS-S6-REFS-OUT
                       MOVE GLS-REFS-UNBAL   TO WS-S6-REFS-UNBAL
                       MOVE GLS-UNMAPPED-CNT TO WS-S6-UNMAPPED
                       MOVE GLS-TOTAL-DR     TO WS-S6-TOTAL-DR
                       MOVE GLS-TOTAL-CR     TO WS-S6-TOTAL-CR
                   WHEN GLS-OUT-OF-BALANCE
                   WHEN GLS-UNMAPPED
                       PERFORM 5100-PRINT-EXCEPTION THRU 5100-EXIT
                   WHEN OTHER
                       DISPLAY 'SRR610 UNKNOWN SUMMARY RECORD '
                               GLS-REC-TYPE
               END-EVALUATE
               PERFORM 8400-READ-SUMMARY THRU 8400-EXIT
           END-PERFORM.
           IF WS-RN-EXCEPTIONS = ZERO
               MOVE SPACES TO WS-MESSAGE-LINE
               MOVE '0'    TO ML-CC
               MOVE '*** NO EXCEPTIONS ***' TO ML-TEXT
               WRITE RPT-RECORD FROM WS-MESSAGE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5100-PRINT-EXCEPTION.
      *----------------------------------------------------------------*
           ADD 1 TO WS-RN-EXCEPTIONS.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           MOVE SPACES           TO WS-EXC-LINE.
           MOVE ' '              TO XL-CC.
           MOVE GLS-SOURCE       TO XL-SOURCE.
           MOVE GLS-REF          TO XL-REF.
           MOVE GLS-TXN-CODE     TO XL-TXN.
           MOVE GLS-ACCT-TYPE    TO XL-ACCT-TYPE.
           MOVE GLS-SEC-TYPE     TO XL-SEC-TYPE.
           MOVE GLS-DR-AMOUNT    TO XL-DR.
           MOVE GLS-CR-AMOUNT    TO XL-CR.
           IF GLS-OUT-OF-BALANCE
               MOVE 'OOB'                 TO XL-TYPE
               MOVE 'REFERENCE OUT OF BALANCE' TO XL-TEXT
           ELSE
               MOVE 'UNM'                 TO XL-TYPE
               MOVE 'NO GL MAP - SUSPENSE USED' TO XL-TEXT
           END-IF.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8250-SECTION-HEADINGS THRU 8250-EXIT
               WRITE RPT-RECORD FROM WS-EXC-HEAD
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
           WRITE RPT-RECORD FROM WS-EXC-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       5100-EXIT.
           EXIT.
      *================================================================*
      * SECTION 3 - SRB600 CONTROL TOTALS AGAINST WHAT WAS READ        *
      *================================================================*
       6000-CONTROL-PROOF.
           IF RPT-LINE-COUNT + 10 > RPT-LINES-PER-PAGE
               PERFORM 8250-SECTION-HEADINGS THRU 8250-EXIT
           END-IF.
           MOVE SPACES TO WS-MESSAGE-LINE.
           MOVE '-'    TO ML-CC.
           MOVE 'CONTROL PROOF - SRB600 RUN TOTALS VS JOURNAL AS READ'
                       TO ML-TEXT.
           WRITE RPT-RECORD FROM WS-MESSAGE-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           IF NOT RUN-TOTALS-SEEN
               MOVE SPACES TO WS-MESSAGE-LINE
               MOVE '0'    TO ML-CC
               MOVE '*** SRB600 RUN TOTALS RECORD MISSING ***'
                           TO ML-TEXT
               WRITE RPT-RECORD FROM WS-MESSAGE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               MOVE 8 TO WS-RETURN-CODE
               GO TO 6000-EXIT
           END-IF.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE '0'              TO PL-CC.
           MOVE 'GL LINES - SRB600 / READ' TO PL-LABEL.
           MOVE WS-S6-LINES-OUT  TO PL-VALUE-1.
           MOVE WS-RN-COUNT      TO PL-VALUE-2.
           IF WS-S6-LINES-OUT = WS-RN-COUNT
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DO NOT AGREE ***' TO PL-RESULT
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'DEBITS - SRB600 / READ' TO PL-LABEL.
           MOVE WS-S6-TOTAL-DR   TO PL-VALUE-1.
           MOVE WS-RN-DR         TO PL-VALUE-2.
           IF WS-S6-TOTAL-DR = WS-RN-DR
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DO NOT AGREE ***' TO PL-RESULT
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'CREDITS - SRB600 / READ' TO PL-LABEL.
           MOVE WS-S6-TOTAL-CR   TO PL-VALUE-1.
           MOVE WS-RN-CR         TO PL-VALUE-2.
           IF WS-S6-TOTAL-CR = WS-RN-CR
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DO NOT AGREE ***' TO PL-RESULT
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'JOURNAL LEGS / ACCRUAL LINES IN' TO PL-LABEL.
           MOVE WS-S6-JRNL-IN    TO PL-VALUE-1.
           MOVE WS-S6-ACCR-IN    TO PL-VALUE-2.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'REFERENCES / OUT OF BALANCE' TO PL-LABEL.
           MOVE WS-S6-REFS-OUT   TO PL-VALUE-1.
           MOVE WS-S6-REFS-UNBAL TO PL-VALUE-2.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES           TO WS-PROOF-LINE.
           MOVE ' '              TO PL-CC.
           MOVE 'GL ACCOUNTS / UNMAPPED LINES' TO PL-LABEL.
           MOVE WS-RN-GL-ACCTS   TO PL-VALUE-1.
           MOVE WS-S6-UNMAPPED   TO PL-VALUE-2.
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       6000-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-GL-LINE.
           READ GLJIN-FILE.
           EVALUATE TRUE
               WHEN GLJIN-OK
                   IF GLJ-AMOUNT NOT NUMERIC
                       MOVE ZERO TO GLJ-AMOUNT
                   END-IF
                   IF GLJ-AMOUNT-USD NOT NUMERIC
                       MOVE ZERO TO GLJ-AMOUNT-USD
                   END-IF
               WHEN GLJIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'GLJIN' TO AB-DDNAME
                   MOVE WS-GLJIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-PRINT-LINE.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               MOVE WS-PREV-GL TO TL-GL
           END-IF.
           WRITE RPT-RECORD FROM WS-TXN-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           PERFORM 8250-SECTION-HEADINGS THRU 8250-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8250-SECTION-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 2 TO RPT-LINE-COUNT.
       8250-EXIT.
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
       8400-READ-SUMMARY.
      *----------------------------------------------------------------*
           READ GLSUMM-FILE.
           EVALUATE TRUE
               WHEN GLSUMM-OK
                   CONTINUE
               WHEN GLSUMM-EOF
                   MOVE 'Y' TO WS-SUM-EOF-SW
               WHEN OTHER
                   MOVE 'GLSUMM' TO AB-DDNAME
                   MOVE WS-GLSUMM-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8400-EXIT.
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
           CLOSE GLJIN-FILE GLSUMM-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'SRR610'        TO CT-STAGE.
           MOVE 'GLJ-IN'        TO CT-COUNTER-NAME.
           MOVE WS-RN-COUNT     TO CT-COUNT.
           MOVE WS-RN-DR        TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SRR610 GL LINES READ   : ' WS-RN-COUNT.
           DISPLAY 'SRR610 GL ACCOUNTS     : ' WS-RN-GL-ACCTS.
           DISPLAY 'SRR610 TOTAL DEBITS    : ' WS-RN-DR.
           DISPLAY 'SRR610 TOTAL CREDITS   : ' WS-RN-CR.
           DISPLAY 'SRR610 EXCEPTIONS      : ' WS-RN-EXCEPTIONS.
           DISPLAY 'SRR610 RETURN CODE     : ' WS-RETURN-CODE.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           IF WS-RETURN-CODE > 4
               MOVE 'E' TO AU-SEVERITY
               MOVE 'GL SUMMARY - JOURNAL OUT OF BALANCE' TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'GL SUMMARY REPORT ENDED' TO AU-MESSAGE
           END-IF.
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
           DISPLAY 'SRR610 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
