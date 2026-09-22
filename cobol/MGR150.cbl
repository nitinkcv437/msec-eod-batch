       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGR150.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 2011.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGR150                                            *
      * DESCRIPTION: MARGIN COLLATERAL CONCENTRATION REPORT (CREDIT    *
      *              RISK).  INPUT IS MG.ISSCONC SORTED BY LONG MARKET *
      *              VALUE DESCENDING WITH THE FIRM TOTAL LAST.        *
      *              SECTION 1  THE 50 LARGEST ISSUERS                 *
      *              SECTION 2  EVERY OTHER FLAGGED ISSUER             *
      *              FIRM TOTAL AND FLAG COUNTS.                       *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD010 / STEP070                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              CONCIN   - MSEC.PROD.MG.ISSCONC.SORTED(+1)(MGISSC)*
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2011-03-21 SPA  ORIGINAL                              CHG21402 *
      * 2011-09-12 SPA  LOAN VALUE COLUMN                     CHG22301 *
      * 2017-08-07 MHC  SINGLE ACCOUNT COLUMNS                CHG31877 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CONCIN-FILE    ASSIGN TO CONCIN
                  FILE STATUS IS WS-CONCIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  CONCIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CONCIN-REC                  PIC X(150).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGR150'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-CONCIN-STATUS        PIC X(02)  VALUE '00'.
               88  CONCIN-OK                      VALUE '00'.
               88  CONCIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-SECTION-SW           PIC X(01)  VALUE '1'.
               88  IN-TOP-SECTION                 VALUE '1'.
               88  IN-FLAGGED-SECTION             VALUE '2'.
           05  WS-TOTAL-SW             PIC X(01)  VALUE 'N'.
               88  FIRM-TOTAL-SEEN                VALUE 'Y'.
       01  WS-TOP-N                    PIC S9(04) COMP  VALUE +50.
       01  WS-RANK                     PIC S9(07) COMP-3 VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAG-F-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAG-S-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OTHER-FLAG-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOP-MV               PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOP-PCT              PIC S9(05)V9(04) COMP-3
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
       01  WS-SECTION-LINE.
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  SC-TEXT                 PIC X(60).
           05  FILLER                  PIC X(71)  VALUE SPACES.
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(05)  VALUE 'RANK'.
           05  FILLER  PIC X(07)  VALUE 'ISSUER'.
           05  FILLER  PIC X(31)  VALUE 'NAME'.
           05  FILLER  PIC X(03)  VALUE 'TY'.
           05  FILLER  PIC X(06)  VALUE ' ACCTS'.
           05  FILLER  PIC X(16)  VALUE '         LONG MV'.
           05  FILLER  PIC X(14)  VALUE '     NON-MARG'.
           05  FILLER  PIC X(16)  VALUE '      LOAN VALUE'.
           05  FILLER  PIC X(09)  VALUE '   FIRM %'.
           05  FILLER  PIC X(11)  VALUE ' LARGEST AC'.
           05  FILLER  PIC X(07)  VALUE '  ACCT%'.
           05  FILLER  PIC X(07)  VALUE '  FLAG'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  DL-RANK                 PIC ZZZ9.
           05  FILLER                  PIC X(01).
           05  DL-ISSUER               PIC X(06).
           05  FILLER                  PIC X(01).
           05  DL-NAME                 PIC X(30).
           05  FILLER                  PIC X(01).
           05  DL-SEC-TYPE             PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-ACCTS                PIC ZZ,ZZ9.
           05  DL-LONG-MV              PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  DL-NONMARG-MV           PIC ZZ,ZZZ,ZZZ,ZZ9.
           05  DL-LOAN-VALUE           PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-FIRM-PCT             PIC ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-LARGEST-ACCT         PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-ACCT-PCT             PIC ZZ9.99.
           05  FILLER                  PIC X(03).
           05  DL-FLAG                 PIC X(01).
           05  FILLER                  PIC X(05).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01)  VALUE '-'.
           05  FILLER                  PIC X(12)  VALUE SPACES.
           05  TL-NAME                 PIC X(30).
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  TL-ACCTS                PIC ZZ,ZZ9.
           05  TL-LONG-MV              PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  TL-NONMARG-MV           PIC ZZ,ZZZ,ZZZ,ZZ9.
           05  TL-LOAN-VALUE           PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(36)  VALUE SPACES.
       01  WS-SUMMARY-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(12).
           05  SM-TEXT                 PIC X(50).
           05  SM-VALUE                PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(54).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO MARGIN COLLATERAL ***'.
       COPY MGISSC.
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
           PERFORM 2000-PROCESS-RECORD THRU 2000-EXIT
               UNTIL END-OF-INPUT.
           PERFORM 3000-SUMMARY THRU 3000-EXIT.
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
           MOVE 'CONCENTRATION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT CONCIN-FILE.
           IF WS-CONCIN-STATUS NOT = '00'
               MOVE 'CONCIN' TO AB-DDNAME
               MOVE WS-CONCIN-STATUS TO AB-FILE-STATUS
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
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'MARGIN COLLATERAL CONCENTRATION BY ISSUER'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           MOVE WS-DI-CCYY     TO WS-DE-CCYY.
           MOVE WS-DI-MM       TO WS-DE-MM.
           MOVE WS-DI-DD       TO WS-DE-DD.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           MOVE 'SECTION 1 - LARGEST ISSUERS BY LONG MARKET VALUE'
                               TO SC-TEXT.
           PERFORM 8250-SECTION THRU 8250-EXIT.
           PERFORM 8000-READ-CONC THRU 8000-EXIT.
           IF END-OF-INPUT
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-RECORD.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF MIC-FIRM-TOTAL
               PERFORM 2500-FIRM-TOTAL THRU 2500-EXIT
               GO TO 2000-NEXT
           END-IF.
           ADD 1 TO WS-RANK.
           IF MIC-OVER-FIRM-LIMIT
               ADD 1 TO WS-FLAG-F-CNT
           END-IF.
           IF MIC-SINGLE-ACCT
               ADD 1 TO WS-FLAG-S-CNT
           END-IF.
           IF WS-RANK NOT > WS-TOP-N
               ADD MIC-LONG-MV     TO WS-TOP-MV
               ADD MIC-PCT-OF-FIRM TO WS-TOP-PCT
               PERFORM 2100-PRINT-ISSUER THRU 2100-EXIT
               GO TO 2000-NEXT
           END-IF.
           IF MIC-NO-FLAG
               GO TO 2000-NEXT
           END-IF.
           IF IN-TOP-SECTION
               MOVE '2' TO WS-SECTION-SW
               IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
                   PERFORM 8200-HEADINGS THRU 8200-EXIT
               END-IF
               MOVE 'SECTION 2 - OTHER FLAGGED ISSUERS' TO SC-TEXT
               PERFORM 8250-SECTION THRU 8250-EXIT
           END-IF.
           ADD 1 TO WS-OTHER-FLAG-CNT.
           PERFORM 2100-PRINT-ISSUER THRU 2100-EXIT.
       2000-NEXT.
           PERFORM 8000-READ-CONC THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-PRINT-ISSUER.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE SPACES               TO WS-DETAIL-LINE.
           MOVE ' '                  TO DL-CC.
           MOVE WS-RANK              TO DL-RANK.
           MOVE MIC-ISSUER-ID        TO DL-ISSUER.
           MOVE MIC-ISSUER-NAME      TO DL-NAME.
           MOVE MIC-SEC-TYPE         TO DL-SEC-TYPE.
           MOVE MIC-ACCT-COUNT       TO DL-ACCTS.
           MOVE MIC-LONG-MV          TO DL-LONG-MV.
           MOVE MIC-NONMARG-MV       TO DL-NONMARG-MV.
           MOVE MIC-LOAN-VALUE       TO DL-LOAN-VALUE.
           MOVE MIC-PCT-OF-FIRM      TO DL-FIRM-PCT.
           MOVE MIC-LARGEST-ACCT     TO DL-LARGEST-ACCT.
           MOVE MIC-LARGEST-ACCT-PCT TO DL-ACCT-PCT.
           MOVE MIC-FLAG             TO DL-FLAG.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT WS-PRINT-CNT.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2500-FIRM-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'Y' TO WS-TOTAL-SW.
           IF RPT-LINE-COUNT + 4 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE MIC-ISSUER-NAME  TO TL-NAME.
           MOVE MIC-ACCT-COUNT   TO TL-ACCTS.
           MOVE MIC-LONG-MV      TO TL-LONG-MV.
           MOVE MIC-NONMARG-MV   TO TL-NONMARG-MV.
           MOVE MIC-LOAN-VALUE   TO TL-LOAN-VALUE.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
       2500-EXIT.
           EXIT.
      *================================================================*
       3000-SUMMARY.
      *================================================================*
           IF RPT-LINE-COUNT + 8 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           MOVE SPACES TO WS-SUMMARY-LINE.
           MOVE '0'    TO SM-CC.
           MOVE 'ISSUERS ON FILE' TO SM-TEXT.
           MOVE WS-RANK TO SM-VALUE.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE ' '    TO SM-CC.
           MOVE 'SHARE OF COLLATERAL IN THE LARGEST 50 (PCT)'
                       TO SM-TEXT.
           MOVE WS-TOP-PCT TO SM-VALUE.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 'ISSUERS ABOVE THE FIRM LIMIT (FLAG F / B)'
                       TO SM-TEXT.
           MOVE WS-FLAG-F-CNT TO SM-VALUE.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 'ONE ACCOUNT OVER HALF OF THE ISSUER (FLAG S / B)'
                       TO SM-TEXT.
           MOVE WS-FLAG-S-CNT TO SM-VALUE.
           WRITE RPT-RECORD FROM WS-SUMMARY-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           IF NOT FIRM-TOTAL-SEEN
               MOVE '*** FIRM TOTAL RECORD MISSING ***' TO SM-TEXT
               MOVE ZERO TO SM-VALUE
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-IF.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-CONC.
      *================================================================*
           READ CONCIN-FILE INTO MIC-ISSUER-CONC-REC.
           EVALUATE TRUE
               WHEN CONCIN-OK
                   CONTINUE
               WHEN CONCIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'CONCIN' TO AB-DDNAME
                   MOVE WS-CONCIN-STATUS TO AB-FILE-STATUS
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
           MOVE 4 TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8250-SECTION.
      *----------------------------------------------------------------*
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
       8250-EXIT.
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
           CLOSE CONCIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'MGR150'        TO CT-STAGE.
           MOVE 'ISSCONC-IN'    TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT     TO CT-COUNT.
           MOVE WS-TOP-MV       TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'MGR150 RECORDS READ        : ' WS-READ-CNT.
           DISPLAY 'MGR150 ISSUERS PRINTED     : ' WS-PRINT-CNT.
           DISPLAY 'MGR150 OTHER FLAGGED       : ' WS-OTHER-FLAG-CNT.
           DISPLAY 'MGR150 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'CONCENTRATION REPORT ENDED' TO AU-MESSAGE.
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
           DISPLAY 'MGR150 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
