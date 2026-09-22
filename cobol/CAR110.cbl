      *================================================================*
      * PROGRAM    : CAR110                                            *
      * DESCRIPTION: CORPORATE ACTION ANNOUNCEMENT VALIDATION REPORT.  *
      *              PRINTS THE RESULT OF THE DAILY ANNOUNCEMENT LOAD  *
      *              (CAB100) GROUPED BY RESULT - ACCEPTED, FILE LEVEL *
      *              ERRORS, REJECTED, ACCEPTED WITH WARNINGS - WITH   *
      *              THE MESSAGE TEXT FOR EVERY CODE AND A SUMMARY.    *
      *----------------------------------------------------------------*
      * JOB        : MSCAD010  STEP030                                 *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              CAVALIN   VALIDATION RESULTS SORTED BY RESULT +   *
      *                        VENDOR REF (CAVALID)                    *
      *                        MSEC.PROD.CA.ANNVAL.SORTED(+1)          *
      * OUTPUT     : RPTFILE   REPORT, FB 133 WITH ASA CONTROL         *
      * CALLS      : CMU050 CMU060 CMU080 CMASM02                      *
      * RETURN CODE: 00 CLEAN   04 REJECTS OR FILE LEVEL ERRORS LISTED *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1993-08-23 DWB  ORIGINAL                                       *
      * 1995-10-19 RJK  UPDATE/CANCEL ACTIONS                 CHG01370 *
      * 1998-11-02 TLM  Y2K - 10 CHARACTER DATES ON REPORT    CHG04471 *
      * 2004-07-12 KAP  MESSAGE FREQUENCY SUMMARY             CHG12118 *
      * 2008-02-25 KAP  MRG MESSAGES                          CHG17444 *
      * 2011-06-20 SPA  STANDARD HEADINGS CMRPTHD, CMU060/080 CHG21877 *
      * 2019-11-18 MFO  W006 CANCEL AFTER ELIGIBILITY         CHG34410 *
      * 2024-02-12 NVR  T+1 WORDING FOR V007                  CHG41120 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAR110.
       AUTHOR.        D W BRANDT.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  08/23/93.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE   ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CAVALIN-FILE    ASSIGN TO CAVALIN
                  FILE STATUS IS WS-CAVALIN-STATUS.
           SELECT REPORT-FILE     ASSIGN TO RPTFILE
                  FILE STATUS IS WS-REPORT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  CAVALIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CAVALID.
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAR110'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-CAVALIN-STATUS       PIC X(02) VALUE '00'.
           05  WS-REPORT-STATUS        PIC X(02) VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01) VALUE 'Y'.
               88  WS-FIRST-RECORD               VALUE 'Y'.
      *
       01  WS-PREV-RESULT              PIC X(01) VALUE SPACES.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-SECTION-COUNT        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-ACCEPTED         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-WARNING          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-REJECTED         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-FILE-LEVEL       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-NEW              PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-UPD              PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-CXL              PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CNT-UNKNOWN-MSG      PIC S9(07) COMP-3 VALUE ZERO.
      *
       01  WS-SUB                      PIC S9(04) COMP VALUE ZERO.
       01  WS-MSUB                     PIC S9(04) COMP VALUE ZERO.
       01  WS-MSG-FOUND-SW             PIC X(01) VALUE 'N'.
           88  WS-MSG-FOUND                      VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * MESSAGE TEXT TABLE.  KEEP IN STEP WITH CAB100.                 *
      *----------------------------------------------------------------*
       01  WS-MSG-TABLE-VALUES.
           05  FILLER PIC X(54) VALUE
               'V001INVALID ACTION CODE - MUST BE N, U OR C           '.
           05  FILLER PIC X(54) VALUE
               'V002VENDOR REFERENCE MISSING                          '.
           05  FILLER PIC X(54) VALUE
               'V003SECURITY NOT ON SECURITY MASTER                   '.
           05  FILLER PIC X(54) VALUE
               'V004SECURITY DELISTED OR MATURED                      '.
           05  FILLER PIC X(54) VALUE
               'V005INVALID EVENT TYPE                                '.
           05  FILLER PIC X(54) VALUE
               'V006INVALID OR MISSING DATE                           '.
           05  FILLER PIC X(54) VALUE
               'V007DATE SEQUENCE - EX/RECORD/PAY (T+1 RULES)         '.
           05  FILLER PIC X(54) VALUE
               'V008RECORD DATE IS NOT A BUSINESS DAY                 '.
           05  FILLER PIC X(54) VALUE
               'V009RATE MISSING, ZERO OR NOT NUMERIC                 '.
           05  FILLER PIC X(54) VALUE
               'V010INVALID RATIO FOR EVENT TYPE                      '.
           05  FILLER PIC X(54) VALUE
               'V011INVALID FRACTION METHOD - MUST BE C, D OR U       '.
           05  FILLER PIC X(54) VALUE
               'V012CASH IN LIEU PRICE REQUIRED                       '.
           05  FILLER PIC X(54) VALUE
               'V013INVALID CURRENCY                                  '.
           05  FILLER PIC X(54) VALUE
               'V014NEW EVENT - VENDOR REFERENCE ALREADY ON FILE      '.
           05  FILLER PIC X(54) VALUE
               'V015EVENT NOT FOUND FOR UPDATE/CANCEL                 '.
           05  FILLER PIC X(54) VALUE
               'V016EVENT STATUS DOES NOT ALLOW THIS ACTION           '.
           05  FILLER PIC X(54) VALUE
               'V017EVENT ALREADY CANCELLED                           '.
           05  FILLER PIC X(54) VALUE
               'V018RECORD DATE BEFORE BUSINESS DATE                  '.
           05  FILLER PIC X(54) VALUE
               'W001SECURITY IS HALTED                                '.
           05  FILLER PIC X(54) VALUE
               'W002PAY DATE IS NOT A BUSINESS DAY                    '.
           05  FILLER PIC X(54) VALUE
               'W003ANNOUNCEMENT DATE AFTER EX DATE                   '.
           05  FILLER PIC X(54) VALUE
               'W004TAXABLE FLAG INVALID - DEFAULTED TO Y             '.
           05  FILLER PIC X(54) VALUE
               'W005RECORD DATE ALREADY PASSED - CHECK ELIGIBILITY    '.
           05  FILLER PIC X(54) VALUE
               'W006CANCELLED AFTER ELIGIBILITY WAS TAKEN             '.
           05  FILLER PIC X(54) VALUE
               'F001TRAILER COUNT DOES NOT MATCH EVENT RECORDS        '.
           05  FILLER PIC X(54) VALUE
               'F002HEADER RECORD MISSING OR FEED EMPTY               '.
           05  FILLER PIC X(54) VALUE
               'F003TRAILER RECORD MISSING                            '.
           05  FILLER PIC X(54) VALUE
               'F004FEED FILE DATE IS NOT CURRENT                     '.
           05  FILLER PIC X(54) VALUE
               'F006UNKNOWN RECORD TYPE IN FEED                       '.
           05  FILLER PIC X(54) VALUE
               'F007RECORDS FOUND AFTER TRAILER - IGNORED             '.
       01  WS-MSG-TABLE REDEFINES WS-MSG-TABLE-VALUES.
           05  WS-MSG-ENTRY            OCCURS 30 TIMES.
               10  WS-MSG-T-CODE       PIC X(04).
               10  WS-MSG-T-TEXT       PIC X(50).
       01  WS-MSG-TABLE-SIZE           PIC S9(04) COMP VALUE +30.
      *
       01  WS-MSG-COUNTS.
           05  WS-MSG-FREQ             PIC S9(07) COMP-3
                                       OCCURS 30 TIMES.
      *
      *----------------------------------------------------------------*
      * DATE FORMATTING                                                *
      *----------------------------------------------------------------*
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-OUT.
           05  WS-DO-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01) VALUE '-'.
           05  WS-DO-MM                PIC 9(02).
           05  FILLER                  PIC X(01) VALUE '-'.
           05  WS-DO-DD                PIC 9(02).
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
           COPY CMRPTHD.
      *
       01  WS-COL-HDR-1.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(08) VALUE '  REC NO'.
           05  FILLER                  PIC X(13) VALUE ' VENDOR REF'.
           05  FILLER                  PIC X(04) VALUE 'ACT'.
           05  FILLER                  PIC X(13) VALUE 'EVENT ID'.
           05  FILLER                  PIC X(10) VALUE 'CUSIP'.
           05  FILLER                  PIC X(04) VALUE 'TYP'.
           05  FILLER                  PIC X(11) VALUE 'EX DATE'.
           05  FILLER                  PIC X(11) VALUE 'RECORD DT'.
           05  FILLER                  PIC X(11) VALUE 'PAY DATE'.
           05  FILLER                  PIC X(23) VALUE
               'TERMS'.
           05  FILLER                  PIC X(07) VALUE 'STATUS'.
           05  FILLER                  PIC X(17) VALUE 'MESSAGES'.
       01  WS-COL-HDR-2.
           05  FILLER                  PIC X(01) VALUE ' '.
           05  FILLER                  PIC X(08) VALUE ' -------'.
           05  FILLER                  PIC X(13) VALUE ' ------------'.
           05  FILLER                  PIC X(04) VALUE '---'.
           05  FILLER                  PIC X(13) VALUE '------------'.
           05  FILLER                  PIC X(10) VALUE '---------'.
           05  FILLER                  PIC X(04) VALUE '---'.
           05  FILLER                  PIC X(11) VALUE '----------'.
           05  FILLER                  PIC X(11) VALUE '----------'.
           05  FILLER                  PIC X(11) VALUE '----------'.
           05  FILLER                  PIC X(23) VALUE
               '----------------------'.
           05  FILLER                  PIC X(07) VALUE '------'.
           05  FILLER                  PIC X(17) VALUE
               '--------------'.
      *
       01  WS-SECTION-LINE.
           05  WS-SL-CC                PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  FILLER                  PIC X(12) VALUE '*** SECTION '.
           05  WS-SL-CODE              PIC X(01).
           05  FILLER                  PIC X(03) VALUE ' - '.
           05  WS-SL-TITLE             PIC X(40).
           05  FILLER                  PIC X(75) VALUE SPACES.
      *
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  DL-SEQ                  PIC ZZZZZZ9.
           05  FILLER                  PIC X(01).
           05  DL-VREF                 PIC X(12).
           05  FILLER                  PIC X(01).
           05  DL-ACTION               PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-EVENT-ID             PIC X(12).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  DL-TYPE                 PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-EX-DATE              PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-REC-DATE             PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-PAY-DATE             PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-TERMS                PIC X(22).
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(06).
           05  FILLER                  PIC X(01).
           05  DL-MSGS                 PIC X(14).
           05  FILLER                  PIC X(03).
      *
       01  WS-TERMS-RATE.
           05  FILLER                  PIC X(05) VALUE 'RATE '.
           05  WS-TR-RATE              PIC ZZZZZZ9.99999999.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  WS-TERMS-RATIO.
           05  WS-TX-NEW               PIC ZZZZ9.9999.
           05  FILLER                  PIC X(01) VALUE ':'.
           05  WS-TX-OLD               PIC ZZZZ9.9999.
           05  FILLER                  PIC X(01) VALUE SPACE.
       01  WS-STATUS-CHG.
           05  WS-SC-OLD               PIC X(02).
           05  WS-SC-ARROW             PIC X(02) VALUE '->'.
           05  WS-SC-NEW               PIC X(02).
      *
       01  WS-MSG-LINE.
           05  ML-CC                   PIC X(01) VALUE ' '.
           05  FILLER                  PIC X(22) VALUE SPACES.
           05  FILLER                  PIC X(04) VALUE '*** '.
           05  ML-CODE                 PIC X(04).
           05  FILLER                  PIC X(03) VALUE ' - '.
           05  ML-TEXT                 PIC X(50).
           05  FILLER                  PIC X(49) VALUE SPACES.
      *
       01  WS-SECTION-TOTAL.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  FILLER                  PIC X(20) VALUE
               'RECORDS IN SECTION: '.
           05  WS-ST-COUNT             PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(95) VALUE SPACES.
      *
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  FILLER                  PIC X(50) VALUE
               '*** NO ANNOUNCEMENT RECORDS RECEIVED TODAY ***'.
           05  FILLER                  PIC X(72) VALUE SPACES.
      *
       01  WS-SUMMARY-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  SM-LABEL                PIC X(40).
           05  SM-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(03) VALUE SPACES.
           05  SM-TEXT                 PIC X(50).
           05  FILLER                  PIC X(22) VALUE SPACES.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZ9.
       01  WS-HOLD-LINE                PIC X(133).
      *
           COPY CMDATEW.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS THRU 2000-EXIT
               UNTIL WS-EOF.
           PERFORM 3000-FINISH-SECTIONS THRU 3000-EXIT.
           PERFORM 4000-PRINT-SUMMARY THRU 4000-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
      *----------------------------------------------------------------*
           INITIALIZE AB-ABEND-PARMS.
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON DATE CARD' TO AB-MESSAGE
               GO TO 9999-ABEND.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DC-VALID-CARD
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1005                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE DATECARD-FILE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID    TO AU-PROGRAM.
           MOVE 'START'          TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE DC-BUS-DATE      TO AU-BUS-DATE.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA VALIDATION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           OPEN INPUT CAVALIN-FILE.
           IF WS-CAVALIN-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAVALIN-STATUS   TO AB-FILE-STATUS
               MOVE 'CAVALIN'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON VALIDATION FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN OUTPUT REPORT-FILE.
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS    TO AB-FILE-STATUS
               MOVE 'RPTFILE'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON REPORT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           PERFORM 1100-CLEAR-FREQ THRU 1100-EXIT
               VARYING WS-MSUB FROM 1 BY 1
               UNTIL WS-MSUB > WS-MSG-TABLE-SIZE.
      *
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE WS-PROGRAM-ID      TO RPT-H1-REPORT-ID
                                      RPT-H2-PROGRAM.
           MOVE TS-TIMESTAMP(1:10) TO RPT-H1-RUN-DATE.
           MOVE 'CORPORATE ACTION ANNOUNCEMENT VALIDATION REPORT'
                                   TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE        TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT        TO RPT-H2-BUS-DATE.
      *
           PERFORM 8000-READ-VALID THRU 8000-EXIT.
           IF WS-EOF
               PERFORM 7100-PAGE-HEADING THRU 7100-EXIT
               MOVE WS-NONE-LINE TO REPORT-REC
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-CLEAR-FREQ.
           MOVE ZERO TO WS-MSG-FREQ (WS-MSUB).
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2000-PROCESS.
      *----------------------------------------------------------------*
           IF VAL-RESULT NOT = WS-PREV-RESULT
               PERFORM 2100-NEW-SECTION THRU 2100-EXIT.
      *
           ADD 1 TO WS-SECTION-COUNT.
           IF VAL-ACCEPTED    ADD 1 TO WS-CNT-ACCEPTED.
           IF VAL-WARNING     ADD 1 TO WS-CNT-WARNING.
           IF VAL-REJECTED    ADD 1 TO WS-CNT-REJECTED.
           IF VAL-FILE-LEVEL  ADD 1 TO WS-CNT-FILE-LEVEL.
      *
           IF VAL-ACCEPTED OR VAL-WARNING
               IF VAL-ACTION = 'N'  ADD 1 TO WS-CNT-NEW.
           IF VAL-ACCEPTED OR VAL-WARNING
               IF VAL-ACTION = 'U'  ADD 1 TO WS-CNT-UPD.
           IF VAL-ACCEPTED OR VAL-WARNING
               IF VAL-ACTION = 'C'  ADD 1 TO WS-CNT-CXL.
      *
           IF VAL-FILE-LEVEL
               PERFORM 2400-FILE-LEVEL-LINE THRU 2400-EXIT
           ELSE
               PERFORM 2200-DETAIL-LINE THRU 2200-EXIT.
      *
           PERFORM 2300-MESSAGE-LINES THRU 2300-EXIT
               VARYING WS-SUB FROM 1 BY 1
               UNTIL WS-SUB > VAL-MSG-COUNT
                  OR WS-SUB > 3.
      *
           PERFORM 8000-READ-VALID THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2100-NEW-SECTION.
      *----------------------------------------------------------------*
           IF NOT WS-FIRST-RECORD
               PERFORM 2900-SECTION-TOTAL THRU 2900-EXIT.
           MOVE 'N' TO WS-FIRST-SW.
           MOVE VAL-RESULT TO WS-PREV-RESULT.
           MOVE ZERO TO WS-SECTION-COUNT.
           MOVE VAL-RESULT TO WS-SL-CODE.
           IF VAL-ACCEPTED
               MOVE 'ANNOUNCEMENTS ACCEPTED' TO WS-SL-TITLE.
           IF VAL-FILE-LEVEL
               MOVE 'FEED FILE LEVEL ERRORS' TO WS-SL-TITLE.
           IF VAL-REJECTED
               MOVE 'ANNOUNCEMENTS REJECTED' TO WS-SL-TITLE.
           IF VAL-WARNING
               MOVE 'ANNOUNCEMENTS ACCEPTED WITH WARNINGS'
                                             TO WS-SL-TITLE.
           IF NOT VAL-ACCEPTED AND NOT VAL-FILE-LEVEL
                               AND NOT VAL-REJECTED
                               AND NOT VAL-WARNING
               MOVE 'UNKNOWN RESULT CODE' TO WS-SL-TITLE.
      *    EVERY SECTION STARTS ON A NEW PAGE
           PERFORM 7100-PAGE-HEADING THRU 7100-EXIT.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2200-DETAIL-LINE.
      *----------------------------------------------------------------*
           MOVE SPACES            TO WS-DETAIL-LINE.
           MOVE ' '               TO DL-CC.
           MOVE VAL-SEQ-NO        TO DL-SEQ.
           MOVE VAL-VENDOR-REF    TO DL-VREF.
           IF VAL-ACTION = 'N'
               MOVE 'NEW' TO DL-ACTION
           ELSE
           IF VAL-ACTION = 'U'
               MOVE 'UPD' TO DL-ACTION
           ELSE
           IF VAL-ACTION = 'C'
               MOVE 'CXL' TO DL-ACTION
           ELSE
               MOVE VAL-ACTION TO DL-ACTION.
           MOVE VAL-EVENT-ID      TO DL-EVENT-ID.
           MOVE VAL-CUSIP         TO DL-CUSIP.
           MOVE VAL-EVENT-TYPE    TO DL-TYPE.
           MOVE VAL-EX-DATE       TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT       TO DL-EX-DATE.
           MOVE VAL-RECORD-DATE   TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT       TO DL-REC-DATE.
           MOVE VAL-PAY-DATE      TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT       TO DL-PAY-DATE.
      *
           IF VAL-ACTION = 'C'
               MOVE SPACES        TO DL-TERMS
           ELSE
           IF VAL-EVENT-TYPE = 'CDV' OR 'MRG'
               MOVE VAL-RATE      TO WS-TR-RATE
               MOVE WS-TERMS-RATE TO DL-TERMS
           ELSE
           IF VAL-EVENT-TYPE = 'SDV' AND VAL-RATE > ZERO
               MOVE VAL-RATE      TO WS-TR-RATE
               MOVE WS-TERMS-RATE TO DL-TERMS
           ELSE
           IF VAL-EVENT-TYPE = 'SDV' OR 'SPL' OR 'RSP'
               MOVE VAL-RATIO-NEW  TO WS-TX-NEW
               MOVE VAL-RATIO-OLD  TO WS-TX-OLD
               MOVE WS-TERMS-RATIO TO DL-TERMS
           ELSE
               MOVE SPACES         TO DL-TERMS.
      *
           IF VAL-OLD-STATUS = SPACES AND VAL-NEW-STATUS = SPACES
               MOVE SPACES TO DL-STATUS
           ELSE
               MOVE VAL-OLD-STATUS TO WS-SC-OLD
               MOVE VAL-NEW-STATUS TO WS-SC-NEW
               MOVE WS-STATUS-CHG  TO DL-STATUS.
           IF VAL-OLD-STATUS = SPACES AND VAL-NEW-STATUS NOT = SPACES
               MOVE '  ' TO WS-SC-OLD
               MOVE WS-STATUS-CHG  TO DL-STATUS.
      *
           STRING VAL-MSG-CODE (1) ' '
                  VAL-MSG-CODE (2) ' '
                  VAL-MSG-CODE (3)
                  DELIMITED BY SIZE INTO DL-MSGS.
           MOVE WS-DETAIL-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       2200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2300-MESSAGE-LINES.
      *----------------------------------------------------------------*
           IF VAL-MSG-CODE (WS-SUB) = SPACES
               GO TO 2300-EXIT.
           MOVE VAL-MSG-CODE (WS-SUB) TO ML-CODE.
           MOVE 'N' TO WS-MSG-FOUND-SW.
           PERFORM 2350-LOOKUP-MSG THRU 2350-EXIT
               VARYING WS-MSUB FROM 1 BY 1
               UNTIL WS-MSUB > WS-MSG-TABLE-SIZE
                  OR WS-MSG-FOUND.
           IF NOT WS-MSG-FOUND
               MOVE '** MESSAGE CODE NOT IN CAR110 TABLE **'
                                     TO ML-TEXT
               ADD 1 TO WS-CNT-UNKNOWN-MSG.
           MOVE WS-MSG-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       2300-EXIT.
           EXIT.
      *
       2350-LOOKUP-MSG.
           IF WS-MSG-T-CODE (WS-MSUB) = VAL-MSG-CODE (WS-SUB)
               MOVE 'Y' TO WS-MSG-FOUND-SW
               MOVE WS-MSG-T-TEXT (WS-MSUB) TO ML-TEXT
               ADD 1 TO WS-MSG-FREQ (WS-MSUB).
       2350-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2400-FILE-LEVEL-LINE.
      *----------------------------------------------------------------*
           MOVE SPACES            TO WS-DETAIL-LINE.
           MOVE ' '               TO DL-CC.
           MOVE VAL-SEQ-NO        TO DL-SEQ.
           MOVE 'FEED FILE'       TO DL-VREF.
           MOVE VAL-MSG-CODE (1)  TO DL-MSGS.
           MOVE WS-DETAIL-LINE    TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       2400-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2900-SECTION-TOTAL.
      *----------------------------------------------------------------*
           MOVE WS-SECTION-COUNT TO WS-ST-COUNT.
           MOVE WS-SECTION-TOTAL TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       2900-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3000-FINISH-SECTIONS.
      *----------------------------------------------------------------*
           IF NOT WS-FIRST-RECORD
               PERFORM 2900-SECTION-TOTAL THRU 2900-EXIT.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       4000-PRINT-SUMMARY.
      *----------------------------------------------------------------*
           MOVE 'S' TO WS-SL-CODE.
           MOVE 'VALIDATION SUMMARY' TO WS-SL-TITLE.
           PERFORM 7100-PAGE-HEADING THRU 7100-EXIT.
      *
           MOVE '0'                      TO SM-CC.
           MOVE 'VALIDATION RECORDS READ' TO SM-LABEL.
           MOVE WS-RECS-READ             TO SM-COUNT.
           MOVE SPACES                   TO SM-TEXT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
           MOVE ' '                      TO SM-CC.
           MOVE '  ACCEPTED'             TO SM-LABEL.
           MOVE WS-CNT-ACCEPTED          TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
           MOVE '  ACCEPTED WITH WARNING' TO SM-LABEL.
           MOVE WS-CNT-WARNING           TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
           MOVE '  REJECTED'             TO SM-LABEL.
           MOVE WS-CNT-REJECTED          TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
           MOVE '  FILE LEVEL ERRORS'    TO SM-LABEL.
           MOVE WS-CNT-FILE-LEVEL        TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
      *
           MOVE '0'                      TO SM-CC.
           MOVE 'APPLIED TO EVENT MASTER - NEW' TO SM-LABEL.
           MOVE WS-CNT-NEW               TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
           MOVE ' '                      TO SM-CC.
           MOVE '                        - UPDATED' TO SM-LABEL.
           MOVE WS-CNT-UPD               TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
           MOVE '                        - CANCELLED' TO SM-LABEL.
           MOVE WS-CNT-CXL               TO SM-COUNT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
      *
           MOVE '0'                      TO SM-CC.
           MOVE 'MESSAGE FREQUENCY'      TO SM-LABEL.
           MOVE ZERO                     TO SM-COUNT.
           MOVE SPACES                   TO SM-TEXT.
           MOVE WS-SUMMARY-LINE          TO REPORT-REC.
           MOVE SPACES                   TO REPORT-REC(52:7).
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           PERFORM 4100-FREQ-LINE THRU 4100-EXIT
               VARYING WS-MSUB FROM 1 BY 1
               UNTIL WS-MSUB > WS-MSG-TABLE-SIZE.
           IF WS-CNT-UNKNOWN-MSG > ZERO
               MOVE ' '                  TO SM-CC
               MOVE '  UNKNOWN CODES'    TO SM-LABEL
               MOVE WS-CNT-UNKNOWN-MSG   TO SM-COUNT
               MOVE SPACES               TO SM-TEXT
               PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
      *
           MOVE RPT-END-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4000-EXIT.
           EXIT.
      *
       4100-FREQ-LINE.
           IF WS-MSG-FREQ (WS-MSUB) = ZERO
               GO TO 4100-EXIT.
           MOVE ' '                          TO SM-CC.
           MOVE SPACES                       TO SM-LABEL.
           MOVE WS-MSG-T-CODE (WS-MSUB)      TO SM-LABEL(3:4).
           MOVE WS-MSG-FREQ (WS-MSUB)        TO SM-COUNT.
           MOVE WS-MSG-T-TEXT (WS-MSUB)      TO SM-TEXT.
           PERFORM 4900-SUMMARY-LINE THRU 4900-EXIT.
       4100-EXIT.
           EXIT.
      *
       4900-SUMMARY-LINE.
           MOVE WS-SUMMARY-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4900-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * CCYYMMDD -> CCYY-MM-DD  (ZERO -> SPACES)                       *
      *----------------------------------------------------------------*
       7000-FORMAT-DATE.
           IF WS-DATE-IN = ZERO
               MOVE SPACES TO WS-DATE-OUT
               GO TO 7000-EXIT.
           MOVE WS-DI-CCYY TO WS-DO-CCYY.
           MOVE WS-DI-MM   TO WS-DO-MM.
           MOVE WS-DI-DD   TO WS-DO-DD.
           MOVE '-' TO WS-DATE-OUT(5:1)
                       WS-DATE-OUT(8:1).
       7000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       7100-PAGE-HEADING.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           MOVE RPT-HEADING-1 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE RPT-HEADING-2 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE WS-SECTION-LINE TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE 4 TO RPT-LINE-COUNT.
           IF WS-SL-CODE = 'S'
               GO TO 7100-EXIT.
           MOVE WS-COL-HDR-1 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE WS-COL-HDR-2 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
       7100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       8000-READ-VALID.
      *----------------------------------------------------------------*
           READ CAVALIN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO 8000-EXIT.
           IF WS-CAVALIN-STATUS NOT = '00'
               MOVE '8000-READ-VALID'   TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAVALIN-STATUS   TO AB-FILE-STATUS
               MOVE 'CAVALIN'           TO AB-DDNAME
               MOVE 'READ FAILED ON VALIDATION FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-RECS-READ.
           IF VAL-MSG-COUNT NOT NUMERIC
               MOVE ZERO TO VAL-MSG-COUNT.
       8000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * WRITE A BODY LINE - PAGE BREAK AT 60 LINES                     *
      *----------------------------------------------------------------*
       8100-WRITE-LINE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               MOVE REPORT-REC TO WS-HOLD-LINE
               PERFORM 7100-PAGE-HEADING THRU 7100-EXIT
               MOVE WS-HOLD-LINE TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
           IF REPORT-REC(1:1) = '0'
               ADD 1 TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *
       8200-PUT-LINE.
           WRITE REPORT-REC.
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '8200-PUT-LINE'     TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS    TO AB-FILE-STATUS
               MOVE 'RPTFILE'           TO AB-DDNAME
               MOVE 'WRITE FAILED ON REPORT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-LINES-WRITTEN.
       8200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       9000-TERMINATE.
      *----------------------------------------------------------------*
           CLOSE CAVALIN-FILE.
           IF WS-CAVALIN-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAVALIN-STATUS   TO AB-FILE-STATUS
               MOVE 'CAVALIN'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON VALIDATION FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE REPORT-FILE.
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS    TO AB-FILE-STATUS
               MOVE 'RPTFILE'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON REPORT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           IF WS-CNT-REJECTED > ZERO
           OR WS-CNT-FILE-LEVEL > ZERO
               MOVE 4 TO WS-RETURN-CODE.
      *
           MOVE 'POST'             TO CT-FUNCTION.
           MOVE DC-BUS-DATE        TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID      TO CT-PROGRAM.
           MOVE 'CAR110'           TO CT-STAGE.
           MOVE 'VALID-IN'         TO CT-COUNTER-NAME.
           MOVE WS-RECS-READ       TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT
                                      CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1010                TO AB-ABEND-CODE
               MOVE 'CTLTOTS'           TO AB-DDNAME
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND.
           MOVE 'CLOS'             TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *
           DISPLAY '*************************************************'.
           DISPLAY '* CAR110  CA VALIDATION REPORT  - RUN STATISTICS *'.
           DISPLAY '*************************************************'.
           MOVE WS-RECS-READ      TO WS-DISP-COUNT.
           DISPLAY ' VALIDATION RECORDS READ  : ' WS-DISP-COUNT.
           MOVE WS-CNT-REJECTED   TO WS-DISP-COUNT.
           DISPLAY ' REJECTED ANNOUNCEMENTS   : ' WS-DISP-COUNT.
           MOVE WS-CNT-FILE-LEVEL TO WS-DISP-COUNT.
           DISPLAY ' FILE LEVEL ERRORS        : ' WS-DISP-COUNT.
           MOVE RPT-PAGE-COUNT    TO WS-DISP-COUNT.
           DISPLAY ' REPORT PAGES             : ' WS-DISP-COUNT.
           MOVE WS-LINES-WRITTEN  TO WS-DISP-COUNT.
           DISPLAY ' REPORT LINES             : ' WS-DISP-COUNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE 'END'            TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE 'CA VALIDATION REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'           TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'CAR110 - ABENDING: ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
