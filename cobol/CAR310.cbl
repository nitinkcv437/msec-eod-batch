      *================================================================*
      * PROGRAM    : CAR310                                            *
      * DESCRIPTION: CORPORATE ACTION ENTITLEMENT REPORT.              *
      *              LISTS THE ENTITLEMENTS CALCULATED TODAY BY CAB300 *
      *              ONE SECTION PER EVENT (NEW PAGE), ONE LINE PER    *
      *              HOLDER, EVENT TOTALS AND AN EVENT PROOF:          *
      *                - ELIGIBLE QTY AGREES WITH THE EVENT MASTER     *
      *                - SHARE EVENTS: SUM OF NEW SHARES AGREES WITH   *
      *                  TOTAL ELIGIBLE QTY X RATIO                    *
      *                - CASH EVENTS: SUM OF GROSS AGREES WITH TOTAL   *
      *                  ELIGIBLE QTY X RATE WITHIN ONE CENT PER HOLDER*
      *                - NET = GROSS - WITHHOLDING                     *
      *----------------------------------------------------------------*
      * JOB        : MSCAD030  STEP020                                 *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              ENTLIN    ENTITLEMENTS (CAENTL) IN EVENT/ACCOUNT  *
      *                        SEQUENCE  MSEC.PROD.CA.ENTL(+1)         *
      *              CAEVENT   EVENT MASTER KSDS (RANDOM, READ ONLY)   *
      * OUTPUT     : RPTFILE   REPORT, FB 133 WITH ASA CONTROL         *
      * CALLS      : CMU050 CMU060 CMU080 CMASM02                      *
      * RETURN CODE: 00 CLEAN   04 PROOF BREAK OR EVENT NOT ON MASTER  *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1995-06-19 RJK  ORIGINAL                                       *
      * 1996-08-05 RJK  DUE BILL INDICATOR                    CHG02731 *
      * 1997-03-17 RJK  CASH PROOF TOLERANCE - GROSS IS       CHG03390 *
      *                 TRUNCATED PER HOLDER                           *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2003-05-19 KAP  TAX STATUS / COUNTRY COLUMN           CHG11020 *
      * 2008-02-25 KAP  CASH MERGER                           CHG17444 *
      * 2011-06-20 SPA  STANDARD HEADINGS CMRPTHD, CMU060/080 CHG21877 *
      * 2017-08-14 MFO  ELIGIBLE QTY PROOF VS EVENT MASTER    CHG31044 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAR310.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  06/19/95.
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
           SELECT ENTLIN-FILE     ASSIGN TO ENTLIN
                  FILE STATUS IS WS-ENTLIN-STATUS.
           SELECT CAEVENT-FILE    ASSIGN TO CAEVENT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CAE-EVENT-ID
                  FILE STATUS IS WS-CAEVENT-STATUS.
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
       FD  ENTLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CAENTL.
      *
       FD  CAEVENT-FILE.
           COPY CAEVENT.
      *
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-REC                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAR310'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-ENTLIN-STATUS        PIC X(02) VALUE '00'.
           05  WS-CAEVENT-STATUS       PIC X(02) VALUE '00'.
               88  CAEVENT-OK                    VALUE '00'.
               88  CAEVENT-NOTFND                VALUE '23'.
           05  WS-REPORT-STATUS        PIC X(02) VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01) VALUE 'N'.
               88  WS-EOF                        VALUE 'Y'.
           05  WS-EVENT-FOUND-SW       PIC X(01) VALUE 'N'.
               88  WS-EVENT-FOUND                VALUE 'Y'.
           05  WS-PROOF-SW             PIC X(01) VALUE 'Y'.
               88  WS-PROOF-OK                   VALUE 'Y'.
               88  WS-PROOF-BREAK                VALUE 'N'.
      *
       01  WS-CURR-EVENT               PIC X(12) VALUE LOW-VALUES.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
       01  WS-HOLD-LINE                PIC X(133).
      *
      *----------------------------------------------------------------*
      * EVENT AND GRAND TOTALS                                         *
      *----------------------------------------------------------------*
       01  WS-EVENT-TOTALS.
           05  WS-EV-COUNT             PIC S9(07)       COMP-3.
           05  WS-EV-DUEBILL           PIC S9(07)       COMP-3.
           05  WS-EV-ELIG-QTY          PIC S9(13)V9(04) COMP-3.
           05  WS-EV-GROSS             PIC S9(15)V99    COMP-3.
           05  WS-EV-WHT               PIC S9(15)V99    COMP-3.
           05  WS-EV-NET               PIC S9(15)V99    COMP-3.
           05  WS-EV-NEW-SHARES        PIC S9(13)V9(04) COMP-3.
           05  WS-EV-WHOLE             PIC S9(13)       COMP-3.
           05  WS-EV-FRAC              PIC S9(07)V9(06) COMP-3.
           05  WS-EV-CIL               PIC S9(15)V99    COMP-3.
      *
       01  WS-GRAND-TOTALS.
           05  WS-GT-EVENTS            PIC S9(07)       COMP-3
                                                 VALUE ZERO.
           05  WS-GT-COUNT             PIC S9(07)       COMP-3
                                                 VALUE ZERO.
           05  WS-GT-GROSS             PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-GT-WHT               PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-GT-NET               PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-GT-WHOLE             PIC S9(13)       COMP-3
                                                 VALUE ZERO.
           05  WS-GT-CIL               PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-GT-BREAKS            PIC S9(07)       COMP-3
                                                 VALUE ZERO.
           05  WS-GT-NOT-ON-MASTER     PIC S9(07)       COMP-3
                                                 VALUE ZERO.
      *
       01  WS-PROOF-WORK.
           05  WS-PF-EXPECTED          PIC S9(15)V9(06) COMP-3.
           05  WS-PF-ACTUAL            PIC S9(15)V9(06) COMP-3.
           05  WS-PF-DIFF              PIC S9(15)V9(06) COMP-3.
           05  WS-PF-TOLERANCE         PIC S9(15)V9(06) COMP-3.
           05  WS-PF-RATE              PIC S9(07)V9(08) COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(09) COMP-3 VALUE ZERO.
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
           05  WS-DO-DASH1             PIC X(01) VALUE '-'.
           05  WS-DO-MM                PIC 9(02).
           05  WS-DO-DASH2             PIC X(01) VALUE '-'.
           05  WS-DO-DD                PIC 9(02).
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
           COPY CMRPTHD.
      *
       01  WS-EVT-HDR-1.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(07) VALUE 'EVENT: '.
           05  EH1-EVENT-ID            PIC X(12).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  EH1-TYPE                PIC X(03).
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  EH1-TYPE-DESC           PIC X(16).
           05  FILLER                  PIC X(07) VALUE 'CUSIP: '.
           05  EH1-CUSIP               PIC X(09).
           05  FILLER                  PIC X(02) VALUE SPACES.
           05  EH1-DESC                PIC X(40).
           05  FILLER                  PIC X(10) VALUE '  STATUS: '.
           05  EH1-STATUS              PIC X(02).
           05  FILLER                  PIC X(21) VALUE SPACES.
       01  WS-EVT-HDR-2.
           05  FILLER                  PIC X(01) VALUE ' '.
           05  FILLER                  PIC X(08) VALUE 'RECORD: '.
           05  EH2-RECORD-DATE         PIC X(10).
           05  FILLER                  PIC X(07) VALUE '  PAY: '.
           05  EH2-PAY-DATE            PIC X(10).
           05  FILLER                  PIC X(09) VALUE '  TERMS: '.
           05  EH2-TERMS               PIC X(40).
           05  FILLER                  PIC X(06) VALUE '  CCY '.
           05  EH2-CCY                 PIC X(03).
           05  FILLER                  PIC X(08) VALUE '  FRAC: '.
           05  EH2-FRAC                PIC X(01).
           05  FILLER                  PIC X(11) VALUE '  TAXABLE: '.
           05  EH2-TAXABLE             PIC X(01).
           05  FILLER                  PIC X(18) VALUE SPACES.
      *
       01  WS-TERMS-RATE.
           05  FILLER                  PIC X(05) VALUE 'RATE '.
           05  WS-TR-RATE              PIC ZZZZZZ9.99999999.
           05  FILLER                  PIC X(19) VALUE SPACES.
       01  WS-TERMS-RATIO.
           05  FILLER                  PIC X(06) VALUE 'RATIO '.
           05  WS-TX-NEW               PIC ZZZ9.999999.
           05  FILLER                  PIC X(01) VALUE ':'.
           05  WS-TX-OLD               PIC ZZZ9.999999.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  WS-TX-NEW-CUSIP         PIC X(09).
           05  FILLER                  PIC X(01) VALUE SPACE.
      *
       01  WS-COL-HDR-1.
           05  FILLER                  PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(11) VALUE 'ACCOUNT'.
           05  FILLER                  PIC X(05) VALUE 'LOC'.
           05  FILLER                  PIC X(03) VALUE 'TY'.
           05  FILLER                  PIC X(05) VALUE 'TAX'.
           05  FILLER                  PIC X(18) VALUE
               '     ELIGIBLE QTY'.
           05  FILLER                  PIC X(16) VALUE
               '      GROSS CASH'.
           05  FILLER                  PIC X(07) VALUE ' WHT %'.
           05  FILLER                  PIC X(14) VALUE
               '   WITHHOLDING'.
           05  FILLER                  PIC X(16) VALUE
               '        NET CASH'.
           05  FILLER                  PIC X(12) VALUE
               ' WHOLE SHRS'.
           05  FILLER                  PIC X(09) VALUE
               ' FRACTION'.
           05  FILLER                  PIC X(12) VALUE
               ' CASH IN LU'.
           05  FILLER                  PIC X(04) VALUE ' DB'.
       01  WS-COL-HDR-2.
           05  FILLER                  PIC X(01) VALUE ' '.
           05  FILLER                  PIC X(132) VALUE ALL '-'.
      *
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-LOC                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-ACCT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-TAX-STATUS           PIC X(01).
           05  DL-TAX-DASH             PIC X(01).
           05  DL-TAX-CTRY             PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-ELIG-QTY             PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-GROSS                PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-WHT-RATE             PIC Z9.999.
           05  FILLER                  PIC X(01).
           05  DL-WHT                  PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-NET                  PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-WHOLE                PIC -ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-FRAC                 PIC Z.999999.
           05  FILLER                  PIC X(01).
           05  DL-CIL                  PIC -ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-DUE-BILL             PIC X(01).
           05  FILLER                  PIC X(02).
      *
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  TL-LABEL                PIC X(22).
           05  FILLER                  PIC X(01).
           05  TL-ELIG-QTY             PIC -ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  TL-GROSS                PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(08).
           05  TL-WHT                  PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-NET                  PIC -ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-WHOLE                PIC -ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(10).
           05  TL-CIL                  PIC -ZZZ,ZZ9.99.
           05  FILLER                  PIC X(05).
      *
       01  WS-PROOF-LINE.
           05  PL-CC                   PIC X(01).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  PL-LABEL                PIC X(34).
           05  FILLER                  PIC X(10) VALUE ' EXPECTED '.
           05  PL-EXPECTED             PIC -ZZZ,ZZZ,ZZZ,ZZ9.999999.
           05  FILLER                  PIC X(09) VALUE '  ACTUAL '.
           05  PL-ACTUAL               PIC -ZZZ,ZZZ,ZZZ,ZZ9.999999.
           05  FILLER                  PIC X(07) VALUE '  DIFF '.
           05  PL-DIFF                 PIC -ZZ,ZZ9.999999.
           05  FILLER                  PIC X(01) VALUE SPACE.
           05  PL-RESULT               PIC X(06).
      *
       01  WS-MSG-LINE.
           05  ML-CC                   PIC X(01) VALUE '0'.
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  ML-TEXT                 PIC X(100).
           05  FILLER                  PIC X(27) VALUE SPACES.
      *
       01  WS-GRAND-LINE.
           05  GL-CC                   PIC X(01).
           05  FILLER                  PIC X(05) VALUE SPACES.
           05  GL-LABEL                PIC X(40).
           05  GL-VALUE                PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(68) VALUE SPACES.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZZ,ZZ9.
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
           IF WS-CURR-EVENT NOT = LOW-VALUES
               PERFORM 3000-EVENT-END THRU 3000-EXIT.
           PERFORM 4000-GRAND-TOTALS THRU 4000-EXIT.
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
           MOVE 'CA ENTITLEMENT REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           OPEN INPUT ENTLIN-FILE.
           IF WS-ENTLIN-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ENTLIN-STATUS    TO AB-FILE-STATUS
               MOVE 'ENTLIN'            TO AB-DDNAME
               MOVE 'OPEN FAILED ON ENTITLEMENT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN INPUT CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON EVENT MASTER' TO AB-MESSAGE
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
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE WS-PROGRAM-ID      TO RPT-H1-REPORT-ID
                                      RPT-H2-PROGRAM.
           MOVE TS-TIMESTAMP(1:10) TO RPT-H1-RUN-DATE.
           MOVE 'CORPORATE ACTION ENTITLEMENT REPORT' TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE        TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT        TO RPT-H2-BUS-DATE.
      *
           PERFORM 8000-READ-ENTL THRU 8000-EXIT.
           IF WS-EOF
               PERFORM 7100-PAGE-HEADING THRU 7100-EXIT
               MOVE 'NO ENTITLEMENTS WERE CALCULATED TODAY'
                                     TO ML-TEXT
               MOVE WS-MSG-LINE      TO REPORT-REC
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2000-PROCESS.
      *----------------------------------------------------------------*
           IF ENT-EVENT-ID NOT = WS-CURR-EVENT
               IF WS-CURR-EVENT NOT = LOW-VALUES
                   PERFORM 3000-EVENT-END THRU 3000-EXIT.
           IF ENT-EVENT-ID NOT = WS-CURR-EVENT
               PERFORM 2100-EVENT-START THRU 2100-EXIT.
      *
           PERFORM 2200-DETAIL THRU 2200-EXIT.
           PERFORM 8000-READ-ENTL THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2100-EVENT-START.
      *----------------------------------------------------------------*
           MOVE ENT-EVENT-ID TO WS-CURR-EVENT.
           INITIALIZE WS-EVENT-TOTALS.
           ADD 1 TO WS-GT-EVENTS.
      *
           MOVE ENT-EVENT-ID TO CAE-EVENT-ID.
           READ CAEVENT-FILE.
           IF CAEVENT-OK
               MOVE 'Y' TO WS-EVENT-FOUND-SW
           ELSE
               IF CAEVENT-NOTFND
                   MOVE 'N' TO WS-EVENT-FOUND-SW
                   ADD 1 TO WS-GT-NOT-ON-MASTER
                   MOVE SPACES TO CAE-EVENT-REC
                   MOVE ENT-EVENT-ID   TO CAE-EVENT-ID
                   MOVE ENT-EVENT-TYPE TO CAE-EVENT-TYPE
                   MOVE ENT-CUSIP      TO CAE-CUSIP
                   MOVE '** EVENT NOT ON EVENT MASTER **' TO CAE-DESC
                   MOVE ZERO TO CAE-RECORD-DATE CAE-PAY-DATE
               ELSE
                   MOVE '2100-EVENT-START' TO AB-PARAGRAPH
                   MOVE 1002                TO AB-ABEND-CODE
                   MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
                   MOVE 'CAEVENT'           TO AB-DDNAME
                   MOVE ENT-EVENT-ID        TO AB-KEY
                   MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
                   GO TO 9999-ABEND.
      *
           MOVE CAE-EVENT-ID    TO EH1-EVENT-ID.
           MOVE CAE-EVENT-TYPE  TO EH1-TYPE.
           EVALUATE CAE-EVENT-TYPE
               WHEN 'CDV'  MOVE 'CASH DIVIDEND'   TO EH1-TYPE-DESC
               WHEN 'SDV'  MOVE 'STOCK DIVIDEND'  TO EH1-TYPE-DESC
               WHEN 'SPL'  MOVE 'FORWARD SPLIT'   TO EH1-TYPE-DESC
               WHEN 'RSP'  MOVE 'REVERSE SPLIT'   TO EH1-TYPE-DESC
               WHEN 'MRG'  MOVE 'CASH MERGER'     TO EH1-TYPE-DESC
               WHEN OTHER  MOVE 'UNKNOWN'         TO EH1-TYPE-DESC
           END-EVALUATE.
           MOVE CAE-CUSIP       TO EH1-CUSIP.
           MOVE CAE-DESC        TO EH1-DESC.
           MOVE CAE-STATUS      TO EH1-STATUS.
           MOVE CAE-RECORD-DATE TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT     TO EH2-RECORD-DATE.
           MOVE CAE-PAY-DATE    TO WS-DATE-IN.
           PERFORM 7000-FORMAT-DATE THRU 7000-EXIT.
           MOVE WS-DATE-OUT     TO EH2-PAY-DATE.
           MOVE CAE-CCY         TO EH2-CCY.
           MOVE CAE-FRAC-METHOD TO EH2-FRAC.
           MOVE CAE-TAXABLE-FLAG TO EH2-TAXABLE.
           MOVE SPACES          TO EH2-TERMS.
           IF WS-EVENT-FOUND
               IF CAE-CASH-DIV
                   MOVE CAE-RATE         TO WS-TR-RATE
                   MOVE WS-TERMS-RATE    TO EH2-TERMS
               ELSE
               IF CAE-CASH-MERGER
                   MOVE CAE-MRG-CASH-RATE TO WS-TR-RATE
                   MOVE WS-TERMS-RATE    TO EH2-TERMS
               ELSE
                   MOVE CAE-RATIO-NEW    TO WS-TX-NEW
                   MOVE CAE-RATIO-OLD    TO WS-TX-OLD
                   MOVE CAE-NEW-CUSIP    TO WS-TX-NEW-CUSIP
                   MOVE WS-TERMS-RATIO   TO EH2-TERMS.
      *
           PERFORM 7100-PAGE-HEADING THRU 7100-EXIT.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2200-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES             TO WS-DETAIL-LINE.
           MOVE ' '                TO DL-CC.
           MOVE ENT-ACCT-NO        TO DL-ACCT.
           MOVE ENT-LOCATION       TO DL-LOC.
           MOVE ENT-ACCT-TYPE      TO DL-ACCT-TYPE.
           MOVE ENT-TAX-STATUS     TO DL-TAX-STATUS.
           MOVE '-'                TO DL-TAX-DASH.
           MOVE ENT-TAX-COUNTRY    TO DL-TAX-CTRY.
           MOVE ENT-ELIGIBLE-QTY   TO DL-ELIG-QTY.
           IF ENT-EVENT-TYPE = 'CDV' OR 'MRG'
               MOVE ENT-GROSS-CASH TO DL-GROSS
               COMPUTE DL-WHT-RATE = ENT-WHT-RATE * 100
               MOVE ENT-WHT-AMOUNT TO DL-WHT
               MOVE ENT-NET-CASH   TO DL-NET
           ELSE
               MOVE ENT-WHOLE-SHARES TO DL-WHOLE
               MOVE ENT-FRAC-SHARES  TO DL-FRAC
               MOVE ENT-CIL-AMOUNT   TO DL-CIL.
           MOVE ENT-DUE-BILL-FLAG  TO DL-DUE-BILL.
           IF ENT-DUE-BILL-FLAG = 'N'
               MOVE SPACE TO DL-DUE-BILL.
           MOVE WS-DETAIL-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
      *
           ADD 1                   TO WS-EV-COUNT.
           IF ENT-DUE-BILL-FLAG = 'Y'
               ADD 1               TO WS-EV-DUEBILL.
           ADD ENT-ELIGIBLE-QTY    TO WS-EV-ELIG-QTY.
           ADD ENT-GROSS-CASH      TO WS-EV-GROSS.
           ADD ENT-WHT-AMOUNT      TO WS-EV-WHT.
           ADD ENT-NET-CASH        TO WS-EV-NET.
           ADD ENT-NEW-SHARES      TO WS-EV-NEW-SHARES.
           ADD ENT-WHOLE-SHARES    TO WS-EV-WHOLE.
           ADD ENT-FRAC-SHARES     TO WS-EV-FRAC.
           ADD ENT-CIL-AMOUNT      TO WS-EV-CIL.
       2200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * EVENT TOTALS AND PROOF                                         *
      *----------------------------------------------------------------*
       3000-EVENT-END.
           MOVE SPACES             TO WS-TOTAL-LINE.
           MOVE '0'                TO TL-CC.
           MOVE 'EVENT TOTAL'      TO TL-LABEL.
           MOVE WS-EV-ELIG-QTY     TO TL-ELIG-QTY.
           MOVE WS-EV-GROSS        TO TL-GROSS.
           MOVE WS-EV-WHT          TO TL-WHT.
           MOVE WS-EV-NET          TO TL-NET.
           MOVE WS-EV-WHOLE        TO TL-WHOLE.
           MOVE WS-EV-CIL          TO TL-CIL.
           MOVE WS-TOTAL-LINE      TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
      *
           MOVE SPACES             TO ML-TEXT.
           MOVE WS-EV-COUNT        TO WS-DISP-COUNT.
           STRING 'HOLDERS: ' WS-DISP-COUNT DELIMITED BY SIZE
                  INTO ML-TEXT.
           MOVE WS-EV-DUEBILL      TO WS-DISP-COUNT.
           MOVE 'DUE BILLS:'       TO ML-TEXT(25:10).
           MOVE WS-DISP-COUNT      TO ML-TEXT(35:11).
           MOVE ' '                TO ML-CC.
           MOVE WS-MSG-LINE        TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0'                TO ML-CC.
      *
           MOVE 'Y' TO WS-PROOF-SW.
           IF NOT WS-EVENT-FOUND
               MOVE '*** EVENT NOT ON EVENT MASTER - NO PROOF ***'
                                     TO ML-TEXT
               MOVE WS-MSG-LINE      TO REPORT-REC
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
               MOVE 'N' TO WS-PROOF-SW
               GO TO 3000-ACCUMULATE.
      *
      *    PROOF 1 - ELIGIBLE QUANTITY VS EVENT MASTER (CAB200)
           MOVE 'ELIGIBLE QTY VS EVENT MASTER'  TO PL-LABEL.
           MOVE CAE-ELIG-QTY       TO WS-PF-EXPECTED.
           MOVE WS-EV-ELIG-QTY     TO WS-PF-ACTUAL.
           MOVE ZERO               TO WS-PF-TOLERANCE.
           PERFORM 3900-PROOF-LINE THRU 3900-EXIT.
      *
           EVALUATE TRUE
               WHEN CAE-CASH-DIV
                   PERFORM 3100-PROOF-CASH THRU 3100-EXIT
               WHEN CAE-CASH-MERGER
                   PERFORM 3100-PROOF-CASH THRU 3100-EXIT
               WHEN CAE-STOCK-EVENT
                   PERFORM 3200-PROOF-SHARES THRU 3200-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      *
       3000-ACCUMULATE.
           IF WS-PROOF-BREAK
               ADD 1 TO WS-GT-BREAKS
               MOVE 4 TO WS-RETURN-CODE
               MOVE 'WRIT'           TO AU-FUNCTION
               MOVE 'PROOFBRK'       TO AU-EVENT
               MOVE 'W'              TO AU-SEVERITY
               MOVE WS-CURR-EVENT    TO AU-KEY
               MOVE 'ENTITLEMENT PROOF OUT OF TOLERANCE' TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS.
           ADD WS-EV-COUNT   TO WS-GT-COUNT.
           ADD WS-EV-GROSS   TO WS-GT-GROSS.
           ADD WS-EV-WHT     TO WS-GT-WHT.
           ADD WS-EV-NET     TO WS-GT-NET.
           ADD WS-EV-WHOLE   TO WS-GT-WHOLE.
           ADD WS-EV-CIL     TO WS-GT-CIL.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * CASH PROOF.  GROSS IS TRUNCATED PER HOLDER (CDV) OR ROUNDED    *
      * (MRG) SO ALLOW ONE CENT PER HOLDER.                            *
      *----------------------------------------------------------------*
       3100-PROOF-CASH.
           IF CAE-CASH-DIV
               MOVE CAE-RATE           TO WS-PF-RATE
           ELSE
               MOVE CAE-MRG-CASH-RATE  TO WS-PF-RATE.
           COMPUTE WS-PF-EXPECTED = WS-EV-ELIG-QTY * WS-PF-RATE.
           MOVE WS-EV-GROSS        TO WS-PF-ACTUAL.
           COMPUTE WS-PF-TOLERANCE = WS-EV-COUNT * .01.
           MOVE 'GROSS CASH VS ELIGIBLE X RATE'  TO PL-LABEL.
           PERFORM 3900-PROOF-LINE THRU 3900-EXIT.
      *
           COMPUTE WS-PF-EXPECTED = WS-EV-GROSS - WS-EV-WHT.
           MOVE WS-EV-NET          TO WS-PF-ACTUAL.
           MOVE ZERO               TO WS-PF-TOLERANCE.
           MOVE 'NET CASH VS GROSS - WITHHOLDING'  TO PL-LABEL.
           PERFORM 3900-PROOF-LINE THRU 3900-EXIT.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * SHARE PROOF.  NEW SHARES ARE CARRIED TO 4 DECIMALS PER HOLDER. *
      *----------------------------------------------------------------*
       3200-PROOF-SHARES.
           IF CAE-FWD-SPLIT
               COMPUTE WS-PF-EXPECTED =
                   WS-EV-ELIG-QTY * CAE-RATIO-NEW / CAE-RATIO-OLD
                   - WS-EV-ELIG-QTY
           ELSE
               COMPUTE WS-PF-EXPECTED =
                   WS-EV-ELIG-QTY * CAE-RATIO-NEW / CAE-RATIO-OLD.
           MOVE WS-EV-NEW-SHARES   TO WS-PF-ACTUAL.
           COMPUTE WS-PF-TOLERANCE = WS-EV-COUNT * .0001.
           MOVE 'NEW SHARES VS ELIGIBLE X RATIO'  TO PL-LABEL.
           PERFORM 3900-PROOF-LINE THRU 3900-EXIT.
      *
      *    WHOLE + FRACTIONS - INFORMATION ONLY (ROUND UP/DOWN METHODS)
           COMPUTE WS-PF-ACTUAL = WS-EV-WHOLE + WS-EV-FRAC.
           MOVE WS-EV-NEW-SHARES   TO WS-PF-EXPECTED.
           COMPUTE WS-PF-DIFF = WS-PF-EXPECTED - WS-PF-ACTUAL.
           MOVE ' '                TO PL-CC.
           MOVE 'WHOLE + FRACTIONS (INFORMATION)' TO PL-LABEL.
           MOVE WS-PF-EXPECTED     TO PL-EXPECTED.
           MOVE WS-PF-ACTUAL       TO PL-ACTUAL.
           MOVE WS-PF-DIFF         TO PL-DIFF.
           MOVE SPACES             TO PL-RESULT.
           MOVE WS-PROOF-LINE      TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3900-PROOF-LINE.
      *----------------------------------------------------------------*
           COMPUTE WS-PF-DIFF = WS-PF-EXPECTED - WS-PF-ACTUAL.
           MOVE ' '                TO PL-CC.
           MOVE WS-PF-EXPECTED     TO PL-EXPECTED.
           MOVE WS-PF-ACTUAL       TO PL-ACTUAL.
           MOVE WS-PF-DIFF         TO PL-DIFF.
           IF WS-PF-DIFF < ZERO
               COMPUTE WS-PF-DIFF = ZERO - WS-PF-DIFF.
           IF WS-PF-DIFF > WS-PF-TOLERANCE
               MOVE '*BREAK' TO PL-RESULT
               MOVE 'N' TO WS-PROOF-SW
           ELSE
               MOVE '  OK  ' TO PL-RESULT.
           MOVE WS-PROOF-LINE      TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       3900-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       4000-GRAND-TOTALS.
      *----------------------------------------------------------------*
           MOVE SPACES TO EH1-EVENT-ID EH1-TYPE EH1-TYPE-DESC
                          EH1-CUSIP EH1-DESC EH1-STATUS.
           MOVE 'GRAND TOTALS' TO EH1-DESC.
           MOVE SPACES TO EH2-RECORD-DATE EH2-PAY-DATE EH2-TERMS
                          EH2-CCY EH2-FRAC EH2-TAXABLE.
           PERFORM 7150-SUMMARY-HEADING THRU 7150-EXIT.
      *
           MOVE '0'                        TO GL-CC.
           MOVE 'EVENTS REPORTED'          TO GL-LABEL.
           MOVE WS-GT-EVENTS               TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE ' '                        TO GL-CC.
           MOVE 'ENTITLEMENTS'             TO GL-LABEL.
           MOVE WS-GT-COUNT                TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE 'GROSS CASH'               TO GL-LABEL.
           MOVE WS-GT-GROSS                TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE 'WITHHOLDING'              TO GL-LABEL.
           MOVE WS-GT-WHT                  TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE 'NET CASH'                 TO GL-LABEL.
           MOVE WS-GT-NET                  TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE 'CASH IN LIEU'             TO GL-LABEL.
           MOVE WS-GT-CIL                  TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE 'WHOLE SHARES'             TO GL-LABEL.
           MOVE WS-GT-WHOLE                TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE '0'                        TO GL-CC.
           MOVE 'EVENTS WITH PROOF BREAKS' TO GL-LABEL.
           MOVE WS-GT-BREAKS               TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
           MOVE ' '                        TO GL-CC.
           MOVE 'EVENTS NOT ON EVENT MASTER' TO GL-LABEL.
           MOVE WS-GT-NOT-ON-MASTER        TO GL-VALUE.
           PERFORM 4900-GRAND-LINE THRU 4900-EXIT.
      *
           MOVE RPT-END-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4000-EXIT.
           EXIT.
      *
       4900-GRAND-LINE.
           MOVE WS-GRAND-LINE TO REPORT-REC.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4900-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       7000-FORMAT-DATE.
      *----------------------------------------------------------------*
           IF WS-DATE-IN = ZERO
               MOVE SPACES TO WS-DATE-OUT
               GO TO 7000-EXIT.
           MOVE WS-DI-CCYY TO WS-DO-CCYY.
           MOVE WS-DI-MM   TO WS-DO-MM.
           MOVE WS-DI-DD   TO WS-DO-DD.
           MOVE '-'        TO WS-DO-DASH1
                              WS-DO-DASH2.
       7000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * PAGE HEADING - STANDARD HEADINGS, EVENT BLOCK, COLUMN HEADINGS *
      *----------------------------------------------------------------*
       7100-PAGE-HEADING.
           PERFORM 7150-SUMMARY-HEADING THRU 7150-EXIT.
           MOVE WS-EVT-HDR-2 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE WS-COL-HDR-1 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE WS-COL-HDR-2 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           ADD 4 TO RPT-LINE-COUNT.
       7100-EXIT.
           EXIT.
      *
       7150-SUMMARY-HEADING.
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           MOVE RPT-HEADING-1 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE RPT-HEADING-2 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE WS-EVT-HDR-1 TO REPORT-REC.
           PERFORM 8200-PUT-LINE THRU 8200-EXIT.
           MOVE 4 TO RPT-LINE-COUNT.
       7150-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       8000-READ-ENTL.
      *----------------------------------------------------------------*
           READ ENTLIN-FILE
               AT END
                   MOVE 'Y' TO WS-EOF-SW
                   GO TO 8000-EXIT.
           IF WS-ENTLIN-STATUS NOT = '00'
               MOVE '8000-READ-ENTL'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLIN-STATUS    TO AB-FILE-STATUS
               MOVE 'ENTLIN'            TO AB-DDNAME
               MOVE 'READ FAILED ON ENTITLEMENT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-RECS-READ.
       8000-EXIT.
           EXIT.
      *
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
           CLOSE ENTLIN-FILE.
           IF WS-ENTLIN-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLIN-STATUS    TO AB-FILE-STATUS
               MOVE 'ENTLIN'            TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ENTITLEMENT FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON EVENT MASTER' TO AB-MESSAGE
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
           IF WS-GT-NOT-ON-MASTER > ZERO
               MOVE 4 TO WS-RETURN-CODE.
      *
           MOVE 'POST'             TO CT-FUNCTION.
           MOVE DC-BUS-DATE        TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID      TO CT-PROGRAM.
           MOVE 'CAR310'           TO CT-STAGE.
           MOVE 'ENTL-IN'          TO CT-COUNTER-NAME.
           MOVE WS-RECS-READ       TO CT-COUNT.
           COMPUTE CT-AMOUNT = WS-GT-NET + WS-GT-CIL.
           MOVE WS-GT-WHOLE        TO CT-QTY-HASH.
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
           DISPLAY '* CAR310  CA ENTITLEMENT REPORT - RUN STATISTICS *'.
           DISPLAY '*************************************************'.
           MOVE WS-RECS-READ      TO WS-DISP-COUNT.
           DISPLAY ' ENTITLEMENTS READ        : ' WS-DISP-COUNT.
           MOVE WS-GT-EVENTS      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS REPORTED          : ' WS-DISP-COUNT.
           MOVE WS-GT-BREAKS      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS WITH PROOF BREAK  : ' WS-DISP-COUNT.
           MOVE RPT-PAGE-COUNT    TO WS-DISP-COUNT.
           DISPLAY ' REPORT PAGES             : ' WS-DISP-COUNT.
           MOVE WS-LINES-WRITTEN  TO WS-DISP-COUNT.
           DISPLAY ' REPORT LINES             : ' WS-DISP-COUNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE 'END'            TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA ENTITLEMENT REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'           TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'CAR310 - ABENDING: ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
