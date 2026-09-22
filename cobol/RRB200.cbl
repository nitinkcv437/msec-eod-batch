       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRB200.
       AUTHOR.        M H CHEN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  OCTOBER 2019.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRB200                                            *
      * DESCRIPTION: CONSOLIDATED EVENT REPORTING - DAILY SUBMISSION   *
      *              EXTRACT.  ONE PIPE-DELIMITED 'E' LINE PER FINAL   *
      *              TRADE OF THE DAY (EQUITY, PREFERRED, ADR, FUND),  *
      *              FIXED INCOME (CB/MU/GV) IS REPORTED BY THE BOND   *
      *              DESK VENDOR AND IS LEFT OUT.                      *
      *                                                                *
      *              H|FILEID|FIRMID|SUBMIT-DATE|RECORD-COUNT          *
      *              E|SEQ|EVENT-TYPE|FIRM-REF|EVENT-TS-UTC|SYMBOL|    *
      *                SIDE|QTY|PRICE|ACCT-TYPE|CAPACITY|DESK|DEPT|    *
      *                SESSION|HANDLING                                *
      *              T|RECORD-COUNT|QTY-HASH                           *
      *                                                                *
      *              EVENT TIMES ARE CAPTURED IN NEW YORK TIME AND ARE *
      *              SUBMITTED IN UTC.  EASTERN TIME IS UTC-5, OR      *
      *              UTC-4 WHILE DAYLIGHT SAVING TIME IS IN EFFECT.    *
      *              NUMBERS ARE SUBMITTED WITHOUT LEADING ZEROS AND   *
      *              WITHOUT TRAILING DECIMAL ZEROS (100, 25.5).       *
      *                                                                *
      *              PASS 1 COUNTS THE EVENTS FOR THE HEADER LINE,     *
      *              PASS 2 WRITES THE LINES.                          *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRRD020 / STEP010  (IKJEFT01 - DB2 PLAN MSRRPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              SYSIN    - CONTROL CARDS RRP020A (OPTIONAL)       *
      *                         FILE-VERSION=NN  RESUBMISSION VERSION  *
      *                         LATE-DAYS=NNN    AS-OF WARNING LIMIT   *
      *                         TEST-ACCT=X(10)  NOT REPORTED (MAX 20) *
      *              TRADEIN  - MSEC.PROD.TC.TRADES.FINAL(0) (TCTRADE) *
      * OUTPUT     : CATOUT   - MSEC.PROD.RR.CATSUB(+1)      (RRCATL)  *
      * CALLS      : CMD010 (LISTING SYMBOL), CMU010 (DOW, ADDC),      *
      *              CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0 CLEAN                                           *
      *              4 SYMBOL MISSING, TIME INVALID, SIDE UNKNOWN,     *
      *                EVENTS OLDER THAN LATE-DAYS, QTY/PRICE NOT > 0  *
      *                (NOT REPORTED), BAD CONTROL CARD                *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2019-10-14 MHC  ORIGINAL - PHASE 2A EQUITY EVENTS     CHG34410 *
      * 2020-02-03 MHC  UTC TIMESTAMPS (WAS EASTERN)          CHG34702 *
      * 2020-06-22 MHC  GO-LIVE - HEADER / TRAILER LINES      CHG35015 *
      * 2020-11-09 JLR  CANCEL AND CORRECTION EVENTS          CHG35388 *
      * 2021-03-15 JLR  NO LEADING/TRAILING ZEROS IN NUMBERS  CHG35790 *
      * 2022-05-02 JLR  SESSION CODE, SYMBOL FROM SEC MASTER  CHG37120 *
      * 2023-01-09 MHC  OLD DST RULE FOR AS-OF EVENTS BEFORE  CHG38044 *
      *                 2007 (HISTORICAL CORRECTIONS PROJECT)          *
      * 2023-06-12 JLR  TEST ACCOUNTS NOT REPORTED, FILE      CHG38460 *
      *                 VERSION CARD FOR RESUBMISSIONS                 *
      * 2024-05-20 NVR  T+1 - NO CHANGE, RECOMPILE            CHG40551 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD       ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT TRADEIN-FILE   ASSIGN TO TRADEIN
                  FILE STATUS IS WS-TRADEIN-STATUS.
           SELECT CATOUT-FILE    ASSIGN TO CATOUT
                  FILE STATUS IS WS-CATOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  TRADEIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRADEIN-REC                 PIC X(400).
       FD  CATOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CATOUT-REC                  PIC X(400).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRB200'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-TRADEIN-STATUS       PIC X(02)  VALUE '00'.
               88  TRADEIN-OK                     VALUE '00'.
               88  TRADEIN-EOF                    VALUE '10'.
           05  WS-CATOUT-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-TRADES                  VALUE 'Y'.
           05  WS-PASS                 PIC 9(01)  VALUE 1.
               88  COUNTING-PASS                  VALUE 1.
               88  WRITING-PASS                   VALUE 2.
           05  WS-ELIGIBLE-SW          PIC X(01)  VALUE 'N'.
               88  EVENT-ELIGIBLE                 VALUE 'Y'.
           05  WS-DST-SW               PIC X(01)  VALUE 'N'.
               88  DST-IN-EFFECT                  VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-REJECT-SW            PIC X(01)  VALUE 'N'.
               88  EVENT-REJECTED                 VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * SUBMISSION CONSTANTS                                           *
      *----------------------------------------------------------------*
       01  WS-SUBMISSION-CONSTANTS.
           05  WS-FIRM-ID              PIC X(07)  VALUE 'MSEC001'.
           05  WS-FILE-KIND            PIC X(04)  VALUE '_TRD'.
           05  WS-FILE-VERSION         PIC X(03)  VALUE '_01'.
           05  WS-TS-FRACTION          PIC X(04)  VALUE '.000'.
           05  WS-OPEN-TIME            PIC 9(06)  VALUE 093000.
           05  WS-CLOSE-TIME           PIC 9(06)  VALUE 160000.
      *----------------------------------------------------------------*
      * US EASTERN TIME ZONE.                                          *
      *   STANDARD TIME  UTC-5          DAYLIGHT TIME  UTC-4           *
      *   CHANGE AT 02:00 LOCAL TIME ON THE CHANGE-OVER SUNDAY.        *
      *   FROM 2007 (ENERGY POLICY ACT 2005): SECOND SUNDAY IN MARCH   *
      *   TO FIRST SUNDAY IN NOVEMBER.                                 *
      *   1987 - 2006: FIRST SUNDAY IN APRIL TO LAST SUNDAY IN         *
      *   OCTOBER.                                                     *
      *----------------------------------------------------------------*
       01  WS-TIME-ZONE-RULES.
           05  WS-EST-OFFSET-HH        PIC 9(02)  VALUE 05.
           05  WS-EDT-OFFSET-HH        PIC 9(02)  VALUE 04.
           05  WS-DST-CHANGE-TIME      PIC 9(06)  VALUE 020000.
           05  WS-NEW-RULE-YEAR        PIC 9(04)  VALUE 2007.
           05  WS-NEW-START-MMDD       PIC 9(04)  VALUE 0301.
           05  WS-NEW-START-WEEK       PIC 9(01)  VALUE 2.
           05  WS-NEW-END-MMDD         PIC 9(04)  VALUE 1101.
           05  WS-OLD-START-MMDD       PIC 9(04)  VALUE 0401.
           05  WS-OLD-END-MMDD         PIC 9(04)  VALUE 1031.
      *----------------------------------------------------------------*
      * DST WINDOW CACHE - ONE ENTRY PER YEAR SEEN                     *
      *----------------------------------------------------------------*
       01  WS-DST-CACHE.
           05  WS-DST-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-DST-ENTRY            OCCURS 40 TIMES
                                       INDEXED BY DST-IDX.
               10  WS-DST-YEAR         PIC 9(04).
               10  WS-DST-START        PIC 9(08).
               10  WS-DST-END          PIC 9(08).
       01  WS-DST-MAX                  PIC S9(04) COMP  VALUE 40.
       01  WS-DST-WORK.
           05  WS-DW-YEAR              PIC 9(04).
           05  WS-DW-START             PIC 9(08).
           05  WS-DW-END               PIC 9(08).
           05  WS-DW-DATE              PIC 9(08).
           05  WS-DW-DATE-R REDEFINES WS-DW-DATE.
               10  WS-DW-CCYY          PIC 9(04).
               10  WS-DW-MMDD          PIC 9(04).
           05  WS-DW-DOW               PIC S9(04) COMP.
           05  WS-DW-DAY               PIC S9(04) COMP.
           05  WS-DW-ADJ               PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * TIME CONVERSION WORK                                           *
      *----------------------------------------------------------------*
       01  WS-LOCAL-TS.
           05  WS-LOCAL-DATE           PIC 9(08).
           05  WS-LOCAL-DATE-R REDEFINES WS-LOCAL-DATE.
               10  WS-LOCAL-CCYY       PIC 9(04).
               10  WS-LOCAL-MM         PIC 9(02).
               10  WS-LOCAL-DD         PIC 9(02).
           05  WS-LOCAL-TIME           PIC 9(06).
           05  WS-LOCAL-TIME-R REDEFINES WS-LOCAL-TIME.
               10  WS-LOCAL-HH         PIC 9(02).
               10  WS-LOCAL-MI         PIC 9(02).
               10  WS-LOCAL-SS         PIC 9(02).
       01  WS-UTC-TS.
           05  WS-UTC-DATE             PIC 9(08).
           05  WS-UTC-HH-WORK          PIC S9(04) COMP.
           05  WS-UTC-TIME.
               10  WS-UTC-HH           PIC 9(02).
               10  WS-UTC-MI           PIC 9(02).
               10  WS-UTC-SS           PIC 9(02).
       01  WS-EVENT-FRACTION           PIC X(04).
       01  WS-ENTRY-TS.
           05  WS-ETS-CCYY             PIC 9(04).
           05  WS-ETS-DASH-1           PIC X(01).
           05  WS-ETS-MM               PIC 9(02).
           05  WS-ETS-DASH-2           PIC X(01).
           05  WS-ETS-DD               PIC 9(02).
           05  WS-ETS-DASH-3           PIC X(01).
           05  WS-ETS-HH               PIC 9(02).
           05  WS-ETS-DOT-1            PIC X(01).
           05  WS-ETS-MI               PIC 9(02).
           05  WS-ETS-DOT-2            PIC X(01).
           05  WS-ETS-SS               PIC 9(02).
           05  WS-ETS-DOT-3            PIC X(01).
           05  WS-ETS-MILLI            PIC 9(03).
           05  WS-ETS-MICRO            PIC 9(03).
       01  WS-UTC-TEXT.
           05  WS-UT-DATE              PIC 9(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  WS-UT-TIME              PIC 9(06).
           05  WS-UT-FRACTION          PIC X(04).
      *----------------------------------------------------------------*
      * NUMBER FORMATTING - NO LEADING ZEROS, NO TRAILING DECIMAL      *
      * ZEROS, NO DECIMAL POINT FOR WHOLE NUMBERS                      *
      *----------------------------------------------------------------*
       01  WS-FMT-WORK.
           05  WS-FMT-IN               PIC S9(13)V9(08) COMP-3.
           05  WS-FMT-EDIT             PIC -(14)9.9(08).
           05  WS-FMT-OUT              PIC X(24).
           05  WS-FMT-LEN              PIC S9(04) COMP.
           05  WS-FMT-LEAD             PIC S9(04) COMP.
           05  WS-FMT-END              PIC S9(04) COMP.
       01  WS-FIELD-TEXTS.
           05  WS-SEQ-TEXT             PIC X(24).
           05  WS-QTY-TEXT             PIC X(24).
           05  WS-PRICE-TEXT           PIC X(24).
           05  WS-COUNT-TEXT           PIC X(24).
           05  WS-HASH-TEXT            PIC X(24).
           05  WS-EVENT-TYPE           PIC X(03).
           05  WS-SIDE-CODE            PIC X(02).
           05  WS-ACCT-CODE            PIC X(01).
           05  WS-CAP-CODE             PIC X(01).
           05  WS-DEPT-CODE            PIC X(01).
           05  WS-SESSION-CODE         PIC X(04).
           05  WS-HANDLING-CODE        PIC X(03).
           05  WS-SYMBOL               PIC X(08).
       01  WS-FILE-ID.
           05  WS-FI-FIRM              PIC X(07).
           05  FILLER                  PIC X(01)  VALUE '_'.
           05  WS-FI-DATE              PIC 9(08).
           05  WS-FI-KIND              PIC X(04).
           05  WS-FI-VERSION           PIC X(03).
       01  WS-LINE-PTR                 PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * CONTROL CARDS                                                  *
      *----------------------------------------------------------------*
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(20).
           05  WS-PARM-NUM             PIC 9(03).
       01  WS-LATE-DAYS                PIC S9(05) COMP-3 VALUE +1.
       01  WS-LATE-CUTOFF              PIC 9(08)  VALUE ZERO.
       01  WS-TEST-ACCOUNTS.
           05  WS-TA-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-TA-ACCT              PIC X(10)  OCCURS 20 TIMES
                                       INDEXED BY TA-IDX.
       01  WS-TA-MAX                   PIC S9(04) COMP  VALUE 20.
      *----------------------------------------------------------------*
      * LISTING SYMBOL CACHE (CUSIP -> SYMBOL)                         *
      *----------------------------------------------------------------*
       01  WS-SYMBOL-CACHE.
           05  WS-SC-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-SC-ENTRY             OCCURS 2000 TIMES
                                       INDEXED BY SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-SYMBOL        PIC X(08).
       01  WS-SC-MAX                   PIC S9(04) COMP  VALUE 2000.
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-PASS1-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PASS1-EVENTS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRADES-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FI-SKIPPED           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STATUS-SKIPPED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EVENTS-OUT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-OUT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CXL-OUT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COR-OUT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DST-EVENTS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EST-EVENTS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DATE-ROLLED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LATE-EVENTS          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-VERY-LATE            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTRY-TS-USED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTRY-TS-MISSING     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TEST-SKIPPED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-QTY-PRICE-REJECT     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PARM-CARDS           PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-PARM-ERRORS          PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-BAD-TIME             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-SYMBOL            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SECM-SYMBOL          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-SIDE         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINE-OVERFLOW        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMD010-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMU010-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LINES-OUT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-QTY-HASH             PIC S9(13)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-NET-HASH             PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
       COPY TCTRADE.
       COPY RRCATL.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMSECLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE.
      *    ---- PASS 1 - COUNT THE EVENTS FOR THE HEADER ------------
           MOVE 1 TO WS-PASS.
           PERFORM 8000-READ-TRADE.
           PERFORM UNTIL END-OF-TRADES
               ADD 1 TO WS-PASS1-READ
               PERFORM 2100-CHECK-ELIGIBLE
               IF EVENT-ELIGIBLE
                   ADD 1 TO WS-PASS1-EVENTS
               END-IF
               PERFORM 8000-READ-TRADE
           END-PERFORM.
           CLOSE TRADEIN-FILE.
      *    ---- PASS 2 - WRITE THE SUBMISSION -----------------------
           MOVE 2   TO WS-PASS.
           MOVE 'N' TO WS-EOF-SW.
           PERFORM 1200-OPEN-TRADES.
           PERFORM 3000-WRITE-HEADER.
           PERFORM 8000-READ-TRADE.
           PERFORM UNTIL END-OF-TRADES
               PERFORM 2000-PROCESS-TRADE
               PERFORM 8000-READ-TRADE
           END-PERFORM.
           PERFORM 3900-WRITE-TRAILER.
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
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
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
           MOVE 'EVENT REPORTING EXTRACT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-READ-PARMS.
           PERFORM 1150-LATE-CUTOFF.
           PERFORM 1200-OPEN-TRADES.
           OPEN OUTPUT CATOUT-FILE.
           IF WS-CATOUT-STATUS NOT = '00'
               MOVE 'CATOUT'           TO AB-DDNAME
               MOVE WS-CATOUT-STATUS   TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE WS-FIRM-ID      TO WS-FI-FIRM.
           MOVE DC-BUS-DATE     TO WS-FI-DATE.
           MOVE WS-FILE-KIND    TO WS-FI-KIND.
           MOVE WS-FILE-VERSION TO WS-FI-VERSION.
      *----------------------------------------------------------------*
      * CONTROL CARDS (SYSIN MAY BE DUMMY).  '*' IN COLUMN 1 = COMMENT *
      *   FILE-VERSION=NN  A CORRECTED FILE FOR A DAY ALREADY SENT     *
      *                    CARRIES THE NEXT VERSION (RUNBOOK RR-020)   *
      *   LATE-DAYS=NNN    AS-OF EVENTS OLDER THAN NNN CALENDAR DAYS   *
      *                    ARE LISTED FOR THE LATE-REPORTING LOG       *
      *   TEST-ACCT=X(10)  QA / TRAINING ACCOUNTS - NOT REPORTED       *
      *----------------------------------------------------------------*
       1100-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1100-READ-PARMS'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           PERFORM UNTIL END-OF-PARMS
               READ PARMCARD
               EVALUATE TRUE
                   WHEN PARMCARD-EOF
                       MOVE 'Y' TO WS-PARM-EOF-SW
                   WHEN PARMCARD-OK
                       PERFORM 1110-PARM-CARD
                   WHEN OTHER
                       MOVE 'SYSIN'            TO AB-DDNAME
                       MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
                       MOVE 1002               TO AB-ABEND-CODE
                       MOVE '1100-READ-PARMS'  TO AB-PARAGRAPH
                       MOVE 'READ FAILED'      TO AB-MESSAGE
                       PERFORM 9999-ABEND
               END-EVALUATE
           END-PERFORM.
           CLOSE PARMCARD.
      *----------------------------------------------------------------*
       1110-PARM-CARD.
      *----------------------------------------------------------------*
           IF PARM-CARD-REC (1:1) = '*' OR PARM-CARD-REC = SPACES
               EXIT PARAGRAPH
           END-IF.
           ADD 1 TO WS-PARM-CARDS.
           DISPLAY 'RRB200 CARD: ' PARM-CARD-REC (1:72).
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING.
           EVALUATE WS-PARM-KEYWORD
               WHEN 'FILE-VERSION'
                   IF WS-PARM-VALUE (1:2) NUMERIC
                   AND WS-PARM-VALUE (3:) = SPACES
                   AND WS-PARM-VALUE (1:2) NOT = '00'
                       MOVE '_'                 TO WS-FILE-VERSION (1:1)
                       MOVE WS-PARM-VALUE (1:2) TO WS-FILE-VERSION (2:2)
                   ELSE
                       PERFORM 1190-BAD-CARD
                   END-IF
               WHEN 'LATE-DAYS'
                   IF WS-PARM-VALUE (1:3) NUMERIC
                   AND WS-PARM-VALUE (4:) = SPACES
                       MOVE WS-PARM-VALUE (1:3) TO WS-PARM-NUM
                       MOVE WS-PARM-NUM         TO WS-LATE-DAYS
                   ELSE
                       PERFORM 1190-BAD-CARD
                   END-IF
               WHEN 'TEST-ACCT'
                   IF WS-PARM-VALUE = SPACES
                   OR WS-TA-USED NOT < WS-TA-MAX
                       PERFORM 1190-BAD-CARD
                   ELSE
                       ADD 1 TO WS-TA-USED
                       MOVE WS-PARM-VALUE (1:10)
                                        TO WS-TA-ACCT (WS-TA-USED)
                   END-IF
               WHEN OTHER
                   PERFORM 1190-BAD-CARD
           END-EVALUATE.
      *----------------------------------------------------------------*
       1150-LATE-CUTOFF.
      *----------------------------------------------------------------*
      *    EVENTS WITH A TRADE DATE BEFORE THE CUTOFF ARE LOGGED
           MOVE 'ADDC'        TO DT-FUNCTION.
           MOVE SPACES        TO DT-CALENDAR.
           MOVE DC-BUS-DATE   TO DT-DATE-1.
           COMPUTE DT-DAYS = WS-LATE-DAYS * -1.
           PERFORM 7000-CALL-CMU010.
           MOVE DT-RESULT-DATE TO WS-LATE-CUTOFF.
      *----------------------------------------------------------------*
       1190-BAD-CARD.
      *----------------------------------------------------------------*
           ADD 1 TO WS-PARM-ERRORS.
           DISPLAY 'RRB200 W - CONTROL CARD IGNORED: '
                   PARM-CARD-REC (1:40).
           PERFORM 7900-SET-WARNING.
      *----------------------------------------------------------------*
       1200-OPEN-TRADES.
      *----------------------------------------------------------------*
           OPEN INPUT TRADEIN-FILE.
           IF WS-TRADEIN-STATUS NOT = '00'
               MOVE 'TRADEIN'          TO AB-DDNAME
               MOVE WS-TRADEIN-STATUS  TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1200-OPEN-TRADES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
      * PASS 2 - ONE FINAL TRADE                                       *
      *================================================================*
       2000-PROCESS-TRADE.
           ADD 1 TO WS-TRADES-READ.
           PERFORM 2100-CHECK-ELIGIBLE.
           IF NOT EVENT-ELIGIBLE
               EXIT PARAGRAPH
           END-IF.
           PERFORM 2200-EVENT-TYPE.
           PERFORM 2300-MAP-CODES.
           PERFORM 2400-GET-SYMBOL.
           PERFORM 4000-CONVERT-TO-UTC.
           PERFORM 2500-FORMAT-NUMBERS.
           PERFORM 3100-WRITE-EVENT.
      *----------------------------------------------------------------*
      * ELIGIBILITY - THE SAME TEST IN BOTH PASSES (HEADER COUNT MUST  *
      * EQUAL THE NUMBER OF E LINES)                                   *
      *----------------------------------------------------------------*
       2100-CHECK-ELIGIBLE.
           MOVE 'Y' TO WS-ELIGIBLE-SW.
           MOVE 'N' TO WS-REJECT-SW.
           IF WS-TA-USED > ZERO
               SET TA-IDX TO 1
               SEARCH WS-TA-ACCT
                   AT END
                       CONTINUE
                   WHEN WS-TA-ACCT (TA-IDX) = TRD-ACCT-NO
                       MOVE 'N' TO WS-ELIGIBLE-SW
                       IF WRITING-PASS
                           ADD 1 TO WS-TEST-SKIPPED
                       END-IF
               END-SEARCH
               IF NOT EVENT-ELIGIBLE
                   EXIT PARAGRAPH
               END-IF
           END-IF.
           EVALUATE TRUE
               WHEN TRD-SEC-TYPE = 'CB' OR 'MU' OR 'GV'
                   MOVE 'N' TO WS-ELIGIBLE-SW
                   IF WRITING-PASS
                       ADD 1 TO WS-FI-SKIPPED
                   END-IF
               WHEN NOT TRD-ST-FINAL
                   MOVE 'N' TO WS-ELIGIBLE-SW
                   IF WRITING-PASS
                       ADD 1 TO WS-STATUS-SKIPPED
                   END-IF
      *        A ZERO QUANTITY OR PRICE IS REJECTED BY THE REPORTING
      *        SYSTEM AND COUNTS AGAINST THE FIRM'S ERROR RATE
               WHEN TRD-QTY NOT > ZERO
               WHEN TRD-PRICE NOT > ZERO
                   MOVE 'N' TO WS-ELIGIBLE-SW
                   MOVE 'Y' TO WS-REJECT-SW
                   IF WRITING-PASS
                       ADD 1 TO WS-QTY-PRICE-REJECT
                       DISPLAY 'RRB200 W - QTY/PRICE NOT > 0, NOT '
                               'REPORTED - TRADE ' TRD-ID
                       PERFORM 7900-SET-WARNING
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      *----------------------------------------------------------------*
       2200-EVENT-TYPE.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN TRD-CANCEL
                   MOVE 'CXL' TO WS-EVENT-TYPE
                   ADD 1 TO WS-CXL-OUT
               WHEN TRD-CORRECT
                   MOVE 'COR' TO WS-EVENT-TYPE
                   ADD 1 TO WS-COR-OUT
               WHEN OTHER
                   MOVE 'NEW' TO WS-EVENT-TYPE
                   ADD 1 TO WS-NEW-OUT
           END-EVALUATE.
      *----------------------------------------------------------------*
      * CODE MAPPINGS (SUBMISSION TECHNICAL SPEC V4.1 SECTION 4)       *
      *----------------------------------------------------------------*
       2300-MAP-CODES.
      *    ---- SIDE ------------------------------------------------
           EVALUATE TRD-SIDE
               WHEN 'B '
                   MOVE 'B'  TO WS-SIDE-CODE
               WHEN 'S '
                   MOVE 'SL' TO WS-SIDE-CODE
               WHEN 'SS'
                   MOVE 'SS' TO WS-SIDE-CODE
               WHEN 'BC'
                   MOVE 'B'  TO WS-SIDE-CODE
               WHEN OTHER
                   MOVE SPACES TO WS-SIDE-CODE
                   ADD 1 TO WS-UNKNOWN-SIDE
                   DISPLAY 'RRB200 W - UNKNOWN SIDE ' TRD-SIDE
                           ' TRADE ' TRD-ID
                   PERFORM 7900-SET-WARNING
           END-EVALUATE.
      *    ---- ACCOUNT HOLDER TYPE -----------------------------------
           EVALUATE TRD-ACCT-TYPE
               WHEN 'IN'
               WHEN 'JT'
                   MOVE 'I' TO WS-ACCT-CODE
               WHEN 'IS'
               WHEN 'OM'
                   MOVE 'A' TO WS-ACCT-CODE
               WHEN 'FI'
                   MOVE 'P' TO WS-ACCT-CODE
               WHEN 'ST'
                   MOVE 'O' TO WS-ACCT-CODE
               WHEN OTHER
                   MOVE 'X' TO WS-ACCT-CODE
           END-EVALUATE.
      *    ---- CAPACITY / DEPARTMENT --------------------------------
           EVALUATE TRUE
               WHEN TRD-PRINCIPAL-CAP
                   MOVE 'P' TO WS-CAP-CODE
                   MOVE 'T' TO WS-DEPT-CODE
               WHEN TRD-RISKLESS-PRIN
                   MOVE 'R' TO WS-CAP-CODE
                   MOVE 'T' TO WS-DEPT-CODE
               WHEN OTHER
                   MOVE 'A' TO WS-CAP-CODE
                   MOVE 'A' TO WS-DEPT-CODE
           END-EVALUATE.
      *    ---- HANDLING --------------------------------------------
           IF TRD-SRC-MANUAL
               MOVE 'MAN' TO WS-HANDLING-CODE
           ELSE
               MOVE 'NH'  TO WS-HANDLING-CODE
           END-IF.
      *    ---- SESSION FROM THE LOCAL (NEW YORK) TIME ---------------
           EVALUATE TRUE
               WHEN TRD-TRADE-TIME < WS-OPEN-TIME
                   MOVE 'PRE'  TO WS-SESSION-CODE
               WHEN TRD-TRADE-TIME > WS-CLOSE-TIME
                   MOVE 'POST' TO WS-SESSION-CODE
               WHEN OTHER
                   MOVE 'REG'  TO WS-SESSION-CODE
           END-EVALUATE.
      *----------------------------------------------------------------*
      * LISTING SYMBOL FROM THE SECURITY MASTER (CHG37120).  THE OMS   *
      * SYMBOL IS KEPT WHEN THE CUSIP IS NOT ON THE MASTER.            *
      *----------------------------------------------------------------*
       2400-GET-SYMBOL.
           MOVE SPACES TO WS-SYMBOL.
           SET SC-IDX TO 1.
           SEARCH WS-SC-ENTRY
               AT END
                   PERFORM 2410-LOOKUP-SECURITY
               WHEN SC-IDX > WS-SC-USED
                   PERFORM 2410-LOOKUP-SECURITY
               WHEN WS-SC-CUSIP (SC-IDX) = TRD-CUSIP
                   MOVE WS-SC-SYMBOL (SC-IDX) TO WS-SYMBOL
           END-SEARCH.
           IF WS-SYMBOL = SPACES
               MOVE TRD-SYMBOL TO WS-SYMBOL
           ELSE
               ADD 1 TO WS-SECM-SYMBOL
           END-IF.
           IF WS-SYMBOL = SPACES
               ADD 1 TO WS-NO-SYMBOL
               DISPLAY 'RRB200 W - NO SYMBOL FOR ' TRD-CUSIP
                       ' TRADE ' TRD-ID
               PERFORM 7900-SET-WARNING
           END-IF.
      *----------------------------------------------------------------*
       2410-LOOKUP-SECURITY.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CMD010-CALLS.
           MOVE 'GET '    TO SL-FUNCTION.
           MOVE TRD-CUSIP TO SL-KEY-CUSIP.
           MOVE SPACES    TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA TO SEC-MASTER-REC
                   MOVE SEC-SYMBOL  TO WS-SYMBOL
               WHEN SL-NOT-FOUND
                   MOVE SPACES      TO WS-SYMBOL
               WHEN OTHER
                   MOVE SL-SQLCODE  TO AB-SQLCODE
                   MOVE 1003        TO AB-ABEND-CODE
                   MOVE '2410-LOOKUP-SECURITY' TO AB-PARAGRAPH
                   MOVE TRD-CUSIP   TO AB-KEY
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
           IF WS-SC-USED < WS-SC-MAX
               ADD 1 TO WS-SC-USED
               MOVE TRD-CUSIP TO WS-SC-CUSIP (WS-SC-USED)
               MOVE WS-SYMBOL TO WS-SC-SYMBOL (WS-SC-USED)
           END-IF.
      *----------------------------------------------------------------*
       2500-FORMAT-NUMBERS.
      *----------------------------------------------------------------*
           COMPUTE WS-FMT-IN = WS-EVENTS-OUT + 1.
           PERFORM 7100-FORMAT-NUMBER.
           MOVE WS-FMT-OUT TO WS-SEQ-TEXT.
           MOVE TRD-QTY    TO WS-FMT-IN.
           PERFORM 7100-FORMAT-NUMBER.
           MOVE WS-FMT-OUT TO WS-QTY-TEXT.
           MOVE TRD-PRICE  TO WS-FMT-IN.
           PERFORM 7100-FORMAT-NUMBER.
           MOVE WS-FMT-OUT TO WS-PRICE-TEXT.
      *================================================================*
      * SUBMISSION LINES                                               *
      *================================================================*
       3000-WRITE-HEADER.
           MOVE WS-PASS1-EVENTS TO WS-FMT-IN.
           PERFORM 7100-FORMAT-NUMBER.
           MOVE WS-FMT-OUT TO WS-COUNT-TEXT.
           MOVE SPACES TO RRC-SUBMISSION-LINE.
           MOVE 1      TO WS-LINE-PTR.
           STRING 'H|'             DELIMITED BY SIZE
                  WS-FILE-ID       DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-FIRM-ID       DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  DC-NEXT-BUS-DATE DELIMITED BY SIZE
                  '|'              DELIMITED BY SIZE
                  WS-COUNT-TEXT    DELIMITED BY SPACE
               INTO RRC-SUBMISSION-LINE
               WITH POINTER WS-LINE-PTR
           END-STRING.
           PERFORM 8100-WRITE-LINE.
      *----------------------------------------------------------------*
      * E LINE.  TEXT FIELDS END AT THE FIRST DOUBLE SPACE - THE UTC   *
      * TIMESTAMP CARRIES ONE EMBEDDED SPACE BETWEEN DATE AND TIME.    *
      *----------------------------------------------------------------*
       3100-WRITE-EVENT.
           MOVE SPACES TO RRC-SUBMISSION-LINE.
           MOVE 1      TO WS-LINE-PTR.
           STRING 'E|'             DELIMITED BY SIZE
                  WS-SEQ-TEXT      DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-EVENT-TYPE    DELIMITED BY SIZE
                  '|'              DELIMITED BY SIZE
                  TRD-ID           DELIMITED BY '  '
                  '|'              DELIMITED BY SIZE
                  WS-UTC-TEXT      DELIMITED BY '  '
                  '|'              DELIMITED BY SIZE
                  WS-SYMBOL        DELIMITED BY '  '
                  '|'              DELIMITED BY SIZE
                  WS-SIDE-CODE     DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-QTY-TEXT      DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-PRICE-TEXT    DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-ACCT-CODE     DELIMITED BY SIZE
                  '|'              DELIMITED BY SIZE
                  WS-CAP-CODE      DELIMITED BY SIZE
                  '|'              DELIMITED BY SIZE
                  TRD-DESK         DELIMITED BY '  '
                  '|'              DELIMITED BY SIZE
                  WS-DEPT-CODE     DELIMITED BY SIZE
                  '|'              DELIMITED BY SIZE
                  WS-SESSION-CODE  DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-HANDLING-CODE DELIMITED BY SPACE
               INTO RRC-SUBMISSION-LINE
               WITH POINTER WS-LINE-PTR
               ON OVERFLOW
                   ADD 1 TO WS-LINE-OVERFLOW
                   DISPLAY 'RRB200 W - LINE OVERFLOW TRADE ' TRD-ID
                   PERFORM 7900-SET-WARNING
           END-STRING.
           PERFORM 8100-WRITE-LINE.
           ADD 1              TO WS-EVENTS-OUT.
           ADD TRD-QTY        TO WS-QTY-HASH.
           ADD TRD-NET-AMOUNT TO WS-NET-HASH.
      *----------------------------------------------------------------*
       3900-WRITE-TRAILER.
      *----------------------------------------------------------------*
           IF WS-EVENTS-OUT NOT = WS-PASS1-EVENTS
               MOVE 'CATOUT'      TO AB-DDNAME
               MOVE 1004          TO AB-ABEND-CODE
               MOVE '3900-WRITE-TRAILER' TO AB-PARAGRAPH
               MOVE 'EVENTS WRITTEN NOT = HEADER COUNT' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE WS-EVENTS-OUT TO WS-FMT-IN.
           PERFORM 7100-FORMAT-NUMBER.
           MOVE WS-FMT-OUT    TO WS-COUNT-TEXT.
           MOVE WS-QTY-HASH   TO WS-FMT-IN.
           PERFORM 7100-FORMAT-NUMBER.
           MOVE WS-FMT-OUT    TO WS-HASH-TEXT.
           MOVE SPACES TO RRC-SUBMISSION-LINE.
           MOVE 1      TO WS-LINE-PTR.
           STRING 'T|'             DELIMITED BY SIZE
                  WS-COUNT-TEXT    DELIMITED BY SPACE
                  '|'              DELIMITED BY SIZE
                  WS-HASH-TEXT     DELIMITED BY SPACE
               INTO RRC-SUBMISSION-LINE
               WITH POINTER WS-LINE-PTR
           END-STRING.
           PERFORM 8100-WRITE-LINE.
      *================================================================*
      * NEW YORK LOCAL TIME -> UTC                                     *
      *================================================================*
       4000-CONVERT-TO-UTC.
           MOVE TRD-TRADE-DATE TO WS-LOCAL-DATE.
           MOVE TRD-TRADE-TIME TO WS-LOCAL-TIME.
           IF TRD-TRADE-TIME NOT NUMERIC
           OR WS-LOCAL-HH > 23 OR WS-LOCAL-MI > 59 OR WS-LOCAL-SS > 59
               ADD 1 TO WS-BAD-TIME
               DISPLAY 'RRB200 W - INVALID TRADE TIME ' TRD-TRADE-TIME
                       ' TRADE ' TRD-ID ' - 00:00:00 USED'
               MOVE ZERO TO WS-LOCAL-TIME
               PERFORM 7900-SET-WARNING
           END-IF.
           IF WS-LOCAL-DATE < DC-BUS-DATE
               ADD 1 TO WS-LATE-EVENTS
               IF WS-LOCAL-DATE < WS-LATE-CUTOFF
                   ADD 1 TO WS-VERY-LATE
                   DISPLAY 'RRB200 W - LATE EVENT ' TRD-ID
                           ' TRADE DATE ' WS-LOCAL-DATE
                           ' TYPE ' WS-EVENT-TYPE
                   PERFORM 7900-SET-WARNING
               END-IF
           END-IF.
           MOVE WS-TS-FRACTION TO WS-EVENT-FRACTION.
           IF WS-EVENT-TYPE = 'CXL' OR WS-EVENT-TYPE = 'COR'
               PERFORM 4050-CXL-EVENT-TIME
           END-IF.
           PERFORM 4100-GET-DST-WINDOW.
      *    ---- IN DAYLIGHT TIME?  BOTH CHANGES AT 02:00 LOCAL -------
           MOVE 'N' TO WS-DST-SW.
           IF (WS-LOCAL-DATE > WS-DW-START
               OR (WS-LOCAL-DATE = WS-DW-START
                   AND WS-LOCAL-TIME NOT < WS-DST-CHANGE-TIME))
           AND (WS-LOCAL-DATE < WS-DW-END
               OR (WS-LOCAL-DATE = WS-DW-END
                   AND WS-LOCAL-TIME < WS-DST-CHANGE-TIME))
               MOVE 'Y' TO WS-DST-SW
           END-IF.
           MOVE WS-LOCAL-DATE TO WS-UTC-DATE.
           MOVE WS-LOCAL-MI   TO WS-UTC-MI.
           MOVE WS-LOCAL-SS   TO WS-UTC-SS.
           IF DST-IN-EFFECT
               ADD 1 TO WS-DST-EVENTS
               COMPUTE WS-UTC-HH-WORK = WS-LOCAL-HH + WS-EDT-OFFSET-HH
           ELSE
               ADD 1 TO WS-EST-EVENTS
               COMPUTE WS-UTC-HH-WORK = WS-LOCAL-HH + WS-EST-OFFSET-HH
           END-IF.
           IF WS-UTC-HH-WORK > 23
               SUBTRACT 24 FROM WS-UTC-HH-WORK
               ADD 1 TO WS-DATE-ROLLED
               MOVE 'ADDC'        TO DT-FUNCTION
               MOVE SPACES        TO DT-CALENDAR
               MOVE WS-LOCAL-DATE TO DT-DATE-1
               MOVE 1             TO DT-DAYS
               PERFORM 7000-CALL-CMU010
               MOVE DT-RESULT-DATE TO WS-UTC-DATE
           END-IF.
           MOVE WS-UTC-HH-WORK TO WS-UTC-HH.
           MOVE WS-UTC-DATE    TO WS-UT-DATE.
           MOVE WS-UTC-TIME    TO WS-UT-TIME.
           MOVE WS-EVENT-FRACTION TO WS-UT-FRACTION.
      *----------------------------------------------------------------*
      * A CANCEL OR CORRECTION IS REPORTED AT THE TIME IT WAS ENTERED  *
      * (CHG35388), NOT THE TIME OF THE ORIGINAL TRADE.  THE ENTRY     *
      * TIME STAMP IS NEW YORK LOCAL TIME FROM CMASM02                 *
      * (CCYY-MM-DD-HH.MM.SS.NNNNNN), MILLISECONDS ARE KEPT.  NO VALID *
      * STAMP - THE TRADE DATE AND TIME ARE USED.                      *
      *----------------------------------------------------------------*
       4050-CXL-EVENT-TIME.
           MOVE TRD-ENTRY-TS TO WS-ENTRY-TS.
           IF WS-ETS-CCYY NOT NUMERIC OR WS-ETS-MM NOT NUMERIC
           OR WS-ETS-DD NOT NUMERIC OR WS-ETS-HH NOT NUMERIC
           OR WS-ETS-MI NOT NUMERIC OR WS-ETS-SS NOT NUMERIC
           OR WS-ETS-DASH-1 NOT = '-' OR WS-ETS-DASH-3 NOT = '-'
           OR WS-ETS-HH > 23 OR WS-ETS-MI > 59 OR WS-ETS-SS > 59
               ADD 1 TO WS-ENTRY-TS-MISSING
               EXIT PARAGRAPH
           END-IF.
           ADD 1 TO WS-ENTRY-TS-USED.
           MOVE WS-ETS-CCYY TO WS-LOCAL-CCYY.
           MOVE WS-ETS-MM   TO WS-LOCAL-MM.
           MOVE WS-ETS-DD   TO WS-LOCAL-DD.
           MOVE WS-ETS-HH   TO WS-LOCAL-HH.
           MOVE WS-ETS-MI   TO WS-LOCAL-MI.
           MOVE WS-ETS-SS   TO WS-LOCAL-SS.
           IF WS-ETS-MILLI NUMERIC
               MOVE '.'          TO WS-EVENT-FRACTION (1:1)
               MOVE WS-ETS-MILLI TO WS-EVENT-FRACTION (2:3)
           END-IF.
      *----------------------------------------------------------------*
      * DST START / END DATES FOR THE YEAR OF THE EVENT (CACHED)       *
      *----------------------------------------------------------------*
       4100-GET-DST-WINDOW.
           MOVE WS-LOCAL-CCYY TO WS-DW-YEAR.
           SET DST-IDX TO 1.
           SEARCH WS-DST-ENTRY
               AT END
                   PERFORM 4200-COMPUTE-DST-WINDOW
               WHEN DST-IDX > WS-DST-USED
                   PERFORM 4200-COMPUTE-DST-WINDOW
               WHEN WS-DST-YEAR (DST-IDX) = WS-DW-YEAR
                   MOVE WS-DST-START (DST-IDX) TO WS-DW-START
                   MOVE WS-DST-END (DST-IDX)   TO WS-DW-END
           END-SEARCH.
      *----------------------------------------------------------------*
      * DAY OF WEEK FROM CMU010: 1 = MONDAY ... 7 = SUNDAY.            *
      *   FIRST SUNDAY ON OR AFTER DAY 1 = 1 + MOD(7 - DOW, 7)         *
      *   LAST SUNDAY ON OR BEFORE DAY 31 = 31 - MOD(DOW, 7)           *
      *----------------------------------------------------------------*
       4200-COMPUTE-DST-WINDOW.
           MOVE WS-DW-YEAR TO WS-DW-CCYY.
           IF WS-DW-YEAR NOT < WS-NEW-RULE-YEAR
      *        ---- SECOND SUNDAY IN MARCH --------------------------
               MOVE WS-NEW-START-MMDD TO WS-DW-MMDD
               PERFORM 4300-GET-DOW
               COMPUTE WS-DW-DAY = 1 + FUNCTION MOD (7 - WS-DW-DOW, 7)
                                 + (WS-NEW-START-WEEK - 1) * 7
               COMPUTE WS-DW-START = WS-DW-DATE + WS-DW-DAY - 1
      *        ---- FIRST SUNDAY IN NOVEMBER ------------------------
               MOVE WS-NEW-END-MMDD TO WS-DW-MMDD
               PERFORM 4300-GET-DOW
               COMPUTE WS-DW-DAY = 1 + FUNCTION MOD (7 - WS-DW-DOW, 7)
               COMPUTE WS-DW-END = WS-DW-DATE + WS-DW-DAY - 1
           ELSE
      *        ---- FIRST SUNDAY IN APRIL ---------------------------
               MOVE WS-OLD-START-MMDD TO WS-DW-MMDD
               PERFORM 4300-GET-DOW
               COMPUTE WS-DW-DAY = 1 + FUNCTION MOD (7 - WS-DW-DOW, 7)
               COMPUTE WS-DW-START = WS-DW-DATE + WS-DW-DAY - 1
      *        ---- LAST SUNDAY IN OCTOBER --------------------------
               MOVE WS-OLD-END-MMDD TO WS-DW-MMDD
               PERFORM 4300-GET-DOW
               COMPUTE WS-DW-ADJ = FUNCTION MOD (WS-DW-DOW, 7)
               COMPUTE WS-DW-END = WS-DW-DATE - WS-DW-ADJ
           END-IF.
           IF WS-DST-USED < WS-DST-MAX
               ADD 1 TO WS-DST-USED
               MOVE WS-DW-YEAR  TO WS-DST-YEAR (WS-DST-USED)
               MOVE WS-DW-START TO WS-DST-START (WS-DST-USED)
               MOVE WS-DW-END   TO WS-DST-END (WS-DST-USED)
           END-IF.
           DISPLAY 'RRB200 DST ' WS-DW-YEAR ' FROM ' WS-DW-START
                   ' TO ' WS-DW-END.
      *----------------------------------------------------------------*
       4300-GET-DOW.
      *----------------------------------------------------------------*
           MOVE 'DOW '     TO DT-FUNCTION.
           MOVE SPACES     TO DT-CALENDAR.
           MOVE WS-DW-DATE TO DT-DATE-1.
           PERFORM 7000-CALL-CMU010.
           MOVE DT-RESULT-NUM TO WS-DW-DOW.
      *================================================================*
      * SERVICES                                                       *
      *================================================================*
       7000-CALL-CMU010.
           ADD 1 TO WS-CMU010-CALLS.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 1010            TO AB-ABEND-CODE
               MOVE '7000-CALL-CMU010' TO AB-PARAGRAPH
               MOVE DT-DATE-1       TO AB-KEY
               MOVE DT-MESSAGE      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
      * WS-FMT-IN -> WS-FMT-OUT (LEFT JUSTIFIED, NO LEADING ZEROS,     *
      * TRAILING DECIMAL ZEROS AND A BARE DECIMAL POINT REMOVED)       *
      *----------------------------------------------------------------*
       7100-FORMAT-NUMBER.
           MOVE WS-FMT-IN TO WS-FMT-EDIT.
           MOVE ZERO      TO WS-FMT-LEAD.
           INSPECT WS-FMT-EDIT TALLYING WS-FMT-LEAD
               FOR LEADING SPACES.
           MOVE LENGTH OF WS-FMT-EDIT TO WS-FMT-END.
           PERFORM UNTIL WS-FMT-END NOT > WS-FMT-LEAD
                      OR WS-FMT-EDIT (WS-FMT-END:1) NOT = '0'
               SUBTRACT 1 FROM WS-FMT-END
           END-PERFORM.
           IF WS-FMT-EDIT (WS-FMT-END:1) = '.'
               SUBTRACT 1 FROM WS-FMT-END
           END-IF.
           COMPUTE WS-FMT-LEN = WS-FMT-END - WS-FMT-LEAD.
           MOVE SPACES TO WS-FMT-OUT.
           MOVE WS-FMT-EDIT (WS-FMT-LEAD + 1:WS-FMT-LEN)
                           TO WS-FMT-OUT.
      *----------------------------------------------------------------*
       7900-SET-WARNING.
      *----------------------------------------------------------------*
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-TRADE.
           READ TRADEIN-FILE INTO TRD-TRADE-REC.
           EVALUATE TRUE
               WHEN TRADEIN-OK
                   IF TRD-QTY NOT NUMERIC
                       MOVE ZERO TO TRD-QTY
                   END-IF
                   IF TRD-PRICE NOT NUMERIC
                       MOVE ZERO TO TRD-PRICE
                   END-IF
                   IF TRD-NET-AMOUNT NOT NUMERIC
                       MOVE ZERO TO TRD-NET-AMOUNT
                   END-IF
               WHEN TRADEIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'TRADEIN'         TO AB-DDNAME
                   MOVE WS-TRADEIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002              TO AB-ABEND-CODE
                   MOVE '8000-READ-TRADE' TO AB-PARAGRAPH
                   MOVE 'READ FAILED'     TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8100-WRITE-LINE.
      *----------------------------------------------------------------*
           WRITE CATOUT-REC FROM RRC-SUBMISSION-LINE.
           IF WS-CATOUT-STATUS NOT = '00'
               MOVE 'CATOUT'          TO AB-DDNAME
               MOVE WS-CATOUT-STATUS  TO AB-FILE-STATUS
               MOVE 1002              TO AB-ABEND-CODE
               MOVE '8100-WRITE-LINE' TO AB-PARAGRAPH
               MOVE RRC-LINE-TEXT (1:40) TO AB-KEY
               MOVE 'WRITE FAILED'    TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           ADD 1 TO WS-LINES-OUT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RRB200'       TO CT-STAGE.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010              TO AB-ABEND-CODE
               MOVE '8500-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME   TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE TRADEIN-FILE.
           IF WS-VERY-LATE > ZERO
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'LATEEVT'      TO AU-EVENT
               MOVE 'W'            TO AU-SEVERITY
               MOVE WS-FILE-ID     TO AU-KEY
               MOVE 'AS-OF EVENTS BEYOND LATE-DAYS - LOG FOR COMPLIANCE'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF.
           CLOSE CATOUT-FILE.
           IF WS-CATOUT-STATUS NOT = '00'
               MOVE 'CATOUT'          TO AB-DDNAME
               MOVE WS-CATOUT-STATUS  TO AB-FILE-STATUS
               MOVE 1002              TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'  TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED'    TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'TRADES-IN'      TO CT-COUNTER-NAME.
           MOVE WS-TRADES-READ   TO CT-COUNT.
           MOVE WS-NET-HASH      TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'EVENTS-OUT'     TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-OUT    TO CT-COUNT.
           MOVE WS-NET-HASH      TO CT-AMOUNT.
           MOVE WS-QTY-HASH      TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           DISPLAY '************************************************'.
           DISPLAY '* RRB200 - EVENT REPORTING EXTRACT             *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' FILE ID                  : ' WS-FILE-ID.
           MOVE WS-PARM-CARDS TO WS-DISP-CNT.
           DISPLAY ' CONTROL CARDS            : ' WS-DISP-CNT.
           MOVE WS-PARM-ERRORS TO WS-DISP-CNT.
           DISPLAY '   IGNORED                : ' WS-DISP-CNT.
           MOVE WS-PASS1-READ TO WS-DISP-CNT.
           DISPLAY ' PASS 1 TRADES READ       : ' WS-DISP-CNT.
           MOVE WS-PASS1-EVENTS TO WS-DISP-CNT.
           DISPLAY ' PASS 1 EVENTS COUNTED    : ' WS-DISP-CNT.
           MOVE WS-TRADES-READ TO WS-DISP-CNT.
           DISPLAY ' PASS 2 TRADES READ       : ' WS-DISP-CNT.
           MOVE WS-FI-SKIPPED TO WS-DISP-CNT.
           DISPLAY '   FIXED INCOME SKIPPED   : ' WS-DISP-CNT.
           MOVE WS-STATUS-SKIPPED TO WS-DISP-CNT.
           DISPLAY '   NOT FINAL - SKIPPED    : ' WS-DISP-CNT.
           MOVE WS-TEST-SKIPPED TO WS-DISP-CNT.
           DISPLAY '   TEST ACCOUNTS SKIPPED  : ' WS-DISP-CNT.
           MOVE WS-QTY-PRICE-REJECT TO WS-DISP-CNT.
           DISPLAY '   QTY/PRICE NOT > ZERO   : ' WS-DISP-CNT.
           MOVE WS-EVENTS-OUT TO WS-DISP-CNT.
           DISPLAY ' EVENTS WRITTEN           : ' WS-DISP-CNT.
           MOVE WS-NEW-OUT TO WS-DISP-CNT.
           DISPLAY '   NEW                    : ' WS-DISP-CNT.
           MOVE WS-CXL-OUT TO WS-DISP-CNT.
           DISPLAY '   CXL                    : ' WS-DISP-CNT.
           MOVE WS-COR-OUT TO WS-DISP-CNT.
           DISPLAY '   COR                    : ' WS-DISP-CNT.
           MOVE WS-DST-EVENTS TO WS-DISP-CNT.
           DISPLAY ' EVENTS IN DAYLIGHT TIME  : ' WS-DISP-CNT.
           MOVE WS-EST-EVENTS TO WS-DISP-CNT.
           DISPLAY ' EVENTS IN STANDARD TIME  : ' WS-DISP-CNT.
           MOVE WS-ENTRY-TS-USED TO WS-DISP-CNT.
           DISPLAY ' CXL/COR AT ENTRY TIME    : ' WS-DISP-CNT.
           MOVE WS-ENTRY-TS-MISSING TO WS-DISP-CNT.
           DISPLAY ' CXL/COR NO ENTRY STAMP   : ' WS-DISP-CNT.
           MOVE WS-DATE-ROLLED TO WS-DISP-CNT.
           DISPLAY ' UTC DATE ROLLED FORWARD  : ' WS-DISP-CNT.
           MOVE WS-LATE-EVENTS TO WS-DISP-CNT.
           DISPLAY ' LATE (AS-OF) EVENTS      : ' WS-DISP-CNT.
           MOVE WS-VERY-LATE TO WS-DISP-CNT.
           DISPLAY '   BEFORE LATE CUTOFF     : ' WS-DISP-CNT
                   ' (' WS-LATE-CUTOFF ')'.
           MOVE WS-BAD-TIME TO WS-DISP-CNT.
           DISPLAY ' INVALID TRADE TIMES      : ' WS-DISP-CNT.
           MOVE WS-SECM-SYMBOL TO WS-DISP-CNT.
           DISPLAY ' SYMBOL FROM SEC MASTER   : ' WS-DISP-CNT.
           MOVE WS-NO-SYMBOL TO WS-DISP-CNT.
           DISPLAY ' NO SYMBOL                : ' WS-DISP-CNT.
           MOVE WS-UNKNOWN-SIDE TO WS-DISP-CNT.
           DISPLAY ' UNKNOWN SIDE             : ' WS-DISP-CNT.
           MOVE WS-LINE-OVERFLOW TO WS-DISP-CNT.
           DISPLAY ' LINE OVERFLOW            : ' WS-DISP-CNT.
           MOVE WS-CMD010-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMD010 CALLS             : ' WS-DISP-CNT.
           MOVE WS-CMU010-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMU010 CALLS             : ' WS-DISP-CNT.
           MOVE WS-LINES-OUT TO WS-DISP-CNT.
           DISPLAY ' SUBMISSION LINES WRITTEN : ' WS-DISP-CNT.
           DISPLAY ' QTY HASH                 : ' WS-HASH-TEXT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE WS-FILE-ID     TO AU-KEY.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'EVENT REPORTING EXTRACT ENDED WITH WARNINGS'
                                   TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'EVENT REPORTING EXTRACT ENDED' TO AU-MESSAGE
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RRB200 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RRB200 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RRB200 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
