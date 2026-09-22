      *================================================================*
      * PROGRAM    : CMB090                                            *
      * TITLE      : CLOSE BUSINESS DAY - CONTROL TOTAL RECONCILIATION *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      * JOB        : MSCMD090  (LAST JOB OF THE EOD CYCLE)             *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   READS EVERY CONTROL TOTAL POSTED TODAY BY THE PROGRAMS OF    *
      *   THE CYCLE (CTLTOTS GDG, WRITTEN THROUGH CMU080), PRINTS THEM *
      *   GROUPED BY JOB AND PROGRAM, AND RECONCILES THE HAND-OFF      *
      *   POINTS BETWEEN SUBSYSTEMS:                                   *
      *     1  TCB300 TRADES-OUT    = TCB500 TRADES-IN                 *
      *     2  TCB500 SRACTV-OUT    = SRB100 TC-ACTV-IN                *
      *     3  CAB400 SRACTV-OUT    = SRB100 CA-ACTV-IN                *
      *   (THE LATEST POSTING OF A STAGE/COUNTER WINS - A RERUN        *
      *   REPLACES THE EARLIER FIGURES.)  RECORD COUNTS MUST AGREE;    *
      *   AMOUNT / QUANTITY HASH DIFFERENCES ARE FLAGGED FOR REVIEW    *
      *   ONLY, BECAUSE THE TWO SIDES MAY HASH DIFFERENT FIELDS.       *
      *   ALSO SUMMARISES TODAY'S AUDIT LOG (WARNINGS / ERRORS /       *
      *   ABENDS BY PROGRAM).                                          *
      *                                                                *
      * FILES:                                                         *
      *   DATECARD  INPUT   MSEC.PROD.CM.DATECARD(0)       FB  80      *
      *   CTLTOTS   INPUT   MSEC.PROD.CM.CTLTOTS(0)        FB 120      *
      *   AUDITLOG  INPUT   MSEC.PROD.CM.AUDIT.LOG         FB 200      *
      *   RPTFILE   OUTPUT  CMR090 CONTROL REPORT          FBA 133     *
      *   (CTLTOTS AND AUDITLOG ARE CLOSED BEFORE THIS PROGRAM POSTS   *
      *   ITS OWN TOTALS / AUDIT EVENTS THROUGH CMU080 / CMU060)       *
      * CALLS      : CMU050 CMU060 CMU080 CMASM02                      *
      * RETURN CODE: 0 ALL IN BALANCE                                  *
      *              4 ANY RULE OUT OF BALANCE OR NOT POSTED, OR NO    *
      *                TOTALS FOR THE BUSINESS DATE                    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1990-07-23 RJK            ORIGINAL - PRINT RRDS CONTROL TOTALS *
      * 1994-09-12 DWB  CHG01288  TC TO SR RECONCILIATION              *
      * 1998-11-02 TLM  CHG04471  Y2K - BUS DATE CCYYMMDD              *
      * 2001-04-09 DWB  CHG08130  DECIMALIZATION - QTY HASH 4 DEC      *
      * 2006-02-27 KAP  CHG14660  CA TO SR RECONCILIATION              *
      * 2009-02-16 SPA  CHG18810  REWRITE FOR CTLTOTS GDG - SORT BY    *
      *                           JOB/PROGRAM, RULE TABLE              *
      * 2013-03-04 JMF  CHG25016  AUDIT LOG EXCEPTION SUMMARY          *
      * 2016-03-14 SPA  CHG29980  RC 4 INSTEAD OF U1004 ON IMBALANCE - *
      *                           BACKUPS MUST STILL RUN               *
      * 2024-06-03 NVR  CHG59102  TCB300 -> TCB500 TRADE HAND-OFF RULE *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMB090.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  07/23/90.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATE-CARD-FILE ASSIGN TO DATECARD
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-DATECARD-STATUS.
           SELECT OPTIONAL CTLTOT-FILE ASSIGN TO CTLTOTS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CTLTOT-STATUS.
           SELECT OPTIONAL AUDIT-FILE ASSIGN TO AUDITLOG
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.
           SELECT REPORT-FILE ASSIGN TO RPTFILE
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.
           SELECT SORT-FILE ASSIGN TO SORTWK01.
       DATA DIVISION.
       FILE SECTION.
       FD  DATE-CARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  DATE-CARD-REC               PIC X(80).
       FD  CTLTOT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
           COPY CMCTLTOT.
       FD  AUDIT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
           COPY CMAUDIT.
       FD  REPORT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  REPORT-REC                  PIC X(133).
       SD  SORT-FILE.
       01  SORT-REC.
           05  SRT-JOBNAME             PIC X(08).
           05  SRT-PROGRAM             PIC X(08).
           05  SRT-TIMESTAMP           PIC X(26).
           05  SRT-SEQ                 PIC 9(07).
           05  SRT-DATA                PIC X(120).
      *
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMB090 WORKING STORAGE BEGINS'.
       01  WS-PGM-NAME                 PIC X(08)  VALUE 'CMB090'.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02).
           05  WS-CTLTOT-STATUS        PIC X(02).
               88  WS-CTLTOT-OK                   VALUE '00'.
               88  WS-CTLTOT-EOF                  VALUE '10'.
           05  WS-AUDIT-STATUS         PIC X(02).
               88  WS-AUDIT-OK                    VALUE '00'.
               88  WS-AUDIT-EOF                   VALUE '10'.
           05  WS-REPORT-STATUS        PIC X(02).
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01).
               88  WS-EOF                         VALUE 'Y'.
               88  WS-NOT-EOF                     VALUE 'N'.
           05  WS-FIRST-DETAIL-SW      PIC X(01)  VALUE 'Y'.
               88  WS-FIRST-DETAIL                VALUE 'Y'.
           05  WS-FOUND-SW             PIC X(01).
               88  WS-FOUND                       VALUE 'Y'.
           05  WS-AUDIT-AVAIL-SW       PIC X(01)  VALUE 'N'.
               88  WS-AUDIT-AVAILABLE             VALUE 'Y'.
           05  WS-SECTION              PIC 9(01)  VALUE 1.
               88  WS-SECTION-TOTALS              VALUE 1.
               88  WS-SECTION-RECON               VALUE 2.
               88  WS-SECTION-AUDIT               VALUE 3.
      *
       01  WS-COUNTERS                 COMP-3.
           05  WS-CTL-READ             PIC S9(09) VALUE ZERO.
           05  WS-CTL-TODAY            PIC S9(09) VALUE ZERO.
           05  WS-CTL-OTHER-DATE       PIC S9(09) VALUE ZERO.
           05  WS-CTL-RETURNED         PIC S9(09) VALUE ZERO.
           05  WS-AUD-READ             PIC S9(09) VALUE ZERO.
           05  WS-AUD-TODAY            PIC S9(09) VALUE ZERO.
           05  WS-AUD-INFO             PIC S9(09) VALUE ZERO.
           05  WS-AUD-WARN             PIC S9(09) VALUE ZERO.
           05  WS-AUD-ERROR            PIC S9(09) VALUE ZERO.
           05  WS-AUD-ABEND            PIC S9(09) VALUE ZERO.
           05  WS-RULES-CHECKED        PIC S9(05) VALUE ZERO.
           05  WS-RULES-BALANCED       PIC S9(05) VALUE ZERO.
           05  WS-RULES-FAILED         PIC S9(05) VALUE ZERO.
           05  WS-JOBS-PRINTED         PIC S9(05) VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(07) VALUE ZERO.
           05  WS-SEQ                  PIC S9(07) VALUE ZERO.
      *
       01  WS-BREAK-FIELDS.
           05  WS-PREV-JOBNAME         PIC X(08)  VALUE LOW-VALUES.
           05  WS-PREV-PROGRAM         PIC X(08)  VALUE LOW-VALUES.
           05  WS-JOB-LINES            PIC S9(05) COMP-3 VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * LATEST POSTING PER STAGE / COUNTER - FEEDS THE RECONCILIATION  *
      *----------------------------------------------------------------*
       01  WS-LATEST-MAX               PIC S9(04) COMP VALUE +1000.
       01  WS-LATEST-COUNT             PIC S9(04) COMP VALUE ZERO.
       01  WS-LATEST-TABLE.
           05  WS-LT-ENTRY             OCCURS 1000 TIMES
                                       INDEXED BY LT-IX.
               10  WS-LT-STAGE         PIC X(08).
               10  WS-LT-COUNTER       PIC X(16).
               10  WS-LT-COUNT         PIC S9(09)       COMP-3.
               10  WS-LT-AMOUNT        PIC S9(15)V99    COMP-3.
               10  WS-LT-QTY-HASH      PIC S9(15)V9(04) COMP-3.
               10  WS-LT-JOBNAME       PIC X(08).
      *
      *----------------------------------------------------------------*
      * RECONCILIATION RULES - STAGE/COUNTER NAMES ARE THE CONTRACT    *
      * WITH THE POSTING PROGRAMS.  DO NOT RENAME WITHOUT CHANGING THE *
      * POSTING SIDE (SEE DOCS/PROGRAMS-CM).                           *
      *----------------------------------------------------------------*
       01  WS-RULE-DATA.
           05  FILLER                  PIC X(40) VALUE
               'TC FINAL TRADES TO TC EXTRACT (TCB500)  '.
           05  FILLER                  PIC X(24) VALUE
               'TCB300  TRADES-OUT      '.
           05  FILLER                  PIC X(24) VALUE
               'TCB500  TRADES-IN       '.
           05  FILLER                  PIC X(40) VALUE
               'TC STOCK RECORD ACTIVITY TO SR (SRB100) '.
           05  FILLER                  PIC X(24) VALUE
               'TCB500  SRACTV-OUT      '.
           05  FILLER                  PIC X(24) VALUE
               'SRB100  TC-ACTV-IN      '.
           05  FILLER                  PIC X(40) VALUE
               'CA PAYABLE ACTIVITY TO SR (SRB100)      '.
           05  FILLER                  PIC X(24) VALUE
               'CAB400  SRACTV-OUT      '.
           05  FILLER                  PIC X(24) VALUE
               'SRB100  CA-ACTV-IN      '.
       01  WS-RULE-TABLE REDEFINES WS-RULE-DATA.
           05  WS-RULE                 OCCURS 3 TIMES.
               10  WS-RULE-DESC        PIC X(40).
               10  WS-RULE-FROM-STAGE  PIC X(08).
               10  WS-RULE-FROM-CTR    PIC X(16).
               10  WS-RULE-TO-STAGE    PIC X(08).
               10  WS-RULE-TO-CTR      PIC X(16).
       01  WS-RULE-COUNT               PIC S9(04) COMP VALUE +3.
       01  WS-RULE-SUB                 PIC S9(04) COMP.
      *
       01  WS-RECON-WORK.
           05  WS-RC-FROM-FOUND        PIC X(01).
           05  WS-RC-TO-FOUND          PIC X(01).
           05  WS-RC-FROM-COUNT        PIC S9(09)       COMP-3.
           05  WS-RC-FROM-AMOUNT       PIC S9(15)V99    COMP-3.
           05  WS-RC-FROM-QTY          PIC S9(15)V9(04) COMP-3.
           05  WS-RC-TO-COUNT          PIC S9(09)       COMP-3.
           05  WS-RC-TO-AMOUNT         PIC S9(15)V99    COMP-3.
           05  WS-RC-TO-QTY            PIC S9(15)V9(04) COMP-3.
           05  WS-RC-DIFF-COUNT        PIC S9(09)       COMP-3.
           05  WS-RC-DIFF-AMOUNT       PIC S9(15)V99    COMP-3.
           05  WS-RC-DIFF-QTY          PIC S9(15)V9(04) COMP-3.
           05  WS-RC-STATUS            PIC X(16).
           05  WS-LK-STAGE             PIC X(08).
           05  WS-LK-COUNTER           PIC X(16).
      *
      *----------------------------------------------------------------*
      * AUDIT EXCEPTIONS (W / E / A) FOR TODAY - FIRST 100 KEPT        *
      *----------------------------------------------------------------*
       01  WS-EXC-MAX                  PIC S9(04) COMP VALUE +100.
       01  WS-EXC-COUNT                PIC S9(04) COMP VALUE ZERO.
       01  WS-EXC-SUB                  PIC S9(04) COMP.
       01  WS-EXC-TABLE.
           05  WS-EXC-ENTRY            OCCURS 100 TIMES.
               10  WS-EXC-TIME         PIC X(08).
               10  WS-EXC-JOBNAME      PIC X(08).
               10  WS-EXC-STEPNAME     PIC X(08).
               10  WS-EXC-PROGRAM      PIC X(08).
               10  WS-EXC-EVENT        PIC X(08).
               10  WS-EXC-SEVERITY     PIC X(01).
               10  WS-EXC-MESSAGE      PIC X(80).
      *
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
       01  WS-DSP-COUNT                PIC ZZZ,ZZZ,ZZ9.
       01  WS-DSP-RC                   PIC 9(02).
       01  WS-DSP-RULE                 PIC 9(01).
       01  WS-DSP-N1                   PIC ZZZ9.
       01  WS-DSP-N2                   PIC ZZZ9.
       01  WS-DSP-N3                   PIC ZZZ9.
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
           COPY CMRPTHD.
      *
       01  RPT-SECTION-LINE.
           05  RPT-SL-CC               PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-SL-TEXT             PIC X(80).
           05  FILLER                  PIC X(50)  VALUE SPACES.
      *
       01  RPT-TOT-COLHDR-1.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(10)  VALUE 'JOB'.
           05  FILLER                  PIC X(10)  VALUE 'PROGRAM'.
           05  FILLER                  PIC X(10)  VALUE 'STAGE'.
           05  FILLER                  PIC X(18)  VALUE 'COUNTER'.
           05  FILLER                  PIC X(12)  VALUE
                                                 '     RECORDS'.
           05  FILLER                  PIC X(24)  VALUE
                                   '                  AMOUNT'.
           05  FILLER                  PIC X(27)  VALUE
                                   '              QUANTITY HASH'.
           05  FILLER                  PIC X(19)  VALUE
                                   '  POSTED'.
       01  RPT-TOT-COLHDR-2.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE ALL '-'.
           05  FILLER                  PIC X(08)  VALUE SPACES.
      *
       01  RPT-TOT-DETAIL.
           05  RPT-TD-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-JOBNAME          PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-PROGRAM          PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-STAGE            PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-COUNTER          PIC X(16).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-COUNT            PIC ZZZ,ZZZ,ZZ9-.
           05  FILLER                  PIC X(01)  VALUE SPACES.
           05  RPT-TD-AMOUNT           PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-QTY-HASH         PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-TD-TIME             PIC X(08).
           05  FILLER                  PIC X(09)  VALUE SPACES.
      *
       01  RPT-RECON-COLHDR.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(60)  VALUE
               'RULE  HAND-OFF                                  STATUS'.
           05  FILLER                  PIC X(70)  VALUE SPACES.
       01  RPT-RECON-RULE-LINE.
           05  RPT-RR-CC               PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RR-RULE             PIC Z9.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-RR-DESC             PIC X(40).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RR-STATUS           PIC X(16).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RR-FLAG             PIC X(03).
           05  FILLER                  PIC X(61)  VALUE SPACES.
       01  RPT-RECON-SIDE-LINE.
           05  RPT-RS-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(08)  VALUE SPACES.
           05  RPT-RS-LABEL            PIC X(06).
           05  RPT-RS-STAGE            PIC X(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-RS-COUNTER          PIC X(16).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RS-COUNT            PIC ZZZ,ZZZ,ZZ9-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RS-AMOUNT           PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RS-QTY              PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999-.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-RS-FLAG             PIC X(20).
           05  FILLER                  PIC X(05)  VALUE SPACES.
      *
       01  RPT-AUDIT-SUMMARY.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(18)  VALUE
                                                 'AUDIT EVENTS TODAY'.
           05  RPT-AS-TOTAL            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(08)  VALUE '   INFO '.
           05  RPT-AS-INFO             PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(11)  VALUE '   WARNING '.
           05  RPT-AS-WARN             PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(09)  VALUE '   ERROR '.
           05  RPT-AS-ERROR            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(09)  VALUE '   ABEND '.
           05  RPT-AS-ABEND            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(40)  VALUE SPACES.
       01  RPT-AUDIT-COLHDR.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(60)  VALUE
               'TIME      JOB       STEP      PROGRAM   EVENT     S'.
           05  FILLER                  PIC X(70)  VALUE 'MESSAGE'.
       01  RPT-AUDIT-DETAIL.
           05  RPT-AD-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-AD-TIME             PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-AD-JOBNAME          PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-AD-STEPNAME         PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-AD-PROGRAM          PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-AD-EVENT            PIC X(08).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-AD-SEVERITY         PIC X(01).
           05  FILLER                  PIC X(09)  VALUE SPACES.
           05  RPT-AD-MESSAGE          PIC X(70).
       01  RPT-MESSAGE-LINE.
           05  RPT-ML-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-ML-TEXT             PIC X(100).
           05  FILLER                  PIC X(30)  VALUE SPACES.
      *
       01  WS-PRINT-LINE               PIC X(133).
      *
           COPY CMDATEW.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMABLNK.
           COPY CMTSLNK.
      *    SECOND VIEW OF THE CONTROL TOTAL RECORD FOR THE SORT OUTPUT
           COPY CMCTLTOT REPLACING LEADING ==CTR-== BY ==WK-==.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-SCAN-AUDIT-LOG
           PERFORM 2900-AUDIT-START
           SORT SORT-FILE
               ON ASCENDING KEY SRT-JOBNAME
                                SRT-PROGRAM
                                SRT-TIMESTAMP
                                SRT-SEQ
               INPUT PROCEDURE  3000-SELECT-TOTALS
               OUTPUT PROCEDURE 3500-PRINT-TOTALS
           IF SORT-RETURN NOT = ZERO
               MOVE '0000-MAINLINE'     TO AB-PARAGRAPH
               MOVE 1008                TO AB-ABEND-CODE
               MOVE 'SORT OF CONTROL TOTALS FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           PERFORM 4000-RECONCILE
           PERFORM 5000-PRINT-AUDIT-SUMMARY
           PERFORM 9000-TERMINATE
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - DATE CARD, REPORT FILE                                  *
      *----------------------------------------------------------------*
       1000-INITIALIZE SECTION.
       1000-START.
           DISPLAY 'CMB090 - CLOSE BUSINESS DAY - STARTED'
           OPEN INPUT DATE-CARD-FILE
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON DATE CARD' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           READ DATE-CARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1005                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE DATE-CARD-FILE
           DISPLAY 'CMB090 - BUSINESS DATE ' DC-BUS-DATE
                   ' CYCLE ' DC-CYCLE-TYPE
      *
           OPEN OUTPUT REPORT-FILE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS    TO AB-FILE-STATUS
               MOVE 'RPTFILE'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON CONTROL REPORT' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           MOVE 'CMR090'             TO RPT-H1-REPORT-ID
           MOVE WS-PGM-NAME          TO RPT-H2-PROGRAM
           MOVE 'EOD CONTROL TOTALS AND CROSS-SUBSYSTEM RECONCILIATION'
                                     TO RPT-H2-TITLE
           MOVE DC-BUS-CCYY          TO WS-DE-CCYY
           MOVE DC-BUS-MM            TO WS-DE-MM
           MOVE DC-BUS-DD            TO WS-DE-DD
           MOVE WS-DATE-EDIT         TO RPT-H2-BUS-DATE
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP(1:10)   TO RPT-H1-RUN-DATE
           MOVE +99                  TO RPT-LINE-COUNT
           MOVE ZERO                 TO RPT-PAGE-COUNT.
      *
      *----------------------------------------------------------------*
      * 2000 - AUDIT LOG SCAN (BEFORE CMU060 OPENS IT FOR EXTEND)      *
      *----------------------------------------------------------------*
       2000-SCAN-AUDIT-LOG SECTION.
       2000-START.
           OPEN INPUT AUDIT-FILE
           IF WS-AUDIT-STATUS NOT = '00' AND NOT = '05'
               DISPLAY 'CMB090 - AUDITLOG NOT AVAILABLE, STATUS '
                       WS-AUDIT-STATUS ' - AUDIT SUMMARY SKIPPED'
               GO TO 2000-EXIT
           END-IF
           SET WS-AUDIT-AVAILABLE TO TRUE
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF
               READ AUDIT-FILE
               EVALUATE TRUE
                   WHEN WS-AUDIT-OK
                       ADD 1 TO WS-AUD-READ
                       IF AUD-BUS-DATE = DC-BUS-DATE
                           PERFORM 2100-TALLY-AUDIT-EVENT
                       END-IF
                   WHEN WS-AUDIT-EOF
                       SET WS-EOF TO TRUE
                   WHEN OTHER
                       DISPLAY 'CMB090 - AUDITLOG READ ERROR, STATUS '
                               WS-AUDIT-STATUS
                       SET WS-EOF TO TRUE
               END-EVALUATE
           END-PERFORM
           CLOSE AUDIT-FILE
           GO TO 2000-EXIT.
      *
       2100-TALLY-AUDIT-EVENT.
           ADD 1 TO WS-AUD-TODAY
           EVALUATE TRUE
               WHEN AUD-INFO     ADD 1 TO WS-AUD-INFO
               WHEN AUD-WARNING  ADD 1 TO WS-AUD-WARN
               WHEN AUD-ERROR    ADD 1 TO WS-AUD-ERROR
               WHEN AUD-ABEND    ADD 1 TO WS-AUD-ABEND
               WHEN OTHER        ADD 1 TO WS-AUD-INFO
           END-EVALUATE
           IF (AUD-WARNING OR AUD-ERROR OR AUD-ABEND)
              AND WS-EXC-COUNT < WS-EXC-MAX
               ADD 1 TO WS-EXC-COUNT
               MOVE AUD-TIMESTAMP(12:8) TO WS-EXC-TIME (WS-EXC-COUNT)
               MOVE AUD-JOBNAME   TO WS-EXC-JOBNAME  (WS-EXC-COUNT)
               MOVE AUD-STEPNAME  TO WS-EXC-STEPNAME (WS-EXC-COUNT)
               MOVE AUD-PROGRAM   TO WS-EXC-PROGRAM  (WS-EXC-COUNT)
               MOVE AUD-EVENT     TO WS-EXC-EVENT    (WS-EXC-COUNT)
               MOVE AUD-SEVERITY  TO WS-EXC-SEVERITY (WS-EXC-COUNT)
               MOVE AUD-MESSAGE   TO WS-EXC-MESSAGE  (WS-EXC-COUNT)
           END-IF.
      *
       2000-EXIT.
           EXIT.
      *
       2900-AUDIT-START SECTION.
       2900-START.
           MOVE 'WRIT'               TO AU-FUNCTION
           MOVE WS-PGM-NAME          TO AU-PROGRAM
           MOVE 'START'              TO AU-EVENT
           MOVE 'I'                  TO AU-SEVERITY
           MOVE DC-BUS-DATE          TO AU-BUS-DATE
           MOVE SPACES               TO AU-KEY
           MOVE 'CLOSE BUSINESS DAY STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
      *----------------------------------------------------------------*
      * 3000 - SORT INPUT PROCEDURE: TODAY'S CONTROL TOTALS            *
      *----------------------------------------------------------------*
       3000-SELECT-TOTALS SECTION.
       3000-START.
           OPEN INPUT CTLTOT-FILE
           IF WS-CTLTOT-STATUS NOT = '00' AND NOT = '05'
               MOVE '3000-SELECT-TOTALS' TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CTLTOT-STATUS    TO AB-FILE-STATUS
               MOVE 'CTLTOTS'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON CONTROL TOTALS' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF
               READ CTLTOT-FILE
               EVALUATE TRUE
                   WHEN WS-CTLTOT-OK
                       ADD 1 TO WS-CTL-READ
                       IF CTR-BUS-DATE = DC-BUS-DATE
                           PERFORM 3100-RELEASE-TOTAL
                       ELSE
                           ADD 1 TO WS-CTL-OTHER-DATE
                       END-IF
                   WHEN WS-CTLTOT-EOF
                       SET WS-EOF TO TRUE
                   WHEN OTHER
                       MOVE '3000-SELECT-TOTALS' TO AB-PARAGRAPH
                       MOVE 1002             TO AB-ABEND-CODE
                       MOVE WS-CTLTOT-STATUS TO AB-FILE-STATUS
                       MOVE 'CTLTOTS'        TO AB-DDNAME
                       MOVE 'READ FAILED ON CONTROL TOTALS'
                                             TO AB-MESSAGE
                       PERFORM 9999-ABEND
               END-EVALUATE
           END-PERFORM
           CLOSE CTLTOT-FILE
           GO TO 3000-EXIT.
      *
       3100-RELEASE-TOTAL.
           ADD 1 TO WS-CTL-TODAY
           ADD 1 TO WS-SEQ
           MOVE CTR-JOBNAME          TO SRT-JOBNAME
           MOVE CTR-PROGRAM          TO SRT-PROGRAM
           MOVE CTR-TIMESTAMP        TO SRT-TIMESTAMP
           MOVE WS-SEQ               TO SRT-SEQ
           MOVE CTR-CONTROL-REC      TO SRT-DATA
           RELEASE SORT-REC
      *    KEEP THE LATEST FIGURES PER STAGE/COUNTER (FILE ORDER)
           SET LT-IX TO 1
           SEARCH WS-LT-ENTRY
               AT END
                   PERFORM 3150-ADD-LATEST
               WHEN LT-IX > WS-LATEST-COUNT
                   PERFORM 3150-ADD-LATEST
               WHEN WS-LT-STAGE (LT-IX) = CTR-STAGE
                AND WS-LT-COUNTER (LT-IX) = CTR-COUNTER
                   PERFORM 3160-SET-LATEST
           END-SEARCH.
      *
       3150-ADD-LATEST.
           IF WS-LATEST-COUNT NOT < WS-LATEST-MAX
               MOVE '3150-ADD-LATEST'   TO AB-PARAGRAPH
               MOVE 1007                TO AB-ABEND-CODE
               MOVE CTR-STAGE           TO AB-KEY
               MOVE 'STAGE/COUNTER TABLE FULL (1000)' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           ADD 1 TO WS-LATEST-COUNT
           SET LT-IX TO WS-LATEST-COUNT
           MOVE CTR-STAGE            TO WS-LT-STAGE (LT-IX)
           MOVE CTR-COUNTER          TO WS-LT-COUNTER (LT-IX)
           PERFORM 3160-SET-LATEST.
      *
       3160-SET-LATEST.
           MOVE CTR-COUNT            TO WS-LT-COUNT (LT-IX)
           MOVE CTR-AMOUNT           TO WS-LT-AMOUNT (LT-IX)
           MOVE CTR-QTY-HASH         TO WS-LT-QTY-HASH (LT-IX)
           MOVE CTR-JOBNAME          TO WS-LT-JOBNAME (LT-IX).
      *
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3500 - SORT OUTPUT PROCEDURE: SECTION 1 OF THE REPORT          *
      *----------------------------------------------------------------*
       3500-PRINT-TOTALS SECTION.
       3500-START.
           SET WS-SECTION-TOTALS TO TRUE
           MOVE 'SECTION 1 - CONTROL TOTALS POSTED TODAY BY JOB'
                                     TO RPT-SL-TEXT
           MOVE RPT-SECTION-LINE     TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           PERFORM 8200-WRITE-COLUMN-HEADINGS
           SET WS-NOT-EOF TO TRUE
           PERFORM UNTIL WS-EOF
               RETURN SORT-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-CTL-RETURNED
                       PERFORM 3600-PRINT-DETAIL
               END-RETURN
           END-PERFORM
           IF WS-CTL-RETURNED = ZERO
               MOVE SPACES TO RPT-ML-TEXT
               STRING '*** NO CONTROL TOTALS POSTED FOR BUSINESS DATE '
                      DC-BUS-DATE ' ***' DELIMITED BY SIZE
                 INTO RPT-ML-TEXT
               MOVE '0' TO RPT-ML-CC
               MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
               PERFORM 8000-WRITE-LINE
               MOVE ' ' TO RPT-ML-CC
               MOVE 4 TO WS-RETURN-CODE
           END-IF
           MOVE SPACES TO RPT-ML-TEXT
           MOVE WS-CTL-TODAY TO WS-DSP-COUNT
           STRING 'RECORDS FOR BUSINESS DATE: ' WS-DSP-COUNT
                  DELIMITED BY SIZE INTO RPT-ML-TEXT
           MOVE '0' TO RPT-ML-CC
           MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           MOVE SPACES TO RPT-ML-TEXT
           MOVE WS-CTL-OTHER-DATE TO WS-DSP-COUNT
           STRING 'RECORDS FOR OTHER DATES (IGNORED): ' WS-DSP-COUNT
                  DELIMITED BY SIZE INTO RPT-ML-TEXT
           MOVE ' ' TO RPT-ML-CC
           MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           GO TO 3500-EXIT.
      *
       3600-PRINT-DETAIL.
           MOVE SRT-DATA             TO WK-CONTROL-REC
           MOVE SPACES               TO RPT-TD-JOBNAME RPT-TD-PROGRAM
           MOVE ' '                  TO RPT-TD-CC
           IF SRT-JOBNAME NOT = WS-PREV-JOBNAME
               ADD 1 TO WS-JOBS-PRINTED
               IF NOT WS-FIRST-DETAIL
                   MOVE '0'          TO RPT-TD-CC
               END-IF
               MOVE SRT-JOBNAME      TO RPT-TD-JOBNAME
               MOVE SRT-PROGRAM      TO RPT-TD-PROGRAM
               MOVE SRT-JOBNAME      TO WS-PREV-JOBNAME
               MOVE SRT-PROGRAM      TO WS-PREV-PROGRAM
           ELSE
               IF SRT-PROGRAM NOT = WS-PREV-PROGRAM
                   MOVE SRT-PROGRAM  TO RPT-TD-PROGRAM
                   MOVE SRT-PROGRAM  TO WS-PREV-PROGRAM
               END-IF
           END-IF
           MOVE 'N'                  TO WS-FIRST-DETAIL-SW
           MOVE WK-STAGE             TO RPT-TD-STAGE
           MOVE WK-COUNTER           TO RPT-TD-COUNTER
           MOVE WK-COUNT             TO RPT-TD-COUNT
           MOVE WK-AMOUNT            TO RPT-TD-AMOUNT
           MOVE WK-QTY-HASH          TO RPT-TD-QTY-HASH
           MOVE WK-TIMESTAMP(12:8)   TO RPT-TD-TIME
           MOVE RPT-TOT-DETAIL       TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE.
      *
       3500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4000 - CROSS-SUBSYSTEM RECONCILIATION (SECTION 2)              *
      *----------------------------------------------------------------*
       4000-RECONCILE SECTION.
       4000-START.
           SET WS-SECTION-RECON TO TRUE
           MOVE +99 TO RPT-LINE-COUNT
           MOVE 'SECTION 2 - CROSS-SUBSYSTEM RECONCILIATION'
                                     TO RPT-SL-TEXT
           MOVE RPT-SECTION-LINE     TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           PERFORM 8200-WRITE-COLUMN-HEADINGS
           PERFORM VARYING WS-RULE-SUB FROM 1 BY 1
                   UNTIL WS-RULE-SUB > WS-RULE-COUNT
               PERFORM 4100-CHECK-RULE
           END-PERFORM
           MOVE SPACES TO RPT-ML-TEXT
           MOVE WS-RULES-CHECKED  TO WS-DSP-N1
           MOVE WS-RULES-BALANCED TO WS-DSP-N2
           MOVE WS-RULES-FAILED   TO WS-DSP-N3
           STRING 'RULES CHECKED ' WS-DSP-N1
                  '   IN BALANCE ' WS-DSP-N2
                  '   FAILED ' WS-DSP-N3
                  DELIMITED BY SIZE INTO RPT-ML-TEXT
           MOVE '-' TO RPT-ML-CC
           MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           MOVE ' ' TO RPT-ML-CC
           GO TO 4000-EXIT.
      *
       4100-CHECK-RULE.
           ADD 1 TO WS-RULES-CHECKED
           MOVE WS-RULE-FROM-STAGE (WS-RULE-SUB) TO WS-LK-STAGE
           MOVE WS-RULE-FROM-CTR (WS-RULE-SUB)   TO WS-LK-COUNTER
           PERFORM 4500-FIND-LATEST
           MOVE WS-FOUND-SW          TO WS-RC-FROM-FOUND
           IF WS-FOUND
               MOVE WS-LT-COUNT (LT-IX)    TO WS-RC-FROM-COUNT
               MOVE WS-LT-AMOUNT (LT-IX)   TO WS-RC-FROM-AMOUNT
               MOVE WS-LT-QTY-HASH (LT-IX) TO WS-RC-FROM-QTY
           ELSE
               MOVE ZERO TO WS-RC-FROM-COUNT WS-RC-FROM-AMOUNT
                            WS-RC-FROM-QTY
           END-IF
           MOVE WS-RULE-TO-STAGE (WS-RULE-SUB)   TO WS-LK-STAGE
           MOVE WS-RULE-TO-CTR (WS-RULE-SUB)     TO WS-LK-COUNTER
           PERFORM 4500-FIND-LATEST
           MOVE WS-FOUND-SW          TO WS-RC-TO-FOUND
           IF WS-FOUND
               MOVE WS-LT-COUNT (LT-IX)    TO WS-RC-TO-COUNT
               MOVE WS-LT-AMOUNT (LT-IX)   TO WS-RC-TO-AMOUNT
               MOVE WS-LT-QTY-HASH (LT-IX) TO WS-RC-TO-QTY
           ELSE
               MOVE ZERO TO WS-RC-TO-COUNT WS-RC-TO-AMOUNT
                            WS-RC-TO-QTY
           END-IF
           COMPUTE WS-RC-DIFF-COUNT  = WS-RC-FROM-COUNT
                                     - WS-RC-TO-COUNT
           COMPUTE WS-RC-DIFF-AMOUNT = WS-RC-FROM-AMOUNT
                                     - WS-RC-TO-AMOUNT
           COMPUTE WS-RC-DIFF-QTY    = WS-RC-FROM-QTY
                                     - WS-RC-TO-QTY
           EVALUATE TRUE
               WHEN WS-RC-FROM-FOUND NOT = 'Y'
                 OR WS-RC-TO-FOUND   NOT = 'Y'
                   MOVE 'NOT POSTED'     TO WS-RC-STATUS
                   ADD 1 TO WS-RULES-FAILED
                   MOVE 4 TO WS-RETURN-CODE
               WHEN WS-RC-DIFF-COUNT NOT = ZERO
                   MOVE 'OUT OF BALANCE' TO WS-RC-STATUS
                   ADD 1 TO WS-RULES-FAILED
                   MOVE 4 TO WS-RETURN-CODE
               WHEN OTHER
                   MOVE 'IN BALANCE'     TO WS-RC-STATUS
                   ADD 1 TO WS-RULES-BALANCED
           END-EVALUATE
           PERFORM 4200-PRINT-RULE
           IF WS-RC-STATUS NOT = 'IN BALANCE'
               PERFORM 4300-AUDIT-IMBALANCE
           END-IF.
      *
       4200-PRINT-RULE.
           MOVE WS-RULE-SUB          TO RPT-RR-RULE
           MOVE WS-RULE-DESC (WS-RULE-SUB) TO RPT-RR-DESC
           MOVE WS-RC-STATUS         TO RPT-RR-STATUS
           MOVE SPACES               TO RPT-RR-FLAG
           IF WS-RC-STATUS NOT = 'IN BALANCE'
               MOVE '***'            TO RPT-RR-FLAG
           END-IF
           MOVE RPT-RECON-RULE-LINE  TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
      *    FROM SIDE
           MOVE 'FROM'               TO RPT-RS-LABEL
           MOVE WS-RULE-FROM-STAGE (WS-RULE-SUB) TO RPT-RS-STAGE
           MOVE WS-RULE-FROM-CTR (WS-RULE-SUB)   TO RPT-RS-COUNTER
           MOVE WS-RC-FROM-COUNT     TO RPT-RS-COUNT
           MOVE WS-RC-FROM-AMOUNT    TO RPT-RS-AMOUNT
           MOVE WS-RC-FROM-QTY       TO RPT-RS-QTY
           MOVE SPACES               TO RPT-RS-FLAG
           IF WS-RC-FROM-FOUND NOT = 'Y'
               MOVE '<< NOT POSTED'  TO RPT-RS-FLAG
           END-IF
           MOVE RPT-RECON-SIDE-LINE  TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
      *    TO SIDE
           MOVE 'TO'                 TO RPT-RS-LABEL
           MOVE WS-RULE-TO-STAGE (WS-RULE-SUB)   TO RPT-RS-STAGE
           MOVE WS-RULE-TO-CTR (WS-RULE-SUB)     TO RPT-RS-COUNTER
           MOVE WS-RC-TO-COUNT       TO RPT-RS-COUNT
           MOVE WS-RC-TO-AMOUNT      TO RPT-RS-AMOUNT
           MOVE WS-RC-TO-QTY         TO RPT-RS-QTY
           MOVE SPACES               TO RPT-RS-FLAG
           IF WS-RC-TO-FOUND NOT = 'Y'
               MOVE '<< NOT POSTED'  TO RPT-RS-FLAG
           END-IF
           MOVE RPT-RECON-SIDE-LINE  TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
      *    DIFFERENCE - HASH DIFFERENCES ARE INFORMATIONAL ONLY
           MOVE 'DIFF'               TO RPT-RS-LABEL
           MOVE SPACES               TO RPT-RS-STAGE RPT-RS-COUNTER
           MOVE WS-RC-DIFF-COUNT     TO RPT-RS-COUNT
           MOVE WS-RC-DIFF-AMOUNT    TO RPT-RS-AMOUNT
           MOVE WS-RC-DIFF-QTY       TO RPT-RS-QTY
           MOVE SPACES               TO RPT-RS-FLAG
           IF WS-RC-FROM-FOUND = 'Y' AND WS-RC-TO-FOUND = 'Y'
              AND WS-RC-FROM-AMOUNT NOT = ZERO
              AND WS-RC-TO-AMOUNT   NOT = ZERO
              AND (WS-RC-DIFF-AMOUNT NOT = ZERO
                   OR WS-RC-DIFF-QTY NOT = ZERO)
               MOVE '* HASH DIFF-REVIEW' TO RPT-RS-FLAG
           END-IF
           MOVE RPT-RECON-SIDE-LINE  TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE.
      *
       4300-AUDIT-IMBALANCE.
           MOVE 'WRIT'               TO AU-FUNCTION
           MOVE WS-PGM-NAME          TO AU-PROGRAM
           MOVE 'RECON'              TO AU-EVENT
           MOVE 'W'                  TO AU-SEVERITY
           MOVE DC-BUS-DATE          TO AU-BUS-DATE
           MOVE SPACES               TO AU-KEY
           STRING WS-RULE-FROM-STAGE (WS-RULE-SUB) ' '
                  WS-RULE-FROM-CTR (WS-RULE-SUB) ' '
                  WS-RULE-TO-STAGE (WS-RULE-SUB)
                  DELIMITED BY SIZE INTO AU-KEY
           MOVE SPACES               TO AU-MESSAGE
           MOVE WS-RC-DIFF-COUNT     TO WS-DSP-COUNT
           MOVE WS-RULE-SUB          TO WS-DSP-RULE
           STRING 'RULE ' WS-DSP-RULE ' ' WS-RC-STATUS
                  ' COUNT DIFFERENCE ' WS-DSP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           DISPLAY 'CMB090 - RULE ' WS-DSP-RULE ' '
                   WS-RULE-DESC (WS-RULE-SUB) ' ' WS-RC-STATUS.
      *
       4500-FIND-LATEST.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING LT-IX FROM 1 BY 1
                   UNTIL LT-IX > WS-LATEST-COUNT OR WS-FOUND
               IF WS-LT-STAGE (LT-IX) = WS-LK-STAGE
                  AND WS-LT-COUNTER (LT-IX) = WS-LK-COUNTER
                   MOVE 'Y' TO WS-FOUND-SW
               END-IF
           END-PERFORM
           IF WS-FOUND
               SET LT-IX DOWN BY 1
           END-IF.
      *
       4000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 5000 - AUDIT LOG SUMMARY (SECTION 3)                           *
      *----------------------------------------------------------------*
       5000-PRINT-AUDIT-SUMMARY SECTION.
       5000-START.
           SET WS-SECTION-AUDIT TO TRUE
           MOVE +99 TO RPT-LINE-COUNT
           MOVE 'SECTION 3 - AUDIT LOG EXCEPTIONS FOR THE DAY'
                                     TO RPT-SL-TEXT
           MOVE RPT-SECTION-LINE     TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           IF NOT WS-AUDIT-AVAILABLE
               MOVE '*** AUDIT LOG NOT AVAILABLE ***' TO RPT-ML-TEXT
               MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
               PERFORM 8000-WRITE-LINE
               GO TO 5000-EXIT
           END-IF
           MOVE WS-AUD-TODAY         TO RPT-AS-TOTAL
           MOVE WS-AUD-INFO          TO RPT-AS-INFO
           MOVE WS-AUD-WARN          TO RPT-AS-WARN
           MOVE WS-AUD-ERROR         TO RPT-AS-ERROR
           MOVE WS-AUD-ABEND         TO RPT-AS-ABEND
           MOVE RPT-AUDIT-SUMMARY    TO WS-PRINT-LINE
           PERFORM 8000-WRITE-LINE
           IF WS-EXC-COUNT = ZERO
               MOVE '0'              TO RPT-ML-CC
               MOVE 'NO WARNING, ERROR OR ABEND EVENTS'
                                     TO RPT-ML-TEXT
               MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
               PERFORM 8000-WRITE-LINE
               MOVE ' '              TO RPT-ML-CC
               GO TO 5000-EXIT
           END-IF
           PERFORM 8200-WRITE-COLUMN-HEADINGS
           PERFORM VARYING WS-EXC-SUB FROM 1 BY 1
                   UNTIL WS-EXC-SUB > WS-EXC-COUNT
               MOVE WS-EXC-TIME (WS-EXC-SUB)     TO RPT-AD-TIME
               MOVE WS-EXC-JOBNAME (WS-EXC-SUB)  TO RPT-AD-JOBNAME
               MOVE WS-EXC-STEPNAME (WS-EXC-SUB) TO RPT-AD-STEPNAME
               MOVE WS-EXC-PROGRAM (WS-EXC-SUB)  TO RPT-AD-PROGRAM
               MOVE WS-EXC-EVENT (WS-EXC-SUB)    TO RPT-AD-EVENT
               MOVE WS-EXC-SEVERITY (WS-EXC-SUB) TO RPT-AD-SEVERITY
               MOVE WS-EXC-MESSAGE (WS-EXC-SUB)  TO RPT-AD-MESSAGE
               MOVE RPT-AUDIT-DETAIL TO WS-PRINT-LINE
               PERFORM 8000-WRITE-LINE
           END-PERFORM
           IF WS-AUD-WARN + WS-AUD-ERROR + WS-AUD-ABEND > WS-EXC-MAX
               MOVE '0'              TO RPT-ML-CC
               MOVE '*** ONLY THE FIRST 100 EXCEPTIONS ARE LISTED ***'
                                     TO RPT-ML-TEXT
               MOVE RPT-MESSAGE-LINE TO WS-PRINT-LINE
               PERFORM 8000-WRITE-LINE
               MOVE ' '              TO RPT-ML-CC
           END-IF.
       5000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 8000 - REPORT I/O                                              *
      *----------------------------------------------------------------*
       8000-WRITE-LINE SECTION.
       8000-START.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8100-PAGE-HEADINGS
           END-IF
           WRITE REPORT-REC FROM WS-PRINT-LINE
           IF WS-REPORT-STATUS NOT = '00'
               MOVE '8000-WRITE-LINE'   TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-REPORT-STATUS    TO AB-FILE-STATUS
               MOVE 'RPTFILE'           TO AB-DDNAME
               MOVE 'WRITE FAILED ON CONTROL REPORT' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           ADD 1 TO WS-LINES-WRITTEN
           EVALUATE WS-PRINT-LINE(1:1)
               WHEN '0'    ADD 2 TO RPT-LINE-COUNT
               WHEN '-'    ADD 3 TO RPT-LINE-COUNT
               WHEN OTHER  ADD 1 TO RPT-LINE-COUNT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
       8100-PAGE-HEADINGS SECTION.
       8100-START.
           ADD 1 TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT       TO RPT-H1-PAGE
           WRITE REPORT-REC FROM RPT-HEADING-1
           WRITE REPORT-REC FROM RPT-HEADING-2
           WRITE REPORT-REC FROM RPT-BLANK-LINE
           ADD 3 TO WS-LINES-WRITTEN
           MOVE 3 TO RPT-LINE-COUNT
      *    REPEAT THE COLUMN HEADINGS OF THE CURRENT SECTION, EXCEPT
      *    WHEN THE PAGE BREAK IS FOR THE SECTION TITLE ITSELF
           IF WS-PRINT-LINE NOT = RPT-SECTION-LINE
               PERFORM 8200-WRITE-COLUMN-HEADINGS
           END-IF.
       8100-EXIT.
           EXIT.
      *
       8200-WRITE-COLUMN-HEADINGS SECTION.
       8200-START.
           EVALUATE TRUE
               WHEN WS-SECTION-TOTALS
                   WRITE REPORT-REC FROM RPT-TOT-COLHDR-1
                   WRITE REPORT-REC FROM RPT-TOT-COLHDR-2
                   ADD 3 TO RPT-LINE-COUNT
                   ADD 2 TO WS-LINES-WRITTEN
               WHEN WS-SECTION-RECON
                   WRITE REPORT-REC FROM RPT-RECON-COLHDR
                   ADD 2 TO RPT-LINE-COUNT
                   ADD 1 TO WS-LINES-WRITTEN
               WHEN WS-SECTION-AUDIT
                   WRITE REPORT-REC FROM RPT-AUDIT-COLHDR
                   ADD 2 TO RPT-LINE-COUNT
                   ADD 1 TO WS-LINES-WRITTEN
           END-EVALUATE.
       8200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 9000 - TERMINATION                                             *
      *----------------------------------------------------------------*
       9000-TERMINATE SECTION.
       9000-START.
           WRITE REPORT-REC FROM RPT-END-LINE
           ADD 1 TO WS-LINES-WRITTEN
           CLOSE REPORT-FILE
      *    OWN CONTROL TOTALS - CTLTOTS IS CLOSED BY NOW
           MOVE 'POST'               TO CT-FUNCTION
           MOVE DC-BUS-DATE          TO CT-BUS-DATE
           MOVE WS-PGM-NAME          TO CT-PROGRAM
           MOVE WS-PGM-NAME          TO CT-STAGE
           MOVE ZERO                 TO CT-AMOUNT CT-QTY-HASH
           MOVE 'CTLTOTS-IN'         TO CT-COUNTER-NAME
           MOVE WS-CTL-TODAY         TO CT-COUNT
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'RECON-FAILED'       TO CT-COUNTER-NAME
           MOVE WS-RULES-FAILED      TO CT-COUNT
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'AUDIT-IN'           TO CT-COUNTER-NAME
           MOVE WS-AUD-TODAY         TO CT-COUNT
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'RPT-LINES'          TO CT-COUNTER-NAME
           MOVE WS-LINES-WRITTEN     TO CT-COUNT
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'CLOS'               TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE WS-RETURN-CODE       TO WS-DSP-RC
           MOVE 'WRIT'               TO AU-FUNCTION
           MOVE WS-PGM-NAME          TO AU-PROGRAM
           MOVE 'END'                TO AU-EVENT
           MOVE 'I'                  TO AU-SEVERITY
           IF WS-RETURN-CODE > ZERO
               MOVE 'W'              TO AU-SEVERITY
           END-IF
           MOVE DC-BUS-DATE          TO AU-BUS-DATE
           MOVE SPACES               TO AU-KEY AU-MESSAGE
           STRING 'CLOSE BUSINESS DAY ENDED RC=' WS-DSP-RC
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'               TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*------------------------------------------------*'
           DISPLAY '* CMB090 - CLOSE BUSINESS DAY - STATISTICS       *'
           DISPLAY '*------------------------------------------------*'
           MOVE WS-CTL-READ          TO WS-DSP-COUNT
           DISPLAY '  CONTROL TOTAL RECORDS READ   ' WS-DSP-COUNT
           MOVE WS-CTL-TODAY         TO WS-DSP-COUNT
           DISPLAY '  FOR THIS BUSINESS DATE       ' WS-DSP-COUNT
           MOVE WS-CTL-OTHER-DATE    TO WS-DSP-COUNT
           DISPLAY '  FOR OTHER DATES (IGNORED)    ' WS-DSP-COUNT
           MOVE WS-JOBS-PRINTED      TO WS-DSP-COUNT
           DISPLAY '  JOBS REPORTED                ' WS-DSP-COUNT
           MOVE WS-RULES-CHECKED     TO WS-DSP-COUNT
           DISPLAY '  RECONCILIATION RULES CHECKED ' WS-DSP-COUNT
           MOVE WS-RULES-FAILED      TO WS-DSP-COUNT
           DISPLAY '  RULES OUT OF BALANCE/MISSING ' WS-DSP-COUNT
           MOVE WS-AUD-TODAY         TO WS-DSP-COUNT
           DISPLAY '  AUDIT EVENTS TODAY           ' WS-DSP-COUNT
           MOVE WS-LINES-WRITTEN     TO WS-DSP-COUNT
           DISPLAY '  REPORT LINES WRITTEN         ' WS-DSP-COUNT
           DISPLAY '  RETURN CODE                  ' WS-DSP-RC
           DISPLAY 'CMB090 - CLOSE BUSINESS DAY - ENDED'.
       9000-EXIT.
           EXIT.
      *
       9999-ABEND SECTION.
       9999-START.
           MOVE WS-PGM-NAME TO AB-PROGRAM
           CALL 'CMU050' USING AB-ABEND-PARMS
           GOBACK.
