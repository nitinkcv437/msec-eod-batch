       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWR210.
       AUTHOR.        T L MORGAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  07/12/1999.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWR210                                            *
      * TITLE      : SETTLEMENT INSTRUCTION STATUS REPORT              *
      * JOB        : MSSWD030  STEP040                                 *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   PART 1  TODAY'S CUSTODIAN EVENTS (SW.STATEVT FROM SWB200):   *
      *           EVERY EXCEPTION EVENT (UNKNOWN REFERENCE, REJECTED,  *
      *           UNMATCHED, PARTIAL, NOT APPLIED) IS LISTED; ALL      *
      *           EVENTS ARE COUNTED BY EVENT CODE.                    *
      *   PART 2  OPEN INSTRUCTIONS NEEDING ATTENTION (SW.INSTR):      *
      *           UNMATCHED, REJECTED, PENDING, FAILING, PARTIAL, AND  *
      *           SENT WITHOUT ANY CUSTODIAN RESPONSE SINCE YESTERDAY. *
      *           DETAIL=ALL LISTS EVERY OPEN INSTRUCTION.             *
      *   PART 3  INVENTORY OF THE INSTRUCTION MASTER BY STATUS AND    *
      *           MESSAGE TYPE.                                        *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETERS SWP210A (DETAIL=EXCEPT/ALL)      *
      *          STATIN    MSEC.PROD.SW.STATEVT(0)        (SWSTAT)     *
      *          SWINSTR   MSEC.PROD.SW.INSTR.KSDS        (SWINSTR)    *
      * OUTPUT : RPTFILE   REPORT SWR210 (FB 133 ASA)                  *
      * CALLS  : CMU050 CMU060 CMU080 CMASM02                          *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1999-07-12 TLM  CHG05230  ORIGINAL                             *
      * 2003-01-27 KAP  CHG10244  SEQUENCE CONTROL ROW SKIPPED         *
      * 2011-06-20 SPA  CHG21877  PART 1 FROM SW.STATEVT               *
      * 2014-05-05 SPA  CHG27115  CANCELLATION ROWS COUNTED SEPARATELY *
      * 2019-05-13 MHC  CHG34410  DETAIL= PARAMETER                    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD      ASSIGN TO SYSIN
                                FILE STATUS IS WS-PARMCARD-FS.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT STATIN-FILE   ASSIGN TO STATIN
                                FILE STATUS IS WS-STATIN-FS.
           SELECT SWINSTR-FILE  ASSIGN TO SWINSTR
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS SEQUENTIAL
                                RECORD KEY IS SWI-SENDER-REF
                                FILE STATUS IS WS-SWINSTR-FS.
           SELECT RPTFILE       ASSIGN TO RPTFILE
                                FILE STATUS IS WS-RPTFILE-FS.
      *
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
       FD  STATIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STATIN-REC                  PIC X(150).
       FD  SWINSTR-FILE.
       COPY SWINSTR.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWR210'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-STATIN-FS            PIC X(02).
               88  STATIN-OK                     VALUE '00'.
               88  STATIN-EOF                    VALUE '10'.
           05  WS-SWINSTR-FS           PIC X(02).
               88  SWINSTR-OK                    VALUE '00'.
               88  SWINSTR-EOF                   VALUE '10'.
           05  WS-RPTFILE-FS           PIC X(02).
               88  RPTFILE-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-STAT-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-END-OF-EVENTS              VALUE 'Y'.
           05  WS-INSTR-EOF-SW         PIC X(01)  VALUE 'N'.
               88  WS-END-OF-MASTER              VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-DETAIL-OPT           PIC X(06)  VALUE 'EXCEPT'.
               88  WS-DETAIL-ALL                 VALUE 'ALL   '.
           05  WS-LIST-SW              PIC X(01).
               88  WS-LIST-IT                    VALUE 'Y'.
       01  WS-PARM-KEYWORD             PIC X(20).
       01  WS-PARM-VALUE               PIC X(20).
       01  WS-CUR-COL-HEAD             PIC X(133) VALUE SPACES.
      *
       COPY SWSTAT.
      *
      *----------------------------------------------------------------*
      * EVENT CODES (SWB200)                                           *
      *----------------------------------------------------------------*
       01  WS-EVENT-VALUES.
           05  FILLER  PIC X(32)  VALUE
               'MAMATCHED                       '.
           05  FILLER  PIC X(32)  VALUE
               'NMUNMATCHED                     '.
           05  FILLER  PIC X(32)  VALUE
               'PEPENDING                       '.
           05  FILLER  PIC X(32)  VALUE
               'RJREJECTED                      '.
           05  FILLER  PIC X(32)  VALUE
               'STSETTLED                       '.
           05  FILLER  PIC X(32)  VALUE
               'PSPARTIALLY SETTLED             '.
           05  FILLER  PIC X(32)  VALUE
               'CXCANCELLATION COMPLETED        '.
           05  FILLER  PIC X(32)  VALUE
               'URUNKNOWN REFERENCE             '.
       01  WS-EVENT-TABLE REDEFINES WS-EVENT-VALUES.
           05  WS-EVT-ENTRY OCCURS 8 TIMES INDEXED BY EVT-IDX.
               10  WS-EVT-CODE         PIC X(02).
               10  WS-EVT-TEXT         PIC X(30).
       01  WS-EVENT-COUNTS.
           05  WS-EVT-COUNT            PIC S9(07) COMP-3 OCCURS 8 TIMES.
           05  WS-EVT-NOT-APPLIED      PIC S9(07) COMP-3 OCCURS 8 TIMES.
       01  WS-EVT-SUB                  PIC S9(04) COMP.
      *
      *----------------------------------------------------------------*
      * STATUS INVENTORY                                               *
      *----------------------------------------------------------------*
       01  WS-STATUS-VALUES.
           05  FILLER  PIC X(32)  VALUE
               'SNSENT - NO RESPONSE YET        '.
           05  FILLER  PIC X(32)  VALUE
               'MAMATCHED                       '.
           05  FILLER  PIC X(32)  VALUE
               'NMUNMATCHED                     '.
           05  FILLER  PIC X(32)  VALUE
               'PEPENDING                       '.
           05  FILLER  PIC X(32)  VALUE
               'PSPARTIALLY SETTLED             '.
           05  FILLER  PIC X(32)  VALUE
               'FLFAILING                       '.
           05  FILLER  PIC X(32)  VALUE
               'RJREJECTED BY CUSTODIAN         '.
           05  FILLER  PIC X(32)  VALUE
               'STSETTLED                       '.
           05  FILLER  PIC X(32)  VALUE
               'CXCANCELLED                     '.
           05  FILLER  PIC X(32)  VALUE
               '??OTHER                         '.
       01  WS-STATUS-TABLE REDEFINES WS-STATUS-VALUES.
           05  WS-STS-ENTRY OCCURS 10 TIMES INDEXED BY STS-IDX.
               10  WS-STS-CODE         PIC X(02).
               10  WS-STS-TEXT         PIC X(30).
       01  WS-STATUS-COUNTS.
           05  WS-STS-541              PIC S9(07) COMP-3
                                       OCCURS 10 TIMES.
           05  WS-STS-543              PIC S9(07) COMP-3
                                       OCCURS 10 TIMES.
       01  WS-STS-SUB                  PIC S9(04) COMP.
      *
       01  WS-COUNTERS.
           05  WS-EVENTS-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EVENTS-LISTED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ROWS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEWM-ROWS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-ROWS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OPEN-LISTED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-RESPONSE          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LAST-SEQ             PIC 9(08)  VALUE ZERO.
       01  WS-WORK-FIELDS.
           05  WS-SEQ-CTL-KEY          PIC X(16)  VALUE 'SEQCONTROL'.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-EXCEPT-TEXT          PIC X(20).
      *
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
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-PART-LINE.
           05  PT-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  PT-TEXT                 PIC X(80).
           05  FILLER                  PIC X(51).
       01  WS-EV-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(10)  VALUE ' IN SEQ'.
           05  FILLER  PIC X(04)  VALUE 'MT'.
           05  FILLER  PIC X(17)  VALUE 'RELATED REF'.
           05  FILLER  PIC X(17)  VALUE 'CUSTODIAN REF'.
           05  FILLER  PIC X(04)  VALUE 'EV'.
           05  FILLER  PIC X(05)  VALUE 'STAT'.
           05  FILLER  PIC X(05)  VALUE 'RSN'.
           05  FILLER  PIC X(17)  VALUE '        QUANTITY'.
           05  FILLER  PIC X(17)  VALUE '          AMOUNT'.
           05  FILLER  PIC X(11)  VALUE 'EFF DATE'.
           05  FILLER  PIC X(04)  VALUE 'APL'.
           05  FILLER  PIC X(21)  VALUE 'EXCEPTION'.
       01  WS-EV-LINE.
           05  EV-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  EV-SEQ                  PIC 9(08).
           05  FILLER                  PIC X(01).
           05  EV-MT                   PIC X(03).
           05  FILLER                  PIC X(01).
           05  EV-RELA                 PIC X(16).
           05  FILLER                  PIC X(01).
           05  EV-CUST-REF             PIC X(16).
           05  FILLER                  PIC X(01).
           05  EV-EVENT                PIC X(02).
           05  FILLER                  PIC X(02).
           05  EV-STATUS               PIC X(04).
           05  FILLER                  PIC X(01).
           05  EV-REASON               PIC X(04).
           05  FILLER                  PIC X(01).
           05  EV-QTY                  PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  EV-AMT                  PIC ZZZ,ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(01).
           05  EV-EFF-DATE             PIC X(10).
           05  FILLER                  PIC X(02).
           05  EV-APPLIED              PIC X(01).
           05  FILLER                  PIC X(02).
           05  EV-EXCEPT               PIC X(20).
       01  WS-IN-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(18)  VALUE ' SENDER REF'.
           05  FILLER  PIC X(04)  VALUE 'MT'.
           05  FILLER  PIC X(11)  VALUE 'ACCOUNT'.
           05  FILLER  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER  PIC X(11)  VALUE 'SETTLE'.
           05  FILLER  PIC X(03)  VALUE 'ST'.
           05  FILLER  PIC X(05)  VALUE 'CODE'.
           05  FILLER  PIC X(05)  VALUE 'RSN'.
           05  FILLER  PIC X(17)  VALUE '        QUANTITY'.
           05  FILLER  PIC X(17)  VALUE '     SETTLED QTY'.
           05  FILLER  PIC X(11)  VALUE 'SENT'.
           05  FILLER  PIC X(11)  VALUE 'LAST STAT'.
           05  FILLER  PIC X(04)  VALUE 'MSG'.
           05  FILLER  PIC X(04)  VALUE 'AGE'.
       01  WS-IN-LINE.
           05  IN-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  IN-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  IN-MT                   PIC X(03).
           05  FILLER                  PIC X(01).
           05  IN-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  IN-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  IN-SETTLE               PIC X(10).
           05  FILLER                  PIC X(01).
           05  IN-STATUS               PIC X(02).
           05  FILLER                  PIC X(01).
           05  IN-CODE                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  IN-REASON               PIC X(04).
           05  FILLER                  PIC X(01).
           05  IN-QTY                  PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  IN-SETTLED              PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  IN-SENT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  IN-LAST                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  IN-LAST-MSG             PIC X(03).
           05  FILLER                  PIC X(01).
           05  IN-AGE                  PIC ZZ9.
       01  WS-COUNT-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  CL-CODE                 PIC X(02).
           05  FILLER                  PIC X(02).
           05  CL-TEXT                 PIC X(30).
           05  CL-COUNT-1              PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  CL-COUNT-2              PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  CL-COUNT-3              PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(54).
       01  WS-COUNT-HEAD.
           05  CH-CC                   PIC X(01).
           05  FILLER                  PIC X(39).
           05  CH-TITLE-1              PIC X(14).
           05  CH-TITLE-2              PIC X(14).
           05  CH-TITLE-3              PIC X(14).
           05  FILLER                  PIC X(51).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  TL-LABEL                PIC X(40).
           05  TL-VALUE                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(76).
      *
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-EVENTS-PART    THRU 2000-EXIT
           PERFORM 3000-MASTER-PART    THRU 3000-EXIT
           PERFORM 4000-INVENTORY-PART THRU 4000-EXIT
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE ZERO                   TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
      * 1000 - INITIALIZE                                              *
      *================================================================*
       1000-INITIALIZE.
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           CLOSE DATECARD-FILE
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'INSTRUCTION STATUS REPORT STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM  THRU 1100-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
           PERFORM VARYING WS-EVT-SUB FROM 1 BY 1 UNTIL WS-EVT-SUB > 8
               MOVE ZERO TO WS-EVT-COUNT (WS-EVT-SUB)
                            WS-EVT-NOT-APPLIED (WS-EVT-SUB)
           END-PERFORM
           PERFORM VARYING WS-STS-SUB FROM 1 BY 1 UNTIL WS-STS-SUB > 10
               MOVE ZERO TO WS-STS-541 (WS-STS-SUB)
                            WS-STS-543 (WS-STS-SUB)
           END-PERFORM
      *
           OPEN INPUT STATIN-FILE
           IF NOT STATIN-OK
               MOVE 'STATIN'           TO AB-DDNAME
               MOVE WS-STATIN-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN INPUT SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP (1:10)    TO RPT-H1-RUN-DATE
           MOVE 'SWR210'               TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'SETTLEMENT INSTRUCTION STATUS - CUSTODIAN EVENTS'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-DATE            TO WS-DATE-IN
           PERFORM 8300-EDIT-DATE      THRU 8300-EXIT
           MOVE WS-DATE-EDIT           TO RPT-H2-BUS-DATE.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARM.
           READ PARMCARD
               AT END
                   SET WS-PARM-EOF     TO TRUE
                   GO TO 1100-EXIT
           END-READ
           IF PARM-CARD-REC (1:1) = '*'
           OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           IF WS-PARM-KEYWORD = 'DETAIL'
               MOVE WS-PARM-VALUE (1:6) TO WS-DETAIL-OPT
           ELSE
               DISPLAY 'SWR210 - UNKNOWN PARAMETER IGNORED: '
                       PARM-CARD-REC (1:40)
           END-IF.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - PART 1: TODAY'S EVENTS                                  *
      *================================================================*
       2000-EVENTS-PART.
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
           MOVE SPACES                 TO WS-PART-LINE
           MOVE '0'                    TO PT-CC
           MOVE 'PART 1 - CUSTODIAN STATUS AND CONFIRMATION EXCEPTIONS'
                                       TO PT-TEXT
           PERFORM 8150-WRITE-PART     THRU 8150-EXIT
           MOVE WS-EV-HEAD-1           TO WS-CUR-COL-HEAD
           WRITE RPT-RECORD FROM WS-CUR-COL-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
      *
           PERFORM 8000-READ-EVENT     THRU 8000-EXIT
           PERFORM UNTIL WS-END-OF-EVENTS
               PERFORM 2100-ONE-EVENT  THRU 2100-EXIT
               PERFORM 8000-READ-EVENT THRU 8000-EXIT
           END-PERFORM
      *
           MOVE SPACES                 TO WS-CUR-COL-HEAD
           IF RPT-LINE-COUNT + 14 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
           END-IF
           MOVE SPACES                 TO WS-COUNT-HEAD
           MOVE '0'                    TO CH-CC
           MOVE '        EVENTS'       TO CH-TITLE-1
           MOVE '   NOT APPLIED'       TO CH-TITLE-2
           WRITE RPT-RECORD FROM WS-COUNT-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
           PERFORM VARYING WS-EVT-SUB FROM 1 BY 1 UNTIL WS-EVT-SUB > 8
               MOVE SPACES             TO WS-COUNT-LINE
               MOVE ' '                TO CL-CC
               MOVE WS-EVT-CODE (WS-EVT-SUB) TO CL-CODE
               MOVE WS-EVT-TEXT (WS-EVT-SUB) TO CL-TEXT
               MOVE WS-EVT-COUNT (WS-EVT-SUB) TO CL-COUNT-1
               MOVE WS-EVT-NOT-APPLIED (WS-EVT-SUB) TO CL-COUNT-2
               MOVE WS-COUNT-LINE      TO WS-EV-LINE
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
           END-PERFORM
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE '0'                    TO TL-CC
           MOVE 'EVENTS RECEIVED TODAY' TO TL-LABEL
           MOVE WS-EVENTS-READ         TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE ' '                    TO TL-CC
           MOVE 'EXCEPTION EVENTS LISTED' TO TL-LABEL
           MOVE WS-EVENTS-LISTED       TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-ONE-EVENT.
           ADD 1                       TO WS-EVENTS-READ
           MOVE 'N'                    TO WS-LIST-SW
           MOVE SPACES                 TO WS-EXCEPT-TEXT
           SET EVT-IDX                 TO 1
           SEARCH WS-EVT-ENTRY
               AT END
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'EVENT CODE UNKNOWN' TO WS-EXCEPT-TEXT
               WHEN WS-EVT-CODE (EVT-IDX) = SWS-EVENT
                   SET WS-EVT-SUB      TO EVT-IDX
                   ADD 1               TO WS-EVT-COUNT (WS-EVT-SUB)
                   IF SWS-APPLIED-FLAG = 'N'
                       ADD 1 TO WS-EVT-NOT-APPLIED (WS-EVT-SUB)
                   END-IF
           END-SEARCH
           EVALUATE TRUE
               WHEN SWS-EV-UNKNOWN-REF
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'NOT SENT BY MSEC'  TO WS-EXCEPT-TEXT
               WHEN SWS-EV-REJECTED
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'REPAIR AND RESEND' TO WS-EXCEPT-TEXT
               WHEN SWS-EV-UNMATCHED
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'CHASE COUNTERPARTY' TO WS-EXCEPT-TEXT
               WHEN SWS-EV-PARTIAL
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'PARTIAL SETTLEMENT' TO WS-EXCEPT-TEXT
               WHEN SWS-APPLIED-FLAG = 'N'
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'INSTRUCTION CLOSED' TO WS-EXCEPT-TEXT
               WHEN SWS-EV-PENDING
                AND SWS-REASON-CODE NOT = SPACES
                   SET WS-LIST-IT      TO TRUE
                   MOVE 'PENDING WITH REASON' TO WS-EXCEPT-TEXT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF NOT WS-LIST-IT
               GO TO 2100-EXIT
           END-IF
           ADD 1                       TO WS-EVENTS-LISTED
           MOVE SPACES                 TO WS-EV-LINE
           MOVE ' '                    TO EV-CC
           MOVE SWS-MSG-SEQ            TO EV-SEQ
           MOVE SWS-MSG-TYPE           TO EV-MT
           MOVE SWS-RELATED-REF        TO EV-RELA
           MOVE SWS-CUST-REF           TO EV-CUST-REF
           MOVE SWS-EVENT              TO EV-EVENT
           MOVE SWS-STATUS-CODE        TO EV-STATUS
           MOVE SWS-REASON-CODE        TO EV-REASON
           MOVE SWS-QTY                TO EV-QTY
           MOVE SWS-AMOUNT             TO EV-AMT
           IF SWS-EFF-DATE > ZERO
               MOVE SWS-EFF-DATE       TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE  THRU 8300-EXIT
               MOVE WS-DATE-EDIT       TO EV-EFF-DATE
           END-IF
           MOVE SWS-APPLIED-FLAG       TO EV-APPLIED
           MOVE WS-EXCEPT-TEXT         TO EV-EXCEPT
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT.
       2100-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - PART 2: OPEN INSTRUCTIONS NEEDING ATTENTION             *
      *================================================================*
       3000-MASTER-PART.
           MOVE 'SETTLEMENT INSTRUCTION STATUS - OPEN INSTRUCTIONS'
                                       TO RPT-H2-TITLE
           MOVE SPACES                 TO WS-CUR-COL-HEAD
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
           MOVE SPACES                 TO WS-PART-LINE
           MOVE '0'                    TO PT-CC
           IF WS-DETAIL-ALL
               MOVE 'PART 2 - ALL OPEN INSTRUCTIONS' TO PT-TEXT
           ELSE
               MOVE 'PART 2 - OPEN INSTRUCTIONS NEEDING ATTENTION'
                                       TO PT-TEXT
           END-IF
           PERFORM 8150-WRITE-PART     THRU 8150-EXIT
           MOVE WS-IN-HEAD-1           TO WS-CUR-COL-HEAD
           WRITE RPT-RECORD FROM WS-CUR-COL-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
      *
           PERFORM 8050-READ-INSTR     THRU 8050-EXIT
           PERFORM UNTIL WS-END-OF-MASTER
               PERFORM 3100-ONE-INSTRUCTION THRU 3100-EXIT
               PERFORM 8050-READ-INSTR THRU 8050-EXIT
           END-PERFORM.
       3000-EXIT.
           EXIT.
      *
       3100-ONE-INSTRUCTION.
           ADD 1                       TO WS-ROWS-READ
           IF SWI-SENDER-REF = WS-SEQ-CTL-KEY
               MOVE SWI-MSG-SEQ        TO WS-LAST-SEQ
               GO TO 3100-EXIT
           END-IF
           IF SWI-FUNCTION = 'CANC'
               ADD 1                   TO WS-CANC-ROWS
               GO TO 3100-EXIT
           END-IF
           ADD 1                       TO WS-NEWM-ROWS
           SET STS-IDX                 TO 1
           SEARCH WS-STS-ENTRY
               AT END
                   SET WS-STS-SUB      TO 10
               WHEN WS-STS-CODE (STS-IDX) = SWI-STATUS
                   SET WS-STS-SUB      TO STS-IDX
           END-SEARCH
           IF SWI-MSG-TYPE = '541'
               ADD 1                   TO WS-STS-541 (WS-STS-SUB)
           ELSE
               ADD 1                   TO WS-STS-543 (WS-STS-SUB)
           END-IF
      *
           IF SWI-SETTLED OR SWI-CANCELLED
               GO TO 3100-EXIT
           END-IF
           MOVE 'N'                    TO WS-LIST-SW
           EVALUATE TRUE
               WHEN WS-DETAIL-ALL
                   SET WS-LIST-IT      TO TRUE
               WHEN SWI-UNMATCHED OR SWI-REJECTED OR SWI-PENDING
               WHEN SWI-FAILED OR SWI-PARTIAL
                   SET WS-LIST-IT      TO TRUE
               WHEN SWI-SENT AND SWI-SENT-DATE < DC-BUS-DATE
                   SET WS-LIST-IT      TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF SWI-SENT AND SWI-SENT-DATE < DC-BUS-DATE
               ADD 1                   TO WS-NO-RESPONSE
           END-IF
           IF NOT WS-LIST-IT
               GO TO 3100-EXIT
           END-IF
           ADD 1                       TO WS-OPEN-LISTED
           MOVE SPACES                 TO WS-IN-LINE
           MOVE ' '                    TO IN-CC
           MOVE SWI-SENDER-REF         TO IN-REF
           MOVE SWI-MSG-TYPE           TO IN-MT
           MOVE SWI-ACCT-NO            TO IN-ACCT
           MOVE SWI-CUSIP              TO IN-CUSIP
           MOVE SWI-SETTLE-DATE        TO WS-DATE-IN
           PERFORM 8300-EDIT-DATE      THRU 8300-EXIT
           MOVE WS-DATE-EDIT           TO IN-SETTLE
           MOVE SWI-STATUS             TO IN-STATUS
           MOVE SWI-STATUS-CODE        TO IN-CODE
           MOVE SWI-REASON-CODE        TO IN-REASON
           MOVE SWI-QTY                TO IN-QTY
           MOVE SWI-SETTLED-QTY        TO IN-SETTLED
           MOVE SWI-SENT-DATE          TO WS-DATE-IN
           PERFORM 8300-EDIT-DATE      THRU 8300-EXIT
           MOVE WS-DATE-EDIT           TO IN-SENT
           MOVE SWI-LAST-STATUS-DATE   TO WS-DATE-IN
           PERFORM 8300-EDIT-DATE      THRU 8300-EXIT
           MOVE WS-DATE-EDIT           TO IN-LAST
           MOVE SWI-LAST-STATUS-MSG    TO IN-LAST-MSG
           MOVE SWI-FAIL-AGE           TO IN-AGE
           MOVE WS-IN-LINE             TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT.
       3100-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - PART 3: INVENTORY BY STATUS                             *
      *================================================================*
       4000-INVENTORY-PART.
           MOVE 'SETTLEMENT INSTRUCTION STATUS - INVENTORY'
                                       TO RPT-H2-TITLE
           MOVE SPACES                 TO WS-CUR-COL-HEAD
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
           MOVE SPACES                 TO WS-PART-LINE
           MOVE '0'                    TO PT-CC
           MOVE 'PART 3 - INSTRUCTION MASTER BY STATUS' TO PT-TEXT
           PERFORM 8150-WRITE-PART     THRU 8150-EXIT
           MOVE SPACES                 TO WS-COUNT-HEAD
           MOVE '0'                    TO CH-CC
           MOVE '   MT541 (RVP)'       TO CH-TITLE-1
           MOVE '   MT543 (DVP)'       TO CH-TITLE-2
           MOVE '         TOTAL'       TO CH-TITLE-3
           WRITE RPT-RECORD FROM WS-COUNT-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
           PERFORM VARYING WS-STS-SUB FROM 1 BY 1 UNTIL WS-STS-SUB > 10
               MOVE SPACES             TO WS-COUNT-LINE
               MOVE ' '                TO CL-CC
               MOVE WS-STS-CODE (WS-STS-SUB) TO CL-CODE
               MOVE WS-STS-TEXT (WS-STS-SUB) TO CL-TEXT
               MOVE WS-STS-541 (WS-STS-SUB)  TO CL-COUNT-1
               MOVE WS-STS-543 (WS-STS-SUB)  TO CL-COUNT-2
               COMPUTE CL-COUNT-3 = WS-STS-541 (WS-STS-SUB)
                                  + WS-STS-543 (WS-STS-SUB)
               MOVE WS-COUNT-LINE      TO WS-EV-LINE
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
           END-PERFORM
      *
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE '0'                    TO TL-CC
           MOVE 'MASTER ROWS READ'     TO TL-LABEL
           MOVE WS-ROWS-READ           TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE ' '                    TO TL-CC
           MOVE 'NEW INSTRUCTIONS (NEWM)' TO TL-LABEL
           MOVE WS-NEWM-ROWS           TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           MOVE 'CANCELLATIONS SENT (CANC)' TO TL-LABEL
           MOVE WS-CANC-ROWS           TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           MOVE 'OPEN INSTRUCTIONS LISTED' TO TL-LABEL
           MOVE WS-OPEN-LISTED         TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           MOVE 'SENT BEFORE TODAY, NO RESPONSE' TO TL-LABEL
           MOVE WS-NO-RESPONSE         TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           MOVE 'LAST MESSAGE SEQUENCE SENT' TO TL-LABEL
           MOVE WS-LAST-SEQ            TO TL-VALUE
           MOVE WS-TOTAL-LINE          TO WS-EV-LINE
           PERFORM 8100-WRITE-LINE     THRU 8100-EXIT
           WRITE RPT-RECORD FROM RPT-END-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O                                                     *
      *================================================================*
       8000-READ-EVENT.
           READ STATIN-FILE INTO SWS-STATUS-REC
           EVALUATE TRUE
               WHEN STATIN-OK
                   CONTINUE
               WHEN STATIN-EOF
                   SET WS-END-OF-EVENTS TO TRUE
               WHEN OTHER
                   MOVE 'STATIN'       TO AB-DDNAME
                   MOVE WS-STATIN-FS   TO AB-FILE-STATUS
                   MOVE '8000-READ-EVENT' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
       8050-READ-INSTR.
           READ SWINSTR-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   CONTINUE
               WHEN SWINSTR-EOF
                   SET WS-END-OF-MASTER TO TRUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE '8050-READ-INSTR' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8050-EXIT.
           EXIT.
      *
       8100-WRITE-LINE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-EV-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *
       8150-WRITE-PART.
           WRITE RPT-RECORD FROM WS-PART-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT.
       8150-EXIT.
           EXIT.
      *
       8200-HEADINGS.
           ADD 1                       TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT         TO RPT-H1-PAGE
           WRITE RPT-RECORD FROM RPT-HEADING-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM RPT-HEADING-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           MOVE 3                      TO RPT-LINE-COUNT
           IF WS-CUR-COL-HEAD NOT = SPACES
               WRITE RPT-RECORD FROM WS-CUR-COL-HEAD
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2                   TO RPT-LINE-COUNT
           END-IF.
       8200-EXIT.
           EXIT.
      *
       8300-EDIT-DATE.
           MOVE WS-DI-CCYY             TO WS-DE-CCYY
           MOVE WS-DI-MM               TO WS-DE-MM
           MOVE WS-DI-DD               TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *
       8900-CHECK-WRITE.
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE '8900-CHECK-WRITE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       8900-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE STATIN-FILE SWINSTR-FILE RPTFILE
           IF NOT STATIN-OK OR NOT SWINSTR-OK OR NOT RPTFILE-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               MOVE 'CLOSE ERROR'      TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM CT-STAGE
           MOVE 'STATEVT-IN'           TO CT-COUNTER-NAME
           MOVE WS-EVENTS-READ         TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'INSTR-READ'           TO CT-COUNTER-NAME
           MOVE WS-ROWS-READ           TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE 'INSTRUCTION STATUS REPORT ENDED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE WS-EVENTS-READ         TO WS-DISP-COUNT
           DISPLAY 'SWR210 EVENTS READ          : ' WS-DISP-COUNT
           MOVE WS-EVENTS-LISTED       TO WS-DISP-COUNT
           DISPLAY 'SWR210 EXCEPTION EVENTS     : ' WS-DISP-COUNT
           MOVE WS-ROWS-READ           TO WS-DISP-COUNT
           DISPLAY 'SWR210 MASTER ROWS READ     : ' WS-DISP-COUNT
           MOVE WS-OPEN-LISTED         TO WS-DISP-COUNT
           DISPLAY 'SWR210 OPEN INSTR LISTED    : ' WS-DISP-COUNT
           DISPLAY 'SWR210 PAGES                : ' RPT-PAGE-COUNT.
       9000-EXIT.
           EXIT.
      *
       9110-CALL-CMU080.
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9110-CALL-CMU080' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME    TO AB-KEY
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF.
       9110-EXIT.
           EXIT.
      *
       9910-OPEN-ERROR.
           MOVE 1001                   TO AB-ABEND-CODE
           MOVE '1000-INITIALIZE'      TO AB-PARAGRAPH
           STRING 'OPEN FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9910-EXIT.
           EXIT.
      *
       9920-IO-ERROR.
           MOVE 1002                   TO AB-ABEND-CODE
           STRING 'I/O ERROR ON ' AB-DDNAME ' STATUS '
                  AB-FILE-STATUS
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9920-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'SWR210 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWR210 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
