       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRB300.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  05/13/1991.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRB300                                            *
      * TITLE      : REGULATORY TRADING ACTIVITY REQUEST ('BLUE SHEET')*
      * JOB        : MSRRX300  STEP010  (AD HOC - ON REQUEST FROM      *
      *              COMPLIANCE, NOT SCHEDULED)                        *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   ANSWERS A REGULATOR'S REQUEST FOR ALL TRADES IN ONE SECURITY *
      *   OVER A DATE RANGE, WITH THE ACCOUNT OF EACH TRADE.  ONE      *
      *   REQUEST PER CONTROL CARD, UP TO 20 CARDS PER RUN:            *
      *                                                                *
      *     REQ=<ID10> CUSIP=<CUSIP9> FROM=<CCYYMMDD> TO=<CCYYMMDD>    *
      *                                                                *
      *   THE TRADE HISTORY FILE IS BROWSED FROM THE START FOR EVERY   *
      *   REQUEST (IT IS KEYED BY TRADE ID ONLY).  EVERY TRADE IN THE  *
      *   CUSIP WITH A TRADE DATE IN THE RANGE IS ANSWERED, WHATEVER   *
      *   ITS STATUS (ACTIVE, CANCELLED, CORRECTED).                   *
      *                                                                *
      *   RESPONSE RECORDS (RRBLUE):                                   *
      *     H  ONE PER REQUEST  TRADE-DATE = FROM DATE,                *
      *                         TRADE-ID(1:8) = TO DATE,               *
      *                         ACCT-NAME = REQUEST DESCRIPTION        *
      *     D  ONE PER TRADE                                           *
      *     T  ONE PER REQUEST  QTY / NET-AMOUNT = TOTALS,             *
      *                         TRADE-ID = TRADE COUNT (16 DIGITS)     *
      *   AND A PRINTED LISTING FOR THE COMPLIANCE FILE.               *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     REQUEST CARDS  RRP300A OR INSTREAM          *
      *          TRDHIST   MSEC.PROD.TC.TRDHIST.KSDS     (TCTRDHS)     *
      *          ACCTMAST  MSEC.PROD.CM.ACCTMAST.KSDS    (CMACCT)      *
      * OUTPUT : BLUEOUT   MSEC.PROD.RR.BLUESHT(+1)      (RRBLUE)      *
      *          RPTFILE   LISTING, FB 133 ASA                         *
      * CALLS  : CMU010 (VALD) CMU050 CMU060 CMU080 CMASM02            *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 A REQUEST CARD WAS REJECTED OR AN     *
      *               ACCOUNT IS NOT ON FILE  8 NO VALID REQUEST       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1991-05-13 RJK            ORIGINAL - TAPE FOR THE EXCHANGE     *
      * 1993-02-08 DWB            MULTIPLE REQUESTS PER RUN            *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD                 *
      * 2006-06-12 KAP  CHG15008  READ TRDHIST KSDS (WAS TRADE TAPES)  *
      * 2014-03-24 SPA  CHG26615  PRINTED LISTING FOR COMPLIANCE FILE  *
      * 2016-10-03 SPA  CHG30112  DTC PARTICIPANT AS CONTRA            *
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
           SELECT TRDHIST-FILE  ASSIGN TO TRDHIST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD KEY IS TH-TRADE-ID
                                FILE STATUS IS WS-TRDHIST-FS.
           SELECT ACCTMAST-FILE ASSIGN TO ACCTMAST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS RANDOM
                                RECORD KEY IS ACCT-NO
                                FILE STATUS IS WS-ACCTMAST-FS.
           SELECT BLUEOUT-FILE  ASSIGN TO BLUEOUT
                                FILE STATUS IS WS-BLUEOUT-FS.
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
       FD  TRDHIST-FILE.
           COPY TCTRDHS.
       FD  ACCTMAST-FILE.
           COPY CMACCT.
       FD  BLUEOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BLUEOUT-REC                 PIC X(300).
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRB300'.
       01  WS-FILE-STATUS-AREA.
           05  WS-PARMCARD-FS          PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-DATECARD-FS          PIC X(02)  VALUE '00'.
           05  WS-TRDHIST-FS           PIC X(02)  VALUE '00'.
               88  TRDHIST-OK                     VALUE '00'.
               88  TRDHIST-EOF                    VALUE '10'.
               88  TRDHIST-NOTFND                 VALUE '23'.
           05  WS-ACCTMAST-FS          PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-BLUEOUT-FS           PIC X(02)  VALUE '00'.
           05  WS-RPTFILE-FS           PIC X(02)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                    VALUE 'Y'.
           05  WS-HIST-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-HIST-EOF                    VALUE 'Y'.
           05  WS-CARD-OK-SW           PIC X(01)  VALUE 'Y'.
               88  WS-CARD-OK                     VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *
      *---- REQUEST CARD WORK ----------------------------------------*
       01  WS-TOKENS.
           05  WS-TOKEN                PIC X(20)  OCCURS 6 TIMES.
       01  WS-TOKEN-COUNT              PIC S9(04) COMP.
       01  WS-TOK-SUB                  PIC S9(04) COMP.
       01  WS-TOK-KEYWORD              PIC X(08).
       01  WS-TOK-VALUE                PIC X(12).
       01  WS-CARD-REQ.
           05  WS-CR-REQ-ID            PIC X(10).
           05  WS-CR-CUSIP             PIC X(12).
           05  WS-CR-FROM-X            PIC X(12).
           05  WS-CR-TO-X              PIC X(12).
       01  WS-CR-FROM                  PIC 9(08).
       01  WS-CR-TO                    PIC 9(08).
       01  WS-REJECT-REASON            PIC X(40).
      *
      *---- REQUEST TABLE --------------------------------------------*
       01  WS-REQUEST-TABLE.
           05  WS-RQ-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-RQ-ENTRY             OCCURS 20 TIMES.
               10  WS-RQ-REQ-ID        PIC X(10).
               10  WS-RQ-CUSIP         PIC X(09).
               10  WS-RQ-FROM          PIC 9(08).
               10  WS-RQ-TO            PIC 9(08).
               10  WS-RQ-TRADES        PIC S9(07)       COMP-3.
               10  WS-RQ-QTY           PIC S9(13)V9(04) COMP-3.
               10  WS-RQ-NET           PIC S9(15)V99    COMP-3.
       01  WS-RQ-MAX                   PIC S9(04) COMP  VALUE 20.
       01  WS-RQ-SUB                   PIC S9(04) COMP.
      *
      *---- ACCOUNT WORK ---------------------------------------------*
       01  WS-ACCT-NAME                PIC X(40).
       01  WS-ACCT-TYPE                PIC X(02).
       01  WS-ACCT-PARTICIPANT         PIC X(04).
      *
      *---- COUNTERS -------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-CARDS-READ           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CARDS-REJECTED       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-HIST-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HIST-SELECTED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-NOT-FOUND       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-BLUE-OUT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOTAL-NET            PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-TOTAL-QTY            PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
       01  WS-TRADE-COUNT-X            PIC 9(16).
      *
      *---- REPORT LINES ---------------------------------------------*
       01  WS-REQ-HEAD-1.
           05  RH-CC                   PIC X(01).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  FILLER                  PIC X(10)  VALUE 'REQUEST:'.
           05  RH-REQ-ID               PIC X(10).
           05  FILLER                  PIC X(10)  VALUE '   CUSIP:'.
           05  RH-CUSIP                PIC X(09).
           05  FILLER                  PIC X(16)  VALUE
               '   TRADE DATES:'.
           05  RH-FROM                 PIC 9(08).
           05  FILLER                  PIC X(04)  VALUE ' TO '.
           05  RH-TO                   PIC 9(08).
           05  FILLER                  PIC X(56)  VALUE SPACES.
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(10)  VALUE ' TRD DATE'.
           05  FILLER  PIC X(18)  VALUE ' TRADE ID'.
           05  FILLER  PIC X(04)  VALUE 'SD'.
           05  FILLER  PIC X(19)  VALUE '          QUANTITY'.
           05  FILLER  PIC X(17)  VALUE '          PRICE'.
           05  FILLER  PIC X(21)  VALUE '       NET AMOUNT'.
           05  FILLER  PIC X(12)  VALUE ' ACCOUNT'.
           05  FILLER  PIC X(31)  VALUE 'NAME / TYPE / PART / STATUS'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-TRADE-DATE           PIC 9(08).
           05  FILLER                  PIC X(01).
           05  DL-TRADE-ID             PIC X(16).
           05  FILLER                  PIC X(02).
           05  DL-SIDE                 PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-PRICE                PIC ZZZ,ZZ9.999999.
           05  FILLER                  PIC X(01).
           05  DL-NET                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(02).
           05  DL-NAME                 PIC X(24).
           05  FILLER                  PIC X(01).
           05  DL-ACCT-TYPE            PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-PART                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(02).
           05  FILLER                  PIC X(01).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(11)  VALUE SPACES.
           05  FILLER                  PIC X(16)  VALUE
               'REQUEST TOTAL - '.
           05  TL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(08)  VALUE ' TRADES '.
           05  TL-QTY                  PIC ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(08)  VALUE SPACES.
           05  TL-NET                  PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(43)  VALUE SPACES.
       01  WS-MSG-LINE.
           05  ML-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  ML-TEXT                 PIC X(131).
      *
           COPY RRBLUE.
           COPY CMDATEW.
           COPY CMDTLNK.
           COPY CMRPTHD.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT.
           PERFORM 1100-READ-REQUEST   THRU 1100-EXIT
               UNTIL WS-PARM-EOF.
           IF WS-RQ-USED = ZERO
               MOVE 8                  TO WS-RETURN-CODE
               MOVE SPACES             TO WS-MSG-LINE
               MOVE '0'                TO ML-CC
               MOVE '*** NO VALID REQUEST CARD - NOTHING ANSWERED ***'
                                       TO ML-TEXT
               PERFORM 8200-PRINT-MESSAGE THRU 8200-EXIT
           END-IF
           PERFORM 2000-ANSWER-REQUEST THRU 2000-EXIT
               VARYING WS-RQ-SUB FROM 1 BY 1
               UNTIL WS-RQ-SUB > WS-RQ-USED.
           PERFORM 9000-TERMINATE      THRU 9000-EXIT.
           MOVE WS-RETURN-CODE         TO RETURN-CODE.
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
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'TRADING ACTIVITY REQUEST STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS NOT = '00'
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           OPEN INPUT TRDHIST-FILE
           IF WS-TRDHIST-FS NOT = '00'
               MOVE 'TRDHIST'          TO AB-DDNAME
               MOVE WS-TRDHIST-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           OPEN INPUT ACCTMAST-FILE
           IF WS-ACCTMAST-FS NOT = '00'
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           OPEN OUTPUT BLUEOUT-FILE
           IF WS-BLUEOUT-FS NOT = '00'
               MOVE 'BLUEOUT'          TO AB-DDNAME
               MOVE WS-BLUEOUT-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           OPEN OUTPUT RPTFILE
           IF WS-RPTFILE-FS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
      *
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP (1:10)    TO RPT-H1-RUN-DATE
           MOVE WS-PROGRAM-ID          TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'REGULATORY TRADING ACTIVITY REQUEST - RESPONSE'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-DATE            TO RPT-H2-BUS-DATE
           PERFORM 8300-HEADINGS       THRU 8300-EXIT.
       1000-EXIT.
           EXIT.
      *
      *================================================================*
      * 1100 - ONE REQUEST CARD                                        *
      *   REQ=<ID10> CUSIP=<9> FROM=<CCYYMMDD> TO=<CCYYMMDD>           *
      *   KEYWORDS IN ANY ORDER, SEPARATED BY ONE OR MORE BLANKS.      *
      *================================================================*
       1100-READ-REQUEST.
           READ PARMCARD
           IF PARMCARD-EOF
               MOVE 'Y'                TO WS-PARM-EOF-SW
               GO TO 1100-EXIT
           END-IF
           IF NOT PARMCARD-OK
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-FS     TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'READ FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           IF PARM-CARD-REC(1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           ADD 1                       TO WS-CARDS-READ
           MOVE 'Y'                    TO WS-CARD-OK-SW
           MOVE SPACES                 TO WS-TOKENS WS-CARD-REQ
                                          WS-REJECT-REASON
           MOVE ZERO                   TO WS-TOKEN-COUNT
           UNSTRING PARM-CARD-REC DELIMITED BY ALL SPACE
               INTO WS-TOKEN(1) WS-TOKEN(2) WS-TOKEN(3)
                    WS-TOKEN(4) WS-TOKEN(5) WS-TOKEN(6)
               TALLYING IN WS-TOKEN-COUNT
           END-UNSTRING
           PERFORM 1110-SPLIT-TOKEN    THRU 1110-EXIT
               VARYING WS-TOK-SUB FROM 1 BY 1
               UNTIL WS-TOK-SUB > 6
           PERFORM 1120-EDIT-REQUEST   THRU 1120-EXIT
           IF WS-CARD-OK
               PERFORM 1130-ADD-REQUEST THRU 1130-EXIT
           ELSE
               PERFORM 1140-REJECT-CARD THRU 1140-EXIT
           END-IF.
       1100-EXIT.
           EXIT.
      *
       1110-SPLIT-TOKEN.
           IF WS-TOKEN(WS-TOK-SUB) = SPACES
               GO TO 1110-EXIT
           END-IF
           MOVE SPACES                 TO WS-TOK-KEYWORD WS-TOK-VALUE
           UNSTRING WS-TOKEN(WS-TOK-SUB) DELIMITED BY '='
               INTO WS-TOK-KEYWORD WS-TOK-VALUE
           END-UNSTRING
           EVALUATE WS-TOK-KEYWORD
               WHEN 'REQ'
                   MOVE WS-TOK-VALUE   TO WS-CR-REQ-ID
               WHEN 'CUSIP'
                   MOVE WS-TOK-VALUE   TO WS-CR-CUSIP
               WHEN 'FROM'
                   MOVE WS-TOK-VALUE   TO WS-CR-FROM-X
               WHEN 'TO'
                   MOVE WS-TOK-VALUE   TO WS-CR-TO-X
               WHEN OTHER
                   MOVE 'N'            TO WS-CARD-OK-SW
                   MOVE 'UNKNOWN KEYWORD' TO WS-REJECT-REASON
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *
       1120-EDIT-REQUEST.
           IF NOT WS-CARD-OK
               GO TO 1120-EXIT
           END-IF
           IF WS-CR-REQ-ID = SPACES
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'REQ= MISSING'     TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           IF WS-CR-CUSIP(1:9) = SPACES OR WS-CR-CUSIP(9:1) = SPACE
           OR WS-CR-CUSIP(10:3) NOT = SPACES
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'CUSIP= NOT 9 CHARACTERS' TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           IF WS-CR-FROM-X(1:8) NOT NUMERIC
           OR WS-CR-TO-X(1:8) NOT NUMERIC
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'FROM= / TO= NOT CCYYMMDD' TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           MOVE WS-CR-FROM-X(1:8)      TO WS-CR-FROM
           MOVE WS-CR-TO-X(1:8)        TO WS-CR-TO
      *    VALID CALENDAR DATES
           MOVE 'VALD'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE WS-CR-FROM             TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'FROM= NOT A VALID DATE' TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           MOVE 'VALD'                 TO DT-FUNCTION
           MOVE WS-CR-TO               TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'TO= NOT A VALID DATE' TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           IF WS-CR-FROM > WS-CR-TO
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'FROM= AFTER TO='  TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           IF WS-CR-TO > DC-BUS-DATE
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'TO= AFTER THE BUSINESS DATE' TO WS-REJECT-REASON
               GO TO 1120-EXIT
           END-IF
           IF WS-RQ-USED NOT < WS-RQ-MAX
               MOVE 'N'                TO WS-CARD-OK-SW
               MOVE 'MORE THAN 20 REQUESTS - RUN AGAIN' TO
                                          WS-REJECT-REASON
           END-IF.
       1120-EXIT.
           EXIT.
      *
       1130-ADD-REQUEST.
           ADD 1                       TO WS-RQ-USED
           MOVE WS-CR-REQ-ID           TO WS-RQ-REQ-ID(WS-RQ-USED)
           MOVE WS-CR-CUSIP            TO WS-RQ-CUSIP(WS-RQ-USED)
           MOVE WS-CR-FROM             TO WS-RQ-FROM(WS-RQ-USED)
           MOVE WS-CR-TO               TO WS-RQ-TO(WS-RQ-USED)
           MOVE ZERO                   TO WS-RQ-TRADES(WS-RQ-USED)
                                          WS-RQ-QTY(WS-RQ-USED)
                                          WS-RQ-NET(WS-RQ-USED)
           DISPLAY 'RRB300 REQUEST ' WS-CR-REQ-ID ' CUSIP '
                   WS-CR-CUSIP(1:9) ' ' WS-CR-FROM ' - ' WS-CR-TO.
       1130-EXIT.
           EXIT.
      *
       1140-REJECT-CARD.
           ADD 1                       TO WS-CARDS-REJECTED
           IF WS-RETURN-CODE < 4
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
           DISPLAY 'RRB300 W - CARD REJECTED: ' WS-REJECT-REASON
           DISPLAY 'RRB300     ' PARM-CARD-REC(1:72)
           MOVE SPACES                 TO WS-MSG-LINE
           MOVE '0'                    TO ML-CC
           STRING '*** REQUEST CARD REJECTED - ' DELIMITED BY SIZE
                  WS-REJECT-REASON     DELIMITED BY '  '
                  ': '                 DELIMITED BY SIZE
                  PARM-CARD-REC(1:72)  DELIMITED BY SIZE
               INTO ML-TEXT
           END-STRING
           PERFORM 8200-PRINT-MESSAGE  THRU 8200-EXIT
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'REQREJ'               TO AU-EVENT
           MOVE 'W'                    TO AU-SEVERITY
           MOVE PARM-CARD-REC(1:40)    TO AU-KEY
           MOVE WS-REJECT-REASON       TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       1140-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - ANSWER ONE REQUEST: FULL BROWSE OF TRADE HISTORY        *
      *================================================================*
       2000-ANSWER-REQUEST.
           PERFORM 2100-REQUEST-HEADER THRU 2100-EXIT
           MOVE 'N'                    TO WS-HIST-EOF-SW
           MOVE LOW-VALUES             TO TH-TRADE-ID
           START TRDHIST-FILE KEY IS NOT LESS THAN TH-TRADE-ID
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   CONTINUE
               WHEN TRDHIST-NOTFND
      *            EMPTY HISTORY FILE
                   MOVE 'Y'            TO WS-HIST-EOF-SW
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE 1002           TO AB-ABEND-CODE
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE
           PERFORM 8000-READ-HISTORY   THRU 8000-EXIT
           PERFORM 2200-CHECK-TRADE    THRU 2200-EXIT
               UNTIL WS-HIST-EOF
           PERFORM 2900-REQUEST-TRAILER THRU 2900-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-REQUEST-HEADER.
           MOVE SPACES                 TO RRB-BLUE-REC
           MOVE WS-RQ-REQ-ID(WS-RQ-SUB) TO RRB-REQUEST-ID
           MOVE 'H'                    TO RRB-REC-TYPE
           MOVE WS-RQ-CUSIP(WS-RQ-SUB) TO RRB-CUSIP
           MOVE WS-RQ-FROM(WS-RQ-SUB)  TO RRB-TRADE-DATE
           MOVE WS-RQ-TO(WS-RQ-SUB)    TO RRB-TRADE-ID(1:8)
           MOVE ZERO                   TO RRB-QTY RRB-PRICE
                                          RRB-NET-AMOUNT
           STRING 'MERIDIAN SECURITIES LLC - REQ ' DELIMITED BY SIZE
                  WS-RQ-REQ-ID(WS-RQ-SUB) DELIMITED BY SPACE
               INTO RRB-ACCT-NAME
           END-STRING
           PERFORM 8100-WRITE-BLUE     THRU 8100-EXIT
      *    ---- LISTING ---------------------------------------------
           IF RPT-LINE-COUNT + 6 > RPT-LINES-PER-PAGE
               PERFORM 8300-HEADINGS   THRU 8300-EXIT
           END-IF
           MOVE '-'                    TO RH-CC
           MOVE WS-RQ-REQ-ID(WS-RQ-SUB) TO RH-REQ-ID
           MOVE WS-RQ-CUSIP(WS-RQ-SUB) TO RH-CUSIP
           MOVE WS-RQ-FROM(WS-RQ-SUB)  TO RH-FROM
           MOVE WS-RQ-TO(WS-RQ-SUB)    TO RH-TO
           WRITE RPT-RECORD FROM WS-REQ-HEAD-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM WS-COL-HEAD-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 5                       TO RPT-LINE-COUNT.
       2100-EXIT.
           EXIT.
      *
       2200-CHECK-TRADE.
           ADD 1                       TO WS-HIST-READ
           IF TH-CUSIP NOT = WS-RQ-CUSIP(WS-RQ-SUB)
           OR TH-TRADE-DATE < WS-RQ-FROM(WS-RQ-SUB)
           OR TH-TRADE-DATE > WS-RQ-TO(WS-RQ-SUB)
               GO TO 2200-NEXT
           END-IF
           ADD 1                       TO WS-HIST-SELECTED
           PERFORM 2300-GET-ACCOUNT    THRU 2300-EXIT
           MOVE SPACES                 TO RRB-BLUE-REC
           MOVE WS-RQ-REQ-ID(WS-RQ-SUB) TO RRB-REQUEST-ID
           MOVE 'D'                    TO RRB-REC-TYPE
           MOVE TH-CUSIP               TO RRB-CUSIP
           MOVE TH-TRADE-DATE          TO RRB-TRADE-DATE
           MOVE TH-TRADE-ID            TO RRB-TRADE-ID
           MOVE TH-SIDE                TO RRB-SIDE
           MOVE TH-QTY                 TO RRB-QTY
           MOVE TH-PRICE               TO RRB-PRICE
           MOVE TH-NET-AMOUNT          TO RRB-NET-AMOUNT
           MOVE TH-ACCT-NO             TO RRB-ACCT-NO
           MOVE WS-ACCT-NAME           TO RRB-ACCT-NAME
           MOVE WS-ACCT-TYPE           TO RRB-ACCT-TYPE
           MOVE TH-STATUS              TO RRB-STATUS
           MOVE WS-ACCT-PARTICIPANT    TO RRB-CONTRA
           PERFORM 8100-WRITE-BLUE     THRU 8100-EXIT
           ADD 1                       TO WS-RQ-TRADES(WS-RQ-SUB)
           ADD TH-QTY                  TO WS-RQ-QTY(WS-RQ-SUB)
           ADD TH-NET-AMOUNT           TO WS-RQ-NET(WS-RQ-SUB)
      *    ---- LISTING ---------------------------------------------
           MOVE SPACES                 TO WS-DETAIL-LINE
           MOVE ' '                    TO DL-CC
           MOVE TH-TRADE-DATE          TO DL-TRADE-DATE
           MOVE TH-TRADE-ID            TO DL-TRADE-ID
           MOVE TH-SIDE                TO DL-SIDE
           MOVE TH-QTY                 TO DL-QTY
           MOVE TH-PRICE               TO DL-PRICE
           MOVE TH-NET-AMOUNT          TO DL-NET
           MOVE TH-ACCT-NO             TO DL-ACCT
           MOVE WS-ACCT-NAME           TO DL-NAME
           MOVE WS-ACCT-TYPE           TO DL-ACCT-TYPE
           MOVE WS-ACCT-PARTICIPANT    TO DL-PART
           MOVE TH-STATUS              TO DL-STATUS
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8300-HEADINGS   THRU 8300-EXIT
               WRITE RPT-RECORD FROM WS-COL-HEAD-1
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2                   TO RPT-LINE-COUNT
           END-IF
           WRITE RPT-RECORD FROM WS-DETAIL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       2200-NEXT.
           PERFORM 8000-READ-HISTORY   THRU 8000-EXIT.
       2200-EXIT.
           EXIT.
      *
       2300-GET-ACCOUNT.
           MOVE TH-ACCT-NO             TO ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE ACCT-NAME      TO WS-ACCT-NAME
                   MOVE ACCT-TYPE      TO WS-ACCT-TYPE
                   MOVE ACCT-DTC-PARTICIPANT TO WS-ACCT-PARTICIPANT
               WHEN ACCTMAST-NOTFND
                   ADD 1               TO WS-ACCT-NOT-FOUND
                   MOVE '*** ACCOUNT NOT ON FILE ***' TO WS-ACCT-NAME
                   MOVE SPACES         TO WS-ACCT-TYPE
                                          WS-ACCT-PARTICIPANT
                   IF WS-RETURN-CODE < 4
                       MOVE 4          TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 'ACCTMAST'     TO AB-DDNAME
                   MOVE WS-ACCTMAST-FS TO AB-FILE-STATUS
                   MOVE 1002           TO AB-ABEND-CODE
                   MOVE TH-ACCT-NO     TO AB-KEY
                   MOVE 'READ FAILED'  TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2300-EXIT.
           EXIT.
      *
       2900-REQUEST-TRAILER.
           MOVE SPACES                 TO RRB-BLUE-REC
           MOVE WS-RQ-REQ-ID(WS-RQ-SUB) TO RRB-REQUEST-ID
           MOVE 'T'                    TO RRB-REC-TYPE
           MOVE WS-RQ-CUSIP(WS-RQ-SUB) TO RRB-CUSIP
           MOVE WS-RQ-TO(WS-RQ-SUB)    TO RRB-TRADE-DATE
           MOVE WS-RQ-TRADES(WS-RQ-SUB) TO WS-TRADE-COUNT-X
           MOVE WS-TRADE-COUNT-X       TO RRB-TRADE-ID
           MOVE WS-RQ-QTY(WS-RQ-SUB)   TO RRB-QTY
           MOVE ZERO                   TO RRB-PRICE
           MOVE WS-RQ-NET(WS-RQ-SUB)   TO RRB-NET-AMOUNT
           PERFORM 8100-WRITE-BLUE     THRU 8100-EXIT
           ADD WS-RQ-QTY(WS-RQ-SUB)    TO WS-TOTAL-QTY
           ADD WS-RQ-NET(WS-RQ-SUB)    TO WS-TOTAL-NET
           MOVE '0'                    TO TL-CC
           MOVE WS-RQ-TRADES(WS-RQ-SUB) TO TL-COUNT
           MOVE WS-RQ-QTY(WS-RQ-SUB)   TO TL-QTY
           MOVE WS-RQ-NET(WS-RQ-SUB)   TO TL-NET
           WRITE RPT-RECORD FROM WS-TOTAL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
           IF WS-RQ-TRADES(WS-RQ-SUB) = ZERO
               MOVE SPACES             TO WS-MSG-LINE
               MOVE ' '                TO ML-CC
               MOVE '    NO TRADES FOUND FOR THIS REQUEST' TO ML-TEXT
               PERFORM 8200-PRINT-MESSAGE THRU 8200-EXIT
           END-IF
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'REQDONE'              TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE WS-RQ-REQ-ID(WS-RQ-SUB) TO AU-KEY
           MOVE 'TRADING ACTIVITY REQUEST ANSWERED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       2900-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O                                                     *
      *================================================================*
       8000-READ-HISTORY.
           IF WS-HIST-EOF
               GO TO 8000-EXIT
           END-IF
           READ TRDHIST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   IF TH-QTY NOT NUMERIC
                       MOVE ZERO       TO TH-QTY
                   END-IF
                   IF TH-NET-AMOUNT NOT NUMERIC
                       MOVE ZERO       TO TH-NET-AMOUNT
                   END-IF
               WHEN TRDHIST-EOF
                   MOVE 'Y'            TO WS-HIST-EOF-SW
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE 1002           TO AB-ABEND-CODE
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
       8100-WRITE-BLUE.
           WRITE BLUEOUT-REC FROM RRB-BLUE-REC
           IF WS-BLUEOUT-FS NOT = '00'
               MOVE 'BLUEOUT'          TO AB-DDNAME
               MOVE WS-BLUEOUT-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE RRB-REQUEST-ID     TO AB-KEY
               MOVE 'WRITE FAILED'     TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           ADD 1                       TO WS-BLUE-OUT.
       8100-EXIT.
           EXIT.
      *
       8200-PRINT-MESSAGE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8300-HEADINGS   THRU 8300-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-MSG-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *
       8300-HEADINGS.
           ADD 1                       TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT         TO RPT-H1-PAGE
           WRITE RPT-RECORD FROM RPT-HEADING-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM RPT-HEADING-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           MOVE 2                      TO RPT-LINE-COUNT.
       8300-EXIT.
           EXIT.
      *
       8900-CHECK-WRITE.
           IF WS-RPTFILE-FS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           WRITE RPT-RECORD FROM RPT-END-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           CLOSE PARMCARD TRDHIST-FILE ACCTMAST-FILE RPTFILE
           CLOSE BLUEOUT-FILE
           IF WS-BLUEOUT-FS NOT = '00'
               MOVE 'BLUEOUT'          TO AB-DDNAME
               MOVE WS-BLUEOUT-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'CLOSE FAILED'     TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
           MOVE 'RRB300'               TO CT-STAGE
           MOVE 'REQUESTS-IN'          TO CT-COUNTER-NAME
           MOVE WS-CARDS-READ          TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'BLUESHT-OUT'          TO CT-COUNTER-NAME
           MOVE WS-BLUE-OUT            TO CT-COUNT
           MOVE WS-TOTAL-NET           TO CT-AMOUNT
           MOVE WS-TOTAL-QTY           TO CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           DISPLAY '************************************************'
           DISPLAY '* RRB300 - TRADING ACTIVITY REQUEST            *'
           DISPLAY '************************************************'
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE
           DISPLAY ' REQUEST CARDS READ       : ' WS-CARDS-READ
           DISPLAY ' REQUEST CARDS REJECTED   : ' WS-CARDS-REJECTED
           DISPLAY ' REQUESTS ANSWERED        : ' WS-RQ-USED
           DISPLAY ' HISTORY RECORDS READ     : ' WS-HIST-READ
           DISPLAY ' TRADES SELECTED          : ' WS-HIST-SELECTED
           DISPLAY ' ACCOUNTS NOT ON FILE     : ' WS-ACCT-NOT-FOUND
           DISPLAY ' RESPONSE RECORDS WRITTEN : ' WS-BLUE-OUT
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE
           DISPLAY '************************************************'
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           MOVE SPACES                 TO AU-KEY
           IF WS-RETURN-CODE > ZERO
               MOVE 'W'                TO AU-SEVERITY
               MOVE 'TRADING ACTIVITY REQUEST ENDED WITH WARNINGS'
                                       TO AU-MESSAGE
           ELSE
               MOVE 'I'                TO AU-SEVERITY
               MOVE 'TRADING ACTIVITY REQUEST ENDED' TO AU-MESSAGE
           END-IF
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9999 - ABEND                                                   *
      *================================================================*
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'RRB300 ABENDING - CODE ' AB-ABEND-CODE
                   ' DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
           DISPLAY 'RRB300 ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
