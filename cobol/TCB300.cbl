       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCB300.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  02/19/1990.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCB300                                            *
      * TITLE      : CANCEL / CORRECT PROCESSING AND DUPLICATE CHECK   *
      * JOB        : MSTCD050   STEP010                                *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   APPLIES THE DAY'S ENRICHED TRADES TO THE TRADE HISTORY FILE: *
      *   NEW (NW)   - MUST NOT ALREADY BE ON HISTORY.  A NEW TRADE    *
      *                WHOSE DUPLICATE HASH MATCHES AN ACTIVE HISTORY  *
      *                ROW FOR THE SAME ACCOUNT, CUSIP AND TRADE DATE  *
      *                IS REJECTED D001 (SUSPECTED DUPLICATE).         *
      *   CANCEL (CX)- MUST MATCH AN ACTIVE ROW BY ORIGINAL TRADE ID.  *
      *                THE HISTORY ROW IS MARKED CANCELLED AND THE     *
      *                CANCEL GOES FORWARD WITH THE ORIGINAL ECONOMICS *
      *   CORRECT(CR)- SUPERSEDES THE ORIGINAL (VERSION + 1).  A       *
      *                REVERSAL OF THE ORIGINAL (CX) IS WRITTEN AHEAD  *
      *                OF THE CORRECTED TRADE SO DOWNSTREAM SYSTEMS    *
      *                SEE CANCEL + REBOOK.                            *
      *   ACCEPTED TRADES ARE WRITTEN WITH STATUS FN (FINAL).          *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETER CARDS TCP300A                     *
      *          TRADEIN   MSEC.PROD.TC.TRADES.ENRICHED(0) (TCTRADE)   *
      * I-O    : TRDHIST   MSEC.PROD.TC.TRDHIST.KSDS      (TCTRDHS)    *
      * OUTPUT : TRADEOUT  MSEC.PROD.TC.TRADES.FINAL(+1)  (TCTRADE)    *
      *          REJOUT    MSEC.PROD.TC.REJECTS.CXL(+1)   (TCREJCT)    *
      * CALLS  : CMASM01 CMASM03 CMU010 CMU050 CMU060 CMU080           *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 REJECTS OR DUPLICATE TABLE FULL       *
      *                                                                *
      * RESTART: TRDHIST IS UPDATED IN PLACE.  BEFORE A RERUN RESTORE  *
      *          TRDHIST FROM MSEC.PROD.BKUP.TRDHIST(0) (MSCMD090 OF   *
      *          THE PREVIOUS NIGHT) OR EVERY TRADE REJECTS C003.      *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1990-02-19 RJK            ORIGINAL - CANCELS ONLY              *
      * 1991-05-06 RJK            BOND DESK AMENDS AS CANCEL + NEW     *
      * 1994-08-15 DWB            DUPLICATE CHECK ON ACCT/CUSIP/QTY    *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD ON HISTORY      *
      * 2001-04-09 KAP  CHG08814  DECIMAL PRICES IN HISTORY            *
      * 2006-06-12 KAP  CHG15008  DUPLICATE HASH VIA CMASM03           *
      * 2006-07-10 KAP  CHG15102  DUPLICATE TABLE LOADED AT START      *
      * 2012-09-10 SPA  CHG23380  CORRECTIONS (CR) - VERSION + 1       *
      * 2012-10-01 SPA  CHG23466  REVERSAL OF ORIGINAL ON CORRECTION   *
      * 2017-02-27 NVR  CHG32570  DUP TABLE 10000 TO 30000             *
      * 2024-02-12 NVR  CHG41007  T+1 REVIEW - NO CHANGE               *
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
           SELECT TRADEIN-FILE  ASSIGN TO TRADEIN
                                FILE STATUS IS WS-TRADEIN-FS.
           SELECT TRDHIST-FILE  ASSIGN TO TRDHIST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD KEY IS TH-TRADE-ID
                                FILE STATUS IS WS-TRDHIST-FS.
           SELECT TRADEOUT-FILE ASSIGN TO TRADEOUT
                                FILE STATUS IS WS-TRADEOUT-FS.
           SELECT REJOUT-FILE   ASSIGN TO REJOUT
                                FILE STATUS IS WS-REJOUT-FS.
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
       FD  TRADEIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRADEIN-REC                 PIC X(400).
       FD  TRDHIST-FILE.
       COPY TCTRDHS.
       FD  TRADEOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRADEOUT-REC                PIC X(400).
       FD  REJOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJOUT-REC                  PIC X(450).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCB300'.
      *
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-TRADEIN-FS           PIC X(02).
               88  TRADEIN-OK                    VALUE '00'.
               88  TRADEIN-EOF                   VALUE '10'.
           05  WS-TRDHIST-FS           PIC X(02).
               88  TRDHIST-OK                    VALUE '00'.
               88  TRDHIST-EOF                   VALUE '10'.
               88  TRDHIST-DUPKEY                VALUE '22'.
               88  TRDHIST-NOTFND                VALUE '23'.
           05  WS-TRADEOUT-FS          PIC X(02).
               88  TRADEOUT-OK                   VALUE '00'.
           05  WS-REJOUT-FS            PIC X(02).
               88  REJOUT-OK                     VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-TRADES              VALUE 'Y'.
           05  WS-HIST-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-END-OF-HISTORY             VALUE 'Y'.
           05  WS-REJECT-SW            PIC X(01)  VALUE 'N'.
               88  WS-TRADE-REJECTED             VALUE 'Y'.
               88  WS-TRADE-ACCEPTED             VALUE 'N'.
           05  WS-DUP-SW               PIC X(01)  VALUE 'N'.
               88  WS-DUPLICATE-FOUND            VALUE 'Y'.
           05  WS-DUP-CHECK-SW         PIC X(01)  VALUE 'Y'.
               88  WS-DUP-CHECK-ON               VALUE 'Y'.
           05  WS-DUP-FULL-SW          PIC X(01)  VALUE 'N'.
               88  WS-DUP-TABLE-FULL             VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
      *
       01  WS-PARM-DUP-WINDOW          PIC 9(03)  VALUE 014.
       01  WS-PARM-KEYWORD             PIC X(30).
       01  WS-PARM-VALUE               PIC X(30).
      *
       01  WS-COUNTERS.
           05  WS-TRADES-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRADES-WRITTEN       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REJECTS-WRITTEN      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-ACCEPTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CXL-ACCEPTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COR-ACCEPTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REVERSALS-WRITTEN    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DUPS-REJECTED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HIST-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HIST-LOADED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HIST-ADDED           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HIST-UPDATED         PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-IN-NET-TOTAL         PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-IN-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-NET-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-REJ-NET-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-REJ-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-REV-NET-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-REV-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
      *
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * DUPLICATE HASH KEY - FIELD ORDER AND FORMAT MUST NOT CHANGE,   *
      * THE HASH IS STORED ON TRDHIST (TH-DUP-HASH).  SEE CHG15008.    *
      *----------------------------------------------------------------*
       01  WS-HASH-KEY.
           05  WS-HK-ACCT              PIC X(10).
           05  WS-HK-CUSIP             PIC X(09).
           05  WS-HK-SIDE              PIC X(02).
           05  WS-HK-QTY               PIC 9(11)V9(04).
           05  WS-HK-PRICE             PIC 9(09)V9(08).
           05  WS-HK-TRADE-DATE        PIC 9(08).
      *
      *----------------------------------------------------------------*
      * DUPLICATE TABLE - ACTIVE HISTORY ROWS INSIDE THE WINDOW PLUS   *
      * TODAY'S NEW TRADES                                             *
      *----------------------------------------------------------------*
       01  WS-DUP-MAX                  PIC S9(08) COMP VALUE +30000.
       01  WS-DUP-COUNT                PIC S9(08) COMP VALUE ZERO.
       01  WS-DUP-SUB                  PIC S9(08) COMP VALUE ZERO.
       01  WS-DUP-TABLE.
           05  WS-DUP-ENTRY            OCCURS 30000 TIMES.
               10  WS-DUP-HASH         PIC S9(08) COMP.
               10  WS-DUP-ACCT         PIC X(10).
               10  WS-DUP-CUSIP        PIC X(09).
               10  WS-DUP-TRADE-DATE   PIC 9(08).
               10  WS-DUP-TRADE-ID     PIC X(16).
      *
       01  WS-WORK-FIELDS.
           05  WS-REJ-CODE             PIC X(04).
           05  WS-REJ-TEXT             PIC X(60).
           05  WS-WINDOW-START         PIC 9(08).
           05  WS-NEW-VERSION          PIC 9(03).
           05  WS-DUP-MATCH-ID         PIC X(16).
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
      *
       01  WS-SAVE-TRADE               PIC X(400).
      *
       COPY CMDATEW.
       COPY TCTRADE.
       COPY TCREJCT.
       COPY CMHSLNK.
       COPY CMJILNK.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-TRADE  THRU 2000-EXIT
               UNTIL WS-END-OF-TRADES
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
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
      *
           CALL 'CMASM01' USING JI-JOB-INFO
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE JI-JOBNAME             TO AU-KEY
           MOVE 'CANCEL/CORRECT AND DUPLICATE CHECK STARTED'
                                       TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           PERFORM 1100-READ-PARAMETERS THRU 1100-EXIT
      *
           OPEN INPUT TRADEIN-FILE
           IF NOT TRADEIN-OK
               MOVE 'TRADEIN'          TO AB-DDNAME
               MOVE WS-TRADEIN-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN I-O TRDHIST-FILE
           IF NOT TRDHIST-OK
               MOVE 'TRDHIST'          TO AB-DDNAME
               MOVE WS-TRDHIST-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT TRADEOUT-FILE
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT REJOUT-FILE
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
      *
           IF WS-DUP-CHECK-ON
               PERFORM 1200-LOAD-DUP-TABLE THRU 1200-EXIT
           END-IF
      *
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARAMETERS.
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS NOT = '00'
               DISPLAY 'TCB300 - NO PARAMETER CARDS - DEFAULTS USED'
               GO TO 1100-EXIT
           END-IF
           PERFORM UNTIL WS-PARM-EOF
               READ PARMCARD
                   AT END
                       SET WS-PARM-EOF TO TRUE
                   NOT AT END
                       IF PARM-CARD-REC(1:1) NOT = '*'
                           MOVE SPACES TO WS-PARM-KEYWORD
                                          WS-PARM-VALUE
                           UNSTRING PARM-CARD-REC
                               DELIMITED BY '=' OR ' '
                               INTO WS-PARM-KEYWORD WS-PARM-VALUE
                           END-UNSTRING
                           EVALUATE WS-PARM-KEYWORD
                               WHEN 'DUP-WINDOW-DAYS'
                                   IF WS-PARM-VALUE(1:3) IS NUMERIC
                                       MOVE WS-PARM-VALUE(1:3)
                                         TO WS-PARM-DUP-WINDOW
                                   END-IF
                               WHEN 'DUP-CHECK'
                                   MOVE WS-PARM-VALUE(1:1)
                                     TO WS-DUP-CHECK-SW
                               WHEN SPACES
                                   CONTINUE
                               WHEN OTHER
                                   DISPLAY 'TCB300 - PARM IGNORED: '
                                           PARM-CARD-REC(1:40)
                           END-EVALUATE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PARMCARD
           DISPLAY 'TCB300 - DUPLICATE CHECK ' WS-DUP-CHECK-SW
                   ' WINDOW ' WS-PARM-DUP-WINDOW ' CALENDAR DAYS'.
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1200 - LOAD ACTIVE HISTORY ROWS WITH TRADE DATE INSIDE THE     *
      *        WINDOW INTO THE DUPLICATE TABLE (SEQUENTIAL BROWSE).    *
      *----------------------------------------------------------------*
       1200-LOAD-DUP-TABLE.
           MOVE 'ADDC'                 TO DT-FUNCTION
           MOVE 'NYSE'                 TO DT-CALENDAR
           MOVE DC-BUS-DATE            TO DT-DATE-1
           COMPUTE DT-DAYS = ZERO - WS-PARM-DUP-WINDOW
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '1200-LOAD-DUP-TABLE' TO AB-PARAGRAPH
               MOVE DT-MESSAGE         TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE DT-RESULT-DATE         TO WS-WINDOW-START
      *
           MOVE LOW-VALUES             TO TH-TRADE-ID
           START TRDHIST-FILE KEY IS NOT LESS THAN TH-TRADE-ID
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   CONTINUE
               WHEN TRDHIST-NOTFND
               WHEN TRDHIST-EOF
                   DISPLAY 'TCB300 - TRADE HISTORY IS EMPTY'
                   GO TO 1200-EXIT
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE '1200-LOAD-DUP-TABLE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           PERFORM 1210-LOAD-ONE-ROW   THRU 1210-EXIT
               UNTIL WS-END-OF-HISTORY
           DISPLAY 'TCB300 - HISTORY ROWS READ ' WS-HIST-READ
                   ' LOADED FOR DUPLICATE CHECK ' WS-HIST-LOADED
                   ' SINCE ' WS-WINDOW-START.
       1200-EXIT.
           EXIT.
      *
       1210-LOAD-ONE-ROW.
           READ TRDHIST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   ADD 1               TO WS-HIST-READ
               WHEN TRDHIST-EOF
                   SET WS-END-OF-HISTORY TO TRUE
                   GO TO 1210-EXIT
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE '1210-LOAD-ONE-ROW' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           IF NOT TH-ACTIVE
           OR TH-TRADE-DATE < WS-WINDOW-START
               GO TO 1210-EXIT
           END-IF
           IF WS-DUP-COUNT NOT < WS-DUP-MAX
               IF NOT WS-DUP-TABLE-FULL
                   SET WS-DUP-TABLE-FULL TO TRUE
                   DISPLAY 'TCB300 - DUPLICATE TABLE FULL AT '
                           WS-DUP-COUNT ' - CHECK INCOMPLETE'
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
               GO TO 1210-EXIT
           END-IF
           ADD 1                       TO WS-DUP-COUNT
                                          WS-HIST-LOADED
           MOVE TH-DUP-HASH            TO WS-DUP-HASH (WS-DUP-COUNT)
           MOVE TH-ACCT-NO             TO WS-DUP-ACCT (WS-DUP-COUNT)
           MOVE TH-CUSIP               TO WS-DUP-CUSIP (WS-DUP-COUNT)
           MOVE TH-TRADE-DATE    TO WS-DUP-TRADE-DATE (WS-DUP-COUNT)
           MOVE TH-TRADE-ID      TO WS-DUP-TRADE-ID (WS-DUP-COUNT).
       1210-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - PROCESS ONE TRADE                                       *
      *================================================================*
       2000-PROCESS-TRADE.
           ADD 1                       TO WS-TRADES-READ
           ADD TRD-NET-AMOUNT          TO WS-IN-NET-TOTAL
           ADD TRD-QTY                 TO WS-IN-QTY-HASH
           SET WS-TRADE-ACCEPTED       TO TRUE
           MOVE SPACES                 TO WS-REJ-CODE
                                          WS-REJ-TEXT
           EVALUATE TRUE
               WHEN TRD-NEW
                   PERFORM 3000-NEW-TRADE THRU 3000-EXIT
               WHEN TRD-CANCEL
                   PERFORM 4000-CANCEL-TRADE THRU 4000-EXIT
               WHEN TRD-CORRECT
                   PERFORM 5000-CORRECT-TRADE THRU 5000-EXIT
               WHEN OTHER
                   MOVE 'S010'         TO WS-REJ-CODE
                   STRING 'UNKNOWN TRANSACTION TYPE [' TRD-TXN-TYPE
                          ']' DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
           END-EVALUATE
           IF WS-TRADE-REJECTED
               PERFORM 7000-WRITE-REJECT THRU 7000-EXIT
           END-IF
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - NEW TRADE                                               *
      *================================================================*
       3000-NEW-TRADE.
           PERFORM 6000-COMPUTE-HASH   THRU 6000-EXIT
      *
           MOVE TRD-ID                 TO TH-TRADE-ID
           READ TRDHIST-FILE
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   MOVE 'C003'         TO WS-REJ-CODE
                   STRING 'TRADE ID ALREADY ON HISTORY - STATUS '
                          TH-STATUS ' SINCE ' TH-FIRST-SEEN-DATE
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 3000-EXIT
               WHEN TRDHIST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE TRD-ID         TO AB-KEY
                   MOVE '3000-NEW-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
      *
           IF WS-DUP-CHECK-ON
               PERFORM 6100-CHECK-DUPLICATE THRU 6100-EXIT
               IF WS-DUPLICATE-FOUND
                   MOVE 'D001'         TO WS-REJ-CODE
                   STRING 'SUSPECTED DUPLICATE OF ' WS-DUP-MATCH-ID
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   ADD 1               TO WS-DUPS-REJECTED
                   GO TO 3000-EXIT
               END-IF
           END-IF
      *
           MOVE TRD-VERSION            TO WS-NEW-VERSION
           IF WS-NEW-VERSION = ZERO
               MOVE 1                  TO WS-NEW-VERSION
           END-IF
           PERFORM 6200-ADD-HISTORY    THRU 6200-EXIT
           IF WS-TRADE-REJECTED
               GO TO 3000-EXIT
           END-IF
           PERFORM 6300-ADD-DUP-ENTRY  THRU 6300-EXIT
           PERFORM 7100-WRITE-FINAL    THRU 7100-EXIT
           ADD 1                       TO WS-NEW-ACCEPTED.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - CANCEL                                                  *
      *================================================================*
       4000-CANCEL-TRADE.
           IF TRD-ORIG-ID = SPACES
               MOVE 'C001'             TO WS-REJ-CODE
               MOVE 'CANCEL WITHOUT ORIGINAL TRADE ID'
                                       TO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
               GO TO 4000-EXIT
           END-IF
           MOVE TRD-ORIG-ID            TO TH-TRADE-ID
           READ TRDHIST-FILE
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   CONTINUE
               WHEN TRDHIST-NOTFND
                   MOVE 'C001'         TO WS-REJ-CODE
                   STRING 'ORIGINAL ' TRD-ORIG-ID ' NOT ON HISTORY'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 4000-EXIT
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE TRD-ORIG-ID    TO AB-KEY
                   MOVE '4000-CANCEL-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           IF NOT TH-ACTIVE
               MOVE 'C002'             TO WS-REJ-CODE
               STRING 'ORIGINAL ' TRD-ORIG-ID ' STATUS ' TH-STATUS
                      ' BY ' TH-SUPERSEDED-BY
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
               GO TO 4000-EXIT
           END-IF
           IF TH-ACCT-NO NOT = TRD-ACCT-NO
           OR TH-CUSIP NOT = TRD-CUSIP
               MOVE 'C006'             TO WS-REJ-CODE
               STRING 'ORIGINAL IS ' TH-ACCT-NO '/' TH-CUSIP
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
               GO TO 4000-EXIT
           END-IF
      *
           MOVE 'CX'                   TO TH-STATUS
           MOVE TRD-ID                 TO TH-SUPERSEDED-BY
           PERFORM 6400-REWRITE-HISTORY THRU 6400-EXIT
           PERFORM 6500-DROP-DUP-ENTRY THRU 6500-EXIT
      *
      *    THE CANCEL GOES FORWARD WITH THE ECONOMICS OF THE ORIGINAL
           MOVE TH-SIDE                TO TRD-SIDE
           MOVE TH-QTY                 TO TRD-QTY
           MOVE TH-PRICE               TO TRD-PRICE
           MOVE TH-NET-AMOUNT          TO TRD-NET-AMOUNT
           MOVE TH-TRADE-DATE          TO TRD-TRADE-DATE
           MOVE TH-SETTLE-DATE         TO TRD-SETTLE-DATE
           MOVE TH-VERSION             TO TRD-VERSION
           MOVE TH-DUP-HASH            TO TRD-DUP-HASH
           IF TRD-CCY = 'USD'
               MOVE TRD-NET-AMOUNT     TO TRD-USD-NET-AMOUNT
           END-IF
           PERFORM 7100-WRITE-FINAL    THRU 7100-EXIT
           ADD 1                       TO WS-CXL-ACCEPTED.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
      * 5000 - CORRECTION: REVERSE ORIGINAL, BOOK CORRECTED TRADE      *
      *================================================================*
       5000-CORRECT-TRADE.
           MOVE TRD-ORIG-ID            TO TH-TRADE-ID
           READ TRDHIST-FILE
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   CONTINUE
               WHEN TRDHIST-NOTFND
                   MOVE 'C004'         TO WS-REJ-CODE
                   STRING 'ORIGINAL ' TRD-ORIG-ID ' NOT ON HISTORY'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 5000-EXIT
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE TRD-ORIG-ID    TO AB-KEY
                   MOVE '5000-CORRECT-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           IF NOT TH-ACTIVE
               MOVE 'C005'             TO WS-REJ-CODE
               STRING 'ORIGINAL ' TRD-ORIG-ID ' STATUS ' TH-STATUS
                      ' BY ' TH-SUPERSEDED-BY
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-TRADE-REJECTED   TO TRUE
               GO TO 5000-EXIT
           END-IF
           COMPUTE WS-NEW-VERSION = TH-VERSION + 1
      *
      *    SAVE THE CORRECTION - THE REVERSAL IS BUILT IN ITS PLACE
           MOVE TRD-TRADE-REC          TO WS-SAVE-TRADE
      *
      *    THE CORRECTION'S OWN ID MUST BE NEW
           MOVE TRD-ID                 TO TH-TRADE-ID
           READ TRDHIST-FILE
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   MOVE 'C003'         TO WS-REJ-CODE
                   STRING 'CORRECTION ID ALREADY ON HISTORY - '
                          TH-STATUS
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
                   GO TO 5000-EXIT
               WHEN TRDHIST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE TRD-ID         TO AB-KEY
                   MOVE '5000-CORRECT-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
      *
      *    SUPERSEDE THE ORIGINAL
           MOVE TRD-ORIG-ID            TO TH-TRADE-ID
           READ TRDHIST-FILE
           IF NOT TRDHIST-OK
               MOVE 'TRDHIST'          TO AB-DDNAME
               MOVE WS-TRDHIST-FS      TO AB-FILE-STATUS
               MOVE TRD-ORIG-ID        TO AB-KEY
               MOVE '5000-CORRECT-TRADE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           MOVE 'CR'                   TO TH-STATUS
           MOVE TRD-ID                 TO TH-SUPERSEDED-BY
           PERFORM 6400-REWRITE-HISTORY THRU 6400-EXIT
           PERFORM 6500-DROP-DUP-ENTRY THRU 6500-EXIT
      *
      *    WRITE THE REVERSAL, THEN THE CORRECTED TRADE
           PERFORM 5100-BUILD-REVERSAL THRU 5100-EXIT
           PERFORM 7100-WRITE-FINAL    THRU 7100-EXIT
           ADD 1                       TO WS-REVERSALS-WRITTEN
           ADD TRD-NET-AMOUNT          TO WS-REV-NET-TOTAL
           ADD TRD-QTY                 TO WS-REV-QTY-HASH
      *
           MOVE WS-SAVE-TRADE          TO TRD-TRADE-REC
           MOVE WS-NEW-VERSION         TO TRD-VERSION
           PERFORM 6000-COMPUTE-HASH   THRU 6000-EXIT
           PERFORM 6200-ADD-HISTORY    THRU 6200-EXIT
           IF WS-TRADE-REJECTED
               GO TO 5000-EXIT
           END-IF
           PERFORM 6300-ADD-DUP-ENTRY  THRU 6300-EXIT
           PERFORM 7100-WRITE-FINAL    THRU 7100-EXIT
           ADD 1                       TO WS-COR-ACCEPTED.
       5000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 5100 - REVERSAL OF THE ORIGINAL FROM THE HISTORY ROW.  FIELDS  *
      *        NOT KEPT ON HISTORY COME FROM THE CORRECTION.           *
      *----------------------------------------------------------------*
       5100-BUILD-REVERSAL.
           MOVE WS-SAVE-TRADE          TO TRD-TRADE-REC
           MOVE TH-TRADE-ID            TO TRD-ID
                                          TRD-ORIG-ID
           MOVE 'CX'                   TO TRD-TXN-TYPE
           MOVE TH-VERSION             TO TRD-VERSION
           MOVE TH-ACCT-NO             TO TRD-ACCT-NO
           MOVE TH-CUSIP               TO TRD-CUSIP
           MOVE TH-SIDE                TO TRD-SIDE
           MOVE TH-QTY                 TO TRD-QTY
           MOVE TH-PRICE               TO TRD-PRICE
           MOVE TH-NET-AMOUNT          TO TRD-NET-AMOUNT
           MOVE TH-TRADE-DATE          TO TRD-TRADE-DATE
           MOVE TH-SETTLE-DATE         TO TRD-SETTLE-DATE
           MOVE TH-DUP-HASH            TO TRD-DUP-HASH
           MOVE ZERO                   TO TRD-COMMISSION
                                          TRD-SEC-FEE
                                          TRD-TAF-FEE
                                          TRD-OTHER-FEES
                                          TRD-ACCRUED-INT
           COMPUTE TRD-PRINCIPAL ROUNDED =
                   TH-QTY * TH-PRICE * TRD-PRICE-FACTOR
           IF TRD-CCY = 'USD'
               MOVE TH-NET-AMOUNT      TO TRD-USD-NET-AMOUNT
           ELSE
               COMPUTE TRD-USD-NET-AMOUNT ROUNDED =
                       TH-NET-AMOUNT * TRD-FX-RATE
           END-IF
           MOVE 'CORRECTION REVERSAL'  TO TRD-REJECT-TEXT.
       5100-EXIT.
           EXIT.
      *
      *================================================================*
      * 6000 - HASH AND HISTORY ROUTINES                               *
      *================================================================*
       6000-COMPUTE-HASH.
           MOVE TRD-ACCT-NO            TO WS-HK-ACCT
           MOVE TRD-CUSIP              TO WS-HK-CUSIP
           MOVE TRD-SIDE               TO WS-HK-SIDE
           MOVE TRD-QTY                TO WS-HK-QTY
           MOVE TRD-PRICE              TO WS-HK-PRICE
           MOVE TRD-TRADE-DATE         TO WS-HK-TRADE-DATE
           MOVE SPACES                 TO HS-BUFFER
           MOVE WS-HASH-KEY            TO HS-BUFFER
           MOVE LENGTH OF WS-HASH-KEY  TO HS-LENGTH
           MOVE ZERO                   TO HS-HASH
           CALL 'CMASM03' USING HS-HASH-AREA
           MOVE HS-HASH                TO TRD-DUP-HASH.
       6000-EXIT.
           EXIT.
      *
      *    SAME HASH + SAME ACCOUNT, CUSIP AND TRADE DATE = DUPLICATE
       6100-CHECK-DUPLICATE.
           MOVE 'N'                    TO WS-DUP-SW
           MOVE SPACES                 TO WS-DUP-MATCH-ID
           PERFORM VARYING WS-DUP-SUB FROM 1 BY 1
                   UNTIL WS-DUP-SUB > WS-DUP-COUNT
                      OR WS-DUPLICATE-FOUND
               IF WS-DUP-HASH (WS-DUP-SUB) = TRD-DUP-HASH
               AND WS-DUP-ACCT (WS-DUP-SUB) = TRD-ACCT-NO
               AND WS-DUP-CUSIP (WS-DUP-SUB) = TRD-CUSIP
               AND WS-DUP-TRADE-DATE (WS-DUP-SUB) = TRD-TRADE-DATE
                   SET WS-DUPLICATE-FOUND TO TRUE
                   MOVE WS-DUP-TRADE-ID (WS-DUP-SUB)
                                       TO WS-DUP-MATCH-ID
               END-IF
           END-PERFORM.
       6100-EXIT.
           EXIT.
      *
       6200-ADD-HISTORY.
           INITIALIZE TH-HISTORY-REC
           MOVE TRD-ID                 TO TH-TRADE-ID
           MOVE WS-NEW-VERSION         TO TH-VERSION
           MOVE 'AC'                   TO TH-STATUS
           MOVE TRD-SOURCE             TO TH-SOURCE
           MOVE TRD-DUP-HASH           TO TH-DUP-HASH
           MOVE TRD-ACCT-NO            TO TH-ACCT-NO
           MOVE TRD-CUSIP              TO TH-CUSIP
           MOVE TRD-SIDE               TO TH-SIDE
           MOVE TRD-QTY                TO TH-QTY
           MOVE TRD-PRICE              TO TH-PRICE
           MOVE TRD-NET-AMOUNT         TO TH-NET-AMOUNT
           MOVE TRD-TRADE-DATE         TO TH-TRADE-DATE
           MOVE TRD-SETTLE-DATE        TO TH-SETTLE-DATE
           MOVE DC-BUS-DATE            TO TH-FIRST-SEEN-DATE
                                          TH-LAST-UPD-DATE
           MOVE JI-JOBNAME             TO TH-LAST-UPD-JOB
           MOVE SPACES                 TO TH-SUPERSEDED-BY
           WRITE TH-HISTORY-REC
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   ADD 1               TO WS-HIST-ADDED
               WHEN TRDHIST-DUPKEY
                   MOVE 'C003'         TO WS-REJ-CODE
                   MOVE 'TRADE ID ALREADY ON HISTORY (WRITE)'
                                       TO WS-REJ-TEXT
                   SET WS-TRADE-REJECTED TO TRUE
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE TRD-ID         TO AB-KEY
                   MOVE '6200-ADD-HISTORY' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       6200-EXIT.
           EXIT.
      *
       6300-ADD-DUP-ENTRY.
           IF NOT WS-DUP-CHECK-ON
               GO TO 6300-EXIT
           END-IF
           IF WS-DUP-COUNT NOT < WS-DUP-MAX
               IF NOT WS-DUP-TABLE-FULL
                   SET WS-DUP-TABLE-FULL TO TRUE
                   DISPLAY 'TCB300 - DUPLICATE TABLE FULL DURING RUN'
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
               GO TO 6300-EXIT
           END-IF
           ADD 1                       TO WS-DUP-COUNT
           MOVE TRD-DUP-HASH           TO WS-DUP-HASH (WS-DUP-COUNT)
           MOVE TRD-ACCT-NO            TO WS-DUP-ACCT (WS-DUP-COUNT)
           MOVE TRD-CUSIP              TO WS-DUP-CUSIP (WS-DUP-COUNT)
           MOVE TRD-TRADE-DATE   TO WS-DUP-TRADE-DATE (WS-DUP-COUNT)
           MOVE TRD-ID           TO WS-DUP-TRADE-ID (WS-DUP-COUNT).
       6300-EXIT.
           EXIT.
      *
       6400-REWRITE-HISTORY.
           MOVE DC-BUS-DATE            TO TH-LAST-UPD-DATE
           MOVE JI-JOBNAME             TO TH-LAST-UPD-JOB
           REWRITE TH-HISTORY-REC
           IF NOT TRDHIST-OK
               MOVE 'TRDHIST'          TO AB-DDNAME
               MOVE WS-TRDHIST-FS      TO AB-FILE-STATUS
               MOVE TH-TRADE-ID        TO AB-KEY
               MOVE '6400-REWRITE-HISTORY' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-HIST-UPDATED.
       6400-EXIT.
           EXIT.
      *
      *    A CANCELLED / SUPERSEDED ORIGINAL NO LONGER COUNTS
       6500-DROP-DUP-ENTRY.
           PERFORM VARYING WS-DUP-SUB FROM 1 BY 1
                   UNTIL WS-DUP-SUB > WS-DUP-COUNT
               IF WS-DUP-TRADE-ID (WS-DUP-SUB) = TH-TRADE-ID
                   MOVE ZERO           TO WS-DUP-TRADE-DATE (WS-DUP-SUB)
                   MOVE ZERO           TO WS-DUP-HASH (WS-DUP-SUB)
               END-IF
           END-PERFORM.
       6500-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - OUTPUT                                                  *
      *================================================================*
       7000-WRITE-REJECT.
           INITIALIZE REJ-REJECT-REC
           MOVE DC-BUS-DATE            TO REJ-BUS-DATE
           MOVE 'CXLCORR'              TO REJ-STAGE
           MOVE WS-PROGRAM-ID          TO REJ-PROGRAM
           MOVE TRD-SOURCE             TO REJ-SOURCE
           MOVE TRD-ID                 TO REJ-TRADE-ID
           MOVE TRD-ACCT-NO            TO REJ-ACCT-NO
           MOVE TRD-CUSIP              TO REJ-CUSIP
           MOVE WS-REJ-CODE            TO REJ-CODE
           MOVE 'E'                    TO REJ-SEVERITY
           MOVE WS-REJ-TEXT            TO REJ-TEXT
           MOVE 300                    TO REJ-RAW-LENGTH
           MOVE TRD-TRADE-REC(1:300)   TO REJ-RAW-IMAGE
           WRITE REJOUT-REC            FROM REJ-REJECT-REC
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7000-WRITE-REJECT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-REJECTS-WRITTEN
           ADD TRD-NET-AMOUNT          TO WS-REJ-NET-TOTAL
           ADD TRD-QTY                 TO WS-REJ-QTY-HASH.
       7000-EXIT.
           EXIT.
      *
       7100-WRITE-FINAL.
           SET TRD-ST-FINAL            TO TRUE
           WRITE TRADEOUT-REC          FROM TRD-TRADE-REC
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7100-WRITE-FINAL' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-TRADES-WRITTEN
           ADD TRD-NET-AMOUNT          TO WS-OUT-NET-TOTAL
           ADD TRD-QTY                 TO WS-OUT-QTY-HASH.
       7100-EXIT.
           EXIT.
      *
       8000-READ-TRADE.
           READ TRADEIN-FILE INTO TRD-TRADE-REC
           EVALUATE TRUE
               WHEN TRADEIN-OK
                   CONTINUE
               WHEN TRADEIN-EOF
                   SET WS-END-OF-TRADES TO TRUE
               WHEN OTHER
                   MOVE 'TRADEIN'      TO AB-DDNAME
                   MOVE WS-TRADEIN-FS  TO AB-FILE-STATUS
                   MOVE '8000-READ-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE TRADEIN-FILE
           IF NOT TRADEIN-OK
               MOVE 'TRADEIN'          TO AB-DDNAME
               MOVE WS-TRADEIN-FS      TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE TRDHIST-FILE
           IF NOT TRDHIST-OK
               MOVE 'TRDHIST'          TO AB-DDNAME
               MOVE WS-TRDHIST-FS      TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE TRADEOUT-FILE
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE REJOUT-FILE
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           IF WS-REJECTS-WRITTEN > ZERO
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'TRADES-IN'            TO CT-COUNTER-NAME
           MOVE WS-TRADES-READ         TO CT-COUNT
           MOVE WS-IN-NET-TOTAL        TO CT-AMOUNT
           MOVE WS-IN-QTY-HASH         TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'TRADES-OUT'           TO CT-COUNTER-NAME
           MOVE WS-TRADES-WRITTEN      TO CT-COUNT
           MOVE WS-OUT-NET-TOTAL       TO CT-AMOUNT
           MOVE WS-OUT-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'REJECT-OUT'           TO CT-COUNTER-NAME
           MOVE WS-REJECTS-WRITTEN     TO CT-COUNT
           MOVE WS-REJ-NET-TOTAL       TO CT-AMOUNT
           MOVE WS-REJ-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CORR-REVERSAL'        TO CT-COUNTER-NAME
           MOVE WS-REVERSALS-WRITTEN   TO CT-COUNT
           MOVE WS-REV-NET-TOTAL       TO CT-AMOUNT
           MOVE WS-REV-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'DUP-REJECT'           TO CT-COUNTER-NAME
           MOVE WS-DUPS-REJECTED       TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RETURN-CODE = ZERO
               MOVE 'I'                TO AU-SEVERITY
           ELSE
               MOVE 'W'                TO AU-SEVERITY
           END-IF
           MOVE WS-TRADES-WRITTEN      TO WS-DISP-COUNT
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'CANCEL/CORRECT ENDED. FINAL TRADES ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '================================================'
           DISPLAY ' TCB300  CANCEL / CORRECT / DUPLICATE  ' DC-BUS-DATE
           DISPLAY '================================================'
           MOVE WS-TRADES-READ         TO WS-DISP-COUNT
           DISPLAY ' TRADES READ ................. ' WS-DISP-COUNT
           MOVE WS-NEW-ACCEPTED        TO WS-DISP-COUNT
           DISPLAY '   NEW ACCEPTED .............. ' WS-DISP-COUNT
           MOVE WS-CXL-ACCEPTED        TO WS-DISP-COUNT
           DISPLAY '   CANCELS APPLIED ........... ' WS-DISP-COUNT
           MOVE WS-COR-ACCEPTED        TO WS-DISP-COUNT
           DISPLAY '   CORRECTIONS APPLIED ....... ' WS-DISP-COUNT
           MOVE WS-REVERSALS-WRITTEN   TO WS-DISP-COUNT
           DISPLAY '   REVERSALS GENERATED ....... ' WS-DISP-COUNT
           MOVE WS-TRADES-WRITTEN      TO WS-DISP-COUNT
           DISPLAY ' FINAL TRADES WRITTEN ........ ' WS-DISP-COUNT
           MOVE WS-REJECTS-WRITTEN     TO WS-DISP-COUNT
           DISPLAY ' REJECTS WRITTEN ............. ' WS-DISP-COUNT
           MOVE WS-DUPS-REJECTED       TO WS-DISP-COUNT
           DISPLAY '   SUSPECTED DUPLICATES ...... ' WS-DISP-COUNT
           MOVE WS-HIST-READ           TO WS-DISP-COUNT
           DISPLAY ' HISTORY ROWS READ ........... ' WS-DISP-COUNT
           MOVE WS-HIST-ADDED          TO WS-DISP-COUNT
           DISPLAY ' HISTORY ROWS ADDED .......... ' WS-DISP-COUNT
           MOVE WS-HIST-UPDATED        TO WS-DISP-COUNT
           DISPLAY ' HISTORY ROWS UPDATED ........ ' WS-DISP-COUNT
           MOVE WS-OUT-NET-TOTAL       TO WS-DISP-AMT
           DISPLAY ' NET AMOUNT WRITTEN .......... ' WS-DISP-AMT
           DISPLAY ' RETURN CODE ................. ' WS-RETURN-CODE
           DISPLAY '================================================'.
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
       9930-CLOSE-ERROR.
           MOVE 1002                   TO AB-ABEND-CODE
           MOVE '9000-TERMINATE'       TO AB-PARAGRAPH
           STRING 'CLOSE FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9930-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'TCB300 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'TCB300 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
