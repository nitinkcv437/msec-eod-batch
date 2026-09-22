       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCB120.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  01/11/1988.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCB120                                            *
      * TITLE      : VALIDATE MANUAL TRADE CARDS                       *
      * JOB        : MSTCD030   STEP010 (IKJEFT01 - DB2 PLAN MSTCPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   EDITS THE MANUAL TRADE AND CANCEL CARDS KEYED BY OPERATIONS  *
      *   (ISPF PANEL TCMAN01).  COMMENT CARDS ('*' IN COLUMN 1) AND   *
      *   BLANK CARDS ARE SKIPPED.  EVERY CARD MUST HAVE BEEN APPROVED *
      *   BY A SECOND OPERATOR (MAKER / CHECKER, CHG26120).            *
      *   CARD TYPE TR = TRADE, CX = CANCEL OF AN EARLIER MANUAL TRADE *
      *   (MAN-REF IS THE REFERENCE OF THE TRADE BEING CANCELLED).     *
      *   SIDE: B BUY  S SELL  X SELL SHORT  C BUY TO COVER            *
      *                                                                *
      * FILES  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     PARAMETER CARDS TCP120A                     *
      *          MANCARDS  MSEC.PROD.TC.MANUAL.CARDS(0)  (TCMANFD)     *
      *          ACCTMAST  MSEC.PROD.CM.ACCTMAST.KSDS    (CMACCT)      *
      *          TRADEOUT  MSEC.PROD.TC.MAN.VALID(+1)    (TCTRADE)     *
      *          REJOUT    MSEC.PROD.TC.REJECTS.MAN(+1)  (TCREJCT)     *
      * CALLS  : CMD010 CMU010 CMU050 CMU060 CMU080 CMASM02            *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 ONE OR MORE CARDS REJECTED            *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1988-01-11 RJK            ORIGINAL                             *
      * 1989-09-25 RJK            CANCEL CARDS (CX)                    *
      * 1992-03-30 DWB            ACCOUNT STATUS EDIT                  *
      * 1996-04-22 DWB  CHG02215  SECURITY MASTER NOW DB2 (CMD010)     *
      * 1998-11-02 TLM  CHG04471  Y2K - CARD DATES CCYYMMDD            *
      * 2001-04-09 KAP  CHG08814  DECIMAL PRICES                       *
      * 2002-03-08 KAP  CHG09930  REJECT SEVERITY                      *
      * 2008-06-16 SPA  CHG18233  SHORT SALE / COVER SIDES X AND C     *
      * 2014-03-03 SPA  CHG26120  MAKER / CHECKER                      *
      * 2014-03-24 SPA  CHG26188  MIXED CASE REFERENCES FROM PANEL     *
      * 2021-08-09 NVR  CHG38917  TRADE AGE LIMIT FROM PARM CARD       *
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
           SELECT MANCARD-FILE  ASSIGN TO MANCARDS
                                FILE STATUS IS WS-MANCARD-FS.
           SELECT ACCTMAST-FILE ASSIGN TO ACCTMAST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS RANDOM
                                RECORD KEY IS ACCT-NO
                                FILE STATUS IS WS-ACCTMAST-FS.
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
       COPY CMDATEW.
       FD  MANCARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCMANFD.
       FD  ACCTMAST-FILE.
       COPY CMACCT.
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
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCB120'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-MANCARD-FS           PIC X(02).
               88  MANCARD-OK                    VALUE '00'.
               88  MANCARD-EOF                   VALUE '10'.
           05  WS-ACCTMAST-FS          PIC X(02).
           05  WS-TRADEOUT-FS          PIC X(02).
           05  WS-REJOUT-FS            PIC X(02).
      *
       01  WS-FLAGS.
           05  WS-EOF-FLAG             PIC X(01)  VALUE 'N'.
               88  END-OF-CARDS                  VALUE 'Y'.
           05  WS-PARM-EOF-FLAG        PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                  VALUE 'Y'.
           05  WS-CARD-FLAG            PIC X(01)  VALUE 'G'.
               88  CARD-GOOD                     VALUE 'G'.
               88  CARD-BAD                      VALUE 'B'.
           05  WS-MAKER-CHECK-FLAG     PIC X(01)  VALUE 'Y'.
               88  MAKER-CHECK-ON                VALUE 'Y'.
      *
       01  WS-PARM-MAX-AGE             PIC 9(03)  VALUE 005.
       01  WS-PARM-KEY                 PIC X(30).
       01  WS-PARM-VAL                 PIC X(30).
      *
       01  WS-COUNTS.
           05  WS-CARDS-READ           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-COMMENTS-SKIPPED     PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-BLANKS-SKIPPED       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-TRADE-CARDS          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CANCEL-CARDS         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-TRADES-WRITTEN       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-REJECTS-WRITTEN      PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-QTY-TOTAL            PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-AMT-TOTAL            PIC S9(15)V99 COMP-3
                                                  VALUE ZERO.
           05  WS-REJ-QTY-TOTAL        PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
      *
       01  WS-WORK.
           05  WS-REJ-CODE             PIC X(04).
           05  WS-REJ-TEXT             PIC X(60).
           05  WS-SIDE                 PIC X(02).
           05  WS-AMT                  PIC S9(15)V99 COMP-3.
           05  WS-ENTRY-TS             PIC X(26).
           05  WS-RC                   PIC S9(04) COMP VALUE ZERO.
           05  WS-EDIT-COUNT           PIC ZZZ,ZZ9.
      *
       COPY TCTRADE.
       COPY TCREJCT.
       COPY CMSECMS.
       COPY CMSECLNK.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-CARD
               UNTIL END-OF-CARDS
           PERFORM 9000-TERMINATE
           MOVE WS-RC                  TO RETURN-CODE
           GOBACK.
      *
      *----------------------------------------------------------------*
       1000-INITIALIZE SECTION.
      *----------------------------------------------------------------*
       1000-START.
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED FOR DATECARD' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           READ DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'MANUAL CARD VALIDATION STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM UNTIL END-OF-PARMS
                   READ PARMCARD
                       AT END SET END-OF-PARMS TO TRUE
                       NOT AT END PERFORM 1100-PARM-CARD
                   END-READ
               END-PERFORM
               CLOSE PARMCARD
           END-IF
           DISPLAY 'TCB120 - MAX TRADE AGE ' WS-PARM-MAX-AGE
                   ' MAKER/CHECKER ' WS-MAKER-CHECK-FLAG
      *
           OPEN INPUT MANCARD-FILE
           IF WS-MANCARD-FS NOT = '00'
               MOVE 'MANCARDS'         TO AB-DDNAME
               MOVE WS-MANCARD-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR
           END-IF
           OPEN INPUT ACCTMAST-FILE
           IF WS-ACCTMAST-FS NOT = '00'
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR
           END-IF
           OPEN OUTPUT TRADEOUT-FILE
           IF WS-TRADEOUT-FS NOT = '00'
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR
           END-IF
           OPEN OUTPUT REJOUT-FILE
           IF WS-REJOUT-FS NOT = '00'
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR
           END-IF
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP           TO WS-ENTRY-TS
           PERFORM 8000-READ-CARD.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       1100-PARM-CARD SECTION.
      *----------------------------------------------------------------*
       1100-START.
           IF PARM-CARD-REC(1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEY WS-PARM-VAL
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEY WS-PARM-VAL
           END-UNSTRING
           EVALUATE WS-PARM-KEY
               WHEN 'MAX-TRADE-AGE-DAYS'
                   IF WS-PARM-VAL(1:3) IS NUMERIC
                       MOVE WS-PARM-VAL(1:3) TO WS-PARM-MAX-AGE
                   END-IF
               WHEN 'MAKER-CHECKER'
                   MOVE WS-PARM-VAL(1:1) TO WS-MAKER-CHECK-FLAG
               WHEN OTHER
                   DISPLAY 'TCB120 - PARAMETER IGNORED: '
                           PARM-CARD-REC(1:40)
           END-EVALUATE.
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2000-PROCESS-CARD SECTION.
      *----------------------------------------------------------------*
       2000-START.
           EVALUATE TRUE
               WHEN MAN-CARD-REC = SPACES
                   ADD 1               TO WS-BLANKS-SKIPPED
               WHEN MAN-COMMENT-CARD
                   ADD 1               TO WS-COMMENTS-SKIPPED
               WHEN MAN-TRADE-CARD
                   ADD 1               TO WS-TRADE-CARDS
                   PERFORM 3000-EDIT-CARD
               WHEN MAN-CANCEL-CARD
                   ADD 1               TO WS-CANCEL-CARDS
                   PERFORM 3000-EDIT-CARD
               WHEN OTHER
                   SET CARD-BAD        TO TRUE
                   MOVE 'S001'         TO WS-REJ-CODE
                   MOVE SPACES         TO WS-REJ-TEXT
                   STRING 'INVALID CARD TYPE [' MAN-REC-TYPE ']'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   PERFORM 7000-WRITE-REJECT
           END-EVALUATE
           PERFORM 8000-READ-CARD.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3000-EDIT-CARD SECTION.
      *----------------------------------------------------------------*
       3000-START.
           SET CARD-GOOD               TO TRUE
           MOVE SPACES                 TO WS-REJ-CODE WS-REJ-TEXT
           INITIALIZE SEC-MASTER-REC
      *
           IF MAN-REF = SPACES
               MOVE 'S017'             TO WS-REJ-CODE
               MOVE 'CARD REFERENCE MISSING' TO WS-REJ-TEXT
               GO TO 3000-BAD
           END-IF
      *
           IF MAKER-CHECK-ON
               IF MAN-APPROVED-BY = SPACES
                   MOVE 'S013'         TO WS-REJ-CODE
                   MOVE 'CARD NOT APPROVED BY A CHECKER'
                                       TO WS-REJ-TEXT
                   GO TO 3000-BAD
               END-IF
               IF MAN-ENTERED-BY = MAN-APPROVED-BY
                   MOVE 'S012'         TO WS-REJ-CODE
                   STRING 'MAKER AND CHECKER ARE BOTH '
                          MAN-ENTERED-BY
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   GO TO 3000-BAD
               END-IF
           END-IF
      *
           EVALUATE MAN-SIDE
               WHEN 'B'  MOVE 'B '     TO WS-SIDE
               WHEN 'S'  MOVE 'S '     TO WS-SIDE
               WHEN 'X'  MOVE 'SS'     TO WS-SIDE
               WHEN 'C'  MOVE 'BC'     TO WS-SIDE
               WHEN OTHER
                   MOVE 'S014'         TO WS-REJ-CODE
                   STRING 'INVALID SIDE [' MAN-SIDE ']'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   GO TO 3000-BAD
           END-EVALUATE
      *
           IF MAN-QTY NOT NUMERIC
               MOVE 'S003'             TO WS-REJ-CODE
               MOVE 'QUANTITY NOT NUMERIC' TO WS-REJ-TEXT
               GO TO 3000-BAD
           END-IF
           IF MAN-QTY = ZERO
               MOVE 'Q001'             TO WS-REJ-CODE
               MOVE 'QUANTITY IS ZERO' TO WS-REJ-TEXT
               GO TO 3000-BAD
           END-IF
           IF MAN-PRICE NOT NUMERIC
               MOVE 'S004'             TO WS-REJ-CODE
               MOVE 'PRICE NOT NUMERIC' TO WS-REJ-TEXT
               GO TO 3000-BAD
           END-IF
           IF MAN-PRICE = ZERO
               MOVE 'Q002'             TO WS-REJ-CODE
               MOVE 'PRICE IS ZERO'    TO WS-REJ-TEXT
               GO TO 3000-BAD
           END-IF
      *
           PERFORM 3100-EDIT-DATES
           IF CARD-BAD
               GO TO 3000-WRITE
           END-IF
           PERFORM 3300-CHECK-ACCOUNT
           IF CARD-BAD
               GO TO 3000-WRITE
           END-IF
           PERFORM 3400-CHECK-SECURITY
           GO TO 3000-WRITE.
      *
       3000-BAD.
           SET CARD-BAD                TO TRUE.
      *
       3000-WRITE.
           IF CARD-BAD
               PERFORM 7000-WRITE-REJECT
               IF MAN-QTY IS NUMERIC
                   ADD MAN-QTY         TO WS-REJ-QTY-TOTAL
               END-IF
           ELSE
               PERFORM 5000-BUILD-TRADE
               PERFORM 7100-WRITE-TRADE
           END-IF.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3100-EDIT-DATES SECTION.
      *----------------------------------------------------------------*
       3100-START.
           MOVE 'NYSE'                 TO DT-CALENDAR
           IF MAN-TRADE-DATE NOT NUMERIC
               MOVE 'D002'             TO WS-REJ-CODE
               MOVE 'TRADE DATE NOT NUMERIC' TO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
               GO TO 3100-EXIT
           END-IF
           MOVE 'VALD'                 TO DT-FUNCTION
           MOVE MAN-TRADE-DATE         TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'D002'             TO WS-REJ-CODE
               STRING 'INVALID TRADE DATE ' MAN-TRADE-DATE
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
               GO TO 3100-EXIT
           END-IF
           IF MAN-TRADE-DATE > DC-BUS-DATE
               MOVE 'D003'             TO WS-REJ-CODE
               STRING 'TRADE DATE ' MAN-TRADE-DATE ' IN THE FUTURE'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
               GO TO 3100-EXIT
           END-IF
           IF MAN-TRADE-DATE < DC-BUS-DATE
               MOVE 'DIFB'             TO DT-FUNCTION
               MOVE MAN-TRADE-DATE     TO DT-DATE-1
               MOVE DC-BUS-DATE        TO DT-DATE-2
               CALL 'CMU010' USING DT-DATE-PARMS
               IF NOT DT-OK
               OR DT-RESULT-NUM > WS-PARM-MAX-AGE
                   MOVE 'D004'         TO WS-REJ-CODE
                   STRING 'TRADE DATE ' MAN-TRADE-DATE
                          ' OLDER THAN LIMIT'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET CARD-BAD        TO TRUE
                   GO TO 3100-EXIT
               END-IF
           END-IF
      *    SETTLE DATE IS OPTIONAL - TCB200 COMPUTES IT WHEN ZERO
           IF MAN-SETTLE-DATE NOT NUMERIC
               MOVE 'D005'             TO WS-REJ-CODE
               MOVE 'SETTLE DATE NOT NUMERIC' TO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
               GO TO 3100-EXIT
           END-IF
           IF MAN-SETTLE-DATE = ZERO
               GO TO 3100-EXIT
           END-IF
           MOVE 'VALD'                 TO DT-FUNCTION
           MOVE MAN-SETTLE-DATE        TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'D005'             TO WS-REJ-CODE
               STRING 'INVALID SETTLE DATE ' MAN-SETTLE-DATE
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
               GO TO 3100-EXIT
           END-IF
           IF MAN-SETTLE-DATE < MAN-TRADE-DATE
               MOVE 'D006'             TO WS-REJ-CODE
               MOVE 'SETTLE DATE BEFORE TRADE DATE' TO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
           END-IF.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3300 - ACCOUNT CHECK                                           *
      *   MANUAL CARDS MAY BE KEYED AGAINST CLIENT, FIRM OR STREET     *
      *   ACCOUNTS (ADJUSTMENTS).  ACCOUNT MUST BE ON FILE AND OPEN.   *
      *----------------------------------------------------------------*
       3300-CHECK-ACCOUNT SECTION.
       3300-START.
           IF MAN-ACCT = SPACES
               MOVE 'A006'             TO WS-REJ-CODE
               MOVE 'ACCOUNT NUMBER MISSING' TO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
               GO TO 3300-EXIT
           END-IF
           MOVE MAN-ACCT               TO ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE WS-ACCTMAST-FS
               WHEN '00'
                   CONTINUE
               WHEN '23'
                   MOVE 'A001'         TO WS-REJ-CODE
                   STRING 'ACCOUNT ' MAN-ACCT ' NOT ON MASTER'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET CARD-BAD        TO TRUE
                   GO TO 3300-EXIT
               WHEN OTHER
                   MOVE 'ACCTMAST'     TO AB-DDNAME
                   MOVE WS-ACCTMAST-FS TO AB-FILE-STATUS
                   MOVE MAN-ACCT       TO AB-KEY
                   MOVE 1002           TO AB-ABEND-CODE
                   MOVE '3300-CHECK-ACCOUNT' TO AB-PARAGRAPH
                   MOVE 'ACCOUNT MASTER READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           EVALUATE TRUE
               WHEN ACCT-CLOSED
                   MOVE 'A002'         TO WS-REJ-CODE
                   MOVE 'ACCOUNT CLOSED' TO WS-REJ-TEXT
                   SET CARD-BAD        TO TRUE
               WHEN ACCT-DECEASED
                   MOVE 'A004'         TO WS-REJ-CODE
                   MOVE 'ACCOUNT DECEASED - ESTATE HOLD'
                                       TO WS-REJ-TEXT
                   SET CARD-BAD        TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       3300-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3400-CHECK-SECURITY SECTION.
      *----------------------------------------------------------------*
       3400-START.
           MOVE 'GET '                 TO SL-FUNCTION
           MOVE MAN-CUSIP              TO SL-KEY-CUSIP
           MOVE SPACES                 TO SL-KEY-ISIN SL-KEY-SYMBOL
           CALL 'CMD010' USING SL-SECURITY-PARMS
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA    TO SEC-MASTER-REC
               WHEN SL-NOT-FOUND
                   MOVE 'P001'         TO WS-REJ-CODE
                   STRING 'CUSIP ' MAN-CUSIP ' NOT ON SECURITY MASTER'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET CARD-BAD        TO TRUE
                   GO TO 3400-EXIT
               WHEN OTHER
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE MAN-CUSIP      TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3400-CHECK-SECURITY' TO AB-PARAGRAPH
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           IF NOT SEC-ACTIVE
               MOVE 'P002'             TO WS-REJ-CODE
               STRING 'SECURITY ' MAN-CUSIP ' STATUS ['
                      SEC-STATUS '] NOT ACTIVE'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET CARD-BAD            TO TRUE
           END-IF.
       3400-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 5000 - BUILD TRADE.  A CANCEL CARD CARRIES THE REFERENCE OF    *
      *        THE TRADE IT CANCELS - TRADE ID = ORIGINAL ID.          *
      *----------------------------------------------------------------*
       5000-BUILD-TRADE SECTION.
       5000-START.
           INITIALIZE TRD-TRADE-REC
           MOVE SPACES                 TO TRD-WARN-FLAGS
           STRING 'M' MAN-REF '     ' DELIMITED BY SIZE INTO TRD-ID
           MOVE 'MAN'                  TO TRD-SOURCE
           MOVE 1                      TO TRD-VERSION
           IF MAN-CANCEL-CARD
               SET TRD-CANCEL          TO TRUE
               MOVE TRD-ID             TO TRD-ORIG-ID
           ELSE
               SET TRD-NEW             TO TRUE
               MOVE SPACES             TO TRD-ORIG-ID
           END-IF
           SET TRD-ST-VALIDATED        TO TRUE
           MOVE MAN-ACCT               TO TRD-ACCT-NO
           MOVE MAN-CUSIP              TO TRD-CUSIP
           MOVE SEC-SYMBOL             TO TRD-SYMBOL
           MOVE WS-SIDE                TO TRD-SIDE
           MOVE MAN-QTY                TO TRD-QTY
           MOVE MAN-PRICE              TO TRD-PRICE
           MOVE MAN-TRADE-DATE         TO TRD-TRADE-DATE
           MOVE ZERO                   TO TRD-TRADE-TIME
           MOVE MAN-SETTLE-DATE        TO TRD-SETTLE-DATE
           MOVE SEC-CCY                TO TRD-CCY
           MOVE SEC-TYPE               TO TRD-SEC-TYPE
           MOVE SEC-PRICE-FACTOR       TO TRD-PRICE-FACTOR
           MOVE 'N'                    TO TRD-COMM-OVR-FLAG
           MOVE 'A'                    TO TRD-CAPACITY
           IF ACCT-FIRM-INVENTORY
               MOVE 'P'                TO TRD-CAPACITY
           END-IF
           MOVE SPACES                 TO TRD-EXEC-BROKER
                                          TRD-CONTRA
                                          TRD-SETTLE-LOC
           MOVE 'MAN '                 TO TRD-MARKET
           MOVE ACCT-TYPE              TO TRD-ACCT-TYPE
           MOVE ACCT-BRANCH            TO TRD-BRANCH
           MOVE ACCT-REP               TO TRD-REP
           MOVE 'OPS '                 TO TRD-DESK
           IF SEC-FIXED-INCOME
               MOVE MAN-QTY            TO TRD-FACE-AMOUNT
           END-IF
           MOVE SPACES                 TO TRD-REJECT-CODE
                                          TRD-REJECT-TEXT
           MOVE WS-ENTRY-TS            TO TRD-ENTRY-TS
           MOVE SPACES                 TO TRD-ENRICH-TS
           MOVE MAN-REF                TO TRD-SOURCE-REF
           MOVE MAN-ENTERED-BY         TO TRD-ENTERED-BY
           MOVE MAN-APPROVED-BY        TO TRD-APPROVED-BY.
       5000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       7000-WRITE-REJECT SECTION.
      *----------------------------------------------------------------*
       7000-START.
           INITIALIZE REJ-REJECT-REC
           MOVE DC-BUS-DATE            TO REJ-BUS-DATE
           MOVE 'VALIDATE'             TO REJ-STAGE
           MOVE WS-PROGRAM-ID          TO REJ-PROGRAM
           MOVE 'MAN'                  TO REJ-SOURCE
           IF MAN-REF NOT = SPACES
               STRING 'M' MAN-REF '     ' DELIMITED BY SIZE
                      INTO REJ-TRADE-ID
           END-IF
           MOVE MAN-ACCT               TO REJ-ACCT-NO
           MOVE MAN-CUSIP              TO REJ-CUSIP
           MOVE WS-REJ-CODE            TO REJ-CODE
           MOVE 'E'                    TO REJ-SEVERITY
           MOVE WS-REJ-TEXT            TO REJ-TEXT
           MOVE 80                     TO REJ-RAW-LENGTH
           MOVE SPACES                 TO REJ-RAW-IMAGE
           MOVE MAN-CARD-REC           TO REJ-RAW-IMAGE(1:80)
           WRITE REJOUT-REC            FROM REJ-REJECT-REC
           IF WS-REJOUT-FS NOT = '00'
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               MOVE '7000-WRITE-REJECT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR
           END-IF
           ADD 1                       TO WS-REJECTS-WRITTEN.
       7000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       7100-WRITE-TRADE SECTION.
      *----------------------------------------------------------------*
       7100-START.
           WRITE TRADEOUT-REC          FROM TRD-TRADE-REC
           IF WS-TRADEOUT-FS NOT = '00'
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7100-WRITE-TRADE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR
           END-IF
           ADD 1                       TO WS-TRADES-WRITTEN
           ADD TRD-QTY                 TO WS-QTY-TOTAL
           COMPUTE WS-AMT ROUNDED = TRD-QTY * TRD-PRICE
           ADD WS-AMT                  TO WS-AMT-TOTAL.
       7100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       8000-READ-CARD SECTION.
      *----------------------------------------------------------------*
       8000-START.
           READ MANCARD-FILE
           EVALUATE TRUE
               WHEN MANCARD-OK
                   ADD 1               TO WS-CARDS-READ
               WHEN MANCARD-EOF
                   SET END-OF-CARDS    TO TRUE
               WHEN OTHER
                   MOVE 'MANCARDS'     TO AB-DDNAME
                   MOVE WS-MANCARD-FS  TO AB-FILE-STATUS
                   MOVE '8000-READ-CARD' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       9000-TERMINATE SECTION.
      *----------------------------------------------------------------*
       9000-START.
           CLOSE MANCARD-FILE
                 ACCTMAST-FILE
                 TRADEOUT-FILE
                 REJOUT-FILE
           IF WS-MANCARD-FS NOT = '00'
           OR WS-ACCTMAST-FS NOT = '00'
           OR WS-TRADEOUT-FS NOT = '00'
           OR WS-REJOUT-FS NOT = '00'
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-MANCARD-FS ' '
                      WS-ACCTMAST-FS ' ' WS-TRADEOUT-FS ' '
                      WS-REJOUT-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           IF WS-REJECTS-WRITTEN > ZERO
               MOVE 4                  TO WS-RC
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'RECORDS-IN'           TO CT-COUNTER-NAME
           COMPUTE CT-COUNT = WS-TRADE-CARDS + WS-CANCEL-CARDS
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9100-POST
           MOVE 'VALID-OUT'            TO CT-COUNTER-NAME
           MOVE WS-TRADES-WRITTEN      TO CT-COUNT
           MOVE WS-AMT-TOTAL           TO CT-AMOUNT
           MOVE WS-QTY-TOTAL           TO CT-QTY-HASH
           PERFORM 9100-POST
           MOVE 'REJECT-OUT'           TO CT-COUNTER-NAME
           MOVE WS-REJECTS-WRITTEN     TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
           MOVE WS-REJ-QTY-TOTAL       TO CT-QTY-HASH
           PERFORM 9100-POST
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RC = ZERO
               MOVE 'I'                TO AU-SEVERITY
           ELSE
               MOVE 'W'                TO AU-SEVERITY
           END-IF
           MOVE WS-CARDS-READ          TO WS-EDIT-COUNT
           STRING 'MANUAL CARD VALIDATION ENDED. CARDS '
                  WS-EDIT-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY 'TCB120 =========================================='
           DISPLAY 'TCB120  MANUAL TRADE CARD VALIDATION ' DC-BUS-DATE
           DISPLAY 'TCB120 =========================================='
           MOVE WS-CARDS-READ          TO WS-EDIT-COUNT
           DISPLAY 'TCB120  CARDS READ           ' WS-EDIT-COUNT
           MOVE WS-COMMENTS-SKIPPED    TO WS-EDIT-COUNT
           DISPLAY 'TCB120  COMMENT CARDS        ' WS-EDIT-COUNT
           MOVE WS-BLANKS-SKIPPED      TO WS-EDIT-COUNT
           DISPLAY 'TCB120  BLANK CARDS          ' WS-EDIT-COUNT
           MOVE WS-TRADE-CARDS         TO WS-EDIT-COUNT
           DISPLAY 'TCB120  TRADE CARDS          ' WS-EDIT-COUNT
           MOVE WS-CANCEL-CARDS        TO WS-EDIT-COUNT
           DISPLAY 'TCB120  CANCEL CARDS         ' WS-EDIT-COUNT
           MOVE WS-TRADES-WRITTEN      TO WS-EDIT-COUNT
           DISPLAY 'TCB120  TRADES WRITTEN       ' WS-EDIT-COUNT
           MOVE WS-REJECTS-WRITTEN     TO WS-EDIT-COUNT
           DISPLAY 'TCB120  CARDS REJECTED       ' WS-EDIT-COUNT
           DISPLAY 'TCB120  RETURN CODE          ' WS-RC
           DISPLAY 'TCB120 =========================================='.
       9000-EXIT.
           EXIT.
      *
       9100-POST SECTION.
       9100-START.
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9100-POST'        TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME    TO AB-KEY
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
       9100-EXIT.
           EXIT.
      *
       9910-OPEN-ERROR SECTION.
       9910-START.
           MOVE 1001                   TO AB-ABEND-CODE
           MOVE '1000-INITIALIZE'      TO AB-PARAGRAPH
           STRING 'OPEN FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND.
       9910-EXIT.
           EXIT.
      *
       9920-IO-ERROR SECTION.
       9920-START.
           MOVE 1002                   TO AB-ABEND-CODE
           STRING 'I/O ERROR ON ' AB-DDNAME ' STATUS '
                  AB-FILE-STATUS
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND.
       9920-EXIT.
           EXIT.
      *
       9999-ABEND SECTION.
       9999-START.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'TCB120 - ABEND ' AB-ABEND-CODE ' ' AB-PARAGRAPH
           DISPLAY 'TCB120 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           STOP RUN.
       9999-EXIT.
           EXIT.
