       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCB100.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  07/17/1995.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCB100                                            *
      * TITLE      : VALIDATE OMS EQUITY EXECUTION FEED                *
      * JOB        : MSTCD010   STEP010 (IKJEFT01 - DB2 PLAN MSTCPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   READS THE DAILY OMS EXECUTION FILE (HEADER / DETAILS /       *
      *   TRAILER), VALIDATES EACH EXECUTION AND WRITES A NORMALIZED   *
      *   TRADE RECORD (TCTRADE, STATUS VL) OR A REJECT RECORD         *
      *   (TCREJCT).  THE OMS SENDS THE TICKER SYMBOL - THE CUSIP IS   *
      *   RESOLVED THROUGH THE SECURITY MASTER (CMD010 'GETS').        *
      *   PRICE TOLERANCE AGAINST THE PREVIOUS CLOSE IS A WARNING ONLY *
      *   (WRITTEN TO THE REJECT FILE WITH SEVERITY W, TRADE PASSES).  *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD           (CMDATEW)      *
      *          SYSIN     PARAMETER CARDS TCP100A                     *
      *          OMSFEED   MSEC.PROD.TC.OMSFEED.RAW(0)  (TCOMSFD)      *
      *          ACCTMAST  MSEC.PROD.CM.ACCTMAST.KSDS   (CMACCT)       *
      * OUTPUT : TRADEOUT  MSEC.PROD.TC.OMS.VALID(+1)   (TCTRADE)      *
      *          REJOUT    MSEC.PROD.TC.REJECTS.OMS(+1) (TCREJCT)      *
      * CALLS  : CMD010 CMD020 CMU010 CMU050 CMU060 CMU080 CMASM02     *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 REJECTS/WARNINGS  8 TRAILER ERROR     *
      *                                                                *
      * RESTART: RERUN FROM THE TOP.  DELETE TC.OMS.VALID(+1) AND      *
      *          TC.REJECTS.OMS(+1) IF CATALOGED BY THE FAILED RUN.    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1995-07-17 DWB  CHG01877  ORIGINAL - REPLACES TAPE FEED        *
      * 1996-04-22 DWB  CHG02215  SECURITY MASTER NOW DB2 (CMD010)     *
      * 1997-02-10 DWB  CHG02981  ACCEPT FIX SIDE CODES 1/2/5          *
      * 1998-11-02 TLM  CHG04471  Y2K - FEED DATES NOW CCYYMMDD        *
      * 1999-03-15 TLM  CHG04802  Y2K - REMOVED DATE WINDOW LOGIC      *
      * 2001-04-09 KAP  CHG08814  DECIMALIZATION - PRICE 6 DECIMALS    *
      * 2002-03-08 KAP  CHG09930  REJECT SEVERITY / WARNINGS           *
      * 2004-10-18 KAP  CHG12670  PRICE TOLERANCE CHECK (CMD020)       *
      * 2007-01-22 KAP  CHG16202  COMMISSION OVERRIDE FROM OMS         *
      * 2009-12-14 SPA  CHG19002  NON-USD CURRENCIES ACCEPTED          *
      * 2013-08-05 SPA  CHG25004  CAPACITY CODE R (RISKLESS PRINCIPAL) *
      * 2016-10-03 SPA  CHG30112  STREET ACCOUNTS REJECTED FROM OMS    *
      * 2020-06-29 NVR  CHG37555  TRADE AGE LIMIT FROM PARM CARD       *
      * 2024-02-12 NVR  CHG41007  T+1 - SETTLE DATE LEFT TO TCB200     *
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
           SELECT OMSFEED-FILE  ASSIGN TO OMSFEED
                                FILE STATUS IS WS-OMSFEED-FS.
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
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY CMDATEW.
      *
       FD  OMSFEED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCOMSFD.
      *
       FD  ACCTMAST-FILE.
       COPY CMACCT.
      *
       FD  TRADEOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TRADEOUT-REC                PIC X(400).
      *
       FD  REJOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REJOUT-REC                  PIC X(450).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCB100'.
      *
      *----------------------------------------------------------------*
      * FILE STATUS FIELDS                                             *
      *----------------------------------------------------------------*
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
               88  PARMCARD-OK                   VALUE '00'.
               88  PARMCARD-EOF                  VALUE '10'.
           05  WS-DATECARD-FS          PIC X(02).
               88  DATECARD-OK                   VALUE '00'.
           05  WS-OMSFEED-FS           PIC X(02).
               88  OMSFEED-OK                    VALUE '00'.
               88  OMSFEED-EOF                   VALUE '10'.
           05  WS-ACCTMAST-FS          PIC X(02).
               88  ACCTMAST-OK                   VALUE '00'.
               88  ACCTMAST-NOTFND               VALUE '23'.
           05  WS-TRADEOUT-FS          PIC X(02).
               88  TRADEOUT-OK                   VALUE '00'.
           05  WS-REJOUT-FS            PIC X(02).
               88  REJOUT-OK                     VALUE '00'.
      *
      *----------------------------------------------------------------*
      * SWITCHES                                                       *
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-FEED                VALUE 'Y'.
           05  WS-HEADER-SW            PIC X(01)  VALUE 'N'.
               88  WS-HEADER-SEEN                VALUE 'Y'.
           05  WS-TRAILER-SW           PIC X(01)  VALUE 'N'.
               88  WS-TRAILER-SEEN               VALUE 'Y'.
           05  WS-REJECT-SW            PIC X(01)  VALUE 'N'.
               88  WS-DETAIL-REJECTED            VALUE 'Y'.
               88  WS-DETAIL-OK                  VALUE 'N'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-PRICE-CHECK-SW       PIC X(01)  VALUE 'Y'.
               88  WS-PRICE-CHECK-ON             VALUE 'Y'.
           05  WS-SEC-FOUND-SW         PIC X(01)  VALUE 'N'.
               88  WS-SEC-FOUND                  VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * PARAMETERS (TCP100A)                                           *
      *----------------------------------------------------------------*
       01  WS-PARAMETERS.
           05  WS-PARM-TOLERANCE-PCT   PIC 9(03)  VALUE 050.
           05  WS-PARM-MAX-AGE-DAYS    PIC 9(03)  VALUE 005.
           05  WS-PARM-SOURCE-ID       PIC X(08)  VALUE 'MSOMS001'.
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(30).
           05  WS-PARM-VALUE           PIC X(30).
           05  WS-PARM-NUM             PIC 9(03).
      *
      *----------------------------------------------------------------*
      * COUNTERS AND TOTALS                                            *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HDR-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DTL-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRL-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRADES-WRITTEN       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANCELS-WRITTEN      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REJECTS-WRITTEN      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WARNINGS-WRITTEN     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STRUCT-ERRORS        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DTL-QTY-HASH         PIC S9(15) COMP-3 VALUE ZERO.
           05  WS-VALID-QTY-HASH       PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-VALID-AMT-HASH       PIC S9(15)V99 COMP-3
                                                  VALUE ZERO.
           05  WS-REJECT-QTY-HASH      PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
      *
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * WORK AREAS                                                     *
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-REJ-CODE             PIC X(04).
           05  WS-REJ-TEXT             PIC X(60).
           05  WS-TRADE-SIDE           PIC X(02).
           05  WS-PRICE-DIFF           PIC S9(09)V9(08) COMP-3.
           05  WS-PRICE-LIMIT          PIC S9(09)V9(08) COMP-3.
           05  WS-TRADE-AMT            PIC S9(15)V99    COMP-3.
           05  WS-AGE-DAYS             PIC S9(07)       COMP-3.
           05  WS-ENTRY-TS             PIC X(26).
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-HASH            PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.
      *
      *    VALID CURRENCY LIST - SEE CHG19002
       01  WS-CCY-VALUES               PIC X(18)
                                       VALUE 'USDEURGBPJPYCADCHF'.
       01  WS-CCY-TABLE  REDEFINES WS-CCY-VALUES.
           05  WS-CCY-ENTRY            PIC X(03)  OCCURS 6 TIMES
                                       INDEXED BY WS-CCY-IDX.
      *
      *----------------------------------------------------------------*
      * RECORD AREAS                                                   *
      *----------------------------------------------------------------*
       COPY TCTRADE.
       COPY TCREJCT.
       COPY CMSECMS.
      *
      *----------------------------------------------------------------*
      * CALL INTERFACES                                                *
      *----------------------------------------------------------------*
       COPY CMSECLNK.
       COPY CMPRLNK.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-FEED   THRU 2000-EXIT
               UNTIL WS-END-OF-FEED
           PERFORM 2900-END-OF-FEED-CHECKS THRU 2900-EXIT
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE
           IF NOT DATECARD-OK
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'OPEN FAILED FOR DATECARD' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           READ DATECARD-FILE
           IF NOT DATECARD-OK
           OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'OMS FEED VALIDATION STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           PERFORM 1100-READ-PARAMETERS THRU 1100-EXIT
      *
           OPEN INPUT  OMSFEED-FILE
           IF NOT OMSFEED-OK
               MOVE 'OMSFEED'          TO AB-DDNAME
               MOVE WS-OMSFEED-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN INPUT  ACCTMAST-FILE
           IF NOT ACCTMAST-OK
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
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
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP           TO WS-ENTRY-TS
      *
           PERFORM 8000-READ-FEED      THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1100 - PARAMETER CARDS  (KEYWORD=VALUE, '*' IN COL 1 COMMENT)  *
      *----------------------------------------------------------------*
       1100-READ-PARAMETERS.
           OPEN INPUT PARMCARD
           IF NOT PARMCARD-OK
               DISPLAY 'TCB100 - NO SYSIN PARAMETERS, DEFAULTS USED'
               GO TO 1100-EXIT
           END-IF
           PERFORM UNTIL WS-PARM-EOF
               READ PARMCARD
                   AT END
                       SET WS-PARM-EOF TO TRUE
                   NOT AT END
                       PERFORM 1110-APPLY-PARAMETER THRU 1110-EXIT
               END-READ
           END-PERFORM
           CLOSE PARMCARD
           DISPLAY 'TCB100 - PRICE TOLERANCE PCT : '
                   WS-PARM-TOLERANCE-PCT
           DISPLAY 'TCB100 - MAX TRADE AGE (BUS) : '
                   WS-PARM-MAX-AGE-DAYS
           DISPLAY 'TCB100 - PRICE CHECK         : '
                   WS-PRICE-CHECK-SW.
       1100-EXIT.
           EXIT.
      *
       1110-APPLY-PARAMETER.
           IF PARM-CARD-REC(1:1) = '*'
           OR PARM-CARD-REC = SPACES
               GO TO 1110-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD
                                          WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'PRICE-TOLERANCE-PCT'
                   IF WS-PARM-VALUE(1:3) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:3)
                                       TO WS-PARM-TOLERANCE-PCT
                   END-IF
               WHEN 'MAX-TRADE-AGE-DAYS'
                   IF WS-PARM-VALUE(1:3) IS NUMERIC
                       MOVE WS-PARM-VALUE(1:3)
                                       TO WS-PARM-MAX-AGE-DAYS
                   ELSE
                       IF WS-PARM-VALUE(1:2) IS NUMERIC
                           MOVE WS-PARM-VALUE(1:2) TO WS-PARM-NUM
                           MOVE WS-PARM-NUM TO WS-PARM-MAX-AGE-DAYS
                       END-IF
                   END-IF
               WHEN 'PRICE-CHECK'
                   MOVE WS-PARM-VALUE(1:1) TO WS-PRICE-CHECK-SW
               WHEN 'SOURCE-ID'
                   MOVE WS-PARM-VALUE(1:8) TO WS-PARM-SOURCE-ID
               WHEN OTHER
                   DISPLAY 'TCB100 - UNKNOWN PARAMETER IGNORED: '
                           PARM-CARD-REC(1:40)
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - PROCESS ONE FEED RECORD                                 *
      *================================================================*
       2000-PROCESS-FEED.
           MOVE SPACES                 TO WS-REJ-CODE
                                          WS-REJ-TEXT
           IF WS-TRAILER-SEEN
               MOVE 'S008'             TO WS-REJ-CODE
               MOVE 'RECORD FOUND AFTER TRAILER - IGNORED'
                                       TO WS-REJ-TEXT
               PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
               GO TO 2000-READ-NEXT
           END-IF
           EVALUATE TRUE
               WHEN OMS-HEADER
                   PERFORM 2100-PROCESS-HEADER THRU 2100-EXIT
               WHEN OMS-DETAIL
                   PERFORM 2200-PROCESS-DETAIL THRU 2200-EXIT
               WHEN OMS-TRAILER
                   PERFORM 2300-PROCESS-TRAILER THRU 2300-EXIT
               WHEN OTHER
                   MOVE 'S001'         TO WS-REJ-CODE
                   STRING 'INVALID RECORD TYPE [' OMS-REC-TYPE ']'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
           END-EVALUATE.
       2000-READ-NEXT.
           PERFORM 8000-READ-FEED      THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2100 - HEADER: MUST BE FIRST, FILE DATE MUST BE BUSINESS DATE  *
      *----------------------------------------------------------------*
       2100-PROCESS-HEADER.
           ADD 1                       TO WS-HDR-READ
           IF WS-HEADER-SEEN
               MOVE 'S005'             TO WS-REJ-CODE
               MOVE 'DUPLICATE HEADER RECORD - IGNORED'
                                       TO WS-REJ-TEXT
               PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
               GO TO 2100-EXIT
           END-IF
           SET WS-HEADER-SEEN          TO TRUE
           IF OMS-HDR-FILE-DATE NOT = DC-BUS-DATE
               MOVE 'OMSFEED'          TO AB-DDNAME
               MOVE SPACES             TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '2100-PROCESS-HEADER' TO AB-PARAGRAPH
               MOVE OMS-HDR-FILE-DATE  TO AB-KEY
               MOVE 'OMS FILE DATE NOT EQUAL TO BUSINESS DATE'
                                       TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           IF OMS-HDR-SOURCE-ID NOT = WS-PARM-SOURCE-ID
               DISPLAY 'TCB100 - WARNING: HEADER SOURCE ID '
                       OMS-HDR-SOURCE-ID ' EXPECTED '
                       WS-PARM-SOURCE-ID
           END-IF
           DISPLAY 'TCB100 - OMS FILE ' OMS-HDR-SOURCE-ID
                   ' DATE ' OMS-HDR-FILE-DATE
                   ' SEQ ' OMS-HDR-FILE-SEQ
                   ' CREATED ' OMS-HDR-CREATE-TS.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2200 - DETAIL                                                  *
      *----------------------------------------------------------------*
       2200-PROCESS-DETAIL.
           ADD 1                       TO WS-DTL-READ
           IF OMS-QTY IS NUMERIC
               ADD OMS-QTY             TO WS-DTL-QTY-HASH
           END-IF
           IF NOT WS-HEADER-SEEN
               MOVE 'S002'             TO WS-REJ-CODE
               MOVE 'DETAIL RECORD BEFORE HEADER'
                                       TO WS-REJ-TEXT
               PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
               GO TO 2200-EXIT
           END-IF
           SET WS-DETAIL-OK            TO TRUE
           MOVE SPACES                 TO WS-REJ-CODE
                                          WS-REJ-TEXT
           INITIALIZE SEC-MASTER-REC
           MOVE 'N'                    TO WS-SEC-FOUND-SW
           PERFORM 3000-VALIDATE-DETAIL THRU 3000-EXIT
           IF WS-DETAIL-REJECTED
               PERFORM 7000-WRITE-REJECT THRU 7000-EXIT
               IF OMS-QTY IS NUMERIC
                   ADD OMS-QTY         TO WS-REJECT-QTY-HASH
               END-IF
           ELSE
               PERFORM 3900-BUILD-TRADE THRU 3900-EXIT
               IF WS-PRICE-CHECK-ON
                   PERFORM 3500-PRICE-TOLERANCE THRU 3500-EXIT
               END-IF
               PERFORM 7100-WRITE-TRADE THRU 7100-EXIT
           END-IF.
       2200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2300 - TRAILER: RECORD COUNT AND QUANTITY HASH                 *
      *----------------------------------------------------------------*
       2300-PROCESS-TRAILER.
           ADD 1                       TO WS-TRL-READ
           SET WS-TRAILER-SEEN         TO TRUE
           IF OMS-TRL-REC-COUNT NOT = WS-DTL-READ
               MOVE 'S006'             TO WS-REJ-CODE
               MOVE OMS-TRL-REC-COUNT  TO WS-DISP-COUNT
               STRING 'TRAILER COUNT ' WS-DISP-COUNT
                      ' NOT EQUAL DETAILS READ'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
               MOVE 8                  TO WS-RETURN-CODE
           END-IF
           IF OMS-TRL-QTY-HASH NOT = WS-DTL-QTY-HASH
               MOVE SPACES             TO WS-REJ-TEXT
               MOVE 'S007'             TO WS-REJ-CODE
               MOVE OMS-TRL-QTY-HASH   TO WS-DISP-HASH
               STRING 'TRAILER QTY HASH ' WS-DISP-HASH
                      ' MISMATCH'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
               MOVE 8                  TO WS-RETURN-CODE
           END-IF.
       2300-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2800 - STRUCTURAL REJECT (RAW RECORD, NO TRADE BUILT)          *
      *----------------------------------------------------------------*
       2800-STRUCTURE-REJECT.
           ADD 1                       TO WS-STRUCT-ERRORS
           PERFORM 7000-WRITE-REJECT   THRU 7000-EXIT.
       2800-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2900 - END OF FILE CHECKS                                      *
      *----------------------------------------------------------------*
       2900-END-OF-FEED-CHECKS.
           IF WS-RECS-READ = ZERO
               DISPLAY 'TCB100 - WARNING: OMS FEED IS EMPTY'
               IF WS-RETURN-CODE < 4
                   MOVE 4              TO WS-RETURN-CODE
               END-IF
               GO TO 2900-EXIT
           END-IF
           IF NOT WS-TRAILER-SEEN
               MOVE SPACES             TO OMS-FEED-REC
               MOVE 'S009'             TO WS-REJ-CODE
               MOVE 'TRAILER RECORD MISSING - FILE TRUNCATED?'
                                       TO WS-REJ-TEXT
               PERFORM 2800-STRUCTURE-REJECT THRU 2800-EXIT
               MOVE 8                  TO WS-RETURN-CODE
           END-IF.
       2900-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - VALIDATE ONE DETAIL.  FIRST ERROR WINS.                 *
      *================================================================*
       3000-VALIDATE-DETAIL.
           PERFORM 3010-EDIT-IDS       THRU 3010-EXIT
           IF WS-DETAIL-REJECTED  GO TO 3000-EXIT.
           PERFORM 3020-EDIT-SIDE      THRU 3020-EXIT
           IF WS-DETAIL-REJECTED  GO TO 3000-EXIT.
           PERFORM 3030-EDIT-QTY-PRICE THRU 3030-EXIT
           IF WS-DETAIL-REJECTED  GO TO 3000-EXIT.
           PERFORM 3040-EDIT-CODES     THRU 3040-EXIT
           IF WS-DETAIL-REJECTED  GO TO 3000-EXIT.
           PERFORM 3050-EDIT-TRADE-DATE THRU 3050-EXIT
           IF WS-DETAIL-REJECTED  GO TO 3000-EXIT.
           PERFORM 3100-VALIDATE-ACCOUNT THRU 3100-EXIT
           IF WS-DETAIL-REJECTED  GO TO 3000-EXIT.
           PERFORM 3200-VALIDATE-SECURITY THRU 3200-EXIT.
       3000-EXIT.
           EXIT.
      *
       3010-EDIT-IDS.
           IF OMS-EXEC-ID = SPACES OR LOW-VALUES
               MOVE 'S017'             TO WS-REJ-CODE
               MOVE 'EXECUTION ID MISSING' TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3010-EXIT
           END-IF
           IF OMS-CANCEL-FLAG NOT = 'Y' AND 'N' AND SPACE
               MOVE 'S010'             TO WS-REJ-CODE
               STRING 'INVALID CANCEL FLAG [' OMS-CANCEL-FLAG ']'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3010-EXIT
           END-IF
           IF OMS-CANCEL-FLAG = 'Y'
           AND OMS-ORIG-EXEC-ID = SPACES
               MOVE 'S011'             TO WS-REJ-CODE
               MOVE 'CANCEL WITHOUT ORIGINAL EXECUTION ID'
                                       TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
           END-IF.
       3010-EXIT.
           EXIT.
      *
      *    FIX TAG 54 CODES ACCEPTED SINCE CHG02981 (OLD OMS RELEASE)
       3020-EDIT-SIDE.
           EVALUATE OMS-SIDE
               WHEN 'B '
               WHEN '1 '
                   MOVE 'B '           TO WS-TRADE-SIDE
               WHEN 'S '
               WHEN '2 '
                   MOVE 'S '           TO WS-TRADE-SIDE
               WHEN 'SS'
               WHEN '5 '
                   MOVE 'SS'           TO WS-TRADE-SIDE
               WHEN 'BC'
                   MOVE 'BC'           TO WS-TRADE-SIDE
               WHEN OTHER
                   MOVE 'S014'         TO WS-REJ-CODE
                   STRING 'INVALID SIDE [' OMS-SIDE ']'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
           END-EVALUATE.
       3020-EXIT.
           EXIT.
      *
       3030-EDIT-QTY-PRICE.
           IF OMS-QTY NOT NUMERIC
               MOVE 'S003'             TO WS-REJ-CODE
               MOVE 'QUANTITY NOT NUMERIC' TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3030-EXIT
           END-IF
           IF OMS-QTY = ZERO
               MOVE 'Q001'             TO WS-REJ-CODE
               MOVE 'QUANTITY IS ZERO' TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3030-EXIT
           END-IF
           IF OMS-PRICE NOT NUMERIC
               MOVE 'S004'             TO WS-REJ-CODE
               MOVE 'PRICE NOT NUMERIC' TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3030-EXIT
           END-IF
           IF OMS-PRICE = ZERO
               MOVE 'Q002'             TO WS-REJ-CODE
               MOVE 'PRICE IS ZERO'    TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3030-EXIT
           END-IF
           IF OMS-COMM-OVR-FLAG = 'Y'
           AND OMS-COMM-OVERRIDE NOT NUMERIC
               MOVE 'S004'             TO WS-REJ-CODE
               MOVE 'COMMISSION OVERRIDE NOT NUMERIC'
                                       TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
           END-IF.
       3030-EXIT.
           EXIT.
      *
       3040-EDIT-CODES.
           SET WS-CCY-IDX              TO 1
           SEARCH WS-CCY-ENTRY
               AT END
                   MOVE 'S015'         TO WS-REJ-CODE
                   STRING 'INVALID CURRENCY [' OMS-CCY ']'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
                   GO TO 3040-EXIT
               WHEN WS-CCY-ENTRY (WS-CCY-IDX) = OMS-CCY
                   CONTINUE
           END-SEARCH
           IF OMS-CAPACITY NOT = 'A' AND 'P' AND 'R'
               MOVE 'S016'             TO WS-REJ-CODE
               STRING 'INVALID CAPACITY [' OMS-CAPACITY ']'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
           END-IF.
       3040-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3050 - TRADE DATE: VALID, NOT IN THE FUTURE, NOT TOO OLD       *
      *----------------------------------------------------------------*
       3050-EDIT-TRADE-DATE.
           IF OMS-TRADE-DATE NOT NUMERIC
               MOVE 'D002'             TO WS-REJ-CODE
               MOVE 'TRADE DATE NOT NUMERIC' TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3050-EXIT
           END-IF
           MOVE 'VALD'                 TO DT-FUNCTION
           MOVE 'NYSE'                 TO DT-CALENDAR
           MOVE OMS-TRADE-DATE         TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'D002'             TO WS-REJ-CODE
               STRING 'INVALID TRADE DATE ' OMS-TRADE-DATE
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3050-EXIT
           END-IF
           IF OMS-TRADE-DATE > DC-BUS-DATE
               MOVE 'D003'             TO WS-REJ-CODE
               STRING 'TRADE DATE ' OMS-TRADE-DATE
                      ' AFTER BUSINESS DATE'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3050-EXIT
           END-IF
           IF OMS-TRADE-DATE = DC-BUS-DATE
               GO TO 3050-EXIT
           END-IF
           MOVE 'DIFB'                 TO DT-FUNCTION
           MOVE OMS-TRADE-DATE         TO DT-DATE-1
           MOVE DC-BUS-DATE            TO DT-DATE-2
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'D002'             TO WS-REJ-CODE
               MOVE 'TRADE DATE AGE COULD NOT BE COMPUTED'
                                       TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3050-EXIT
           END-IF
           MOVE DT-RESULT-NUM          TO WS-AGE-DAYS
           IF WS-AGE-DAYS > WS-PARM-MAX-AGE-DAYS
               MOVE 'D004'             TO WS-REJ-CODE
               STRING 'TRADE DATE ' OMS-TRADE-DATE
                      ' OLDER THAN LIMIT'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
           END-IF.
       3050-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3100 - ACCOUNT VALIDATION                                      *
      *   ACCOUNT MUST EXIST AND BE ACTIVE.  STREET-SIDE ACCOUNTS MAY  *
      *   NOT BE TRADED THROUGH THE OMS (CHG30112).                    *
      *----------------------------------------------------------------*
       3100-VALIDATE-ACCOUNT.
           IF OMS-ACCOUNT = SPACES
           OR OMS-ACCOUNT(1:1) = SPACE
               MOVE 'A006'             TO WS-REJ-CODE
               MOVE 'ACCOUNT NUMBER BLANK OR NOT LEFT JUSTIFIED'
                                       TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3100-EXIT
           END-IF
           MOVE OMS-ACCOUNT            TO ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   CONTINUE
               WHEN ACCTMAST-NOTFND
                   MOVE 'A001'         TO WS-REJ-CODE
                   STRING 'ACCOUNT ' OMS-ACCOUNT ' NOT ON MASTER'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
                   GO TO 3100-EXIT
               WHEN OTHER
                   MOVE 'ACCTMAST'     TO AB-DDNAME
                   MOVE WS-ACCTMAST-FS TO AB-FILE-STATUS
                   MOVE OMS-ACCOUNT    TO AB-KEY
                   MOVE '3100-VALIDATE-ACCOUNT' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           EVALUATE TRUE
               WHEN ACCT-ACTIVE
                   CONTINUE
               WHEN ACCT-CLOSED
                   MOVE 'A002'         TO WS-REJ-CODE
                   MOVE 'ACCOUNT CLOSED' TO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
               WHEN ACCT-RESTRICTED
                   MOVE 'A003'         TO WS-REJ-CODE
                   MOVE 'ACCOUNT RESTRICTED - COMPLIANCE HOLD'
                                       TO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
               WHEN ACCT-DECEASED
                   MOVE 'A004'         TO WS-REJ-CODE
                   MOVE 'ACCOUNT DECEASED - ESTATE HOLD'
                                       TO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
               WHEN OTHER
                   MOVE 'A002'         TO WS-REJ-CODE
                   STRING 'ACCOUNT STATUS [' ACCT-STATUS
                          '] NOT ACTIVE'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
           END-EVALUATE
           IF WS-DETAIL-REJECTED
               GO TO 3100-EXIT
           END-IF
           IF ACCT-STREET-SIDE
               MOVE 'A005'             TO WS-REJ-CODE
               MOVE 'STREET ACCOUNT NOT ALLOWED FROM OMS'
                                       TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
           END-IF.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3200 - SECURITY: SYMBOL TO CUSIP VIA SECURITY MASTER           *
      *----------------------------------------------------------------*
       3200-VALIDATE-SECURITY.
           IF OMS-SYMBOL = SPACES
               MOVE 'P001'             TO WS-REJ-CODE
               MOVE 'SYMBOL MISSING'   TO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3200-EXIT
           END-IF
           MOVE 'GETS'                 TO SL-FUNCTION
           MOVE SPACES                 TO SL-KEY-CUSIP
                                          SL-KEY-ISIN
           MOVE OMS-SYMBOL             TO SL-KEY-SYMBOL
           CALL 'CMD010' USING SL-SECURITY-PARMS
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA    TO SEC-MASTER-REC
                   SET WS-SEC-FOUND    TO TRUE
               WHEN SL-NOT-FOUND
                   MOVE 'P001'         TO WS-REJ-CODE
                   STRING 'SYMBOL ' OMS-SYMBOL
                          ' NOT ON SECURITY MASTER'
                          DELIMITED BY SIZE INTO WS-REJ-TEXT
                   SET WS-DETAIL-REJECTED TO TRUE
                   GO TO 3200-EXIT
               WHEN OTHER
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE OMS-SYMBOL     TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3200-VALIDATE-SECURITY' TO AB-PARAGRAPH
                   MOVE 'CMD010 GETS FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE
           IF NOT SEC-ACTIVE
               MOVE 'P002'             TO WS-REJ-CODE
               STRING 'SECURITY ' SEC-CUSIP ' STATUS ['
                      SEC-STATUS '] NOT ACTIVE'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
               GO TO 3200-EXIT
           END-IF
           IF SEC-FIXED-INCOME
               MOVE 'P003'             TO WS-REJ-CODE
               STRING 'SECURITY TYPE ' SEC-TYPE
                      ' NOT VALID ON EQUITY FEED'
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               SET WS-DETAIL-REJECTED  TO TRUE
           END-IF.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3500 - PRICE TOLERANCE (WARNING ONLY)                          *
      *   COMPARE EXECUTION PRICE TO THE LATEST CLOSE ON OR BEFORE THE *
      *   PREVIOUS BUSINESS DATE.                                      *
      *----------------------------------------------------------------*
       3500-PRICE-TOLERANCE.
           IF TRD-CANCEL
               GO TO 3500-EXIT
           END-IF
           MOVE 'GETL'                 TO PL-FUNCTION
           MOVE TRD-CUSIP              TO PL-CUSIP
           MOVE DC-PREV-BUS-DATE       TO PL-PRICE-DATE
           CALL 'CMD020' USING PL-PRICE-PARMS
           EVALUATE TRUE
               WHEN PL-FOUND
                   CONTINUE
               WHEN PL-NOT-FOUND
                   MOVE 'W002'         TO WS-REJ-CODE
                   MOVE 'NO CLOSING PRICE - TOLERANCE NOT CHECKED'
                                       TO WS-REJ-TEXT
                   MOVE 'N'            TO TRD-WARN-FLAG (1)
                   PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
                   GO TO 3500-EXIT
               WHEN OTHER
                   MOVE PL-SQLCODE     TO AB-SQLCODE
                   MOVE TRD-CUSIP      TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3500-PRICE-TOLERANCE' TO AB-PARAGRAPH
                   MOVE 'CMD020 GETL FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE
           IF PL-PRICE NOT > ZERO
               GO TO 3500-EXIT
           END-IF
           COMPUTE WS-PRICE-DIFF = TRD-PRICE - PL-PRICE
           IF WS-PRICE-DIFF < ZERO
               COMPUTE WS-PRICE-DIFF = ZERO - WS-PRICE-DIFF
           END-IF
           COMPUTE WS-PRICE-LIMIT =
                   PL-PRICE * WS-PARM-TOLERANCE-PCT / 100
           IF WS-PRICE-DIFF > WS-PRICE-LIMIT
               MOVE 'W001'             TO WS-REJ-CODE
               MOVE 'P'                TO TRD-WARN-FLAG (1)
               STRING 'PRICE OUTSIDE ' WS-PARM-TOLERANCE-PCT
                      '% OF CLOSE ON ' PL-ACTUAL-DATE
                      DELIMITED BY SIZE INTO WS-REJ-TEXT
               PERFORM 7050-WRITE-WARNING THRU 7050-EXIT
           END-IF.
       3500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3900 - BUILD THE NORMALIZED TRADE RECORD                       *
      *----------------------------------------------------------------*
       3900-BUILD-TRADE.
           INITIALIZE TRD-TRADE-REC
           MOVE SPACES                 TO TRD-WARN-FLAGS
           STRING 'O' OMS-EXEC-ID '     '
                  DELIMITED BY SIZE INTO TRD-ID
           MOVE 'OMS'                  TO TRD-SOURCE
           MOVE 1                      TO TRD-VERSION
           IF OMS-CANCEL-FLAG = 'Y'
               SET TRD-CANCEL          TO TRUE
               STRING 'O' OMS-ORIG-EXEC-ID '     '
                      DELIMITED BY SIZE INTO TRD-ORIG-ID
           ELSE
               SET TRD-NEW             TO TRUE
               MOVE SPACES             TO TRD-ORIG-ID
           END-IF
           SET TRD-ST-VALIDATED        TO TRUE
           MOVE OMS-ACCOUNT            TO TRD-ACCT-NO
           MOVE SEC-CUSIP              TO TRD-CUSIP
           MOVE OMS-SYMBOL             TO TRD-SYMBOL
           MOVE WS-TRADE-SIDE          TO TRD-SIDE
           MOVE OMS-QTY                TO TRD-QTY
           MOVE OMS-PRICE              TO TRD-PRICE
           MOVE OMS-TRADE-DATE         TO TRD-TRADE-DATE
           IF OMS-TRADE-TIME IS NUMERIC
               MOVE OMS-TRADE-TIME     TO TRD-TRADE-TIME
           ELSE
               MOVE ZERO               TO TRD-TRADE-TIME
           END-IF
           MOVE ZERO                   TO TRD-SETTLE-DATE
           MOVE OMS-CCY                TO TRD-CCY
           MOVE SEC-TYPE               TO TRD-SEC-TYPE
           MOVE SEC-PRICE-FACTOR       TO TRD-PRICE-FACTOR
           IF OMS-COMM-OVR-FLAG = 'Y'
               MOVE OMS-COMM-OVERRIDE  TO TRD-COMMISSION
               MOVE 'Y'                TO TRD-COMM-OVR-FLAG
           ELSE
               MOVE ZERO               TO TRD-COMMISSION
               MOVE 'N'                TO TRD-COMM-OVR-FLAG
           END-IF
           MOVE OMS-CAPACITY           TO TRD-CAPACITY
           MOVE OMS-EXEC-BROKER        TO TRD-EXEC-BROKER
           MOVE SPACES                 TO TRD-CONTRA
           MOVE OMS-MARKET             TO TRD-MARKET
           MOVE SPACES                 TO TRD-SETTLE-LOC
           MOVE ACCT-TYPE              TO TRD-ACCT-TYPE
           MOVE ACCT-BRANCH            TO TRD-BRANCH
           MOVE ACCT-REP               TO TRD-REP
           MOVE OMS-DESK               TO TRD-DESK
           MOVE SPACES                 TO TRD-REJECT-CODE
                                          TRD-REJECT-TEXT
           MOVE WS-ENTRY-TS            TO TRD-ENTRY-TS
           MOVE SPACES                 TO TRD-ENRICH-TS
           MOVE OMS-ORDER-ID           TO TRD-SOURCE-REF
           MOVE OMS-TRADER-ID          TO TRD-ENTERED-BY
           MOVE SPACES                 TO TRD-APPROVED-BY.
       3900-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - OUTPUT                                                  *
      *================================================================*
       7000-WRITE-REJECT.
           MOVE 'E'                    TO REJ-SEVERITY
           PERFORM 7010-BUILD-REJECT   THRU 7010-EXIT
           WRITE REJOUT-REC            FROM REJ-REJECT-REC
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               MOVE '7000-WRITE-REJECT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-REJECTS-WRITTEN.
       7000-EXIT.
           EXIT.
      *
       7010-BUILD-REJECT.
           MOVE DC-BUS-DATE            TO REJ-BUS-DATE
           MOVE 'VALIDATE'             TO REJ-STAGE
           MOVE WS-PROGRAM-ID          TO REJ-PROGRAM
           MOVE 'OMS'                  TO REJ-SOURCE
           IF OMS-DETAIL
               STRING 'O' OMS-EXEC-ID '     '
                      DELIMITED BY SIZE INTO REJ-TRADE-ID
               MOVE OMS-ACCOUNT        TO REJ-ACCT-NO
           ELSE
               MOVE SPACES             TO REJ-TRADE-ID
                                          REJ-ACCT-NO
           END-IF
           IF WS-SEC-FOUND
               MOVE SEC-CUSIP          TO REJ-CUSIP
           ELSE
               MOVE SPACES             TO REJ-CUSIP
           END-IF
           MOVE WS-REJ-CODE            TO REJ-CODE
           MOVE WS-REJ-TEXT            TO REJ-TEXT
           MOVE 250                    TO REJ-RAW-LENGTH
           MOVE SPACES                 TO REJ-RAW-IMAGE
           MOVE OMS-FEED-REC           TO REJ-RAW-IMAGE(1:250).
       7010-EXIT.
           EXIT.
      *
       7050-WRITE-WARNING.
           PERFORM 7010-BUILD-REJECT   THRU 7010-EXIT
           MOVE 'W'                    TO REJ-SEVERITY
           WRITE REJOUT-REC            FROM REJ-REJECT-REC
           IF NOT REJOUT-OK
               MOVE 'REJOUT'           TO AB-DDNAME
               MOVE WS-REJOUT-FS       TO AB-FILE-STATUS
               MOVE '7050-WRITE-WARNING' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-WARNINGS-WRITTEN.
       7050-EXIT.
           EXIT.
      *
       7100-WRITE-TRADE.
           WRITE TRADEOUT-REC          FROM TRD-TRADE-REC
           IF NOT TRADEOUT-OK
               MOVE 'TRADEOUT'         TO AB-DDNAME
               MOVE WS-TRADEOUT-FS     TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '7100-WRITE-TRADE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-TRADES-WRITTEN
           IF TRD-CANCEL
               ADD 1                   TO WS-CANCELS-WRITTEN
           END-IF
           ADD TRD-QTY                 TO WS-VALID-QTY-HASH
           COMPUTE WS-TRADE-AMT ROUNDED = TRD-QTY * TRD-PRICE
           ADD WS-TRADE-AMT            TO WS-VALID-AMT-HASH.
       7100-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - READ FEED                                               *
      *================================================================*
       8000-READ-FEED.
           READ OMSFEED-FILE
           EVALUATE TRUE
               WHEN OMSFEED-OK
                   ADD 1               TO WS-RECS-READ
               WHEN OMSFEED-EOF
                   SET WS-END-OF-FEED  TO TRUE
               WHEN OTHER
                   MOVE 'OMSFEED'      TO AB-DDNAME
                   MOVE WS-OMSFEED-FS  TO AB-FILE-STATUS
                   MOVE '8000-READ-FEED' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE: CONTROL TOTALS, AUDIT, STATISTICS            *
      *================================================================*
       9000-TERMINATE.
           CLOSE OMSFEED-FILE
           IF NOT OMSFEED-OK
               MOVE 'OMSFEED'          TO AB-DDNAME
               MOVE WS-OMSFEED-FS      TO AB-FILE-STATUS
               PERFORM 9930-CLOSE-ERROR THRU 9930-EXIT
           END-IF
           CLOSE ACCTMAST-FILE
           IF NOT ACCTMAST-OK
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
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
      *
           IF WS-RETURN-CODE < 4
              AND (WS-REJECTS-WRITTEN > ZERO
                   OR WS-WARNINGS-WRITTEN > ZERO)
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
      *
           PERFORM 9100-POST-CONTROL-TOTALS THRU 9100-EXIT
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RETURN-CODE > 4
               MOVE 'E'                TO AU-SEVERITY
           ELSE
               IF WS-RETURN-CODE = 4
                   MOVE 'W'            TO AU-SEVERITY
               ELSE
                   MOVE 'I'            TO AU-SEVERITY
               END-IF
           END-IF
           MOVE WS-DTL-READ            TO WS-DISP-COUNT
           STRING 'OMS VALIDATION ENDED. DETAILS READ '
                  WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '************************************************'
           DISPLAY '* TCB100 - OMS FEED VALIDATION STATISTICS       *'
           DISPLAY '************************************************'
           DISPLAY '* BUSINESS DATE            : ' DC-BUS-DATE
           MOVE WS-RECS-READ           TO WS-DISP-COUNT
           DISPLAY '* RECORDS READ             : ' WS-DISP-COUNT
           MOVE WS-HDR-READ            TO WS-DISP-COUNT
           DISPLAY '*   HEADERS                : ' WS-DISP-COUNT
           MOVE WS-DTL-READ            TO WS-DISP-COUNT
           DISPLAY '*   DETAILS                : ' WS-DISP-COUNT
           MOVE WS-TRL-READ            TO WS-DISP-COUNT
           DISPLAY '*   TRAILERS               : ' WS-DISP-COUNT
           MOVE WS-TRADES-WRITTEN      TO WS-DISP-COUNT
           DISPLAY '* VALID TRADES WRITTEN     : ' WS-DISP-COUNT
           MOVE WS-CANCELS-WRITTEN     TO WS-DISP-COUNT
           DISPLAY '*   OF WHICH CANCELS       : ' WS-DISP-COUNT
           MOVE WS-REJECTS-WRITTEN     TO WS-DISP-COUNT
           DISPLAY '* REJECTS WRITTEN          : ' WS-DISP-COUNT
           MOVE WS-STRUCT-ERRORS       TO WS-DISP-COUNT
           DISPLAY '*   OF WHICH STRUCTURAL    : ' WS-DISP-COUNT
           MOVE WS-WARNINGS-WRITTEN    TO WS-DISP-COUNT
           DISPLAY '* WARNINGS WRITTEN         : ' WS-DISP-COUNT
           MOVE WS-DTL-QTY-HASH        TO WS-DISP-HASH
           DISPLAY '* DETAIL QTY HASH          : ' WS-DISP-HASH
           DISPLAY '* RETURN CODE              : ' WS-RETURN-CODE
           DISPLAY '************************************************'.
       9000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 9100 - CONTROL TOTALS TO CMU080                                *
      *----------------------------------------------------------------*
       9100-POST-CONTROL-TOTALS.
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
           MOVE WS-PROGRAM-ID          TO CT-STAGE
      *
           MOVE 'RECORDS-IN'           TO CT-COUNTER-NAME
           MOVE WS-DTL-READ            TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
           MOVE WS-DTL-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'VALID-OUT'            TO CT-COUNTER-NAME
           MOVE WS-TRADES-WRITTEN      TO CT-COUNT
           MOVE WS-VALID-AMT-HASH      TO CT-AMOUNT
           MOVE WS-VALID-QTY-HASH      TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'REJECT-OUT'           TO CT-COUNTER-NAME
           COMPUTE CT-COUNT = WS-REJECTS-WRITTEN - WS-STRUCT-ERRORS
           MOVE ZERO                   TO CT-AMOUNT
           MOVE WS-REJECT-QTY-HASH     TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'WARN-OUT'             TO CT-COUNTER-NAME
           MOVE WS-WARNINGS-WRITTEN    TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT
                                          CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
      *
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9100-EXIT.
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
      *================================================================*
      * 99XX - ERROR HANDLING                                          *
      *================================================================*
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
           DISPLAY 'TCB100 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'TCB100 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
