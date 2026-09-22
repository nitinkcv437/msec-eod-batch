       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGB400.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  NOVEMBER 2015.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGB400                                            *
      * DESCRIPTION: MONTHLY MARGIN ACCOUNT SUMMARY (MONTH-END).       *
      *              ONE RECORD PER MARGIN ACCOUNT FOR THE MONTH:      *
      *                - MONTH-END REQUIREMENT (MG.REQ OF THE LAST     *
      *                  BUSINESS DAY)                                 *
      *                - INTEREST CHARGED FOR THE MONTH, DAYS AND      *
      *                  AVERAGE DEBIT (MG.INTMTD AFTER THE MONTH-END  *
      *                  POSTING OF MGB300)                            *
      *                - CALLS ISSUED / MET / LIQUIDATED IN THE MONTH, *
      *                  CALLS STILL OPEN, EXTENSIONS (MG.CALLS)       *
      *              MG.REQ AND MG.INTMTD ARE MATCHED ON THE ACCOUNT   *
      *              NUMBER (BOTH IN ACCOUNT ORDER).  AN ACCOUNT       *
      *              CHARGED INTEREST THAT IS NO LONGER IN THE MARGIN  *
      *              RUN GETS STATUS 'X'.  FIRM TOTAL RECORD LAST.     *
      *              THE FILE FEEDS MGR410 AND THE MARGIN SECTION OF   *
      *              THE CLIENT STATEMENT.                             *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGM010 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - MGP400A  FORCE=Y/N                     *
      *              REQIN    - MSEC.PROD.MG.REQ(0)             (MGREQ)*
      *              MGINTMTD - MSEC.PROD.MG.INTMTD.KSDS       (MGINTM)*
      *              MGCALLS  - MSEC.PROD.MG.CALLS.KSDS        (MGCALL)*
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS     (CMACCT)*
      * OUTPUT     : STMTOUT  - MSEC.PROD.MG.MSTMT(+1)         (MGMSTM)*
      * CALLS      : CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  NOT A MONTH-END CYCLE (ONLY THE TOTAL RECORD   *
      *                 IS WRITTEN) OR THE MONTH'S INTEREST HAS NOT    *
      *                 BEEN POSTED YET FOR SOME ACCOUNTS              *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2015-11-30 SPA  ORIGINAL - MARGIN SECTION FOR THE     CHG28844 *
      *                 CLIENT STATEMENT                               *
      * 2016-03-07 SPA  ACCOUNTS WHICH LEFT THE MARGIN RUN    CHG29405 *
      * 2018-07-30 MHC  WATCH FLAG FOR THE CREDIT COMMITTEE   CHG32655 *
      * 2021-09-20 MHC  FORCE CARD (MONTH-END RERUN)          CHG36480 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT PARMCARD       ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
           SELECT REQIN-FILE     ASSIGN TO REQIN
                  FILE STATUS IS WS-REQIN-STATUS.
           SELECT MGINTMTD-FILE  ASSIGN TO MGINTMTD
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS MIM-ACCT-NO
                  FILE STATUS IS WS-MGINTMTD-STATUS.
           SELECT MGCALLS-FILE   ASSIGN TO MGCALLS
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS MGC-KEY
                  FILE STATUS IS WS-MGCALLS-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCT-NO
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT STMTOUT-FILE   ASSIGN TO STMTOUT
                  FILE STATUS IS WS-STMTOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARMCARD-REC                PIC X(80).
       FD  REQIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGREQ.
       FD  MGINTMTD-FILE.
           COPY MGINTM.
       FD  MGCALLS-FILE.
           COPY MGCALL.
       FD  ACCTMAST-FILE.
           COPY CMACCT.
       FD  STMTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STMTOUT-REC                 PIC X(200).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGB400'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-REQIN-STATUS         PIC X(02)  VALUE '00'.
               88  REQIN-OK                       VALUE '00'.
               88  REQIN-EOF                      VALUE '10'.
           05  WS-MGINTMTD-STATUS      PIC X(02)  VALUE '00'.
               88  MGINTMTD-OK                    VALUE '00'.
               88  MGINTMTD-EOF                   VALUE '10'.
           05  WS-MGCALLS-STATUS       PIC X(02)  VALUE '00'.
               88  MGCALLS-OK                     VALUE '00'.
               88  MGCALLS-EOF                    VALUE '10'.
               88  MGCALLS-NOTFND                 VALUE '23'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-STMTOUT-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-CALL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCT-CALLS              VALUE 'Y'.
           05  WS-FORCE-SW             PIC X(01)  VALUE 'N'.
               88  FORCE-SUMMARY                  VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * MATCH KEYS - HIGH-VALUES AT END OF FILE                        *
      *----------------------------------------------------------------*
       01  WS-MATCH-KEYS.
           05  WS-REQ-KEY              PIC X(10)  VALUE LOW-VALUES.
           05  WS-MTD-KEY              PIC X(10)  VALUE LOW-VALUES.
       01  WS-MONTH                    PIC 9(06).
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(20).
      *----------------------------------------------------------------*
      * WATCH LIST CRITERIA (CREDIT COMMITTEE 2018-07)                 *
      *----------------------------------------------------------------*
       01  WS-WATCH-CALLS              PIC S9(03) COMP-3 VALUE +2.
       01  WS-CALL-WORK.
           05  WS-LARGEST-CALL-AMT     PIC S9(13)V99    COMP-3.
           05  WS-CALL-MONTH           PIC 9(06).
           05  WS-MET-MONTH            PIC 9(06).
       01  WS-TOTALS.
           05  WS-TOT-ACCTS            PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-EQUITY           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-LONG-MV          PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-DEBIT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-AVG-DEBIT        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-INTEREST         PIC S9(13)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-ISSUED           PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-MET              PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-LIQ              PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-OPEN             PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-EXT              PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TOT-CALL-AMT         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-REQ-IN-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MTD-IN-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MATCHED-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REQ-ONLY-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MTD-ONLY-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LEFT-RUN-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NOT-POSTED-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WAIVED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WATCH-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-READ-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-ACCT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OUT-CNT              PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       COPY MGMSTM.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE.
           IF DC-MONTH-END OR FORCE-SUMMARY
               PERFORM 8000-READ-REQ
               PERFORM 8100-READ-MTD
               PERFORM 2000-MATCH
                   UNTIL WS-REQ-KEY = HIGH-VALUES
                     AND WS-MTD-KEY = HIGH-VALUES
           ELSE
               DISPLAY 'MGB400 NOT A MONTH-END CYCLE (' DC-CYCLE-TYPE
                       ') - TOTAL RECORD ONLY'
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           PERFORM 5000-WRITE-TOTAL.
           PERFORM 9000-TERMINATE.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE DC-BUS-DATE (1:6) TO WS-MONTH.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MONTHLY MARGIN SUMMARY STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-READ-PARMS.
           OPEN INPUT REQIN-FILE.
           IF WS-REQIN-STATUS NOT = '00'
               MOVE 'REQIN' TO AB-DDNAME
               MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT MGINTMTD-FILE.
           IF WS-MGINTMTD-STATUS NOT = '00'
               MOVE 'MGINTMTD' TO AB-DDNAME
               MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT MGCALLS-FILE.
           IF WS-MGCALLS-STATUS NOT = '00'
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN OUTPUT STMTOUT-FILE.
           IF WS-STMTOUT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO AB-DDNAME
               MOVE WS-STMTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
       1100-READ-PARMS.
      *----------------------------------------------------------------*
           OPEN INPUT PARMCARD.
           IF NOT PARMCARD-OK
               DISPLAY 'MGB400 NO SYSIN - NO FORCE CARD'
           ELSE
               PERFORM UNTIL END-OF-PARMS
                   READ PARMCARD
                   EVALUATE TRUE
                       WHEN PARMCARD-EOF
                           MOVE 'Y' TO WS-PARM-EOF-SW
                       WHEN NOT PARMCARD-OK
                           MOVE 'SYSIN' TO AB-DDNAME
                           MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
                           MOVE 1002 TO AB-ABEND-CODE
                           MOVE '1100-READ-PARMS' TO AB-PARAGRAPH
                           MOVE 'READ FAILED' TO AB-MESSAGE
                           PERFORM 9999-ABEND
                       WHEN PARMCARD-REC (1:1) = '*'
                       WHEN PARMCARD-REC = SPACES
                           CONTINUE
                       WHEN OTHER
                           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE
                           UNSTRING PARMCARD-REC DELIMITED BY '=' OR ' '
                               INTO WS-PARM-KEYWORD WS-PARM-VALUE
                           END-UNSTRING
                           IF WS-PARM-KEYWORD = 'FORCE'
                               MOVE WS-PARM-VALUE (1:1) TO WS-FORCE-SW
                               DISPLAY 'MGB400 FORCE=' WS-FORCE-SW
                           ELSE
                               DISPLAY 'MGB400 UNKNOWN PARAMETER '
                                       WS-PARM-KEYWORD
                           END-IF
                   END-EVALUATE
               END-PERFORM
               CLOSE PARMCARD
           END-IF.
           IF FORCE-SUMMARY AND NOT DC-MONTH-END
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'FORCSUMM'     TO AU-EVENT
               MOVE 'W'            TO AU-SEVERITY
               MOVE WS-MONTH       TO AU-KEY
               MOVE 'MONTHLY MARGIN SUMMARY FORCED ON A DAILY CYCLE'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF.
      *================================================================*
      * MATCH-MERGE MG.REQ WITH MG.INTMTD ON ACCOUNT NUMBER            *
      *================================================================*
       2000-MATCH.
           EVALUATE TRUE
               WHEN WS-REQ-KEY = WS-MTD-KEY
                   ADD 1 TO WS-MATCHED-CNT
                   PERFORM 3000-BUILD-FROM-REQ
                   PERFORM 3200-INTEREST
                   PERFORM 3500-CALL-ACTIVITY
                   PERFORM 4000-WRITE-ACCOUNT
                   PERFORM 8000-READ-REQ
                   PERFORM 8100-READ-MTD
               WHEN WS-REQ-KEY < WS-MTD-KEY
                   ADD 1 TO WS-REQ-ONLY-CNT
                   PERFORM 3000-BUILD-FROM-REQ
                   MOVE 'N' TO MMS-INT-FLAG
                   PERFORM 3500-CALL-ACTIVITY
                   PERFORM 4000-WRITE-ACCOUNT
                   PERFORM 8000-READ-REQ
               WHEN OTHER
                   ADD 1 TO WS-MTD-ONLY-CNT
                   IF MIM-LAST-POSTED-MONTH = WS-MONTH
                   AND MIM-LAST-POSTED-AMT > ZERO
                       ADD 1 TO WS-LEFT-RUN-CNT
                       PERFORM 3100-BUILD-FROM-MTD
                       PERFORM 3200-INTEREST
                       PERFORM 3500-CALL-ACTIVITY
                       PERFORM 4000-WRITE-ACCOUNT
                   END-IF
                   PERFORM 8100-READ-MTD
           END-EVALUATE.
      *----------------------------------------------------------------*
       3000-BUILD-FROM-REQ.
      *----------------------------------------------------------------*
           INITIALIZE MMS-MONTHLY-REC.
           MOVE 'A'                 TO MMS-REC-TYPE.
           MOVE DC-BUS-DATE         TO MMS-BUS-DATE.
           MOVE WS-MONTH            TO MMS-MONTH.
           MOVE MRQ-ACCT-NO         TO MMS-ACCT-NO.
           MOVE MRQ-BRANCH          TO MMS-BRANCH.
           MOVE MRQ-REP             TO MMS-REP.
           MOVE MRQ-STATUS          TO MMS-STATUS.
           MOVE MRQ-LONG-MV         TO MMS-LONG-MV.
           MOVE MRQ-EQUITY          TO MMS-EQUITY.
           MOVE MRQ-DEBIT-BALANCE   TO MMS-DEBIT-BALANCE.
           MOVE MRQ-EXCESS          TO MMS-EXCESS.
           MOVE MRQ-SMA             TO MMS-SMA.
           MOVE MRQ-ACCT-NO         TO ACCT-NO.
           PERFORM 3900-ACCOUNT-NAME.
      *----------------------------------------------------------------*
      * INTEREST CHARGED BUT THE ACCOUNT IS NOT ON TODAY'S MG.REQ      *
      *----------------------------------------------------------------*
       3100-BUILD-FROM-MTD.
           INITIALIZE MMS-MONTHLY-REC.
           MOVE 'A'                 TO MMS-REC-TYPE.
           MOVE DC-BUS-DATE         TO MMS-BUS-DATE.
           MOVE WS-MONTH            TO MMS-MONTH.
           MOVE MIM-ACCT-NO         TO MMS-ACCT-NO.
           MOVE 'X'                 TO MMS-STATUS.
           MOVE MIM-ACCT-NO         TO ACCT-NO.
           PERFORM 3900-ACCOUNT-NAME.
           IF ACCTMAST-OK
               MOVE ACCT-BRANCH     TO MMS-BRANCH
               MOVE ACCT-REP        TO MMS-REP
           ELSE
               MOVE '???'           TO MMS-BRANCH
               MOVE '????'          TO MMS-REP
           END-IF.
      *----------------------------------------------------------------*
      * INTEREST OF THE MONTH FROM THE MONTH-TO-DATE ROW               *
      *----------------------------------------------------------------*
       3200-INTEREST.
           MOVE MIM-AVG-DEBIT TO MMS-AVG-DEBIT.
           MOVE MIM-MTD-DAYS  TO MMS-INT-DAYS.
           EVALUATE TRUE
               WHEN MIM-LAST-POSTED-MONTH = WS-MONTH
                AND MIM-LAST-POSTED-AMT > ZERO
                   MOVE 'P' TO MMS-INT-FLAG
                   MOVE MIM-LAST-POSTED-AMT TO MMS-INT-CHARGED
               WHEN MIM-LAST-POSTED-MONTH = WS-MONTH
                   MOVE 'W' TO MMS-INT-FLAG
                   MOVE ZERO TO MMS-INT-CHARGED
                   ADD 1 TO WS-WAIVED-CNT
               WHEN MIM-MONTH = WS-MONTH
                AND MIM-MTD-INTEREST > ZERO
                   MOVE 'N' TO MMS-INT-FLAG
                   MOVE ZERO TO MMS-INT-CHARGED
                   ADD 1 TO WS-NOT-POSTED-CNT
                   MOVE 4 TO WS-RETURN-CODE
                   DISPLAY 'MGB400 INTEREST NOT POSTED FOR MONTH '
                           WS-MONTH ' ACCOUNT ' MIM-ACCT-NO
               WHEN OTHER
                   MOVE 'N' TO MMS-INT-FLAG
                   MOVE ZERO TO MMS-INT-CHARGED MMS-INT-DAYS
                                MMS-AVG-DEBIT
           END-EVALUATE.
      *================================================================*
      * CALL ACTIVITY OF THE ACCOUNT IN THE MONTH                      *
      *================================================================*
       3500-CALL-ACTIVITY.
           MOVE ZERO   TO WS-LARGEST-CALL-AMT.
           MOVE SPACES TO MMS-LARGEST-CALL-TYPE.
           MOVE 'N'    TO WS-CALL-EOF-SW.
           MOVE LOW-VALUES  TO MGC-KEY.
           MOVE MMS-ACCT-NO TO MGC-ACCT-NO.
           START MGCALLS-FILE KEY IS NOT LESS THAN MGC-KEY.
           EVALUATE TRUE
               WHEN MGCALLS-OK
                   CONTINUE
               WHEN MGCALLS-NOTFND
                   MOVE 'Y' TO WS-CALL-EOF-SW
               WHEN OTHER
                   MOVE 'MGCALLS' TO AB-DDNAME
                   MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '3500-CALL-ACTIVITY' TO AB-PARAGRAPH
                   MOVE MMS-ACCT-NO TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
           PERFORM UNTIL END-OF-ACCT-CALLS
               READ MGCALLS-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN MGCALLS-EOF
                       MOVE 'Y' TO WS-CALL-EOF-SW
                   WHEN MGCALLS-OK
                       IF MGC-ACCT-NO NOT = MMS-ACCT-NO
                           MOVE 'Y' TO WS-CALL-EOF-SW
                       ELSE
                           PERFORM 3600-ONE-CALL
                       END-IF
                   WHEN OTHER
                       MOVE 'MGCALLS' TO AB-DDNAME
                       MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '3500-CALL-ACTIVITY' TO AB-PARAGRAPH
                       MOVE MMS-ACCT-NO TO AB-KEY
                       MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                       PERFORM 9999-ABEND
               END-EVALUATE
           END-PERFORM.
      *----------------------------------------------------------------*
       3600-ONE-CALL.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CALLS-READ-CNT.
           MOVE MGC-ISSUE-DATE (1:6) TO WS-CALL-MONTH.
           IF MGC-MET-DATE NUMERIC
               MOVE MGC-MET-DATE (1:6) TO WS-MET-MONTH
           ELSE
               MOVE ZERO TO WS-MET-MONTH
           END-IF.
           IF WS-CALL-MONTH = WS-MONTH
               ADD 1 TO MMS-CALLS-ISSUED
               ADD MGC-CALL-AMOUNT TO MMS-CALL-AMT-ISSUED
               ADD MGC-EXTENSION-COUNT TO MMS-EXTENSIONS
               IF MGC-CALL-AMOUNT > WS-LARGEST-CALL-AMT
                   MOVE MGC-CALL-AMOUNT TO WS-LARGEST-CALL-AMT
                   MOVE MGC-CALL-TYPE   TO MMS-LARGEST-CALL-TYPE
               END-IF
           END-IF.
           EVALUATE TRUE
               WHEN MGC-MET AND WS-MET-MONTH = WS-MONTH
                   ADD 1 TO MMS-CALLS-MET
               WHEN MGC-LIQUIDATE
                   ADD 1 TO MMS-CALLS-LIQ
               WHEN MGC-OPEN OR MGC-EXTENDED
                   ADD 1 TO MMS-CALLS-OPEN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
      *----------------------------------------------------------------*
       3900-ACCOUNT-NAME.
      *----------------------------------------------------------------*
           READ ACCTMAST-FILE.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE ACCT-NAME (1:30) TO MMS-ACCT-NAME
               WHEN ACCTMAST-NOTFND
                   ADD 1 TO WS-NO-ACCT-CNT
                   MOVE '*** NOT ON ACCOUNT MASTER ***'
                                          TO MMS-ACCT-NAME
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '3900-ACCOUNT-NAME' TO AB-PARAGRAPH
                   MOVE ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *================================================================*
      * WATCH FLAG, TOTALS, WRITE                                      *
      *================================================================*
       4000-WRITE-ACCOUNT.
           IF MMS-CALLS-LIQ > ZERO
           OR MMS-CALLS-ISSUED NOT < WS-WATCH-CALLS
           OR MMS-STATUS = 'H'
               MOVE 'W' TO MMS-WATCH-FLAG
               ADD 1 TO WS-WATCH-CNT
           ELSE
               MOVE SPACE TO MMS-WATCH-FLAG
           END-IF.
           PERFORM 8300-WRITE-STMT.
           ADD 1                    TO WS-TOT-ACCTS.
           ADD MMS-EQUITY           TO WS-TOT-EQUITY.
           ADD MMS-LONG-MV          TO WS-TOT-LONG-MV.
           ADD MMS-DEBIT-BALANCE    TO WS-TOT-DEBIT.
           ADD MMS-AVG-DEBIT        TO WS-TOT-AVG-DEBIT.
           ADD MMS-INT-CHARGED      TO WS-TOT-INTEREST.
           ADD MMS-CALLS-ISSUED     TO WS-TOT-ISSUED.
           ADD MMS-CALLS-MET        TO WS-TOT-MET.
           ADD MMS-CALLS-LIQ        TO WS-TOT-LIQ.
           ADD MMS-CALLS-OPEN       TO WS-TOT-OPEN.
           ADD MMS-EXTENSIONS       TO WS-TOT-EXT.
           ADD MMS-CALL-AMT-ISSUED  TO WS-TOT-CALL-AMT.
      *================================================================*
       5000-WRITE-TOTAL.
      *================================================================*
           INITIALIZE MMS-MONTHLY-REC.
           MOVE 'T'                 TO MMS-REC-TYPE.
           MOVE DC-BUS-DATE         TO MMS-BUS-DATE.
           MOVE WS-MONTH            TO MMS-MONTH.
           MOVE HIGH-VALUES         TO MMS-ACCT-NO.
           MOVE HIGH-VALUES         TO MMS-BRANCH MMS-REP.
           MOVE 'FIRM TOTAL - MARGIN ACCOUNTS' TO MMS-ACCT-NAME.
           MOVE '*'                 TO MMS-STATUS.
           MOVE WS-TOT-LONG-MV      TO MMS-LONG-MV.
           MOVE WS-TOT-EQUITY       TO MMS-EQUITY.
           MOVE WS-TOT-DEBIT        TO MMS-DEBIT-BALANCE.
           MOVE WS-TOT-AVG-DEBIT    TO MMS-AVG-DEBIT.
           MOVE WS-TOT-INTEREST     TO MMS-INT-CHARGED.
           MOVE WS-TOT-ISSUED       TO MMS-CALLS-ISSUED.
           MOVE WS-TOT-MET          TO MMS-CALLS-MET.
           MOVE WS-TOT-LIQ          TO MMS-CALLS-LIQ.
           MOVE WS-TOT-OPEN         TO MMS-CALLS-OPEN.
           MOVE WS-TOT-EXT          TO MMS-EXTENSIONS.
           MOVE WS-TOT-CALL-AMT     TO MMS-CALL-AMT-ISSUED.
           MOVE SPACE               TO MMS-INT-FLAG MMS-WATCH-FLAG.
           PERFORM 8300-WRITE-STMT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-REQ.
           READ REQIN-FILE.
           EVALUATE TRUE
               WHEN REQIN-OK
                   ADD 1 TO WS-REQ-IN-CNT
                   IF MRQ-ACCT-NO NOT > WS-REQ-KEY
                       MOVE 'REQIN' TO AB-DDNAME
                       MOVE 1006 TO AB-ABEND-CODE
                       MOVE '8000-READ-REQ' TO AB-PARAGRAPH
                       MOVE MRQ-ACCT-NO TO AB-KEY
                       MOVE 'MG.REQ NOT IN ACCOUNT SEQUENCE'
                                            TO AB-MESSAGE
                       PERFORM 9999-ABEND
                   END-IF
                   MOVE MRQ-ACCT-NO TO WS-REQ-KEY
               WHEN REQIN-EOF
                   MOVE HIGH-VALUES TO WS-REQ-KEY
               WHEN OTHER
                   MOVE 'REQIN' TO AB-DDNAME
                   MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-REQ' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8100-READ-MTD.
      *----------------------------------------------------------------*
           READ MGINTMTD-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN MGINTMTD-OK
                   ADD 1 TO WS-MTD-IN-CNT
                   MOVE MIM-ACCT-NO TO WS-MTD-KEY
               WHEN MGINTMTD-EOF
                   MOVE HIGH-VALUES TO WS-MTD-KEY
               WHEN OTHER
                   MOVE 'MGINTMTD' TO AB-DDNAME
                   MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-MTD' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8300-WRITE-STMT.
      *----------------------------------------------------------------*
           WRITE STMTOUT-REC FROM MMS-MONTHLY-REC.
           IF WS-STMTOUT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO AB-DDNAME
               MOVE WS-STMTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8300-WRITE-STMT' TO AB-PARAGRAPH
               MOVE MMS-ACCT-NO TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           ADD 1 TO WS-OUT-CNT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'MGB400'       TO CT-STAGE.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '8500-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE REQIN-FILE MGINTMTD-FILE MGCALLS-FILE ACCTMAST-FILE.
           CLOSE STMTOUT-FILE.
           IF WS-STMTOUT-STATUS NOT = '00'
               MOVE 'STMTOUT' TO AB-DDNAME
               MOVE WS-STMTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'REQ-IN'          TO CT-COUNTER-NAME.
           MOVE WS-REQ-IN-CNT     TO CT-COUNT.
           MOVE WS-TOT-EQUITY     TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL.
           MOVE 'MSTMT-OUT'       TO CT-COUNTER-NAME.
           MOVE WS-OUT-CNT        TO CT-COUNT.
           MOVE WS-TOT-INTEREST   TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL.
           MOVE 'CALLS-IN-MONTH'  TO CT-COUNTER-NAME.
           MOVE WS-TOT-ISSUED     TO CT-COUNT.
           MOVE WS-TOT-CALL-AMT   TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL.
           DISPLAY '************************************************'.
           DISPLAY '* MGB400 - MONTHLY MARGIN ACCOUNT SUMMARY      *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' MONTH                    : ' WS-MONTH.
           DISPLAY ' CYCLE TYPE / FORCE       : ' DC-CYCLE-TYPE ' / '
                   WS-FORCE-SW.
           MOVE WS-REQ-IN-CNT TO WS-DISP-CNT.
           DISPLAY ' REQUIREMENTS READ        : ' WS-DISP-CNT.
           MOVE WS-MTD-IN-CNT TO WS-DISP-CNT.
           DISPLAY ' MONTH-TO-DATE ROWS READ  : ' WS-DISP-CNT.
           MOVE WS-MATCHED-CNT TO WS-DISP-CNT.
           DISPLAY '   MATCHED                : ' WS-DISP-CNT.
           MOVE WS-REQ-ONLY-CNT TO WS-DISP-CNT.
           DISPLAY '   REQUIREMENT ONLY       : ' WS-DISP-CNT.
           MOVE WS-MTD-ONLY-CNT TO WS-DISP-CNT.
           DISPLAY '   INTEREST ROW ONLY      : ' WS-DISP-CNT.
           MOVE WS-LEFT-RUN-CNT TO WS-DISP-CNT.
           DISPLAY '   LEFT MARGIN RUN (X)    : ' WS-DISP-CNT.
           MOVE WS-NOT-POSTED-CNT TO WS-DISP-CNT.
           DISPLAY ' INTEREST NOT YET POSTED  : ' WS-DISP-CNT.
           MOVE WS-WAIVED-CNT TO WS-DISP-CNT.
           DISPLAY ' INTEREST WAIVED          : ' WS-DISP-CNT.
           MOVE WS-CALLS-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' CALL ROWS READ           : ' WS-DISP-CNT.
           MOVE WS-TOT-ISSUED TO WS-DISP-CNT.
           DISPLAY ' CALLS ISSUED IN MONTH    : ' WS-DISP-CNT.
           MOVE WS-WATCH-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS ON WATCH        : ' WS-DISP-CNT.
           MOVE WS-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' RECORDS OUT (INCL TOTAL) : ' WS-DISP-CNT.
           MOVE WS-TOT-INTEREST TO WS-DISP-AMT.
           DISPLAY ' INTEREST CHARGED         : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MONTHLY MARGIN SUMMARY ENDED' TO AU-MESSAGE.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
           ELSE
               MOVE 'I' TO AU-SEVERITY
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
           DISPLAY 'MGB400 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'MGB400 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
