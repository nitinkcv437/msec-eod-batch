       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB300.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  FEBRUARY 1988.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB300                                            *
      * DESCRIPTION: CASH POSTING.                                     *
      *              PASS A - POSTING JOURNAL (TRADE-DATE CASH):       *
      *                TD BALANCE MOVES BY THE NET CASH OF EVERY       *
      *                CUSTOMER / FIRM LEG.  LEGS THAT SETTLED WHEN    *
      *                POSTED ALSO MOVE THE SD BALANCE, THE REST GO TO *
      *                PENDING CREDIT / DEBIT.                         *
      *              PASS B - SETTLED ACTIVITY FROM SRB250 (SETTLE-    *
      *                DATE CASH): SD BALANCE MOVES, PENDING RELIEVED. *
      *              DIVIDEND INCOME AND WITHHOLDING ACCUMULATE YEAR   *
      *              TO DATE.  BALANCES ARE KEPT PER ACCOUNT AND       *
      *              CURRENCY (NO CONVERSION).                         *
      *              EVERY MOVEMENT IS WRITTEN TO THE CASH ACTIVITY    *
      *              FILE FOR THE SRR310 REPORT.                       *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD040 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              JRNLIN   - MSEC.PROD.SR.POSTJRNL(0)      (SRPSTJ) *
      *              SETLIN   - MSEC.PROD.SR.SETTLED(0)       (SRACTV) *
      * IN/OUT     : CASHBAL  - MSEC.PROD.SR.CASHBAL.KSDS     (SRCASH) *
      * OUTPUT     : CASHACT  - MSEC.PROD.SR.CASHACT(+1)      (SRCSHA) *
      * CALLS      : CMU050, CMU060, CMU080, CMASM01                   *
      * RETURN CODE: 0 CLEAN, 4 SETTLEMENTS WITHOUT A BALANCE ROW OR   *
      *              NEGATIVE PENDING                                  *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1988-02-15 RJK  ORIGINAL                                       *
      * 1989-03-20 RJK  DATE CARD INSTEAD OF SYSTEM DATE      CHG00212 *
      * 1991-06-03 DWB  INPUT FROM POSTING JOURNAL            CHG00987 *
      * 1994-10-03 DWB  DIVIDEND INCOME / WITHHOLDING YTD     CHG01880 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 1999-06-14 TLM  Y2K - YTD RESET ON YEAR ROLL          CHG04802 *
      * 2009-12-14 SPA  MULTI-CURRENCY BALANCES               CHG19002 *
      * 2009-12-14 SPA  SETTLE-DATE CASH FROM SRB250          CHG19002 *
      * 2015-04-20 SPA  CASH ACTIVITY FILE FOR SRR310         CHG28004 *
      * 2024-05-20 NVR  T+1                                   CHG40551 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT JRNLIN-FILE    ASSIGN TO JRNLIN
                  FILE STATUS IS WS-JRNLIN-STATUS.
           SELECT SETLIN-FILE    ASSIGN TO SETLIN
                  FILE STATUS IS WS-SETLIN-STATUS.
           SELECT CASHACT-FILE   ASSIGN TO CASHACT
                  FILE STATUS IS WS-CASHACT-STATUS.
           SELECT CASHBAL-FILE   ASSIGN TO CASHBAL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CSH-KEY
                  FILE STATUS IS WS-CASHBAL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  JRNLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  JRNLIN-REC                  PIC X(300).
       FD  SETLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SETLIN-REC                  PIC X(200).
       FD  CASHACT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CASHACT-REC                 PIC X(150).
       FD  CASHBAL-FILE.
       COPY SRCASH.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB300'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-JRNLIN-STATUS        PIC X(02)  VALUE '00'.
               88  JRNLIN-OK                      VALUE '00'.
               88  JRNLIN-EOF                     VALUE '10'.
           05  WS-SETLIN-STATUS        PIC X(02)  VALUE '00'.
               88  SETLIN-OK                      VALUE '00'.
               88  SETLIN-EOF                     VALUE '10'.
           05  WS-CASHACT-STATUS       PIC X(02)  VALUE '00'.
           05  WS-CASHBAL-STATUS       PIC X(02)  VALUE '00'.
               88  CASHBAL-OK                     VALUE '00' '02'.
               88  CASHBAL-NOTFND                 VALUE '23'.
       01  WS-SWITCHES.
           05  WS-JRNL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-JOURNAL                 VALUE 'Y'.
           05  WS-SETL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-SETTLED                 VALUE 'Y'.
           05  WS-NEW-BAL-SW           PIC X(01)  VALUE 'N'.
               88  NEW-BALANCE                    VALUE 'Y'.
           05  WS-SETTLES-SW           PIC X(01)  VALUE 'N'.
               88  CASH-SETTLES-NOW               VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-WORK.
           05  WS-CASH                 PIC S9(15)V99    COMP-3.
           05  WS-ABS-CASH             PIC S9(15)V99    COMP-3.
           05  WS-BEFORE-TD            PIC S9(15)V99    COMP-3.
           05  WS-BEFORE-SD            PIC S9(15)V99    COMP-3.
           05  WS-BUS-CCYY             PIC 9(04).
           05  WS-LAST-CCYY            PIC 9(04).
           05  WS-CURRENT-SOURCE       PIC X(01).
               88  FROM-JOURNAL                   VALUE 'T'.
               88  FROM-SETTLED                   VALUE 'S'.
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-JRNL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-POSTED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-NO-CASH         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-SD-NOW          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-PENDING         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETL-POSTED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETL-NO-CASH         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETL-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETL-NO-BAL          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEG-PEND-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSERT-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UPDATE-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-YTD-RESET-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DIV-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WHT-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CSA-CNT              PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-JRNL-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-SETL-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TD-POSTED-AMT        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-SD-POSTED-AMT        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-DIV-AMT              PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-WHT-AMT              PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * PER CURRENCY TOTALS FOR THE STATISTICS DISPLAY                 *
      *----------------------------------------------------------------*
       01  WS-CCY-TOTALS.
           05  WS-CCY-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-CCY-ENTRY OCCURS 20 TIMES INDEXED BY CT-IDX.
               10  WS-CT-CCY           PIC X(03).
               10  WS-CT-TD-AMT        PIC S9(15)V99    COMP-3.
               10  WS-CT-SD-AMT        PIC S9(15)V99    COMP-3.
       COPY SRPSTJ.
       COPY SRACTV.
       COPY SRCSHA.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMJILNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           SET FROM-JOURNAL TO TRUE.
           PERFORM 8000-READ-JOURNAL THRU 8000-EXIT.
           PERFORM 2000-POST-JOURNAL THRU 2000-EXIT
               UNTIL END-OF-JOURNAL.
           SET FROM-SETTLED TO TRUE.
           PERFORM 8100-READ-SETTLED THRU 8100-EXIT.
           PERFORM 3000-POST-SETTLED THRU 3000-EXIT
               UNTIL END-OF-SETTLED.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           CALL 'CMASM01' USING JI-JOB-INFO.
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE DC-BUS-CCYY TO WS-BUS-CCYY.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'CASH POSTING STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT JRNLIN-FILE.
           IF WS-JRNLIN-STATUS NOT = '00'
               MOVE 'JRNLIN' TO AB-DDNAME
               MOVE WS-JRNLIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT SETLIN-FILE.
           IF WS-SETLIN-STATUS NOT = '00'
               MOVE 'SETLIN' TO AB-DDNAME
               MOVE WS-SETLIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O CASHBAL-FILE.
           IF WS-CASHBAL-STATUS NOT = '00'
               MOVE 'CASHBAL' TO AB-DDNAME
               MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT CASHACT-FILE.
           IF WS-CASHACT-STATUS NOT = '00'
               MOVE 'CASHACT' TO AB-DDNAME
               MOVE WS-CASHACT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
      * PASS A - TRADE DATE CASH FROM THE POSTING JOURNAL              *
      *================================================================*
       2000-POST-JOURNAL.
           ADD 1 TO WS-JRNL-READ.
           MOVE PSJ-ACTIVITY TO ACT-ACTIVITY-REC.
           ADD ACT-CASH-CHANGE TO WS-JRNL-CASH-HASH.
           IF ACT-ACCT-TYPE = 'ST'
               ADD 1 TO WS-JRNL-STREET
               GO TO 2000-NEXT
           END-IF.
           IF ACT-CASH-CHANGE = ZERO
               ADD 1 TO WS-JRNL-NO-CASH
               GO TO 2000-NEXT
           END-IF.
           MOVE ACT-CASH-CHANGE TO WS-CASH.
           IF ACT-SETTLED-TODAY OR ACT-NO-SETTLEMENT
               MOVE 'Y' TO WS-SETTLES-SW
           ELSE
               MOVE 'N' TO WS-SETTLES-SW
           END-IF.
           PERFORM 4000-GET-BALANCE THRU 4000-EXIT.
           MOVE CSH-TD-BALANCE TO WS-BEFORE-TD.
           MOVE CSH-SD-BALANCE TO WS-BEFORE-SD.
           ADD WS-CASH TO CSH-TD-BALANCE.
           ADD WS-CASH TO WS-TD-POSTED-AMT.
           IF CASH-SETTLES-NOW
               ADD WS-CASH TO CSH-SD-BALANCE
               ADD WS-CASH TO WS-SD-POSTED-AMT
               ADD 1 TO WS-JRNL-SD-NOW
           ELSE
               IF WS-CASH > ZERO
                   ADD WS-CASH TO CSH-PEND-CR
               ELSE
                   SUBTRACT WS-CASH FROM CSH-PEND-DR
               END-IF
               ADD 1 TO WS-JRNL-PENDING
           END-IF.
           PERFORM 2100-INCOME-TAX THRU 2100-EXIT.
           PERFORM 4100-PUT-BALANCE THRU 4100-EXIT.
           PERFORM 5000-WRITE-CASH-ACTV THRU 5000-EXIT.
           PERFORM 5100-CCY-TOTALS THRU 5100-EXIT.
           ADD 1 TO WS-JRNL-POSTED.
       2000-NEXT.
           PERFORM 8000-READ-JOURNAL THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * DIVIDEND INCOME AND WITHHOLDING YEAR TO DATE (CHG01880)        *
      *----------------------------------------------------------------*
       2100-INCOME-TAX.
           EVALUATE TRUE
               WHEN ACT-CASH-DIVIDEND
                   ADD WS-CASH TO CSH-INCOME-YTD
                   ADD WS-CASH TO WS-DIV-AMT
                   ADD 1 TO WS-DIV-CNT
      *            PAYMENT RELIEVES ANY RECEIVABLE CARRIED
                   IF CSH-DIV-RECEIVABLE > ZERO
                       IF CSH-DIV-RECEIVABLE > WS-CASH
                           SUBTRACT WS-CASH FROM CSH-DIV-RECEIVABLE
                       ELSE
                           MOVE ZERO TO CSH-DIV-RECEIVABLE
                       END-IF
                   END-IF
               WHEN ACT-WITHHOLDING
                   IF WS-CASH < ZERO
                       COMPUTE WS-ABS-CASH = WS-CASH * -1
                   ELSE
                       MOVE WS-CASH TO WS-ABS-CASH
                   END-IF
                   ADD WS-ABS-CASH TO CSH-WHT-YTD
                   ADD WS-ABS-CASH TO WS-WHT-AMT
                   ADD 1 TO WS-WHT-CNT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *================================================================*
      * PASS B - SETTLE DATE CASH FROM SRB250                          *
      *================================================================*
       3000-POST-SETTLED.
           ADD 1 TO WS-SETL-READ.
           ADD ACT-CASH-CHANGE TO WS-SETL-CASH-HASH.
           IF ACT-ACCT-TYPE = 'ST'
               ADD 1 TO WS-SETL-STREET
               GO TO 3000-NEXT
           END-IF.
           IF ACT-CASH-CHANGE = ZERO
               ADD 1 TO WS-SETL-NO-CASH
               GO TO 3000-NEXT
           END-IF.
           MOVE ACT-CASH-CHANGE TO WS-CASH.
           PERFORM 4000-GET-BALANCE THRU 4000-EXIT.
           IF NEW-BALANCE
               ADD 1 TO WS-SETL-NO-BAL
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               DISPLAY 'SRB300 SETTLEMENT WITHOUT CASH BALANCE '
                       CSH-KEY ' REF ' ACT-REF
           END-IF.
           MOVE CSH-TD-BALANCE TO WS-BEFORE-TD.
           MOVE CSH-SD-BALANCE TO WS-BEFORE-SD.
           ADD WS-CASH TO CSH-SD-BALANCE.
           ADD WS-CASH TO WS-SD-POSTED-AMT.
           IF WS-CASH > ZERO
               SUBTRACT WS-CASH FROM CSH-PEND-CR
               IF CSH-PEND-CR < ZERO
                   ADD 1 TO WS-NEG-PEND-CNT
                   MOVE ZERO TO CSH-PEND-CR
               END-IF
           ELSE
               ADD WS-CASH TO CSH-PEND-DR
               IF CSH-PEND-DR < ZERO
                   ADD 1 TO WS-NEG-PEND-CNT
                   MOVE ZERO TO CSH-PEND-DR
               END-IF
           END-IF.
           PERFORM 4100-PUT-BALANCE THRU 4100-EXIT.
           PERFORM 5000-WRITE-CASH-ACTV THRU 5000-EXIT.
           PERFORM 5100-CCY-TOTALS THRU 5100-EXIT.
           ADD 1 TO WS-SETL-POSTED.
       3000-NEXT.
           PERFORM 8100-READ-SETTLED THRU 8100-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
      * CASH BALANCE ROW - READ OR BUILD NEW                           *
      *================================================================*
       4000-GET-BALANCE.
           IF ACT-CCY = SPACES OR LOW-VALUES
               MOVE 'USD' TO ACT-CCY
           END-IF.
           MOVE ACT-ACCT-NO TO CSH-ACCT-NO.
           MOVE ACT-CCY     TO CSH-CCY.
           MOVE 'N' TO WS-NEW-BAL-SW.
           READ CASHBAL-FILE.
           EVALUATE TRUE
               WHEN CASHBAL-OK
                   CONTINUE
               WHEN CASHBAL-NOTFND
                   MOVE 'Y' TO WS-NEW-BAL-SW
                   INITIALIZE CSH-CASH-REC
                   MOVE ACT-ACCT-NO TO CSH-ACCT-NO
                   MOVE ACT-CCY     TO CSH-CCY
                   MOVE DC-BUS-DATE TO CSH-LAST-ACTV-DATE
               WHEN OTHER
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '4000-GET-BALANCE' TO AB-PARAGRAPH
                   MOVE CSH-KEY TO AB-KEY
                   MOVE 'CASH BALANCE READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
      *    1999-06-14 TLM - YTD FIGURES RESET ON FIRST TOUCH IN A NEW
      *    YEAR.  ACCOUNTS WITH NO ACTIVITY KEEP LAST YEAR'S FIGURES.
           IF CSH-LAST-ACTV-DATE NUMERIC
               MOVE CSH-LAST-ACTV-DATE (1:4) TO WS-LAST-CCYY
               IF WS-LAST-CCYY < WS-BUS-CCYY
                   MOVE ZERO TO CSH-INCOME-YTD CSH-WHT-YTD
                   ADD 1 TO WS-YTD-RESET-CNT
               END-IF
           END-IF.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-PUT-BALANCE.
      *----------------------------------------------------------------*
           MOVE DC-BUS-DATE TO CSH-LAST-ACTV-DATE.
           MOVE JI-JOBNAME  TO CSH-LAST-UPD-JOB.
           IF NEW-BALANCE
               WRITE CSH-CASH-REC
               IF NOT CASHBAL-OK
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '4100-PUT-BALANCE' TO AB-PARAGRAPH
                   MOVE CSH-KEY TO AB-KEY
                   MOVE 'CASH BALANCE WRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
               ADD 1 TO WS-INSERT-CNT
           ELSE
               REWRITE CSH-CASH-REC
               IF NOT CASHBAL-OK
                   MOVE 'CASHBAL' TO AB-DDNAME
                   MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '4100-PUT-BALANCE' TO AB-PARAGRAPH
                   MOVE CSH-KEY TO AB-KEY
                   MOVE 'CASH BALANCE REWRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
               ADD 1 TO WS-UPDATE-CNT
           END-IF.
       4100-EXIT.
           EXIT.
      *================================================================*
      * CASH ACTIVITY RECORD FOR SRR310                                *
      *================================================================*
       5000-WRITE-CASH-ACTV.
           MOVE SPACES           TO CSA-CASH-ACTV-REC.
           MOVE DC-BUS-DATE      TO CSA-BUS-DATE.
           MOVE CSH-ACCT-NO      TO CSA-ACCT-NO.
           MOVE CSH-CCY          TO CSA-CCY.
           MOVE WS-CURRENT-SOURCE TO CSA-BASIS.
           MOVE ACT-SOURCE       TO CSA-SOURCE.
           MOVE ACT-REF          TO CSA-REF.
           MOVE ACT-LEG-NO       TO CSA-LEG-NO.
           MOVE ACT-TYPE         TO CSA-ACT-TYPE.
           MOVE ACT-CUSIP        TO CSA-CUSIP.
           MOVE WS-CASH          TO CSA-AMOUNT.
           MOVE WS-BEFORE-TD     TO CSA-BEFORE-TD-BAL.
           MOVE CSH-TD-BALANCE   TO CSA-AFTER-TD-BAL.
           MOVE WS-BEFORE-SD     TO CSA-BEFORE-SD-BAL.
           MOVE CSH-SD-BALANCE   TO CSA-AFTER-SD-BAL.
           MOVE ACT-SETTLE-DATE  TO CSA-SETTLE-DATE.
           MOVE WS-NEW-BAL-SW    TO CSA-NEW-BAL-FLAG.
           MOVE ACT-DESC         TO CSA-DESC.
           WRITE CASHACT-REC FROM CSA-CASH-ACTV-REC.
           IF WS-CASHACT-STATUS NOT = '00'
               MOVE 'CASHACT' TO AB-DDNAME
               MOVE WS-CASHACT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '5000-WRITE-CASH-ACTV' TO AB-PARAGRAPH
               MOVE CSH-KEY TO AB-KEY
               MOVE 'CASH ACTIVITY WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-CSA-CNT.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5100-CCY-TOTALS.
      *----------------------------------------------------------------*
           SET CT-IDX TO 1.
           SEARCH WS-CCY-ENTRY
               AT END
                   DISPLAY 'SRB300 CURRENCY TOTAL TABLE FULL - '
                           CSH-CCY ' NOT TOTALLED'
               WHEN CT-IDX > WS-CCY-USED
                   ADD 1 TO WS-CCY-USED
                   MOVE CSH-CCY TO WS-CT-CCY (CT-IDX)
                   MOVE ZERO TO WS-CT-TD-AMT (CT-IDX)
                                WS-CT-SD-AMT (CT-IDX)
                   PERFORM 5110-ADD-CCY THRU 5110-EXIT
               WHEN WS-CT-CCY (CT-IDX) = CSH-CCY
                   PERFORM 5110-ADD-CCY THRU 5110-EXIT
           END-SEARCH.
       5100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5110-ADD-CCY.
      *----------------------------------------------------------------*
           IF FROM-JOURNAL
               ADD WS-CASH TO WS-CT-TD-AMT (CT-IDX)
               IF CASH-SETTLES-NOW
                   ADD WS-CASH TO WS-CT-SD-AMT (CT-IDX)
               END-IF
           ELSE
               ADD WS-CASH TO WS-CT-SD-AMT (CT-IDX)
           END-IF.
       5110-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-JOURNAL.
           READ JRNLIN-FILE INTO PSJ-JOURNAL-REC.
           EVALUATE TRUE
               WHEN JRNLIN-OK
                   CONTINUE
               WHEN JRNLIN-EOF
                   MOVE 'Y' TO WS-JRNL-EOF-SW
               WHEN OTHER
                   MOVE 'JRNLIN' TO AB-DDNAME
                   MOVE WS-JRNLIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-JOURNAL' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-SETTLED.
      *----------------------------------------------------------------*
           READ SETLIN-FILE INTO ACT-ACTIVITY-REC.
           EVALUATE TRUE
               WHEN SETLIN-OK
                   CONTINUE
               WHEN SETLIN-EOF
                   MOVE 'Y' TO WS-SETL-EOF-SW
               WHEN OTHER
                   MOVE 'SETLIN' TO AB-DDNAME
                   MOVE WS-SETLIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-SETTLED' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB300'       TO CT-STAGE.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '8500-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8500-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE JRNLIN-FILE SETLIN-FILE CASHACT-FILE.
           IF WS-CASHACT-STATUS NOT = '00'
               MOVE 'CASHACT' TO AB-DDNAME
               MOVE WS-CASHACT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE CASHBAL-FILE.
           IF WS-CASHBAL-STATUS NOT = '00'
               MOVE 'CASHBAL' TO AB-DDNAME
               MOVE WS-CASHBAL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF WS-NEG-PEND-CNT > ZERO AND WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           MOVE 'JRNL-IN'        TO CT-COUNTER-NAME.
           MOVE WS-JRNL-READ     TO CT-COUNT.
           MOVE WS-JRNL-CASH-HASH TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'SETTLED-IN'     TO CT-COUNTER-NAME.
           MOVE WS-SETL-READ     TO CT-COUNT.
           MOVE WS-SETL-CASH-HASH TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'TD-CASH-POSTED' TO CT-COUNTER-NAME.
           MOVE WS-JRNL-POSTED   TO CT-COUNT.
           MOVE WS-TD-POSTED-AMT TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'SD-CASH-POSTED' TO CT-COUNTER-NAME.
           COMPUTE CT-COUNT = WS-JRNL-SD-NOW + WS-SETL-POSTED.
           MOVE WS-SD-POSTED-AMT TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CASHACT-OUT'    TO CT-COUNTER-NAME.
           MOVE WS-CSA-CNT       TO CT-COUNT.
           COMPUTE CT-AMOUNT = WS-TD-POSTED-AMT + WS-SD-POSTED-AMT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'DIV-INCOME'     TO CT-COUNTER-NAME.
           MOVE WS-DIV-CNT       TO CT-COUNT.
           MOVE WS-DIV-AMT       TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'WHT-WITHHELD'   TO CT-COUNTER-NAME.
           MOVE WS-WHT-CNT       TO CT-COUNT.
           MOVE WS-WHT-AMT       TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* SRB300 - CASH POSTING                        *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-JRNL-READ TO WS-DISP-CNT.
           DISPLAY ' JOURNAL RECORDS READ     : ' WS-DISP-CNT.
           MOVE WS-JRNL-POSTED TO WS-DISP-CNT.
           DISPLAY '   CASH POSTED (TD)       : ' WS-DISP-CNT.
           MOVE WS-JRNL-SD-NOW TO WS-DISP-CNT.
           DISPLAY '     SETTLED ON POSTING   : ' WS-DISP-CNT.
           MOVE WS-JRNL-PENDING TO WS-DISP-CNT.
           DISPLAY '     PENDING SETTLEMENT   : ' WS-DISP-CNT.
           MOVE WS-JRNL-NO-CASH TO WS-DISP-CNT.
           DISPLAY '   NO CASH MOVEMENT       : ' WS-DISP-CNT.
           MOVE WS-JRNL-STREET TO WS-DISP-CNT.
           DISPLAY '   STREET SIDE LEGS       : ' WS-DISP-CNT.
           MOVE WS-SETL-READ TO WS-DISP-CNT.
           DISPLAY ' SETTLED RECORDS READ     : ' WS-DISP-CNT.
           MOVE WS-SETL-POSTED TO WS-DISP-CNT.
           DISPLAY '   CASH POSTED (SD)       : ' WS-DISP-CNT.
           MOVE WS-SETL-NO-BAL TO WS-DISP-CNT.
           DISPLAY '   WITHOUT BALANCE ROW    : ' WS-DISP-CNT.
           MOVE WS-NEG-PEND-CNT TO WS-DISP-CNT.
           DISPLAY ' NEGATIVE PENDING RESET   : ' WS-DISP-CNT.
           MOVE WS-INSERT-CNT TO WS-DISP-CNT.
           DISPLAY ' BALANCE ROWS ADDED       : ' WS-DISP-CNT.
           MOVE WS-UPDATE-CNT TO WS-DISP-CNT.
           DISPLAY ' BALANCE ROWS UPDATED     : ' WS-DISP-CNT.
           MOVE WS-YTD-RESET-CNT TO WS-DISP-CNT.
           DISPLAY ' YTD RESETS               : ' WS-DISP-CNT.
           MOVE WS-TD-POSTED-AMT TO WS-DISP-AMT.
           DISPLAY ' TD CASH POSTED           : ' WS-DISP-AMT.
           MOVE WS-SD-POSTED-AMT TO WS-DISP-AMT.
           DISPLAY ' SD CASH POSTED           : ' WS-DISP-AMT.
           MOVE WS-DIV-AMT TO WS-DISP-AMT.
           DISPLAY ' DIVIDEND INCOME          : ' WS-DISP-AMT.
           MOVE WS-WHT-AMT TO WS-DISP-AMT.
           DISPLAY ' WITHHOLDING              : ' WS-DISP-AMT.
           PERFORM VARYING CT-IDX FROM 1 BY 1
                   UNTIL CT-IDX > WS-CCY-USED
               MOVE WS-CT-TD-AMT (CT-IDX) TO WS-DISP-AMT
               DISPLAY '   ' WS-CT-CCY (CT-IDX) ' TD MOVEMENT        : '
                       WS-DISP-AMT
               MOVE WS-CT-SD-AMT (CT-IDX) TO WS-DISP-AMT
               DISPLAY '   ' WS-CT-CCY (CT-IDX) ' SD MOVEMENT        : '
                       WS-DISP-AMT
           END-PERFORM.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'CASH POSTING ENDED' TO AU-MESSAGE.
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
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'SRB300 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB300 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB300 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
