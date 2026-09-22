       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGR220.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MAY 1997.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGR220                                            *
      * DESCRIPTION: MARGIN CALL NOTICES.                              *
      *              ONE PRINTED NOTICE (ONE PAGE) PER CALL EVENT THAT *
      *              MUST BE SENT TO THE CLIENT:                       *
      *                NW  NEW CALL             - MARGIN CALL NOTICE   *
      *                EX  EXTENDED DUE DATE    - EXTENSION NOTICE     *
      *                LQ  PAST DUE             - FINAL NOTICE         *
      *              NAME AND ADDRESS FROM THE ACCOUNT MASTER (RETAIL  *
      *              OR INSTITUTIONAL VIEW BY ACCOUNT TYPE).  THE      *
      *              PRINT GOES TO THE MAIL ROOM (FORM MG-3, 1-PART).  *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD020 / STEP025                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              EVTIN    - MSEC.PROD.MG.CALLEVT(+1)       (MGCEVT)*
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS     (CMACCT)*
      * OUTPUT     : NOTICES  - NOTICE PRINT, FB 133 ASA               *
      * CALLS      : CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1997-05-12 RJK  ORIGINAL - MINIMUM EQUITY CALLS ADDED CHG03511 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2004-03-15 KAP  READS MG.CALLEVT, EXTENSION NOTICE    CHG12230 *
      * 2009-06-01 SPA  INSTITUTIONAL ADDRESS (BIC / LEI)     CHG18402 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT EVTIN-FILE     ASSIGN TO EVTIN
                  FILE STATUS IS WS-EVTIN-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCT-NO
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT NOTICES-FILE   ASSIGN TO NOTICES
                  FILE STATUS IS WS-NOTICES-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  EVTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGCEVT.
       FD  ACCTMAST-FILE.
           COPY CMACCT.
       FD  NOTICES-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  NOTICE-RECORD               PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGR220'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-EVTIN-STATUS         PIC X(02)  VALUE '00'.
               88  EVTIN-OK                       VALUE '00'.
               88  EVTIN-EOF                      VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-NOTICES-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-EVENTS                  VALUE 'Y'.
           05  WS-ACCT-FOUND-SW        PIC X(01)  VALUE 'N'.
               88  ACCOUNT-FOUND                  VALUE 'Y'.
      *----------------------------------------------------------------*
      * CALL TYPE WORDING (MARGIN DEPT FORM MG-3)                      *
      *----------------------------------------------------------------*
       01  WS-TYPE-TEXT-VALUES.
           05  FILLER  PIC X(02)  VALUE 'RT'.
           05  FILLER  PIC X(60)  VALUE
               'FEDERAL (REGULATION T) INITIAL MARGIN CALL'.
           05  FILLER  PIC X(02)  VALUE 'HM'.
           05  FILLER  PIC X(60)  VALUE
               'HOUSE MAINTENANCE MARGIN CALL'.
           05  FILLER  PIC X(02)  VALUE 'ME'.
           05  FILLER  PIC X(60)  VALUE
               'MINIMUM EQUITY CALL'.
       01  WS-TYPE-TEXT-TABLE REDEFINES WS-TYPE-TEXT-VALUES.
           05  WS-TT-ENTRY OCCURS 3 TIMES INDEXED BY TT-IDX.
               10  WS-TT-CODE          PIC X(02).
               10  WS-TT-TEXT          PIC X(60).
       01  WS-CALL-TYPE-TEXT           PIC X(60).
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NOTICE-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EXT-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FINAL-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-ADDR-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-AMOUNT           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-MONTH-NAMES.
           05  FILLER  PIC X(10)  VALUE 'JANUARY'.
           05  FILLER  PIC X(10)  VALUE 'FEBRUARY'.
           05  FILLER  PIC X(10)  VALUE 'MARCH'.
           05  FILLER  PIC X(10)  VALUE 'APRIL'.
           05  FILLER  PIC X(10)  VALUE 'MAY'.
           05  FILLER  PIC X(10)  VALUE 'JUNE'.
           05  FILLER  PIC X(10)  VALUE 'JULY'.
           05  FILLER  PIC X(10)  VALUE 'AUGUST'.
           05  FILLER  PIC X(10)  VALUE 'SEPTEMBER'.
           05  FILLER  PIC X(10)  VALUE 'OCTOBER'.
           05  FILLER  PIC X(10)  VALUE 'NOVEMBER'.
           05  FILLER  PIC X(10)  VALUE 'DECEMBER'.
       01  WS-MONTH-TABLE REDEFINES WS-MONTH-NAMES.
           05  WS-MONTH-NAME OCCURS 12 TIMES PIC X(10).
       01  WS-LONG-DATE                PIC X(20).
       01  WS-BUS-LONG-DATE            PIC X(20).
       01  WS-DAY-EDIT                 PIC Z9.
       01  WS-AMOUNT-EDIT              PIC $$$,$$$,$$$,$$9.99.
       01  WS-EQUITY-EDIT              PIC $$$,$$$,$$$,$$9.99-.
      *----------------------------------------------------------------*
      * NOTICE LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-NOTICE-LINE.
           05  NL-CC                   PIC X(01).
           05  FILLER                  PIC X(09).
           05  NL-TEXT                 PIC X(100).
           05  FILLER                  PIC X(23).
       01  WS-LETTERHEAD-1.
           05  FILLER                  PIC X(01)  VALUE '1'.
           05  FILLER                  PIC X(09)  VALUE SPACES.
           05  FILLER                  PIC X(60)  VALUE
               'MERIDIAN SECURITIES LLC'.
           05  LH-FORM                 PIC X(40)  VALUE
               '                        FORM MG-3'.
           05  FILLER                  PIC X(23)  VALUE SPACES.
       01  WS-LETTERHEAD-2.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(09)  VALUE SPACES.
           05  FILLER                  PIC X(60)  VALUE
               'MARGIN DEPARTMENT - ONE MERIDIAN PLAZA - NEW YORK NY'.
           05  FILLER                  PIC X(63)  VALUE SPACES.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-EVENT THRU 2000-EXIT
               UNTIL END-OF-EVENTS.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE ZERO TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MARGIN CALL NOTICES STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT EVTIN-FILE.
           IF WS-EVTIN-STATUS NOT = '00'
               MOVE 'EVTIN' TO AB-DDNAME
               MOVE WS-EVTIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT NOTICES-FILE.
           IF WS-NOTICES-STATUS NOT = '00'
               MOVE 'NOTICES' TO AB-DDNAME
               MOVE WS-NOTICES-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE DC-BUS-DATE TO WS-DATE-IN.
           PERFORM 8300-LONG-DATE THRU 8300-EXIT.
           MOVE WS-LONG-DATE TO WS-BUS-LONG-DATE.
           PERFORM 8000-READ-EVENT THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-EVENT.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF NOT MCE-EVT-NEW AND NOT MCE-EVT-EXTENDED
           AND NOT MCE-EVT-LIQUIDATE
               GO TO 2000-NEXT
           END-IF.
           PERFORM 2100-GET-ACCOUNT THRU 2100-EXIT.
           SET TT-IDX TO 1.
           SEARCH WS-TT-ENTRY
               AT END
                   MOVE 'MARGIN CALL' TO WS-CALL-TYPE-TEXT
               WHEN WS-TT-CODE (TT-IDX) = MCE-CALL-TYPE
                   MOVE WS-TT-TEXT (TT-IDX) TO WS-CALL-TYPE-TEXT
           END-SEARCH.
           PERFORM 3000-HEADER THRU 3000-EXIT.
           EVALUATE TRUE
               WHEN MCE-EVT-NEW
                   ADD 1 TO WS-NEW-CNT
                   PERFORM 4000-NEW-CALL-TEXT THRU 4000-EXIT
               WHEN MCE-EVT-EXTENDED
                   ADD 1 TO WS-EXT-CNT
                   PERFORM 4100-EXTENSION-TEXT THRU 4100-EXIT
               WHEN MCE-EVT-LIQUIDATE
                   ADD 1 TO WS-FINAL-CNT
                   PERFORM 4200-FINAL-TEXT THRU 4200-EXIT
           END-EVALUATE.
           PERFORM 5000-FOOTER THRU 5000-EXIT.
           ADD 1 TO WS-NOTICE-CNT.
           ADD MCE-CALL-AMOUNT TO WS-TOT-AMOUNT.
       2000-NEXT.
           PERFORM 8000-READ-EVENT THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-GET-ACCOUNT.
      *----------------------------------------------------------------*
           MOVE MCE-ACCT-NO TO ACCT-NO.
           READ ACCTMAST-FILE.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE 'Y' TO WS-ACCT-FOUND-SW
               WHEN ACCTMAST-NOTFND
                   MOVE 'N' TO WS-ACCT-FOUND-SW
                   ADD 1 TO WS-NO-ADDR-CNT
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE MCE-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *================================================================*
      * LETTERHEAD, DATE, ADDRESS BLOCK                                *
      *================================================================*
       3000-HEADER.
           IF MCE-EVT-LIQUIDATE
               MOVE '             FORM MG-3F  FINAL NOTICE' TO LH-FORM
           ELSE
               MOVE '                        FORM MG-3' TO LH-FORM
           END-IF.
           WRITE NOTICE-RECORD FROM WS-LETTERHEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE NOTICE-RECORD FROM WS-LETTERHEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-NOTICE-LINE.
           MOVE '-' TO NL-CC.
           MOVE WS-BUS-LONG-DATE TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           IF ACCOUNT-FOUND
               MOVE ACCT-NAME TO NL-TEXT
           ELSE
               MOVE 'ACCOUNT HOLDER' TO NL-TEXT
           END-IF.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           IF NOT ACCOUNT-FOUND
               STRING 'C/O BRANCH ' MCE-BRANCH ' REPRESENTATIVE '
                      MCE-REP DELIMITED BY SIZE INTO NL-TEXT
               PERFORM 8100-WRITE-LINE THRU 8100-EXIT
               GO TO 3000-ACCOUNT-LINE
           END-IF.
           EVALUATE TRUE
               WHEN ACCT-RETAIL
                   MOVE ACCT-RTL-ADDR-1 TO NL-TEXT
                   PERFORM 8100-WRITE-LINE THRU 8100-EXIT
                   IF ACCT-RTL-ADDR-2 NOT = SPACES
                       MOVE ACCT-RTL-ADDR-2 TO NL-TEXT
                       PERFORM 8100-WRITE-LINE THRU 8100-EXIT
                   END-IF
                   STRING ACCT-RTL-STATE '  ' ACCT-RTL-ZIP (1:5)
                          DELIMITED BY SIZE INTO NL-TEXT
                   PERFORM 8100-WRITE-LINE THRU 8100-EXIT
               WHEN ACCT-INSTITUTIONAL OR ACCT-OMNIBUS
                   MOVE 'ATTN: MARGIN / COLLATERAL OPERATIONS'
                                             TO NL-TEXT
                   PERFORM 8100-WRITE-LINE THRU 8100-EXIT
                   STRING 'SWIFT ' ACCT-INST-BIC '   LEI '
                          ACCT-INST-LEI DELIMITED BY SIZE INTO NL-TEXT
                   PERFORM 8100-WRITE-LINE THRU 8100-EXIT
               WHEN OTHER
                   STRING 'C/O BRANCH ' ACCT-BRANCH ' REPRESENTATIVE '
                          ACCT-REP DELIMITED BY SIZE INTO NL-TEXT
                   PERFORM 8100-WRITE-LINE THRU 8100-EXIT
           END-EVALUATE.
       3000-ACCOUNT-LINE.
           MOVE '0' TO NL-CC.
           STRING 'RE: ACCOUNT ' MCE-ACCT-NO '   BRANCH ' MCE-BRANCH
                  '   REPRESENTATIVE ' MCE-REP
                  DELIMITED BY SIZE INTO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE WS-CALL-TYPE-TEXT TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
      * BODY TEXT                                                      *
      *================================================================*
       4000-NEW-CALL-TEXT.
           MOVE MCE-CALL-AMOUNT TO WS-AMOUNT-EDIT.
           MOVE MCE-DUE-DATE    TO WS-DATE-IN.
           PERFORM 8300-LONG-DATE THRU 8300-EXIT.
           MOVE '-' TO NL-CC.
           MOVE 'DEAR CLIENT,' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           MOVE 'THE EQUITY IN YOUR MARGIN ACCOUNT IS BELOW THE'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE 'REQUIRED LEVEL.  TO MEET THIS CALL PLEASE DEPOSIT'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           STRING 'CASH OR MARGINABLE SECURITIES OF ' WS-AMOUNT-EDIT
                  ' NO LATER THAN ' WS-LONG-DATE
                  DELIMITED BY SIZE INTO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           MOVE 'IF THE CALL IS NOT MET BY THAT DATE, MERIDIAN MAY SELL'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE 'SECURITIES IN YOUR ACCOUNT WITHOUT FURTHER' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE 'NOTICE TO YOU, AS PROVIDED IN YOUR MARGIN AGREEMENT.'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-EXTENSION-TEXT.
      *----------------------------------------------------------------*
           MOVE MCE-CALL-AMOUNT TO WS-AMOUNT-EDIT.
           MOVE MCE-DUE-DATE    TO WS-DATE-IN.
           PERFORM 8300-LONG-DATE THRU 8300-EXIT.
           MOVE '-' TO NL-CC.
           MOVE 'DEAR CLIENT,' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           STRING 'AT YOUR REQUEST THE DUE DATE OF THE CALL FOR '
                  WS-AMOUNT-EDIT ' HAS BEEN EXTENDED'
                  DELIMITED BY SIZE INTO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           STRING 'TO ' WS-LONG-DATE '.  NO FURTHER EXTENSION CAN BE '
                  'GRANTED AFTER THE SECOND.'
                  DELIMITED BY SIZE INTO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4200-FINAL-TEXT.
      *----------------------------------------------------------------*
           MOVE MCE-CURR-DEFICIT TO WS-AMOUNT-EDIT.
           MOVE MCE-EQUITY       TO WS-EQUITY-EDIT.
           MOVE '-' TO NL-CC.
           MOVE 'DEAR CLIENT,' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           MOVE 'THE MARGIN CALL REFERRED TO ABOVE HAS NOT BEEN MET BY'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE 'ITS DUE DATE.' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           STRING 'THE DEFICIENCY IS NOW ' WS-AMOUNT-EDIT
                  ' ON ACCOUNT EQUITY OF ' WS-EQUITY-EDIT '.'
                  DELIMITED BY SIZE INTO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           MOVE 'MERIDIAN WILL LIQUIDATE POSITIONS IN YOUR ACCOUNT TO'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE 'COVER THE DEFICIENCY.' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       4200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5000-FOOTER.
      *----------------------------------------------------------------*
           MOVE '-' TO NL-CC.
           MOVE 'QUESTIONS: CALL YOUR REPRESENTATIVE OR THE MARGIN'
                                TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE ' ' TO NL-CC.
           MOVE 'DEPARTMENT AT 1-800-555-0144.' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
           MOVE '0' TO NL-CC.
           MOVE 'MARGIN DEPARTMENT' TO NL-TEXT.
           PERFORM 8100-WRITE-LINE THRU 8100-EXIT.
       5000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-EVENT.
      *================================================================*
           READ EVTIN-FILE.
           EVALUATE TRUE
               WHEN EVTIN-OK
                   CONTINUE
               WHEN EVTIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'EVTIN' TO AB-DDNAME
                   MOVE WS-EVTIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-WRITE-LINE.
      *----------------------------------------------------------------*
           WRITE NOTICE-RECORD FROM WS-NOTICE-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO NL-TEXT.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CCYYMMDD IN WS-DATE-IN -> 'SEPTEMBER 24, 2026'                 *
      *----------------------------------------------------------------*
       8300-LONG-DATE.
           MOVE SPACES TO WS-LONG-DATE.
           IF WS-DATE-IN NOT NUMERIC OR WS-DI-MM < 1 OR WS-DI-MM > 12
               MOVE 'UNKNOWN DATE' TO WS-LONG-DATE
               GO TO 8300-EXIT
           END-IF.
           MOVE WS-DI-DD TO WS-DAY-EDIT.
           STRING WS-MONTH-NAME (WS-DI-MM) DELIMITED BY SPACE
                  ' ' WS-DAY-EDIT DELIMITED BY SIZE
                  ', ' WS-DI-CCYY DELIMITED BY SIZE
                  INTO WS-LONG-DATE.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-NOTICES-STATUS NOT = '00'
               MOVE 'NOTICES' TO AB-DDNAME
               MOVE WS-NOTICES-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'NOTICE WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE EVTIN-FILE ACCTMAST-FILE NOTICES-FILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'MGR220'        TO CT-STAGE.
           MOVE 'NOTICES-OUT'   TO CT-COUNTER-NAME.
           MOVE WS-NOTICE-CNT   TO CT-COUNT.
           MOVE WS-TOT-AMOUNT   TO CT-AMOUNT.
           MOVE ZERO            TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'MGR220 EVENTS READ         : ' WS-READ-CNT.
           DISPLAY 'MGR220 NOTICES PRINTED     : ' WS-NOTICE-CNT.
           DISPLAY 'MGR220   NEW CALL          : ' WS-NEW-CNT.
           DISPLAY 'MGR220   EXTENSION         : ' WS-EXT-CNT.
           DISPLAY 'MGR220   FINAL NOTICE      : ' WS-FINAL-CNT.
           DISPLAY 'MGR220 NO ADDRESS ON FILE  : ' WS-NO-ADDR-CNT.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           MOVE 'MARGIN CALL NOTICES ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'MGR220 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
