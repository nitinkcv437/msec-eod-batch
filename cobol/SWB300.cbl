       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWB300.
       AUTHOR.        T L MORGAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  06/21/1999.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWB300                                            *
      * TITLE      : SETTLEMENT FAILS, CLOSE-OUT AND BUY-IN            *
      * JOB        : MSSWD030  STEP010                                 *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   BROWSES THE SETTLEMENT INSTRUCTION MASTER.  EVERY NEW        *
      *   INSTRUCTION (NEWM) THAT IS NOT SETTLED OR CANCELLED AND      *
      *   WHOSE CONTRACTUAL SETTLE DATE IS BEFORE THE BUSINESS DATE    *
      *   IS A FAIL:                                                   *
      *     DIRECTION    D FAIL TO DELIVER (MT543)                     *
      *                  R FAIL TO RECEIVE (MT541)                     *
      *     AGE          BUSINESS DAYS SINCE THE SETTLE DATE (CMU010)  *
      *     CLOSE-OUT    FAIL TO DELIVER: SETTLE DATE + 1 BUSINESS DAY *
      *                  FOR A SHORT SALE, + 3 FOR A LONG SALE (REG    *
      *                  SHO RULE 204).  SHORT = TRADE SIDE 'SS' ON    *
      *                  THE TRADE HISTORY.                            *
      *     ACTION       BI  BUY-IN, FAIL OLDER THAN 10 BUSINESS DAYS  *
      *                  CD  CLOSE-OUT DUE (DELIVER, DATE REACHED)     *
      *                  PN  EUROCLEAR SETTLEMENT PENALTY (CSDR)       *
      *                  MO  MONITOR                                   *
      *   THE MASTER IS UPDATED WITH AGE / CLOSE-OUT / BUY-IN FLAG,    *
      *   OPEN INSTRUCTIONS PAST SETTLE DATE GO TO STATUS FL.          *
      *   HOUSEKEEPING: SETTLED AND CANCELLED ROWS OLDER THAN THE      *
      *   PURGE PERIOD (PURGE=, CALENDAR DAYS) ARE DELETED.  THE       *
      *   SEQUENCE CONTROL ROW IS NEVER TOUCHED.                       *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETERS SWP300A (FX=, PURGE=)            *
      *          TRDHIST   MSEC.PROD.TC.TRDHIST.KSDS      (TCTRDHS)    *
      * UPDATE : SWINSTR   MSEC.PROD.SW.INSTR.KSDS        (SWINSTR)    *
      * OUTPUT : FAILOUT   MSEC.PROD.SW.FAILS(+1)         (SWFAIL)     *
      * CALLS  : CMU010 CMASM01 CMU050 CMU060 CMU080                   *
      *                                                                *
      * RETURN CODES: 0 CLEAN OR MONITOR ONLY                          *
      *               4 CLOSE-OUTS DUE OR BUY-INS (RUNBOOK SW-05)      *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1999-06-21 TLM  CHG05230  ORIGINAL - FAILS FROM MT548 STATUS   *
      * 2001-04-09 KAP  CHG08820  DECIMALIZATION                       *
      * 2004-01-12 KAP  CHG11650  BUY-IN AFTER 10 BUSINESS DAYS        *
      * 2008-01-14 SPA  CHG17720  REG SHO CLOSE-OUT: SHORT S+1, LONG   *
      *                           S+3 FROM TRADE HISTORY SIDE          *
      * 2009-12-14 SPA  CHG19002  EUROCLEAR, DESK FX RATES (SYSIN)     *
      * 2011-06-20 SPA  CHG21877  PURGE OF SETTLED / CANCELLED ROWS    *
      * 2022-02-01 MHC  CHG38804  CSDR PENALTY FLAG FOR EUROCLEAR      *
      * 2024-05-20 NVR  CHG40551  T+1 - CLOSE-OUT DAYS UNCHANGED,      *
      *                           COMPLIANCE REVIEW OPEN               *
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
           SELECT SWINSTR-FILE  ASSIGN TO SWINSTR
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD KEY IS SWI-SENDER-REF
                                FILE STATUS IS WS-SWINSTR-FS.
           SELECT TRDHIST-FILE  ASSIGN TO TRDHIST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS RANDOM
                                RECORD KEY IS TH-TRADE-ID
                                FILE STATUS IS WS-TRDHIST-FS.
           SELECT FAILOUT-FILE  ASSIGN TO FAILOUT
                                FILE STATUS IS WS-FAILOUT-FS.
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
       FD  SWINSTR-FILE.
       COPY SWINSTR.
       FD  TRDHIST-FILE.
       COPY TCTRDHS.
       FD  FAILOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SWFAIL.
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWB300'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-SWINSTR-FS           PIC X(02).
               88  SWINSTR-OK                    VALUE '00'.
               88  SWINSTR-EOF                   VALUE '10'.
               88  SWINSTR-NOTFND                VALUE '23'.
           05  WS-TRDHIST-FS           PIC X(02).
               88  TRDHIST-OK                    VALUE '00'.
               88  TRDHIST-NOTFND                VALUE '23'.
           05  WS-FAILOUT-FS           PIC X(02).
               88  FAILOUT-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-MASTER              VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-SHORT-SW             PIC X(01)  VALUE 'N'.
               88  WS-SHORT-SALE                 VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * CLOSE-OUT AND BUY-IN RULES (BUSINESS DAYS)                     *
      *----------------------------------------------------------------*
       01  WS-FAIL-RULES.
           05  WS-CLOSEOUT-SHORT-DAYS  PIC S9(03) COMP-3 VALUE +1.
           05  WS-CLOSEOUT-LONG-DAYS   PIC S9(03) COMP-3 VALUE +3.
           05  WS-BUYIN-AGE            PIC S9(03) COMP-3 VALUE +10.
           05  WS-PENALTY-DEPOSITORY   PIC X(04)  VALUE 'EUCL'.
           05  WS-NO-RESPONSE-DAYS     PIC S9(03) COMP-3 VALUE +1.
      *
      *----------------------------------------------------------------*
      * REG SHO THRESHOLD WATCH LIST: AGGREGATE FAILS TO DELIVER PER   *
      * CUSIP.  10,000 SHARES FAILING FOR 5 SETTLEMENT DAYS - REPORTED *
      * TO COMPLIANCE (THE 0.5% OF SHARES OUTSTANDING TEST IS DONE BY  *
      * COMPLIANCE, NOT HERE).                                         *
      *----------------------------------------------------------------*
       01  WS-THRESHOLD-RULES.
           05  WS-THR-MIN-QTY          PIC S9(11)V9(04) COMP-3
                                       VALUE +10000.
           05  WS-THR-MIN-AGE          PIC S9(03) COMP-3 VALUE +5.
       01  WS-THRESHOLD-TABLE.
           05  WS-THR-COUNT            PIC S9(04) COMP VALUE ZERO.
           05  WS-THR-MAX              PIC S9(04) COMP VALUE 2000.
           05  WS-THR-ENTRY            OCCURS 2000 TIMES
                                       INDEXED BY WS-THR-IDX.
               10  WS-THR-CUSIP        PIC X(09).
               10  WS-THR-QTY          PIC S9(13)V9(04) COMP-3.
               10  WS-THR-MAX-AGE      PIC S9(05) COMP-3.
               10  WS-THR-FAILS        PIC S9(05) COMP-3.
       01  WS-THR-DISP-QTY             PIC ZZZ,ZZZ,ZZZ,ZZ9.
       01  WS-THR-DISP-AGE             PIC ZZ9.
       01  WS-THR-DISP-CNT             PIC ZZ,ZZ9.
      *
      *----------------------------------------------------------------*
      * DESK FX RATES TO USD (SWP300A FX= CARDS OVERRIDE / ADD)        *
      *----------------------------------------------------------------*
       01  WS-FX-VALUES.
           05  FILLER  PIC X(15)  VALUE 'USD000100000000'.
           05  FILLER  PIC X(15)  VALUE 'EUR000108000000'.
           05  FILLER  PIC X(15)  VALUE 'GBP000125000000'.
           05  FILLER  PIC X(15)  VALUE 'CHF000110000000'.
           05  FILLER  PIC X(15)  VALUE 'CAD000074000000'.
           05  FILLER  PIC X(15)  VALUE 'JPY000000700000'.
           05  FILLER  PIC X(15)  VALUE SPACES.
           05  FILLER  PIC X(15)  VALUE SPACES.
           05  FILLER  PIC X(15)  VALUE SPACES.
           05  FILLER  PIC X(15)  VALUE SPACES.
       01  WS-FX-TABLE REDEFINES WS-FX-VALUES.
           05  WS-FX-ENTRY             OCCURS 10 TIMES
                                       INDEXED BY WS-FX-IDX.
               10  WS-FX-CCY           PIC X(03).
               10  WS-FX-RATE          PIC 9(04)V9(08).
       01  WS-FX-CARD.
           05  WS-FXC-CCY              PIC X(03).
           05  WS-FXC-RATE-TXT         PIC X(20).
           05  WS-FXC-INT-TXT          PIC X(06).
           05  WS-FXC-DEC-TXT          PIC X(08).
           05  WS-FXC-INT              PIC 9(04).
           05  WS-FXC-DEC              PIC 9(08).
           05  WS-FXC-DEC-V REDEFINES WS-FXC-DEC PIC V9(08).
      *
       01  WS-PARMS.
           05  WS-PURGE-DAYS           PIC 9(03)  VALUE 030.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(40).
       01  WS-DATES.
           05  WS-PURGE-DATE           PIC 9(08).
           05  WS-NO-RESPONSE-DATE     PIC 9(08).
           05  WS-CLOSEOUT-DATE        PIC 9(08).
      *
       01  WS-FAIL-WORK.
           05  WS-AGE                  PIC S9(05) COMP-3.
           05  WS-OPEN-QTY             PIC S9(11)V9(04) COMP-3.
           05  WS-OPEN-AMT             PIC S9(15)V99    COMP-3.
           05  WS-MV-USD               PIC S9(15)V99    COMP-3.
           05  WS-RATE                 PIC 9(04)V9(08).
           05  WS-ACTION               PIC X(02).
           05  WS-DIRECTION            PIC X(01).
           05  WS-REASON               PIC X(04).
      *
       01  WS-COUNTERS.
           05  WS-ROWS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-ROWS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLOSED-ROWS          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NOT-DUE              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAILS-OUT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAILS-DLV            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAILS-RCV            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-FAILS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACT-MONITOR          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACT-CLOSEOUT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACT-BUYIN            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACT-PENALTY          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SHORT-SALES          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HIST-MISSING         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-RESPONSE          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PURGED               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-MISSING           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-THRESHOLD-SECS       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-THR-OVERFLOW         PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-FAIL-MV-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-FAIL-QTY-TOTAL       PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-OLDEST-AGE           PIC S9(05) COMP-3 VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-SEQ-CTL-KEY          PIC X(16)  VALUE 'SEQCONTROL'.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
      *
       COPY CMDTLNK.
       COPY CMJILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-ROW    THRU 2000-EXIT
               UNTIL WS-END-OF-MASTER
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
           READ DATECARD-FILE
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
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'SETTLEMENT FAILS / CLOSE-OUT STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           CALL 'CMASM01' USING JI-JOB-INFO
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM  THRU 1100-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
      *
      *    PURGE LIMIT (CALENDAR) AND NO-RESPONSE LIMIT (BUSINESS)
           MOVE 'ADDC'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE DC-BUS-DATE            TO DT-DATE-1
           COMPUTE DT-DAYS = ZERO - WS-PURGE-DAYS
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'CMU010 ADDC FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE DT-RESULT-DATE         TO WS-PURGE-DATE
           MOVE 'ADDB'                 TO DT-FUNCTION
           MOVE DC-BUS-DATE            TO DT-DATE-1
           COMPUTE DT-DAYS = ZERO - WS-NO-RESPONSE-DAYS
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE DC-PREV-BUS-DATE   TO WS-NO-RESPONSE-DATE
           ELSE
               MOVE DT-RESULT-DATE     TO WS-NO-RESPONSE-DATE
           END-IF
           DISPLAY 'SWB300 - PURGE SETTLED/CANCELLED BEFORE '
                   WS-PURGE-DATE
      *
           OPEN I-O SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN INPUT TRDHIST-FILE
           IF NOT TRDHIST-OK
               MOVE 'TRDHIST'          TO AB-DDNAME
               MOVE WS-TRDHIST-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT FAILOUT-FILE
           IF NOT FAILOUT-OK
               MOVE 'FAILOUT'          TO AB-DDNAME
               MOVE WS-FAILOUT-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
      *
           MOVE LOW-VALUES             TO SWI-SENDER-REF
           START SWINSTR-FILE KEY IS NOT LESS THAN SWI-SENDER-REF
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   PERFORM 8000-READ-NEXT THRU 8000-EXIT
               WHEN SWINSTR-NOTFND
                   SET WS-END-OF-MASTER TO TRUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 1100 - PARAMETER CARDS  FX=CCY N.NNNNNNNN   PURGE=NNN          *
      *----------------------------------------------------------------*
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
           UNSTRING PARM-CARD-REC DELIMITED BY '='
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'FX'
                   PERFORM 1200-FX-CARD THRU 1200-EXIT
               WHEN 'PURGE'
                   IF WS-PARM-VALUE (1:3) NUMERIC
                       MOVE WS-PARM-VALUE (1:3) TO WS-PURGE-DAYS
                   ELSE
                       DISPLAY 'SWB300 - PURGE NOT NUMERIC, IGNORED'
                   END-IF
               WHEN OTHER
                   DISPLAY 'SWB300 - UNKNOWN PARAMETER IGNORED: '
                           PARM-CARD-REC (1:40)
           END-EVALUATE.
       1100-EXIT.
           EXIT.
      *
       1200-FX-CARD.
           MOVE SPACES                 TO WS-FXC-CCY WS-FXC-RATE-TXT
                                          WS-FXC-INT-TXT WS-FXC-DEC-TXT
           UNSTRING WS-PARM-VALUE DELIMITED BY ALL SPACE
               INTO WS-FXC-CCY WS-FXC-RATE-TXT
           END-UNSTRING
           UNSTRING WS-FXC-RATE-TXT DELIMITED BY '.' OR SPACE
               INTO WS-FXC-INT-TXT WS-FXC-DEC-TXT
           END-UNSTRING
           INSPECT WS-FXC-DEC-TXT REPLACING ALL SPACE BY ZERO
           IF WS-FXC-INT-TXT = SPACES
               MOVE ZERO               TO WS-FXC-INT
           ELSE
               IF FUNCTION TRIM (WS-FXC-INT-TXT) NOT NUMERIC
                   DISPLAY 'SWB300 - FX CARD INVALID, IGNORED: '
                           PARM-CARD-REC (1:40)
                   GO TO 1200-EXIT
               END-IF
               MOVE FUNCTION TRIM (WS-FXC-INT-TXT) TO WS-FXC-INT
           END-IF
           IF WS-FXC-DEC-TXT NOT NUMERIC
               DISPLAY 'SWB300 - FX CARD INVALID, IGNORED: '
                       PARM-CARD-REC (1:40)
               GO TO 1200-EXIT
           END-IF
           MOVE WS-FXC-DEC-TXT         TO WS-FXC-DEC
           SET WS-FX-IDX               TO 1
           SEARCH WS-FX-ENTRY
               AT END
                   DISPLAY 'SWB300 - FX TABLE FULL, IGNORED: '
                           WS-FXC-CCY
               WHEN WS-FX-CCY (WS-FX-IDX) = WS-FXC-CCY
                   COMPUTE WS-FX-RATE (WS-FX-IDX) =
                           WS-FXC-INT + WS-FXC-DEC-V
               WHEN WS-FX-CCY (WS-FX-IDX) = SPACES
                   MOVE WS-FXC-CCY     TO WS-FX-CCY (WS-FX-IDX)
                   COMPUTE WS-FX-RATE (WS-FX-IDX) =
                           WS-FXC-INT + WS-FXC-DEC-V
           END-SEARCH
           DISPLAY 'SWB300 - DESK RATE ' WS-FXC-CCY ' '
                   WS-FXC-INT '.' WS-FXC-DEC.
       1200-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - ONE INSTRUCTION MASTER ROW                              *
      *================================================================*
       2000-PROCESS-ROW.
           ADD 1                       TO WS-ROWS-READ
           IF SWI-SENDER-REF = WS-SEQ-CTL-KEY
               GO TO 2000-NEXT
           END-IF
      *
      *    HOUSEKEEPING - CLOSED ROWS PAST THE PURGE PERIOD
           IF (SWI-SETTLED OR SWI-CANCELLED)
           AND SWI-LAST-STATUS-DATE < WS-PURGE-DATE
               PERFORM 2900-PURGE-ROW  THRU 2900-EXIT
               GO TO 2000-NEXT
           END-IF
           IF SWI-FUNCTION = 'CANC'
               ADD 1                   TO WS-CANC-ROWS
               GO TO 2000-NEXT
           END-IF
           ADD 1                       TO WS-INSTR-READ
           IF SWI-SETTLED OR SWI-CANCELLED
               ADD 1                   TO WS-CLOSED-ROWS
               GO TO 2000-NEXT
           END-IF
      *
           IF SWI-SETTLE-DATE < DC-BUS-DATE
               PERFORM 3000-PROCESS-FAIL THRU 3000-EXIT
           ELSE
               ADD 1                   TO WS-NOT-DUE
               IF SWI-SENT
               AND SWI-SENT-DATE < WS-NO-RESPONSE-DATE
                   ADD 1               TO WS-NO-RESPONSE
               END-IF
           END-IF.
       2000-NEXT.
           PERFORM 8000-READ-NEXT      THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2900-PURGE-ROW.
           DELETE SWINSTR-FILE RECORD
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE '2900-PURGE-ROW'   TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-PURGED.
       2900-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - FAILED INSTRUCTION                                      *
      *================================================================*
       3000-PROCESS-FAIL.
           MOVE 'DIFB'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE SWI-SETTLE-DATE        TO DT-DATE-1
           MOVE DC-BUS-DATE            TO DT-DATE-2
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '3000-PROCESS-FAIL' TO AB-PARAGRAPH
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE 'CMU010 DIFB FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE DT-RESULT-NUM          TO WS-AGE
           IF WS-AGE < 1
               MOVE 1                  TO WS-AGE
           END-IF
           IF SWI-FAIL-AGE = ZERO
               ADD 1                   TO WS-NEW-FAILS
           END-IF
      *
           IF SWI-MSG-TYPE = '543'
               MOVE 'D'                TO WS-DIRECTION
               ADD 1                   TO WS-FAILS-DLV
           ELSE
               MOVE 'R'                TO WS-DIRECTION
               ADD 1                   TO WS-FAILS-RCV
           END-IF
      *
           COMPUTE WS-OPEN-QTY = SWI-QTY - SWI-SETTLED-QTY
           COMPUTE WS-OPEN-AMT = SWI-AMOUNT - SWI-SETTLED-AMOUNT
           PERFORM 3100-MARKET-VALUE   THRU 3100-EXIT
           PERFORM 3200-CLOSEOUT-DATE  THRU 3200-EXIT
           PERFORM 3300-DECIDE-ACTION  THRU 3300-EXIT
           PERFORM 3400-UPDATE-MASTER  THRU 3400-EXIT
           PERFORM 3500-WRITE-FAIL     THRU 3500-EXIT
           IF WS-DIRECTION = 'D'
               PERFORM 3600-THRESHOLD-ADD THRU 3600-EXIT
           END-IF.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3100 - OPEN AMOUNT IN USD AT THE DESK RATE                     *
      *----------------------------------------------------------------*
       3100-MARKET-VALUE.
           MOVE ZERO                   TO WS-RATE
           SET WS-FX-IDX               TO 1
           SEARCH WS-FX-ENTRY
               AT END
                   CONTINUE
               WHEN WS-FX-CCY (WS-FX-IDX) = SWI-CCY
                   MOVE WS-FX-RATE (WS-FX-IDX) TO WS-RATE
           END-SEARCH
           IF WS-RATE = ZERO
               ADD 1                   TO WS-FX-MISSING
               DISPLAY 'SWB300 - NO DESK RATE FOR ' SWI-CCY
                       ' - ' SWI-SENDER-REF ' VALUED AT ZERO'
           END-IF
           COMPUTE WS-MV-USD ROUNDED = WS-OPEN-AMT * WS-RATE.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3200 - REG SHO CLOSE-OUT DATE FOR FAILS TO DELIVER             *
      *----------------------------------------------------------------*
       3200-CLOSEOUT-DATE.
           MOVE ZERO                   TO WS-CLOSEOUT-DATE
           MOVE 'N'                    TO WS-SHORT-SW
           IF WS-DIRECTION NOT = 'D'
               GO TO 3200-EXIT
           END-IF
           MOVE SWI-SENDER-REF         TO TH-TRADE-ID
           READ TRDHIST-FILE
           EVALUATE TRUE
               WHEN TRDHIST-OK
                   IF TH-SIDE = 'SS'
                       SET WS-SHORT-SALE TO TRUE
                       ADD 1           TO WS-SHORT-SALES
                   END-IF
               WHEN TRDHIST-NOTFND
                   ADD 1               TO WS-HIST-MISSING
               WHEN OTHER
                   MOVE 'TRDHIST'      TO AB-DDNAME
                   MOVE WS-TRDHIST-FS  TO AB-FILE-STATUS
                   MOVE SWI-SENDER-REF TO AB-KEY
                   MOVE '3200-CLOSEOUT-DATE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE
           MOVE 'ADDB'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE SWI-SETTLE-DATE        TO DT-DATE-1
           IF WS-SHORT-SALE
               MOVE WS-CLOSEOUT-SHORT-DAYS TO DT-DAYS
           ELSE
               MOVE WS-CLOSEOUT-LONG-DAYS  TO DT-DAYS
           END-IF
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '3200-CLOSEOUT-DATE' TO AB-PARAGRAPH
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE 'CMU010 ADDB FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE DT-RESULT-DATE         TO WS-CLOSEOUT-DATE.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3300 - ACTION: BUY-IN, CLOSE-OUT DUE, PENALTY, MONITOR         *
      *----------------------------------------------------------------*
       3300-DECIDE-ACTION.
           EVALUATE TRUE
               WHEN WS-AGE > WS-BUYIN-AGE
                   SET SWF-ACT-BUYIN   TO TRUE
                   ADD 1               TO WS-ACT-BUYIN
               WHEN WS-DIRECTION = 'D'
                AND DC-BUS-DATE NOT < WS-CLOSEOUT-DATE
                   SET SWF-ACT-CLOSEOUT-DUE TO TRUE
                   ADD 1               TO WS-ACT-CLOSEOUT
               WHEN SWI-DEPOSITORY = WS-PENALTY-DEPOSITORY
                   SET SWF-ACT-PENALTY TO TRUE
                   ADD 1               TO WS-ACT-PENALTY
               WHEN OTHER
                   SET SWF-ACT-MONITOR TO TRUE
                   ADD 1               TO WS-ACT-MONITOR
           END-EVALUATE
           MOVE SWF-ACTION             TO WS-ACTION
      *
      *    REASON: CUSTODIAN REASON, ELSE WHAT WE KNOW OF THE STATUS
           EVALUATE TRUE
               WHEN SWI-REASON-CODE NOT = SPACES
                   MOVE SWI-REASON-CODE TO WS-REASON
               WHEN SWI-SENT
                   MOVE 'NORS'         TO WS-REASON
               WHEN SWI-UNMATCHED
                   MOVE 'NMAT'         TO WS-REASON
               WHEN SWI-REJECTED
                   MOVE 'REJT'         TO WS-REASON
               WHEN SWI-PARTIAL
                   MOVE 'PART'         TO WS-REASON
               WHEN SWI-STATUS-CODE NOT = SPACES
                   MOVE SWI-STATUS-CODE TO WS-REASON
               WHEN OTHER
                   MOVE 'UNKN'         TO WS-REASON
           END-EVALUATE.
       3300-EXIT.
           EXIT.
      *
       3400-UPDATE-MASTER.
           MOVE WS-AGE                 TO SWI-FAIL-AGE
           MOVE WS-CLOSEOUT-DATE       TO SWI-CLOSEOUT-DATE
           IF WS-ACTION = 'BI'
               MOVE 'Y'                TO SWI-BUYIN-FLAG
           END-IF
           IF SWI-SENT OR SWI-MATCHED OR SWI-UNMATCHED OR SWI-PENDING
               SET SWI-FAILED          TO TRUE
           END-IF
           MOVE JI-JOBNAME             TO SWI-LAST-UPD-JOB
           REWRITE SWI-INSTR-REC
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE '3400-UPDATE-MASTER' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       3400-EXIT.
           EXIT.
      *
       3500-WRITE-FAIL.
           INITIALIZE SWF-FAIL-REC
           MOVE DC-BUS-DATE            TO SWF-BUS-DATE
           MOVE SWI-SENDER-REF         TO SWF-SENDER-REF
           MOVE WS-DIRECTION           TO SWF-DIRECTION
           MOVE SWI-ACCT-NO            TO SWF-ACCT-NO
           MOVE SWI-CUSIP              TO SWF-CUSIP
           MOVE SWI-SETTLE-DATE        TO SWF-SETTLE-DATE
           MOVE WS-AGE                 TO SWF-AGE-BUS-DAYS
           MOVE WS-OPEN-QTY            TO SWF-QTY
           MOVE WS-OPEN-AMT            TO SWF-AMOUNT
           MOVE WS-MV-USD              TO SWF-MKT-VALUE-USD
           MOVE WS-REASON              TO SWF-REASON-CODE
           MOVE WS-CLOSEOUT-DATE       TO SWF-CLOSEOUT-DATE
           MOVE WS-ACTION              TO SWF-ACTION
           WRITE SWF-FAIL-REC
           IF NOT FAILOUT-OK
               MOVE 'FAILOUT'          TO AB-DDNAME
               MOVE WS-FAILOUT-FS      TO AB-FILE-STATUS
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE '3500-WRITE-FAIL'  TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-FAILS-OUT
           ADD WS-MV-USD               TO WS-FAIL-MV-TOTAL
           ADD WS-OPEN-QTY             TO WS-FAIL-QTY-TOTAL
           IF WS-AGE > WS-OLDEST-AGE
               MOVE WS-AGE             TO WS-OLDEST-AGE
           END-IF.
       3500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3600 - ACCUMULATE FAILS TO DELIVER BY CUSIP                    *
      *----------------------------------------------------------------*
       3600-THRESHOLD-ADD.
           SET WS-THR-IDX              TO 1
           SEARCH WS-THR-ENTRY
               AT END
                   ADD 1               TO WS-THR-OVERFLOW
               WHEN WS-THR-IDX > WS-THR-COUNT
                   ADD 1               TO WS-THR-COUNT
                   SET WS-THR-IDX      TO WS-THR-COUNT
                   MOVE SWI-CUSIP      TO WS-THR-CUSIP (WS-THR-IDX)
                   MOVE WS-OPEN-QTY    TO WS-THR-QTY (WS-THR-IDX)
                   MOVE WS-AGE         TO WS-THR-MAX-AGE (WS-THR-IDX)
                   MOVE 1              TO WS-THR-FAILS (WS-THR-IDX)
               WHEN WS-THR-CUSIP (WS-THR-IDX) = SWI-CUSIP
                   ADD WS-OPEN-QTY     TO WS-THR-QTY (WS-THR-IDX)
                   ADD 1               TO WS-THR-FAILS (WS-THR-IDX)
                   IF WS-AGE > WS-THR-MAX-AGE (WS-THR-IDX)
                       MOVE WS-AGE     TO WS-THR-MAX-AGE (WS-THR-IDX)
                   END-IF
           END-SEARCH.
       3600-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 7000 - THRESHOLD WATCH LIST TO SYSOUT FOR COMPLIANCE           *
      *----------------------------------------------------------------*
       7000-THRESHOLD-LIST.
           DISPLAY 'SWB300 - REG SHO THRESHOLD WATCH LIST (FAILS TO '
                   'DELIVER >= 10,000 FOR 5 DAYS)'
           PERFORM VARYING WS-THR-IDX FROM 1 BY 1
                     UNTIL WS-THR-IDX > WS-THR-COUNT
               IF WS-THR-QTY (WS-THR-IDX) NOT < WS-THR-MIN-QTY
               AND WS-THR-MAX-AGE (WS-THR-IDX) NOT < WS-THR-MIN-AGE
                   ADD 1               TO WS-THRESHOLD-SECS
                   MOVE WS-THR-QTY (WS-THR-IDX)     TO WS-THR-DISP-QTY
                   MOVE WS-THR-MAX-AGE (WS-THR-IDX) TO WS-THR-DISP-AGE
                   MOVE WS-THR-FAILS (WS-THR-IDX)   TO WS-THR-DISP-CNT
                   DISPLAY 'SWB300 -   CUSIP ' WS-THR-CUSIP (WS-THR-IDX)
                           ' QTY ' WS-THR-DISP-QTY
                           ' OLDEST ' WS-THR-DISP-AGE
                           ' FAILS ' WS-THR-DISP-CNT
               END-IF
           END-PERFORM
           IF WS-THRESHOLD-SECS = ZERO
               DISPLAY 'SWB300 -   NONE'
           END-IF
           IF WS-THR-OVERFLOW > ZERO
               DISPLAY 'SWB300 -   TABLE FULL, FAILS NOT AGGREGATED: '
                       WS-THR-OVERFLOW
           END-IF.
       7000-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - BROWSE                                                  *
      *================================================================*
       8000-READ-NEXT.
           READ SWINSTR-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   CONTINUE
               WHEN SWINSTR-EOF
                   SET WS-END-OF-MASTER TO TRUE
               WHEN OTHER
                   MOVE 'SWINSTR'      TO AB-DDNAME
                   MOVE WS-SWINSTR-FS  TO AB-FILE-STATUS
                   MOVE '8000-READ-NEXT' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE SWINSTR-FILE
                 TRDHIST-FILE
                 FAILOUT-FILE
           IF NOT SWINSTR-OK OR NOT TRDHIST-OK OR NOT FAILOUT-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-SWINSTR-FS ' '
                      WS-TRDHIST-FS ' ' WS-FAILOUT-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           IF WS-ACT-CLOSEOUT > ZERO OR WS-ACT-BUYIN > ZERO
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
           PERFORM 7000-THRESHOLD-LIST THRU 7000-EXIT
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM CT-STAGE
           MOVE 'INSTR-READ'           TO CT-COUNTER-NAME
           MOVE WS-INSTR-READ          TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'FAILS-OUT'            TO CT-COUNTER-NAME
           MOVE WS-FAILS-OUT           TO CT-COUNT
           MOVE WS-FAIL-MV-TOTAL       TO CT-AMOUNT
           MOVE WS-FAIL-QTY-TOTAL      TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CLOSEOUT-DUE'         TO CT-COUNTER-NAME
           MOVE WS-ACT-CLOSEOUT        TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'BUYIN'                TO CT-COUNTER-NAME
           MOVE WS-ACT-BUYIN           TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'PENALTY'              TO CT-COUNTER-NAME
           MOVE WS-ACT-PENALTY         TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'THRESHOLD-SEC'        TO CT-COUNTER-NAME
           MOVE WS-THRESHOLD-SECS      TO CT-COUNT
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'PURGED'               TO CT-COUNTER-NAME
           MOVE WS-PURGED              TO CT-COUNT
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
           MOVE WS-FAILS-OUT           TO WS-DISP-COUNT
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'FAILS / CLOSE-OUT ENDED. FAILS ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* SWB300 - FAILS AND CLOSE-OUT       ' DC-BUS-DATE
           DISPLAY '*************************************************'
           MOVE WS-ROWS-READ           TO WS-DISP-COUNT
           DISPLAY '* MASTER ROWS READ          : ' WS-DISP-COUNT
           MOVE WS-INSTR-READ          TO WS-DISP-COUNT
           DISPLAY '* NEW INSTRUCTIONS (NEWM)   : ' WS-DISP-COUNT
           MOVE WS-CLOSED-ROWS         TO WS-DISP-COUNT
           DISPLAY '*   SETTLED / CANCELLED     : ' WS-DISP-COUNT
           MOVE WS-NOT-DUE             TO WS-DISP-COUNT
           DISPLAY '*   NOT YET DUE             : ' WS-DISP-COUNT
           MOVE WS-FAILS-OUT           TO WS-DISP-COUNT
           DISPLAY '*   FAILING                 : ' WS-DISP-COUNT
           MOVE WS-FAILS-DLV           TO WS-DISP-COUNT
           DISPLAY '*     FAIL TO DELIVER       : ' WS-DISP-COUNT
           MOVE WS-FAILS-RCV           TO WS-DISP-COUNT
           DISPLAY '*     FAIL TO RECEIVE       : ' WS-DISP-COUNT
           MOVE WS-NEW-FAILS           TO WS-DISP-COUNT
           DISPLAY '*     NEW TODAY             : ' WS-DISP-COUNT
           MOVE WS-ACT-BUYIN           TO WS-DISP-COUNT
           DISPLAY '* ACTION BI  BUY-IN         : ' WS-DISP-COUNT
           MOVE WS-ACT-CLOSEOUT        TO WS-DISP-COUNT
           DISPLAY '* ACTION CD  CLOSE-OUT DUE  : ' WS-DISP-COUNT
           MOVE WS-ACT-PENALTY         TO WS-DISP-COUNT
           DISPLAY '* ACTION PN  CSDR PENALTY   : ' WS-DISP-COUNT
           MOVE WS-ACT-MONITOR         TO WS-DISP-COUNT
           DISPLAY '* ACTION MO  MONITOR        : ' WS-DISP-COUNT
           MOVE WS-SHORT-SALES         TO WS-DISP-COUNT
           DISPLAY '* SHORT SALES (S+1)         : ' WS-DISP-COUNT
           MOVE WS-HIST-MISSING        TO WS-DISP-COUNT
           DISPLAY '* NOT ON TRADE HISTORY      : ' WS-DISP-COUNT
           MOVE WS-NO-RESPONSE         TO WS-DISP-COUNT
           DISPLAY '* NO CUSTODIAN RESPONSE     : ' WS-DISP-COUNT
           MOVE WS-FX-MISSING          TO WS-DISP-COUNT
           DISPLAY '* NO DESK FX RATE           : ' WS-DISP-COUNT
           MOVE WS-CANC-ROWS           TO WS-DISP-COUNT
           DISPLAY '* CANCELLATION ROWS         : ' WS-DISP-COUNT
           MOVE WS-THRESHOLD-SECS      TO WS-DISP-COUNT
           DISPLAY '* REG SHO THRESHOLD CUSIPS  : ' WS-DISP-COUNT
           MOVE WS-PURGED              TO WS-DISP-COUNT
           DISPLAY '* ROWS PURGED               : ' WS-DISP-COUNT
           MOVE WS-OLDEST-AGE          TO WS-DISP-COUNT
           DISPLAY '* OLDEST FAIL (BUS DAYS)    : ' WS-DISP-COUNT
           MOVE WS-FAIL-MV-TOTAL       TO WS-DISP-AMT
           DISPLAY '* FAIL VALUE USD            : ' WS-DISP-AMT
           DISPLAY '* RETURN CODE               : ' WS-RETURN-CODE
           DISPLAY '*************************************************'.
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
           DISPLAY 'SWB300 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWB300 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
