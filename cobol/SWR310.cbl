       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWR310.
       AUTHOR.        T L MORGAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  06/28/1999.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWR310                                            *
      * TITLE      : SETTLEMENT FAILS / CLOSE-OUT / BUY-IN REPORT      *
      * JOB        : MSSWD030  STEP030                                 *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   LISTS THE FAILS PRODUCED BY SWB300, ONE SECTION PER ACTION   *
      *   (BI BUY-IN, CD CLOSE-OUT DUE, MO MONITOR, PN PENALTY),       *
      *   OLDEST FIRST WITHIN DELIVER / RECEIVE.  CURRENCY,            *
      *   DEPOSITORY AND CUSTODIAN STATUS ARE TAKEN FROM THE           *
      *   INSTRUCTION MASTER.                                          *
      *   EUROCLEAR FAILS SHOW AN ESTIMATED CSDR CASH PENALTY          *
      *   (MARKET VALUE X DAILY RATE X DAYS FAILED).                   *
      *   SUMMARY BY ACTION AND DIRECTION AND BY AGING BUCKET.         *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          FAILIN    MSEC.PROD.SW.FAILS.SORTED(+1)  (SWFAIL)     *
      *          SWINSTR   MSEC.PROD.SW.INSTR.KSDS        (SWINSTR)    *
      * OUTPUT : RPTFILE   REPORT SWR310 (FB 133 ASA)                  *
      * CALLS  : CMU050 CMU060 CMU080 CMASM02                          *
      * RETURN CODE: 0                                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1999-06-28 TLM  CHG05230  ORIGINAL                             *
      * 2004-01-12 KAP  CHG11650  BUY-IN SECTION                       *
      * 2008-01-14 SPA  CHG17720  CLOSE-OUT DATE COLUMN (REG SHO)      *
      * 2009-12-14 SPA  CHG19002  CURRENCY / DEPOSITORY FROM SW.INSTR  *
      * 2022-02-01 MHC  CHG38804  CSDR PENALTY ESTIMATE (EUROCLEAR)    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT FAILIN-FILE    ASSIGN TO FAILIN
                  FILE STATUS IS WS-FAILIN-STATUS.
           SELECT SWINSTR-FILE   ASSIGN TO SWINSTR
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS SWI-SENDER-REF
                  FILE STATUS IS WS-SWINSTR-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  FAILIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SWFAIL.
       FD  SWINSTR-FILE.
       COPY SWINSTR.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWR310'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-FAILIN-STATUS        PIC X(02)  VALUE '00'.
               88  FAILIN-OK                      VALUE '00'.
               88  FAILIN-EOF                     VALUE '10'.
           05  WS-SWINSTR-STATUS       PIC X(02)  VALUE '00'.
               88  SWINSTR-OK                     VALUE '00'.
               88  SWINSTR-NOTFND                 VALUE '23'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-EOF-SW                   PIC X(01)  VALUE 'N'.
           88  END-OF-FAILS                       VALUE 'Y'.
      *----------------------------------------------------------------*
      * ACTION SECTIONS                                                *
      *----------------------------------------------------------------*
       01  WS-ACTION-VALUES.
           05  FILLER  PIC X(52)  VALUE
               'BIBUY-IN - FAILING MORE THAN 10 BUSINESS DAYS      '.
           05  FILLER  PIC X(52)  VALUE
               'CDCLOSE-OUT DUE - REG SHO RULE 204 DATE REACHED    '.
           05  FILLER  PIC X(52)  VALUE
               'MOMONITOR                                          '.
           05  FILLER  PIC X(52)  VALUE
               'PNEUROCLEAR - CSDR SETTLEMENT PENALTY ACCRUING     '.
       01  WS-ACTION-TABLE REDEFINES WS-ACTION-VALUES.
           05  WS-ACT-ENTRY OCCURS 4 TIMES INDEXED BY ACT-IDX.
               10  WS-ACT-CODE         PIC X(02).
               10  WS-ACT-TITLE        PIC X(50).
       01  WS-ACT-TOTALS.
           05  WS-AT OCCURS 4 TIMES.
               10  WS-AT-DLV-CNT       PIC S9(07)       COMP-3.
               10  WS-AT-RCV-CNT       PIC S9(07)       COMP-3.
               10  WS-AT-MV            PIC S9(15)V99    COMP-3.
               10  WS-AT-PENALTY       PIC S9(13)V99    COMP-3.
       01  WS-ACT-SUB                  PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * AGING BUCKETS (BUSINESS DAYS)                                  *
      *----------------------------------------------------------------*
       01  WS-BUCKET-VALUES.
           05  FILLER  PIC X(21)  VALUE '001001 1 DAY         '.
           05  FILLER  PIC X(21)  VALUE '002003 2 - 3 DAYS    '.
           05  FILLER  PIC X(21)  VALUE '004005 4 - 5 DAYS    '.
           05  FILLER  PIC X(21)  VALUE '006010 6 - 10 DAYS   '.
           05  FILLER  PIC X(21)  VALUE '011999 OVER 10 DAYS  '.
       01  WS-BUCKET-TABLE REDEFINES WS-BUCKET-VALUES.
           05  WS-BKT OCCURS 5 TIMES INDEXED BY BK-IDX.
               10  WS-BKT-LOW          PIC 9(03).
               10  WS-BKT-HIGH         PIC 9(03).
               10  WS-BKT-NAME         PIC X(15).
       01  WS-BUCKET-TOTALS.
           05  WS-BT OCCURS 5 TIMES.
               10  WS-BT-CNT           PIC S9(07)       COMP-3.
               10  WS-BT-MV            PIC S9(15)V99    COMP-3.
       01  WS-BK-SUB                   PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * CSDR PENALTY - DAILY RATE ON THE FAILING VALUE                 *
      *----------------------------------------------------------------*
       01  WS-PENALTY-RATE             PIC V9(06)  VALUE .000100.
       01  WS-PENALTY                  PIC S9(13)V99 COMP-3.
      *
       01  WS-CONTROL.
           05  WS-PREV-ACTION          PIC X(02)  VALUE LOW-VALUES.
           05  WS-SEC-CNT              PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-SEC-MV               PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-SEC-PENALTY          PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRINT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NOT-ON-MASTER        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOT-MV               PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-PENALTY          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-OLDEST               PIC S9(03) COMP-3 VALUE ZERO.
       01  WS-INSTR-DATA.
           05  WS-I-CCY                PIC X(03).
           05  WS-I-DEP                PIC X(04).
           05  WS-I-STATUS             PIC X(02).
           05  WS-I-MT                 PIC X(03).
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
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(18)  VALUE ' SENDER REF'.
           05  FILLER  PIC X(02)  VALUE 'D'.
           05  FILLER  PIC X(11)  VALUE 'ACCOUNT'.
           05  FILLER  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER  PIC X(05)  VALUE 'DEP'.
           05  FILLER  PIC X(11)  VALUE 'SETTLE'.
           05  FILLER  PIC X(04)  VALUE 'AGE'.
           05  FILLER  PIC X(17)  VALUE '   OPEN QUANTITY'.
           05  FILLER  PIC X(04)  VALUE 'CCY'.
           05  FILLER  PIC X(16)  VALUE '    OPEN AMOUNT'.
           05  FILLER  PIC X(15)  VALUE '   VALUE USD'.
           05  FILLER  PIC X(03)  VALUE 'ST'.
           05  FILLER  PIC X(05)  VALUE 'RSN'.
           05  FILLER  PIC X(11)  VALUE 'CLOSE-OUT'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(18)  VALUE ' ----------------'.
           05  FILLER  PIC X(02)  VALUE '-'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(17)  VALUE ' ---------------'.
           05  FILLER  PIC X(04)  VALUE '---'.
           05  FILLER  PIC X(16)  VALUE ' --------------'.
           05  FILLER  PIC X(15)  VALUE ' -------------'.
           05  FILLER  PIC X(03)  VALUE '--'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(11)  VALUE '----------'.
       01  WS-SECTION-LINE.
           05  SC-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  FILLER                  PIC X(07)  VALUE 'ACTION '.
           05  SC-CODE                 PIC X(02).
           05  FILLER                  PIC X(03)  VALUE ' - '.
           05  SC-TITLE                PIC X(50).
           05  FILLER                  PIC X(69).
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-DIR                  PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  DL-DEP                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-SETTLE               PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-AGE                  PIC ZZ9.
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-CCY                  PIC X(03).
           05  FILLER                  PIC X(01).
           05  DL-AMT                  PIC ZZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-MV                   PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-STATUS               PIC X(02).
           05  FILLER                  PIC X(01).
           05  DL-REASON               PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-CLOSEOUT             PIC X(10).
           05  FILLER                  PIC X(01).
       01  WS-PENALTY-LINE.
           05  PL-CC                   PIC X(01).
           05  FILLER                  PIC X(60).
           05  FILLER                  PIC X(29)  VALUE
               'EST. CSDR PENALTY TO DATE USD'.
           05  FILLER                  PIC X(01).
           05  PL-PENALTY              PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(29).
       01  WS-SUBTOTAL-LINE.
           05  ST-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  FILLER                  PIC X(16)  VALUE 'SECTION TOTAL'.
           05  ST-CNT                  PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(07)  VALUE ' FAILS '.
           05  FILLER                  PIC X(10)  VALUE 'VALUE USD '.
           05  ST-MV                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(10)  VALUE ' PENALTY '.
           05  ST-PENALTY              PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(43).
       01  WS-SUMMARY-HEAD.
           05  FILLER  PIC X(01)  VALUE '-'.
           05  FILLER  PIC X(06)  VALUE SPACES.
           05  FILLER  PIC X(10)  VALUE 'ACTION'.
           05  FILLER  PIC X(12)  VALUE '  DELIVER'.
           05  FILLER  PIC X(12)  VALUE '  RECEIVE'.
           05  FILLER  PIC X(24)  VALUE '         VALUE USD'.
           05  FILLER  PIC X(20)  VALUE '     EST. PENALTY'.
           05  FILLER  PIC X(48)  VALUE SPACES.
       01  WS-SUMMARY-LINE.
           05  SL-CC                   PIC X(01).
           05  FILLER                  PIC X(06).
           05  SL-ACTION               PIC X(02).
           05  FILLER                  PIC X(06).
           05  SL-DLV                  PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  SL-RCV                  PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(04).
           05  SL-MV                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(04).
           05  SL-PENALTY              PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(57).
       01  WS-BUCKET-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(06).
           05  BL-NAME                 PIC X(15).
           05  FILLER                  PIC X(03).
           05  BL-CNT                  PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(04).
           05  BL-MV                   PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(76).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(06).
           05  TL-LABEL                PIC X(36).
           05  TL-VALUE                PIC ZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  TL-AMOUNT               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(59).
       01  WS-NO-FAILS-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO SETTLEMENT INSTRUCTIONS ARE FAILING ***'.
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-FAIL THRU 2000-EXIT
               UNTIL END-OF-FAILS.
           IF WS-READ-CNT > ZERO
               PERFORM 2800-END-SECTION THRU 2800-EXIT
           END-IF.
           PERFORM 3000-PRINT-SUMMARY THRU 3000-EXIT.
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
           MOVE 'SW FAILS REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT FAILIN-FILE.
           IF NOT FAILIN-OK
               MOVE 'FAILIN' TO AB-DDNAME
               MOVE WS-FAILIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT SWINSTR-FILE.
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR' TO AB-DDNAME
               MOVE WS-SWINSTR-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM VARYING WS-ACT-SUB FROM 1 BY 1 UNTIL WS-ACT-SUB > 4
               MOVE ZERO TO WS-AT-DLV-CNT (WS-ACT-SUB)
                            WS-AT-RCV-CNT (WS-ACT-SUB)
                            WS-AT-MV      (WS-ACT-SUB)
                            WS-AT-PENALTY (WS-ACT-SUB)
           END-PERFORM.
           PERFORM VARYING WS-BK-SUB FROM 1 BY 1 UNTIL WS-BK-SUB > 5
               MOVE ZERO TO WS-BT-CNT (WS-BK-SUB)
                            WS-BT-MV  (WS-BK-SUB)
           END-PERFORM.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE 'SWR310'       TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'SETTLEMENT FAILS - CLOSE-OUT AND BUY-IN'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8000-READ-FAIL THRU 8000-EXIT.
           IF END-OF-FAILS
               PERFORM 8200-HEADINGS THRU 8200-EXIT
               WRITE RPT-RECORD FROM WS-NO-FAILS-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2 TO RPT-LINE-COUNT
           END-IF.
       1000-EXIT.
           EXIT.
      *================================================================*
       2000-PROCESS-FAIL.
      *================================================================*
           ADD 1 TO WS-READ-CNT.
           IF SWF-ACTION NOT = WS-PREV-ACTION
               IF WS-READ-CNT > 1
                   PERFORM 2800-END-SECTION THRU 2800-EXIT
               END-IF
               PERFORM 2700-START-SECTION THRU 2700-EXIT
           END-IF.
           PERFORM 2100-GET-INSTRUCTION THRU 2100-EXIT.
           PERFORM 2200-FIND-BUCKET THRU 2200-EXIT.
      *    PENALTY ACCRUES ON EUROCLEAR FAILS WHATEVER THE ACTION
           MOVE ZERO TO WS-PENALTY.
           IF WS-I-DEP = 'EUCL'
               COMPUTE WS-PENALTY ROUNDED =
                       SWF-MKT-VALUE-USD * WS-PENALTY-RATE
                       * SWF-AGE-BUS-DAYS
           END-IF.
           SET ACT-IDX TO 1.
           SEARCH WS-ACT-ENTRY
               AT END
                   MOVE 3 TO WS-ACT-SUB
               WHEN WS-ACT-CODE (ACT-IDX) = SWF-ACTION
                   SET WS-ACT-SUB TO ACT-IDX
           END-SEARCH.
           IF SWF-FAIL-TO-DELIVER
               ADD 1 TO WS-AT-DLV-CNT (WS-ACT-SUB)
           ELSE
               ADD 1 TO WS-AT-RCV-CNT (WS-ACT-SUB)
           END-IF.
           ADD SWF-MKT-VALUE-USD TO WS-AT-MV (WS-ACT-SUB)
                                    WS-SEC-MV WS-TOT-MV.
           ADD WS-PENALTY TO WS-AT-PENALTY (WS-ACT-SUB)
                             WS-SEC-PENALTY WS-TOT-PENALTY.
           ADD 1 TO WS-SEC-CNT.
           ADD 1 TO WS-BT-CNT (WS-BK-SUB).
           ADD SWF-MKT-VALUE-USD TO WS-BT-MV (WS-BK-SUB).
           IF SWF-AGE-BUS-DAYS > WS-OLDEST
               MOVE SWF-AGE-BUS-DAYS TO WS-OLDEST
           END-IF.
           PERFORM 2300-PRINT-DETAIL THRU 2300-EXIT.
           PERFORM 8000-READ-FAIL THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-GET-INSTRUCTION.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-INSTR-DATA.
           MOVE SWF-SENDER-REF TO SWI-SENDER-REF.
           READ SWINSTR-FILE.
           EVALUATE TRUE
               WHEN SWINSTR-OK
                   MOVE SWI-CCY        TO WS-I-CCY
                   MOVE SWI-DEPOSITORY TO WS-I-DEP
                   MOVE SWI-STATUS     TO WS-I-STATUS
                   MOVE SWI-MSG-TYPE   TO WS-I-MT
               WHEN SWINSTR-NOTFND
                   ADD 1 TO WS-NOT-ON-MASTER
                   MOVE '???' TO WS-I-CCY
               WHEN OTHER
                   MOVE 'SWINSTR' TO AB-DDNAME
                   MOVE WS-SWINSTR-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-FIND-BUCKET.
      *----------------------------------------------------------------*
           MOVE 5 TO WS-BK-SUB.
           PERFORM VARYING BK-IDX FROM 1 BY 1 UNTIL BK-IDX > 5
               IF SWF-AGE-BUS-DAYS NOT < WS-BKT-LOW (BK-IDX)
               AND SWF-AGE-BUS-DAYS NOT > WS-BKT-HIGH (BK-IDX)
                   SET WS-BK-SUB TO BK-IDX
               END-IF
           END-PERFORM.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-PRINT-DETAIL.
      *----------------------------------------------------------------*
           MOVE SPACES           TO WS-DETAIL-LINE.
           MOVE ' '              TO DL-CC.
           MOVE SWF-SENDER-REF   TO DL-REF.
           MOVE SWF-DIRECTION    TO DL-DIR.
           MOVE SWF-ACCT-NO      TO DL-ACCT.
           MOVE SWF-CUSIP        TO DL-CUSIP.
           MOVE WS-I-DEP         TO DL-DEP.
           MOVE SWF-SETTLE-DATE  TO WS-DATE-IN.
           PERFORM 8300-EDIT-DATE THRU 8300-EXIT.
           MOVE WS-DATE-EDIT     TO DL-SETTLE.
           MOVE SWF-AGE-BUS-DAYS TO DL-AGE.
           MOVE SWF-QTY          TO DL-QTY.
           MOVE WS-I-CCY         TO DL-CCY.
           MOVE SWF-AMOUNT       TO DL-AMT.
           MOVE SWF-MKT-VALUE-USD TO DL-MV.
           MOVE WS-I-STATUS      TO DL-STATUS.
           MOVE SWF-REASON-CODE  TO DL-REASON.
           IF SWF-CLOSEOUT-DATE > ZERO
               MOVE SWF-CLOSEOUT-DATE TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE THRU 8300-EXIT
               MOVE WS-DATE-EDIT TO DL-CLOSEOUT
           END-IF.
           PERFORM 8100-WRITE-DETAIL THRU 8100-EXIT.
           IF WS-PENALTY > ZERO
               MOVE SPACES       TO WS-PENALTY-LINE
               MOVE ' '          TO PL-CC
               MOVE 'EST. CSDR PENALTY TO DATE USD'
                                 TO WS-PENALTY-LINE (62:29)
               MOVE WS-PENALTY   TO PL-PENALTY
               MOVE WS-PENALTY-LINE TO WS-DETAIL-LINE
               PERFORM 8100-WRITE-DETAIL THRU 8100-EXIT
           END-IF.
           ADD 1 TO WS-PRINT-CNT.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2700-START-SECTION.
      *----------------------------------------------------------------*
           MOVE SWF-ACTION TO WS-PREV-ACTION.
           MOVE ZERO TO WS-SEC-CNT WS-SEC-MV WS-SEC-PENALTY.
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           MOVE SPACES TO WS-SECTION-LINE.
           MOVE '0' TO SC-CC.
           MOVE 'ACTION ' TO WS-SECTION-LINE (3:7).
           MOVE ' - ' TO WS-SECTION-LINE (12:3).
           MOVE SWF-ACTION TO SC-CODE.
           MOVE 'UNKNOWN ACTION CODE' TO SC-TITLE.
           PERFORM VARYING ACT-IDX FROM 1 BY 1 UNTIL ACT-IDX > 4
               IF WS-ACT-CODE (ACT-IDX) = SWF-ACTION
                   MOVE WS-ACT-TITLE (ACT-IDX) TO SC-TITLE
               END-IF
           END-PERFORM.
           WRITE RPT-RECORD FROM WS-SECTION-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
       2700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2800-END-SECTION.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-SUBTOTAL-LINE.
           MOVE '0' TO ST-CC.
           MOVE 'SECTION TOTAL' TO WS-SUBTOTAL-LINE (7:16).
           MOVE ' FAILS ' TO WS-SUBTOTAL-LINE (33:7).
           MOVE 'VALUE USD ' TO WS-SUBTOTAL-LINE (40:10).
           MOVE ' PENALTY ' TO WS-SUBTOTAL-LINE (68:10).
           MOVE WS-SEC-CNT TO ST-CNT.
           MOVE WS-SEC-MV TO ST-MV.
           MOVE WS-SEC-PENALTY TO ST-PENALTY.
           MOVE WS-SUBTOTAL-LINE TO WS-DETAIL-LINE.
           PERFORM 8100-WRITE-DETAIL THRU 8100-EXIT.
       2800-EXIT.
           EXIT.
      *================================================================*
       3000-PRINT-SUMMARY.
      *================================================================*
           PERFORM 8200-HEADINGS THRU 8200-EXIT.
           WRITE RPT-RECORD FROM WS-SUMMARY-HEAD.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           PERFORM VARYING WS-ACT-SUB FROM 1 BY 1 UNTIL WS-ACT-SUB > 4
               MOVE SPACES TO WS-SUMMARY-LINE
               IF WS-ACT-SUB = 1
                   MOVE '0' TO SL-CC
               ELSE
                   MOVE ' ' TO SL-CC
               END-IF
               MOVE WS-ACT-CODE   (WS-ACT-SUB) TO SL-ACTION
               MOVE WS-AT-DLV-CNT (WS-ACT-SUB) TO SL-DLV
               MOVE WS-AT-RCV-CNT (WS-ACT-SUB) TO SL-RCV
               MOVE WS-AT-MV      (WS-ACT-SUB) TO SL-MV
               MOVE WS-AT-PENALTY (WS-ACT-SUB) TO SL-PENALTY
               WRITE RPT-RECORD FROM WS-SUMMARY-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-PERFORM.
           PERFORM VARYING WS-BK-SUB FROM 1 BY 1 UNTIL WS-BK-SUB > 5
               MOVE SPACES TO WS-BUCKET-LINE
               IF WS-BK-SUB = 1
                   MOVE '-' TO BL-CC
               ELSE
                   MOVE ' ' TO BL-CC
               END-IF
               MOVE WS-BKT-NAME (WS-BK-SUB) TO BL-NAME
               MOVE WS-BT-CNT   (WS-BK-SUB) TO BL-CNT
               MOVE WS-BT-MV    (WS-BK-SUB) TO BL-MV
               WRITE RPT-RECORD FROM WS-BUCKET-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
           END-PERFORM.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE '0' TO TL-CC.
           MOVE 'FAILING INSTRUCTIONS' TO TL-LABEL.
           MOVE WS-READ-CNT TO TL-VALUE.
           MOVE WS-TOT-MV TO TL-AMOUNT.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
           MOVE 'ESTIMATED CSDR PENALTIES (USD)' TO TL-LABEL.
           MOVE WS-TOT-PENALTY TO TL-AMOUNT.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
           MOVE 'OLDEST FAIL (BUSINESS DAYS)' TO TL-LABEL.
           MOVE WS-OLDEST TO TL-VALUE.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE SPACES TO WS-TOTAL-LINE.
           MOVE ' ' TO TL-CC.
           MOVE 'FAILS NOT ON INSTRUCTION MASTER' TO TL-LABEL.
           MOVE WS-NOT-ON-MASTER TO TL-VALUE.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
       8000-READ-FAIL.
      *================================================================*
           READ FAILIN-FILE.
           EVALUATE TRUE
               WHEN FAILIN-OK
                   CONTINUE
               WHEN FAILIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'FAILIN' TO AB-DDNAME
                   MOVE WS-FAILIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-WRITE-DETAIL.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS THRU 8200-EXIT
           END-IF.
           WRITE RPT-RECORD FROM WS-DETAIL-LINE.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE THRU 8900-EXIT.
           MOVE 5 TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-EDIT-DATE.
      *----------------------------------------------------------------*
           MOVE WS-DI-CCYY TO WS-DE-CCYY.
           MOVE WS-DI-MM   TO WS-DE-MM.
           MOVE WS-DI-DD   TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE FAILIN-FILE SWINSTR-FILE RPTFILE.
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SWR310'       TO CT-STAGE.
           MOVE 'FAILS-IN'     TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT    TO CT-COUNT.
           MOVE WS-TOT-MV      TO CT-AMOUNT.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'FAILS-PRINTED' TO CT-COUNTER-NAME.
           MOVE WS-PRINT-CNT   TO CT-COUNT.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'SWR310 FAIL RECORDS READ   : ' WS-READ-CNT.
           DISPLAY 'SWR310 FAIL LINES PRINTED  : ' WS-PRINT-CNT.
           DISPLAY 'SWR310 NOT ON SW.INSTR     : ' WS-NOT-ON-MASTER.
           DISPLAY 'SWR310 PAGES               : ' RPT-PAGE-COUNT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE 'SW FAILS REPORT ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SWR310 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
