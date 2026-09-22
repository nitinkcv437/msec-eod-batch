       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWB400.
       AUTHOR.        T L MORGAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/15/1999.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWB400                                            *
      * TITLE      : STATEMENT OF PENDING TRANSACTIONS - EXTRACT       *
      * JOB        : MSSWD030  STEP050  (IKJEFT01 - DB2 PLAN MSSWPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   DAILY STATEMENT OF PENDING TRANSACTIONS FOR THE DVP / RVP    *
      *   CLIENTS (REPLACES THE MT537 THE CUSTODIAN STOPPED SENDING    *
      *   IN 1999).  FROM THE SETTLEMENT INSTRUCTION MASTER:           *
      *     SECTION F  FAILING   - OPEN, SETTLE DATE PASSED            *
      *     SECTION P  PENDING   - OPEN, SETTLE DATE TODAY OR LATER    *
      *     SECTION S  SETTLED TODAY (EFFECTIVE SETTLE DATE = TODAY)   *
      *   CANCELLED INSTRUCTIONS AND CANCELLATION ROWS ARE LEFT OUT.   *
      *   HOUSE ACCOUNTS (FIRM INVENTORY / STREET) DO NOT RECEIVE A    *
      *   STATEMENT.  THE SECURITY DESCRIPTION COMES FROM THE          *
      *   SECURITY MASTER (CMD010 GET, CACHED).                        *
      *   THE EXTRACT IS SORTED BY ACCOUNT / SECTION / SETTLE DATE     *
      *   (SWS400A) AND PRINTED BY SWR410.                             *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SWINSTR   MSEC.PROD.SW.INSTR.KSDS        (SWINSTR)    *
      * OUTPUT : PSTMOUT   MSEC.PROD.SW.PENDSTMT(+1)      (SWPSTM)     *
      * CALLS  : CMD010 CMU050 CMU060 CMU080                           *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 SECURITY NOT ON MASTER                *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1999-03-15 TLM  CHG04890  ORIGINAL - MT537 REPLACEMENT         *
      * 1999-06-21 TLM  CHG05230  FAILING SECTION, OPEN QUANTITY       *
      * 2004-01-12 KAP  CHG11650  BUY-IN FLAG                          *
      * 2011-06-20 SPA  CHG21877  DESCRIPTION FROM DB2 (CMD010)        *
      * 2016-10-03 SPA  CHG30112  HOUSE ACCOUNTS EXCLUDED              *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT SWINSTR-FILE  ASSIGN TO SWINSTR
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS SEQUENTIAL
                                RECORD KEY IS SWI-SENDER-REF
                                FILE STATUS IS WS-SWINSTR-FS.
           SELECT PSTMOUT-FILE  ASSIGN TO PSTMOUT
                                FILE STATUS IS WS-PSTMOUT-FS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY CMDATEW.
       FD  SWINSTR-FILE.
       COPY SWINSTR.
       FD  PSTMOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PSTMOUT-REC                 PIC X(200).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWB400'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-SWINSTR-FS           PIC X(02).
               88  SWINSTR-OK                    VALUE '00'.
               88  SWINSTR-EOF                   VALUE '10'.
           05  WS-PSTMOUT-FS           PIC X(02).
               88  PSTMOUT-OK                    VALUE '00'.
      *
       01  WS-EOF-SW                   PIC X(01)  VALUE 'N'.
           88  WS-END-OF-MASTER                   VALUE 'Y'.
       01  WS-SEQ-CTL-KEY              PIC X(16)  VALUE 'SEQCONTROL'.
      *
       COPY SWPSTM.
      *
      *----------------------------------------------------------------*
      * SECURITY DESCRIPTION CACHE                                     *
      *----------------------------------------------------------------*
       01  WS-SEC-CACHE.
           05  WS-SC-COUNT             PIC S9(04) COMP VALUE ZERO.
           05  WS-SC-MAX               PIC S9(04) COMP VALUE 2000.
           05  WS-SC-NEXT              PIC S9(04) COMP VALUE ZERO.
           05  WS-SC-ENTRY             OCCURS 2000 TIMES
                                       INDEXED BY WS-SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-DESC          PIC X(30).
       01  WS-SEC-DESC                 PIC X(30).
      *
       01  WS-COUNTERS.
           05  WS-ROWS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HOUSE-SKIPPED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLOSED-SKIPPED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-SKIPPED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STMT-OUT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-F                PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-P                PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-S                PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SECM-LOOKUPS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SECM-NOTFOUND        PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-OUT-AMT-HASH         PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-OUT-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
       01  WS-WORK-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-SECTION              PIC X(01).
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
      *
       COPY CMSECMS.
       COPY CMSECLNK.
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
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'PENDING TRANSACTION STATEMENT EXTRACT STARTED'
                                       TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT SWINSTR-FILE
           IF NOT SWINSTR-OK
               MOVE 'SWINSTR'          TO AB-DDNAME
               MOVE WS-SWINSTR-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT PSTMOUT-FILE
           IF NOT PSTMOUT-OK
               MOVE 'PSTMOUT'          TO AB-DDNAME
               MOVE WS-PSTMOUT-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           PERFORM 8000-READ-NEXT      THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - ONE MASTER ROW                                          *
      *================================================================*
       2000-PROCESS-ROW.
           ADD 1                       TO WS-ROWS-READ
           IF SWI-SENDER-REF = WS-SEQ-CTL-KEY
               GO TO 2000-NEXT
           END-IF
           IF SWI-FUNCTION NOT = 'NEWM'
               ADD 1                   TO WS-CANC-SKIPPED
               GO TO 2000-NEXT
           END-IF
      *    FIRM ACCTS SORT LOW - ALPHA PREFIX, NO CLIENT STATEMENT
           IF SWI-ACCT-NO (1:1) < '0'
               ADD 1                   TO WS-HOUSE-SKIPPED
               GO TO 2000-NEXT
           END-IF
      *
           EVALUATE TRUE
               WHEN SWI-CANCELLED
                   ADD 1               TO WS-CLOSED-SKIPPED
                   GO TO 2000-NEXT
               WHEN SWI-SETTLED
                   IF SWI-EFF-SETTLE-DATE = DC-BUS-DATE
                   OR SWI-LAST-STATUS-DATE = DC-BUS-DATE
                       MOVE 'S'        TO WS-SECTION
                       ADD 1           TO WS-SEC-S
                   ELSE
                       ADD 1           TO WS-CLOSED-SKIPPED
                       GO TO 2000-NEXT
                   END-IF
               WHEN SWI-SETTLE-DATE < DC-BUS-DATE
                   MOVE 'F'            TO WS-SECTION
                   ADD 1               TO WS-SEC-F
               WHEN OTHER
                   MOVE 'P'            TO WS-SECTION
                   ADD 1               TO WS-SEC-P
           END-EVALUATE
      *
           PERFORM 3000-GET-DESCRIPTION THRU 3000-EXIT
           PERFORM 4000-WRITE-STATEMENT THRU 4000-EXIT.
       2000-NEXT.
           PERFORM 8000-READ-NEXT      THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - SECURITY DESCRIPTION (CACHE, THEN CMD010)               *
      *================================================================*
       3000-GET-DESCRIPTION.
           SET WS-SC-IDX               TO 1
           SEARCH WS-SC-ENTRY
               AT END
                   CONTINUE
               WHEN WS-SC-IDX > WS-SC-COUNT
                   SET WS-SC-IDX       TO WS-SC-MAX
               WHEN WS-SC-CUSIP (WS-SC-IDX) = SWI-CUSIP
                   MOVE WS-SC-DESC (WS-SC-IDX) TO WS-SEC-DESC
                   GO TO 3000-EXIT
           END-SEARCH
      *
           MOVE 'GET '                 TO SL-FUNCTION
           MOVE SWI-CUSIP              TO SL-KEY-CUSIP
           MOVE SPACES                 TO SL-KEY-ISIN SL-KEY-SYMBOL
           CALL 'CMD010' USING SL-SECURITY-PARMS
           ADD 1                       TO WS-SECM-LOOKUPS
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA    TO SEC-MASTER-REC
                   MOVE SEC-DESC (1:30) TO WS-SEC-DESC
               WHEN SL-NOT-FOUND
                   ADD 1               TO WS-SECM-NOTFOUND
                   MOVE 4              TO WS-RETURN-CODE
                   MOVE '** NOT ON SECURITY MASTER **'
                                       TO WS-SEC-DESC
               WHEN OTHER
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE SWI-CUSIP      TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3000-GET-DESCRIPTION' TO AB-PARAGRAPH
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE
      *    CACHE FULL: OVERWRITE ROUND-ROBIN
           IF WS-SC-COUNT < WS-SC-MAX
               ADD 1                   TO WS-SC-COUNT
               MOVE WS-SC-COUNT        TO WS-SC-NEXT
           ELSE
               ADD 1                   TO WS-SC-NEXT
               IF WS-SC-NEXT > WS-SC-MAX
                   MOVE 1              TO WS-SC-NEXT
               END-IF
           END-IF
           MOVE SWI-CUSIP              TO WS-SC-CUSIP (WS-SC-NEXT)
           MOVE WS-SEC-DESC            TO WS-SC-DESC (WS-SC-NEXT).
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - STATEMENT LINE                                          *
      *================================================================*
       4000-WRITE-STATEMENT.
           INITIALIZE PST-STMT-REC
           MOVE SWI-ACCT-NO            TO PST-ACCT-NO
           MOVE WS-SECTION             TO PST-SECTION
           MOVE SWI-SETTLE-DATE        TO PST-SETTLE-DATE
           MOVE SWI-SENDER-REF         TO PST-SENDER-REF
           MOVE DC-BUS-DATE            TO PST-BUS-DATE
           MOVE SWI-MSG-TYPE           TO PST-MSG-TYPE
           MOVE SWI-STATUS             TO PST-STATUS
           MOVE SWI-REASON-CODE        TO PST-REASON-CODE
           MOVE SWI-CUSIP              TO PST-CUSIP
           MOVE SWI-ISIN               TO PST-ISIN
           MOVE WS-SEC-DESC            TO PST-SEC-DESC
           MOVE SWI-TRADE-DATE         TO PST-TRADE-DATE
           MOVE SWI-QTY                TO PST-QTY
           IF PST-SETTLED-TODAY
               MOVE SWI-SETTLED-QTY    TO PST-OPEN-QTY
               MOVE SWI-SETTLED-AMOUNT TO PST-AMOUNT
           ELSE
               COMPUTE PST-OPEN-QTY = SWI-QTY - SWI-SETTLED-QTY
               COMPUTE PST-AMOUNT = SWI-AMOUNT - SWI-SETTLED-AMOUNT
           END-IF
           MOVE SWI-CCY                TO PST-CCY
           MOVE SWI-DEPOSITORY         TO PST-DEPOSITORY
           MOVE SWI-FAIL-AGE           TO PST-FAIL-AGE
           MOVE SWI-CLOSEOUT-DATE      TO PST-CLOSEOUT-DATE
           MOVE SWI-BUYIN-FLAG         TO PST-BUYIN-FLAG
           MOVE SWI-EFF-SETTLE-DATE    TO PST-EFF-SETTLE-DATE
           WRITE PSTMOUT-REC           FROM PST-STMT-REC
           IF NOT PSTMOUT-OK
               MOVE 'PSTMOUT'          TO AB-DDNAME
               MOVE WS-PSTMOUT-FS      TO AB-FILE-STATUS
               MOVE SWI-SENDER-REF     TO AB-KEY
               MOVE '4000-WRITE-STATEMENT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-STMT-OUT
           ADD PST-AMOUNT              TO WS-OUT-AMT-HASH
           ADD PST-OPEN-QTY            TO WS-OUT-QTY-HASH.
       4000-EXIT.
           EXIT.
      *
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
           CLOSE SWINSTR-FILE PSTMOUT-FILE
           IF NOT SWINSTR-OK OR NOT PSTMOUT-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-SWINSTR-FS ' ' WS-PSTMOUT-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM CT-STAGE
           MOVE 'INSTR-READ'           TO CT-COUNTER-NAME
           MOVE WS-ROWS-READ           TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'STMT-OUT'             TO CT-COUNTER-NAME
           MOVE WS-STMT-OUT            TO CT-COUNT
           MOVE WS-OUT-AMT-HASH        TO CT-AMOUNT
           MOVE WS-OUT-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RETURN-CODE = ZERO
               MOVE 'I'                TO AU-SEVERITY
           ELSE
               MOVE 'W'                TO AU-SEVERITY
           END-IF
           MOVE WS-STMT-OUT            TO WS-DISP-COUNT
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'PENDING STATEMENT EXTRACT ENDED. LINES '
                  WS-DISP-COUNT DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* SWB400 - PENDING STATEMENT EXTRACT ' DC-BUS-DATE
           DISPLAY '*************************************************'
           MOVE WS-ROWS-READ           TO WS-DISP-COUNT
           DISPLAY '* MASTER ROWS READ          : ' WS-DISP-COUNT
           MOVE WS-STMT-OUT            TO WS-DISP-COUNT
           DISPLAY '* STATEMENT LINES WRITTEN   : ' WS-DISP-COUNT
           MOVE WS-SEC-F               TO WS-DISP-COUNT
           DISPLAY '*   FAILING           (F)   : ' WS-DISP-COUNT
           MOVE WS-SEC-P               TO WS-DISP-COUNT
           DISPLAY '*   PENDING           (P)   : ' WS-DISP-COUNT
           MOVE WS-SEC-S               TO WS-DISP-COUNT
           DISPLAY '*   SETTLED TODAY     (S)   : ' WS-DISP-COUNT
           MOVE WS-HOUSE-SKIPPED       TO WS-DISP-COUNT
           DISPLAY '* HOUSE ACCOUNTS SKIPPED    : ' WS-DISP-COUNT
           MOVE WS-CLOSED-SKIPPED      TO WS-DISP-COUNT
           DISPLAY '* CLOSED INSTR SKIPPED      : ' WS-DISP-COUNT
           MOVE WS-CANC-SKIPPED        TO WS-DISP-COUNT
           DISPLAY '* CANCELLATION ROWS SKIPPED : ' WS-DISP-COUNT
           MOVE WS-SECM-LOOKUPS        TO WS-DISP-COUNT
           DISPLAY '* SECURITY MASTER LOOKUPS   : ' WS-DISP-COUNT
           MOVE WS-SECM-NOTFOUND       TO WS-DISP-COUNT
           DISPLAY '* NOT ON SECURITY MASTER    : ' WS-DISP-COUNT
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
           DISPLAY 'SWB400 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWB400 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
