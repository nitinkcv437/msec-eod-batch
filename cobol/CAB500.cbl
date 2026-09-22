      *================================================================*
      * PROGRAM    : CAB500                                            *
      * DESCRIPTION: CORPORATE ACTION DIVIDEND ACCRUALS.               *
      *              FOR EVERY ENTITLEMENT THAT IS CALCULATED BUT NOT  *
      *              YET PAID (STATUS CA) AND CARRIES CASH, BOOKS A    *
      *              RECEIVABLE ACCRUAL TO THE GENERAL LEDGER:         *
      *                DR  DIVIDEND RECEIVABLE                         *
      *                CR  DIVIDEND INCOME CLEARING                    *
      *              GL ACCOUNTS COME FROM MSEC.GL_ACCOUNT_MAP (CMD050)*
      *              FOR TRANSACTION CODE 'CACR'.  THE LINES ARE       *
      *              AUTO-REVERSING (GLJ-REVERSAL-FLAG 'R') - THE GL   *
      *              REVERSES THEM NEXT DAY AND THEY ARE RE-BOOKED     *
      *              UNTIL THE PAY DATE.  AN ACCRUAL SUMMARY RECORD IS *
      *              WRITTEN PER EVENT FOR THE ACCRUAL REPORT (CAR510).*
      *----------------------------------------------------------------*
      * JOB        : MSCAD050  STEP010  (IKJEFT01 - PLAN MSCAPLN)      *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              ENTLMAST  ENTITLEMENT MASTER KSDS (SEQUENTIAL)    *
      *              CAEVENT   EVENT MASTER KSDS (RANDOM, READ ONLY)   *
      * OUTPUT     : GLOUT     GL JOURNAL LINES (SRGLJNL)              *
      *                        MSEC.PROD.CA.ACCRUAL(+1) - ALWAYS       *
      *                        CREATED, MERGED BY SRB600 (MSSRD070)    *
      *              ACCSUM    ACCRUAL SUMMARY BY EVENT (CAACRSM)      *
      *                        MSEC.PROD.CA.ACCRSUM(+1)                *
      * CALLS      : CMD050 (GL ACCOUNT MAP)  CMU040 (FX TO USD)       *
      *              CMU050 CMU060 CMU080                              *
      *----------------------------------------------------------------*
      * JOURNAL REFERENCE: GLJ-REF = EVENT ID (12) + ENTITLEMENT       *
      *   SEQUENCE WITHIN THE EVENT (4).  LINE 1 DEBIT, LINE 2 CREDIT. *
      * ACCRUAL AMOUNT   : GROSS CASH + CASH IN LIEU (RECEIVABLE FROM  *
      *   THE PAYING AGENT BEFORE WITHHOLDING).                        *
      *----------------------------------------------------------------*
      * RETURN CODES:                                                  *
      *   00 CLEAN                                                     *
      *   04 GL MAP ROW NOT FOUND (ENTITLEMENT NOT ACCRUED) OR FX      *
      *      RATE NOT FOUND (USD AMOUNT ZERO)                          *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2003-03-10 KAP  ORIGINAL - SOX ACCRUAL REQUIREMENT    CHG10877 *
      * 2004-11-01 KAP  READ ENTITLEMENT MASTER KSDS          CHG12877 *
      * 2006-01-16 KAP  AUTO-REVERSING LINES                  CHG14720 *
      * 2008-02-25 KAP  CASH MERGER PROCEEDS ACCRUED          CHG17444 *
      * 2011-06-20 SPA  AUDIT AND CONTROL TOTALS              CHG21877 *
      * 2011-09-12 SPA  USD EQUIVALENT VIA CMU040             CHG22410 *
      * 2014-09-08 SPA  GL MAP FROM DB2 GL_ACCOUNT_MAP        CHG27115 *
      *                 (CMD050) - HARD CODED ACCOUNTS REMOVED         *
      * 2021-03-01 MFO  RECOMPILED ENTERPRISE COBOL 6.3       CHG36620 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAB500.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  2003-03-10.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE   ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
      *
           SELECT ENTLMAST-FILE   ASSIGN TO ENTLMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS ENT-KEY
                  FILE STATUS IS WS-ENTLMAST-STATUS.
      *
           SELECT CAEVENT-FILE    ASSIGN TO CAEVENT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CAE-EVENT-ID
                  FILE STATUS IS WS-CAEVENT-STATUS.
      *
           SELECT GLOUT-FILE      ASSIGN TO GLOUT
                  FILE STATUS IS WS-GLOUT-STATUS.
      *
           SELECT ACCSUM-FILE     ASSIGN TO ACCSUM
                  FILE STATUS IS WS-ACCSUM-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  ENTLMAST-FILE.
           COPY CAENTL.
      *
       FD  CAEVENT-FILE.
           COPY CAEVENT.
      *
       FD  GLOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLOUT-REC                   PIC X(150).
      *
       FD  ACCSUM-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACCSUM-REC                  PIC X(100).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAB500'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-ENTLMAST-STATUS      PIC X(02) VALUE '00'.
               88  ENTLMAST-OK                   VALUE '00'.
               88  ENTLMAST-EOF                  VALUE '10'.
           05  WS-CAEVENT-STATUS       PIC X(02) VALUE '00'.
               88  CAEVENT-OK                    VALUE '00'.
               88  CAEVENT-NOTFND                VALUE '23'.
           05  WS-GLOUT-STATUS         PIC X(02) VALUE '00'.
           05  WS-ACCSUM-STATUS        PIC X(02) VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-ENTL-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-ENTL-EOF                   VALUE 'Y'.
           05  WS-EVENT-ACCRUE-SW      PIC X(01) VALUE 'N'.
               88  WS-EVENT-ACCRUABLE            VALUE 'Y'.
               88  WS-EVENT-NOT-ACCRUABLE        VALUE 'N'.
           05  WS-MAP-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-MAP-FOUND                  VALUE 'Y'.
      *
       01  WS-CURR-EVENT               PIC X(12) VALUE LOW-VALUES.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
       01  WS-SEC-TYPE                 PIC X(02) VALUE 'EQ'.
       01  WS-TXN-CODE                 PIC X(04) VALUE 'CACR'.
      *
      *----------------------------------------------------------------*
      * GL MAP CACHE BY ACCOUNT TYPE (ONE TXN CODE, ONE SEC TYPE)      *
      *----------------------------------------------------------------*
       01  WS-MAP-CACHE.
           05  WS-MC-COUNT             PIC S9(04) COMP VALUE ZERO.
           05  WS-MC-ENTRY             OCCURS 10 TIMES
                                       INDEXED BY MC-IDX.
               10  WS-MC-ACCT-TYPE     PIC X(02).
               10  WS-MC-FOUND         PIC X(01).
               10  WS-MC-DR-GL         PIC X(10).
               10  WS-MC-CR-GL         PIC X(10).
               10  WS-MC-COST-CTR      PIC X(06).
               10  WS-MC-DESC          PIC X(30).
       01  WS-MAP-RESULT.
           05  WS-MR-DR-GL             PIC X(10).
           05  WS-MR-CR-GL             PIC X(10).
           05  WS-MR-COST-CTR          PIC X(06).
           05  WS-MR-DESC              PIC X(30).
      *
      *----------------------------------------------------------------*
      * WORK FIELDS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK.
           05  WS-ACCRUAL-AMT          PIC S9(15)V99    COMP-3.
           05  WS-ACCRUAL-USD          PIC S9(15)V99    COMP-3.
           05  WS-ENTL-SEQ             PIC 9(04)        VALUE ZERO.
       01  WS-GLJ-REF-WORK.
           05  WS-GREF-EVENT-ID        PIC X(12).
           05  WS-GREF-SEQ             PIC 9(04).
      *
       01  WS-EVENT-TOTALS.
           05  WS-EV-ENTL-COUNT        PIC S9(07)       COMP-3.
           05  WS-EV-ACCRUED-COUNT     PIC S9(07)       COMP-3.
           05  WS-EV-SKIP-COUNT        PIC S9(07)       COMP-3.
           05  WS-EV-AMOUNT            PIC S9(15)V99    COMP-3.
           05  WS-EV-USD               PIC S9(15)V99    COMP-3.
           05  WS-EV-DR-GL             PIC X(10).
           05  WS-EV-CR-GL             PIC X(10).
      *
       01  WS-COUNTERS.
           05  WS-ENTL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-ACCRUED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-NOT-CA          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-NO-CASH         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-DUE-TODAY       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-EVENT-SKIP      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MAP-NOT-FOUND        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-NOT-FOUND         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GL-WRITTEN           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SUM-WRITTEN          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOTAL-DR             PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-TOTAL-CR             PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-TOTAL-USD            PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
           COPY SRGLJNL.
           COPY CAACRSM.
           COPY CMDATEW.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMGMLNK.
           COPY CMFXLNK.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE
      *
           PERFORM UNTIL WS-ENTL-EOF
               IF ENT-EVENT-ID NOT = WS-CURR-EVENT
                   IF WS-CURR-EVENT NOT = LOW-VALUES
                       PERFORM 3500-END-EVENT
                   END-IF
                   PERFORM 3000-START-EVENT
               END-IF
               PERFORM 4000-PROCESS-ENTITLEMENT
               PERFORM 8000-READ-ENTLMAST
           END-PERFORM
      *
           IF WS-CURR-EVENT NOT = LOW-VALUES
               PERFORM 3500-END-EVENT
           END-IF
      *
           PERFORM 9000-TERMINATE
      *
           MOVE WS-RETURN-CODE TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           INITIALIZE AB-ABEND-PARMS
      *
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS   TO AB-FILE-STATUS
               MOVE 'DATECARD'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON DATE CARD' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1005                 TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS   TO AB-FILE-STATUS
               MOVE 'DATECARD'           TO AB-DDNAME
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                   TO AU-FUNCTION
           MOVE WS-PROGRAM-ID            TO AU-PROGRAM
           MOVE 'START'                  TO AU-EVENT
           MOVE 'I'                      TO AU-SEVERITY
           MOVE DC-BUS-DATE              TO AU-BUS-DATE
           MOVE SPACES                   TO AU-KEY
           MOVE 'CA DIVIDEND ACCRUAL STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT ENTLMAST-FILE
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS   TO AB-FILE-STATUS
               MOVE 'ENTLMAST'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           OPEN INPUT CAEVENT-FILE
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS    TO AB-FILE-STATUS
               MOVE 'CAEVENT'            TO AB-DDNAME
               MOVE 'OPEN FAILED ON EVENT MASTER' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *    BOTH OUTPUTS ARE CREATED EVEN WHEN NOTHING IS ACCRUED
           OPEN OUTPUT GLOUT-FILE
           IF WS-GLOUT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-GLOUT-STATUS      TO AB-FILE-STATUS
               MOVE 'GLOUT'              TO AB-DDNAME
               MOVE 'OPEN FAILED ON GL ACCRUAL FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           OPEN OUTPUT ACCSUM-FILE
           IF WS-ACCSUM-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'    TO AB-PARAGRAPH
               MOVE 1001                 TO AB-ABEND-CODE
               MOVE WS-ACCSUM-STATUS     TO AB-FILE-STATUS
               MOVE 'ACCSUM'             TO AB-DDNAME
               MOVE 'OPEN FAILED ON ACCRUAL SUMMARY FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
           PERFORM 8000-READ-ENTLMAST
           IF WS-ENTL-EOF
               DISPLAY 'CAB500 - ENTITLEMENT MASTER EMPTY - '
                       'EMPTY ACCRUAL FILE CREATED'
           END-IF
           .
      *
      *================================================================*
      * NEW EVENT - DECIDE WHETHER ITS ENTITLEMENTS ARE ACCRUABLE      *
      *================================================================*
       3000-START-EVENT.
           MOVE ENT-EVENT-ID             TO WS-CURR-EVENT
           INITIALIZE WS-EVENT-TOTALS
           MOVE ZERO                     TO WS-ENTL-SEQ
           SET WS-EVENT-ACCRUABLE        TO TRUE
      *
           MOVE ENT-EVENT-ID             TO CAE-EVENT-ID
           READ CAEVENT-FILE
           EVALUATE TRUE
               WHEN CAEVENT-OK
                   EVALUATE TRUE
                       WHEN CAE-ENTITLED
                           CONTINUE
                       WHEN CAE-CANCELLED
                       WHEN CAE-PAID
                           SET WS-EVENT-NOT-ACCRUABLE TO TRUE
                       WHEN OTHER
                           DISPLAY 'CAB500 - EVENT ' CAE-EVENT-ID
                                   ' STATUS ' CAE-STATUS
                                   ' HAS ENTITLEMENTS - NOT ACCRUED'
                           SET WS-EVENT-NOT-ACCRUABLE TO TRUE
                   END-EVALUATE
               WHEN CAEVENT-NOTFND
                   DISPLAY 'CAB500 - EVENT ' ENT-EVENT-ID
                           ' NOT ON EVENT MASTER - NOT ACCRUED'
                   MOVE SPACES           TO CAE-EVENT-REC
                   MOVE ENT-EVENT-ID     TO CAE-EVENT-ID
                   SET WS-EVENT-NOT-ACCRUABLE TO TRUE
                   IF WS-RETURN-CODE < 4
                       MOVE 4            TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE '3000-START-EVENT'   TO AB-PARAGRAPH
                   MOVE 1002                 TO AB-ABEND-CODE
                   MOVE WS-CAEVENT-STATUS    TO AB-FILE-STATUS
                   MOVE 'CAEVENT'            TO AB-DDNAME
                   MOVE ENT-EVENT-ID         TO AB-KEY
                   MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           .
      *
      *================================================================*
      * END OF EVENT - WRITE THE ACCRUAL SUMMARY RECORD                *
      *================================================================*
       3500-END-EVENT.
           IF WS-EV-ACCRUED-COUNT = ZERO
               EXIT PARAGRAPH
           END-IF
      *
           MOVE SPACES                   TO ACS-SUMMARY-REC
           MOVE WS-CURR-EVENT            TO ACS-EVENT-ID
           MOVE CAE-CUSIP                TO ACS-CUSIP
           MOVE CAE-EVENT-TYPE           TO ACS-EVENT-TYPE
           MOVE CAE-PAY-DATE             TO ACS-PAY-DATE
           MOVE CAE-CCY                  TO ACS-CCY
           MOVE WS-EV-ENTL-COUNT         TO ACS-ENTL-COUNT
           MOVE WS-EV-ACCRUED-COUNT      TO ACS-ACCRUED-COUNT
           MOVE WS-EV-AMOUNT             TO ACS-ACCRUAL-AMT
           MOVE WS-EV-USD                TO ACS-ACCRUAL-USD
           MOVE WS-EV-SKIP-COUNT         TO ACS-SKIP-COUNT
           MOVE WS-EV-DR-GL              TO ACS-DR-GL-ACCOUNT
           MOVE WS-EV-CR-GL              TO ACS-CR-GL-ACCOUNT
           MOVE DC-BUS-DATE              TO ACS-BUS-DATE
      *
           WRITE ACCSUM-REC FROM ACS-SUMMARY-REC
           IF WS-ACCSUM-STATUS NOT = '00'
               MOVE '3500-END-EVENT'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-ACCSUM-STATUS     TO AB-FILE-STATUS
               MOVE 'ACCSUM'             TO AB-DDNAME
               MOVE WS-CURR-EVENT        TO AB-KEY
               MOVE 'WRITE FAILED ON ACCRUAL SUMMARY FILE'
                                         TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           ADD 1                         TO WS-SUM-WRITTEN
      *
           MOVE WS-EV-AMOUNT             TO WS-DISP-AMT
           MOVE WS-EV-ACCRUED-COUNT      TO WS-DISP-COUNT
           DISPLAY 'CAB500 - ' WS-CURR-EVENT ' ' CAE-EVENT-TYPE
                   ' ACCRUED ' WS-DISP-COUNT ' ENTITLEMENTS '
                   WS-DISP-AMT ' ' CAE-CCY
           .
      *
      *================================================================*
      * ONE ENTITLEMENT                                                *
      *================================================================*
       4000-PROCESS-ENTITLEMENT.
           ADD 1                         TO WS-EV-ENTL-COUNT
      *
           EVALUATE TRUE
               WHEN WS-EVENT-NOT-ACCRUABLE
                   ADD 1                 TO WS-ENTL-EVENT-SKIP
                   EXIT PARAGRAPH
               WHEN NOT ENT-CALCULATED
                   ADD 1                 TO WS-ENTL-NOT-CA
                   EXIT PARAGRAPH
               WHEN ENT-PAY-DATE NOT > DC-BUS-DATE
                   ADD 1                 TO WS-ENTL-DUE-TODAY
                   EXIT PARAGRAPH
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           COMPUTE WS-ACCRUAL-AMT = ENT-GROSS-CASH + ENT-CIL-AMOUNT
           IF WS-ACCRUAL-AMT NOT > ZERO
               ADD 1                     TO WS-ENTL-NO-CASH
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM 5000-GET-GL-MAP
           IF NOT WS-MAP-FOUND
               ADD 1                     TO WS-EV-SKIP-COUNT
               EXIT PARAGRAPH
           END-IF
      *
           PERFORM 5500-CONVERT-TO-USD
      *
           ADD 1                         TO WS-ENTL-SEQ
           PERFORM 6000-WRITE-GL-PAIR
      *
           ADD 1                         TO WS-EV-ACCRUED-COUNT
                                            WS-ENTL-ACCRUED
           ADD WS-ACCRUAL-AMT            TO WS-EV-AMOUNT
           ADD WS-ACCRUAL-USD            TO WS-EV-USD
                                            WS-TOTAL-USD
           MOVE WS-MR-DR-GL              TO WS-EV-DR-GL
           MOVE WS-MR-CR-GL              TO WS-EV-CR-GL
           .
      *
      *================================================================*
      * GL ACCOUNTS FOR CACR BY ACCOUNT TYPE (CACHED)                  *
      *================================================================*
       5000-GET-GL-MAP.
           MOVE 'N'                      TO WS-MAP-FOUND-SW
           SET MC-IDX                    TO 1
           SEARCH WS-MC-ENTRY
               AT END
                   CONTINUE
               WHEN MC-IDX > WS-MC-COUNT
                   CONTINUE
               WHEN WS-MC-ACCT-TYPE (MC-IDX) = ENT-ACCT-TYPE
                   MOVE WS-MC-FOUND (MC-IDX)    TO WS-MAP-FOUND-SW
                   MOVE WS-MC-DR-GL (MC-IDX)    TO WS-MR-DR-GL
                   MOVE WS-MC-CR-GL (MC-IDX)    TO WS-MR-CR-GL
                   MOVE WS-MC-COST-CTR (MC-IDX) TO WS-MR-COST-CTR
                   MOVE WS-MC-DESC (MC-IDX)     TO WS-MR-DESC
                   IF NOT WS-MAP-FOUND
                       ADD 1             TO WS-MAP-NOT-FOUND
                   END-IF
                   EXIT PARAGRAPH
           END-SEARCH
      *
           MOVE WS-TXN-CODE              TO GM-TXN-CODE
           MOVE ENT-ACCT-TYPE            TO GM-ACCT-TYPE
           MOVE WS-SEC-TYPE              TO GM-SEC-TYPE
           CALL 'CMD050' USING GM-GLMAP-PARMS
      *
           EVALUATE TRUE
               WHEN GM-FOUND
                   MOVE 'Y'              TO WS-MAP-FOUND-SW
                   MOVE GM-DR-GL-ACCOUNT TO WS-MR-DR-GL
                   MOVE GM-CR-GL-ACCOUNT TO WS-MR-CR-GL
                   MOVE GM-COST-CENTER   TO WS-MR-COST-CTR
                   MOVE GM-DESC          TO WS-MR-DESC
               WHEN GM-NOT-FOUND
                   MOVE 'N'              TO WS-MAP-FOUND-SW
                   MOVE SPACES           TO WS-MAP-RESULT
                   ADD 1                 TO WS-MAP-NOT-FOUND
                   IF WS-RETURN-CODE < 4
                       MOVE 4            TO WS-RETURN-CODE
                   END-IF
                   DISPLAY 'CAB500 - NO GL MAP FOR ' WS-TXN-CODE
                           ' ACCT TYPE ' ENT-ACCT-TYPE
                           ' SEC TYPE ' WS-SEC-TYPE
                   MOVE 'WRIT'           TO AU-FUNCTION
                   MOVE 'GLMAPNF'        TO AU-EVENT
                   MOVE 'W'              TO AU-SEVERITY
                   MOVE ENT-KEY          TO AU-KEY
                   MOVE 'NO GL_ACCOUNT_MAP ROW FOR CACR - NOT ACCRUED'
                                         TO AU-MESSAGE
                   CALL 'CMU060' USING AU-AUDIT-PARMS
               WHEN OTHER
                   MOVE '5000-GET-GL-MAP'    TO AB-PARAGRAPH
                   MOVE 1003                 TO AB-ABEND-CODE
                   MOVE GM-SQLCODE           TO AB-SQLCODE
                   MOVE 'CMD050'             TO AB-DDNAME
                   MOVE ENT-KEY              TO AB-KEY
                   MOVE 'DB2 ERROR READING GL_ACCOUNT_MAP'
                                             TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
      *
           IF WS-MC-COUNT < 10
               ADD 1                     TO WS-MC-COUNT
               SET MC-IDX                TO WS-MC-COUNT
               MOVE ENT-ACCT-TYPE        TO WS-MC-ACCT-TYPE (MC-IDX)
               MOVE WS-MAP-FOUND-SW      TO WS-MC-FOUND (MC-IDX)
               MOVE WS-MR-DR-GL          TO WS-MC-DR-GL (MC-IDX)
               MOVE WS-MR-CR-GL          TO WS-MC-CR-GL (MC-IDX)
               MOVE WS-MR-COST-CTR       TO WS-MC-COST-CTR (MC-IDX)
               MOVE WS-MR-DESC           TO WS-MC-DESC (MC-IDX)
           END-IF
           .
      *
      *================================================================*
      * USD EQUIVALENT                                                 *
      *================================================================*
       5500-CONVERT-TO-USD.
           IF ENT-CCY = 'USD' OR ENT-CCY = SPACES
               MOVE WS-ACCRUAL-AMT       TO WS-ACCRUAL-USD
               EXIT PARAGRAPH
           END-IF
      *
           MOVE ENT-CCY                  TO FX-FROM-CCY
           MOVE 'USD'                    TO FX-TO-CCY
           MOVE DC-BUS-DATE              TO FX-RATE-DATE
           MOVE WS-ACCRUAL-AMT           TO FX-AMOUNT-IN
           CALL 'CMU040' USING FX-CONVERT-PARMS
      *
           EVALUATE TRUE
               WHEN FX-OK
               WHEN FX-STALE-RATE
                   MOVE FX-AMOUNT-OUT    TO WS-ACCRUAL-USD
               WHEN FX-RATE-NOT-FOUND
                   MOVE ZERO             TO WS-ACCRUAL-USD
                   ADD 1                 TO WS-FX-NOT-FOUND
                   IF WS-RETURN-CODE < 4
                       MOVE 4            TO WS-RETURN-CODE
                   END-IF
                   DISPLAY 'CAB500 - NO FX RATE ' ENT-CCY
                           ' - USD AMOUNT ZERO FOR ' ENT-KEY
               WHEN OTHER
                   MOVE '5500-CONVERT-TO-USD' TO AB-PARAGRAPH
                   MOVE 1010                 TO AB-ABEND-CODE
                   MOVE SPACES               TO AB-FILE-STATUS
                   MOVE 'CMU040'             TO AB-DDNAME
                   MOVE ENT-KEY              TO AB-KEY
                   MOVE FX-MESSAGE           TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           .
      *
      *================================================================*
      * DEBIT RECEIVABLE / CREDIT INCOME CLEARING, AUTO-REVERSING      *
      *================================================================*
       6000-WRITE-GL-PAIR.
           MOVE ENT-EVENT-ID             TO WS-GREF-EVENT-ID
           MOVE WS-ENTL-SEQ              TO WS-GREF-SEQ
      *
           MOVE SPACES                   TO GLJ-JOURNAL-REC
           MOVE DC-BUS-DATE              TO GLJ-BUS-DATE
           MOVE 'CA'                     TO GLJ-SOURCE
           MOVE WS-GLJ-REF-WORK          TO GLJ-REF
           MOVE 1                        TO GLJ-LINE-NO
           MOVE WS-TXN-CODE              TO GLJ-TXN-CODE
           MOVE WS-MR-DR-GL              TO GLJ-GL-ACCOUNT
           MOVE WS-MR-COST-CTR           TO GLJ-COST-CENTER
           SET GLJ-DEBIT                 TO TRUE
           MOVE WS-ACCRUAL-AMT           TO GLJ-AMOUNT
           MOVE ENT-CCY                  TO GLJ-CCY
           MOVE WS-ACCRUAL-USD           TO GLJ-AMOUNT-USD
           MOVE ENT-ACCT-NO              TO GLJ-ACCT-NO
           MOVE ENT-CUSIP                TO GLJ-CUSIP
           STRING 'ACCR ' ENT-EVENT-TYPE ' ' ENT-EVENT-ID
                  DELIMITED BY SIZE INTO GLJ-DESC
           SET GLJ-AUTO-REVERSE          TO TRUE
           PERFORM 8100-WRITE-GL
           ADD WS-ACCRUAL-AMT            TO WS-TOTAL-DR
      *
           MOVE 2                        TO GLJ-LINE-NO
           MOVE WS-MR-CR-GL              TO GLJ-GL-ACCOUNT
           SET GLJ-CREDIT                TO TRUE
           PERFORM 8100-WRITE-GL
           ADD WS-ACCRUAL-AMT            TO WS-TOTAL-CR
           .
      *
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-ENTLMAST.
           READ ENTLMAST-FILE NEXT RECORD
           EVALUATE TRUE
               WHEN ENTLMAST-OK
                   ADD 1                 TO WS-ENTL-READ
               WHEN ENTLMAST-EOF
                   SET WS-ENTL-EOF       TO TRUE
               WHEN OTHER
                   MOVE '8000-READ-ENTLMAST' TO AB-PARAGRAPH
                   MOVE 1002                 TO AB-ABEND-CODE
                   MOVE WS-ENTLMAST-STATUS   TO AB-FILE-STATUS
                   MOVE 'ENTLMAST'           TO AB-DDNAME
                   MOVE 'READ FAILED ON ENTITLEMENT MASTER'
                                             TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE
           .
      *
       8100-WRITE-GL.
           WRITE GLOUT-REC FROM GLJ-JOURNAL-REC
           IF WS-GLOUT-STATUS NOT = '00'
               MOVE '8100-WRITE-GL'      TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-GLOUT-STATUS      TO AB-FILE-STATUS
               MOVE 'GLOUT'              TO AB-DDNAME
               MOVE GLJ-REF              TO AB-KEY
               MOVE 'WRITE FAILED ON GL ACCRUAL FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           ADD 1                         TO WS-GL-WRITTEN
           .
      *
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE ENTLMAST-FILE
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS   TO AB-FILE-STATUS
               MOVE 'ENTLMAST'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE CAEVENT-FILE
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS    TO AB-FILE-STATUS
               MOVE 'CAEVENT'            TO AB-DDNAME
               MOVE 'CLOSE FAILED ON EVENT MASTER' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE GLOUT-FILE
           IF WS-GLOUT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-GLOUT-STATUS      TO AB-FILE-STATUS
               MOVE 'GLOUT'              TO AB-DDNAME
               MOVE 'CLOSE FAILED ON GL ACCRUAL FILE' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           CLOSE ACCSUM-FILE
           IF WS-ACCSUM-STATUS NOT = '00'
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1002                 TO AB-ABEND-CODE
               MOVE WS-ACCSUM-STATUS     TO AB-FILE-STATUS
               MOVE 'ACCSUM'             TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ACCRUAL SUMMARY FILE'
                                         TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
      *    JOURNAL MUST BALANCE
           IF WS-TOTAL-DR NOT = WS-TOTAL-CR
               MOVE '9000-TERMINATE'     TO AB-PARAGRAPH
               MOVE 1004                 TO AB-ABEND-CODE
               MOVE SPACES               TO AB-FILE-STATUS
               MOVE 'GLOUT'              TO AB-DDNAME
               MOVE 'ACCRUAL JOURNAL DEBITS NOT EQUAL CREDITS'
                                         TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
      *
           MOVE 'ENTL-IN'                TO CT-COUNTER-NAME
           MOVE WS-ENTL-READ             TO CT-COUNT
           MOVE ZERO                     TO CT-AMOUNT
                                            CT-QTY-HASH
           PERFORM 9100-POST-TOTAL
           MOVE 'ACCRUAL-OUT'            TO CT-COUNTER-NAME
           MOVE WS-GL-WRITTEN            TO CT-COUNT
           MOVE WS-TOTAL-DR              TO CT-AMOUNT
           MOVE ZERO                     TO CT-QTY-HASH
           PERFORM 9100-POST-TOTAL
           MOVE 'ACCR-SUM-OUT'           TO CT-COUNTER-NAME
           MOVE WS-SUM-WRITTEN           TO CT-COUNT
           MOVE WS-TOTAL-USD             TO CT-AMOUNT
           PERFORM 9100-POST-TOTAL
           MOVE 'CLOS'                   TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* CAB500  CA DIVIDEND ACCRUAL   - RUN STATISTICS *'
           DISPLAY '*************************************************'
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE
           MOVE WS-ENTL-READ             TO WS-DISP-COUNT
           DISPLAY ' ENTITLEMENTS READ        : ' WS-DISP-COUNT
           MOVE WS-ENTL-ACCRUED          TO WS-DISP-COUNT
           DISPLAY ' ENTITLEMENTS ACCRUED     : ' WS-DISP-COUNT
           MOVE WS-ENTL-NOT-CA           TO WS-DISP-COUNT
           DISPLAY ' NOT STATUS CA            : ' WS-DISP-COUNT
           MOVE WS-ENTL-DUE-TODAY        TO WS-DISP-COUNT
           DISPLAY ' PAY DATE REACHED         : ' WS-DISP-COUNT
           MOVE WS-ENTL-NO-CASH          TO WS-DISP-COUNT
           DISPLAY ' NO CASH COMPONENT        : ' WS-DISP-COUNT
           MOVE WS-ENTL-EVENT-SKIP       TO WS-DISP-COUNT
           DISPLAY ' EVENT NOT ACCRUABLE      : ' WS-DISP-COUNT
           MOVE WS-MAP-NOT-FOUND         TO WS-DISP-COUNT
           DISPLAY ' GL MAP NOT FOUND         : ' WS-DISP-COUNT
           MOVE WS-FX-NOT-FOUND          TO WS-DISP-COUNT
           DISPLAY ' FX RATE NOT FOUND        : ' WS-DISP-COUNT
           MOVE WS-GL-WRITTEN            TO WS-DISP-COUNT
           DISPLAY ' GL LINES WRITTEN         : ' WS-DISP-COUNT
           MOVE WS-SUM-WRITTEN           TO WS-DISP-COUNT
           DISPLAY ' SUMMARY RECORDS WRITTEN  : ' WS-DISP-COUNT
           MOVE WS-TOTAL-DR              TO WS-DISP-AMT
           DISPLAY ' TOTAL DEBITS             : ' WS-DISP-AMT
           MOVE WS-TOTAL-CR              TO WS-DISP-AMT
           DISPLAY ' TOTAL CREDITS            : ' WS-DISP-AMT
           MOVE WS-TOTAL-USD             TO WS-DISP-AMT
           DISPLAY ' TOTAL USD EQUIVALENT     : ' WS-DISP-AMT
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE
      *
           MOVE 'WRIT'                   TO AU-FUNCTION
           MOVE WS-PROGRAM-ID            TO AU-PROGRAM
           MOVE 'END'                    TO AU-EVENT
           IF WS-RETURN-CODE > 0
               MOVE 'W'                  TO AU-SEVERITY
           ELSE
               MOVE 'I'                  TO AU-SEVERITY
           END-IF
           MOVE DC-BUS-DATE              TO AU-BUS-DATE
           MOVE SPACES                   TO AU-KEY
           MOVE 'CA DIVIDEND ACCRUAL ENDED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                   TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
           .
      *
       9100-POST-TOTAL.
           MOVE 'POST'                   TO CT-FUNCTION
           MOVE DC-BUS-DATE              TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID            TO CT-PROGRAM
           MOVE 'CAB500'                 TO CT-STAGE
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE '9100-POST-TOTAL'    TO AB-PARAGRAPH
               MOVE 1010                 TO AB-ABEND-CODE
               MOVE SPACES               TO AB-FILE-STATUS
               MOVE 'CTLTOTS'            TO AB-DDNAME
               MOVE CT-COUNTER-NAME      TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF
           .
      *
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID            TO AB-PROGRAM
           DISPLAY 'CAB500 - ABENDING: ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                       TO RETURN-CODE
           GOBACK
           .
