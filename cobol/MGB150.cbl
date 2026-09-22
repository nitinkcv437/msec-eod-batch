       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGB150.
       AUTHOR.        S P AGARWAL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 2011.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGB150                                            *
      * DESCRIPTION: MARGIN COLLATERAL CONCENTRATION BY ISSUER.        *
      *              FIRM-WIDE VIEW OF THE COLLATERAL BEHIND CLIENT    *
      *              MARGIN LOANS, FOR CREDIT RISK (REQUEST CR-2011-07)*
      *              INPUT IS THE POSITION REQUIREMENT DETAIL FROM     *
      *              MGB100 SORTED BY ISSUER / ACCOUNT / CUSIP.        *
      *              PASS 1 TOTALS THE FIRM'S LONG MARGIN COLLATERAL.  *
      *              PASS 2 WRITES ONE RECORD PER ISSUER:              *
      *                LONG / SHORT / NON-MARGINABLE MV, HOUSE         *
      *                REQUIREMENT, LOAN VALUE (LONG MARGINABLE MV     *
      *                LESS ITS REQUIREMENT), PERCENT OF THE FIRM      *
      *                TOTAL, NUMBER OF ACCOUNTS AND CUSIPS, AND THE   *
      *                LARGEST SINGLE ACCOUNT.                         *
      *              FLAGS: F  ISSUER ABOVE THE FIRM LIMIT (5 PCT OF   *
      *                        ALL LONG MARGIN COLLATERAL)             *
      *                     S  ONE ACCOUNT HOLDS MORE THAN HALF OF A   *
      *                        LARGE ISSUER POSITION (> 1,000,000)     *
      *                     B  BOTH                                    *
      *              A FIRM TOTAL RECORD ('T') IS WRITTEN LAST.        *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD010 / STEP050  (IKJEFT01 - DB2 PLAN MSMGPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              POSNIN   - MSEC.PROD.MG.POSREQ.ISSUER(+1) (MGPREQ)*
      * OUTPUT     : CONCOUT  - MSEC.PROD.MG.ISSCONC(+1)       (MGISSC)*
      * CALLS      : CMD010 (ISSUER NAME FROM THE FIRST CUSIP),        *
      *              CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  AN ISSUER IS ABOVE THE FIRM LIMIT              *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2011-03-21 SPA  ORIGINAL                              CHG21402 *
      * 2011-09-12 SPA  LOAN VALUE COLUMN                     CHG22301 *
      * 2014-01-13 SPA  ISSUER NAME FROM SECURITY MASTER      CHG25507 *
      * 2017-08-07 MHC  SINGLE ACCOUNT DOMINANCE FLAG         CHG31877 *
      * 2020-03-16 MHC  FIRM LIMIT LOWERED 10 -> 5 PCT        CHG35190 *
      *                 (CREDIT RISK COMMITTEE 2020-03-11)             *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT POSNIN-FILE    ASSIGN TO POSNIN
                  FILE STATUS IS WS-POSNIN-STATUS.
           SELECT CONCOUT-FILE   ASSIGN TO CONCOUT
                  FILE STATUS IS WS-CONCOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  POSNIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGPREQ.
       FD  CONCOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CONCOUT-REC                 PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGB150'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-POSNIN-STATUS        PIC X(02)  VALUE '00'.
               88  POSNIN-OK                      VALUE '00'.
               88  POSNIN-EOF                     VALUE '10'.
           05  WS-CONCOUT-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-POSITIONS               VALUE 'Y'.
           05  WS-PASS                 PIC 9(01)  VALUE 1.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * LIMITS (CREDIT RISK COMMITTEE)                                 *
      *----------------------------------------------------------------*
       01  WS-FIRM-LIMIT-PCT           PIC S9(03)V9(04) COMP-3
                                                     VALUE +5.0000.
       01  WS-SINGLE-ACCT-PCT          PIC S9(03)V9(04) COMP-3
                                                     VALUE +50.0000.
       01  WS-SINGLE-ACCT-MIN-MV       PIC S9(15)V99    COMP-3
                                                     VALUE +1000000.00.
      *----------------------------------------------------------------*
      * CONTROL BREAK WORK                                             *
      *----------------------------------------------------------------*
       01  WS-PREV-KEYS.
           05  WS-PREV-ISSUER          PIC X(06)  VALUE LOW-VALUES.
           05  WS-PREV-ACCT            PIC X(10)  VALUE LOW-VALUES.
       01  WS-ISSUER-WORK.
           05  WS-IW-FIRST-CUSIP       PIC X(09).
           05  WS-IW-SEC-TYPE          PIC X(02).
           05  WS-IW-ACCT-CNT          PIC S9(07)       COMP-3.
           05  WS-IW-LONG-MV           PIC S9(15)V99    COMP-3.
           05  WS-IW-SHORT-MV          PIC S9(15)V99    COMP-3.
           05  WS-IW-NONMARG-MV        PIC S9(15)V99    COMP-3.
           05  WS-IW-REQ               PIC S9(15)V99    COMP-3.
           05  WS-IW-MARG-LONG         PIC S9(15)V99    COMP-3.
           05  WS-IW-MARG-LONG-REQ     PIC S9(15)V99    COMP-3.
           05  WS-IW-LARGEST-ACCT      PIC X(10).
           05  WS-IW-LARGEST-MV        PIC S9(15)V99    COMP-3.
           05  WS-IW-ACCT-MV           PIC S9(15)V99    COMP-3.
       01  WS-CUSIP-TABLE.
           05  WS-CT-COUNT             PIC S9(04) COMP  VALUE ZERO.
           05  WS-CT-ENTRY OCCURS 99 TIMES INDEXED BY CT-IDX.
               10  WS-CT-CUSIP         PIC X(09).
       01  WS-CALC.
           05  WS-LOAN-VALUE           PIC S9(15)V99    COMP-3.
           05  WS-PCT-OF-FIRM          PIC S9(03)V9(04) COMP-3.
           05  WS-LARGEST-PCT          PIC S9(03)V9(04) COMP-3.
       01  WS-FIRM-TOTALS.
           05  WS-FT-LONG-MV           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FT-SHORT-MV          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FT-NONMARG-MV        PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FT-REQ               PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FT-LOAN-VALUE        PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-FT-PASS2-LONG        PIC S9(15)V99 COMP-3 VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-PASS1-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PASS2-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ISSUER-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-TOTAL-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAG-F-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAG-S-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CUSIP-OVFL-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-NAME-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OUT-CNT              PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       COPY MGISSC.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMSECLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE.
      *    PASS 1 - FIRM TOTAL OF LONG COLLATERAL
           PERFORM 8000-OPEN-INPUT.
           PERFORM 8100-READ-POSITION.
           PERFORM UNTIL END-OF-POSITIONS
               ADD 1 TO WS-PASS1-CNT
               IF MPQ-MKT-VALUE-USD > ZERO
                   ADD MPQ-MKT-VALUE-USD TO WS-FT-LONG-MV
               END-IF
               PERFORM 8100-READ-POSITION
           END-PERFORM.
           PERFORM 8200-CLOSE-INPUT.
      *    PASS 2 - ONE RECORD PER ISSUER
           MOVE 2 TO WS-PASS.
           PERFORM 8000-OPEN-INPUT.
           PERFORM 8100-READ-POSITION.
           IF NOT END-OF-POSITIONS
               PERFORM 2100-START-ISSUER
           END-IF.
           PERFORM 2000-PROCESS-POSITION UNTIL END-OF-POSITIONS.
           IF WS-PASS2-CNT > ZERO
               PERFORM 3000-END-ISSUER
           END-IF.
           PERFORM 8200-CLOSE-INPUT.
           PERFORM 4000-FIRM-TOTAL.
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
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'ISSUER CONCENTRATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN OUTPUT CONCOUT-FILE.
           IF WS-CONCOUT-STATUS NOT = '00'
               MOVE 'CONCOUT' TO AB-DDNAME
               MOVE WS-CONCOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
      * PASS 2 - ONE POSITION DETAIL                                   *
      *================================================================*
       2000-PROCESS-POSITION.
           IF MPQ-ISSUER-ID NOT = WS-PREV-ISSUER
               PERFORM 3000-END-ISSUER
               PERFORM 2100-START-ISSUER
           END-IF.
           ADD 1 TO WS-PASS2-CNT.
           IF MPQ-ACCT-NO NOT = WS-PREV-ACCT
               PERFORM 2200-END-ACCOUNT
               MOVE MPQ-ACCT-NO TO WS-PREV-ACCT
               ADD 1 TO WS-IW-ACCT-CNT
           END-IF.
           EVALUATE TRUE
               WHEN MPQ-MKT-VALUE-USD < ZERO
                   SUBTRACT MPQ-MKT-VALUE-USD FROM WS-IW-SHORT-MV
               WHEN MPQ-MARGINABLE-FLAG = 'N'
                   ADD MPQ-MKT-VALUE-USD TO WS-IW-LONG-MV
                                            WS-IW-NONMARG-MV
                                            WS-IW-ACCT-MV
               WHEN OTHER
                   ADD MPQ-MKT-VALUE-USD TO WS-IW-LONG-MV
                                            WS-IW-MARG-LONG
                                            WS-IW-ACCT-MV
                   ADD MPQ-REQ-AMOUNT    TO WS-IW-MARG-LONG-REQ
           END-EVALUATE.
           ADD MPQ-REQ-AMOUNT TO WS-IW-REQ.
           PERFORM 2300-COUNT-CUSIP.
           PERFORM 8100-READ-POSITION.
      *----------------------------------------------------------------*
       2100-START-ISSUER.
      *----------------------------------------------------------------*
           INITIALIZE WS-ISSUER-WORK.
           MOVE MPQ-ISSUER-ID TO WS-PREV-ISSUER.
           MOVE LOW-VALUES    TO WS-PREV-ACCT.
           MOVE MPQ-CUSIP     TO WS-IW-FIRST-CUSIP.
           MOVE MPQ-SEC-TYPE  TO WS-IW-SEC-TYPE.
           MOVE SPACES        TO WS-IW-LARGEST-ACCT.
           MOVE ZERO          TO WS-CT-COUNT.
      *----------------------------------------------------------------*
      * LARGEST SINGLE ACCOUNT WITHIN THE ISSUER                       *
      *----------------------------------------------------------------*
       2200-END-ACCOUNT.
           IF WS-PREV-ACCT NOT = LOW-VALUES
               IF WS-IW-ACCT-MV > WS-IW-LARGEST-MV
                   MOVE WS-IW-ACCT-MV TO WS-IW-LARGEST-MV
                   MOVE WS-PREV-ACCT  TO WS-IW-LARGEST-ACCT
               END-IF
           END-IF.
           MOVE ZERO TO WS-IW-ACCT-MV.
      *----------------------------------------------------------------*
       2300-COUNT-CUSIP.
      *----------------------------------------------------------------*
           SET CT-IDX TO 1.
           SEARCH WS-CT-ENTRY
               AT END
                   ADD 1 TO WS-CUSIP-OVFL-CNT
               WHEN CT-IDX > WS-CT-COUNT
                   ADD 1 TO WS-CT-COUNT
                   MOVE MPQ-CUSIP TO WS-CT-CUSIP (CT-IDX)
               WHEN WS-CT-CUSIP (CT-IDX) = MPQ-CUSIP
                   CONTINUE
           END-SEARCH.
      *================================================================*
      * ISSUER COMPLETE - PERCENTAGES, FLAGS, NAME, WRITE              *
      *================================================================*
       3000-END-ISSUER.
           PERFORM 2200-END-ACCOUNT.
           ADD 1 TO WS-ISSUER-CNT.
           ADD WS-IW-ACCT-CNT TO WS-ACCT-TOTAL-CNT.
           COMPUTE WS-LOAN-VALUE =
               WS-IW-MARG-LONG - WS-IW-MARG-LONG-REQ.
           IF WS-FT-LONG-MV > ZERO
               COMPUTE WS-PCT-OF-FIRM ROUNDED =
                   WS-IW-LONG-MV * 100 / WS-FT-LONG-MV
           ELSE
               MOVE ZERO TO WS-PCT-OF-FIRM
           END-IF.
           IF WS-IW-LONG-MV > ZERO
               COMPUTE WS-LARGEST-PCT ROUNDED =
                   WS-IW-LARGEST-MV * 100 / WS-IW-LONG-MV
           ELSE
               MOVE ZERO TO WS-LARGEST-PCT
           END-IF.
           MOVE SPACES TO MIC-ISSUER-CONC-REC.
           MOVE 'D'                 TO MIC-REC-TYPE.
           MOVE DC-BUS-DATE         TO MIC-BUS-DATE.
           MOVE WS-PREV-ISSUER      TO MIC-ISSUER-ID.
           PERFORM 3100-ISSUER-NAME.
           MOVE WS-IW-SEC-TYPE      TO MIC-SEC-TYPE.
           MOVE WS-CT-COUNT         TO MIC-CUSIP-COUNT.
           MOVE WS-IW-ACCT-CNT      TO MIC-ACCT-COUNT.
           MOVE WS-IW-LONG-MV       TO MIC-LONG-MV.
           MOVE WS-IW-SHORT-MV      TO MIC-SHORT-MV.
           MOVE WS-IW-NONMARG-MV    TO MIC-NONMARG-MV.
           MOVE WS-IW-REQ           TO MIC-REQ-AMOUNT.
           MOVE WS-LOAN-VALUE       TO MIC-LOAN-VALUE.
           MOVE WS-PCT-OF-FIRM      TO MIC-PCT-OF-FIRM.
           MOVE WS-IW-LARGEST-ACCT  TO MIC-LARGEST-ACCT.
           MOVE WS-IW-LARGEST-MV    TO MIC-LARGEST-ACCT-MV.
           MOVE WS-LARGEST-PCT      TO MIC-LARGEST-ACCT-PCT.
           MOVE SPACE               TO MIC-FLAG.
           IF WS-PCT-OF-FIRM > WS-FIRM-LIMIT-PCT
               MOVE 'F' TO MIC-FLAG
               ADD 1 TO WS-FLAG-F-CNT
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           IF WS-LARGEST-PCT > WS-SINGLE-ACCT-PCT
           AND WS-IW-LONG-MV > WS-SINGLE-ACCT-MIN-MV
           AND WS-IW-ACCT-CNT > 1
               ADD 1 TO WS-FLAG-S-CNT
               IF MIC-FLAG = 'F'
                   MOVE 'B' TO MIC-FLAG
               ELSE
                   MOVE 'S' TO MIC-FLAG
               END-IF
           END-IF.
           PERFORM 8300-WRITE-CONC.
           ADD WS-IW-LONG-MV    TO WS-FT-PASS2-LONG.
           ADD WS-IW-SHORT-MV   TO WS-FT-SHORT-MV.
           ADD WS-IW-NONMARG-MV TO WS-FT-NONMARG-MV.
           ADD WS-IW-REQ        TO WS-FT-REQ.
           ADD WS-LOAN-VALUE    TO WS-FT-LOAN-VALUE.
      *----------------------------------------------------------------*
      * ISSUER NAME = DESCRIPTION OF THE FIRST CUSIP OF THE ISSUER     *
      *----------------------------------------------------------------*
       3100-ISSUER-NAME.
           MOVE 'GET '             TO SL-FUNCTION.
           MOVE WS-IW-FIRST-CUSIP  TO SL-KEY-CUSIP.
           MOVE SPACES             TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA TO SEC-MASTER-REC
                   MOVE SEC-DESC (1:30) TO MIC-ISSUER-NAME
               WHEN SL-NOT-FOUND
                   ADD 1 TO WS-NO-NAME-CNT
                   MOVE '*** NOT ON SECURITY MASTER ***'
                                     TO MIC-ISSUER-NAME
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '3100-ISSUER-NAME' TO AB-PARAGRAPH
                   MOVE SL-SQLCODE TO AB-SQLCODE
                   MOVE WS-IW-FIRST-CUSIP TO AB-KEY
                   MOVE 'CMD010 SECURITY MASTER ERROR' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *================================================================*
      * FIRM TOTAL RECORD                                              *
      *================================================================*
       4000-FIRM-TOTAL.
           IF WS-FT-PASS2-LONG NOT = WS-FT-LONG-MV
               MOVE 'POSNIN' TO AB-DDNAME
               MOVE 1004 TO AB-ABEND-CODE
               MOVE '4000-FIRM-TOTAL' TO AB-PARAGRAPH
               MOVE 'PASS 1 AND PASS 2 LONG MV DIFFER' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE SPACES TO MIC-ISSUER-CONC-REC.
           MOVE 'T'                 TO MIC-REC-TYPE.
           MOVE DC-BUS-DATE         TO MIC-BUS-DATE.
           MOVE '******'            TO MIC-ISSUER-ID.
           MOVE 'FIRM TOTAL - MARGIN COLLATERAL' TO MIC-ISSUER-NAME.
           MOVE '**'                TO MIC-SEC-TYPE.
           MOVE WS-ISSUER-CNT       TO MIC-CUSIP-COUNT.
           MOVE WS-ACCT-TOTAL-CNT   TO MIC-ACCT-COUNT.
           MOVE WS-FT-LONG-MV       TO MIC-LONG-MV.
           MOVE WS-FT-SHORT-MV      TO MIC-SHORT-MV.
           MOVE WS-FT-NONMARG-MV    TO MIC-NONMARG-MV.
           MOVE WS-FT-REQ           TO MIC-REQ-AMOUNT.
           MOVE WS-FT-LOAN-VALUE    TO MIC-LOAN-VALUE.
           MOVE 100                 TO MIC-PCT-OF-FIRM.
           MOVE SPACES              TO MIC-LARGEST-ACCT.
           MOVE ZERO                TO MIC-LARGEST-ACCT-MV
                                       MIC-LARGEST-ACCT-PCT.
           MOVE SPACE               TO MIC-FLAG.
           PERFORM 8300-WRITE-CONC.
      *================================================================*
       8000-OPEN-INPUT.
      *================================================================*
           MOVE 'N' TO WS-EOF-SW.
           OPEN INPUT POSNIN-FILE.
           IF WS-POSNIN-STATUS NOT = '00'
               MOVE 'POSNIN' TO AB-DDNAME
               MOVE WS-POSNIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '8000-OPEN-INPUT' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
       8100-READ-POSITION.
      *----------------------------------------------------------------*
           READ POSNIN-FILE.
           EVALUATE TRUE
               WHEN POSNIN-OK
                   IF WS-PASS = 2 AND MPQ-ISSUER-ID < WS-PREV-ISSUER
                       MOVE 'POSNIN' TO AB-DDNAME
                       MOVE 1006 TO AB-ABEND-CODE
                       MOVE '8100-READ-POSITION' TO AB-PARAGRAPH
                       MOVE MPQ-ISSUER-ID TO AB-KEY
                       MOVE 'INPUT NOT IN ISSUER SEQUENCE' TO AB-MESSAGE
                       PERFORM 9999-ABEND
                   END-IF
               WHEN POSNIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'POSNIN' TO AB-DDNAME
                   MOVE WS-POSNIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-POSITION' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8200-CLOSE-INPUT.
      *----------------------------------------------------------------*
           CLOSE POSNIN-FILE.
           IF WS-POSNIN-STATUS NOT = '00'
               MOVE 'POSNIN' TO AB-DDNAME
               MOVE WS-POSNIN-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-CLOSE-INPUT' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
       8300-WRITE-CONC.
      *----------------------------------------------------------------*
           WRITE CONCOUT-REC FROM MIC-ISSUER-CONC-REC.
           IF WS-CONCOUT-STATUS NOT = '00'
               MOVE 'CONCOUT' TO AB-DDNAME
               MOVE WS-CONCOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8300-WRITE-CONC' TO AB-PARAGRAPH
               MOVE MIC-ISSUER-ID TO AB-KEY
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
           MOVE 'MGB150'       TO CT-STAGE.
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
           CLOSE CONCOUT-FILE.
           IF WS-CONCOUT-STATUS NOT = '00'
               MOVE 'CONCOUT' TO AB-DDNAME
               MOVE WS-CONCOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'POSREQ-IN'       TO CT-COUNTER-NAME.
           MOVE WS-PASS2-CNT      TO CT-COUNT.
           MOVE WS-FT-LONG-MV     TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL.
           MOVE 'ISSUERS-OUT'     TO CT-COUNTER-NAME.
           MOVE WS-ISSUER-CNT     TO CT-COUNT.
           MOVE WS-FT-LOAN-VALUE  TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL.
           MOVE 'OVER-LIMIT'      TO CT-COUNTER-NAME.
           MOVE WS-FLAG-F-CNT     TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL.
           DISPLAY '************************************************'.
           DISPLAY '* MGB150 - COLLATERAL CONCENTRATION BY ISSUER  *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-PASS1-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS READ PASS 1    : ' WS-DISP-CNT.
           MOVE WS-PASS2-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS READ PASS 2    : ' WS-DISP-CNT.
           MOVE WS-ISSUER-CNT TO WS-DISP-CNT.
           DISPLAY ' ISSUERS                  : ' WS-DISP-CNT.
           MOVE WS-FLAG-F-CNT TO WS-DISP-CNT.
           DISPLAY ' ABOVE FIRM LIMIT (F/B)   : ' WS-DISP-CNT.
           MOVE WS-FLAG-S-CNT TO WS-DISP-CNT.
           DISPLAY ' SINGLE ACCOUNT (S/B)     : ' WS-DISP-CNT.
           MOVE WS-NO-NAME-CNT TO WS-DISP-CNT.
           DISPLAY ' NAME NOT FOUND           : ' WS-DISP-CNT.
           MOVE WS-CUSIP-OVFL-CNT TO WS-DISP-CNT.
           DISPLAY ' CUSIP TABLE OVERFLOW     : ' WS-DISP-CNT.
           MOVE WS-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' RECORDS OUT (INCL TOTAL) : ' WS-DISP-CNT.
           MOVE WS-FT-LONG-MV TO WS-DISP-AMT.
           DISPLAY ' LONG COLLATERAL          : ' WS-DISP-AMT.
           MOVE WS-FT-LOAN-VALUE TO WS-DISP-AMT.
           DISPLAY ' LOAN VALUE               : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'ISSUER CONCENTRATION ENDED' TO AU-MESSAGE.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
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
           DISPLAY 'MGB150 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'MGB150 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
