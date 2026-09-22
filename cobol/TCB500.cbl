       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCB500.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  09/01/1987.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCB500                                            *
      * TITLE      : TRADE EXTRACTS - STOCK RECORD ACTIVITY AND        *
      *              SETTLEMENT INSTRUCTIONS                           *
      * JOB        : MSTCD070   STEP010 (IKJEFT01 - DB2 PLAN MSTCPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   FOR EVERY FINAL TRADE TWO STOCK RECORD ACTIVITY LEGS ARE     *
      *   WRITTEN (SRACTV):                                            *
      *     LEG 1  OWNERSHIP - CLIENT OR FIRM ACCOUNT                  *
      *            BUY  +QTY  CASH -NET  COST +NET                     *
      *            SELL -QTY  CASH +NET                                *
      *     LEG 2  LOCATION  - STREET ACCOUNT OF THE DEPOSITORY        *
      *            QTY = -(LEG 1 QTY), NO CASH                         *
      *   CANCELS ARE WRITTEN AS REVERSALS (XBY / XSL) WITH ALL SIGNS  *
      *   INVERTED.  A SETTLEMENT INSTRUCTION (TCSETIN) IS WRITTEN FOR *
      *   DVP ACCOUNTS AND FOR HOUSE TRADES AGAINST A STREET CONTRA:   *
      *     BUY  MT541 RECEIVE VS PAYMENT                              *
      *     SELL MT543 DELIVER VS PAYMENT                              *
      *     CANCEL = SAME MESSAGE TYPE, FUNCTION CANC                  *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     PARAMETER CARDS TCP500A                     *
      *          TRADEIN   MSEC.PROD.TC.TRADES.FINAL(0)  (TCTRADE)     *
      *          ACCTMAST  MSEC.PROD.CM.ACCTMAST.KSDS    (CMACCT)      *
      * OUTPUT : ACTVOUT   MSEC.PROD.TC.SRACTV(+1)       (SRACTV)      *
      *          SETLOUT   MSEC.PROD.TC.SETLINST(+1)     (TCSETIN)     *
      * CALLS  : CMD010 CMU050 CMU060 CMU080                           *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 ACCOUNT OR ISIN NOT FOUND             *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1987-09-01 RJK            ORIGINAL - STOCK RECORD EXTRACT      *
      * 1990-02-19 RJK            CANCEL REVERSALS                     *
      * 1993-05-24 DWB            STREET CONTRA TRADES FOR FIRM ACCTS  *
      * 1996-04-22 DWB  CHG02215  SECURITY MASTER NOW DB2 (CMD010)     *
      * 1997-10-06 DWB  CHG03390  SETTLEMENT INSTRUCTIONS (ISO 15022)  *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD                 *
      * 2005-08-30 KAP  CHG13391  ISIN ON INSTRUCTIONS                 *
      * 2009-12-14 SPA  CHG19002  EUROCLEAR STREET ACCOUNT             *
      * 2010-04-05 SPA  CHG19870  ACT-SOURCE 'TC'                      *
      * 2014-11-17 SPA  CHG27740  GL TRANSACTION CODE ON ACTIVITY      *
      * 2024-02-12 NVR  CHG41007  T+1 - SETTLE FLAG FROM SETTLE DATE   *
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
           SELECT TRADEIN-FILE  ASSIGN TO TRADEIN
                                FILE STATUS IS WS-TRADEIN-FS.
           SELECT ACCTMAST-FILE ASSIGN TO ACCTMAST
                                ORGANIZATION IS INDEXED
                                ACCESS MODE IS RANDOM
                                RECORD KEY IS ACCT-NO
                                FILE STATUS IS WS-ACCTMAST-FS.
           SELECT ACTVOUT-FILE  ASSIGN TO ACTVOUT
                                FILE STATUS IS WS-ACTVOUT-FS.
           SELECT SETLOUT-FILE  ASSIGN TO SETLOUT
                                FILE STATUS IS WS-SETLOUT-FS.
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
       FD  TRADEIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCTRADE.
       FD  ACCTMAST-FILE.
       COPY CMACCT.
       FD  ACTVOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRACTV.
       FD  SETLOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCSETIN.
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCB500'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-TRADEIN-FS           PIC X(02).
               88  TRADEIN-OK                    VALUE '00'.
               88  TRADEIN-EOF                   VALUE '10'.
           05  WS-ACCTMAST-FS          PIC X(02).
               88  ACCTMAST-OK                   VALUE '00'.
               88  ACCTMAST-NOTFND               VALUE '23'.
           05  WS-ACTVOUT-FS           PIC X(02).
               88  ACTVOUT-OK                    VALUE '00'.
           05  WS-SETLOUT-FS           PIC X(02).
               88  SETLOUT-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-TRADES              VALUE 'Y'.
           05  WS-ACCT-FOUND-SW        PIC X(01)  VALUE 'N'.
               88  WS-ACCT-FOUND                 VALUE 'Y'.
           05  WS-INSTR-SW             PIC X(01)  VALUE 'N'.
               88  WS-INSTRUCTION-NEEDED         VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-INSTR-ACTIVE-SW      PIC X(01)  VALUE 'Y'.
               88  WS-INSTRUCTIONS-ON            VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * STREET ACCOUNTS AND PLACE OF SETTLEMENT BY DEPOSITORY          *
      *----------------------------------------------------------------*
       01  WS-DEPOSITORY-VALUES.
           05  FILLER  PIC X(25)  VALUE 'DTC STREETDTC0DTCYUS33XXX'.
           05  FILLER  PIC X(25)  VALUE 'FED STREETFED0FRNYUS33XXX'.
           05  FILLER  PIC X(25)  VALUE 'EUCLSTREETEUC0MGTCBEBEXXX'.
           05  FILLER  PIC X(25)  VALUE 'BOX STREETDTC0DTCYUS33XXX'.
           05  FILLER  PIC X(25)  VALUE 'SEG STREETDTC0DTCYUS33XXX'.
       01  WS-DEPOSITORY-TABLE REDEFINES WS-DEPOSITORY-VALUES.
           05  WS-DEP-ENTRY            OCCURS 5 TIMES
                                       INDEXED BY WS-DEP-IDX.
               10  WS-DEP-LOCATION     PIC X(04).
               10  WS-DEP-STREET-ACCT  PIC X(10).
               10  WS-DEP-PLACE-BIC    PIC X(11).
       01  WS-FIRM-BIC                 PIC X(11)  VALUE 'MRDNUS33XXX'.
      *
       01  WS-COUNTERS.
           05  WS-TRADES-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LEGS-WRITTEN         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-WRITTEN        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-DVP            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-CONTRA         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-CANCEL         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REVERSALS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-MISSING         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ISIN-MISSING         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PENDING-LEGS         PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-IN-NET-TOTAL         PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-IN-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-LEG-CASH-TOTAL       PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-LEG-QTY-ABS-HASH     PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-INSTR-AMT-TOTAL      PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-ACT-TYPE             PIC X(03).
           05  WS-QTY-SIGNED           PIC S9(11)V9(04) COMP-3.
           05  WS-CASH-SIGNED          PIC S9(15)V99    COMP-3.
           05  WS-COST-SIGNED          PIC S9(15)V99    COMP-3.
           05  WS-QTY-ABS              PIC S9(11)V9(04) COMP-3.
           05  WS-STREET-ACCT          PIC X(10).
           05  WS-PLACE-BIC            PIC X(11).
           05  WS-SETTLE-FLAG          PIC X(01).
           05  WS-LAST-CUSIP           PIC X(09)  VALUE LOW-VALUES.
           05  WS-LAST-ISIN            PIC X(12)  VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-DISP-QTY             PIC Z(9)9.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
           05  WS-PARM-KEYWORD         PIC X(30).
           05  WS-PARM-VALUE           PIC X(30).
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
           PERFORM 2000-PROCESS-TRADE  THRU 2000-EXIT
               UNTIL WS-END-OF-TRADES
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
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
           MOVE 'TRADE EXTRACTS STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM  THRU 1100-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
           DISPLAY 'TCB500 - SETTLEMENT INSTRUCTIONS '
                   WS-INSTR-ACTIVE-SW
      *
           OPEN INPUT TRADEIN-FILE
           IF NOT TRADEIN-OK
               MOVE 'TRADEIN'          TO AB-DDNAME
               MOVE WS-TRADEIN-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN INPUT ACCTMAST-FILE
           IF NOT ACCTMAST-OK
               MOVE 'ACCTMAST'         TO AB-DDNAME
               MOVE WS-ACCTMAST-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT ACTVOUT-FILE
           IF NOT ACTVOUT-OK
               MOVE 'ACTVOUT'          TO AB-DDNAME
               MOVE WS-ACTVOUT-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT SETLOUT-FILE
           IF NOT SETLOUT-OK
               MOVE 'SETLOUT'          TO AB-DDNAME
               MOVE WS-SETLOUT-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARM.
           READ PARMCARD
               AT END
                   SET WS-PARM-EOF     TO TRUE
                   GO TO 1100-EXIT
           END-READ
           IF PARM-CARD-REC(1:1) = '*'
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           IF WS-PARM-KEYWORD = 'SETTLEMENT-INSTRUCTIONS'
               MOVE WS-PARM-VALUE(1:1) TO WS-INSTR-ACTIVE-SW
           END-IF
           IF WS-PARM-KEYWORD = 'FIRM-BIC'
               MOVE WS-PARM-VALUE(1:11) TO WS-FIRM-BIC
           END-IF.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - ONE FINAL TRADE                                         *
      *================================================================*
       2000-PROCESS-TRADE.
           ADD 1                       TO WS-TRADES-READ
           ADD TRD-NET-AMOUNT          TO WS-IN-NET-TOTAL
           ADD TRD-QTY                 TO WS-IN-QTY-HASH
      *
           PERFORM 2100-READ-ACCOUNT   THRU 2100-EXIT
           PERFORM 2200-DERIVE-ACTIVITY THRU 2200-EXIT
           PERFORM 2300-FIND-STREET-ACCT THRU 2300-EXIT
      *
           PERFORM 3000-OWNERSHIP-LEG  THRU 3000-EXIT
           PERFORM 3100-LOCATION-LEG   THRU 3100-EXIT
      *
           IF WS-INSTRUCTIONS-ON
               PERFORM 4000-CHECK-INSTRUCTION THRU 4000-EXIT
               IF WS-INSTRUCTION-NEEDED
                   PERFORM 4100-BUILD-INSTRUCTION THRU 4100-EXIT
               END-IF
           END-IF
      *
           PERFORM 8000-READ-TRADE     THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-READ-ACCOUNT.
           MOVE TRD-ACCT-NO            TO ACCT-NO
           READ ACCTMAST-FILE
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   SET WS-ACCT-FOUND   TO TRUE
               WHEN ACCTMAST-NOTFND
                   MOVE 'N'            TO WS-ACCT-FOUND-SW
                   ADD 1               TO WS-ACCT-MISSING
                   DISPLAY 'TCB500 - ACCOUNT NOT ON MASTER '
                           TRD-ACCT-NO ' TRADE ' TRD-ID
                           ' - NO INSTRUCTION'
                   INITIALIZE ACCT-MASTER-REC
               WHEN OTHER
                   MOVE 'ACCTMAST'     TO AB-DDNAME
                   MOVE WS-ACCTMAST-FS TO AB-FILE-STATUS
                   MOVE TRD-ACCT-NO    TO AB-KEY
                   MOVE '2100-READ-ACCOUNT' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2200 - ACTIVITY TYPE AND SIGNS FOR THE OWNERSHIP LEG           *
      *----------------------------------------------------------------*
       2200-DERIVE-ACTIVITY.
           EVALUATE TRUE
               WHEN TRD-CANCEL AND TRD-BUY-SIDE
                   MOVE 'XBY'          TO WS-ACT-TYPE
               WHEN TRD-CANCEL
                   MOVE 'XSL'          TO WS-ACT-TYPE
               WHEN TRD-BUY
                   MOVE 'BUY'          TO WS-ACT-TYPE
               WHEN TRD-BUY-COVER
                   MOVE 'BCV'          TO WS-ACT-TYPE
               WHEN TRD-SELL
                   MOVE 'SEL'          TO WS-ACT-TYPE
               WHEN TRD-SELL-SHORT
                   MOVE 'SSL'          TO WS-ACT-TYPE
               WHEN OTHER
                   MOVE 1008           TO AB-ABEND-CODE
                   MOVE '2200-DERIVE-ACTIVITY' TO AB-PARAGRAPH
                   MOVE TRD-ID         TO AB-KEY
                   STRING 'INVALID SIDE ON FINAL TRADE [' TRD-SIDE ']'
                          DELIMITED BY SIZE INTO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE
      *
           IF TRD-BUY-SIDE
               MOVE TRD-QTY            TO WS-QTY-SIGNED
               COMPUTE WS-CASH-SIGNED = ZERO - TRD-NET-AMOUNT
               MOVE TRD-NET-AMOUNT     TO WS-COST-SIGNED
           ELSE
               COMPUTE WS-QTY-SIGNED = ZERO - TRD-QTY
               MOVE TRD-NET-AMOUNT     TO WS-CASH-SIGNED
               MOVE ZERO               TO WS-COST-SIGNED
           END-IF
           IF TRD-CANCEL
               COMPUTE WS-QTY-SIGNED  = ZERO - WS-QTY-SIGNED
               COMPUTE WS-CASH-SIGNED = ZERO - WS-CASH-SIGNED
               COMPUTE WS-COST-SIGNED = ZERO - WS-COST-SIGNED
               ADD 1                   TO WS-REVERSALS
           END-IF
      *
           IF TRD-SETTLE-DATE > DC-BUS-DATE
               MOVE 'P'                TO WS-SETTLE-FLAG
           ELSE
               MOVE 'S'                TO WS-SETTLE-FLAG
           END-IF
           IF WS-QTY-SIGNED = ZERO
               MOVE 'N'                TO WS-SETTLE-FLAG
           END-IF.
       2200-EXIT.
           EXIT.
      *
       2300-FIND-STREET-ACCT.
           SET WS-DEP-IDX              TO 1
           SEARCH WS-DEP-ENTRY
               AT END
                   MOVE 'STREETDTC0'   TO WS-STREET-ACCT
                   MOVE 'DTCYUS33XXX'  TO WS-PLACE-BIC
               WHEN WS-DEP-LOCATION (WS-DEP-IDX) = TRD-SETTLE-LOC
                   MOVE WS-DEP-STREET-ACCT (WS-DEP-IDX)
                                       TO WS-STREET-ACCT
                   MOVE WS-DEP-PLACE-BIC (WS-DEP-IDX)
                                       TO WS-PLACE-BIC
           END-SEARCH.
       2300-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - LEG 1  OWNERSHIP                                        *
      *================================================================*
       3000-OWNERSHIP-LEG.
           INITIALIZE ACT-ACTIVITY-REC
           MOVE 'TC'                   TO ACT-SOURCE
           MOVE TRD-ID                 TO ACT-REF
           MOVE 1                      TO ACT-LEG-NO
           MOVE TRD-ACCT-NO            TO ACT-ACCT-NO
           MOVE TRD-CUSIP              TO ACT-CUSIP
           MOVE TRD-SETTLE-LOC         TO ACT-LOCATION
           MOVE WS-ACT-TYPE            TO ACT-TYPE
           MOVE WS-QTY-SIGNED          TO ACT-QTY-CHANGE
           MOVE WS-CASH-SIGNED         TO ACT-CASH-CHANGE
           MOVE WS-COST-SIGNED         TO ACT-COST-CHANGE
           PERFORM 3500-COMMON-FIELDS  THRU 3500-EXIT
           IF WS-ACCT-FOUND
               MOVE ACCT-TYPE          TO ACT-ACCT-TYPE
           ELSE
               MOVE TRD-ACCT-TYPE      TO ACT-ACCT-TYPE
           END-IF
           PERFORM 7000-WRITE-ACTIVITY THRU 7000-EXIT.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 3100 - LEG 2  STREET LOCATION                                  *
      *================================================================*
       3100-LOCATION-LEG.
           INITIALIZE ACT-ACTIVITY-REC
           MOVE 'TC'                   TO ACT-SOURCE
           MOVE TRD-ID                 TO ACT-REF
           MOVE 2                      TO ACT-LEG-NO
           MOVE WS-STREET-ACCT         TO ACT-ACCT-NO
           MOVE TRD-CUSIP              TO ACT-CUSIP
           MOVE TRD-SETTLE-LOC         TO ACT-LOCATION
           MOVE WS-ACT-TYPE            TO ACT-TYPE
           COMPUTE ACT-QTY-CHANGE = ZERO - WS-QTY-SIGNED
           MOVE ZERO                   TO ACT-CASH-CHANGE
                                          ACT-COST-CHANGE
           PERFORM 3500-COMMON-FIELDS  THRU 3500-EXIT
           MOVE 'ST'                   TO ACT-ACCT-TYPE
           PERFORM 7000-WRITE-ACTIVITY THRU 7000-EXIT.
       3100-EXIT.
           EXIT.
      *
       3500-COMMON-FIELDS.
           MOVE TRD-PRICE              TO ACT-PRICE
           MOVE TRD-CCY                TO ACT-CCY
           MOVE TRD-TRADE-DATE         TO ACT-TRADE-DATE
           MOVE TRD-SETTLE-DATE        TO ACT-SETTLE-DATE
           MOVE TRD-TRADE-DATE         TO ACT-EFFECTIVE-DATE
           STRING 'T' WS-ACT-TYPE DELIMITED BY SIZE
                  INTO ACT-GL-TXN-CODE
           MOVE TRD-SEC-TYPE           TO ACT-SEC-TYPE
           MOVE WS-SETTLE-FLAG         TO ACT-SETTLE-FLAG
           MOVE DC-BUS-DATE            TO ACT-BUS-DATE
           MOVE TRD-QTY                TO WS-DISP-QTY
           MOVE SPACES                 TO ACT-DESC
           STRING WS-ACT-TYPE ' ' WS-DISP-QTY ' ' TRD-SYMBOL
                  ' ' TRD-SOURCE
                  DELIMITED BY SIZE INTO ACT-DESC.
       3500-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - IS A SETTLEMENT INSTRUCTION REQUIRED?                   *
      *   DVP / RVP ACCOUNTS: ALWAYS.                                  *
      *   HOUSE ACCOUNTS (FIRM INVENTORY, STREET) TRADING AGAINST A    *
      *   STREET CONTRA BROKER: THE FIRM INSTRUCTS ITS OWN SIDE.       *
      *   FIRM ACCTS SORT LOW - ALPHA PREFIX, CLIENT ACCTS NUMERIC.    *
      *================================================================*
       4000-CHECK-INSTRUCTION.
           MOVE 'N'                    TO WS-INSTR-SW
           IF NOT WS-ACCT-FOUND
               GO TO 4000-EXIT
           END-IF
           IF ACCT-DVP
               SET WS-INSTRUCTION-NEEDED TO TRUE
               ADD 1                   TO WS-INSTR-DVP
               GO TO 4000-EXIT
           END-IF
           IF TRD-CONTRA NOT = SPACES
               IF TRD-ACCT-NO(1:1) < '0'
                   SET WS-INSTRUCTION-NEEDED TO TRUE
                   ADD 1               TO WS-INSTR-CONTRA
               END-IF
           END-IF.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
      * 4100 - BUILD AND WRITE THE INSTRUCTION                         *
      *================================================================*
       4100-BUILD-INSTRUCTION.
           INITIALIZE SI-INSTRUCTION-REC
           MOVE TRD-ID                 TO SI-TRADE-ID
           IF TRD-BUY-SIDE
               MOVE 'MT541'            TO SI-MSG-TYPE
           ELSE
               MOVE 'MT543'            TO SI-MSG-TYPE
           END-IF
           IF TRD-CANCEL
               SET SI-CANCEL-INSTR     TO TRUE
               ADD 1                   TO WS-INSTR-CANCEL
           ELSE
               SET SI-NEW-INSTR        TO TRUE
           END-IF
           MOVE TRD-ACCT-NO            TO SI-ACCT-NO
           MOVE TRD-CUSIP              TO SI-CUSIP
           PERFORM 4200-GET-ISIN       THRU 4200-EXIT
           MOVE WS-LAST-ISIN           TO SI-ISIN
           MOVE TRD-QTY                TO WS-QTY-ABS
           IF WS-QTY-ABS < ZERO
               COMPUTE WS-QTY-ABS = ZERO - WS-QTY-ABS
           END-IF
           MOVE WS-QTY-ABS             TO SI-QTY
           IF TRD-NET-AMOUNT < ZERO
               COMPUTE SI-SETTLE-AMOUNT = ZERO - TRD-NET-AMOUNT
           ELSE
               MOVE TRD-NET-AMOUNT     TO SI-SETTLE-AMOUNT
           END-IF
           MOVE TRD-CCY                TO SI-CCY
           MOVE TRD-TRADE-DATE         TO SI-TRADE-DATE
           MOVE TRD-SETTLE-DATE        TO SI-SETTLE-DATE
           MOVE TRD-SETTLE-LOC         TO SI-DEPOSITORY
           IF TRD-CONTRA NOT = SPACES
               MOVE TRD-CONTRA         TO SI-CONTRA
           ELSE
               MOVE ACCT-DTC-PARTICIPANT TO SI-CONTRA
           END-IF
           MOVE WS-PLACE-BIC           TO SI-PLACE-BIC
           EVALUATE TRUE
               WHEN ACCT-INSTITUTIONAL
               WHEN ACCT-OMNIBUS
                   IF ACCT-INST-BIC = SPACES
                       MOVE WS-FIRM-BIC TO SI-AGENT-BIC
                   ELSE
                       MOVE ACCT-INST-BIC TO SI-AGENT-BIC
                   END-IF
                   IF ACCT-INST-CUST-ACCT = SPACES
                       MOVE TRD-ACCT-NO TO SI-SAFEKEEPING-ACCT
                   ELSE
                       MOVE ACCT-INST-CUST-ACCT
                                       TO SI-SAFEKEEPING-ACCT
                   END-IF
               WHEN OTHER
                   MOVE WS-FIRM-BIC    TO SI-AGENT-BIC
                   MOVE TRD-ACCT-NO    TO SI-SAFEKEEPING-ACCT
           END-EVALUATE
           WRITE SI-INSTRUCTION-REC
           IF NOT SETLOUT-OK
               MOVE 'SETLOUT'          TO AB-DDNAME
               MOVE WS-SETLOUT-FS      TO AB-FILE-STATUS
               MOVE TRD-ID             TO AB-KEY
               MOVE '4100-BUILD-INSTRUCTION' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-INSTR-WRITTEN
           ADD SI-SETTLE-AMOUNT        TO WS-INSTR-AMT-TOTAL.
       4100-EXIT.
           EXIT.
      *
      *    ISIN FROM THE SECURITY MASTER - SAVED FOR REPEAT CUSIPS
       4200-GET-ISIN.
           IF TRD-CUSIP = WS-LAST-CUSIP
               GO TO 4200-EXIT
           END-IF
           MOVE TRD-CUSIP              TO WS-LAST-CUSIP
           MOVE 'GET '                 TO SL-FUNCTION
           MOVE TRD-CUSIP              TO SL-KEY-CUSIP
           MOVE SPACES                 TO SL-KEY-ISIN SL-KEY-SYMBOL
           CALL 'CMD010' USING SL-SECURITY-PARMS
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA    TO SEC-MASTER-REC
                   MOVE SEC-ISIN       TO WS-LAST-ISIN
               WHEN SL-NOT-FOUND
                   MOVE SPACES         TO WS-LAST-ISIN
                   ADD 1               TO WS-ISIN-MISSING
                   DISPLAY 'TCB500 - NO ISIN FOR CUSIP ' TRD-CUSIP
               WHEN OTHER
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE TRD-CUSIP      TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '4200-GET-ISIN' TO AB-PARAGRAPH
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
           END-EVALUATE.
       4200-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - WRITE ACTIVITY LEG                                      *
      *================================================================*
       7000-WRITE-ACTIVITY.
           WRITE ACT-ACTIVITY-REC
           IF NOT ACTVOUT-OK
               MOVE 'ACTVOUT'          TO AB-DDNAME
               MOVE WS-ACTVOUT-FS      TO AB-FILE-STATUS
               MOVE ACT-REF            TO AB-KEY
               MOVE '7000-WRITE-ACTIVITY' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-LEGS-WRITTEN
           ADD ACT-CASH-CHANGE         TO WS-LEG-CASH-TOTAL
           IF ACT-QTY-CHANGE < ZERO
               SUBTRACT ACT-QTY-CHANGE FROM WS-LEG-QTY-ABS-HASH
           ELSE
               ADD ACT-QTY-CHANGE      TO WS-LEG-QTY-ABS-HASH
           END-IF
           IF ACT-SETTLES-LATER
               ADD 1                   TO WS-PENDING-LEGS
           END-IF.
       7000-EXIT.
           EXIT.
      *
       8000-READ-TRADE.
           READ TRADEIN-FILE
           EVALUATE TRUE
               WHEN TRADEIN-OK
                   CONTINUE
               WHEN TRADEIN-EOF
                   SET WS-END-OF-TRADES TO TRUE
               WHEN OTHER
                   MOVE 'TRADEIN'      TO AB-DDNAME
                   MOVE WS-TRADEIN-FS  TO AB-FILE-STATUS
                   MOVE '8000-READ-TRADE' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE TRADEIN-FILE
                 ACCTMAST-FILE
                 ACTVOUT-FILE
                 SETLOUT-FILE
           IF NOT TRADEIN-OK OR NOT ACCTMAST-OK
           OR NOT ACTVOUT-OK OR NOT SETLOUT-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-TRADEIN-FS ' '
                      WS-ACCTMAST-FS ' ' WS-ACTVOUT-FS ' '
                      WS-SETLOUT-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           IF WS-ACCT-MISSING > ZERO
           OR WS-ISIN-MISSING > ZERO
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'TRADES-IN'            TO CT-COUNTER-NAME
           MOVE WS-TRADES-READ         TO CT-COUNT
           MOVE WS-IN-NET-TOTAL        TO CT-AMOUNT
           MOVE WS-IN-QTY-HASH         TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'SRACTV-OUT'           TO CT-COUNTER-NAME
           MOVE WS-LEGS-WRITTEN        TO CT-COUNT
           MOVE WS-LEG-CASH-TOTAL      TO CT-AMOUNT
           MOVE WS-LEG-QTY-ABS-HASH    TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'SETLINST-OUT'         TO CT-COUNTER-NAME
           MOVE WS-INSTR-WRITTEN       TO CT-COUNT
           MOVE WS-INSTR-AMT-TOTAL     TO CT-AMOUNT
           MOVE ZERO                   TO CT-QTY-HASH
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
           MOVE WS-LEGS-WRITTEN        TO WS-DISP-COUNT
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'TRADE EXTRACTS ENDED. SRACTV LEGS ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* TCB500 - TRADE EXTRACT STATISTICS  ' DC-BUS-DATE
           DISPLAY '*************************************************'
           MOVE WS-TRADES-READ         TO WS-DISP-COUNT
           DISPLAY '* FINAL TRADES READ         : ' WS-DISP-COUNT
           MOVE WS-REVERSALS           TO WS-DISP-COUNT
           DISPLAY '*   OF WHICH CANCELS        : ' WS-DISP-COUNT
           MOVE WS-LEGS-WRITTEN        TO WS-DISP-COUNT
           DISPLAY '* SRACTV LEGS WRITTEN       : ' WS-DISP-COUNT
           MOVE WS-PENDING-LEGS        TO WS-DISP-COUNT
           DISPLAY '*   SETTLING LATER          : ' WS-DISP-COUNT
           MOVE WS-INSTR-WRITTEN       TO WS-DISP-COUNT
           DISPLAY '* INSTRUCTIONS WRITTEN      : ' WS-DISP-COUNT
           MOVE WS-INSTR-DVP           TO WS-DISP-COUNT
           DISPLAY '*   DVP ACCOUNTS            : ' WS-DISP-COUNT
           MOVE WS-INSTR-CONTRA        TO WS-DISP-COUNT
           DISPLAY '*   STREET CONTRA           : ' WS-DISP-COUNT
           MOVE WS-INSTR-CANCEL        TO WS-DISP-COUNT
           DISPLAY '*   CANCELLATIONS (CANC)    : ' WS-DISP-COUNT
           MOVE WS-ACCT-MISSING        TO WS-DISP-COUNT
           DISPLAY '* ACCOUNTS NOT FOUND        : ' WS-DISP-COUNT
           MOVE WS-ISIN-MISSING        TO WS-DISP-COUNT
           DISPLAY '* ISIN NOT FOUND            : ' WS-DISP-COUNT
           MOVE WS-LEG-CASH-TOTAL      TO WS-DISP-AMT
           DISPLAY '* NET CASH ON LEGS          : ' WS-DISP-AMT
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
           DISPLAY 'TCB500 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'TCB500 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
