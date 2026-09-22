       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCU22E.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  08/20/1990.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCU22E                                            *
      * TITLE      : REGULATORY FEES - EQUITY-LIKE SECURITIES          *
      * TYPE       : CALLABLE UTILITY (NO FILES)                       *
      *                                                                *
      * CALLED BY  : TCB200 (DYNAMICALLY, NAME BUILT AT RUN TIME)      *
      * LINKAGE    : TCFEELNK  (FE-FEE-PARMS)                          *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   SECTION 31 FEE ("SEC FEE") ON SALES OF EQUITY, PREFERRED     *
      *   AND ADR.  RATE IS DOLLARS PER MILLION OF PRINCIPAL, TAKEN    *
      *   FROM THE EFFECTIVE-DATED TABLE BELOW BY TRADE DATE.  THE FEE *
      *   IS ALWAYS ROUNDED UP TO THE NEXT WHOLE CENT (SEC RULE).      *
      *                                                                *
      *   TRADING ACTIVITY FEE ("TAF") ON SHARES SOLD, PER SHARE RATE  *
      *   WITH A PER TRADE MAXIMUM, ROUNDED TO THE NEAREST CENT.       *
      *                                                                *
      *   MUTUAL FUNDS AND ANY OTHER TYPE: NO FEES, RC 04.             *
      *                                                                *
      * RATE MAINTENANCE: OPERATIONS RAISES A CHG TICKET WHEN THE SEC  *
      *   ANNOUNCES A NEW RATE.  ADD A ROW AT THE END OF THE TABLE AND *
      *   BUMP WS-SEC-RATE-COUNT.  ROWS MUST BE IN DATE ORDER.         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1990-08-20 RJK            ORIGINAL (AS TCU220) - 1/300 OF 1%   *
      * 1998-10-19 TLM  CHG04471  Y2K - TRADE DATE CCYYMMDD            *
      * 2002-03-08 KAP  CHG09930  TAF FEE ADDED                        *
      * 2002-10-01 KAP  CHG10412  RATE TABLE BY EFFECTIVE DATE         *
      * 2011-11-14 SPA  CHG22418  RENAMED TCU22E, FI SPLIT TO TCU22F   *
      * 2012-01-03 SPA  CHG22560  TAF RATE/MAX TABLE                   *
      * 2019-05-13 NVR  CHG35902  SEC RATE 2019-05-16                  *
      * 2020-02-14 NVR  CHG37015  SEC RATE 2020-02-18                  *
      * 2021-02-22 NVR  CHG38200  SEC RATE 2021-02-25                  *
      * 2023-02-21 NVR  CHG40115  SEC RATE 2023-02-27                  *
      * 2023-05-15 NVR  CHG40388  SEC RATE 2023-05-22                  *
      * 2023-12-18 NVR  CHG40902  TAF RATE 2024-01-01                  *
      * 2024-05-20 NVR  CHG41390  SEC RATE 2024-05-22                  *
      * 2025-05-09 JMH  CHG43011  SEC RATE 2025-05-14 (ZERO)           *
      * 2026-03-25 JMH  CHG44870  SEC RATE 2026-04-01                  *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCU22E'.
       01  WS-CALL-COUNT               PIC S9(09) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * SEC FEE RATE TABLE - DOLLARS PER $1,000,000 OF PRINCIPAL       *
      *   EFFECTIVE DATE (CCYYMMDD)  RATE                              *
      *----------------------------------------------------------------*
       01  WS-SEC-RATE-VALUES.
           05  FILLER  PIC X(15)  VALUE '201905160002070'.
           05  FILLER  PIC X(15)  VALUE '202002180002210'.
           05  FILLER  PIC X(15)  VALUE '202102250000510'.
           05  FILLER  PIC X(15)  VALUE '202302270002290'.
           05  FILLER  PIC X(15)  VALUE '202305220000800'.
           05  FILLER  PIC X(15)  VALUE '202405220002780'.
           05  FILLER  PIC X(15)  VALUE '202505140000000'.
           05  FILLER  PIC X(15)  VALUE '202604010002060'.
           05  FILLER  PIC X(15)  VALUE '999999999999999'.
           05  FILLER  PIC X(15)  VALUE '999999999999999'.
           05  FILLER  PIC X(15)  VALUE '999999999999999'.
           05  FILLER  PIC X(15)  VALUE '999999999999999'.
       01  WS-SEC-RATE-TABLE  REDEFINES WS-SEC-RATE-VALUES.
           05  WS-SEC-RATE-ENTRY       OCCURS 12 TIMES.
               10  WS-SEC-EFF-DATE     PIC 9(08).
               10  WS-SEC-RATE         PIC 9(05)V99.
       01  WS-SEC-RATE-COUNT           PIC S9(04) COMP VALUE +8.
      *
      *----------------------------------------------------------------*
      * TAF TABLE - RATE PER SHARE AND MAXIMUM PER TRADE               *
      *   EFFECTIVE DATE   RATE (V9(6))   MAXIMUM (9(3)V99)            *
      *----------------------------------------------------------------*
       01  WS-TAF-RATE-VALUES.
           05  FILLER  PIC X(19)  VALUE '2012010100011900595'.
           05  FILLER  PIC X(19)  VALUE '2024010100016600830'.
           05  FILLER  PIC X(19)  VALUE '9999999999999999999'.
       01  WS-TAF-RATE-TABLE  REDEFINES WS-TAF-RATE-VALUES.
           05  WS-TAF-RATE-ENTRY       OCCURS 3 TIMES.
               10  WS-TAF-EFF-DATE     PIC 9(08).
               10  WS-TAF-RATE         PIC V9(06).
               10  WS-TAF-MAXIMUM      PIC 9(03)V99.
       01  WS-TAF-RATE-COUNT           PIC S9(04) COMP VALUE +2.
      *
      *----------------------------------------------------------------*
      * WORK FIELDS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-IDX                  PIC S9(04) COMP.
           05  WS-FOUND-IDX            PIC S9(04) COMP.
           05  WS-RATE-APPLIED         PIC S9(05)V99    COMP-3.
           05  WS-FEE-WORK             PIC S9(09)V9(07) COMP-3.
           05  WS-TAF-WORK             PIC S9(09)V99    COMP-3.
           05  WS-TAF-RATE-APPLIED     PIC SV9(06)      COMP-3.
           05  WS-TAF-MAX-APPLIED      PIC S9(03)V99    COMP-3.
           05  WS-MILLION              PIC S9(07)       COMP-3
                                                  VALUE +1000000.
      *    ROUND-UP CONSTANT - ONE TENTH OF A MILLS SHORT OF A CENT
           05  WS-ROUND-UP             PIC SV9(07)      COMP-3
                                                  VALUE +.0099999.
       01  WS-SWITCHES.
           05  WS-SELL-SW              PIC X(01).
               88  WS-IS-SALE                    VALUE 'Y'.
               88  WS-NOT-SALE                   VALUE 'N'.
           05  WS-TYPE-SW              PIC X(01).
               88  WS-FEE-ELIGIBLE-TYPE          VALUE 'Y'.
               88  WS-NOT-ELIGIBLE-TYPE          VALUE 'N'.
      *    1990 FORMULA WAS 1/300 OF 1 PERCENT - SWITCH KEPT FOR THE
      *    AUDIT REPRODUCTION JOB WHICH NO LONGER EXISTS.
           05  WS-OLD-FORMULA-SW       PIC X(01)  VALUE 'N'.
               88  WS-USE-OLD-FORMULA            VALUE 'Y'.
      *
       LINKAGE SECTION.
       COPY TCFEELNK.
      *
       PROCEDURE DIVISION USING FE-FEE-PARMS.
      *
       0000-MAINLINE.
           ADD 1                       TO WS-CALL-COUNT
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           IF FE-INVALID-INPUT
               GOBACK
           END-IF
           IF WS-NOT-ELIGIBLE-TYPE
               MOVE 04                 TO FE-RETURN-CODE
               MOVE 'NOT AN EQUITY-LIKE SECURITY - NO FEES'
                                       TO FE-MESSAGE
               GOBACK
           END-IF
           IF WS-IS-SALE
               PERFORM 2000-SEC-FEE    THRU 2000-EXIT
               PERFORM 3000-TAF-FEE    THRU 3000-EXIT
           ELSE
               MOVE 'PURCHASE - NO SEC FEE / TAF'
                                       TO FE-MESSAGE
           END-IF
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - EDIT INPUT, CLASSIFY TRADE                              *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           MOVE ZERO                   TO FE-SEC-FEE
                                          FE-TAF-FEE
                                          FE-OTHER-FEES
                                          FE-SEC-RATE
                                          FE-RETURN-CODE
           MOVE SPACES                 TO FE-MESSAGE
           IF FE-PRINCIPAL NOT NUMERIC
           OR FE-QTY NOT NUMERIC
           OR FE-TRADE-DATE NOT NUMERIC
               MOVE 08                 TO FE-RETURN-CODE
               MOVE 'NON-NUMERIC INPUT TO TCU22E'
                                       TO FE-MESSAGE
               GO TO 1000-EXIT
           END-IF
           IF FE-TRADE-DATE < WS-SEC-EFF-DATE (1)
               MOVE 08                 TO FE-RETURN-CODE
               MOVE 'TRADE DATE BEFORE FIRST RATE IN TABLE'
                                       TO FE-MESSAGE
               GO TO 1000-EXIT
           END-IF
           IF FE-SIDE = 'S ' OR 'SS'
               SET WS-IS-SALE          TO TRUE
           ELSE
               SET WS-NOT-SALE         TO TRUE
           END-IF
           IF FE-SEC-TYPE = 'EQ' OR 'PF' OR 'AD'
               SET WS-FEE-ELIGIBLE-TYPE TO TRUE
           ELSE
               SET WS-NOT-ELIGIBLE-TYPE TO TRUE
           END-IF.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2000 - SEC FEE                                                 *
      *   FIND THE LAST TABLE ROW WITH EFFECTIVE DATE <= TRADE DATE.   *
      *   FEE = PRINCIPAL * RATE / 1,000,000  ROUNDED UP TO THE CENT.  *
      *----------------------------------------------------------------*
       2000-SEC-FEE.
           MOVE ZERO                   TO WS-FOUND-IDX
           PERFORM 2100-FIND-SEC-RATE  THRU 2100-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SEC-RATE-COUNT
           IF WS-FOUND-IDX = ZERO
               MOVE 08                 TO FE-RETURN-CODE
               MOVE 'NO SEC FEE RATE FOR TRADE DATE'
                                       TO FE-MESSAGE
               GO TO 2000-EXIT
           END-IF
           MOVE WS-SEC-RATE (WS-FOUND-IDX) TO WS-RATE-APPLIED
           MOVE WS-RATE-APPLIED        TO FE-SEC-RATE
           IF WS-RATE-APPLIED = ZERO
               MOVE ZERO               TO FE-SEC-FEE
               MOVE 'SEC FEE RATE ZERO FOR PERIOD'
                                       TO FE-MESSAGE
               GO TO 2000-EXIT
           END-IF
           IF WS-USE-OLD-FORMULA
               COMPUTE WS-FEE-WORK = FE-PRINCIPAL / 30000
           ELSE
               COMPUTE WS-FEE-WORK =
                       FE-PRINCIPAL * WS-RATE-APPLIED / WS-MILLION
           END-IF
           IF WS-FEE-WORK > ZERO
               ADD WS-ROUND-UP         TO WS-FEE-WORK
           END-IF
           MOVE WS-FEE-WORK            TO FE-SEC-FEE.
       2000-EXIT.
           EXIT.
      *
       2100-FIND-SEC-RATE.
           IF WS-SEC-EFF-DATE (WS-IDX) NOT > FE-TRADE-DATE
               MOVE WS-IDX             TO WS-FOUND-IDX
           END-IF.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3000 - TRADING ACTIVITY FEE                                    *
      *   TAF = SHARES SOLD * RATE, ROUNDED TO THE NEAREST CENT,       *
      *   CAPPED AT THE MAXIMUM PER TRADE.                             *
      *----------------------------------------------------------------*
       3000-TAF-FEE.
           MOVE ZERO                   TO WS-FOUND-IDX
           PERFORM 3100-FIND-TAF-RATE  THRU 3100-EXIT
               VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TAF-RATE-COUNT
           IF WS-FOUND-IDX = ZERO
               MOVE ZERO               TO FE-TAF-FEE
               GO TO 3000-EXIT
           END-IF
           MOVE WS-TAF-RATE (WS-FOUND-IDX)    TO WS-TAF-RATE-APPLIED
           MOVE WS-TAF-MAXIMUM (WS-FOUND-IDX) TO WS-TAF-MAX-APPLIED
           COMPUTE WS-TAF-WORK ROUNDED = FE-QTY * WS-TAF-RATE-APPLIED
           IF WS-TAF-WORK > WS-TAF-MAX-APPLIED
               MOVE WS-TAF-MAX-APPLIED TO WS-TAF-WORK
           END-IF
           MOVE WS-TAF-WORK            TO FE-TAF-FEE.
       3000-EXIT.
           EXIT.
      *
       3100-FIND-TAF-RATE.
           IF WS-TAF-EFF-DATE (WS-IDX) NOT > FE-TRADE-DATE
               MOVE WS-IDX             TO WS-FOUND-IDX
           END-IF.
       3100-EXIT.
           EXIT.
