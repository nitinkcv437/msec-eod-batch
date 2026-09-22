       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCU210.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  06/05/1989.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCU210                                            *
      * TITLE      : COMMISSION CALCULATOR                             *
      * TYPE       : CALLABLE UTILITY (NO FILES)                       *
      *                                                                *
      * CALLED BY  : TCB200 (TRADE ENRICHMENT)                         *
      * LINKAGE    : TCCOMLNK  (CO-COMMISSION-PARMS)                   *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   COMPUTES THE COMMISSION FOR ONE TRADE FROM THE ACCOUNT'S     *
      *   COMMISSION SCHEDULE CODE.                                    *
      *     01  RETAIL TIERED ON PRINCIPAL                             *
      *           2.50% FIRST 10,000  1.75% NEXT 40,000                *
      *           1.00% NEXT 200,000  0.50% ABOVE                      *
      *           MINIMUM $25.00  MAXIMUM $2,500.00 PER TRADE          *
      *     02  PER SHARE $0.02, MINIMUM $10.00                        *
      *     03  INSTITUTIONAL NEGOTIATED RATE (BPS OF PRINCIPAL)       *
      *     04  ZERO COMMISSION (WRAP / FEE BASED)                     *
      *     05  1995 SCHEDULE - TICKET CHARGE + SHARE TIERS            *
      *   FIXED INCOME: MARKUP IS IN THE PRICE - NO COMMISSION.        *
      *   PRINCIPAL CAPACITY TRADES - NO COMMISSION.                   *
      *                                                                *
      * RETURN CODES (CO-RETURN-CODE):                                 *
      *   00 OK   04 UNKNOWN SCHEDULE, 01 APPLIED   08 BAD INPUT       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1989-06-05 RJK            ORIGINAL - SCHEDULES 01 02           *
      * 1990-01-22 RJK            MINIMUM COMMISSION $20.00            *
      * 1992-07-13 DWB  CHG00412  SCHEDULE 03 INSTITUTIONAL            *
      * 1995-03-13 DWB  CHG01702  SCHEDULE 05 (NEW PRICING 1995)       *
      * 1996-11-04 DWB  CHG02690  SCHEDULE 01 TIERS REVISED, 05 FROZEN *
      * 1998-10-19 TLM  CHG04471  Y2K - TRADE DATE CCYYMMDD            *
      * 2001-04-09 KAP  CHG08814  DECIMALIZATION - 8 DEC PRICE         *
      * 2003-09-29 KAP  CHG11544  MINIMUM $25.00 / MAXIMUM $2,500      *
      * 2008-02-11 SPA  CHG17730  SCHEDULE 04 ZERO COMMISSION          *
      * 2012-05-07 SPA  CHG23112  PRINCIPAL CAPACITY - NO COMMISSION   *
      * 2019-10-07 NVR  CHG36640  RETAIL ONLINE ZERO COMMISSION MOVED  *
      *                           TO SCHEDULE 04 - NO CODE CHANGE      *
      * 2024-02-12 NVR  CHG41007  T+1 REVIEW - NO CHANGE REQUIRED      *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCU210'.
       01  WS-CALL-COUNT               PIC S9(09) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * SCHEDULE 01 RETAIL TIERS - PRINCIPAL BANDS AND RATES           *
      * BAND UPPER LIMITS ARE CUMULATIVE.  LAST BAND IS OPEN ENDED.    *
      * REVISED 1996 PER COMPLIANCE MEMO 96-114.  DO NOT CHANGE        *
      * WITHOUT SALES MANAGEMENT SIGN OFF - SEE CHG02690.              *
      *----------------------------------------------------------------*
       01  WS-RETAIL-TIER-VALUES.
           05  FILLER.
               10  FILLER              PIC 9(11)V99 VALUE 10000.00.
               10  FILLER              PIC V9(06)   VALUE .025000.
           05  FILLER.
               10  FILLER              PIC 9(11)V99 VALUE 50000.00.
               10  FILLER              PIC V9(06)   VALUE .017500.
           05  FILLER.
               10  FILLER              PIC 9(11)V99 VALUE 250000.00.
               10  FILLER              PIC V9(06)   VALUE .010000.
           05  FILLER.
               10  FILLER              PIC 9(11)V99
                                       VALUE 99999999999.99.
               10  FILLER              PIC V9(06)   VALUE .005000.
       01  WS-RETAIL-TIER-TABLE  REDEFINES WS-RETAIL-TIER-VALUES.
           05  WS-RTL-TIER             OCCURS 4 TIMES.
               10  WS-RTL-UPPER        PIC 9(11)V99.
               10  WS-RTL-RATE         PIC V9(06).
       01  WS-RTL-TIER-COUNT           PIC S9(04) COMP VALUE +4.
       01  WS-RTL-MINIMUM              PIC S9(07)V99 COMP-3
                                                  VALUE +25.00.
       01  WS-RTL-MAXIMUM              PIC S9(07)V99 COMP-3
                                                  VALUE +2500.00.
      *    OLD 1990 MINIMUM - KEPT FOR REFERENCE
      *01  WS-RTL-MINIMUM-OLD          PIC S9(07)V99 COMP-3
      *                                           VALUE +20.00.
      *
      *----------------------------------------------------------------*
      * SCHEDULE 02 PER SHARE                                          *
      *----------------------------------------------------------------*
       01  WS-PSH-RATE                 PIC SV9(04)   COMP-3
                                                  VALUE +.0200.
       01  WS-PSH-MINIMUM              PIC S9(07)V99 COMP-3
                                                  VALUE +10.00.
      *
      *----------------------------------------------------------------*
      * SCHEDULE 05 - 1995 PRICING.  FROZEN BY CHG02690 (1996) FOR THE *
      * GRANDFATHERED ACCOUNTS ONLY.  NEW ACCOUNTS MAY NOT BE OPENED   *
      * ON SCHEDULE 05.                                                *
      *----------------------------------------------------------------*
       01  WS-LGY-TICKET-CHARGE        PIC S9(05)V99 COMP-3
                                                  VALUE +30.00.
       01  WS-LGY-SHARE-TIER-VALUES.
           05  FILLER.
               10  FILLER              PIC 9(11)    VALUE 1000.
               10  FILLER              PIC V9(04)   VALUE .0500.
           05  FILLER.
               10  FILLER              PIC 9(11)    VALUE 5000.
               10  FILLER              PIC V9(04)   VALUE .0300.
           05  FILLER.
               10  FILLER              PIC 9(11)    VALUE 99999999999.
               10  FILLER              PIC V9(04)   VALUE .0150.
       01  WS-LGY-SHARE-TIER-TABLE REDEFINES WS-LGY-SHARE-TIER-VALUES.
           05  WS-LGY-TIER             OCCURS 3 TIMES.
               10  WS-LGY-UPPER        PIC 9(11).
               10  WS-LGY-RATE         PIC V9(04).
       01  WS-LGY-PRIN-THRESHOLD       PIC S9(11)V99 COMP-3
                                                  VALUE +50000.00.
       01  WS-LGY-PRIN-RATE            PIC SV9(06)   COMP-3
                                                  VALUE +.003000.
       01  WS-LGY-ODD-LOT-LIMIT        PIC S9(05)    COMP-3
                                                  VALUE +100.
       01  WS-LGY-ODD-LOT-CHARGE       PIC S9(05)V99 COMP-3
                                                  VALUE +5.00.
       01  WS-LGY-MINIMUM              PIC S9(07)V99 COMP-3
                                                  VALUE +38.50.
       01  WS-LGY-MAXIMUM              PIC S9(07)V99 COMP-3
                                                  VALUE +1850.00.
      *
      *----------------------------------------------------------------*
      * WORK FIELDS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK-FIELDS.
           05  WS-TIER-IDX             PIC S9(04) COMP.
           05  WS-REMAINING            PIC S9(15)V99    COMP-3.
           05  WS-LOWER-BOUND          PIC S9(15)V99    COMP-3.
           05  WS-BAND-WIDTH           PIC S9(15)V99    COMP-3.
           05  WS-BAND-AMOUNT          PIC S9(15)V99    COMP-3.
           05  WS-TIER-COMM            PIC S9(11)V999   COMP-3.
           05  WS-COMM-ACCUM           PIC S9(11)V999   COMP-3.
           05  WS-RATE-FACTOR          PIC SV9(06)      COMP-3.
           05  WS-SHARES-REMAINING     PIC S9(11)V9(04) COMP-3.
           05  WS-SHARE-LOWER          PIC S9(11)V9(04) COMP-3.
           05  WS-SHARE-BAND           PIC S9(11)V9(04) COMP-3.
           05  WS-SHARE-WIDTH          PIC S9(11)V9(04) COMP-3.
           05  WS-PRIN-EXCESS          PIC S9(15)V99    COMP-3.
           05  WS-COMM-RESULT          PIC S9(11)V99    COMP-3.
       01  WS-SWITCHES.
           05  WS-DONE-SW              PIC X(01).
               88  WS-TIERS-DONE                 VALUE 'Y'.
               88  WS-TIERS-NOT-DONE             VALUE 'N'.
      *    MIN/MAX WAIVER FOR EMPLOYEE ACCOUNTS - NEVER IMPLEMENTED
      *    IN THE ACCOUNT MASTER.  LEFT OFF.       DWB 1995
           05  WS-EMPLOYEE-WAIVER-SW   PIC X(01)  VALUE 'N'.
               88  WS-EMPLOYEE-WAIVER            VALUE 'Y'.
      *
       LINKAGE SECTION.
       COPY TCCOMLNK.
      *
       PROCEDURE DIVISION USING CO-COMMISSION-PARMS.
      *
       0000-MAINLINE.
           ADD 1                       TO WS-CALL-COUNT
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           IF CO-INVALID-INPUT
               GO TO 0000-RETURN
           END-IF
           PERFORM 2000-SELECT-SCHEDULE THRU 2000-EXIT
           MOVE WS-COMM-RESULT         TO CO-COMMISSION.
       0000-RETURN.
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - EDIT INPUT                                              *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           MOVE ZERO                   TO CO-COMMISSION
                                          CO-RETURN-CODE
                                          WS-COMM-RESULT
                                          WS-COMM-ACCUM
           MOVE SPACES                 TO CO-RULE-APPLIED
                                          CO-MESSAGE
           IF CO-QTY NOT NUMERIC
           OR CO-PRINCIPAL NOT NUMERIC
               MOVE 08                 TO CO-RETURN-CODE
               MOVE 'NON-NUMERIC QTY OR PRINCIPAL'
                                       TO CO-MESSAGE
               GO TO 1000-EXIT
           END-IF
           IF CO-QTY NOT > ZERO
               MOVE 08                 TO CO-RETURN-CODE
               MOVE 'QUANTITY NOT POSITIVE'
                                       TO CO-MESSAGE
               GO TO 1000-EXIT
           END-IF
           IF CO-PRINCIPAL < ZERO
               MOVE 08                 TO CO-RETURN-CODE
               MOVE 'PRINCIPAL NEGATIVE'
                                       TO CO-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2000 - DECIDE WHICH RULE APPLIES                               *
      *   ORDER MATTERS: FIXED INCOME, THEN CAPACITY, THEN SCHEDULE.   *
      *----------------------------------------------------------------*
       2000-SELECT-SCHEDULE.
           IF CO-SEC-TYPE = 'CB' OR 'MU' OR 'GV'
               MOVE 'FIXI'             TO CO-RULE-APPLIED
               MOVE ZERO               TO WS-COMM-RESULT
               MOVE 'FIXED INCOME - MARKUP IN PRICE'
                                       TO CO-MESSAGE
               GO TO 2000-EXIT
           END-IF
           IF CO-CAPACITY = 'P'
               MOVE 'PRIN'             TO CO-RULE-APPLIED
               MOVE ZERO               TO WS-COMM-RESULT
               MOVE 'PRINCIPAL CAPACITY - NO COMMISSION'
                                       TO CO-MESSAGE
               GO TO 2000-EXIT
           END-IF
           EVALUATE CO-COMM-SCHED
               WHEN '01'
                   PERFORM 3100-SCHED-01-RETAIL THRU 3100-EXIT
               WHEN '02'
                   PERFORM 3200-SCHED-02-PER-SHARE THRU 3200-EXIT
               WHEN '03'
                   PERFORM 3300-SCHED-03-INST THRU 3300-EXIT
               WHEN '04'
                   MOVE 'ZERO'         TO CO-RULE-APPLIED
                   MOVE ZERO           TO WS-COMM-RESULT
               WHEN '05'
                   PERFORM 3500-SCHED-05-LEGACY THRU 3500-EXIT
               WHEN OTHER
                   MOVE 04             TO CO-RETURN-CODE
                   STRING 'SCHEDULE ' CO-COMM-SCHED
                          ' UNKNOWN - 01 USED'
                          DELIMITED BY SIZE INTO CO-MESSAGE
                   PERFORM 3100-SCHED-01-RETAIL THRU 3100-EXIT
           END-EVALUATE.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3100 - SCHEDULE 01 RETAIL TIERED ON PRINCIPAL                  *
      *   EACH BAND IS CHARGED AT ITS OWN RATE.  BAND CHARGES ARE      *
      *   CARRIED TO 3 DECIMALS, TOTAL ROUNDED TO THE CENT, THEN THE   *
      *   MINIMUM / MAXIMUM ARE APPLIED.                               *
      *----------------------------------------------------------------*
       3100-SCHED-01-RETAIL.
           MOVE 'TIER'                 TO CO-RULE-APPLIED
           MOVE ZERO                   TO WS-COMM-ACCUM
                                          WS-LOWER-BOUND
           MOVE CO-PRINCIPAL           TO WS-REMAINING
           SET WS-TIERS-NOT-DONE       TO TRUE
           PERFORM 3110-RETAIL-BAND    THRU 3110-EXIT
               VARYING WS-TIER-IDX FROM 1 BY 1
               UNTIL WS-TIER-IDX > WS-RTL-TIER-COUNT
                  OR WS-TIERS-DONE
           COMPUTE WS-COMM-RESULT ROUNDED = WS-COMM-ACCUM
           IF WS-EMPLOYEE-WAIVER
               GO TO 3100-EXIT
           END-IF
           IF WS-COMM-RESULT < WS-RTL-MINIMUM
               MOVE WS-RTL-MINIMUM     TO WS-COMM-RESULT
               MOVE 'MIN '             TO CO-RULE-APPLIED
           END-IF
           IF WS-COMM-RESULT > WS-RTL-MAXIMUM
               MOVE WS-RTL-MAXIMUM     TO WS-COMM-RESULT
               MOVE 'MAX '             TO CO-RULE-APPLIED
           END-IF.
       3100-EXIT.
           EXIT.
      *
       3110-RETAIL-BAND.
           COMPUTE WS-BAND-WIDTH =
                   WS-RTL-UPPER (WS-TIER-IDX) - WS-LOWER-BOUND
           IF WS-REMAINING > WS-BAND-WIDTH
               MOVE WS-BAND-WIDTH      TO WS-BAND-AMOUNT
           ELSE
               MOVE WS-REMAINING       TO WS-BAND-AMOUNT
               SET WS-TIERS-DONE       TO TRUE
           END-IF
           COMPUTE WS-TIER-COMM =
                   WS-BAND-AMOUNT * WS-RTL-RATE (WS-TIER-IDX)
           ADD WS-TIER-COMM            TO WS-COMM-ACCUM
           SUBTRACT WS-BAND-AMOUNT     FROM WS-REMAINING
           MOVE WS-RTL-UPPER (WS-TIER-IDX)
                                       TO WS-LOWER-BOUND
           IF WS-REMAINING NOT > ZERO
               SET WS-TIERS-DONE       TO TRUE
           END-IF.
       3110-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3200 - SCHEDULE 02 PER SHARE                                   *
      *----------------------------------------------------------------*
       3200-SCHED-02-PER-SHARE.
           MOVE 'PSHR'                 TO CO-RULE-APPLIED
           COMPUTE WS-COMM-RESULT ROUNDED = CO-QTY * WS-PSH-RATE
           IF WS-COMM-RESULT < WS-PSH-MINIMUM
               MOVE WS-PSH-MINIMUM     TO WS-COMM-RESULT
               MOVE 'MIN '             TO CO-RULE-APPLIED
           END-IF.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3300 - SCHEDULE 03 INSTITUTIONAL NEGOTIATED RATE               *
      *   RATE IS HELD ON THE ACCOUNT MASTER IN BASIS POINTS.          *
      *   CONVERT TO A FACTOR FIRST (6 DECIMALS, AS THE OLD RATE CARD) *
      *   THEN APPLY TO PRINCIPAL.  NO MIN / MAX.                      *
      *----------------------------------------------------------------*
       3300-SCHED-03-INST.
           MOVE 'INST'                 TO CO-RULE-APPLIED
           IF CO-INST-RATE-BPS NOT NUMERIC
               MOVE ZERO               TO CO-INST-RATE-BPS
           END-IF
           IF CO-INST-RATE-BPS NOT > ZERO
               MOVE ZERO               TO WS-COMM-RESULT
               MOVE 'NO NEGOTIATED RATE ON ACCOUNT'
                                       TO CO-MESSAGE
               GO TO 3300-EXIT
           END-IF
           COMPUTE WS-RATE-FACTOR = CO-INST-RATE-BPS / 10000
           COMPUTE WS-COMM-RESULT ROUNDED =
                   CO-PRINCIPAL * WS-RATE-FACTOR.
       3300-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3500 - SCHEDULE 05 (1995 PRICING - GRANDFATHERED)              *
      *   TICKET CHARGE                                                *
      *   + PER SHARE BY SHARE BAND (EACH BAND AT ITS OWN RATE)        *
      *   + 0.30% OF PRINCIPAL ABOVE $50,000                           *
      *   + ODD LOT CHARGE UNDER 100 SHARES                            *
      *   MINIMUM $38.50  MAXIMUM $1,850.00                            *
      *----------------------------------------------------------------*
       3500-SCHED-05-LEGACY.
           MOVE 'LGCY'                 TO CO-RULE-APPLIED
           MOVE WS-LGY-TICKET-CHARGE   TO WS-COMM-ACCUM
           MOVE CO-QTY                 TO WS-SHARES-REMAINING
           MOVE ZERO                   TO WS-SHARE-LOWER
           SET WS-TIERS-NOT-DONE       TO TRUE
           PERFORM 3510-LEGACY-SHARE-BAND THRU 3510-EXIT
               VARYING WS-TIER-IDX FROM 1 BY 1
               UNTIL WS-TIER-IDX > 3
                  OR WS-TIERS-DONE
           IF CO-PRINCIPAL > WS-LGY-PRIN-THRESHOLD
               COMPUTE WS-PRIN-EXCESS =
                       CO-PRINCIPAL - WS-LGY-PRIN-THRESHOLD
               COMPUTE WS-TIER-COMM =
                       WS-PRIN-EXCESS * WS-LGY-PRIN-RATE
               ADD WS-TIER-COMM        TO WS-COMM-ACCUM
           END-IF
           IF CO-QTY < WS-LGY-ODD-LOT-LIMIT
               ADD WS-LGY-ODD-LOT-CHARGE TO WS-COMM-ACCUM
           END-IF
           COMPUTE WS-COMM-RESULT ROUNDED = WS-COMM-ACCUM
           IF WS-COMM-RESULT < WS-LGY-MINIMUM
               MOVE WS-LGY-MINIMUM     TO WS-COMM-RESULT
           END-IF
           IF WS-COMM-RESULT > WS-LGY-MAXIMUM
               MOVE WS-LGY-MAXIMUM     TO WS-COMM-RESULT
           END-IF.
       3500-EXIT.
           EXIT.
      *
       3510-LEGACY-SHARE-BAND.
           COMPUTE WS-SHARE-WIDTH =
                   WS-LGY-UPPER (WS-TIER-IDX) - WS-SHARE-LOWER
           IF WS-SHARES-REMAINING > WS-SHARE-WIDTH
               MOVE WS-SHARE-WIDTH     TO WS-SHARE-BAND
           ELSE
               MOVE WS-SHARES-REMAINING TO WS-SHARE-BAND
               SET WS-TIERS-DONE       TO TRUE
           END-IF
           COMPUTE WS-TIER-COMM =
                   WS-SHARE-BAND * WS-LGY-RATE (WS-TIER-IDX)
           ADD WS-TIER-COMM            TO WS-COMM-ACCUM
           SUBTRACT WS-SHARE-BAND      FROM WS-SHARES-REMAINING
           MOVE WS-LGY-UPPER (WS-TIER-IDX)
                                       TO WS-SHARE-LOWER
           IF WS-SHARES-REMAINING NOT > ZERO
               SET WS-TIERS-DONE       TO TRUE
           END-IF.
       3510-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3900 - OLD 1989 FLAT PERCENT CALCULATION.  REPLACED BY THE     *
      *        TIER TABLE IN 1996.  KEPT FOR AUDIT REPRODUCTION.       *
      *----------------------------------------------------------------*
      *3900-SCHED-01-FLAT.
      *    COMPUTE WS-COMM-RESULT ROUNDED = CO-PRINCIPAL * .0200
      *    IF WS-COMM-RESULT < WS-RTL-MINIMUM-OLD
      *        MOVE WS-RTL-MINIMUM-OLD TO WS-COMM-RESULT
      *    END-IF.
      *3900-EXIT.
      *    EXIT.
