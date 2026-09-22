      *================================================================*
      * PROGRAM    : CMU030                                            *
      * TITLE      : BOND ACCRUED INTEREST                             *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   COMPUTES ACCRUED INTEREST FROM THE LAST COUPON DATE TO THE   *
      *   SETTLEMENT DATE FOR A FIXED-COUPON BOND.                     *
      *     '30'  30/360 BOND BASIS   FACE*RATE/100*DAYS360/360        *
      *     'A3'  ACTUAL/360          FACE*RATE/100*ACTDAYS/360        *
      *     'AA'  ACTUAL/ACTUAL ICMA  FACE*RATE/100/FREQ*ACTDAYS/      *
      *                               ACTUAL DAYS IN COUPON PERIOD     *
      *   WHEN AI-LAST-CPN-DATE IS ZERO THE COUPON SCHEDULE IS BUILT   *
      *   BACKWARDS FROM MATURITY IN STEPS OF 12/FREQ MONTHS (END OF   *
      *   MONTH MATURITIES PAY ON MONTH END).  IF A FIRST COUPON DATE  *
      *   IS GIVEN AND SETTLEMENT FALLS BEFORE IT, INTEREST ACCRUES    *
      *   FROM THE ISSUE DATE (ODD FIRST COUPON).                      *
      *   FREQUENCY 0 (ZERO COUPON) OR RATE 0 GIVES ZERO ACCRUED.      *
      *                                                                *
      * LINKAGE    : CALL 'CMU030' USING AI-ACCRUAL-PARMS  (CMAILNK)   *
      * CALLS      : CMU010 (VALD, D360, DIFC, EOM)                    *
      * RETURN     : AI-RETURN-CODE 00 OK, 04 BAD DAY COUNT,           *
      *              08 BAD DATES / FREQUENCY, 12 SETTLE AFTER MATURITY*
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1993-05-03 DWB  CHG00588  ORIGINAL - 30/360 CORPORATES AND     *
      *                           MUNIS FOR BOND DESK                  *
      * 1994-10-10 DWB  CHG01322  ACT/ACT FOR TREASURIES               *
      * 1996-09-09 DWB  CHG03017  ACT/360 FOR FLOATERS / CP            *
      * 1998-11-02 TLM  CHG04471  Y2K - CCYYMMDD                       *
      * 2001-04-09 DWB  CHG08130  DECIMALIZATION REVIEW - NO CHANGE    *
      * 2005-11-14 KAP  CHG14002  DERIVE LAST/NEXT COUPON FROM         *
      *                           MATURITY WHEN NOT SUPPLIED           *
      * 2010-03-22 SPA  CHG19870  ODD FIRST COUPON FROM ISSUE DATE     *
      * 2018-07-30 JMF  CHG34418  RETURN PERIOD DAYS TO CALLER         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU030.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  05/03/93.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU030 WORKING STORAGE BEGINS'.
      *
       01  WS-SWITCHES.
           05  WS-EOM-MATURITY-SW      PIC X(01).
               88  WS-EOM-MATURITY                VALUE 'Y'.
           05  WS-SCHED-DONE-SW        PIC X(01).
               88  WS-SCHED-DONE                  VALUE 'Y'.
      *
       01  WS-WORK.
           05  WS-MONTHS-PER-PERIOD    PIC S9(03)  COMP-3.
           05  WS-PERIODS-BACK         PIC S9(05)  COMP-3.
           05  WS-TOTAL-MONTHS         PIC S9(07)  COMP-3.
           05  WS-NEW-YEAR             PIC S9(05)  COMP-3.
           05  WS-NEW-MONTH            PIC S9(03)  COMP-3.
           05  WS-QUOT                 PIC S9(07)  COMP-3.
           05  WS-LOOP-GUARD           PIC S9(05)  COMP-3.
           05  WS-LAST-CPN             PIC 9(08).
           05  WS-NEXT-CPN             PIC 9(08).
           05  WS-QUASI-START          PIC 9(08).
           05  WS-SHIFT-MONTHS         PIC S9(05)  COMP-3.
           05  WS-PERIOD-COUPON        PIC S9(13)V9(06) COMP-3.
      *
       01  WS-MAT-DATE                 PIC 9(08).
       01  WS-MAT-DATE-R REDEFINES WS-MAT-DATE.
           05  WS-MAT-CCYY             PIC 9(04).
           05  WS-MAT-MM               PIC 9(02).
           05  WS-MAT-DD               PIC 9(02).
      *
       01  WS-BASE-DATE                PIC 9(08).
       01  WS-BASE-DATE-R REDEFINES WS-BASE-DATE.
           05  WS-BASE-CCYY            PIC 9(04).
           05  WS-BASE-MM              PIC 9(02).
           05  WS-BASE-DD              PIC 9(02).
       01  WS-SHIFT-DATE               PIC 9(08).
       01  WS-SHIFT-DATE-R REDEFINES WS-SHIFT-DATE.
           05  WS-SHIFT-CCYY           PIC 9(04).
           05  WS-SHIFT-MM             PIC 9(02).
           05  WS-SHIFT-DD             PIC 9(02).
       01  WS-SHIFT-EOM                PIC 9(08).
       01  WS-SHIFT-EOM-R REDEFINES WS-SHIFT-EOM.
           05  FILLER                  PIC 9(06).
           05  WS-SHIFT-EOM-DD         PIC 9(02).
      *
           COPY CMDTLNK.
      *
       LINKAGE SECTION.
           COPY CMAILNK.
      *
       PROCEDURE DIVISION USING AI-ACCRUAL-PARMS.
       0000-MAINLINE.
           MOVE ZERO   TO AI-RETURN-CODE
           MOVE ZERO   TO AI-ACCRUED-AMOUNT AI-ACCRUAL-DAYS
                          AI-PERIOD-DAYS
           MOVE SPACES TO AI-MESSAGE
           PERFORM 1000-EDIT-INPUT THRU 1000-EXIT
           IF NOT AI-OK
               GO TO 0000-EXIT.
      *    ZERO COUPON / ZERO RATE / SETTLING ON MATURITY - NO ACCRUAL
           IF AI-COUPON-FREQ = ZERO OR AI-COUPON-RATE = ZERO
               GO TO 0000-EXIT.
           IF AI-SETTLE-DATE = AI-MATURITY-DATE
               MOVE AI-MATURITY-DATE TO AI-LAST-CPN-DATE
               MOVE AI-MATURITY-DATE TO AI-NEXT-CPN-DATE
               GO TO 0000-EXIT.
           PERFORM 2000-COUPON-DATES THRU 2000-EXIT
           IF NOT AI-OK
               GO TO 0000-EXIT.
           PERFORM 3000-DAY-COUNTS THRU 3000-EXIT
           IF NOT AI-OK
               GO TO 0000-EXIT.
           PERFORM 4000-CALC-ACCRUED THRU 4000-EXIT.
       0000-EXIT.
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - INPUT EDITS                                             *
      *----------------------------------------------------------------*
       1000-EDIT-INPUT.
           IF AI-DAYCOUNT NOT = '30' AND NOT = 'A3' AND NOT = 'AA'
               MOVE 04 TO AI-RETURN-CODE
               STRING 'INVALID DAY COUNT ' AI-DAYCOUNT
                   DELIMITED BY SIZE INTO AI-MESSAGE
               GO TO 1000-EXIT.
           IF AI-COUPON-FREQ NOT NUMERIC
               MOVE 08 TO AI-RETURN-CODE
               MOVE 'COUPON FREQUENCY NOT NUMERIC' TO AI-MESSAGE
               GO TO 1000-EXIT.
           IF AI-COUPON-FREQ NOT = 0 AND NOT = 1 AND NOT = 2
                             AND NOT = 4
               MOVE 08 TO AI-RETURN-CODE
               STRING 'INVALID COUPON FREQUENCY ' AI-COUPON-FREQ
                   DELIMITED BY SIZE INTO AI-MESSAGE
               GO TO 1000-EXIT.
           MOVE AI-SETTLE-DATE TO DT-DATE-1
           PERFORM 1100-VALIDATE THRU 1100-EXIT
           IF NOT AI-OK
               MOVE 'INVALID SETTLEMENT DATE' TO AI-MESSAGE
               GO TO 1000-EXIT.
           MOVE AI-MATURITY-DATE TO DT-DATE-1
           PERFORM 1100-VALIDATE THRU 1100-EXIT
           IF NOT AI-OK
               MOVE 'INVALID MATURITY DATE' TO AI-MESSAGE
               GO TO 1000-EXIT.
           IF AI-SETTLE-DATE > AI-MATURITY-DATE
               MOVE 12 TO AI-RETURN-CODE
               MOVE 'SETTLEMENT AFTER MATURITY' TO AI-MESSAGE
               GO TO 1000-EXIT.
           IF AI-ISSUE-DATE NOT NUMERIC
               MOVE ZERO TO AI-ISSUE-DATE.
           IF AI-FIRST-CPN-DATE NOT NUMERIC
               MOVE ZERO TO AI-FIRST-CPN-DATE.
           IF AI-LAST-CPN-DATE NOT NUMERIC
               MOVE ZERO TO AI-LAST-CPN-DATE.
           IF AI-NEXT-CPN-DATE NOT NUMERIC
               MOVE ZERO TO AI-NEXT-CPN-DATE.
           IF AI-ISSUE-DATE > ZERO
              AND AI-SETTLE-DATE < AI-ISSUE-DATE
               MOVE 08 TO AI-RETURN-CODE
               MOVE 'SETTLEMENT BEFORE ISSUE DATE' TO AI-MESSAGE.
       1000-EXIT.
           EXIT.
      *
       1100-VALIDATE.
           MOVE 'VALD' TO DT-FUNCTION
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 08 TO AI-RETURN-CODE.
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2000 - LAST / NEXT COUPON DATES                                *
      *----------------------------------------------------------------*
       2000-COUPON-DATES.
           MOVE 'N' TO WS-EOM-MATURITY-SW
           DIVIDE 12 BY AI-COUPON-FREQ GIVING WS-MONTHS-PER-PERIOD
      *    CALLER SUPPLIED THE LAST COUPON - TRUST IT
           IF AI-LAST-CPN-DATE > ZERO
               MOVE AI-LAST-CPN-DATE TO WS-LAST-CPN
               IF AI-NEXT-CPN-DATE > ZERO
                   MOVE AI-NEXT-CPN-DATE TO WS-NEXT-CPN
               ELSE
                   MOVE WS-LAST-CPN TO WS-BASE-DATE
                   MOVE WS-MONTHS-PER-PERIOD TO WS-SHIFT-MONTHS
                   PERFORM 2500-SHIFT-MONTHS THRU 2500-EXIT
                   MOVE WS-SHIFT-DATE TO WS-NEXT-CPN
               END-IF
               GO TO 2090-SET-OUTPUT.
      *    ODD FIRST COUPON - ACCRUE FROM ISSUE DATE     CHG19870
           IF AI-FIRST-CPN-DATE > ZERO
              AND AI-SETTLE-DATE < AI-FIRST-CPN-DATE
              AND AI-ISSUE-DATE > ZERO
               MOVE AI-ISSUE-DATE     TO WS-LAST-CPN
               MOVE AI-FIRST-CPN-DATE TO WS-NEXT-CPN
               GO TO 2090-SET-OUTPUT.
      *    WALK BACK FROM MATURITY                        CHG14002
           MOVE AI-MATURITY-DATE TO WS-MAT-DATE
           MOVE 'EOM '           TO DT-FUNCTION
           MOVE AI-MATURITY-DATE TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF DT-RESULT-DATE = AI-MATURITY-DATE
               MOVE 'Y' TO WS-EOM-MATURITY-SW
           ELSE
               MOVE 'N' TO WS-EOM-MATURITY-SW
           END-IF
           MOVE AI-MATURITY-DATE TO WS-NEXT-CPN
           MOVE AI-MATURITY-DATE TO WS-BASE-DATE
           MOVE ZERO TO WS-PERIODS-BACK WS-LOOP-GUARD
           MOVE 'N'  TO WS-SCHED-DONE-SW
           PERFORM 2100-STEP-BACK THRU 2100-EXIT
               UNTIL WS-SCHED-DONE OR NOT AI-OK.
           IF NOT AI-OK
               GO TO 2000-EXIT.
       2090-SET-OUTPUT.
           MOVE WS-LAST-CPN TO AI-LAST-CPN-DATE
           MOVE WS-NEXT-CPN TO AI-NEXT-CPN-DATE.
       2000-EXIT.
           EXIT.
      *
       2100-STEP-BACK.
           ADD 1 TO WS-PERIODS-BACK
           ADD 1 TO WS-LOOP-GUARD
           IF WS-LOOP-GUARD > 1200
               MOVE 08 TO AI-RETURN-CODE
               MOVE 'COUPON SCHEDULE TOO LONG' TO AI-MESSAGE
               GO TO 2100-EXIT.
           COMPUTE WS-SHIFT-MONTHS = ZERO -
                   (WS-PERIODS-BACK * WS-MONTHS-PER-PERIOD)
           PERFORM 2500-SHIFT-MONTHS THRU 2500-EXIT
           IF WS-SHIFT-DATE NOT > AI-SETTLE-DATE
               MOVE WS-SHIFT-DATE TO WS-LAST-CPN
               MOVE 'Y' TO WS-SCHED-DONE-SW
           ELSE
               MOVE WS-SHIFT-DATE TO WS-NEXT-CPN
           END-IF.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2500 - WS-BASE-DATE + WS-SHIFT-MONTHS -> WS-SHIFT-DATE         *
      *   DAY IS KEPT, CAPPED AT MONTH END; EOM MATURITIES STAY EOM.   *
      *----------------------------------------------------------------*
       2500-SHIFT-MONTHS.
           COMPUTE WS-TOTAL-MONTHS = WS-BASE-CCYY * 12
                                   + WS-BASE-MM - 1
                                   + WS-SHIFT-MONTHS
           DIVIDE WS-TOTAL-MONTHS BY 12 GIVING WS-NEW-YEAR
                  REMAINDER WS-NEW-MONTH
           ADD 1 TO WS-NEW-MONTH
           MOVE WS-NEW-YEAR  TO WS-SHIFT-CCYY
           MOVE WS-NEW-MONTH TO WS-SHIFT-MM
           MOVE 01           TO WS-SHIFT-DD
           MOVE 'EOM '        TO DT-FUNCTION
           MOVE WS-SHIFT-DATE TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           MOVE DT-RESULT-DATE TO WS-SHIFT-EOM
           IF WS-EOM-MATURITY
              OR WS-BASE-DD > WS-SHIFT-EOM-DD
               MOVE WS-SHIFT-EOM-DD TO WS-SHIFT-DD
           ELSE
               MOVE WS-BASE-DD      TO WS-SHIFT-DD
           END-IF.
       2500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3000 - ACCRUAL DAYS AND PERIOD DAYS                            *
      *----------------------------------------------------------------*
       3000-DAY-COUNTS.
           MOVE WS-LAST-CPN    TO DT-DATE-1
           MOVE AI-SETTLE-DATE TO DT-DATE-2
           IF AI-DAYCOUNT = '30'
               MOVE 'D360' TO DT-FUNCTION
           ELSE
               MOVE 'DIFC' TO DT-FUNCTION
           END-IF
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 08 TO AI-RETURN-CODE
               MOVE DT-MESSAGE TO AI-MESSAGE
               GO TO 3000-EXIT.
           MOVE DT-RESULT-NUM TO AI-ACCRUAL-DAYS
           IF AI-ACCRUAL-DAYS < ZERO
               MOVE ZERO TO AI-ACCRUAL-DAYS.
      *
           EVALUATE AI-DAYCOUNT
               WHEN '30'
               WHEN 'A3'
                   DIVIDE 360 BY AI-COUPON-FREQ GIVING AI-PERIOD-DAYS
               WHEN 'AA'
      *            ODD FIRST PERIOD USES THE REGULAR (QUASI) PERIOD
      *            ENDING ON THE FIRST COUPON DATE
                   MOVE WS-NEXT-CPN TO WS-BASE-DATE
                   COMPUTE WS-SHIFT-MONTHS = ZERO - WS-MONTHS-PER-PERIOD
                   PERFORM 2500-SHIFT-MONTHS THRU 2500-EXIT
                   MOVE WS-SHIFT-DATE TO WS-QUASI-START
                   IF WS-QUASI-START < WS-LAST-CPN
                       MOVE WS-LAST-CPN TO WS-QUASI-START
                   END-IF
                   MOVE 'DIFC'         TO DT-FUNCTION
                   MOVE WS-QUASI-START TO DT-DATE-1
                   MOVE WS-NEXT-CPN    TO DT-DATE-2
                   CALL 'CMU010' USING DT-DATE-PARMS
                   MOVE DT-RESULT-NUM TO AI-PERIOD-DAYS
                   IF AI-PERIOD-DAYS NOT > ZERO
                       MOVE 08 TO AI-RETURN-CODE
                       MOVE 'ZERO LENGTH COUPON PERIOD' TO AI-MESSAGE
                   END-IF
           END-EVALUATE.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 4000 - ACCRUED AMOUNT                                          *
      *   30/360 FORMULA AS ORIGINALLY SIGNED OFF BY THE BOND DESK     *
      *   (DWB 05/93) - RESULTS TIE TO THE BLOTTER, DO NOT RE-ORDER.   *
      *----------------------------------------------------------------*
       4000-CALC-ACCRUED.
           EVALUATE AI-DAYCOUNT
               WHEN '30'
                   COMPUTE AI-ACCRUED-AMOUNT =
                       AI-FACE-AMOUNT * AI-COUPON-RATE / 100
                           * AI-ACCRUAL-DAYS / 360
               WHEN 'A3'
                   COMPUTE AI-ACCRUED-AMOUNT ROUNDED =
                       AI-FACE-AMOUNT * AI-COUPON-RATE / 100
                           * AI-ACCRUAL-DAYS / 360
               WHEN 'AA'
                   COMPUTE WS-PERIOD-COUPON =
                       AI-FACE-AMOUNT * AI-COUPON-RATE / 100
                           / AI-COUPON-FREQ
                   COMPUTE AI-ACCRUED-AMOUNT ROUNDED =
                       WS-PERIOD-COUPON * AI-ACCRUAL-DAYS
                           / AI-PERIOD-DAYS
           END-EVALUATE.
       4000-EXIT.
           EXIT.
