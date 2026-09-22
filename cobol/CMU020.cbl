      *================================================================*
      * PROGRAM    : CMU020                                            *
      * TITLE      : SETTLEMENT DATE CALCULATION                       *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   RETURNS THE REGULAR-WAY SETTLEMENT DATE FOR A TRADE DATE AND *
      *   SECURITY TYPE.  SETTLEMENT CYCLE RULES:                      *
      *     SD-SETTLE-DAYS-OVR > 0     OVERRIDE (CASH, SELLER'S OPT.)  *
      *     NON-USD SECURITY           T+2 ALWAYS (LOCAL MARKET)       *
      *     GV  GOVERNMENT             T+1, FED CALENDAR               *
      *     EQ/PF/AD/MF/CB/MU          T+1 FROM 05/28/2024 (SEC RULE   *
      *                                15C6-1 AMENDMENT), T+2 FROM     *
      *                                09/05/2017, T+3 FROM 06/07/1995,*
      *                                T+5 BEFORE THAT                 *
      *   THE SETTLEMENT DATE IS THE TRADE DATE PLUS N BUSINESS DAYS   *
      *   ON THE CALENDAR (CMU010 ADDB).  A TRADE DATE THAT IS NOT A   *
      *   BUSINESS DAY IS FLAGGED (RC 06) BUT A DATE IS STILL RETURNED.*
      *                                                                *
      * LINKAGE    : CALL 'CMU020' USING SD-SETTLE-PARMS  (CMSTLNK)    *
      * CALLS      : CMU010 (VALD, BUSD, ADDB)                         *
      * RETURN     : SD-RETURN-CODE 00 OK, 04 BAD TRADE DATE,          *
      *              06 TRADE DATE NOT A BUSINESS DAY,                 *
      *              12 CALENDAR ERROR FROM CMU010                     *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1990-04-02 RJK            ORIGINAL - T+5 ALL PRODUCTS          *
      * 1994-10-10 DWB  CHG01322  GOVERNMENTS T+1 ON FED CALENDAR      *
      * 1995-05-15 DWB  CHG01670  T+3 CONVERSION EFFECTIVE 06/07/1995  *
      * 1998-11-02 TLM  CHG04471  Y2K - CCYYMMDD                       *
      * 2006-02-27 KAP  CHG14660  NON-USD SECURITIES T+2 (ADR LOCAL    *
      *                           LINES, EUROCLEAR)                    *
      * 2017-08-21 SPA  CHG32205  T+2 CONVERSION EFFECTIVE 09/05/2017  *
      * 2024-02-12 NVR  CHG58810  T+1 CONVERSION EFFECTIVE 05/28/2024  *
      * 2024-06-03 NVR  CHG59102  RETURN SD-SETTLE-CYCLE TO CALLER     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU020.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  04/02/90.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU020 WORKING STORAGE BEGINS'.
      *
      *    CUT-OVER DATES - DO NOT CHANGE WITHOUT OPS / COMPLIANCE
       01  WS-CUTOVER-DATES.
           05  WS-T3-EFFECTIVE         PIC 9(08)  VALUE 19950607.
           05  WS-T2-EFFECTIVE         PIC 9(08)  VALUE 20170905.
           05  WS-T1-EFFECTIVE         PIC 9(08)  VALUE 20240528.
      *
       01  WS-WORK.
           05  WS-CYCLE                PIC 9(01).
           05  WS-CALENDAR             PIC X(04).
           05  WS-HOLIDAY-TD-SW        PIC X(01).
               88  WS-TD-NOT-BUS-DAY              VALUE 'Y'.
      *
           COPY CMDTLNK.
      *
       LINKAGE SECTION.
           COPY CMSTLNK.
      *
       PROCEDURE DIVISION USING SD-SETTLE-PARMS.
       0000-MAINLINE.
           MOVE ZERO   TO SD-RETURN-CODE
           MOVE ZERO   TO SD-SETTLE-DATE SD-SETTLE-CYCLE
           MOVE SPACES TO SD-MESSAGE
           MOVE 'N'    TO WS-HOLIDAY-TD-SW
           PERFORM 1000-EDIT-TRADE-DATE THRU 1000-EXIT
           IF SD-RETURN-CODE = ZERO
               PERFORM 2000-DETERMINE-CYCLE THRU 2000-EXIT
               PERFORM 3000-CALC-SETTLE-DATE THRU 3000-EXIT
           END-IF
           GOBACK.
      *
      *----------------------------------------------------------------*
       1000-EDIT-TRADE-DATE.
           IF SD-TRADE-DATE NOT NUMERIC
               MOVE 04 TO SD-RETURN-CODE
               MOVE 'TRADE DATE NOT NUMERIC' TO SD-MESSAGE
               GO TO 1000-EXIT.
           MOVE 'VALD'        TO DT-FUNCTION
           MOVE SD-TRADE-DATE TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 04 TO SD-RETURN-CODE
               MOVE DT-MESSAGE TO SD-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * SETTLEMENT CYCLE AND CALENDAR                                  *
      *----------------------------------------------------------------*
       2000-DETERMINE-CYCLE.
           MOVE 'NYSE' TO WS-CALENDAR
           EVALUATE TRUE
               WHEN SD-SETTLE-DAYS-OVR IS NUMERIC
                AND SD-SETTLE-DAYS-OVR > ZERO
                   MOVE SD-SETTLE-DAYS-OVR TO WS-CYCLE
                   IF SD-SEC-TYPE = 'GV'
                       MOVE 'FED ' TO WS-CALENDAR
                   END-IF
               WHEN SD-CCY NOT = 'USD' AND SD-CCY NOT = SPACES
                   MOVE 2 TO WS-CYCLE
               WHEN SD-SEC-TYPE = 'GV'
                   MOVE 1 TO WS-CYCLE
                   MOVE 'FED ' TO WS-CALENDAR
               WHEN SD-TRADE-DATE < WS-T3-EFFECTIVE
                   MOVE 5 TO WS-CYCLE
               WHEN SD-TRADE-DATE < WS-T2-EFFECTIVE
                   MOVE 3 TO WS-CYCLE
               WHEN SD-TRADE-DATE < WS-T1-EFFECTIVE
                   MOVE 2 TO WS-CYCLE
               WHEN OTHER
      *            EQ PF AD MF CB MU AND ANYTHING UNCLASSIFIED
                   MOVE 1 TO WS-CYCLE
           END-EVALUATE
           MOVE WS-CYCLE TO SD-SETTLE-CYCLE.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3000-CALC-SETTLE-DATE.
           MOVE 'BUSD'        TO DT-FUNCTION
           MOVE WS-CALENDAR   TO DT-CALENDAR
           MOVE SD-TRADE-DATE TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 12 TO SD-RETURN-CODE
               MOVE DT-MESSAGE TO SD-MESSAGE
               GO TO 3000-EXIT.
           IF DT-RESULT-NO
               MOVE 'Y' TO WS-HOLIDAY-TD-SW.
      *
           MOVE 'ADDB'        TO DT-FUNCTION
           MOVE WS-CALENDAR   TO DT-CALENDAR
           MOVE SD-TRADE-DATE TO DT-DATE-1
           MOVE WS-CYCLE      TO DT-DAYS
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 12 TO SD-RETURN-CODE
               MOVE DT-MESSAGE TO SD-MESSAGE
               GO TO 3000-EXIT.
           MOVE DT-RESULT-DATE TO SD-SETTLE-DATE
           IF WS-TD-NOT-BUS-DAY
               MOVE 06 TO SD-RETURN-CODE
               STRING 'TRADE DATE NOT A BUSINESS DAY ('
                      WS-CALENDAR ')' DELIMITED BY SIZE
                 INTO SD-MESSAGE.
       3000-EXIT.
           EXIT.
