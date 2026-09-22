      *================================================================*
      * PROGRAM    : CMU010                                            *
      * TITLE      : DATE SERVICES / BUSINESS CALENDAR                 *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   CALLABLE DATE ARITHMETIC AND BUSINESS CALENDAR MODULE USED   *
      *   BY THE WHOLE EOD CYCLE.  ALL DATES ARE CCYYMMDD.             *
      *                                                                *
      *   FUNCTION  IN                  OUT                            *
      *   VALD      DATE-1              RETURN CODE, RESULT-FLAG Y/N   *
      *   ADDC      DATE-1, DAYS        RESULT-DATE (CALENDAR DAYS)    *
      *   ADDB      DATE-1, DAYS        RESULT-DATE (BUSINESS DAYS,    *
      *                                 DAYS MAY BE NEGATIVE)          *
      *   NXTB      DATE-1              RESULT-DATE NEXT BUSINESS DAY  *
      *   PRVB      DATE-1              RESULT-DATE PREV BUSINESS DAY  *
      *   BUSD      DATE-1              RESULT-FLAG Y = BUSINESS DAY   *
      *   DIFC      DATE-1, DATE-2      RESULT-NUM = DATE-2 - DATE-1   *
      *   DIFB      DATE-1, DATE-2      RESULT-NUM = BUSINESS DAYS     *
      *                                 AFTER DATE-1 UP TO AND         *
      *                                 INCLUDING DATE-2 (NEGATIVE IF  *
      *                                 DATE-2 BEFORE DATE-1)          *
      *   D360      DATE-1, DATE-2      RESULT-NUM 30/360 DAYS         *
      *   DOW       DATE-1              RESULT-NUM 1=MON ... 7=SUN     *
      *   EOM       DATE-1              RESULT-DATE LAST CALENDAR DAY  *
      *   LBDM      DATE-1              RESULT-DATE LAST BUSINESS DAY  *
      *                                 OF THE MONTH                   *
      *   JUL       DATE-1              RESULT-NUM CCYYDDD             *
      *   W2Y4      DATE-6 (YYMMDD)     RESULT-DATE CCYYMMDD           *
      *                                                                *
      *   CALENDAR: DT-CALENDAR 'NYSE' (DEFAULT WHEN SPACES) = EQUITY  *
      *   MARKET, 'FED ' = FEDERAL RESERVE (GOVERNMENT SECURITIES).    *
      *   A BUSINESS DAY IS MONDAY-FRIDAY AND NOT A HOLIDAY OF THE     *
      *   CALENDAR.  HOLIDAYS ARE LOADED ON THE FIRST CALL FROM DDNAME *
      *   HOLIDAYS (SORTED BY DATE, CMHOLID, CALENDAR NYSE/FED /BOTH). *
      *                                                                *
      * LINKAGE    : CALL 'CMU010' USING DT-DATE-PARMS    (CMDTLNK)    *
      * FILES      : HOLIDAYS  INPUT  MSEC.PROD.CM.HOLIDAYS  FB 40     *
      * RETURN     : DT-RETURN-CODE 00 OK  04 INVALID DATE             *
      *              08 INVALID FUNCTION  12 CALENDAR ERROR (UNKNOWN   *
      *              CALENDAR OR HOLIDAY FILE NOT AVAILABLE)           *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1989-03-14 RJK            ORIGINAL - YYMMDD, VALD ADDC NXTB    *
      *                           PRVB BUSD DOW                        *
      * 1989-11-06 RJK            ADDB, DIFC                           *
      * 1990-02-26 RJK            HOLIDAY TABLE FROM FILE INSTEAD OF   *
      *                           COMPILED-IN TABLE                    *
      * 1993-05-03 DWB  CHG00588  D360 FOR BOND ACCRUALS (CMU030)      *
      * 1994-10-10 DWB  CHG01322  FED CALENDAR FOR GOVT SETTLEMENT     *
      * 1995-06-01 DWB  CHG01670  DIFB FOR T+3 FAILS AGING             *
      * 1998-11-02 TLM  CHG04471  Y2K - ALL DATES CCYYMMDD.  NEW       *
      *                           FUNCTION W2Y4 FOR FEEDS STILL        *
      *                           SENDING 2-DIGIT YEARS                *
      * 1999-02-22 TLM  CHG04471  Y2K - LEAP YEAR 2000 (DIV BY 400)    *
      * 2003-09-15 KAP  CHG11544  JUL, EOM FOR STATEMENT CYCLE         *
      * 2008-01-07 SPA  CHG17402  HOLIDAY TABLE 300 -> 600 ENTRIES     *
      * 2011-06-20 SPA  CHG21877  LBDM FOR MONTH-END CYCLE TYPE        *
      * 2013-03-04 JMF  CHG25016  HOLIDAY FILE OUT OF SEQUENCE - FALL  *
      *                           BACK TO SERIAL SEARCH, DO NOT ABEND  *
      * 2025-01-03 NVR  CHG61094  NYSE CLOSED 01/09/2025 (NATIONAL DAY *
      *                           OF MOURNING) - SEE 7520              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU010.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/14/89.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HOLIDAY-FILE ASSIGN TO HOLIDAYS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-HOL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  HOLIDAY-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
           COPY CMHOLID.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU010 WORKING STORAGE BEGINS'.
       01  WS-HOL-STATUS               PIC X(02).
           88  WS-HOL-OK                      VALUE '00'.
           88  WS-HOL-EOF                     VALUE '10'.
      *
       01  WS-SWITCHES.
           05  WS-FIRST-CALL-SW        PIC X(01)  VALUE 'Y'.
               88  WS-FIRST-CALL                  VALUE 'Y'.
           05  WS-HOL-LOADED-SW        PIC X(01)  VALUE 'N'.
               88  WS-HOLIDAYS-LOADED             VALUE 'Y'.
           05  WS-HOL-EOF-SW           PIC X(01)  VALUE 'N'.
               88  WS-END-OF-HOLIDAYS             VALUE 'Y'.
           05  WS-SERIAL-SW            PIC X(01)  VALUE 'N'.
               88  WS-SERIAL-SEARCH               VALUE 'Y'.
           05  WS-DATE-VALID-SW        PIC X(01).
               88  WS-DATE-VALID                  VALUE 'Y'.
               88  WS-DATE-INVALID                VALUE 'N'.
           05  WS-LEAP-SW              PIC X(01).
               88  WS-LEAP-YEAR                   VALUE 'Y'.
           05  WS-BUS-DAY-SW           PIC X(01).
               88  WS-BUSINESS-DAY                VALUE 'Y'.
           05  WS-HOLIDAY-SW           PIC X(01).
               88  WS-IS-HOLIDAY                  VALUE 'Y'.
           05  WS-CAL-CODE             PIC X(04).
               88  WS-CAL-NYSE                    VALUE 'NYSE'.
               88  WS-CAL-FED                     VALUE 'FED '.
               88  WS-CAL-BOTH                    VALUE 'BOTH'.
               88  WS-CAL-VALID           VALUE 'NYSE' 'FED ' 'BOTH'.
      *
      *----------------------------------------------------------------*
      * CALENDAR TABLES                                                *
      *----------------------------------------------------------------*
       01  WS-MONTH-DAYS-DATA.
           05  FILLER                  PIC X(24)
                                 VALUE '312831303130313130313031'.
       01  WS-MONTH-DAYS-TBL REDEFINES WS-MONTH-DAYS-DATA.
           05  WS-MONTH-DAYS           PIC 9(02) OCCURS 12 TIMES.
       01  WS-CUM-DAYS-DATA.
           05  FILLER                  PIC X(36)
                     VALUE '000031059090120151181212243273304334'.
       01  WS-CUM-DAYS-TBL REDEFINES WS-CUM-DAYS-DATA.
           05  WS-CUM-DAYS             PIC 9(03) OCCURS 12 TIMES.
      *
      *----------------------------------------------------------------*
      * HOLIDAY TABLE - ONE ENTRY PER DATE, CALENDARS MERGED           *
      *----------------------------------------------------------------*
       01  WS-HOL-MAX                  PIC S9(04) COMP VALUE +600.
       01  WS-HOL-COUNT                PIC S9(04) COMP VALUE ZERO.
       01  WS-HOLIDAY-TABLE.
           05  WS-HOL-ENTRY            OCCURS 1 TO 600 TIMES
                                       DEPENDING ON WS-HOL-COUNT
                                       ASCENDING KEY IS WS-HOL-DATE
                                       INDEXED BY HOL-IX.
               10  WS-HOL-DATE         PIC 9(08).
               10  WS-HOL-NYSE-SW      PIC X(01).
               10  WS-HOL-FED-SW       PIC X(01).
       01  WS-HOL-STATS.
           05  WS-HOL-READ             PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-HOL-REJECTED         PIC S9(05) COMP-3 VALUE ZERO.
           05  WS-HOL-PREV-DATE        PIC 9(08)         VALUE ZERO.
           05  WS-SUB                  PIC S9(04) COMP   VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * CONVERSION WORK AREAS                                          *
      *   DAY NUMBER 1 = 01/01/1601 (SAME BASE AS INTEGER-OF-DATE)     *
      *----------------------------------------------------------------*
       01  WS-CNV-DATE                 PIC 9(08).
       01  WS-CNV-DATE-R REDEFINES WS-CNV-DATE.
           05  WS-CNV-CCYY             PIC 9(04).
           05  WS-CNV-MM               PIC 9(02).
           05  WS-CNV-DD               PIC 9(02).
       01  WS-CNV-DAYNUM               PIC S9(09) COMP-3.
      *
       01  WS-CONV-WORK.
           05  WS-Y1                   PIC S9(05) COMP-3.
           05  WS-Q4                   PIC S9(05) COMP-3.
           05  WS-Q100                 PIC S9(05) COMP-3.
           05  WS-Q400                 PIC S9(05) COMP-3.
           05  WS-REM                  PIC S9(05) COMP-3.
           05  WS-DOY                  PIC S9(05) COMP-3.
           05  WS-MM-SUB               PIC S9(04) COMP.
           05  WS-LEAP-ADJ             PIC S9(01) COMP-3.
           05  WS-DIM                  PIC S9(03) COMP-3.
           05  WS-J-YEAR               PIC S9(05) COMP-3.
           05  WS-J-DAYNUM             PIC S9(09) COMP-3.
           05  WS-J-SAVE-DAYNUM        PIC S9(09) COMP-3.
           05  WS-CHK-CCYY             PIC 9(04).
           05  WS-QUOT                 PIC S9(09) COMP-3.
      *
       01  WS-CHK-DATE                 PIC 9(08).
       01  WS-CHK-DAYNUM               PIC S9(09) COMP-3.
       01  WS-CHK-DOW                  PIC 9(01).
      *
       01  WS-DAYNUM-1                 PIC S9(09) COMP-3.
       01  WS-DAYNUM-2                 PIC S9(09) COMP-3.
       01  WS-DAYS-LEFT                PIC S9(07) COMP-3.
       01  WS-DAY-COUNT                PIC S9(07) COMP-3.
       01  WS-LOOP-GUARD               PIC S9(05) COMP-3.
       01  WS-STEP                     PIC S9(01) COMP-3.
      *
       01  WS-360-WORK.
           05  WS-360-DATE-1           PIC 9(08).
           05  WS-360-DATE-1-R REDEFINES WS-360-DATE-1.
               10  WS-360-Y1           PIC 9(04).
               10  WS-360-M1           PIC 9(02).
               10  WS-360-D1           PIC 9(02).
           05  WS-360-DATE-2           PIC 9(08).
           05  WS-360-DATE-2-R REDEFINES WS-360-DATE-2.
               10  WS-360-Y2           PIC 9(04).
               10  WS-360-M2           PIC 9(02).
               10  WS-360-D2           PIC 9(02).
           05  WS-360-DD1              PIC S9(03) COMP-3.
           05  WS-360-DD2              PIC S9(03) COMP-3.
      *
       01  WS-W2Y4-WORK.
           05  WS-DATE-6               PIC 9(06).
           05  WS-DATE-6-R REDEFINES WS-DATE-6.
               10  WS-D6-YY            PIC 9(02).
               10  WS-D6-MMDD          PIC 9(04).
           05  WS-W2Y4-CENTURY         PIC 9(02).
           05  WS-PIVOT-YY             PIC 9(02)  VALUE 50.
      *
       01  WS-DISPLAY-COUNT            PIC ZZZ9.
      *
       LINKAGE SECTION.
           COPY CMDTLNK.
      *
       PROCEDURE DIVISION USING DT-DATE-PARMS.
      *
       0000-MAINLINE.
           MOVE ZERO   TO DT-RETURN-CODE
           MOVE ZERO   TO DT-RESULT-DATE DT-RESULT-NUM
           MOVE SPACE  TO DT-RESULT-FLAG
           MOVE SPACES TO DT-MESSAGE
           IF WS-FIRST-CALL
               PERFORM 1000-LOAD-HOLIDAYS THRU 1000-EXIT
               MOVE 'N' TO WS-FIRST-CALL-SW
           END-IF
           EVALUATE DT-FUNCTION
               WHEN 'VALD'
                   PERFORM 2000-VALIDATE       THRU 2000-EXIT
               WHEN 'ADDC'
                   PERFORM 2100-ADD-CALENDAR   THRU 2100-EXIT
               WHEN 'ADDB'
                   PERFORM 2200-ADD-BUSINESS   THRU 2200-EXIT
               WHEN 'NXTB'
                   PERFORM 2300-NEXT-BUSINESS  THRU 2300-EXIT
               WHEN 'PRVB'
                   PERFORM 2400-PREV-BUSINESS  THRU 2400-EXIT
               WHEN 'BUSD'
                   PERFORM 2500-IS-BUSINESS    THRU 2500-EXIT
               WHEN 'DIFC'
                   PERFORM 2600-DIFF-CALENDAR  THRU 2600-EXIT
               WHEN 'DIFB'
                   PERFORM 2700-DIFF-BUSINESS  THRU 2700-EXIT
               WHEN 'D360'
                   PERFORM 2800-DAYS-360       THRU 2800-EXIT
               WHEN 'DOW '
               WHEN 'DOW'
                   PERFORM 2900-DAY-OF-WEEK    THRU 2900-EXIT
               WHEN 'EOM '
               WHEN 'EOM'
                   PERFORM 3000-END-OF-MONTH   THRU 3000-EXIT
               WHEN 'LBDM'
                   PERFORM 3100-LAST-BUS-MONTH THRU 3100-EXIT
               WHEN 'JUL '
               WHEN 'JUL'
                   PERFORM 3200-JULIAN         THRU 3200-EXIT
               WHEN 'W2Y4'
                   PERFORM 3300-WINDOW-YEAR    THRU 3300-EXIT
               WHEN OTHER
                   MOVE 08 TO DT-RETURN-CODE
                   STRING 'INVALID FUNCTION ' DT-FUNCTION
                       DELIMITED BY SIZE INTO DT-MESSAGE
           END-EVALUATE
           GOBACK.
      *
      *================================================================*
      * 1000 - LOAD THE HOLIDAY TABLE (FIRST CALL ONLY)                *
      *================================================================*
       1000-LOAD-HOLIDAYS.
           MOVE ZERO TO WS-HOL-COUNT WS-HOL-READ WS-HOL-REJECTED
                        WS-HOL-PREV-DATE
           OPEN INPUT HOLIDAY-FILE
           IF NOT WS-HOL-OK
               DISPLAY 'CMU010 - HOLIDAYS OPEN FAILED, STATUS '
                       WS-HOL-STATUS
                       ' - BUSINESS DAY FUNCTIONS UNAVAILABLE'
               GO TO 1000-EXIT
           END-IF
           MOVE 'N' TO WS-HOL-EOF-SW
           PERFORM 1100-READ-HOLIDAY THRU 1100-EXIT
               UNTIL WS-END-OF-HOLIDAYS
           CLOSE HOLIDAY-FILE
           MOVE 'Y' TO WS-HOL-LOADED-SW
           MOVE WS-HOL-COUNT TO WS-DISPLAY-COUNT
           DISPLAY 'CMU010 - ' WS-DISPLAY-COUNT
                   ' HOLIDAY DATES LOADED'
           IF WS-SERIAL-SEARCH
               DISPLAY 'CMU010 - HOLIDAY FILE OUT OF SEQUENCE - '
                       'SERIAL SEARCH IN USE'
           END-IF.
       1000-EXIT.
           EXIT.
      *
       1100-READ-HOLIDAY.
           READ HOLIDAY-FILE
               AT END
                   MOVE 'Y' TO WS-HOL-EOF-SW
                   GO TO 1100-EXIT.
           IF NOT WS-HOL-OK
               DISPLAY 'CMU010 - HOLIDAYS READ ERROR, STATUS '
                       WS-HOL-STATUS
               MOVE 'Y' TO WS-HOL-EOF-SW
               GO TO 1100-EXIT.
           ADD 1 TO WS-HOL-READ.
      *    SKIP COMMENT / BLANK RECORDS
           IF HOL-HOLIDAY-REC = SPACES OR HOL-HOLIDAY-REC(1:1) = '*'
               GO TO 1100-EXIT.
           IF HOL-DATE NOT NUMERIC
               ADD 1 TO WS-HOL-REJECTED
               DISPLAY 'CMU010 - BAD HOLIDAY RECORD ' HOL-HOLIDAY-REC
               GO TO 1100-EXIT.
           MOVE HOL-DATE TO WS-CNV-DATE.
           PERFORM 7200-VALIDATE-DATE THRU 7200-EXIT.
           IF WS-DATE-INVALID
               ADD 1 TO WS-HOL-REJECTED
               DISPLAY 'CMU010 - BAD HOLIDAY DATE ' HOL-DATE
               GO TO 1100-EXIT.
           IF NOT HOL-NYSE AND NOT HOL-FED
               ADD 1 TO WS-HOL-REJECTED
               DISPLAY 'CMU010 - UNKNOWN HOLIDAY CALENDAR '
                       HOL-CALENDAR ' ' HOL-DATE
               GO TO 1100-EXIT.
      *    SAME DATE AS PREVIOUS RECORD - MERGE THE CALENDARS
           IF HOL-DATE = WS-HOL-PREV-DATE AND WS-HOL-COUNT > ZERO
               IF HOL-NYSE
                   MOVE 'Y' TO WS-HOL-NYSE-SW (WS-HOL-COUNT)
               END-IF
               IF HOL-FED
                   MOVE 'Y' TO WS-HOL-FED-SW (WS-HOL-COUNT)
               END-IF
               GO TO 1100-EXIT.
           IF HOL-DATE < WS-HOL-PREV-DATE
               MOVE 'Y' TO WS-SERIAL-SW.
           IF WS-HOL-COUNT NOT < WS-HOL-MAX
               DISPLAY 'CMU010 - HOLIDAY TABLE FULL (600) - '
                       'REMAINING RECORDS IGNORED'
               MOVE 'Y' TO WS-HOL-EOF-SW
               GO TO 1100-EXIT.
           ADD 1 TO WS-HOL-COUNT.
           MOVE HOL-DATE TO WS-HOL-DATE (WS-HOL-COUNT).
           MOVE 'N'      TO WS-HOL-NYSE-SW (WS-HOL-COUNT)
                            WS-HOL-FED-SW (WS-HOL-COUNT).
           IF HOL-NYSE
               MOVE 'Y' TO WS-HOL-NYSE-SW (WS-HOL-COUNT).
           IF HOL-FED
               MOVE 'Y' TO WS-HOL-FED-SW (WS-HOL-COUNT).
           MOVE HOL-DATE TO WS-HOL-PREV-DATE.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - VALD : VALIDATE DATE-1                                  *
      *================================================================*
       2000-VALIDATE.
           MOVE DT-DATE-1 TO WS-CNV-DATE
           PERFORM 7200-VALIDATE-DATE THRU 7200-EXIT
           IF WS-DATE-VALID
               MOVE 'Y' TO DT-RESULT-FLAG
               MOVE DT-DATE-1 TO DT-RESULT-DATE
           ELSE
               MOVE 'N' TO DT-RESULT-FLAG
               PERFORM 8000-BAD-DATE-1 THRU 8000-EXIT
           END-IF.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * 2100 - ADDC : DATE-1 + DAYS CALENDAR DAYS                      *
      *================================================================*
       2100-ADD-CALENDAR.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2100-EXIT.
           COMPUTE WS-CNV-DAYNUM = WS-DAYNUM-1 + DT-DAYS
           IF WS-CNV-DAYNUM < 1 OR WS-CNV-DAYNUM > 3067671
               MOVE 04 TO DT-RETURN-CODE
               MOVE 'RESULT DATE OUT OF RANGE' TO DT-MESSAGE
               GO TO 2100-EXIT.
           PERFORM 7100-DAYNUM-TO-DATE THRU 7100-EXIT
           MOVE WS-CNV-DATE TO DT-RESULT-DATE.
       2100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2200 - ADDB : DATE-1 + DAYS BUSINESS DAYS                      *
      *   DAYS = 0 RETURNS DATE-1 UNCHANGED.  THE START DATE ITSELF    *
      *   NEED NOT BE A BUSINESS DAY.  EACH STEP MOVES TO THE NEXT     *
      *   BUSINESS DAY, SO SAT + 1 = MON, FRI + 1 = MON,               *
      *   FRI + 2 = TUE (DWB 06/95 - AGREED WITH OPS FOR T+3).         *
      *================================================================*
       2200-ADD-BUSINESS.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2200-EXIT.
           PERFORM 6100-EDIT-CALENDAR THRU 6100-EXIT
           IF NOT DT-OK
               GO TO 2200-EXIT.
           MOVE WS-DAYNUM-1 TO WS-CHK-DAYNUM
           IF DT-DAYS < ZERO
               COMPUTE WS-DAYS-LEFT = ZERO - DT-DAYS
               MOVE -1 TO WS-STEP
           ELSE
               MOVE DT-DAYS TO WS-DAYS-LEFT
               MOVE +1 TO WS-STEP
           END-IF
           PERFORM 2210-STEP-ONE-BUS-DAY THRU 2210-EXIT
               UNTIL WS-DAYS-LEFT = ZERO OR NOT DT-OK
           IF DT-OK
               MOVE WS-CHK-DAYNUM TO WS-CNV-DAYNUM
               PERFORM 7100-DAYNUM-TO-DATE THRU 7100-EXIT
               MOVE WS-CNV-DATE TO DT-RESULT-DATE
           END-IF.
       2200-EXIT.
           EXIT.
      *
       2210-STEP-ONE-BUS-DAY.
           MOVE ZERO TO WS-LOOP-GUARD.
       2210-NEXT-DAY.
           ADD WS-STEP TO WS-CHK-DAYNUM
           ADD 1 TO WS-LOOP-GUARD
           IF WS-LOOP-GUARD > 30
               MOVE 12 TO DT-RETURN-CODE
               MOVE 'NO BUSINESS DAY WITHIN 30 DAYS' TO DT-MESSAGE
               GO TO 2210-EXIT.
           PERFORM 7500-CHECK-BUSINESS-DAY THRU 7500-EXIT
           IF NOT WS-BUSINESS-DAY
               GO TO 2210-NEXT-DAY.
           SUBTRACT 1 FROM WS-DAYS-LEFT.
       2210-EXIT.
           EXIT.
      *
      *================================================================*
      * 2300 - NXTB : NEXT BUSINESS DAY AFTER DATE-1                   *
      *================================================================*
       2300-NEXT-BUSINESS.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2300-EXIT.
           PERFORM 6100-EDIT-CALENDAR THRU 6100-EXIT
           IF NOT DT-OK
               GO TO 2300-EXIT.
           MOVE WS-DAYNUM-1 TO WS-CHK-DAYNUM
           MOVE +1 TO WS-STEP
           MOVE 1  TO WS-DAYS-LEFT
           PERFORM 2210-STEP-ONE-BUS-DAY THRU 2210-EXIT
           IF DT-OK
               MOVE WS-CHK-DAYNUM TO WS-CNV-DAYNUM
               PERFORM 7100-DAYNUM-TO-DATE THRU 7100-EXIT
               MOVE WS-CNV-DATE TO DT-RESULT-DATE
           END-IF.
       2300-EXIT.
           EXIT.
      *
      *================================================================*
      * 2400 - PRVB : PREVIOUS BUSINESS DAY BEFORE DATE-1              *
      *================================================================*
       2400-PREV-BUSINESS.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2400-EXIT.
           PERFORM 6100-EDIT-CALENDAR THRU 6100-EXIT
           IF NOT DT-OK
               GO TO 2400-EXIT.
           MOVE WS-DAYNUM-1 TO WS-CHK-DAYNUM
           MOVE -1 TO WS-STEP
           MOVE 1  TO WS-DAYS-LEFT
           PERFORM 2210-STEP-ONE-BUS-DAY THRU 2210-EXIT
           IF DT-OK
               MOVE WS-CHK-DAYNUM TO WS-CNV-DAYNUM
               PERFORM 7100-DAYNUM-TO-DATE THRU 7100-EXIT
               MOVE WS-CNV-DATE TO DT-RESULT-DATE
           END-IF.
       2400-EXIT.
           EXIT.
      *
      *================================================================*
      * 2500 - BUSD : IS DATE-1 A BUSINESS DAY                         *
      *================================================================*
       2500-IS-BUSINESS.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2500-EXIT.
           PERFORM 6100-EDIT-CALENDAR THRU 6100-EXIT
           IF NOT DT-OK
               GO TO 2500-EXIT.
           MOVE WS-DAYNUM-1 TO WS-CHK-DAYNUM
           PERFORM 7500-CHECK-BUSINESS-DAY THRU 7500-EXIT
           IF WS-BUSINESS-DAY
               MOVE 'Y' TO DT-RESULT-FLAG
           ELSE
               MOVE 'N' TO DT-RESULT-FLAG
               IF WS-IS-HOLIDAY
                   MOVE 'HOLIDAY' TO DT-MESSAGE
               ELSE
                   MOVE 'WEEKEND' TO DT-MESSAGE
               END-IF
           END-IF.
       2500-EXIT.
           EXIT.
      *
      *================================================================*
      * 2600 - DIFC : CALENDAR DAYS DATE-2 MINUS DATE-1                *
      *================================================================*
       2600-DIFF-CALENDAR.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2600-EXIT.
           PERFORM 6050-EDIT-DATE-2 THRU 6050-EXIT
           IF NOT DT-OK
               GO TO 2600-EXIT.
           COMPUTE DT-RESULT-NUM = WS-DAYNUM-2 - WS-DAYNUM-1.
       2600-EXIT.
           EXIT.
      *
      *================================================================*
      * 2700 - DIFB : BUSINESS DAYS FROM DATE-1 TO DATE-2              *
      *   COUNTS BUSINESS DAYS D WITH DATE-1 < D <= DATE-2.            *
      *   NEGATIVE WHEN DATE-2 < DATE-1 (DATE-2 < D <= DATE-1).        *
      *================================================================*
       2700-DIFF-BUSINESS.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2700-EXIT.
           PERFORM 6050-EDIT-DATE-2 THRU 6050-EXIT
           IF NOT DT-OK
               GO TO 2700-EXIT.
           PERFORM 6100-EDIT-CALENDAR THRU 6100-EXIT
           IF NOT DT-OK
               GO TO 2700-EXIT.
           MOVE ZERO TO WS-DAY-COUNT
           IF WS-DAYNUM-2 > WS-DAYNUM-1
               MOVE WS-DAYNUM-1 TO WS-CHK-DAYNUM
               PERFORM 2710-COUNT-FORWARD THRU 2710-EXIT
                   UNTIL WS-CHK-DAYNUM NOT < WS-DAYNUM-2
               MOVE WS-DAY-COUNT TO DT-RESULT-NUM
           ELSE
               MOVE WS-DAYNUM-2 TO WS-CHK-DAYNUM
               PERFORM 2710-COUNT-FORWARD THRU 2710-EXIT
                   UNTIL WS-CHK-DAYNUM NOT < WS-DAYNUM-1
               COMPUTE DT-RESULT-NUM = ZERO - WS-DAY-COUNT
           END-IF.
       2700-EXIT.
           EXIT.
      *
       2710-COUNT-FORWARD.
           ADD 1 TO WS-CHK-DAYNUM
           PERFORM 7500-CHECK-BUSINESS-DAY THRU 7500-EXIT
           IF WS-BUSINESS-DAY
               ADD 1 TO WS-DAY-COUNT.
       2710-EXIT.
           EXIT.
      *
      *================================================================*
      * 2800 - D360 : 30/360 DAY COUNT (BOND BASIS)          CHG00588  *
      *   D1 = 31 -> 30.  D2 = 31 AND D1 (ADJUSTED) = 30 -> 30.        *
      *   DAYS = 360*(Y2-Y1) + 30*(M2-M1) + (D2-D1)                    *
      *================================================================*
       2800-DAYS-360.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2800-EXIT.
           PERFORM 6050-EDIT-DATE-2 THRU 6050-EXIT
           IF NOT DT-OK
               GO TO 2800-EXIT.
           MOVE DT-DATE-1 TO WS-360-DATE-1
           MOVE DT-DATE-2 TO WS-360-DATE-2
           MOVE WS-360-D1 TO WS-360-DD1
           MOVE WS-360-D2 TO WS-360-DD2
           IF WS-360-DD1 = 31
               MOVE 30 TO WS-360-DD1
           END-IF
           IF WS-360-DD2 = 31 AND WS-360-DD1 = 30
               MOVE 30 TO WS-360-DD2
           END-IF
           COMPUTE DT-RESULT-NUM =
                   (WS-360-Y2 - WS-360-Y1) * 360
                 + (WS-360-M2 - WS-360-M1) * 30
                 + (WS-360-DD2 - WS-360-DD1).
       2800-EXIT.
           EXIT.
      *
      *================================================================*
      * 2900 - DOW : DAY OF WEEK 1=MONDAY ... 7=SUNDAY                 *
      *================================================================*
       2900-DAY-OF-WEEK.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 2900-EXIT.
           MOVE WS-DAYNUM-1 TO WS-CHK-DAYNUM
           PERFORM 7400-DAY-OF-WEEK THRU 7400-EXIT
           MOVE WS-CHK-DOW TO DT-RESULT-NUM.
       2900-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - EOM : LAST CALENDAR DAY OF THE MONTH          CHG11544  *
      *================================================================*
       3000-END-OF-MONTH.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 3000-EXIT.
           MOVE DT-DATE-1 TO WS-CNV-DATE
           MOVE WS-CNV-CCYY TO WS-CHK-CCYY
           PERFORM 7300-CHECK-LEAP THRU 7300-EXIT
           MOVE WS-MONTH-DAYS (WS-CNV-MM) TO WS-CNV-DD
           IF WS-CNV-MM = 2 AND WS-LEAP-YEAR
               MOVE 29 TO WS-CNV-DD
           END-IF
           MOVE WS-CNV-DATE TO DT-RESULT-DATE.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 3100 - LBDM : LAST BUSINESS DAY OF THE MONTH         CHG21877  *
      *================================================================*
       3100-LAST-BUS-MONTH.
           PERFORM 3000-END-OF-MONTH THRU 3000-EXIT
           IF NOT DT-OK
               GO TO 3100-EXIT
           END-IF
           PERFORM 6100-EDIT-CALENDAR THRU 6100-EXIT
           IF NOT DT-OK
               GO TO 3100-EXIT
           END-IF
           MOVE DT-RESULT-DATE TO WS-CNV-DATE
           PERFORM 7000-DATE-TO-DAYNUM THRU 7000-EXIT
           MOVE WS-CNV-DAYNUM TO WS-CHK-DAYNUM
           MOVE ZERO TO WS-LOOP-GUARD
           PERFORM 7500-CHECK-BUSINESS-DAY THRU 7500-EXIT
           PERFORM UNTIL WS-BUSINESS-DAY OR WS-LOOP-GUARD > 30
               SUBTRACT 1 FROM WS-CHK-DAYNUM
               ADD 1 TO WS-LOOP-GUARD
               PERFORM 7500-CHECK-BUSINESS-DAY THRU 7500-EXIT
           END-PERFORM
           IF WS-BUSINESS-DAY
               MOVE WS-CHK-DAYNUM TO WS-CNV-DAYNUM
               PERFORM 7100-DAYNUM-TO-DATE THRU 7100-EXIT
               MOVE WS-CNV-DATE TO DT-RESULT-DATE
           ELSE
               MOVE ZERO TO DT-RESULT-DATE
               MOVE 12 TO DT-RETURN-CODE
               MOVE 'NO BUSINESS DAY IN MONTH' TO DT-MESSAGE
           END-IF.
       3100-EXIT.
           EXIT.
      *
      *================================================================*
      * 3200 - JUL : CCYYDDD                                 CHG11544  *
      *================================================================*
       3200-JULIAN.
           PERFORM 6000-EDIT-DATE-1 THRU 6000-EXIT
           IF NOT DT-OK
               GO TO 3200-EXIT.
           MOVE DT-DATE-1 TO WS-CNV-DATE
           MOVE WS-CNV-CCYY TO WS-J-YEAR
           PERFORM 7050-JAN1-DAYNUM THRU 7050-EXIT
           COMPUTE WS-DOY = WS-DAYNUM-1 - WS-J-DAYNUM + 1
           COMPUTE DT-RESULT-NUM = WS-CNV-CCYY * 1000 + WS-DOY.
       3200-EXIT.
           EXIT.
      *
      *================================================================*
      * 3300 - W2Y4 : WINDOW A 2-DIGIT YEAR                  CHG04471  *
      *   BOND DESK FEED STILL SENDS YYMMDD.  YY < 50 -> 20YY,         *
      *   YY >= 50 -> 19YY.  AGREED WITH FI DESK 11/98 - REVIEW 2049.  *
      *================================================================*
       3300-WINDOW-YEAR.
           IF DT-DATE-6 NOT NUMERIC
               MOVE 04 TO DT-RETURN-CODE
               MOVE 'DATE-6 NOT NUMERIC' TO DT-MESSAGE
               GO TO 3300-EXIT
           END-IF
           MOVE DT-DATE-6 TO WS-DATE-6
           IF WS-D6-YY < WS-PIVOT-YY
               MOVE 20 TO WS-W2Y4-CENTURY
           ELSE
               MOVE 19 TO WS-W2Y4-CENTURY
           END-IF
           COMPUTE WS-CNV-DATE = WS-W2Y4-CENTURY * 1000000
                               + WS-DATE-6
           PERFORM 7200-VALIDATE-DATE THRU 7200-EXIT
           IF WS-DATE-VALID
               MOVE WS-CNV-DATE TO DT-RESULT-DATE
           ELSE
               MOVE 04 TO DT-RETURN-CODE
               STRING 'INVALID YYMMDD ' DT-DATE-6
                   DELIMITED BY SIZE INTO DT-MESSAGE
           END-IF.
       3300-EXIT.
           EXIT.
      *
      *================================================================*
      * 6000 - COMMON EDITS                                            *
      *================================================================*
       6000-EDIT-DATE-1.
           MOVE DT-DATE-1 TO WS-CNV-DATE
           PERFORM 7200-VALIDATE-DATE THRU 7200-EXIT
           IF WS-DATE-INVALID
               PERFORM 8000-BAD-DATE-1 THRU 8000-EXIT
               GO TO 6000-EXIT.
           PERFORM 7000-DATE-TO-DAYNUM THRU 7000-EXIT
           MOVE WS-CNV-DAYNUM TO WS-DAYNUM-1.
       6000-EXIT.
           EXIT.
      *
       6050-EDIT-DATE-2.
           MOVE DT-DATE-2 TO WS-CNV-DATE
           PERFORM 7200-VALIDATE-DATE THRU 7200-EXIT
           IF WS-DATE-INVALID
               MOVE 04 TO DT-RETURN-CODE
               STRING 'INVALID DATE-2 ' DT-DATE-2
                   DELIMITED BY SIZE INTO DT-MESSAGE
               GO TO 6050-EXIT.
           PERFORM 7000-DATE-TO-DAYNUM THRU 7000-EXIT
           MOVE WS-CNV-DAYNUM TO WS-DAYNUM-2.
       6050-EXIT.
           EXIT.
      *
       6100-EDIT-CALENDAR.
           IF DT-CALENDAR = SPACES OR LOW-VALUES
               MOVE 'NYSE' TO WS-CAL-CODE
           ELSE
               MOVE DT-CALENDAR TO WS-CAL-CODE
           END-IF
           IF NOT WS-CAL-VALID
               MOVE 12 TO DT-RETURN-CODE
               STRING 'UNKNOWN CALENDAR ' DT-CALENDAR
                   DELIMITED BY SIZE INTO DT-MESSAGE
               GO TO 6100-EXIT.
           IF NOT WS-HOLIDAYS-LOADED
               MOVE 12 TO DT-RETURN-CODE
               MOVE 'HOLIDAY FILE NOT AVAILABLE' TO DT-MESSAGE.
       6100-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - CCYYMMDD (WS-CNV-DATE) TO DAY NUMBER (WS-CNV-DAYNUM)    *
      *   DAY 1 = 01/01/1601.  DATE MUST ALREADY BE VALIDATED.         *
      *================================================================*
       7000-DATE-TO-DAYNUM.
           MOVE WS-CNV-CCYY TO WS-J-YEAR
           PERFORM 7050-JAN1-DAYNUM THRU 7050-EXIT
           MOVE WS-CNV-CCYY TO WS-CHK-CCYY
           PERFORM 7300-CHECK-LEAP THRU 7300-EXIT
           MOVE WS-CNV-MM TO WS-MM-SUB
           MOVE ZERO TO WS-LEAP-ADJ
           IF WS-LEAP-YEAR AND WS-CNV-MM > 2
               MOVE 1 TO WS-LEAP-ADJ.
           COMPUTE WS-CNV-DAYNUM = WS-J-DAYNUM
                                 + WS-CUM-DAYS (WS-MM-SUB)
                                 + WS-LEAP-ADJ
                                 + WS-CNV-DD - 1.
       7000-EXIT.
           EXIT.
      *
      *    DAY NUMBER OF JANUARY 1ST OF YEAR WS-J-YEAR
       7050-JAN1-DAYNUM.
           COMPUTE WS-Y1 = WS-J-YEAR - 1601
           DIVIDE WS-Y1 BY 4   GIVING WS-Q4
           DIVIDE WS-Y1 BY 100 GIVING WS-Q100
           DIVIDE WS-Y1 BY 400 GIVING WS-Q400
           COMPUTE WS-J-DAYNUM = WS-Y1 * 365
                               + WS-Q4 - WS-Q100 + WS-Q400 + 1.
       7050-EXIT.
           EXIT.
      *
      *================================================================*
      * 7100 - DAY NUMBER (WS-CNV-DAYNUM) TO CCYYMMDD (WS-CNV-DATE)    *
      *================================================================*
       7100-DAYNUM-TO-DATE.
      *    ESTIMATE THE YEAR (146097 DAYS PER 400 YEARS) THEN ADJUST
           COMPUTE WS-QUOT = ((WS-CNV-DAYNUM - 1) * 400) / 146097
           COMPUTE WS-J-YEAR = 1601 + WS-QUOT
           PERFORM 7050-JAN1-DAYNUM THRU 7050-EXIT.
       7110-ADJUST-DOWN.
           IF WS-J-DAYNUM > WS-CNV-DAYNUM
               SUBTRACT 1 FROM WS-J-YEAR
               PERFORM 7050-JAN1-DAYNUM THRU 7050-EXIT
               GO TO 7110-ADJUST-DOWN.
       7120-ADJUST-UP.
           MOVE WS-J-DAYNUM TO WS-J-SAVE-DAYNUM
           ADD 1 TO WS-J-YEAR
           PERFORM 7050-JAN1-DAYNUM THRU 7050-EXIT
           IF WS-J-DAYNUM NOT > WS-CNV-DAYNUM
               GO TO 7120-ADJUST-UP.
           SUBTRACT 1 FROM WS-J-YEAR
           MOVE WS-J-SAVE-DAYNUM TO WS-J-DAYNUM
      *    DAY OF YEAR, THEN MONTH FROM THE CUMULATIVE TABLE
           COMPUTE WS-DOY = WS-CNV-DAYNUM - WS-J-DAYNUM + 1
           MOVE WS-J-YEAR TO WS-CHK-CCYY
           PERFORM 7300-CHECK-LEAP THRU 7300-EXIT
           MOVE 12 TO WS-MM-SUB.
       7130-FIND-MONTH.
           MOVE ZERO TO WS-LEAP-ADJ
           IF WS-LEAP-YEAR AND WS-MM-SUB > 2
               MOVE 1 TO WS-LEAP-ADJ.
           IF WS-CUM-DAYS (WS-MM-SUB) + WS-LEAP-ADJ NOT < WS-DOY
               SUBTRACT 1 FROM WS-MM-SUB
               GO TO 7130-FIND-MONTH.
           MOVE WS-J-YEAR TO WS-CNV-CCYY
           MOVE WS-MM-SUB TO WS-CNV-MM
           COMPUTE WS-CNV-DD = WS-DOY - WS-CUM-DAYS (WS-MM-SUB)
                             - WS-LEAP-ADJ.
       7100-EXIT.
           EXIT.
      *
      *================================================================*
      * 7200 - VALIDATE WS-CNV-DATE                                    *
      *================================================================*
       7200-VALIDATE-DATE.
           MOVE 'N' TO WS-DATE-VALID-SW
           IF WS-CNV-DATE NOT NUMERIC
               GO TO 7200-EXIT.
           IF WS-CNV-CCYY < 1601
               GO TO 7200-EXIT.
           IF WS-CNV-MM < 1 OR WS-CNV-MM > 12
               GO TO 7200-EXIT.
           MOVE WS-CNV-CCYY TO WS-CHK-CCYY
           PERFORM 7300-CHECK-LEAP THRU 7300-EXIT
           MOVE WS-MONTH-DAYS (WS-CNV-MM) TO WS-DIM
           IF WS-CNV-MM = 2 AND WS-LEAP-YEAR
               MOVE 29 TO WS-DIM.
           IF WS-CNV-DD < 1 OR WS-CNV-DD > WS-DIM
               GO TO 7200-EXIT.
           MOVE 'Y' TO WS-DATE-VALID-SW.
       7200-EXIT.
           EXIT.
      *
      *================================================================*
      * 7300 - LEAP YEAR TEST ON WS-CHK-CCYY                           *
      *   DIVISIBLE BY 4 AND NOT BY 100, OR DIVISIBLE BY 400 (Y2K FIX: *
      *   2000 IS A LEAP YEAR - WAS MISSING UNTIL 02/99)               *
      *================================================================*
       7300-CHECK-LEAP.
           MOVE 'N' TO WS-LEAP-SW
           DIVIDE WS-CHK-CCYY BY 4 GIVING WS-QUOT REMAINDER WS-REM
           IF WS-REM NOT = ZERO
               GO TO 7300-EXIT.
           DIVIDE WS-CHK-CCYY BY 100 GIVING WS-QUOT REMAINDER WS-REM
           IF WS-REM NOT = ZERO
               MOVE 'Y' TO WS-LEAP-SW
               GO TO 7300-EXIT.
           DIVIDE WS-CHK-CCYY BY 400 GIVING WS-QUOT REMAINDER WS-REM
           IF WS-REM = ZERO
               MOVE 'Y' TO WS-LEAP-SW.
       7300-EXIT.
           EXIT.
      *
      *================================================================*
      * 7400 - DAY OF WEEK OF WS-CHK-DAYNUM -> WS-CHK-DOW              *
      *   01/01/1601 WAS A MONDAY.                                     *
      *================================================================*
       7400-DAY-OF-WEEK.
           COMPUTE WS-QUOT = WS-CHK-DAYNUM - 1
           DIVIDE WS-QUOT BY 7 GIVING WS-QUOT REMAINDER WS-REM
           COMPUTE WS-CHK-DOW = WS-REM + 1.
       7400-EXIT.
           EXIT.
      *
      *================================================================*
      * 7500 - IS WS-CHK-DAYNUM A BUSINESS DAY ON WS-CAL-CODE          *
      *================================================================*
       7500-CHECK-BUSINESS-DAY.
           MOVE 'N' TO WS-BUS-DAY-SW
           MOVE 'N' TO WS-HOLIDAY-SW
           PERFORM 7400-DAY-OF-WEEK THRU 7400-EXIT
           IF WS-CHK-DOW > 5
               GO TO 7500-EXIT.
           MOVE WS-CHK-DAYNUM TO WS-CNV-DAYNUM
           PERFORM 7100-DAYNUM-TO-DATE THRU 7100-EXIT
           MOVE WS-CNV-DATE TO WS-CHK-DATE
           PERFORM 7600-HOLIDAY-LOOKUP THRU 7600-EXIT
           IF WS-IS-HOLIDAY
               GO TO 7500-EXIT.
           MOVE 'Y' TO WS-BUS-DAY-SW.
       7520-SPECIAL-CLOSINGS.
      *    NYSE CLOSED THURSDAY 01/09/2025 - NATIONAL DAY OF MOURNING.
      *    2025 HOLIDAY FILE ALREADY DISTRIBUTED TO ALL REGIONS -
      *    OPS REQUEST 01/03/25.  FED/BOND MARKET OPEN.     CHG61094
           IF WS-CHK-DATE = 20250109
               IF WS-CAL-NYSE OR WS-CAL-BOTH
                   MOVE 'N' TO WS-BUS-DAY-SW
                   MOVE 'Y' TO WS-HOLIDAY-SW.
       7500-EXIT.
           EXIT.
      *
      *================================================================*
      * 7600 - LOOK UP WS-CHK-DATE IN THE HOLIDAY TABLE                *
      *================================================================*
       7600-HOLIDAY-LOOKUP.
           MOVE 'N' TO WS-HOLIDAY-SW
           IF WS-HOL-COUNT = ZERO
               GO TO 7600-EXIT.
           IF WS-SERIAL-SEARCH
               PERFORM 7610-SERIAL-CHECK THRU 7610-EXIT
                   VARYING WS-SUB FROM 1 BY 1
                   UNTIL WS-SUB > WS-HOL-COUNT
               GO TO 7600-EXIT.
           SEARCH ALL WS-HOL-ENTRY
               AT END
                   GO TO 7600-EXIT
               WHEN WS-HOL-DATE (HOL-IX) = WS-CHK-DATE
                   SET WS-SUB TO HOL-IX
                   PERFORM 7610-SERIAL-CHECK THRU 7610-EXIT.
       7600-EXIT.
           EXIT.
      *
       7610-SERIAL-CHECK.
           IF WS-HOL-DATE (WS-SUB) NOT = WS-CHK-DATE
               GO TO 7610-EXIT.
           EVALUATE TRUE
               WHEN WS-CAL-NYSE
                   IF WS-HOL-NYSE-SW (WS-SUB) = 'Y'
                       MOVE 'Y' TO WS-HOLIDAY-SW
                   END-IF
               WHEN WS-CAL-FED
                   IF WS-HOL-FED-SW (WS-SUB) = 'Y'
                       MOVE 'Y' TO WS-HOLIDAY-SW
                   END-IF
               WHEN OTHER
                   IF WS-HOL-NYSE-SW (WS-SUB) = 'Y'
                   OR WS-HOL-FED-SW (WS-SUB) = 'Y'
                       MOVE 'Y' TO WS-HOLIDAY-SW
                   END-IF
           END-EVALUATE.
       7610-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - ERROR HELPERS                                           *
      *================================================================*
       8000-BAD-DATE-1.
           MOVE 04 TO DT-RETURN-CODE
           MOVE SPACES TO DT-MESSAGE
           STRING 'INVALID DATE-1 ' DT-DATE-1
               DELIMITED BY SIZE INTO DT-MESSAGE.
       8000-EXIT.
           EXIT.
