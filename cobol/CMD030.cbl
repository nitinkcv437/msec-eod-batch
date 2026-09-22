      *================================================================*
      * PROGRAM    : CMD030                                            *
      * TITLE      : DB2 I/O MODULE - MSEC.FX_RATE                     *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   FX RATE LOOKUP (USD PER ONE UNIT OF CURRENCY) FOR CMU040.    *
      *     'GETL'  LATEST RATE ON OR BEFORE FD-RATE-DATE (CURSOR,     *
      *             NEWEST ROW FIRST)                                  *
      *     'GET '  EXACT CURRENCY + DATE                              *
      *   TREASURY LOADS ONE ROW PER CURRENCY PER BUSINESS DAY THROUGH *
      *   MSCMD005 (DSNUTILB LOAD RESUME).                             *
      *                                                                *
      * LINKAGE    : CALL 'CMD030' USING FD-FXRATE-PARMS (CMFXDLNK)    *
      * TABLES     : MSEC.FX_RATE  (SELECT)                            *
      * PLAN/PKG   : COLLECTION MSCMCOL                                *
      * RETURN     : FD-RETURN-CODE 00 FOUND, 04 NOT FOUND,            *
      *              12 SQL ERROR (FD-SQLCODE)                         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 2004-10-25 KAP  CHG12690  ORIGINAL - FX RATES MOVED FROM VSAM  *
      *                           FXRATES FILE TO DB2                  *
      * 2009-06-15 SPA  CHG19204  CURSOR CLOSED ON ALL PATHS (-502 ON  *
      *                           SECOND CALL AFTER NOT FOUND)         *
      * 2015-09-08 SPA  CHG28815  RETURN ACTUAL RATE DATE FOR STALE    *
      *                           RATE CHECK IN CMU040                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMD030.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  10/25/04.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMD030 WORKING STORAGE BEGINS'.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLFXRAT END-EXEC.
      *
       01  WS-REQ-ISO-DATE             PIC X(10).
       01  WS-ISO-DATE                 PIC X(10).
       01  WS-ISO-DATE-R REDEFINES WS-ISO-DATE.
           05  WS-ISO-CCYY             PIC X(04).
           05  WS-ISO-SEP1             PIC X(01).
           05  WS-ISO-MM               PIC X(02).
           05  WS-ISO-SEP2             PIC X(01).
           05  WS-ISO-DD               PIC X(02).
       01  WS-DATE-8                   PIC 9(08).
       01  WS-DATE-8-X REDEFINES WS-DATE-8.
           05  WS-D8-CCYY              PIC X(04).
           05  WS-D8-MM                PIC X(02).
           05  WS-D8-DD                PIC X(02).
      *
       01  WS-CURSOR-SW                PIC X(01)  VALUE 'N'.
           88  WS-CURSOR-OPEN                     VALUE 'Y'.
           88  WS-CURSOR-CLOSED                   VALUE 'N'.
      *
       01  WS-ERROR-MESSAGE.
           05  WS-ERROR-LEN            PIC S9(04) COMP VALUE +720.
           05  WS-ERROR-TEXT           PIC X(72) OCCURS 10 TIMES
                                       INDEXED BY ERR-IX.
       01  WS-ERROR-TEXT-LEN           PIC S9(09) COMP VALUE +72.
      *
           EXEC SQL DECLARE C-FXRATE-LATEST CURSOR FOR
               SELECT CHAR(RATE_DATE, ISO), USD_RATE
                 FROM MSEC.FX_RATE
                WHERE CCY        = :FR-CCY
                  AND RATE_DATE <= DATE(:WS-REQ-ISO-DATE)
                ORDER BY RATE_DATE DESC
                FETCH FIRST 1 ROW ONLY
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
       LINKAGE SECTION.
           COPY CMFXDLNK.
      *
       PROCEDURE DIVISION USING FD-FXRATE-PARMS.
      *
       0000-MAINLINE.
           MOVE ZERO TO FD-RETURN-CODE FD-SQLCODE
           MOVE ZERO TO FD-USD-RATE FD-ACTUAL-DATE
           MOVE FD-CCY       TO FR-CCY
           MOVE FD-RATE-DATE TO WS-DATE-8
           PERFORM 7100-CCYYMMDD-TO-ISO
           MOVE WS-ISO-DATE  TO WS-REQ-ISO-DATE
           EVALUATE FD-FUNCTION
               WHEN 'GETL'
                   PERFORM 1000-GET-LATEST
               WHEN 'GET '
                   PERFORM 2000-GET-EXACT
               WHEN OTHER
                   DISPLAY 'CMD030 - INVALID FUNCTION ' FD-FUNCTION
                   MOVE 12 TO FD-RETURN-CODE
           END-EVALUATE
           GOBACK.
      *
       1000-GET-LATEST.
           EXEC SQL OPEN C-FXRATE-LATEST END-EXEC
           IF SQLCODE NOT = ZERO
               PERFORM 9000-SQL-ERROR
           ELSE
               SET WS-CURSOR-OPEN TO TRUE
               EXEC SQL
                   FETCH C-FXRATE-LATEST
                    INTO :FR-RATE-DATE, :FR-USD-RATE
               END-EXEC
               PERFORM 8000-CHECK-SQLCODE
               IF WS-CURSOR-OPEN
                   EXEC SQL CLOSE C-FXRATE-LATEST END-EXEC
                   SET WS-CURSOR-CLOSED TO TRUE
               END-IF
           END-IF.
      *
       2000-GET-EXACT.
           EXEC SQL
               SELECT CHAR(RATE_DATE, ISO), USD_RATE
                 INTO :FR-RATE-DATE, :FR-USD-RATE
                 FROM MSEC.FX_RATE
                WHERE CCY       = :FR-CCY
                  AND RATE_DATE = DATE(:WS-REQ-ISO-DATE)
                WITH UR
           END-EXEC
           PERFORM 8000-CHECK-SQLCODE.
      *
       7000-ISO-TO-CCYYMMDD.
           MOVE WS-ISO-CCYY TO WS-D8-CCYY
           MOVE WS-ISO-MM   TO WS-D8-MM
           MOVE WS-ISO-DD   TO WS-D8-DD
           IF WS-DATE-8 NOT NUMERIC
               MOVE ZERO TO WS-DATE-8
           END-IF.
      *
       7100-CCYYMMDD-TO-ISO.
           MOVE WS-D8-CCYY  TO WS-ISO-CCYY
           MOVE '-'         TO WS-ISO-SEP1
           MOVE WS-D8-MM    TO WS-ISO-MM
           MOVE '-'         TO WS-ISO-SEP2
           MOVE WS-D8-DD    TO WS-ISO-DD.
      *
       8000-CHECK-SQLCODE.
           MOVE SQLCODE TO FD-SQLCODE
           EVALUATE TRUE
               WHEN SQLCODE = ZERO
                   MOVE 00            TO FD-RETURN-CODE
                   MOVE FR-USD-RATE   TO FD-USD-RATE
                   MOVE FR-RATE-DATE  TO WS-ISO-DATE
                   PERFORM 7000-ISO-TO-CCYYMMDD
                   MOVE WS-DATE-8     TO FD-ACTUAL-DATE
               WHEN SQLCODE = +100
                   MOVE 04            TO FD-RETURN-CODE
               WHEN OTHER
                   PERFORM 9000-SQL-ERROR
           END-EVALUATE.
      *
       9000-SQL-ERROR.
           MOVE SQLCODE TO FD-SQLCODE
           MOVE 12      TO FD-RETURN-CODE
           DISPLAY 'CMD030 - SQL ERROR, FUNCTION ' FD-FUNCTION
                   ' CCY ' FD-CCY ' DATE ' FD-RATE-DATE
                   ' SQLCODE ' SQLCODE
           CALL 'DSNTIAR' USING SQLCA WS-ERROR-MESSAGE
                                WS-ERROR-TEXT-LEN
           PERFORM VARYING ERR-IX FROM 1 BY 1 UNTIL ERR-IX > 10
               IF WS-ERROR-TEXT (ERR-IX) NOT = SPACES
                   DISPLAY 'CMD030 - ' WS-ERROR-TEXT (ERR-IX)
               END-IF
           END-PERFORM
           IF WS-CURSOR-OPEN
               EXEC SQL CLOSE C-FXRATE-LATEST END-EXEC
               SET WS-CURSOR-CLOSED TO TRUE
           END-IF.
