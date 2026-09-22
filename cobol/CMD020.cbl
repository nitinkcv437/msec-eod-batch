      *================================================================*
      * PROGRAM    : CMD020                                            *
      * TITLE      : DB2 I/O MODULE - MSEC.SECURITY_PRICE              *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   CLOSING PRICE LOOKUP.                                        *
      *     'GET '  PRICE FOR EXACTLY CUSIP + PL-PRICE-DATE            *
      *     'GETL'  LATEST PRICE ON OR BEFORE PL-PRICE-DATE - USED BY  *
      *             VALUATION SO THAT A SECURITY THAT DID NOT TRADE    *
      *             STILL PRICES (CALLER DECIDES IF IT IS STALE FROM   *
      *             PL-ACTUAL-DATE).                                   *
      *   DATES ARE PASSED AS CCYYMMDD AND CONVERTED TO / FROM THE DB2 *
      *   ISO FORMAT 'YYYY-MM-DD'.                                     *
      *                                                                *
      * LINKAGE    : CALL 'CMD020' USING PL-PRICE-PARMS  (CMPRLNK)     *
      * TABLES     : MSEC.SECURITY_PRICE  (SELECT)                     *
      * PLAN/PKG   : COLLECTION MSCMCOL                                *
      * RETURN     : PL-RETURN-CODE 00 FOUND, 04 NOT FOUND,            *
      *              12 SQL ERROR (PL-SQLCODE)                         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1996-05-13 DWB  CHG02240  ORIGINAL - PRICES MOVED TO DB2       *
      * 1998-11-02 TLM  CHG04471  Y2K - CCYYMMDD <-> ISO               *
      * 2001-04-09 DWB  CHG08130  DECIMALIZATION - PRICE DEC(17,8)     *
      * 2007-10-01 KAP  CHG16633  'GETL' REWRITTEN AS ONE SELECT WITH  *
      *                           CORRELATED MAX() - WAS A CURSOR      *
      *                           ORDER BY PRICE_DATE DESC (DBA REQ.   *
      *                           - INDEX-ONLY ACCESS ON XSECPR01)     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMD020.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  05/13/96.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMD020 WORKING STORAGE BEGINS'.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLSECPR END-EXEC.
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
       01  WS-CALL-COUNTS              COMP.
           05  WS-GET-CALLS            PIC S9(09) VALUE ZERO.
           05  WS-GETL-CALLS           PIC S9(09) VALUE ZERO.
      *
       01  WS-ERROR-MESSAGE.
           05  WS-ERROR-LEN            PIC S9(04) COMP VALUE +720.
           05  WS-ERROR-TEXT           PIC X(72) OCCURS 10 TIMES
                                       INDEXED BY ERR-IX.
       01  WS-ERROR-TEXT-LEN           PIC S9(09) COMP VALUE +72.
      *
       LINKAGE SECTION.
           COPY CMPRLNK.
      *
       PROCEDURE DIVISION USING PL-PRICE-PARMS.
      *
       0000-MAINLINE.
           MOVE ZERO   TO PL-RETURN-CODE PL-SQLCODE
           MOVE ZERO   TO PL-PRICE PL-ACTUAL-DATE
           MOVE SPACES TO PL-PRICE-SOURCE PL-PRICE-CCY
           IF PL-PRICE-DATE NOT NUMERIC OR PL-PRICE-DATE = ZERO
               DISPLAY 'CMD020 - INVALID PRICE DATE ' PL-PRICE-DATE
               MOVE 12   TO PL-RETURN-CODE
               MOVE -181 TO PL-SQLCODE
               GOBACK
           END-IF
           MOVE PL-CUSIP      TO PR-CUSIP
           MOVE PL-PRICE-DATE TO WS-DATE-8
           PERFORM 7100-CCYYMMDD-TO-ISO
           MOVE WS-ISO-DATE   TO WS-REQ-ISO-DATE
           EVALUATE PL-FUNCTION
               WHEN 'GET '
                   ADD 1 TO WS-GET-CALLS
                   PERFORM 1000-GET-EXACT
               WHEN 'GETL'
                   ADD 1 TO WS-GETL-CALLS
                   PERFORM 2000-GET-LATEST
               WHEN OTHER
                   DISPLAY 'CMD020 - INVALID FUNCTION ' PL-FUNCTION
                   MOVE 12 TO PL-RETURN-CODE
           END-EVALUATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - EXACT DATE                                              *
      *----------------------------------------------------------------*
       1000-GET-EXACT.
           EXEC SQL
               SELECT CHAR(PRICE_DATE, ISO), PRICE, SOURCE, CCY
                 INTO :PR-PRICE-DATE, :PR-PRICE, :PR-SOURCE, :PR-CCY
                 FROM MSEC.SECURITY_PRICE
                WHERE CUSIP      = :PR-CUSIP
                  AND PRICE_DATE = DATE(:WS-REQ-ISO-DATE)
                WITH UR
           END-EXEC
           PERFORM 8000-CHECK-SQLCODE.
      *
      *----------------------------------------------------------------*
      * 2000 - LATEST ON OR BEFORE THE REQUESTED DATE (CHG16633)       *
      *----------------------------------------------------------------*
       2000-GET-LATEST.
           EXEC SQL
               SELECT CHAR(P.PRICE_DATE, ISO), P.PRICE, P.SOURCE,
                      P.CCY
                 INTO :PR-PRICE-DATE, :PR-PRICE, :PR-SOURCE, :PR-CCY
                 FROM MSEC.SECURITY_PRICE P
                WHERE P.CUSIP      = :PR-CUSIP
                  AND P.PRICE_DATE =
                      (SELECT MAX(P2.PRICE_DATE)
                         FROM MSEC.SECURITY_PRICE P2
                        WHERE P2.CUSIP       = P.CUSIP
                          AND P2.PRICE_DATE <= DATE(:WS-REQ-ISO-DATE))
                WITH UR
           END-EXEC
           PERFORM 8000-CHECK-SQLCODE.
      *
      *----------------------------------------------------------------*
      * 7000 - DATE CONVERSIONS                                        *
      *----------------------------------------------------------------*
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
      *----------------------------------------------------------------*
      * 8000 - SQLCODE HANDLING                                        *
      *----------------------------------------------------------------*
       8000-CHECK-SQLCODE.
           MOVE SQLCODE TO PL-SQLCODE
           EVALUATE TRUE
               WHEN SQLCODE = ZERO
                   MOVE 00               TO PL-RETURN-CODE
                   MOVE PR-PRICE         TO PL-PRICE
                   MOVE PR-SOURCE        TO PL-PRICE-SOURCE
                   MOVE PR-CCY           TO PL-PRICE-CCY
                   MOVE PR-PRICE-DATE    TO WS-ISO-DATE
                   PERFORM 7000-ISO-TO-CCYYMMDD
                   MOVE WS-DATE-8        TO PL-ACTUAL-DATE
               WHEN SQLCODE = +100
                   MOVE 04               TO PL-RETURN-CODE
               WHEN OTHER
                   PERFORM 9000-SQL-ERROR
           END-EVALUATE.
      *
       9000-SQL-ERROR.
           MOVE SQLCODE TO PL-SQLCODE
           MOVE 12      TO PL-RETURN-CODE
           DISPLAY 'CMD020 - SQL ERROR, FUNCTION ' PL-FUNCTION
                   ' CUSIP ' PL-CUSIP ' DATE ' PL-PRICE-DATE
                   ' SQLCODE ' SQLCODE
           CALL 'DSNTIAR' USING SQLCA WS-ERROR-MESSAGE
                                WS-ERROR-TEXT-LEN
           PERFORM VARYING ERR-IX FROM 1 BY 1 UNTIL ERR-IX > 10
               IF WS-ERROR-TEXT (ERR-IX) NOT = SPACES
                   DISPLAY 'CMD020 - ' WS-ERROR-TEXT (ERR-IX)
               END-IF
           END-PERFORM.
