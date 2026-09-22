      *================================================================*
      * PROGRAM    : CMD040                                            *
      * TITLE      : DB2 I/O MODULE - MSEC.WHT_RATE (WITHHOLDING TAX)  *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   DIVIDEND / INTEREST WITHHOLDING RATE FOR A HOLDER COUNTRY,   *
      *   ISSUER COUNTRY, INCOME TYPE AND TAX STATUS AS OF A DATE.     *
      *   THE TABLE HOLDS TREATY ROWS AND '**' WILDCARD ROWS; THE MOST *
      *   SPECIFIC ROW IN FORCE ON WH-EFF-DATE WINS:                   *
      *     LEVEL 1  HOLDER + ISSUER                                   *
      *     LEVEL 2  HOLDER + '**'                                     *
      *     LEVEL 3  '**'   + ISSUER                                   *
      *     LEVEL 4  '**'   + '**'      (STATUTORY DEFAULT)            *
      *   WITHIN A LEVEL THE LATEST EFF_DATE WINS.  END_DATE NULL =    *
      *   OPEN ENDED.  ONE SELECT DOES THE WHOLE FALLBACK (SEE 1000).  *
      *                                                                *
      * LINKAGE    : CALL 'CMD040' USING WH-WHTAX-PARMS  (CMWHLNK)     *
      * TABLES     : MSEC.WHT_RATE  (SELECT)                           *
      * PLAN/PKG   : COLLECTION MSCMCOL                                *
      * RETURN     : WH-RETURN-CODE 00 FOUND (WH-MATCH-LEVEL 1-4),     *
      *              04 NOT FOUND, 12 SQL ERROR (WH-SQLCODE)           *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 2003-05-19 KAP  CHG11020  ORIGINAL - W-8BEN TREATY RATES,      *
      *                           FOUR SELECTS IN SEQUENCE             *
      * 2008-11-03 SPA  CHG18377  EFFECTIVE / END DATES ON RATES       *
      * 2012-04-16 JMF  CHG23206  SINGLE SELECT WITH ORDER BY CASE -   *
      *                           CUT DB2 CALLS BY 75% IN CAB300       *
      * 2019-01-07 JMF  CHG36120  RETURN MATCH LEVEL FOR CAR310 REPORT *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMD040.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  05/19/03.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMD040 WORKING STORAGE BEGINS'.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLWHTRT END-EXEC.
      *
       01  WS-HOST-VARS.
           05  WS-REQ-ISO-DATE         PIC X(10).
           05  WS-MATCH-LEVEL          PIC S9(04) COMP.
           05  WS-END-DATE-IND         PIC S9(04) COMP.
      *
       01  WS-DATE-8                   PIC 9(08).
       01  WS-DATE-8-X REDEFINES WS-DATE-8.
           05  WS-D8-CCYY              PIC X(04).
           05  WS-D8-MM                PIC X(02).
           05  WS-D8-DD                PIC X(02).
      *
       01  WS-ERROR-MESSAGE.
           05  WS-ERROR-LEN            PIC S9(04) COMP VALUE +720.
           05  WS-ERROR-TEXT           PIC X(72) OCCURS 10 TIMES
                                       INDEXED BY ERR-IX.
       01  WS-ERROR-TEXT-LEN           PIC S9(09) COMP VALUE +72.
      *
       LINKAGE SECTION.
           COPY CMWHLNK.
      *
       PROCEDURE DIVISION USING WH-WHTAX-PARMS.
      *
       0000-MAINLINE.
           MOVE ZERO   TO WH-RETURN-CODE WH-SQLCODE WH-MATCH-LEVEL
           MOVE ZERO   TO WH-RATE
           MOVE SPACES TO WH-TREATY-FLAG
           IF WH-EFF-DATE NOT NUMERIC OR WH-EFF-DATE = ZERO
               DISPLAY 'CMD040 - INVALID EFFECTIVE DATE ' WH-EFF-DATE
               MOVE 12   TO WH-RETURN-CODE
               MOVE -181 TO WH-SQLCODE
               GOBACK
           END-IF
           MOVE WH-HOLDER-COUNTRY TO WR-HOLDER-CTRY
           MOVE WH-ISSUER-COUNTRY TO WR-ISSUER-CTRY
           MOVE WH-INCOME-TYPE    TO WR-INCOME-TYPE
           MOVE WH-TAX-STATUS     TO WR-TAX-STATUS
           MOVE WH-EFF-DATE       TO WS-DATE-8
           STRING WS-D8-CCYY '-' WS-D8-MM '-' WS-D8-DD
                  DELIMITED BY SIZE INTO WS-REQ-ISO-DATE
           PERFORM 1000-SELECT-RATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - ONE SELECT, MOST SPECIFIC QUALIFYING ROW FIRST          *
      *   THE CASE IN THE SELECT LIST AND THE ORDER BY MUST STAY IN    *
      *   STEP - CAR310 PRINTS THE LEVEL.                              *
      *----------------------------------------------------------------*
       1000-SELECT-RATE.
           EXEC SQL
               SELECT RATE, TREATY_FLAG, CHAR(EFF_DATE, ISO),
                      CHAR(END_DATE, ISO),
                      CASE WHEN HOLDER_CTRY = :WR-HOLDER-CTRY
                            AND ISSUER_CTRY = :WR-ISSUER-CTRY THEN 1
                           WHEN HOLDER_CTRY = :WR-HOLDER-CTRY
                            AND ISSUER_CTRY = '**'            THEN 2
                           WHEN HOLDER_CTRY = '**'
                            AND ISSUER_CTRY = :WR-ISSUER-CTRY THEN 3
                           ELSE 4
                      END
                 INTO :WR-RATE, :WR-TREATY-FLAG, :WR-EFF-DATE,
                      :WR-END-DATE :WS-END-DATE-IND,
                      :WS-MATCH-LEVEL
                 FROM MSEC.WHT_RATE
                WHERE INCOME_TYPE = :WR-INCOME-TYPE
                  AND TAX_STATUS  = :WR-TAX-STATUS
                  AND HOLDER_CTRY IN (:WR-HOLDER-CTRY, '**')
                  AND ISSUER_CTRY IN (:WR-ISSUER-CTRY, '**')
                  AND EFF_DATE   <= DATE(:WS-REQ-ISO-DATE)
                  AND (END_DATE IS NULL
                       OR END_DATE >= DATE(:WS-REQ-ISO-DATE))
                ORDER BY
                      CASE WHEN HOLDER_CTRY = :WR-HOLDER-CTRY
                            AND ISSUER_CTRY = :WR-ISSUER-CTRY THEN 1
                           WHEN HOLDER_CTRY = :WR-HOLDER-CTRY
                            AND ISSUER_CTRY = '**'            THEN 2
                           WHEN HOLDER_CTRY = '**'
                            AND ISSUER_CTRY = :WR-ISSUER-CTRY THEN 3
                           ELSE 4
                      END,
                      EFF_DATE DESC
                FETCH FIRST 1 ROW ONLY
                WITH UR
           END-EXEC
           MOVE SQLCODE TO WH-SQLCODE
           EVALUATE TRUE
               WHEN SQLCODE = ZERO
                   MOVE 00               TO WH-RETURN-CODE
                   MOVE WR-RATE          TO WH-RATE
                   MOVE WR-TREATY-FLAG   TO WH-TREATY-FLAG
                   MOVE WS-MATCH-LEVEL   TO WH-MATCH-LEVEL
               WHEN SQLCODE = +100
                   MOVE 04               TO WH-RETURN-CODE
               WHEN OTHER
                   PERFORM 9000-SQL-ERROR
           END-EVALUATE.
      *
       9000-SQL-ERROR.
           MOVE SQLCODE TO WH-SQLCODE
           MOVE 12      TO WH-RETURN-CODE
           DISPLAY 'CMD040 - SQL ERROR ' WH-HOLDER-COUNTRY '/'
                   WH-ISSUER-COUNTRY '/' WH-INCOME-TYPE '/'
                   WH-TAX-STATUS ' ' WH-EFF-DATE ' SQLCODE ' SQLCODE
           CALL 'DSNTIAR' USING SQLCA WS-ERROR-MESSAGE
                                WS-ERROR-TEXT-LEN
           PERFORM VARYING ERR-IX FROM 1 BY 1 UNTIL ERR-IX > 10
               IF WS-ERROR-TEXT (ERR-IX) NOT = SPACES
                   DISPLAY 'CMD040 - ' WS-ERROR-TEXT (ERR-IX)
               END-IF
           END-PERFORM.
