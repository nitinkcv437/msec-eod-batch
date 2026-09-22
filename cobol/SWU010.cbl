       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWU010.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  10/14/1997.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWU010                                            *
      * TITLE      : ISO 15022 NUMBER FORMATTING (DECIMAL COMMA)       *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   CALLABLE UTILITY FOR THE SWIFT GATEWAY PROGRAMS.  CONVERTS   *
      *   PACKED QUANTITIES AND AMOUNTS TO THE ISO 15022 NUMBER        *
      *   FORMAT (NO LEADING ZEROS, DECIMAL COMMA ALWAYS PRESENT, NO   *
      *   THOUSANDS SEPARATOR) AND BACK.                               *
      *     'QTY '  7000,0000   -> '7000,'   0,5000 -> '0,5'           *
      *     'AMT '  USD 118581,11 -> 'USD118581,11'                    *
      *             CURRENCIES WITHOUT MINOR UNIT (JPY) -> 'JPY1234,'  *
      *     'NUM '  '118581,11' -> 118581,1100                         *
      *   THIS MODULE IS COMPILED WITH DECIMAL-POINT IS COMMA SO THE   *
      *   EDIT PICTURES PRODUCE THE SWIFT FORMAT DIRECTLY.             *
      *                                                                *
      * LINKAGE    : SWFMLNK                                           *
      * CALLED BY  : SWB100 SWR110 SWB400                              *
      * RETURN     : FM-RETURN-CODE (SEE SWFMLNK)                      *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1997-10-14 DWB  CHG03390  ORIGINAL (ISO 15022 MIGRATION)       *
      * 1998-11-02 TLM  CHG04471  Y2K REVIEW - NO DATES, NO CHANGE     *
      * 2001-04-09 KAP  CHG08820  DECIMALIZATION - QTY TO 4 DECIMALS   *
      * 2002-11-18 KAP  CHG09977  'NUM ' FUNCTION FOR THE JOURNAL      *
      * 2009-12-14 SPA  CHG19002  CURRENCY TABLE FOR EUROCLEAR (JPY 0) *
      * 2013-03-04 SPA  CHG25102  BHD/KWD 3 DECIMALS (NOT TRADED YET)  *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWU010'.
      *
      *----------------------------------------------------------------*
      * CURRENCY MINOR UNITS (ISO 4217).  NOT IN THE TABLE -> 2.       *
      *----------------------------------------------------------------*
       01  WS-CCY-DEC-VALUES.
           05  FILLER                  PIC X(04)  VALUE 'USD2'.
           05  FILLER                  PIC X(04)  VALUE 'EUR2'.
           05  FILLER                  PIC X(04)  VALUE 'GBP2'.
           05  FILLER                  PIC X(04)  VALUE 'CHF2'.
           05  FILLER                  PIC X(04)  VALUE 'CAD2'.
           05  FILLER                  PIC X(04)  VALUE 'JPY0'.
           05  FILLER                  PIC X(04)  VALUE 'BHD3'.
           05  FILLER                  PIC X(04)  VALUE 'KWD3'.
       01  WS-CCY-DEC-TABLE REDEFINES WS-CCY-DEC-VALUES.
           05  WS-CCY-ENTRY            OCCURS 8 TIMES
                                       INDEXED BY WS-CCY-IDX.
               10  WS-CCY-CODE         PIC X(03).
               10  WS-CCY-DECIMALS     PIC 9(01).
      *
       01  WS-LIMITS.
           05  WS-MAX-AMOUNT           PIC S9(15)V99 COMP-3
                                       VALUE 999999999999999,99.
           05  WS-MAX-QTY              PIC S9(11)V9(04) COMP-3
                                       VALUE 99999999999,9999.
      *
       01  WS-WORK.
           05  WS-QTY-ABS              PIC S9(11)V9(04) COMP-3.
           05  WS-AMT-ABS              PIC S9(15)V99    COMP-3.
           05  WS-AMT-WHOLE            PIC 9(15).
           05  WS-AMT-3DEC             PIC 9(15)V999.
           05  WS-QTY-EDIT             PIC Z(10)9,9999.
           05  WS-AMT-EDIT             PIC Z(14)9,99.
           05  WS-AMT-EDIT-0           PIC Z(14)9.
           05  WS-AMT-EDIT-3           PIC Z(14)9,999.
           05  WS-EDIT-TEXT            PIC X(24).
           05  WS-OUT-TEXT             PIC X(35).
           05  WS-DECIMALS             PIC 9(01).
           05  WS-LEAD                 PIC S9(04) COMP.
           05  WS-LEN                  PIC S9(04) COMP.
           05  WS-POS                  PIC S9(04) COMP.
           05  WS-COMMA-POS            PIC S9(04) COMP.
           05  WS-OUT-LEN              PIC S9(04) COMP.
      *
      *    'NUM ' WORK AREAS
       01  WS-NUM-WORK.
           05  WS-NUM-TEXT             PIC X(35).
           05  WS-NUM-INT-TXT          PIC X(18).
           05  WS-NUM-FRC-TXT          PIC X(04).
           05  WS-NUM-REST             PIC X(18).
           05  WS-NUM-INT-LEN          PIC S9(04) COMP.
           05  WS-NUM-FRC-LEN          PIC S9(04) COMP.
           05  WS-NUM-COMMAS           PIC S9(04) COMP.
           05  WS-NUM-INT              PIC 9(15).
           05  WS-NUM-FRC              PIC 9(04).
           05  WS-NUM-FRC-V REDEFINES WS-NUM-FRC
                                       PIC V9(04).
           05  WS-NUM-SIGN             PIC X(01).
      *
       01  WS-CALL-COUNT               PIC S9(09) COMP VALUE ZERO.
      *
       LINKAGE SECTION.
       COPY SWFMLNK.
      *
       PROCEDURE DIVISION USING FM-PARMS.
      *
       0000-MAINLINE.
           ADD 1                       TO WS-CALL-COUNT
           MOVE ZERO                   TO FM-RETURN-CODE
           EVALUATE FM-FUNCTION
               WHEN 'QTY '
                   MOVE SPACES         TO FM-TEXT
                   PERFORM 1000-FORMAT-QTY THRU 1000-EXIT
               WHEN 'AMT '
                   MOVE SPACES         TO FM-TEXT
                   PERFORM 2000-FORMAT-AMT THRU 2000-EXIT
               WHEN 'NUM '
                   PERFORM 3000-PARSE-NUMBER THRU 3000-EXIT
               WHEN OTHER
                   MOVE 12             TO FM-RETURN-CODE
                   MOVE ZERO           TO FM-TEXT-LEN
           END-EVALUATE
           GOBACK.
      *
      *================================================================*
      * 1000 - QUANTITY:  INTEGER PART, COMMA, DECIMALS WITHOUT        *
      *        TRAILING ZEROS.  SWIFT DOES NOT CARRY A SIGN ON 36B.    *
      *================================================================*
       1000-FORMAT-QTY.
           IF FM-QTY-IN < ZERO
               COMPUTE WS-QTY-ABS = ZERO - FM-QTY-IN
               MOVE 04                 TO FM-RETURN-CODE
           ELSE
               MOVE FM-QTY-IN          TO WS-QTY-ABS
           END-IF
           MOVE WS-QTY-ABS             TO WS-QTY-EDIT
           MOVE WS-QTY-EDIT            TO WS-EDIT-TEXT
      *    DROP TRAILING ZERO DECIMALS - '7000,0000' BECOMES '7000,'
           MOVE 16                     TO WS-POS
           PERFORM UNTIL WS-POS < 13
                      OR WS-EDIT-TEXT (WS-POS:1) NOT = '0'
               MOVE SPACE              TO WS-EDIT-TEXT (WS-POS:1)
               SUBTRACT 1              FROM WS-POS
           END-PERFORM
           PERFORM 8000-LEFT-JUSTIFY   THRU 8000-EXIT
           MOVE WS-OUT-TEXT            TO FM-TEXT
           MOVE WS-OUT-LEN             TO FM-TEXT-LEN
           MOVE 4                      TO FM-DECIMALS.
       1000-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - AMOUNT:  CCY + NUMBER WITH THE CURRENCY'S MINOR UNITS.  *
      *        CURRENCIES WITHOUT DECIMALS ARE TRUNCATED (THE          *
      *        CUSTODIAN REJECTS DECIMALS ON JPY - CHG19002).          *
      *================================================================*
       2000-FORMAT-AMT.
           IF FM-AMT-IN < ZERO
               COMPUTE WS-AMT-ABS = ZERO - FM-AMT-IN
               MOVE 04                 TO FM-RETURN-CODE
           ELSE
               MOVE FM-AMT-IN          TO WS-AMT-ABS
           END-IF
           IF WS-AMT-ABS > WS-MAX-AMOUNT
               MOVE WS-MAX-AMOUNT      TO WS-AMT-ABS
               MOVE 08                 TO FM-RETURN-CODE
           END-IF
           MOVE 2                      TO WS-DECIMALS
           SET WS-CCY-IDX              TO 1
           SEARCH WS-CCY-ENTRY
               AT END
                   CONTINUE
               WHEN WS-CCY-CODE (WS-CCY-IDX) = FM-CCY
                   MOVE WS-CCY-DECIMALS (WS-CCY-IDX)
                                       TO WS-DECIMALS
           END-SEARCH
           MOVE SPACES                 TO WS-EDIT-TEXT
           EVALUATE WS-DECIMALS
               WHEN 0
                   MOVE WS-AMT-ABS     TO WS-AMT-WHOLE
                   IF WS-AMT-WHOLE NOT = WS-AMT-ABS
                       MOVE 04         TO FM-RETURN-CODE
                   END-IF
                   MOVE WS-AMT-WHOLE   TO WS-AMT-EDIT-0
                   MOVE WS-AMT-EDIT-0  TO WS-EDIT-TEXT (1:15)
                   MOVE ','            TO WS-EDIT-TEXT (16:1)
               WHEN 3
                   MOVE WS-AMT-ABS     TO WS-AMT-3DEC
                   MOVE WS-AMT-3DEC    TO WS-AMT-EDIT-3
                   MOVE WS-AMT-EDIT-3  TO WS-EDIT-TEXT
               WHEN OTHER
                   MOVE WS-AMT-ABS     TO WS-AMT-EDIT
                   MOVE WS-AMT-EDIT    TO WS-EDIT-TEXT
           END-EVALUATE
           PERFORM 8000-LEFT-JUSTIFY   THRU 8000-EXIT
           MOVE SPACES                 TO FM-TEXT
           STRING FM-CCY               DELIMITED BY SIZE
                  WS-OUT-TEXT (1:WS-OUT-LEN)
                                       DELIMITED BY SIZE
                  INTO FM-TEXT
           END-STRING
           COMPUTE FM-TEXT-LEN = WS-OUT-LEN + 3
           MOVE WS-DECIMALS            TO FM-DECIMALS.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - PARSE 'DIGITS,DIGITS' INTO FM-NUM-OUT.  A LEADING 'N'   *
      *        (SWIFT NEGATIVE SIGN) IS HONOURED.  A NUMBER WITHOUT    *
      *        COMMA IS TAKEN AS A WHOLE NUMBER.                       *
      *================================================================*
       3000-PARSE-NUMBER.
           MOVE ZERO                   TO FM-NUM-OUT
           MOVE FM-TEXT                TO WS-NUM-TEXT
           MOVE SPACE                  TO WS-NUM-SIGN
           IF WS-NUM-TEXT (1:1) = 'N'
               MOVE '-'                TO WS-NUM-SIGN
               MOVE FM-TEXT (2:34)     TO WS-NUM-TEXT
           END-IF
           MOVE ZERO                   TO WS-NUM-COMMAS
           INSPECT WS-NUM-TEXT TALLYING WS-NUM-COMMAS FOR ALL ','
           IF WS-NUM-COMMAS > 1
               MOVE 08                 TO FM-RETURN-CODE
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES                 TO WS-NUM-INT-TXT
                                          WS-NUM-FRC-TXT
                                          WS-NUM-REST
           MOVE ZERO                   TO WS-NUM-INT-LEN
                                          WS-NUM-FRC-LEN
           UNSTRING WS-NUM-TEXT DELIMITED BY ',' OR ALL SPACE
               INTO WS-NUM-INT-TXT COUNT IN WS-NUM-INT-LEN
                    WS-NUM-FRC-TXT COUNT IN WS-NUM-FRC-LEN
                    WS-NUM-REST
           END-UNSTRING
           IF WS-NUM-COMMAS = ZERO
               MOVE SPACES             TO WS-NUM-FRC-TXT
               MOVE ZERO               TO WS-NUM-FRC-LEN
           END-IF
           IF WS-NUM-INT-LEN = ZERO
           OR WS-NUM-INT-LEN > 15
               MOVE 08                 TO FM-RETURN-CODE
               GO TO 3000-EXIT
           END-IF
           IF WS-NUM-INT-TXT (1:WS-NUM-INT-LEN) NOT NUMERIC
               MOVE 08                 TO FM-RETURN-CODE
               GO TO 3000-EXIT
           END-IF
           MOVE WS-NUM-INT-TXT (1:WS-NUM-INT-LEN) TO WS-NUM-INT
           IF WS-NUM-FRC-LEN > 4
               MOVE 04                 TO FM-RETURN-CODE
           END-IF
           INSPECT WS-NUM-FRC-TXT REPLACING ALL SPACE BY '0'
           IF WS-NUM-FRC-TXT NOT NUMERIC
               MOVE 08                 TO FM-RETURN-CODE
               GO TO 3000-EXIT
           END-IF
           MOVE WS-NUM-FRC-TXT         TO WS-NUM-FRC
           COMPUTE FM-NUM-OUT = WS-NUM-INT + WS-NUM-FRC-V
           IF WS-NUM-SIGN = '-'
               COMPUTE FM-NUM-OUT = ZERO - FM-NUM-OUT
           END-IF
           MOVE WS-NUM-FRC-LEN         TO FM-DECIMALS.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - REMOVE LEADING SPACES OF WS-EDIT-TEXT -> WS-OUT-TEXT    *
      *================================================================*
       8000-LEFT-JUSTIFY.
           MOVE ZERO                   TO WS-LEAD
           INSPECT WS-EDIT-TEXT TALLYING WS-LEAD FOR LEADING SPACE
           MOVE SPACES                 TO WS-OUT-TEXT
           IF WS-LEAD NOT < 24
               MOVE '0,'               TO WS-OUT-TEXT
               MOVE 2                  TO WS-OUT-LEN
               GO TO 8000-EXIT
           END-IF
           COMPUTE WS-LEN = 24 - WS-LEAD
           MOVE WS-EDIT-TEXT (WS-LEAD + 1:WS-LEN) TO WS-OUT-TEXT
      *    SIGNIFICANT LENGTH = UP TO THE LAST NON-BLANK
           MOVE WS-LEN                 TO WS-OUT-LEN
           PERFORM UNTIL WS-OUT-LEN < 1
                      OR WS-OUT-TEXT (WS-OUT-LEN:1) NOT = SPACE
               SUBTRACT 1              FROM WS-OUT-LEN
           END-PERFORM
      *    ',99' FOR AN AMOUNT BELOW ONE - SWIFT WANTS THE ZERO
           IF WS-OUT-TEXT (1:1) = ','
               MOVE WS-OUT-TEXT        TO WS-EDIT-TEXT
               MOVE SPACES             TO WS-OUT-TEXT
               STRING '0' WS-EDIT-TEXT DELIMITED BY SIZE
                      INTO WS-OUT-TEXT
               END-STRING
               ADD 1                   TO WS-OUT-LEN
           END-IF.
       8000-EXIT.
           EXIT.
