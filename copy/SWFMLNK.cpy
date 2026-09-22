      *================================================================*
      * COPYBOOK   : SWFMLNK                                           *
      * DESCRIPTION: LINKAGE FOR SWU010 - ISO 15022 NUMBER FORMATTING  *
      *             (DECIMAL COMMA).  CALL 'SWU010' USING FM-PARMS.    *
      * FM-FUNCTION 'QTY ' FM-QTY-IN  -> FM-TEXT  E.G. 7000,  12,5     *
      *             'AMT ' FM-CCY + FM-AMT-IN -> FM-TEXT (CCY+AMOUNT)  *
      *                    E.G. USD118581,11   JPY967200958,           *
      *             'NUM ' FM-TEXT (DIGITS, COMMA, DIGITS) ->          *
      *                    FM-NUM-OUT                                  *
      * FM-TEXT-LEN   SIGNIFICANT LENGTH OF FM-TEXT ON RETURN          *
      * FM-RETURN-CODE 00 OK  04 NEGATIVE VALUE SENT AS ABSOLUTE OR    *
      *                DECIMALS DROPPED  08 INVALID TEXT               *
      *                12 INVALID FUNCTION                             *
      *================================================================*
       01  FM-PARMS.
           05  FM-FUNCTION             PIC X(04).
           05  FM-CCY                  PIC X(03).
           05  FM-QTY-IN               PIC S9(11)V9(04) COMP-3.
           05  FM-AMT-IN               PIC S9(15)V99    COMP-3.
           05  FM-TEXT                 PIC X(35).
           05  FM-TEXT-LEN             PIC S9(04)       COMP.
           05  FM-NUM-OUT              PIC S9(15)V9(04) COMP-3.
           05  FM-DECIMALS             PIC 9(01).
           05  FM-RETURN-CODE          PIC 9(02).
               88  FM-OK                         VALUE 00.
               88  FM-WARNING                    VALUE 04.
               88  FM-INVALID                    VALUE 08.
