      *================================================================*
      * COPYBOOK   : SRSTMT                                            *
      * DESCRIPTION: MONTH-END CLIENT STATEMENT EXTRACT.  SRB700.      *
      *              DSN MSEC.PROD.SR.STMTEXT(+1)                      *
      *              RECORD TYPES: 'A' ACCOUNT HEADER, 'P' POSITION,   *
      *              'C' CASH, 'T' ACCOUNT TOTAL                       *
      * RECFM/LRECL: FB / 200                                          *
      *================================================================*
       01  STM-STATEMENT-REC.
           05  STM-ACCT-NO             PIC X(10).
           05  STM-REC-TYPE            PIC X(01).
               88  STM-ACCT-HEADER               VALUE 'A'.
               88  STM-POSITION-LINE             VALUE 'P'.
               88  STM-CASH-LINE                 VALUE 'C'.
               88  STM-ACCT-TOTAL                VALUE 'T'.
           05  STM-SEQ-NO              PIC 9(05).
           05  STM-PERIOD-END          PIC 9(08).
           05  STM-BODY                PIC X(176).
           05  STM-HDR-BODY      REDEFINES STM-BODY.
               10  STM-NAME            PIC X(40).
               10  STM-ADDR-1          PIC X(30).
               10  STM-ADDR-2          PIC X(30).
               10  STM-BRANCH          PIC X(03).
               10  STM-REP             PIC X(04).
               10  STM-BASE-CCY        PIC X(03).
               10  FILLER              PIC X(66).
           05  STM-POS-BODY      REDEFINES STM-BODY.
               10  STM-CUSIP           PIC X(09).
               10  STM-SEC-DESC        PIC X(40).
               10  STM-QTY             PIC S9(11)V9(04) COMP-3.
               10  STM-PRICE           PIC S9(09)V9(08) COMP-3.
               10  STM-MKT-VALUE       PIC S9(15)V99    COMP-3.
               10  STM-COST-BASIS      PIC S9(15)V99    COMP-3.
               10  STM-UNRLZD-PL       PIC S9(15)V99    COMP-3.
               10  STM-EST-INCOME      PIC S9(13)V99    COMP-3.
               10  FILLER              PIC X(75).
           05  STM-CASH-BODY     REDEFINES STM-BODY.
               10  STM-CASH-CCY        PIC X(03).
               10  STM-CASH-BALANCE    PIC S9(15)V99    COMP-3.
               10  STM-CASH-INCOME-YTD PIC S9(15)V99    COMP-3.
               10  STM-CASH-WHT-YTD    PIC S9(13)V99    COMP-3.
               10  FILLER              PIC X(147).
           05  STM-TOT-BODY      REDEFINES STM-BODY.
               10  STM-TOT-MKT-VALUE   PIC S9(15)V99    COMP-3.
               10  STM-TOT-CASH        PIC S9(15)V99    COMP-3.
               10  STM-TOT-EQUITY      PIC S9(15)V99    COMP-3.
               10  STM-TOT-POSITIONS   PIC S9(05)       COMP-3.
               10  FILLER              PIC X(146).
