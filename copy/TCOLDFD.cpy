      *================================================================*
      * COPYBOOK   : TCOLDFD                                           *
      * DESCRIPTION: EQUITY EXECUTION TAPE FROM THE ORDER ROOM         *
      *              (3480 CARTRIDGE, RECEIVED NIGHTLY BY COURIER).    *
      *              DSN MSEC.PROD.TC.EXECTAPE  VOL=SER=EXnnnn         *
      * RECFM/LRECL: FB / 160   BLKSIZE 16000                          *
      *----------------------------------------------------------------*
      * PRICES ARE IN FRACTIONS - WHOLE DOLLARS PLUS NUMERATOR OVER    *
      * DENOMINATOR (2,4,8,16,32,64,256).  DATES ARE YYMMDD.           *
      *----------------------------------------------------------------*
      * 1988-01-11 RJK  ORIGINAL                                       *
      * 1990-10-01 RJK  ADDED BRANCH OVERRIDE                          *
      * 1993-05-24 DWB  ADDED CONTRA BROKER NUMBER                     *
      *================================================================*
       01  OLD-TAPE-REC.
           05  OLD-REC-CODE            PIC X(01).
               88  OLD-TAPE-HEADER               VALUE '0'.
               88  OLD-TAPE-EXEC                 VALUE '1'.
               88  OLD-TAPE-CANCEL               VALUE '2'.
               88  OLD-TAPE-TRAILER              VALUE '9'.
           05  OLD-TICKET-NO           PIC X(08).
           05  OLD-ACCT-NO             PIC X(08).
           05  OLD-BRANCH-OVR          PIC X(03).
           05  OLD-SYMBOL              PIC X(06).
           05  OLD-CUSIP               PIC X(09).
           05  OLD-BUY-SELL            PIC X(01).
               88  OLD-BUY                       VALUE 'B'.
               88  OLD-SELL                      VALUE 'S'.
               88  OLD-SHORT                     VALUE 'T'.
           05  OLD-QTY                 PIC 9(07).
           05  OLD-PRICE-WHOLE         PIC 9(05).
           05  OLD-PRICE-NUMER         PIC 9(03).
           05  OLD-PRICE-DENOM         PIC 9(03).
           05  OLD-TRADE-DATE          PIC 9(06).
           05  OLD-SETTLE-DATE         PIC 9(06).
           05  OLD-COMMISSION          PIC S9(07)V99 COMP-3.
           05  OLD-SEC-FEE             PIC S9(05)V99 COMP-3.
           05  OLD-CONTRA-BKR          PIC X(04).
           05  OLD-ORIG-TICKET         PIC X(08).
           05  OLD-SOLICITED           PIC X(01).
               88  OLD-SOLICITED-ORDER           VALUE 'S'.
               88  OLD-UNSOLICITED-ORDER         VALUE 'U'.
           05  OLD-REP-NO              PIC X(03).
           05  FILLER                  PIC X(69).
       01  OLD-TAPE-TRAILER-REC  REDEFINES OLD-TAPE-REC.
           05  FILLER                  PIC X(01).
           05  OLD-TRL-COUNT           PIC 9(07).
           05  OLD-TRL-QTY-HASH        PIC 9(11).
           05  FILLER                  PIC X(141).
