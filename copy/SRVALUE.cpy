      *================================================================*
      * COPYBOOK   : SRVALUE                                           *
      * DESCRIPTION: DAILY VALUATION DETAIL - ONE RECORD PER OPEN      *
      *              OWNERSHIP POSITION.  WRITTEN BY SRB400.           *
      *              DSN MSEC.PROD.SR.VALUATION(+1)                    *
      * RECFM/LRECL: FB / 200                                          *
      *================================================================*
       01  VAL-VALUATION-REC.
           05  VAL-BUS-DATE            PIC 9(08).
           05  VAL-ACCT-NO             PIC X(10).
           05  VAL-CUSIP               PIC X(09).
           05  VAL-LOCATION            PIC X(04).
           05  VAL-BRANCH              PIC X(03).
           05  VAL-REP                 PIC X(04).
           05  VAL-ACCT-TYPE           PIC X(02).
           05  VAL-SEC-TYPE            PIC X(02).
           05  VAL-CCY                 PIC X(03).
           05  VAL-QTY                 PIC S9(11)V9(04) COMP-3.
           05  VAL-PRICE               PIC S9(09)V9(08) COMP-3.
           05  VAL-PRICE-DATE          PIC 9(08).
           05  VAL-PRICE-STALE-FLAG    PIC X(01).
               88  VAL-PRICE-STALE               VALUE 'Y'.
           05  VAL-PRICE-FACTOR        PIC S9(05)V9(04) COMP-3.
           05  VAL-MKT-VALUE           PIC S9(15)V99    COMP-3.
           05  VAL-FX-RATE             PIC S9(05)V9(08) COMP-3.
           05  VAL-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  VAL-COST-BASIS          PIC S9(15)V99    COMP-3.
           05  VAL-UNRLZD-PL           PIC S9(15)V99    COMP-3.
           05  VAL-ACCRUED-INT         PIC S9(13)V99    COMP-3.
           05  FILLER                  PIC X(73).
