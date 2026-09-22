      *================================================================*
      * COPYBOOK   : RRBLUE                                            *
      * DESCRIPTION: REGULATORY TRADING ACTIVITY REQUEST RESPONSE      *
      *             ('BLUE SHEET' STYLE).  WRITTEN BY RRB300 (AD HOC). *
      *             DSN MSEC.PROD.RR.BLUESHT(+1)                       *
      * RECFM/LRECL: FB / 300                                          *
      *================================================================*
       01  RRB-BLUE-REC.
           05  RRB-REQUEST-ID          PIC X(10).
           05  RRB-REC-TYPE            PIC X(01).
           05  RRB-CUSIP               PIC X(09).
           05  RRB-TRADE-DATE          PIC 9(08).
           05  RRB-TRADE-ID            PIC X(16).
           05  RRB-SIDE                PIC X(02).
           05  RRB-QTY                 PIC S9(11)V9(04) COMP-3.
           05  RRB-PRICE               PIC S9(09)V9(08) COMP-3.
           05  RRB-NET-AMOUNT          PIC S9(15)V99    COMP-3.
           05  RRB-ACCT-NO             PIC X(10).
           05  RRB-ACCT-NAME           PIC X(40).
           05  RRB-ACCT-TYPE           PIC X(02).
           05  RRB-STATUS              PIC X(02).
           05  RRB-CONTRA              PIC X(04).
           05  FILLER                  PIC X(170).
