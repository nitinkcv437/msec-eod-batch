      *================================================================*
      * COPYBOOK   : MGPREQ                                            *
      * DESCRIPTION: POSITION-LEVEL MARGIN REQUIREMENT DETAIL (MGB100) *
      *             DSN MSEC.PROD.MG.POSREQ(+1)                        *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  MPQ-POSREQ-REC.
           05  MPQ-BUS-DATE            PIC 9(08).
           05  MPQ-ACCT-NO             PIC X(10).
           05  MPQ-CUSIP               PIC X(09).
           05  MPQ-SEC-TYPE            PIC X(02).
           05  MPQ-ISSUER-ID           PIC X(06).
           05  MPQ-QTY                 PIC S9(11)V9(04) COMP-3.
           05  MPQ-PRICE               PIC S9(09)V9(08) COMP-3.
           05  MPQ-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  MPQ-RULE-KEY            PIC X(08).
           05  MPQ-REQ-PCT             PIC S9(03)V9(04) COMP-3.
           05  MPQ-REQ-AMOUNT          PIC S9(15)V99    COMP-3.
           05  MPQ-PER-SHARE-FLAG      PIC X(01).
           05  MPQ-MARGINABLE-FLAG     PIC X(01).
           05  FILLER                  PIC X(66).
