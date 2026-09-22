      *================================================================*
      * COPYBOOK   : CAELIG                                            *
      * DESCRIPTION: RECORD-DATE ELIGIBILITY SNAPSHOT.  ONE RECORD PER *
      *              HOLDER POSITION IN AN EVENT'S SECURITY AS OF THE  *
      *              RECORD DATE.  WRITTEN BY CAB200.                  *
      *              DSN MSEC.PROD.CA.ELIG(+1)                         *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * DUE BILL: WHEN A TRADE EXECUTED BEFORE EX-DATE HAS NOT SETTLED *
      * BY RECORD DATE, THE BUYER IS ENTITLED BUT THE SELLER IS HOLDER *
      * OF RECORD.  ELG-DUE-BILL-QTY CARRIES TD-QTY MINUS SD-QTY.      *
      *================================================================*
       01  ELG-ELIGIBILITY-REC.
           05  ELG-EVENT-ID            PIC X(12).
           05  ELG-ACCT-NO             PIC X(10).
           05  ELG-CUSIP               PIC X(09).
           05  ELG-LOCATION            PIC X(04).
           05  ELG-EVENT-TYPE          PIC X(03).
           05  ELG-ACCT-TYPE           PIC X(02).
           05  ELG-TAX-STATUS          PIC X(01).
           05  ELG-TAX-COUNTRY         PIC X(02).
           05  ELG-SD-QTY              PIC S9(11)V9(04) COMP-3.
           05  ELG-TD-QTY              PIC S9(11)V9(04) COMP-3.
           05  ELG-DUE-BILL-QTY        PIC S9(11)V9(04) COMP-3.
           05  ELG-ENTITLED-QTY        PIC S9(11)V9(04) COMP-3.
           05  ELG-RECORD-DATE         PIC 9(08).
           05  ELG-SNAPSHOT-DATE       PIC 9(08).
           05  FILLER                  PIC X(59).
