      *================================================================*
      * COPYBOOK   : RRTAXA                                            *
      * DESCRIPTION: TAX ACTIVITY - ONE RECORD PER TRANSACTION APPLIED *
      *             TO THE TAX YEAR-TO-DATE MASTER BY RRB400.  INPUT   *
      *             TO THE DAILY TAX ACTIVITY REPORT RRR410.           *
      *             DSN MSEC.PROD.RR.TAXACT(+1)                        *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * RTA-CATEGORY  ODIV ORDINARY DIVIDEND (NOT QUALIFIED)           *
      *               QDIV QUALIFIED DIVIDEND (ALSO IN ORDINARY)       *
      *               FTAX FOREIGN TAX PAID            (1099)          *
      *               42GI GROSS INCOME                (1042)          *
      *               42TW TAX WITHHELD                (1042)          *
      *               CILP CASH IN LIEU PROCEEDS                       *
      *               MRGP CASH MERGER PROCEEDS                        *
      *               SALE GROSS PROCEEDS OF A SALE (1099-B)           *
      *----------------------------------------------------------------*
      * 2021-01-11 JLR  ORIGINAL                              CHG35610 *
      *================================================================*
       01  RTA-TAX-ACTIVITY-REC.
           05  RTA-BUS-DATE            PIC 9(08).
           05  RTA-ACCT-NO             PIC X(10).
           05  RTA-TAX-YEAR            PIC 9(04).
           05  RTA-FORM                PIC X(04).
           05  RTA-SOURCE              PIC X(02).
               88  RTA-FROM-CORP-ACTION          VALUE 'CA'.
               88  RTA-FROM-POSTING              VALUE 'SR'.
           05  RTA-REF                 PIC X(16).
           05  RTA-ACT-TYPE            PIC X(03).
           05  RTA-CUSIP               PIC X(09).
           05  RTA-CATEGORY            PIC X(04).
           05  RTA-AMOUNT-USD          PIC S9(15)V99    COMP-3.
           05  RTA-REALIZED-PL         PIC S9(15)V99    COMP-3.
           05  RTA-ORIG-AMOUNT         PIC S9(15)V99    COMP-3.
           05  RTA-ORIG-CCY            PIC X(03).
           05  RTA-ISSUER-CTRY         PIC X(02).
           05  RTA-SEC-TYPE            PIC X(02).
           05  RTA-TAX-STATUS          PIC X(01).
           05  RTA-TAX-COUNTRY         PIC X(02).
           05  RTA-UPSERT-ACTION       PIC X(01).
               88  RTA-YTD-INSERTED              VALUE 'I'.
               88  RTA-YTD-UPDATED               VALUE 'U'.
           05  FILLER                  PIC X(52).
