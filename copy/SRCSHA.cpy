      *================================================================*
      * COPYBOOK   : SRCSHA                                            *
      * DESCRIPTION: CASH ACTIVITY DETAIL.  ONE RECORD PER CASH        *
      *              MOVEMENT APPLIED TO THE CASH BALANCE FILE BY      *
      *              SRB300.  INPUT TO THE CASH ACTIVITY REPORT SRR310 *
      *              DSN MSEC.PROD.SR.CASHACT(+1)                      *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * 1988-02-15 RJK  ORIGINAL                                       *
      * 2009-12-14 SPA  MULTI-CURRENCY                        CHG19002 *
      *================================================================*
       01  CSA-CASH-ACTV-REC.
           05  CSA-BUS-DATE            PIC 9(08).
           05  CSA-ACCT-NO             PIC X(10).
           05  CSA-CCY                 PIC X(03).
           05  CSA-BASIS               PIC X(01).
               88  CSA-TRADE-DATE-CASH           VALUE 'T'.
               88  CSA-SETTLE-DATE-CASH          VALUE 'S'.
           05  CSA-SOURCE              PIC X(02).
           05  CSA-REF                 PIC X(16).
           05  CSA-LEG-NO              PIC 9(01).
           05  CSA-ACT-TYPE            PIC X(03).
           05  CSA-CUSIP               PIC X(09).
           05  CSA-AMOUNT              PIC S9(15)V99    COMP-3.
           05  CSA-BEFORE-TD-BAL       PIC S9(15)V99    COMP-3.
           05  CSA-AFTER-TD-BAL        PIC S9(15)V99    COMP-3.
           05  CSA-BEFORE-SD-BAL       PIC S9(15)V99    COMP-3.
           05  CSA-AFTER-SD-BAL        PIC S9(15)V99    COMP-3.
           05  CSA-SETTLE-DATE         PIC 9(08).
           05  CSA-NEW-BAL-FLAG        PIC X(01).
               88  CSA-NEW-BALANCE               VALUE 'Y'.
           05  CSA-DESC                PIC X(30).
           05  FILLER                  PIC X(13).
