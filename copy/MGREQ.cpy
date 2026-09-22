      *================================================================*
      * COPYBOOK   : MGREQ                                             *
      * DESCRIPTION: ACCOUNT MARGIN REQUIREMENT - ONE RECORD PER MARGIN*
      *             ACCOUNT PER DAY.  WRITTEN BY MGB100.               *
      *             DSN MSEC.PROD.MG.REQ(+1)                           *
      * RECFM/LRECL: FB / 250                                          *
      *----------------------------------------------------------------*
      * EQUITY = LONG MV - SHORT MV + CASH (TD BALANCE, USD)           *
      * EXCESS = EQUITY - HOUSE REQUIREMENT (NEGATIVE = DEFICIT)       *
      *----------------------------------------------------------------*
      * 1994-06-13 DWB  ORIGINAL                                       *
      * 2011-02-28 SPA  CONCENTRATION FIELDS                  CHG21340 *
      *================================================================*
       01  MRQ-REQUIREMENT-REC.
           05  MRQ-BUS-DATE            PIC 9(08).
           05  MRQ-ACCT-NO             PIC X(10).
           05  MRQ-ACCT-TYPE           PIC X(02).
           05  MRQ-BRANCH              PIC X(03).
           05  MRQ-REP                 PIC X(04).
           05  MRQ-LONG-MV             PIC S9(15)V99    COMP-3.
           05  MRQ-SHORT-MV            PIC S9(15)V99    COMP-3.
           05  MRQ-CASH-BALANCE        PIC S9(15)V99    COMP-3.
           05  MRQ-DEBIT-BALANCE       PIC S9(15)V99    COMP-3.
           05  MRQ-CREDIT-BALANCE      PIC S9(15)V99    COMP-3.
           05  MRQ-EQUITY              PIC S9(15)V99    COMP-3.
           05  MRQ-REGT-REQ            PIC S9(15)V99    COMP-3.
           05  MRQ-HOUSE-REQ           PIC S9(15)V99    COMP-3.
           05  MRQ-CONC-ADDON          PIC S9(15)V99    COMP-3.
           05  MRQ-EXCESS              PIC S9(15)V99    COMP-3.
           05  MRQ-SMA                 PIC S9(15)V99    COMP-3.
           05  MRQ-NONMARG-MV          PIC S9(15)V99    COMP-3.
           05  MRQ-LARGEST-ISSUER      PIC X(06).
           05  MRQ-LARGEST-PCT         PIC S9(03)V9(04) COMP-3.
           05  MRQ-POSITION-COUNT      PIC S9(05)       COMP-3.
           05  MRQ-STATUS              PIC X(01).
               88  MRQ-IN-GOOD-ORDER             VALUE 'G'.
               88  MRQ-REGT-CALL                 VALUE 'T'.
               88  MRQ-HOUSE-CALL                VALUE 'H'.
               88  MRQ-MIN-EQUITY-CALL           VALUE 'M'.
           05  MRQ-PRICE-STALE-FLAG    PIC X(01).
           05  FILLER                  PIC X(100).
