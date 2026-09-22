      *================================================================*
      * COPYBOOK   : MGINT                                             *
      * DESCRIPTION: DAILY MARGIN INTEREST ACCRUAL DETAIL (MGB300)     *
      *             DSN MSEC.PROD.MG.INTACCR(+1)                       *
      * RECFM/LRECL: FB / 120                                          *
      *================================================================*
       01  MGI-ACCRUAL-REC.
           05  MGI-BUS-DATE            PIC 9(08).
           05  MGI-ACCT-NO             PIC X(10).
           05  MGI-DEBIT-BALANCE       PIC S9(15)V99    COMP-3.
           05  MGI-BASE-RATE           PIC S9(03)V9(06) COMP-3.
           05  MGI-SPREAD              PIC S9(03)V9(06) COMP-3.
           05  MGI-EFFECTIVE-RATE      PIC S9(03)V9(06) COMP-3.
           05  MGI-TIER                PIC 9(01).
           05  MGI-DAYS                PIC S9(03)       COMP-3.
           05  MGI-DAILY-INTEREST      PIC S9(11)V99    COMP-3.
           05  MGI-MTD-INTEREST        PIC S9(11)V99    COMP-3.
           05  MGI-POSTED-FLAG         PIC X(01).
           05  FILLER                  PIC X(60).
