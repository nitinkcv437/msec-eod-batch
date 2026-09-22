      *================================================================*
      * COPYBOOK   : SWFAIL                                            *
      * DESCRIPTION: SETTLEMENT FAIL / CLOSE-OUT DETAIL (SWB300)       *
      *             DSN MSEC.PROD.SW.FAILS(+1)                         *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  SWF-FAIL-REC.
           05  SWF-BUS-DATE            PIC 9(08).
           05  SWF-SENDER-REF          PIC X(16).
           05  SWF-DIRECTION           PIC X(01).
               88  SWF-FAIL-TO-DELIVER           VALUE 'D'.
               88  SWF-FAIL-TO-RECEIVE           VALUE 'R'.
           05  SWF-ACCT-NO             PIC X(10).
           05  SWF-CUSIP               PIC X(09).
           05  SWF-SETTLE-DATE         PIC 9(08).
           05  SWF-AGE-BUS-DAYS        PIC S9(03)       COMP-3.
           05  SWF-QTY                 PIC S9(11)V9(04) COMP-3.
           05  SWF-AMOUNT              PIC S9(15)V99    COMP-3.
           05  SWF-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  SWF-REASON-CODE         PIC X(04).
           05  SWF-CLOSEOUT-DATE       PIC 9(08).
           05  SWF-ACTION              PIC X(02).
               88  SWF-ACT-MONITOR               VALUE 'MO'.
               88  SWF-ACT-CLOSEOUT-DUE          VALUE 'CD'.
               88  SWF-ACT-BUYIN                 VALUE 'BI'.
               88  SWF-ACT-PENALTY               VALUE 'PN'.
           05  FILLER                  PIC X(56).
