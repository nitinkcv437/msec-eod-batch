      *================================================================*
      * COPYBOOK   : SRPEND                                            *
      * DESCRIPTION: PENDING SETTLEMENT (VSAM KSDS).  ONE RECORD PER   *
      *              ACTIVITY LEG THAT SETTLES IN THE FUTURE.  WRITTEN *
      *              BY SRB200, CONSUMED BY SRB250 ON SETTLE DATE.     *
      *              DSN MSEC.PROD.SR.PENDSETL.KSDS   DDNAME PENDSETL  *
      * KEY        : PND-KEY  OFFSET 0 LENGTH 25                       *
      *              (SETTLE DATE 8 + REF 16 + LEG 1)                  *
      * RECFM/LRECL: F / 150                                           *
      *================================================================*
       01  PND-PENDING-REC.
           05  PND-KEY.
               10  PND-SETTLE-DATE     PIC 9(08).
               10  PND-REF             PIC X(16).
               10  PND-LEG-NO          PIC 9(01).
           05  PND-ACCT-NO             PIC X(10).
           05  PND-CUSIP               PIC X(09).
           05  PND-LOCATION            PIC X(04).
           05  PND-ACT-TYPE            PIC X(03).
           05  PND-QTY                 PIC S9(11)V9(04) COMP-3.
           05  PND-CASH                PIC S9(15)V99    COMP-3.
           05  PND-CCY                 PIC X(03).
           05  PND-TRADE-DATE          PIC 9(08).
           05  PND-STATUS              PIC X(01).
               88  PND-OPEN                      VALUE 'O'.
               88  PND-SETTLED                   VALUE 'S'.
               88  PND-FAILED                    VALUE 'F'.
               88  PND-CANCELLED                 VALUE 'X'.
           05  PND-FAIL-DAYS           PIC S9(03)       COMP-3.
           05  FILLER                  PIC X(68).
