      *================================================================*
      * COPYBOOK   : SRFAIL                                            *
      * DESCRIPTION: SETTLEMENT FAIL DETAIL.  ONE RECORD PER PENDING   *
      *              SETTLEMENT LEG THAT DID NOT SETTLE ON ITS SETTLE  *
      *              DATE.  WRITTEN BY SRB250, REPORTED BY SRR260.     *
      *              DSN MSEC.PROD.SR.FAILS(+1)                        *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * 1991-06-03 DWB  ORIGINAL                                       *
      * 2024-05-20 NVR  T+1 - FAIL DAYS NOW BUSINESS DAYS     CHG40551 *
      *================================================================*
       01  FLR-FAIL-REC.
           05  FLR-BUS-DATE            PIC 9(08).
           05  FLR-SETTLE-DATE         PIC 9(08).
           05  FLR-REF                 PIC X(16).
           05  FLR-LEG-NO              PIC 9(01).
           05  FLR-ACCT-NO             PIC X(10).
           05  FLR-CUSIP               PIC X(09).
           05  FLR-LOCATION            PIC X(04).
           05  FLR-ACT-TYPE            PIC X(03).
           05  FLR-ACCT-TYPE           PIC X(02).
           05  FLR-SEC-TYPE            PIC X(02).
           05  FLR-QTY                 PIC S9(11)V9(04) COMP-3.
           05  FLR-CASH                PIC S9(15)V99    COMP-3.
           05  FLR-CCY                 PIC X(03).
           05  FLR-TRADE-DATE          PIC 9(08).
           05  FLR-FAIL-DAYS           PIC S9(03)       COMP-3.
           05  FLR-NEW-FAIL-FLAG       PIC X(01).
               88  FLR-NEW-FAIL                  VALUE 'Y'.
           05  FLR-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  FILLER                  PIC X(47).
