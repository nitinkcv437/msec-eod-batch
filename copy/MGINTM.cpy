      *================================================================*
      * COPYBOOK   : MGINTM                                            *
      * DESCRIPTION: MONTH-TO-DATE MARGIN INTEREST (VSAM KSDS)         *
      *             DSN MSEC.PROD.MG.INTMTD.KSDS     DDNAME MGINTMTD   *
      * KEY        : MIM-ACCT-NO  OFFSET 0 LENGTH 10                   *
      * RECFM/LRECL: F / 100                                           *
      *================================================================*
       01  MIM-MTD-REC.
           05  MIM-ACCT-NO             PIC X(10).
           05  MIM-MONTH               PIC 9(06).
           05  MIM-MTD-INTEREST        PIC S9(11)V99    COMP-3.
           05  MIM-MTD-DAYS            PIC S9(03)       COMP-3.
           05  MIM-AVG-DEBIT           PIC S9(15)V99    COMP-3.
           05  MIM-LAST-ACCR-DATE      PIC 9(08).
           05  MIM-LAST-POSTED-MONTH   PIC 9(06).
           05  MIM-LAST-POSTED-AMT     PIC S9(11)V99    COMP-3.
           05  FILLER                  PIC X(45).
