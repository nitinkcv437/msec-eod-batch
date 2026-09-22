      *================================================================*
      * COPYBOOK   : RRTAXY                                            *
      * DESCRIPTION: TAX YEAR-TO-DATE ACCUMULATORS (VSAM KSDS) FOR     *
      *             1099-DIV / 1099-B / 1042-S STYLE REPORTING.        *
      *             DSN MSEC.PROD.RR.TAXYTD.KSDS     DDNAME TAXYTD     *
      * KEY        : RTX-KEY  OFFSET 0 LENGTH 18                       *
      * RECFM/LRECL: F / 200                                           *
      *----------------------------------------------------------------*
      * FORM '1099' DOMESTIC HOLDERS, '1042' FOREIGN HOLDERS           *
      *================================================================*
       01  RTX-TAX-YTD-REC.
           05  RTX-KEY.
               10  RTX-ACCT-NO         PIC X(10).
               10  RTX-TAX-YEAR        PIC 9(04).
               10  RTX-FORM            PIC X(04).
           05  RTX-TAX-STATUS          PIC X(01).
           05  RTX-TAX-COUNTRY         PIC X(02).
           05  RTX-ORD-DIVIDENDS       PIC S9(13)V99    COMP-3.
           05  RTX-QUAL-DIVIDENDS      PIC S9(13)V99    COMP-3.
           05  RTX-FOREIGN-TAX-PAID    PIC S9(11)V99    COMP-3.
           05  RTX-FED-WITHHELD        PIC S9(11)V99    COMP-3.
           05  RTX-GROSS-PROCEEDS      PIC S9(15)V99    COMP-3.
           05  RTX-COST-BASIS          PIC S9(15)V99    COMP-3.
           05  RTX-REALIZED-PL         PIC S9(15)V99    COMP-3.
           05  RTX-CIL-PROCEEDS        PIC S9(11)V99    COMP-3.
           05  RTX-MERGER-PROCEEDS     PIC S9(15)V99    COMP-3.
           05  RTX-MARGIN-INT-PAID     PIC S9(11)V99    COMP-3.
           05  RTX-1042-GROSS-INCOME   PIC S9(13)V99    COMP-3.
           05  RTX-1042-TAX-WITHHELD   PIC S9(11)V99    COMP-3.
           05  RTX-TXN-COUNT           PIC S9(07)       COMP-3.
           05  RTX-LAST-UPD-DATE       PIC 9(08).
           05  RTX-LAST-UPD-JOB        PIC X(08).
           05  FILLER                  PIC X(64).
