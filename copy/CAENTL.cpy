      *================================================================*
      * COPYBOOK   : CAENTL                                            *
      * DESCRIPTION: ENTITLEMENT RECORD.  WRITTEN BY CAB300 TO BOTH    *
      *              A DAILY SEQUENTIAL FILE AND THE ENTITLEMENT KSDS. *
      *              DSN MSEC.PROD.CA.ENTL(+1)                         *
      *              DSN MSEC.PROD.CA.ENTLMAST.KSDS   DDNAME ENTLMAST  *
      * KEY        : ENT-KEY  OFFSET 0 LENGTH 26                       *
      *              (EVENT 12 + ACCOUNT 10 + LOCATION 4)              *
      * RECFM/LRECL: FB / 250                                          *
      *================================================================*
       01  ENT-ENTITLEMENT-REC.
           05  ENT-KEY.
               10  ENT-EVENT-ID        PIC X(12).
               10  ENT-ACCT-NO         PIC X(10).
               10  ENT-LOCATION        PIC X(04).
           05  ENT-CUSIP               PIC X(09).
           05  ENT-EVENT-TYPE          PIC X(03).
           05  ENT-ACCT-TYPE           PIC X(02).
           05  ENT-TAX-STATUS          PIC X(01).
           05  ENT-TAX-COUNTRY         PIC X(02).
           05  ENT-ELIGIBLE-QTY        PIC S9(11)V9(04) COMP-3.
           05  ENT-GROSS-CASH          PIC S9(13)V99    COMP-3.
           05  ENT-WHT-RATE            PIC S9(01)V9(04) COMP-3.
           05  ENT-WHT-AMOUNT          PIC S9(13)V99    COMP-3.
           05  ENT-NET-CASH            PIC S9(13)V99    COMP-3.
           05  ENT-CCY                 PIC X(03).
           05  ENT-NEW-SHARES          PIC S9(11)V9(04) COMP-3.
           05  ENT-WHOLE-SHARES        PIC S9(11)       COMP-3.
           05  ENT-FRAC-SHARES         PIC S9(01)V9(06) COMP-3.
           05  ENT-CIL-AMOUNT          PIC S9(11)V99    COMP-3.
           05  ENT-NEW-CUSIP           PIC X(09).
           05  ENT-DUE-BILL-FLAG       PIC X(01).
               88  ENT-DUE-BILL                  VALUE 'Y'.
           05  ENT-STATUS              PIC X(02).
               88  ENT-CALCULATED                VALUE 'CA'.
               88  ENT-ACCRUED                   VALUE 'AC'.
               88  ENT-PAID                      VALUE 'PD'.
               88  ENT-REVERSED                  VALUE 'RV'.
           05  ENT-PAY-DATE            PIC 9(08).
           05  ENT-CALC-DATE           PIC 9(08).
           05  ENT-PAID-DATE           PIC 9(08).
           05  FILLER                  PIC X(108).
