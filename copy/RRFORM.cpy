      *================================================================*
      * COPYBOOK   : RRFORM                                            *
      * DESCRIPTION: CUSTOMER RESERVE FORMULA LINE (RULE 15C3-3 STYLE  *
      *             COMPUTATION).  WRITTEN BY RRB100.                  *
      *             DSN MSEC.PROD.RR.RESERVE(+1)                       *
      * RECFM/LRECL: FB / 100                                          *
      *================================================================*
       01  RRF-FORMULA-REC.
           05  RRF-BUS-DATE            PIC 9(08).
           05  RRF-SECTION             PIC X(01).
               88  RRF-CREDIT-ITEM               VALUE 'C'.
               88  RRF-DEBIT-ITEM                VALUE 'D'.
               88  RRF-SUMMARY-ITEM              VALUE 'S'.
           05  RRF-LINE-NO             PIC 9(02).
           05  RRF-DESC                PIC X(40).
           05  RRF-AMOUNT              PIC S9(15)V99    COMP-3.
           05  RRF-ITEM-COUNT          PIC S9(07)       COMP-3.
           05  FILLER                  PIC X(36).
