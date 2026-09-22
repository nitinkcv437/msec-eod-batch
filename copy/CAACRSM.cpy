      *================================================================*
      * COPYBOOK   : CAACRSM                                           *
      * DESCRIPTION: DIVIDEND ACCRUAL SUMMARY.  ONE RECORD PER EVENT   *
      *              WITH ENTITLEMENTS ACCRUED TODAY.  WRITTEN BY      *
      *              CAB500, PRINTED BY CAR510.                        *
      *              DSN MSEC.PROD.CA.ACCRSUM(+1)                      *
      * RECFM/LRECL: FB / 100                                          *
      *----------------------------------------------------------------*
      * 2003-03-10 KAP  ORIGINAL                              CHG10877 *
      * 2011-09-12 SPA  USD EQUIVALENT                        CHG22410 *
      *================================================================*
       01  ACS-SUMMARY-REC.
           05  ACS-EVENT-ID            PIC X(12).
           05  ACS-CUSIP               PIC X(09).
           05  ACS-EVENT-TYPE          PIC X(03).
           05  ACS-PAY-DATE            PIC 9(08).
           05  ACS-CCY                 PIC X(03).
           05  ACS-ENTL-COUNT          PIC S9(07)       COMP-3.
           05  ACS-ACCRUED-COUNT       PIC S9(07)       COMP-3.
           05  ACS-ACCRUAL-AMT         PIC S9(15)V99    COMP-3.
           05  ACS-ACCRUAL-USD         PIC S9(15)V99    COMP-3.
           05  ACS-SKIP-COUNT          PIC S9(07)       COMP-3.
           05  ACS-DR-GL-ACCOUNT       PIC X(10).
           05  ACS-CR-GL-ACCOUNT       PIC X(10).
           05  ACS-BUS-DATE            PIC 9(08).
           05  FILLER                  PIC X(07).
