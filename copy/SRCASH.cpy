      *================================================================*
      * COPYBOOK   : SRCASH                                            *
      * DESCRIPTION: ACCOUNT CASH BALANCE (VSAM KSDS)                  *
      *              DSN MSEC.PROD.SR.CASHBAL.KSDS    DDNAME CASHBAL   *
      * KEY        : CSH-KEY  OFFSET 0 LENGTH 13  (ACCOUNT + CCY)      *
      * RECFM/LRECL: F / 150                                           *
      *----------------------------------------------------------------*
      * 1987-09-01 RJK  ORIGINAL                                       *
      * 2009-12-14 SPA  MULTI-CURRENCY                        CHG19002 *
      *================================================================*
       01  CSH-CASH-REC.
           05  CSH-KEY.
               10  CSH-ACCT-NO         PIC X(10).
               10  CSH-CCY             PIC X(03).
           05  CSH-TD-BALANCE          PIC S9(15)V99    COMP-3.
           05  CSH-SD-BALANCE          PIC S9(15)V99    COMP-3.
           05  CSH-PEND-CR             PIC S9(15)V99    COMP-3.
           05  CSH-PEND-DR             PIC S9(15)V99    COMP-3.
           05  CSH-DIV-RECEIVABLE      PIC S9(15)V99    COMP-3.
           05  CSH-WHT-YTD             PIC S9(13)V99    COMP-3.
           05  CSH-INCOME-YTD          PIC S9(15)V99    COMP-3.
           05  CSH-LAST-ACTV-DATE      PIC 9(08).
           05  CSH-LAST-UPD-JOB        PIC X(08).
           05  FILLER                  PIC X(59).
