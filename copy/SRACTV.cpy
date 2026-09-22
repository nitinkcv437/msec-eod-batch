      *================================================================*
      * COPYBOOK   : SRACTV                                            *
      * DESCRIPTION: STOCK RECORD ACTIVITY.  STANDARD INTERFACE INTO   *
      *              THE STOCK RECORD FROM TRADE CAPTURE (TCB500),     *
      *              CORPORATE ACTIONS (CAB400) AND ADJUSTMENTS.       *
      *              EACH BUSINESS EVENT PRODUCES A BALANCED PAIR OF   *
      *              LEGS: CUSTOMER/FIRM SIDE AND STREET/DEPOSITORY    *
      *              SIDE (ACT-LEG-NO 1 AND 2).                        *
      * RECFM/LRECL: FB / 200                                          *
      *----------------------------------------------------------------*
      * 1987-09-01 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2010-04-05 SPA  ACT-SOURCE 'CA' FOR CORP ACTIONS      CHG19870 *
      *================================================================*
       01  ACT-ACTIVITY-REC.
           05  ACT-SOURCE              PIC X(02).
               88  ACT-FROM-TRADE                VALUE 'TC'.
               88  ACT-FROM-CORP-ACTION          VALUE 'CA'.
               88  ACT-FROM-ADJUSTMENT           VALUE 'AJ'.
           05  ACT-REF                 PIC X(16).
           05  ACT-LEG-NO              PIC 9(01).
           05  ACT-ACCT-NO             PIC X(10).
           05  ACT-CUSIP               PIC X(09).
           05  ACT-LOCATION            PIC X(04).
           05  ACT-TYPE                PIC X(03).
               88  ACT-BUY                       VALUE 'BUY'.
               88  ACT-SELL                      VALUE 'SEL'.
               88  ACT-SHORT-SELL                VALUE 'SSL'.
               88  ACT-BUY-COVER                 VALUE 'BCV'.
               88  ACT-CXL-BUY                   VALUE 'XBY'.
               88  ACT-CXL-SELL                  VALUE 'XSL'.
               88  ACT-CASH-DIVIDEND             VALUE 'DIV'.
               88  ACT-WITHHOLDING               VALUE 'WHT'.
               88  ACT-STOCK-DIVIDEND            VALUE 'SDV'.
               88  ACT-SPLIT                     VALUE 'SPL'.
               88  ACT-CASH-IN-LIEU              VALUE 'CIL'.
               88  ACT-MERGER-OUT                VALUE 'MGO'.
               88  ACT-MERGER-CASH               VALUE 'MGC'.
               88  ACT-QTY-ADJUST                VALUE 'QAJ'.
           05  ACT-QTY-CHANGE          PIC S9(11)V9(04) COMP-3.
           05  ACT-CASH-CHANGE         PIC S9(15)V99    COMP-3.
           05  ACT-COST-CHANGE         PIC S9(15)V99    COMP-3.
           05  ACT-PRICE               PIC S9(09)V9(08) COMP-3.
           05  ACT-CCY                 PIC X(03).
           05  ACT-TRADE-DATE          PIC 9(08).
           05  ACT-SETTLE-DATE         PIC 9(08).
           05  ACT-EFFECTIVE-DATE      PIC 9(08).
           05  ACT-GL-TXN-CODE         PIC X(04).
           05  ACT-SEC-TYPE            PIC X(02).
           05  ACT-ACCT-TYPE           PIC X(02).
           05  ACT-SETTLE-FLAG         PIC X(01).
               88  ACT-SETTLES-LATER             VALUE 'P'.
               88  ACT-SETTLED-TODAY             VALUE 'S'.
               88  ACT-NO-SETTLEMENT             VALUE 'N'.
           05  ACT-BUS-DATE            PIC 9(08).
           05  ACT-DESC                PIC X(30).
           05  FILLER                  PIC X(46).
