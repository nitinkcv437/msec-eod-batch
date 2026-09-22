      *================================================================*
      * COPYBOOK   : SRGLJNL                                           *
      * DESCRIPTION: GENERAL LEDGER JOURNAL LINE.  PRODUCED BY SRB600  *
      *              (FROM POSTING JOURNAL) AND CAB500 (DIVIDEND       *
      *              ACCRUALS).  FEEDS THE CORPORATE GL (OUT OF SCOPE) *
      *              DSN MSEC.PROD.SR.GLJRNL(+1)                       *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * EVERY GLJ-REF MUST NET TO ZERO (DEBITS = CREDITS).             *
      *================================================================*
       01  GLJ-JOURNAL-REC.
           05  GLJ-BUS-DATE            PIC 9(08).
           05  GLJ-SOURCE              PIC X(02).
           05  GLJ-REF                 PIC X(16).
           05  GLJ-LINE-NO             PIC 9(03).
           05  GLJ-TXN-CODE            PIC X(04).
           05  GLJ-GL-ACCOUNT          PIC X(10).
           05  GLJ-COST-CENTER         PIC X(06).
           05  GLJ-DR-CR               PIC X(01).
               88  GLJ-DEBIT                     VALUE 'D'.
               88  GLJ-CREDIT                    VALUE 'C'.
           05  GLJ-AMOUNT              PIC S9(15)V99    COMP-3.
           05  GLJ-CCY                 PIC X(03).
           05  GLJ-AMOUNT-USD          PIC S9(15)V99    COMP-3.
           05  GLJ-ACCT-NO             PIC X(10).
           05  GLJ-CUSIP               PIC X(09).
           05  GLJ-DESC                PIC X(30).
           05  GLJ-REVERSAL-FLAG       PIC X(01).
               88  GLJ-AUTO-REVERSE              VALUE 'R'.
           05  FILLER                  PIC X(29).
