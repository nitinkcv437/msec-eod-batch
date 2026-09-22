      *================================================================*
      * COPYBOOK   : SWPSTM                                            *
      * DESCRIPTION: STATEMENT OF PENDING TRANSACTIONS - EXTRACT LINE. *
      *             WRITTEN BY SWB400, SORTED BY ACCOUNT / SECTION /   *
      *             SETTLE DATE / REFERENCE AND PRINTED BY SWR410.     *
      *             DSN MSEC.PROD.SW.PENDSTMT(+1)                      *
      * RECFM/LRECL: FB / 200                                          *
      *----------------------------------------------------------------*
      * SORT KEY   : PST-ACCT-NO     1,10  CH                          *
      *              PST-SECTION    11,1   CH  (F, P, S)               *
      *              PST-SETTLE-DATE 12,8  CH                          *
      *              PST-SENDER-REF 20,16  CH                          *
      *----------------------------------------------------------------*
      * 1999-03-15 TLM  ORIGINAL (MT537 REPLACEMENT)          CHG04890 *
      * 2011-06-20 SPA  SECURITY DESCRIPTION FROM DB2         CHG21877 *
      *================================================================*
       01  PST-STMT-REC.
           05  PST-ACCT-NO             PIC X(10).
           05  PST-SECTION             PIC X(01).
               88  PST-FAILING                   VALUE 'F'.
               88  PST-PENDING                   VALUE 'P'.
               88  PST-SETTLED-TODAY             VALUE 'S'.
           05  PST-SETTLE-DATE         PIC 9(08).
           05  PST-SENDER-REF          PIC X(16).
           05  PST-BUS-DATE            PIC 9(08).
           05  PST-MSG-TYPE            PIC X(03).
           05  PST-STATUS              PIC X(02).
           05  PST-REASON-CODE         PIC X(04).
           05  PST-CUSIP               PIC X(09).
           05  PST-ISIN                PIC X(12).
           05  PST-SEC-DESC            PIC X(30).
           05  PST-TRADE-DATE          PIC 9(08).
           05  PST-QTY                 PIC S9(11)V9(04) COMP-3.
           05  PST-OPEN-QTY            PIC S9(11)V9(04) COMP-3.
           05  PST-AMOUNT              PIC S9(15)V99    COMP-3.
           05  PST-CCY                 PIC X(03).
           05  PST-DEPOSITORY          PIC X(04).
           05  PST-FAIL-AGE            PIC S9(03)       COMP-3.
           05  PST-CLOSEOUT-DATE       PIC 9(08).
           05  PST-BUYIN-FLAG          PIC X(01).
           05  PST-EFF-SETTLE-DATE     PIC 9(08).
           05  FILLER                  PIC X(38).
