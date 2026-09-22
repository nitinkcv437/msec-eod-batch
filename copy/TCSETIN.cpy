      *================================================================*
      * COPYBOOK   : TCSETIN                                           *
      * DESCRIPTION: SETTLEMENT INSTRUCTION EXTRACT - CONSUMED BY THE  *
      *              SWIFT GATEWAY (OUT OF SCOPE OF THIS CYCLE).       *
      *              DSN MSEC.PROD.TC.SETLINST(+1)                     *
      * RECFM/LRECL: FB / 250                                          *
      *----------------------------------------------------------------*
      * MT540 RECEIVE FREE   MT541 RECEIVE VS PAYMENT                  *
      * MT542 DELIVER FREE   MT543 DELIVER VS PAYMENT                  *
      *----------------------------------------------------------------*
      * 1997-10-06 DWB  ORIGINAL (ISO 15022 MIGRATION)        CHG03390 *
      *================================================================*
       01  SI-INSTRUCTION-REC.
           05  SI-TRADE-ID             PIC X(16).
           05  SI-MSG-TYPE             PIC X(05).
           05  SI-FUNCTION             PIC X(04).
               88  SI-NEW-INSTR                  VALUE 'NEWM'.
               88  SI-CANCEL-INSTR               VALUE 'CANC'.
           05  SI-ACCT-NO              PIC X(10).
           05  SI-CUSIP                PIC X(09).
           05  SI-ISIN                 PIC X(12).
           05  SI-QTY                  PIC S9(11)V9(04) COMP-3.
           05  SI-SETTLE-AMOUNT        PIC S9(15)V99    COMP-3.
           05  SI-CCY                  PIC X(03).
           05  SI-TRADE-DATE           PIC 9(08).
           05  SI-SETTLE-DATE          PIC 9(08).
           05  SI-DEPOSITORY           PIC X(04).
           05  SI-CONTRA               PIC X(04).
           05  SI-PLACE-BIC            PIC X(11).
           05  SI-AGENT-BIC            PIC X(11).
           05  SI-SAFEKEEPING-ACCT     PIC X(12).
           05  FILLER                  PIC X(116).
