      *================================================================*
      * COPYBOOK   : RCSTPOS                                           *
      * DESCRIPTION: NORMALIZED STREET-SIDE POSITION - ONE RECORD PER  *
      *             DEPOSITORY + CUSIP.  WRITTEN BY RCB100 (DTC) AND   *
      *             RCB110 (FED, EUROCLEAR).                           *
      *             DSN MSEC.PROD.RC.STPOS.DTC / .FED / .EUC (+1)      *
      * RECFM/LRECL: FB / 120                                          *
      *================================================================*
       01  RSP-STREET-POS-REC.
           05  RSP-DEPOSITORY          PIC X(04).
           05  RSP-CUSIP               PIC X(09).
           05  RSP-STMT-DATE           PIC 9(08).
           05  RSP-ISIN                PIC X(12).
           05  RSP-TOTAL-QTY           PIC S9(13)V9(04) COMP-3.
           05  RSP-FREE-QTY            PIC S9(13)V9(04) COMP-3.
           05  RSP-PLEDGED-QTY         PIC S9(13)V9(04) COMP-3.
           05  RSP-SEG-QTY             PIC S9(13)V9(04) COMP-3.
           05  RSP-DEL-PEND-QTY        PIC S9(13)V9(04) COMP-3.
           05  RSP-REC-PEND-QTY        PIC S9(13)V9(04) COMP-3.
           05  RSP-SOURCE-SEQ          PIC 9(07).
           05  RSP-ID-FLAG             PIC X(01).
               88  RSP-CUSIP-FROM-ISIN           VALUE 'I'.
               88  RSP-ISIN-NOT-FOUND            VALUE 'N'.
           05  FILLER                  PIC X(25).
