      *================================================================*
      * COPYBOOK   : TCREJCT                                           *
      * DESCRIPTION: TRADE REJECT RECORD - WRITTEN BY ANY TC PROGRAM   *
      *              THAT REJECTS A TRADE.  CARRIES THE ORIGINAL RAW   *
      *              INPUT IMAGE (UP TO 300 BYTES) FOR REPAIR.         *
      * RECFM/LRECL: FB / 450                                          *
      *----------------------------------------------------------------*
      * 1988-01-11 RJK  ORIGINAL                                       *
      * 2002-03-08 KAP  SEVERITY ADDED                        CHG09930 *
      *================================================================*
       01  REJ-REJECT-REC.
           05  REJ-BUS-DATE            PIC 9(08).
           05  REJ-STAGE               PIC X(08).
           05  REJ-PROGRAM             PIC X(08).
           05  REJ-SOURCE              PIC X(03).
           05  REJ-TRADE-ID            PIC X(16).
           05  REJ-ACCT-NO             PIC X(10).
           05  REJ-CUSIP               PIC X(09).
           05  REJ-CODE                PIC X(04).
           05  REJ-SEVERITY            PIC X(01).
               88  REJ-SEV-WARNING               VALUE 'W'.
               88  REJ-SEV-ERROR                 VALUE 'E'.
               88  REJ-SEV-FATAL                 VALUE 'F'.
           05  REJ-TEXT                PIC X(60).
           05  REJ-RAW-LENGTH          PIC 9(04).
           05  REJ-RAW-IMAGE           PIC X(300).
           05  FILLER                  PIC X(19).
