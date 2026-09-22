      *================================================================*
      * COPYBOOK   : CMPRCLD                                           *
      * DESCRIPTION: DSNUTILB LOAD / UNLOAD RECORD FOR                 *
      *             MSEC.SECURITY_PRICE.  RECFM/LRECL: FB / 40         *
      * KEY (LOCAL STUB KSDS): OFFSET 0 LENGTH 17 (CUSIP + DATE)       *
      *================================================================*
       01  PRL-PRICE-LOAD-REC.
           05  PRL-CUSIP               PIC X(09).
           05  PRL-PRICE-DATE          PIC 9(08).
           05  PRL-PRICE               PIC S9(09)V9(08) COMP-3.
           05  PRL-SOURCE              PIC X(04).
           05  PRL-CCY                 PIC X(03).
           05  FILLER                  PIC X(07).
