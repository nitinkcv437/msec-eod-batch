      *================================================================*
      * COPYBOOK   : TCMANFD                                           *
      * DESCRIPTION: MANUAL TRADE / ADJUSTMENT CARD IMAGE.  KEYED BY   *
      *              OPERATIONS VIA ISPF PANEL INTO                    *
      *              MSEC.PROD.TC.MANUAL.CARDS(+1).  AN ASTERISK IN    *
      *              COLUMN 1 IS A COMMENT CARD.                       *
      * RECFM/LRECL: FB / 80                                           *
      *----------------------------------------------------------------*
      * 1988-01-11 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2014-03-03 SPA  MAKER/CHECKER FIELDS                  CHG26120 *
      *================================================================*
       01  MAN-CARD-REC.
           05  MAN-REC-TYPE            PIC X(02).
               88  MAN-COMMENT-CARD              VALUE '* ' '**'.
               88  MAN-TRADE-CARD                VALUE 'TR'.
               88  MAN-CANCEL-CARD               VALUE 'CX'.
           05  MAN-REF                 PIC X(10).
           05  MAN-ACCT                PIC X(10).
           05  MAN-CUSIP               PIC X(09).
           05  MAN-SIDE                PIC X(01).
           05  MAN-QTY                 PIC 9(09).
           05  MAN-PRICE               PIC 9(05)V9(04).
           05  MAN-TRADE-DATE          PIC 9(08).
           05  MAN-SETTLE-DATE         PIC 9(08).
           05  MAN-ENTERED-BY          PIC X(06).
           05  MAN-APPROVED-BY         PIC X(06).
           05  FILLER                  PIC X(02).
