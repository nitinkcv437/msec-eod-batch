      *================================================================*
      * COPYBOOK   : RRCATL                                            *
      * DESCRIPTION: CONSOLIDATED EVENT REPORTING SUBMISSION LINE.     *
      *             PIPE-DELIMITED TEXT BUILT BY RRB200.               *
      *             DSN MSEC.PROD.RR.CATSUB(+1)                        *
      * RECFM/LRECL: FB / 400                                          *
      *----------------------------------------------------------------*
      * LINE LAYOUT (PIPE DELIMITED, NO TRAILING BLANKS INSIDE FIELDS):*
      *  H|FILEID|FIRMID|SUBMIT-DATE|RECORD-COUNT                      *
      *  E|SEQ|EVENT-TYPE|FIRM-REF|EVENT-TS-UTC|SYMBOL|SIDE|QTY|PRICE| *
      *    ACCT-TYPE|CAPACITY|DESK|DEPT|SESSION|HANDLING               *
      *  T|RECORD-COUNT|QTY-HASH                                       *
      *================================================================*
       01  RRC-SUBMISSION-LINE.
           05  RRC-LINE-TYPE           PIC X(01).
           05  RRC-LINE-TEXT           PIC X(399).
