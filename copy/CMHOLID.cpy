      *================================================================*
      * COPYBOOK   : CMHOLID                                           *
      * DESCRIPTION: HOLIDAY CALENDAR RECORD                           *
      *              DSN MSEC.PROD.CM.HOLIDAYS   DDNAME HOLIDAYS       *
      * RECFM/LRECL: FB / 40   SORTED ASCENDING BY HOL-DATE            *
      *----------------------------------------------------------------*
      * 1989-03-14 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      *================================================================*
       01  HOL-HOLIDAY-REC.
           05  HOL-DATE                PIC 9(08).
           05  HOL-CALENDAR            PIC X(04).
               88  HOL-NYSE                      VALUE 'NYSE' 'BOTH'.
               88  HOL-FED                       VALUE 'FED ' 'BOTH'.
           05  HOL-DESC                PIC X(28).
