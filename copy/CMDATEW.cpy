      *================================================================*
      * COPYBOOK   : CMDATEW                                           *
      * DESCRIPTION: BUSINESS DATE CARD - PRODUCED BY CMB010 (MSCMD010)*
      *              AND READ BY EVERY PROGRAM IN THE EOD CYCLE VIA    *
      *              DDNAME DATECARD.  DSN MSEC.PROD.CM.DATECARD(0)    *
      * RECFM/LRECL: FB / 80                                           *
      *----------------------------------------------------------------*
      * 1989-03-14 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K - EXPANDED ALL DATES TO CCYYMMDD  CHG04471 *
      * 2011-06-20 SPA  ADDED DC-REGION FOR LONDON CYCLE      CHG21877 *
      *================================================================*
       01  DC-DATE-CARD.
           05  DC-REC-ID               PIC X(04).
               88  DC-VALID-CARD                 VALUE 'DATE'.
           05  DC-BUS-DATE             PIC 9(08).
           05  DC-BUS-DATE-R  REDEFINES DC-BUS-DATE.
               10  DC-BUS-CCYY         PIC 9(04).
               10  DC-BUS-MM           PIC 9(02).
               10  DC-BUS-DD           PIC 9(02).
           05  DC-PREV-BUS-DATE        PIC 9(08).
           05  DC-NEXT-BUS-DATE        PIC 9(08).
           05  DC-CAL-DATE             PIC 9(08).
           05  DC-CYCLE-TYPE           PIC X(01).
               88  DC-DAILY-CYCLE                VALUE 'D'.
               88  DC-MONTH-END                  VALUE 'M' 'Q' 'Y'.
               88  DC-QTR-END                    VALUE 'Q' 'Y'.
               88  DC-YEAR-END                   VALUE 'Y'.
           05  DC-JULIAN-DATE          PIC 9(07).
           05  DC-DAY-OF-WEEK          PIC 9(01).
      *        1=MONDAY ... 7=SUNDAY
           05  DC-RUN-NUMBER           PIC 9(03).
           05  DC-RERUN-FLAG           PIC X(01).
               88  DC-RERUN                      VALUE 'Y'.
           05  DC-REGION               PIC X(04).
           05  FILLER                  PIC X(27).
