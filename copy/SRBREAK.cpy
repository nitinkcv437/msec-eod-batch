      *================================================================*
      * COPYBOOK   : SRBREAK                                           *
      * DESCRIPTION: STOCK RECORD BREAK - ONE RECORD PER CUSIP WHERE   *
      *              OWNERSHIP DOES NOT EQUAL LOCATION.  SRB500.       *
      *              DSN MSEC.PROD.SR.BREAKS(+1)                       *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  BRK-BREAK-REC.
           05  BRK-BUS-DATE            PIC 9(08).
           05  BRK-CUSIP               PIC X(09).
           05  BRK-SEC-TYPE            PIC X(02).
           05  BRK-TYPE                PIC X(02).
               88  BRK-SETTLED-BREAK             VALUE 'SD'.
               88  BRK-TRADE-DATE-BREAK          VALUE 'TD'.
               88  BRK-FIRM-SHORT                VALUE 'FS'.
           05  BRK-OWNER-QTY           PIC S9(13)V9(04) COMP-3.
           05  BRK-FIRM-QTY            PIC S9(13)V9(04) COMP-3.
           05  BRK-LOCATION-QTY        PIC S9(13)V9(04) COMP-3.
           05  BRK-DIFFERENCE          PIC S9(13)V9(04) COMP-3.
           05  BRK-OWNER-COUNT         PIC S9(07)       COMP-3.
           05  BRK-LOCATION-COUNT      PIC S9(07)       COMP-3.
           05  BRK-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  BRK-AGE-DAYS            PIC S9(03)       COMP-3.
           05  FILLER                  PIC X(74).
