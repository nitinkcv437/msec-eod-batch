      *================================================================*
      * COPYBOOK   : SRBRKD                                            *
      * DESCRIPTION: STOCK RECORD BREAK DETAIL.  FOR EVERY CUSIP IN    *
      *              BREAK SRB500 WRITES ONE 'H' RECORD (THE BREAK)    *
      *              FOLLOWED BY ONE 'D' RECORD PER POSITION ROW OF    *
      *              THAT CUSIP.  INPUT TO BREAK REPORT SRR510.        *
      *              DSN MSEC.PROD.SR.BREAKS.DETAIL(+1)                *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  BKD-DETAIL-REC.
           05  BKD-REC-TYPE            PIC X(01).
               88  BKD-BREAK-HEADER              VALUE 'H'.
               88  BKD-POSITION-DETAIL           VALUE 'D'.
           05  BKD-CUSIP               PIC X(09).
           05  BKD-BODY                PIC X(140).
           05  BKD-HDR-BODY      REDEFINES BKD-BODY.
               10  BKD-BREAK-IMAGE     PIC X(76).
               10  FILLER              PIC X(64).
           05  BKD-DTL-BODY      REDEFINES BKD-BODY.
               10  BKD-ACCT-NO         PIC X(10).
               10  BKD-LOCATION        PIC X(04).
               10  BKD-ACCT-TYPE       PIC X(02).
               10  BKD-ROLE            PIC X(01).
                   88  BKD-ROLE-CLIENT           VALUE 'C'.
                   88  BKD-ROLE-FIRM             VALUE 'F'.
                   88  BKD-ROLE-LOCATION         VALUE 'L'.
               10  BKD-TD-QTY          PIC S9(11)V9(04) COMP-3.
               10  BKD-SD-QTY          PIC S9(11)V9(04) COMP-3.
               10  BKD-PEND-IN-QTY     PIC S9(11)V9(04) COMP-3.
               10  BKD-PEND-OUT-QTY    PIC S9(11)V9(04) COMP-3.
               10  BKD-LAST-ACTV-DATE  PIC 9(08).
               10  FILLER              PIC X(83).
