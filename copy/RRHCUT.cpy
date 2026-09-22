      *================================================================*
      * COPYBOOK   : RRHCUT                                            *
      * DESCRIPTION: NET CAPITAL HAIRCUT DETAIL - FIRM INVENTORY       *
      *             (RULE 15C3-1 STYLE).  WRITTEN BY RRB500.           *
      *             DSN MSEC.PROD.RR.HAIRCUT(+1)                       *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  RRH-HAIRCUT-REC.
           05  RRH-BUS-DATE            PIC 9(08).
           05  RRH-ACCT-NO             PIC X(10).
           05  RRH-CUSIP               PIC X(09).
           05  RRH-SEC-TYPE            PIC X(02).
           05  RRH-ISSUER-ID           PIC X(06).
           05  RRH-BUCKET              PIC X(04).
           05  RRH-QTY                 PIC S9(11)V9(04) COMP-3.
           05  RRH-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  RRH-HAIRCUT-PCT         PIC S9(03)V9(04) COMP-3.
           05  RRH-HAIRCUT-AMT         PIC S9(15)V99    COMP-3.
           05  RRH-UNDUE-CONC-AMT      PIC S9(15)V99    COMP-3.
           05  RRH-LONG-SHORT          PIC X(01).
           05  FILLER                  PIC X(71).
