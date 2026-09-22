      *================================================================*
      * COPYBOOK   : RCEUCST                                           *
      * DESCRIPTION: EUROCLEAR STATEMENT OF HOLDINGS (MT535 FLATTENED) *
      *             RECEIVED AS MSEC.PROD.RC.EUCSTMT.RAW(+1)           *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * SECURITIES ARE IDENTIFIED BY ISIN ONLY.  QUANTITIES ARE TEXT IN*
      * SWIFT FORMAT: DIGITS WITH A DECIMAL COMMA, LEFT JUSTIFIED, E.G.*
      * '1500,' OR '1234,5'.                                           *
      *----------------------------------------------------------------*
      * 2003-10-06 KAP  ORIGINAL                              CHG11702 *
      *================================================================*
       01  EUC-STMT-REC.
           05  EUC-REC-TYPE            PIC X(03).
               88  EUC-HEADER                    VALUE 'HDR'.
               88  EUC-HOLDING                   VALUE 'HLD'.
               88  EUC-TRAILER                   VALUE 'TRL'.
           05  EUC-BODY                PIC X(147).
           05  EUC-HDR-BODY      REDEFINES EUC-BODY.
               10  EUC-HDR-ACCOUNT     PIC X(08).
               10  EUC-HDR-STMT-DATE   PIC X(08).
               10  EUC-HDR-STMT-TYPE   PIC X(04).
               10  EUC-HDR-PAGE        PIC X(05).
               10  FILLER              PIC X(122).
           05  EUC-HLD-BODY      REDEFINES EUC-BODY.
               10  EUC-HLD-ACCOUNT     PIC X(08).
               10  EUC-HLD-ISIN        PIC X(12).
               10  EUC-HLD-QTY-TYPE    PIC X(04).
               10  EUC-HLD-AGGR-QTY    PIC X(18).
               10  EUC-HLD-AVAIL-QTY   PIC X(18).
               10  EUC-HLD-NOTAVL-QTY  PIC X(18).
               10  EUC-HLD-PRICE       PIC X(15).
               10  EUC-HLD-CCY         PIC X(03).
               10  EUC-HLD-DESC        PIC X(35).
               10  FILLER              PIC X(16).
           05  EUC-TRL-BODY      REDEFINES EUC-BODY.
               10  EUC-TRL-COUNT       PIC X(07).
               10  FILLER              PIC X(140).
