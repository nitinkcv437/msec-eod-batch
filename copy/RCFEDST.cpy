      *================================================================*
      * COPYBOOK   : RCFEDST                                           *
      * DESCRIPTION: FEDERAL RESERVE BOOK-ENTRY SECURITIES STATEMENT   *
      *             (GOVERNMENT SECURITIES, PAR AMOUNTS).              *
      *             RECEIVED AS MSEC.PROD.RC.FEDSTMT.RAW(+1)           *
      * RECFM/LRECL: FB / 100                                          *
      *----------------------------------------------------------------*
      * 1998-01-12 TLM  ORIGINAL                              CHG03880 *
      *================================================================*
       01  FED-STMT-REC.
           05  FED-REC-TYPE            PIC X(02).
               88  FED-HEADER                    VALUE '00'.
               88  FED-DETAIL                    VALUE '10'.
               88  FED-TRAILER                   VALUE '99'.
           05  FED-ABA                 PIC X(09).
           05  FED-STMT-DATE           PIC 9(08).
           05  FED-CUSIP               PIC X(09).
           05  FED-PAR-AMOUNT          PIC S9(13)V99    COMP-3.
           05  FED-PLEDGED-PAR         PIC S9(13)V99    COMP-3.
           05  FED-REC-COUNT           PIC 9(07).
           05  FILLER                  PIC X(49).
