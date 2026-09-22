      *================================================================*
      * COPYBOOK   : CMPRLNK                                           *
      * DESCRIPTION: LINKAGE FOR DB2 I/O MODULE CMD020                 *
      *             (MSEC.SECURITY_PRICE).                             *
      *             CALL 'CMD020' USING PL-PRICE-PARMS.                *
      * PL-FUNCTION 'GET ' EXACT CUSIP + DATE                          *
      *             'GETL' LATEST PRICE ON OR BEFORE PL-PRICE-DATE     *
      * PL-RETURN-CODE 00 FOUND  04 NOT FOUND  12 DB2 ERROR            *
      *================================================================*
       01  PL-PRICE-PARMS.
           05  PL-FUNCTION             PIC X(04).
           05  PL-CUSIP                PIC X(09).
           05  PL-PRICE-DATE           PIC 9(08).
           05  PL-PRICE                PIC S9(09)V9(08) COMP-3.
           05  PL-PRICE-SOURCE         PIC X(04).
           05  PL-PRICE-CCY            PIC X(03).
           05  PL-ACTUAL-DATE          PIC 9(08).
           05  PL-RETURN-CODE          PIC 9(02).
               88  PL-FOUND                      VALUE 00.
               88  PL-NOT-FOUND                  VALUE 04.
               88  PL-DB-ERROR                   VALUE 12.
           05  PL-SQLCODE              PIC S9(09)       COMP.
