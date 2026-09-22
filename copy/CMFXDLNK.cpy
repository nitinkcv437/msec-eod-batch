      *================================================================*
      * COPYBOOK   : CMFXDLNK                                          *
      * DESCRIPTION: LINKAGE FOR DB2 I/O MODULE CMD030                 *
      *             (MSEC.FX_RATE).  RATE = USD PER 1 UNIT OF CCY.     *
      *             CALL 'CMD030' USING FD-FXRATE-PARMS.               *
      * FD-FUNCTION 'GETL' LATEST RATE ON OR BEFORE FD-RATE-DATE       *
      * FD-RETURN-CODE 00 FOUND  04 NOT FOUND  12 DB2 ERROR            *
      *================================================================*
       01  FD-FXRATE-PARMS.
           05  FD-FUNCTION             PIC X(04).
           05  FD-CCY                  PIC X(03).
           05  FD-RATE-DATE            PIC 9(08).
           05  FD-USD-RATE             PIC S9(05)V9(08) COMP-3.
           05  FD-ACTUAL-DATE          PIC 9(08).
           05  FD-RETURN-CODE          PIC 9(02).
               88  FD-FOUND                      VALUE 00.
               88  FD-NOT-FOUND                  VALUE 04.
               88  FD-DB-ERROR                   VALUE 12.
           05  FD-SQLCODE              PIC S9(09)       COMP.
