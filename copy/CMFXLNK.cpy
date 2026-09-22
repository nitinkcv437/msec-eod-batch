      *================================================================*
      * COPYBOOK   : CMFXLNK                                           *
      * DESCRIPTION: LINKAGE FOR FX CONVERSION MODULE CMU040.          *
      *             CALL 'CMU040' USING FX-CONVERT-PARMS.              *
      * RATES ARE USD PER 1 UNIT OF CCY (FROM CMD030 / FX_RATE).       *
      * CROSS RATES ARE COMPUTED THROUGH USD.  RATE CACHE OF 50        *
      * ENTRIES IS HELD IN WORKING STORAGE ACROSS CALLS.               *
      * AMOUNT-OUT IS ROUNDED HALF-UP TO 2 DECIMALS.                   *
      *================================================================*
       01  FX-CONVERT-PARMS.
           05  FX-FROM-CCY             PIC X(03).
           05  FX-TO-CCY               PIC X(03).
           05  FX-RATE-DATE            PIC 9(08).
           05  FX-AMOUNT-IN            PIC S9(15)V99    COMP-3.
           05  FX-RATE                 PIC S9(05)V9(08) COMP-3.
           05  FX-AMOUNT-OUT           PIC S9(15)V99    COMP-3.
           05  FX-RATE-ACTUAL-DATE     PIC 9(08).
           05  FX-RETURN-CODE          PIC 9(02).
               88  FX-OK                         VALUE 00.
               88  FX-STALE-RATE                 VALUE 02.
               88  FX-RATE-NOT-FOUND             VALUE 04.
               88  FX-DB-ERROR                   VALUE 12.
           05  FX-MESSAGE              PIC X(40).
