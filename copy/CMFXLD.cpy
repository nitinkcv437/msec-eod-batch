      *================================================================*
      * COPYBOOK   : CMFXLD                                            *
      * DESCRIPTION: DSNUTILB LOAD / UNLOAD RECORD FOR                 *
      *             MSEC.FX_RATE.  RECFM/LRECL: FB / 30                *
      * KEY (LOCAL STUB KSDS): OFFSET 0 LENGTH 11 (CCY + DATE)         *
      *================================================================*
       01  FXL-FXRATE-LOAD-REC.
           05  FXL-CCY                 PIC X(03).
           05  FXL-RATE-DATE           PIC 9(08).
           05  FXL-USD-RATE            PIC S9(05)V9(08) COMP-3.
           05  FILLER                  PIC X(12).
