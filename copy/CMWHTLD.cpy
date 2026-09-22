      *================================================================*
      * COPYBOOK   : CMWHTLD                                           *
      * DESCRIPTION: DSNUTILB LOAD / UNLOAD RECORD FOR                 *
      *             MSEC.WHT_RATE.  RECFM/LRECL: FB / 40               *
      * KEY (LOCAL STUB KSDS): OFFSET 0 LENGTH 15                      *
      *   (HOLDER 2 + ISSUER 2 + INCOME 2 + STATUS 1 + EFF DATE 8)     *
      *================================================================*
       01  WHL-WHTAX-LOAD-REC.
           05  WHL-HOLDER-COUNTRY      PIC X(02).
           05  WHL-ISSUER-COUNTRY      PIC X(02).
           05  WHL-INCOME-TYPE         PIC X(02).
           05  WHL-TAX-STATUS          PIC X(01).
           05  WHL-EFF-DATE            PIC 9(08).
           05  WHL-END-DATE            PIC 9(08).
           05  WHL-RATE                PIC S9(01)V9(04) COMP-3.
           05  WHL-TREATY-FLAG         PIC X(01).
           05  FILLER                  PIC X(13).
