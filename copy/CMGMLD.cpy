      *================================================================*
      * COPYBOOK   : CMGMLD                                            *
      * DESCRIPTION: DSNUTILB LOAD / UNLOAD RECORD FOR                 *
      *             MSEC.GL_ACCOUNT_MAP.  RECFM/LRECL: FB / 80         *
      * KEY (LOCAL STUB KSDS): OFFSET 0 LENGTH 8                       *
      *   (TXN CODE 4 + ACCT TYPE 2 + SEC TYPE 2)                      *
      *================================================================*
       01  GML-GLMAP-LOAD-REC.
           05  GML-TXN-CODE            PIC X(04).
           05  GML-ACCT-TYPE           PIC X(02).
           05  GML-SEC-TYPE            PIC X(02).
           05  GML-DR-GL-ACCOUNT       PIC X(10).
           05  GML-CR-GL-ACCOUNT       PIC X(10).
           05  GML-COST-CENTER         PIC X(06).
           05  GML-DESC                PIC X(30).
           05  FILLER                  PIC X(16).
