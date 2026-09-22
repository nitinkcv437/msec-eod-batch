      *================================================================*
      * COPYBOOK   : SRREJCT                                           *
      * DESCRIPTION: STOCK RECORD ACTIVITY REJECT.  WRITTEN BY SRB100  *
      *              FOR EVERY ACTIVITY LEG THAT FAILS VALIDATION OR   *
      *              BELONGS TO A REFERENCE THAT DOES NOT BALANCE.     *
      *              DSN MSEC.PROD.SR.ACTV.REJECT(+1)                  *
      * RECFM/LRECL: FB / 250                                          *
      *----------------------------------------------------------------*
      * 1987-10-12 RJK  ORIGINAL                                       *
      * 2002-03-08 KAP  SEVERITY ADDED                        CHG09930 *
      *================================================================*
       01  SRJ-REJECT-REC.
           05  SRJ-ACTIVITY            PIC X(200).
           05  SRJ-BUS-DATE            PIC 9(08).
           05  SRJ-CODE                PIC X(04).
           05  SRJ-SEVERITY            PIC X(01).
               88  SRJ-SEV-WARNING               VALUE 'W'.
               88  SRJ-SEV-ERROR                 VALUE 'E'.
           05  SRJ-TEXT                PIC X(30).
           05  FILLER                  PIC X(07).
