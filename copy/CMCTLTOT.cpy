      *================================================================*
      * COPYBOOK   : CMCTLTOT                                          *
      * DESCRIPTION: CONTROL TOTAL RECORD - WRITTEN ONLY BY CMU080.    *
      *              EVERY PROGRAM POSTS ITS INPUT/OUTPUT COUNTS AND   *
      *              AMOUNT/QUANTITY HASHES; TCB900 AND CMB090         *
      *              RECONCILE STAGE TO STAGE.                         *
      *              DSN MSEC.PROD.CM.CTLTOTS(0)  (DISP=MOD)           *
      *              DDNAME CTLTOTS                                    *
      * RECFM/LRECL: FB / 120                                          *
      *================================================================*
       01  CTR-CONTROL-REC.
           05  CTR-BUS-DATE            PIC 9(08).
           05  CTR-JOBNAME             PIC X(08).
           05  CTR-PROGRAM             PIC X(08).
           05  CTR-STAGE               PIC X(08).
           05  CTR-COUNTER             PIC X(16).
           05  CTR-COUNT               PIC S9(09)       COMP-3.
           05  CTR-AMOUNT              PIC S9(15)V99    COMP-3.
           05  CTR-QTY-HASH            PIC S9(15)V9(04) COMP-3.
           05  CTR-TIMESTAMP           PIC X(26).
           05  FILLER                  PIC X(22).
