      *================================================================*
      * COPYBOOK   : CMAUDIT                                           *
      * DESCRIPTION: AUDIT TRAIL RECORD - WRITTEN ONLY BY CMU060.      *
      *              DSN MSEC.PROD.CM.AUDIT.LOG  (DISP=MOD)            *
      *              DDNAME AUDITLOG                                   *
      * RECFM/LRECL: FB / 200                                          *
      *================================================================*
       01  AUD-AUDIT-REC.
           05  AUD-TIMESTAMP           PIC X(26).
           05  AUD-JOBNAME             PIC X(08).
           05  AUD-STEPNAME            PIC X(08).
           05  AUD-PROGRAM             PIC X(08).
           05  AUD-EVENT               PIC X(08).
           05  AUD-SEVERITY            PIC X(01).
               88  AUD-INFO                      VALUE 'I'.
               88  AUD-WARNING                   VALUE 'W'.
               88  AUD-ERROR                     VALUE 'E'.
               88  AUD-ABEND                     VALUE 'A'.
           05  AUD-BUS-DATE            PIC 9(08).
           05  AUD-KEY                 PIC X(40).
           05  AUD-MESSAGE             PIC X(80).
           05  FILLER                  PIC X(13).
