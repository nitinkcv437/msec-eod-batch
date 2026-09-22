      *================================================================*
      * COPYBOOK   : CMCHKPT                                           *
      * DESCRIPTION: CHECKPOINT / RESTART RECORD - MAINTAINED BY       *
      *              CMU070.  ONE RECORD PER PROGRAM (KSDS).           *
      *              DSN MSEC.PROD.CM.CHKPT.KSDS      DDNAME CHKPTFL   *
      * KEY        : CKR-KEY  OFFSET 0 LENGTH 16 (PROGRAM + JOBNAME)   *
      * RECFM/LRECL: F / 100                                           *
      *================================================================*
       01  CKR-CHECKPOINT-REC.
           05  CKR-KEY.
               10  CKR-PROGRAM         PIC X(08).
               10  CKR-JOBNAME         PIC X(08).
           05  CKR-BUS-DATE            PIC 9(08).
           05  CKR-TIMESTAMP           PIC X(26).
           05  CKR-RECORD-COUNT        PIC 9(09).
           05  CKR-RESTART-KEY         PIC X(40).
           05  CKR-STATUS              PIC X(01).
               88  CKR-IN-FLIGHT                 VALUE 'F'.
               88  CKR-COMPLETE                  VALUE 'C'.
