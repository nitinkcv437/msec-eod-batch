      *================================================================*
      * COPYBOOK   : CMAULNK                                           *
      * DESCRIPTION: LINKAGE FOR AUDIT WRITER CMU060.                  *
      *             CALL 'CMU060' USING AU-AUDIT-PARMS.                *
      * AU-FUNCTION 'WRIT' WRITE ONE AUDIT RECORD (OPENS ON FIRST      *
      *             CALL, DDNAME AUDITLOG, EXTEND MODE)                *
      *             'CLOS' CLOSE THE AUDIT FILE (CALL AT END OF JOB)   *
      *================================================================*
       01  AU-AUDIT-PARMS.
           05  AU-FUNCTION             PIC X(04).
           05  AU-PROGRAM              PIC X(08).
           05  AU-EVENT                PIC X(08).
           05  AU-SEVERITY             PIC X(01).
           05  AU-BUS-DATE             PIC 9(08).
           05  AU-KEY                  PIC X(40).
           05  AU-MESSAGE              PIC X(80).
           05  AU-RETURN-CODE          PIC 9(02).
