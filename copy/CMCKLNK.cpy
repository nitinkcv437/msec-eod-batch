      *================================================================*
      * COPYBOOK   : CMCKLNK                                           *
      * DESCRIPTION: LINKAGE FOR CHECKPOINT/RESTART MODULE CMU070.     *
      *             CALL 'CMU070' USING CK-CHECKPOINT-PARMS.           *
      * CK-FUNCTION 'INIT' READ CHECKPOINT KSDS (DDNAME CHKPTFL). IF AN*
      *             IN-FLIGHT RECORD EXISTS FOR THIS PROGRAM/JOB AND   *
      *             BUS DATE, SETS CK-RESTARTING AND RETURNS THE KEY.  *
      *             'TAKE' CALLED PER RECORD; EVERY CK-INTERVAL RECORDS*
      *             WRITES CK-CURRENT-KEY AND COUNT (CALLER MUST HAVE  *
      *             COMMITTED ITS OWN UPDATES FIRST).                  *
      *             'DONE' MARK CHECKPOINT COMPLETE.                   *
      *================================================================*
       01  CK-CHECKPOINT-PARMS.
           05  CK-FUNCTION             PIC X(04).
           05  CK-PROGRAM              PIC X(08).
           05  CK-JOBNAME              PIC X(08).
           05  CK-BUS-DATE             PIC 9(08).
           05  CK-INTERVAL             PIC S9(07)       COMP-3.
           05  CK-RECORD-COUNT         PIC S9(09)       COMP-3.
           05  CK-CURRENT-KEY          PIC X(40).
           05  CK-RESTART-FLAG         PIC X(01).
               88  CK-RESTARTING                 VALUE 'Y'.
               88  CK-FRESH-START                VALUE 'N'.
           05  CK-RESTART-KEY          PIC X(40).
           05  CK-RESTART-COUNT        PIC S9(09)       COMP-3.
           05  CK-CHECKPOINT-TAKEN     PIC X(01).
           05  CK-RETURN-CODE          PIC 9(02).
