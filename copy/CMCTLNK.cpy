      *================================================================*
      * COPYBOOK   : CMCTLNK                                           *
      * DESCRIPTION: LINKAGE FOR CONTROL TOTAL MODULE CMU080.          *
      *             CALL 'CMU080' USING CT-CONTROL-PARMS.              *
      * CT-FUNCTION 'POST' APPEND ONE CONTROL TOTAL RECORD (DDNAME     *
      *             CTLTOTS, EXTEND MODE)                              *
      *             'GET ' RETURN THE LATEST POSTED VALUES FOR         *
      *             CT-STAGE/CT-COUNTER-NAME FOR TODAY (SCANS FILE)    *
      *             'CLOS' CLOSE THE FILE                              *
      *================================================================*
       01  CT-CONTROL-PARMS.
           05  CT-FUNCTION             PIC X(04).
           05  CT-BUS-DATE             PIC 9(08).
           05  CT-PROGRAM              PIC X(08).
           05  CT-STAGE                PIC X(08).
           05  CT-COUNTER-NAME         PIC X(16).
           05  CT-COUNT                PIC S9(09)       COMP-3.
           05  CT-AMOUNT               PIC S9(15)V99    COMP-3.
           05  CT-QTY-HASH             PIC S9(15)V9(04) COMP-3.
           05  CT-RETURN-CODE          PIC 9(02).
               88  CT-OK                         VALUE 00.
               88  CT-NOT-FOUND                  VALUE 04.
               88  CT-IO-ERROR                   VALUE 12.
