      *================================================================*
      * COPYBOOK   : CMABLNK                                           *
      * DESCRIPTION: LINKAGE FOR ABEND HANDLER CMU050.                 *
      *             CALL 'CMU050' USING AB-ABEND-PARMS.                *
      * WRITES DIAGNOSTICS TO SYSOUT AND THE AUDIT LOG, THEN ISSUES    *
      * CALL 'CEE3ABD' WITH AB-ABEND-CODE (USER ABEND U0NNN).          *
      * STANDARD CODES: 1001 FILE OPEN  1002 FILE I/O  1003 DB2        *
      *   1004 CONTROL TOTAL OUT OF BALANCE  1005 BAD DATE CARD        *
      *   1006 SEQUENCE ERROR  1007 TABLE OVERFLOW  1008 LOGIC         *
      *   1009 RESTART ERROR  1010 CALLED MODULE FAILURE               *
      *================================================================*
       01  AB-ABEND-PARMS.
           05  AB-PROGRAM              PIC X(08).
           05  AB-PARAGRAPH            PIC X(30).
           05  AB-ABEND-CODE           PIC 9(04).
           05  AB-FILE-STATUS          PIC X(02).
           05  AB-DDNAME               PIC X(08).
           05  AB-SQLCODE              PIC S9(09)       COMP.
           05  AB-KEY                  PIC X(40).
           05  AB-MESSAGE              PIC X(80).
