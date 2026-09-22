      *================================================================*
      * COPYBOOK   : CMGMLNK                                           *
      * DESCRIPTION: LINKAGE FOR DB2 I/O MODULE CMD050                 *
      *             (MSEC.GL_ACCOUNT_MAP).                             *
      *             CALL 'CMD050' USING GM-GLMAP-PARMS.                *
      * LOOKUP: MOST SPECIFIC ROW WINS -                               *
      *   TXN-CODE + ACCT-TYPE + SEC-TYPE, THEN TXN + ACCT-TYPE + '**',*
      *   THEN TXN + '**' + SEC-TYPE, THEN TXN + '**' + '**'.          *
      * GM-RETURN-CODE 00 FOUND  04 NOT FOUND  12 DB2 ERROR            *
      *================================================================*
       01  GM-GLMAP-PARMS.
           05  GM-TXN-CODE             PIC X(04).
           05  GM-ACCT-TYPE            PIC X(02).
           05  GM-SEC-TYPE             PIC X(02).
           05  GM-DR-GL-ACCOUNT        PIC X(10).
           05  GM-CR-GL-ACCOUNT        PIC X(10).
           05  GM-COST-CENTER          PIC X(06).
           05  GM-DESC                 PIC X(30).
           05  GM-RETURN-CODE          PIC 9(02).
               88  GM-FOUND                      VALUE 00.
               88  GM-NOT-FOUND                  VALUE 04.
               88  GM-DB-ERROR                   VALUE 12.
           05  GM-SQLCODE              PIC S9(09)       COMP.
