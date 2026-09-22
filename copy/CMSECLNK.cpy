      *================================================================*
      * COPYBOOK   : CMSECLNK                                          *
      * DESCRIPTION: LINKAGE FOR DB2 I/O MODULE CMD010                 *
      *             (MSEC.SECURITY_MASTER).                            *
      *             CALL 'CMD010' USING SL-SECURITY-PARMS.             *
      * SL-FUNCTION 'GET ' BY SL-KEY-CUSIP                             *
      *             'GETI' BY SL-KEY-ISIN                              *
      *             'GETS' BY SL-KEY-SYMBOL (ACTIVE ROW ONLY)          *
      * SL-RETURN-CODE 00 FOUND  04 NOT FOUND  12 DB2 ERROR (SQLCODE)  *
      * ON RETURN THE ROW IS IN SL-SEC-DATA (FORMAT OF CMSECMS).       *
      *================================================================*
       01  SL-SECURITY-PARMS.
           05  SL-FUNCTION             PIC X(04).
           05  SL-KEY-CUSIP            PIC X(09).
           05  SL-KEY-ISIN             PIC X(12).
           05  SL-KEY-SYMBOL           PIC X(08).
           05  SL-RETURN-CODE          PIC 9(02).
               88  SL-FOUND                      VALUE 00.
               88  SL-NOT-FOUND                  VALUE 04.
               88  SL-DB-ERROR                   VALUE 12.
           05  SL-SQLCODE              PIC S9(09)       COMP.
           05  SL-SEC-DATA             PIC X(250).
