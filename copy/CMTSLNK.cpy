      *================================================================*
      * COPYBOOK   : CMTSLNK                                           *
      * DESCRIPTION: LINKAGE FOR HLASM ROUTINE CMASM02 (TIMESTAMP).    *
      *             CALL 'CMASM02' USING TS-TIMESTAMP-AREA.            *
      * STCK CONVERTED TO LOCAL TIME, DB2 FORMAT                       *
      * YYYY-MM-DD-HH.MM.SS.NNNNNN                                     *
      *================================================================*
       01  TS-TIMESTAMP-AREA.
           05  TS-TIMESTAMP            PIC X(26).
           05  TS-STCK-VALUE           PIC X(08).
