      *================================================================*
      * COPYBOOK   : SWSTAT                                            *
      * DESCRIPTION: INBOUND STATUS / CONFIRMATION EVENT, ONE PER      *
      *             PARSED INBOUND MESSAGE.  WRITTEN BY SWB200.        *
      *             DSN MSEC.PROD.SW.STATEVT(+1)                       *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  SWS-STATUS-REC.
           05  SWS-BUS-DATE            PIC 9(08).
           05  SWS-MSG-SEQ             PIC 9(08).
           05  SWS-MSG-TYPE            PIC X(03).
           05  SWS-RELATED-REF         PIC X(16).
           05  SWS-CUST-REF            PIC X(16).
           05  SWS-EVENT               PIC X(02).
               88  SWS-EV-MATCHED                VALUE 'MA'.
               88  SWS-EV-UNMATCHED              VALUE 'NM'.
               88  SWS-EV-PENDING                VALUE 'PE'.
               88  SWS-EV-SETTLED                VALUE 'ST'.
               88  SWS-EV-PARTIAL                VALUE 'PS'.
               88  SWS-EV-REJECTED               VALUE 'RJ'.
               88  SWS-EV-UNKNOWN-REF            VALUE 'UR'.
           05  SWS-STATUS-CODE         PIC X(04).
           05  SWS-REASON-CODE         PIC X(04).
           05  SWS-QTY                 PIC S9(11)V9(04) COMP-3.
           05  SWS-AMOUNT              PIC S9(15)V99    COMP-3.
           05  SWS-EFF-DATE            PIC 9(08).
           05  SWS-APPLIED-FLAG        PIC X(01).
           05  FILLER                  PIC X(63).
