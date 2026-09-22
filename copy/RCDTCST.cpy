      *================================================================*
      * COPYBOOK   : RCDTCST                                           *
      * DESCRIPTION: DEPOSITORY (DTC) PARTICIPANT POSITION STATEMENT.  *
      *             RECEIVED AS MSEC.PROD.RC.DTCSTMT.RAW(+1)           *
      *             HEADER / DETAIL / TRAILER, VARIABLE LENGTH RECORDS.*
      *             DETAIL CARRIES 1-20 BALANCE SEGMENTS (ODO).        *
      * RECFM/LRECL: VB / 417  (MAX DATA 413 + 4 BYTE RDW)             *
      *----------------------------------------------------------------*
      * SEGMENT TYPES: FR FREE  PL PLEDGED  SG SEGREGATED              *
      *                DP DELIVER PENDING  RP RECEIVE PENDING          *
      * SUM OF SEGMENTS = DTD-TOTAL-QTY.                               *
      *----------------------------------------------------------------*
      * 1996-03-18 DWB  ORIGINAL - REPLACED CCF TAPE          CHG02101 *
      * 2009-07-20 SPA  SEGMENT TABLE 10 -> 20               CHG18866  *
      *================================================================*
       01  DTH-HEADER-REC.
           05  DTH-REC-TYPE            PIC X(01).
               88  DTH-HEADER                    VALUE 'H'.
           05  DTH-PARTICIPANT         PIC X(04).
           05  DTH-STMT-DATE           PIC 9(08).
           05  DTH-CREATE-TS           PIC X(14).
           05  DTH-FILE-SEQ            PIC 9(05).
           05  FILLER                  PIC X(08).
       01  DTD-DETAIL-REC.
           05  DTD-REC-TYPE            PIC X(01).
               88  DTD-DETAIL                    VALUE 'D'.
           05  DTD-PARTICIPANT         PIC X(04).
           05  DTD-STMT-DATE           PIC 9(08).
           05  DTD-CUSIP               PIC X(09).
           05  DTD-TOTAL-QTY           PIC S9(13)V9(04) COMP-3.
           05  DTD-SEG-COUNT           PIC 9(02).
           05  DTD-SEGMENT             OCCURS 1 TO 20 TIMES
                                       DEPENDING ON DTD-SEG-COUNT.
               10  DTD-SEG-TYPE        PIC X(02).
               10  DTD-SEG-QTY         PIC S9(13)V9(04) COMP-3.
               10  DTD-SEG-REF         PIC X(08).
       01  DTT-TRAILER-REC.
           05  DTT-REC-TYPE            PIC X(01).
               88  DTT-TRAILER                   VALUE 'T'.
           05  DTT-PARTICIPANT         PIC X(04).
           05  DTT-DETAIL-COUNT        PIC 9(09).
           05  DTT-QTY-HASH            PIC S9(15)V9(04) COMP-3.
