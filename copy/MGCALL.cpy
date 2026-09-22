      *================================================================*
      * COPYBOOK   : MGCALL                                            *
      * DESCRIPTION: MARGIN CALL MASTER (VSAM KSDS) - OPEN AND CLOSED  *
      *             CALLS CARRIED ACROSS DAYS.  MGB200 MAINTAINS.      *
      *             DSN MSEC.PROD.MG.CALLS.KSDS      DDNAME MGCALLS    *
      * KEY        : MGC-KEY  OFFSET 0 LENGTH 20                       *
      * RECFM/LRECL: F / 150                                           *
      *----------------------------------------------------------------*
      * STATUS  O OPEN  M MET  X EXTENDED  L LIQUIDATION  C CANCELLED  *
      *================================================================*
       01  MGC-CALL-REC.
           05  MGC-KEY.
               10  MGC-ACCT-NO         PIC X(10).
               10  MGC-CALL-TYPE       PIC X(02).
                   88  MGC-REGT-CALL             VALUE 'RT'.
                   88  MGC-HOUSE-CALL            VALUE 'HM'.
                   88  MGC-MIN-EQUITY-CALL       VALUE 'ME'.
               10  MGC-ISSUE-DATE      PIC 9(08).
           05  MGC-DUE-DATE            PIC 9(08).
           05  MGC-CALL-AMOUNT         PIC S9(13)V99    COMP-3.
           05  MGC-AMOUNT-MET          PIC S9(13)V99    COMP-3.
           05  MGC-STATUS              PIC X(01).
               88  MGC-OPEN                      VALUE 'O'.
               88  MGC-MET                       VALUE 'M'.
               88  MGC-EXTENDED                  VALUE 'X'.
               88  MGC-LIQUIDATE                 VALUE 'L'.
               88  MGC-CANCELLED                 VALUE 'C'.
           05  MGC-AGE-BUS-DAYS        PIC S9(03)       COMP-3.
           05  MGC-EXTENSION-COUNT     PIC S9(01)       COMP-3.
           05  MGC-LAST-EQUITY         PIC S9(15)V99    COMP-3.
           05  MGC-LAST-DEFICIT        PIC S9(15)V99    COMP-3.
           05  MGC-MET-DATE            PIC 9(08).
           05  MGC-LAST-UPD-DATE       PIC 9(08).
           05  MGC-LAST-UPD-JOB        PIC X(08).
           05  FILLER                  PIC X(60).
