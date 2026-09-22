      *================================================================*
      * COPYBOOK   : MGCEVT                                            *
      * DESCRIPTION: MARGIN CALL EVENT - ONE RECORD PER CALL TOUCHED   *
      *             BY MGB200 IN THE DAY (NEW, UPDATED, EXTENDED, MET, *
      *             LIQUIDATION, CANCELLED, EXTENSION REJECTED).       *
      *             READ BY MGR210 (CALL REPORT).                      *
      *             DSN MSEC.PROD.MG.CALLEVT(+1)                       *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * 1996-09-30 LFM  ORIGINAL (SPLIT FROM MGB100)          CHG02390 *
      * 2004-03-15 KAP  EXTENSION COUNT, REJECTED EXTENSIONS  CHG12230 *
      *================================================================*
       01  MCE-CALL-EVENT-REC.
           05  MCE-BUS-DATE            PIC 9(08).
           05  MCE-EVENT-CODE          PIC X(02).
               88  MCE-EVT-NEW                   VALUE 'NW'.
               88  MCE-EVT-UPDATED               VALUE 'UP'.
               88  MCE-EVT-EXTENDED              VALUE 'EX'.
               88  MCE-EVT-MET                   VALUE 'MT'.
               88  MCE-EVT-LIQUIDATE             VALUE 'LQ'.
               88  MCE-EVT-CANCELLED             VALUE 'CX'.
               88  MCE-EVT-EXT-REJECTED          VALUE 'XR'.
           05  MCE-CALL-KEY.
               10  MCE-ACCT-NO         PIC X(10).
               10  MCE-CALL-TYPE       PIC X(02).
               10  MCE-ISSUE-DATE      PIC 9(08).
           05  MCE-BRANCH              PIC X(03).
           05  MCE-REP                 PIC X(04).
           05  MCE-STATUS              PIC X(01).
           05  MCE-DUE-DATE            PIC 9(08).
           05  MCE-CALL-AMOUNT         PIC S9(13)V99    COMP-3.
           05  MCE-AMOUNT-MET          PIC S9(13)V99    COMP-3.
           05  MCE-CURR-DEFICIT        PIC S9(15)V99    COMP-3.
           05  MCE-EQUITY              PIC S9(15)V99    COMP-3.
           05  MCE-AGE-BUS-DAYS        PIC S9(03)       COMP-3.
           05  MCE-EXTENSION-COUNT     PIC S9(01)       COMP-3.
           05  MCE-MET-DATE            PIC 9(08).
           05  MCE-REQ-STATUS          PIC X(01).
           05  MCE-MESSAGE             PIC X(40).
           05  FILLER                  PIC X(18).
