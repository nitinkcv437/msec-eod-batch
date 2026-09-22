      *================================================================*
      * COPYBOOK   : RCBRKM                                            *
      * DESCRIPTION: RECONCILIATION BREAK MASTER (VSAM KSDS).  OPEN AND*
      *             CLOSED STREET-SIDE BREAKS CARRIED ACROSS DAYS.     *
      *             DSN MSEC.PROD.RC.BRKMAST.KSDS    DDNAME RCBRKMST   *
      * KEY        : RBM-KEY  OFFSET 0 LENGTH 22                       *
      * RECFM/LRECL: F / 200                                           *
      *----------------------------------------------------------------*
      * KEY: TYPE 'SP' POSITION + DEPOSITORY + CUSIP (LEFT JUST.)      *
      *      TYPE 'CS' CASH     + CCY (+ BLANK) + BANK/BOOK REF        *
      *================================================================*
       01  RBM-BREAK-REC.
           05  RBM-KEY.
               10  RBM-BREAK-CLASS     PIC X(02).
                   88  RBM-POSITION-BREAK        VALUE 'SP'.
                   88  RBM-CASH-BREAK            VALUE 'CS'.
               10  RBM-DEPOSITORY      PIC X(04).
               10  RBM-ITEM-ID         PIC X(16).
           05  RBM-CATEGORY            PIC X(02).
               88  RBM-MISSING-AT-STREET         VALUE 'MS'.
               88  RBM-MISSING-IN-BOOKS          VALUE 'MB'.
               88  RBM-QTY-DIFFERENCE            VALUE 'QD'.
               88  RBM-BANK-UNMATCHED            VALUE 'BU'.
               88  RBM-BOOK-UNMATCHED            VALUE 'KU'.
               88  RBM-AMOUNT-DIFFERENCE         VALUE 'AD'.
           05  RBM-STATUS              PIC X(01).
               88  RBM-OPEN                      VALUE 'O'.
               88  RBM-CLOSED                    VALUE 'C'.
               88  RBM-WRITTEN-OFF               VALUE 'W'.
           05  RBM-FIRST-SEEN-DATE     PIC 9(08).
           05  RBM-LAST-SEEN-DATE      PIC 9(08).
           05  RBM-CLOSED-DATE         PIC 9(08).
           05  RBM-AGE-BUS-DAYS        PIC S9(03)       COMP-3.
           05  RBM-STREET-QTY          PIC S9(13)V9(04) COMP-3.
           05  RBM-BOOK-QTY            PIC S9(13)V9(04) COMP-3.
           05  RBM-DIFF-QTY            PIC S9(13)V9(04) COMP-3.
           05  RBM-STREET-AMOUNT       PIC S9(15)V99    COMP-3.
           05  RBM-BOOK-AMOUNT         PIC S9(15)V99    COMP-3.
           05  RBM-DIFF-AMOUNT         PIC S9(15)V99    COMP-3.
           05  RBM-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  RBM-CCY                 PIC X(03).
           05  RBM-ASSIGNED-TO         PIC X(08).
           05  RBM-ESCALATION-LVL      PIC 9(01).
           05  RBM-LAST-UPD-JOB        PIC X(08).
           05  RBM-COMMENT             PIC X(40).
           05  FILLER                  PIC X(26).
