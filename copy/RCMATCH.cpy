      *================================================================*
      * COPYBOOK   : RCMATCH                                           *
      * DESCRIPTION: CASH RECONCILIATION RESULT - ONE PER BANK OR      *
      *             BOOK ITEM.  WRITTEN BY RCB300.                     *
      *             DSN MSEC.PROD.RC.CASHREC(+1)                       *
      * RECFM/LRECL: FB / 200                                          *
      *================================================================*
       01  RCM-MATCH-REC.
           05  RCM-BUS-DATE            PIC 9(08).
           05  RCM-SIDE                PIC X(01).
               88  RCM-BANK-SIDE                 VALUE 'B'.
               88  RCM-BOOK-SIDE                 VALUE 'K'.
           05  RCM-MATCH-STATUS        PIC X(02).
               88  RCM-MATCHED                   VALUE 'MA'.
               88  RCM-MATCHED-TOLERANCE         VALUE 'MT'.
               88  RCM-MATCHED-NET               VALUE 'MN'.
               88  RCM-UNMATCHED                 VALUE 'UN'.
           05  RCM-RULE-ID             PIC X(03).
           05  RCM-CCY                 PIC X(03).
           05  RCM-VALUE-DATE          PIC 9(08).
           05  RCM-REF                 PIC X(16).
           05  RCM-OTHER-REF           PIC X(16).
           05  RCM-AMOUNT              PIC S9(13)V99    COMP-3.
           05  RCM-OTHER-AMOUNT        PIC S9(13)V99    COMP-3.
           05  RCM-DIFF-AMOUNT         PIC S9(13)V99    COMP-3.
           05  RCM-TYPE-CODE           PIC X(03).
           05  RCM-TEXT                PIC X(40).
           05  FILLER                  PIC X(76).
