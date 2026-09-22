      *================================================================*
      * COPYBOOK   : RCCASHT                                           *
      * DESCRIPTION: NORMALIZED BANK TRANSACTION (BAI2 16 RECORDS).    *
      *             WRITTEN BY RCB120.  DSN MSEC.PROD.RC.BANKTXN(+1)   *
      * RECFM/LRECL: FB / 150                                          *
      *================================================================*
       01  RCT-BANK-TXN-REC.
           05  RCT-BANK-ID             PIC X(09).
           05  RCT-ACCOUNT             PIC X(12).
           05  RCT-CCY                 PIC X(03).
           05  RCT-AS-OF-DATE          PIC 9(08).
           05  RCT-VALUE-DATE          PIC 9(08).
           05  RCT-TYPE-CODE           PIC X(03).
           05  RCT-DR-CR               PIC X(01).
               88  RCT-DEBIT                     VALUE 'D'.
               88  RCT-CREDIT                    VALUE 'C'.
           05  RCT-AMOUNT              PIC S9(13)V99    COMP-3.
           05  RCT-BANK-REF            PIC X(16).
           05  RCT-CUST-REF            PIC X(16).
           05  RCT-TEXT                PIC X(50).
           05  RCT-SEQ-NO              PIC 9(07).
           05  FILLER                  PIC X(09).
