      *================================================================*
      * COPYBOOK   : MGRULE                                            *
      * DESCRIPTION: MARGIN RULE TABLE (VSAM KSDS), HOUSE AND REG T    *
      *             DSN MSEC.PROD.MG.RULES.KSDS      DDNAME MGRULES    *
      * KEY        : MGR-KEY  OFFSET 0 LENGTH 8                        *
      * RECFM/LRECL: F / 80                                            *
      *----------------------------------------------------------------*
      * RULE TYPES: RT REG T INITIAL          HL HOUSE MAINT LONG      *
      *             HS HOUSE MAINT SHORT      NM NON-MARGINABLE        *
      *             CN CONCENTRATION ADD-ON   MN MINIMUM EQUITY        *
      * QUALIFIER : PRICE BAND ('LT05' 'GE05' 'LT02'), MATURITY BUCKET *
      *             ('M01 ' 'M05 ' 'M10 ' 'M99 ') OR '****' = ANY      *
      *----------------------------------------------------------------*
      * 1994-06-13 DWB  ORIGINAL (FROM MARGIN SYSTEM TAPE)             *
      * 2001-04-09 KAP  DECIMALIZATION - PER SHARE MINIMUMS   CHG08201 *
      * 2011-02-28 SPA  CONCENTRATION ADD-ON                  CHG21340 *
      *================================================================*
       01  MGR-RULE-REC.
           05  MGR-KEY.
               10  MGR-RULE-TYPE       PIC X(02).
                   88  MGR-REG-T                 VALUE 'RT'.
                   88  MGR-HOUSE-LONG            VALUE 'HL'.
                   88  MGR-HOUSE-SHORT           VALUE 'HS'.
                   88  MGR-NON-MARGINABLE        VALUE 'NM'.
                   88  MGR-CONCENTRATION         VALUE 'CN'.
                   88  MGR-MIN-EQUITY            VALUE 'MN'.
               10  MGR-SEC-TYPE        PIC X(02).
               10  MGR-QUALIFIER       PIC X(04).
           05  MGR-PCT                 PIC S9(03)V9(04) COMP-3.
           05  MGR-PER-SHARE-MIN       PIC S9(05)V99    COMP-3.
           05  MGR-AMOUNT              PIC S9(11)V99    COMP-3.
           05  MGR-THRESHOLD-PCT       PIC S9(03)V9(04) COMP-3.
           05  MGR-EFF-DATE            PIC 9(08).
           05  MGR-DESC                PIC X(30).
           05  FILLER                  PIC X(15).
