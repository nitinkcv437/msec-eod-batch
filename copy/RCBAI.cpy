      *================================================================*
      * COPYBOOK   : RCBAI                                             *
      * DESCRIPTION: BANK STATEMENT - BAI2 TEXT LINES AS RECEIVED      *
      *             FROM THE SETTLEMENT BANK.                          *
      *             RECEIVED AS MSEC.PROD.RC.BAISTMT.RAW(+1)           *
      * RECFM/LRECL: FB / 80                                           *
      *----------------------------------------------------------------*
      * RECORD CODES: 01 FILE HDR  02 GROUP HDR  03 ACCOUNT ID/SUMMARY *
      *   16 TRANSACTION DETAIL  88 CONTINUATION  49 ACCOUNT TRAILER   *
      *   98 GROUP TRAILER  99 FILE TRAILER.  FIELDS ARE COMMA         *
      *   DELIMITED, RECORD ENDS WITH '/'.  AMOUNTS HAVE NO DECIMAL    *
      *   POINT (IMPLIED 2).  A RECORD MAY CONTINUE ON 88 LINES.       *
      *================================================================*
       01  BAI-LINE-REC.
           05  BAI-REC-CODE            PIC X(02).
               88  BAI-FILE-HEADER               VALUE '01'.
               88  BAI-GROUP-HEADER              VALUE '02'.
               88  BAI-ACCOUNT-ID                VALUE '03'.
               88  BAI-TXN-DETAIL                VALUE '16'.
               88  BAI-CONTINUATION              VALUE '88'.
               88  BAI-ACCOUNT-TRAILER           VALUE '49'.
               88  BAI-GROUP-TRAILER             VALUE '98'.
               88  BAI-FILE-TRAILER              VALUE '99'.
           05  BAI-TEXT                PIC X(78).
