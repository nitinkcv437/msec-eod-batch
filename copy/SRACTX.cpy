      *================================================================*
      * COPYBOOK   : SRACTX                                            *
      * DESCRIPTION: STOCK RECORD ACTIVITY - TRADE CHARGES EXTENSION.  *
      *              WORKING-STORAGE OVERLAY OF THE SRACTV TRAILING    *
      *              FILLER (OFFSET 154).  TCB500 MAY CARRY THE TRADE  *
      *              COMMISSION AND REGULATORY FEES HERE.  WHEN THE    *
      *              FIELDS ARE NOT NUMERIC OR ZERO THE CONSUMER MUST  *
      *              DERIVE CHARGES FROM NET CASH AND PRINCIPAL.       *
      * RECFM/LRECL: 200 (SAME AS SRACTV)                              *
      *----------------------------------------------------------------*
      * 2001-07-16 KAP  ORIGINAL - DECIMALIZATION             CHG08811 *
      *================================================================*
       01  ACX-ACTIVITY-EXT.
           05  FILLER                  PIC X(154).
           05  ACX-COMMISSION          PIC S9(11)V99    COMP-3.
           05  ACX-FEES                PIC S9(09)V99    COMP-3.
           05  ACX-ACCRUED-INT         PIC S9(11)V99    COMP-3.
           05  FILLER                  PIC X(26).
