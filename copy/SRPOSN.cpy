      *================================================================*
      * COPYBOOK   : SRPOSN                                            *
      * DESCRIPTION: POSITION MASTER (VSAM KSDS) - THE STOCK RECORD.   *
      *              DSN MSEC.PROD.SR.POSITION.KSDS   DDNAME POSMAST   *
      * KEY        : POS-KEY  OFFSET 0 LENGTH 23                       *
      *              (ACCOUNT 10 + CUSIP 9 + LOCATION 4)               *
      * RECFM/LRECL: F / 200                                           *
      *----------------------------------------------------------------*
      * THE STOCK RECORD MUST BALANCE: FOR EVERY CUSIP THE SUM OF      *
      * SETTLED QUANTITY ACROSS ALL OWNERSHIP ACCOUNTS (CLIENT + FIRM) *
      * EQUALS THE SUM ACROSS ALL LOCATION ACCOUNTS (STREET SIDE).     *
      * OWNERSHIP ROWS CARRY POSITIVE (LONG) QUANTITIES, LOCATION ROWS *
      * CARRY NEGATIVE QUANTITIES, SO THE NET PER CUSIP IS ZERO.       *
      *----------------------------------------------------------------*
      * 1987-09-01 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2001-07-16 KAP  AVERAGE COST                          CHG08811 *
      * 2009-12-14 SPA  USD MARKET VALUE                      CHG19002 *
      *================================================================*
       01  POS-POSITION-REC.
           05  POS-KEY.
               10  POS-ACCT-NO         PIC X(10).
               10  POS-CUSIP           PIC X(09).
               10  POS-LOCATION        PIC X(04).
           05  POS-SEC-TYPE            PIC X(02).
           05  POS-ACCT-TYPE           PIC X(02).
           05  POS-CCY                 PIC X(03).
           05  POS-TD-QTY              PIC S9(11)V9(04) COMP-3.
           05  POS-SD-QTY              PIC S9(11)V9(04) COMP-3.
           05  POS-PEND-IN-QTY         PIC S9(11)V9(04) COMP-3.
           05  POS-PEND-OUT-QTY        PIC S9(11)V9(04) COMP-3.
           05  POS-AVG-COST            PIC S9(09)V9(06) COMP-3.
           05  POS-COST-BASIS          PIC S9(15)V99    COMP-3.
           05  POS-MKT-PRICE           PIC S9(09)V9(08) COMP-3.
           05  POS-PRICE-DATE          PIC 9(08).
           05  POS-MKT-VALUE           PIC S9(15)V99    COMP-3.
           05  POS-FX-RATE             PIC S9(05)V9(08) COMP-3.
           05  POS-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  POS-UNRLZD-PL           PIC S9(15)V99    COMP-3.
           05  POS-REALIZED-PL-YTD     PIC S9(15)V99    COMP-3.
           05  POS-OPEN-DATE           PIC 9(08).
           05  POS-LAST-ACTV-DATE      PIC 9(08).
           05  POS-LAST-UPD-JOB        PIC X(08).
           05  POS-SHORT-FLAG          PIC X(01).
               88  POS-IS-SHORT                  VALUE 'Y'.
           05  POS-STATUS              PIC X(01).
               88  POS-OPEN                      VALUE 'O'.
               88  POS-FLAT                      VALUE 'F'.
               88  POS-FROZEN                    VALUE 'Z'.
           05  FILLER                  PIC X(35).
