      *================================================================*
      * COPYBOOK   : TCFIFD                                            *
      * DESCRIPTION: INBOUND FIXED INCOME TICKET FEED FROM THE BOND    *
      *              DESK SYSTEM.  MSEC.PROD.TC.FIFEED.RAW(+1)         *
      * RECFM/LRECL: FB / 300                                          *
      *----------------------------------------------------------------*
      * WARNING: THE BOND DESK SYSTEM WAS NEVER Y2K REMEDIATED.  DATES *
      *          ARRIVE AS YYMMDD AND ARE WINDOWED BY CMU010 ('W2Y4'). *
      *          PRICE IS PERCENT OF PAR.  ACCRUED OVERRIDE IS SIGNED  *
      *          ZONED DECIMAL (TRAILING OVERPUNCH).                   *
      *----------------------------------------------------------------*
      * 1991-05-06 RJK  ORIGINAL                                       *
      * 1999-01-15 TLM  Y2K - WINDOWING ONLY, SOURCE NOT FIXED CHG0460 *
      * 2012-09-10 SPA  AMEND STATUS 'A'                      CHG23380 *
      *================================================================*
       01  FI-FEED-REC.
           05  FI-REC-TYPE             PIC X(03).
               88  FI-HEADER                     VALUE 'HDR'.
               88  FI-DETAIL                     VALUE 'DTL'.
               88  FI-TRAILER                    VALUE 'TRL'.
           05  FI-REC-BODY             PIC X(297).
      *    ---- HEADER ------------------------------------------------
           05  FI-HDR-BODY       REDEFINES FI-REC-BODY.
               10  FI-HDR-DESK-SYS     PIC X(08).
               10  FI-HDR-RUN-DATE     PIC 9(06).
               10  FI-HDR-BATCH-NO     PIC 9(04).
               10  FILLER              PIC X(279).
      *    ---- DETAIL ------------------------------------------------
           05  FI-DTL-BODY       REDEFINES FI-REC-BODY.
               10  FI-TICKET           PIC X(14).
               10  FI-STATUS           PIC X(01).
                   88  FI-NEW-TICKET             VALUE 'N'.
                   88  FI-CANCEL-TICKET          VALUE 'C'.
                   88  FI-AMEND-TICKET           VALUE 'A'.
               10  FI-ORIG-TICKET      PIC X(14).
               10  FI-ACCT             PIC X(10).
               10  FI-CUSIP            PIC X(09).
               10  FI-BUY-SELL         PIC X(01).
                   88  FI-PURCHASE               VALUE 'P'.
                   88  FI-SALE                   VALUE 'S'.
               10  FI-FACE-AMT         PIC 9(11)V99.
               10  FI-PRICE-PCT        PIC 9(03)V9(06).
               10  FI-YIELD            PIC 9(03)V9(06).
               10  FI-TRADE-DATE       PIC 9(06).
               10  FI-SETTLE-DATE      PIC 9(06).
               10  FI-ACCRUED-OVR      PIC S9(09)V99.
               10  FI-ACCRUED-OVR-FLAG PIC X(01).
               10  FI-SALES-CREDIT     PIC 9(07)V99.
               10  FI-CCY              PIC X(03).
               10  FI-CONTRA           PIC X(04).
               10  FI-DESK             PIC X(04).
               10  FI-TRADER           PIC X(06).
               10  FI-CAPACITY         PIC X(01).
               10  FI-MARKUP-BPS       PIC 9(03)V99.
               10  FILLER              PIC X(161).
      *    ---- TRAILER -----------------------------------------------
           05  FI-TRL-BODY       REDEFINES FI-REC-BODY.
               10  FI-TRL-COUNT        PIC 9(07).
               10  FI-TRL-FACE-TOTAL   PIC 9(15)V99.
               10  FILLER              PIC X(273).
