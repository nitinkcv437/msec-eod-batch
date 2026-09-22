      *================================================================*
      * COPYBOOK   : TCOMSFD                                           *
      * DESCRIPTION: INBOUND EQUITY EXECUTION FEED FROM THE ORDER      *
      *              MANAGEMENT SYSTEM (OMS).  ONE HEADER, N DETAILS,  *
      *              ONE TRAILER.  RECEIVED VIA CONNECT:DIRECT AS      *
      *              MSEC.PROD.TC.OMSFEED.RAW(+1)                      *
      * RECFM/LRECL: FB / 250                                          *
      *----------------------------------------------------------------*
      * NOTE: OMS SENDS SYMBOL, NOT CUSIP.  QUANTITY AND PRICE ARE     *
      *       UNSIGNED ZONED DECIMAL.  CANCELS ARRIVE AS A NEW DETAIL  *
      *       WITH OMS-CANCEL-FLAG = 'Y' AND OMS-ORIG-EXEC-ID SET.     *
      *----------------------------------------------------------------*
      * 1995-07-17 DWB  ORIGINAL - REPLACED TAPE FEED         CHG01877 *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2007-01-22 KAP  COMMISSION OVERRIDE FIELDS            CHG16202 *
      *================================================================*
       01  OMS-FEED-REC.
           05  OMS-REC-TYPE            PIC X(01).
               88  OMS-HEADER                    VALUE 'H'.
               88  OMS-DETAIL                    VALUE 'D'.
               88  OMS-TRAILER                   VALUE 'T'.
           05  OMS-REC-BODY            PIC X(249).
      *    ---- HEADER ------------------------------------------------
           05  OMS-HDR-BODY      REDEFINES OMS-REC-BODY.
               10  OMS-HDR-SOURCE-ID   PIC X(08).
               10  OMS-HDR-FILE-DATE   PIC 9(08).
               10  OMS-HDR-FILE-SEQ    PIC 9(05).
               10  OMS-HDR-CREATE-TS   PIC X(14).
               10  FILLER              PIC X(214).
      *    ---- DETAIL ------------------------------------------------
           05  OMS-DTL-BODY      REDEFINES OMS-REC-BODY.
               10  OMS-ORDER-ID        PIC X(12).
               10  OMS-EXEC-ID         PIC X(10).
               10  OMS-ACCOUNT         PIC X(10).
               10  OMS-SYMBOL          PIC X(08).
               10  OMS-SIDE            PIC X(02).
               10  OMS-QTY             PIC 9(09).
               10  OMS-PRICE           PIC 9(07)V9(06).
               10  OMS-TRADE-DATE      PIC 9(08).
               10  OMS-TRADE-TIME      PIC 9(06).
               10  OMS-EXEC-BROKER     PIC X(04).
               10  OMS-CAPACITY        PIC X(01).
               10  OMS-CCY             PIC X(03).
               10  OMS-MARKET          PIC X(04).
               10  OMS-CANCEL-FLAG     PIC X(01).
               10  OMS-ORIG-EXEC-ID    PIC X(10).
               10  OMS-COMM-OVERRIDE   PIC 9(07)V99.
               10  OMS-COMM-OVR-FLAG   PIC X(01).
               10  OMS-TRADER-ID       PIC X(06).
               10  OMS-DESK            PIC X(04).
               10  FILLER              PIC X(128).
      *    ---- TRAILER -----------------------------------------------
           05  OMS-TRL-BODY      REDEFINES OMS-REC-BODY.
               10  OMS-TRL-REC-COUNT   PIC 9(09).
               10  OMS-TRL-QTY-HASH    PIC 9(15).
               10  FILLER              PIC X(225).
