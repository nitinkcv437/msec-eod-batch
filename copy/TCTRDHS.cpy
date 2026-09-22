      *================================================================*
      * COPYBOOK   : TCTRDHS                                           *
      * DESCRIPTION: TRADE HISTORY (VSAM KSDS).  ONE RECORD PER TRADE  *
      *              ID, HOLDS THE LATEST VERSION.  USED FOR CANCEL /  *
      *              CORRECT MATCHING AND DUPLICATE DETECTION.         *
      *              DSN MSEC.PROD.TC.TRDHIST.KSDS   DDNAME TRDHIST    *
      * KEY        : TH-TRADE-ID  OFFSET 0 LENGTH 16                   *
      * RECFM/LRECL: F / 200                                           *
      *----------------------------------------------------------------*
      * TH-DUP-HASH IS COMPUTED BY HLASM ROUTINE CMASM03 OVER THE      *
      * EBCDIC BYTES OF ACCT/CUSIP/SIDE/QTY/PRICE/TRADE-DATE.          *
      *----------------------------------------------------------------*
      * 1990-02-19 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2006-06-12 KAP  DUP HASH VIA CMASM03                  CHG15008 *
      *================================================================*
       01  TH-HISTORY-REC.
           05  TH-TRADE-ID             PIC X(16).
           05  TH-VERSION              PIC 9(03).
           05  TH-STATUS               PIC X(02).
               88  TH-ACTIVE                     VALUE 'AC'.
               88  TH-CANCELLED                  VALUE 'CX'.
               88  TH-SUPERSEDED                 VALUE 'CR'.
           05  TH-SOURCE               PIC X(03).
           05  TH-DUP-HASH             PIC S9(08)       COMP.
           05  TH-ACCT-NO              PIC X(10).
           05  TH-CUSIP                PIC X(09).
           05  TH-SIDE                 PIC X(02).
           05  TH-QTY                  PIC S9(11)V9(04) COMP-3.
           05  TH-PRICE                PIC S9(09)V9(08) COMP-3.
           05  TH-NET-AMOUNT           PIC S9(15)V99    COMP-3.
           05  TH-TRADE-DATE           PIC 9(08).
           05  TH-SETTLE-DATE          PIC 9(08).
           05  TH-FIRST-SEEN-DATE      PIC 9(08).
           05  TH-LAST-UPD-DATE        PIC 9(08).
           05  TH-LAST-UPD-JOB         PIC X(08).
           05  TH-SUPERSEDED-BY        PIC X(16).
           05  FILLER                  PIC X(69).
