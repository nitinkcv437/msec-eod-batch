      *================================================================*
      * COPYBOOK   : CAANNFD                                           *
      * DESCRIPTION: CORPORATE ACTION ANNOUNCEMENT FEED FROM THE DATA  *
      *              VENDOR.  MSEC.PROD.CA.ANNFEED.RAW(+1)             *
      * RECFM/LRECL: FB / 200                                          *
      *----------------------------------------------------------------*
      * EVENT TYPES: CDV CASH DIVIDEND   SDV STOCK DIVIDEND            *
      *              SPL FORWARD SPLIT   RSP REVERSE SPLIT             *
      *              MRG CASH MERGER                                   *
      *----------------------------------------------------------------*
      * 1993-08-09 DWB  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2008-02-25 KAP  CASH MERGER                           CHG17444 *
      *================================================================*
       01  CAN-FEED-REC.
           05  CAN-REC-TYPE            PIC X(03).
               88  CAN-HEADER                    VALUE 'HDR'.
               88  CAN-EVENT                     VALUE 'EVT'.
               88  CAN-TRAILER                   VALUE 'TRL'.
           05  CAN-REC-BODY            PIC X(197).
           05  CAN-HDR-BODY      REDEFINES CAN-REC-BODY.
               10  CAN-HDR-VENDOR      PIC X(08).
               10  CAN-HDR-FILE-DATE   PIC 9(08).
               10  FILLER              PIC X(181).
           05  CAN-EVT-BODY      REDEFINES CAN-REC-BODY.
               10  CAN-VENDOR-REF      PIC X(12).
               10  CAN-ACTION          PIC X(01).
                   88  CAN-ACT-NEW               VALUE 'N'.
                   88  CAN-ACT-UPDATE            VALUE 'U'.
                   88  CAN-ACT-CANCEL            VALUE 'C'.
               10  CAN-CUSIP           PIC X(09).
               10  CAN-EVENT-TYPE      PIC X(03).
               10  CAN-ANN-DATE        PIC 9(08).
               10  CAN-EX-DATE         PIC 9(08).
               10  CAN-RECORD-DATE     PIC 9(08).
               10  CAN-PAY-DATE        PIC 9(08).
               10  CAN-RATE            PIC 9(07)V9(08).
               10  CAN-RATIO-NEW       PIC 9(05)V9(06).
               10  CAN-RATIO-OLD       PIC 9(05)V9(06).
               10  CAN-CIL-PRICE       PIC 9(09)V9(06).
               10  CAN-NEW-CUSIP       PIC X(09).
               10  CAN-CCY             PIC X(03).
               10  CAN-FRAC-METHOD     PIC X(01).
               10  CAN-TAXABLE         PIC X(01).
               10  CAN-DESC            PIC X(40).
               10  FILLER              PIC X(34).
           05  CAN-TRL-BODY      REDEFINES CAN-REC-BODY.
               10  CAN-TRL-COUNT       PIC 9(07).
               10  FILLER              PIC X(190).
