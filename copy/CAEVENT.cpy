      *================================================================*
      * COPYBOOK   : CAEVENT                                           *
      * DESCRIPTION: CORPORATE ACTION EVENT MASTER (VSAM KSDS)         *
      *              DSN MSEC.PROD.CA.EVENT.KSDS      DDNAME CAEVENT   *
      * KEY        : CAE-EVENT-ID  OFFSET 0 LENGTH 12                  *
      * RECFM/LRECL: F / 300                                           *
      *----------------------------------------------------------------*
      * STATUS LIFE CYCLE:                                             *
      *   AN ANNOUNCED -> EL ELIGIBILITY TAKEN (RECORD DATE)           *
      *   -> EN ENTITLED -> PD PAID (PAY DATE)        CX CANCELLED     *
      *----------------------------------------------------------------*
      * 1993-08-09 DWB  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2008-02-25 KAP  CASH MERGER TERMS                     CHG17444 *
      *================================================================*
       01  CAE-EVENT-REC.
           05  CAE-EVENT-ID            PIC X(12).
           05  CAE-CUSIP               PIC X(09).
           05  CAE-EVENT-TYPE          PIC X(03).
               88  CAE-CASH-DIV                  VALUE 'CDV'.
               88  CAE-STOCK-DIV                 VALUE 'SDV'.
               88  CAE-FWD-SPLIT                 VALUE 'SPL'.
               88  CAE-REV-SPLIT                 VALUE 'RSP'.
               88  CAE-CASH-MERGER               VALUE 'MRG'.
               88  CAE-CASH-EVENT                VALUE 'CDV' 'MRG'.
               88  CAE-STOCK-EVENT               VALUE 'SDV' 'SPL'
                                                       'RSP'.
           05  CAE-STATUS              PIC X(02).
               88  CAE-ANNOUNCED                 VALUE 'AN'.
               88  CAE-ELIGIBLE-TAKEN            VALUE 'EL'.
               88  CAE-ENTITLED                  VALUE 'EN'.
               88  CAE-PAID                      VALUE 'PD'.
               88  CAE-CANCELLED                 VALUE 'CX'.
           05  CAE-ANN-DATE            PIC 9(08).
           05  CAE-EX-DATE             PIC 9(08).
           05  CAE-RECORD-DATE         PIC 9(08).
           05  CAE-PAY-DATE            PIC 9(08).
           05  CAE-CCY                 PIC X(03).
           05  CAE-TAXABLE-FLAG        PIC X(01).
           05  CAE-FRAC-METHOD         PIC X(01).
               88  CAE-FRAC-ROUND-UP             VALUE 'U'.
               88  CAE-FRAC-ROUND-DOWN           VALUE 'D'.
               88  CAE-FRAC-CASH-IN-LIEU         VALUE 'C'.
           05  CAE-TERMS               PIC X(40).
           05  CAE-CASH-TERMS    REDEFINES CAE-TERMS.
               10  CAE-RATE            PIC S9(07)V9(08) COMP-3.
               10  CAE-GROSS-UP-FLAG   PIC X(01).
               10  FILLER              PIC X(31).
           05  CAE-STOCK-TERMS   REDEFINES CAE-TERMS.
               10  CAE-RATIO-NEW       PIC S9(05)V9(06) COMP-3.
               10  CAE-RATIO-OLD       PIC S9(05)V9(06) COMP-3.
               10  CAE-CIL-PRICE       PIC S9(09)V9(06) COMP-3.
               10  CAE-NEW-CUSIP       PIC X(09).
               10  FILLER              PIC X(11).
           05  CAE-MERGER-TERMS  REDEFINES CAE-TERMS.
               10  CAE-MRG-CASH-RATE   PIC S9(07)V9(08) COMP-3.
               10  CAE-MRG-ACQUIRER    PIC X(09).
               10  CAE-MRG-EFF-DATE    PIC 9(08).
               10  FILLER              PIC X(15).
           05  CAE-VENDOR-REF          PIC X(12).
           05  CAE-DESC                PIC X(40).
           05  CAE-ELIG-COUNT          PIC S9(07)       COMP-3.
           05  CAE-ELIG-QTY            PIC S9(13)V9(04) COMP-3.
           05  CAE-ENTL-CASH-TOTAL     PIC S9(15)V99    COMP-3.
           05  CAE-ENTL-SHARE-TOTAL    PIC S9(13)V9(04) COMP-3.
           05  CAE-LAST-UPD-DATE       PIC 9(08).
           05  CAE-LAST-UPD-JOB        PIC X(08).
           05  FILLER                  PIC X(98).
