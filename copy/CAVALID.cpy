      *================================================================*
      * COPYBOOK   : CAVALID                                           *
      * DESCRIPTION: ANNOUNCEMENT VALIDATION RESULT.  ONE RECORD PER   *
      *              FEED EVENT RECORD PLUS FILE-LEVEL RESULTS         *
      *              (HEADER / TRAILER).  WRITTEN BY CAB100, SORTED BY *
      *              MSCAD010 STEP020 (CAS010A) AND PRINTED BY CAR110. *
      *              DSN MSEC.PROD.CA.ANNVAL(+1)                       *
      *                  MSEC.PROD.CA.ANNVAL.SORTED(+1)                *
      * RECFM/LRECL: FB / 200                                          *
      *----------------------------------------------------------------*
      * VAL-RESULT  A ACCEPTED   W ACCEPTED WITH WARNING               *
      *             R REJECTED   F FILE LEVEL (HEADER/TRAILER)         *
      * MESSAGE CODES ARE EXPANDED TO TEXT BY CAR110 (TABLE CAR110-T)  *
      *   Vnnn REJECT   Wnnn WARNING   Fnnn FILE LEVEL                 *
      *----------------------------------------------------------------*
      * 1993-08-16 DWB  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2008-02-25 KAP  CASH MERGER - RATIO FIELDS PACKED     CHG17444 *
      *================================================================*
       01  VAL-VALIDATION-REC.
           05  VAL-SORT-KEY.
               10  VAL-RESULT          PIC X(01).
                   88  VAL-ACCEPTED              VALUE 'A'.
                   88  VAL-WARNING               VALUE 'W'.
                   88  VAL-REJECTED              VALUE 'R'.
                   88  VAL-FILE-LEVEL            VALUE 'F'.
               10  VAL-VENDOR-REF      PIC X(12).
           05  VAL-SEQ-NO              PIC 9(07).
           05  VAL-ACTION              PIC X(01).
           05  VAL-EVENT-ID            PIC X(12).
           05  VAL-CUSIP               PIC X(09).
           05  VAL-EVENT-TYPE          PIC X(03).
           05  VAL-EX-DATE             PIC 9(08).
           05  VAL-RECORD-DATE         PIC 9(08).
           05  VAL-PAY-DATE            PIC 9(08).
           05  VAL-RATE                PIC S9(07)V9(08) COMP-3.
           05  VAL-RATIO-NEW           PIC S9(05)V9(06) COMP-3.
           05  VAL-RATIO-OLD           PIC S9(05)V9(06) COMP-3.
           05  VAL-OLD-STATUS          PIC X(02).
           05  VAL-NEW-STATUS          PIC X(02).
           05  VAL-SEC-DESC            PIC X(30).
           05  VAL-MSG-COUNT           PIC 9(01).
           05  VAL-MSG-CODE            PIC X(04)  OCCURS 3 TIMES.
           05  VAL-BUS-DATE            PIC 9(08).
           05  FILLER                  PIC X(56).
