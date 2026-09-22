      *================================================================*
      * COPYBOOK   : TCTRADE                                           *
      * DESCRIPTION: NORMALIZED TRADE RECORD.  PRODUCED BY THE FEED    *
      *              VALIDATION PROGRAMS (TCB100/110/120) AND CARRIED  *
      *              THROUGH ENRICHMENT (TCB200), CANCEL/CORRECT       *
      *              (TCB300) AND EXTRACT (TCB500).                    *
      * RECFM/LRECL: FB / 400                                          *
      *----------------------------------------------------------------*
      * SORT KEYS USED IN JCL (1-BASED POSITIONS):                     *
      *   TRD-ID        1,16 CH     TRD-ACCT-NO  43,10 CH              *
      *   TRD-CUSIP    53,9  CH     TRD-BRANCH   (SEE OFFSET TABLE)    *
      *   FULL OFFSET TABLE IN DOCS/RECORD-LAYOUTS.MD                  *
      *----------------------------------------------------------------*
      * 1988-01-11 RJK  ORIGINAL                                       *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2002-03-08 KAP  ADDED TAF FEE                         CHG09930 *
      * 2009-12-14 SPA  ADDED FX / USD NET FOR NON-USD TRADES CHG19002 *
      * 2024-02-12 NVR  T+1                                   CHG41007 *
      *================================================================*
       01  TRD-TRADE-REC.
           05  TRD-ID                  PIC X(16).
           05  TRD-SOURCE              PIC X(03).
               88  TRD-SRC-OMS                   VALUE 'OMS'.
               88  TRD-SRC-FIXED-INC             VALUE 'FIX'.
               88  TRD-SRC-MANUAL                VALUE 'MAN'.
           05  TRD-VERSION             PIC 9(03).
           05  TRD-TXN-TYPE            PIC X(02).
               88  TRD-NEW                       VALUE 'NW'.
               88  TRD-CANCEL                    VALUE 'CX'.
               88  TRD-CORRECT                   VALUE 'CR'.
           05  TRD-ORIG-ID             PIC X(16).
           05  TRD-STATUS              PIC X(02).
               88  TRD-ST-RAW                    VALUE 'RW'.
               88  TRD-ST-VALIDATED              VALUE 'VL'.
               88  TRD-ST-ENRICHED               VALUE 'EN'.
               88  TRD-ST-FINAL                  VALUE 'FN'.
               88  TRD-ST-REJECTED               VALUE 'RJ'.
               88  TRD-ST-SUPPRESSED             VALUE 'SP'.
           05  TRD-ACCT-NO             PIC X(10).
           05  TRD-CUSIP               PIC X(09).
           05  TRD-SYMBOL              PIC X(08).
           05  TRD-SIDE                PIC X(02).
               88  TRD-BUY                       VALUE 'B '.
               88  TRD-SELL                      VALUE 'S '.
               88  TRD-SELL-SHORT                VALUE 'SS'.
               88  TRD-BUY-COVER                 VALUE 'BC'.
               88  TRD-BUY-SIDE                  VALUE 'B ' 'BC'.
               88  TRD-SELL-SIDE                 VALUE 'S ' 'SS'.
           05  TRD-QTY                 PIC S9(11)V9(04) COMP-3.
           05  TRD-PRICE               PIC S9(09)V9(08) COMP-3.
           05  TRD-TRADE-DATE          PIC 9(08).
           05  TRD-TRADE-TIME          PIC 9(06).
           05  TRD-SETTLE-DATE         PIC 9(08).
           05  TRD-CCY                 PIC X(03).
           05  TRD-SEC-TYPE            PIC X(02).
           05  TRD-PRICE-FACTOR        PIC S9(05)V9(04) COMP-3.
           05  TRD-PRINCIPAL           PIC S9(15)V99    COMP-3.
           05  TRD-COMMISSION          PIC S9(11)V99    COMP-3.
           05  TRD-COMM-OVR-FLAG       PIC X(01).
               88  TRD-COMM-OVERRIDDEN           VALUE 'Y'.
           05  TRD-SEC-FEE             PIC S9(09)V99    COMP-3.
           05  TRD-TAF-FEE             PIC S9(09)V99    COMP-3.
           05  TRD-OTHER-FEES          PIC S9(09)V99    COMP-3.
           05  TRD-ACCRUED-INT         PIC S9(13)V99    COMP-3.
           05  TRD-NET-AMOUNT          PIC S9(15)V99    COMP-3.
           05  TRD-FX-RATE             PIC S9(05)V9(08) COMP-3.
           05  TRD-USD-NET-AMOUNT      PIC S9(15)V99    COMP-3.
           05  TRD-CAPACITY            PIC X(01).
               88  TRD-AGENCY                    VALUE 'A'.
               88  TRD-PRINCIPAL-CAP             VALUE 'P'.
               88  TRD-RISKLESS-PRIN             VALUE 'R'.
           05  TRD-EXEC-BROKER         PIC X(04).
           05  TRD-CONTRA              PIC X(04).
           05  TRD-MARKET              PIC X(04).
           05  TRD-SETTLE-LOC          PIC X(04).
               88  TRD-LOC-DTC                   VALUE 'DTC '.
               88  TRD-LOC-FED                   VALUE 'FED '.
               88  TRD-LOC-EUROCLEAR             VALUE 'EUCL'.
           05  TRD-ACCT-TYPE           PIC X(02).
           05  TRD-BRANCH              PIC X(03).
           05  TRD-REP                 PIC X(04).
           05  TRD-DESK                PIC X(04).
           05  TRD-FACE-AMOUNT         PIC S9(13)V99    COMP-3.
           05  TRD-YIELD               PIC S9(03)V9(06) COMP-3.
           05  TRD-DUP-HASH            PIC S9(08)       COMP.
           05  TRD-REJECT-CODE         PIC X(04).
           05  TRD-REJECT-TEXT         PIC X(40).
           05  TRD-WARN-FLAGS.
               10  TRD-WARN-FLAG       PIC X(01)  OCCURS 8 TIMES.
           05  TRD-ENTRY-TS            PIC X(26).
           05  TRD-ENRICH-TS           PIC X(26).
           05  TRD-SOURCE-REF          PIC X(20).
           05  TRD-ENTERED-BY          PIC X(06).
           05  TRD-APPROVED-BY         PIC X(06).
           05  FILLER                  PIC X(29).
