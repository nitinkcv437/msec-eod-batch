      *================================================================*
      * COPYBOOK   : SWINSTR                                           *
      * DESCRIPTION: SETTLEMENT INSTRUCTION STATUS MASTER (VSAM KSDS). *
      *             ONE ROW PER INSTRUCTION (SENDER REF = TRADE ID).   *
      *             DSN MSEC.PROD.SW.INSTR.KSDS      DDNAME SWINSTR    *
      * KEY        : SWI-SENDER-REF  OFFSET 0 LENGTH 16                *
      * RECFM/LRECL: F / 250                                           *
      *================================================================*
       01  SWI-INSTR-REC.
           05  SWI-SENDER-REF          PIC X(16).
           05  SWI-MSG-TYPE            PIC X(03).
           05  SWI-FUNCTION            PIC X(04).
           05  SWI-STATUS              PIC X(02).
               88  SWI-SENT                      VALUE 'SN'.
               88  SWI-MATCHED                   VALUE 'MA'.
               88  SWI-UNMATCHED                 VALUE 'NM'.
               88  SWI-PENDING                   VALUE 'PE'.
               88  SWI-SETTLED                   VALUE 'ST'.
               88  SWI-PARTIAL                   VALUE 'PS'.
               88  SWI-FAILED                    VALUE 'FL'.
               88  SWI-CANCELLED                 VALUE 'CX'.
               88  SWI-REJECTED                  VALUE 'RJ'.
           05  SWI-ACCT-NO             PIC X(10).
           05  SWI-CUSIP               PIC X(09).
           05  SWI-ISIN                PIC X(12).
           05  SWI-TRADE-DATE          PIC 9(08).
           05  SWI-SETTLE-DATE         PIC 9(08).
           05  SWI-QTY                 PIC S9(11)V9(04) COMP-3.
           05  SWI-AMOUNT              PIC S9(15)V99    COMP-3.
           05  SWI-CCY                 PIC X(03).
           05  SWI-DEPOSITORY          PIC X(04).
           05  SWI-SENT-DATE           PIC 9(08).
           05  SWI-MSG-SEQ             PIC 9(08).
           05  SWI-LAST-STATUS-DATE    PIC 9(08).
           05  SWI-LAST-STATUS-MSG     PIC X(03).
           05  SWI-STATUS-CODE         PIC X(04).
           05  SWI-REASON-CODE         PIC X(04).
           05  SWI-SETTLED-QTY         PIC S9(11)V9(04) COMP-3.
           05  SWI-SETTLED-AMOUNT      PIC S9(15)V99    COMP-3.
           05  SWI-EFF-SETTLE-DATE     PIC 9(08).
           05  SWI-FAIL-AGE            PIC S9(03)       COMP-3.
           05  SWI-CLOSEOUT-DATE       PIC 9(08).
           05  SWI-BUYIN-FLAG          PIC X(01).
           05  SWI-CUST-REF            PIC X(16).
           05  SWI-LAST-UPD-JOB        PIC X(08).
           05  FILLER                  PIC X(59).
