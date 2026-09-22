      *================================================================*
      * COPYBOOK   : CMAILNK                                           *
      * DESCRIPTION: LINKAGE FOR ACCRUED INTEREST MODULE CMU030.       *
      *             CALL 'CMU030' USING AI-ACCRUAL-PARMS.              *
      * AI-DAYCOUNT '30' 30/360, 'A3' ACT/360, 'AA' ACT/ACT (ICMA).    *
      * ACCRUED = FACE * COUPON-RATE/100 * DAYS / BASIS                *
      *   (ACT/ACT: FACE * RATE/100 / FREQ * DAYS / DAYS-IN-PERIOD)    *
      * LAST/NEXT COUPON DATES ARE DERIVED FROM MATURITY AND FREQ      *
      * WHEN AI-LAST-CPN-DATE IS ZERO.                                 *
      *================================================================*
       01  AI-ACCRUAL-PARMS.
           05  AI-DAYCOUNT             PIC X(02).
           05  AI-COUPON-RATE          PIC S9(03)V9(06) COMP-3.
           05  AI-COUPON-FREQ          PIC 9(01).
           05  AI-MATURITY-DATE        PIC 9(08).
           05  AI-ISSUE-DATE           PIC 9(08).
           05  AI-FIRST-CPN-DATE       PIC 9(08).
           05  AI-SETTLE-DATE          PIC 9(08).
           05  AI-FACE-AMOUNT          PIC S9(13)V99    COMP-3.
           05  AI-LAST-CPN-DATE        PIC 9(08).
           05  AI-NEXT-CPN-DATE        PIC 9(08).
           05  AI-ACCRUAL-DAYS         PIC S9(05)       COMP-3.
           05  AI-PERIOD-DAYS          PIC S9(05)       COMP-3.
           05  AI-ACCRUED-AMOUNT       PIC S9(13)V99    COMP-3.
           05  AI-RETURN-CODE          PIC 9(02).
               88  AI-OK                         VALUE 00.
               88  AI-BAD-DAYCOUNT               VALUE 04.
               88  AI-BAD-DATES                  VALUE 08.
               88  AI-AFTER-MATURITY             VALUE 12.
           05  AI-MESSAGE              PIC X(40).
