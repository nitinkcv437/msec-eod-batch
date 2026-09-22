      *================================================================*
      * COPYBOOK   : TCFEELNK                                          *
      * DESCRIPTION: LINKAGE FOR THE REGULATORY FEE MODULES            *
      *             TCU22E (EQUITY-LIKE)  AND  TCU22F (FIXED INCOME).  *
      *             CALLER BUILDS THE MODULE NAME 'TCU22' + SUFFIX.    *
      *             CALL WS-FEE-PGM USING FE-FEE-PARMS.                *
      *----------------------------------------------------------------*
      * FE-SEC-RATE     RATE APPLIED (SEC $ PER MILLION, OR FI $ PER   *
      *                 1000 FACE) - RETURNED FOR THE AUDIT TRAIL      *
      * FE-RETURN-CODE  00 OK                                          *
      *                 04 SECURITY TYPE NOT HANDLED BY THIS MODULE    *
      *                    (ALL FEES RETURNED AS ZERO)                 *
      *                 08 INVALID INPUT                               *
      *----------------------------------------------------------------*
      * 1990-08-20 RJK  ORIGINAL (SEC FEE ONLY)                        *
      * 2002-03-08 KAP  TAF FEE                               CHG09930 *
      * 2011-11-14 SPA  SPLIT INTO TCU22E / TCU22F            CHG22418 *
      *================================================================*
       01  FE-FEE-PARMS.
           05  FE-SEC-TYPE             PIC X(02).
           05  FE-SIDE                 PIC X(02).
           05  FE-CAPACITY             PIC X(01).
           05  FE-TRADE-DATE           PIC 9(08).
           05  FE-QTY                  PIC S9(11)V9(04) COMP-3.
           05  FE-PRINCIPAL            PIC S9(15)V99    COMP-3.
           05  FE-FACE-AMOUNT          PIC S9(13)V99    COMP-3.
           05  FE-SEC-FEE              PIC S9(09)V99    COMP-3.
           05  FE-TAF-FEE              PIC S9(09)V99    COMP-3.
           05  FE-OTHER-FEES           PIC S9(09)V99    COMP-3.
           05  FE-SEC-RATE             PIC S9(05)V9(06) COMP-3.
           05  FE-RETURN-CODE          PIC 9(02).
               88  FE-OK                         VALUE 00.
               88  FE-NOT-APPLICABLE             VALUE 04.
               88  FE-INVALID-INPUT              VALUE 08.
           05  FE-MESSAGE              PIC X(40).
