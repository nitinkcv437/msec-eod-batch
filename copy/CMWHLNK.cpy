      *================================================================*
      * COPYBOOK   : CMWHLNK                                           *
      * DESCRIPTION: LINKAGE FOR DB2 I/O MODULE CMD040                 *
      *             (MSEC.WHT_RATE - WITHHOLDING TAX).                 *
      *             CALL 'CMD040' USING WH-WHTAX-PARMS.                *
      * LOOKUP ORDER (FIRST ROW WINS, EFFECTIVE ON WH-EFF-DATE):       *
      *   1 HOLDER-CTRY + ISSUER-CTRY + INCOME-TYPE + TAX-STATUS       *
      *   2 HOLDER-CTRY + '**'        + INCOME-TYPE + TAX-STATUS       *
      *   3 '**'        + ISSUER-CTRY + INCOME-TYPE + TAX-STATUS       *
      *   4 '**' + '**' + INCOME-TYPE + TAX-STATUS  (DEFAULT ROW)      *
      * WH-RETURN-CODE 00 FOUND  04 NOT FOUND  12 DB2 ERROR            *
      *================================================================*
       01  WH-WHTAX-PARMS.
           05  WH-HOLDER-COUNTRY       PIC X(02).
           05  WH-ISSUER-COUNTRY       PIC X(02).
           05  WH-INCOME-TYPE          PIC X(02).
               88  WH-DIVIDEND-INCOME            VALUE 'DV'.
               88  WH-INTEREST-INCOME            VALUE 'IN'.
           05  WH-TAX-STATUS           PIC X(01).
           05  WH-EFF-DATE             PIC 9(08).
           05  WH-RATE                 PIC S9(01)V9(04) COMP-3.
           05  WH-TREATY-FLAG          PIC X(01).
           05  WH-MATCH-LEVEL          PIC 9(01).
           05  WH-RETURN-CODE          PIC 9(02).
               88  WH-FOUND                      VALUE 00.
               88  WH-NOT-FOUND                  VALUE 04.
               88  WH-DB-ERROR                   VALUE 12.
           05  WH-SQLCODE              PIC S9(09)       COMP.
