      *================================================================*
      * COPYBOOK   : RCPREC                                            *
      * DESCRIPTION: POSITION RECONCILIATION RESULT - ONE RECORD PER   *
      *             DEPOSITORY + CUSIP COMPARED BY RCB200 (MATCHED     *
      *             ITEMS INCLUDED, RESULT 'OK').  READ BY RCB400 TO   *
      *             MAINTAIN THE BREAK MASTER AND BY RCR210.           *
      *             DSN MSEC.PROD.RC.POSREC(+1)                        *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * RESULT: OK MATCHED  MS BOOK ONLY (MISSING AT STREET)           *
      *         MB STREET ONLY (MISSING IN BOOKS)  QD QTY DIFFERENCE   *
      * DIFF = STREET - BOOK.  BOOK QTY = -(POS-SD-QTY) OF THE STREET  *
      * (LOCATION) ACCOUNT, SUMMED OVER ITS LOCATION ROWS.             *
      *----------------------------------------------------------------*
      * 1997-02-10 DWB  ORIGINAL                              CHG02644 *
      * 2003-10-06 KAP  ISIN / ID FLAG FOR EUROCLEAR          CHG11702 *
      * 2009-12-14 SPA  MARKET VALUE USD                      CHG19002 *
      *================================================================*
       01  RPR-POSREC-REC.
           05  RPR-BUS-DATE            PIC 9(08).
           05  RPR-STMT-DATE           PIC 9(08).
           05  RPR-DEPOSITORY          PIC X(04).
           05  RPR-CUSIP               PIC X(09).
           05  RPR-ISIN                PIC X(12).
           05  RPR-RESULT              PIC X(02).
               88  RPR-MATCHED                   VALUE 'OK'.
               88  RPR-MISSING-AT-STREET         VALUE 'MS'.
               88  RPR-MISSING-IN-BOOKS          VALUE 'MB'.
               88  RPR-QTY-DIFFERENCE            VALUE 'QD'.
               88  RPR-IS-BREAK                  VALUE 'MS' 'MB' 'QD'.
           05  RPR-STREET-QTY          PIC S9(13)V9(04) COMP-3.
           05  RPR-BOOK-QTY            PIC S9(13)V9(04) COMP-3.
           05  RPR-DIFF-QTY            PIC S9(13)V9(04) COMP-3.
           05  RPR-MKT-VALUE-USD       PIC S9(15)V99    COMP-3.
           05  RPR-BOOK-ACCT           PIC X(10).
           05  RPR-BOOK-ROWS           PIC 9(03).
           05  RPR-SEC-TYPE            PIC X(02).
           05  RPR-CCY                 PIC X(03).
           05  RPR-ID-FLAG             PIC X(01).
           05  RPR-STREET-FREE         PIC S9(13)V9(04) COMP-3.
           05  RPR-STREET-PLEDGED      PIC S9(13)V9(04) COMP-3.
           05  RPR-VALUE-FLAG          PIC X(01).
               88  RPR-NO-VALUATION              VALUE 'N'.
           05  FILLER                  PIC X(33).
