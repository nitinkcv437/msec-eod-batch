      ******************************************************************
      * DCLGEN TABLE(MSEC.SECURITY_PRICE)                              *
      *        LIBRARY(MSEC.PROD.COPYLIB(DCLSECPR))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(PR-)                                              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MSEC.SECURITY_PRICE TABLE
           ( CUSIP                          CHAR(9) NOT NULL,
             PRICE_DATE                     DATE NOT NULL,
             PRICE                          DECIMAL(17, 8) NOT NULL,
             SOURCE                         CHAR(4) NOT NULL,
             CCY                            CHAR(3) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MSEC.SECURITY_PRICE                *
      ******************************************************************
       01  DCLSECURITY-PRICE.
           10 PR-CUSIP             PIC X(9).
           10 PR-PRICE-DATE        PIC X(10).
           10 PR-PRICE             PIC S9(9)V9(8) USAGE COMP-3.
           10 PR-SOURCE            PIC X(4).
           10 PR-CCY               PIC X(3).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
