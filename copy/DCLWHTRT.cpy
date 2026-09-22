      ******************************************************************
      * DCLGEN TABLE(MSEC.WHT_RATE)                                    *
      *        LIBRARY(MSEC.PROD.COPYLIB(DCLWHTRT))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(WR-)                                              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MSEC.WHT_RATE TABLE
           ( HOLDER_CTRY                    CHAR(2) NOT NULL,
             ISSUER_CTRY                    CHAR(2) NOT NULL,
             INCOME_TYPE                    CHAR(2) NOT NULL,
             TAX_STATUS                     CHAR(1) NOT NULL,
             EFF_DATE                       DATE NOT NULL,
             END_DATE                       DATE,
             RATE                           DECIMAL(5, 4) NOT NULL,
             TREATY_FLAG                    CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MSEC.WHT_RATE                      *
      ******************************************************************
       01  DCLWHT-RATE.
           10 WR-HOLDER-CTRY       PIC X(2).
           10 WR-ISSUER-CTRY       PIC X(2).
           10 WR-INCOME-TYPE       PIC X(2).
           10 WR-TAX-STATUS        PIC X(1).
           10 WR-EFF-DATE          PIC X(10).
           10 WR-END-DATE          PIC X(10).
           10 WR-RATE              PIC S9(1)V9(4) USAGE COMP-3.
           10 WR-TREATY-FLAG       PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
