      ******************************************************************
      * DCLGEN TABLE(MSEC.FX_RATE)                                     *
      *        LIBRARY(MSEC.PROD.COPYLIB(DCLFXRAT))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(FR-)                                              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MSEC.FX_RATE TABLE
           ( CCY                            CHAR(3) NOT NULL,
             RATE_DATE                      DATE NOT NULL,
             USD_RATE                       DECIMAL(13, 8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MSEC.FX_RATE                       *
      ******************************************************************
       01  DCLFX-RATE.
           10 FR-CCY               PIC X(3).
           10 FR-RATE-DATE         PIC X(10).
           10 FR-USD-RATE          PIC S9(5)V9(8) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
