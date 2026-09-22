      ******************************************************************
      * DCLGEN TABLE(MSEC.GL_ACCOUNT_MAP)                              *
      *        LIBRARY(MSEC.PROD.COPYLIB(DCLGLMAP))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(GA-)                                              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MSEC.GL_ACCOUNT_MAP TABLE
           ( TXN_CODE                       CHAR(4) NOT NULL,
             ACCT_TYPE                      CHAR(2) NOT NULL,
             SEC_TYPE                       CHAR(2) NOT NULL,
             DR_GL                          CHAR(10) NOT NULL,
             CR_GL                          CHAR(10) NOT NULL,
             COST_CENTER                    CHAR(6) NOT NULL,
             GL_DESC                        CHAR(30) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MSEC.GL_ACCOUNT_MAP                *
      ******************************************************************
       01  DCLGL-ACCOUNT-MAP.
           10 GA-TXN-CODE          PIC X(4).
           10 GA-ACCT-TYPE         PIC X(2).
           10 GA-SEC-TYPE          PIC X(2).
           10 GA-DR-GL             PIC X(10).
           10 GA-CR-GL             PIC X(10).
           10 GA-COST-CENTER       PIC X(6).
           10 GA-GL-DESC           PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
