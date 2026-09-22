      ******************************************************************
      * DCLGEN TABLE(MSEC.SECURITY_MASTER)                             *
      *        LIBRARY(MSEC.PROD.COPYLIB(DCLSECMS))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(SM-)                                              *
      *        QUOTE                                                   *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE MSEC.SECURITY_MASTER TABLE
           ( CUSIP                          CHAR(9) NOT NULL,
             ISIN                           CHAR(12) NOT NULL,
             SYMBOL                         CHAR(8) NOT NULL,
             SEC_DESC                       CHAR(40) NOT NULL,
             SEC_TYPE                       CHAR(2) NOT NULL,
             CCY                            CHAR(3) NOT NULL,
             COUNTRY                        CHAR(2) NOT NULL,
             EXCHANGE                       CHAR(4) NOT NULL,
             SEC_STATUS                     CHAR(1) NOT NULL,
             PRICE_FACTOR                   DECIMAL(9, 4) NOT NULL,
             SETTLE_DAYS                    SMALLINT NOT NULL,
             DEPOSITORY                     CHAR(4) NOT NULL,
             MIN_DENOM                      DECIMAL(11, 2) NOT NULL,
             ISSUER_ID                      CHAR(6) NOT NULL,
             COUPON_RATE                    DECIMAL(9, 6),
             MATURITY_DATE                  DATE,
             ISSUE_DATE                     DATE,
             FIRST_CPN_DATE                 DATE,
             DAYCOUNT                       CHAR(2),
             COUPON_FREQ                    SMALLINT,
             CALLABLE_FLAG                  CHAR(1),
             TAX_EXEMPT_FLAG                CHAR(1),
             SHARES_OUT                     DECIMAL(13, 0),
             DIV_FREQ                       SMALLINT,
             ADR_RATIO                      DECIMAL(7, 4),
             PAR_VALUE                      DECIMAL(11, 4),
             SIC_CODE                       CHAR(4),
             SHORT_SALE_FLAG                CHAR(1),
             LAST_UPD_DATE                  DATE NOT NULL,
             LAST_UPD_USER                  CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MSEC.SECURITY_MASTER               *
      ******************************************************************
       01  DCLSECURITY-MASTER.
           10 SM-CUSIP             PIC X(9).
           10 SM-ISIN              PIC X(12).
           10 SM-SYMBOL            PIC X(8).
           10 SM-SEC-DESC          PIC X(40).
           10 SM-SEC-TYPE          PIC X(2).
           10 SM-CCY               PIC X(3).
           10 SM-COUNTRY           PIC X(2).
           10 SM-EXCHANGE          PIC X(4).
           10 SM-SEC-STATUS        PIC X(1).
           10 SM-PRICE-FACTOR      PIC S9(5)V9(4) USAGE COMP-3.
           10 SM-SETTLE-DAYS       PIC S9(4) USAGE COMP.
           10 SM-DEPOSITORY        PIC X(4).
           10 SM-MIN-DENOM         PIC S9(9)V9(2) USAGE COMP-3.
           10 SM-ISSUER-ID         PIC X(6).
           10 SM-COUPON-RATE       PIC S9(3)V9(6) USAGE COMP-3.
           10 SM-MATURITY-DATE     PIC X(10).
           10 SM-ISSUE-DATE        PIC X(10).
           10 SM-FIRST-CPN-DATE    PIC X(10).
           10 SM-DAYCOUNT          PIC X(2).
           10 SM-COUPON-FREQ       PIC S9(4) USAGE COMP.
           10 SM-CALLABLE-FLAG     PIC X(1).
           10 SM-TAX-EXEMPT-FLAG   PIC X(1).
           10 SM-SHARES-OUT        PIC S9(13)V USAGE COMP-3.
           10 SM-DIV-FREQ          PIC S9(4) USAGE COMP.
           10 SM-ADR-RATIO         PIC S9(3)V9(4) USAGE COMP-3.
           10 SM-PAR-VALUE         PIC S9(7)V9(4) USAGE COMP-3.
           10 SM-SIC-CODE          PIC X(4).
           10 SM-SHORT-SALE-FLAG   PIC X(1).
           10 SM-LAST-UPD-DATE     PIC X(10).
           10 SM-LAST-UPD-USER     PIC X(8).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ISECURITY-MASTER.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 30 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
