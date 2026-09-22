      *================================================================*
      * PROGRAM    : CMD010                                            *
      * TITLE      : DB2 I/O MODULE - MSEC.SECURITY_MASTER             *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   READ-ONLY ACCESS TO THE SECURITY MASTER TABLE.  RETURNS THE  *
      *   ROW IN THE OLD VSAM RECORD LAYOUT (CMSECMS) SO THAT CALLERS  *
      *   WRITTEN BEFORE THE DB2 CONVERSION DID NOT HAVE TO CHANGE.    *
      *   THE FIXED INCOME / EQUITY COLUMNS ARE NULLABLE AND ARE       *
      *   MAPPED INTO THE MATCHING REDEFINES OF SEC-TYPE-DATA.         *
      *     'GET '  BY CUSIP  (SINGLETON SELECT)                       *
      *     'GETI'  BY ISIN   (CURSOR, LOWEST CUSIP)                   *
      *     'GETS'  BY SYMBOL (CURSOR, ACTIVE ROW, LOWEST CUSIP)       *
      *                                                                *
      * LINKAGE    : CALL 'CMD010' USING SL-SECURITY-PARMS (CMSECLNK)  *
      * TABLES     : MSEC.SECURITY_MASTER  (SELECT)                    *
      * PLAN/PKG   : COLLECTION MSCMCOL, IN PLANS MSCMPLN MSTCPLN      *
      *              MSSRPLN MSCAPLN                                   *
      * RETURN     : SL-RETURN-CODE 00 FOUND, 04 NOT FOUND (+100),     *
      *              12 SQL ERROR (SL-SQLCODE)                         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1996-04-22 DWB  CHG02215  ORIGINAL - SECURITY MASTER MOVED     *
      *                           FROM VSAM TO DB2, LAYOUT KEPT        *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD FROM ISO        *
      * 2005-08-30 KAP  CHG13391  ISIN LOOKUP ('GETI')                 *
      * 2011-09-12 SPA  CHG22410  SYMBOL LOOKUP ('GETS') FOR OMS FEED  *
      *                           - ACTIVE ROWS ONLY, SYMBOLS ARE      *
      *                           REUSED AFTER DELISTING               *
      * 2024-02-12 NVR  CHG58810  T+1 - SETTLE_DAYS COLUMN             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMD010.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  04/22/96.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMD010 WORKING STORAGE BEGINS'.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLSECMS END-EXEC.
      *
      *    NAMED VIEW OF THE DCLGEN INDICATOR ARRAY (ONE PER COLUMN)
       01  WS-IND-NAMES REDEFINES ISECURITY-MASTER.
           05  FILLER                  PIC S9(4) COMP OCCURS 14 TIMES.
           05  IND-COUPON-RATE         PIC S9(4) COMP.
           05  IND-MATURITY-DATE       PIC S9(4) COMP.
           05  IND-ISSUE-DATE          PIC S9(4) COMP.
           05  IND-FIRST-CPN-DATE      PIC S9(4) COMP.
           05  IND-DAYCOUNT            PIC S9(4) COMP.
           05  IND-COUPON-FREQ         PIC S9(4) COMP.
           05  IND-CALLABLE-FLAG       PIC S9(4) COMP.
           05  IND-TAX-EXEMPT-FLAG     PIC S9(4) COMP.
           05  IND-SHARES-OUT          PIC S9(4) COMP.
           05  IND-DIV-FREQ            PIC S9(4) COMP.
           05  IND-ADR-RATIO           PIC S9(4) COMP.
           05  IND-PAR-VALUE           PIC S9(4) COMP.
           05  IND-SIC-CODE            PIC S9(4) COMP.
           05  IND-SHORT-SALE-FLAG     PIC S9(4) COMP.
           05  FILLER                  PIC S9(4) COMP OCCURS 2 TIMES.
      *
      *    ROW REBUILT IN THE VSAM-ERA LAYOUT
           COPY CMSECMS.
      *
       01  WS-ISO-DATE                 PIC X(10).
       01  WS-ISO-DATE-R REDEFINES WS-ISO-DATE.
           05  WS-ISO-CCYY             PIC X(04).
           05  FILLER                  PIC X(01).
           05  WS-ISO-MM               PIC X(02).
           05  FILLER                  PIC X(01).
           05  WS-ISO-DD               PIC X(02).
       01  WS-DATE-8                   PIC 9(08).
       01  WS-DATE-8-X REDEFINES WS-DATE-8.
           05  WS-D8-CCYY              PIC X(04).
           05  WS-D8-MM                PIC X(02).
           05  WS-D8-DD                PIC X(02).
      *
       01  WS-CALL-COUNTS              COMP.
           05  WS-GET-CALLS            PIC S9(09) VALUE ZERO.
           05  WS-GETI-CALLS           PIC S9(09) VALUE ZERO.
           05  WS-GETS-CALLS           PIC S9(09) VALUE ZERO.
      *
      *    DSNTIAR MESSAGE AREA
       01  WS-ERROR-MESSAGE.
           05  WS-ERROR-LEN            PIC S9(04) COMP VALUE +720.
           05  WS-ERROR-TEXT           PIC X(72) OCCURS 10 TIMES
                                       INDEXED BY ERR-IX.
       01  WS-ERROR-TEXT-LEN           PIC S9(09) COMP VALUE +72.
      *
      *----------------------------------------------------------------*
      * CURSORS                                                        *
      *----------------------------------------------------------------*
           EXEC SQL DECLARE C-SECM-ISIN CURSOR FOR
               SELECT CUSIP, ISIN, SYMBOL, SEC_DESC, SEC_TYPE, CCY,
                      COUNTRY, EXCHANGE, SEC_STATUS, PRICE_FACTOR,
                      SETTLE_DAYS, DEPOSITORY, MIN_DENOM, ISSUER_ID,
                      COUPON_RATE,
                      CHAR(MATURITY_DATE, ISO),
                      CHAR(ISSUE_DATE, ISO),
                      CHAR(FIRST_CPN_DATE, ISO),
                      DAYCOUNT, COUPON_FREQ, CALLABLE_FLAG,
                      TAX_EXEMPT_FLAG, SHARES_OUT, DIV_FREQ,
                      ADR_RATIO, PAR_VALUE, SIC_CODE, SHORT_SALE_FLAG,
                      CHAR(LAST_UPD_DATE, ISO), LAST_UPD_USER
                 FROM MSEC.SECURITY_MASTER
                WHERE ISIN = :SM-ISIN
                ORDER BY CUSIP
                FETCH FIRST 1 ROW ONLY
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
           EXEC SQL DECLARE C-SECM-SYMBOL CURSOR FOR
               SELECT CUSIP, ISIN, SYMBOL, SEC_DESC, SEC_TYPE, CCY,
                      COUNTRY, EXCHANGE, SEC_STATUS, PRICE_FACTOR,
                      SETTLE_DAYS, DEPOSITORY, MIN_DENOM, ISSUER_ID,
                      COUPON_RATE,
                      CHAR(MATURITY_DATE, ISO),
                      CHAR(ISSUE_DATE, ISO),
                      CHAR(FIRST_CPN_DATE, ISO),
                      DAYCOUNT, COUPON_FREQ, CALLABLE_FLAG,
                      TAX_EXEMPT_FLAG, SHARES_OUT, DIV_FREQ,
                      ADR_RATIO, PAR_VALUE, SIC_CODE, SHORT_SALE_FLAG,
                      CHAR(LAST_UPD_DATE, ISO), LAST_UPD_USER
                 FROM MSEC.SECURITY_MASTER
                WHERE SYMBOL     = :SM-SYMBOL
                  AND SEC_STATUS = 'A'
                ORDER BY CUSIP
                FETCH FIRST 1 ROW ONLY
                FOR FETCH ONLY
                WITH UR
           END-EXEC.
      *
       LINKAGE SECTION.
           COPY CMSECLNK.
      *
       PROCEDURE DIVISION USING SL-SECURITY-PARMS.
      *
       0000-MAINLINE.
           MOVE ZERO   TO SL-RETURN-CODE SL-SQLCODE
           MOVE SPACES TO SL-SEC-DATA
           EVALUATE SL-FUNCTION
               WHEN 'GET '
                   ADD 1 TO WS-GET-CALLS
                   PERFORM 1000-GET-BY-CUSIP
               WHEN 'GETI'
                   ADD 1 TO WS-GETI-CALLS
                   PERFORM 2000-GET-BY-ISIN
               WHEN 'GETS'
                   ADD 1 TO WS-GETS-CALLS
                   PERFORM 3000-GET-BY-SYMBOL
               WHEN OTHER
                   DISPLAY 'CMD010 - INVALID FUNCTION ' SL-FUNCTION
                   MOVE 12 TO SL-RETURN-CODE
           END-EVALUATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - SINGLETON SELECT BY PRIMARY KEY                         *
      *----------------------------------------------------------------*
       1000-GET-BY-CUSIP.
           MOVE SL-KEY-CUSIP TO SM-CUSIP
           EXEC SQL
               SELECT CUSIP, ISIN, SYMBOL, SEC_DESC, SEC_TYPE, CCY,
                      COUNTRY, EXCHANGE, SEC_STATUS, PRICE_FACTOR,
                      SETTLE_DAYS, DEPOSITORY, MIN_DENOM, ISSUER_ID,
                      COUPON_RATE,
                      CHAR(MATURITY_DATE, ISO),
                      CHAR(ISSUE_DATE, ISO),
                      CHAR(FIRST_CPN_DATE, ISO),
                      DAYCOUNT, COUPON_FREQ, CALLABLE_FLAG,
                      TAX_EXEMPT_FLAG, SHARES_OUT, DIV_FREQ,
                      ADR_RATIO, PAR_VALUE, SIC_CODE, SHORT_SALE_FLAG,
                      CHAR(LAST_UPD_DATE, ISO), LAST_UPD_USER
                 INTO :SM-CUSIP, :SM-ISIN, :SM-SYMBOL, :SM-SEC-DESC,
                      :SM-SEC-TYPE, :SM-CCY, :SM-COUNTRY,
                      :SM-EXCHANGE, :SM-SEC-STATUS, :SM-PRICE-FACTOR,
                      :SM-SETTLE-DAYS, :SM-DEPOSITORY, :SM-MIN-DENOM,
                      :SM-ISSUER-ID,
                      :SM-COUPON-RATE      :IND-COUPON-RATE,
                      :SM-MATURITY-DATE    :IND-MATURITY-DATE,
                      :SM-ISSUE-DATE       :IND-ISSUE-DATE,
                      :SM-FIRST-CPN-DATE   :IND-FIRST-CPN-DATE,
                      :SM-DAYCOUNT         :IND-DAYCOUNT,
                      :SM-COUPON-FREQ      :IND-COUPON-FREQ,
                      :SM-CALLABLE-FLAG    :IND-CALLABLE-FLAG,
                      :SM-TAX-EXEMPT-FLAG  :IND-TAX-EXEMPT-FLAG,
                      :SM-SHARES-OUT       :IND-SHARES-OUT,
                      :SM-DIV-FREQ         :IND-DIV-FREQ,
                      :SM-ADR-RATIO        :IND-ADR-RATIO,
                      :SM-PAR-VALUE        :IND-PAR-VALUE,
                      :SM-SIC-CODE         :IND-SIC-CODE,
                      :SM-SHORT-SALE-FLAG  :IND-SHORT-SALE-FLAG,
                      :SM-LAST-UPD-DATE, :SM-LAST-UPD-USER
                 FROM MSEC.SECURITY_MASTER
                WHERE CUSIP = :SM-CUSIP
                WITH UR
           END-EXEC
           PERFORM 8000-CHECK-SQLCODE.
      *
      *----------------------------------------------------------------*
      * 2000 - BY ISIN (CHG13391).  AN ISIN CAN MAP TO MORE THAN ONE   *
      * CUSIP (144A / REG S LINES) - THE LOWEST CUSIP IS RETURNED.     *
      *----------------------------------------------------------------*
       2000-GET-BY-ISIN.
           MOVE SL-KEY-ISIN TO SM-ISIN
           EXEC SQL OPEN C-SECM-ISIN END-EXEC
           IF SQLCODE NOT = ZERO
               PERFORM 9000-SQL-ERROR
           ELSE
               EXEC SQL
                   FETCH C-SECM-ISIN
                    INTO :SM-CUSIP, :SM-ISIN, :SM-SYMBOL, :SM-SEC-DESC,
                         :SM-SEC-TYPE, :SM-CCY, :SM-COUNTRY,
                         :SM-EXCHANGE, :SM-SEC-STATUS,
                         :SM-PRICE-FACTOR, :SM-SETTLE-DAYS,
                         :SM-DEPOSITORY, :SM-MIN-DENOM, :SM-ISSUER-ID,
                         :SM-COUPON-RATE      :IND-COUPON-RATE,
                         :SM-MATURITY-DATE    :IND-MATURITY-DATE,
                         :SM-ISSUE-DATE       :IND-ISSUE-DATE,
                         :SM-FIRST-CPN-DATE   :IND-FIRST-CPN-DATE,
                         :SM-DAYCOUNT         :IND-DAYCOUNT,
                         :SM-COUPON-FREQ      :IND-COUPON-FREQ,
                         :SM-CALLABLE-FLAG    :IND-CALLABLE-FLAG,
                         :SM-TAX-EXEMPT-FLAG  :IND-TAX-EXEMPT-FLAG,
                         :SM-SHARES-OUT       :IND-SHARES-OUT,
                         :SM-DIV-FREQ         :IND-DIV-FREQ,
                         :SM-ADR-RATIO        :IND-ADR-RATIO,
                         :SM-PAR-VALUE        :IND-PAR-VALUE,
                         :SM-SIC-CODE         :IND-SIC-CODE,
                         :SM-SHORT-SALE-FLAG  :IND-SHORT-SALE-FLAG,
                         :SM-LAST-UPD-DATE, :SM-LAST-UPD-USER
               END-EXEC
               PERFORM 8000-CHECK-SQLCODE
               EXEC SQL CLOSE C-SECM-ISIN END-EXEC
           END-IF.
      *
      *----------------------------------------------------------------*
      * 3000 - BY TICKER SYMBOL, ACTIVE SECURITIES ONLY (CHG22410)     *
      *----------------------------------------------------------------*
       3000-GET-BY-SYMBOL.
           MOVE SL-KEY-SYMBOL TO SM-SYMBOL
           EXEC SQL OPEN C-SECM-SYMBOL END-EXEC
           IF SQLCODE NOT = ZERO
               PERFORM 9000-SQL-ERROR
           ELSE
               EXEC SQL
                   FETCH C-SECM-SYMBOL
                    INTO :SM-CUSIP, :SM-ISIN, :SM-SYMBOL, :SM-SEC-DESC,
                         :SM-SEC-TYPE, :SM-CCY, :SM-COUNTRY,
                         :SM-EXCHANGE, :SM-SEC-STATUS,
                         :SM-PRICE-FACTOR, :SM-SETTLE-DAYS,
                         :SM-DEPOSITORY, :SM-MIN-DENOM, :SM-ISSUER-ID,
                         :SM-COUPON-RATE      :IND-COUPON-RATE,
                         :SM-MATURITY-DATE    :IND-MATURITY-DATE,
                         :SM-ISSUE-DATE       :IND-ISSUE-DATE,
                         :SM-FIRST-CPN-DATE   :IND-FIRST-CPN-DATE,
                         :SM-DAYCOUNT         :IND-DAYCOUNT,
                         :SM-COUPON-FREQ      :IND-COUPON-FREQ,
                         :SM-CALLABLE-FLAG    :IND-CALLABLE-FLAG,
                         :SM-TAX-EXEMPT-FLAG  :IND-TAX-EXEMPT-FLAG,
                         :SM-SHARES-OUT       :IND-SHARES-OUT,
                         :SM-DIV-FREQ         :IND-DIV-FREQ,
                         :SM-ADR-RATIO        :IND-ADR-RATIO,
                         :SM-PAR-VALUE        :IND-PAR-VALUE,
                         :SM-SIC-CODE         :IND-SIC-CODE,
                         :SM-SHORT-SALE-FLAG  :IND-SHORT-SALE-FLAG,
                         :SM-LAST-UPD-DATE, :SM-LAST-UPD-USER
               END-EXEC
               PERFORM 8000-CHECK-SQLCODE
               EXEC SQL CLOSE C-SECM-SYMBOL END-EXEC
           END-IF.
      *
      *----------------------------------------------------------------*
      * 5000 - REBUILD THE CMSECMS RECORD FROM THE HOST VARIABLES      *
      *----------------------------------------------------------------*
       5000-BUILD-RECORD.
           MOVE SPACES              TO SEC-MASTER-REC
           MOVE SM-CUSIP            TO SEC-CUSIP
           MOVE SM-ISIN             TO SEC-ISIN
           MOVE SM-SYMBOL           TO SEC-SYMBOL
           MOVE SM-SEC-DESC         TO SEC-DESC
           MOVE SM-SEC-TYPE         TO SEC-TYPE
           MOVE SM-CCY              TO SEC-CCY
           MOVE SM-COUNTRY          TO SEC-COUNTRY
           MOVE SM-EXCHANGE         TO SEC-EXCHANGE
           MOVE SM-SEC-STATUS       TO SEC-STATUS
           MOVE SM-PRICE-FACTOR     TO SEC-PRICE-FACTOR
           MOVE SM-SETTLE-DAYS      TO SEC-SETTLE-DAYS
           MOVE SM-DEPOSITORY       TO SEC-DEPOSITORY
           MOVE SM-MIN-DENOM        TO SEC-MIN-DENOM
           MOVE SM-ISSUER-ID        TO SEC-ISSUER-ID
           MOVE SM-LAST-UPD-DATE    TO WS-ISO-DATE
           PERFORM 7000-ISO-TO-CCYYMMDD
           MOVE WS-DATE-8           TO SEC-LAST-UPD-DATE
           MOVE SM-LAST-UPD-USER    TO SEC-LAST-UPD-USER
           IF SEC-FIXED-INCOME
               PERFORM 5100-FIXED-INCOME-VIEW
           ELSE
               PERFORM 5200-EQUITY-VIEW
           END-IF
           MOVE SEC-MASTER-REC      TO SL-SEC-DATA.
      *
       5100-FIXED-INCOME-VIEW.
           MOVE ZERO TO SEC-COUPON-RATE SEC-MATURITY-DATE
                        SEC-ISSUE-DATE SEC-FIRST-CPN-DATE
                        SEC-COUPON-FREQ
           IF IND-COUPON-RATE NOT < ZERO
               MOVE SM-COUPON-RATE  TO SEC-COUPON-RATE
           END-IF
           IF IND-MATURITY-DATE NOT < ZERO
               MOVE SM-MATURITY-DATE TO WS-ISO-DATE
               PERFORM 7000-ISO-TO-CCYYMMDD
               MOVE WS-DATE-8       TO SEC-MATURITY-DATE
           END-IF
           IF IND-ISSUE-DATE NOT < ZERO
               MOVE SM-ISSUE-DATE   TO WS-ISO-DATE
               PERFORM 7000-ISO-TO-CCYYMMDD
               MOVE WS-DATE-8       TO SEC-ISSUE-DATE
           END-IF
           IF IND-FIRST-CPN-DATE NOT < ZERO
               MOVE SM-FIRST-CPN-DATE TO WS-ISO-DATE
               PERFORM 7000-ISO-TO-CCYYMMDD
               MOVE WS-DATE-8       TO SEC-FIRST-CPN-DATE
           END-IF
           IF IND-DAYCOUNT NOT < ZERO
               MOVE SM-DAYCOUNT     TO SEC-DAYCOUNT
           END-IF
           IF IND-COUPON-FREQ NOT < ZERO
               MOVE SM-COUPON-FREQ  TO SEC-COUPON-FREQ
           END-IF
           IF IND-CALLABLE-FLAG NOT < ZERO
               MOVE SM-CALLABLE-FLAG TO SEC-CALLABLE-FLAG
           END-IF
           IF IND-TAX-EXEMPT-FLAG NOT < ZERO
               MOVE SM-TAX-EXEMPT-FLAG TO SEC-TAX-EXEMPT-FLAG
           END-IF.
      *
       5200-EQUITY-VIEW.
           MOVE ZERO TO SEC-SHARES-OUT SEC-DIV-FREQ SEC-ADR-RATIO
                        SEC-PAR-VALUE
           IF IND-SHARES-OUT NOT < ZERO
               MOVE SM-SHARES-OUT   TO SEC-SHARES-OUT
           END-IF
           IF IND-DIV-FREQ NOT < ZERO
               MOVE SM-DIV-FREQ     TO SEC-DIV-FREQ
           END-IF
           IF IND-ADR-RATIO NOT < ZERO
               MOVE SM-ADR-RATIO    TO SEC-ADR-RATIO
           END-IF
           IF IND-PAR-VALUE NOT < ZERO
               MOVE SM-PAR-VALUE    TO SEC-PAR-VALUE
           END-IF
           IF IND-SIC-CODE NOT < ZERO
               MOVE SM-SIC-CODE     TO SEC-SIC-CODE
           END-IF
           IF IND-SHORT-SALE-FLAG NOT < ZERO
               MOVE SM-SHORT-SALE-FLAG TO SEC-SHORT-SALE-FLAG
           END-IF.
      *
      *----------------------------------------------------------------*
      * 7000 - 'YYYY-MM-DD' (WS-ISO-DATE) TO CCYYMMDD (WS-DATE-8)      *
      *----------------------------------------------------------------*
       7000-ISO-TO-CCYYMMDD.
           IF WS-ISO-DATE = SPACES OR LOW-VALUES
               MOVE ZERO TO WS-DATE-8
           ELSE
               MOVE WS-ISO-CCYY TO WS-D8-CCYY
               MOVE WS-ISO-MM   TO WS-D8-MM
               MOVE WS-ISO-DD   TO WS-D8-DD
           END-IF.
      *
      *----------------------------------------------------------------*
      * 8000 - SQLCODE HANDLING                                        *
      *----------------------------------------------------------------*
       8000-CHECK-SQLCODE.
           MOVE SQLCODE TO SL-SQLCODE
           EVALUATE TRUE
               WHEN SQLCODE = ZERO
                   MOVE 00 TO SL-RETURN-CODE
                   PERFORM 5000-BUILD-RECORD
               WHEN SQLCODE = +100
                   MOVE 04 TO SL-RETURN-CODE
               WHEN SQLCODE > ZERO
      *            WARNINGS (E.G. +304 VALUE TRUNCATED) - KEEP THE ROW
                   DISPLAY 'CMD010 - SQL WARNING ' SQLCODE
                           ' CUSIP ' SM-CUSIP
                   MOVE 00 TO SL-RETURN-CODE
                   PERFORM 5000-BUILD-RECORD
               WHEN OTHER
                   PERFORM 9000-SQL-ERROR
           END-EVALUATE.
      *
       9000-SQL-ERROR.
           MOVE SQLCODE TO SL-SQLCODE
           MOVE 12      TO SL-RETURN-CODE
           DISPLAY 'CMD010 - SQL ERROR, FUNCTION ' SL-FUNCTION
                   ' SQLCODE ' SQLCODE
           CALL 'DSNTIAR' USING SQLCA WS-ERROR-MESSAGE
                                WS-ERROR-TEXT-LEN
           PERFORM VARYING ERR-IX FROM 1 BY 1 UNTIL ERR-IX > 10
               IF WS-ERROR-TEXT (ERR-IX) NOT = SPACES
                   DISPLAY 'CMD010 - ' WS-ERROR-TEXT (ERR-IX)
               END-IF
           END-PERFORM.
