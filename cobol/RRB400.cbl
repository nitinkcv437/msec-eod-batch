       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRB400.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JANUARY 1992.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRB400                                            *
      * DESCRIPTION: TAX YEAR-TO-DATE ACCUMULATION.                    *
      *              POSTS THE DAY'S TAX-REPORTABLE ACTIVITY TO THE    *
      *              TAX YEAR-TO-DATE MASTER (ONE ROW PER ACCOUNT, TAX *
      *              YEAR AND FORM) USED FOR THE YEAR-END 1099-DIV,    *
      *              1099-B AND 1042-S PRODUCTION.                     *
      *                                                                *
      *              TAX YEAR = YEAR OF THE BUSINESS DATE.             *
      *              FORM     = 1042 FOREIGN HOLDER (TAX STATUS F)     *
      *                         1099 DOMESTIC HOLDER (TAX STATUS D)    *
      *                         EXEMPT HOLDERS (E) ARE NOT REPORTED.   *
      *                                                                *
      *              CORPORATE ACTION ACTIVITY (CA.SRACTV):            *
      *                DIV  1099 ORDINARY DIVIDENDS, ALSO QUALIFIED    *
      *                          WHEN THE ISSUER IS US AND THE         *
      *                          SECURITY IS EQ OR AD                  *
      *                     1042 GROSS INCOME                          *
      *                WHT  1099 FOREIGN TAX PAID                      *
      *                     1042 TAX WITHHELD                          *
      *                CIL  CASH IN LIEU PROCEEDS  (+ GROSS PROCEEDS)  *
      *                MGC  CASH MERGER PROCEEDS   (+ GROSS PROCEEDS)  *
      *              POSTING JOURNAL (SR.POSTJRNL), CLIENT OWNERSHIP   *
      *              LEG OF SEL / SSL:                                 *
      *                GROSS PROCEEDS = ABS(CASH)                      *
      *                REALIZED P&L   = PSJ-REALIZED-PL                *
      *                COST BASIS     = PROCEEDS - REALIZED P&L        *
      *              NON-USD AMOUNTS ARE CONVERTED WITH CMU040 AT THE  *
      *              RATE OF THE EFFECTIVE (PAY) DATE.                 *
      *                                                                *
      *              EVERY TRANSACTION APPLIED IS ALSO WRITTEN TO THE  *
      *              TAX ACTIVITY FILE FOR THE DAILY REPORT RRR410.    *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRRD030 / STEP010  (IKJEFT01 - DB2 PLAN MSRRPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              CAACTV   - MSEC.PROD.CA.SRACTV(0)     (SRACTV)    *
      *              JRNLIN   - MSEC.PROD.SR.POSTJRNL(0)   (SRPSTJ)    *
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS (CMACCT)    *
      * UPDATE     : TAXYTD   - MSEC.PROD.RR.TAXYTD.KSDS   (RRTAXY)    *
      * OUTPUT     : TAXACT   - MSEC.PROD.RR.TAXACT(+1)    (RRTAXA)    *
      * CALLS      : CMD010 (ISSUER COUNTRY), CMU040, CMU050, CMU060,  *
      *              CMU080, CMASM01                                   *
      * RETURN CODE: 0 CLEAN                                           *
      *              4 ACCOUNT NOT ON FILE / NO TAX STATUS, SECURITY   *
      *                NOT ON FILE, FX RATE MISSING OR STALE, SALE     *
      *                CANCELLATIONS NOT REVERSED                      *
      * RERUN      : THE MASTER IS UPDATED IN PLACE - RESTORE IT FROM  *
      *              THE PREVIOUS NIGHT'S BACKUP BEFORE A RERUN        *
      *              (RUNBOOK RR-030).                                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1992-01-20 DWB  ORIGINAL - 1099-DIV ACCUMULATION      CHG01102 *
      * 1994-11-07 DWB  1099-B GROSS PROCEEDS FROM JOURNAL    CHG01744 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES, 4-DIGIT YEAR    CHG04471 *
      * 2001-01-15 KAP  1042-S FOR FOREIGN HOLDERS (QI)       CHG07905 *
      * 2003-06-02 KAP  QUALIFIED DIVIDENDS                   CHG11102 *
      * 2009-12-14 SPA  NON-USD INCOME VIA CMU040             CHG19002 *
      * 2011-01-03 SPA  COST BASIS / REALIZED P&L (1099-B)    CHG21150 *
      * 2016-10-03 SPA  CA.SRACTV REPLACES THE ENTITLEMENT    CHG30112 *
      *                 PAYMENT TAPE                                   *
      * 2021-01-11 JLR  TAX ACTIVITY FILE FOR RRR410          CHG35610 *
      * 2022-01-10 JLR  WARN ON PRIOR-YEAR PAYMENTS AND ON A  CHG36540 *
      *                 CHANGE OF HOLDER TAX STATUS                    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CAACTV-FILE    ASSIGN TO CAACTV
                  FILE STATUS IS WS-CAACTV-STATUS.
           SELECT JRNLIN-FILE    ASSIGN TO JRNLIN
                  FILE STATUS IS WS-JRNLIN-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCTMAST-KEY
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT TAXYTD-FILE    ASSIGN TO TAXYTD
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS TAXYTD-KEY
                  FILE STATUS IS WS-TAXYTD-STATUS.
           SELECT TAXACT-FILE    ASSIGN TO TAXACT
                  FILE STATUS IS WS-TAXACT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  CAACTV-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CAACTV-REC                  PIC X(200).
       FD  JRNLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  JRNLIN-REC                  PIC X(300).
       FD  ACCTMAST-FILE.
       01  ACCTMAST-REC.
           05  ACCTMAST-KEY            PIC X(10).
           05  FILLER                  PIC X(290).
       FD  TAXYTD-FILE.
       01  TAXYTD-REC.
           05  TAXYTD-KEY              PIC X(18).
           05  FILLER                  PIC X(182).
       FD  TAXACT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  TAXACT-REC                  PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRB400'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-CAACTV-STATUS        PIC X(02)  VALUE '00'.
               88  CAACTV-OK                      VALUE '00'.
               88  CAACTV-EOF                     VALUE '10'.
           05  WS-JRNLIN-STATUS        PIC X(02)  VALUE '00'.
               88  JRNLIN-OK                      VALUE '00'.
               88  JRNLIN-EOF                     VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-TAXYTD-STATUS        PIC X(02)  VALUE '00'.
               88  TAXYTD-OK                      VALUE '00'.
               88  TAXYTD-NOTFND                  VALUE '23'.
           05  WS-TAXACT-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-CA-EOF-SW            PIC X(01)  VALUE 'N'.
               88  END-OF-CA-ACTIVITY             VALUE 'Y'.
           05  WS-JRNL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-JOURNAL                 VALUE 'Y'.
           05  WS-REPORTABLE-SW        PIC X(01)  VALUE 'N'.
               88  HOLDER-REPORTABLE              VALUE 'Y'.
           05  WS-QUALIFIED-SW         PIC X(01)  VALUE 'N'.
               88  DIVIDEND-QUALIFIED             VALUE 'Y'.
           05  WS-YTD-FOUND-SW         PIC X(01)  VALUE 'N'.
               88  YTD-ROW-FOUND                  VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-TAX-YEAR                 PIC 9(04).
      *----------------------------------------------------------------*
      * ONE TRANSACTION TO APPLY                                       *
      *----------------------------------------------------------------*
       01  WS-TXN.
           05  WS-TX-SOURCE            PIC X(02).
           05  WS-TX-REF               PIC X(16).
           05  WS-TX-ACCT              PIC X(10).
           05  WS-TX-CUSIP             PIC X(09).
           05  WS-TX-TYPE              PIC X(03).
           05  WS-TX-SEC-TYPE          PIC X(02).
           05  WS-TX-CCY               PIC X(03).
           05  WS-TX-RATE-DATE         PIC 9(08).
           05  WS-TX-AMOUNT            PIC S9(15)V99    COMP-3.
           05  WS-TX-AMOUNT-USD        PIC S9(15)V99    COMP-3.
           05  WS-TX-REALIZED-PL       PIC S9(15)V99    COMP-3.
           05  WS-TX-PL-USD            PIC S9(15)V99    COMP-3.
           05  WS-TX-CATEGORY          PIC X(04).
           05  WS-TX-ACTION            PIC X(01).
       01  WS-TX-FORM                  PIC X(04).
      *----------------------------------------------------------------*
      * ACCOUNT / SECURITY WORK                                        *
      *----------------------------------------------------------------*
       01  WS-ACCT-CACHE.
           05  WS-AC-ACCT              PIC X(10)  VALUE LOW-VALUES.
           05  WS-AC-FOUND             PIC X(01)  VALUE 'N'.
           05  WS-AC-TAX-STATUS        PIC X(01)  VALUE SPACE.
           05  WS-AC-TAX-COUNTRY       PIC X(02)  VALUE SPACES.
       01  WS-SEC-CACHE.
           05  WS-SC-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-SC-ENTRY             OCCURS 1000 TIMES
                                       INDEXED BY SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-COUNTRY       PIC X(02).
               10  WS-SC-TYPE          PIC X(02).
       01  WS-SC-MAX                   PIC S9(04) COMP  VALUE 1000.
       01  WS-ISSUER-COUNTRY           PIC X(02).
       01  WS-ISSUER-SEC-TYPE          PIC X(02).
       01  WS-ABS-CASH                 PIC S9(15)V99    COMP-3.
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-CA-READ              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CA-STREET            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CA-NOT-TAXABLE       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CA-HASH              PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-JRNL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-NOT-SALE        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-XSL             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-HASH            PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
           05  WS-EXEMPT-SKIPPED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRIOR-YEAR-PAY       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STATUS-CHANGED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-STATUS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-NOT-FOUND       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-NOT-FOUND        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-CALLS             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FX-PROBLEMS          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TXN-APPLIED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-YTD-INSERTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-YTD-UPDATED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TAXACT-OUT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-APPLIED-USD          PIC S9(15)V99 COMP-3
                                                     VALUE ZERO.
      *    ---- BY CATEGORY FOR THE STATISTICS ------------------------
       01  WS-CATEGORY-VALUES.
           05  FILLER                  PIC X(04)  VALUE 'ODIV'.
           05  FILLER                  PIC X(04)  VALUE 'QDIV'.
           05  FILLER                  PIC X(04)  VALUE 'FTAX'.
           05  FILLER                  PIC X(04)  VALUE '42GI'.
           05  FILLER                  PIC X(04)  VALUE '42TW'.
           05  FILLER                  PIC X(04)  VALUE 'CILP'.
           05  FILLER                  PIC X(04)  VALUE 'MRGP'.
           05  FILLER                  PIC X(04)  VALUE 'SALE'.
       01  WS-CATEGORY-TABLE REDEFINES WS-CATEGORY-VALUES.
           05  WS-CAT-CODE             PIC X(04)  OCCURS 8 TIMES.
       01  WS-CATEGORY-TOTALS.
           05  WS-CAT-TOTAL            OCCURS 8 TIMES.
               10  WS-CAT-COUNT        PIC S9(09)       COMP-3.
               10  WS-CAT-AMOUNT       PIC S9(15)V99    COMP-3.
       01  WS-CAT-SUB                  PIC S9(04) COMP.
       COPY SRACTV.
       COPY SRPSTJ.
       COPY CMACCT.
       COPY RRTAXY.
       COPY RRTAXA.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMSECLNK.
       COPY CMFXLNK.
       COPY CMJILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-CA-ACTIVITY THRU 2000-EXIT
               UNTIL END-OF-CA-ACTIVITY.
           PERFORM 3000-POSTING-JOURNAL THRU 3000-EXIT
               UNTIL END-OF-JOURNAL.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE DC-BUS-CCYY TO WS-TAX-YEAR.
           CALL 'CMASM01' USING JI-JOB-INFO.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE WS-TAX-YEAR    TO AU-KEY.
           MOVE 'TAX YEAR-TO-DATE ACCUMULATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           INITIALIZE WS-CATEGORY-TOTALS.
           OPEN INPUT CAACTV-FILE.
           IF WS-CAACTV-STATUS NOT = '00'
               MOVE 'CAACTV' TO AB-DDNAME
               MOVE WS-CAACTV-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT JRNLIN-FILE.
           IF WS-JRNLIN-STATUS NOT = '00'
               MOVE 'JRNLIN' TO AB-DDNAME
               MOVE WS-JRNLIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O TAXYTD-FILE.
           IF WS-TAXYTD-STATUS NOT = '00'
               MOVE 'TAXYTD' TO AB-DDNAME
               MOVE WS-TAXYTD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN I-O FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT TAXACT-FILE.
           IF WS-TAXACT-STATUS NOT = '00'
               MOVE 'TAXACT' TO AB-DDNAME
               MOVE WS-TAXACT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-CA THRU 8000-EXIT.
           PERFORM 8100-READ-JOURNAL THRU 8100-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * CORPORATE ACTION PAYMENTS.  CASH IS ON LEG 1 ONLY; THE STREET  *
      * LEG CARRIES THE SHARE MOVEMENT.                                *
      *================================================================*
       2000-CA-ACTIVITY.
           ADD 1 TO WS-CA-READ.
           ADD ACT-CASH-CHANGE TO WS-CA-HASH.
           IF ACT-ACCT-TYPE = 'ST' OR ACT-LEG-NO NOT = 1
               ADD 1 TO WS-CA-STREET
               GO TO 2000-NEXT
           END-IF.
           MOVE SPACES TO WS-TX-CATEGORY.
           EVALUATE TRUE
               WHEN ACT-CASH-DIVIDEND
                   MOVE ACT-CASH-CHANGE TO WS-TX-AMOUNT
               WHEN ACT-WITHHOLDING
                   COMPUTE WS-TX-AMOUNT = ACT-CASH-CHANGE * -1
               WHEN ACT-CASH-IN-LIEU
                   MOVE ACT-CASH-CHANGE TO WS-TX-AMOUNT
                   MOVE 'CILP' TO WS-TX-CATEGORY
               WHEN ACT-MERGER-CASH
                   MOVE ACT-CASH-CHANGE TO WS-TX-AMOUNT
                   MOVE 'MRGP' TO WS-TX-CATEGORY
               WHEN OTHER
      *            SDV / SPL / MGO - SHARE MOVEMENTS, BASIS ONLY
                   ADD 1 TO WS-CA-NOT-TAXABLE
                   GO TO 2000-NEXT
           END-EVALUATE.
           IF WS-TX-AMOUNT = ZERO
               ADD 1 TO WS-CA-NOT-TAXABLE
               GO TO 2000-NEXT
           END-IF.
           MOVE 'CA'            TO WS-TX-SOURCE.
           MOVE ACT-REF         TO WS-TX-REF.
           MOVE ACT-ACCT-NO     TO WS-TX-ACCT.
           MOVE ACT-CUSIP       TO WS-TX-CUSIP.
           MOVE ACT-TYPE        TO WS-TX-TYPE.
           MOVE ACT-SEC-TYPE    TO WS-TX-SEC-TYPE.
           MOVE ACT-CCY         TO WS-TX-CCY.
           MOVE ACT-EFFECTIVE-DATE TO WS-TX-RATE-DATE.
           MOVE ZERO            TO WS-TX-REALIZED-PL.
      *    PAYMENTS ARE REPORTED IN THE YEAR THEY ARE PROCESSED - A
      *    DECEMBER PAY DATE PROCESSED IN JANUARY IS LISTED FOR TAX
      *    OPERATIONS TO MOVE BY HAND (YEAR-END CHECKLIST ITEM 7)
           IF ACT-EFFECTIVE-DATE NUMERIC
           AND ACT-EFFECTIVE-DATE (1:4) NOT = DC-BUS-DATE (1:4)
           AND ACT-EFFECTIVE-DATE NOT = ZERO
               ADD 1 TO WS-PRIOR-YEAR-PAY
               DISPLAY 'RRB400 W - PAYMENT DATED ' ACT-EFFECTIVE-DATE
                       ' POSTED TO TAX YEAR ' WS-TAX-YEAR ' ' ACT-REF
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           PERFORM 5000-APPLY-TRANSACTION THRU 5000-EXIT.
       2000-NEXT.
           PERFORM 8000-READ-CA THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *================================================================*
      * SALES FROM THE POSTING JOURNAL (1099-B).  ONLY THE OWNERSHIP   *
      * LEG OF A SALE CARRIES THE CASH AND THE REALIZED P&L.           *
      *================================================================*
       3000-POSTING-JOURNAL.
           ADD 1 TO WS-JRNL-READ.
           MOVE PSJ-ACTIVITY TO ACT-ACTIVITY-REC.
           ADD ACT-CASH-CHANGE TO WS-JRNL-HASH.
           IF ACT-ACCT-TYPE = 'ST' OR ACT-LEG-NO NOT = 1
               ADD 1 TO WS-JRNL-STREET
               GO TO 3000-NEXT
           END-IF.
           EVALUATE TRUE
               WHEN ACT-SELL
               WHEN ACT-SHORT-SELL
                   CONTINUE
               WHEN ACT-CXL-SELL
      *            CANCELLED SALES ARE ADJUSTED BY TAX OPERATIONS ON
      *            THE YEAR-END CORRECTION SCREEN (CHG21150)
                   ADD 1 TO WS-JRNL-XSL
                   DISPLAY 'RRB400 W - SALE CANCELLATION NOT REVERSED '
                           ACT-REF ' ' ACT-ACCT-NO
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
                   GO TO 3000-NEXT
               WHEN OTHER
                   ADD 1 TO WS-JRNL-NOT-SALE
                   GO TO 3000-NEXT
           END-EVALUATE.
           IF ACT-CASH-CHANGE < ZERO
               COMPUTE WS-ABS-CASH = ACT-CASH-CHANGE * -1
           ELSE
               MOVE ACT-CASH-CHANGE TO WS-ABS-CASH
           END-IF.
           IF PSJ-REALIZED-PL NOT NUMERIC
               MOVE ZERO TO PSJ-REALIZED-PL
           END-IF.
           MOVE 'SR'            TO WS-TX-SOURCE.
           MOVE ACT-REF         TO WS-TX-REF.
           MOVE ACT-ACCT-NO     TO WS-TX-ACCT.
           MOVE ACT-CUSIP       TO WS-TX-CUSIP.
           MOVE ACT-TYPE        TO WS-TX-TYPE.
           MOVE ACT-SEC-TYPE    TO WS-TX-SEC-TYPE.
           MOVE ACT-CCY         TO WS-TX-CCY.
           MOVE ACT-TRADE-DATE  TO WS-TX-RATE-DATE.
           MOVE WS-ABS-CASH     TO WS-TX-AMOUNT.
           MOVE PSJ-REALIZED-PL TO WS-TX-REALIZED-PL.
           MOVE 'SALE'          TO WS-TX-CATEGORY.
           PERFORM 5000-APPLY-TRANSACTION THRU 5000-EXIT.
       3000-NEXT.
           PERFORM 8100-READ-JOURNAL THRU 8100-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
      * APPLY ONE TRANSACTION: HOLDER, FORM, ISSUER, USD, UPSERT       *
      *================================================================*
       5000-APPLY-TRANSACTION.
           PERFORM 5100-GET-HOLDER THRU 5100-EXIT.
           IF NOT HOLDER-REPORTABLE
               GO TO 5000-EXIT
           END-IF.
           PERFORM 5200-GET-ISSUER THRU 5200-EXIT.
           PERFORM 5300-CONVERT-TO-USD THRU 5300-EXIT.
           PERFORM 5400-SET-CATEGORY THRU 5400-EXIT.
           PERFORM 6000-UPSERT-TAXYTD THRU 6000-EXIT.
           PERFORM 7000-WRITE-TAX-ACTIVITY THRU 7000-EXIT.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * HOLDER TAX STATUS FROM THE ACCOUNT MASTER (LAST ACCOUNT KEPT). *
      *----------------------------------------------------------------*
       5100-GET-HOLDER.
           MOVE 'N' TO WS-REPORTABLE-SW.
           IF WS-TX-ACCT NOT = WS-AC-ACCT
               MOVE WS-TX-ACCT TO WS-AC-ACCT ACCTMAST-KEY
               READ ACCTMAST-FILE INTO ACCT-MASTER-REC
               EVALUATE TRUE
                   WHEN ACCTMAST-OK
                       MOVE 'Y'              TO WS-AC-FOUND
                       MOVE ACCT-TAX-STATUS  TO WS-AC-TAX-STATUS
                       MOVE ACCT-TAX-COUNTRY TO WS-AC-TAX-COUNTRY
                   WHEN ACCTMAST-NOTFND
                       MOVE 'N'              TO WS-AC-FOUND
                       MOVE SPACE            TO WS-AC-TAX-STATUS
                       MOVE SPACES           TO WS-AC-TAX-COUNTRY
                   WHEN OTHER
                       MOVE 'ACCTMAST' TO AB-DDNAME
                       MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '5100-GET-HOLDER' TO AB-PARAGRAPH
                       MOVE WS-TX-ACCT TO AB-KEY
                       MOVE 'READ FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-IF.
           IF WS-AC-FOUND NOT = 'Y'
               ADD 1 TO WS-ACCT-NOT-FOUND
               DISPLAY 'RRB400 W - ACCOUNT NOT ON MASTER ' WS-TX-ACCT
                       ' REF ' WS-TX-REF
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               GO TO 5100-EXIT
           END-IF.
           EVALUATE WS-AC-TAX-STATUS
               WHEN 'D'
                   MOVE '1099' TO WS-TX-FORM
                   MOVE 'Y'    TO WS-REPORTABLE-SW
               WHEN 'F'
                   MOVE '1042' TO WS-TX-FORM
                   MOVE 'Y'    TO WS-REPORTABLE-SW
               WHEN 'E'
                   ADD 1 TO WS-EXEMPT-SKIPPED
               WHEN OTHER
                   ADD 1 TO WS-NO-STATUS
                   DISPLAY 'RRB400 W - NO TAX STATUS ' WS-TX-ACCT
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE.
       5100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ISSUER COUNTRY AND SECURITY TYPE (CMD010, CACHED)              *
      *----------------------------------------------------------------*
       5200-GET-ISSUER.
           SET SC-IDX TO 1.
           SEARCH WS-SC-ENTRY
               AT END
                   PERFORM 5210-LOOKUP-SECURITY THRU 5210-EXIT
               WHEN SC-IDX > WS-SC-USED
                   PERFORM 5210-LOOKUP-SECURITY THRU 5210-EXIT
               WHEN WS-SC-CUSIP (SC-IDX) = WS-TX-CUSIP
                   MOVE WS-SC-COUNTRY (SC-IDX) TO WS-ISSUER-COUNTRY
                   MOVE WS-SC-TYPE (SC-IDX)    TO WS-ISSUER-SEC-TYPE
           END-SEARCH.
       5200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5210-LOOKUP-SECURITY.
      *----------------------------------------------------------------*
           MOVE 'GET '      TO SL-FUNCTION.
           MOVE WS-TX-CUSIP TO SL-KEY-CUSIP.
           MOVE SPACES      TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA TO SEC-MASTER-REC
                   MOVE SEC-COUNTRY TO WS-ISSUER-COUNTRY
                   MOVE SEC-TYPE    TO WS-ISSUER-SEC-TYPE
               WHEN SL-NOT-FOUND
                   ADD 1 TO WS-SEC-NOT-FOUND
                   MOVE '??'           TO WS-ISSUER-COUNTRY
                   MOVE WS-TX-SEC-TYPE TO WS-ISSUER-SEC-TYPE
                   DISPLAY 'RRB400 W - SECURITY NOT ON MASTER '
                           WS-TX-CUSIP
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE SL-SQLCODE TO AB-SQLCODE
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '5210-LOOKUP-SECURITY' TO AB-PARAGRAPH
                   MOVE WS-TX-CUSIP TO AB-KEY
                   MOVE 'CMD010 GET FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF WS-SC-USED < WS-SC-MAX
               ADD 1 TO WS-SC-USED
               MOVE WS-TX-CUSIP        TO WS-SC-CUSIP (WS-SC-USED)
               MOVE WS-ISSUER-COUNTRY  TO WS-SC-COUNTRY (WS-SC-USED)
               MOVE WS-ISSUER-SEC-TYPE TO WS-SC-TYPE (WS-SC-USED)
           END-IF.
       5210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TAX REPORTING IS IN USD.  RATE OF THE EFFECTIVE DATE.          *
      *----------------------------------------------------------------*
       5300-CONVERT-TO-USD.
           IF WS-TX-CCY = 'USD' OR WS-TX-CCY = SPACES
               MOVE WS-TX-AMOUNT      TO WS-TX-AMOUNT-USD
               MOVE WS-TX-REALIZED-PL TO WS-TX-PL-USD
               GO TO 5300-EXIT
           END-IF.
           IF WS-TX-RATE-DATE NOT NUMERIC OR WS-TX-RATE-DATE = ZERO
               MOVE DC-BUS-DATE TO WS-TX-RATE-DATE
           END-IF.
           ADD 1 TO WS-FX-CALLS.
           MOVE WS-TX-CCY       TO FX-FROM-CCY.
           MOVE 'USD'           TO FX-TO-CCY.
           MOVE WS-TX-RATE-DATE TO FX-RATE-DATE.
           MOVE WS-TX-AMOUNT    TO FX-AMOUNT-IN.
           CALL 'CMU040' USING FX-CONVERT-PARMS.
           EVALUATE TRUE
               WHEN FX-OK
                   MOVE FX-AMOUNT-OUT TO WS-TX-AMOUNT-USD
               WHEN FX-STALE-RATE
                   ADD 1 TO WS-FX-PROBLEMS
                   MOVE FX-AMOUNT-OUT TO WS-TX-AMOUNT-USD
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN FX-RATE-NOT-FOUND
                   ADD 1 TO WS-FX-PROBLEMS
                   MOVE WS-TX-AMOUNT TO WS-TX-AMOUNT-USD
                   DISPLAY 'RRB400 W - NO FX RATE ' WS-TX-CCY
                           ' - AMOUNT TAKEN AT PAR ' WS-TX-REF
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '5300-CONVERT-TO-USD' TO AB-PARAGRAPH
                   MOVE WS-TX-CCY TO AB-KEY
                   MOVE FX-MESSAGE TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
      *    P&L AT THE SAME RATE AS THE PROCEEDS
           IF WS-TX-REALIZED-PL = ZERO OR FX-RATE-NOT-FOUND
               MOVE WS-TX-REALIZED-PL TO WS-TX-PL-USD
           ELSE
               COMPUTE WS-TX-PL-USD ROUNDED =
                       WS-TX-REALIZED-PL * FX-RATE
           END-IF.
       5300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CATEGORY BY ACTIVITY TYPE AND FORM                             *
      *----------------------------------------------------------------*
       5400-SET-CATEGORY.
           IF WS-TX-CATEGORY NOT = SPACES
               GO TO 5400-EXIT
           END-IF.
           IF WS-TX-TYPE = 'DIV'
               IF WS-TX-FORM = '1042'
                   MOVE '42GI' TO WS-TX-CATEGORY
               ELSE
      *            QUALIFIED = DOMESTIC ISSUER, COMMON OR ADR.  THE
      *            HOLDING PERIOD TEST IS DONE AT YEAR END (CHG11102)
                   MOVE 'N' TO WS-QUALIFIED-SW
                   IF WS-ISSUER-COUNTRY = 'US'
                   AND (WS-ISSUER-SEC-TYPE = 'EQ'
                     OR WS-ISSUER-SEC-TYPE = 'AD')
                       MOVE 'Y' TO WS-QUALIFIED-SW
                   END-IF
                   IF DIVIDEND-QUALIFIED
                       MOVE 'QDIV' TO WS-TX-CATEGORY
                   ELSE
                       MOVE 'ODIV' TO WS-TX-CATEGORY
                   END-IF
               END-IF
           ELSE
               IF WS-TX-FORM = '1042'
                   MOVE '42TW' TO WS-TX-CATEGORY
               ELSE
                   MOVE 'FTAX' TO WS-TX-CATEGORY
               END-IF
           END-IF.
       5400-EXIT.
           EXIT.
      *================================================================*
      * UPSERT THE TAX YEAR-TO-DATE ROW                                *
      *================================================================*
       6000-UPSERT-TAXYTD.
           MOVE WS-TX-ACCT  TO RTX-ACCT-NO.
           MOVE WS-TAX-YEAR TO RTX-TAX-YEAR.
           MOVE WS-TX-FORM  TO RTX-FORM.
           MOVE RTX-KEY     TO TAXYTD-KEY.
           READ TAXYTD-FILE INTO RTX-TAX-YTD-REC.
           EVALUATE TRUE
               WHEN TAXYTD-OK
                   MOVE 'Y' TO WS-YTD-FOUND-SW
               WHEN TAXYTD-NOTFND
                   MOVE 'N' TO WS-YTD-FOUND-SW
                   PERFORM 6100-NEW-YTD-ROW THRU 6100-EXIT
               WHEN OTHER
                   MOVE 'TAXYTD' TO AB-DDNAME
                   MOVE WS-TAXYTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '6000-UPSERT-TAXYTD' TO AB-PARAGRAPH
                   MOVE TAXYTD-KEY TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 6200-ACCUMULATE THRU 6200-EXIT.
      *    W-8BEN / W-9 CHANGED DURING THE YEAR: THE ROW KEEPS THE
      *    FORM IT WAS OPENED UNDER, THE NEW STATUS IS RECORDED
           IF YTD-ROW-FOUND
           AND (RTX-TAX-STATUS NOT = WS-AC-TAX-STATUS
             OR RTX-TAX-COUNTRY NOT = WS-AC-TAX-COUNTRY)
               ADD 1 TO WS-STATUS-CHANGED
               DISPLAY 'RRB400 W - TAX STATUS CHANGED ' RTX-KEY ' '
                       RTX-TAX-STATUS RTX-TAX-COUNTRY ' -> '
                       WS-AC-TAX-STATUS WS-AC-TAX-COUNTRY
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           ADD 1                 TO RTX-TXN-COUNT.
           MOVE DC-BUS-DATE      TO RTX-LAST-UPD-DATE.
           MOVE JI-JOBNAME       TO RTX-LAST-UPD-JOB.
           MOVE WS-AC-TAX-STATUS TO RTX-TAX-STATUS.
           MOVE WS-AC-TAX-COUNTRY TO RTX-TAX-COUNTRY.
           IF YTD-ROW-FOUND
               REWRITE TAXYTD-REC FROM RTX-TAX-YTD-REC
               IF WS-TAXYTD-STATUS NOT = '00'
                   MOVE 'TAXYTD' TO AB-DDNAME
                   MOVE WS-TAXYTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '6000-UPSERT-TAXYTD' TO AB-PARAGRAPH
                   MOVE TAXYTD-KEY TO AB-KEY
                   MOVE 'REWRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
               ADD 1 TO WS-YTD-UPDATED
               MOVE 'U' TO WS-TX-ACTION
           ELSE
               WRITE TAXYTD-REC FROM RTX-TAX-YTD-REC
               IF WS-TAXYTD-STATUS NOT = '00'
                   MOVE 'TAXYTD' TO AB-DDNAME
                   MOVE WS-TAXYTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '6000-UPSERT-TAXYTD' TO AB-PARAGRAPH
                   MOVE TAXYTD-KEY TO AB-KEY
                   MOVE 'WRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
               ADD 1 TO WS-YTD-INSERTED
               MOVE 'I' TO WS-TX-ACTION
           END-IF.
           ADD 1                TO WS-TXN-APPLIED.
           ADD WS-TX-AMOUNT-USD TO WS-APPLIED-USD.
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIRST ACTIVITY OF THE YEAR FOR ACCOUNT / FORM                  *
      *----------------------------------------------------------------*
       6100-NEW-YTD-ROW.
           MOVE SPACES           TO RTX-TAX-YTD-REC.
           MOVE WS-TX-ACCT       TO RTX-ACCT-NO.
           MOVE WS-TAX-YEAR      TO RTX-TAX-YEAR.
           MOVE WS-TX-FORM       TO RTX-FORM.
           MOVE ZERO TO RTX-ORD-DIVIDENDS     RTX-QUAL-DIVIDENDS
                        RTX-FOREIGN-TAX-PAID  RTX-FED-WITHHELD
                        RTX-GROSS-PROCEEDS    RTX-COST-BASIS
                        RTX-REALIZED-PL       RTX-CIL-PROCEEDS
                        RTX-MERGER-PROCEEDS   RTX-MARGIN-INT-PAID
                        RTX-1042-GROSS-INCOME RTX-1042-TAX-WITHHELD
                        RTX-TXN-COUNT.
       6100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ADD THE AMOUNT TO THE BOX(ES) OF THE CATEGORY                  *
      *----------------------------------------------------------------*
       6200-ACCUMULATE.
           EVALUATE WS-TX-CATEGORY
               WHEN 'ODIV'
                   ADD WS-TX-AMOUNT-USD TO RTX-ORD-DIVIDENDS
               WHEN 'QDIV'
                   ADD WS-TX-AMOUNT-USD TO RTX-ORD-DIVIDENDS
                   ADD WS-TX-AMOUNT-USD TO RTX-QUAL-DIVIDENDS
               WHEN 'FTAX'
                   ADD WS-TX-AMOUNT-USD TO RTX-FOREIGN-TAX-PAID
               WHEN '42GI'
                   ADD WS-TX-AMOUNT-USD TO RTX-1042-GROSS-INCOME
               WHEN '42TW'
                   ADD WS-TX-AMOUNT-USD TO RTX-1042-TAX-WITHHELD
               WHEN 'CILP'
                   ADD WS-TX-AMOUNT-USD TO RTX-CIL-PROCEEDS
                   ADD WS-TX-AMOUNT-USD TO RTX-GROSS-PROCEEDS
               WHEN 'MRGP'
                   ADD WS-TX-AMOUNT-USD TO RTX-MERGER-PROCEEDS
                   ADD WS-TX-AMOUNT-USD TO RTX-GROSS-PROCEEDS
               WHEN 'SALE'
                   ADD WS-TX-AMOUNT-USD TO RTX-GROSS-PROCEEDS
                   ADD WS-TX-PL-USD     TO RTX-REALIZED-PL
                   COMPUTE RTX-COST-BASIS = RTX-COST-BASIS
                         + WS-TX-AMOUNT-USD - WS-TX-PL-USD
               WHEN OTHER
                   MOVE 1008 TO AB-ABEND-CODE
                   MOVE '6200-ACCUMULATE' TO AB-PARAGRAPH
                   MOVE WS-TX-CATEGORY TO AB-KEY
                   MOVE 'UNKNOWN TAX CATEGORY' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM VARYING WS-CAT-SUB FROM 1 BY 1
                   UNTIL WS-CAT-SUB > 8
               IF WS-CAT-CODE (WS-CAT-SUB) = WS-TX-CATEGORY
                   ADD 1 TO WS-CAT-COUNT (WS-CAT-SUB)
                   ADD WS-TX-AMOUNT-USD TO WS-CAT-AMOUNT (WS-CAT-SUB)
               END-IF
           END-PERFORM.
       6200-EXIT.
           EXIT.
      *================================================================*
      * TAX ACTIVITY RECORD FOR RRR410                                 *
      *================================================================*
       7000-WRITE-TAX-ACTIVITY.
           MOVE SPACES            TO RTA-TAX-ACTIVITY-REC.
           MOVE DC-BUS-DATE       TO RTA-BUS-DATE.
           MOVE WS-TX-ACCT        TO RTA-ACCT-NO.
           MOVE WS-TAX-YEAR       TO RTA-TAX-YEAR.
           MOVE WS-TX-FORM        TO RTA-FORM.
           MOVE WS-TX-SOURCE      TO RTA-SOURCE.
           MOVE WS-TX-REF         TO RTA-REF.
           MOVE WS-TX-TYPE        TO RTA-ACT-TYPE.
           MOVE WS-TX-CUSIP       TO RTA-CUSIP.
           MOVE WS-TX-CATEGORY    TO RTA-CATEGORY.
           MOVE WS-TX-AMOUNT-USD  TO RTA-AMOUNT-USD.
           MOVE WS-TX-PL-USD      TO RTA-REALIZED-PL.
           MOVE WS-TX-AMOUNT      TO RTA-ORIG-AMOUNT.
           MOVE WS-TX-CCY         TO RTA-ORIG-CCY.
           MOVE WS-ISSUER-COUNTRY TO RTA-ISSUER-CTRY.
           MOVE WS-ISSUER-SEC-TYPE TO RTA-SEC-TYPE.
           MOVE WS-AC-TAX-STATUS  TO RTA-TAX-STATUS.
           MOVE WS-AC-TAX-COUNTRY TO RTA-TAX-COUNTRY.
           MOVE WS-TX-ACTION      TO RTA-UPSERT-ACTION.
           WRITE TAXACT-REC FROM RTA-TAX-ACTIVITY-REC.
           IF WS-TAXACT-STATUS NOT = '00'
               MOVE 'TAXACT' TO AB-DDNAME
               MOVE WS-TAXACT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '7000-WRITE-TAX-ACTIVITY' TO AB-PARAGRAPH
               MOVE WS-TX-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-TAXACT-OUT.
       7000-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-CA.
           READ CAACTV-FILE INTO ACT-ACTIVITY-REC.
           EVALUATE TRUE
               WHEN CAACTV-OK
                   IF ACT-CASH-CHANGE NOT NUMERIC
                       MOVE ZERO TO ACT-CASH-CHANGE
                   END-IF
               WHEN CAACTV-EOF
                   MOVE 'Y' TO WS-CA-EOF-SW
               WHEN OTHER
                   MOVE 'CAACTV' TO AB-DDNAME
                   MOVE WS-CAACTV-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-CA' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-JOURNAL.
      *----------------------------------------------------------------*
           READ JRNLIN-FILE INTO PSJ-JOURNAL-REC.
           EVALUATE TRUE
               WHEN JRNLIN-OK
                   CONTINUE
               WHEN JRNLIN-EOF
                   MOVE 'Y' TO WS-JRNL-EOF-SW
               WHEN OTHER
                   MOVE 'JRNLIN' TO AB-DDNAME
                   MOVE WS-JRNLIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-JOURNAL' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RRB400'       TO CT-STAGE.
           MOVE ZERO           TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '8500-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8500-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE CAACTV-FILE JRNLIN-FILE ACCTMAST-FILE.
           CLOSE TAXYTD-FILE.
           IF WS-TAXYTD-STATUS NOT = '00'
               MOVE 'TAXYTD' TO AB-DDNAME
               MOVE WS-TAXYTD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE TAXACT-FILE.
           IF WS-TAXACT-STATUS NOT = '00'
               MOVE 'TAXACT' TO AB-DDNAME
               MOVE WS-TAXACT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'TAX-TXN-IN'     TO CT-COUNTER-NAME.
           COMPUTE CT-COUNT = WS-CA-READ + WS-JRNL-READ.
           COMPUTE CT-AMOUNT = WS-CA-HASH + WS-JRNL-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'TAXYTD-UPD'     TO CT-COUNTER-NAME.
           MOVE WS-TXN-APPLIED   TO CT-COUNT.
           MOVE WS-APPLIED-USD   TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'TAXACT-OUT'     TO CT-COUNTER-NAME.
           MOVE WS-TAXACT-OUT    TO CT-COUNT.
           MOVE WS-APPLIED-USD   TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           PERFORM 9100-DISPLAY-STATISTICS THRU 9100-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE WS-TAX-YEAR    TO AU-KEY.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'TAX YTD ACCUMULATION ENDED WITH WARNINGS'
                               TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'TAX YTD ACCUMULATION ENDED' TO AU-MESSAGE
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9100-DISPLAY-STATISTICS.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'.
           DISPLAY '* RRB400 - TAX YEAR-TO-DATE ACCUMULATION       *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' TAX YEAR                 : ' WS-TAX-YEAR.
           MOVE WS-CA-READ TO WS-DISP-CNT.
           DISPLAY ' CA ACTIVITY READ         : ' WS-DISP-CNT.
           MOVE WS-CA-STREET TO WS-DISP-CNT.
           DISPLAY '   STREET / LEG 2         : ' WS-DISP-CNT.
           MOVE WS-CA-NOT-TAXABLE TO WS-DISP-CNT.
           DISPLAY '   NOT TAX REPORTABLE     : ' WS-DISP-CNT.
           MOVE WS-JRNL-READ TO WS-DISP-CNT.
           DISPLAY ' POSTING JOURNAL READ     : ' WS-DISP-CNT.
           MOVE WS-JRNL-STREET TO WS-DISP-CNT.
           DISPLAY '   STREET / LEG 2         : ' WS-DISP-CNT.
           MOVE WS-JRNL-NOT-SALE TO WS-DISP-CNT.
           DISPLAY '   NOT A SALE             : ' WS-DISP-CNT.
           MOVE WS-JRNL-XSL TO WS-DISP-CNT.
           DISPLAY '   SALE CXL NOT REVERSED  : ' WS-DISP-CNT.
           MOVE WS-EXEMPT-SKIPPED TO WS-DISP-CNT.
           DISPLAY ' EXEMPT HOLDERS SKIPPED   : ' WS-DISP-CNT.
           MOVE WS-PRIOR-YEAR-PAY TO WS-DISP-CNT.
           DISPLAY ' PRIOR-YEAR PAYMENTS      : ' WS-DISP-CNT.
           MOVE WS-STATUS-CHANGED TO WS-DISP-CNT.
           DISPLAY ' TAX STATUS CHANGED       : ' WS-DISP-CNT.
           MOVE WS-NO-STATUS TO WS-DISP-CNT.
           DISPLAY ' NO TAX STATUS            : ' WS-DISP-CNT.
           MOVE WS-ACCT-NOT-FOUND TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS NOT ON MASTER   : ' WS-DISP-CNT.
           MOVE WS-SEC-NOT-FOUND TO WS-DISP-CNT.
           DISPLAY ' SECURITIES NOT ON MASTER : ' WS-DISP-CNT.
           MOVE WS-FX-CALLS TO WS-DISP-CNT.
           DISPLAY ' FX CONVERSIONS           : ' WS-DISP-CNT.
           MOVE WS-FX-PROBLEMS TO WS-DISP-CNT.
           DISPLAY '   STALE / MISSING RATES  : ' WS-DISP-CNT.
           MOVE WS-TXN-APPLIED TO WS-DISP-CNT.
           DISPLAY ' TRANSACTIONS APPLIED     : ' WS-DISP-CNT.
           MOVE WS-YTD-INSERTED TO WS-DISP-CNT.
           DISPLAY '   YTD ROWS INSERTED      : ' WS-DISP-CNT.
           MOVE WS-YTD-UPDATED TO WS-DISP-CNT.
           DISPLAY '   YTD ROWS UPDATED       : ' WS-DISP-CNT.
           PERFORM VARYING WS-CAT-SUB FROM 1 BY 1
                   UNTIL WS-CAT-SUB > 8
               MOVE WS-CAT-COUNT (WS-CAT-SUB)  TO WS-DISP-CNT
               MOVE WS-CAT-AMOUNT (WS-CAT-SUB) TO WS-DISP-AMT
               DISPLAY '   ' WS-CAT-CODE (WS-CAT-SUB) '   '
                       WS-DISP-CNT ' ' WS-DISP-AMT
           END-PERFORM.
           MOVE WS-TAXACT-OUT TO WS-DISP-CNT.
           DISPLAY ' TAX ACTIVITY WRITTEN     : ' WS-DISP-CNT.
           MOVE WS-APPLIED-USD TO WS-DISP-AMT.
           DISPLAY ' AMOUNT APPLIED (USD)     : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
       9100-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RRB400 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RRB400 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RRB400 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
