       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCB120.
       AUTHOR.        T L MORRISON.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JUNE 1999.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCB120                                            *
      * DESCRIPTION: SETTLEMENT BANK STATEMENT (BAI2) PARSER.          *
      *              READS THE PREVIOUS DAY BAI VERSION 2 STATEMENT    *
      *              OF THE FIRM'S SETTLEMENT ACCOUNTS AND WRITES ONE  *
      *              NORMALIZED BANK TRANSACTION (RCCASHT) FOR EVERY   *
      *              16 (TRANSACTION DETAIL) RECORD FOR THE CASH       *
      *              RECONCILIATION (RCB300).                          *
      *              BAI2 RULES IMPLEMENTED:                           *
      *              - FIELDS ARE SEPARATED BY COMMAS, A RECORD ENDS   *
      *                WITH '/' (FIELDS AFTER THE '/' ARE DEFAULTED)   *
      *              - A RECORD MAY BE CONTINUED ON 88 RECORDS         *
      *              - AMOUNTS CARRY NO DECIMAL POINT: 2 IMPLIED       *
      *                DECIMALS, NONE FOR JPY                          *
      *              - DATES ARE YYMMDD (WINDOWED BY CMU010 W2Y4)      *
      *              - THE 16 RECORD TEXT FIELD RUNS TO THE END OF THE *
      *                RECORD AND MAY CONTAIN COMMAS                   *
      *              - FUNDS TYPE V/S/D CARRY EXTRA FIELDS             *
      *              - 49/98/99 CONTROL TOTALS AND RECORD COUNTS       *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD020 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - PARAMETER CARDS  (RCP120A)             *
      *              BAISTMT  - MSEC.PROD.RC.BAISTMT.RAW(0)   (RCBAI)  *
      * OUTPUT     : BANKTXN  - MSEC.PROD.RC.BANKTXN(+1)      (RCCASHT)*
      * CALLS      : CMU010 (W2Y4), CMU050, CMU060, CMU080             *
      * RETURN CODE: 0 CLEAN                                           *
      *              4 CONTROL TOTAL / RECORD COUNT DIFFERENCES,       *
      *                UNKNOWN TYPE CODES, DETAILS REJECTED            *
      *              U1005 AS-OF DATE NOT THE PREVIOUS BUSINESS DAY    *
      *              U1008 NO 01 FILE HEADER OR NO 99 FILE TRAILER     *
      *----------------------------------------------------------------*
      * PARAMETER CARDS (SYSIN, * IN COLUMN 1 = COMMENT):              *
      *   DATECHK=Y|N   AS-OF DATE MUST = PREVIOUS BUSINESS DAY        *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1999-06-14 TLM  ORIGINAL - BAI2 FROM THE SETTLEMENT   CHG05120 *
      *                 BANK REPLACES THE PRINTED STATEMENT            *
      * 1999-09-27 TLM  YYMMDD DATES WINDOWED VIA CMU010      CHG05390 *
      * 2002-03-11 KAP  FUNDS TYPE V (VALUE DATED)            CHG09944 *
      * 2003-10-06 KAP  EUR / GBP ACCOUNTS - JPY NO DECIMALS  CHG11702 *
      * 2006-02-20 KAP  88 CONTINUATION OF THE TEXT FIELD     CHG14870 *
      * 2008-05-12 SPA  FUNDS TYPE S AND D (DISTRIBUTED)      CHG17702 *
      * 2012-11-05 SPA  BANK CONVERSION - 49 TOTAL MAY        CHG23121 *
      *                 INCLUDE THE 03 SUMMARY AMOUNTS.  TOTALS        *
      *                 ARE A WARNING ONLY FROM NOW ON                 *
      * 2014-08-18 MHC  DATECHK PARAMETER FOR DR RECOVERY     CHG27715 *
      * 2019-02-25 MHC  CONTROL TOTALS NAMES PER CMB090       CHG33410 *
      * 2021-07-19 NVR  SIGNED AMOUNTS ON 16 (BANK NETTING)   CHG38804 *
      * 2022-10-03 NVR  SETTLEMENT ACCOUNT / CURRENCY CHECK   CHG39516 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT PARMCARD       ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
           SELECT BAISTMT-FILE   ASSIGN TO BAISTMT
                  FILE STATUS IS WS-BAISTMT-STATUS.
           SELECT BANKTXN-FILE   ASSIGN TO BANKTXN
                  FILE STATUS IS WS-BANKTXN-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
       FD  BAISTMT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RCBAI.
       FD  BANKTXN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BANKTXN-REC                 PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCB120'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-BAISTMT-STATUS       PIC X(02)  VALUE '00'.
               88  BAISTMT-OK                     VALUE '00'.
               88  BAISTMT-EOF                    VALUE '10'.
           05  WS-BANKTXN-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-BAI                     VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-PENDING-SW           PIC X(01)  VALUE 'N'.
               88  RECORD-PENDING                 VALUE 'Y'.
           05  WS-FILE-HDR-SW          PIC X(01)  VALUE 'N'.
               88  FILE-HEADER-SEEN               VALUE 'Y'.
           05  WS-FILE-TRL-SW          PIC X(01)  VALUE 'N'.
               88  FILE-TRAILER-SEEN              VALUE 'Y'.
           05  WS-GROUP-SW             PIC X(01)  VALUE 'N'.
               88  IN-GROUP                       VALUE 'Y'.
           05  WS-ACCOUNT-SW           PIC X(01)  VALUE 'N'.
               88  IN-ACCOUNT                     VALUE 'Y'.
           05  WS-DATECHK-SW           PIC X(01)  VALUE 'Y'.
               88  CHECK-ASOF-DATE                VALUE 'Y'.
           05  WS-END-OF-REC-SW        PIC X(01)  VALUE 'N'.
               88  END-OF-LOGICAL-REC             VALUE 'Y'.
           05  WS-AMT-OK-SW            PIC X(01)  VALUE 'Y'.
               88  AMOUNT-OK                      VALUE 'Y'.
               88  AMOUNT-BAD                     VALUE 'N'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-PARM-KEYWORD             PIC X(10).
       01  WS-PARM-VALUE               PIC X(20).
      *----------------------------------------------------------------*
      * LOGICAL RECORD = PHYSICAL RECORD + ITS 88 CONTINUATIONS        *
      *----------------------------------------------------------------*
       01  WS-LOGICAL-AREA.
           05  WS-LOG-CODE             PIC X(02).
               88  LOG-FILE-HEADER               VALUE '01'.
               88  LOG-GROUP-HEADER              VALUE '02'.
               88  LOG-ACCOUNT-ID                VALUE '03'.
               88  LOG-TXN-DETAIL                VALUE '16'.
               88  LOG-ACCOUNT-TRAILER           VALUE '49'.
               88  LOG-GROUP-TRAILER             VALUE '98'.
               88  LOG-FILE-TRAILER              VALUE '99'.
           05  WS-LOG-LEN              PIC S9(04) COMP.
           05  WS-LOG-PHYS             PIC S9(04) COMP.
           05  WS-LOG-REC              PIC X(1200).
       01  WS-LOG-MAX                  PIC S9(04) COMP  VALUE +1200.
       01  WS-LINE-LEN                 PIC S9(04) COMP.
       01  WS-CONT-START               PIC S9(04) COMP.
       01  WS-CONT-LEN                 PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * FIELD SCANNER (UNSTRING WITH POINTER OVER THE LOGICAL RECORD)  *
      *----------------------------------------------------------------*
       01  WS-SCAN-AREA.
           05  WS-PTR                  PIC S9(04) COMP.
           05  WS-FIELD                PIC X(80).
           05  WS-FIELD-LEN            PIC S9(04) COMP.
           05  WS-DELIM                PIC X(01).
           05  WS-TEXT-START           PIC S9(04) COMP.
           05  WS-TEXT-LEN             PIC S9(04) COMP.
       01  WS-FIELD-ARRAY.
           05  WS-FLD OCCURS 12 TIMES  PIC X(80).
       01  WS-FLD-CNT                  PIC S9(04) COMP.
       01  WS-FX                       PIC S9(04) COMP.
      *----------------------------------------------------------------*
      * AMOUNT CONVERSION - IMPLIED DECIMALS                           *
      *----------------------------------------------------------------*
       01  WS-AMT-AREA.
           05  WS-AMT-TEXT             PIC X(80).
           05  WS-AMT-SIGN             PIC X(01).
           05  WS-AMT-START            PIC S9(04) COMP.
           05  WS-AMT-DIGITS           PIC S9(04) COMP.
           05  WS-AMT-18               PIC X(18).
           05  WS-AMT-18-9 REDEFINES WS-AMT-18
                                       PIC 9(18).
           05  WS-AMT-RAW              PIC S9(18) COMP-3.
           05  WS-AMT-VALUE            PIC S9(15)V99 COMP-3.
           05  WS-IMPLIED-DEC          PIC 9(01).
       01  WS-CCY-DECIMALS-VALUES.
           05  FILLER  PIC X(04)  VALUE 'JPY0'.
           05  FILLER  PIC X(04)  VALUE 'KRW0'.
           05  FILLER  PIC X(04)  VALUE 'BHD3'.
           05  FILLER  PIC X(04)  VALUE 'KWD3'.
       01  WS-CCY-DECIMALS-TABLE REDEFINES WS-CCY-DECIMALS-VALUES.
           05  WS-CD-ENTRY OCCURS 4 TIMES INDEXED BY CD-IDX.
               10  WS-CD-CCY           PIC X(03).
               10  WS-CD-DEC           PIC 9(01).
      *----------------------------------------------------------------*
      * BAI2 TYPE CODES USED BY THE SETTLEMENT BANK                    *
      *----------------------------------------------------------------*
       01  WS-TYPE-CODE-VALUES.
           05  FILLER  PIC X(34)  VALUE
               '115CLOCKBOX DEPOSIT               '.
           05  FILLER  PIC X(34)  VALUE
               '142CACH CREDIT RECEIVED           '.
           05  FILLER  PIC X(34)  VALUE
               '165CPREAUTHORIZED ACH CREDIT      '.
           05  FILLER  PIC X(34)  VALUE
               '195CINCOMING MONEY TRANSFER       '.
           05  FILLER  PIC X(34)  VALUE
               '206CBOOK TRANSFER CREDIT          '.
           05  FILLER  PIC X(34)  VALUE
               '208CINCOMING INTL MONEY TRANSFER  '.
           05  FILLER  PIC X(34)  VALUE
               '275CZBA CREDIT                    '.
           05  FILLER  PIC X(34)  VALUE
               '301CCOMMERCIAL DEPOSIT            '.
           05  FILLER  PIC X(34)  VALUE
               '399CMISCELLANEOUS CREDIT          '.
           05  FILLER  PIC X(34)  VALUE
               '451DACH DEBIT RECEIVED            '.
           05  FILLER  PIC X(34)  VALUE
               '475DCHECK PAID                    '.
           05  FILLER  PIC X(34)  VALUE
               '495DOUTGOING MONEY TRANSFER       '.
           05  FILLER  PIC X(34)  VALUE
               '506DBOOK TRANSFER DEBIT           '.
           05  FILLER  PIC X(34)  VALUE
               '508DOUTGOING INTL MONEY TRANSFER  '.
           05  FILLER  PIC X(34)  VALUE
               '575DZBA DEBIT                     '.
           05  FILLER  PIC X(34)  VALUE
               '661DACCOUNT ANALYSIS FEE          '.
           05  FILLER  PIC X(34)  VALUE
               '698DMISCELLANEOUS FEES            '.
           05  FILLER  PIC X(34)  VALUE
               '699DMISCELLANEOUS DEBIT           '.
       01  WS-TYPE-CODE-TABLE REDEFINES WS-TYPE-CODE-VALUES.
           05  WS-TC-ENTRY OCCURS 18 TIMES
                   ASCENDING KEY IS WS-TC-CODE
                   INDEXED BY TC-IDX.
               10  WS-TC-CODE          PIC X(03).
               10  WS-TC-DR-CR         PIC X(01).
               10  WS-TC-DESC          PIC X(30).
      *----------------------------------------------------------------*
      * THE FIRM'S SETTLEMENT ACCOUNTS AT THE BANK, ONE PER CURRENCY   *
      *----------------------------------------------------------------*
       01  WS-SETL-ACCT-VALUES.
           05  FILLER  PIC X(15)  VALUE '400012345678USD'.
           05  FILLER  PIC X(15)  VALUE '400012345686EUR'.
           05  FILLER  PIC X(15)  VALUE '400012345694GBP'.
           05  FILLER  PIC X(15)  VALUE '400012345708JPY'.
           05  FILLER  PIC X(15)  VALUE '400012345716CAD'.
           05  FILLER  PIC X(15)  VALUE '400012345724CHF'.
       01  WS-SETL-ACCT-TABLE REDEFINES WS-SETL-ACCT-VALUES.
           05  WS-SA-ENTRY OCCURS 6 TIMES INDEXED BY SA-IDX.
               10  WS-SA-ACCOUNT       PIC X(12).
               10  WS-SA-CCY           PIC X(03).
       01  WS-TYPE-CODE-N              PIC 9(03).
       01  WS-TYPE-CODE-X REDEFINES WS-TYPE-CODE-N PIC X(03).
      *----------------------------------------------------------------*
      * CURRENT FILE / GROUP / ACCOUNT CONTEXT                         *
      *----------------------------------------------------------------*
       01  WS-CONTEXT.
           05  WS-FILE-SENDER          PIC X(20)  VALUE SPACES.
           05  WS-FILE-RECEIVER        PIC X(20)  VALUE SPACES.
           05  WS-FILE-ID              PIC X(10)  VALUE SPACES.
           05  WS-FILE-CREATE-DATE     PIC 9(08)  VALUE ZERO.
           05  WS-FILE-VERSION         PIC X(02)  VALUE SPACES.
           05  WS-GRP-ORIGINATOR       PIC X(09)  VALUE SPACES.
           05  WS-GRP-STATUS           PIC X(01)  VALUE SPACES.
           05  WS-GRP-ASOF-DATE        PIC 9(08)  VALUE ZERO.
           05  WS-GRP-CCY              PIC X(03)  VALUE SPACES.
           05  WS-ACCT-NUMBER          PIC X(12)  VALUE SPACES.
           05  WS-ACCT-CCY             PIC X(03)  VALUE SPACES.
           05  WS-FIRST-ASOF-DATE      PIC 9(08)  VALUE ZERO.
       01  WS-DATE-WORK.
           05  WS-YYMMDD-X             PIC X(06).
           05  WS-YYMMDD-9 REDEFINES WS-YYMMDD-X PIC 9(06).
           05  WS-DATE-OUT             PIC 9(08).
      *----------------------------------------------------------------*
      * 16 DETAIL WORK                                                 *
      *----------------------------------------------------------------*
       01  WS-DETAIL-WORK.
           05  WS-DTL-TYPE-CODE        PIC X(03).
           05  WS-DTL-FUNDS-TYPE       PIC X(01).
           05  WS-DTL-VALUE-DATE       PIC 9(08).
           05  WS-DTL-DR-CR            PIC X(01).
           05  WS-DTL-BANK-REF         PIC X(80).
           05  WS-DTL-CUST-REF         PIC X(80).
           05  WS-DTL-TEXT             PIC X(200).
           05  WS-DTL-DIST-CNT         PIC S9(04) COMP.
           05  WS-DTL-DIST-IX          PIC S9(04) COMP.
           05  WS-DTL-REJECT           PIC X(40).
      *----------------------------------------------------------------*
      * CONTROL TOTALS - RAW (UNSCALED) AMOUNTS AS IN THE FILE         *
      *----------------------------------------------------------------*
       01  WS-CONTROL-TOTALS.
           05  WS-ACCT-SUMM-RAW        PIC S9(18) COMP-3.
           05  WS-ACCT-DTL-RAW         PIC S9(18) COMP-3.
           05  WS-ACCT-RECS            PIC S9(09) COMP-3.
           05  WS-GRP-TOTAL-RAW        PIC S9(18) COMP-3.
           05  WS-GRP-ACCTS            PIC S9(09) COMP-3.
           05  WS-GRP-RECS             PIC S9(09) COMP-3.
           05  WS-FILE-TOTAL-RAW       PIC S9(18) COMP-3.
           05  WS-FILE-GROUPS          PIC S9(09) COMP-3.
           05  WS-FILE-RECS            PIC S9(09) COMP-3.
           05  WS-TRL-TOTAL-RAW        PIC S9(18) COMP-3.
           05  WS-TRL-COUNT            PIC S9(09) COMP-3.
           05  WS-TRL-COUNT-2          PIC S9(09) COMP-3.
           05  WS-ACCT-BOTH-RAW        PIC S9(18) COMP-3.
       01  WS-COUNTERS.
           05  WS-PHYS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LOGICAL-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CONT-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GROUP-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCOUNT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DETAIL-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DETAIL-REJ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TXN-OUT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CREDIT-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DEBIT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-TYPE         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-REC          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TOTAL-WARN           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-VALUE-DATED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRUNC-REF            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-ACCT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CREDIT-AMT           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-DEBIT-AMT            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-NET-AMT              PIC S9(15)V99 COMP-3 VALUE ZERO.
       COPY RCCASHT.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-RAW             PIC -ZZZZZZZZZZZZZZZZZ9.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-LINE THRU 2000-EXIT
               UNTIL END-OF-BAI.
           IF RECORD-PENDING
               PERFORM 3000-PROCESS-LOGICAL THRU 3000-EXIT
           END-IF.
           PERFORM 3900-END-OF-FILE-CHECKS THRU 3900-EXIT.
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
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'BAI2 BANK STATEMENT PARSE STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           INITIALIZE WS-CONTROL-TOTALS.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           OPEN INPUT BAISTMT-FILE.
           IF WS-BAISTMT-STATUS NOT = '00'
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE WS-BAISTMT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT BANKTXN-FILE.
           IF WS-BANKTXN-STATUS NOT = '00'
               MOVE 'BANKTXN' TO AB-DDNAME
               MOVE WS-BANKTXN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-BAI THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-READ-PARMS.
      *----------------------------------------------------------------*
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'RCB120 NO SYSIN PARAMETERS - DEFAULTS USED'
               GO TO 1100-EXIT
           END-IF.
           PERFORM 1110-READ-ONE-PARM THRU 1110-EXIT
               UNTIL END-OF-PARMS.
           CLOSE PARMCARD.
       1100-EXIT.
           EXIT.
       1110-READ-ONE-PARM.
           READ PARMCARD.
           IF PARMCARD-EOF
               MOVE 'Y' TO WS-PARM-EOF-SW
               GO TO 1110-EXIT
           END-IF.
           IF NOT PARMCARD-OK
               MOVE 'SYSIN' TO AB-DDNAME
               MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '1110-READ-ONE-PARM' TO AB-PARAGRAPH
               MOVE 'READ FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF PARM-CARD-REC (1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1110-EXIT
           END-IF.
           DISPLAY 'RCB120 PARM: ' PARM-CARD-REC (1:60).
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING.
           IF WS-PARM-KEYWORD = 'DATECHK'
               IF WS-PARM-VALUE (1:1) = 'N'
                   MOVE 'N' TO WS-DATECHK-SW
                   MOVE 'WRIT'  TO AU-FUNCTION
                   MOVE 'PARMOVR' TO AU-EVENT
                   MOVE 'W'     TO AU-SEVERITY
                   MOVE 'DATECHK=N' TO AU-KEY
                   MOVE 'AS-OF DATE CHECK SWITCHED OFF BY SYSIN'
                                TO AU-MESSAGE
                   CALL 'CMU060' USING AU-AUDIT-PARMS
               END-IF
           ELSE
               DISPLAY 'RCB120 UNKNOWN PARAMETER IGNORED: '
                       WS-PARM-KEYWORD
           END-IF.
       1110-EXIT.
           EXIT.
      *================================================================*
      * ONE PHYSICAL LINE.  AN 88 LINE IS ADDED TO THE PENDING LOGICAL *
      * RECORD, ANY OTHER LINE FIRST PROCESSES THE PENDING RECORD.     *
      *================================================================*
       2000-PROCESS-LINE.
           ADD 1 TO WS-PHYS-READ.
           PERFORM 2050-LINE-LENGTH THRU 2050-EXIT.
           IF BAI-CONTINUATION
               IF NOT RECORD-PENDING
                   DISPLAY 'RCB120 88 CONTINUATION WITHOUT A RECORD AT '
                           'LINE ' WS-PHYS-READ ' - IGNORED'
                   ADD 1 TO WS-UNKNOWN-REC
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               ELSE
                   PERFORM 2200-ADD-CONTINUATION THRU 2200-EXIT
               END-IF
           ELSE
               IF RECORD-PENDING
                   PERFORM 3000-PROCESS-LOGICAL THRU 3000-EXIT
               END-IF
               PERFORM 2100-START-LOGICAL THRU 2100-EXIT
           END-IF.
           PERFORM 8000-READ-BAI THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SIGNIFICANT LENGTH OF THE 80 BYTE LINE (TRAILING BLANKS OFF)   *
      *----------------------------------------------------------------*
       2050-LINE-LENGTH.
           MOVE 80 TO WS-LINE-LEN.
           PERFORM UNTIL WS-LINE-LEN < 1
                      OR BAI-LINE-REC (WS-LINE-LEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-LINE-LEN
           END-PERFORM.
       2050-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-START-LOGICAL.
      *----------------------------------------------------------------*
           MOVE SPACES       TO WS-LOG-REC.
           MOVE BAI-REC-CODE TO WS-LOG-CODE.
           MOVE ZERO         TO WS-LOG-LEN.
           MOVE 1            TO WS-LOG-PHYS.
           IF WS-LINE-LEN > ZERO
               MOVE BAI-LINE-REC (1:WS-LINE-LEN)
                                TO WS-LOG-REC (1:WS-LINE-LEN)
               MOVE WS-LINE-LEN TO WS-LOG-LEN
           END-IF.
           MOVE 'Y' TO WS-PENDING-SW.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 88 CONTINUATION.  THE '88,' IS DROPPED.  WHEN THE RECORD SO    *
      * FAR ENDS WITH '/' THE SLASH IS REMOVED AND THE FIELDS CONTINUE *
      * AFTER A COMMA.  A 16 RECORD CONTINUES ITS TEXT FIELD, SO THE   *
      * PIECES ARE JOINED WITH ONE BLANK.                 (CHG14870)   *
      *----------------------------------------------------------------*
       2200-ADD-CONTINUATION.
           ADD 1 TO WS-CONT-CNT.
           ADD 1 TO WS-LOG-PHYS.
           IF WS-LINE-LEN < 4
               GO TO 2200-EXIT
           END-IF.
           MOVE 4 TO WS-CONT-START.
           COMPUTE WS-CONT-LEN = WS-LINE-LEN - 3.
           IF WS-LOG-LEN > ZERO
           AND WS-LOG-REC (WS-LOG-LEN:1) = '/'
               SUBTRACT 1 FROM WS-LOG-LEN
               IF NOT LOG-TXN-DETAIL
                   ADD 1 TO WS-LOG-LEN
                   MOVE ',' TO WS-LOG-REC (WS-LOG-LEN:1)
               END-IF
           END-IF.
           IF LOG-TXN-DETAIL
               IF WS-LOG-REC (WS-LOG-LEN:1) NOT = ','
                   ADD 1 TO WS-LOG-LEN
                   MOVE SPACE TO WS-LOG-REC (WS-LOG-LEN:1)
               END-IF
           ELSE
               IF WS-LOG-REC (WS-LOG-LEN:1) NOT = ','
                   ADD 1 TO WS-LOG-LEN
                   MOVE ',' TO WS-LOG-REC (WS-LOG-LEN:1)
               END-IF
           END-IF.
           IF WS-LOG-LEN + WS-CONT-LEN > WS-LOG-MAX
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1007 TO AB-ABEND-CODE
               MOVE '2200-ADD-CONTINUATION' TO AB-PARAGRAPH
               MOVE WS-PHYS-READ TO WS-DISP-CNT
               MOVE WS-DISP-CNT TO AB-KEY
               MOVE 'BAI LOGICAL RECORD LONGER THAN 1200 BYTES'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE BAI-LINE-REC (WS-CONT-START:WS-CONT-LEN)
             TO WS-LOG-REC (WS-LOG-LEN + 1:WS-CONT-LEN).
           ADD WS-CONT-LEN TO WS-LOG-LEN.
       2200-EXIT.
           EXIT.
      *================================================================*
      * ONE COMPLETE LOGICAL RECORD                                    *
      *================================================================*
       3000-PROCESS-LOGICAL.
           ADD 1 TO WS-LOGICAL-CNT.
           MOVE 'N' TO WS-PENDING-SW.
           ADD WS-LOG-PHYS TO WS-FILE-RECS.
           IF IN-GROUP
               ADD WS-LOG-PHYS TO WS-GRP-RECS
           END-IF.
           IF IN-ACCOUNT
               ADD WS-LOG-PHYS TO WS-ACCT-RECS
           END-IF.
      *    FIELD 1 IS THE RECORD CODE - SKIPPED.  WS-FIELD THEN HOLDS
      *    THE FIRST DATA FIELD WHEN THE RECORD ROUTINE GETS CONTROL.
           MOVE 1 TO WS-PTR.
           MOVE 'N' TO WS-END-OF-REC-SW.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           EVALUATE TRUE
               WHEN LOG-FILE-HEADER
                   PERFORM 3100-FILE-HEADER THRU 3100-EXIT
               WHEN LOG-GROUP-HEADER
                   PERFORM 3200-GROUP-HEADER THRU 3200-EXIT
               WHEN LOG-ACCOUNT-ID
                   PERFORM 3300-ACCOUNT-ID THRU 3300-EXIT
               WHEN LOG-TXN-DETAIL
                   PERFORM 3400-TXN-DETAIL THRU 3400-EXIT
               WHEN LOG-ACCOUNT-TRAILER
                   PERFORM 3500-ACCOUNT-TRAILER THRU 3500-EXIT
               WHEN LOG-GROUP-TRAILER
                   PERFORM 3600-GROUP-TRAILER THRU 3600-EXIT
               WHEN LOG-FILE-TRAILER
                   PERFORM 3700-FILE-TRAILER THRU 3700-EXIT
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-REC
                   DISPLAY 'RCB120 UNKNOWN BAI RECORD CODE ' WS-LOG-CODE
                           ' - IGNORED'
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 01 SENDER,RECEIVER,CREATE DATE,CREATE TIME,FILE ID,REC LEN,    *
      *    BLOCK SIZE,VERSION/                                         *
      *----------------------------------------------------------------*
       3100-FILE-HEADER.
           IF FILE-HEADER-SEEN
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3100-FILE-HEADER' TO AB-PARAGRAPH
               MOVE 'SECOND 01 FILE HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'Y' TO WS-FILE-HDR-SW.
           MOVE 1 TO WS-FILE-RECS.
           PERFORM 7100-SPLIT-FIELDS THRU 7100-EXIT.
           MOVE WS-FLD (1) TO WS-FILE-SENDER.
           MOVE WS-FLD (2) TO WS-FILE-RECEIVER.
           MOVE WS-FLD (3) TO WS-YYMMDD-X.
           PERFORM 7300-WINDOW-DATE THRU 7300-EXIT.
           MOVE WS-DATE-OUT TO WS-FILE-CREATE-DATE.
           MOVE WS-FLD (5) TO WS-FILE-ID.
           MOVE WS-FLD (8) TO WS-FILE-VERSION.
           DISPLAY 'RCB120 BAI FILE FROM ' WS-FILE-SENDER (1:12)
                   ' TO ' WS-FILE-RECEIVER (1:12) ' ID ' WS-FILE-ID
                   ' CREATED ' WS-FILE-CREATE-DATE
                   ' VERSION ' WS-FILE-VERSION.
           IF WS-FILE-VERSION NOT = '2'
               DISPLAY 'RCB120 WARNING - BAI VERSION IS NOT 2'
           END-IF.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 02 ULTIMATE RECEIVER,ORIGINATOR,GROUP STATUS,AS-OF DATE,       *
      *    AS-OF TIME,CURRENCY,AS-OF DATE MODIFIER/                    *
      *----------------------------------------------------------------*
       3200-GROUP-HEADER.
           IF NOT FILE-HEADER-SEEN
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3200-GROUP-HEADER' TO AB-PARAGRAPH
               MOVE 'GROUP HEADER BEFORE THE 01 FILE HEADER'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-GROUP-CNT.
           MOVE 'Y' TO WS-GROUP-SW.
           MOVE WS-LOG-PHYS TO WS-GRP-RECS.
           MOVE ZERO TO WS-GRP-TOTAL-RAW WS-GRP-ACCTS.
           PERFORM 7100-SPLIT-FIELDS THRU 7100-EXIT.
           MOVE WS-FLD (2) TO WS-GRP-ORIGINATOR.
           MOVE WS-FLD (3) TO WS-GRP-STATUS.
           MOVE WS-FLD (4) TO WS-YYMMDD-X.
           PERFORM 7300-WINDOW-DATE THRU 7300-EXIT.
           MOVE WS-DATE-OUT TO WS-GRP-ASOF-DATE.
           MOVE WS-FLD (6) TO WS-GRP-CCY.
           IF WS-GRP-CCY = SPACES
               MOVE 'USD' TO WS-GRP-CCY
           END-IF.
           IF WS-FIRST-ASOF-DATE = ZERO
               MOVE WS-GRP-ASOF-DATE TO WS-FIRST-ASOF-DATE
           END-IF.
           MOVE WS-GROUP-CNT TO WS-DISP-CNT.
           DISPLAY 'RCB120 GROUP ' WS-DISP-CNT (9:3) ' BANK '
                   WS-GRP-ORIGINATOR ' AS OF ' WS-GRP-ASOF-DATE
                   ' STATUS ' WS-GRP-STATUS ' CCY ' WS-GRP-CCY.
      *    GROUP STATUS 1 = UPDATE, 2 = DELETE, 3 = CORRECTION,
      *    4 = TEST.  ONLY 1 IS EXPECTED FROM THE BANK.
           IF WS-GRP-STATUS NOT = '1'
               DISPLAY 'RCB120 WARNING - GROUP STATUS ' WS-GRP-STATUS
                       ' PROCESSED AS AN UPDATE'
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           IF CHECK-ASOF-DATE
           AND WS-GRP-ASOF-DATE NOT = DC-PREV-BUS-DATE
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '3200-GROUP-HEADER' TO AB-PARAGRAPH
               STRING 'AS OF ' WS-GRP-ASOF-DATE ' EXPECTED '
                      DC-PREV-BUS-DATE
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'BAI AS-OF DATE NOT PREVIOUS BUSINESS DAY'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 03 ACCOUNT NUMBER,CURRENCY,TYPE,AMOUNT,ITEM COUNT,FUNDS TYPE,  *
      *    (TYPE,AMOUNT,ITEM COUNT,FUNDS TYPE)... /                    *
      * THE SUMMARY AMOUNTS ARE KEPT FOR THE 49 CONTROL TOTAL ONLY.    *
      *----------------------------------------------------------------*
       3300-ACCOUNT-ID.
           IF NOT IN-GROUP
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3300-ACCOUNT-ID' TO AB-PARAGRAPH
               MOVE 'ACCOUNT RECORD OUTSIDE A GROUP' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-ACCOUNT-CNT WS-GRP-ACCTS.
           MOVE 'Y' TO WS-ACCOUNT-SW.
           MOVE WS-LOG-PHYS TO WS-ACCT-RECS.
           MOVE ZERO TO WS-ACCT-SUMM-RAW WS-ACCT-DTL-RAW.
           MOVE WS-FIELD TO WS-ACCT-NUMBER.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE WS-FIELD TO WS-ACCT-CCY.
           IF WS-ACCT-CCY = SPACES
               MOVE WS-GRP-CCY TO WS-ACCT-CCY
           END-IF.
           PERFORM 7400-IMPLIED-DECIMALS THRU 7400-EXIT.
           PERFORM UNTIL END-OF-LOGICAL-REC
      *        TYPE CODE
               PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
               IF NOT END-OF-LOGICAL-REC OR WS-FIELD-LEN > ZERO
      *            AMOUNT
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   IF WS-FIELD-LEN > ZERO
                       MOVE WS-FIELD TO WS-AMT-TEXT
                       PERFORM 7200-CONVERT-AMOUNT THRU 7200-EXIT
                       IF AMOUNT-OK
                           ADD WS-AMT-RAW TO WS-ACCT-SUMM-RAW
                       END-IF
                   END-IF
      *            ITEM COUNT, FUNDS TYPE
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
               END-IF
           END-PERFORM.
           DISPLAY 'RCB120   ACCOUNT ' WS-ACCT-NUMBER ' CCY '
                   WS-ACCT-CCY ' IMPLIED DECIMALS ' WS-IMPLIED-DEC.
           PERFORM 3350-CHECK-SETTLEMENT-ACCT THRU 3350-EXIT.
       3300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CHG39516 - AN ACCOUNT THAT IS NOT ONE OF OURS, OR IN ANOTHER   *
      * CURRENCY, IS STILL LOADED (THE BANK MAY HAVE OPENED IT) BUT    *
      * TREASURY MUST CONFIRM IT.                                      *
      *----------------------------------------------------------------*
       3350-CHECK-SETTLEMENT-ACCT.
           SET SA-IDX TO 1.
           SEARCH WS-SA-ENTRY
               AT END
                   ADD 1 TO WS-UNKNOWN-ACCT
                   DISPLAY 'RCB120 WARNING - ACCOUNT ' WS-ACCT-NUMBER
                           ' IS NOT A MERIDIAN SETTLEMENT ACCOUNT'
                   MOVE 'WRIT'          TO AU-FUNCTION
                   MOVE 'BAIACCT'       TO AU-EVENT
                   MOVE 'W'             TO AU-SEVERITY
                   MOVE WS-ACCT-NUMBER  TO AU-KEY
                   MOVE 'BAI2 ACCOUNT NOT IN THE SETTLEMENT ACCT TABLE'
                                        TO AU-MESSAGE
                   CALL 'CMU060' USING AU-AUDIT-PARMS
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
               WHEN WS-SA-ACCOUNT (SA-IDX) = WS-ACCT-NUMBER
                   IF WS-SA-CCY (SA-IDX) NOT = WS-ACCT-CCY
                       ADD 1 TO WS-UNKNOWN-ACCT
                       DISPLAY 'RCB120 WARNING - ACCOUNT '
                               WS-ACCT-NUMBER ' IS A '
                               WS-SA-CCY (SA-IDX) ' ACCOUNT, FILE SAYS '
                               WS-ACCT-CCY
                       IF WS-RETURN-CODE < 4
                           MOVE 4 TO WS-RETURN-CODE
                       END-IF
                   END-IF
           END-SEARCH.
       3350-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 16 TYPE CODE,AMOUNT,FUNDS TYPE,[FUNDS FIELDS],BANK REF,        *
      *    CUSTOMER REF,TEXT                                           *
      *----------------------------------------------------------------*
       3400-TXN-DETAIL.
           ADD 1 TO WS-DETAIL-CNT.
           MOVE SPACES TO WS-DTL-REJECT.
           IF NOT IN-ACCOUNT
               DISPLAY 'RCB120 16 RECORD OUTSIDE AN ACCOUNT - LINE '
                       WS-PHYS-READ ' REJECTED'
               ADD 1 TO WS-DETAIL-REJ
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               GO TO 3400-EXIT
           END-IF.
      *    TYPE CODE - FIRST DATA FIELD, SCANNED BY 3000
           MOVE WS-FIELD (1:3) TO WS-DTL-TYPE-CODE.
      *    AMOUNT
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE WS-FIELD TO WS-AMT-TEXT.
           PERFORM 7200-CONVERT-AMOUNT THRU 7200-EXIT.
           IF AMOUNT-BAD
               MOVE 'AMOUNT NOT NUMERIC' TO WS-DTL-REJECT
           ELSE
               ADD WS-AMT-RAW TO WS-ACCT-DTL-RAW
           END-IF.
      *    FUNDS TYPE AND ITS EXTRA FIELDS
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE WS-FIELD (1:1) TO WS-DTL-FUNDS-TYPE.
           MOVE WS-GRP-ASOF-DATE TO WS-DTL-VALUE-DATE.
           PERFORM 3450-FUNDS-TYPE-FIELDS THRU 3450-EXIT.
      *    BANK REFERENCE, CUSTOMER REFERENCE
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE WS-FIELD TO WS-DTL-BANK-REF.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE WS-FIELD TO WS-DTL-CUST-REF.
           IF WS-FIELD-LEN > 16
               ADD 1 TO WS-TRUNC-REF
               DISPLAY 'RCB120 CUSTOMER REF LONGER THAN 16 - TRUNCATED '
                       WS-FIELD (1:30)
           END-IF.
      *    TEXT - REST OF THE LOGICAL RECORD, COMMAS INCLUDED
           PERFORM 3460-TEXT-FIELD THRU 3460-EXIT.
           PERFORM 3470-TYPE-CODE-LOOKUP THRU 3470-EXIT.
           IF WS-DTL-REJECT NOT = SPACES
               ADD 1 TO WS-DETAIL-REJ
               DISPLAY 'RCB120 DETAIL REJECTED LINE ' WS-PHYS-READ
                       ' TYPE ' WS-DTL-TYPE-CODE ' - ' WS-DTL-REJECT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               GO TO 3400-EXIT
           END-IF.
           PERFORM 3480-BUILD-BANK-TXN THRU 3480-EXIT.
           PERFORM 8200-WRITE-BANKTXN THRU 8200-EXIT.
       3400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FUNDS TYPE 0 1 2 Z : NO FIELDS       V : VALUE DATE, TIME      *
      *            S : IMMEDIATE, ONE DAY, TWO+ DAY AMOUNTS            *
      *            D : NUMBER OF DISTRIBUTIONS, THEN DAYS,AMOUNT PAIRS *
      *----------------------------------------------------------------*
       3450-FUNDS-TYPE-FIELDS.
           EVALUATE WS-DTL-FUNDS-TYPE
               WHEN '0'
               WHEN '1'
               WHEN '2'
               WHEN 'Z'
               WHEN SPACE
                   CONTINUE
               WHEN 'V'
                   ADD 1 TO WS-VALUE-DATED
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   IF WS-FIELD-LEN = 6
                       MOVE WS-FIELD (1:6) TO WS-YYMMDD-X
                       PERFORM 7300-WINDOW-DATE THRU 7300-EXIT
                       IF WS-DATE-OUT > ZERO
                           MOVE WS-DATE-OUT TO WS-DTL-VALUE-DATE
                       END-IF
                   END-IF
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
               WHEN 'S'
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
               WHEN 'D'
                   PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   MOVE ZERO TO WS-DTL-DIST-CNT
                   IF WS-FIELD-LEN > ZERO AND WS-FIELD-LEN < 3
                   AND WS-FIELD (1:WS-FIELD-LEN) NUMERIC
                       MOVE WS-FIELD (1:WS-FIELD-LEN)
                                        TO WS-DTL-DIST-CNT
                   END-IF
                   PERFORM VARYING WS-DTL-DIST-IX FROM 1 BY 1
                           UNTIL WS-DTL-DIST-IX > WS-DTL-DIST-CNT
                       PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                       PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
                   END-PERFORM
               WHEN OTHER
                   DISPLAY 'RCB120 UNKNOWN FUNDS TYPE '''
                           WS-DTL-FUNDS-TYPE ''' LINE ' WS-PHYS-READ
                           ' - NO FUNDS FIELDS ASSUMED'
           END-EVALUATE.
       3450-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3460-TEXT-FIELD.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-DTL-TEXT.
           IF END-OF-LOGICAL-REC
               GO TO 3460-EXIT
           END-IF.
           MOVE WS-PTR TO WS-TEXT-START.
           COMPUTE WS-TEXT-LEN = WS-LOG-LEN - WS-TEXT-START + 1.
           IF WS-TEXT-LEN > ZERO
      *        A TRAILING SLASH IS NOT PART OF THE TEXT
               IF WS-LOG-REC (WS-LOG-LEN:1) = '/'
                   SUBTRACT 1 FROM WS-TEXT-LEN
               END-IF
           END-IF.
           IF WS-TEXT-LEN > 200
               MOVE 200 TO WS-TEXT-LEN
           END-IF.
           IF WS-TEXT-LEN > ZERO
               MOVE WS-LOG-REC (WS-TEXT-START:WS-TEXT-LEN)
                                TO WS-DTL-TEXT
           END-IF.
       3460-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * DEBIT OR CREDIT FROM THE TYPE CODE.  CODES NOT IN THE TABLE:   *
      * BAI RANGES 100-399 CREDIT, 400-699 DEBIT, 900-959 CREDIT AND   *
      * 960-999 DEBIT (BANK SPECIFIC).  A SIGNED AMOUNT REVERSES IT.   *
      *----------------------------------------------------------------*
       3470-TYPE-CODE-LOOKUP.
           MOVE SPACE TO WS-DTL-DR-CR.
           SEARCH ALL WS-TC-ENTRY
               AT END
                   CONTINUE
               WHEN WS-TC-CODE (TC-IDX) = WS-DTL-TYPE-CODE
                   MOVE WS-TC-DR-CR (TC-IDX) TO WS-DTL-DR-CR
           END-SEARCH.
           IF WS-DTL-DR-CR = SPACE
               IF WS-DTL-TYPE-CODE NUMERIC
                   MOVE WS-DTL-TYPE-CODE TO WS-TYPE-CODE-X
                   ADD 1 TO WS-UNKNOWN-TYPE
                   EVALUATE TRUE
                       WHEN WS-TYPE-CODE-N >= 100 AND <= 399
                           MOVE 'C' TO WS-DTL-DR-CR
                       WHEN WS-TYPE-CODE-N >= 400 AND <= 699
                           MOVE 'D' TO WS-DTL-DR-CR
                       WHEN WS-TYPE-CODE-N >= 900 AND <= 959
                           MOVE 'C' TO WS-DTL-DR-CR
                       WHEN WS-TYPE-CODE-N >= 960 AND <= 999
                           MOVE 'D' TO WS-DTL-DR-CR
                       WHEN OTHER
                           MOVE 'TYPE CODE NOT A TRANSACTION CODE'
                                            TO WS-DTL-REJECT
                   END-EVALUATE
                   IF WS-DTL-DR-CR NOT = SPACE
                       DISPLAY 'RCB120 TYPE CODE ' WS-DTL-TYPE-CODE
                               ' NOT IN TABLE - TAKEN AS '
                               WS-DTL-DR-CR
                   END-IF
               ELSE
                   MOVE 'TYPE CODE NOT NUMERIC' TO WS-DTL-REJECT
               END-IF
           END-IF.
           IF WS-AMT-SIGN = '-' AND WS-DTL-DR-CR NOT = SPACE
               IF WS-DTL-DR-CR = 'C'
                   MOVE 'D' TO WS-DTL-DR-CR
               ELSE
                   MOVE 'C' TO WS-DTL-DR-CR
               END-IF
           END-IF.
       3470-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3480-BUILD-BANK-TXN.
      *----------------------------------------------------------------*
           MOVE SPACES             TO RCT-BANK-TXN-REC.
           MOVE WS-GRP-ORIGINATOR  TO RCT-BANK-ID.
           MOVE WS-ACCT-NUMBER     TO RCT-ACCOUNT.
           MOVE WS-ACCT-CCY        TO RCT-CCY.
           MOVE WS-GRP-ASOF-DATE   TO RCT-AS-OF-DATE.
           MOVE WS-DTL-VALUE-DATE  TO RCT-VALUE-DATE.
           MOVE WS-DTL-TYPE-CODE   TO RCT-TYPE-CODE.
           MOVE WS-DTL-DR-CR       TO RCT-DR-CR.
           MOVE WS-AMT-VALUE       TO RCT-AMOUNT.
           MOVE WS-DTL-BANK-REF    TO RCT-BANK-REF.
           MOVE WS-DTL-CUST-REF    TO RCT-CUST-REF.
           MOVE WS-DTL-TEXT        TO RCT-TEXT.
           ADD 1 TO WS-TXN-OUT.
           MOVE WS-TXN-OUT         TO RCT-SEQ-NO.
           IF RCT-CREDIT
               ADD 1 TO WS-CREDIT-CNT
               ADD WS-AMT-VALUE TO WS-CREDIT-AMT
           ELSE
               ADD 1 TO WS-DEBIT-CNT
               ADD WS-AMT-VALUE TO WS-DEBIT-AMT
           END-IF.
       3480-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 49 ACCOUNT CONTROL TOTAL,NUMBER OF RECORDS/                    *
      * CHG23121 - SINCE THE BANK CONVERSION THE TOTAL MAY OR MAY NOT  *
      * INCLUDE THE 03 SUMMARY AMOUNTS.  EITHER IS ACCEPTED.           *
      *----------------------------------------------------------------*
       3500-ACCOUNT-TRAILER.
           IF NOT IN-ACCOUNT
               DISPLAY 'RCB120 49 WITHOUT AN 03 RECORD - IGNORED'
               ADD 1 TO WS-UNKNOWN-REC
               GO TO 3500-EXIT
           END-IF.
           MOVE WS-FIELD TO WS-AMT-TEXT.
           PERFORM 7200-CONVERT-AMOUNT THRU 7200-EXIT.
           MOVE WS-AMT-RAW TO WS-TRL-TOTAL-RAW.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE ZERO TO WS-TRL-COUNT.
           IF WS-FIELD-LEN > ZERO AND WS-FIELD-LEN < 10
           AND WS-FIELD (1:WS-FIELD-LEN) NUMERIC
               MOVE WS-FIELD (1:WS-FIELD-LEN) TO WS-TRL-COUNT
           END-IF.
           COMPUTE WS-ACCT-BOTH-RAW = WS-ACCT-SUMM-RAW
                                    + WS-ACCT-DTL-RAW.
           IF WS-TRL-TOTAL-RAW NOT = WS-ACCT-DTL-RAW
           AND WS-TRL-TOTAL-RAW NOT = WS-ACCT-BOTH-RAW
               MOVE WS-TRL-TOTAL-RAW TO WS-DISP-RAW
               DISPLAY 'RCB120 *** ACCOUNT ' WS-ACCT-NUMBER
                       ' 49 CONTROL TOTAL ' WS-DISP-RAW
               MOVE WS-ACCT-BOTH-RAW TO WS-DISP-RAW
               DISPLAY 'RCB120 *** COMPUTED (03 + 16)   ' WS-DISP-RAW
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
           IF WS-TRL-COUNT NOT = WS-ACCT-RECS
               MOVE WS-TRL-COUNT TO WS-DISP-CNT
               DISPLAY 'RCB120 *** ACCOUNT ' WS-ACCT-NUMBER
                       ' 49 RECORD COUNT ' WS-DISP-CNT
               MOVE WS-ACCT-RECS TO WS-DISP-CNT
               DISPLAY 'RCB120 *** RECORDS COUNTED        ' WS-DISP-CNT
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
           ADD WS-TRL-TOTAL-RAW TO WS-GRP-TOTAL-RAW.
           MOVE 'N' TO WS-ACCOUNT-SW.
       3500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 98 GROUP CONTROL TOTAL,NUMBER OF ACCOUNTS,NUMBER OF RECORDS/   *
      *----------------------------------------------------------------*
       3600-GROUP-TRAILER.
           IF NOT IN-GROUP
               DISPLAY 'RCB120 98 WITHOUT AN 02 RECORD - IGNORED'
               ADD 1 TO WS-UNKNOWN-REC
               GO TO 3600-EXIT
           END-IF.
           MOVE WS-FIELD TO WS-AMT-TEXT.
           PERFORM 7200-CONVERT-AMOUNT THRU 7200-EXIT.
           MOVE WS-AMT-RAW TO WS-TRL-TOTAL-RAW.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE ZERO TO WS-TRL-COUNT WS-TRL-COUNT-2.
           IF WS-FIELD-LEN > ZERO AND WS-FIELD-LEN < 10
           AND WS-FIELD (1:WS-FIELD-LEN) NUMERIC
               MOVE WS-FIELD (1:WS-FIELD-LEN) TO WS-TRL-COUNT
           END-IF.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           IF WS-FIELD-LEN > ZERO AND WS-FIELD-LEN < 10
           AND WS-FIELD (1:WS-FIELD-LEN) NUMERIC
               MOVE WS-FIELD (1:WS-FIELD-LEN) TO WS-TRL-COUNT-2
           END-IF.
           IF WS-TRL-TOTAL-RAW NOT = WS-GRP-TOTAL-RAW
               MOVE WS-TRL-TOTAL-RAW TO WS-DISP-RAW
               DISPLAY 'RCB120 *** 98 GROUP CONTROL TOTAL ' WS-DISP-RAW
               MOVE WS-GRP-TOTAL-RAW TO WS-DISP-RAW
               DISPLAY 'RCB120 *** SUM OF 49 TOTALS       ' WS-DISP-RAW
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
           IF WS-TRL-COUNT NOT = WS-GRP-ACCTS
           OR WS-TRL-COUNT-2 NOT = WS-GRP-RECS
               DISPLAY 'RCB120 *** 98 ACCOUNTS/RECORDS ' WS-TRL-COUNT
                       '/' WS-TRL-COUNT-2 ' COUNTED ' WS-GRP-ACCTS
                       '/' WS-GRP-RECS
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
           ADD WS-TRL-TOTAL-RAW TO WS-FILE-TOTAL-RAW.
           ADD 1 TO WS-FILE-GROUPS.
           MOVE 'N' TO WS-GROUP-SW.
       3600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 99 FILE CONTROL TOTAL,NUMBER OF GROUPS,NUMBER OF RECORDS/      *
      *----------------------------------------------------------------*
       3700-FILE-TRAILER.
           MOVE 'Y' TO WS-FILE-TRL-SW.
           MOVE WS-FIELD TO WS-AMT-TEXT.
           PERFORM 7200-CONVERT-AMOUNT THRU 7200-EXIT.
           MOVE WS-AMT-RAW TO WS-TRL-TOTAL-RAW.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           MOVE ZERO TO WS-TRL-COUNT WS-TRL-COUNT-2.
           IF WS-FIELD-LEN > ZERO AND WS-FIELD-LEN < 10
           AND WS-FIELD (1:WS-FIELD-LEN) NUMERIC
               MOVE WS-FIELD (1:WS-FIELD-LEN) TO WS-TRL-COUNT
           END-IF.
           PERFORM 7000-NEXT-FIELD THRU 7000-EXIT.
           IF WS-FIELD-LEN > ZERO AND WS-FIELD-LEN < 10
           AND WS-FIELD (1:WS-FIELD-LEN) NUMERIC
               MOVE WS-FIELD (1:WS-FIELD-LEN) TO WS-TRL-COUNT-2
           END-IF.
           IF WS-TRL-TOTAL-RAW NOT = WS-FILE-TOTAL-RAW
               MOVE WS-TRL-TOTAL-RAW TO WS-DISP-RAW
               DISPLAY 'RCB120 *** 99 FILE CONTROL TOTAL ' WS-DISP-RAW
               MOVE WS-FILE-TOTAL-RAW TO WS-DISP-RAW
               DISPLAY 'RCB120 *** SUM OF 98 TOTALS      ' WS-DISP-RAW
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
           IF WS-TRL-COUNT NOT = WS-FILE-GROUPS
           OR WS-TRL-COUNT-2 NOT = WS-FILE-RECS
               DISPLAY 'RCB120 *** 99 GROUPS/RECORDS ' WS-TRL-COUNT
                       '/' WS-TRL-COUNT-2 ' COUNTED ' WS-FILE-GROUPS
                       '/' WS-FILE-RECS
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
       3700-EXIT.
           EXIT.
      *================================================================*
       3900-END-OF-FILE-CHECKS.
      *================================================================*
           IF NOT FILE-HEADER-SEEN
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3900-END-OF-FILE-CHECKS' TO AB-PARAGRAPH
               MOVE 'NO 01 FILE HEADER - EMPTY OR WRONG FILE RECEIVED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF NOT FILE-TRAILER-SEEN
               MOVE 'BAISTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3900-END-OF-FILE-CHECKS' TO AB-PARAGRAPH
               MOVE 'NO 99 FILE TRAILER - STATEMENT TRUNCATED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF IN-GROUP OR IN-ACCOUNT
               DISPLAY 'RCB120 WARNING - GROUP OR ACCOUNT NOT CLOSED '
                       'BY 98/49 BEFORE THE 99 TRAILER'
               PERFORM 3950-TOTAL-WARNING THRU 3950-EXIT
           END-IF.
       3900-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CHG23121 - OUT OF BALANCE IS A WARNING (RC 4), NOT AN ABEND    *
      *----------------------------------------------------------------*
       3950-TOTAL-WARNING.
           ADD 1 TO WS-TOTAL-WARN.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'BAITOT'       TO AU-EVENT.
           MOVE 'W'            TO AU-SEVERITY.
           MOVE WS-ACCT-NUMBER TO AU-KEY.
           MOVE 'BAI2 CONTROL TOTAL OR RECORD COUNT DIFFERENCE'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       3950-EXIT.
           EXIT.
      *================================================================*
      * NEXT FIELD OF THE LOGICAL RECORD.  FIELDS END AT ',' OR '/'.   *
      * AFTER THE '/' (OR THE END OF THE DATA) EVERY FURTHER FIELD IS  *
      * EMPTY - BAI2 DEFAULTED FIELDS.                                 *
      *================================================================*
       7000-NEXT-FIELD.
           MOVE SPACES TO WS-FIELD.
           MOVE ZERO   TO WS-FIELD-LEN.
           MOVE SPACE  TO WS-DELIM.
           IF END-OF-LOGICAL-REC OR WS-PTR > WS-LOG-LEN
               MOVE 'Y' TO WS-END-OF-REC-SW
               GO TO 7000-EXIT
           END-IF.
           UNSTRING WS-LOG-REC (1:WS-LOG-LEN)
               DELIMITED BY ',' OR '/'
               INTO WS-FIELD DELIMITER IN WS-DELIM
                             COUNT IN WS-FIELD-LEN
               WITH POINTER WS-PTR
           END-UNSTRING.
           IF WS-DELIM = '/' OR WS-DELIM = SPACE
               MOVE 'Y' TO WS-END-OF-REC-SW
           END-IF.
       7000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ALL REMAINING FIELDS INTO WS-FLD (1..12) (01 AND 02 RECORDS)   *
      *----------------------------------------------------------------*
       7100-SPLIT-FIELDS.
           MOVE SPACES TO WS-FIELD-ARRAY.
           MOVE ZERO   TO WS-FLD-CNT.
           MOVE WS-FIELD TO WS-FLD (1).
           MOVE 1 TO WS-FLD-CNT.
           PERFORM VARYING WS-FX FROM 2 BY 1 UNTIL WS-FX > 12
               PERFORM 7000-NEXT-FIELD THRU 7000-EXIT
               MOVE WS-FIELD TO WS-FLD (WS-FX)
               IF WS-FIELD-LEN > ZERO
                   MOVE WS-FX TO WS-FLD-CNT
               END-IF
           END-PERFORM.
       7100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * AMOUNT TEXT -> WS-AMT-RAW (AS SENT) AND WS-AMT-VALUE (SCALED   *
      * BY THE IMPLIED DECIMALS OF THE ACCOUNT CURRENCY).  OPTIONAL    *
      * LEADING SIGN (CHG38804).  BLANK AMOUNT = ZERO.                 *
      *----------------------------------------------------------------*
       7200-CONVERT-AMOUNT.
           MOVE 'Y'   TO WS-AMT-OK-SW.
           MOVE '+'   TO WS-AMT-SIGN.
           MOVE ZERO  TO WS-AMT-RAW WS-AMT-VALUE.
           IF WS-AMT-TEXT = SPACES
               GO TO 7200-EXIT
           END-IF.
           MOVE 1 TO WS-AMT-START.
           IF WS-AMT-TEXT (1:1) = '+' OR WS-AMT-TEXT (1:1) = '-'
               MOVE WS-AMT-TEXT (1:1) TO WS-AMT-SIGN
               MOVE 2 TO WS-AMT-START
           END-IF.
           MOVE ZERO TO WS-AMT-DIGITS.
           INSPECT WS-AMT-TEXT (WS-AMT-START:)
               TALLYING WS-AMT-DIGITS FOR CHARACTERS BEFORE INITIAL ' '.
           IF WS-AMT-DIGITS = ZERO OR WS-AMT-DIGITS > 18
               MOVE 'N' TO WS-AMT-OK-SW
               GO TO 7200-EXIT
           END-IF.
           IF WS-AMT-TEXT (WS-AMT-START:WS-AMT-DIGITS) NOT NUMERIC
               MOVE 'N' TO WS-AMT-OK-SW
               GO TO 7200-EXIT
           END-IF.
           MOVE ZEROS TO WS-AMT-18.
           MOVE WS-AMT-TEXT (WS-AMT-START:WS-AMT-DIGITS)
             TO WS-AMT-18 (19 - WS-AMT-DIGITS:WS-AMT-DIGITS).
           MOVE WS-AMT-18-9 TO WS-AMT-RAW.
           EVALUATE WS-IMPLIED-DEC
               WHEN 0
                   COMPUTE WS-AMT-VALUE = WS-AMT-RAW
               WHEN 3
                   COMPUTE WS-AMT-VALUE = WS-AMT-RAW / 1000
               WHEN OTHER
                   COMPUTE WS-AMT-VALUE = WS-AMT-RAW / 100
           END-EVALUATE.
           IF WS-AMT-SIGN = '-'
               COMPUTE WS-AMT-RAW = WS-AMT-RAW * -1
           END-IF.
       7200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * YYMMDD -> CCYYMMDD THROUGH CMU010 W2Y4                         *
      *----------------------------------------------------------------*
       7300-WINDOW-DATE.
           MOVE ZERO TO WS-DATE-OUT.
           IF WS-YYMMDD-X NOT NUMERIC
               GO TO 7300-EXIT
           END-IF.
           MOVE 'W2Y4'       TO DT-FUNCTION.
           MOVE SPACES       TO DT-CALENDAR.
           MOVE WS-YYMMDD-9  TO DT-DATE-6.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE DT-RESULT-DATE TO WS-DATE-OUT
           ELSE
               DISPLAY 'RCB120 DATE ' WS-YYMMDD-X ' NOT VALID - CMU010 '
                       'RC ' DT-RETURN-CODE
           END-IF.
       7300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7400-IMPLIED-DECIMALS.
      *----------------------------------------------------------------*
           MOVE 2 TO WS-IMPLIED-DEC.
           SET CD-IDX TO 1.
           SEARCH WS-CD-ENTRY
               AT END
                   CONTINUE
               WHEN WS-CD-CCY (CD-IDX) = WS-ACCT-CCY
                   MOVE WS-CD-DEC (CD-IDX) TO WS-IMPLIED-DEC
           END-SEARCH.
       7400-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-BAI.
           READ BAISTMT-FILE.
           EVALUATE TRUE
               WHEN BAISTMT-OK
                   CONTINUE
               WHEN BAISTMT-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'BAISTMT' TO AB-DDNAME
                   MOVE WS-BAISTMT-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-BAI' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-WRITE-BANKTXN.
      *----------------------------------------------------------------*
           WRITE BANKTXN-REC FROM RCT-BANK-TXN-REC.
           IF WS-BANKTXN-STATUS NOT = '00'
               MOVE 'BANKTXN' TO AB-DDNAME
               MOVE WS-BANKTXN-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-WRITE-BANKTXN' TO AB-PARAGRAPH
               MOVE RCT-BANK-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RCB120'       TO CT-STAGE.
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
           CLOSE BAISTMT-FILE.
           CLOSE BANKTXN-FILE.
           IF WS-BANKTXN-STATUS NOT = '00'
               MOVE 'BANKTXN' TO AB-DDNAME
               MOVE WS-BANKTXN-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           COMPUTE WS-NET-AMT = WS-CREDIT-AMT - WS-DEBIT-AMT.
           MOVE 'BAI-RECS-IN'    TO CT-COUNTER-NAME.
           MOVE WS-PHYS-READ     TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BAI-TXN-OUT'    TO CT-COUNTER-NAME.
           MOVE WS-TXN-OUT       TO CT-COUNT.
           MOVE WS-NET-AMT       TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BAI-TXN-REJECT' TO CT-COUNTER-NAME.
           MOVE WS-DETAIL-REJ    TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* RCB120 - BAI2 BANK STATEMENT PARSE           *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' STATEMENT AS-OF DATE     : ' WS-FIRST-ASOF-DATE.
           MOVE WS-PHYS-READ TO WS-DISP-CNT.
           DISPLAY ' PHYSICAL RECORDS READ    : ' WS-DISP-CNT.
           MOVE WS-CONT-CNT TO WS-DISP-CNT.
           DISPLAY '   88 CONTINUATIONS       : ' WS-DISP-CNT.
           MOVE WS-LOGICAL-CNT TO WS-DISP-CNT.
           DISPLAY ' LOGICAL RECORDS          : ' WS-DISP-CNT.
           MOVE WS-GROUP-CNT TO WS-DISP-CNT.
           DISPLAY '   GROUPS (02)            : ' WS-DISP-CNT.
           MOVE WS-ACCOUNT-CNT TO WS-DISP-CNT.
           DISPLAY '   ACCOUNTS (03)          : ' WS-DISP-CNT.
           MOVE WS-DETAIL-CNT TO WS-DISP-CNT.
           DISPLAY '   TRANSACTIONS (16)      : ' WS-DISP-CNT.
           MOVE WS-UNKNOWN-REC TO WS-DISP-CNT.
           DISPLAY '   UNKNOWN / OUT OF PLACE : ' WS-DISP-CNT.
           MOVE WS-DETAIL-REJ TO WS-DISP-CNT.
           DISPLAY ' TRANSACTIONS REJECTED    : ' WS-DISP-CNT.
           MOVE WS-UNKNOWN-TYPE TO WS-DISP-CNT.
           DISPLAY ' TYPE CODES NOT IN TABLE  : ' WS-DISP-CNT.
           MOVE WS-VALUE-DATED TO WS-DISP-CNT.
           DISPLAY ' VALUE DATED (FUNDS V)    : ' WS-DISP-CNT.
           MOVE WS-TRUNC-REF TO WS-DISP-CNT.
           DISPLAY ' CUSTOMER REFS TRUNCATED  : ' WS-DISP-CNT.
           MOVE WS-UNKNOWN-ACCT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS NOT IN TABLE    : ' WS-DISP-CNT.
           MOVE WS-TXN-OUT TO WS-DISP-CNT.
           DISPLAY ' BANK TXNS WRITTEN        : ' WS-DISP-CNT.
           MOVE WS-CREDIT-CNT TO WS-DISP-CNT.
           MOVE WS-CREDIT-AMT TO WS-DISP-AMT.
           DISPLAY '   CREDITS  ' WS-DISP-CNT '  ' WS-DISP-AMT.
           MOVE WS-DEBIT-CNT TO WS-DISP-CNT.
           MOVE WS-DEBIT-AMT TO WS-DISP-AMT.
           DISPLAY '   DEBITS   ' WS-DISP-CNT '  ' WS-DISP-AMT.
           MOVE WS-TOTAL-WARN TO WS-DISP-CNT.
           DISPLAY ' CONTROL TOTAL WARNINGS   : ' WS-DISP-CNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'BAI2 STATEMENT PARSED WITH WARNINGS' TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'BAI2 STATEMENT PARSED' TO AU-MESSAGE
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RCB120 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RCB120 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RCB120 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
