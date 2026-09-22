       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCB110.
       AUTHOR.        T L MORRISON.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JANUARY 1998.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCB110                                            *
      * DESCRIPTION: FED BOOK-ENTRY AND EUROCLEAR STATEMENT LOAD.      *
      *              PART 1 - FEDERAL RESERVE BOOK-ENTRY STATEMENT     *
      *                (GOVERNMENTS, PAR AMOUNTS, BY CUSIP).           *
      *              PART 2 - EUROCLEAR STATEMENT OF HOLDINGS (MT535   *
      *                FLATTENED BY THE BANK).  EUROCLEAR IDENTIFIES   *
      *                SECURITIES BY ISIN ONLY - THE CUSIP IS FOUND    *
      *                THROUGH THE SECURITY MASTER (CMD010 'GETI').    *
      *                QUANTITIES ARE SWIFT TEXT WITH A DECIMAL COMMA. *
      *              BOTH ARE WRITTEN AS NORMALIZED STREET POSITIONS   *
      *              (RCSTPOS) FOR THE POSITION RECONCILIATION.        *
      *              STATEMENTS ARE AS OF THE PREVIOUS BUSINESS DAY.   *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD010 / STEP020  (IKJEFT01 - PLAN MSRCPLN)     *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - PARAMETER CARDS  (RCP110A)             *
      *              FEDSTMT  - MSEC.PROD.RC.FEDSTMT.RAW(0)   (RCFEDST)*
      *              EUCSTMT  - MSEC.PROD.RC.EUCSTMT.RAW(0)   (RCEUCST)*
      * OUTPUT     : FEDOUT   - MSEC.PROD.RC.STPOS.FED(+1)    (RCSTPOS)*
      *              EUCOUT   - MSEC.PROD.RC.STPOS.EUC(+1)    (RCSTPOS)*
      * CALLS      : CMD010 (GET, GETI), CMU050, CMU060, CMU080        *
      * RETURN CODE: 0 CLEAN                                           *
      *              4 ISIN NOT ON SECURITY MASTER, QUANTITY NOT       *
      *                READABLE, RECORD REJECTED                       *
      *              8 TRAILER COUNT OUT OF BALANCE                    *
      *              U1003 DB2 ERROR  U1005 WRONG STATEMENT DATE       *
      *              U1008 HEADER OR TRAILER MISSING                   *
      *----------------------------------------------------------------*
      * PARAMETER CARDS (SYSIN, * IN COLUMN 1 = COMMENT):              *
      *   DATECHK=Y|N   STATEMENT DATE MUST = PREVIOUS BUSINESS DAY    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1998-01-12 TLM  ORIGINAL - FED BOOK ENTRY ONLY        CHG03880 *
      * 1998-12-07 TLM  Y2K - DATES CCYYMMDD                  CHG04471 *
      * 2001-05-14 KAP  QTY 4 DECIMALS                        CHG08811 *
      * 2003-10-06 KAP  EUROCLEAR HOLDINGS STATEMENT (PART 2) CHG11702 *
      * 2003-11-17 KAP  ISIN TO CUSIP VIA SECURITY MASTER     CHG11760 *
      * 2005-08-30 KAP  CMD010 GETI (ISIN INDEX XSECMS02)     CHG13391 *
      * 2007-04-02 SPA  UNKNOWN ISIN - KEEP THE HOLDING WITH  CHG16220 *
      *                 THE CUSIP FROM THE ISIN, FLAG 'N'              *
      * 2011-06-20 SPA  NEGATIVE SWIFT QTY ('N' PREFIX)       CHG21877 *
      * 2014-08-18 MHC  DATECHK PARAMETER FOR DR RECOVERY     CHG27715 *
      * 2016-05-09 SPA  RUN UNDER MSPDB2                      CHG29344 *
      * 2019-02-25 MHC  CONTROL TOTALS NAMES PER CMB090       CHG33410 *
      * 2020-06-08 NVR  FED TRAILER PAR TOTAL PROOF           CHG36620 *
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
           SELECT FEDSTMT-FILE   ASSIGN TO FEDSTMT
                  FILE STATUS IS WS-FEDSTMT-STATUS.
           SELECT EUCSTMT-FILE   ASSIGN TO EUCSTMT
                  FILE STATUS IS WS-EUCSTMT-STATUS.
           SELECT FEDOUT-FILE    ASSIGN TO FEDOUT
                  FILE STATUS IS WS-FEDOUT-STATUS.
           SELECT EUCOUT-FILE    ASSIGN TO EUCOUT
                  FILE STATUS IS WS-EUCOUT-STATUS.
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
       FD  FEDSTMT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RCFEDST.
       FD  EUCSTMT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EUCSTMT-REC                 PIC X(150).
       FD  FEDOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FEDOUT-REC                  PIC X(120).
       FD  EUCOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EUCOUT-REC                  PIC X(120).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCB110'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-FEDSTMT-STATUS       PIC X(02)  VALUE '00'.
               88  FEDSTMT-OK                     VALUE '00'.
               88  FEDSTMT-EOF                    VALUE '10'.
           05  WS-EUCSTMT-STATUS       PIC X(02)  VALUE '00'.
               88  EUCSTMT-OK                     VALUE '00'.
               88  EUCSTMT-EOF                    VALUE '10'.
           05  WS-FEDOUT-STATUS        PIC X(02)  VALUE '00'.
           05  WS-EUCOUT-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-FED-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-FED                     VALUE 'Y'.
           05  WS-EUC-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-EUC                     VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-FED-HDR-SW           PIC X(01)  VALUE 'N'.
               88  FED-HEADER-SEEN                VALUE 'Y'.
           05  WS-FED-TRL-SW           PIC X(01)  VALUE 'N'.
               88  FED-TRAILER-SEEN               VALUE 'Y'.
           05  WS-EUC-HDR-SW           PIC X(01)  VALUE 'N'.
               88  EUC-HEADER-SEEN                VALUE 'Y'.
           05  WS-EUC-TRL-SW           PIC X(01)  VALUE 'N'.
               88  EUC-TRAILER-SEEN               VALUE 'Y'.
           05  WS-DATECHK-SW           PIC X(01)  VALUE 'Y'.
               88  CHECK-STMT-DATE                VALUE 'Y'.
           05  WS-HLD-OK-SW            PIC X(01)  VALUE 'Y'.
               88  HOLDING-OK                     VALUE 'Y'.
               88  HOLDING-REJECTED               VALUE 'N'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-PARM-KEYWORD             PIC X(10).
       01  WS-PARM-VALUE               PIC X(20).
      *----------------------------------------------------------------*
      * FED WORK AREAS                                                 *
      *----------------------------------------------------------------*
       01  WS-FED-WORK.
           05  WS-FED-ABA              PIC X(09)  VALUE SPACES.
           05  WS-FED-STMT-DATE        PIC 9(08)  VALUE ZERO.
           05  WS-FED-TRL-COUNT        PIC 9(07)  VALUE ZERO.
           05  WS-FED-TRL-PAR          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-FED-DTL-PAR          PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-FED-FREE-PAR         PIC S9(13)V99 COMP-3.
       01  WS-FED-COUNTERS.
           05  WS-FED-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FED-DTL-READ         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FED-REJECTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FED-OUT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FED-NO-SECM          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FED-PAR-HASH         PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * EUROCLEAR WORK AREAS                                           *
      *----------------------------------------------------------------*
       COPY RCEUCST.
       01  WS-EUC-WORK.
           05  WS-EUC-ACCOUNT          PIC X(08)  VALUE SPACES.
           05  WS-EUC-STMT-DATE        PIC 9(08)  VALUE ZERO.
           05  WS-EUC-STMT-DATE-X REDEFINES WS-EUC-STMT-DATE
                                       PIC X(08).
           05  WS-EUC-TRL-COUNT        PIC 9(07)  VALUE ZERO.
           05  WS-EUC-AGGR-QTY         PIC S9(13)V9(04) COMP-3.
           05  WS-EUC-AVAIL-QTY        PIC S9(13)V9(04) COMP-3.
           05  WS-EUC-NOTAVL-QTY       PIC S9(13)V9(04) COMP-3.
           05  WS-EUC-REJECT-REASON    PIC X(40).
           05  WS-EUC-CUSIP            PIC X(09).
           05  WS-EUC-ID-FLAG          PIC X(01).
       01  WS-EUC-COUNTERS.
           05  WS-EUC-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EUC-HLD-READ         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EUC-REJECTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EUC-OUT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ISIN-NOTFOUND        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ISIN-FOUND           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NOT-EUCL-DEPO        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-QTY-UNREADABLE       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EUC-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * SWIFT QUANTITY PARSE (DECIMAL COMMA)                           *
      *   '1500,'  -> 1500.0000     '1234,5' -> 1234.5000              *
      *   'N25,'   -> -25.0000      ',5'     -> 0.5000                 *
      * THE INTEGER PART IS RIGHT JUSTIFIED, THE FRACTION IS LEFT      *
      * JUSTIFIED (DIGITS AFTER THE COMMA ARE TENTHS, HUNDREDTHS..).   *
      *----------------------------------------------------------------*
       01  WS-SWQ-AREA.
           05  WS-SWQ-TEXT             PIC X(18).
           05  WS-SWQ-WORK             PIC X(18).
           05  WS-SWQ-INT-X            PIC X(18).
           05  WS-SWQ-DEC-X            PIC X(18).
           05  WS-SWQ-INT-LEN          PIC S9(04) COMP.
           05  WS-SWQ-DEC-LEN          PIC S9(04) COMP.
           05  WS-SWQ-COMMAS           PIC S9(04) COMP.
           05  WS-SWQ-POINTS           PIC S9(04) COMP.
           05  WS-SWQ-FIELDS           PIC S9(04) COMP.
           05  WS-SWQ-START            PIC S9(04) COMP.
           05  WS-SWQ-INT-13           PIC X(13).
           05  WS-SWQ-INT-9 REDEFINES WS-SWQ-INT-13
                                       PIC 9(13).
           05  WS-SWQ-DEC-4            PIC X(04).
           05  WS-SWQ-DEC-9 REDEFINES WS-SWQ-DEC-4
                                       PIC 9(04).
           05  WS-SWQ-VALUE            PIC S9(13)V9(04) COMP-3.
           05  WS-SWQ-SIGN             PIC X(01).
           05  WS-SWQ-VALID-SW         PIC X(01).
               88  SWQ-VALID                      VALUE 'Y'.
               88  SWQ-INVALID                    VALUE 'N'.
           05  WS-SWQ-BLANK-SW         PIC X(01).
               88  SWQ-BLANK                      VALUE 'Y'.
       01  WS-CNT7-X                   PIC X(07).
       01  WS-CNT7-LEN                 PIC S9(04) COMP.
       01  WS-CNT7-R                   PIC X(07).
       01  WS-CNT7-9 REDEFINES WS-CNT7-R PIC 9(07).
       COPY RCSTPOS.
       COPY CMSECMS.
       COPY CMSECLNK.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-QTY             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  WS-DISP-SQL             PIC -ZZZZZZZZ9.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-FED-STATEMENT THRU 2000-EXIT.
           PERFORM 4000-EUROCLEAR-STATEMENT.
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
           MOVE 'FED / EUROCLEAR STATEMENT LOAD STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           OPEN INPUT FEDSTMT-FILE.
           IF WS-FEDSTMT-STATUS NOT = '00'
               MOVE 'FEDSTMT' TO AB-DDNAME
               MOVE WS-FEDSTMT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT EUCSTMT-FILE.
           IF WS-EUCSTMT-STATUS NOT = '00'
               MOVE 'EUCSTMT' TO AB-DDNAME
               MOVE WS-EUCSTMT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT FEDOUT-FILE.
           IF WS-FEDOUT-STATUS NOT = '00'
               MOVE 'FEDOUT' TO AB-DDNAME
               MOVE WS-FEDOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT EUCOUT-FILE.
           IF WS-EUCOUT-STATUS NOT = '00'
               MOVE 'EUCOUT' TO AB-DDNAME
               MOVE WS-EUCOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-READ-PARMS.
      *----------------------------------------------------------------*
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'RCB110 NO SYSIN PARAMETERS - DEFAULTS USED'
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
           DISPLAY 'RCB110 PARM: ' PARM-CARD-REC (1:60).
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
                   MOVE 'STATEMENT DATE CHECK SWITCHED OFF BY SYSIN'
                                TO AU-MESSAGE
                   CALL 'CMU060' USING AU-AUDIT-PARMS
               END-IF
           ELSE
               DISPLAY 'RCB110 UNKNOWN PARAMETER IGNORED: '
                       WS-PARM-KEYWORD
           END-IF.
       1110-EXIT.
           EXIT.
      *================================================================*
      * PART 1 - FEDERAL RESERVE BOOK-ENTRY STATEMENT                  *
      *================================================================*
       2000-FED-STATEMENT.
           PERFORM 8000-READ-FED THRU 8000-EXIT.
           PERFORM 2100-FED-RECORD THRU 2100-EXIT
               UNTIL END-OF-FED.
           IF NOT FED-HEADER-SEEN
               MOVE 'FEDSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2000-FED-STATEMENT' TO AB-PARAGRAPH
               MOVE 'NO FED HEADER - EMPTY OR WRONG FILE RECEIVED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF NOT FED-TRAILER-SEEN
               MOVE 'FEDSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2000-FED-STATEMENT' TO AB-PARAGRAPH
               MOVE 'NO FED TRAILER - STATEMENT TRUNCATED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-FED-RECORD.
      *----------------------------------------------------------------*
           ADD 1 TO WS-FED-READ.
           IF FED-HEADER
               PERFORM 2200-FED-HEADER THRU 2200-EXIT
           ELSE
           IF FED-DETAIL
               PERFORM 2300-FED-DETAIL THRU 2300-EXIT
           ELSE
           IF FED-TRAILER
               PERFORM 2400-FED-TRAILER THRU 2400-EXIT
           ELSE
               DISPLAY 'RCB110 FED RECORD TYPE ' FED-REC-TYPE
                       ' NOT KNOWN - SKIPPED'
               ADD 1 TO WS-FED-REJECTED
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF
           END-IF
           END-IF.
           PERFORM 8000-READ-FED THRU 8000-EXIT.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-FED-HEADER.
      *----------------------------------------------------------------*
           IF FED-HEADER-SEEN
               MOVE 'FEDSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2200-FED-HEADER' TO AB-PARAGRAPH
               MOVE 'SECOND FED HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'Y' TO WS-FED-HDR-SW.
           MOVE FED-ABA TO WS-FED-ABA.
           IF FED-STMT-DATE NUMERIC
               MOVE FED-STMT-DATE TO WS-FED-STMT-DATE
           END-IF.
           DISPLAY 'RCB110 FED STATEMENT ABA ' FED-ABA
                   ' DATE ' WS-FED-STMT-DATE.
           IF CHECK-STMT-DATE
           AND WS-FED-STMT-DATE NOT = DC-PREV-BUS-DATE
               MOVE 'FEDSTMT' TO AB-DDNAME
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '2200-FED-HEADER' TO AB-PARAGRAPH
               STRING 'STMT ' WS-FED-STMT-DATE ' EXPECTED '
                      DC-PREV-BUS-DATE
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'FED STATEMENT DATE NOT PREVIOUS BUSINESS DAY'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FED DETAIL - PAR AMOUNT (FACE) PER CUSIP                       *
      *----------------------------------------------------------------*
       2300-FED-DETAIL.
           ADD 1 TO WS-FED-DTL-READ.
           IF NOT FED-HEADER-SEEN
               MOVE 'FEDSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2300-FED-DETAIL' TO AB-PARAGRAPH
               MOVE 'FED DETAIL BEFORE HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF FED-CUSIP = SPACES
           OR FED-PAR-AMOUNT NOT NUMERIC
               DISPLAY 'RCB110 FED DETAIL ' WS-FED-DTL-READ
                       ' REJECTED - CUSIP OR PAR AMOUNT INVALID'
               ADD 1 TO WS-FED-REJECTED
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               GO TO 2300-EXIT
           END-IF.
           IF FED-PLEDGED-PAR NOT NUMERIC
               MOVE ZERO TO FED-PLEDGED-PAR
           END-IF.
           ADD FED-PAR-AMOUNT TO WS-FED-PAR-HASH WS-FED-DTL-PAR.
           MOVE SPACES           TO RSP-STREET-POS-REC.
           MOVE 'FED '           TO RSP-DEPOSITORY.
           MOVE FED-CUSIP        TO RSP-CUSIP.
           MOVE WS-FED-STMT-DATE TO RSP-STMT-DATE.
           MOVE FED-PAR-AMOUNT   TO RSP-TOTAL-QTY.
           MOVE FED-PLEDGED-PAR  TO RSP-PLEDGED-QTY.
           COMPUTE WS-FED-FREE-PAR = FED-PAR-AMOUNT - FED-PLEDGED-PAR.
           MOVE WS-FED-FREE-PAR  TO RSP-FREE-QTY.
           MOVE ZERO             TO RSP-SEG-QTY RSP-DEL-PEND-QTY
                                    RSP-REC-PEND-QTY.
           MOVE WS-FED-DTL-READ  TO RSP-SOURCE-SEQ.
           MOVE SPACE            TO RSP-ID-FLAG.
      *    ISIN FOR THE REPORTS ONLY - A GOVT NOT ON THE MASTER IS
      *    STILL RECONCILED BY CUSIP
           MOVE 'GET '           TO SL-FUNCTION.
           MOVE FED-CUSIP        TO SL-KEY-CUSIP.
           PERFORM 8900-CALL-CMD010 THRU 8900-EXIT.
           IF SL-FOUND
               MOVE SEC-ISIN     TO RSP-ISIN
           ELSE
               ADD 1 TO WS-FED-NO-SECM
               MOVE SPACES       TO RSP-ISIN
           END-IF.
           PERFORM 8200-WRITE-FED THRU 8200-EXIT.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2400-FED-TRAILER.
      *----------------------------------------------------------------*
           MOVE 'Y' TO WS-FED-TRL-SW.
           IF FED-REC-COUNT NUMERIC
               MOVE FED-REC-COUNT TO WS-FED-TRL-COUNT
           ELSE
               MOVE ZERO TO WS-FED-TRL-COUNT
           END-IF.
           IF WS-FED-TRL-COUNT NOT = WS-FED-DTL-READ
               MOVE WS-FED-TRL-COUNT TO WS-DISP-CNT
               DISPLAY 'RCB110 *** FED TRAILER COUNT ' WS-DISP-CNT
               MOVE WS-FED-DTL-READ TO WS-DISP-CNT
               DISPLAY 'RCB110 *** FED DETAILS READ  ' WS-DISP-CNT
               MOVE 'WRIT'      TO AU-FUNCTION
               MOVE 'TRLBAL'    TO AU-EVENT
               MOVE 'E'         TO AU-SEVERITY
               MOVE 'FEDSTMT'   TO AU-KEY
               MOVE 'FED STATEMENT TRAILER COUNT OUT OF BALANCE'
                                TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
      *    CHG36620 - THE FED ADDED THE PAR TOTAL TO THE TRAILER.  OLD
      *    FORMAT FILES (DR SITE) CARRY ZERO - NOT CHECKED THEN.
           IF FED-PAR-AMOUNT NUMERIC
               MOVE FED-PAR-AMOUNT TO WS-FED-TRL-PAR
           ELSE
               MOVE ZERO TO WS-FED-TRL-PAR
           END-IF.
           IF WS-FED-TRL-PAR NOT = ZERO
           AND WS-FED-TRL-PAR NOT = WS-FED-DTL-PAR
               MOVE WS-FED-TRL-PAR TO WS-DISP-QTY
               DISPLAY 'RCB110 *** FED TRAILER PAR TOTAL ' WS-DISP-QTY
               MOVE WS-FED-DTL-PAR TO WS-DISP-QTY
               DISPLAY 'RCB110 *** FED DETAIL PAR TOTAL  ' WS-DISP-QTY
               MOVE 'WRIT'      TO AU-FUNCTION
               MOVE 'TRLBAL'    TO AU-EVENT
               MOVE 'E'         TO AU-SEVERITY
               MOVE 'FEDSTMT'   TO AU-KEY
               MOVE 'FED STATEMENT TRAILER PAR TOTAL OUT OF BALANCE'
                                TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
       2400-EXIT.
           EXIT.
      *================================================================*
      * PART 2 - EUROCLEAR STATEMENT OF HOLDINGS          (CHG11702)   *
      *================================================================*
       4000-EUROCLEAR-STATEMENT.
           PERFORM 8100-READ-EUC.
           PERFORM UNTIL END-OF-EUC
               ADD 1 TO WS-EUC-READ
               EVALUATE TRUE
                   WHEN EUC-HEADER
                       PERFORM 4100-EUC-HEADER
                   WHEN EUC-HOLDING
                       PERFORM 4200-EUC-HOLDING
                   WHEN EUC-TRAILER
                       PERFORM 4300-EUC-TRAILER
                   WHEN OTHER
                       DISPLAY 'RCB110 EUROCLEAR RECORD TYPE '
                               EUC-REC-TYPE ' NOT KNOWN - SKIPPED'
                       ADD 1 TO WS-EUC-REJECTED
                       IF WS-RETURN-CODE < 4
                           MOVE 4 TO WS-RETURN-CODE
                       END-IF
               END-EVALUATE
               PERFORM 8100-READ-EUC
           END-PERFORM.
           IF NOT EUC-HEADER-SEEN
               MOVE 'EUCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '4000-EUROCLEAR-STATEMENT' TO AB-PARAGRAPH
               MOVE 'NO EUROCLEAR HDR - EMPTY OR WRONG FILE RECEIVED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF NOT EUC-TRAILER-SEEN
               MOVE 'EUCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '4000-EUROCLEAR-STATEMENT' TO AB-PARAGRAPH
               MOVE 'NO EUROCLEAR TRL - STATEMENT TRUNCATED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
       4100-EUC-HEADER.
      *----------------------------------------------------------------*
           IF EUC-HEADER-SEEN
               MOVE 'EUCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '4100-EUC-HEADER' TO AB-PARAGRAPH
               MOVE 'SECOND EUROCLEAR HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'Y' TO WS-EUC-HDR-SW.
           MOVE EUC-HDR-ACCOUNT TO WS-EUC-ACCOUNT.
           IF EUC-HDR-STMT-DATE NUMERIC
               MOVE EUC-HDR-STMT-DATE TO WS-EUC-STMT-DATE-X
           ELSE
               MOVE ZERO TO WS-EUC-STMT-DATE
           END-IF.
           DISPLAY 'RCB110 EUROCLEAR STATEMENT ACCOUNT ' EUC-HDR-ACCOUNT
                   ' DATE ' WS-EUC-STMT-DATE ' TYPE ' EUC-HDR-STMT-TYPE
                   ' PAGE ' EUC-HDR-PAGE.
           IF CHECK-STMT-DATE
           AND WS-EUC-STMT-DATE NOT = DC-PREV-BUS-DATE
               MOVE 'EUCSTMT' TO AB-DDNAME
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '4100-EUC-HEADER' TO AB-PARAGRAPH
               STRING 'STMT ' EUC-HDR-STMT-DATE ' EXPECTED '
                      DC-PREV-BUS-DATE
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'EUROCLEAR STMT DATE NOT PREVIOUS BUSINESS DAY'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
      * ONE HOLDING - ISIN, AGGREGATE / AVAILABLE / NOT AVAILABLE QTY  *
      *----------------------------------------------------------------*
       4200-EUC-HOLDING.
           ADD 1 TO WS-EUC-HLD-READ.
           MOVE 'Y' TO WS-HLD-OK-SW.
           MOVE SPACES TO WS-EUC-REJECT-REASON.
           IF NOT EUC-HEADER-SEEN
               MOVE 'EUCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '4200-EUC-HOLDING' TO AB-PARAGRAPH
               MOVE 'EUROCLEAR HOLDING BEFORE HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF EUC-HLD-ISIN = SPACES
               MOVE 'N' TO WS-HLD-OK-SW
               MOVE 'ISIN MISSING' TO WS-EUC-REJECT-REASON
           END-IF.
      *    AGGREGATE BALANCE - MANDATORY
           IF HOLDING-OK
               MOVE EUC-HLD-AGGR-QTY TO WS-SWQ-TEXT
               PERFORM 5000-PARSE-SWIFT-QTY
               IF SWQ-INVALID OR SWQ-BLANK
                   MOVE 'N' TO WS-HLD-OK-SW
                   STRING 'AGGREGATE QTY NOT READABLE: '
                          EUC-HLD-AGGR-QTY
                          DELIMITED BY SIZE INTO WS-EUC-REJECT-REASON
               ELSE
                   MOVE WS-SWQ-VALUE TO WS-EUC-AGGR-QTY
               END-IF
           END-IF.
      *    AVAILABLE / NOT AVAILABLE - OPTIONAL, BLANK = ZERO
           IF HOLDING-OK
               MOVE EUC-HLD-AVAIL-QTY TO WS-SWQ-TEXT
               PERFORM 5000-PARSE-SWIFT-QTY
               EVALUATE TRUE
                   WHEN SWQ-BLANK
                       MOVE WS-EUC-AGGR-QTY TO WS-EUC-AVAIL-QTY
                   WHEN SWQ-VALID
                       MOVE WS-SWQ-VALUE TO WS-EUC-AVAIL-QTY
                   WHEN OTHER
                       MOVE 'N' TO WS-HLD-OK-SW
                       STRING 'AVAILABLE QTY NOT READABLE: '
                              EUC-HLD-AVAIL-QTY
                              DELIMITED BY SIZE
                              INTO WS-EUC-REJECT-REASON
               END-EVALUATE
           END-IF.
           IF HOLDING-OK
               MOVE EUC-HLD-NOTAVL-QTY TO WS-SWQ-TEXT
               PERFORM 5000-PARSE-SWIFT-QTY
               EVALUATE TRUE
                   WHEN SWQ-BLANK
                       MOVE ZERO TO WS-EUC-NOTAVL-QTY
                   WHEN SWQ-VALID
                       MOVE WS-SWQ-VALUE TO WS-EUC-NOTAVL-QTY
                   WHEN OTHER
                       MOVE 'N' TO WS-HLD-OK-SW
                       STRING 'NOT AVAIL QTY NOT READABLE: '
                              EUC-HLD-NOTAVL-QTY
                              DELIMITED BY SIZE
                              INTO WS-EUC-REJECT-REASON
               END-EVALUATE
           END-IF.
           IF HOLDING-REJECTED
               ADD 1 TO WS-EUC-REJECTED
               ADD 1 TO WS-QTY-UNREADABLE
               DISPLAY 'RCB110 EUROCLEAR HOLDING ' EUC-HLD-ISIN
                       ' REJECTED - ' WS-EUC-REJECT-REASON
               MOVE 'WRIT'          TO AU-FUNCTION
               MOVE 'EUCREJ'        TO AU-EVENT
               MOVE 'W'             TO AU-SEVERITY
               MOVE EUC-HLD-ISIN    TO AU-KEY
               MOVE WS-EUC-REJECT-REASON TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           ELSE
               ADD WS-EUC-AGGR-QTY TO WS-EUC-QTY-HASH
               PERFORM 4400-ISIN-TO-CUSIP
               PERFORM 4500-BUILD-EUC-POSITION
               PERFORM 8300-WRITE-EUC
           END-IF.
      *----------------------------------------------------------------*
       4300-EUC-TRAILER.
      *----------------------------------------------------------------*
           MOVE 'Y' TO WS-EUC-TRL-SW.
      *    COUNT IS TEXT - RIGHT JUSTIFIED ZEROS FROM THE BANK, BUT
      *    THE TEST FILES CAME LEFT JUSTIFIED (CHG11702)
           IF EUC-TRL-COUNT NUMERIC
               MOVE EUC-TRL-COUNT TO WS-CNT7-R
           ELSE
               MOVE SPACES TO WS-CNT7-X
               MOVE ZERO   TO WS-CNT7-LEN
               UNSTRING EUC-TRL-COUNT DELIMITED BY ALL SPACE
                   INTO WS-CNT7-X COUNT IN WS-CNT7-LEN
               END-UNSTRING
               MOVE ZEROS TO WS-CNT7-R
               IF WS-CNT7-LEN > ZERO AND WS-CNT7-LEN NOT > 7
                   MOVE WS-CNT7-X (1:WS-CNT7-LEN)
                     TO WS-CNT7-R (8 - WS-CNT7-LEN:WS-CNT7-LEN)
               END-IF
               IF WS-CNT7-R NOT NUMERIC
                   MOVE ZEROS TO WS-CNT7-R
               END-IF
           END-IF.
           MOVE WS-CNT7-9 TO WS-EUC-TRL-COUNT.
           IF WS-EUC-TRL-COUNT NOT = WS-EUC-HLD-READ
               MOVE WS-EUC-TRL-COUNT TO WS-DISP-CNT
               DISPLAY 'RCB110 *** EUROCLEAR TRAILER COUNT ' WS-DISP-CNT
               MOVE WS-EUC-HLD-READ TO WS-DISP-CNT
               DISPLAY 'RCB110 *** EUROCLEAR HOLDINGS READ ' WS-DISP-CNT
               MOVE 'WRIT'      TO AU-FUNCTION
               MOVE 'TRLBAL'    TO AU-EVENT
               MOVE 'E'         TO AU-SEVERITY
               MOVE 'EUCSTMT'   TO AU-KEY
               MOVE 'EUROCLEAR TRAILER COUNT OUT OF BALANCE'
                                TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
      *----------------------------------------------------------------*
      * ISIN TO CUSIP THROUGH THE SECURITY MASTER.  NOT FOUND: THE     *
      * CUSIP IS TAKEN FROM POSITIONS 3-11 OF THE ISIN (US/CA ISINS    *
      * CARRY THE CUSIP) AND THE HOLDING IS FLAGGED 'N' SO THE RECON   *
      * SHOWS IT AS A STREET-ONLY ITEM WITH THE ISIN.     (CHG16220)   *
      *----------------------------------------------------------------*
       4400-ISIN-TO-CUSIP.
           MOVE 'GETI'           TO SL-FUNCTION.
           MOVE EUC-HLD-ISIN     TO SL-KEY-ISIN.
           MOVE SPACES           TO SL-KEY-CUSIP SL-KEY-SYMBOL.
           PERFORM 8900-CALL-CMD010 THRU 8900-EXIT.
           IF SL-FOUND
               ADD 1 TO WS-ISIN-FOUND
               MOVE SEC-CUSIP    TO WS-EUC-CUSIP
               MOVE 'I'          TO WS-EUC-ID-FLAG
               IF SEC-DEPOSITORY NOT = 'EUCL'
                   ADD 1 TO WS-NOT-EUCL-DEPO
                   DISPLAY 'RCB110 ISIN ' EUC-HLD-ISIN ' CUSIP '
                           SEC-CUSIP ' DEPOSITORY ON MASTER IS '
                           SEC-DEPOSITORY
               END-IF
           ELSE
               ADD 1 TO WS-ISIN-NOTFOUND
               MOVE EUC-HLD-ISIN (3:9) TO WS-EUC-CUSIP
               MOVE 'N'          TO WS-EUC-ID-FLAG
               DISPLAY 'RCB110 ISIN ' EUC-HLD-ISIN
                       ' NOT ON SECURITY MASTER - CUSIP TAKEN AS '
                       WS-EUC-CUSIP
               MOVE 'WRIT'       TO AU-FUNCTION
               MOVE 'ISINNF'     TO AU-EVENT
               MOVE 'W'          TO AU-SEVERITY
               MOVE EUC-HLD-ISIN TO AU-KEY
               MOVE 'EUROCLEAR ISIN NOT ON SECURITY MASTER'
                                 TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
      *----------------------------------------------------------------*
       4500-BUILD-EUC-POSITION.
      *----------------------------------------------------------------*
           MOVE SPACES             TO RSP-STREET-POS-REC.
           MOVE 'EUCL'             TO RSP-DEPOSITORY.
           MOVE WS-EUC-CUSIP       TO RSP-CUSIP.
           MOVE WS-EUC-STMT-DATE   TO RSP-STMT-DATE.
           MOVE EUC-HLD-ISIN       TO RSP-ISIN.
           MOVE WS-EUC-AGGR-QTY    TO RSP-TOTAL-QTY.
           MOVE WS-EUC-AVAIL-QTY   TO RSP-FREE-QTY.
           MOVE WS-EUC-NOTAVL-QTY  TO RSP-PLEDGED-QTY.
           MOVE ZERO               TO RSP-SEG-QTY RSP-DEL-PEND-QTY
                                      RSP-REC-PEND-QTY.
           MOVE WS-EUC-HLD-READ    TO RSP-SOURCE-SEQ.
           MOVE WS-EUC-ID-FLAG     TO RSP-ID-FLAG.
      *================================================================*
      * SWIFT QUANTITY (DECIMAL COMMA) -> S9(13)V9(4)                  *
      * IN : WS-SWQ-TEXT    OUT: WS-SWQ-VALUE, WS-SWQ-VALID-SW,        *
      *                          WS-SWQ-BLANK-SW                       *
      *================================================================*
       5000-PARSE-SWIFT-QTY.
           MOVE 'Y'    TO WS-SWQ-VALID-SW.
           MOVE 'N'    TO WS-SWQ-BLANK-SW.
           MOVE ZERO   TO WS-SWQ-VALUE.
           MOVE '+'    TO WS-SWQ-SIGN.
           IF WS-SWQ-TEXT = SPACES
               MOVE 'Y' TO WS-SWQ-BLANK-SW
           ELSE
      *        LEADING BLANKS ARE NOT SWIFT BUT THE BANK SENDS THEM ON
      *        THE NOT-AVAILABLE COLUMN
               MOVE 1 TO WS-SWQ-START
               PERFORM UNTIL WS-SWQ-START > 18
                          OR WS-SWQ-TEXT (WS-SWQ-START:1) NOT = SPACE
                   ADD 1 TO WS-SWQ-START
               END-PERFORM
               MOVE SPACES TO WS-SWQ-WORK
               MOVE WS-SWQ-TEXT (WS-SWQ-START:) TO WS-SWQ-WORK
               IF WS-SWQ-WORK (1:1) = 'N'
                   MOVE '-' TO WS-SWQ-SIGN
                   MOVE WS-SWQ-WORK (2:) TO WS-SWQ-INT-X
                   MOVE WS-SWQ-INT-X TO WS-SWQ-WORK
               END-IF
               MOVE ZERO TO WS-SWQ-COMMAS WS-SWQ-POINTS
               INSPECT WS-SWQ-WORK TALLYING WS-SWQ-COMMAS FOR ALL ','
                                            WS-SWQ-POINTS FOR ALL '.'
               IF WS-SWQ-COMMAS NOT = 1 OR WS-SWQ-POINTS NOT = ZERO
                   MOVE 'N' TO WS-SWQ-VALID-SW
               ELSE
                   PERFORM 5100-SPLIT-SWIFT-QTY
               END-IF
           END-IF.
      *----------------------------------------------------------------*
       5100-SPLIT-SWIFT-QTY.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-SWQ-INT-X WS-SWQ-DEC-X.
           MOVE ZERO   TO WS-SWQ-INT-LEN WS-SWQ-DEC-LEN WS-SWQ-FIELDS.
           UNSTRING WS-SWQ-WORK DELIMITED BY ',' OR SPACE
               INTO WS-SWQ-INT-X COUNT IN WS-SWQ-INT-LEN
                    WS-SWQ-DEC-X COUNT IN WS-SWQ-DEC-LEN
               TALLYING IN WS-SWQ-FIELDS
           END-UNSTRING.
           IF WS-SWQ-INT-LEN > 13
               MOVE 'N' TO WS-SWQ-VALID-SW
           END-IF.
           IF WS-SWQ-INT-LEN = ZERO AND WS-SWQ-DEC-LEN = ZERO
               MOVE 'N' TO WS-SWQ-VALID-SW
           END-IF.
           IF SWQ-VALID
               MOVE ZEROS TO WS-SWQ-INT-13
               IF WS-SWQ-INT-LEN > ZERO
                   MOVE WS-SWQ-INT-X (1:WS-SWQ-INT-LEN)
                     TO WS-SWQ-INT-13 (14 - WS-SWQ-INT-LEN:
                                       WS-SWQ-INT-LEN)
               END-IF
               MOVE ZEROS TO WS-SWQ-DEC-4
      *        4 DECIMALS ON THE STOCK RECORD - FURTHER DIGITS DROPPED
               IF WS-SWQ-DEC-LEN > 4
                   MOVE 4 TO WS-SWQ-DEC-LEN
               END-IF
               IF WS-SWQ-DEC-LEN > ZERO
                   MOVE WS-SWQ-DEC-X (1:WS-SWQ-DEC-LEN)
                     TO WS-SWQ-DEC-4 (1:WS-SWQ-DEC-LEN)
               END-IF
               IF WS-SWQ-INT-13 NOT NUMERIC
               OR WS-SWQ-DEC-4 NOT NUMERIC
                   MOVE 'N' TO WS-SWQ-VALID-SW
               ELSE
                   COMPUTE WS-SWQ-VALUE =
                           WS-SWQ-INT-9 + WS-SWQ-DEC-9 / 10000
                   IF WS-SWQ-SIGN = '-'
                       COMPUTE WS-SWQ-VALUE = WS-SWQ-VALUE * -1
                   END-IF
               END-IF
           END-IF.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-FED.
           READ FEDSTMT-FILE.
           EVALUATE TRUE
               WHEN FEDSTMT-OK
                   CONTINUE
               WHEN FEDSTMT-EOF
                   MOVE 'Y' TO WS-FED-EOF-SW
               WHEN OTHER
                   MOVE 'FEDSTMT' TO AB-DDNAME
                   MOVE WS-FEDSTMT-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-FED' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-EUC.
      *----------------------------------------------------------------*
           READ EUCSTMT-FILE INTO EUC-STMT-REC.
           EVALUATE TRUE
               WHEN EUCSTMT-OK
                   CONTINUE
               WHEN EUCSTMT-EOF
                   MOVE 'Y' TO WS-EUC-EOF-SW
               WHEN OTHER
                   MOVE 'EUCSTMT' TO AB-DDNAME
                   MOVE WS-EUCSTMT-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-EUC' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8200-WRITE-FED.
      *----------------------------------------------------------------*
           WRITE FEDOUT-REC FROM RSP-STREET-POS-REC.
           IF WS-FEDOUT-STATUS NOT = '00'
               MOVE 'FEDOUT' TO AB-DDNAME
               MOVE WS-FEDOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-WRITE-FED' TO AB-PARAGRAPH
               MOVE RSP-CUSIP TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-FED-OUT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-WRITE-EUC.
      *----------------------------------------------------------------*
           WRITE EUCOUT-REC FROM RSP-STREET-POS-REC.
           IF WS-EUCOUT-STATUS NOT = '00'
               MOVE 'EUCOUT' TO AB-DDNAME
               MOVE WS-EUCOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8300-WRITE-EUC' TO AB-PARAGRAPH
               MOVE RSP-ISIN TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-EUC-OUT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RCB110'       TO CT-STAGE.
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
      *----------------------------------------------------------------*
      * SECURITY MASTER (DB2) - NOT FOUND IS NORMAL, SQL ERROR ABENDS  *
      *----------------------------------------------------------------*
       8900-CALL-CMD010.
           MOVE ZERO   TO SL-RETURN-CODE SL-SQLCODE.
           MOVE SPACES TO SL-SEC-DATA.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA TO SEC-MASTER-REC
               WHEN SL-NOT-FOUND
                   MOVE SPACES TO SEC-MASTER-REC
               WHEN OTHER
                   MOVE SL-SQLCODE TO AB-SQLCODE WS-DISP-SQL
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '8900-CALL-CMD010' TO AB-PARAGRAPH
                   STRING SL-FUNCTION ' ' SL-KEY-CUSIP ' '
                          SL-KEY-ISIN ' SQLCODE ' WS-DISP-SQL
                          DELIMITED BY SIZE INTO AB-KEY
                   MOVE 'CMD010 SECURITY MASTER ACCESS FAILED'
                                   TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8900-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE FEDSTMT-FILE EUCSTMT-FILE.
           CLOSE FEDOUT-FILE.
           IF WS-FEDOUT-STATUS NOT = '00'
               MOVE 'FEDOUT' TO AB-DDNAME
               MOVE WS-FEDOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE EUCOUT-FILE.
           IF WS-EUCOUT-STATUS NOT = '00'
               MOVE 'EUCOUT' TO AB-DDNAME
               MOVE WS-EUCOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'FED-IN'          TO CT-COUNTER-NAME.
           MOVE WS-FED-DTL-READ   TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT.
           MOVE WS-FED-PAR-HASH   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'EUC-IN'          TO CT-COUNTER-NAME.
           MOVE WS-EUC-HLD-READ   TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT.
           MOVE WS-EUC-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'ISIN-NOTFOUND'   TO CT-COUNTER-NAME.
           MOVE WS-ISIN-NOTFOUND  TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'STPOS-FED-OUT'   TO CT-COUNTER-NAME.
           MOVE WS-FED-OUT        TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'STPOS-EUC-OUT'   TO CT-COUNTER-NAME.
           MOVE WS-EUC-OUT        TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* RCB110 - FED / EUROCLEAR STATEMENT LOAD      *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' FED STATEMENT DATE       : ' WS-FED-STMT-DATE.
           MOVE WS-FED-READ TO WS-DISP-CNT.
           DISPLAY ' FED RECORDS READ         : ' WS-DISP-CNT.
           MOVE WS-FED-DTL-READ TO WS-DISP-CNT.
           DISPLAY '   FED DETAILS            : ' WS-DISP-CNT.
           MOVE WS-FED-REJECTED TO WS-DISP-CNT.
           DISPLAY '   FED REJECTED           : ' WS-DISP-CNT.
           MOVE WS-FED-NO-SECM TO WS-DISP-CNT.
           DISPLAY '   FED CUSIP NOT ON SECM  : ' WS-DISP-CNT.
           MOVE WS-FED-OUT TO WS-DISP-CNT.
           DISPLAY '   FED POSITIONS WRITTEN  : ' WS-DISP-CNT.
           MOVE WS-FED-PAR-HASH TO WS-DISP-QTY.
           DISPLAY '   FED PAR TOTAL          : ' WS-DISP-QTY.
           DISPLAY ' EUROCLEAR STATEMENT DATE : ' WS-EUC-STMT-DATE.
           DISPLAY ' EUROCLEAR ACCOUNT        : ' WS-EUC-ACCOUNT.
           MOVE WS-EUC-READ TO WS-DISP-CNT.
           DISPLAY ' EUROCLEAR RECORDS READ   : ' WS-DISP-CNT.
           MOVE WS-EUC-HLD-READ TO WS-DISP-CNT.
           DISPLAY '   HOLDINGS               : ' WS-DISP-CNT.
           MOVE WS-EUC-REJECTED TO WS-DISP-CNT.
           DISPLAY '   REJECTED               : ' WS-DISP-CNT.
           MOVE WS-QTY-UNREADABLE TO WS-DISP-CNT.
           DISPLAY '   QTY NOT READABLE       : ' WS-DISP-CNT.
           MOVE WS-ISIN-FOUND TO WS-DISP-CNT.
           DISPLAY '   ISIN FOUND ON SECM     : ' WS-DISP-CNT.
           MOVE WS-ISIN-NOTFOUND TO WS-DISP-CNT.
           DISPLAY '   ISIN NOT FOUND         : ' WS-DISP-CNT.
           MOVE WS-NOT-EUCL-DEPO TO WS-DISP-CNT.
           DISPLAY '   SECM DEPOSITORY NOT EUCL: ' WS-DISP-CNT.
           MOVE WS-EUC-OUT TO WS-DISP-CNT.
           DISPLAY '   EUC POSITIONS WRITTEN  : ' WS-DISP-CNT.
           MOVE WS-EUC-QTY-HASH TO WS-DISP-QTY.
           DISPLAY '   EUC AGGREGATE TOTAL    : ' WS-DISP-QTY.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
                   MOVE 'FED/EUC LOADED - TRAILER OUT OF BALANCE'
                                   TO AU-MESSAGE
               WHEN WS-RETURN-CODE = 4
                   MOVE 'W' TO AU-SEVERITY
                   MOVE 'FED/EUC LOADED WITH WARNINGS' TO AU-MESSAGE
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
                   MOVE 'FED/EUC STATEMENTS LOADED' TO AU-MESSAGE
           END-EVALUATE.
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
           DISPLAY 'RCB110 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RCB110 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RCB110 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
