       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCB200.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  FEBRUARY 1997.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCB200                                            *
      * DESCRIPTION: STREET-SIDE POSITION RECONCILIATION.              *
      *              MATCHES THE DEPOSITORY STATEMENTS (DTC, FED,      *
      *              EUROCLEAR - NORMALIZED BY RCB100/RCB110) AGAINST  *
      *              THE FIRM'S BOOKS: THE LOCATION ROWS OF THE STREET *
      *              ACCOUNTS ON THE PRIOR-DAY CLOSE COPY OF THE       *
      *              POSITION MASTER.  BOTH SIDES ARE AS OF THE        *
      *              PREVIOUS BUSINESS DAY CLOSE.                      *
      *                STREETDTC0 -> DTC     STREETFED0 -> FED         *
      *                STREETEUC0 -> EUCL                              *
      *              BOOK QTY = -(SETTLED QTY) OF THE LOCATION ROWS    *
      *              (LOCATION ROWS CARRY THE NEGATIVE OF OWNERSHIP).  *
      *              RESULTS, ONE RECORD PER DEPOSITORY/CUSIP:         *
      *                OK MATCHED                                      *
      *                MS MISSING AT STREET - BOOK NOT ZERO, NO STMT   *
      *                MB MISSING IN BOOKS  - STMT, NO BOOK ROW        *
      *                QD QUANTITY DIFFERENCE (NO TOLERANCE - SHARES   *
      *                   AND PAR MUST AGREE EXACTLY)                  *
      *              THE BREAK VALUE IS THE DIFFERENCE AT THE BOOK     *
      *              VALUE PER UNIT (POS-MKT-VALUE-USD / SD QTY).      *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD030 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              STPOSIN  - MSEC.PROD.RC.STPOS.ALL(0)     (RCSTPOS)*
      *                         SORTED DEPOSITORY / CUSIP (MSRCD010)   *
      *              BOOKLOC  - MSEC.PROD.RC.BOOKLOC(+1)      (SRPOSN) *
      *                         STREET ROWS OF BKUP.POSITION(0) SORTED *
      *                         ACCOUNT / CUSIP / LOCATION (STEP010)   *
      * OUTPUT     : POSREC   - MSEC.PROD.RC.POSREC(+1)       (RCPREC) *
      * CALLS      : CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0 ALL MATCHED   4 BREAKS FOUND                    *
      *              U1006 AN INPUT IS OUT OF SEQUENCE                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1997-02-10 DWB  ORIGINAL - DTC ONLY, READ POSITION    CHG02644 *
      *                 KSDS DIRECTLY                                  *
      * 1998-01-12 TLM  FED BOOK ENTRY                        CHG03880 *
      * 1998-12-07 TLM  Y2K                                   CHG04471 *
      * 2001-05-14 KAP  QTY 4 DECIMALS                        CHG08811 *
      * 2003-10-06 KAP  EUROCLEAR, ISIN ON THE RESULT         CHG11702 *
      * 2006-06-12 KAP  BOOK SIDE FROM THE NIGHTLY BACKUP -   CHG15008 *
      *                 THE KSDS HAS TODAY'S ACTIVITY WHEN THE         *
      *                 RECON RUNS, THE STATEMENTS ARE YESTERDAY'S     *
      * 2009-12-14 SPA  BREAK VALUE IN USD                    CHG19002 *
      * 2013-05-06 SPA  WRITE MATCHED ITEMS TOO (RCB400 CLOSE CHG24790 *
      *                 LOGIC, RCR210 COUNTS)                          *
      * 2016-10-03 SPA  VALUATION FIELDS MAY BE SPACES ON     CHG30112 *
      *                 ROWS NOT YET VALUED - GUARD                    *
      * 2019-02-25 MHC  CONTROL TOTALS NAMES PER CMB090       CHG33410 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT STPOSIN-FILE   ASSIGN TO STPOSIN
                  FILE STATUS IS WS-STPOSIN-STATUS.
           SELECT BOOKLOC-FILE   ASSIGN TO BOOKLOC
                  FILE STATUS IS WS-BOOKLOC-STATUS.
           SELECT POSREC-FILE    ASSIGN TO POSREC
                  FILE STATUS IS WS-POSREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  STPOSIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RCSTPOS.
       FD  BOOKLOC-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRPOSN.
       FD  POSREC-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  POSREC-OUT-REC              PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCB200'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-STPOSIN-STATUS       PIC X(02)  VALUE '00'.
               88  STPOSIN-OK                     VALUE '00'.
               88  STPOSIN-EOF                    VALUE '10'.
           05  WS-BOOKLOC-STATUS       PIC X(02)  VALUE '00'.
               88  BOOKLOC-OK                     VALUE '00'.
               88  BOOKLOC-EOF                    VALUE '10'.
           05  WS-POSREC-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-ST-EOF-SW            PIC X(01)  VALUE 'N'.
               88  END-OF-STREET                  VALUE 'Y'.
           05  WS-BK-EOF-SW            PIC X(01)  VALUE 'N'.
               88  END-OF-BOOK                    VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * STREET ACCOUNT -> DEPOSITORY.  ORDER OF THIS TABLE FOLLOWS THE *
      * ACCOUNT NUMBER SO THE BOOK FILE (SORTED BY ACCOUNT) COMES OUT  *
      * IN DEPOSITORY ORDER.                                           *
      *----------------------------------------------------------------*
       01  WS-STREET-ACCT-VALUES.
           05  FILLER   PIC X(14)  VALUE 'STREETDTC0DTC '.
           05  FILLER   PIC X(14)  VALUE 'STREETEUC0EUCL'.
           05  FILLER   PIC X(14)  VALUE 'STREETFED0FED '.
       01  WS-STREET-ACCT-TABLE REDEFINES WS-STREET-ACCT-VALUES.
           05  WS-SA-ENTRY OCCURS 3 TIMES INDEXED BY SA-IDX.
               10  WS-SA-ACCT          PIC X(10).
               10  WS-SA-DEPO          PIC X(04).
      *----------------------------------------------------------------*
      * MATCH KEYS                                                     *
      *----------------------------------------------------------------*
       01  WS-STREET-KEY.
           05  WS-SK-DEPO              PIC X(04).
           05  WS-SK-CUSIP             PIC X(09).
       01  WS-BOOK-KEY.
           05  WS-BK-DEPO              PIC X(04).
           05  WS-BK-CUSIP             PIC X(09).
       01  WS-PREV-STREET-KEY          PIC X(13)  VALUE LOW-VALUES.
       01  WS-PREV-BOOK-ROW-KEY        PIC X(23)  VALUE LOW-VALUES.
       01  WS-NEXT-BOOK-KEY            PIC X(13).
      *----------------------------------------------------------------*
      * ONE STREET ITEM (DUPLICATE STATEMENT LINES ARE ADDED)          *
      *----------------------------------------------------------------*
       01  WS-STREET-ITEM.
           05  WS-SI-DEPO              PIC X(04).
           05  WS-SI-CUSIP             PIC X(09).
           05  WS-SI-ISIN              PIC X(12).
           05  WS-SI-STMT-DATE         PIC 9(08).
           05  WS-SI-ID-FLAG           PIC X(01).
           05  WS-SI-TOTAL             PIC S9(13)V9(04) COMP-3.
           05  WS-SI-FREE              PIC S9(13)V9(04) COMP-3.
           05  WS-SI-PLEDGED           PIC S9(13)V9(04) COMP-3.
           05  WS-SI-LINES             PIC S9(04) COMP.
       01  WS-STREET-NEXT-SW           PIC X(01)  VALUE 'N'.
           88  STREET-ITEM-READY                  VALUE 'Y'.
      *----------------------------------------------------------------*
      * ONE BOOK ITEM (ALL LOCATION ROWS OF ONE ACCOUNT/CUSIP)         *
      *----------------------------------------------------------------*
       01  WS-BOOK-ITEM.
           05  WS-BI-DEPO              PIC X(04).
           05  WS-BI-CUSIP             PIC X(09).
           05  WS-BI-ACCT              PIC X(10).
           05  WS-BI-SEC-TYPE          PIC X(02).
           05  WS-BI-CCY               PIC X(03).
           05  WS-BI-QTY               PIC S9(13)V9(04) COMP-3.
           05  WS-BI-SD-QTY            PIC S9(13)V9(04) COMP-3.
           05  WS-BI-MV-USD            PIC S9(15)V99    COMP-3.
           05  WS-BI-ROWS              PIC S9(04) COMP.
           05  WS-BI-VALUED-SW         PIC X(01).
               88  BOOK-VALUED                    VALUE 'Y'.
               88  BOOK-NOT-VALUED                VALUE 'N'.
       01  WS-BOOK-NEXT-SW             PIC X(01)  VALUE 'N'.
           88  BOOK-ITEM-READY                    VALUE 'Y'.
       01  WS-CUR-DEPO                 PIC X(04).
       01  WS-HOLD-POSITION            PIC X(200).
       01  WS-HOLD-SW                  PIC X(01)  VALUE 'N'.
           88  BOOK-ROW-HELD                      VALUE 'Y'.
      *----------------------------------------------------------------*
      * RESULT WORK                                                    *
      *----------------------------------------------------------------*
       01  WS-RESULT-WORK.
           05  WS-DIFF-QTY             PIC S9(13)V9(04) COMP-3.
           05  WS-ABS-DIFF             PIC S9(13)V9(04) COMP-3.
           05  WS-UNIT-VALUE           PIC S9(09)V9(06) COMP-3.
           05  WS-BREAK-MV             PIC S9(15)V99    COMP-3.
       01  WS-DEPO-SUB                 PIC S9(04) COMP.
       01  WS-HAS-STREET-SW            PIC X(01)  VALUE 'N'.
           88  HAS-STREET-SIDE                    VALUE 'Y'.
      *----------------------------------------------------------------*
      * STATISTICS PER DEPOSITORY                                      *
      *----------------------------------------------------------------*
       01  WS-DEPO-STATS.
           05  WS-DS-ENTRY OCCURS 3 TIMES.
               10  WS-DS-STREET-ITEMS  PIC S9(09) COMP-3.
               10  WS-DS-BOOK-ITEMS    PIC S9(09) COMP-3.
               10  WS-DS-MATCHED       PIC S9(09) COMP-3.
               10  WS-DS-MS            PIC S9(09) COMP-3.
               10  WS-DS-MB            PIC S9(09) COMP-3.
               10  WS-DS-QD            PIC S9(09) COMP-3.
               10  WS-DS-STREET-QTY    PIC S9(15)V9(04) COMP-3.
               10  WS-DS-BOOK-QTY      PIC S9(15)V9(04) COMP-3.
               10  WS-DS-BREAK-MV      PIC S9(15)V99    COMP-3.
      *----------------------------------------------------------------*
      * BREAKS BY SECURITY TYPE OF THE BOOK ROW (SYSOUT ONLY)          *
      *----------------------------------------------------------------*
       01  WS-TYPE-USED                PIC S9(04) COMP  VALUE ZERO.
       01  WS-TYPE-STATS.
           05  WS-TY OCCURS 12 TIMES INDEXED BY TY-IDX.
               10  WS-TY-SEC-TYPE      PIC X(02).
               10  WS-TY-BREAKS        PIC S9(07)    COMP-3.
               10  WS-TY-VALUE         PIC S9(15)V99 COMP-3.
       01  WS-COUNTERS.
           05  WS-ST-READ              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ST-DUPLICATES        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ST-BAD-DATE          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ST-BAD-QTY           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ST-UNKNOWN-DEPO      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BK-READ              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BK-UNKNOWN-ACCT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BK-MULTI-LOC         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BK-NOT-VALUED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BK-BAD-QTY           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BK-FLAT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-RESULTS-OUT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MATCHED-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BREAK-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MS-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MB-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-QD-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ISIN-NF-CNT          PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-ST-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-BK-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-BREAK-QTY-HASH       PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-BREAK-MV-TOTAL       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       COPY RCPREC.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-QTY             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-MATCH-MERGE THRU 2000-EXIT
               UNTIL END-OF-STREET AND END-OF-BOOK
                 AND NOT STREET-ITEM-READY AND NOT BOOK-ITEM-READY.
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
           MOVE 'STREET POSITION RECONCILIATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           INITIALIZE WS-DEPO-STATS.
           OPEN INPUT STPOSIN-FILE.
           IF WS-STPOSIN-STATUS NOT = '00'
               MOVE 'STPOSIN' TO AB-DDNAME
               MOVE WS-STPOSIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT BOOKLOC-FILE.
           IF WS-BOOKLOC-STATUS NOT = '00'
               MOVE 'BOOKLOC' TO AB-DDNAME
               MOVE WS-BOOKLOC-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT POSREC-FILE.
           IF WS-POSREC-STATUS NOT = '00'
               MOVE 'POSREC' TO AB-DDNAME
               MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-STREET THRU 8000-EXIT.
           PERFORM 3000-NEXT-STREET-ITEM THRU 3000-EXIT.
           PERFORM 8100-READ-BOOK THRU 8100-EXIT.
           PERFORM 4000-NEXT-BOOK-ITEM THRU 4000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * MATCH-MERGE ON DEPOSITORY + CUSIP                              *
      *================================================================*
       2000-MATCH-MERGE.
           IF STREET-ITEM-READY
               MOVE WS-SI-DEPO  TO WS-SK-DEPO
               MOVE WS-SI-CUSIP TO WS-SK-CUSIP
           ELSE
               MOVE HIGH-VALUES TO WS-STREET-KEY
           END-IF.
           IF BOOK-ITEM-READY
               MOVE WS-BI-DEPO  TO WS-BK-DEPO
               MOVE WS-BI-CUSIP TO WS-BK-CUSIP
           ELSE
               MOVE HIGH-VALUES TO WS-BOOK-KEY
           END-IF.
           EVALUATE TRUE
               WHEN WS-STREET-KEY = WS-BOOK-KEY
                   PERFORM 5000-BOTH-SIDES THRU 5000-EXIT
                   PERFORM 3000-NEXT-STREET-ITEM THRU 3000-EXIT
                   PERFORM 4000-NEXT-BOOK-ITEM THRU 4000-EXIT
               WHEN WS-STREET-KEY < WS-BOOK-KEY
                   PERFORM 5100-STREET-ONLY THRU 5100-EXIT
                   PERFORM 3000-NEXT-STREET-ITEM THRU 3000-EXIT
               WHEN OTHER
                   PERFORM 5200-BOOK-ONLY THRU 5200-EXIT
                   PERFORM 4000-NEXT-BOOK-ITEM THRU 4000-EXIT
           END-EVALUATE.
       2000-EXIT.
           EXIT.
      *================================================================*
      * NEXT STREET ITEM - ADDS STATEMENT LINES WITH THE SAME KEY      *
      *================================================================*
       3000-NEXT-STREET-ITEM.
           MOVE 'N' TO WS-STREET-NEXT-SW.
           IF END-OF-STREET
               GO TO 3000-EXIT
           END-IF.
           MOVE RSP-DEPOSITORY  TO WS-SI-DEPO.
           MOVE RSP-CUSIP       TO WS-SI-CUSIP.
           MOVE RSP-ISIN        TO WS-SI-ISIN.
           MOVE RSP-STMT-DATE   TO WS-SI-STMT-DATE.
           MOVE RSP-ID-FLAG     TO WS-SI-ID-FLAG.
           MOVE ZERO            TO WS-SI-TOTAL WS-SI-FREE WS-SI-PLEDGED
                                   WS-SI-LINES.
           MOVE 'Y' TO WS-STREET-NEXT-SW.
           PERFORM 3100-ADD-STREET-LINE THRU 3100-EXIT
               UNTIL END-OF-STREET
                  OR RSP-DEPOSITORY NOT = WS-SI-DEPO
                  OR RSP-CUSIP NOT = WS-SI-CUSIP.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-ADD-STREET-LINE.
      *----------------------------------------------------------------*
           ADD 1 TO WS-SI-LINES.
           IF WS-SI-LINES > 1
               ADD 1 TO WS-ST-DUPLICATES
               DISPLAY 'RCB200 STATEMENT LINES ADDED FOR ' WS-SI-DEPO
                       ' ' WS-SI-CUSIP
           END-IF.
           IF RSP-TOTAL-QTY NOT NUMERIC
               ADD 1 TO WS-ST-BAD-QTY
               DISPLAY 'RCB200 STREET QTY NOT NUMERIC ' RSP-DEPOSITORY
                       ' ' RSP-CUSIP ' - TAKEN AS ZERO'
           ELSE
               ADD RSP-TOTAL-QTY TO WS-SI-TOTAL WS-ST-QTY-HASH
               IF RSP-FREE-QTY NUMERIC
                   ADD RSP-FREE-QTY TO WS-SI-FREE
               END-IF
               IF RSP-PLEDGED-QTY NUMERIC
                   ADD RSP-PLEDGED-QTY TO WS-SI-PLEDGED
               END-IF
           END-IF.
           IF RSP-ID-FLAG = 'N'
               MOVE 'N' TO WS-SI-ID-FLAG
           END-IF.
           PERFORM 8000-READ-STREET THRU 8000-EXIT.
       3100-EXIT.
           EXIT.
      *================================================================*
      * NEXT BOOK ITEM - ALL LOCATION ROWS OF ONE STREET ACCOUNT AND   *
      * CUSIP (A SECOND LOCATION ON THE SAME ACCOUNT IS ADDED IN)      *
      *================================================================*
       4000-NEXT-BOOK-ITEM.
           MOVE 'N' TO WS-BOOK-NEXT-SW.
           IF END-OF-BOOK
               GO TO 4000-EXIT
           END-IF.
           MOVE WS-CUR-DEPO     TO WS-BI-DEPO.
           MOVE POS-CUSIP       TO WS-BI-CUSIP.
           MOVE POS-ACCT-NO     TO WS-BI-ACCT.
           MOVE POS-SEC-TYPE    TO WS-BI-SEC-TYPE.
           MOVE POS-CCY         TO WS-BI-CCY.
           MOVE ZERO            TO WS-BI-QTY WS-BI-SD-QTY WS-BI-MV-USD
                                   WS-BI-ROWS.
           MOVE 'Y'             TO WS-BI-VALUED-SW.
           MOVE 'Y' TO WS-BOOK-NEXT-SW.
           PERFORM 4100-ADD-BOOK-ROW THRU 4100-EXIT
               UNTIL END-OF-BOOK
                  OR POS-ACCT-NO NOT = WS-BI-ACCT
                  OR POS-CUSIP NOT = WS-BI-CUSIP.
           IF WS-BI-ROWS > 1
               ADD 1 TO WS-BK-MULTI-LOC
           END-IF.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-ADD-BOOK-ROW.
      *----------------------------------------------------------------*
           ADD 1 TO WS-BI-ROWS.
           ADD POS-SD-QTY TO WS-BI-SD-QTY.
           SUBTRACT POS-SD-QTY FROM WS-BI-QTY.
           SUBTRACT POS-SD-QTY FROM WS-BK-QTY-HASH.
      *    CHG30112 - ROWS NOT YET VALUED BY SRB400 HAVE SPACES IN THE
      *    PACKED VALUATION FIELDS.  CONVERTED ROWS HAVE ZEROS AND NO
      *    PRICE DATE - NOT VALUED EITHER.
           IF POS-MKT-VALUE-USD NUMERIC
           AND POS-PRICE-DATE NUMERIC
           AND POS-PRICE-DATE > ZERO
               ADD POS-MKT-VALUE-USD TO WS-BI-MV-USD
           ELSE
               MOVE 'N' TO WS-BI-VALUED-SW
               ADD 1 TO WS-BK-NOT-VALUED
           END-IF.
           PERFORM 8100-READ-BOOK THRU 8100-EXIT.
       4100-EXIT.
           EXIT.
      *================================================================*
      * BOTH SIDES PRESENT - COMPARE QUANTITIES                        *
      *================================================================*
       5000-BOTH-SIDES.
           MOVE 'Y' TO WS-HAS-STREET-SW.
           PERFORM 6000-CLEAR-RESULT THRU 6000-EXIT.
           PERFORM 6100-STREET-TO-RESULT THRU 6100-EXIT.
           PERFORM 6200-BOOK-TO-RESULT THRU 6200-EXIT.
           COMPUTE WS-DIFF-QTY = WS-SI-TOTAL - WS-BI-QTY.
           MOVE WS-DIFF-QTY TO RPR-DIFF-QTY.
           IF WS-DIFF-QTY = ZERO
               SET RPR-MATCHED TO TRUE
               MOVE ZERO TO RPR-MKT-VALUE-USD
           ELSE
               SET RPR-QTY-DIFFERENCE TO TRUE
               PERFORM 6300-BREAK-VALUE THRU 6300-EXIT
           END-IF.
           PERFORM 6900-WRITE-RESULT THRU 6900-EXIT.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STATEMENT ONLY - MISSING IN BOOKS (A ZERO STATEMENT LINE IS    *
      * NOT A BREAK)                                                   *
      *----------------------------------------------------------------*
       5100-STREET-ONLY.
           MOVE 'Y' TO WS-HAS-STREET-SW.
           PERFORM 6000-CLEAR-RESULT THRU 6000-EXIT.
           PERFORM 6100-STREET-TO-RESULT THRU 6100-EXIT.
           MOVE ZERO TO RPR-BOOK-QTY RPR-BOOK-ROWS.
           MOVE WS-SI-TOTAL TO RPR-DIFF-QTY.
           IF WS-SI-TOTAL = ZERO
               SET RPR-MATCHED TO TRUE
           ELSE
               SET RPR-MISSING-IN-BOOKS TO TRUE
      *        NO BOOK ROW - NO VALUE PER UNIT
               MOVE ZERO TO RPR-MKT-VALUE-USD
               MOVE 'N'  TO RPR-VALUE-FLAG
           END-IF.
           PERFORM 6900-WRITE-RESULT THRU 6900-EXIT.
       5100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BOOKS ONLY - MISSING AT STREET (A FLAT LOCATION ROW IS NOT A   *
      * BREAK)                                                         *
      *----------------------------------------------------------------*
       5200-BOOK-ONLY.
           MOVE 'N' TO WS-HAS-STREET-SW.
           PERFORM 6000-CLEAR-RESULT THRU 6000-EXIT.
           PERFORM 6200-BOOK-TO-RESULT THRU 6200-EXIT.
           MOVE WS-BI-DEPO  TO RPR-DEPOSITORY.
           MOVE WS-BI-CUSIP TO RPR-CUSIP.
           MOVE ZERO        TO RPR-STREET-QTY RPR-STREET-FREE
                               RPR-STREET-PLEDGED.
           COMPUTE WS-DIFF-QTY = ZERO - WS-BI-QTY.
           MOVE WS-DIFF-QTY TO RPR-DIFF-QTY.
           IF WS-BI-QTY = ZERO
               ADD 1 TO WS-BK-FLAT
               SET RPR-MATCHED TO TRUE
           ELSE
               SET RPR-MISSING-AT-STREET TO TRUE
               PERFORM 6300-BREAK-VALUE THRU 6300-EXIT
           END-IF.
           PERFORM 6900-WRITE-RESULT THRU 6900-EXIT.
       5200-EXIT.
           EXIT.
      *================================================================*
      * RESULT RECORD                                                  *
      *================================================================*
       6000-CLEAR-RESULT.
           MOVE SPACES           TO RPR-POSREC-REC.
           MOVE DC-BUS-DATE      TO RPR-BUS-DATE.
           MOVE DC-PREV-BUS-DATE TO RPR-STMT-DATE.
           MOVE ZERO             TO RPR-STREET-QTY RPR-BOOK-QTY
                                    RPR-DIFF-QTY RPR-MKT-VALUE-USD
                                    RPR-STREET-FREE RPR-STREET-PLEDGED
                                    RPR-BOOK-ROWS.
           MOVE 'Y'              TO RPR-VALUE-FLAG.
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6100-STREET-TO-RESULT.
      *----------------------------------------------------------------*
           MOVE WS-SI-DEPO       TO RPR-DEPOSITORY.
           MOVE WS-SI-CUSIP      TO RPR-CUSIP.
           MOVE WS-SI-ISIN       TO RPR-ISIN.
           IF WS-SI-STMT-DATE NUMERIC AND WS-SI-STMT-DATE > ZERO
               MOVE WS-SI-STMT-DATE TO RPR-STMT-DATE
           END-IF.
           MOVE WS-SI-TOTAL      TO RPR-STREET-QTY.
           MOVE WS-SI-FREE       TO RPR-STREET-FREE.
           MOVE WS-SI-PLEDGED    TO RPR-STREET-PLEDGED.
           MOVE WS-SI-ID-FLAG    TO RPR-ID-FLAG.
           IF WS-SI-ID-FLAG = 'N'
               ADD 1 TO WS-ISIN-NF-CNT
           END-IF.
       6100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6200-BOOK-TO-RESULT.
      *----------------------------------------------------------------*
           MOVE WS-BI-QTY        TO RPR-BOOK-QTY.
           MOVE WS-BI-ACCT       TO RPR-BOOK-ACCT.
           MOVE WS-BI-ROWS       TO RPR-BOOK-ROWS.
           MOVE WS-BI-SEC-TYPE   TO RPR-SEC-TYPE.
           MOVE WS-BI-CCY        TO RPR-CCY.
       6200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BREAK VALUE = |DIFF| x (BOOK MV USD / BOOK SD QTY)             *
      *----------------------------------------------------------------*
       6300-BREAK-VALUE.
           MOVE ZERO TO WS-BREAK-MV WS-UNIT-VALUE.
           IF BOOK-NOT-VALUED OR WS-BI-SD-QTY = ZERO
               MOVE 'N' TO RPR-VALUE-FLAG
               MOVE ZERO TO RPR-MKT-VALUE-USD
               GO TO 6300-EXIT
           END-IF.
           COMPUTE WS-UNIT-VALUE ROUNDED = WS-BI-MV-USD / WS-BI-SD-QTY
               ON SIZE ERROR
                   MOVE ZERO TO WS-UNIT-VALUE
                   MOVE 'N' TO RPR-VALUE-FLAG
           END-COMPUTE.
           IF WS-DIFF-QTY < ZERO
               COMPUTE WS-ABS-DIFF = WS-DIFF-QTY * -1
           ELSE
               MOVE WS-DIFF-QTY TO WS-ABS-DIFF
           END-IF.
           COMPUTE WS-BREAK-MV ROUNDED = WS-ABS-DIFF * WS-UNIT-VALUE
               ON SIZE ERROR
                   MOVE ZERO TO WS-BREAK-MV
                   MOVE 'N' TO RPR-VALUE-FLAG
           END-COMPUTE.
           IF WS-BREAK-MV < ZERO
               COMPUTE WS-BREAK-MV = WS-BREAK-MV * -1
           END-IF.
           MOVE WS-BREAK-MV TO RPR-MKT-VALUE-USD.
       6300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6900-WRITE-RESULT.
      *----------------------------------------------------------------*
           PERFORM 6950-DEPO-INDEX THRU 6950-EXIT.
           IF HAS-STREET-SIDE AND WS-DEPO-SUB > ZERO
               ADD 1 TO WS-DS-STREET-ITEMS (WS-DEPO-SUB)
           END-IF.
           IF WS-DEPO-SUB > ZERO
               ADD RPR-STREET-QTY TO WS-DS-STREET-QTY (WS-DEPO-SUB)
               ADD RPR-BOOK-QTY   TO WS-DS-BOOK-QTY (WS-DEPO-SUB)
               IF RPR-BOOK-ROWS > ZERO
                   ADD 1 TO WS-DS-BOOK-ITEMS (WS-DEPO-SUB)
               END-IF
           END-IF.
           EVALUATE TRUE
               WHEN RPR-MATCHED
                   ADD 1 TO WS-MATCHED-CNT
                   IF WS-DEPO-SUB > ZERO
                       ADD 1 TO WS-DS-MATCHED (WS-DEPO-SUB)
                   END-IF
               WHEN RPR-MISSING-AT-STREET
                   ADD 1 TO WS-MS-CNT
                   IF WS-DEPO-SUB > ZERO
                       ADD 1 TO WS-DS-MS (WS-DEPO-SUB)
                   END-IF
               WHEN RPR-MISSING-IN-BOOKS
                   ADD 1 TO WS-MB-CNT
                   IF WS-DEPO-SUB > ZERO
                       ADD 1 TO WS-DS-MB (WS-DEPO-SUB)
                   END-IF
               WHEN RPR-QTY-DIFFERENCE
                   ADD 1 TO WS-QD-CNT
                   IF WS-DEPO-SUB > ZERO
                       ADD 1 TO WS-DS-QD (WS-DEPO-SUB)
                   END-IF
           END-EVALUATE.
           IF RPR-IS-BREAK
               PERFORM 6960-TYPE-STATISTICS THRU 6960-EXIT
               ADD 1 TO WS-BREAK-CNT
               ADD RPR-MKT-VALUE-USD TO WS-BREAK-MV-TOTAL
               IF WS-DEPO-SUB > ZERO
                   ADD RPR-MKT-VALUE-USD
                                   TO WS-DS-BREAK-MV (WS-DEPO-SUB)
               END-IF
               IF RPR-DIFF-QTY < ZERO
                   SUBTRACT RPR-DIFF-QTY FROM WS-BREAK-QTY-HASH
               ELSE
                   ADD RPR-DIFF-QTY TO WS-BREAK-QTY-HASH
               END-IF
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           WRITE POSREC-OUT-REC FROM RPR-POSREC-REC.
           IF WS-POSREC-STATUS NOT = '00'
               MOVE 'POSREC' TO AB-DDNAME
               MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '6900-WRITE-RESULT' TO AB-PARAGRAPH
               STRING RPR-DEPOSITORY RPR-CUSIP
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-RESULTS-OUT.
       6900-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6950-DEPO-INDEX.
      *----------------------------------------------------------------*
           MOVE ZERO TO WS-DEPO-SUB.
           SET SA-IDX TO 1.
           SEARCH WS-SA-ENTRY
               AT END
                   CONTINUE
               WHEN WS-SA-DEPO (SA-IDX) = RPR-DEPOSITORY
                   SET WS-DEPO-SUB TO SA-IDX
           END-SEARCH.
       6950-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MB BREAKS HAVE NO BOOK ROW - THEY COUNT UNDER TYPE '  '        *
      *----------------------------------------------------------------*
       6960-TYPE-STATISTICS.
           SET TY-IDX TO 1.
           SEARCH WS-TY
               AT END
                   DISPLAY 'RCB200 MORE THAN 12 SECURITY TYPES IN BREAK'
               WHEN TY-IDX > WS-TYPE-USED
                   ADD 1 TO WS-TYPE-USED
                   MOVE RPR-SEC-TYPE      TO WS-TY-SEC-TYPE (TY-IDX)
                   MOVE 1                 TO WS-TY-BREAKS (TY-IDX)
                   MOVE RPR-MKT-VALUE-USD TO WS-TY-VALUE (TY-IDX)
               WHEN WS-TY-SEC-TYPE (TY-IDX) = RPR-SEC-TYPE
                   ADD 1                  TO WS-TY-BREAKS (TY-IDX)
                   ADD RPR-MKT-VALUE-USD  TO WS-TY-VALUE (TY-IDX)
           END-SEARCH.
       6960-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-STREET.
           READ STPOSIN-FILE.
           EVALUATE TRUE
               WHEN STPOSIN-OK
                   ADD 1 TO WS-ST-READ
               WHEN STPOSIN-EOF
                   MOVE 'Y' TO WS-ST-EOF-SW
                   GO TO 8000-EXIT
               WHEN OTHER
                   MOVE 'STPOSIN' TO AB-DDNAME
                   MOVE WS-STPOSIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-STREET' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
      *    SEQUENCE CHECK - STPOS.ALL IS SORTED BY MSRCD010
           IF RSP-STREET-POS-REC (1:13) < WS-PREV-STREET-KEY
               MOVE 'STPOSIN' TO AB-DDNAME
               MOVE 1006 TO AB-ABEND-CODE
               MOVE '8000-READ-STREET' TO AB-PARAGRAPH
               STRING 'PREV ' WS-PREV-STREET-KEY ' CURR '
                      RSP-STREET-POS-REC (1:13)
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'STREET POSITIONS NOT IN DEPOSITORY/CUSIP SEQUENCE'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE RSP-STREET-POS-REC (1:13) TO WS-PREV-STREET-KEY.
           SET SA-IDX TO 1.
           SEARCH WS-SA-ENTRY
               AT END
                   ADD 1 TO WS-ST-UNKNOWN-DEPO
                   DISPLAY 'RCB200 UNKNOWN DEPOSITORY ' RSP-DEPOSITORY
                           ' CUSIP ' RSP-CUSIP ' - RECONCILED ANYWAY'
               WHEN WS-SA-DEPO (SA-IDX) = RSP-DEPOSITORY
                   CONTINUE
           END-SEARCH.
           IF RSP-STMT-DATE NOT = DC-PREV-BUS-DATE
               ADD 1 TO WS-ST-BAD-DATE
           END-IF.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BOOK ROWS - ONLY THE THREE STREET ACCOUNTS ARE RECONCILED.     *
      * THE DEPOSITORY OF THE ROW COMES FROM THE ACCOUNT, NOT FROM     *
      * POS-LOCATION (BOX AND SEG ROWS SIT ON STREETDTC0).             *
      *----------------------------------------------------------------*
       8100-READ-BOOK.
           READ BOOKLOC-FILE.
           EVALUATE TRUE
               WHEN BOOKLOC-OK
                   ADD 1 TO WS-BK-READ
               WHEN BOOKLOC-EOF
                   MOVE 'Y' TO WS-BK-EOF-SW
                   GO TO 8100-EXIT
               WHEN OTHER
                   MOVE 'BOOKLOC' TO AB-DDNAME
                   MOVE WS-BOOKLOC-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-BOOK' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF POS-KEY < WS-PREV-BOOK-ROW-KEY
               MOVE 'BOOKLOC' TO AB-DDNAME
               MOVE 1006 TO AB-ABEND-CODE
               MOVE '8100-READ-BOOK' TO AB-PARAGRAPH
               STRING 'PREV ' WS-PREV-BOOK-ROW-KEY ' CURR ' POS-KEY
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'BOOK LOCATION ROWS NOT IN ACCOUNT/CUSIP SEQUENCE'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE POS-KEY TO WS-PREV-BOOK-ROW-KEY.
           IF POS-SD-QTY NOT NUMERIC
               ADD 1 TO WS-BK-BAD-QTY
               DISPLAY 'RCB200 BOOK SD QTY NOT NUMERIC ' POS-KEY
                       ' - TAKEN AS ZERO'
               MOVE ZERO TO POS-SD-QTY
           END-IF.
           SET SA-IDX TO 1.
           SEARCH WS-SA-ENTRY
               AT END
                   ADD 1 TO WS-BK-UNKNOWN-ACCT
                   DISPLAY 'RCB200 STREET ACCOUNT ' POS-ACCT-NO
                           ' HAS NO DEPOSITORY - ROW SKIPPED'
                   GO TO 8100-READ-BOOK
               WHEN WS-SA-ACCT (SA-IDX) = POS-ACCT-NO
                   MOVE WS-SA-DEPO (SA-IDX) TO WS-CUR-DEPO
           END-SEARCH.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RCB200'       TO CT-STAGE.
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
           CLOSE STPOSIN-FILE BOOKLOC-FILE.
           CLOSE POSREC-FILE.
           IF WS-POSREC-STATUS NOT = '00'
               MOVE 'POSREC' TO AB-DDNAME
               MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'STREET-IN'       TO CT-COUNTER-NAME.
           MOVE WS-ST-READ        TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT.
           MOVE WS-ST-QTY-HASH    TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BOOK-IN'         TO CT-COUNTER-NAME.
           MOVE WS-BK-READ        TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT.
           MOVE WS-BK-QTY-HASH    TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'POSREC-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-RESULTS-OUT    TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BREAKS-FOUND'    TO CT-COUNTER-NAME.
           MOVE WS-BREAK-CNT      TO CT-COUNT.
           MOVE WS-BREAK-MV-TOTAL TO CT-AMOUNT.
           MOVE WS-BREAK-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           PERFORM 9100-DISPLAY-STATISTICS THRU 9100-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           IF WS-BREAK-CNT > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'STREET POSITION BREAKS FOUND - SEE RCR210'
                               TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'STREET POSITIONS RECONCILED - NO BREAKS'
                               TO AU-MESSAGE
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
           DISPLAY '* RCB200 - STREET POSITION RECONCILIATION      *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' POSITIONS AS OF          : ' DC-PREV-BUS-DATE.
           MOVE WS-ST-READ TO WS-DISP-CNT.
           DISPLAY ' STATEMENT LINES READ     : ' WS-DISP-CNT.
           MOVE WS-ST-DUPLICATES TO WS-DISP-CNT.
           DISPLAY '   ADDED TO PREVIOUS LINE : ' WS-DISP-CNT.
           MOVE WS-ST-BAD-DATE TO WS-DISP-CNT.
           DISPLAY '   OTHER STATEMENT DATE   : ' WS-DISP-CNT.
           MOVE WS-ST-BAD-QTY TO WS-DISP-CNT.
           DISPLAY '   QTY NOT NUMERIC        : ' WS-DISP-CNT.
           MOVE WS-ST-UNKNOWN-DEPO TO WS-DISP-CNT.
           DISPLAY '   UNKNOWN DEPOSITORY     : ' WS-DISP-CNT.
           MOVE WS-BK-READ TO WS-DISP-CNT.
           DISPLAY ' BOOK LOCATION ROWS READ  : ' WS-DISP-CNT.
           MOVE WS-BK-UNKNOWN-ACCT TO WS-DISP-CNT.
           DISPLAY '   NOT A RECONCILED ACCT  : ' WS-DISP-CNT.
           MOVE WS-BK-MULTI-LOC TO WS-DISP-CNT.
           DISPLAY '   CUSIPS ON 2+ LOCATIONS : ' WS-DISP-CNT.
           MOVE WS-BK-FLAT TO WS-DISP-CNT.
           DISPLAY '   FLAT, NO STATEMENT     : ' WS-DISP-CNT.
           MOVE WS-BK-NOT-VALUED TO WS-DISP-CNT.
           DISPLAY '   ROWS WITH NO VALUATION : ' WS-DISP-CNT.
           MOVE WS-BK-BAD-QTY TO WS-DISP-CNT.
           DISPLAY '   QTY NOT NUMERIC        : ' WS-DISP-CNT.
           MOVE WS-RESULTS-OUT TO WS-DISP-CNT.
           DISPLAY ' ITEMS COMPARED           : ' WS-DISP-CNT.
           MOVE WS-MATCHED-CNT TO WS-DISP-CNT.
           DISPLAY '   MATCHED                : ' WS-DISP-CNT.
           MOVE WS-MS-CNT TO WS-DISP-CNT.
           DISPLAY '   MS MISSING AT STREET   : ' WS-DISP-CNT.
           MOVE WS-MB-CNT TO WS-DISP-CNT.
           DISPLAY '   MB MISSING IN BOOKS    : ' WS-DISP-CNT.
           MOVE WS-QD-CNT TO WS-DISP-CNT.
           DISPLAY '   QD QUANTITY DIFFERENCE : ' WS-DISP-CNT.
           MOVE WS-ISIN-NF-CNT TO WS-DISP-CNT.
           DISPLAY '   ISIN NOT ON MASTER     : ' WS-DISP-CNT.
           PERFORM VARYING WS-DEPO-SUB FROM 1 BY 1
                   UNTIL WS-DEPO-SUB > 3
               DISPLAY ' DEPOSITORY ' WS-SA-DEPO (WS-DEPO-SUB)
                       ' (' WS-SA-ACCT (WS-DEPO-SUB) ')'
               MOVE WS-DS-MATCHED (WS-DEPO-SUB) TO WS-DISP-CNT
               DISPLAY '   MATCHED                : ' WS-DISP-CNT
               MOVE WS-DS-MS (WS-DEPO-SUB) TO WS-DISP-CNT
               DISPLAY '   MS                     : ' WS-DISP-CNT
               MOVE WS-DS-MB (WS-DEPO-SUB) TO WS-DISP-CNT
               DISPLAY '   MB                     : ' WS-DISP-CNT
               MOVE WS-DS-QD (WS-DEPO-SUB) TO WS-DISP-CNT
               DISPLAY '   QD                     : ' WS-DISP-CNT
               MOVE WS-DS-STREET-QTY (WS-DEPO-SUB) TO WS-DISP-QTY
               DISPLAY '   STREET QTY             : ' WS-DISP-QTY
               MOVE WS-DS-BOOK-QTY (WS-DEPO-SUB) TO WS-DISP-QTY
               DISPLAY '   BOOK QTY               : ' WS-DISP-QTY
               MOVE WS-DS-BREAK-MV (WS-DEPO-SUB) TO WS-DISP-AMT
               DISPLAY '   BREAK VALUE USD        : ' WS-DISP-AMT
           END-PERFORM.
           PERFORM VARYING TY-IDX FROM 1 BY 1
                   UNTIL TY-IDX > WS-TYPE-USED
               MOVE WS-TY-BREAKS (TY-IDX) TO WS-DISP-CNT
               MOVE WS-TY-VALUE (TY-IDX)  TO WS-DISP-AMT
               DISPLAY ' BREAKS SEC TYPE ''' WS-TY-SEC-TYPE (TY-IDX)
                       '''     : ' WS-DISP-CNT ' ' WS-DISP-AMT
           END-PERFORM.
           MOVE WS-BREAK-MV-TOTAL TO WS-DISP-AMT.
           DISPLAY ' TOTAL BREAK VALUE USD    : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
       9100-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RCB200 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RCB200 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RCB200 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
