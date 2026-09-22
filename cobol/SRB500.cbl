       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB500.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 1988.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB500                                            *
      * DESCRIPTION: STOCK RECORD BALANCING.                           *
      *              READS THE POSITION UNLOAD SORTED BY CUSIP /       *
      *              LOCATION / ACCOUNT AND PROVES, FOR EVERY CUSIP,   *
      *              THAT OWNERSHIP (CLIENT + FIRM ACCOUNTS, LONG)     *
      *              EQUALS LOCATION (STREET ACCOUNTS, SHORT):         *
      *                SETTLED:     OWNER SD + LOCATION SD = 0         *
      *                TRADE DATE:  OWNER TD + LOCATION TD = 0         *
      *              FIRM INVENTORY WITH A NEGATIVE SETTLED QUANTITY   *
      *              IS REPORTED AS FIRM SHORT.                        *
      *              BREAKS ARE AGED AGAINST THE PREVIOUS BREAK FILE   *
      *              (MATCHED ON CUSIP AND BREAK TYPE).                *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD060 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              POSNIN   - MSEC.PROD.SR.POSN.SORTED(+1)  (SRPOSN) *
      *              PRVBRK   - MSEC.PROD.SR.BREAKS(0)       (SRBREAK) *
      * OUTPUT     : BRKOUT   - MSEC.PROD.SR.BREAKS(+1)      (SRBREAK) *
      *              BRKDTL   - MSEC.PROD.SR.BREAKS.DETAIL(+1)(SRBRKD) *
      * CALLS      : CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0 IN BALANCE, 4 BREAKS FOUND                      *
      *              ABEND U1006 IF THE INPUT IS OUT OF CUSIP SEQUENCE *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1988-03-07 RJK  ORIGINAL                                       *
      * 1989-03-20 RJK  DATE CARD INSTEAD OF SYSTEM DATE      CHG00212 *
      * 1990-08-13 DWB  SEQUENCE CHECK - SEE INCIDENT 90-114  CHG00702 *
      * 1991-06-03 DWB  TRADE DATE BREAKS                     CHG00987 *
      * 1994-01-24 DWB  BREAK AGING FROM PRIOR BREAK FILE     CHG01790 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2002-09-16 KAP  FIRM SHORT DETECTION                  CHG10012 *
      * 2009-12-14 SPA  BREAK MARKET VALUE IN USD             CHG19002 *
      * 2013-05-06 SPA  BREAK DETAIL FILE FOR SRR510          CHG24790 *
      * 2016-10-03 SPA  OMNIBUS ACCOUNTS COUNT AS CLIENTS     CHG30112 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT POSNIN-FILE    ASSIGN TO POSNIN
                  FILE STATUS IS WS-POSNIN-STATUS.
           SELECT PRVBRK-FILE    ASSIGN TO PRVBRK
                  FILE STATUS IS WS-PRVBRK-STATUS.
           SELECT BRKOUT-FILE    ASSIGN TO BRKOUT
                  FILE STATUS IS WS-BRKOUT-STATUS.
           SELECT BRKDTL-FILE    ASSIGN TO BRKDTL
                  FILE STATUS IS WS-BRKDTL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  POSNIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRPOSN.
       FD  PRVBRK-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PRVBRK-REC                  PIC X(150).
       FD  BRKOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BRKOUT-REC                  PIC X(150).
       FD  BRKDTL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  BRKDTL-REC                  PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB500'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-POSNIN-STATUS        PIC X(02)  VALUE '00'.
               88  POSNIN-OK                      VALUE '00'.
               88  POSNIN-EOF                     VALUE '10'.
           05  WS-PRVBRK-STATUS        PIC X(02)  VALUE '00'.
               88  PRVBRK-OK                      VALUE '00'.
               88  PRVBRK-EOF                     VALUE '10'.
           05  WS-BRKOUT-STATUS        PIC X(02)  VALUE '00'.
           05  WS-BRKDTL-STATUS        PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-POSITIONS               VALUE 'Y'.
           05  WS-PRV-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-PREV-BREAKS             VALUE 'Y'.
           05  WS-PRV-OPEN-SW          PIC X(01)  VALUE 'N'.
               88  PREV-BREAKS-OPEN               VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
               88  FIRST-POSITION                 VALUE 'Y'.
           05  WS-DTL-OVERFLOW-SW      PIC X(01)  VALUE 'N'.
               88  DETAIL-OVERFLOW                VALUE 'Y'.
           05  WS-DTL-WRITTEN-SW       PIC X(01)  VALUE 'N'.
               88  DETAIL-WRITTEN                 VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-CUR-CUSIP                PIC X(09)  VALUE SPACES.
       01  WS-PREV-CUSIP               PIC X(09)  VALUE LOW-VALUES.
       01  WS-ROW-ROLE                 PIC X(01).
           88  ROLE-CLIENT                        VALUE 'C'.
           88  ROLE-FIRM                          VALUE 'F'.
           88  ROLE-LOCATION                      VALUE 'L'.
      *----------------------------------------------------------------*
      * PER-CUSIP ACCUMULATORS                                         *
      *----------------------------------------------------------------*
       01  WS-CUSIP-TOTALS.
           05  WS-CT-SEC-TYPE          PIC X(02).
           05  WS-CT-OWNER-SD          PIC S9(13)V9(04) COMP-3.
           05  WS-CT-OWNER-TD          PIC S9(13)V9(04) COMP-3.
           05  WS-CT-FIRM-SD           PIC S9(13)V9(04) COMP-3.
           05  WS-CT-FIRM-TD           PIC S9(13)V9(04) COMP-3.
           05  WS-CT-LOC-SD            PIC S9(13)V9(04) COMP-3.
           05  WS-CT-LOC-TD            PIC S9(13)V9(04) COMP-3.
           05  WS-CT-OWNER-CNT         PIC S9(07)       COMP-3.
           05  WS-CT-LOC-CNT           PIC S9(07)       COMP-3.
           05  WS-CT-OWNER-MV-USD      PIC S9(15)V99    COMP-3.
       01  WS-BREAK-WORK.
           05  WS-SD-DIFF              PIC S9(13)V9(04) COMP-3.
           05  WS-TD-DIFF              PIC S9(13)V9(04) COMP-3.
           05  WS-UNIT-MV              PIC S9(09)V9(08) COMP-3.
           05  WS-BREAK-MV             PIC S9(15)V99    COMP-3.
           05  WS-BREAK-AGE            PIC S9(03)       COMP-3.
           05  WS-BREAKS-THIS-CUSIP    PIC S9(03)       COMP-3.
      *----------------------------------------------------------------*
      * POSITION ROWS OF THE CURRENT CUSIP (FOR THE DETAIL FILE)       *
      *----------------------------------------------------------------*
       01  WS-ROW-TABLE.
           05  WS-ROW-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-ROW-MAX              PIC S9(04) COMP  VALUE +2000.
           05  WS-ROW OCCURS 2000 TIMES INDEXED BY RW-IDX.
               10  WS-RW-ACCT-NO       PIC X(10).
               10  WS-RW-LOCATION      PIC X(04).
               10  WS-RW-ACCT-TYPE     PIC X(02).
               10  WS-RW-ROLE          PIC X(01).
               10  WS-RW-TD-QTY        PIC S9(11)V9(04) COMP-3.
               10  WS-RW-SD-QTY        PIC S9(11)V9(04) COMP-3.
               10  WS-RW-PEND-IN       PIC S9(11)V9(04) COMP-3.
               10  WS-RW-PEND-OUT      PIC S9(11)V9(04) COMP-3.
               10  WS-RW-LAST-ACTV     PIC 9(08).
      *----------------------------------------------------------------*
      * PREVIOUS BREAKS FOR THE CURRENT CUSIP                          *
      *----------------------------------------------------------------*
       01  WS-PRV-TABLE.
           05  WS-PRV-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-PRV OCCURS 10 TIMES INDEXED BY PV-IDX.
               10  WS-PV-TYPE          PIC X(02).
               10  WS-PV-AGE           PIC S9(03)       COMP-3.
               10  WS-PV-MATCHED       PIC X(01).
       01  WS-PRV-BREAK.
           05  WS-PB-BUS-DATE          PIC 9(08).
           05  WS-PB-CUSIP             PIC X(09).
           05  WS-PB-SEC-TYPE          PIC X(02).
           05  WS-PB-TYPE              PIC X(02).
           05  FILLER                  PIC X(53).
           05  WS-PB-AGE-DAYS          PIC S9(03)       COMP-3.
           05  FILLER                  PIC X(74).
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CUSIP-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BALANCED-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CUSIP-BREAK-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SD-BREAK-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TD-BREAK-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FS-BREAK-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BREAK-OUT-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-BREAK-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AGED-BREAK-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-RESOLVED-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PRV-READ-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DTL-OUT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CLIENT-ROWS          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FIRM-ROWS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LOC-ROWS             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAT-ROWS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BAD-MV-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OVERFLOW-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MAX-AGE              PIC S9(03) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-OWNER-SD-HASH        PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-LOC-SD-HASH          PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-OWNER-MV-HASH        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-BREAK-MV-TOTAL       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-BREAK-QTY-HASH       PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
       01  WS-ABS-DIFF                 PIC S9(13)V9(04) COMP-3.
       COPY SRBREAK.
       COPY SRBRKD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-QTY             PIC -ZZ,ZZZ,ZZZ,ZZ9.9999.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-POSITION THRU 2000-EXIT
               UNTIL END-OF-POSITIONS.
           IF NOT FIRST-POSITION
               PERFORM 3000-CUSIP-BREAK THRU 3000-EXIT
           END-IF.
           PERFORM 6900-DRAIN-PREV-BREAKS THRU 6900-EXIT.
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
           MOVE 'STOCK RECORD BALANCING STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT POSNIN-FILE.
           IF WS-POSNIN-STATUS NOT = '00'
               MOVE 'POSNIN' TO AB-DDNAME
               MOVE WS-POSNIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
      *    PRIOR BREAKS ARE OPTIONAL (FIRST RUN, DD DUMMY)
           OPEN INPUT PRVBRK-FILE.
           IF WS-PRVBRK-STATUS = '00'
               MOVE 'Y' TO WS-PRV-OPEN-SW
               PERFORM 8100-READ-PREV-BREAK THRU 8100-EXIT
           ELSE
               DISPLAY 'SRB500 NO PREVIOUS BREAK FILE (STATUS '
                       WS-PRVBRK-STATUS ') - ALL BREAKS AGE 1'
               MOVE 'Y' TO WS-PRV-EOF-SW
           END-IF.
           OPEN OUTPUT BRKOUT-FILE.
           IF WS-BRKOUT-STATUS NOT = '00'
               MOVE 'BRKOUT' TO AB-DDNAME
               MOVE WS-BRKOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT BRKDTL-FILE.
           IF WS-BRKDTL-STATUS NOT = '00'
               MOVE 'BRKDTL' TO AB-DDNAME
               MOVE WS-BRKDTL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 2900-CLEAR-CUSIP THRU 2900-EXIT.
           PERFORM 8000-READ-POSITION THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *================================================================*
      * ONE POSITION ROW                                               *
      *================================================================*
       2000-PROCESS-POSITION.
           ADD 1 TO WS-READ-CNT.
           MOVE POS-CUSIP TO WS-CUR-CUSIP.
      *    ---- SEQUENCE CHECK (CHG00702) ---------------------------
           IF WS-CUR-CUSIP < WS-PREV-CUSIP
               MOVE 1006 TO AB-ABEND-CODE
               MOVE '2000-PROCESS-POSITION' TO AB-PARAGRAPH
               MOVE 'POSNIN' TO AB-DDNAME
               STRING 'PREV ' WS-PREV-CUSIP ' CURR ' WS-CUR-CUSIP
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'POSITION UNLOAD NOT IN CUSIP SEQUENCE'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF FIRST-POSITION
               MOVE 'N' TO WS-FIRST-SW
           ELSE
               IF WS-CUR-CUSIP NOT = WS-PREV-CUSIP
                   PERFORM 3000-CUSIP-BREAK THRU 3000-EXIT
               END-IF
           END-IF.
           MOVE WS-CUR-CUSIP TO WS-PREV-CUSIP.
           IF WS-CT-SEC-TYPE = SPACES
               MOVE POS-SEC-TYPE TO WS-CT-SEC-TYPE
           END-IF.
           IF POS-TD-QTY = ZERO AND POS-SD-QTY = ZERO
               ADD 1 TO WS-FLAT-ROWS
           END-IF.
           PERFORM 2100-CLASSIFY-ROW THRU 2100-EXIT.
           PERFORM 2200-ACCUMULATE-ROW THRU 2200-EXIT.
           PERFORM 2300-SAVE-ROW THRU 2300-EXIT.
           PERFORM 8000-READ-POSITION THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * OWNER OR LOCATION.  FIRM ACCTS SORT LOW (FIRMINVNNN, STREETXXX *
      * BEFORE THE NUMERIC CLIENT ACCOUNTS).                           *
      *----------------------------------------------------------------*
       2100-CLASSIFY-ROW.
           IF POS-ACCT-NO (1:1) < '0'
               IF POS-ACCT-TYPE = 'ST'
                   SET ROLE-LOCATION TO TRUE
               ELSE
                   SET ROLE-FIRM TO TRUE
               END-IF
           ELSE
               SET ROLE-CLIENT TO TRUE
           END-IF.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-ACCUMULATE-ROW.
      *----------------------------------------------------------------*
           EVALUATE TRUE
               WHEN ROLE-LOCATION
                   ADD 1          TO WS-CT-LOC-CNT WS-LOC-ROWS
                   ADD POS-SD-QTY TO WS-CT-LOC-SD WS-LOC-SD-HASH
                   ADD POS-TD-QTY TO WS-CT-LOC-TD
               WHEN ROLE-FIRM
                   ADD 1          TO WS-CT-OWNER-CNT WS-FIRM-ROWS
                   ADD POS-SD-QTY TO WS-CT-OWNER-SD WS-CT-FIRM-SD
                                     WS-OWNER-SD-HASH
                   ADD POS-TD-QTY TO WS-CT-OWNER-TD WS-CT-FIRM-TD
                   PERFORM 2250-ADD-OWNER-MV THRU 2250-EXIT
               WHEN OTHER
                   ADD 1          TO WS-CT-OWNER-CNT WS-CLIENT-ROWS
                   ADD POS-SD-QTY TO WS-CT-OWNER-SD WS-OWNER-SD-HASH
                   ADD POS-TD-QTY TO WS-CT-OWNER-TD
                   PERFORM 2250-ADD-OWNER-MV THRU 2250-EXIT
           END-EVALUATE.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2250-ADD-OWNER-MV.
      *----------------------------------------------------------------*
           IF POS-MKT-VALUE-USD NUMERIC
               ADD POS-MKT-VALUE-USD TO WS-CT-OWNER-MV-USD
                                        WS-OWNER-MV-HASH
           ELSE
               ADD 1 TO WS-BAD-MV-CNT
           END-IF.
       2250-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-SAVE-ROW.
      *----------------------------------------------------------------*
           IF WS-ROW-USED NOT < WS-ROW-MAX
               IF NOT DETAIL-OVERFLOW
                   ADD 1 TO WS-OVERFLOW-CNT
                   DISPLAY 'SRB500 DETAIL TABLE FULL FOR CUSIP '
                           WS-CUR-CUSIP ' - DETAIL TRUNCATED'
               END-IF
               MOVE 'Y' TO WS-DTL-OVERFLOW-SW
               GO TO 2300-EXIT
           END-IF.
           ADD 1 TO WS-ROW-USED.
           SET RW-IDX TO WS-ROW-USED.
           MOVE POS-ACCT-NO        TO WS-RW-ACCT-NO (RW-IDX).
           MOVE POS-LOCATION       TO WS-RW-LOCATION (RW-IDX).
           MOVE POS-ACCT-TYPE      TO WS-RW-ACCT-TYPE (RW-IDX).
           MOVE WS-ROW-ROLE        TO WS-RW-ROLE (RW-IDX).
           MOVE POS-TD-QTY         TO WS-RW-TD-QTY (RW-IDX).
           MOVE POS-SD-QTY         TO WS-RW-SD-QTY (RW-IDX).
           MOVE POS-PEND-IN-QTY    TO WS-RW-PEND-IN (RW-IDX).
           MOVE POS-PEND-OUT-QTY   TO WS-RW-PEND-OUT (RW-IDX).
           IF POS-LAST-ACTV-DATE NUMERIC
               MOVE POS-LAST-ACTV-DATE TO WS-RW-LAST-ACTV (RW-IDX)
           ELSE
               MOVE ZERO TO WS-RW-LAST-ACTV (RW-IDX)
           END-IF.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2900-CLEAR-CUSIP.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-CT-SEC-TYPE.
           MOVE ZERO   TO WS-CT-OWNER-SD WS-CT-OWNER-TD
                          WS-CT-FIRM-SD WS-CT-FIRM-TD
                          WS-CT-LOC-SD WS-CT-LOC-TD
                          WS-CT-OWNER-CNT WS-CT-LOC-CNT
                          WS-CT-OWNER-MV-USD.
           MOVE ZERO   TO WS-ROW-USED WS-BREAKS-THIS-CUSIP.
           MOVE 'N'    TO WS-DTL-OVERFLOW-SW WS-DTL-WRITTEN-SW.
       2900-EXIT.
           EXIT.
      *================================================================*
      * END OF A CUSIP - PROVE IT                                      *
      *================================================================*
       3000-CUSIP-BREAK.
           ADD 1 TO WS-CUSIP-CNT.
           PERFORM 6000-MATCH-PREV-BREAKS THRU 6000-EXIT.
           COMPUTE WS-SD-DIFF = WS-CT-OWNER-SD + WS-CT-LOC-SD.
           COMPUTE WS-TD-DIFF = WS-CT-OWNER-TD + WS-CT-LOC-TD.
           IF WS-SD-DIFF NOT = ZERO
               ADD 1 TO WS-SD-BREAK-CNT
               MOVE 'SD' TO BRK-TYPE
               MOVE WS-SD-DIFF TO BRK-DIFFERENCE
               PERFORM 4000-WRITE-BREAK THRU 4000-EXIT
           END-IF.
           IF WS-TD-DIFF NOT = ZERO
               ADD 1 TO WS-TD-BREAK-CNT
               MOVE 'TD' TO BRK-TYPE
               MOVE WS-TD-DIFF TO BRK-DIFFERENCE
               PERFORM 4000-WRITE-BREAK THRU 4000-EXIT
           END-IF.
           IF WS-CT-FIRM-SD < ZERO
               ADD 1 TO WS-FS-BREAK-CNT
               MOVE 'FS' TO BRK-TYPE
               MOVE WS-CT-FIRM-SD TO BRK-DIFFERENCE
               PERFORM 4000-WRITE-BREAK THRU 4000-EXIT
           END-IF.
           IF WS-BREAKS-THIS-CUSIP = ZERO
               ADD 1 TO WS-BALANCED-CNT
           ELSE
               ADD 1 TO WS-CUSIP-BREAK-CNT
               PERFORM 5000-WRITE-DETAIL THRU 5000-EXIT
           END-IF.
           PERFORM 6800-COUNT-RESOLVED THRU 6800-EXIT.
           PERFORM 2900-CLEAR-CUSIP THRU 2900-EXIT.
       3000-EXIT.
           EXIT.
      *================================================================*
      * WRITE ONE BREAK (TYPE AND DIFFERENCE ALREADY SET)              *
      *================================================================*
       4000-WRITE-BREAK.
           ADD 1 TO WS-BREAKS-THIS-CUSIP.
           MOVE DC-BUS-DATE         TO BRK-BUS-DATE.
           MOVE WS-PREV-CUSIP       TO BRK-CUSIP.
           MOVE WS-CT-SEC-TYPE      TO BRK-SEC-TYPE.
           IF BRK-TYPE = 'TD'
               MOVE WS-CT-OWNER-TD  TO BRK-OWNER-QTY
               MOVE WS-CT-FIRM-TD   TO BRK-FIRM-QTY
               MOVE WS-CT-LOC-TD    TO BRK-LOCATION-QTY
           ELSE
               MOVE WS-CT-OWNER-SD  TO BRK-OWNER-QTY
               MOVE WS-CT-FIRM-SD   TO BRK-FIRM-QTY
               MOVE WS-CT-LOC-SD    TO BRK-LOCATION-QTY
           END-IF.
           MOVE WS-CT-OWNER-CNT     TO BRK-OWNER-COUNT.
           MOVE WS-CT-LOC-CNT       TO BRK-LOCATION-COUNT.
           PERFORM 4100-BREAK-MARKET-VALUE THRU 4100-EXIT.
           MOVE WS-BREAK-MV         TO BRK-MKT-VALUE-USD.
           PERFORM 4200-AGE-BREAK THRU 4200-EXIT.
           MOVE WS-BREAK-AGE        TO BRK-AGE-DAYS.
           MOVE SPACES              TO BRK-BREAK-REC (77:74).
           WRITE BRKOUT-REC FROM BRK-BREAK-REC.
           IF WS-BRKOUT-STATUS NOT = '00'
               MOVE 'BRKOUT' TO AB-DDNAME
               MOVE WS-BRKOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '4000-WRITE-BREAK' TO AB-PARAGRAPH
               MOVE WS-PREV-CUSIP TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-BREAK-OUT-CNT.
           ADD WS-BREAK-MV TO WS-BREAK-MV-TOTAL.
           IF BRK-DIFFERENCE < ZERO
               COMPUTE WS-ABS-DIFF = BRK-DIFFERENCE * -1
           ELSE
               MOVE BRK-DIFFERENCE TO WS-ABS-DIFF
           END-IF.
           ADD WS-ABS-DIFF TO WS-BREAK-QTY-HASH.
      *    HEADER RECORD FOR THE DETAIL FILE
           MOVE SPACES              TO BKD-DETAIL-REC.
           MOVE 'H'                 TO BKD-REC-TYPE.
           MOVE WS-PREV-CUSIP       TO BKD-CUSIP.
           MOVE BRK-BREAK-REC (1:76) TO BKD-BREAK-IMAGE.
           PERFORM 8300-WRITE-DETAIL-REC THRU 8300-EXIT.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * VALUE OF THE DIFFERENCE AT THE AVERAGE OWNER VALUE PER UNIT    *
      *----------------------------------------------------------------*
       4100-BREAK-MARKET-VALUE.
           MOVE ZERO TO WS-BREAK-MV.
           IF WS-CT-OWNER-TD = ZERO
               GO TO 4100-EXIT
           END-IF.
           COMPUTE WS-UNIT-MV ROUNDED =
               WS-CT-OWNER-MV-USD / WS-CT-OWNER-TD
               ON SIZE ERROR
                   MOVE ZERO TO WS-UNIT-MV
           END-COMPUTE.
           COMPUTE WS-BREAK-MV ROUNDED = BRK-DIFFERENCE * WS-UNIT-MV
               ON SIZE ERROR
                   MOVE ZERO TO WS-BREAK-MV
           END-COMPUTE.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * AGE = PRIOR AGE + 1 IF THE SAME CUSIP/TYPE WAS IN BREAK        *
      * YESTERDAY, ELSE 1 (NEW BREAK)                                  *
      *----------------------------------------------------------------*
       4200-AGE-BREAK.
           MOVE 1 TO WS-BREAK-AGE.
           PERFORM VARYING PV-IDX FROM 1 BY 1
                   UNTIL PV-IDX > WS-PRV-USED
               IF WS-PV-TYPE (PV-IDX) = BRK-TYPE
                   COMPUTE WS-BREAK-AGE = WS-PV-AGE (PV-IDX) + 1
                   MOVE 'Y' TO WS-PV-MATCHED (PV-IDX)
               END-IF
           END-PERFORM.
           IF WS-BREAK-AGE > 999
               MOVE 999 TO WS-BREAK-AGE
           END-IF.
           IF WS-BREAK-AGE = 1
               ADD 1 TO WS-NEW-BREAK-CNT
           ELSE
               ADD 1 TO WS-AGED-BREAK-CNT
           END-IF.
           IF WS-BREAK-AGE > WS-MAX-AGE
               MOVE WS-BREAK-AGE TO WS-MAX-AGE
           END-IF.
       4200-EXIT.
           EXIT.
      *================================================================*
      * DETAIL ROWS OF A CUSIP IN BREAK                                *
      *================================================================*
       5000-WRITE-DETAIL.
           PERFORM VARYING RW-IDX FROM 1 BY 1
                   UNTIL RW-IDX > WS-ROW-USED
               MOVE SPACES                   TO BKD-DETAIL-REC
               MOVE 'D'                      TO BKD-REC-TYPE
               MOVE WS-PREV-CUSIP            TO BKD-CUSIP
               MOVE WS-RW-ACCT-NO (RW-IDX)   TO BKD-ACCT-NO
               MOVE WS-RW-LOCATION (RW-IDX)  TO BKD-LOCATION
               MOVE WS-RW-ACCT-TYPE (RW-IDX) TO BKD-ACCT-TYPE
               MOVE WS-RW-ROLE (RW-IDX)      TO BKD-ROLE
               MOVE WS-RW-TD-QTY (RW-IDX)    TO BKD-TD-QTY
               MOVE WS-RW-SD-QTY (RW-IDX)    TO BKD-SD-QTY
               MOVE WS-RW-PEND-IN (RW-IDX)   TO BKD-PEND-IN-QTY
               MOVE WS-RW-PEND-OUT (RW-IDX)  TO BKD-PEND-OUT-QTY
               MOVE WS-RW-LAST-ACTV (RW-IDX) TO BKD-LAST-ACTV-DATE
               PERFORM 8300-WRITE-DETAIL-REC THRU 8300-EXIT
           END-PERFORM.
       5000-EXIT.
           EXIT.
      *================================================================*
      * PREVIOUS BREAK FILE - MATCH ON CUSIP                           *
      *================================================================*
       6000-MATCH-PREV-BREAKS.
           MOVE ZERO TO WS-PRV-USED.
      *    PRIOR CUSIPS BELOW THE CURRENT ONE ARE NO LONGER IN BREAK
           PERFORM UNTIL END-OF-PREV-BREAKS
                      OR WS-PB-CUSIP NOT < WS-PREV-CUSIP
               ADD 1 TO WS-RESOLVED-CNT
               PERFORM 8100-READ-PREV-BREAK THRU 8100-EXIT
           END-PERFORM.
           PERFORM UNTIL END-OF-PREV-BREAKS
                      OR WS-PB-CUSIP NOT = WS-PREV-CUSIP
               IF WS-PRV-USED < 10
                   ADD 1 TO WS-PRV-USED
                   SET PV-IDX TO WS-PRV-USED
                   MOVE WS-PB-TYPE     TO WS-PV-TYPE (PV-IDX)
                   MOVE WS-PB-AGE-DAYS TO WS-PV-AGE (PV-IDX)
                   MOVE 'N'            TO WS-PV-MATCHED (PV-IDX)
               END-IF
               PERFORM 8100-READ-PREV-BREAK THRU 8100-EXIT
           END-PERFORM.
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6800-COUNT-RESOLVED.
      *----------------------------------------------------------------*
           PERFORM VARYING PV-IDX FROM 1 BY 1
                   UNTIL PV-IDX > WS-PRV-USED
               IF WS-PV-MATCHED (PV-IDX) = 'N'
                   ADD 1 TO WS-RESOLVED-CNT
               END-IF
           END-PERFORM.
       6800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PRIOR BREAKS BEYOND THE LAST CUSIP ARE RESOLVED                *
      *----------------------------------------------------------------*
       6900-DRAIN-PREV-BREAKS.
           PERFORM UNTIL END-OF-PREV-BREAKS
               ADD 1 TO WS-RESOLVED-CNT
               PERFORM 8100-READ-PREV-BREAK THRU 8100-EXIT
           END-PERFORM.
       6900-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-POSITION.
           READ POSNIN-FILE.
           EVALUATE TRUE
               WHEN POSNIN-OK
                   CONTINUE
               WHEN POSNIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'POSNIN' TO AB-DDNAME
                   MOVE WS-POSNIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-POSITION' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF NOT END-OF-POSITIONS
               IF POS-TD-QTY NOT NUMERIC OR POS-SD-QTY NOT NUMERIC
                   MOVE 'POSNIN' TO AB-DDNAME
                   MOVE 1008 TO AB-ABEND-CODE
                   MOVE '8000-READ-POSITION' TO AB-PARAGRAPH
                   MOVE POS-KEY TO AB-KEY
                   MOVE 'QUANTITY FIELDS NOT NUMERIC' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
           END-IF.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-PREV-BREAK.
      *----------------------------------------------------------------*
           READ PRVBRK-FILE INTO WS-PRV-BREAK.
           EVALUATE TRUE
               WHEN PRVBRK-OK
                   ADD 1 TO WS-PRV-READ-CNT
                   IF WS-PB-AGE-DAYS NOT NUMERIC
                       MOVE ZERO TO WS-PB-AGE-DAYS
                   END-IF
               WHEN PRVBRK-EOF
                   MOVE 'Y' TO WS-PRV-EOF-SW
                   MOVE HIGH-VALUES TO WS-PB-CUSIP
               WHEN OTHER
                   MOVE 'PRVBRK' TO AB-DDNAME
                   MOVE WS-PRVBRK-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-PREV-BREAK' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8300-WRITE-DETAIL-REC.
      *----------------------------------------------------------------*
           WRITE BRKDTL-REC FROM BKD-DETAIL-REC.
           IF WS-BRKDTL-STATUS NOT = '00'
               MOVE 'BRKDTL' TO AB-DDNAME
               MOVE WS-BRKDTL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8300-WRITE-DETAIL-REC' TO AB-PARAGRAPH
               MOVE BKD-CUSIP TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-DTL-OUT-CNT.
       8300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB500'       TO CT-STAGE.
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
           CLOSE POSNIN-FILE BRKOUT-FILE BRKDTL-FILE.
           IF WS-BRKOUT-STATUS NOT = '00'
           OR WS-BRKDTL-STATUS NOT = '00'
               MOVE 'BRKOUT' TO AB-DDNAME
               MOVE WS-BRKOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF PREV-BREAKS-OPEN
               CLOSE PRVBRK-FILE
           END-IF.
           MOVE 'POSN-IN'        TO CT-COUNTER-NAME.
           MOVE WS-READ-CNT      TO CT-COUNT.
           MOVE WS-OWNER-MV-HASH TO CT-AMOUNT.
           MOVE WS-OWNER-SD-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'LOCATION-SD'    TO CT-COUNTER-NAME.
           MOVE WS-LOC-ROWS      TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           MOVE WS-LOC-SD-HASH   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CUSIPS'         TO CT-COUNTER-NAME.
           MOVE WS-CUSIP-CNT     TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BREAKS-OUT'     TO CT-COUNTER-NAME.
           MOVE WS-BREAK-OUT-CNT TO CT-COUNT.
           MOVE WS-BREAK-MV-TOTAL TO CT-AMOUNT.
           MOVE WS-BREAK-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'FIRM-SHORT'     TO CT-COUNTER-NAME.
           MOVE WS-FS-BREAK-CNT  TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* SRB500 - STOCK RECORD BALANCING              *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITION ROWS READ       : ' WS-DISP-CNT.
           MOVE WS-CLIENT-ROWS TO WS-DISP-CNT.
           DISPLAY '   CLIENT ROWS            : ' WS-DISP-CNT.
           MOVE WS-FIRM-ROWS TO WS-DISP-CNT.
           DISPLAY '   FIRM ROWS              : ' WS-DISP-CNT.
           MOVE WS-LOC-ROWS TO WS-DISP-CNT.
           DISPLAY '   LOCATION ROWS          : ' WS-DISP-CNT.
           MOVE WS-FLAT-ROWS TO WS-DISP-CNT.
           DISPLAY '   FLAT ROWS              : ' WS-DISP-CNT.
           MOVE WS-CUSIP-CNT TO WS-DISP-CNT.
           DISPLAY ' CUSIPS PROVED            : ' WS-DISP-CNT.
           MOVE WS-BALANCED-CNT TO WS-DISP-CNT.
           DISPLAY '   IN BALANCE             : ' WS-DISP-CNT.
           MOVE WS-CUSIP-BREAK-CNT TO WS-DISP-CNT.
           DISPLAY '   IN BREAK               : ' WS-DISP-CNT.
           MOVE WS-SD-BREAK-CNT TO WS-DISP-CNT.
           DISPLAY ' SETTLED BREAKS           : ' WS-DISP-CNT.
           MOVE WS-TD-BREAK-CNT TO WS-DISP-CNT.
           DISPLAY ' TRADE DATE BREAKS        : ' WS-DISP-CNT.
           MOVE WS-FS-BREAK-CNT TO WS-DISP-CNT.
           DISPLAY ' FIRM SHORTS              : ' WS-DISP-CNT.
           MOVE WS-NEW-BREAK-CNT TO WS-DISP-CNT.
           DISPLAY ' NEW BREAKS               : ' WS-DISP-CNT.
           MOVE WS-AGED-BREAK-CNT TO WS-DISP-CNT.
           DISPLAY ' AGED BREAKS              : ' WS-DISP-CNT.
           MOVE WS-RESOLVED-CNT TO WS-DISP-CNT.
           DISPLAY ' PRIOR BREAKS RESOLVED    : ' WS-DISP-CNT.
           MOVE WS-MAX-AGE TO WS-DISP-CNT.
           DISPLAY ' OLDEST BREAK (DAYS)      : ' WS-DISP-CNT.
           MOVE WS-PRV-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' PRIOR BREAK RECORDS READ : ' WS-DISP-CNT.
           MOVE WS-DTL-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' DETAIL RECORDS WRITTEN   : ' WS-DISP-CNT.
           MOVE WS-BAD-MV-CNT TO WS-DISP-CNT.
           DISPLAY ' ROWS WITH NO VALUATION   : ' WS-DISP-CNT.
           MOVE WS-OWNER-SD-HASH TO WS-DISP-QTY.
           DISPLAY ' OWNER SD TOTAL           : ' WS-DISP-QTY.
           MOVE WS-LOC-SD-HASH TO WS-DISP-QTY.
           DISPLAY ' LOCATION SD TOTAL        : ' WS-DISP-QTY.
           MOVE WS-BREAK-MV-TOTAL TO WS-DISP-AMT.
           DISPLAY ' BREAK MARKET VALUE USD   : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           IF WS-BREAK-OUT-CNT > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'STOCK RECORD OUT OF BALANCE - SEE SRR510'
                               TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'STOCK RECORD IN BALANCE' TO AU-MESSAGE
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
           DISPLAY 'SRB500 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB500 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB500 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
