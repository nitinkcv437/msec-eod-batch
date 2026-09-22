       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB250.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  JUNE 1991.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB250                                            *
      * DESCRIPTION: SETTLEMENT PROCESSING.                            *
      *              BROWSES THE PENDING SETTLEMENT FILE FROM THE      *
      *              OLDEST SETTLE DATE UP TO THE BUSINESS DATE.       *
      *              EVERY OPEN OR FAILED ROW WHOSE REFERENCE IS NOT   *
      *              ON TODAY'S FAIL LIST SETTLES:                     *
      *                POSITION SD QTY += QTY, PENDING IN/OUT -= QTY,  *
      *                ROW STATUS 'S', SETTLED ACTIVITY WRITTEN (FOR   *
      *                SETTLE-DATE CASH IN SRB300).                    *
      *              ROWS ON THE FAIL LIST STAY OPEN WITH STATUS 'F'   *
      *              AND AGE (BUSINESS DAYS SINCE CONTRACTUAL SETTLE); *
      *              THEY ARE WRITTEN TO THE FAILS FILE FOR SRR260.    *
      *              SETTLED ROWS OLDER THAN THE RETENTION PERIOD ARE  *
      *              DELETED.                                          *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD030 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              SYSIN    - FAIL REFERENCES, ONE PER CARD          *
      *                         FAIL=<REFERENCE 16>                    *
      *                         (MSEC.PROD.SR.FAILREFS(0) FROM DTC     *
      *                         RECONCILIATION - MAY BE EMPTY)         *
      * IN/OUT     : PENDSETL - MSEC.PROD.SR.PENDSETL.KSDS    (SRPEND) *
      *              POSMAST  - MSEC.PROD.SR.POSITION.KSDS    (SRPOSN) *
      * OUTPUT     : SETLOUT  - MSEC.PROD.SR.SETTLED(+1)      (SRACTV) *
      *              FAILOUT  - MSEC.PROD.SR.FAILS(+1)        (SRFAIL) *
      * CALLS      : CMU010, CMU050, CMU060, CMU080, CMASM01           *
      * RETURN CODE: 0 CLEAN, 4 FAILS OR UNMATCHED FAIL CARDS,         *
      *              8 PENDING ROW WITHOUT A POSITION                  *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1991-06-03 DWB  ORIGINAL - SPLIT OUT OF SRB200        CHG00987 *
      * 1993-02-15 DWB  FAIL LIST FROM SYSIN                  CHG01502 *
      * 1995-06-07 LFM  T+3 SETTLEMENT                        CHG02140 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2004-03-29 KAP  PURGE OF SETTLED ROWS                 CHG12117 *
      * 2009-12-14 SPA  SETTLED ACTIVITY FOR CASH POSTING     CHG19002 *
      * 2017-09-05 MHC  FAIL MARKET VALUE FOR SRR260          CHG31388 *
      * 2024-05-20 NVR  T+1 - FAIL DAYS IN BUSINESS DAYS      CHG40551 *
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
           SELECT PENDSETL-FILE  ASSIGN TO PENDSETL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS PND-KEY
                  FILE STATUS IS WS-PENDSETL-STATUS.
           SELECT POSMAST-FILE   ASSIGN TO POSMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS POS-KEY
                  FILE STATUS IS WS-POSMAST-STATUS.
           SELECT SETLOUT-FILE   ASSIGN TO SETLOUT
                  FILE STATUS IS WS-SETLOUT-STATUS.
           SELECT FAILOUT-FILE   ASSIGN TO FAILOUT
                  FILE STATUS IS WS-FAILOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARMCARD-REC                PIC X(80).
       FD  PENDSETL-FILE.
       COPY SRPEND.
       FD  POSMAST-FILE.
       COPY SRPOSN.
       FD  SETLOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  SETLOUT-REC                 PIC X(200).
       FD  FAILOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FAILOUT-REC                 PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB250'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PENDSETL-STATUS      PIC X(02)  VALUE '00'.
               88  PENDSETL-OK                    VALUE '00' '02'.
               88  PENDSETL-EOF                   VALUE '10'.
               88  PENDSETL-NOTFND                VALUE '23'.
           05  WS-POSMAST-STATUS       PIC X(02)  VALUE '00'.
               88  POSMAST-OK                     VALUE '00' '02'.
               88  POSMAST-NOTFND                 VALUE '23'.
           05  WS-SETLOUT-STATUS       PIC X(02)  VALUE '00'.
           05  WS-FAILOUT-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-PEND-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PENDING                 VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-FAIL-SW              PIC X(01)  VALUE 'N'.
               88  REF-IS-FAILING                 VALUE 'Y'.
           05  WS-POS-FOUND-SW         PIC X(01)  VALUE 'N'.
               88  POSITION-FOUND                 VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * FAIL REFERENCE TABLE (FROM SYSIN)                              *
      *----------------------------------------------------------------*
       01  WS-FAIL-TABLE-AREA.
           05  WS-FAIL-USED            PIC S9(04) COMP  VALUE ZERO.
           05  WS-FAIL-MAX             PIC S9(04) COMP  VALUE +2000.
           05  WS-FAIL-ENTRY OCCURS 2000 TIMES INDEXED BY FL-IDX.
               10  WS-FAIL-REF         PIC X(16).
               10  WS-FAIL-HITS        PIC S9(05) COMP-3.
       01  WS-PARM-CARD.
           05  WS-PARM-KEYWORD         PIC X(05).
               88  PARM-IS-FAIL                   VALUE 'FAIL='.
           05  WS-PARM-REF             PIC X(16).
           05  FILLER                  PIC X(59).
      *----------------------------------------------------------------*
      * DATES                                                          *
      *----------------------------------------------------------------*
       01  WS-PURGE-DAYS               PIC S9(07) COMP-3 VALUE -10.
       01  WS-PURGE-BEFORE-DATE        PIC 9(08)  VALUE ZERO.
       01  WS-FAIL-DAYS                PIC S9(07) COMP-3.
      *----------------------------------------------------------------*
      * ACTIVITY TYPE TO SOURCE AND GL CODE                            *
      *----------------------------------------------------------------*
       01  WS-TYPE-MAP-VALUES.
           05  FILLER  PIC X(09)  VALUE 'BUYTCTBUY'.
           05  FILLER  PIC X(09)  VALUE 'SELTCTSEL'.
           05  FILLER  PIC X(09)  VALUE 'SSLTCTSSL'.
           05  FILLER  PIC X(09)  VALUE 'BCVTCTBCV'.
           05  FILLER  PIC X(09)  VALUE 'XBYTCTXBY'.
           05  FILLER  PIC X(09)  VALUE 'XSLTCTXSL'.
           05  FILLER  PIC X(09)  VALUE 'DIVCACDIV'.
           05  FILLER  PIC X(09)  VALUE 'WHTCACWHT'.
           05  FILLER  PIC X(09)  VALUE 'SDVCACSDV'.
           05  FILLER  PIC X(09)  VALUE 'SPLCACSPL'.
           05  FILLER  PIC X(09)  VALUE 'CILCACCIL'.
           05  FILLER  PIC X(09)  VALUE 'MGOCACMGO'.
           05  FILLER  PIC X(09)  VALUE 'MGCCACMGC'.
           05  FILLER  PIC X(09)  VALUE 'QAJAJAQAJ'.
       01  WS-TYPE-MAP REDEFINES WS-TYPE-MAP-VALUES.
           05  WS-TM-ENTRY OCCURS 14 TIMES INDEXED BY TM-IDX.
               10  WS-TM-TYPE          PIC X(03).
               10  WS-TM-SOURCE        PIC X(02).
               10  WS-TM-GL-CODE       PIC X(04).
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-PEND-READ-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ALREADY-SETL-CNT     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANCELLED-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETTLED-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETTLED-AFTER-FAIL   PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FAIL-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEW-FAIL-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PURGED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-POSN-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POS-UPDATE-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLAT-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CARD-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNMATCHED-CNT        PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-SETL-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-SETL-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-FAIL-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-FAIL-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-READ-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
       01  WS-ABS-QTY                  PIC S9(11)V9(04) COMP-3.
       01  WS-VALUE-WORK               PIC S9(15)V9(06) COMP-3.
      *----------------------------------------------------------------*
      * COPYBOOKS                                                      *
      *----------------------------------------------------------------*
       COPY SRACTV.
       COPY SRFAIL.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMJILNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-PENDING THRU 2000-EXIT
               UNTIL END-OF-PENDING.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           CALL 'CMASM01' USING JI-JOB-INFO.
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
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DC-VALID-CARD
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
           MOVE 'SETTLEMENT PROCESSING STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-LOAD-FAIL-LIST THRU 1100-EXIT.
           PERFORM 1200-PURGE-DATE THRU 1200-EXIT.
           PERFORM 1300-OPEN-FILES THRU 1300-EXIT.
           PERFORM 1400-START-BROWSE THRU 1400-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FAIL LIST.  A MISSING OR EMPTY SYSIN MEANS NO FAILS TODAY.     *
      *----------------------------------------------------------------*
       1100-LOAD-FAIL-LIST.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'SRB250 NO FAIL LIST (SYSIN STATUS '
                       WS-PARMCARD-STATUS ') - ALL ROWS SETTLE'
               GO TO 1100-EXIT
           END-IF.
           PERFORM UNTIL END-OF-PARMS
               READ PARMCARD INTO WS-PARM-CARD
                   AT END
                       MOVE 'Y' TO WS-PARM-EOF-SW
                   NOT AT END
                       PERFORM 1110-ADD-FAIL-REF THRU 1110-EXIT
               END-READ
           END-PERFORM.
           CLOSE PARMCARD.
           MOVE WS-FAIL-USED TO WS-DISP-CNT.
           DISPLAY 'SRB250 FAIL REFERENCES LOADED: ' WS-DISP-CNT.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-ADD-FAIL-REF.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CARD-CNT.
           IF WS-PARM-CARD(1:1) = '*' OR WS-PARM-CARD = SPACES
               GO TO 1110-EXIT
           END-IF.
           IF NOT PARM-IS-FAIL OR WS-PARM-REF = SPACES
               DISPLAY 'SRB250 INVALID CONTROL CARD IGNORED: '
                       WS-PARM-CARD
               GO TO 1110-EXIT
           END-IF.
           IF WS-FAIL-USED NOT < WS-FAIL-MAX
               MOVE 1007 TO AB-ABEND-CODE
               MOVE '1110-ADD-FAIL-REF' TO AB-PARAGRAPH
               MOVE 'SYSIN' TO AB-DDNAME
               MOVE 'FAIL TABLE FULL (2000)' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-FAIL-USED.
           SET FL-IDX TO WS-FAIL-USED.
           MOVE WS-PARM-REF TO WS-FAIL-REF (FL-IDX).
           MOVE ZERO        TO WS-FAIL-HITS (FL-IDX).
       1110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SETTLED ROWS WITH A SETTLE DATE BEFORE THIS ARE DELETED        *
      *----------------------------------------------------------------*
       1200-PURGE-DATE.
           MOVE 'ADDB'         TO DT-FUNCTION.
           MOVE 'NYSE'         TO DT-CALENDAR.
           MOVE DC-BUS-DATE    TO DT-DATE-1.
           MOVE WS-PURGE-DAYS  TO DT-DAYS.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE DT-RESULT-DATE TO WS-PURGE-BEFORE-DATE
           ELSE
               DISPLAY 'SRB250 CMU010 ADDB RC ' DT-RETURN-CODE
                       ' - NO PURGE TODAY'
               MOVE ZERO TO WS-PURGE-BEFORE-DATE
           END-IF.
       1200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1300-OPEN-FILES.
      *----------------------------------------------------------------*
           OPEN I-O PENDSETL-FILE.
           IF WS-PENDSETL-STATUS NOT = '00'
               MOVE 'PENDSETL' TO AB-DDNAME
               MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT SETLOUT-FILE.
           IF WS-SETLOUT-STATUS NOT = '00'
               MOVE 'SETLOUT' TO AB-DDNAME
               MOVE WS-SETLOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT FAILOUT-FILE.
           IF WS-FAILOUT-STATUS NOT = '00'
               MOVE 'FAILOUT' TO AB-DDNAME
               MOVE WS-FAILOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1300-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       1300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * POSITION AT THE OLDEST PENDING ROW                             *
      *----------------------------------------------------------------*
       1400-START-BROWSE.
           MOVE LOW-VALUES TO PND-KEY.
           START PENDSETL-FILE KEY IS NOT LESS THAN PND-KEY.
           EVALUATE TRUE
               WHEN PENDSETL-OK
                   PERFORM 8000-READ-NEXT-PENDING THRU 8000-EXIT
               WHEN PENDSETL-NOTFND
                   DISPLAY 'SRB250 PENDING SETTLEMENT FILE IS EMPTY'
                   MOVE 'Y' TO WS-PEND-EOF-SW
               WHEN OTHER
                   MOVE 'PENDSETL' TO AB-DDNAME
                   MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '1400-START-BROWSE' TO AB-PARAGRAPH
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       1400-EXIT.
           EXIT.
      *================================================================*
      * ONE PENDING ROW                                                *
      *================================================================*
       2000-PROCESS-PENDING.
           ADD 1 TO WS-PEND-READ-CNT.
           EVALUATE TRUE
               WHEN PND-SETTLED
                   ADD 1 TO WS-ALREADY-SETL-CNT
                   IF PND-SETTLE-DATE < WS-PURGE-BEFORE-DATE
                       PERFORM 2800-PURGE-ROW THRU 2800-EXIT
                   END-IF
               WHEN PND-CANCELLED
                   ADD 1 TO WS-CANCELLED-CNT
                   IF PND-SETTLE-DATE < WS-PURGE-BEFORE-DATE
                       PERFORM 2800-PURGE-ROW THRU 2800-EXIT
                   END-IF
               WHEN OTHER
                   IF PND-QTY < ZERO
                       COMPUTE WS-ABS-QTY = PND-QTY * -1
                   ELSE
                       MOVE PND-QTY TO WS-ABS-QTY
                   END-IF
                   ADD WS-ABS-QTY TO WS-READ-QTY-HASH
                   PERFORM 2100-CHECK-FAIL-LIST THRU 2100-EXIT
                   PERFORM 2200-READ-POSITION THRU 2200-EXIT
                   IF REF-IS-FAILING
                       PERFORM 2500-FAIL-ROW THRU 2500-EXIT
                   ELSE
                       PERFORM 2300-SETTLE-ROW THRU 2300-EXIT
                   END-IF
           END-EVALUATE.
           PERFORM 8000-READ-NEXT-PENDING THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-CHECK-FAIL-LIST.
      *----------------------------------------------------------------*
           MOVE 'N' TO WS-FAIL-SW.
           IF WS-FAIL-USED = ZERO
               GO TO 2100-EXIT
           END-IF.
           SET FL-IDX TO 1.
           SEARCH WS-FAIL-ENTRY
               AT END
                   CONTINUE
               WHEN FL-IDX > WS-FAIL-USED
                   CONTINUE
               WHEN WS-FAIL-REF (FL-IDX) = PND-REF
                   MOVE 'Y' TO WS-FAIL-SW
                   ADD 1 TO WS-FAIL-HITS (FL-IDX)
           END-SEARCH.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-READ-POSITION.
      *----------------------------------------------------------------*
           MOVE 'N' TO WS-POS-FOUND-SW.
           MOVE PND-ACCT-NO  TO POS-ACCT-NO.
           MOVE PND-CUSIP    TO POS-CUSIP.
           MOVE PND-LOCATION TO POS-LOCATION.
           READ POSMAST-FILE.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   MOVE 'Y' TO WS-POS-FOUND-SW
               WHEN POSMAST-NOTFND
                   CONTINUE
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2200-READ-POSITION' TO AB-PARAGRAPH
                   MOVE POS-KEY TO AB-KEY
                   MOVE 'POSITION READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2200-EXIT.
           EXIT.
      *================================================================*
      * SETTLE A ROW                                                   *
      *================================================================*
       2300-SETTLE-ROW.
           IF PND-QTY NOT = ZERO
               IF NOT POSITION-FOUND
                   ADD 1 TO WS-NO-POSN-CNT
                   MOVE 8 TO WS-RETURN-CODE
                   DISPLAY 'SRB250 NO POSITION FOR PENDING ROW '
                           PND-KEY ' - LEFT OPEN'
                   GO TO 2300-EXIT
               END-IF
               ADD PND-QTY TO POS-SD-QTY
               IF PND-QTY > ZERO
                   SUBTRACT PND-QTY FROM POS-PEND-IN-QTY
               ELSE
                   ADD PND-QTY TO POS-PEND-OUT-QTY
               END-IF
               IF POS-TD-QTY = ZERO AND POS-SD-QTY = ZERO
                   IF NOT POS-FROZEN
                       MOVE 'F' TO POS-STATUS
                       ADD 1 TO WS-FLAT-CNT
                   END-IF
               END-IF
               MOVE DC-BUS-DATE TO POS-LAST-ACTV-DATE
               MOVE JI-JOBNAME  TO POS-LAST-UPD-JOB
               REWRITE POS-POSITION-REC
               IF WS-POSMAST-STATUS NOT = '00'
               AND WS-POSMAST-STATUS NOT = '02'
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2300-SETTLE-ROW' TO AB-PARAGRAPH
                   MOVE POS-KEY TO AB-KEY
                   MOVE 'POSITION REWRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
               ADD 1 TO WS-POS-UPDATE-CNT
           END-IF.
           IF PND-FAILED
               ADD 1 TO WS-SETTLED-AFTER-FAIL
           END-IF.
           PERFORM 2400-WRITE-SETTLED THRU 2400-EXIT.
           MOVE 'S' TO PND-STATUS.
           PERFORM 8100-REWRITE-PENDING THRU 8100-EXIT.
           ADD 1 TO WS-SETTLED-CNT.
           ADD WS-ABS-QTY TO WS-SETL-QTY-HASH.
           ADD PND-CASH   TO WS-SETL-CASH-HASH.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SETTLED ACTIVITY IN SRACTV FORMAT                              *
      *----------------------------------------------------------------*
       2400-WRITE-SETTLED.
           MOVE SPACES           TO ACT-ACTIVITY-REC.
           SET TM-IDX TO 1.
           SEARCH WS-TM-ENTRY
               AT END
                   MOVE 'AJ'   TO ACT-SOURCE
                   MOVE SPACES TO ACT-GL-TXN-CODE
               WHEN WS-TM-TYPE (TM-IDX) = PND-ACT-TYPE
                   MOVE WS-TM-SOURCE  (TM-IDX) TO ACT-SOURCE
                   MOVE WS-TM-GL-CODE (TM-IDX) TO ACT-GL-TXN-CODE
           END-SEARCH.
           MOVE PND-REF          TO ACT-REF.
           MOVE PND-LEG-NO       TO ACT-LEG-NO.
           MOVE PND-ACCT-NO      TO ACT-ACCT-NO.
           MOVE PND-CUSIP        TO ACT-CUSIP.
           MOVE PND-LOCATION     TO ACT-LOCATION.
           MOVE PND-ACT-TYPE     TO ACT-TYPE.
           MOVE PND-QTY          TO ACT-QTY-CHANGE.
           MOVE PND-CASH         TO ACT-CASH-CHANGE.
           MOVE ZERO             TO ACT-COST-CHANGE ACT-PRICE.
           MOVE PND-CCY          TO ACT-CCY.
           MOVE PND-TRADE-DATE   TO ACT-TRADE-DATE.
           MOVE PND-SETTLE-DATE  TO ACT-SETTLE-DATE.
           MOVE DC-BUS-DATE      TO ACT-EFFECTIVE-DATE ACT-BUS-DATE.
           IF POSITION-FOUND
               MOVE POS-SEC-TYPE  TO ACT-SEC-TYPE
               MOVE POS-ACCT-TYPE TO ACT-ACCT-TYPE
           END-IF.
           MOVE 'S'              TO ACT-SETTLE-FLAG.
           IF PND-FAILED
               MOVE 'SETTLED AFTER FAIL' TO ACT-DESC
           ELSE
               MOVE 'SETTLED ON CONTRACT DATE' TO ACT-DESC
           END-IF.
           WRITE SETLOUT-REC FROM ACT-ACTIVITY-REC.
           IF WS-SETLOUT-STATUS NOT = '00'
               MOVE 'SETLOUT' TO AB-DDNAME
               MOVE WS-SETLOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2400-WRITE-SETTLED' TO AB-PARAGRAPH
               MOVE PND-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       2400-EXIT.
           EXIT.
      *================================================================*
      * FAIL A ROW - STAYS OPEN, AGES                                  *
      *================================================================*
       2500-FAIL-ROW.
           MOVE SPACES TO FLR-FAIL-REC.
           IF PND-OPEN
               MOVE 'Y' TO FLR-NEW-FAIL-FLAG
               ADD 1 TO WS-NEW-FAIL-CNT
           ELSE
               MOVE 'N' TO FLR-NEW-FAIL-FLAG
           END-IF.
           MOVE 'DIFB'          TO DT-FUNCTION.
           MOVE 'NYSE'          TO DT-CALENDAR.
           MOVE PND-SETTLE-DATE TO DT-DATE-1.
           MOVE DC-BUS-DATE     TO DT-DATE-2.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE DT-RESULT-NUM TO WS-FAIL-DAYS
           ELSE
               COMPUTE WS-FAIL-DAYS = PND-FAIL-DAYS + 1
           END-IF.
           IF WS-FAIL-DAYS > 999
               MOVE 999 TO WS-FAIL-DAYS
           END-IF.
           MOVE WS-FAIL-DAYS TO PND-FAIL-DAYS.
           MOVE 'F'          TO PND-STATUS.
           PERFORM 8100-REWRITE-PENDING THRU 8100-EXIT.
           MOVE DC-BUS-DATE     TO FLR-BUS-DATE.
           MOVE PND-SETTLE-DATE TO FLR-SETTLE-DATE.
           MOVE PND-REF         TO FLR-REF.
           MOVE PND-LEG-NO      TO FLR-LEG-NO.
           MOVE PND-ACCT-NO     TO FLR-ACCT-NO.
           MOVE PND-CUSIP       TO FLR-CUSIP.
           MOVE PND-LOCATION    TO FLR-LOCATION.
           MOVE PND-ACT-TYPE    TO FLR-ACT-TYPE.
           MOVE PND-QTY         TO FLR-QTY.
           MOVE PND-CASH        TO FLR-CASH.
           MOVE PND-CCY         TO FLR-CCY.
           MOVE PND-TRADE-DATE  TO FLR-TRADE-DATE.
           MOVE PND-FAIL-DAYS   TO FLR-FAIL-DAYS.
           MOVE ZERO            TO FLR-MKT-VALUE-USD.
           IF POSITION-FOUND
               MOVE POS-SEC-TYPE  TO FLR-SEC-TYPE
               MOVE POS-ACCT-TYPE TO FLR-ACCT-TYPE
               PERFORM 2600-FAIL-MARKET-VALUE THRU 2600-EXIT
           END-IF.
           WRITE FAILOUT-REC FROM FLR-FAIL-REC.
           IF WS-FAILOUT-STATUS NOT = '00'
               MOVE 'FAILOUT' TO AB-DDNAME
               MOVE WS-FAILOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2500-FAIL-ROW' TO AB-PARAGRAPH
               MOVE PND-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-FAIL-CNT.
           ADD WS-ABS-QTY TO WS-FAIL-QTY-HASH.
           ADD PND-CASH   TO WS-FAIL-CASH-HASH.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       2500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MARKET VALUE OF THE FAILING QUANTITY FROM YESTERDAY'S          *
      * VALUATION ON THE POSITION (PRO RATA OF TRADE DATE QUANTITY)    *
      *----------------------------------------------------------------*
       2600-FAIL-MARKET-VALUE.
           IF POS-MKT-VALUE-USD NOT NUMERIC
           OR POS-TD-QTY NOT NUMERIC
               GO TO 2600-EXIT
           END-IF.
           IF POS-TD-QTY = ZERO
               GO TO 2600-EXIT
           END-IF.
           COMPUTE WS-VALUE-WORK ROUNDED =
               POS-MKT-VALUE-USD * PND-QTY / POS-TD-QTY
               ON SIZE ERROR
                   MOVE ZERO TO WS-VALUE-WORK
           END-COMPUTE.
           COMPUTE FLR-MKT-VALUE-USD ROUNDED = WS-VALUE-WORK.
       2600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * RETENTION PURGE OF SETTLED / CANCELLED ROWS                    *
      *----------------------------------------------------------------*
       2800-PURGE-ROW.
           DELETE PENDSETL-FILE RECORD.
           IF WS-PENDSETL-STATUS NOT = '00'
               MOVE 'PENDSETL' TO AB-DDNAME
               MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2800-PURGE-ROW' TO AB-PARAGRAPH
               MOVE PND-KEY TO AB-KEY
               MOVE 'DELETE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-PURGED-CNT.
       2800-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-NEXT-PENDING.
           READ PENDSETL-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN PENDSETL-OK
                   IF PND-SETTLE-DATE > DC-BUS-DATE
                       MOVE 'Y' TO WS-PEND-EOF-SW
                   END-IF
               WHEN PENDSETL-EOF
                   MOVE 'Y' TO WS-PEND-EOF-SW
               WHEN OTHER
                   MOVE 'PENDSETL' TO AB-DDNAME
                   MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-NEXT-PENDING' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-REWRITE-PENDING.
      *----------------------------------------------------------------*
           REWRITE PND-PENDING-REC.
           IF NOT PENDSETL-OK
               MOVE 'PENDSETL' TO AB-DDNAME
               MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8100-REWRITE-PENDING' TO AB-PARAGRAPH
               MOVE PND-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB250'       TO CT-STAGE.
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
           CLOSE PENDSETL-FILE POSMAST-FILE SETLOUT-FILE FAILOUT-FILE.
           IF WS-SETLOUT-STATUS NOT = '00'
           OR WS-FAILOUT-STATUS NOT = '00'
               MOVE 'SETLOUT' TO AB-DDNAME
               MOVE WS-SETLOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
      *    ---- FAIL CARDS THAT MATCHED NOTHING ---------------------
           PERFORM VARYING FL-IDX FROM 1 BY 1
                   UNTIL FL-IDX > WS-FAIL-USED
               IF WS-FAIL-HITS (FL-IDX) = ZERO
                   ADD 1 TO WS-UNMATCHED-CNT
                   DISPLAY 'SRB250 FAIL REF NOT PENDING: '
                           WS-FAIL-REF (FL-IDX)
               END-IF
           END-PERFORM.
           IF WS-UNMATCHED-CNT > ZERO AND WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           MOVE 'PEND-READ'      TO CT-COUNTER-NAME.
           MOVE WS-PEND-READ-CNT TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           MOVE WS-READ-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'SETTLED-OUT'    TO CT-COUNTER-NAME.
           MOVE WS-SETTLED-CNT   TO CT-COUNT.
           MOVE WS-SETL-CASH-HASH TO CT-AMOUNT.
           MOVE WS-SETL-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'FAILS-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-FAIL-CNT      TO CT-COUNT.
           MOVE WS-FAIL-CASH-HASH TO CT-AMOUNT.
           MOVE WS-FAIL-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'PEND-PURGED'    TO CT-COUNTER-NAME.
           MOVE WS-PURGED-CNT    TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* SRB250 - SETTLEMENT PROCESSING               *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' PURGE SETTLED BEFORE     : ' WS-PURGE-BEFORE-DATE.
           MOVE WS-CARD-CNT TO WS-DISP-CNT.
           DISPLAY ' SYSIN CARDS READ         : ' WS-DISP-CNT.
           MOVE WS-FAIL-USED TO WS-DISP-CNT.
           DISPLAY ' FAIL REFERENCES          : ' WS-DISP-CNT.
           MOVE WS-PEND-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' PENDING ROWS READ        : ' WS-DISP-CNT.
           MOVE WS-ALREADY-SETL-CNT TO WS-DISP-CNT.
           DISPLAY '   ALREADY SETTLED        : ' WS-DISP-CNT.
           MOVE WS-CANCELLED-CNT TO WS-DISP-CNT.
           DISPLAY '   CANCELLED              : ' WS-DISP-CNT.
           MOVE WS-SETTLED-CNT TO WS-DISP-CNT.
           DISPLAY ' ROWS SETTLED TODAY       : ' WS-DISP-CNT.
           MOVE WS-SETTLED-AFTER-FAIL TO WS-DISP-CNT.
           DISPLAY '   OF WHICH PRIOR FAILS   : ' WS-DISP-CNT.
           MOVE WS-FAIL-CNT TO WS-DISP-CNT.
           DISPLAY ' ROWS FAILING             : ' WS-DISP-CNT.
           MOVE WS-NEW-FAIL-CNT TO WS-DISP-CNT.
           DISPLAY '   NEW FAILS TODAY        : ' WS-DISP-CNT.
           MOVE WS-UNMATCHED-CNT TO WS-DISP-CNT.
           DISPLAY ' FAIL CARDS NOT MATCHED   : ' WS-DISP-CNT.
           MOVE WS-POS-UPDATE-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS UPDATED        : ' WS-DISP-CNT.
           MOVE WS-FLAT-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS NOW FLAT       : ' WS-DISP-CNT.
           MOVE WS-NO-POSN-CNT TO WS-DISP-CNT.
           DISPLAY ' ROWS WITHOUT POSITION    : ' WS-DISP-CNT.
           MOVE WS-PURGED-CNT TO WS-DISP-CNT.
           DISPLAY ' ROWS PURGED              : ' WS-DISP-CNT.
           MOVE WS-SETL-CASH-HASH TO WS-DISP-AMT.
           DISPLAY ' SETTLED CASH             : ' WS-DISP-AMT.
           MOVE WS-FAIL-CASH-HASH TO WS-DISP-AMT.
           DISPLAY ' FAILING CASH             : ' WS-DISP-AMT.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'SETTLEMENT PROCESSING ENDED' TO AU-MESSAGE.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
           ELSE
               MOVE 'I' TO AU-SEVERITY
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
           DISPLAY 'SRB250 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB250 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB250 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
