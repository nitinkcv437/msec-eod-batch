      *================================================================*
      * PROGRAM    : CMU080                                            *
      * TITLE      : CONTROL TOTAL POSTING / RETRIEVAL                 *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   EVERY BATCH PROGRAM POSTS ITS RECORD COUNTS AND AMOUNT /     *
      *   QUANTITY HASHES HERE ('POST').  DOWNSTREAM PROGRAMS (TCB900, *
      *   CMB090) RECONCILE STAGE TO STAGE.  'GET ' RETURNS THE LATEST *
      *   RECORD POSTED TODAY FOR A STAGE / COUNTER NAME.              *
      *                                                                *
      *   THE CONTROL FILE IS ONE GDG GENERATION PER BUSINESS DAY,     *
      *   ALLOCATED EMPTY BY MSCMD010 AND WRITTEN DISP=MOD BY EVERY    *
      *   JOB OF THE CYCLE.  A QSAM FILE CANNOT BE EXTENDED AND READ   *
      *   AT THE SAME TIME, SO THIS MODULE KEEPS AN OPEN STATE:        *
      *       CLOSED -> POST -> OPEN EXTEND                            *
      *       CLOSED -> GET  -> OPEN INPUT, SCAN, CLOSE                *
      *       EXTEND -> GET  -> CLOSE, OPEN INPUT, SCAN, CLOSE         *
      *   SO A LATER POST RE-OPENS EXTEND.  RECORDS WRITTEN BEFORE THE *
      *   GET ARE THEREFORE VISIBLE TO IT.                             *
      *                                                                *
      * LINKAGE    : CALL 'CMU080' USING CT-CONTROL-PARMS  (CMCTLNK)   *
      * FILES      : CTLTOTS  EXTEND / INPUT  MSEC.PROD.CM.CTLTOTS(0)  *
      *                       FB 120  DISP=MOD  (CMCTLTOT)             *
      * CALLS      : CMASM01 (JOB NAME), CMASM02 (TIMESTAMP)           *
      * RETURN     : CT-RETURN-CODE 00 OK, 04 NOT FOUND (GET),         *
      *              08 INVALID FUNCTION, 12 I/O ERROR                 *
      *----------------------------------------------------------------*
      * CONVENTIONS (SEE DOCS/PROGRAMS-CM):                            *
      *   CT-PROGRAM = POSTING PROGRAM, CT-STAGE = STAGE ID (NORMALLY  *
      *   THE PROGRAM NAME), CT-COUNTER-NAME = FREE TEXT, E.G.         *
      *   'TRADES-IN', 'SRACTV-OUT'.  CT-BUS-DATE = DC-BUS-DATE.       *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1990-07-23 RJK            ORIGINAL - TOTALS KEPT IN VSAM RRDS  *
      * 1998-11-02 TLM  CHG04471  Y2K - BUS DATE CCYYMMDD              *
      * 2001-04-09 DWB  CHG08130  DECIMALIZATION - QTY HASH NOW 4 DEC  *
      * 2009-02-16 SPA  CHG18810  REWRITE - RRDS REPLACED BY DAILY     *
      *                           CTLTOTS GDG (DISP=MOD), 'GET ' SCANS *
      * 2012-05-07 SPA  CHG23391  GET AFTER POST IN SAME STEP - CLOSE  *
      *                           AND RE-OPEN (WAS STATUS 48 ABEND)    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU080.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  07/23/90.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CTLTOT-FILE ASSIGN TO CTLTOTS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-CTL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CTLTOT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  CTLTOT-FILE-REC             PIC X(120).
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU080 WORKING STORAGE BEGINS'.
       01  WS-CTL-STATUS               PIC X(02).
           88  WS-CTL-OK                      VALUE '00'.
           88  WS-CTL-OPEN-OK                 VALUE '00' '05' '97'.
           88  WS-CTL-EOF                     VALUE '10'.
       01  WS-FILE-STATE               PIC X(01)  VALUE 'C'.
           88  WS-STATE-CLOSED                VALUE 'C'.
           88  WS-STATE-EXTEND                VALUE 'E'.
           88  WS-STATE-INPUT                 VALUE 'I'.
       01  WS-JOBINFO-SW               PIC X(01)  VALUE 'N'.
           88  WS-HAVE-JOBINFO                VALUE 'Y'.
       01  WS-SCAN-SW                  PIC X(01).
           88  WS-SCAN-DONE                   VALUE 'Y'.
       01  WS-MATCH-SW                 PIC X(01).
           88  WS-MATCH-FOUND                 VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-POST-COUNT           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-GET-COUNT            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-SCAN-COUNT           PIC S9(09) COMP-3 VALUE ZERO.
           COPY CMCTLTOT.
       01  WS-SAVE-REC                 PIC X(120).
           COPY CMJILNK.
           COPY CMTSLNK.
       LINKAGE SECTION.
           COPY CMCTLNK.
       PROCEDURE DIVISION USING CT-CONTROL-PARMS.
       0000-MAINLINE.
           MOVE ZERO TO CT-RETURN-CODE
           IF NOT WS-HAVE-JOBINFO
               CALL 'CMASM01' USING JI-JOB-INFO
               SET WS-HAVE-JOBINFO TO TRUE
           END-IF
           EVALUATE CT-FUNCTION
               WHEN 'POST'
                   PERFORM 2000-POST-TOTAL
               WHEN 'GET '
                   PERFORM 3000-GET-TOTAL
               WHEN 'CLOS'
                   PERFORM 8900-CLOSE-FILE
               WHEN OTHER
                   DISPLAY 'CMU080 - INVALID FUNCTION ' CT-FUNCTION
                   MOVE 08 TO CT-RETURN-CODE
           END-EVALUATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * POST - APPEND ONE CONTROL TOTAL RECORD                         *
      *----------------------------------------------------------------*
       2000-POST-TOTAL.
           IF WS-STATE-INPUT
               PERFORM 8900-CLOSE-FILE
           END-IF
           IF WS-STATE-CLOSED
               PERFORM 8100-OPEN-EXTEND
               IF CT-IO-ERROR
                   EXIT PARAGRAPH
               END-IF
           END-IF
           INITIALIZE CTR-CONTROL-REC
           MOVE CT-BUS-DATE         TO CTR-BUS-DATE
           MOVE JI-JOBNAME          TO CTR-JOBNAME
           MOVE CT-PROGRAM          TO CTR-PROGRAM
           MOVE CT-STAGE            TO CTR-STAGE
           IF CTR-STAGE = SPACES
               MOVE CT-PROGRAM      TO CTR-STAGE
           END-IF
           MOVE CT-COUNTER-NAME     TO CTR-COUNTER
           MOVE CT-COUNT            TO CTR-COUNT
           MOVE CT-AMOUNT           TO CTR-AMOUNT
           MOVE CT-QTY-HASH         TO CTR-QTY-HASH
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP        TO CTR-TIMESTAMP
           WRITE CTLTOT-FILE-REC FROM CTR-CONTROL-REC
           IF WS-CTL-OK
               ADD 1 TO WS-POST-COUNT
           ELSE
               DISPLAY 'CMU080 - CTLTOTS WRITE FAILED, STATUS '
                       WS-CTL-STATUS ' ' CT-STAGE ' ' CT-COUNTER-NAME
               MOVE 12 TO CT-RETURN-CODE
           END-IF.
      *
      *----------------------------------------------------------------*
      * GET - LATEST RECORD FOR BUS DATE + STAGE + COUNTER NAME.       *
      * THE FILE IS IN POSTING ORDER SO THE LAST MATCH IS THE LATEST.  *
      *----------------------------------------------------------------*
       3000-GET-TOTAL.
           ADD 1 TO WS-GET-COUNT
           IF NOT WS-STATE-CLOSED
               PERFORM 8900-CLOSE-FILE
           END-IF
           PERFORM 8200-OPEN-INPUT
           IF CT-IO-ERROR
               EXIT PARAGRAPH
           END-IF
           MOVE 'N' TO WS-SCAN-SW WS-MATCH-SW
           PERFORM UNTIL WS-SCAN-DONE
               READ CTLTOT-FILE INTO CTR-CONTROL-REC
               EVALUATE TRUE
                 WHEN WS-CTL-OK
                   ADD 1 TO WS-SCAN-COUNT
                   IF CTR-BUS-DATE = CT-BUS-DATE
                      AND CTR-STAGE = CT-STAGE
                      AND CTR-COUNTER = CT-COUNTER-NAME
                       MOVE CTR-CONTROL-REC TO WS-SAVE-REC
                       SET WS-MATCH-FOUND TO TRUE
                   END-IF
                 WHEN WS-CTL-EOF
                   SET WS-SCAN-DONE TO TRUE
                 WHEN OTHER
                   DISPLAY 'CMU080 - CTLTOTS READ FAILED, STATUS '
                           WS-CTL-STATUS
                   MOVE 12 TO CT-RETURN-CODE
                   SET WS-SCAN-DONE TO TRUE
               END-EVALUATE
           END-PERFORM
           PERFORM 8900-CLOSE-FILE
           IF CT-IO-ERROR
               EXIT PARAGRAPH
           END-IF
           IF WS-MATCH-FOUND
               MOVE WS-SAVE-REC     TO CTR-CONTROL-REC
               MOVE CTR-PROGRAM     TO CT-PROGRAM
               MOVE CTR-COUNT       TO CT-COUNT
               MOVE CTR-AMOUNT      TO CT-AMOUNT
               MOVE CTR-QTY-HASH    TO CT-QTY-HASH
               MOVE 00              TO CT-RETURN-CODE
           ELSE
               MOVE ZERO            TO CT-COUNT CT-AMOUNT CT-QTY-HASH
               MOVE 04              TO CT-RETURN-CODE
           END-IF.
      *
      *----------------------------------------------------------------*
      * I/O PARAGRAPHS                                                 *
      *----------------------------------------------------------------*
       8100-OPEN-EXTEND.
           OPEN EXTEND CTLTOT-FILE
           IF WS-CTL-OPEN-OK
               SET WS-STATE-EXTEND TO TRUE
           ELSE
               DISPLAY 'CMU080 - CTLTOTS OPEN EXTEND FAILED, STATUS '
                       WS-CTL-STATUS
               MOVE 12 TO CT-RETURN-CODE
           END-IF.
      *
       8200-OPEN-INPUT.
           OPEN INPUT CTLTOT-FILE
           IF WS-CTL-OPEN-OK
               SET WS-STATE-INPUT TO TRUE
           ELSE
               DISPLAY 'CMU080 - CTLTOTS OPEN INPUT FAILED, STATUS '
                       WS-CTL-STATUS
               MOVE 12 TO CT-RETURN-CODE
           END-IF.
      *
       8900-CLOSE-FILE.
           IF NOT WS-STATE-CLOSED
               CLOSE CTLTOT-FILE
               IF NOT WS-CTL-OK
                   DISPLAY 'CMU080 - CTLTOTS CLOSE STATUS '
                           WS-CTL-STATUS
                   MOVE 12 TO CT-RETURN-CODE
               END-IF
           END-IF
           SET WS-STATE-CLOSED TO TRUE.
