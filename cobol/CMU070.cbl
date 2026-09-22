      *================================================================*
      * PROGRAM    : CMU070                                            *
      * TITLE      : CHECKPOINT / RESTART SERVICES                     *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   KEEPS ONE CHECKPOINT RECORD PER PROGRAM + JOB NAME IN THE    *
      *   CHECKPOINT KSDS.  A LONG-RUNNING UPDATE PROGRAM CALLS:       *
      *     'INIT' AT START.  IF AN IN-FLIGHT RECORD EXISTS FOR THE    *
      *            SAME PROGRAM / JOB AND THE SAME BUSINESS DATE, THE  *
      *            RUN IS A RESTART: CK-RESTART-FLAG = 'Y' AND         *
      *            CK-RESTART-KEY / CK-RESTART-COUNT ARE RETURNED.     *
      *            OTHERWISE A NEW IN-FLIGHT RECORD IS WRITTEN.        *
      *     'TAKE' ONCE PER INPUT RECORD PROCESSED, WITH CK-CURRENT-KEY*
      *            SET TO THE KEY OF THAT RECORD AND CK-RECORD-COUNT   *
      *            TO THE CALLER'S CUMULATIVE COUNT (INCLUDING RECORDS *
      *            SKIPPED ON RESTART).  EVERY CK-INTERVAL CALLS THE   *
      *            CHECKPOINT RECORD IS REWRITTEN AND                  *
      *            CK-CHECKPOINT-TAKEN = 'Y'.  THE CALLER MUST HAVE    *
      *            WRITTEN / COMMITTED ITS OWN UPDATES BEFORE CALLING. *
      *     'DONE' AT NORMAL END - RECORD MARKED COMPLETE.             *
      *   IF CK-RECORD-COUNT IS ZERO ON 'TAKE' THE MODULE STORES ITS   *
      *   OWN COUNT OF 'TAKE' CALLS (PLUS THE RESTART COUNT).          *
      *   CK-INTERVAL ZERO DEFAULTS TO 500.                            *
      *                                                                *
      * LINKAGE    : CALL 'CMU070' USING CK-CHECKPOINT-PARMS (CMCKLNK) *
      * FILES      : CHKPTFL  I-O  KSDS MSEC.PROD.CM.CHKPT.KSDS        *
      *                       KEY 0/16  LRECL 100  (CMCHKPT)           *
      * CALLS      : CMASM01 (JOB NAME IF CK-JOBNAME BLANK)            *
      *              CMASM02 (TIMESTAMP)                               *
      * RETURN     : CK-RETURN-CODE 00 OK, 04 RESTART BUS DATE         *
      *              MISMATCH (OLD IN-FLIGHT RECORD RESET), 08 BAD     *
      *              FUNCTION OR NO INIT, 12 VSAM ERROR                *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1994-09-12 DWB  CHG01288  ORIGINAL FOR SRB200 POSITION POSTING *
      * 1996-01-08 DWB  CHG01902  OPEN STATUS 97 ACCEPTED - CLUSTER    *
      *                           NOT CLOSED AFTER ABEND, IMPLICIT     *
      *                           VERIFY DONE BY VSAM                  *
      * 1998-11-02 TLM  CHG04471  Y2K - BUS DATE CCYYMMDD              *
      * 2004-07-19 KAP  CHG12207  JOB NAME IN KEY (PARALLEL REGIONS)   *
      * 2016-03-14 SPA  CHG29980  DEFAULT INTERVAL 500                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU070.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  09/12/94.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CHKPT-FILE ASSIGN TO CHKPTFL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CKR-KEY
               FILE STATUS IS WS-CHK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CHKPT-FILE.
           COPY CMCHKPT.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU070 WORKING STORAGE BEGINS'.
       01  WS-CHK-STATUS               PIC X(02).
           88  WS-CHK-OK                      VALUE '00'.
           88  WS-CHK-NOT-FOUND               VALUE '23'.
       01  WS-SWITCHES.
           05  WS-OPEN-SW              PIC X(01)  VALUE 'N'.
               88  WS-CHK-OPEN                    VALUE 'Y'.
           05  WS-INIT-SW              PIC X(01)  VALUE 'N'.
               88  WS-INIT-DONE                   VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-TAKE-CALLS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SINCE-CHKPT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CHKPT-COUNT          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-INTERVAL             PIC S9(07) COMP-3 VALUE +500.
           05  WS-BASE-COUNT           PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-SAVE-KEY.
           05  WS-SAVE-PROGRAM         PIC X(08).
           05  WS-SAVE-JOBNAME         PIC X(08).
       01  WS-SAVE-BUS-DATE            PIC 9(08).
           COPY CMJILNK.
           COPY CMTSLNK.
       LINKAGE SECTION.
           COPY CMCKLNK.
       PROCEDURE DIVISION USING CK-CHECKPOINT-PARMS.
       0000-MAINLINE.
           MOVE ZERO TO CK-RETURN-CODE
           MOVE 'N'  TO CK-CHECKPOINT-TAKEN
           EVALUATE CK-FUNCTION
               WHEN 'INIT'
                   PERFORM 1000-INIT THRU 1000-EXIT
               WHEN 'TAKE'
                   PERFORM 2000-TAKE THRU 2000-EXIT
               WHEN 'DONE'
                   PERFORM 3000-DONE THRU 3000-EXIT
               WHEN OTHER
                   DISPLAY 'CMU070 - INVALID FUNCTION ' CK-FUNCTION
                   MOVE 08 TO CK-RETURN-CODE
           END-EVALUATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * INIT - OPEN THE KSDS AND DETERMINE FRESH START OR RESTART      *
      *----------------------------------------------------------------*
       1000-INIT.
           IF CK-JOBNAME = SPACES OR LOW-VALUES
               CALL 'CMASM01' USING JI-JOB-INFO
               MOVE JI-JOBNAME TO CK-JOBNAME
           END-IF
           IF CK-INTERVAL > ZERO
               MOVE CK-INTERVAL TO WS-INTERVAL
           ELSE
               MOVE +500 TO WS-INTERVAL
               MOVE +500 TO CK-INTERVAL
           END-IF
           MOVE CK-PROGRAM  TO WS-SAVE-PROGRAM
           MOVE CK-JOBNAME  TO WS-SAVE-JOBNAME
           MOVE CK-BUS-DATE TO WS-SAVE-BUS-DATE
           MOVE ZERO        TO WS-TAKE-CALLS WS-SINCE-CHKPT
                               WS-CHKPT-COUNT WS-BASE-COUNT
           SET CK-FRESH-START TO TRUE
           MOVE SPACES      TO CK-RESTART-KEY
           MOVE ZERO        TO CK-RESTART-COUNT
      *
           IF NOT WS-CHK-OPEN
               OPEN I-O CHKPT-FILE
      *        97 = OPEN OK AFTER IMPLICIT VERIFY  05 = CREATED EMPTY
               IF WS-CHK-STATUS = '00' OR '97' OR '05'
                   SET WS-CHK-OPEN TO TRUE
               ELSE
                   DISPLAY 'CMU070 - CHKPTFL OPEN FAILED, STATUS '
                           WS-CHK-STATUS
                   MOVE 12 TO CK-RETURN-CODE
                   GO TO 1000-EXIT
               END-IF
           END-IF
      *
           MOVE WS-SAVE-KEY TO CKR-KEY
           READ CHKPT-FILE
           EVALUATE TRUE
               WHEN WS-CHK-OK
                   PERFORM 1100-CHECK-RESTART THRU 1100-EXIT
               WHEN WS-CHK-NOT-FOUND
                   PERFORM 1200-NEW-CHECKPOINT THRU 1200-EXIT
               WHEN OTHER
                   DISPLAY 'CMU070 - CHKPTFL READ FAILED, STATUS '
                           WS-CHK-STATUS ' KEY ' WS-SAVE-KEY
                   MOVE 12 TO CK-RETURN-CODE
           END-EVALUATE
           IF CK-RETURN-CODE < 12
               SET WS-INIT-DONE TO TRUE
           END-IF.
       1000-EXIT.
           EXIT.
      *
       1100-CHECK-RESTART.
           IF CKR-IN-FLIGHT AND CKR-BUS-DATE = WS-SAVE-BUS-DATE
               SET CK-RESTARTING      TO TRUE
               MOVE CKR-RESTART-KEY   TO CK-RESTART-KEY
               MOVE CKR-RECORD-COUNT  TO CK-RESTART-COUNT
               MOVE CKR-RECORD-COUNT  TO WS-BASE-COUNT
               DISPLAY 'CMU070 - RESTART ' WS-SAVE-PROGRAM
                       ' ' WS-SAVE-JOBNAME ' FROM KEY '
                       CKR-RESTART-KEY
               DISPLAY 'CMU070 - RECORDS ALREADY PROCESSED '
                       CKR-RECORD-COUNT
               GO TO 1100-EXIT
           END-IF
           IF CKR-IN-FLIGHT
      *        LEFT OVER FROM A PRIOR BUSINESS DAY - IGNORE IT
               DISPLAY 'CMU070 - STALE IN-FLIGHT CHECKPOINT FOR '
                       CKR-BUS-DATE ' RESET'
               MOVE 04 TO CK-RETURN-CODE
           END-IF
           PERFORM 1300-BUILD-RECORD
           REWRITE CKR-CHECKPOINT-REC
           IF NOT WS-CHK-OK
               DISPLAY 'CMU070 - CHKPTFL REWRITE FAILED, STATUS '
                       WS-CHK-STATUS
               MOVE 12 TO CK-RETURN-CODE
           END-IF.
       1100-EXIT.
           EXIT.
      *
       1200-NEW-CHECKPOINT.
           PERFORM 1300-BUILD-RECORD
           WRITE CKR-CHECKPOINT-REC
           IF NOT WS-CHK-OK
               DISPLAY 'CMU070 - CHKPTFL WRITE FAILED, STATUS '
                       WS-CHK-STATUS
               MOVE 12 TO CK-RETURN-CODE
           END-IF.
       1200-EXIT.
           EXIT.
      *
       1300-BUILD-RECORD.
           MOVE SPACES           TO CKR-CHECKPOINT-REC
           MOVE WS-SAVE-KEY      TO CKR-KEY
           MOVE WS-SAVE-BUS-DATE TO CKR-BUS-DATE
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP     TO CKR-TIMESTAMP
           MOVE ZERO             TO CKR-RECORD-COUNT
           MOVE SPACES           TO CKR-RESTART-KEY
           SET CKR-IN-FLIGHT     TO TRUE.
      *
      *----------------------------------------------------------------*
      * TAKE - COUNT THE CALL; CHECKPOINT EVERY WS-INTERVAL CALLS.     *
      * THE STORED KEY IS THE LAST KEY THE CALLER HAS FINISHED WITH -  *
      * ON RESTART THE CALLER SKIPS INPUT WITH KEY <= RESTART KEY.     *
      *----------------------------------------------------------------*
       2000-TAKE.
           IF NOT WS-INIT-DONE
               DISPLAY 'CMU070 - TAKE WITHOUT INIT'
               MOVE 08 TO CK-RETURN-CODE
               GO TO 2000-EXIT
           END-IF
           ADD 1 TO WS-TAKE-CALLS
           ADD 1 TO WS-SINCE-CHKPT
           IF WS-SINCE-CHKPT < WS-INTERVAL
               GO TO 2000-EXIT
           END-IF
           MOVE WS-SAVE-KEY TO CKR-KEY
           READ CHKPT-FILE
           IF NOT WS-CHK-OK
               DISPLAY 'CMU070 - CHKPTFL READ FAILED, STATUS '
                       WS-CHK-STATUS
               MOVE 12 TO CK-RETURN-CODE
               GO TO 2000-EXIT
           END-IF
           IF CK-RECORD-COUNT > ZERO
               MOVE CK-RECORD-COUNT TO CKR-RECORD-COUNT
           ELSE
               COMPUTE CKR-RECORD-COUNT = WS-BASE-COUNT
                                        + WS-TAKE-CALLS
           END-IF
           MOVE CK-CURRENT-KEY   TO CKR-RESTART-KEY
           MOVE WS-SAVE-BUS-DATE TO CKR-BUS-DATE
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP     TO CKR-TIMESTAMP
           SET CKR-IN-FLIGHT     TO TRUE
           REWRITE CKR-CHECKPOINT-REC
           IF NOT WS-CHK-OK
               DISPLAY 'CMU070 - CHKPTFL REWRITE FAILED, STATUS '
                       WS-CHK-STATUS
               MOVE 12 TO CK-RETURN-CODE
               GO TO 2000-EXIT
           END-IF
           MOVE ZERO TO WS-SINCE-CHKPT
           ADD 1 TO WS-CHKPT-COUNT
           MOVE 'Y' TO CK-CHECKPOINT-TAKEN.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * DONE - MARK COMPLETE AND CLOSE                                 *
      *----------------------------------------------------------------*
       3000-DONE.
           IF NOT WS-INIT-DONE
               DISPLAY 'CMU070 - DONE WITHOUT INIT'
               MOVE 08 TO CK-RETURN-CODE
               GO TO 3000-EXIT
           END-IF
           MOVE WS-SAVE-KEY TO CKR-KEY
           READ CHKPT-FILE
           IF NOT WS-CHK-OK
               DISPLAY 'CMU070 - CHKPTFL READ FAILED, STATUS '
                       WS-CHK-STATUS
               MOVE 12 TO CK-RETURN-CODE
               GO TO 3000-EXIT
           END-IF
           IF CK-RECORD-COUNT > ZERO
               MOVE CK-RECORD-COUNT TO CKR-RECORD-COUNT
           ELSE
               COMPUTE CKR-RECORD-COUNT = WS-BASE-COUNT
                                        + WS-TAKE-CALLS
           END-IF
           MOVE CK-CURRENT-KEY   TO CKR-RESTART-KEY
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP     TO CKR-TIMESTAMP
           SET CKR-COMPLETE      TO TRUE
           REWRITE CKR-CHECKPOINT-REC
           IF NOT WS-CHK-OK
               DISPLAY 'CMU070 - CHKPTFL REWRITE FAILED, STATUS '
                       WS-CHK-STATUS
               MOVE 12 TO CK-RETURN-CODE
           END-IF
           CLOSE CHKPT-FILE
           MOVE 'N' TO WS-OPEN-SW WS-INIT-SW
           DISPLAY 'CMU070 - ' WS-SAVE-PROGRAM ' COMPLETE, '
                   WS-CHKPT-COUNT ' CHECKPOINTS TAKEN'.
       3000-EXIT.
           EXIT.
