      *================================================================*
      * PROGRAM    : CMU060                                            *
      * TITLE      : AUDIT TRAIL WRITER                                *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   CALLABLE MODULE.  APPENDS ONE RECORD TO THE AUDIT LOG FOR    *
      *   EACH 'WRIT' REQUEST.  THE FILE IS OPENED EXTEND ON THE FIRST *
      *   CALL AND LEFT OPEN UNTIL A 'CLOS' REQUEST OR END OF STEP.    *
      *   JOB / STEP NAMES COME FROM CMASM01 (TIOT), THE TIMESTAMP     *
      *   FROM CMASM02 (STCK).  AN AUDIT FAILURE NEVER ABENDS THE      *
      *   CALLER - THE EVENT IS ECHOED TO SYSOUT AND RC 12 RETURNED.   *
      *                                                                *
      * LINKAGE    : CALL 'CMU060' USING AU-AUDIT-PARMS  (CMAULNK)     *
      * FILES      : AUDITLOG  OUTPUT (EXTEND)  MSEC.PROD.CM.AUDIT.LOG *
      *                        FB 200  DISP=MOD  (CMAUDIT)             *
      * CALLS      : CMASM01, CMASM02                                  *
      * RETURN     : AU-RETURN-CODE 00 OK, 08 BAD FUNCTION,            *
      *              12 AUDIT FILE UNAVAILABLE                         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1991-02-04 RJK            ORIGINAL - REPLACES INLINE AUDIT     *
      *                           WRITES IN EACH BATCH PROGRAM         *
      * 1993-08-17 DWB  CHG00614  ADD SEVERITY CODE                    *
      * 1998-11-02 TLM  CHG04471  Y2K - BUS DATE CCYYMMDD, TIMESTAMP   *
      *                           NOW 26 BYTES                         *
      * 2002-03-11 KAP  CHG09982  JOB/STEP FROM CMASM01 INSTEAD OF     *
      *                           CALLER-SUPPLIED FIELDS               *
      * 2006-06-12 KAP  CHG15008  TIMESTAMP FROM CMASM02 (STCK)        *
      * 2014-10-20 SPA  CHG27740  DO NOT ABEND ON AUDIT OPEN FAILURE - *
      *                           ECHO TO SYSOUT INSTEAD               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU060.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  02/04/91.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL AUDIT-FILE ASSIGN TO AUDITLOG
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  AUDIT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
           COPY CMAUDIT.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU060 WORKING STORAGE BEGINS'.
       01  WS-AUDIT-STATUS             PIC X(02).
           88  WS-AUDIT-OK                    VALUE '00'.
           88  WS-AUDIT-OPEN-OK               VALUE '00' '05' '97'.
       01  WS-SWITCHES.
           05  WS-FILE-STATE           PIC X(01)  VALUE 'C'.
               88  WS-FILE-CLOSED                 VALUE 'C'.
               88  WS-FILE-OPEN                   VALUE 'O'.
               88  WS-FILE-FAILED                 VALUE 'F'.
           05  WS-JOBINFO-SW           PIC X(01)  VALUE 'N'.
               88  WS-HAVE-JOBINFO                VALUE 'Y'.
       01  WS-COUNTERS.
           05  WS-WRITE-COUNT          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-ECHO-COUNT           PIC S9(07) COMP-3 VALUE ZERO.
           COPY CMJILNK.
           COPY CMTSLNK.
       01  WS-ECHO-LINE.
           05  FILLER                  PIC X(08)  VALUE 'AUDIT** '.
           05  WS-EL-PROGRAM           PIC X(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  WS-EL-EVENT             PIC X(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  WS-EL-SEVERITY          PIC X(01).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  WS-EL-MESSAGE           PIC X(80).
       LINKAGE SECTION.
           COPY CMAULNK.
       PROCEDURE DIVISION USING AU-AUDIT-PARMS.
       0000-MAINLINE.
           MOVE ZERO TO AU-RETURN-CODE
           IF NOT WS-HAVE-JOBINFO
               CALL 'CMASM01' USING JI-JOB-INFO
               SET WS-HAVE-JOBINFO TO TRUE
           END-IF
           EVALUATE AU-FUNCTION
               WHEN 'WRIT'
                   PERFORM 2000-WRITE-AUDIT THRU 2000-EXIT
               WHEN 'CLOS'
                   PERFORM 3000-CLOSE-AUDIT THRU 3000-EXIT
               WHEN OTHER
                   DISPLAY 'CMU060 - INVALID FUNCTION ' AU-FUNCTION
                   MOVE 08 TO AU-RETURN-CODE
           END-EVALUATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * OPEN ON FIRST WRITE.  IF THE OPEN FAILS WE KEEP GOING AND      *
      * ECHO THE EVENTS TO SYSOUT (CHG27740).                          *
      *----------------------------------------------------------------*
       1000-OPEN-AUDIT.
           OPEN EXTEND AUDIT-FILE
           IF WS-AUDIT-OPEN-OK
               SET WS-FILE-OPEN TO TRUE
           ELSE
               DISPLAY 'CMU060 - AUDITLOG OPEN FAILED, STATUS '
                       WS-AUDIT-STATUS ' - EVENTS WILL BE ECHOED'
               SET WS-FILE-FAILED TO TRUE
           END-IF.
       1000-EXIT.
           EXIT.
      *
       2000-WRITE-AUDIT.
           IF WS-FILE-CLOSED
               PERFORM 1000-OPEN-AUDIT THRU 1000-EXIT
           END-IF
           MOVE SPACES              TO AUD-AUDIT-REC
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP        TO AUD-TIMESTAMP
           MOVE JI-JOBNAME          TO AUD-JOBNAME
      *    JOB STEP NAME (EXEC STATEMENT), NOT THE PROC STEP NAME
           IF JI-STEPNAME NOT = SPACES
               MOVE JI-STEPNAME     TO AUD-STEPNAME
           ELSE
               MOVE JI-PROCSTEP     TO AUD-STEPNAME
           END-IF
           MOVE AU-PROGRAM          TO AUD-PROGRAM
           MOVE AU-EVENT            TO AUD-EVENT
           MOVE AU-SEVERITY         TO AUD-SEVERITY
           IF AUD-SEVERITY = SPACE
               MOVE 'I'             TO AUD-SEVERITY
           END-IF
           IF AU-BUS-DATE IS NUMERIC
               MOVE AU-BUS-DATE     TO AUD-BUS-DATE
           ELSE
               MOVE ZERO            TO AUD-BUS-DATE
           END-IF
           MOVE AU-KEY              TO AUD-KEY
           MOVE AU-MESSAGE          TO AUD-MESSAGE
           IF WS-FILE-OPEN
               WRITE AUD-AUDIT-REC
               IF WS-AUDIT-OK
                   ADD 1 TO WS-WRITE-COUNT
                   GO TO 2000-EXIT
               END-IF
               DISPLAY 'CMU060 - AUDITLOG WRITE FAILED, STATUS '
                       WS-AUDIT-STATUS
               SET WS-FILE-FAILED TO TRUE
           END-IF
      *    ---- FALL BACK: ECHO TO SYSOUT --------------------------
           MOVE AUD-PROGRAM         TO WS-EL-PROGRAM
           MOVE AUD-EVENT           TO WS-EL-EVENT
           MOVE AUD-SEVERITY        TO WS-EL-SEVERITY
           MOVE AUD-MESSAGE         TO WS-EL-MESSAGE
           DISPLAY WS-ECHO-LINE
           ADD 1 TO WS-ECHO-COUNT
           MOVE 12 TO AU-RETURN-CODE.
       2000-EXIT.
           EXIT.
      *
       3000-CLOSE-AUDIT.
           IF WS-FILE-OPEN
               CLOSE AUDIT-FILE
               IF NOT WS-AUDIT-OK
                   DISPLAY 'CMU060 - AUDITLOG CLOSE STATUS '
                           WS-AUDIT-STATUS
                   MOVE 12 TO AU-RETURN-CODE
               END-IF
           END-IF
           SET WS-FILE-CLOSED TO TRUE.
       3000-EXIT.
           EXIT.
