      *================================================================*
      * PROGRAM    : CMU050                                            *
      * TITLE      : STANDARD ABEND HANDLER                            *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   CALLED BY EVERY BATCH PROGRAM ON A FATAL CONDITION.  PRINTS  *
      *   A DIAGNOSTIC BLOCK ON SYSOUT, WRITES AN 'A' SEVERITY AUDIT   *
      *   RECORD THROUGH CMU060, CLOSES THE AUDIT LOG AND TERMINATES   *
      *   THE ENCLAVE WITH USER ABEND U0NNN VIA LE SERVICE CEE3ABD.    *
      *   THIS MODULE DOES NOT RETURN TO THE CALLER.                   *
      *                                                                *
      * LINKAGE    : CALL 'CMU050' USING AB-ABEND-PARMS  (CMABLNK)     *
      * FILES      : SYSOUT (DISPLAY)                                  *
      * CALLS      : CMU060 (AUDIT), CMASM01 (JOB INFO), CEE3ABD (LE)  *
      *----------------------------------------------------------------*
      * STANDARD USER ABEND CODES (SEE CMABLNK):                       *
      *   1001 FILE OPEN       1002 FILE I/O      1003 DB2 ERROR       *
      *   1004 CONTROL TOTALS  1005 BAD DATE CARD 1006 SEQUENCE ERROR  *
      *   1007 TABLE OVERFLOW  1008 LOGIC ERROR   1009 RESTART ERROR   *
      *   1010 CALLED MODULE FAILURE                                   *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1988-05-09 RJK            ORIGINAL (CALL 'ILBOABN0')           *
      * 1991-02-04 RJK            WRITE AUDIT RECORD VIA CMU060        *
      * 1996-04-22 DWB  CHG02215  SQLCODE IN DIAGNOSTICS (DB2)         *
      * 1998-11-02 TLM  CHG04471  Y2K REVIEW - NO CHANGE               *
      * 2002-03-11 KAP  CHG09982  LE CONVERSION - ILBOABN0 REPLACED    *
      *                           BY CEE3ABD, TIMING 1 (CLEAN-UP)      *
      * 2011-01-17 SPA  CHG20455  CODES > 4095 FORCED TO 1008          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU050.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  05/09/88.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU050 WORKING STORAGE BEGINS'.
      *
      *    LE CEE3ABD PARAMETERS - BOTH FULLWORD BINARY
       01  WS-CEE3ABD-PARMS.
           05  WS-ABEND-CODE           PIC S9(09)  COMP.
           05  WS-ABEND-TIMING         PIC S9(09)  COMP  VALUE +1.
      *
       01  WS-SQLCODE-DISP             PIC -(9)9.
       01  WS-CODE-DISP                PIC 9(04).
       01  WS-STARS                    PIC X(70)  VALUE ALL '*'.
           COPY CMJILNK.
           COPY CMAULNK.
       01  WS-DIAG-LINE.
           05  FILLER                  PIC X(03)  VALUE '** '.
           05  WS-DL-LABEL             PIC X(16).
           05  WS-DL-VALUE             PIC X(80).
       LINKAGE SECTION.
           COPY CMABLNK.
       PROCEDURE DIVISION USING AB-ABEND-PARMS.
       0000-MAINLINE.
           IF AB-ABEND-CODE IS NOT NUMERIC
               MOVE 1008 TO AB-ABEND-CODE
           END-IF
           IF AB-ABEND-CODE > 4095 OR AB-ABEND-CODE = ZERO
               MOVE 1008 TO AB-ABEND-CODE
           END-IF
           MOVE AB-ABEND-CODE TO WS-CODE-DISP
           CALL 'CMASM01' USING JI-JOB-INFO
           PERFORM 1000-DISPLAY-DIAGNOSTICS
           PERFORM 2000-WRITE-AUDIT
           PERFORM 3000-ISSUE-ABEND
           GOBACK.
      *
       1000-DISPLAY-DIAGNOSTICS.
           DISPLAY WS-STARS
           DISPLAY '** MERIDIAN EOD - PROGRAM ABEND - CMU050'
           DISPLAY WS-STARS
           MOVE 'JOB / STEP'       TO WS-DL-LABEL
           MOVE SPACES             TO WS-DL-VALUE
           STRING JI-JOBNAME  DELIMITED BY SPACE
                  ' / '       DELIMITED BY SIZE
                  JI-STEPNAME DELIMITED BY SPACE
                  ' '         DELIMITED BY SIZE
                  JI-PROCSTEP DELIMITED BY SPACE
                  '  ('       DELIMITED BY SIZE
                  JI-JOBID    DELIMITED BY SPACE
                  ')'         DELIMITED BY SIZE
             INTO WS-DL-VALUE
           DISPLAY WS-DIAG-LINE
           MOVE 'PROGRAM'          TO WS-DL-LABEL
           MOVE AB-PROGRAM         TO WS-DL-VALUE
           DISPLAY WS-DIAG-LINE
           MOVE 'PARAGRAPH'        TO WS-DL-LABEL
           MOVE AB-PARAGRAPH       TO WS-DL-VALUE
           DISPLAY WS-DIAG-LINE
           MOVE 'ABEND CODE'       TO WS-DL-LABEL
           MOVE SPACES             TO WS-DL-VALUE
           STRING 'U' WS-CODE-DISP DELIMITED BY SIZE
             INTO WS-DL-VALUE
           DISPLAY WS-DIAG-LINE
           IF AB-DDNAME NOT = SPACES AND LOW-VALUES
               MOVE 'DDNAME / STATUS'  TO WS-DL-LABEL
               MOVE SPACES             TO WS-DL-VALUE
               STRING AB-DDNAME      DELIMITED BY SPACE
                      ' / '          DELIMITED BY SIZE
                      AB-FILE-STATUS DELIMITED BY SIZE
                 INTO WS-DL-VALUE
               DISPLAY WS-DIAG-LINE
           END-IF
           IF AB-SQLCODE NOT = ZERO
               MOVE AB-SQLCODE         TO WS-SQLCODE-DISP
               MOVE 'SQLCODE'          TO WS-DL-LABEL
               MOVE WS-SQLCODE-DISP    TO WS-DL-VALUE
               DISPLAY WS-DIAG-LINE
           END-IF
           IF AB-KEY NOT = SPACES AND LOW-VALUES
               MOVE 'KEY'              TO WS-DL-LABEL
               MOVE AB-KEY             TO WS-DL-VALUE
               DISPLAY WS-DIAG-LINE
           END-IF
           MOVE 'MESSAGE'          TO WS-DL-LABEL
           MOVE AB-MESSAGE         TO WS-DL-VALUE
           DISPLAY WS-DIAG-LINE
           DISPLAY WS-STARS.
      *
       2000-WRITE-AUDIT.
           MOVE 'WRIT'             TO AU-FUNCTION
           MOVE AB-PROGRAM         TO AU-PROGRAM
           MOVE 'ABEND'            TO AU-EVENT
           MOVE 'A'                TO AU-SEVERITY
           MOVE ZERO               TO AU-BUS-DATE
           MOVE AB-KEY             TO AU-KEY
           MOVE SPACES             TO AU-MESSAGE
           STRING 'U'              DELIMITED BY SIZE
                  WS-CODE-DISP     DELIMITED BY SIZE
                  ' '              DELIMITED BY SIZE
                  AB-PARAGRAPH     DELIMITED BY '  '
                  ' '              DELIMITED BY SIZE
                  AB-DDNAME        DELIMITED BY SPACE
                  ' '              DELIMITED BY SIZE
                  AB-FILE-STATUS   DELIMITED BY SIZE
                  ' '              DELIMITED BY SIZE
                  AB-MESSAGE       DELIMITED BY SIZE
             INTO AU-MESSAGE
           END-STRING
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'             TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
       3000-ISSUE-ABEND.
           MOVE AB-ABEND-CODE      TO WS-ABEND-CODE
           MOVE +1                 TO WS-ABEND-TIMING
           DISPLAY '** ISSUING USER ABEND U' WS-CODE-DISP
           CALL 'CEE3ABD' USING WS-ABEND-CODE WS-ABEND-TIMING.
