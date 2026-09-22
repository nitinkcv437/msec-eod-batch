       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCB400.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  AUGUST 2002.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCB400                                            *
      * DESCRIPTION: STREET-SIDE POSITION BREAK MASTER MAINTENANCE.    *
      *              CARRIES THE POSITION BREAKS FOUND BY RCB200 FROM  *
      *              DAY TO DAY IN THE BREAK MASTER (VSAM KSDS):       *
      *              - A NEW BREAK IS OPENED WITH TODAY AS FIRST SEEN  *
      *              - A BREAK STILL PRESENT IS AGED (BUSINESS DAYS    *
      *                SINCE FIRST SEEN), ITS QUANTITIES AND VALUE ARE *
      *                REFRESHED AND IT IS ESCALATED:                  *
      *                  AGE >= 3 LEVEL 1  (CAGE SUPERVISOR)           *
      *                  AGE >= 5 LEVEL 2  (OPERATIONS MANAGER)        *
      *                  AGE >= 10 LEVEL 3 (CONTROLLER / FINOP)        *
      *              - AN OPEN BREAK NOT IN TODAY'S RECONCILIATION IS  *
      *                CLOSED WITH TODAY AS CLOSED DATE                *
      *              - A CLOSED BREAK THAT COMES BACK IS REOPENED AS   *
      *                A NEW BREAK                                     *
      *              - A WRITTEN-OFF BREAK (STATUS W, SET ONLINE BY    *
      *                THE CAGE) IS ONLY MARKED AS SEEN                *
      *              CASH BREAKS (CLASS CS) ARE MAINTAINED BY RCB300.  *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD030 / STEP030                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              POSREC   - MSEC.PROD.RC.POSREC(+1)       (RCPREC) *
      *              HOLIDAYS - FOR CMU010 (BUSINESS DAY AGE)          *
      *              SYSIN    - CAGE ACTION CARDS (RCP400A)            *
      *                COLS 1-8 WRITEOFF OR ASSIGN, COL 9 '=',         *
      *                COLS 10-31 BREAK KEY (CLASS, DEPO, ITEM),       *
      *                COLS 33-40 NEW OWNER (ASSIGN ONLY)              *
      * UPDATE     : RCBRKMST - MSEC.PROD.RC.BRKMAST.KSDS     (RCBRKM) *
      * CALLS      : CMU010 (DIFB), CMU050, CMU060, CMU080, CMASM01    *
      * RETURN CODE: 0 NOTHING NEW   4 NEW OR ESCALATED BREAKS         *
      * RESTART    : RESTORE RC.BRKMAST.KSDS FROM THE BACKUP TAKEN IN  *
      *              STEP005 BEFORE RERUNNING - AGES AND FIRST-SEEN    *
      *              DATES ARE NOT RECOVERABLE OTHERWISE.              *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2002-08-19 KAP  ORIGINAL - REPLACES THE BREAK LOG     CHG10240 *
      *                 SPREADSHEET OF THE CAGE                        *
      * 2003-10-06 KAP  EUROCLEAR BREAKS                      CHG11702 *
      * 2005-01-24 KAP  ESCALATION LEVELS (SOX 404 FINDING)   CHG12966 *
      * 2009-12-14 SPA  BREAK VALUE IN USD                    CHG19002 *
      * 2013-05-06 SPA  CLOSE FROM THE MATCHED ITEMS - POSREC CHG24790 *
      *                 NOW CARRIES EVERY COMPARED ITEM                *
      * 2017-03-13 MHC  AGE IN BUSINESS DAYS VIA CMU010 DIFB  CHG31555 *
      *                 (WAS CALENDAR DAYS)                            *
      * 2019-02-25 MHC  CONTROL TOTALS NAMES PER CMB090       CHG33410 *
      * 2020-09-14 NVR  WRITE-OFF / ASSIGN CARDS FROM THE     CHG36977 *
      *                 CAGE (ONLINE BREAK SCREEN RETIRED)             *
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
           SELECT POSREC-FILE    ASSIGN TO POSREC
                  FILE STATUS IS WS-POSREC-STATUS.
           SELECT BRKMAST-FILE   ASSIGN TO RCBRKMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS RBM-KEY
                  FILE STATUS IS WS-BRKMAST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC.
           05  PC-KEYWORD              PIC X(08).
           05  PC-EQUAL                PIC X(01).
           05  PC-BREAK-KEY            PIC X(22).
           05  FILLER                  PIC X(01).
           05  PC-OWNER                PIC X(08).
           05  FILLER                  PIC X(40).
       FD  POSREC-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  POSREC-IN-REC               PIC X(150).
       FD  BRKMAST-FILE.
       COPY RCBRKM.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCB400'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-POSREC-STATUS        PIC X(02)  VALUE '00'.
               88  POSREC-OK                      VALUE '00'.
               88  POSREC-EOF                     VALUE '10'.
           05  WS-BRKMAST-STATUS       PIC X(02)  VALUE '00'.
               88  BRKMAST-OK                     VALUE '00' '02'.
               88  BRKMAST-NOT-FOUND              VALUE '23'.
               88  BRKMAST-EOF                    VALUE '10'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-POSREC                  VALUE 'Y'.
           05  WS-BROWSE-EOF-SW        PIC X(01)  VALUE 'N'.
               88  END-OF-BROWSE                  VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
      *----------------------------------------------------------------*
      * CAGE ACTION CARDS - HELD UNTIL THE BREAK MASTER IS OPEN        *
      *----------------------------------------------------------------*
       01  WS-ACTION-USED              PIC S9(04) COMP  VALUE ZERO.
       01  WS-ACTION-TABLE.
           05  WS-ACT OCCURS 50 TIMES INDEXED BY AC-IDX.
               10  WS-ACT-VERB         PIC X(08).
               10  WS-ACT-KEY          PIC X(22).
               10  WS-ACT-OWNER        PIC X(08).
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * ESCALATION THRESHOLDS (BUSINESS DAYS) - CHG12966               *
      *----------------------------------------------------------------*
       01  WS-ESCALATION-VALUES.
           05  FILLER  PIC X(12)  VALUE '0101CAGESUPV'.
           05  FILLER  PIC X(12)  VALUE '0502OPSMGR  '.
           05  FILLER  PIC X(12)  VALUE '1003CONTROLR'.
       01  WS-ESCALATION-TABLE REDEFINES WS-ESCALATION-VALUES.
           05  WS-ESC-ENTRY OCCURS 3 TIMES.
               10  WS-ESC-MIN-AGE      PIC 9(02).
               10  WS-ESC-LEVEL        PIC 9(02).
               10  WS-ESC-OWNER        PIC X(08).
      *    NOTHING YOUNGER THAN 3 BUSINESS DAYS IS ESCALATED (AUDIT
      *    FINDING CHG12966 - THE CAGE HAS 3 DAYS TO CLEAR A BREAK)
       01  WS-ESC-MIN-AGE-1            PIC 9(02)  VALUE 03.
      *----------------------------------------------------------------*
      * FIRST OWNER OF A NEW BREAK BY DEPOSITORY                       *
      *----------------------------------------------------------------*
       01  WS-OWNER-VALUES.
           05  FILLER  PIC X(12)  VALUE 'DTC CAGEDTC '.
           05  FILLER  PIC X(12)  VALUE 'FED CAGEFED '.
           05  FILLER  PIC X(12)  VALUE 'EUCLINTLOPS '.
       01  WS-OWNER-TABLE REDEFINES WS-OWNER-VALUES.
           05  WS-OWN-ENTRY OCCURS 3 TIMES INDEXED BY OWN-IDX.
               10  WS-OWN-DEPO         PIC X(04).
               10  WS-OWN-USER         PIC X(08).
       01  WS-SUB                      PIC S9(04) COMP.
       01  WS-NEW-LEVEL                PIC 9(01).
       01  WS-OLD-LEVEL                PIC 9(01).
       01  WS-NEW-AGE                  PIC S9(05) COMP-3.
       01  WS-BROWSE-KEY               PIC X(22).
       01  WS-TODAY-KEY-CNT            PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-JOBNAME                  PIC X(08)  VALUE SPACES.
       01  WS-COUNTERS.
           05  WS-POSREC-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSREC-BREAKS        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSREC-MATCHED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-NEW              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-AGED             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-REOPENED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-WRITTEN-OFF      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-CLOSED           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-ESCALATED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-OPEN-END         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-BROWSED          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BRK-CAT-CHANGED      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AGE-FALLBACK         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CARDS-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CARDS-APPLIED        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CARDS-REJECTED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LEVEL-CNT OCCURS 4 TIMES
                                       PIC S9(09) COMP-3.
           05  WS-OPEN-MV              PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-CLOSED-MV            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-OLDEST-AGE           PIC S9(05)    COMP-3 VALUE ZERO.
       COPY RCPREC.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMJILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-AGE             PIC ZZZZ9.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-RESULT THRU 2000-EXIT
               UNTIL END-OF-POSREC.
           PERFORM 3000-CLOSE-RESOLVED THRU 3000-EXIT.
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
           CALL 'CMASM01' USING JI-JOB-INFO.
           MOVE JI-JOBNAME TO WS-JOBNAME.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'POSITION BREAK MASTER MAINTENANCE STARTED'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE ZERO TO WS-LEVEL-CNT (1) WS-LEVEL-CNT (2)
                        WS-LEVEL-CNT (3) WS-LEVEL-CNT (4).
           PERFORM 1100-READ-ACTION-CARDS THRU 1100-EXIT.
           OPEN INPUT POSREC-FILE.
           IF WS-POSREC-STATUS NOT = '00'
               MOVE 'POSREC' TO AB-DDNAME
               MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O BRKMAST-FILE.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 1200-APPLY-ACTION THRU 1200-EXIT
               VARYING AC-IDX FROM 1 BY 1
               UNTIL AC-IDX > WS-ACTION-USED.
           PERFORM 8000-READ-POSREC THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CAGE ACTION CARDS (CHG36977).  DD DUMMY OR NO CARDS = NOTHING. *
      *----------------------------------------------------------------*
       1100-READ-ACTION-CARDS.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'RCB400 NO CAGE ACTION CARDS (STATUS '
                       WS-PARMCARD-STATUS ')'
               GO TO 1100-EXIT
           END-IF.
           PERFORM UNTIL END-OF-PARMS
               READ PARMCARD
               EVALUATE TRUE
                   WHEN PARMCARD-EOF
                       MOVE 'Y' TO WS-PARM-EOF-SW
                   WHEN NOT PARMCARD-OK
                       MOVE 'SYSIN' TO AB-DDNAME
                       MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '1100-READ-ACTION-CARDS' TO AB-PARAGRAPH
                       MOVE 'READ FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   WHEN PARM-CARD-REC (1:1) = '*'
                   WHEN PARM-CARD-REC = SPACES
                       CONTINUE
                   WHEN OTHER
                       PERFORM 1150-HOLD-ACTION THRU 1150-EXIT
               END-EVALUATE
           END-PERFORM.
           CLOSE PARMCARD.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1150-HOLD-ACTION.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CARDS-READ.
           DISPLAY 'RCB400 CARD: ' PARM-CARD-REC (1:50).
           IF PC-EQUAL NOT = '='
           OR (PC-KEYWORD NOT = 'WRITEOFF'
               AND PC-KEYWORD NOT = 'ASSIGN')
           OR PC-BREAK-KEY = SPACES
               ADD 1 TO WS-CARDS-REJECTED
               DISPLAY 'RCB400 CARD REJECTED - FORMAT'
               GO TO 1150-EXIT
           END-IF.
           IF PC-KEYWORD = 'ASSIGN' AND PC-OWNER = SPACES
               ADD 1 TO WS-CARDS-REJECTED
               DISPLAY 'RCB400 CARD REJECTED - NO OWNER ON ASSIGN'
               GO TO 1150-EXIT
           END-IF.
           IF WS-ACTION-USED NOT < 50
               ADD 1 TO WS-CARDS-REJECTED
               DISPLAY 'RCB400 CARD REJECTED - MORE THAN 50 CARDS'
               GO TO 1150-EXIT
           END-IF.
           ADD 1 TO WS-ACTION-USED.
           SET AC-IDX TO WS-ACTION-USED.
           MOVE PC-KEYWORD   TO WS-ACT-VERB (AC-IDX).
           MOVE PC-BREAK-KEY TO WS-ACT-KEY (AC-IDX).
           MOVE PC-OWNER     TO WS-ACT-OWNER (AC-IDX).
       1150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * APPLY ONE CARD.  ONLY OPEN BREAKS CAN BE WRITTEN OFF OR        *
      * REASSIGNED.  A WRITTEN-OFF BREAK STAYS 'W' FOR GOOD - IT IS    *
      * NEVER AGED, ESCALATED OR CLOSED AGAIN.                         *
      *----------------------------------------------------------------*
       1200-APPLY-ACTION.
           MOVE WS-ACT-KEY (AC-IDX) TO RBM-KEY.
           READ BRKMAST-FILE.
           IF BRKMAST-NOT-FOUND
               ADD 1 TO WS-CARDS-REJECTED
               DISPLAY 'RCB400 CARD REJECTED - NO BREAK ' RBM-KEY
               GO TO 1200-EXIT
           END-IF.
           IF NOT BRKMAST-OK
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '1200-APPLY-ACTION' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'READ FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF NOT RBM-OPEN
               ADD 1 TO WS-CARDS-REJECTED
               DISPLAY 'RCB400 CARD REJECTED - BREAK NOT OPEN '
                       RBM-KEY ' STATUS ' RBM-STATUS
               GO TO 1200-EXIT
           END-IF.
           MOVE SPACES TO RBM-COMMENT.
           IF WS-ACT-VERB (AC-IDX) = 'WRITEOFF'
               MOVE 'W' TO RBM-STATUS
               STRING 'WRITTEN OFF BY CAGE CARD ' DC-BUS-DATE
                      DELIMITED BY SIZE INTO RBM-COMMENT
           ELSE
               MOVE WS-ACT-OWNER (AC-IDX) TO RBM-ASSIGNED-TO
               STRING 'ASSIGNED TO ' WS-ACT-OWNER (AC-IDX) ' ON '
                      DC-BUS-DATE
                      DELIMITED BY SIZE INTO RBM-COMMENT
           END-IF.
           MOVE WS-JOBNAME TO RBM-LAST-UPD-JOB.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '1200-APPLY-ACTION' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-CARDS-APPLIED.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'CAGECARD'      TO AU-EVENT.
           MOVE 'W'             TO AU-SEVERITY.
           MOVE RBM-KEY         TO AU-KEY.
           MOVE RBM-COMMENT     TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       1200-EXIT.
           EXIT.
      *================================================================*
      * ONE RECONCILIATION RESULT                                      *
      *================================================================*
       2000-PROCESS-RESULT.
           IF RPR-IS-BREAK
               ADD 1 TO WS-POSREC-BREAKS
               PERFORM 2100-UPSERT-BREAK THRU 2100-EXIT
           ELSE
               ADD 1 TO WS-POSREC-MATCHED
           END-IF.
           PERFORM 8000-READ-POSREC THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-UPSERT-BREAK.
      *----------------------------------------------------------------*
           MOVE SPACES           TO RBM-KEY.
           MOVE 'SP'             TO RBM-BREAK-CLASS.
           MOVE RPR-DEPOSITORY   TO RBM-DEPOSITORY.
           MOVE RPR-CUSIP        TO RBM-ITEM-ID.
           READ BRKMAST-FILE.
           EVALUATE TRUE
               WHEN BRKMAST-NOT-FOUND
                   PERFORM 2200-NEW-BREAK THRU 2200-EXIT
               WHEN BRKMAST-OK
                   EVALUATE TRUE
                       WHEN RBM-OPEN
                           PERFORM 2300-AGE-BREAK THRU 2300-EXIT
                       WHEN RBM-CLOSED
                           PERFORM 2400-REOPEN-BREAK THRU 2400-EXIT
                       WHEN RBM-WRITTEN-OFF
                           PERFORM 2500-WRITTEN-OFF-SEEN THRU 2500-EXIT
                       WHEN OTHER
                           DISPLAY 'RCB400 BREAK ' RBM-KEY
                                   ' HAS STATUS ''' RBM-STATUS
                                   ''' - TREATED AS OPEN'
                           MOVE 'O' TO RBM-STATUS
                           PERFORM 2300-AGE-BREAK THRU 2300-EXIT
                   END-EVALUATE
               WHEN OTHER
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2100-UPSERT-BREAK' TO AB-PARAGRAPH
                   MOVE RBM-KEY TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-NEW-BREAK.
      *----------------------------------------------------------------*
           MOVE SPACES           TO RBM-BREAK-REC (23:).
           MOVE 'SP'             TO RBM-BREAK-CLASS.
           MOVE RPR-DEPOSITORY   TO RBM-DEPOSITORY.
           MOVE RPR-CUSIP        TO RBM-ITEM-ID.
           MOVE 'O'              TO RBM-STATUS.
           MOVE DC-BUS-DATE      TO RBM-FIRST-SEEN-DATE
                                    RBM-LAST-SEEN-DATE.
           MOVE ZERO             TO RBM-CLOSED-DATE
                                    RBM-AGE-BUS-DAYS
                                    RBM-ESCALATION-LVL
                                    RBM-STREET-AMOUNT
                                    RBM-BOOK-AMOUNT
                                    RBM-DIFF-AMOUNT.
           PERFORM 2900-REFRESH-FROM-RESULT THRU 2900-EXIT.
           PERFORM 2950-FIRST-OWNER THRU 2950-EXIT.
           STRING 'OPENED ' DC-BUS-DATE ' STMT ' RPR-STMT-DATE
                  DELIMITED BY SIZE INTO RBM-COMMENT.
           WRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2200-NEW-BREAK' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-BRK-NEW.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STILL IN BREAK - AGE, REFRESH, ESCALATE                        *
      *----------------------------------------------------------------*
       2300-AGE-BREAK.
           IF RBM-CATEGORY NOT = RPR-RESULT
               ADD 1 TO WS-BRK-CAT-CHANGED
               DISPLAY 'RCB400 BREAK ' RBM-DEPOSITORY ' ' RPR-CUSIP
                       ' CATEGORY ' RBM-CATEGORY ' -> ' RPR-RESULT
           END-IF.
           MOVE DC-BUS-DATE TO RBM-LAST-SEEN-DATE.
           PERFORM 2800-COMPUTE-AGE THRU 2800-EXIT.
           PERFORM 2900-REFRESH-FROM-RESULT THRU 2900-EXIT.
           PERFORM 2700-ESCALATE THRU 2700-EXIT.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2300-AGE-BREAK' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-BRK-AGED.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * A CLOSED BREAK IS BACK - STARTS AGAIN AS A NEW BREAK           *
      *----------------------------------------------------------------*
       2400-REOPEN-BREAK.
           MOVE 'O'              TO RBM-STATUS.
           MOVE DC-BUS-DATE      TO RBM-FIRST-SEEN-DATE
                                    RBM-LAST-SEEN-DATE.
           MOVE ZERO             TO RBM-CLOSED-DATE
                                    RBM-AGE-BUS-DAYS
                                    RBM-ESCALATION-LVL.
           PERFORM 2900-REFRESH-FROM-RESULT THRU 2900-EXIT.
           PERFORM 2950-FIRST-OWNER THRU 2950-EXIT.
           MOVE SPACES TO RBM-COMMENT.
           STRING 'REOPENED ' DC-BUS-DATE
                  DELIMITED BY SIZE INTO RBM-COMMENT.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2400-REOPEN-BREAK' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-BRK-REOPENED WS-BRK-NEW.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       2400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2500-WRITTEN-OFF-SEEN.
      *----------------------------------------------------------------*
           MOVE DC-BUS-DATE      TO RBM-LAST-SEEN-DATE.
           MOVE WS-JOBNAME       TO RBM-LAST-UPD-JOB.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '2500-WRITTEN-OFF-SEEN' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-BRK-WRITTEN-OFF.
       2500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ESCALATION - HIGHEST LEVEL WHOSE MINIMUM AGE IS REACHED.  THE  *
      * LEVEL NEVER GOES DOWN WHILE THE BREAK IS OPEN.                 *
      *----------------------------------------------------------------*
       2700-ESCALATE.
           MOVE RBM-ESCALATION-LVL TO WS-OLD-LEVEL WS-NEW-LEVEL.
           IF RBM-AGE-BUS-DAYS < WS-ESC-MIN-AGE-1
               GO TO 2700-EXIT
           END-IF.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 3
               IF RBM-AGE-BUS-DAYS >= WS-ESC-MIN-AGE (WS-SUB)
               AND WS-ESC-LEVEL (WS-SUB) > WS-NEW-LEVEL
                   MOVE WS-ESC-LEVEL (WS-SUB) TO WS-NEW-LEVEL
                   MOVE WS-ESC-OWNER (WS-SUB) TO RBM-ASSIGNED-TO
               END-IF
           END-PERFORM.
           IF WS-NEW-LEVEL > WS-OLD-LEVEL
               MOVE WS-NEW-LEVEL TO RBM-ESCALATION-LVL
               ADD 1 TO WS-BRK-ESCALATED
               MOVE WS-NEW-LEVEL TO WS-DISP-AGE
               MOVE SPACES TO RBM-COMMENT
               STRING 'ESCALATED TO LEVEL ' WS-NEW-LEVEL ' ON '
                      DC-BUS-DATE
                      DELIMITED BY SIZE INTO RBM-COMMENT
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'ESCALATE'     TO AU-EVENT
               IF WS-NEW-LEVEL > 2
                   MOVE 'E'        TO AU-SEVERITY
               ELSE
                   MOVE 'W'        TO AU-SEVERITY
               END-IF
               MOVE RBM-KEY        TO AU-KEY
               MOVE RBM-COMMENT    TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
       2700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * AGE = BUSINESS DAYS FROM FIRST SEEN TO TODAY (NEW BREAK = 0)   *
      *----------------------------------------------------------------*
       2800-COMPUTE-AGE.
           MOVE 'DIFB'              TO DT-FUNCTION.
           MOVE 'NYSE'              TO DT-CALENDAR.
           MOVE RBM-FIRST-SEEN-DATE TO DT-DATE-1.
           MOVE DC-BUS-DATE         TO DT-DATE-2.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE DT-RESULT-NUM   TO WS-NEW-AGE
           ELSE
      *        CALENDAR PROBLEM - ONE MORE DAY THAN YESTERDAY
               ADD 1 TO WS-AGE-FALLBACK
               DISPLAY 'RCB400 CMU010 DIFB RC ' DT-RETURN-CODE
                       ' FOR ' RBM-KEY ' - AGE + 1'
               COMPUTE WS-NEW-AGE = RBM-AGE-BUS-DAYS + 1
           END-IF.
           IF WS-NEW-AGE < ZERO
               MOVE ZERO TO WS-NEW-AGE
           END-IF.
           IF WS-NEW-AGE > 999
               MOVE 999 TO WS-NEW-AGE
           END-IF.
           MOVE WS-NEW-AGE TO RBM-AGE-BUS-DAYS.
       2800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2900-REFRESH-FROM-RESULT.
      *----------------------------------------------------------------*
           MOVE RPR-RESULT         TO RBM-CATEGORY.
           MOVE RPR-STREET-QTY     TO RBM-STREET-QTY.
           MOVE RPR-BOOK-QTY       TO RBM-BOOK-QTY.
           MOVE RPR-DIFF-QTY       TO RBM-DIFF-QTY.
           MOVE RPR-MKT-VALUE-USD  TO RBM-MKT-VALUE-USD.
           IF RPR-CCY = SPACES
               MOVE 'USD'          TO RBM-CCY
           ELSE
               MOVE RPR-CCY        TO RBM-CCY
           END-IF.
           MOVE WS-JOBNAME         TO RBM-LAST-UPD-JOB.
       2900-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2950-FIRST-OWNER.
      *----------------------------------------------------------------*
           MOVE 'RECON   ' TO RBM-ASSIGNED-TO.
           SET OWN-IDX TO 1.
           SEARCH WS-OWN-ENTRY
               AT END
                   CONTINUE
               WHEN WS-OWN-DEPO (OWN-IDX) = RBM-DEPOSITORY
                   MOVE WS-OWN-USER (OWN-IDX) TO RBM-ASSIGNED-TO
           END-SEARCH.
       2950-EXIT.
           EXIT.
      *================================================================*
      * PASS 2 - BROWSE THE POSITION BREAKS.  AN OPEN BREAK NOT SEEN   *
      * TODAY HAS BEEN RESOLVED.                                       *
      *================================================================*
       3000-CLOSE-RESOLVED.
           MOVE SPACES TO WS-BROWSE-KEY.
           MOVE 'SP'   TO WS-BROWSE-KEY (1:2).
           MOVE WS-BROWSE-KEY TO RBM-KEY.
           START BRKMAST-FILE KEY IS NOT LESS THAN RBM-KEY.
           EVALUATE TRUE
               WHEN BRKMAST-OK
                   CONTINUE
               WHEN BRKMAST-NOT-FOUND
                   MOVE 'Y' TO WS-BROWSE-EOF-SW
               WHEN OTHER
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '3000-CLOSE-RESOLVED' TO AB-PARAGRAPH
                   MOVE RBM-KEY TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 3100-NEXT-BREAK THRU 3100-EXIT
               UNTIL END-OF-BROWSE.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-NEXT-BREAK.
      *----------------------------------------------------------------*
           READ BRKMAST-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN BRKMAST-OK
                   CONTINUE
               WHEN BRKMAST-EOF
                   MOVE 'Y' TO WS-BROWSE-EOF-SW
                   GO TO 3100-EXIT
               WHEN OTHER
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '3100-NEXT-BREAK' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF NOT RBM-POSITION-BREAK
               MOVE 'Y' TO WS-BROWSE-EOF-SW
               GO TO 3100-EXIT
           END-IF.
           ADD 1 TO WS-BRK-BROWSED.
           IF NOT RBM-OPEN
               GO TO 3100-EXIT
           END-IF.
           IF RBM-LAST-SEEN-DATE NOT < DC-BUS-DATE
               PERFORM 3300-COUNT-OPEN THRU 3300-EXIT
               GO TO 3100-EXIT
           END-IF.
           PERFORM 3200-CLOSE-BREAK THRU 3200-EXIT.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CLOSE-BREAK.
      *----------------------------------------------------------------*
           MOVE 'C'              TO RBM-STATUS.
           MOVE DC-BUS-DATE      TO RBM-CLOSED-DATE.
           MOVE WS-JOBNAME       TO RBM-LAST-UPD-JOB.
           MOVE SPACES           TO RBM-COMMENT.
           STRING 'RESOLVED - NOT IN RECON OF ' DC-BUS-DATE
                  DELIMITED BY SIZE INTO RBM-COMMENT.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '3200-CLOSE-BREAK' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-BRK-CLOSED.
           ADD RBM-MKT-VALUE-USD TO WS-CLOSED-MV.
           MOVE RBM-AGE-BUS-DAYS TO WS-DISP-AGE.
           DISPLAY 'RCB400 BREAK CLOSED ' RBM-DEPOSITORY ' '
                   RBM-ITEM-ID (1:9) ' ' RBM-CATEGORY
                   ' AGE ' WS-DISP-AGE.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-COUNT-OPEN.
      *----------------------------------------------------------------*
           ADD 1 TO WS-BRK-OPEN-END.
           ADD RBM-MKT-VALUE-USD TO WS-OPEN-MV.
           COMPUTE WS-SUB = RBM-ESCALATION-LVL + 1.
           IF WS-SUB > ZERO AND WS-SUB < 5
               ADD 1 TO WS-LEVEL-CNT (WS-SUB)
           END-IF.
           IF RBM-AGE-BUS-DAYS > WS-OLDEST-AGE
               MOVE RBM-AGE-BUS-DAYS TO WS-OLDEST-AGE
           END-IF.
       3300-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-POSREC.
           READ POSREC-FILE INTO RPR-POSREC-REC.
           EVALUATE TRUE
               WHEN POSREC-OK
                   ADD 1 TO WS-POSREC-READ
               WHEN POSREC-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'POSREC' TO AB-DDNAME
                   MOVE WS-POSREC-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-POSREC' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF NOT END-OF-POSREC
           AND RPR-BUS-DATE NOT = DC-BUS-DATE
               MOVE 'POSREC' TO AB-DDNAME
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '8000-READ-POSREC' TO AB-PARAGRAPH
               STRING 'POSREC ' RPR-BUS-DATE ' TODAY ' DC-BUS-DATE
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'RECON RESULTS ARE NOT FROM TODAY''S RCB200 RUN'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RCB400'       TO CT-STAGE.
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
           CLOSE POSREC-FILE.
           CLOSE BRKMAST-FILE.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'POSREC-IN'       TO CT-COUNTER-NAME.
           MOVE WS-POSREC-READ    TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BRK-OPEN'        TO CT-COUNTER-NAME.
           MOVE WS-BRK-OPEN-END   TO CT-COUNT.
           MOVE WS-OPEN-MV        TO CT-AMOUNT.
           MOVE ZERO              TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BRK-NEW'         TO CT-COUNTER-NAME.
           MOVE WS-BRK-NEW        TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BRK-CLOSED'      TO CT-COUNTER-NAME.
           MOVE WS-BRK-CLOSED     TO CT-COUNT.
           MOVE WS-CLOSED-MV      TO CT-AMOUNT.
           MOVE ZERO              TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'BRK-ESCALATED'   TO CT-COUNTER-NAME.
           MOVE WS-BRK-ESCALATED  TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* RCB400 - POSITION BREAK MASTER MAINTENANCE   *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-POSREC-READ TO WS-DISP-CNT.
           DISPLAY ' RECON RESULTS READ       : ' WS-DISP-CNT.
           MOVE WS-POSREC-MATCHED TO WS-DISP-CNT.
           DISPLAY '   MATCHED                : ' WS-DISP-CNT.
           MOVE WS-POSREC-BREAKS TO WS-DISP-CNT.
           DISPLAY '   BREAKS                 : ' WS-DISP-CNT.
           MOVE WS-BRK-NEW TO WS-DISP-CNT.
           DISPLAY ' BREAKS OPENED            : ' WS-DISP-CNT.
           MOVE WS-BRK-REOPENED TO WS-DISP-CNT.
           DISPLAY '   OF WHICH REOPENED      : ' WS-DISP-CNT.
           MOVE WS-BRK-AGED TO WS-DISP-CNT.
           DISPLAY ' BREAKS AGED              : ' WS-DISP-CNT.
           MOVE WS-BRK-CAT-CHANGED TO WS-DISP-CNT.
           DISPLAY '   CATEGORY CHANGED       : ' WS-DISP-CNT.
           MOVE WS-BRK-WRITTEN-OFF TO WS-DISP-CNT.
           DISPLAY ' WRITTEN-OFF SEEN AGAIN   : ' WS-DISP-CNT.
           MOVE WS-BRK-CLOSED TO WS-DISP-CNT.
           DISPLAY ' BREAKS CLOSED            : ' WS-DISP-CNT.
           MOVE WS-BRK-ESCALATED TO WS-DISP-CNT.
           DISPLAY ' BREAKS ESCALATED         : ' WS-DISP-CNT.
           MOVE WS-BRK-OPEN-END TO WS-DISP-CNT.
           DISPLAY ' POSITION BREAKS OPEN     : ' WS-DISP-CNT.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 4
               MOVE WS-LEVEL-CNT (WS-SUB) TO WS-DISP-CNT
               COMPUTE WS-DISP-AGE = WS-SUB - 1
               DISPLAY '   ESCALATION LEVEL ' WS-DISP-AGE '  : '
                       WS-DISP-CNT
           END-PERFORM.
           MOVE WS-OLDEST-AGE TO WS-DISP-CNT.
           DISPLAY ' OLDEST OPEN BREAK (DAYS) : ' WS-DISP-CNT.
           MOVE WS-OPEN-MV TO WS-DISP-AMT.
           DISPLAY ' OPEN BREAK VALUE USD     : ' WS-DISP-AMT.
           MOVE WS-AGE-FALLBACK TO WS-DISP-CNT.
           DISPLAY ' AGE FALLBACK (NO CAL)    : ' WS-DISP-CNT.
           MOVE WS-CARDS-READ TO WS-DISP-CNT.
           DISPLAY ' CAGE ACTION CARDS READ   : ' WS-DISP-CNT.
           MOVE WS-CARDS-APPLIED TO WS-DISP-CNT.
           DISPLAY '   APPLIED                : ' WS-DISP-CNT.
           MOVE WS-CARDS-REJECTED TO WS-DISP-CNT.
           DISPLAY '   REJECTED               : ' WS-DISP-CNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'NEW OR ESCALATED POSITION BREAKS - SEE RCR410'
                               TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'POSITION BREAK MASTER UPDATED' TO AU-MESSAGE
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
           DISPLAY 'RCB400 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RCB400 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RCB400 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
