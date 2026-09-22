       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRR210.
       AUTHOR.        M H CHEN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  NOVEMBER 2019.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRR210                                            *
      * DESCRIPTION: EVENT REPORTING SUBMISSION - CONTROL REPORT.      *
      *              READS THE PIPE-DELIMITED SUBMISSION WRITTEN BY    *
      *              RRB200 BACK IN, SPLITS EVERY LINE INTO ITS FIELDS *
      *              AND PRINTS:                                       *
      *                1. THE HEADER LINE                              *
      *                2. EVERY EVENT (ONE PRINT LINE PER E LINE)      *
      *                3. COUNTS BY EVENT TYPE, SIDE AND SESSION       *
      *                4. CONTROL PROOF - HEADER COUNT, EVENTS READ    *
      *                   AND TRAILER COUNT; QTY HASH RECOMPUTED FROM  *
      *                   THE SUBMITTED TEXT AGAINST THE TRAILER.      *
      *              OPERATIONS CHECKS THE PROOF BEFORE THE FILE IS    *
      *              RELEASED TO THE TRANSMISSION QUEUE (RUNBOOK       *
      *              RR-020).                                          *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRRD020 / STEP020                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              CATIN    - MSEC.PROD.RR.CATSUB(+1)      (RRCATL)  *
      * OUTPUT     : RPTFILE  - REPORT, FB 133 ASA                     *
      * CALLS      : CMU050, CMU060, CMU080, CMASM02                   *
      * RETURN CODE: 0 PROVED, 4 MALFORMED OR UNKNOWN LINES,           *
      *              8 COUNTS OR HASH DO NOT AGREE - DO NOT RELEASE    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2019-11-04 MHC  ORIGINAL                              CHG34410 *
      * 2020-06-22 MHC  HEADER / TRAILER PROOF                CHG35015 *
      * 2020-11-09 JLR  EVENT TYPE COUNTS                     CHG35388 *
      * 2022-05-02 JLR  SESSION COUNTS                        CHG37120 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT CATIN-FILE     ASSIGN TO CATIN
                  FILE STATUS IS WS-CATIN-STATUS.
           SELECT RPTFILE        ASSIGN TO RPTFILE
                  FILE STATUS IS WS-RPTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  CATIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RRCATL.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRR210'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-CATIN-STATUS         PIC X(02)  VALUE '00'.
               88  CATIN-OK                       VALUE '00'.
               88  CATIN-EOF                      VALUE '10'.
           05  WS-RPTFILE-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-SUBMISSION              VALUE 'Y'.
           05  WS-HEADER-SW            PIC X(01)  VALUE 'N'.
               88  HEADER-SEEN                    VALUE 'Y'.
           05  WS-TRAILER-SW           PIC X(01)  VALUE 'N'.
               88  TRAILER-SEEN                   VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * FIELDS OF ONE LINE                                             *
      *----------------------------------------------------------------*
       01  WS-FIELD-COUNT              PIC S9(04) COMP.
       01  WS-LINE-FIELDS.
           05  WS-F-TYPE               PIC X(01).
           05  WS-F-SEQ                PIC X(10).
           05  WS-F-EVENT              PIC X(03).
           05  WS-F-REF                PIC X(16).
           05  WS-F-TS                 PIC X(19).
           05  WS-F-SYMBOL             PIC X(08).
           05  WS-F-SIDE               PIC X(02).
           05  WS-F-QTY                PIC X(20).
           05  WS-F-PRICE              PIC X(20).
           05  WS-F-ACCT               PIC X(01).
           05  WS-F-CAP                PIC X(01).
           05  WS-F-DESK               PIC X(04).
           05  WS-F-DEPT               PIC X(01).
           05  WS-F-SESSION            PIC X(04).
           05  WS-F-HANDLING           PIC X(03).
       01  WS-HEADER-FIELDS.
           05  WS-H-TYPE               PIC X(01).
           05  WS-H-FILE-ID            PIC X(30).
           05  WS-H-FIRM               PIC X(10).
           05  WS-H-SUBMIT             PIC X(08).
           05  WS-H-COUNT              PIC X(12).
       01  WS-TRAILER-FIELDS.
           05  WS-T-TYPE               PIC X(01).
           05  WS-T-COUNT              PIC X(12).
           05  WS-T-HASH               PIC X(24).
       01  WS-EXPECTED-E-FIELDS        PIC S9(04) COMP  VALUE 15.
      *----------------------------------------------------------------*
      * TOTALS                                                         *
      *----------------------------------------------------------------*
       01  WS-TOTALS.
           05  WS-LINES-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EVENTS-READ          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MALFORMED            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-LINES        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HEADER-COUNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRAILER-COUNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRAILER-HASH         PIC S9(13)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-QTY-HASH             PIC S9(13)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-QTY-NUM              PIC S9(13)V9(04) COMP-3.
           05  WS-PROOF-ERRORS         PIC S9(04) COMP   VALUE ZERO.
      *----------------------------------------------------------------*
      * COUNT TABLES - EVENT TYPE, SIDE, SESSION                       *
      *----------------------------------------------------------------*
       01  WS-COUNT-TABLE.
           05  WS-CT-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-CT-ENTRY             OCCURS 40 TIMES
                                       INDEXED BY CT-IDX.
               10  WS-CT-GROUP         PIC X(08).
               10  WS-CT-VALUE         PIC X(04).
               10  WS-CT-COUNT         PIC S9(09) COMP-3.
               10  WS-CT-QTY           PIC S9(13)V9(04) COMP-3.
       01  WS-CT-MAX                   PIC S9(04) COMP  VALUE 40.
       01  WS-CT-KEY.
           05  WS-CK-GROUP             PIC X(08).
           05  WS-CK-VALUE             PIC X(04).
       01  WS-SUB                      PIC S9(04) COMP.
       01  WS-PREV-GROUP               PIC X(08).
       01  WS-GRP-SUB                  PIC S9(04) COMP.
       01  WS-FIRST-OF-GROUP-SW        PIC X(01).
           88  FIRST-OF-GROUP                     VALUE 'Y'.
       01  WS-GROUP-VALUES.
           05  FILLER                  PIC X(08)  VALUE 'EVENT'.
           05  FILLER                  PIC X(08)  VALUE 'SIDE'.
           05  FILLER                  PIC X(08)  VALUE 'SESSION'.
           05  FILLER                  PIC X(08)  VALUE 'ACCTTYPE'.
       01  WS-GROUP-TABLE REDEFINES WS-GROUP-VALUES.
           05  WS-GROUP-NAME           PIC X(08)  OCCURS 4 TIMES.
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(09)  VALUE '      SEQ'.
           05  FILLER  PIC X(05)  VALUE ' TYP'.
           05  FILLER  PIC X(18)  VALUE ' FIRM REFERENCE'.
           05  FILLER  PIC X(21)  VALUE ' EVENT TIME (UTC)'.
           05  FILLER  PIC X(10)  VALUE ' SYMBOL'.
           05  FILLER  PIC X(04)  VALUE ' SD'.
           05  FILLER  PIC X(21)  VALUE '             QUANTITY'.
           05  FILLER  PIC X(21)  VALUE '                PRICE'.
           05  FILLER  PIC X(23)  VALUE ' A C DESK D SESS HND'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(09)  VALUE ' --------'.
           05  FILLER  PIC X(05)  VALUE ' ---'.
           05  FILLER  PIC X(18)  VALUE ' ----------------'.
           05  FILLER  PIC X(21)  VALUE ' -------------------'.
           05  FILLER  PIC X(10)  VALUE ' --------'.
           05  FILLER  PIC X(04)  VALUE ' --'.
           05  FILLER  PIC X(21)  VALUE ' --------------------'.
           05  FILLER  PIC X(21)  VALUE ' --------------------'.
           05  FILLER  PIC X(23)  VALUE ' - - ---- - ---- ---'.
       01  WS-EVENT-LINE.
           05  EL-CC                   PIC X(01).
           05  EL-SEQ                  PIC X(09) JUSTIFIED RIGHT.
           05  FILLER                  PIC X(01).
           05  EL-EVENT                PIC X(03).
           05  FILLER                  PIC X(02).
           05  EL-REF                  PIC X(16).
           05  FILLER                  PIC X(02).
           05  EL-TS                   PIC X(19).
           05  FILLER                  PIC X(02).
           05  EL-SYMBOL               PIC X(08).
           05  FILLER                  PIC X(02).
           05  EL-SIDE                 PIC X(02).
           05  FILLER                  PIC X(01).
           05  EL-QTY                  PIC X(20) JUSTIFIED RIGHT.
           05  FILLER                  PIC X(01).
           05  EL-PRICE                PIC X(20) JUSTIFIED RIGHT.
           05  FILLER                  PIC X(02).
           05  EL-ACCT                 PIC X(01).
           05  FILLER                  PIC X(01).
           05  EL-CAP                  PIC X(01).
           05  FILLER                  PIC X(01).
           05  EL-DESK                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  EL-DEPT                 PIC X(01).
           05  FILLER                  PIC X(01).
           05  EL-SESSION              PIC X(04).
           05  FILLER                  PIC X(01).
           05  EL-HANDLING             PIC X(03).
           05  FILLER                  PIC X(02).
       01  WS-TEXT-LINE.
           05  TX-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  TX-TEXT                 PIC X(129).
       01  WS-COUNT-LINE.
           05  CL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  CL-GROUP                PIC X(10).
           05  CL-VALUE                PIC X(06).
           05  CL-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(04).
           05  CL-QTY                  PIC ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(75).
       01  WS-PROOF-LINE.
           05  PL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  PL-LABEL                PIC X(36).
           05  PL-VALUE-1              PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(03).
           05  PL-VALUE-2              PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(03).
           05  PL-RESULT               PIC X(30).
           05  FILLER                  PIC X(11).
       01  WS-MALFORMED-LINE.
           05  ML-CC                   PIC X(01).
           05  FILLER                  PIC X(03).
           05  ML-LABEL                PIC X(14).
           05  ML-TEXT                 PIC X(115).
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE.
           PERFORM 8000-READ-LINE.
           PERFORM UNTIL END-OF-SUBMISSION
               PERFORM 2000-PROCESS-LINE
               PERFORM 8000-READ-LINE
           END-PERFORM.
           PERFORM 4000-PRINT-COUNTS.
           PERFORM 5000-CONTROL-PROOF.
           PERFORM 9000-TERMINATE.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'EVENT SUBMISSION REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT CATIN-FILE.
           IF WS-CATIN-STATUS NOT = '00'
               MOVE 'CATIN'            TO AB-DDNAME
               MOVE WS-CATIN-STATUS    TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS  TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP (1:10) TO RPT-H1-RUN-DATE.
           MOVE WS-PROGRAM-ID  TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'EVENT REPORTING SUBMISSION - CONTROL REPORT'
                               TO RPT-H2-TITLE.
           MOVE DC-BUS-DATE    TO WS-DATE-IN.
           MOVE WS-DI-CCYY     TO WS-DE-CCYY.
           MOVE WS-DI-MM       TO WS-DE-MM.
           MOVE WS-DI-DD       TO WS-DE-DD.
           MOVE WS-DATE-EDIT   TO RPT-H2-BUS-DATE.
           PERFORM 8200-HEADINGS.
      *================================================================*
      * ONE SUBMISSION LINE                                            *
      *================================================================*
       2000-PROCESS-LINE.
           ADD 1 TO WS-LINES-READ.
           EVALUATE RRC-LINE-TYPE
               WHEN 'H'
                   PERFORM 2100-HEADER-LINE
               WHEN 'E'
                   PERFORM 2200-EVENT-LINE
               WHEN 'T'
                   PERFORM 2300-TRAILER-LINE
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-LINES
                   MOVE 'UNKNOWN LINE: ' TO ML-LABEL
                   PERFORM 2900-PRINT-BAD-LINE
           END-EVALUATE.
      *----------------------------------------------------------------*
       2100-HEADER-LINE.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-HEADER-FIELDS.
           MOVE ZERO   TO WS-FIELD-COUNT.
           UNSTRING RRC-SUBMISSION-LINE DELIMITED BY '|'
               INTO WS-H-TYPE WS-H-FILE-ID WS-H-FIRM
                    WS-H-SUBMIT WS-H-COUNT
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING.
           IF HEADER-SEEN OR WS-FIELD-COUNT NOT = 5
               ADD 1 TO WS-MALFORMED
               MOVE 'BAD HEADER  : ' TO ML-LABEL
               PERFORM 2900-PRINT-BAD-LINE
           END-IF.
           MOVE 'Y' TO WS-HEADER-SW.
           COMPUTE WS-HEADER-COUNT = FUNCTION NUMVAL (WS-H-COUNT).
           MOVE SPACES TO WS-TEXT-LINE.
           MOVE '0'    TO TX-CC.
           STRING 'FILE ID: '     DELIMITED BY SIZE
                  WS-H-FILE-ID    DELIMITED BY SPACE
                  '   FIRM: '     DELIMITED BY SIZE
                  WS-H-FIRM       DELIMITED BY SPACE
                  '   SUBMIT DATE: ' DELIMITED BY SIZE
                  WS-H-SUBMIT     DELIMITED BY SPACE
                  '   HEADER RECORD COUNT: ' DELIMITED BY SIZE
                  WS-H-COUNT      DELIMITED BY SPACE
               INTO TX-TEXT
           END-STRING.
           PERFORM 8100-PRINT-TEXT.
      *----------------------------------------------------------------*
      * E LINE - FIFTEEN FIELDS.  EMPTY FIELDS ARE TWO ADJACENT PIPES. *
      *----------------------------------------------------------------*
       2200-EVENT-LINE.
           ADD 1 TO WS-EVENTS-READ.
           MOVE SPACES TO WS-LINE-FIELDS.
           MOVE ZERO   TO WS-FIELD-COUNT.
           UNSTRING RRC-SUBMISSION-LINE DELIMITED BY '|'
               INTO WS-F-TYPE WS-F-SEQ WS-F-EVENT WS-F-REF WS-F-TS
                    WS-F-SYMBOL WS-F-SIDE WS-F-QTY WS-F-PRICE
                    WS-F-ACCT WS-F-CAP WS-F-DESK WS-F-DEPT
                    WS-F-SESSION WS-F-HANDLING
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING.
           IF WS-FIELD-COUNT NOT = WS-EXPECTED-E-FIELDS
               ADD 1 TO WS-MALFORMED
               MOVE 'BAD EVENT   : ' TO ML-LABEL
               PERFORM 2900-PRINT-BAD-LINE
               EXIT PARAGRAPH
           END-IF.
           IF FUNCTION TEST-NUMVAL (WS-F-QTY) = ZERO
               COMPUTE WS-QTY-NUM = FUNCTION NUMVAL (WS-F-QTY)
           ELSE
               MOVE ZERO TO WS-QTY-NUM
               ADD 1 TO WS-MALFORMED
               MOVE 'BAD QUANTITY: ' TO ML-LABEL
               PERFORM 2900-PRINT-BAD-LINE
           END-IF.
           ADD WS-QTY-NUM TO WS-QTY-HASH.
           MOVE 'EVENT' TO WS-CK-GROUP.
           MOVE WS-F-EVENT TO WS-CK-VALUE.
           PERFORM 3000-ADD-COUNT.
           MOVE 'SIDE' TO WS-CK-GROUP.
           MOVE WS-F-SIDE TO WS-CK-VALUE.
           PERFORM 3000-ADD-COUNT.
           MOVE 'SESSION' TO WS-CK-GROUP.
           MOVE WS-F-SESSION TO WS-CK-VALUE.
           PERFORM 3000-ADD-COUNT.
           MOVE 'ACCTTYPE' TO WS-CK-GROUP.
           MOVE WS-F-ACCT TO WS-CK-VALUE.
           PERFORM 3000-ADD-COUNT.
           MOVE SPACES        TO WS-EVENT-LINE.
           MOVE ' '           TO EL-CC.
           MOVE FUNCTION TRIM (WS-F-SEQ)   TO EL-SEQ.
           MOVE WS-F-EVENT    TO EL-EVENT.
           MOVE WS-F-REF      TO EL-REF.
           MOVE WS-F-TS       TO EL-TS.
           MOVE WS-F-SYMBOL   TO EL-SYMBOL.
           MOVE WS-F-SIDE     TO EL-SIDE.
           MOVE FUNCTION TRIM (WS-F-QTY)   TO EL-QTY.
           MOVE FUNCTION TRIM (WS-F-PRICE) TO EL-PRICE.
           MOVE WS-F-ACCT     TO EL-ACCT.
           MOVE WS-F-CAP      TO EL-CAP.
           MOVE WS-F-DESK     TO EL-DESK.
           MOVE WS-F-DEPT     TO EL-DEPT.
           MOVE WS-F-SESSION  TO EL-SESSION.
           MOVE WS-F-HANDLING TO EL-HANDLING.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-EVENT-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       2300-TRAILER-LINE.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-TRAILER-FIELDS.
           MOVE ZERO   TO WS-FIELD-COUNT.
           UNSTRING RRC-SUBMISSION-LINE DELIMITED BY '|'
               INTO WS-T-TYPE WS-T-COUNT WS-T-HASH
               TALLYING IN WS-FIELD-COUNT
           END-UNSTRING.
           IF TRAILER-SEEN OR WS-FIELD-COUNT NOT = 3
               ADD 1 TO WS-MALFORMED
               MOVE 'BAD TRAILER : ' TO ML-LABEL
               PERFORM 2900-PRINT-BAD-LINE
           END-IF.
           MOVE 'Y' TO WS-TRAILER-SW.
           COMPUTE WS-TRAILER-COUNT = FUNCTION NUMVAL (WS-T-COUNT).
           COMPUTE WS-TRAILER-HASH  = FUNCTION NUMVAL (WS-T-HASH).
      *----------------------------------------------------------------*
       2900-PRINT-BAD-LINE.
      *----------------------------------------------------------------*
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
           MOVE SPACES TO WS-MALFORMED-LINE.
           MOVE ' '    TO ML-CC.
           MOVE RRC-SUBMISSION-LINE (1:115) TO ML-TEXT.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-MALFORMED-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
      * ADD ONE TO THE COUNT TABLE ENTRY FOR WS-CT-KEY                 *
      *----------------------------------------------------------------*
       3000-ADD-COUNT.
           SET CT-IDX TO 1.
           SEARCH WS-CT-ENTRY
               AT END
                   PERFORM 3100-NEW-COUNT
               WHEN CT-IDX > WS-CT-USED
                   PERFORM 3100-NEW-COUNT
               WHEN WS-CT-GROUP (CT-IDX) = WS-CK-GROUP
                AND WS-CT-VALUE (CT-IDX) = WS-CK-VALUE
                   ADD 1          TO WS-CT-COUNT (CT-IDX)
                   ADD WS-QTY-NUM TO WS-CT-QTY (CT-IDX)
           END-SEARCH.
      *----------------------------------------------------------------*
       3100-NEW-COUNT.
      *----------------------------------------------------------------*
           IF WS-CT-USED NOT < WS-CT-MAX
               DISPLAY 'RRR210 W - COUNT TABLE FULL ' WS-CT-KEY
               EXIT PARAGRAPH
           END-IF.
           ADD 1 TO WS-CT-USED.
           MOVE WS-CK-GROUP TO WS-CT-GROUP (WS-CT-USED).
           MOVE WS-CK-VALUE TO WS-CT-VALUE (WS-CT-USED).
           MOVE 1           TO WS-CT-COUNT (WS-CT-USED).
           MOVE WS-QTY-NUM  TO WS-CT-QTY (WS-CT-USED).
      *================================================================*
      * COUNTS BY EVENT TYPE, SIDE, SESSION, ACCOUNT TYPE              *
      *================================================================*
       4000-PRINT-COUNTS.
           MOVE 'EVENT REPORTING SUBMISSION - EVENT COUNTS'
                               TO RPT-H2-TITLE.
           PERFORM 8250-PAGE-HEADINGS.
           PERFORM VARYING WS-GRP-SUB FROM 1 BY 1
                   UNTIL WS-GRP-SUB > 4
               MOVE WS-GROUP-NAME (WS-GRP-SUB) TO WS-PREV-GROUP
               MOVE 'Y' TO WS-FIRST-OF-GROUP-SW
               PERFORM VARYING WS-SUB FROM 1 BY 1
                       UNTIL WS-SUB > WS-CT-USED
                   IF WS-CT-GROUP (WS-SUB) = WS-PREV-GROUP
                       PERFORM 4100-PRINT-COUNT-LINE
                   END-IF
               END-PERFORM
           END-PERFORM.
      *----------------------------------------------------------------*
       4100-PRINT-COUNT-LINE.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-COUNT-LINE.
           MOVE ' '    TO CL-CC.
           IF FIRST-OF-GROUP
               MOVE '0' TO CL-CC
               MOVE WS-CT-GROUP (WS-SUB) TO CL-GROUP
               MOVE 'N' TO WS-FIRST-OF-GROUP-SW
               ADD 1 TO RPT-LINE-COUNT
           END-IF.
           MOVE WS-CT-VALUE (WS-SUB) TO CL-VALUE.
           IF CL-VALUE = SPACES
               MOVE '(NONE)' TO CL-VALUE
           END-IF.
           MOVE WS-CT-COUNT (WS-SUB) TO CL-COUNT.
           MOVE WS-CT-QTY (WS-SUB)   TO CL-QTY.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8250-PAGE-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-COUNT-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *================================================================*
      * CONTROL PROOF                                                  *
      *================================================================*
       5000-CONTROL-PROOF.
           IF RPT-LINE-COUNT + 10 > RPT-LINES-PER-PAGE
               PERFORM 8250-PAGE-HEADINGS
           END-IF.
           MOVE SPACES TO WS-TEXT-LINE.
           MOVE '-'    TO TX-CC.
           MOVE 'CONTROL PROOF - HEADER / EVENTS / TRAILER' TO TX-TEXT.
           PERFORM 8100-PRINT-TEXT.
           IF NOT HEADER-SEEN OR NOT TRAILER-SEEN
               MOVE SPACES TO WS-TEXT-LINE
               MOVE '0'    TO TX-CC
               MOVE '*** HEADER OR TRAILER LINE MISSING ***' TO TX-TEXT
               PERFORM 8100-PRINT-TEXT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           MOVE SPACES            TO WS-PROOF-LINE.
           MOVE '0'               TO PL-CC.
           MOVE 'HEADER COUNT / EVENTS READ' TO PL-LABEL.
           MOVE WS-HEADER-COUNT   TO PL-VALUE-1.
           MOVE WS-EVENTS-READ    TO PL-VALUE-2.
           IF WS-HEADER-COUNT = WS-EVENTS-READ
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DO NOT AGREE ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF.
           MOVE SPACES            TO WS-PROOF-LINE.
           MOVE ' '               TO PL-CC.
           MOVE 'TRAILER COUNT / EVENTS READ' TO PL-LABEL.
           MOVE WS-TRAILER-COUNT  TO PL-VALUE-1.
           MOVE WS-EVENTS-READ    TO PL-VALUE-2.
           IF WS-TRAILER-COUNT = WS-EVENTS-READ
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DO NOT AGREE ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF.
           MOVE SPACES            TO WS-PROOF-LINE.
           MOVE ' '               TO PL-CC.
           MOVE 'TRAILER QTY HASH / QTY AS SUBMITTED' TO PL-LABEL.
           MOVE WS-TRAILER-HASH   TO PL-VALUE-1.
           MOVE WS-QTY-HASH       TO PL-VALUE-2.
           IF WS-TRAILER-HASH = WS-QTY-HASH
               MOVE 'AGREE' TO PL-RESULT
           ELSE
               MOVE '*** DO NOT AGREE ***' TO PL-RESULT
               ADD 1 TO WS-PROOF-ERRORS
           END-IF.
           PERFORM 8150-PRINT-PROOF.
           MOVE SPACES            TO WS-PROOF-LINE.
           MOVE ' '               TO PL-CC.
           MOVE 'MALFORMED / UNKNOWN LINES' TO PL-LABEL.
           MOVE WS-MALFORMED      TO PL-VALUE-1.
           MOVE WS-UNKNOWN-LINES  TO PL-VALUE-2.
           PERFORM 8150-PRINT-PROOF.
           MOVE SPACES TO WS-TEXT-LINE.
           MOVE '0'    TO TX-CC.
           IF WS-PROOF-ERRORS > ZERO
               MOVE 8 TO WS-RETURN-CODE
               MOVE '*** SUBMISSION DOES NOT PROVE - DO NOT RELEASE ***'
                           TO TX-TEXT
           ELSE
               MOVE 'SUBMISSION PROVED - RELEASE TO TRANSMISSION QUEUE'
                           TO TX-TEXT
           END-IF.
           PERFORM 8100-PRINT-TEXT.
           WRITE RPT-RECORD FROM RPT-END-LINE.
           PERFORM 8900-CHECK-WRITE.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-LINE.
           READ CATIN-FILE.
           EVALUATE TRUE
               WHEN CATIN-OK
                   CONTINUE
               WHEN CATIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'CATIN'           TO AB-DDNAME
                   MOVE WS-CATIN-STATUS   TO AB-FILE-STATUS
                   MOVE 1002              TO AB-ABEND-CODE
                   MOVE 'READ FAILED'     TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8100-PRINT-TEXT.
      *----------------------------------------------------------------*
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8250-PAGE-HEADINGS
           END-IF.
           WRITE RPT-RECORD FROM WS-TEXT-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 2 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8150-PRINT-PROOF.
      *----------------------------------------------------------------*
           WRITE RPT-RECORD FROM WS-PROOF-LINE.
           PERFORM 8900-CHECK-WRITE.
           ADD 1 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8200-HEADINGS.
      *----------------------------------------------------------------*
           PERFORM 8250-PAGE-HEADINGS.
           WRITE RPT-RECORD FROM WS-COL-HEAD-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM WS-COL-HEAD-2.
           PERFORM 8900-CHECK-WRITE.
           ADD 3 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8250-PAGE-HEADINGS.
      *----------------------------------------------------------------*
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           WRITE RPT-RECORD FROM RPT-HEADING-1.
           PERFORM 8900-CHECK-WRITE.
           WRITE RPT-RECORD FROM RPT-HEADING-2.
           PERFORM 8900-CHECK-WRITE.
           MOVE 2 TO RPT-LINE-COUNT.
      *----------------------------------------------------------------*
       8900-CHECK-WRITE.
      *----------------------------------------------------------------*
           IF WS-RPTFILE-STATUS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-STATUS  TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE CATIN-FILE RPTFILE.
           MOVE 'POST'          TO CT-FUNCTION.
           MOVE DC-BUS-DATE     TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID   TO CT-PROGRAM.
           MOVE 'RRR210'        TO CT-STAGE.
           MOVE 'CATSUB-IN'     TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-READ  TO CT-COUNT.
           MOVE ZERO            TO CT-AMOUNT.
           MOVE WS-QTY-HASH     TO CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'CLOS'          TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           DISPLAY 'RRR210 LINES READ      : ' WS-LINES-READ.
           DISPLAY 'RRR210 EVENTS READ     : ' WS-EVENTS-READ.
           DISPLAY 'RRR210 HEADER COUNT    : ' WS-HEADER-COUNT.
           DISPLAY 'RRR210 TRAILER COUNT   : ' WS-TRAILER-COUNT.
           DISPLAY 'RRR210 MALFORMED LINES : ' WS-MALFORMED.
           DISPLAY 'RRR210 UNKNOWN LINES   : ' WS-UNKNOWN-LINES.
           DISPLAY 'RRR210 PROOF ERRORS    : ' WS-PROOF-ERRORS.
           DISPLAY 'RRR210 RETURN CODE     : ' WS-RETURN-CODE.
           MOVE 'WRIT'          TO AU-FUNCTION.
           MOVE 'END'           TO AU-EVENT.
           IF WS-RETURN-CODE > 4
               MOVE 'E' TO AU-SEVERITY
               MOVE 'EVENT SUBMISSION DOES NOT PROVE - NOT RELEASED'
                               TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'EVENT SUBMISSION REPORT ENDED' TO AU-MESSAGE
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'          TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'RRR210 ABENDING - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
