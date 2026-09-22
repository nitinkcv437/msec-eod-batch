       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCR410.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  02/01/1988.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCR410                                            *
      * TITLE      : TRADE CAPTURE REJECT REPORT                       *
      * JOB        : MSTCD060   STEP020                                *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   PRINTS ALL TRADE CAPTURE REJECTS AND WARNINGS OF THE DAY     *
      *   FOR THE OPERATIONS REPAIR DESK.  INPUT IS THE CONCATENATION  *
      *   OF THE FIVE REJECT FILES SORTED BY SOURCE, REJECT CODE AND   *
      *   TRADE ID (DUPLICATES REMOVED BY THE SORT).                   *
      *   CONTROL BREAKS: SOURCE (NEW PAGE) / REJECT CODE.             *
      *   THE RAW INPUT IMAGE IS DUMPED UNDER EACH REJECT (100 BYTES   *
      *   PER LINE) UNLESS DUMP-RAW=N.  A SUMMARY BY CODE FOLLOWS.     *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     PARAMETER CARDS TCP410A                     *
      *          REJIN     MSEC.PROD.TC.REJECTS.ALL(+1)  (TCREJCT)     *
      * OUTPUT : RPTFILE   REPORT FBA 133                (CMRPTHD)     *
      * CALLS  : CMU050 CMU060 CMU080                                  *
      *                                                                *
      * RETURN CODES: 0 NO REJECTS  4 REJECTS REPORTED                 *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1988-02-01 RJK            ORIGINAL                             *
      * 1991-05-06 RJK            BOND DESK SOURCE                     *
      * 1995-07-17 DWB  CHG01877  OMS SOURCE REPLACES EXEC TAPE        *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES ON HEADINGS              *
      * 2002-03-08 KAP  CHG09930  SEVERITY COLUMN, WARNING COUNTS      *
      * 2006-06-12 KAP  CHG15008  RAW IMAGE UP TO 300 BYTES            *
      * 2013-01-14 SPA  CHG24410  SUMMARY BY CODE WITH DESCRIPTIONS    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       SPECIAL-NAMES.
           CLASS PRINTABLE-CHAR IS 'A' THRU 'I' 'J' THRU 'R'
                                   'S' THRU 'Z' 'a' THRU 'i'
                                   'j' THRU 'r' 's' THRU 'z'
                                   '0' THRU '9' ' ' '.' ',' '-'
                                   '/' '*' '+' '(' ')' '$' '#'
                                   '@' '&' ':' ';' '=' '_' '%'
                                   '{' '}' '<' '>' '?' '!'.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD      ASSIGN TO SYSIN
                                FILE STATUS IS WS-PARMCARD-FS.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT REJIN-FILE    ASSIGN TO REJIN
                                FILE STATUS IS WS-REJIN-FS.
           SELECT RPTFILE       ASSIGN TO RPTFILE
                                FILE STATUS IS WS-RPTFILE-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY CMDATEW.
       FD  REJIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCREJCT.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-LINE                    PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCR410'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-REJIN-FS             PIC X(02).
           05  WS-RPTFILE-FS           PIC X(02).
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
           05  WS-DUMP-RAW-SW          PIC X(01)  VALUE 'Y'.
           05  WS-FIRST-SW             PIC X(01)  VALUE 'Y'.
           05  WS-CONT-SW              PIC X(01)  VALUE 'N'.
       01  WS-PARM-KEYWORD             PIC X(30).
       01  WS-PARM-VALUE               PIC X(30).
      *
       01  WS-SAVE-KEYS.
           05  WS-SAVE-SOURCE          PIC X(03)  VALUE LOW-VALUES.
           05  WS-SAVE-CODE            PIC X(04)  VALUE LOW-VALUES.
      *
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CODE-COUNT           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CODE-ERRORS          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-CODE-WARNINGS        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-SRC-COUNT            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-SRC-ERRORS           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-SRC-WARNINGS         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-TOT-ERRORS           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-TOT-WARNINGS         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-TOT-FATAL            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-CODES        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-LINES-WRITTEN        PIC S9(07) COMP-3 VALUE ZERO.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *    COUNTS BY CODE - PARALLEL TO TC-RJ-ENTRY (TCRJTAB)
       01  WS-CODE-TOTALS.
           05  WS-CT-COUNT             PIC S9(07) COMP-3
                                       OCCURS 100 TIMES.
       01  WS-CT-SUB                   PIC S9(04) COMP.
      *
       01  WS-WORK.
           05  WS-DUMP-POS             PIC S9(04) COMP.
           05  WS-DUMP-LEN             PIC S9(04) COMP.
           05  WS-DUMP-LINE-NO         PIC S9(04) COMP.
           05  WS-CHAR-SUB             PIC S9(04) COMP.
           05  WS-CODE-DESC            PIC X(36).
           05  WS-SOURCE-DESC          PIC X(40).
           05  WS-EDIT-DATE.
               10  WS-ED-MM            PIC 9(02).
               10  FILLER              PIC X(01)  VALUE '/'.
               10  WS-ED-DD            PIC 9(02).
               10  FILLER              PIC X(01)  VALUE '/'.
               10  WS-ED-CCYY          PIC 9(04).
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       COPY CMRPTHD.
      *
       01  RPT-SOURCE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE 'SOURCE:  '.
           05  RPT-SRC-CODE            PIC X(03).
           05  FILLER                  PIC X(03)  VALUE ' - '.
           05  RPT-SRC-DESC            PIC X(40).
           05  FILLER                  PIC X(76)  VALUE SPACES.
       01  RPT-CODE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(13)  VALUE 'REJECT CODE '.
           05  RPT-CODE-CODE           PIC X(04).
           05  FILLER                  PIC X(03)  VALUE ' - '.
           05  RPT-CODE-DESC           PIC X(36).
           05  FILLER                  PIC X(74)  VALUE SPACES.
       01  RPT-COLUMN-HDR.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  FILLER                  PIC X(17)  VALUE 'TRADE ID'.
           05  FILLER                  PIC X(11)  VALUE 'ACCOUNT'.
           05  FILLER                  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER                  PIC X(04)  VALUE 'SEV'.
           05  FILLER                  PIC X(09)  VALUE 'PROGRAM'.
           05  FILLER                  PIC X(09)  VALUE 'STAGE'.
           05  FILLER                  PIC X(60)  VALUE
               'REASON'.
           05  FILLER                  PIC X(08)  VALUE SPACES.
       01  RPT-DETAIL-LINE.
           05  RPT-DTL-CC              PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-DTL-TRADE-ID        PIC X(16).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DTL-ACCT            PIC X(10).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DTL-CUSIP           PIC X(09).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-DTL-SEV             PIC X(01).
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  RPT-DTL-PROGRAM         PIC X(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DTL-STAGE           PIC X(08).
           05  FILLER                  PIC X(01)  VALUE SPACE.
           05  RPT-DTL-TEXT            PIC X(60).
           05  FILLER                  PIC X(08)  VALUE SPACES.
       01  RPT-RAW-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(09)  VALUE SPACES.
           05  RPT-RAW-LABEL           PIC X(08).
           05  RPT-RAW-DATA            PIC X(100).
           05  FILLER                  PIC X(15)  VALUE SPACES.
       01  RPT-CODE-TOTAL-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  FILLER                  PIC X(12)  VALUE '** CODE '.
           05  RPT-CT-CODE             PIC X(04).
           05  FILLER                  PIC X(09)  VALUE ' TOTAL: '.
           05  RPT-CT-COUNT            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(11)  VALUE '   ERRORS: '.
           05  RPT-CT-ERRORS           PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(13)  VALUE '   WARNINGS: '.
           05  RPT-CT-WARNINGS         PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(58)  VALUE SPACES.
       01  RPT-SRC-TOTAL-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(16)  VALUE
               '*** SOURCE '.
           05  RPT-ST-SOURCE           PIC X(03).
           05  FILLER                  PIC X(10)  VALUE ' TOTAL: '.
           05  RPT-ST-COUNT            PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(11)  VALUE '   ERRORS: '.
           05  RPT-ST-ERRORS           PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(13)  VALUE '   WARNINGS: '.
           05  RPT-ST-WARNINGS         PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(58)  VALUE SPACES.
       01  RPT-SUMMARY-HDR.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  FILLER                  PIC X(08)  VALUE 'CODE'.
           05  FILLER                  PIC X(40)  VALUE 'DESCRIPTION'.
           05  FILLER                  PIC X(10)  VALUE '    COUNT'.
           05  FILLER                  PIC X(70)  VALUE SPACES.
       01  RPT-SUMMARY-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-SUM-CODE            PIC X(04).
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-SUM-DESC            PIC X(36).
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-SUM-COUNT           PIC Z,ZZZ,ZZ9.
           05  FILLER                  PIC X(71)  VALUE SPACES.
       01  RPT-GRAND-LINE.
           05  RPT-GR-CC               PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(04)  VALUE SPACES.
           05  RPT-GR-LABEL            PIC X(44).
           05  RPT-GR-COUNT            PIC Z,ZZZ,ZZ9.
           05  FILLER                  PIC X(75)  VALUE SPACES.
       01  RPT-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(40)  VALUE SPACES.
           05  FILLER                  PIC X(50)  VALUE
               '*** NO TRADE CAPTURE REJECTS FOR THIS DATE ***'.
           05  FILLER                  PIC X(42)  VALUE SPACES.
      *
       COPY TCRJTAB.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-REJECT THRU 2000-EXIT
               UNTIL WS-EOF-SW = 'Y'.
           PERFORM 3000-END-OF-REPORT THRU 3000-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
      *----------------------------------------------------------------*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-FS TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT.
           READ DATECARD-FILE.
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-FS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND THRU 9999-EXIT.
           CLOSE DATECARD-FILE.
           MOVE 'WRIT' TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID TO AU-PROGRAM.
           MOVE 'START' TO AU-EVENT.
           MOVE 'I' TO AU-SEVERITY.
           MOVE DC-BUS-DATE TO AU-BUS-DATE.
           MOVE SPACES TO AU-KEY.
           MOVE 'REJECT REPORT STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM THRU 1100-EXIT
                   UNTIL WS-PARM-EOF-SW = 'Y'
               CLOSE PARMCARD.
      *
           OPEN INPUT REJIN-FILE.
           IF WS-REJIN-FS NOT = '00'
               MOVE 'REJIN' TO AB-DDNAME
               MOVE WS-REJIN-FS TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT.
           OPEN OUTPUT RPTFILE.
           IF WS-RPTFILE-FS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-FS TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT.
      *
           PERFORM VARYING WS-CT-SUB FROM 1 BY 1 UNTIL WS-CT-SUB > 100
               MOVE ZERO TO WS-CT-COUNT (WS-CT-SUB)
           END-PERFORM.
      *
           MOVE 'TCR410' TO RPT-H1-REPORT-ID RPT-H2-PROGRAM.
           MOVE 'TRADE CAPTURE REJECTS AND WARNINGS' TO RPT-H2-TITLE.
           MOVE DC-BUS-MM TO WS-ED-MM.
           MOVE DC-BUS-DD TO WS-ED-DD.
           MOVE DC-BUS-CCYY TO WS-ED-CCYY.
           MOVE WS-EDIT-DATE TO RPT-H2-BUS-DATE.
           MOVE DC-CAL-DATE(5:2) TO WS-ED-MM.
           MOVE DC-CAL-DATE(7:2) TO WS-ED-DD.
           MOVE DC-CAL-DATE(1:4) TO WS-ED-CCYY.
           MOVE WS-EDIT-DATE TO RPT-H1-RUN-DATE.
      *
           PERFORM 8000-READ-REJECT THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARM.
           READ PARMCARD
               AT END MOVE 'Y' TO WS-PARM-EOF-SW
                      GO TO 1100-EXIT.
           IF PARM-CARD-REC (1:1) = '*'
               GO TO 1100-EXIT.
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE.
           IF WS-PARM-KEYWORD = 'DUMP-RAW'
               MOVE WS-PARM-VALUE (1:1) TO WS-DUMP-RAW-SW.
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2000 - CONTROL BREAKS AND DETAIL                               *
      *----------------------------------------------------------------*
       2000-PROCESS-REJECT.
           IF WS-FIRST-SW = 'Y'
               MOVE 'N' TO WS-FIRST-SW
               MOVE REJ-SOURCE TO WS-SAVE-SOURCE
               MOVE REJ-CODE TO WS-SAVE-CODE
               PERFORM 2100-SOURCE-HEADING THRU 2100-EXIT
               PERFORM 2200-CODE-HEADING THRU 2200-EXIT
           ELSE
               IF REJ-SOURCE NOT = WS-SAVE-SOURCE
                   PERFORM 2500-CODE-BREAK THRU 2500-EXIT
                   PERFORM 2600-SOURCE-BREAK THRU 2600-EXIT
                   MOVE REJ-SOURCE TO WS-SAVE-SOURCE
                   MOVE REJ-CODE TO WS-SAVE-CODE
                   PERFORM 2100-SOURCE-HEADING THRU 2100-EXIT
                   PERFORM 2200-CODE-HEADING THRU 2200-EXIT
               ELSE
                   IF REJ-CODE NOT = WS-SAVE-CODE
                       PERFORM 2500-CODE-BREAK THRU 2500-EXIT
                       MOVE REJ-CODE TO WS-SAVE-CODE
                       PERFORM 2200-CODE-HEADING THRU 2200-EXIT.
      *
           PERFORM 2300-DETAIL THRU 2300-EXIT.
           PERFORM 8000-READ-REJECT THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-SOURCE-HEADING.
           EVALUATE WS-SAVE-SOURCE
               WHEN 'OMS'
                   MOVE 'ORDER MANAGEMENT SYSTEM (EQUITY)'
                       TO WS-SOURCE-DESC
               WHEN 'FIX'
                   MOVE 'BOND DESK TICKETS (FIXED INCOME)'
                       TO WS-SOURCE-DESC
               WHEN 'MAN'
                   MOVE 'MANUAL TRADE CARDS (OPERATIONS)'
                       TO WS-SOURCE-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN SOURCE' TO WS-SOURCE-DESC
           END-EVALUATE.
           MOVE WS-SAVE-SOURCE TO RPT-SRC-CODE.
           MOVE WS-SOURCE-DESC TO RPT-SRC-DESC.
           MOVE 'N' TO WS-CONT-SW.
           MOVE 99 TO RPT-LINE-COUNT.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
       2100-EXIT.
           EXIT.
      *
       2200-CODE-HEADING.
           PERFORM 2210-LOOKUP-CODE THRU 2210-EXIT.
           MOVE WS-SAVE-CODE TO RPT-CODE-CODE.
           MOVE WS-CODE-DESC TO RPT-CODE-DESC.
           IF RPT-LINE-COUNT + 4 > RPT-LINES-PER-PAGE
               MOVE 'N' TO WS-CONT-SW
               MOVE 99 TO RPT-LINE-COUNT
               PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE RPT-CODE-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE RPT-COLUMN-HDR TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 3 TO RPT-LINE-COUNT.
           MOVE 'Y' TO WS-CONT-SW.
       2200-EXIT.
           EXIT.
      *
       2210-LOOKUP-CODE.
           SET TC-RJ-IDX TO 1.
           SEARCH TC-RJ-ENTRY
               AT END
                   MOVE '*** CODE NOT IN TABLE ***' TO WS-CODE-DESC
               WHEN TC-RJ-CODE (TC-RJ-IDX) = WS-SAVE-CODE
                   MOVE TC-RJ-DESC (TC-RJ-IDX) TO WS-CODE-DESC
           END-SEARCH.
       2210-EXIT.
           EXIT.
      *
       2300-DETAIL.
           ADD 1 TO WS-CODE-COUNT WS-SRC-COUNT.
           IF REJ-SEV-WARNING
               ADD 1 TO WS-CODE-WARNINGS WS-SRC-WARNINGS
                        WS-TOT-WARNINGS
           ELSE
               ADD 1 TO WS-CODE-ERRORS WS-SRC-ERRORS WS-TOT-ERRORS
               IF REJ-SEV-FATAL
                   ADD 1 TO WS-TOT-FATAL.
           PERFORM 2310-COUNT-BY-CODE THRU 2310-EXIT.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE REJ-TRADE-ID TO RPT-DTL-TRADE-ID.
           MOVE REJ-ACCT-NO TO RPT-DTL-ACCT.
           MOVE REJ-CUSIP TO RPT-DTL-CUSIP.
           MOVE REJ-SEVERITY TO RPT-DTL-SEV.
           MOVE REJ-PROGRAM TO RPT-DTL-PROGRAM.
           MOVE REJ-STAGE TO RPT-DTL-STAGE.
           MOVE REJ-TEXT TO RPT-DTL-TEXT.
           MOVE ' ' TO RPT-DTL-CC.
           MOVE RPT-DETAIL-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
           IF WS-DUMP-RAW-SW = 'Y'
               PERFORM 2400-DUMP-RAW THRU 2400-EXIT.
       2300-EXIT.
           EXIT.
      *
       2310-COUNT-BY-CODE.
           SET TC-RJ-IDX TO 1.
           SEARCH TC-RJ-ENTRY
               AT END
                   ADD 1 TO WS-UNKNOWN-CODES
               WHEN TC-RJ-CODE (TC-RJ-IDX) = REJ-CODE
                   SET WS-CT-SUB TO TC-RJ-IDX
                   ADD 1 TO WS-CT-COUNT (WS-CT-SUB)
           END-SEARCH.
       2310-EXIT.
           EXIT.
      *
      *    RAW INPUT IMAGE, 100 BYTES PER LINE
       2400-DUMP-RAW.
           IF REJ-RAW-LENGTH NOT NUMERIC
              OR REJ-RAW-LENGTH = ZERO
               GO TO 2400-EXIT.
           MOVE REJ-RAW-LENGTH TO WS-DUMP-LEN.
           IF WS-DUMP-LEN > 300
               MOVE 300 TO WS-DUMP-LEN.
           MOVE 1 TO WS-DUMP-POS.
           MOVE ZERO TO WS-DUMP-LINE-NO.
       2400-DUMP-LOOP.
           IF WS-DUMP-POS > WS-DUMP-LEN
               GO TO 2400-EXIT.
           ADD 1 TO WS-DUMP-LINE-NO.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE SPACES TO RPT-RAW-DATA.
           IF WS-DUMP-LINE-NO = 1
               MOVE 'RAW:   ' TO RPT-RAW-LABEL
           ELSE
               MOVE SPACES TO RPT-RAW-LABEL.
           IF WS-DUMP-POS + 99 > WS-DUMP-LEN
               MOVE REJ-RAW-IMAGE (WS-DUMP-POS:
                                   WS-DUMP-LEN - WS-DUMP-POS + 1)
                   TO RPT-RAW-DATA
           ELSE
               MOVE REJ-RAW-IMAGE (WS-DUMP-POS:100) TO RPT-RAW-DATA.
      *    PACKED AND BINARY FIELDS DO NOT PRINT - SHOW AS '.'
           PERFORM VARYING WS-CHAR-SUB FROM 1 BY 1
                   UNTIL WS-CHAR-SUB > 100
               IF RPT-RAW-DATA (WS-CHAR-SUB:1) IS NOT PRINTABLE-CHAR
                   MOVE '.' TO RPT-RAW-DATA (WS-CHAR-SUB:1)
               END-IF
           END-PERFORM.
           MOVE RPT-RAW-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
           ADD 100 TO WS-DUMP-POS.
           GO TO 2400-DUMP-LOOP.
       2400-EXIT.
           EXIT.
      *
       2500-CODE-BREAK.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE WS-SAVE-CODE TO RPT-CT-CODE.
           MOVE WS-CODE-COUNT TO RPT-CT-COUNT.
           MOVE WS-CODE-ERRORS TO RPT-CT-ERRORS.
           MOVE WS-CODE-WARNINGS TO RPT-CT-WARNINGS.
           MOVE RPT-CODE-TOTAL-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
           MOVE ZERO TO WS-CODE-COUNT WS-CODE-ERRORS WS-CODE-WARNINGS.
       2500-EXIT.
           EXIT.
      *
       2600-SOURCE-BREAK.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE WS-SAVE-SOURCE TO RPT-ST-SOURCE.
           MOVE WS-SRC-COUNT TO RPT-ST-COUNT.
           MOVE WS-SRC-ERRORS TO RPT-ST-ERRORS.
           MOVE WS-SRC-WARNINGS TO RPT-ST-WARNINGS.
           MOVE RPT-SRC-TOTAL-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
           MOVE ZERO TO WS-SRC-COUNT WS-SRC-ERRORS WS-SRC-WARNINGS.
       2600-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3000 - FINAL BREAKS AND SUMMARY PAGE                           *
      *----------------------------------------------------------------*
       3000-END-OF-REPORT.
           MOVE 'N' TO WS-CONT-SW.
           IF WS-RECS-READ = ZERO
               MOVE 'NONE' TO RPT-SRC-CODE
               MOVE SPACES TO RPT-SRC-DESC
               MOVE 99 TO RPT-LINE-COUNT
               PERFORM 7000-CHECK-PAGE THRU 7000-EXIT
               MOVE RPT-NONE-LINE TO RPT-LINE
               PERFORM 7100-WRITE-LINE THRU 7100-EXIT
               GO TO 3000-FINAL.
           PERFORM 2500-CODE-BREAK THRU 2500-EXIT.
           PERFORM 2600-SOURCE-BREAK THRU 2600-EXIT.
      *
           MOVE 'N' TO WS-CONT-SW.
           MOVE 'SUM' TO RPT-SRC-CODE.
           MOVE 'SUMMARY BY REJECT CODE - ALL SOURCES' TO RPT-SRC-DESC.
           MOVE 99 TO RPT-LINE-COUNT.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE RPT-SUMMARY-HDR TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 2 TO RPT-LINE-COUNT.
           PERFORM 3100-SUMMARY-LINE THRU 3100-EXIT
               VARYING WS-CT-SUB FROM 1 BY 1
               UNTIL WS-CT-SUB > TC-RJ-ENTRIES.
      *
           MOVE '0' TO RPT-GR-CC.
           MOVE 'TOTAL REJECT RECORDS (ERRORS)' TO RPT-GR-LABEL.
           MOVE WS-TOT-ERRORS TO RPT-GR-COUNT.
           MOVE RPT-GRAND-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE ' ' TO RPT-GR-CC.
           MOVE 'TOTAL WARNING RECORDS' TO RPT-GR-LABEL.
           MOVE WS-TOT-WARNINGS TO RPT-GR-COUNT.
           MOVE RPT-GRAND-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE 'RECORDS WITH CODES NOT IN TABLE' TO RPT-GR-LABEL.
           MOVE WS-UNKNOWN-CODES TO RPT-GR-COUNT.
           MOVE RPT-GRAND-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE 'TOTAL RECORDS READ' TO RPT-GR-LABEL.
           MOVE WS-RECS-READ TO RPT-GR-COUNT.
           MOVE RPT-GRAND-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
       3000-FINAL.
           MOVE RPT-END-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
       3000-EXIT.
           EXIT.
      *
       3100-SUMMARY-LINE.
           IF WS-CT-COUNT (WS-CT-SUB) = ZERO
               GO TO 3100-EXIT.
           PERFORM 7000-CHECK-PAGE THRU 7000-EXIT.
           MOVE TC-RJ-CODE (WS-CT-SUB) TO RPT-SUM-CODE.
           MOVE TC-RJ-DESC (WS-CT-SUB) TO RPT-SUM-DESC.
           MOVE WS-CT-COUNT (WS-CT-SUB) TO RPT-SUM-COUNT.
           MOVE RPT-SUMMARY-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           ADD 1 TO RPT-LINE-COUNT.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 7000 - PAGE CONTROL AND WRITE                                  *
      *----------------------------------------------------------------*
       7000-CHECK-PAGE.
           IF RPT-LINE-COUNT < RPT-LINES-PER-PAGE
               GO TO 7000-EXIT.
           ADD 1 TO RPT-PAGE-COUNT.
           MOVE RPT-PAGE-COUNT TO RPT-H1-PAGE.
           MOVE RPT-HEADING-1 TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE RPT-HEADING-2 TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE RPT-SOURCE-LINE TO RPT-LINE.
           PERFORM 7100-WRITE-LINE THRU 7100-EXIT.
           MOVE 5 TO RPT-LINE-COUNT.
           IF WS-CONT-SW = 'Y'
               MOVE WS-SAVE-CODE TO RPT-CODE-CODE
               MOVE RPT-CODE-LINE TO RPT-LINE
               PERFORM 7100-WRITE-LINE THRU 7100-EXIT
               MOVE RPT-COLUMN-HDR TO RPT-LINE
               PERFORM 7100-WRITE-LINE THRU 7100-EXIT
               ADD 3 TO RPT-LINE-COUNT.
       7000-EXIT.
           EXIT.
      *
       7100-WRITE-LINE.
           WRITE RPT-LINE.
           IF WS-RPTFILE-FS NOT = '00'
               MOVE 'RPTFILE' TO AB-DDNAME
               MOVE WS-RPTFILE-FS TO AB-FILE-STATUS
               MOVE '7100-WRITE-LINE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR THRU 9920-EXIT.
           ADD 1 TO WS-LINES-WRITTEN.
       7100-EXIT.
           EXIT.
      *
       8000-READ-REJECT.
           READ REJIN-FILE.
           IF WS-REJIN-FS = '10'
               MOVE 'Y' TO WS-EOF-SW
               GO TO 8000-EXIT.
           IF WS-REJIN-FS NOT = '00'
               MOVE 'REJIN' TO AB-DDNAME
               MOVE WS-REJIN-FS TO AB-FILE-STATUS
               MOVE '8000-READ-REJECT' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR THRU 9920-EXIT.
           ADD 1 TO WS-RECS-READ.
       8000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       9000-TERMINATE.
      *----------------------------------------------------------------*
           CLOSE REJIN-FILE RPTFILE.
           IF WS-REJIN-FS NOT = '00' OR WS-RPTFILE-FS NOT = '00'
               MOVE 'CLOSE' TO AB-DDNAME
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-REJIN-FS ' ' WS-RPTFILE-FS
                   DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND THRU 9999-EXIT.
           IF WS-RECS-READ > ZERO
               MOVE 4 TO WS-RETURN-CODE.
           MOVE 'POST' TO CT-FUNCTION.
           MOVE DC-BUS-DATE TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID TO CT-PROGRAM CT-STAGE.
           MOVE 'RECORDS-IN' TO CT-COUNTER-NAME.
           MOVE WS-RECS-READ TO CT-COUNT.
           MOVE ZERO TO CT-AMOUNT CT-QTY-HASH.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 'CTLTOTS' TO AB-DDNAME
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND THRU 9999-EXIT.
           MOVE 'CLOS' TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           MOVE 'WRIT' TO AU-FUNCTION.
           MOVE 'END' TO AU-EVENT.
           MOVE 'I' TO AU-SEVERITY.
           MOVE SPACES TO AU-MESSAGE.
           STRING 'REJECT REPORT ENDED. PAGES ' RPT-H1-PAGE
               DELIMITED BY SIZE INTO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS' TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           DISPLAY 'TCR410 - REJECT REPORT ' DC-BUS-DATE.
           DISPLAY 'TCR410 - RECORDS READ     ' WS-RECS-READ.
           DISPLAY 'TCR410 - ERRORS           ' WS-TOT-ERRORS.
           DISPLAY 'TCR410 - WARNINGS         ' WS-TOT-WARNINGS.
           DISPLAY 'TCR410 - UNKNOWN CODES    ' WS-UNKNOWN-CODES.
           DISPLAY 'TCR410 - PAGES PRINTED    ' RPT-PAGE-COUNT.
           DISPLAY 'TCR410 - LINES WRITTEN    ' WS-LINES-WRITTEN.
           DISPLAY 'TCR410 - RETURN CODE      ' WS-RETURN-CODE.
       9000-EXIT.
           EXIT.
      *
       9910-OPEN-ERROR.
           MOVE 1001 TO AB-ABEND-CODE.
           MOVE '1000-INITIALIZE' TO AB-PARAGRAPH.
           STRING 'OPEN FAILED FOR ' AB-DDNAME
               DELIMITED BY SIZE INTO AB-MESSAGE.
           PERFORM 9999-ABEND THRU 9999-EXIT.
       9910-EXIT.
           EXIT.
      *
       9920-IO-ERROR.
           MOVE 1002 TO AB-ABEND-CODE.
           STRING 'I/O ERROR ON ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
               DELIMITED BY SIZE INTO AB-MESSAGE.
           PERFORM 9999-ABEND THRU 9999-EXIT.
       9920-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'TCR410 - ABEND ' AB-ABEND-CODE ' ' AB-PARAGRAPH.
           DISPLAY 'TCR410 - ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.
       9999-EXIT.
           EXIT.
