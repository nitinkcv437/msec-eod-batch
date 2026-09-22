       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SWB050.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  02/09/1998.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SWB050                                            *
      * TITLE      : SETTLEMENT INSTRUCTION PRE-VALIDATION             *
      * JOB        : MSSWD010  STEP010  (IKJEFT01 - DB2 PLAN MSSWPLN)  *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   EDITS EVERY SETTLEMENT INSTRUCTION EXTRACTED BY TCB500       *
      *   BEFORE IT IS FORMATTED AS A SWIFT MT541/MT543 (SWB100).      *
      *   THE CUSTODIAN CHARGES FOR EVERY REJECTED MESSAGE (NAK AND    *
      *   IPRC//REJT), SO FORMAT ERRORS ARE STOPPED HERE:              *
      *     - MESSAGE TYPE / FUNCTION / REFERENCE                      *
      *     - CUSIP CHECK DIGIT, ISIN CHECK DIGIT, ISIN VS CUSIP,      *
      *       ISIN KNOWN TO THE SECURITY MASTER (CMD010 GETI)          *
      *     - QUANTITY, AMOUNT, CURRENCY, DATES                        *
      *     - DEPOSITORY, PLACE OF SETTLEMENT, AGENT BIC FORMAT,       *
      *       SAFEKEEPING ACCOUNT, DTC CONTRA PARTICIPANT              *
      *     - DUPLICATE REFERENCE IN THE SAME FILE                     *
      *   ERRORS (E) STOP THE INSTRUCTION, WARNINGS (W) ARE REPORTED   *
      *   AND THE INSTRUCTION GOES FORWARD.  EVERY EXCEPTION IS        *
      *   WRITTEN TO INSTREJ AND LISTED ON THE EXCEPTION REPORT.       *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD             (CMDATEW)    *
      *          SYSIN     PARAMETERS SWP050A  (MAXREJ=NNNN)           *
      *          SETLIN    MSEC.PROD.TC.SETLINST(0)       (TCSETIN)    *
      * OUTPUT : INSTOK    MSEC.PROD.SW.INSTVAL(+1)       (TCSETIN)    *
      *          INSTREJ   MSEC.PROD.SW.INSTREJ(+1)       (SWVREJ)     *
      *          RPTFILE   EXCEPTION REPORT SWB050 (FB 133 ASA)        *
      * CALLS  : CMD010 CMU010 CMU050 CMU060 CMU080 CMASM02            *
      *                                                                *
      * RETURN CODES: 0 CLEAN (WARNINGS ONLY)                          *
      *               4 INSTRUCTIONS REJECTED                          *
      *               8 MORE REJECTS THAN MAXREJ - SUSPECT EXTRACT,    *
      *                 MSSWD010 DOES NOT SEND (COND=(4,LT))           *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1998-02-09 DWB  CHG03512  ORIGINAL - CUSTODIAN NAK REDUCTION   *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD, CMU010 VALD    *
      * 2001-04-09 KAP  CHG08820  DECIMALIZATION - QTY 4 DECIMALS      *
      * 2005-08-30 KAP  CHG13391  ISIN CHECKS (V006-V009)              *
      * 2009-12-14 SPA  CHG19002  EUROCLEAR - EUR/GBP/CHF/JPY/CAD      *
      * 2012-07-23 SPA  CHG23918  TEST BIC (LOCATION X0) REJECTED      *
      * 2016-10-03 SPA  CHG30112  DTC CONTRA PARTICIPANT EDIT (V020)   *
      * 2019-05-13 MHC  CHG34410  MAXREJ PARAMETER, RC 8               *
      * 2024-02-12 NVR  CHG41007  T+1 - STALE SETTLE DATE 30 DAYS      *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMCARD      ASSIGN TO SYSIN
                                FILE STATUS IS WS-PARMCARD-FS.
           SELECT DATECARD-FILE ASSIGN TO DATECARD
                                FILE STATUS IS WS-DATECARD-FS.
           SELECT SETLIN-FILE   ASSIGN TO SETLIN
                                FILE STATUS IS WS-SETLIN-FS.
           SELECT INSTOK-FILE   ASSIGN TO INSTOK
                                FILE STATUS IS WS-INSTOK-FS.
           SELECT INSTREJ-FILE  ASSIGN TO INSTREJ
                                FILE STATUS IS WS-INSTREJ-FS.
           SELECT RPTFILE       ASSIGN TO RPTFILE
                                FILE STATUS IS WS-RPTFILE-FS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  SETLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY TCSETIN.
       FD  INSTOK-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  INSTOK-REC                  PIC X(250).
       FD  INSTREJ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SWVREJ.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SWB050'.
       01  WS-FILE-STATUSES.
           05  WS-PARMCARD-FS          PIC X(02).
           05  WS-DATECARD-FS          PIC X(02).
           05  WS-SETLIN-FS            PIC X(02).
               88  SETLIN-OK                     VALUE '00'.
               88  SETLIN-EOF                    VALUE '10'.
           05  WS-INSTOK-FS            PIC X(02).
               88  INSTOK-OK                     VALUE '00'.
           05  WS-INSTREJ-FS           PIC X(02).
               88  INSTREJ-OK                    VALUE '00'.
           05  WS-RPTFILE-FS           PIC X(02).
               88  RPTFILE-OK                    VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  WS-END-OF-INSTR               VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-ERROR-SW             PIC X(01)  VALUE 'N'.
               88  WS-INSTR-IN-ERROR             VALUE 'Y'.
           05  WS-CHECK-SW             PIC X(01)  VALUE 'N'.
               88  WS-CHECK-OK                   VALUE 'Y'.
               88  WS-CHECK-FAILED               VALUE 'N'.
           05  WS-DATES-OK-SW          PIC X(01)  VALUE 'N'.
               88  WS-DATES-OK                   VALUE 'Y'.
      *
       01  WS-PARMS.
           05  WS-MAX-REJECTS          PIC 9(04)  VALUE 0100.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(20).
      *
      *----------------------------------------------------------------*
      * EXCEPTION CODES - KEEP THE TEXTS IN STEP WITH THE RUNBOOK SW-01*
      *----------------------------------------------------------------*
       01  WS-CODE-VALUES.
           05  FILLER PIC X(50) VALUE
               'V001ESENDER REFERENCE (TRADE ID) BLANK           '.
           05  FILLER PIC X(50) VALUE
               'V002EMESSAGE TYPE NOT MT541/MT543                '.
           05  FILLER PIC X(50) VALUE
               'V003EFUNCTION NOT NEWM/CANC                      '.
           05  FILLER PIC X(50) VALUE
               'V004EACCOUNT NUMBER BLANK OR INVALID             '.
           05  FILLER PIC X(50) VALUE
               'V005ECUSIP CHECK DIGIT INVALID                   '.
           05  FILLER PIC X(50) VALUE
               'V006WISIN BLANK - SENT WITH CUSIP ONLY           '.
           05  FILLER PIC X(50) VALUE
               'V007EISIN CHECK DIGIT INVALID                    '.
           05  FILLER PIC X(50) VALUE
               'V008WISIN DOES NOT CONTAIN THE CUSIP             '.
           05  FILLER PIC X(50) VALUE
               'V009WISIN NOT ON SECURITY MASTER                 '.
           05  FILLER PIC X(50) VALUE
               'V010EQUANTITY ZERO OR NEGATIVE                   '.
           05  FILLER PIC X(50) VALUE
               'V011ESETTLEMENT AMOUNT ZERO OR NEGATIVE          '.
           05  FILLER PIC X(50) VALUE
               'V012ECURRENCY NOT SUPPORTED BY CUSTODIAN         '.
           05  FILLER PIC X(50) VALUE
               'V013ETRADE OR SETTLE DATE INVALID                '.
           05  FILLER PIC X(50) VALUE
               'V014ESETTLE DATE MORE THAN 30 DAYS PAST          '.
           05  FILLER PIC X(50) VALUE
               'V015EDEPOSITORY NOT DTC/FED/EUCL                 '.
           05  FILLER PIC X(50) VALUE
               'V016EPLACE OF SETTLEMENT BIC INVALID             '.
           05  FILLER PIC X(50) VALUE
               'V017EAGENT BIC INVALID OR TEST BIC               '.
           05  FILLER PIC X(50) VALUE
               'V018ESAFEKEEPING ACCOUNT BLANK OR INVALID        '.
           05  FILLER PIC X(50) VALUE
               'V019WPLACE OF SETTLEMENT NOT DEPOSITORY BIC      '.
           05  FILLER PIC X(50) VALUE
               'V020EDTC CONTRA PARTICIPANT NOT NUMERIC          '.
           05  FILLER PIC X(50) VALUE
               'V021EDUPLICATE REFERENCE IN EXTRACT              '.
           05  FILLER PIC X(50) VALUE
               'V022WSETTLE DATE BEFORE TRADE DATE               '.
       01  WS-CODE-TABLE REDEFINES WS-CODE-VALUES.
           05  WS-CODE-ENTRY           OCCURS 22 TIMES
                                       INDEXED BY WS-CODE-IDX.
               10  WS-CODE-ID          PIC X(04).
               10  WS-CODE-SEV         PIC X(01).
               10  WS-CODE-TEXT        PIC X(45).
       01  WS-CODE-COUNTS.
           05  WS-CODE-COUNT           PIC S9(07) COMP-3
                                       OCCURS 22 TIMES.
       01  WS-CODE-SUB                 PIC S9(04) COMP.
      *
      *----------------------------------------------------------------*
      * DEPOSITORIES AND THEIR PLACE OF SETTLEMENT                     *
      *----------------------------------------------------------------*
       01  WS-DEPOSITORY-VALUES.
           05  FILLER  PIC X(15)  VALUE 'DTC DTCYUS33XXX'.
           05  FILLER  PIC X(15)  VALUE 'FED FRNYUS33XXX'.
           05  FILLER  PIC X(15)  VALUE 'EUCLMGTCBEBEXXX'.
       01  WS-DEPOSITORY-TABLE REDEFINES WS-DEPOSITORY-VALUES.
           05  WS-DEP-ENTRY            OCCURS 3 TIMES
                                       INDEXED BY WS-DEP-IDX.
               10  WS-DEP-CODE         PIC X(04).
               10  WS-DEP-BIC          PIC X(11).
      *
       01  WS-CCY-VALUES               PIC X(18)
                                       VALUE 'USDEURGBPJPYCADCHF'.
       01  WS-CCY-TABLE REDEFINES WS-CCY-VALUES.
           05  WS-CCY-CODE             PIC X(03) OCCURS 6 TIMES
                                       INDEXED BY WS-CCY-IDX.
      *
      *----------------------------------------------------------------*
      * CHECK DIGIT WORK AREAS.  CHARACTER VALUES A=10 ... Z=35,       *
      * * = 36, @ = 37, # = 38 (CUSIP SERVICE BUREAU RULES)            *
      *----------------------------------------------------------------*
       01  WS-ALPHA-VALUES             PIC X(29)
                                VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ*@#'.
       01  WS-ALPHA-TABLE REDEFINES WS-ALPHA-VALUES.
           05  WS-ALPHA-CHAR           PIC X(01) OCCURS 29 TIMES
                                       INDEXED BY WS-ALPHA-IDX.
       01  WS-CHECK-WORK.
           05  WS-CHK-CHAR             PIC X(01).
           05  WS-CHK-DIGIT            PIC 9(01).
           05  WS-CHK-VALUE            PIC S9(04) COMP.
           05  WS-CHK-SUM              PIC S9(04) COMP.
           05  WS-CHK-POS              PIC S9(04) COMP.
           05  WS-CHK-RESULT           PIC S9(04) COMP.
           05  WS-CHK-QUOT             PIC S9(04) COMP.
           05  WS-CHK-REM              PIC S9(04) COMP.
           05  WS-CHK-EXPECTED         PIC 9(01).
           05  WS-CHK-DOUBLE-SW        PIC X(01).
               88  WS-CHK-DOUBLE                 VALUE 'Y'.
      *    ISIN EXPANDED TO DIGITS (LETTERS BECOME TWO DIGITS)
           05  WS-ISIN-DIGITS          PIC X(24).
           05  WS-ISIN-DIGIT-TAB REDEFINES WS-ISIN-DIGITS.
               10  WS-ISIN-DGT         PIC 9(01) OCCURS 24 TIMES.
           05  WS-ISIN-DIG-LEN         PIC S9(04) COMP.
           05  WS-TWO-DIGITS           PIC 9(02).
           05  WS-TWO-DIGITS-X REDEFINES WS-TWO-DIGITS
                                       PIC X(02).
      *
      *----------------------------------------------------------------*
      * DUPLICATE REFERENCE TABLE (SEARCH ALL NEEDS ASCENDING KEYS -   *
      * LOADED IN ARRIVAL ORDER AND SEARCHED SERIALLY, CHG30112)       *
      *----------------------------------------------------------------*
       01  WS-REF-TABLE.
           05  WS-REF-COUNT            PIC S9(07) COMP VALUE ZERO.
           05  WS-REF-MAX              PIC S9(07) COMP VALUE 9000.
           05  WS-REF-ENTRY            OCCURS 9000 TIMES
                                       INDEXED BY WS-REF-IDX.
               10  WS-REF-ID           PIC X(16).
               10  WS-REF-FUNC         PIC X(04).
      *
       01  WS-COUNTERS.
           05  WS-INSTR-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-VALID          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-REJECTED       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSTR-WARNED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EXCEPTIONS-OUT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WARNINGS-OUT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SECM-LOOKUPS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NEWM-COUNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CANC-COUNT           PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-TOTALS.
           05  WS-IN-AMT-TOTAL         PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-IN-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-OK-AMT-TOTAL         PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-OK-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
           05  WS-REJ-AMT-TOTAL        PIC S9(15)V99    COMP-3
                                                  VALUE ZERO.
           05  WS-REJ-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                  VALUE ZERO.
      *
       01  WS-WORK-FIELDS.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE ZERO.
           05  WS-CUR-CODE             PIC X(04).
           05  WS-CUR-FIELD            PIC X(12).
           05  WS-INSTR-ERRORS         PIC S9(04) COMP.
           05  WS-INSTR-WARNINGS       PIC S9(04) COMP.
           05  WS-LAST-ISIN            PIC X(12)  VALUE LOW-VALUES.
           05  WS-LAST-ISIN-RC         PIC 9(02)  VALUE ZERO.
           05  WS-STALE-DATE           PIC 9(08).
           05  WS-BIC                  PIC X(11).
           05  WS-BIC-OK-SW            PIC X(01).
               88  WS-BIC-OK                     VALUE 'Y'.
           05  WS-SUB                  PIC S9(04) COMP.
           05  WS-DISP-COUNT           PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC ZZZ,ZZZ,ZZZ,ZZ9.99-.
      *
       01  WS-DATE-EDIT.
           05  WS-DE-CCYY              PIC 9(04).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-MM                PIC 9(02).
           05  FILLER                  PIC X(01)  VALUE '-'.
           05  WS-DE-DD                PIC 9(02).
       01  WS-DATE-IN                  PIC 9(08).
       01  WS-DATE-IN-R REDEFINES WS-DATE-IN.
           05  WS-DI-CCYY              PIC 9(04).
           05  WS-DI-MM                PIC 9(02).
           05  WS-DI-DD                PIC 9(02).
      *
      *----------------------------------------------------------------*
      * REPORT LINES                                                   *
      *----------------------------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(17)  VALUE ' SENDER REF'.
           05  FILLER  PIC X(10)  VALUE 'MT   FUNC'.
           05  FILLER  PIC X(11)  VALUE 'ACCOUNT'.
           05  FILLER  PIC X(10)  VALUE 'CUSIP'.
           05  FILLER  PIC X(13)  VALUE 'ISIN'.
           05  FILLER  PIC X(05)  VALUE 'DEP'.
           05  FILLER  PIC X(10)  VALUE 'SETTLE'.
           05  FILLER  PIC X(06)  VALUE 'CODE'.
           05  FILLER  PIC X(02)  VALUE 'S'.
           05  FILLER  PIC X(13)  VALUE 'FIELD'.
           05  FILLER  PIC X(35)  VALUE 'EXCEPTION'.
       01  WS-COL-HEAD-2.
           05  FILLER  PIC X(01)  VALUE ' '.
           05  FILLER  PIC X(17)  VALUE ' ---------------'.
           05  FILLER  PIC X(10)  VALUE '----------'.
           05  FILLER  PIC X(11)  VALUE '----------'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(13)  VALUE '------------'.
           05  FILLER  PIC X(05)  VALUE '----'.
           05  FILLER  PIC X(10)  VALUE '---------'.
           05  FILLER  PIC X(06)  VALUE '----'.
           05  FILLER  PIC X(02)  VALUE '-'.
           05  FILLER  PIC X(13)  VALUE '------------'.
           05  FILLER  PIC X(35)  VALUE ALL '-'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-REF                  PIC X(16).
           05  FILLER                  PIC X(01).
           05  DL-MT                   PIC X(05).
           05  FILLER                  PIC X(01).
           05  DL-FUNC                 PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(01).
           05  DL-ISIN                 PIC X(12).
           05  FILLER                  PIC X(01).
           05  DL-DEP                  PIC X(04).
           05  FILLER                  PIC X(01).
           05  DL-SETTLE               PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-CODE                 PIC X(04).
           05  FILLER                  PIC X(02).
           05  DL-SEV                  PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-FIELD                PIC X(12).
           05  FILLER                  PIC X(01).
           05  DL-TEXT                 PIC X(32).
       01  WS-SUMM-HEAD.
           05  FILLER  PIC X(01)  VALUE '-'.
           05  FILLER  PIC X(10)  VALUE ' CODE'.
           05  FILLER  PIC X(05)  VALUE 'SEV'.
           05  FILLER  PIC X(47)  VALUE 'DESCRIPTION'.
           05  FILLER  PIC X(12)  VALUE '       COUNT'.
           05  FILLER  PIC X(58)  VALUE SPACES.
       01  WS-SUMM-LINE.
           05  SM-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  SM-CODE                 PIC X(04).
           05  FILLER                  PIC X(05).
           05  SM-SEV                  PIC X(01).
           05  FILLER                  PIC X(03).
           05  SM-TEXT                 PIC X(45).
           05  FILLER                  PIC X(02).
           05  SM-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(60).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  TL-LABEL                PIC X(40).
           05  TL-COUNT                PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                  PIC X(03).
           05  TL-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(58).
       01  WS-NONE-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(122) VALUE
               '*** NO EXCEPTIONS - ALL INSTRUCTIONS PASSED ***'.
      *
       COPY CMRPTHD.
       COPY CMDATEW.
       COPY CMSECMS.
       COPY CMSECLNK.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *
       0000-MAINLINE.
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT
           PERFORM 2000-PROCESS-INSTR  THRU 2000-EXIT
               UNTIL WS-END-OF-INSTR
           PERFORM 7000-PRINT-SUMMARY  THRU 7000-EXIT
           PERFORM 9000-TERMINATE      THRU 9000-EXIT
           MOVE WS-RETURN-CODE         TO RETURN-CODE
           GOBACK.
      *
      *================================================================*
      * 1000 - INITIALIZE                                              *
      *================================================================*
       1000-INITIALIZE.
           OPEN INPUT DATECARD-FILE
           IF WS-DATECARD-FS NOT = '00'
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           CLOSE DATECARD-FILE
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'SETTLEMENT INSTRUCTION PRE-VALIDATION STARTED'
                                       TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS = '00'
               PERFORM 1100-READ-PARM  THRU 1100-EXIT
                   UNTIL WS-PARM-EOF
               CLOSE PARMCARD
           END-IF
           DISPLAY 'SWB050 - MAXIMUM REJECTS BEFORE RC 8: '
                   WS-MAX-REJECTS
      *
      *    STALE SETTLE DATE LIMIT - 30 CALENDAR DAYS BACK
           MOVE 'ADDC'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE DC-BUS-DATE            TO DT-DATE-1
           MOVE -30                    TO DT-DAYS
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE'  TO AB-PARAGRAPH
               MOVE 'CMU010 ADDC FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
           MOVE DT-RESULT-DATE         TO WS-STALE-DATE
      *
           PERFORM VARYING WS-CODE-SUB FROM 1 BY 1
                     UNTIL WS-CODE-SUB > 22
               MOVE ZERO               TO WS-CODE-COUNT (WS-CODE-SUB)
           END-PERFORM
      *
           OPEN INPUT SETLIN-FILE
           IF NOT SETLIN-OK
               MOVE 'SETLIN'           TO AB-DDNAME
               MOVE WS-SETLIN-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT INSTOK-FILE
           IF NOT INSTOK-OK
               MOVE 'INSTOK'           TO AB-DDNAME
               MOVE WS-INSTOK-FS       TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT INSTREJ-FILE
           IF NOT INSTREJ-OK
               MOVE 'INSTREJ'          TO AB-DDNAME
               MOVE WS-INSTREJ-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
           OPEN OUTPUT RPTFILE
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               PERFORM 9910-OPEN-ERROR THRU 9910-EXIT
           END-IF
      *
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP (1:10)    TO RPT-H1-RUN-DATE
           MOVE 'SWB050'               TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'SETTLEMENT INSTRUCTION PRE-VALIDATION EXCEPTIONS'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-DATE            TO WS-DATE-IN
           PERFORM 8300-EDIT-DATE      THRU 8300-EXIT
           MOVE WS-DATE-EDIT           TO RPT-H2-BUS-DATE
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
      *
           PERFORM 8000-READ-INSTR     THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARM.
           READ PARMCARD
               AT END
                   SET WS-PARM-EOF     TO TRUE
                   GO TO 1100-EXIT
           END-READ
           IF PARM-CARD-REC (1:1) = '*'
           OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD
                                          WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           EVALUATE WS-PARM-KEYWORD
               WHEN 'MAXREJ'
                   IF WS-PARM-VALUE (1:4) NUMERIC
                       MOVE WS-PARM-VALUE (1:4) TO WS-MAX-REJECTS
                   ELSE
                       DISPLAY 'SWB050 - MAXREJ NOT NUMERIC, IGNORED: '
                               PARM-CARD-REC (1:40)
                   END-IF
               WHEN OTHER
                   DISPLAY 'SWB050 - UNKNOWN PARAMETER IGNORED: '
                           PARM-CARD-REC (1:40)
           END-EVALUATE.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - EDIT ONE INSTRUCTION                                    *
      *================================================================*
       2000-PROCESS-INSTR.
           ADD 1                       TO WS-INSTR-READ
           ADD SI-SETTLE-AMOUNT        TO WS-IN-AMT-TOTAL
           ADD SI-QTY                  TO WS-IN-QTY-HASH
           MOVE 'N'                    TO WS-ERROR-SW
           MOVE ZERO                   TO WS-INSTR-ERRORS
                                          WS-INSTR-WARNINGS
      *
           PERFORM 3000-EDIT-HEADER    THRU 3000-EXIT
           PERFORM 3100-EDIT-SECURITY  THRU 3100-EXIT
           PERFORM 3200-EDIT-AMOUNTS   THRU 3200-EXIT
           PERFORM 3300-EDIT-DATES     THRU 3300-EXIT
           PERFORM 3400-EDIT-PARTIES   THRU 3400-EXIT
           PERFORM 3600-EDIT-DUPLICATE THRU 3600-EXIT
      *
           IF WS-INSTR-IN-ERROR
               ADD 1                   TO WS-INSTR-REJECTED
               ADD SI-SETTLE-AMOUNT    TO WS-REJ-AMT-TOTAL
               ADD SI-QTY              TO WS-REJ-QTY-HASH
           ELSE
               PERFORM 7100-WRITE-VALID THRU 7100-EXIT
               IF WS-INSTR-WARNINGS > ZERO
                   ADD 1               TO WS-INSTR-WARNED
               END-IF
           END-IF
           PERFORM 8000-READ-INSTR     THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3000 - REFERENCE, MESSAGE TYPE, FUNCTION, ACCOUNT              *
      *----------------------------------------------------------------*
       3000-EDIT-HEADER.
           IF SI-TRADE-ID = SPACES
               MOVE 'V001'             TO WS-CUR-CODE
               MOVE 'SI-TRADE-ID'      TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
           IF SI-MSG-TYPE NOT = 'MT541'
           AND SI-MSG-TYPE NOT = 'MT543'
               MOVE 'V002'             TO WS-CUR-CODE
               MOVE 'SI-MSG-TYPE'      TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
           EVALUATE TRUE
               WHEN SI-NEW-INSTR
                   ADD 1               TO WS-NEWM-COUNT
               WHEN SI-CANCEL-INSTR
                   ADD 1               TO WS-CANC-COUNT
               WHEN OTHER
                   MOVE 'V003'         TO WS-CUR-CODE
                   MOVE 'SI-FUNCTION'  TO WS-CUR-FIELD
                   PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-EVALUATE
           IF SI-ACCT-NO = SPACES
           OR SI-ACCT-NO (1:1) = SPACE
               MOVE 'V004'             TO WS-CUR-CODE
               MOVE 'SI-ACCT-NO'       TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3100 - CUSIP / ISIN                                            *
      *----------------------------------------------------------------*
       3100-EDIT-SECURITY.
           PERFORM 3500-CUSIP-CHECK-DIGIT THRU 3500-EXIT
           IF WS-CHECK-FAILED
               MOVE 'V005'             TO WS-CUR-CODE
               MOVE 'SI-CUSIP'         TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
           IF SI-ISIN = SPACES
               MOVE 'V006'             TO WS-CUR-CODE
               MOVE 'SI-ISIN'          TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               GO TO 3100-EXIT
           END-IF
           PERFORM 3550-ISIN-CHECK-DIGIT THRU 3550-EXIT
           IF WS-CHECK-FAILED
               MOVE 'V007'             TO WS-CUR-CODE
               MOVE 'SI-ISIN'          TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               GO TO 3100-EXIT
           END-IF
      *    NORTH AMERICAN ISINS EMBED THE CUSIP (CC + CUSIP + CHECK)
           IF SI-ISIN (1:2) = 'US' OR 'CA'
               IF SI-ISIN (3:9) NOT = SI-CUSIP
                   MOVE 'V008'         TO WS-CUR-CODE
                   MOVE 'SI-ISIN'      TO WS-CUR-FIELD
                   PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               END-IF
           END-IF
           PERFORM 3150-LOOKUP-ISIN    THRU 3150-EXIT.
       3100-EXIT.
           EXIT.
      *
      *    ISIN MUST BE KNOWN TO THE SECURITY MASTER - SAVED FOR REPEATS
       3150-LOOKUP-ISIN.
           IF SI-ISIN NOT = WS-LAST-ISIN
               MOVE SI-ISIN            TO WS-LAST-ISIN
               MOVE 'GETI'             TO SL-FUNCTION
               MOVE SPACES             TO SL-KEY-CUSIP SL-KEY-SYMBOL
               MOVE SI-ISIN            TO SL-KEY-ISIN
               CALL 'CMD010' USING SL-SECURITY-PARMS
               ADD 1                   TO WS-SECM-LOOKUPS
               MOVE SL-RETURN-CODE     TO WS-LAST-ISIN-RC
               IF SL-DB-ERROR
                   MOVE SL-SQLCODE     TO AB-SQLCODE
                   MOVE SI-ISIN        TO AB-KEY
                   MOVE 1003           TO AB-ABEND-CODE
                   MOVE '3150-LOOKUP-ISIN' TO AB-PARAGRAPH
                   MOVE 'CMD010 GETI FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
               END-IF
           END-IF
           IF WS-LAST-ISIN-RC = 04
               MOVE 'V009'             TO WS-CUR-CODE
               MOVE 'SI-ISIN'          TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF.
       3150-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3200 - QUANTITY, AMOUNT, CURRENCY                              *
      *----------------------------------------------------------------*
       3200-EDIT-AMOUNTS.
           IF SI-QTY NOT NUMERIC
           OR SI-QTY NOT > ZERO
               MOVE 'V010'             TO WS-CUR-CODE
               MOVE 'SI-QTY'           TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
           IF SI-SETTLE-AMOUNT NOT NUMERIC
           OR SI-SETTLE-AMOUNT NOT > ZERO
               MOVE 'V011'             TO WS-CUR-CODE
               MOVE 'SI-SETTLE-AMT'    TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
           SET WS-CCY-IDX              TO 1
           SEARCH WS-CCY-CODE
               AT END
                   MOVE 'V012'         TO WS-CUR-CODE
                   MOVE 'SI-CCY'       TO WS-CUR-FIELD
                   PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               WHEN WS-CCY-CODE (WS-CCY-IDX) = SI-CCY
                   CONTINUE
           END-SEARCH.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3300 - TRADE AND SETTLE DATES                                  *
      *----------------------------------------------------------------*
       3300-EDIT-DATES.
           MOVE 'N'                    TO WS-DATES-OK-SW
           MOVE 'VALD'                 TO DT-FUNCTION
           MOVE SPACES                 TO DT-CALENDAR
           MOVE SI-TRADE-DATE          TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'V013'             TO WS-CUR-CODE
               MOVE 'SI-TRADE-DATE'    TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               GO TO 3300-EXIT
           END-IF
           MOVE SI-SETTLE-DATE         TO DT-DATE-1
           CALL 'CMU010' USING DT-DATE-PARMS
           IF NOT DT-OK
               MOVE 'V013'             TO WS-CUR-CODE
               MOVE 'SI-SETTLE-DATE'   TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               GO TO 3300-EXIT
           END-IF
           SET WS-DATES-OK             TO TRUE
           IF SI-SETTLE-DATE < SI-TRADE-DATE
               MOVE 'V022'             TO WS-CUR-CODE
               MOVE 'SI-SETTLE-DATE'   TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
      *    A CANCEL OF AN OLD TRADE IS STILL SENT
           IF SI-NEW-INSTR
           AND SI-SETTLE-DATE < WS-STALE-DATE
               MOVE 'V014'             TO WS-CUR-CODE
               MOVE 'SI-SETTLE-DATE'   TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF.
       3300-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 3400 - DEPOSITORY, PLACE OF SETTLEMENT, AGENT, SAFEKEEPING     *
      *----------------------------------------------------------------*
       3400-EDIT-PARTIES.
           SET WS-DEP-IDX              TO 1
           SEARCH WS-DEP-ENTRY
               AT END
                   MOVE 'V015'         TO WS-CUR-CODE
                   MOVE 'SI-DEPOSITORY' TO WS-CUR-FIELD
                   PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               WHEN WS-DEP-CODE (WS-DEP-IDX) = SI-DEPOSITORY
                   IF SI-PLACE-BIC NOT = WS-DEP-BIC (WS-DEP-IDX)
                       MOVE 'V019'     TO WS-CUR-CODE
                       MOVE 'SI-PLACE-BIC' TO WS-CUR-FIELD
                       PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
                   END-IF
           END-SEARCH
      *
           MOVE SI-PLACE-BIC           TO WS-BIC
           PERFORM 3700-CHECK-BIC      THRU 3700-EXIT
           IF NOT WS-BIC-OK
               MOVE 'V016'             TO WS-CUR-CODE
               MOVE 'SI-PLACE-BIC'     TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
           MOVE SI-AGENT-BIC           TO WS-BIC
           PERFORM 3700-CHECK-BIC      THRU 3700-EXIT
           IF NOT WS-BIC-OK
               MOVE 'V017'             TO WS-CUR-CODE
               MOVE 'SI-AGENT-BIC'     TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           END-IF
      *
      *    SAFEKEEPING: SWIFT 'X' CHARACTER SET, NO EMBEDDED BLANKS
           IF SI-SAFEKEEPING-ACCT = SPACES
           OR SI-SAFEKEEPING-ACCT (1:1) = SPACE
               MOVE 'V018'             TO WS-CUR-CODE
               MOVE 'SI-SAFEKEEP'      TO WS-CUR-FIELD
               PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
           ELSE
               PERFORM VARYING WS-SUB FROM 1 BY 1
                         UNTIL WS-SUB > 12
                   IF SI-SAFEKEEPING-ACCT (WS-SUB:1) = ':'
                   OR SI-SAFEKEEPING-ACCT (WS-SUB:1) = '{'
                   OR SI-SAFEKEEPING-ACCT (WS-SUB:1) = '}'
                   OR SI-SAFEKEEPING-ACCT (WS-SUB:1) = '/'
                       MOVE 'V018'     TO WS-CUR-CODE
                       MOVE 'SI-SAFEKEEP' TO WS-CUR-FIELD
                       PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
                       MOVE 13         TO WS-SUB
                   END-IF
               END-PERFORM
           END-IF
      *
      *    DTC CONTRA IS A 4 DIGIT PARTICIPANT NUMBER
           IF SI-DEPOSITORY = 'DTC '
           AND SI-CONTRA NOT = SPACES
               IF SI-CONTRA NOT NUMERIC
                   MOVE 'V020'         TO WS-CUR-CODE
                   MOVE 'SI-CONTRA'    TO WS-CUR-FIELD
                   PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
               END-IF
           END-IF.
       3400-EXIT.
           EXIT.
      *
      *================================================================*
      * 3500 - CUSIP CHECK DIGIT (MODULUS 10 DOUBLE-ADD-DOUBLE)        *
      *================================================================*
       3500-CUSIP-CHECK-DIGIT.
           SET WS-CHECK-FAILED         TO TRUE
           IF SI-CUSIP (9:1) NOT NUMERIC
               GO TO 3500-EXIT
           END-IF
           MOVE ZERO                   TO WS-CHK-SUM
           PERFORM VARYING WS-CHK-POS FROM 1 BY 1
                     UNTIL WS-CHK-POS > 8
               MOVE SI-CUSIP (WS-CHK-POS:1) TO WS-CHK-CHAR
               PERFORM 3800-CHAR-VALUE THRU 3800-EXIT
               IF WS-CHK-VALUE < ZERO
                   GO TO 3500-EXIT
               END-IF
      *        EVEN POSITIONS ARE DOUBLED
               DIVIDE WS-CHK-POS BY 2 GIVING WS-CHK-QUOT
                   REMAINDER WS-CHK-REM
               IF WS-CHK-REM = ZERO
                   COMPUTE WS-CHK-VALUE = WS-CHK-VALUE * 2
               END-IF
               DIVIDE WS-CHK-VALUE BY 10 GIVING WS-CHK-QUOT
                   REMAINDER WS-CHK-REM
               ADD WS-CHK-QUOT WS-CHK-REM TO WS-CHK-SUM
           END-PERFORM
           DIVIDE WS-CHK-SUM BY 10 GIVING WS-CHK-QUOT
               REMAINDER WS-CHK-REM
           COMPUTE WS-CHK-RESULT = 10 - WS-CHK-REM
           IF WS-CHK-RESULT = 10
               MOVE ZERO               TO WS-CHK-RESULT
           END-IF
           MOVE SI-CUSIP (9:1)         TO WS-CHK-EXPECTED
           IF WS-CHK-RESULT = WS-CHK-EXPECTED
               SET WS-CHECK-OK         TO TRUE
           END-IF.
       3500-EXIT.
           EXIT.
      *
      *================================================================*
      * 3550 - ISIN CHECK DIGIT.  LETTERS EXPAND TO TWO DIGITS, THEN   *
      *        LUHN FROM THE RIGHT (RIGHTMOST DIGIT DOUBLED).          *
      *================================================================*
       3550-ISIN-CHECK-DIGIT.
           SET WS-CHECK-FAILED         TO TRUE
           IF SI-ISIN (12:1) NOT NUMERIC
               GO TO 3550-EXIT
           END-IF
           MOVE SPACES                 TO WS-ISIN-DIGITS
           MOVE ZERO                   TO WS-ISIN-DIG-LEN
           PERFORM VARYING WS-CHK-POS FROM 1 BY 1
                     UNTIL WS-CHK-POS > 11
               MOVE SI-ISIN (WS-CHK-POS:1) TO WS-CHK-CHAR
               PERFORM 3800-CHAR-VALUE THRU 3800-EXIT
               IF WS-CHK-VALUE < ZERO
                   GO TO 3550-EXIT
               END-IF
               IF WS-CHK-VALUE > 9
                   MOVE WS-CHK-VALUE   TO WS-TWO-DIGITS
                   ADD 1               TO WS-ISIN-DIG-LEN
                   MOVE WS-TWO-DIGITS-X (1:1)
                            TO WS-ISIN-DIGITS (WS-ISIN-DIG-LEN:1)
                   ADD 1               TO WS-ISIN-DIG-LEN
                   MOVE WS-TWO-DIGITS-X (2:1)
                            TO WS-ISIN-DIGITS (WS-ISIN-DIG-LEN:1)
               ELSE
                   ADD 1               TO WS-ISIN-DIG-LEN
                   MOVE WS-CHK-VALUE   TO WS-ISIN-DGT (WS-ISIN-DIG-LEN)
               END-IF
           END-PERFORM
           MOVE ZERO                   TO WS-CHK-SUM
           MOVE 'Y'                    TO WS-CHK-DOUBLE-SW
           PERFORM VARYING WS-CHK-POS FROM WS-ISIN-DIG-LEN BY -1
                     UNTIL WS-CHK-POS < 1
               MOVE WS-ISIN-DGT (WS-CHK-POS) TO WS-CHK-VALUE
               IF WS-CHK-DOUBLE
                   COMPUTE WS-CHK-VALUE = WS-CHK-VALUE * 2
                   MOVE 'N'            TO WS-CHK-DOUBLE-SW
               ELSE
                   MOVE 'Y'            TO WS-CHK-DOUBLE-SW
               END-IF
               DIVIDE WS-CHK-VALUE BY 10 GIVING WS-CHK-QUOT
                   REMAINDER WS-CHK-REM
               ADD WS-CHK-QUOT WS-CHK-REM TO WS-CHK-SUM
           END-PERFORM
           DIVIDE WS-CHK-SUM BY 10 GIVING WS-CHK-QUOT
               REMAINDER WS-CHK-REM
           COMPUTE WS-CHK-RESULT = 10 - WS-CHK-REM
           IF WS-CHK-RESULT = 10
               MOVE ZERO               TO WS-CHK-RESULT
           END-IF
           MOVE SI-ISIN (12:1)         TO WS-CHK-EXPECTED
           IF WS-CHK-RESULT = WS-CHK-EXPECTED
               SET WS-CHECK-OK         TO TRUE
           END-IF.
       3550-EXIT.
           EXIT.
      *
      *================================================================*
      * 3600 - SAME REFERENCE TWICE IN ONE EXTRACT                     *
      *================================================================*
       3600-EDIT-DUPLICATE.
           IF SI-TRADE-ID = SPACES
               GO TO 3600-EXIT
           END-IF
           SET WS-REF-IDX              TO 1
           SEARCH WS-REF-ENTRY VARYING WS-REF-IDX
               AT END
                   CONTINUE
               WHEN WS-REF-IDX > WS-REF-COUNT
                   SET WS-REF-IDX      TO WS-REF-MAX
               WHEN WS-REF-ID (WS-REF-IDX) = SI-TRADE-ID
                AND WS-REF-FUNC (WS-REF-IDX) = SI-FUNCTION
                   MOVE 'V021'         TO WS-CUR-CODE
                   MOVE 'SI-TRADE-ID'  TO WS-CUR-FIELD
                   PERFORM 6000-RECORD-EXCEPTION THRU 6000-EXIT
                   GO TO 3600-EXIT
           END-SEARCH
           IF WS-REF-COUNT < WS-REF-MAX
               ADD 1                   TO WS-REF-COUNT
               MOVE SI-TRADE-ID        TO WS-REF-ID (WS-REF-COUNT)
               MOVE SI-FUNCTION        TO WS-REF-FUNC (WS-REF-COUNT)
           ELSE
               MOVE 1007               TO AB-ABEND-CODE
               MOVE '3600-EDIT-DUPLICATE' TO AB-PARAGRAPH
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE 'REFERENCE TABLE FULL - INCREASE WS-REF-MAX'
                                       TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF.
       3600-EXIT.
           EXIT.
      *
      *================================================================*
      * 3700 - BIC FORMAT: BANK(4) ALPHA, COUNTRY(2) ALPHA,            *
      *        LOCATION(2) ALPHANUMERIC, BRANCH(3) ALPHANUMERIC OR     *
      *        BLANK.  LOCATION WITH '0' IN POSITION 2 IS A TEST BIC.  *
      *================================================================*
       3700-CHECK-BIC.
           MOVE 'N'                    TO WS-BIC-OK-SW
           IF WS-BIC (1:6) NOT ALPHABETIC
           OR WS-BIC (1:1) = SPACE
           OR WS-BIC (5:1) = SPACE
               GO TO 3700-EXIT
           END-IF
           IF WS-BIC (7:1) = SPACE
           OR WS-BIC (8:1) = SPACE
               GO TO 3700-EXIT
           END-IF
           IF WS-BIC (8:1) = '0'
               GO TO 3700-EXIT
           END-IF
           IF WS-BIC (9:3) NOT = SPACES
               IF WS-BIC (9:1) = SPACE
               OR WS-BIC (10:1) = SPACE
               OR WS-BIC (11:1) = SPACE
                   GO TO 3700-EXIT
               END-IF
           END-IF
           MOVE 'Y'                    TO WS-BIC-OK-SW.
       3700-EXIT.
           EXIT.
      *
      *================================================================*
      * 3800 - VALUE OF ONE CUSIP/ISIN CHARACTER                       *
      *        DIGIT 0-9, LETTER 10-35, * 36, @ 37, # 38, ELSE -1      *
      *        LETTERS AND SPECIALS SORT BELOW THE DIGITS              *
      *================================================================*
       3800-CHAR-VALUE.
           MOVE -1                     TO WS-CHK-VALUE
           IF WS-CHK-CHAR = SPACE
               GO TO 3800-EXIT
           END-IF
           IF WS-CHK-CHAR < '0'
               SET WS-ALPHA-IDX        TO 1
               SEARCH WS-ALPHA-CHAR
                   AT END
                       CONTINUE
                   WHEN WS-ALPHA-CHAR (WS-ALPHA-IDX) = WS-CHK-CHAR
                       SET WS-CHK-VALUE TO WS-ALPHA-IDX
                       ADD 9           TO WS-CHK-VALUE
               END-SEARCH
           ELSE
               MOVE WS-CHK-CHAR        TO WS-CHK-DIGIT
               MOVE WS-CHK-DIGIT       TO WS-CHK-VALUE
           END-IF.
       3800-EXIT.
           EXIT.
      *
      *================================================================*
      * 6000 - RECORD ONE EXCEPTION (WS-CUR-CODE / WS-CUR-FIELD)       *
      *================================================================*
       6000-RECORD-EXCEPTION.
           SET WS-CODE-IDX             TO 1
           SEARCH WS-CODE-ENTRY
               AT END
                   MOVE 1008           TO AB-ABEND-CODE
                   MOVE '6000-RECORD-EXCEPTION' TO AB-PARAGRAPH
                   MOVE WS-CUR-CODE    TO AB-KEY
                   MOVE 'EXCEPTION CODE NOT IN TABLE' TO AB-MESSAGE
                   PERFORM 9999-ABEND  THRU 9999-EXIT
               WHEN WS-CODE-ID (WS-CODE-IDX) = WS-CUR-CODE
                   CONTINUE
           END-SEARCH
           SET WS-CODE-SUB             TO WS-CODE-IDX
           ADD 1                       TO WS-CODE-COUNT (WS-CODE-SUB)
      *
           INITIALIZE SVR-EXCEPTION-REC
           MOVE DC-BUS-DATE            TO SVR-BUS-DATE
           MOVE WS-CUR-CODE            TO SVR-REJ-CODE
           MOVE WS-CODE-SEV (WS-CODE-IDX) TO SVR-SEVERITY
           MOVE WS-CUR-FIELD           TO SVR-FIELD-NAME
           MOVE WS-CODE-TEXT (WS-CODE-IDX) TO SVR-REJ-TEXT
           MOVE SI-INSTRUCTION-REC     TO SVR-INSTR-IMAGE
           IF SVR-ERROR
               SET WS-INSTR-IN-ERROR   TO TRUE
               ADD 1                   TO WS-INSTR-ERRORS
           ELSE
               ADD 1                   TO WS-INSTR-WARNINGS
                                          WS-WARNINGS-OUT
           END-IF
           WRITE SVR-EXCEPTION-REC
           IF NOT INSTREJ-OK
               MOVE 'INSTREJ'          TO AB-DDNAME
               MOVE WS-INSTREJ-FS      TO AB-FILE-STATUS
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE '6000-RECORD-EXCEPTION' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-EXCEPTIONS-OUT
           PERFORM 6100-PRINT-EXCEPTION THRU 6100-EXIT.
       6000-EXIT.
           EXIT.
      *
       6100-PRINT-EXCEPTION.
           MOVE SPACES                 TO WS-DETAIL-LINE
           MOVE ' '                    TO DL-CC
           MOVE SI-TRADE-ID            TO DL-REF
           MOVE SI-MSG-TYPE            TO DL-MT
           MOVE SI-FUNCTION            TO DL-FUNC
           MOVE SI-ACCT-NO             TO DL-ACCT
           MOVE SI-CUSIP               TO DL-CUSIP
           MOVE SI-ISIN                TO DL-ISIN
           MOVE SI-DEPOSITORY          TO DL-DEP
           IF SI-SETTLE-DATE NUMERIC
               MOVE SI-SETTLE-DATE     TO WS-DATE-IN
               PERFORM 8300-EDIT-DATE  THRU 8300-EXIT
               MOVE WS-DATE-EDIT       TO DL-SETTLE
           ELSE
               MOVE '**INVALID*'       TO DL-SETTLE
           END-IF
           MOVE SVR-REJ-CODE           TO DL-CODE
           MOVE SVR-SEVERITY           TO DL-SEV
           MOVE SVR-FIELD-NAME         TO DL-FIELD
           MOVE SVR-REJ-TEXT           TO DL-TEXT
           PERFORM 8100-WRITE-DETAIL   THRU 8100-EXIT.
       6100-EXIT.
           EXIT.
      *
      *================================================================*
      * 7000 - SUMMARY BY EXCEPTION CODE                               *
      *================================================================*
       7000-PRINT-SUMMARY.
           IF WS-EXCEPTIONS-OUT = ZERO
               WRITE RPT-RECORD FROM WS-NONE-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 2                   TO RPT-LINE-COUNT
           END-IF
           IF RPT-LINE-COUNT + 34 > RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-SUMM-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 3                       TO RPT-LINE-COUNT
           PERFORM VARYING WS-CODE-SUB FROM 1 BY 1
                     UNTIL WS-CODE-SUB > 22
               MOVE SPACES             TO WS-SUMM-LINE
               MOVE ' '                TO SM-CC
               MOVE WS-CODE-ID (WS-CODE-SUB)   TO SM-CODE
               MOVE WS-CODE-SEV (WS-CODE-SUB)  TO SM-SEV
               MOVE WS-CODE-TEXT (WS-CODE-SUB) TO SM-TEXT
               MOVE WS-CODE-COUNT (WS-CODE-SUB) TO SM-COUNT
               WRITE RPT-RECORD FROM WS-SUMM-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1                   TO RPT-LINE-COUNT
           END-PERFORM
      *
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE '0'                    TO TL-CC
           MOVE 'INSTRUCTIONS READ'    TO TL-LABEL
           MOVE WS-INSTR-READ          TO TL-COUNT
           MOVE WS-IN-AMT-TOTAL        TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL    THRU 7200-EXIT
           MOVE 'INSTRUCTIONS PASSED TO SWB100' TO TL-LABEL
           MOVE WS-INSTR-VALID         TO TL-COUNT
           MOVE WS-OK-AMT-TOTAL        TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL    THRU 7200-EXIT
           MOVE '  OF WHICH WITH WARNINGS' TO TL-LABEL
           MOVE WS-INSTR-WARNED        TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL    THRU 7200-EXIT
           MOVE 'INSTRUCTIONS REJECTED (NOT SENT)' TO TL-LABEL
           MOVE WS-INSTR-REJECTED      TO TL-COUNT
           MOVE WS-REJ-AMT-TOTAL       TO TL-AMOUNT
           PERFORM 7200-WRITE-TOTAL    THRU 7200-EXIT
           MOVE 'SECURITY MASTER LOOKUPS' TO TL-LABEL
           MOVE WS-SECM-LOOKUPS        TO TL-COUNT
           PERFORM 7200-WRITE-TOTAL    THRU 7200-EXIT
           WRITE RPT-RECORD FROM RPT-END-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT.
       7000-EXIT.
           EXIT.
      *
       7200-WRITE-TOTAL.
           WRITE RPT-RECORD FROM WS-TOTAL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE ' '                    TO TL-CC.
       7200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 7100 - PASS THE INSTRUCTION ON UNCHANGED                       *
      *----------------------------------------------------------------*
       7100-WRITE-VALID.
           WRITE INSTOK-REC            FROM SI-INSTRUCTION-REC
           IF NOT INSTOK-OK
               MOVE 'INSTOK'           TO AB-DDNAME
               MOVE WS-INSTOK-FS       TO AB-FILE-STATUS
               MOVE SI-TRADE-ID        TO AB-KEY
               MOVE '7100-WRITE-VALID' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF
           ADD 1                       TO WS-INSTR-VALID
           ADD SI-SETTLE-AMOUNT        TO WS-OK-AMT-TOTAL
           ADD SI-QTY                  TO WS-OK-QTY-HASH.
       7100-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O                                                     *
      *================================================================*
       8000-READ-INSTR.
           READ SETLIN-FILE
           EVALUATE TRUE
               WHEN SETLIN-OK
                   CONTINUE
               WHEN SETLIN-EOF
                   SET WS-END-OF-INSTR TO TRUE
               WHEN OTHER
                   MOVE 'SETLIN'       TO AB-DDNAME
                   MOVE WS-SETLIN-FS   TO AB-FILE-STATUS
                   MOVE '8000-READ-INSTR' TO AB-PARAGRAPH
                   PERFORM 9920-IO-ERROR THRU 9920-EXIT
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
       8100-WRITE-DETAIL.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-DETAIL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *
       8200-HEADINGS.
           ADD 1                       TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT         TO RPT-H1-PAGE
           WRITE RPT-RECORD FROM RPT-HEADING-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM RPT-HEADING-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM WS-COL-HEAD-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM WS-COL-HEAD-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           MOVE 5                      TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *
       8300-EDIT-DATE.
           MOVE WS-DI-CCYY             TO WS-DE-CCYY
           MOVE WS-DI-MM               TO WS-DE-MM
           MOVE WS-DI-DD               TO WS-DE-DD.
       8300-EXIT.
           EXIT.
      *
       8900-CHECK-WRITE.
           IF NOT RPTFILE-OK
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE '8900-CHECK-WRITE' TO AB-PARAGRAPH
               PERFORM 9920-IO-ERROR   THRU 9920-EXIT
           END-IF.
       8900-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE SETLIN-FILE
                 INSTOK-FILE
                 INSTREJ-FILE
                 RPTFILE
           IF NOT SETLIN-OK OR NOT INSTOK-OK
           OR NOT INSTREJ-OK OR NOT RPTFILE-OK
               MOVE 'CLOSE'            TO AB-DDNAME
               MOVE 1002               TO AB-ABEND-CODE
               MOVE '9000-TERMINATE'   TO AB-PARAGRAPH
               STRING 'CLOSE ERROR ' WS-SETLIN-FS ' '
                      WS-INSTOK-FS ' ' WS-INSTREJ-FS ' '
                      WS-RPTFILE-FS
                      DELIMITED BY SIZE INTO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF
      *
           IF WS-INSTR-REJECTED > ZERO
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
           IF WS-INSTR-REJECTED > WS-MAX-REJECTS
               MOVE 8                  TO WS-RETURN-CODE
               DISPLAY 'SWB050 - REJECTS ' WS-INSTR-REJECTED
                       ' EXCEED MAXREJ ' WS-MAX-REJECTS
                       ' - EXTRACT SUSPECT, MESSAGES NOT SENT'
           END-IF
      *
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
                                          CT-STAGE
           MOVE 'SETLINST-IN'          TO CT-COUNTER-NAME
           MOVE WS-INSTR-READ          TO CT-COUNT
           MOVE WS-IN-AMT-TOTAL        TO CT-AMOUNT
           MOVE WS-IN-QTY-HASH         TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'VALID-OUT'            TO CT-COUNTER-NAME
           MOVE WS-INSTR-VALID         TO CT-COUNT
           MOVE WS-OK-AMT-TOTAL        TO CT-AMOUNT
           MOVE WS-OK-QTY-HASH         TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'REJECT-OUT'           TO CT-COUNTER-NAME
           MOVE WS-INSTR-REJECTED      TO CT-COUNT
           MOVE WS-REJ-AMT-TOTAL       TO CT-AMOUNT
           MOVE WS-REJ-QTY-HASH        TO CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'WARN-OUT'             TO CT-COUNTER-NAME
           MOVE WS-WARNINGS-OUT        TO CT-COUNT
           MOVE ZERO                   TO CT-AMOUNT CT-QTY-HASH
           PERFORM 9110-CALL-CMU080    THRU 9110-EXIT
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
      *
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RETURN-CODE = ZERO
               MOVE 'I'                TO AU-SEVERITY
           ELSE
               MOVE 'W'                TO AU-SEVERITY
           END-IF
           MOVE WS-INSTR-REJECTED      TO WS-DISP-COUNT
           MOVE SPACES                 TO AU-MESSAGE
           STRING 'PRE-VALIDATION ENDED. REJECTED ' WS-DISP-COUNT
                  DELIMITED BY SIZE INTO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS
      *
           DISPLAY '*************************************************'
           DISPLAY '* SWB050 - INSTRUCTION PRE-VALIDATION ' DC-BUS-DATE
           DISPLAY '*************************************************'
           MOVE WS-INSTR-READ          TO WS-DISP-COUNT
           DISPLAY '* INSTRUCTIONS READ         : ' WS-DISP-COUNT
           MOVE WS-NEWM-COUNT          TO WS-DISP-COUNT
           DISPLAY '*   NEWM                    : ' WS-DISP-COUNT
           MOVE WS-CANC-COUNT          TO WS-DISP-COUNT
           DISPLAY '*   CANC                    : ' WS-DISP-COUNT
           MOVE WS-INSTR-VALID         TO WS-DISP-COUNT
           DISPLAY '* PASSED TO SWB100          : ' WS-DISP-COUNT
           MOVE WS-INSTR-WARNED        TO WS-DISP-COUNT
           DISPLAY '*   WITH WARNINGS           : ' WS-DISP-COUNT
           MOVE WS-INSTR-REJECTED      TO WS-DISP-COUNT
           DISPLAY '* REJECTED                  : ' WS-DISP-COUNT
           MOVE WS-EXCEPTIONS-OUT      TO WS-DISP-COUNT
           DISPLAY '* EXCEPTION RECORDS WRITTEN : ' WS-DISP-COUNT
           MOVE WS-SECM-LOOKUPS        TO WS-DISP-COUNT
           DISPLAY '* SECURITY MASTER LOOKUPS   : ' WS-DISP-COUNT
           MOVE WS-OK-AMT-TOTAL        TO WS-DISP-AMT
           DISPLAY '* AMOUNT PASSED             : ' WS-DISP-AMT
           DISPLAY '* RETURN CODE               : ' WS-RETURN-CODE
           DISPLAY '*************************************************'.
       9000-EXIT.
           EXIT.
      *
       9110-CALL-CMU080.
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 'CTLTOTS'          TO AB-DDNAME
               MOVE 1010               TO AB-ABEND-CODE
               MOVE '9110-CALL-CMU080' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME    TO AB-KEY
               MOVE 'CMU080 POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND      THRU 9999-EXIT
           END-IF.
       9110-EXIT.
           EXIT.
      *
       9910-OPEN-ERROR.
           MOVE 1001                   TO AB-ABEND-CODE
           MOVE '1000-INITIALIZE'      TO AB-PARAGRAPH
           STRING 'OPEN FAILED FOR ' AB-DDNAME
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9910-EXIT.
           EXIT.
      *
       9920-IO-ERROR.
           MOVE 1002                   TO AB-ABEND-CODE
           STRING 'I/O ERROR ON ' AB-DDNAME ' STATUS '
                  AB-FILE-STATUS
                  DELIMITED BY SIZE INTO AB-MESSAGE
           PERFORM 9999-ABEND          THRU 9999-EXIT.
       9920-EXIT.
           EXIT.
      *
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           DISPLAY 'SWB050 - ABEND ' AB-ABEND-CODE ' IN '
                   AB-PARAGRAPH
           DISPLAY 'SWB050 - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
       9999-EXIT.
           EXIT.
