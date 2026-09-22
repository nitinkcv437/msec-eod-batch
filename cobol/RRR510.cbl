       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RRR510.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  05/03/1993.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RRR510                                            *
      * TITLE      : NET CAPITAL - HAIRCUT SCHEDULE REPORT             *
      * JOB        : MSRRD040   STEP020                                *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   PRINTS THE HAIRCUT FILE WRITTEN BY RRB500:                   *
      *     SECTION 1  FIRM INVENTORY BY ACCOUNT WITH THE BAND,        *
      *                PERCENT, HAIRCUT AND UNDUE CONCENTRATION OF     *
      *                EVERY POSITION; ACCOUNT TOTALS                  *
      *     SECTION 2  SUMMARY BY HAIRCUT BAND                         *
      *     SECTION 3  UNDUE CONCENTRATION DETAIL                      *
      *     SECTION 4  NET CAPITAL COMPUTATION - TENTATIVE NET CAPITAL *
      *                (SYSIN TNC=, SAME CARD AS RRB500) LESS HAIRCUTS *
      *                LESS UNDUE CONCENTRATION.  EVERY HAIRCUT IS     *
      *                RECOMPUTED FROM MARKET VALUE AND PERCENT.       *
      *                                                                *
      * INPUT  : DATECARD  BUSINESS DATE CARD            (CMDATEW)     *
      *          SYSIN     TNC=NNNNNNNNNNNNN.NN (RRP040A)              *
      *          HCUTIN    MSEC.PROD.RR.HAIRCUT(+1)      (RRHCUT)      *
      * OUTPUT : RPTFILE   REPORT, FB 133 ASA                          *
      * CALLS  : CMU050 CMU060 CMU080 CMASM02                          *
      *                                                                *
      * RETURN CODES: 0 CLEAN  4 NO TNC CARD  8 A HAIRCUT DOES NOT     *
      *               RECOMPUTE                                        *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1993-05-03 RJK  CHG01512  ORIGINAL                             *
      * 1998-11-02 TLM  CHG04471  Y2K - DATES CCYYMMDD                 *
      * 2004-03-15 KAP  CHG12011  MUNICIPAL BANDS IN THE SUMMARY       *
      * 2014-06-30 SPA  CHG27120  UNDUE CONCENTRATION SECTION, TNC     *
      * 2020-03-30 MHC  CHG34990  RECOMPUTE ON ABSOLUTE MARKET VALUE   *
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
           SELECT HCUTIN-FILE   ASSIGN TO HCUTIN
                                FILE STATUS IS WS-HCUTIN-FS.
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
       FD  HCUTIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY RRHCUT.
       FD  RPTFILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-RECORD                  PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RRR510'.
       01  WS-FILE-STATUS-AREA.
           05  WS-PARMCARD-FS          PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-DATECARD-FS          PIC X(02)  VALUE '00'.
           05  WS-HCUTIN-FS            PIC X(02)  VALUE '00'.
               88  HCUTIN-OK                      VALUE '00'.
               88  HCUTIN-EOF                     VALUE '10'.
           05  WS-RPTFILE-FS           PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-PARM-EOF                    VALUE 'Y'.
           05  WS-HCUT-EOF-SW          PIC X(01)  VALUE 'N'.
               88  WS-HCUT-EOF                    VALUE 'Y'.
           05  WS-TNC-SW               PIC X(01)  VALUE 'N'.
               88  WS-TNC-GIVEN                   VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *
       01  WS-PARM-KEYWORD             PIC X(20).
       01  WS-PARM-VALUE               PIC X(30).
       01  WS-PARM-VALUE-R REDEFINES WS-PARM-VALUE.
           05  WS-PV-DOLLARS           PIC X(13).
           05  WS-PV-POINT             PIC X(01).
           05  WS-PV-CENTS             PIC X(02).
           05  WS-PV-REST              PIC X(14).
       01  WS-PV-AMOUNT-X.
           05  WS-PVA-DOLLARS          PIC X(13).
           05  WS-PVA-CENTS            PIC X(02).
       01  WS-PV-AMOUNT REDEFINES WS-PV-AMOUNT-X
                                       PIC 9(13)V99.
       01  WS-TNC                      PIC S9(15)V99 COMP-3 VALUE ZERO.
      *
      *---- CONTROL BREAK --------------------------------------------*
       01  WS-PREV-ACCT                PIC X(10)  VALUE LOW-VALUES.
       01  WS-ACCT-TOTALS.
           05  WS-AT-COUNT             PIC S9(07)       COMP-3.
           05  WS-AT-MV                PIC S9(15)V99    COMP-3.
           05  WS-AT-HAIRCUT           PIC S9(15)V99    COMP-3.
           05  WS-AT-CONC              PIC S9(15)V99    COMP-3.
       01  WS-GRAND-TOTALS.
           05  WS-GT-COUNT             PIC S9(07)       COMP-3 VALUE 0.
           05  WS-GT-LONG-MV           PIC S9(15)V99    COMP-3 VALUE 0.
           05  WS-GT-SHORT-MV          PIC S9(15)V99    COMP-3 VALUE 0.
           05  WS-GT-HAIRCUT           PIC S9(15)V99    COMP-3 VALUE 0.
           05  WS-GT-CONC              PIC S9(15)V99    COMP-3 VALUE 0.
           05  WS-GT-QTY               PIC S9(15)V9(04) COMP-3 VALUE 0.
           05  WS-RECOMPUTE-ERRORS     PIC S9(07)       COMP-3 VALUE 0.
      *
      *---- BAND SUMMARY ---------------------------------------------*
       01  WS-BAND-TABLE.
           05  WS-BD-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-BD-ENTRY             OCCURS 40 TIMES
                                       INDEXED BY BD-IDX.
               10  WS-BD-BAND          PIC X(04).
               10  WS-BD-PCT           PIC S9(03)V9(04) COMP-3.
               10  WS-BD-COUNT         PIC S9(07)       COMP-3.
               10  WS-BD-MV            PIC S9(15)V99    COMP-3.
               10  WS-BD-HAIRCUT       PIC S9(15)V99    COMP-3.
       01  WS-BD-MAX                   PIC S9(04) COMP  VALUE 40.
       01  WS-BD-SUB                   PIC S9(04) COMP.
      *
      *---- CONCENTRATION LINES (SECTION 3) --------------------------*
       01  WS-CONC-TABLE.
           05  WS-CC-USED              PIC S9(04) COMP  VALUE ZERO.
           05  WS-CC-ENTRY             OCCURS 300 TIMES.
               10  WS-CC-ISSUER        PIC X(06).
               10  WS-CC-ACCT          PIC X(10).
               10  WS-CC-CUSIP         PIC X(09).
               10  WS-CC-MV            PIC S9(15)V99    COMP-3.
               10  WS-CC-PCT           PIC S9(03)V9(04) COMP-3.
               10  WS-CC-CONC          PIC S9(15)V99    COMP-3.
       01  WS-CC-MAX                   PIC S9(04) COMP  VALUE 300.
       01  WS-CC-SUB                   PIC S9(04) COMP.
      *
       01  WS-WORK.
           05  WS-ABS-MV               PIC S9(15)V99    COMP-3.
           05  WS-CHECK-HAIRCUT        PIC S9(15)V99    COMP-3.
           05  WS-NET-CAPITAL          PIC S9(15)V99    COMP-3.
           05  WS-HAIRCUT-PCT-TNC      PIC S9(05)V99    COMP-3.
      *
      *---- REPORT LINES ---------------------------------------------*
       01  WS-COL-HEAD-1.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(12)  VALUE ' ACCOUNT'.
           05  FILLER  PIC X(11)  VALUE 'CUSIP'.
           05  FILLER  PIC X(04)  VALUE 'TY'.
           05  FILLER  PIC X(08)  VALUE 'ISSUER'.
           05  FILLER  PIC X(06)  VALUE 'BAND'.
           05  FILLER  PIC X(03)  VALUE 'LS'.
           05  FILLER  PIC X(19)  VALUE '          QUANTITY'.
           05  FILLER  PIC X(22)  VALUE '     MARKET VALUE USD'.
           05  FILLER  PIC X(09)  VALUE '     PCT'.
           05  FILLER  PIC X(19)  VALUE '           HAIRCUT'.
           05  FILLER  PIC X(19)  VALUE '     CONCENTRATION'.
       01  WS-DETAIL-LINE.
           05  DL-CC                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-ACCT                 PIC X(10).
           05  FILLER                  PIC X(01).
           05  DL-CUSIP                PIC X(09).
           05  FILLER                  PIC X(02).
           05  DL-SEC-TYPE             PIC X(02).
           05  FILLER                  PIC X(02).
           05  DL-ISSUER               PIC X(06).
           05  FILLER                  PIC X(02).
           05  DL-BAND                 PIC X(04).
           05  FILLER                  PIC X(02).
           05  DL-LS                   PIC X(01).
           05  FILLER                  PIC X(01).
           05  DL-QTY                  PIC -ZZ,ZZZ,ZZZ,ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-MV                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-PCT                  PIC ZZ9.9999.
           05  FILLER                  PIC X(01).
           05  DL-HAIRCUT              PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  DL-CONC                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
       01  WS-TOTAL-LINE.
           05  TL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  TL-LABEL                PIC X(40).
           05  TL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(13).
           05  TL-MV                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(10).
           05  TL-HAIRCUT              PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
           05  TL-CONC                 PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(01).
       01  WS-BAND-HEAD.
           05  FILLER  PIC X(01)  VALUE '0'.
           05  FILLER  PIC X(10)  VALUE '     BAND'.
           05  FILLER  PIC X(12)  VALUE '     PCT'.
           05  FILLER  PIC X(12)  VALUE '  POSITIONS'.
           05  FILLER  PIC X(22)  VALUE '     MARKET VALUE USD'.
           05  FILLER  PIC X(20)  VALUE '           HAIRCUT'.
           05  FILLER  PIC X(56)  VALUE SPACES.
       01  WS-BAND-LINE.
           05  BL-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  BL-BAND                 PIC X(04).
           05  FILLER                  PIC X(03).
           05  BL-PCT                  PIC ZZ9.9999.
           05  FILLER                  PIC X(03).
           05  BL-COUNT                PIC ZZZ,ZZ9.
           05  FILLER                  PIC X(02).
           05  BL-MV                   PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(02).
           05  BL-HAIRCUT              PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(62).
       01  WS-NC-LINE.
           05  NC-CC                   PIC X(01).
           05  FILLER                  PIC X(10).
           05  NC-LABEL                PIC X(50).
           05  NC-AMOUNT               PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(49).
       01  WS-MSG-LINE.
           05  ML-CC                   PIC X(01).
           05  FILLER                  PIC X(05).
           05  ML-TEXT                 PIC X(127).
      *
           COPY CMRPTHD.
           COPY CMDATEW.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMTSLNK.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE     THRU 1000-EXIT.
           PERFORM 2000-PRINT-POSITION THRU 2000-EXIT
               UNTIL WS-HCUT-EOF.
           IF WS-PREV-ACCT NOT = LOW-VALUES
               PERFORM 2500-ACCOUNT-BREAK THRU 2500-EXIT
           END-IF
           PERFORM 3000-BAND-SUMMARY   THRU 3000-EXIT.
           PERFORM 4000-CONCENTRATION  THRU 4000-EXIT.
           PERFORM 5000-NET-CAPITAL    THRU 5000-EXIT.
           PERFORM 9000-TERMINATE      THRU 9000-EXIT.
           MOVE WS-RETURN-CODE         TO RETURN-CODE.
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
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           READ DATECARD-FILE INTO DC-DATE-CARD
           IF WS-DATECARD-FS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD'         TO AB-DDNAME
               MOVE WS-DATECARD-FS     TO AB-FILE-STATUS
               MOVE 1005               TO AB-ABEND-CODE
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           CLOSE DATECARD-FILE
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE WS-PROGRAM-ID          TO AU-PROGRAM
           MOVE 'START'                TO AU-EVENT
           MOVE 'I'                    TO AU-SEVERITY
           MOVE DC-BUS-DATE            TO AU-BUS-DATE
           MOVE SPACES                 TO AU-KEY
           MOVE 'HAIRCUT SCHEDULE REPORT STARTED' TO AU-MESSAGE
           CALL 'CMU060' USING AU-AUDIT-PARMS
           OPEN INPUT PARMCARD
           IF WS-PARMCARD-FS NOT = '00'
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-FS     TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           PERFORM 1100-READ-PARM      THRU 1100-EXIT
               UNTIL WS-PARM-EOF
           CLOSE PARMCARD
           IF NOT WS-TNC-GIVEN
               MOVE 4                  TO WS-RETURN-CODE
           END-IF
           OPEN INPUT HCUTIN-FILE
           IF WS-HCUTIN-FS NOT = '00'
               MOVE 'HCUTIN'           TO AB-DDNAME
               MOVE WS-HCUTIN-FS       TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           OPEN OUTPUT RPTFILE
           IF WS-RPTFILE-FS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1001               TO AB-ABEND-CODE
               MOVE 'OPEN FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           INITIALIZE WS-ACCT-TOTALS
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA
           MOVE TS-TIMESTAMP(1:10)     TO RPT-H1-RUN-DATE
           MOVE WS-PROGRAM-ID          TO RPT-H1-REPORT-ID
                                          RPT-H2-PROGRAM
           MOVE 'NET CAPITAL - HAIRCUTS ON FIRM INVENTORY'
                                       TO RPT-H2-TITLE
           MOVE DC-BUS-DATE            TO RPT-H2-BUS-DATE
           PERFORM 8200-HEADINGS       THRU 8200-EXIT
           PERFORM 8000-READ-HAIRCUT   THRU 8000-EXIT
           IF WS-HCUT-EOF
               MOVE SPACES             TO WS-MSG-LINE
               MOVE '0'                TO ML-CC
               MOVE '*** NO FIRM INVENTORY POSITIONS ***' TO ML-TEXT
               PERFORM 8100-PRINT-MESSAGE THRU 8100-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *
       1100-READ-PARM.
           READ PARMCARD
           IF PARMCARD-EOF
               MOVE 'Y'                TO WS-PARM-EOF-SW
               GO TO 1100-EXIT
           END-IF
           IF NOT PARMCARD-OK
               MOVE 'SYSIN'            TO AB-DDNAME
               MOVE WS-PARMCARD-FS     TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'READ FAILED'      TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           IF PARM-CARD-REC(1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1100-EXIT
           END-IF
           MOVE SPACES                 TO WS-PARM-KEYWORD WS-PARM-VALUE
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING
           IF WS-PARM-KEYWORD = 'TNC'
           AND WS-PV-DOLLARS NUMERIC
           AND WS-PV-POINT = '.'
           AND WS-PV-CENTS NUMERIC
               MOVE WS-PV-DOLLARS      TO WS-PVA-DOLLARS
               MOVE WS-PV-CENTS        TO WS-PVA-CENTS
               MOVE WS-PV-AMOUNT       TO WS-TNC
               MOVE 'Y'                TO WS-TNC-SW
           END-IF.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * 2000 - SECTION 1: ONE POSITION                                 *
      *================================================================*
       2000-PRINT-POSITION.
           IF RRH-ACCT-NO NOT = WS-PREV-ACCT
               IF WS-PREV-ACCT NOT = LOW-VALUES
                   PERFORM 2500-ACCOUNT-BREAK THRU 2500-EXIT
               END-IF
               MOVE RRH-ACCT-NO        TO WS-PREV-ACCT
           END-IF
      *    ---- RECOMPUTE THE HAIRCUT --------------------------------
           IF RRH-MKT-VALUE-USD < ZERO
               COMPUTE WS-ABS-MV = RRH-MKT-VALUE-USD * -1
           ELSE
               MOVE RRH-MKT-VALUE-USD  TO WS-ABS-MV
           END-IF
           COMPUTE WS-CHECK-HAIRCUT ROUNDED =
                   WS-ABS-MV * RRH-HAIRCUT-PCT / 100
           IF WS-CHECK-HAIRCUT NOT = RRH-HAIRCUT-AMT
               ADD 1                   TO WS-RECOMPUTE-ERRORS
               MOVE 8                  TO WS-RETURN-CODE
               DISPLAY 'RRR510 HAIRCUT DOES NOT RECOMPUTE '
                       RRH-ACCT-NO ' ' RRH-CUSIP
           END-IF
           MOVE SPACES                 TO WS-DETAIL-LINE
           MOVE ' '                    TO DL-CC
           IF WS-AT-COUNT = ZERO
               MOVE RRH-ACCT-NO        TO DL-ACCT
           END-IF
           MOVE RRH-CUSIP              TO DL-CUSIP
           MOVE RRH-SEC-TYPE           TO DL-SEC-TYPE
           MOVE RRH-ISSUER-ID          TO DL-ISSUER
           MOVE RRH-BUCKET             TO DL-BAND
           MOVE RRH-LONG-SHORT         TO DL-LS
           MOVE RRH-QTY                TO DL-QTY
           MOVE RRH-MKT-VALUE-USD      TO DL-MV
           MOVE RRH-HAIRCUT-PCT        TO DL-PCT
           MOVE RRH-HAIRCUT-AMT        TO DL-HAIRCUT
           IF RRH-UNDUE-CONC-AMT NOT = ZERO
               MOVE RRH-UNDUE-CONC-AMT TO DL-CONC
           END-IF
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
               MOVE RRH-ACCT-NO        TO DL-ACCT
           END-IF
           WRITE RPT-RECORD FROM WS-DETAIL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT
      *    ---- TOTALS ----------------------------------------------
           ADD 1                       TO WS-AT-COUNT WS-GT-COUNT
           ADD RRH-MKT-VALUE-USD       TO WS-AT-MV
           ADD RRH-HAIRCUT-AMT         TO WS-AT-HAIRCUT WS-GT-HAIRCUT
           ADD RRH-UNDUE-CONC-AMT      TO WS-AT-CONC WS-GT-CONC
           ADD RRH-QTY                 TO WS-GT-QTY
           IF RRH-LONG-SHORT = 'S'
               ADD WS-ABS-MV           TO WS-GT-SHORT-MV
           ELSE
               ADD WS-ABS-MV           TO WS-GT-LONG-MV
           END-IF
           PERFORM 2100-ADD-BAND       THRU 2100-EXIT
           IF RRH-UNDUE-CONC-AMT NOT = ZERO
               PERFORM 2200-ADD-CONC   THRU 2200-EXIT
           END-IF
           PERFORM 8000-READ-HAIRCUT   THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
       2100-ADD-BAND.
           SET BD-IDX                  TO 1
           SEARCH WS-BD-ENTRY
               AT END
                   DISPLAY 'RRR510 BAND TABLE FULL ' RRH-BUCKET
               WHEN BD-IDX > WS-BD-USED
                   ADD 1               TO WS-BD-USED
                   MOVE RRH-BUCKET     TO WS-BD-BAND(WS-BD-USED)
                   MOVE RRH-HAIRCUT-PCT TO WS-BD-PCT(WS-BD-USED)
                   MOVE 1              TO WS-BD-COUNT(WS-BD-USED)
                   MOVE WS-ABS-MV      TO WS-BD-MV(WS-BD-USED)
                   MOVE RRH-HAIRCUT-AMT TO WS-BD-HAIRCUT(WS-BD-USED)
               WHEN WS-BD-BAND(BD-IDX) = RRH-BUCKET
                   ADD 1               TO WS-BD-COUNT(BD-IDX)
                   ADD WS-ABS-MV       TO WS-BD-MV(BD-IDX)
                   ADD RRH-HAIRCUT-AMT TO WS-BD-HAIRCUT(BD-IDX)
           END-SEARCH.
       2100-EXIT.
           EXIT.
      *
       2200-ADD-CONC.
           IF WS-CC-USED NOT < WS-CC-MAX
               GO TO 2200-EXIT
           END-IF
           ADD 1                       TO WS-CC-USED
           MOVE RRH-ISSUER-ID          TO WS-CC-ISSUER(WS-CC-USED)
           MOVE RRH-ACCT-NO            TO WS-CC-ACCT(WS-CC-USED)
           MOVE RRH-CUSIP              TO WS-CC-CUSIP(WS-CC-USED)
           MOVE RRH-MKT-VALUE-USD      TO WS-CC-MV(WS-CC-USED)
           MOVE RRH-HAIRCUT-PCT        TO WS-CC-PCT(WS-CC-USED)
           MOVE RRH-UNDUE-CONC-AMT     TO WS-CC-CONC(WS-CC-USED).
       2200-EXIT.
           EXIT.
      *
       2500-ACCOUNT-BREAK.
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE ' '                    TO TL-CC
           MOVE '  ACCOUNT TOTAL'      TO TL-LABEL
           MOVE WS-AT-COUNT            TO TL-COUNT
           MOVE WS-AT-MV               TO TL-MV
           MOVE WS-AT-HAIRCUT          TO TL-HAIRCUT
           MOVE WS-AT-CONC             TO TL-CONC
           PERFORM 8150-PRINT-TOTAL    THRU 8150-EXIT
           WRITE RPT-RECORD FROM RPT-BLANK-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT
           INITIALIZE WS-ACCT-TOTALS.
       2500-EXIT.
           EXIT.
      *
      *================================================================*
      * 3000 - SECTION 2: SUMMARY BY BAND (ORDER OF FIRST APPEARANCE)  *
      *================================================================*
       3000-BAND-SUMMARY.
           MOVE 'NET CAPITAL - HAIRCUT SUMMARY BY BAND' TO RPT-H2-TITLE
           PERFORM 8250-PAGE-HEADINGS  THRU 8250-EXIT
           WRITE RPT-RECORD FROM WS-BAND-HEAD
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
           PERFORM VARYING WS-BD-SUB FROM 1 BY 1
                   UNTIL WS-BD-SUB > WS-BD-USED
               MOVE SPACES             TO WS-BAND-LINE
               MOVE ' '                TO BL-CC
               MOVE WS-BD-BAND(WS-BD-SUB)    TO BL-BAND
               MOVE WS-BD-PCT(WS-BD-SUB)     TO BL-PCT
               MOVE WS-BD-COUNT(WS-BD-SUB)   TO BL-COUNT
               MOVE WS-BD-MV(WS-BD-SUB)      TO BL-MV
               MOVE WS-BD-HAIRCUT(WS-BD-SUB) TO BL-HAIRCUT
               WRITE RPT-RECORD FROM WS-BAND-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1                   TO RPT-LINE-COUNT
           END-PERFORM
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE '0'                    TO TL-CC
           MOVE 'ALL BANDS'            TO TL-LABEL
           MOVE WS-GT-COUNT            TO TL-COUNT
           COMPUTE TL-MV = WS-GT-LONG-MV + WS-GT-SHORT-MV
           MOVE WS-GT-HAIRCUT          TO TL-HAIRCUT
           PERFORM 8150-PRINT-TOTAL    THRU 8150-EXIT.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * 4000 - SECTION 3: UNDUE CONCENTRATION                          *
      *================================================================*
       4000-CONCENTRATION.
           MOVE 'NET CAPITAL - UNDUE CONCENTRATION' TO RPT-H2-TITLE
           PERFORM 8250-PAGE-HEADINGS  THRU 8250-EXIT
           IF WS-CC-USED = ZERO
               MOVE SPACES             TO WS-MSG-LINE
               MOVE '0'                TO ML-CC
               IF WS-TNC-GIVEN
                   MOVE 'NO ISSUER ABOVE 10 PCT OF TNC'
                                       TO ML-TEXT
               ELSE
                   MOVE '*** NO TNC CARD - CONCENTRATION NOT COMPUTED'
                                       TO ML-TEXT
               END-IF
               PERFORM 8100-PRINT-MESSAGE THRU 8100-EXIT
               GO TO 4000-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-COL-HEAD-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT
           PERFORM VARYING WS-CC-SUB FROM 1 BY 1
                   UNTIL WS-CC-SUB > WS-CC-USED
               MOVE SPACES             TO WS-DETAIL-LINE
               MOVE ' '                TO DL-CC
               MOVE WS-CC-ACCT(WS-CC-SUB)   TO DL-ACCT
               MOVE WS-CC-CUSIP(WS-CC-SUB)  TO DL-CUSIP
               MOVE WS-CC-ISSUER(WS-CC-SUB) TO DL-ISSUER
               MOVE WS-CC-MV(WS-CC-SUB)     TO DL-MV
               MOVE WS-CC-PCT(WS-CC-SUB)    TO DL-PCT
               MOVE WS-CC-CONC(WS-CC-SUB)   TO DL-CONC
               WRITE RPT-RECORD FROM WS-DETAIL-LINE
               PERFORM 8900-CHECK-WRITE THRU 8900-EXIT
               ADD 1                   TO RPT-LINE-COUNT
           END-PERFORM
           MOVE SPACES                 TO WS-TOTAL-LINE
           MOVE '0'                    TO TL-CC
           MOVE 'TOTAL UNDUE CONCENTRATION' TO TL-LABEL
           MOVE WS-CC-USED             TO TL-COUNT
           MOVE WS-GT-CONC             TO TL-CONC
           PERFORM 8150-PRINT-TOTAL    THRU 8150-EXIT.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
      * 5000 - SECTION 4: NET CAPITAL COMPUTATION                      *
      *================================================================*
       5000-NET-CAPITAL.
           MOVE 'NET CAPITAL COMPUTATION' TO RPT-H2-TITLE
           PERFORM 8250-PAGE-HEADINGS  THRU 8250-EXIT
           COMPUTE WS-NET-CAPITAL = WS-TNC - WS-GT-HAIRCUT - WS-GT-CONC
           MOVE SPACES                 TO WS-NC-LINE
           MOVE '0'                    TO NC-CC
           MOVE 'TENTATIVE NET CAPITAL (TNC CARD)' TO NC-LABEL
           MOVE WS-TNC                 TO NC-AMOUNT
           PERFORM 8160-PRINT-NC       THRU 8160-EXIT
           MOVE ' '                    TO NC-CC
           MOVE 'LESS: HAIRCUTS ON SECURITIES POSITIONS' TO NC-LABEL
           COMPUTE NC-AMOUNT = WS-GT-HAIRCUT * -1
           PERFORM 8160-PRINT-NC       THRU 8160-EXIT
           MOVE 'LESS: UNDUE CONCENTRATION' TO NC-LABEL
           COMPUTE NC-AMOUNT = WS-GT-CONC * -1
           PERFORM 8160-PRINT-NC       THRU 8160-EXIT
           MOVE '0'                    TO NC-CC
           MOVE 'NET CAPITAL'          TO NC-LABEL
           MOVE WS-NET-CAPITAL         TO NC-AMOUNT
           PERFORM 8160-PRINT-NC       THRU 8160-EXIT
           MOVE '0'                    TO NC-CC
           MOVE 'MEMO: LONG MARKET VALUE' TO NC-LABEL
           MOVE WS-GT-LONG-MV          TO NC-AMOUNT
           PERFORM 8160-PRINT-NC       THRU 8160-EXIT
           MOVE ' '                    TO NC-CC
           MOVE 'MEMO: SHORT MARKET VALUE' TO NC-LABEL
           MOVE WS-GT-SHORT-MV         TO NC-AMOUNT
           PERFORM 8160-PRINT-NC       THRU 8160-EXIT
           IF WS-TNC > ZERO
               COMPUTE WS-HAIRCUT-PCT-TNC ROUNDED =
                   (WS-GT-HAIRCUT + WS-GT-CONC) * 100 / WS-TNC
               MOVE SPACES             TO WS-MSG-LINE
               MOVE '0'                TO ML-CC
               MOVE WS-HAIRCUT-PCT-TNC TO BL-PCT
               STRING 'TOTAL DEDUCTIONS AS PERCENT OF TNC: '
                                       DELIMITED BY SIZE
                      BL-PCT           DELIMITED BY SIZE
                   INTO ML-TEXT
               END-STRING
               PERFORM 8100-PRINT-MESSAGE THRU 8100-EXIT
           END-IF
           MOVE SPACES                 TO WS-MSG-LINE
           MOVE '0'                    TO ML-CC
           IF WS-RECOMPUTE-ERRORS > ZERO
               MOVE '*** HAIRCUTS DO NOT RECOMPUTE - SEE SYSOUT ***'
                                       TO ML-TEXT
           ELSE
               MOVE 'ALL HAIRCUTS RECOMPUTED FROM MARKET VALUE X PCT'
                                       TO ML-TEXT
           END-IF
           PERFORM 8100-PRINT-MESSAGE  THRU 8100-EXIT
           WRITE RPT-RECORD FROM RPT-END-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT.
       5000-EXIT.
           EXIT.
      *
      *================================================================*
      * 8000 - I/O                                                     *
      *================================================================*
       8000-READ-HAIRCUT.
           READ HCUTIN-FILE
           EVALUATE TRUE
               WHEN HCUTIN-OK
                   IF RRH-MKT-VALUE-USD NOT NUMERIC
                   OR RRH-HAIRCUT-AMT NOT NUMERIC
                   OR RRH-UNDUE-CONC-AMT NOT NUMERIC
                       MOVE 'HCUTIN'   TO AB-DDNAME
                       MOVE 1008       TO AB-ABEND-CODE
                       MOVE RRH-CUSIP  TO AB-KEY
                       MOVE 'HAIRCUT AMOUNTS NOT NUMERIC' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   END-IF
               WHEN HCUTIN-EOF
                   MOVE 'Y'            TO WS-HCUT-EOF-SW
               WHEN OTHER
                   MOVE 'HCUTIN'       TO AB-DDNAME
                   MOVE WS-HCUTIN-FS   TO AB-FILE-STATUS
                   MOVE 1002           TO AB-ABEND-CODE
                   MOVE 'READ FAILED'  TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *
       8100-PRINT-MESSAGE.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8250-PAGE-HEADINGS THRU 8250-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-MSG-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT.
       8100-EXIT.
           EXIT.
      *
       8150-PRINT-TOTAL.
           IF RPT-LINE-COUNT NOT < RPT-LINES-PER-PAGE
               PERFORM 8200-HEADINGS   THRU 8200-EXIT
           END-IF
           WRITE RPT-RECORD FROM WS-TOTAL-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 1                       TO RPT-LINE-COUNT.
       8150-EXIT.
           EXIT.
      *
       8160-PRINT-NC.
           WRITE RPT-RECORD FROM WS-NC-LINE
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT.
       8160-EXIT.
           EXIT.
      *
       8200-HEADINGS.
           PERFORM 8250-PAGE-HEADINGS  THRU 8250-EXIT
           WRITE RPT-RECORD FROM WS-COL-HEAD-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           ADD 2                       TO RPT-LINE-COUNT.
       8200-EXIT.
           EXIT.
      *
       8250-PAGE-HEADINGS.
           ADD 1                       TO RPT-PAGE-COUNT
           MOVE RPT-PAGE-COUNT         TO RPT-H1-PAGE
           WRITE RPT-RECORD FROM RPT-HEADING-1
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           WRITE RPT-RECORD FROM RPT-HEADING-2
           PERFORM 8900-CHECK-WRITE    THRU 8900-EXIT
           MOVE 2                      TO RPT-LINE-COUNT.
       8250-EXIT.
           EXIT.
      *
       8900-CHECK-WRITE.
           IF WS-RPTFILE-FS NOT = '00'
               MOVE 'RPTFILE'          TO AB-DDNAME
               MOVE WS-RPTFILE-FS      TO AB-FILE-STATUS
               MOVE 1002               TO AB-ABEND-CODE
               MOVE 'REPORT WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8900-EXIT.
           EXIT.
      *
      *================================================================*
      * 9000 - TERMINATE                                               *
      *================================================================*
       9000-TERMINATE.
           CLOSE HCUTIN-FILE RPTFILE
           MOVE 'POST'                 TO CT-FUNCTION
           MOVE DC-BUS-DATE            TO CT-BUS-DATE
           MOVE WS-PROGRAM-ID          TO CT-PROGRAM
           MOVE 'RRR510'               TO CT-STAGE
           MOVE 'HAIRCUT-IN'           TO CT-COUNTER-NAME
           MOVE WS-GT-COUNT            TO CT-COUNT
           COMPUTE CT-AMOUNT = WS-GT-HAIRCUT + WS-GT-CONC
           MOVE WS-GT-QTY              TO CT-QTY-HASH
           CALL 'CMU080' USING CT-CONTROL-PARMS
           IF NOT CT-OK
               MOVE 1010               TO AB-ABEND-CODE
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF
           MOVE 'CLOS'                 TO CT-FUNCTION
           CALL 'CMU080' USING CT-CONTROL-PARMS
           DISPLAY 'RRR510 HAIRCUT RECORDS READ  : ' WS-GT-COUNT
           DISPLAY 'RRR510 BANDS                 : ' WS-BD-USED
           DISPLAY 'RRR510 CONCENTRATION LINES   : ' WS-CC-USED
           DISPLAY 'RRR510 RECOMPUTE ERRORS      : ' WS-RECOMPUTE-ERRORS
           DISPLAY 'RRR510 NET CAPITAL           : ' WS-NET-CAPITAL
           DISPLAY 'RRR510 RETURN CODE           : ' WS-RETURN-CODE
           MOVE 'WRIT'                 TO AU-FUNCTION
           MOVE 'END'                  TO AU-EVENT
           IF WS-RETURN-CODE > 4
               MOVE 'E'                TO AU-SEVERITY
               MOVE 'HAIRCUT REPORT - HAIRCUTS DO NOT RECOMPUTE'
                                       TO AU-MESSAGE
           ELSE
               MOVE 'I'                TO AU-SEVERITY
               MOVE 'HAIRCUT SCHEDULE REPORT ENDED' TO AU-MESSAGE
           END-IF
           CALL 'CMU060' USING AU-AUDIT-PARMS
           MOVE 'CLOS'                 TO AU-FUNCTION
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
      *================================================================*
      * 9999 - ABEND                                                   *
      *================================================================*
       9999-ABEND.
           MOVE WS-PROGRAM-ID          TO AB-PROGRAM
           MOVE '9999-ABEND'           TO AB-PARAGRAPH
           DISPLAY 'RRR510 ABENDING - ' AB-MESSAGE
           CALL 'CMU050' USING AB-ABEND-PARMS
           MOVE 16                     TO RETURN-CODE
           GOBACK.
