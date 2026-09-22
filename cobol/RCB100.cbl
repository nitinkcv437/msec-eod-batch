       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCB100.
       AUTHOR.        D W BRENNAN.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  MARCH 1996.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCB100                                            *
      * DESCRIPTION: DTC PARTICIPANT POSITION STATEMENT LOAD.          *
      *              READS THE DAILY DTC STATEMENT (VARIABLE LENGTH    *
      *              RECORDS - ONE HEADER, ONE DETAIL PER CUSIP WITH   *
      *              1 TO 20 BALANCE SEGMENTS, ONE TRAILER), PROVES    *
      *              THE FILE (HEADER DATE, TRAILER COUNT AND QUANTITY *
      *              HASH, SEGMENTS ADD UP TO THE CUSIP TOTAL) AND     *
      *              WRITES ONE NORMALIZED STREET POSITION PER CUSIP   *
      *              FOR THE POSITION RECONCILIATION (RCB200).         *
      *              THE STATEMENT IS AS OF THE PREVIOUS BUSINESS DAY  *
      *              CLOSE OF BUSINESS.                                *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD010 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - PARAMETER CARDS  (RCP100A)             *
      *              DTCSTMT  - MSEC.PROD.RC.DTCSTMT.EDIT(+1)  VB 417  *
      *                         (RCDTCST)                              *
      * OUTPUT     : STPOSOUT - MSEC.PROD.RC.STPOS.DTC(+1)    (RCSTPOS)*
      * CALLS      : CMU050, CMU060, CMU080                            *
      * RETURN CODE: 0 CLEAN                                           *
      *              4 DETAILS REJECTED OR SEGMENTS OUT OF BALANCE     *
      *              8 TRAILER COUNT / HASH OUT OF BALANCE (OUTPUT IS  *
      *                COMPLETE - OPERATIONS CALL THE DTC DESK)        *
      *              U1005 STATEMENT DATE NOT PREVIOUS BUSINESS DAY    *
      *              U1008 NO HEADER / NO TRAILER / RECORDS AFTER THE  *
      *                    TRAILER                                     *
      *----------------------------------------------------------------*
      * PARAMETER CARDS (SYSIN, * IN COLUMN 1 = COMMENT):              *
      *   DATECHK=Y     STATEMENT DATE MUST = PREVIOUS BUSINESS DATE   *
      *   DATECHK=N     ANY STATEMENT DATE ACCEPTED (RECOVERY ONLY)    *
      *   PARTID=NNNN   EXPECTED DTC PARTICIPANT (BLANK = ANY)         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1996-03-18 DWB  ORIGINAL - REPLACES CCF TAPE PROGRAM  CHG02101 *
      *                 RCB090 (FIXED 200 BYTE TAPE RECORDS)           *
      * 1996-07-29 DWB  PLEDGED SEGMENT                       CHG02290 *
      * 1997-02-10 DWB  WRITE RCSTPOS FOR NEW RECON RCB200    CHG02644 *
      * 1998-12-07 TLM  Y2K - STATEMENT DATE CCYYMMDD         CHG04471 *
      * 2001-05-14 KAP  QTY 4 DECIMALS (FRACTIONAL SHARES)    CHG08811 *
      * 2004-01-12 KAP  DELIVER / RECEIVE PENDING SEGMENTS    CHG11905 *
      * 2009-07-20 SPA  SEGMENT TABLE 10 -> 20                CHG18866 *
      * 2011-03-07 SPA  DTC PADS DETAIL TO FULLWORD - ACCEPT  CHG21340 *
      *                 LONGER RECORDS                                 *
      * 2014-08-18 MHC  DATECHK PARAMETER FOR DR RECOVERY     CHG27715 *
      * 2016-02-01 MHC  DUPLICATE CUSIP DETAIL - WARNING,     CHG29112 *
      *                 BOTH LINES GO TO THE RECON (RCB200 ADDS)       *
      * 2019-02-25 MHC  CONTROL TOTALS NAMES PER CMB090       CHG33410 *
      * 2024-02-12 NVR  T+1 - NO CHANGE, VERIFIED             CHG41007 *
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
           SELECT DTCSTMT-FILE   ASSIGN TO DTCSTMT
                  FILE STATUS IS WS-DTCSTMT-STATUS.
           SELECT STPOSOUT-FILE  ASSIGN TO STPOSOUT
                  FILE STATUS IS WS-STPOSOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARM-CARD-REC               PIC X(80).
      *----------------------------------------------------------------*
      * DTC STATEMENT - VB.  THREE RECORD LAYOUTS SHARE THE BUFFER,    *
      * THE LENGTH OF THE RECORD JUST READ IS IN WS-DTC-LEN.           *
      *----------------------------------------------------------------*
       FD  DTCSTMT-FILE
           RECORDING MODE IS V
           RECORD VARYING IN SIZE FROM 20 TO 413 CHARACTERS
               DEPENDING ON WS-DTC-LEN
           BLOCK CONTAINS 0 RECORDS.
       COPY RCDTCST.
       FD  STPOSOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  STPOSOUT-REC                PIC X(120).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCB100'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-DTCSTMT-STATUS       PIC X(02)  VALUE '00'.
               88  DTCSTMT-OK                     VALUE '00' '04'.
               88  DTCSTMT-EOF                    VALUE '10'.
           05  WS-STPOSOUT-STATUS      PIC X(02)  VALUE '00'.
      *----------------------------------------------------------------*
      * RECORD LENGTH OF THE VB RECORD (RECORD VARYING DEPENDING ON)   *
      *----------------------------------------------------------------*
       01  WS-DTC-LEN                  PIC 9(05)  COMP VALUE ZERO.
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-STATEMENT               VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-HEADER-SW            PIC X(01)  VALUE 'N'.
               88  HEADER-SEEN                    VALUE 'Y'.
           05  WS-TRAILER-SW           PIC X(01)  VALUE 'N'.
               88  TRAILER-SEEN                   VALUE 'Y'.
           05  WS-DATECHK-SW           PIC X(01)  VALUE 'Y'.
               88  CHECK-STMT-DATE                VALUE 'Y'.
           05  WS-DETAIL-OK-SW         PIC X(01)  VALUE 'Y'.
               88  DETAIL-OK                      VALUE 'Y'.
               88  DETAIL-REJECTED                VALUE 'N'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-EXPECTED-PART            PIC X(04)  VALUE SPACES.
       01  WS-HDR-PARTICIPANT          PIC X(04)  VALUE SPACES.
       01  WS-HDR-STMT-DATE            PIC 9(08)  VALUE ZERO.
       01  WS-HDR-FILE-SEQ             PIC 9(05)  VALUE ZERO.
       01  WS-HDR-CREATE-TS            PIC X(14)  VALUE SPACES.
       01  WS-PARM-KEYWORD             PIC X(10).
       01  WS-PARM-VALUE               PIC X(20).
      *----------------------------------------------------------------*
      * RECORD LENGTHS (DATA PORTION, WITHOUT THE RDW)                 *
      *----------------------------------------------------------------*
       01  WS-LENGTH-CONSTANTS.
           05  WS-HDR-MIN-LEN          PIC 9(05)  COMP VALUE 40.
           05  WS-TRL-MIN-LEN          PIC 9(05)  COMP VALUE 24.
           05  WS-DTL-FIXED-LEN        PIC 9(05)  COMP VALUE 33.
           05  WS-SEG-LEN              PIC 9(05)  COMP VALUE 19.
           05  WS-MAX-SEGMENTS         PIC 9(02)       VALUE 20.
       01  WS-EXPECTED-LEN             PIC 9(05)  COMP VALUE ZERO.
       01  WS-PREV-CUSIP               PIC X(09)  VALUE LOW-VALUES.
      *----------------------------------------------------------------*
      * SEGMENT TYPE TABLE                                             *
      *----------------------------------------------------------------*
       01  WS-SEG-TYPE-VALUES.
           05  FILLER  PIC X(16)  VALUE 'FRFREE          '.
           05  FILLER  PIC X(16)  VALUE 'PLPLEDGED       '.
           05  FILLER  PIC X(16)  VALUE 'SGSEGREGATED    '.
           05  FILLER  PIC X(16)  VALUE 'DPDELIVER PEND  '.
           05  FILLER  PIC X(16)  VALUE 'RPRECEIVE PEND  '.
       01  WS-SEG-TYPE-TABLE REDEFINES WS-SEG-TYPE-VALUES.
           05  WS-ST-ENTRY OCCURS 5 TIMES INDEXED BY ST-IDX.
               10  WS-ST-CODE          PIC X(02).
               10  WS-ST-NAME          PIC X(14).
       01  WS-SEG-COUNTS.
           05  WS-SEG-TYPE-CNT OCCURS 5 TIMES
                                       PIC S9(09) COMP-3.
       01  WS-SUB                      PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * PER-DETAIL WORK                                                *
      *----------------------------------------------------------------*
       01  WS-DETAIL-WORK.
           05  WS-SEG-SUM              PIC S9(15)V9(04) COMP-3.
           05  WS-FREE-QTY             PIC S9(13)V9(04) COMP-3.
           05  WS-PLEDGED-QTY          PIC S9(13)V9(04) COMP-3.
           05  WS-SEGR-QTY             PIC S9(13)V9(04) COMP-3.
           05  WS-DELPEND-QTY          PIC S9(13)V9(04) COMP-3.
           05  WS-RECPEND-QTY          PIC S9(13)V9(04) COMP-3.
           05  WS-SEG-DIFF             PIC S9(15)V9(04) COMP-3.
           05  WS-SEG-IX               PIC S9(04) COMP.
           05  WS-REJECT-REASON        PIC X(40).
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-HDR-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DTL-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TRL-READ             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OTHER-READ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEGS-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DTL-REJECTED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEG-OUT-BAL          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNKNOWN-SEG          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PADDED-RECS          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-STPOS-OUT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ZERO-TOTAL           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DUP-CUSIP            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-OUT-OF-SEQ           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MAX-SEGS-SEEN        PIC S9(04) COMP   VALUE ZERO.
           05  WS-PARM-CNT             PIC S9(04) COMP   VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-DTL-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-OUT-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-SEG-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-TRL-QTY-HASH         PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-TRL-DTL-COUNT        PIC 9(09)         VALUE ZERO.
       COPY RCSTPOS.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-QTY             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.9999.
           05  WS-DISP-LEN             PIC ZZZZ9.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-RECORD THRU 2000-EXIT
               UNTIL END-OF-STATEMENT.
           PERFORM 3000-END-OF-FILE-CHECKS THRU 3000-EXIT.
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
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'DTC POSITION STATEMENT LOAD STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           INITIALIZE WS-SEG-COUNTS.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           OPEN INPUT DTCSTMT-FILE.
           IF WS-DTCSTMT-STATUS NOT = '00'
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE WS-DTCSTMT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT STPOSOUT-FILE.
           IF WS-STPOSOUT-STATUS NOT = '00'
               MOVE 'STPOSOUT' TO AB-DDNAME
               MOVE WS-STPOSOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-DTC THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PARAMETER CARDS - OPTIONAL (DD DUMMY GIVES THE DEFAULTS)       *
      *----------------------------------------------------------------*
       1100-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'RCB100 NO SYSIN PARAMETERS (STATUS '
                       WS-PARMCARD-STATUS ') - DEFAULTS USED'
               GO TO 1100-EXIT
           END-IF.
           PERFORM 1110-READ-ONE-PARM THRU 1110-EXIT
               UNTIL END-OF-PARMS.
           CLOSE PARMCARD.
       1100-EXIT.
           EXIT.
       1110-READ-ONE-PARM.
           READ PARMCARD.
           IF PARMCARD-EOF
               MOVE 'Y' TO WS-PARM-EOF-SW
               GO TO 1110-EXIT
           END-IF.
           IF NOT PARMCARD-OK
               MOVE 'SYSIN' TO AB-DDNAME
               MOVE WS-PARMCARD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '1110-READ-ONE-PARM' TO AB-PARAGRAPH
               MOVE 'READ FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF PARM-CARD-REC (1:1) = '*' OR PARM-CARD-REC = SPACES
               GO TO 1110-EXIT
           END-IF.
           ADD 1 TO WS-PARM-CNT.
           DISPLAY 'RCB100 PARM: ' PARM-CARD-REC (1:60).
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARM-CARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING.
           EVALUATE WS-PARM-KEYWORD
               WHEN 'DATECHK'
                   IF WS-PARM-VALUE (1:1) = 'N'
                       MOVE 'N' TO WS-DATECHK-SW
                       MOVE 'WRIT'  TO AU-FUNCTION
                       MOVE 'PARMOVR' TO AU-EVENT
                       MOVE 'W'     TO AU-SEVERITY
                       MOVE 'DATECHK=N' TO AU-KEY
                       MOVE 'STATEMENT DATE CHECK SWITCHED OFF BY SYSIN'
                                    TO AU-MESSAGE
                       CALL 'CMU060' USING AU-AUDIT-PARMS
                   ELSE
                       MOVE 'Y' TO WS-DATECHK-SW
                   END-IF
               WHEN 'PARTID'
                   MOVE WS-PARM-VALUE (1:4) TO WS-EXPECTED-PART
               WHEN OTHER
                   DISPLAY 'RCB100 UNKNOWN PARAMETER IGNORED: '
                           WS-PARM-KEYWORD
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *================================================================*
      * ONE STATEMENT RECORD - TYPE IS THE FIRST BYTE OF EVERY LAYOUT  *
      *================================================================*
       2000-PROCESS-RECORD.
           ADD 1 TO WS-RECS-READ.
           IF TRAILER-SEEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2000-PROCESS-RECORD' TO AB-PARAGRAPH
               MOVE WS-RECS-READ TO WS-DISP-CNT
               MOVE WS-DISP-CNT TO AB-KEY
               MOVE 'RECORDS FOUND AFTER THE DTC TRAILER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           EVALUATE TRUE
               WHEN DTH-HEADER
                   PERFORM 2100-PROCESS-HEADER THRU 2100-EXIT
               WHEN DTD-DETAIL
                   PERFORM 2200-PROCESS-DETAIL THRU 2200-EXIT
               WHEN DTT-TRAILER
                   PERFORM 2300-PROCESS-TRAILER THRU 2300-EXIT
               WHEN OTHER
                   ADD 1 TO WS-OTHER-READ
                   DISPLAY 'RCB100 UNKNOWN RECORD TYPE ''' DTH-REC-TYPE
                           ''' AT RECORD ' WS-RECS-READ ' - SKIPPED'
                   IF WS-RETURN-CODE < 4
                       MOVE 4 TO WS-RETURN-CODE
                   END-IF
           END-EVALUATE.
           PERFORM 8000-READ-DTC THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *================================================================*
      * HEADER                                                         *
      *================================================================*
       2100-PROCESS-HEADER.
           ADD 1 TO WS-HDR-READ.
           IF HEADER-SEEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2100-PROCESS-HEADER' TO AB-PARAGRAPH
               MOVE 'SECOND HEADER - TWO STATEMENTS CONCATENATED?'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF WS-DTC-LEN < WS-HDR-MIN-LEN
               MOVE WS-DTC-LEN TO WS-DISP-LEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2100-PROCESS-HEADER' TO AB-PARAGRAPH
               STRING 'HEADER LENGTH ' WS-DISP-LEN
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'DTC HEADER RECORD TOO SHORT' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'Y'               TO WS-HEADER-SW.
           MOVE DTH-PARTICIPANT   TO WS-HDR-PARTICIPANT.
           MOVE DTH-CREATE-TS     TO WS-HDR-CREATE-TS.
           IF DTH-STMT-DATE NUMERIC
               MOVE DTH-STMT-DATE TO WS-HDR-STMT-DATE
           ELSE
               MOVE ZERO          TO WS-HDR-STMT-DATE
           END-IF.
           IF DTH-FILE-SEQ NUMERIC
               MOVE DTH-FILE-SEQ  TO WS-HDR-FILE-SEQ
           END-IF.
           DISPLAY 'RCB100 DTC STATEMENT PARTICIPANT ' DTH-PARTICIPANT
                   ' DATE ' WS-HDR-STMT-DATE ' FILE SEQ '
                   WS-HDR-FILE-SEQ ' CREATED ' DTH-CREATE-TS.
      *    STATEMENT IS AS OF THE PRIOR BUSINESS DAY CLOSE (DTC SENDS
      *    IT OVERNIGHT) - A STALE FILE MUST NOT BE RECONCILED
           IF CHECK-STMT-DATE
               IF WS-HDR-STMT-DATE NOT = DC-PREV-BUS-DATE
                   MOVE 'DTCSTMT' TO AB-DDNAME
                   MOVE 1005 TO AB-ABEND-CODE
                   MOVE '2100-PROCESS-HEADER' TO AB-PARAGRAPH
                   STRING 'STMT ' WS-HDR-STMT-DATE ' EXPECTED '
                          DC-PREV-BUS-DATE
                          DELIMITED BY SIZE INTO AB-KEY
                   MOVE 'DTC STATEMENT DATE NOT PREVIOUS BUSINESS DAY'
                                   TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
           END-IF.
           IF WS-EXPECTED-PART NOT = SPACES
           AND WS-EXPECTED-PART NOT = DTH-PARTICIPANT
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2100-PROCESS-HEADER' TO AB-PARAGRAPH
               STRING 'PARTICIPANT ' DTH-PARTICIPANT ' EXPECTED '
                      WS-EXPECTED-PART
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'STATEMENT IS FOR ANOTHER DTC PARTICIPANT'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       2100-EXIT.
           EXIT.
      *================================================================*
      * DETAIL - ONE CUSIP, SEGMENTS IN DTD-SEGMENT (1 TO SEG-COUNT)   *
      *================================================================*
       2200-PROCESS-DETAIL.
           ADD 1 TO WS-DTL-READ.
           MOVE 'Y' TO WS-DETAIL-OK-SW.
           MOVE SPACES TO WS-REJECT-REASON.
           IF NOT HEADER-SEEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2200-PROCESS-DETAIL' TO AB-PARAGRAPH
               MOVE 'DETAIL RECORD BEFORE THE DTC HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 2210-EDIT-DETAIL THRU 2210-EXIT.
      *    THE TOTAL COUNTS INTO THE HASH EVEN WHEN THE DETAIL IS BAD -
      *    DTC HASHES EVERY DETAIL IT SENDS
           IF DTD-TOTAL-QTY NUMERIC
               ADD DTD-TOTAL-QTY TO WS-DTL-QTY-HASH
           END-IF.
           IF DETAIL-REJECTED
               ADD 1 TO WS-DTL-REJECTED
               DISPLAY 'RCB100 DETAIL REJECTED CUSIP ' DTD-CUSIP
                       ' - ' WS-REJECT-REASON
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               GO TO 2200-EXIT
           END-IF.
           PERFORM 2215-CUSIP-SEQUENCE THRU 2215-EXIT.
           PERFORM 2220-SUM-SEGMENTS THRU 2220-EXIT.
           PERFORM 2230-CHECK-SEGMENTS THRU 2230-EXIT.
           PERFORM 2240-BUILD-STREET-POSN THRU 2240-EXIT.
           PERFORM 8200-WRITE-STPOS THRU 8200-EXIT.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STRUCTURE EDITS.  THE SEGMENT COUNT MUST BE CHECKED BEFORE ANY *
      * SEGMENT IS REFERENCED (OCCURS DEPENDING ON).                   *
      *----------------------------------------------------------------*
       2210-EDIT-DETAIL.
           IF DTD-SEG-COUNT NOT NUMERIC
               MOVE 'N' TO WS-DETAIL-OK-SW
               MOVE 'SEGMENT COUNT NOT NUMERIC' TO WS-REJECT-REASON
               GO TO 2210-EXIT
           END-IF.
           IF DTD-SEG-COUNT < 1 OR DTD-SEG-COUNT > WS-MAX-SEGMENTS
               MOVE 'N' TO WS-DETAIL-OK-SW
               MOVE 'SEGMENT COUNT NOT 01-20' TO WS-REJECT-REASON
               GO TO 2210-EXIT
           END-IF.
           COMPUTE WS-EXPECTED-LEN =
                   WS-DTL-FIXED-LEN + WS-SEG-LEN * DTD-SEG-COUNT.
      *    CHG21340 - DTC PADS THE DETAIL TO A FULLWORD BOUNDARY SINCE
      *    THE 2011 FORMAT CHANGE.  SHORTER IS STILL AN ERROR.
           IF WS-DTC-LEN < WS-EXPECTED-LEN
               MOVE 'N' TO WS-DETAIL-OK-SW
               MOVE 'RECORD SHORTER THAN SEGMENT COUNT'
                               TO WS-REJECT-REASON
               GO TO 2210-EXIT
           END-IF.
           IF WS-DTC-LEN > WS-EXPECTED-LEN
               ADD 1 TO WS-PADDED-RECS
           END-IF.
           IF DTD-TOTAL-QTY NOT NUMERIC
               MOVE 'N' TO WS-DETAIL-OK-SW
               MOVE 'TOTAL QUANTITY NOT NUMERIC' TO WS-REJECT-REASON
               GO TO 2210-EXIT
           END-IF.
           IF DTD-CUSIP = SPACES OR DTD-CUSIP = LOW-VALUES
               MOVE 'N' TO WS-DETAIL-OK-SW
               MOVE 'CUSIP MISSING' TO WS-REJECT-REASON
               GO TO 2210-EXIT
           END-IF.
           IF DTD-PARTICIPANT NOT = WS-HDR-PARTICIPANT
               MOVE 'N' TO WS-DETAIL-OK-SW
               MOVE 'PARTICIPANT DIFFERS FROM HEADER'
                               TO WS-REJECT-REASON
               GO TO 2210-EXIT
           END-IF.
           IF DTD-SEG-COUNT > WS-MAX-SEGS-SEEN
               MOVE DTD-SEG-COUNT TO WS-MAX-SEGS-SEEN
           END-IF.
       2210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * DTC SENDS THE DETAILS IN CUSIP ORDER, ONE PER CUSIP.  A REPEAT *
      * IS REPORTED (CHG29112); ORDER IS ONLY COUNTED - STEP030 SORTS. *
      *----------------------------------------------------------------*
       2215-CUSIP-SEQUENCE.
           IF DTD-CUSIP = WS-PREV-CUSIP
               ADD 1 TO WS-DUP-CUSIP
               DISPLAY 'RCB100 CUSIP ' DTD-CUSIP
                       ' REPEATED ON THE STATEMENT - BOTH LINES KEPT'
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           ELSE
               IF DTD-CUSIP < WS-PREV-CUSIP
                   ADD 1 TO WS-OUT-OF-SEQ
               END-IF
           END-IF.
           MOVE DTD-CUSIP TO WS-PREV-CUSIP.
       2215-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ADD UP THE BALANCE SEGMENTS BY TYPE                            *
      *----------------------------------------------------------------*
       2220-SUM-SEGMENTS.
           MOVE ZERO TO WS-SEG-SUM WS-FREE-QTY WS-PLEDGED-QTY
                        WS-SEGR-QTY WS-DELPEND-QTY WS-RECPEND-QTY.
           PERFORM VARYING WS-SEG-IX FROM 1 BY 1
                   UNTIL WS-SEG-IX > DTD-SEG-COUNT
               ADD 1 TO WS-SEGS-READ
               IF DTD-SEG-QTY (WS-SEG-IX) NOT NUMERIC
                   DISPLAY 'RCB100 CUSIP ' DTD-CUSIP ' SEGMENT '
                           WS-SEG-IX ' QTY NOT NUMERIC - TAKEN AS ZERO'
                   MOVE ZERO TO DTD-SEG-QTY (WS-SEG-IX)
               END-IF
               ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-SEG-SUM
                                              WS-SEG-QTY-HASH
               SET ST-IDX TO 1
               SEARCH WS-ST-ENTRY
                   AT END
                       ADD 1 TO WS-UNKNOWN-SEG
                       DISPLAY 'RCB100 CUSIP ' DTD-CUSIP
                               ' UNKNOWN SEGMENT TYPE '
                               DTD-SEG-TYPE (WS-SEG-IX)
                               ' - COUNTED AS FREE'
                       ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-FREE-QTY
                   WHEN WS-ST-CODE (ST-IDX) = DTD-SEG-TYPE (WS-SEG-IX)
                       SET WS-SUB TO ST-IDX
                       ADD 1 TO WS-SEG-TYPE-CNT (WS-SUB)
                       PERFORM 2225-ADD-SEGMENT THRU 2225-EXIT
               END-SEARCH
           END-PERFORM.
       2220-EXIT.
           EXIT.
       2225-ADD-SEGMENT.
           EVALUATE DTD-SEG-TYPE (WS-SEG-IX)
               WHEN 'FR'
                   ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-FREE-QTY
               WHEN 'PL'
                   ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-PLEDGED-QTY
               WHEN 'SG'
                   ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-SEGR-QTY
               WHEN 'DP'
                   ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-DELPEND-QTY
               WHEN 'RP'
                   ADD DTD-SEG-QTY (WS-SEG-IX) TO WS-RECPEND-QTY
           END-EVALUATE.
       2225-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SUM OF SEGMENTS MUST EQUAL THE CUSIP TOTAL.  THE DTC TOTAL IS  *
      * THE OFFICIAL POSITION - A DIFFERENCE IS REPORTED, THE RECORD   *
      * STILL GOES TO THE RECONCILIATION WITH THE DTC TOTAL.           *
      *----------------------------------------------------------------*
       2230-CHECK-SEGMENTS.
           IF WS-SEG-SUM = DTD-TOTAL-QTY
               GO TO 2230-EXIT
           END-IF.
           ADD 1 TO WS-SEG-OUT-BAL.
           COMPUTE WS-SEG-DIFF = DTD-TOTAL-QTY - WS-SEG-SUM.
           MOVE WS-SEG-DIFF TO WS-DISP-QTY.
           DISPLAY 'RCB100 CUSIP ' DTD-CUSIP
                   ' SEGMENTS DO NOT ADD TO TOTAL - DIFF ' WS-DISP-QTY.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'SEGBAL'       TO AU-EVENT.
           MOVE 'W'            TO AU-SEVERITY.
           MOVE DTD-CUSIP      TO AU-KEY.
           MOVE 'DTC SEGMENTS OUT OF BALANCE WITH CUSIP TOTAL'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           IF WS-RETURN-CODE < 4
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       2230-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2240-BUILD-STREET-POSN.
      *----------------------------------------------------------------*
           MOVE SPACES             TO RSP-STREET-POS-REC.
           MOVE 'DTC '             TO RSP-DEPOSITORY.
           MOVE DTD-CUSIP          TO RSP-CUSIP.
           MOVE WS-HDR-STMT-DATE   TO RSP-STMT-DATE.
           MOVE SPACES             TO RSP-ISIN.
           MOVE DTD-TOTAL-QTY      TO RSP-TOTAL-QTY.
           MOVE WS-FREE-QTY        TO RSP-FREE-QTY.
           MOVE WS-PLEDGED-QTY     TO RSP-PLEDGED-QTY.
           MOVE WS-SEGR-QTY        TO RSP-SEG-QTY.
           MOVE WS-DELPEND-QTY     TO RSP-DEL-PEND-QTY.
           MOVE WS-RECPEND-QTY     TO RSP-REC-PEND-QTY.
           MOVE WS-DTL-READ        TO RSP-SOURCE-SEQ.
           MOVE SPACE              TO RSP-ID-FLAG.
           IF DTD-TOTAL-QTY = ZERO
               ADD 1 TO WS-ZERO-TOTAL
           END-IF.
       2240-EXIT.
           EXIT.
      *================================================================*
      * TRAILER - DETAIL COUNT AND QUANTITY HASH                       *
      *================================================================*
       2300-PROCESS-TRAILER.
           ADD 1 TO WS-TRL-READ.
           IF NOT HEADER-SEEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2300-PROCESS-TRAILER' TO AB-PARAGRAPH
               MOVE 'TRAILER RECORD BEFORE THE DTC HEADER' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF WS-DTC-LEN < WS-TRL-MIN-LEN
               MOVE WS-DTC-LEN TO WS-DISP-LEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '2300-PROCESS-TRAILER' TO AB-PARAGRAPH
               STRING 'TRAILER LENGTH ' WS-DISP-LEN
                      DELIMITED BY SIZE INTO AB-KEY
               MOVE 'DTC TRAILER RECORD TOO SHORT' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'Y' TO WS-TRAILER-SW.
           IF DTT-DETAIL-COUNT NUMERIC
               MOVE DTT-DETAIL-COUNT TO WS-TRL-DTL-COUNT
           ELSE
               MOVE ZERO TO WS-TRL-DTL-COUNT
           END-IF.
           IF DTT-QTY-HASH NUMERIC
               MOVE DTT-QTY-HASH TO WS-TRL-QTY-HASH
           ELSE
               MOVE ZERO TO WS-TRL-QTY-HASH
           END-IF.
           IF WS-TRL-DTL-COUNT NOT = WS-DTL-READ
               MOVE WS-TRL-DTL-COUNT TO WS-DISP-CNT
               DISPLAY 'RCB100 *** TRAILER DETAIL COUNT ' WS-DISP-CNT
               MOVE WS-DTL-READ TO WS-DISP-CNT
               DISPLAY 'RCB100 *** DETAILS READ         ' WS-DISP-CNT
               PERFORM 2390-TRAILER-OUT-OF-BALANCE THRU 2390-EXIT
           END-IF.
           IF WS-TRL-QTY-HASH NOT = WS-DTL-QTY-HASH
               MOVE WS-TRL-QTY-HASH TO WS-DISP-QTY
               DISPLAY 'RCB100 *** TRAILER QTY HASH ' WS-DISP-QTY
               MOVE WS-DTL-QTY-HASH TO WS-DISP-QTY
               DISPLAY 'RCB100 *** DETAIL QTY HASH  ' WS-DISP-QTY
               PERFORM 2390-TRAILER-OUT-OF-BALANCE THRU 2390-EXIT
           END-IF.
       2300-EXIT.
           EXIT.
       2390-TRAILER-OUT-OF-BALANCE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'TRLBAL'       TO AU-EVENT.
           MOVE 'E'            TO AU-SEVERITY.
           MOVE WS-HDR-PARTICIPANT TO AU-KEY.
           MOVE 'DTC STATEMENT TRAILER OUT OF BALANCE - CALL DTC DESK'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 8 TO WS-RETURN-CODE.
       2390-EXIT.
           EXIT.
      *================================================================*
       3000-END-OF-FILE-CHECKS.
      *================================================================*
           IF NOT HEADER-SEEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3000-END-OF-FILE-CHECKS' TO AB-PARAGRAPH
               MOVE 'NO DTC HEADER - EMPTY OR WRONG FILE RECEIVED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF NOT TRAILER-SEEN
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3000-END-OF-FILE-CHECKS' TO AB-PARAGRAPH
               MOVE 'NO DTC TRAILER - STATEMENT TRUNCATED IN TRANSMIT'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       3000-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-DTC.
           MOVE ZERO TO WS-DTC-LEN.
           READ DTCSTMT-FILE.
           EVALUATE TRUE
               WHEN DTCSTMT-OK
                   CONTINUE
               WHEN DTCSTMT-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'DTCSTMT' TO AB-DDNAME
                   MOVE WS-DTCSTMT-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-DTC' TO AB-PARAGRAPH
                   MOVE WS-RECS-READ TO WS-DISP-CNT
                   MOVE WS-DISP-CNT TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-WRITE-STPOS.
      *----------------------------------------------------------------*
           WRITE STPOSOUT-REC FROM RSP-STREET-POS-REC.
           IF WS-STPOSOUT-STATUS NOT = '00'
               MOVE 'STPOSOUT' TO AB-DDNAME
               MOVE WS-STPOSOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-WRITE-STPOS' TO AB-PARAGRAPH
               MOVE RSP-CUSIP TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-STPOS-OUT.
           ADD RSP-TOTAL-QTY TO WS-OUT-QTY-HASH.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RCB100'       TO CT-STAGE.
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
           CLOSE DTCSTMT-FILE.
           IF WS-DTCSTMT-STATUS NOT = '00'
               MOVE 'DTCSTMT' TO AB-DDNAME
               MOVE WS-DTCSTMT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE STPOSOUT-FILE.
           IF WS-STPOSOUT-STATUS NOT = '00'
               MOVE 'STPOSOUT' TO AB-DDNAME
               MOVE WS-STPOSOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'DTC-DETAIL-IN'  TO CT-COUNTER-NAME.
           MOVE WS-DTL-READ      TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           MOVE WS-DTL-QTY-HASH  TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'SEGMENTS-IN'    TO CT-COUNTER-NAME.
           MOVE WS-SEGS-READ     TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           MOVE WS-SEG-QTY-HASH  TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'STPOS-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-STPOS-OUT     TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT.
           MOVE WS-OUT-QTY-HASH  TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'DTL-REJECTED'   TO CT-COUNTER-NAME.
           MOVE WS-DTL-REJECTED  TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           PERFORM 9100-DISPLAY-STATISTICS THRU 9100-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
                   MOVE 'DTC STATEMENT LOADED - TRAILER OUT OF BALANCE'
                                   TO AU-MESSAGE
               WHEN WS-RETURN-CODE = 4
                   MOVE 'W' TO AU-SEVERITY
                   MOVE 'DTC STATEMENT LOADED WITH WARNINGS'
                                   TO AU-MESSAGE
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
                   MOVE 'DTC STATEMENT LOADED' TO AU-MESSAGE
           END-EVALUATE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9100-DISPLAY-STATISTICS.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'.
           DISPLAY '* RCB100 - DTC POSITION STATEMENT LOAD         *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' STATEMENT DATE           : ' WS-HDR-STMT-DATE.
           DISPLAY ' DTC PARTICIPANT          : ' WS-HDR-PARTICIPANT.
           MOVE WS-RECS-READ TO WS-DISP-CNT.
           DISPLAY ' RECORDS READ             : ' WS-DISP-CNT.
           MOVE WS-HDR-READ TO WS-DISP-CNT.
           DISPLAY '   HEADERS                : ' WS-DISP-CNT.
           MOVE WS-DTL-READ TO WS-DISP-CNT.
           DISPLAY '   DETAILS                : ' WS-DISP-CNT.
           MOVE WS-TRL-READ TO WS-DISP-CNT.
           DISPLAY '   TRAILERS               : ' WS-DISP-CNT.
           MOVE WS-OTHER-READ TO WS-DISP-CNT.
           DISPLAY '   UNKNOWN TYPES SKIPPED  : ' WS-DISP-CNT.
           MOVE WS-SEGS-READ TO WS-DISP-CNT.
           DISPLAY ' BALANCE SEGMENTS READ    : ' WS-DISP-CNT.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 5
               MOVE WS-SEG-TYPE-CNT (WS-SUB) TO WS-DISP-CNT
               DISPLAY '   ' WS-ST-CODE (WS-SUB) ' '
                       WS-ST-NAME (WS-SUB) '      : ' WS-DISP-CNT
           END-PERFORM.
           MOVE WS-UNKNOWN-SEG TO WS-DISP-CNT.
           DISPLAY '   UNKNOWN SEGMENT TYPES  : ' WS-DISP-CNT.
           MOVE WS-MAX-SEGS-SEEN TO WS-DISP-CNT.
           DISPLAY ' MOST SEGMENTS ON A CUSIP : ' WS-DISP-CNT.
           MOVE WS-PADDED-RECS TO WS-DISP-CNT.
           DISPLAY ' PADDED DETAIL RECORDS    : ' WS-DISP-CNT.
           MOVE WS-DTL-REJECTED TO WS-DISP-CNT.
           DISPLAY ' DETAILS REJECTED         : ' WS-DISP-CNT.
           MOVE WS-SEG-OUT-BAL TO WS-DISP-CNT.
           DISPLAY ' SEGMENTS OUT OF BALANCE  : ' WS-DISP-CNT.
           MOVE WS-ZERO-TOTAL TO WS-DISP-CNT.
           DISPLAY ' ZERO BALANCE CUSIPS      : ' WS-DISP-CNT.
           MOVE WS-DUP-CUSIP TO WS-DISP-CNT.
           DISPLAY ' CUSIPS REPEATED          : ' WS-DISP-CNT.
           MOVE WS-OUT-OF-SEQ TO WS-DISP-CNT.
           DISPLAY ' DETAILS OUT OF SEQUENCE  : ' WS-DISP-CNT.
           MOVE WS-STPOS-OUT TO WS-DISP-CNT.
           DISPLAY ' STREET POSITIONS WRITTEN : ' WS-DISP-CNT.
           MOVE WS-DTL-QTY-HASH TO WS-DISP-QTY.
           DISPLAY ' DETAIL QUANTITY HASH     : ' WS-DISP-QTY.
           MOVE WS-TRL-QTY-HASH TO WS-DISP-QTY.
           DISPLAY ' TRAILER QUANTITY HASH    : ' WS-DISP-QTY.
           MOVE WS-TRL-DTL-COUNT TO WS-DISP-CNT.
           DISPLAY ' TRAILER DETAIL COUNT     : ' WS-DISP-CNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
       9100-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RCB100 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RCB100 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RCB100 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
