      *================================================================*
      * PROGRAM    : CAB100                                            *
      * DESCRIPTION: CORPORATE ACTION ANNOUNCEMENT LOAD.               *
      *              READS THE DATA VENDOR ANNOUNCEMENT FEED AND       *
      *              APPLIES NEW / UPDATE / CANCEL ACTIONS TO THE      *
      *              CORPORATE ACTION EVENT MASTER.  EVERY FEED RECORD *
      *              IS VALIDATED AND A RESULT RECORD IS WRITTEN FOR   *
      *              THE EVENT VALIDATION REPORT (CAR110).             *
      *----------------------------------------------------------------*
      * JOB        : MSCAD010  STEP010  (IKJEFT01 - PLAN MSCAPLN)      *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              SYSIN     CONTROL CARD CAP100A                    *
      *              ANNFEED   VENDOR FEED  MSEC.PROD.CA.ANNFEED.RAW(0)*
      *              CAEVENT   EVENT MASTER KSDS (I-O)                 *
      * OUTPUT     : CAEVENT   EVENT MASTER KSDS                       *
      *              CAVALOUT  VALIDATION RESULTS (CAVALID)            *
      *                        MSEC.PROD.CA.ANNVAL(+1)                 *
      * CALLS      : CMD010 (SECURITY MASTER)   CMU010 (DATES)         *
      *              CMU050 (ABEND)  CMU060 (AUDIT)  CMU080 (TOTALS)   *
      *              CMASM01 (JOB INFO)                                *
      *----------------------------------------------------------------*
      * EVENT ID ASSIGNMENT:                                           *
      *   NEW EVENTS ARE NUMBERED 'CA' + BUSINESS DATE (CCYYMMDD) +    *
      *   2 DIGIT SEQUENCE 01-99 WITHIN THE LOAD DATE.  THE STARTING   *
      *   SEQUENCE IS THE HIGHEST ALREADY ON FILE FOR THE DATE (RERUN  *
      *   SAFE).  UPDATES AND CANCELS LOCATE THE EVENT THROUGH THE     *
      *   VENDOR REFERENCE (CAE-VENDOR-REF), HELD IN A TABLE LOADED    *
      *   FROM THE EVENT MASTER AT START OF RUN.                       *
      *----------------------------------------------------------------*
      * RETURN CODES:                                                  *
      *   00 ALL RECORDS ACCEPTED                                      *
      *   04 WARNINGS AND/OR REJECTED ANNOUNCEMENTS                    *
      *   08 FILE LEVEL ERROR (HEADER/TRAILER) OR REJECTS OVER MAXREJ  *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1993-08-16 DWB  ORIGINAL - CASH/STOCK DIV AND SPLITS           *
      * 1994-03-02 DWB  REVERSE SPLITS                        CHG00412 *
      * 1995-10-19 RJK  VENDOR REF TABLE FOR UPDATE/CANCEL    CHG01370 *
      * 1996-04-22 DWB  SECURITY MASTER NOW DB2 (CMD010)      CHG02215 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES, EVENT ID FORMAT CHG04471 *
      * 1999-02-15 TLM  Y2K - REMOVED 2 DIGIT YEAR WINDOW     CHG04602 *
      * 2001-04-09 KAP  DECIMALIZATION - CIL PRICE 6 DEC      CHG08260 *
      * 2004-07-12 KAP  CONTROL CARD MAXREJ / PASTREC         CHG12118 *
      * 2008-02-25 KAP  CASH MERGER EVENTS (MRG)              CHG17444 *
      * 2011-06-20 SPA  AUDIT AND CONTROL TOTALS VIA CMU060/80CHG21877 *
      * 2013-01-14 SPA  DUPLICATE KEY ON WRITE - RETRY SEQ    CHG25003 *
      * 2016-05-02 MFO  HALTED SECURITY NOW WARNING NOT REJECTCHG30055 *
      * 2019-11-18 MFO  CANCEL ALLOWED AFTER ELIGIBILITY (EL) CHG34410 *
      * 2024-02-12 NVR  T+1 - EX DATE MAY EQUAL RECORD DATE   CHG41120 *
      *                 FROM 2024-05-28                                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAB100.
       AUTHOR.        D W BRANDT.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  08/16/93.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE   ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
      *
           SELECT PARMCARD        ASSIGN TO SYSIN
                  FILE STATUS IS WS-PARMCARD-STATUS.
      *
           SELECT ANNFEED-FILE    ASSIGN TO ANNFEED
                  FILE STATUS IS WS-ANNFEED-STATUS.
      *
           SELECT CAEVENT-FILE    ASSIGN TO CAEVENT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CAE-EVENT-ID
                  FILE STATUS IS WS-CAEVENT-STATUS.
      *
           SELECT CAVALOUT-FILE   ASSIGN TO CAVALOUT
                  FILE STATUS IS WS-CAVALOUT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARMCARD-REC                PIC X(80).
      *
       FD  ANNFEED-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CAANNFD.
      *
       FD  CAEVENT-FILE.
           COPY CAEVENT.
      *
       FD  CAVALOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CAVALOUT-REC                PIC X(200).
      *
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
           VALUE 'CAB100 WORKING STORAGE BEGINS'.
      *
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAB100'.
      *
      *----------------------------------------------------------------*
      * FILE STATUS FIELDS                                             *
      *----------------------------------------------------------------*
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-ANNFEED-STATUS       PIC X(02) VALUE '00'.
               88  ANNFEED-OK                    VALUE '00'.
               88  ANNFEED-EOF                   VALUE '10'.
           05  WS-CAEVENT-STATUS       PIC X(02) VALUE '00'.
               88  CAEVENT-OK                    VALUE '00'.
               88  CAEVENT-EOF                   VALUE '10'.
               88  CAEVENT-DUPKEY                VALUE '22'.
               88  CAEVENT-NOTFND                VALUE '23'.
           05  WS-CAVALOUT-STATUS      PIC X(02) VALUE '00'.
      *
      *----------------------------------------------------------------*
      * SWITCHES                                                       *
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05  WS-FEED-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-FEED-EOF                   VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-HEADER-SW            PIC X(01) VALUE 'N'.
               88  WS-HEADER-FOUND               VALUE 'Y'.
           05  WS-TRAILER-SW           PIC X(01) VALUE 'N'.
               88  WS-TRAILER-FOUND              VALUE 'Y'.
           05  WS-TBL-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-TBL-EOF                    VALUE 'Y'.
           05  WS-REJECT-SW            PIC X(01) VALUE 'N'.
               88  WS-REC-REJECTED               VALUE 'Y'.
           05  WS-WARNING-SW           PIC X(01) VALUE 'N'.
               88  WS-REC-WARNING                VALUE 'Y'.
           05  WS-FOUND-SW             PIC X(01) VALUE 'N'.
               88  WS-EVENT-IN-TABLE             VALUE 'Y'.
           05  WS-WRITE-DONE-SW        PIC X(01) VALUE 'N'.
               88  WS-WRITE-DONE                 VALUE 'Y'.
           05  WS-FILE-ERROR-SW        PIC X(01) VALUE 'N'.
               88  WS-FILE-ERROR                 VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * CONTROL CARD  (CAP100A)                                        *
      * CAB100 VENDOR=XXXXXXXX MAXREJ=NNNN PASTREC=Y                   *
      *----------------------------------------------------------------*
       01  WS-PARM-CARD.
           05  PC-PROGRAM              PIC X(06).
           05  FILLER                  PIC X(01).
           05  PC-VENDOR-KW            PIC X(07).
           05  PC-VENDOR               PIC X(08).
           05  FILLER                  PIC X(01).
           05  PC-MAXREJ-KW            PIC X(07).
           05  PC-MAXREJ               PIC 9(04).
           05  FILLER                  PIC X(01).
           05  PC-PASTREC-KW           PIC X(08).
           05  PC-PASTREC              PIC X(01).
           05  FILLER                  PIC X(36).
      *
       01  WS-PARMS.
           05  WS-PARM-VENDOR          PIC X(08) VALUE SPACES.
           05  WS-PARM-MAXREJ          PIC S9(07) COMP-3 VALUE +50.
           05  WS-PARM-PASTREC         PIC X(01) VALUE 'N'.
               88  WS-ALLOW-PAST-RECORD          VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-RECS-READ            PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-READ          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-ADDED         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-UPDATED       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-CANCELLED     PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-REJECTED      PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-WARNED        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-VAL-WRITTEN          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-DUPKEY-RETRIES       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-AFTER-TRAILER        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-TBL-LOADED           PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-RATE-HASH            PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * EVENT ID WORK AREA                                             *
      *----------------------------------------------------------------*
       01  WS-EVENT-ID-WORK.
           05  WS-EID-PREFIX           PIC X(02) VALUE 'CA'.
           05  WS-EID-DATE             PIC 9(08) VALUE ZERO.
           05  WS-EID-SEQ              PIC 9(02) VALUE ZERO.
       01  WS-EVENT-ID-X   REDEFINES WS-EVENT-ID-WORK
                                       PIC X(12).
       01  WS-HIGH-SEQ                 PIC 9(02) VALUE ZERO.
       01  WS-RETRY-COUNT              PIC S9(03) COMP-3 VALUE ZERO.
       01  WS-SCAN-ID.
           05  WS-SCAN-PREFIX          PIC X(10).
           05  WS-SCAN-SEQ             PIC X(02).
      *
      *----------------------------------------------------------------*
      * VENDOR REFERENCE TABLE (LOADED FROM EVENT MASTER)              *
      *----------------------------------------------------------------*
       01  WS-VREF-TABLE-CTL.
           05  WS-VREF-MAX             PIC S9(05) COMP VALUE +9000.
           05  WS-VREF-COUNT           PIC S9(05) COMP VALUE ZERO.
           05  WS-VREF-HIT             PIC S9(05) COMP VALUE ZERO.
       01  WS-VREF-TABLE.
           05  WS-VREF-ENTRY           OCCURS 9000 TIMES
                                       INDEXED BY VX-IDX.
               10  WS-VREF-REF         PIC X(12).
               10  WS-VREF-EVENT-ID    PIC X(12).
               10  WS-VREF-STATUS      PIC X(02).
      *
      *----------------------------------------------------------------*
      * VALIDATION WORK                                                *
      *----------------------------------------------------------------*
       01  WS-VAL-WORK.
           05  WS-MSG-COUNT            PIC 9(01) VALUE ZERO.
           05  WS-MSG-CODE             PIC X(04) OCCURS 3 TIMES.
           05  WS-NEW-MSG              PIC X(04).
           05  WS-OLD-STATUS           PIC X(02).
           05  WS-NEW-STATUS           PIC X(02).
           05  WS-DATE-TO-CHECK        PIC 9(08).
           05  WS-DATE-VALID-SW        PIC X(01).
               88  WS-DATE-IS-VALID              VALUE 'Y'.
      *
       01  WS-CONSTANTS.
           05  WS-TPLUS1-DATE          PIC 9(08) VALUE 20240528.
           05  WS-VALID-CCYS           PIC X(18)
               VALUE 'USDEURGBPJPYCADCHF'.
           05  WS-VALID-CCY-TBL REDEFINES WS-VALID-CCYS.
               10  WS-VALID-CCY        PIC X(03) OCCURS 6 TIMES.
           05  WS-CCY-IDX              PIC S9(03) COMP VALUE ZERO.
           05  WS-CCY-FOUND-SW         PIC X(01) VALUE 'N'.
               88  WS-CCY-FOUND                  VALUE 'Y'.
      *
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
       01  WS-DISP-COUNT               PIC ZZZ,ZZ9.
      *
      *----------------------------------------------------------------*
      * RECORD AREAS                                                   *
      *----------------------------------------------------------------*
           COPY CMDATEW.
           COPY CAVALID.
           COPY CMSECMS.
      *
      *----------------------------------------------------------------*
      * LINKAGE AREAS FOR CALLED MODULES                               *
      *----------------------------------------------------------------*
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMDTLNK.
           COPY CMSECLNK.
           COPY CMJILNK.
      *
       01  FILLER                      PIC X(32)
           VALUE 'CAB100 WORKING STORAGE ENDS'.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
      *
           PERFORM 2000-PROCESS-FEED THRU 2000-EXIT
               UNTIL WS-FEED-EOF.
      *
           PERFORM 3000-CHECK-FILE-LEVEL THRU 3000-EXIT.
      *
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
      *
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           INITIALIZE AB-ABEND-PARMS.
           CALL 'CMASM01' USING JI-JOB-INFO.
      *
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON DATE CARD' TO AB-MESSAGE
               GO TO 9999-ABEND.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DC-VALID-CARD
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1005                TO AB-ABEND-CODE
               MOVE WS-DATECARD-STATUS  TO AB-FILE-STATUS
               MOVE 'DATECARD'          TO AB-DDNAME
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE DATECARD-FILE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID    TO AU-PROGRAM.
           MOVE 'START'          TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE DC-BUS-DATE      TO AU-BUS-DATE.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA ANNOUNCEMENT LOAD STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           PERFORM 1100-READ-PARMCARD THRU 1100-EXIT.
      *
           OPEN INPUT ANNFEED-FILE.
           IF WS-ANNFEED-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ANNFEED-STATUS   TO AB-FILE-STATUS
               MOVE 'ANNFEED'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON ANNOUNCEMENT FEED' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           OPEN I-O CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           OPEN OUTPUT CAVALOUT-FILE.
           IF WS-CAVALOUT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAVALOUT-STATUS  TO AB-FILE-STATUS
               MOVE 'CAVALOUT'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON VALIDATION FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           MOVE DC-BUS-DATE TO WS-EID-DATE.
           PERFORM 1200-LOAD-VREF-TABLE THRU 1200-EXIT.
      *
           PERFORM 8000-READ-FEED THRU 8000-EXIT.
           IF WS-FEED-EOF
               DISPLAY 'CAB100 - ANNOUNCEMENT FEED IS EMPTY'
               MOVE 'F002' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT
               GO TO 1000-EXIT.
           IF CAN-HEADER
               PERFORM 1300-CHECK-HEADER THRU 1300-EXIT
               PERFORM 8000-READ-FEED THRU 8000-EXIT
           ELSE
               DISPLAY 'CAB100 - FIRST RECORD IS NOT A HEADER'
               MOVE 'F002' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       1100-READ-PARMCARD.
      *----------------------------------------------------------------*
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               MOVE '1100-READ-PARMCARD' TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-PARMCARD-STATUS  TO AB-FILE-STATUS
               MOVE 'SYSIN'             TO AB-DDNAME
               MOVE 'OPEN FAILED ON CONTROL CARD' TO AB-MESSAGE
               GO TO 9999-ABEND.
       1100-READ-NEXT.
           READ PARMCARD INTO WS-PARM-CARD
               AT END
                   MOVE 'Y' TO WS-PARM-EOF-SW.
           IF WS-PARM-EOF
               GO TO 1100-CLOSE.
           IF WS-PARMCARD-STATUS NOT = '00'
               MOVE '1100-READ-PARMCARD' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-PARMCARD-STATUS  TO AB-FILE-STATUS
               MOVE 'SYSIN'             TO AB-DDNAME
               MOVE 'READ FAILED ON CONTROL CARD' TO AB-MESSAGE
               GO TO 9999-ABEND.
           IF PARMCARD-REC(1:1) = '*'
               GO TO 1100-READ-NEXT.
           IF PC-PROGRAM NOT = 'CAB100'
               DISPLAY 'CAB100 - CONTROL CARD IGNORED: ' PARMCARD-REC
               GO TO 1100-READ-NEXT.
           IF PC-VENDOR-KW = 'VENDOR='
               MOVE PC-VENDOR TO WS-PARM-VENDOR.
           IF PC-MAXREJ-KW = 'MAXREJ='
           AND PC-MAXREJ IS NUMERIC
               MOVE PC-MAXREJ TO WS-PARM-MAXREJ.
           IF PC-PASTREC-KW = 'PASTREC='
               MOVE PC-PASTREC TO WS-PARM-PASTREC.
           GO TO 1100-READ-NEXT.
       1100-CLOSE.
           CLOSE PARMCARD.
           DISPLAY 'CAB100 - CONTROL CARD VENDOR=' WS-PARM-VENDOR
                   ' MAXREJ=' PC-MAXREJ
                   ' PASTREC=' WS-PARM-PASTREC.
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * LOAD VENDOR REFERENCE TABLE AND FIND THE HIGHEST EVENT         *
      * SEQUENCE ALREADY ASSIGNED FOR TODAY                            *
      *----------------------------------------------------------------*
       1200-LOAD-VREF-TABLE.
           MOVE LOW-VALUES TO CAE-EVENT-ID.
           START CAEVENT-FILE KEY IS NOT LESS THAN CAE-EVENT-ID.
           IF CAEVENT-NOTFND
               DISPLAY 'CAB100 - EVENT MASTER IS EMPTY'
               GO TO 1200-EXIT.
           IF NOT CAEVENT-OK
               MOVE '1200-LOAD-VREF-TABLE' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'START FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
       1200-READ-LOOP.
           READ CAEVENT-FILE NEXT RECORD.
           IF CAEVENT-EOF
               GO TO 1200-DONE.
           IF NOT CAEVENT-OK
               MOVE '1200-LOAD-VREF-TABLE' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ NEXT FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-TBL-LOADED.
           MOVE CAE-EVENT-ID TO WS-SCAN-ID.
           IF WS-SCAN-PREFIX = WS-EVENT-ID-X(1:10)
           AND WS-SCAN-SEQ IS NUMERIC
               IF WS-SCAN-SEQ > WS-HIGH-SEQ
                   MOVE WS-SCAN-SEQ TO WS-HIGH-SEQ.
           IF CAE-VENDOR-REF = SPACES
               GO TO 1200-READ-LOOP.
           IF WS-VREF-COUNT NOT < WS-VREF-MAX
               MOVE '1200-LOAD-VREF-TABLE' TO AB-PARAGRAPH
               MOVE 1007                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'VENDOR REF TABLE FULL - INCREASE WS-VREF-MAX'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-VREF-COUNT.
           SET VX-IDX TO WS-VREF-COUNT.
           MOVE CAE-VENDOR-REF TO WS-VREF-REF (VX-IDX).
           MOVE CAE-EVENT-ID   TO WS-VREF-EVENT-ID (VX-IDX).
           MOVE CAE-STATUS     TO WS-VREF-STATUS (VX-IDX).
           GO TO 1200-READ-LOOP.
       1200-DONE.
           MOVE WS-HIGH-SEQ TO WS-EID-SEQ.
           MOVE WS-TBL-LOADED TO WS-DISP-COUNT.
           DISPLAY 'CAB100 - EVENTS ON MASTER    : ' WS-DISP-COUNT.
           DISPLAY 'CAB100 - LAST EVENT ID TODAY : ' WS-EVENT-ID-X.
       1200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       1300-CHECK-HEADER.
      *----------------------------------------------------------------*
           MOVE 'Y' TO WS-HEADER-SW.
           DISPLAY 'CAB100 - FEED HEADER VENDOR=' CAN-HDR-VENDOR
                   ' FILE DATE=' CAN-HDR-FILE-DATE.
           IF WS-PARM-VENDOR NOT = SPACES
           AND CAN-HDR-VENDOR NOT = WS-PARM-VENDOR
               MOVE '1300-CHECK-HEADER' TO AB-PARAGRAPH
               MOVE 1008                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'ANNFEED'           TO AB-DDNAME
               MOVE CAN-HDR-VENDOR      TO AB-KEY
               MOVE 'FEED VENDOR DOES NOT MATCH CONTROL CARD'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           IF CAN-HDR-FILE-DATE NOT = DC-BUS-DATE
           AND CAN-HDR-FILE-DATE NOT = DC-PREV-BUS-DATE
               DISPLAY 'CAB100 - FEED FILE DATE NOT CURRENT'
               MOVE 'F004' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT.
       1300-EXIT.
           EXIT.
      *
      *================================================================*
       2000-PROCESS-FEED.
      *================================================================*
           IF WS-TRAILER-FOUND
               ADD 1 TO WS-AFTER-TRAILER
               GO TO 2000-READ.
      *
           IF CAN-TRAILER
               MOVE 'Y' TO WS-TRAILER-SW
               PERFORM 3100-CHECK-TRAILER THRU 3100-EXIT
               GO TO 2000-READ.
      *
           IF CAN-HEADER
               DISPLAY 'CAB100 - DUPLICATE HEADER RECORD IGNORED'
               GO TO 2000-READ.
      *
           IF NOT CAN-EVENT
               DISPLAY 'CAB100 - UNKNOWN RECORD TYPE ' CAN-REC-TYPE
                       ' AT RECORD ' WS-RECS-READ
               MOVE 'F006' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT
               GO TO 2000-READ.
      *
           ADD 1 TO WS-EVENTS-READ.
           ADD CAN-RATE TO WS-RATE-HASH.
           PERFORM 2050-RESET-RECORD THRU 2050-EXIT.
      *
           PERFORM 2100-VALIDATE-COMMON THRU 2100-EXIT.
      *
           IF NOT WS-REC-REJECTED
               IF CAN-ACT-CANCEL
                   PERFORM 2600-APPLY-CANCEL THRU 2600-EXIT
               ELSE
                   PERFORM 2200-VALIDATE-DATES THRU 2200-EXIT
                   PERFORM 2300-VALIDATE-TERMS THRU 2300-EXIT
                   IF NOT WS-REC-REJECTED
                       IF CAN-ACT-NEW
                           PERFORM 2400-APPLY-NEW THRU 2400-EXIT
                       ELSE
                           PERFORM 2500-APPLY-UPDATE THRU 2500-EXIT.
      *
           PERFORM 2800-WRITE-RESULT THRU 2800-EXIT.
       2000-READ.
           PERFORM 8000-READ-FEED THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2050-RESET-RECORD.
      *----------------------------------------------------------------*
           MOVE 'N'    TO WS-REJECT-SW.
           MOVE 'N'    TO WS-WARNING-SW.
           MOVE 'N'    TO WS-FOUND-SW.
           MOVE ZERO   TO WS-MSG-COUNT.
           MOVE SPACES TO WS-MSG-CODE (1)
                          WS-MSG-CODE (2)
                          WS-MSG-CODE (3).
           MOVE SPACES TO WS-OLD-STATUS
                          WS-NEW-STATUS.
           MOVE SPACES TO SEC-MASTER-REC.
           MOVE SPACES TO VAL-VALIDATION-REC.
           MOVE CAN-VENDOR-REF TO VAL-VENDOR-REF.
       2050-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * ACTION, REFERENCE, EVENT TYPE, SECURITY                        *
      *----------------------------------------------------------------*
       2100-VALIDATE-COMMON.
           IF NOT CAN-ACT-NEW
           AND NOT CAN-ACT-UPDATE
           AND NOT CAN-ACT-CANCEL
               MOVE 'V001' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2100-EXIT.
      *
           IF CAN-VENDOR-REF = SPACES
               MOVE 'V002' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2100-EXIT.
      *
           PERFORM 2150-FIND-VREF THRU 2150-EXIT.
      *
           IF CAN-ACT-NEW
               IF WS-EVENT-IN-TABLE
                   MOVE 'V014' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   GO TO 2100-EXIT.
           IF CAN-ACT-UPDATE OR CAN-ACT-CANCEL
               IF NOT WS-EVENT-IN-TABLE
                   MOVE 'V015' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   GO TO 2100-EXIT.
      *
      *    CANCELS NEED ONLY THE VENDOR REFERENCE
           IF CAN-ACT-CANCEL
               GO TO 2100-EXIT.
      *
           IF CAN-EVENT-TYPE NOT = 'CDV' AND NOT = 'SDV'
                         AND NOT = 'SPL' AND NOT = 'RSP'
                         AND NOT = 'MRG'
               MOVE 'V005' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
           MOVE 'GET '         TO SL-FUNCTION.
           MOVE CAN-CUSIP      TO SL-KEY-CUSIP.
           MOVE SPACES         TO SL-KEY-ISIN
                                  SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           IF SL-DB-ERROR
               MOVE '2100-VALIDATE-COMMON' TO AB-PARAGRAPH
               MOVE 1003                TO AB-ABEND-CODE
               MOVE SL-SQLCODE          TO AB-SQLCODE
               MOVE 'CMD010'            TO AB-DDNAME
               MOVE CAN-CUSIP           TO AB-KEY
               MOVE 'DB2 ERROR READING SECURITY MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           IF SL-NOT-FOUND
               MOVE 'V003' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2100-EXIT.
           MOVE SL-SEC-DATA TO SEC-MASTER-REC.
           IF SEC-HALTED
               MOVE 'W001' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
           ELSE
               IF NOT SEC-ACTIVE
                   MOVE 'V004' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
       2100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       2150-FIND-VREF.
      *----------------------------------------------------------------*
           MOVE 'N' TO WS-FOUND-SW.
           MOVE ZERO TO WS-VREF-HIT.
           IF WS-VREF-COUNT = ZERO
               GO TO 2150-EXIT.
           PERFORM 2160-SCAN-VREF THRU 2160-EXIT
               VARYING VX-IDX FROM 1 BY 1
               UNTIL VX-IDX > WS-VREF-COUNT
                  OR WS-EVENT-IN-TABLE.
       2150-EXIT.
           EXIT.
      *
       2160-SCAN-VREF.
           IF WS-VREF-REF (VX-IDX) = CAN-VENDOR-REF
               MOVE 'Y' TO WS-FOUND-SW
               SET WS-VREF-HIT TO VX-IDX.
       2160-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * DATE EDITS: VALID DATES, EX <= RECORD <= PAY, RECORD DATE IS   *
      * A BUSINESS DAY.                                                *
      *----------------------------------------------------------------*
       2200-VALIDATE-DATES.
           MOVE CAN-ANN-DATE TO WS-DATE-TO-CHECK.
           PERFORM 2250-CHECK-DATE THRU 2250-EXIT.
           IF NOT WS-DATE-IS-VALID
               MOVE 'V006' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2200-EXIT.
           MOVE CAN-EX-DATE TO WS-DATE-TO-CHECK.
           PERFORM 2250-CHECK-DATE THRU 2250-EXIT.
           IF NOT WS-DATE-IS-VALID
               MOVE 'V006' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2200-EXIT.
           MOVE CAN-RECORD-DATE TO WS-DATE-TO-CHECK.
           PERFORM 2250-CHECK-DATE THRU 2250-EXIT.
           IF NOT WS-DATE-IS-VALID
               MOVE 'V006' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2200-EXIT.
           MOVE CAN-PAY-DATE TO WS-DATE-TO-CHECK.
           PERFORM 2250-CHECK-DATE THRU 2250-EXIT.
           IF NOT WS-DATE-IS-VALID
               MOVE 'V006' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2200-EXIT.
      *
      *    DATE SEQUENCE.  BEFORE T+1 THE EX DATE PRECEDES THE RECORD
      *    DATE.  FROM 2024-05-28 EX DATE = RECORD DATE IS NORMAL.
           IF CAN-EX-DATE > CAN-RECORD-DATE
           OR CAN-RECORD-DATE > CAN-PAY-DATE
               MOVE 'V007' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
           ELSE
               IF CAN-RECORD-DATE < WS-TPLUS1-DATE
               AND CAN-EX-DATE = CAN-RECORD-DATE
                   MOVE 'V007' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
           IF CAN-ANN-DATE > CAN-EX-DATE
               MOVE 'W003' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
           MOVE 'BUSD'          TO DT-FUNCTION.
           MOVE 'NYSE'          TO DT-CALENDAR.
           MOVE CAN-RECORD-DATE TO DT-DATE-1.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 'V006' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
           ELSE
               IF DT-RESULT-NO
                   MOVE 'V008' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
           MOVE 'BUSD'          TO DT-FUNCTION.
           MOVE 'NYSE'          TO DT-CALENDAR.
           MOVE CAN-PAY-DATE    TO DT-DATE-1.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK AND DT-RESULT-NO
               MOVE 'W002' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
      *    RECORD DATE ALREADY PASSED - ELIGIBILITY WOULD BE MISSED
           IF CAN-RECORD-DATE < DC-BUS-DATE
               IF CAN-ACT-NEW AND NOT WS-ALLOW-PAST-RECORD
                   MOVE 'V018' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               ELSE
                   MOVE 'W005' TO WS-NEW-MSG
                   PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
       2200-EXIT.
           EXIT.
      *
       2250-CHECK-DATE.
           MOVE 'N' TO WS-DATE-VALID-SW.
           IF WS-DATE-TO-CHECK NOT NUMERIC
           OR WS-DATE-TO-CHECK = ZERO
               GO TO 2250-EXIT.
           MOVE 'VALD'           TO DT-FUNCTION.
           MOVE 'NYSE'           TO DT-CALENDAR.
           MOVE WS-DATE-TO-CHECK TO DT-DATE-1.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE 'Y' TO WS-DATE-VALID-SW.
       2250-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * TERMS BY EVENT TYPE                                            *
      *----------------------------------------------------------------*
       2300-VALIDATE-TERMS.
           MOVE 'N' TO WS-CCY-FOUND-SW.
           PERFORM VARYING WS-CCY-IDX FROM 1 BY 1
                   UNTIL WS-CCY-IDX > 6 OR WS-CCY-FOUND
               IF WS-VALID-CCY (WS-CCY-IDX) = CAN-CCY
                   MOVE 'Y' TO WS-CCY-FOUND-SW
               END-IF
           END-PERFORM.
           IF NOT WS-CCY-FOUND
               MOVE 'V013' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
           IF CAN-RATE NOT NUMERIC
           OR CAN-RATIO-NEW NOT NUMERIC
           OR CAN-RATIO-OLD NOT NUMERIC
           OR CAN-CIL-PRICE NOT NUMERIC
               MOVE 'V009' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2300-EXIT.
      *
           EVALUATE CAN-EVENT-TYPE
               WHEN 'CDV'
                   IF CAN-RATE NOT > ZERO
                       MOVE 'V009' TO WS-NEW-MSG
                       PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   END-IF
                   IF CAN-TAXABLE NOT = 'Y' AND NOT = 'N'
                                  AND NOT = SPACE
                       MOVE 'W004' TO WS-NEW-MSG
                       PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   END-IF
               WHEN 'SDV'
                   IF CAN-RATE NOT > ZERO
                       IF CAN-RATIO-NEW NOT > ZERO
                       OR CAN-RATIO-OLD NOT > ZERO
                           MOVE 'V010' TO WS-NEW-MSG
                           PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                       END-IF
                   END-IF
                   PERFORM 2350-EDIT-FRACTIONS THRU 2350-EXIT
               WHEN 'SPL'
                   IF CAN-RATIO-OLD NOT > ZERO
                   OR CAN-RATIO-NEW NOT > CAN-RATIO-OLD
                       MOVE 'V010' TO WS-NEW-MSG
                       PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   END-IF
                   PERFORM 2350-EDIT-FRACTIONS THRU 2350-EXIT
               WHEN 'RSP'
                   IF CAN-RATIO-NEW NOT > ZERO
                   OR CAN-RATIO-NEW NOT < CAN-RATIO-OLD
                       MOVE 'V010' TO WS-NEW-MSG
                       PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   END-IF
      *            REVERSE SPLIT FRACTIONS ALWAYS PAID AS CASH
                   IF CAN-CIL-PRICE NOT > ZERO
                       MOVE 'V012' TO WS-NEW-MSG
                       PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   END-IF
               WHEN 'MRG'
                   IF CAN-RATE NOT > ZERO
                       MOVE 'V009' TO WS-NEW-MSG
                       PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       2300-EXIT.
           EXIT.
      *
       2350-EDIT-FRACTIONS.
           IF CAN-FRAC-METHOD NOT = 'C' AND NOT = 'D'
                              AND NOT = 'U'
               MOVE 'V011' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2350-EXIT.
           IF CAN-FRAC-METHOD = 'C'
           AND CAN-CIL-PRICE NOT > ZERO
               MOVE 'V012' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
       2350-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * NEW EVENT - ASSIGN ID AND WRITE                                *
      *----------------------------------------------------------------*
       2400-APPLY-NEW.
           MOVE SPACES TO CAE-EVENT-REC.
           PERFORM 2700-BUILD-EVENT THRU 2700-EXIT.
           MOVE 'AN'        TO CAE-STATUS.
           MOVE ZERO        TO CAE-ELIG-COUNT
                               CAE-ELIG-QTY
                               CAE-ENTL-CASH-TOTAL
                               CAE-ENTL-SHARE-TOTAL.
           MOVE 'N'  TO WS-WRITE-DONE-SW.
           MOVE ZERO TO WS-RETRY-COUNT.
       2400-NEXT-ID.
           IF WS-EID-SEQ = 99
               MOVE '2400-APPLY-NEW'    TO AB-PARAGRAPH
               MOVE 1007                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE WS-EVENT-ID-X       TO AB-KEY
               MOVE 'MORE THAN 99 NEW EVENTS FOR BUSINESS DATE'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EID-SEQ.
           MOVE WS-EVENT-ID-X TO CAE-EVENT-ID.
           WRITE CAE-EVENT-REC.
           IF CAEVENT-OK
               MOVE 'Y' TO WS-WRITE-DONE-SW
               GO TO 2400-WRITTEN.
           IF CAEVENT-DUPKEY
               ADD 1 TO WS-DUPKEY-RETRIES
               ADD 1 TO WS-RETRY-COUNT
               DISPLAY 'CAB100 - EVENT ID ' WS-EVENT-ID-X
                       ' ALREADY ON FILE - NEXT SEQUENCE'
               GO TO 2400-NEXT-ID.
           MOVE '2400-APPLY-NEW'    TO AB-PARAGRAPH.
           MOVE 1002                TO AB-ABEND-CODE.
           MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS.
           MOVE 'CAEVENT'           TO AB-DDNAME.
           MOVE CAE-EVENT-ID        TO AB-KEY.
           MOVE 'WRITE FAILED ON EVENT MASTER' TO AB-MESSAGE.
           GO TO 9999-ABEND.
       2400-WRITTEN.
           ADD 1 TO WS-EVENTS-ADDED.
           MOVE SPACES       TO WS-OLD-STATUS.
           MOVE CAE-STATUS   TO WS-NEW-STATUS.
           MOVE CAE-EVENT-ID TO VAL-EVENT-ID.
           IF WS-VREF-COUNT < WS-VREF-MAX
               ADD 1 TO WS-VREF-COUNT
               SET VX-IDX TO WS-VREF-COUNT
               MOVE CAN-VENDOR-REF TO WS-VREF-REF (VX-IDX)
               MOVE CAE-EVENT-ID   TO WS-VREF-EVENT-ID (VX-IDX)
               MOVE CAE-STATUS     TO WS-VREF-STATUS (VX-IDX).
       2400-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * UPDATE - ONLY WHILE THE EVENT IS STILL ANNOUNCED               *
      *----------------------------------------------------------------*
       2500-APPLY-UPDATE.
           SET VX-IDX TO WS-VREF-HIT.
           MOVE WS-VREF-EVENT-ID (VX-IDX) TO CAE-EVENT-ID.
           MOVE CAE-EVENT-ID TO VAL-EVENT-ID.
           READ CAEVENT-FILE.
           IF CAEVENT-NOTFND
               MOVE 'V015' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2500-EXIT.
           IF NOT CAEVENT-OK
               MOVE '2500-APPLY-UPDATE' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           MOVE CAE-STATUS TO WS-OLD-STATUS.
           IF CAE-CANCELLED
               MOVE 'V017' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2500-EXIT.
           IF NOT CAE-ANNOUNCED
               MOVE 'V016' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2500-EXIT.
           PERFORM 2700-BUILD-EVENT THRU 2700-EXIT.
           REWRITE CAE-EVENT-REC.
           IF NOT CAEVENT-OK
               MOVE '2500-APPLY-UPDATE' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'REWRITE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-UPDATED.
           MOVE CAE-STATUS TO WS-NEW-STATUS.
       2500-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * CANCEL - ALLOWED WHILE ANNOUNCED OR ELIGIBILITY TAKEN          *
      *----------------------------------------------------------------*
       2600-APPLY-CANCEL.
           SET VX-IDX TO WS-VREF-HIT.
           MOVE WS-VREF-EVENT-ID (VX-IDX) TO CAE-EVENT-ID.
           MOVE CAE-EVENT-ID TO VAL-EVENT-ID.
           READ CAEVENT-FILE.
           IF WS-CAEVENT-STATUS(1:1) = '2'
               MOVE 'V015' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2600-EXIT.
           IF NOT CAEVENT-OK
               MOVE '2600-APPLY-CANCEL' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           MOVE CAE-CUSIP      TO VAL-CUSIP.
           MOVE CAE-EVENT-TYPE TO VAL-EVENT-TYPE.
           MOVE CAE-STATUS     TO WS-OLD-STATUS.
           IF CAE-CANCELLED
               MOVE 'V017' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2600-EXIT.
      *    ENTITLED OR PAID EVENTS MUST BE REVERSED BY CA OPERATIONS
           IF CAE-ENTITLED OR CAE-PAID
               MOVE 'V016' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT
               GO TO 2600-EXIT.
           MOVE 'CX'            TO CAE-STATUS.
           MOVE DC-BUS-DATE     TO CAE-LAST-UPD-DATE.
           MOVE JI-JOBNAME      TO CAE-LAST-UPD-JOB.
           REWRITE CAE-EVENT-REC.
           IF NOT CAEVENT-OK
               MOVE '2600-APPLY-CANCEL' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'REWRITE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-CANCELLED.
           MOVE CAE-STATUS TO WS-NEW-STATUS.
           MOVE CAE-STATUS TO WS-VREF-STATUS (VX-IDX).
           IF WS-OLD-STATUS = 'EL'
               MOVE 'W006' TO WS-NEW-MSG
               PERFORM 2900-ADD-MESSAGE THRU 2900-EXIT.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE 'CXLEVENT'       TO AU-EVENT.
           MOVE 'W'              TO AU-SEVERITY.
           MOVE CAE-EVENT-ID     TO AU-KEY.
           STRING 'EVENT CANCELLED BY VENDOR REF '
                  CAN-VENDOR-REF DELIMITED BY SIZE
                  INTO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       2600-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * BUILD EVENT MASTER FIELDS FROM THE FEED RECORD.  THE TERMS     *
      * AREA IS LAID OUT ACCORDING TO THE EVENT TYPE.                  *
      *----------------------------------------------------------------*
       2700-BUILD-EVENT.
           MOVE CAN-CUSIP        TO CAE-CUSIP.
           MOVE CAN-EVENT-TYPE   TO CAE-EVENT-TYPE.
           MOVE CAN-ANN-DATE     TO CAE-ANN-DATE.
           MOVE CAN-EX-DATE      TO CAE-EX-DATE.
           MOVE CAN-RECORD-DATE  TO CAE-RECORD-DATE.
           MOVE CAN-PAY-DATE     TO CAE-PAY-DATE.
           MOVE CAN-CCY          TO CAE-CCY.
           MOVE CAN-VENDOR-REF   TO CAE-VENDOR-REF.
           MOVE CAN-DESC         TO CAE-DESC.
           MOVE DC-BUS-DATE      TO CAE-LAST-UPD-DATE.
           MOVE JI-JOBNAME       TO CAE-LAST-UPD-JOB.
           IF CAN-TAXABLE = 'N'
               MOVE 'N' TO CAE-TAXABLE-FLAG
           ELSE
               MOVE 'Y' TO CAE-TAXABLE-FLAG.
           MOVE CAN-FRAC-METHOD  TO CAE-FRAC-METHOD.
           MOVE SPACES           TO CAE-TERMS.
      *
           IF CAE-CASH-DIV
               MOVE CAN-RATE     TO CAE-RATE
               MOVE 'N'          TO CAE-GROSS-UP-FLAG
               MOVE SPACE        TO CAE-FRAC-METHOD
               GO TO 2700-EXIT.
      *
           IF CAE-CASH-MERGER
               MOVE CAN-RATE     TO CAE-MRG-CASH-RATE
               MOVE CAN-NEW-CUSIP TO CAE-MRG-ACQUIRER
               MOVE CAN-PAY-DATE TO CAE-MRG-EFF-DATE
               MOVE 'N'          TO CAE-TAXABLE-FLAG
               MOVE SPACE        TO CAE-FRAC-METHOD
               GO TO 2700-EXIT.
      *
      *    STOCK EVENTS.  A STOCK DIVIDEND ANNOUNCED AS A RATE
      *    (E.G. 0.05 = 5 PCT) IS CARRIED AS RATIO RATE : 1.
           IF CAE-STOCK-DIV AND CAN-RATE > ZERO
               MOVE CAN-RATE     TO CAE-RATIO-NEW
               MOVE 1            TO CAE-RATIO-OLD
           ELSE
               MOVE CAN-RATIO-NEW TO CAE-RATIO-NEW
               MOVE CAN-RATIO-OLD TO CAE-RATIO-OLD.
           MOVE CAN-CIL-PRICE    TO CAE-CIL-PRICE.
           IF CAN-NEW-CUSIP = SPACES
               MOVE CAN-CUSIP     TO CAE-NEW-CUSIP
           ELSE
               MOVE CAN-NEW-CUSIP TO CAE-NEW-CUSIP.
           IF CAE-REV-SPLIT
               MOVE 'C'          TO CAE-FRAC-METHOD.
       2700-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * WRITE THE VALIDATION RESULT RECORD                             *
      *----------------------------------------------------------------*
       2800-WRITE-RESULT.
           IF WS-REC-REJECTED
               MOVE 'R' TO VAL-RESULT
               ADD 1 TO WS-EVENTS-REJECTED
           ELSE
               IF WS-REC-WARNING
                   MOVE 'W' TO VAL-RESULT
                   ADD 1 TO WS-EVENTS-WARNED
               ELSE
                   MOVE 'A' TO VAL-RESULT.
           MOVE CAN-VENDOR-REF   TO VAL-VENDOR-REF.
           MOVE WS-RECS-READ     TO VAL-SEQ-NO.
           MOVE CAN-ACTION       TO VAL-ACTION.
           IF CAN-ACT-CANCEL AND VAL-CUSIP NOT = SPACES
               NEXT SENTENCE
           ELSE
               MOVE CAN-CUSIP      TO VAL-CUSIP
               MOVE CAN-EVENT-TYPE TO VAL-EVENT-TYPE.
           IF CAN-EX-DATE IS NUMERIC
               MOVE CAN-EX-DATE     TO VAL-EX-DATE
           ELSE
               MOVE ZERO            TO VAL-EX-DATE.
           IF CAN-RECORD-DATE IS NUMERIC
               MOVE CAN-RECORD-DATE TO VAL-RECORD-DATE
           ELSE
               MOVE ZERO            TO VAL-RECORD-DATE.
           IF CAN-PAY-DATE IS NUMERIC
               MOVE CAN-PAY-DATE    TO VAL-PAY-DATE
           ELSE
               MOVE ZERO            TO VAL-PAY-DATE.
           IF CAN-RATE IS NUMERIC
               MOVE CAN-RATE        TO VAL-RATE
           ELSE
               MOVE ZERO            TO VAL-RATE.
           IF CAN-RATIO-NEW IS NUMERIC
           AND CAN-RATIO-OLD IS NUMERIC
               MOVE CAN-RATIO-NEW   TO VAL-RATIO-NEW
               MOVE CAN-RATIO-OLD   TO VAL-RATIO-OLD
           ELSE
               MOVE ZERO            TO VAL-RATIO-NEW
                                       VAL-RATIO-OLD.
           MOVE WS-OLD-STATUS    TO VAL-OLD-STATUS.
           MOVE WS-NEW-STATUS    TO VAL-NEW-STATUS.
           MOVE SEC-DESC         TO VAL-SEC-DESC.
           MOVE WS-MSG-COUNT     TO VAL-MSG-COUNT.
           MOVE WS-MSG-CODE (1)  TO VAL-MSG-CODE (1).
           MOVE WS-MSG-CODE (2)  TO VAL-MSG-CODE (2).
           MOVE WS-MSG-CODE (3)  TO VAL-MSG-CODE (3).
           MOVE DC-BUS-DATE      TO VAL-BUS-DATE.
           PERFORM 8100-WRITE-VALIDATION THRU 8100-EXIT.
       2800-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * ADD A MESSAGE CODE (MAX 3 KEPT).  V = REJECT, W = WARNING.     *
      *----------------------------------------------------------------*
       2900-ADD-MESSAGE.
           IF WS-NEW-MSG(1:1) = 'V'
               MOVE 'Y' TO WS-REJECT-SW
           ELSE
               MOVE 'Y' TO WS-WARNING-SW.
           IF WS-MSG-COUNT < 3
               ADD 1 TO WS-MSG-COUNT
               MOVE WS-NEW-MSG TO WS-MSG-CODE (WS-MSG-COUNT).
       2900-EXIT.
           EXIT.
      *
      *================================================================*
       3000-CHECK-FILE-LEVEL.
      *================================================================*
           IF NOT WS-TRAILER-FOUND
           AND WS-RECS-READ > ZERO
               DISPLAY 'CAB100 - TRAILER RECORD MISSING'
               MOVE 'F003' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT.
           IF WS-AFTER-TRAILER > ZERO
               MOVE WS-AFTER-TRAILER TO WS-DISP-COUNT
               DISPLAY 'CAB100 - RECORDS AFTER TRAILER IGNORED: '
                       WS-DISP-COUNT
               MOVE 'F007' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3100-CHECK-TRAILER.
      *----------------------------------------------------------------*
           IF CAN-TRL-COUNT NOT NUMERIC
           OR CAN-TRL-COUNT NOT = WS-EVENTS-READ
               MOVE WS-EVENTS-READ TO WS-DISP-COUNT
               DISPLAY 'CAB100 - TRAILER COUNT ' CAN-TRL-COUNT
                       ' DOES NOT MATCH EVENTS READ ' WS-DISP-COUNT
               MOVE 'F001' TO WS-NEW-MSG
               PERFORM 3900-WRITE-FILE-RESULT THRU 3900-EXIT.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * FILE LEVEL RESULT - FORCES RETURN CODE 8                       *
      *----------------------------------------------------------------*
       3900-WRITE-FILE-RESULT.
           MOVE 'Y' TO WS-FILE-ERROR-SW.
           IF WS-NEW-MSG = 'F004'
               NEXT SENTENCE
           ELSE
               IF WS-RETURN-CODE < 8
                   MOVE 8 TO WS-RETURN-CODE.
           MOVE SPACES           TO VAL-VALIDATION-REC.
           MOVE 'F'              TO VAL-RESULT.
           MOVE WS-NEW-MSG       TO VAL-VENDOR-REF.
           MOVE WS-RECS-READ     TO VAL-SEQ-NO.
           MOVE ZERO             TO VAL-EX-DATE
                                    VAL-RECORD-DATE
                                    VAL-PAY-DATE
                                    VAL-RATE
                                    VAL-RATIO-NEW
                                    VAL-RATIO-OLD.
           MOVE 1                TO VAL-MSG-COUNT.
           MOVE WS-NEW-MSG       TO VAL-MSG-CODE (1).
           MOVE DC-BUS-DATE      TO VAL-BUS-DATE.
           PERFORM 8100-WRITE-VALIDATION THRU 8100-EXIT.
       3900-EXIT.
           EXIT.
      *
      *================================================================*
      * I/O ROUTINES                                                   *
      *================================================================*
       8000-READ-FEED.
           READ ANNFEED-FILE
               AT END
                   MOVE 'Y' TO WS-FEED-EOF-SW
                   GO TO 8000-EXIT.
           IF NOT ANNFEED-OK
               MOVE '8000-READ-FEED'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ANNFEED-STATUS   TO AB-FILE-STATUS
               MOVE 'ANNFEED'           TO AB-DDNAME
               MOVE 'READ FAILED ON ANNOUNCEMENT FEED' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-RECS-READ.
       8000-EXIT.
           EXIT.
      *
       8100-WRITE-VALIDATION.
           WRITE CAVALOUT-REC FROM VAL-VALIDATION-REC.
           IF WS-CAVALOUT-STATUS NOT = '00'
               MOVE '8100-WRITE-VALIDATION' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAVALOUT-STATUS  TO AB-FILE-STATUS
               MOVE 'CAVALOUT'          TO AB-DDNAME
               MOVE VAL-VENDOR-REF      TO AB-KEY
               MOVE 'WRITE FAILED ON VALIDATION FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-VAL-WRITTEN.
       8100-EXIT.
           EXIT.
      *
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE ANNFEED-FILE.
           IF WS-ANNFEED-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ANNFEED-STATUS   TO AB-FILE-STATUS
               MOVE 'ANNFEED'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ANNOUNCEMENT FEED' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE CAVALOUT-FILE.
           IF WS-CAVALOUT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAVALOUT-STATUS  TO AB-FILE-STATUS
               MOVE 'CAVALOUT'          TO AB-DDNAME
               MOVE 'CLOSE FAILED ON VALIDATION FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
      *    RETURN CODE
           IF WS-EVENTS-REJECTED > ZERO
           OR WS-EVENTS-WARNED > ZERO
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE.
           IF WS-EVENTS-REJECTED > WS-PARM-MAXREJ
               DISPLAY 'CAB100 - REJECTS EXCEED MAXREJ LIMIT'
               MOVE 8 TO WS-RETURN-CODE.
      *
      *    CONTROL TOTALS
           MOVE 'EVENTS-IN'        TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-READ     TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT.
           MOVE WS-RATE-HASH       TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'EVENTS-ADDED'     TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-ADDED    TO CT-COUNT.
           MOVE ZERO               TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'EVENTS-UPDATED'   TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-UPDATED  TO CT-COUNT.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'EVENTS-CANCELLED' TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-CANCELLED TO CT-COUNT.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'EVENTS-REJECTED'  TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-REJECTED TO CT-COUNT.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'VALID-OUT'        TO CT-COUNTER-NAME.
           MOVE WS-VAL-WRITTEN     TO CT-COUNT.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'CLOS'             TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *
      *    STATISTICS
           DISPLAY '*************************************************'.
           DISPLAY '* CAB100  CA ANNOUNCEMENT LOAD  - RUN STATISTICS *'.
           DISPLAY '*************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-RECS-READ        TO WS-DISP-COUNT.
           DISPLAY ' FEED RECORDS READ        : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-READ      TO WS-DISP-COUNT.
           DISPLAY ' EVENT RECORDS READ       : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-ADDED     TO WS-DISP-COUNT.
           DISPLAY ' EVENTS ADDED             : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-UPDATED   TO WS-DISP-COUNT.
           DISPLAY ' EVENTS UPDATED           : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-CANCELLED TO WS-DISP-COUNT.
           DISPLAY ' EVENTS CANCELLED         : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-REJECTED  TO WS-DISP-COUNT.
           DISPLAY ' EVENTS REJECTED          : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-WARNED    TO WS-DISP-COUNT.
           DISPLAY ' ACCEPTED WITH WARNING    : ' WS-DISP-COUNT.
           MOVE WS-DUPKEY-RETRIES   TO WS-DISP-COUNT.
           DISPLAY ' EVENT ID RETRIES (22)    : ' WS-DISP-COUNT.
           MOVE WS-VAL-WRITTEN      TO WS-DISP-COUNT.
           DISPLAY ' VALIDATION RECS WRITTEN  : ' WS-DISP-COUNT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID    TO AU-PROGRAM.
           MOVE 'END'            TO AU-EVENT.
           IF WS-RETURN-CODE > 0
               MOVE 'W'          TO AU-SEVERITY
           ELSE
               MOVE 'I'          TO AU-SEVERITY.
           MOVE DC-BUS-DATE      TO AU-BUS-DATE.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA ANNOUNCEMENT LOAD ENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'           TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       9000-EXIT.
           EXIT.
      *
       9100-POST-TOTAL.
           MOVE 'POST'           TO CT-FUNCTION.
           MOVE DC-BUS-DATE      TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID    TO CT-PROGRAM.
           MOVE 'CAB100'         TO CT-STAGE.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE '9100-POST-TOTAL'   TO AB-PARAGRAPH
               MOVE 1010                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'CTLTOTS'           TO AB-DDNAME
               MOVE CT-COUNTER-NAME     TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND.
       9100-EXIT.
           EXIT.
      *
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'CAB100 - ABENDING: ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
