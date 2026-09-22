      *================================================================*
      * PROGRAM    : CAB200                                            *
      * DESCRIPTION: CORPORATE ACTION RECORD DATE ELIGIBILITY.         *
      *              FOR EVERY ANNOUNCED EVENT WHOSE RECORD DATE IS    *
      *              THE BUSINESS DATE, TAKES A SNAPSHOT OF ALL HOLDER *
      *              POSITIONS IN THE EVENT SECURITY FROM THE POSITION *
      *              MASTER AND WRITES ONE ELIGIBILITY RECORD PER      *
      *              OWNER POSITION (CLIENT AND FIRM ACCOUNTS).        *
      *              STREET / LOCATION ACCOUNTS ARE NOT OWNERS - THEIR *
      *              QUANTITY IS ONLY ACCUMULATED FOR THE LOCATION     *
      *              RECONCILIATION.  EVENT STATUS AN -> EL.           *
      *----------------------------------------------------------------*
      * JOB        : MSCAD020  STEP010                                 *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              SYSIN     CONTROL CARD CAP200A                    *
      *              CAEVENT   EVENT MASTER KSDS (I-O)                 *
      *              POSMAST   POSITION MASTER KSDS (READ ONLY, SHR)   *
      *              ACCTMAST  ACCOUNT MASTER KSDS (RANDOM)            *
      * OUTPUT     : ELIGOUT   ELIGIBILITY SNAPSHOT (CAELIG)           *
      *                        MSEC.PROD.CA.ELIG.RAW(+1) - SORTED BY   *
      *                        STEP020 INTO MSEC.PROD.CA.ELIG(+1)      *
      * CALLS      : CMU050 CMU060 CMU080 CMASM01                      *
      *----------------------------------------------------------------*
      * NOTE: THE POSITION MASTER KEY IS ACCOUNT + CUSIP + LOCATION.   *
      *       THERE IS NO CUSIP PATH, SO THE WHOLE FILE IS PASSED ONCE *
      *       AND EVERY POSITION IS CHECKED AGAINST THE EVENT TABLE.   *
      *       MSCAD020 MUST RUN BEFORE POSITION POSTING (MSSRD020).    *
      *----------------------------------------------------------------*
      * DUE BILLS: A HOLDER WHOSE TRADE-DATE QUANTITY DIFFERS FROM THE *
      *       SETTLED QUANTITY ON RECORD DATE HAS TRADES PENDING.  THE *
      *       BUYER IS ENTITLED - ELIGIBLE QTY = SD QTY + (TD - SD).   *
      *----------------------------------------------------------------*
      * RETURN CODES:                                                  *
      *   00 CLEAN                                                     *
      *   04 LOCATION RECONCILIATION BREAK, ACCOUNT NOT FOUND, OR      *
      *      UNREADABLE QUANTITY RECOVERED                             *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1993-09-07 DWB  ORIGINAL                                       *
      * 1994-01-24 DWB  SKIP STREET SIDE ACCOUNTS             CHG00288 *
      * 1996-08-05 RJK  DUE BILL QUANTITY FROM TD - SD        CHG02731 *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2001-07-16 KAP  POSITION LAYOUT CHANGE (AVG COST)     CHG08811 *
      * 2003-05-19 KAP  TAX COUNTRY FROM ACCOUNT (W-8BEN)     CHG11020 *
      * 2006-10-30 KAP  S0C7 ON CONVERTED POSITIONS - EDIT    CHG15532 *
      *                 PACKED FIELDS BEFORE USE                       *
      * 2009-03-16 SPA  LOCATION RECONCILIATION TOTALS        CHG18120 *
      * 2011-06-20 SPA  AUDIT AND CONTROL TOTALS              CHG21877 *
      * 2015-04-27 MFO  RERUN - RETAKE EL EVENTS WHEN RERUN   CHG28802 *
      * 2018-01-08 MFO  RECDATE OVERRIDE ON CONTROL CARD      CHG32291 *
      * 2024-02-12 NVR  T+1 - NO CHANGE, EX = RECORD DATE     CHG41120 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAB200.
       AUTHOR.        D W BRANDT.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  09/07/93.
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
           SELECT CAEVENT-FILE    ASSIGN TO CAEVENT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CAE-EVENT-ID
                  FILE STATUS IS WS-CAEVENT-STATUS.
      *
           SELECT POSMAST-FILE    ASSIGN TO POSMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS POS-KEY
                  FILE STATUS IS WS-POSMAST-STATUS.
      *
           SELECT ACCTMAST-FILE   ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCT-NO
                  FILE STATUS IS WS-ACCTMAST-STATUS.
      *
           SELECT ELIGOUT-FILE    ASSIGN TO ELIGOUT
                  FILE STATUS IS WS-ELIGOUT-STATUS.
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
       FD  CAEVENT-FILE.
           COPY CAEVENT.
      *
       FD  POSMAST-FILE.
           COPY SRPOSN.
      *
       FD  ACCTMAST-FILE.
           COPY CMACCT.
      *
       FD  ELIGOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY CAELIG.
      *
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
           VALUE 'CAB200 WORKING STORAGE BEGINS'.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAB200'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-CAEVENT-STATUS       PIC X(02) VALUE '00'.
               88  CAEVENT-OK                    VALUE '00'.
               88  CAEVENT-EOF                   VALUE '10'.
               88  CAEVENT-NOTFND                VALUE '23'.
           05  WS-POSMAST-STATUS       PIC X(02) VALUE '00'.
               88  POSMAST-OK                    VALUE '00'.
               88  POSMAST-EOF                   VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02) VALUE '00'.
               88  ACCTMAST-OK                   VALUE '00'.
               88  ACCTMAST-NOTFND               VALUE '23'.
           05  WS-ELIGOUT-STATUS       PIC X(02) VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-EVT-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-EVT-EOF                    VALUE 'Y'.
           05  WS-POS-EOF-SW           PIC X(01) VALUE 'N'.
               88  WS-POS-EOF                    VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01) VALUE 'N'.
               88  WS-PARM-EOF                   VALUE 'Y'.
           05  WS-ACCT-FOUND-SW        PIC X(01) VALUE 'N'.
               88  WS-ACCT-FOUND                 VALUE 'Y'.
           05  WS-CUSIP-HIT-SW         PIC X(01) VALUE 'N'.
               88  WS-CUSIP-HIT                  VALUE 'Y'.
      *
      *----------------------------------------------------------------*
      * CONTROL CARD (CAP200A)                                         *
      * CAB200 RECDATE=CCYYMMDD   (00000000 = USE BUSINESS DATE)       *
      *----------------------------------------------------------------*
       01  WS-PARM-CARD.
           05  PC-PROGRAM              PIC X(06).
           05  FILLER                  PIC X(01).
           05  PC-RECDATE-KW           PIC X(08).
           05  PC-RECDATE              PIC X(08).
           05  FILLER                  PIC X(57).
      *
       01  WS-TARGET-DATE              PIC 9(08) VALUE ZERO.
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * EVENT TABLE - EVENTS WITH RECORD DATE = TARGET DATE            *
      *----------------------------------------------------------------*
       01  WS-EVT-MAX                  PIC S9(04) COMP VALUE +50.
       01  WS-EVT-COUNT                PIC S9(04) COMP VALUE ZERO.
       01  WS-EVT-SUB                  PIC S9(04) COMP VALUE ZERO.
       01  WS-EVENT-TABLE.
           05  WS-ET-ENTRY             OCCURS 50 TIMES.
               10  WS-ET-EVENT-ID      PIC X(12).
               10  WS-ET-CUSIP         PIC X(09).
               10  WS-ET-TYPE          PIC X(03).
               10  WS-ET-OLD-STATUS    PIC X(02).
               10  WS-ET-ELIG-COUNT    PIC S9(07)       COMP-3.
               10  WS-ET-ELIG-QTY      PIC S9(13)V9(04) COMP-3.
               10  WS-ET-OWNER-SD      PIC S9(13)V9(04) COMP-3.
               10  WS-ET-LOCN-SD       PIC S9(13)V9(04) COMP-3.
               10  WS-ET-DUEBILL-CNT   PIC S9(07)       COMP-3.
               10  WS-ET-DUEBILL-QTY   PIC S9(13)V9(04) COMP-3.
               10  WS-ET-SHORT-CNT     PIC S9(07)       COMP-3.
               10  WS-ET-LOCN-CNT      PIC S9(07)       COMP-3.
      *
      *----------------------------------------------------------------*
      * POSITION WORK FIELDS                                           *
      *----------------------------------------------------------------*
       01  WS-POS-WORK.
           05  WS-SD-QTY               PIC S9(11)V9(04) COMP-3.
           05  WS-TD-QTY               PIC S9(11)V9(04) COMP-3.
           05  WS-DUE-BILL-QTY         PIC S9(11)V9(04) COMP-3.
           05  WS-ENTITLED-QTY         PIC S9(11)V9(04) COMP-3.
           05  WS-LOCN-DIFF            PIC S9(13)V9(04) COMP-3.
       01  WS-LAST-ACCT                PIC X(10) VALUE LOW-VALUES.
      *
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-EVENTS-READ          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-SELECTED      PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-UPDATED       PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-POSN-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-MATCHED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-FLAT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-SHORT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSN-BAD-QTY         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-NOT-FOUND       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCT-READS           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ELIG-WRITTEN         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DUE-BILLS            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-RECON-BREAKS         PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-ELIG-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
           05  WS-LOCN-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISP-QTY                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
       01  WS-DISP-QTY2                PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
      *
           COPY CMDATEW.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMJILNK.
      *
       01  FILLER                      PIC X(32)
           VALUE 'CAB200 WORKING STORAGE ENDS'.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
      *
           PERFORM 2000-LOAD-EVENTS THRU 2000-EXIT.
      *
           IF WS-EVT-COUNT > ZERO
               PERFORM 3000-SCAN-POSITIONS THRU 3000-EXIT
                   UNTIL WS-POS-EOF
               PERFORM 4000-UPDATE-EVENTS THRU 4000-EXIT
                   VARYING WS-EVT-SUB FROM 1 BY 1
                   UNTIL WS-EVT-SUB > WS-EVT-COUNT
           ELSE
               DISPLAY 'CAB200 - NO EVENTS WITH RECORD DATE '
                       WS-TARGET-DATE ' - EMPTY ELIGIBILITY FILE'.
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
           MOVE DC-BUS-DATE TO WS-TARGET-DATE.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID    TO AU-PROGRAM.
           MOVE 'START'          TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE DC-BUS-DATE      TO AU-BUS-DATE.
           MOVE SPACES           TO AU-KEY.
           MOVE 'CA RECORD DATE ELIGIBILITY STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           PERFORM 1100-READ-PARMCARD THRU 1100-EXIT.
      *
      *    ACCUMULATORS ARE CLEARED WHEN AN EVENT IS ADDED
           MOVE SPACES TO WS-EVENT-TABLE.
      *
           OPEN I-O CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN INPUT POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-POSMAST-STATUS   TO AB-FILE-STATUS
               MOVE 'POSMAST'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON POSITION MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ACCTMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ACCTMAST'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON ACCOUNT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN OUTPUT ELIGOUT-FILE.
           IF WS-ELIGOUT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ELIGOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ELIGOUT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON ELIGIBILITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
       1000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       1100-READ-PARMCARD.
      *----------------------------------------------------------------*
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'CAB200 - NO CONTROL CARD - DEFAULTS USED'
               GO TO 1100-EXIT.
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
           OR PC-PROGRAM NOT = 'CAB200'
               GO TO 1100-READ-NEXT.
           IF PC-RECDATE-KW = 'RECDATE='
           AND PC-RECDATE IS NUMERIC
           AND PC-RECDATE NOT = '00000000'
               MOVE PC-RECDATE TO WS-TARGET-DATE
               DISPLAY 'CAB200 - RECORD DATE OVERRIDE ' WS-TARGET-DATE
               MOVE 'WRIT'           TO AU-FUNCTION
               MOVE 'OVERRIDE'       TO AU-EVENT
               MOVE 'W'              TO AU-SEVERITY
               MOVE PC-RECDATE       TO AU-KEY
               MOVE 'RECORD DATE OVERRIDDEN BY CONTROL CARD'
                                     TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS.
           GO TO 1100-READ-NEXT.
       1100-CLOSE.
           CLOSE PARMCARD.
       1100-EXIT.
           EXIT.
      *
      *================================================================*
      * PASS 1 - EVENTS WHOSE RECORD DATE IS THE TARGET DATE           *
      *================================================================*
       2000-LOAD-EVENTS.
           MOVE LOW-VALUES TO CAE-EVENT-ID.
           START CAEVENT-FILE KEY IS NOT LESS THAN CAE-EVENT-ID.
           IF CAEVENT-NOTFND
               GO TO 2000-EXIT.
           IF NOT CAEVENT-OK
               MOVE '2000-LOAD-EVENTS'  TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'START FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
       2000-READ-LOOP.
           READ CAEVENT-FILE NEXT RECORD.
           IF CAEVENT-EOF
               GO TO 2000-EXIT.
           IF NOT CAEVENT-OK
               MOVE '2000-LOAD-EVENTS'  TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ NEXT FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-READ.
           IF CAE-RECORD-DATE NOT = WS-TARGET-DATE
               GO TO 2000-READ-LOOP.
           IF CAE-ANNOUNCED
               GO TO 2000-SELECT.
           IF CAE-ELIGIBLE-TAKEN AND DC-RERUN
               DISPLAY 'CAB200 - RERUN - RETAKING ELIGIBILITY FOR '
                       CAE-EVENT-ID
               GO TO 2000-SELECT.
           IF NOT CAE-CANCELLED
               DISPLAY 'CAB200 - EVENT ' CAE-EVENT-ID ' STATUS '
                       CAE-STATUS ' NOT ELIGIBLE FOR SNAPSHOT'
           END-IF.
           GO TO 2000-READ-LOOP.
       2000-SELECT.
           IF WS-EVT-COUNT NOT < WS-EVT-MAX
               MOVE '2000-LOAD-EVENTS'  TO AB-PARAGRAPH
               MOVE 1007                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'MORE THAN 50 EVENTS ON ONE RECORD DATE'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVT-COUNT.
           ADD 1 TO WS-EVENTS-SELECTED.
           MOVE CAE-EVENT-ID   TO WS-ET-EVENT-ID (WS-EVT-COUNT).
           MOVE CAE-CUSIP      TO WS-ET-CUSIP (WS-EVT-COUNT).
           MOVE CAE-EVENT-TYPE TO WS-ET-TYPE (WS-EVT-COUNT).
           MOVE CAE-STATUS     TO WS-ET-OLD-STATUS (WS-EVT-COUNT).
           DISPLAY 'CAB200 - EVENT SELECTED ' CAE-EVENT-ID
                   ' CUSIP ' CAE-CUSIP ' TYPE ' CAE-EVENT-TYPE.
           GO TO 2000-READ-LOOP.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * PASS 2 - FULL PASS OF THE POSITION MASTER                      *
      *================================================================*
       3000-SCAN-POSITIONS.
           READ POSMAST-FILE NEXT RECORD.
           IF POSMAST-EOF
               MOVE 'Y' TO WS-POS-EOF-SW
               GO TO 3000-EXIT.
           IF NOT POSMAST-OK
               MOVE '3000-SCAN-POSITIONS' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-POSMAST-STATUS   TO AB-FILE-STATUS
               MOVE 'POSMAST'           TO AB-DDNAME
               MOVE POS-KEY             TO AB-KEY
               MOVE 'READ FAILED ON POSITION MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-POSN-READ.
      *
           MOVE 'N' TO WS-CUSIP-HIT-SW.
           PERFORM 3100-MATCH-EVENT THRU 3100-EXIT
               VARYING WS-EVT-SUB FROM 1 BY 1
               UNTIL WS-EVT-SUB > WS-EVT-COUNT.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * ONE SECURITY MAY HAVE MORE THAN ONE EVENT ON A RECORD DATE     *
      *----------------------------------------------------------------*
       3100-MATCH-EVENT.
           IF POS-CUSIP NOT = WS-ET-CUSIP (WS-EVT-SUB)
               GO TO 3100-EXIT.
           IF NOT WS-CUSIP-HIT
               MOVE 'Y' TO WS-CUSIP-HIT-SW
               ADD 1 TO WS-POSN-MATCHED
               PERFORM 3200-EDIT-QUANTITIES THRU 3200-EXIT
               PERFORM 3300-GET-ACCOUNT THRU 3300-EXIT.
      *
           IF WS-SD-QTY = ZERO AND WS-TD-QTY = ZERO
               ADD 1 TO WS-POSN-FLAT
               GO TO 3100-EXIT.
      *
           IF NOT WS-ACCT-FOUND
               GO TO 3100-EXIT.
      *
           IF WS-ET-OWNER-SD (WS-EVT-SUB) NOT NUMERIC
               MOVE ZERO TO WS-ET-ELIG-COUNT  (WS-EVT-SUB)
                            WS-ET-ELIG-QTY    (WS-EVT-SUB)
                            WS-ET-OWNER-SD    (WS-EVT-SUB)
                            WS-ET-LOCN-SD     (WS-EVT-SUB)
                            WS-ET-DUEBILL-CNT (WS-EVT-SUB)
                            WS-ET-DUEBILL-QTY (WS-EVT-SUB)
                            WS-ET-SHORT-CNT   (WS-EVT-SUB)
                            WS-ET-LOCN-CNT    (WS-EVT-SUB).
      *
      *    STREET / LOCATION ACCOUNTS - RECONCILIATION ONLY
           IF ACCT-STREET-SIDE
               ADD 1 TO WS-POSN-STREET
               ADD 1 TO WS-ET-LOCN-CNT (WS-EVT-SUB)
               ADD WS-SD-QTY TO WS-ET-LOCN-SD (WS-EVT-SUB)
               ADD WS-SD-QTY TO WS-LOCN-QTY-HASH
               GO TO 3100-EXIT.
      *
           ADD WS-SD-QTY TO WS-ET-OWNER-SD (WS-EVT-SUB).
      *
           PERFORM 3400-COMPUTE-ELIGIBLE THRU 3400-EXIT.
           IF WS-ENTITLED-QTY NOT > ZERO
               ADD 1 TO WS-POSN-SHORT
               ADD 1 TO WS-ET-SHORT-CNT (WS-EVT-SUB)
               GO TO 3100-EXIT.
      *
           PERFORM 3500-WRITE-ELIGIBLE THRU 3500-EXIT.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * QUANTITIES ON CONVERTED AND ZERO-FILLED POSITIONS MAY NOT BE   *
      * VALID PACKED DECIMAL - SEE CHG15532                            *
      *----------------------------------------------------------------*
       3200-EDIT-QUANTITIES.
           IF POS-SD-QTY NUMERIC
               MOVE POS-SD-QTY TO WS-SD-QTY
           ELSE
               MOVE ZERO TO WS-SD-QTY
               ADD 1 TO WS-POSN-BAD-QTY
               DISPLAY 'CAB200 - SD QTY NOT NUMERIC ' POS-KEY.
           IF POS-TD-QTY NUMERIC
               MOVE POS-TD-QTY TO WS-TD-QTY
           ELSE
               MOVE WS-SD-QTY TO WS-TD-QTY
               ADD 1 TO WS-POSN-BAD-QTY
               DISPLAY 'CAB200 - TD QTY NOT NUMERIC ' POS-KEY.
       3200-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3300-GET-ACCOUNT.
      *----------------------------------------------------------------*
           IF POS-ACCT-NO = WS-LAST-ACCT
               GO TO 3300-EXIT.
           MOVE POS-ACCT-NO TO WS-LAST-ACCT.
           MOVE POS-ACCT-NO TO ACCT-NO.
           MOVE 'N' TO WS-ACCT-FOUND-SW.
           ADD 1 TO WS-ACCT-READS.
           READ ACCTMAST-FILE.
           IF ACCTMAST-OK
               MOVE 'Y' TO WS-ACCT-FOUND-SW
               GO TO 3300-EXIT.
           IF ACCTMAST-NOTFND
               ADD 1 TO WS-ACCT-NOT-FOUND
               DISPLAY 'CAB200 - ACCOUNT NOT ON MASTER ' POS-ACCT-NO
                       ' - POSITION ' POS-CUSIP ' SKIPPED'
               MOVE 'WRIT'        TO AU-FUNCTION
               MOVE 'ACCTNF'      TO AU-EVENT
               MOVE 'W'           TO AU-SEVERITY
               MOVE POS-KEY       TO AU-KEY
               MOVE 'POSITION ACCOUNT NOT ON ACCOUNT MASTER'
                                  TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
               GO TO 3300-EXIT.
           MOVE '3300-GET-ACCOUNT'  TO AB-PARAGRAPH.
           MOVE 1002                TO AB-ABEND-CODE.
           MOVE WS-ACCTMAST-STATUS  TO AB-FILE-STATUS.
           MOVE 'ACCTMAST'          TO AB-DDNAME.
           MOVE POS-ACCT-NO         TO AB-KEY.
           MOVE 'READ FAILED ON ACCOUNT MASTER' TO AB-MESSAGE.
           GO TO 9999-ABEND.
       3300-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * ELIGIBLE QUANTITY WITH DUE BILL                                *
      *----------------------------------------------------------------*
       3400-COMPUTE-ELIGIBLE.
           MOVE ZERO TO WS-DUE-BILL-QTY.
           IF WS-TD-QTY NOT = WS-SD-QTY
               COMPUTE WS-DUE-BILL-QTY = WS-TD-QTY - WS-SD-QTY.
           COMPUTE WS-ENTITLED-QTY = WS-SD-QTY + WS-DUE-BILL-QTY.
       3400-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3500-WRITE-ELIGIBLE.
      *----------------------------------------------------------------*
           MOVE SPACES                TO ELG-ELIGIBILITY-REC.
           MOVE WS-ET-EVENT-ID (WS-EVT-SUB) TO ELG-EVENT-ID.
           MOVE POS-ACCT-NO           TO ELG-ACCT-NO.
           MOVE POS-CUSIP             TO ELG-CUSIP.
           MOVE POS-LOCATION          TO ELG-LOCATION.
           MOVE WS-ET-TYPE (WS-EVT-SUB) TO ELG-EVENT-TYPE.
           MOVE ACCT-TYPE             TO ELG-ACCT-TYPE.
           MOVE ACCT-TAX-STATUS       TO ELG-TAX-STATUS.
           MOVE ACCT-TAX-COUNTRY      TO ELG-TAX-COUNTRY.
           MOVE WS-SD-QTY             TO ELG-SD-QTY.
           MOVE WS-TD-QTY             TO ELG-TD-QTY.
           MOVE WS-DUE-BILL-QTY       TO ELG-DUE-BILL-QTY.
           MOVE WS-ENTITLED-QTY       TO ELG-ENTITLED-QTY.
           MOVE WS-TARGET-DATE        TO ELG-RECORD-DATE.
           MOVE DC-BUS-DATE           TO ELG-SNAPSHOT-DATE.
      *
           WRITE ELG-ELIGIBILITY-REC.
           IF WS-ELIGOUT-STATUS NOT = '00'
               MOVE '3500-WRITE-ELIGIBLE' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ELIGOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ELIGOUT'           TO AB-DDNAME
               MOVE POS-KEY             TO AB-KEY
               MOVE 'WRITE FAILED ON ELIGIBILITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           ADD 1 TO WS-ELIG-WRITTEN.
           ADD WS-ENTITLED-QTY TO WS-ELIG-QTY-HASH.
           ADD 1 TO WS-ET-ELIG-COUNT (WS-EVT-SUB).
           ADD WS-ENTITLED-QTY TO WS-ET-ELIG-QTY (WS-EVT-SUB).
           IF WS-DUE-BILL-QTY NOT = ZERO
               ADD 1 TO WS-DUE-BILLS
               ADD 1 TO WS-ET-DUEBILL-CNT (WS-EVT-SUB)
               ADD WS-DUE-BILL-QTY TO WS-ET-DUEBILL-QTY (WS-EVT-SUB).
       3500-EXIT.
           EXIT.
      *
      *================================================================*
      * PASS 3 - UPDATE EVENT MASTER, LOCATION RECONCILIATION          *
      *================================================================*
       4000-UPDATE-EVENTS.
           IF WS-ET-OWNER-SD (WS-EVT-SUB) NOT NUMERIC
               MOVE ZERO TO WS-ET-ELIG-COUNT  (WS-EVT-SUB)
                            WS-ET-ELIG-QTY    (WS-EVT-SUB)
                            WS-ET-OWNER-SD    (WS-EVT-SUB)
                            WS-ET-LOCN-SD     (WS-EVT-SUB)
                            WS-ET-DUEBILL-CNT (WS-EVT-SUB)
                            WS-ET-DUEBILL-QTY (WS-EVT-SUB)
                            WS-ET-SHORT-CNT   (WS-EVT-SUB)
                            WS-ET-LOCN-CNT    (WS-EVT-SUB).
      *
           MOVE WS-ET-EVENT-ID (WS-EVT-SUB) TO CAE-EVENT-ID.
           READ CAEVENT-FILE.
           IF NOT CAEVENT-OK
               MOVE '4000-UPDATE-EVENTS' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           MOVE 'EL'                          TO CAE-STATUS.
           MOVE WS-ET-ELIG-COUNT (WS-EVT-SUB) TO CAE-ELIG-COUNT.
           MOVE WS-ET-ELIG-QTY (WS-EVT-SUB)   TO CAE-ELIG-QTY.
           MOVE ZERO                          TO CAE-ENTL-CASH-TOTAL
                                                 CAE-ENTL-SHARE-TOTAL.
           MOVE DC-BUS-DATE                   TO CAE-LAST-UPD-DATE.
           MOVE JI-JOBNAME                    TO CAE-LAST-UPD-JOB.
           REWRITE CAE-EVENT-REC.
           IF NOT CAEVENT-OK
               MOVE '4000-UPDATE-EVENTS' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'REWRITE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-UPDATED.
      *
      *    OWNER SETTLED QTY PLUS LOCATION QTY MUST NET TO ZERO
           COMPUTE WS-LOCN-DIFF = WS-ET-OWNER-SD (WS-EVT-SUB)
                                + WS-ET-LOCN-SD (WS-EVT-SUB).
           DISPLAY 'CAB200 - EVENT ' WS-ET-EVENT-ID (WS-EVT-SUB)
                   ' ' WS-ET-TYPE (WS-EVT-SUB)
                   ' CUSIP ' WS-ET-CUSIP (WS-EVT-SUB).
           MOVE WS-ET-ELIG-COUNT (WS-EVT-SUB) TO WS-DISP-COUNT.
           DISPLAY '         ELIGIBLE HOLDERS       : ' WS-DISP-COUNT.
           MOVE WS-ET-ELIG-QTY (WS-EVT-SUB)   TO WS-DISP-QTY.
           DISPLAY '         ELIGIBLE QUANTITY      : ' WS-DISP-QTY.
           MOVE WS-ET-DUEBILL-CNT (WS-EVT-SUB) TO WS-DISP-COUNT.
           DISPLAY '         DUE BILL POSITIONS     : ' WS-DISP-COUNT.
           MOVE WS-ET-DUEBILL-QTY (WS-EVT-SUB) TO WS-DISP-QTY.
           DISPLAY '         DUE BILL QUANTITY      : ' WS-DISP-QTY.
           MOVE WS-ET-SHORT-CNT (WS-EVT-SUB)  TO WS-DISP-COUNT.
           DISPLAY '         SHORT/ZERO SKIPPED     : ' WS-DISP-COUNT.
           MOVE WS-ET-OWNER-SD (WS-EVT-SUB)   TO WS-DISP-QTY.
           MOVE WS-ET-LOCN-SD (WS-EVT-SUB)    TO WS-DISP-QTY2.
           DISPLAY '         OWNER SD / LOCATION SD : ' WS-DISP-QTY
                   ' / ' WS-DISP-QTY2.
           IF WS-LOCN-DIFF NOT = ZERO
               ADD 1 TO WS-RECON-BREAKS
               MOVE WS-LOCN-DIFF TO WS-DISP-QTY
               DISPLAY '         *** LOCATION BREAK     : '
                       WS-DISP-QTY
               MOVE 'WRIT'           TO AU-FUNCTION
               MOVE 'LOCNBRK'        TO AU-EVENT
               MOVE 'W'              TO AU-SEVERITY
               MOVE WS-ET-EVENT-ID (WS-EVT-SUB) TO AU-KEY
               MOVE 'OWNER VS LOCATION QTY OUT OF BALANCE ON RECORD DT'
                                     TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS.
       4000-EXIT.
           EXIT.
      *
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-POSMAST-STATUS   TO AB-FILE-STATUS
               MOVE 'POSMAST'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON POSITION MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ACCTMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ACCTMAST'          TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ACCOUNT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE ELIGOUT-FILE.
           IF WS-ELIGOUT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ELIGOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ELIGOUT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ELIGIBILITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           IF WS-RECON-BREAKS > ZERO
           OR WS-ACCT-NOT-FOUND > ZERO
           OR WS-POSN-BAD-QTY > ZERO
               MOVE 4 TO WS-RETURN-CODE.
      *
           MOVE 'EVENTS-IN'        TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-SELECTED TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT
                                      CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'POSN-READ'        TO CT-COUNTER-NAME.
           MOVE WS-POSN-READ       TO CT-COUNT.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'ELIG-OUT'         TO CT-COUNTER-NAME.
           MOVE WS-ELIG-WRITTEN    TO CT-COUNT.
           MOVE WS-ELIG-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'LOCN-RECON'       TO CT-COUNTER-NAME.
           MOVE WS-RECON-BREAKS    TO CT-COUNT.
           MOVE WS-LOCN-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'CLOS'             TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *
           DISPLAY '*************************************************'.
           DISPLAY '* CAB200  CA ELIGIBILITY        - RUN STATISTICS *'.
           DISPLAY '*************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' RECORD DATE PROCESSED    : ' WS-TARGET-DATE.
           MOVE WS-EVENTS-READ      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS ON MASTER         : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-SELECTED  TO WS-DISP-COUNT.
           DISPLAY ' EVENTS SELECTED          : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-UPDATED   TO WS-DISP-COUNT.
           DISPLAY ' EVENTS SET TO EL         : ' WS-DISP-COUNT.
           MOVE WS-POSN-READ        TO WS-DISP-COUNT.
           DISPLAY ' POSITIONS READ           : ' WS-DISP-COUNT.
           MOVE WS-POSN-MATCHED     TO WS-DISP-COUNT.
           DISPLAY ' POSITIONS IN EVENT CUSIP : ' WS-DISP-COUNT.
           MOVE WS-POSN-STREET      TO WS-DISP-COUNT.
           DISPLAY ' LOCATION POSITIONS       : ' WS-DISP-COUNT.
           MOVE WS-POSN-FLAT        TO WS-DISP-COUNT.
           DISPLAY ' FLAT POSITIONS SKIPPED   : ' WS-DISP-COUNT.
           MOVE WS-POSN-SHORT       TO WS-DISP-COUNT.
           DISPLAY ' SHORT POSITIONS SKIPPED  : ' WS-DISP-COUNT.
           MOVE WS-POSN-BAD-QTY     TO WS-DISP-COUNT.
           DISPLAY ' QTY FIELDS RECOVERED     : ' WS-DISP-COUNT.
           MOVE WS-ACCT-NOT-FOUND   TO WS-DISP-COUNT.
           DISPLAY ' ACCOUNTS NOT FOUND       : ' WS-DISP-COUNT.
           MOVE WS-ELIG-WRITTEN     TO WS-DISP-COUNT.
           DISPLAY ' ELIGIBILITY RECS WRITTEN : ' WS-DISP-COUNT.
           MOVE WS-DUE-BILLS        TO WS-DISP-COUNT.
           DISPLAY ' DUE BILLS                : ' WS-DISP-COUNT.
           MOVE WS-RECON-BREAKS     TO WS-DISP-COUNT.
           DISPLAY ' LOCATION RECON BREAKS    : ' WS-DISP-COUNT.
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
           MOVE 'CA RECORD DATE ELIGIBILITY ENDED' TO AU-MESSAGE.
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
           MOVE 'CAB200'         TO CT-STAGE.
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
           DISPLAY 'CAB200 - ABENDING: ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
