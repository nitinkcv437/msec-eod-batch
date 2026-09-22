      *================================================================*
      * PROGRAM    : CAB400                                            *
      * DESCRIPTION: CORPORATE ACTION PAYABLE DATE PROCESSING.         *
      *              FOR EVERY ENTITLED EVENT (STATUS EN) WHOSE PAY    *
      *              DATE IS THE BUSINESS DATE, READS THE EVENT'S      *
      *              ENTITLEMENTS FROM THE ENTITLEMENT MASTER AND      *
      *              PRODUCES STOCK RECORD ACTIVITY (SRACTV) FOR THE   *
      *              STOCK RECORD POSTING RUN (MSSRD010/020):          *
      *                DIV  CASH DIVIDEND (CASH +)                     *
      *                WHT  WITHHOLDING   (CASH -)                     *
      *                SDV  STOCK DIVIDEND SHARES (QTY +)              *
      *                SPL  SPLIT / REVERSE SPLIT NEW SHARES (QTY +)   *
      *                CIL  CASH IN LIEU OF FRACTIONS (CASH +)         *
      *                MGO  MERGER / REVERSE SPLIT SHARES OUT (QTY -)  *
      *                MGC  MERGER CASH (CASH +)                       *
      *              EVERY SHARE MOVEMENT ON THE OWNER ACCOUNT HAS A   *
      *              LOCATION LEG ON THE STREET ACCOUNT OF THE SAME    *
      *              LOCATION SO THE STOCK RECORD STAYS IN BALANCE.    *
      *              ENTITLEMENTS -> PD, EVENT -> PD.                  *
      *----------------------------------------------------------------*
      * JOB        : MSCAD040  STEP010                                 *
      * INPUT      : DATECARD  BUSINESS DATE CARD (CMDATEW)            *
      *              CAEVENT   EVENT MASTER KSDS (I-O)                 *
      *              ENTLMAST  ENTITLEMENT MASTER KSDS (I-O)           *
      * OUTPUT     : ACTVOUT   STOCK RECORD ACTIVITY (SRACTV)          *
      *                        MSEC.PROD.CA.SRACTV(+1)                 *
      *                        ALWAYS CREATED - EMPTY WHEN NOTHING     *
      *                        PAYS (MSSRD010 CONCATENATES IT)         *
      * CALLS      : CMU050 CMU060 CMU080 CMASM01                      *
      *----------------------------------------------------------------*
      * ACTIVITY REFERENCE: ACT-REF = EVENT ID (12) + MOVEMENT         *
      *   SEQUENCE WITHIN THE EVENT (4).  ONE REFERENCE PER MOVEMENT:  *
      *   CASH MOVEMENTS ARE A SINGLE LEG 1 ON THE OWNER ACCOUNT,      *
      *   SHARE MOVEMENTS ARE LEG 1 OWNER + LEG 2 STREET ACCOUNT.      *
      * REVERSE SPLIT: THE OLD POSITION IS DELIVERED OUT (MGO) AND THE *
      *   NEW WHOLE SHARES ARE RECEIVED (SPL) - POSITION REPLACED.     *
      *----------------------------------------------------------------*
      * RETURN CODES:                                                  *
      *   00 CLEAN                                                     *
      *   04 LATE PAYMENT (PAY DATE BEFORE BUSINESS DATE) OR           *
      *      ENTITLEMENTS FOUND ALREADY PAID                           *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1994-04-11 DWB  ORIGINAL                                       *
      * 1995-06-12 RJK  CASH IN LIEU ACTIVITY                 CHG01102 *
      * 1996-11-04 RJK  LOCATION LEG ON STREET ACCOUNT        CHG02960 *
      * 1997-02-10 RJK  BOX AND SEG CARRIED ON DTC STREET ACCT CHG03215*
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2003-05-19 KAP  SEPARATE WHT ACTIVITY                 CHG11020 *
      * 2004-11-01 KAP  READ ENTITLEMENT MASTER KSDS          CHG12877 *
      * 2008-02-25 KAP  CASH MERGER MGO / MGC                 CHG17444 *
      * 2010-04-05 SPA  ACT-SOURCE 'CA'                       CHG19870 *
      * 2011-06-20 SPA  AUDIT AND CONTROL TOTALS              CHG21877 *
      * 2012-10-29 SPA  CATCH UP EVENTS MISSED WHEN THE CYCLE CHG24190 *
      *                 DID NOT RUN (PAY DATE < BUSINESS DATE)         *
      * 2019-05-06 MFO  REVERSE SPLIT INTO NEW CUSIP          CHG33718 *
      * 2019-08-19 MFO  SRB100 ACCEPTS LEGS 1-2 ONLY - ONE REF PER     *
      *                 MOVEMENT, RSP AS MGO OUT + SPL IN     CHG34102 *
      * 2022-03-14 MFO  CMB090 COUNTER SRACTV-OUT             CHG38150 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAB400.
       AUTHOR.        D W BRANDT.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  04/11/94.
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
           SELECT CAEVENT-FILE    ASSIGN TO CAEVENT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CAE-EVENT-ID
                  FILE STATUS IS WS-CAEVENT-STATUS.
      *
           SELECT ENTLMAST-FILE   ASSIGN TO ENTLMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ENT-KEY
                  FILE STATUS IS WS-ENTLMAST-STATUS.
      *
           SELECT ACTVOUT-FILE    ASSIGN TO ACTVOUT
                  FILE STATUS IS WS-ACTVOUT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
      *
       FD  CAEVENT-FILE.
           COPY CAEVENT.
      *
       FD  ENTLMAST-FILE.
           COPY CAENTL.
      *
       FD  ACTVOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACTVOUT-REC                 PIC X(200).
      *
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
           VALUE 'CAB400 WORKING STORAGE BEGINS'.
       01  WS-PROGRAM-ID               PIC X(08) VALUE 'CAB400'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02) VALUE '00'.
           05  WS-CAEVENT-STATUS       PIC X(02) VALUE '00'.
               88  CAEVENT-OK                    VALUE '00'.
               88  CAEVENT-EOF                   VALUE '10'.
               88  CAEVENT-NOTFND                VALUE '23'.
           05  WS-ENTLMAST-STATUS      PIC X(02) VALUE '00'.
               88  ENTLMAST-OK                   VALUE '00'.
               88  ENTLMAST-EOF                  VALUE '10'.
               88  ENTLMAST-NOTFND               VALUE '23'.
           05  WS-ACTVOUT-STATUS       PIC X(02) VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-ENTL-DONE-SW         PIC X(01) VALUE 'N'.
               88  WS-ENTL-DONE                  VALUE 'Y'.
      *
       01  WS-RETURN-CODE              PIC S9(04) COMP VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * EVENTS PAYABLE TODAY                                           *
      *----------------------------------------------------------------*
       01  WS-PAY-MAX                  PIC S9(04) COMP VALUE +100.
       01  WS-PAY-COUNT                PIC S9(04) COMP VALUE ZERO.
       01  WS-PAY-SUB                  PIC S9(04) COMP VALUE ZERO.
       01  WS-PAY-TABLE.
           05  WS-PAY-ENTRY            OCCURS 100 TIMES.
               10  WS-PAY-EVENT-ID     PIC X(12).
               10  WS-PAY-LATE-FLAG    PIC X(01).
      *
      *----------------------------------------------------------------*
      * STREET ACCOUNTS BY LOCATION                                    *
      * BOX AND SEG ARE CARRIED ON THE DTC STREET ACCOUNT - CHG03215   *
      *----------------------------------------------------------------*
       01  WS-STREET-TABLE-VALUES.
           05  FILLER                  PIC X(14) VALUE 'DTC STREETDTC0'.
           05  FILLER                  PIC X(14) VALUE 'FED STREETFED0'.
           05  FILLER                  PIC X(14) VALUE 'EUCLSTREETEUC0'.
           05  FILLER                  PIC X(14) VALUE 'BOX STREETDTC0'.
           05  FILLER                  PIC X(14) VALUE 'SEG STREETDTC0'.
       01  WS-STREET-TABLE REDEFINES WS-STREET-TABLE-VALUES.
           05  WS-ST-ENTRY             OCCURS 5 TIMES.
               10  WS-ST-LOCATION      PIC X(04).
               10  WS-ST-ACCOUNT       PIC X(10).
       01  WS-ST-SUB                   PIC S9(04) COMP VALUE ZERO.
       01  WS-STREET-ACCT              PIC X(10).
      *
      *----------------------------------------------------------------*
      * LEG BUILD AREA                                                 *
      *----------------------------------------------------------------*
       01  WS-LEG-WORK.
           05  WS-MOVE-SEQ             PIC 9(04) VALUE ZERO.
           05  WS-LEG-NO               PIC 9(01) VALUE ZERO.
           05  WS-LEG-ACCT             PIC X(10).
           05  WS-LEG-ACCT-TYPE        PIC X(02).
           05  WS-LEG-CUSIP            PIC X(09).
           05  WS-LEG-TYPE             PIC X(03).
           05  WS-LEG-QTY              PIC S9(11)V9(04) COMP-3.
           05  WS-LEG-CASH             PIC S9(15)V99    COMP-3.
           05  WS-LEG-PRICE            PIC S9(09)V9(08) COMP-3.
           05  WS-LEG-GL-CODE          PIC X(04).
           05  WS-QTY-CHANGE           PIC S9(11)V9(04) COMP-3.
       01  WS-ACT-REF-WORK.
           05  WS-REF-EVENT-ID         PIC X(12).
           05  WS-REF-SEQ              PIC 9(04).
      *
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-EVENTS-READ          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-PAID          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EVENTS-LATE          PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-ENTL-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-PAID            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ENTL-ALREADY         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACTV-WRITTEN         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACTV-CASH-HASH       PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-ACTV-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
           05  WS-ENTL-CASH-PAID       PIC S9(15)V99    COMP-3
                                                 VALUE ZERO.
           05  WS-ENTL-SHARES-PAID     PIC S9(15)V9(04) COMP-3
                                                 VALUE ZERO.
           05  WS-EV-ENTL-COUNT        PIC S9(07) COMP-3 VALUE ZERO.
           05  WS-EV-LEG-COUNT         PIC S9(07) COMP-3 VALUE ZERO.
      *
       01  WS-DISP-COUNT               PIC ZZZ,ZZZ,ZZ9.
       01  WS-DISP-AMT                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-DISP-QTY                 PIC -ZZZ,ZZZ,ZZZ,ZZ9.9999.
      *
           COPY SRACTV.
           COPY CMDATEW.
           COPY CMABLNK.
           COPY CMAULNK.
           COPY CMCTLNK.
           COPY CMJILNK.
      *
       01  FILLER                      PIC X(32)
           VALUE 'CAB400 WORKING STORAGE ENDS'.
      *
       PROCEDURE DIVISION.
      *
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
      *
           PERFORM 2000-SELECT-EVENTS THRU 2000-EXIT.
      *
           IF WS-PAY-COUNT = ZERO
               DISPLAY 'CAB400 - NO EVENTS PAYABLE ON ' DC-BUS-DATE
                       ' - EMPTY ACTIVITY FILE CREATED'
           ELSE
               PERFORM 3000-PAY-EVENT THRU 3000-EXIT
                   VARYING WS-PAY-SUB FROM 1 BY 1
                   UNTIL WS-PAY-SUB > WS-PAY-COUNT.
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
           MOVE 'CA PAYABLE DATE PROCESSING STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
      *
           OPEN I-O CAEVENT-FILE.
           IF WS-CAEVENT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           OPEN I-O ENTLMAST-FILE.
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE 'OPEN FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *    THE ACTIVITY FILE IS OPENED EVEN WHEN NOTHING PAYS
           OPEN OUTPUT ACTVOUT-FILE.
           IF WS-ACTVOUT-STATUS NOT = '00'
               MOVE '1000-INITIALIZE'   TO AB-PARAGRAPH
               MOVE 1001                TO AB-ABEND-CODE
               MOVE WS-ACTVOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ACTVOUT'           TO AB-DDNAME
               MOVE 'OPEN FAILED ON ACTIVITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
       1000-EXIT.
           EXIT.
      *
      *================================================================*
      * EVENTS ENTITLED AND PAYABLE ON OR BEFORE TODAY                 *
      *================================================================*
       2000-SELECT-EVENTS.
           MOVE LOW-VALUES TO CAE-EVENT-ID.
           START CAEVENT-FILE KEY IS NOT LESS THAN CAE-EVENT-ID.
           IF CAEVENT-NOTFND
               GO TO 2000-EXIT.
           IF NOT CAEVENT-OK
               MOVE '2000-SELECT-EVENTS' TO AB-PARAGRAPH
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
               MOVE '2000-SELECT-EVENTS' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ NEXT FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-READ.
           IF NOT CAE-ENTITLED
           OR CAE-PAY-DATE > DC-BUS-DATE
               GO TO 2000-READ-LOOP.
           IF WS-PAY-COUNT NOT < WS-PAY-MAX
               MOVE '2000-SELECT-EVENTS' TO AB-PARAGRAPH
               MOVE 1007                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'MORE THAN 100 EVENTS PAYABLE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-PAY-COUNT.
           MOVE CAE-EVENT-ID TO WS-PAY-EVENT-ID (WS-PAY-COUNT).
           MOVE 'N'          TO WS-PAY-LATE-FLAG (WS-PAY-COUNT).
           IF CAE-PAY-DATE < DC-BUS-DATE
               MOVE 'Y' TO WS-PAY-LATE-FLAG (WS-PAY-COUNT)
               ADD 1 TO WS-EVENTS-LATE
               DISPLAY 'CAB400 - LATE PAYMENT ' CAE-EVENT-ID
                       ' PAY DATE ' CAE-PAY-DATE
               MOVE 'WRIT'           TO AU-FUNCTION
               MOVE 'LATEPAY'        TO AU-EVENT
               MOVE 'W'              TO AU-SEVERITY
               MOVE CAE-EVENT-ID     TO AU-KEY
               MOVE 'EVENT PAID AFTER ITS PAY DATE' TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS.
           GO TO 2000-READ-LOOP.
       2000-EXIT.
           EXIT.
      *
      *================================================================*
      * PAY ONE EVENT                                                  *
      *================================================================*
       3000-PAY-EVENT.
           MOVE WS-PAY-EVENT-ID (WS-PAY-SUB) TO CAE-EVENT-ID.
           READ CAEVENT-FILE.
           IF NOT CAEVENT-OK
               MOVE '3000-PAY-EVENT'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           DISPLAY 'CAB400 - PAYING ' CAE-EVENT-ID ' '
                   CAE-EVENT-TYPE ' CUSIP ' CAE-CUSIP.
           MOVE ZERO TO WS-MOVE-SEQ
                        WS-EV-ENTL-COUNT
                        WS-EV-LEG-COUNT.
      *
      *    POSITION ON THE FIRST ENTITLEMENT OF THE EVENT
           MOVE LOW-VALUES      TO ENT-KEY.
           MOVE CAE-EVENT-ID    TO ENT-EVENT-ID.
           MOVE 'N'             TO WS-ENTL-DONE-SW.
           START ENTLMAST-FILE KEY IS NOT LESS THAN ENT-KEY.
           IF ENTLMAST-NOTFND
               MOVE 'Y' TO WS-ENTL-DONE-SW
           ELSE
               IF NOT ENTLMAST-OK
                   MOVE '3000-PAY-EVENT'    TO AB-PARAGRAPH
                   MOVE 1002                TO AB-ABEND-CODE
                   MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
                   MOVE 'ENTLMAST'          TO AB-DDNAME
                   MOVE CAE-EVENT-ID        TO AB-KEY
                   MOVE 'START FAILED ON ENTITLEMENT MASTER'
                                            TO AB-MESSAGE
                   GO TO 9999-ABEND.
      *
           PERFORM 3100-NEXT-ENTITLEMENT THRU 3100-EXIT
               UNTIL WS-ENTL-DONE.
      *
           IF WS-EV-ENTL-COUNT = ZERO
               DISPLAY 'CAB400 - NO UNPAID ENTITLEMENTS FOR '
                       CAE-EVENT-ID.
      *
      *    RE-READ THE EVENT (RECORD AREA WAS NOT CHANGED BY THE
      *    ENTITLEMENT BROWSE BUT THE READ RE-ESTABLISHES POSITION)
           MOVE WS-PAY-EVENT-ID (WS-PAY-SUB) TO CAE-EVENT-ID.
           READ CAEVENT-FILE.
           IF NOT CAEVENT-OK
               MOVE '3000-PAY-EVENT'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           MOVE 'PD'         TO CAE-STATUS.
           MOVE DC-BUS-DATE  TO CAE-LAST-UPD-DATE.
           MOVE JI-JOBNAME   TO CAE-LAST-UPD-JOB.
           REWRITE CAE-EVENT-REC.
           IF NOT CAEVENT-OK
               MOVE '3000-PAY-EVENT'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-CAEVENT-STATUS   TO AB-FILE-STATUS
               MOVE 'CAEVENT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'REWRITE FAILED ON EVENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-EVENTS-PAID.
      *
           MOVE WS-EV-ENTL-COUNT TO WS-DISP-COUNT.
           DISPLAY '         ENTITLEMENTS PAID      : ' WS-DISP-COUNT.
           MOVE WS-EV-LEG-COUNT  TO WS-DISP-COUNT.
           DISPLAY '         ACTIVITY LEGS WRITTEN  : ' WS-DISP-COUNT.
      *
           MOVE 'WRIT'           TO AU-FUNCTION.
           MOVE 'PAID'           TO AU-EVENT.
           MOVE 'I'              TO AU-SEVERITY.
           MOVE CAE-EVENT-ID     TO AU-KEY.
           MOVE 'EVENT PAID - STOCK RECORD ACTIVITY CREATED'
                                 TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       3000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3100-NEXT-ENTITLEMENT.
      *----------------------------------------------------------------*
           READ ENTLMAST-FILE NEXT RECORD.
           IF ENTLMAST-EOF
               MOVE 'Y' TO WS-ENTL-DONE-SW
               GO TO 3100-EXIT.
           IF NOT ENTLMAST-OK
               MOVE '3100-NEXT-ENTITLEMENT' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'READ NEXT FAILED ON ENTITLEMENT MASTER'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           IF ENT-EVENT-ID NOT = CAE-EVENT-ID
               MOVE 'Y' TO WS-ENTL-DONE-SW
               GO TO 3100-EXIT.
           ADD 1 TO WS-ENTL-READ.
      *
           IF ENT-PAID OR ENT-REVERSED
               ADD 1 TO WS-ENTL-ALREADY
               GO TO 3100-EXIT.
      *
           PERFORM 3200-FIND-STREET-ACCT THRU 3200-EXIT.
      *
           EVALUATE TRUE
               WHEN CAE-CASH-DIV
                   PERFORM 4100-CASH-DIVIDEND THRU 4100-EXIT
               WHEN CAE-STOCK-DIV
               WHEN CAE-FWD-SPLIT
                   PERFORM 4200-SHARE-DISTRIBUTION THRU 4200-EXIT
               WHEN CAE-REV-SPLIT
                   PERFORM 4300-REVERSE-SPLIT THRU 4300-EXIT
               WHEN CAE-CASH-MERGER
                   PERFORM 4400-CASH-MERGER THRU 4400-EXIT
               WHEN OTHER
                   MOVE '3100-NEXT-ENTITLEMENT' TO AB-PARAGRAPH
                   MOVE 1008                TO AB-ABEND-CODE
                   MOVE SPACES              TO AB-FILE-STATUS
                   MOVE 'CAEVENT'           TO AB-DDNAME
                   MOVE CAE-EVENT-ID        TO AB-KEY
                   MOVE 'UNSUPPORTED EVENT TYPE' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
      *
           MOVE 'PD'         TO ENT-STATUS.
           MOVE DC-BUS-DATE  TO ENT-PAID-DATE.
           REWRITE ENT-ENTITLEMENT-REC.
           IF NOT ENTLMAST-OK
               MOVE '3100-NEXT-ENTITLEMENT' TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE ENT-KEY             TO AB-KEY
               MOVE 'REWRITE FAILED ON ENTITLEMENT MASTER'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-ENTL-PAID.
           ADD 1 TO WS-EV-ENTL-COUNT.
           ADD ENT-NET-CASH     TO WS-ENTL-CASH-PAID.
           ADD ENT-CIL-AMOUNT   TO WS-ENTL-CASH-PAID.
           ADD ENT-WHOLE-SHARES TO WS-ENTL-SHARES-PAID.
       3100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       3200-FIND-STREET-ACCT.
      *----------------------------------------------------------------*
           MOVE 'STREETDTC0' TO WS-STREET-ACCT.
           PERFORM VARYING WS-ST-SUB FROM 1 BY 1
                   UNTIL WS-ST-SUB > 5
               IF WS-ST-LOCATION (WS-ST-SUB) = ENT-LOCATION
                   MOVE WS-ST-ACCOUNT (WS-ST-SUB) TO WS-STREET-ACCT
                   MOVE 6 TO WS-ST-SUB
               END-IF
           END-PERFORM.
       3200-EXIT.
           EXIT.
      *
      *================================================================*
      * CASH DIVIDEND - DIV (GROSS) AND WHT (WITHHOLDING)              *
      *================================================================*
       4100-CASH-DIVIDEND.
           IF ENT-GROSS-CASH NOT = ZERO
               MOVE ENT-ACCT-NO      TO WS-LEG-ACCT
               MOVE ENT-ACCT-TYPE    TO WS-LEG-ACCT-TYPE
               MOVE ENT-CUSIP        TO WS-LEG-CUSIP
               MOVE 'DIV'            TO WS-LEG-TYPE
               MOVE ZERO             TO WS-LEG-QTY
               MOVE ENT-GROSS-CASH   TO WS-LEG-CASH
               MOVE CAE-RATE         TO WS-LEG-PRICE
               MOVE 'CDIV'           TO WS-LEG-GL-CODE
               PERFORM 8050-NEW-MOVEMENT THRU 8050-EXIT
               PERFORM 8100-WRITE-LEG THRU 8100-EXIT.
           IF ENT-WHT-AMOUNT NOT = ZERO
               MOVE ENT-ACCT-NO      TO WS-LEG-ACCT
               MOVE ENT-ACCT-TYPE    TO WS-LEG-ACCT-TYPE
               MOVE ENT-CUSIP        TO WS-LEG-CUSIP
               MOVE 'WHT'            TO WS-LEG-TYPE
               MOVE ZERO             TO WS-LEG-QTY
               COMPUTE WS-LEG-CASH = ZERO - ENT-WHT-AMOUNT
               MOVE ZERO             TO WS-LEG-PRICE
               MOVE 'CWHT'           TO WS-LEG-GL-CODE
               PERFORM 8050-NEW-MOVEMENT THRU 8050-EXIT
               PERFORM 8100-WRITE-LEG THRU 8100-EXIT.
       4100-EXIT.
           EXIT.
      *
      *================================================================*
      * STOCK DIVIDEND / FORWARD SPLIT - NEW WHOLE SHARES + CIL        *
      *================================================================*
       4200-SHARE-DISTRIBUTION.
           IF ENT-WHOLE-SHARES NOT = ZERO
               MOVE ENT-WHOLE-SHARES TO WS-QTY-CHANGE
               IF CAE-STOCK-DIV
                   MOVE 'SDV'  TO WS-LEG-TYPE
                   MOVE 'CSDV' TO WS-LEG-GL-CODE
               ELSE
                   MOVE 'SPL'  TO WS-LEG-TYPE
                   MOVE 'CSPL' TO WS-LEG-GL-CODE
               END-IF
               MOVE ENT-NEW-CUSIP TO WS-LEG-CUSIP
               IF WS-LEG-CUSIP = SPACES
                   MOVE ENT-CUSIP TO WS-LEG-CUSIP
               END-IF
               PERFORM 4900-SHARE-PAIR THRU 4900-EXIT.
           PERFORM 4800-CASH-IN-LIEU THRU 4800-EXIT.
       4200-EXIT.
           EXIT.
      *
      *================================================================*
      * REVERSE SPLIT - POSITION REPLACED BY THE NEW WHOLE SHARES      *
      *================================================================*
       4300-REVERSE-SPLIT.
      *    OLD POSITION OUT
           COMPUTE WS-QTY-CHANGE = ZERO - ENT-ELIGIBLE-QTY.
           MOVE ENT-CUSIP   TO WS-LEG-CUSIP.
           MOVE 'MGO'       TO WS-LEG-TYPE.
           MOVE 'CMGO'      TO WS-LEG-GL-CODE.
           IF WS-QTY-CHANGE NOT = ZERO
               PERFORM 4900-SHARE-PAIR THRU 4900-EXIT.
      *    NEW WHOLE SHARES IN (SAME OR NEW CUSIP)
           IF ENT-WHOLE-SHARES NOT = ZERO
               MOVE ENT-WHOLE-SHARES TO WS-QTY-CHANGE
               IF ENT-NEW-CUSIP = SPACES
                   MOVE ENT-CUSIP     TO WS-LEG-CUSIP
               ELSE
                   MOVE ENT-NEW-CUSIP TO WS-LEG-CUSIP
               END-IF
               MOVE 'SPL'  TO WS-LEG-TYPE
               MOVE 'CSPL' TO WS-LEG-GL-CODE
               PERFORM 4900-SHARE-PAIR THRU 4900-EXIT.
           PERFORM 4800-CASH-IN-LIEU THRU 4800-EXIT.
       4300-EXIT.
           EXIT.
      *
      *================================================================*
      * CASH MERGER - SHARES OUT (MGO) AND CASH IN (MGC)               *
      *================================================================*
       4400-CASH-MERGER.
           COMPUTE WS-QTY-CHANGE = ZERO - ENT-ELIGIBLE-QTY.
           MOVE ENT-CUSIP    TO WS-LEG-CUSIP.
           MOVE 'MGO'        TO WS-LEG-TYPE.
           MOVE 'CMGO'       TO WS-LEG-GL-CODE.
           PERFORM 4900-SHARE-PAIR THRU 4900-EXIT.
      *
           MOVE ENT-ACCT-NO       TO WS-LEG-ACCT.
           MOVE ENT-ACCT-TYPE     TO WS-LEG-ACCT-TYPE.
           MOVE ENT-CUSIP         TO WS-LEG-CUSIP.
           MOVE 'MGC'             TO WS-LEG-TYPE.
           MOVE ZERO              TO WS-LEG-QTY.
           MOVE ENT-NET-CASH      TO WS-LEG-CASH.
           MOVE CAE-MRG-CASH-RATE TO WS-LEG-PRICE.
           MOVE 'CMGC'            TO WS-LEG-GL-CODE.
           PERFORM 8050-NEW-MOVEMENT THRU 8050-EXIT.
           PERFORM 8100-WRITE-LEG THRU 8100-EXIT.
       4400-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * CASH IN LIEU OF FRACTIONS                                      *
      *----------------------------------------------------------------*
       4800-CASH-IN-LIEU.
           IF ENT-CIL-AMOUNT = ZERO
               GO TO 4800-EXIT.
           MOVE ENT-ACCT-NO       TO WS-LEG-ACCT.
           MOVE ENT-ACCT-TYPE     TO WS-LEG-ACCT-TYPE.
           MOVE ENT-CUSIP         TO WS-LEG-CUSIP.
           MOVE 'CIL'             TO WS-LEG-TYPE.
           MOVE ZERO              TO WS-LEG-QTY.
           MOVE ENT-CIL-AMOUNT    TO WS-LEG-CASH.
           MOVE CAE-CIL-PRICE     TO WS-LEG-PRICE.
           MOVE 'CCIL'            TO WS-LEG-GL-CODE.
           PERFORM 8050-NEW-MOVEMENT THRU 8050-EXIT.
           PERFORM 8100-WRITE-LEG THRU 8100-EXIT.
       4800-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * SHARE MOVEMENT: OWNER LEG + OPPOSITE LOCATION LEG ON THE       *
      * STREET ACCOUNT.  CALLER SETS WS-QTY-CHANGE, CUSIP, TYPE, GL.   *
      *----------------------------------------------------------------*
       4900-SHARE-PAIR.
           PERFORM 8050-NEW-MOVEMENT THRU 8050-EXIT.
           MOVE ENT-ACCT-NO       TO WS-LEG-ACCT.
           MOVE ENT-ACCT-TYPE     TO WS-LEG-ACCT-TYPE.
           MOVE WS-QTY-CHANGE     TO WS-LEG-QTY.
           MOVE ZERO              TO WS-LEG-CASH
                                     WS-LEG-PRICE.
           PERFORM 8100-WRITE-LEG THRU 8100-EXIT.
      *
           MOVE WS-STREET-ACCT    TO WS-LEG-ACCT.
           MOVE 'ST'              TO WS-LEG-ACCT-TYPE.
           COMPUTE WS-LEG-QTY = ZERO - WS-QTY-CHANGE.
           MOVE ZERO              TO WS-LEG-CASH
                                     WS-LEG-PRICE.
           PERFORM 8100-WRITE-LEG THRU 8100-EXIT.
       4900-EXIT.
           EXIT.
      *
      *================================================================*
      * NEW MOVEMENT - NEXT ACTIVITY REFERENCE FOR THE EVENT           *
      *================================================================*
       8050-NEW-MOVEMENT.
           IF WS-MOVE-SEQ = 9999
               MOVE '8050-NEW-MOVEMENT' TO AB-PARAGRAPH
               MOVE 1007                TO AB-ABEND-CODE
               MOVE SPACES              TO AB-FILE-STATUS
               MOVE 'ACTVOUT'           TO AB-DDNAME
               MOVE CAE-EVENT-ID        TO AB-KEY
               MOVE 'MORE THAN 9999 MOVEMENTS FOR ONE EVENT'
                                        TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-MOVE-SEQ.
           MOVE ZERO TO WS-LEG-NO.
       8050-EXIT.
           EXIT.
      *
      *================================================================*
      * WRITE ONE ACTIVITY LEG                                         *
      *================================================================*
       8100-WRITE-LEG.
           ADD 1 TO WS-LEG-NO.
           MOVE SPACES              TO ACT-ACTIVITY-REC.
           MOVE 'CA'                TO ACT-SOURCE.
           MOVE ENT-EVENT-ID        TO WS-REF-EVENT-ID.
           MOVE WS-MOVE-SEQ         TO WS-REF-SEQ.
           MOVE WS-ACT-REF-WORK     TO ACT-REF.
           MOVE WS-LEG-NO           TO ACT-LEG-NO.
           MOVE WS-LEG-ACCT         TO ACT-ACCT-NO.
           MOVE WS-LEG-CUSIP        TO ACT-CUSIP.
           MOVE ENT-LOCATION        TO ACT-LOCATION.
           MOVE WS-LEG-TYPE         TO ACT-TYPE.
           MOVE WS-LEG-QTY          TO ACT-QTY-CHANGE.
           MOVE WS-LEG-CASH         TO ACT-CASH-CHANGE.
           MOVE ZERO                TO ACT-COST-CHANGE.
           MOVE WS-LEG-PRICE        TO ACT-PRICE.
           MOVE ENT-CCY             TO ACT-CCY.
           MOVE CAE-PAY-DATE        TO ACT-TRADE-DATE
                                       ACT-SETTLE-DATE
                                       ACT-EFFECTIVE-DATE.
           MOVE WS-LEG-GL-CODE      TO ACT-GL-TXN-CODE.
      *    CORPORATE ACTIONS ARE PROCESSED FOR EQUITIES ONLY
           MOVE 'EQ'                TO ACT-SEC-TYPE.
           MOVE WS-LEG-ACCT-TYPE    TO ACT-ACCT-TYPE.
           MOVE 'S'                 TO ACT-SETTLE-FLAG.
           MOVE DC-BUS-DATE         TO ACT-BUS-DATE.
           STRING 'CA ' CAE-EVENT-TYPE ' ' CAE-DESC
                  DELIMITED BY SIZE INTO ACT-DESC.
      *
           WRITE ACTVOUT-REC FROM ACT-ACTIVITY-REC.
           IF WS-ACTVOUT-STATUS NOT = '00'
               MOVE '8100-WRITE-LEG'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ACTVOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ACTVOUT'           TO AB-DDNAME
               MOVE ACT-REF             TO AB-KEY
               MOVE 'WRITE FAILED ON ACTIVITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
           ADD 1 TO WS-ACTV-WRITTEN.
           ADD 1 TO WS-EV-LEG-COUNT.
           ADD ACT-CASH-CHANGE TO WS-ACTV-CASH-HASH.
           IF ACT-QTY-CHANGE < ZERO
               SUBTRACT ACT-QTY-CHANGE FROM WS-ACTV-QTY-HASH
           ELSE
               ADD ACT-QTY-CHANGE TO WS-ACTV-QTY-HASH.
       8100-EXIT.
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
           CLOSE ENTLMAST-FILE.
           IF WS-ENTLMAST-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ENTLMAST-STATUS  TO AB-FILE-STATUS
               MOVE 'ENTLMAST'          TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ENTITLEMENT MASTER' TO AB-MESSAGE
               GO TO 9999-ABEND.
           CLOSE ACTVOUT-FILE.
           IF WS-ACTVOUT-STATUS NOT = '00'
               MOVE '9000-TERMINATE'    TO AB-PARAGRAPH
               MOVE 1002                TO AB-ABEND-CODE
               MOVE WS-ACTVOUT-STATUS   TO AB-FILE-STATUS
               MOVE 'ACTVOUT'           TO AB-DDNAME
               MOVE 'CLOSE FAILED ON ACTIVITY FILE' TO AB-MESSAGE
               GO TO 9999-ABEND.
      *
           IF WS-EVENTS-LATE > ZERO
           OR WS-ENTL-ALREADY > ZERO
               MOVE 4 TO WS-RETURN-CODE.
      *
      *    SRACTV-OUT IS RECONCILED BY CMB090 AGAINST THE STOCK RECORD
           MOVE 'SRACTV-OUT'       TO CT-COUNTER-NAME.
           MOVE WS-ACTV-WRITTEN    TO CT-COUNT.
           MOVE WS-ACTV-CASH-HASH  TO CT-AMOUNT.
           MOVE WS-ACTV-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'ENTL-PAID'        TO CT-COUNTER-NAME.
           MOVE WS-ENTL-PAID       TO CT-COUNT.
           MOVE WS-ENTL-CASH-PAID  TO CT-AMOUNT.
           MOVE WS-ENTL-SHARES-PAID TO CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'EVENTS-PAID'      TO CT-COUNTER-NAME.
           MOVE WS-EVENTS-PAID     TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT
                                      CT-QTY-HASH.
           PERFORM 9100-POST-TOTAL THRU 9100-EXIT.
           MOVE 'CLOS'             TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *
           DISPLAY '*************************************************'.
           DISPLAY '* CAB400  CA PAYABLE DATE       - RUN STATISTICS *'.
           DISPLAY '*************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-EVENTS-READ      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS ON MASTER         : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-PAID      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS PAID              : ' WS-DISP-COUNT.
           MOVE WS-EVENTS-LATE      TO WS-DISP-COUNT.
           DISPLAY ' EVENTS PAID LATE         : ' WS-DISP-COUNT.
           MOVE WS-ENTL-READ        TO WS-DISP-COUNT.
           DISPLAY ' ENTITLEMENTS READ        : ' WS-DISP-COUNT.
           MOVE WS-ENTL-PAID        TO WS-DISP-COUNT.
           DISPLAY ' ENTITLEMENTS PAID        : ' WS-DISP-COUNT.
           MOVE WS-ENTL-ALREADY     TO WS-DISP-COUNT.
           DISPLAY ' ALREADY PAID / REVERSED  : ' WS-DISP-COUNT.
           MOVE WS-ACTV-WRITTEN     TO WS-DISP-COUNT.
           DISPLAY ' SRACTV RECORDS WRITTEN   : ' WS-DISP-COUNT.
           MOVE WS-ACTV-CASH-HASH   TO WS-DISP-AMT.
           DISPLAY ' CASH ACTIVITY            : ' WS-DISP-AMT.
           MOVE WS-ACTV-QTY-HASH    TO WS-DISP-QTY.
           DISPLAY ' SHARE ACTIVITY (ABS)     : ' WS-DISP-QTY.
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
           MOVE 'CA PAYABLE DATE PROCESSING ENDED' TO AU-MESSAGE.
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
           MOVE 'CAB400'         TO CT-STAGE.
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
           DISPLAY 'CAB400 - ABENDING: ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
