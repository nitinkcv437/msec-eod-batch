      *================================================================*
      * PROGRAM    : MGB200                                            *
      * DESCRIPTION: MARGIN CALL PROCESSING.                           *
      *              MATCHES THE DAY'S MARGIN REQUIREMENTS (MG.REQ,    *
      *              ONE RECORD PER MARGIN ACCOUNT, ACCOUNT ORDER)     *
      *              AGAINST THE MARGIN CALL MASTER (MG.CALLS KSDS)    *
      *              AND MAINTAINS THE CALL LIFE CYCLE:                *
      *                NEW    REQUIREMENT STATUS H / T / M WITH NO     *
      *                       ACTIVE CALL OF THAT TYPE -> CALL ISSUED  *
      *                       H -> HM   T -> RT   M -> ME              *
      *                OPEN   DEFICIT REMAINS, NOT YET DUE             *
      *                EXTEND OPS CARD EXTEND=<ACCT> <TYPE> PUSHES THE *
      *                       DUE DATE 2 BUSINESS DAYS (MAX 2 TIMES)   *
      *                LIQ    DEFICIT REMAINS PAST THE DUE DATE -> L   *
      *                MET    DEFICIT OF THE CALL TYPE CURED -> M      *
      *                CANCEL ACCOUNT NO LONGER IN THE MARGIN RUN -> C *
      *              ONE EVENT RECORD PER CALL TOUCHED (MG.CALLEVT).   *
      *              CLOSED CALLS ARE PURGED AFTER 180 CALENDAR DAYS.  *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD020 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - OPS EXTENSION CARDS (MGP200A)          *
      *              REQIN    - MSEC.PROD.MG.REQ(0)             (MGREQ)*
      * IN/OUT     : MGCALLS  - MSEC.PROD.MG.CALLS.KSDS        (MGCALL)*
      * OUTPUT     : EVTOUT   - MSEC.PROD.MG.CALLEVT(+1)       (MGCEVT)*
      * CALLS      : CMU010 (ADDB, ADDC, DIFB), CMU050, CMU060, CMU080,*
      *              CMASM01                                           *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  LIQUIDATION DUE, EXTENSION CARD REJECTED OR    *
      *                 NOT MATCHED                                    *
      *----------------------------------------------------------------*
      * DUE DATES (MARGIN DEPT PROCEDURE MD-120)                       *
      *   RT  REG T CALL          ISSUE DATE + 4 BUSINESS DAYS         *
      *   HM  HOUSE MAINTENANCE   ISSUE DATE + 2 BUSINESS DAYS         *
      *   ME  MINIMUM EQUITY      ISSUE DATE + 5 BUSINESS DAYS         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1996-09-30 LFM  ORIGINAL - SPLIT FROM MGB100          CHG02390 *
      * 1997-05-12 RJK  MINIMUM EQUITY CALLS (ME)             CHG03511 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 1999-06-21 TLM  REG T CALLS IN WHOLE DOLLARS          CHG05102 *
      * 2004-03-15 KAP  EXTENSION CARDS, EVENT FILE           CHG12230 *
      * 2004-09-27 KAP  CANCEL CALLS OF ACCOUNTS NO LONGER    CHG12661 *
      *                 IN THE MARGIN RUN                              *
      * 2009-02-16 SPA  CONTROL TOTALS                        CHG18810 *
      * 2013-11-04 SPA  PURGE CLOSED CALLS AFTER 180 DAYS     CHG25390 *
      * 2018-07-30 MHC  LIQUIDATED CALL CAN STILL BE MET      CHG32655 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGB200.
       AUTHOR.        L F MORETTI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  09/30/96.
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
           SELECT REQIN-FILE      ASSIGN TO REQIN
                  FILE STATUS IS WS-REQIN-STATUS.
      *
           SELECT MGCALLS-FILE    ASSIGN TO MGCALLS
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS MGC-KEY
                  FILE STATUS IS WS-MGCALLS-STATUS.
      *
           SELECT EVTOUT-FILE     ASSIGN TO EVTOUT
                  FILE STATUS IS WS-EVTOUT-STATUS.
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
       FD  REQIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGREQ.
      *
       FD  MGCALLS-FILE.
           COPY MGCALL.
      *
       FD  EVTOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  EVTOUT-REC                  PIC X(150).
      *
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGB200'.
      *
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-REQIN-STATUS         PIC X(02)  VALUE '00'.
               88  REQIN-OK                       VALUE '00'.
               88  REQIN-EOF                      VALUE '10'.
           05  WS-MGCALLS-STATUS       PIC X(02)  VALUE '00'.
               88  MGCALLS-OK                     VALUE '00' '02'.
               88  MGCALLS-EOF                    VALUE '10'.
               88  MGCALLS-NOTFND                 VALUE '23'.
               88  MGCALLS-DUPKEY                 VALUE '22'.
           05  WS-EVTOUT-STATUS        PIC X(02)  VALUE '00'.
      *
       01  WS-SWITCHES.
           05  WS-REQ-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-REQUIREMENTS            VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-CALL-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-ACCT-CALLS              VALUE 'Y'.
           05  WS-SWEEP-EOF-SW         PIC X(01)  VALUE 'N'.
               88  END-OF-SWEEP                   VALUE 'Y'.
           05  WS-TYPE-ACTIVE-SW       PIC X(01)  VALUE 'N'.
               88  TYPE-HAS-ACTIVE-CALL           VALUE 'Y'.
      *
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
       01  WS-SUB                      PIC S9(04) COMP  VALUE ZERO.
       01  WS-SUB2                     PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * DUE DAYS BY CALL TYPE (MD-120)                                 *
      *----------------------------------------------------------------*
       01  WS-DUE-DAYS-VALUES.
           05  FILLER                  PIC X(04)  VALUE 'RT04'.
           05  FILLER                  PIC X(04)  VALUE 'HM02'.
           05  FILLER                  PIC X(04)  VALUE 'ME05'.
       01  WS-DUE-DAYS-TABLE REDEFINES WS-DUE-DAYS-VALUES.
           05  WS-DD-ENTRY OCCURS 3 TIMES INDEXED BY DD-IDX.
               10  WS-DD-CALL-TYPE     PIC X(02).
               10  WS-DD-DAYS          PIC 9(02).
       01  WS-EXT-DAYS                 PIC S9(03) COMP-3 VALUE +2.
       01  WS-MAX-EXTENSIONS           PIC S9(01) COMP-3 VALUE +2.
       01  WS-PURGE-DAYS               PIC S9(05) COMP-3 VALUE -180.
       01  WS-PURGE-DATE               PIC 9(08)  VALUE ZERO.
      *----------------------------------------------------------------*
      * MINIMUM EQUITY - REG T 220.4(B) / NYSE 431(B)                  *
      *----------------------------------------------------------------*
       01  WS-MIN-EQUITY               PIC S9(11)V99 COMP-3
                                                     VALUE +2000.00.
      *----------------------------------------------------------------*
      * EXTENSION CARDS  EXTEND=AAAAAAAAAA TT                          *
      *----------------------------------------------------------------*
       01  WS-EXT-TABLE.
           05  WS-EXT-COUNT            PIC S9(04) COMP  VALUE ZERO.
           05  WS-EXT-MAX              PIC S9(04) COMP  VALUE +200.
           05  WS-EXT-ENTRY OCCURS 200 TIMES INDEXED BY EX-IDX.
               10  WS-EXT-ACCT         PIC X(10).
               10  WS-EXT-TYPE         PIC X(02).
               10  WS-EXT-USED         PIC X(01).
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(10).
           05  WS-PARM-ACCT            PIC X(10).
           05  WS-PARM-TYPE            PIC X(02).
      *----------------------------------------------------------------*
      * CALLS OF THE CURRENT ACCOUNT (LOADED FROM THE KSDS)            *
      *----------------------------------------------------------------*
       01  WS-ACCT-CALLS.
           05  WS-AC-COUNT             PIC S9(04) COMP  VALUE ZERO.
           05  WS-AC-MAX               PIC S9(04) COMP  VALUE +50.
           05  WS-AC-ENTRY OCCURS 50 TIMES INDEXED BY AC-IDX.
               10  WS-AC-RECORD        PIC X(150).
      *
       01  WS-CALL-WORK.
           05  WS-CURR-ACCT            PIC X(10).
           05  WS-REQ-CALL-TYPE        PIC X(02).
           05  WS-DEFICIT-RT           PIC S9(15)V99    COMP-3.
           05  WS-DEFICIT-HM           PIC S9(15)V99    COMP-3.
           05  WS-DEFICIT-ME           PIC S9(15)V99    COMP-3.
           05  WS-TYPE-DEFICIT         PIC S9(15)V99    COMP-3.
           05  WS-WHOLE-DOLLARS        PIC S9(13)       COMP-3.
           05  WS-NEW-CALL-AMT         PIC S9(13)V99    COMP-3.
           05  WS-AMT-MET              PIC S9(13)V99    COMP-3.
           05  WS-EVENT-CODE           PIC X(02).
           05  WS-EVENT-MSG            PIC X(40).
           05  WS-DUE-DAYS             PIC S9(03)       COMP-3.
      *
       01  WS-COUNTERS.
           05  WS-REQ-IN-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REQ-DEFICIT-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-READ-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-NEW-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-UPD-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-MET-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-LIQ-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-EXT-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-CXL-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-PURGED-CNT     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-OPEN-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CALLS-INLIQ-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EXT-REJ-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-EVT-OUT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-RERUN-CNT            PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-AMOUNTS.
           05  WS-NEW-AMT-TOT          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-MET-AMT-TOT          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-LIQ-AMT-TOT          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-OPEN-AMT-TOT         PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-DEFICIT-TOT          PIC S9(15)V99 COMP-3 VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * AGING OF THE CALLS STILL ACTIVE AT END OF RUN, BY CALL TYPE    *
      *   BUCKET 1 = ISSUED TODAY / 1 DAY, 2 = 2-3, 3 = 4-5, 4 = 6 +   *
      *----------------------------------------------------------------*
       01  WS-AGING-TABLE.
           05  WS-AG-TYPE OCCURS 3 TIMES.
               10  WS-AG-BUCKET OCCURS 4 TIMES.
                   15  WS-AG-COUNT     PIC S9(07)       COMP-3.
                   15  WS-AG-AMOUNT    PIC S9(15)V99    COMP-3.
       01  WS-AG-TYPE-SUB              PIC S9(04) COMP  VALUE ZERO.
       01  WS-AG-BKT-SUB               PIC S9(04) COMP  VALUE ZERO.
       01  WS-AG-BUCKET-NAMES.
           05  FILLER                  PIC X(10)  VALUE '0 - 1 DAY '.
           05  FILLER                  PIC X(10)  VALUE '2 - 3 DAYS'.
           05  FILLER                  PIC X(10)  VALUE '4 - 5 DAYS'.
           05  FILLER                  PIC X(10)  VALUE '6 + DAYS  '.
       01  WS-AG-BUCKET-TABLE REDEFINES WS-AG-BUCKET-NAMES.
           05  WS-AG-BUCKET-NAME OCCURS 4 TIMES PIC X(10).
       01  WS-NEW-BY-TYPE.
           05  WS-NEW-TYPE-CNT OCCURS 3 TIMES PIC S9(07) COMP-3.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
       COPY MGCEVT.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMJILNK.
      *
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-REQUIREMENT THRU 2000-EXIT
               UNTIL END-OF-REQUIREMENTS.
           PERFORM 5000-SWEEP-CALLS THRU 5000-EXIT.
           PERFORM 5500-UNMATCHED-CARDS THRU 5500-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           CALL 'CMASM01' USING JI-JOB-INFO.
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
           MOVE 'MARGIN CALL PROCESSING STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-LOAD-EXTENSIONS THRU 1100-EXIT.
      *    PURGE CUT-OFF FOR CLOSED CALLS
           INITIALIZE DT-DATE-PARMS.
           MOVE 'ADDC'         TO DT-FUNCTION.
           MOVE DC-BUS-DATE    TO DT-DATE-1.
           MOVE WS-PURGE-DAYS  TO DT-DAYS.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE DT-MESSAGE TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE DT-RESULT-DATE TO WS-PURGE-DATE.
           INITIALIZE WS-AGING-TABLE WS-NEW-BY-TYPE.
           OPEN INPUT REQIN-FILE.
           IF WS-REQIN-STATUS NOT = '00'
               MOVE 'REQIN' TO AB-DDNAME
               MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O MGCALLS-FILE.
           IF WS-MGCALLS-STATUS NOT = '00'
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT EVTOUT-FILE.
           IF WS-EVTOUT-STATUS NOT = '00'
               MOVE 'EVTOUT' TO AB-DDNAME
               MOVE WS-EVTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 8000-READ-REQ THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EXTENSION CARDS FROM THE MARGIN DEPT (OPS ADD THEM TO MGP200A) *
      *----------------------------------------------------------------*
       1100-LOAD-EXTENSIONS.
           OPEN INPUT PARMCARD.
           IF NOT PARMCARD-OK
               DISPLAY 'MGB200 NO SYSIN - NO EXTENSIONS TODAY'
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
                       MOVE '1100-LOAD-EXTENSIONS' TO AB-PARAGRAPH
                       MOVE 'READ FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   WHEN PARMCARD-REC (1:1) = '*'
                   WHEN PARMCARD-REC = SPACES
                       CONTINUE
                   WHEN OTHER
                       PERFORM 1110-EDIT-CARD THRU 1110-EXIT
               END-EVALUATE
           END-PERFORM.
           CLOSE PARMCARD.
           DISPLAY 'MGB200 EXTENSION CARDS ACCEPTED : ' WS-EXT-COUNT.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-EDIT-CARD.
      *----------------------------------------------------------------*
           DISPLAY 'MGB200 CARD: ' PARMCARD-REC (1:60).
           MOVE SPACES TO WS-PARM-WORK.
           UNSTRING PARMCARD-REC DELIMITED BY '=' OR ALL ' '
               INTO WS-PARM-KEYWORD WS-PARM-ACCT WS-PARM-TYPE
           END-UNSTRING.
           IF WS-PARM-KEYWORD NOT = 'EXTEND'
               DISPLAY 'MGB200 UNKNOWN CARD IGNORED'
               ADD 1 TO WS-EXT-REJ-CNT
               MOVE 4 TO WS-RETURN-CODE
               GO TO 1110-EXIT
           END-IF.
           IF WS-PARM-TYPE NOT = 'RT' AND NOT = 'HM' AND NOT = 'ME'
               DISPLAY 'MGB200 INVALID CALL TYPE ON EXTEND CARD'
               ADD 1 TO WS-EXT-REJ-CNT
               MOVE 4 TO WS-RETURN-CODE
               GO TO 1110-EXIT
           END-IF.
           IF WS-EXT-COUNT NOT < WS-EXT-MAX
               DISPLAY 'MGB200 TOO MANY EXTEND CARDS - IGNORED'
               ADD 1 TO WS-EXT-REJ-CNT
               MOVE 4 TO WS-RETURN-CODE
               GO TO 1110-EXIT
           END-IF.
           ADD 1 TO WS-EXT-COUNT.
           SET EX-IDX TO WS-EXT-COUNT.
           MOVE WS-PARM-ACCT TO WS-EXT-ACCT (EX-IDX).
           MOVE WS-PARM-TYPE TO WS-EXT-TYPE (EX-IDX).
           MOVE 'N'          TO WS-EXT-USED (EX-IDX).
       1110-EXIT.
           EXIT.
      *================================================================*
      * ONE REQUIREMENT RECORD = ONE MARGIN ACCOUNT                    *
      *================================================================*
       2000-PROCESS-REQUIREMENT.
           ADD 1 TO WS-REQ-IN-CNT.
           MOVE MRQ-ACCT-NO TO WS-CURR-ACCT.
           PERFORM 2100-TYPE-DEFICITS THRU 2100-EXIT.
           PERFORM 2200-LOAD-ACCT-CALLS THRU 2200-EXIT.
      *    EXISTING ACTIVE CALLS FIRST - MET, AGED, EXTENDED, LIQ
           PERFORM VARYING AC-IDX FROM 1 BY 1
                   UNTIL AC-IDX > WS-AC-COUNT
               MOVE WS-AC-RECORD (AC-IDX) TO MGC-CALL-REC
               IF MGC-OPEN OR MGC-EXTENDED OR MGC-LIQUIDATE
                   PERFORM 3000-UPDATE-CALL THRU 3000-EXIT
               END-IF
           END-PERFORM.
      *    THEN A NEW CALL IF THE STATUS NEEDS ONE
           IF NOT MRQ-IN-GOOD-ORDER
               ADD 1 TO WS-REQ-DEFICIT-CNT
               PERFORM 4000-NEW-CALL THRU 4000-EXIT
           END-IF.
           PERFORM 8000-READ-REQ THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * DEFICIT PER CALL TYPE.  A CALL IS MET WHEN THE DEFICIT OF ITS  *
      * OWN TYPE IS GONE, WHATEVER TODAY'S STATUS IS.                  *
      *----------------------------------------------------------------*
       2100-TYPE-DEFICITS.
           COMPUTE WS-DEFICIT-HM =
               MRQ-HOUSE-REQ + MRQ-CONC-ADDON - MRQ-EQUITY.
           COMPUTE WS-DEFICIT-RT = MRQ-REGT-REQ - MRQ-EQUITY.
           IF MRQ-DEBIT-BALANCE > ZERO
               COMPUTE WS-DEFICIT-ME = WS-MIN-EQUITY - MRQ-EQUITY
           ELSE
               MOVE ZERO TO WS-DEFICIT-ME
           END-IF.
           EVALUATE TRUE
               WHEN MRQ-HOUSE-CALL
                   MOVE 'HM' TO WS-REQ-CALL-TYPE
               WHEN MRQ-REGT-CALL
                   MOVE 'RT' TO WS-REQ-CALL-TYPE
               WHEN MRQ-MIN-EQUITY-CALL
                   MOVE 'ME' TO WS-REQ-CALL-TYPE
               WHEN OTHER
                   MOVE SPACES TO WS-REQ-CALL-TYPE
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ALL CALLS OF THE ACCOUNT (KEY STARTS WITH THE ACCOUNT NUMBER)  *
      *----------------------------------------------------------------*
       2200-LOAD-ACCT-CALLS.
           MOVE ZERO TO WS-AC-COUNT.
           MOVE 'N'  TO WS-CALL-EOF-SW.
           MOVE LOW-VALUES  TO MGC-KEY.
           MOVE WS-CURR-ACCT TO MGC-ACCT-NO.
           START MGCALLS-FILE KEY IS NOT LESS THAN MGC-KEY.
           EVALUATE TRUE
               WHEN MGCALLS-OK
                   CONTINUE
               WHEN MGCALLS-NOTFND
                   GO TO 2200-EXIT
               WHEN OTHER
                   MOVE 'MGCALLS' TO AB-DDNAME
                   MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2200-LOAD-ACCT-CALLS' TO AB-PARAGRAPH
                   MOVE WS-CURR-ACCT TO AB-KEY
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM UNTIL END-OF-ACCT-CALLS
               READ MGCALLS-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN MGCALLS-EOF
                       MOVE 'Y' TO WS-CALL-EOF-SW
                   WHEN MGCALLS-OK
                       IF MGC-ACCT-NO NOT = WS-CURR-ACCT
                           MOVE 'Y' TO WS-CALL-EOF-SW
                       ELSE
                           PERFORM 2210-STORE-CALL THRU 2210-EXIT
                       END-IF
                   WHEN OTHER
                       MOVE 'MGCALLS' TO AB-DDNAME
                       MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '2200-LOAD-ACCT-CALLS' TO AB-PARAGRAPH
                       MOVE WS-CURR-ACCT TO AB-KEY
                       MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-PERFORM.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2210-STORE-CALL.
      *----------------------------------------------------------------*
           ADD 1 TO WS-CALLS-READ-CNT.
           IF WS-AC-COUNT NOT < WS-AC-MAX
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE 1007 TO AB-ABEND-CODE
               MOVE '2210-STORE-CALL' TO AB-PARAGRAPH
               MOVE WS-CURR-ACCT TO AB-KEY
               MOVE 'MORE THAN 50 CALLS FOR ONE ACCOUNT' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-AC-COUNT.
           SET AC-IDX TO WS-AC-COUNT.
           MOVE MGC-CALL-REC TO WS-AC-RECORD (AC-IDX).
       2210-EXIT.
           EXIT.
      *================================================================*
      * UPDATE AN ACTIVE CALL (O / X / L) - MGC-CALL-REC HOLDS IT      *
      *================================================================*
       3000-UPDATE-CALL.
           PERFORM 3100-CALL-AGE THRU 3100-EXIT.
           EVALUATE MGC-CALL-TYPE
               WHEN 'HM'  MOVE WS-DEFICIT-HM TO WS-TYPE-DEFICIT
               WHEN 'RT'  MOVE WS-DEFICIT-RT TO WS-TYPE-DEFICIT
               WHEN 'ME'  MOVE WS-DEFICIT-ME TO WS-TYPE-DEFICIT
               WHEN OTHER MOVE ZERO          TO WS-TYPE-DEFICIT
           END-EVALUATE.
           MOVE MRQ-EQUITY   TO MGC-LAST-EQUITY.
           MOVE WS-TYPE-DEFICIT TO MGC-LAST-DEFICIT.
           MOVE SPACES       TO WS-EVENT-MSG.
      *    EXTENSION BEFORE THE DUE DATE TEST
           IF NOT MGC-LIQUIDATE
               PERFORM 3200-APPLY-EXTENSION THRU 3200-EXIT
           END-IF.
           EVALUATE TRUE
               WHEN WS-TYPE-DEFICIT NOT > ZERO
                   PERFORM 3300-CALL-MET THRU 3300-EXIT
               WHEN MGC-LIQUIDATE
                   MOVE 'UP' TO WS-EVENT-CODE
                   MOVE 'IN LIQUIDATION - DEFICIT REMAINS'
                                       TO WS-EVENT-MSG
                   ADD 1 TO WS-CALLS-UPD-CNT
               WHEN DC-BUS-DATE > MGC-DUE-DATE
                   PERFORM 3400-CALL-LIQUIDATE THRU 3400-EXIT
               WHEN OTHER
                   PERFORM 3500-CALL-STILL-OPEN THRU 3500-EXIT
           END-EVALUATE.
           MOVE DC-BUS-DATE  TO MGC-LAST-UPD-DATE.
           MOVE JI-JOBNAME   TO MGC-LAST-UPD-JOB.
           PERFORM 8200-REWRITE-CALL THRU 8200-EXIT.
           MOVE MGC-CALL-REC TO WS-AC-RECORD (AC-IDX).
           PERFORM 6000-WRITE-EVENT THRU 6000-EXIT.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * AGE IN BUSINESS DAYS SINCE ISSUE                               *
      *----------------------------------------------------------------*
       3100-CALL-AGE.
           INITIALIZE DT-DATE-PARMS.
           MOVE 'DIFB'          TO DT-FUNCTION.
           MOVE MGC-ISSUE-DATE  TO DT-DATE-1.
           MOVE DC-BUS-DATE     TO DT-DATE-2.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '3100-CALL-AGE' TO AB-PARAGRAPH
               MOVE MGC-KEY TO AB-KEY
               MOVE DT-MESSAGE TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF DT-RESULT-NUM > 999
               MOVE 999 TO MGC-AGE-BUS-DAYS
           ELSE
               MOVE DT-RESULT-NUM TO MGC-AGE-BUS-DAYS
           END-IF.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EXTENSION CARD FOR THIS ACCOUNT / CALL TYPE                    *
      *----------------------------------------------------------------*
       3200-APPLY-EXTENSION.
           IF WS-EXT-COUNT = ZERO
               GO TO 3200-EXIT
           END-IF.
           SET EX-IDX TO 1.
           SEARCH WS-EXT-ENTRY
               AT END
                   GO TO 3200-EXIT
               WHEN EX-IDX > WS-EXT-COUNT
                   GO TO 3200-EXIT
               WHEN WS-EXT-ACCT (EX-IDX) = MGC-ACCT-NO
                AND WS-EXT-TYPE (EX-IDX) = MGC-CALL-TYPE
                AND WS-EXT-USED (EX-IDX) = 'N'
                   MOVE 'Y' TO WS-EXT-USED (EX-IDX)
           END-SEARCH.
           IF MGC-EXTENSION-COUNT NOT < WS-MAX-EXTENSIONS
               ADD 1 TO WS-EXT-REJ-CNT
               MOVE 4 TO WS-RETURN-CODE
               MOVE 'XR' TO WS-EVENT-CODE
               MOVE 'EXTENSION REJECTED - MAXIMUM REACHED'
                                   TO WS-EVENT-MSG
               PERFORM 6000-WRITE-EVENT THRU 6000-EXIT
               MOVE SPACES TO WS-EVENT-MSG
               GO TO 3200-EXIT
           END-IF.
           INITIALIZE DT-DATE-PARMS.
           MOVE 'ADDB'          TO DT-FUNCTION.
           MOVE MGC-DUE-DATE    TO DT-DATE-1.
           MOVE WS-EXT-DAYS     TO DT-DAYS.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '3200-APPLY-EXTENSION' TO AB-PARAGRAPH
               MOVE MGC-KEY TO AB-KEY
               MOVE DT-MESSAGE TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE DT-RESULT-DATE TO MGC-DUE-DATE.
           ADD 1 TO MGC-EXTENSION-COUNT.
           SET MGC-EXTENDED TO TRUE.
           ADD 1 TO WS-CALLS-EXT-CNT.
           MOVE 'EXTENDED 2 BUSINESS DAYS BY MARGIN DEPT'
                               TO WS-EVENT-MSG.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'MGEXTEND'     TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE MGC-KEY        TO AU-KEY.
           MOVE 'MARGIN CALL DUE DATE EXTENDED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * DEFICIT OF THE CALL TYPE CURED                                 *
      *----------------------------------------------------------------*
       3300-CALL-MET.
           SET MGC-MET TO TRUE.
           MOVE DC-BUS-DATE     TO MGC-MET-DATE.
           MOVE MGC-CALL-AMOUNT TO MGC-AMOUNT-MET.
           MOVE 'MT'            TO WS-EVENT-CODE.
           MOVE 'CALL MET - DEFICIT CURED' TO WS-EVENT-MSG.
           ADD 1 TO WS-CALLS-MET-CNT.
           ADD MGC-CALL-AMOUNT TO WS-MET-AMT-TOT.
       3300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PAST DUE - HAND TO THE MARGIN DEPT FOR LIQUIDATION             *
      *----------------------------------------------------------------*
       3400-CALL-LIQUIDATE.
           SET MGC-LIQUIDATE TO TRUE.
           PERFORM 3600-AMOUNT-MET THRU 3600-EXIT.
           MOVE 'LQ' TO WS-EVENT-CODE.
           MOVE 'PAST DUE - LIQUIDATION REQUIRED' TO WS-EVENT-MSG.
           ADD 1 TO WS-CALLS-LIQ-CNT.
           ADD WS-TYPE-DEFICIT TO WS-LIQ-AMT-TOT.
           MOVE 4 TO WS-RETURN-CODE.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'MGLIQ'        TO AU-EVENT.
           MOVE 'W'            TO AU-SEVERITY.
           MOVE MGC-KEY        TO AU-KEY.
           MOVE 'MARGIN CALL PAST DUE - LIQUIDATION' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
       3400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3500-CALL-STILL-OPEN.
      *----------------------------------------------------------------*
           PERFORM 3600-AMOUNT-MET THRU 3600-EXIT.
           IF WS-EVENT-MSG = SPACES
               MOVE 'UP' TO WS-EVENT-CODE
               MOVE 'CALL OPEN - DEFICIT REMAINS' TO WS-EVENT-MSG
           ELSE
               MOVE 'EX' TO WS-EVENT-CODE
           END-IF.
           ADD 1 TO WS-CALLS-UPD-CNT.
       3500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PARTIAL PAYMENT - WHAT OF THE ORIGINAL CALL HAS BEEN COVERED   *
      *----------------------------------------------------------------*
       3600-AMOUNT-MET.
           COMPUTE WS-AMT-MET = MGC-CALL-AMOUNT - WS-TYPE-DEFICIT.
           IF WS-AMT-MET < ZERO
               MOVE ZERO TO WS-AMT-MET
           END-IF.
           MOVE WS-AMT-MET TO MGC-AMOUNT-MET.
       3600-EXIT.
           EXIT.
      *================================================================*
      * NEW CALL FOR TODAY'S STATUS UNLESS ONE OF THAT TYPE IS ACTIVE  *
      *================================================================*
       4000-NEW-CALL.
           MOVE 'N' TO WS-TYPE-ACTIVE-SW.
           PERFORM VARYING AC-IDX FROM 1 BY 1
                   UNTIL AC-IDX > WS-AC-COUNT
               MOVE WS-AC-RECORD (AC-IDX) TO MGC-CALL-REC
               IF MGC-CALL-TYPE = WS-REQ-CALL-TYPE
                   IF MGC-OPEN OR MGC-EXTENDED OR MGC-LIQUIDATE
                       MOVE 'Y' TO WS-TYPE-ACTIVE-SW
                   END-IF
      *            RERUN OF THE SAME DAY - THE CALL WAS ISSUED TODAY
                   IF MGC-ISSUE-DATE = DC-BUS-DATE
                       MOVE 'Y' TO WS-TYPE-ACTIVE-SW
                       ADD 1 TO WS-RERUN-CNT
                   END-IF
               END-IF
           END-PERFORM.
           IF TYPE-HAS-ACTIVE-CALL
               GO TO 4000-EXIT
           END-IF.
           EVALUATE WS-REQ-CALL-TYPE
               WHEN 'HM'  MOVE WS-DEFICIT-HM TO WS-TYPE-DEFICIT
               WHEN 'RT'  MOVE WS-DEFICIT-RT TO WS-TYPE-DEFICIT
               WHEN 'ME'  MOVE WS-DEFICIT-ME TO WS-TYPE-DEFICIT
           END-EVALUATE.
           IF WS-TYPE-DEFICIT NOT > ZERO
               DISPLAY 'MGB200 STATUS ' MRQ-STATUS ' WITHOUT DEFICIT '
                       MRQ-ACCT-NO ' - NO CALL ISSUED'
               GO TO 4000-EXIT
           END-IF.
      *    REG T CALLS ARE ISSUED IN WHOLE DOLLARS (CHG05102)
           IF WS-REQ-CALL-TYPE = 'RT'
               COMPUTE WS-WHOLE-DOLLARS = WS-TYPE-DEFICIT + .99
               MOVE WS-WHOLE-DOLLARS TO WS-NEW-CALL-AMT
           ELSE
               MOVE WS-TYPE-DEFICIT TO WS-NEW-CALL-AMT
           END-IF.
           PERFORM 4100-DUE-DATE THRU 4100-EXIT.
           INITIALIZE MGC-CALL-REC.
           MOVE WS-CURR-ACCT      TO MGC-ACCT-NO.
           MOVE WS-REQ-CALL-TYPE  TO MGC-CALL-TYPE.
           MOVE DC-BUS-DATE       TO MGC-ISSUE-DATE.
           MOVE DT-RESULT-DATE    TO MGC-DUE-DATE.
           MOVE WS-NEW-CALL-AMT   TO MGC-CALL-AMOUNT.
           MOVE ZERO              TO MGC-AMOUNT-MET MGC-AGE-BUS-DAYS
                                     MGC-EXTENSION-COUNT MGC-MET-DATE.
           SET MGC-OPEN           TO TRUE.
           MOVE MRQ-EQUITY        TO MGC-LAST-EQUITY.
           MOVE WS-TYPE-DEFICIT   TO MGC-LAST-DEFICIT.
           MOVE DC-BUS-DATE       TO MGC-LAST-UPD-DATE.
           MOVE JI-JOBNAME        TO MGC-LAST-UPD-JOB.
           PERFORM 8100-WRITE-CALL THRU 8100-EXIT.
           ADD 1 TO WS-CALLS-NEW-CNT.
           ADD WS-NEW-CALL-AMT TO WS-NEW-AMT-TOT.
           SET DD-IDX TO 1.
           SEARCH WS-DD-ENTRY
               AT END
                   CONTINUE
               WHEN WS-DD-CALL-TYPE (DD-IDX) = WS-REQ-CALL-TYPE
                   SET WS-SUB TO DD-IDX
                   ADD 1 TO WS-NEW-TYPE-CNT (WS-SUB)
           END-SEARCH.
           ADD WS-TYPE-DEFICIT TO WS-DEFICIT-TOT.
           MOVE 'NW' TO WS-EVENT-CODE.
           MOVE 'NEW CALL ISSUED' TO WS-EVENT-MSG.
      *    AN EXTENSION CARD CANNOT APPLY TO A CALL ISSUED TODAY
           PERFORM 6000-WRITE-EVENT THRU 6000-EXIT.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-DUE-DATE.
      *----------------------------------------------------------------*
           MOVE ZERO TO WS-DUE-DAYS.
           SET DD-IDX TO 1.
           SEARCH WS-DD-ENTRY
               AT END
                   MOVE 1008 TO AB-ABEND-CODE
                   MOVE '4100-DUE-DATE' TO AB-PARAGRAPH
                   MOVE WS-REQ-CALL-TYPE TO AB-KEY
                   MOVE 'CALL TYPE NOT IN DUE DAYS TABLE' TO AB-MESSAGE
                   GO TO 9999-ABEND
               WHEN WS-DD-CALL-TYPE (DD-IDX) = WS-REQ-CALL-TYPE
                   MOVE WS-DD-DAYS (DD-IDX) TO WS-DUE-DAYS
           END-SEARCH.
           INITIALIZE DT-DATE-PARMS.
           MOVE 'ADDB'         TO DT-FUNCTION.
           MOVE DC-BUS-DATE    TO DT-DATE-1.
           MOVE WS-DUE-DAYS    TO DT-DAYS.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '4100-DUE-DATE' TO AB-PARAGRAPH
               MOVE WS-CURR-ACCT TO AB-KEY
               MOVE DT-MESSAGE TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       4100-EXIT.
           EXIT.
      *================================================================*
      * SWEEP THE CALL MASTER:                                         *
      *   ACTIVE CALLS NOT TOUCHED TODAY -> ACCOUNT LEFT THE MARGIN    *
      *   RUN (CLOSED / CASH ACCOUNT) -> CANCELLED                     *
      *   CLOSED CALLS OLDER THAN THE PURGE DATE -> DELETED            *
      *   COUNT WHAT IS STILL OPEN FOR THE CONTROL TOTALS              *
      *================================================================*
       5000-SWEEP-CALLS.
           MOVE LOW-VALUES TO MGC-KEY.
           START MGCALLS-FILE KEY IS NOT LESS THAN MGC-KEY.
           EVALUATE TRUE
               WHEN MGCALLS-OK
                   CONTINUE
               WHEN MGCALLS-NOTFND
                   GO TO 5000-EXIT
               WHEN OTHER
                   MOVE 'MGCALLS' TO AB-DDNAME
                   MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5000-SWEEP-CALLS' TO AB-PARAGRAPH
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 5100-SWEEP-ONE THRU 5100-EXIT
               UNTIL END-OF-SWEEP.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5100-SWEEP-ONE.
      *----------------------------------------------------------------*
           READ MGCALLS-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN MGCALLS-EOF
                   MOVE 'Y' TO WS-SWEEP-EOF-SW
                   GO TO 5100-EXIT
               WHEN MGCALLS-OK
                   CONTINUE
               WHEN OTHER
                   MOVE 'MGCALLS' TO AB-DDNAME
                   MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5100-SWEEP-ONE' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           EVALUATE TRUE
               WHEN (MGC-OPEN OR MGC-EXTENDED OR MGC-LIQUIDATE)
                AND MGC-LAST-UPD-DATE < DC-BUS-DATE
                   PERFORM 5200-CANCEL-CALL THRU 5200-EXIT
               WHEN MGC-OPEN OR MGC-EXTENDED
                   ADD 1 TO WS-CALLS-OPEN-CNT
                   ADD MGC-CALL-AMOUNT TO WS-OPEN-AMT-TOT
                   PERFORM 5150-AGE-BUCKET THRU 5150-EXIT
               WHEN MGC-LIQUIDATE
                   ADD 1 TO WS-CALLS-INLIQ-CNT
                   PERFORM 5150-AGE-BUCKET THRU 5150-EXIT
               WHEN MGC-LAST-UPD-DATE < WS-PURGE-DATE
                   PERFORM 5300-PURGE-CALL THRU 5300-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       5100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5150-AGE-BUCKET.
      *----------------------------------------------------------------*
           MOVE ZERO TO WS-AG-TYPE-SUB.
           SET DD-IDX TO 1.
           SEARCH WS-DD-ENTRY
               AT END
                   GO TO 5150-EXIT
               WHEN WS-DD-CALL-TYPE (DD-IDX) = MGC-CALL-TYPE
                   SET WS-AG-TYPE-SUB TO DD-IDX
           END-SEARCH.
           EVALUATE TRUE
               WHEN MGC-AGE-BUS-DAYS NOT > 1
                   MOVE 1 TO WS-AG-BKT-SUB
               WHEN MGC-AGE-BUS-DAYS NOT > 3
                   MOVE 2 TO WS-AG-BKT-SUB
               WHEN MGC-AGE-BUS-DAYS NOT > 5
                   MOVE 3 TO WS-AG-BKT-SUB
               WHEN OTHER
                   MOVE 4 TO WS-AG-BKT-SUB
           END-EVALUATE.
           ADD 1 TO WS-AG-COUNT (WS-AG-TYPE-SUB WS-AG-BKT-SUB).
           ADD MGC-CALL-AMOUNT
                 TO WS-AG-AMOUNT (WS-AG-TYPE-SUB WS-AG-BKT-SUB).
       5150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5200-CANCEL-CALL.
      *----------------------------------------------------------------*
           SET MGC-CANCELLED TO TRUE.
           MOVE DC-BUS-DATE  TO MGC-LAST-UPD-DATE.
           MOVE JI-JOBNAME   TO MGC-LAST-UPD-JOB.
           REWRITE MGC-CALL-REC.
           IF NOT MGCALLS-OK
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '5200-CANCEL-CALL' TO AB-PARAGRAPH
               MOVE MGC-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-CALLS-CXL-CNT.
           MOVE SPACES TO MRQ-REQUIREMENT-REC.
           MOVE ZERO   TO MRQ-EQUITY.
           MOVE 'CX'   TO WS-EVENT-CODE.
           MOVE 'ACCOUNT NOT IN MARGIN RUN - CANCELLED'
                       TO WS-EVENT-MSG.
           MOVE ZERO   TO WS-TYPE-DEFICIT.
           PERFORM 6000-WRITE-EVENT THRU 6000-EXIT.
       5200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5300-PURGE-CALL.
      *----------------------------------------------------------------*
           DELETE MGCALLS-FILE RECORD.
           IF NOT MGCALLS-OK
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '5300-PURGE-CALL' TO AB-PARAGRAPH
               MOVE MGC-KEY TO AB-KEY
               MOVE 'DELETE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-CALLS-PURGED-CNT.
       5300-EXIT.
           EXIT.
      *================================================================*
      * EXTENSION CARDS THAT FOUND NO ACTIVE CALL                      *
      *================================================================*
       5500-UNMATCHED-CARDS.
           PERFORM VARYING EX-IDX FROM 1 BY 1
                   UNTIL EX-IDX > WS-EXT-COUNT
               IF WS-EXT-USED (EX-IDX) = 'N'
                   ADD 1 TO WS-EXT-REJ-CNT
                   MOVE 4 TO WS-RETURN-CODE
                   DISPLAY 'MGB200 EXTEND CARD NOT MATCHED '
                           WS-EXT-ACCT (EX-IDX) ' '
                           WS-EXT-TYPE (EX-IDX)
                   INITIALIZE MGC-CALL-REC
                   MOVE WS-EXT-ACCT (EX-IDX) TO MGC-ACCT-NO
                   MOVE WS-EXT-TYPE (EX-IDX) TO MGC-CALL-TYPE
                   MOVE ZERO TO MGC-ISSUE-DATE MGC-DUE-DATE
                                MGC-MET-DATE
                   MOVE SPACE TO MGC-STATUS
                   MOVE SPACES TO MRQ-REQUIREMENT-REC
                   MOVE ZERO   TO MRQ-EQUITY WS-TYPE-DEFICIT
                   MOVE 'XR' TO WS-EVENT-CODE
                   MOVE 'EXTENSION REJECTED - NO ACTIVE CALL'
                                   TO WS-EVENT-MSG
                   PERFORM 6000-WRITE-EVENT THRU 6000-EXIT
               END-IF
           END-PERFORM.
       5500-EXIT.
           EXIT.
      *================================================================*
      * CALL EVENT RECORD                                              *
      *================================================================*
       6000-WRITE-EVENT.
           MOVE SPACES              TO MCE-CALL-EVENT-REC.
           MOVE DC-BUS-DATE         TO MCE-BUS-DATE.
           MOVE WS-EVENT-CODE       TO MCE-EVENT-CODE.
           MOVE MGC-ACCT-NO         TO MCE-ACCT-NO.
           MOVE MGC-CALL-TYPE       TO MCE-CALL-TYPE.
           MOVE MGC-ISSUE-DATE      TO MCE-ISSUE-DATE.
           MOVE MRQ-BRANCH          TO MCE-BRANCH.
           MOVE MRQ-REP             TO MCE-REP.
           MOVE MGC-STATUS          TO MCE-STATUS.
           MOVE MGC-DUE-DATE        TO MCE-DUE-DATE.
           MOVE MGC-CALL-AMOUNT     TO MCE-CALL-AMOUNT.
           MOVE MGC-AMOUNT-MET      TO MCE-AMOUNT-MET.
           MOVE WS-TYPE-DEFICIT     TO MCE-CURR-DEFICIT.
           MOVE MRQ-EQUITY          TO MCE-EQUITY.
           MOVE MGC-AGE-BUS-DAYS    TO MCE-AGE-BUS-DAYS.
           MOVE MGC-EXTENSION-COUNT TO MCE-EXTENSION-COUNT.
           MOVE MGC-MET-DATE        TO MCE-MET-DATE.
           MOVE MRQ-STATUS          TO MCE-REQ-STATUS.
           MOVE WS-EVENT-MSG        TO MCE-MESSAGE.
           WRITE EVTOUT-REC FROM MCE-CALL-EVENT-REC.
           IF WS-EVTOUT-STATUS NOT = '00'
               MOVE 'EVTOUT' TO AB-DDNAME
               MOVE WS-EVTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '6000-WRITE-EVENT' TO AB-PARAGRAPH
               MOVE MCE-CALL-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-EVT-OUT-CNT.
       6000-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-REQ.
           READ REQIN-FILE.
           EVALUATE TRUE
               WHEN REQIN-OK
                   CONTINUE
               WHEN REQIN-EOF
                   MOVE 'Y' TO WS-REQ-EOF-SW
               WHEN OTHER
                   MOVE 'REQIN' TO AB-DDNAME
                   MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-REQ' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * NEW CALL.  '22' = SAME ACCOUNT, TYPE AND ISSUE DATE ALREADY ON *
      * FILE (RERUN AFTER A LATER STEP FAILED) - REPLACE IT.           *
      *----------------------------------------------------------------*
       8100-WRITE-CALL.
           WRITE MGC-CALL-REC.
           EVALUATE TRUE
               WHEN MGCALLS-OK
                   CONTINUE
               WHEN MGCALLS-DUPKEY
                   ADD 1 TO WS-RERUN-CNT
                   REWRITE MGC-CALL-REC
                   IF NOT MGCALLS-OK
                       MOVE 'MGCALLS' TO AB-DDNAME
                       MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '8100-WRITE-CALL' TO AB-PARAGRAPH
                       MOVE MGC-KEY TO AB-KEY
                       MOVE 'REWRITE AFTER 22 FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   END-IF
               WHEN OTHER
                   MOVE 'MGCALLS' TO AB-DDNAME
                   MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-WRITE-CALL' TO AB-PARAGRAPH
                   MOVE MGC-KEY TO AB-KEY
                   MOVE 'WRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-REWRITE-CALL.
      *----------------------------------------------------------------*
           REWRITE MGC-CALL-REC.
           IF NOT MGCALLS-OK
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-REWRITE-CALL' TO AB-PARAGRAPH
               MOVE MGC-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'MGB200'       TO CT-STAGE.
           MOVE ZERO           TO CT-QTY-HASH.
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
           CLOSE REQIN-FILE.
           IF WS-REQIN-STATUS NOT = '00'
               MOVE 'REQIN' TO AB-DDNAME
               MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE MGCALLS-FILE.
           IF WS-MGCALLS-STATUS NOT = '00'
               MOVE 'MGCALLS' TO AB-DDNAME
               MOVE WS-MGCALLS-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE EVTOUT-FILE.
           IF WS-EVTOUT-STATUS NOT = '00'
               MOVE 'EVTOUT' TO AB-DDNAME
               MOVE WS-EVTOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'REQ-IN'           TO CT-COUNTER-NAME.
           MOVE WS-REQ-IN-CNT      TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CALLS-NEW'        TO CT-COUNTER-NAME.
           MOVE WS-CALLS-NEW-CNT   TO CT-COUNT.
           MOVE WS-NEW-AMT-TOT     TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CALLS-OPEN'       TO CT-COUNTER-NAME.
           MOVE WS-CALLS-OPEN-CNT  TO CT-COUNT.
           MOVE WS-OPEN-AMT-TOT    TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CALLS-MET'        TO CT-COUNTER-NAME.
           MOVE WS-CALLS-MET-CNT   TO CT-COUNT.
           MOVE WS-MET-AMT-TOT     TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CALLS-LIQ'        TO CT-COUNTER-NAME.
           MOVE WS-CALLS-LIQ-CNT   TO CT-COUNT.
           MOVE WS-LIQ-AMT-TOT     TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CALLEVT-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-EVT-OUT-CNT     TO CT-COUNT.
           MOVE ZERO               TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* MGB200 - MARGIN CALL PROCESSING              *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' PURGE CLOSED CALLS BEFORE: ' WS-PURGE-DATE.
           MOVE WS-REQ-IN-CNT TO WS-DISP-CNT.
           DISPLAY ' REQUIREMENTS READ        : ' WS-DISP-CNT.
           MOVE WS-REQ-DEFICIT-CNT TO WS-DISP-CNT.
           DISPLAY '   NOT IN GOOD ORDER      : ' WS-DISP-CNT.
           MOVE WS-CALLS-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS READ (ACCOUNTS)    : ' WS-DISP-CNT.
           MOVE WS-CALLS-NEW-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS ISSUED             : ' WS-DISP-CNT.
           MOVE WS-CALLS-UPD-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS STILL OPEN / AGED  : ' WS-DISP-CNT.
           MOVE WS-CALLS-EXT-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS EXTENDED           : ' WS-DISP-CNT.
           MOVE WS-CALLS-MET-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS MET                : ' WS-DISP-CNT.
           MOVE WS-CALLS-LIQ-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS TO LIQUIDATION     : ' WS-DISP-CNT.
           MOVE WS-CALLS-CXL-CNT TO WS-DISP-CNT.
           DISPLAY ' CALLS CANCELLED          : ' WS-DISP-CNT.
           MOVE WS-CALLS-PURGED-CNT TO WS-DISP-CNT.
           DISPLAY ' CLOSED CALLS PURGED      : ' WS-DISP-CNT.
           MOVE WS-CALLS-OPEN-CNT TO WS-DISP-CNT.
           DISPLAY ' OPEN AT END OF RUN       : ' WS-DISP-CNT.
           MOVE WS-CALLS-INLIQ-CNT TO WS-DISP-CNT.
           DISPLAY ' IN LIQUIDATION           : ' WS-DISP-CNT.
           MOVE WS-EXT-REJ-CNT TO WS-DISP-CNT.
           DISPLAY ' CARDS REJECTED/UNMATCHED : ' WS-DISP-CNT.
           MOVE WS-RERUN-CNT TO WS-DISP-CNT.
           DISPLAY ' ISSUED EARLIER TODAY     : ' WS-DISP-CNT.
           MOVE WS-EVT-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' EVENT RECORDS OUT        : ' WS-DISP-CNT.
           MOVE WS-NEW-AMT-TOT TO WS-DISP-AMT.
           DISPLAY ' NEW CALL AMOUNT          : ' WS-DISP-AMT.
           MOVE WS-OPEN-AMT-TOT TO WS-DISP-AMT.
           DISPLAY ' OPEN CALL AMOUNT         : ' WS-DISP-AMT.
           MOVE WS-MET-AMT-TOT TO WS-DISP-AMT.
           DISPLAY ' MET CALL AMOUNT          : ' WS-DISP-AMT.
           MOVE WS-LIQ-AMT-TOT TO WS-DISP-AMT.
           DISPLAY ' LIQUIDATION DEFICIT      : ' WS-DISP-AMT.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           PERFORM 9100-AGING-DISPLAY THRU 9100-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MARGIN CALL PROCESSING ENDED' TO AU-MESSAGE.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
           ELSE
               MOVE 'I' TO AU-SEVERITY
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ACTIVE CALLS BY TYPE AND AGE (OPEN, EXTENDED, LIQUIDATION)     *
      *----------------------------------------------------------------*
       9100-AGING-DISPLAY.
           DISPLAY ' '.
           DISPLAY ' ACTIVE CALLS BY AGE   TYPE  NEW TODAY'
                   '   AGE BUCKET      CALLS              AMOUNT'.
           PERFORM VARYING WS-AG-TYPE-SUB FROM 1 BY 1
                   UNTIL WS-AG-TYPE-SUB > 3
               MOVE WS-NEW-TYPE-CNT (WS-AG-TYPE-SUB) TO WS-DISP-CNT
               DISPLAY '                       '
                       WS-DD-CALL-TYPE (WS-AG-TYPE-SUB) '  '
                       WS-DISP-CNT
               PERFORM VARYING WS-AG-BKT-SUB FROM 1 BY 1
                       UNTIL WS-AG-BKT-SUB > 4
                   MOVE WS-AG-COUNT (WS-AG-TYPE-SUB WS-AG-BKT-SUB)
                                          TO WS-DISP-CNT
                   MOVE WS-AG-AMOUNT (WS-AG-TYPE-SUB WS-AG-BKT-SUB)
                                          TO WS-DISP-AMT
                   DISPLAY '                                      '
                           WS-AG-BUCKET-NAME (WS-AG-BKT-SUB) ' '
                           WS-DISP-CNT ' ' WS-DISP-AMT
               END-PERFORM
           END-PERFORM.
       9100-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'MGB200 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'MGB200 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'MGB200 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
