       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MGB300.
       AUTHOR.        L F MORETTI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  OCTOBER 1996.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : MGB300                                            *
      * DESCRIPTION: MARGIN INTEREST ACCRUAL AND MONTH-END CHARGE.     *
      *              DAILY: FOR EVERY MARGIN ACCOUNT WITH A DEBIT      *
      *              BALANCE ON MG.REQ, ACCRUE INTEREST FOR THE        *
      *              CALENDAR DAYS SINCE THE LAST ACCRUAL AT THE       *
      *              BROKER CALL RATE (SYSIN) PLUS A SPREAD TIERED ON  *
      *              THE DEBIT BALANCE.  MONTH-TO-DATE INTEREST IS     *
      *              KEPT ON THE MG.INTMTD KSDS; THE MONTH ROLLS ON    *
      *              THE FIRST ACCRUAL OF A NEW MONTH.                 *
      *              MONTH-END (DATE CARD CYCLE M/Q/Y): CHARGE THE     *
      *              MONTH'S INTEREST - GL LINES TXN MINT (DR CLIENT   *
      *              INTEREST RECEIVABLE / CR MARGIN INTEREST INCOME)  *
      *              TO MG.INTCHG AND MARK THE MONTH POSTED.           *
      *              THE CHARGE ITSELF IS DEBITED TO THE CLIENT CASH   *
      *              ACCOUNT BY THE GL FEED (OUT OF SCOPE).            *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSMGD030 / STEP010  (IKJEFT01 - DB2 PLAN MSMGPLN) *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              SYSIN    - MGP300A  BROKER-CALL=NN.NNNN           *
      *                                  FORCE-POST=N                  *
      *              REQIN    - MSEC.PROD.MG.REQ(0)             (MGREQ)*
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS     (CMACCT)*
      * IN/OUT     : MGINTMTD - MSEC.PROD.MG.INTMTD.KSDS       (MGINTM)*
      * OUTPUT     : ACCROUT  - MSEC.PROD.MG.INTACCR(+1)        (MGINT)*
      *              GLOUT    - MSEC.PROD.MG.INTCHG(+1)       (SRGLJNL)*
      *                         (EMPTY EXCEPT AT MONTH-END)            *
      * CALLS      : CMD050 (GL ACCOUNT MAP), CMU010 (DIFC), CMU050,   *
      *              CMU060, CMU080                                    *
      * RETURN CODE: 0  CLEAN                                          *
      *              4  PRIOR MONTH CARRIED FORWARD UNPOSTED, GL MAP   *
      *                 MISSING (SUSPENSE), NO BROKER CALL CARD        *
      *----------------------------------------------------------------*
      * RATE = BROKER CALL + SPREAD (MARGIN AGREEMENT SCHEDULE B)      *
      *        DEBIT BALANCE      UNDER     25,000   + 2.00            *
      *                           UNDER    100,000   + 1.50            *
      *                           UNDER    500,000   + 1.00            *
      *                           UNDER  1,000,000   + 0.50            *
      *                           1,000,000 AND OVER + 0.25            *
      * DAILY INTEREST = DEBIT X RATE / 100 / 360 X CALENDAR DAYS      *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1996-10-14 LFM  ORIGINAL - FROM SRB200 7000-7600      CHG02390 *
      * 1997-01-06 LFM  CALENDAR DAYS SINCE LAST ACCRUAL      CHG02511 *
      *                 (WEEKENDS / HOLIDAYS ACCRUE MONDAY)            *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2000-06-05 TLM  NEW SPREAD SCHEDULE (5 TIERS)         CHG06120 *
      * 2002-03-11 KAP  BROKER CALL RATE FROM CONTROL CARD    CHG09431 *
      * 2005-10-03 KAP  DAILY FACTOR IN FLOATING POINT        CHG14207 *
      * 2009-12-14 SPA  GL LINES VIA CMD050 (TXN MINT)        CHG19002 *
      * 2011-06-20 SPA  AUDIT AND CONTROL TOTALS              CHG21877 *
      * 2014-04-28 SPA  UNPOSTED PRIOR MONTH CARRIED FORWARD  CHG26113 *
      * 2017-02-13 MHC  CHARGES UNDER 1.00 WAIVED             CHG31022 *
      * 2021-09-20 MHC  FORCE-POST CARD FOR MONTH-END RERUN   CHG36480 *
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
           SELECT REQIN-FILE     ASSIGN TO REQIN
                  FILE STATUS IS WS-REQIN-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCT-NO
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT MGINTMTD-FILE  ASSIGN TO MGINTMTD
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS MIM-ACCT-NO
                  FILE STATUS IS WS-MGINTMTD-STATUS.
           SELECT ACCROUT-FILE   ASSIGN TO ACCROUT
                  FILE STATUS IS WS-ACCROUT-STATUS.
           SELECT GLOUT-FILE     ASSIGN TO GLOUT
                  FILE STATUS IS WS-GLOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  PARMCARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PARMCARD-REC                PIC X(80).
       FD  REQIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REQIN-REC                   PIC X(250).
       FD  ACCTMAST-FILE.
           COPY CMACCT.
       FD  MGINTMTD-FILE.
           COPY MGINTM.
       FD  ACCROUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY MGINT.
       FD  GLOUT-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  GLOUT-REC                   PIC X(150).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'MGB300'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
               88  PARMCARD-OK                    VALUE '00'.
               88  PARMCARD-EOF                   VALUE '10'.
           05  WS-REQIN-STATUS         PIC X(02)  VALUE '00'.
               88  REQIN-OK                       VALUE '00'.
               88  REQIN-EOF                      VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
           05  WS-MGINTMTD-STATUS      PIC X(02)  VALUE '00'.
               88  MGINTMTD-OK                    VALUE '00' '02'.
               88  MGINTMTD-EOF                   VALUE '10'.
               88  MGINTMTD-NOTFND                VALUE '23'.
           05  WS-ACCROUT-STATUS       PIC X(02)  VALUE '00'.
           05  WS-GLOUT-STATUS         PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-REQ-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-REQUIREMENTS            VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-MTD-EOF-SW           PIC X(01)  VALUE 'N'.
               88  END-OF-MTD                     VALUE 'Y'.
           05  WS-MTD-FOUND-SW         PIC X(01)  VALUE 'N'.
               88  MTD-ROW-FOUND                  VALUE 'Y'.
           05  WS-POST-SW              PIC X(01)  VALUE 'N'.
               88  POSTING-RUN                    VALUE 'Y'.
           05  WS-BROKER-CARD-SW       PIC X(01)  VALUE 'N'.
               88  BROKER-CALL-FROM-CARD          VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * PARAMETERS                                                     *
      *----------------------------------------------------------------*
       01  WS-PARM-WORK.
           05  WS-PARM-KEYWORD         PIC X(20).
           05  WS-PARM-VALUE           PIC X(20).
           05  WS-PARM-RATE-X.
               10  WS-PARM-RATE-INT    PIC 9(02).
               10  WS-PARM-RATE-DOT    PIC X(01).
               10  WS-PARM-RATE-DEC    PIC 9(04).
       01  WS-FORCE-POST               PIC X(01)  VALUE 'N'.
      *    LAST RATE SET BY THE TREASURY DESK BEFORE THE CARD (CHG09431)
       01  WS-BROKER-CALL-RATE         PIC S9(03)V9(06) COMP-3
                                                     VALUE +06.250000.
      *----------------------------------------------------------------*
      * SPREAD SCHEDULE (MARGIN AGREEMENT SCHEDULE B - CHG06120)       *
      * LIMIT 9(10) WHOLE DOLLARS, SPREAD 9(2)V9(4) PERCENT            *
      *----------------------------------------------------------------*
       01  WS-SPREAD-TIER-VALUES.
           05  FILLER  PIC X(16)  VALUE '0000025000020000'.
           05  FILLER  PIC X(16)  VALUE '0000100000015000'.
           05  FILLER  PIC X(16)  VALUE '0000500000010000'.
           05  FILLER  PIC X(16)  VALUE '0001000000005000'.
           05  FILLER  PIC X(16)  VALUE '9999999999002500'.
       01  WS-SPREAD-TIER-TABLE REDEFINES WS-SPREAD-TIER-VALUES.
           05  WS-TIER OCCURS 5 TIMES.
               10  WS-TIER-LIMIT       PIC 9(10).
               10  WS-TIER-SPREAD      PIC 9(02)V9(04).
       01  WS-TIER-SUB                 PIC S9(04) COMP  VALUE ZERO.
       01  WS-TIER-FOUND               PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * ACCRUAL WORK.  THE DAILY FACTOR IS CARRIED IN FLOATING POINT   *
      * SO 1/360 IS NOT CUT TO SIX PLACES (CHG14207).                  *
      *----------------------------------------------------------------*
       01  WS-ACCRUAL-WORK.
           05  WS-DEBIT                PIC S9(15)V99    COMP-3.
           05  WS-SPREAD               PIC S9(03)V9(06) COMP-3.
           05  WS-EFF-RATE             PIC S9(03)V9(06) COMP-3.
           05  WS-DAYS                 PIC S9(05)       COMP-3.
           05  WS-DAILY-INT            PIC S9(11)V99    COMP-3.
           05  WS-PREV-MTD-DAYS        PIC S9(05)       COMP-3.
           05  WS-NEW-AVG              PIC S9(15)V99    COMP-3.
           05  WS-LAST-ACCR            PIC 9(08).
           05  WS-BUS-MONTH            PIC 9(06).
       01  WS-FLOAT-WORK.
           05  WS-FL-HUNDRED           COMP-2    VALUE 100.
           05  WS-FL-BASIS             COMP-2    VALUE 360.
           05  WS-FL-RATE              COMP-2.
           05  WS-FL-DAILY-FACTOR      COMP-2.
           05  WS-FL-DEBIT             COMP-2.
           05  WS-FL-DAYS              COMP-2.
           05  WS-FL-INTEREST          COMP-2.
       01  WS-WAIVE-LIMIT              PIC S9(03)V99    COMP-3
                                                     VALUE +1.00.
      *----------------------------------------------------------------*
      * GL POSTING                                                     *
      *----------------------------------------------------------------*
       01  WS-GL-TXN-CODE              PIC X(04)  VALUE 'MINT'.
       01  WS-SUSPENSE-GL              PIC X(10)  VALUE 'SUSP999999'.
       01  WS-DEFAULT-CC-PREFIX        PIC X(03)  VALUE 'CC9'.
       01  WS-GL-WORK.
           05  WS-GL-DR                PIC X(10).
           05  WS-GL-CR                PIC X(10).
           05  WS-GL-CC                PIC X(06).
           05  WS-GL-DESC              PIC X(30).
           05  WS-GL-ACCT-TYPE         PIC X(02).
           05  WS-GL-BRANCH            PIC X(03).
           05  WS-GL-AMOUNT            PIC S9(15)V99    COMP-3.
       01  WS-GLJ-REF-WORK.
           05  WS-GR-ACCT              PIC X(10).
           05  WS-GR-MONTH             PIC 9(06).
      *----------------------------------------------------------------*
      * GL MAP CACHE BY ACCOUNT TYPE                                   *
      *----------------------------------------------------------------*
       01  WS-MAP-CACHE.
           05  WS-MAP-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-MAP-ENTRY OCCURS 20 TIMES INDEXED BY MP-IDX.
               10  WS-MP-ACCT-TYPE     PIC X(02).
               10  WS-MP-FOUND         PIC X(01).
               10  WS-MP-DR            PIC X(10).
               10  WS-MP-CR            PIC X(10).
               10  WS-MP-CC            PIC X(06).
               10  WS-MP-DESC          PIC X(30).
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-REQ-IN-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DEBIT-ACCT-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ACCRUAL-OUT-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MTD-NEW-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MTD-UPD-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MONTH-ROLL-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CARRY-FWD-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SAME-DAY-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-DEBIT-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MTD-READ-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSTED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WAIVED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-ALREADY-POSTED-CNT   PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-GLJ-OUT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNMAPPED-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-NO-ACCT-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TIER-CNT OCCURS 5 TIMES
                                       PIC S9(09) COMP-3.
       01  WS-AMOUNTS.
           05  WS-TOT-DEBIT            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-DAILY            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-POSTED           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-WAIVED           PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-GL-DR            PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-TOT-GL-CR            PIC S9(15)V99 COMP-3 VALUE ZERO.
      *----------------------------------------------------------------*
      * TEN LARGEST MONTH-END CHARGES FOR THE TREASURY MEMO            *
      *----------------------------------------------------------------*
       01  WS-TOP-TABLE.
           05  WS-TOP-USED             PIC S9(04) COMP  VALUE ZERO.
           05  WS-TOP-ENTRY OCCURS 10 TIMES.
               10  WS-TOP-ACCT         PIC X(10).
               10  WS-TOP-AMOUNT       PIC S9(11)V99    COMP-3.
               10  WS-TOP-AVG-DEBIT    PIC S9(15)V99    COMP-3.
       01  WS-TOP-SUB                  PIC S9(04) COMP  VALUE ZERO.
       01  WS-TOP-POS                  PIC S9(04) COMP  VALUE ZERO.
       01  WS-TIER-INTEREST.
           05  WS-TIER-INT OCCURS 5 TIMES
                                       PIC S9(13)V99    COMP-3.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-RATE            PIC ZZ9.999999.
           05  WS-DISP-TIER            PIC 9.
       COPY MGREQ.
       COPY SRGLJNL.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMGMLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-REQUIREMENT THRU 2000-EXIT
               UNTIL END-OF-REQUIREMENTS.
           IF POSTING-RUN
               PERFORM 5000-MONTH-END-POSTING THRU 5000-EXIT
           END-IF.
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
           OR DC-PREV-BUS-DATE NOT NUMERIC
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           MOVE DC-BUS-DATE (1:6) TO WS-BUS-MONTH.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MARGIN INTEREST ACCRUAL STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           PERFORM 1100-READ-PARMS THRU 1100-EXIT.
           IF DC-MONTH-END OR WS-FORCE-POST = 'Y'
               MOVE 'Y' TO WS-POST-SW
           END-IF.
           IF WS-FORCE-POST = 'Y' AND NOT DC-MONTH-END
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'FORCPOST'     TO AU-EVENT
               MOVE 'W'            TO AU-SEVERITY
               MOVE WS-BUS-MONTH   TO AU-KEY
               MOVE 'MARGIN INTEREST POSTED ON A DAILY CYCLE'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF.
           MOVE WS-BROKER-CALL-RATE TO WS-DISP-RATE.
           DISPLAY 'MGB300 BROKER CALL RATE : ' WS-DISP-RATE.
           DISPLAY 'MGB300 CYCLE TYPE       : ' DC-CYCLE-TYPE
                   '   POSTING RUN : ' WS-POST-SW.
           OPEN INPUT REQIN-FILE.
           IF WS-REQIN-STATUS NOT = '00'
               MOVE 'REQIN' TO AB-DDNAME
               MOVE WS-REQIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O MGINTMTD-FILE.
           IF WS-MGINTMTD-STATUS NOT = '00'
               MOVE 'MGINTMTD' TO AB-DDNAME
               MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT ACCROUT-FILE.
           IF WS-ACCROUT-STATUS NOT = '00'
               MOVE 'ACCROUT' TO AB-DDNAME
               MOVE WS-ACCROUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT GLOUT-FILE.
           IF WS-GLOUT-STATUS NOT = '00'
               MOVE 'GLOUT' TO AB-DDNAME
               MOVE WS-GLOUT-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           INITIALIZE WS-TIER-CNT (1) WS-TIER-CNT (2) WS-TIER-CNT (3)
                      WS-TIER-CNT (4) WS-TIER-CNT (5).
           INITIALIZE WS-TIER-INTEREST WS-TOP-TABLE.
           PERFORM 8000-READ-REQ THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BROKER-CALL=NN.NNNN   FORCE-POST=Y/N                           *
      *----------------------------------------------------------------*
       1100-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF NOT PARMCARD-OK
               DISPLAY 'MGB300 NO SYSIN - BROKER CALL DEFAULT USED'
               MOVE 4 TO WS-RETURN-CODE
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
                       MOVE '1100-READ-PARMS' TO AB-PARAGRAPH
                       MOVE 'READ FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   WHEN PARMCARD-REC (1:1) = '*'
                   WHEN PARMCARD-REC = SPACES
                       CONTINUE
                   WHEN OTHER
                       PERFORM 1110-EDIT-PARM THRU 1110-EXIT
               END-EVALUATE
           END-PERFORM.
           CLOSE PARMCARD.
           IF NOT BROKER-CALL-FROM-CARD
               DISPLAY 'MGB300 NO BROKER-CALL CARD - DEFAULT USED'
               MOVE 4 TO WS-RETURN-CODE
           END-IF.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-EDIT-PARM.
      *----------------------------------------------------------------*
           DISPLAY 'MGB300 PARM: ' PARMCARD-REC (1:60).
           MOVE SPACES TO WS-PARM-KEYWORD WS-PARM-VALUE.
           UNSTRING PARMCARD-REC DELIMITED BY '=' OR ' '
               INTO WS-PARM-KEYWORD WS-PARM-VALUE
           END-UNSTRING.
           EVALUATE WS-PARM-KEYWORD
               WHEN 'BROKER-CALL'
                   MOVE WS-PARM-VALUE (1:7) TO WS-PARM-RATE-X
                   IF WS-PARM-RATE-INT NUMERIC
                   AND WS-PARM-RATE-DOT = '.'
                   AND WS-PARM-RATE-DEC NUMERIC
                       COMPUTE WS-BROKER-CALL-RATE =
                           WS-PARM-RATE-INT + WS-PARM-RATE-DEC / 10000
                       MOVE 'Y' TO WS-BROKER-CARD-SW
                   ELSE
                       MOVE 'SYSIN' TO AB-DDNAME
                       MOVE 1008 TO AB-ABEND-CODE
                       MOVE '1110-EDIT-PARM' TO AB-PARAGRAPH
                       MOVE PARMCARD-REC (1:40) TO AB-KEY
                       MOVE 'BROKER-CALL MUST BE NN.NNNN' TO AB-MESSAGE
                       GO TO 9999-ABEND
                   END-IF
               WHEN 'FORCE-POST'
                   MOVE WS-PARM-VALUE (1:1) TO WS-FORCE-POST
               WHEN OTHER
                   DISPLAY 'MGB300 UNKNOWN PARAMETER IGNORED'
                   MOVE 4 TO WS-RETURN-CODE
           END-EVALUATE.
       1110-EXIT.
           EXIT.
      *================================================================*
      * ONE MARGIN ACCOUNT                                             *
      *================================================================*
       2000-PROCESS-REQUIREMENT.
           ADD 1 TO WS-REQ-IN-CNT.
           MOVE MRQ-DEBIT-BALANCE TO WS-DEBIT.
           PERFORM 2100-READ-MTD THRU 2100-EXIT.
           IF NOT MTD-ROW-FOUND AND WS-DEBIT NOT > ZERO
               ADD 1 TO WS-NO-DEBIT-CNT
               GO TO 2000-NEXT
           END-IF.
           PERFORM 2200-DAYS-SINCE-LAST THRU 2200-EXIT.
           PERFORM 2300-MONTH-ROLL THRU 2300-EXIT.
           IF WS-DAYS NOT > ZERO
               ADD 1 TO WS-SAME-DAY-CNT
               GO TO 2000-NEXT
           END-IF.
           IF WS-DEBIT > ZERO
               ADD 1 TO WS-DEBIT-ACCT-CNT
               ADD WS-DEBIT TO WS-TOT-DEBIT
               PERFORM 3000-RATE THRU 3000-EXIT
               PERFORM 3100-DAILY-INTEREST THRU 3100-EXIT
               PERFORM 3200-UPDATE-MTD THRU 3200-EXIT
               PERFORM 3300-WRITE-ACCRUAL THRU 3300-EXIT
           ELSE
               ADD 1 TO WS-NO-DEBIT-CNT
           END-IF.
           MOVE DC-BUS-DATE TO MIM-LAST-ACCR-DATE.
           PERFORM 8100-SAVE-MTD THRU 8100-EXIT.
       2000-NEXT.
           PERFORM 8000-READ-REQ THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MONTH-TO-DATE ROW.  A NEW ROW STARTS ACCRUING FROM THE PREVIOUS*
      * BUSINESS DAY.                                                  *
      *----------------------------------------------------------------*
       2100-READ-MTD.
           MOVE MRQ-ACCT-NO TO MIM-ACCT-NO.
           READ MGINTMTD-FILE.
           EVALUATE TRUE
               WHEN MGINTMTD-OK
                   MOVE 'Y' TO WS-MTD-FOUND-SW
               WHEN MGINTMTD-NOTFND
                   MOVE 'N' TO WS-MTD-FOUND-SW
                   INITIALIZE MIM-MTD-REC
                   MOVE MRQ-ACCT-NO      TO MIM-ACCT-NO
                   MOVE WS-BUS-MONTH     TO MIM-MONTH
                   MOVE DC-PREV-BUS-DATE TO MIM-LAST-ACCR-DATE
                   MOVE ZERO             TO MIM-LAST-POSTED-MONTH
               WHEN OTHER
                   MOVE 'MGINTMTD' TO AB-DDNAME
                   MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2100-READ-MTD' TO AB-PARAGRAPH
                   MOVE MRQ-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CALENDAR DAYS SINCE THE LAST ACCRUAL (CHG02511)                *
      *----------------------------------------------------------------*
       2200-DAYS-SINCE-LAST.
           IF MIM-LAST-ACCR-DATE NOT NUMERIC
           OR MIM-LAST-ACCR-DATE = ZERO
               MOVE DC-PREV-BUS-DATE TO MIM-LAST-ACCR-DATE
           END-IF.
           MOVE MIM-LAST-ACCR-DATE TO WS-LAST-ACCR.
           INITIALIZE DT-DATE-PARMS.
           MOVE 'DIFC'          TO DT-FUNCTION.
           MOVE WS-LAST-ACCR    TO DT-DATE-1.
           MOVE DC-BUS-DATE     TO DT-DATE-2.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF NOT DT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '2200-DAYS-SINCE-LAST' TO AB-PARAGRAPH
               MOVE MRQ-ACCT-NO TO AB-KEY
               MOVE DT-MESSAGE TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE DT-RESULT-NUM TO WS-DAYS.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIRST ACCRUAL OF A NEW MONTH.  AN UNPOSTED PRIOR MONTH IS      *
      * CARRIED INTO THE NEW MONTH (CHG26113) - IT IS CHARGED WITH THE *
      * NEXT MONTH-END.                                                *
      *----------------------------------------------------------------*
       2300-MONTH-ROLL.
           IF NOT MTD-ROW-FOUND
               GO TO 2300-EXIT
           END-IF.
           IF MIM-MONTH = WS-BUS-MONTH
               GO TO 2300-EXIT
           END-IF.
           ADD 1 TO WS-MONTH-ROLL-CNT.
           IF MIM-MTD-INTEREST > ZERO
           AND MIM-LAST-POSTED-MONTH NOT = MIM-MONTH
               ADD 1 TO WS-CARRY-FWD-CNT
               MOVE 4 TO WS-RETURN-CODE
               MOVE MIM-MTD-INTEREST TO WS-DISP-AMT
               DISPLAY 'MGB300 MONTH ' MIM-MONTH ' NOT POSTED FOR '
                       MIM-ACCT-NO ' - CARRIED FORWARD ' WS-DISP-AMT
               MOVE 'WRIT'         TO AU-FUNCTION
               MOVE 'INTCARRY'     TO AU-EVENT
               MOVE 'W'            TO AU-SEVERITY
               MOVE MIM-ACCT-NO    TO AU-KEY
               MOVE 'UNPOSTED MARGIN INTEREST CARRIED TO NEW MONTH'
                                   TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           ELSE
               MOVE ZERO TO MIM-MTD-INTEREST MIM-MTD-DAYS
                            MIM-AVG-DEBIT
           END-IF.
           MOVE WS-BUS-MONTH TO MIM-MONTH.
       2300-EXIT.
           EXIT.
      *================================================================*
      * RATE = BROKER CALL + SPREAD OF THE FIRST TIER THE DEBIT IS     *
      * UNDER                                                          *
      *================================================================*
       3000-RATE.
           MOVE ZERO TO WS-TIER-FOUND.
           PERFORM VARYING WS-TIER-SUB FROM 1 BY 1
                   UNTIL WS-TIER-SUB > 5 OR WS-TIER-FOUND > ZERO
               IF WS-DEBIT < WS-TIER-LIMIT (WS-TIER-SUB)
                   MOVE WS-TIER-SUB TO WS-TIER-FOUND
               END-IF
           END-PERFORM.
           IF WS-TIER-FOUND = ZERO
               MOVE 5 TO WS-TIER-FOUND
           END-IF.
           MOVE WS-TIER-SPREAD (WS-TIER-FOUND) TO WS-SPREAD.
           COMPUTE WS-EFF-RATE = WS-BROKER-CALL-RATE + WS-SPREAD.
           ADD 1 TO WS-TIER-CNT (WS-TIER-FOUND).
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * DAILY INTEREST - FACTOR AND PRODUCT IN FLOATING POINT, THE     *
      * RESULT ROUNDED TO THE CENT                                     *
      *----------------------------------------------------------------*
       3100-DAILY-INTEREST.
           MOVE WS-EFF-RATE TO WS-FL-RATE.
           COMPUTE WS-FL-DAILY-FACTOR =
               WS-FL-RATE / WS-FL-HUNDRED / WS-FL-BASIS.
           MOVE WS-DEBIT    TO WS-FL-DEBIT.
           MOVE WS-DAYS     TO WS-FL-DAYS.
           COMPUTE WS-FL-INTEREST =
               WS-FL-DEBIT * WS-FL-DAILY-FACTOR * WS-FL-DAYS.
           COMPUTE WS-DAILY-INT ROUNDED = WS-FL-INTEREST.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MONTH TO DATE - INTEREST, DAYS, DAY-WEIGHTED AVERAGE DEBIT     *
      *----------------------------------------------------------------*
       3200-UPDATE-MTD.
           MOVE MIM-MTD-DAYS TO WS-PREV-MTD-DAYS.
           ADD WS-DAILY-INT  TO MIM-MTD-INTEREST.
           ADD WS-DAYS       TO MIM-MTD-DAYS.
           IF MIM-MTD-DAYS > ZERO
               COMPUTE WS-NEW-AVG ROUNDED =
                   (MIM-AVG-DEBIT * WS-PREV-MTD-DAYS
                    + WS-DEBIT * WS-DAYS) / MIM-MTD-DAYS
               MOVE WS-NEW-AVG TO MIM-AVG-DEBIT
           END-IF.
           ADD WS-DAILY-INT TO WS-TOT-DAILY.
           ADD WS-DAILY-INT TO WS-TIER-INT (WS-TIER-FOUND).
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-WRITE-ACCRUAL.
      *----------------------------------------------------------------*
           MOVE SPACES              TO MGI-ACCRUAL-REC.
           MOVE DC-BUS-DATE         TO MGI-BUS-DATE.
           MOVE MRQ-ACCT-NO         TO MGI-ACCT-NO.
           MOVE WS-DEBIT            TO MGI-DEBIT-BALANCE.
           MOVE WS-BROKER-CALL-RATE TO MGI-BASE-RATE.
           MOVE WS-SPREAD           TO MGI-SPREAD.
           MOVE WS-EFF-RATE         TO MGI-EFFECTIVE-RATE.
           MOVE WS-TIER-FOUND       TO MGI-TIER.
           MOVE WS-DAYS             TO MGI-DAYS.
           MOVE WS-DAILY-INT        TO MGI-DAILY-INTEREST.
           MOVE MIM-MTD-INTEREST    TO MGI-MTD-INTEREST.
           EVALUATE TRUE
               WHEN NOT POSTING-RUN
                   MOVE 'N' TO MGI-POSTED-FLAG
               WHEN MIM-MTD-INTEREST < WS-WAIVE-LIMIT
                   MOVE 'W' TO MGI-POSTED-FLAG
               WHEN OTHER
                   MOVE 'Y' TO MGI-POSTED-FLAG
           END-EVALUATE.
           WRITE MGI-ACCRUAL-REC.
           IF WS-ACCROUT-STATUS NOT = '00'
               MOVE 'ACCROUT' TO AB-DDNAME
               MOVE WS-ACCROUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '3300-WRITE-ACCRUAL' TO AB-PARAGRAPH
               MOVE MRQ-ACCT-NO TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-ACCRUAL-OUT-CNT.
       3300-EXIT.
           EXIT.
      *================================================================*
      * MONTH-END: CHARGE EVERY UNPOSTED MONTH-TO-DATE ROW OF THE      *
      * CURRENT MONTH (ALSO ACCOUNTS WITH NO DEBIT TODAY)              *
      *================================================================*
       5000-MONTH-END-POSTING.
           DISPLAY 'MGB300 MONTH-END POSTING FOR MONTH ' WS-BUS-MONTH.
           MOVE LOW-VALUES TO MIM-ACCT-NO.
           START MGINTMTD-FILE KEY IS NOT LESS THAN MIM-ACCT-NO.
           EVALUATE TRUE
               WHEN MGINTMTD-OK
                   CONTINUE
               WHEN MGINTMTD-NOTFND
                   GO TO 5000-EXIT
               WHEN OTHER
                   MOVE 'MGINTMTD' TO AB-DDNAME
                   MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5000-MONTH-END-POSTING' TO AB-PARAGRAPH
                   MOVE 'START FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 5100-POST-ONE THRU 5100-EXIT
               UNTIL END-OF-MTD.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5100-POST-ONE.
      *----------------------------------------------------------------*
           READ MGINTMTD-FILE NEXT RECORD.
           EVALUATE TRUE
               WHEN MGINTMTD-EOF
                   MOVE 'Y' TO WS-MTD-EOF-SW
                   GO TO 5100-EXIT
               WHEN MGINTMTD-OK
                   ADD 1 TO WS-MTD-READ-CNT
               WHEN OTHER
                   MOVE 'MGINTMTD' TO AB-DDNAME
                   MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5100-POST-ONE' TO AB-PARAGRAPH
                   MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF MIM-MONTH NOT = WS-BUS-MONTH
               GO TO 5100-EXIT
           END-IF.
           IF MIM-LAST-POSTED-MONTH = WS-BUS-MONTH
               ADD 1 TO WS-ALREADY-POSTED-CNT
               GO TO 5100-EXIT
           END-IF.
           IF MIM-MTD-INTEREST NOT > ZERO
               GO TO 5100-EXIT
           END-IF.
      *    CHARGES UNDER 1.00 ARE WAIVED (CHG31022)
           IF MIM-MTD-INTEREST < WS-WAIVE-LIMIT
               ADD 1 TO WS-WAIVED-CNT
               ADD MIM-MTD-INTEREST TO WS-TOT-WAIVED
               MOVE ZERO TO MIM-LAST-POSTED-AMT
           ELSE
               PERFORM 5200-ACCOUNT-DATA THRU 5200-EXIT
               PERFORM 5300-GL-MAP THRU 5300-EXIT
               MOVE MIM-MTD-INTEREST TO WS-GL-AMOUNT
               PERFORM 5400-WRITE-GL-PAIR THRU 5400-EXIT
               MOVE MIM-MTD-INTEREST TO MIM-LAST-POSTED-AMT
               ADD 1 TO WS-POSTED-CNT
               ADD MIM-MTD-INTEREST TO WS-TOT-POSTED
               PERFORM 5500-TOP-CHARGES THRU 5500-EXIT
           END-IF.
           MOVE WS-BUS-MONTH TO MIM-LAST-POSTED-MONTH.
           REWRITE MIM-MTD-REC.
           IF NOT MGINTMTD-OK
               MOVE 'MGINTMTD' TO AB-DDNAME
               MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '5100-POST-ONE' TO AB-PARAGRAPH
               MOVE MIM-ACCT-NO TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       5100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * BRANCH AND ACCOUNT TYPE FOR THE COST CENTER AND THE GL MAP     *
      *----------------------------------------------------------------*
       5200-ACCOUNT-DATA.
           MOVE MIM-ACCT-NO TO ACCT-NO.
           READ ACCTMAST-FILE.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE ACCT-TYPE   TO WS-GL-ACCT-TYPE
                   MOVE ACCT-BRANCH TO WS-GL-BRANCH
               WHEN ACCTMAST-NOTFND
                   ADD 1 TO WS-NO-ACCT-CNT
                   MOVE '**'  TO WS-GL-ACCT-TYPE
                   MOVE '999' TO WS-GL-BRANCH
                   DISPLAY 'MGB300 ACCOUNT NOT ON MASTER ' MIM-ACCT-NO
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5200-ACCOUNT-DATA' TO AB-PARAGRAPH
                   MOVE MIM-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       5200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GL ACCOUNTS FOR TXN MINT BY ACCOUNT TYPE (CACHED)              *
      *----------------------------------------------------------------*
       5300-GL-MAP.
           SET MP-IDX TO 1.
           SEARCH WS-MAP-ENTRY
               AT END
                   PERFORM 5310-CALL-CMD050 THRU 5310-EXIT
               WHEN MP-IDX > WS-MAP-USED
                   PERFORM 5310-CALL-CMD050 THRU 5310-EXIT
               WHEN WS-MP-ACCT-TYPE (MP-IDX) = WS-GL-ACCT-TYPE
                   CONTINUE
           END-SEARCH.
           IF WS-MP-FOUND (MP-IDX) = 'Y'
               MOVE WS-MP-DR (MP-IDX)   TO WS-GL-DR
               MOVE WS-MP-CR (MP-IDX)   TO WS-GL-CR
               MOVE WS-MP-CC (MP-IDX)   TO WS-GL-CC
               MOVE WS-MP-DESC (MP-IDX) TO WS-GL-DESC
           ELSE
               ADD 1 TO WS-UNMAPPED-CNT
               MOVE 4 TO WS-RETURN-CODE
               MOVE WS-SUSPENSE-GL TO WS-GL-DR WS-GL-CR
               MOVE SPACES TO WS-GL-CC
               MOVE 'UNMAPPED - SUSPENSE' TO WS-GL-DESC
           END-IF.
           IF WS-GL-CC = SPACES
               MOVE WS-DEFAULT-CC-PREFIX TO WS-GL-CC (1:3)
           END-IF.
           MOVE WS-GL-BRANCH TO WS-GL-CC (4:3).
       5300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       5310-CALL-CMD050.
      *----------------------------------------------------------------*
           IF WS-MAP-USED < 20
               ADD 1 TO WS-MAP-USED
           END-IF.
           SET MP-IDX TO WS-MAP-USED.
           MOVE WS-GL-ACCT-TYPE TO WS-MP-ACCT-TYPE (MP-IDX).
           MOVE WS-GL-TXN-CODE  TO GM-TXN-CODE.
           MOVE WS-GL-ACCT-TYPE TO GM-ACCT-TYPE.
           MOVE '**'            TO GM-SEC-TYPE.
           CALL 'CMD050' USING GM-GLMAP-PARMS.
           EVALUATE TRUE
               WHEN GM-FOUND
                   MOVE 'Y'              TO WS-MP-FOUND (MP-IDX)
                   MOVE GM-DR-GL-ACCOUNT TO WS-MP-DR (MP-IDX)
                   MOVE GM-CR-GL-ACCOUNT TO WS-MP-CR (MP-IDX)
                   MOVE GM-COST-CENTER   TO WS-MP-CC (MP-IDX)
                   MOVE GM-DESC          TO WS-MP-DESC (MP-IDX)
               WHEN GM-NOT-FOUND
                   MOVE 'N'    TO WS-MP-FOUND (MP-IDX)
                   MOVE SPACES TO WS-MP-DR (MP-IDX) WS-MP-CR (MP-IDX)
                                  WS-MP-CC (MP-IDX) WS-MP-DESC (MP-IDX)
                   DISPLAY 'MGB300 NO GL MAP FOR ' WS-GL-TXN-CODE ' '
                           WS-GL-ACCT-TYPE ' - SUSPENSE'
               WHEN OTHER
                   MOVE 1003 TO AB-ABEND-CODE
                   MOVE '5310-CALL-CMD050' TO AB-PARAGRAPH
                   MOVE GM-SQLCODE TO AB-SQLCODE
                   MOVE WS-GL-ACCT-TYPE TO AB-KEY
                   MOVE 'CMD050 GL ACCOUNT MAP ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       5310-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * LINE 1 DEBIT CLIENT INTEREST RECEIVABLE, LINE 2 CREDIT INCOME  *
      * GLJ-REF = ACCOUNT + CCYYMM                                     *
      *----------------------------------------------------------------*
       5400-WRITE-GL-PAIR.
           MOVE MIM-ACCT-NO        TO WS-GR-ACCT.
           MOVE WS-BUS-MONTH       TO WS-GR-MONTH.
           MOVE SPACES             TO GLJ-JOURNAL-REC.
           MOVE DC-BUS-DATE        TO GLJ-BUS-DATE.
           MOVE 'MG'               TO GLJ-SOURCE.
           MOVE WS-GLJ-REF-WORK    TO GLJ-REF.
           MOVE 1                  TO GLJ-LINE-NO.
           MOVE WS-GL-TXN-CODE     TO GLJ-TXN-CODE.
           MOVE WS-GL-DR           TO GLJ-GL-ACCOUNT.
           MOVE WS-GL-CC           TO GLJ-COST-CENTER.
           SET GLJ-DEBIT           TO TRUE.
           MOVE WS-GL-AMOUNT       TO GLJ-AMOUNT GLJ-AMOUNT-USD.
           MOVE 'USD'              TO GLJ-CCY.
           MOVE MIM-ACCT-NO        TO GLJ-ACCT-NO.
           MOVE SPACES             TO GLJ-CUSIP.
           STRING 'MARGIN INT ' WS-BUS-MONTH ' '
                  WS-GL-DESC DELIMITED BY SIZE INTO GLJ-DESC.
           MOVE SPACE              TO GLJ-REVERSAL-FLAG.
           PERFORM 8200-WRITE-GL THRU 8200-EXIT.
           ADD WS-GL-AMOUNT TO WS-TOT-GL-DR.
           MOVE 2                  TO GLJ-LINE-NO.
           MOVE WS-GL-CR           TO GLJ-GL-ACCOUNT.
           SET GLJ-CREDIT          TO TRUE.
           PERFORM 8200-WRITE-GL THRU 8200-EXIT.
           ADD WS-GL-AMOUNT TO WS-TOT-GL-CR.
       5400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * KEEP THE TEN LARGEST CHARGES (INSERTION INTO A SORTED TABLE)   *
      *----------------------------------------------------------------*
       5500-TOP-CHARGES.
           MOVE ZERO TO WS-TOP-POS.
           PERFORM VARYING WS-TOP-SUB FROM 1 BY 1
                   UNTIL WS-TOP-SUB > WS-TOP-USED
                      OR WS-TOP-POS > ZERO
               IF MIM-MTD-INTEREST > WS-TOP-AMOUNT (WS-TOP-SUB)
                   MOVE WS-TOP-SUB TO WS-TOP-POS
               END-IF
           END-PERFORM.
           IF WS-TOP-POS = ZERO
               IF WS-TOP-USED < 10
                   ADD 1 TO WS-TOP-USED
                   MOVE WS-TOP-USED TO WS-TOP-POS
               ELSE
                   GO TO 5500-EXIT
               END-IF
           ELSE
               IF WS-TOP-USED < 10
                   ADD 1 TO WS-TOP-USED
               END-IF
               PERFORM VARYING WS-TOP-SUB FROM WS-TOP-USED BY -1
                       UNTIL WS-TOP-SUB NOT > WS-TOP-POS
                   MOVE WS-TOP-ENTRY (WS-TOP-SUB - 1)
                                     TO WS-TOP-ENTRY (WS-TOP-SUB)
               END-PERFORM
           END-IF.
           MOVE MIM-ACCT-NO      TO WS-TOP-ACCT (WS-TOP-POS).
           MOVE MIM-MTD-INTEREST TO WS-TOP-AMOUNT (WS-TOP-POS).
           MOVE MIM-AVG-DEBIT    TO WS-TOP-AVG-DEBIT (WS-TOP-POS).
       5500-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-REQ.
           READ REQIN-FILE INTO MRQ-REQUIREMENT-REC.
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
       8100-SAVE-MTD.
      *----------------------------------------------------------------*
           IF MTD-ROW-FOUND
               REWRITE MIM-MTD-REC
               ADD 1 TO WS-MTD-UPD-CNT
           ELSE
               WRITE MIM-MTD-REC
               ADD 1 TO WS-MTD-NEW-CNT
           END-IF.
           IF NOT MGINTMTD-OK
               MOVE 'MGINTMTD' TO AB-DDNAME
               MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8100-SAVE-MTD' TO AB-PARAGRAPH
               MOVE MIM-ACCT-NO TO AB-KEY
               MOVE 'WRITE / REWRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-WRITE-GL.
      *----------------------------------------------------------------*
           WRITE GLOUT-REC FROM GLJ-JOURNAL-REC.
           IF WS-GLOUT-STATUS NOT = '00'
               MOVE 'GLOUT' TO AB-DDNAME
               MOVE WS-GLOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '8200-WRITE-GL' TO AB-PARAGRAPH
               MOVE GLJ-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-GLJ-OUT-CNT.
       8200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'MGB300'       TO CT-STAGE.
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
           CLOSE REQIN-FILE ACCTMAST-FILE.
           CLOSE MGINTMTD-FILE.
           IF WS-MGINTMTD-STATUS NOT = '00'
               MOVE 'MGINTMTD' TO AB-DDNAME
               MOVE WS-MGINTMTD-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE ACCROUT-FILE.
           IF WS-ACCROUT-STATUS NOT = '00'
               MOVE 'ACCROUT' TO AB-DDNAME
               MOVE WS-ACCROUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE GLOUT-FILE.
           IF WS-GLOUT-STATUS NOT = '00'
               MOVE 'GLOUT' TO AB-DDNAME
               MOVE WS-GLOUT-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           MOVE 'REQ-IN'           TO CT-COUNTER-NAME.
           MOVE WS-REQ-IN-CNT      TO CT-COUNT.
           MOVE WS-TOT-DEBIT       TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'ACCRUAL-OUT'      TO CT-COUNTER-NAME.
           MOVE WS-ACCRUAL-OUT-CNT TO CT-COUNT.
           MOVE WS-TOT-DAILY       TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'INT-POSTED'       TO CT-COUNTER-NAME.
           MOVE WS-POSTED-CNT      TO CT-COUNT.
           MOVE WS-TOT-POSTED      TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'GLJ-OUT'          TO CT-COUNTER-NAME.
           MOVE WS-GLJ-OUT-CNT     TO CT-COUNT.
           MOVE WS-TOT-GL-DR       TO CT-AMOUNT.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           DISPLAY '************************************************'.
           DISPLAY '* MGB300 - MARGIN INTEREST                     *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' CYCLE TYPE               : ' DC-CYCLE-TYPE.
           MOVE WS-BROKER-CALL-RATE TO WS-DISP-RATE.
           DISPLAY ' BROKER CALL RATE         : ' WS-DISP-RATE.
           MOVE WS-REQ-IN-CNT TO WS-DISP-CNT.
           DISPLAY ' REQUIREMENTS READ        : ' WS-DISP-CNT.
           MOVE WS-DEBIT-ACCT-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCOUNTS WITH A DEBIT    : ' WS-DISP-CNT.
           MOVE WS-NO-DEBIT-CNT TO WS-DISP-CNT.
           DISPLAY ' NO DEBIT - NOT ACCRUED   : ' WS-DISP-CNT.
           MOVE WS-SAME-DAY-CNT TO WS-DISP-CNT.
           DISPLAY ' ALREADY ACCRUED TODAY    : ' WS-DISP-CNT.
           MOVE WS-ACCRUAL-OUT-CNT TO WS-DISP-CNT.
           DISPLAY ' ACCRUAL RECORDS OUT      : ' WS-DISP-CNT.
           PERFORM VARYING WS-TIER-SUB FROM 1 BY 1
                   UNTIL WS-TIER-SUB > 5
               MOVE WS-TIER-CNT (WS-TIER-SUB) TO WS-DISP-CNT
               MOVE WS-TIER-SUB TO WS-DISP-TIER
               MOVE WS-TIER-INT (WS-TIER-SUB) TO WS-DISP-AMT
               DISPLAY '   TIER ' WS-DISP-TIER '                 : '
                       WS-DISP-CNT ' ' WS-DISP-AMT
           END-PERFORM.
           MOVE WS-MTD-NEW-CNT TO WS-DISP-CNT.
           DISPLAY ' MTD ROWS ADDED           : ' WS-DISP-CNT.
           MOVE WS-MTD-UPD-CNT TO WS-DISP-CNT.
           DISPLAY ' MTD ROWS UPDATED         : ' WS-DISP-CNT.
           MOVE WS-MONTH-ROLL-CNT TO WS-DISP-CNT.
           DISPLAY ' MONTH ROLLED             : ' WS-DISP-CNT.
           MOVE WS-CARRY-FWD-CNT TO WS-DISP-CNT.
           DISPLAY ' UNPOSTED MONTH CARRIED   : ' WS-DISP-CNT.
           MOVE WS-TOT-DEBIT TO WS-DISP-AMT.
           DISPLAY ' DEBIT BALANCES           : ' WS-DISP-AMT.
           MOVE WS-TOT-DAILY TO WS-DISP-AMT.
           DISPLAY ' INTEREST ACCRUED TODAY   : ' WS-DISP-AMT.
           IF POSTING-RUN
               MOVE WS-MTD-READ-CNT TO WS-DISP-CNT
               DISPLAY ' MTD ROWS READ (POSTING)  : ' WS-DISP-CNT
               MOVE WS-POSTED-CNT TO WS-DISP-CNT
               DISPLAY ' ACCOUNTS CHARGED         : ' WS-DISP-CNT
               MOVE WS-WAIVED-CNT TO WS-DISP-CNT
               DISPLAY ' CHARGES WAIVED (< 1.00)  : ' WS-DISP-CNT
               MOVE WS-ALREADY-POSTED-CNT TO WS-DISP-CNT
               DISPLAY ' ALREADY POSTED           : ' WS-DISP-CNT
               MOVE WS-GLJ-OUT-CNT TO WS-DISP-CNT
               DISPLAY ' GL LINES OUT             : ' WS-DISP-CNT
               MOVE WS-UNMAPPED-CNT TO WS-DISP-CNT
               DISPLAY ' GL MAP MISSING (SUSP)    : ' WS-DISP-CNT
               MOVE WS-NO-ACCT-CNT TO WS-DISP-CNT
               DISPLAY ' ACCOUNT NOT ON MASTER    : ' WS-DISP-CNT
               MOVE WS-TOT-POSTED TO WS-DISP-AMT
               DISPLAY ' INTEREST CHARGED         : ' WS-DISP-AMT
               MOVE WS-TOT-WAIVED TO WS-DISP-AMT
               DISPLAY ' INTEREST WAIVED          : ' WS-DISP-AMT
               MOVE WS-TOT-GL-DR TO WS-DISP-AMT
               DISPLAY ' GL DEBITS                : ' WS-DISP-AMT
               MOVE WS-TOT-GL-CR TO WS-DISP-AMT
               DISPLAY ' GL CREDITS               : ' WS-DISP-AMT
               DISPLAY ' LARGEST CHARGES          :'
               PERFORM VARYING WS-TOP-SUB FROM 1 BY 1
                       UNTIL WS-TOP-SUB > WS-TOP-USED
                   MOVE WS-TOP-AMOUNT (WS-TOP-SUB) TO WS-DISP-AMT
                   DISPLAY '   ' WS-TOP-ACCT (WS-TOP-SUB) '  '
                           WS-DISP-AMT WITH NO ADVANCING
                   MOVE WS-TOP-AVG-DEBIT (WS-TOP-SUB) TO WS-DISP-AMT
                   DISPLAY '  AVG DEBIT ' WS-DISP-AMT
               END-PERFORM
           ELSE
               DISPLAY ' MONTH-END POSTING        : NOT A MONTH-END'
           END-IF.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'MARGIN INTEREST ACCRUAL ENDED' TO AU-MESSAGE.
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
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'MGB300 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'MGB300 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'MGB300 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
