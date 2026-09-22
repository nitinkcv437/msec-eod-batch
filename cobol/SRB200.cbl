       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB200.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  SEPTEMBER 1987.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB200                                            *
      * DESCRIPTION: STOCK RECORD POSITION POSTING.                    *
      *              APPLIES THE DAY'S VALIDATED ACTIVITY LEGS TO THE  *
      *              POSITION MASTER (THE STOCK RECORD).               *
      *              - TRADE-DATE QUANTITY IS ALWAYS UPDATED.          *
      *              - SETTLED QUANTITY IS UPDATED WHEN THE LEG        *
      *                SETTLES ON OR BEFORE THE BUSINESS DATE,         *
      *                OTHERWISE A PENDING SETTLEMENT ROW IS WRITTEN   *
      *                AND PENDING IN / OUT IS INCREASED.              *
      *              - WEIGHTED AVERAGE COST ON PURCHASES, REALIZED    *
      *                P&L AGAINST AVERAGE COST ON SALES.              *
      *              - FLAT POSITIONS MARKED 'F', SHORTS FLAGGED.      *
      *              - ONE POSTING JOURNAL RECORD PER LEG WITH BEFORE  *
      *                AND AFTER VALUES (INPUT TO GL BUILD SRB600 AND  *
      *                CASH POSTING SRB300).                           *
      *              CHECKPOINT / RESTART THROUGH CMU070.              *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD020 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              ACTVIN   - MSEC.PROD.SR.ACTV.VALID(0)    (SRACTV) *
      *              SYSIN    - OPTIONAL CONTROL CARDS                 *
      *                         CKPT=NNNNN   CHECKPOINT INTERVAL       *
      *                         RERUN=FORCE  ALLOW RERUN W/O RESTART   *
      * IN/OUT     : POSMAST  - MSEC.PROD.SR.POSITION.KSDS    (SRPOSN) *
      *              PENDSETL - MSEC.PROD.SR.PENDSETL.KSDS    (SRPEND) *
      *              CHKPTFL  - CHECKPOINT KSDS (VIA CMU070)           *
      * OUTPUT     : POSTJRNL - MSEC.PROD.SR.POSTJRNL(+1)     (SRPSTJ) *
      * CALLS      : CMU070, CMU050, CMU060, CMU080, CMASM01, CMASM02  *
      * RETURN CODE: 0 CLEAN, 4 WARNINGS (FROZEN POSITIONS, DUPLICATE  *
      *              PENDING ROWS), 8 UNPOSTABLE LEGS                  *
      *----------------------------------------------------------------*
      * RESTART    : RESUBMIT THE STEP.  CMU070 RETURNS THE LAST KEY   *
      *              CHECKPOINTED AND RECORDS UP TO AND INCLUDING THAT *
      *              KEY ARE BYPASSED.  POSTJRNL MUST BE RE-ALLOCATED  *
      *              BY OPERATIONS (SEE RUNBOOK SR-020).               *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1987-09-01 RJK  ORIGINAL                                       *
      * 1988-04-18 RJK  PENDING SETTLEMENT FILE               CHG00061 *
      * 1989-03-20 RJK  DATE CARD INSTEAD OF SYSTEM DATE      CHG00212 *
      * 1990-01-08 DWB  MARGIN INTEREST ACCRUAL ADDED         CHG00544 *
      * 1991-06-03 DWB  POSTING JOURNAL FOR GL                CHG00987 *
      * 1993-11-22 DWB  CHECKPOINT/RESTART (CMU070)           CHG01733 *
      * 1995-03-06 LFM  BOX LOCATION DEMATERIALIZATION        CHG02011 *
      * 1996-09-30 LFM  MARGIN INTEREST MOVED TO MARGIN SYS   CHG02390 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 1999-06-14 TLM  Y2K - YTD RESET ON YEAR ROLL          CHG04802 *
      * 2001-07-16 KAP  AVERAGE COST / REALIZED P&L           CHG08811 *
      * 2001-07-16 KAP  DECIMALIZATION                        CHG08811 *
      * 2003-01-27 KAP  SHORT SALE / BUY TO COVER             CHG10355 *
      * 2004-10-11 KAP  POSITION BUFFERING BY KEY             CHG12880 *
      * 2006-05-02 SPA  FILE STATUS 02 ON POSITION AIX        CHG15502 *
      * 2009-12-14 SPA  MULTI-CURRENCY                        CHG19002 *
      * 2010-04-05 SPA  CORPORATE ACTION ACTIVITY TYPES       CHG19870 *
      * 2013-02-18 SPA  INSERT COLLISION RECOVERY (22)        CHG24418 *
      * 2017-08-07 MHC  FROZEN POSITIONS POST WITH WARNING    CHG31207 *
      * 2019-02-25 MHC  CONTROL TOTALS FOR CMB090             CHG33410 *
      * 2024-05-20 NVR  T+1 SETTLEMENT                        CHG40551 *
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
           SELECT ACTVIN-FILE    ASSIGN TO ACTVIN
                  FILE STATUS IS WS-ACTVIN-STATUS.
           SELECT POSTJRNL-FILE  ASSIGN TO POSTJRNL
                  FILE STATUS IS WS-POSTJRNL-STATUS.
           SELECT POSMAST-FILE   ASSIGN TO POSMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS POS-KEY
                  FILE STATUS IS WS-POSMAST-STATUS.
           SELECT PENDSETL-FILE  ASSIGN TO PENDSETL
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS PND-KEY
                  FILE STATUS IS WS-PENDSETL-STATUS.
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
       FD  ACTVIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACTVIN-REC                  PIC X(200).
       FD  POSTJRNL-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  POSTJRNL-REC                PIC X(300).
       FD  POSMAST-FILE.
       COPY SRPOSN.
       FD  PENDSETL-FILE.
       COPY SRPEND.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB200'.
       01  FILLER                      PIC X(24)
                                       VALUE 'SRB200 WORKING STORAGE '.
      *----------------------------------------------------------------*
      * FILE STATUS AREAS                                              *
      *----------------------------------------------------------------*
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-PARMCARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-ACTVIN-STATUS        PIC X(02)  VALUE '00'.
               88  ACTVIN-OK                      VALUE '00'.
               88  ACTVIN-EOF                     VALUE '10'.
           05  WS-POSTJRNL-STATUS      PIC X(02)  VALUE '00'.
           05  WS-POSMAST-STATUS       PIC X(02)  VALUE '00'.
               88  POSMAST-OK                     VALUE '00'.
               88  POSMAST-DUP-AIX                VALUE '02'.
               88  POSMAST-DUP-KEY                VALUE '22'.
               88  POSMAST-NOTFND                 VALUE '23'.
           05  WS-PENDSETL-STATUS      PIC X(02)  VALUE '00'.
               88  PENDSETL-OK                    VALUE '00'.
               88  PENDSETL-DUP-AIX               VALUE '02'.
               88  PENDSETL-DUP-KEY               VALUE '22'.
      *----------------------------------------------------------------*
      * SWITCHES                                                       *
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-ACTIVITY                VALUE 'Y'.
           05  WS-PARM-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-PARMS                   VALUE 'Y'.
           05  WS-BUFFER-SW            PIC X(01)  VALUE 'N'.
               88  POSITION-BUFFERED              VALUE 'Y'.
               88  BUFFER-EMPTY                   VALUE 'N'.
           05  WS-BUFFER-ACTION-SW     PIC X(01)  VALUE SPACE.
               88  BUFFER-IS-NEW                  VALUE 'I'.
               88  BUFFER-IS-EXISTING             VALUE 'U'.
           05  WS-LEG-SIDE-SW          PIC X(01)  VALUE SPACE.
               88  OWNERSHIP-LEG                  VALUE 'O'.
               88  LOCATION-LEG                   VALUE 'L'.
           05  WS-CASH-ONLY-SW         PIC X(01)  VALUE 'N'.
               88  CASH-ONLY-LEG                  VALUE 'Y'.
           05  WS-SETTLE-NOW-SW        PIC X(01)  VALUE 'N'.
               88  SETTLES-NOW                    VALUE 'Y'.
           05  WS-RESTART-SW           PIC X(01)  VALUE 'N'.
               88  RESTART-IN-PROGRESS            VALUE 'Y'.
           05  WS-SKIPPING-SW          PIC X(01)  VALUE 'N'.
               88  SKIPPING-TO-RESTART            VALUE 'Y'.
           05  WS-FORCE-RERUN-SW       PIC X(01)  VALUE 'N'.
               88  FORCE-RERUN                    VALUE 'Y'.
           05  WS-DEBUG-SW             PIC X(01)  VALUE 'N'.
               88  DEBUG-ON                       VALUE 'Y'.
           05  WS-BOX-CONVERT-SW       PIC X(01)  VALUE 'N'.
               88  BOX-CONVERT-ON                 VALUE 'Y'.
           05  WS-POSTABLE-SW          PIC X(01)  VALUE 'Y'.
               88  LEG-POSTABLE                   VALUE 'Y'.
               88  LEG-NOT-POSTABLE               VALUE 'N'.
           05  WS-YTD-RESET-SW         PIC X(01)  VALUE 'N'.
               88  YTD-WAS-RESET                  VALUE 'Y'.
      *----------------------------------------------------------------*
      * CHECKPOINT CONTROL                                             *
      *----------------------------------------------------------------*
       01  WS-CKPT-INTERVAL            PIC S9(07) COMP-3 VALUE +500.
       01  WS-CKPT-SINCE-LAST          PIC S9(07) COMP-3 VALUE ZERO.
       01  WS-INPUT-KEY.
           05  WS-IK-ACCT-NO           PIC X(10).
           05  WS-IK-CUSIP             PIC X(09).
           05  WS-IK-LOCATION          PIC X(04).
           05  WS-IK-REF               PIC X(16).
           05  WS-IK-LEG-NO            PIC 9(01).
       01  WS-RESTART-KEY              PIC X(40)  VALUE LOW-VALUES.
      *----------------------------------------------------------------*
      * POSITION KEY CONTROL FOR BUFFERING                             *
      *----------------------------------------------------------------*
       01  WS-CUR-POS-KEY.
           05  WS-CPK-ACCT-NO          PIC X(10).
           05  WS-CPK-CUSIP            PIC X(09).
           05  WS-CPK-LOCATION         PIC X(04).
       01  WS-BUF-POS-KEY              PIC X(23)  VALUE LOW-VALUES.
      *----------------------------------------------------------------*
      * BUFFERED POSITION - SAME LAYOUT AS SRPOSN                      *
      *----------------------------------------------------------------*
       01  WS-BUF-POSITION.
           05  WS-BUF-KEY.
               10  WS-BUF-ACCT-NO      PIC X(10).
               10  WS-BUF-CUSIP        PIC X(09).
               10  WS-BUF-LOCATION     PIC X(04).
           05  WS-BUF-SEC-TYPE         PIC X(02).
           05  WS-BUF-ACCT-TYPE        PIC X(02).
           05  WS-BUF-CCY              PIC X(03).
           05  WS-BUF-TD-QTY           PIC S9(11)V9(04) COMP-3.
           05  WS-BUF-SD-QTY           PIC S9(11)V9(04) COMP-3.
           05  WS-BUF-PEND-IN-QTY      PIC S9(11)V9(04) COMP-3.
           05  WS-BUF-PEND-OUT-QTY     PIC S9(11)V9(04) COMP-3.
           05  WS-BUF-AVG-COST         PIC S9(09)V9(06) COMP-3.
           05  WS-BUF-COST-BASIS       PIC S9(15)V99    COMP-3.
           05  WS-BUF-MKT-PRICE        PIC S9(09)V9(08) COMP-3.
           05  WS-BUF-PRICE-DATE       PIC 9(08).
           05  WS-BUF-MKT-VALUE        PIC S9(15)V99    COMP-3.
           05  WS-BUF-FX-RATE          PIC S9(05)V9(08) COMP-3.
           05  WS-BUF-MKT-VALUE-USD    PIC S9(15)V99    COMP-3.
           05  WS-BUF-UNRLZD-PL        PIC S9(15)V99    COMP-3.
           05  WS-BUF-REALIZED-PL-YTD  PIC S9(15)V99    COMP-3.
           05  WS-BUF-OPEN-DATE        PIC 9(08).
           05  WS-BUF-LAST-ACTV-DATE   PIC 9(08).
           05  WS-BUF-LAST-UPD-JOB     PIC X(08).
           05  WS-BUF-SHORT-FLAG       PIC X(01).
           05  WS-BUF-STATUS           PIC X(01).
               88  WS-BUF-OPEN                    VALUE 'O'.
               88  WS-BUF-FLAT                    VALUE 'F'.
               88  WS-BUF-FROZEN                  VALUE 'Z'.
           05  FILLER                  PIC X(35).
      *----------------------------------------------------------------*
      * POSTING WORK FIELDS                                            *
      *----------------------------------------------------------------*
       01  WS-POSTING-WORK.
           05  WS-QTY                  PIC S9(11)V9(04) COMP-3.
           05  WS-ABS-QTY              PIC S9(11)V9(04) COMP-3.
           05  WS-OLD-TD-QTY           PIC S9(11)V9(04) COMP-3.
           05  WS-NEW-TD-QTY           PIC S9(11)V9(04) COMP-3.
           05  WS-OLD-AVG-COST         PIC S9(09)V9(06) COMP-3.
           05  WS-OLD-COST-BASIS       PIC S9(15)V99    COMP-3.
           05  WS-TRADE-AMT            PIC S9(15)V99    COMP-3.
           05  WS-UNIT-COST            PIC S9(09)V9(06) COMP-3.
           05  WS-CLOSE-QTY            PIC S9(11)V9(04) COMP-3.
           05  WS-OPEN-QTY             PIC S9(11)V9(04) COMP-3.
           05  WS-CLOSE-AMT            PIC S9(15)V99    COMP-3.
           05  WS-OPEN-AMT             PIC S9(15)V99    COMP-3.
           05  WS-COST-RELIEVED        PIC S9(15)V99    COMP-3.
           05  WS-REALIZED-PL          PIC S9(15)V99    COMP-3.
           05  WS-WORK-AMT             PIC S9(15)V9(06) COMP-3.
           05  WS-PRICE-MULT           PIC S9(01)V9(04) COMP-3.
           05  WS-POST-ACTION          PIC X(01).
           05  WS-BUS-CCYY             PIC 9(04).
           05  WS-LAST-CCYY            PIC 9(04).
       01  WS-Y2K-WORK.
           05  WS-Y2K-DATE-IN          PIC 9(08).
           05  WS-Y2K-DATE-6 REDEFINES WS-Y2K-DATE-IN.
               10  FILLER              PIC 9(02).
               10  WS-Y2K-YY           PIC 9(02).
               10  WS-Y2K-MM           PIC 9(02).
               10  WS-Y2K-DD           PIC 9(02).
           05  WS-Y2K-DATE-OUT         PIC 9(08).
           05  WS-Y2K-DATE-OUT-R REDEFINES WS-Y2K-DATE-OUT.
               10  WS-Y2K-OUT-CC       PIC 9(02).
               10  WS-Y2K-OUT-YY       PIC 9(02).
               10  WS-Y2K-OUT-MM       PIC 9(02).
               10  WS-Y2K-OUT-DD       PIC 9(02).
           05  WS-Y2K-PIVOT            PIC 9(02)  VALUE 50.
       01  WS-TDSD-WORK.
           05  WS-TDSD-EXPECTED        PIC S9(11)V9(04) COMP-3.
           05  WS-TDSD-DIFF            PIC S9(11)V9(04) COMP-3.
           05  WS-TDSD-DIFF-CNT        PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-TDSD-DISPLAY-MAX     PIC S9(07)       COMP-3
                                                     VALUE +50.
      *----------------------------------------------------------------*
      * SOURCE / SIDE MATRIX FOR THE STATISTICS DISPLAY                *
      *   ROW 1 TC  2 CA  3 AJ      COL 1 OWNERSHIP  2 LOCATION        *
      *----------------------------------------------------------------*
       01  WS-SIDE-MATRIX.
           05  WS-SM-ROW OCCURS 3 TIMES.
               10  WS-SM-COL OCCURS 2 TIMES.
                   15  WS-SM-CNT       PIC S9(09) COMP-3.
                   15  WS-SM-QTY       PIC S9(15)V9(04) COMP-3.
       01  WS-SM-R                     PIC S9(04) COMP.
       01  WS-SM-C                     PIC S9(04) COMP.
       01  WS-SM-SOURCES               PIC X(06)  VALUE 'TCCAAJ'.
       01  WS-JOURNAL-BEFORE.
           05  WS-JB-TD-QTY            PIC S9(11)V9(04) COMP-3.
           05  WS-JB-AVG-COST          PIC S9(09)V9(06) COMP-3.
      *----------------------------------------------------------------*
      * OBSOLETE T+3 / T+2 CONSTANTS - LEFT FOR REFERENCE              *
      *----------------------------------------------------------------*
       01  WS-OLD-SETTLE-CONSTANTS.
           05  WS-T3-SETTLE-DAYS       PIC 9(01)  VALUE 3.
           05  WS-T2-SETTLE-DAYS       PIC 9(01)  VALUE 2.
           05  WS-T2-CUTOVER-DATE      PIC 9(08)  VALUE 19950607.
           05  WS-T1-CUTOVER-DATE      PIC 9(08)  VALUE 20240528.
      *----------------------------------------------------------------*
      * MARGIN INTEREST WORK AREA (CHG00544)                           *
      *----------------------------------------------------------------*
       01  WS-MARGIN-WORK.
           05  WS-MGN-DEBIT-BAL        PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-CALL-RATE        PIC S9(03)V9(06) COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-SPREAD           PIC S9(03)V9(06) COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-ANNUAL-RATE      PIC S9(03)V9(06) COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-DAILY-INT        PIC S9(13)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-MTD-INT          PIC S9(13)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-DAYS             PIC S9(03)       COMP-3
                                                     VALUE +1.
           05  WS-MGN-BASIS-DAYS       PIC S9(03)       COMP-3
                                                     VALUE +360.
           05  WS-MGN-ACCT-CNT         PIC S9(07)       COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-TIER-SUB         PIC S9(04)       COMP
                                                     VALUE ZERO.
           05  WS-MGN-HOUSE-MIN        PIC S9(03)V99    COMP-3
                                                     VALUE +25.00.
       01  WS-MARGIN-TIER-VALUES.
           05  FILLER  PIC X(16)  VALUE '0000010000002500'.
           05  FILLER  PIC X(16)  VALUE '0000050000002000'.
           05  FILLER  PIC X(16)  VALUE '0000100000001500'.
           05  FILLER  PIC X(16)  VALUE '0000500000001000'.
           05  FILLER  PIC X(16)  VALUE '9999999999000750'.
       01  WS-MARGIN-TIER-TABLE REDEFINES WS-MARGIN-TIER-VALUES.
           05  WS-MGN-TIER OCCURS 5 TIMES.
               10  WS-MGN-TIER-LIMIT   PIC 9(10).
               10  WS-MGN-TIER-SPREAD  PIC 9(02)V9(04).
       01  WS-BROKER-CALL-RATE         PIC 9(02)V9(04) VALUE 06.2500.
       01  WS-MARGIN-CALL-WORK.
           05  WS-MGN-LONG-MV          PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-EQUITY           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-EQUITY-PCT       PIC S9(05)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-REG-T-PCT        PIC S9(03)V99    COMP-3
                                                     VALUE +25.00.
           05  WS-MGN-HOUSE-PCT        PIC S9(03)V99    COMP-3
                                                     VALUE +30.00.
           05  WS-MGN-CALL-TYPE        PIC X(01)  VALUE SPACE.
           05  WS-MGN-CALL-AMT         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-MGN-CALL-CNT         PIC S9(07)       COMP-3
                                                     VALUE ZERO.
      *----------------------------------------------------------------*
      * ACTIVITY TYPE CLASSIFICATION                                   *
      *----------------------------------------------------------------*
       01  WS-CASH-ONLY-TYPES          PIC X(12)  VALUE 'DIVWHTCILMGC'.
       01  WS-CASH-ONLY-TABLE REDEFINES WS-CASH-ONLY-TYPES.
           05  WS-CASH-ONLY-TYPE OCCURS 4 TIMES INDEXED BY CO-IDX
                                       PIC X(03).
      *----------------------------------------------------------------*
      * COUNTERS                                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SKIP-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POSTED-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNPOSTED-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TC-POSTED-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CA-POSTED-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AJ-POSTED-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASH-ONLY-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-INSERT-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UPDATE-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FLATTEN-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SHORT-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-FROZEN-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-JRNL-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PEND-WRITE-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PEND-DUP-CNT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-PEND-RESTART-CNT     PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SETTLED-NOW-CNT      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-DUP-AIX-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-COLLISION-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CKPT-CNT             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOX-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AVG-ERR-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-YTD-RESET-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POS-WRITES           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-POS-REWRITES         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SELL-SHORT-WARN      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-LOC-LONG-WARN        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CCY-MISMATCH-WARN    PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TYPE-FILL-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-Y2K-WINDOW-CNT       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BY-TYPE-CNT OCCURS 14 TIMES
                                       PIC S9(09) COMP-3.
       01  WS-HASH-TOTALS.
           05  WS-IN-CASH-HASH         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-IN-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-TC-CASH-HASH         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TC-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-CA-CASH-HASH         PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-CA-QTY-HASH          PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-JRNL-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-JRNL-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-PEND-QTY-HASH        PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-PEND-CASH-HASH       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-REALIZED-TOTAL       PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
       01  WS-TYPE-NAMES               PIC X(42) VALUE
           'BUYSELSSLBCVXBYXSLDIVWHTSDVSPLCILMGOMGCQAJ'.
       01  WS-TYPE-NAME-TABLE REDEFINES WS-TYPE-NAMES.
           05  WS-TYPE-NAME OCCURS 14 TIMES INDEXED BY TN-IDX
                                       PIC X(03).
       01  WS-TYPE-SUB                 PIC S9(04) COMP.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * CONTROL CARD                                                   *
      *----------------------------------------------------------------*
       01  WS-PARM-CARD.
           05  WS-PARM-KEYWORD         PIC X(05).
           05  WS-PARM-REST            PIC X(75).
       01  WS-PARM-CKPT REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(05).
           05  WS-PARM-CKPT-VALUE      PIC 9(05).
           05  FILLER                  PIC X(70).
       01  WS-PARM-RERUN REDEFINES WS-PARM-CARD.
           05  FILLER                  PIC X(06).
           05  WS-PARM-RERUN-VALUE     PIC X(05).
           05  FILLER                  PIC X(69).
      *----------------------------------------------------------------*
      * COPYBOOKS                                                      *
      *----------------------------------------------------------------*
       COPY SRACTV.
       COPY SRPSTJ.
       COPY CMDATEW.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMCKLNK.
       COPY CMJILNK.
       COPY CMTSLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-CNT2            PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-QTY             PIC -ZZ,ZZZ,ZZZ,ZZ9.9999.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS-ACTIVITY THRU 2000-EXIT
               UNTIL END-OF-ACTIVITY.
           PERFORM 6000-FLUSH-BUFFER THRU 6000-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           GOBACK.
      *================================================================*
       1000-INITIALIZE.
      *================================================================*
           CALL 'CMASM01' USING JI-JOB-INFO.
           PERFORM 1100-READ-DATECARD THRU 1100-EXIT.
           PERFORM 1200-READ-PARMS THRU 1200-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'POSITION POSTING STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE DC-BUS-CCYY TO WS-BUS-CCYY.
           INITIALIZE WS-BY-TYPE-CNT (1) WS-BY-TYPE-CNT (2)
                      WS-BY-TYPE-CNT (3) WS-BY-TYPE-CNT (4)
                      WS-BY-TYPE-CNT (5) WS-BY-TYPE-CNT (6)
                      WS-BY-TYPE-CNT (7) WS-BY-TYPE-CNT (8)
                      WS-BY-TYPE-CNT (9) WS-BY-TYPE-CNT (10)
                      WS-BY-TYPE-CNT (11) WS-BY-TYPE-CNT (12)
                      WS-BY-TYPE-CNT (13) WS-BY-TYPE-CNT (14).
           PERFORM VARYING WS-SM-R FROM 1 BY 1 UNTIL WS-SM-R > 3
               PERFORM VARYING WS-SM-C FROM 1 BY 1 UNTIL WS-SM-C > 2
                   MOVE ZERO TO WS-SM-CNT (WS-SM-R WS-SM-C)
                                WS-SM-QTY (WS-SM-R WS-SM-C)
               END-PERFORM
           END-PERFORM.
           PERFORM 1300-INIT-CHECKPOINT THRU 1300-EXIT.
           PERFORM 1400-OPEN-FILES THRU 1400-EXIT.
           PERFORM 8000-READ-ACTIVITY THRU 8000-EXIT.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-READ-DATECARD.
      *----------------------------------------------------------------*
           OPEN INPUT DATECARD-FILE.
           IF WS-DATECARD-STATUS NOT = '00'
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1100-READ-DATECARD' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00'
           OR NOT DC-VALID-CARD
           OR DC-BUS-DATE NOT NUMERIC
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1100-READ-DATECARD' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * OPTIONAL CONTROL CARDS.  NO SYSIN DD (STATUS 35) IS NOT AN     *
      * ERROR - DEFAULTS APPLY.                                        *
      *----------------------------------------------------------------*
       1200-READ-PARMS.
           OPEN INPUT PARMCARD.
           IF WS-PARMCARD-STATUS NOT = '00'
               DISPLAY 'SRB200 NO CONTROL CARDS - DEFAULTS USED ('
                       WS-PARMCARD-STATUS ')'
               GO TO 1200-EXIT
           END-IF.
           PERFORM UNTIL END-OF-PARMS
               READ PARMCARD INTO WS-PARM-CARD
                   AT END
                       MOVE 'Y' TO WS-PARM-EOF-SW
                   NOT AT END
                       PERFORM 1210-APPLY-PARM THRU 1210-EXIT
               END-READ
           END-PERFORM.
           CLOSE PARMCARD.
       1200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1210-APPLY-PARM.
      *----------------------------------------------------------------*
           IF WS-PARM-CARD(1:1) = '*'
               GO TO 1210-EXIT
           END-IF.
           DISPLAY 'SRB200 CONTROL CARD: ' WS-PARM-CARD.
           EVALUATE TRUE
               WHEN WS-PARM-KEYWORD = 'CKPT='
                   IF WS-PARM-CKPT-VALUE NUMERIC
                   AND WS-PARM-CKPT-VALUE > ZERO
                       MOVE WS-PARM-CKPT-VALUE TO WS-CKPT-INTERVAL
                   ELSE
                       DISPLAY 'SRB200 INVALID CKPT= VALUE IGNORED'
                   END-IF
               WHEN WS-PARM-CARD(1:6) = 'RERUN='
                   IF WS-PARM-RERUN-VALUE = 'FORCE'
                       MOVE 'Y' TO WS-FORCE-RERUN-SW
                   END-IF
               WHEN WS-PARM-CARD(1:6) = 'DEBUG='
                   IF WS-PARM-CARD(7:1) = 'Y'
                       MOVE 'Y' TO WS-DEBUG-SW
                   END-IF
               WHEN OTHER
                   DISPLAY 'SRB200 UNKNOWN CONTROL CARD IGNORED'
           END-EVALUATE.
       1210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CHECKPOINT INITIALIZATION.  IF CMU070 FINDS AN IN-FLIGHT       *
      * RECORD FOR THIS PROGRAM/JOB/DATE WE ARE RESTARTING.            *
      *----------------------------------------------------------------*
       1300-INIT-CHECKPOINT.
           MOVE 'INIT'           TO CK-FUNCTION.
           MOVE WS-PROGRAM-ID    TO CK-PROGRAM.
           MOVE JI-JOBNAME       TO CK-JOBNAME.
           MOVE DC-BUS-DATE      TO CK-BUS-DATE.
           MOVE WS-CKPT-INTERVAL TO CK-INTERVAL.
           MOVE ZERO             TO CK-RECORD-COUNT.
           MOVE SPACES           TO CK-CURRENT-KEY.
           CALL 'CMU070' USING CK-CHECKPOINT-PARMS.
           IF CK-RETURN-CODE = 04
               DISPLAY 'SRB200 WARNING - STALE CHECKPOINT FROM A PRIOR '
                       'BUSINESS DATE WAS RESET BY CMU070'
           END-IF.
           IF CK-RETURN-CODE > 04
               MOVE 1009 TO AB-ABEND-CODE
               MOVE '1300-INIT-CHECKPOINT' TO AB-PARAGRAPH
               MOVE 'CHKPTFL' TO AB-DDNAME
               MOVE 'CMU070 INIT FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF CK-RESTARTING
               MOVE 'Y' TO WS-RESTART-SW WS-SKIPPING-SW
               MOVE CK-RESTART-KEY TO WS-RESTART-KEY
               DISPLAY 'SRB200 *** RESTART *** FROM KEY '
                       CK-RESTART-KEY
               MOVE CK-RESTART-COUNT TO WS-DISP-CNT
               DISPLAY 'SRB200 RECORDS ALREADY PROCESSED: '
                       WS-DISP-CNT
               MOVE 'WRIT'     TO AU-FUNCTION
               MOVE 'RESTART'  TO AU-EVENT
               MOVE 'W'        TO AU-SEVERITY
               MOVE CK-RESTART-KEY TO AU-KEY
               MOVE 'POSITION POSTING RESTARTED FROM CHECKPOINT'
                               TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           ELSE
               IF DC-RERUN AND NOT FORCE-RERUN
                   MOVE 1009 TO AB-ABEND-CODE
                   MOVE '1300-INIT-CHECKPOINT' TO AB-PARAGRAPH
                   MOVE 'RERUN FLAG SET BUT NO CHECKPOINT - USE RERUN=F
      -                 'ORCE' TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
           END-IF.
       1300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1400-OPEN-FILES.
      *----------------------------------------------------------------*
           OPEN INPUT ACTVIN-FILE.
           IF WS-ACTVIN-STATUS NOT = '00'
               MOVE 'ACTVIN' TO AB-DDNAME
               MOVE WS-ACTVIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1400-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1400-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN I-O PENDSETL-FILE.
           IF WS-PENDSETL-STATUS NOT = '00'
               MOVE 'PENDSETL' TO AB-DDNAME
               MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1400-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT POSTJRNL-FILE.
           IF WS-POSTJRNL-STATUS NOT = '00'
               MOVE 'POSTJRNL' TO AB-DDNAME
               MOVE WS-POSTJRNL-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1400-OPEN-FILES' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       1400-EXIT.
           EXIT.
      *================================================================*
      * MAIN LOOP - ONE ACTIVITY LEG                                   *
      *================================================================*
       2000-PROCESS-ACTIVITY.
           ADD 1 TO WS-READ-CNT.
           MOVE ACT-ACCT-NO   TO WS-IK-ACCT-NO.
           MOVE ACT-CUSIP     TO WS-IK-CUSIP.
           MOVE ACT-LOCATION  TO WS-IK-LOCATION.
           MOVE ACT-REF       TO WS-IK-REF.
           MOVE ACT-LEG-NO    TO WS-IK-LEG-NO.
      *    ---- RESTART: BYPASS WHAT WAS ALREADY CHECKPOINTED -------
           IF SKIPPING-TO-RESTART
               IF WS-INPUT-KEY NOT > WS-RESTART-KEY
                   ADD 1 TO WS-SKIP-CNT
                   GO TO 2000-READ-NEXT
               ELSE
                   MOVE 'N' TO WS-SKIPPING-SW
                   DISPLAY 'SRB200 RESTART POINT REACHED AT RECORD '
                           WS-READ-CNT
               END-IF
           END-IF.
           PERFORM 2050-ACCUMULATE-INPUT THRU 2050-EXIT.
           PERFORM 2150-Y2K-DATE-FIX THRU 2150-EXIT.
           PERFORM 2100-CLASSIFY-LEG THRU 2100-EXIT.
           IF LEG-NOT-POSTABLE
               ADD 1 TO WS-UNPOSTED-CNT
               MOVE 8 TO WS-RETURN-CODE
               DISPLAY 'SRB200 UNPOSTABLE LEG ' ACT-REF ' '
                       ACT-LEG-NO ' TYPE ' ACT-TYPE
               GO TO 2000-CHECKPOINT
           END-IF.
           IF CASH-ONLY-LEG
               PERFORM 2600-POST-CASH-ONLY THRU 2600-EXIT
           ELSE
               PERFORM 2200-POST-SHARE-LEG THRU 2200-EXIT
           END-IF.
           PERFORM 2900-COUNT-POSTED THRU 2900-EXIT.
       2000-CHECKPOINT.
           PERFORM 6800-TAKE-CHECKPOINT THRU 6800-EXIT.
       2000-READ-NEXT.
           PERFORM 8000-READ-ACTIVITY THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2050-ACCUMULATE-INPUT.
      *----------------------------------------------------------------*
           MOVE ACT-QTY-CHANGE TO WS-QTY.
           IF WS-QTY < ZERO
               COMPUTE WS-ABS-QTY = WS-QTY * -1
           ELSE
               MOVE WS-QTY TO WS-ABS-QTY
           END-IF.
           ADD ACT-CASH-CHANGE TO WS-IN-CASH-HASH.
           ADD WS-ABS-QTY      TO WS-IN-QTY-HASH.
       2050-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * OWNERSHIP OR LOCATION SIDE, CASH ONLY OR SHARE MOVEMENT        *
      *----------------------------------------------------------------*
       2100-CLASSIFY-LEG.
           MOVE 'Y' TO WS-POSTABLE-SW.
           MOVE 'N' TO WS-CASH-ONLY-SW.
           IF ACT-ACCT-TYPE = 'ST'
               SET LOCATION-LEG TO TRUE
           ELSE
               SET OWNERSHIP-LEG TO TRUE
           END-IF.
           SET CO-IDX TO 1.
           SEARCH WS-CASH-ONLY-TYPE
               AT END
                   CONTINUE
               WHEN WS-CASH-ONLY-TYPE (CO-IDX) = ACT-TYPE
                   MOVE 'Y' TO WS-CASH-ONLY-SW
           END-SEARCH.
           SET TN-IDX TO 1.
           SEARCH WS-TYPE-NAME
               AT END
                   MOVE 'N' TO WS-POSTABLE-SW
               WHEN WS-TYPE-NAME (TN-IDX) = ACT-TYPE
                   SET WS-TYPE-SUB TO TN-IDX
           END-SEARCH.
           IF ACT-QTY-CHANGE NOT NUMERIC
           OR ACT-CASH-CHANGE NOT NUMERIC
           OR ACT-COST-CHANGE NOT NUMERIC
               MOVE 'N' TO WS-POSTABLE-SW
           END-IF.
      *    1995-03-06 LFM BOX CERTIFICATES MOVED TO DTC
      *    IF ACT-LOCATION = 'BOX ' AND BOX-CONVERT-ON
      *        MOVE 'DTC ' TO ACT-LOCATION
      *    END-IF.
           IF ACT-LOCATION = 'BOX '
               ADD 1 TO WS-BOX-CNT
           END-IF.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 1998-11-02 TLM - LEGS FROM THE OLD ADJUSTMENT SCREENS STILL    *
      * CARRY YYMMDD IN THE LOW 6 DIGITS.  WINDOW THEM (PIVOT 50).     *
      *----------------------------------------------------------------*
       2150-Y2K-DATE-FIX.
           IF ACT-TRADE-DATE < 01000000
               MOVE ACT-TRADE-DATE TO WS-Y2K-DATE-IN
               PERFORM 2160-WINDOW-DATE THRU 2160-EXIT
               MOVE WS-Y2K-DATE-OUT TO ACT-TRADE-DATE
           END-IF.
           IF ACT-SETTLE-DATE < 01000000
           AND ACT-SETTLE-DATE NOT = ZERO
               MOVE ACT-SETTLE-DATE TO WS-Y2K-DATE-IN
               PERFORM 2160-WINDOW-DATE THRU 2160-EXIT
               MOVE WS-Y2K-DATE-OUT TO ACT-SETTLE-DATE
           END-IF.
       2150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2160-WINDOW-DATE.
      *----------------------------------------------------------------*
           ADD 1 TO WS-Y2K-WINDOW-CNT.
           MOVE WS-Y2K-YY TO WS-Y2K-OUT-YY.
           MOVE WS-Y2K-MM TO WS-Y2K-OUT-MM.
           MOVE WS-Y2K-DD TO WS-Y2K-OUT-DD.
           IF WS-Y2K-YY < WS-Y2K-PIVOT
               MOVE 20 TO WS-Y2K-OUT-CC
           ELSE
               MOVE 19 TO WS-Y2K-OUT-CC
           END-IF.
       2160-EXIT.
           EXIT.
      *================================================================*
      * SHARE MOVEMENT LEG                                             *
      *================================================================*
       2200-POST-SHARE-LEG.
           MOVE ACT-ACCT-NO  TO WS-CPK-ACCT-NO.
           MOVE ACT-CUSIP    TO WS-CPK-CUSIP.
           MOVE ACT-LOCATION TO WS-CPK-LOCATION.
           IF POSITION-BUFFERED
           AND WS-CUR-POS-KEY NOT = WS-BUF-POS-KEY
               PERFORM 6000-FLUSH-BUFFER THRU 6000-EXIT
           END-IF.
           IF BUFFER-EMPTY
               PERFORM 3000-LOAD-POSITION THRU 3000-EXIT
           END-IF.
           MOVE WS-BUF-TD-QTY   TO WS-JB-TD-QTY WS-OLD-TD-QTY.
           MOVE WS-BUF-AVG-COST TO WS-JB-AVG-COST WS-OLD-AVG-COST.
           MOVE WS-BUF-COST-BASIS TO WS-OLD-COST-BASIS.
           MOVE ZERO TO WS-REALIZED-PL.
           IF WS-BUF-FROZEN
               ADD 1 TO WS-FROZEN-CNT
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
               DISPLAY 'SRB200 WARNING - POSTING TO FROZEN POSITION '
                       WS-BUF-KEY
           END-IF.
           PERFORM 3500-YTD-ROLL THRU 3500-EXIT.
           PERFORM 2300-POSITION-EDITS THRU 2300-EXIT.
      *    ---- TRADE DATE QUANTITY ---------------------------------
           COMPUTE WS-NEW-TD-QTY = WS-OLD-TD-QTY + ACT-QTY-CHANGE.
      *    ---- COST / P&L (OWNERSHIP SIDE ONLY) --------------------
           IF OWNERSHIP-LEG
               PERFORM 4000-COST-AND-PL THRU 4000-EXIT
           END-IF.
           MOVE WS-NEW-TD-QTY TO WS-BUF-TD-QTY.
      *    ---- SETTLED QUANTITY OR PENDING -------------------------
           PERFORM 5000-SETTLEMENT THRU 5000-EXIT.
      *    ---- FLAGS -----------------------------------------------
           PERFORM 5500-SET-FLAGS THRU 5500-EXIT.
           MOVE DC-BUS-DATE TO WS-BUF-LAST-ACTV-DATE.
           MOVE JI-JOBNAME  TO WS-BUF-LAST-UPD-JOB.
      *    ---- JOURNAL ---------------------------------------------
           PERFORM 6500-WRITE-JOURNAL THRU 6500-EXIT.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * POSITION LEVEL EDITS - WARNINGS ONLY, THE LEG IS ALWAYS POSTED *
      *----------------------------------------------------------------*
       2300-POSITION-EDITS.
           IF WS-BUF-SEC-TYPE = SPACES OR LOW-VALUES
               MOVE ACT-SEC-TYPE TO WS-BUF-SEC-TYPE
               ADD 1 TO WS-TYPE-FILL-CNT
           END-IF.
           IF WS-BUF-ACCT-TYPE = SPACES OR LOW-VALUES
               MOVE ACT-ACCT-TYPE TO WS-BUF-ACCT-TYPE
               ADD 1 TO WS-TYPE-FILL-CNT
           END-IF.
           IF WS-BUF-CCY = SPACES OR LOW-VALUES
               MOVE ACT-CCY TO WS-BUF-CCY
           ELSE
               IF WS-BUF-CCY NOT = ACT-CCY
                   ADD 1 TO WS-CCY-MISMATCH-WARN
                   DISPLAY 'SRB200 CCY MISMATCH ' WS-BUF-KEY ' POS '
                           WS-BUF-CCY ' ACT ' ACT-CCY
               END-IF
           END-IF.
      *    PLAIN SELL THAT TAKES AN OWNER POSITION SHORT
           IF OWNERSHIP-LEG AND ACT-SELL
               IF WS-OLD-TD-QTY + ACT-QTY-CHANGE < ZERO
                   ADD 1 TO WS-SELL-SHORT-WARN
                   DISPLAY 'SRB200 SELL CREATES SHORT ' WS-BUF-KEY
                           ' REF ' ACT-REF
               END-IF
           END-IF.
      *    STREET SIDE SHOULD NEVER GO LONG
           IF LOCATION-LEG
               IF WS-OLD-TD-QTY + ACT-QTY-CHANGE > ZERO
                   ADD 1 TO WS-LOC-LONG-WARN
                   DISPLAY 'SRB200 LOCATION ROW WENT LONG '
                           WS-BUF-KEY
               END-IF
           END-IF.
       2300-EXIT.
           EXIT.
      *================================================================*
      * CASH ONLY LEG (DIV/WHT/CIL/MGC).  POSITION IS NOT CREATED -    *
      * ONLY THE LAST ACTIVITY DATE IS TOUCHED IF IT EXISTS.  PENDING  *
      * ROW STILL WRITTEN WHEN THE CASH IS PAYABLE IN THE FUTURE.      *
      *================================================================*
       2600-POST-CASH-ONLY.
           ADD 1 TO WS-CASH-ONLY-CNT.
           MOVE ACT-ACCT-NO  TO WS-CPK-ACCT-NO.
           MOVE ACT-CUSIP    TO WS-CPK-CUSIP.
           MOVE ACT-LOCATION TO WS-CPK-LOCATION.
           MOVE ZERO TO WS-REALIZED-PL.
           MOVE 'N'  TO WS-POST-ACTION.
           IF POSITION-BUFFERED
           AND WS-CUR-POS-KEY = WS-BUF-POS-KEY
               MOVE WS-BUF-TD-QTY   TO WS-JB-TD-QTY
               MOVE WS-BUF-AVG-COST TO WS-JB-AVG-COST
               MOVE DC-BUS-DATE     TO WS-BUF-LAST-ACTV-DATE
               MOVE 'U' TO WS-POST-ACTION
           ELSE
               MOVE WS-CUR-POS-KEY TO POS-KEY
               READ POSMAST-FILE
               EVALUATE TRUE
                   WHEN POSMAST-OK
                   WHEN POSMAST-DUP-AIX
                       MOVE POS-TD-QTY   TO WS-JB-TD-QTY
                       MOVE POS-AVG-COST TO WS-JB-AVG-COST
                       MOVE DC-BUS-DATE  TO POS-LAST-ACTV-DATE
                       MOVE JI-JOBNAME   TO POS-LAST-UPD-JOB
                       PERFORM 6200-REWRITE-POSITION THRU 6200-EXIT
                       MOVE 'U' TO WS-POST-ACTION
                   WHEN POSMAST-NOTFND
                       MOVE ZERO TO WS-JB-TD-QTY WS-JB-AVG-COST
                   WHEN OTHER
                       MOVE 'POSMAST' TO AB-DDNAME
                       MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '2600-POST-CASH-ONLY' TO AB-PARAGRAPH
                       MOVE WS-CUR-POS-KEY TO AB-KEY
                       MOVE 'POSITION READ FAILED' TO AB-MESSAGE
                       GO TO 9999-ABEND
               END-EVALUATE
           END-IF.
           MOVE 'N' TO WS-SETTLE-NOW-SW.
           IF ACT-NO-SETTLEMENT
           OR ACT-SETTLE-DATE NOT > DC-BUS-DATE
               MOVE 'Y' TO WS-SETTLE-NOW-SW
               ADD 1 TO WS-SETTLED-NOW-CNT
           ELSE
               PERFORM 5200-WRITE-PENDING THRU 5200-EXIT
           END-IF.
           PERFORM 6600-BUILD-JOURNAL THRU 6600-EXIT.
           MOVE WS-JB-TD-QTY   TO PSJ-AFTER-TD-QTY.
           MOVE WS-JB-AVG-COST TO PSJ-AFTER-AVG-COST.
           MOVE WS-POST-ACTION TO PSJ-POST-ACTION.
           PERFORM 6700-PUT-JOURNAL THRU 6700-EXIT.
       2600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2900-COUNT-POSTED.
      *----------------------------------------------------------------*
           ADD 1 TO WS-POSTED-CNT.
           EVALUATE TRUE
               WHEN ACT-FROM-TRADE        MOVE 1 TO WS-SM-R
               WHEN ACT-FROM-CORP-ACTION  MOVE 2 TO WS-SM-R
               WHEN OTHER                 MOVE 3 TO WS-SM-R
           END-EVALUATE.
           IF LOCATION-LEG
               MOVE 2 TO WS-SM-C
           ELSE
               MOVE 1 TO WS-SM-C
           END-IF.
           ADD 1          TO WS-SM-CNT (WS-SM-R WS-SM-C).
           ADD WS-ABS-QTY TO WS-SM-QTY (WS-SM-R WS-SM-C).
           ADD 1 TO WS-BY-TYPE-CNT (WS-TYPE-SUB).
           EVALUATE TRUE
               WHEN ACT-FROM-TRADE
                   ADD 1 TO WS-TC-POSTED-CNT
                   ADD ACT-CASH-CHANGE TO WS-TC-CASH-HASH
                   ADD WS-ABS-QTY      TO WS-TC-QTY-HASH
               WHEN ACT-FROM-CORP-ACTION
                   ADD 1 TO WS-CA-POSTED-CNT
                   ADD ACT-CASH-CHANGE TO WS-CA-CASH-HASH
                   ADD WS-ABS-QTY      TO WS-CA-QTY-HASH
               WHEN OTHER
                   ADD 1 TO WS-AJ-POSTED-CNT
           END-EVALUATE.
       2900-EXIT.
           EXIT.
      *================================================================*
      * LOAD POSITION INTO THE BUFFER - READ BY KEY, BUILD NEW IF 23   *
      *================================================================*
       3000-LOAD-POSITION.
           MOVE WS-CUR-POS-KEY TO POS-KEY.
           READ POSMAST-FILE.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   MOVE POS-POSITION-REC TO WS-BUF-POSITION
                   SET BUFFER-IS-EXISTING TO TRUE
               WHEN POSMAST-DUP-AIX
                   ADD 1 TO WS-DUP-AIX-CNT
                   MOVE POS-POSITION-REC TO WS-BUF-POSITION
                   SET BUFFER-IS-EXISTING TO TRUE
               WHEN POSMAST-NOTFND
                   PERFORM 3100-BUILD-NEW-POSITION THRU 3100-EXIT
                   SET BUFFER-IS-NEW TO TRUE
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '3000-LOAD-POSITION' TO AB-PARAGRAPH
                   MOVE WS-CUR-POS-KEY TO AB-KEY
                   MOVE 'POSITION READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           PERFORM 3200-CHECK-BUFFER-NUMERIC THRU 3200-EXIT.
           MOVE WS-CUR-POS-KEY TO WS-BUF-POS-KEY.
           MOVE 'Y' TO WS-BUFFER-SW.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * NEW POSITION.  VALUATION FIELDS ARE SET BY SRB400.             *
      *----------------------------------------------------------------*
       3100-BUILD-NEW-POSITION.
           MOVE SPACES           TO WS-BUF-POSITION.
           MOVE ACT-ACCT-NO      TO WS-BUF-ACCT-NO.
           MOVE ACT-CUSIP        TO WS-BUF-CUSIP.
           MOVE ACT-LOCATION     TO WS-BUF-LOCATION.
           MOVE ACT-SEC-TYPE     TO WS-BUF-SEC-TYPE.
           MOVE ACT-ACCT-TYPE    TO WS-BUF-ACCT-TYPE.
           MOVE ACT-CCY          TO WS-BUF-CCY.
           MOVE ZERO             TO WS-BUF-TD-QTY
                                    WS-BUF-SD-QTY
                                    WS-BUF-PEND-IN-QTY
                                    WS-BUF-PEND-OUT-QTY
                                    WS-BUF-AVG-COST
                                    WS-BUF-COST-BASIS
                                    WS-BUF-REALIZED-PL-YTD.
           MOVE DC-BUS-DATE      TO WS-BUF-OPEN-DATE
                                    WS-BUF-LAST-ACTV-DATE.
           MOVE JI-JOBNAME       TO WS-BUF-LAST-UPD-JOB.
           MOVE 'N'              TO WS-BUF-SHORT-FLAG.
           MOVE 'O'              TO WS-BUF-STATUS.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GUARD AGAINST BAD PACKED DATA IN THE QUANTITY / COST FIELDS    *
      * (S0C7).  VALUATION FIELDS ARE NOT TOUCHED HERE.                *
      *----------------------------------------------------------------*
       3200-CHECK-BUFFER-NUMERIC.
           IF WS-BUF-TD-QTY NOT NUMERIC
           OR WS-BUF-SD-QTY NOT NUMERIC
           OR WS-BUF-PEND-IN-QTY NOT NUMERIC
           OR WS-BUF-PEND-OUT-QTY NOT NUMERIC
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '3200-CHECK-BUFFER-NUMERIC' TO AB-PARAGRAPH
               MOVE WS-CUR-POS-KEY TO AB-KEY
               MOVE 'POSITION QUANTITY FIELDS NOT NUMERIC'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF WS-BUF-AVG-COST NOT NUMERIC
               MOVE ZERO TO WS-BUF-AVG-COST
           END-IF.
           IF WS-BUF-COST-BASIS NOT NUMERIC
               MOVE ZERO TO WS-BUF-COST-BASIS
           END-IF.
           IF WS-BUF-REALIZED-PL-YTD NOT NUMERIC
               MOVE ZERO TO WS-BUF-REALIZED-PL-YTD
           END-IF.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * 1999-06-14 TLM - REALIZED P&L YTD RESETS ON FIRST ACTIVITY OF  *
      * A NEW YEAR (NO SEPARATE YEAR-END JOB).                         *
      *----------------------------------------------------------------*
       3500-YTD-ROLL.
           MOVE 'N' TO WS-YTD-RESET-SW.
           IF WS-BUF-LAST-ACTV-DATE NUMERIC
               MOVE WS-BUF-LAST-ACTV-DATE (1:4) TO WS-LAST-CCYY
               IF WS-LAST-CCYY < WS-BUS-CCYY
                   MOVE ZERO TO WS-BUF-REALIZED-PL-YTD
                   MOVE 'Y' TO WS-YTD-RESET-SW
                   ADD 1 TO WS-YTD-RESET-CNT
               END-IF
           END-IF.
       3500-EXIT.
           EXIT.
      *================================================================*
      * COST BASIS, AVERAGE COST AND REALIZED P&L                      *
      *================================================================*
       4000-COST-AND-PL.
           PERFORM 4050-DETERMINE-TRADE-AMT THRU 4050-EXIT.
           EVALUATE ACT-TYPE
               WHEN 'BUY'
               WHEN 'BCV'
                   PERFORM 4100-POST-PURCHASE THRU 4100-EXIT
               WHEN 'SEL'
               WHEN 'SSL'
                   PERFORM 4200-POST-SALE THRU 4200-EXIT
               WHEN 'XBY'
                   PERFORM 4500-POST-CXL-BUY THRU 4500-EXIT
               WHEN 'XSL'
                   PERFORM 4600-POST-CXL-SELL THRU 4600-EXIT
               WHEN 'SDV'
               WHEN 'SPL'
                   PERFORM 4700-POST-DISTRIBUTION THRU 4700-EXIT
               WHEN 'MGO'
                   PERFORM 4800-POST-MERGER-OUT THRU 4800-EXIT
               WHEN 'QAJ'
                   PERFORM 4900-POST-QTY-ADJUST THRU 4900-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
           IF WS-REALIZED-PL NOT = ZERO
               ADD WS-REALIZED-PL TO WS-BUF-REALIZED-PL-YTD
                                     WS-REALIZED-TOTAL
           END-IF.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TRADE AMOUNT: COST CHANGE IF SUPPLIED, ELSE NET CASH, ELSE     *
      * QTY X PRICE (BONDS QUOTED IN PERCENT OF PAR).                  *
      *----------------------------------------------------------------*
       4050-DETERMINE-TRADE-AMT.
           EVALUATE TRUE
               WHEN ACT-COST-CHANGE NOT = ZERO
                   IF ACT-COST-CHANGE < ZERO
                       COMPUTE WS-TRADE-AMT = ACT-COST-CHANGE * -1
                   ELSE
                       MOVE ACT-COST-CHANGE TO WS-TRADE-AMT
                   END-IF
               WHEN ACT-CASH-CHANGE NOT = ZERO
                   IF ACT-CASH-CHANGE < ZERO
                       COMPUTE WS-TRADE-AMT = ACT-CASH-CHANGE * -1
                   ELSE
                       MOVE ACT-CASH-CHANGE TO WS-TRADE-AMT
                   END-IF
               WHEN OTHER
                   IF ACT-SEC-TYPE = 'CB' OR 'MU' OR 'GV'
                       MOVE .0100 TO WS-PRICE-MULT
                   ELSE
                       MOVE 1.0000 TO WS-PRICE-MULT
                   END-IF
                   COMPUTE WS-TRADE-AMT ROUNDED =
                       WS-ABS-QTY * ACT-PRICE * WS-PRICE-MULT
           END-EVALUATE.
       4050-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PURCHASE (BUY, BUY TO COVER).                                  *
      * LONG OR FLAT: ADD TO COST, RECOMPUTE WEIGHTED AVERAGE.         *
      * SHORT: COVER FIRST AT SHORT AVERAGE (REALIZED), REMAINDER      *
      * OPENS A LONG AT THE TRADE UNIT COST.                           *
      *----------------------------------------------------------------*
       4100-POST-PURCHASE.
           IF WS-OLD-TD-QTY NOT < ZERO
               ADD WS-TRADE-AMT TO WS-BUF-COST-BASIS
               PERFORM 4950-RECALC-AVERAGE THRU 4950-EXIT
               GO TO 4100-EXIT
           END-IF.
      *    ---- COVERING A SHORT ------------------------------------
           COMPUTE WS-CLOSE-QTY = WS-OLD-TD-QTY * -1.
           IF WS-CLOSE-QTY > WS-ABS-QTY
               MOVE WS-ABS-QTY TO WS-CLOSE-QTY
           END-IF.
           COMPUTE WS-OPEN-QTY = WS-ABS-QTY - WS-CLOSE-QTY.
           IF WS-OPEN-QTY = ZERO
               MOVE WS-TRADE-AMT TO WS-CLOSE-AMT
               MOVE ZERO TO WS-OPEN-AMT
           ELSE
               COMPUTE WS-CLOSE-AMT ROUNDED =
                   WS-TRADE-AMT * WS-CLOSE-QTY / WS-ABS-QTY
               COMPUTE WS-OPEN-AMT = WS-TRADE-AMT - WS-CLOSE-AMT
           END-IF.
      *    SHORT PROCEEDS RELIEVED AT AVERAGE SHORT PRICE
           COMPUTE WS-COST-RELIEVED ROUNDED =
               WS-OLD-AVG-COST * WS-CLOSE-QTY.
           COMPUTE WS-REALIZED-PL = WS-COST-RELIEVED - WS-CLOSE-AMT.
           IF WS-OPEN-QTY > ZERO
               MOVE WS-OPEN-AMT TO WS-BUF-COST-BASIS
               COMPUTE WS-BUF-AVG-COST ROUNDED =
                   WS-OPEN-AMT / WS-OPEN-QTY
                   ON SIZE ERROR
                       ADD 1 TO WS-AVG-ERR-CNT
                       MOVE ZERO TO WS-BUF-AVG-COST
               END-COMPUTE
           ELSE
               IF WS-NEW-TD-QTY = ZERO
                   MOVE ZERO TO WS-BUF-COST-BASIS WS-BUF-AVG-COST
               ELSE
                   COMPUTE WS-BUF-COST-BASIS ROUNDED =
                       WS-OLD-AVG-COST * WS-NEW-TD-QTY
               END-IF
           END-IF.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SALE (SELL, SHORT SELL).                                       *
      * LONG: RELIEVE COST AT AVERAGE, REALIZE P&L; ANY EXCESS OPENS   *
      * A SHORT AT THE TRADE UNIT PRICE.                               *
      * FLAT OR SHORT: ADD TO SHORT (NEGATIVE COST BASIS).             *
      *----------------------------------------------------------------*
       4200-POST-SALE.
           IF WS-OLD-TD-QTY NOT > ZERO
               SUBTRACT WS-TRADE-AMT FROM WS-BUF-COST-BASIS
               PERFORM 4950-RECALC-AVERAGE THRU 4950-EXIT
               GO TO 4200-EXIT
           END-IF.
           MOVE WS-OLD-TD-QTY TO WS-CLOSE-QTY.
           IF WS-CLOSE-QTY > WS-ABS-QTY
               MOVE WS-ABS-QTY TO WS-CLOSE-QTY
           END-IF.
           COMPUTE WS-OPEN-QTY = WS-ABS-QTY - WS-CLOSE-QTY.
           IF WS-OPEN-QTY = ZERO
               MOVE WS-TRADE-AMT TO WS-CLOSE-AMT
               MOVE ZERO TO WS-OPEN-AMT
           ELSE
               COMPUTE WS-CLOSE-AMT ROUNDED =
                   WS-TRADE-AMT * WS-CLOSE-QTY / WS-ABS-QTY
               COMPUTE WS-OPEN-AMT = WS-TRADE-AMT - WS-CLOSE-AMT
           END-IF.
           COMPUTE WS-COST-RELIEVED ROUNDED =
               WS-OLD-AVG-COST * WS-CLOSE-QTY.
           COMPUTE WS-REALIZED-PL = WS-CLOSE-AMT - WS-COST-RELIEVED.
           IF WS-OPEN-QTY > ZERO
               COMPUTE WS-BUF-COST-BASIS = WS-OPEN-AMT * -1
               COMPUTE WS-BUF-AVG-COST ROUNDED =
                   WS-OPEN-AMT / WS-OPEN-QTY
                   ON SIZE ERROR
                       ADD 1 TO WS-AVG-ERR-CNT
                       MOVE ZERO TO WS-BUF-AVG-COST
               END-COMPUTE
           ELSE
               IF WS-NEW-TD-QTY = ZERO
                   MOVE ZERO TO WS-BUF-COST-BASIS WS-BUF-AVG-COST
               ELSE
                   SUBTRACT WS-COST-RELIEVED FROM WS-BUF-COST-BASIS
               END-IF
           END-IF.
       4200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CANCEL OF A BUY - TAKE THE COST BACK OUT, NO P&L.              *
      *----------------------------------------------------------------*
       4500-POST-CXL-BUY.
           SUBTRACT WS-TRADE-AMT FROM WS-BUF-COST-BASIS.
           PERFORM 4950-RECALC-AVERAGE THRU 4950-EXIT.
       4500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CANCEL OF A SELL - RESTORE COST AT CURRENT AVERAGE AND BACK    *
      * OUT THE P&L THAT THE ORIGINAL SALE REALIZED (APPROXIMATE WHEN  *
      * THE AVERAGE HAS MOVED SINCE - SEE CHG08811 NOTES).             *
      *----------------------------------------------------------------*
       4600-POST-CXL-SELL.
           COMPUTE WS-COST-RELIEVED ROUNDED =
               WS-OLD-AVG-COST * WS-ABS-QTY.
           IF WS-OLD-TD-QTY < ZERO AND WS-NEW-TD-QTY NOT > ZERO
      *        SHORT SALE BEING CANCELLED - NO REALIZED P&L
               ADD WS-TRADE-AMT TO WS-BUF-COST-BASIS
           ELSE
               ADD WS-COST-RELIEVED TO WS-BUF-COST-BASIS
               COMPUTE WS-REALIZED-PL =
                   (WS-TRADE-AMT - WS-COST-RELIEVED) * -1
           END-IF.
           IF WS-NEW-TD-QTY = ZERO
               MOVE ZERO TO WS-BUF-COST-BASIS WS-BUF-AVG-COST
           ELSE
               IF WS-OLD-AVG-COST = ZERO
                   PERFORM 4950-RECALC-AVERAGE THRU 4950-EXIT
               END-IF
           END-IF.
       4600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * STOCK DIVIDEND / SPLIT - SAME TOTAL COST SPREAD OVER MORE      *
      * SHARES.                                                        *
      *----------------------------------------------------------------*
       4700-POST-DISTRIBUTION.
           IF ACT-COST-CHANGE NOT = ZERO
               ADD ACT-COST-CHANGE TO WS-BUF-COST-BASIS
           END-IF.
           PERFORM 4950-RECALC-AVERAGE THRU 4950-EXIT.
       4700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MERGER SHARES OUT - COST RELIEVED AT AVERAGE.  THE CASH LEG    *
      * (MGC) IS POSTED SEPARATELY; NO REALIZED P&L COMPUTED HERE.     *
      *----------------------------------------------------------------*
       4800-POST-MERGER-OUT.
           IF WS-NEW-TD-QTY = ZERO
               MOVE ZERO TO WS-BUF-COST-BASIS WS-BUF-AVG-COST
           ELSE
               COMPUTE WS-COST-RELIEVED ROUNDED =
                   WS-OLD-AVG-COST * WS-ABS-QTY
               SUBTRACT WS-COST-RELIEVED FROM WS-BUF-COST-BASIS
           END-IF.
       4800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MANUAL QUANTITY ADJUSTMENT - COST CHANGE AS SUPPLIED.          *
      *----------------------------------------------------------------*
       4900-POST-QTY-ADJUST.
           ADD ACT-COST-CHANGE TO WS-BUF-COST-BASIS.
           PERFORM 4950-RECALC-AVERAGE THRU 4950-EXIT.
       4900-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4950-RECALC-AVERAGE.
      *----------------------------------------------------------------*
           IF WS-NEW-TD-QTY = ZERO
               MOVE ZERO TO WS-BUF-AVG-COST WS-BUF-COST-BASIS
           ELSE
               COMPUTE WS-BUF-AVG-COST ROUNDED =
                   WS-BUF-COST-BASIS / WS-NEW-TD-QTY
                   ON SIZE ERROR
                       ADD 1 TO WS-AVG-ERR-CNT
                       DISPLAY 'SRB200 AVG COST SIZE ERROR '
                               WS-BUF-KEY
               END-COMPUTE
           END-IF.
       4950-EXIT.
           EXIT.
      *================================================================*
      * SETTLEMENT - SETTLED QTY NOW, OR PENDING ROW FOR SRB250        *
      *================================================================*
       5000-SETTLEMENT.
           MOVE 'N' TO WS-SETTLE-NOW-SW.
           IF ACT-NO-SETTLEMENT
           OR ACT-SETTLE-DATE NOT > DC-BUS-DATE
               MOVE 'Y' TO WS-SETTLE-NOW-SW
           END-IF.
           IF SETTLES-NOW
               ADD ACT-QTY-CHANGE TO WS-BUF-SD-QTY
               ADD 1 TO WS-SETTLED-NOW-CNT
           ELSE
               IF ACT-QTY-CHANGE > ZERO
                   ADD ACT-QTY-CHANGE TO WS-BUF-PEND-IN-QTY
               ELSE
                   SUBTRACT ACT-QTY-CHANGE FROM WS-BUF-PEND-OUT-QTY
               END-IF
               PERFORM 5200-WRITE-PENDING THRU 5200-EXIT
           END-IF.
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * PENDING SETTLEMENT ROW.  22 ON A RESTART MEANS THE ROW WAS     *
      * WRITTEN BEFORE THE FAILURE - ACCEPT IT.                        *
      *----------------------------------------------------------------*
       5200-WRITE-PENDING.
           MOVE SPACES          TO PND-PENDING-REC.
           MOVE ACT-SETTLE-DATE TO PND-SETTLE-DATE.
           MOVE ACT-REF         TO PND-REF.
           MOVE ACT-LEG-NO      TO PND-LEG-NO.
           MOVE ACT-ACCT-NO     TO PND-ACCT-NO.
           MOVE ACT-CUSIP       TO PND-CUSIP.
           MOVE ACT-LOCATION    TO PND-LOCATION.
           MOVE ACT-TYPE        TO PND-ACT-TYPE.
           MOVE ACT-QTY-CHANGE  TO PND-QTY.
           MOVE ACT-CASH-CHANGE TO PND-CASH.
           MOVE ACT-CCY         TO PND-CCY.
           MOVE ACT-TRADE-DATE  TO PND-TRADE-DATE.
           MOVE 'O'             TO PND-STATUS.
           MOVE ZERO            TO PND-FAIL-DAYS.
           WRITE PND-PENDING-REC.
           EVALUATE TRUE
               WHEN PENDSETL-OK
                   ADD 1 TO WS-PEND-WRITE-CNT
                   ADD WS-ABS-QTY TO WS-PEND-QTY-HASH
                   ADD ACT-CASH-CHANGE TO WS-PEND-CASH-HASH
               WHEN PENDSETL-DUP-AIX
                   ADD 1 TO WS-PEND-WRITE-CNT WS-DUP-AIX-CNT
                   ADD WS-ABS-QTY TO WS-PEND-QTY-HASH
                   ADD ACT-CASH-CHANGE TO WS-PEND-CASH-HASH
               WHEN PENDSETL-DUP-KEY
                   IF RESTART-IN-PROGRESS
                       ADD 1 TO WS-PEND-RESTART-CNT
                   ELSE
                       ADD 1 TO WS-PEND-DUP-CNT
                       IF WS-RETURN-CODE < 4
                           MOVE 4 TO WS-RETURN-CODE
                       END-IF
                       DISPLAY 'SRB200 DUPLICATE PENDING ROW NOT '
                               'WRITTEN ' PND-KEY
                   END-IF
               WHEN OTHER
                   MOVE 'PENDSETL' TO AB-DDNAME
                   MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '5200-WRITE-PENDING' TO AB-PARAGRAPH
                   MOVE PND-KEY TO AB-KEY
                   MOVE 'PENDING SETTLEMENT WRITE FAILED'
                                   TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       5200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FLAT / SHORT / OPEN.  FROZEN STAYS FROZEN.                     *
      *----------------------------------------------------------------*
       5500-SET-FLAGS.
           IF OWNERSHIP-LEG AND WS-BUF-TD-QTY < ZERO
               IF WS-BUF-SHORT-FLAG NOT = 'Y'
                   ADD 1 TO WS-SHORT-CNT
               END-IF
               MOVE 'Y' TO WS-BUF-SHORT-FLAG
           ELSE
               MOVE 'N' TO WS-BUF-SHORT-FLAG
           END-IF.
           IF WS-BUF-FROZEN
               GO TO 5500-EXIT
           END-IF.
           IF WS-BUF-TD-QTY = ZERO AND WS-BUF-SD-QTY = ZERO
               IF NOT WS-BUF-FLAT
                   ADD 1 TO WS-FLATTEN-CNT
               END-IF
               MOVE 'F' TO WS-BUF-STATUS
           ELSE
               MOVE 'O' TO WS-BUF-STATUS
           END-IF.
       5500-EXIT.
           EXIT.
      *================================================================*
      * WRITE THE BUFFERED POSITION BACK TO THE KSDS                   *
      *================================================================*
       6000-FLUSH-BUFFER.
           IF BUFFER-EMPTY
               GO TO 6000-EXIT
           END-IF.
           IF DEBUG-ON
               PERFORM 7900-DUMP-BUFFER THRU 7900-EXIT
           END-IF.
           PERFORM 6050-TD-SD-INTEGRITY THRU 6050-EXIT.
           MOVE WS-BUF-POSITION TO POS-POSITION-REC.
           IF BUFFER-IS-NEW
               PERFORM 6100-WRITE-POSITION THRU 6100-EXIT
           ELSE
               PERFORM 6200-REWRITE-POSITION THRU 6200-EXIT
           END-IF.
           MOVE 'N' TO WS-BUFFER-SW.
           MOVE LOW-VALUES TO WS-BUF-POS-KEY.
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TRADE DATE QTY MUST EQUAL SETTLED + PENDING IN - PENDING OUT.  *
      * DIFFERENCES ARE REPORTED, NOT CORRECTED (SEE CHG12880 - THE    *
      * NIGHTLY SRB500 BALANCING IS THE CONTROL OF RECORD).            *
      *----------------------------------------------------------------*
       6050-TD-SD-INTEGRITY.
           COMPUTE WS-TDSD-EXPECTED =
               WS-BUF-SD-QTY + WS-BUF-PEND-IN-QTY
                             - WS-BUF-PEND-OUT-QTY.
           IF WS-TDSD-EXPECTED NOT = WS-BUF-TD-QTY
               ADD 1 TO WS-TDSD-DIFF-CNT
               COMPUTE WS-TDSD-DIFF =
                   WS-BUF-TD-QTY - WS-TDSD-EXPECTED
               IF WS-TDSD-DIFF-CNT NOT > WS-TDSD-DISPLAY-MAX
                   MOVE WS-TDSD-DIFF TO WS-DISP-QTY
                   DISPLAY 'SRB200 TD/SD OUT OF LINE ' WS-BUF-KEY
                           ' DIFF ' WS-DISP-QTY
               END-IF
               IF WS-TDSD-DIFF-CNT = WS-TDSD-DISPLAY-MAX
                   DISPLAY 'SRB200 FURTHER TD/SD MESSAGES SUPPRESSED'
               END-IF
           END-IF.
       6050-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INSERT.  22 = SOMEBODY ELSE (CAB200 CORRECTION RUN, MANUAL     *
      * REPRO) ADDED THE KEY SINCE WE READ IT - RE-READ AND APPLY THE  *
      * DELTA TO THE CURRENT RECORD INSTEAD.                           *
      *----------------------------------------------------------------*
       6100-WRITE-POSITION.
           WRITE POS-POSITION-REC.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   ADD 1 TO WS-INSERT-CNT WS-POS-WRITES
               WHEN POSMAST-DUP-AIX
                   ADD 1 TO WS-INSERT-CNT WS-POS-WRITES
                            WS-DUP-AIX-CNT
               WHEN POSMAST-DUP-KEY
                   ADD 1 TO WS-COLLISION-CNT
                   PERFORM 6150-COLLISION-RECOVERY THRU 6150-EXIT
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '6100-WRITE-POSITION' TO AB-PARAGRAPH
                   MOVE POS-KEY TO AB-KEY
                   MOVE 'POSITION WRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       6100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6150-COLLISION-RECOVERY.
      *----------------------------------------------------------------*
           DISPLAY 'SRB200 INSERT COLLISION - MERGING ' WS-BUF-KEY.
           MOVE WS-BUF-KEY TO POS-KEY.
           READ POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
           AND WS-POSMAST-STATUS NOT = '02'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1008 TO AB-ABEND-CODE
               MOVE '6150-COLLISION-RECOVERY' TO AB-PARAGRAPH
               MOVE WS-BUF-KEY TO AB-KEY
               MOVE 'KEY EXISTS ON WRITE BUT NOT ON READ'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD WS-BUF-TD-QTY       TO POS-TD-QTY.
           ADD WS-BUF-SD-QTY       TO POS-SD-QTY.
           ADD WS-BUF-PEND-IN-QTY  TO POS-PEND-IN-QTY.
           ADD WS-BUF-PEND-OUT-QTY TO POS-PEND-OUT-QTY.
           ADD WS-BUF-COST-BASIS   TO POS-COST-BASIS.
           ADD WS-BUF-REALIZED-PL-YTD TO POS-REALIZED-PL-YTD.
           IF POS-TD-QTY NOT = ZERO
               COMPUTE POS-AVG-COST ROUNDED =
                   POS-COST-BASIS / POS-TD-QTY
                   ON SIZE ERROR
                       ADD 1 TO WS-AVG-ERR-CNT
               END-COMPUTE
           ELSE
               MOVE ZERO TO POS-AVG-COST POS-COST-BASIS
           END-IF.
           IF POS-TD-QTY = ZERO AND POS-SD-QTY = ZERO
               MOVE 'F' TO POS-STATUS
           ELSE
               IF NOT POS-FROZEN
                   MOVE 'O' TO POS-STATUS
               END-IF
           END-IF.
           MOVE DC-BUS-DATE TO POS-LAST-ACTV-DATE.
           MOVE JI-JOBNAME  TO POS-LAST-UPD-JOB.
           PERFORM 6200-REWRITE-POSITION THRU 6200-EXIT.
       6150-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6200-REWRITE-POSITION.
      *----------------------------------------------------------------*
           REWRITE POS-POSITION-REC.
           EVALUATE TRUE
               WHEN POSMAST-OK
                   ADD 1 TO WS-UPDATE-CNT WS-POS-REWRITES
               WHEN POSMAST-DUP-AIX
                   ADD 1 TO WS-UPDATE-CNT WS-POS-REWRITES
                            WS-DUP-AIX-CNT
               WHEN OTHER
                   MOVE 'POSMAST' TO AB-DDNAME
                   MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '6200-REWRITE-POSITION' TO AB-PARAGRAPH
                   MOVE POS-KEY TO AB-KEY
                   MOVE 'POSITION REWRITE FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       6200-EXIT.
           EXIT.
      *================================================================*
      * POSTING JOURNAL                                                *
      *================================================================*
       6500-WRITE-JOURNAL.
           PERFORM 6600-BUILD-JOURNAL THRU 6600-EXIT.
           MOVE WS-BUF-TD-QTY   TO PSJ-AFTER-TD-QTY.
           MOVE WS-BUF-AVG-COST TO PSJ-AFTER-AVG-COST.
           EVALUATE TRUE
               WHEN WS-BUF-FLAT
                   MOVE 'F' TO PSJ-POST-ACTION
               WHEN BUFFER-IS-NEW
                   MOVE 'I' TO PSJ-POST-ACTION
               WHEN OTHER
                   MOVE 'U' TO PSJ-POST-ACTION
           END-EVALUATE.
           PERFORM 6700-PUT-JOURNAL THRU 6700-EXIT.
       6500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6600-BUILD-JOURNAL.
      *----------------------------------------------------------------*
           MOVE SPACES             TO PSJ-JOURNAL-REC.
           IF SETTLES-NOW
               MOVE 'S' TO ACT-SETTLE-FLAG
           ELSE
               IF NOT ACT-NO-SETTLEMENT
                   MOVE 'P' TO ACT-SETTLE-FLAG
               END-IF
           END-IF.
           MOVE ACT-ACTIVITY-REC   TO PSJ-ACTIVITY.
           MOVE WS-JB-TD-QTY       TO PSJ-BEFORE-TD-QTY.
           MOVE WS-JB-AVG-COST     TO PSJ-BEFORE-AVG-COST.
           MOVE WS-REALIZED-PL     TO PSJ-REALIZED-PL.
           CALL 'CMASM02' USING TS-TIMESTAMP-AREA.
           MOVE TS-TIMESTAMP       TO PSJ-POST-TS.
       6600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6700-PUT-JOURNAL.
      *----------------------------------------------------------------*
           WRITE POSTJRNL-REC FROM PSJ-JOURNAL-REC.
           IF WS-POSTJRNL-STATUS NOT = '00'
               MOVE 'POSTJRNL' TO AB-DDNAME
               MOVE WS-POSTJRNL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '6700-PUT-JOURNAL' TO AB-PARAGRAPH
               MOVE ACT-REF TO AB-KEY
               MOVE 'POSTING JOURNAL WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-JRNL-CNT.
           ADD ACT-CASH-CHANGE TO WS-JRNL-CASH-HASH.
           ADD WS-ABS-QTY      TO WS-JRNL-QTY-HASH.
           IF DEBUG-ON
               DISPLAY 'SRB200 JRNL ' ACT-REF ' ' ACT-LEG-NO ' '
                       ACT-TYPE ' ' PSJ-POST-ACTION
           END-IF.
       6700-EXIT.
           EXIT.
      *================================================================*
      * CHECKPOINT.  THE BUFFERED POSITION IS WRITTEN BEFORE CMU070    *
      * IS DUE TO RECORD THE KEY (VSAM UPDATES ARE IMMEDIATE - THERE   *
      * IS NOTHING ELSE TO COMMIT).                                    *
      *================================================================*
       6800-TAKE-CHECKPOINT.
           ADD 1 TO WS-CKPT-SINCE-LAST.
           IF WS-CKPT-SINCE-LAST NOT < WS-CKPT-INTERVAL
               PERFORM 6000-FLUSH-BUFFER THRU 6000-EXIT
           END-IF.
           MOVE 'TAKE'           TO CK-FUNCTION.
           MOVE WS-READ-CNT      TO CK-RECORD-COUNT.
           MOVE WS-INPUT-KEY     TO CK-CURRENT-KEY.
           CALL 'CMU070' USING CK-CHECKPOINT-PARMS.
           IF CK-RETURN-CODE NOT = ZERO
               MOVE 1009 TO AB-ABEND-CODE
               MOVE '6800-TAKE-CHECKPOINT' TO AB-PARAGRAPH
               MOVE 'CHKPTFL' TO AB-DDNAME
               MOVE WS-INPUT-KEY TO AB-KEY
               MOVE 'CMU070 TAKE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF CK-CHECKPOINT-TAKEN = 'Y'
               ADD 1 TO WS-CKPT-CNT
               MOVE ZERO TO WS-CKPT-SINCE-LAST
               IF DEBUG-ON
                   DISPLAY 'SRB200 CHECKPOINT AT ' WS-INPUT-KEY
               END-IF
           END-IF.
       6800-EXIT.
           EXIT.
      *================================================================*
      * MARGIN INTEREST ACCRUAL (CHG00544).                            *
      * DAILY INTEREST ON DEBIT BALANCES OF MARGIN ACCOUNTS AT BROKER  *
      * CALL PLUS A TIERED SPREAD.  ACCRUED MONTH TO DATE, CHARGED BY  *
      * THE MONTH-END STATEMENT RUN.                                   *
      *================================================================*
       7000-MARGIN-INTEREST.
           IF WS-MGN-DEBIT-BAL NOT < ZERO
               GO TO 7000-EXIT
           END-IF.
           ADD 1 TO WS-MGN-ACCT-CNT.
           PERFORM 7100-MARGIN-RATE THRU 7100-EXIT.
           PERFORM 7200-MARGIN-ACCRUE THRU 7200-EXIT.
           IF DC-MONTH-END
               PERFORM 7300-MARGIN-CHARGE THRU 7300-EXIT
           END-IF.
       7000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7100-MARGIN-RATE.
      *----------------------------------------------------------------*
           MOVE WS-BROKER-CALL-RATE TO WS-MGN-CALL-RATE.
           MOVE ZERO TO WS-MGN-SPREAD.
           PERFORM VARYING WS-MGN-TIER-SUB FROM 1 BY 1
                   UNTIL WS-MGN-TIER-SUB > 5
                      OR WS-MGN-SPREAD NOT = ZERO
               IF (WS-MGN-DEBIT-BAL * -1) NOT >
                  WS-MGN-TIER-LIMIT (WS-MGN-TIER-SUB)
                   MOVE WS-MGN-TIER-SPREAD (WS-MGN-TIER-SUB)
                                   TO WS-MGN-SPREAD
               END-IF
           END-PERFORM.
           COMPUTE WS-MGN-ANNUAL-RATE =
               WS-MGN-CALL-RATE + WS-MGN-SPREAD.
       7100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7200-MARGIN-ACCRUE.
      *----------------------------------------------------------------*
           COMPUTE WS-MGN-DAILY-INT ROUNDED =
               (WS-MGN-DEBIT-BAL * -1) * WS-MGN-ANNUAL-RATE / 100
                   * WS-MGN-DAYS / WS-MGN-BASIS-DAYS.
           ADD WS-MGN-DAILY-INT TO WS-MGN-MTD-INT.
       7200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7300-MARGIN-CHARGE.
      *----------------------------------------------------------------*
           IF WS-MGN-MTD-INT < WS-MGN-HOUSE-MIN
               MOVE WS-MGN-HOUSE-MIN TO WS-MGN-MTD-INT
           END-IF.
           DISPLAY 'SRB200 MARGIN INTEREST CHARGED '
                   WS-MGN-MTD-INT.
           MOVE ZERO TO WS-MGN-MTD-INT.
       7300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * HOUSE MAINTENANCE REQUIREMENT CHECK.  EQUITY BELOW 30 PCT OF   *
      * LONG MARKET VALUE (25 PCT REG T) RAISES A MAINTENANCE CALL.    *
      *----------------------------------------------------------------*
       7400-MARGIN-CALL-CHECK.
           COMPUTE WS-MGN-EQUITY =
               WS-MGN-LONG-MV + WS-MGN-DEBIT-BAL.
           IF WS-MGN-LONG-MV = ZERO
               GO TO 7400-EXIT
           END-IF.
           COMPUTE WS-MGN-EQUITY-PCT ROUNDED =
               WS-MGN-EQUITY / WS-MGN-LONG-MV * 100.
           EVALUATE TRUE
               WHEN WS-MGN-EQUITY-PCT < WS-MGN-REG-T-PCT
                   MOVE 'R' TO WS-MGN-CALL-TYPE
                   COMPUTE WS-MGN-CALL-AMT ROUNDED =
                       (WS-MGN-LONG-MV * WS-MGN-REG-T-PCT / 100)
                       - WS-MGN-EQUITY
               WHEN WS-MGN-EQUITY-PCT < WS-MGN-HOUSE-PCT
                   MOVE 'H' TO WS-MGN-CALL-TYPE
                   COMPUTE WS-MGN-CALL-AMT ROUNDED =
                       (WS-MGN-LONG-MV * WS-MGN-HOUSE-PCT / 100)
                       - WS-MGN-EQUITY
               WHEN OTHER
                   MOVE SPACE TO WS-MGN-CALL-TYPE
                   MOVE ZERO  TO WS-MGN-CALL-AMT
           END-EVALUATE.
           IF WS-MGN-CALL-TYPE NOT = SPACE
               ADD 1 TO WS-MGN-CALL-CNT
               PERFORM 7500-MARGIN-CALL-NOTICE THRU 7500-EXIT
           END-IF.
       7400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       7500-MARGIN-CALL-NOTICE.
      *----------------------------------------------------------------*
           MOVE WS-MGN-CALL-AMT TO WS-DISP-AMT.
           DISPLAY 'SRB200 MARGIN CALL ' WS-MGN-CALL-TYPE ' '
                   WS-BUF-ACCT-NO ' AMOUNT ' WS-DISP-AMT.
           IF WS-MGN-CALL-TYPE = 'R'
               MOVE 'WRIT'      TO AU-FUNCTION
               MOVE 'MGNCALL'   TO AU-EVENT
               MOVE 'W'         TO AU-SEVERITY
               MOVE WS-BUF-ACCT-NO TO AU-KEY
               MOVE 'REG T MARGIN CALL ISSUED' TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF.
       7500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * MONTH-END MARGIN INTEREST SUMMARY                              *
      *----------------------------------------------------------------*
       7600-MARGIN-SUMMARY.
           DISPLAY 'SRB200 MARGIN ACCOUNTS ACCRUED : '
                   WS-MGN-ACCT-CNT.
           DISPLAY 'SRB200 MARGIN CALLS RAISED     : '
                   WS-MGN-CALL-CNT.
           MOVE ZERO TO WS-MGN-ACCT-CNT WS-MGN-CALL-CNT.
       7600-EXIT.
           EXIT.
      *================================================================*
      * DEBUG DUMP OF THE BUFFERED POSITION (DEBUG=Y CONTROL CARD)     *
      *================================================================*
       7900-DUMP-BUFFER.
           DISPLAY 'SRB200 ---- BUFFER DUMP ----'.
           DISPLAY '  KEY        : ' WS-BUF-KEY.
           DISPLAY '  TYPES      : ' WS-BUF-SEC-TYPE ' '
                   WS-BUF-ACCT-TYPE ' ' WS-BUF-CCY.
           MOVE WS-BUF-TD-QTY TO WS-DISP-QTY.
           DISPLAY '  TD QTY     : ' WS-DISP-QTY.
           MOVE WS-BUF-SD-QTY TO WS-DISP-QTY.
           DISPLAY '  SD QTY     : ' WS-DISP-QTY.
           MOVE WS-BUF-PEND-IN-QTY TO WS-DISP-QTY.
           DISPLAY '  PEND IN    : ' WS-DISP-QTY.
           MOVE WS-BUF-PEND-OUT-QTY TO WS-DISP-QTY.
           DISPLAY '  PEND OUT   : ' WS-DISP-QTY.
           MOVE WS-BUF-COST-BASIS TO WS-DISP-AMT.
           DISPLAY '  COST BASIS : ' WS-DISP-AMT.
           MOVE WS-BUF-REALIZED-PL-YTD TO WS-DISP-AMT.
           DISPLAY '  REAL P&L   : ' WS-DISP-AMT.
           DISPLAY '  STATUS     : ' WS-BUF-STATUS ' SHORT '
                   WS-BUF-SHORT-FLAG ' ACTION ' WS-BUFFER-ACTION-SW.
       7900-EXIT.
           EXIT.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-ACTIVITY.
           READ ACTVIN-FILE INTO ACT-ACTIVITY-REC.
           EVALUATE TRUE
               WHEN ACTVIN-OK
                   CONTINUE
               WHEN ACTVIN-EOF
                   MOVE 'Y' TO WS-EOF-SW
               WHEN OTHER
                   MOVE 'ACTVIN' TO AB-DDNAME
                   MOVE WS-ACTVIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-ACTIVITY' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'SRB200'       TO CT-STAGE.
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
           MOVE 'DONE'        TO CK-FUNCTION.
           MOVE WS-READ-CNT   TO CK-RECORD-COUNT.
           MOVE WS-INPUT-KEY  TO CK-CURRENT-KEY.
           CALL 'CMU070' USING CK-CHECKPOINT-PARMS.
           IF CK-RETURN-CODE NOT = ZERO
               DISPLAY 'SRB200 WARNING - CMU070 DONE RC '
                       CK-RETURN-CODE
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
           CLOSE ACTVIN-FILE.
           IF WS-ACTVIN-STATUS NOT = '00'
               MOVE 'ACTVIN' TO AB-DDNAME
               MOVE WS-ACTVIN-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE POSMAST-FILE.
           IF WS-POSMAST-STATUS NOT = '00'
               MOVE 'POSMAST' TO AB-DDNAME
               MOVE WS-POSMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE PENDSETL-FILE.
           IF WS-PENDSETL-STATUS NOT = '00'
               MOVE 'PENDSETL' TO AB-DDNAME
               MOVE WS-PENDSETL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           CLOSE POSTJRNL-FILE.
           IF WS-POSTJRNL-STATUS NOT = '00'
               MOVE 'POSTJRNL' TO AB-DDNAME
               MOVE WS-POSTJRNL-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 9100-POST-CONTROL-TOTALS THRU 9100-EXIT.
           PERFORM 9200-DISPLAY-STATISTICS THRU 9200-EXIT.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           MOVE 'POSITION POSTING ENDED' TO AU-MESSAGE.
           EVALUATE TRUE
               WHEN WS-RETURN-CODE > 4
                   MOVE 'E' TO AU-SEVERITY
               WHEN WS-RETURN-CODE = 4
                   MOVE 'W' TO AU-SEVERITY
               WHEN OTHER
                   MOVE 'I' TO AU-SEVERITY
           END-EVALUATE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9100-POST-CONTROL-TOTALS.
      *----------------------------------------------------------------*
           MOVE 'ACTV-IN'        TO CT-COUNTER-NAME.
           COMPUTE CT-COUNT = WS-READ-CNT - WS-SKIP-CNT.
           MOVE WS-IN-CASH-HASH  TO CT-AMOUNT.
           MOVE WS-IN-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'TC-ACTV-POSTED' TO CT-COUNTER-NAME.
           MOVE WS-TC-POSTED-CNT TO CT-COUNT.
           MOVE WS-TC-CASH-HASH  TO CT-AMOUNT.
           MOVE WS-TC-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CA-ACTV-POSTED' TO CT-COUNTER-NAME.
           MOVE WS-CA-POSTED-CNT TO CT-COUNT.
           MOVE WS-CA-CASH-HASH  TO CT-AMOUNT.
           MOVE WS-CA-QTY-HASH   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'POSTJRNL-OUT'   TO CT-COUNTER-NAME.
           MOVE WS-JRNL-CNT      TO CT-COUNT.
           MOVE WS-JRNL-CASH-HASH TO CT-AMOUNT.
           MOVE WS-JRNL-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'PENDSETL-OUT'   TO CT-COUNTER-NAME.
           MOVE WS-PEND-WRITE-CNT TO CT-COUNT.
           MOVE WS-PEND-CASH-HASH TO CT-AMOUNT.
           MOVE WS-PEND-QTY-HASH TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'POSN-INSERTED'  TO CT-COUNTER-NAME.
           MOVE WS-INSERT-CNT    TO CT-COUNT.
           MOVE ZERO             TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'POSN-UPDATED'   TO CT-COUNTER-NAME.
           MOVE WS-UPDATE-CNT    TO CT-COUNT.
           MOVE WS-REALIZED-TOTAL TO CT-AMOUNT.
           MOVE ZERO             TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
       9100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       9200-DISPLAY-STATISTICS.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'.
           DISPLAY '* SRB200 - STOCK RECORD POSITION POSTING       *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' JOB NAME                 : ' JI-JOBNAME.
           IF RESTART-IN-PROGRESS
               DISPLAY ' *** RESTARTED RUN *** RESTART KEY:'
               DISPLAY '   ' WS-RESTART-KEY
           END-IF.
           MOVE WS-READ-CNT TO WS-DISP-CNT.
           DISPLAY ' ACTIVITY LEGS READ       : ' WS-DISP-CNT.
           MOVE WS-SKIP-CNT TO WS-DISP-CNT.
           DISPLAY ' BYPASSED ON RESTART      : ' WS-DISP-CNT.
           MOVE WS-POSTED-CNT TO WS-DISP-CNT.
           DISPLAY ' LEGS POSTED              : ' WS-DISP-CNT.
           MOVE WS-UNPOSTED-CNT TO WS-DISP-CNT.
           DISPLAY ' LEGS NOT POSTABLE        : ' WS-DISP-CNT.
           MOVE WS-CASH-ONLY-CNT TO WS-DISP-CNT.
           DISPLAY '   CASH ONLY LEGS         : ' WS-DISP-CNT.
           PERFORM VARYING WS-TYPE-SUB FROM 1 BY 1
                   UNTIL WS-TYPE-SUB > 14
               IF WS-BY-TYPE-CNT (WS-TYPE-SUB) > ZERO
                   MOVE WS-BY-TYPE-CNT (WS-TYPE-SUB) TO WS-DISP-CNT
                   DISPLAY '   TYPE ' WS-TYPE-NAME (WS-TYPE-SUB)
                           '               : ' WS-DISP-CNT
               END-IF
           END-PERFORM.
           MOVE WS-INSERT-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS INSERTED       : ' WS-DISP-CNT.
           MOVE WS-UPDATE-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS UPDATED        : ' WS-DISP-CNT.
           MOVE WS-FLATTEN-CNT TO WS-DISP-CNT.
           DISPLAY ' POSITIONS FLATTENED      : ' WS-DISP-CNT.
           MOVE WS-SHORT-CNT TO WS-DISP-CNT.
           DISPLAY ' NEW SHORT POSITIONS      : ' WS-DISP-CNT.
           MOVE WS-FROZEN-CNT TO WS-DISP-CNT.
           DISPLAY ' POSTINGS TO FROZEN POSNS : ' WS-DISP-CNT.
           MOVE WS-SETTLED-NOW-CNT TO WS-DISP-CNT.
           DISPLAY ' LEGS SETTLED ON POSTING  : ' WS-DISP-CNT.
           MOVE WS-PEND-WRITE-CNT TO WS-DISP-CNT.
           DISPLAY ' PENDING ROWS WRITTEN     : ' WS-DISP-CNT.
           MOVE WS-PEND-DUP-CNT TO WS-DISP-CNT.
           DISPLAY ' PENDING DUPLICATES       : ' WS-DISP-CNT.
           MOVE WS-PEND-RESTART-CNT TO WS-DISP-CNT.
           DISPLAY ' PENDING KEPT ON RESTART  : ' WS-DISP-CNT.
           MOVE WS-JRNL-CNT TO WS-DISP-CNT.
           DISPLAY ' JOURNAL RECORDS WRITTEN  : ' WS-DISP-CNT.
           MOVE WS-COLLISION-CNT TO WS-DISP-CNT.
           DISPLAY ' INSERT COLLISIONS        : ' WS-DISP-CNT.
           MOVE WS-DUP-AIX-CNT TO WS-DISP-CNT.
           DISPLAY ' STATUS 02 (ALT KEY)      : ' WS-DISP-CNT.
           MOVE WS-AVG-ERR-CNT TO WS-DISP-CNT.
           DISPLAY ' AVERAGE COST ERRORS      : ' WS-DISP-CNT.
           MOVE WS-YTD-RESET-CNT TO WS-DISP-CNT.
           DISPLAY ' YTD P&L RESETS           : ' WS-DISP-CNT.
           MOVE WS-BOX-CNT TO WS-DISP-CNT.
           DISPLAY ' BOX LOCATION LEGS        : ' WS-DISP-CNT.
           MOVE WS-SELL-SHORT-WARN TO WS-DISP-CNT.
           DISPLAY ' SELLS CREATING SHORTS    : ' WS-DISP-CNT.
           MOVE WS-LOC-LONG-WARN TO WS-DISP-CNT.
           DISPLAY ' LOCATION ROWS GONE LONG  : ' WS-DISP-CNT.
           MOVE WS-CCY-MISMATCH-WARN TO WS-DISP-CNT.
           DISPLAY ' CURRENCY MISMATCHES      : ' WS-DISP-CNT.
           MOVE WS-TYPE-FILL-CNT TO WS-DISP-CNT.
           DISPLAY ' SEC/ACCT TYPES FILLED    : ' WS-DISP-CNT.
           MOVE WS-Y2K-WINDOW-CNT TO WS-DISP-CNT.
           DISPLAY ' 6-DIGIT DATES WINDOWED   : ' WS-DISP-CNT.
           MOVE WS-CKPT-CNT TO WS-DISP-CNT.
           DISPLAY ' CHECKPOINTS TAKEN        : ' WS-DISP-CNT.
           MOVE WS-TDSD-DIFF-CNT TO WS-DISP-CNT.
           DISPLAY ' TD/SD INTEGRITY DIFFS    : ' WS-DISP-CNT.
           DISPLAY ' POSTED BY SOURCE / SIDE  :   OWNERSHIP'
                   '     LOCATION'.
           PERFORM VARYING WS-SM-R FROM 1 BY 1 UNTIL WS-SM-R > 3
               MOVE WS-SM-CNT (WS-SM-R 1) TO WS-DISP-CNT
               MOVE WS-SM-CNT (WS-SM-R 2) TO WS-DISP-CNT2
               DISPLAY '   SOURCE '
                       WS-SM-SOURCES ((WS-SM-R * 2) - 1 : 2)
                       '             : ' WS-DISP-CNT '  '
                       WS-DISP-CNT2
           END-PERFORM.
           MOVE WS-REALIZED-TOTAL TO WS-DISP-AMT.
           DISPLAY ' REALIZED P&L TODAY       : ' WS-DISP-AMT.
           MOVE WS-IN-CASH-HASH TO WS-DISP-AMT.
           DISPLAY ' INPUT CASH HASH          : ' WS-DISP-AMT.
           MOVE WS-IN-QTY-HASH TO WS-DISP-QTY.
           DISPLAY ' INPUT QTY HASH (ABS)     : ' WS-DISP-QTY.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
       9200-EXIT.
           EXIT.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'SRB200 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'SRB200 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'SRB200 ' AB-MESSAGE.
           DISPLAY 'SRB200 LAST INPUT KEY ' WS-INPUT-KEY
                   ' RECORD ' WS-READ-CNT.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
