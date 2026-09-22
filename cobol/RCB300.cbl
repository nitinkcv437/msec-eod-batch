       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCB300.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  APRIL 2004.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : RCB300                                            *
      * DESCRIPTION: SETTLEMENT CASH RECONCILIATION.                   *
      *              MATCHES THE SETTLEMENT BANK'S TRANSACTIONS (BAI2  *
      *              16 RECORDS, NORMALIZED BY RCB120) AGAINST THE     *
      *              CASH LEGS THAT SETTLED ON THE BOOKS THE SAME DAY  *
      *              (SR.SETTLED - CLIENT AND FIRM LEGS WITH A CASH    *
      *              MOVEMENT; STREET LEGS CARRY NO CASH).             *
      *              BANK SIGN: CREDIT = CASH IN.                      *
      *              BOOK SIGN: ACT-CASH-CHANGE > 0 = CASH IN.         *
      *              RULES, IN THIS ORDER:                             *
      *                R01 CUSTOMER REF = ACTIVITY REF, SAME AMOUNT    *
      *                    -> MA MATCHED                               *
      *                R02 SAME REF, AMOUNT WITHIN $0.05               *
      *                    -> MT MATCHED WITHIN TOLERANCE              *
      *                R03 DTC NET SETTLEMENT LINE (TYPE 195, TEXT     *
      *                    'DTC NET...') AGAINST THE SUM PER CURRENCY  *
      *                    OF THE LEGS OF NON-DVP ACCOUNTS, WITHIN     *
      *                    $1.00 -> MN MATCHED NET                     *
      *              EVERYTHING ELSE IS UN AND OPENS A CASH BREAK IN   *
      *              THE BREAK MASTER (CLASS CS):                      *
      *                BU BANK ITEM UNMATCHED                          *
      *                KU BOOK ITEM UNMATCHED                          *
      *                AD AMOUNT DIFFERENCE (SAME REF / NET LINE)      *
      *              CASH BREAKS ARE AGED, ESCALATED AND CLOSED THE    *
      *              SAME WAY AS POSITION BREAKS (SEE RCB400).         *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSRCD040 / STEP010                                *
      * INPUT      : DATECARD - BUSINESS DATE CARD            (CMDATEW)*
      *              BANKTXN  - MSEC.PROD.RC.BANKTXN(0)       (RCCASHT)*
      *              SETLIN   - MSEC.PROD.SR.SETTLED(-1)      (SRACTV) *
      *                         (PRIOR DAY - THIS JOB RUNS AFTER       *
      *                         MSSRD030 HAS CATALOGED TODAY'S)        *
      *              ACCTMAST - MSEC.PROD.CM.ACCTMAST.KSDS    (CMACCT) *
      *              HOLIDAYS - FOR CMU010 (BUSINESS DAY AGE)          *
      * UPDATE     : RCBRKMST - MSEC.PROD.RC.BRKMAST.KSDS     (RCBRKM) *
      * OUTPUT     : CASHREC  - MSEC.PROD.RC.CASHREC(+1)      (RCMATCH)*
      * CALLS      : CMU010 (DIFB), CMU050, CMU060, CMU080, CMASM01    *
      * RETURN CODE: 0 ALL MATCHED   4 UNMATCHED ITEMS / CASH BREAKS   *
      *              U1007 TABLE OVERFLOW                              *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 2004-04-19 KAP  ORIGINAL - REPLACES MANUAL TICK-OFF   CHG12230 *
      *                 OF THE BANK STATEMENT                          *
      * 2004-06-07 KAP  R02 TOLERANCE - BANK ROUNDS EUR/GBP   CHG12301 *
      *                 CONVERSIONS TO THE DIME                        *
      * 2005-01-24 KAP  CASH BREAKS TO THE BREAK MASTER       CHG12966 *
      * 2008-09-15 SPA  R03 DTC NET SETTLEMENT (NON-DVP       CHG17955 *
      *                 ACCOUNTS SETTLE NET THROUGH DTC)               *
      * 2011-06-20 SPA  NET TOLERANCE $1.00 (WAS $0.10)       CHG21877 *
      * 2013-05-06 SPA  ESCALATION AS RCB400                  CHG24790 *
      * 2017-03-13 MHC  AGE IN BUSINESS DAYS VIA CMU010 DIFB  CHG31555 *
      * 2019-02-25 MHC  CONTROL TOTALS NAMES PER CMB090       CHG33410 *
      * 2021-07-19 NVR  NET LINE MAY BE A DEBIT (495 OR       CHG38804 *
      *                 SIGNED 195)                                    *
      * 2022-10-03 NVR  JPY / KRW BOOK AMOUNTS ROUNDED TO     CHG39516 *
      *                 WHOLE UNITS FOR R01/R02 (BANK SENDS NO         *
      *                 DECIMALS) - NET (R03) STAYS UNROUNDED          *
      * 2024-02-12 NVR  T+1 - SETTLED(-1) STILL THE BOOK DAY  CHG41007 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT BANKTXN-FILE   ASSIGN TO BANKTXN
                  FILE STATUS IS WS-BANKTXN-STATUS.
           SELECT SETLIN-FILE    ASSIGN TO SETLIN
                  FILE STATUS IS WS-SETLIN-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCT-NO
                  FILE STATUS IS WS-ACCTMAST-STATUS.
           SELECT BRKMAST-FILE   ASSIGN TO RCBRKMST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS RBM-KEY
                  FILE STATUS IS WS-BRKMAST-STATUS.
           SELECT CASHREC-FILE   ASSIGN TO CASHREC
                  FILE STATUS IS WS-CASHREC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  BANKTXN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY RCCASHT.
       FD  SETLIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       COPY SRACTV.
       FD  ACCTMAST-FILE.
       COPY CMACCT.
       FD  BRKMAST-FILE.
       COPY RCBRKM.
       FD  CASHREC-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CASHREC-OUT-REC             PIC X(200).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'RCB300'.
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-BANKTXN-STATUS       PIC X(02)  VALUE '00'.
               88  BANKTXN-OK                     VALUE '00'.
               88  BANKTXN-EOF                    VALUE '10'.
           05  WS-SETLIN-STATUS        PIC X(02)  VALUE '00'.
               88  SETLIN-OK                      VALUE '00'.
               88  SETLIN-EOF                     VALUE '10'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-OK                    VALUE '00'.
               88  ACCTMAST-NOT-FOUND             VALUE '23'.
           05  WS-BRKMAST-STATUS       PIC X(02)  VALUE '00'.
               88  BRKMAST-OK                     VALUE '00' '02'.
               88  BRKMAST-NOT-FOUND              VALUE '23'.
               88  BRKMAST-EOF                    VALUE '10'.
           05  WS-CASHREC-STATUS       PIC X(02)  VALUE '00'.
       01  WS-SWITCHES.
           05  WS-BANK-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-BANK                    VALUE 'Y'.
           05  WS-BOOK-EOF-SW          PIC X(01)  VALUE 'N'.
               88  END-OF-BOOK                    VALUE 'Y'.
           05  WS-BROWSE-EOF-SW        PIC X(01)  VALUE 'N'.
               88  END-OF-BROWSE                  VALUE 'Y'.
           05  WS-FOUND-SW             PIC X(01)  VALUE 'N'.
               88  ITEM-FOUND                     VALUE 'Y'.
       01  WS-RETURN-CODE              PIC S9(04) COMP  VALUE ZERO.
      *----------------------------------------------------------------*
      * MATCHING RULE CONSTANTS                                        *
      *----------------------------------------------------------------*
       01  WS-RULE-CONSTANTS.
           05  WS-R02-TOLERANCE        PIC S9(01)V99 COMP-3 VALUE +.05.
           05  WS-R03-TOLERANCE        PIC S9(03)V99 COMP-3
                                                     VALUE +1.00.
           05  WS-NET-TYPE-CR          PIC X(03)  VALUE '195'.
           05  WS-NET-TYPE-DR          PIC X(03)  VALUE '495'.
           05  WS-NET-TEXT             PIC X(07)  VALUE 'DTC NET'.
      *----------------------------------------------------------------*
      * CURRENCIES THE BANK SENDS WITHOUT DECIMALS (SEE RCB120)        *
      *----------------------------------------------------------------*
       01  WS-WHOLE-CCY-VALUES         PIC X(12)  VALUE 'JPYKRW      '.
       01  WS-WHOLE-CCY-TABLE REDEFINES WS-WHOLE-CCY-VALUES.
           05  WS-WHOLE-CCY OCCURS 4 TIMES INDEXED BY WC-IDX
                                       PIC X(03).
       01  WS-WHOLE-AMOUNT             PIC S9(13)    COMP-3.
      *----------------------------------------------------------------*
      * BANK TRANSACTIONS                                              *
      *----------------------------------------------------------------*
       01  WS-BANK-MAX                 PIC S9(08) COMP  VALUE +5000.
       01  WS-BANK-USED                PIC S9(08) COMP  VALUE ZERO.
       01  WS-BANK-TABLE.
           05  WS-BK OCCURS 5000 TIMES INDEXED BY BK-IDX.
               10  WS-BK-CCY           PIC X(03).
               10  WS-BK-VALUE-DATE    PIC 9(08).
               10  WS-BK-TYPE-CODE     PIC X(03).
               10  WS-BK-AMOUNT        PIC S9(13)V99 COMP-3.
               10  WS-BK-BANK-REF      PIC X(16).
               10  WS-BK-CUST-REF      PIC X(16).
               10  WS-BK-TEXT          PIC X(50).
               10  WS-BK-SEQ           PIC 9(07).
               10  WS-BK-STATUS        PIC X(02).
               10  WS-BK-RULE          PIC X(03).
               10  WS-BK-OTHER-REF     PIC X(16).
               10  WS-BK-OTHER-AMT     PIC S9(13)V99 COMP-3.
      *----------------------------------------------------------------*
      * BOOK CASH LEGS                                                 *
      *----------------------------------------------------------------*
       01  WS-BOOK-MAX                 PIC S9(08) COMP  VALUE +20000.
       01  WS-BOOK-USED                PIC S9(08) COMP  VALUE ZERO.
       01  WS-BOOK-TABLE.
           05  WS-BL OCCURS 20000 TIMES INDEXED BY BL-IDX BL-IDX2.
               10  WS-BL-REF           PIC X(16).
               10  WS-BL-ACCT          PIC X(10).
               10  WS-BL-TYPE          PIC X(03).
               10  WS-BL-CCY           PIC X(03).
               10  WS-BL-SETTLE-DATE   PIC 9(08).
               10  WS-BL-AMOUNT        PIC S9(13)V99 COMP-3.
               10  WS-BL-CMP-AMOUNT    PIC S9(13)V99 COMP-3.
               10  WS-BL-DVP           PIC X(01).
               10  WS-BL-STATUS        PIC X(02).
               10  WS-BL-RULE          PIC X(03).
               10  WS-BL-OTHER-REF     PIC X(16).
               10  WS-BL-OTHER-AMT     PIC S9(13)V99 COMP-3.
      *----------------------------------------------------------------*
      * PER CURRENCY NET OF NON-DVP LEGS (R03)                         *
      *----------------------------------------------------------------*
       01  WS-NET-USED                 PIC S9(04) COMP  VALUE ZERO.
       01  WS-NET-TABLE.
           05  WS-NT OCCURS 20 TIMES INDEXED BY NT-IDX.
               10  WS-NT-CCY           PIC X(03).
               10  WS-NT-AMOUNT        PIC S9(15)V99 COMP-3.
               10  WS-NT-LEGS          PIC S9(07)    COMP-3.
               10  WS-NT-STATUS        PIC X(02).
               10  WS-NT-BANK-SUB      PIC S9(08)    COMP.
               10  WS-NT-BANK-AMT      PIC S9(15)V99 COMP-3.
      *----------------------------------------------------------------*
      * WORK FIELDS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK.
           05  WS-SUB                  PIC S9(08) COMP.
           05  WS-SUB2                 PIC S9(08) COMP.
           05  WS-BEST-SUB             PIC S9(08) COMP.
           05  WS-DIFF                 PIC S9(15)V99 COMP-3.
           05  WS-ABS-DIFF             PIC S9(15)V99 COMP-3.
      *    DIFFERENCE TO THE DIME FOR R02 (CHG12301)
           05  WS-DIFF-DIME            PIC S9(15)V9  COMP-3.
           05  WS-NET-DIFF             PIC S9(15)V99 COMP-3.
           05  WS-SIGNED-AMT           PIC S9(13)V99 COMP-3.
           05  WS-LAST-ACCT            PIC X(10)  VALUE LOW-VALUES.
           05  WS-LAST-DVP             PIC X(01)  VALUE 'N'.
           05  WS-LAST-TYPE            PIC X(02)  VALUE SPACES.
           05  WS-ITEM-ID              PIC X(16).
           05  WS-ITEM-CCY             PIC X(03).
           05  WS-ITEM-CAT             PIC X(02).
           05  WS-ITEM-BANK-AMT        PIC S9(15)V99 COMP-3.
           05  WS-ITEM-BOOK-AMT        PIC S9(15)V99 COMP-3.
           05  WS-NEW-AGE              PIC S9(05)    COMP-3.
           05  WS-NEW-LEVEL            PIC 9(01).
           05  WS-JOBNAME              PIC X(08)  VALUE SPACES.
           05  WS-MIN-SETTLE           PIC 9(08)  VALUE 99999999.
           05  WS-MAX-SETTLE           PIC 9(08)  VALUE ZERO.
       01  WS-COUNTERS.
           05  WS-BANK-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-READ            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-NO-CASH         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-STREET          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-NO-ACCT         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-DVP             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BOOK-NON-DVP         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-R01-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-R02-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-R03-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-R03-LEGS             PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-BU-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-KU-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AD-CNT               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UN-LEGS-IN-NET       PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MATCHED-ITEMS        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-UNMATCHED-ITEMS      PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CASHREC-OUT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CS-NEW               PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CS-AGED              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CS-SAME-DAY          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CS-CLOSED            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CS-OPEN-END          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CS-ESCALATED         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-MATCHED-AMT          PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-UNMATCHED-AMT        PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-BANK-NET             PIC S9(15)V99 COMP-3 VALUE ZERO.
           05  WS-BOOK-NET             PIC S9(15)V99 COMP-3 VALUE ZERO.
       COPY RCMATCH.
       COPY CMDATEW.
       COPY CMDTLNK.
       COPY CMJILNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
      *----------------------------------------------------------------*
      * STATISTICS PER CURRENCY (SYSOUT ONLY)                          *
      *----------------------------------------------------------------*
       01  WS-CS-USED                  PIC S9(04) COMP  VALUE ZERO.
       01  WS-CCY-STATS.
           05  WS-CS OCCURS 20 TIMES INDEXED BY CS-IDX.
               10  WS-CS-CCY           PIC X(03).
               10  WS-CS-BANK-CNT      PIC S9(07)    COMP-3.
               10  WS-CS-BANK-AMT      PIC S9(15)V99 COMP-3.
               10  WS-CS-BOOK-CNT      PIC S9(07)    COMP-3.
               10  WS-CS-BOOK-AMT      PIC S9(15)V99 COMP-3.
               10  WS-CS-UN-CNT        PIC S9(07)    COMP-3.
       01  WS-STAT-CCY                 PIC X(03).
       01  WS-STAT-SIDE                PIC X(01).
       01  WS-STAT-AMT                 PIC S9(15)V99 COMP-3.
       01  WS-STAT-UN                  PIC X(01).
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-CNT2            PIC ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-DISP-AMT2            PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-LOAD-BANK.
           PERFORM 2500-LOAD-BOOK.
           PERFORM 3000-MATCH-BY-REFERENCE.
           PERFORM 4000-MATCH-DTC-NET.
           PERFORM 5000-WRITE-RESULTS.
           PERFORM 6000-CASH-BREAKS.
           PERFORM 7000-CLOSE-RESOLVED.
           PERFORM 9000-TERMINATE.
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
               PERFORM 9999-ABEND
           END-IF.
           READ DATECARD-FILE INTO DC-DATE-CARD.
           IF WS-DATECARD-STATUS NOT = '00' OR NOT DC-VALID-CARD
               MOVE 'DATECARD' TO AB-DDNAME
               MOVE WS-DATECARD-STATUS TO AB-FILE-STATUS
               MOVE 1005 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'DATE CARD MISSING OR INVALID' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CLOSE DATECARD-FILE.
           CALL 'CMASM01' USING JI-JOB-INFO.
           MOVE JI-JOBNAME TO WS-JOBNAME.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE WS-PROGRAM-ID  TO AU-PROGRAM.
           MOVE 'START'        TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE DC-BUS-DATE    TO AU-BUS-DATE.
           MOVE SPACES         TO AU-KEY.
           MOVE 'SETTLEMENT CASH RECONCILIATION STARTED' TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT BANKTXN-FILE.
           IF WS-BANKTXN-STATUS NOT = '00'
               MOVE 'BANKTXN' TO AB-DDNAME
               MOVE WS-BANKTXN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT SETLIN-FILE.
           IF WS-SETLIN-STATUS NOT = '00'
               MOVE 'SETLIN' TO AB-DDNAME
               MOVE WS-SETLIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN I-O BRKMAST-FILE.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           OPEN OUTPUT CASHREC-FILE.
           IF WS-CASHREC-STATUS NOT = '00'
               MOVE 'CASHREC' TO AB-DDNAME
               MOVE WS-CASHREC-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE '1000-INITIALIZE' TO AB-PARAGRAPH
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
      * LOAD THE BANK TRANSACTIONS - SIGNED, CREDIT = CASH IN          *
      *================================================================*
       2000-LOAD-BANK.
           PERFORM 8000-READ-BANK.
           PERFORM UNTIL END-OF-BANK
               IF WS-BANK-USED NOT < WS-BANK-MAX
                   MOVE 'BANKTXN' TO AB-DDNAME
                   MOVE 1007 TO AB-ABEND-CODE
                   MOVE '2000-LOAD-BANK' TO AB-PARAGRAPH
                   MOVE 'BANK TRANSACTION TABLE FULL (5000)'
                                   TO AB-MESSAGE
                   PERFORM 9999-ABEND
               END-IF
               ADD 1 TO WS-BANK-USED
               SET BK-IDX TO WS-BANK-USED
               MOVE RCT-CCY         TO WS-BK-CCY (BK-IDX)
               MOVE RCT-VALUE-DATE  TO WS-BK-VALUE-DATE (BK-IDX)
               MOVE RCT-TYPE-CODE   TO WS-BK-TYPE-CODE (BK-IDX)
               IF RCT-AMOUNT NOT NUMERIC
                   DISPLAY 'RCB300 BANK AMOUNT NOT NUMERIC SEQ '
                           RCT-SEQ-NO ' - TAKEN AS ZERO'
                   MOVE ZERO TO RCT-AMOUNT
               END-IF
               IF RCT-DEBIT
                   COMPUTE WS-BK-AMOUNT (BK-IDX) = RCT-AMOUNT * -1
               ELSE
                   MOVE RCT-AMOUNT TO WS-BK-AMOUNT (BK-IDX)
               END-IF
               ADD WS-BK-AMOUNT (BK-IDX) TO WS-BANK-NET
               MOVE RCT-BANK-REF    TO WS-BK-BANK-REF (BK-IDX)
               MOVE RCT-CUST-REF    TO WS-BK-CUST-REF (BK-IDX)
               MOVE RCT-TEXT        TO WS-BK-TEXT (BK-IDX)
               MOVE RCT-SEQ-NO      TO WS-BK-SEQ (BK-IDX)
               MOVE 'UN'            TO WS-BK-STATUS (BK-IDX)
               MOVE 'BU '           TO WS-BK-RULE (BK-IDX)
               MOVE SPACES          TO WS-BK-OTHER-REF (BK-IDX)
               MOVE ZERO            TO WS-BK-OTHER-AMT (BK-IDX)
               PERFORM 8000-READ-BANK
           END-PERFORM.
      *================================================================*
      * LOAD THE SETTLED CASH LEGS OF CLIENT AND FIRM ACCOUNTS         *
      *================================================================*
       2500-LOAD-BOOK.
           PERFORM 8100-READ-SETTLED.
           PERFORM UNTIL END-OF-BOOK
               EVALUATE TRUE
                   WHEN ACT-CASH-CHANGE NOT NUMERIC
                       ADD 1 TO WS-BOOK-NO-CASH
                   WHEN ACT-CASH-CHANGE = ZERO
                       ADD 1 TO WS-BOOK-NO-CASH
                   WHEN OTHER
                       PERFORM 2600-ACCOUNT-LOOKUP
                       IF WS-LAST-TYPE = 'ST'
                           ADD 1 TO WS-BOOK-STREET
                       ELSE
                           PERFORM 2700-ADD-BOOK-LEG
                       END-IF
               END-EVALUATE
               PERFORM 8100-READ-SETTLED
           END-PERFORM.
      *----------------------------------------------------------------*
      * DVP FLAG AND ACCOUNT TYPE - ONE READ PER ACCOUNT CHANGE        *
      *----------------------------------------------------------------*
       2600-ACCOUNT-LOOKUP.
           IF ACT-ACCT-NO NOT = WS-LAST-ACCT
               PERFORM 2650-READ-ACCOUNT
           END-IF.
      *----------------------------------------------------------------*
       2650-READ-ACCOUNT.
      *----------------------------------------------------------------*
           MOVE ACT-ACCT-NO TO WS-LAST-ACCT ACCT-NO.
           READ ACCTMAST-FILE.
           EVALUATE TRUE
               WHEN ACCTMAST-OK
                   MOVE ACCT-TYPE TO WS-LAST-TYPE
                   IF ACCT-DVP
                       MOVE 'Y' TO WS-LAST-DVP
                   ELSE
                       MOVE 'N' TO WS-LAST-DVP
                   END-IF
               WHEN ACCTMAST-NOT-FOUND
                   ADD 1 TO WS-BOOK-NO-ACCT
                   DISPLAY 'RCB300 ACCOUNT ' ACT-ACCT-NO
                           ' NOT ON ACCTMAST - TAKEN AS NON-DVP'
                   MOVE ACT-ACCT-TYPE TO WS-LAST-TYPE
                   MOVE 'N' TO WS-LAST-DVP
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '2600-ACCOUNT-LOOKUP' TO AB-PARAGRAPH
                   MOVE ACT-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       2700-ADD-BOOK-LEG.
      *----------------------------------------------------------------*
           IF WS-BOOK-USED NOT < WS-BOOK-MAX
               MOVE 'SETLIN' TO AB-DDNAME
               MOVE 1007 TO AB-ABEND-CODE
               MOVE '2700-ADD-BOOK-LEG' TO AB-PARAGRAPH
               MOVE 'BOOK CASH LEG TABLE FULL (20000)' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           ADD 1 TO WS-BOOK-USED.
           SET BL-IDX TO WS-BOOK-USED.
           MOVE ACT-REF          TO WS-BL-REF (BL-IDX).
           MOVE ACT-ACCT-NO      TO WS-BL-ACCT (BL-IDX).
           MOVE ACT-TYPE         TO WS-BL-TYPE (BL-IDX).
           MOVE ACT-CCY          TO WS-BL-CCY (BL-IDX).
           MOVE ACT-SETTLE-DATE  TO WS-BL-SETTLE-DATE (BL-IDX).
           MOVE ACT-CASH-CHANGE  TO WS-BL-AMOUNT (BL-IDX)
                                    WS-BL-CMP-AMOUNT (BL-IDX).
           SET WC-IDX TO 1.
           SEARCH WS-WHOLE-CCY
               AT END
                   CONTINUE
               WHEN WS-WHOLE-CCY (WC-IDX) = ACT-CCY
                   COMPUTE WS-WHOLE-AMOUNT ROUNDED = ACT-CASH-CHANGE
                   MOVE WS-WHOLE-AMOUNT TO WS-BL-CMP-AMOUNT (BL-IDX)
           END-SEARCH.
           MOVE WS-LAST-DVP      TO WS-BL-DVP (BL-IDX).
           MOVE 'UN'             TO WS-BL-STATUS (BL-IDX).
           MOVE 'KU '            TO WS-BL-RULE (BL-IDX).
           MOVE SPACES           TO WS-BL-OTHER-REF (BL-IDX).
           MOVE ZERO             TO WS-BL-OTHER-AMT (BL-IDX).
           ADD ACT-CASH-CHANGE   TO WS-BOOK-NET.
           IF WS-LAST-DVP = 'Y'
               ADD 1 TO WS-BOOK-DVP
           ELSE
               ADD 1 TO WS-BOOK-NON-DVP
           END-IF.
           IF ACT-SETTLE-DATE NUMERIC
               IF ACT-SETTLE-DATE < WS-MIN-SETTLE
                   MOVE ACT-SETTLE-DATE TO WS-MIN-SETTLE
               END-IF
               IF ACT-SETTLE-DATE > WS-MAX-SETTLE
                   MOVE ACT-SETTLE-DATE TO WS-MAX-SETTLE
               END-IF
           END-IF.
      *================================================================*
      * R01 / R02 - CUSTOMER REFERENCE = ACTIVITY REFERENCE            *
      *================================================================*
       3000-MATCH-BY-REFERENCE.
           PERFORM VARYING BK-IDX FROM 1 BY 1
                   UNTIL BK-IDX > WS-BANK-USED
               IF WS-BK-CUST-REF (BK-IDX) NOT = SPACES
                   PERFORM 3100-FIND-EXACT
                   IF NOT ITEM-FOUND
                       PERFORM 3200-FIND-TOLERANCE
                   END-IF
                   IF NOT ITEM-FOUND
                       PERFORM 3300-FIND-SAME-REF
                   END-IF
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
      * R01 - SAME REF, SAME CURRENCY, SAME AMOUNT                     *
      *----------------------------------------------------------------*
       3100-FIND-EXACT.
           MOVE 'N' TO WS-FOUND-SW.
           PERFORM VARYING BL-IDX FROM 1 BY 1
                   UNTIL BL-IDX > WS-BOOK-USED OR ITEM-FOUND
               IF WS-BL-STATUS (BL-IDX) = 'UN'
               AND WS-BL-REF (BL-IDX) = WS-BK-CUST-REF (BK-IDX)
               AND WS-BL-CCY (BL-IDX) = WS-BK-CCY (BK-IDX)
               AND WS-BL-CMP-AMOUNT (BL-IDX) = WS-BK-AMOUNT (BK-IDX)
                   MOVE 'Y' TO WS-FOUND-SW
                   MOVE 'MA'  TO WS-BK-STATUS (BK-IDX)
                                 WS-BL-STATUS (BL-IDX)
                   MOVE 'R01' TO WS-BK-RULE (BK-IDX)
                                 WS-BL-RULE (BL-IDX)
                   PERFORM 3900-LINK-PAIR
                   ADD 1 TO WS-R01-CNT
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
      * R02 - SAME REF, SAME CURRENCY, WITHIN TOLERANCE                *
      *----------------------------------------------------------------*
       3200-FIND-TOLERANCE.
           MOVE 'N' TO WS-FOUND-SW.
           PERFORM VARYING BL-IDX FROM 1 BY 1
                   UNTIL BL-IDX > WS-BOOK-USED OR ITEM-FOUND
               IF WS-BL-STATUS (BL-IDX) = 'UN'
               AND WS-BL-REF (BL-IDX) = WS-BK-CUST-REF (BK-IDX)
               AND WS-BL-CCY (BL-IDX) = WS-BK-CCY (BK-IDX)
                   COMPUTE WS-DIFF = WS-BK-AMOUNT (BK-IDX)
                                   - WS-BL-CMP-AMOUNT (BL-IDX)
                   IF WS-DIFF < ZERO
                       COMPUTE WS-ABS-DIFF = WS-DIFF * -1
                   ELSE
                       MOVE WS-DIFF TO WS-ABS-DIFF
                   END-IF
                   MOVE WS-ABS-DIFF TO WS-DIFF-DIME
                   IF WS-DIFF-DIME NOT > WS-R02-TOLERANCE
                       MOVE 'Y' TO WS-FOUND-SW
                       MOVE 'MT'  TO WS-BK-STATUS (BK-IDX)
                                     WS-BL-STATUS (BL-IDX)
                       MOVE 'R02' TO WS-BK-RULE (BK-IDX)
                                     WS-BL-RULE (BL-IDX)
                       PERFORM 3900-LINK-PAIR
                       ADD 1 TO WS-R02-CNT
                   END-IF
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
      * SAME REF BUT THE AMOUNT (OR CURRENCY) IS OUT - AMOUNT          *
      * DIFFERENCE.  BOTH ITEMS STAY UNMATCHED, ONE BREAK (THE REF).   *
      *----------------------------------------------------------------*
       3300-FIND-SAME-REF.
           MOVE 'N' TO WS-FOUND-SW.
           PERFORM VARYING BL-IDX FROM 1 BY 1
                   UNTIL BL-IDX > WS-BOOK-USED OR ITEM-FOUND
               IF WS-BL-STATUS (BL-IDX) = 'UN'
               AND WS-BL-RULE (BL-IDX) = 'KU '
               AND WS-BL-REF (BL-IDX) = WS-BK-CUST-REF (BK-IDX)
                   MOVE 'Y' TO WS-FOUND-SW
                   MOVE 'AD ' TO WS-BK-RULE (BK-IDX)
                                 WS-BL-RULE (BL-IDX)
                   PERFORM 3900-LINK-PAIR
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
      * CROSS REFERENCE OF A BANK ITEM AND A BOOK LEG                  *
      *----------------------------------------------------------------*
       3900-LINK-PAIR.
           MOVE WS-BL-REF (BL-IDX)    TO WS-BK-OTHER-REF (BK-IDX).
           MOVE WS-BL-AMOUNT (BL-IDX) TO WS-BK-OTHER-AMT (BK-IDX).
           IF WS-BK-CUST-REF (BK-IDX) NOT = SPACES
               MOVE WS-BK-CUST-REF (BK-IDX) TO WS-BL-OTHER-REF (BL-IDX)
           ELSE
               MOVE WS-BK-BANK-REF (BK-IDX) TO WS-BL-OTHER-REF (BL-IDX)
           END-IF.
           MOVE WS-BK-AMOUNT (BK-IDX) TO WS-BL-OTHER-AMT (BL-IDX).
      *================================================================*
      * R03 - DTC NET SETTLEMENT.  NON-DVP ACCOUNTS SETTLE THROUGH THE *
      * DTC NET SETTLEMENT - THE BANK SHOWS ONE LINE PER CURRENCY.     *
      *================================================================*
       4000-MATCH-DTC-NET.
           MOVE ZERO TO WS-NET-USED.
           PERFORM VARYING BL-IDX FROM 1 BY 1
                   UNTIL BL-IDX > WS-BOOK-USED
               IF WS-BL-STATUS (BL-IDX) = 'UN'
               AND WS-BL-RULE (BL-IDX) = 'KU '
               AND WS-BL-DVP (BL-IDX) NOT = 'Y'
                   PERFORM 4100-ADD-TO-NET
               END-IF
           END-PERFORM.
           PERFORM VARYING NT-IDX FROM 1 BY 1
                   UNTIL NT-IDX > WS-NET-USED
               PERFORM 4200-FIND-NET-LINE
           END-PERFORM.
      *----------------------------------------------------------------*
       4100-ADD-TO-NET.
      *----------------------------------------------------------------*
           SET NT-IDX TO 1.
           SEARCH WS-NT
               AT END
                   IF WS-NET-USED NOT < 20
                       MOVE 1007 TO AB-ABEND-CODE
                       MOVE '4100-ADD-TO-NET' TO AB-PARAGRAPH
                       MOVE 'MORE THAN 20 SETTLEMENT CURRENCIES'
                                       TO AB-MESSAGE
                       PERFORM 9999-ABEND
                   END-IF
                   ADD 1 TO WS-NET-USED
                   SET NT-IDX TO WS-NET-USED
                   MOVE WS-BL-CCY (BL-IDX)    TO WS-NT-CCY (NT-IDX)
                   MOVE WS-BL-AMOUNT (BL-IDX) TO WS-NT-AMOUNT (NT-IDX)
                   MOVE 1                     TO WS-NT-LEGS (NT-IDX)
                   MOVE 'UN'                  TO WS-NT-STATUS (NT-IDX)
                   MOVE ZERO                  TO WS-NT-BANK-SUB (NT-IDX)
                                                 WS-NT-BANK-AMT (NT-IDX)
               WHEN NT-IDX > WS-NET-USED
                   ADD 1 TO WS-NET-USED
                   MOVE WS-BL-CCY (BL-IDX)    TO WS-NT-CCY (NT-IDX)
                   MOVE WS-BL-AMOUNT (BL-IDX) TO WS-NT-AMOUNT (NT-IDX)
                   MOVE 1                     TO WS-NT-LEGS (NT-IDX)
                   MOVE 'UN'                  TO WS-NT-STATUS (NT-IDX)
                   MOVE ZERO                  TO WS-NT-BANK-SUB (NT-IDX)
                                                 WS-NT-BANK-AMT (NT-IDX)
               WHEN WS-NT-CCY (NT-IDX) = WS-BL-CCY (BL-IDX)
                   ADD WS-BL-AMOUNT (BL-IDX)  TO WS-NT-AMOUNT (NT-IDX)
                   ADD 1                      TO WS-NT-LEGS (NT-IDX)
           END-SEARCH.
      *----------------------------------------------------------------*
      * THE NET LINE: TYPE 195 (OR 495 FOR A NET PAYMENT), TEXT        *
      * BEGINNING 'DTC NET', SAME CURRENCY, STILL UNMATCHED            *
      *----------------------------------------------------------------*
       4200-FIND-NET-LINE.
           MOVE 'N' TO WS-FOUND-SW.
           PERFORM VARYING BK-IDX FROM 1 BY 1
                   UNTIL BK-IDX > WS-BANK-USED OR ITEM-FOUND
               IF WS-BK-STATUS (BK-IDX) = 'UN'
               AND WS-BK-RULE (BK-IDX) = 'BU '
               AND (WS-BK-TYPE-CODE (BK-IDX) = WS-NET-TYPE-CR
                    OR WS-BK-TYPE-CODE (BK-IDX) = WS-NET-TYPE-DR)
               AND WS-BK-TEXT (BK-IDX) (1:7) = WS-NET-TEXT
               AND WS-BK-CCY (BK-IDX) = WS-NT-CCY (NT-IDX)
                   MOVE 'Y' TO WS-FOUND-SW
                   SET WS-NT-BANK-SUB (NT-IDX) TO BK-IDX
                   MOVE WS-BK-AMOUNT (BK-IDX) TO WS-NT-BANK-AMT (NT-IDX)
                   COMPUTE WS-NET-DIFF = WS-BK-AMOUNT (BK-IDX)
                                       - WS-NT-AMOUNT (NT-IDX)
                   IF WS-NET-DIFF < ZERO
                       COMPUTE WS-NET-DIFF = WS-NET-DIFF * -1
                   END-IF
                   MOVE WS-NT-AMOUNT (NT-IDX)
                                   TO WS-BK-OTHER-AMT (BK-IDX)
                   STRING 'NET ' WS-NT-CCY (NT-IDX)
                          DELIMITED BY SIZE
                          INTO WS-BK-OTHER-REF (BK-IDX)
                   IF WS-NET-DIFF NOT > WS-R03-TOLERANCE
                       MOVE 'MN'  TO WS-BK-STATUS (BK-IDX)
                                     WS-NT-STATUS (NT-IDX)
                       MOVE 'R03' TO WS-BK-RULE (BK-IDX)
                       ADD 1 TO WS-R03-CNT
                       PERFORM 4300-MARK-NET-LEGS
                   ELSE
                       MOVE 'AD ' TO WS-BK-RULE (BK-IDX)
                       MOVE 'AD'  TO WS-NT-STATUS (NT-IDX)
                       PERFORM 4300-MARK-NET-LEGS
                   END-IF
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
      * THE LEGS OF THE NET TAKE THE RESULT OF THE NET LINE.  WHEN THE *
      * NET IS OUT, THE LEGS ARE LEFT UNMATCHED UNDER THE NET BREAK -  *
      * THEY DO NOT OPEN BREAKS OF THEIR OWN.                          *
      *----------------------------------------------------------------*
       4300-MARK-NET-LEGS.
           PERFORM VARYING BL-IDX2 FROM 1 BY 1
                   UNTIL BL-IDX2 > WS-BOOK-USED
               IF WS-BL-STATUS (BL-IDX2) = 'UN'
               AND WS-BL-RULE (BL-IDX2) = 'KU '
               AND WS-BL-DVP (BL-IDX2) NOT = 'Y'
               AND WS-BL-CCY (BL-IDX2) = WS-NT-CCY (NT-IDX)
                   STRING 'NET ' WS-NT-CCY (NT-IDX)
                          DELIMITED BY SIZE
                          INTO WS-BL-OTHER-REF (BL-IDX2)
                   IF WS-NT-STATUS (NT-IDX) = 'MN'
                       MOVE 'MN'  TO WS-BL-STATUS (BL-IDX2)
                       MOVE 'R03' TO WS-BL-RULE (BL-IDX2)
                       ADD 1 TO WS-R03-LEGS
                   ELSE
                       MOVE 'NT ' TO WS-BL-RULE (BL-IDX2)
                       ADD 1 TO WS-UN-LEGS-IN-NET
                   END-IF
               END-IF
           END-PERFORM.
      *================================================================*
      * RESULTS - ONE RECORD PER BANK ITEM, THEN ONE PER BOOK LEG      *
      *================================================================*
       5000-WRITE-RESULTS.
           PERFORM VARYING BK-IDX FROM 1 BY 1
                   UNTIL BK-IDX > WS-BANK-USED
               MOVE SPACES              TO RCM-MATCH-REC
               MOVE DC-BUS-DATE         TO RCM-BUS-DATE
               MOVE 'B'                 TO RCM-SIDE
               MOVE WS-BK-STATUS (BK-IDX) TO RCM-MATCH-STATUS
               MOVE WS-BK-RULE (BK-IDX)   TO RCM-RULE-ID
               MOVE WS-BK-CCY (BK-IDX)    TO RCM-CCY
               MOVE WS-BK-VALUE-DATE (BK-IDX) TO RCM-VALUE-DATE
               IF WS-BK-CUST-REF (BK-IDX) NOT = SPACES
                   MOVE WS-BK-CUST-REF (BK-IDX) TO RCM-REF
               ELSE
                   MOVE WS-BK-BANK-REF (BK-IDX) TO RCM-REF
               END-IF
               MOVE WS-BK-OTHER-REF (BK-IDX) TO RCM-OTHER-REF
               MOVE WS-BK-AMOUNT (BK-IDX)    TO RCM-AMOUNT
               MOVE WS-BK-OTHER-AMT (BK-IDX) TO RCM-OTHER-AMOUNT
               COMPUTE RCM-DIFF-AMOUNT = WS-BK-AMOUNT (BK-IDX)
                                       - WS-BK-OTHER-AMT (BK-IDX)
               MOVE WS-BK-TYPE-CODE (BK-IDX) TO RCM-TYPE-CODE
               MOVE WS-BK-TEXT (BK-IDX)      TO RCM-TEXT
               PERFORM 5900-WRITE-CASHREC
               MOVE 'B'                   TO WS-STAT-SIDE
               MOVE WS-BK-CCY (BK-IDX)    TO WS-STAT-CCY
               MOVE WS-BK-AMOUNT (BK-IDX) TO WS-STAT-AMT
               MOVE WS-BK-STATUS (BK-IDX) (1:1) TO WS-STAT-UN
               PERFORM 5950-CCY-STATISTICS
               IF WS-BK-STATUS (BK-IDX) = 'UN'
                   ADD 1 TO WS-UNMATCHED-ITEMS
                   ADD WS-BK-AMOUNT (BK-IDX) TO WS-UNMATCHED-AMT
               ELSE
                   ADD 1 TO WS-MATCHED-ITEMS
                   ADD WS-BK-AMOUNT (BK-IDX) TO WS-MATCHED-AMT
               END-IF
           END-PERFORM.
           PERFORM VARYING BL-IDX FROM 1 BY 1
                   UNTIL BL-IDX > WS-BOOK-USED
               MOVE SPACES              TO RCM-MATCH-REC
               MOVE DC-BUS-DATE         TO RCM-BUS-DATE
               MOVE 'K'                 TO RCM-SIDE
               MOVE WS-BL-STATUS (BL-IDX) TO RCM-MATCH-STATUS
               MOVE WS-BL-RULE (BL-IDX)   TO RCM-RULE-ID
               MOVE WS-BL-CCY (BL-IDX)    TO RCM-CCY
               MOVE WS-BL-SETTLE-DATE (BL-IDX) TO RCM-VALUE-DATE
               MOVE WS-BL-REF (BL-IDX)       TO RCM-REF
               MOVE WS-BL-OTHER-REF (BL-IDX) TO RCM-OTHER-REF
               MOVE WS-BL-AMOUNT (BL-IDX)    TO RCM-AMOUNT
               MOVE WS-BL-OTHER-AMT (BL-IDX) TO RCM-OTHER-AMOUNT
               IF WS-BL-STATUS (BL-IDX) = 'MN'
               OR WS-BL-RULE (BL-IDX) = 'NT '
               OR WS-BL-RULE (BL-IDX) = 'KU '
                   MOVE ZERO TO RCM-DIFF-AMOUNT
               ELSE
                   COMPUTE RCM-DIFF-AMOUNT = WS-BL-OTHER-AMT (BL-IDX)
                                           - WS-BL-AMOUNT (BL-IDX)
               END-IF
               MOVE WS-BL-TYPE (BL-IDX)      TO RCM-TYPE-CODE
               STRING WS-BL-ACCT (BL-IDX) ' '
                      WS-BL-TYPE (BL-IDX) ' DVP='
                      WS-BL-DVP (BL-IDX)
                      DELIMITED BY SIZE INTO RCM-TEXT
               PERFORM 5900-WRITE-CASHREC
               MOVE 'K'                   TO WS-STAT-SIDE
               MOVE WS-BL-CCY (BL-IDX)    TO WS-STAT-CCY
               MOVE WS-BL-AMOUNT (BL-IDX) TO WS-STAT-AMT
               MOVE WS-BL-STATUS (BL-IDX) (1:1) TO WS-STAT-UN
               PERFORM 5950-CCY-STATISTICS
               IF WS-BL-STATUS (BL-IDX) = 'UN'
                   ADD 1 TO WS-UNMATCHED-ITEMS
               END-IF
           END-PERFORM.
      *----------------------------------------------------------------*
       5900-WRITE-CASHREC.
      *----------------------------------------------------------------*
           WRITE CASHREC-OUT-REC FROM RCM-MATCH-REC.
           IF WS-CASHREC-STATUS NOT = '00'
               MOVE 'CASHREC' TO AB-DDNAME
               MOVE WS-CASHREC-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '5900-WRITE-CASHREC' TO AB-PARAGRAPH
               MOVE RCM-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           ADD 1 TO WS-CASHREC-OUT.
      *----------------------------------------------------------------*
       5950-CCY-STATISTICS.
      *----------------------------------------------------------------*
           SET CS-IDX TO 1.
           SEARCH WS-CS
               AT END
                   DISPLAY 'RCB300 MORE THAN 20 CURRENCIES - '
                           WS-STAT-CCY ' NOT IN STATISTICS'
               WHEN CS-IDX > WS-CS-USED
                   ADD 1 TO WS-CS-USED
                   MOVE WS-STAT-CCY TO WS-CS-CCY (CS-IDX)
                   MOVE ZERO TO WS-CS-BANK-CNT (CS-IDX)
                                WS-CS-BANK-AMT (CS-IDX)
                                WS-CS-BOOK-CNT (CS-IDX)
                                WS-CS-BOOK-AMT (CS-IDX)
                                WS-CS-UN-CNT (CS-IDX)
                   PERFORM 5960-ADD-CCY-STATISTICS
               WHEN WS-CS-CCY (CS-IDX) = WS-STAT-CCY
                   PERFORM 5960-ADD-CCY-STATISTICS
           END-SEARCH.
      *----------------------------------------------------------------*
       5960-ADD-CCY-STATISTICS.
      *----------------------------------------------------------------*
           IF WS-STAT-SIDE = 'B'
               ADD 1           TO WS-CS-BANK-CNT (CS-IDX)
               ADD WS-STAT-AMT TO WS-CS-BANK-AMT (CS-IDX)
           ELSE
               ADD 1           TO WS-CS-BOOK-CNT (CS-IDX)
               ADD WS-STAT-AMT TO WS-CS-BOOK-AMT (CS-IDX)
           END-IF.
           IF WS-STAT-UN = 'U'
               ADD 1 TO WS-CS-UN-CNT (CS-IDX)
           END-IF.
      *================================================================*
      * CASH BREAKS - EVERY ITEM STILL UN OPENS OR AGES A CS BREAK:    *
      *   BANK ITEMS  BU (OR AD WHEN A SAME-REF / NET PARTNER EXISTS)  *
      *   BOOK LEGS   KU (AD LEGS ARE COVERED BY THE BANK ITEM'S       *
      *               BREAK, LEGS OF AN OUT-OF-BALANCE NET BY THE NET) *
      *   A NET WITH NO BANK LINE AT ALL IS ONE KU BREAK 'DTC NET'.    *
      *================================================================*
       6000-CASH-BREAKS.
           PERFORM VARYING BK-IDX FROM 1 BY 1
                   UNTIL BK-IDX > WS-BANK-USED
               IF WS-BK-STATUS (BK-IDX) = 'UN'
                   MOVE WS-BK-CCY (BK-IDX) TO WS-ITEM-CCY
                   IF WS-BK-CUST-REF (BK-IDX) NOT = SPACES
                       MOVE WS-BK-CUST-REF (BK-IDX) TO WS-ITEM-ID
                   ELSE
                       IF WS-BK-BANK-REF (BK-IDX) NOT = SPACES
                           MOVE WS-BK-BANK-REF (BK-IDX) TO WS-ITEM-ID
                       ELSE
                           MOVE SPACES TO WS-ITEM-ID
                           STRING 'BAISEQ' WS-BK-SEQ (BK-IDX)
                                  DELIMITED BY SIZE INTO WS-ITEM-ID
                       END-IF
                   END-IF
                   MOVE WS-BK-AMOUNT (BK-IDX)    TO WS-ITEM-BANK-AMT
                   MOVE WS-BK-OTHER-AMT (BK-IDX) TO WS-ITEM-BOOK-AMT
                   IF WS-BK-RULE (BK-IDX) = 'AD '
                       MOVE 'AD' TO WS-ITEM-CAT
                       ADD 1 TO WS-AD-CNT
                   ELSE
                       MOVE 'BU' TO WS-ITEM-CAT
                       ADD 1 TO WS-BU-CNT
                   END-IF
                   PERFORM 6100-UPSERT-CASH-BREAK
               END-IF
           END-PERFORM.
           PERFORM VARYING BL-IDX FROM 1 BY 1
                   UNTIL BL-IDX > WS-BOOK-USED
               IF WS-BL-STATUS (BL-IDX) = 'UN'
               AND WS-BL-RULE (BL-IDX) = 'KU '
               AND WS-BL-DVP (BL-IDX) = 'Y'
                   MOVE WS-BL-CCY (BL-IDX)  TO WS-ITEM-CCY
                   MOVE WS-BL-REF (BL-IDX)  TO WS-ITEM-ID
                   MOVE ZERO                TO WS-ITEM-BANK-AMT
                   MOVE WS-BL-AMOUNT (BL-IDX) TO WS-ITEM-BOOK-AMT
                   MOVE 'KU' TO WS-ITEM-CAT
                   ADD 1 TO WS-KU-CNT
                   PERFORM 6100-UPSERT-CASH-BREAK
               END-IF
           END-PERFORM.
           PERFORM VARYING NT-IDX FROM 1 BY 1
                   UNTIL NT-IDX > WS-NET-USED
               IF WS-NT-STATUS (NT-IDX) = 'UN'
                   MOVE WS-NT-CCY (NT-IDX)    TO WS-ITEM-CCY
                   MOVE 'DTC NET'             TO WS-ITEM-ID
                   MOVE ZERO                  TO WS-ITEM-BANK-AMT
                   MOVE WS-NT-AMOUNT (NT-IDX) TO WS-ITEM-BOOK-AMT
                   MOVE 'KU' TO WS-ITEM-CAT
                   ADD 1 TO WS-KU-CNT
                   PERFORM 6100-UPSERT-CASH-BREAK
      *            ITS LEGS ARE COVERED BY THIS ONE BREAK
                   PERFORM VARYING BL-IDX FROM 1 BY 1
                           UNTIL BL-IDX > WS-BOOK-USED
                       IF WS-BL-STATUS (BL-IDX) = 'UN'
                       AND WS-BL-RULE (BL-IDX) = 'KU '
                       AND WS-BL-DVP (BL-IDX) NOT = 'Y'
                       AND WS-BL-CCY (BL-IDX) = WS-NT-CCY (NT-IDX)
                           ADD 1 TO WS-UN-LEGS-IN-NET
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
           IF WS-BU-CNT + WS-KU-CNT + WS-AD-CNT > ZERO
               IF WS-RETURN-CODE < 4
                   MOVE 4 TO WS-RETURN-CODE
               END-IF
           END-IF.
      *----------------------------------------------------------------*
      * KEY: 'CS' + CCY + BLANK + ITEM ID                              *
      *----------------------------------------------------------------*
       6100-UPSERT-CASH-BREAK.
           MOVE SPACES       TO RBM-KEY.
           MOVE 'CS'         TO RBM-BREAK-CLASS.
           MOVE WS-ITEM-CCY  TO RBM-DEPOSITORY (1:3).
           MOVE WS-ITEM-ID   TO RBM-ITEM-ID.
           READ BRKMAST-FILE.
           EVALUATE TRUE
               WHEN BRKMAST-NOT-FOUND
                   PERFORM 6200-NEW-CASH-BREAK
               WHEN BRKMAST-OK
                   IF RBM-OPEN AND RBM-LAST-SEEN-DATE = DC-BUS-DATE
      *                SAME ITEM ID TWICE TODAY - ADD IT IN
                       ADD 1 TO WS-CS-SAME-DAY
                       ADD WS-ITEM-BANK-AMT TO RBM-STREET-AMOUNT
                       ADD WS-ITEM-BOOK-AMT TO RBM-BOOK-AMOUNT
                       PERFORM 6400-AMOUNTS-AND-REWRITE
                   ELSE
                       IF RBM-OPEN OR RBM-WRITTEN-OFF
                           PERFORM 6300-AGE-CASH-BREAK
                       ELSE
                           PERFORM 6250-REOPEN-CASH-BREAK
                       END-IF
                   END-IF
               WHEN OTHER
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '6100-UPSERT-CASH-BREAK' TO AB-PARAGRAPH
                   MOVE RBM-KEY TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       6200-NEW-CASH-BREAK.
      *----------------------------------------------------------------*
           MOVE SPACES           TO RBM-BREAK-REC (23:).
           MOVE WS-ITEM-CAT      TO RBM-CATEGORY.
           MOVE 'O'              TO RBM-STATUS.
           MOVE DC-BUS-DATE      TO RBM-FIRST-SEEN-DATE
                                    RBM-LAST-SEEN-DATE.
           MOVE ZERO             TO RBM-CLOSED-DATE RBM-AGE-BUS-DAYS
                                    RBM-ESCALATION-LVL
                                    RBM-STREET-QTY RBM-BOOK-QTY
                                    RBM-DIFF-QTY.
           MOVE WS-ITEM-BANK-AMT TO RBM-STREET-AMOUNT.
           MOVE WS-ITEM-BOOK-AMT TO RBM-BOOK-AMOUNT.
           MOVE WS-ITEM-CCY      TO RBM-CCY.
           MOVE 'CASHRECN'       TO RBM-ASSIGNED-TO.
           MOVE WS-JOBNAME       TO RBM-LAST-UPD-JOB.
           PERFORM 6500-SET-AMOUNTS.
           STRING 'OPENED ' DC-BUS-DATE ' BOOK DAY '
                  DC-PREV-BUS-DATE
                  DELIMITED BY SIZE INTO RBM-COMMENT.
           WRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '6200-NEW-CASH-BREAK' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           ADD 1 TO WS-CS-NEW.
      *----------------------------------------------------------------*
       6250-REOPEN-CASH-BREAK.
      *----------------------------------------------------------------*
           MOVE WS-ITEM-CAT      TO RBM-CATEGORY.
           MOVE 'O'              TO RBM-STATUS.
           MOVE DC-BUS-DATE      TO RBM-FIRST-SEEN-DATE
                                    RBM-LAST-SEEN-DATE.
           MOVE ZERO             TO RBM-CLOSED-DATE RBM-AGE-BUS-DAYS
                                    RBM-ESCALATION-LVL.
           MOVE WS-ITEM-BANK-AMT TO RBM-STREET-AMOUNT.
           MOVE WS-ITEM-BOOK-AMT TO RBM-BOOK-AMOUNT.
           MOVE 'CASHRECN'       TO RBM-ASSIGNED-TO.
           MOVE SPACES           TO RBM-COMMENT.
           STRING 'REOPENED ' DC-BUS-DATE
                  DELIMITED BY SIZE INTO RBM-COMMENT.
           ADD 1 TO WS-CS-NEW.
           PERFORM 6400-AMOUNTS-AND-REWRITE.
      *----------------------------------------------------------------*
      * STILL UNMATCHED - AGE AND ESCALATE (SAME LEVELS AS RCB400)     *
      *----------------------------------------------------------------*
       6300-AGE-CASH-BREAK.
           MOVE WS-ITEM-CAT      TO RBM-CATEGORY.
           MOVE DC-BUS-DATE      TO RBM-LAST-SEEN-DATE.
           MOVE WS-ITEM-BANK-AMT TO RBM-STREET-AMOUNT.
           MOVE WS-ITEM-BOOK-AMT TO RBM-BOOK-AMOUNT.
           MOVE 'DIFB'              TO DT-FUNCTION.
           MOVE 'NYSE'              TO DT-CALENDAR.
           MOVE RBM-FIRST-SEEN-DATE TO DT-DATE-1.
           MOVE DC-BUS-DATE         TO DT-DATE-2.
           CALL 'CMU010' USING DT-DATE-PARMS.
           IF DT-OK
               MOVE DT-RESULT-NUM TO WS-NEW-AGE
           ELSE
               COMPUTE WS-NEW-AGE = RBM-AGE-BUS-DAYS + 1
           END-IF.
           IF WS-NEW-AGE > 999
               MOVE 999 TO WS-NEW-AGE
           END-IF.
           MOVE WS-NEW-AGE TO RBM-AGE-BUS-DAYS.
           EVALUATE TRUE
               WHEN RBM-AGE-BUS-DAYS >= 10
                   MOVE 3 TO WS-NEW-LEVEL
               WHEN RBM-AGE-BUS-DAYS >= 5
                   MOVE 2 TO WS-NEW-LEVEL
               WHEN RBM-AGE-BUS-DAYS >= 3
                   MOVE 1 TO WS-NEW-LEVEL
               WHEN OTHER
                   MOVE 0 TO WS-NEW-LEVEL
           END-EVALUATE.
           IF WS-NEW-LEVEL > RBM-ESCALATION-LVL
               MOVE WS-NEW-LEVEL TO RBM-ESCALATION-LVL
               ADD 1 TO WS-CS-ESCALATED
               MOVE SPACES TO RBM-COMMENT
               STRING 'ESCALATED TO LEVEL ' WS-NEW-LEVEL ' ON '
                      DC-BUS-DATE
                      DELIMITED BY SIZE INTO RBM-COMMENT
               MOVE 'WRIT'      TO AU-FUNCTION
               MOVE 'ESCALATE'  TO AU-EVENT
               MOVE 'W'         TO AU-SEVERITY
               MOVE RBM-KEY     TO AU-KEY
               MOVE RBM-COMMENT TO AU-MESSAGE
               CALL 'CMU060' USING AU-AUDIT-PARMS
           END-IF.
           ADD 1 TO WS-CS-AGED.
           PERFORM 6400-AMOUNTS-AND-REWRITE.
      *----------------------------------------------------------------*
       6400-AMOUNTS-AND-REWRITE.
      *----------------------------------------------------------------*
           MOVE WS-JOBNAME TO RBM-LAST-UPD-JOB.
           PERFORM 6500-SET-AMOUNTS.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '6400-AMOUNTS-AND-REWRITE' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *----------------------------------------------------------------*
      * CASH BREAK VALUE IS THE ABSOLUTE DIFFERENCE IN THE ITEM        *
      * CURRENCY - NOT CONVERTED (MKT-VALUE-USD FIELD REUSED)          *
      *----------------------------------------------------------------*
       6500-SET-AMOUNTS.
           COMPUTE RBM-DIFF-AMOUNT = RBM-STREET-AMOUNT
                                   - RBM-BOOK-AMOUNT.
           IF RBM-DIFF-AMOUNT < ZERO
               COMPUTE RBM-MKT-VALUE-USD = RBM-DIFF-AMOUNT * -1
           ELSE
               MOVE RBM-DIFF-AMOUNT TO RBM-MKT-VALUE-USD
           END-IF.
      *================================================================*
      * CLOSE THE CASH BREAKS NOT SEEN TODAY                           *
      *================================================================*
       7000-CLOSE-RESOLVED.
           MOVE SPACES TO RBM-KEY.
           MOVE 'CS'   TO RBM-BREAK-CLASS.
           START BRKMAST-FILE KEY IS NOT LESS THAN RBM-KEY.
           EVALUATE TRUE
               WHEN BRKMAST-OK
                   MOVE 'N' TO WS-BROWSE-EOF-SW
               WHEN BRKMAST-NOT-FOUND
                   MOVE 'Y' TO WS-BROWSE-EOF-SW
               WHEN OTHER
                   MOVE 'RCBRKMST' TO AB-DDNAME
                   MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '7000-CLOSE-RESOLVED' TO AB-PARAGRAPH
                   MOVE 'START FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
           PERFORM UNTIL END-OF-BROWSE
               READ BRKMAST-FILE NEXT RECORD
               EVALUATE TRUE
                   WHEN BRKMAST-EOF
                       MOVE 'Y' TO WS-BROWSE-EOF-SW
                   WHEN NOT BRKMAST-OK
                       MOVE 'RCBRKMST' TO AB-DDNAME
                       MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
                       MOVE 1002 TO AB-ABEND-CODE
                       MOVE '7000-CLOSE-RESOLVED' TO AB-PARAGRAPH
                       MOVE 'READ NEXT FAILED' TO AB-MESSAGE
                       PERFORM 9999-ABEND
                   WHEN NOT RBM-CASH-BREAK
                       MOVE 'Y' TO WS-BROWSE-EOF-SW
                   WHEN RBM-OPEN
                        AND RBM-LAST-SEEN-DATE < DC-BUS-DATE
                       PERFORM 7100-CLOSE-CASH-BREAK
                   WHEN RBM-OPEN
                       ADD 1 TO WS-CS-OPEN-END
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.
      *----------------------------------------------------------------*
       7100-CLOSE-CASH-BREAK.
      *----------------------------------------------------------------*
           MOVE 'C'         TO RBM-STATUS.
           MOVE DC-BUS-DATE TO RBM-CLOSED-DATE.
           MOVE WS-JOBNAME  TO RBM-LAST-UPD-JOB.
           MOVE SPACES      TO RBM-COMMENT.
           STRING 'CLEARED - NOT UNMATCHED ON ' DC-BUS-DATE
                  DELIMITED BY SIZE INTO RBM-COMMENT.
           REWRITE RBM-BREAK-REC.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '7100-CLOSE-CASH-BREAK' TO AB-PARAGRAPH
               MOVE RBM-KEY TO AB-KEY
               MOVE 'REWRITE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           ADD 1 TO WS-CS-CLOSED.
      *================================================================*
      * I/O                                                            *
      *================================================================*
       8000-READ-BANK.
           READ BANKTXN-FILE.
           EVALUATE TRUE
               WHEN BANKTXN-OK
                   ADD 1 TO WS-BANK-READ
               WHEN BANKTXN-EOF
                   MOVE 'Y' TO WS-BANK-EOF-SW
               WHEN OTHER
                   MOVE 'BANKTXN' TO AB-DDNAME
                   MOVE WS-BANKTXN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8000-READ-BANK' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8100-READ-SETTLED.
      *----------------------------------------------------------------*
           READ SETLIN-FILE.
           EVALUATE TRUE
               WHEN SETLIN-OK
                   ADD 1 TO WS-BOOK-READ
               WHEN SETLIN-EOF
                   MOVE 'Y' TO WS-BOOK-EOF-SW
               WHEN OTHER
                   MOVE 'SETLIN' TO AB-DDNAME
                   MOVE WS-SETLIN-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE '8100-READ-SETTLED' TO AB-PARAGRAPH
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   PERFORM 9999-ABEND
           END-EVALUATE.
      *----------------------------------------------------------------*
       8500-POST-TOTAL.
      *----------------------------------------------------------------*
           MOVE 'POST'         TO CT-FUNCTION.
           MOVE DC-BUS-DATE    TO CT-BUS-DATE.
           MOVE WS-PROGRAM-ID  TO CT-PROGRAM.
           MOVE 'RCB300'       TO CT-STAGE.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE '8500-POST-TOTAL' TO AB-PARAGRAPH
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE BANKTXN-FILE SETLIN-FILE ACCTMAST-FILE.
           CLOSE BRKMAST-FILE.
           IF WS-BRKMAST-STATUS NOT = '00'
               MOVE 'RCBRKMST' TO AB-DDNAME
               MOVE WS-BRKMAST-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           CLOSE CASHREC-FILE.
           IF WS-CASHREC-STATUS NOT = '00'
               MOVE 'CASHREC' TO AB-DDNAME
               MOVE WS-CASHREC-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE '9000-TERMINATE' TO AB-PARAGRAPH
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
               PERFORM 9999-ABEND
           END-IF.
           MOVE 'BANK-IN'         TO CT-COUNTER-NAME.
           MOVE WS-BANK-READ      TO CT-COUNT.
           MOVE WS-BANK-NET       TO CT-AMOUNT.
           MOVE ZERO              TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'BOOK-IN'         TO CT-COUNTER-NAME.
           MOVE WS-BOOK-USED      TO CT-COUNT.
           MOVE WS-BOOK-NET       TO CT-AMOUNT.
           MOVE ZERO              TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'MATCHED'         TO CT-COUNTER-NAME.
           MOVE WS-MATCHED-ITEMS  TO CT-COUNT.
           MOVE WS-MATCHED-AMT    TO CT-AMOUNT.
           MOVE ZERO              TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'UNMATCHED'       TO CT-COUNTER-NAME.
           MOVE WS-UNMATCHED-ITEMS TO CT-COUNT.
           MOVE WS-UNMATCHED-AMT  TO CT-AMOUNT.
           MOVE ZERO              TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'CASHREC-OUT'     TO CT-COUNTER-NAME.
           MOVE WS-CASHREC-OUT    TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'R01-MATCHED'     TO CT-COUNTER-NAME.
           MOVE WS-R01-CNT        TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'R02-MATCHED'     TO CT-COUNTER-NAME.
           MOVE WS-R02-CNT        TO CT-COUNT.
           PERFORM 8500-POST-TOTAL.
           MOVE 'R03-NET-LINES'   TO CT-COUNTER-NAME.
           MOVE WS-R03-CNT        TO CT-COUNT.
           PERFORM 8500-POST-TOTAL.
           MOVE 'CS-BRK-OPEN'     TO CT-COUNTER-NAME.
           MOVE WS-CS-OPEN-END    TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           MOVE 'CS-BRK-CLOSED'   TO CT-COUNTER-NAME.
           MOVE WS-CS-CLOSED      TO CT-COUNT.
           MOVE ZERO              TO CT-AMOUNT CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL.
           PERFORM 9100-DISPLAY-STATISTICS.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE SPACES         TO AU-KEY.
           IF WS-RETURN-CODE > ZERO
               MOVE 'W' TO AU-SEVERITY
               MOVE 'UNMATCHED CASH ITEMS - SEE RCR310' TO AU-MESSAGE
           ELSE
               MOVE 'I' TO AU-SEVERITY
               MOVE 'SETTLEMENT CASH RECONCILED' TO AU-MESSAGE
           END-IF.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO AU-FUNCTION.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           MOVE 'CLOS'         TO CT-FUNCTION.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
      *----------------------------------------------------------------*
       9100-DISPLAY-STATISTICS.
      *----------------------------------------------------------------*
           DISPLAY '************************************************'.
           DISPLAY '* RCB300 - SETTLEMENT CASH RECONCILIATION      *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           DISPLAY ' BOOK SETTLE DATES        : ' WS-MIN-SETTLE ' - '
                   WS-MAX-SETTLE.
           MOVE WS-BANK-READ TO WS-DISP-CNT.
           DISPLAY ' BANK TRANSACTIONS READ   : ' WS-DISP-CNT.
           MOVE WS-BANK-NET TO WS-DISP-AMT.
           DISPLAY '   BANK NET               : ' WS-DISP-AMT.
           MOVE WS-BOOK-READ TO WS-DISP-CNT.
           DISPLAY ' SETTLED LEGS READ        : ' WS-DISP-CNT.
           MOVE WS-BOOK-NO-CASH TO WS-DISP-CNT.
           DISPLAY '   NO CASH MOVEMENT       : ' WS-DISP-CNT.
           MOVE WS-BOOK-STREET TO WS-DISP-CNT.
           DISPLAY '   STREET ACCOUNT LEGS    : ' WS-DISP-CNT.
           MOVE WS-BOOK-USED TO WS-DISP-CNT.
           DISPLAY '   CASH LEGS RECONCILED   : ' WS-DISP-CNT.
           MOVE WS-BOOK-DVP TO WS-DISP-CNT.
           DISPLAY '     DVP ACCOUNTS         : ' WS-DISP-CNT.
           MOVE WS-BOOK-NON-DVP TO WS-DISP-CNT.
           DISPLAY '     NON-DVP ACCOUNTS     : ' WS-DISP-CNT.
           MOVE WS-BOOK-NO-ACCT TO WS-DISP-CNT.
           DISPLAY '     ACCOUNT NOT ON FILE  : ' WS-DISP-CNT.
           MOVE WS-BOOK-NET TO WS-DISP-AMT.
           DISPLAY '   BOOK NET               : ' WS-DISP-AMT.
           MOVE WS-R01-CNT TO WS-DISP-CNT.
           DISPLAY ' R01 EXACT (MA)           : ' WS-DISP-CNT.
           MOVE WS-R02-CNT TO WS-DISP-CNT.
           DISPLAY ' R02 TOLERANCE (MT)       : ' WS-DISP-CNT.
           MOVE WS-R03-CNT TO WS-DISP-CNT.
           DISPLAY ' R03 DTC NET LINES (MN)   : ' WS-DISP-CNT.
           MOVE WS-R03-LEGS TO WS-DISP-CNT.
           DISPLAY '   LEGS IN MATCHED NETS   : ' WS-DISP-CNT.
           MOVE WS-UN-LEGS-IN-NET TO WS-DISP-CNT.
           DISPLAY '   LEGS IN UNMATCHED NETS : ' WS-DISP-CNT.
           MOVE WS-BU-CNT TO WS-DISP-CNT.
           DISPLAY ' BU BANK UNMATCHED        : ' WS-DISP-CNT.
           MOVE WS-KU-CNT TO WS-DISP-CNT.
           DISPLAY ' KU BOOK UNMATCHED        : ' WS-DISP-CNT.
           MOVE WS-AD-CNT TO WS-DISP-CNT.
           DISPLAY ' AD AMOUNT DIFFERENCE     : ' WS-DISP-CNT.
           MOVE WS-CASHREC-OUT TO WS-DISP-CNT.
           DISPLAY ' RESULT RECORDS WRITTEN   : ' WS-DISP-CNT.
           MOVE WS-CS-NEW TO WS-DISP-CNT.
           DISPLAY ' CASH BREAKS OPENED       : ' WS-DISP-CNT.
           MOVE WS-CS-AGED TO WS-DISP-CNT.
           DISPLAY ' CASH BREAKS AGED         : ' WS-DISP-CNT.
           MOVE WS-CS-SAME-DAY TO WS-DISP-CNT.
           DISPLAY ' SAME ITEM ID TWICE       : ' WS-DISP-CNT.
           MOVE WS-CS-ESCALATED TO WS-DISP-CNT.
           DISPLAY ' CASH BREAKS ESCALATED    : ' WS-DISP-CNT.
           MOVE WS-CS-CLOSED TO WS-DISP-CNT.
           DISPLAY ' CASH BREAKS CLOSED       : ' WS-DISP-CNT.
           MOVE WS-CS-OPEN-END TO WS-DISP-CNT.
           DISPLAY ' CASH BREAKS OPEN         : ' WS-DISP-CNT.
           DISPLAY ' CCY  BANK ITEMS          BANK AMOUNT'
                   '  BOOK LEGS          BOOK AMOUNT  UNMATCHED'.
           PERFORM VARYING CS-IDX FROM 1 BY 1
                   UNTIL CS-IDX > WS-CS-USED
               MOVE WS-CS-BANK-CNT (CS-IDX) TO WS-DISP-CNT2
               MOVE WS-CS-BANK-AMT (CS-IDX) TO WS-DISP-AMT2
               DISPLAY ' ' WS-CS-CCY (CS-IDX) '  ' WS-DISP-CNT2 ' '
                       WS-DISP-AMT2 WITH NO ADVANCING
               MOVE WS-CS-BOOK-CNT (CS-IDX) TO WS-DISP-CNT2
               MOVE WS-CS-BOOK-AMT (CS-IDX) TO WS-DISP-AMT2
               DISPLAY '    ' WS-DISP-CNT2 ' ' WS-DISP-AMT2
                       WITH NO ADVANCING
               MOVE WS-CS-UN-CNT (CS-IDX) TO WS-DISP-CNT2
               DISPLAY '    ' WS-DISP-CNT2
           END-PERFORM.
           DISPLAY ' RETURN CODE              : ' WS-RETURN-CODE.
           DISPLAY '************************************************'.
      *================================================================*
       9999-ABEND.
      *================================================================*
           MOVE WS-PROGRAM-ID TO AB-PROGRAM.
           DISPLAY 'RCB300 ABENDING - CODE ' AB-ABEND-CODE
                   ' PARA ' AB-PARAGRAPH.
           DISPLAY 'RCB300 DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS
                   ' KEY ' AB-KEY.
           DISPLAY 'RCB300 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
