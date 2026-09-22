       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SRB100.
       AUTHOR.        R J KOWALSKI.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  OCTOBER 1987.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : SRB100                                            *
      * DESCRIPTION: STOCK RECORD ACTIVITY VALIDATION.                 *
      *              READS THE SORTED DAILY ACTIVITY (TRADE CAPTURE +  *
      *              CORPORATE ACTIONS + ADJUSTMENTS) AND SPLITS IT    *
      *              INTO VALID AND REJECTED LEGS.  A REFERENCE IS     *
      *              ONLY PASSED WHEN ALL OF ITS LEGS ARE VALID AND    *
      *              THE SHARE QUANTITIES OF THE LEGS NET TO ZERO, SO  *
      *              THE STOCK RECORD STAYS IN BALANCE.                *
      *              TWO PASSES OVER THE INPUT: PASS 1 VALIDATES AND   *
      *              ACCUMULATES PER REFERENCE, PASS 2 WRITES.         *
      *----------------------------------------------------------------*
      * JOB/STEP   : MSSRD010 / STEP020  (IKJEFT01 - CALLS CMD010)     *
      * INPUT      : DATECARD - BUSINESS DATE CARD                     *
      *              ACTVIN   - MSEC.PROD.SR.ACTV.SORTED(+1)  (SRACTV) *
      *              ACCTMAST - ACCOUNT MASTER KSDS (READ RANDOM)      *
      * OUTPUT     : ACTVOK   - MSEC.PROD.SR.ACTV.VALID(+1)   (SRACTV) *
      *              ACTVREJ  - MSEC.PROD.SR.ACTV.REJECT(+1) (SRREJCT) *
      * CALLS      : CMD010 (SECURITY MASTER), CMU050, CMU060, CMU080, *
      *              CMASM01, CMASM02                                  *
      * RETURN CODE: 0 CLEAN, 4 REJECTS OR WARNINGS                    *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   DESCRIPTION                           TICKET   *
      * ---------- ---- ------------------------------------- -------- *
      * 1987-10-12 RJK  ORIGINAL                                       *
      * 1989-03-20 RJK  DATE CARD INSTEAD OF SYSTEM DATE      CHG00212 *
      * 1991-06-03 DWB  REJECT ENTIRE REF WHEN ONE LEG BAD    CHG00987 *
      * 1996-04-22 DWB  SECURITY MASTER NOW DB2 (CMD010)      CHG02215 *
      * 1998-11-02 TLM  Y2K - CCYYMMDD DATES                  CHG04471 *
      * 2001-07-16 KAP  DECIMALIZATION - PRICE 8 DECIMALS     CHG08811 *
      * 2002-03-08 KAP  REJECT SEVERITY / WARNINGS            CHG09930 *
      * 2009-12-14 SPA  CURRENCY VALIDATION                   CHG19002 *
      * 2010-04-05 SPA  CORPORATE ACTION SOURCE 'CA'          CHG19870 *
      * 2014-08-11 SPA  HASHED REFERENCE TABLE (WAS SEARCH)   CHG26120 *
      * 2019-02-25 MHC  CONTROL TOTALS BY SOURCE FOR CMB090   CHG33410 *
      * 2024-02-12 NVR  T+1 - SETTLE DATE EDIT RELAXED        CHG40290 *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATECARD-FILE  ASSIGN TO DATECARD
                  FILE STATUS IS WS-DATECARD-STATUS.
           SELECT ACTVIN-FILE    ASSIGN TO ACTVIN
                  FILE STATUS IS WS-ACTVIN-STATUS.
           SELECT ACTVOK-FILE    ASSIGN TO ACTVOK
                  FILE STATUS IS WS-ACTVOK-STATUS.
           SELECT ACTVREJ-FILE   ASSIGN TO ACTVREJ
                  FILE STATUS IS WS-ACTVREJ-STATUS.
           SELECT ACCTMAST-FILE  ASSIGN TO ACCTMAST
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCTMAST-KEY
                  FILE STATUS IS WS-ACCTMAST-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  DATECARD-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DATECARD-REC                PIC X(80).
       FD  ACTVIN-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACTVIN-REC                  PIC X(200).
       FD  ACTVOK-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACTVOK-REC                  PIC X(200).
       FD  ACTVREJ-FILE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ACTVREJ-REC                 PIC X(250).
       FD  ACCTMAST-FILE.
       01  ACCTMAST-REC.
           05  ACCTMAST-KEY            PIC X(10).
           05  FILLER                  PIC X(290).
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'SRB100'.
      *----------------------------------------------------------------*
      * FILE STATUS AREAS                                              *
      *----------------------------------------------------------------*
       01  WS-FILE-STATUSES.
           05  WS-DATECARD-STATUS      PIC X(02)  VALUE '00'.
           05  WS-ACTVIN-STATUS        PIC X(02)  VALUE '00'.
               88  ACTVIN-OK                      VALUE '00'.
               88  ACTVIN-EOF                     VALUE '10'.
           05  WS-ACTVOK-STATUS        PIC X(02)  VALUE '00'.
           05  WS-ACTVREJ-STATUS       PIC X(02)  VALUE '00'.
           05  WS-ACCTMAST-STATUS      PIC X(02)  VALUE '00'.
               88  ACCTMAST-FOUND                 VALUE '00'.
               88  ACCTMAST-NOTFND                VALUE '23'.
      *----------------------------------------------------------------*
      * SWITCHES                                                       *
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05  WS-EOF-SW               PIC X(01)  VALUE 'N'.
               88  END-OF-ACTIVITY                VALUE 'Y'.
           05  WS-PASS-SW              PIC X(01)  VALUE '1'.
               88  PASS-ONE                       VALUE '1'.
               88  PASS-TWO                       VALUE '2'.
           05  WS-FOUND-SW             PIC X(01)  VALUE 'N'.
               88  ENTRY-FOUND                    VALUE 'Y'.
           05  WS-SHARE-MOVE-SW        PIC X(01)  VALUE 'N'.
               88  SHARE-MOVEMENT                 VALUE 'Y'.
           05  WS-CASH-ONLY-SW         PIC X(01)  VALUE 'N'.
               88  CASH-ONLY-ACTIVITY             VALUE 'Y'.
      *----------------------------------------------------------------*
      * COUNTERS AND HASH TOTALS                                       *
      *----------------------------------------------------------------*
       01  WS-COUNTERS.
           05  WS-READ-P1              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-READ-P2              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-TC-IN-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CA-IN-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-AJ-IN-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-VALID-CNT            PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REJECT-CNT           PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-WARNING-CNT          PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REF-CNT              PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-REF-UNBAL-CNT        PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-CMD010-CALLS         PIC S9(09) COMP-3 VALUE ZERO.
           05  WS-SEC-CACHE-HITS       PIC S9(09) COMP-3 VALUE ZERO.
       01  WS-HASH-TOTALS.
           05  WS-TC-IN-AMT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-TC-IN-QTY            PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-CA-IN-AMT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-CA-IN-QTY            PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-AJ-IN-AMT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-AJ-IN-QTY            PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-VALID-AMT            PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-VALID-QTY            PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
           05  WS-REJECT-AMT           PIC S9(15)V99    COMP-3
                                                     VALUE ZERO.
           05  WS-REJECT-QTY           PIC S9(15)V9(04) COMP-3
                                                     VALUE ZERO.
       01  WS-ABS-QTY                  PIC S9(11)V9(04) COMP-3.
      *----------------------------------------------------------------*
      * REJECT CODE TALLY (FOR THE STATISTICS DISPLAY)                 *
      *----------------------------------------------------------------*
       01  WS-REJ-TALLY-AREA.
           05  WS-REJ-TALLY-USED       PIC S9(04) COMP  VALUE ZERO.
           05  WS-REJ-TALLY OCCURS 40 TIMES
                            INDEXED BY RT-IDX.
               10  WS-RT-CODE          PIC X(04).
               10  WS-RT-COUNT         PIC S9(07) COMP-3.
      *----------------------------------------------------------------*
      * CURRENT LEG RESULT                                             *
      *----------------------------------------------------------------*
       01  WS-LEG-RESULT.
           05  WS-LEG-REJ-CODE         PIC X(04)  VALUE SPACES.
           05  WS-LEG-REJ-TEXT         PIC X(30)  VALUE SPACES.
           05  WS-LEG-WARN-CODE        PIC X(04)  VALUE SPACES.
       01  WS-REF-KEY.
           05  WS-REF-KEY-SOURCE       PIC X(02).
           05  WS-REF-KEY-REF          PIC X(16).
      *----------------------------------------------------------------*
      * REFERENCE TABLE - OPEN ADDRESSED HASH, LINEAR PROBE.           *
      * SIZE IS A PRIME.  DO NOT CHANGE WITHOUT CHANGING WS-REF-PRIME. *
      *----------------------------------------------------------------*
       01  WS-REF-PRIME                PIC S9(08) COMP VALUE +20011.
       01  WS-REF-MAX-LOAD             PIC S9(08) COMP VALUE +18000.
       01  WS-REF-HASH-WORK.
           05  WS-HASH-ACCUM           PIC S9(09) COMP.
           05  WS-HASH-SLOT            PIC S9(08) COMP.
           05  WS-HASH-QUOT            PIC S9(09) COMP.
           05  WS-HASH-POS             PIC S9(04) COMP.
           05  WS-HASH-PROBES          PIC S9(08) COMP.
           05  WS-HASH-MAX-PROBES      PIC S9(08) COMP VALUE ZERO.
       01  WS-REF-TABLE.
           05  WS-REF-ENTRY OCCURS 20011 TIMES.
               10  WS-RE-KEY           PIC X(18).
               10  WS-RE-LEG-CNT       PIC S9(04) COMP.
               10  WS-RE-QTY-SUM       PIC S9(13)V9(04) COMP-3.
               10  WS-RE-REJ-CODE      PIC X(04).
       01  WS-RE-SUB                   PIC S9(08) COMP.
      *----------------------------------------------------------------*
      * SECURITY CACHE - AVOIDS A SECOND CMD010 CALL IN PASS 2         *
      *----------------------------------------------------------------*
       01  WS-SEC-CACHE-AREA.
           05  WS-SEC-CACHE-USED       PIC S9(04) COMP  VALUE ZERO.
           05  WS-SEC-CACHE-MAX        PIC S9(04) COMP  VALUE +3000.
           05  WS-SEC-CACHE OCCURS 3000 TIMES
                            INDEXED BY SC-IDX.
               10  WS-SC-CUSIP         PIC X(09).
               10  WS-SC-FOUND         PIC X(01).
               10  WS-SC-TYPE          PIC X(02).
               10  WS-SC-STATUS        PIC X(01).
               10  WS-SC-CCY           PIC X(03).
       01  WS-SEC-RESULT.
           05  WS-SR-FOUND             PIC X(01).
               88  SEC-WAS-FOUND                  VALUE 'Y'.
           05  WS-SR-TYPE              PIC X(02).
           05  WS-SR-STATUS            PIC X(01).
           05  WS-SR-CCY               PIC X(03).
      *----------------------------------------------------------------*
      * ACTIVITY TYPE TABLE: TYPE / GL CODE / MOVEMENT CLASS / SIGN    *
      *   CLASS S = SHARE MOVEMENT, C = CASH ONLY, B = BOTH ALLOWED    *
      *   SIGN  + = LEG 1 MUST BE POSITIVE, - = NEGATIVE, * = EITHER   *
      *----------------------------------------------------------------*
       01  WS-ACT-TYPE-VALUES.
           05  FILLER  PIC X(09)  VALUE 'BUYTBUYS+'.
           05  FILLER  PIC X(09)  VALUE 'SELTSELS-'.
           05  FILLER  PIC X(09)  VALUE 'SSLTSSLS-'.
           05  FILLER  PIC X(09)  VALUE 'BCVTBCVS+'.
           05  FILLER  PIC X(09)  VALUE 'XBYTXBYS-'.
           05  FILLER  PIC X(09)  VALUE 'XSLTXSLS+'.
           05  FILLER  PIC X(09)  VALUE 'DIVCDIVC*'.
           05  FILLER  PIC X(09)  VALUE 'WHTCWHTC*'.
           05  FILLER  PIC X(09)  VALUE 'SDVCSDVS+'.
           05  FILLER  PIC X(09)  VALUE 'SPLCSPLS+'.
           05  FILLER  PIC X(09)  VALUE 'CILCCILC*'.
           05  FILLER  PIC X(09)  VALUE 'MGOCMGOS-'.
           05  FILLER  PIC X(09)  VALUE 'MGCCMGCC*'.
           05  FILLER  PIC X(09)  VALUE 'QAJAQAJB*'.
       01  WS-ACT-TYPE-TABLE REDEFINES WS-ACT-TYPE-VALUES.
           05  WS-AT-ENTRY OCCURS 14 TIMES INDEXED BY AT-IDX.
               10  WS-AT-TYPE          PIC X(03).
               10  WS-AT-GL-CODE       PIC X(04).
               10  WS-AT-CLASS         PIC X(01).
               10  WS-AT-SIGN          PIC X(01).
      *----------------------------------------------------------------*
      * VALID CURRENCIES                                               *
      *----------------------------------------------------------------*
       01  WS-CCY-VALUES               PIC X(18)
                                       VALUE 'USDEURGBPJPYCADCHF'.
       01  WS-CCY-TABLE REDEFINES WS-CCY-VALUES.
           05  WS-CCY-ENTRY OCCURS 6 TIMES INDEXED BY CCY-IDX
                                       PIC X(03).
      *----------------------------------------------------------------*
      * COPYBOOKS                                                      *
      *----------------------------------------------------------------*
       COPY SRACTV.
       COPY SRREJCT.
       COPY CMACCT.
       COPY CMSECMS.
       COPY CMDATEW.
       COPY CMSECLNK.
       COPY CMABLNK.
       COPY CMAULNK.
       COPY CMCTLNK.
       COPY CMJILNK.
       COPY CMTSLNK.
       01  WS-DISPLAY-FIELDS.
           05  WS-DISP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-DISP-AMT             PIC -ZZZ,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
      *================================================================*
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PASS-ONE THRU 2000-EXIT.
           PERFORM 3000-PASS-TWO THRU 3000-EXIT.
           PERFORM 9000-TERMINATE THRU 9000-EXIT.
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
           MOVE 'STOCK RECORD ACTIVITY VALIDATION STARTED'
                               TO AU-MESSAGE.
           CALL 'CMU060' USING AU-AUDIT-PARMS.
           OPEN INPUT ACCTMAST-FILE.
           IF WS-ACCTMAST-STATUS NOT = '00'
               MOVE 'ACCTMAST' TO AB-DDNAME
               MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT ACTVOK-FILE.
           IF WS-ACTVOK-STATUS NOT = '00'
               MOVE 'ACTVOK' TO AB-DDNAME
               MOVE WS-ACTVOK-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           OPEN OUTPUT ACTVREJ-FILE.
           IF WS-ACTVREJ-STATUS NOT = '00'
               MOVE 'ACTVREJ' TO AB-DDNAME
               MOVE WS-ACTVREJ-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           PERFORM 1100-CLEAR-REF-TABLE THRU 1100-EXIT
               VARYING WS-RE-SUB FROM 1 BY 1
               UNTIL WS-RE-SUB > WS-REF-PRIME.
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-CLEAR-REF-TABLE.
      *----------------------------------------------------------------*
           MOVE SPACES TO WS-RE-KEY (WS-RE-SUB).
           MOVE ZERO   TO WS-RE-LEG-CNT (WS-RE-SUB)
                          WS-RE-QTY-SUM (WS-RE-SUB).
           MOVE SPACES TO WS-RE-REJ-CODE (WS-RE-SUB).
       1100-EXIT.
           EXIT.
      *================================================================*
      * PASS 1 - VALIDATE EVERY LEG AND ACCUMULATE BY REFERENCE        *
      *================================================================*
       2000-PASS-ONE.
           SET PASS-ONE TO TRUE.
           PERFORM 8000-OPEN-ACTVIN THRU 8000-EXIT.
           PERFORM 8100-READ-ACTVIN THRU 8100-EXIT.
           PERFORM 2100-PASS-ONE-LEG THRU 2100-EXIT
               UNTIL END-OF-ACTIVITY.
           PERFORM 8200-CLOSE-ACTVIN THRU 8200-EXIT.
           PERFORM 2500-COUNT-UNBALANCED THRU 2500-EXIT
               VARYING WS-RE-SUB FROM 1 BY 1
               UNTIL WS-RE-SUB > WS-REF-PRIME.
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2100-PASS-ONE-LEG.
      *----------------------------------------------------------------*
           ADD 1 TO WS-READ-P1.
           IF ACT-QTY-CHANGE NUMERIC
               IF ACT-QTY-CHANGE < ZERO
                   COMPUTE WS-ABS-QTY = ACT-QTY-CHANGE * -1
               ELSE
                   MOVE ACT-QTY-CHANGE TO WS-ABS-QTY
               END-IF
           ELSE
               MOVE ZERO TO WS-ABS-QTY
           END-IF.
           EVALUATE TRUE
               WHEN ACT-FROM-TRADE
                   ADD 1 TO WS-TC-IN-CNT
                   ADD WS-ABS-QTY TO WS-TC-IN-QTY
                   IF ACT-CASH-CHANGE NUMERIC
                       ADD ACT-CASH-CHANGE TO WS-TC-IN-AMT
                   END-IF
               WHEN ACT-FROM-CORP-ACTION
                   ADD 1 TO WS-CA-IN-CNT
                   ADD WS-ABS-QTY TO WS-CA-IN-QTY
                   IF ACT-CASH-CHANGE NUMERIC
                       ADD ACT-CASH-CHANGE TO WS-CA-IN-AMT
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-AJ-IN-CNT
                   ADD WS-ABS-QTY TO WS-AJ-IN-QTY
                   IF ACT-CASH-CHANGE NUMERIC
                       ADD ACT-CASH-CHANGE TO WS-AJ-IN-AMT
                   END-IF
           END-EVALUATE.
           PERFORM 4000-VALIDATE-LEG THRU 4000-EXIT.
           PERFORM 2200-ACCUM-REF THRU 2200-EXIT.
           PERFORM 8100-READ-ACTVIN THRU 8100-EXIT.
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-ACCUM-REF.
      *----------------------------------------------------------------*
           MOVE ACT-SOURCE TO WS-REF-KEY-SOURCE.
           MOVE ACT-REF    TO WS-REF-KEY-REF.
           PERFORM 2300-HASH-LOOKUP THRU 2300-EXIT.
           IF NOT ENTRY-FOUND
               IF WS-REF-CNT NOT < WS-REF-MAX-LOAD
                   MOVE 1007 TO AB-ABEND-CODE
                   MOVE WS-REF-KEY TO AB-KEY
                   MOVE 'REFERENCE TABLE FULL - INCREASE WS-REF-PRIME'
                                   TO AB-MESSAGE
                   GO TO 9999-ABEND
               END-IF
               MOVE WS-REF-KEY TO WS-RE-KEY (WS-HASH-SLOT)
               ADD 1 TO WS-REF-CNT
           END-IF.
           ADD 1 TO WS-RE-LEG-CNT (WS-HASH-SLOT).
           IF ACT-QTY-CHANGE NUMERIC
               ADD ACT-QTY-CHANGE TO WS-RE-QTY-SUM (WS-HASH-SLOT)
           END-IF.
           IF WS-LEG-REJ-CODE NOT = SPACES
           AND WS-RE-REJ-CODE (WS-HASH-SLOT) = SPACES
               MOVE WS-LEG-REJ-CODE TO WS-RE-REJ-CODE (WS-HASH-SLOT)
           END-IF.
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * HASH THE 18 BYTE KEY.  SLOT RETURNED IN WS-HASH-SLOT, FOUND    *
      * SWITCH SET WHEN THE KEY IS ALREADY IN THE TABLE.               *
      *----------------------------------------------------------------*
       2300-HASH-LOOKUP.
           MOVE 'N'  TO WS-FOUND-SW.
           MOVE ZERO TO WS-HASH-ACCUM.
           PERFORM VARYING WS-HASH-POS FROM 1 BY 1
                   UNTIL WS-HASH-POS > 18
               COMPUTE WS-HASH-ACCUM =
                   WS-HASH-ACCUM * 31
                 + FUNCTION ORD (WS-REF-KEY (WS-HASH-POS:1))
               DIVIDE WS-HASH-ACCUM BY WS-REF-PRIME
                   GIVING WS-HASH-QUOT REMAINDER WS-HASH-ACCUM
           END-PERFORM.
           COMPUTE WS-HASH-SLOT = WS-HASH-ACCUM + 1.
           MOVE ZERO TO WS-HASH-PROBES.
           PERFORM UNTIL ENTRY-FOUND
                      OR WS-RE-KEY (WS-HASH-SLOT) = SPACES
               IF WS-RE-KEY (WS-HASH-SLOT) = WS-REF-KEY
                   MOVE 'Y' TO WS-FOUND-SW
               ELSE
                   ADD 1 TO WS-HASH-SLOT WS-HASH-PROBES
                   IF WS-HASH-SLOT > WS-REF-PRIME
                       MOVE 1 TO WS-HASH-SLOT
                   END-IF
               END-IF
           END-PERFORM.
           IF WS-HASH-PROBES > WS-HASH-MAX-PROBES
               MOVE WS-HASH-PROBES TO WS-HASH-MAX-PROBES
           END-IF.
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2500-COUNT-UNBALANCED.
      *----------------------------------------------------------------*
           IF WS-RE-KEY (WS-RE-SUB) NOT = SPACES
               IF WS-RE-QTY-SUM (WS-RE-SUB) NOT = ZERO
                   ADD 1 TO WS-REF-UNBAL-CNT
                   IF WS-RE-REJ-CODE (WS-RE-SUB) = SPACES
                       MOVE 'Q001' TO WS-RE-REJ-CODE (WS-RE-SUB)
                   END-IF
               END-IF
           END-IF.
       2500-EXIT.
           EXIT.
      *================================================================*
      * PASS 2 - RE-READ, DECIDE AND WRITE                             *
      *================================================================*
       3000-PASS-TWO.
           SET PASS-TWO TO TRUE.
           MOVE 'N' TO WS-EOF-SW.
           PERFORM 8000-OPEN-ACTVIN THRU 8000-EXIT.
           PERFORM 8100-READ-ACTVIN THRU 8100-EXIT.
           PERFORM 3100-PASS-TWO-LEG THRU 3100-EXIT
               UNTIL END-OF-ACTIVITY.
           PERFORM 8200-CLOSE-ACTVIN THRU 8200-EXIT.
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-PASS-TWO-LEG.
      *----------------------------------------------------------------*
           ADD 1 TO WS-READ-P2.
           PERFORM 4000-VALIDATE-LEG THRU 4000-EXIT.
           MOVE ACT-SOURCE TO WS-REF-KEY-SOURCE.
           MOVE ACT-REF    TO WS-REF-KEY-REF.
           PERFORM 2300-HASH-LOOKUP THRU 2300-EXIT.
           IF NOT ENTRY-FOUND
               MOVE 1008 TO AB-ABEND-CODE
               MOVE WS-REF-KEY TO AB-KEY
               MOVE 'REFERENCE NOT FOUND IN PASS 2 - INPUT CHANGED'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF WS-LEG-REJ-CODE = SPACES
               EVALUATE WS-RE-REJ-CODE (WS-HASH-SLOT)
                   WHEN SPACES
                       CONTINUE
                   WHEN 'Q001'
                       MOVE 'Q001' TO WS-LEG-REJ-CODE
                       MOVE 'LEGS DO NOT NET TO ZERO QTY'
                                   TO WS-LEG-REJ-TEXT
                   WHEN OTHER
                       MOVE 'Q002' TO WS-LEG-REJ-CODE
                       MOVE 'COMPANION LEG REJECTED'
                                   TO WS-LEG-REJ-TEXT
               END-EVALUATE
           END-IF.
           IF WS-LEG-REJ-CODE = SPACES
               PERFORM 3200-WRITE-VALID THRU 3200-EXIT
           ELSE
               PERFORM 3300-WRITE-REJECT THRU 3300-EXIT
           END-IF.
           PERFORM 8100-READ-ACTVIN THRU 8100-EXIT.
       3100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-WRITE-VALID.
      *----------------------------------------------------------------*
           WRITE ACTVOK-REC FROM ACT-ACTIVITY-REC.
           IF WS-ACTVOK-STATUS NOT = '00'
               MOVE 'ACTVOK' TO AB-DDNAME
               MOVE WS-ACTVOK-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE ACT-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-VALID-CNT.
           ADD ACT-CASH-CHANGE TO WS-VALID-AMT.
           IF ACT-QTY-CHANGE < ZERO
               SUBTRACT ACT-QTY-CHANGE FROM WS-VALID-QTY
           ELSE
               ADD ACT-QTY-CHANGE TO WS-VALID-QTY
           END-IF.
           IF WS-LEG-WARN-CODE NOT = SPACES
               ADD 1 TO WS-WARNING-CNT
               MOVE WS-LEG-WARN-CODE TO WS-LEG-REJ-CODE
               PERFORM 3400-TALLY-CODE THRU 3400-EXIT
           END-IF.
       3200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3300-WRITE-REJECT.
      *----------------------------------------------------------------*
           MOVE SPACES           TO SRJ-REJECT-REC.
           MOVE ACTVIN-REC       TO SRJ-ACTIVITY.
           MOVE DC-BUS-DATE      TO SRJ-BUS-DATE.
           MOVE WS-LEG-REJ-CODE  TO SRJ-CODE.
           MOVE 'E'              TO SRJ-SEVERITY.
           MOVE WS-LEG-REJ-TEXT  TO SRJ-TEXT.
           WRITE ACTVREJ-REC FROM SRJ-REJECT-REC.
           IF WS-ACTVREJ-STATUS NOT = '00'
               MOVE 'ACTVREJ' TO AB-DDNAME
               MOVE WS-ACTVREJ-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE ACT-REF TO AB-KEY
               MOVE 'WRITE FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           ADD 1 TO WS-REJECT-CNT.
           IF ACT-CASH-CHANGE NUMERIC
               ADD ACT-CASH-CHANGE TO WS-REJECT-AMT
           END-IF.
           IF ACT-QTY-CHANGE NUMERIC
               IF ACT-QTY-CHANGE < ZERO
                   SUBTRACT ACT-QTY-CHANGE FROM WS-REJECT-QTY
               ELSE
                   ADD ACT-QTY-CHANGE TO WS-REJECT-QTY
               END-IF
           END-IF.
           PERFORM 3400-TALLY-CODE THRU 3400-EXIT.
       3300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3400-TALLY-CODE.
      *----------------------------------------------------------------*
           SET RT-IDX TO 1.
           SEARCH WS-REJ-TALLY
               AT END
                   IF WS-REJ-TALLY-USED < 40
                       ADD 1 TO WS-REJ-TALLY-USED
                       SET RT-IDX TO WS-REJ-TALLY-USED
                       MOVE WS-LEG-REJ-CODE TO WS-RT-CODE (RT-IDX)
                       MOVE 1 TO WS-RT-COUNT (RT-IDX)
                   END-IF
               WHEN WS-RT-CODE (RT-IDX) = WS-LEG-REJ-CODE
                   ADD 1 TO WS-RT-COUNT (RT-IDX)
           END-SEARCH.
       3400-EXIT.
           EXIT.
      *================================================================*
      * LEG VALIDATION - USED BY BOTH PASSES.  THE ACTIVITY RECORD IS  *
      * CORRECTED IN PLACE (ACCOUNT TYPE, SECURITY TYPE, GL CODE, CCY) *
      * SO THAT THE VALID FILE CARRIES MASTER-FILE VALUES.             *
      *================================================================*
       4000-VALIDATE-LEG.
           MOVE SPACES TO WS-LEG-REJ-CODE WS-LEG-REJ-TEXT
                          WS-LEG-WARN-CODE.
           MOVE 'N' TO WS-SHARE-MOVE-SW WS-CASH-ONLY-SW.
      *    ---- SOURCE ----------------------------------------------
           IF NOT ACT-FROM-TRADE
           AND NOT ACT-FROM-CORP-ACTION
           AND NOT ACT-FROM-ADJUSTMENT
               MOVE 'S001' TO WS-LEG-REJ-CODE
               MOVE 'INVALID ACTIVITY SOURCE' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
      *    ---- ACTIVITY TYPE ---------------------------------------
           SET AT-IDX TO 1.
           SEARCH WS-AT-ENTRY
               AT END
                   MOVE 'S002' TO WS-LEG-REJ-CODE
                   MOVE 'INVALID ACTIVITY TYPE' TO WS-LEG-REJ-TEXT
               WHEN WS-AT-TYPE (AT-IDX) = ACT-TYPE
                   IF WS-AT-CLASS (AT-IDX) = 'S'
                       MOVE 'Y' TO WS-SHARE-MOVE-SW
                   END-IF
                   IF WS-AT-CLASS (AT-IDX) = 'C'
                       MOVE 'Y' TO WS-CASH-ONLY-SW
                   END-IF
           END-SEARCH.
           IF WS-LEG-REJ-CODE NOT = SPACES
               GO TO 4000-EXIT
           END-IF.
      *    ---- LEG NUMBER ------------------------------------------
           IF ACT-LEG-NO NOT NUMERIC
           OR (ACT-LEG-NO NOT = 1 AND ACT-LEG-NO NOT = 2)
               MOVE 'S003' TO WS-LEG-REJ-CODE
               MOVE 'INVALID LEG NUMBER' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
      *    ---- PACKED FIELDS ---------------------------------------
           IF ACT-QTY-CHANGE  NOT NUMERIC
           OR ACT-CASH-CHANGE NOT NUMERIC
           OR ACT-COST-CHANGE NOT NUMERIC
           OR ACT-PRICE       NOT NUMERIC
               MOVE 'S004' TO WS-LEG-REJ-CODE
               MOVE 'AMOUNT FIELD NOT NUMERIC' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
      *    ---- CURRENCY --------------------------------------------
           IF ACT-CCY = SPACES OR LOW-VALUES
               MOVE 'USD' TO ACT-CCY
           END-IF.
           SET CCY-IDX TO 1.
           SEARCH WS-CCY-ENTRY
               AT END
                   MOVE 'S005' TO WS-LEG-REJ-CODE
                   MOVE 'INVALID CURRENCY' TO WS-LEG-REJ-TEXT
               WHEN WS-CCY-ENTRY (CCY-IDX) = ACT-CCY
                   CONTINUE
           END-SEARCH.
           IF WS-LEG-REJ-CODE NOT = SPACES
               GO TO 4000-EXIT
           END-IF.
      *    ---- DATES -----------------------------------------------
           IF ACT-TRADE-DATE NOT NUMERIC
           OR ACT-TRADE-DATE = ZERO
               MOVE 'D010' TO WS-LEG-REJ-CODE
               MOVE 'TRADE DATE MISSING' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
           IF ACT-TRADE-DATE > DC-BUS-DATE
               MOVE 'D011' TO WS-LEG-REJ-CODE
               MOVE 'TRADE DATE IN THE FUTURE' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
           IF ACT-SETTLE-DATE NOT NUMERIC
               MOVE 'D012' TO WS-LEG-REJ-CODE
               MOVE 'SETTLE DATE NOT NUMERIC' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
           IF ACT-SETTLE-DATE = ZERO
               MOVE ACT-TRADE-DATE TO ACT-SETTLE-DATE
           END-IF.
      *    2024-02-12 NVR T+1: SETTLE = TRADE DATE NOW ALLOWED (CASH)
           IF ACT-SETTLE-DATE < ACT-TRADE-DATE
               MOVE 'D013' TO WS-LEG-REJ-CODE
               MOVE 'SETTLE DATE BEFORE TRADE DATE' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
           IF ACT-BUS-DATE NOT = DC-BUS-DATE
               MOVE 'W001' TO WS-LEG-WARN-CODE
               MOVE DC-BUS-DATE TO ACT-BUS-DATE
           END-IF.
           IF ACT-EFFECTIVE-DATE NOT NUMERIC
           OR ACT-EFFECTIVE-DATE = ZERO
               MOVE ACT-TRADE-DATE TO ACT-EFFECTIVE-DATE
           END-IF.
      *    ---- QUANTITY RULES --------------------------------------
           IF SHARE-MOVEMENT AND ACT-QTY-CHANGE = ZERO
               MOVE 'Q003' TO WS-LEG-REJ-CODE
               MOVE 'ZERO QUANTITY ON SHARE MOVEMENT' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
           IF CASH-ONLY-ACTIVITY AND ACT-QTY-CHANGE NOT = ZERO
               MOVE 'Q004' TO WS-LEG-REJ-CODE
               MOVE 'QUANTITY ON CASH-ONLY ACTIVITY' TO WS-LEG-REJ-TEXT
               GO TO 4000-EXIT
           END-IF.
           IF ACT-LEG-NO = 1
               IF (WS-AT-SIGN (AT-IDX) = '+' AND ACT-QTY-CHANGE < 0)
               OR (WS-AT-SIGN (AT-IDX) = '-' AND ACT-QTY-CHANGE > 0)
                   MOVE 'Q005' TO WS-LEG-REJ-CODE
                   MOVE 'QUANTITY SIGN WRONG FOR TYPE'
                                   TO WS-LEG-REJ-TEXT
                   GO TO 4000-EXIT
               END-IF
           END-IF.
      *    ---- GL TRANSACTION CODE ---------------------------------
           IF ACT-GL-TXN-CODE = SPACES OR LOW-VALUES
               MOVE WS-AT-GL-CODE (AT-IDX) TO ACT-GL-TXN-CODE
           END-IF.
      *    ---- ACCOUNT ---------------------------------------------
           PERFORM 4100-CHECK-ACCOUNT THRU 4100-EXIT.
           IF WS-LEG-REJ-CODE NOT = SPACES
               GO TO 4000-EXIT
           END-IF.
      *    ---- SECURITY --------------------------------------------
           PERFORM 4200-CHECK-SECURITY THRU 4200-EXIT.
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4100-CHECK-ACCOUNT.
      *----------------------------------------------------------------*
           MOVE ACT-ACCT-NO TO ACCTMAST-KEY.
           READ ACCTMAST-FILE INTO ACCT-MASTER-REC.
           EVALUATE TRUE
               WHEN ACCTMAST-FOUND
                   CONTINUE
               WHEN ACCTMAST-NOTFND
                   MOVE 'A001' TO WS-LEG-REJ-CODE
                   MOVE 'ACCOUNT NOT ON MASTER' TO WS-LEG-REJ-TEXT
                   GO TO 4100-EXIT
               WHEN OTHER
                   MOVE 'ACCTMAST' TO AB-DDNAME
                   MOVE WS-ACCTMAST-STATUS TO AB-FILE-STATUS
                   MOVE 1002 TO AB-ABEND-CODE
                   MOVE ACT-ACCT-NO TO AB-KEY
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF ACCT-CLOSED
           AND (ACT-BUY OR ACT-SHORT-SELL)
               MOVE 'A002' TO WS-LEG-REJ-CODE
               MOVE 'NEW POSITION IN CLOSED ACCOUNT' TO WS-LEG-REJ-TEXT
               GO TO 4100-EXIT
           END-IF.
           IF ACT-LEG-NO = 2 AND SHARE-MOVEMENT
           AND NOT ACCT-STREET-SIDE
               MOVE 'A003' TO WS-LEG-REJ-CODE
               MOVE 'LOCATION LEG NOT STREET ACCOUNT' TO WS-LEG-REJ-TEXT
               GO TO 4100-EXIT
           END-IF.
           IF ACT-ACCT-TYPE NOT = ACCT-TYPE
               IF ACT-ACCT-TYPE NOT = SPACES
                   MOVE 'W002' TO WS-LEG-WARN-CODE
               END-IF
               MOVE ACCT-TYPE TO ACT-ACCT-TYPE
           END-IF.
       4100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4200-CHECK-SECURITY.
      *----------------------------------------------------------------*
           PERFORM 4300-GET-SECURITY THRU 4300-EXIT.
           IF NOT SEC-WAS-FOUND
               MOVE 'P001' TO WS-LEG-REJ-CODE
               MOVE 'CUSIP NOT ON SECURITY MASTER' TO WS-LEG-REJ-TEXT
               GO TO 4200-EXIT
           END-IF.
           IF WS-SR-STATUS = 'D' AND ACT-BUY
               MOVE 'P002' TO WS-LEG-REJ-CODE
               MOVE 'BUY OF DELISTED SECURITY' TO WS-LEG-REJ-TEXT
               GO TO 4200-EXIT
           END-IF.
           IF ACT-SEC-TYPE = SPACES OR LOW-VALUES
               MOVE WS-SR-TYPE TO ACT-SEC-TYPE
           ELSE
               IF ACT-SEC-TYPE NOT = WS-SR-TYPE
                   MOVE 'W003' TO WS-LEG-WARN-CODE
                   MOVE WS-SR-TYPE TO ACT-SEC-TYPE
               END-IF
           END-IF.
       4200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * SECURITY LOOKUP WITH CACHE.  CACHE IS FILLED IN PASS 1 SO      *
      * PASS 2 NORMALLY MAKES NO DB2 CALLS.                            *
      *----------------------------------------------------------------*
       4300-GET-SECURITY.
           MOVE 'N' TO WS-SR-FOUND.
           SET SC-IDX TO 1.
           SEARCH WS-SEC-CACHE
               AT END
                   CONTINUE
               WHEN SC-IDX > WS-SEC-CACHE-USED
                   CONTINUE
               WHEN WS-SC-CUSIP (SC-IDX) = ACT-CUSIP
                   ADD 1 TO WS-SEC-CACHE-HITS
                   MOVE WS-SC-FOUND  (SC-IDX) TO WS-SR-FOUND
                   MOVE WS-SC-TYPE   (SC-IDX) TO WS-SR-TYPE
                   MOVE WS-SC-STATUS (SC-IDX) TO WS-SR-STATUS
                   MOVE WS-SC-CCY    (SC-IDX) TO WS-SR-CCY
                   GO TO 4300-EXIT
           END-SEARCH.
           MOVE 'GET '     TO SL-FUNCTION.
           MOVE ACT-CUSIP  TO SL-KEY-CUSIP.
           MOVE SPACES     TO SL-KEY-ISIN SL-KEY-SYMBOL.
           CALL 'CMD010' USING SL-SECURITY-PARMS.
           ADD 1 TO WS-CMD010-CALLS.
           EVALUATE TRUE
               WHEN SL-FOUND
                   MOVE SL-SEC-DATA TO SEC-MASTER-REC
                   MOVE 'Y'         TO WS-SR-FOUND
                   MOVE SEC-TYPE    TO WS-SR-TYPE
                   MOVE SEC-STATUS  TO WS-SR-STATUS
                   MOVE SEC-CCY     TO WS-SR-CCY
               WHEN SL-NOT-FOUND
                   MOVE 'N'         TO WS-SR-FOUND
                   MOVE SPACES      TO WS-SR-TYPE WS-SR-STATUS
                                       WS-SR-CCY
               WHEN OTHER
                   MOVE 1003         TO AB-ABEND-CODE
                   MOVE SL-SQLCODE   TO AB-SQLCODE
                   MOVE ACT-CUSIP    TO AB-KEY
                   MOVE 'CMD010 SECURITY MASTER ERROR' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
           IF WS-SEC-CACHE-USED < WS-SEC-CACHE-MAX
               ADD 1 TO WS-SEC-CACHE-USED
               SET SC-IDX TO WS-SEC-CACHE-USED
               MOVE ACT-CUSIP    TO WS-SC-CUSIP  (SC-IDX)
               MOVE WS-SR-FOUND  TO WS-SC-FOUND  (SC-IDX)
               MOVE WS-SR-TYPE   TO WS-SC-TYPE   (SC-IDX)
               MOVE WS-SR-STATUS TO WS-SC-STATUS (SC-IDX)
               MOVE WS-SR-CCY    TO WS-SC-CCY    (SC-IDX)
           END-IF.
       4300-EXIT.
           EXIT.
      *================================================================*
      * I/O ROUTINES                                                   *
      *================================================================*
       8000-OPEN-ACTVIN.
           OPEN INPUT ACTVIN-FILE.
           IF WS-ACTVIN-STATUS NOT = '00'
               MOVE 'ACTVIN' TO AB-DDNAME
               MOVE WS-ACTVIN-STATUS TO AB-FILE-STATUS
               MOVE 1001 TO AB-ABEND-CODE
               MOVE 'OPEN FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-READ-ACTVIN.
      *----------------------------------------------------------------*
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
                   MOVE 'READ FAILED' TO AB-MESSAGE
                   GO TO 9999-ABEND
           END-EVALUATE.
       8100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8200-CLOSE-ACTVIN.
      *----------------------------------------------------------------*
           CLOSE ACTVIN-FILE.
           IF WS-ACTVIN-STATUS NOT = '00'
               MOVE 'ACTVIN' TO AB-DDNAME
               MOVE WS-ACTVIN-STATUS TO AB-FILE-STATUS
               MOVE 1002 TO AB-ABEND-CODE
               MOVE 'CLOSE FAILED' TO AB-MESSAGE
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
           MOVE 'SRB100'       TO CT-STAGE.
           CALL 'CMU080' USING CT-CONTROL-PARMS.
           IF NOT CT-OK
               MOVE 1010 TO AB-ABEND-CODE
               MOVE CT-COUNTER-NAME TO AB-KEY
               MOVE 'CMU080 CONTROL TOTAL POST FAILED' TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
       8500-EXIT.
           EXIT.
      *================================================================*
       9000-TERMINATE.
      *================================================================*
           CLOSE ACCTMAST-FILE ACTVOK-FILE ACTVREJ-FILE.
           IF WS-READ-P1 NOT = WS-READ-P2
               MOVE 1004 TO AB-ABEND-CODE
               MOVE 'PASS 1 AND PASS 2 RECORD COUNTS DIFFER'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
           IF WS-VALID-CNT + WS-REJECT-CNT NOT = WS-READ-P2
               MOVE 1004 TO AB-ABEND-CODE
               MOVE 'VALID + REJECT NOT EQUAL TO RECORDS READ'
                               TO AB-MESSAGE
               GO TO 9999-ABEND
           END-IF.
      *    ---- CONTROL TOTALS FOR CMB090 - NAMES ARE FIXED ---------
           MOVE 'TC-ACTV-IN'   TO CT-COUNTER-NAME.
           MOVE WS-TC-IN-CNT   TO CT-COUNT.
           MOVE WS-TC-IN-AMT   TO CT-AMOUNT.
           MOVE WS-TC-IN-QTY   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'CA-ACTV-IN'   TO CT-COUNTER-NAME.
           MOVE WS-CA-IN-CNT   TO CT-COUNT.
           MOVE WS-CA-IN-AMT   TO CT-AMOUNT.
           MOVE WS-CA-IN-QTY   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'AJ-ACTV-IN'   TO CT-COUNTER-NAME.
           MOVE WS-AJ-IN-CNT   TO CT-COUNT.
           MOVE WS-AJ-IN-AMT   TO CT-AMOUNT.
           MOVE WS-AJ-IN-QTY   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'VALID-OUT'    TO CT-COUNTER-NAME.
           MOVE WS-VALID-CNT   TO CT-COUNT.
           MOVE WS-VALID-AMT   TO CT-AMOUNT.
           MOVE WS-VALID-QTY   TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
           MOVE 'REJECT-OUT'   TO CT-COUNTER-NAME.
           MOVE WS-REJECT-CNT  TO CT-COUNT.
           MOVE WS-REJECT-AMT  TO CT-AMOUNT.
           MOVE WS-REJECT-QTY  TO CT-QTY-HASH.
           PERFORM 8500-POST-TOTAL THRU 8500-EXIT.
      *    ---- STATISTICS ------------------------------------------
           DISPLAY '************************************************'.
           DISPLAY '* SRB100 - STOCK RECORD ACTIVITY VALIDATION    *'.
           DISPLAY '************************************************'.
           DISPLAY ' BUSINESS DATE            : ' DC-BUS-DATE.
           MOVE WS-READ-P1 TO WS-DISP-CNT.
           DISPLAY ' RECORDS READ             : ' WS-DISP-CNT.
           MOVE WS-TC-IN-CNT TO WS-DISP-CNT.
           DISPLAY '   FROM TRADE CAPTURE     : ' WS-DISP-CNT.
           MOVE WS-CA-IN-CNT TO WS-DISP-CNT.
           DISPLAY '   FROM CORPORATE ACTIONS : ' WS-DISP-CNT.
           MOVE WS-AJ-IN-CNT TO WS-DISP-CNT.
           DISPLAY '   ADJUSTMENTS / OTHER    : ' WS-DISP-CNT.
           MOVE WS-REF-CNT TO WS-DISP-CNT.
           DISPLAY ' REFERENCES               : ' WS-DISP-CNT.
           MOVE WS-REF-UNBAL-CNT TO WS-DISP-CNT.
           DISPLAY ' REFERENCES OUT OF BALANCE: ' WS-DISP-CNT.
           MOVE WS-VALID-CNT TO WS-DISP-CNT.
           DISPLAY ' VALID LEGS WRITTEN       : ' WS-DISP-CNT.
           MOVE WS-REJECT-CNT TO WS-DISP-CNT.
           DISPLAY ' REJECTED LEGS WRITTEN    : ' WS-DISP-CNT.
           MOVE WS-WARNING-CNT TO WS-DISP-CNT.
           DISPLAY ' LEGS WITH WARNINGS       : ' WS-DISP-CNT.
           MOVE WS-VALID-AMT TO WS-DISP-AMT.
           DISPLAY ' VALID CASH HASH          : ' WS-DISP-AMT.
           MOVE WS-CMD010-CALLS TO WS-DISP-CNT.
           DISPLAY ' CMD010 CALLS             : ' WS-DISP-CNT.
           MOVE WS-HASH-MAX-PROBES TO WS-DISP-CNT.
           DISPLAY ' MAX HASH PROBES          : ' WS-DISP-CNT.
           IF WS-REJ-TALLY-USED > ZERO
               DISPLAY ' REJECT / WARNING CODES   :'
               PERFORM VARYING RT-IDX FROM 1 BY 1
                       UNTIL RT-IDX > WS-REJ-TALLY-USED
                   MOVE WS-RT-COUNT (RT-IDX) TO WS-DISP-CNT
                   DISPLAY '   ' WS-RT-CODE (RT-IDX) '   '
                           WS-DISP-CNT
               END-PERFORM
           END-IF.
           DISPLAY '************************************************'.
           MOVE 'WRIT'         TO AU-FUNCTION.
           MOVE 'END'          TO AU-EVENT.
           MOVE 'I'            TO AU-SEVERITY.
           MOVE SPACES         TO AU-KEY.
           MOVE 'STOCK RECORD ACTIVITY VALIDATION ENDED'
                               TO AU-MESSAGE.
           IF WS-REJECT-CNT > ZERO OR WS-WARNING-CNT > ZERO
               MOVE 'W'        TO AU-SEVERITY
               MOVE 4          TO RETURN-CODE
           ELSE
               MOVE 0          TO RETURN-CODE
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
           MOVE '9999-ABEND'  TO AB-PARAGRAPH.
           DISPLAY 'SRB100 ABENDING - CODE ' AB-ABEND-CODE
                   ' DD ' AB-DDNAME ' STATUS ' AB-FILE-STATUS.
           DISPLAY 'SRB100 ' AB-MESSAGE.
           CALL 'CMU050' USING AB-ABEND-PARMS.
           MOVE 16 TO RETURN-CODE.
           GOBACK.
