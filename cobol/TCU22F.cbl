       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TCU22F.
       AUTHOR.        S P ACHARYA.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  11/14/2011.
       DATE-COMPILED.
      *================================================================*
      * PROGRAM    : TCU22F                                            *
      * TITLE      : REGULATORY FEES - FIXED INCOME                    *
      * TYPE       : CALLABLE UTILITY (NO FILES)                       *
      *                                                                *
      * CALLED BY  : TCB200 (DYNAMICALLY, NAME BUILT AT RUN TIME)      *
      * LINKAGE    : TCFEELNK  (FE-FEE-PARMS)                          *
      *                                                                *
      * DESCRIPTION:                                                   *
      *   TRANSACTION ASSESSMENTS ON BOND TRADES, CHARGED PER $1,000   *
      *   OF FACE AMOUNT AND RETURNED IN FE-OTHER-FEES.                *
      *     MU  MUNICIPAL  - DEALER ASSESSMENT BOTH SIDES              *
      *                      $0.0100 PER $1,000 FACE                   *
      *     CB  CORPORATE  - REPORTING FEE ON SALES ONLY               *
      *                      $0.0200 PER $1,000 FACE                   *
      *                      MINIMUM $0.30  MAXIMUM $5.00              *
      *     GV  GOVERNMENT - NONE                                      *
      *   SEC FEE AND TAF DO NOT APPLY TO BONDS (ALWAYS ZERO).         *
      *   RESULT ROUNDED TO THE NEAREST CENT.                          *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 2011-11-14 SPA  CHG22418  ORIGINAL - SPLIT FROM TCU220         *
      * 2014-07-21 SPA  CHG27002  CORPORATE MIN/MAX                    *
      * 2017-01-09 NVR  CHG32118  MUNI RATE .0050 TO .0100             *
      * 2024-02-12 NVR  CHG41007  T+1 REVIEW - NO CHANGE REQUIRED      *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-Z15.
       OBJECT-COMPUTER.  IBM-Z15.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-ID               PIC X(08)  VALUE 'TCU22F'.
       01  WS-CALL-COUNT               PIC S9(09) COMP VALUE ZERO.
      *
       01  WS-FI-FEE-CONSTANTS.
           05  WS-MUNI-RATE-PER-M      PIC S9(03)V9(04) COMP-3
                                                  VALUE +0.0100.
      *    05  WS-MUNI-RATE-PER-M      PIC S9(03)V9(04) COMP-3
      *                                           VALUE +0.0050.
           05  WS-CORP-RATE-PER-M      PIC S9(03)V9(04) COMP-3
                                                  VALUE +0.0200.
           05  WS-CORP-MINIMUM         PIC S9(03)V99    COMP-3
                                                  VALUE +0.30.
           05  WS-CORP-MAXIMUM         PIC S9(03)V99    COMP-3
                                                  VALUE +5.00.
           05  WS-FACE-UNIT            PIC S9(05)       COMP-3
                                                  VALUE +1000.
      *
       01  WS-WORK-FIELDS.
           05  WS-FACE-WORK            PIC S9(13)V99    COMP-3.
           05  WS-FEE-WORK             PIC S9(09)V99    COMP-3.
           05  WS-RATE-APPLIED         PIC S9(03)V9(04) COMP-3.
      *
       01  WS-SWITCHES.
           05  WS-SIDE-SW              PIC X(01).
               88  WS-SALE                       VALUE 'S'.
               88  WS-PURCHASE                   VALUE 'B'.
      *
       LINKAGE SECTION.
       COPY TCFEELNK.
      *
       PROCEDURE DIVISION USING FE-FEE-PARMS.
      *
       0000-MAINLINE.
           ADD 1                       TO WS-CALL-COUNT
           PERFORM 1000-INITIALIZE
           IF FE-INVALID-INPUT
               GOBACK
           END-IF
           EVALUATE FE-SEC-TYPE
               WHEN 'MU'
                   PERFORM 2000-MUNI-ASSESSMENT
               WHEN 'CB'
                   PERFORM 3000-CORP-REPORTING-FEE
               WHEN 'GV'
                   MOVE 'GOVERNMENT - NO TRANSACTION FEE'
                                       TO FE-MESSAGE
               WHEN OTHER
                   MOVE 04             TO FE-RETURN-CODE
                   MOVE 'NOT A FIXED INCOME SECURITY - NO FEES'
                                       TO FE-MESSAGE
           END-EVALUATE
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - EDIT INPUT                                              *
      *   FACE AMOUNT DEFAULTS TO QUANTITY WHEN NOT SUPPLIED (THE FI   *
      *   FEED CARRIES FACE IN THE QUANTITY FIELD).                    *
      *----------------------------------------------------------------*
       1000-INITIALIZE.
           MOVE ZERO                   TO FE-SEC-FEE
                                          FE-TAF-FEE
                                          FE-OTHER-FEES
                                          FE-SEC-RATE
                                          FE-RETURN-CODE
           MOVE SPACES                 TO FE-MESSAGE
           IF FE-FACE-AMOUNT NOT NUMERIC
               MOVE ZERO               TO FE-FACE-AMOUNT
           END-IF
           IF FE-QTY NOT NUMERIC
               MOVE 08                 TO FE-RETURN-CODE
               MOVE 'NON-NUMERIC QUANTITY TO TCU22F'
                                       TO FE-MESSAGE
               EXIT PARAGRAPH
           END-IF
           IF FE-FACE-AMOUNT > ZERO
               MOVE FE-FACE-AMOUNT     TO WS-FACE-WORK
           ELSE
               MOVE FE-QTY             TO WS-FACE-WORK
           END-IF
           IF WS-FACE-WORK NOT > ZERO
               MOVE 08                 TO FE-RETURN-CODE
               MOVE 'FACE AMOUNT NOT POSITIVE'
                                       TO FE-MESSAGE
               EXIT PARAGRAPH
           END-IF
           EVALUATE FE-SIDE
               WHEN 'S '
               WHEN 'SS'
                   SET WS-SALE         TO TRUE
               WHEN OTHER
                   SET WS-PURCHASE     TO TRUE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
      * 2000 - MUNICIPAL DEALER ASSESSMENT (BOTH SIDES)                *
      *----------------------------------------------------------------*
       2000-MUNI-ASSESSMENT.
           MOVE WS-MUNI-RATE-PER-M     TO WS-RATE-APPLIED
           COMPUTE WS-FEE-WORK ROUNDED =
                   WS-FACE-WORK / WS-FACE-UNIT * WS-RATE-APPLIED
           MOVE WS-RATE-APPLIED        TO FE-SEC-RATE
           MOVE WS-FEE-WORK            TO FE-OTHER-FEES
           MOVE 'MUNI DEALER ASSESSMENT'
                                       TO FE-MESSAGE.
      *
      *----------------------------------------------------------------*
      * 3000 - CORPORATE BOND REPORTING FEE (SALES ONLY)               *
      *----------------------------------------------------------------*
       3000-CORP-REPORTING-FEE.
           IF WS-PURCHASE
               MOVE 'CORPORATE PURCHASE - NO FEE'
                                       TO FE-MESSAGE
               EXIT PARAGRAPH
           END-IF
           MOVE WS-CORP-RATE-PER-M     TO WS-RATE-APPLIED
           COMPUTE WS-FEE-WORK ROUNDED =
                   WS-FACE-WORK / WS-FACE-UNIT * WS-RATE-APPLIED
           EVALUATE TRUE
               WHEN WS-FEE-WORK < WS-CORP-MINIMUM
                   MOVE WS-CORP-MINIMUM TO WS-FEE-WORK
               WHEN WS-FEE-WORK > WS-CORP-MAXIMUM
                   MOVE WS-CORP-MAXIMUM TO WS-FEE-WORK
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           MOVE WS-RATE-APPLIED        TO FE-SEC-RATE
           MOVE WS-FEE-WORK            TO FE-OTHER-FEES
           MOVE 'CORPORATE REPORTING FEE'
                                       TO FE-MESSAGE.
