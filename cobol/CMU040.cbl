      *================================================================*
      * PROGRAM    : CMU040                                            *
      * TITLE      : FOREIGN EXCHANGE CONVERSION                       *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   CONVERTS FX-AMOUNT-IN FROM FX-FROM-CCY TO FX-TO-CCY USING    *
      *   THE LATEST MSEC.FX_RATE ROW ON OR BEFORE FX-RATE-DATE        *
      *   (CMD030 'GETL').  RATES ARE USD PER ONE UNIT OF CURRENCY;    *
      *   CROSS RATES GO THROUGH USD:                                  *
      *       FX-RATE       = RATE(FROM) / RATE(TO)                    *
      *       FX-AMOUNT-OUT = FX-AMOUNT-IN * FX-RATE   (ROUNDED)       *
      *   USD IS ALWAYS 1.00000000 AND IS NOT LOOKED UP.               *
      *   A 50-ENTRY CACHE (CCY + REQUESTED DATE) AVOIDS REPEATED DB2  *
      *   CALLS - MOST RUNS SEE ONLY A HANDFUL OF CURRENCIES.  WHEN    *
      *   THE CACHE IS FULL THE OLDEST SLOT IS REUSED.                 *
      *   IF THE RATE FOUND IS DATED BEFORE FX-RATE-DATE THE RESULT IS *
      *   STILL RETURNED WITH FX-RETURN-CODE 02 (STALE RATE).          *
      *                                                                *
      * LINKAGE    : CALL 'CMU040' USING FX-CONVERT-PARMS  (CMFXLNK)   *
      * CALLS      : CMD030 (DB2 I/O - MSEC.FX_RATE)                   *
      * RETURN     : FX-RETURN-CODE 00 OK, 02 STALE RATE,              *
      *              04 RATE NOT FOUND, 12 DB2 ERROR                   *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 1997-03-17 KAP  CHG03390  ORIGINAL - RATES FROM VSAM FXRATES   *
      * 1998-11-02 TLM  CHG04471  Y2K - CCYYMMDD                       *
      * 1999-01-04 KAP  CHG04602  EURO INTRODUCED - DEM FRF ITL ETC    *
      *                           NO LONGER LOADED BY TREASURY         *
      * 2001-04-09 DWB  CHG08130  DECIMALIZATION - AMOUNT OUT ROUNDED  *
      * 2004-10-25 KAP  CHG12690  RATES FROM DB2 VIA CMD030; CACHE     *
      *                           RAISED 20 -> 50                      *
      * 2015-09-08 SPA  CHG28815  STALE RATE WARNING (RC 02)           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMU040.
       AUTHOR.        K A PATEL.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/17/97.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMU040 WORKING STORAGE BEGINS'.
      *
       01  WS-CACHE-CONTROL.
           05  WS-CACHE-MAX            PIC S9(04) COMP VALUE +50.
           05  WS-CACHE-USED           PIC S9(04) COMP VALUE ZERO.
           05  WS-CACHE-NEXT           PIC S9(04) COMP VALUE +1.
           05  WS-CACHE-SUB            PIC S9(04) COMP VALUE ZERO.
           05  WS-CACHE-HITS           PIC S9(09) COMP VALUE ZERO.
           05  WS-CACHE-MISSES         PIC S9(09) COMP VALUE ZERO.
       01  WS-RATE-CACHE.
           05  WS-CACHE-ENTRY          OCCURS 50 TIMES.
               10  WS-CE-CCY           PIC X(03).
               10  WS-CE-REQ-DATE      PIC 9(08).
               10  WS-CE-USD-RATE      PIC S9(05)V9(08) COMP-3.
               10  WS-CE-ACTUAL-DATE   PIC 9(08).
               10  WS-CE-RC            PIC 9(02).
      *
       01  WS-LOOKUP.
           05  WS-LK-CCY               PIC X(03).
           05  WS-LK-USD-RATE          PIC S9(05)V9(08) COMP-3.
           05  WS-LK-ACTUAL-DATE       PIC 9(08).
           05  WS-LK-RC                PIC 9(02).
               88  WS-LK-FOUND                    VALUE 00.
               88  WS-LK-NOT-FOUND                VALUE 04.
               88  WS-LK-DB-ERROR                 VALUE 12.
      *
       01  WS-FROM-RATE                PIC S9(05)V9(08) COMP-3.
       01  WS-TO-RATE                  PIC S9(05)V9(08) COMP-3.
       01  WS-FROM-DATE                PIC 9(08).
       01  WS-TO-DATE                  PIC 9(08).
       01  WS-FOUND-SW                 PIC X(01).
           88  WS-IN-CACHE                        VALUE 'Y'.
      *
           COPY CMFXDLNK.
      *
       LINKAGE SECTION.
           COPY CMFXLNK.
      *
       PROCEDURE DIVISION USING FX-CONVERT-PARMS.
       0000-MAINLINE.
           MOVE ZERO   TO FX-RETURN-CODE
           MOVE SPACES TO FX-MESSAGE
           MOVE ZERO   TO FX-AMOUNT-OUT FX-RATE FX-RATE-ACTUAL-DATE
      *
      *    SAME CURRENCY - NO CONVERSION
           IF FX-FROM-CCY = FX-TO-CCY
               MOVE 1              TO FX-RATE
               MOVE FX-AMOUNT-IN   TO FX-AMOUNT-OUT
               MOVE FX-RATE-DATE   TO FX-RATE-ACTUAL-DATE
               GOBACK
           END-IF
      *
           MOVE FX-FROM-CCY TO WS-LK-CCY
           PERFORM 1000-GET-USD-RATE THRU 1000-EXIT
           IF NOT WS-LK-FOUND
               PERFORM 8000-LOOKUP-FAILED THRU 8000-EXIT
               GOBACK
           END-IF
           MOVE WS-LK-USD-RATE    TO WS-FROM-RATE
           MOVE WS-LK-ACTUAL-DATE TO WS-FROM-DATE
      *
           MOVE FX-TO-CCY TO WS-LK-CCY
           PERFORM 1000-GET-USD-RATE THRU 1000-EXIT
           IF NOT WS-LK-FOUND
               PERFORM 8000-LOOKUP-FAILED THRU 8000-EXIT
               GOBACK
           END-IF
           MOVE WS-LK-USD-RATE    TO WS-TO-RATE
           MOVE WS-LK-ACTUAL-DATE TO WS-TO-DATE
      *
           IF WS-TO-RATE = ZERO
               MOVE 04 TO FX-RETURN-CODE
               STRING 'ZERO RATE FOR ' FX-TO-CCY
                   DELIMITED BY SIZE INTO FX-MESSAGE
               GOBACK
           END-IF
      *
           PERFORM 2000-CONVERT THRU 2000-EXIT
           GOBACK.
      *
      *----------------------------------------------------------------*
      * 1000 - USD RATE FOR WS-LK-CCY ON FX-RATE-DATE (CACHE, THEN DB2)*
      *----------------------------------------------------------------*
       1000-GET-USD-RATE.
           IF WS-LK-CCY = 'USD'
               MOVE 1            TO WS-LK-USD-RATE
               MOVE FX-RATE-DATE TO WS-LK-ACTUAL-DATE
               MOVE 00           TO WS-LK-RC
               GO TO 1000-EXIT.
      *
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING WS-CACHE-SUB FROM 1 BY 1
                   UNTIL WS-CACHE-SUB > WS-CACHE-USED
                      OR WS-IN-CACHE
               IF WS-CE-CCY (WS-CACHE-SUB) = WS-LK-CCY
                  AND WS-CE-REQ-DATE (WS-CACHE-SUB) = FX-RATE-DATE
                   MOVE 'Y' TO WS-FOUND-SW
                   MOVE WS-CE-USD-RATE (WS-CACHE-SUB)
                     TO WS-LK-USD-RATE
                   MOVE WS-CE-ACTUAL-DATE (WS-CACHE-SUB)
                     TO WS-LK-ACTUAL-DATE
                   MOVE WS-CE-RC (WS-CACHE-SUB) TO WS-LK-RC
               END-IF
           END-PERFORM
           IF WS-IN-CACHE
               ADD 1 TO WS-CACHE-HITS
               GO TO 1000-EXIT.
      *
           ADD 1 TO WS-CACHE-MISSES
           MOVE 'GETL'        TO FD-FUNCTION
           MOVE WS-LK-CCY     TO FD-CCY
           MOVE FX-RATE-DATE  TO FD-RATE-DATE
           MOVE ZERO          TO FD-USD-RATE FD-ACTUAL-DATE FD-SQLCODE
           CALL 'CMD030' USING FD-FXRATE-PARMS
           EVALUATE TRUE
               WHEN FD-FOUND
                   MOVE FD-USD-RATE    TO WS-LK-USD-RATE
                   MOVE FD-ACTUAL-DATE TO WS-LK-ACTUAL-DATE
                   MOVE 00             TO WS-LK-RC
               WHEN FD-NOT-FOUND
                   MOVE ZERO           TO WS-LK-USD-RATE
                                          WS-LK-ACTUAL-DATE
                   MOVE 04             TO WS-LK-RC
               WHEN OTHER
                   DISPLAY 'CMU040 - CMD030 ERROR, SQLCODE '
                           FD-SQLCODE ' CCY ' WS-LK-CCY
                   MOVE 12             TO WS-LK-RC
                   GO TO 1000-EXIT
           END-EVALUATE
      *    DB2 ERRORS ARE NOT CACHED - FOUND / NOT FOUND ARE
           PERFORM 1100-ADD-TO-CACHE THRU 1100-EXIT.
       1000-EXIT.
           EXIT.
      *
       1100-ADD-TO-CACHE.
           IF WS-CACHE-USED < WS-CACHE-MAX
               ADD 1 TO WS-CACHE-USED
               MOVE WS-CACHE-USED TO WS-CACHE-SUB
           ELSE
               MOVE WS-CACHE-NEXT TO WS-CACHE-SUB
               ADD 1 TO WS-CACHE-NEXT
               IF WS-CACHE-NEXT > WS-CACHE-MAX
                   MOVE 1 TO WS-CACHE-NEXT
               END-IF
           END-IF
           MOVE WS-LK-CCY         TO WS-CE-CCY (WS-CACHE-SUB)
           MOVE FX-RATE-DATE      TO WS-CE-REQ-DATE (WS-CACHE-SUB)
           MOVE WS-LK-USD-RATE    TO WS-CE-USD-RATE (WS-CACHE-SUB)
           MOVE WS-LK-ACTUAL-DATE TO WS-CE-ACTUAL-DATE (WS-CACHE-SUB)
           MOVE WS-LK-RC          TO WS-CE-RC (WS-CACHE-SUB).
       1100-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
      * 2000 - CROSS RATE AND CONVERSION                               *
      *   CROSS RATE CARRIED TO 8 PLACES AS QUOTED BY TREASURY, THEN   *
      *   AMOUNT ROUNDED TO THE CENT (CHG08130).                       *
      *----------------------------------------------------------------*
       2000-CONVERT.
           COMPUTE FX-RATE = WS-FROM-RATE / WS-TO-RATE
           COMPUTE FX-AMOUNT-OUT ROUNDED = FX-AMOUNT-IN * FX-RATE
      *    ACTUAL DATE REPORTED IS THE OLDER OF THE TWO RATES USED
           IF WS-FROM-DATE < WS-TO-DATE
               MOVE WS-FROM-DATE TO FX-RATE-ACTUAL-DATE
           ELSE
               MOVE WS-TO-DATE   TO FX-RATE-ACTUAL-DATE
           END-IF
           IF FX-RATE-ACTUAL-DATE < FX-RATE-DATE
               MOVE 02 TO FX-RETURN-CODE
               STRING 'STALE RATE - DATED ' FX-RATE-ACTUAL-DATE
                   DELIMITED BY SIZE INTO FX-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.
      *
      *----------------------------------------------------------------*
       8000-LOOKUP-FAILED.
           MOVE WS-LK-RC TO FX-RETURN-CODE
           IF WS-LK-DB-ERROR
               STRING 'DB2 ERROR ON FX_RATE FOR ' WS-LK-CCY
                   DELIMITED BY SIZE INTO FX-MESSAGE
           ELSE
               MOVE 04 TO FX-RETURN-CODE
               STRING 'NO FX RATE FOR ' WS-LK-CCY ' ON/BEFORE '
                      FX-RATE-DATE
                   DELIMITED BY SIZE INTO FX-MESSAGE
           END-IF.
       8000-EXIT.
           EXIT.
