      *================================================================*
      * PROGRAM    : CMD050                                            *
      * TITLE      : DB2 I/O MODULE - MSEC.GL_ACCOUNT_MAP              *
      * SYSTEM     : MERIDIAN EOD - COMMON SERVICES (CM)               *
      *----------------------------------------------------------------*
      * DESCRIPTION:                                                   *
      *   RETURNS THE DEBIT / CREDIT GENERAL LEDGER ACCOUNTS AND COST  *
      *   CENTER FOR A TRANSACTION CODE, ACCOUNT TYPE AND SECURITY     *
      *   TYPE.  THE MAP HOLDS '**' WILDCARD ROWS; THE MOST SPECIFIC   *
      *   ROW WINS:                                                    *
      *     1  TXN + ACCT-TYPE + SEC-TYPE                              *
      *     2  TXN + ACCT-TYPE + '**'                                  *
      *     3  TXN + '**'      + SEC-TYPE                              *
      *     4  TXN + '**'      + '**'                                  *
      *   THE MAP IS SMALL AND STATIC DURING THE BATCH WINDOW, SO THE  *
      *   LAST 200 DISTINCT LOOKUPS ARE KEPT IN WORKING STORAGE        *
      *   (SRB600 CALLS ONCE PER JOURNAL LEG).                         *
      *                                                                *
      * LINKAGE    : CALL 'CMD050' USING GM-GLMAP-PARMS  (CMGMLNK)     *
      * TABLES     : MSEC.GL_ACCOUNT_MAP  (SELECT)                     *
      * PLAN/PKG   : COLLECTION MSCMCOL                                *
      * RETURN     : GM-RETURN-CODE 00 FOUND, 04 NOT FOUND,            *
      *              12 SQL ERROR (GM-SQLCODE)                         *
      *----------------------------------------------------------------*
      * CHANGE HISTORY                                                 *
      * DATE       BY   TICKET    DESCRIPTION                          *
      * ---------- ---  --------  ------------------------------------ *
      * 2008-03-10 SPA  CHG17611  ORIGINAL - GL MAP MOVED FROM THE     *
      *                           HARD-CODED TABLE IN SRB600 TO DB2    *
      * 2008-06-23 SPA  CHG17930  WILDCARD ROWS, ORDER BY CASE         *
      * 2012-04-16 JMF  CHG23206  LOOKUP CACHE (200 ENTRIES)           *
      * 2016-10-03 SPA  CHG30112  NOT-FOUND RESULTS ARE CACHED TOO     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMD050.
       AUTHOR.        S P ALVAREZ.
       INSTALLATION.  MERIDIAN SECURITIES - NYC DATA CENTER.
       DATE-WRITTEN.  03/10/08.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z15.
       OBJECT-COMPUTER. IBM-Z15.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(32)
                                 VALUE 'CMD050 WORKING STORAGE BEGINS'.
      *
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
           EXEC SQL INCLUDE DCLGLMAP END-EXEC.
      *
       01  WS-CACHE-CONTROL.
           05  WS-CACHE-MAX            PIC S9(04) COMP VALUE +200.
           05  WS-CACHE-USED           PIC S9(04) COMP VALUE ZERO.
           05  WS-CACHE-NEXT           PIC S9(04) COMP VALUE +1.
           05  WS-CACHE-HITS           PIC S9(09) COMP VALUE ZERO.
           05  WS-DB2-CALLS            PIC S9(09) COMP VALUE ZERO.
       01  WS-CACHE-TABLE.
           05  WS-CE-ENTRY             OCCURS 200 TIMES
                                       INDEXED BY CE-IX.
               10  WS-CE-KEY.
                   15  WS-CE-TXN-CODE  PIC X(04).
                   15  WS-CE-ACCT-TYPE PIC X(02).
                   15  WS-CE-SEC-TYPE  PIC X(02).
               10  WS-CE-RC            PIC 9(02).
               10  WS-CE-DR-GL         PIC X(10).
               10  WS-CE-CR-GL         PIC X(10).
               10  WS-CE-COST-CENTER   PIC X(06).
               10  WS-CE-DESC          PIC X(30).
      *
       01  WS-LOOKUP-KEY.
           05  WS-LK-TXN-CODE          PIC X(04).
           05  WS-LK-ACCT-TYPE         PIC X(02).
           05  WS-LK-SEC-TYPE          PIC X(02).
       01  WS-FOUND-SW                 PIC X(01).
           88  WS-IN-CACHE                        VALUE 'Y'.
      *
       01  WS-ERROR-MESSAGE.
           05  WS-ERROR-LEN            PIC S9(04) COMP VALUE +720.
           05  WS-ERROR-TEXT           PIC X(72) OCCURS 10 TIMES
                                       INDEXED BY ERR-IX.
       01  WS-ERROR-TEXT-LEN           PIC S9(09) COMP VALUE +72.
      *
       LINKAGE SECTION.
           COPY CMGMLNK.
      *
       PROCEDURE DIVISION USING GM-GLMAP-PARMS.
      *
       0000-MAINLINE.
           MOVE ZERO   TO GM-RETURN-CODE GM-SQLCODE
           MOVE SPACES TO GM-DR-GL-ACCOUNT GM-CR-GL-ACCOUNT
                          GM-COST-CENTER GM-DESC
           MOVE GM-TXN-CODE  TO WS-LK-TXN-CODE
           MOVE GM-ACCT-TYPE TO WS-LK-ACCT-TYPE
           MOVE GM-SEC-TYPE  TO WS-LK-SEC-TYPE
           PERFORM 1000-SEARCH-CACHE
           IF WS-IN-CACHE
               ADD 1 TO WS-CACHE-HITS
               PERFORM 1100-RETURN-FROM-CACHE
           ELSE
               ADD 1 TO WS-DB2-CALLS
               PERFORM 2000-SELECT-MAP
               IF NOT GM-DB-ERROR
                   PERFORM 3000-ADD-TO-CACHE
               END-IF
           END-IF
           GOBACK.
      *
       1000-SEARCH-CACHE.
           MOVE 'N' TO WS-FOUND-SW
           PERFORM VARYING CE-IX FROM 1 BY 1
                   UNTIL CE-IX > WS-CACHE-USED OR WS-IN-CACHE
               IF WS-CE-KEY (CE-IX) = WS-LOOKUP-KEY
                   MOVE 'Y' TO WS-FOUND-SW
               END-IF
           END-PERFORM
           IF WS-IN-CACHE
               SET CE-IX DOWN BY 1
           END-IF.
      *
       1100-RETURN-FROM-CACHE.
           MOVE WS-CE-RC (CE-IX)          TO GM-RETURN-CODE
           IF GM-FOUND
               MOVE WS-CE-DR-GL (CE-IX)       TO GM-DR-GL-ACCOUNT
               MOVE WS-CE-CR-GL (CE-IX)       TO GM-CR-GL-ACCOUNT
               MOVE WS-CE-COST-CENTER (CE-IX) TO GM-COST-CENTER
               MOVE WS-CE-DESC (CE-IX)        TO GM-DESC
           ELSE
               MOVE +100                      TO GM-SQLCODE
           END-IF.
      *
      *----------------------------------------------------------------*
      * 2000 - MOST SPECIFIC ROW WINS (CHG17930)                       *
      *----------------------------------------------------------------*
       2000-SELECT-MAP.
           MOVE WS-LK-TXN-CODE  TO GA-TXN-CODE
           MOVE WS-LK-ACCT-TYPE TO GA-ACCT-TYPE
           MOVE WS-LK-SEC-TYPE  TO GA-SEC-TYPE
           EXEC SQL
               SELECT DR_GL, CR_GL, COST_CENTER, GL_DESC
                 INTO :GA-DR-GL, :GA-CR-GL, :GA-COST-CENTER,
                      :GA-GL-DESC
                 FROM MSEC.GL_ACCOUNT_MAP
                WHERE TXN_CODE  = :GA-TXN-CODE
                  AND ACCT_TYPE IN (:GA-ACCT-TYPE, '**')
                  AND SEC_TYPE  IN (:GA-SEC-TYPE, '**')
                ORDER BY
                      CASE WHEN ACCT_TYPE = :GA-ACCT-TYPE
                            AND SEC_TYPE  = :GA-SEC-TYPE  THEN 1
                           WHEN ACCT_TYPE = :GA-ACCT-TYPE THEN 2
                           WHEN SEC_TYPE  = :GA-SEC-TYPE  THEN 3
                           ELSE 4
                      END
                FETCH FIRST 1 ROW ONLY
                WITH UR
           END-EXEC
           MOVE SQLCODE TO GM-SQLCODE
           EVALUATE TRUE
               WHEN SQLCODE = ZERO
                   MOVE 00             TO GM-RETURN-CODE
                   MOVE GA-DR-GL       TO GM-DR-GL-ACCOUNT
                   MOVE GA-CR-GL       TO GM-CR-GL-ACCOUNT
                   MOVE GA-COST-CENTER TO GM-COST-CENTER
                   MOVE GA-GL-DESC     TO GM-DESC
               WHEN SQLCODE = +100
                   MOVE 04             TO GM-RETURN-CODE
               WHEN OTHER
                   PERFORM 9000-SQL-ERROR
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
      * 3000 - REMEMBER THE RESULT (ROUND ROBIN WHEN FULL)             *
      *----------------------------------------------------------------*
       3000-ADD-TO-CACHE.
           IF WS-CACHE-USED < WS-CACHE-MAX
               ADD 1 TO WS-CACHE-USED
               SET CE-IX TO WS-CACHE-USED
           ELSE
               SET CE-IX TO WS-CACHE-NEXT
               ADD 1 TO WS-CACHE-NEXT
               IF WS-CACHE-NEXT > WS-CACHE-MAX
                   MOVE 1 TO WS-CACHE-NEXT
               END-IF
           END-IF
           MOVE WS-LOOKUP-KEY      TO WS-CE-KEY (CE-IX)
           MOVE GM-RETURN-CODE     TO WS-CE-RC (CE-IX)
           MOVE GM-DR-GL-ACCOUNT   TO WS-CE-DR-GL (CE-IX)
           MOVE GM-CR-GL-ACCOUNT   TO WS-CE-CR-GL (CE-IX)
           MOVE GM-COST-CENTER     TO WS-CE-COST-CENTER (CE-IX)
           MOVE GM-DESC            TO WS-CE-DESC (CE-IX).
      *
       9000-SQL-ERROR.
           MOVE SQLCODE TO GM-SQLCODE
           MOVE 12      TO GM-RETURN-CODE
           DISPLAY 'CMD050 - SQL ERROR ' GM-TXN-CODE '/'
                   GM-ACCT-TYPE '/' GM-SEC-TYPE ' SQLCODE ' SQLCODE
           CALL 'DSNTIAR' USING SQLCA WS-ERROR-MESSAGE
                                WS-ERROR-TEXT-LEN
           PERFORM VARYING ERR-IX FROM 1 BY 1 UNTIL ERR-IX > 10
               IF WS-ERROR-TEXT (ERR-IX) NOT = SPACES
                   DISPLAY 'CMD050 - ' WS-ERROR-TEXT (ERR-IX)
               END-IF
           END-PERFORM.
