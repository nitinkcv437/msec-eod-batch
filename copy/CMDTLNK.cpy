      *================================================================*
      * COPYBOOK   : CMDTLNK                                           *
      * DESCRIPTION: LINKAGE FOR DATE SERVICES MODULE CMU010.          *
      *             CALL 'CMU010' USING DT-DATE-PARMS.                 *
      * FUNCTIONS:                                                     *
      *   VALD VALIDATE DT-DATE-1 (CCYYMMDD)                           *
      *   ADDC DT-DATE-1 + DT-DAYS CALENDAR DAYS -> DT-RESULT-DATE     *
      *   ADDB DT-DATE-1 + DT-DAYS BUSINESS DAYS -> DT-RESULT-DATE*
      *   NXTB NEXT BUSINESS DAY AFTER DT-DATE-1  -> RESULT            *
      *   PRVB PREVIOUS BUSINESS DAY BEFORE DT-DATE-1 -> RESULT        *
      *   BUSD IS DT-DATE-1 A BUSINESS DAY -> DT-RESULT-FLAG Y/N       *
      *   DIFC CALENDAR DAYS DT-DATE-2 MINUS DT-DATE-1 -> DT-RESULT-NUM*
      *   DIFB BUSINESS DAYS DT-DATE-2 MINUS DT-DATE-1 -> DT-RESULT-NUM*
      *   D360 30/360 DAY COUNT DATE-1 TO DATE-2 -> DT-RESULT-NUM      *
      *   DOW  DAY OF WEEK OF DT-DATE-1 (1=MON..7=SUN) -> RESULT-NUM   *
      *   EOM  LAST CALENDAR DAY OF MONTH OF DT-DATE-1 -> RESULT-DATE  *
      *   LBDM LAST BUSINESS DAY OF MONTH OF DT-DATE-1 -> RESULT-DATE  *
      *   JUL  CCYYDDD JULIAN OF DT-DATE-1 -> DT-RESULT-NUM            *
      *   W2Y4 WINDOW DT-DATE-6 (YYMMDD) TO CCYYMMDD -> RESULT-DATE    *
      *        PIVOT YEAR 50: 00-49 -> 20XX, 50-99 -> 19XX             *
      * DT-CALENDAR 'NYSE' (DEFAULT IF SPACES) OR 'FED '.              *
      * HOLIDAY TABLE IS LOADED ON FIRST CALL FROM DDNAME HOLIDAYS.    *
      *================================================================*
       01  DT-DATE-PARMS.
           05  DT-FUNCTION             PIC X(04).
           05  DT-CALENDAR             PIC X(04).
           05  DT-DATE-1               PIC 9(08).
           05  DT-DATE-2               PIC 9(08).
           05  DT-DATE-6               PIC 9(06).
           05  DT-DAYS                 PIC S9(07)       COMP-3.
           05  DT-RESULT-DATE          PIC 9(08).
           05  DT-RESULT-NUM           PIC S9(07)       COMP-3.
           05  DT-RESULT-FLAG          PIC X(01).
               88  DT-RESULT-YES                 VALUE 'Y'.
               88  DT-RESULT-NO                  VALUE 'N'.
           05  DT-RETURN-CODE          PIC 9(02).
               88  DT-OK                         VALUE 00.
               88  DT-INVALID-DATE               VALUE 04.
               88  DT-INVALID-FUNCTION           VALUE 08.
               88  DT-CALENDAR-ERROR             VALUE 12.
           05  DT-MESSAGE              PIC X(40).
