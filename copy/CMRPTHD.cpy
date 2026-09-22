      *================================================================*
      * COPYBOOK   : CMRPTHD                                           *
      * DESCRIPTION: STANDARD REPORT HEADINGS (133 BYTE FBA, ASA CC IN *
      *              COLUMN 1).  CALLER MOVES REPORT-ID, TITLE AND     *
      *              PAGE NUMBER BEFORE WRITING.  60 LINES PER PAGE.   *
      *================================================================*
       01  RPT-HEADING-1.
           05  RPT-H1-CC               PIC X(01)  VALUE '1'.
           05  RPT-H1-REPORT-ID        PIC X(08)  VALUE SPACES.
           05  FILLER                  PIC X(20)  VALUE SPACES.
           05  FILLER                  PIC X(40)  VALUE
               'MERIDIAN SECURITIES LLC - NEW YORK'.
           05  FILLER                  PIC X(32)  VALUE SPACES.
           05  FILLER                  PIC X(10)  VALUE 'RUN DATE: '.
           05  RPT-H1-RUN-DATE         PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(02)  VALUE SPACES.
           05  FILLER                  PIC X(05)  VALUE 'PAGE '.
           05  RPT-H1-PAGE             PIC ZZZZ9.
       01  RPT-HEADING-2.
           05  RPT-H2-CC               PIC X(01)  VALUE ' '.
           05  RPT-H2-PROGRAM          PIC X(08)  VALUE SPACES.
           05  FILLER                  PIC X(20)  VALUE SPACES.
           05  RPT-H2-TITLE            PIC X(60)  VALUE SPACES.
           05  FILLER                  PIC X(14)  VALUE SPACES.
           05  FILLER                  PIC X(10)  VALUE 'BUS DATE: '.
           05  RPT-H2-BUS-DATE         PIC X(10)  VALUE SPACES.
           05  FILLER                  PIC X(10)  VALUE SPACES.
       01  RPT-BLANK-LINE.
           05  FILLER                  PIC X(01)  VALUE ' '.
           05  FILLER                  PIC X(132) VALUE SPACES.
       01  RPT-END-LINE.
           05  FILLER                  PIC X(01)  VALUE '0'.
           05  FILLER                  PIC X(50)  VALUE SPACES.
           05  FILLER                  PIC X(30)  VALUE
               '***  END OF REPORT  ***'.
           05  FILLER                  PIC X(52)  VALUE SPACES.
       01  RPT-CONTROL-FIELDS.
           05  RPT-LINE-COUNT          PIC S9(03) COMP-3 VALUE +99.
           05  RPT-PAGE-COUNT          PIC S9(05) COMP-3 VALUE ZERO.
           05  RPT-LINES-PER-PAGE      PIC S9(03) COMP-3 VALUE +60.
