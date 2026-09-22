      *================================================================*
      * COPYBOOK   : SRGLSUM                                           *
      * DESCRIPTION: GL JOURNAL BUILD CONTROL / EXCEPTION RECORD.      *
      *              WRITTEN BY SRB600, READ BY SRR610 FOR THE DR = CR *
      *              PROOF.  RECORD TYPES:                             *
      *                'X' REFERENCE OUT OF BALANCE                    *
      *                'U' UNMAPPED TXN CODE (SUSPENSE USED)           *
      *                'T' RUN TOTALS (ALWAYS LAST, ALWAYS ONE)        *
      *              DSN MSEC.PROD.SR.GLSUMM(+1)                       *
      * RECFM/LRECL: FB / 100                                          *
      *================================================================*
       01  GLS-SUMMARY-REC.
           05  GLS-REC-TYPE            PIC X(01).
               88  GLS-OUT-OF-BALANCE            VALUE 'X'.
               88  GLS-UNMAPPED                  VALUE 'U'.
               88  GLS-RUN-TOTALS                VALUE 'T'.
           05  GLS-BUS-DATE            PIC 9(08).
           05  GLS-BODY                PIC X(91).
           05  GLS-EXC-BODY      REDEFINES GLS-BODY.
               10  GLS-SOURCE          PIC X(02).
               10  GLS-REF             PIC X(16).
               10  GLS-TXN-CODE        PIC X(04).
               10  GLS-ACCT-TYPE       PIC X(02).
               10  GLS-SEC-TYPE        PIC X(02).
               10  GLS-DR-AMOUNT       PIC S9(15)V99    COMP-3.
               10  GLS-CR-AMOUNT       PIC S9(15)V99    COMP-3.
               10  FILLER              PIC X(47).
           05  GLS-TOT-BODY      REDEFINES GLS-BODY.
               10  GLS-JRNL-IN         PIC S9(09)       COMP-3.
               10  GLS-ACCR-IN         PIC S9(09)       COMP-3.
               10  GLS-LINES-OUT       PIC S9(09)       COMP-3.
               10  GLS-REFS-OUT        PIC S9(09)       COMP-3.
               10  GLS-REFS-UNBAL      PIC S9(09)       COMP-3.
               10  GLS-UNMAPPED-CNT    PIC S9(09)       COMP-3.
               10  GLS-TOTAL-DR        PIC S9(15)V99    COMP-3.
               10  GLS-TOTAL-CR        PIC S9(15)V99    COMP-3.
               10  FILLER              PIC X(43).
