      *================================================================*
      * COPYBOOK   : SRPSTJ                                            *
      * DESCRIPTION: POSITION POSTING JOURNAL - ONE RECORD PER         *
      *              ACTIVITY LEG POSTED BY SRB200, WITH BEFORE/AFTER  *
      *              QUANTITIES.  INPUT TO GL GENERATION (SRB600).     *
      *              DSN MSEC.PROD.SR.POSTJRNL(+1)                     *
      * RECFM/LRECL: FB / 300                                          *
      *================================================================*
       01  PSJ-JOURNAL-REC.
           05  PSJ-ACTIVITY            PIC X(200).
           05  PSJ-BEFORE-TD-QTY       PIC S9(11)V9(04) COMP-3.
           05  PSJ-AFTER-TD-QTY        PIC S9(11)V9(04) COMP-3.
           05  PSJ-BEFORE-AVG-COST     PIC S9(09)V9(06) COMP-3.
           05  PSJ-AFTER-AVG-COST      PIC S9(09)V9(06) COMP-3.
           05  PSJ-REALIZED-PL         PIC S9(15)V99    COMP-3.
           05  PSJ-POST-ACTION         PIC X(01).
               88  PSJ-INSERTED                  VALUE 'I'.
               88  PSJ-UPDATED                   VALUE 'U'.
               88  PSJ-FLATTENED                 VALUE 'F'.
           05  PSJ-POST-TS             PIC X(26).
           05  FILLER                  PIC X(32).
