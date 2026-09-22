      *================================================================*
      * COPYBOOK   : SWVREJ                                            *
      * DESCRIPTION: SETTLEMENT INSTRUCTION PRE-VALIDATION EXCEPTION.  *
      *             ONE RECORD PER ERROR / WARNING FOUND BY SWB050.    *
      *             THE ORIGINAL TCSETIN IMAGE IS CARRIED UNCHANGED.   *
      *             DSN MSEC.PROD.SW.INSTREJ(+1)                       *
      * RECFM/LRECL: FB / 330                                          *
      *----------------------------------------------------------------*
      * SVR-SEVERITY  E = INSTRUCTION NOT SENT                         *
      *               W = WARNING ONLY, INSTRUCTION SENT               *
      *----------------------------------------------------------------*
      * 1998-02-09 DWB  ORIGINAL                              CHG03512 *
      * 2005-08-30 KAP  ISIN CHECKS - FIELD NAME ADDED        CHG13391 *
      *================================================================*
       01  SVR-EXCEPTION-REC.
           05  SVR-BUS-DATE            PIC 9(08).
           05  SVR-REJ-CODE            PIC X(04).
           05  SVR-SEVERITY            PIC X(01).
               88  SVR-ERROR                     VALUE 'E'.
               88  SVR-WARNING                   VALUE 'W'.
           05  SVR-FIELD-NAME          PIC X(12).
           05  SVR-REJ-TEXT            PIC X(50).
           05  SVR-INSTR-IMAGE         PIC X(250).
           05  FILLER                  PIC X(05).
