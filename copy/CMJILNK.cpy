      *================================================================*
      * COPYBOOK   : CMJILNK                                           *
      * DESCRIPTION: LINKAGE FOR HLASM ROUTINE CMASM01 (JOB INFO).     *
      *             CALL 'CMASM01' USING JI-JOB-INFO.                  *
      * RETURNS JOB/STEP/PROCSTEP NAMES FROM TIOT AND JOB ID FROM      *
      * THE JSCB/SSIB.  NO RETURN CODE - ALWAYS SUCCEEDS.              *
      *================================================================*
       01  JI-JOB-INFO.
           05  JI-JOBNAME              PIC X(08).
           05  JI-STEPNAME             PIC X(08).
           05  JI-PROCSTEP             PIC X(08).
           05  JI-JOBID                PIC X(08).
