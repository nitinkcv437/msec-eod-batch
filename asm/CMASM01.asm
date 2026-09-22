*=====================================================================*
* MODULE   : CMASM01                                                  *
* TITLE    : RETURN JOB NAME, STEP NAME, PROC STEP NAME AND JOB ID    *
* SYSTEM   : MERIDIAN EOD - COMMON SERVICES (CM)                      *
*---------------------------------------------------------------------*
* FUNCTION : CALLED FROM COBOL (CMU050, CMU060, CMU070, CMU080) TO    *
*            IDENTIFY THE RUNNING JOB FOR AUDIT, CONTROL TOTAL AND    *
*            CHECKPOINT RECORDS.                                      *
*              JOB NAME   - TIOCNJOB                                  *
*              STEP NAME  - TIOCJSTP WHEN RUNNING IN A PROCEDURE      *
*                           (THE EXEC STATEMENT THAT CALLED THE PROC),*
*                           OTHERWISE TIOCSTPN                        *
*              PROC STEP  - TIOCSTPN WHEN RUNNING IN A PROCEDURE,     *
*                           OTHERWISE BLANK                           *
*              JOB ID     - JES2 JOB ID FROM THE JSAB (IAZXJSAB)      *
*            THE TIOT IS LOCATED THROUGH PSA -> CURRENT TCB -> TIOT.  *
*                                                                     *
* LINKAGE  : CALL 'CMASM01' USING JI-JOB-INFO      (COPYBOOK CMJILNK) *
*            R1  -> A(JI-JOB-INFO)  (HIGH-ORDER BIT = END OF LIST)    *
*            JI-JOB-INFO   DS 0CL32                                   *
*              JI-JOBNAME   CL8                                       *
*              JI-STEPNAME  CL8                                       *
*              JI-PROCSTEP  CL8                                       *
*              JI-JOBID     CL8                                       *
*            NO RETURN CODE - ALWAYS RETURNS R15 = 0.                 *
*                                                                     *
* ATTRIBUTES: REENTRANT, AMODE 31, RMODE ANY, PROBLEM STATE, KEY 8.   *
*            LINK WITH RENT,REUS.  CALLED DYNAMICALLY (DYNAM).        *
*                                                                     *
* REGISTERS: R3  CURRENT TCB           R9  CALLER'S JI-JOB-INFO       *
*            R4  TIOT                  R10 DYNAMIC WORK AREA          *
*            R11 PARAMETER LIST        R12 BASE                       *
*---------------------------------------------------------------------*
* CHANGE HISTORY                                                      *
* 1991-02-04 RJK            ORIGINAL - JOB NAME AND STEP NAME ONLY    *
* 1994-09-12 DWB  CHG01288  PROC STEP NAME FOR CHECKPOINT KEY         *
* 2002-03-11 KAP  CHG09982  JES JOB ID VIA IAZXJSAB (WAS SSIB)        *
* 2010-08-16 SPA  CHG20117  AMODE 31 / RMODE ANY, STORAGE OBTAIN      *
*                           INSTEAD OF GETMAIN R                      *
*=====================================================================*
CMASM01  CSECT
CMASM01  AMODE 31
CMASM01  RMODE ANY
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
*---------------------------------------------------------------------*
* ENTRY - STANDARD LINKAGE, OBTAIN DYNAMIC AREA, CHAIN SAVE AREAS     *
*---------------------------------------------------------------------*
         SAVE  (14,12),,'CMASM01 &SYSDATE &SYSTIME'
         LR    R12,R15                 BASE REGISTER
         USING CMASM01,R12
         LR    R11,R1                  SAVE PARAMETER LIST ADDRESS
*
         STORAGE OBTAIN,LENGTH=WORKLEN,LOC=31,COND=NO
         LR    R10,R1                  A(DYNAMIC AREA)
         USING WORKAREA,R10
         XC    WORKAREA(WORKLEN),WORKAREA
         ST    R13,SAVEAREA+4          BACKWARD CHAIN
         LA    R2,SAVEAREA
         ST    R2,8(,R13)              FORWARD CHAIN
         LR    R13,R2                  OUR SAVE AREA
*
*---------------------------------------------------------------------*
* ADDRESS THE CALLER'S JI-JOB-INFO AND BLANK IT                       *
*---------------------------------------------------------------------*
         L     R9,0(,R11)              A(JI-JOB-INFO)
         LA    R9,0(,R9)               CLEAR END-OF-LIST BIT
         USING JOBINFO,R9
         MVC   JIJOBNM,BLANKS
         MVC   JISTEPNM,BLANKS
         MVC   JIPROCST,BLANKS
         MVC   JIJOBID,BLANKS
*
*---------------------------------------------------------------------*
* PSA -> TCB -> TIOT                                                  *
*---------------------------------------------------------------------*
         USING PSA,0                   PSA IS AT LOCATION ZERO
         L     R3,PSATOLD              A(CURRENT TCB)
         DROP  0
         USING TCB,R3
         L     R4,TCBTIO               A(TIOT)
         DROP  R3
         LTR   R4,R4                   TIOT PRESENT?
         BZ    GETJOBID                NO - LEAVE NAMES BLANK
         USING TIOTPFX,R4
         MVC   JIJOBNM,TIOCNJOB        JOB NAME
         CLI   TIOCJSTP,C' '           STEP INVOKED A PROCEDURE?
         BNH   NOPROC                  NO  - BLANK OR ZEROS
         MVC   JISTEPNM,TIOCJSTP       YES - EXEC STATEMENT NAME
         MVC   JIPROCST,TIOCSTPN             PROC STEP NAME
         B     GETJOBID
NOPROC   DS    0H
         MVC   JISTEPNM,TIOCSTPN       STEP NAME, NO PROC STEP
         DROP  R4
*
*---------------------------------------------------------------------*
* JES JOB ID FROM THE JOB SCHEDULER ANCHOR BLOCK                      *
*---------------------------------------------------------------------*
GETJOBID DS    0H
         IAZXJSAB READ,JOBID=WJOBID
         LTR   R15,R15                 JSAB AVAILABLE?
         BNZ   NOJOBID                 NO  - E.G. STARTED TASK
         MVC   JIJOBID,WJOBID
         B     EXIT
NOJOBID  DS    0H
         MVC   JIJOBID,UNKNOWN
*
*---------------------------------------------------------------------*
* EXIT - RESTORE CALLER'S SAVE AREA, RELEASE DYNAMIC AREA, RETURN     *
*---------------------------------------------------------------------*
EXIT     DS    0H
         DROP  R9
         L     R13,SAVEAREA+4          CALLER'S SAVE AREA
         LR    R1,R10
         DROP  R10
         STORAGE RELEASE,LENGTH=WORKLEN,ADDR=(1),COND=NO
         RETURN (14,12),RC=0
*
*---------------------------------------------------------------------*
* CONSTANTS                                                           *
*---------------------------------------------------------------------*
BLANKS   DC    CL8' '
UNKNOWN  DC    CL8'UNKNOWN'
         LTORG
*
*---------------------------------------------------------------------*
* DYNAMIC WORK AREA (REENTRANT)                                       *
*---------------------------------------------------------------------*
WORKAREA DSECT
SAVEAREA DS    18F                     STANDARD 72-BYTE SAVE AREA
WJOBID   DS    CL8                     JOB ID FROM IAZXJSAB
         DS    0D
WORKLEN  EQU   *-WORKAREA
*
*---------------------------------------------------------------------*
* CALLER'S PARAMETER (COPYBOOK CMJILNK)                               *
*---------------------------------------------------------------------*
JOBINFO  DSECT
JIJOBNM  DS    CL8                     JI-JOBNAME
JISTEPNM DS    CL8                     JI-STEPNAME
JIPROCST DS    CL8                     JI-PROCSTEP
JIJOBID  DS    CL8                     JI-JOBID
*
*---------------------------------------------------------------------*
* TIOT PREFIX - SAME OFFSETS AS IEFTIOT1 (ONLY THE FIELDS WE USE)     *
*---------------------------------------------------------------------*
TIOTPFX  DSECT
TIOCNJOB DS    CL8                     JOB NAME
TIOCSTPN DS    CL8                     STEP NAME / PROC STEP NAME
TIOCJSTP DS    CL8                     JOB STEP NAME IF IN A PROC
*
*---------------------------------------------------------------------*
* SYSTEM MAPPINGS                                                     *
*---------------------------------------------------------------------*
         IHAPSA DSECT=YES,LIST=NO
         IKJTCB DSECT=YES,LIST=NO
         END   CMASM01
