*=====================================================================*
* MODULE   : CMASM02                                                  *
* TITLE    : LOCAL TIMESTAMP IN DB2 FORMAT FROM THE TOD CLOCK         *
* SYSTEM   : MERIDIAN EOD - COMMON SERVICES (CM)                      *
*---------------------------------------------------------------------*
* FUNCTION : STORES THE TOD CLOCK (STCK) AND RETURNS THE LOCAL DATE   *
*            AND TIME AS A 26-BYTE DB2-STYLE TIMESTAMP:               *
*                  YYYY-MM-DD-HH.MM.SS.NNNNNN                         *
*            UTC IS CONVERTED TO LOCAL TIME WITH THE CVT LOCAL TIME   *
*            OFFSET (CVTLDTO) LESS THE LEAP SECOND OFFSET (CVTLSO),   *
*            THEN STCKCONV PRODUCES PACKED DATE AND TIME WHICH ARE    *
*            UNPACKED AND EDITED INTO THE CALLER'S AREA.              *
*            USED FOR AUDIT, CONTROL TOTAL AND CHECKPOINT RECORDS,    *
*            AND BY CMB010 FOR THE CALENDAR DATE ON THE DATE CARD.    *
*                                                                     *
* LINKAGE  : CALL 'CMASM02' USING TS-TIMESTAMP-AREA (COPYBOOK CMTSLNK)*
*            R1  -> A(TS-TIMESTAMP-AREA)                              *
*              TS-TIMESTAMP   CL26  LOCAL TIME, EDITED                *
*              TS-STCK-VALUE  XL8   RAW STCK VALUE (UTC)              *
*            R15 = 0 ALWAYS.  IF STCKCONV FAILS THE TIMESTAMP IS      *
*            SET TO 0001-01-01-00.00.00.000000.                       *
*                                                                     *
* ATTRIBUTES: REENTRANT, AMODE 31, RMODE ANY.  LINK RENT,REUS.        *
*            USES 64-BIT REGISTERS (Z/ARCHITECTURE) FOR THE OFFSETS.  *
*---------------------------------------------------------------------*
* CHANGE HISTORY                                                      *
* 1999-06-07 TLM  CHG04471  ORIGINAL (Y2K) - REPLACES COBOL ACCEPT    *
*                           FROM DATE / TIME (2-DIGIT YEAR)           *
* 2006-06-12 KAP  CHG15008  MICROSECONDS IN THE TIMESTAMP             *
* 2010-08-16 SPA  CHG20117  AMODE 31 / RMODE ANY, STORAGE OBTAIN      *
* 2015-03-09 JMF  CHG28102  LEAP SECOND OFFSET (CVTLSO) SUBTRACTED -  *
*                           TIMESTAMPS WERE 26 SECONDS AHEAD OF DB2   *
*=====================================================================*
CMASM02  CSECT
CMASM02  AMODE 31
CMASM02  RMODE ANY
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
* ENTRY                                                               *
*---------------------------------------------------------------------*
         SAVE  (14,12),,'CMASM02 &SYSDATE &SYSTIME'
         LR    R12,R15
         USING CMASM02,R12
         LR    R11,R1                  PARAMETER LIST
*
         STORAGE OBTAIN,LENGTH=WORKLEN,LOC=31,COND=NO
         LR    R10,R1
         USING WORKAREA,R10
         XC    WORKAREA(WORKLEN),WORKAREA
         ST    R13,SAVEAREA+4
         LA    R2,SAVEAREA
         ST    R2,8(,R13)
         LR    R13,R2
*
         L     R9,0(,R11)              A(TS-TIMESTAMP-AREA)
         LA    R9,0(,R9)
         USING TSAREA,R9
*
*---------------------------------------------------------------------*
* READ THE CLOCK AND RETURN THE RAW VALUE                             *
*---------------------------------------------------------------------*
         STCK  WSTCK                   TOD CLOCK (UTC)
         MVC   TSSTCK,WSTCK
*
*---------------------------------------------------------------------*
* UTC -> LOCAL:  LOCAL = STCK - CVTLSO + CVTLDTO                      *
*---------------------------------------------------------------------*
         L     R3,CVTPTR               A(CVT)
         USING CVT,R3
         L     R4,CVTEXT2              A(CVT EXTENSION 2)
         DROP  R3
         USING CVTXTNT2,R4
         LG    R5,WSTCK                64-BIT TOD VALUE
         SLG   R5,CVTLSO               LESS LEAP SECONDS
         ALG   R5,CVTLDTO              PLUS LOCAL OFFSET (SIGNED)
         STG   R5,WLOCAL
         DROP  R4
*
*---------------------------------------------------------------------*
* CONVERT TO PACKED DATE / TIME                                       *
*   WCONV+0  XL8  HHMMSSTHMIJU0000  (TIME, UNSIGNED PACKED DIGITS)    *
*   WCONV+8  XL4  YYYYMMDD          (DATE, UNSIGNED PACKED DIGITS)    *
*---------------------------------------------------------------------*
         MVC   WSTKCL(STKCLEN),STKCLIST
         STCKCONV STCKVAL=WLOCAL,CONVVAL=WCONV,TIMETYPE=DEC,           X
               DATETYPE=YYYYMMDD,MF=(E,WSTKCL)
         LTR   R15,R15
         BNZ   BADCONV
*
*---------------------------------------------------------------------*
* UNPACK - APPEND A SIGN NIBBLE (X'0F') SO UNPK GIVES ZONED DIGITS    *
*---------------------------------------------------------------------*
         MVC   WDATEP(4),WCONV+8       YYYYMMDD
         MVI   WDATEP+4,X'0F'          -> 9 DIGITS + SIGN
         UNPK  WDATEZ(9),WDATEP(5)     'YYYYMMDD0'
         MVC   WTIMEP(6),WCONV         HHMMSSTHMIJU
         MVI   WTIMEP+6,X'0F'          -> 13 DIGITS + SIGN
         UNPK  WTIMEZ(13),WTIMEP(7)    'HHMMSSTHMIJU0'
*
*---------------------------------------------------------------------*
* EDIT YYYY-MM-DD-HH.MM.SS.NNNNNN                                     *
*---------------------------------------------------------------------*
         MVC   TSSTAMP(4),WDATEZ       YYYY
         MVI   TSSTAMP+4,C'-'
         MVC   TSSTAMP+5(2),WDATEZ+4   MM
         MVI   TSSTAMP+7,C'-'
         MVC   TSSTAMP+8(2),WDATEZ+6   DD
         MVI   TSSTAMP+10,C'-'
         MVC   TSSTAMP+11(2),WTIMEZ    HH
         MVI   TSSTAMP+13,C'.'
         MVC   TSSTAMP+14(2),WTIMEZ+2  MM
         MVI   TSSTAMP+16,C'.'
         MVC   TSSTAMP+17(2),WTIMEZ+4  SS
         MVI   TSSTAMP+19,C'.'
         MVC   TSSTAMP+20(6),WTIMEZ+6  NNNNNN (MICROSECONDS)
         B     EXIT
*
BADCONV  DS    0H
         MVC   TSSTAMP,NULLTS          CONVERSION FAILED
*
*---------------------------------------------------------------------*
* EXIT                                                                *
*---------------------------------------------------------------------*
EXIT     DS    0H
         DROP  R9
         L     R13,SAVEAREA+4
         LR    R1,R10
         DROP  R10
         STORAGE RELEASE,LENGTH=WORKLEN,ADDR=(1),COND=NO
         RETURN (14,12),RC=0
*
*---------------------------------------------------------------------*
* CONSTANTS                                                           *
*---------------------------------------------------------------------*
NULLTS   DC    CL26'0001-01-01-00.00.00.000000'
STKCLIST STCKCONV MF=L                 LIST FORM MODEL
STKCLEN  EQU   *-STKCLIST
         LTORG
*
*---------------------------------------------------------------------*
* DYNAMIC WORK AREA (REENTRANT)                                       *
*---------------------------------------------------------------------*
WORKAREA DSECT
SAVEAREA DS    18F
         DS    0D
WSTCK    DS    XL8                     RAW TOD CLOCK
WLOCAL   DS    XL8                     LOCAL TOD
WCONV    DS    XL16                    STCKCONV OUTPUT
WDATEP   DS    XL5                     PACKED DATE + SIGN
WTIMEP   DS    XL7                     PACKED TIME + SIGN
WDATEZ   DS    CL9                     ZONED DATE
WTIMEZ   DS    CL13                    ZONED TIME
         DS    0D
WSTKCL   DS    XL(STKCLEN)             STCKCONV PARAMETER LIST
         DS    0D
WORKLEN  EQU   *-WORKAREA
*
*---------------------------------------------------------------------*
* CALLER'S PARAMETER (COPYBOOK CMTSLNK)                               *
*---------------------------------------------------------------------*
TSAREA   DSECT
TSSTAMP  DS    CL26                    TS-TIMESTAMP
TSSTCK   DS    XL8                     TS-STCK-VALUE
*
*---------------------------------------------------------------------*
* SYSTEM MAPPINGS                                                     *
*---------------------------------------------------------------------*
         CVT   DSECT=YES,LIST=NO
         END   CMASM02
