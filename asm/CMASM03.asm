*=====================================================================*
* MODULE   : CMASM03                                                  *
* TITLE    : 32-BIT CHECKSUM OF A BUFFER (CKSM INSTRUCTION)           *
* SYSTEM   : MERIDIAN EOD - COMMON SERVICES (CM)                      *
*---------------------------------------------------------------------*
* FUNCTION : RETURNS THE CHECKSUM OF HS-BUFFER(1:HS-LENGTH) COMPUTED  *
*            BY THE HARDWARE CHECKSUM INSTRUCTION WITH AN INITIAL     *
*            VALUE OF ZERO: THE BUFFER IS ADDED AS 32-BIT UNSIGNED    *
*            FULLWORDS WITH END-AROUND CARRY; A FINAL PARTIAL WORD    *
*            IS PADDED ON THE RIGHT WITH ZEROS.  CKSM MAY STOP BEFORE *
*            THE END OF THE OPERAND (CONDITION CODE 3) - THE LOOP     *
*            RE-DRIVES IT UNTIL CONDITION CODE 0.                     *
*            USED BY TCB300 FOR THE TRADE HISTORY DUPLICATE HASH      *
*            (TH-DUP-HASH) - THE VALUE IS STORED IN THE TRDHIST KSDS, *
*            SO THE ALGORITHM MUST NOT CHANGE (CHG15008).             *
*                                                                     *
* LINKAGE  : CALL 'CMASM03' USING HS-HASH-AREA   (COPYBOOK CMHSLNK)   *
*            R1  -> A(HS-HASH-AREA)                                   *
*              HS-LENGTH   F      BYTES TO HASH (1 - 256)             *
*              HS-BUFFER   CL256  DATA                                *
*              HS-HASH     F      RESULT                              *
*            LENGTH <= 0 RETURNS HASH ZERO; LENGTH > 256 IS TREATED   *
*            AS 256.  R15 = 0 ALWAYS.                                 *
*                                                                     *
* ATTRIBUTES: REENTRANT, AMODE 31, RMODE ANY.  LINK RENT,REUS.        *
*---------------------------------------------------------------------*
* CHANGE HISTORY                                                      *
* 2006-06-12 KAP  CHG15008  ORIGINAL - DUPLICATE TRADE DETECTION      *
* 2010-08-16 SPA  CHG20117  AMODE 31 / RMODE ANY, STORAGE OBTAIN      *
* 2013-11-18 JMF  CHG26031  LENGTH CAPPED AT 256 (S0C4 WHEN TCB300    *
*                           PASSED A NEGATIVE LENGTH)                 *
*=====================================================================*
CMASM03  CSECT
CMASM03  AMODE 31
CMASM03  RMODE ANY
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
MAXLEN   EQU   256                     LARGEST BUFFER ACCEPTED
*
*---------------------------------------------------------------------*
* ENTRY                                                               *
*---------------------------------------------------------------------*
         SAVE  (14,12),,'CMASM03 &SYSDATE &SYSTIME'
         LR    R12,R15
         USING CMASM03,R12
         LR    R11,R1
*
         STORAGE OBTAIN,LENGTH=WORKLEN,LOC=31,COND=NO
         LR    R10,R1
         USING WORKAREA,R10
         ST    R13,SAVEAREA+4
         LA    R2,SAVEAREA
         ST    R2,8(,R13)
         LR    R13,R2
*
         L     R9,0(,R11)              A(HS-HASH-AREA)
         LA    R9,0(,R9)
         USING HSAREA,R9
*
*---------------------------------------------------------------------*
* VALIDATE THE LENGTH                                                 *
*---------------------------------------------------------------------*
         SR    R6,R6                   CHECKSUM ACCUMULATOR = 0
         L     R3,HSLEN                LENGTH
         LTR   R3,R3
         BNP   STORE                   <= 0 - HASH IS ZERO
         CH    R3,=AL2(MAXLEN)
         BNH   LENOK
         LA    R3,MAXLEN               CAP AT 256
LENOK    DS    0H
*
*---------------------------------------------------------------------*
* CHECKSUM - R6 = RESULT, R2/R3 = EVEN/ODD PAIR (ADDRESS/LENGTH)      *
*---------------------------------------------------------------------*
         LA    R2,HSBUF                A(DATA)
CKSMLOOP CKSM  R6,R2                   ADD FULLWORDS, END-AROUND CARRY
         BC    1,CKSMLOOP              CC3 - NOT FINISHED, RE-DRIVE
*
STORE    DS    0H
         ST    R6,HSHASH               RETURN THE 32-BIT RESULT
*
*---------------------------------------------------------------------*
* EXIT                                                                *
*---------------------------------------------------------------------*
         DROP  R9
         L     R13,SAVEAREA+4
         LR    R1,R10
         DROP  R10
         STORAGE RELEASE,LENGTH=WORKLEN,ADDR=(1),COND=NO
         RETURN (14,12),RC=0
*
         LTORG
*
*---------------------------------------------------------------------*
* DYNAMIC WORK AREA (REENTRANT)                                       *
*---------------------------------------------------------------------*
WORKAREA DSECT
SAVEAREA DS    18F
         DS    0D
WORKLEN  EQU   *-WORKAREA
*
*---------------------------------------------------------------------*
* CALLER'S PARAMETER (COPYBOOK CMHSLNK)                               *
*---------------------------------------------------------------------*
HSAREA   DSECT
HSLEN    DS    F                       HS-LENGTH   PIC S9(8) COMP
HSBUF    DS    CL256                   HS-BUFFER
HSHASH   DS    F                       HS-HASH     PIC S9(8) COMP
*
         END   CMASM03
