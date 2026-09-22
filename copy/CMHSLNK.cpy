      *================================================================*
      * COPYBOOK   : CMHSLNK                                           *
      * DESCRIPTION: LINKAGE FOR HLASM ROUTINE CMASM03 (HASH).         *
      *             CALL 'CMASM03' USING HS-HASH-AREA.                 *
      * 32-BIT CRC OF HS-BUFFER(1:HS-LENGTH) USING THE CKSM            *
      * INSTRUCTION.  RESULT IS DEPENDENT ON THE EBCDIC BYTE VALUES    *
      * OF THE BUFFER.  MAX LENGTH 256.                                *
      *================================================================*
       01  HS-HASH-AREA.
           05  HS-LENGTH               PIC S9(08)       COMP.
           05  HS-BUFFER               PIC X(256).
           05  HS-HASH                 PIC S9(08)       COMP.
