      *================================================================*
      * COPYBOOK   : SWMSG                                             *
      * DESCRIPTION: SWIFT FIN MESSAGE TEXT LINE.  ONE RECORD PER LINE *
      *             OF AN ISO 15022 MESSAGE.  USED FOR OUTBOUND        *
      *             INSTRUCTIONS (MSEC.PROD.SW.OUTMSG) AND INBOUND     *
      *             CONFIRMATIONS/STATUS (MSEC.PROD.SW.INMSG.RAW)      *
      * RECFM/LRECL: FB / 120                                          *
      *----------------------------------------------------------------*
      * A MESSAGE STARTS WITH LINE 001 '{1:...}{2:...}{4:' AND ENDS    *
      * WITH A LINE '-}'.  FIELDS ':16R:' / ':16S:' OPEN/CLOSE BLOCKS. *
      *================================================================*
       01  SWM-MSG-LINE.
           05  SWM-MSG-SEQ             PIC 9(08).
           05  SWM-LINE-NO             PIC 9(03).
           05  SWM-MSG-TYPE            PIC X(03).
           05  SWM-DIRECTION           PIC X(01).
               88  SWM-OUTBOUND                  VALUE 'O'.
               88  SWM-INBOUND                   VALUE 'I'.
           05  SWM-TEXT                PIC X(105).
