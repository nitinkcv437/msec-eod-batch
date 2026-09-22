      *================================================================*
      * COPYBOOK   : TCCOMLNK                                          *
      * DESCRIPTION: LINKAGE FOR COMMISSION CALCULATOR TCU210.         *
      *             CALL 'TCU210' USING CO-COMMISSION-PARMS.           *
      *----------------------------------------------------------------*
      * CO-COMM-SCHED   FROM ACCT-COMM-SCHED (01-05)                   *
      * CO-INST-RATE-BPS FROM ACCT-INST-COMM-RATE (SCHEDULE 03 ONLY)   *
      * CO-RULE-APPLIED TIER PSHR INST ZERO LGCY FIXI PRIN MIN  MAX    *
      * CO-RETURN-CODE  00 OK                                          *
      *                 04 UNKNOWN SCHEDULE - SCHEDULE 01 APPLIED      *
      *                 08 INVALID INPUT (QTY/PRINCIPAL NOT POSITIVE)  *
      *----------------------------------------------------------------*
      * 1989-06-05 RJK  ORIGINAL                                       *
      * 1995-03-13 DWB  SCHEDULE 05 ADDED                     CHG01702 *
      * 2001-04-09 KAP  DECIMALIZATION - PRICE 8 DECIMALS     CHG08814 *
      *================================================================*
       01  CO-COMMISSION-PARMS.
           05  CO-COMM-SCHED           PIC X(02).
           05  CO-SEC-TYPE             PIC X(02).
           05  CO-CAPACITY             PIC X(01).
           05  CO-SIDE                 PIC X(02).
           05  CO-QTY                  PIC S9(11)V9(04) COMP-3.
           05  CO-PRICE                PIC S9(09)V9(08) COMP-3.
           05  CO-PRINCIPAL            PIC S9(15)V99    COMP-3.
           05  CO-INST-RATE-BPS        PIC S9(03)V9(06) COMP-3.
           05  CO-TRADE-DATE           PIC 9(08).
           05  CO-COMMISSION           PIC S9(11)V99    COMP-3.
           05  CO-RULE-APPLIED         PIC X(04).
           05  CO-RETURN-CODE          PIC 9(02).
               88  CO-OK                         VALUE 00.
               88  CO-SCHED-DEFAULTED            VALUE 04.
               88  CO-INVALID-INPUT              VALUE 08.
           05  CO-MESSAGE              PIC X(40).
