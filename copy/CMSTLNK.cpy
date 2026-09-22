      *================================================================*
      * COPYBOOK   : CMSTLNK                                           *
      * DESCRIPTION: LINKAGE FOR SETTLEMENT DATE MODULE CMU020.        *
      *             CALL 'CMU020' USING SD-SETTLE-PARMS.               *
      * RULES: EQ/PF/AD/MF T+1 (FROM 2024-05-28, T+2 BEFORE),          *
      *        GV T+1 FED CALENDAR, CB/MU T+1 (T+2 BEFORE 2024-05-28), *
      *        SD-SETTLE-DAYS-OVR > 0 OVERRIDES THE RULE.              *
      *        NON-USD SECURITIES USE T+2 ALWAYS.                      *
      *================================================================*
       01  SD-SETTLE-PARMS.
           05  SD-TRADE-DATE           PIC 9(08).
           05  SD-SEC-TYPE             PIC X(02).
           05  SD-CCY                  PIC X(03).
           05  SD-MARKET               PIC X(04).
           05  SD-SETTLE-DAYS-OVR      PIC 9(01).
           05  SD-SETTLE-DATE          PIC 9(08).
           05  SD-SETTLE-CYCLE         PIC 9(01).
           05  SD-RETURN-CODE          PIC 9(02).
               88  SD-OK                         VALUE 00.
               88  SD-BAD-TRADE-DATE             VALUE 04.
               88  SD-TRADE-DATE-HOLIDAY         VALUE 06.
           05  SD-MESSAGE              PIC X(40).
