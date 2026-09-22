      *================================================================*
      * COPYBOOK   : CMACCT                                            *
      * DESCRIPTION: ACCOUNT MASTER RECORD (VSAM KSDS)                 *
      *              DSN MSEC.PROD.CM.ACCTMAST.KSDS  DDNAME ACCTMAST   *
      * KEY        : ACCT-NO  OFFSET 0 LENGTH 10                       *
      * RECFM/LRECL: F / 300                                           *
      *----------------------------------------------------------------*
      * NOTE: FIRM AND STREET-SIDE ACCOUNTS BEGIN WITH AN ALPHABETIC   *
      *       CHARACTER (E.G. 'FIRMINV001', 'STREETDTC0').  CLIENT     *
      *       ACCOUNTS ARE ALL-NUMERIC.                                *
      *----------------------------------------------------------------*
      * 1987-09-01 RJK  ORIGINAL                                       *
      * 1994-02-11 DWB  ADDED INSTITUTIONAL SECTION (REDEFINES)        *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2003-05-19 KAP  W-8BEN TAX COUNTRY                    CHG11020 *
      * 2016-10-03 SPA  DTC PARTICIPANT FOR OMNIBUS           CHG30112 *
      *================================================================*
       01  ACCT-MASTER-REC.
           05  ACCT-NO                 PIC X(10).
           05  ACCT-NAME               PIC X(40).
           05  ACCT-TYPE               PIC X(02).
               88  ACCT-INDIVIDUAL               VALUE 'IN'.
               88  ACCT-JOINT                    VALUE 'JT'.
               88  ACCT-INSTITUTIONAL            VALUE 'IS'.
               88  ACCT-OMNIBUS                  VALUE 'OM'.
               88  ACCT-FIRM-INVENTORY           VALUE 'FI'.
               88  ACCT-STREET-SIDE              VALUE 'ST'.
               88  ACCT-RETAIL                   VALUE 'IN' 'JT'.
           05  ACCT-STATUS             PIC X(01).
               88  ACCT-ACTIVE                   VALUE 'A'.
               88  ACCT-CLOSED                   VALUE 'C'.
               88  ACCT-RESTRICTED               VALUE 'R'.
               88  ACCT-DECEASED                 VALUE 'D'.
           05  ACCT-BRANCH             PIC X(03).
           05  ACCT-REP                PIC X(04).
           05  ACCT-TAX-STATUS         PIC X(01).
               88  ACCT-TAX-DOMESTIC             VALUE 'D'.
               88  ACCT-TAX-FOREIGN              VALUE 'F'.
               88  ACCT-TAX-EXEMPT               VALUE 'E'.
           05  ACCT-TAX-COUNTRY        PIC X(02).
           05  ACCT-BASE-CCY           PIC X(03).
           05  ACCT-COMM-SCHED         PIC X(02).
           05  ACCT-OPEN-DATE          PIC 9(08).
           05  ACCT-CLOSE-DATE         PIC 9(08).
           05  ACCT-DVP-FLAG           PIC X(01).
               88  ACCT-DVP                      VALUE 'Y'.
           05  ACCT-DTC-PARTICIPANT    PIC X(04).
           05  ACCT-MARGIN-TYPE        PIC X(01).
               88  ACCT-CASH-ACCT                VALUE 'C'.
               88  ACCT-MARGIN-ACCT              VALUE 'M'.
           05  ACCT-LAST-ACTIVITY      PIC 9(08).
           05  ACCT-STMT-CYCLE         PIC X(01).
               88  ACCT-STMT-MONTHLY             VALUE 'M'.
               88  ACCT-STMT-QUARTERLY           VALUE 'Q'.
           05  ACCT-TYPE-DATA          PIC X(80).
      *    ---- RETAIL VIEW (ACCT-TYPE 'IN' 'JT') ---------------------
           05  ACCT-RETAIL-DATA  REDEFINES ACCT-TYPE-DATA.
               10  ACCT-RTL-ADDR-1     PIC X(30).
               10  ACCT-RTL-ADDR-2     PIC X(30).
               10  ACCT-RTL-STATE      PIC X(02).
               10  ACCT-RTL-ZIP        PIC X(09).
               10  ACCT-RTL-BIRTH-YY   PIC 9(02).
               10  ACCT-RTL-RISK-CODE  PIC X(01).
               10  FILLER              PIC X(06).
      *    ---- INSTITUTIONAL VIEW (ACCT-TYPE 'IS' 'OM') --------------
           05  ACCT-INST-DATA    REDEFINES ACCT-TYPE-DATA.
               10  ACCT-INST-LEI       PIC X(20).
               10  ACCT-INST-BIC       PIC X(11).
               10  ACCT-INST-AGENT     PIC X(04).
               10  ACCT-INST-CUST-ACCT PIC X(12).
               10  ACCT-INST-COMM-RATE PIC S9(03)V9(06) COMP-3.
               10  ACCT-INST-SOFT-DOL  PIC X(01).
               10  ACCT-INST-SUB-CNT   PIC S9(04) COMP.
               10  FILLER              PIC X(25).
      *    ---- FIRM / STREET VIEW (ACCT-TYPE 'FI' 'ST') ---------------
           05  ACCT-FIRM-DATA    REDEFINES ACCT-TYPE-DATA.
               10  ACCT-FIRM-DESK      PIC X(04).
               10  ACCT-FIRM-GL-ACCT   PIC X(10).
               10  ACCT-FIRM-LOCATION  PIC X(04).
               10  ACCT-FIRM-TRADER    PIC X(06).
               10  FILLER              PIC X(56).
           05  ACCT-LAST-MAINT-DATE    PIC 9(08).
           05  ACCT-LAST-MAINT-USER    PIC X(08).
           05  FILLER                  PIC X(105).
