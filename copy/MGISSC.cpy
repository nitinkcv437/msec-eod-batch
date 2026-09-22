      *================================================================*
      * COPYBOOK   : MGISSC                                            *
      * DESCRIPTION: MARGIN COLLATERAL CONCENTRATION BY ISSUER -       *
      *             FIRM-WIDE TOTALS OF CLIENT MARGIN POSITIONS PER    *
      *             ISSUER.  WRITTEN BY MGB150, PRINTED BY MGR150.     *
      *             DSN MSEC.PROD.MG.ISSCONC(+1)                       *
      * RECFM/LRECL: FB / 150                                          *
      *----------------------------------------------------------------*
      * MIC-REC-TYPE 'D' ONE PER ISSUER, 'T' ONE FIRM TOTAL (LAST)     *
      *----------------------------------------------------------------*
      * 2011-03-21 SPA  ORIGINAL (CREDIT RISK REQUEST)        CHG21402 *
      * 2017-08-07 MHC  SINGLE ACCOUNT DOMINANCE FLAG         CHG31877 *
      *================================================================*
       01  MIC-ISSUER-CONC-REC.
           05  MIC-REC-TYPE            PIC X(01).
               88  MIC-ISSUER-DETAIL             VALUE 'D'.
               88  MIC-FIRM-TOTAL                VALUE 'T'.
           05  MIC-BUS-DATE            PIC 9(08).
           05  MIC-ISSUER-ID           PIC X(06).
           05  MIC-ISSUER-NAME         PIC X(30).
           05  MIC-SEC-TYPE            PIC X(02).
           05  MIC-CUSIP-COUNT         PIC S9(05)       COMP-3.
           05  MIC-ACCT-COUNT          PIC S9(07)       COMP-3.
           05  MIC-LONG-MV             PIC S9(15)V99    COMP-3.
           05  MIC-SHORT-MV            PIC S9(15)V99    COMP-3.
           05  MIC-NONMARG-MV          PIC S9(15)V99    COMP-3.
           05  MIC-REQ-AMOUNT          PIC S9(15)V99    COMP-3.
           05  MIC-LOAN-VALUE          PIC S9(15)V99    COMP-3.
           05  MIC-PCT-OF-FIRM         PIC S9(03)V9(04) COMP-3.
           05  MIC-LARGEST-ACCT        PIC X(10).
           05  MIC-LARGEST-ACCT-MV     PIC S9(15)V99    COMP-3.
           05  MIC-LARGEST-ACCT-PCT    PIC S9(03)V9(04) COMP-3.
           05  MIC-FLAG                PIC X(01).
               88  MIC-OVER-FIRM-LIMIT           VALUE 'F' 'B'.
               88  MIC-SINGLE-ACCT               VALUE 'S' 'B'.
               88  MIC-NO-FLAG                   VALUE ' '.
           05  FILLER                  PIC X(23).
