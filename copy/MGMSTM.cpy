      *================================================================*
      * COPYBOOK   : MGMSTM                                            *
      * DESCRIPTION: MONTHLY MARGIN ACCOUNT SUMMARY - ONE RECORD PER   *
      *             MARGIN ACCOUNT FOR THE MONTH (INTEREST CHARGED,    *
      *             CALL ACTIVITY, MONTH-END REQUIREMENT) AND ONE FIRM *
      *             TOTAL.  WRITTEN BY MGB400, PRINTED BY MGR410,      *
      *             MARGIN SECTION OF THE CLIENT STATEMENT.            *
      *             DSN MSEC.PROD.MG.MSTMT(+1)                         *
      * RECFM/LRECL: FB / 200                                          *
      *----------------------------------------------------------------*
      * MMS-REC-TYPE  'A' ACCOUNT   'T' FIRM TOTAL (LAST)              *
      * MMS-STATUS    MONTH-END MARGIN STATUS G/H/T/M, 'X' = ACCOUNT   *
      *               CHARGED INTEREST BUT NO LONGER IN THE MARGIN RUN *
      *----------------------------------------------------------------*
      * 2015-11-30 SPA  ORIGINAL                              CHG28844 *
      *================================================================*
       01  MMS-MONTHLY-REC.
           05  MMS-REC-TYPE            PIC X(01).
               88  MMS-ACCOUNT-REC               VALUE 'A'.
               88  MMS-TOTAL-REC                 VALUE 'T'.
           05  MMS-BUS-DATE            PIC 9(08).
           05  MMS-MONTH               PIC 9(06).
           05  MMS-ACCT-NO             PIC X(10).
           05  MMS-BRANCH              PIC X(03).
           05  MMS-REP                 PIC X(04).
           05  MMS-ACCT-NAME           PIC X(30).
           05  MMS-STATUS              PIC X(01).
           05  MMS-LONG-MV             PIC S9(15)V99    COMP-3.
           05  MMS-EQUITY              PIC S9(15)V99    COMP-3.
           05  MMS-DEBIT-BALANCE       PIC S9(15)V99    COMP-3.
           05  MMS-EXCESS              PIC S9(15)V99    COMP-3.
           05  MMS-SMA                 PIC S9(15)V99    COMP-3.
           05  MMS-AVG-DEBIT           PIC S9(15)V99    COMP-3.
           05  MMS-INT-DAYS            PIC S9(03)       COMP-3.
           05  MMS-INT-CHARGED         PIC S9(11)V99    COMP-3.
           05  MMS-INT-FLAG            PIC X(01).
               88  MMS-INT-POSTED                VALUE 'P'.
               88  MMS-INT-WAIVED                VALUE 'W'.
               88  MMS-INT-NOT-POSTED            VALUE 'N'.
           05  MMS-CALLS-ISSUED        PIC S9(03)       COMP-3.
           05  MMS-CALLS-MET           PIC S9(03)       COMP-3.
           05  MMS-CALLS-LIQ           PIC S9(03)       COMP-3.
           05  MMS-CALLS-OPEN          PIC S9(03)       COMP-3.
           05  MMS-EXTENSIONS          PIC S9(03)       COMP-3.
           05  MMS-CALL-AMT-ISSUED     PIC S9(13)V99    COMP-3.
           05  MMS-LARGEST-CALL-TYPE   PIC X(02).
           05  MMS-WATCH-FLAG          PIC X(01).
               88  MMS-ON-WATCH                  VALUE 'W'.
           05  FILLER                  PIC X(52).
