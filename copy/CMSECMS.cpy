      *================================================================*
      * COPYBOOK   : CMSECMS                                           *
      * DESCRIPTION: SECURITY MASTER RECORD.  HOST-VARIABLE IMAGE OF   *
      *              DB2 TABLE MSEC.SECURITY_MASTER AS RETURNED BY     *
      *              I/O MODULE CMD010.  ALSO THE DSNUTILB LOAD FORMAT *
      * KEY        : SEC-CUSIP  OFFSET 0 LENGTH 9                      *
      * LRECL      : 250                                               *
      *----------------------------------------------------------------*
      * SEC-PRICE-FACTOR: EQUITIES 1.0000, BONDS 0.0100 (PRICE IS PCT  *
      * OF PAR), SOME PREFERREDS 0.0400 (QUOTED PER $25 PAR).          *
      *----------------------------------------------------------------*
      * 1987-09-01 RJK  ORIGINAL (VSAM)                                *
      * 1996-04-22 DWB  CONVERTED TO DB2 - LAYOUT KEPT        CHG02215 *
      * 1998-11-02 TLM  Y2K                                   CHG04471 *
      * 2005-08-30 KAP  ISIN ADDED                            CHG13391 *
      * 2024-02-12 NVR  T+1 - SEC-SETTLE-DAYS NOW DRIVEN FROM TABLE    *
      *================================================================*
       01  SEC-MASTER-REC.
           05  SEC-CUSIP               PIC X(09).
           05  SEC-ISIN                PIC X(12).
           05  SEC-SYMBOL              PIC X(08).
           05  SEC-DESC                PIC X(40).
           05  SEC-TYPE                PIC X(02).
               88  SEC-EQUITY                    VALUE 'EQ'.
               88  SEC-PREFERRED                 VALUE 'PF'.
               88  SEC-ADR                       VALUE 'AD'.
               88  SEC-CORP-BOND                 VALUE 'CB'.
               88  SEC-MUNI-BOND                 VALUE 'MU'.
               88  SEC-GOVT-BOND                 VALUE 'GV'.
               88  SEC-MUTUAL-FUND               VALUE 'MF'.
               88  SEC-FIXED-INCOME              VALUE 'CB' 'MU' 'GV'.
               88  SEC-EQUITY-LIKE               VALUE 'EQ' 'PF' 'AD'.
           05  SEC-CCY                 PIC X(03).
           05  SEC-COUNTRY             PIC X(02).
           05  SEC-EXCHANGE            PIC X(04).
           05  SEC-STATUS              PIC X(01).
               88  SEC-ACTIVE                    VALUE 'A'.
               88  SEC-HALTED                    VALUE 'H'.
               88  SEC-MATURED                   VALUE 'M'.
               88  SEC-DELISTED                  VALUE 'D'.
           05  SEC-PRICE-FACTOR        PIC S9(05)V9(04) COMP-3.
           05  SEC-SETTLE-DAYS         PIC 9(01).
           05  SEC-DEPOSITORY          PIC X(04).
           05  SEC-MIN-DENOM           PIC S9(09)V99    COMP-3.
           05  SEC-ISSUER-ID           PIC X(06).
           05  SEC-TYPE-DATA           PIC X(60).
      *    ---- FIXED INCOME VIEW (SEC-TYPE 'CB' 'MU' 'GV') ----------
           05  SEC-FI-DATA       REDEFINES SEC-TYPE-DATA.
               10  SEC-COUPON-RATE     PIC S9(03)V9(06) COMP-3.
               10  SEC-MATURITY-DATE   PIC 9(08).
               10  SEC-ISSUE-DATE      PIC 9(08).
               10  SEC-FIRST-CPN-DATE  PIC 9(08).
               10  SEC-DAYCOUNT        PIC X(02).
                   88  SEC-DC-30-360             VALUE '30'.
                   88  SEC-DC-ACT-360            VALUE 'A3'.
                   88  SEC-DC-ACT-ACT            VALUE 'AA'.
               10  SEC-COUPON-FREQ     PIC 9(01).
               10  SEC-CALLABLE-FLAG   PIC X(01).
               10  SEC-TAX-EXEMPT-FLAG PIC X(01).
               10  FILLER              PIC X(26).
      *    ---- EQUITY VIEW (SEC-TYPE 'EQ' 'PF' 'AD' 'MF') -----------
           05  SEC-EQ-DATA       REDEFINES SEC-TYPE-DATA.
               10  SEC-SHARES-OUT      PIC S9(13)       COMP-3.
               10  SEC-DIV-FREQ        PIC 9(01).
               10  SEC-ADR-RATIO       PIC S9(03)V9(04) COMP-3.
               10  SEC-PAR-VALUE       PIC S9(07)V9(04) COMP-3.
               10  SEC-SIC-CODE        PIC X(04).
               10  SEC-SHORT-SALE-FLAG PIC X(01).
               10  FILLER              PIC X(37).
           05  SEC-LAST-UPD-DATE       PIC 9(08).
           05  SEC-LAST-UPD-USER       PIC X(08).
           05  FILLER                  PIC X(71).
