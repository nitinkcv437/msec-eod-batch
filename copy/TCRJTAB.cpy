      *================================================================*
      * COPYBOOK   : TCRJTAB                                           *
      * DESCRIPTION: TRADE CAPTURE REJECT / WARNING CODE TABLE.        *
      *              USED BY THE REJECT REPORT (TCR410) TO PRINT THE   *
      *              CODE DESCRIPTION.  KEEP IN CODE SEQUENCE WITHIN   *
      *              EACH GROUP.  ADD NEW CODES AT THE END OF A GROUP  *
      *              AND BUMP TC-RJ-ENTRIES.                           *
      *----------------------------------------------------------------*
      * 1988-02-01 RJK  ORIGINAL                                       *
      * 1995-07-17 DWB  OMS FEED CODES                        CHG01877 *
      * 2002-03-08 KAP  WARNING CODES W0XX                    CHG09930 *
      * 2006-06-12 KAP  D001 DUPLICATE SUSPECT                CHG15008 *
      * 2014-03-03 SPA  S012/S013 MAKER CHECKER               CHG26120 *
      *================================================================*
       01  TC-REJECT-CODE-TABLE.
           05  FILLER    PIC X(40) VALUE
               'S001INVALID RECORD TYPE                 '.
           05  FILLER    PIC X(40) VALUE
               'S002DETAIL BEFORE HEADER RECORD         '.
           05  FILLER    PIC X(40) VALUE
               'S003QUANTITY NOT NUMERIC                '.
           05  FILLER    PIC X(40) VALUE
               'S004PRICE NOT NUMERIC                   '.
           05  FILLER    PIC X(40) VALUE
               'S005DUPLICATE HEADER RECORD             '.
           05  FILLER    PIC X(40) VALUE
               'S006TRAILER RECORD COUNT MISMATCH       '.
           05  FILLER    PIC X(40) VALUE
               'S007TRAILER HASH TOTAL MISMATCH         '.
           05  FILLER    PIC X(40) VALUE
               'S008RECORD FOUND AFTER TRAILER          '.
           05  FILLER    PIC X(40) VALUE
               'S009TRAILER RECORD MISSING              '.
           05  FILLER    PIC X(40) VALUE
               'S010INVALID TXN/CANCEL INDICATOR        '.
           05  FILLER    PIC X(40) VALUE
               'S011ORIGINAL TRADE ID MISSING           '.
           05  FILLER    PIC X(40) VALUE
               'S012ENTERED-BY EQUALS APPROVED-BY       '.
           05  FILLER    PIC X(40) VALUE
               'S013APPROVER MISSING                    '.
           05  FILLER    PIC X(40) VALUE
               'S014INVALID BUY/SELL INDICATOR          '.
           05  FILLER    PIC X(40) VALUE
               'S015INVALID CURRENCY CODE               '.
           05  FILLER    PIC X(40) VALUE
               'S016INVALID CAPACITY CODE               '.
           05  FILLER    PIC X(40) VALUE
               'S017TRADE ID MISSING                    '.
           05  FILLER    PIC X(40) VALUE
               'A001ACCOUNT NOT ON MASTER               '.
           05  FILLER    PIC X(40) VALUE
               'A002ACCOUNT CLOSED                      '.
           05  FILLER    PIC X(40) VALUE
               'A003ACCOUNT RESTRICTED                  '.
           05  FILLER    PIC X(40) VALUE
               'A004ACCOUNT DECEASED - ESTATE HOLD      '.
           05  FILLER    PIC X(40) VALUE
               'A005ACCOUNT TYPE NOT ALLOWED            '.
           05  FILLER    PIC X(40) VALUE
               'A006ACCOUNT NUMBER INVALID FORMAT       '.
           05  FILLER    PIC X(40) VALUE
               'P001SECURITY NOT FOUND                  '.
           05  FILLER    PIC X(40) VALUE
               'P002SECURITY NOT ACTIVE                 '.
           05  FILLER    PIC X(40) VALUE
               'P003SECURITY TYPE NOT VALID FOR FEED    '.
           05  FILLER    PIC X(40) VALUE
               'Q001QUANTITY ZERO OR INVALID            '.
           05  FILLER    PIC X(40) VALUE
               'Q002PRICE ZERO OR INVALID               '.
           05  FILLER    PIC X(40) VALUE
               'Q003FACE NOT MULTIPLE OF MIN DENOM      '.
           05  FILLER    PIC X(40) VALUE
               'Q004ACCRUED OVERRIDE INVALID            '.
           05  FILLER    PIC X(40) VALUE
               'D001SUSPECTED DUPLICATE TRADE           '.
           05  FILLER    PIC X(40) VALUE
               'D002INVALID TRADE DATE                  '.
           05  FILLER    PIC X(40) VALUE
               'D003TRADE DATE IN THE FUTURE            '.
           05  FILLER    PIC X(40) VALUE
               'D004TRADE DATE TOO OLD (> 5 BUS DAYS)   '.
           05  FILLER    PIC X(40) VALUE
               'D005INVALID SETTLEMENT DATE             '.
           05  FILLER    PIC X(40) VALUE
               'D006SETTLEMENT DATE BEFORE TRADE DATE   '.
           05  FILLER    PIC X(40) VALUE
               'C001CANCEL - ORIGINAL NOT FOUND         '.
           05  FILLER    PIC X(40) VALUE
               'C002CANCEL - ORIGINAL NOT ACTIVE        '.
           05  FILLER    PIC X(40) VALUE
               'C003TRADE ID ALREADY ON HISTORY         '.
           05  FILLER    PIC X(40) VALUE
               'C004CORRECTION - ORIGINAL NOT FOUND     '.
           05  FILLER    PIC X(40) VALUE
               'C005CORRECTION - ORIGINAL NOT ACTIVE    '.
           05  FILLER    PIC X(40) VALUE
               'C006CANCEL ACCOUNT/CUSIP MISMATCH       '.
           05  FILLER    PIC X(40) VALUE
               'E001SECURITY NOT FOUND AT ENRICHMENT    '.
           05  FILLER    PIC X(40) VALUE
               'E002ACCOUNT NOT FOUND AT ENRICHMENT     '.
           05  FILLER    PIC X(40) VALUE
               'E003SETTLEMENT DATE CALC FAILED         '.
           05  FILLER    PIC X(40) VALUE
               'E004FX RATE NOT AVAILABLE               '.
           05  FILLER    PIC X(40) VALUE
               'E005FEE MODULE FAILURE                  '.
           05  FILLER    PIC X(40) VALUE
               'E006ACCRUED INTEREST CALC FAILED        '.
           05  FILLER    PIC X(40) VALUE
               'E007COMMISSION CALC FAILED              '.
           05  FILLER    PIC X(40) VALUE
               'E008PRINCIPAL ZERO OR OVERFLOW          '.
           05  FILLER    PIC X(40) VALUE
               'W001PRICE OUTSIDE TOLERANCE OF CLOSE    '.
           05  FILLER    PIC X(40) VALUE
               'W002NO CLOSING PRICE FOR TOLERANCE      '.
           05  FILLER    PIC X(40) VALUE
               'W003STALE FX RATE USED                  '.
           05  FILLER    PIC X(40) VALUE
               'W004COMMISSION SCHEDULE DEFAULTED       '.
           05  FILLER    PIC X(40) VALUE
               'W005TRADE OUT OF SEQUENCE               '.
           05  FILLER    PIC X(40) VALUE
               'W006SHORT SALE - SECURITY NOT SHORTABLE '.
           05  FILLER    PIC X(40) VALUE
               'W007COMMISSION OVER 5 PCT OF PRINCIPAL  '.
       01  TC-REJECT-CODE-TAB  REDEFINES TC-REJECT-CODE-TABLE.
           05  TC-RJ-ENTRY             OCCURS 57 TIMES
                                       INDEXED BY TC-RJ-IDX.
               10  TC-RJ-CODE          PIC X(04).
               10  TC-RJ-DESC          PIC X(36).
       01  TC-RJ-ENTRIES               PIC S9(04) COMP VALUE +57.
