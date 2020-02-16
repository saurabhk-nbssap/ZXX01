FUNCTION ZUSR_TCODES_LIST_SELOPT_PROF_V.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PROFILE) LIKE  USR10-PROFN DEFAULT 'S_USER_ALL'
*"--------------------------------------------------------------------
TABLES USTSTCAP.    " View
DATA:  BEGIN OF WA ,
       TCODE LIKE TSTCA-TCODE,
       TTEXT LIKE TSTCT-TTEXT,
       VALUE LIKE TSTCA-VALUE,
       VON LIKE UST12-VON,
       BIS LIKE UST12-BIS.
DATA:  END OF WA.
DATA WA_TCODES LIKE WA OCCURS 50 WITH HEADER LINE.
RANGES VAL FOR WA_TCODES-VALUE.
DATA WA_PROFLIST LIKE PROFLIST.

  CALL FUNCTION 'PROFIL_AUFLOESEN'
       EXPORTING
            AKTPAS            = AKTIVATED
            PROFILE           = PROFILE
       TABLES
            AUTHLIST          = AUTHLIST
            PROFLIST          = PROFLIST
       EXCEPTIONS
            PROFILE_NOT_FOUND = 1
            OTHERS            = 2.

LOOP AT PROFLIST INTO WA_PROFLIST.
WRITE WA_PROFLIST.
ENDLOOP.
SELECT *
*       tcode ttext value von bis
                                 FROM USTSTCAP     .
WRITE: / USTSTCAP-TCODE,USTSTCAP-TTEXT.  ENDSELECT.
*                        into table  wa_tcodes
*                                  for all entries in proflist
*                                where profn = proflist-profile and
*                                sprsl = sy-langu.
  VAL-SIGN = 'I'.
  VAL-OPTION = 'EQ'.
LOOP AT WA_TCODES.
  VAL-LOW  = WA_TCODES-VON.
  VAL-HIGH = WA_TCODES-BIS.
  APPEND VAL.
  IF WA_TCODES-VALUE IN VAL.
  WRITE : / WA_TCODES-TCODE, 20 WA_TCODES-TTEXT.
    ENDIF.
ENDLOOP.
ENDFUNCTION.
