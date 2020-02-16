FUNCTION ZSUSR_TCODES_LIST_SELOPT_PROF.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PROFILE) LIKE  USR10-PROFN DEFAULT 'S_USER_ALL'
*"     VALUE(SELTYPE) LIKE  USSEL1-SELTYPE DEFAULT ' '
*"--------------------------------------------------------------------
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
  REFRESH AUTS.
  CLEAR AUTS.
  LOOP AT AUTHLIST.
    AUTS-OBJECT  = AUTHLIST-OBJECT.
    AUTS-AUTH    = AUTHLIST-AUTH.
    APPEND AUTS.
  ENDLOOP.
  CALL FUNCTION 'SUSR_GET_TCODES_WITH_AUTH_LIST'
     EXPORTING
          SELTYPE = SELTYPE
          SELTYPETEXT = 'PR'
          NAME    = PROFILE
          AUTHS   = AUTS
     EXCEPTIONS
          OTHERS  = 1.
ENDFUNCTION.
