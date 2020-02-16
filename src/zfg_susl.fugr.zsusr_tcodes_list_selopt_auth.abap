FUNCTION ZSUSR_TCODES_LIST_SELOPT_AUTH.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(OBJECT) LIKE  TOBJ-OBJCT DEFAULT 'S_USER_GRP'
*"     VALUE(AUTH) LIKE  USR12-AUTH DEFAULT 'S_USER_ALL'
*"     VALUE(SELTYPE) LIKE  USSEL1-SELTYPE
*"--------------------------------------------------------------------
  REFRESH AUTS.
  CLEAR AUTS.
  AUTS-OBJECT  = OBJECT.
  AUTS-AUTH    = AUTH.
  APPEND AUTS.
  CALL FUNCTION 'SUSR_GET_TCODES_WITH_AUTH_LIST'
     EXPORTING
          SELTYPE =  SELTYPE
          SELTYPETEXT = 'AU'
          NAME    =  OBJECT
          NAME2   =   AUTH
          AUTHS   =   AUTS
     EXCEPTIONS
          OTHERS  = 1.
ENDFUNCTION.
