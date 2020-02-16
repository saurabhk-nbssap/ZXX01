FUNCTION ZSUSR_USER_DISPLAY_WITH_AUTHS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(USER) LIKE  USR02-BNAME
*"  EXCEPTIONS
*"      USER_DOESNT_EXIST
*"--------------------------------------------------------------------
  SELECT SINGLE * FROM usr02
         WHERE bname = user.
  IF sy-subrc <> 0.
    RAISE user_doesnt_exist.
  ENDIF.
  PERFORM display_hierarchie_user USING user.
ENDFUNCTION.
