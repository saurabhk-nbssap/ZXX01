FUNCTION ZUSR_USER_DISPLAY_WITH_S_TCODE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(USER) LIKE  USR02-BNAME
*"  EXCEPTIONS
*"      USER_DOESNT_EXIST
*"--------------------------------------------------------------------
  SELECT SINGLE * FROM USR02
         WHERE BNAME = USER.
  IF SY-SUBRC <> 0.
    RAISE USER_DOESNT_EXIST.
  ENDIF.
  PERFORM DISPLAY_HIERARCHIE_S_TCODE USING USER.
ENDFUNCTION.
