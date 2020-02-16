FUNCTION ZSUSR_AUTH_DISPLAY_WITH_VALUES.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(OBJECT) TYPE  USR12-OBJCT
*"     REFERENCE(AUTH) TYPE  USR12-AUTH
*"     VALUE(P_STATE) TYPE  USR12-AKTPS DEFAULT 'A'
*"  EXCEPTIONS
*"      OBJECT_DONT_EXIST
*"--------------------------------------------------------------------
  SELECT SINGLE * FROM usr12
         WHERE objct  = object
           AND auth   = auth
           AND aktps  = p_state.
  IF sy-subrc <> 0.
    RAISE object_dont_exist.
  ENDIF.

  PERFORM display_hierarchie_auth USING object auth p_state.


ENDFUNCTION.
