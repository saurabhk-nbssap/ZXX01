FUNCTION ZSUSR_PROF_DISPLAY_WITH_AUTHS.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PROFILE) LIKE  USR10-PROFN
*"     VALUE(P_STATE) DEFAULT 'A'
*"  EXCEPTIONS
*"      PROF_DONT_EXIST
*"--------------------------------------------------------------------
  SELECT SINGLE * FROM usr10
         WHERE profn = profile
         AND   aktps = p_state.
  IF sy-subrc <> 0.
    RAISE prof_dont_exist.
  ENDIF.
  PERFORM display_hierarchie_prof USING profile p_state.
ENDFUNCTION.
