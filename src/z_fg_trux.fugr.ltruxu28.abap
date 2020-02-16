FUNCTION CALL_TRANSACTION_FROM_TABLE_CO.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"     VALUE(I_MODE) TYPE  CHAR01 DEFAULT 'N'
*"     VALUE(I_UPDATE) TYPE  CHAR01 DEFAULT 'A'
*"  TABLES
*"      T_BDCDATA STRUCTURE  BDCDATA OPTIONAL
*"      T_BDCMESSAGES STRUCTURE  BDCMSGCOLL OPTIONAL
*"      T_PARAMETER_IDS STRUCTURE  BDCDATA OPTIONAL
*"  EXCEPTIONS
*"      ERROR_FOUND
*"----------------------------------------------------------------------
  data: ctu_params like ctu_params.
  data: l_memoryid type memoryid.
  data authrc type i.
  call function 'AUTHORITY_CHECK_TCODE'
    exporting
      tcode  = i_tcode
    exceptions
      ok     = 0
      not_ok = 2
      others = 3.
  authrc = sy-subrc.

  ctu_params-dismode = i_mode.
  ctu_params-updmode = i_update.
  ctu_params-defsize = c_true.
  describe table t_bdcdata lines sy-tabix.
  if sy-tabix <> 0.
    if authrc = 0.
      call transaction i_tcode using    t_bdcdata
                             options from ctu_params
                             messages into t_bdcmessages.
    check sy-subrc <> c_rc0.
    raise error_found.
    endif.
  else.
    loop at t_parameter_ids.
      l_memoryid = t_parameter_ids-fnam.
      set parameter id l_memoryid field t_parameter_ids-fval.
    endloop.
    if sy-subrc = 0.
      if authrc = 0.
        call transaction i_tcode and skip first screen.
      endif.
    else.
      if authrc = 0.
        call transaction i_tcode.
      endif.
    endif.
  endif.

ENDFUNCTION.
