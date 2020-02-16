function loan_move_corresponding.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_STRUKTUR_OLD_I1)
*"             VALUE(I_STRUKTUR_OLD_I2) OPTIONAL
*"             VALUE(I_STRUKTUR_NAME_I1) LIKE  DD02L-TABNAME OPTIONAL
*"             VALUE(I_STRUKTUR_NAME_I2) LIKE  DD02L-TABNAME OPTIONAL
*"             VALUE(I_STRUKTUR_NAME_NEW) LIKE  DD02L-TABNAME
*"                             OPTIONAL
*"       CHANGING
*"             VALUE(C_STRUKTUR_NEW)
*"       EXCEPTIONS
*"              ERROR_FOUND
*"----------------------------------------------------------------------
  call function 'TRUT_MOVE_CORRESPONDING'
       exporting
            pi_source_structure_1 = i_struktur_old_i1
            pi_source_structure_2 = i_struktur_old_i2
       changing
            pc_target_structure   = c_struktur_new
       exceptions
            error_found           = 1
            others                = 2.
  if sy-subrc <> 0.
    raise error_found.
  endif.
endfunction.
