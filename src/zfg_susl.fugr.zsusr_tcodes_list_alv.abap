FUNCTION ZSUSR_TCODES_LIST_ALV.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TITLE) TYPE  LVC_TITLE OPTIONAL
*"  TABLES
*"      TCODES STRUCTURE  TSTCT
*"--------------------------------------------------------------------

  DATA:
*   field catalog
    ct_fieldcat TYPE  slis_t_fieldcat_alv,
    wa_fieldcat LIKE LINE OF ct_fieldcat,
*   sort order
    gt_sort     TYPE slis_t_sortinfo_alv,
    ls_sort     TYPE slis_sortinfo_alv.

* Modify field catalog.
  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = 'TCODE'.
  wa_fieldcat-ref_fieldname = 'TCODE'.
  wa_fieldcat-ref_tabname   = 'TSTC'.
  wa_fieldcat-key           = 'X'.
  wa_fieldcat-col_pos       = '1'.
  wa_fieldcat-outputlen     = '30'.
  APPEND wa_fieldcat TO ct_fieldcat .

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'TTEXT'.
  wa_fieldcat-ref_fieldname = 'TTEXT'.
  wa_fieldcat-ref_tabname = 'TSTCT'.
  wa_fieldcat-col_pos = '2'.
  wa_fieldcat-outputlen     = '100'.
  APPEND wa_fieldcat TO ct_fieldcat .
* Set sort order
  ls_sort-spos = '1'.
  ls_sort-fieldname = 'TCODE'.
* Ascending sort
  ls_sort-up = 'X'.
  APPEND ls_sort TO gt_sort.

  DATA ls_variant LIKE  disvariant.
  ls_variant-report = 'TRANS'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*      i_callback_program       = 'SAPLSUSL'
*      i_callback_pf_status_set = 'PF_STATUS_SET_TCODE'
*      i_callback_user_command  = 'USER_COMMAND_TCODE'
      it_fieldcat              = ct_fieldcat
      it_sort                  = gt_sort
      i_grid_title             = title
      is_variant               = ls_variant
      i_buffer_active          = 'X'
      i_save                   = 'A'
    TABLES
      t_outtab                 = tcodes
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFUNCTION.
