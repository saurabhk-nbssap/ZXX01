function sap_convert_to_xls_format .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME
*"     VALUE(I_APPL_KEEP) TYPE  CHAR01 DEFAULT SPACE
*"  TABLES
*"      I_TAB_SAP_DATA TYPE  STANDARD TABLE
*"  CHANGING
*"     VALUE(I_TAB_CONVERTED_DATA) TYPE  TRUXS_T_TEXT_DATA OPTIONAL
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------
  type-pools: soi.
  constants: g_con_excel type char80 value 'Excel.Sheet',
             g_max_empty_rows type i value 5.

  data l_oref_container type ref to cl_gui_custom_container.
  data l_iref_control type ref to i_oi_container_control.
  data l_iref_error type ref to i_oi_error.
  data l_iref_document type ref to i_oi_document_proxy.
  data:l_itab_entries like sy-tabix.
  data  l_iref_spreadsheet type ref to i_oi_spreadsheet.
  data  l_retcode type soi_ret_string.
  data: l_file like  rlgrap-filename,
        l_current_row type i,
        l_return(40)  type c,
        l_percentage(4)  type c,
        l_text80(80),
        l_text6(6),
        l_tabix type i,
        l_top type i,
        l_left type i,
        l_rows type i,
        l_columns type i.
  data: l_oref_structure type ref to cl_abap_structdescr.
  data: l_range_list type soi_range_list.
  data: l_table      type soi_generic_table.
  data: l_fields_table type table of rfc_fields.

  field-symbols <fs_data>.

  check not i_tab_sap_data[] is initial.
  l_file = i_filename.

  perform create_spreadsheet using g_con_excel
                             changing
                                      i_filename
                                      l_oref_container l_iref_control
                                      l_iref_error     l_iref_document
                                      l_iref_spreadsheet.

  if l_iref_spreadsheet is initial.
    message e893(ux) with i_filename raising conversion_failed.
  endif.

  l_oref_structure ?= cl_abap_typedescr=>describe_by_data(
                      i_tab_sap_data ).

  perform build_field_table tables l_fields_table
                            using l_oref_structure.

* adjust column and rows ...
  describe table l_oref_structure->components lines l_columns.
  describe table i_tab_sap_data lines l_itab_entries.
  l_left = 1.
  l_rows = l_itab_entries.
  l_top =  1.

  if sy-batch is initial.
    l_text80 = text-xls.
    call function 'SAPGUI_PROGRESS_INDICATOR'
         exporting
              percentage = 0
              text       = l_text80.
  endif.
  call method l_iref_spreadsheet->insert_range_dim
      exporting name = 'range1'
              top = l_top
              left = l_left
              rows = l_rows
              columns = l_columns
      importing retcode = l_retcode.
  call method l_iref_spreadsheet->insert_one_table
      exporting
              rangename = 'range1'
              data_table = i_tab_sap_data[]
              fields_table = l_fields_table
              wholetable = 'X'
    importing retcode = l_retcode.

  call method l_iref_document->save_document_to_url
                                 exporting url = i_filename
                                 importing retcode = l_retcode.
  if not i_appl_keep is initial and sy-batch is initial.
    g_iref_document = l_iref_document.
    g_iref_spreadsheet = l_iref_spreadsheet.
    call screen 1010.
  endif.
  call method l_iref_document->close_document
                            importing retcode = l_retcode.

  call method l_iref_document->release_document
                            importing retcode = l_retcode.

  free: l_iref_spreadsheet.
  free l_iref_document.

  call method l_iref_control->release_all_documents.
  call method l_iref_control->destroy_control.

endfunction.
