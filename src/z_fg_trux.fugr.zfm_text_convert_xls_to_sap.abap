function zfm_text_convert_xls_to_sap .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_TAB_RAW_DATA) TYPE  TRUXS_T_TEXT_DATA
*"     VALUE(I_FILENAME) TYPE  STRING
*"  TABLES
*"      I_TAB_CONVERTED_DATA TYPE  STANDARD TABLE
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"--------------------------------------------------------------------
  type-pools: soi, cntl.
  constants: g_con_excel      type char80 value 'Excel.Sheet',
             g_max_empty_rows type i value 5.

  data l_oref_container type ref to cl_gui_custom_container.
  data l_iref_control type ref to i_oi_container_control.
  data l_iref_error type ref to i_oi_error.
  data l_iref_document type ref to i_oi_document_proxy.
  data: l_cntl_handle type cntl_handle.
  data  l_iref_spreadsheet type ref to i_oi_spreadsheet.
  data  l_retcode type soi_ret_string.
  data: l_current_row type i,
        l_tabix       type i,
        l_top         type i,
        l_left        type i,
        l_rows        type i,
        l_columns     type i.
  data: l_oref_structure type ref to cl_abap_structdescr.
  data: l_range_list type soi_range_list.
  data: l_table       type soi_generic_table.
  data: l_table_range type soi_generic_table.
  data: l_gen_tab like line of l_table.
  field-symbols <fs_data>.

  data:
    l_text6(6),
    l_text80(80).

  try.
      perform zget_spreadsheet_interface using g_con_excel
                                        changing
                                          i_filename
                                          l_oref_container l_iref_control
                                          l_iref_error     l_iref_document
                                          l_iref_spreadsheet.
    catch cx_sy_range_out_of_bounds.
  endtry.

  if l_iref_spreadsheet is initial.
    message e893(ux) with i_filename raising conversion_failed.
  endif.

  l_oref_structure ?= cl_abap_typedescr=>describe_by_data(
                      i_tab_converted_data ).
* adjust column and rows ...
  describe table l_oref_structure->components lines l_columns.
  l_left = 1.
  l_rows = 100.
  if not i_line_header is initial.
    l_top = 2.
  else.
    l_top = 1.
  endif.

  while l_current_row <= g_max_empty_rows.
    refresh l_table_range.
    call method l_iref_spreadsheet->set_selection
      exporting
        top     = l_top
        left    = l_left
        rows    = l_rows
        columns = l_columns
      importing
        retcode = l_retcode.
    call method l_iref_spreadsheet->insert_range
      exporting
        columns = l_columns
        rows    = l_rows
        name    = 'SAP_range1'
      importing
        retcode = l_retcode.

    call method l_iref_spreadsheet->get_ranges_names
      importing
        ranges  = l_range_list
        retcode = l_retcode.

    delete l_range_list where name <> 'SAP_range1'.

    call method l_iref_spreadsheet->get_ranges_data
*             exporting all      = 'X'
      importing
        contents = l_table_range
        retcode  = l_retcode
      changing
        ranges   = l_range_list.
    loop at l_table_range into l_gen_tab.
      at new row.
        refresh l_table.
      endat.

      append l_gen_tab to l_table.

      at end of row.
        perform parse_table_line using    l_table sy-tabix
                                 changing i_tab_converted_data.
        if i_tab_converted_data is initial.
          l_current_row = l_current_row + 1.
        else.
          append i_tab_converted_data.
          l_tabix = sy-tabix.
          if sy-batch is initial.
            l_text80 = text-kox.
            l_text6 = l_tabix.
            replace '&&&&&&' with l_text6 into l_text80.
            replace '&&&&&&' with text-exl into l_text80.
            condense l_text80.
            call function 'SAPGUI_PROGRESS_INDICATOR'
              exporting
                text = l_text80.
          endif.
          clear l_current_row.
        endif.
      endat.
    endloop.
    l_top = l_top + l_rows.
  endwhile.

  free: l_iref_spreadsheet.
  call method l_iref_document->close_document.
  call method l_iref_document->release_document.
  free l_iref_document.

  call method l_iref_control->release_all_documents.
  call method l_iref_control->destroy_control.

* correct the decimals
  perform correct_decimals_for_current tables i_tab_converted_data.


endfunction.

*&---------------------------------------------------------------------*
*&      Form  GET_SPREADSHEET_INTERFACE
*&---------------------------------------------------------------------*
form zget_spreadsheet_interface
      using
           value(pi_application) type char80
      changing
            value(pc_filename) type string "rlgrap-filename
            value(pc_oref_container) type ref to cl_gui_custom_container
            value(pc_iref_control) type ref to i_oi_container_control
            value(pc_iref_error) type ref to i_oi_error
            value(pc_iref_document) type ref to i_oi_document_proxy
            value(pc_iref_spreadsheet).
  data l_item_url(256) type c.
  data l_retcode type soi_ret_string.
  data l_has type i.

* don't do anything in batch, because there is no GUI...
  check sy-batch is initial.

  l_item_url = pc_filename.
  set locale language sy-langu.
  translate pc_filename to upper case.
  set locale language space.

  if pc_filename(7) <> 'HTTP://' and pc_filename(7) <> 'FILE://'.
    concatenate 'FILE://' l_item_url into l_item_url.
  endif.
  pc_filename = l_item_url.

  call method c_oi_container_control_creator=>get_container_control
    importing
      control = pc_iref_control
      error   = pc_iref_error.
  create object pc_oref_container
    exporting
      container_name = 'TRUX_CONTAINER'.

*  call method pc_oref_container->set_visible exporting visible = space.

  call method pc_iref_control->init_control
    exporting
      r3_application_name      =
                                 'R/3 TR'        "#EC NOTEXT
      inplace_enabled          = 'X'
      inplace_scroll_documents = 'X'
      parent                   = pc_oref_container
*     register_on_close_event  = 'X'
*     register_on_custom_event = 'X'
*     no_flush                 = 'X'
    importing
      error                    = pc_iref_error.

  call method pc_iref_control->get_document_proxy
    exporting
      document_type  = pi_application
    importing
      document_proxy = pc_iref_document
      error          = pc_iref_error.

  call method pc_iref_document->open_document
    exporting
      open_inplace = 'X'
      document_url = l_item_url                     "open_readonly = 'X'
    importing
      retcode      = l_retcode.

  call method pc_iref_document->has_spreadsheet_interface
    importing
      is_available = l_has.

  if not l_has is initial.
    call method pc_iref_document->get_spreadsheet_interface
      importing
        sheet_interface = pc_iref_spreadsheet.
  endif.

endform.                               " GET_SPREADSHEET_INTERFACE

include ltruxf01.
include ltruxu22.   "FILE_READ_AND_CONVERT_SAP_DATA
include ltruxu23.   "SAP_DATA_CONVERT_WRITE_FILE
include ltruxu24.   "TEXT_CONVERT_CSV_TO_SAP
include ltruxu26.   "TEXT_CONVERT_TEX_TO_SAP
include ltruxu27.   "TEXT_CONVERT_TXT_TO_SAP
include ltruxu31.   "TEXT_CONVERT_XLS_TO_SAP
include ltruxu33.   "CREATE_XLS_HEADER_FROM_DDIC
include ltruxu41.   "TEXT_CONVERT_XML_TO_SAP
include ltruxu01.   "NACHKOMMASTELLEN_SETZEN
include ltruxu02.   "LOAN_CHECK_STRUCTURE_INIT
include ltruxu03.   "LOAN_DOMAEN_MANAGER
include ltruxu04.   "SAP_CONVERT_TO_CSV_FORMAT
include ltruxu05.   "SAP_CONVERT_TO_TXT_FORMAT
include ltruxu06.   "SAP_CONVERT_TO_TEX_FORMAT
include ltruxu07.   "SAP_CONVERT_TO_XLS_FORMAT
include ltruxu08.   "SAP_CONVERT_TO_XML_FORMAT
include ltruxu09.   "DISPLAY_BTCI_TREE
include ltruxu10.   "FILE_READ_AND_GET_TAB
include ltruxu11.   "SAP_STARTS_EXCEL
include ltruxu12.   "CHECK_REQUIRED_FIELDS
include ltruxu13.   "CLEAR_EIS_INITIAL_VALUE
include ltruxu14.   "LOAN_CHECK_DOMAENS_STRUCTURE
include ltruxu15.   "TRUT_MOVE_CORRESPONDING
include ltruxu16.   "TRFDUE_ERROR_HANDLER
include ltruxu17.   "LOAN_MOVE_CORRESPONDING
include ltruxu18.   "LOAN_CHECK_STRUCTURE_COMPLETE
include ltruxu19.   "TRANSLATE_CODEPAGE_IN
include ltruxu20.   "TRANSLATE_CODEPAGE_OUT
include ltruxu21.   "PROCESS_GUI_DDIC_CONVERSION
include ltruxu25.   "TRUT_MOVE_CORRESPONDING_TABLE
include ltruxu28.   "CALL_TRANSACTION_FROM_TABLE_CO
