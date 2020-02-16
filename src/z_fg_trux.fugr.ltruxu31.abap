FUNCTION text_convert_xls_to_sap .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_TAB_RAW_DATA) TYPE  TRUXS_T_TEXT_DATA
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME
*"  TABLES
*"      I_TAB_CONVERTED_DATA TYPE  STANDARD TABLE
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------
  TYPE-POOLS: soi, cntl.
  CONSTANTS: g_con_excel TYPE char80 VALUE 'Excel.Sheet',
             g_max_empty_rows TYPE i VALUE 5.

  DATA l_oref_container TYPE REF TO cl_gui_custom_container.
  DATA l_iref_control TYPE REF TO i_oi_container_control.
  DATA l_iref_error TYPE REF TO i_oi_error.
  DATA l_iref_document TYPE REF TO i_oi_document_proxy.
  DATA: l_cntl_handle TYPE cntl_handle.
  DATA  l_iref_spreadsheet TYPE REF TO i_oi_spreadsheet.
  DATA  l_retcode TYPE soi_ret_string.
  DATA: l_current_row TYPE i,
        l_tabix TYPE i,
        l_top TYPE i,
        l_left TYPE i,
        l_rows TYPE i,
        l_columns TYPE i.
  DATA: l_oref_structure TYPE REF TO cl_abap_structdescr.
  DATA: l_range_list TYPE soi_range_list.
  DATA: l_table       TYPE soi_generic_table.
  DATA: l_table_range TYPE soi_generic_table.
  DATA: l_gen_tab LIKE LINE OF l_table.
  FIELD-SYMBOLS <fs_data>.

  DATA:
        l_text6(6),
        l_text80(80).

  PERFORM get_spreadsheet_interface USING g_con_excel
                                    CHANGING
                                      i_filename
                                      l_oref_container l_iref_control
                                      l_iref_error     l_iref_document
                                      l_iref_spreadsheet.

  IF l_iref_spreadsheet IS INITIAL.
    MESSAGE e893(ux) WITH i_filename RAISING conversion_failed.
  ENDIF.

  l_oref_structure ?= cl_abap_typedescr=>describe_by_data(
                      i_tab_converted_data ).
* adjust column and rows ...
  DESCRIBE TABLE l_oref_structure->components LINES l_columns.
  l_left = 1.
  l_rows = 100.
  IF NOT i_line_header IS INITIAL.
    l_top = 2.
  ELSE.
    l_top = 1.
  ENDIF.

  WHILE l_current_row <= g_max_empty_rows.
    REFRESH l_table_range.
    CALL METHOD l_iref_spreadsheet->set_selection
                 EXPORTING top   = l_top
                           left  = l_left
                           rows  = l_rows
                           columns = l_columns
                 IMPORTING
                           retcode = l_retcode.
    CALL METHOD l_iref_spreadsheet->insert_range
                 EXPORTING columns = l_columns
                           rows    = l_rows
                           name    = 'SAP_range1'
                 IMPORTING
                           retcode = l_retcode.

    CALL METHOD l_iref_spreadsheet->get_ranges_names
                 IMPORTING  ranges = l_range_list
                           retcode = l_retcode.

    DELETE l_range_list WHERE name <> 'SAP_range1'.

    CALL METHOD l_iref_spreadsheet->get_ranges_data
*             exporting all      = 'X'
                 IMPORTING contents = l_table_range
                           retcode = l_retcode
                 CHANGING  ranges = l_range_list.
    LOOP AT l_table_range INTO l_gen_tab.
      AT NEW row.
        REFRESH l_table.
      ENDAT.

      APPEND l_gen_tab TO l_table.

      AT END OF row.
        PERFORM parse_table_line USING    l_table sy-tabix
                                 CHANGING i_tab_converted_data.
        IF i_tab_converted_data IS INITIAL.
          l_current_row = l_current_row + 1.
        ELSE.
          APPEND i_tab_converted_data.
          l_tabix = sy-tabix.
          IF sy-batch IS INITIAL.
            l_text80 = text-kox.
            l_text6 = l_tabix.
            REPLACE '&&&&&&' WITH l_text6 INTO l_text80.
            REPLACE '&&&&&&' WITH text-exl INTO l_text80.
            CONDENSE l_text80.
            CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
                 EXPORTING
                      text = l_text80.
          ENDIF.
          CLEAR l_current_row.
        ENDIF.
      ENDAT.
    ENDLOOP.
    l_top = l_top + l_rows.
  ENDWHILE.

  FREE: l_iref_spreadsheet.
  CALL METHOD l_iref_document->close_document.
  CALL METHOD l_iref_document->release_document.
  FREE l_iref_document.

  CALL METHOD l_iref_control->release_all_documents.
  CALL METHOD l_iref_control->destroy_control.

* correct the decimals
  PERFORM correct_decimals_for_current TABLES i_tab_converted_data.


ENDFUNCTION.
