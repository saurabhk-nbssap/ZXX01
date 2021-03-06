*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*&---------------------------------------------------------------------*
*& Class (Definition) lcl_helper
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_helper definition final.
  public section.
    types: begin of ty_alsmex_tabline,
             row   type kcd_ex_row_n,
             col   type kcd_ex_col_n,
             value type string, " c length 50,
           end of ty_alsmex_tabline,

           begin of ty_excel,
             row   type i,
             col   type i,
             value type string,
           end of ty_excel.

    types: ltty_fields_fcat type standard table of rfc_fields with default key.

    data: gt_alsmex_tabline type standard table of ty_alsmex_tabline,
          gt_excel          type standard table of ty_excel with default key.

*      value of excel-cell
    types: ty_d_itabvalue type ty_alsmex_tabline-value,
*      internal table containing the excel data
           ty_t_itab      like gt_alsmex_tabline,

*      line type of sender table
           begin of ty_s_senderline,
             line type c length 4096,
           end of ty_s_senderline,
*      sender table
           ty_t_sender type table of ty_s_senderline.
*
    types:
      begin of gty_s_cell,
        position      type string,
        value         type string,
        index         type i,
        style         type i,
        sharedstring  type string,
        output_colnum type i,
      end of gty_s_cell .
    types:
      gty_t_cells type standard table of gty_s_cell with non-unique key position initial size 1 .

    types: begin of gty_s_col,
             position    type i,
             customwidth type i,
             style       type i,
             width       type string,
             max         type i,
             min         type i,
             hidden      type i,
             bestfit     type i,
           end of gty_s_col.

    types:
      begin of gty_s_row,
        spans     type string,
        position  type i,
        cells_tab type gty_t_cells,
      end of gty_s_row .

    types:
     gty_t_cols type standard table of gty_s_col with non-unique key position initial size 1 .
    types:
     gty_t_rows type standard table of gty_s_row with non-unique key position initial size 1 .
    types:
      begin of gty_s_sheet,
        dim       type string,
        cols_tab  type gty_t_cols,
        rows_tab  type gty_t_rows,
        table_rid type string,
      end of gty_s_sheet .

    constants: gc_esc type c length 1 value '"'.

    methods:
      excel_to_itab_mass_spreadsheet  " sap office integration API
        importing
          value(iv_filename) type if_mass_spreadsheet_types=>file_name
          value(iv_sheet_no) type i default 1
        exporting
          value(et_sheets)   type if_mass_spreadsheet_types=>t_spreadsheet_by_sheetname
        returning
          value(rt_excel)    like gt_excel,

      excel_to_itab_ehfnd   " uses OpenXML/xlsx support library
        importing
          value(iv_filename)        type string
          value(iv_sheet_no)        type i default 1
          value(iv_read_all_sheets) type abap_bool default abap_false
        exporting
          value(et_sheets)          type if_mass_spreadsheet_types=>t_spreadsheet_by_sheetname
        returning
          value(rt_excel)           like gt_excel,

      sheets_to_itabs
        importing
          value(it_sheets)     type if_mass_spreadsheet_types=>t_spreadsheet_by_sheetname
        returning
          value(rt_excel_itab) type zcl_helper=>tty_excel,

      sheet_to_itab
        importing
          value(it_sheet) type if_mass_spreadsheet_types=>t_spreadsheet
        returning
          value(rr_data)  type ref to data,

      generate_table_type
        importing
          value(it_fields) type string_table
        returning
          value(rr_data)   type ref to data,

      itab_to_excel_ehfnd " uses OpenXML/xlsx support library
        importing
          value(it_multi_sheet_data) type zcl_helper=>tty_sheet
        returning
          value(rv_data)             type xstring,

      itab_to_excel_soi  " sap office integration API
        importing
          value(it_multi_sheet_data) type zcl_helper=>tty_sheet
        returning
          value(rv_data)             type xstring,

      build_fields_fcat
        importing
          value(io_struct_descr) type ref to cl_abap_structdescr
        returning
          value(rt_fields_fcat)  type ltty_fields_fcat,

      " deprecated - Do NOT Use
      excel_to_itab_ole
        importing
          value(filename)    type  rlgrap-filename
          value(i_begin_col) type  i
          value(i_begin_row) type  i
          value(i_end_col)   type  i
          value(i_end_row)   type  i
        returning
          value(rt_excel)    like gt_excel
        exceptions
          inconsistent_parameters
          upload_ole,

      " deprecated - Do NOT Use
      separated_to_intern_convert
        importing
          value(i_separator) type char1
        changing
          value(i_tab)       type ty_t_sender
          value(i_intern)    type ty_t_itab,

      " deprecated - Do NOT Use
      line_to_cell_separat
        importing
          value(i_row)       type sy-tabix
          value(ch_cell_col) type kcd_ex_col
          value(i_separator) type char1
          value(i_fdpos)     type sy-fdpos
        changing
          value(i_intern)    type ty_t_itab
          value(i_line)      type ty_s_senderline,

      " deprecated - Do NOT Use
      line_to_cell_esc_sep
        importing
          value(i_separator)    type char1
        changing
          value(i_string)       type ty_s_senderline
          value(i_sic_int)      type i
          value(i_intern_value) type ty_d_itabvalue,

      " required for xls to binary conversion and vice verca
      get_temp_file_path
        importing
          value(iv_front_end)  type abap_bool optional
          value(iv_app_server) type abap_bool optional
          value(iv_extension)  type clike     optional
        returning
          value(rv_temp_file)  type string,

      delete_temp_file
        importing
          value(iv_temp_file) type string,

      validate_file_path
        importing
          value(iv_filepath)          type string
          value(iv_contains_filename) type abap_bool
        returning
          value(rv_valid)             type abap_bool,

      excel_time_to_sap_time
        changing
          value(cv_excel_time) type any,

      excel_date_to_sap_date
        changing
          value(cv_excel_date) type any.

*  protected section.
    " placeholder

*  private section.
    " placeholder
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_helper
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_helper implementation.
  method excel_to_itab_ole.
    type-pools: ole2.

    data: excel_tab    type ty_t_sender,
          ld_separator type c length 1,
          application  type ole2_object,
          workbook     type ole2_object,
          range        type ole2_object,
          worksheet    type ole2_object,
          h_cell       type ole2_object,
          h_cell1      type ole2_object,
          ld_rc        type i,
          intern       like gt_alsmex_tabline.
*   Rückgabewert der Methode "clipboard_export     "

    clear:
      rt_excel.

* Makro für Fehlerbehandlung der Methods
    define m_message.
      case sy-subrc.
        when 0.
        when 1.
          message id sy-msgid type sy-msgty number sy-msgno
                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        when others. raise upload_ole.
      endcase.
    end-of-definition.


* check parameters
    if i_begin_row > i_end_row. raise inconsistent_parameters. endif.
    if i_begin_col > i_end_col. raise inconsistent_parameters. endif.

* Get TAB-sign for separation of fields
    class cl_abap_char_utilities definition load.
    ld_separator = cl_abap_char_utilities=>horizontal_tab.

* open file in Excel
    if application-header = space or application-handle = -1.
      create object application 'Excel.Application'.
      m_message.
    endif.
    call method of application 'Workbooks' = workbook.
    m_message.
    call method of workbook 'Open' exporting #1 = filename.
    m_message.
*  set property of application 'Visible' = 1.
*  m_message.
    get property of  application 'ACTIVESHEET' = worksheet.
    m_message.

* mark whole spread sheet
    call method of worksheet 'Cells' = h_cell
        exporting #1 = i_begin_row #2 = i_begin_col.
    m_message.
    call method of worksheet 'Cells' = h_cell1
        exporting #1 = i_end_row #2 = i_end_col.
    m_message.

    call method  of worksheet 'RANGE' = range
                   exporting #1 = h_cell #2 = h_cell1.
    m_message.
    call method of range 'SELECT'.
    m_message.

* copy marked area (whole spread sheet) into Clippboard
    call method of range 'COPY'.
    m_message.

* read clipboard into ABAP
    call method cl_gui_frontend_services=>clipboard_import
      importing
        data       = excel_tab
      exceptions
        cntl_error = 1
*       ERROR_NO_GUI         = 2
*       NOT_SUPPORTED_BY_GUI = 3
        others     = 4.
    if sy-subrc <> 0.
      message a037(alsmex).
    endif.

    separated_to_intern_convert(
      exporting
        i_separator = ld_separator
      changing
        i_tab       = excel_tab
        i_intern    = intern  ).

    rt_excel = corresponding #( intern ).

* clear clipboard
    refresh excel_tab.
    call method cl_gui_frontend_services=>clipboard_export
      importing
        data       = excel_tab
      changing
        rc         = ld_rc
      exceptions
        cntl_error = 1
*       ERROR_NO_GUI         = 2
*       NOT_SUPPORTED_BY_GUI = 3
        others     = 4.

* quit Excel and free ABAP Object - unfortunately, this does not kill
* the Excel process
    call method of application 'QUIT'.
    m_message.

* >>>>> Begin of change note 575877
* to kill the Excel process it's necessary to free all used objects
    free object h_cell.       m_message.
    free object h_cell1.      m_message.
    free object range.        m_message.
    free object worksheet.    m_message.
    free object workbook.     m_message.
    free object application.  m_message.
* <<<<< End of change note 575877
  endmethod.

  method separated_to_intern_convert.
    data: l_sic_tabix like sy-tabix,
          l_sic_col   type kcd_ex_col.
    data: l_fdpos     like sy-fdpos.
    data: w_intern like line of i_intern.

    refresh i_intern.

    loop at i_tab into data(wa).
      l_sic_tabix = sy-tabix.
      l_sic_col = 0.
      while wa ca i_separator.
        l_fdpos = sy-fdpos.
        l_sic_col = l_sic_col + 1.

        line_to_cell_separat(
          exporting
            i_row       = l_sic_tabix
            ch_cell_col = l_sic_col
            i_separator = i_separator
            i_fdpos     = l_fdpos
          changing
            i_intern    = i_intern
            i_line      = wa ).

      endwhile.
      if wa <> space.
        clear w_intern.
        w_intern-row = l_sic_tabix.
        w_intern-col = l_sic_col + 1.
        w_intern-value = wa.
        append w_intern to i_intern.
      endif.
    endloop.
  endmethod.

  method line_to_cell_separat.
    data: l_string   type ty_s_senderline.
    data: w_intern   like line of i_intern.
    data  l_sic_int  type i.

    clear w_intern.
    l_sic_int = i_fdpos.
    w_intern-row = i_row.
    l_string = i_line.
    w_intern-col = ch_cell_col.
* csv Dateien mit separator in Zelle: --> ;"abc;cd";
    if ( i_separator = ';' or  i_separator = ',' ) and
         l_string(1) = gc_esc.

      line_to_cell_esc_sep(
        exporting
          i_separator    = i_separator
        changing
          i_string       = l_string
          i_sic_int      = l_sic_int
          i_intern_value = w_intern-value ).

    else.
      if l_sic_int > 0.
        w_intern-value = i_line(l_sic_int).
      endif.
    endif.
    if l_sic_int > 0.
      append w_intern to i_intern.
    endif.
    l_sic_int = l_sic_int + 1.
    i_line = i_line+l_sic_int.
  endmethod.

  method line_to_cell_esc_sep.
    data: l_int      type i,
          l_cell_end type c length 2.
    field-symbols: <l_cell> type any.
    l_cell_end = gc_esc.
    l_cell_end+1 = i_separator .

    if i_string cs gc_esc.
      i_string = i_string+1.
      if i_string cs l_cell_end.
        l_int = sy-fdpos.
        assign i_string(l_int) to <l_cell>.
        i_intern_value = <l_cell>.
        l_int = l_int + 2.
        i_sic_int = l_int.
        i_string = i_string+l_int.
      elseif i_string cs gc_esc.
*     letzte Celle
        l_int = sy-fdpos.
        assign i_string(l_int) to <l_cell>.
        i_intern_value = <l_cell>.
        l_int = l_int + 1.
        i_sic_int = l_int.
        i_string = i_string+l_int.
        l_int = strlen( i_string ).
        if l_int > 0 . message x001(kx) . endif.
      else.
        message x001(kx) . "was ist mit csv-Format
      endif.
    endif.
  endmethod.

  method excel_to_itab_mass_spreadsheet.
    " refer cl_mass_spreadsheet_service
    clear rt_excel.
    if iv_filename is not initial.
      data(lo_imp_excel) = new cl_mass_spsh_file_imp_excel( ).
      if lo_imp_excel is bound.
        data(lv_file_path_type) = cond #( when iv_filename ca '\' then 'W'
                                          when iv_filename ca '/' then 'U' ).

        case lv_file_path_type.
          when 'W'. " windows

            cl_progress_indicator=>progress_indicate(
              exporting
                i_text               = 'Loading file from frontend'           " Progress Text (If no message transferred in I_MSG*)
                i_processed          = 1                                      " Number of Objects Already Processed
                i_total              = 2                                      " Total Number of Objects to Be Processed
                i_output_immediately = abap_true ).                           " X = Display Progress Immediately

            lo_imp_excel->set_file( exporting iv_file = conv #( iv_filename ) ). "usual filepath, gotten by cl_gui_frontend_services=>file_open_dialog
          when 'U'. " unix

            cl_progress_indicator=>progress_indicate(
              exporting
                i_text               = 'Loading file from app server'         " Progress Text (If no message transferred in I_MSG*)
                i_processed          = 1                                      " Number of Objects Already Processed
                i_total              = 2                                      " Total Number of Objects to Be Processed
                i_output_immediately = abap_true ).                           " X = Display Progress Immediately

            try.
                data(lv_temp_file) = get_temp_file_path(
                                       exporting
                                         iv_front_end = abap_true
                                         iv_extension = zcl_helper=>gc_extension-xls ).
                data(lv_file_length) = value i( ).
                data(lt_data_bin) = zcl_helper=>read_file_from_path(
                                      exporting
                                        iv_filepath     = conv #( iv_filename ) " File path on app server(AL11)
                                      importing
                                        ev_file_length  = lv_file_length ).

                if lt_data_bin is not initial and lv_file_length is not initial.
                  try.
                      data(lv_uploaded) = zcl_helper=>write_file_to_path(
                                            exporting
                                              iv_filepath    = lv_temp_file     " Path of file to write to on frontend or app server
                                              iv_overwrite   = abap_true        " Overwrite file if already exists
                                              iv_file_length = lv_file_length   " Size of binary data
                                              it_data        = lt_data_bin ).   " Binary Data
                    catch zcx_generic into data(lox_generic). " Generic Exception Class
                      message lox_generic type 'S' display like 'E'.
                      return.
                  endtry.
                endif.

                if zcl_helper=>check_file_exists( exporting iv_filepath = lv_temp_file ).
                  lo_imp_excel->set_file( exporting iv_file = conv #( lv_temp_file ) ).
                endif.

              catch zcx_generic into lox_generic. " Generic Exception Class
                message lox_generic type 'S' display like 'E'.
                return.
            endtry.
          when others.
        endcase.
      endif.

      try.
          cl_progress_indicator=>progress_indicate(
            exporting
              i_text               = 'Importing and converting excel data'  " Progress Text (If no message transferred in I_MSG*)
              i_processed          = 1                                      " Number of Objects Already Processed
              i_total              = 2                                      " Total Number of Objects to Be Processed
              i_output_immediately = abap_true ).                           " X = Display Progress Immediately

          data(lo_excel) = cast cl_mass_spreadsheet( lo_imp_excel->if_mass_spreadsheet_import~import(
                                                       exporting io_imp_conf = cl_mass_spsh_imp_conf_excel=>create_default_config( ) ) ).

          if lo_excel is not bound.
            message 'File open error' type 'E' display like 'I'.
          endif.

          lo_excel->if_mass_spreadsheet~get_data(
            importing
              et_data = data(lt_data) ).

          et_sheets = lt_data.

          try.
              data(lt_excel) = lt_data[ iv_sheet_no ]-spreadsheet.
              rt_excel = corresponding #( lt_excel mapping row = row col = column value = value ).
            catch cx_sy_itab_line_not_found ##no_handler.
          endtry.
        catch cx_mass_spreadsheet into data(lox_mass_spreadsheet).
          message lox_mass_spreadsheet->get_longtext( ) type 'E' display like 'I'.
      endtry.

      delete_temp_file( exporting iv_temp_file = lv_temp_file ).
    endif.
  endmethod.

  method excel_to_itab_ehfnd.
    clear:
      rt_excel,
      et_sheets.

    field-symbols <ls_sheet_data> type gty_s_sheet.

    data(lt_excel) = rt_excel.

    if iv_filename is not initial.
      try.
          data(lv_file_path_type) = cond #( when iv_filename ca '\' then 'W'
                                            when iv_filename ca '/' then 'U' ).

          case lv_file_path_type.
            when 'W'. " windows

              cl_progress_indicator=>progress_indicate(
                exporting
                  i_text               = 'Loading file from frontend'           " Progress Text (If no message transferred in I_MSG*)
                  i_processed          = 1                                      " Number of Objects Already Processed
                  i_total              = 2                                      " Total Number of Objects to Be Processed
                  i_output_immediately = abap_true ).                           " X = Display Progress Immediately

              data(lv_data_xstr) = cl_ehfnd_file_util=>upload_file( exporting iv_file_name = conv #( iv_filename ) ).
            when 'U'. " unix

              cl_progress_indicator=>progress_indicate(
                  exporting
                    i_text               = 'Loading file from app server'         " Progress Text (If no message transferred in I_MSG*)
                    i_processed          = 1                                      " Number of Objects Already Processed
                    i_total              = 2                                      " Total Number of Objects to Be Processed
                    i_output_immediately = abap_true ).                           " X = Display Progress Immediately

              try.
                  data(lv_file_size) = value i( ).
                  data(lt_data) = zcl_helper=>read_file_from_path(
                                    exporting
                                      iv_filepath     = iv_filename " File path on app server(AL11)
                                    importing
                                      ev_file_length  = lv_file_size ).         " Binary file length

                  if lt_data is not initial.
                    lv_data_xstr = cl_bcs_convert=>solix_to_xstring(
                                     exporting
                                       it_solix = lt_data
                                       iv_size  = lv_file_size ).
                  endif.
                catch zcx_generic into data(lox_generic). " Generic Exception Class
                  message lox_generic type 'S' display like 'E'.
                  return.
              endtry.
            when others.
          endcase.

          cl_progress_indicator=>progress_indicate(
            exporting
              i_text               = 'Importing and converting excel data'  " Progress Text (If no message transferred in I_MSG*)
              i_processed          = 1                                      " Number of Objects Already Processed
              i_total              = 2                                      " Total Number of Objects to Be Processed
              i_output_immediately = abap_true ).                           " X = Display Progress Immediately

          data(lo_xlsx) = cl_ehfnd_xlsx=>get_instance( ).
          if lo_xlsx is bound and lv_data_xstr is not initial.
            try.
                data(lo_doc) = lo_xlsx->load_doc( exporting iv_file_data = lv_data_xstr ).
                if lo_doc is bound.
                  data(lt_sheet_info) = lo_doc->get_sheets( ).

                  " The above method reads sheets in reverse order; meaning the sheets are numbered incorrectly(reversed)
                  " Following is the swapping algo to correct the sequence
                  " Sheet id's of the 1st half of the table are swapped with the corresponding 2nd half of the table...
                  " ...without altering the sheet names
                  do conv i( lines( lt_sheet_info ) / 2 ) times.
                    try.
                        assign lt_sheet_info[ sy-index ] to field-symbol(<ls_sheet_info_top>).
                        if <ls_sheet_info_top> is assigned.
                          data(ls_sheet_info_swap) = <ls_sheet_info_top>.
                          assign lt_sheet_info[ lines( lt_sheet_info ) - ( sy-index - 1 )  ] to field-symbol(<ls_sheet_info_bot>).
                          if <ls_sheet_info_bot> is assigned.
                            if <ls_sheet_info_top> <> <ls_sheet_info_bot>.  " prevents corresponding_self dump
                              <ls_sheet_info_top> = corresponding #( base ( <ls_sheet_info_top> ) <ls_sheet_info_bot> except name ).
                              <ls_sheet_info_bot> = corresponding #( base ( <ls_sheet_info_bot> ) ls_sheet_info_swap except name ).
                            endif.
                          endif.
                        endif.
                      catch cx_sy_itab_line_not_found.
                    endtry.

                    clear ls_sheet_info_swap.

                    unassign:
                      <ls_sheet_info_top>,
                      <ls_sheet_info_bot>.
                  enddo.

                  sort lt_sheet_info ascending by sheet_id.

                  if line_exists( lt_sheet_info[ sheet_id = iv_sheet_no ] ) or lines( lt_sheet_info ) >= 1.
                    if iv_read_all_sheets = abap_false and lines( lt_sheet_info ) > 1.
                      delete lt_sheet_info where sheet_id <> iv_sheet_no.
                      if lines( lt_sheet_info ) > 1.
                        delete lt_sheet_info from 2.
                      endif.
                    endif.

                    loop at lt_sheet_info into data(ls_sheet_info).
                      clear lt_excel.
                      try.
                          " sheet id's are still in reverse order in the internal attributes of the ehfnd class
                          " we fetch the corresponding sheet data using the corrected sheet id
                          data(lo_sheet) = lo_doc->get_sheet_by_id(
                                             exporting
                                               iv_sheet_id = cond #( when lines( lt_sheet_info ) > 1
                                                                     then ( lines( lt_sheet_info ) - ( ls_sheet_info-sheet_id - 1 ) )
                                                                     else ls_sheet_info-sheet_id ) ).
                          if lo_sheet is bound.
*--------------------------------------------------------------------*
                            " Access local class data from global class
                            data(lo) = cast object( lo_sheet ).

                            data(lv_fname) = conv string( 'MS_SHEET_DATA' ).

                            unassign <ls_sheet_data>.
                            assign lo->(lv_fname) to <ls_sheet_data>.

                            clear lv_fname.
                            lv_fname = 'MO_XLSX_DOC->MO_XLSX_STYLE'."->MV_STYLE_DATE'.
                            assign lo->(lv_fname) to field-symbol(<lo_style>).

                            data(lo_style) = cast object( <lo_style> ).

                            data(lv_style_date) = value i( ).
                            call method lo_style->('GET_DEFAULT_DATE_STYLE')
                              receiving
                                rv_style = lv_style_date.

                            data(lv_style_time) = value i( ).
                            call method lo_style->('GET_DEFAULT_TIME_STYLE')
                              receiving
                                rv_style = lv_style_time.
*--------------------------------------------------------------------*
                            " all sheets table
                            append initial line to et_sheets assigning field-symbol(<ls_sheet>).
                            if <ls_sheet> is assigned.
*                          if lo_sheet->has_cell_content( exporting iv_column = 1 iv_row = 1 ).
                              data(lv_column_count) = lo_sheet->get_last_column_number_in_row( exporting iv_row = 1 ).
                              data(lv_row_count) = lo_sheet->get_last_row_number( ).
                              if lv_column_count is not initial and lv_row_count is not initial.
                                <ls_sheet> = value #( sheetname = ls_sheet_info-name
                                                      num_rows  = lv_row_count
                                                      num_cols  = lv_column_count ).
                                do lv_row_count times.
                                  data(lv_row_index) = sy-index.
                                  do lv_column_count times.
                                    append initial line to lt_excel assigning field-symbol(<ls_excel>).
                                    if <ls_excel> is assigned.
                                      data(lv_column_index) = sy-index.
                                      <ls_excel>-row = lv_row_index.
                                      <ls_excel>-col = lv_column_index.
                                      <ls_excel>-value = conv #( lo_sheet->get_cell_content(
                                                                   exporting
                                                                     iv_row = lv_row_index
                                                                     iv_column = lv_column_index ) ).

                                      try.
                                          data(lv_style)
                                            = <ls_sheet_data>-rows_tab[
                                                position = conv #( lv_row_index )
                                              ]-cells_tab[
                                                  output_colnum = conv #( lv_column_index )
                                                ]-style.

                                          if lv_style = lv_style_date.
                                            excel_date_to_sap_date( changing cv_excel_date = <ls_excel>-value ).
                                          endif.
                                        catch cx_sy_itab_line_not_found ##no_handler.
                                      endtry.
                                    endif.
                                    clear lv_column_index.
                                    unassign <ls_excel>.
                                  enddo.
                                  clear lv_row_index.
                                enddo.
                                <ls_sheet>-spreadsheet = corresponding #( lt_excel mapping row = row column = col value = value ).
                                " return data for the sheet requested in standard table format
                                if ls_sheet_info-sheet_id = iv_sheet_no or lines( lt_sheet_info ) = 1.
                                  rt_excel = lt_excel.
                                endif.
                              endif.
*                          endif.
                            endif.
                            unassign <ls_sheet>.
                          endif.

                          clear:
                            lo,
                            lv_fname,
                            lo_style,
                            lv_style_date,
                            lv_style_time.

                          unassign:
                            <ls_sheet_data>,
                            <lo_style>.
                        catch cx_openxml_format into data(lox_openxml_format).    " Packaging Error - Invalid Content
                          message lox_openxml_format->get_text( ) type 'S' display like 'E'.
                        catch cx_openxml_not_found into data(lox_openxml_not_found). " Part not found
                          message lox_openxml_not_found->get_text( ) type 'S' display like 'E'.
                      endtry.
                      clear ls_sheet_info.
                    endloop.

                  endif.
                endif.
              catch cx_openxml_format into lox_openxml_format.      " Packaging Error - Invalid Content
                message lox_openxml_format->get_text( ) type 'S' display like 'E'.
              catch cx_openxml_not_allowed into data(lox_openxml_not_allowed). " Action is not allowed
                message lox_openxml_not_allowed->get_text( ) type 'S' display like 'E'.
            endtry.
          else.
            message 'No data uploaded' type 'S' display like 'E'.
          endif.
        catch cx_openxml_not_found.
      endtry.
    endif.

    delete et_sheets where num_rows < 0 or num_cols < 0.  " delete empty content of standard excel sheet - Sheet1
  endmethod.

  method itab_to_excel_ehfnd.
    constants: lc_sheet_name type string value 'SAP_DATA'.
    field-symbols: <lt_data> type any table.
    clear rv_data.
    if it_multi_sheet_data is not initial.
      data(lo_xlsx) = cl_ehfnd_xlsx=>get_instance( ).
      if lo_xlsx is bound.
        data(lo_doc) = lo_xlsx->create_doc( ).
        if lo_doc is bound.
          loop at it_multi_sheet_data into data(ls_sheet).
            try.
                try.
*                  data(lo_sheet) = lo_doc->get_sheet_by_id( iv_sheet_id = conv #( gc_s_sheet_number-default ) ).
                    data(lo_sheet) = lo_doc->add_new_sheet(
                                       exporting
                                         iv_sheet_name = cond #( when ls_sheet-name is not initial
                                                                 then ls_sheet-name
                                                                 else |{ lc_sheet_name }{ sy-tabix }| ) ).
                  catch cx_openxml_format into data(lox_openxml_format).    " Packaging Error - Invalid Content
                    message lox_openxml_format->get_text( ) type 'S' display like 'E'.
                  catch cx_openxml_not_found into data(lox_openxml_not_found). " Part not found
                    message lox_openxml_not_found->get_text( ) type 'S' display like 'E'.
                endtry.
                if lo_sheet is bound.
*                lo_sheet->change_sheet_name( exporting iv_new_name = iv_sheet_name ). " not implemented by SAP
                  if ls_sheet-data is bound.
                    unassign <lt_data>.
                    assign ls_sheet-data->* to <lt_data>.
                    if <lt_data> is assigned.
                      data(lt_fields) = ls_sheet-fields.
                      if lt_fields is not initial or ls_sheet-header = abap_true.
                        data(lo_table) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <lt_data> ) ).
                        if lo_table is bound.
                          data(lo_struct) = cast cl_abap_structdescr( lo_table->get_table_line_type( ) ).
                          if lo_struct is bound.
                            data(lt_comp) = lo_struct->components.
*                            if line_exists( lt_comp[ as_include = abap_true ] ).
*                              loop at lt_comp into data(ls_comp) where as_include = abap_true.
*                                try.
*                                    lo_struct ?= ls_comp-type.
*                                    append lines of lo_struct->components to lt_comp.
*                                  catch cx_sy_move_cast_error ##no_handler.
*                                endtry.
*                                clear ls_comp.
*                              endloop.
*                              delete lt_comp where as_include = abap_true.
*                            endif.
                          endif.

                          if lt_fields is not initial.
                            loop at lt_fields assigning field-symbol(<ls_fields>) where name is initial.
                              try.
                                  <ls_fields>-name = lt_comp[ sy-tabix ]-name.
                                catch cx_sy_itab_line_not_found ##no_handler.
                              endtry.
                            endloop.
                          else.
                            lt_fields = corresponding #( lt_comp ).
                          endif.
                        endif.
                      endif.
                      if lt_fields is not initial.
                        do lines( lt_fields ) times.
                          try.
                              lo_sheet->set_cell_content(
                                  exporting
                                    iv_row          = conv #( 1 )
                                    iv_column       = conv #( sy-index )
                                    iv_value        = lt_fields[ sy-index ]-name
                                    iv_force_string = abap_true ).
                            catch cx_sy_itab_line_not_found ##no_handler.
                          endtry.
                        enddo.
                      endif.
                      loop at <lt_data> assigning field-symbol(<ls_data>).
                        data(lv_row) = sy-tabix + cond i( when lt_fields is not initial then 1 ).
                        do.
                          data(lv_col) = sy-index.
                          assign component lv_col of structure <ls_data> to field-symbol(<lv>).
                          if <lv> is assigned.
                            lo_sheet->set_cell_content(
                              exporting
                                iv_row          = lv_row
                                iv_column       = lv_col
                                iv_value        = <lv>
                                iv_input_type   = lo_xlsx->get_input_type_for_abap_type(
                                                    exporting
                                                      io_abap_type_desc = cl_abap_typedescr=>describe_by_data(
                                                                            exporting
                                                                              p_data = <lv> ) )
                                iv_force_string = conv #( ls_sheet-string ) ).
                          else.
                            exit.
                          endif.
                          unassign <lv>.
                          clear lv_col.
                        enddo.
                        clear lv_row.
                      endloop.
                    endif.
                  endif.
                endif.
              catch cx_openxml_format into lox_openxml_format.      " Packaging Error - Invalid Content
                message lox_openxml_format->get_text( ) type 'S' display like 'E'.
              catch cx_openxml_not_allowed into data(lox_openxml_not_allowed). " Action is not allowed
                message lox_openxml_not_allowed->get_text( ) type 'S' display like 'E'.
            endtry.
            clear:
              ls_sheet,
              lo_sheet,
              lt_fields,
              lo_table,
              lo_struct,
              lt_comp.

            unassign:
              <ls_fields>,
              <ls_data>.
          endloop.
          try.
              rv_data = lo_doc->save( ).
            catch cx_openxml_format into lox_openxml_format.      " Packaging Error - Invalid Content
              message lox_openxml_format->get_text( ) type 'S' display like 'E'.
            catch cx_openxml_not_found into lox_openxml_not_found.   " Part not found
              message lox_openxml_not_found->get_text( ) type 'S' display like 'E'.
            catch cx_openxml_not_allowed into lox_openxml_not_allowed. " Action is not allowed
              message lox_openxml_not_allowed->get_text( ) type 'S' display like 'E'.
          endtry.
        endif.
      endif.
    endif.
  endmethod.

  method sheets_to_itabs.
    clear rt_excel_itab.
    if it_sheets is not initial.
      loop at it_sheets into data(ls_sheet).
        append initial line to rt_excel_itab assigning field-symbol(<ls_excel_itab>).
        if <ls_excel_itab> is assigned.
          <ls_excel_itab> = value #( sheet_name = ls_sheet-sheetname
                                     rows       = ls_sheet-num_rows
                                     cols       = ls_sheet-num_cols
                                     data_tab   = sheet_to_itab( exporting it_sheet = ls_sheet-spreadsheet ) ).
        endif.
        clear ls_sheet.
        unassign <ls_excel_itab>.
      endloop.
    endif.
  endmethod.

  method sheet_to_itab.
    data: lr_data type ref to data.
    field-symbols: <lt_data> type any table.

    unassign <lt_data>.
    clear rr_data.
    if it_sheet is not initial.
      rr_data = generate_table_type(
                  exporting
                    it_fields = value #( for ls in it_sheet where ( row = 1 ) ( ls-value ) ) ).

      if rr_data is bound.
        assign rr_data->* to <lt_data>.
        if <lt_data> is assigned.
          create data lr_data like line of <lt_data>.
          loop at it_sheet into data(ls_sheet) where row gt 1.  " exclude header row
            at new row.
              if lr_data is bound.
                assign lr_data->* to field-symbol(<ls_data>).
              endif.
              if <ls_data> is assigned.
                clear <ls_data>.
              endif.
            endat.
            assign component ls_sheet-column of structure <ls_data> to field-symbol(<lv>).
            if sy-subrc = 0 and <lv> is assigned. " Incase there are more xls columns than fields
*--------------------------------------------------------------------*
              " Date field conversion to internal format
              if cl_abap_typedescr=>describe_by_data( exporting p_data = <lv> )->type_kind = cl_abap_typedescr=>typekind_date.
                excel_date_to_sap_date( changing cv_excel_date = ls_sheet-value ).
              endif.

              " Time field conversion to internal format
              if cl_abap_typedescr=>describe_by_data( exporting p_data = <lv> )->type_kind = cl_abap_typedescr=>typekind_time.
                excel_time_to_sap_time( changing cv_excel_time = ls_sheet-value ).
              endif.
*--------------------------------------------------------------------*
              move ls_sheet-value to <lv>.
            endif.
            clear ls_sheet.
            unassign <lv>.

            at end of row.
              insert <ls_data> into table <lt_data>.
              unassign <ls_data>.
            endat.
          endloop.
        endif.
      endif.
    endif.
  endmethod.

  method generate_table_type.
    clear rr_data.
    if it_fields is not initial.
      try.
          data(lo_struct_descr) = cl_abap_structdescr=>create(
                                    exporting
                                      p_components = value #( for ls in it_fields
                                                                ( name = ls
                                                                  type = cast cl_abap_elemdescr(
                                                                           cl_abap_elemdescr=>describe_by_name( 'EHFND_STRING' ) ) ) ) ).

          if lo_struct_descr is bound.
            try.
                data(lo_table_descr) = cl_abap_tabledescr=>create(
                                         exporting
                                           p_line_type  = lo_struct_descr ).

                if lo_table_descr is bound.
                  create data rr_data type handle lo_table_descr.
                endif.
              catch cx_sy_table_creation. " Exception when Creating a Table Type
            endtry.
          endif.
        catch cx_sy_struct_creation. " Exception when creating a structure description
        catch cx_sy_move_cast_error.
      endtry.
    endif.
  endmethod.

  method itab_to_excel_soi.
    constants: lc_app_name      type c length 50 value 'EXCEL_CONTAINER',
               lc_doc_title     type c length 50 value 'SAP_EXCEL',
               lc_range_header  type c length 50 value 'RANGE_HEADER',
               lc_range_data    type c length 50 value 'RANGE_DATA',
               lc_string_format type c length 1 value '@',
               lc_sheet_name    type string value 'SAP_DATA'.

    field-symbols: <lt_data> type any table.

    data(lt_data) = value solix_tab( ).

    clear rv_data.
    if it_multi_sheet_data is not initial.
      c_oi_container_control_creator=>get_container_control(
        importing
          control = data(lo_container_control)  " Container Control
          error   = data(lo_error)              " Error Object
          retcode = data(lv_retcode) ).         " Error Code (Obsolete)

      lo_container_control->init_control(
        exporting
          inplace_enabled          = abap_true
          r3_application_name      = lc_app_name                                  " Application Name
          parent                   = cl_gui_container=>default_screen             " Parent Container
*          no_flush                 = abap_true                                    " Do not flush automation queue (yet!)
        importing
          error                    = lo_error                                     " Error Object
          retcode                  = lv_retcode                                   " Error Value: Obsolete
        exceptions
          javabeannotsupported     = 1                                            " JavaBeans are not supported
          others                   = 2 ).

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_dummy).
      endif.

      if lv_retcode = c_oi_errors=>ret_ok.
        lo_container_control->get_document_proxy(
          exporting
            document_type      = soi_doctype_excel_sheet
*            no_flush           = abap_true                          " Do not flush automation queue (yet!)
          importing
            document_proxy     = data(lo_document_proxy)
            error              = lo_error
            retcode            = lv_retcode ).

        if lo_document_proxy is bound.
          lo_document_proxy->create_document(
            exporting
              document_title   = lc_doc_title
              open_inplace     = abap_true
*              no_flush         = abap_true                          " Do not flush automation queue (yet!)
            importing
              error            = lo_error
              retcode          = lv_retcode ).

          if lv_retcode = c_oi_errors=>ret_ok.
            lo_document_proxy->has_spreadsheet_interface(
*              exporting
*                no_flush     = abap_true                " Do not flush automation queue (yet!)
              importing
                error        = lo_error                 " Error?
                is_available = data(lv_is_available)    " Is Interface Available
                retcode      = lv_retcode ).            " Text of Error
          endif.

          if lv_is_available = 1.
            lo_document_proxy->get_spreadsheet_interface(
*              exporting
*                no_flush        = abap_true                 " Do not flush automation queue (yet!)
              importing
                error           = lo_error                  " Error?
                sheet_interface = data(lo_sheet_interface)  " Reference to Spreadsheet Interface
                retcode         = lv_retcode ).             " Text of Error
          endif.

          if lo_sheet_interface is bound.
            loop at it_multi_sheet_data into data(ls_sheet).

              unassign <lt_data>.
              assign ls_sheet-data->* to <lt_data>.
              if <lt_data> is assigned.
                data(lv_rows) = lines( <lt_data> ).

                lo_sheet_interface->add_sheet(
                  exporting
                    name     = condense( cond char100( when ls_sheet-name is not initial
                                                       then ls_sheet-name
                                                       else |{ lc_sheet_name }{ sy-tabix }| ) )    " Name of Worksheet
*                    no_flush = abap_true      " Do not flush automation queue (yet!)
                  importing
                    error    = lo_error       " Error?
                    retcode  = lv_retcode ).  " Text of Error

                if lv_retcode = c_oi_errors=>ret_ok.
                  data(lt_fields) = ls_sheet-fields.

                  data(lo_struct_descr) = cast cl_abap_structdescr(
                                            cast cl_abap_tabledescr(
                                              cl_abap_typedescr=>describe_by_data(
                                                exporting
                                                  p_data = <lt_data> ) )->get_table_line_type( ) ).

                  data(lt_components) = cond #( when lo_struct_descr is bound
                                                then lo_struct_descr->components ).

                  if lt_fields is not initial or ls_sheet-header = abap_true.

*                    loop at lt_components into data(ls_component) where as_include = abap_true.
*                      append lines of cast cl_abap_structdescr( ls_component-type )->components to lt_components.
*                      clear ls_component.
*                    endloop.
*
*                    delete lt_components where as_include = abap_true.

                    if lt_fields is not initial.
                      loop at lt_fields assigning field-symbol(<ls_field>) where name is initial.
                        try.
                            <ls_field>-name = lt_components[ sy-tabix ]-name.
                          catch cx_sy_itab_line_not_found ##no_handler.
                        endtry.
                      endloop.
                    else.
                      lt_fields = corresponding #( lt_components ).
                    endif.

                    data(lv_columns) = lines( lt_components ).

                    " add header row
                    lo_sheet_interface->insert_range_dim(
                      exporting
                        name      = lc_range_header     " Name of Range
                        left      = 1                   " Top Left-Hand Corner    ; top-left cell and fill entire first row = header
                        top       = 1                   " Top Left-Hand Corner
                        rows      = 1                   " Rows
                        columns   = lv_columns          " Columns
*                        no_flush  = abap_true           " Do not flush automation queue (yet!)
                      importing
                        error     = lo_error            " Errors?
                        retcode   = lv_retcode ).       " text

                    lo_sheet_interface->set_format_string(
                      exporting
                        rangename    = lc_range_header    " Name of Range
                        formatstring = lc_string_format   " Format String
*                        no_flush     = abap_true          " Do not flush automation queue (yet!)
                      importing
                        error        = lo_error           " Error?
                        retcode      = lv_retcode ).      " Text of Error

                    data(lt_content) = value soi_generic_table( for ls_field in lt_fields index into lv_index
                                                                ( row     = 1
                                                                  column  = lv_index
                                                                  value   = ls_field-name ) ).


                    data(lt_ranges) = value soi_range_list( ( name    = lc_range_header
                                                              rows    = 1
                                                              columns = lv_columns ) ).

                    lo_sheet_interface->set_ranges_data(
                      exporting
                        ranges    = lt_ranges       " Ranges in the Sheet
                        contents  = lt_content      " Contents of the Ranges
*                        no_flush  = abap_true       " Do not flush automation queue (yet!)
                      importing
                        error     = lo_error        " Errors?
                        retcode   = lv_retcode ).   " Text of the Error

                    lo_sheet_interface->set_color(
                      exporting
                        rangename = lc_range_header " Name of Range
                        front     = 2               " Foreground color
                        back      = 16              " Background color
*                        no_flush  = abap_true       " Do not flush automation queue (yet!)
                      importing
                        error     = lo_error        " Error?
                        retcode   = lv_retcode ).   " Text of Error
                  endif.

                  " main table data
                  data(lt_fields_fcat) = cond #( when lo_struct_descr is bound
                                                 then build_fields_fcat(
                                                        exporting
                                                          io_struct_descr = lo_struct_descr ) ).

                  lv_columns = cond #( when lv_columns is initial then lines( lt_fields_fcat ) else lv_columns ).

                  constants: lc_max_rows type i value 9999.

                  data(lv_top)   = value i( ).
                  data(lv_count) = conv i( round( val = ( lv_rows / lc_max_rows )
                                                  dec = 0
                                                  mode = cl_abap_math=>round_up ) ).
                  data(lv_start) = 1.
                  data(lv_end)   = 0.

                  do lv_count times.

                    lv_end = cond #( when ( lv_end + lc_max_rows ) <= lines( <lt_data> ) then ( lv_end + lc_max_rows ) else lines( <lt_data> ) ).

                    lv_top = cond #( when lt_fields is not initial or ls_sheet-header = abap_true
                                     then ( lv_start + 1 ) else lv_start ). " Top Left-Hand Corner

                    lv_rows = ( lv_end - lv_start ) + 1.

                    lo_sheet_interface->insert_range_dim(
                      exporting
                        name      = lc_range_data     " Name of Range
                        left      = 1                 " Top Left-Hand Corner
                        top       = lv_top
                        rows      = lv_rows           " Rows
                        columns   = lv_columns        " Columns
*                        no_flush  = abap_true         " Do not flush automation queue (yet!)
                      importing
                        error     = lo_error          " Errors?
                        retcode   = lv_retcode ).     " text

                    if ls_sheet-string = abap_true.
                      lo_sheet_interface->set_format_string(
                        exporting
                          rangename    = lc_range_data      " Name of Range
                          formatstring = lc_string_format   " Format String
*                          no_flush     = abap_true          " Do not flush automation queue (yet!)
                        importing
                          error        = lo_error           " Error?
                          retcode      = lv_retcode ).      " Text of Error
                    endif.

                    clear:
                     lt_content,
                     lt_ranges.

                    data(lv_row) = value i( ).
                    clear lv_row.
                    loop at <lt_data> assigning field-symbol(<ls_data>).
                      if sy-tabix > lv_end.
                        exit.
                      endif.
                      if sy-tabix not between lv_start and lv_end.
                        continue.
                      endif.
                      lv_row = lv_row + 1.
                      do lines( lt_components ) times.
                        data(lv_column) = sy-index.
                        append initial line to lt_content assigning field-symbol(<ls_content>).
                        if <ls_content> is assigned.
                          <ls_content>-row = condense( |{ lv_row }| ).
                          <ls_content>-column = condense( |{ lv_column }| ).
                          assign component sy-index of structure <ls_data> to field-symbol(<lv>).
                          if <lv> is assigned.
                            <ls_content>-value = condense( conv char256( <lv> ) ).
                          endif.
                        endif.
                        unassign:
                          <ls_content>,
                          <lv>.
                        clear lv_column.
                      enddo.
                    endloop.

                    lt_ranges = value soi_range_list( ( name    = lc_range_data
                                                        rows    = lv_rows
                                                        columns = lv_columns ) ).

                    lo_sheet_interface->set_ranges_data(
                      exporting
                        ranges    = lt_ranges       " Ranges in the Sheet
                        contents  = lt_content      " Contents of the Ranges
*                        no_flush  = abap_true       " Do not flush automation queue (yet!)
                      importing
                        error     = lo_error        " Errors?
                        retcode   = lv_retcode ).   " Text of the Error

                    lv_start = lv_end + 1.
                  enddo.

                  " returns message SOFFICEINTEGRATION 205 many a times - above method is an alternative that works
*                  lo_sheet_interface->insert_one_table(
*                    exporting
*                      data_table   = <lt_data>        " Data
*                      fields_table = lt_fields_fcat   " The Fields of the Table
*                      rangename    = lc_range_data    " The Name of the Range
*                      wholetable   = abap_true        " Inserts Whole Table
*                    importing
*                      error        = lo_error         " Errors?
*                      retcode      = lv_retcode ).    " Text of the Error

                  lo_sheet_interface->fit_widest(
                    exporting
                      name     = space
*                      no_flush = abap_true      " Do not flush automation queue (yet!)
                    importing
                      error    = lo_error       " Error?
                      retcode  = lv_retcode ).  " text
                endif.
              endif.

              clear:
                ls_sheet,
                lo_error,
                lv_retcode,
                lt_fields,
                lo_struct_descr,
                lt_components,
                lv_rows,
                lv_columns,
                lt_content,
                lt_ranges,
                lt_fields_fcat,
                lv_top,
                lv_count,
                lv_start,
                lv_end.

              unassign <ls_field>.
            endloop.

            " delete default sheet?
*            lo_sheet_interface->delete_sheet(
*              exporting
*                name     = 'Sheet1'       " Name of Worksheet
*              importing
*                error    = lo_error       " Error?
*                retcode  = lv_retcode ).  " Text of Error

            " temp save without user interaction
            data(lv_temp_file) = get_temp_file_path(
                                   exporting
                                     iv_front_end = abap_true
                                     iv_extension = zcl_helper=>gc_extension-xls ).

            lo_document_proxy->save_as(
              exporting
                file_name   = |{ lv_temp_file }|
*                no_flush    = abap_false            " now flush the queue
              importing
                error       = lo_error
                retcode     = lv_retcode ).

            if lv_temp_file is not initial and zcl_helper=>check_file_exists( exporting iv_filepath = lv_temp_file ).
              data(lv_file_length) = value i( ).
              try.
                  lt_data = zcl_helper=>read_file_from_path(
                              exporting
                                iv_filepath    = lv_temp_file    " File path on frontend/app server
                              importing
                                ev_file_length = lv_file_length ). " Binary file length

                  if lt_data is not initial.
                    rv_data = cl_bcs_convert=>solix_to_xstring(
                                                exporting
                                                  it_solix = lt_data
                                                  iv_size  = lv_file_length ).
                  endif.
                catch zcx_generic into data(lox_generic). " Generic Exception Class
                  message lox_generic type 'S' display like 'E'.
                  return.
              endtry.
            endif.
          endif.

          lo_document_proxy->close_document(
*            exporting
*              no_flush    = abap_true
            importing
              error       = lo_error
              retcode     = lv_retcode ).

          lo_document_proxy->release_document(
*            exporting
*              no_flush = abap_true
            importing
              error    = lo_error
              retcode  = lv_retcode ).
        endif.
      endif.

      lo_container_control->release_all_documents(
*        exporting
*          no_flush = abap_true
        importing
          error    = lo_error
          retcode  = lv_retcode ).

      lo_container_control->destroy_control(
*        exporting
*          no_flush = abap_false
        importing
          error    = lo_error
          retcode  = lv_retcode ).

      delete_temp_file( exporting iv_temp_file = lv_temp_file ).
    endif.
  endmethod.

  method build_fields_fcat.
    clear rt_fields_fcat.

    split io_struct_descr->absolute_name at '=' into data(lv_type) data(lv_tab_name).
    if lv_tab_name is initial.
      lv_tab_name = 'UNKNOWN'.
    endif.

    data(lv_offset) = value i( ).

    loop at io_struct_descr->components into data(ls_component).
      append initial line to rt_fields_fcat assigning field-symbol(<ls_field_fcat>).
      if <ls_field_fcat> is assigned.
        <ls_field_fcat> = corresponding #( ls_component ).

        <ls_field_fcat> = value #( tabname   = lv_tab_name
                                   fieldname = ls_component-name
                                   exid      = ls_component-type_kind
                                   intlength = ls_component-length / cl_abap_char_utilities=>charsize
                                   position  = sy-tabix
                                   offset    = lv_offset ).

        lv_offset = lv_offset + <ls_field_fcat>-intlength.
      endif.

      clear ls_component.
      unassign <ls_field_fcat>.
    endloop.
  endmethod.

  method get_temp_file_path.
    clear rv_temp_file.

    data(lv_front_end)  = iv_front_end.
    data(lv_app_server) = iv_app_server.
    data(lv_extension)  = iv_extension.

    if lv_front_end = abap_false and lv_app_server = abap_false.
      if not cl_demo_sap_gui=>check( ) or sy-batch = abap_true.
        lv_app_server = abap_true.
      else.
        lv_front_end = abap_true.
      endif.
    endif.

    if lv_front_end = abap_true and lv_app_server = abap_true.
      return. " invalid parameter combo
    endif.

    if lv_extension is initial.
      lv_extension = zcl_helper=>gc_extension-bin.
    endif.

    data(lv_temp_dir) = value string( ).

    if lv_front_end = abap_true.
      data(lv_separator) = zcl_helper=>gc_path_sep-windows.
      cl_gui_frontend_services=>get_temp_directory(
        changing
          temp_dir             = lv_temp_dir " Temporary Directory
        exceptions
          cntl_error           = 1        " Control error
          error_no_gui         = 2        " No GUI available
          not_supported_by_gui = 3        " GUI does not support this
          others               = 4 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        cl_gui_cfw=>flush(
          exceptions
            cntl_system_error = 1 " cntl_system_error
            cntl_error        = 2 " cntl_error
            others            = 3 ).
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.
    endif.

    if lv_app_server = abap_true.
      lv_separator = zcl_helper=>gc_path_sep-unix.
      data(lv_dir_path) = value dirname_al11( ).
      data(lv_program)  = conv authb-program( sy-cprog ).
      data(lv_function) = conv authb-cfuncname( 'C_SAPGPARAM' ).

      " check authority before calling C function to avoid runtime error
      call function 'AUTHORITY_CHECK_C_FUNCTION'
        exporting
          program          = lv_program
          activity         = sabc_act_call
          function         = lv_function
        exceptions
          no_authority     = 1
          activity_unknown = 2
          others           = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_msg).

        return.
      endif.

      " C function call to retrieve app server path using paramter(alias)
      call 'C_SAPGPARAM' id 'NAME'  field 'DIR_TRANS'
                         id 'VALUE' field lv_dir_path.

      if lv_dir_path is not initial.
        lv_temp_dir = lv_dir_path.
      else.
        return.
      endif.
    endif.

    data(lv_temp_file) = condense( |{ condense( lv_temp_dir ) }{ lv_separator }{ sy-cprog }_{ sy-datum }_{ sy-uzeit }.{ lv_extension }| ).
    rv_temp_file = lv_temp_file.
  endmethod.

  method delete_temp_file.
    data(lv_temp_file) = iv_temp_file.
    if lv_temp_file is not initial and zcl_helper=>check_file_exists( exporting iv_filepath = lv_temp_file ).
      data(lv_rc) = value i( ).
      cl_gui_frontend_services=>file_delete(
        exporting
          filename             = lv_temp_file " Name of the file to be deleted
        changing
          rc                   = lv_rc
        exceptions
          file_delete_failed   = 1        " Could not delete file
          cntl_error           = 2        " Control error
          error_no_gui         = 3        " Error: No GUI
          file_not_found       = 4        " File not found
          access_denied        = 5        " Access denied
          unknown_error        = 6        " Unknown error
          not_supported_by_gui = 7        " GUI does not support this
          wrong_parameter      = 8        " Wrong parameter
          others               = 9 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_dummy).
      else.
        cl_gui_cfw=>flush(
          exceptions
            cntl_system_error = 1 " cntl_system_error
            cntl_error        = 2 " cntl_error
            others            = 3 ).
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.
    endif.
  endmethod.

  method validate_file_path.
    clear rv_valid.
    if iv_filepath is not initial.
      data(lv_filepath) = iv_filepath.
      " basic check
      " first check direct existence
      rv_valid = zcl_helper=>check_file_exists( exporting iv_filepath = lv_filepath ).
      if rv_valid = abap_true.
        return.
      endif.

      " setup
      try.
          data(lo_path) = cl_fs_path=>create(
                            exporting
                              name           = lv_filepath
                              path_kind      = cond #( when lv_filepath ca zcl_helper=>gc_path_sep-windows then 'W'
                                                       when lv_filepath ca zcl_helper=>gc_path_sep-unix then 'U' ) ).

          if lo_path is bound.
            data(lv_directory) = lo_path->get_path_component( ).
            data(lv_file_name) = lo_path->get_file_name( ).
            data(lv_extension) = lo_path->get_file_extension( ).
          endif.
        catch cx_smart_path_syntax. " Syntax error in smart path
      endtry.

* ---- Compute filepath type ---- *
      data(lv_file_path_type) = cond #( when lv_filepath ca zcl_helper=>gc_path_sep-windows then zcl_helper=>gc_path_sep-windows
                                        when lv_filepath ca zcl_helper=>gc_path_sep-unix then zcl_helper=>gc_path_sep-unix ).

      case lv_file_path_type.
        when zcl_helper=>gc_path_sep-windows.
          " split the filepath into parts and validate separately
          cl_gui_frontend_services=>directory_exist(
            exporting
              directory            = lv_directory " Directory name
            receiving
              result               = rv_valid    " Result
            exceptions
              cntl_error           = 1         " Control error
              error_no_gui         = 2         " No GUI available
              wrong_parameter      = 3         " Incorrect parameter
              not_supported_by_gui = 4         " GUI does not support this
              others               = 5 ).
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into data(lv_dummy).
          endif.

          if iv_contains_filename = abap_true and ( lv_file_name is initial or lv_extension is initial ).
            rv_valid = abap_false.
          endif.
        when zcl_helper=>gc_path_sep-unix.
          " split the filepath into parts and validate separately
          " get directory listing
          data: lv_err_no  type c length 3,
                lv_err_msg type c length 40.

          call 'C_DIR_READ_FINISH'                  " just to be sure
            id 'ERRNO'  field lv_err_no
            id 'ERRMSG' field lv_err_msg.

          call 'C_DIR_READ_START'
            id 'DIR'    field lv_directory
            id 'FILE'   field '*'
            id 'ERRNO'  field lv_err_no
            id 'ERRMSG' field lv_err_msg.
          if sy-subrc = 0.
            rv_valid = abap_true.
          endif.

          call 'C_DIR_READ_FINISH'
            id 'ERRNO'  field lv_err_no
            id 'ERRMSG' field lv_err_msg.

          constants: lc_logical_filename type filename-fileintern value 'EHS_FTAPPL_2'. " same as that used in CG3Y
* ---- validate physical filename against logical filename ---- *
          call function 'FILE_VALIDATE_NAME'
            exporting
              logical_filename           = lc_logical_filename
            changing
              physical_filename          = lv_filepath
            exceptions
              logical_filename_not_found = 1
              validation_failed          = 2
              others                     = 3.
          if sy-subrc = 0.
            rv_valid = abap_true.
          endif.
        when others.
      endcase.
    endif.
  endmethod.

  method excel_time_to_sap_time.
    data:
      lv_h    type n length 2,
      lv_m    type n length 2,
      lv_s    type n length 2,
      lv_time type sy-uzeit,
      lv_num  type float.

    clear:
      lv_h,
      lv_m,
      lv_s,
      lv_time,
      lv_num.

    if conv string( cv_excel_time ) co '.0123456789'.
      split conv string( cv_excel_time ) at '.' into data(lv_i) data(lv_dec).
      if strlen( lv_dec ) >= 1 and conv i( lv_i ) <= '1'. " this is a decimal time field
        lv_num = cv_excel_time.
        lv_num = lv_num * 24 .
        lv_h = floor( lv_num ).
        lv_num = lv_num - lv_h.
        lv_num = lv_num * 60.
        lv_m = floor( lv_num ).
        lv_num = lv_num - lv_m.
        lv_num = lv_num * 60.
        lv_s = lv_num.
        concatenate lv_h lv_m lv_s into lv_time.

        call function 'TIME_CHECK_PLAUSIBILITY'
          exporting
            time                      = lv_time " Time to be checked
          exceptions
            plausibility_check_failed = 1    " Time is not plausible
            others                    = 2.
        if sy-subrc = 0.
          cv_excel_time = lv_time.
*      write cv_excel_time to cv_excel_time using edit mask '__:__:__'.
        endif.
      endif.
    endif.
  endmethod.

  method excel_date_to_sap_date.
    if strlen( conv string( cv_excel_date ) ) < 8 and cv_excel_date is not initial.
      data(lv_date) = conv sy-datum( '19000101' ).
      data(lv_days) = cv_excel_date - 2.

      lv_date = lv_date + lv_days.

      call function 'DATE_CHECK_PLAUSIBILITY'
        exporting
          date                      = lv_date " Transfer of date to be checked
        exceptions
          plausibility_check_failed = 1    " Date is not plausible
          others                    = 2.
      if sy-subrc = 0.
        cv_excel_date = lv_date.
      endif.
    endif.
  endmethod.
endclass.
