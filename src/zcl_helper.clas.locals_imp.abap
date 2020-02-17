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
    constants: gc_esc type c length 1 value '"'.

    methods:
      excel_to_itab_mass_spreadsheet
        importing
          value(iv_filename) type if_mass_spreadsheet_types=>file_name
          value(iv_sheet_no) type i default 1
        returning
          value(rt_excel)    like gt_excel,

      excel_to_itab_ehfnd
        importing
          value(iv_filename) type string
          value(iv_sheet_no) type i default 1
        returning
          value(rt_excel)    like gt_excel,

      itab_to_excel_ehfnd
        importing
          value(it_multi_sheet_data) type zcl_helper=>tty_sheet
        returning
          value(rv_data)             type xstring,

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

      separated_to_intern_convert
        importing
          value(i_separator) type char1
        changing
          value(i_tab)       type ty_t_sender
          value(i_intern)    type ty_t_itab,

      line_to_cell_separat
        importing
          value(i_row)       type sy-tabix
          value(ch_cell_col) type kcd_ex_col
          value(i_separator) type char1
          value(i_fdpos)     type sy-fdpos
        changing
          value(i_intern)    type ty_t_itab
          value(i_line)      type ty_s_senderline,

      line_to_cell_esc_sep
        importing
          value(i_separator)    type char1
        changing
          value(i_string)       type ty_s_senderline
          value(i_sic_int)      type i
          value(i_intern_value) type ty_d_itabvalue.

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

    data: excel_tab    type  ty_t_sender,
          ld_separator type  c length 1,
          application  type  ole2_object,
          workbook     type  ole2_object,
          range        type  ole2_object,
          worksheet    type  ole2_object,
          h_cell       type  ole2_object,
          h_cell1      type  ole2_object,
          ld_rc        type  i,
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
        try.
            data(lo_path) = cl_fs_path=>create(
                              name           = conv #( iv_filename )
                              path_kind      = cl_fs_path=>path_kind_smart ).

            if lo_path is bound.
              case lo_path->separator_actual.
                when '\'. " windows
                  lo_imp_excel->set_file( exporting iv_file = conv #( iv_filename ) ). "usual filepath, gotten by cl_gui_frontend_services=>file_open_dialog
                when '/'. " unix
                  try.
                      data(lv_temp_dir) = value string( ).
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
                        else.
                          data(lv_temp_file) = |{ lv_temp_dir }\\sap_data_{ sy-datum }_{ sy-uzeit }.xls|.
                        endif.
                      endif.

                      zcl_helper=>read_file_from_app_server(
                        exporting
                          iv_app_server_filepath = conv #( iv_filename ) " File path on app server(AL11)
                          iv_frontend_filepath = conv #( lv_temp_file ) ).

                      lo_imp_excel->set_file( exporting iv_file = conv #( lv_temp_file ) ).

                    catch zcx_generic. " Generic Exception Class
                  endtry.
                when others.
              endcase.
            endif.
          catch cx_smart_path_syntax. " Syntax error in smart path
        endtry.
      endif.

      try.
          data(lo_excel) = cast cl_mass_spreadsheet( lo_imp_excel->if_mass_spreadsheet_import~import(
                                                       exporting io_imp_conf = cl_mass_spsh_imp_conf_excel=>create_default_config( ) ) ).

          if lo_excel is not bound.
            message 'File open error' type 'E' display like 'I'.
          endif.

          lo_excel->if_mass_spreadsheet~get_data(
            importing
              et_data = data(lt_data) ).

          try.
              data(lt_excel) = lt_data[ iv_sheet_no ]-spreadsheet.
              rt_excel = corresponding #( lt_excel mapping row = row col = column value = value ).
            catch cx_sy_itab_line_not_found ##no_handler.
          endtry.
        catch cx_mass_spreadsheet into data(lox_mass_spreadsheet).
          message lox_mass_spreadsheet->get_longtext( ) type 'E' display like 'I'.
      endtry.

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
    endif.
  endmethod.

  method excel_to_itab_ehfnd.
    clear rt_excel.

    if iv_filename is not initial.
      try.
          try.
              data(lo_path) = cl_fs_path=>create(
                                name           = conv #( iv_filename )
                                path_kind      = cl_fs_path=>path_kind_smart ).

              if lo_path is bound.
                case lo_path->separator_actual.
                  when '\'. " windows
                    data(lv_data_xstr) = cl_ehfnd_file_util=>upload_file( exporting iv_file_name = conv #( iv_filename ) ).
                  when '/'. " unix
                    try.
                        data(lv_file_size) = value i( ).
                        data(lt_data) = zcl_helper=>read_file_from_app_server(
                                          exporting
                                            iv_app_server_filepath = iv_filename " File path on app server(AL11)
                                          importing
                                            ev_file_length         = lv_file_size ).         " Binary file length

                        if lt_data is not initial.
                          lv_data_xstr = cl_bcs_convert=>solix_to_xstring(
                                           exporting
                                             it_solix = lt_data
                                             iv_size  = lv_file_size ).
                        endif.
                      catch zcx_generic. " Generic Exception Class
                    endtry.
                  when others.
                endcase.
              endif.
            catch cx_smart_path_syntax. " Syntax error in smart path
          endtry.

          data(lo_xlsx) = cl_ehfnd_xlsx=>get_instance( ).
          if lo_xlsx is bound and lv_data_xstr is not initial.
            try.
                data(lo_doc) = lo_xlsx->load_doc( exporting iv_file_data = lv_data_xstr ).
                if lo_doc is bound.
                  data(lt_sheet_info) = lo_doc->get_sheets( ).
                  if line_exists( lt_sheet_info[ sheet_id = iv_sheet_no ] ).
                    try.
                        data(lo_sheet) = lo_doc->get_sheet_by_id( exporting iv_sheet_id = iv_sheet_no ).
                        if lo_sheet is bound.
*                          if lo_sheet->has_cell_content( exporting iv_column = 1 iv_row = 1 ).
                          data(lv_column_count) = lo_sheet->get_last_column_number_in_row( exporting iv_row = 1 ).
                          data(lv_row_count) = lo_sheet->get_last_row_number( ).
                          if lv_column_count is not initial and lv_row_count is not initial.
                            do lv_row_count times.
                              data(lv_row_index) = sy-index.
                              do lv_column_count times.
                                append initial line to rt_excel assigning field-symbol(<ls_excel>).
                                if <ls_excel> is assigned.
                                  data(lv_column_index) = sy-index.
                                  <ls_excel>-row = lv_row_index.
                                  <ls_excel>-col = lv_column_index.
                                  <ls_excel>-value = conv #( lo_sheet->get_cell_content(
                                                               exporting
                                                                 iv_row = lv_row_index
                                                                 iv_column = lv_column_index ) ).
                                endif.
                                clear lv_column_index.
                              enddo.
                              clear lv_row_index.
                            enddo.
                          endif.
*                          endif.
                        endif.
                      catch cx_openxml_format into data(lox_openxml_format).    " Packaging Error - Invalid Content
                      catch cx_openxml_not_found into data(lox_openxml_not_found). " Part not found
                    endtry.
                  endif.
                endif.
              catch cx_openxml_format into lox_openxml_format.      " Packaging Error - Invalid Content
              catch cx_openxml_not_allowed into data(lox_openxml_not_allowed). " Action is not allowed
            endtry.
          endif.
        catch cx_openxml_not_found.
      endtry.
    endif.
  endmethod.

  method itab_to_excel_ehfnd.
    constants: lc_sheet_name type string value 'SAP_DATA'.
    field-symbols: <lt_data> type standard table.
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
                  catch cx_openxml_format.    " Packaging Error - Invalid Content
                  catch cx_openxml_not_found. " Part not found
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
                            data(lt_comp) = lo_struct->get_components( ).
                            if line_exists( lt_comp[ as_include = abap_true ] ).
                              loop at lt_comp into data(ls_comp) where as_include = abap_true.
                                try.
                                    lo_struct ?= ls_comp-type.
                                    append lines of lo_struct->get_components( ) to lt_comp.
                                  catch cx_sy_move_cast_error ##no_handler.
                                endtry.
                                clear ls_comp.
                              endloop.
                              delete lt_comp where as_include = abap_true.
                            endif.
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
              catch cx_openxml_format.      " Packaging Error - Invalid Content
              catch cx_openxml_not_allowed. " Action is not allowed
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
            catch cx_openxml_format.      " Packaging Error - Invalid Content
            catch cx_openxml_not_found.   " Part not found
            catch cx_openxml_not_allowed. " Action is not allowed
          endtry.
        endif.
      endif.
    endif.
  endmethod.
endclass.
