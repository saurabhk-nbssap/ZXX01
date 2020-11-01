*"* use this source file for your ABAP unit test classes

class ltcl_file_io definition for testing
  duration medium
  risk level harmless.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ltcl_File_Io
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_HELPER
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  private section.
    data:
      f_cut type ref to zcl_helper.  "class under test

    class-data:
      mo_helper                   type ref to lcl_helper,
      mv_frontend_filepath_xls    type string,
      mv_app_server_filepath_xls  type string,
      mv_frontend_filepath_xlsx   type string,
      mv_app_server_filepath_xlsx type string,
      mt_data                     type solix_tab,
      mv_file_length              type i,
      mt_sheet_in                 type zcl_helper=>tty_sheet,
      mt_sheet_out                type zcl_helper=>tty_excel,
      mt_mara                     type sorted table of mara with unique key primary_key components matnr,
      mt_makt                     type sorted table of makt with unique key primary_key components matnr,
      mt_marc                     type sorted table of marc with unique key primary_key components matnr werks,
      mt_mard                     type sorted table of mard with unique key primary_key components matnr werks lgort,
      mt_mvke                     type sorted table of mvke with unique key primary_key components matnr vkorg vtweg,
      mt_mbew                     type sorted table of mbew with unique key primary_key components matnr bwkey bwtar,
      mt_mlan                     type sorted table of mlan with unique key primary_key components matnr,
      mt_marm                     type sorted table of marm with unique key primary_key components matnr meinh.

    class-methods:
      class_setup,
      get_default_filepaths,
      data_selection,
      class_teardown.

    methods:
      setup,
      teardown.

    methods:
      xls_frontend    for testing,
      xls_app_server  for testing,
      xlsx_frontend   for testing,
      xlsx_app_server for testing.
endclass.       "ltcl_File_Io


class ltcl_file_io implementation.

  method class_setup.
    mo_helper = new #( ).

    get_default_filepaths( ).
    data_selection( ).
  endmethod.

  method get_default_filepaths.
    mv_frontend_filepath_xls =
      mo_helper->get_temp_file_path(
        exporting
          iv_front_end = abap_true
          iv_extension = zcl_helper=>gc_extension-xls ).

    mv_app_server_filepath_xls =
      mo_helper->get_temp_file_path(
        exporting
          iv_app_server = abap_true
          iv_extension  = zcl_helper=>gc_extension-xls ).

    mv_frontend_filepath_xlsx =
      mo_helper->get_temp_file_path(
        exporting
          iv_front_end = abap_true
          iv_extension = zcl_helper=>gc_extension-xlsx ).

    mv_app_server_filepath_xlsx =
      mo_helper->get_temp_file_path(
        exporting
          iv_app_server = abap_true
          iv_extension  = zcl_helper=>gc_extension-xlsx ).
  endmethod.

  method data_selection.
    " class selections
    select *
      from mara
      using client '300'
      where mtart = 'ZFGM'
      into table @mt_mara
      up to 10 rows.

    if mt_mara is not initial.
      select *
        from makt
        using client '300'
        for all entries in @mt_mara
        where matnr = @mt_mara-matnr
        and   spras = @sy-langu
        into table @mt_makt.

      select *
        from marc
        using client '300'
        for all entries in @mt_mara
        where matnr = @mt_mara-matnr
        into table @mt_marc.

      if mt_marc is not initial.
        select *
          from mard
          using client '300'
          for all entries in @mt_marc
          where matnr = @mt_marc-matnr
          and   werks = @mt_marc-werks
          into table @mt_mard.

        select *
          from mbew
          using client '300'
          for all entries in @mt_marc
          where matnr = @mt_marc-matnr
          and   bwkey = @mt_marc-werks
          into table @mt_mbew.
      endif.

      select *
        from mvke
        using client '300'
        for all entries in @mt_mara
        where matnr = @mt_mara-matnr
        into table @mt_mvke.

      select *
        from mlan
        using client '300'
        for all entries in @mt_mara
        where matnr = @mt_mara-matnr
        and   aland = @sy-zonlo+0(2)
        into table @mt_mlan.

      select *
        from marm
        using client '300'
        for all entries in @mt_mara
        where matnr = @mt_mara-matnr
        into table @mt_marm.
    endif.

    data(lv_failed) =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_mara                            " Actual value
          msg              = |MARA selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_makt                            " Actual value
          msg              = |MAKT selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_marc                            " Actual value
          msg              = |MARC selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_mard                            " Actual value
          msg              = |MARD selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_mvke                            " Actual value
          msg              = |MVKE selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_mbew                            " Actual value
          msg              = |MBEW selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_mlan                            " Actual value
          msg              = |MLAN selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_marm                            " Actual value
          msg              = |MARM selection failed|            " Description
          level            = if_aunit_constants=>tolerable      " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    mt_sheet_in = value #(
                           ( name   = 'Basic'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_mara ) )
                           ( name   = 'Description'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_makt ) )
                           ( name   = 'Plant'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_marc ) )
                           ( name   = 'Storage Location'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_mard ) )
                           ( name   = 'Sales'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_mvke ) )
                           ( name   = 'Valuation'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_mbew ) )
                           ( name   = 'Tax classification'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_mlan ) )
                           ( name   = 'UoM'
                             string = abap_true
                             header = abap_true
                             data   = ref #( mt_marm ) )
                          ).
  endmethod.

  method class_teardown.

    data(lv_deleted) =
      zcl_helper=>delete_file( exporting iv_filepath = mv_frontend_filepath_xls ). " Path of file on frontend/app server

    data(lv_failed) =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_deleted                           " Actual value
          msg              = |Frontend xls deletion failed|       " Description
          level            = if_aunit_constants=>critical         " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).            " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear:
      lv_deleted,
      lv_failed.

    lv_deleted =
      zcl_helper=>delete_file( exporting iv_filepath = mv_app_server_filepath_xls ). " Path of file on frontend/app server

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_deleted                           " Actual value
          msg              = |Frontend xlsx deletion failed|      " Description
          level            = if_aunit_constants=>critical         " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).            " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear:
      lv_deleted,
      lv_failed.

    lv_deleted =
      zcl_helper=>delete_file( exporting iv_filepath = mv_frontend_filepath_xlsx ). " Path of file on frontend/app server

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_deleted                           " Actual value
          msg              = |App server xls deletion failed|     " Description
          level            = if_aunit_constants=>critical         " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).            " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    clear:
      lv_deleted,
      lv_failed.

    lv_deleted =
      zcl_helper=>delete_file( exporting iv_filepath = mv_app_server_filepath_xlsx ). " Path of file on frontend/app server

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_deleted                           " Actual value
          msg              = |App server xlsx deletion failed|    " Description
          level            = if_aunit_constants=>critical         " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).            " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    free:
      mo_helper,
      mv_frontend_filepath_xls,
      mv_app_server_filepath_xls,
      mv_frontend_filepath_xlsx,
      mv_app_server_filepath_xlsx,
      mt_data,
      mv_file_length,
      mt_sheet_in,
      mt_sheet_out,
      mt_mara,
      mt_makt,
      mt_marc,
      mt_mard,
      mt_mvke,
      mt_mbew,
      mt_mlan,
      mt_marm,
      lv_deleted,
      lv_failed.
  endmethod.

  method setup.
    f_cut = new #( ).
  endmethod.

  method teardown.
    free f_cut.
  endmethod.

  method xls_frontend.

*--------------------------------------------------------------------*
* Itab to excel - xls frontend
*--------------------------------------------------------------------*

    data(lv_failed) =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mv_frontend_filepath_xls                           " Actual value
          msg              = |Frontend xls path could not be determined|        " Description
          level            = if_aunit_constants=>critical                       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).                          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(mt_data) = zcl_helper=>itab_to_excel(                " Generated excel data in binary format
      exporting
        " <<<< For easy conversion of single table to excel >>>
*        it_itab             = it_itab                        " Single internal table to be converted
*        it_fields           = it_fields                      " Column names
*        iv_insert_header    = abap_true                      " Add header line
*        iv_force_string     = iv_force_string                " Convert all values to string
*        iv_sheet_name       = 'SAP_DATA'                     " Excel sheet name
        " <<<< End single table fields >>>>
        iv_file_path        = mv_frontend_filepath_xls        " Filepath on frontend or app server to download to...
        it_multi_sheet_data = mt_sheet_in                     " Multiple Sheets With Data
      importing
        ev_file_length      = mv_file_length ).               " Binary file length

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_data                            " Actual value
          msg              = |ITAB to XLS conversion failed|    " Description
          level            = if_aunit_constants=>critical       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).      " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(lv_exists) = zcl_helper=>check_file_exists( exporting iv_filepath = mv_frontend_filepath_xls ). " File path to check

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_exists                                      " Actual value
          msg              = |XLS file could not downloaded to frontend|    " Description
          level            = if_aunit_constants=>critical                   " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).                  " Alter control flow/ quit test (NO, >METHOD<, CLASS)

*--------------------------------------------------------------------*
* Excel to itab - xls frontend
*--------------------------------------------------------------------*

    data(lt_mard) = mt_mard.
    clear lt_mard.

    zcl_helper=>excel_to_itab(
      exporting
        iv_file               = mv_frontend_filepath_xls          " Local file for upload/download
*        iv_no_of_headers      = 0                                " Number of header rows in file
        iv_check_file_format  = abap_true                         " Check file field sequence
        iv_move_corresponding = abap_true                         " Maps excel columns to itab fields
*        iv_with_conv_exit     = abap_false                       " Call FORMAT_EXCEL_TO_BAPI internally
        iv_sheet_number       = zcl_helper=>gc_s_sheet_number-_4  " Excel sheet to read
        iv_read_all_sheets    = abap_true                         " Read data from all sheets of excel
      importing
        et_excel              = mt_sheet_out                      " Data from all excel sheets
      changing
        ct_itab               = lt_mard ).                        " Internal table

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lines( mt_sheet_out )
          exp      = lines( mt_sheet_in )
          msg      = |XLS to ITAB: multi sheet read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lt_mard
          exp      = mt_mard
          msg      = |XLS to ITAB: read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).
  endmethod.

  method xls_app_server.

*--------------------------------------------------------------------*
* Itab to excel - xls app server
*--------------------------------------------------------------------*

    data(lv_failed) =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mv_app_server_filepath_xls                         " Actual value
          msg              = |App server xls path could not be determined|       " Description
          level            = if_aunit_constants=>critical                       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).                          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(mt_data) = zcl_helper=>itab_to_excel(                " Generated excel data in binary format
      exporting
        " <<<< For easy conversion of single table to excel >>>
*        it_itab             = it_itab                        " Single internal table to be converted
*        it_fields           = it_fields                      " Column names
*        iv_insert_header    = abap_true                      " Add header line
*        iv_force_string     = iv_force_string                " Convert all values to string
*        iv_sheet_name       = 'SAP_DATA'                     " Excel sheet name
        " <<<< End single table fields >>>>
        iv_file_path        = mv_app_server_filepath_xls      " Filepath on frontend or app server to download to...
        it_multi_sheet_data = mt_sheet_in                     " Multiple Sheets With Data
      importing
        ev_file_length      = mv_file_length ).               " Binary file length

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_data                            " Actual value
          msg              = |ITAB to XLS conversion failed|    " Description
          level            = if_aunit_constants=>critical       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).      " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(lv_exists) = zcl_helper=>check_file_exists( exporting iv_filepath = mv_app_server_filepath_xls ). " File path to check

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_exists                                      " Actual value
          msg              = |XLS file could not uploaded to app server|    " Description
          level            = if_aunit_constants=>critical                   " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).                  " Alter control flow/ quit test (NO, >METHOD<, CLASS)

*--------------------------------------------------------------------*
* Excel to itab - xls app server
*--------------------------------------------------------------------*

    data(lt_mard) = mt_mard.
    clear lt_mard.

    zcl_helper=>excel_to_itab(
      exporting
        iv_file               = mv_app_server_filepath_xls        " Local file for upload/download
*        iv_no_of_headers      = 0                                " Number of header rows in file
        iv_check_file_format  = abap_true                         " Check file field sequence
        iv_move_corresponding = abap_true                         " Maps excel columns to itab fields
*        iv_with_conv_exit     = abap_false                       " Call FORMAT_EXCEL_TO_BAPI internally
        iv_sheet_number       = zcl_helper=>gc_s_sheet_number-_4  " Excel sheet to read
        iv_read_all_sheets    = abap_true                         " Read data from all sheets of excel
      importing
        et_excel              = mt_sheet_out                      " Data from all excel sheets
      changing
        ct_itab               = lt_mard ).                        " Internal table

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lines( mt_sheet_out )
          exp      = lines( mt_sheet_in )
          msg      = |XLS to ITAB: multi sheet read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lt_mard
          exp      = mt_mard
          msg      = |XLS to ITAB: read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).
  endmethod.

  method xlsx_frontend.

*--------------------------------------------------------------------*
* Itab to excel - xlsx frontend
*--------------------------------------------------------------------*

    data(lv_failed) =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mv_frontend_filepath_xlsx                          " Actual value
          msg              = |Frontend xlsx path could not be determined|       " Description
          level            = if_aunit_constants=>critical                       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).                          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(mt_data) = zcl_helper=>itab_to_excel(                " Generated excel data in binary format
      exporting
        " <<<< For easy conversion of single table to excel >>>
*        it_itab             = it_itab                        " Single internal table to be converted
*        it_fields           = it_fields                      " Column names
*        iv_insert_header    = abap_true                      " Add header line
*        iv_force_string     = iv_force_string                " Convert all values to string
*        iv_sheet_name       = 'SAP_DATA'                     " Excel sheet name
        " <<<< End single table fields >>>>
        iv_file_path        = mv_frontend_filepath_xlsx       " Filepath on frontend or app server to download to...
        it_multi_sheet_data = mt_sheet_in                     " Multiple Sheets With Data
      importing
        ev_file_length      = mv_file_length ).               " Binary file length

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_data                            " Actual value
          msg              = |ITAB to XLSX conversion failed|   " Description
          level            = if_aunit_constants=>critical       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).      " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(lv_exists) = zcl_helper=>check_file_exists( exporting iv_filepath = mv_frontend_filepath_xlsx ). " File path to check

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_exists                                      " Actual value
          msg              = |XLSX file could not downloaded to frontend|   " Description
          level            = if_aunit_constants=>critical                   " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).                  " Alter control flow/ quit test (NO, >METHOD<, CLASS)

*--------------------------------------------------------------------*
* Excel to itab - xlsx frontend
*--------------------------------------------------------------------*

    data(lt_mard) = mt_mard.
    clear lt_mard.

    zcl_helper=>excel_to_itab(
      exporting
        iv_file               = mv_frontend_filepath_xlsx         " Local file for upload/download
*        iv_no_of_headers      = 0                                " Number of header rows in file
        iv_check_file_format  = abap_true                         " Check file field sequence
        iv_move_corresponding = abap_true                         " Maps excel columns to itab fields
*        iv_with_conv_exit     = abap_false                       " Call FORMAT_EXCEL_TO_BAPI internally
        iv_sheet_number       = zcl_helper=>gc_s_sheet_number-_4  " Excel sheet to read
        iv_read_all_sheets    = abap_true                         " Read data from all sheets of excel
      importing
        et_excel              = mt_sheet_out                      " Data from all excel sheets
      changing
        ct_itab               = lt_mard ).                        " Internal table

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lines( mt_sheet_out )
          exp      = lines( mt_sheet_in )
          msg      = |XLSX to ITAB: multi sheet read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lt_mard
          exp      = mt_mard
          msg      = |XLSX to ITAB: read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).
  endmethod.

  method xlsx_app_server.

*--------------------------------------------------------------------*
* Itab to excel - xlsx app server
*--------------------------------------------------------------------*

    data(lv_failed) =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mv_app_server_filepath_xls                         " Actual value
          msg              = |App server xlsx path could not be determined|     " Description
          level            = if_aunit_constants=>critical                       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>no ).                          " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(mt_data) = zcl_helper=>itab_to_excel(                " Generated excel data in binary format
      exporting
        " <<<< For easy conversion of single table to excel >>>
*        it_itab             = it_itab                        " Single internal table to be converted
*        it_fields           = it_fields                      " Column names
*        iv_insert_header    = abap_true                      " Add header line
*        iv_force_string     = iv_force_string                " Convert all values to string
*        iv_sheet_name       = 'SAP_DATA'                     " Excel sheet name
        " <<<< End single table fields >>>>
        iv_file_path        = mv_app_server_filepath_xlsx     " Filepath on frontend or app server to download to...
        it_multi_sheet_data = mt_sheet_in                     " Multiple Sheets With Data
      importing
        ev_file_length      = mv_file_length ).               " Binary file length

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_not_initial(
        exporting
          act              = mt_data                            " Actual value
          msg              = |ITAB to XLSX conversion failed|   " Description
          level            = if_aunit_constants=>critical       " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).      " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    data(lv_exists) = zcl_helper=>check_file_exists( exporting iv_filepath = mv_app_server_filepath_xlsx ). " File path to check

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_true(
        exporting
          act              = lv_exists                                      " Actual value
          msg              = |XLSX file could not uploaded to app server|   " Description
          level            = if_aunit_constants=>critical                   " Severity (TOLERABLE, >CRITICAL<, FATAL)
          quit             = if_aunit_constants=>method ).                  " Alter control flow/ quit test (NO, >METHOD<, CLASS)

*--------------------------------------------------------------------*
* Excel to itab - xlsx app server
*--------------------------------------------------------------------*

    data(lt_mard) = mt_mard.
    clear lt_mard.

    zcl_helper=>excel_to_itab(
      exporting
        iv_file               = mv_app_server_filepath_xlsx       " Local file for upload/download
*        iv_no_of_headers      = 0                                " Number of header rows in file
        iv_check_file_format  = abap_true                         " Check file field sequence
        iv_move_corresponding = abap_true                         " Maps excel columns to itab fields
*        iv_with_conv_exit     = abap_false                       " Call FORMAT_EXCEL_TO_BAPI internally
        iv_sheet_number       = zcl_helper=>gc_s_sheet_number-_4  " Excel sheet to read
        iv_read_all_sheets    = abap_true                         " Read data from all sheets of excel
      importing
        et_excel              = mt_sheet_out                      " Data from all excel sheets
      changing
        ct_itab               = lt_mard ).                        " Internal table

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lines( mt_sheet_out )
          exp      = lines( mt_sheet_in )
          msg      = |XLSX to ITAB: multi sheet read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).

    clear lv_failed.

    lv_failed =
      cl_abap_unit_assert=>assert_equals(
        exporting
          act      = lt_mard
          exp      = mt_mard
          msg      = |XLSX to ITAB: read failed|
          level    = if_aunit_constants=>critical
          quit     = if_aunit_constants=>method ).
  endmethod.
endclass.
