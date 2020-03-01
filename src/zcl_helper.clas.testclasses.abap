*"* use this source file for your ABAP unit test classes

class ltcl_file_io definition for testing
  duration short
  risk level harmless
.
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

    class-data: mv_frontend_filepath   type string,
                mv_app_server_filepath type string,
                lt_data                type solix_tab.

    class-methods: class_setup.
    class-methods: class_teardown.
    methods: setup.
    methods: teardown.

    methods:
      itab_to_excel_xls_frontend    for testing,
      excel_to_itab_xls_frontend    for testing,
      itab_to_excel_xls_app_server  for testing,
      excel_to_itab_xls_app_server  for testing,
      itab_to_excel_xlsx_frontend   for testing,
      excel_to_itab_xlsx_frontend   for testing,
      itab_to_excel_xlsx_app_server for testing,
      excel_to_itab_xlsx_app_server for testing.
endclass.       "ltcl_File_Io


class ltcl_file_io implementation.

  method class_setup.
    clear:
      mv_frontend_filepath,
      mv_app_server_filepath.

    cl_gui_frontend_services=>get_temp_directory(
      changing
        temp_dir             = mv_frontend_filepath " Temporary Directory
      exceptions
        cntl_error           = 1        " Control error
        error_no_gui         = 2        " No GUI available
        not_supported_by_gui = 3        " GUI does not support this
        others               = 4 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endmethod.

  method class_teardown.
  endmethod.

  method setup.
    create object f_cut.
  endmethod.

  method teardown.
  endmethod.

  method itab_to_excel_xls_frontend.
  endmethod.

  method excel_to_itab_xls_frontend.
  endmethod.

  method itab_to_excel_xls_app_server.
  endmethod.

  method excel_to_itab_xls_app_server.
  endmethod.

  method itab_to_excel_xlsx_frontend.
  endmethod.

  method excel_to_itab_xlsx_frontend.
  endmethod.

  method itab_to_excel_xlsx_app_server.
  endmethod.

  method excel_to_itab_xlsx_app_server.
  endmethod.
endclass.
