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
                mv_app_server_filepath type string.

    class-methods: class_setup.
    class-methods: class_teardown.
    methods: setup.
    methods: teardown.
    methods: delete_file_from_app_server for testing.
    methods: excel_to_itab for testing.
    methods: itab_to_excel for testing.
    methods: read_file_from_app_server for testing.
    methods: write_file_to_app_server for testing.
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

  method itab_to_excel.

    data it_itab type table of mara.
    data it_fields type zcl_helper=>tty_fields.
    data iv_insert_header type abap_bool.
    data iv_force_string type abap_bool.
    data iv_file_name type string.
    data iv_sheet_name type string.
    data it_multi_sheet_data type zcl_helper=>tty_sheet.
    data iv_direct_download type abap_bool.
    data iv_app_server_filepath type string.
    data ev_file_length type i.
    data rt_data type solix_tab.

    rt_data = zcl_helper=>itab_to_excel(
*     EXPORTING
*       IT_ITAB = it_Itab
*       IT_FIELDS = it_Fields
*       IV_INSERT_HEADER = iv_Insert_Header
*       IV_FORCE_STRING = iv_Force_String
*       IV_FILE_NAME = iv_File_Name
*       IV_SHEET_NAME = iv_Sheet_Name
*       IT_MULTI_SHEET_DATA = it_Multi_Sheet_Data
*       IV_DIRECT_DOWNLOAD = iv_Direct_Download
*       IV_APP_SERVER_FILEPATH = iv_App_Server_Filepath
*     IMPORTING
*       EV_FILE_LENGTH = ev_File_Length
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = ev_file_length
      exp   = ev_file_length          "<--- please adapt expected value
    " msg   = 'Testing value ev_File_Length'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = rt_data
      exp   = rt_data          "<--- please adapt expected value
    " msg   = 'Testing value rt_Data'
*     level =
    ).
  endmethod.

  method write_file_to_app_server.

    data iv_frontend_filepath type string.
    data iv_overwrite type abap_bool.
    data iv_file_length type i.
    data cv_app_server_filepath type string.
    data ct_data type solix_tab.
    data rv_uploaded type abap_bool.

    rv_uploaded = zcl_helper=>write_file_to_app_server(
*     EXPORTING
*       IV_FRONTEND_FILEPATH = iv_Frontend_Filepath
*       IV_OVERWRITE = iv_Overwrite
*       IV_FILE_LENGTH = iv_File_Length
*     CHANGING
*       CV_APP_SERVER_FILEPATH = cv_App_Server_Filepath
*       CT_DATA = ct_Data
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = cv_app_server_filepath
      exp   = cv_app_server_filepath          "<--- please adapt expected value
    " msg   = 'Testing value cv_App_Server_Filepath'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = ct_data
      exp   = ct_data          "<--- please adapt expected value
    " msg   = 'Testing value ct_Data'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = rv_uploaded
      exp   = rv_uploaded          "<--- please adapt expected value
    " msg   = 'Testing value rv_Uploaded'
*     level =
    ).
  endmethod.

  method read_file_from_app_server.

    data iv_app_server_filepath type string.
    data iv_frontend_filepath type string.
    data iv_download_prompt type abap_bool.
    data ev_file_length type i.
    data rt_data type solix_tab.

    rt_data = zcl_helper=>read_file_from_app_server(
      exporting
        iv_app_server_filepath = iv_app_server_filepath
*       IV_FRONTEND_FILEPATH = iv_Frontend_Filepath
*       IV_DOWNLOAD_PROMPT = iv_Download_Prompt
*     IMPORTING
*       EV_FILE_LENGTH = ev_File_Length
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = ev_file_length
      exp   = ev_file_length          "<--- please adapt expected value
    " msg   = 'Testing value ev_File_Length'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = rt_data
      exp   = rt_data          "<--- please adapt expected value
    " msg   = 'Testing value rt_Data'
*     level =
    ).
  endmethod.

  method excel_to_itab.

    data iv_file type string.
    data iv_no_of_headers type i.
    data iv_check_file_format type abap_bool.
    data iv_move_corresponding type abap_bool.
    data iv_with_conv_exit type abap_bool.
    data iv_sheet_number type i.
    data iv_read_all_sheets type abap_bool.
    data et_excel type zcl_helper=>tty_excel.
    data ct_itab type table of mara.

    zcl_helper=>excel_to_itab(
      exporting
        iv_file = iv_file
*       IV_NO_OF_HEADERS = iv_No_Of_Headers
*       IV_CHECK_FILE_FORMAT = iv_Check_File_Format
*       IV_MOVE_CORRESPONDING = iv_Move_Corresponding
*       IV_WITH_CONV_EXIT = iv_With_Conv_Exit
*       IV_SHEET_NUMBER = iv_Sheet_Number
*       IV_READ_ALL_SHEETS = iv_Read_All_Sheets
*     IMPORTING
*       ET_EXCEL = et_Excel
      changing
        ct_itab = ct_itab ).

    cl_abap_unit_assert=>assert_equals(
      act   = et_excel
      exp   = et_excel          "<--- please adapt expected value
    " msg   = 'Testing value et_Excel'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = ct_itab
      exp   = ct_itab          "<--- please adapt expected value
    " msg   = 'Testing value ct_Itab'
*     level =
    ).
  endmethod.

  method delete_file_from_app_server.

    data iv_app_server_filepath type string.
    data ev_message type string.
    data rv_deleted type abap_bool.

    rv_deleted = zcl_helper=>delete_file_from_app_server(
      exporting
        iv_app_server_filepath = iv_app_server_filepath
*     IMPORTING
*       EV_MESSAGE = ev_Message
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = ev_message
      exp   = ev_message          "<--- please adapt expected value
    " msg   = 'Testing value ev_Message'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = rv_deleted
      exp   = rv_deleted          "<--- please adapt expected value
    " msg   = 'Testing value rv_Deleted'
*     level =
    ).
  endmethod.

endclass.
