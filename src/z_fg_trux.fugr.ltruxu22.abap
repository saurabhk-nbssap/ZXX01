FUNCTION file_read_and_convert_sap_data .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FILENAME) LIKE  FILENAME-FILEINTERN
*"     VALUE(I_SERVERTYP) TYPE  TRUXS_SERVER
*"         DEFAULT C_APPLICATION_SERVER
*"     VALUE(I_FILEFORMAT) TYPE  TRUXS_FILEFORMAT OPTIONAL
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     VALUE(E_BIN_FILELENGTH)
*"  TABLES
*"      I_TAB_RECEIVER OPTIONAL
*"  EXCEPTIONS
*"      FILE_NOT_FOUND
*"      CLOSE_FAILED
*"      AUTHORIZATION_FAILED
*"      OPEN_FAILED
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------

  CONSTANTS:
        c_ole_header  LIKE tfdir-funcname VALUE 'LOAD_',
        c_fm_header  LIKE tfdir-funcname VALUE 'TEXT_CONVERT_',
        c_fm_trailer LIKE tfdir-funcname VALUE '_TO_SAP',
        c_file_type_binary LIKE  rlgrap-filetype VALUE 'BIN',
        c_file_type_xml LIKE  rlgrap-filetype VALUE 'XML',
        c_file_type_ascii  LIKE  rlgrap-filetype VALUE 'ASC'.
  DATA:
        l_type,
        l_component TYPE i,
        l_sys_cp  LIKE tcp00-cpcodepage, "system codepage
        l_lan_cp  LIKE tcp00-cpcodepage, "language codepage
        l_tcp00   LIKE tcp00,
        i_tab_input_data TYPE truxs_t_text_data,
        i_tab_xinput_data TYPE truxs_xml_table,
        s_input_data LIKE LINE OF i_tab_input_data,
        l_fm_name LIKE tfdir-funcname,
        l_tfdir   LIKE tfdir,
        l_text80(80)  TYPE c,
        l_return(40)  TYPE c,
        l_file_format LIKE filename-fileformat,
        l_totalsize   type i,
        l_file_name   LIKE rlgrap-filename.
  data: l_file_name_string type string,
        l_file_format_10 type char10.
  data: l_filename_logical TYPE fileintern.
  FIELD-SYMBOLS: <fs_itab> TYPE table.
  FIELD-SYMBOLS: <fs_struc>.

  data: l_len type i.

  CASE i_fileformat.
    WHEN c_file_type_binary.
      ASSIGN i_tab_receiver[] TO <fs_itab>.
    WHEN c_file_type_xml.
      ASSIGN i_tab_xinput_data[] TO <fs_itab>.
    WHEN OTHERS.
      ASSIGN i_tab_input_data[] TO <fs_itab>.
  ENDCASE.
  ASSIGN LOCAL COPY OF INITIAL LINE OF <fs_itab> TO <fs_struc>.

* note 1509869/1511995
* In order to avoid a second conversion from log.filename to phys.filename
* (this is done in the callers already)
* the FILE_GET_NAME is deactivated
* The parameter I_FILENAME is assumed to be the physical name (should be type as FILENAME-FILEEXTERN...)
* The logical filenames will be derived from SY-CPROG to restrict this function module for
* "known" callers only (SEC reasons)
*
** Get physical file name first
*  CALL FUNCTION 'FILE_GET_NAME'
*    EXPORTING
*      logical_filename = i_filename
*    IMPORTING
*      file_format      = l_file_format
*      file_name        = l_file_name
*    EXCEPTIONS
*      OTHERS           = c_rc4.
*  IF sy-subrc <> c_rc0.

* note 1793250
*
*    IF l_file_format IS INITIAL.
*      l_file_format = c_file_type_ascii.
*    ELSE.
*      IF i_fileformat = c_file_type_xml.
*        l_file_format = c_file_type_binary.
*      ELSE.
*        l_file_format = c_file_type_ascii.
*      ENDIF.
*    ENDIF.

      IF i_fileformat = c_file_type_xml.
        l_file_format = c_file_type_binary.
      ELSE.
        l_file_format = c_file_type_ascii.
    ENDIF.
* note 1793250

    l_file_name   = i_filename.
*  ENDIF.

* Read data from application server
  IF i_servertyp = c_application_server.

*   derive log.filenames
    CASE sy-cprog.
      WHEN 'RFTR_INTF_MAINFLOWS_UPLOAD'.
        l_filename_logical = 'FTRM_FTR_DEALDATA_AMORTIZATION_SCHEDULES_IMPORT'.
      WHEN 'RFTS6510'.
        l_filename_logical = 'FCLM_CM_MEMO_RECORD_IMPORT'.
      WHEN 'RFVOBJ01'.
        l_filename_logical = 'CML_MIGRATION_OBJECTS_PHYSFILE_IN'.
      WHEN 'RFVSIC01'.
        l_filename_logical = 'CML_MIGRATION_COLLATERALS_PHYSFILE_IN'.
      WHEN OTHERS.
        MESSAGE ID 'SG' TYPE 'E' NUMBER '809'
          WITH 'SPACE'
          RAISING authorization_failed.
    ENDCASE.

*   check code page first
    CALL FUNCTION 'SYSTEM_CODEPAGE'
      IMPORTING
        codepage = l_sys_cp.
    SELECT SINGLE * FROM tcp00 INTO l_tcp00 WHERE cpcodepage = l_sys_cp.
    CALL FUNCTION 'SCP_CODEPAGE_FOR_LANGUAGE'
      EXPORTING
        language = sy-langu
      IMPORTING
        codepage = l_lan_cp
      EXCEPTIONS
        OTHERS   = c_rc4.
    IF sy-subrc <> 0.
      l_lan_cp = l_sys_cp.
    ENDIF.

*   note 1509869: File validation
    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        logical_filename  = l_filename_logical
*       any parameters defined for that logical file name
      CHANGING
        physical_filename = l_file_name
      EXCEPTIONS
        OTHERS            = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
      RAISING authorization_failed.
    ENDIF.

*   Handle the file now
    IF l_file_format = c_file_type_ascii.
      OPEN DATASET l_file_name FOR INPUT IN TEXT MODE
                                     encoding Default.
    ELSE.
      OPEN DATASET l_file_name FOR INPUT IN BINARY MODE.
    ENDIF.
    CATCH SYSTEM-EXCEPTIONS file_access_errors = c_rc4.
      IF sy-subrc <> c_rc0.
        MESSAGE e890(ux) WITH l_file_name RAISING open_failed.
      ENDIF.
    ENDCATCH.
    CLEAR sy-subrc.
    WHILE sy-subrc = c_rc0.
      READ DATASET l_file_name INTO <fs_struc> LENGTH l_len.
      CATCH SYSTEM-EXCEPTIONS file_access_errors = c_rc4.
        IF sy-subrc <> c_rc0.
          IF i_fileformat = c_file_type_xml.   "N1804808
            APPEND <fs_struc> TO <fs_itab>.
          ENDIF.
          l_totalsize = l_totalsize + l_len.
          EXIT.
        ENDIF.
      ENDCATCH.
      l_totalsize = l_totalsize + l_len.
      APPEND <fs_struc> TO <fs_itab>.
      IF sy-batch IS INITIAL.
        l_text80 = sy-tabix.
        CONDENSE l_text80 NO-GAPS.
        CONCATENATE text-red l_text80 INTO l_text80
                     SEPARATED BY space.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 0
            text       = l_text80
          EXCEPTIONS
            others     = 0.
      ENDIF.
    ENDWHILE.
    IF i_fileformat <> c_file_type_binary AND
      i_fileformat <> c_file_type_xml.
      IF l_lan_cp <> l_sys_cp.
        data: ld_lan_cp type abap_encod.
        ld_lan_cp = l_lan_cp.
        CALL FUNCTION 'TRANSLATE_CODEPAGE_IN'
          EXPORTING
            CODEPAGE_FROM = ld_lan_cp
          TABLES
            T_DATA        = <fs_itab>
          EXCEPTIONS
            OTHERS        = 0.

      ENDIF.
    ENDIF.

    CLOSE DATASET l_file_name.
    CATCH SYSTEM-EXCEPTIONS file_access_errors = c_rc4.
      IF sy-subrc = c_rc4.
        MESSAGE e891(ux) WITH l_file_name RAISING close_failed.
      ENDIF.
    ENDCATCH.
  ENDIF.

* Upload data from presentation server
  IF i_servertyp = c_presentation_server.
*   CALL FUNCTION 'WS_UPLOAD'
    move l_file_name to l_file_name_string.
    move l_file_format to l_file_format_10.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
      EXPORTING
        filename        = l_file_name_string
        filetype        = l_file_format_10
      IMPORTING
        filelength      = l_totalsize
      CHANGING
        data_tab        = <fs_itab>
      EXCEPTIONS
        file_read_error = c_rc4
        file_open_error = c_rc8
        OTHERS          = c_rc8.

    CASE sy-subrc.
      WHEN c_rc0.
      WHEN c_rc4.
        MESSAGE e890(ux) WITH l_file_name RAISING file_not_found.
      WHEN OTHERS.
        MESSAGE e890(ux) WITH l_file_name RAISING file_not_found.
    ENDCASE.
  ENDIF.
* Upload data from OLE2 server
  IF i_servertyp = c_ole2_server.
    CALL FUNCTION 'WS_QUERY'
      EXPORTING
        query    = 'FL'
        filename = l_file_name
      IMPORTING
        return   = l_return
      EXCEPTIONS
        OTHERS   = c_rc4.
    IF sy-subrc <> c_rc0 OR l_return IS INITIAL OR l_return = '0'.
      MESSAGE e890(ux) WITH l_file_name RAISING file_not_found.
    ENDIF.
  ENDIF.

  CASE i_fileformat.
    WHEN space.
      i_tab_receiver[] = <fs_itab>.
    WHEN c_file_type_binary.
    WHEN c_file_type_ascii.
*     i_tab_receiver[] = i_tab_input_data[].
      CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
        EXPORTING
          i_line_header        = i_line_header
          i_tab_raw_data       = i_tab_input_data[]
          i_filename           = l_file_name
        TABLES
          i_tab_converted_data = i_tab_receiver
        EXCEPTIONS
          OTHERS               = c_rc4.
      IF sy-subrc <> c_rc0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                                           RAISING conversion_failed.
      ENDIF.
    WHEN OTHERS.
      l_fm_name    = c_fm_header.
      l_fm_name+13 = i_fileformat.
      l_fm_name+23 = c_fm_trailer.
      CONDENSE l_fm_name NO-GAPS.
      SELECT SINGLE * FROM tfdir INTO l_tfdir
                                 WHERE funcname = l_fm_name.
      IF sy-subrc = c_rc0.
        CALL FUNCTION l_fm_name
          EXPORTING
            i_fileformat         = i_fileformat
            i_field_seperator    = i_field_seperator
            i_line_header        = i_line_header
            i_tab_raw_data       = <fs_itab>
            i_filename           = l_file_name
            i_totalsize          = l_totalsize
          TABLES
            i_tab_converted_data = i_tab_receiver
          EXCEPTIONS
            OTHERS               = c_rc4.
      ELSE.
        MESSAGE e046(fl) WITH l_fm_name RAISING conversion_failed.
      ENDIF.
      IF sy-subrc <> c_rc0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                                           RAISING conversion_failed.
      ENDIF.
  ENDCASE.
  e_bin_filelength = l_totalsize.

ENDFUNCTION.
