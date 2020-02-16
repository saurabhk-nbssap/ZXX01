FUNCTION sap_data_convert_write_file.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FILENAME) LIKE  FILENAME-FILEINTERN
*"     VALUE(I_SERVERTYP) TYPE  TRUXS_SERVER
*"         DEFAULT C_APPLICATION_SERVER
*"     VALUE(I_FILEFORMAT) TYPE  TRUXS_FILEFORMAT OPTIONAL
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_APPL_KEEP) TYPE  CHAR01 DEFAULT SPACE
*"     VALUE(I_BIN_FILESIZE) DEFAULT SPACE
*"     VALUE(I_XML_DOC_NAME) TYPE  CHAR30 OPTIONAL
*"  TABLES
*"      I_TAB_SENDER TYPE  STANDARD TABLE OPTIONAL
*"  EXCEPTIONS
*"      OPEN_FAILED
*"      CLOSE_FAILED
*"      AUTHORIZATION_FAILED
*"      WRITE_FAILED
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------
CONSTANTS:
        c_ole_header  LIKE tfdir-funcname VALUE 'SAVE_SAP_TO_',
        c_fm_header  LIKE tfdir-funcname  VALUE 'SAP_CONVERT_TO_',
        c_fm_trailer LIKE tfdir-funcname VALUE '_FORMAT',
        c_file_type_binary LIKE  rlgrap-filetype VALUE 'BIN',
        c_file_type_xml    LIKE  rlgrap-filetype VALUE 'XML',
        c_file_type_ascii  LIKE  rlgrap-filetype VALUE 'ASC'.
  DATA:
        l_iref_pixml TYPE REF TO if_ixml,
        l_iref_pstreamfactory TYPE REF TO if_ixml_stream_factory,
        l_iref_postream TYPE REF TO if_ixml_ostream,
        l_data    TYPE REF TO data,
        l_sys_cp  LIKE tcp00-cpcodepage, "system codepage
        l_lan_cp  LIKE tcp00-cpcodepage, "language codepage
        l_tcp00   LIKE tcp00,
        i_tab_output_data  TYPE truxs_t_text_data,
        i_tab_xml_output  TYPE truxs_xml_table,
        s_output_data LIKE LINE OF i_tab_output_data,
        l_text80(80)  TYPE c,
        l_fm_name LIKE tfdir-funcname,
        l_tfdir   LIKE tfdir,
        l_total_size TYPE i,
        l_file_format LIKE filename-fileformat,
        l_file_name   LIKE rlgrap-filename.
  data: l_file_name_string type string,
        l_file_format_10 type char10.
  data: l_filename_logical TYPE fileintern.
  FIELD-SYMBOLS: <fs_itab> TYPE table.
  FIELD-SYMBOLS: <fs_struc>.

  DESCRIBE TABLE i_tab_sender LINES sy-tabix.
  IF i_fileformat <> c_file_type_xml.
    CHECK sy-tabix <> 0.
  ENDIF.

* N1511995
* Logical file names are no longer supported for security reasons
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
*    l_file_format = c_file_type_ascii.
*    l_file_name   = i_filename.
*  ENDIF.

  l_file_name   = i_filename.
  l_file_format = c_file_type_ascii.

  CASE i_fileformat.
    WHEN space.
      i_tab_output_data[] = i_tab_sender[].
      ASSIGN i_tab_output_data[] TO <fs_itab>.
    WHEN c_file_type_binary.
      l_file_format = c_file_type_binary.
      ASSIGN i_tab_sender[] TO <fs_itab>.
      ASSIGN LOCAL COPY OF INITIAL LINE OF <fs_itab> TO <fs_struc>.
    WHEN c_file_type_ascii.
      i_tab_output_data[] = i_tab_sender[].
      ASSIGN i_tab_output_data[] TO <fs_itab>.
    WHEN OTHERS.
      l_fm_name    = c_fm_header.
      l_fm_name+15 = i_fileformat.
      l_fm_name+23 = c_fm_trailer.
      CONDENSE l_fm_name NO-GAPS.
      SELECT SINGLE * FROM tfdir INTO l_tfdir
                                 WHERE funcname = l_fm_name.
      IF sy-subrc = c_rc0.
        IF i_fileformat <> c_file_type_xml.
          ASSIGN i_tab_output_data[] TO <fs_itab>.
        ELSE.
          ASSIGN i_tab_xml_output[] TO <fs_itab>.
          L_File_Format = c_file_type_binary.
        ENDIF.
        CALL FUNCTION l_fm_name
          EXPORTING
            i_field_seperator    = i_field_seperator
            i_line_header        = i_line_header
            i_filename           = l_file_name
            i_appl_keep          = i_appl_keep
            i_xml_doc_name       = i_xml_doc_name
          IMPORTING
            pe_bin_filesize      = l_total_size
          TABLES
            i_tab_sap_data       = i_tab_sender
          CHANGING
            i_tab_converted_data = <fs_itab>
          EXCEPTIONS
            OTHERS               = c_rc4.
        IF sy-subrc <> c_rc0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                                        RAISING conversion_failed.
        ENDIF.
      ELSE.
        MESSAGE e046(fl) WITH l_fm_name RAISING conversion_failed.
      ENDIF.
  ENDCASE.

* Download data via OLE2
  CHECK i_servertyp <> c_ole2_server.

* Write data to application server
  IF i_servertyp = c_application_server.

*   derive log.filenames
    CASE sy-cprog.
      WHEN 'RFTS6510_CREATE_STRUCTURE'.
        l_filename_logical = 'FCLM_CM_MEMO_RECORD_EXPORT'.
      WHEN 'RFVOBJ01'.
        l_filename_logical = 'CML_MIGRATION_OBJECTS_PHYSFILE_OUT'.
      WHEN 'RFVOBJ01_CREATE_STRUCTURE'.
        l_filename_logical = 'CML_MIGRATION_OBJECTS_PHYSFILE_OUT'.
      WHEN 'RFVSIC01'.
        l_filename_logical = 'CML_MIGRATION_COLLATERALS_PHYSFILE_OUT'.
      WHEN 'RFVSIC01_CREATE_STRUCTURE'.
        l_filename_logical = 'CML_MIGRATION_COLLATERALS_PHYSFILE_OUT'.
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

*   N1511995: File validation
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
    LOOP AT <fs_itab> ASSIGNING <fs_struc>.
      AT FIRST.
        IF l_file_format = c_file_type_ascii.
          OPEN DATASET l_file_name FOR OUTPUT IN TEXT MODE
                                          encoding default.
        ELSE.
          OPEN DATASET l_file_name FOR OUTPUT IN BINARY MODE.
        ENDIF.
        CATCH SYSTEM-EXCEPTIONS file_access_errors = c_rc4.
          IF sy-subrc <> c_rc0.
            MESSAGE e890(ux) WITH l_file_name RAISING open_failed.
          ENDIF.
        ENDCATCH.
      ENDAT.
*     transfer data to app server
      IF l_lan_cp <> l_sys_cp AND
         l_file_format = c_file_type_ascii.
      ENDIF.
      TRANSFER <fs_struc> TO l_file_name .
*      TRANSFER s_output_data TO l_file_name .
      CATCH SYSTEM-EXCEPTIONS file_access_errors = c_rc4.
        IF sy-subrc <> c_rc0.
          MESSAGE e892(ux) WITH l_file_name RAISING write_failed.
        ENDIF.
      ENDCATCH.
      IF sy-batch IS INITIAL.
        l_text80 = sy-tabix.
        CONDENSE l_text80 NO-GAPS.
        CONCATENATE text-wri l_text80 INTO l_text80
                     SEPARATED BY space.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 0
            text       = l_text80.
      ENDIF.
      AT LAST.
        CLOSE DATASET l_file_name.
        CATCH SYSTEM-EXCEPTIONS file_access_errors = c_rc4.
          IF sy-subrc <> c_rc0.
            MESSAGE e891(ux) WITH l_file_name RAISING close_failed.
          ENDIF.
        ENDCATCH.
      ENDAT.
    ENDLOOP.
    IF i_fileformat <> c_file_type_binary AND
      i_fileformat <> c_file_type_xml.
      IF l_lan_cp <> l_sys_cp.
        data: ld_lan_cp type abap_encod.
        write l_lan_cp to ld_lan_cp.
        CALL FUNCTION 'TRANSLATE_CODEPAGE_OUT'
          EXPORTING
            CODEPAGE_FROM = ld_lan_cp
          TABLES
            T_DATA        = <fs_itab>
          EXCEPTIONS
            OTHERS        = 0.

      ENDIF.
    ENDIF.

  ENDIF.

* Download data to presentation server
  IF i_servertyp = c_presentation_server.
    IF l_total_size IS INITIAL.
      l_total_size = i_bin_filesize.
    ENDIF.
*   CALL FUNCTION 'WS_DOWNLOAD'
    move l_file_name to l_file_name_string.
    move l_file_format to l_file_format_10.
    call method cl_gui_frontend_services=>gui_download
            EXPORTING
                  filename = l_file_name_string
                  filetype = l_file_format_10
                  bin_filesize = l_total_size
             changing
                  data_tab = <fs_itab>
*              data_tab = i_tab_output_data
             EXCEPTIONS
                  OTHERS   = c_rc4.
    CASE sy-subrc.
      WHEN c_rc0.
      WHEN OTHERS.
        MESSAGE e892(ux) WITH l_file_name RAISING write_failed.
    ENDCASE.
    IF NOT i_appl_keep IS INITIAL.
      CALL FUNCTION 'SAP_STARTS_EXCEL'
        EXPORTING
          i_filename = l_file_name
        EXCEPTIONS
          OTHERS     = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                RAISING open_failed.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
