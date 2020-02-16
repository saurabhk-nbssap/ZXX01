FUNCTION text_convert_tex_to_sap.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_TAB_RAW_DATA) TYPE  TRUXS_T_TEXT_DATA
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME OPTIONAL
*"  TABLES
*"      I_TAB_CONVERTED_DATA TYPE  STANDARD TABLE
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------
  FIELD-SYMBOLS: <f_source>, <f_target>.

* note 1945460
  DATA: l_oref_descr_struc TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <fs_components_struc>
                  LIKE LINE OF l_oref_descr_struc->components.

  DATA:
        l_percentage(4)  TYPE c,
        l_text80(80),
        l_text6(6),
        l_itab_entries LIKE sy-tabix,
        l_tabix LIKE sy-tabix,
        l_struc_raw_data LIKE LINE OF i_tab_raw_data,
        l_start_string LIKE sy-fdpos,
        l_token_next LIKE sy-fdpos,
        l_end_string LIKE sy-fdpos,
        l_eol_string LIKE sy-fdpos,
        l_len_string(6) TYPE n,
        l_field_type,
        l_field_decimals,
        l_struc_index LIKE sy-index.

  CLEAR: i_tab_converted_data.
  REFRESH: i_tab_converted_data.

* note 1945460
  l_oref_descr_struc ?=
    cl_abap_typedescr=>describe_by_data( i_tab_converted_data ).

  DESCRIBE FIELD l_struc_raw_data LENGTH l_eol_string
                                    in character mode.
  DESCRIBE TABLE i_tab_raw_data LINES l_itab_entries.
  LOOP AT i_tab_raw_data INTO l_struc_raw_data.
    AT FIRST.
      CHECK i_line_header IS INITIAL.
    ENDAT.
    CHECK NOT l_struc_raw_data IS INITIAL.
    l_tabix = sy-tabix.
    CLEAR l_struc_index.
    IF NOT i_field_seperator IS INITIAL.
      l_start_string = 1.
    ELSE.
      CLEAR l_start_string.
    ENDIF.
    IF sy-batch IS INITIAL.
      l_percentage = l_tabix / l_itab_entries * 100.
      l_text6 = l_tabix.
      CONDENSE l_text6 NO-GAPS.
      l_text80 = text-kon.
      REPLACE '&&&&&&' WITH l_text6 INTO l_text80.
      l_text6 = l_itab_entries.
      REPLACE '&&&&&&' WITH l_text6 INTO l_text80.
      CONDENSE l_text80.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_percentage
          text       = l_text80.
    ENDIF.
    DO.
      l_token_next = 2.
      l_struc_index = l_struc_index + 1.
      IF NOT i_field_seperator IS INITIAL.
        SEARCH l_struc_raw_data FOR i_field_seperator STARTING AT
                                                    l_start_string
                                                    ENDING AT
                                                    l_eol_string.
        IF sy-subrc <> c_rc0.
          IF l_start_string < l_eol_string.
            sy-fdpos = l_eol_string - l_start_string + 1.
            CLEAR sy-subrc.
          ELSE.
            EXIT.
          ENDIF.
        ENDIF.
        IF sy-subrc = c_rc0.
          l_end_string = l_start_string + sy-fdpos - 1.
          l_len_string = l_end_string - l_start_string + 1.
*         Check leading control character
          IF l_len_string >= c_rc4.
            l_start_string = l_start_string - 1.
            ASSIGN l_struc_raw_data+l_start_string(3)
                           TO <f_source>.
            l_start_string = l_start_string + 1.
            IF sy-subrc = c_rc0 AND <f_source> = '"""'.
              l_start_string = l_start_string + 3.
              l_end_string = l_end_string - 1.
              l_len_string = l_end_string - l_start_string + 1.
              l_token_next = l_token_next + 1.
            ENDIF.
          ENDIF.
          IF l_len_string > c_rc0.
            l_start_string = l_start_string - 1.
            ASSIGN l_struc_raw_data+l_start_string(l_len_string)
                           TO <f_source>.
          ELSE.
*           if l_start_string = 1.
*             l_struc_index = l_struc_index - 1.
*           endif.
            l_start_string = l_start_string + l_token_next - 1.
          ENDIF.
        ENDIF.
      ELSE.
        ASSIGN COMPONENT l_struc_index OF
               STRUCTURE i_tab_converted_data TO <f_target>.
        IF sy-subrc = c_rc0.

*         note 1945460 <
          READ TABLE l_oref_descr_struc->components
           INDEX l_struc_index
            ASSIGNING <fs_components_struc>.

          IF <fs_components_struc>-type_kind = 'P'.
            l_len_string = <fs_components_struc>-length +
                           <fs_components_struc>-decimals + 1. "N1951546
          ELSE.
            DESCRIBE FIELD <f_target> LENGTH l_len_string
              IN CHARACTER MODE.
          ENDIF.
*         note 1945460 >

          ASSIGN l_struc_raw_data+l_start_string(l_len_string)
               TO <f_source>.
          l_end_string = l_len_string + l_start_string - 1.
          l_token_next = 1.
        ENDIF.
      ENDIF.
      IF sy-subrc = c_rc0.
        ASSIGN COMPONENT l_struc_index OF
               STRUCTURE i_tab_converted_data TO <f_target>.
      ENDIF.
      IF sy-subrc = c_rc0.
*>>>>> Begin of Modification <<<<<<<<<<<<<<<<<<<<<<< MurawskiW / 588278
        CLEAR <f_target>.
        CHECK l_len_string > c_rc0.
*>>>>> End   of Modification <<<<<<<<<<<<<<<<<<<<<<< MurawskiW / 588278
          PERFORM input_data2sap_data USING <f_source>
                                      CHANGING <f_target> sy-subrc.
          IF sy-subrc <> c_rc0.
            DESCRIBE FIELD <f_target> TYPE l_field_type.
          ENDIF.
          CASE sy-subrc.
            WHEN 0.
            WHEN 4.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                      RAISING conversion_failed.

            WHEN 8.
              MESSAGE ID c_ux TYPE c_error NUMBER c_899
                      WITH l_field_type  <f_source>
                           l_struc_index l_tabix
                      RAISING conversion_failed.
          ENDCASE.
          l_start_string = l_end_string + l_token_next.
*>>>>> Lines Deleted <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MurawskiW / 588278
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    IF NOT i_tab_converted_data IS INITIAL.
      APPEND i_tab_converted_data.
    ENDIF.
  ENDLOOP.

* correct the decimals
  PERFORM correct_decimals_for_current TABLES i_tab_converted_data.

ENDFUNCTION.
