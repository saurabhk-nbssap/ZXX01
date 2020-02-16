function create_xls_header_from_ddic.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FILEFORMAT) TYPE  TRUXS_FILEFORMAT DEFAULT 'XLS'
*"     VALUE(I_FIELDNAME) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FIELDTEXT) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_DATATYPE) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_INTTYPE) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LENG) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_DECIMALS) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_TABNAME) LIKE  DCOBJDEF-NAME
*"     VALUE(I_LANGU) LIKE  T002-SPRAS DEFAULT SY-LANGU
*"  TABLES
*"      T_HEADER TYPE  TABLE OPTIONAL
*"  EXCEPTIONS
*"      TABLE_NOT_FOUND
*"----------------------------------------------------------------------
  field-symbols: <f_source>.
  data:
        l_tabix like sy-tabix,
        l_str_len type i,
        l_col_len type i,
        l_numc_2_char(12),
        l_type_i  type i,
        l_tabname like dcobjdef-name,
        i_dfies_tab like dfies occurs 0,
        s_dfies_struc like dfies.

  data: f_type.      "field type wg. Unicode

  call function 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = i_tabname
      langu     = i_langu
    TABLES
      dfies_tab = i_dfies_tab
    EXCEPTIONS
      others    = 4.
  if sy-subrc <> 0.
    message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            raising table_not_found.
  endif.

  clear t_header.
  loop at i_dfies_tab into s_dfies_struc.
    l_tabix = sy-tabix.
    if i_fileformat <> 'ASC'.
      assign component l_tabix of structure t_header to <f_source>.
      if sy-subrc = 0.
        describe field <f_source> type f_type.
        case f_type.
          when 'C'.
            describe field <f_source> length l_col_len
                               in character mode.
          when others.
            describe field <f_source> length l_col_len
                               in byte mode.
        endcase.
        if not i_fieldname is initial.
          <f_source> = s_dfies_struc-fieldname.
        endif.
        if not i_fieldtext is initial.
          if not s_dfies_struc-fieldtext is initial.
            <f_source> = s_dfies_struc-fieldtext.
          else.
            <f_source> = s_dfies_struc-fieldname.
          endif.
        endif.
        if not ( i_datatype is initial and
                 i_inttype  is initial and
                 i_leng     is initial and
                 i_decimals is initial ).
          concatenate <f_source> '(' into <f_source> separated by space.
        endif.
        l_str_len = strlen( <f_source> ).
        if l_str_len < l_col_len.
          if not i_datatype is initial and
             not s_dfies_struc-datatype is initial.
          concatenate <f_source> s_dfies_struc-datatype into <f_source>
                           separated by space.
          endif.
          l_str_len = strlen( <f_source> ).
          if l_str_len < l_col_len.
            if not i_inttype is initial and
               not s_dfies_struc-inttype is initial.
           concatenate <f_source> s_dfies_struc-inttype into <f_source>
                                separated by space.
            endif.
            l_str_len = strlen( <f_source> ).
            if l_str_len < l_col_len.
              if not i_leng  is initial and
                 not s_dfies_struc-leng is initial.
                l_type_i = s_dfies_struc-leng.
                l_numc_2_char = l_type_i.
                condense l_numc_2_char no-gaps.
                concatenate <f_source> l_numc_2_char into <f_source>
                           separated by space.
              endif.
              l_str_len = strlen( <f_source> ).
              if l_str_len < l_col_len.
                if not i_decimals is initial and
                   not s_dfies_struc-decimals is initial.
                  l_type_i = s_dfies_struc-decimals.
                  l_numc_2_char = l_type_i.
                  condense l_numc_2_char no-gaps.
                  concatenate <f_source> l_numc_2_char into <f_source>
                             separated by space.
                endif.
                if not ( i_datatype is initial and
                         i_inttype  is initial and
                         i_leng     is initial and
                         i_decimals is initial ).
          concatenate <f_source> ')' into <f_source> separated by space.
                endif.
              endif.
            endif.
          endif.
        else.
        endif.
      endif.
    else.
*     Special processing when Filetype is ASCII
      assign component 1 of structure t_header to <f_source>.
      if sy-subrc = 0.
        <f_source> = s_dfies_struc-fieldname.
        concatenate <f_source> s_dfies_struc-offset into <f_source>
                   separated by space.
        append t_header.
        clear t_header.
      endif.
    endif.
    at last.
      check not t_header is initial.
      append t_header.
    endat.
  endloop.

endfunction.
