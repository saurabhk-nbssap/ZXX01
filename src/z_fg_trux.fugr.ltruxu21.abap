FUNCTION PROCESS_GUI_DDIC_CONVERSION.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_STRUKTUR_NAME_I1)
*"     VALUE(I_STRUKTUR_I1)
*"  CHANGING
*"     VALUE(I_STRUKTUR_I2)
*"  EXCEPTIONS
*"      ERROR_FOUND
*"----------------------------------------------------------------------
  DATA: tmp_i1_dfies LIKE i_dfies .
  DATA: tmp_i2_dfies LIKE i_dfies OCCURS 1000 WITH HEADER LINE.

  DATA: BEGIN OF ub_dfies OCCURS 2000.
          INCLUDE STRUCTURE dfies.
  DATA: END   OF ub_dfies.

  DATA:
        l_tfdir     LIKE tfdir,
        l_fm_name   LIKE tfdir-funcname,
        rc          LIKE sy-subrc,
        fieldlen    TYPE i,
        strlen      TYPE i.

  CONSTANTS:
     c_fm_header     LIKE tfdir-funcname VALUE 'CONVERSION_EXIT_',
     c_fm_trailer    LIKE tfdir-funcname VALUE '_INPUT',
     c_datatype_numc LIKE dfies-datatype VALUE 'NUMC',
     c_datatype_curr LIKE dfies-datatype VALUE 'CURR',
     c_p_type        LIKE dfies-inttype  VALUE 'P'.

* DDIC Info besorgen
  CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
    EXPORTING
      i_structure_tabname = i_struktur_name_i1
    TABLES
      it_dfies            = ub_dfies
    EXCEPTIONS
      OTHERS              = c_rc4.
  IF rc <> c_rc0.
    RAISE error_found.
  ENDIF.

  i_struktur_i2 = i_struktur_i1.

  LOOP AT ub_dfies WHERE tabname = i_struktur_name_i1.
*   Uppercase translation
    IF ub_dfies-lowercase is initial and ub_dfies-inttype = 'C'.
      ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i1
          TO <f1> TYPE ub_dfies-inttype.
      IF sy-subrc = c_rc0.
        set locale language sy-langu.
        TRANSLATE <f1> TO UPPER CASE.                    "#EC TRANSLANG
        ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i2
                TO <f2> TYPE ub_dfies-inttype.
        <f2> = <f1> .
      ENDIF.
    ENDIF.

*   Conversion Exits bearbeiten
    IF not ub_dfies-convexit is initial.
      IF ub_dfies-inttype = c_p_type.
        ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i2
                TO <f1> TYPE ub_dfies-inttype
                                  DECIMALS ub_dfies-decimals.
      ELSE.
        ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i2
                TO <f1> TYPE ub_dfies-inttype.
      ENDIF.
      IF sy-subrc = c_rc0.
        CONCATENATE c_fm_header ub_dfies-convexit c_fm_trailer
                                             INTO l_fm_name.
        CONDENSE l_fm_name NO-GAPS.
        SELECT SINGLE * FROM tfdir INTO l_tfdir
                                   WHERE funcname = l_fm_name.
        IF sy-subrc = c_rc0.
          CALL FUNCTION l_fm_name
            EXPORTING
              input  = <f1>
            IMPORTING
              output = <f1>
            EXCEPTIONS
              OTHERS = c_rc4.
          IF sy-subrc <> c_rc0.
            RAISE error_found.
          ELSE.
           ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i2
                TO <f2> TYPE ub_dfies-inttype.
           <f2> = <f1> .
           ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i1
                TO <f2> TYPE ub_dfies-inttype.
           <f2> = <f1> .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
