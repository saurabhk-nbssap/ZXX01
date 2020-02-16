FUNCTION trut_move_corresponding .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(PI_SOURCE_STRUCTURE_1)
*"     VALUE(PI_SOURCE_STRUCTURE_2) OPTIONAL
*"     VALUE(PI_SOURCE_STRUCTURE_3) OPTIONAL
*"     VALUE(PI_INTELLY_COPY) TYPE  BOOLEAN DEFAULT 'X'
*"  CHANGING
*"     VALUE(PC_TARGET_STRUCTURE)
*"  EXCEPTIONS
*"      ERROR_FOUND
*"      INVALID_TYPE
*"----------------------------------------------------------------------
  CONSTANTS: c_exclamation   VALUE '!'.

  DATA tmp_char.
  DATA l_tabname TYPE dfies-tabname.
  DATA l_data TYPE REF TO data.
  DATA l_oref_descr_structure_1 TYPE REF TO cl_abap_structdescr.
  DATA l_oref_descr_structure_2 TYPE REF TO cl_abap_structdescr.
  DATA l_oref_descr_structure_3 TYPE REF TO cl_abap_structdescr.
  DATA l_oref_descr_target TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS:
         <fs_structure> ,
         <fs_target>,
         <fs_components_structure_1>
            LIKE LINE OF l_oref_descr_structure_1->components,
         <fs_components_structure_2>
            LIKE LINE OF l_oref_descr_structure_2->components,
         <fs_components_structure_3>
            LIKE LINE OF l_oref_descr_structure_3->components,
         <fs_components_target> TYPE abap_compdescr.

  l_oref_descr_structure_1 ?=
       cl_abap_typedescr=>describe_by_data( pi_source_structure_1 ).
  IF l_oref_descr_structure_1->type_kind <>
     cl_abap_typedescr=>typekind_struct1.
    RAISE invalid_type.
  ENDIF.
  IF NOT pi_source_structure_2 IS INITIAL.
    l_oref_descr_structure_2 ?=
       cl_abap_typedescr=>describe_by_data( pi_source_structure_2 ).
    IF l_oref_descr_structure_2->type_kind <>
         cl_abap_typedescr=>typekind_struct1.
      RAISE invalid_type.
    ENDIF.
  ENDIF.
  IF NOT pi_source_structure_3 IS INITIAL.
    l_oref_descr_structure_3 ?=
       cl_abap_typedescr=>describe_by_data( pi_source_structure_3 ).
    IF l_oref_descr_structure_3->type_kind <>
         cl_abap_typedescr=>typekind_struct1.
      RAISE invalid_type.
    ENDIF.
  ENDIF.
  l_oref_descr_target ?=
       cl_abap_typedescr=>describe_by_data( pc_target_structure ).
  IF l_oref_descr_target->type_kind <>
    cl_abap_typedescr=>typekind_struct1.
    RAISE invalid_type.
  ENDIF.

* Simple logic like MOVE-CORRESPONDING

* Start copying fields
  LOOP AT l_oref_descr_target->components
          ASSIGNING <fs_components_target>.
*   position to field in target structure
    CLEAR: sy-subrc.
    IF NOT <fs_structure> IS INITIAL.
      CLEAR <fs_structure>.
    ENDIF.
    ASSIGN COMPONENT sy-tabix OF STRUCTURE pc_target_structure TO
              <fs_target> TYPE <fs_components_target>-type_kind.
*   take care of deletion character "!"
    CHECK sy-subrc = 0.
    tmp_char = <fs_target>.
    IF tmp_char = c_exclamation.
      CLEAR <fs_target>.
    ENDIF.
*   check if target fiedl is initial. if this is true try to make a copy
*    IF NOT pi_intelly_copy IS INITIAL.
*      CHECK <fs_target> IS INITIAL.
*    ENDIF.
*   read the corresponding field of the 1st source structure
    READ TABLE l_oref_descr_structure_1->components
                           ASSIGNING <fs_components_structure_1>
                           WITH KEY name = <fs_components_target>-name.
    IF sy-subrc = 0.
      ASSIGN COMPONENT sy-tabix OF STRUCTURE pi_source_structure_1 TO
          <fs_structure> TYPE <fs_components_structure_1>-type_kind.
      IF sy-subrc = 0 AND  pi_intelly_copy IS INITIAL.
        IF <fs_structure> IS INITIAL.
          tmp_char = c_exclamation.
        ENDIF.
        sy-subrc = 4.
      ENDIF.
    ENDIF.
*   handle 2nd source structure the same way as the 1st one.
    IF sy-subrc <> 0 AND NOT l_oref_descr_structure_2 IS INITIAL.
      IF NOT l_oref_descr_structure_2->components IS INITIAL.
        READ TABLE l_oref_descr_structure_2->components
                            ASSIGNING <fs_components_structure_2>
                            WITH KEY name = <fs_components_target>-name.
        IF sy-subrc = 0.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE pi_source_structure_2 TO
              <fs_structure> TYPE <fs_components_structure_2>-type_kind.
          IF sy-subrc = 0 AND  pi_intelly_copy IS INITIAL.
            IF <fs_structure> IS INITIAL.
              tmp_char = c_exclamation.
            ENDIF.
            sy-subrc = 4.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF pi_intelly_copy IS INITIAL
         AND l_oref_descr_structure_3 IS INITIAL.
        CLEAR sy-subrc.
      ELSE.
        sy-subrc = 4.
      ENDIF.
    ENDIF.
*   handle 3rd source structure the same way as the 1st one.
    IF sy-subrc <> 0 AND NOT l_oref_descr_structure_3 IS INITIAL.
      IF NOT l_oref_descr_structure_3->components IS INITIAL.
        READ TABLE l_oref_descr_structure_3->components
                            ASSIGNING <fs_components_structure_3>
                            WITH KEY name = <fs_components_target>-name.
        IF sy-subrc = 0.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE pi_source_structure_3 TO
              <fs_structure> TYPE <fs_components_structure_3>-type_kind.
          IF sy-subrc = 0 AND pi_intelly_copy IS INITIAL.
            IF <fs_structure> IS INITIAL.
              tmp_char = c_exclamation.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR sy-subrc.
    ENDIF.

*   copy field from source to target:
    CHECK sy-subrc = 0.
    IF NOT ( pi_intelly_copy IS INITIAL AND tmp_char = c_exclamation ).
      CHECK NOT <fs_structure> IS INITIAL.
    ENDIF.
*    tmp_char = <fs_structure>.                               "N2468930
    IF NOT pi_intelly_copy IS INITIAL.
*     take care of deletion character if necessary.
      IF tmp_char = c_exclamation.
        CLEAR <fs_target>.
      ELSE.
        IF <fs_target> IS INITIAL.
          <fs_target> = <fs_structure>.
        ENDIF.
      ENDIF.
    ELSE.
      <fs_target> = <fs_structure>.
*     take care of deletion character if necessary.
      IF tmp_char = c_exclamation.
        CLEAR <fs_target>.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
