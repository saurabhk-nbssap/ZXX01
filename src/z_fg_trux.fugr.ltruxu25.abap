function trut_move_corresponding_table.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(PI_SOURCE_TABLE) TYPE  TABLE
*"     VALUE(PI_INTELLY_COPY) TYPE  BOOLEAN DEFAULT 'X'
*"  CHANGING
*"     VALUE(PC_TARGET_TABLE) TYPE  TABLE
*"----------------------------------------------------------------------
  field-symbols: <fs_source>, <fs_source_struct> , <fs_target>.
  data: l_data  type ref to data.
  data: l_data2 type ref to data.

  data: l_tabix  type sytabix.

  loop at pi_source_table assigning <fs_source>.
    read table pc_target_table index sy-tabix
                               assigning <fs_target>.
    if sy-subrc <> 0.
      clear l_tabix.
      if l_data is initial.
        create data l_data like line of pc_target_table.
        assign l_data->* to <fs_target>.
        check sy-subrc = 0.
      endif.
      clear <fs_target>.
    else.
      l_tabix = sy-tabix.
    endif.
    call function 'TRUT_MOVE_CORRESPONDING'
         exporting
              pi_source_structure_1 = <fs_source>
              pi_intelly_copy       = pi_intelly_copy
         changing
              pc_target_structure   = <fs_target>
         exceptions
              error_found           = 1
              others                = 2.
    if sy-subrc = 0 and not <fs_target> is initial.
      if l_tabix is initial.
        append <fs_target> to pc_target_table.
      else.
        modify pc_target_table from <fs_target> index l_tabix.
      endif.
    endif.

  endloop.

endfunction.
