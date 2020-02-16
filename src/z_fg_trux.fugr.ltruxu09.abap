function display_btci_tree.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_COLLECTED_DATA) TYPE  BOOLEAN DEFAULT SPACE
*"             VALUE(I_HIDE_TREE) TYPE  BOOLEAN DEFAULT SPACE
*"----------------------------------------------------------------------
  data: l_tab_btcidata type rlfvbtci occurs 0.
  data: l_repid type syrepid.

  if not g_tab_current_btcidata is initial.
    append lines of g_tab_current_btcidata to g_tab_all_btcidata.
    clear g_tab_current_btcidata.
  endif.

  if i_collected_data is initial.
    l_tab_btcidata = g_tab_current_btcidata.
  else.
    l_tab_btcidata = g_tab_all_btcidata.
  endif.

  if g_oref_btci_tree is initial.
    check not l_tab_btcidata is initial.
    l_repid = sy-repid.
    create object g_oref_btci_tree exporting
                                    pi_caption = text-cap
                                    pi_tabname = 'RLFVBTCI'
                                    pi_repid   = l_repid.
  endif.

  if i_hide_tree is initial.
    call method g_oref_btci_tree->show_grid
                           exporting pi_structure_name = 'RLFVBTCI'
                                     pi_rlfvbtci = l_tab_btcidata
                           exceptions others = 4.
  else.
    call method g_oref_btci_tree->hide_grid.
  endif.
endfunction.
