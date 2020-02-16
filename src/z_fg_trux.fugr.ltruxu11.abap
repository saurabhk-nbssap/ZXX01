FUNCTION SAP_STARTS_EXCEL.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME
*"  EXCEPTIONS
*"      EXCECUTION_FAILED
*"----------------------------------------------------------------------
  type-pools: soi, cntl.
  constants: g_con_excel type char80 value 'Excel.Sheet',
             g_max_empty_rows type i value 5.


  perform get_spreadsheet_interface using g_con_excel
                                    changing
                                      i_filename
                                      g_oref_container g_iref_control
                                      g_iref_error     g_iref_document
                                      g_iref_spreadsheet.


  call screen 1010.

  free: g_iref_spreadsheet.
  call method g_iref_document->close_document.
  call method g_iref_document->release_document.
  free g_iref_document.

  call method g_iref_control->release_all_documents.
  call method g_iref_control->destroy_control.

ENDFUNCTION.
