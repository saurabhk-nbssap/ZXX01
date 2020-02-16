FUNCTION sap_convert_to_xml_format.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME OPTIONAL
*"     VALUE(I_APPL_KEEP) TYPE  CHAR01 DEFAULT SPACE
*"     VALUE(I_XML_DOC_NAME) TYPE  CHAR30 OPTIONAL
*"  EXPORTING
*"     VALUE(PE_BIN_FILESIZE) TYPE  I
*"  TABLES
*"      I_TAB_SAP_DATA TYPE  STANDARD TABLE
*"  CHANGING
*"     REFERENCE(I_TAB_CONVERTED_DATA) TYPE  TRUXS_XML_TABLE OPTIONAL
*"  EXCEPTIONS
*"      CONVERSION_FAILED
*"----------------------------------------------------------------------
  DATA: l_iref_pixml          TYPE REF TO if_ixml,
        l_iref_pdocument      TYPE REF TO if_ixml_document,
        l_iref_pstreamfactory TYPE REF TO if_ixml_stream_factory,
        l_iref_pparser        TYPE REF TO if_ixml_parser,
        l_iref_pnode          TYPE REF TO if_ixml_node,
        l_iref_ptext          TYPE REF TO if_ixml_text,
        l_iref_postream       TYPE REF TO if_ixml_ostream,
        l_iref_root_elem TYPE REF TO if_ixml_element,
        l_iref_elem TYPE REF TO if_ixml_element,
        l_data  TYPE ref to data,
        l_result TYPE i,
        l_name  TYPE string,
        l_value TYPE string,
        l_numc_2_char(12),
        l_totalsize    TYPE i.

  FIELD-SYMBOLS: <fs_temp_table> TYPE table.
  FIELD-SYMBOLS: <fs_temp_data>.
  FIELD-SYMBOLS: <fs_component_data>.

  DATA l_oref_descr_source TYPE REF TO cl_abap_structdescr.
  DATA l_oref_descr_elem TYPE REF TO cl_abap_elemdescr.

  DATA: l_tab_dfies TYPE TABLE OF dfies.
  DATA: l_tabname TYPE  dd02l-tabname.
  DATA: l_tabname_string TYPE  string.

  DATA: itab_fdes_import TYPE TABLE OF fdes_import.
  DATA: l_xml_root TYPE string.

  ASSIGN i_tab_sap_data[] TO <fs_temp_table>.
  CREATE DATA l_data LIKE LINE OF <fs_temp_table>.
  ASSIGN l_data->* TO <fs_temp_data>.

  l_oref_descr_source ?=
      cl_abap_typedescr=>describe_by_data( <fs_temp_data> ).

  SEARCH l_oref_descr_source->absolute_name FOR '\TYPE='.
  IF sy-subrc = 0.
    sy-fdpos = sy-fdpos + STRLEN( '\TYPE=' ) .
    l_tabname = l_oref_descr_source->absolute_name+sy-fdpos.
    l_tabname_string = l_tabname.
    CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
         EXPORTING
              i_structure_tabname = l_tabname
         TABLES
              it_dfies            = l_tab_dfies
         EXCEPTIONS
              OTHERS              = 4.
  ENDIF.
*-- create the main factory
  IF l_iref_pixml IS INITIAL.
    l_iref_pixml = cl_ixml=>create( ).
  ENDIF.
*-- create the initial document
  l_iref_pdocument = l_iref_pixml->create_document( ).

  l_xml_root = i_xml_doc_name.
  if l_xml_root is initial.
    l_xml_root = l_tabname_string.
  endif.

* Create XML doc with content
  LOOP AT <fs_temp_table> INTO <fs_temp_data>.
    AT FIRST.
      PERFORM xml_header USING l_iref_pdocument l_xml_root
                         CHANGING l_iref_root_elem.
    ENDAT.
    l_iref_elem = l_iref_root_elem.
    PERFORM xml_node USING l_iref_pdocument l_tabname_string
                     CHANGING l_iref_elem.
    IF NOT l_tab_dfies IS INITIAL.
      PERFORM dfies_to_xml TABLES l_tab_dfies
                           USING l_iref_pdocument l_iref_elem
                                 <fs_temp_data> 'X'.
    ELSE.
      PERFORM descr_to_xml USING l_oref_descr_source->components
                                 l_iref_pdocument l_iref_elem
                                 <fs_temp_data> 'X'.
    ENDIF.
  ENDLOOP.
* Create XML doc as structure only
  IF sy-subrc <> 0.
    CLEAR <fs_temp_data>.
    PERFORM xml_header USING l_iref_pdocument l_xml_root
                       CHANGING l_iref_elem.
    PERFORM xml_node USING l_iref_pdocument l_tabname_string
                     CHANGING l_iref_elem.
    IF NOT l_tab_dfies IS INITIAL.
      PERFORM dfies_to_xml TABLES l_tab_dfies
                           USING l_iref_pdocument l_iref_elem
                                 <fs_temp_data> space.
    ELSE.
      PERFORM descr_to_xml USING l_oref_descr_source->components
                                 l_iref_pdocument l_iref_elem
                                 <fs_temp_data> space.
    ENDIF.
  ENDIF.

*-- create the stream factory
  l_iref_pstreamfactory = l_iref_pixml->create_stream_factory( ).
  l_iref_postream =
          l_iref_pstreamfactory->create_ostream_itable(
                          table = i_tab_converted_data ).
  CALL METHOD l_iref_pdocument->render(
                                ostream   = l_iref_postream ).
* -- how many bytes were written to the table?
  l_result = l_iref_postream->get_num_written_raw( ).
  pe_bin_filesize = l_result.

ENDFUNCTION.
