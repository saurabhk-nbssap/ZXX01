FUNCTION text_convert_xml_to_sap.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_FIELD_SEPERATOR) TYPE  CHAR01 DEFAULT ';'
*"             VALUE(I_LINE_HEADER) TYPE  CHAR01 OPTIONAL
*"             VALUE(I_TAB_RAW_DATA) TYPE  TRUXS_XML_TABLE
*"             VALUE(I_FILENAME) LIKE  RLGRAP-FILENAME OPTIONAL
*"             VALUE(I_TOTALSIZE) TYPE  I
*"       TABLES
*"              I_TAB_CONVERTED_DATA TYPE  STANDARD TABLE
*"       EXCEPTIONS
*"              CONVERSION_FAILED
*"----------------------------------------------------------------------
  DATA: l_iref_ixml          TYPE REF TO if_ixml,
        l_iref_document      TYPE REF TO if_ixml_document,
        l_iref_streamfactory TYPE REF TO if_ixml_stream_factory,
        l_iref_istream       TYPE REF TO if_ixml_istream,
        l_iref_parser        TYPE REF TO if_ixml_parser,
        l_iref_node          TYPE REF TO if_ixml_node,
        l_FIELDNAME          type FIELDNAME.

  data: l_errors type i,
        l_index type sy-tabix,
        lo_err  type ref to if_ixml_parse_error,
        l_errtxt type string.

  FIELD-SYMBOLS: <fs_converted_data>.

*-- create the main factory
  l_iref_ixml = cl_ixml=>create( ).

*-- create the initial document
  l_iref_document = l_iref_ixml->create_document( ).

*-- create the stream factory
  l_iref_streamfactory = l_iref_ixml->create_stream_factory( ).

*-- create an input stream for the table
  l_iref_istream = l_iref_streamfactory->create_istream_itable(
                                                  table = i_tab_raw_data
                                                  size  = i_totalsize ).


*-- create the parser
  l_iref_parser = l_iref_ixml->create_parser(
                                 stream_factory = l_iref_streamfactory
                                 istream        = l_iref_istream
                                 document       = l_iref_document ).
*-- parse the stream
  IF l_iref_parser->parse( ) <> 0 AND l_iref_parser->num_errors( ) <> 0.

    l_errors = l_iref_parser->num_errors( min_severity = 1 ).
    IF l_errors > 0.
    DO l_errors TIMES.
      l_index = sy-index - 1.
      lo_err = l_iref_parser->get_error( index = l_index min_severity = 1 ).
      l_errtxt = lo_err->get_reason( ).
      WRITE: / l_errtxt.
    ENDDO.
    ENDIF.

    RAISE conversion_failed.
  ENDIF.

  CALL METHOD l_iref_istream->close( ).

  ASSIGN LOCAL COPY OF INITIAL LINE OF i_tab_converted_data
                                                TO <fs_converted_data>.

  l_iref_node ?= l_iref_document.
  PERFORM parse_node USING    l_iref_node l_FIELDNAME
                     CHANGING i_tab_converted_data[]
                                                   <fs_converted_data>.

* correct the decimals
  PERFORM correct_decimals_for_current TABLES i_tab_converted_data.


ENDFUNCTION.
