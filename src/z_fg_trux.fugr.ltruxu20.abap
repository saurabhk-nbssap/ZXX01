FUNCTION TRANSLATE_CODEPAGE_OUT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(CODEPAGE_FROM) TYPE  ABAP_ENCOD
*"  TABLES
*"      T_DATA
*"  EXCEPTIONS
*"      ERROR_TRANSLATE
*"----------------------------------------------------------------------
* This functions makes a conversion from the external codepage
* to the system-codepage

  TYPES:
    LONG_X TYPE X LENGTH 2048,
    LONG_C TYPE C LENGTH 512.

  data: wa_t_data type string,
        buffer type long_x,
        conv type ref to cl_abap_conv_out_ce.

  loop at t_data into wa_t_data.

    CONV = CL_ABAP_CONV_out_CE=>CREATE( ENCODING = codepage_from ).
    CALL METHOD CONV->write( data = WA_T_DATA ).
    buffer = conv->get_buffer( ).
    WA_T_DATA = buffer.
    MODIFY T_DATA FROM WA_T_DATA.
  endloop.

ENDFUNCTION.
