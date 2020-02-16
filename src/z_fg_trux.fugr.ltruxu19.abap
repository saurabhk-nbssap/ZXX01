FUNCTION TRANSLATE_CODEPAGE_IN.
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

  data: buffer type long_x,
        conv type ref to cl_abap_conv_in_ce.

  FIELD-SYMBOLS:
    <FS> TYPE any.

  loop at t_data.
    assign t_data to <fs> casting type long_x.
    CONV = CL_ABAP_CONV_in_CE=>CREATE( ENCODING = codepage_from
                                       input = <fs> ).
    CALL METHOD CONV->read( importing data = t_data ).
    MODIFY T_DATA.
  endloop.

ENDFUNCTION.
