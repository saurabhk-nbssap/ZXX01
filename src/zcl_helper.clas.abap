class ZCL_HELPER definition
  public
  final
  create public .

public section.

  types:
    tty_document_number type range of draw-doknr .
  types:
    ty_document_number type line of tty_document_number .
  types:
    tty_object_key type range of drad-objky .
  types:
    ty_object_key type line of tty_object_key .
  types:
    begin of ty_fields,
        name type string,
      end of ty_fields .
  types:
    tty_fields type table of ty_fields with default key .
  types:
    begin of ty_sheet,
        name   type string,
        data   type ref to data,
        string type abap_bool,
        fields type tty_fields,
        header type abap_bool,
      end of ty_sheet .
  types:
    tty_sheet type table of ty_sheet with default key .
  types:
    begin of ty_excel,
        sheet_name type string,
        rows       type i,
        cols       type i,
        data_tab   type ref to data,
      end of ty_excel .
  types:
    tty_excel type table of ty_excel with default key .

  constants:
    begin of gc_s_sheet_number,
        default type i value 1,
        _1      type i value 1,
        _2      type i value 2,
        _3      type i value 3,
        _4      type i value 4,
        _5      type i value 5,
        _6      type i value 6,
        _7      type i value 7,
        _8      type i value 8,
        _9      type i value 9,
        _10     type i value 10,
      end of gc_s_sheet_number .
  constants:
    begin of gc_path_sep,
      windows type c length 1 value '\',
      unix    type c length 1 value '/',
    end of gc_path_sep .
  constants:
    begin of gc_extension,
      xls  type c length 3 value 'XLS',
      xlsx type c length 4 value 'XLSX',
      bin  type c length 3 value 'BIN',
    end of gc_extension .

  class-methods XML_TO_ABAP
    importing
      value(XML_INPUT) type STRING
      value(ROOT) type STRING
    exporting
      value(ASXML_OUT) type STRING
    changing
      value(ABAP_OUT) type ANY .
  class-methods PRINT_NO_PG_BRK .
  class-methods IS_PRODUCTION
    returning
      value(PRD) type ABAP_BOOL .
  class-methods IS_QUALITY
    returning
      value(QAS) type ABAP_BOOL .
  class-methods IS_DEVELOPMENT
    returning
      value(DEV) type ABAP_BOOL .
  class-methods IS_SANDBOX
    returning
      value(SBX) type ABAP_BOOL .
  class-methods FILL_X_FIELDS
    importing
      value(DATA) type ANY
    changing
      value(DATAX) type ANY .
  class-methods ABAP_TO_XML
    importing
      !ABAP_IN type ANY
      !ROOT type STRING
    exporting
      !ASXML_OUT type STRING
      !XML_OUT type STRING .
  class-methods GET_DOBJ_NAME
    importing
      value(DATA) type ANY
    returning
      value(NAME) type STRING .
  class-methods CONV_SPOOL_LIST_2_ALV
    importing
      value(I_SPOOLN) type TSP01-RQIDENT
      value(I_KEEP_SUM_LINE) type BOOLEAN default 'X'
      value(I_START_TABLE) type INT4 default 1 .
  class-methods CALC_PO_ITEM_TAX
    importing
      value(IS_EKKO) type EKKO
      value(IS_EKPO) type EKPO
    exporting
      value(ET_TAXES) type KOMV_T .
  class-methods DMS_DOWNLOAD_UTILITY
    importing
      value(IT_DOCUMENT_NUMBERS) type TTY_DOCUMENT_NUMBER optional
      value(IT_OBJECT_KEYS) type TTY_OBJECT_KEY optional
      value(IV_DWNLD_FOLDER_PATH) type STRING optional
      value(IV_UNCOMPRESSED) type BOOLEAN default ABAP_TRUE
      value(IV_COMPRESSED) type BOOLEAN default ABAP_FALSE
    exceptions
      INVALID_OUTPUT_MODE
      EMPTY_INPUT
      NO_OUTPUT_FOLDER .
  class-methods UPDATE_REPORT_VARIANTS_DB
    importing
      value(IS_LAYOUT) type UPD_S_LAYO
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods FILE_SELECTION_DIALOG
    importing
      value(IV_WINDOW_TITLE) type STRING default 'Select File/s'
      value(IV_FILE_FILTER) type STRING default CL_GUI_FRONTEND_SERVICES=>FILETYPE_ALL
      value(IV_INITIAL_DIRECTORY) type STRING optional
      value(IV_MULTISELECTION) type ABAP_BOOL default ABAP_FALSE
    exporting
      value(ET_FILES) type FILETABLE
    returning
      value(RV_FILE) type STRING .
  class-methods FILE_SAVE_DIALOG
    importing
      value(IV_WINDOW_TITLE) type STRING default 'Select File'
      value(IV_FILE_FILTER) type STRING default CL_GUI_FRONTEND_SERVICES=>FILETYPE_ALL
      value(IV_INITIAL_DIRECTORY) type STRING optional
      value(IV_DEFAULT_EXTENSION) type STRING optional
      value(IV_DEFAULT_FILE_NAME) type STRING optional
    returning
      value(RV_FILE) type STRING .
  class-methods WRITE_FILE_TO_PATH
    importing
      value(IV_FILEPATH) type STRING
      value(IV_OVERWRITE) type ABAP_BOOL default ABAP_FALSE
      value(IV_FILE_LENGTH) type I optional
      value(IT_DATA) type SOLIX_TAB
    returning
      value(RV_UPLOADED) type ABAP_BOOL
    raising
      ZCX_GENERIC .
  class-methods READ_FILE_FROM_PATH
    importing
      value(IV_FILEPATH) type STRING
    exporting
      value(EV_FILE_LENGTH) type I
    returning
      value(RT_DATA) type SOLIX_TAB
    raising
      ZCX_GENERIC .
  class-methods DELETE_FILE
    importing
      value(IV_FILEPATH) type STRING
    exporting
      value(EV_MESSAGE) type STRING
    returning
      value(RV_DELETED) type ABAP_BOOL .
  class-methods CHECK_FILE_EXISTS
    importing
      value(IV_FILEPATH) type STRING
    exporting
      value(EV_MESSAGE) type STRING
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  class-methods EXCEL_TO_ITAB
    importing
      value(IV_FILE) type STRING
      value(IV_NO_OF_HEADERS) type I default 0
      value(IV_CHECK_FILE_FORMAT) type ABAP_BOOL default ABAP_FALSE
      value(IV_MOVE_CORRESPONDING) type ABAP_BOOL default ABAP_FALSE
      value(IV_WITH_CONV_EXIT) type ABAP_BOOL default ABAP_FALSE
      value(IV_SHEET_NUMBER) type I default GC_S_SHEET_NUMBER-DEFAULT
      value(IV_READ_ALL_SHEETS) type ABAP_BOOL default ABAP_FALSE
    exporting
      value(ET_EXCEL) type TTY_EXCEL
    changing
      value(CT_ITAB) type ANY TABLE .
  class-methods ITAB_TO_EXCEL
    importing
      value(IT_ITAB) type ANY TABLE optional
      value(IT_FIELDS) type TTY_FIELDS optional
      value(IV_INSERT_HEADER) type ABAP_BOOL default ABAP_TRUE
      value(IV_FORCE_STRING) type ABAP_BOOL optional
      value(IV_FILE_PATH) type STRING optional
      value(IV_SHEET_NAME) type STRING default 'SAP_DATA'
      value(IT_MULTI_SHEET_DATA) type TTY_SHEET optional
    exporting
      value(EV_FILE_LENGTH) type I
    returning
      value(RT_DATA) type SOLIX_TAB .
  class-methods FORMAT_EXCEL_TO_BAPI
    changing
      value(IS_EXCEL) type ANY
      value(IS_DATA) type ANY .
  class-methods CHECK_FILE_FORMAT
    changing
      value(CTAB) type ANY TABLE
    exceptions
      FILE_FORMAT_ALTERED .
  class-methods CLEAR_X_FIELDS
    changing
      value(CS_DATA) type ANY .
  class-methods BUILD_INDUCTION_EMAIL_BODY
    importing
      value(IV_SALUTATION) type STRING
    returning
      value(RT_BODY) type SOLI_TAB .
  class-methods BUILD_BASE64_FROM_URL
    importing
      value(IV_URL) type STRING
    returning
      value(RV_BASE64_STR) type STRING .
  class-methods GET_MANAGER_EMP_ID
    importing
      value(IV_EMPL_ID) type PA0001-PERNR
    returning
      value(IV_MGR_EMPL_ID) type PA0001-PERNR .
  class-methods CONV_EXIT
    importing
      value(IV) type ANY
      value(IV_MODE) type MODE default 'I'
    exporting
      value(EV) type ANY .
  class-methods REMOVE_SPECIAL_CHARS
    changing
      value(CV_TEXT) type CLIKE .
  class-methods CONDENSE_DATA
    changing
      value(CS_DATA) type DATA .
  class-methods DISPLAY_EXCEPTION
    importing
      value(IV_CLASS) type SEOCLSNAME optional
      value(IV_METHOD) type SEOCPDNAME optional
      value(IV_SUBRC) type SY-SUBRC optional
      value(IV_FM) type EU_LNAME optional
      value(IS_MSG) type SYMSG optional
      value(IV_DISP_LIKE) type SY-MSGTY default 'E' .
  class-methods ITAB_TO_FCAT
    importing
      value(IV_DATA) type ANY
    exporting
      value(ET_LVC_FCAT) type LVC_T_FCAT
      value(ET_SLIS_FCAT) type SLIS_T_FIELDCAT_ALV .
  class-methods SIMPLE_SALV_DISPLAY
    importing
      value(IT_TABLE) type STANDARD TABLE
      value(IV_USE_COL_NAME_AS_HEADER) type ABAP_BOOL default ABAP_FALSE .
  class-methods GET_BGRFC_UNIT
    returning
      value(RO_UNIT) type ref to IF_TRFC_UNIT_OUTBOUND .
  class-methods GET_IRN_QRCODE
    importing
      value(IV_COMP_CODE) type ZEI_DET_COMPANY_CODE optional
      value(IV_DOCUMENT) type ZEI_DET_CUSTOM1 optional
      value(IV_FIS_YEAR) type GJAHR optional
      value(IV_ODN_NUMBER) type ZEI_DET_DOCUMENT optional
      value(IV_ODN_DATE) type ZEI_DET_DOC_DATE optional
    exporting
      value(RV_QR10) type CHAR255
      value(RV_QR1) type CHAR255
      value(RV_QR2) type CHAR255
      value(RV_QR3) type CHAR255
      value(RV_QR4) type CHAR255
      value(RV_QR5) type CHAR255
      value(RV_QR6) type CHAR255
      value(RV_QR7) type CHAR255
      value(RV_QR8) type CHAR255
      value(RV_QR9) type CHAR255 .
  class-methods ITAB_TO_HTML
    importing
      value(IT_TABLE) type STANDARD TABLE
    returning
      value(RV_HTML) type STRING .
protected section.
private section.

  constants GC_SANDBOX_SYSID type SYST-SYSID value 'SBX' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_HELPER IMPLEMENTATION.


  method abap_to_xml.
*{   INSERT         SBXK900102                                        1
    " Premise is to transform/map any abap structure/table to xml string
    " For this we first convert abap data to as/canonical-xml
    " Then convert as-xml to xml string
    " Conversion to intermediate as-xml is manadatory since we are relying on using the built-in ID transformation that can only process as-xml
    data: lv_root type string.

    " mapping between xml and abap data is case-sensitive
    clear lv_root.
    move root to lv_root.
    translate lv_root to upper case. " (Since field-names in abap data are always UPPER CASE)

    " convert abap data to name-value tabular format for as-xml conversion
    data(source_tab) = value abap_trans_srcbind_tab(
          ( name = lv_root value = ref #( abap_in ) ) ).

    " generate as-xml in xstring format from abap data using built-in ID transformation
    call transformation demo_id_upper_lower "id
    parameters mode = 'UP'
    source (source_tab)
    result xml data(asxml_xstr).

    " convert as-xml xstring to as-xml string(readable)
    asxml_out = cl_abap_codepage=>convert_from( asxml_xstr ).
    xml_out = asxml_out.

    data: pattern type string value '<(/asx|asx|\?xml)(?:"[^"]*"[''"]*|''[^'']*''[''"]*|[^''">])+>'.
    try.
        data(lo_regex) = new cl_abap_regex( pattern = pattern ignore_case = abap_true ).
        if lo_regex is bound.
          replace all occurrences of regex lo_regex in xml_out with ''.
          replace all occurrences of '#' in xml_out with ''.
        endif.
      catch cx_sy_regex into data(lox_regex).
    endtry.
*}   INSERT
  endmethod.


  method build_base64_from_url.
    check iv_url is not initial.
    data: begin of ls,
            x type x,
          end of ls,
          lt like standard table of ls.

    clear: ls.
    refresh lt.
    call function 'DP_GET_STREAM_FROM_URL'
      exporting
        url               = iv_url
      tables
        data              = lt
      exceptions
        dp_fail           = 1
        dp_failed_init    = 2
        blocked_by_policy = 3
        unknown_error     = 4
        others            = 5.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    check lt is not initial.
    try.
        data(lv_unencoded_xstr) = cl_bcs_convert=>xtab_to_xstring( exporting it_xtab = lt ).
      catch cx_bcs.
    endtry.

    check lv_unencoded_xstr is not initial.
    " here you may use cl_bcs_convert=>xstring_to_solix( ) for conversion to binary
    data(lv_encoded_base64) = cl_http_utility=>encode_x_base64( exporting unencoded = lv_unencoded_xstr ).

    check lv_encoded_base64 is not initial.
    clear rv_base64_str.
    rv_base64_str = lv_encoded_base64.
  endmethod.


  method build_induction_email_body.
    " IHDK900606: HR: S_K: ZHR_PS: Add induction/welcome email: 13.2.19
    check iv_salutation is not initial.
    data: lv_salutation type c length 50.

    clear lv_salutation.
    lv_salutation = condense( iv_salutation ).
    call function 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
      exporting
        input_string  = lv_salutation
      importing
        output_string = lv_salutation.

    " build main html for image conversion
    data(lv_base_img_url) = 'https://i.imgur.com/D4v8yHY.png'.
*    'https://drive.google.com/uc?export=view&id=1U2BtbdVOkhkZ-3bOkCR1y_bR7ofyQ5EK'.
    data(lv_html_for_image) = |<img style='display: block; margin-left: auto; margin-right: auto' | &&
                              |src='{ lv_base_img_url }' alt='induction'/>| &&
*                              |src='data:image/png;base64,{ zcl_helper=>build_base64_from_url( iv_url = lv_base_img_url ) }' | &&
*                              |alt='induction'/>| &&
                              |<div style='position: absolute;top: 50px;left: 485px;| &&
                              |font-size: 18.0pt;font-family: Candara, sans-serif;color:black;'>| &&
                              |<strong>Welcome Aboard!</strong></div>| &&
                              |<div style='position: absolute;top: 80px;left: 405px;| &&
                              |font-size: 12.0pt;font-family: Candara, sans-serif;color:black;'>| &&
                              |<br>Dear { condense( lv_salutation ) },| &&
                              |<br>| &&
                              |<br>We are pleased to welcome you to Indofil family.| &&
                              |<br>| &&
                              |<br>To help you acquaint better with K K Modi group and| &&
                              |<br>Indofil, please go through the ‘Induction Module’| &&
                              |<br>by clicking on the below link.| &&
                              |<br>| &&
                              |<br><a href='' style='color: blue'>Click Here to start</a>| &&
                              |<br>| &&
                              |<br>The module is for an hour and a half and we suggest| &&
                              |<br>that you go through the same at one stretch.| &&
                              |<br>| &&
                              |<br>Feedback/suggestions are most welcome, so please| &&
                              |<br>do write back to us.| &&
                              |<br> | &&
                              |<br>Best wishes for a great career journey at Indofil.| &&
                              |<br>| &&
                              |<br>| &&
                              |<strong>Regards,| &&
                              |<br>| &&
                              |<br>Sonal Raj| &&
                              |<br>Sr. GM – People Strategy| &&
                              |</strong>| &&
                              |</div>|.

    check lv_html_for_image is not initial.
    data: begin of ls_data,
            html         type string,
            google_fonts type string,
          end of ls_data.

    constants: lc_font type string value 'Candara'.

    ls_data-html = lv_html_for_image.
    ls_data-google_fonts = 'Candara'.

    data(lv_json) = /ui2/cl_json=>serialize(
      exporting
        data        = ls_data
        pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    check lv_json is not initial.
    constants: lc_url type string value 'https://hcti.io/v1/image'.
    data: lv_username type string,
          lv_password type string.

    clear: lv_username, lv_password.

    data: lv_month_start type d,
          lv_month_end   type d.

    clear: lv_month_start, lv_month_end.
    call function 'HR_JP_MONTH_BEGIN_END_DATE'
      exporting
        iv_date             = conv d( sy-datum )
      importing
        ev_month_begin_date = lv_month_start
        ev_month_end_date   = lv_month_end.

    select count( distinct url )
      from zhr_t_induct
      into @data(lv_count)
      where cr_date between @lv_month_start and @lv_month_end
      and   username = 'd61dfaa2-435d-40b5-bb4f-257d0d59e43d'.  " hohelpdesk-icc@modi.com

    if lv_count < 50. " 50 free renders per month for each account
      lv_username = 'd61dfaa2-435d-40b5-bb4f-257d0d59e43d'.
      lv_password = 'b110f73a-02d0-4103-b668-f79042687b32'.
    else.

      clear lv_count.
      select count( distinct url )
        from zhr_t_induct
        into lv_count
        where cr_date between lv_month_start and lv_month_end
        and   username = '57eb0908-f782-43bc-a89b-ec3183994db1'.  " saphelpdesk-icc@modi.com

      if lv_count < 50. " 50 free renders per month for each account
        lv_username = '57eb0908-f782-43bc-a89b-ec3183994db1'.
        lv_password = 'be088657-4210-470f-a126-61178e4de16c'.
      endif.
    endif.

    call method cl_http_client=>create_by_url
      exporting
        url                = lc_url
      importing
        client             = data(lo_http_client)
      exceptions
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        others             = 4.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    if lo_http_client is bound.
      lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
      lo_http_client->authenticate(
        exporting
          username = lv_username
          password = lv_password ).

      try.
          data(lv_payload) = cl_bcs_convert=>string_to_xstring( exporting iv_string = conv #( lv_json ) ).
        catch cx_bcs.
      endtry.

      check lv_payload is not initial.
      lo_http_client->request->set_method( 'POST' ).
      lo_http_client->request->set_content_type( 'application/json' ).
      lo_http_client->request->set_data( lv_payload ).

      lo_http_client->send(
        exceptions
          http_communication_failure = 1
          http_invalid_state         = 2 ).

      check sy-subrc = 0.

      lo_http_client->receive(
        exceptions
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3 ).

      check sy-subrc = 0.

      data(lv_response) = lo_http_client->response->get_cdata( ).

      check lv_response is not initial and lv_response cs 'url'.
      data: begin of ls_response,
              url type string,
            end of ls_response.

      /ui2/cl_json=>deserialize(
        exporting
          json        = conv #( lv_response )
          pretty_name = /ui2/cl_json=>pretty_mode-low_case
        changing
          data        = ls_response ).

      check ls_response is not initial.
      " maintain api log table
      data(ls_hr_induct) = value zhr_t_induct(
                                  username = condense( lv_username )
                                  url = condense( ls_response-url )
                                  cr_date = sy-datum ).
      if ls_hr_induct is not initial.
        insert into zhr_t_induct values ls_hr_induct.
        if sy-dbcnt = 1.
          commit work.
        endif.
      endif.
      data(lv_response_url) = ls_response-url && '.png'.
      data(lv_html_for_body) = |<html>| &&
                               |<body>| &&
                               |<a style="text-decoration: none; color: inherit;" | &&
                               |href = "https://indofilcc.com/wp-content/uploads/2016/Indofil-Induction-Gold_07-Oct-2016/index.html">| &&
                               |<img style='display: block; margin-left: auto; margin-right: auto' | &&
                               |src='{ lv_response_url }' alt='induction' width="800" height="600"/>| &&
*                               |src='data:image/png;base64,{ zcl_helper=>build_base64_from_url( iv_url = lv_response_url ) }' | &&
*                               |alt='induction' width="800" height="600"/>| &&
                               |</a>| &&
                               |</body>| &&
                               |</html>|.

      check lv_html_for_body is not initial.
      refresh rt_body.
      rt_body = cl_document_bcs=>string_to_soli( exporting ip_string = conv #( lv_html_for_body ) ).
    endif.
  endmethod.


  method calc_po_item_tax.
*{   INSERT         SBXK900267                                        1
* code adopted from kond_taxes( mm06efko_kond_taxes )
    data: ls_ekpo type ekpo.

    clear ls_ekpo.
    ls_ekpo = is_ekpo.  " IHDK900056
*  call function 'ME_EKPO_SINGLE_READ'
*    exporting
*      pi_ebeln                  = iv_ebeln
*      pi_ebelp                  = iv_ebelp
*    importing
*      po_ekpo                   = ls_ekpo
*    exceptions
*      no_records_found          = 1
*      others                    = 2
*            .
*  if sy-subrc <> 0.
**   Implement suitable error handling here
*  endif.

    data: ls_ekko type ekko.

    clear ls_ekko.
    ls_ekko = is_ekko.  " IHDK900056
*  call function 'ME_EKKO_SINGLE_READ'
*    exporting
*      pi_ebeln                  = iv_ebeln
*    importing
*      po_ekko                   = ls_ekko
*    exceptions
*      no_records_found          = 1
*      others                    = 2
*            .
*  if sy-subrc <> 0.
**   Implement suitable error handling here
*  endif.

    check ls_ekko is not initial and ls_ekpo is not initial.
    data: ls_t001 type t001,
          ls_t005 type t005.
*   Fill t00* here
    clear: ls_t001, ls_t005.
    select single * from t001 into ls_t001 where bukrs eq ls_ekpo-bukrs.
    if ls_t001-land1 is not initial.
      select single * from t005 into ls_t005 where land1 eq ls_t001-land1.
    endif.

    call function 'FIND_TAX_SPREADSHEET'
      exporting
        buchungskreis = ls_ekpo-bukrs
      importing
        schema        = ls_t005-kalsm.

    data: ls_taxcom type taxcom.
    constants: bstyp_info value 'I',
               bstyp_ordr value 'W',
               bstyp_banf value 'B',
               bstyp_best value 'F',
               bstyp_anfr value 'A',
               bstyp_kont value 'K',
               bstyp_lfpl value 'L',
               bstyp_lerf value 'Q'.

    clear ls_taxcom.
    check ls_ekpo-mwskz is not initial.
    ls_taxcom-bukrs = ls_ekpo-bukrs.
    ls_taxcom-budat = ls_ekko-bedat.
    ls_taxcom-waers = ls_ekko-waers.
    ls_taxcom-kposn = ls_ekpo-ebelp.
    ls_taxcom-mwskz = ls_ekpo-mwskz.
    ls_taxcom-txjcd = ls_ekpo-txjcd.
    ls_taxcom-ebeln = ls_ekpo-ebeln.                        "N1427028
    ls_taxcom-ebelp = ls_ekpo-ebelp.                        "N1427028
    ls_taxcom-shkzg = 'H'.
    ls_taxcom-xmwst = 'X'.
    if ls_ekko-bstyp eq bstyp_best.
      ls_taxcom-wrbtr = ls_ekpo-netwr.
    else.
      ls_taxcom-wrbtr = ls_ekpo-zwert.
    endif.
    ls_taxcom-lifnr = ls_ekko-lifnr.
*   removing to fill taxcom-land1 using note 2348893
    ls_taxcom-ekorg = ls_ekko-ekorg.
    ls_taxcom-hwaer = ls_t001-waers.
    ls_taxcom-llief = ls_ekko-llief.
    ls_taxcom-bldat = ls_ekko-bedat.
    ls_taxcom-matnr = ls_ekpo-matnr.         "HTN-Abwicklung
    ls_taxcom-werks = ls_ekpo-werks.
    ls_taxcom-bwtar = ls_ekpo-bwtar.
    ls_taxcom-matkl = ls_ekpo-matkl.
    ls_taxcom-meins = ls_ekpo-meins.
*- Mengen richtig fuellen ---------------------------------------------*
    if ls_ekko-bstyp eq bstyp_best.
      ls_taxcom-mglme = ls_ekpo-menge.
    else.
      if ls_ekko-bstyp eq bstyp_kont and ls_ekpo-abmng gt 0.
        ls_taxcom-mglme = ls_ekpo-abmng.
      else.
        ls_taxcom-mglme = ls_ekpo-ktmng.
      endif.
    endif.
    if ls_taxcom-mglme eq 0.  "falls keine Menge gesetzt --> auf 1 setzen
      ls_taxcom-mglme = 1000.  "z.B. bestellte Banf nochmal bestellt
    endif.
    ls_taxcom-mtart = ls_ekpo-mtart.

* localisation for India
    call function 'J_1BSA_COMPONENT_ACTIVE'
      exporting
        bukrs                = ls_t001-bukrs
        component            = 'IN'
      exceptions
        component_not_active = 1
        others               = 2.

    if sy-subrc = 0.

      call function 'SAP_TO_ISO_COUNTRY_CODE'
        exporting
          sap_code    = ls_t001-land1
        importing
          iso_code    = ls_t005-intca
        exceptions
          not_found   = 1
          no_iso_code = 2
          others      = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      data: me_copypo type ref to if_ex_me_cin_mm06efko.
      call method cl_exithandler=>get_instance
        exporting
          exit_name              = 'ME_CIN_MM06EFKO'
          null_instance_accepted = 'X'
        changing
          instance               = me_copypo.

      if not me_copypo is initial.
        call method me_copypo->me_cin_copy_po_data
          exporting
            flt_val = ls_t005-intca
            y_ekpo  = ls_ekpo.
      endif.

*** GST India ---------------------------------------------------------------*
***Check if Company Code Country is India
      call function 'J_1BSA_COMPONENT_ACTIVE'
        exporting
          bukrs                = ls_t001-bukrs
          component            = 'IN'
        exceptions
          component_not_active = 1
          others               = 2.
      if sy-subrc eq 0.
        data:im_gst_rele type char1.
        clear im_gst_rele.
        call function 'J_1IG_DATE_CHECK'
          importing
            ex_gst_rele = im_gst_rele.
        if im_gst_rele eq 'X'.
*          PASS EKKO and EKPO to pricing
          call function 'J_1IG_PASS_EKKO_EKPO'
            exporting
              im_ekko = ls_ekko
              im_ekpo = ls_ekpo.
*** Begin of note 2444868
          data lo_gst_service_purchasing type ref to j_1icl_gst_service_purchasing.
          lo_gst_service_purchasing = j_1icl_gst_service_purchasing=>get_instance( ).
          lo_gst_service_purchasing->extend_mm06efko_kond_taxes( ls_ekpo ).
*** End of note 2444868
        endif.
      endif.
*** GST India ---------------------------------------------------------------*

      call function 'CALCULATE_TAX_ITEM'
        exporting
          i_taxcom        = ls_taxcom
          display_only    = 'X'
          dialog          = space
        importing
          e_taxcom        = ls_taxcom
        tables
          t_xkomv         = et_taxes
        exceptions
          mwskz_not_valid = 1                                 "N1553099
          txjcd_not_valid = 2                                 "N1553099
          others          = 3.
    endif.
*}   INSERT
  endmethod.


  method check_file_exists.
    clear:
     rv_exists,
     ev_message.

    if iv_filepath is initial.
      ev_message = 'Please supply filepath to check'.
      return.
    endif.

    " 1. check whether the file is an app server file
    try.
        open dataset iv_filepath for input in binary mode.
        case sy-subrc.
          when 0.
            rv_exists = abap_true.
            try.
                close dataset iv_filepath.
              catch cx_sy_file_close into data(lox_file_close).
                ev_message = lox_file_close->get_text( ).
            endtry.
            return.
          when others.
        endcase.
      catch cx_sy_file_open into data(lox_file_open).
        ev_message = lox_file_open->get_text( ).
      catch cx_sy_file_authority into data(lox_file_authority).
        ev_message = lox_file_authority->get_text( ).
      catch cx_sy_too_many_files into data(lox_file_too_many_files).
        ev_message = lox_file_too_many_files->get_text( ).
    endtry.

    " 2. check whether the file is a front end file
    if sy-batch = abap_false.
      cl_gui_frontend_services=>file_exist(
        exporting
          file                 = iv_filepath   " File to Check
        receiving
          result               = rv_exists
        exceptions
          cntl_error           = 1      " Control error
          error_no_gui         = 2      " Error: No GUI
          wrong_parameter      = 3      " Incorrect parameter
          not_supported_by_gui = 4      " GUI does not support this
          others               = 5 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into ev_message.
      endif.
    endif.
  endmethod.


  method check_file_format.
    check ctab is not initial.
    data: lo_struct type ref to cl_abap_structdescr,
          lo_table  type ref to cl_abap_tabledescr.

    free: lo_table, lo_struct.
    lo_table  ?=  cl_abap_structdescr=>describe_by_data( ctab[] ).
    lo_struct ?=  lo_table->get_table_line_type( ).
    data(lt_comp) = lo_struct->components.

*    loop at lt_comp into ls_comp where as_include = abap_true.
*      try.
*          lo_struct ?= ls_comp-type.
*          append lines of lo_struct->components to lt_comp.
*        catch cx_sy_move_cast_error ##no_handler.
*      endtry.
*      clear ls_comp.
*    endloop.
*
*    delete lt_comp where as_include = abap_true.

    " support for hashed tables
    loop at ctab assigning field-symbol(<table_line>).
      exit.
    endloop.
*    read table ctab assigning field-symbol(<table_line>) index 1.
*    if sy-subrc = 0.
      do.
        assign component sy-index of structure <table_line> to field-symbol(<fs>).
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs> is assigned.
          condense <fs>.
          <fs> = shift_left( val = <fs> sub = '' ).

          read table lt_comp into data(ls_comp) index sy-index.
          if sy-subrc = 0 and to_upper( ls_comp-name ) <> to_upper( <fs> ).
            message ls_comp-name && ': Field sequence altered. Please check your file format' type 'S' display like 'E'.
            raise file_format_altered.
          endif.
        endif.
        unassign <fs>.
        clear ls_comp.
      enddo.
*    endif.
    delete table ctab from <table_line>.
  endmethod.


  method clear_x_fields.
    check cs_data is not initial.
    field-symbols: <flt> type any table.
    constants: lc_x_type type string value 'BAPIUPDATE'.

    case type of cl_abap_typedescr=>describe_by_data( exporting p_data = cs_data ).
      when type cl_abap_elemdescr into data(lo_elemdescr).
        if lo_elemdescr->type_kind = cl_abap_typedescr=>typekind_char
          and lo_elemdescr->get_relative_name( ) = lc_x_type.
          clear cs_data.
        endif.
      when type cl_abap_structdescr into data(lo_structdescr).
        if lo_structdescr->type_kind = cl_abap_typedescr=>typekind_struct1
          or lo_structdescr->type_kind = cl_abap_typedescr=>typekind_struct2.
          do.
            assign component sy-index of structure cs_data to field-symbol(<fs>).
            if sy-subrc <> 0.
              exit.
            endif.
            clear_x_fields( changing cs_data = <fs> ).
          enddo.
        endif.
      when type cl_abap_tabledescr into data(lo_tabledescr).
        if lo_tabledescr->type_kind = cl_abap_typedescr=>typekind_table.
          assign cs_data to <flt>.
          loop at <flt> assigning field-symbol(<fls>).
            clear_x_fields( changing cs_data = <fls> ).
          endloop.
        endif.
      when others.
    endcase.
  endmethod.


  method condense_data.
    field-symbols: <flt> type any table.
    check cs_data is not initial.

    data(lo_descr) = cl_abap_typedescr=>describe_by_data( exporting p_data  = cs_data ).
    if lo_descr->type_kind eq cl_abap_typedescr=>typekind_char
      or lo_descr->type_kind = cl_abap_typedescr=>typekind_string
      or lo_descr->type_kind = cl_abap_typedescr=>typekind_clike
      or lo_descr->type_kind = cl_abap_typedescr=>typekind_csequence.
      cs_data = condense( cs_data ).
      cs_data = shift_left( val = cs_data sub = ' ' ).
    endif.
    if lo_descr->type_kind eq cl_abap_typedescr=>typekind_table.
      assign cs_data to <flt>.
      loop at <flt> assigning field-symbol(<fls>).
        condense_data( changing cs_data = <fls> ).
      endloop.
    endif.
    if lo_descr->type_kind eq cl_abap_typedescr=>typekind_struct1.
      do.
        assign component sy-index of structure cs_data to field-symbol(<fs>).
        if sy-subrc <> 0.
          exit.
        endif.
        condense_data( changing cs_data = <fs> ).
      enddo.
    endif.
  endmethod.


  method conv_exit.
    check iv is not initial.
    data: lv type ref to data.
    data(lo_data) = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( iv ) ).
    check lo_data is bound.
    create data lv type handle lo_data.
    assign lv->* to field-symbol(<fs_iv>).
    assign ev to field-symbol(<fs_ev>).
    check <fs_iv> is assigned and <fs_ev> is assigned.
    <fs_iv> = iv.

    data(ls_iv) = conv string( <fs_iv> ).
    data(lo_helper) = new lcl_helper( ).

    if iv_mode eq 'I'.
      data(lo_elem) = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <fs_iv> ) ).
      if lo_elem is bound and ( lo_elem->type_kind eq 'C' or lo_elem->type_kind eq 'g' ). " char or string
        <fs_iv> = condense( |{ shift_left( val = <fs_iv> sub = ' ' ) }| ).
        try.  " for date conversion
            if strlen( <fs_iv> ) eq 10 and
              ( ( count( val = <fs_iv> sub = '.' ) eq 2 ) or ( count( val = <fs_iv> sub = '/' ) eq 2 ) or ( count( val = <fs_iv> sub = '-' ) eq 2 ) ).
              replace all occurrences of '/' in <fs_iv> with '.'.
              replace all occurrences of '-' in <fs_iv> with '.'.
              data lv_date type sy-datum.
              clear lv_date.
              call function 'CONVERT_DATE_INPUT'
                exporting
                  input                     = <fs_iv>
                  plausibility_check        = abap_true
                importing
                  output                    = lv_date
                exceptions
                  plausibility_check_failed = 1
                  wrong_format_in_input     = 2
                  others                    = 3.
              if sy-subrc <> 0.
* Implement suitable error handling here
              endif.
              <fs_ev> = lv_date.
              if <fs_ev> is not initial.
                return. " supplied input is a date, processing should end here
              endif.
            endif.

            if cl_abap_typedescr=>describe_by_data( exporting p_data = <fs_ev> )->type_kind = cl_abap_typedescr=>typekind_date.
              lo_helper->excel_date_to_sap_date( changing cv_excel_date = ls_iv ).
              <fs_ev> = ls_iv.
              if <fs_ev> is not initial.
                return. " supplied input is a date, processing should end here
              endif.
            endif.

            if strlen( <fs_iv> ) eq 8 and
            ( ( count( val = <fs_iv> sub = ':' ) eq 2 ) or ( count( val = <fs_iv> sub = '.' ) eq 2 ) ).
              replace all occurrences of '.' in <fs_iv> with ':'.
              data lv_time type sy-uzeit.
              clear lv_time.
              call function 'CONVERT_TIME_INPUT'
                exporting
                  input                     = <fs_iv>
                  plausibility_check        = abap_true
                importing
                  output                    = lv_time
                exceptions
                  plausibility_check_failed = 1
                  wrong_format_in_input     = 2
                  others                    = 3.
              if sy-subrc <> 0.
* Implement suitable error handling here
              endif.
              <fs_ev> = lv_time.
              if <fs_ev> is not initial.
                return. " supplied input is a date, processing should end here
              endif.
            endif.

            if cl_abap_typedescr=>describe_by_data( exporting p_data = <fs_ev> )->type_kind = cl_abap_typedescr=>typekind_time.
              lo_helper->excel_time_to_sap_time( changing cv_excel_time = ls_iv ).
              <fs_ev> = ls_iv.
              if <fs_ev> is not initial.
                return. " supplied input is a time, processing should end here
              endif.
            endif.
          catch cx_sy_range_out_of_bounds.
            return.
          catch cx_sy_regex_too_complex.
            return.
          catch cx_sy_strg_par_val.
            return.
        endtry.
      endif.
    endif.

    if iv_mode eq 'O'.
      lo_elem = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <fs_iv> ) ).
      if lo_elem is bound." and lo_elem->type_kind eq 'D'.  " date could be supplied in non-date field
        clear lv_date.
        lv_date = cond #( when lo_elem->type_kind eq 'D' or lo_elem->type_kind eq 'C' or lo_elem->type_kind eq 'g' then <fs_iv> ).
        if lv_date is not initial.  " or <fs_iv> = '00000000'.
          call function 'CONVERT_DATE_TO_EXTERNAL'
            exporting
              date_internal            = lv_date
            importing
              date_external            = <fs_ev>
            exceptions
              date_internal_is_invalid = 1
              others                   = 2.
          if sy-subrc <> 0.
* Implement suitable error handling here
          endif.
          if <fs_ev> is not initial.
            return. " date conversion was successful, end processing
          endif.
        endif.

        clear lv_time.
        lv_time = cond #( when lo_elem->type_kind eq 'T' or lo_elem->type_kind eq 'C' or lo_elem->type_kind eq 'g' then <fs_iv> ).
        if lv_time is not initial.  " or <fs_iv> = '000000'.
          try.
              cl_abap_timefm=>conv_time_int_to_ext(
                exporting
                  time_int            = lv_time              " Type T for Internal Time Format
                importing
                  time_ext            = data(lv_time_out) ).            " External Represenation of Time

              <fs_ev> = lv_time_out.

              if <fs_ev> is not initial.
                return. " time conversion was successful, end processing
              endif.
            catch cx_parameter_invalid_range. " Parameter with invalid value range
          endtry.
        endif.
      endif.
    endif.

    " for non-date fields
    if iv_mode eq 'I'.
      data(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = <fs_ev> ).
    endif.
    if iv_mode eq 'O'.
      lo_type = cl_abap_typedescr=>describe_by_data( p_data = <fs_iv> ).
    endif.

    if lo_type is bound.
      data(lv_relative_name) = lo_type->get_relative_name( ).

      if lv_relative_name is not initial.
        data: lv_data_element type dd04l-rollname.
        clear: lv_data_element.
        move lv_relative_name to lv_data_element.
        select single *
          from dd04l
          into @data(wa_dd04l)
          where rollname = @lv_data_element.

        if wa_dd04l-convexit is not initial.
          if iv_mode eq 'I'.
            concatenate 'CONVERSION_EXIT_' wa_dd04l-convexit '_INPUT' into data(lv_function).
          endif.
          if iv_mode eq 'O'.
            concatenate 'CONVERSION_EXIT_' wa_dd04l-convexit '_OUTPUT' into lv_function.
          endif.

          call function 'FUNCTION_EXISTS'
            exporting
              funcname           = conv rs38l-name( lv_function )
            exceptions
              function_not_exist = 1
              others             = 2.
          if sy-subrc = 0.
            try.
                call function lv_function
                  exporting
                    input  = <fs_iv>
                  importing
                    output = <fs_ev>
                  exceptions
                    others = 1.
              catch cx_sy_dyn_call_illegal_func.
                " catch-block
                return.
            endtry.
          endif.
        endif.
      endif.
    endif.
  endmethod.


  method conv_spool_list_2_alv.
*{   INSERT         SBXK900102                                        1
    type-pools: slis.

    data: begin of gstr_data,
            data(2048) type c,
          end of gstr_data,
          gtbl_data like standard table of gstr_data.
    data: begin of gstr_col,
            col(255) type c,
          end of gstr_col,
          gtbl_col like standard table of gstr_col.

    data gv_line type i.
    data gv_length type i.
    data gv_cnt type i.
    data gv_dec type i.
    data gv_row(2048) type c. "string.
    data gv_row_d(2048) type c. "string.
    data gv_spoolnum type tsp01-rqident.

    data gtbl_fieldcat type  slis_t_fieldcat_alv.
    data gstr_fieldcat like line of gtbl_fieldcat.
    data gstr_data_d like line of gtbl_data.
    data gtbl_match type match_result_tab.
    data gtbl_match_last type match_result_tab.
    data gtbl_fcat type lvc_t_fcat.
    data gstr_fcat like line of gtbl_fcat.

    data gref_data type ref to data.
    data gref_new_line type ref to data.

    field-symbols: <fs_data> type ref to data.
    field-symbols: <fs_dyntable> type standard table.
    field-symbols: <fs_match> like line of gtbl_match.
    field-symbols: <fs_dynline> type any.
    field-symbols: <fs_dynstruct> type any.

*******************************************************************************
    " Read the spool
    clear gv_spoolnum.
    refresh gtbl_data.
    gv_spoolnum = i_spooln.
    call function 'RSPO_RETURN_ABAP_SPOOLJOB'
      exporting
        rqident = gv_spoolnum
      tables
        buffer  = gtbl_data.

*******************************************************************************
    " delete header if any
    clear gstr_data.
    read table gtbl_data into gstr_data index 1.
    while not gstr_data-data(1) eq '-'.
      clear gstr_data.
      delete gtbl_data index 1.
      read table gtbl_data into gstr_data index 1.
    endwhile.

    " Pre-check to make sure we have a list
    " simply checking the first char of the
    " first and the last line
    clear: gstr_data, gv_line.
    read table gtbl_data into gstr_data index 1.
    if sy-subrc = 0.
      if gstr_data-data(1) <> '-'.
        message 'Spool does not contain any ALV list' type 'E'.
      else.
        " Number of rows
        gv_line = lines( gtbl_data ).

        " Read the last line
        clear gstr_data.
        read table gtbl_data into gstr_data index gv_line.
        if sy-subrc = 0.
          if gstr_data-data(1) <> '-'.
            message 'Spool does not contain any ALV list' type 'E'.
          endif.
        endif.
      endif.
    endif.

*******************************************************************************
    " Start with the N table in the spool.
    if i_start_table > 1.
      free: gv_line, gv_dec.

      gv_dec = i_start_table.
      subtract 1 from gv_dec.
      gv_dec = gv_dec * 2.

      " Find the end of the table N - 1
      clear gstr_data.
      loop at gtbl_data into gstr_data where data(1) = '-'.
        add 1 to gv_line.

        if gv_line = gv_dec.
          gv_dec = sy-tabix.

          exit.
        endif.
        clear: gstr_data.
      endloop.

      " Delete the rows up table N
      delete gtbl_data from 1 to gv_dec.
    endif.

*******************************************************************************
    " Check how many ALV list are in the spool
    " and make sure if more than one the # of columns are matching
    free: gv_line, gstr_data.
    loop at gtbl_data into gstr_data where data(1) = '-'.
      add 1 to gv_line.
      clear: gv_cnt.
      gv_cnt = gv_line mod 2.

      " The column headings are on odd number in our find counter
      if gv_cnt <> 0.
        " Save the find counter value
        gv_cnt = gv_line.

        " Update index to point to column heading row and read
        gv_line = sy-tabix + 1.
        clear gstr_data.
        read table gtbl_data index gv_line into gstr_data.
        gv_row = gstr_data-data.

        " Find the columns: position and length
        find all occurrences of '|' in gv_row results gtbl_match.

        " Compare the previous heading w/ current
        if gtbl_match[] <> gtbl_match_last[] and sy-tabix > 2.
          message 'Spool contains more than one ALV list where column headings are different.' type 'E'.
        endif.

        free: gtbl_match_last, gv_row.
        gtbl_match_last[] = gtbl_match[].
        free gtbl_match.

        " Get back the find counter value
        gv_line = gv_cnt.
      endif.
      clear gstr_data.
    endloop.

*******************************************************************************
    " Read column heading row
    clear: gstr_data, gv_row.
    read table gtbl_data index 2 into gstr_data.
    gv_row = gstr_data-data.

    " Read also the first data row
    " Here we are assuming tha the first row all fields
    " filled in; will use this to determine mumeric or char
    clear: gstr_data_d, gv_row_d.
    read table gtbl_data index 4 into gstr_data_d.
    gv_row_d = gstr_data_d-data.

    " Find out the columns
    refresh gtbl_match.
    find all occurrences of '|' in gv_row results gtbl_match.

    free: gv_cnt, gv_line.

    replace first occurrence of '|' in gstr_data-data with space.
    refresh gtbl_col.
    split gstr_data at '|' into table gtbl_col.

    data: prev_length like gv_length.
    " Setup the field catalog for itab
    refresh: gtbl_fcat, gtbl_fieldcat.
    loop at gtbl_match assigning <fs_match>.
      if sy-tabix > 1.
        " Field length
        " Start position of next column
        gv_line = <fs_match>-offset + 1.
        gv_length = ( gv_line + 1 ) - prev_length.
        prev_length = prev_length + gv_length.

        " Update counter used for column heading
        add 1 to gv_cnt.

        read table gtbl_col into gstr_col index gv_cnt.
        if sy-subrc = 0.
          condense gstr_col-col.
          replace all occurrences of regex '[^a-zA-Z0-9 :]' in gstr_col-col with ''.  " special characters
          replace all occurrences of ` ` in gstr_col-col with '_'.

          shift gstr_col-col right deleting trailing '_'.
          condense gstr_col-col.
        endif.

        " Used for dynamic itab
        gstr_fcat-datatype = 'C'.
        gstr_fcat-fieldname = gstr_col-col && '_' && gv_cnt.
        gstr_fcat-intlen = gv_length.
        gstr_fcat-tabname = ''.

        " Debug and you will see why...
        subtract 1 from gv_length.

        " Used for ALV grid
        gstr_fieldcat-reptext_ddic = gstr_col-col && '_' && gv_cnt.
        gstr_fieldcat-tabname = ''.
        gstr_fieldcat-fieldname = gstr_col-col && '_' && gv_cnt.
        gstr_fieldcat-just = 'L'.
        gstr_fieldcat-outputlen = gv_length.

        append gstr_fcat to gtbl_fcat.
        append gstr_fieldcat to gtbl_fieldcat.

        free: gstr_fcat, gstr_fieldcat, gv_dec, gv_line.
      endif.

    endloop.

    assign gref_data to <fs_data>.

*******************************************************************************
    " Create a dynamic table based on the number of columns above
    call method cl_alv_table_create=>create_dynamic_table
      exporting
        it_fieldcatalog           = gtbl_fcat
      importing
        ep_table                  = <fs_data>
      exceptions
        generate_subpool_dir_full = 1
        others                    = 2.

    assign <fs_data>->* to <fs_dyntable>.

    " Create a new mem area
    create data gref_new_line like line of <fs_dyntable>.

    " Now point our <FS_*> to the mem area
    assign gref_new_line->* to <fs_dynstruct>.
    assign gref_new_line->* to <fs_dynline>.

*******************************************************************************
    " Remove column headings that appears in the middle
    " which are caused due to spool page-break
    loop at gtbl_data into gstr_data_d from 4 where data = gstr_data-data or data = gv_row.
      delete gtbl_data.
      clear gstr_data_d.
    endloop.

*******************************************************************************
    " Push data to itab
    loop at gtbl_data into gstr_data.
      " The first 3 rows are col heading related
      if sy-tabix > 3 and (
      gstr_data-data(2) <> '|-' and " Column heading row
      gstr_data-data(2) <> '--'     " End of list row
            ).

        refresh gtbl_col.
        replace first occurrence of '|' in gstr_data-data with space.
        split gstr_data at '|' into table gtbl_col.

        gv_cnt = 0.

        " Split above makes each column to a row
        " Get each column
        loop at gtbl_col into gstr_col.
          add 1 to gv_cnt.
          assign component gv_cnt of structure <fs_dynstruct> to <fs_dynline>.
          if <fs_dynline> is assigned.
            " Remove space front/end
            condense gstr_col-col.

            move gstr_col-col to <fs_dynline>.
          endif.
          clear gstr_col.
        endloop.

        append <fs_dynstruct> to <fs_dyntable>.

        gv_cnt = 0.
        free: gstr_col.
      endif.

      free gstr_data.
    endloop.

*******************************************************************************
    " Sum line flag, keep or delete
    if i_keep_sum_line is initial.
      loop at <fs_dyntable> assigning <fs_dynline>.
        if <fs_dynline>(1) = '*'.
          delete <fs_dyntable>.
        endif.
      endloop.
    endif.

*******************************************************************************
    " Display
    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        it_fieldcat = gtbl_fieldcat
      tables
        t_outtab    = <fs_dyntable>.
*}   INSERT
  endmethod.


  method delete_file.

    clear:
      ev_message,
      rv_deleted.

    data(lv_filepath) = iv_filepath.
    data(lo_helper) = new lcl_helper( ).

    if lv_filepath is initial.
      ev_message = 'Please supply path of file to delete on app server'.
      return.
    endif.

    rv_deleted = xsdbool( check_file_exists( exporting iv_filepath = lv_filepath ) = abap_false ).

    if rv_deleted = abap_true.
      return.
    endif.

* ---- Compute filepath type ---- *
    data(lv_file_path_type) = cond #( when lv_filepath ca gc_path_sep-windows then gc_path_sep-windows
                                      when lv_filepath ca gc_path_sep-unix then gc_path_sep-unix ).

    case lv_file_path_type.
      when gc_path_sep-windows.
        if sy-batch = abap_true.
          ev_message = 'GUI not available in background mode'.
          return.
        endif.
        if lo_helper is bound.
          lo_helper->delete_temp_file( exporting iv_temp_file = lv_filepath ).
          rv_deleted = xsdbool( check_file_exists( exporting iv_filepath = lv_filepath ) = abap_false ).
        endif.
      when gc_path_sep-unix.
        data(lv_program) = conv authb-program( sy-cprog ).
        data(lv_auth_filename) = conv authb-filename( lv_filepath ).
        call function 'AUTHORITY_CHECK_DATASET'
          exporting
            program          = lv_program  " ABAP program in which access occurs
            activity         = sabc_act_delete " Access Type (See Function Documentation)
            filename         = lv_auth_filename " File name
          exceptions
            no_authority     = 1        " You are not authorized for this access
            activity_unknown = 2        " Access type unknown
            others           = 3.
        if sy-subrc <> 0.
          case sy-subrc.
            when 1.
              ev_message = 'Not authorised to delete the file from app server'.
            when others.
              ev_message = 'File open error'.
          endcase.
          return.
        endif.
        try.
            delete dataset lv_filepath.
            case sy-subrc.
              when 0.
                rv_deleted = abap_true.
                rv_deleted = xsdbool( check_file_exists( exporting iv_filepath = lv_filepath ) = abap_false ).
              when others.
                ev_message = 'File could not be deleted.'.
            endcase.
          catch cx_sy_file_authority into data(lox_file_authority).
            ev_message = lox_file_authority->get_text( ).
          catch cx_sy_file_open into data(lox_file_open).
            ev_message = lox_file_open->get_text( ).
        endtry.
      when others.
    endcase.
  endmethod.


  method display_exception.
    data(ls_msg) = is_msg.
    if ls_msg-msgty is initial.
      ls_msg-msgty = 'S'.
    endif.
    " single message
    if ls_msg-msgid is not initial and ls_msg-msgno is not initial and ls_msg-msgty is not initial.
      message id ls_msg-msgid type ls_msg-msgty number ls_msg-msgno
        with ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
        display like iv_disp_like.
      " non-class based exception thrown from a class
    elseif iv_class is not initial and iv_method is not initial and iv_subrc is not initial.
      try.
          data(lt_exceptions) = new cl_oo_class(
                                     clsname = iv_class
                                     with_inherited_components = seox_true
                                     with_interface_components = seox_true )->get_component_signature(
                                                                                exporting
                                                                                  cpdname = iv_method )-exceps.

          try.
              data(lv_exception) = lt_exceptions[ editorder = iv_subrc ]-sconame.
              message |{ iv_class } - { iv_method } : { lv_exception }| type ls_msg-msgty display like iv_disp_like.
            catch cx_sy_itab_line_not_found into data(lox_line_not_found) ##NEEDED.
              message id 'Z_MESSAGE' type ls_msg-msgty number '009'
                with iv_class iv_method display like iv_disp_like.
          endtry.
        catch cx_class_not_existent into data(lox_class_not_existent) ##NEEDED.
          message id 'Z_MESSAGE' type ls_msg-msgty number '009'
                with iv_class iv_method display like iv_disp_like.
        catch cx_component_not_existing into data(lox_component_not_existing) ##NEEDED.
          message id 'Z_MESSAGE' type ls_msg-msgty number '009'
                with iv_class iv_method display like iv_disp_like.
      endtry.
      " exceptions from FM's
    elseif iv_fm is not initial.
      cl_fb_function_utility=>meth_get_interface(
        exporting
          im_name = iv_fm
        importing
          ex_interface = data(ls_fm_exceptions)
        exceptions
          error_occured       = 1
          object_not_existing = 2
          others              = 3 ).
      if sy-subrc <> 0.
        data(lv_subrc) = sy-subrc.

        " recursive call
        display_exception(
          exporting
            iv_class  = 'CL_FB_FUNCTION_UTILITY'
            iv_method = 'METH_GET_INTERFACE'
            iv_subrc  = lv_subrc
            is_msg    = value #( msgid = sy-msgid
                                 msgno = sy-msgno
*                                 msgty = sy-msgty
                                 msgv1 = sy-msgv1
                                 msgv2 = sy-msgv2
                                 msgv3 = sy-msgv3
                                 msgv4 = sy-msgv4 ) ).
      else.
        try.
            data(lv_fm_exception) = ls_fm_exceptions-except[ iv_subrc ]-parameter.
            message |{ iv_fm } : { lv_fm_exception }| type ls_msg-msgty display like iv_disp_like.
          catch cx_sy_itab_line_not_found into lox_line_not_found.
            message id 'Z_MESSAGE' type ls_msg-msgty number '008'
                with iv_fm display like iv_disp_like.
        endtry.
      endif.
    endif.
  endmethod.


  method dms_download_utility.
*{   INSERT         SBXK900321                                        1
    " atleast one input must be specified
    if it_document_numbers is initial and it_object_keys is initial.
      raise empty_input.
    endif.

    " this is a much better approach than a series of if else's
    data: mode_tab type standard table of boole with non-unique sorted key key components boole.
    refresh mode_tab.
    append iv_uncompressed to mode_tab.
    append iv_compressed to mode_tab.

    " allow only and atleast 1 output mode
    if lines( filter #( mode_tab using key key where boole eq abap_true ) ) > 1
          or lines( filter #( mode_tab using key key where boole eq abap_true ) ) = 0.
      raise invalid_output_mode.
    endif.

    " prompt for folder selection if not already supplied
    if iv_dwnld_folder_path is not supplied or iv_dwnld_folder_path is initial.
      data window_title    type string value 'Select output folder'.
      data selected_folder type string.
      data initial_folder type string.
      clear initial_folder.
      cl_gui_frontend_services=>get_desktop_directory(
        changing
          desktop_directory     = initial_folder
        exceptions
          cntl_error            = 1
          error_no_gui          = 2
          not_supported_by_gui  = 3
          others                = 0 ).
      cl_gui_cfw=>flush( exceptions cntl_error = 1 cntl_system_error = 2 others = 3 ).
      clear selected_folder.
      call method cl_gui_frontend_services=>directory_browse
        exporting
          window_title         = window_title
          initial_folder       = initial_folder
        changing
          selected_folder      = selected_folder
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          others               = 4.
      if sy-subrc <> 0.
*     Implement suitable error handling here
      endif.
    else.
      selected_folder = iv_dwnld_folder_path.
    endif.

    if selected_folder is initial.
      raise no_output_folder.
    endif.

    " if object keys are supplied, get relevant documents based on object keys
    if it_object_keys is not initial.
      select *
        from drad
        into table @data(lt_drad)
        where objky in @it_object_keys.

      if sy-subrc is initial and lt_drad is not initial.
        select *
          from draw
          into table @data(lt_draw)
          for all entries in @lt_drad
          where ( doknr = @lt_drad-doknr and doknr in @it_document_numbers )
          and dokar = @lt_drad-dokar
          and dokvr = @lt_drad-dokvr
          and doktl = @lt_drad-doktl
          and dokst = 'FR'. " ? only released ?
      endif.
    endif.
    " if document numbers are supplied, fetch document numbers directly
    if it_document_numbers is not initial.
      select *
        from draw
        appending table lt_draw
        where doknr in it_document_numbers
        and   dokst = 'FR'. " ? only released ?.
    endif.

    " above logic works well even if both document numbers and object keys are supplied

    if lt_draw is not initial.
      sort lt_draw ascending.
      delete adjacent duplicates from lt_draw comparing all fields. " remove duplicates incase both inputs are supplied and there's an overlap
      data(lo_dms) = new cl_cv_cpdc_dms_access( ).
      data: is_doc_key       type bapi_doc_keys,
            es_docdata       type bapi_doc_draw2,
            et_original_data type cpdc_t_original,
            es_original_data like line of et_original_data,
            et_message       type bapiret2_t,
            es_message       like line of et_message,
            rc               type i.

      " now we have all the dms documents to be processed
      if lo_dms is bound.
        loop at lt_draw into data(ls_draw).
          clear: is_doc_key, es_docdata, es_original_data, es_message.
          refresh: et_original_data, et_message.

          is_doc_key-documenttype    = ls_draw-dokar.
          is_doc_key-documentnumber  = ls_draw-doknr.
          is_doc_key-documentversion = ls_draw-dokvr.
          is_doc_key-documentpart    = ls_draw-doktl.
          call method lo_dms->get_dms_data(
            exporting
              is_doc_key       = is_doc_key
            importing
              es_docdata       = es_docdata
              et_original_data = et_original_data
              et_message       = et_message ).

          if et_original_data is not initial.
            if iv_uncompressed eq abap_true.  " one folder per dms document = may contain multiple files
              data(directory) =
                  |{ selected_folder }\\{ es_docdata-description }-{ es_docdata-documenttype }-| &&
                  |{ condense( |{ es_docdata-documentnumber alpha = out }| ) }-{ es_docdata-documentversion }-{ es_docdata-documentpart }|.
              clear rc.
              call method cl_gui_frontend_services=>directory_create
                exporting
                  directory                = directory
                changing
                  rc                       = rc
                exceptions
                  directory_create_failed  = 1
                  cntl_error               = 2
                  error_no_gui             = 3
                  directory_access_denied  = 4
                  directory_already_exists = 5
                  path_not_found           = 6
                  unknown_error            = 7
                  not_supported_by_gui     = 8
                  wrong_parameter          = 9
                  others                   = 10.
              if sy-subrc <> 0.
*             Implement suitable error handling here
                rc = sy-subrc.  " rc = 183 = subrc = 5 mean the same thing => directory_already_exists
              endif.
            elseif iv_compressed eq abap_true.  " generate one zip file per dms document = may contain multiple files
              directory = selected_folder.
              rc = 0.
              data(lo_zip) = new cl_abap_zip( ).
            endif.

            if rc = 0 or rc = 5.  " rc 5 => directory already exists
              loop at et_original_data assigning field-symbol(<fs_og_data>).
                " uncompressed -> convert xstring to binary and download to generated folder above
                if iv_uncompressed eq abap_true.
                  data(binary_data) = cl_bcs_convert=>xstring_to_solix( <fs_og_data>-contentstream-stream ).
                  data(bin_filesize) = conv i( <fs_og_data>-contentstream-length ).
                  data(filename) = |{ directory }\\{ <fs_og_data>-contentstream-filename }|.
                  call method cl_gui_frontend_services=>gui_download
                    exporting
*                     bin_filesize            = bin_filesize
                      filename                = filename
                      filetype                = 'BIN'
                    changing
                      data_tab                = binary_data
                    exceptions
                      file_write_error        = 1
                      no_batch                = 2
                      gui_refuse_filetransfer = 3
                      invalid_type            = 4
                      no_authority            = 5
                      unknown_error           = 6
                      header_not_allowed      = 7
                      separator_not_allowed   = 8
                      filesize_not_allowed    = 9
                      header_too_long         = 10
                      dp_error_create         = 11
                      dp_error_send           = 12
                      dp_error_write          = 13
                      unknown_dp_error        = 14
                      access_denied           = 15
                      dp_out_of_memory        = 16
                      disk_full               = 17
                      dp_timeout              = 18
                      file_not_found          = 19
                      dataprovider_exception  = 20
                      control_flush_error     = 21
                      not_supported_by_gui    = 22
                      error_no_gui            = 23
                      others                  = 24.
                  if sy-subrc <> 0.
*                   Implement suitable error handling here
                  endif.

                endif.
                " compressed -> add xstring to zip and download zip after loop
                if iv_compressed eq abap_true.
                  if lo_zip is bound.
                    lo_zip->add(
                      exporting
                        name    = <fs_og_data>-contentstream-filename
                        content = <fs_og_data>-contentstream-stream ).
                  endif.
                endif.
                clear: bin_filesize, filename.
                refresh: binary_data.
              endloop.

              if iv_compressed eq abap_true and lo_zip is bound.
                binary_data = cl_bcs_convert=>xstring_to_solix( lo_zip->save( ) ).
                bin_filesize = conv i( xstrlen( lo_zip->save( ) ) ).
                filename = |{ selected_folder }\\{ es_docdata-description }-{ es_docdata-documenttype }-| &&
                  |{ condense( |{ es_docdata-documentnumber alpha = out }| ) }-{ es_docdata-documentversion }-{ es_docdata-documentpart }.zip|.
                call method cl_gui_frontend_services=>gui_download
                  exporting
                    bin_filesize            = bin_filesize
                    filename                = filename
                    filetype                = 'BIN'
                  changing
                    data_tab                = binary_data
                  exceptions
                    file_write_error        = 1
                    no_batch                = 2
                    gui_refuse_filetransfer = 3
                    invalid_type            = 4
                    no_authority            = 5
                    unknown_error           = 6
                    header_not_allowed      = 7
                    separator_not_allowed   = 8
                    filesize_not_allowed    = 9
                    header_too_long         = 10
                    dp_error_create         = 11
                    dp_error_send           = 12
                    dp_error_write          = 13
                    unknown_dp_error        = 14
                    access_denied           = 15
                    dp_out_of_memory        = 16
                    disk_full               = 17
                    dp_timeout              = 18
                    file_not_found          = 19
                    dataprovider_exception  = 20
                    control_flush_error     = 21
                    not_supported_by_gui    = 22
                    error_no_gui            = 23
                    others                  = 24.
                if sy-subrc <> 0.
*                   Implement suitable error handling here
                endif.
              endif.
            endif.
          endif.

          clear: ls_draw, directory.
        endloop.
      endif.
    endif.
*}   INSERT
  endmethod.


  method excel_to_itab.
    check iv_file is not initial.
    clear ct_itab.

    assign ct_itab to field-symbol(<lt_itab>).

    data(lo_helper) = new lcl_helper( ).

    data: index       type i,
          lv_file_ext type c length 50.

*    constants: lc_xls_end_row  type i value 65536,
*               lc_xls_end_col  type i value 256,
*               lc_xlsx_end_row type i value 1048576,
*               lc_xlsx_end_col type i value 16384.

    cl_progress_indicator=>progress_indicate(
      exporting
        i_text               = |Uploading & converting excel data...|               " Progress Text (If no message transferred in I_MSG*)
        i_processed          = 1                " Number of Objects Already Processed
        i_total              = 2                " Total Number of Objects to Be Processed
        i_output_immediately = abap_true ).     " X = Display Progress Immediately

    data(lo_table)  = cast cl_abap_tabledescr( cl_abap_structdescr=>describe_by_data( <lt_itab> ) ).
    if lo_table is bound.
      data(lo_struct) = cast cl_abap_structdescr( lo_table->get_table_line_type( ) ).
    endif.
    if lo_struct is bound.
      data(lt_comp) = lo_struct->components.

*      loop at lt_comp into data(ls_comp) where as_include = abap_true.
*        try.
*            lo_struct ?= ls_comp-type.
*            append lines of lo_struct->components to lt_comp.
*          catch cx_sy_move_cast_error ##no_handler.
*        endtry.
*        clear ls_comp.
*      endloop.
*
*      delete lt_comp where as_include = abap_true.
    endif.

    data(lv_file) = conv char1024( iv_file ).
    clear lv_file_ext.
    call function 'TRINT_FILE_GET_EXTENSION'
      exporting
        filename  = lv_file
      importing
        extension = lv_file_ext.

*    data(i_begin_row) = iv_no_of_headers + 1.
*    data(i_end_row) = cond #( when lv_file_ext eq to_upper('xls')  then lc_xls_end_row
*                              when lv_file_ext eq to_upper('xlsx') then lc_xlsx_end_row ).

*    data(i_end_col) = lines( lt_comp ).
*    data(i_begin_col) = 1.

    data: lt_sheets type if_mass_spreadsheet_types=>t_spreadsheet_by_sheetname.
    clear lt_sheets.

    if lo_helper is bound.
      case to_upper( lv_file_ext ).
        when gc_extension-xls.
*      lo_helper->excel_to_itab_ole(
*        exporting
*          filename                = iv_file
*          i_begin_col             = i_begin_col
*          i_begin_row             = i_begin_row
*          i_end_col               = i_end_col
*          i_end_row               = i_end_row
*        receiving
*          intern                  = data(lt_excel)
*        exceptions
*          inconsistent_parameters = 1
*          upload_ole              = 2
*          others                  = 3 ).
          " here upload_ole exception could mean 2 things
          " 1. file in a location that requires admin rights
          " 2. end row/col beyond the max limits of the file extension, excel version

          data(lt_excel) = lo_helper->excel_to_itab_mass_spreadsheet(
                             exporting
                               iv_filename = conv #( iv_file )
                               iv_sheet_no = iv_sheet_number
                             importing
                               et_sheets   = lt_sheets ).
        when gc_extension-xlsx.
*        data: lt_tab_raw_data type truxs_t_text_data.
*
*        call function 'TEXT_CONVERT_XLS_TO_SAP'
*          exporting
*            i_field_seperator    = abap_true
*            i_line_header        = conv char01( boolc( iv_no_of_headers ge 1 ) )
*            i_tab_raw_data       = lt_tab_raw_data
*            i_filename           = iv_file
*          tables
*            i_tab_converted_data = <lt_itab>
*          exceptions
*            conversion_failed    = 1
*            others               = 2.
*        if sy-subrc <> 0.
** Implement suitable error handling here
*        endif.
          lt_excel = lo_helper->excel_to_itab_ehfnd(
                       exporting
                         iv_filename        = conv #( iv_file )
                         iv_sheet_no        = iv_sheet_number
                         iv_read_all_sheets = iv_read_all_sheets
                       importing
                         et_sheets          = lt_sheets ).
      endcase.
    endif.

    if lt_excel is not initial.
      cl_progress_indicator=>progress_indicate(
        exporting
          i_text               = 'Transposing excel data to internal table'  " Progress Text (If no message transferred in I_MSG*)
          i_processed          = 1                                      " Number of Objects Already Processed
          i_total              = 2                                      " Total Number of Objects to Be Processed
          i_output_immediately = abap_true ).                           " X = Display Progress Immediately

      delete lt_excel where row <= iv_no_of_headers.

      data lr_itab type ref to data.
      create data lr_itab like line of <lt_itab>.

      if iv_move_corresponding = abap_true.
        " check file format here

        " move corresponding implementation
        data:
          begin of ls_map,
            excel_index type i,
            itab_index  type i,
          end of ls_map,
          lt_map like sorted table of ls_map with unique key primary_key components excel_index.

        clear: lt_map.
        loop at lt_excel into data(ls_excel) where row = 1. " header row
          data(lv_col) = condense( to_upper( ls_excel-value ) ).

*        if lv_col is not initial.
          read table lt_comp into data(ls_comp) with key name = lv_col.
          if sy-subrc = 0.
            clear: ls_map.
            ls_map-excel_index = ls_excel-col.
            ls_map-itab_index = sy-tabix.
            append ls_map to lt_map.
          else.
            if iv_check_file_format = abap_true.
              message |{ lv_col }: Unknown column in excel. No corresponding field found in internal structure.| type 'I' display like 'E'.
              return.
            endif.
          endif.
*        endif.
          clear:
            ls_excel,
            ls_comp.
        endloop.

        if lt_map is not initial.
          delete lt_excel where row = 1.
        else.
          message |Excel must contain header row with column names matching internal field names.| type 'I' display like 'E'.
          return.
        endif.

        if lt_excel is not initial and lt_map is not initial.
          loop at lt_excel into ls_excel.
            at new row.
              if lr_itab is bound.
                assign lr_itab->* to field-symbol(<ls_itab>).
              endif.
              if <ls_itab> is assigned.
                clear <ls_itab>.
              endif.
            endat.
            clear index.
            try.
                index = lt_map[ key primary_key excel_index = ls_excel-col ]-itab_index.
*        move ls_excel-col to index.
                assign component index of structure <ls_itab> to field-symbol(<fv>).
                if sy-subrc = 0 and <fv> is assigned. " Incase there are more xls columns than fields
*--------------------------------------------------------------------*
                  " Date field conversion to internal format
                  if cl_abap_typedescr=>describe_by_data( exporting p_data = <fv> )->type_kind = cl_abap_typedescr=>typekind_date.
                    lo_helper->excel_date_to_sap_date( changing cv_excel_date = ls_excel-value ).
                  endif.

                  " Time field conversion to internal format
                  if cl_abap_typedescr=>describe_by_data( exporting p_data = <fv> )->type_kind = cl_abap_typedescr=>typekind_time.
                    lo_helper->excel_time_to_sap_time( changing cv_excel_time = ls_excel-value ).
                  endif.
*--------------------------------------------------------------------*
                  move ls_excel-value to <fv>.
                endif.
              catch cx_sy_itab_line_not_found ##no_handler.
            endtry.
            clear ls_excel.
            unassign <fv>.

            at end of row.
              insert <ls_itab> into table <lt_itab>.
              unassign <ls_itab>.
            endat.
          endloop.
        endif.
      else.
        " move sequential implemetation
        if lt_excel is not initial.
          loop at lt_excel into ls_excel.
            at new row.
              if lr_itab is bound.
                assign lr_itab->* to <ls_itab>.
              endif.
              if <ls_itab> is assigned.
                clear <ls_itab>.
              endif.
            endat.
            clear index.
            move ls_excel-col to index.
            assign component index of structure <ls_itab> to <fv>.
            if sy-subrc = 0 and <fv> is assigned. " Incase there are more xls columns than fields
*--------------------------------------------------------------------*
              " Date field conversion to internal format
              if cl_abap_typedescr=>describe_by_data( exporting p_data = <fv> )->type_kind = cl_abap_typedescr=>typekind_date.
                lo_helper->excel_date_to_sap_date( changing cv_excel_date = ls_excel-value ).
              endif.

              " Time field conversion to internal format
              if cl_abap_typedescr=>describe_by_data( exporting p_data = <fv> )->type_kind = cl_abap_typedescr=>typekind_time.
                lo_helper->excel_time_to_sap_time( changing cv_excel_time = ls_excel-value ).
              endif.
*--------------------------------------------------------------------*
              move ls_excel-value to <fv>.
            endif.
            clear ls_excel.
            unassign <fv>.

            at end of row.
              insert <ls_itab> into table <lt_itab>.
              unassign <ls_itab>.
            endat.
          endloop.
        endif.
      endif.

      if <lt_itab> is not initial and iv_read_all_sheets = abap_false.
        append initial line to et_excel assigning field-symbol(<ls_excel>).
        if <ls_excel> is assigned.
          try.
              data(ls_sheet) = lt_sheets[ cond #( when lines( lt_sheets ) gt 1 then iv_sheet_number else 1 ) ].
              <ls_excel> = value #( sheet_name = ls_sheet-sheetname
                                    rows       = ls_sheet-num_rows
                                    cols       = ls_sheet-num_cols
                                    data_tab   = ref #( <lt_itab> ) ).
            catch cx_sy_itab_line_not_found ##no_handler.
          endtry.
        endif.
      endif.

      if iv_read_all_sheets = abap_true and lt_sheets is not initial.
        et_excel = lo_helper->sheets_to_itabs( exporting it_sheets = lt_sheets ).
      endif.

      " check file format for move corresponding is tackled in its own block above
      if <lt_itab> is not initial and iv_check_file_format eq abap_true and iv_move_corresponding = abap_false.
        check_file_format( changing ctab = <lt_itab> exceptions file_format_altered = 1 others = 2 ).
        if sy-subrc <> 0.
          clear <lt_itab>.
        endif.
      endif.
    endif.

    if <lt_itab> is not initial and iv_with_conv_exit = abap_true.
      loop at <lt_itab> assigning <ls_itab>.
        format_excel_to_bapi(
          changing
            is_excel = <ls_itab>      " Single line of data from excel
            is_data  = <ls_itab> ).   " Single line of BAPI data
      endloop.
    endif.
  endmethod.


  method file_save_dialog.

    data:
      lv_initial_directory type string,
      lv_user_action       like cl_gui_frontend_services=>action_ok.

    clear:
      lv_user_action,
      rv_file,
      lv_initial_directory.

    if iv_initial_directory is initial.
      cl_gui_frontend_services=>get_desktop_directory(
        changing
          desktop_directory    = lv_initial_directory " Desktop Directory
        exceptions
          cntl_error           = 1                 " Control error
          error_no_gui         = 2                 " No GUI available
          not_supported_by_gui = 3                 " GUI does not support this
          others               = 4 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        cl_gui_cfw=>flush(
          exceptions
            cntl_system_error = 1 " cntl_system_error
            cntl_error        = 2 " cntl_error
            others            = 3 ).
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.
    endif.

    data(lv_filename) = value string( ).
    data(lv_path) = value string( ).
    cl_gui_frontend_services=>file_save_dialog(
      exporting
        window_title              = iv_window_title      " Window Title
        default_extension         = cond #( when iv_default_extension is not initial then iv_default_extension ) " Default Extension
        default_file_name         = cond #( when iv_default_file_name is not initial then iv_default_file_name ) " Default File Name
        file_filter               = iv_file_filter       " File Type Filter Table
        initial_directory         = cond #( when iv_initial_directory is not initial then iv_initial_directory
                                            else lv_initial_directory ) " Initial Directory " Initial Directory
        prompt_on_overwrite       = abap_true
      changing
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = rv_file          " Path + File Name
        user_action               = lv_user_action       " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
      exceptions
        cntl_error                = 1                 " Control error
        error_no_gui              = 2                 " No GUI available
        not_supported_by_gui      = 3                 " GUI does not support this
        invalid_default_file_name = 4                 " Invalid default file name
        others                    = 5 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      if lv_user_action = cl_gui_frontend_services=>action_cancel.
        message 'File selection cancelled by user' type 'S' display like 'E'.
      endif.
    endif.
  endmethod.


  method file_selection_dialog.

    data:
      lv_initial_directory type string,
      lt_files             type filetable,
      lv_rc                type i,
      lv_user_action       like cl_gui_frontend_services=>action_ok.

    clear:
      lt_files,
      lv_rc,
      lv_user_action,
      et_files,
      rv_file,
      lv_initial_directory.

    if iv_initial_directory is initial.
      cl_gui_frontend_services=>get_desktop_directory(
        changing
          desktop_directory    = lv_initial_directory " Desktop Directory
        exceptions
          cntl_error           = 1                 " Control error
          error_no_gui         = 2                 " No GUI available
          not_supported_by_gui = 3                 " GUI does not support this
          others               = 4 ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        cl_gui_cfw=>flush(
          exceptions
            cntl_system_error = 1 " cntl_system_error
            cntl_error        = 2 " cntl_error
            others            = 3 ).
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
      endif.
    endif.

    cl_gui_frontend_services=>file_open_dialog(
      exporting
        window_title            = iv_window_title      " Title Of File Open Dialog
        file_filter             = iv_file_filter       " File Extension Filter String
        initial_directory       = cond #( when iv_initial_directory is not initial then iv_initial_directory
                                          else lv_initial_directory ) " Initial Directory
        multiselection          = iv_multiselection    " Multiple selections poss.
      changing
        file_table              = lt_files        " Table Holding Selected Files
        rc                      = lv_rc           " Return Code, Number of Files or -1 If Error Occurred
        user_action             = lv_user_action  " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
      exceptions
        file_open_dialog_failed = 1                 " "Open File" dialog failed
        cntl_error              = 2                 " Control error
        error_no_gui            = 3                 " No GUI available
        not_supported_by_gui    = 4                 " GUI does not support this
        others                  = 5 ).
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      if lv_user_action = cl_gui_frontend_services=>action_cancel.
        message 'File selection cancelled by user' type 'S' display like 'E'.
      endif.
      if lv_rc ge 1 and lv_user_action = cl_gui_frontend_services=>action_ok and lt_files is not initial.
        et_files = lt_files.
        try.
            rv_file = conv #( lt_files[ 1 ]-filename ).
          catch cx_sy_itab_line_not_found ##no_handler.
        endtry.
      endif.
    endif.
  endmethod.


  method fill_x_fields.
*{   INSERT         SBXK900102                                        1
    " Local field symbols
    field-symbols: <fs_data>  type any,
                   <fs_datax> type any,
                   <fs_x>     type any,
                   <fs>       type any.

    data: lo_descr          type ref to cl_abap_typedescr.  " type info of supplied data
    data: lv_relative_name  type string.    " data element of supplied variable

    constants: c_datax_type type rollname value 'BAPIUPDATE'.

    try.
        data(lt_components) =
          cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = data ) )->components.

        clear: lo_descr, lv_relative_name.

        unassign: <fs_data>, <fs_datax>, <fs_x>.

        assign: data  to <fs_data>,
        datax to <fs_datax>.

        if <fs_data> is assigned and <fs_datax> is assigned.
          clear: <fs_datax>.
*          move 0 to sy-subrc.
          loop at lt_components into data(ls_component).
            unassign <fs_x>.
            assign component ls_component-name of structure <fs_datax> to <fs_x>.
*            if sy-subrc <> 0.
*              exit.
*            endif.
            if <fs_x> is assigned.
              unassign <fs>.
              assign component ls_component-name of structure <fs_data> to <fs>.
              if <fs> is assigned and <fs> is not initial.
                free lo_descr.
                lo_descr = cl_abap_typedescr=>describe_by_data( p_data = <fs_x> ).

                if lo_descr is bound.
                  clear lv_relative_name.
                  lv_relative_name = lo_descr->get_relative_name( ).
                  if lv_relative_name eq c_datax_type.
                    move abap_true to <fs_x>.
                  else.
                    move <fs> to <fs_x>.
                  endif.
                endif.
              endif.
            endif.
            clear ls_component.
          endloop.
        endif.
      catch cx_root into data(lox_root).
    endtry.
*}   INSERT
  endmethod.


  method format_excel_to_bapi.
    check is_excel is not initial.
    assign is_excel to field-symbol(<fs_excel>).
    assign is_data  to field-symbol(<fs_data>).

    data(lo_helper) = new lcl_helper( ).

    data(lt_comp_excel) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <fs_excel> ) )->components.
    data(lt_comp_data) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <fs_data> ) )->components.

    check <fs_excel> is assigned and <fs_data> is assigned.

    do lines( lt_comp_excel ) times.
      try.
          data(ls_comp_excel) = lt_comp_excel[ sy-index ].
          data(ls_comp_data) = lt_comp_data[ name = ls_comp_excel-name ].
          assign component ls_comp_excel-name of structure <fs_excel> to field-symbol(<fv_excel>).
          assign component ls_comp_data-name of structure <fs_data> to field-symbol(<fv_data>).
          if <fv_excel> is assigned and <fv_data> is assigned.
            data(lo_elem) = cast cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <fv_excel> ) ).
            if lo_elem is bound and ( lo_elem->type_kind eq 'C' or lo_elem->type_kind eq 'g' ). " char or string
              <fv_excel> = condense( |{ shift_left( val = <fv_excel> sub = ' ' ) }| ).
              try.  " for date conversion
                  if strlen( <fv_excel> ) eq 10 and
                    ( ( count( val = <fv_excel> sub = '.' ) eq 2 )
                   or ( count( val = <fv_excel> sub = '/' ) eq 2 )
                   or ( count( val = <fv_excel> sub = '-' ) eq 2 ) ).
                    replace all occurrences of '/' in <fv_excel> with '.'.
                    replace all occurrences of '-' in <fv_excel> with '.'.
                    data lv_date type sy-datum.
                    clear lv_date.
                    call function 'CONVERT_DATE_INPUT'
                      exporting
                        input                     = <fv_excel>
                        plausibility_check        = 'X'
                      importing
                        output                    = lv_date
                      exceptions
                        plausibility_check_failed = 1
                        wrong_format_in_input     = 2
                        others                    = 3.
                    if sy-subrc = 0.
                      <fv_data> = lv_date.
                    endif.
                  endif.

                  if <fv_data> is initial.
                    if ls_comp_data-type_kind = cl_abap_typedescr=>typekind_date.
                      lo_helper->excel_date_to_sap_date(
                        changing
                          cv_excel_date = <fv_excel> ).
                    endif.
                  endif.

                  if strlen( <fv_excel> ) eq 8 and
                    ( ( count( val = <fv_excel> sub = ':' ) eq 2 ) or ( count( val = <fv_excel> sub = '.' ) eq 2 ) ).
                    replace all occurrences of '.' in <fv_excel> with ':'.
                    data lv_time type sy-uzeit.
                    clear lv_time.
                    call function 'CONVERT_TIME_INPUT'
                      exporting
                        input                     = <fv_excel>
                        plausibility_check        = abap_true
                      importing
                        output                    = lv_time
                      exceptions
                        plausibility_check_failed = 1
                        wrong_format_in_input     = 2
                        others                    = 3.
                    if sy-subrc = 0.
                      <fv_data> = lv_time.
                    endif.
                  endif.

                  if <fv_data> is initial.
                    if ls_comp_data-type_kind = cl_abap_typedescr=>typekind_time.
                      lo_helper->excel_time_to_sap_time(
                        changing
                          cv_excel_time = <fv_excel> ).
                    endif.
                  endif.
                catch cx_sy_range_out_of_bounds.
                catch cx_sy_regex_too_complex.
                catch cx_sy_strg_par_val.
              endtry.
            endif.

            if <fv_data> is initial.
              <fv_data> = <fv_excel>.
            endif.

            data(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = <fv_data> ).

            if lo_type is bound.
              data(lv_relative_name) = lo_type->get_relative_name( ).

              if lv_relative_name is not initial.
                data: lv_data_element type dd04l-rollname.
                clear: lv_data_element.
                move lv_relative_name to lv_data_element.
                select single *
                  from dd04l
                  into @data(wa_dd04l)
                  where rollname = @lv_data_element.

                if wa_dd04l-convexit is not initial.
                  concatenate 'CONVERSION_EXIT_' wa_dd04l-convexit '_INPUT' into data(lv_function).

                  call function 'FUNCTION_EXISTS'
                    exporting
                      funcname           = conv rs38l-name( lv_function )
                    exceptions
                      function_not_exist = 1
                      others             = 2.
                  if sy-subrc = 0.
                    try.
                        call function lv_function
                          exporting
                            input  = <fv_data>
                          importing
                            output = <fv_data>.
                      catch cx_sy_dyn_call_illegal_func.
                        " catch-block
                    endtry.
                  endif.
                endif.
              endif.
              " fail safety date conversion - albeit redundant
              free lo_elem.
              lo_elem = cast cl_abap_elemdescr( lo_type ).
              if lo_elem is bound and lo_elem->type_kind eq 'D'.
                assign component ls_comp_excel-name of structure <fs_excel> to field-symbol(<fv_date>).
                if sy-subrc = 0 and <fv_date> is assigned and <fv_date> is not initial and <fv_data> is initial.
                  call function 'CONVERT_DATE_TO_INTERNAL'
                    exporting
                      date_external            = <fv_date>
                      accept_initial_date      = abap_false
                    importing
                      date_internal            = <fv_data>
                    exceptions
                      date_external_is_invalid = 1
                      others                   = 2.
                  if sy-subrc <> 0.
* Implement suitable error handling here
                  endif.
                endif.
              endif.
            endif.
          endif.

          unassign:
            <fv_excel>,
            <fv_data>,
            <fv_date>.

          free lo_type.

          clear:
            lv_function,
            lv_relative_name,
            lv_data_element,
            wa_dd04l.
        catch cx_sy_itab_line_not_found ##no_handler.
      endtry.
    enddo.

    if <fs_data> is not initial.
      <fs_excel> = corresponding #( <fs_data> ).
    endif.
  endmethod.


  method get_bgrfc_unit.
    " RFCDEST must exist in table RFCDES / SM59 - ABAP Connections
    " One destination must physically exist in each server (DEV, QA & PRD)
    " Following setup must be used in each system separately
    " a. Name it as ZZ(SY-SYSID)_BGRFC -> For sake of uniformity
    " b. In description add: DO NOT DELETE THIS DESTINATION PLEASE --> so that it is not tampered with unknowingly
    " c. In logon & security tab: 1. language = '' 2. client = '' 3. current user = 'X' 4. Trust relationship = 'Yes'
    " d. In special options tab: under select protocols section: qRFC version = 1 bgRGC, Serializer = C classic
    " Set the FM as RFC enabled
    " Do not use commit/rollback work
    " Do not add exporting or changing parameters
    " Do not use "wait up to X seconds" in BUNIT FM's --> SYSTEM_ILLEGAL_STATEMENT runtime error occurs

    clear ro_unit.
    free ro_unit.

    data(lv_rfcdest) = conv rfcdest( cond #( when is_development( ) then 'ZZIHD_BGRFC'
                                             when is_quality( )     then 'ZZIHQ_BGRFC'
                                             when is_production( )  then 'ZZIHP_BGRFC'
                                             when is_sandbox( )     then 'ZZSBX_BGRFC' ) ).

    if lv_rfcdest is not initial.
* Create the destination object references
      try.
          data(lo_rfcdest) = cl_bgrfc_destination_outbound=>create( exporting dest_name = lv_rfcdest ).
          if lo_rfcdest is bound.
            ro_unit = lo_rfcdest->create_trfc_unit( ).
          endif.
        catch cx_bgrfc_invalid_destination into data(lox_bgrfc_invalid_destination).
*          message e103(bgrfc) with lv_rfcdest.
      endtry.
    endif.
  endmethod.


  method get_dobj_name.
*{   INSERT         SBXK900102                                        1
    " code adopted from cl_demo_output -> local class code_analysis
    data: lt_stack type abap_callstack,
          ls_stack type line of abap_callstack,
          lt_lines type table of abap_callstack_line-line.

    call function 'SYSTEM_CALLSTACK'
      importing
        callstack = lt_stack.

    loop at lt_stack into ls_stack where mainprogram cs sy-repid ##no_text ##INTO_OK.
      data(idx) = sy-tabix.
    endloop.
    loop at lt_stack into ls_stack from idx + 1 ##INTO_OK.
      append ls_stack-line to lt_lines.
    endloop.
    read table lt_stack into ls_stack index idx + 1.

    data:
      progtab     type table of string,
      progline    type string,
      moff        type i,
      stack_frame type line of abap_callstack,
      lines       type standard table of abap_callstack_line-line.

    field-symbols <progline> type string.

    stack_frame = ls_stack.
    lines = lt_lines.

    clear idx.
    read report stack_frame-include into progtab.
    if sy-subrc <> 0.
*    raise exception type cx_name.
    endif.
    delete progtab to stack_frame-line - 1.
    "Remove comments
    loop at progtab assigning <progline>.
      if strlen( <progline> ) > 0 and <progline>(1) = '*'.
        delete progtab.
        continue.
      endif.
      replace regex `\A\s*".*` in <progline> with `` ##no_text.
      if sy-subrc <> 0.
        replace regex `(.*)(")([^'|{``]+\z)` in <progline> with `$1`.
      endif.
      if <progline> is initial.
        delete progtab.
      endif.
    endloop.
    "Get all statements that are in or begin in the line
    loop at progtab assigning <progline>.
      condense <progline>.
      idx = sy-tabix.
      replace all occurrences of regex `'[^']*\.[^']*'` in <progline> with `'dummy'` ##no_text.
      replace all occurrences of regex '`[^`]*\.[^`]*`' in <progline> with '`dummy`' ##no_text.
      replace all occurrences of regex '\|[^|]*\.[^|]*\|' in <progline> with '`dummy`' ##no_text.
      if idx = 1.
        progline = progline && ` ` && <progline>.
        if substring( val = progline off = strlen( progline ) - 1 len = 1 ) = `.`.
          exit.
        endif.
      else.
        find `.` in <progline> match offset moff.
        if sy-subrc = 0.
          progline = progline && ` ` && substring( val = <progline> len = moff + 1 ).
          exit.
        else.
          progline = progline && ` ` && <progline>.
        endif.
      endif.
    endloop.
    "Separate the calls of one line
    condense progline.
    replace all occurrences of regex `\s?CALL METHOD\s([^.]+)\(\s(?:EXPORTING\s)?(?:value|data)\s=\s([^.]+)\s\)\s?\.` ##NO_TEXT
    in progline with `$1( $2 ).` ignoring case.
    condense progline.
    replace all occurrences of regex `\s?CALL METHOD\s([^.]+)\sEXPORTING\s(?:value|data)\s=\s([^.]+)\s?\.` ##NO_TEXT
    in progline with `$1( $2 ).` ignoring case.
    condense progline.
    "Exactly the following methods call get_dobj_name( )
    replace all occurrences of `=>GET_DOBJ_NAME(` in progline with `###(` ignoring case.
    split progline at `###(` into table progtab.
    if lines( progtab ) <= 1.
*    raise exception type cx_name.
    endif.
    delete progtab index 1.
    loop at progtab assigning <progline>.
      replace regex `([^)]+)(\).*)` in <progline> with `$1`.
      condense <progline>.
      replace regex `(?:EXPORTING )?(?:value|data) = ` in <progline> with `` ignoring case ##NO_TEXT.
      if <progline> cs ` `  or
      matches( val = <progline> regex = `-?\d+` ) or           "no numeric literals
      <progline> cs `'`                           or           "no text field literals
      <progline> cs '`'                           or           "no string literals
      <progline> cs `[` or <progline> cs `]`      or           "expressions (parenthesis)
      <progline> cs `(` or <progline> cs `)`.                  "expressions (parenthesis)
        clear <progline>.
      endif.
    endloop.

    read table progtab into name index 1.
    name = to_upper( name ).
*}   INSERT
  endmethod.


  method get_irn_qrcode.
*--------------------------------------------------------------------*
    " IHDK908270: SD: S_K: E-INV: QR Code Print: 22.9.20
*--------------------------------------------------------------------*
    clear:
      rv_qr1,
      rv_qr2,
      rv_qr3,
      rv_qr4,
      rv_qr5,
      rv_qr6,
      rv_qr7,
      rv_qr8,
      rv_qr9,
      rv_qr10.

    data:
      r_comp_code like range of iv_comp_code,   " bukrs
      r_document  like range of iv_document,    " vbeln, belnr
      r_fis_year  like range of iv_fis_year,    " gjahr
      r_odn_num   like range of iv_odn_number,  " xblnr/odn
      r_odn_date  like range of iv_odn_date.    " odn date/document date

    clear:
      r_comp_code[], r_comp_code,
      r_document[], r_document,
      r_fis_year[], r_fis_year,
      r_odn_num[], r_odn_num,
      r_odn_date[], r_odn_date.

    check iv_comp_code is not initial
      or iv_document is not initial
      or iv_fis_year is not initial
      or iv_odn_number is not initial
      or iv_odn_date is not initial.

    if iv_comp_code is not initial.
      r_comp_code = value #( ( sign = 'I' option = 'EQ' low = iv_comp_code ) ).
    endif.

    if iv_document is not initial.
      r_document = value #( ( sign = 'I' option = 'EQ' low = iv_document ) ).
    endif.

    if iv_fis_year is not initial.
      r_fis_year = value #( ( sign = 'I' option = 'EQ' low = iv_fis_year ) ).
    endif.

    if iv_odn_number is not initial.
      r_odn_num = value #( ( sign = 'I' option = 'EQ' low = iv_odn_number ) ).
    endif.

    if iv_odn_date is not initial.
      r_odn_date = value #( ( sign = 'I' option = 'EQ' low = iv_odn_date ) ).
    endif.

    select single signedqrcode
      from zei_api_global
      where company_code in @r_comp_code[]
      and   custom1 in @r_document[]
      and   fiscal_year in @r_fis_year[]
      and   documentnumber in @r_odn_num[]
      and   documentdate in @r_odn_date[]
      into @data(lv_signed_qr).

    if lv_signed_qr is not initial.
      data:
        lv_input_string      type string,
        lt_string_components type standard table of swastrtab.

      constants lc_max_component_length type i value '255'.

      clear:
        lv_input_string,
        lt_string_components.

      lv_input_string = lv_signed_qr.

      call function 'SWA_STRING_SPLIT'
        exporting
          input_string                 = lv_input_string
          max_component_length         = lc_max_component_length
        tables
          string_components            = lt_string_components
        exceptions
          max_component_length_invalid = 1
          others                       = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      define get_qr_data.
        try.
            &1 = lt_string_components[ &2 ]-str.
          catch cx_sy_itab_line_not_found ##no_handler.
        endtry.
      end-of-definition.

      if lt_string_components is not initial.
        get_qr_data rv_qr1 1.
        get_qr_data rv_qr2 2.
        get_qr_data rv_qr3 3.
        get_qr_data rv_qr4 4.
        get_qr_data rv_qr5 5.
        get_qr_data rv_qr6 6.
        get_qr_data rv_qr7 7.
        get_qr_data rv_qr8 8.
        get_qr_data rv_qr9 9.
        get_qr_data rv_qr10 10.
      endif.

    endif.
  endmethod.


  method get_manager_emp_id.
    " IHDK900614: XX: S_K: ZCL_HELPER: Add manager fetch method: 13.2.19
    check iv_empl_id is not initial.
    data plvar       type hrsobid-plvar value '01'.
    data otype       type hrsobid-otype value 'P '.
    data sobid       type hrsobid-sobid.
    data leading_pos type standard table of hrobject.

    clear sobid.
    data: lv_empl_id like iv_empl_id.
    clear lv_empl_id.
    lv_empl_id = |{ iv_empl_id alpha = in }|.
    sobid = lv_empl_id.
    refresh leading_pos.
    call function 'RH_GET_LEADING_POSITION'
      exporting
        plvar             = plvar
        otype             = otype
        sobid             = sobid
      tables
        leading_pos       = leading_pos
      exceptions
        no_lead_pos_found = 1
        others            = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
    if leading_pos is not initial.
      data im_objecttab type hrobject_t.
      data im_begda     type begda value '00010101'.
      data im_endda     type endda value '99991231'.
      data ex_1001_tab  type p1001tab.
      data ex_return    type hrhcp00_error_tab.

      refresh im_objecttab.
      append lines of leading_pos to im_objecttab.
      refresh: ex_1001_tab, ex_return.
      call function 'HR_HCP_GET_EMPL_FOR_POSITION'
        exporting
          im_objecttab    = im_objecttab
          im_begda        = im_begda
          im_endda        = im_endda
        importing
          ex_1001_tab     = ex_1001_tab
          ex_return       = ex_return
        exceptions
          read_1001_error = 1
          others          = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      if ex_1001_tab is not initial.
        clear iv_mgr_empl_id.
        try.
            iv_mgr_empl_id = ex_1001_tab[ 1 ]-sobid.
            iv_mgr_empl_id = |{ iv_mgr_empl_id alpha = in }|.
          catch cx_sy_itab_line_not_found.
        endtry.
      endif.
    endif.
  endmethod.


  method is_development.
*{   INSERT         SBXK900102                                        1
    select single * from t000 into @data(client) where mandt = @sy-mandt.
    if client-cccategory = 'C'  " simplify system role detection, IHDK900039
*    and client-cccoractiv eq '1'
*    and client-ccnocliind eq ''
*    and client-cccopylock eq ''
      and not sy-sysid eq gc_sandbox_sysid. " don't return sandbox system as quality
      dev = abap_true.
    endif.
*}   INSERT
  endmethod.


  method is_production.
*{   INSERT         SBXK900102                                        1
    select single * from t000 into @data(client) where mandt = @sy-mandt.
    if client-cccategory = 'P' and not sy-sysid eq gc_sandbox_sysid. " don't return sandbox system as production.
      prd = abap_true.
    endif.
*}   INSERT
  endmethod.


  method is_quality.
*{   INSERT         SBXK900102                                        1
    select single * from t000 into @data(client) where mandt = @sy-mandt.
    if client-cccategory = 'T'  " simplify system role detection, IHDK900039
*    and client-cccoractiv eq '2'
*    and ( client-ccnocliind eq '2' or client-ccnocliind eq '3' )
*    and client-cccopylock eq 'L'
      and not sy-sysid eq gc_sandbox_sysid. " don't return sandbox system as quality
      qas = abap_true.
    endif.
*}   INSERT
  endmethod.


  method is_sandbox.
*{   INSERT         SBXK900102                                        1
    select single cccategory from t000 into @data(client_category) where mandt = @sy-mandt.
    if client_category eq 'C' and sy-sysid eq gc_sandbox_sysid. " don't return sandbox system as development
      sbx = abap_true.
    endif.
*}   INSERT
  endmethod.


  method itab_to_excel.
    clear:
      rt_data,
      ev_file_length.

    data(lo_helper) = new lcl_helper( ).

    data(lv_filepath) = iv_file_path.

    " compute file extension
    data(lv_file_ext) = value char50( ).

    if lv_filepath is not initial.
      data: lr_file type ref to data.
      data(lv_len) = strlen( lv_filepath ).

      create data lr_file type c length lv_len.
      if lr_file is bound.
        assign lr_file->* to field-symbol(<lv_file>).
      endif.
    endif.

    if <lv_file> is assigned.
      <lv_file> = lv_filepath.

      call function 'TRINT_FILE_GET_EXTENSION'
        exporting
          filename  = <lv_file>
        importing
          extension = lv_file_ext.
    endif.

    if lv_file_ext is initial.
      lv_file_ext = gc_extension-xlsx.
    endif.

* ---- Compute filepath type ---- *
    if lv_filepath is not initial.
      data(lv_file_path_type) = cond #( when lv_filepath ca gc_path_sep-windows then gc_path_sep-windows
                                        when lv_filepath ca gc_path_sep-unix then gc_path_sep-unix ).

      if not lo_helper->validate_file_path(
                exporting
                  iv_filepath          = lv_filepath
                  iv_contains_filename = abap_true ).

        lv_filepath = switch #( lv_file_path_type
                                  when gc_path_sep-windows then gc_path_sep-windows
                                  when gc_path_sep-unix then gc_path_sep-unix ).

        message 'Invalid download/file path specified. File will be downloaded to default path.' type 'S' display like 'W'.
      endif.
    endif.

    if it_multi_sheet_data is not initial.
      loop at it_multi_sheet_data into data(ls_sheet) where data is bound.
        assign ls_sheet-data->* to field-symbol(<lt_data>).
        if <lt_data> is assigned and <lt_data> is not initial.
          data(lv_data_supplied) = abap_true.
          exit.
        endif.
        clear ls_sheet.
      endloop.
    endif.

    if it_itab is initial and lv_data_supplied = abap_false.
      message 'No data supplied. Excel conversion not possible.' type 'S' display like 'E'.
      return.
    endif.

    data(lt_multi_sheet_data) = it_multi_sheet_data.
    if it_itab is not initial.
      insert value #( name   = iv_sheet_name
                      data   = ref #( it_itab )
                      fields = it_fields
                      string = iv_force_string
                      header = iv_insert_header ) into lt_multi_sheet_data index 1.
    endif.

    if lo_helper is bound.
      if lines( lt_multi_sheet_data ) gt 1.
        cl_rs_data=>switch_order( changing c_t_data = lt_multi_sheet_data ). " Table to Be Sorted
      endif.

      case lv_file_ext.
        when gc_extension-xlsx.
          data(lv_data) = lo_helper->itab_to_excel_ehfnd(
                        exporting
                          it_multi_sheet_data = lt_multi_sheet_data ).
        when gc_extension-xls.
          lv_data = lo_helper->itab_to_excel_soi(
                      exporting
                        it_multi_sheet_data = lt_multi_sheet_data ).
        when others.
      endcase.
    endif.

    if lv_data is not initial.
      data(lv_file_length) = xstrlen( lv_data ).
      ev_file_length = lv_file_length.
      data(lt_data) = cl_bcs_convert=>xstring_to_solix(
                            exporting
                              iv_xstring = lv_data ).
      rt_data = lt_data.

      if lv_filepath is not initial.
        try.
            data(lv_uploaded) = write_file_to_path(
              exporting
                iv_filepath    = lv_filepath        " Path of file to write to on frontend or app server
                iv_file_length = lv_file_length     " Size of binary data
                it_data        = lt_data ).         " Binary Data
          catch zcx_generic into data(lox_generic). " Generic Exception Class
            message lox_generic type 'S' display like 'E'.
        endtry.
      endif.
    endif.

*        data:
*          lv_window_title      type string,
*          lv_default_extension type string,
*          lv_default_file_name type string,
*          lv_file_filter       type string.
*
*        clear:
*          lv_window_title,
*          lv_default_extension,
*          lv_default_file_name,
*          lv_file_filter.
*
*        lv_window_title = 'Specify folder and file name to save excel file'.
*        lv_default_extension = lv_file_ext.
*        lv_file_filter = |Excel files(*.{ lv_file_ext })\|*.{ lv_file_ext }|. " description|*.extension
*        lv_default_file_name = replace( val = to_upper( iv_file_name ) sub = |.{ lv_file_ext }| with = '' occ = 0 ).
*
*        lv_filepath = file_save_dialog(
*                        exporting
*                          iv_window_title      = lv_window_title
*                          iv_file_filter       = lv_file_filter
*                          iv_default_extension = lv_default_extension
*                          iv_default_file_name = lv_default_file_name ).
*
*        if lv_filepath is initial.
*          message 'No path selected' type 'S' display like 'E'.
*        endif.
  endmethod.


  method itab_to_fcat.
*    if iv_data is supplied.
    data(lo_type_descr) = cl_abap_typedescr=>describe_by_data( exporting p_data = iv_data ).
    if lo_type_descr is bound.
      case type of lo_type_descr.
        when type cl_abap_tabledescr.
          data(lo_struct_descr) = cast cl_abap_structdescr( cast cl_abap_tabledescr( lo_type_descr )->get_table_line_type( ) ).
        when type cl_abap_structdescr.
          lo_struct_descr ?= lo_type_descr.
        when others.
      endcase.

      if lo_struct_descr is bound.
        data(lt_ddic_field_list) = lo_struct_descr->get_ddic_field_list(
                                     exporting
                                       p_langu                  = sy-langu
                                       p_including_substructres = abap_true ).

        if lt_ddic_field_list is not initial.
          et_lvc_fcat = corresponding #( lt_ddic_field_list ).

          " todo - fill description fields using reptext

          call function 'LVC_TRANSFER_TO_SLIS'
            exporting
              it_fieldcat_lvc         = et_lvc_fcat " Field Catalog
            importing
              et_fieldcat_alv         = et_slis_fcat " Field Catalog
            exceptions
              it_data_missing         = 1
              it_fieldcat_lvc_missing = 2
              others                  = 3.
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          endif.
        endif.
      endif.
    endif.
*    endif.
  endmethod.


  method itab_to_html.
    clear rv_html.

    " easiest/most efficient way of converting an internal table to HTML
    check it_table[] is not initial.
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = data(lo_alv)
          changing
            t_table      = it_table[] ).
      catch cx_salv_msg.                                "#EC NO_HANDLER
    endtry.

    check lo_alv is bound.
    data(lo_columns) = lo_alv->get_columns( ).

    if lo_columns is bound.
      lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
    endif.

    " get alv in html xml format
    data(lv_xml) = lo_alv->to_xml(
                    exporting
                      xml_type = if_salv_bs_xml=>c_type_mhtml
                      xml_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export ).

    " convert xstring xml to string html
    rv_html = cl_bcs_convert=>xstring_to_string( exporting iv_xstr = lv_xml iv_cp = '1101' ).  " 1101 - US-ASCII

    " keep shifting string to the left till first < is encountered which marks the beginning of html document "<html>"
    " all text to the left of <html> is superflous; needs to be removed
    do.
      if rv_html+0(1) ne '<'.
        shift rv_html.
      else.
        exit.
      endif.
    enddo.

*    data(lv_mime_text) = |MIME-Version: 1.0 X-Document-Type: Worksheet Content-Type: multipart/related; | &&
*      |boundary="----=_NextPart_01C5084F.FEF9A7A0" ------=_NextPart_01C5084F.FEF9A7A0 Content-Location: | &&
*      |file:///C:/Mappe1.htm Content-Transfer-Encoding: text/html Content-Type: text/html; charset="utf-8"|.

*    replace all occurrences of lv_mime_text in lv_html with ''.

*    clear lv_mime_text.
    data(lv_mime_text) = |------=_NextPart_01C5084F.FEF9A7A0--|.  " some redundant text at the end of the html code; needs to be removed
    replace all occurrences of lv_mime_text in rv_html with ''.
  endmethod.


  method print_no_pg_brk.
    data: pri_params type pri_params,
          valid(1)   type c.

    clear: pri_params, valid.
    call function 'GET_PRINT_PARAMETERS'    " Obtain print parameters
      exporting
        no_dialog      = abap_true
      importing
        out_parameters = pri_params
        valid          = valid.

    if valid ne space.
      pri_params-linct = '99999'.
      pri_params-linsz = '300'.
      pri_params-paart = 'ZALV_NO_PG_BRK'.  " custom page format for LOCL without page breaks upto 99999 lines
      pri_params-pdest = 'LOCL'.

      call function 'SET_PRINT_PARAMETERS'  " Modify print parameters
        exporting
          in_parameters = pri_params.
    endif.
  endmethod.


  method read_file_from_path.
    constants: lc_msg_id           type syst-msgid value '00',
               lc_msg_no           type syst-msgno value '001',
               lc_logical_filename type filename-fileintern value 'EHS_FTAPPL_2'. " same as that used in CG3Y

    data ls_data like line of rt_data.

    data(lv_filepath) = iv_filepath.

    clear:
      ls_data,
      ev_file_length,
      rt_data.

    if lv_filepath is initial.
      data(lv_msg) = conv bapi_msg( 'Mandatory input missing: Filepath' ).
      raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
    endif.

    if not check_file_exists( exporting iv_filepath = lv_filepath ).
      lv_msg = conv bapi_msg( 'Requested file does not exist' ).
      raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
    endif.

* ---- Compute filepath type ---- *
    data(lv_file_path_type) = cond #( when lv_filepath ca gc_path_sep-windows then gc_path_sep-windows
                                      when lv_filepath ca gc_path_sep-unix then gc_path_sep-unix ).

    case lv_file_path_type.
      when gc_path_sep-windows.
        if sy-batch = abap_true.
          lv_msg = 'GUI not available in background mode'.
          raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        endif.
        cl_gui_frontend_services=>gui_upload(
          exporting
            filename                = lv_filepath        " Name of file
            filetype                = conv #( gc_extension-bin )   " File Type (ASCII, Binary)
          importing
            filelength              = ev_file_length     " File Length
          changing
            data_tab                = rt_data            " Transfer table for file contents
          exceptions
            file_open_error         = 1                  " File does not exist and cannot be opened
            file_read_error         = 2                  " Error when reading file
            no_batch                = 3                  " Cannot execute front-end function in background
            gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
            invalid_type            = 5                  " Incorrect parameter FILETYPE
            no_authority            = 6                  " No upload authorization
            unknown_error           = 7                  " Unknown error
            bad_data_format         = 8                  " Cannot Interpret Data in File
            header_not_allowed      = 9                  " Invalid header
            separator_not_allowed   = 10                 " Invalid separator
            header_too_long         = 11                 " Header information currently restricted to 1023 bytes
            unknown_dp_error        = 12                 " Error when calling data provider
            access_denied           = 13                 " Access to File Denied
            dp_out_of_memory        = 14                 " Not enough memory in data provider
            disk_full               = 15                 " Storage medium is full.
            dp_timeout              = 16                 " Data provider timeout
            not_supported_by_gui    = 17                 " GUI does not support this
            error_no_gui            = 18                 " GUI not available
            others                  = 19 ).
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_msg.
          raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        else.
          if ev_file_length is not initial.
            message |{ ev_file_length } bytes read from { lv_filepath }| type 'S'.
          endif.
        endif.
      when gc_path_sep-unix.
* ---- check the authority to read the file from the application server ---- *
        data(lv_program) = conv authb-program( sy-cprog ).
        data(lv_auth_filename) = conv authb-filename( lv_filepath ).
        call function 'AUTHORITY_CHECK_DATASET'
          exporting
            program          = lv_program  " ABAP program in which access occurs
            activity         = sabc_act_read " Access Type (See Function Documentation)
            filename         = lv_auth_filename " File name
          exceptions
            no_authority     = 1        " You are not authorized for this access
            activity_unknown = 2        " Access type unknown
            others           = 3.
        if sy-subrc <> 0.
          case sy-subrc.
            when 1.
              lv_msg = 'Not authorised to write the file to app server'.
            when others.
              lv_msg = 'File open error'.
          endcase.
          raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        endif.

        if not check_file_exists( exporting iv_filepath = lv_filepath ).
          lv_msg = 'App server file path does not exist'.
          raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        endif.

* ---- validate physical filename against logical filename ---- *
        call function 'FILE_VALIDATE_NAME'
          exporting
            logical_filename           = lc_logical_filename
          changing
            physical_filename          = lv_filepath
          exceptions
            logical_filename_not_found = 1
            validation_failed          = 2
            others                     = 3.
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_msg.
          raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        endif.

        try.
            " open the dataset for reading
            open dataset lv_filepath for input in binary mode.
            if sy-subrc = 0.
              try.
                  do.
                    clear ls_data.
                    read dataset lv_filepath into ls_data length data(lv_len).
                    if sy-subrc <> 0.
                      if lv_len > 0.
                        ev_file_length = ev_file_length + lv_len.
                        append ls_data to rt_data.
                      endif.
                      exit.
                    endif.
                    ev_file_length = ev_file_length + lv_len.
                    append ls_data to rt_data.
                  enddo.
                  try.
                      " close the dataset after reading
                      close dataset lv_filepath.

                      if ev_file_length is not initial.
                        message |{ ev_file_length } bytes read from { lv_filepath }| type 'S'.
                      endif.
                    catch cx_sy_file_close into data(lox_file_close).
                      lv_msg = lox_file_close->get_text( ).
                      raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
                  endtry.
                catch cx_sy_file_io into data(lox_file_io).
                  lv_msg = lox_file_io->get_text( ).
                  raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
                catch cx_sy_file_open_mode into data(lox_file_open_mode).
                  lv_msg = lox_file_open_mode->get_text( ).
                  raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
              endtry.
            else.
              lv_msg = 'File open error'.
              raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
            endif.
          catch cx_sy_file_authority into data(lox_file_authority).
            lv_msg = lox_file_authority->get_text( ).
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          catch cx_sy_file_open into data(lox_file_open).
            lv_msg = lox_file_open->get_text( ).
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          catch cx_sy_too_many_files into data(lox_too_many_files).
            lv_msg = lox_too_many_files->get_text( ).
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        endtry.
      when others.
    endcase.
  endmethod.


  method remove_special_chars.
    check cv_text is not initial.
    replace all occurrences of regex '[^a-zA-Z0-9 ]' in cv_text with ''.
    condense cv_text.
  endmethod.


  method simple_salv_display.
    check it_table is not initial.

    field-symbols <lt_table> type standard table.
    unassign <lt_table>.
    assign it_table to <lt_table>.
    if <lt_table> is assigned.
      try.
          cl_salv_table=>factory(
*           exporting
*             list_display   = if_salv_c_bool_sap=>false " ALV Displayed in List Mode
            importing
              r_salv_table   = data(lo_alv)              " Basis Class Simple ALV Tables
            changing
              t_table        = <lt_table> ).

          if lo_alv is bound.
            data(lo_columns) = lo_alv->get_columns( ).
            if lo_columns is bound.
              try.
                  data(lo_column) = lo_columns->get_column( exporting columnname = 'MANDT' ).
                  if lo_column is bound.
                    lo_column->set_technical( exporting value = if_salv_c_bool_sap=>true ).
                  endif.
                catch cx_salv_not_found ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
              endtry.

              data(lt_col) = lo_columns->get( ).

              if lt_col is not initial.
                loop at lt_col into data(ls_col).
                  translate ls_col-columnname using '_ '.
                  ls_col-columnname = to_mixed( ls_col-columnname ).
                  if ls_col-r_column->get_long_text( ) is initial or iv_use_col_name_as_header = abap_true.
                    ls_col-r_column->set_long_text( exporting value = conv #( ls_col-columnname ) ).
                  endif.
                  if ls_col-r_column->get_medium_text( ) is initial or iv_use_col_name_as_header = abap_true.
                    ls_col-r_column->set_medium_text( exporting value = conv #( ls_col-columnname ) ).
                  endif.
                  if ls_col-r_column->get_short_text( ) is initial or iv_use_col_name_as_header = abap_true.
                    ls_col-r_column->set_short_text( exporting value = conv #( ls_col-columnname ) ).
                  endif.
                  ls_col-r_column->set_output_length( exporting value = conv #( strlen( ls_col-columnname ) ) ).

                  clear ls_col.
                endloop.
              endif.

              lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
            endif.

            lo_alv->get_functions( )->set_all( exporting value = if_salv_c_bool_sap=>true ).
            lo_alv->get_display_settings( )->set_striped_pattern( exporting value = abap_true ).

            data(lo_layout) = lo_alv->get_layout( ).

            data(lo_key) = value salv_s_layout_key( report = sy-repid ).

            if lo_layout is bound.
              lo_layout->set_key( exporting value = lo_key ).

              lo_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

              lo_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
            endif.

            lo_alv->display( ).
          endif.
        catch cx_salv_msg ##no_handler. " ALV: General Error Class with Message
      endtry.
    endif.
  endmethod.


  method update_report_variants_db.
    data: ls_layout type upd_s_layo,
          ls_ltdx   type ltdx,
          ls_ltdxt  type ltdxt,
          ls_ltdxd  type ltdxd,
          ls_ltdxs  type ltdxs.

    clear rv_ok.
    rv_ok = abap_true.
    move-corresponding is_layout to ls_layout.

    if is_layout-upd_mode ne 'I'.
      is_layout-upd_mode = 'I'.
    endif.

    if ls_layout-text is initial.
      clear rv_ok.
      exit.
    endif.

    if ls_layout-mandt is initial.
      ls_layout-mandt = sy-mandt.
    endif.

    if ls_layout-langu is initial.
      ls_layout-langu = sy-langu.
    endif.

    if ls_layout-relid is initial.
      ls_layout-relid = 'LT'.
    endif.

    if ls_layout-type is initial.
      ls_layout-type = 'F'.
    endif.

    if ls_layout-variant+0(1) co '0123456789/'.
      clear ls_layout-username.
    endif.

    clear ls_ltdx.
    move-corresponding ls_layout to ls_ltdx.

    clear ls_ltdxt.
    move-corresponding ls_layout to ls_ltdxt.

    clear ls_ltdxd.
    if ls_layout-defaultvar ne space.
      move-corresponding ls_layout to ls_ltdxd.
    endif.

    clear ls_ltdxs.
    if ls_layout-variant(1) co '0123456789'.
      move-corresponding ls_layout to ls_ltdxs.
    endif.

    if ls_ltdx is not initial.
      call function 'ENQUEUE_E_LTDX'
        exporting
          relid          = ls_ltdx-relid
          report         = ls_ltdx-report
          handle         = ls_ltdx-handle
          log_group      = ls_ltdx-log_group
          username       = ls_ltdx-username
          variant        = ls_ltdx-variant
          type           = ls_ltdx-type
          srtf2          = ls_ltdx-srtf2
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.
      if sy-subrc = 0.
* Implement suitable error handling here

        delete from ltdx client specified
                            where mandt     eq ls_ltdx-mandt
                              and relid     eq ls_ltdx-relid
                              and report    eq ls_ltdx-report
                              and handle    eq ls_ltdx-handle
                              and log_group eq ls_ltdx-log_group
                              and username  eq ls_ltdx-username
                              and type      eq ls_ltdx-type
                              and variant   eq ls_ltdx-variant
                              and srtf2     eq ls_ltdx-srtf2.

        insert ltdx from ls_ltdx.
        if sy-dbcnt ne 1.
          clear rv_ok.
        endif.

        call function 'DEQUEUE_E_LTDX'
          exporting
            relid     = ls_ltdx-relid
            report    = ls_ltdx-report
            handle    = ls_ltdx-handle
            log_group = ls_ltdx-log_group
            username  = ls_ltdx-username
            variant   = ls_ltdx-variant
            type      = ls_ltdx-type
            srtf2     = ls_ltdx-srtf2.
      else.
        clear rv_ok.
      endif.
    endif.

    check rv_ok eq abap_true.
    if ls_ltdxt is not initial.
      delete from ltdxt client specified
                          where mandt     eq ls_ltdxt-mandt
                            and relid     eq ls_ltdxt-relid
                            and report    eq ls_ltdxt-report
                            and handle    eq ls_ltdxt-handle
                            and log_group eq ls_ltdxt-log_group
                            and username  eq ls_ltdxt-username
                            and type      eq ls_ltdxt-type
                            and variant   eq ls_ltdxt-variant.

      insert ltdxt from ls_ltdxt.
      if sy-dbcnt ne 1.
        clear rv_ok.
      endif.
    endif.

    check rv_ok eq abap_true.
    if ls_ltdxd is not initial.
      delete from ltdxd client specified
                        where mandt     eq ls_ltdxd-mandt
                          and relid     eq ls_ltdxd-relid
                          and report    eq ls_ltdxd-report
                          and handle    eq ls_ltdxd-handle
                          and log_group eq ls_ltdxd-log_group
                          and username  eq ls_ltdxd-username
                          and type      eq ls_ltdxd-type
                          and variant   eq ls_ltdxd-variant.

      insert ltdxd from ls_ltdxd.
      if sy-dbcnt ne 1.
        clear rv_ok.
      endif.
    endif.

    check rv_ok eq abap_true.
    if ls_ltdxs is not initial.
      delete from ltdxs client specified
                        where mandt     eq ls_ltdxs-mandt
                          and relid     eq ls_ltdxs-relid
                          and report    eq ls_ltdxs-report
                          and handle    eq ls_ltdxs-handle
                          and log_group eq ls_ltdxs-log_group
                          and variant   eq ls_ltdxs-variant.

      insert ltdxs from ls_ltdxs.
      if sy-dbcnt ne 1.
        clear rv_ok.
      endif.
    endif.

    if rv_ok eq abap_true.
      commit work.
    endif.
  endmethod.


  method write_file_to_path.

    constants: lc_msg_id           type syst-msgid value '00',
               lc_msg_no           type syst-msgno value '001',
               lc_logical_filename type filename-fileintern value 'EHS_FTAPPL_2'. " same as that used in CG3Y

    clear rv_uploaded.

    data(lv_filepath) = iv_filepath.
    data(lt_data) = it_data.
    data(lv_overwrite) = iv_overwrite.
    data(lv_file_length) = iv_file_length.
    data(lo_helper) = new lcl_helper( ).

* ---- input parameter validation ---- *
    if lv_filepath is initial or lt_data is initial.
      data(lv_msg) = conv bapi_msg( 'Mandatory input missing: Filepath/Binary data' ).
      raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
    endif.

    if lt_data is not initial and lv_file_length is initial.
      lv_msg = 'Please supply length of binary data for accurate conversion'.
      raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
    endif.

* ---- Compute filepath type ---- *
    data(lv_file_path_type) = cond #( when lv_filepath ca gc_path_sep-windows then gc_path_sep-windows
                                      when lv_filepath ca gc_path_sep-unix then gc_path_sep-unix ).

    " Note on .bin : => file extension does not matter. The data is stored in binary format anyways...
    " ...and can be directly converted to target extension while reading/downoading
    case lv_file_path_type.
      when gc_path_sep-windows.
        if sy-batch = abap_true.
          lv_msg = 'GUI not available in background mode'.
          raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
        endif.
        if strlen( lv_filepath ) = 1 and lv_filepath = gc_path_sep-windows. " indicator that the default filepath is to be used
          lv_filepath = cond #( when lo_helper is bound then lo_helper->get_temp_file_path(
                                                               exporting
                                                                 iv_front_end  = abap_true ) ).
        endif.
        if lv_filepath is not initial.
          " to-do: validate filename/path
          if lo_helper->validate_file_path(
               exporting
                 iv_filepath          = lv_filepath
                 iv_contains_filename = abap_true ).
            if lv_file_length is initial.
              lv_file_length = xstrlen( cl_bcs_convert=>solix_to_xstring(
                                          exporting
                                            it_solix = lt_data ) ).
            endif.
            cl_gui_frontend_services=>gui_download(
              exporting
                bin_filesize              = cond #( when lv_file_length is not initial then lv_file_length )
                filename                  = lv_filepath
                filetype                  = conv #( gc_extension-bin )
                confirm_overwrite         = lv_overwrite
              importing
                filelength                = data(lv_bytes_transferred)
              changing
                data_tab                  = lt_data
              exceptions
                file_write_error          = 1
                no_batch                  = 2
                gui_refuse_filetransfer   = 3
                invalid_type              = 4
                no_authority              = 5
                unknown_error             = 6
                header_not_allowed        = 7
                separator_not_allowed     = 8
                filesize_not_allowed      = 9
                header_too_long           = 10
                dp_error_create           = 11
                dp_error_send             = 12
                dp_error_write            = 13
                unknown_dp_error          = 14
                access_denied             = 15
                dp_out_of_memory          = 16
                disk_full                 = 17
                dp_timeout                = 18
                file_not_found            = 19
                dataprovider_exception    = 20
                control_flush_error       = 21
                not_supported_by_gui      = 22
                error_no_gui              = 23
                others                    = 24 ).
            if sy-subrc <> 0.
              message id sy-msgid type sy-msgty number sy-msgno
                         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_msg.
              raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
            else.
              message |{ lv_bytes_transferred } bytes transferred to { lv_filepath }| type 'S'.
              " verify data upload...
              rv_uploaded = check_file_exists( iv_filepath = lv_filepath ).
            endif.
          else.
            lv_msg = 'Invalid filepath specified'.
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          endif.
        endif.
      when gc_path_sep-unix.
* ---- set default app server filepath if not supplied ---- *
        if strlen( lv_filepath ) = 1 and lv_filepath = gc_path_sep-unix. " indicator that the default filepath is to be used
          lv_filepath = cond #( when lo_helper is bound then lo_helper->get_temp_file_path(
                                                               exporting
                                                                 iv_app_server  = abap_true ) ).
        endif.

        if lv_filepath is not initial.
* ---- check the authority to write the file to the application server ---- *
*    if iv_with_auth_check = abap_true.
          data(lv_program) = conv authb-program( sy-cprog ).
          data(lv_auth_filename) = conv authb-filename( lv_filepath ).
          call function 'AUTHORITY_CHECK_DATASET'
            exporting
              program          = lv_program  " ABAP program in which access occurs
              activity         = sabc_act_write " Access Type (See Function Documentation)
              filename         = lv_auth_filename " File name
            exceptions
              no_authority     = 1        " You are not authorized for this access
              activity_unknown = 2        " Access type unknown
              others           = 3.
          if sy-subrc <> 0.
            case sy-subrc.
              when 1.
                lv_msg = 'Not authorised to write the file to app server'.
              when others.
                lv_msg = 'File open error'.
            endcase.
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          endif.
*    endif.

* ---- validate physical filename against logical filename ---- *
          call function 'FILE_VALIDATE_NAME'
            exporting
              logical_filename           = lc_logical_filename
            changing
              physical_filename          = lv_filepath
            exceptions
              logical_filename_not_found = 1
              validation_failed          = 2
              others                     = 3.
          if sy-subrc <> 0.
            message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into lv_msg.
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          endif.

* ---- handle overwriting ---- *
          if lv_overwrite = abap_false
            and check_file_exists( exporting iv_filepath = lv_filepath ). " generic file existence check, works for both frontend and app server
            lv_msg = 'File already exists on app server.'.  " overwriting not requested
            raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          elseif lv_overwrite = abap_true
            and check_file_exists( exporting iv_filepath = lv_filepath ).
            try.
                delete dataset lv_filepath.  " overwriting requested, so delete the file first if it exists
                " alternative "open dataset dset for appending..."
              catch cx_sy_file_authority into data(lox_file_authority).
                lv_msg = lox_file_authority->get_text( ).
                raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
              catch cx_sy_file_open into data(lox_file_open).
                lv_msg = lox_file_open->get_text( ).
                raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
            endtry.
          endif.

          try.
              " open the dataset for writing
              open dataset lv_filepath for output in binary mode.
              if sy-subrc = 0.
                try.
                    " lines and file length are required for exact calculation of size of last line of data
                    data(lv_lines) = lines( lt_data ).

                    " In case of direct ct_data input, file length is supplied by user since...
                    " ...it's not possible to calculate exact binary file size from lt_data alone
                    " But if its not supplied last resort is to compute length using binary tab...
                    if lv_file_length is initial.
                      lv_file_length = xstrlen( cl_bcs_convert=>solix_to_xstring(
                                                 exporting
                                                   it_solix = lt_data ) ).
                    endif.

                    loop at lt_data assigning field-symbol(<ls_data>).
                      " this part....
                      " ...is required since the size of data in the last line may be less than 255 bytes
                      " If we do not pass the exact binary length, some non-existant empty data is pushed...
                      " ...which renders the file unreadable/uncompatible
                      data(lv_max_len) = xstrlen( <ls_data>-line ). " max length of each line
                      if lv_lines gt 1.
                        if sy-tabix = lv_lines. " calculate length of last line...
                          data(lv_len) = lv_file_length - ( lv_max_len * ( lv_lines - 1 ) ).
                          " last line size = total file size - totat length of previous lines
                        else.
                          lv_len = lv_max_len.  " all other lines = max line length
                        endif.
                      else.
                        lv_len = lv_file_length.  " lines = 1 means entire filedata is one line
                      endif.
                      " end of this part
                      " write binary data to dataset line by line
                      transfer <ls_data>-line to lv_filepath length lv_len.  " pass exact length of data to be transfered
                      clear lv_len.
                    endloop.

                    try.
                        " close the dataset after writing
                        close dataset lv_filepath.

                        message |{ lv_file_length } bytes transferred to { lv_filepath }| type 'S'.

                        " verify data upload...
                        rv_uploaded = check_file_exists( iv_filepath = lv_filepath ).
                      catch cx_sy_file_close into data(lox_file_close).
                        lv_msg = lox_file_close->get_text( ).
                        raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
                    endtry.
                  catch cx_sy_file_io into data(lox_file_io).
                    lv_msg = lox_file_io->get_text( ).
                    raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
                  catch cx_sy_file_open_mode into data(lox_file_open_mode).
                    lv_msg = lox_file_open_mode->get_text( ).
                    raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
                  catch cx_sy_file_access_error into data(lox_file_access_error).
                    lv_msg = lox_file_access_error->get_text( ).
                    raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
                endtry.
              else.
                lv_msg = 'File open error'.
                raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
              endif.
            catch cx_sy_file_authority into lox_file_authority.
              lv_msg = lox_file_authority->get_text( ).
              raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
            catch cx_sy_file_open into lox_file_open.
              lv_msg = lox_file_open->get_text( ).
              raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
            catch cx_sy_too_many_files into data(lox_too_many_files).
              lv_msg = lox_too_many_files->get_text( ).
              raise exception type zcx_generic message id lc_msg_id type 'E' number lc_msg_no with lv_msg.
          endtry.

        endif.
      when others.
    endcase.
  endmethod.


  method xml_to_abap.

    " Premise is to transform/map any xml string to given abap data
    " For this we first convert string xml to as/canonical-xml
    " Then convert as-xml to abap data
    " Conversion to intermediate as-xml is manadatory since we are relying on using the built-in ID transformation that can only process as-xml

    data: xml_xstr   type xstring,  " hexa-decimal string generated from input xml string
          xml_xsd    type xsdany,   " interim xsd string generated from hexa-decimal string -> used to generate tabular format from xsd...
          " ...for asxml generation
          asxml_xstr type xstring,  " hexa-decimal as-xml string generated from xsd table
          asxml_str  type string.   " readable as-xml string

*{   INSERT         SBXK900102                                        1
    clear: xml_xstr, xml_xsd, asxml_xstr, asxml_str.
*}   INSERT
    " convert xml string to xstring
    xml_xstr = cl_abap_codepage=>convert_to( xml_input ).

    " convert xstring to xsd
    xml_xsd = xml_xstr.

    " convert xsd to name-value tabular format for as-xml conversion
    data(source_tab) = value abap_trans_srcbind_tab(
          ( name = root value = ref #( xml_xsd ) ) ).
    try.
        " generate as-xml in xstring format from xsd using built-in ID transformation
        call transformation demo_id_upper_lower "id
        parameters mode = 'UP'
        source (source_tab)
        result xml asxml_xstr.

        " convert as-xml xstring to as-xml string(readable)
        asxml_str = cl_abap_codepage=>convert_from( asxml_xstr ).
*{   INSERT         SBXK900102                                        2
        asxml_out = asxml_str.
*}   INSERT

        " mapping between xml and abap data is case-sensitive
        translate root to upper case. " (Since field-names in abap data are always UPPER CASE)

        " generate a skeleton table to hold the transformed data in name-value tabular format
        data(result_tab) = value abap_trans_resbind_tab(
        ( name = root value = ref #( abap_out ) ) ).  " name = dynamic root element, value = ref of abap data(type underlying ddic/pgm struct)

        " NAME          | VALUE
        " valueOf(root) | ->(ref to) abap_out

        call transformation demo_id_upper_lower "id
        parameters mode = 'UP'
        source xml asxml_str
        result (result_tab).

        if result_tab is not initial.
          read table result_tab into data(result_wa) index 1.
          if sy-subrc = 0.
            assign result_wa-value->* to field-symbol(<abap_out>).  " get mappe data from ref
            if <abap_out> is assigned.
              abap_out = <abap_out>.
            endif.
          endif.
        endif.

      catch cx_transformation_error into data(lo_tr_cx).
        " Transaformation error
        data(ex_text) = lo_tr_cx->get_text( ).
      catch cx_dynamic_check into data(lo_fs_cx).
        " fs error
        ex_text = lo_fs_cx->get_text( ).
    endtry.

  endmethod.
ENDCLASS.
