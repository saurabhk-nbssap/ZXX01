function zfm_send_whatsapp_msg.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MESSAGE_TEXT) TYPE  STRING
*"     VALUE(IV_MOBILE_NUMBER) TYPE  TELF1
*"     VALUE(IV_COUNTRY_CODE) TYPE  SKTELFTO DEFAULT 91
*"  EXPORTING
*"     VALUE(EV_OK) TYPE  BOOLEAN
*"----------------------------------------------------------------------
  clear ev_ok.
  check iv_message_text is not initial.   " insert error handling
  check iv_mobile_number is not initial.  " insert error handling
  data(lv_message_text) = iv_message_text.
  data(lv_mobile_number) = iv_mobile_number.

  condense iv_mobile_number no-gaps.

  " Add a country code if not present
  if lv_mobile_number+0(1) ne '+'.  " supplied number does not include a country code
    lv_mobile_number = |+{ cond #( when iv_country_code is not initial then iv_country_code else '91' ) }{ lv_mobile_number }|. " India
  endif.

  " check for a valid mobile number
  data(lv_pattern) = '^\+[1-9]{1}[0-9]{3,14}$'.
  try.
      data(lo_regex) = new cl_abap_regex( pattern = lv_pattern ).
    catch cx_sy_regex.
  endtry.
  if lo_regex is bound.
    try.
        data(lv_matches) = lo_regex->create_matcher(
                             exporting
                               text = lv_mobile_number )->match( ).
      catch cx_sy_matcher.
    endtry.
    check lv_matches eq abap_true.  " insert error handling
  endif.

  clear: lv_matches, lv_pattern.
  free lo_regex.

  " create autoremote url to trigger the task in tasker
  data(lv_autoremote_device_key) =
    |cW2QKLyawYE:APA91bHLdCHk3jha_YLkaZe1uQW5eXiRduTVOB-beSMdAjQtFWaZKyACwWnoyPVUJc4NH6HJO0Z85-499AV-BJlPrsUxbgJm3XRxeyTmwZwT5qH4rY5LuVrwj7ILkTiagrKjf3WDj8Gx|.

  data(lv_url) = |https://| && " protocol
                 |autoremotejoaomgcd.appspot.com| && " domain/host
                 |:443| && " port
                 |/sendmessage| && " path
                 |?key={ lv_autoremote_device_key }&message=ARWTMSG=:={ lv_mobile_number }=:={ lv_message_text }|.  " parameters

  check lv_url is not initial.
  cl_http_client=>create_by_url(
    exporting
      url                = lv_url
    importing
      client             = data(lo_http_client)
    exceptions
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      others             = 4 ).   " insert error handling

  check lo_http_client is bound.
  lo_http_client->send(
    exceptions
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4
      others                     = 5 ).   " insert error handling

  check sy-subrc is initial.
  " trigger the url and recieve the result as plain text
  lo_http_client->receive(
    exceptions
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      others                     = 4 ).   " insert error handling

  data(lv_response) = lo_http_client->response->get_cdata( ).

  if condense( to_upper( lv_response ) ) eq 'OK'.   " insert error handling
    ev_ok = abap_true.
  endif.

endfunction.
