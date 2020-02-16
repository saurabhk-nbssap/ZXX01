class ZCL_ASP_CONNECTOR_HELPER_IN definition
  public
  inheriting from CL_ASP_CONNECTOR_HELPER_IN
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EDOC_INTERFACE_CONNECTOR .

  data MO_HELPER type ref to CL_ASP_CONNECTOR_HELPER_IN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ASP_CONNECTOR_HELPER_IN IMPLEMENTATION.


  method IF_EDOC_INTERFACE_CONNECTOR~CANCEL.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~CLEAN_UP_MESSAGES.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~COMMUNICATE_ACTION.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~DELETE_REQUEST.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~DISPLAY_EDOCUMENT.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~NAVIGATE_TO_MONITOR.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~PREPARE_MESSAGES.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~PULL_MESSAGES.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~PULL_REQUEST.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~RESUBMIT.
  endmethod.


  method IF_EDOC_INTERFACE_CONNECTOR~TRIGGER.

   DATA: lv_error_txt    TYPE string,
          lv_interface_id TYPE edoc_interface_id.
    IF iv_interface_id IS INITIAL.
      lv_interface_id = io_edocument->determine_interface_id( iv_process_step = 'REQ_SEND' ).
    ELSE.
      lv_interface_id = iv_interface_id.
    ENDIF.
    CREATE OBJECT mo_helper.
    CASE lv_interface_id.
      WHEN 'IN_SUMMARY_TRANS'.
        IF io_edocument->ms_edocument-edoc_type = 'IN_INVSUM'.
          mo_helper->send_summary_to_asp(  iv_edoc_guid    = iv_edoc_guid
                           io_edocument    = io_edocument
                           iv_test_mode    = iv_test_mode
                           iv_interface_id = lv_interface_id ).
        ENDIF.


      WHEN 'IN_INVOICE_REQUEST'.

        IF io_edocument->ms_edocument-edoc_type = 'IN_INV'.
          mo_helper->sendedoc_to_asp(  iv_edoc_guid    = iv_edoc_guid
                            io_edocument    = io_edocument
                            iv_test_mode    = iv_test_mode
                            iv_interface_id = lv_interface_id ).
        ENDIF.
      WHEN 'IN_STATUS_REQUEST'.

        mo_helper->get_status(  iv_edoc_guid    = iv_edoc_guid
                          io_edocument    = io_edocument
                          iv_test_mode    = iv_test_mode
                          iv_interface_id = lv_interface_id
                          iv_int_version  = 1 ).

      WHEN 'IN_SUMMARY_STATUS'.
        IF io_edocument->ms_edocument-edoc_type = 'IN_INVSUM'.
          mo_helper->get_status(  iv_edoc_guid    = iv_edoc_guid
                            io_edocument    = io_edocument
                            iv_test_mode    = iv_test_mode
                            iv_interface_id = lv_interface_id
                            iv_int_version  = 1 ).
        ENDIF.
      WHEN 'IN_CANCEL_REQUEST'.
        mo_helper->cancel_invoice(  iv_edoc_guid    = iv_edoc_guid
                        io_edocument    = io_edocument
                        iv_test_mode    = iv_test_mode
                        iv_interface_id = lv_interface_id
                        iv_int_version  = 1 ).
      WHEN 'IN_PINVOICE_REQUEST'.
        mo_helper->sendedoc_to_asp_2(  iv_edoc_guid    = iv_edoc_guid
                         io_edocument    = io_edocument
                         iv_test_mode    = iv_test_mode
                         iv_interface_id = lv_interface_id ).
      WHEN 'IN_PSUMMARY_TRANS'.
        mo_helper->send_summary_to_asp_2(  iv_edoc_guid    = iv_edoc_guid
                           io_edocument    = io_edocument
                           iv_test_mode    = iv_test_mode
                           iv_interface_id = lv_interface_id ).
      WHEN 'IN_PSTATUS_REQUEST'.
        mo_helper->get_status_2(  iv_edoc_guid    = iv_edoc_guid
                         io_edocument    = io_edocument
                         iv_test_mode    = iv_test_mode
                         iv_interface_id = lv_interface_id
                         iv_int_version  = 1 ).
      WHEN 'IN_PSUMMARY_STATUS'.
        mo_helper->get_status_2(  iv_edoc_guid    = iv_edoc_guid
                         io_edocument    = io_edocument
                         iv_test_mode    = iv_test_mode
                         iv_interface_id = lv_interface_id
                         iv_int_version  = 1 ).
      WHEN 'IN_PCANCEL_REQUEST'.
        mo_helper->cancel_invoice_2(  iv_edoc_guid    = iv_edoc_guid
                                io_edocument    = io_edocument
                                iv_test_mode    = iv_test_mode
                                iv_interface_id = lv_interface_id
                                iv_int_version  = 1 ).
      WHEN OTHERS.

        CONCATENATE 'No logic defined for interface' iv_interface_id   INTO lv_error_txt.

        cl_edocument=>raise_edoc_exception( iv_error_txt =  lv_error_txt ).

    ENDCASE.


  endmethod.
ENDCLASS.
