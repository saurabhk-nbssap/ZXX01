class ZCL_EMAIL definition
  public
  final
  create public .

public section.

  data ATTACHMENT type ZFI_S_VP_ATTACHMENT .
*    begin of attachment,
*        att_type    type soodk-objtp,
*        att_subj    type sood-objdes,
*        att_content type string,
*      end of attachment .
  data RECIPIENT type ZFI_S_VP_RECIPIENT .
  data:
*    begin of recipient,
*        copy      type abap_bool ,  " 'X' = CC, '' = TO
*        recipient type string,
*      end of recipient .
    attachments like standard table of attachment .
  data:
    recipients like standard table of recipient .

  methods SEND_EMAIL
    importing
      value(SUBJECT) type STRING
      value(SENDER) type STRING
      value(BODY) type SOLI_TAB optional
      value(BODY_OBJ_TYPE) type SOODK-OBJTP optional
      value(RECIPIENTS) like RECIPIENTS
      value(ATTACHMENTS) like ATTACHMENTS optional
    exporting
      value(SENT) type ABAP_BOOL .
  methods ITAB_TO_STRING
    importing
      value(HEADER) type STRING optional
      value(TABLE) type STANDARD TABLE
    exporting
      value(TAB_STRING) type STRING .
  methods TAB_TO_SPOOL
    importing
      value(TABLE) type STANDARD TABLE
      value(IV_HEADERS) type I optional
    exporting
      value(SPOOL_NO) type SY-SPONO .
  methods CREATE_ATTACHMENT
    importing
      value(ATTACHMENT) like ATTACHMENT
    exporting
      value(OUTPUT_SOLIX_TAB) type SOLIX_TAB
      value(DOC_SIZE) type SO_OBJ_LEN .
  methods SEND_INDUCTION_EMAIL
    importing
      value(IV_SALUTATION) type STRING
      value(IT_RECIPIENT) like RECIPIENTS
      value(IV_SENDER) type STRING
    exporting
      value(EV_SENT) type ABAP_BOOL .
  methods DELETE_CONF_EMAIL_ON_SEP
    importing
      value(IV_PERNR) type PA0000-PERNR
    returning
      value(RV_OK) type ABAP_BOOL .
  protected section.

private section.
ENDCLASS.



CLASS ZCL_EMAIL IMPLEMENTATION.


  method create_attachment.
    data: code_page type abap_encod.
    data: l_rex_bcs type ref to cx_bcs.

    free l_rex_bcs.
    refresh output_solix_tab.
    clear: doc_size, code_page.
    check attachment is not initial.
    case attachment-att_type.
      when 'XLS'.
        try.
            cl_bcs_convert=>string_to_solix(
                              exporting
                                iv_string   = attachment-att_content     " Delimited string converted from itab
                                iv_codepage = '4103'        " For MS Excel
                                iv_add_bom  = abap_true
                              importing
                                et_solix    = output_solix_tab    " The binary, XLS file
                                ev_size     = doc_size ).
          catch cx_bcs into l_rex_bcs.
            message l_rex_bcs->get_text( ) type 'S' display like 'E'.
            return.
        endtry.
      when 'PDF'. " for spool send content as SP:Spool_NO; for al11 path send content as AL:path(case-sensitive)
        case attachment-att_content+0(2).
          when 'SP'.  " spool no
            shift attachment-att_content by 3 places left.
            condense attachment-att_content.

            data spool_no type tsp01-rqident.
            data rq       type tsp01.
            data bin_size type i.
            data dummy    type table of rspoattr.
            data pdf_xstring type xstring.

            clear: spool_no, rq, bin_size, dummy, pdf_xstring.

            spool_no = attachment-att_content.

*   ------------ get attributes of spool request ---------------------
            call function 'RSPO_GET_ATTRIBUTES_SPOOLJOB'
              exporting
                rqident     = spool_no
              importing
                rq          = rq
              tables
                attributes  = dummy
              exceptions
                no_such_job = 1
                others      = 2.
            if sy-subrc <> 0.
              message e126(po) with spool_no.
            endif.

*   --- convert spool request into PDF, dependent on document type ---
            if rq-rqdoctype = 'OTF' or rq-rqdoctype = 'SMART'.
              call function 'CONVERT_OTFSPOOLJOB_2_PDF'
                exporting
                  src_spoolid              = spool_no
                  no_dialog                = 'X'
                  pdf_destination          = 'X'
                  no_background            = 'X'
                importing
                  pdf_bytecount            = bin_size
                  bin_file                 = pdf_xstring
                exceptions
                  err_no_otf_spooljob      = 1
                  err_no_spooljob          = 2
                  err_no_permission        = 3
                  err_conv_not_possible    = 4
                  err_bad_dstdevice        = 5
                  user_cancelled           = 6
                  err_spoolerror           = 7
                  err_temseerror           = 8
                  err_btcjob_open_failed   = 9
                  err_btcjob_submit_failed = 10
                  err_btcjob_close_failed  = 11
                  others                   = 12.
              if sy-subrc <> 0.
                message e712(po) with sy-subrc 'CONVERT_OTFSPOOLJOB_2_PDF'.
              endif.
            elseif rq-rqdoctype = 'LIST'.
              call function 'CONVERT_ABAPSPOOLJOB_2_PDF'
                exporting
                  src_spoolid              = spool_no
                  no_dialog                = 'X'
                  pdf_destination          = 'X'
                  no_background            = 'X'
                importing
                  pdf_bytecount            = bin_size
                  bin_file                 = pdf_xstring
                exceptions
                  err_no_abap_spooljob     = 1
                  err_no_spooljob          = 2
                  err_no_permission        = 3
                  err_conv_not_possible    = 4
                  err_bad_destdevice       = 5
                  user_cancelled           = 6
                  err_spoolerror           = 7
                  err_temseerror           = 8
                  err_btcjob_open_failed   = 9
                  err_btcjob_submit_failed = 10
                  err_btcjob_close_failed  = 11
                  others                   = 12.
              if sy-subrc <> 0.
                message e712(po) with sy-subrc 'CONVERT_ABAPSPOOLJOB_2_PDF'.
              endif.
            else.
              message e789(po) with rq-rqdoctype.
            endif.
            doc_size = bin_size.

            output_solix_tab = cl_document_bcs=>xstring_to_solix( pdf_xstring ).
          when 'AL'.  " al11 path
            shift attachment-att_content by 3 places left.
            condense attachment-att_content.

            data: file_path(128) type c.
            data: msg type string.
            clear: file_path, pdf_xstring, msg.

            file_path = attachment-att_content.
            if file_path is not initial.
              try.
                  open dataset file_path for input in binary mode message msg.
                  if sy-subrc = 0.
                    read dataset file_path into pdf_xstring.  "all in one go
                    close dataset file_path.
                  endif.
                catch cx_sy_file_open.
              endtry.

              if pdf_xstring is not initial.
                output_solix_tab = cl_document_bcs=>xstring_to_solix( pdf_xstring ).
                doc_size = xstrlen( pdf_xstring ).
              endif.
            endif.
          when others.
        endcase.
      when others.
    endcase.
  endmethod.


  method delete_conf_email_on_sep.

    break: 6010959, 10106.
    rv_ok = abap_true.  " assume success, unless failure detected
    check iv_pernr is not initial.
    select single massn from pa0000 into @data(lv_separated) where pernr = @iv_pernr and massn = 'I7'.  " separated
    check sy-subrc = 0.

    data lt_sndrec type soxsp2tab.
    refresh lt_sndrec.
    call function 'SX_SNDREC_SELECT'
      exporting
        status      = value soststatus( wait = abap_true future = abap_true ) " only waiting and future/scheduled emails
        description = conv so_obj_des( condense( |*{ iv_pernr alpha = out }*CONFIRMATION*PENDING*| ) )  " search pattern => *Employee_No*CONFIRMATION*PENDING*
      importing
        sndrecs     = lt_sndrec.

    check lt_sndrec is not initial.
    loop at lt_sndrec into data(ls_sndrec).
      try.
          " call email deletion api
          call method cl_sndrec_bcs=>delete       " implicit commit
            exporting
              i_sndrec = conv soxsp2( ls_sndrec ).
        catch cx_bcs.
          rv_ok = abap_false.
      endtry.
      clear: ls_sndrec.
    endloop.
    " cleanup
    refresh lt_sndrec.
  endmethod.


  method itab_to_string.
    data: lo_table type ref to cl_abap_tabledescr,
          lo_struc type ref to cl_abap_structdescr.
    data: lv_index type syst-index.
    constants: tab type abap_char1 value cl_abap_char_utilities=>horizontal_tab.

    check table is not initial.
    assign table to field-symbol(<table>).

    check <table> is assigned.
    free lo_table.
    lo_table ?= cl_abap_tabledescr=>describe_by_data( <table> ). " Get the description of the data

    check lo_table is bound.
    free lo_struc.
    lo_struc ?= lo_table->get_table_line_type( ). " Get table structure

    check lo_struc is bound.
    data(components) = lo_struc->get_components( ). "Get the fields of the structure

    check components is not initial.
    clear lv_index.
    loop at <table> assigning field-symbol(<wa>).
      add 1 to lv_index.
      if lv_index gt 1. " from 2nd row onwards
        " Add a newline character after 1st row upto n-1th row
        tab_string = tab_string && cl_abap_char_utilities=>cr_lf.
      endif.
      do lines( components ) times.
        assign component sy-index of structure <wa> to field-symbol(<fs>).
        " Add separated by tab for 2 or more fields
        if <fs> is assigned.
          read table components into data(component) index sy-index.
          if sy-subrc = 0.
            if to_upper( component-type->type_kind ) = 'D'. " date field
              data(date) = |{ conv d( <fs> ) date = user }|.
              assign date to <fs>.
            endif.
          endif.
*          <fs> = |{ <fs> alpha = in }|.
          if sy-index = lines( components ).
            tab_string = tab_string && <fs>.
          else.
            tab_string = tab_string && <fs> && tab.
          endif.
          unassign <fs>.
        endif.
      enddo.
    endloop.

    " Add header
    check header is supplied and header is not initial.
    tab_string = header && cl_abap_char_utilities=>cr_lf && tab_string.
  endmethod.


  method send_email.
    data: l_sender_req_ref type ref to cl_bcs.
    data: l_smtp_addr      type adr6-smtp_addr.
    data: l_sender_ref     type ref to if_sender_bcs.
    data: l_rex_addr       type ref to cx_address_bcs.
    data: l_uname          type uname.
    data: l_rex_bcs        type ref to cx_bcs.
    data: l_rex_send_req   type ref to cx_send_req_bcs.
    data: l_recipient_ref  type ref to if_recipient_bcs.
    data: l_doc_ref        type ref to cl_document_bcs.
    data: l_rex_doc        type ref to cx_document_bcs.
    data: l_ok             type abap_bool.
    data: att_solix        type solix_tab.
    data: att_size         type so_obj_len.
    data: v_persno         type usr21-persnumber.
    data: v_addrnum        type usr21-addrnumber.

    clear: l_smtp_addr, l_uname, sent.
    free: l_sender_req_ref, l_sender_ref, l_rex_addr, l_rex_bcs, l_rex_send_req.

    if body_obj_type is not supplied or body_obj_type is initial.
      body_obj_type = 'RAW'.
    endif.

    try.
        " sender
        try.
            check sender is not initial.
            l_sender_req_ref = cl_bcs=>create_persistent( ).
            check l_sender_req_ref is bound and subject is not initial.
            l_sender_req_ref->set_message_subject( ip_subject = subject ).
          catch cx_send_req_bcs into l_rex_send_req.
            message l_rex_send_req->get_text( ) type 'S' display like 'E'.
            return.
        endtry.
        try.
            if sender cs '@'. " sender is a direct email address
              l_smtp_addr = condense( sender ).
              l_sender_ref = cl_cam_address_bcs=>create_internet_address( i_address_string = l_smtp_addr ).
            else.
              l_uname = condense( sender ).
              l_sender_ref = cl_sapuser_bcs=>create( i_user =  l_uname ).
            endif.
          catch cx_address_bcs into l_rex_addr.
            message l_rex_addr->get_text( ) type 'S' display like 'E'.
            return.
        endtry.

        try.
            check l_sender_ref is bound and l_sender_req_ref is bound.
            l_sender_req_ref->set_sender( i_sender = l_sender_ref ).
          catch cx_send_req_bcs into l_rex_send_req.
            message l_rex_send_req->get_text( ) type 'S' display like 'E'.
            return.
        endtry.

        " recipient
        check recipients is not initial.
        clear recipient.
        loop at recipients into recipient.
          clear: l_smtp_addr, l_uname.
          try.
              free l_recipient_ref.
              if recipient-recipient cs '@'.  " this is a direct email address
                l_smtp_addr = condense( to_upper( recipient-recipient ) ).  " IHDK900899
              else. " this is a username, derive the emai address below
                l_uname = condense( recipient-recipient ).
                clear: v_persno, v_addrnum.
                select single persnumber addrnumber from usr21 into ( v_persno, v_addrnum ) where bname eq l_uname.
                if v_persno is not initial.
                  select single smtp_addr from adr6 into l_smtp_addr where addrnumber = v_addrnum and persnumber eq v_persno.
                endif.

                if l_smtp_addr is initial.
                  data(v_pernr) = conv pa0105-pernr( l_uname ).
                  select single usrid_long from pa0105 into @data(userid_long)
                    where pernr = @v_pernr and subty = '0010' and endda ge @sy-datum.
                  l_smtp_addr = userid_long.
                endif.
              endif.
              if l_smtp_addr is not initial.
                l_recipient_ref = cl_cam_address_bcs=>create_internet_address( i_address_string = l_smtp_addr ).
              endif.
            catch cx_address_bcs into l_rex_addr.
              message l_rex_addr->get_text( ) type 'S' display like 'E'.
              return.
          endtry.

*     add recipient with its respective attributes to send request
          try.
              check l_recipient_ref is bound.
              l_sender_req_ref->add_recipient(    " IHDK900637
                exporting
                  i_recipient = l_recipient_ref
                  i_copy = recipient-copy
                  i_blind_copy = recipient-blind_copy
                  i_express = abap_true ).
            catch cx_send_req_bcs into l_rex_send_req.
              message l_rex_send_req->get_text( ) type 'S' display like 'E'.
              return.
          endtry.

          clear recipient.
        endloop.

        " message body
        free: l_doc_ref, l_rex_doc.
        try.
            check body is not initial and body_obj_type is not initial and subject is not initial.
            l_doc_ref = cl_document_bcs=>create_document(
                                          i_type    = body_obj_type
                                          i_text    = body
                                          i_subject = conv so_obj_des( subject ) ).

            " Add attachments
            if l_doc_ref is bound and attachments is not initial.
              clear attachment.
              loop at attachments into attachment.
                refresh att_solix.
                clear att_size.
                if attachment-att_solix is not initial.
                  att_solix = attachment-att_solix.
                  describe table att_solix.
                  att_size = sy-tfill * sy-tleng.
                else.
                  me->create_attachment(
                        exporting
                          attachment = attachment
                        importing
                          output_solix_tab = att_solix
                          doc_size         = att_size ).

                endif.
                if att_solix is not initial.
                  l_doc_ref->add_attachment(
                              exporting
                                i_attachment_type    = attachment-att_type
                                i_attachment_subject = attachment-att_subj
                                i_att_content_hex    = att_solix
                                i_attachment_size    = condense( cond #( when attachment-att_size is not initial then attachment-att_size else att_size ) ) ).  " IHDK900967
                endif.

                clear attachment.
              endloop.
            endif.
          catch cx_document_bcs into l_rex_doc.
            message l_rex_doc->get_text( ) type 'S' display like 'E'.
            return.
        endtry.

        try.
            " add document to send request
            l_sender_req_ref->set_document( i_document = l_doc_ref ).
            " mail has to be sent immediately
            l_sender_req_ref->set_send_immediately( i_send_immediately = abap_true ).
            " send
            clear l_ok.
            l_ok = l_sender_req_ref->send( ).
            check l_ok eq abap_true.
            commit work.
            sent = abap_true.
          catch cx_send_req_bcs into l_rex_send_req.
            message l_rex_send_req->get_text( ) type 'S' display like 'E'.
            return.
        endtry.

      catch cx_bcs into l_rex_bcs.
        message l_rex_bcs->get_text( ) type 'S' display like 'E'.
        return.
    endtry.

  endmethod.


  method send_induction_email.
    " IHDK900606: HR: S_K: ZHR_PS: Add induction/welcome email: 13.2.19
    " generate dynamic image from html(containing user name) and embed it in the email body
    data(lt_body) = zcl_helper=>build_induction_email_body( exporting iv_salutation = iv_salutation ).

    check lt_body is not initial.
    send_email(
      exporting
        subject       = conv #( 'Welcome to Indofil family' )
        sender        = conv #( iv_sender )
        body          = lt_body
        body_obj_type = conv #( 'HTM' )
        recipients    = it_recipient
      importing
        sent          = ev_sent ).  " IHDK900610
  endmethod.


  method tab_to_spool.

    check table is not initial.
    data(lo_table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) ).
    if lo_table_descr is bound.
      data(lo_struct_descr) = cast cl_abap_structdescr( lo_table_descr->get_table_line_type( ) ).
    endif.

    data(lt_component) = lo_struct_descr->get_components( ).

    data: t_color type lvc_t_scol.
    append value #( name = 'T_COLOR' type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( t_color ) ) ) to lt_component.

    if lt_component is not initial.
      try.
          data(lo_struct) = cl_abap_structdescr=>create(
                                 exporting
                                   p_components = lt_component ).
        catch cx_sy_struct_creation.
      endtry.
    endif.

    if lo_struct is bound.
      data: ls_table type ref to data.
      create data ls_table type handle lo_struct.
    endif.

    if ls_table is bound.
      try.
          data(lo_tabletype) = cl_abap_tabledescr=>create(
                                exporting
                                  p_line_type = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ls_table ) )
                                  p_table_kind = cl_abap_tabledescr=>tablekind_std ).
        catch cx_sy_table_creation.
      endtry.
    endif.

    if lo_tabletype is bound.
      data: lt_table type ref to data.
      create data lt_table type handle lo_tabletype.
    endif.

    field-symbols: <lt_table> type standard table.

    if lt_table is bound.
      assign lt_table->* to <lt_table>.
    endif.

    if <lt_table> is assigned.
      refresh <lt_table>.
      loop at table assigning field-symbol(<ls>).
        append initial line to <lt_table> assigning field-symbol(<ls_table>).
        move-corresponding <ls> to <ls_table>.
      endloop.

      unassign <ls_table>.
      field-symbols: <ft_color> like t_color.
      loop at <lt_table> assigning <ls_table>.
        if sy-tabix le iv_headers.
          assign component 'T_COLOR' of structure <ls_table> to <ft_color>.
          append initial line to <ft_color> assigning field-symbol(<fs_color>).
          if <fs_color> is assigned.
            <fs_color>-color-col = col_positive.
            <fs_color>-color-int = 0.
            <fs_color>-color-inv = 0.
          endif.
        else.
          exit.
        endif.
      endloop.

      data: o_table type ref to cl_salv_table.
      free: o_table.

      try.
          cl_salv_table=>factory(
          importing
            r_salv_table   = o_table
          changing
            t_table        = <lt_table>
            ).

          if o_table is bound.
            data(o_columns) = o_table->get_columns( ).
            if o_table is bound.
              o_columns->set_headers_visible( exporting value = if_salv_c_bool_sap=>false ).
              try.
                  o_columns->set_color_column( exporting value = 'T_COLOR' ).
                catch cx_salv_data_error.
              endtry.
            endif.
          endif.
        catch cx_salv_msg into data(lo_cx_salv).
      endtry.

      data: print_parameters type pri_params,
            valid_flag       type c length 1.

      clear: print_parameters, valid_flag.
      call function 'GET_PRINT_PARAMETERS'
        exporting
          immediately          = ''
          mode                 = 'BATCH'
          no_dialog            = 'X'
        importing
          out_parameters       = print_parameters
          valid                = valid_flag
        exceptions
          invalid_print_params = 2
          others               = 4.

      if valid_flag = 'X' and sy-subrc = 0.
        if o_table is bound.
          try.
              new-page print on parameters print_parameters no dialog.

              if o_table is bound.
                o_table->display( ).
              endif.

              new-page print off.

            catch cx_sy_nested_print_on into data(lo_cx_print).
          endtry.

          spool_no = sy-spono.
        endif.
      endif.
    endif.
  endmethod.
ENDCLASS.
