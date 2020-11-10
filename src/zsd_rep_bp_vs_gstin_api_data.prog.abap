*&---------------------------------------------------------------------*
*& Report ZSD_REP_BP_VS_GSTIN_API_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report sy-cprog.

*--------------------------------------------------------------------*
* Global data
*--------------------------------------------------------------------*
" placeholder
*--------------------------------------------------------------------*
* local class definitions
*--------------------------------------------------------------------*
class lcl_app definition.
  public section.
    class-data:
      " Customer
      mv_customer      type kna1-kunnr,
      mv_cus_comp_code type knb1-bukrs,
      mv_sales_org     type knvv-vkorg,
      mv_dist_chnl     type knvv-vtweg,
      mv_division      type knvv-spart,
      mv_cus_acc_grp   type kna1-ktokd,
      mv_cus_gstin     type kna1-stcd3,

      " Vendor
      mv_vendor        type lfa1-lifnr,
      mv_ven_comp_code type lfb1-bukrs,
      mv_purch_grp     type lfm1-ekorg,
      mv_ven_acc_grp   type lfa1-ktokk,
      mv_ven_gstin     type lfa1-stcd3.

    class-methods:
      sel_screen_pbo,
      sel_screen_pai.

    methods: process.

  protected section.
    " placeholder

  private section.
    " placeholder
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.
    " placeholder

  private section.
    " placeholder
endclass.
*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
selection-screen begin of block sel with frame title text-sel.
selection-screen begin of block ptyp with frame title ptyp.
parameters:
  r_cus radiobutton group ptyp user-command ptyp default 'X',
  r_ven radiobutton group ptyp.
selection-screen end of block ptyp.

selection-screen begin of block cus with frame title text-cus.
select-options:
  s_cust   for lcl_app=>mv_customer      modif id cus,
  s_cuscmp for lcl_app=>mv_cus_comp_code modif id cus,
  s_sorg   for lcl_app=>mv_sales_org     modif id cus,
  s_dchnl  for lcl_app=>mv_dist_chnl     modif id cus,
  s_div    for lcl_app=>mv_division      modif id cus,
  s_cusacc for lcl_app=>mv_cus_acc_grp   modif id cus,
  s_cusgst for lcl_app=>mv_cus_gstin     modif id cus.
selection-screen end of block cus.

selection-screen begin of block ven with frame title text-ven.
select-options:
  s_vend    for lcl_app=>mv_vendor        modif id ven,
  s_vencmp  for lcl_app=>mv_ven_comp_code modif id ven,
  s_porg    for lcl_app=>mv_purch_grp     modif id ven,
  s_venacc  for lcl_app=>mv_ven_acc_grp   modif id ven,
  s_vengst  for lcl_app=>mv_ven_gstin     modif id ven.
selection-screen end of block ven.

selection-screen begin of block cmt with frame title text-cmt.
selection-screen begin of line.
selection-screen comment 1(75) cus1 modif id cus.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment 1(75) cus2 modif id cus.
selection-screen end of line.
selection-screen end of block cmt.
selection-screen end of block sel.
*--------------------------------------------------------------------*
* pre-selection screen events
*--------------------------------------------------------------------*
load-of-program.
  " placeholder

initialization.
  ptyp = 'Partner Type'.
  cus1 = '@MA@ Non-INDIAN( Country <> ''IN'' ) customers are excluded'.
  cus2 = '@MA@ SEZ( State Code = ''Z1'' ) customers are excluded'.
*--------------------------------------------------------------------*
* selection screen events
*--------------------------------------------------------------------*
at selection-screen output.
  lcl_app=>sel_screen_pbo( ).

at selection-screen.
  lcl_app=>sel_screen_pai( ).
*--------------------------------------------------------------------*
* start-of-selection
*--------------------------------------------------------------------*
start-of-selection.
  lcl_main=>start( ).
*--------------------------------------------------------------------*
* end-of-selection
*--------------------------------------------------------------------*
end-of-selection.

*--------------------------------------------------------------------*
* local class implementation
*--------------------------------------------------------------------*
class lcl_app implementation.
  method sel_screen_pbo.
    loop at screen.
      if screen-group1 = 'CUS'.
        if r_cus = abap_true.
          screen-active = '1'.
        else.
          screen-active = '0'.
        endif.
        modify screen.
      endif.
      if screen-group1 = 'VEN'.
        if r_ven = abap_true.
          screen-active = '1'.
        else.
          screen-active = '0'.
        endif.
      endif.
      modify screen.
    endloop.
  endmethod.

  method sel_screen_pai.
  endmethod.

  method process.
    data:
      begin of ls_data,
        cus_ven        type c length 10,
        acc_grp        type c length 4,
        gstin          type stcd3,
        gstin_status   type string,
        title          type anred,
        api_ctb        type string,
        title_status   type string,
        name           type name1_gp,
        api_legal_name type string,
        name_status    type string,
        pincode        type pstlz,
        pincode_status type string,
        state_code     type regio,
        state          type bezei,
        api_state      type string,
        state_status   type string,
        message        type string,
      end of ls_data,
      lt_data like standard table of ls_data.

    clear lt_data.
    case abap_true.
      when r_cus.
        select
          distinct
          a~kunnr as cus_ven,
          a~ktokd as acc_grp,
          a~stcd3 as gstin,
          upper( a~anred ) as title,
          upper( a~name1 ) as name,
          a~pstlz as pincode,
          a~regio as state_code,
          upper( d~bezei ) as state
          from kna1 as a
          inner join knb1 as b
          on a~kunnr = b~kunnr
          inner join knvv as c
          on a~kunnr = c~kunnr
          inner join t005u as d
          on a~regio = d~bland
          where a~kunnr in @s_cust[]
          and   a~ktokd in @s_cusacc[]
          and   a~stcd3 in @s_cusgst
          and   a~land1 = 'IN'
          and   a~regio <> 'Z1'  " Exlcude SEZ vendors
          and   b~bukrs in @s_cuscmp[]
          and   c~vkorg in @s_sorg[]
          and   c~vtweg in @s_dchnl[]
          and   c~spart in @s_div[]
          and   d~spras = @sy-langu
          and   d~land1 = 'IN'
          into corresponding fields of table @lt_data.
      when r_ven.
        select
          distinct
          a~kunnr as cus_ven,
          a~ktokk as acc_grp,
          a~stcd3 as gstin,
          upper( a~anred ) as title,
          upper( a~name1 ) as name,
          a~pstlz as pincode,
          a~regio as state_code,
          upper( d~bezei ) as state
          from lfa1 as a
          inner join lfb1 as b
          on a~lifnr = b~lifnr
          inner join lfm1 as c
          on a~lifnr = c~lifnr
          inner join t005u as d
          on a~regio = d~bland
          where a~lifnr in @s_vend[]
          and   ( a~ktokk in @s_venacc[] and a~ktokk <> 'EMPL' )
          and   a~stcd3 in @s_vengst
          and   a~ven_class = ''
          and   a~land1 = 'IN' " registered
          and   b~bukrs in @s_vencmp[]
          and   c~ekorg in @s_porg[]
          and   d~spras = @sy-langu
          and   d~land1 = 'IN'
          into corresponding fields of table @lt_data.
      when others.
    endcase.

    if lt_data is not initial.
      select single @abap_true
        from z6mma_params
        where progname = 'GSTNO_VAL_MASTER'
        and   active_flag = @abap_true
        into @data(lv_validation_active).

      if lv_validation_active = abap_false.
        message 'GST validation is not active - Master switch' type 'S' display like 'E'.
        return.
      endif.

      case abap_true.
        when r_cus.
          select single @abap_true
            from z6mma_params
            where progname = 'GSTNO_VAL_CUST'
            and   active_flag = @abap_true
            into @data(lv_cust_active).

          if lv_cust_active = abap_false.
            message 'GST validation is not active - Customer switch' type 'S' display like 'E'.
            return.
          endif.
        when r_ven.
          select single @abap_true
            from z6mma_params
            where progname = 'GSTNO_VAL_VEND'
            and   active_flag = @abap_true
            into @data(lv_vend_active).

          if lv_vend_active = abap_false.
            message 'GST validation is not active - Vendor switch' type 'S' display like 'E'.
            return.
          endif.
      endcase.

      select param1 as sap_state, param2 as gst_state
        from z6mma_params
        where progname eq 'STATE_MAP'
        into table @data(lt_gst_state).

      select title as title, upper( title_medi ) as title_medi
        from tsad3t
        where langu = @sy-langu
        into table @data(lt_title).

      select upper( gstin_ctb ) as gstin_ctb, sap_title as sap_title
        from zxx_t_ctb_title
        into table @data(lt_ctb_title).

      loop at lt_data assigning field-symbol(<ls_data>).
        try.
            if <ls_data>-cus_ven is initial.
              raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '000'
                with 'Customer/Vendor is a mandarory input'.
            endif.

            if <ls_data>-cus_ven is not initial.
              case abap_true.
                when r_cus.
                  data(lv_customer) = conv kunnr( |{  <ls_data>-cus_ven alpha = in }| ).
                when r_ven.
                  data(lv_vendor) = conv lifnr( |{ <ls_data>-cus_ven alpha = in }| ).
              endcase.
            endif.

            if <ls_data>-cus_ven is not initial and lv_customer is initial and lv_vendor is initial.
              raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '002'. " invalid entity
            endif.

            if <ls_data>-title is initial.
              message id 'Z_BUPA' type 'E' number '023' into <ls_data>-title_status. " Title not supplied/not maintained
            endif.

            if <ls_data>-name is initial.
              message id 'Z_BUPA' type 'E' number '024' into <ls_data>-name_status. " Name not supplied/not maintained
            endif.

            if <ls_data>-pincode is initial.
              message id 'Z_BUPA' type 'E' number '025' into <ls_data>-pincode_status. " Pincode not supplied/not maintained
            else.
              if condense( <ls_data>-pincode ) cn '0123456789' or strlen( <ls_data>-pincode ) ne 6.
                message id 'Z_BUPA' type 'E' number '028' into <ls_data>-pincode_status. " postal code contains non-numeric characters
              endif.
            endif.

            if <ls_data>-state_code is initial.
              message id 'Z_BUPA' type 'E' number '003' into <ls_data>-state_status. " state not supplied/maintained
            endif.

            if <ls_data>-gstin is initial.
              message id 'Z_BUPA' type 'E' number '009' into <ls_data>-gstin_status. " GST number not supplied/maintained
            endif.

            if condense( <ls_data>-gstin ) ca space or <ls_data>-gstin+0(1) eq space.
              message id 'Z_BUPA' type 'E' number '010' into <ls_data>-gstin_status. " GST number should not contain spaces
            endif.

            zcl_helper=>condense_data( changing cs_data = <ls_data> ). " ABAP data to be condensed

            if not line_exists( lt_gst_state[ sap_state = <ls_data>-state_code ] ).
              message id 'Z_BUPA' type 'E' number '011' with <ls_data>-state_code into <ls_data>-state_status. " GST state code not found for &
            endif.

            loop at lt_gst_state into data(ls_gst_state) where sap_state = <ls_data>-state_code.
              condense:
                ls_gst_state-sap_state,
                ls_gst_state-gst_state.

              if <ls_data>-gstin+0(2) eq ls_gst_state-gst_state.
                data(lv_state_ok) = abap_true.
              endif.

              clear ls_gst_state.
            endloop.

            if lv_state_ok = abap_false.
              message id 'Z_BUPA' type 'E' number '012' into <ls_data>-state_status. " State code does not match first 2 characters of GSTIN
            endif.

            " IHDK909105
            zcl_bupa_utilities=>get_gstin_info_with_gstin(
              exporting
                iv_gstin      = <ls_data>-gstin           " Tax Number 3
              importing
                ev_active     = data(lv_gstin_active)     " Is GSTIN Active?
                ev_status     = data(lv_gstin_status)     " Status Text
              receiving
                rs_gstin_info = data(ls_gstin_info) ).

            if lv_gstin_active = abap_false.
              message id 'Z_BUPA' type 'E' number '000'
                with |GSTIN Check: Status: { lv_gstin_status }| into <ls_data>-gstin_status.
            else. " for active cases
              <ls_data>-gstin_status = lv_gstin_status.
              <ls_data>-api_state = to_upper( ls_gstin_info-data-pradr-addr-stcd ).

              " title check - to be implemented
              if strlen( <ls_data>-title ) <> 4 or <ls_data>-title cn '0123456789'.
                " title description is provided - get the code for the query below
                <ls_data>-title = to_upper( <ls_data>-title ).

                try.
                    <ls_data>-title = lt_title[ title_medi = <ls_data>-title ]-title.
                  catch cx_sy_itab_line_not_found ##no_handler.
                    message id 'Z_BUPA' type 'E' number '023' into <ls_data>-title_status. " Title not supplied/not maintained
                endtry.
                if <ls_data>-title is initial.
                  message id 'Z_BUPA' type 'E' number '023' into <ls_data>-title_status. " Title not supplied/not maintained
                endif.
              endif.

              <ls_data>-api_ctb = conv char100( to_upper( ls_gstin_info-data-ctb ) ).
              if <ls_data>-api_ctb is initial.
                message id 'Z_BUPA' type 'E' number '032' into <ls_data>-title_status. " Constitution Of Business not available with GSTIN
              endif.

              if not line_exists( lt_ctb_title[ gstin_ctb = <ls_data>-api_ctb sap_title = <ls_data>-title ] ).
                message id 'Z_BUPA' type 'E' number '029' into <ls_data>-title_status. " Title does not match GSTIN "Constitution Of Business"
              endif.
              try.
                  <ls_data>-title = lt_title[ title = <ls_data>-title ]-title_medi.
                catch cx_sy_itab_line_not_found ##no_handler.
                  message id 'Z_BUPA' type 'E' number '023' into <ls_data>-title_status. " Title not supplied/not maintained
              endtry.

              " name check
              <ls_data>-api_legal_name = to_upper( ls_gstin_info-data-lgnm ).
              if to_upper( <ls_data>-name ) <> <ls_data>-api_legal_name.
                message id 'Z_BUPA' type 'E' number '030' into <ls_data>-name_status.
              endif.

              " pincode check
              if not line_exists( ls_gstin_info-data-adadr[ addr-pncd = <ls_data>-pincode ] ).
                message id 'Z_BUPA' type 'E' number '031' into <ls_data>-pincode_status.
              endif.
            endif.

            if <ls_data>-title_status is initial.
              <ls_data>-title_status = 'OK'.
            endif.

            if <ls_data>-name_status is initial.
              <ls_data>-name_status = 'OK'.
            endif.

            if <ls_data>-pincode_status is initial.
              <ls_data>-pincode_status = 'OK'.
            endif.

            if <ls_data>-state_status is initial.
              <ls_data>-state_status = 'OK'.
            endif.
          catch zcx_generic into data(lox_generic).
            <ls_data>-message = lox_generic->get_text( ).
        endtry.

        clear:
          lox_generic,
          ls_gstin_info,
          lv_gstin_active,
          lv_gstin_status,
          ls_gst_state,
          lv_state_ok,
          lv_customer,
          lv_vendor.
      endloop.

      data:
        lo_column           type ref to cl_salv_column_table,
        lox_salv_not_found  type ref to cx_salv_not_found ##needed,
        lox_move_cast_error type ref to cx_sy_move_cast_error ##needed,
        lv_index            type i.

      define set_field_text.
        try.
            free:
              lo_column,
              lox_salv_not_found,
              lox_move_cast_error.

            lv_index = lv_index + 1.

            lo_column ?= lo_columns->get_column( exporting columnname = &1 ).
            if lo_column is bound.
              lo_column->set_long_text( exporting value = conv #( &2 ) ).
              lo_column->set_medium_text( exporting value = conv #( &3 ) ).
              lo_column->set_short_text( exporting value = conv #( &4 ) ).
              lo_columns->set_column_position( exporting columnname = &1 position = lv_index ).
            endif.
          catch cx_salv_not_found into lox_salv_not_found ##needed ##no_handler.
          catch cx_sy_move_cast_error into lox_move_cast_error ##needed ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
        endtry.
      end-of-definition.

      define set_field_technical.
        try.
            free:
              lo_column,
              lox_salv_not_found,
              lox_move_cast_error.

            lo_column ?= lo_columns->get_column( exporting columnname = &1 ).
            if lo_column is bound.
              lo_column->set_technical( exporting value = if_salv_c_bool_sap=>true ).
            endif.
          catch cx_salv_not_found into lox_salv_not_found ##needed ##no_handler.
          catch cx_sy_move_cast_error into lox_move_cast_error ##needed ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
        endtry.
      end-of-definition.

      define set_key_field.
        try.
            free:
              lo_column,
              lox_salv_not_found,
              lox_move_cast_error.

            lo_column ?= lo_columns->get_column( exporting columnname = &1 ).
            if lo_column is bound.
              lo_column->set_key( exporting value = if_salv_c_bool_sap=>true ).
            endif.
          catch cx_salv_not_found into lox_salv_not_found ##needed ##no_handler.
          catch cx_sy_move_cast_error into lox_move_cast_error ##needed ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
        endtry.
      end-of-definition.

      try.
          cl_salv_table=>factory(
            importing
              r_salv_table   = data(lo_alv)              " Basis Class Simple ALV Tables
            changing
              t_table        = lt_data ).

          if lo_alv is bound.
            lo_alv->get_functions( )->set_all( exporting value = if_salv_c_bool_sap=>true ).
            lo_alv->get_display_settings( )->set_striped_pattern( exporting value = if_salv_c_bool_sap=>true ).

            data(lo_columns) = lo_alv->get_columns( ).
            if lo_columns is bound.
              data(lv_entity) = cond string( when r_cus = abap_true then 'Customer'
                                             when r_ven = abap_true then 'Vendor' ).
              set_field_text:
                'CUS_VEN'                   lv_entity                       lv_entity               lv_entity,
                'ACC_GRP'                   'Account Group'                 'Acc. Grp.'             'Acc Grp',
                'GSTIN'                     'GSTIN'                         'GSTIN'                 'GSTIN',
                'GSTIN_STATUS'              'GSTIN Status'                  'GSTIN Status'          'GSTIN Sts',
                'TITLE'                     'Title'                         'Title'                 'Title',
                'API_CTB'                   'API Const. Of Business'        'API CTB'               'API CTB',
                'TITLE_STATUS'              'Title Status'                  'Title Status'          'Title Sts',
                'NAME'                      'Name'                          'Name'                  'Name',
                'API_LEGAL_NAME'            'API Legal Name'                'API Legal Name'        'API Name',
                'NAME_STATUS'               'Name Status'                   'Name Status'           'Name Sts',
                'PINCODE'                   'Pincode'                       'Pincode'               'Pincode',
                'PINCODE_STATUS'            'Pincode Status'                'Pincode Status'        'Pin Sts',
                'STATE_CODE'                'State Code'                    'State Code'            'State Code',
                'STATE'                     'State'                         'State'                 'State',
                'API_STATE'                 'API State'                     'API State'             'API State',
                'STATE_STATUS'              'State Status'                  'State Status'          'State Sts',
                'MESSAGE'                   'Message'                       'Message'               'Message'.

              set_key_field:
                'CUS_VEN',
                'GSTIN',
                'GSTIN_STATUS'.

              " optimise column width as per content length
              lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
              lo_columns->set_key_fixation( exporting value = if_salv_c_bool_sap=>true ).
            endif.

            lo_alv->get_layout( )->set_default( exporting value = if_salv_c_bool_sap=>true ).
            lo_alv->get_layout( )->set_save_restriction( exporting value = if_salv_c_layout=>restrict_none ).

            lo_alv->display( ).
          endif.
        catch cx_salv_msg ##no_handler. " ALV: General Error Class with Message
      endtry.
    else.
      message 'No data found for selection criteria' type 'S' display like 'E'.
      return.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    try.
        new lcl_app( )->process( ).
      catch cx_root into data(lox_root).
        message lox_root->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
endclass.
