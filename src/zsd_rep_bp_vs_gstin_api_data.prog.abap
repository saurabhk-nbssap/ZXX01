*&---------------------------------------------------------------------*
*& Report ZSD_REP_BP_VS_GSTIN_API_DATA
*&---------------------------------------------------------------------*
*& ZXX002 - 6010859; SaurabhK - Tuesday, November 10, 2020 11:38:12
*& IHDK909113 - XX: S_K: ZXX001: Rep: BP vs GSTIN API data: 4.11.20
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
      mv_ven_gstin     type lfa1-stcd3,

      mv_email_addr    type adr6-smtp_addr.

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

selection-screen begin of block src with frame title text-src.
parameters:
  r_api radiobutton group src default 'X',
  r_tab radiobutton group src.
selection-screen end of block src.

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

selection-screen begin of block xls with frame title text-xls.
parameters c_excel as checkbox default 'X'.
select-options s_email for lcl_app=>mv_email_addr.
selection-screen end of block xls.

selection-screen begin of block cmt with frame title text-cmt.
selection-screen begin of line.
selection-screen comment 1(75) cus1 modif id cus.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(75) cus2 modif id cus.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(75) cus3 modif id cus.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(75) ven1 modif id ven.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(75) ven2 modif id ven.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(75) ven3 modif id ven.
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
  cus3 = '@MA@ Unregisterd( Maintained in T-Code ZSD101 ) customers are excluded'.
  ven1 = '@MA@ Non-INDIAN( Country <> ''IN'' ) vendors are excluded'.
  ven2 = '@MA@ Unregistered( Ven_Class <> '''' ) vendors are excluded'.
  ven3 = '@MA@ Employee( Acc Grp = ''EMPL'' ) vendors are excluded'.
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
    " To-Do
  endmethod.

  method process.
*--------------------------------------------------------------------*
    " Declaration
    data lt_data type standard table of zxx_t_gstin_api.
*--------------------------------------------------------------------*
    " DB fetch
    clear lt_data.
    if r_cus = abap_true.
      data lrt_unreg_cust type range of kunnr.
      select 'I' as sign, 'EQ' as option, concat( '0000', param1 ) as low
        from z6mma_params
        where progname = 'UNREG_CUST'
        into table @lrt_unreg_cust.

      if lrt_unreg_cust[] is initial. " otherwise all cusstomers are excluded
        lrt_unreg_cust = value #( ( sign = 'I' option = 'EQ' low = 'XXXXXXXXXX' ) ).
      endif.
    endif.

    if r_api = abap_true.
      case abap_true.
        when r_cus.
          select
            distinct
            a~kunnr as cus_ven,
            'C' as type,
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
            where ( a~kunnr in @s_cust[] and a~kunnr not in @lrt_unreg_cust[] )
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
            order by a~kunnr ascending
            into corresponding fields of table @lt_data.
        when r_ven.
          select
            distinct
            a~lifnr as cus_ven,
            'V' as type,
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
            order by a~lifnr ascending
            into corresponding fields of table @lt_data.
        when others.
      endcase.
*--------------------------------------------------------------------*
      " Check GSTIN validation activation status
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
*--------------------------------------------------------------------*
        " Re-usable masters
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
*--------------------------------------------------------------------*
        " Validate
        loop at lt_data assigning field-symbol(<ls_data>).
          cl_progress_indicator=>progress_indicate(
            exporting
              i_text               = |Validating entity { <ls_data>-cus_ven }...|               " Progress Text (If no message transferred in I_MSG*)
              i_processed          = sy-tabix             " Number of Objects Already Processed
              i_total              = lines( lt_data )     " Total Number of Objects to Be Processed
              i_output_immediately = abap_true ).         " X = Display Progress Immediately

          try.
              if <ls_data>-cus_ven is initial.
                raise exception type zcx_generic message id '00' type 'E' number '001'
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
                raise exception type zcx_generic message id '00' type 'E' number '001'
                  with 'GSTIN Check: Invalid entity (Customer/Vendor)' ' supplied'.
                " invalid entity
              endif.

              if <ls_data>-title is initial.
                <ls_data>-title_status = |GSTIN Check: Title not supplied/not maintained|. " Title not supplied/not maintained
              endif.

              if <ls_data>-name is initial.
                <ls_data>-name_status = |GSTIN Check: Name not supplied/not maintained|. " Name not supplied/not maintained
              endif.

              if <ls_data>-pincode is initial.
                <ls_data>-pincode_status = |GSTIN Check: Pincode not supplied/not maintained|. " Pincode not supplied/not maintained
              else.
                if condense( <ls_data>-pincode ) cn '0123456789' or strlen( <ls_data>-pincode ) ne 6.
                  <ls_data>-pincode_status = |GSTIN Check: Pincode must be 6 character numeric data|. " postal code contains non-numeric characters
                endif.
              endif.

              if <ls_data>-state_code is initial.
                <ls_data>-state_status = |GSTIN Check: State code not supplied/not maintained|. " state not supplied/maintained
              endif.

              if <ls_data>-gstin is initial.
                <ls_data>-gstin_status = |GSTIN Check: GST number not supplied/not maintained|. " GST number not supplied/maintained
              endif.

              if condense( <ls_data>-gstin ) ca space or <ls_data>-gstin+0(1) eq space.
                <ls_data>-gstin_status = |GSTIN Check: GST number should not contain spaces|. " GST number should not contain spaces
              endif.

              zcl_helper=>condense_data( changing cs_data = <ls_data> ). " ABAP data to be condensed

              if not line_exists( lt_gst_state[ sap_state = <ls_data>-state_code ] ).
                <ls_data>-state_status = |GSTIN Check: GST state code not found for { <ls_data>-state_code }|. " GST state code not found for &
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
                <ls_data>-state_status = |GSTIN Check: State code does not match first 2 characters of GSTIN|.
                " State code does not match first 2 characters of GSTIN
              endif.

              " IHDK909105
              zcl_bupa_utilities=>get_gstin_info_with_gstin(
                exporting
                  iv_gstin      = <ls_data>-gstin           " Tax Number 3
                  iv_call_api   = abap_true
                importing
                  ev_active     = data(lv_gstin_active)     " Is GSTIN Active?
                  ev_status     = data(lv_gstin_status)     " Status Text
                  ev_json_response = <ls_data>-api_json_response
                receiving
                  rs_gstin_info = data(ls_gstin_info) ).

              if lv_gstin_active = abap_false.
                message id '00' type 'E' number '001'
                  with |GSTIN Check: Status: { to_upper( lv_gstin_status ) }| into <ls_data>-gstin_status.
              else. " for active cases
                <ls_data>-gstin_status = to_upper( lv_gstin_status ).
                <ls_data>-api_state = to_upper( ls_gstin_info-data-pradr-addr-stcd ).

                " title check - to be implemented
                if strlen( <ls_data>-title ) <> 4 or <ls_data>-title cn '0123456789'.
                  " title description is provided - get the code for the query below
                  <ls_data>-title = to_upper( <ls_data>-title ).

                  try.
                      <ls_data>-title = lt_title[ title_medi = <ls_data>-title ]-title.
                    catch cx_sy_itab_line_not_found ##no_handler.
                      <ls_data>-title_status = |GSTIN Check: Title not supplied/not maintained|. " Title not supplied/not maintained
                  endtry.
                  if <ls_data>-title is initial.
                    <ls_data>-title_status = |GSTIN Check: Title not supplied/not maintained|. " Title not supplied/not maintained
                  endif.
                endif.

                <ls_data>-api_ctb = conv char100( to_upper( ls_gstin_info-data-ctb ) ).
                if <ls_data>-api_ctb is initial.
                  <ls_data>-title_status = |GSTIN Check: "Constitution Of Business" not available with GSTIN|. " Constitution Of Business not available with GSTIN
                endif.

                if not line_exists( lt_ctb_title[ gstin_ctb = <ls_data>-api_ctb sap_title = <ls_data>-title ] ).
                  <ls_data>-title_status = |GSTIN Check: Title does not match GSTIN "Constitution Of Business"|. " Title does not match GSTIN "Constitution Of Business"
                endif.
                try.
                    <ls_data>-title = lt_title[ title = <ls_data>-title ]-title_medi.
                  catch cx_sy_itab_line_not_found ##no_handler.
                    <ls_data>-title_status = |GSTIN Check: Title not supplied/not maintained|. " Title not supplied/not maintained
                endtry.

                " name check
                <ls_data>-api_legal_name = to_upper( ls_gstin_info-data-lgnm ).
                <ls_data>-api_trade_name = to_upper( ls_gstin_info-data-tradenam ).
                if to_upper( <ls_data>-name ) <> <ls_data>-api_legal_name
                  and to_upper( <ls_data>-name ) <> <ls_data>-api_trade_name.
                  <ls_data>-name_status = |GSTIN Check: Name does not match GSTIN legal name|.
                endif.

                " pincode check
                <ls_data>-api_pincode = ls_gstin_info-data-pradr-addr-pncd.
                loop at ls_gstin_info-data-adadr into data(ls_adadr).
                  <ls_data>-api_pincode = |{ <ls_data>-api_pincode }, { ls_adadr-addr-pncd }|.
                  clear ls_adadr.
                endloop.
                if not line_exists( ls_gstin_info-data-adadr[ addr-pncd = <ls_data>-pincode ] )
                  and <ls_data>-pincode <> ls_gstin_info-data-pradr-addr-pncd.
                  <ls_data>-pincode_status = |GSTIN Check: Pincode does not match GSTIN address data|.
                endif.
              endif.

              if <ls_data>-gstin_status = 'ACTIVE'.
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

        modify zxx_t_gstin_api from table lt_data.
        if sy-dbcnt >= 1.
          commit work.
        endif.
      endif.
    endif.
*--------------------------------------------------------------------*
    if r_tab = abap_true.
      case abap_true.
        when r_cus.
          select a~*
            from zxx_t_gstin_api as a
            inner join kna1 as b
            on a~cus_ven = b~kunnr
            inner join knb1 as c
            on b~kunnr = c~kunnr
            inner join knvv as d
            on b~kunnr = d~kunnr
            where ( a~cus_ven in @s_cust[] and a~cus_ven not in @lrt_unreg_cust[] and b~kunnr in @s_cust[] )
            and   a~type = 'C'
            and   ( a~acc_grp in @s_cusacc[] and b~ktokd in @s_cusacc[] )
            and   ( a~gstin in @s_cusgst[] and b~stcd3 in @s_cusgst[] )
            and   b~land1 = 'IN'
            and   b~regio <> 'Z1'  " Exlcude SEZ vendors
            and   c~bukrs in @s_cuscmp[]
            and   d~vkorg in @s_sorg[]
            and   d~vtweg in @s_dchnl[]
            and   d~spart in @s_div[]
            order by a~cus_ven ascending
            into corresponding fields of table @lt_data.
        when r_ven.
          select a~*
            from zxx_t_gstin_api as a
            inner join lfa1 as b
            on a~cus_ven = b~lifnr
            inner join lfb1 as c
            on b~lifnr = c~lifnr
            inner join lfm1 as d
            on b~lifnr = d~lifnr
            where ( a~cus_ven in @s_vend[] and b~lifnr in @s_vend[] )
            and   a~type = 'V'
            and   ( a~acc_grp in @s_venacc[] and b~ktokk in @s_venacc[] and b~ktokk <> 'EMPL' )
            and   ( a~gstin in @s_vengst[] and b~stcd3 in @s_vengst )
            and   b~ven_class = ''
            and   b~land1 = 'IN' " registered
            and   c~bukrs in @s_vencmp[]
            and   d~ekorg in @s_porg[]
            order by b~lifnr ascending
            into corresponding fields of table @lt_data.
        when others.
      endcase.
    endif.
*--------------------------------------------------------------------*
    " ALV  display
    if lt_data is not initial.
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
                'API_LEGAL_NAME'            'API Legal Name'                'API Legal Name'        'API L Name',
                'API_TRADE_NAME'            'API Trade Name'                'API Trade Name'        'API T Name',
                'NAME_STATUS'               'Name Status'                   'Name Status'           'Name Sts',
                'PINCODE'                   'Pincode'                       'Pincode'               'Pincode',
                'API_PINCODE'               'API Pincode'                   'API Pincode'           'API Pin',
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

              set_field_technical:
                'MANDT',
                'TYPE'.

              " optimise column width as per content length
              lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
              lo_columns->set_key_fixation( exporting value = if_salv_c_bool_sap=>true ).
            endif.

            lo_alv->get_layout( )->set_default( exporting value = if_salv_c_bool_sap=>true ).
            lo_alv->get_layout( )->set_save_restriction( exporting value = if_salv_c_layout=>restrict_none ).

            if c_excel = abap_true.
              data(lv_xml) = lo_alv->to_xml(
                               exporting
                                 xml_type = if_salv_bs_xml=>c_type_xlsx
                                 xml_flavour = if_salv_bs_c_tt=>c_tt_xml_flavour_export ).

              delete s_email[] where low na '@'.
              insert value #( sign = 'I' option = 'EQ' low = sy-uname ) into s_email[] index 1.

              new zcl_email( )->send_email(
                exporting
                  subject        = conv #( |ZXX002: BP vs GSTIN API Data Report| )
                  sender         = conv #( 'sapautomail@indofil.com' )
                  body           = value #( ( |<html><body>Hello,<br><br>Your requested report is attached to this email.| )
                                            ( |<br><br>Regards,<br>SAP AutoBot<br><br>| )
                                            ( |<center>This is an auto-generated email. Kindly do not reply.</body></html>| ) )
                  body_obj_type  = 'HTM'
                  recipients     = value #( for ls in s_email[]
                                              index into lv_indx
                                              ( recipient = ls-low
                                                copy = cond #( when lv_indx = 1
                                                               then abap_false
                                                               else abap_true ) ) )
                  attachments    = value #( ( att_type  = conv #( 'BIN' )
                                              att_subj  = conv #( |ZXX002_BP_vs_GSTIN_Data.xlsx| )
                                              att_solix = conv #( cl_bcs_convert=>xstring_to_solix( exporting iv_xstring = lv_xml ) )
                                              att_size  = conv #( xstrlen( lv_xml ) ) ) )
                importing
                  sent           = data(lv_sent) ).
            endif.

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
