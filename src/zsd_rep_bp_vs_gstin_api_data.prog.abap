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
selection-screen end of block sel.
*--------------------------------------------------------------------*
* pre-selection screen events
*--------------------------------------------------------------------*
load-of-program.
  " placeholder

initialization.
  ptyp = 'Partner Type'.
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
          upper( d~bezei ) as state
          from lfa1 as a
          inner join lfb1 as b
          on a~lifnr = b~lifnr
          inner join lfm1 as c
          on a~lifnr = c~lifnr
          inner join t005u as d
          on a~regio = d~bland
          where a~lifnr in @s_vend[]
          and   a~ktokk in @s_venacc[]
          and   a~stcd3 in @s_vengst
          and   b~bukrs in @s_vencmp[]
          and   c~ekorg in @s_porg[]
          and   d~spras = @sy-langu
          and   d~land1 = 'IN'
          into corresponding fields of table @lt_data.
      when others.
    endcase.

    if lt_data is not initial.
      loop at lt_data assigning field-symbol(<ls_data>).
        try.
            select single @abap_true
              from z6mma_params
              where progname = 'GSTNO_VAL_MASTER' " IHDK904637
              and   active_flag = @abap_true
              into @data(lv_validation_active).

            if lv_validation_active = abap_true.
              if <ls_data>-cus_ven is initial.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '000'
                  with 'Business partner is a mandarory input'. " invalid input combination
              endif.

              data(lv_customer) = conv kunnr( |{  iv_entity alpha = in }| ).
              data(lv_vendor) = conv lifnr( |{ iv_entity alpha = in }| ).

              if iv_entity is not initial.
                select single @abap_true
                  from kna1
                  into @data(lv_is_customer)
                  where kunnr = @lv_customer.

                if lv_is_customer ne abap_true.
                  clear lv_customer.
                endif.

                select single @abap_true
                   from lfa1
                   into @data(lv_is_vendor)
                   where lifnr = @lv_vendor.

                if lv_is_vendor ne abap_true.
                  clear lv_vendor.
                endif.

                if iv_entity is not initial and lv_customer is initial and lv_vendor is initial.
                  raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '002'. " invalid entity
                endif.

                if lv_customer is not initial.
                  " IHDK904637
                  select single @abap_true
                    from z6mma_params
                    where progname = 'GSTNO_VAL_CUST'
                    and   active_flag = @abap_true
                    into @data(lv_cust_active).

                  if lv_cust_active = abap_true.
                    select single @abap_true
                      from kna1
                      into @data(lv_is_registered)
                      where kunnr = @lv_customer
                      and   land1 = 'IN'
                      and   regio <> 'Z1'.  " Exlcude SEZ vendors, if any; IHDK904605

                    if lv_is_registered eq abap_false.
                      rv_valid = abap_true.
                      return.
                    endif.

                    select single anred, name1, pstlz, regio, stcd3
                      from kna1
                      into @data(ls_data)
                      where kunnr = @lv_customer.
                  else.
                    rv_valid = abap_true.
                    return.
                  endif.
                endif.

                if lv_vendor is not initial.
                  " IHDK904637
                  select single @abap_true
                    from z6mma_params
                    where progname = 'GSTNO_VAL_VEND'
                    and   active_flag = @abap_true
                    into @data(lv_vend_active).

                  if lv_vend_active = abap_true.
                    select single @abap_true
                      from lfa1
                      into @lv_is_registered
                      where lifnr = @lv_vendor
                      and   ktokk <> 'EMPL'     " IHDK904831
                      and   ven_class = ''
                      and   land1 = 'IN'. " registered

                    if lv_is_registered eq abap_false.
                      rv_valid = abap_true.
                      return.
                    endif.

                    select single anred, name1, pstlz, regio, stcd3
                      from lfa1
                      into @ls_data
                      where lifnr = @lv_vendor.
                  else.
                    rv_valid = abap_true.
                    return.
                  endif.
                endif.
              endif.

              if iv_title is supplied.
                ls_data-anred = iv_title.
              endif.

              if iv_name is supplied.
                ls_data-name1 = iv_name.
              endif.

              if iv_pincode is supplied.
                ls_data-pstlz = iv_pincode.
              endif.

              if iv_state is supplied.
                ls_data-regio = iv_state.
              endif.

              if iv_gst_number is supplied.
                ls_data-stcd3 = iv_gst_number.
              endif.

              if ls_data-anred is initial.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '023'. " Title not supplied/not maintained
              endif.

              if ls_data-name1 is initial.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '024'. " Name not supplied/not maintained
              endif.

              if ls_data-pstlz is initial.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '025'. " Pincode not supplied/not maintained
              else.
                if condense( ls_data-pstlz ) cn '0123456789' or strlen( ls_data-pstlz ) ne 6.
                  raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '028'. " postal code contains non-numeric characters
                endif.
              endif.

              if ls_data-regio is initial.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '003'. " state not supplied/maintained
              endif.

              if ls_data-stcd3 is initial.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '009'. " GST number not supplied/maintained
              endif.

              if condense( ls_data-stcd3 ) ca space or ls_data-stcd3+0(1) eq space.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '010'. " GST number should not contain spaces
              endif.

              zcl_helper=>condense_data( changing cs_data = ls_data ). " ABAP data to be condensed

              " IHDK907779
              select param1 as sap_state, param2 as gst_state
                from z6mma_params
                into table @data(lt_gst_state)
                where progname eq 'STATE_MAP'
                and   param1 eq @ls_data-regio.
              " End IHDK907779

              if lt_gst_state is initial or sy-subrc <> 0.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '011' with ls_data-regio. " GST state code not found for &
              endif.

              " IHDK907779
              loop at lt_gst_state into data(ls_gst_state).
                condense:
                  ls_gst_state-sap_state,
                  ls_gst_state-gst_state.

                if ls_data-stcd3+0(2) eq ls_gst_state-gst_state.
                  data(lv_state_ok) = abap_true.
                endif.

                clear ls_gst_state.
              endloop.

              if lv_state_ok = abap_true.
                rv_valid = abap_true.
              else.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '012'. " State code does not match first 2 characters of GSTIN
              endif.
              " End IHDK907779

              " IHDK909105
              get_gstin_info_with_gstin(
                exporting
                  iv_gstin      = ls_data-stcd3             " Tax Number 3
                importing
                  ev_active     = data(lv_gstin_active)     " Is GSTIN Active?
                  ev_status     = data(lv_gstin_status)     " Status Text
                receiving
                  rs_gstin_info = data(ls_gstin_info) ).

              if lv_gstin_active = abap_false.
                raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '000'
                  with
                      |GSTIN Check: Status: { lv_gstin_status }|.
              else. " for active cases
                " title check - to be implemented
                if strlen( ls_data-anred ) <> 4 or ls_data-anred cn '0123456789'.
                  " title description is provided
                  ls_data-anred = to_upper( ls_data-anred ).
                  select single title
                    from tsad3t
                    where upper( title_medi ) = @ls_data-anred
                    into @ls_data-anred.

                  if ls_data-anred is initial or sy-subrc <> 0.
*            raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '023'. " Title not supplied/not maintained
                  endif.
                endif.

                data(lv_gstin_ctb) = conv char100( to_upper( ls_gstin_info-data-ctb ) ).
                if lv_gstin_ctb is initial.
*          raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '032'. " Constitution Of Business not available with GSTIN
                endif.

                select single @abap_true
                  from zxx_t_ctb_title
                  where upper( gstin_ctb ) = @lv_gstin_ctb
                  and   sap_title = @ls_data-anred
                  into @data(lv_valid_title).

                if lv_valid_title = abap_false.
*          raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '029'. " Title does not match GSTIN "Constitution Of Business"
                endif.

                " name check
                if to_upper( ls_data-name1 ) <> to_upper( ls_gstin_info-data-lgnm ).
*          raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '030'.
                endif.

                " pincode check
                if not line_exists( ls_gstin_info-data-adadr[ addr-pncd = ls_data-pstlz ] ).
                  raise exception type zcx_generic message id 'Z_BUPA' type 'E' number '031'.
                endif.
              endif.
              " End IHDK909105
            else.
              <ls_data>-message = 'Validation is not active'.
            endif.
          catch zcx_generic into data(lox_generic).
            <ls_data>-message = lox_generic->get_text( ).
        endtry.
      endloop.
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
