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
      mv_vend_acc_grp  type lfa1-ktokk,
      mv_vend_gstin    type lfa1-stcd3.

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
  s_cust   for lcl_app=>mv_customer     modif id cus,
  s_sorg   for lcl_app=>mv_sales_org    modif id cus,
  s_dchnl  for lcl_app=>mv_dist_chnl    modif id cus,
  s_div    for lcl_app=>mv_division     modif id cus,
  s_cusacc for lcl_app=>mv_cus_acc_grp modif id cus,
  s_cusgst for lcl_app=>mv_cus_gstin   modif id cus.
selection-screen end of block cus.

selection-screen begin of block ven with frame title text-ven.
select-options:
  s_vend    for lcl_app=>mv_vendor       modif id ven,
  s_comp    for lcl_app=>mv_comp_code    modif id ven,
  s_porg    for lcl_app=>mv_purch_grp    modif id ven,
  s_venacc  for lcl_app=>mv_ven_acc_grp modif id ven,
  s_vengst  for lcl_app=>mv_ven_gstin   modif id ven.
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
        cus_ven              type c length 10,
        acc_grp              type c length 4,
        gstin                type stcd3,
        gstin_status         type string,
        title                type anred,
        api_ctb              type string,
        title_check_status   type string,
        name                 type name1_gp,
        api_legal_name       type string,
        name_check_status    type string,
        pincode              type pstlz,
        pincode_check_status type string,
        state                type bezei,
        api_state            type string,
        state_check_status   type string,
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
          upper( bezei ) as state
          from kna1 as a
          inner join
      when r_ven.

        when others.
      endcase.
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
