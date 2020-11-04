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
      mv_customer  type kna1-kunnr,
      mv_sales_org type knvv-vkorg,
      mv_dist_chnl type knvv-vtweg,
      mv_division  type knvv-spart,
      mv_cust_acc_grp   type kna1-ktokd,
      mv_cust_gstin     type kna1-stcd3,

      mv_vendor    type lfa1-lifnr,
      mv_comp_code type lfb1-bukrs,
      mv_purch_grp type lfm1-ekorg,
      mv_vend_acc_grp type lfa1-ktokk,
      mv_vend_gstin type lfa1-stcd3.

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
* local class implementation
*--------------------------------------------------------------------*
class lcl_app implementation.
  method process.
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
  s_cust   for lcl_app=>mv_customer  modif id cus,
  s_sorg   for lcl_app=>mv_sales_org modif id cus,
  s_dchnl  for lcl_app=>mv_dist_chnl modif id cus,
  s_div    for lcl_app=>mv_division  modif id cus,
  s_accgrp for lcl_app=>mv_acc_grp   modif id cus,
  s_gstin  for lcl_app=>mv_gstin     modif id cus.
selection-screen end of block cus.

selection-screen begin of block ven with frame title text-ven.
select-options:
  s_vend
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
  " placeholder

at selection-screen.
  " placeholder
*--------------------------------------------------------------------*
* start-of-selection
*--------------------------------------------------------------------*
start-of-selection.
  lcl_main=>start( ).
*--------------------------------------------------------------------*
* end-of-selection
*--------------------------------------------------------------------*
end-of-selection.
