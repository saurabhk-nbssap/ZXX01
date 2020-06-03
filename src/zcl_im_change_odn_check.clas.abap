class ZCL_IM_CHANGE_ODN_CHECK definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CHANGE_ODN_CHECK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_CHANGE_ODN_CHECK IMPLEMENTATION.


  method if_ex_change_odn_check~change_odn_checks.
*--------------------------------------------------------------------*
    " called in FM: J_1IG_CHECK_BEFORE_ODN
*--------------------------------------------------------------------*
    " Wednesday, June 03, 2020 16:13:38
    " 6010859: SaurabhK
    " IHDK906795 - XX: S_K: Impl J_1IG_CHANGE_ODN_CHECK BADI: 3.6.20
    " Force generate ODN in case of registered vendor with RCM tax code
    " in ZN and ZO doc types
    " Refer email: Subj: RCM Self Invoicing Format | From: Ravi Pathak [rkpathak@indofil.com] | Sent: Fri 29/05/2020 16:13
*--------------------------------------------------------------------*
    if i_bkpf-blart = 'ZN' or i_bkpf-blart = 'ZO'.
      try.
          data(ls_bseg) = i_bseg[ koart = 'K' ].  " vendor item

          select single @abap_true
            from lfa1
            where lifnr = @ls_bseg-lifnr
            and   ven_class = ''
            into @data(lv_registered).

          if lv_registered = abap_true. " registered vendor
            data(ls_bset) = i_bset[ 1 ].

            select single @abap_true
              from t007s as a
              inner join t007a as b
              on  a~kalsm = b~kalsm
              and a~mwskz = b~mwskz
              where a~spras = @sy-langu
              and   a~kalsm = 'ZTXINN'    " GST tax procedure
              and   a~mwskz = @ls_bset-mwskz
              and   a~text1 like '%RCM%'
              and   b~mwart = 'V'   " Input tax code
              into @data(lv_rcm_tax_code).

            if lv_rcm_tax_code = abap_true. " rcm tax code
              c_supply_type = '1'.    " Vendor
              c_odn_flag = abap_true. " tell the system to generate ODN
            endif.
          endif.
        catch cx_sy_itab_line_not_found ##no_handler.
      endtry.
    endif.
  endmethod.
ENDCLASS.
