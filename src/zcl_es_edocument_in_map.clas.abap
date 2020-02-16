class ZCL_ES_EDOCUMENT_IN_MAP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EDOC_IN_MAP .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ES_EDOCUMENT_IN_MAP IMPLEMENTATION.


  method IF_EDOC_IN_MAP~CHANGE_API_ATTR_PI_INTEGRATION.
  endmethod.


  method IF_EDOC_IN_MAP~DETERMINE_FI_PROCESS_GSTR1.
  endmethod.


  method IF_EDOC_IN_MAP~DETERMINE_FI_PROCESS_GSTR2.
  endmethod.


  method IF_EDOC_IN_MAP~DETERMINE_MM_PROCESS_GSTR2.
  endmethod.


  method IF_EDOC_IN_MAP~DETERMINE_SD_PROCESS_GSTR1.
  endmethod.


  method IF_EDOC_IN_MAP~FILL_FI_HEADER_GSTR1.
  endmethod.


  method IF_EDOC_IN_MAP~FILL_FI_HEADER_GSTR2.
  endmethod.


  method IF_EDOC_IN_MAP~FILL_FI_ITEM_GSTR1.
*This method will be called for each item of FI invoice
*When IV_FI_PROCESS Equals '0' or '1' or 3 -  B2B, B2C, Export items
*    Fill CS_ITEM
*When IV_FI_PROCESS equals '2' or '2A' or '2B' - Credit note or debit note or Refund Voucher item
*    Fill CS_CDN_ITEM
*When IV_FI_PROCESS equals '4'  - Advance/Downpayment items
*    Fill CS_AT_ITEM
*When IV_FI_PROCESS equal '5'  - Taxpaid or downpayment clearing
*   Fill  CS_TXPD_ITEM
  endmethod.


  method IF_EDOC_IN_MAP~FILL_FI_ITEM_GSTR2.
*This method will be called for FI invoice items.
*When iv_fi_process equals '0' or '1'  - B2B or B2BUR
*    Fill CS_ITEM
*When  iv_fi_process equals '2' or '2A'  - Credit or Debit notes
*    Fill CS_CDN_ITEM
*When  iv_fi_process equals '3' - Import of goods
*    Fill CS_IMPG_ITEM
*When  iv_fi_process equals  '4' - Import of Service
*    Fill CS_IMPS_ITEM
*When iv_fi_process  equals '5'  -  Advance tax
*    Fill CS_AT_ITEM
*When iv_fi_process  equals '6'  - Tax paid
*    Fill CS_TXPD_ITEM
*When iv_fi_process  equals '7'  - Journal Voucher(JV)
*    Fill  CS_JV_ITEM
  endmethod.


  method IF_EDOC_IN_MAP~FILL_MM_HEADER_GSTR2.
  endmethod.


  method IF_EDOC_IN_MAP~FILL_MM_ITEM_GSTR2.
*This method will be called for MM invoice items.
*When iv_mm_process equals '0' or '1'  - B2B or B2BUR
*    Fill CS_ITEM
*When  iv_mm_process equals '2' or '2A'  - Credit or Debit notes
*    Fill CS_CDN_ITEM
*When  iv_mm_process equals '3' - Import of goods
*    Fill CS_IMPG_ITEM
*When  iv_mm_process equals  '4' - Import of Service
*    Fill CS_IMPS_ITEM
*When iv_mm_process  equals '6'  - Tax paid
*    Fill CS_TXPD_ITEM
  endmethod.


  method IF_EDOC_IN_MAP~FILL_SD_HEADER_GSTR1.
  endmethod.


  method IF_EDOC_IN_MAP~FILL_SD_ITEM_GSTR1.
*This method will be called for each item of SD invoice
*When IV_SD_PROCESS Equals '0' or '1' or 3   -  B2B, B2C, Export invoice items
*    Fill CS_ITEM
*When IV_SD_PROCESS equals '2' or '2A' or '2B'  - Credit note or Debit note or Refund Voucher
*    Fill CS_CDN_ITEM
*When IV_SD_PROCESS equal '5'  - Taxpaid or downpayment clearing
*   Fill  CS_TXPD_ITEM
* Below is the sample code for  B2B, B2C, Export invoice items Similarly you need to write the logic
* for remaining scenarios

    DATA: ls_excdefn TYPE j_1iexcdefn,
          lt_excdefn TYPE TABLE OF j_1iexcdefn,
          ls_komv    TYPE komv,
          lv_taxm    TYPE c LENGTH 5,
          lv_tax_class TYPE c,
          lv_lfdnr   TYPE  n.

    SELECT * FROM j_1iexcdefn INTO CORRESPONDING FIELDS OF TABLE lt_excdefn
         WHERE kalsm = is_sd_header-kalsm.

     IF iv_sd_process CA '013'.
        CALL FUNCTION 'J_1IG_GET_HSN_SAC'
           EXPORTING
            im_matnr   = is_sd_item-matnr
            im_werks   = is_sd_item-werks
*           im_asnum   = ls_sd_item-matnr
            im_country = 'IN'
          IMPORTING
            ex_hsn_sac = cs_item-hsnsac.
*     Goods or Service Indicator
          IF cs_item-hsnsac IS NOT INITIAL.
            CALL METHOD cl_edoc_map_in_utilities=>identify_good_or_service
              EXPORTING
                i_hsnsac = cs_item-hsnsac    " HSN or SAC Code
              IMPORTING
                e_type   = cs_item-type.     " Returns 'G' for Goods 'S' for Service
          ENDIF.
*             Item Description
          cs_item-description = is_sd_item-arktx.
          REPLACE ALL OCCURRENCES OF REGEX '[^[:alpha:]1234567890 ]' IN cs_item-description WITH space.
*   Item quantity & Unit Quantity Code
          IF cs_item-type = 'G'.    "Goods
            cs_item-quantity = is_sd_item-fkimg.
            CALL METHOD cl_edoc_map_in_utilities=>get_unit_quant_code
              EXPORTING
                iv_msehi = is_sd_item-meins
              IMPORTING
                ev_uqc   = cs_item-uqc.
          ENDIF.

      LOOP AT is_sd_invoice-conditions_record INTO ls_komv
            WHERE knumv EQ is_sd_header-knumv
            AND kposn EQ is_sd_item-posnr
            AND kstat NE 'X'
            AND kinak EQ space
            AND koaid EQ 'D'.

      READ TABLE lt_excdefn INTO ls_excdefn
                             WITH KEY kalsm = is_sd_header-kalsm
                                      kschl = ls_komv-kschl.
       CASE ls_excdefn-cond_name.
        WHEN 'CGSTAR'.
            cs_item-taxbase = abs( ls_komv-kawrt ).
            cs_item-cgstamt = abs( ls_komv-kwert ).
            cs_item-cgstrate = abs( ls_komv-kbetr / 10 ).
        WHEN 'SGSTAR'.
            cs_item-taxbase = abs( ls_komv-kawrt ).
            cs_item-sgstamt = abs( ls_komv-kwert ).
            cs_item-sgstrate = abs( ls_komv-kbetr / 10 ).
        WHEN 'IGSTAR'.
            cs_item-taxbase = abs( ls_komv-kawrt ).
            cs_item-igstamt = abs( ls_komv-kwert ).
            cs_item-igstrate = abs( ls_komv-kbetr / 10 ).
        WHEN 'UTGSTAR'.
            cs_item-taxbase = abs( ls_komv-kawrt ).
            cs_item-sgstamt = abs( ls_komv-kwert ).
            cs_item-sgstrate = abs( ls_komv-kbetr / 10 ).
        WHEN 'CCPSOFFAR'.
            cs_item-taxbase = abs( ls_komv-kawrt ).
            cs_item-cessamt = cs_item-cessamt + ls_komv-kwert.
            cs_item-cessamt = abs( cs_item-cessamt ).
            cs_item-cessrate = abs( ls_komv-kbetr / 10 ).
        WHEN 'CCQSOFFAR'.
            cs_item-taxbase = abs( ls_komv-kawrt ).
            cs_item-cessamt = cs_item-cessamt + ls_komv-kwert.
            cs_item-cessamt = abs( cs_item-cessamt ).
       ENDCASE.
      CLEAR: ls_komv, ls_excdefn.

     ENDLOOP.

*  *  NR - Nil Rated, EX - Exempted, NG - Non GST
         SELECT SINGLE lfdnr INTO lv_lfdnr FROM tstl WHERE talnd = 'IN'
                                                 AND tatyp = 'JOIG'.
          IF sy-subrc EQ 0.
            CONCATENATE 'TAXM' lv_lfdnr INTO lv_taxm.
            SELECT SINGLE (lv_taxm) FROM mlan INTO lv_tax_class
                                         WHERE matnr = is_sd_item-matnr
                                           AND aland = 'IN'.
              IF sy-subrc EQ 0.
                CASE lv_tax_class.
                  WHEN '1'.
                    cs_item-niltype = 'EX'.
                  WHEN '2'.
                    cs_item-niltype = 'NG'.
                  WHEN '0'.
                   IF cs_item-igstrate IS INITIAL
                   AND cs_item-cgstrate IS INITIAL
                   AND cs_item-sgstrate IS INITIAL.
                     cs_item-niltype = 'NR'.
                   ENDIF.
                ENDCASE.

              ENDIF.
          ENDIF.


   ENDIF.
  endmethod.
ENDCLASS.
