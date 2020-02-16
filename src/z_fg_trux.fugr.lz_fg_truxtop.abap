FUNCTION-POOL Z_FG_TRUX.                 "MESSAGE-ID ..

* INCLUDE LZSK_LOCAL_FGD...                  " Local class definition
type-pools: jbtmk, truxs, cntb, abap.
tables: trux_display.
* Constants
constants:
* Presentation/Applicationserver
      c_application_server type truxs_server value 'APP',
      c_presentation_server type truxs_server value 'PRS',
      c_ole2_server type truxs_server value 'OLE2',
* Fehlerbehandlung
      c_error_header(3)                        value 'XXX',"#EC NOTEXT
      c_error                                  value 'E',
      c_ux                  like sprot_x-ag    value 'UX',
      c_67                  like sprot_x-ag    value '67',
      c_102                 like sprot_u-msgnr value '102',
      c_275                 like sprot_u-msgnr value '275',
      c_276                 like sprot_u-msgnr value '276',
      c_383                 like sprot_u-msgnr value '383',
      c_506                 like sprot_u-msgnr value '506',
      c_801                 like sprot_u-msgnr value '801',
      c_802                 like sprot_u-msgnr value '802',
      c_899                 like sprot_u-msgnr value '899',
*     Rantypen
      c_rantyp_darlehen     value '1',
* Numerische Darlehensnummer
      c_darl_number(12)     type c value '1234567890 ',
* Strukturen und Tabellen
      c_jbiuppa1            like dd02l-tabname value 'JBIUPPA1',
      c_jbiupdab            like dd02l-tabname value 'JBIUPDAB',
      c_jbiupda1            like dd02l-tabname value 'JBIUPDA1',
*      c_jbiuda1             like dd02l-tabname value 'JBIUDA1',
      c_vdarl               like dd02l-tabname value 'VDARL',
*      c_vzgpo               like dd02l-tabname value 'VZGPO',
      c_vzzkoko             like dd02l-tabname value 'VZZKOKO',
*      c_vvzzkopo            like dd02l-tabname value 'VVZZKOPO',
      c_vzzkopo             like dd02l-tabname value 'VZZKOPO',
*      c_vddast              like dd02l-tabname value 'VDDAST',
      c_bp000               like dd02l-tabname value 'BP000',
*      c_bp1000              like dd02l-tabname value 'BP1000',
*      c_bp2000              like dd02l-tabname value 'BP2000',
      c_bp011               like dd02l-tabname value 'BP011',
      c_bpdadr              like dd02l-tabname value 'BPADR',
      c_bpdazah             like dd02l-tabname value 'BPDAZAH',
      c_bpdbank             like dd02l-tabname value 'BPDBANK',
      c_bpdadref            like dd02l-tabname value 'BPADREF',
      c_bpdkto              like dd02l-tabname value 'BPDKTO',
      c_bpdcon1             like dd02l-tabname value 'BPDCON1',
      c_bpdcon2             like dd02l-tabname value 'BPDCON2',
      c_bpdcon3             like dd02l-tabname value 'BPDCON3',
*      c_vzbavf              like dd02l-tabname value 'VZBAVF',
*      c_vzbavv              like dd02l-tabname value 'VZBAVV',
      c_bpdmeld             like dd02l-tabname value 'BPDMELD',
      c_bpdrepb             like dd02l-tabname value 'BPDREPB',
      c_bpdboni             like dd02l-tabname value 'BPDBONI',
      c_bpdmahn             like dd02l-tabname value 'BPDMAHN',
      c_bpdvzin             like dd02l-tabname value 'BPDVZIN',
      c_bpdzahl             like dd02l-tabname value 'BPDZAHL',
      c_bpdtax              like dd02l-tabname value 'BPDTAX',
*     Allgemine Stamm- und Konditionsdateen
      c_st_vdarl  value   'A',
      c_kon_koko  value   'B',
      c_kon_kopo  value   'C',
      c_kon_part  value   'D',
      c_kon_kopa  value   'E',
*      c_kon_meldv value   'V',
*      c_kon_meldf value   'M',
*      c_kon_notiz value   'N',
*      c_kon_vdarldvs value   'V',
*     R/2 Darlehenstamm- und -bewegungsdaten
      c_fisd  value   'F',
*      c_bagb  value   'G',
*      c_fifd  value   'I',          " R/2 Bewegungdaten
*      c_baku  value   'K',
*      c_balv  value   'L',
      c_bako  value   'O',
*      c_op    value   'P',          " R/2 Bewegungdaten
*      c_tbass value   'T',
*      c_r2t037s value 'S',
*      c_vor   value   'V',          " R/2 Bewegungdaten
*      c_bave  value   'V',
*      c_t037a value   'Y',
*      c_t037f value   'Z',
* allgemeine Schnittstelle Partner,
      c_allg_kopf  value 'O',          "GP: Kopfdaten
      c_allg_role  value 'R',          "Rollendaten
      c_allg_adr   value 'S',          "Adressdaten
*      c_allg_pers  value 'P',      "Persönliche Daten
*      c_allg_bd    value 'K',      "Beschäftigungsdaten
      c_allg_bv    value 'V',          "Bankverbindung
      c_allg_zv    value 'X',          "autom. Zahlungsverkehr
      c_allg_neben value '1',          "Kontosteuerung Nebenbuch
      c_allg_zahlu value '2',          "Zahlungssteuerung
      c_allg_allgs value '3',          "allg. Steuerung
*      c_allg_kto   value 'F',      "Kontoführung
*      c_allg_rel   value 'Z',      "Beziehungen
      c_allg_meld  value 'P',          "Meldedaten allgemein     "
      c_allg_repb  value 'Q',          "Meldedaten Buchungskreis "
      c_allg_boni  value 'B',          "Bonitätsdaten            "
      c_allg_mahn  value 'M',          "Mahndaten                "
      c_allg_vzin  value 'I',          "Kontoverzinsung          "
      c_allg_zahl  value 'J',          "Zahldaten                "
      c_allg_tax   value 'T',          "Fiskal. Daten            "
* STYPE Konstanten aus R/2.
*      c_bana  value 'A',
*      c_baan  value 'B',
*      c_basl  value 'H',
*      c_bavv  value 'C',
*      c_babi  value 'E',
      c_bask  value 'F',
* Partnertypen
      c_type_org like bp000-type value '1',
      c_type_nat like bp000-type value '2',
*      c_type_prt like bp000-type value '3',
* Bearbeitungsmodi:
      c_act_crea            like tp105-aktyp   value '01',
      c_act_chng            like tp105-aktyp   value '02',
*      c_act_modify          like tp105-aktyp   value '99',
*     Initialwerte für Char aus EIS Tool
      c_latte value '#',
* Konstanten für Ranges
      c_inclusive    value 'I',
      c_exclusive    value 'E',
      c_option_eq(2) value 'EQ',
      c_option_ne(2) value 'NE',
      c_option_cp(2) value 'CP',
      c_option_np(2) value 'NP',
      c_option_ge(2) value 'GE',
      c_option_lt(2) value 'LT',
      c_option_le(2) value 'LE',
      c_option_gt(2) value 'GT',
      c_option_bt(2) value 'BT',
      c_option_nb(2) value 'NB',
* Feldeingabesteuerung
      c_muss_feld value '+',
*      c_kann_feld value '-',
*      c_ausb_feld value '.',
      c_space                                  value ' ',
* Return codes
      c_rc0       like sy-subrc   value 0,
      c_rc4       like sy-subrc   value 4,
      c_rc8       like sy-subrc   value 8,
      c_rc12      like sy-subrc  value 12,
      c_rc16      like sy-subrc  value 16,
*      c_rc20      like sy-subrc  value 20,
*      c_rc24      like sy-subrc  value 24,
* xml
      c_amount_and_currency(30) type c value 'Currency_and_Amount',
* boolean
      c_true  value 'X',
      c_false value ' '.

* Globale Variable
data:
      dynpro_trux    type sydynnr,
      ok_code        like sy-tcode,
      inp_trux_display   like trux_display,
      rc             like sy-subrc,
      lines_itab     like sy-tabix.

* interne Tabellen

data:
      it_forkey_tab  like dd05p occurs 500 with header line.
data: begin of it_buff_values occurs 1000.
include type truxs_struc_buffered_values.
data: end of it_buff_values.

data: tmp_textname         like thead-tdname,
      begin of textname occurs 100,
        appl(2)      type c,
        enty(4)      type c,
        key(40)      type c,
      end of textname.

data: all_partnr like bp000 occurs 0.

* Überprüfte Domänen
data: it_domaen like  dd07v occurs 1000 with header line.

* Tabelle für Mußfelder bei Übernahmeanlieferung
data: begin of ub_req_fields occurs 20.
        include structure dfies.
data: end of ub_req_fields.

* interne Tabelle mit Feldern verwendeter Structuren
data: begin of i_dfies occurs 5000.
        include structure dfies.
data: end   of i_dfies.

* interne Tabelle mit Dynpro Sourcen
data: begin of i_d020s occurs 2000.
        include structure d020s.
data: end   of i_d020s.
* interne Tabelle mit Dynpro-Felder
data: begin of i_d021s occurs 2000.
        include structure d021s.
data: end   of i_d021s.

* Mußfelder für Partner
data:    begin of requ_fields_a occurs 10.
        include structure tpz3r.
data:    end   of requ_fields_a.

data:    begin of requ_fields_b occurs 10.
        include structure tpz3r.
data:    end   of requ_fields_b.

data:    begin of requ_fields_c occurs 10.
        include structure tpz3r.
data:    end   of requ_fields_c.

data:    begin of requ_fields_d occurs 10.
        include structure tpz3r.
data:    end   of requ_fields_d.

data:    begin of opt_fields_a occurs 10.
        include structure tpz3r.
data:    end   of opt_fields_a.

data:    begin of opt_fields_b occurs 10.
        include structure tpz3r.
data:    end   of opt_fields_b.

data:    begin of opt_fields_c occurs 10.
        include structure tpz3r.
data:    end   of opt_fields_c.

data:    begin of opt_fields_d occurs 10.
        include structure tpz3r.
data:    end   of opt_fields_d.

data: r_customer like bp000 occurs 2500 with header line.

* internal table for local currencies - begin of note 593354
types: begin of s_t001,
         bukrs    type t001-bukrs,               "company code
         waers    type t001-waers,               "currency
         currdec  type tcurx-CURRDEC,            "after decimals
       end of s_t001.
data: it_t001 type standard table of s_t001,
      wa_t001 type s_t001.
* end of note 593354

* tree support when generating BTCI
data: g_current_object type rlfvbtci-objekt.
data: g_current_tcode  type sytcode.
data: g_current_itemno type rlfvbtci-itemno.
data: g_tab_current_btcidata type rlfvbtci occurs 0.
data: g_tab_all_btcidata type rlfvbtci occurs 0.
* Fieldsymbols
field-symbols: <f1>, <f2>.

include ole2incl.
*INCLUDE bcciixml_decl.
*INCLUDE bcciixml_impl.
CLASS cl_ixml DEFINITION LOAD. " note 911334 2005 CS
*include <icon>.

* lokale Klassen
class lcl_btci_tree definition.

  public section.
    methods:
    constructor importing pi_tabname type slis_fieldcat_alv-ref_tabname
                          pi_repid   type syrepid
                          pi_caption type c optional,
    show_grid importing pi_structure_name type slis_tabname
                        pi_rlfvbtci like g_tab_current_btcidata
                        pi_only_when_visible type boolean optional
              exceptions display_failed,
    hide_grid,
    set_caption importing pi_caption type c.

  private section.
    methods:
    show_container,
    hide_container,
    create_hierarchy importing pi_rlfvbtci like g_tab_current_btcidata
                     exporting pe_tabix type sytabix,
    fieldcat_build importing pi_tabname type
                        slis_fieldcat_alv-ref_tabname,

    sort_build,

    set_registered_events,

    handle_close_request for event close of cl_gui_dialogbox_container,

    handle_button_click for event function_selected of cl_gui_toolbar
            importing fcode,

    handle_node_ctmenu_request
      for event node_context_menu_request of cl_gui_alv_tree
        importing node_key menu,

    handle_item_ctmenu_request
      for event item_context_menu_request of cl_gui_alv_tree
        importing node_key fieldname menu,

    handle_double_click_on_node
            for event node_double_click of cl_gui_alv_tree
            importing node_key,

    handle_double_click_on_item
            for event item_double_click of cl_gui_alv_tree
            importing node_key fieldname,

    handle_selection_click_on_node
            for event node_context_menu_selected of cl_gui_alv_tree
            importing node_key fcode,

    handle_selection_click_on_item
            for event item_context_menu_selected of cl_gui_alv_tree
            importing node_key fieldname fcode,

    call_viewing_transaction importing pi_node_key type lvc_nkey.

    constants:
      a_ucomm          type syucomm value 'FVVD_SELECT',
      a_con_width      type i value 525.

    data:
      a_oref_container type ref to cl_gui_dialogbox_container,
      a_oref_alv_grid  type ref to cl_gui_alv_tree,
      a_oref_toolbar   type ref to cl_gui_toolbar,
      a_caption(132)   type c,
      a_layout         type lvc_s_layo,
      a_fieldcat       type lvc_t_fcat,
      a_sort           type lvc_t_sort,"Sortiertabelle
      a_ui_functions   type ui_functions,
      a_repid          type syrepid,
      a_tabname        type slis_fieldcat_alv-ref_tabname,
      a_tab_rlfvbtci   type table of rlfvbtci initial size 0,
      a_size_of_tab    type sytabix,
      a_is_invisible   type boolean.
endclass.

data: g_oref_btci_tree type ref to lcl_btci_tree.
data g_iref_document type ref to i_oi_document_proxy.
data g_oref_container type ref to cl_gui_custom_container.
data g_iref_control type ref to i_oi_container_control.
data g_iref_error type ref to i_oi_error.
data g_iref_spreadsheet type ref to i_oi_spreadsheet.

* N1409243
data:
  gt_mig_buffer     TYPE STANDARD TABLE OF cml_mig_buffer,
  g_flg_buffer_chk  TYPE xfeld.              "N1411334
