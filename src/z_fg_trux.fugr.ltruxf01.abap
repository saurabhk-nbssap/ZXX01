*-------------------------------------------------------------------
***INCLUDE LTRUXF01 .
*---------------------------------------------------------------------*
*       FORM GET_REQUESTED_FIELDS                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RFIELDS                                                       *
*  -->  OFIELDS                                                       *
*  -->  VALUE(TYPE)                                                   *
*  -->  VALUE(AKTYP)                                                  *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
form get_requested_fields tables   rfields     structure tpz3r
                                   ofields     structure tpz3r
                          using    value(type)  like bp000-type
                                   value(aktyp) like tp105-aktyp
                          changing value(rc)    like sy-subrc.
  call function 'BPAR_M_FIELDMOD_REQU_FIELDS'
    exporting
      aktyp       = aktyp
      i_type      = type
    tables
      requ_fields = rfields
      opt_fields  = ofields
    exceptions
      others      = c_rc4.
  rc = sy-subrc.
  sort rfields by tabnm.
  sort ofields by tabnm.
endform.                               " GET_REQUESTED_FIELDS

*---------------------------------------------------------------------*
*       FORM MUSSFELD_INFO_HOLEN                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MF                                                            *
*  -->  REQU_FIELDS                                                   *
*  -->  VALUE(STRUCTURE)                                              *
*---------------------------------------------------------------------*
form mussfeld_info_holen tables mf               structure dfies
                                requ_fields      structure tpz3r
                         using  value(modus)     like tp105-aktyp
                                value(structure) like dd02l-tabname.

  data:    begin of requ_fields_temp occurs 10.
          include structure tpz3r.
  data:    end   of requ_fields_temp.

  mf-tabname = structure.
  case structure.
    when c_jbiupda1.
*     Folgende Felder müssen für alle Sätze angeliefert werden:
      mf-logflag   = c_space.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SFEMODE'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'STYPE'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'RANL'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DERF'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für Allgemeine Stammdaten
      mf-logflag   = c_st_vdarl.
      mf-fieldname = 'GSART'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'RDARNEHM'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'ROLETYP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SANTWHR'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     note 1819790
*     the field may be set as optional
*     mf-fieldname = 'BZUSAGE'.          " Zusagekapital
*     perform add_to_mf_tab tables mf
*                            using  mf-fieldname mf-logflag.

*     Partnerzuordnung
      mf-logflag   = c_kon_part.
      mf-fieldname = 'RDARNEHM'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'ROLETYP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für R/2 Stammdaten
      mf-logflag   = c_fisd.
      mf-fieldname = 'GSART'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'RDARNEHM'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     mf-fieldname = 'ROLE'.
*     perform add_to_mf_tab tables mf
*                           using  mf-fieldname mf-logflag.
      mf-fieldname = 'ROLETYP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SANTWHR'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für Kopfdaten allgemein
      mf-fieldname = 'DGUEL_KK'.
      mf-logflag   = c_kon_koko.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.

*     note 1819790
*     the field may be set as optional
*     mf-fieldname = 'BZUSAGE'.
*     perform add_to_mf_tab tables mf
*                          using  mf-fieldname mf-logflag.

*     Relevant für Konditonspositionsdaten
      mf-fieldname = 'DGUEL_KK'.
      mf-logflag   = c_kon_kopo.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für R/2 Kopfdaten
      mf-fieldname = 'DGUEL_KK'.
      mf-logflag   = c_bako.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für Positionsdaten allgemein
      mf-logflag   = c_kon_kopo.
      mf-fieldname = 'DFAELL'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DGUEL_KP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DVALUT'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SKOART'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für R/2 Positionsdaten
      mf-logflag   = c_bako.
      mf-fieldname = 'DFAELL'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DGUEL_KP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DVALUT'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SKOART'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Relevant für Formeln zu Positionsdaten allgemein
      mf-logflag   = c_kon_kopa.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DGUEL_KK'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DGUEL_KP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SKOART'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SFORMREF'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SVARNAME'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*   Bewegungsdaten
    when c_jbiupdab.
*     Folgende Felder müssen für alle Sätze angeliefert werden:
      mf-logflag   = c_space.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SDBMODE'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'RANL'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'DFAELL'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Feld 'BBWHR' wird an anderer Stelle geprüft
*     darf hier wg. Devisenbewertungen nicht geprüft werden
*      mf-fieldname = 'BBWHR'.
*      PERFORM add_to_mf_tab TABLES mf
*                            USING  mf-fieldname mf-logflag.
*      mf-fieldname = 'BBASIS'.
*      perform add_to_mf_tab tables mf
*                            using  mf-fieldname mf-logflag.
*      mf-fieldname = 'DBERBIS'.
*      perform add_to_mf_tab tables mf
*                            using  mf-fieldname mf-logflag.
*   Geschäftspartner relevante Felder
    when c_jbiuppa1.
*     Für Übernahme erforderliche Daten
      mf-logflag   = c_space.
      mf-fieldname = 'PARTNR'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'SFEMODE'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'STYPE'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Allgemeine Partnerdaten (BP000)
      mf-logflag   = c_allg_kopf.
*     mf-logflag   = c_baan.
      if modus <> c_act_chng.
        mf-fieldname = 'GROUP_ID'.
        perform add_to_mf_tab tables mf
                              using  mf-fieldname mf-logflag.
        mf-fieldname = 'ADR_REF_K'.
        perform add_to_mf_tab tables mf
                              using  mf-fieldname mf-logflag.
      endif.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bp000.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bp011.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdadref.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdadr.
*     Rollendatem
      mf-logflag   = c_allg_role.
      mf-fieldname = 'ROLETYP'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'ROLE'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Adreßdaten
      mf-logflag   = c_allg_adr.
      mf-fieldname = 'ADR_REF_K'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'ORT01'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'LAND1'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Allgemeine Bankverbindung (bpdkto)
      mf-logflag   = c_bask.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'AKONT'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdkto.
*     Kontosteuerung (Nebenbuch)
      mf-logflag   = c_allg_neben.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdcon1.
*     Zahlungssteuerung
      mf-logflag   = c_allg_zahlu.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdcon2.
*     allg. Steuerung
      mf-logflag   = c_allg_allgs.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdcon3.
*     Bankverbindungen  (bpdbank)
      mf-logflag   = c_allg_bv.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdbank.
      mf-fieldname = 'PARTNR'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'BANKL'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'BANKN'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
      mf-fieldname = 'BANKS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Allgemeine autom. Zahlungsverkehr (bpdazah)
      mf-logflag   = c_allg_zv.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdazah.
*     Bonitätsdaten
      mf-logflag   = c_allg_boni.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdboni.
*     Allgemeine Meldedaten
      mf-logflag   = c_allg_meld.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdmeld.
*     Buchungkreis Meldedaten
      mf-logflag   = c_allg_repb.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdrepb.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Mahndaten
      mf-logflag   = c_allg_mahn.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdmahn.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Kontoverzinsung
      mf-logflag   = c_allg_vzin.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdvzin.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Zahldaten
      mf-logflag   = c_allg_zahl.
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                            mf
                                     using  c_bpdzahl.
      mf-fieldname = 'BUKRS'.
      perform add_to_mf_tab tables mf
                            using  mf-fieldname mf-logflag.
*     Fiskal. Daten
      mf-logflag   = c_allg_tax .
      requ_fields_temp[] = requ_fields[].
      perform fill_requ_field_table tables requ_fields_temp
                                           mf
                                    using  c_bpdtax.
    when others.
  endcase.
  sort mf by tabname fieldname.
endform.                               " MUSSFELD_INFO_HOLEN

*---------------------------------------------------------------------*
*       FORM ADD_TO_MF_TAB                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MF                                                            *
*  -->  FIELDNAME                                                     *
*  -->  LOGFLAG                                                       *
*---------------------------------------------------------------------*
form add_to_mf_tab tables mf        structure dfies
                   using  fieldname like dfies-fieldname
                          logflag   like dfies-logflag.

  mf-fieldname = fieldname.
  mf-logflag   = logflag.
  append mf.
endform.                               " ADD_TO_MF_TAB

*---------------------------------------------------------------------*
*       FORM FILL_REQU_FIELD_TABLE                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  REQU_FIELDS_TEMP                                              *
*  -->  MF                                                            *
*  -->  VALUE(STRUCTURE)                                              *
*---------------------------------------------------------------------*
form fill_requ_field_table tables requ_fields_temp structure tpz3r
                                  mf               structure dfies
                           using  value(structure) like dd02l-tabname.

  call function 'ISB_TR_CHECK_REQUIRED_FIELDS'
    exporting
      i_structure    = structure
    tables
      it_requ_fields = requ_fields_temp
    exceptions
      others         = c_rc4.
  loop at requ_fields_temp.
    mf-fieldname = requ_fields_temp-fldnm.
    perform add_to_mf_tab tables mf
                          using  mf-fieldname mf-logflag.
  endloop.
endform.                               " FILL_REQU_FIELD_TABLE

*---------------------------------------------------------------------*
*       FORM DDIC_INFO_HOLEN                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DFIES                                                         *
*  -->  VALUE(STRUCTURE)                                              *
*  -->  VALUE(FMODULE)                                                *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
form ddic_info_holen tables dfies structure dfies
                     using    value(structure)  like dcobjdef-name
                     changing value(fmodule)    like tfdir-funcname
                              value(rc)         like sy-subrc.
  data:
        tmp_tabname like dcobjdef-name.

  data: begin of tmp_dfies occurs 100.
          include structure dfies.
  data: end   of tmp_dfies.

  tmp_tabname = structure.
  if tmp_tabname(1) = '*'.
    structure = structure+1.
  endif.

  fmodule = 'DDIF_FIELDINFO_GET'.
* fmodule = 'DDIF_NAMETAB_GET'   tut nicht DOMNAME kommt nicht zurück
  call function 'DDIF_FIELDINFO_GET'
    exporting
      tabname   = structure
    tables
      dfies_tab = tmp_dfies
    exceptions
      others    = c_rc4.
  rc = sy-subrc.
  check rc = c_rc0.
  if tmp_tabname(1) = '*'.
    tmp_dfies-tabname = tmp_tabname.
    modify tmp_dfies from tmp_dfies transporting tabname
                                    where tabname = structure.
  endif.
* 170502alf analog Hinweis 447169 Beginn
* offset must be specified in characters but dfies returns bytes
* causes problems in UNICODE environment
  data: h_offset like tmp_dfies-leng.


  h_offset = 0.
  sort tmp_dfies by position.
  loop at tmp_dfies.
    tmp_dfies-offset = h_offset.
    add tmp_dfies-leng to h_offset.
    modify tmp_dfies.
  endloop.

* 170502alf analog Hinweis 447169 Ende

  append lines of tmp_dfies to dfies.
endform.                               " DDIC_INFO_HOLEN

*---------------------------------------------------------------------*
*       FORM FIND_DOMAEN_VALUE                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(DOMAEN)                                                 *
*  -->  VALUE(DOMVAL)                                                 *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
form find_domaen_value using    value(domaen) like dd07v-domname
                                value(domval) type any
                       changing value(rc)     like sy-subrc.
  clear rc.

  call function 'LOAN_DOMAEN_MANAGER'
    exporting
      i_domname    = domaen
      i_domvalue_l = domval
    exceptions
      others       = c_rc4.
  rc = sy-subrc.

endform.                    "find_domaen_value

**&---------------------------------------------------------------------
*&      Form  BUILD_ERROR_HEADER
*&---------------------------------------------------------------------*
form build_error_header   tables   error_itab      structure sprot_x
                          using    value(severity) like sprot_x-severity
                                   value(var1)
                                   value(var2)
                          changing value(rc)       like sy-subrc.
  clear error_itab.
  error_itab-msgnr    = c_error_header.
  error_itab-var1     = var1.
  condense error_itab-var1 no-gaps.
  error_itab-var2     = var2.
  condense error_itab-var2 no-gaps.
  error_itab-severity = severity.
  read table error_itab.
  if sy-subrc <> c_rc0.
    append error_itab.
  else.
    perform change_error_header tables   error_itab
                                using var1 var2
                                changing rc.
  endif.
  clear rc.
endform.                               " BUILD_ERROR_HEADER

*&---------------------------------------------------------------------*
*&      Form  CHANGE_ERROR_HEADER
*&---------------------------------------------------------------------*
form change_error_header   tables   error_itab      structure sprot_x
                           using    value(var1)
                                    value(var2)
                           changing value(rc)       like sy-subrc.

  loop at error_itab where ( index is initial or index = space )
                     and     msgnr  = c_error_header
                     and     var1  >= var1.               "#EC PORTABLE
    error_itab-var2 = var2.
    condense error_itab-var2 no-gaps.
    modify error_itab.
  endloop.
  rc = sy-subrc.

endform.                               " CHANGE_ERROR_HEADER

*&---------------------------------------------------------------------*
*&      Form  FILL_ERROR_ITAB
*&---------------------------------------------------------------------*
form fill_error_itab tables   error_itab         structure sprot_x
                    using    value(ag)          like sprot_x-ag
                             value(severity)    like sprot_x-severity
                             value(msgnr)       like sprot_x-msgnr
                             value(index)       like sprot_x-index
                             value(var1)       " like sprot_x-var1
                             value(var2)       " like sprot_x-var2
                             value(var3)       " like sprot_x-var3
                             value(var4)       " like sprot_x-var4
                    changing value(rc) like sy-subrc.
  clear rc.
*  verarbeiten des eigentlichen fehlers
  clear error_itab.
  error_itab-ag       = ag.
  error_itab-msgnr    = msgnr.
  error_itab-index    = index.
  error_itab-var1     = var1.
  error_itab-var2     = var2.
  error_itab-var3     = var3.
  error_itab-var4     = var4.
  condense error_itab-var1 no-gaps.
  condense error_itab-var2 no-gaps.
  condense error_itab-var3 no-gaps.
  condense error_itab-var4 no-gaps.
  error_itab-severity = severity.
  append error_itab.
endform.                               " FILL_ERROR_ITAB

*&---------------------------------------------------------------------*
*&      Form  CLEAN_UP_ERROR_TABLE
*&---------------------------------------------------------------------*
form clean_up_error_table tables   error_itab      structure sprot_x
                          using    value(p_complete) like c_true
                          changing value(rc).
  data: begin of error_tab_copy occurs 10.
          include structure sprot_x.
  data: end   of error_tab_copy.
  data:
    min_rel_index like error_itab-index,
    max_rel_index like error_itab-index.

  clear rc.

  error_tab_copy[] = error_itab[].
  loop at error_itab where index is initial
                     and   msgnr = c_error_header.
    clear: min_rel_index, max_rel_index.
    loop at error_tab_copy where msgnr <> c_error_header
                           and   index >= error_itab-var1 "#EC PORTABLE
                           and   index <= error_itab-var2."#EC PORTABLE
      max_rel_index = error_tab_copy-index.
      check min_rel_index is initial.
      min_rel_index = error_tab_copy-index.
    endloop.
    if sy-subrc <> c_rc0.
      delete error_itab.
    else.
      check p_complete = c_true.
      error_itab-var3 = error_itab-var1.
      error_itab-var4 = error_itab-var2.
      error_itab-var1 = min_rel_index.
      error_itab-var2 = error_itab-var4 - error_itab-var3 + 1.
      if error_itab-var2 < min_rel_index or               "#EC PORTABLE
         error_itab-var2 > max_rel_index.                 "#EC PORTABLE
        error_itab-var2 = error_itab-var4.
      endif.
      condense error_itab-var1 no-gaps.
      condense error_itab-var2 no-gaps.
      modify error_itab.
    endif.
  endloop.

  check p_complete = c_true.
* org_index füllen
  refresh error_tab_copy.
  error_tab_copy[] = error_itab[].
  loop at error_tab_copy where msgnr = c_error_header.
    loop at error_itab where msgnr <> c_error_header
                       and   index >= error_tab_copy-var3 "#EC PORTABLE
                       and   index <= error_tab_copy-var4."#EC PORTABLE
      error_itab-org_index = error_tab_copy-var3 + error_itab-index - 1.
      if error_itab-org_index > error_tab_copy-var4.      "#EC PORTABLE
        error_itab-org_index = error_itab-index.
      endif.
      modify error_itab.
    endloop.
  endloop.

endform.                               " CLEAN_UP_ERROR_TABLE

*&---------------------------------------------------------------------*
*&      Form  PERPARE_NUMBER
*&---------------------------------------------------------------------*
form perpare_number changing value(p_parameter) type any
                             value(pc_decimal_pos) type i.
  data l_strlen type i.
  data l_last_decimal type i.
  data l_hlpvz type i.

  l_strlen = strlen( p_parameter ).
  clear: sy-subrc, pc_decimal_pos.
  while sy-subrc = c_rc0.
    if p_parameter ca '.'.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace '.' with space into p_parameter.
  endwhile.
  clear sy-subrc.
  while sy-subrc = c_rc0.
    if p_parameter ca ','.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace ',' with space into p_parameter.
  endwhile.
  clear sy-subrc.
  while sy-subrc = c_rc0.
    if p_parameter ca ';'.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace ';' with space into p_parameter.
  endwhile.
  clear sy-subrc.
  while sy-subrc = c_rc0.
    if p_parameter ca '/'.
      if sy-fdpos  > l_last_decimal.
        l_last_decimal = sy-fdpos + 1.
      endif.
    endif.
    replace '/' with space into p_parameter.
  endwhile.
  if not l_last_decimal is initial.
    l_hlpvz = l_strlen - 1.
    if p_parameter+l_hlpvz(1) = '-'.
      pc_decimal_pos = l_strlen - l_last_decimal - 1.
    else.
      pc_decimal_pos = l_strlen - l_last_decimal.
    endif.
  endif.
  condense p_parameter no-gaps.
endform.                               " PERPARE_NUMBER

*---------------------------------------------------------------------*
*       FORM PREPARE_NUMBER                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(CHAR)                                                   *
*  -->  VALUE(NUM)                                                    *
*---------------------------------------------------------------------*
form prepare_number using value(char) type any
                    changing value(num) type any.

  clear sy-subrc.
  while sy-subrc = 0.
    replace '-' with c_space into char.
  endwhile.

  clear sy-subrc.
  while sy-subrc = 0.
    replace '.' with c_space into char.
  endwhile.

  clear sy-subrc.
  while sy-subrc = 0.
    replace ',' with c_space into char.
  endwhile.
  condense char no-gaps.
  num = char.

endform.                              " PREPARE_NUMBER

*&---------------------------------------------------------------------*
*&      Form  SELECTION_TABSTRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0022   text
*      <--P_DYNPRO_TRUX  text
*----------------------------------------------------------------------*
form selection_tabstrip using    value(p_okcode) type sytcode
                        changing value(p_dynpro) type sydynnr.
  constants: c_dynpro_form type sydynnr value '1002',
             c_dynpro_file type sydynnr value '1001'.
  case p_okcode.
    when 'TRUX_DATAIN'.
      p_dynpro = c_dynpro_file.
*    when 'TRUX_DATAFORM'.
*      p_dynpro = c_dynpro_form.
    when others.
      p_dynpro = c_dynpro_file.
  endcase.

endform.                               " SELECTION_TABSTRIP

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_1000
*&---------------------------------------------------------------------*
form user_command_1000  using   value(p_okcode) type sytcode
                       changing value(p_display) structure trux_display
                                value(p_rc)  type sysubrc.
  clear p_rc.
  case ok_code.
    when 'OK'.
      if not (
         p_display-file_name is initial or
         p_display-file_serv is initial or
         p_display-file_format is initial ).
        if p_display-file_format = 'XLS'.
          p_display-file_serv = 'OLE2'.
        endif.
        set screen 0.
        leave screen.
      endif.
    when others.
      p_rc = c_rc4.
      set screen 0.
      leave screen.
  endcase.
endform.                               " USER_COMMAND_1000

*&---------------------------------------------------------------------*
*&      Form  GET_WS_FILENAME
*&---------------------------------------------------------------------*
form get_ws_filename using    value(p_fileserv)
                                          like trux_display-file_serv
                              value(p_ftype)
                                          like trux_display-file_format
                     changing value(p_wsfile)
                                          like trux_display-file_name.
  data:
        tmp_upload_path like rlgrap-filename,
        tmp_fieldln     type i,
        tmp_mask(80),
        tmp_ftype_head  like tmp_mask  value ',*.*,*.',
        tmp_ftype_trail like tmp_mask  value '.',
        tmp_asterisk    like tmp_mask  value ',*.*,*.*.'.

  check p_fileserv <> 'APP'.
  field-symbols: <f_tmp_upload_path>.

  get parameter id 'GR9' field tmp_upload_path.
  if tmp_upload_path is initial.
    set extended check off.
    call function 'PROFILE_GET'
      exporting
        filename = 'FRONT.INI'
        key      = 'Path'
        section  = 'Filetransfer'
      importing
        value    = tmp_upload_path.

    if tmp_upload_path is initial.     "// not found
      call function 'PROFILE_GET'
        exporting
          filename = 'FRONT.INI'
          key      = 'PathUpload'
          section  = 'Filetransfer'
        importing
          value    = tmp_upload_path.
    endif.
    set extended check on.
    if tmp_upload_path is initial.     "// not found
      call function 'WS_QUERY'
        exporting
          query  = 'CD'  "// Get Current Directory
        importing
          return = tmp_upload_path.
    endif.
    set parameter id 'GR9' field tmp_upload_path.
  endif.
  tmp_fieldln = strlen( tmp_upload_path ) - 1.
  assign tmp_upload_path+tmp_fieldln(1) to <f_tmp_upload_path>.
  if <f_tmp_upload_path> = '/' or <f_tmp_upload_path> = '\'.
    clear <f_tmp_upload_path>.
  endif.

  if p_ftype = 'ASC'.
    tmp_mask =  tmp_asterisk.
  else.
    concatenate tmp_ftype_head p_ftype tmp_ftype_trail into tmp_mask.
    condense tmp_mask no-gaps.
  endif.

  call function 'WS_FILENAME_GET'
    exporting
      def_path = tmp_upload_path
      mask     = tmp_mask
      mode     = 'O'
    importing
      filename = p_wsfile
    exceptions
      others   = 4.
endform.                               " GET_WS_FILENAME

*&---------------------------------------------------------------------*
*&      Form  GET_WS_FILE_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_REPID  text
*      -->P_SY_DYNNR  text
*      <--P_TRUX_DISPLAY_FILE_FORMAT  text
*----------------------------------------------------------------------*
form get_ws_file_format using    value(p_repid) type syrepid
                                 value(p_dynnr) type sydynnr
                        changing value(p_ftype) type truxs_fileformat
                                 value(p_serv)  type trux_servertyp.
  data:
    t_dynpread like dynpread occurs 0 with header line,
    l_dyname like  d020s-prog,
    l_dynumb like  d020s-dnum.

  l_dyname = p_repid.
  l_dynumb = p_dynnr.
  t_dynpread-fieldname = 'TRUX_DISPLAY-FILE_FORMAT'.
  append t_dynpread.
  t_dynpread-fieldname = 'TRUX_DISPLAY-FILE_SERV'.
  append t_dynpread.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname     = l_dyname
      dynumb     = l_dynumb
    tables
      dynpfields = t_dynpread
    exceptions
      others     = c_rc4.
  if sy-subrc <> c_rc0.
    p_ftype = 'ASC'.
  else.
    read table t_dynpread
              with key fieldname = 'TRUX_DISPLAY-FILE_FORMAT'.
    if sy-subrc = c_rc0.
      p_ftype = t_dynpread-fieldvalue.
    endif.
    read table t_dynpread
              with key fieldname = 'TRUX_DISPLAY-FILE_SERV'.
    if sy-subrc = c_rc0.
      p_serv = t_dynpread-fieldvalue.
    endif.
  endif.

endform.                               " GET_WS_FILE_FORMAT

*---------------------------------------------------------------------*
*       CLASS lcl_btci_tree IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
class lcl_btci_tree implementation.
  method constructor.
    data: l_iconquick type iconquick.
    data: l_fcode     type ui_func.

    a_tabname = pi_tabname.

    if sy-batch is initial.
      create object a_oref_container
                    exporting width = a_con_width
                              no_autodef_progid_dynnr = 'X'
                              height = 66.

      create object a_oref_alv_grid
             exporting parent = a_oref_container
                       item_selection = 'X'
                       no_html_header = 'X'.
      call method a_oref_alv_grid->get_toolbar_object
        importing
          er_toolbar = a_oref_toolbar.
      l_iconquick = 'Zurück'(bck).                          "#EC NOTEXT
      l_fcode = 'BACK'.                                     "#EC NOTEXT
      call method a_oref_toolbar->add_button
        exporting
          fcode     = l_fcode
          icon      = '@2O@'
          butn_type = cntb_btype_button
          text      = ''
          quickinfo = l_iconquick.
      call method a_oref_toolbar->add_button
        exporting
          fcode     = ''
          icon      = ''
          butn_type = cntb_btype_sep
          text      = ''
          quickinfo = ''.
      call method fieldcat_build
        exporting
          pi_tabname = pi_tabname.
      call method sort_build.
*      append cl_gui_alv_tree=>mc_fc_current_variant to a_ui_functions.
      append cl_gui_alv_tree=>mc_fc_calculate to a_ui_functions.

      if not pi_caption is initial.
        call method set_caption
          exporting
            pi_caption = pi_caption.
      endif.
      a_repid = pi_repid.
      call method hide_container.
      set handler handle_close_request for a_oref_container.
      set handler handle_item_ctmenu_request for a_oref_alv_grid.
      set handler handle_node_ctmenu_request for a_oref_alv_grid.
      set handler handle_double_click_on_item for a_oref_alv_grid.
      set handler handle_double_click_on_node for a_oref_alv_grid.
      set handler handle_selection_click_on_item for a_oref_alv_grid.
      set handler handle_selection_click_on_node for a_oref_alv_grid.
      set handler handle_button_click for a_oref_toolbar.
      call method set_registered_events.
    else.
      a_repid = pi_repid.
    endif.
  endmethod.                    "constructor

  method set_caption.
    a_caption = pi_caption.
    call method a_oref_container->set_caption
      exporting
        caption = pi_caption
      exceptions
        others  = 4.
  endmethod.                    "set_caption

  method show_container.
    if not a_is_invisible is initial.
      call method a_oref_container->set_visible
        exporting
          visible = cl_gui_control=>visible_true
        exceptions
          others  = 4.
    endif.
    clear a_is_invisible.
    call method a_oref_container->set_focus
      exporting
        control = a_oref_container
      exceptions
        others  = 4.

    " We flush the automation queue of the gui framework
    call method cl_gui_cfw=>flush.
  endmethod.                    "show_container

  method show_grid.
    data: l_disvariant type disvariant.
    data: l_heigth type i.
    data: l_stable type lvc_s_stbl.
    data: l_tabix  type sytabix.
    data: l_tab_rlfvbtci like a_tab_rlfvbtci.
    data: l_tab_fieldcat type slis_t_fieldcat_alv.
    data: l_fieldcat type slis_fieldcat_alv.
    data: l_print_alv type slis_print_alv.
    data: l_hierarchy_header type treev_hhdr.

    if sy-batch is initial.
      if a_tab_rlfvbtci is initial.
        if a_layout-grid_title is initial.
          a_layout-grid_title = a_caption.
        endif.

*   Prepare Header
        l_hierarchy_header-heading = 'Hierarchy'(hie).      "#EC NOTEXT
        l_hierarchy_header-tooltip = 'tootip'(hit).         "#EC NOTEXT
        l_hierarchy_header-width = 40.
        l_hierarchy_header-width_pix = ''.

        l_disvariant-report = a_repid.

        call method a_oref_alv_grid->set_table_for_first_display
          exporting
            is_hierarchy_header  = l_hierarchy_header
            i_structure_name     = pi_structure_name
            i_save               = 'A'
            is_variant           = l_disvariant
            it_toolbar_excluding = a_ui_functions
          changing
            it_fieldcatalog      = a_fieldcat[]
            it_outtab            = a_tab_rlfvbtci[]
          exceptions
            others               = 4.
        if sy-subrc <> 0.
          raise display_failed.
        endif.

        call method create_hierarchy
          exporting
            pi_rlfvbtci = pi_rlfvbtci
          importing
            pe_tabix    = a_size_of_tab.

        l_heigth = 58 + ( a_size_of_tab * 8 ).
        call method a_oref_container->set_position
          exporting
            width  = a_con_width
            height = l_heigth.

        if pi_only_when_visible is initial.
          call method show_container.
        endif.
      else.
        call method show_container.
      endif.
    else.
      call function 'REUSE_ALV_FIELDCATALOG_MERGE'
        exporting
          i_structure_name = a_tabname
        changing
          ct_fieldcat      = l_tab_fieldcat
        exceptions
          others           = 4.
      if sy-subrc = 0.
        loop at l_tab_fieldcat into l_fieldcat
                                    where fieldname = 'ITEMNO'.
          l_fieldcat-no_out = 'X'.
          modify l_tab_fieldcat index sy-tabix
                            from l_fieldcat transporting no_out.
        endloop.
        l_print_alv-print = 'X'.
        l_print_alv-no_print_listinfos = 'X'.
        l_print_alv-no_print_selinfos = 'X'.
        call function 'REUSE_ALV_LIST_DISPLAY'
          exporting
            it_fieldcat = l_tab_fieldcat
            is_print    = l_print_alv
          tables
            t_outtab    = pi_rlfvbtci
          exceptions
            others      = 4.
      endif.
    endif.
  endmethod.                    "show_grid

  method hide_grid.
    call method hide_container.
  endmethod.                    "hide_grid

  method create_hierarchy.
    data: l_rlfvbtci like line of a_tab_rlfvbtci.
    data: l_old_rlfvbtci like line of a_tab_rlfvbtci.
    data: l_disp_rlfvbtci like line of a_tab_rlfvbtci.
    data: l_repid_key type lvc_nkey,
          l_tcode_key type lvc_nkey,
          l_objekt_key type lvc_nkey,
          l_restofit_key type lvc_nkey,
          l_tab_node_key type lvc_t_nkey,
          l_node_text type lvc_value,
          l_tstct type tstct,
          l_tab_item_layout type lvc_t_layi,
          l_item_layout type lvc_s_layi.

    clear pe_tabix.
    loop at pi_rlfvbtci into l_rlfvbtci.
      l_disp_rlfvbtci = l_rlfvbtci.
      clear: l_disp_rlfvbtci-fnam, l_disp_rlfvbtci-fval.
*     when ever the TCODE is changing: new node
      at new tcode.
        pe_tabix = pe_tabix + 1.
        clear: l_tab_item_layout, l_item_layout.
        l_item_layout-fieldname =
                             a_oref_alv_grid->c_hierarchy_column_name.
*        l_item_layout-style = cl_gui_column_tree=>style_intensified.
        append l_item_layout to l_tab_item_layout.
        l_tstct-tcode = l_rlfvbtci-tcode.
        call function 'TSTCT_READ'
          exporting
            f_tstct = l_tstct
          importing
            f_tstct = l_tstct.
        l_node_text =  l_tstct-ttext.
        call method a_oref_alv_grid->add_node
          exporting
                i_relat_node_key = space
                i_relationship   = cl_gui_column_tree=>relat_last_child
                i_node_text      = l_node_text
                it_item_layout   = l_tab_item_layout
             importing
                e_new_node_key = l_tcode_key.
        append l_tcode_key to l_tab_node_key.
      endat.
*     when ever the OBJEKT is changing: new node
      at new objekt.
        pe_tabix = pe_tabix + 1.
        clear: l_tab_item_layout, l_item_layout.
        l_item_layout-fieldname =
                             a_oref_alv_grid->c_hierarchy_column_name.
        l_item_layout-style = cl_gui_column_tree=>style_intensified.
        append l_item_layout to l_tab_item_layout.
        l_node_text =  l_rlfvbtci-objekt.
        call method a_oref_alv_grid->add_node
          exporting
                i_relat_node_key = l_tcode_key
                i_relationship   = cl_gui_column_tree=>relat_last_child
                i_node_text      = l_node_text
                it_item_layout   = l_tab_item_layout
                is_outtab_line   = l_disp_rlfvbtci
             importing
                e_new_node_key = l_objekt_key.
        append l_objekt_key to l_tab_node_key.
      endat.
*     when ever the REPID is changing: new node
      at new itemno.                   "repid.
        l_node_text =  l_rlfvbtci-repid.
        l_node_text =  l_disp_rlfvbtci-repid.
        clear: l_tab_item_layout, l_item_layout.
        l_item_layout-fieldname =
                            a_oref_alv_grid->c_hierarchy_column_name.
        append l_item_layout to l_tab_item_layout.
        call method a_oref_alv_grid->add_node
          exporting
                i_relat_node_key = l_objekt_key
                i_relationship   = cl_gui_column_tree=>relat_last_child
                i_node_text      = l_node_text
                it_item_layout   = l_tab_item_layout
                is_outtab_line   = l_disp_rlfvbtci
             importing
                e_new_node_key = l_repid_key.
        append l_repid_key to l_tab_node_key.
      endat.
      l_node_text =  l_rlfvbtci-dynnr.
      call method a_oref_alv_grid->add_node
        exporting
          i_relat_node_key = l_repid_key
          i_relationship   = cl_gui_column_tree=>relat_last_child
          i_node_text      = l_node_text
          is_outtab_line   = l_rlfvbtci
        importing
          e_new_node_key   = l_restofit_key.
      append l_restofit_key to l_tab_node_key.
*     save the current copy
      l_old_rlfvbtci = l_rlfvbtci.
    endloop.
    if sy-subrc = 0.
      call method a_oref_alv_grid->update_calculations.
      call method a_oref_alv_grid->frontend_update.
    endif.
  endmethod.                    "create_hierarchy

  method call_viewing_transaction.
    data: l_rlfvbtci type rlfvbtci.
    data: l_bdcdata     type bdcdata,
          l_tab_bdcdata type table of bdcdata initial size 0,
          l_bukrs type bukrs,
          l_ranl  type ranl.
    data: l_node_text type lvc_value.

    if not pi_node_key is initial.
      call method a_oref_alv_grid->get_outtab_line
        exporting
          i_node_key    = pi_node_key
        importing
          e_outtab_line = l_rlfvbtci
          e_node_text   = l_node_text.
      check not l_rlfvbtci-new_objekt is initial.
      case l_rlfvbtci-tcode.
        when 'FNO1'.
*          set parameter id 'BOB' field l_rlfvbtci-new_objekt.
*          call transaction 'FNO3' and skip first screen.
          l_bdcdata-fnam     = 'BOB'.
          l_bdcdata-fval     = l_rlfvbtci-new_objekt.
          append l_bdcdata to l_tab_bdcdata.
*          call function 'CALL_TRANSACTION_FROM_TABLE'
          call function 'CALL_TRANSACTION_FROM_TABLE_CO' "Hw 1036092
            starting new task l_node_text
            exporting
              i_tcode         = 'FNO3'
            tables
              t_parameter_ids = l_tab_bdcdata
            exceptions
              others          = 2.
        when 'FNO5'.
          clear l_bdcdata.
          l_bdcdata-program  = 'SAPLFVDS'.
          l_bdcdata-dynpro   = '0250'.
          l_bdcdata-dynbegin = 'X'.
          append l_bdcdata to l_tab_bdcdata.
          clear l_bdcdata.
          l_bdcdata-fnam     = 'VDARLSIC-RSICHER'.
          l_bdcdata-fval     = l_rlfvbtci-new_objekt.
          append l_bdcdata to l_tab_bdcdata.
          clear l_bdcdata.
          l_bdcdata-fnam     = 'BDC_OKCODE'.
          l_bdcdata-fval     = '=SRCH'.
          append l_bdcdata to l_tab_bdcdata.
*          call function 'CALL_TRANSACTION_FROM_TABLE'
          call function 'CALL_TRANSACTION_FROM_TABLE_CO' "Hw 1036092
            starting new task l_node_text
            exporting
              i_tcode   = 'FNO7'
              i_mode    = 'E'
            tables
              t_bdcdata = l_tab_bdcdata
            exceptions
              others    = 2.
        when 'FNVM'.
          l_bukrs = l_rlfvbtci-new_objekt(4).
          l_ranl  = l_rlfvbtci-new_objekt+5(13).
*          set parameter id 'BUK' field l_bukrs.
*          set parameter id 'RAD' field l_ranl.
*          call transaction 'FNVS' and skip first screen.
          l_bdcdata-fnam     = 'BUK'.
          l_bdcdata-fval     = l_bukrs.
          append l_bdcdata to l_tab_bdcdata.
          l_bdcdata-fnam     = 'RAD'.
          l_bdcdata-fval     = l_ranl.
          append l_bdcdata to l_tab_bdcdata.
*          call function 'CALL_TRANSACTION_FROM_TABLE'
          call function 'CALL_TRANSACTION_FROM_TABLE_CO' "Hw 1036092
            starting new task l_node_text
            exporting
              i_tcode         = 'FNVS'
            tables
              t_parameter_ids = l_tab_bdcdata
            exceptions
              others          = 2.
        when others.
      endcase.
    endif.
  endmethod.                    "call_viewing_transaction

  method handle_selection_click_on_item.
    check fcode = a_ucomm.
    call method call_viewing_transaction
      exporting
        pi_node_key = node_key.
  endmethod.                    "handle_selection_click_on_item

  method handle_selection_click_on_node.
    check fcode = a_ucomm.
    call method call_viewing_transaction
      exporting
        pi_node_key = node_key.
  endmethod.                    "handle_selection_click_on_node

  method handle_double_click_on_item.
    call method call_viewing_transaction
      exporting
        pi_node_key = node_key.
  endmethod.                    "handle_double_click_on_item

  method handle_double_click_on_node.
    call method call_viewing_transaction
      exporting
        pi_node_key = node_key.
  endmethod.                    "handle_double_click_on_node

  method handle_button_click.
    case fcode.
      when 'BACK'.                                          "#EC NOTEXT
        call method hide_container.
      when others.
        clear fcode.
    endcase.
  endmethod.                    "handle_button_click

  method handle_close_request.
    call method hide_container.
  endmethod.                    "handle_close_request

  method hide_container.
    call method a_oref_container->set_visible
      exporting
        visible = cl_gui_control=>visible_false.

    " We flush the atumation queue of the gui framework
    call method cl_gui_cfw=>flush.
    a_is_invisible = 'X'.
  endmethod.                    "hide_container

  method fieldcat_build.
    data: ls_fieldcatalog type lvc_s_fcat.

    if a_fieldcat is initial.
      call function 'LVC_FIELDCATALOG_MERGE'
        exporting
          i_structure_name = pi_tabname
        changing
          ct_fieldcat      = a_fieldcat[].
      loop at a_fieldcat into ls_fieldcatalog.
        case ls_fieldcatalog-fieldname.
          when 'FNAM'.
            ls_fieldcatalog-outputlen = '30'.
            modify a_fieldcat index sy-tabix
                            from ls_fieldcatalog transporting outputlen.
          when 'FVAL'.
            ls_fieldcatalog-outputlen = '30'.
            ls_fieldcatalog-scrtext_m = 'Feldinhalt'(fih).
            ls_fieldcatalog-scrtext_s = 'Feldinhalt'(fih).
            modify a_fieldcat index sy-tabix
                            from ls_fieldcatalog transporting outputlen
                                                              scrtext_s
                                                              scrtext_m.
          when others.
            clear ls_fieldcatalog-key.
            ls_fieldcatalog-no_out = 'X'.
            ls_fieldcatalog-tech = 'X'.
            modify a_fieldcat index sy-tabix
                              from ls_fieldcatalog transporting no_out
                                                   tech key.
        endcase.
      endloop.
    endif.
  endmethod.                    "fieldcat_build

  method sort_build.
    data ls_sort_wa type lvc_s_sort.

    ls_sort_wa-spos = 1.
    ls_sort_wa-fieldname = 'OBJEKT'.
    append ls_sort_wa to a_sort.

    ls_sort_wa-spos = 2.
    ls_sort_wa-fieldname = 'ITEMNO'.
    ls_sort_wa-no_out = 'X'.
    append ls_sort_wa to a_sort.

  endmethod.                    "sort_build

  method handle_item_ctmenu_request.
    data: l_rlfvbtci type rlfvbtci.
    data: l_node_text type lvc_value.

    if not node_key is initial.
      call method a_oref_alv_grid->get_outtab_line
        exporting
          i_node_key    = node_key
        importing
          e_outtab_line = l_rlfvbtci
          e_node_text   = l_node_text.
      check not l_rlfvbtci-new_objekt is initial.
      call method menu->add_function
        exporting
          fcode = a_ucomm
          text  = text-dis.
    endif.
  endmethod.                    "handle_item_ctmenu_request

  method handle_node_ctmenu_request.
    data: l_rlfvbtci type rlfvbtci.
    data: l_node_text type lvc_value.

    if not node_key is initial.
      call method a_oref_alv_grid->get_outtab_line
        exporting
          i_node_key    = node_key
        importing
          e_outtab_line = l_rlfvbtci
          e_node_text   = l_node_text.
      check not l_rlfvbtci-new_objekt is initial.
      call method menu->add_function
        exporting
          fcode = a_ucomm
          text  = text-dis.
    endif.
  endmethod.                    "handle_node_ctmenu_request

  method set_registered_events.
    data: l_tab_events type cntl_simple_events,
          l_event type cntl_simple_event.

    l_event-eventid = cl_gui_column_tree=>eventid_header_click.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_button_click.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_link_click.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
    append l_event to l_tab_events.
   l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    append l_event to l_tab_events.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    append l_event to l_tab_events.

    call method a_oref_alv_grid->set_registered_events
      exporting
        events = l_tab_events
      exceptions
        others = 4.

  endmethod.                    "set_registered_events
endclass.                    "lcl_btci_tree IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  dfies_to_xml
*&---------------------------------------------------------------------*
form dfies_to_xml tables   pi_tab_dfies structure dfies
                  using value(pi_pdocument) type ref to if_ixml_document
                        value(pi_elem)  type ref to if_ixml_element
                        value(pi_value) type any
                        value(pi_content) type boolean.

  data:
        l_dfies type dfies,
        l_simple_elem type ref to if_ixml_element,
        l_elem  type ref to if_ixml_element,
        result  type i,
        l_name  type string,
        l_value type string,
        l_tabix type sytabix,
        l_numc_2_char(12),
        l_tab_tmp_dfies type table of dfies,
        l_tmp_dfies type dfies.
  field-symbols: <fs_value>.

  l_elem = pi_elem.

*  LOOP AT pi_tab_dfies INTO l_dfies
*                       WHERE datatype = 'CUKY'.
*    l_tabix = sy-tabix.
*    READ TABLE pi_tab_dfies TRANSPORTING NO FIELDS
*                            WITH KEY reffield = l_dfies-fieldname.
*    CHECK sy-subrc = 0.
*    DELETE pi_tab_dfies INDEX l_tabix.
*    APPEND l_dfies TO l_tab_tmp_dfies.
*  ENDLOOP.

  loop at pi_tab_dfies into l_dfies.
*    IF l_dfies-datatype = 'CURR' AND NOT l_dfies-reffield IS INITIAL.
*      ASSIGN COMPONENT l_dfies-reffield OF STRUCTURE pi_value
*                                      TO <fs_value>.
*      IF sy-subrc = c_rc0.
**      generate a special not for currency and amound
*        CLEAR l_value.
*        l_name = c_amount_and_currency.
*        l_simple_elem = pi_pdocument->create_simple_element(
*                                             name = l_name
*                                             parent = l_elem ).
*        l_elem = l_simple_elem.
**      tag the currency first
*        l_name = l_dfies-reffield.
*        READ TABLE l_tab_tmp_dfies INTO l_tmp_dfies
*                                 WITH KEY fieldname = l_dfies-reffield.
*        IF sy-subrc = 0.
*         IF <fs_value> IS INITIAL.
*            CLEAR l_value.
*          ELSE.
*            l_value = <fs_value>.
*          ENDIF.
*          IF <fs_value> IS INITIAL AND NOT pi_content IS INITIAL.
*            CONTINUE.
*          ENDIF.
*          l_simple_elem =
*             pi_pdocument->create_simple_element( name = l_name
*                                                  value = l_value
*                                                  parent = l_elem ).
**         set attributes:
*          l_name = 'Description'.
*          l_value = l_tmp_dfies-fieldtext.
*          result = l_simple_elem->set_attribute( name = l_name
*                                                value = l_value ).
*          l_name = 'Datatype'.
*          l_value = l_tmp_dfies-datatype.
*          result = l_simple_elem->set_attribute( name = l_name
*                                                 value = l_value ).
*          l_name = 'Length'.
*          WRITE l_tmp_dfies-leng TO l_numc_2_char NO-ZERO.
*          CONDENSE l_numc_2_char NO-GAPS.
*          l_value = l_numc_2_char.
*          result = l_simple_elem->set_attribute( name = l_name
*                                                 value = l_value ).
*        ENDIF.
*      ENDIF.
*    ELSE.
*      l_elem = pi_elem.
*    ENDIF.
    l_name = l_dfies-fieldname.
    assign component l_dfies-fieldname of structure pi_value
                                       to <fs_value>.
    if sy-subrc <> 0 or <fs_value> is initial.
      clear l_value.
    else.
      l_value = <fs_value>.
    endif.
    if <fs_value> is initial and not pi_content is initial.
      continue.
    endif.
    l_simple_elem =
       pi_pdocument->create_simple_element( name = l_name
                                            value = l_value
                                            parent = l_elem ).
*     set attributes:
    l_name = 'Description'.                                 "#EC NOTEXT
    l_value = l_dfies-fieldtext.
    result = l_simple_elem->set_attribute( name = l_name
                                          value = l_value ).
    l_name = 'Datatype'.                                    "#EC NOTEXT
    l_value = l_dfies-datatype.
    result = l_simple_elem->set_attribute( name = l_name
                                           value = l_value ).
    l_name = 'Length'.                                      "#EC NOTEXT
    write l_dfies-leng to l_numc_2_char no-zero.
    condense l_numc_2_char no-gaps.
    l_value = l_numc_2_char.
    result = l_simple_elem->set_attribute( name = l_name
                                           value = l_value ).
    if not l_dfies-decimals is initial.
      l_name = 'Decimals'.                                  "#EC NOTEXT
      write l_dfies-decimals to l_numc_2_char no-zero.
      condense l_numc_2_char no-gaps.
      l_value = l_numc_2_char.
      result = l_simple_elem->set_attribute( name = l_name
                                             value = l_value ).
    endif.
  endloop.

endform.                               " dfies_to_xml

*---------------------------------------------------------------------*
*       FORM xml_header                                               *
*---------------------------------------------------------------------*
form xml_header using value(pi_pdocument) type ref to if_ixml_document
                      value(pi_xml_root) type string
                changing value(pc_elem)  type ref to if_ixml_element.

  data:
        l_simple_elem type ref to if_ixml_element,
        l_elem type ref to if_ixml_element,
        l_result  type i,
        l_name  type string.

  l_name = pi_xml_root .
  l_elem = pi_pdocument->create_element( name = l_name ).
  l_result = pi_pdocument->append_child( l_elem ).
  pc_elem = l_elem.
endform.                               "xml_header

*---------------------------------------------------------------------*
*       FORM xml_node                                                 *
*---------------------------------------------------------------------*
form xml_node  using value(pi_pdocument) type ref to if_ixml_document
                      value(pi_xml_node) type string
                changing value(pc_elem)  type ref to if_ixml_element.

  data:
        l_elem type ref to if_ixml_element.

  l_elem = pi_pdocument->create_simple_element(
                                    name = pi_xml_node
                                    parent = pc_elem ).
  pc_elem = l_elem.
endform.                             "xml_node

*&---------------------------------------------------------------------*
*&      Form  DESCR_TO_XML
*&---------------------------------------------------------------------*
form descr_to_xml using value(pi_tab_descr) type abap_compdescr_tab
                        value(pi_pdocument) type ref to if_ixml_document
                        value(pi_elem)  type ref to if_ixml_element
                        value(pi_value) type any
                        value(pi_content) type boolean.

  data:
        l_field like line of pi_tab_descr,
        l_simple_elem type ref to if_ixml_element,
        result  type i,
        l_name  type string,
        l_value type string,
        l_numc_2_char(12).
  field-symbols: <fs_value>.

  loop at pi_tab_descr into l_field.
    l_name = l_field-name.
    assign component l_field-name of structure pi_value
                                     to <fs_value>.
    if sy-subrc <> 0 or <fs_value> is initial.
      clear l_value.
    else.
      l_value = <fs_value>.
    endif.
    if <fs_value> is initial and not pi_content is initial.
      continue.
    endif.
    l_simple_elem =
       pi_pdocument->create_simple_element( name = l_name
                                            value = l_value
                                            parent = pi_elem ).
    l_name = 'Datatype'.
    l_value = l_field-type_kind.
    result = l_simple_elem->set_attribute( name = l_name
                                           value = l_value ).
    l_name = 'Length'.
    write l_field-length to l_numc_2_char no-zero.
    condense l_numc_2_char no-gaps.
    l_value = l_numc_2_char.
    result = l_simple_elem->set_attribute( name = l_name
                                           value = l_value ).
    if not l_field-decimals is initial.
      l_name = 'Decimals'.
      write l_field-decimals to l_numc_2_char no-zero.
      condense l_numc_2_char no-gaps.
      l_value = l_numc_2_char.
      result = l_simple_elem->set_attribute( name = l_name
                                               value = l_value ).
    endif.
  endloop.

endform.                               " DESCR_TO_XML

*&---------------------------------------------------------------------*
*&      Form  PARSE_NODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_IREF_NODE  text
*----------------------------------------------------------------------*
form parse_node using value(pi_iref_node) type ref to if_ixml_node
                      value(pi_fieldname) type fieldname
                changing value(pc_tab_data) type standard table
                         value(pc_data) type any.

  data: l_iref_text   type ref to if_ixml_text.
  data: l_string      type string.
  data: l_string2     type string.
  data: l_data        type ref to data.
  data: l_indent      type i.

  field-symbols: <fs_source> type string.
  field-symbols: <fs_target>.
  field-symbols: <fs_structure>.

  l_indent = pi_iref_node->get_height( ).
  l_string = pi_iref_node->get_name( ).

  case pi_iref_node->get_type( ).
    when if_ixml_node=>co_node_element.
      case l_indent.
        when 0.
        when 1.
        when 2.
          clear l_data.
          create data l_data type (l_string).
          catch system-exceptions create_data_unknown_type = 4
                                  create_data_not_allowed_type = 4.
            if sy-subrc <> 0.
              exit.
            endif.
            assign l_data->* to <fs_structure>.
            pc_data = <fs_structure>.
          endcatch.
        when 3.
          pi_fieldname = l_string.
        when others.
      endcase.
    when if_ixml_node=>co_node_text.
*      l_iref_text = cl_ixml_text=>downcast( pi_iref_node ).
*      l_iref_text ?=  pi_iref_node.
      l_string = pi_iref_node->get_name( ).
*      if l_iref_text->ws_only( ) is initial.
      l_string2 = pi_iref_node->get_value( ).
      assign l_string2 to <fs_source>.
      assign component pi_fieldname
                                of structure pc_data to <fs_target>.
      if sy-subrc = 0.
        perform input_data2sap_data using <fs_source>
                                    changing <fs_target> sy-subrc.
      endif.
*      endif.
  endcase.

  pi_iref_node = pi_iref_node->get_first_child( ).

  while not pi_iref_node is initial.

    perform parse_node using pi_iref_node pi_fieldname
                       changing pc_tab_data pc_data.
    pi_iref_node = pi_iref_node->get_next( ).
    check l_indent = 1.
    if not pc_data is initial.
      append pc_data to pc_tab_data.
      clear pc_data.
    endif.
  endwhile.

endform.                               " PARSE_NODE

*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA2SAP_DATA
*&---------------------------------------------------------------------*
form input_data2sap_data using value(pi_source)  type any
                         changing value(pi_target)  type any
                                  value(pc_subrc)   type sysubrc.
  data: pi_field_type type c.
  data: l_date type sydatum.
  data: l_decimals type i.
  data: l_decimals_target type i.
  field-symbols: <fs_type_x> type x.

  describe field pi_target type pi_field_type.

  clear pc_subrc.

  case pi_field_type.
    when 'C'.
      pi_target = pi_source.

    when 'D'.
* Bitte beachten:
* Die Funktion CONVERT_DATE_TO_INTERNAL erwartet das Datum analog den
* Einstellungen in den Festwerten des Benutzers. Ist dort das Format
* TT.MM.JJJJ eingestellt, dann muss der Routine das Datum in der
* Form 31.12.1999 oder 31121999 übergeben werden.
* Ist bei den Benutzerfestwerten das amerikanische Format JJJJ.MM.TT
* eingestellt, dann erwartet die Funktion das Datum in der Form
* 1999.12.31 oder 19991231
* Einige Excel Datumsformate haben die Eigenart, zusätzliche Leerzeichen
* und Punkte in das Datum einzufügen. Deshalb werden Punkte in der
* DO Schleife entfernt und anschliessend noch die Leerzeichen durch
* den condense entfernt.
      do 3 times.
        replace '.' with ' ' into pi_source.
      enddo.
      condense pi_source no-gaps.

      if  not pi_source is initial   "Space
      and pi_source <> '00.00.0000'
      and pi_source <> '00000000'.
        call function 'CONVERT_DATE_TO_INTERNAL'
             exporting
                  date_external = pi_source
             importing
                  date_internal = pi_target
             exceptions
                  error_message = c_rc4
                  others        = c_rc4.
        if sy-subrc <> c_rc0.
          pc_subrc = 4.
        endif.
      endif.

    when 'T'.
      if not pi_source is initial and pi_source <> '00:00:00'.
        call function 'CONVERT_TIME_INPUT'
          exporting
            input         = pi_source
          importing
            output        = pi_target
          exceptions
            error_message = c_rc4
            others        = c_rc4.
        if sy-subrc <> c_rc0.
          pc_subrc = 4.
        endif.
      endif.
    when 'X'.
      assign pi_source to <fs_type_x> .
      if sy-subrc <> c_rc0.
        pc_subrc = 8.
      endif.
      pi_target = <fs_type_x> .
    when 'N'.
      describe field pi_target decimals l_decimals_target.
      perform perpare_number changing pi_source l_decimals.
      if pi_source cn c_darl_number.
        pc_subrc = c_rc8.
      else.
        l_decimals_target = l_decimals_target - l_decimals.
        pi_target = pi_source * ( 10 ** ( l_decimals_target ) ).
      endif.
    when 'I'.
      describe field pi_target decimals l_decimals_target.
      perform perpare_number changing pi_source l_decimals.
      if pi_source cn c_darl_number.
        pc_subrc = 8.
      else.
        l_decimals_target = l_decimals_target - l_decimals.
        pi_target = pi_source * ( 10 ** ( l_decimals_target ) ).
      endif.
    when 'P'.
      data:
        l_len_source type i,
        l_len_target type i,
        l_len_prefix type i,
        l_minus type xfeld.
      l_len_source = strlen( pi_source ).
      l_len_prefix = l_len_source - 1.
      if pi_source(1) = '-'.
        l_minus = 'X'.
        shift pi_source by 1 places left.
      elseif pi_source+l_len_prefix(1) = '-'.
        l_minus = 'X'.
        pi_source = pi_source(l_len_prefix).
      endif.

      describe field pi_target decimals l_decimals_target.
      perform perpare_number changing pi_source l_decimals.

      describe field pi_target length l_len_target in byte mode.
      l_len_target = ( l_len_target * 2 ) - 1.
      l_len_source = strlen( pi_source ).
      if l_len_source > l_len_target.
        l_decimals_target = l_decimals_target -  l_len_target +
                            l_len_source.
        pi_source = pi_source(l_len_target).
      endif.

      if pi_source cn '1234567890 -+'.
        pc_subrc = 8.
      else.
        l_decimals_target = l_decimals_target - l_decimals.
        pack pi_source to pi_target.
        pi_target = pi_target * ( 10 ** ( l_decimals_target ) ).
      endif.

*     calculation type of .. *(10**10) is binary float
*     binary float calculations might always have an inherent
*     small loss of precision.
      if l_decimals_target > 7.
        data l_diff type decfloat16.
        data l_source type decfloat16.
        pack pi_source to l_source.
        l_source = l_source / ( 10 ** ( l_decimals ) ).
        l_diff = l_source - pi_target.
        pi_target = pi_target + l_diff.
      endif.

      if not l_minus is initial.
       pi_target = pi_target * -1.
      endif.
    when 'F'.
      call function 'CHAR_FLTP_CONVERSION'
        exporting
          string = pi_source
        importing
          flstr  = pi_target
        exceptions
          others = c_rc4.
      if sy-subrc <> 0.
        pc_subrc = 4.
      endif.
    when others.
      pi_target = pi_source.
  endcase.

endform.                               " INPUT_DATA2SAP_DATA

*---------------------------------------------------------------------*
*       FORM create_spreadsheet                                       *
*---------------------------------------------------------------------*
form create_spreadsheet
      using
            value(pi_application) type char80
      changing
            value(pc_filename) type rlgrap-filename
            value(pc_oref_container) type ref to cl_gui_custom_container
            value(pc_iref_control) type ref to i_oi_container_control
            value(pc_iref_error) type ref to i_oi_error
            value(pc_iref_document) type ref to i_oi_document_proxy
            value(pc_iref_spreadsheet).

  data l_item_url(256) type c.
  data l_retcode type soi_ret_string.
  data: l_has type i.

* open an existing spreadsheet
  perform get_spreadsheet_interface using pi_application
                                    changing
                                      pc_filename
                                      pc_oref_container pc_iref_control
                                      pc_iref_error     pc_iref_document
                                      pc_iref_spreadsheet.
  check not pc_iref_document is initial.
* a spreadsheet must be created
  if pc_iref_spreadsheet is initial.
    call method pc_iref_document->create_document
      exporting
        open_inplace = 'X'
      importing
        retcode      = l_retcode.

    call method pc_iref_document->has_spreadsheet_interface
      importing
        is_available = l_has.

    if not l_has is initial.
      call method pc_iref_document->get_spreadsheet_interface
        importing
          sheet_interface = pc_iref_spreadsheet.
    endif.
  endif.

endform.                           "create_spreadsheet

*&---------------------------------------------------------------------*
*&      Form  GET_SPREADSHEET_INTERFACE
*&---------------------------------------------------------------------*
form get_spreadsheet_interface
      using
           value(pi_application) type char80
      changing
            value(pc_filename) type rlgrap-filename
            value(pc_oref_container) type ref to cl_gui_custom_container
            value(pc_iref_control) type ref to i_oi_container_control
            value(pc_iref_error) type ref to i_oi_error
            value(pc_iref_document) type ref to i_oi_document_proxy
            value(pc_iref_spreadsheet).
  data l_item_url(256) type c.
  data l_retcode type soi_ret_string.
  data l_has type i.

* don't do anything in batch, because there is no GUI...
  check sy-batch is initial.

  l_item_url = pc_filename.
  set locale language sy-langu.
  translate pc_filename to upper case.
  set locale language space.

  if pc_filename(7) <> 'HTTP://' and pc_filename(7) <> 'FILE://'.
    concatenate 'FILE://' l_item_url into l_item_url.
  endif.
  pc_filename = l_item_url.

  call method c_oi_container_control_creator=>get_container_control
    importing
      control = pc_iref_control
      error   = pc_iref_error.
  create object pc_oref_container
            exporting container_name = 'TRUX_CONTAINER'.

*  call method pc_oref_container->set_visible exporting visible = space.

  call method pc_iref_control->init_control
                      exporting r3_application_name =
                                            'R/3 TR'        "#EC NOTEXT
                                inplace_enabled = 'X'
                                inplace_scroll_documents = 'X'
                                parent = pc_oref_container
*                              register_on_close_event = 'X'
*                              register_on_custom_event = 'X'
*                              no_flush = 'X'
                      importing error = pc_iref_error.

  call method pc_iref_control->get_document_proxy
    exporting
      document_type  = pi_application
    importing
      document_proxy = pc_iref_document
      error          = pc_iref_error.

  call method pc_iref_document->open_document
    exporting
      open_inplace = 'X'
      document_url = l_item_url                     "open_readonly = 'X'
    importing
      retcode      = l_retcode.

  call method pc_iref_document->has_spreadsheet_interface
    importing
      is_available = l_has.

  if not l_has is initial.
    call method pc_iref_document->get_spreadsheet_interface
      importing
        sheet_interface = pc_iref_spreadsheet.
  endif.

endform.                               " GET_SPREADSHEET_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  PARSE_TABLE_LINE
*&---------------------------------------------------------------------*
form parse_table_line using    value(pi_table) type soi_generic_table
                               value(pi_tabix) type sytabix
                      changing value(pc_struc_data) type any.
  data: l_table like line of pi_table.
  data: l_field_type.
  data: l_component type i.

  field-symbols: <fs_table>, <fs_struc_data>.

  clear pc_struc_data.

  loop at pi_table into l_table.
    check not l_table-value is initial.
    l_component = l_table-column.
    assign component l_component of structure pc_struc_data to
                     <fs_struc_data>.
    check sy-subrc = 0.
    perform input_data2sap_data using l_table-value
                                changing <fs_struc_data> sy-subrc.
    if sy-subrc <> c_rc0.
      clear pc_struc_data.
      describe field <fs_struc_data> type l_field_type.
    endif.
    case sy-subrc.
      when 0.
      when 4.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                raising conversion_failed.

      when 8.
        message id c_ux type c_error number c_899
                with l_field_type  l_table-value
                     sy-index pi_tabix
                raising conversion_failed.
    endcase.
  endloop.
endform.                               " PARSE_TABLE_LINE

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_TABLE
*&---------------------------------------------------------------------*
form build_field_table tables pc_tab_fields structure rfc_fields
                       using  value(pi_oref_structure)
                                       type ref to cl_abap_structdescr.
  data l_tabname type  dd02l-tabname.
  data l_component type abap_compdescr.
  data l_field type rfc_fields.
  data l_offset type i.

  search pi_oref_structure->absolute_name for '\TYPE='.
  if sy-subrc = 0.
    sy-fdpos = sy-fdpos + strlen( '\TYPE=' ) .
    l_tabname = pi_oref_structure->absolute_name+sy-fdpos.
  else.
    l_tabname = 'UNKNOWN'.
  endif.

  clear l_offset.
  l_field-tabname = l_tabname.
  loop at pi_oref_structure->components into l_component.
    move-corresponding l_component to l_field.
    l_field-fieldname = l_component-name.
    l_field-exid = l_component-type_kind.
*   note 932775 2006 CS Unicode format
    l_field-intlength = l_component-length
                        / cl_abap_char_utilities=>charsize.
    l_field-position = sy-tabix.
    l_field-offset = l_offset.
    l_offset = l_offset + l_field-intlength.
    append l_field to pc_tab_fields.
  endloop.

endform.                               " BUILD_FIELD_TABLE

*&---------------------------------------------------------------------*
*       FORM correct_decimals_for_current                             *
*---------------------------------------------------------------------*
form correct_decimals_for_current tables i_tab_converted_data.
  data l_oref_descr_source type ref to cl_abap_structdescr.
  data l_tab_dfies type table of dfies.
  data l_dfies like line of l_tab_dfies.
  data l_decimals_target type currdec.
  data l_decimals_ddic type currdec.
  data l_decimals_int type i.
* data l_currency type trca_company-currency.
  data l_tabname type  dd02l-tabname.

  data: companycode_detail like t001_bf.
  data: t_appendix         like t001z_bf occurs 0 with header line.

  field-symbols: <l_amount>, <l_waers>, <l_bukrs>.

  describe table i_tab_converted_data lines sy-tabix.
  if sy-tabix <> 0.
* get information from DDIC
    l_oref_descr_source ?=
        cl_abap_typedescr=>describe_by_data( i_tab_converted_data ).

    search l_oref_descr_source->absolute_name for '\TYPE='.
    if sy-subrc = 0.
      sy-fdpos = sy-fdpos + strlen( '\TYPE=' ) .
      l_tabname = l_oref_descr_source->absolute_name+sy-fdpos.
      call function 'LOAN_CHECK_STRUCTURE_INIT'
        exporting
          i_structure_tabname = l_tabname
        tables
          it_dfies            = l_tab_dfies
        exceptions
          others              = 4.
    endif.
    loop at l_tab_dfies into l_dfies where datatype = 'CURR'.
      l_decimals_ddic = l_dfies-decimals.
      loop at i_tab_converted_data.
*       get the Currency
        assign component l_dfies-reffield
               of structure i_tab_converted_data
               to <l_waers>.
        if sy-subrc <> 0.     "Hauswährung!!!
          assign component 'BUKRS'
                 of structure i_tab_converted_data
                 to <l_bukrs>.
          if sy-subrc <> 0.
            continue.
          else.
*            call function 'TRCA_COMPANYCODE_GETDETAIL'
*                 exporting
*                      companycode = <l_bukrs>
*                 importing
*                      currency    = l_currency
*                 exceptions
*                      not_found   = 1
*                      others      = 2.
            call function 'FI_COMPANYCODE_GETDETAIL'
              exporting
                bukrs_int       = <l_bukrs>
                authority_check = space
              importing
                t001_int        = companycode_detail
              tables
                t001z_int       = t_appendix
              exceptions
                bukrs_not_found = 1
                others          = 2.
            if sy-subrc = 0.
              assign component 'WAERS'
              of structure companycode_detail to <l_waers>.
            else.
              continue.
            endif.
          endif.
        endif.
*       get the decimals
        perform get_currency_decimals using    <l_waers>
                                      changing l_decimals_target.
*       correction of ddic:
        l_decimals_int = l_decimals_ddic - l_decimals_target.
        if l_decimals_int <> 0.
*         get the amount
          assign component l_dfies-fieldname
                 of structure i_tab_converted_data
                 to <l_amount>.
          if sy-subrc = 0.
*           calculate currency dependend
            <l_amount> = <l_amount>  / ( 10 ** l_decimals_int ).
            modify i_tab_converted_data.
          endif.
        endif.
      endloop.
    endloop.
  endif.
endform.                    "correct_decimals_for_current
