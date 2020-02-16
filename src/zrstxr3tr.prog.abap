* R3TR R3-Transport Utilities SAPscript ADO objects
*
* TM  L1AK000671 - added forms for uploading a device type without a dialog
* QVN B20K004565 - adapted to new protocol interface NEW_LOGPROT...
* QVN B20K005515 - skip records only at ERROR
* QVN B20K005546 - skip records only if not already at end of object...
* QVN B20K005854 - increase performance with PRIN, use array insert...
*
* QCJ B20K006933 - load of PRIN is deleted for all clients
*                  load of FORM, STYLE is deleted by activation
* QVN B20K008770 - use SAVEMODE_DIRECT = X for Verbuchungstexte
* B20K009048 allow to override the default filename
*            /usr/sap/trans/clipboard/RSTXR3TR.sy-uname
* B20K011772     - no error but warning when import fails since language
*                  of object does not exist in target system
* QVN B20K014424 - do NOT ignore leading blanks in name,obj,id in
*                  R3TR TEXT obj,name,id,s
*                  Allow max.length of text name to be 70 characters
*                  instead of 50.
* QVN B20K015825 - make sure that for layout sets/styles only text IDs
*                  DEF and TXT are transported
* QVN B20K016101   Allow max.length of text key to be 88 characters:
*                  R3TR TEXT object,name,id,l
*                  where len(object) <= 10
*                        len(name)   <= 70
*                        len(id)     <= 4
*                        len(l)       = 1
* QVN B20K018550   Increase maximum record length from 255 to 370
* QVN B20K022408   do not transport short texts TSP1T
*                  do not transport font families TFO01
*                  do not transport system barcodes TFO05
* QVN B20K022630   if TRANSTAT=1, no export of styl/form translations
*                  and import deletes form/style translations
* QVN B20K025470   prepare LANGUAGE VECTOR control and FORT/STYT
*                  TRANSPORT OBJECTS FOR LANGUAGE TRANSPORT
*                  prevent EXPORT of non-SAPscript texts (i.e. word)
*                  transport ALL translations of FORM/STYL,ignore
*                  TDTRANSTAT
* QVN B20K025858   activate LANGUAGE VECTOR control on FORM/STYL/TEXT
*                  export/import
*                  allow for FORT/STYT objects
* QVN B20K026137   add LANGVEC as report parameter
* QVN B20K026627   allow FORT/STYT in direct RSTXR3TR call
* QVN B20K026700   ignore LANGVEC when layout set/style has attribute
*                  TRANSLATION_NOT_WANTED
* QVN B20K027316   allow transport of non-sapscript texs using hex
*                  conversion (TDTEXTTYPE)
* QVN B20K030020   correct FORT/STYT error, could not import these
* QVN B20K030170   make dummy export (EXPORT_NOTHING) for TEXT/STYx/FORx
*                  when language vector prohibits export
* QVN B20K032381   inform spooler after uploading PRIN with RSTXSCRP
* QVN B20K037787   use SELECT_TEXT to get generic text entries
* QVN B20K040519   R3TR FORT,R3TR STYT: prevent export and import of
*                  any language if TDTRANSTAT=1
* QVN B20K043010 - add binary file format and codepage conversion
*                  option
*                  increase record length from 370 to 512
* QVN B20K046925   TDTRANSTAT=1 -> export TXT part only in OSPRAS
* QVN B20K047847   RSTXSCRP: add binary file format
* QVN B20K049141   Replace SYSTEM_CODEPAGE with current codepage
* QVN B20K049191   SET LOCALE LANGUAGE for LANGVEC with single language
*                  so syscp will be correct for language export/import
* QVN B20K051883   hardcoded translate codepage for AS/400 lang imp
* QVN B20K053928   allow GUI file upload/download
* QVN B20K055867   add no-display parameter EXTPROT
* QVN B20K057010   allow deleting device types if deleted in source
* QVN B20K057147   new export routinge EXPORT_31 for masterlang only
*                  FORM/STYL: check against langvec during export
*                             only when MASTERLANG_ONLY mode
* QVN B20K060611   TSP1D import: only INSERT, no UPDATE->new fields
* QVN B20K061027   new selection screen
* QVN B20K061850   new spool table TSP06A for aggregated device formats
* QVN B20K064895   don't export/import DPAP cmd (don't delete TSP1D)
* B20K065126   output T100 messages in initial SY-LANGU
* B20K067651   use RSPO_PTYPE_FLUSH to inform spool of devtype import
* B20K068290   use msg 085 instead of 084
* B20K070087   use TR_READ_COMM to get objects from transport request
* B20K086195   new fields in TSP1D, TSP0A, TSP06A
* B20K089520   binary file format for GUI-upload/download
* B20K8A03QU   dataset like rlgrap-filename  " length 128
* B20K8A0IS4   replace DOWNLOAD/UPLOAD by GUI_DOWNLOAD/UPLOAD
* B20K8A0JDO bad param type for TRANSLATE_FROM(..)
*
report zrstxr3tr line-size 132 no standard page heading message-id td.
include rstxdata.

tables:
  dderr, stxh,
  itcrs, t100,
  tfo03, tfo04, tsp06, tsp1d, tsp06a,
  tsp0a, tsp09, t022d, tfo06, tcp00.

data e071_tab like e071 occurs 100 with header line.         "B20K070087

data: dummy(80).
dummy = 'Auftragsselektion und Modussteuerung'(021).
selection-screen begin of block par_obj with frame title text-021.
parameters:
  trkorr like e071-trkorr,
  mode(6) default 'EXPORT'.
selection-screen end of block par_obj.
dummy = 'Steuerparameter für Datei-Operation'(022).
selection-screen begin of block par_fil with frame title text-022.
parameters:
  servfil  radiobutton group fsrc default 'X',
  localfil radiobutton group fsrc,
  dataset like rlgrap-filename default 'c:\temp\RSTXR3TR.****',
  binfile as checkbox default space, " if =X, binary format
  listfile as checkbox default space.  "if =X, list dataset lines
selection-screen end of block par_fil.
dummy = 'Kontrolle über Sprachversionen'(023).
selection-screen begin of block par_lan with frame title text-023.
parameters:
  plangvec(60) default space,          "custom language vector
  mastlang as checkbox default space.  "if =X, export masterlang only
selection-screen end of block par_lan.
*                                      "B20K055867
parameters:
  extprot(1) default space no-display. "use external protocol interf.?

data begin of header.
       include structure thead.
data end of header.

data begin of header_tab occurs 20.
       include structure thead.
data end of header_tab.

data begin of header_def.
       include structure thead.
data end of header_def.

data begin of lines occurs 50.
       include structure tline.
data end of lines.

data:begin of tfo03_line,
       cpi(10),
       all(240),
     end of tfo03_line.

data:begin of tsp1dx_line,               "SP1X holds new info from TSP1D
       papart like tsp1d-papart,
       listarea like tsp1d-listarea,
       mrg_top(6) type n,
       mrg_left(6) type n,
       mrg_bot(6) type n,
       mrg_right(6) type n,
     end of tsp1dx_line.

data:begin of tsp06_line,
       pdlfdnr(3) type n,
       pddatalen(3) type n,
       all(240),
     end of tsp06_line.

data:begin of tsp06a_line,              "SP6A holds old info from TSP06A
       ptype like tsp06a-ptype,
       paper like tsp06a-paper,
       base like tsp06a-base,
       version like tsp06a-version,
       convflag like tsp06a-convflag,
       convcodep like tsp06a-convcodep,
       postflag like tsp06a-postflag,
       listdriver like tsp06a-listdriver,
       extension like tsp06a-extension,
       chgname1 like tsp06a-chgname1,
       chgtstmp1 like tsp06a-chgtstmp1,
       chgsaprel1 like tsp06a-chgsaprel1,
       chgsapsys1 like tsp06a-chgsapsys1,
       chgname2 like tsp06a-chgname2,
       chgtstmp2 like tsp06a-chgtstmp2,
       chgsaprel2 like tsp06a-chgsaprel2,
       chgsapsys2 like tsp06a-chgsapsys2,
       chgname3 like tsp06a-chgname3,
       chgtstmp3 like tsp06a-chgtstmp3,
       chgsaprel3 like tsp06a-chgsaprel3,
       chgsapsys3 like tsp06a-chgsapsys3,
     end of tsp06a_line.

data:begin of tsp06ax_line,             "SP6X holds new info from TSP06A
       ptype like tsp06a-ptype,
       paper like tsp06a-paper,
       lstdriver like tsp06a-lstdriver,
       lstsubtype like tsp06a-lstsubtype,
       driverinfo like tsp06a-driverinfo,
       spacemode like tsp06a-spacemode,
       charwidth(5) type n,
       fontsize like tsp06a-fontsize,
     end of tsp06ax_line.

data:begin of tsp1d_line,
       papart like tsp1d-papart,
       pformat like tsp1d-pformat,
       orient like tsp1d-orient,
       type like tsp1d-type,
       outcolumns like tsp1d-outcolumns,
       outrows like tsp1d-outrows,
       chgname1 like tsp1d-chgname1,
       chgtstmp1 like tsp1d-chgtstmp1,
       chgsaprel1 like tsp1d-chgsaprel1,
       chgsapsys1 like tsp1d-chgsapsys1,
       chgname2 like tsp1d-chgname2,
       chgtstmp2 like tsp1d-chgtstmp2,
       chgsaprel2 like tsp1d-chgsaprel2,
       chgsapsys2 like tsp1d-chgsapsys2,
       chgname3 like tsp1d-chgname3,
       chgtstmp3 like tsp1d-chgtstmp3,
       chgsaprel3 like tsp1d-chgsaprel3,
       chgsapsys3 like tsp1d-chgsapsys3,
     end of tsp1d_line.

data: begin of tsp06pot_line,
      devtype like tsp06pot-devtype,
      poption(3) type n,
      optval(3) type n,
      lang(2) type c,
      utf16len(3) type n,
      utf16hexstring(240) type c,
      end of tsp06pot_line.

* internal tables for T022D, TSP06, TSP06A, TFO03, TFO04, TFO06
data buf_t022d like t022d occurs 50 with header line.
data buf_tsp06 like tsp06 occurs 200 with header line.
data buf_tsp06a like tsp06a occurs 200 with header line.
data buf_tfo03 like tfo03 occurs 20 with header line.
data buf_tfo04 like tfo04 occurs 1000 with header line.
data buf_tfo06 like tfo06 occurs 10 with header line.
data buf_tsp1d like tsp1d occurs 10 with header line.
data buf_tsp06pot like tsp06pot occurs 10 with header line.

field-symbols: <name>.

data:
  begin of textlow,
    tdobject like thead-tdobject,
    tdname   like thead-tdname,
    tdid     like thead-tdid,
    tdspras  like thead-tdspras,
  end of textlow,

**filename(60),
  filename          like rlgrap-filename,
  func_activate(4)  value 'ACTV',
  func_olanguage(4) value 'OLAN',
  func_nothing(4)   value 'NONE',
  func_del_paper(4) value 'DPAP',
  func_del_form(4)  value 'DFOR', "B20K022630
  func_del_styl(4)  value 'DSTY', "B20K022630
  func_del_prin(4)  value 'DPRI', "B20K057010
  clipboard  like boolean,
  export_flag like boolean,
  subrc like sy-subrc,
  begin of record occurs 0,
    typ(1),
    command(4),
    data(507), "B20K043010
  end of record,
  record_nostruct(512).

constants: c_record_numchars type i value 512.
* infos for UNICODE, binary file handling
data:
  record_numbytes type i,
  r3_internal_charset(1).
constants: c_charset_ascii  value 'A',
           c_charset_ebcdic value 'E',
           c_charset_unicode value 'U'.
* GUI file table
data: begin of gui_file_text occurs 0,
  l(512) type c, "number of bytes must fit one CHAR RECORD
  end of gui_file_text.
data: gui_file_text_cur_line_index like sy-tabix.
data: begin of gui_file_bin occurs 0,
  l(512) type x,
  end of gui_file_bin.
data: gui_file_bin_cur_line_index like sy-tabix,
      gui_file_bin_cur_lineofs type i,
      gui_file_bin_total_bytes type i.
constants: c_guifile_bin_numbytes type i value 512.
data:      c_guifile_bin_codepage like tcp00-cpcodepage value '1100'.
*
data:
* the following flag is TRUE when IMPDATA returned RC <> 0 ONCE
  end_of_data like boolean,
* the following flag is TRUE when our "E" marker is read from the file
  end_of_objdata like boolean,
* the following flag is TRUE when READ on transport dataset fails
  end_of_clipboard like boolean,
* the following flag is TRUE when a dummy object was exported/imported
  nothing like boolean,
* the following flag is TRUE when object import was successful
  import_ok like boolean,
* counts number of fatal error messages
  count_error(3) type n,
* counts number of warning messages
  count_warning(3) type n,
  object(10) type c,
* max len of NAME will be 10+1+70+1+4+1+1 = 88   "B20K016101
  name_len like integer value 88,                "B20K016101
  name_pos like integer,
  name(89) type c, "B20K016101
* language vector to be used for export/import
* LANGUAGE_VECTOR LIKE LCOLOBJ-LANGVECTOR. "not transported yet
  language_vector(60) type c,
  custom_language_vector(60) type c,
  custom_language_vector_valid(8) type c value space,
  custom_language_vector_magic(8) type c value 'LangVect',
* flag if dataset contents should be printed with the protocol
  list_file_contents(1) type c,
* flags for GUI upload/download
  file_source_local(5) type c value space,       "B20K053928
  file_source_local_magic(5) type c value 'Local',           "#EC NOTEXT
* flags for MASTERLANG_ONLY transport
  masterlang_only_flag(5) type c value space,    "B20K057147
  masterlang_only_magic(5) type c value 'MastO',
**********************************************************
* data for binary file and compression
*
* binfile flag and codepage
  binary_file_format_valid(7) type c value space,
  binary_file_format_magic(7) type c value 'BinFile',
  binfile_codepage like tcp02-cpcodepage value '0000',
  system_codepage like tcp02-cpcodepage.
constants:
  binary_file_header_byte1 type x value '52', "R
  binary_file_header_byte2 type x value '53', "S
  binary_file_header_byte3 type x value '54', "T
  binary_file_header_byte4 type x value '58', "X
  binary_file_header_byte5 type x value '40'. "@
* compression tables and data
data: begin of fc_full_tab occurs 0,
  record(512) type x,
      end   of fc_full_tab.
data: fc_full_tab_lines like sy-tabix.
data: begin of fc_comp_tab occurs 0,
  record(1024) type x,
      end   of fc_comp_tab.
constants: fc_comp_tab_numbytes type i value 1024.
data: fc_comp_tab_lines like sy-tabix.
constants: fc_full_tab_maxlines like sy-tabix value 5000.
**********************************************************
data begin of paper.
data  pdpaper like tsp06-pdpaper.
data  pdptype like tsp06-pdptype.
data end of paper.
* constants for new protocol interface NEW_LOGPROT
data: nlp_lv1(1) type c value '1',
      nlp_lv2(1) type c value '2',
      nlp_lv3(1) type c value '3',
      nlp_err(1) type c value 'E', "error
      nlp_war(1) type c value 'W', "warning
      nlp_inf(1) type c value ' ', "info
      nlp_lan(1) type c value 'E', "default message language
      nlp_mid(2) type c value 'TD',"message id
      nlp_nob(1) type c value ' '. "no new object?
* variables for new protocol interface NEW_LOGPROT
data: activate_object like boolean,     "tried to activate object
      activate_object_ok like boolean,  "activate o.k.
      external_protocol(1) value space, "langimp->use ext.protocol?
      message_language like sy-langu value space.            "B20K065126

data: l_authority_check like boolean value 'X'.
data: g_flag_utf8 type boolean value false.
data: cop like c_guifile_bin_codepage value '0000'.
data: lop like c_guifile_bin_codepage value '0000'.
data: eop type abap_encod.
data: record_nostruct_z(512).
********************************
*
* main program used by report RSTXR3TR
*
********************************
start-of-selection.
data rc like sy-subrc.

perform set_message_language using sy-langu.                 "B20K065126
* set list file mode...
if listfile = 'X'.
  list_file_contents = 'X'.
else.
  list_file_contents = space.
endif.
* set custom langvec...
perform set_custom_language_vector using plangvec.
* set masterlanguage mode
perform set_masterlang_only_flag using mastlang. "B20K057147
* set record source/target to file
clipboard = true.
* set log protocol target
if extprot = true. "B20K055867
  external_protocol = 'X'.
else.
  external_protocol = space.
endif.
* set file format
if binfile = 'X'.
  perform set_binary_file_format using true.
  perform fc_init. "init compress tables
else.
  perform set_binary_file_format using false.
endif.
* set file source
if localfil = space or sy-batch = 'X'. "B20K053928
  perform set_file_source using false.
else.
  perform set_file_source using true.
endif.
* the default dataset name is /usr/sap/trans/clipboard/RSTXR3TR.sy-uname
if dataset cs '****'.
  replace '****' with sy-uname into dataset.
  condense dataset no-gaps.
endif.
filename = dataset.
condense filename no-gaps.
case mode.
************************** E X P O R T ***************
  when 'EXPORT'.
    format color col_group.
    write: /
'*************** Start SAPscript Transport RSTXR3TR ************'(019).
    perform get_ta.
*   open file
    perform get_binary_file_format_flag.
    if sy-subrc = 0.
      perform file_open using filename 'O' 'B'. "binary
      check sy-subrc = 0.
      perform export_clipboard_bin_header.
    else.
      perform file_open using filename 'O' 'T'. "text
      check sy-subrc = 0.
    endif.
*   write header entry TCOMM...with transport number
    perform export_ta.
*   loop over export objects
    loop at e071_tab where pgmid = 'R3TR'.
      perform export using e071_tab-object e071_tab-obj_name.
    endloop.
*   close file
    perform get_binary_file_format_flag.
    if sy-subrc = 0. "binary format
      perform fc_flush_buffer using filename.
    endif.
    perform file_close using filename 'O'.

************************** I M P O R T ***************
  when 'IMPORT'.
    format color col_group.
    write / text-019. "Start SAPscript transport
    end_of_clipboard = false.
*   authority-check: IMPORT
    if l_authority_check eq 'X'.
      call function 'TRINT_TP_CHECK_AUTHORITY'
           exporting  iv_tp_command     = 'IMPORT'
           exceptions permission_denied = 1
                      others            = 2.
      if sy-subrc <> 0.
        write: / 'keine Berechtigung für IMPORT'(080).
        perform newprot using sy-msgty nlp_lv2 sy-msgid sy-msgno
                              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*       PERFORM NEWPROT USING NLP_ERR NLP_LV2 NLP_MID '342' '' '' '' ''.
        exit.
      endif.
    endif.
*   open file
    perform get_binary_file_format_flag.
    if sy-subrc = 0.
      perform file_open using filename 'I' 'B'.
      check sy-subrc = 0.
*     read binary header, get file codepage
      perform import_clipboard_bin_header using binfile_codepage.
      case sy-subrc.
        when 0.
          format color col_total.
          write: /
  'Binäres Dataset enthält Daten in Codepage'(060), binfile_codepage.
        when 1.
          format color col_negative.
          write: /
      'Dataset enthält kein korrektes Binärformat'(061).
          exit.
        when 2.
          format color col_negative.
          write: /
      'Dataset enthält eine unbekannte Codepage'(062), binfile_codepage.
          exit.
      endcase.
    else.
      perform file_open using filename 'I' 'T'.
      check sy-subrc = 0.
    endif.
*   read header entry with transport number
    perform import_ta using rc.
    if rc <> 0.
      perform file_close using filename 'I'.
      exit.
    endif.
*   loop over import-objects
    while end_of_clipboard = false.
      perform import using sy-subrc.
    endwhile.
*   close file
    perform file_close using filename 'I'.
  when others.
    format color col_negative.
    write: /
'ERROR - Als Modus-Parameter nur EXPORT oder IMPORT verwenden'(051).
    exit.
endcase.
format color col_group.
write: /
'*************** Ende SAPscript Transport RSTXR3TR *************'(020).

********************************
*
* entry routine for report RSTXSCRP
*
********************************
form rstxscrp using object obj_name mode file list_file lang_vec
                    binfile_flag localfile_flag mastlang_flag.
data rc like sy-subrc.
statics devtype like tsp03-patype.
data erc type i.
data wa_tsp0a type tsp0a.

perform set_message_language using sy-langu.                 "B20K065126
* set list file mode...
if list_file = 'X'.
  list_file_contents = 'X'.
else.
  list_file_contents = space.
endif.
* set record source/target to file
clipboard = true.
* set log protocol target to report list
external_protocol = space. "B20K055867
* set file format
if binfile_flag = 'X'.
  perform set_binary_file_format using true.
  perform fc_init. "init compress tables
else.
  perform set_binary_file_format using false.
endif.
* set file source
if localfile_flag = space or sy-batch = 'X'. "B20K053928
  perform set_file_source using false.
else.
  perform set_file_source using true.
endif.
* set custom langvec...
perform set_custom_language_vector using lang_vec.
* set masterlanguage mode
perform set_masterlang_only_flag using mastlang_flag. "B20K057147
subrc = 0.
end_of_objdata = false.
end_of_clipboard = false.
filename = file.
format color col_group.
write: / text-019. "Start of SAPscript transporter RSTXR3TR
case mode.
  when 'EXPORT'.
************************** E X P O R T ***************
    perform get_binary_file_format_flag.
    if sy-subrc = 0.
      perform file_open using filename 'O' 'B'. "binary
      check sy-subrc = 0.
      perform export_clipboard_bin_header.
    else.
      perform file_open using filename 'O' 'T'. "text
      check sy-subrc = 0.
    endif.
*   write header entry S...with object key
    perform export_sapscript using object obj_name.
*   export object
    perform export using object obj_name.
*   close file
    perform get_binary_file_format_flag.
    if sy-subrc = 0.
      perform fc_flush_buffer using filename.
    endif.
    perform file_close using filename 'O'.
************************** I M P O R T ***************
  when 'IMPORT'.
    end_of_clipboard = false.
    perform get_binary_file_format_flag.
    if sy-subrc = 0.
      perform file_open using filename 'I' 'B'.
      check sy-subrc = 0.
*     read binary header, get file codepage
      perform import_clipboard_bin_header using binfile_codepage.
      case sy-subrc.
        when 0.
          format color col_total.
          write: / text-060, binfile_codepage. "contains data in cp..
        when 1.
          format color col_negative.
          write: / text-061. "no valid binary format
          exit.
        when 2.
          format color col_negative.
          write: / text-062, binfile_codepage. "unknown codepage
          exit.
      endcase.
    else.
      perform file_open using filename 'I' 'T'.
      check sy-subrc = 0.
    endif.
*   read header entry S...with object key
    perform import_sapscript using object obj_name rc.
    if rc <> 0.
      perform file_close using filename 'I'.
      exit.
    endif.
*   loop over import objects
    while end_of_clipboard = false.
      perform import using sy-subrc.
    endwhile.
*   close file
    perform file_close using filename 'I'.
*   inform spooler
    if object = 'PRIN'.
      devtype = obj_name.
      CALL FUNCTION 'RSPO_CF_IS_CASCADING_DEVTYPE'
        EXPORTING
          DEVTYPE                        = devtype
        EXCEPTIONS
          DEVICETYPE_NOT_FOUND           = 1
          DEVICETYPE_NOT_CASCADING       = 2
          OTHERS                         = 3.
      IF SY-SUBRC = 0.
        select single * from tsp0a into wa_tsp0a where patype = devtype.
        if sy-subrc = 0 and wa_tsp0a-driver = 'SWIN'.
          CALL FUNCTION 'I18N_CF_INIT_DEVICETYPE'
            EXPORTING
              IM_DEVNAME       = devtype
              IM_PDLTYPE       = 'SAPWIN'
            IMPORTING
              EX_RC            = erc.
         endif.
      ENDIF.
      call function 'RSPO_PTYPE_FLUSH'
           exporting
                ptype            = devtype
           exceptions
                call_error       = 1
                operation_failed = 2
                others           = 3.
      format color col_total.
      write: / 'Der Spooler wurde über die Änderung informiert'(057).
*     send warning if printer codepage of devtype does not exist or is not migrated
      perform check_dt_codepage using devtype.
    endif.
endcase.
endform.

* new 3.1G export routine for exporting master language ONLY
* B20K057147
form export_31 using export_object export_name value(masterlang_only).
if masterlang_only = 'X'.
  perform set_masterlang_only_flag using true.
else.
  perform set_masterlang_only_flag using false.
endif.
perform export using export_object export_name.
endform.

********************************
*
* global entry for transport routines: object EXPORT
*
********************************
form export using export_object export_name.
export_flag = true.
subrc = 0.
count_error = 0.
count_warning = 0.
nothing = false.
end_of_objdata = false.
object = export_object.
name = export_name.
* object ... ... is presently at work
perform newprot using nlp_inf nlp_lv2 nlp_mid '093'
                    object name space space.
* check object
case object.
  when 'FORM'.
    perform export_formstyl using object_form false.  "complete object
  when 'FORT'.
    perform export_formstyl using object_form true.   "languages only
  when 'PRIN'.
    perform export_printer.
  when 'STYL'.
    perform export_formstyl using object_style false. "complete object
  when 'STYT'.
    perform export_formstyl using object_style true.  "languages only
  when 'TEXT'.
    perform export_texts.
  when others.
*   the transport object .. is unknown
    perform newprot using nlp_err nlp_lv2 nlp_mid '057'
                          object space space space.
    exit.
endcase.
if nothing = false.
  if count_error = 0.
*   export was o.k.
    perform newprot using nlp_inf nlp_lv3 nlp_mid '052'
                          space space space space.
  else.
*   export encountered x fatal errors
    perform newprot using nlp_inf nlp_lv3 nlp_mid '058'
                          count_error space space space.
  endif.
  if count_warning ne 0.
*   export encountered x warnings
    perform newprot using nlp_inf nlp_lv3 nlp_mid '059'
                          count_warning space space space.
  endif.
endif.
endform.

* export object TEXT, expanding generic entries
form export_texts.
data: fill like sy-tfill,
      rc like sy-subrc.

perform get_language_vector using language_vector.
if sy-subrc <> 0. "exit if language vector cannot be read
  nothing = true. exit.
endif.
perform name_to_textkey. "get text key components into TEXTLOW-...
refresh header_tab.
call function 'SELECT_TEXT'
     exporting
*         CLIENT                  = SY-MANDT
          database_only           = 'X'
          id                      = textlow-tdid
          language                = textlow-tdspras
          name                    = textlow-tdname
          object                  = textlow-tdobject
*         TEXTMEMORY_ONLY         = ' '
*         ARCHIVE_HANDLE          = 0
     importing
          entries                 = fill
     tables
          selections              = header_tab
     exceptions
          wrong_access_to_archive = 1
          others                  = 2.
* eliminate some entries?
loop at header_tab.
  if header_tab-tdobject = object_form or
     header_tab-tdobject = object_style.
    delete header_tab.
  else.
*   B20K057147 - no check against langvec during export
  endif.
endloop.
describe table header_tab lines fill.
if fill = 0.
* the object does not exist or is damaged and was not exported
  perform newprot using nlp_err nlp_lv2 nlp_mid '074'
                  space space space space.
  perform export_nothing. exit.
endif.
perform export_header.
loop at header_tab.
* export TEXT object
  perform export_txt using header_tab-tdobject
                            header_tab-tdid
                            header_tab-tdname
                            header_tab-tdspras
                            false rc.
  if rc = 0.
*   text object ... was exported
    perform newprot using nlp_inf nlp_lv3 nlp_mid '086'
                          header_tab-tdobject
                          header_tab-tdname
                          header_tab-tdid
                          header_tab-tdspras.
  else.
*   text object ... was not exported
    perform newprot using nlp_inf nlp_lv3 nlp_mid '091'
                          header_tab-tdobject
                          header_tab-tdname
                          header_tab-tdid
                          header_tab-tdspras.
  endif.
endloop.
perform export_end.
endform.

* get single text key components into TEXTLOW-...
form name_to_textkey.
assign name(1) to <name>.
name_pos = 1.
perform textkey using 10 textlow-tdobject.
perform textkey using 70 textlow-tdname.
perform textkey using 4 textlow-tdid.
perform textkey using 1 textlow-tdspras.
endform.

form textkey using value(len) low.
data n like name.
field-symbols <n>.

assign n(1) to <n>.
while  <name> ne ',' and len > 0 and
       name_pos <= name_len.
  <n> = <name>. subtract 1 from len.
  if len >= 1.
    assign <n>+1 to <n>.
  endif.
  assign <name>+1 to <name>. add 1 to name_pos.
endwhile.
if <name> = ',' and name_pos <= name_len.
  assign <name>+1 to <name>. add 1 to name_pos.
endif.
low = n.
endform.

* check FORM/STYL before exporting
* HEADER_DEF contains DEF part header
* HEADER_TAB table contains TXT part headers
* TRANSTAT is always valid, since DEF part is also read for STYT/FORT
* RC = 0 if o.k. to export
*    = 4 if langvec prohibits export
*    = 8 if inconsistent object
form check_formstyl_for_export using value(language_only)
                                     value(olang)
                                     value(langvec)
                                     value(transtat)
                                     rc.
data: numotxt like integer, "number of TXT parts in olang
      numtxt like integer.  "number of TXT parts allowed by LANGVEC
statics: masterlang_only like boolean. "B20K057147

perform get_masterlang_only_flag.      "B20K057147
if sy-subrc = 0.                       "
  masterlang_only = true.              "
else.                                  "
  masterlang_only = false.             "
endif.                                 "
rc = 0.
numtxt = 0.
if language_only = true.
* FORT/STYT export, only TXT parts must be exported
  if transtat = translation_not_wanted.
*   no language export since object is not language dependent
    perform newprot using nlp_inf nlp_lv3 nlp_mid '049'
                          header_tab-tdspras space space space.
    rc = 4. exit.
  endif.
  loop at header_tab.
    if langvec ca header_tab-tdspras. "spras in LANGVEC
      add 1 to numtxt.
    else.                             "spras not in LANGVEC
*     language vector prohibits translation export/import
      perform newprot using nlp_inf nlp_lv3 nlp_mid '083'
                            header_tab-tdspras space space space.
      delete header_tab.
    endif.
  endloop.
else.
* FORM/STYL export, complete DEF and TXT export
* B20K057147 - check against language vector only for masterlang mode
  if masterlang_only = true.
    if langvec na olang.             "spras in LANGVEC
*     language vector prohibits orig lang export/import
      perform newprot using nlp_inf nlp_lv3 nlp_mid '082'
                            olang space space space.
      rc = 4. exit.
*     msg TD 054 is unused now...
    endif.
  endif.
  numotxt = 0.
  loop at header_tab.
    if header_tab-tdspras = olang.
      add 1 to numotxt.
    endif.
    if transtat = translation_not_wanted. "no translations wanted
      if header_tab-tdspras = olang.      "TXT in OSPRAS
        add 1 to numtxt.
      else.
        delete header_tab.                "TXT other than OSPRAS
      endif.
    else.                                 "translations wanted
      if masterlang_only = true.          "B20K057147
        if header_tab-tdspras <> olang.   "
          delete header_tab.              "export only masterlanguage
        else.                             "
          add 1 to numtxt.
        endif.                            "
      else.                               "
*       B20K057147 - no check against langvec during export
        add 1 to numtxt.                  "
*       msg TD 083 is unused now...
      endif.                              "B20K057147
    endif.
  endloop.
  if numotxt <> 1.
    rc = 8. exit.                     "0 or several TXT parts in olang
  endif.
endif.
if numtxt = 0.                        "no TXT parts to export
  rc = 4. exit.
endif.
endform.

* export a FORM or STYL
* only ACTIVE versions are exported
form export_formstyl using value(object_type) value(language_only).
data: olang like thead-tdospras,
      rc like sy-subrc,
      transtat like thead-tdtranstat,
      tdname like thead-tdname,
      tdobject like thead-tdobject.

case object_type.
  when object_form.
    tdobject = object_form. tdname = name(16).
  when object_style.
    tdobject = object_style. tdname = name(8).
  when others.
    nothing = true. exit.
endcase.
perform get_language_vector using language_vector.
if sy-subrc <> 0. "exit if language vector cannot be read
  nothing = true. exit.
endif.
* always get DEF part, we need TDTRANSTAT for FORT,STYT
clear header_def.
select * from stxh where tdobject = tdobject
                   and   tdname = tdname
                   and   tdid = id_def.
  move-corresponding stxh to header_def.
endselect.
if sy-dbcnt = 1. "only one DEF part allowed
  move-corresponding stxh to header_def.
  olang = header_def-tdspras.       "get original language
  transtat = header_def-tdtranstat. "get translation-allowed flag
else.            "inconsistent: no or several DEF parts
* the object does not exist or is damaged and was not exported
  perform newprot using nlp_err nlp_lv2 nlp_mid '074'
                  space space space space.
  perform export_nothing. exit.
endif.
* get TXT part(s)
refresh header_tab.
select * from stxh where tdobject = tdobject
                   and   tdname = tdname
                   and   tdid = id_txt.
  move-corresponding stxh to header_tab. append header_tab.
endselect.
* check for consistency of FORM/STYL and use LANGUAGE_VECTOR
perform check_formstyl_for_export using language_only
                                        olang
                                        language_vector
                                        transtat
                                        rc.
case rc.
  when 0. " o.k.
  when 4. " LANGVEC prohibits export/import
    perform export_nothing. exit.
  when 8. " inconsistent object
*   the object does not exist or is damaged and was not exported
    perform newprot using nlp_err nlp_lv2 nlp_mid '074'
                    space space space space.
    perform export_nothing. exit.
endcase.
perform export_header.
if language_only = false. "if complete layout set is exported ...
* set olanguage in target system
  perform export_data using func_olanguage olang.
* delete form in target system if no translations wanted
  if transtat = translation_not_wanted.
    if object_type = object_form.
      perform export_data using func_del_form tdname.
    else.
      perform export_data using func_del_styl tdname.
    endif.
  endif.
* export DEF part
  perform export_txt using header_def-tdobject
                           header_def-tdid
                           header_def-tdname
                           header_def-tdspras
                           language_only
                           rc.
  if rc = 0.  "export o.k.
*   definition ... was exported
    perform newprot using nlp_inf nlp_lv3 nlp_mid '062'
                    header_def-tdspras space space space.
  else.       "error during export
*   object ... was not exported
    perform newprot using nlp_inf nlp_lv3 nlp_mid '094'
                    header_def-tdobject header_def-tdname
                    header_def-tdid     header_def-tdspras.
    exit.
  endif.
endif.
* export TXT parts
loop at header_tab.
  perform export_txt using header_tab-tdobject
                            header_tab-tdid
                            header_tab-tdname
                            header_tab-tdspras
                            language_only
                            rc.
  if rc = 0. "export o.k.
*   export was o.k.
    if language_only = false and header_tab-tdspras = olang.
*     original language ... was exported
      perform newprot using nlp_inf nlp_lv3 nlp_mid '061'
                      header_tab-tdspras space space space.
    else.
*     language ... was exported
      perform newprot using nlp_inf nlp_lv3 nlp_mid '063'
                      header_tab-tdspras space space space.
    endif.
  else.      "error during export
*   object ... was not exported
    perform newprot using nlp_inf nlp_lv3 nlp_mid '094'
                    header_tab-tdobject header_tab-tdname
                    header_tab-tdid     header_tab-tdspras.
  endif.
endloop.
if language_only = false.
* activate object in target system if complete transport
  perform export_function using func_activate.
endif.
perform export_end.
endform.

* export a printer definition
form export_printer.
* TSP0A  Printer
  select single * from tsp0a where patype = name(8).
  if sy-subrc ne 0.
*   the object does not exist and will be deleted in target system
    perform newprot using nlp_war nlp_lv2 nlp_mid '056' "B20K057010
                    object name(8) space space.         "
    perform export_header.                              "
    perform export_function using func_del_prin.        "
    perform export_end.                                 "
    exit.                                               "
  endif.
* Driver
  select single * from tsp09 where driver = tsp0a-driver.
  if sy-subrc ne 0.
*   SAPscript driver ... is missing in table TSP09
    perform newprot using nlp_err nlp_lv2 nlp_mid '064'
                    tsp0a-driver space space space.
    exit.
  endif.
  perform export_header.
  perform export_data using 'SP09' tsp09.
  perform export_data using 'SP0A' tsp0a.
* TFO03 Printer-Fonts
  select * from tfo03 where tdprinter = name(8).
    tfo03_line-cpi = tfo03-tdcpi.
*    TFO03_LINE-ALL = TFO03.
    clear tfo03_line-all.
    tfo03_line-all(8) = tfo03-tdprinter.
    tfo03_line-all+8(8) = tfo03-tdfamily.
    tfo03_line-all+16(3) = tfo03-tdfontsize.
    tfo03_line-all+19(1) = tfo03-tdbold.
    tfo03_line-all+20(1) = tfo03-tditalic.
    tfo03_line-all+21(3) = space.  " TFO03-TDCPI NE ASCII
    tfo03_line-all+24(30) = tfo03-tdpprintid.
    tfo03_line-all+54(30) = tfo03-tdlprintid.
    tfo03_line-all+84(1) = tfo03-tdafmflag.
    tfo03_line-all+85(1) = tfo03-tdcpselect.
    perform export_data using 'FO03' tfo03_line.
  endselect.
* TFO06 Printer-Barcodes
  select * from tfo06 where tdprinter = name(8).
    perform export_data using 'FO06' tfo06.
  endselect.
* T022D Print-Controls
  select * from t022d where typ = name(8).
    perform export_data using '022D' t022d.
  endselect.
* TSP06A device format - summary
  select * from tsp06a where ptype = name(8).
*    PERFORM EXPORT_DATA USING 'SP6A' TSP06A.
    clear tsp06a_line.
    move-corresponding tsp06a to tsp06a_line.
    perform export_data using 'SP6A' tsp06a_line.

    tsp06ax_line-ptype      = tsp06a-ptype.
    tsp06ax_line-paper      = tsp06a-paper.
    tsp06ax_line-lstdriver  = tsp06a-lstdriver.
    tsp06ax_line-lstsubtype = tsp06a-lstsubtype.
    tsp06ax_line-driverinfo = tsp06a-driverinfo.
    tsp06ax_line-spacemode  = tsp06a-spacemode.
    tsp06ax_line-charwidth  = tsp06a-charwidth.
    tsp06ax_line-fontsize   = tsp06a-fontsize.
    perform export_data using 'SP6X' tsp06ax_line.
  endselect.
* TSP06 device format - details
  clear paper.
  select * from tsp06 where pdptype = name(8).
    if paper-pdpaper ne tsp06-pdpaper.
       paper-pdptype = tsp06-pdptype.
       paper-pdpaper = tsp06-pdpaper.
*      export current device format entry
       select single * from tsp1d where papart = paper-pdpaper.
       if sy-subrc = 0.
*         PERFORM EXPORT_DATA USING 'SP1D' TSP1D.
         clear tsp1d_line.
         move-corresponding tsp1d to tsp1d_line.
         perform export_data using 'SP1D' tsp1d_line.

         tsp1dx_line-papart = tsp1d-papart.
         tsp1dx_line-listarea = tsp1d-listarea.
         tsp1dx_line-mrg_top  = tsp1d-mrg_top.
         tsp1dx_line-mrg_left = tsp1d-mrg_left.
         tsp1dx_line-mrg_bot  = tsp1d-mrg_bot.
         tsp1dx_line-mrg_right = tsp1d-mrg_right.
         perform export_data using 'SP1X' tsp1dx_line.
       else.
         clear paper.
       endif.
    endif.
    check paper ne space.
    tsp06_line-pdlfdnr = tsp06-pdlfdnr.
    tsp06_line-pddatalen = tsp06-pddatalen.
*    TSP06_LINE-ALL = TSP06.
    clear tsp06_line-all.
    tsp06_line-all(8) = tsp06-pdptype.
    tsp06_line-all+8(16) = tsp06-pdpaper.
    tsp06_line-all+24(8) = tsp06-pdname.
    tsp06_line-all+32(2) = space.     " PDLFDNR,PDDATALEN ne ASCII
    tsp06_line-all+34(72) = tsp06-pddata.
    perform export_data using 'SP06' tsp06_line.
  endselect.
* TFO04 Printer-Metrics
  select * from tfo04 where tdprinter = name(8).
    perform export_data using 'FO04' tfo04.
  endselect.
* TSP06POT print option texts
  refresh buf_tsp06pot.
  select * from tsp06pot into table buf_tsp06pot where devtype = name(8).
  loop at buf_tsp06pot.
    perform opttxt_export using buf_tsp06pot tsp06pot_line.
    if sy-subrc = 0.
      perform export_data using 'SP6T' tsp06pot_line.
    endif.
  endloop.
  perform export_end.
endform.

********************************
*
* global entry for transport routines: object IMPORT
*
********************************
form import using import_subrc.
data msgno like t100-msgnr.

export_flag = false.
subrc = 0.
count_error = 0.
count_warning = 0.
nothing = false.
end_of_data = false.
end_of_objdata = false.
activate_object = false.
activate_object_ok = false.
import_ok = true.
* read header entry of transport object
* sets OBJECT, NAME
perform import_header. "may set SUBRC > 0 if error
if subrc <> 0.
* skip remaining records
  perform skip_unread_records.
  import_subrc = 4.
  exit.
endif.
* call IMPORT subroutines depending on object
case object.
  when 'FORM'.
    perform import_formstyl using object_form false. "complete object
  when 'FORT'.
    perform import_formstyl using object_form true.  "languages only
  when 'PRIN'.
    perform import_prin.
  when 'STYL'.
    perform import_formstyl using object_style false. "complete object
  when 'STYT'.
    perform import_formstyl using object_style true.  "languages only
  when 'TEXT'.
    perform import_text.
endcase.
* save SUBRC
import_subrc = subrc.
* skip remaining records...
perform skip_unread_records.
* send final message about success, errors or warnings...
if nothing = false.
  if import_ok = true.
    if activate_object = false.
*     the object was imported successfully
      perform newprot using nlp_inf nlp_lv3 nlp_mid '071'
                      space space space space.
    else.
      if activate_object_ok = true.
*       the object was imported and activated successfully
        perform newprot using nlp_inf nlp_lv3 nlp_mid '088'
                        space space space space.
      else.
*       the object was imported successfully but not activated
        perform newprot using nlp_inf nlp_lv3 nlp_mid '089'
                        space space space space.
      endif.
    endif.
  endif.
  if count_error > 0.
*   ... fatal errors were encountered during import
    perform newprot using nlp_inf nlp_lv3 nlp_mid '072'
                    count_error space space space.
  endif.
  if count_warning > 0.
*   ... warnings were encountered during import
    perform newprot using nlp_inf nlp_lv3 nlp_mid '073'
                    count_warning space space space.
  endif.
endif.
endform.

* check layout set/style part before saving during import
* RC = 0 if o.k. to import
* RC = 4 if TXT not in langvec
* RC = 8 if DEF not in langvec
form check_formstyl_for_import using value(header) structure thead
                                     value(language_only)
                                     value(olang)
                                     value(langvec)
                                     transtat
                                     rc.
rc = 0.
if language_only = true.
* FORT/STYT import, import only TXT parts, do NOT import anything if
* object is language independent!!!
* check if DEF exists
  select * from stxh where tdobject = header-tdobject
                     and   tdname   = header-tdname
                     and   tdid     = id_def.
  endselect.
  if sy-dbcnt = 1.  "o.k., one DEF part exists, check TDTRANSTAT
    if stxh-tdtranstat = translation_not_wanted. "not language dependent
*     language .. not imported since object is language independent
      perform newprot using nlp_war nlp_lv3 nlp_mid '050'
                            header-tdspras space space space.
      rc = 4. exit.
    endif.
  else.             "no or several DEF parts -> no import
*   language .. not imported since DEF is missing
    perform newprot using nlp_war nlp_lv2 nlp_mid '055'
                          header-tdspras space space space.
    rc = 4. exit.
  endif.
  if langvec na header-tdspras.
*   language vector prohibits translation export/import
    perform newprot using nlp_war nlp_lv3 nlp_mid '083'
                          header-tdspras space space space.
    rc = 4. exit.
  endif.
else.
* FORM/STYL import, import DEF and TXT parts
  if header-tdid = id_def. "def part contains transtat info, use it
    transtat = header-tdtranstat.
*     no message "langvec ignored", message 054 is unused now
  endif.
  if transtat = translation_not_wanted. "no translation
*   no action, always import
  else.                                 "translations wanted
    if langvec na olang.          "DEF not in langvec
*     language vector prohibits OLANG export/import
      perform newprot using nlp_war nlp_lv2 nlp_mid '082'
                            olang space space space.
      rc = 8. exit.
    endif.
    if langvec na header-tdspras. "TXT not in langvec
*     language vector prohibits translation export/import
      perform newprot using nlp_war nlp_lv3 nlp_mid '083'
                            header-tdspras space space space.
      rc = 4. exit.
    endif.
  endif.
endif.
endform.

* IMPORT a style or layout set
* OBJECT_TYPE is FORM/FORT or STYL/STYT
form import_formstyl using value(object_type) value(language_only).
data: rc like sy-subrc,
      olang like thead-tdospras,
      transtat like thead-tdtranstat.

perform get_language_vector using language_vector.
if sy-subrc <> 0. "exit if language vector cannot be read
  nothing = true. subrc = 4. exit.
endif.
* invalidate HEADER,LINES
clear header. refresh lines.
clear olang.
transtat = translation_wanted. "default
perform import_record.
while end_of_objdata = false and subrc = 0.
  case record-command.
*   header data
    when 'HEAD'.
      refresh lines.
      header = record-data.
      if header-tdname(16) ne header-tdform.
        if cl_abap_char_utilities=>charsize > 1.
          while header-tdform ne header-tdname(16).
            shift header+85 right.
          endwhile.
          condense header-tdtitle.
        else.
          while header-tdform ne header-tdname(16).
            shift header+135 left.
          endwhile.
        endif.
      endif.
*   lines data
    when 'LINE'.
      lines = record-data. append lines.
*   end of header & lines
    when 'END'.
*     check if LANGVEC allows import
      perform check_formstyl_for_import using header
                                              language_only
                                              olang
                                              language_vector
                                              transtat
                                              rc.
      case rc.
        when 0. "o.k. to import
          perform save_text using rc.
* TODO: COMMIT WORK.            BV
*          add 1 to commit_counter.
*          if commit_counter > c_commit_max_records.
*            clear commit_counter.
*            commit work.
*          endif.
          case rc.
            when 0. "SAVE_TEXT o.k.
            when 2. "BAD_LANGUAGE
              if header-tdspras = header-tdospras.
                import_ok = false. "suppress 'successful import' message
              endif.
            when others. "other error
              import_ok = false. "should not happen
          endcase.
        when 4. "TXT not in langvec -> cannot import language
*         no action, only info
        when 8. "DEF not in langvec -> cannot import complete object
          import_ok = false.
          nothing = true. "no final message on warnings/errors/success
      endcase.
*     invalidate HEADER,LINES
      clear header. refresh lines.
*   delete command FORM
    when func_del_form.
      if language_only = false. "complete import
        perform delete_object using object_form record-data(16).
      endif.
*   delete command STYLE
    when func_del_styl.
      if language_only = false. "complete import
        perform delete_object using object_style record-data(8).
      endif.
*   activate command
    when func_activate.
      if language_only = false. "complete import
        if import_ok = true.    "DEF was imported
          perform activate_object.
        endif.
      endif.
*   set original language command
    when func_olanguage.
      olang = record-data(1).
      if language_only = false. "complete import
        perform set_olanguage using object_type olang language_vector.
      endif.
*   nothing data (dummy export)
    when func_nothing.
*     nothing was imported
      perform newprot using nlp_war nlp_lv3 nlp_mid '069'
                      space space space space.
      nothing = true.
*   unknown command
    when others.
*     format ... is unknown and will be ignored
      perform newprot using nlp_war nlp_lv3 nlp_mid '070'
                      record-command space space space.
  endcase.
  perform import_record.
endwhile.
endform.

* import a TEXT
form import_text.
data: rc like sy-subrc.

perform get_language_vector using language_vector.
if sy-subrc <> 0. "exit if language vector cannot be read
  nothing = true. subrc = 4. exit.
endif.
* invalidate HEADER,LINES
clear header.
refresh lines.
perform import_record.
while end_of_objdata = false and subrc = 0.
  case record-command.
    when 'HEAD'.
      header = record-data. refresh lines.
    when 'LINE'.
      lines = record-data. append lines.
    when 'END'.
      if language_vector ca header-tdspras.
        perform save_text using rc.
        case rc.
          when 0. "SAVE_TEXT o.k.
          when 2. "BAD_LANGUAGE
            import_ok = false. "suppress 'successful import' message
          when others. "other ERROR
            import_ok = false. "should not happen
        endcase.
      else.
*       language vector prohibits OLANG export/import
        perform newprot using nlp_war nlp_lv2 nlp_mid '082'
                              header-tdspras space space space.
        import_ok = false. "suppress 'successful import' message
      endif.
*     invalidate HEADER,LINES
      clear header. refresh lines.
    when func_nothing.
*     nothing was imported
      perform newprot using nlp_war nlp_lv3 nlp_mid '069'
                      space space space space.
      nothing = true.
    when others.
*     format ... is unknown and will be ignored
      perform newprot using nlp_war nlp_lv3 nlp_mid '070'
                      record-command space space space.
  endcase.
  perform import_record.
endwhile.
endform.

* import a PRINTER definition
form import_prin.
constants: allclients like sy-mandt value '*  '.
statics: version like tsp0a-pvers,
         printer like tsp0a-patype,
         numlines like sy-tabix.
data wa_tsp0a type tsp0a.
data msg(80) type c.
data title(80) type c.
data answer_yes type c.

* refresh buffer tables
refresh: buf_t022d, buf_tsp06, buf_tsp06a,
         buf_tfo03, buf_tfo04, buf_tfo06,
         buf_tsp1d, buf_tsp06pot.
perform import_record.
while end_of_objdata = false and subrc = 0.
  case record-command.
*   print controls T022D (REPLACE ALL)
    when '022D'.
      buf_t022d = record-data.
      append buf_t022d.
*   font families  TFO01
    when 'FO01'. "ignore, no import
*   printer fonts  TFO03 (REPLACE ALL)
    when 'FO03'.
      tfo03_line = record-data.
      buf_tfo03-tdprinter = tfo03_line-all(8).
      buf_tfo03-tdfamily = tfo03_line-all+8(8).
      buf_tfo03-tdfontsize = tfo03_line-all+16(3).
      buf_tfo03-tdbold = tfo03_line-all+19(1).
      buf_tfo03-tditalic = tfo03_line-all+20(1).
      buf_tfo03-tdpprintid = tfo03_line-all+24(30).
      buf_tfo03-tdlprintid = tfo03_line-all+54(30).
      buf_tfo03-tdafmflag = tfo03_line-all+84(1).
      buf_tfo03-tdcpselect = tfo03_line-all+85(1).
      buf_tfo03-tdcpi = tfo03_line-cpi.
      append buf_tfo03.
*   printer font metrics TFO04 (REPLACE ALL)
    when 'FO04'.
      buf_tfo04 = record-data.
      append buf_tfo04.
*   system barcodes TFO05
    when 'FO05'. "ignore, no import
*   printer barcodes TFO06 (REPLACE ALL)
    when 'FO06'.
      buf_tfo06 = record-data.
      append buf_tfo06.
*   device format summary TSP06A (REPLACE ALL)
    when 'SP6A'.
*      BUF_TSP06A = RECORD-DATA.
      clear buf_tsp06a.
      tsp06a_line = record-data.
      move-corresponding tsp06a_line to buf_tsp06a.
      append buf_tsp06a.
    when 'SP6X'.
      tsp06ax_line = record-data.
      describe table buf_tsp06a lines numlines.
      read table buf_tsp06a with key
        ptype = tsp06ax_line-ptype
        paper = tsp06ax_line-paper.
      if sy-subrc = 0.
        buf_tsp06a-lstdriver = tsp06ax_line-lstdriver.
        buf_tsp06a-lstsubtype = tsp06ax_line-lstsubtype.
        buf_tsp06a-driverinfo = tsp06ax_line-driverinfo.
        buf_tsp06a-spacemode = tsp06ax_line-spacemode.
        buf_tsp06a-charwidth = tsp06ax_line-charwidth.
        buf_tsp06a-fontsize = tsp06ax_line-fontsize.
        modify buf_tsp06a index sy-tabix.
      endif.
*   device format details TSP06 (REPLACE ALL)
    when 'SP06'.
      tsp06_line = record-data.
*      BUF_TSP06 = TSP06_LINE-ALL.
      clear buf_tsp06.
      buf_tsp06-pdptype   = tsp06_line-all(8).
      buf_tsp06-pdpaper   = tsp06_line-all+8(16).
      buf_tsp06-pdname    = tsp06_line-all+24(8).
      buf_tsp06-pddata    = tsp06_line-all+34(72).
      buf_tsp06-pdlfdnr   = tsp06_line-pdlfdnr.
      buf_tsp06-pddatalen = tsp06_line-pddatalen.
      append buf_tsp06.
*   spooler formats TSP1D (insert NEW only, do not change existing)
    when 'SP1D'.
*      BUF_TSP1D = RECORD-DATA.
      clear buf_tsp1d.
      tsp1d_line = record-data.
      move-corresponding tsp1d_line to buf_tsp1d.
      append buf_tsp1d.
    when 'SP1X'.
      tsp1dx_line = record-data.
      read table buf_tsp1d with key
        papart = tsp1dx_line-papart.
      if sy-subrc = 0.
        buf_tsp1d-listarea  = tsp1dx_line-listarea.
        buf_tsp1d-mrg_top   = tsp1dx_line-mrg_top.
        buf_tsp1d-mrg_left  = tsp1dx_line-mrg_left.
        buf_tsp1d-mrg_bot   = tsp1dx_line-mrg_bot.
        buf_tsp1d-mrg_right = tsp1dx_line-mrg_right.
        modify buf_tsp1d index numlines.
      endif.
*   printer type TSP0A (REPLACE 1 ENTRY)
    when 'SP0A'.
      tsp0a = record-data.
      printer = tsp0a-patype.
*     compare  devtype versions
      select single * from tsp0a into wa_tsp0a where patype = printer.
      if sy-subrc = 0 and sy-batch <> 'X'.
*       check for newer version
        title = 'Warnung vor Überschreiben von Gerätetyp &1'(030).
        replace '&1' with printer into title.
        if wa_tsp0a-pvers > tsp0a-pvers.
*         warning popup
          msg = 'Version im System (&1) neuer als in Datei (&2)'(031).
          replace '&1' with wa_tsp0a-pvers into msg.
          replace '&2' with tsp0a-pvers into msg.
          perform popup_to_confirm_step_defno(SAPMSSCO) using
            title msg
            'Trotzdem fortfahren?'(032)
            answer_yes.
          if answer_yes = ' '.
            stop.
          endif.
        elseif wa_tsp0a-chgtstmp1 > tsp0a-chgtstmp1.
          msg = 'Zeitstempel im System (&1) neuer als in Datei (&2)'(033).
          replace '&1' with wa_tsp0a-chgtstmp1(8) into msg.
          replace '&2' with tsp0a-chgtstmp1(8) into msg.
          perform popup_to_confirm_step_defno(SAPMSSCO) using
            title msg
            'Trotzdem fortfahren?'(032)
            answer_yes.
          if answer_yes = ' '.
            stop.
          endif.
        endif.
      endif.
*     the printer, font, style, form load is deleted in FORM IMPORT
      insert tsp0a. if sy-subrc > 0. update tsp0a. endif.
*   SAPscript drivers (INSERT 1 ENTRY)
    when 'SP09'.
      tsp09 = record-data.
      insert tsp09.
*     do nothing, if driver exists already, no update!
    when 'SP6T'.
*     POSS option texts
      tsp06pot_line = record-data.
      perform opttxt_import using tsp06pot_line buf_tsp06pot.
      if sy-subrc = 0.
        append buf_tsp06pot.
      endif.
    when func_del_paper.
      paper = record-data.
*     DELETE FROM TSP06 WHERE PDPTYPE = PAPER-PDPTYPE        "B20K064895
*                       AND   PDPAPER = PAPER-PDPAPER.
    when func_del_prin.                                      "B20K057010
*     delete printer, tables will be deleted in UPDATE_PRIN_TABLES
      printer = name(8).                                     "
      delete from tsp0a where patype = printer.              "
      perform newprot using nlp_war nlp_lv2 nlp_mid '097'    "
                      object name(8) space space.            "
    when func_nothing.
*     nothing was imported
      perform newprot using nlp_war nlp_lv3 nlp_mid '069'
                      space space space space.
      nothing = true.
    when others.
*     format ... is unknown and will be ignored
      perform newprot using nlp_war nlp_lv3 nlp_mid '070'
                      record-command space space space.
  endcase.
  perform import_record.
endwhile.
* update database tables from buffers
perform update_prin_tables_from_buffer using printer.
* delete complete load
* this will take longer if a single object is imported, but saves a
* lot of time if several objects are imported in a put
call function 'SAPSCRIPT_DELETE_LOAD'
  exporting all    = 'X'
            client = allclients
            delete = 'X'
            write  = ' '.
endform.

* insert entries from PRIN import buffered in BUF_... tables into DB
form update_prin_tables_from_buffer using value(printer).
statics: num_device_formats type i,
         tsp06_timestamp(6).

* formats
loop at buf_tsp1d.                                           "B20K086195
  move-corresponding buf_tsp1d to tsp1d.
  insert tsp1d.        "only insert new items, no update of existing
endloop.
* print controls
delete from t022d where typ = printer.
loop at buf_t022d. "avoid runtime-error from ARRAY INSERT
  buf_t022d-typ = printer. modify buf_t022d.
endloop.
insert t022d from table buf_t022d.
* printer fonts
delete from tfo03 where tdprinter = printer.
loop at buf_tfo03. "avoid runtime-error from ARRAY INSERT
  buf_tfo03-tdprinter = printer. modify buf_tfo03.
endloop.
insert tfo03 from table buf_tfo03.
* printer font metrics
delete from tfo04 where tdprinter = printer.
loop at buf_tfo04. "avoid runtime-error from ARRAY INSERT
  buf_tfo04-tdprinter = printer. modify buf_tfo04.
endloop.
insert tfo04 from table buf_tfo04.
* printer barcodes
delete from tfo06 where tdprinter = printer.
loop at buf_tfo06. "avoid runtime-error from ARRAY INSERT
  buf_tfo06-tdprinter = printer. modify buf_tfo06.
endloop.
insert tfo06 from table buf_tfo06.
* device format summary - new in 4.0A
delete from tsp06a where ptype = printer.                    "B20K061850
loop at buf_tsp06a. "avoid runtime-error from ARRAY INSERT
  buf_tsp06a-ptype = printer. modify buf_tsp06a.
endloop.
insert tsp06a from table buf_tsp06a.
describe table buf_tsp06a lines num_device_formats.
* device format details
clear tsp06a. "prepare TSP06A workarea                       "B20K061850
delete from tsp06 where pdptype = printer.
loop at buf_tsp06. "avoid runtime-error from ARRAY INSERT
  buf_tsp06-pdptype = printer. modify buf_tsp06.
  if num_device_formats = 0. "no TSP06A info in import data
*   collect existing device formats from TSP06 for TSP06A
    if buf_tsp06-pdname = 'CONTROL'.                         "B20K061850
      if     buf_tsp06-pdlfdnr = 1.
        tsp06a-ptype = printer.
        tsp06a-paper = buf_tsp06-pdpaper.
        tsp06a-version = buf_tsp06-pddata.
        insert tsp06a.
      elseif buf_tsp06-pdlfdnr = 2.
        tsp06a-ptype = printer.
        tsp06a-paper = buf_tsp06-pdpaper.
*       user info 1
        tsp06a-chgname1 = buf_tsp06-pddata+0(12).
        tsp06_timestamp = buf_tsp06-pddata+12(6).
        if tsp06_timestamp <> space.
          tsp06a-chgtstmp1+2(6) = tsp06_timestamp.
          perform extend_year changing tsp06a-chgtstmp1.
        else.
          clear tsp06a-chgtstmp1.
        endif.
        tsp06a-chgsapsys1 = buf_tsp06-pddata+18(4).
        tsp06a-chgsaprel1 = buf_tsp06-pddata+22(4).
*       user info 2
        tsp06a-chgname2 = buf_tsp06-pddata+26(12).
        tsp06_timestamp = buf_tsp06-pddata+38(6).
        if tsp06_timestamp <> space.
          tsp06a-chgtstmp2+2(6) = tsp06_timestamp.
          perform extend_year changing tsp06a-chgtstmp2.
        else.
          clear tsp06a-chgtstmp2.
        endif.
        tsp06a-chgsapsys2 = buf_tsp06-pddata+44(4).
        tsp06a-chgsaprel2 = buf_tsp06-pddata+48(4).
*       user info 3
        tsp06a-chgname3 = buf_tsp06-pddata+52(12).
        tsp06_timestamp = buf_tsp06-pddata+64(6).
        if tsp06_timestamp <> space.
          tsp06a-chgtstmp3+2(6) = tsp06_timestamp.
          perform extend_year changing tsp06a-chgtstmp3.
        else.
          clear tsp06a-chgtstmp3.
        endif.
        tsp06a-chgsapsys3 = ' '.
        insert tsp06a. if sy-subrc <> 0. update tsp06a. endif.
      elseif buf_tsp06-pdlfdnr = 3.
        tsp06a-ptype = printer.
        tsp06a-paper = buf_tsp06-pdpaper.
        tsp06a-postflag = buf_tsp06-pddata+0(1).
        tsp06a-convflag = buf_tsp06-pddata+1(1).
        if tsp06a-convflag = 'X'.
          tsp06a-convcodep = buf_tsp06-pddata+2(4).
        else.
          clear tsp06a-convcodep.
        endif.
        insert tsp06a. if sy-subrc <> 0. update tsp06a. endif.
      elseif buf_tsp06-pdlfdnr = 4. "4.0A only
        tsp06a-base = buf_tsp06-pddata.
        insert tsp06a. if sy-subrc <> 0. update tsp06a. endif.
      endif.
    endif.
  endif.
endloop.
insert tsp06 from table buf_tsp06.
* POSS option texts
delete from tsp06pot where devtype = printer
                     and   lang    = 'E'.
loop at buf_tsp06pot where lang = 'E'.
  buf_tsp06pot-devtype = printer.
  insert tsp06pot from buf_tsp06pot.
endloop.
* release memory for table buffers
free: buf_t022d, buf_tsp06, buf_tsp06a,
      buf_tfo03, buf_tfo04, buf_tfo06.
endform.

* convert 2-digit year to 4-digit for TSP06A timestamps
form extend_year changing tstmp.                             "B20K061850
statics: y type i.

if not tstmp is initial.
  y = tstmp+2(2).
  if y < 50.
    tstmp(2) = '20'.
  else.
    tstmp(2) = '19'.
  endif.
  tstmp+8(6) = '000000'.
endif.
endform.

* call function SAVE_TEXT to save a single HEADER + LINES
* RC = 0 if o.k.
* RC > 0 if error, and message number+parameters are returned in ITCRS
* IF RC = 2 then language does not exist in target system
form save_text using rc.
if header-tdobject = object_style or
   header-tdobject = object_form. "this is a style/layout set
* no action
else.                             "this is a text
  if header-tdtexttype <> space.
*   convert LINES data from ascii to binary
    perform convert_lines_ascii_2_bin.
  endif.
endif.
* call SAVE_TEXT
call function 'SAVE_TEXT'
  exporting header = header
            savemode_direct = 'X'
  tables    lines = lines
  exceptions id       = 1
             language = 2
             name     = 3
             object   = 4
             others   = 5.
rc = sy-subrc.
case rc.
  when 0.
*   SAVE_TEXT o.k.
    if header-tdobject = object_form or header-tdobject = object_style.
*     style/layout set
      case header-tdid.
        when id_def.
*         definition ... was imported
          perform newprot using nlp_inf nlp_lv3 nlp_mid '066'
                              header-tdspras space space space.
        when id_txt.
          if header-tdspras = header-tdospras.
*           original language ... was imported
            perform newprot using nlp_inf nlp_lv3 nlp_mid '065'
                          header-tdspras space space space.
          else.
*           language ... was imported
            perform newprot using nlp_inf nlp_lv3 nlp_mid '067'
                                  header-tdspras space space space.
          endif.
      endcase.
    else.
*     normal text
*     TEXT object ..... was imported
      perform newprot using nlp_inf nlp_lv3 nlp_mid '087'
                            header-tdobject  header-tdname
                            header-tdid      header-tdspras.
    endif.
  when 2.
*   BAD_LANGUAGE exception
    if header-tdobject = object_form or header-tdobject = object_style.
*     style/layout set: WARNING
      if header-tdspras = header-tdospras.
*       object not imported since language & does not exist
         perform newprot using nlp_war nlp_lv2 nlp_mid '095'
                         header-tdspras space space space.
      else.
*       language & not imported since language does not exist
        perform newprot using nlp_war nlp_lv3 nlp_mid '096'
                        header-tdspras space space space.
      endif.
    else.
*     normal text: WARNING
*     object not imported since language & does not exist
      perform newprot using nlp_war nlp_lv3 nlp_mid '095'
                      header-tdspras space space space.
    endif.
  when others.
*   get sapscript's error message number+parameters into ITCRS
    call function 'SAPSCRIPT_MESSAGE_GET_NO'
      importing no = itcrs-msgno
                v1 = itcrs-msgv1
                v2 = itcrs-msgv2
                v3 = itcrs-msgv3
                v4 = itcrs-msgv4.
*   SAVE_TEXT(...) failed
    perform newprot using nlp_err nlp_lv2 nlp_mid '068'
                        header-tdobject   header-tdname
                        header-tdid       header-tdspras.
*   pass on error message from SAVE_TEXT
    perform newprot using nlp_inf nlp_lv2 nlp_mid itcrs-msgno
                          itcrs-msgv1 itcrs-msgv2
                          itcrs-msgv3 itcrs-msgv4.
endcase.
endform.

* delete a style/layout set
form delete_object using value(object) value(name).
data: formname like itcta-tdform,
      stylename like itcda-tdstyle,
      found like boolean.

found = false.
case object.
  when object_form.
    formname = name.
    call function 'DELETE_FORM'
      exporting form     = formname
                language = '*'
      importing found    = found.
  when object_style.
    stylename = name.
    call function 'DELETE_STYLE'
      exporting style    = stylename
                language = '*'
      importing  found   = found.
endcase.
if found = true.
* object ... ... was deleted
  perform newprot using nlp_inf nlp_lv3 nlp_mid '097'
                  object name space space.
endif.
endform.

* set custom language vector
form set_custom_language_vector using value(langvec).
statics: len like sy-fdpos.

if langvec = space.
  custom_language_vector_valid = space. "invalidate custom langvec
else.
  condense langvec no-gaps. "no blanks in langvec
  custom_language_vector = langvec.
  custom_language_vector_valid = custom_language_vector_magic.
  len = strlen( custom_language_vector ).
  if len = 1.
    set locale language custom_language_vector(1). "B20K049191
  endif.
endif.
endform.

* for FORM/STYL transport: get current language vector to be
* used for export/import in LANGVEC
* returns sy-subrc = 0 if o.k., else 1
form get_language_vector changing langvec.
statics: masterlang_only like boolean.

* do we have a custom langvec that overrides system vector?
if custom_language_vector_valid = custom_language_vector_magic.
  langvec = custom_language_vector.
  sy-subrc = 0.
else.
  call function 'SYSTEM_INSTALLED_LANGUAGES'
     importing
          languages       = langvec. "#EC *
endif.
if sy-subrc = 0.
* used language vector: ...
  perform newprot using nlp_inf nlp_lv3 nlp_mid '080'
                  langvec space space space.
  sy-subrc = 0.
else.
  sy-subrc = 1.
endif.
endform.

form set_message_language using value(lang).                 "B20K065126
if message_language = space.
  message_language = lang.
endif.
endform.

form get_message_language using lang.
lang = message_language.
if lang = space.
  lang = sy-langu.
endif.
endform.

*********************
* Basic Procedures
*********************

* set binary file format flag
form set_binary_file_format using value(binfile_flag).
statics: appl_syscp like tcp00-cpcodepage.

call function 'SYSTEM_CODEPAGE'
     importing
          codepage                 = appl_syscp
          current_dynamic_codepage = system_codepage.
if system_codepage(2) = '41'.
  r3_internal_charset = c_charset_unicode.
  record_numbytes = 2 * c_record_numchars.
  system_codepage = '4110'.
else.
  record_numbytes = c_record_numchars.
  if system_codepage(1) = '0'.
    r3_internal_charset = c_charset_ebcdic.
  else.
    r3_internal_charset = c_charset_ascii.
  endif.
endif.
if binfile_flag = true.
  if appl_syscp <> system_codepage.
    perform newprot using nlp_inf nlp_lv3 nlp_mid '048'
                    system_codepage appl_syscp space space.
  endif.
  binfile_codepage = '0000'.
  binary_file_format_valid = binary_file_format_magic.
else.
  binary_file_format_valid = space.
endif.
endform.

* set sy-subrc = 0 if BINARY format wanted
form get_binary_file_format_flag.
if binary_file_format_valid = binary_file_format_magic.
  sy-subrc = 0.
else.
  sy-subrc = 1.
endif.
endform.

form set_file_source using value(localfile). "B20K053928
if localfile = true.
  file_source_local = file_source_local_magic.
else.
  file_source_local = space.
endif.
endform.

* set sy-subrc = 0 if file source is LOCAL (GUI)
form get_file_source.                       "B20K053928
if file_source_local = file_source_local_magic.
  sy-subrc = 0.
else.
  sy-subrc = 1.
endif.
endform.

* set MASTERLANGUAGE_ONLY mode (3.1G export)
form set_masterlang_only_flag using value(flag). "B20K057147
if flag = true.
  masterlang_only_flag = masterlang_only_magic.
else.
  masterlang_only_flag = space.
endif.
endform.

* set sy-subrc = 0 if masterlang_only mode is active
form get_masterlang_only_flag.                   "B20K057147
if masterlang_only_flag = masterlang_only_magic.
  sy-subrc = 0.
else.
  sy-subrc = 1.
endif.
endform.

form export_ta.
record-typ = 'T'.
record-command = 'COMM'.
record-data   = trkorr.
perform export_clipboard using false. "not end_of_obj
endform.

form export_sapscript using object obj_name.
  record-typ = 'S'.
  record-command = object.
  record-data   = obj_name.
  perform export_clipboard using false. "not end_of_obj
endform.

form export_header.
  record-typ = 'H'.
  record-command = object.
  record-data = name.
  if clipboard = true.
    perform export_clipboard using false. "not end_of_obj
*  else.
*    perform expdata(rdddic00) using record.
  endif.
endform.

* export dummy entry
form export_nothing.
nothing = true.
* don't send a message it's done by caller of EXPORT_NOTHING
perform export_header.
perform export_function using func_nothing.
perform export_end.
endform.

* export function like activate...
form export_function using function.
record-typ = space.
record-command = function.
record-data = status_sap. "QVN always use SAP, never CUS
if clipboard = true.
  perform export_clipboard using false. "not end_of_obj
*else.
*  perform expdata(rdddic00) using record.
endif.
endform.

form export_end.
  clear record.
  record-typ = 'E'.
  if clipboard = true.
    perform export_clipboard using true. "end_of_obj
*  else.
*    perform expdata(rdddic00) using record.
  endif.
endform.

form export_data using command data.
  clear record.
  record-command = command.
  record-data = data.
  if clipboard = true.
    perform export_clipboard using false. "not end_of_obj
*  else.
*    perform expdata(rdddic00) using record.
  endif.
endform.

* read header line of TA with TCOMM<transportname>
* sets RC to 0 if o.k  else > 0
* only used when reading dataset with RSTXR3TR
form import_ta using rc.
rc = 0.
perform import_clipboard.
if subrc ne 0.
* illegal end of transportfile
  perform newprot using nlp_err nlp_lv2 nlp_mid '078'
                  record-typ record-command space space.
  rc = 1.
  exit.
endif.
if record-typ ne 'T' or record-command ne 'COMM'.
* transport header was not found
  perform newprot using nlp_err nlp_lv2 nlp_mid '075'
                  record-typ record-command space space.
  rc = 1.
  exit.
endif.
if record-data ne trkorr.
* transport number ... and content of file ... differ
  perform newprot using nlp_err nlp_lv2 nlp_mid '076'
                  trkorr record-data space space.
  rc = 1.
  exit.
endif.
endform.

* read header line of TA with S<objectname>
* sets RC to 0 if o.k  else > 0
* only used when reading DATASET with RSTXSCRP
form import_sapscript using object obj_name rc.
statics: typcmd(5),
         sobj(10).

rc = 0.
perform import_clipboard.
if subrc ne 0.
* illegal end of transportfile
  perform newprot using nlp_err nlp_lv2 nlp_mid '078'
                  record-typ record-command space space.
  rc = 1.
  exit.
endif.
if record-typ ne 'S' or record-command ne object.
* illegal header: ... instead of ...
  typcmd = record-typ. typcmd+1 = record-command.
  sobj = 'S'.          sobj+1 = object.
  perform newprot using nlp_err nlp_lv2 nlp_mid '085'
                  typcmd sobj space space.
  rc = 1.
  exit.
endif.
if record-data ne obj_name.
* transport object ... and content of file ... differ
  perform newprot using nlp_err nlp_lv2 nlp_mid '092'
                  obj_name record-data space space.
  rc = 1.
  exit.
endif.
endform.

* read header entry in file, i.e. HTEXT, HPRIN, HFORM, HSTYL
*
* sets globals OBJECT, NAME
*
form import_header.
* invalidate transport object, we are looking for a new one
clear object. clear name.
if clipboard = true.
  perform import_clipboard.
else.
  perform call_impdata using record subrc.
endif.
if subrc ne 0.
* ???
* illegal end of transportfile
* PERFORM NEWPROT USING NLP_ERR NLP_LV2 NLP_MID '078'
*                 SPACE SPACE SPACE SPACE.
  exit.
endif.
if record-typ ne 'H'.
* SAPscript transport header (HTEXT,HSTYL, ...) was not found
  perform newprot using nlp_err nlp_lv2 nlp_mid '085'
                  record-typ 'H' space space.
  exit.
endif.
object = record-command.
name = record-data.
case object.
  when 'PRIN'.
  when 'STYL'.
  when 'STYT'. "style languages only
  when 'FORM'.
  when 'FORT'. "form languages only
  when 'TEXT'.
  when others.
*   the transport object ... is invalid
    perform newprot using nlp_err nlp_lv2 nlp_mid '077'
                    object space space space.
    exit.
endcase.
* object ... ... is presently at work
perform newprot using nlp_inf nlp_lv2 nlp_mid '093'
                    object name space space.
endform.

* read a single line from DATASET or TA
* sets SUBRC...
* if the end marker "E" is read, the END_OF_OBJDATA flag is set to TRUE
form import_record.
data: rc like sy-subrc.

check subrc = 0 and end_of_objdata = false.
if clipboard = true.
  perform import_clipboard.
else.
  perform call_impdata using record subrc.
endif.
if subrc ne 0.
* ???
* illegal end of transportfile
* PERFORM NEWPROT USING NLP_ERR NLP_LV2 NLP_MID '078' "QVN
*                 SPACE SPACE SPACE SPACE.            "QVN
  exit.
endif.
if record-typ = 'E'.
* we have read our own "end marker" in the transport file
  end_of_objdata = true.
* if this is the "official" transporter, make sure that this is the
* end of the data
  if clipboard <> true.
    perform call_impdata using record rc.
    if rc = 0. "there is some data left...
*     end of transportfile was expected
      perform newprot using nlp_war nlp_lv2 nlp_mid '079' "QVN
                      space space space space.            "QVN
    else.
*     there is no data left, that's what we expected
    endif.
  else.
*   no action necessary for clipboard import
  endif.
endif.
subrc = 0.
endform.

* read all remaining records from IMPORT file. We might have stopped
* reading in the middle of an object due to an error
* this is done ONLY if we are called from the "official" transporter
* i.e. not from RSTXR3TR or RSTXSCRP.
* Careful: We MUST NOT read again if the IMPDATA routine has already
*          delivered us a RC <> 0 because then we would overread
*          the next object record!
*          To prevent this, the END_OF_DATA flag is used
form skip_unread_records.
data: count like integer.

if clipboard <> true.
  count = 0.
  while end_of_data = false and count < 9000.
*   PERFORM IMPDATA(RDDDIC10) USING RECORD RC.
    perform call_impdata using record rc. "QVN B20K005546
    add 1 to count.
  endwhile.
else.
* no action necessary for CLIPBOARD import
endif.
endform.

* interface to transport software: call form IMPDATA to get next record
* RC returns IMPDATA's return code
* ATTENTION: structure RECORD must be passed as REC parameter!
form call_impdata using rec rc.
perform impdata(rdddic10) using rec rc.
if rc <> 0.
  end_of_data = true.
endif.
endform.

* export a single text (=HEADER from STXH and LINES from STXL)
* it is used for TEXT, STYL and FORM objects
* the flag LANGUAGE_ONLY controls the state how FORM/STYL is
* exported/imported:
* LANGUAGE_ONLY = FALSE -> object is exported/imported as MOD
*               = TRUE  -> object is exported/imported as ACT
form export_txt using value(object) value(id)
                      value(name)   value(language)
                      value(language_only) rc.
call function 'READ_TEXT'
     exporting object = object
               id   = id
               name = name
               language = language
     importing header = header
     tables    lines = lines
     exceptions id       = 1
                language = 2
                name     = 3
                not_found = 4
                object    = 5
                reference_check = 6
                others = 7.
if sy-subrc = 0.
* READ_TEXT o.k.
  if header-tdobject = object_style or
     header-tdobject = object_form. "this is a style/layout set
    if language_only = true.
      header-tdname+16(3) = space.      "export and import ACT state
    else.
      header-tdname+16(3) = status_sap. "export and import MOD state
    endif.
  else.                             "this is a text
    if header-tdtexttype <> space.
*     convert LINES data from binary to ascii
      perform convert_lines_bin_2_ascii.
    endif.
  endif.
  perform export_data using 'HEAD' header.
  loop at lines.
    perform export_data using 'LINE' lines.
  endloop.
  perform export_data using 'END ' space.
  rc = 0.
else.
* READ_TEXT(...) failed
  perform newprot using nlp_err nlp_lv2 nlp_mid '090'
                  object name id language.
* get sapscript's error message number and pass it on
  call function 'SAPSCRIPT_MESSAGE_GET_NO'
    importing no = itcrs-msgno
              v1 = itcrs-msgv1
              v2 = itcrs-msgv2
              v3 = itcrs-msgv3
              v4 = itcrs-msgv4.
  perform newprot using nlp_inf nlp_lv2 nlp_mid itcrs-msgno
                  itcrs-msgv1 itcrs-msgv2
                  itcrs-msgv3 itcrs-msgv4.
  rc = 1.
endif.
endform.

* convert binary text in LINES table to ASCII format
form convert_lines_bin_2_ascii.
data begin of l occurs 100.
  include structure tline.
data end   of l.
field-symbols <p>.

refresh l.
loop at lines.
* convert format
  clear l.
  perform conv_bin_2_hex using lines-tdformat(1) l-tdformat.
  assign l-tdline(2) to <p>.
  perform conv_bin_2_hex using lines-tdformat+1(1) <p>.
* convert line(1..65)
  do 65 times.
    assign <p>+2(2) to <p>.
    perform conv_bin_2_hex using lines-tdline(1) <p>.
    shift lines-tdline.
  enddo.
  append l. clear l.
* convert line(66)
  perform conv_bin_2_hex using lines-tdline(1) l-tdformat.
  shift lines-tdline.
* convert line(67)
  assign l-tdline(2) to <p>.
  perform conv_bin_2_hex using lines-tdline(1) <p>.
  shift lines-tdline.
* convert line(68..132)
  do 65 times.
    assign <p>+2(2) to <p>.
    perform conv_bin_2_hex using lines-tdline(1) <p>.
    shift lines-tdline.
  enddo.
  append l.
endloop.
refresh lines.
loop at l.
  lines = l. append lines.
endloop.
free l.
endform.

* convert ASCII text in LINES table to binary
form convert_lines_ascii_2_bin.
data begin of l occurs 100.
  include structure tline.
data end   of l.
field-symbols <p>.
data l_offs type i.

refresh l.
l_offs = 0.
loop at lines.
  if l_offs = 0. "begin of a L-line
    clear l.
*   convert l-format
    perform conv_hex_2_bin using lines-tdformat l-tdformat(1).
    perform conv_hex_2_bin using lines-tdline(2) l-tdformat+1(1).
    shift lines-tdline by 2 places.
*   convert l-line(1..65)
    assign l-tdline(1) to <p>.
    do 65 times.
      perform conv_hex_2_bin using lines-tdline(2) <p>.
      shift lines-tdline by 2 places.
      assign <p>+1(1) to <p>.
    enddo.
    l_offs = 65.
  else.           "middle of a l-line
    assign l-tdline+65(1) to <p>.
*   convert LINES-TDFORMAT
    perform conv_hex_2_bin using lines-tdformat <p>.
*   convert LINES-TDLINE
    do 66 times.
      assign <p>+1(1) to <p>.
      perform conv_hex_2_bin using lines-tdline(2) <p>.
      shift lines-tdline by 2 places.
    enddo.
    append l.
    l_offs = 0.
  endif.
endloop.
refresh lines.
loop at l.
  lines = l. append lines.
endloop.
free l.
endform.

* set original language of style/layout set in target system
* OBJECT=FORM/STYL
form set_olanguage using value(object) value(olang) value(langvec).
data: tdname like thead-tdname,
      tdobject like thead-tdobject,
      tdolang like thead-tdspras.

case object.
  when object_style. tdobject = object_style. tdname = name(8).
  when object_form.  tdobject = object_form.  tdname = name(16).
  when others. exit.
endcase.
tdolang = olang.
if langvec ca tdolang. "if OLANG in langvec...
  call function 'SAPSCRIPT_CHANGE_OLANGUAGE'
     exporting forced = true
               object = tdobject
               name   = tdname
               olanguage = tdolang
     exceptions others = 1.
  if sy-subrc = 0.
*   original language was changed to ...
    perform newprot using nlp_inf nlp_lv3 nlp_mid '053'
                    tdolang space space space.
  endif.
endif.
endform.

* activate STYLE,FORM
form activate_object.
data: style like itcda-tdstyle,
      form like itcta-tdform.

activate_object = true.
case object.
  when 'STYL'. style = name(8).
    call function 'ACTIVATE_STYLE'
         exporting style  = style
                   status = status_sap
         importing result = itcrs.
  when 'FORM'. form = name(16).
    call function 'ACTIVATE_FORM'
         exporting form   = form
                   status = status_sap
         importing result = itcrs.
  when others. exit.
endcase.
* check result of activating object...
if itcrs-subrc = 0.
  activate_object_ok = true.
else.
* object could not be activated
  perform newprot using nlp_war nlp_lv3 nlp_mid '081'
                  space space space space.
* pass on error message from ITCRS structure, ID is 'TD'
  perform newprot using nlp_inf nlp_lv3 'TD' itcrs-msgno
                  itcrs-msgv1 itcrs-msgv2
                  itcrs-msgv3 itcrs-msgv4.
  activate_object_ok = false.
endif.
endform.

***************************************
* routines for interfacing correction system
***************************************

form get_ta.                                                 "B20K070087
call function 'TR_READ_COMM'
     exporting
          wi_trkorr             = trkorr
*         WI_DIALOG             = 'X'
*         WI_LANGU              = SY-LANGU
*         WI_SEL_E070           = ' '
          wi_sel_e071           = 'X'
*         WI_SEL_E071K          = ' '
*         IV_SEL_E071KF         = ' '
*         WI_SEL_E07T           = ' '
*         WI_SEL_E070C          = ' '
*         IV_SEL_E070M          = ' '
*    IMPORTING
*         WE_E070               =
*         WE_E07T               =
*         WE_E070C              =
*         ES_E070M              =
*         WE_E07T_DOESNT_EXIST  =
*         WE_E070C_DOESNT_EXIST =
*         EV_E070M_DOESNT_EXIST =
     tables
          wt_e071               = e071_tab
*         WT_E071K              =
*         ET_E071KF             =
     exceptions
          not_exist_e070        = 1
          no_authorization      = 2.
if sy-subrc <> 0.
  message a051 with trkorr.
endif.
endform.                                                     "B20K070087

***************************************
* routines for data conversion, codepage handling, UNICODE
***************************************

* convert 1-ascii digit to 1 hex byte,
* 0 -> $30
* 1 -> $31
* ...
* 9 -> $39
form convert_digit_to_hexbyte using value(digit) byte type x.
case digit.
  when '1'. byte = '31'.
  when '2'. byte = '32'.
  when '3'. byte = '33'.
  when '4'. byte = '34'.
  when '5'. byte = '35'.
  when '6'. byte = '36'.
  when '7'. byte = '37'.
  when '8'. byte = '38'.
  when '9'. byte = '39'.
  when others. byte = '30'.
endcase.
endform.

* convert 1 hex byte to 1 ascii digit
* $30 -> 0
* $31 -> 1
* ...
* $39 -> 9
form convert_hexbyte_to_digit using value(hexbyte) type x
                                    digit.
case hexbyte.
  when '31'. digit = '1'.
  when '32'. digit = '2'.
  when '33'. digit = '3'.
  when '34'. digit = '4'.
  when '35'. digit = '5'.
  when '36'. digit = '6'.
  when '37'. digit = '7'.
  when '38'. digit = '8'.
  when '39'. digit = '9'.
  when others. digit = '0'.
endcase.
endform.

* convert hex byte to 2 ascii characters (1st two chars of CHARFIELD)
form conv_bin_2_hex using value(bytechar) type c
                          charfield type c.
statics: x type x,
         c2(2) type c.
field-symbols <p>.

assign bytechar to <p> type 'X'.
x = <p>.
c2 = x.
charfield = c2.
endform.

* convert 2 ascii characters to single hex byte
form conv_hex_2_bin using value(charfield) type c
                          bytechar type c.
statics: x type x,
         c2(2) type c.
field-symbols <p>.

c2 = charfield.
case c2+1(1).
  when '1'. x = 1.
  when '2'. x = 2.
  when '3'. x = 3.
  when '4'. x = 4.
  when '5'. x = 5.
  when '6'. x = 6.
  when '7'. x = 7.
  when '8'. x = 8.
  when '9'. x = 9.
  when 'A'. x = 10.
  when 'B'. x = 11.
  when 'C'. x = 12.
  when 'D'. x = 13.
  when 'E'. x = 14.
  when 'F'. x = 15.
  when others. x = 0.
endcase.
case c2(1).
  when '1'. x = x + 1 * 16.
  when '2'. x = x + 2 * 16.
  when '3'. x = x + 3 * 16.
  when '4'. x = x + 4 * 16.
  when '5'. x = x + 5 * 16.
  when '6'. x = x + 6 * 16.
  when '7'. x = x + 7 * 16.
  when '8'. x = x + 8 * 16.
  when '9'. x = x + 9 * 16.
  when 'A'. x = x + 10 * 16.
  when 'B'. x = x + 11 * 16.
  when 'C'. x = x + 12 * 16.
  when 'D'. x = x + 13 * 16.
  when 'E'. x = x + 14 * 16.
  when 'F'. x = x + 15 * 16.
endcase.
assign bytechar to <p> type 'X'.
<p> = x.
endform.

***************************************
* protocol/message handling routines
***************************************

* new protocol routine which uses the
* new protocol interface of transporter: NEW_LOGPROT
form newprot using value(severity) value(level) value(mesgid)
                   value(num) value(p1) value(p2) value(p3) value(p4).

case severity.
  when nlp_err. add 1 to count_error.  subrc = 4. import_ok = false.
  when nlp_war. add 1 to count_warning.
  when others. severity = nlp_inf.
endcase.
case level.
  when nlp_lv1. "ok
  when nlp_lv2. "ok
  when nlp_lv3. "ok
  when others. level = nlp_lv3. "default is level 3
endcase.
if clipboard = true and external_protocol = space. "B20K055867
* transport online via RSTXR3TR,RSTXSCRP
  format color col_key.
  write: / space.
  do level times.
    write ' '.
  enddo.
  perform write_msg using mesgid num p1 p2 p3 p4.
else.
* transport via official TRANSPORT
  if export_flag = true.
*    perform new_logprot(rdddic00) using level severity nlp_lan
*                                        mesgid num nlp_nob
*                                        p1 p2 p3 p4.
  else.
    perform new_logprot(rdddic10) using level severity nlp_lan
                                        mesgid num nlp_nob
                                        p1 p2 p3 p4.
  endif.
endif.
endform.

* write a message from T100...
form write_msg using value(abg) value(num)
                     value(p1)  value(p2) value(p3) value(p4).
statics: ln(250) type c,
         mslang like sy-langu.

perform get_message_language using mslang.                   "B20K065126
select single * from t100 where sprsl = mslang               "
                          and   arbgb = abg
                          and   msgnr = num.
if sy-subrc = 0.
* message exists
  ln = t100-text.
  replace '&' with p1 into ln. condense ln.
  replace '&' with p2 into ln. condense ln.
  replace '&' with p3 into ln. condense ln.
  replace '&' with p4 into ln. condense ln.
  write ln(120).
else.
* message not found, write info message
  ln = 'Die Nachricht & Nummer & Sprache & ist nicht gepflegt'(052).
  replace '&' with abg into ln. condense ln.
  replace '&' with num into ln. condense ln.
  replace '&' with message_language into ln. condense ln.
  write ln(120).
endif.
endform.

***************************************
* file (=clipboard) import/export handling routines
***************************************

form export_clipboard using value(end_of_obj).
perform get_binary_file_format_flag.
if sy-subrc = 0.
* binary mode + compress
  perform fc_write_record using filename record end_of_obj.
else.
* text mode
  perform file_write_record using filename 'T'. "B20K053928
endif.
if list_file_contents = 'X'.
  format color col_normal.
  write: / record.
endif.
endform.

* exports binfile header including codepage in a binary representation
* in hex, this reads:
* FF 52 53 54 58 <codepage>
form export_clipboard_bin_header.
statics: rec_ascii like fc_full_tab-record.
field-symbols <p>.

clear rec_ascii.
rec_ascii(1)   = binary_file_header_byte1.
rec_ascii+1(1) = binary_file_header_byte2.
rec_ascii+2(1) = binary_file_header_byte3.
rec_ascii+3(1) = binary_file_header_byte4.
rec_ascii+4(1) = binary_file_header_byte5.
* now write system codepage
perform convert_digit_to_hexbyte using system_codepage(1)
                                       rec_ascii+5(1).
perform convert_digit_to_hexbyte using system_codepage+1(1)
                                       rec_ascii+6(1).
perform convert_digit_to_hexbyte using system_codepage+2(1)
                                       rec_ascii+7(1).
perform convert_digit_to_hexbyte using system_codepage+3(1)
                                       rec_ascii+8(1).
clear rec_ascii+9.
case r3_internal_charset.
  when c_charset_ascii.
    assign rec_ascii to <p> type 'C'.
    record = <p>.
  when c_charset_ebcdic.
    assign rec_ascii to <p> type 'C'.
    record = <p>.
  when c_charset_unicode.
    assign rec_ascii to <p> type 'C'.
    record = <p>.
    c_guifile_bin_codepage = '4110'.
*   TRANSLATE RECORD FROM CODE PAGE C_GUIFILE_BIN_CODEPAGE.
    record_nostruct = record.
   perform translate_from(rstxtranslate) using record_nostruct
                                                c_guifile_bin_codepage.
    record = record_nostruct.
endcase.
perform export_clipboard using false. "not end_of_obj
endform.

* import binary file header including codepage
* set sy-subrc = 0 if format o.k.
* set sy-subrc = 1 if bad format
* set sy-subrc = 2 if bad codepage
* else 3
form import_clipboard_bin_header using codepage.
statics: cp like tcp02-cpcodepage,
         sys_cp like tcp02-cpcodepage,
         eof(1) type c,
         hexbyte type x,
         len like sy-fdpos,
         rec like fc_full_tab-record.

perform fc_read_record_ascii using filename rec eof.
if eof = true.
  subrc = 4.
endif.
if subrc ne 0.
  end_of_clipboard = true. sy-subrc = 3. exit.
endif.
* check binary header
hexbyte = rec(1).
if hexbyte <> binary_file_header_byte1.
  sy-subrc = 1. exit.
endif.
hexbyte = rec+1(1).
if hexbyte <> binary_file_header_byte2.
  sy-subrc = 1. exit.
endif.
hexbyte = rec+2(1).
if hexbyte <> binary_file_header_byte3.
  sy-subrc = 1. exit.
endif.
hexbyte = rec+3(1).
if hexbyte <> binary_file_header_byte4.
  sy-subrc = 1. exit.
endif.
hexbyte = rec+4(1).
if hexbyte <> binary_file_header_byte5.
  sy-subrc = 1. exit.
endif.
* get codepage
perform convert_hexbyte_to_digit using rec+5(1) cp(1).
perform convert_hexbyte_to_digit using rec+6(1) cp+1(1).
perform convert_hexbyte_to_digit using rec+7(1) cp+2(1).
perform convert_hexbyte_to_digit using rec+8(1) cp+3(1).
select single * from tcp00 where cpcodepage = cp.
if sy-subrc = 0.
  codepage = cp.
else.
  sy-subrc = 2. exit.
endif.
* AS/400: use hardcoded codepage for binfile format and language import
len = strlen( custom_language_vector ). "B20K051883
if len = 1 and r3_internal_charset = c_charset_ebcdic.
  sys_cp = system_codepage.
  case cp.
    when '1100'. sys_cp = '0120'.
    when '1400'. sys_cp = '0410'.
    when '1500'. sys_cp = '0500'.
    when '1610'. sys_cp = '0610'.
    when '1700'. sys_cp = '0700'.
    when '1802'. sys_cp = '0800'.
  endcase.
  if sys_cp <> system_codepage.
    format color col_key.
    write: / 'Zielzeichensatz für Import geändert:'(205),
             system_codepage, '->', sys_cp.
    system_codepage = sys_cp.
  endif.
endif.                                  "B20K051883
endform.

* read a line from transport dataset into RECORD
* sets SUBRC <> 0 if line could not be read
form import_clipboard.
statics: eof like boolean.

if binary_file_format_valid = binary_file_format_magic.
* binary and compressed file
  perform fc_read_record using filename record eof.
  if eof = true.
    subrc = 4.
    end_of_clipboard = true.
    exit.
  endif.
* codepage conversion...
  if binfile_codepage <> system_codepage
*  and system_codepage(2) ne '41'.
   and binfile_codepage(2) ne '41'.
*   TRANSLATE RECORD FROM CODE PAGE BINFILE_CODEPAGE.
*    record_nostruct = record.
*    perform translate_from(rstxtranslate) using record_nostruct
*                                                binfile_codepage.
*    record = record_nostruct.
  endif.
else.
* text mode file
  perform file_read_record using filename eof.
  subrc = sy-subrc.
  if subrc ne 0.
    end_of_clipboard = true.
    exit.
  endif.
endif.
if list_file_contents = 'X'.
  format color col_normal.
  write: / record.
endif.
endform.

***************************************
* routines for file open, write, read, close
***************************************

form gui_file_text_init.
refresh gui_file_text. clear gui_file_text.
gui_file_text_cur_line_index = 1.
endform.

form gui_file_bin_init.
refresh gui_file_bin. clear gui_file_bin.
gui_file_bin_cur_line_index = 1.
gui_file_bin_cur_lineofs = 0.
gui_file_bin_total_bytes = 0.
endform.

form gui_file_putbyte using value(byte) type x.
gui_file_bin-l+gui_file_bin_cur_lineofs(1) = byte.
add 1 to gui_file_bin_cur_lineofs.
add 1 to gui_file_bin_total_bytes.
if gui_file_bin_cur_lineofs = c_guifile_bin_numbytes.
  append gui_file_bin.
  add 1 to gui_file_bin_cur_line_index.
  gui_file_bin_cur_lineofs = 0.
endif.
endform.

form gui_file_flush_buffer.
if gui_file_bin_cur_lineofs > 0.
  append gui_file_bin.
endif.
endform.

form gui_file_getbyte using byte type x.
read table gui_file_bin index gui_file_bin_cur_line_index.
if sy-subrc <> 0.
  byte = '00'.
else.
  byte = gui_file_bin-l+gui_file_bin_cur_lineofs(1).
  add 1 to gui_file_bin_cur_lineofs.
  subtract 1 from gui_file_bin_total_bytes.
  if gui_file_bin_total_bytes < 0.
    sy-subrc = 1. exit.
  endif.
  if gui_file_bin_cur_lineofs = c_guifile_bin_numbytes.
    add 1 to gui_file_bin_cur_line_index.
    gui_file_bin_cur_lineofs = 0.
  endif.
  sy-subrc = 0.
endif.
endform.

* replace DOWNLOAD with new calls (UNICODE)
form download tables datatab
              using filename like rlgrap-filename
                    mode type c
                    bin_filesize type i
                    cancel type c.
data: name type string,
      path type string,
      fullpath type string,
      filter type string,
      guiobj type ref to cl_gui_frontend_services,
      uact type i.

if mode <> 'ASC' and mode <> 'BIN'.
  sy-subrc = 1. exit.
endif.
clear cancel.
if filename is initial.
  name = 'test.txt'.
else.
  name = filename.
endif.
filter = '(*.*)|*.*'.  "fix file filter
create object guiobj.
call method guiobj->file_save_dialog
  exporting default_extension = 'txt'
            default_file_name = name
            file_filter       = filter
  changing  filename          = name
            path              = path
            fullpath          = fullpath
            user_action       = uact.
if uact = guiobj->action_cancel.
  cancel = 'X'. exit.
endif.
call function 'GUI_DOWNLOAD'
  exporting
    bin_filesize                  = bin_filesize
    filename                      = fullpath
    filetype                      = mode
*   APPEND                        = ' '
    codepage                      = '4110'
*   NO_BYTEORDER_MARK             = ' '
  importing
    filelength                    = bin_filesize
  tables
    data_tab                      = datatab
*   FORMAT_TAB                    =
  exceptions
    file_write_error              = 1
    no_batch                      = 2
    gui_refuse_filetransfer       = 3
    invalid_type                  = 4
    no_authority                  = 5
    unknown_error                 = 6.
filename = fullpath.
endform.

form upload tables datatab
            using filename like rlgrap-filename
                  mode type c
                  bin_filesize type i
                  cancel type c.
data: name type string,
      filetype type char10,
      filetable type filetable,
      filter type string,
      rc type i,
      guiobj type ref to cl_gui_frontend_services,
      uact type i.

if mode <> 'ASC' and mode <> 'BIN'.
  sy-subrc = 1. exit.
endif.
filetype = mode.
name = filename.
clear cancel.
create object guiobj.
filter = '(*.*)|*.*|)'.
call method guiobj->file_open_dialog
  exporting default_filename = name
            file_filter = filter
  changing  file_table  = filetable
            rc          = rc
            user_action = uact
  exceptions file_open_dialog_failed = 1
            cntl_error               = 2
            error_no_gui             = 3.
check sy-subrc = 0.
if rc < 0.
  cancel = 'X'. exit.
endif.
read table filetable index 1 into name.
check sy-subrc = 0.
call function 'GUI_UPLOAD'
    exporting filename = name
              filetype = filetype
              codepage = '4110'
    importing filelength = bin_filesize
    tables    data_tab = datatab
    exceptions file_open_error  = 2
               file_read_error  = 3
               invalid_type     = 4
               no_batch         = 5
               others           = 6.
filename = name.
endform.

form file_open using value(filename) value(i_o_flag)
                                     value(b_t_flag).
statics: cancel(1),
         file like rlgrap-filename.

perform get_file_source.
if sy-subrc = 0. "GUI upload/download
  if i_o_flag = 'O'.
*   start of output, write later at CLOSE
    if b_t_flag = 'B'.
      perform gui_file_bin_init.
    else.
      perform gui_file_text_init.
    endif.
    sy-subrc = 0.
  else.
*   start of input, read everything now
    file = filename.
    if b_t_flag = 'B'.
      perform gui_file_bin_init.
      perform upload tables gui_file_bin
                     using file
                           'BIN'
                           gui_file_bin_total_bytes
                           cancel.
    else.
      perform gui_file_text_init.
      perform upload tables gui_file_text
                     using file
                           'ASC'
                           gui_file_bin_total_bytes
                           cancel.
    endif.
    if cancel = space and sy-subrc = 0.
      sy-subrc = 0.
    else.
      format color col_negative.
      write: /
   'Dataset konnte nicht zum Lesen geöffnet werden'(018), file.
      sy-subrc = 1.
    endif.
  endif.
  exit.
endif.
* server file system read/write
if b_t_flag = 'B'. "binary mode
  if i_o_flag = 'O'.
    open dataset filename for output in binary mode.
  else.
    open dataset filename for input in binary mode.
  endif.
else.              "text mode
  if i_o_flag = 'O'.
    open dataset filename for output in text mode encoding utf-8.
  else.
    open dataset filename for input in text mode encoding utf-8.
  endif.
endif.
if sy-subrc ne 0.
  if i_o_flag = 'O'.
    format color col_negative.
    write: /
   'Dataset konnte nicht zum Schreiben geöffnet werden'(017),filename.
  else.
    format color col_negative.
    write: /
   'Dataset konnte nicht zum Lesen geöffnet werden'(018), filename.
  endif.
endif.
endform.

form file_close using value(filename) value(i_o_flag).
statics: file like rlgrap-filename,
         bin_filesize type i,
         cancel(1).

perform get_file_source.
if sy-subrc = 0.    "GUI upload/download
  if i_o_flag = 'O'.
*   end of output, write data to gui
    file = filename.
    perform get_binary_file_format_flag.
    if sy-subrc = 0.
*     write last bytes to table
      perform gui_file_flush_buffer.
*     get binary file size
      bin_filesize = gui_file_bin_total_bytes.
      file = filename.
      perform download tables gui_file_bin
                       using file
                             'BIN'
                             bin_filesize
                             cancel.
    else.
      perform download tables gui_file_text
                       using file
                             'ASC'
                             bin_filesize
                             cancel.
    endif.
    if cancel = space.
      sy-subrc = 0.
    else.
      format color col_negative.
      write: /
   'Dataset konnte nicht zum Schreiben geöffnet werden'(017),file.
      sy-subrc = 1.
    endif.
  else.
*    end of input, nothing to do
    sy-subrc = 0.
  endif.
  exit.
else.               "server filesystem read/write
  close dataset filename.
endif.
endform.

form file_write_record using value(file) value(b_t_flag).
if file_source_local = file_source_local_magic. "GUI download
  case r3_internal_charset.
    when c_charset_ascii.
      gui_file_text = record.
    when c_charset_ebcdic.
      gui_file_text = record.
    when c_charset_unicode.
      gui_file_text = record.
  endcase.
  append gui_file_text.
else.                                           "server file write
  transfer record to file.
endif.
endform.

form file_write_bin_rec using value(file) value(bin_rec) type x
                                          value(numbytes).
statics x type x.
field-symbols <p>.

if file_source_local = file_source_local_magic. "GUI download
  assign bin_rec to <p> type 'X'.
  do numbytes times.
    x = <p>.
    perform gui_file_putbyte using x.
*    SHIFT BIN_REC.
    assign <p>+1(1) to <p>.
  enddo.
else.                                           "server file write
  transfer bin_rec to file.
endif.
endform.

form file_read_record using value(file) eof.
if file_source_local = file_source_local_magic. "GUI upload
  read table gui_file_text index gui_file_text_cur_line_index.
  if sy-subrc = 0.
    case r3_internal_charset.
      when c_charset_ascii.
        record = gui_file_text.
      when c_charset_ebcdic.
        record = gui_file_text.
      when c_charset_unicode.
        record = gui_file_text.
    endcase.
    add 1 to gui_file_text_cur_line_index.
    eof = false.
  else.
    eof = true.
  endif.
else.                                           "server file read
  read dataset file into record.
  if sy-subrc = 0.
    eof = false.
  else.
    eof = true.
  endif.
endif.
endform.

form file_read_bin_rec using value(file) bin_rec type x
                                         numbytes.
statics: byte type x.
field-symbols <p>.

if file_source_local = file_source_local_magic. "GUI upload
  assign bin_rec(1) to <p>.
  do numbytes times.
    perform gui_file_getbyte using byte.
    check sy-subrc = 0.
    <p> = byte.
    assign <p>+1(1) to <p>.
  enddo.
else.                                           "server file read
  read dataset file into bin_rec.
endif.
endform.

***************************************
* routines for file compress/decompress
***************************************

* clear all tables
form fc_init.
refresh fc_full_tab. free fc_full_tab.
fc_full_tab_lines = 0.
refresh fc_comp_tab. free fc_comp_tab.
fc_comp_tab_lines = 0.
endform.

* compress FC_FULL_TAB into FC_COMP_TAB
form fc_compress_tab.
call function 'TABLE_COMPRESS'
*    IMPORTING
*         COMPRESSED_SIZE =
     tables
          in              = fc_full_tab
          out             = fc_comp_tab
     exceptions
          compress_error  = 1
          others          = 2.
if sy-subrc <> 0.
  format color col_negative.
  write: / 'Fehler bei TABLE COMPRESS'(200), sy-subrc.
  stop.
endif.
describe table fc_comp_tab lines fc_comp_tab_lines.
endform.

* decompress FC_COMP_TAB into FC_FULL_TAB
form fc_decompress_tab.
check fc_comp_tab_lines > 0.
call function 'TABLE_DECOMPRESS'
     tables
          in                   = fc_comp_tab
          out                  = fc_full_tab
     exceptions
          compress_error       = 1
          table_not_compressed = 2
          others               = 3.
if sy-subrc <> 0.
  format color col_negative.
  write: / 'Fehler bei TABLE DECOMPRESS'(201), sy-subrc.
  stop.
endif.
describe table fc_full_tab lines fc_full_tab_lines.
endform.

* header enth?t: 5 Hex werte als magic number und 6 Stellen
* BCD Darstellung der Anzahl folgender records
form fc_write_compheader using value(dataset_name) value(numrecords).
statics: compheader(50) type x,
         numrec(6) type n.

numrec = numrecords.
compheader(1)   = binary_file_header_byte1.
compheader+1(1) = binary_file_header_byte2.
compheader+2(1) = binary_file_header_byte3.
compheader+3(1) = binary_file_header_byte4.
compheader+4(1) = binary_file_header_byte5.
perform convert_digit_to_hexbyte using numrec(1) compheader+5(1).
perform convert_digit_to_hexbyte using numrec+1(1) compheader+6(1).
perform convert_digit_to_hexbyte using numrec+2(1) compheader+7(1).
perform convert_digit_to_hexbyte using numrec+3(1) compheader+8(1).
perform convert_digit_to_hexbyte using numrec+4(1) compheader+9(1).
perform convert_digit_to_hexbyte using numrec+5(1) compheader+10(1).
clear compheader+11.
perform file_write_bin_rec using dataset_name compheader 50."B20K053928
endform.

* read compressed block header, return number of records
* if error, sy-subrc <> 0, NUMRECORDS=0 indicates end of file
form fc_read_compheader using value(dataset_name) numrecords.
statics: compheader(50) type x,
         numrec(6) type n,
         hexbyte type x,
         eof(1) type c.

perform file_read_bin_rec using dataset_name compheader
                                             50.
if sy-subrc <> 0.
  numrecords = 0. sy-subrc = 2. exit.
endif.
* read binary header id
hexbyte = compheader(1).
if hexbyte <> binary_file_header_byte1.
  sy-subrc = 1. exit.
endif.
hexbyte = compheader+1(1).
if hexbyte <> binary_file_header_byte2.
  sy-subrc = 1. exit.
endif.
hexbyte = compheader+2(1).
if hexbyte <> binary_file_header_byte3.
  sy-subrc = 1. exit.
endif.
hexbyte = compheader+3(1).
if hexbyte <> binary_file_header_byte4.
  sy-subrc = 1. exit.
endif.
hexbyte = compheader+4(1).
if hexbyte <> binary_file_header_byte5.
  sy-subrc = 1. exit.
endif.
* read block size
perform convert_hexbyte_to_digit using compheader+5(1) numrec(1).
perform convert_hexbyte_to_digit using compheader+6(1) numrec+1(1).
perform convert_hexbyte_to_digit using compheader+7(1) numrec+2(1).
perform convert_hexbyte_to_digit using compheader+8(1) numrec+3(1).
perform convert_hexbyte_to_digit using compheader+9(1) numrec+4(1).
perform convert_hexbyte_to_digit using compheader+10(1) numrec+5(1).
numrecords = numrec.
endform.

* write header and compressed blocks to dataset
form fc_output_comp_tab using value(dataset_name).
describe table fc_comp_tab lines fc_comp_tab_lines.
check fc_comp_tab_lines > 0.
perform fc_write_compheader using dataset_name fc_comp_tab_lines.
loop at fc_comp_tab.
  perform file_write_bin_rec using dataset_name          "B20K053928
                                   fc_comp_tab-record    "
                                   1024.                 "
endloop.
fc_comp_tab_lines = 0.
refresh fc_comp_tab. free fc_comp_tab.
endform.

* read header and compressed blocks from dataset
* EOF = TRUE if end of file (i.e. no header)
form fc_input_comp_tab using value(dataset_name) eof.
eof = false.
refresh fc_comp_tab.
perform fc_read_compheader using dataset_name fc_comp_tab_lines.
case sy-subrc.
  when 0.
  when 1.
    format color col_negative.
  write: / 'Fehler: Ungültige Blockkennung, falsches Dateiformat'(202).
    stop.
  when 2.
    eof = true. exit.
  when others.
    if fc_comp_tab_lines = 0.
      eof = true. exit.
    endif.
endcase.
do fc_comp_tab_lines times.
  perform file_read_bin_rec using dataset_name fc_comp_tab-record
                                               fc_comp_tab_numbytes.
  if sy-subrc = 0.
    append fc_comp_tab.
  else.
    format color col_negative.
    write: /
    'Fehler: Zu wenige komprimierte Blöcke, Datei unvollständig'(203).
    write: /
    'Soll-Anzahl:', fc_comp_tab_lines.                       "#EC NOTEXT
    describe table fc_comp_tab lines fc_comp_tab_lines.
    write: /
    'Ist-Anzahl:', fc_comp_tab_lines.                        "#EC NOTEXT
    stop.
  endif.
enddo.
describe table fc_comp_tab lines fc_comp_tab_lines.
endform.

* write single record into FC_FULL_TABLE
* if maximum lines reached, wait till end of object, then
* compress and output data
form fc_write_record using value(dataset_name)
                           value(rec) like record
                           value(end_of_obj).
field-symbols <p>.

case r3_internal_charset.
  when c_charset_ascii.
    assign rec to <p> type 'X'.
    fc_full_tab-record = <p>.
  when c_charset_ebcdic.
    assign rec to <p> type 'X'.
    fc_full_tab-record = <p>.
  when c_charset_unicode.
*    TRANSLATE R TO CODE PAGE C_GUIFILE_BIN_CODEPAGE.
    record_nostruct = rec.
    perform translate_to(rstxtranslate) using record_nostruct
                                              c_guifile_bin_codepage.
    assign record_nostruct to <p> type 'X'.
    fc_full_tab-record = <p>.
endcase.
append fc_full_tab.
add 1 to fc_full_tab_lines.
if fc_full_tab_lines >= fc_full_tab_maxlines and end_of_obj = true.
  perform fc_compress_tab.
  refresh fc_full_tab.
  fc_full_tab_lines = 0.
  perform fc_output_comp_tab using dataset_name.
endif.
endform.

* close file after writing
form fc_flush_buffer using value(dataset_name).
if fc_full_tab_lines > 0.
  perform fc_compress_tab.
  perform fc_output_comp_tab using dataset_name.
  fc_full_tab_lines = 0. refresh fc_full_tab.
endif.
endform.

* read single record from FC_FULL_TABLE
* if table empty, read next block
* compress and output data
* eof = TRUE if end of file
form fc_read_record using value(dataset_name)
                          rec like record
                          eof.
field-symbols <p>.
if fc_full_tab_lines = 0.
* read next block
  perform fc_input_comp_tab using dataset_name eof.
  if eof = true.
    exit.
  endif.
  perform fc_decompress_tab.
endif.
read table fc_full_tab index 1.
if sy-subrc <> 0.
  format color col_negative.
  write: / 'Fehler: Entkomprimierte Tabelle ist leer'(204). stop.
endif.
case r3_internal_charset.
  when c_charset_ascii.
*    assign rec to <p> type 'X'.
*    <p> = fc_full_tab-record.
***************************************
      assign record_nostruct to <p> type 'X'.
      <p> = fc_full_tab-record. "Lollr
      if binfile_codepage(2) = '41'.
        if g_flag_utf8 = false.
          g_flag_utf8 = true.
          c_guifile_bin_codepage = '4110'.
        endif.
        record_nostruct_z = record_nostruct.
*    TRANSLATE R FROM CODE PAGE C_GUIFILE_BIN_CODEPAGE.
        if cop = '0000'.
          cop = c_guifile_bin_codepage.
          lop = cop.
        endif.
        perform translate(rstxtranslate) using record_nostruct cop lop.
        if record_nostruct(9) = ' HEADFORM'.
          perform non_uni_codepage using record_nostruct+89(1) changing eop.
          lop = eop.
          record_nostruct = record_nostruct_z.
          perform translate(rstxtranslate) using record_nostruct cop lop.
        endif.
      endif.
      rec = record_nostruct.
***************************************
  when c_charset_ebcdic.
    assign rec to <p> type 'X'.
    <p> = fc_full_tab-record.
  when c_charset_unicode.
      assign record_nostruct to <p> type 'X'.
      if binfile_codepage(2) = '41' and g_flag_utf8 = false.
        g_flag_utf8 = true.
        c_guifile_bin_codepage = '4110'.
      endif.
      <p> = fc_full_tab-record. "Lollr
      record_nostruct_z = record_nostruct.
*    TRANSLATE R FROM CODE PAGE C_GUIFILE_BIN_CODEPAGE.
      if cop = '0000'.
        cop = c_guifile_bin_codepage.
      endif.
      perform translate_from(rstxtranslate) using record_nostruct cop.
       if record_nostruct(9) = ' HEADFORM' and g_flag_utf8 = false.
        call function 'NLS_GET_FRONTEND_CP'
          exporting
            langu                       = record_nostruct+89(1)
          importing
            frontend_codepage           = cop
          exceptions
             others                      = 4.
         if sy-subrc <> 0.
           message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
         endif.
         record_nostruct = record_nostruct_z.
         perform translate_from(rstxtranslate) using record_nostruct cop.
       endif.
     rec = record_nostruct.
endcase.
delete fc_full_tab index 1.
fc_full_tab_lines = fc_full_tab_lines - 1.
endform.

form fc_read_record_ascii using value(dataset_name)
                                rec type x
                                eof.
statics: r like record.
field-symbols <p> .    "#EC *

if fc_full_tab_lines = 0.
* read next block
  perform fc_input_comp_tab using dataset_name eof.
  if eof = true.
    exit.
  endif.
  perform fc_decompress_tab.
endif.
read table fc_full_tab index 1.
if sy-subrc <> 0.
  format color col_negative.
  write: / 'Fehler: Entkomprimierte Tabelle ist leer'(204). stop.
endif.
rec = fc_full_tab-record.
delete fc_full_tab index 1.
fc_full_tab_lines = fc_full_tab_lines - 1.
endform.

* test form routine that inputs dataset and lists decompressed data
* to test, do external perform on this routine with dataset name
form fc_decompress_and_list using value(dataset).
statics: eof like boolean.

perform set_binary_file_format using true.
open dataset dataset for input in binary mode.
if sy-subrc <> 0.
  format color col_negative.
  write: /
      'Dataset konnte nicht zum Lesen geöffnet werden'(018), dataset.
  exit.
endif.
perform fc_init.
format color col_normal.
do.
  perform fc_read_record using dataset record eof.
  if eof = true.
    exit.
  endif.
  write: / record(80).
enddo.
close dataset dataset.
endform.
*************
form non_uni_codepage using language like thead-tdspras
                   changing codepage type abap_encoding.
  tables: tcp0c.
  data: l_mnls_active(1),
        l_codepage like tcp00-cpcodepage,
        l_platform    like tcp0c-platform.

  call function 'SCP_CODEPAGES_MIXED'
    importing
      ismixed = l_mnls_active.

  call 'CUR_LCL' id 'OPSYSREL' field l_platform.
  select single * from tcp0c
     where langu     = language
      and   platform  = l_platform
       and   country   = space
        and   modifier  = space.

  if sy-subrc = 0.
    if l_mnls_active = ' '.                             "not mixed
      l_codepage = tcp0c-charco.
    else.
      l_codepage = tcp0c-charcomnls.
    endif.
  else.
** default codepage.
    call function 'SCP_GET_CODEPAGE_NUMBER'
      importing
        appl_codepage = l_codepage.
  endif.
  codepage = l_codepage.
endform.                    "non_uni_codepage

*
constants: cp_utf16be type cpcodepage value '4102'.

form opttxt_export using p_tsp06pot type tsp06pot
                         p_line like tsp06pot_line.
data: inlen type i,
      outlen type i,
      inused type i,
      outused type i,
      outbuff(120) type x,
      lang like sy-langu,
      ok type c,
      syscp type cpcodepage,
      clen type i,
      syslang like sy-langu.
data l_cnvobj type ref to cl_abap_conv_obj.

describe field outbuff length outlen in byte mode.
clear ok.
syslang = sy-langu.
catch system-exceptions TEXTENV_CODEPAGE_NOT_ALLOWED = 1
                        TEXTENV_KEY_INVALID          = 2.
if cl_abap_char_utilities=>charsize = 1.
  set locale language p_tsp06pot-lang.
endif.
clear p_line.
p_line-devtype = p_tsp06pot-devtype.
p_line-poption = p_tsp06pot-poption.
p_line-optval  = p_tsp06pot-optval.
write p_tsp06pot-lang to p_line-lang.
CREATE OBJECT l_cnvobj
   EXPORTING
*    INCODE           = '0000'
     OUTCODE          = cp_utf16be
     CTRLCODE         = '.'
     SAPOWNCH         = '.'
   EXCEPTIONS
     INVALID_CODEPAGE = 1
     INTERNAL_ERROR   = 2
     others           = 3.
if sy-subrc = 0.
  clen = strlen( p_tsp06pot-optiontext ) * cl_abap_char_utilities=>charsize.
  CALL METHOD l_cnvobj->CONVERT
    EXPORTING
      INBUFF            = p_tsp06pot-optiontext
      INBUFFLG          = clen
      OUTBUFFLG         = outlen
    IMPORTING
      OUTBUFF           = outbuff
      INUSED            = inused
      OUTUSED           = outused
    EXCEPTIONS
       others            = 1.
  if sy-subrc = 0 and outused > 0.
    p_line-utf16len = outused.
    write outbuff(outused) to p_line-utf16hexstring.
    ok = 'X'.
  endif.
endif.
if cl_abap_char_utilities=>charsize = 1.
  set locale language syslang.
endif.
endcatch.
if ok = 'X'.
  sy-subrc = 0.
else.
  sy-subrc = 1.
endif.
endform.

form opttxt_import using p_line like tsp06pot_line
                         p_tsp06pot type tsp06pot.
data: ok type c,
      syslang like sy-langu,
      hexlen type i,
      clen type i,
      inbuff(120) type x,
      inlen type i,
      outlen type i,
      inused type i,
      outused type i.
data l_cnvobj type ref to cl_abap_conv_obj.

clear ok.
clear p_tsp06pot.
p_tsp06pot-devtype = p_line-devtype.
p_tsp06pot-poption = p_line-poption.
p_tsp06pot-optval  = p_line-optval.
CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
  EXPORTING
    INPUT                  = p_line-lang
  IMPORTING
    OUTPUT                 = p_tsp06pot-lang
  EXCEPTIONS
    UNKNOWN_LANGUAGE       = 1
    OTHERS                 = 2.
check sy-subrc = 0.
syslang = sy-langu.
catch system-exceptions TEXTENV_CODEPAGE_NOT_ALLOWED = 1
                        TEXTENV_KEY_INVALID          = 2.
if cl_abap_char_utilities=>charsize = 1.
  set locale language p_tsp06pot-lang.
endif.
hexlen = p_line-utf16len * 2.
if hexlen > 0.
  inbuff = p_line-utf16hexstring(hexlen).
  inlen = p_line-utf16len.
  CREATE OBJECT l_cnvobj
    EXPORTING
     INCODE           = cp_utf16be
*    OUTCODE          = '0000'
     CTRLCODE         = '.'
     SAPOWNCH         = '.'
    EXCEPTIONS
     INVALID_CODEPAGE = 1
     INTERNAL_ERROR   = 2
     others           = 3.
  if sy-subrc = 0.
    describe field p_tsp06pot-optiontext length clen in byte mode.
    CALL METHOD l_cnvobj->CONVERT
      EXPORTING
        INBUFF            = inbuff
        INBUFFLG          = inlen
        OUTBUFFLG         = clen
      IMPORTING
        OUTBUFF           = p_tsp06pot-optiontext
        INUSED            = inused
        OUTUSED           = outused
      EXCEPTIONS
        others            = 1.
    if sy-subrc = 0 and outused > 0.
      ok = 'X'.
    endif.
  endif.
endif.
if cl_abap_char_utilities=>charsize = 1.
  set locale language syslang.
endif.
endcatch.
if ok = 'X'.
  sy-subrc = 0.
else.
  sy-subrc = 1.
endif.
endform.

******************************************************************************
* specialised for uploading / downloading device type without file dialog
******************************************************************************

FORM rstxscrp_dt USING dt mod file zipped.      " simplified rstxscrp

DATA: rc      LIKE sy-subrc,
      devtype LIKE tsp03-patype.

PERFORM set_message_language USING sy-langu.
list_file_contents = space.
clipboard = true.
external_protocol = space.
PERFORM set_binary_file_format USING false.
PERFORM set_file_source USING true.
PERFORM set_custom_language_vector USING space.
PERFORM set_masterlang_only_flag USING space.
subrc = 0.
end_of_objdata = false.
end_of_clipboard = false.
filename = file.

CASE mod.
  WHEN 'EXPORT'.
    PERFORM file_open_no_dia USING dt filename 'O' ' '.
    CHECK sy-subrc = 0.
    PERFORM export_sapscript USING 'PRIN' dt.
    PERFORM export USING 'PRIN' dt.
    PERFORM file_close_no_dia USING filename 'O'.
  WHEN 'IMPORT'.
    end_of_clipboard = false.
    PERFORM file_open_no_dia USING dt filename 'I' zipped.
    CHECK sy-subrc = 0.
    PERFORM import_sapscript USING 'PRIN' dt rc.
    IF rc <> 0.
      PERFORM file_close_no_dia USING filename 'I'.
      RETURN.
    endif.
    WHILE end_of_clipboard = false.
      PERFORM import USING sy-subrc.
    ENDWHILE.
    PERFORM file_close_no_dia USING filename 'I'.
    devtype = dt.
    CALL FUNCTION 'RSPO_PTYPE_FLUSH'
                  EXPORTING  ptype            = devtype
                  EXCEPTIONS call_error       = 1
                             operation_failed = 2
                             others           = 3.
ENDCASE.

ENDFORM.

FORM download_no_dia TABLES datatab
                     USING  filename     LIKE rlgrap-filename
                            bin_filesize TYPE i.

DATA: name TYPE string.

name = filename.
CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING  bin_filesize = bin_filesize
                         filename     = name
                         filetype     = 'ASC'
                         codepage     = '4110'
              TABLES     data_tab     = datatab
              EXCEPTIONS others       = 31.

ENDFORM.

FORM upload_no_dia TABLES datatab
                   USING  filename     LIKE rlgrap-filename
                          bin_filesize TYPE i
                          zipped       TYPE c.

DATA: name  TYPE string,
      ftype TYPE c LENGTH 10.

name = filename.
IF zipped = ' '. ftype = 'ASC'.
ELSE.            ftype = 'BIN'.
ENDIF.
CALL FUNCTION 'GUI_UPLOAD'
              EXPORTING  filename     = name
                         filetype     = ftype
                         codepage     = '4110'
              IMPORTING  filelength   = bin_filesize
              TABLES     data_tab     = datatab
              EXCEPTIONS others       = 32.

ENDFORM.

FORM file_open_no_dia USING VALUE(dt) VALUE(filename) VALUE(i_o_flag) VALUE(zipped).

PERFORM gui_file_text_init.
IF i_o_flag = 'O'. sy-subrc = 0. RETURN. ENDIF.

IF zipped = ' '.
  PERFORM upload_no_dia TABLES gui_file_text
                        USING  filename
                               gui_file_bin_total_bytes
                               zipped.
ELSE.
  PERFORM gui_file_bin_init.
  PERFORM upload_no_dia TABLES gui_file_bin
                        USING  filename
                               gui_file_bin_total_bytes
                               zipped.
  PERFORM unzipdata USING dt.
  IF sy-subrc <> 0. RETURN. ENDIF.
  PERFORM gui_file_bin_init.
ENDIF.

ENDFORM.

FORM unzipdata USING dt.

DATA: fname   TYPE string,
      zipobj  TYPE REF TO cl_abap_zip,
      xstr1   TYPE xstring,
      xstr2   TYPE xstring.

CONCATENATE dt '.PRI' INTO fname.
TRANSLATE fname TO UPPER CASE.

PERFORM tab2xstr CHANGING xstr1.
CREATE OBJECT zipobj.
CALL METHOD zipobj->load
            EXPORTING zip      = xstr1
            EXCEPTIONS others  = 33.
IF sy-subrc <> 0. RETURN. ENDIF.
CALL METHOD zipobj->get
            EXPORTING  name    = fname
            IMPORTING  content = xstr2
            EXCEPTIONS others  = 34.
IF sy-subrc <> 0. RETURN. ENDIF.

PERFORM xstr2tab USING xstr2.
PERFORM gui_file_bin_init.

ENDFORM.

FORM file_close_no_dia USING VALUE(filename) VALUE(i_o_flag).

DATA: bin_filesize TYPE i.

IF i_o_flag <> 'O'. sy-subrc = 0. RETURN. ENDIF.

PERFORM download_no_dia TABLES gui_file_text
                        USING  filename bin_filesize.

endform.

FORM tab2xstr CHANGING xstr TYPE xstring.

DATA: lines TYPE i.

CLEAR: xstr.
DESCRIBE TABLE gui_file_bin LINES lines.
LOOP AT gui_file_bin.
  CONCATENATE xstr gui_file_bin-l INTO xstr IN BYTE MODE.
ENDLOOP.

ENDFORM.

FORM xstr2tab USING VALUE(xstr) TYPE xstring.

DATA: cnvobj  TYPE REF TO cl_abap_conv_obj,
      rec     LIKE LINE OF gui_file_text,
      lenrest TYPE i,
      ofs     TYPE i,
      cnt     TYPE i,
      xstrsrc TYPE xstring,
      crlf    TYPE x LENGTH 2,
      cstrdst TYPE string.

crlf(1)   = 13.
crlf+1(1) = 10.
lenrest   = xstrlen( xstr ).

CREATE OBJECT cnvobj
              EXPORTING outcode   = '4110'
                        ctrlcode  = 'T'.

WHILE lenrest > 0.
  FIND crlf IN xstr IN BYTE MODE MATCH COUNT cnt MATCH OFFSET ofs.
  IF cnt = 0.                                   " if not found
    xstrsrc = xstr(lenrest).
    lenrest = 0.
  ELSE.                                         " if found
    xstrsrc = xstr(ofs).
    lenrest = lenrest - ofs - 2.
  ENDIF.
  CALL METHOD cnvobj->convert
              EXPORTING inbuff    = xstrsrc
                        outbufflg = 0
              IMPORTING outbuff   = cstrdst.
  CLEAR: rec.
  rec-l = cstrdst.
  APPEND rec TO gui_file_text.
  IF lenrest > 0.
    ofs = ofs + 2.
    xstr = xstr+ofs.
  ENDIF.
ENDWHILE.

ENDFORM.

type-pools scp.

* form called at end of RSTXSCRP import of device type
form check_dt_codepage using p_devtype type rspoptype.
data wa_tsp0a type tsp0a.
data cp type cpcodepage.
data wa_tcp00 type tcp00.
data msg1(80) type c.
data msg2(80) type c.

select single * from tsp0a into wa_tsp0a where patype = p_devtype.
check sy-subrc = 0.
cp = wa_tsp0a-cpcodepage.
select single * from tcp00 into wa_tcp00 where cpcodepage = cp.
if sy-subrc = 0.
* check if cp is migrated to new format
  if wa_tcp00-cpsource < scp_source_tcp237sptsl.
    msg1 = 'Device type $ uses codepage $ which is not migrated to new format'. "#EC NOTEXT
    replace '$' with p_devtype into msg1.
    replace '$' with cp into msg1.
    msg2 = 'Please migrate codepage $ with RSCP0126'. "#EC NOTEXT
    replace '$' with cp into msg2.
  endif.
else. "cp does not exist
  msg1 = 'Device type $ uses codepage $ which does not exist'. "#EC NOTEXT
  replace '$' with p_devtype into msg1.
  replace '$' with cp into msg1.
  condense msg1.
  msg2 = 'Please import codepage $ with RSCP0025'. "#EC NOTEXT
  replace '$' with cp into msg2.
endif.
if msg1 is not initial.
* show warning
  write: / 'The device type will not function in this system' color col_negative, "#EC NOTEXT
         / msg1 color col_negative,
         / msg2 color col_negative.
endif.
endform.
