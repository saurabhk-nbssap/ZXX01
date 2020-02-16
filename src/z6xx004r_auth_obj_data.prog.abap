*&---------------------------------------------------------------------*
*& Report  Z6_AUTH
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6XX004R_AUTH_OBJ_DATA.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: List of Authorization Objects
* OBJECT TYPE       : Report             FUNC. CONSULTANT  :
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 04.08.2010
*        DEV REQUEST: IRDK900813
*  TCODE            : ZAUTH
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

tables: tstc, tobjt, usobt,  tactz, tobj, sscrfields.

data: begin of itab occurs 0.
        include structure usobt.

data: end of itab.

data: begin of i_tc occurs 0,
      tcode_l like tstc-tcode,
      tcode_h like tstc-tcode,
      end of i_tc.

data: begin of i_file occurs 0,
      tcode_low  like tstc-tcode,
      tcode_high like tstc-tcode,
      end of i_file.

data: fld_txt like dfies-fieldtext.

data: aa type i, bb type i, cc type i, dd type i, ee type i, ff type i.

data: begin of values occurs 0.
        include structure tpr01.
data: end of values.

*
data  new_line.
data  upload.
data  obj_change.
data  fld_change.
*
data  fld(20).
data  val(20).
data  ln_o like sy-lilli.
data  ln_f like sy-lilli.
data  field like itab-field.
data  object like itab-object.
*
ranges: i_tcode for tstc-tcode. " OPTION EQ SIGN I.
SELECTION-SCREEN BEGIN OF BLOCK S01 WITH FRAME TITLE TEXT-S01.
select-options: tcode for tstc-tcode memory id tcd.


selection-screen function key 1.
selection-screen function key 2.

SELECTION-SCREEN END OF BLOCK S01.
*SELECTION-SCREEN FUNCTION KEY 4.

initialization.

  move 'Create File' to sscrfields-functxt_01.
  move 'Upload Transaction Codes' to sscrfields-functxt_02.

at selection-screen.

  case sy-ucomm.

    when 'FC01'.
      loop at tcode.
        i_file-tcode_high = tcode-high.
        i_file-tcode_low = tcode-low.
        append i_file.
      endloop.
      if sy-subrc <> 0.
        message i000(8i) with 'Empty File will be created!'.
      endif.

      call function 'DOWNLOAD'
            exporting
*               BIN_FILESIZE            = ' '
*               CODEPAGE                = ' '
               filename                = 'C:\Windows\Desktop\TCODE.txt '
               filetype                = 'DAT'
*               ITEM                    = ' '
*               MODE                    = ' '
*               WK1_N_FORMAT            = ' '
*               WK1_N_SIZE              = ' '
*               WK1_T_FORMAT            = ' '
*               WK1_T_SIZE              = ' '
*               FILEMASK_MASK           = ' '
*               FILEMASK_TEXT           = ' '
*               FILETYPE_NO_CHANGE      = ' '
*               FILEMASK_ALL            = ' '
*               FILETYPE_NO_SHOW        = ' '
*               SILENT                  = 'S'
*               COL_SELECT              = ' '
*               COL_SELECTMASK          = ' '
*               NO_AUTH_CHECK           = ' '
*          IMPORTING
*               ACT_FILENAME            =
*               ACT_FILETYPE            =
*               FILESIZE                =
*               CANCEL                  =
             tables
                  data_tab                = i_file
*               FIELDNAMES              =
*          EXCEPTIONS
*               INVALID_FILESIZE        = 1
*               INVALID_TABLE_WIDTH     = 2
*               INVALID_TYPE            = 3
*               NO_BATCH                = 4
*               UNKNOWN_ERROR           = 5
*               GUI_REFUSE_FILETRANSFER = 6
*               OTHERS                  = 7
                  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGN
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.


    when 'FC02'.
      refresh i_tc. clear i_tc. refresh tcode. clear tcode.
      upload = 'X'.
      call function 'UPLOAD'
exporting
*         CODEPAGE                = ' '
   filename                = 'C:\Windows\Desktop\Tcode.txt'
   filetype                = 'DAT'
*         ITEM                    = ' '
*         FILEMASK_MASK           = ' '
*         FILEMASK_TEXT           = ' '
*         FILETYPE_NO_CHANGE      = ' '
*         FILEMASK_ALL            = ' '
*         FILETYPE_NO_SHOW        = ' '
*         LINE_EXIT               = ' '
*         USER_FORM               = ' '
*         USER_PROG               = ' '
*         SILENT                  = 'S'
*    IMPORTING
*         FILESIZE                =
*         CANCEL                  =
*         ACT_FILENAME            =
*         ACT_FILETYPE            =
    tables
         data_tab                = i_tc
*    EXCEPTIONS
*         CONVERSION_ERROR        = 1
*         INVALID_TABLE_WIDTH     = 2
*         INVALID_TYPE            = 3
*         NO_BATCH                = 4
*         UNKNOWN_ERROR           = 5
*         GUI_REFUSE_FILETRANSFER = 6
*         OTHERS                  = 7
         .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      else.
        refresh i_tcode. clear i_tcode.
        loop at i_tc.
          if i_tc-tcode_h is initial.
            if i_tc-tcode_l ca '*'.endif.
            if sy-fdpos <> 20.
              i_tcode-sign   = 'I'.
              i_tcode-option = 'CP'.
              i_tcode-low    = i_tc-tcode_l.
              i_tcode-high   = i_tc-tcode_h.
            else.
              i_tcode-sign   = 'I'.
              i_tcode-option = 'EQ'.
              i_tcode-low    = i_tc-tcode_l.
              i_tcode-high   = i_tc-tcode_h.
            endif.
          else.
            i_tcode-sign   = 'I'.
            i_tcode-option = 'BT'.
            i_tcode-low    = i_tc-tcode_l.
            i_tcode-high   = i_tc-tcode_h.
          endif.
          append i_tcode.
        endloop.
        tcode[] = i_tcode[].
      endif.

  endcase.
*select * from tstc where tcode in tcode.

*ENDSELECT.
*IF sy-subrc <> 0.
*ENDIF.
*  read table tcode.
**  IF tcode IS INITIAL." AND UPLOAD <> 'X'.
**  if sy-subrc <> 0.
**    message e000(8i) with 'Please enter a Transaction Code'.
**  endif.

start-of-selection.
*  2 Transaction     AA Object        BB Object name
*  CC Field      DD Field Description EE Value (interval Low) FF High
  aa = 18.
  bb = aa + 15.
  cc = bb + 50.
  dd = cc + 10.
  ee = dd + 30.
  ff = ee + 20.

*If sy-uname <> 'BASIS'.
* Write: / 'You are not authorised to use this transaction'.
* exit.
* Endif.

*  SET PF-STATUS 'DOWN'.

*IF upload = 'X'.
*  SELECT * FROM usobt INTO TABLE itab WHERE name IN i_tcode.
*ELSE.
  select * from usobt into table itab where name in tcode.
*ENDIF.

  loop at itab.
    select single * from tobjt where object = itab-object
                             and langu = sy-langu.
    clear new_line.
    clear obj_change.
    clear fld_change.
*
    format color off.
    on change of itab-name.
      new_line = 'Y'.
      format color 4.
      write:/2 itab-name(30).
      hide: itab-name.
    endon.
    if new_line <> 'Y'.
      new-line.
    endif.
*
    on change of itab-object.
      obj_change = 'X'.
      write: at aa itab-object.
      hide: itab-object.
    endon.
    if new_line = 'Y' and obj_change <> 'X'.
      write: at aa itab-object.
      hide: itab-object.
    endif.
*
    on change of tobjt-ttext.
      write: at bb tobjt-ttext.
    endon.

    if new_line = 'Y' and obj_change <> 'X'.
      write: at bb tobjt-ttext.
      hide: tobjt-ttext.
    endif.
*
    on change of itab-field.
      fld_change = 'X'.
      write: at cc itab-field color 6 inverse on hotspot.
      hide: itab-field .

      call function 'AUTH_FIELD_GET_INFO'
      exporting
                fieldname = itab-field
*             LANGU     = SY-LANGU
        importing
*             DATEL     =
*             INTTYPE   =
*             LNG       =
*             RC        =
                text      = fld_txt                .
.
      write: at dd fld_txt.
    endon.

    if new_line = 'Y' and fld_change <> 'X'.
      write: at cc itab-field color 6 inverse on hotspot.
      write: at dd fld_txt.
    endif.

*
    write: at ee itab-low.
*
*    itab-high = 'Me'.
    write: at ff itab-high.
  endloop.

end-of-selection.
  clear itab.

at line-selection.

  clear object. clear field.
  ln_o = sy-lilli.
  ln_f = sy-lilli.
  field = itab-field.
  get cursor field fld value val.

  if itab-object is initial.
    do.
      ln_o = ln_o - 1.
      read line ln_o.
      if sy-subrc <> 0.
        exit.
      endif.
      if not itab-object is initial.
        exit.
      endif.
    enddo.
    object = itab-object.
  else.
    object = itab-object.
  endif.

  if field is initial.
    do.
      ln_f = ln_f - 1.
      read line ln_f.
      if sy-subrc <> 0.
        exit.
      endif.
      if not itab-field is initial.
        exit.
      endif.
    enddo.
    field = itab-field.
  endif.

  call function 'SUPRN_MAINTAIN_VALUES'
       exporting
            object                    = object
            field                     = field
*         SHOW_ONLY                 =
*         WITH_VARIABLES            =
*         CHECK_MODE                =
*         AUTHORITY_CHECK_TCODE     = ' '
*         CONVERT_IF_ALLOWED        = 'X'
*         SHOW_ALL_ACTIVITIES       = ' '
*         NO_COMPLETE_AUTHORITY     = ' '
*         AUTH_FOR_S_TABU_LIN       = ' '
*         AUTH_CALL_MODE_S_TABU_LIN = ' '
*    IMPORTING
*         SAVE                      =
       tables
            f_values                  = values.
*         FULL_VALUE_TAB            =
