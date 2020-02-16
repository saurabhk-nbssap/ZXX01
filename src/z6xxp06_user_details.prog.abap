*&---------------------------------------------------------------------*
*& Report  Z6PP011_MATERIAL_BOM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*



REPORT  Z6PP011_MATERIAL_BOM  LINE-SIZE 120 NO STANDARD PAGE HEADING
        MESSAGE-ID zpp01.

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: List of Users with Activity Grps
* OBJECT TYPE       : Report                FUNC. CONSULTANT  : Sanjay
*          DEVELOPER: Ramakrishna
*      CREATION DATE:  09.11.2010
*        DEV REQUEST: IRDK903040
*  TCODE            :
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

*&----------------------Tables Declaration----------------------------&*
TABLES : AGR_TCODES,usr02, BAPIAGR.

TYPE-POOLS : SLIS.
*&----------------------Global Data Declaration-----------------------&*
DATA :  x     LIKE sy-tabix,
        antxt LIKE t416t-antxt,
        sttxt LIKE t415t-sttxt,
        mtart LIKE mara-mtart,
        matkl LIKE mara-matkl,
        first LIKE stko-stlnr,
        istcount TYPE i,
        last  LIKE stko-stlnr.
DATA :   flag,
        tabix like sy-tabix.
*&----------------------Internal Table Declaration--------------------&*
types : BEGIN OF i_user ,
       uname LIKE BAPIBNAME-BAPIBNAME,
       usname type AD_NAMTEXT ,
       AGR_NAME like BAPIAGR-agr_name,
       from_dat type datum,
       to_dat type datum,
       agr_text type BAPIAGR-AGR_Text,
       TCODE type AGR_TCODES-tcode,
       TTEXT TYPE TSTCT-TTEXT,
       END OF i_user.
data : it_user type TABLE OF i_user,
       wa_user type i_user.
data : tt_user type  TABLE OF i_user.
DATA : g_save(1) TYPE c,
       g_exit(1) TYPE c,
       g_variant LIKE disvariant,
       gx_variant LIKE disvariant,
       P_VARI LIKE DISVARIANT-VARIANT. " ALV Variant
*****
DATA : ROLES_LIST TYPE AGR_DEFINE OCCURS 0 WITH HEADER LINE.
DATA : IT_TEMPTSTCT TYPE TSTCT OCCURS 0 WITH HEADER LINE.
data : i_tcodes type  AGR_TCODES OCCURS 0 WITH HEADER LINE.
DATA : it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       is_layout TYPE slis_layout_alv,
        i_repid   LIKE sy-repid,
       it_events TYPE slis_t_event,
       it_listheader TYPE slis_t_listheader.
DATA roles_number TYPE I.
DATA: BEGIN OF profiles_list OCCURS 0,
       prf_name LIKE agr_1016-profile,
      END OF profiles_list.
DATA it_agr_1016 LIKE agr_1016 OCCURS 0 WITH HEADER LINE.
DATA : text(100).

CONSTANTS : formname_top_of_page TYPE slis_formname
                                 VALUE 'TOP_OF_PAGE'.
TYPES: BEGIN OF t_varinfo,
       flag TYPE c,

       olength TYPE x,

        line LIKE raldb-infoline,

END OF t_varinfo.
DATA : IT_INT_TAB TYPE RSPARAMS OCCURS 0 WITH HEADER LINE.

DATA: tables TYPE trdir-name OCCURS 0 WITH HEADER LINE ,

infotab TYPE t_varinfo OCCURS 0 WITH HEADER LINE,

variant_info TYPE rsvaradmin OCCURS 0 WITH HEADER LINE ,

variant_names TYPE rsvarrange OCCURS 0 WITH HEADER LINE .

DATA :  keyinfo  TYPE slis_keyinfo_alv,
        fieldtab TYPE slis_t_fieldcat_alv.
*&----------------------Selection Screen Declaration------------------&*

*----------------------------------------------------------------------*
*                        selection-screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-s00.
PARAMETERS : p_var LIKE disvariant-variant.
SELECTION-SCREEN END   OF BLOCK blk2.
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-s01.
parameters : p_user like usr02-bname.
SELECTION-SCREEN END OF BLOCK A.

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE TEXT-s02.
PARAMETERS : P_UFLAG TYPE USR02-UFLAG OBLIGATORY.
select-OPTIONS : s_ag   for BAPIAGR-agr_name OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B.


INITIALIZATION.


  i_repid = sy-repid.
  PERFORM initialize_variant.
*----------------------------------------------------------------------*
*                   at selection-screen.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  p_vari = p_var.
  PERFORM f4_for_variant.
  p_var = p_vari.

AT SELECTION-SCREEN.
  i_repid = sy-repid.
  p_vari = p_var.
  PERFORM pai_of_selection_screen.


*&----------------------Start-Of-Selection----------------------------&*
START-OF-SELECTION.
  PERFORM get_data.


END-OF-SELECTION.


  PERFORM print_main_details.


AT LINE-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data.
  data : it_usr02 type SALV_BS_T_USR02.
  data : wa_usr02 type usr02.
  DATA : LV_USRNAME TYPE BAPIBNAME-BAPIBNAME,
         WA_ADDR TYPE BAPIADDR3.
  DATA : IT_AGR TYPE BAPIAGR OCCURS 0 WITH HEADER LINE,
         IT_RET TYPE BAPIRET2 OCCURS 0 WITH HEADER LINE.

  select * from usr02 into CORRESPONDING FIELDS OF TABLE it_usr02
                      where bname eq p_user
                        and uflag eq p_uflag.
  IF NOT IT_USR02 IS INITIAL.
    LOOP AT IT_USR02 INTO WA_USR02.
      REFRESH : IT_AGR, IT_RET.
      CLEAR : IT_AGR,IT_RET,WA_ADDR.
      lv_usrname  = wa_usr02-bname.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          USERNAME             = LV_USRNAME
*   CACHE_RESULTS        = 'X'
  IMPORTING
*   LOGONDATA            =
*   DEFAULTS             =
    ADDRESS              = WA_ADDR
*   COMPANY              =
*   SNC                  =
*   REF_USER             =
*   ALIAS                =
*   UCLASS               =
*   LASTMODIFIED         =
*   ISLOCKED             =
        TABLES
*   PARAMETER            =
*   PROFILES             =
    ACTIVITYGROUPS       = IT_AGR
          RETURN               = IT_RET
*   ADDTEL               =
*   ADDFAX               =
*   ADDTTX               =
*   ADDTLX               =
*   ADDSMTP              =
*   ADDRML               =
*   ADDX400              =
*   ADDRFC               =
*   ADDPRT               =
*   ADDSSF               =
*   ADDURI               =
*   ADDPAG               =
*   ADDCOMREM            =
*   PARAMETER1           =
*   GROUPS               =
*   UCLASSSYS            =
*   EXTIDHEAD            =
*   EXTIDPART            =
*   SYSTEMS              =
                .
      IF NOT IT_AGR[] IS INITIAL.

        loop at it_agr.
          move-CORRESPONDING it_agr to wa_user.
          wa_user-uname = lv_usrname.
          wa_user-usname = wa_addr-fullname.

          append wa_user to it_user.
          clear wa_user.
        endloop.
      ENDIF.


    ENDLOOP.

  if not s_ag is INITIAL.
   delete it_user where agr_name not in s_ag.
  endif.
    if not it_user is INITIAL.
      sort it_user by agr_name.
      loop at it_user into wa_user.
        on change of wa_user-agr_name.
          refresh : IT_TEMPTSTCT,ROLES_LIST.
          clear   : IT_TEMPTSTCT,ROLES_LIST.

*          select * from agr_tcodes into CORRESPONDING FIELDS OF table i_tcodes
*                                     where agr_name eq wa_user-agr_name.
          roles_list-agr_name = wa_user-agr_name.
          APPEND roles_list.
          clear  roles_list.
           CALL FUNCTION 'ZUSR_TCODES_LIST_SELOPT_ROLES'
            EXPORTING
               ROLE             = wa_user-agr_name
               SELTYPE          = 'B'
             TABLES
               ROLES_LIST       = roles_list
               E_I_TEMPTSTCT = IT_TEMPTSTCT
                     .

        endon.

*           CALL FUNCTION 'PRGN_STRU_GET_TCODES'
*             EXPORTING
*               ACTIVITY_GROUP       = wa_user-agr_name
*             TABLES
*               I_AGR_TCODES         = i_tcodes
*                     .


        if not IT_TEMPTSTCT[] is INITIAL.
          loop at IT_TEMPTSTCT.
            wa_user-tcode = IT_TEMPTSTCT-tcode.
            WA_USER-TTEXT = IT_TEMPTSTCT-TTEXT.
            append wa_user to tt_user.

          endloop.
          clear wa_user.
        else.
          append wa_user to tt_user.

        endif.
        clear wa_user.
      endloop.
    endif.
    clear it_user.
    it_user = tt_user.
    sort it_user by uname.
  ENDIF.

    delete ADJACENT DUPLICATES FROM it_user COMPARING all fields.


ENDFORM.                    "get_data

*&---------------------------------------------------------------------*
*&      Form  print_main_details
*&---------------------------------------------------------------------*
FORM print_main_details.
  DATA fieldcat TYPE slis_fieldcat_alv.



  PERFORM fill_fieldtabnew(Z6SDINC_ALV1) USING :
   'IT_USER' 'UNAME' 'User ID  '   'UNAME'   'USR02' 1 fieldtab,
   'IT_USER' 'USNAME' 'User Name '   'FULLNAME'   'BAPIADDR3' 1 fieldtab,
   'IT_USER' 'AGR_NAME' 'ROLE Name'   'AGR_NAME'   'AGR_TCODES' 2 fieldtab,
   'IT_USER' 'AGR_TEXT' 'Role Desc'   'AGR_TITLE'   'BAPIAGR' 3 fieldtab,
   'IT_USER' 'FROM_DAT' 'Valid From'   'FROM_DAT'   'BAPIAGR' 4 fieldtab,
   'IT_USER' 'TO_DAT' 'Valid To'   'TO_DAT'   'BAPIAGR' 5 fieldtab,
   'IT_USER' 'TCODE' 'Trans.Code'   'TCODE'   'TSTC' 6 fieldtab,
   'IT_USER' 'TTEXT' 'Trans.Code.Text'   'TTEXT'   'TSTCT' 7 fieldtab.



  text = 'List of Users with Activity Groups and TCodes Attached'.

  PERFORM build_layout.
  PERFORM build_events CHANGING it_events[].
  PERFORM build_comment CHANGING it_listheader[].
*
*  keyinfo-header01 = 'KUNNR'.
*  keyinfo-item01   = 'KUNNR'.
*  keyinfo-header02 = 'SGTXT'.
*  keyinfo-item02   = 'SGTXT'.
*
*  delete it_header where sgtxt eq space or sgtxt is initial.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
    I_CALLBACK_PROGRAM                = I_rEPID
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
    IS_LAYOUT                         = IS_LAYOUT
    IT_FIELDCAT                       = FIELDTAB
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*    I_DEFAULT                         = 'A'
    I_SAVE                            = 'A'
    IS_VARIANT                        = g_variant
    IT_EVENTS                         = it_events
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = it_user
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*  LOOP AT istpob.
*    AT NEW werks.
*      ULINE.
*      FORMAT COLOR 7 INTENSIFIED ON.
*      WRITE :/'|','Plant :',istpob-werks,120 '|'.
*      FORMAT RESET.
*    ENDAT.
*    AT NEW matnr.
*      flag = 1.
*    ENDAT.
*    IF flag = 1.
*      FORMAT COLOR 5 INTENSIFIED ON.
*      WRITE :/'|','Material :',(18) istpob-matnr,
*              (6) 'Name', (40) istpob-maktx,(10) 'Mat.Type',
*              (6) istpob-mtart, (10) 'Mat. Group', (10) istpob-matkl,
*               120 '|'.
*      FORMAT RESET.
*      FORMAT COLOR 5 INTENSIFIED OFF.
*      WRITE :/'|',(10) 'Base Qty',(15) istpob-bmeng,
*                   (5) 'Uom',(5) istpob-bmein,120 '|'.
*      FORMAT RESET.
*      FORMAT COLOR 1 INTENSIFIED ON.
*      WRITE:/'|', (18) 'BOM Component',
*             '|', (4)  'Item',
*             '|', (13) 'Comp.Qty',
*             '|', (5)  'Comp.Uom',
*             '|', (22) 'BOM Usage',
*             '|', (22) 'BOM Status',
*             '|', (10) 'Valid From',
*             '|'.
*      FORMAT RESET.
*      flag = 0.
*    ENDIF.
*    x =  sy-tabix MOD 2.
*    IF x = 0.
*      FORMAT COLOR 2 INTENSIFIED ON.
*    ELSE.
*      FORMAT COLOR 2 INTENSIFIED OFF.
*    ENDIF.
*    WRITE:/'|',(18) istpob-idnrk,
*           '|', (4)  istpob-posnr,
*           '|', (13) istpob-menge,
*           '|', (5)  istpob-meins,
*           '|', (22) istpob-antxt,
*           '|', (22) istpob-sttxt,
*           '|', (10) istpob-datuv,
*           '|'.
*
*    FORMAT RESET.
*    AT END OF matnr.
*    ENDAT.
*    AT END OF werks.
*      ULINE.
*      SKIP.
*    ENDAT.
*  ENDLOOP.
*  ULINE.

ENDFORM.                    " print_main_details

*&---------------------------------------------------------------------*
*&      Form  initialize_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialize_variant.
  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = i_repid.
  gx_variant = g_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    P_VARI = GX_VARIANT-VARIANT.
  ENDIF.
ENDFORM.                               " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form F4_FOR_VARIANT.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
FORM f4_for_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
*    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF g_exit = space.
      P_VARI = GX_VARIANT-VARIANT.
    ENDIF.
  ENDIF.
ENDFORM.                               " F4_FOR_VARIANT

*&---------------------------------------------------------------------*
*&      Form PAI_OF_SELECTION_SCREEN.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*AT SELECTION-SCREEN.

FORM pai_of_selection_screen.
*
  IF NOT P_VARI IS INITIAL.
    MOVE g_variant TO gx_variant.
    MOVE P_VARI TO GX_VARIANT-VARIANT.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
    g_variant = gx_variant.
  ELSE.
    PERFORM initialize_variant.
  ENDIF.
ENDFORM.                               " PAI_OF_SELECTION_SCREEN


*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events CHANGING lt_events TYPE slis_t_event.

  DATA : it_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = lt_events.

  READ TABLE it_events WITH KEY name = slis_ev_top_of_page
                                       INTO it_event.

  IF sy-subrc = 0.
    MOVE formname_top_of_page TO it_event-form.
    APPEND it_event TO lt_events.
  ENDIF.



ENDFORM.                    " build_events

*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_comment CHANGING lt_listheader TYPE slis_t_listheader.

  DATA : line TYPE slis_listheader.
*       text(100).


  CLEAR line.
  line-typ = 'H'.
  line-info = text.
  APPEND line TO lt_listheader.

  if not gx_variant-text is INITIAL.
    CLEAR line.
    line-typ = 'S'.
    line-info = gx_variant-text.
    APPEND line TO lt_listheader.
  endif.

  DELETE INFOTAB WHERE LINE IS INITIAL.
  LOOP AT INFOTAB.


    CLEAR line.
    line-typ = 'S'.
    CONDENSE INFOTAB-LINE.
    line-info = INFOTAB-LINE.

    APPEND line TO lt_listheader.


  ENDLOOP.

*clear line.
*clear text.
*line-typ = 'S'.
*line-key = ''.
*line-info = ''.
*append line to lt_listheader.

ENDFORM.                    " build_comment

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_listheader.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_layout.
  is_layout-zebra = 'X'.
  is_layout-detail_popup = 'X'.
  is_layout-colwidth_optimize = 'X'.
ENDFORM.                    "build_layout

*I001     Basic Data                                                                                     10
*I002     Report Options                                                                                 14
*I003     Pending Boms                                                                                   12
*I004     Maintained Boms                                                                                15
*I005     Material Type Selection                                                                        23
*R        List of Pending BOMS for FERT & Halb                                                           36
*SRB1             Pending BOMs                                                                           20
*SRB2             Maintained BOMs                                                                        23
*SS_MTART D       Material type                                                                          21
