*-------------------------------------------------------------------
***INCLUDE LSUSLF01  .
*-------------------------------------------------------------------

***********************************************************
*START OF ALV MIGRATIONS          C5056319
***********************************************************
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*   define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION DEFERRED.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:

     on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,


      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.


  METHOD on_user_command.
    PERFORM user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command



  METHOD on_double_click.
    PERFORM double_click USING row column.
  ENDMETHOD.                    "on_double_click

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

DATA: gt_temptstct  TYPE STANDARD TABLE OF  tstct.

DATA: gr_table TYPE REF TO cl_salv_table.
DATA: gr_table1 TYPE REF TO cl_salv_table.

*&---------------------------------------------------------------------*
*&      Form  LIST_TCODES
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_tcodes.
************************************************************************
* Start of ALV Migration declarations - C5056319
************************************************************************

  DATA: lr_content TYPE REF TO cl_salv_form_element.

* DATA: lr1_content type ref to cl_salv_form_element.

  DATA: lr_columns TYPE REF TO cl_salv_columns.

  CONSTANTS: gc_true  TYPE sap_bool VALUE 'X'.


* reference to a functions object
  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  DATA: gr_events TYPE REF TO lcl_handle_events.
  DATA: lr_events TYPE REF TO cl_salv_events_table.
  DATA lin TYPE i.

* for Output Table entries
  LOOP AT temptstct.
    CLEAR: h_tcod_tcode.
    APPEND temptstct TO gt_temptstct.
  ENDLOOP.





* to get the total number of output rows.
  lin = 0.
  LOOP AT temptstct.
    lin = lin + 1.
  ENDLOOP.


* creating an ALV table - grid display

  TRY.
      cl_salv_table=>factory(

        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_temptstct ).

    CATCH cx_salv_msg.
  ENDTRY.


  gr_table->set_screen_status(
  pfstatus    =   'TCOD'
  report      =   sy-repid
  set_functions = gr_table->c_functions_all ).

* setting all ALV Generic functions
  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( gc_true ).

* to optimize columns

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( gc_true ).

  PERFORM set_columns_technical USING lr_columns.

  PERFORM create_alv_form_content_tol USING lin CHANGING lr_content.

* to set the top of list
  gr_table->set_top_of_list( lr_content ).



* gr_table->set_end_of_list( lr1_content ).



* register to the events of cl_salv_table
  lr_events = gr_table->get_event( ).
  CREATE OBJECT gr_events.

*... register to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command FOR lr_events.

* register to the event DOUBLE_CLICK
  SET HANDLER gr_events->on_double_click FOR lr_events.

** setting default ALV generic funtions
*  lr_functions = gr_table->get_functions( ).
*  lr_functions->SET_ALL( gc_true ).


*to display the table
  gr_table->display( ).

ENDFORM.                    "LIST_TCODES



*&--------------------------------------------------------------------*
*&      Form  set_columns_technical
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->IR_COLUMNS text
*---------------------------------------------------------------------*
FORM set_columns_technical USING ir_columns TYPE REF TO
cl_salv_columns.

  DATA: lr_column TYPE REF TO cl_salv_column.

  TRY.
      lr_column = ir_columns->get_column( 'SPRSL' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
ENDFORM.                    "set_columns_technical


*&---------------------------------------------------------------------*
*&      Form  create_alv_form_content_tol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--CR_CONTENT  text
*----------------------------------------------------------------------*
* 13.10.2006  D036028  (note 979532)
* add comment for profiles which are part of a role that contains
* further profiles
*----------------------------------------------------------------------*
FORM create_alv_form_content_tol
                 USING i_lin  CHANGING cr_content TYPE REF TO
cl_salv_form_element.

  DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
        l_title TYPE lvc_title.

  DATA:   lr_grid      TYPE REF TO cl_salv_form_layout_grid,
          lr_text        TYPE REF TO cl_salv_form_text,
          lr_label       TYPE REF TO cl_salv_form_label.
  CREATE OBJECT lr_grid.

  CASE seltyp.
    WHEN 'US'.

      lr_label = lr_grid->create_label(
        row     = 1
        column  = 1
        text    = text-026 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
          row     = 1
          column  = 8
          text    = text-001 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.
      DATA: ustype LIKE usr02-ustyp.
      SELECT SINGLE ustyp FROM usr02 INTO ustype
                    WHERE bname = title1.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 12
            text    = title1 ).

    WHEN 'RO'.



      lr_label = lr_grid->create_label(
            row     = 1
            column  = 1
            text    = text-026 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 8
            text    = text-012 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      SELECT SINGLE * FROM agr_texts
                       WHERE agr_name = title1
                       AND spras = sy-langu
                       AND LINE = '00000'.



      lr_label = lr_grid->create_label(
            row     = 1
            column  = 12
            text    = title1 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 14

            text    = agr_texts-text ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

    WHEN 'PR'.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 1
            text    = text-026 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 08
            text    = text-022 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.
      CLEAR usr11.
      SELECT SINGLE * FROM usr11
                       WHERE langu = sy-langu
                       AND   profn = title1
                       AND   aktps = aktivated.


      lr_label = lr_grid->create_label(
            row     = 1
            column  = 12
            text    = title1 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


      lr_label = lr_grid->create_label(
            row     = 1
            column  = 14
            text    = usr11-ptext ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

* start 13.10.2006 D036028 (note 979532)
      DATA: agr_1016_used TYPE agr_1016,
            prof_10       TYPE agprofile,
            l_count       TYPE i,
            l_text        TYPE string.

      prof_10 = title1(10).
      CALL FUNCTION 'PRGN_CHECK_ROLE_FOR_PROFILE'
        EXPORTING
          profile_name         = prof_10
        IMPORTING
          agr_1016_used        = agr_1016_used
        EXCEPTIONS
          no_data              = 1
          profile_inconsistent = 2
          OTHERS               = 3.

      IF NOT agr_1016_used-agr_name IS INITIAL.
        SELECT COUNT(*)
          FROM agr_1016
          INTO l_count
          WHERE agr_name = agr_1016_used-agr_name.

        IF l_count > 1.

          MESSAGE i020(ue) WITH title1 agr_1016_used-agr_name
            INTO l_text.

          CONCATENATE '(' l_text ')' INTO l_text.

          lr_label = lr_grid->create_label(
            row     = 1
            column  = 14
            text    = l_text ).

          lr_label->set_label_for( lr_text ).
          cr_content = lr_grid.

        ENDIF.
      ENDIF.
* end 13.10.2006 D036028 (note 979532)

    WHEN 'OB'.
      lr_label = lr_grid->create_label(
            row     = 1
            column  = 1
            text    = text-000 ). " OBJECT SHOULD COME HERE

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      SELECT SINGLE * FROM tobjt
                     WHERE langu = sy-langu
                     AND   object = title1.
      lr_label = lr_grid->create_label(
            row     = 1
            column  = 12
            text    = title1 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.



    WHEN 'AU'.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 1
            text    = text-026 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 8
            text    = text-025 ).
      SELECT SINGLE * FROM usr13
                       WHERE langu = sy-langu
                       AND   objct = title1
                       AND   auth  = title2
                       AND   aktps = aktivated.

      lr_label = lr_grid->create_label(
            row     = 1
            column  = 12
            text    = title2 ).
      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

  ENDCASE.

  IF temptstct[] IS INITIAL.

    lr_label = lr_grid->create_label(
          row     = 03
          column  = 01
          text    = text-100 ).
    lr_label->set_label_for( lr_text ).
    cr_content = lr_grid.
  ENDIF.


  lr_label = lr_grid->create_label(
      row     = 3
      column  = 1
      text    = text-043 ).

  lr_label->set_label_for( lr_text ).
  cr_content = lr_grid.


  lr_label = lr_grid->create_label(
        row     = 3
        column  = 12
        text    = i_lin ).

  lr_label->set_label_for( lr_text ).
  cr_content = lr_grid.





ENDFORM.                    "create_alv_form_content_tol

************************************************************************
* The following statements are commented       C5056319
*
************************************************************************

*  DATA POS TYPE P.
*  DATA LIN TYPE I.
*  SET PF-STATUS 'TCOD'.
*  IF SELTYP = SPACE.
*    SET TITLEBAR 'TC0'.
*  ELSE.
*    SET TITLEBAR 'TCD'.
*  ENDIF.
*  LIN = 0.
*  LOOP AT TEMPTSTCT.
*    CLEAR: H_TCOD_TCODE.
*    NEW-LINE.
**     write (110) space color col_normal intensified off.
*    WRITE  SY-VLINE.
*    POS = 43 + C_TCODE_LONGER .
*    WRITE AT 1(POS) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
*    WRITE 1 SY-VLINE.
*    WRITE 2 TEMPTSTCT-TCODE COLOR COL_KEY INTENSIFIED.
*    H_TCOD_TCODE = TEMPTSTCT-TCODE.
*    POS = 6 + C_TCODE_LONGER .
*    POSITION POS. WRITE  SY-VLINE.
*    POS = 7 + C_TCODE_LONGER .
*    POSITION POS.
*    WRITE  TEMPTSTCT-TTEXT(36) COLOR COL_NORMAL INTENSIFIED OFF.
*    POS = 43 + C_TCODE_LONGER .
*    POSITION POS.
*    WRITE  SY-VLINE.
**     write 44 tc-ctext color col_normal intensified off.
**     write 110 sy-vline.
*    HIDE: H_TCOD_TCODE.
*   LIN = LIN + 1.
*   ENDLOOP.
** uline /(110).
*  IF SY-SUBRC = 0.
*    POS = 43 + C_TCODE_LONGER .
*    POSITION POS. WRITE  SY-VLINE.
*    ULINE /(POS).
*    WRITE: / 'Anzahl der selektierten Transaktionen: '(043), LIN.
*  ELSE.
*    IF SELTYP = SPACE.
*      WRITE: 3 ' Keine Transaktion selektiert'(101)
*                                  INTENSIFIED OFF.
*    ELSE.
*      WRITE: 3 ' Keine Transaktionen ausführbar'(100)
*                                 INTENSIFIED OFF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                               " LIST_TCODES


***********************************************************************
* End of AlV Migration Declarations
***********************************************************************


*&---------------------------------------------------------------------*
*&      Form  LIST_TCODES_B
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM list_tcodes_b.
  SET PF-STATUS 'TCO1'.
  SET TITLEBAR 'TCD'.

  LOOP AT temptstct1.
    CLEAR: h_tcod_tcode.
    NEW-LINE.
*     write (110) space color col_normal intensified off.
    WRITE (43) space COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE 1 sy-vline.
    WRITE 2 temptstct1-tcode COLOR COL_KEY INTENSIFIED.
    h_tcod_tcode = temptstct1-tcode.
    WRITE 6 sy-vline.
    WRITE 7 temptstct1-ttext(36) COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE 43 sy-vline.
    IF temptstct1-seltype EQ 'B'.
      WRITE 44  ' TSTCA ' COLOR COL_KEY INVERSE.
      WRITE 52  ' S_TCODE ' COLOR COL_GROUP INVERSE.
    ELSEIF temptstct1-seltype EQ 'S'.
      WRITE 52  ' S_TCODE ' COLOR COL_GROUP INVERSE.
    ELSEIF temptstct1-seltype EQ 'T'.
      WRITE 44  ' TSTCA ' COLOR COL_KEY INVERSE.
    ENDIF.
*   write 62 sy-vline.
*     write 44 tc-ctext color col_normal intensified off.
*     write 110 sy-vline.
    HIDE: h_tcod_tcode.
  ENDLOOP.
* uline /(110).
  ULINE /(43).
ENDFORM.                               " LIST_TCODES
*&---------------------------------------------------------------------*
*&      Form  LISTTITEL
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM listtitel.
  DATA pos TYPE p.
  DATA: ustype LIKE usr02-ustyp.
  save_sy_pfkey = sy-pfkey.
* zusätzlicher sortier button bei TCO1, ansonsten gleiche Behandlung
  IF save_sy_pfkey = 'TCO1'.
    save_sy_pfkey = 'TCOD'.
  ENDIF.
  CASE save_sy_pfkey.
    WHEN 'TCOD'.
      SKIP.
      IF seltyp NE space.
        WRITE: / 'Selektion nach:'(026) COLOR COL_HEADING INVERSE.
      ENDIF.
      CASE seltyp.
        WHEN space.                    " keine headerinformation
        WHEN 'US'.
          WRITE: 'Benutzer'(001) COLOR COL_HEADING INVERSE,
                  30 title1 COLOR COL_HEADING INVERSE.
          SELECT SINGLE ustyp FROM usr02 INTO ustype
            WHERE bname = title1.
          IF sy-subrc EQ 0 AND ustype EQ 'L'.
            WRITE: / 'This user is a reference one!'(013).
            WRITE: /
'The executable transactions can be started only by '(014).
            WRITE: / 'a dialog user that has this reference user assigned!'(015).
          ENDIF.
        WHEN 'RO'. " note 636507
          WRITE: 'Role'(012) COLOR COL_HEADING INVERSE,
                  30 title1 COLOR COL_HEADING INVERSE.
          SELECT SINGLE * FROM agr_texts
                 WHERE agr_name = title1
                 AND spras = sy-langu
                 AND LINE = '00000'.
          IF sy-subrc NE 0.
            SELECT SINGLE * FROM agr_texts
                   WHERE agr_name = title1
                   AND spras = 'E'
                   AND LINE = '00000'.
          ENDIF.
          WRITE agr_texts-text COLOR COL_HEADING INVERSE.
          CLEAR agr_texts.
        WHEN 'PR'.
          WRITE: 'Profil'(022) COLOR COL_HEADING INVERSE,
                  30 title1 COLOR COL_HEADING INVERSE.
          CLEAR usr11.
          SELECT SINGLE * FROM usr11
                 WHERE langu = sy-langu
                 AND   profn = title1
                 AND   aktps = aktivated.
          WRITE: usr11-ptext COLOR COL_HEADING INVERSE.
        WHEN 'OB'.
          WRITE: 'Objekt'(003) COLOR COL_HEADING INVERSE,
                  30 title1 COLOR COL_HEADING INVERSE.
          CLEAR tobjt.
          SELECT SINGLE * FROM tobjt
                 WHERE langu = sy-langu
                 AND   object = title1.
          WRITE: tobjt-ttext COLOR COL_HEADING INVERSE.
        WHEN 'AU'.
          WRITE:  20 'Berechtigung'(025) COLOR COL_HEADING INVERSE,
                  35 title2 COLOR COL_HEADING INVERSE.
          CLEAR usr13.
          SELECT SINGLE * FROM usr13
                 WHERE langu = sy-langu
                 AND   objct = title1
                 AND   auth  = title2
                 AND   aktps = aktivated.
          WRITE: 48 usr13-atext COLOR COL_HEADING INVERSE.
      ENDCASE.
      SKIP.
*     uline /(110).
      pos = 43 + c_tcode_longer .
      ULINE AT /(pos).
*     write /(110) space color col_heading intensified.
      WRITE AT /(pos) space COLOR COL_HEADING INTENSIFIED.
      WRITE 1  sy-vline.
      pos = 4 + c_tcode_longer.
      WRITE: AT  2(pos) 'TCode'(010) COLOR COL_HEADING INTENSIFIED.
      pos = 6 + c_tcode_longer. POSITION pos.
      WRITE   sy-vline.
      pos = 7 + c_tcode_longer. POSITION pos.
      WRITE:  'Text'(011) COLOR COL_HEADING INTENSIFIED.
      pos = 43 + c_tcode_longer. POSITION pos.
      WRITE  sy-vline.
*     write: 44 'Komponente'(012) color col_heading intensified.
*     write 110 sy-vline.
*     uline /(110).
      pos = 43 + c_tcode_longer.
      ULINE AT /(pos).
  ENDCASE.
ENDFORM.                               " LISTTITEL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HIERARCHIE_USER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*      -->P_USER  text                                                 *
*----------------------------------------------------------------------*
FORM display_hierarchie_user USING user.
  DATA: f15,
        rc LIKE sy-subrc.
  DATA: ld_len_nam1 TYPE seu_nodeln,
        ld_len_nam2 TYPE seu_nodeln,
        ld_len_nam3 TYPE seu_nodeln,
        ld_len_nam4 TYPE seu_nodeln,
        ld_len_nam5 TYPE seu_nodeln,
        ld_name1    TYPE seu_text ,
        ld_name2    TYPE seu_text ,
        ld_name3    TYPE seu_text ,
        ld_name4    TYPE seu_text ,
        ld_name5    TYPE seu_text ,
        ld_key      TYPE seu_name .
*
* Pruefen, ob er ueberhaupt den Benutzer anzeigen darf
  SELECT SINGLE * FROM usr02
         WHERE bname = user.
  PERFORM auth_check(sapms01c) USING obj_group
          usr02-class space act_show rc.
  IF rc <> 0.
    MESSAGE s512 WITH usr02-class.
  ELSE.

    CLEAR: gd_refuser.
    SELECT SINGLE refuser FROM usrefus INTO gd_refuser
      WHERE bname = user.

    gd_tree_status = '' .        "note 795769 - initialize tree status
    REFRESH nodetab.

    hidefield(1) = 'U'.
    hidefield+1 = user.

    CLEAR usr03.                                            "USR0340A
    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = user
      IMPORTING
        user_usr03             = usr03
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.                         "USR0340A

    MOVE usr03-name1 TO ld_name1 .
    MOVE usr03-name2 TO ld_name2 .

    ld_len_nam1 = STRLEN( ld_name1 ).                       "USR0340A
    ld_len_nam2 = STRLEN( ld_name2 ).                       "USR0340A
    ld_key = user .
    PERFORM define_colors.
    PERFORM fill_nodetab USING
            ld_key 12 col_usr intsv_usr 1
            ld_name1 ld_len_nam1 0 intsv_val
            ld_name2 ld_len_nam2 0 intsv_val
            ld_name3 ld_len_nam3 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' space.

    PERFORM fill_nodetab_attr USING user 2 space gd_refuser.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      TABLES
        nodetab            = nodetab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.

    SET PF-STATUS 'TREE'.

    CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
         EXPORTING
              callback_program      = 'SAPLSUSL'
              callback_user_command = 'COMMAND_ROUTINE'
              status                = 'OWN'
*              use_control           = 'F'
         IMPORTING
              f15                   = f15
         EXCEPTIONS
              OTHERS                = 1.
  ENDIF.
ENDFORM.                               " DISPLAY_HIERARCHIE_USER
*&---------------------------------------------------------------------*
*&      Form  DEFINE_COLORS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM define_colors.
  col_usr     = col_turq.
  intsv_usr   = 1.
  col_pro     = col_white.
  intsv_pro   = 0.
  col_obj     = col_green.
  intsv_obj   = 1.
  col_aut     = col_green.
  intsv_aut   = 0.
  col_fld     = col_yellow.
  intsv_fld   = 1.
  col_val     = col_yellow.
  intsv_val   = 0.
  col_att     = col_blue.
  intsv_val   = 0.
ENDFORM.                               " DEFINE_COLORS

*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_nodetab USING
                  key        TYPE seu_name
                  keylng     TYPE seu_nodeln
                  keycol     TYPE seu_color
                  intensiv   TYPE seu_intens
                  level      TYPE seu_level
                  pretext    TYPE seu_text
                  ptlng      TYPE seu_nodeln
                  ptcol      TYPE seu_color
                  ptintensiv TYPE seu_intens
                  text1       TYPE seu_text
                  textlng1    TYPE seu_nodeln
                  textcol1    TYPE seu_color
                  tintensiv1  TYPE seu_intens
                  text2       TYPE seu_text
                  textlng2    TYPE seu_nodeln
                  textcol2    TYPE seu_color
                  tintensiv2  TYPE seu_intens
                  text3       TYPE seu_text
                  textlng3    TYPE seu_nodeln
                  textcol3    TYPE seu_color
                  tintensiv3  TYPE seu_intens
                  text4       TYPE seu_text
                  textlng4    TYPE seu_nodeln
                  textcol4    TYPE seu_color
                  tintensiv4  TYPE seu_intens
                  text5       TYPE seu_text
                  textlng5    TYPE seu_nodeln
                  textcol5    TYPE seu_color
                  tintensiv5  TYPE seu_intens
                  text6       TYPE seu_text
                  textlng6    TYPE seu_nodeln
                  textcol6    TYPE seu_color
                  tintensiv6  TYPE seu_intens
                  hide       TYPE seu_hide
                  index      TYPE i
                  force_plus TYPE c
                  text9   TYPE seu_text.

  CLEAR nodetab.
  nodetab-name        = key.
  nodetab-tlevel      = level.
  nodetab-nlength     = keylng.
  nodetab-color       = keycol.
  nodetab-intensiv    = intensiv.

  nodetab-text        = pretext.
  nodetab-tlength     = ptlng.
  nodetab-tcolor      = ptcol.
  nodetab-tintensiv   = ptintensiv.

  nodetab-text1       = text1.
  nodetab-tlength1    = textlng1.
  nodetab-tcolor1     = textcol1.
  nodetab-tintensiv1  = tintensiv1.

  nodetab-text2       = text2.
  nodetab-tlength2    = textlng2.
  nodetab-tcolor2     = textcol2.
  nodetab-tintensiv2  = tintensiv2.

  nodetab-text3       = text3.
  nodetab-tlength3    = textlng3.
  nodetab-tcolor3     = textcol3.
  nodetab-tintensiv3  = tintensiv3.

  nodetab-text4       = text4.
  nodetab-tlength4    = textlng4.
  nodetab-tcolor4     = textcol4.
  nodetab-tintensiv4  = tintensiv4.

  nodetab-text5       = text5.
  nodetab-tlength5    = textlng5.
  nodetab-tcolor5     = textcol5.
  nodetab-tintensiv5  = tintensiv5.

  nodetab-text6       = text6.
  nodetab-tlength6    = textlng6.
  nodetab-tcolor6     = textcol6.
  nodetab-tintensiv6  = tintensiv6.

  nodetab-hide        = hide.
  nodetab-force_plus  = force_plus.
  nodetab-text9       = text9.
  IF index = 0.
    APPEND nodetab.
  ELSE.
    INSERT nodetab INDEX index.
  ENDIF.
ENDFORM.                               " FILL_NODETAB
*&---------------------------------------------------------------------*
*&      Form  COMMAND_ROUTINE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM command_routine TABLES node STRUCTURE seucomm
                  USING command
                  CHANGING exit list_refresh.
  DATA: parent(12),
        hideshort.

  REFRESH: users, objects, profs.

  CLEAR rc_tree.

  CASE command.
    WHEN 'TRPI'.
      hideshort = node-hide(1).
      parent    = node-hide+1.
      IF     gd_tree_status EQ 'SELO'
         AND node-selfield  =  'PLUS' .

        PERFORM next_level_initial
                USING node-id node-name node-tlevel hideshort parent node-text9.
        IF node-parent = '000001' .
          gd_tree_status = '' .
        ENDIF.
      ELSE.
        PERFORM define_colors.
        PERFORM next_level USING hideshort node-id parent node-name
                node-child node-tlevel node-text9.
      ENDIF.

    WHEN 'TREP'.
      IF node-tlevel < 1.
        MESSAGE s023.
        READ TABLE node WITH KEY id = node-id.
        node-ok = ' '.
        MODIFY node INDEX sy-tabix.
      ELSE.
        gd_tree_status = '' .
        hideshort = node-hide(1).
        parent    = node-hide+1.
        PERFORM fill_nodetab_with_subtree
                USING node-id node-name node-tlevel hideshort parent node-text9.
      ENDIF.

    WHEN 'SELO'.
      IF node-tlevel < 1.
        MESSAGE s023.
        READ TABLE node WITH KEY id = node-id.
        node-ok = ' '.
        MODIFY node INDEX sy-tabix.
      ELSE.
        hideshort = node-hide(1).
        parent    = node-hide+1.

        CASE hideshort.

          WHEN 'P'.
            IF node-text9 = gc_attr_prof OR
               node-text  = '<PRO>'.       " note 1234918
              gd_xflag  = hideshort .
              users-low  = parent.
              APPEND users.
              profs-low = node-name .
              APPEND profs.
              CLEAR: objects[] .
            ELSE.
              READ TABLE node WITH KEY id = node-id.
              node-ok = ' '.
              MODIFY node INDEX sy-tabix.
            ENDIF.

          WHEN 'K'.

            CASE node-text9.

              WHEN gc_attr_prof.
                gd_xflag  = hideshort .
                users-low  = parent.
                APPEND users.
                IF gd_refuser IS NOT INITIAL.
                  users-low = gd_refuser.
                  APPEND users.
                ENDIF.
                CLEAR: profs[], objects[] .

              WHEN gc_pro_own.
                gd_xflag  = hideshort .
                users-low  = parent.
                APPEND users.
                CLEAR: profs[], objects[] .

              WHEN gc_pro_ref.
                gd_xflag  = hideshort .
                users-low  = gd_refuser.
                APPEND users.
                CLEAR: profs[], objects[] .

              WHEN gc_attr_refuser.
                IF gd_refuser IS INITIAL.
                  MESSAGE s228(0h).  " ... nicht enthalten.
                  EXIT.
                ENDIF.

              WHEN OTHERS.
                READ TABLE node WITH KEY id = node-id.
                node-ok = ' '.
                MODIFY node INDEX sy-tabix.

            ENDCASE.

          WHEN 'U'.              "User is marked
            gd_xflag = hideshort .
            users-low  = node-name.
            APPEND users.
            IF gd_refuser IS NOT INITIAL.
              users-low = gd_refuser.
              APPEND users.
            ENDIF.
            CLEAR: profs[], objects[] .

          WHEN OTHERS.     "Something goes wrong, but that's not possible yet
            EXIT.

        ENDCASE.

* call the selection screen and return with the selection data
        EXPORT gd_xflag users profs objects TO MEMORY ID 'SEL0004' .
        SUBMIT rsusr004 VIA SELECTION-SCREEN AND RETURN.
        IMPORT gd_xflag users profs objects FROM MEMORY ID 'SEL0004' .


        IF gd_xflag = 'X' .          "RSUSR004 leaved with exit-code
          MESSAGE s854 WITH users-low.  "exception user has no profiles !!!
          EXIT .
        ELSE.
          gd_tree_status = 'SELO'.  "tree status is selective expanded
        ENDIF.

        PERFORM fill_nodetab_with_subtree
                USING node-id node-name node-tlevel
                      hideshort parent node-text9.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                               " COMMAND_ROUTINE

*&---------------------------------------------------------------------*
*&      Form  NEXT_LEVEL
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM next_level USING hide id parent name child level flag.

  DATA: ld_object LIKE ust12-objct,                          "note 795769
        ld_auth   LIKE ust12-auth ,
        ld_cnt    TYPE i .

  CASE hide.
    WHEN 'X'.
    WHEN 'U'.
    WHEN 'K'.
      CASE flag.

        WHEN gc_attr_refuser.

          IF gd_refuser IS NOT INITIAL.
            name = gd_refuser.
            REFRESH nodetab.
            IF child <= 0.
              PERFORM fill_nodetab_refuser USING name level space gd_refuser.
            ENDIF.
          ELSE.
            MESSAGE s593(s#)  .
            EXIT.
          ENDIF.

        WHEN gc_attr_prof.
          name = parent.
          REFRESH nodetab.
          IF child <= 0.
            PERFORM fill_nodetab_all_prof USING name level space gd_refuser.
          ENDIF.

        WHEN gc_pro_own.
          name = parent.
          REFRESH nodetab.
          IF child <= 0.
            PERFORM fill_nodetab_user USING name level space .
          ENDIF.

        WHEN gc_pro_ref.
          name = gd_refuser.
          REFRESH nodetab.
          IF child <= 0.
            PERFORM fill_nodetab_user USING name level space.
          ENDIF.

        WHEN OTHERS.
          rc_tree = 3.

      ENDCASE.

    WHEN 'P'.
      REFRESH nodetab.
      IF child <= 0.
        PERFORM fill_nodetab_prof USING name level space .
      ENDIF.
    WHEN 'O'.
    WHEN 'F'.
    WHEN 'V'.
    WHEN 'A'.
      IF child <= 0.
        REFRESH nodetab.
        ld_object = parent .                              "note 795769
        ld_auth   = name   .
        PERFORM fill_nodetab_auth USING ld_object ld_auth level.
      ENDIF.
  ENDCASE.

  DESCRIBE TABLE nodetab LINES ld_cnt .
  IF ld_cnt EQ 0 AND child <= 0  .
    rc_tree = 2 .
  ENDIF.

  CASE rc_tree .
    WHEN 0 .                      "there some nodes to expand
      IF hide = 'U' OR hide = 'P' OR hide = 'A' OR hide = 'K'.
        IF child <= 0.
          CALL FUNCTION 'RS_TREE_CONSTRUCT'
            EXPORTING
              insert_id          = id
              relationship       = 'CHILD'
            TABLES
              nodetab            = nodetab
            EXCEPTIONS
              tree_failure       = 1
              id_not_found       = 2
              wrong_relationship = 3
              OTHERS             = 4.
        ENDIF.
        CALL FUNCTION 'RS_TREE_EXPAND'
          EXPORTING
            all       = ' '
            node_id   = id
          EXCEPTIONS
            not_found = 01.
      ENDIF.
    WHEN 1 . "there are no further data for the user
      MESSAGE s854(01) WITH name .
    WHEN 2 . "there are no further data for the profile or objects
      MESSAGE s228(0h)  .
    WHEN 3. "referenzbenutzer
  ENDCASE .

ENDFORM.                               " NEXT_LEVEL

*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_USER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_nodetab_user USING bname level with_tree.
  DATA: next_level TYPE seu_level,
        profs_lin  TYPE i.

  DATA: lt_ust04 LIKE TABLE OF ust04 .                 "note 795769
  DATA: ls_ust04 LIKE ust04 .                          "note 795769
  DATA: lt_ust04_all       LIKE TABLE OF ust04,
        lt_father_profiles TYPE TABLE OF usref,
        ls_father_profiles TYPE usref .

  next_level = level + 1.
  hidefield(1) = 'P'.

  DESCRIBE TABLE profs LINES profs_lin.
  IF profs_lin = 0.             "select all profiles from the user
    SELECT * FROM ust04
      INTO TABLE lt_ust04
      WHERE bname = bname.

* Note 773001 - avoid ROOT nodes
    IF sy-subrc NE 0 .
      rc_tree = 1 .
    ENDIF.

    LOOP AT lt_ust04 INTO ls_ust04 .
      PERFORM fill_one_profile
        USING ls_ust04-profile bname next_level with_tree.
    ENDLOOP.

  ELSE.                         "there restrictions regarding profiles

    SELECT * FROM ust04
      INTO TABLE lt_ust04
      WHERE bname   EQ bname
        AND profile IN profs.

    SELECT * FROM ust04
      INTO TABLE lt_ust04_all
      WHERE bname = bname .

    ls_ust04-mandt   = sy-mandt .
    ls_ust04-bname   = bname .

    LOOP AT profs .

      READ TABLE lt_ust04
        WITH TABLE KEY mandt   = sy-mandt
                       bname   = bname
                       profile = profs-low
        TRANSPORTING NO FIELDS.

      IF sy-subrc > 2 .

        CLEAR: lt_father_profiles, ls_father_profiles-profile.
        REFRESH lt_father_profiles.

        ls_father_profiles-profile = profs-low .
        APPEND ls_father_profiles TO lt_father_profiles.

        CALL FUNCTION 'SUSI_GET_FATHER_PROFILES'
          EXPORTING
            aktps    = 'A'
          TABLES
            profiles = lt_father_profiles.

        LOOP AT lt_father_profiles INTO ls_father_profiles .
          READ TABLE lt_ust04_all
            WITH TABLE KEY mandt   = sy-mandt
                           bname   = bname
                           profile = ls_father_profiles-profile
            TRANSPORTING NO FIELDS.

          IF sy-subrc < 4 .
            ls_ust04-profile = ls_father_profiles-profile .
            APPEND ls_ust04 TO lt_ust04 .
          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDLOOP.

    SORT lt_ust04 .
    DELETE ADJACENT DUPLICATES FROM lt_ust04 .

* now all profiles for expansion are located

    LOOP AT lt_ust04 INTO ls_ust04 .
      SELECT SINGLE * FROM usr10
             WHERE profn = ls_ust04-profile
             AND   aktps = aktivated.
      IF usr10-typ = colectprof.
        PERFORM fill_one_profile
                USING ls_ust04-profile bname next_level with_tree.
      ELSE.
        IF ls_ust04-profile IN profs.
          PERFORM fill_one_profile
                  USING ls_ust04-profile bname next_level with_tree.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " FILL_NODETAB_USER

*&---------------------------------------------------------------------*
*&      Form  FILL_ONE_PROFILE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_one_profile
            USING value(subprof) value(profile) value(next_level)
                  with_tree.
  DATA: ld_prof_text TYPE seu_text,
        ld_key       TYPE seu_name.

  IF subprof EQ space .
    EXIT .                        "if no value given do not process
  ENDIF.

  CLEAR: ld_prof_text, ld_key .
  SELECT SINGLE ptext FROM usr11 INTO ld_prof_text
         WHERE  langu = sy-langu
         AND    profn = subprof
         AND    aktps = aktivated.
  hidefield+1 = profile.
  hidefield(1) = 'P'.

  ld_key = subprof .
  PERFORM fill_nodetab USING
          ld_key 12 col_pro intsv_pro next_level
          '<PRO>'(050) 5 0 intsv_val
          ld_prof_text 60 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          hidefield 0 'X' space.

  IF with_tree <> space.
    PERFORM fill_nodetab_prof
            USING subprof next_level with_tree.
  ENDIF.
ENDFORM.                               " FILL_ONE_PROFILE

*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_PROF
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_nodetab_prof USING value(profile)
                             level   TYPE seu_level
                             with_tree.

  DATA: next_level  TYPE seu_level,
        next_level2 TYPE seu_level,
        oldobj LIKE tobj-objct VALUE ' ',
        profs_lin  TYPE i,
        objects_lin TYPE i.
  DATA: lt_ust10s   LIKE TABLE OF ust10s .                 "note 795769
  DATA: ls_ust10s   LIKE ust10s .                          "note 795769

*

  " note   184810
  DATA rc LIKE sy-subrc.
  PERFORM auth_check(sapms01c) USING obj_prof profile space act_show rc.

  IF rc NE 0.
    MESSAGE e475.  "Keine Berechtigung zur Anzeige Profile
    EXIT.
  ENDIF.

  next_level = level + 1.
  next_level2 = level + 2.
  hidefield(1) = 'P'.

  DESCRIBE TABLE profs LINES profs_lin.
  IF profs_lin = 0.
    SELECT * FROM ust10c
           WHERE profn = profile
           AND   aktps = aktivated.
      PERFORM fill_one_profile
              USING ust10c-subprof profile next_level with_tree.
    ENDSELECT.
    IF sy-subrc NE 0.
      rc_tree = 2 .
    ENDIF.
  ELSE.
    SELECT * FROM ust10c
           WHERE profn = profile
           AND   aktps = aktivated.
      SELECT SINGLE * FROM usr10
             WHERE profn = ust10c-subprof
             AND   aktps = aktivated.
      IF usr10-typ = colectprof.
        PERFORM fill_one_profile
                USING ust10c-subprof profile next_level with_tree.
      ELSE.
        IF ust10c-subprof IN profs.
          PERFORM fill_one_profile
                  USING ust10c-subprof profile next_level with_tree.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDIF.

  DESCRIBE TABLE objects LINES objects_lin.
  IF objects_lin = 0.

    SELECT * FROM ust10s INTO TABLE lt_ust10s
       WHERE profn = profile
         AND aktps = aktivated .

    IF sy-subrc EQ 0 AND  rc_tree = 2 .
      rc_tree = 0 .
    ENDIF.

    LOOP AT lt_ust10s INTO ls_ust10s .
      PERFORM fill_one_object
          USING ls_ust10s-objct oldobj profile ls_ust10s-auth
                next_level next_level2 with_tree.
    ENDLOOP.

  ELSE.
    SELECT * FROM ust10s INTO TABLE lt_ust10s
           WHERE profn = profile
           AND   aktps = aktivated
           AND   objct IN objects.

    LOOP AT lt_ust10s INTO ls_ust10s .
      PERFORM fill_one_object
              USING ls_ust10s-objct oldobj profile ls_ust10s-auth
                    next_level next_level2 with_tree.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " FILL_NODETAB_PROF

*&---------------------------------------------------------------------*
*&      Form  FILL_ONE_OBJECT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_one_object
     USING objct oldobj profile auth next_level next_level2 with_tree.

  CONSTANTS:
      lc_keylng_12  TYPE seu_nodeln VALUE 12 ,
      lc_keylng_10  TYPE seu_nodeln VALUE 10 ,
      lc_keylng_0   TYPE seu_nodeln VALUE 0  ,
      lc_ptlng_5    TYPE seu_nodeln VALUE 5  ,
      lc_col_undef  TYPE seu_color  VALUE 0  ,
      lc_txtlng_60  TYPE seu_nodeln VALUE 60 .

  DATA: ld_seu_txt TYPE seu_text ,
        ld_objct   TYPE xuobject ,
        ld_auth    TYPE xuauth ,
        ld_key     TYPE seu_name .

  IF objct EQ space .
    EXIT .                        "if no value given do not process
  ENDIF.

  ld_objct = objct.
  ld_auth  = auth .

  IF ld_objct <> oldobj.
    CLEAR ld_seu_txt .
    SELECT SINGLE ttext FROM tobjt
      INTO ld_seu_txt
      WHERE langu = sy-langu
        AND object = ld_objct .
    hidefield(1) = 'O'.
    hidefield+1 = profile.
    ld_key = ld_objct .
    PERFORM fill_nodetab
      USING ld_key lc_keylng_10 col_obj intsv_obj next_level
            '<OBJ>'(051) lc_ptlng_5 lc_col_undef intsv_val
            ld_seu_txt lc_txtlng_60 lc_col_undef intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
            hidefield 0 'X' space.
    oldobj = ld_objct.
  ENDIF.

  CLEAR ld_seu_txt.
  SELECT SINGLE atext FROM usr13
         INTO ld_seu_txt
         WHERE langu = sy-langu
         AND   objct = ld_objct
         AND   auth  = ld_auth
         AND   aktps = aktivated.

  CLEAR hidefield.
  hidefield(1) = 'A'.
  hidefield+1  = ld_objct.
  ld_key       = auth .
  PERFORM fill_nodetab
    USING ld_key       lc_keylng_12 col_aut intsv_aut next_level2
          '<AUT>'(052) lc_ptlng_5   lc_col_undef intsv_val
          ld_seu_txt   lc_txtlng_60 lc_col_undef intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          hidefield 0 'X' space.

  IF with_tree <> space.
    PERFORM fill_nodetab_auth
            USING ld_objct ld_auth next_level2.
  ENDIF.
ENDFORM.                               " FILL_ONE_OBJECT

*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_AUTH
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_nodetab_auth USING object LIKE ust12-objct
                             auth   LIKE ust12-auth
                             level  TYPE seu_level.
  CONSTANTS:
       lc_keylng_80  TYPE seu_nodeln VALUE 80 ,
       lc_keylng_10  TYPE seu_nodeln VALUE 10 ,
       lc_keylng_0   TYPE seu_nodeln VALUE 0  ,
       lc_ptlng_5    TYPE seu_nodeln VALUE 5  ,
       lc_col_undef  TYPE seu_color  VALUE 0  ,
       lc_txtlng_60  TYPE seu_nodeln VALUE 60 .

  DATA: next_level  TYPE seu_level,
        next_level2 TYPE seu_level,
        ld_seu_text TYPE seu_text ,
        oldfld LIKE tobj-fiel0,
        rc     LIKE sy-subrc.
* Namensraumerweiterung
  DATA: von_bis     TYPE seu_text.
  DATA: len_von_bis TYPE i.
  DATA: ld_len      TYPE seu_nodeln .
  DATA: ld_key      TYPE seu_name .
  DATA: lt_ust12    LIKE TABLE OF ust12 .                 "note 795769
  DATA: ls_ust12    LIKE ust12 .                          "note 795769


  PERFORM auth_check(sapms01c) USING obj_auth object auth act_show rc.
  IF rc NE 0.
    MESSAGE e485.  "Keine Berechtigung zur Anzeige Berechtigungen
    EXIT.
  ENDIF.

  next_level = level + 1.
  next_level2 = level + 2.

  SELECT * FROM ust12
    INTO TABLE lt_ust12
         WHERE objct = object
         AND   auth  = auth
         AND   aktps = aktivated.

  LOOP AT lt_ust12 INTO ls_ust12 .
* Namensraumerweiterung
    CALL FUNCTION 'SUSR_VALUES_CONVERT_FOR_DISP'
      EXPORTING
        von           = ls_ust12-von
        bis           = ls_ust12-bis
      IMPORTING
        von_bis       = von_bis
        output_length = len_von_bis
      EXCEPTIONS
        OTHERS        = 1.

    IF ls_ust12-field <> oldfld.
*     Feldtext lesen
      CALL FUNCTION 'AUTH_FIELD_GET_INFO'
        EXPORTING
          fieldname = ls_ust12-field
        IMPORTING
          datel     = dfies-rollname
          inttype   = dfies-inttype
          lng       = dfies-outputlen
          rc        = rc
          text      = dfies-fieldtext
        EXCEPTIONS
          OTHERS    = 1.

      hidefield(1) = 'F'.
      hidefield+1 = auth.
      ld_seu_text = dfies-fieldtext .
      ld_key      = ls_ust12-field .
      PERFORM fill_nodetab
        USING ld_key       lc_keylng_10  col_fld   intsv_fld  next_level
              '<FLD>'(053) lc_ptlng_5    lc_col_undef intsv_val
               ld_seu_text lc_txtlng_60  lc_col_undef intsv_aut
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               hidefield   0       'X' space.

      oldfld = ls_ust12-field.
      CLEAR hidefield.
      hidefield(1) = 'V'.
      hidefield+1 = ls_ust12-field.

      PERFORM fill_nodetab
        USING  space   lc_keylng_0  lc_col_undef  intsv_val next_level2
               von_bis lc_keylng_80 col_val       intsv_aut
               space   lc_keylng_0  lc_col_undef  intsv_aut
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               hidefield 0 space space.
    ELSE.
      CLEAR hidefield.
      hidefield(1) = 'V'.
      hidefield+1 = ls_ust12-field.
      ld_len = len_von_bis .

      PERFORM fill_nodetab USING
               space   lc_keylng_0  col_val       intsv_val next_level2
               space   lc_keylng_0  col_val       intsv_val
               von_bis ld_len       lc_col_undef  intsv_aut
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               ' ' 0 0 intsv_val
               hidefield 0 space space.
    ENDIF.
  ENDLOOP .
ENDFORM.                               " FILL_NODETAB_AUTH
*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_WITH_SUBTREE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_nodetab_with_subtree
     USING id name level hide parent flag.

  DATA: BEGIN OF save_nodetab OCCURS 100.
          INCLUDE STRUCTURE snodetext.
  DATA: END OF save_nodetab.
  DATA: nodetab_lin LIKE sy-tabix ,
        ld_objct    TYPE xuobject  ,
        ld_auth     TYPE xuauth   .

  save_nodetab[] = nodetab[].
  REFRESH: nodetab.
  CASE hide.
    WHEN 'X'.
    WHEN 'U'.
      PERFORM fill_nodetab_attr_subtree USING name level hide 'X' flag.
    WHEN 'P'.
      PERFORM fill_nodetab_prof USING name level 'X'.
      PERFORM fill_nodetab_prof USING gd_refuser level 'X'.
    WHEN 'O'.
      PERFORM fill_nodetab_obj USING parent name level.
    WHEN 'F'.
    WHEN 'V'.
    WHEN 'A'.                          "Berechtigungen
      ld_objct = parent .
      ld_auth  = name   .
      PERFORM fill_nodetab_auth USING ld_objct ld_auth level.
    WHEN 'K'.

      CASE flag.

        WHEN gc_attr_refuser.
          name = parent.
          IF gd_refuser IS NOT INITIAL.
            name = gd_refuser.
            REFRESH nodetab.
            PERFORM fill_nodetab_refuser USING name level space gd_refuser.
          ELSE.
            MESSAGE s593(s#)  .
            EXIT.
          ENDIF.

        WHEN gc_attr_prof.
          name = parent.
          REFRESH nodetab.
          PERFORM fill_nodetab_attr_subtree USING name level hide 'X' flag.

        WHEN gc_pro_own.
          name = parent.
          REFRESH nodetab.
          PERFORM fill_nodetab_user USING name level 'X'.

        WHEN gc_pro_ref.
          name = gd_refuser.
          REFRESH nodetab.
          PERFORM fill_nodetab_user USING name level 'X'.

        WHEN OTHERS.
          rc_tree = 3.
      ENDCASE.

  ENDCASE.

  DESCRIBE TABLE nodetab LINES nodetab_lin.
  IF nodetab_lin = 0 OR rc_tree = 1.
*  NODETAB[] = SAVE_NODETAB[].
    MESSAGE s228(0h).  " ... nicht enthalten.
  ENDIF.
  IF nodetab_lin GT 0. " note 708324

    CALL FUNCTION 'RS_TREE_DELETE_NODE'
      EXPORTING
        node_id      = id
        without_root = 'X'
      EXCEPTIONS
        id_not_found = 1
        OTHERS       = 2.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      EXPORTING
        insert_id          = id
        relationship       = 'CHILD'
      TABLES
        nodetab            = nodetab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.

    CALL FUNCTION 'RS_TREE_EXPAND'
      EXPORTING
        all       = 'X'
        node_id   = id
      EXCEPTIONS
        not_found = 01.
    CLEAR nodetab.
  ENDIF.

ENDFORM.                               " FILL_NODETAB_WITH_SUBTREE
*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_OBJ
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_nodetab_obj
     USING profile object level.
  DATA: next_level  TYPE seu_level VALUE 0,                "note 795769
        ld_seu_text TYPE seu_text,                         "note 795769
        ld_key      TYPE seu_name ,                        "note 795769
        ld_objct    TYPE ust10s-objct .                    "note 795769
  DATA: lt_ust10s   LIKE TABLE OF ust10s .                 "note 795769
  DATA: ls_ust10s   LIKE ust10s .                          "note 795769

  next_level = level + 1.
  ld_objct   = object .
  SELECT * FROM ust10s INTO TABLE lt_ust10s
         WHERE profn = profile
         AND   aktps = aktivated
         AND   objct = ld_objct .

  LOOP AT lt_ust10s INTO ls_ust10s .
    CLEAR ld_seu_text.
    SELECT SINGLE atext FROM usr13
       INTO ld_seu_text
           WHERE langu = sy-langu
           AND   objct = ls_ust10s-objct
           AND   auth  = ls_ust10s-auth
           AND   aktps = aktivated.
    CLEAR hidefield.
    hidefield(1) = 'A'.
    hidefield+1 = ls_ust10s-objct.
    ld_key = ls_ust10s-auth .
    PERFORM fill_nodetab USING
            ld_key 12 col_green intsv_val next_level
            '<AUT>'(052) 5 0 intsv_val
            ld_seu_text 60 0 intsv_aut
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' space.

    PERFORM fill_nodetab_auth USING ld_objct ust10s-auth next_level.
  ENDLOOP.
ENDFORM.                               " FILL_NODETAB_OBJ
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HIERARCHIE_PROF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROFILE  text                                              *
*      -->P_P_STATE  text                                              *
*----------------------------------------------------------------------*
FORM display_hierarchie_prof USING profile p_state.
  DATA: f15,
        rc LIKE sy-subrc.
  DATA: ld_seu_text TYPE seu_text,
        ld_key      TYPE seu_name.
*       Pruefen, ob er ueberhaupt das Profil anzeigen darf
  PERFORM auth_check(sapms01c) USING obj_prof
                profile space act_show rc.
  IF rc <> 0.
    MESSAGE s511 WITH profile.
  ELSE.
    gd_tree_status = '' .        "note 795769 - initialize tree status
    PERFORM define_colors.

    REFRESH nodetab.
    PERFORM fill_nodetab USING
            'Profil'(162) 10 col_pro intsv_pro 1
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_pro
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            'X' 0 'X' space.
    hidefield(1) = 'P'.
    hidefield+1 = profile.
    CLEAR ld_seu_text.
    SELECT SINGLE ptext FROM usr11
      INTO ld_seu_text
           WHERE  langu = sy-langu
           AND    profn = profile
           AND    aktps = p_state.
    ld_key = profile .
    PERFORM fill_nodetab USING
            ld_key 12 col_pro intsv_pro 2
            '<PRO>'(050) 5 0 intsv_val
            ld_seu_text 60 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' space.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      TABLES
        nodetab            = nodetab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.

    SET PF-STATUS 'TREE'.

    CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
      EXPORTING
        callback_program      = 'SAPLSUSL'
        callback_user_command = 'COMMAND_ROUTINE'
        status                = 'OWN'
      IMPORTING
        f15                   = f15
      EXCEPTIONS
        OTHERS                = 1.
  ENDIF.
ENDFORM.                               " DISPLAY_HIERARCHIE_PROF
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HIERARCHIE_S_TCODE
*&---------------------------------------------------------------------*
*  ms   Variante von display_hierarchie_user, zur Anzeige der
*       Äste, die das Object S_TCODE enthalten
*       Verwendung in Report RSUSR008
*----------------------------------------------------------------------*
*      -->P_USER  Benuzer dessen Profile angezeigt werden sollen     *
*----------------------------------------------------------------------*
FORM display_hierarchie_s_tcode USING    user.
  DATA: f15,
        rc LIKE sy-subrc.
  DATA: parent(12),
        ld_key TYPE seu_name .
*
* Pruefen, ob er ueberhaupt den Benutzer anzeigen darf
  SELECT SINGLE * FROM usr02
         WHERE bname = user.
  PERFORM auth_check(sapms01c) USING obj_group
          usr02-class space act_show rc.
  IF rc <> 0.
    MESSAGE s512 WITH usr02-class.
  ELSE.
    PERFORM define_colors.

    REFRESH nodetab.
    gd_tree_status = '' .        "note 795769 - initialize tree status
    PERFORM fill_nodetab USING
            'Benutzer'(106) 10 col_usr intsv_usr 1
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_usr
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            'X' 0 'X' space.
    hidefield(1) = 'U'.
    hidefield+1 = user.
    ld_key = user .
    PERFORM fill_nodetab USING
            ld_key 12 col_usr intsv_usr 2
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' space.
    PERFORM fill_nodetab_with_subtr_stcode
               USING nodetab-id
               nodetab-name nodetab-tlevel
               'U' user.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      TABLES
        nodetab            = nodetab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.

    CALL FUNCTION 'RS_TREE_EXPAND'
      EXPORTING
        all       = 'X'
        node_id   = 1
      EXCEPTIONS
        not_found = 01.

    CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
      EXPORTING
        callback_program    = 'SAPLSUSL'
        screen_start_column = 65
        screen_start_line   = 2
        screen_end_column   = 150
        screen_end_line     = 30
        status              = 'OWN'
      IMPORTING
        f15                 = f15
      EXCEPTIONS
        OTHERS              = 1.
  ENDIF.
ENDFORM.                               " DISPLAY_HIERARCHIE_S_TCODE
*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_WITH_SUBTR_stcode
*&---------------------------------------------------------------------*
*  ms   Variante von fill_nodetab_with_subtree Anzeige der
*       Äste, die das Object S_TCODE enthalten
*       Verwendung in Report RSUSR008
*----------------------------------------------------------------------*
FORM fill_nodetab_with_subtr_stcode
     USING id name level hide parent.

  DATA: BEGIN OF save_nodetab OCCURS 100.
          INCLUDE STRUCTURE snodetext.
  DATA: END OF save_nodetab.
  DATA nodetab_lin LIKE sy-tabix.

  save_nodetab[] = nodetab[].
  REFRESH: nodetab.

  PERFORM fill_nodetab_user_s_tcode USING name level 'X'.


  DESCRIBE TABLE nodetab LINES nodetab_lin.
  IF nodetab_lin = 0.
    nodetab[] = save_nodetab[].
    MESSAGE s027 .   " ... nicht enthalten.
  ENDIF.

  CALL FUNCTION 'RS_TREE_DELETE_NODE'
    EXPORTING
      node_id      = id
      without_root = 'X'
    EXCEPTIONS
      id_not_found = 1
      OTHERS       = 2.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    EXPORTING
      insert_id          = id
      relationship       = 'CHILD'
    TABLES
      nodetab            = nodetab
    EXCEPTIONS
      tree_failure       = 1
      id_not_found       = 2
      wrong_relationship = 3
      OTHERS             = 4.

  CALL FUNCTION 'RS_TREE_EXPAND'
    EXPORTING
      all       = 'X'
      node_id   = id
    EXCEPTIONS
      not_found = 01.
ENDFORM.                               " FILL_NODETAB_WITH_SUBTREE

*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_USER_s_tcode
*&---------------------------------------------------------------------*
*                                                                      *
*  ms   Variante von fill-nodetab_user, zur Anzeige der
*       Äste, die das Object S_TCODE enthalten
*       Verwendung in Report RSUSR008
*----------------------------------------------------------------------*
FORM fill_nodetab_user_s_tcode USING bname level with_tree.
  DATA: next_level TYPE seu_level.
  DATA: lt_ust04 LIKE TABLE OF ust04 .                 "note 795769
  DATA: ls_ust04 LIKE ust04 .                          "note 795769

  next_level = level + 1.
  hidefield(1) = 'P'.

  SELECT * FROM ust04
    INTO TABLE lt_ust04
    WHERE bname = bname.

  LOOP AT lt_ust04 INTO ls_ust04 .
    PERFORM fill_one_profile_s_tcode
      USING ls_ust04-profile bname next_level with_tree.
  ENDLOOP.
ENDFORM.                               " FILL_NODETAB_USER_s_tcode
*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_PROF_s_tcode
*&---------------------------------------------------------------------*
*       ms                                                          *
*  ms   Variante von fill_nodetab_prof, zur Anzeige der
*       Äste, die das Object S_TCODE enthalten
*       Verwendung in Report RSUSR008
*----------------------------------------------------------------------*
FORM fill_nodetab_prof_s_tcode  USING
                  value(profile) value(level) with_tree.
  DATA: next_level  TYPE seu_level,
        next_level2 TYPE seu_level,
        oldobj LIKE tobj-objct VALUE ' '.
*
  next_level = level + 1.
  next_level2 = level + 2.
  hidefield(1) = 'P'.

  SELECT * FROM ust10s                 "einzelprofile
           WHERE profn = profile
           AND   aktps = aktivated
           AND   objct = 'S_TCODE'.
    PERFORM fill_one_object
              USING ust10s-objct oldobj profile ust10s-auth
                    next_level next_level2 with_tree.
  ENDSELECT.

  SELECT * FROM ust10c                 "(sammel)profile
           WHERE profn = profile
           AND   aktps = aktivated.
    PERFORM fill_one_profile_s_tcode
           USING ust10c-subprof profile next_level with_tree.
  ENDSELECT.
ENDFORM.                               " FILL_NODETAB_PROF_s_tcode
*&---------------------------------------------------------------------*
*&      Form  check_if_s_tcode
*&---------------------------------------------------------------------*
*   ms   wie fill_nodetab_prof_s_tcode, aber keine Anderung an  nodetab
*        testet, ob in tieferliegenden Hierarchieebenen das Objekt
*        S_TCODE auftaucht
*----------------------------------------------------------------------*
*  -->  ...
*       s_tcode_found  'X'  wenn S_Tcode in tieferer Hierarchieebene
*                      ' '  wenn nicht
*                      'C'  Zwischenstatus
*                           bei Sammelprofilen => weiterbearbeitung bis
*                           'X' oder ' ' gesetzt wird
*----------------------------------------------------------------------*
FORM check_if_s_tcode  USING
                  value(profile) value(level) with_tree
                  CHANGING
                           s_tcode_found.
  DATA: next_level  TYPE seu_level ,
        next_level2 TYPE seu_level,
        oldobj LIKE tobj-objct VALUE ' '.
*
  next_level = level + 1.
  next_level2 = level + 2.
  hidefield(1) = 'P'.

  SELECT * FROM ust10s                 "einzelprofile
           WHERE profn = profile
           AND   aktps = aktivated
           AND   objct = 'S_TCODE'.
    s_tcode_found = 'X'.
  ENDSELECT.

  SELECT * FROM ust10c                 "(sammel)profile
           WHERE profn = profile
           AND   aktps = aktivated.
    IF  s_tcode_found <> 'X'.
      s_tcode_found = 'C'.
    ENDIF.

    PERFORM check_if_s_tcode
           USING ust10c-subprof  next_level with_tree
           CHANGING s_tcode_found.
  ENDSELECT.
ENDFORM.                               " check_if_s_tcode

*&---------------------------------------------------------------------*
*&      Form  FILL_ONE_PROFILE_s_tcode
*&---------------------------------------------------------------------*
*  ms   Variante von fill-one-profile, zur Anzeige der
*       Äste, die das Object S_TCODE enthalten
*       Verwendung in Report RSUSR008
*----------------------------------------------------------------------*
FORM fill_one_profile_s_tcode
            USING value(subprof) value(profile) value(next_level)
                  with_tree.
  DATA: s_tcode_found VALUE ' '.
  DATA: ld_seu_text TYPE seu_text,
        ld_key      TYPE seu_name.

  CLEAR ld_seu_text.
  SELECT SINGLE ptext FROM usr11 INTO ld_seu_text
         WHERE  langu = sy-langu
         AND    profn = subprof
         AND    aktps = aktivated.
  IF with_tree <> space.
    PERFORM check_if_s_tcode
            USING subprof next_level with_tree
            CHANGING s_tcode_found.
  ENDIF.
  IF s_tcode_found = 'X'.
    hidefield+1 = profile.
    ld_key = subprof.
    PERFORM fill_nodetab USING
          ld_key 12 col_pro intsv_pro next_level
          '<PRO>'(050) 5 0 intsv_val
          ld_seu_text 60 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          hidefield 0 'X' space.
    IF with_tree <> space.
      PERFORM fill_nodetab_prof_s_tcode
            USING ld_key next_level with_tree.
    ENDIF.
  ENDIF.
ENDFORM.                               " FILL_ONE_PROFILE_s_tcode
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET_TCODE
*&---------------------------------------------------------------------*
*       Set status in ALV Grid control
*----------------------------------------------------------------------*
*  -->  rt_extab        exclude table
*----------------------------------------------------------------------*
FORM pf_status_set_tcode USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'TCODE_ALV' EXCLUDING rt_extab.
ENDFORM.                               " PF_STATUS_SET_TCODE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_TCODE
*&---------------------------------------------------------------------*
*       User command for FM SUSR_TCODES_LIST_ALV
*----------------------------------------------------------------------*
*  -->  r_ucomm        user command
*  -->  rs_selfield    selected field structure
*----------------------------------------------------------------------*
FORM user_command_tcode USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
  DATA:
* selected tcode
    tcode LIKE tstc-tcode.

  CASE r_ucomm.
    WHEN 'MYPICK'.
      IF rs_selfield-sel_tab_field = '1-TCODE'.
        tcode = rs_selfield-value.
      ENDIF.
  ENDCASE.
ENDFORM.                               " USER_COMMAND_TCODE



***********************************************************
*START OF ALV MIGRATIONS           C5056319
***********************************************************

*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM double_click  USING    p_row
                            p_column.

  DATA ls_gt_temptstct TYPE tstct.
  DATA: gt_intusobt_new  TYPE STANDARD TABLE OF usobt_c.
  DATA: lv_type1 TYPE usobt-type.
  DATA: lv_name1 TYPE usobt-name.
  DATA: lv_text1 TYPE tstct-ttext.


  READ TABLE gt_temptstct INTO ls_gt_temptstct INDEX p_row.

  MOVE ls_gt_temptstct-tcode TO h_tcod_tcode.


  CALL FUNCTION 'SUSR_TCODE_DISPLAY'
    EXPORTING
      name        = h_tcod_tcode
      type        = type_tcode
    IMPORTING
      ev_type     = lv_type1
      ev_name     = lv_name1
      ev_text     = lv_text1
    TABLES
      gt_intusobt = gt_intusobt_new.


*   ENDCASE.


ENDFORM.                     " double_click


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*      Displays the content based on the selection criteria.
*----------------------------------------------------------------------*
FORM user_command USING i_function TYPE salv_de_function.

  DATA: l_delete_box TYPE i VALUE 0,
        l_tabix TYPE sy-tabix.

  DATA: lr_selections TYPE REF TO cl_salv_selections,
       lt_rows       TYPE salv_t_row.

  lr_selections = gr_table->get_selections( ).

* set selection mode
  lr_selections->set_selection_mode(
  if_salv_c_selection_mode=>row_column ).

  lt_rows = lr_selections->get_selected_rows( ).

  CASE i_function.
    WHEN 'PICK'.
      LOOP AT lt_rows INTO l_tabix.
        PERFORM double_click  USING l_tabix ''.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------

*&      Form  set_columns_technicaL_detail
*&---------------------------------------------------------------------

*       text
*----------------------------------------------------------------------

*      -->P_LR_COLUMNS  text
*----------------------------------------------------------------------

FORM set_columns_technical_detail USING ir_columns TYPE REF TO
cl_salv_columns.

  DATA: lr_column TYPE REF TO cl_salv_column.

  TRY.
      lr_column = ir_columns->get_column( 'MODIFIER' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  TRY.
      lr_column = ir_columns->get_column( 'MODIFIED' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  TRY.
      lr_column = ir_columns->get_column( 'NAME' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.




  TRY.
      lr_column = ir_columns->get_column( 'TYPE' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  TRY.
      lr_column = ir_columns->get_column( 'MODTIME' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  TRY.
      lr_column = ir_columns->get_column( 'MODDATE' ).
      lr_column->set_technical( if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " set_columns_technicaL_detail
*&---------------------------------------------------------------------

*&      Form  create_alv_form_content_tol_1
*&---------------------------------------------------------------------

*       text
*----------------------------------------------------------------------

*      <--P_lr_content  text
*----------------------------------------------------------------------

FORM create_alv_form_content_tol_1 USING i_type_new i_name_new
i_text_new CHANGING cr_content TYPE REF TO cl_salv_form_element.

  DATA:   lr_grid      TYPE REF TO cl_salv_form_layout_grid,
          lr_text        TYPE REF TO cl_salv_form_text,
          lr_label       TYPE REF TO cl_salv_form_label.
  CREATE OBJECT lr_grid.

  CASE i_type_new.
    WHEN 'TR'.

      lr_label = lr_grid->create_label(
        row     = 1
        column  = 1
        text    = text-002 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


      lr_label = lr_grid->create_label(
      row     = 1
      column  = 8
      text    = i_name_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
      row     = 1
      column  = 15
      text    = i_text_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


    WHEN 'RE'.

      lr_label = lr_grid->create_label(
        row     = 1
        column  = 1
        text    = text-008 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


      lr_label = lr_grid->create_label(
      row     = 1
      column  = 8
      text    = i_name_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
      row     = 1
      column  = 15
      text    = i_text_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.




    WHEN 'FB'.

      lr_label = lr_grid->create_label(
        row     = 1
        column  = 1
        text    = text-009 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


      lr_label = lr_grid->create_label(
      row     = 1
      column  = 8
      text    = i_name_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
      row     = 1
      column  = 15
      text    = i_text_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


    WHEN 'RU'.

      lr_label = lr_grid->create_label(
        row     = 1
        column  = 1
        text    = text-023 ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


      lr_label = lr_grid->create_label(
      row     = 1
      column  = 8
      text    = i_name_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.

      lr_label = lr_grid->create_label(
      row     = 1
      column  = 15
      text    = i_text_new ).

      lr_label->set_label_for( lr_text ).
      cr_content = lr_grid.


  ENDCASE.
ENDFORM.                   " create_alv_form_content_tol_1




*&---------------------------------------------------------------------

*&      Form  set_sort
*&---------------------------------------------------------------------

*       to sort  the  required columns
*----------------------------------------------------------------------


*----------------------------------------------------------------------

FORM set_sort  .

  DATA:
         lr_sorts TYPE REF TO cl_salv_sorts. "sort information
  lr_sorts = gr_table1->get_sorts( ).
  lr_sorts->clear( ).
  "remove all existing sort setings
*
  TRY.
      lr_sorts->add_sort(
        columnname = 'OBJECT'
        position   = 1
        sequence   = if_salv_c_sort=>sort_up ).
    CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
*    "#EC NO_HANDLER
  ENDTRY.
  TRY.
      lr_sorts->add_sort(
        columnname = 'FIELD'
        position   = 2
"        subtotal   = abap_true"applies only if some aggregations are
*turned on
        sequence   = if_salv_c_sort=>sort_up ).
    CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
*    "#EC NO_HANDLER
  ENDTRY.



ENDFORM.                    " set_sort

*

*&---------------------------------------------------------------------*
*&      Form  DETAILS_LIST
*&---------------------------------------------------------------------*
*        to display detail list
*----------------------------------------------------------------------*
*      -->P_EV_TYPE  text
*      -->P_EV_NAME  text
*      -->P_EV_TEXT  text
*      -->P_GT_INTUSOBT[]  text
*----------------------------------------------------------------------*
FORM details_list  USING    p_ev_type
                            p_ev_name
                            p_ev_text
                            p_gt_intusobt TYPE table.


  CONSTANTS: gc_true  TYPE sap_bool VALUE 'X'.
  DATA: lr_column TYPE REF TO cl_salv_column.
  DATA: ir_columns TYPE REF TO cl_salv_columns.
  DATA: lr_table   TYPE REF TO cl_salv_table.


*...create an ALV table
*   create an instance  of  lcl_salv_table

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table1
        CHANGING
          t_table      = p_gt_intusobt ).
    CATCH cx_salv_msg.
  ENDTRY.

*...Functions
*.. activate ALV generic Functions
  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
  DATA: lr_content TYPE REF TO cl_salv_form_element.

  lr_functions = gr_table1->get_functions( ).
  lr_functions->set_all( gc_true ).

*... set the columns technical
  DATA: lr_columns TYPE REF TO cl_salv_columns.

  lr_columns = gr_table1->get_columns( ).
  lr_columns->set_optimize( gc_true ).


**... to set sorting.
  PERFORM set_sort  .


  PERFORM set_columns_technical_detail USING lr_columns.


  PERFORM create_alv_form_content_tol_1 USING p_ev_type p_ev_name
 p_ev_text
  CHANGING lr_content  .

* to set the top of list
  gr_table1->set_top_of_list( lr_content ).

*... §4 display the table
  gr_table1->display( ).

ENDFORM.                    " DETAILS_LIST



*******************************************************
* END OF ALV MIGRATION CHANGES  C5056319
********************************************************

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HIERARCHIE_PROF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROFILE  text                                              *
*      -->P_P_STATE  text                                              *
*----------------------------------------------------------------------*
FORM display_hierarchie_auth USING object auth p_state.
  DATA: f15,
        rc LIKE sy-subrc,
        ld_seu_name TYPE seu_name ,
        ld_seu_text TYPE seu_text .

* Pruefen, ob er (sie) ueberhaupt die Berechtigung anzeigen darf
  PERFORM auth_check(sapms01c) USING obj_auth
                                     object auth act_show rc.
  IF rc <> 0.
*   Keine Berechtigung zum Anzeigen der Berechtigung &1 von Objekt &2
    MESSAGE s510 WITH auth object.
  ELSE.
    PERFORM define_colors.

    REFRESH nodetab.
    PERFORM fill_nodetab USING
            'Object'(003) 10 col_obj intsv_obj 1
            ' ' 0 0 intsv_pro
            ' ' 0 0 intsv_obj
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            'X' 0 'X' space.
    hidefield(1) = 'O'.
    hidefield+1 = object.

    CLEAR: tobjt.
    SELECT SINGLE * FROM tobjt
           WHERE langu  = sy-langu
           AND   object = object.
    ld_seu_name = object .
    ld_seu_text = tobjt-ttext .
    PERFORM fill_nodetab USING
            ld_seu_name 12 col_obj intsv_obj 2
            '<OBJ>'(051) 5 0 intsv_pro
            ld_seu_text 60 0 intsv_aut
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' space.

    CLEAR usr13.
    SELECT SINGLE * FROM usr13
           WHERE langu = sy-langu
           AND   objct = object
           AND   auth  = auth
           AND   aktps = p_state.
    CLEAR hidefield.
    hidefield(1) = 'A'.
    hidefield+1 = object.

    ld_seu_name = auth .
    ld_seu_text = usr13-atext .
    PERFORM fill_nodetab USING
            ld_seu_name 12 col_green intsv_aut 3
            '<AUT>'(052) 5 0 intsv_aut
            ld_seu_text 60 0 intsv_aut
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' space.

    PERFORM fill_nodetab_auth USING object ust10s-auth 3.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      TABLES
        nodetab            = nodetab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.

    SET PF-STATUS 'TREE_AUTH'.

    CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
      EXPORTING
        callback_program      = 'SAPLSUSL'
        callback_user_command = 'COMMAND_ROUTINE'
        status                = 'OWN'
      IMPORTING
        f15                   = f15
      EXCEPTIONS
        OTHERS                = 1.
  ENDIF.
ENDFORM.                               " DISPLAY_HIERARCHIE_PROF
*&---------------------------------------------------------------------*
*&      Form  next_level_initial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM next_level_initial USING id name level hide parent flag.

  DATA: BEGIN OF save_nodetab OCCURS 100.
          INCLUDE STRUCTURE snodetext.
  DATA: END OF save_nodetab.
  DATA nodetab_lin LIKE sy-tabix.
  DATA: ld_object TYPE ust12-objct,
        ld_auth   TYPE ust12-auth .

  DATA: refuser   LIKE usrefus-refuser,
        bname LIKE usr02-bname.

  bname = parent.

  save_nodetab[] = nodetab[].
  REFRESH: nodetab.
  CASE hide.
    WHEN 'X'.
    WHEN 'U'.
      PERFORM fill_nodetab_attr USING name level space gd_refuser.
    WHEN 'K'.

      CASE flag.

        WHEN gc_attr_refuser.

          IF gd_refuser IS NOT INITIAL.
            name = gd_refuser.
            REFRESH nodetab.
            PERFORM fill_nodetab_refuser USING name level space gd_refuser.
          ELSE.
            MESSAGE s593(s#)  .
            EXIT.
          ENDIF.

        WHEN gc_attr_prof.
          name = parent.
          REFRESH nodetab.
          PERFORM fill_nodetab_all_prof USING name level space gd_refuser.


        WHEN gc_pro_own.
          name = parent.
          REFRESH nodetab.
          PERFORM fill_nodetab_user USING name level space .

        WHEN gc_pro_ref.
          name = gd_refuser.
          REFRESH nodetab.
          PERFORM fill_nodetab_user USING name level space .

      ENDCASE.

    WHEN 'P'.
      PERFORM fill_nodetab_prof USING name level space.
    WHEN 'O'.
      PERFORM fill_nodetab_obj USING parent name level.
    WHEN 'F'.
    WHEN 'V'.
    WHEN 'A'.                          "Berechtigungen
      IF parent NE space .
        ld_object = parent .
        ld_auth   = name .
        PERFORM fill_nodetab_auth USING ld_object ld_auth level.
      ENDIF.
  ENDCASE.

  DESCRIBE TABLE nodetab LINES nodetab_lin.
  IF nodetab_lin = 0 AND hide NE 'F' .
    MESSAGE s228(0h).  " ... nicht enthalten.
  ENDIF.
  IF nodetab_lin GT 0. " note 708324
    CALL FUNCTION 'RS_TREE_DELETE_NODE'
      EXPORTING
        node_id      = id
        without_root = 'X'
      EXCEPTIONS
        id_not_found = 1
        OTHERS       = 2.

    CALL FUNCTION 'RS_TREE_CONSTRUCT'
      EXPORTING
        insert_id          = id
        relationship       = 'CHILD'
      TABLES
        nodetab            = nodetab
      EXCEPTIONS
        tree_failure       = 1
        id_not_found       = 2
        wrong_relationship = 3
        OTHERS             = 4.

    CALL FUNCTION 'RS_TREE_EXPAND'
      EXPORTING
        all       = ' '
        node_id   = id
      EXCEPTIONS
        not_found = 01.
    CLEAR nodetab.
  ENDIF.


ENDFORM.                    " next_level_initial
*&---------------------------------------------------------------------*
*&      Form  fill_nodetab_attr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_nodetab_attr USING user level with_tree gd_refuser.


  TYPES: BEGIN OF type_attr,
           attr TYPE seu_text,
           flag TYPE seu_text,
         END OF type_attr.
  TYPES: type_attr_table TYPE STANDARD TABLE OF type_attr.

  DATA: next_level TYPE seu_level.

  DATA: lt_attribute TYPE type_attr_table WITH HEADER LINE.
  DATA: ls_attr TYPE type_attr.

  next_level = level.
  hidefield(1) = 'K'.

  lt_attribute-attr = 'Profile'(031).
  lt_attribute-flag = gc_attr_prof.
  APPEND lt_attribute.

  lt_attribute-attr = 'Reference user'(030).
  lt_attribute-flag = gc_attr_refuser.
  APPEND lt_attribute.

  LOOP AT lt_attribute INTO ls_attr.
    PERFORM fill_one_attribute
      USING ls_attr-attr user next_level with_tree gd_refuser ls_attr-flag.
  ENDLOOP.

  IF with_tree <> space.
    CASE lt_attribute-flag.
      WHEN gc_attr_prof.
        PERFORM fill_nodetab_all_prof USING user level with_tree gd_refuser.
      WHEN gc_attr_refuser.
        IF gd_refuser IS NOT INITIAL AND
           gd_tree_status <> 'SELO'.
          PERFORM fill_nodetab_refuser USING user next_level with_tree gd_refuser.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " fill_nodetab_attr
*&---------------------------------------------------------------------*
*&      Form  FILL_ONE_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ATTR_ATTR  text
*      -->P_USER  text
*      -->P_NEXT_LEVEL  text
*      -->P_WITH_TREE  text
*      -->P_GD_REFUSER  text
*      -->P_LS_ATTR_FLAG  text
*----------------------------------------------------------------------*
FORM fill_one_attribute  USING value(attribute) value(user) value(next_level)
                  with_tree gd_refuser flag.

  DATA: ld_key   TYPE seu_name,
        user_old TYPE  usr02-bname.

  hidefield+1 = user.
  hidefield(1) = 'K'.

  user_old = user.

  IF flag = gc_attr_prof       OR
     flag = gc_attr_refuser.

    ld_key = attribute.
    PERFORM fill_nodetab USING
            ld_key 29 col_att intsv_pro next_level
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' flag.
  ELSE.
    ld_key = attribute.
    PERFORM fill_nodetab USING
            ld_key 29 col_pro intsv_pro next_level
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' flag.
  ENDIF.

  IF with_tree <> space.

    CASE flag.

      WHEN gc_pro_own.
        PERFORM fill_nodetab_user USING user next_level with_tree.
      WHEN gc_pro_ref .
        IF gd_refuser IS NOT INITIAL.
          user = gd_refuser.
          PERFORM fill_nodetab_user USING user next_level with_tree.
        ENDIF.

    ENDCASE.

    hidefield+1 = user_old.
    hidefield(1) = 'K'.

  ENDIF.

ENDFORM.                    " FILL_ONE_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  fill_nodetab_all_prof
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ATTR_ATTR  text
*      -->P_USER  text
*      -->P_NEXT_LEVEL  text
*      -->P_WITH_TREE  text
*      -->P_GD_REFUSER  text
*      -->P_LS_ATTR_FLAG  text
*----------------------------------------------------------------------*
FORM fill_nodetab_all_prof  USING user level with_tree gd_refuser.

  TYPES: BEGIN OF type_attr,
           attr TYPE seu_text,
           flag TYPE seu_text,
         END OF type_attr.
  TYPES: type_attr_table TYPE STANDARD TABLE OF type_attr.

  DATA: next_level TYPE seu_level.

  DATA: lt_attribute TYPE type_attr_table WITH HEADER LINE.
  DATA: ls_attr TYPE type_attr.

  next_level = level + 1.

  lt_attribute-attr = 'own Profile'(033).
  lt_attribute-flag = gc_pro_own.
  APPEND lt_attribute.

  lt_attribute-attr = 'Profile of Reference user'(034).
  lt_attribute-flag = gc_pro_ref.
  APPEND lt_attribute.

  LOOP AT lt_attribute INTO ls_attr.

    PERFORM fill_one_attribute USING ls_attr-attr user next_level
                                with_tree gd_refuser ls_attr-flag.
  ENDLOOP.

ENDFORM.                    " fill_nodetab_all_prof
*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_REFUSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_USER  text
*      -->P_NEXT_LEVEL  text
*      -->P_WITH_TREE  text
*      -->P_GD_REFUSER  text
*----------------------------------------------------------------------*
FORM fill_nodetab_refuser  USING user level with_tree gd_refuser.

  DATA: next_level TYPE seu_level,
        ld_key       TYPE seu_name.

  next_level = level + 1.
  ld_key = gd_refuser.
  PERFORM fill_nodetab USING
          ld_key 12 col_pro intsv_pro next_level
          ' ' 5 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          ' ' 0 0 intsv_val
          hidefield 0 space space.

ENDFORM.                    " FILL_NODETAB_REFUSER
*&---------------------------------------------------------------------*
*&      Form  FILL_NODETAB_ATTR_SUBTREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*      -->P_LEVEL  text
*      -->P_HIDE  text
*      -->P_3636   text
*      -->P_FLAG  text
*----------------------------------------------------------------------*
FORM fill_nodetab_attr_subtree  USING user level hide with_tree flag.

  DATA: next_level  TYPE seu_level,
        ld_key      TYPE seu_name.

  hidefield(1) = 'K'.
  next_level = level + 1.

  IF hide = 'U'.

    ld_key = 'Profile'(031).
    flag = gc_attr_prof.
    PERFORM fill_nodetab USING
            ld_key 29 col_att intsv_pro next_level
            ' ' 5 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' flag.

    PERFORM fill_nodetab_all_prof USING user next_level with_tree gd_refuser.

    ld_key = 'Reference user'(030).
    flag = gc_attr_refuser.
    PERFORM fill_nodetab USING
            ld_key 29 col_att intsv_pro next_level
            ' ' 5 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            ' ' 0 0 intsv_val
            hidefield 0 'X' flag.

    IF gd_tree_status <> 'SELO' AND
       gd_refuser IS NOT INITIAL.
      PERFORM fill_nodetab_refuser USING user next_level with_tree gd_refuser.
    ENDIF.

  ENDIF.

  IF flag = gc_attr_prof.
    next_level = level + 2.
    PERFORM fill_nodetab_all_prof USING user next_level with_tree gd_refuser.
    EXIT.
  ENDIF.

ENDFORM.                    " FILL_NODETAB_ATTR_SUBTREE
