*&---------------------------------------------------------------------*
*& Report  Z6XX002_PROG_DOWNLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6XX002_PROG_DOWNLOAD NO STANDARD PAGE HEADING .
*EPORT zrpt_bc_001 NO STANDARD PAGE HEADING .

*----------------------------------------------------------------------*
*  Tables                                                              *
*----------------------------------------------------------------------*
TABLES:   tadir ,
          trdir ,
          dd03l ,
          dd03m ,
          dd03t ,
          dd02t ,
          d010inc ,
          dd30l ,
          dd30t ,
          dd31s ,
          dd32s ,
          dd33s .

*----------------------------------------------------------------------*
*  Selection Screen                                                    *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-001 .

PARAMETERS:
    devclass            LIKE    tadir-devclass  ,
    dir_name(132)       OBLIGATORY .
SELECTION-SCREEN SKIP .
SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-005 .
*   Program
SELECTION-SCREEN BEGIN OF LINE .
PARAMETERS :
    r1               AS CHECKBOX . "DEFAULT 'X' .
SELECTION-SCREEN COMMENT 5(22)  text-c01 .
SELECT-OPTIONS:
    name                FOR     trdir-name .
SELECTION-SCREEN END   OF LINE .
*   Tables
SELECTION-SCREEN BEGIN OF LINE .
PARAMETERS :
    r2               AS CHECKBOX . "DEFAULT 'X' .
SELECTION-SCREEN COMMENT 5(22)  text-c02 .
SELECT-OPTIONS:
    tab_name            FOR     tadir-obj_name  MATCHCODE OBJECT ztable.
SELECTION-SCREEN END   OF LINE .
*   Scrit Forms
SELECTION-SCREEN BEGIN OF LINE .
PARAMETERS :
    r3                AS CHECKBOX . " DEFAULT 'X' .
SELECTION-SCREEN COMMENT 5(22)  text-c03 .
SELECT-OPTIONS:
    frm_name            FOR     tadir-obj_name .
SELECTION-SCREEN END   OF LINE .
*   Search Helps
SELECTION-SCREEN BEGIN OF LINE .
PARAMETERS :
    r4                AS CHECKBOX . " DEFAULT 'X' .
SELECTION-SCREEN COMMENT 5(22)  text-c04 .
SELECT-OPTIONS:
    hlp_name           FOR     tadir-obj_name .
SELECTION-SCREEN END   OF LINE .
SELECTION-SCREEN END   OF BLOCK s2 .
SELECTION-SCREEN END   OF BLOCK s1 .

*----------------------------------------------------------------------*
*  Globle Data Declaration                                             *
*----------------------------------------------------------------------*
DATA:   file_name   TYPE string ,
        path        TYPE string ,
        full_path   TYPE string ,
        text(50)    TYPE c ,
        prog_name   LIKE trdir-name .

DATA :  dynp_header    LIKE d020s .

DATA : flag_download .

DATA : file(60),
       textname LIKE stxh-tdname,
       mode(6)  TYPE c VALUE  'EXPORT',
       obj_name(40),
       object LIKE tadir-object.

DATA success_flag .

*----------------------------------------------------------------------*
*       $ :   Internal Table Declaration                               *
*----------------------------------------------------------------------*
DATA :  BEGIN OF i_repo OCCURS 100 ,
          object         LIKE   tadir-object ,
          obj_name       LIKE   tadir-obj_name ,
        END   OF i_repo .

DATA    BEGIN OF i_d020s OCCURS 100 .
INCLUDE  STRUCTURE  d020s  .
DATA    END   OF i_d020s .

DATA :  BEGIN OF i_list OCCURS 999 ,
          line(132)      TYPE   c ,
        END   OF i_list .

DATA    BEGIN OF i_textpool OCCURS 100 .
INCLUDE  STRUCTURE  textpool .
DATA    END   OF i_textpool .

DATA:   BEGIN OF dynproname,
          prog    LIKE   d020s-prog,
          dnum    LIKE   d020s-dnum,
        END OF dynproname .

DATA :  dynp_fields    LIKE d021s OCCURS 1 WITH HEADER LINE ,
        dynp_logic     LIKE d022s OCCURS 1 WITH HEADER LINE ,
        dynp_matchc    LIKE d023s OCCURS 1 WITH HEADER LINE .

**-- For Table and Views
DATA : BEGIN OF itab_table_download OCCURS 0 ,
         position     LIKE dd03l-position   ,
         fieldname    LIKE dd03l-fieldname  ,
         rollname     LIKE dd03l-rollname   ,
         domname      LIKE dd03l-domname    ,
         keyflag      LIKE dd03l-keyflag    ,
         checktable   LIKE dd03l-checktable ,
         datatype(15) TYPE c                ,
         leng         LIKE dd03l-leng       ,
         decimals     LIKE dd03l-decimals   ,
         ddtext       LIKE dd03m-ddtext     ,
       END OF itab_table_download .

DATA : BEGIN OF itab_table_header OCCURS 0 ,
         position(10)    TYPE c ,
         fieldname(10)   TYPE c ,
         rollname(10)    TYPE c ,
         domname(10)     TYPE c ,
         keyflag(10)     TYPE c ,
         checktable(11)  TYPE c ,
         datatype(10)    TYPE c ,
         leng(10)        TYPE c ,
         decimals(10)    TYPE c ,
         ddtext(11)      TYPE c ,
       END OF itab_table_header .

DATA : BEGIN OF itab_table OCCURS 0 ,
         tabname LIKE dd03l-tabname .
        INCLUDE STRUCTURE itab_table_download .
DATA : END OF itab_table .

**-- For Search Help header
DATA : BEGIN OF itab_help_basic OCCURS 0 ,
         heading(25)      TYPE   c   ,
         value(25)        TYPE   c   ,
         desc(40)         TYPE   c   ,
       END OF itab_help_basic .

DATA : BEGIN OF itab_help_header OCCURS 0 ,
         fieldname(25)    TYPE   c   ,
         flposition(25)   TYPE   c   ,
         datatype(25)     TYPE   c   ,
         leng(25)         TYPE   c   ,
         decimals(25)     TYPE   c   ,
         rollname(25)     TYPE   c   ,
         shlpinput(25)    TYPE   c   ,
         shlpoutput(25)   TYPE   c   ,
         defaultval(25)   TYPE   c   ,
         defaulttyp(25)   TYPE   c   ,
       END OF itab_help_header .

**-- For Search Help Data
DATA : BEGIN OF itab_help OCCURS 0 ,
         fieldname    LIKE   dd32s-fieldname  ,
         flposition   LIKE   dd32s-flposition ,
         datatype     LIKE   dd32s-datatype   ,
         leng         LIKE   dd32s-leng       ,
         decimals     LIKE   dd32s-decimals   ,
         rollname     LIKE   dd32s-rollname   ,
         shlpinput    LIKE   dd32s-shlpinput  ,
         shlpoutput   LIKE   dd32s-shlpoutput ,
         defaultval   LIKE   dd32s-defaultval ,
         defaulttyp   LIKE   dd32s-defaulttyp ,
       END OF itab_help .

*----------------------------------------------------------------------*
*       $ :   MACRO DEFINATION                                         *
*----------------------------------------------------------------------*
DEFINE fill_help_itab .
  select * from dd32s where shlpname eq &1 .
    move-corresponding dd32s to &2 .
    append &2 .
    clear &2 .
  endselect .
END-OF-DEFINITION .

*----------------------------------------------------------------------*
*  At Selection Screen                                                 *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
  PERFORM  at_selection_screen .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR dir_name .
  PERFORM  f4_display .


*----------------------------------------------------------------------*
*  Start of Selection                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION .

  PERFORM  get_program_names .
  PERFORM  download_table_views .
*  PERFORM  download_forms .
  PERFORM  get_search_help_data .

*----------------------------------------------------------------------*
*  End of Selection                                                    *
*----------------------------------------------------------------------*
END-OF-SELECTION .


************************************************************************
*                                                                      *
*   F o r m     R o u t i n e s     S t a r t s     H e r e            *
*                                                                      *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  At_selection_screen
*&---------------------------------------------------------------------*
FORM at_selection_screen.

  IF devclass NE space .
    SELECT SINGLE * FROM tadir WHERE pgmid  EQ 'R3TR'
                               AND   object EQ 'DEVC'
                               AND   obj_name EQ devclass .
    IF sy-subrc NE 0 .
      MESSAGE e002(sy) WITH 'Development Class Does not Exist !!!! ' .
      CLEAR devclass .
    ENDIF .
  ENDIF .

  IF r1 EQ 'X' .                " Program
    IF devclass EQ space AND name EQ space .
      MESSAGE e002(sy) WITH 'Please Enter Program Or Dev. class' .
    ENDIF .
  ENDIF.
  IF r2 EQ 'X' .            " Table
    IF devclass EQ space AND tab_name EQ space .
      MESSAGE e002(sy) WITH 'Please Enter Table Or Dev. class' .
    ENDIF .
  ENDIF .
  IF r3 EQ 'X' .            " Script Form
    IF devclass EQ space AND tab_name EQ space .
      MESSAGE e002(sy) WITH 'Please Enter Form Name Or Dev. class' .
    ENDIF .
  ENDIF .
ENDFORM.                    " At_selection_screen

*&---------------------------------------------------------------------*
*&      Form  f4_display
*&---------------------------------------------------------------------*
FORM f4_display.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
     EXPORTING
       window_title      = 'Just Select the Directory'
       default_file_name = 'Test'
       file_filter       = '*.*'
       initial_directory = 'C:\WINDOWS\Desktop'
    CHANGING
      filename          = file_name
      path              = path
      fullpath          = full_path
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3 .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE .
    dir_name = full_path .
    CONCATENATE 'Directory "' file_name '" will be created ' INTO text.
  ENDIF.
ENDFORM.                    " f4_display

*&---------------------------------------------------------------------*
*&      Form  Download_report
*&---------------------------------------------------------------------*
FORM download_report       TABLES  list     STRUCTURE  i_list
                           USING   file .
  DATA: file_name   TYPE   rlgrap-filename .        " STRING
  DATA  dtab        TYPE   STANDARD  TABLE OF line .

  dtab = list[] .
  CONCATENATE dir_name file INTO file_name SEPARATED BY '\' .
  CONCATENATE file_name '.txt' INTO file_name .

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = file_name
            filetype = 'ASC'
       TABLES
            data_tab = list
       EXCEPTIONS
            OTHERS   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CLEAR success_flag .
  ELSE .
    success_flag = 'T' .
  ENDIF.
ENDFORM.                    " Download_report

*&---------------------------------------------------------------------*
*&      Form  Get_Program_names
*&---------------------------------------------------------------------*
FORM get_program_names .
  CHECK r1  EQ 'X' .
  IF devclass EQ space .
   SELECT obj_name FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                               WHERE pgmid    EQ 'R3TR'
                                               AND   object   EQ 'PROG'
                                                AND   obj_name IN name .
  ELSE .
    SELECT object obj_name FROM tadir INTO TABLE i_repo
                               WHERE pgmid    EQ 'R3TR'
                               AND   object   IN ('PROG','FUGR')
                               AND   obj_name IN name
                               AND   devclass EQ devclass .
* Get Function Group
    LOOP AT i_repo WHERE object EQ 'FUGR' .
      CONCATENATE '%L' i_repo-obj_name '%' INTO prog_name .
      SELECT * FROM trdir WHERE name LIKE prog_name .
        i_repo-object = ' ' .
        i_repo-obj_name = trdir-name .
        APPEND i_repo .
      ENDSELECT .
    ENDLOOP .
    DELETE i_repo WHERE object EQ 'FUGR' .
  ENDIF .

  LOOP AT i_repo .
    SELECT * FROM d020s APPENDING TABLE i_d020s
                        WHERE prog EQ i_repo-obj_name
                        AND   type NE 'S' .
  ENDLOOP .

  PERFORM  download_programs .
  PERFORM  download_dynpro .
ENDFORM.                    " Get_Program_names

*&---------------------------------------------------------------------*
*&      Form  Download_programs
*&---------------------------------------------------------------------*
FORM download_programs.
  LOOP AT i_repo .
    CLEAR: i_list , i_list[] , i_textpool , i_textpool[] .
    READ REPORT i_repo-obj_name INTO i_list .
    IF sy-subrc EQ 0 .
      READ TEXTPOOL i_repo-obj_name INTO i_textpool .
      IF sy-subrc EQ 0 .
        i_list-line = text-002 .  APPEND i_list .     CLEAR i_list .
        i_list-line = text-003 .  APPEND i_list .     CLEAR i_list .
        i_list-line = text-004 .  APPEND i_list .     CLEAR i_list .
        LOOP AT i_textpool .
          i_list-line = i_textpool .
          i_list-line+100(7) = i_textpool-length .
          APPEND i_list .     CLEAR i_list .
        ENDLOOP .
      ENDIF .
      PERFORM  download_report  TABLES  i_list
                                USING   i_repo-obj_name .
      IF success_flag = 'T' .
        CONCATENATE dir_name i_repo-obj_name
                              INTO file_name SEPARATED BY '\' .
        PERFORM download_include .
        CONCATENATE file_name 'Downloaded..' INTO text .
        MESSAGE s002(sy) WITH text .
        WRITE:/ i_repo-obj_name , (4) space , 'Downloaded' .
      ELSE .
        WRITE:/ i_repo-obj_name , (4) space , 'Not Downloaded' .
      ENDIF .
    ELSE .
      WRITE:/ i_repo-obj_name , (4) space , 'Not Downloaded' .
    ENDIF .
  ENDLOOP .
ENDFORM.                    " Download_programs

*&---------------------------------------------------------------------*
*&      Form  Download_dynpro
*&---------------------------------------------------------------------*
FORM download_dynpro.
  DATA file(128) .

  LOOP AT i_d020s .
    CLEAR   : dynproname , dynp_header , dynp_fields , dynp_logic .
    REFRESH : dynp_fields , dynp_logic .

    dynproname-prog = i_d020s-prog .
    dynproname-dnum = i_d020s-dnum .

    IMPORT DYNPRO dynp_header dynp_fields dynp_logic dynp_matchc
                                                     ID dynproname .
    CONCATENATE i_d020s-prog '_' i_d020s-dnum INTO file .
    CONCATENATE dir_name file INTO file SEPARATED BY '\' .
    CONCATENATE file '.txt' INTO file .
    CONDENSE file .
    CALL FUNCTION 'Z_DYNPRO_DOWNLOAD'
         EXPORTING
              header              = dynp_header
              file                = file
         TABLES
              fields              = dynp_fields
              flowlogic           = dynp_logic
         EXCEPTIONS
              invalid_filesize    = 1
              invalid_table_width = 2
              invalid_type        = 3
              no_batch            = 4
              unknown_error       = 5
              not_executed        = 6
              OTHERS              = 7.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      WRITE:/ i_d020s-prog , i_d020s-dnum , 'Not Downloaded' .
    ELSE .
      WRITE:/ i_d020s-prog , i_d020s-dnum , 'Downloaded' .
    ENDIF.

  ENDLOOP .
ENDFORM.                    " Download_dynpro

*&---------------------------------------------------------------------*
*&      Form  Download_table_views
*&---------------------------------------------------------------------*
FORM download_table_views.
  CLEAR i_repo .
  REFRESH i_repo .
  CHECK r2 EQ 'X' .
  IF devclass EQ space .
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                    WHERE pgmid    EQ 'R3TR'
                                    AND   object   IN ('TABL', 'VIEW')
                                    AND   obj_name IN tab_name .
  ELSE .
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                    WHERE pgmid    EQ 'R3TR'
                                    AND   object   IN ('TABL', 'VIEW')
                                    AND   obj_name IN tab_name
                                    AND   devclass EQ devclass .
  ENDIF .
**-- Enter Header Data .
  REFRESH itab_table_header .
  itab_table_header-position = 'Position' .
  itab_table_header-fieldname = 'Field name' .
  itab_table_header-rollname = 'Roll Name' .
  itab_table_header-domname = 'Domain' .
  itab_table_header-keyflag  = 'Key' .
  itab_table_header-checktable  = 'Check Table' .
  itab_table_header-datatype = 'Data Type' .
  itab_table_header-leng = 'Length' .
  itab_table_header-decimals = 'Decimals' .
  itab_table_header-ddtext = 'Description' .
  APPEND itab_table_header .

  LOOP AT i_repo .
    SELECT * FROM dd03l WHERE tabname EQ i_repo-obj_name .
      MOVE dd03l-tabname    TO itab_table-tabname .
      MOVE dd03l-position   TO itab_table-position .
      MOVE dd03l-fieldname  TO itab_table-fieldname .
      MOVE dd03l-rollname   TO itab_table-rollname .
      MOVE dd03l-domname    TO itab_table-domname .
      MOVE dd03l-keyflag    TO itab_table-keyflag .
      MOVE dd03l-checktable TO itab_table-checktable .
      MOVE dd03l-datatype   TO itab_table-datatype .
      MOVE dd03l-leng       TO itab_table-leng .
      MOVE dd03l-decimals   TO itab_table-decimals .
      MOVE dd03m-ddtext     TO itab_table-ddtext .

      IF itab_table-fieldname(1) EQ '.' .
        MOVE dd03l-precfield TO itab_table-datatype .
        SELECT SINGLE * FROM dd02t
                            WHERE tabname    EQ itab_table-datatype
                            AND   ddlanguage EQ 'EN' .
        MOVE dd02t-ddtext TO itab_table-ddtext .
      ENDIF.

      PERFORM get_field_description .

      APPEND itab_table .
      CLEAR itab_table .
    ENDSELECT.
  ENDLOOP .

  SORT itab_table BY tabname position .
  CLEAR flag_download .

  LOOP AT itab_table .
    MOVE-CORRESPONDING itab_table TO itab_table_download .
    APPEND itab_table_download .
    AT END OF tabname .
      flag_download = 'X' .
    ENDAT .
    IF flag_download = 'X' .
      PERFORM download_table .
      CLEAR flag_download .
      REFRESH itab_table_download .
      IF success_flag = 'T' .
        WRITE:/ itab_table-tabname , 'Downloaded'.
      ELSE .
        WRITE:/ itab_table-tabname , 'Not Downloaded'.
      ENDIF .
    ENDIF .
    CLEAR itab_table .
  ENDLOOP .
ENDFORM.                    " Download_table_views

*&---------------------------------------------------------------------*
*&      Form  get_field_description
*&---------------------------------------------------------------------*
FORM get_field_description.
  IF itab_table-rollname NE space .
    SELECT SINGLE * FROM dd03m WHERE tabname EQ itab_table-tabname
                               AND   fieldname EQ itab_table-fieldname
                               AND   position  EQ itab_table-position
                               AND   ddlanguage EQ 'EN' .
    IF sy-subrc EQ 0 .
      MOVE dd03m-ddtext TO itab_table-ddtext .
    ENDIF .
  ELSE .
    SELECT SINGLE * FROM dd03t WHERE tabname EQ itab_table-tabname
                               AND   fieldname EQ itab_table-fieldname
                               AND   ddlanguage EQ 'EN' .
    IF sy-subrc EQ 0 .
      MOVE dd03t-ddtext TO itab_table-ddtext .
    ENDIF .
  ENDIF .
ENDFORM.                    " get_field_description

*&---------------------------------------------------------------------*
*&      Form  download_table
*&---------------------------------------------------------------------*
FORM download_table.
  DATA : file_name LIKE rlgrap-filename .
  file_name  = itab_table-tabname .
  CONCATENATE dir_name file_name INTO file_name SEPARATED BY '\'.
  CONCATENATE file_name '.XLS' INTO file_name .
**-- Download table header
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = file_name
            filetype = 'DAT'
       TABLES
            data_tab = itab_table_header
       EXCEPTIONS
            OTHERS   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CLEAR success_flag .
  ELSE .
    success_flag = 'T' .
  ENDIF .

**-- Download table Detail
  IF success_flag = 'T' .
    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              filename = file_name
              filetype = 'DAT'
              mode     = 'A'
         TABLES
              data_tab = itab_table_download.
  ENDIF .
ENDFORM.                    " download_table

*&---------------------------------------------------------------------*
*&      Form  download_forms
*&---------------------------------------------------------------------*
FORM download_forms.
  DATA : file_name LIKE rlgrap-filename .
  CLEAR i_repo .
  REFRESH i_repo .
  CHECK r3 EQ 'X' .
  IF devclass EQ space .
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                    WHERE pgmid    EQ 'R3TR'
                                    AND   object   IN ('FORM')
                                    AND   obj_name IN frm_name .
  ELSE .
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                    WHERE pgmid    EQ 'R3TR'
                                    AND   object   IN ('FORM')
                                    AND   obj_name IN frm_name
                                    AND   devclass EQ devclass .
  ENDIF .
  LOOP AT i_repo .
    CLEAR file_name .
    CONCATENATE dir_name i_repo-obj_name INTO file_name
                                           SEPARATED BY '\' .
    CONCATENATE file_name '.TXT' INTO file_name .
    object = 'FORM' .
    obj_name = i_repo-obj_name .

    PERFORM rstxscrp(zrstxr3tr) USING object
                                      obj_name
                                      mode
                                      file_name '' '' '' 'X' '' .
  ENDLOOP .
ENDFORM.                    " download_forms

*&---------------------------------------------------------------------*
*&      Form  download_include
*&---------------------------------------------------------------------*
FORM download_include.
  DATA : BEGIN OF itab_include OCCURS 0 ,
           include LIKE d010inc-include ,
         END OF itab_include .
  DATA : f_name LIKE rlgrap-filename .
  DATA : include_path LIKE rlgrap-filename .

  SELECT * FROM d010inc
                    INTO CORRESPONDING FIELDS OF TABLE itab_include
                    WHERE master EQ  i_repo-obj_name.

  DELETE itab_include WHERE include(1) NE 'Z'
                      AND   include(1) NE 'Y'
                      AND   include(2) NE 'MZ'
                      AND   include(2) NE 'MY'
                      AND   include(5) NE 'SAPMZ'
                      AND   include(5) NE 'SAPMY' .

  CLEAR: i_list , i_list[] .
  IF NOT itab_include[] IS INITIAL .
    CONCATENATE dir_name i_repo-obj_name INTO include_path
                                          SEPARATED BY '\' .
    CALL FUNCTION 'GUI_CREATE_DIRECTORY'
         EXPORTING
              dirname = include_path
         EXCEPTIONS
              failed  = 1
              OTHERS  = 2.
    LOOP AT itab_include .
      READ REPORT itab_include-include INTO i_list .
      IF sy-subrc EQ 0 .
        CONCATENATE include_path itab_include-include
                                               INTO f_name
                                               SEPARATED BY '\' .
        CONCATENATE f_name '.txt' INTO f_name .
        CALL FUNCTION 'WS_DOWNLOAD'
             EXPORTING
                  filename = f_name
                  filetype = 'ASC'
             TABLES
                  data_tab = i_list
             EXCEPTIONS
                  OTHERS   = 10.

      ENDIF.
    ENDLOOP .
  ENDIF .
ENDFORM. " download_include

*&---------------------------------------------------------------------*
*&      Form  download_help
*&---------------------------------------------------------------------*
FORM get_search_help_data .
  DATA : sub_help LIKE dd31s-subshlp .
  CLEAR i_repo .
  REFRESH i_repo .
  CHECK r4 EQ 'X' .
  PERFORM get_help_header_data .
  IF devclass EQ space .
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                    WHERE pgmid    EQ 'R3TR'
                                    AND   object   EQ 'SHLP'
                                    AND   obj_name IN hlp_name .
  ELSE .
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE i_repo
                                    WHERE pgmid    EQ 'R3TR'
                                    AND   object   EQ 'SHLP'
                                    AND   obj_name IN hlp_name
                                    AND   devclass EQ devclass .
  ENDIF .
  LOOP AT i_repo .
    SELECT SINGLE * FROM dd30l WHERE shlpname EQ i_repo-obj_name .
    IF sy-subrc EQ 0 .
      REFRESH itab_help_basic .
      PERFORM fill_help_basic_itab .
      fill_help_itab i_repo-obj_name itab_help .
      PERFORM download_help .
      IF success_flag = 'T' .
        WRITE :/ i_repo-obj_name , 'Downloded' .
      ELSE .
        WRITE :/ i_repo-obj_name , 'Not Downloded' .
      ENDIF .
      CLEAR itab_help .
      REFRESH itab_help .
    ENDIF .
  ENDLOOP .
ENDFORM.                    " download_help

*&---------------------------------------------------------------------*
*&      Form  download_help
*&---------------------------------------------------------------------*
FORM download_help.
  DATA : file_name LIKE rlgrap-filename .
  CONCATENATE dir_name i_repo-obj_name INTO file_name
                                      SEPARATED BY '\' .

  CONCATENATE file_name '.XLS' INTO file_name .
**-- Download Basic Detail
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = file_name
            filetype = 'DAT'
       TABLES
            data_tab = itab_help_basic
       EXCEPTIONS
            OTHERS   = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CLEAR success_flag .
  ELSE .
    success_flag = 'T' .
  ENDIF .

  IF success_flag = 'T' .
**-- Download Header Detail
    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              filename = file_name
              filetype = 'DAT'
              mode     = 'A'
         TABLES
              data_tab = itab_help_header.

**-- Download Data
    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              filename = file_name
              filetype = 'DAT'
              mode     = 'A'
         TABLES
              data_tab = itab_help.
  ENDIF .
ENDFORM.                    " download_help

*&---------------------------------------------------------------------*
*&      Form  get_help_header_data
*&---------------------------------------------------------------------*
FORM get_help_header_data.
  CLEAR itab_help_header .
  itab_help_header-fieldname  = 'Parameter Field' .
  itab_help_header-flposition = 'Position' .
  itab_help_header-datatype   = 'Data Type' .
  itab_help_header-leng       = 'Length' .
  itab_help_header-decimals   = 'Decimals' .
  itab_help_header-rollname   = 'Roll Name' .
  itab_help_header-shlpinput  = 'Import' .
  itab_help_header-shlpoutput = 'Export' .
  itab_help_header-defaultval = 'Default value' .
  itab_help_header-defaulttyp = 'Default Type' .
  APPEND itab_help_header .
ENDFORM.                    " get_help_header_data

*&---------------------------------------------------------------------*
*&      Form  fill_help_basic_itab
*&---------------------------------------------------------------------*
FORM fill_help_basic_itab.

  IF dd30l-issimple = 'X' .
    MOVE 'Ele. / Coll.'  TO itab_help_basic-heading .
    MOVE 'Elementary Help' TO itab_help_basic-value .
    APPEND itab_help_basic .
    CLEAR itab_help_basic .

    MOVE 'Cat. of Selection Method' TO itab_help_basic-heading .
    MOVE dd30l-selmtype TO itab_help_basic-value .
    CASE dd30l-selmtype .
      WHEN 'T' .
        MOVE 'Selection from Table'
                               TO itab_help_basic-desc .
      WHEN 'X' .
        MOVE 'Selection from Table with Text Table'
                                 TO itab_help_basic-desc .
      WHEN 'V' .
        MOVE 'Selection from DB or Projection View'
                               TO itab_help_basic-desc .
      WHEN 'H' .
        MOVE 'Selection with a Help View'
                               TO itab_help_basic-desc .
      WHEN 'F' .
        MOVE 'Selection by Function Module'
                               TO itab_help_basic-desc .
    ENDCASE .
    APPEND itab_help_basic .
    CLEAR itab_help_basic .

    MOVE 'Selection Method' TO itab_help_basic-heading .
    MOVE dd30l-selmethod TO itab_help_basic-value .
    APPEND itab_help_basic .
    CLEAR itab_help_basic .

    MOVE 'Hotkey' TO itab_help_basic-heading .
    MOVE dd30l-hotkey TO itab_help_basic-value .
    APPEND itab_help_basic .
    CLEAR itab_help_basic .

    MOVE 'Dialog Type' TO itab_help_basic-heading .
    MOVE dd30l-dialogtype TO itab_help_basic-value .
    CASE dd30l-dialogtype .
      WHEN 'A' .
        MOVE 'Dialog depends on set of values'
                               TO itab_help_basic-desc .
      WHEN 'D' .
        MOVE 'Display values immediately'
                               TO itab_help_basic-desc .
      WHEN 'C' .
        MOVE 'Dialog with value restriction'
                               TO itab_help_basic-desc .
    ENDCASE .
    APPEND itab_help_basic .
    CLEAR itab_help_basic .

  ELSE .
    MOVE 'Ele.(X) / Coll.'  TO itab_help_basic-heading .
    MOVE 'Collective Help' TO itab_help_basic-value .
    APPEND itab_help_basic .

    MOVE 'Search help Included' TO itab_help_basic-heading.
    SELECT * FROM dd31s WHERE shlpname EQ i_repo-obj_name .
      MOVE dd31s-subshlp TO itab_help_basic-value .
      APPEND itab_help_basic .
      CLEAR itab_help_basic .
    ENDSELECT .
  ENDIF .
  APPEND itab_help_basic .
ENDFORM.                    " fill_help_basic_itab



*I001     Download Parameters
*I005     Selection
*IC01     Programs
*IC02     Tables & View
*IC03     Forms
*IC04     Search Helps
*R        Download Programs, Tables, Views, Search Help and Forms
*SDEVCLASSD       Development class
*SDIR_NAME        Directory Path
*SR1              Programs
*SR2              Tables & View
*SR3              Forms
*SR4              Search Help
