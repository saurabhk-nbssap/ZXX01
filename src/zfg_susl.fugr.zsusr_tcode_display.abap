FUNCTION ZSUSR_TCODE_DISPLAY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NAME) LIKE  USOBT-NAME DEFAULT 'SU01'
*"     VALUE(TYPE) LIKE  USOBT-TYPE DEFAULT 'TR'
*"  EXPORTING
*"     REFERENCE(EV_TYPE) LIKE  USOBT-TYPE
*"     REFERENCE(EV_NAME) LIKE  USOBT-NAME
*"     REFERENCE(EV_TEXT) LIKE  TSTCT-TTEXT
*"  TABLES
*"      GT_INTUSOBT STRUCTURE  USOBT_C
*"--------------------------------------------------------------------



************************************************************************
* BEGIN OF ALV MIGRATION DECLARATIONS     C5056319*
************************************************************************
TYPES: BEGIN OF INTUSOBT_NEW_TEMP,
       OBJECT TYPE USOBT_C-OBJECT,
       FIELD  TYPE  USOBT_C-FIELD,
       LOW    TYPE  USOBT_C-LOW,
       HIGH   TYPE  USOBT_C-HIGH,
       END OF INTUSOBT_NEW_TEMP.


DATA: GS_INTUSOBT_NEW1 TYPE INTUSOBT_NEW_TEMP.


************************************************************************
* END OF ALV MIGRATION DECLARATIONS  C5056319
************************************************************************


  DATA: OLDOBJ LIKE TOBJ-OBJCT,
        OLD_OBJ LIKE TOBJ-OBJCT,
        OLDFLD LIKE TOBJ-FIEL0,
        OLD_FLD LIKE TOBJ-FIEL0,
        Z      TYPE I VALUE 0.
  DATA POS TYPE P.
*
  SET PF-STATUS 'LSTO'.
  SET TITLEBAR  'LSO'.
  SKIP.

  EV_TYPE = TYPE.


***********************************************************
* START OF COMMENTING        C5056319
***********************************************************

*  CASE TYPE.
*    WHEN TYPE_TCODE.
*      WRITE: 'Transaktion'(002) COLOR COL_HEADING INVERSE.
*    WHEN TYPE_REPORT.
*      WRITE: 'Report'(008) COLOR COL_HEADING INVERSE.
*    WHEN TYPE_FB.
*      WRITE: 'Funktionsbaustein'(009) COLOR COL_HEADING INVERSE.
*    WHEN TYPE_REUSE.
*      WRITE: 'Objektgruppe'(023) COLOR COL_HEADING INVERSE.
*  ENDCASE.
*  WRITE: 20 NAME COLOR COL_HEADING INVERSE.

******************************************************************
* END OF COMMENTING           C5056319
******************************************************************



******************START OF ALV MIGRATIONS    C5056319 **************
  EV_NAME = NAME.
*******************END OF ALV MIGRATIONS     C5056319 **************


  CLEAR TSTCT.
  SELECT SINGLE * FROM TSTCT
         WHERE SPRSL = SY-LANGU
         AND   TCODE = NAME.

***********************************************************
* START OF COMMENTING        C5056319
***********************************************************
*  WRITE: 26 TSTCT-TTEXT COLOR COL_HEADING INVERSE.
***********************************************************
* START OF COMMENTING        C5056319
***********************************************************

******************START OF ALV MIGRATIONS    C5056319 **************
EV_TEXT = TSTCT-TTEXT.
*******************START OF ALV MIGRATIONS    C5056319 **************

*  SKIP.
*  POS = 61 + 2 * C_VALUE_LONGER.
*  ULINE /(POS).
**  position /(pos).
*  WRITE  AT  /1(POS) SPACE COLOR COL_HEADING INTENSIFIED.
*"
*  WRITE 1  SY-VLINE.
*  WRITE: 2 'Objekt'(003) COLOR COL_HEADING INTENSIFIED.
*  WRITE 12 SY-VLINE.
*  WRITE: 13 'Feld'(004) COLOR COL_HEADING INTENSIFIED.
*  WRITE 23 SY-VLINE.
*  WRITE: 24 'Wert von'(005) COLOR COL_HEADING INTENSIFIED.
*  POS = 42 + C_VALUE_LONGER. POSITION POS.
*  WRITE    SY-VLINE.
*  POS = 43 + C_VALUE_LONGER. POSITION POS.
*  WRITE: 'Wert bis'(006) COLOR COL_HEADING INTENSIFIED.
*  POS = 61 + 2 * C_VALUE_LONGER. POSITION POS.
*  WRITE  SY-VLINE.

***********************************************************
* END OF COMMENTING        C5056319
***********************************************************
*  4.0B/C  Umstellung USOBT_C statt USOBT  MS 15.12.97
  SELECT * FROM USOBT_C INTO TABLE INTUSOBT
         WHERE TYPE = TYPE
         AND   NAME = NAME.
  IF TYPE = TYPE_TCODE.
    SELECT * FROM USOBT_C INTO INTUSOBT
           WHERE TYPE = 'T'
           AND   NAME = NAME.
      INTUSOBT-TYPE = TYPE_TCODE.
      APPEND INTUSOBT.
    ENDSELECT.
  ELSEIF TYPE = TYPE_REPORT.
    SELECT * FROM USOBT_C INTO INTUSOBT
           WHERE TYPE = 'R'
           AND   NAME = NAME.
      INTUSOBT-TYPE = TYPE_REPORT.
      APPEND INTUSOBT.
    ENDSELECT.
  ENDIF.
  CLEAR OLDOBJ.

  LOOP AT INTUSOBT.

    IF OLDOBJ <> INTUSOBT-OBJECT.


***********************************************************
* START OF COMMENTING        C5056319
***********************************************************


*      NEW-LINE.
*      POS = 61 + 2 * C_VALUE_LONGER.
*      ULINE AT (POS).
*      NEW-LINE.

*      WRITE: AT 1(POS) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
*
*      WRITE 1 SY-VLINE.
*      WRITE: 2 INTUSOBT-OBJECT COLOR COL_KEY INTENSIFIED ON.

***********************************************************
* END OF COMMENTING         C5056319
***********************************************************


**************************************************************
* START OF ALV MIGRATIONS   C5056319
**************************************************************
  MOVE INTUSOBT-OBJECT TO GS_INTUSOBT_NEW1-OBJECT.
  MOVE-CORRESPONDING GS_INTUSOBT_NEW1 TO GT_INTUSOBT.
**************************************************************
* END OF ALV MIGRATIONS     C5056319
**************************************************************

      OLDOBJ = INTUSOBT-OBJECT.

       CLEAR OLDFLD.
    ELSE.

***********************************************************
* START OF COMMENTING        C5056319
***********************************************************
*      NEW-LINE.
*      POS = 61 + 2 * C_VALUE_LONGER.

*      WRITE: AT 1(POS) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
*"
*      WRITE 1 SY-VLINE.
*      WRITE: 2(10) SPACE COLOR COL_KEY INTENSIFIED ON.

***********************************************************
* END  OF COMMENTING        C5056319
***********************************************************
   ENDIF.

***********************************************************
* START OF COMMENTING        C5056319
***********************************************************

*    WRITE 12 SY-VLINE.
*    WRITE: 13 INTUSOBT-FIELD COLOR COL_NORMAL INTENSIFIED OFF.


***********************************************************
* END OF COMMENTING             C5056319
***********************************************************


      IF OLDFLD <> INTUSOBT-FIELD.

***********************************************************
* START OF ALV MIGRATIONS        C5056319
***********************************************************
      MOVE INTUSOBT-FIELD TO GS_INTUSOBT_NEW1-FIELD.
      MOVE-CORRESPONDING GS_INTUSOBT_NEW1 TO GT_INTUSOBT.
***********************************************************
*  END  OF ALV MIGRATIONS         C5056319
***********************************************************

        OLDFLD = INTUSOBT-FIELD.


    ENDIF.



*    WRITE 23 SY-VLINE.
*    WRITE: 24 INTUSOBT-LOW COLOR COL_NORMAL INTENSIFIED OFF.

***********************************************************
* END OF COMMENTING              C5056319
***********************************************************


***********************************************************
* START OF ALV MIGRATIONS        C505631
***********************************************************

     MOVE INTUSOBT-LOW TO GS_INTUSOBT_NEW1-LOW.
     MOVE-CORRESPONDING GS_INTUSOBT_NEW1 TO GT_INTUSOBT.

***********************************************************
*  END  OF ALV MIGRATIONS        C5056319
***********************************************************

    CLEAR INTUSOBT.

***********************************************************
* START OF COMMENTING           C5056319
***********************************************************

*    POS = 42 + C_VALUE_LONGER. POSITION POS.
*    WRITE  SY-VLINE.
*    POS = 43 + C_VALUE_LONGER. POSITION POS.
*    WRITE:  INTUSOBT-HIGH COLOR COL_NORMAL INTENSIFIED OFF.

***********************************************************
* END OF COMMENTING        C5056319
***********************************************************



***********************************************************
*  START  OF ALV MIGRATIONS        C5056319
***********************************************************

  MOVE INTUSOBT-HIGH TO GS_INTUSOBT_NEW1-HIGH.
  MOVE-CORRESPONDING GS_INTUSOBT_NEW1 TO GT_INTUSOBT.


*      IF OLD_FLD = GT_INTUSOBT-FIELD.
*       OLD_FLD  =   GT_INTUSOBT-FIELD.
*      CLEAR GT_INTUSOBT-FIELD.
*
*      ELSE.
*
*      OLD_FLD = GT_INTUSOBT-FIELD.
*
*      ENDIF.
*
*      IF OLD_OBJ = GT_INTUSOBT-OBJECT.
*       OLD_OBJ  =   GT_INTUSOBT-OBJECT.
*      CLEAR GT_INTUSOBT-OBJECT.
*
*      ELSE .
*
*      OLD_OBJ = GT_INTUSOBT-OBJECT.
*
*      ENDIF.

      APPEND GT_INTUSOBT.


***********************************************************
*  END  OF ALV MIGRATIONS        C5056319
***********************************************************

***********************************************************
* START OF COMMENTING           C5056319
***********************************************************
*    POS = 61 + 2 * C_VALUE_LONGER. POSITION POS.
*    WRITE  SY-VLINE.

***********************************************************
* END OF COMMENTING           C5056319
***********************************************************


  ENDLOOP.

***********************************************************
* START OF COMMENTING           C5056319
***********************************************************
*  POS = 61 + 2 * C_VALUE_LONGER.
*  ULINE /(POS).
*
*    IF Z > 0.
*    ULINE /(POS).
*
*    endif.
***********************************************************
* END OF COMMENTING           C5056319
***********************************************************



************ start of Alv migrations    C5056319***********

PERFORM DETAILS_LIST USING Ev_type Ev_name Ev_text gt_intusobT[].

************ end  of Alv migrations    C5056319***********
ENDFUNCTION.










* Zugriff auf USOTT auskommentiert, HW 594300
*  SELECT * FROM USOTT INTO TABLE INTUSOTT
*         WHERE TYPE = TYPE
*         AND   NAME = NAME.
*  LOOP AT INTUSOTT.
*    POS = 61 + 2 *  C_VALUE_LONGER.
*    WRITE AT /(POS)  SPACE COLOR COL_HEADING INTENSIFIED.
*    WRITE 1  SY-VLINE.
*    WRITE: 2 'Objektgruppe'(023) COLOR COL_HEADING INTENSIFIED.
*    WRITE: 20 INTUSOTT-SUBNAME COLOR COL_HEADING INTENSIFIED.
*    WRITE 61 SY-VLINE.
*    POS = 61 + 2 * C_VALUE_LONGER.
*    ULINE /(POS).
*    POS = 61 + 2 *  C_VALUE_LONGER.
*    WRITE  AT  /1(POS) SPACE COLOR COL_HEADING INTENSIFIED.
*    WRITE 1  SY-VLINE.
*    WRITE: 2 'Objekt'(003) COLOR COL_HEADING INTENSIFIED.
*    WRITE 12 SY-VLINE.
*    WRITE: 13 'Feld'(004) COLOR COL_HEADING INTENSIFIED.
*    WRITE 23 SY-VLINE.
*    WRITE: 24 'Wert von'(005) COLOR COL_HEADING INTENSIFIED.
*    POS = 42 + C_VALUE_LONGER. POSITION POS.
*    WRITE 42 SY-VLINE.
*    POS = 43 + C_VALUE_LONGER. POSITION POS.
*    WRITE: 43 'Wert bis'(006) COLOR COL_HEADING INTENSIFIED.
*    POS = 61 + 2 * C_VALUE_LONGER. POSITION POS.
*    WRITE SY-VLINE.
*    SELECT * FROM USOBT INTO TABLE INTUSOBT
*           WHERE TYPE = TYPE_REUSE
*           AND   NAME = INTUSOTT-SUBNAME.
*    CLEAR OLDOBJ.
*    LOOP AT INTUSOBT.
*      IF OLDOBJ <> INTUSOBT-OBJECT.
*        NEW-LINE.
*        POS = 61 + 2 * C_VALUE_LONGER.
*        ULINE AT (POS).
*        NEW-LINE.
*        WRITE: AT 1(POS) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
*        WRITE 1 SY-VLINE.
*        WRITE: 2 INTUSOBT-OBJECT COLOR COL_KEY INTENSIFIED ON.
*        OLDOBJ = INTUSOBT-OBJECT.
*        CLEAR OLDFLD.
*      ELSE.
*        NEW-LINE.
*        POS = 61 + 2 *  C_VALUE_LONGER.
*        WRITE: AT 1(POS) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
*        WRITE 1 SY-VLINE.
*        WRITE: 2(10) SPACE COLOR COL_KEY INTENSIFIED ON.
*      ENDIF.
*      WRITE 12 SY-VLINE.
*      IF OLDFLD <> INTUSOBT-FIELD.
*        WRITE: 13 INTUSOBT-FIELD COLOR COL_NORMAL INTENSIFIED OFF.
*        OLDFLD = INTUSOBT-FIELD.
*      ENDIF.
*      WRITE 23 SY-VLINE.
*      WRITE: 24 INTUSOBT-LOW COLOR COL_NORMAL INTENSIFIED OFF.
*      POS = 42 + C_VALUE_LONGER. POSITION POS.
*      WRITE  SY-VLINE.
*      POS = 43 + C_VALUE_LONGER. POSITION POS.
*      WRITE:  INTUSOBT-HIGH COLOR COL_NORMAL INTENSIFIED OFF.
*      POS = 61 + 2 * C_VALUE_LONGER. POSITION POS.
*      WRITE  SY-VLINE.
*    ENDLOOP.
*    POS = 61 + 2 * C_VALUE_LONGER.
*    ULINE /(POS).
*  ENDLOOP.
