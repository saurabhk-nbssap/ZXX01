FUNCTION LOAN_CHECK_STRUCTURE_INIT.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_STRUCTURE_TABNAME) LIKE  DD02L-TABNAME
*"     VALUE(I_KEYS) TYPE  C DEFAULT ' '
*"  EXPORTING
*"     VALUE(E_BUFFERED_VALUES) TYPE  TRUXS_BUFFERED_VALUES
*"  TABLES
*"      IT_DFIES STRUCTURE  DFIES OPTIONAL
*"      T_IGNORE_FIELDS TYPE  TRUXS_ERROR_FIELDS OPTIONAL
*"  EXCEPTIONS
*"      FOREIGN_KEY_NOT_FOUND
*"      ERROR_FOUND
*"----------------------------------------------------------------------
  DATA: TMP_FORKEY_TAB LIKE IT_FORKEY_TAB OCCURS 1000 WITH HEADER LINE.
  DATA: TMP_FIELDNAME LIKE DFIES-FIELDNAME OCCURS 1000 WITH HEADER LINE.
  DATA: TMP_DD07V      LIKE DD07V OCCURS 1000 WITH HEADER LINE.
  DATA: TMP_CHECKTABLE LIKE DD05P-CHECKTABLE.
  DATA: TMP_BUFF_VALUES LIKE IT_BUFF_VALUES OCCURS 990 WITH HEADER LINE.
  DATA: TMP_CONDITION_FLAG LIKE C_FALSE,
        L_FIELDNAME(100),
        FMODULE     LIKE TFDIR-FUNCNAME.
  DATA:   BEGIN OF TMP_VALUES OCCURS 200,
               FIELDVALUES(4096) TYPE C,
          END OF TMP_VALUES.
  DATA:   BEGIN OF LT_SEL_COND OCCURS 200,
               CONDITION(72) TYPE C,
          END OF LT_SEL_COND.

* N1409243
  DATA:
   mig_type  TYPE cml_mig_buffer-mig_type,
*   tab_mig_buffer   TYPE STANDARD TABLE OF cml_mig_buffer,
   str_mig_buffer   TYPE cml_mig_buffer,
   str_dfies        TYPE dfies.

  DO 1 TIMES.
    CHECK NOT E_BUFFERED_VALUES IS SUPPLIED.

    REFRESH: IT_DFIES.

    IF i_structure_tabname+0(8) EQ  'JBIUPDAB'.
      mig_type = 'F'.
    ELSE.
      mig_type = 'M'.
    ENDIF.

* N1411334
*    READ TABLE gt_mig_buffer
*      WITH KEY mig_type = mig_type
*      TRANSPORTING NO FIELDS.
*    IF sy-subrc NE 0.
* Buffer all values
*      SELECT * FROM cml_mig_buffer INTO TABLE gt_mig_buffer.
*        WHERE mig_type EQ mig_type.
*          AND langu    EQ sy-langu.
*    ENDIF.
    IF gt_mig_buffer[]  IS INITIAL AND
       g_flg_buffer_chk NE 'X'.
      SELECT * FROM cml_mig_buffer INTO TABLE gt_mig_buffer.
      g_flg_buffer_chk = 'X'.
    ENDIF.

    CHECK NOT gt_mig_buffer[] IS INITIAL.

    LOOP AT gt_mig_buffer INTO str_mig_buffer
      WHERE tabname  = i_structure_tabname
        AND mig_type = mig_type          "N1411334
        AND langu    = sy-langu.         "N1411334
      MOVE-CORRESPONDING str_mig_buffer TO str_dfies.
      str_dfies-position = str_mig_buffer-xposition.
      str_dfies-offset   = str_mig_buffer-xoffset.
      APPEND str_dfies TO it_dfies.
    ENDLOOP.

    CHECK NOT it_dfies[] IS INITIAL.

    SORT IT_DFIES BY TABNAME FIELDNAME.  "N1602803

    RETURN.
  ENDDO.

  REFRESH IT_DFIES.
  SORT T_IGNORE_FIELDS BY TABNAME FIELDNAME.
  READ TABLE I_DFIES WITH KEY TABNAME = I_STRUCTURE_TABNAME
                     BINARY SEARCH
                     TRANSPORTING NO FIELDS.
  IF SY-SUBRC <> C_RC0.
    PERFORM DDIC_INFO_HOLEN TABLES I_DFIES
                           USING  I_STRUCTURE_TABNAME
                           CHANGING FMODULE RC.
    SORT I_DFIES BY TABNAME FIELDNAME.
    IF RC <> C_RC0.
      MESSAGE E007(E2) WITH I_STRUCTURE_TABNAME RAISING ERROR_FOUND.
    ENDIF.
  ENDIF.

* At first get possible fieldvalues
  LOOP AT I_DFIES WHERE TABNAME = I_STRUCTURE_TABNAME.
    IF T_IGNORE_FIELDS IS REQUESTED.
      READ TABLE T_IGNORE_FIELDS WITH KEY
                                      TABNAME   = I_DFIES-TABNAME
                                      FIELDNAME = I_DFIES-FIELDNAME
                                      BINARY SEARCH.
      IF SY-SUBRC = C_RC0.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR IT_BUFF_VALUES.
    IF IT_DFIES IS REQUESTED.
      APPEND I_DFIES TO IT_DFIES.
    ENDIF.
    CHECK NOT I_KEYS IS INITIAL.
*   Are values of the required buffered already?
    READ TABLE IT_BUFF_VALUES WITH KEY TABNAME   = I_DFIES-TABNAME
                                       FIELDNAME = I_DFIES-FIELDNAME
                                       BINARY SEARCH.
    CHECK SY-SUBRC <> C_RC0.
*   this is not the case. so try to find them...
    IF NOT I_DFIES-CHECKTABLE IS INITIAL.
*      check reference key
      READ TABLE IT_FORKEY_TAB WITH KEY TABNAME   = I_DFIES-TABNAME
                                        FIELDNAME = I_DFIES-FIELDNAME.
      IF SY-SUBRC <> C_RC0.
*        read data from DDIC
        CALL FUNCTION 'DD_FORKEY_GET'
             EXPORTING
                  FELDNAME  = I_DFIES-FIELDNAME
                  SPRACHE   = 'D'
                  TABNAME   = I_DFIES-TABNAME
             TABLES
                  FORKEYTAB = TMP_FORKEY_TAB
             EXCEPTIONS
                  NOT_EQUAL = C_RC0
                  OTHERS    = C_RC4.
        IF SY-SUBRC = C_RC0.
          APPEND LINES OF TMP_FORKEY_TAB TO IT_FORKEY_TAB.
          SORT IT_FORKEY_TAB BY TABNAME FIELDNAME FORTABLE FORKEY.
          REFRESH TMP_FORKEY_TAB.
        ELSE.
          CONCATENATE I_DFIES-TABNAME '-'  I_DFIES-FIELDNAME
                                           INTO L_FIELDNAME.
          MESSAGE E301(E2) WITH L_FIELDNAME
                  RAISING FOREIGN_KEY_NOT_FOUND.
        ENDIF.
      ENDIF.
      REFRESH TMP_FIELDNAME.
      CLEAR   TMP_FIELDNAME.
      CLEAR TMP_CONDITION_FLAG.
      CLEAR LT_SEL_COND.
      REFRESH LT_SEL_COND.

      READ TABLE IT_FORKEY_TAB WITH KEY TABNAME   = I_DFIES-TABNAME
                                        FIELDNAME = I_DFIES-FIELDNAME
                                        FORTABLE  = I_DFIES-TABNAME
                                        FORKEY    = I_DFIES-FIELDNAME
                                        BINARY SEARCH.
      IF SY-SUBRC = C_RC0.
        TMP_CHECKTABLE = IT_FORKEY_TAB-CHECKTABLE.
*       Determine the checktable and the checkfield for
*       the field which is considered at the moment.
        TMP_FIELDNAME = IT_FORKEY_TAB-CHECKFIELD.
        APPEND TMP_FIELDNAME.
        IF IT_FORKEY_TAB-FORTABLE+0(1) = ''''.
*       The foreign key contains restrictions to a
*       constant value for another KEY FIELD OF THE CHECKTABLE.
          IF TMP_CONDITION_FLAG = C_FALSE.
            CONCATENATE '(' IT_FORKEY_TAB-CHECKFIELD '='
                        IT_FORKEY_TAB-FORTABLE
                        INTO LT_SEL_COND-CONDITION
                        SEPARATED BY SPACE.
            APPEND LT_SEL_COND.
            TMP_CONDITION_FLAG = C_TRUE.
          ELSE.
            CONCATENATE 'AND' IT_FORKEY_TAB-CHECKFIELD '='
                        IT_FORKEY_TAB-FORTABLE
                        INTO LT_SEL_COND-CONDITION
                        SEPARATED BY SPACE.
            APPEND LT_SEL_COND.
          ENDIF.
        ENDIF.
      ENDIF.
      IF TMP_CONDITION_FLAG = C_TRUE.
        CONCATENATE LT_SEL_COND-CONDITION ')'
                    INTO LT_SEL_COND-CONDITION
                    SEPARATED BY SPACE.
        DESCRIBE TABLE LT_SEL_COND LINES SY-TABIX.
        MODIFY LT_SEL_COND INDEX SY-TABIX.
      ENDIF.
      DELETE ADJACENT DUPLICATES FROM TMP_FIELDNAME.
      SELECT (TMP_FIELDNAME) FROM (TMP_CHECKTABLE)
                          INTO TABLE TMP_VALUES
                          WHERE (LT_SEL_COND).
      IF SY-SUBRC = C_RC0.
        SORT TMP_VALUES BY FIELDVALUES.
        MOVE-CORRESPONDING I_DFIES TO IT_BUFF_VALUES.
        LOOP AT TMP_VALUES.
          MOVE-CORRESPONDING TMP_VALUES TO IT_BUFF_VALUES.
          APPEND IT_BUFF_VALUES.
        ENDLOOP.
        REFRESH TMP_VALUES.
        CLEAR TMP_VALUES.
      ENDIF.
    ELSE.
*      maybe theres fieldvalues assigned to a domain
      CALL FUNCTION 'LOAN_DOMAEN_MANAGER'
           EXPORTING
                I_DOMNAME = I_DFIES-DOMNAME
           TABLES
                T_DD07V   = TMP_DD07V
           EXCEPTIONS
                OTHERS    = C_RC4.
*      if sy-subrc = c_rc0.
      MOVE-CORRESPONDING I_DFIES TO IT_BUFF_VALUES.
      LOOP AT TMP_DD07V.
        MOVE-CORRESPONDING TMP_DD07V TO IT_BUFF_VALUES.
        APPEND IT_BUFF_VALUES.
      ENDLOOP.
      REFRESH TMP_DD07V.
*      endif.
    ENDIF.
  ENDLOOP.
  SORT IT_DFIES BY TABNAME FIELDNAME.

  CHECK NOT I_KEYS IS INITIAL.

  LOOP AT IT_BUFF_VALUES WHERE TABNAME = I_STRUCTURE_TABNAME.
    APPEND IT_BUFF_VALUES TO TMP_BUFF_VALUES.
  ENDLOOP.
  E_BUFFERED_VALUES[] = TMP_BUFF_VALUES[].
  SORT E_BUFFERED_VALUES BY TABNAME FIELDNAME.

ENDFUNCTION.
