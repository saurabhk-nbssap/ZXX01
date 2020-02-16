FUNCTION LOAN_CHECK_DOMAENS_STRUCTURE.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_STRUKTUR_NAME_I1)
*"     VALUE(I_STRUKTUR)
*"  TABLES
*"      T_DOMAEN_FIELDS STRUCTURE  DFIES
*"  EXCEPTIONS
*"      ERROR_FOUND
*"----------------------------------------------------------------------
  DATA: TMP_I_DFIES LIKE I_DFIES OCCURS 1000 WITH HEADER LINE.
  DATA:
* allgemeine Variable
        DOMNAME     LIKE DD07V-DOMNAME,
        RC          LIKE SY-SUBRC.

* DDIC Info besorgen
  CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
    EXPORTING
      I_STRUCTURE_TABNAME = I_STRUKTUR_NAME_I1
    TABLES
      IT_DFIES            = TMP_I_DFIES
    EXCEPTIONS
      OTHERS              = C_RC4.
  IF RC <> C_RC0.
    RAISE ERROR_FOUND.
  ENDIF.
* Felder kopieren
  SORT TMP_I_DFIES  BY TABNAME VALEXI DESCENDING.
*  sort ub_dfies by fieldname.

  LOOP AT TMP_I_DFIES WHERE TABNAME = I_STRUKTUR_NAME_I1
                      AND   VALEXI  = C_TRUE.
*   Originalfeld lesen
    CLEAR SY-SUBRC.
*    ASSIGN I_STRUKTUR+TMP_I_DFIES-OFFSET(TMP_I_DFIES-INTLEN)
    ASSIGN COMPONENT tmp_I_dfies-position of structure i_struktur
            TO <F1> TYPE TMP_I_DFIES-INTTYPE.

    CHECK: SY-SUBRC = C_RC0,
           NOT <F1> IS INITIAL.
    DOMNAME = TMP_I_DFIES-DOMNAME.
    PERFORM FIND_DOMAEN_VALUE USING    DOMNAME <F1>
                              CHANGING SY-SUBRC.
    CHECK SY-SUBRC <> C_RC0.
    MOVE-CORRESPONDING TMP_I_DFIES TO T_DOMAEN_FIELDS.
    T_DOMAEN_FIELDS-FIELDTEXT = <F1>.
*    read table t_domaen_fields.
*    check sy-subrc <> c_rc0.
    APPEND T_DOMAEN_FIELDS.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM T_DOMAEN_FIELDS.
ENDFUNCTION.
