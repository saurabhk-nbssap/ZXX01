FUNCTION NACHKOMMASTELLEN_SETZEN.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_STRUKTUR_NAME_I1)
*"     VALUE(I_STRUKTUR_NAME_I2)
*"     VALUE(I_STRUKTUR_I1)
*"  CHANGING
*"     VALUE(I_STRUKTUR_I2)
*"  EXCEPTIONS
*"      ERROR_FOUND
*"----------------------------------------------------------------------
  DATA: TMP_I1_DFIES LIKE I_DFIES .
  DATA: TMP_I2_DFIES LIKE I_DFIES OCCURS 1000 WITH HEADER LINE.

  DATA: BEGIN OF UB_DFIES OCCURS 2000.
          INCLUDE STRUCTURE DFIES.
  DATA: END   OF UB_DFIES.

  FIELD-SYMBOLS: <F_DDIC_TABLE>.
  FIELD-SYMBOLS: <F_WAERS>.
  DATA: L_CURR_DECIMALS TYPE CURRDEC.
  DATA: RC          LIKE SY-SUBRC,
        FIELDLEN    TYPE I,
        STRLEN      TYPE I.

  CONSTANTS:
    C_DATATYPE_NUMC LIKE DFIES-DATATYPE VALUE 'NUMC',
    C_DATATYPE_CURR LIKE DFIES-DATATYPE VALUE 'CURR',
    C_P_TYPE        LIKE DFIES-INTTYPE  VALUE 'P'.

  STATICS: L_BUKRS   type t001-bukrs,        "note 593354
           L_CURRDEC type tcurx-currdec.     "note 593354

* DDIC Info besorgen
  CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
       EXPORTING
            I_STRUCTURE_TABNAME = I_STRUKTUR_NAME_I1
       TABLES
            IT_DFIES            = UB_DFIES
       EXCEPTIONS
            OTHERS              = C_RC4.
  IF RC <> C_RC0.
    RAISE ERROR_FOUND.
  ENDIF.

* N1411334
  IF I_STRUKTUR_NAME_I1 EQ I_STRUKTUR_NAME_I2.
    TMP_I2_DFIES[] = UB_DFIES[].
  ELSE.
    CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
       EXPORTING
            I_STRUCTURE_TABNAME = I_STRUKTUR_NAME_I2
       TABLES
            IT_DFIES            = TMP_I2_DFIES
       EXCEPTIONS
            OTHERS              = C_RC4.
  IF RC <> C_RC0.
    RAISE ERROR_FOUND.
    ENDIF.
  ENDIF.

* Felder die Nachkommastellen beinhalten bearbeiten (Prefix: N_).
  LOOP AT UB_DFIES WHERE TABNAME = I_STRUKTUR_NAME_I1
                   AND   FIELDNAME(2) = 'N_'.
    CLEAR: FIELDLEN, STRLEN.
*   Fieldsymbol auf Feld in Zielstruktur positionieren
    READ TABLE TMP_I2_DFIES WITH KEY TABNAME = I_STRUKTUR_NAME_I2
                                     FIELDNAME = UB_DFIES-FIELDNAME+2
                                     BINARY SEARCH.
    IF SY-SUBRC = C_RC0.
      IF TMP_I2_DFIES-INTTYPE = C_P_TYPE       AND
        ( TMP_I2_DFIES-DATATYPE = C_DATATYPE_NUMC OR
          TMP_I2_DFIES-DATATYPE = C_DATATYPE_CURR ).
        FIELDLEN = TMP_I2_DFIES-LENG - TMP_I2_DFIES-DECIMALS.
      ENDIF.
      IF TMP_I2_DFIES-INTTYPE = C_P_TYPE.
       ASSIGN COMPONENT tmp_i2_dfies-position of structure i_struktur_i2
                TO <F_DDIC_TABLE> TYPE TMP_I2_DFIES-INTTYPE
                                  DECIMALS TMP_I2_DFIES-DECIMALS.
      ELSE.
       ASSIGN COMPONENT tmp_i2_dfies-position of structure i_struktur_i2
                TO <F_DDIC_TABLE> TYPE TMP_I2_DFIES-INTTYPE.
      ENDIF.
    ENDIF.
    CHECK SY-SUBRC = C_RC0.

*   fieldsymbol auf Wert der Nachkommastellen beinhaltet positionieren
    IF UB_DFIES-INTTYPE = C_P_TYPE.
      ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i1
              TO <F2> TYPE UB_DFIES-INTTYPE DECIMALS UB_DFIES-DECIMALS.
    ELSE.
      ASSIGN COMPONENT ub_dfies-position of structure i_struktur_i1
              TO <F2> TYPE UB_DFIES-INTTYPE.
    ENDIF.

    IF TMP_I2_DFIES-DATATYPE <> 'CURR'.
      CHECK: SY-SUBRC = C_RC0,
             <F2> <> SPACE,
             <F2> CO C_DARL_NUMBER,
             <F2> <> C_RC0.
    ELSE.
      CHECK: SY-SUBRC = C_RC0,
             <F2> CO C_DARL_NUMBER.
    ENDIF.
*   fieldsymbol auf umzusetzenden Wert positionieren
    READ TABLE UB_DFIES WITH KEY TABNAME = I_STRUKTUR_NAME_I1
*                                fieldname = tmp_i2_dfies-fieldname
                                 FIELDNAME = UB_DFIES-FIELDNAME+2
                                 INTO  TMP_I1_DFIES
                                 BINARY SEARCH.
    IF SY-SUBRC = C_RC0.
      IF TMP_I1_DFIES-INTTYPE = C_P_TYPE.
       ASSIGN COMPONENT tmp_i1_dfies-position of structure i_struktur_i1
                TO <F1> TYPE TMP_I1_DFIES-INTTYPE
                        DECIMALS TMP_I1_DFIES-DECIMALS.
      ELSE.
       ASSIGN COMPONENT tmp_i1_dfies-position of structure i_struktur_i1
                TO <F1> TYPE TMP_I1_DFIES-INTTYPE.
      ENDIF.
      IF UB_DFIES-INTTYPE = C_P_TYPE       AND
        ( UB_DFIES-DATATYPE = C_DATATYPE_NUMC OR
          UB_DFIES-DATATYPE = C_DATATYPE_CURR ).
        STRLEN = STRLEN( <F1> ) - <F2>.
      ENDIF.
    ENDIF.
*   Das Ausgabefeld muß größer gleich Sendefeld sein
    IF STRLEN > FIELDLEN.
      RAISE ERROR_FOUND.
    ENDIF.

    CHECK: SY-SUBRC = C_RC0,
           <F1> <> SPACE,
           <F1> <> C_RC0.

*   u.U. müssen character eliminiert werden
    IF <F1> CA '.,-'.
      PERFORM PREPARE_NUMBER USING    <F1>
                             CHANGING <F1>.
    ENDIF.
*    <f2> = <f2> - 1.
    CHECK <F1> CO C_DARL_NUMBER.
*   Anzahl Nachkommastellen hängt auch von Zielfeld ab (HW193193)
*   gilt nur für CURR-Felder (HW302920/2000)
*   Dezimalstellen der betreff. Währung beachten! (HW310946/2000)
    IF TMP_I2_DFIES-DATATYPE = C_DATATYPE_CURR.
*     zugehörigen Währungsschlüssel bestimmen ==> <F_WAERS>
      ASSIGN COMPONENT    TMP_I2_DFIES-REFFIELD
             OF STRUCTURE I_STRUKTUR_I1
             TO           <F_WAERS>.
*     Dezimalstellen der Währung bestimmen    ==> L_CURR_DECIMALS
      IF SY-SUBRC = C_RC0.
        PERFORM GET_CURRENCY_DECIMALS USING    <F_WAERS>
                                      CHANGING L_CURR_DECIMALS.
      ELSE.

* start of insertion, note 593354
         IF it_T001[] is initial.
           perform fill_it_t001.
         endif.

         IF TMP_I2_DFIES-REFTABLE eq 'T001' and
            TMP_I2_DFIES-REFFIELD EQ 'WAERS'.

            IF L_BUKRS = I_STRUKTUR_I1(4).
               L_CURR_DECIMALS = L_CURRDEC.
            ELSE.
               READ TABLE IT_T001 INTO WA_T001
                    WITH KEY BUKRS = I_STRUKTUR_I1(4).
               L_BUKRS   = WA_T001-BUKRS.
               L_CURRDEC = WA_T001-CURRDEC.
               L_CURR_DECIMALS = L_CURRDEC.      "N 739148
            ENDIF.
         ELSE.
           L_CURR_DECIMALS = 2.
         ENDIF.
* end of insertion, note 593354

      ENDIF.
*     Dezimalstellen des Zielfeldes festlegen unter Berücksichtigung
*     - der Dezimalstellen des Quellfeldes,
*     - der Dezimalstellen des Zielfeldes sowie
*     - der Dezimalstellen der betreffenden Währung
      <F2> = <F2> + TMP_I2_DFIES-DECIMALS - L_CURR_DECIMALS.
    ENDIF.
*   Umrechnen der Nachkommastellen
    <F_DDIC_TABLE> = <F1> / ( 10 ** <F2> ).
  ENDLOOP.

ENDFUNCTION.

*---------------------------------------------------------------------*
*       FORM GET_CURRENCY_DECIMALS                                    *
*---------------------------------------------------------------------*
*       get decimals of a selected currency (table TCURX)             *
*---------------------------------------------------------------------*
FORM GET_CURRENCY_DECIMALS USING    VALUE(I_WAERS)  TYPE WAERS
                           CHANGING C_CURR_DECIMALS TYPE CURRDEC.
  STATICS: L_WRK_CURR   TYPE BAPI1090_1.
  DATA:    L_WRK_RETURN TYPE BAPIRETURN.
  IF I_WAERS IS INITIAL.
    C_CURR_DECIMALS = 2.
    EXIT.
  ENDIF.
  IF I_WAERS <> L_WRK_CURR-CURRENCY.
    L_WRK_CURR-CURRENCY = I_WAERS.
    SELECT SINGLE CURRDEC FROM TCURX
           INTO L_WRK_CURR-CURDECIMALS
           WHERE  CURRKEY     = I_WAERS.
    IF SY-SUBRC <> 0.
*     default: 2 decimals
      L_WRK_CURR-CURDECIMALS = 2.
    ENDIF.
  ENDIF.
  C_CURR_DECIMALS = L_WRK_CURR-CURDECIMALS.
ENDFORM.

form FILL_IT_T001.
  select bukrs waers from t001
     into corresponding fields of table it_t001.

  loop at it_t001 into wa_t001.
    select single currdec from tcurx into wa_t001-currdec
      where currkey eq wa_t001-waers.
        if sy-subrc ne 0.
          wa_t001-currdec = 2.
        endif.
        modify it_t001 from wa_t001.
  endloop.
endform.
