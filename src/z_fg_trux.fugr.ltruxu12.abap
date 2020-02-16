FUNCTION CHECK_REQUIRED_FIELDS.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_STRUKTUR_NAME_I1)
*"     VALUE(I_STRUKTUR)
*"     VALUE(I_MODUS) LIKE  TP105-AKTYP DEFAULT 01
*"     VALUE(I_TYPE) LIKE  BP000-TYPE OPTIONAL
*"     VALUE(I_INIT_TABLE) TYPE  ANY DEFAULT SPACE
*"  TABLES
*"      T_MISSING_FIELDS STRUCTURE  DFIES
*"  EXCEPTIONS
*"      ERROR_FOUND
*"----------------------------------------------------------------------
  DATA: TMP_I_DFIES LIKE I_DFIES OCCURS 0 WITH HEADER LINE.

  DATA:
        RC          LIKE SY-SUBRC.
  DATA:    BEGIN OF REQU_FIELDS_TEMP OCCURS 10.
          INCLUDE STRUCTURE TPZ3R.
  DATA:    END   OF REQU_FIELDS_TEMP.
*  data:    begin of opt_fields_temp occurs 10.
*          include structure tpz3r.
*  data:    end   of opt_fields_temp.

  IF NOT I_INIT_TABLE IS INITIAL.
    REFRESH UB_REQ_FIELDS.
  ENDIF.
* DDIC Info u.U. besorgen
  CASE I_TYPE.
    WHEN C_TYPE_NAT.
      IF I_MODUS = C_ACT_CREA.
        DESCRIBE TABLE REQU_FIELDS_A LINES LINES_ITAB.
        IF LINES_ITAB = C_RC0.
          PERFORM GET_REQUESTED_FIELDS TABLES REQU_FIELDS_A
                                              OPT_FIELDS_A
                                       USING  I_TYPE I_MODUS
                              CHANGING RC.
        ENDIF.
        REQU_FIELDS_TEMP[] = REQU_FIELDS_A[].
*        opt_fields_temp[]  = opt_fields_a[].
      ELSE.
        DESCRIBE TABLE REQU_FIELDS_B LINES LINES_ITAB.
        IF LINES_ITAB = C_RC0.
          PERFORM GET_REQUESTED_FIELDS TABLES REQU_FIELDS_B
                                              OPT_FIELDS_B
                                       USING  I_TYPE I_MODUS
                              CHANGING RC.
        ENDIF.
        REQU_FIELDS_TEMP[] = REQU_FIELDS_B[].
*        opt_fields_temp[]  = opt_fields_b[].
      ENDIF.
    WHEN C_TYPE_ORG.
      IF I_MODUS = C_ACT_CREA.
        DESCRIBE TABLE REQU_FIELDS_C LINES LINES_ITAB.
        IF LINES_ITAB = C_RC0.
          PERFORM GET_REQUESTED_FIELDS TABLES REQU_FIELDS_C
                                              OPT_FIELDS_C
                                       USING  I_TYPE I_MODUS
                              CHANGING RC.
        ENDIF.
        REQU_FIELDS_TEMP[] = REQU_FIELDS_C[].
*        opt_fields_temp[]  = opt_fields_c[].
      ELSE.
        DESCRIBE TABLE REQU_FIELDS_D LINES LINES_ITAB.
        IF LINES_ITAB = C_RC0.
          PERFORM GET_REQUESTED_FIELDS TABLES REQU_FIELDS_D
                                              OPT_FIELDS_D
                                       USING  I_TYPE I_MODUS
                              CHANGING RC.
        ENDIF.
        REQU_FIELDS_TEMP[] = REQU_FIELDS_D[].
*        opt_fields_temp[]  = opt_fields_d[].
      ENDIF.
    WHEN OTHERS.
                                       "do nothing.
  ENDCASE.

  READ TABLE UB_REQ_FIELDS WITH KEY TABNAME = I_STRUKTUR_NAME_I1
                                    BINARY SEARCH.
  IF SY-SUBRC <> C_RC0.
    PERFORM MUSSFELD_INFO_HOLEN TABLES UB_REQ_FIELDS
                                       REQU_FIELDS_TEMP
                                USING  I_MODUS I_STRUKTUR_NAME_I1.
  ENDIF.
* DDIC Info besorgen
  CALL FUNCTION 'LOAN_CHECK_STRUCTURE_INIT'
       EXPORTING
            I_STRUCTURE_TABNAME = I_STRUKTUR_NAME_I1
       TABLES
            IT_DFIES            = TMP_I_DFIES
       EXCEPTIONS
            OTHERS              = C_RC4.

* Field-symbol auf STYPE setzen
  READ TABLE TMP_I_DFIES BINARY SEARCH
                         WITH KEY TABNAME = I_STRUKTUR_NAME_I1
                                FIELDNAME = 'STYPE'.
  IF SY-SUBRC <> C_RC0.
    CLEAR <F2>.
  ELSE.
    ASSIGN COMPONENT tmp_i_dfies-position of structure i_struktur
           TO <F2> TYPE TMP_I_DFIES-INTTYPE.
    IF SY-SUBRC <> C_RC0.
      CLEAR <F2>.
    ENDIF.
  ENDIF.

  LOOP AT UB_REQ_FIELDS WHERE TABNAME = I_STRUKTUR_NAME_I1.
    READ TABLE TMP_I_DFIES BINARY SEARCH
                           WITH KEY TABNAME   = UB_REQ_FIELDS-TABNAME
                                    FIELDNAME = UB_REQ_FIELDS-FIELDNAME.
    IF SY-SUBRC <> C_RC0.
*      t_missing_fields = ub_req_fields-fieldname.
      T_MISSING_FIELDS = UB_REQ_FIELDS.
*      read table t_missing_fields.
*      if sy-subrc <> c_rc0.
        APPEND T_MISSING_FIELDS.
*      endif.
      CONTINUE.
    ENDIF.
    CLEAR SY-SUBRC.
    ASSIGN COMPONENT tmp_i_dfies-position of structure i_struktur
             TO <F1> TYPE TMP_I_DFIES-INTTYPE.
    CHECK: SY-SUBRC = C_RC0.
    IF <F1> CO C_LATTE OR <F1> IS INITIAL OR <F1> = SPACE.
*     Stype vergleichen
      IF UB_REQ_FIELDS-LOGFLAG = <F2>    OR
         <F2>                  = C_SPACE OR
         UB_REQ_FIELDS-LOGFLAG = C_SPACE.
*       prüfen ob Einmalkondition vorliegt.
        IF UB_REQ_FIELDS-LOGFLAG = <F2> AND
          ( UB_REQ_FIELDS-FIELDNAME = 'DVALUT' OR
            UB_REQ_FIELDS-FIELDNAME = 'DFAELL' ).
          READ TABLE TMP_I_DFIES BINARY SEARCH
                              WITH KEY TABNAME = UB_REQ_FIELDS-TABNAME
                                     FIELDNAME = 'AMMRHY'.
          IF SY-SUBRC = C_RC0.
           ASSIGN COMPONENT tmp_i_dfies-position of structure i_struktur
                         TO <F1> TYPE TMP_I_DFIES-INTTYPE.
            IF SY-SUBRC = C_RC0.
              IF <F1> IS INITIAL.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

*         note 2154039 <
*         check if it is zero condition
          READ TABLE TMP_I_DFIES BINARY SEARCH
                              WITH KEY TABNAME = UB_REQ_FIELDS-TABNAME
                                     FIELDNAME = 'JNULLKON'.
          IF SY-SUBRC = C_RC0.
           ASSIGN COMPONENT tmp_i_dfies-position of structure i_struktur
                         TO <F1> TYPE TMP_I_DFIES-INTTYPE.
            IF SY-SUBRC = C_RC0.
              IF <F1> = '1'.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
*         note 2154039 >

        ELSE.
       if ( not tmp_i_dfies-checktable is initial and
          ub_req_fields-logflag <> <f2> ) or
          ( tmp_i_dfies-checktable is initial and
          ub_req_fields-logflag = c_allg_bv and <f2> is initial ).
            CONTINUE.
          ENDIF.
        ENDIF.
*        t_missing_fields = ub_req_fields-fieldname.
        T_MISSING_FIELDS = UB_REQ_FIELDS.
*        read table t_missing_fields.
*        if sy-subrc <> c_rc0.
          APPEND T_MISSING_FIELDS.
*        endif.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM T_MISSING_FIELDS.
ENDFUNCTION.
