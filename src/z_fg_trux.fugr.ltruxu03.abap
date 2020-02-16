FUNCTION LOAN_DOMAEN_MANAGER.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_DOMNAME) LIKE  DD07V-DOMNAME
*"             VALUE(I_DOMVALUE_L) OPTIONAL
*"       EXPORTING
*"             VALUE(E_DD07V) LIKE  DD07V STRUCTURE  DD07V
*"       TABLES
*"              T_DD07V STRUCTURE  DD07V OPTIONAL
*"       EXCEPTIONS
*"              ILLEGAL_VALUE
*"              NO_DOMAIN_FOUND
*"              NO_VALUES_FOUND
*"----------------------------------------------------------------------
  DATA: TMP_DD07V LIKE DD07V OCCURS 10 WITH HEADER LINE.
  DATA: L_NAME    LIKE  DCOBJDEF-NAME.

* Domäne schon gelesene?

  IF IT_DOMAEN-DOMNAME     <> I_DOMNAME OR I_DOMNAME IS INITIAL OR
     IT_DOMAEN-DOMVALUE_L  <> I_DOMVALUE_L OR I_DOMVALUE_L IS INITIAL.
    PERFORM FIND_DOMAEN_IN_ITAB USING    I_DOMNAME I_DOMVALUE_L
                                CHANGING E_DD07V RC.
  ENDIF.
* Domänenwerte nachlesen und puffern
  IF RC <> C_RC0.
    READ TABLE IT_DOMAEN WITH KEY DOMNAME = I_DOMNAME.
    IF SY-SUBRC <> C_RC0.
      L_NAME = I_DOMNAME.
      CALL FUNCTION 'DDUT_DOMVALUES_GET'
           EXPORTING
                NAME          = L_NAME
           TABLES
                DD07V_TAB     = TMP_DD07V
           EXCEPTIONS
                OTHERS          = C_RC12.
      CASE SY-SUBRC.
        WHEN C_RC0.
          APPEND LINES OF TMP_DD07V TO IT_DOMAEN.
      ENDCASE.
    ENDIF.
  ENDIF.

  IF T_DD07V IS REQUESTED.
    READ TABLE T_DD07V WITH KEY DOMNAME = I_DOMNAME.
    IF SY-SUBRC <> C_RC0.
      LOOP AT IT_DOMAEN WHERE DOMNAME    = I_DOMNAME.
        APPEND IT_DOMAEN TO T_DD07V.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF RC <> C_RC0.
    PERFORM FIND_DOMAEN_IN_ITAB USING    I_DOMNAME I_DOMVALUE_L
                                CHANGING E_DD07V SY-SUBRC.
    CASE SY-SUBRC.
      WHEN C_RC0.
      WHEN C_RC4.
        RAISE ILLEGAL_VALUE.
      WHEN C_RC8.
        RAISE NO_DOMAIN_FOUND.
      WHEN OTHERS.
        RAISE NO_VALUES_FOUND.
    ENDCASE.
  ENDIF.

ENDFUNCTION.

*---------------------------------------------------------------------*
*       FORM FIND_DOMAEN_IN_ITAB                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(DOMNAME)                                                *
*  -->  VALUE(DOMVALUE)                                               *
*  -->  VALUE(RC)                                                     *
*---------------------------------------------------------------------*
FORM FIND_DOMAEN_IN_ITAB USING    VALUE(DOMNAME)  LIKE  DD07V-DOMNAME
                                  VALUE(DOMVALUE) TYPE ANY
                         CHANGING VALUE(S_DD07V)  STRUCTURE DD07V
                                  VALUE(RC)       LIKE SY-SUBRC.

  READ TABLE IT_DOMAEN WITH KEY DOMNAME    = DOMNAME
                            DOMVALUE_L = DOMVALUE.
  IF SY-SUBRC <> C_RC0.
    LOOP AT IT_DOMAEN WHERE DOMNAME    = DOMNAME
                      AND   DOMVALUE_L <= DOMVALUE  "#EC PORTABLE
                      AND   DOMVALUE_H >= DOMVALUE  "#EC PORTABLE
                      AND   DOMVALUE_H <> C_SPACE.  "#EC PORTABLE
      S_DD07V = IT_DOMAEN.
      EXIT.
    ENDLOOP.
  ELSE.
    S_DD07V = IT_DOMAEN.
  ENDIF.
  RC = SY-SUBRC.

ENDFORM.
