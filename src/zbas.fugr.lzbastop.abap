FUNCTION-POOL ZBAS.                         "MESSAGE-ID ..
*----------------------------------------------------------------------*
*   INCLUDE ZLSCR2TOP                                                  *
*----------------------------------------------------------------------*
class cl_oo_include_naming definition load.

TYPES: BOOLEAN TYPE C.

CONSTANTS: YES TYPE BOOLEAN VALUE 'X',
           NO  TYPE BOOLEAN VALUE ' '.

* release- oder versionsabhängige Konstanten und Daten
INCLUDE MSEUSREL.

* Farben
CONSTANTS: C_PROT_COL(1) TYPE N VALUE 2,
           C_PROT_COL_HEAD(1) TYPE N VALUE 2,
           C_PROT_COL_HEAD_TEXT(1) TYPE N VALUE 0,
           C_PROT_COL_NODE(1) TYPE N VALUE 2,
           C_PROT_COL_ERROR(1) TYPE N VALUE 6,
           C_PROT_COL_NODE_TEXT(1) TYPE N VALUE 0,
           C_NODE_LENGTH TYPE I VALUE 30,
           C_TEXT_LENGTH TYPE I VALUE 75,
           C_TEXT_HEAD_LENGTH TYPE I VALUE 40,
           C_TOP_LEVEL TYPE C VALUE 2,
           C_NODE_LEVEL TYPE C VALUE 3,
           C_LEAF_LEVEL TYPE C VALUE 4,
           C_SUB_LEVEL TYPE C VALUE 5.
CONSTANTS:  C_TEXT_COL(1)
                   TYPE N VALUE 0, """"alt 1 oder 4
           C_MARK_COL(1)
                   TYPE N VALUE 2,
           C_ORIG_COL(1)
                   TYPE N VALUE 1,
           C_MODI_COL(1)
                   TYPE N VALUE 1,
           C_INTENSIV     VALUE '1',
           C_NORMAL       VALUE ' '.

*-----------------------------------------------------------------------
* Makros
*-----------------------------------------------------------------------
DATA: RAW1(1) TYPE X,      "zur Konvertierung
      NUMBER  TYPE I.      "

* Zuweisung mit Test auf Ziffern-Zeichen
DEFINE MOVE_SIGN.
  IF &1 = SPACE.
    &1 = '0'.
  ENDIF.
  IF &1 CN ' 0123456789ABCDEF'.
    &1 = '0'.
  ENDIF.
  MOVE &1 TO &2.
END-OF-DEFINITION.

* Zuweisung mit Konvertierung über eine RAW(1)-Variable mit Test auf
* Ziffernzeichen
DEFINE MOVE_RAW1.
  IF &1 = SPACE.
    &1 = '0'.
  ENDIF.
  IF &1 CN ' 0123456789ABCDEF'.
    &1 = '0'.
  ENDIF.
  MOVE &1 TO RAW1.
  MOVE RAW1 TO &2.
END-OF-DEFINITION.

* Zuweisung mit Konvertierung über Typ INT
DEFINE MOVE_INT.
  MOVE &1 TO NUMBER.
  MOVE NUMBER TO &2.
END-OF-DEFINITION.

* Zuweisung mit Konvertierung über Typ INT und WRITE-Befehl
DEFINE MOVE_INTW.
  IF &1 = SPACE.
    &1 = '0'.
  ENDIF.
  MOVE &1 TO NUMBER.
  WRITE NUMBER TO &2 NO-SIGN.
END-OF-DEFINITION.

*-----------------------------------------------------------------------

INCLUDE MSEUSBIT.   "Masken

*-----------------------------------------------------------------------
* Kundenerweiterungen
*-----------------------------------------------------------------------
CONSTANTS:
      COLOR_CUSTOMER_EXTEND TYPE C      "Feldliste: Farbe
                         VALUE 7,       "  Kundenerweiterung
*      COLUMN_ACT_SIGN TYPE I                  "Ablauflogik/ABAP-Source:
*                        VALUE 50,      "  Spaltennr. Kennzeichnung act.
*      COLUMN_ACT_DATE TYPE I                  "Ablauflogik/ABAP-Source:
*                         VALUE 61,      "  Spaltennr. Aktivierungsdatum
*      EXT_SIGN_ACTIVATED(10)            "Ablauflogik/ABAP-Source:
*                                        "  Kennzeichnung: activated
*               VALUE 'activated:',                          "#EC NOTEXT
*      EXT_SIGN_FIRST(30)                "Kennung des Beginns eines
*                                        "  Blocks von Zeilen als
*                                        "  Erweiterung
*               VALUE '*{ #### Extension',                   "#EC NOTEXT
*      EXT_SIGN_CONT(30)                 "Kennung einer Inhaltszeile im
*                                        "  Block von Zeilen als
*                                        "  Erweiterung
*               VALUE '*#',                                  "#EC NOTEXT
*      EXT_SIGN_LAST(30)                 "Kennung des Endes eines
*                                        "  Blocks von Zeilen als
*                                        "  Erweiterung
*               VALUE '*} #### Extension',                   "#EC NOTEXT
      SMODILOG_INT_TYPE_DYNP            "SMODILOG-INT_TYPE für Dynpros
           LIKE SMODILOG-INT_TYPE
                         VALUE 'DYNP',                       "#EC NOTEXT
      SMODILOG_D_TYPE_DYNP              "Typ 'allg.Dynproangaben' in
           LIKE SMODILOG_D-XTYPE        " SMODILOG_DYNP
                         VALUE 'DYNP',                       "#EC NOTEXT
      SMODILOG_D_TYPE_SSCR              "Subscreen-Typ in
           LIKE SMODILOG_D-XTYPE        " SMODILOG_DYNP
                         VALUE 'SSCR',                       "#EC NOTEXT
      SMODILOG_D_TYPE_MODL              "Abl-logik(EXIT)-Typ
           LIKE SMODILOG_D-XTYPE        " SMODILOG_DYNP
                         VALUE 'MODL'.                       "#EC NOTEXT
*CONSTANTS: C_PRE  LIKE SMODISRC-OPERATION  VALUE 'PRE ',  "#EC NOTEXT
*           C_POST LIKE SMODISRC-OPERATION  VALUE 'POST',  "#EC NOTEXT
*           C_ALL  LIKE SMODISRC-OPERATION  VALUE 'ALL ',  "#EC NOTEXT
*           C_NEW  LIKE SMODISRC-OPERATION  VALUE 'NEW ',  "#EC NOTEXT
*           C_PBO  LIKE SMODISRC-INT_NAME   VALUE 'A_PBO', "#EC NOTEXT
*           C_PAI  LIKE SMODISRC-INT_NAME   VALUE 'B_PAI', "#EC NOTEXT
*           C_POH  LIKE SMODISRC-INT_NAME   VALUE 'C_POH'. "#EC NOTEXT
*           c_pov  like smodisrc-int_name   value 'D_POV'. "#EC NOTEXT

CONSTANTS:
      NORM_LINES TYPE I VALUE 21,       "Normdynpro Zeilenanzahl
      NORM_COLUMNS TYPE I VALUE 83,     "Normdynpro Spaltenanzahl
      MARK_VALUE   TYPE I VALUE 1024.   "Wert, der in F-DIDX und
                                        "zur Markierung verwendet wird

TYPE-POOLS:
        SMODI.               "Kundenerweiterungen
TABLES: SMODILOG,
        SMODISRC,
        D020T,
        D020S.

DATA: BEGIN OF DYNPRO_NAME,
        PROG LIKE D020S-PROG,
        DNUM LIKE D020S-DNUM,
      END OF DYNPRO_NAME.

DATA: LINES TYPE I,
      SUBRC LIKE SY-SUBRC,
      TR_KEY LIKE TRKEY.
DATA: LINES_DIFF_HEADER        TYPE I,
      LINES_DIFF_CONTAINERS    TYPE I,
      LINES_DIFF_FIELDS        TYPE I,
      LINES_DIFF_PARAMS        TYPE I,
      LINES_DIFF_FLOW          TYPE I,
      LINES_SMODILOG_DYNP      TYPE I,
      LINES_SMODILOG_ABAP      TYPE I.
DATA: D_TRANSPORT_KEY type TRKEY,
      D_HEADER       LIKE RPY_DCHEAD
           OCCURS 0 WITH HEADER LINE,
      D_HEADER_O     type RPY_DCHEAD,
      D_HEADER_A     type RPY_DCHEAD,
      D_CONTAINERS   type RPY_DCCATT
           OCCURS 0 WITH HEADER LINE,
      D_CONTAINERS_O  type RPY_DCCATT,
      D_CONTAINERS_A  type RPY_DCCATT,
      D_CONTAINERS_E  type RPY_DCCATT,
      D_CONTAINERS_D  type RPY_DCCATT,
      D_FIELDS        type RPY_DCFATC
           OCCURS 0 WITH HEADER LINE,
      D_FIELDS_O      type RPY_DCFATC,
      D_FIELDS_A      type RPY_DCFATC,
      D_FIELDS_E      type RPY_DCFATC,
      D_FIELDS_D      type RPY_DCFATC,
      D_FLOW          type RPY_DCFLOW
           OCCURS 0 WITH HEADER LINE,
      D_PARAMS        type RPY_DCPARA
           OCCURS 0 WITH HEADER LINE,
      D_PARAMS_O      type RPY_DCPARA,
      D_PARAMS_A      type RPY_DCPARA,
      D_PARAMS_E      type RPY_DCPARA,
      D_PARAMS_D      type RPY_DCPARA,
      D_SMODILOG_DYNP type SMODILOG_D
           OCCURS 0 WITH HEADER LINE,
*      d_flow_logic_details type smodi_fl_det_tab,
*      d_flow_logic_details_wa type smodi_fl_details.
      d_smodisrc_prot_abap type smodi_mod_tab,
      D_SMODILOG_ABAP LIKE SMODILOG
           OCCURS 0 WITH HEADER LINE.
DATA: L_NAME type RPY_DCCATT-NAME,
      L_TYPE type RPY_DCCATT-TYPE,
      L_PNAME LIKE RPY_DCPARA-CLS_PNAME,
      L_TYPE_TEXT LIKE SNODETEXT-TEXT,
      L_DIFF type RPY_DCCATT-DIFF,
      L_WITH_FLOW_DETAILS,
      L_EXTENDING_STATUS.

data: headername      type snodetext-text,
      headervalue     type snodetext-text,
      subheadervalue  type snodetext-text.

data: d_extending_status,              "Status: Modifikationsvergleich
      d_comp_vers_status,              "Status: Versionenvergleich
      begin of d_comp,
        text_1 type snodetext-text,
        text_2 type snodetext-text,
      end of d_comp.

TYPES: NAMETAB_FIELD_TYPE LIKE DFIES,
       NAMETAB_TYPE TYPE NAMETAB_FIELD_TYPE
                    OCCURS 0.
DATA: NAMETAB_D_HEADER     TYPE NAMETAB_TYPE
                    WITH HEADER LINE,
      NAMETAB_D_CONTAINERS TYPE NAMETAB_TYPE
                    WITH HEADER LINE,
      NAMETAB_D_FIELDS     TYPE NAMETAB_TYPE
                    WITH HEADER LINE,
      NAMETAB_D_PARAMS     TYPE NAMETAB_TYPE
                    WITH HEADER LINE,
      NAMETAB_D_FLOW       TYPE NAMETAB_TYPE
                    WITH HEADER LINE.
DATA: FIRST_NODE TYPE I.

FIELD-SYMBOLS: <F>,
               <FO>,
               <FA>.
DATA: DIFFERENCES LIKE SNODETEXT       "Für TREE-Bausteine zur
           OCCURS 0 WITH HEADER LINE,  "Differenzenanzeige
      DIFFER_NEW  LIKE SNODETEXT
           OCCURS 0 WITH HEADER LINE,
      DIFFER_CHG  LIKE SNODETEXT
           OCCURS 0 WITH HEADER LINE,
      DIFFER_DEL  LIKE SNODETEXT
           OCCURS 0 WITH HEADER LINE,
      DIFFER_ABAP  LIKE SNODETEXT
           OCCURS 0 WITH HEADER LINE,
      DIFFER_HEAD_NEW_ATTR,
      DIFFER_HEAD_NEW_FIELDS,
      DIFFER_HEAD_NEW_PARAMS,
      DIFFER_HEAD_NEW_FLOW,
      DIFFER_HEAD_CHG_ATTR,
      DIFFER_HEAD_CHG_FIELDS,
      DIFFER_HEAD_CHG_PARAMS,
      DIFFER_HEAD_CHG_FLOW,
      DIFFER_HEAD_DEL_ATTR,
      DIFFER_HEAD_DEL_FIELDS,
      DIFFER_HEAD_DEL_PARAMS,
      DIFFER_HEAD_DEL_FLOW.

DATA: SMODISRC_KEY TYPE SMODI_SRC_KEY.  "Key für SMODISRC
DATA: SMODILOG_DYNP LIKE SMODILOG_D     "SMODILOG/SMODISRC: Einträge zum
           OCCURS 0 WITH HEADER LINE.   "  aktuellen Dynpro
DATA: SMODILOG_ABAP LIKE SMODILOG OCCURS 0 WITH HEADER LINE.
DATA: SMODISRC_PROT_ABAP TYPE SMODI_MOD_TAB.

DATA: BEGIN OF SV_LOG,                "Status-Info (SAP)
        PROT_ONLY,
      END OF SV_LOG.
DATA: SV_HEADER LIKE D020S.          "Dynproheader (SAP)
DATA: CV_HEADER LIKE D020S.          "Dynproheader (Cust)
DATA: BEGIN OF SV_FIELDS OCCURS 0.   "Feldliste    (SAP)
         INCLUDE STRUCTURE D021S.
DATA: END OF SV_FIELDS.
DATA: BEGIN OF CV_FIELDS OCCURS 0.   "Feldliste    "(Cust)
         INCLUDE STRUCTURE D021S.
DATA: END OF CV_FIELDS.
DATA: BEGIN OF SV_FLOW  OCCURS 0.    "Ablauflogik  "(SAP)
         INCLUDE STRUCTURE D022S.
DATA: END OF SV_FLOW.
DATA: BEGIN OF SV_PARAMS OCCURS 0.   "Dynp-Params  "(SAP)
         INCLUDE STRUCTURE D023S.
DATA: END OF SV_PARAMS.
DATA: BEGIN OF CV_FLOW  OCCURS 0.    "Ablauflogik  "(Cust)
         INCLUDE STRUCTURE D022S.
DATA: END OF CV_FLOW.
DATA: BEGIN OF SV_TTAB  OCCURS 0.    "Feldtexte    "(SAP)
         INCLUDE STRUCTURE D021T.
DATA: END OF SV_TTAB.
DATA: BEGIN OF CV_PARAMS OCCURS 0.    "Dynp-Params "(Cust.)
         INCLUDE STRUCTURE D023S.
DATA: END OF CV_PARAMS.
DATA: BEGIN OF SV_BTAB  OCCURS 0.    "Beschr.texte "(SAP)
         INCLUDE STRUCTURE D020T.
DATA: END OF SV_BTAB.
DATA: BEGIN OF CV_BTAB  OCCURS 0.    "Beschr.texte "(Cust)
         INCLUDE STRUCTURE D020T.
DATA: END OF CV_BTAB.

DATA: HEADER_A type RPY_DYHEAD,
      HEADER_O type RPY_DYHEAD,
      CONTAINERS_A type rpy_dycatt occurs 0 with header line,
      CONTAINERS_O type rpy_dycatt occurs 0 with header line,
      FIELDS_A type rpy_dyfatc occurs 0 with header line,
      FIELDS_O type rpy_dyfatc occurs 0 with header line,
      PARAMS_A type rpy_dypara occurs 0 with header line,
      PARAMS_O type rpy_dypara occurs 0 with header line,
      BEGIN OF HEADER_TEXTS_O OCCURS 0.
        INCLUDE STRUCTURE D020T.
DATA: END OF HEADER_TEXTS_O.

DATA: BEGIN OF DIFF_FLOW_DETAIL OCCURS 0,
        INT_NAME  LIKE SMODISRC_KEY-INT_NAME,   "Zeitpunkt
        OPERATION LIKE SMODISRC_KEY-OPERATION,  "Erw.-Art (Anfang, Ende)
        INT_TYPE  LIKE SMODISRC_KEY-INT_TYPE,   "Subscr. oder Modul
        MODINR    LIKE SMODILOG_D-XNAME,        "Index in SMODILOG_DYNP
        NR        LIKE SY-INDEX,                "zeilennr.
        LINE      LIKE D022S-LINE,              "Abl-Zeile
        XNAME     LIKE SMODILOG_DYNP-XNAME,     "Name der Erw.
      END OF DIFF_FLOW_DETAIL,
      L_INT_NAME  LIKE SMODISRC_KEY-INT_NAME,
      L_OPERATION LIKE SMODISRC_KEY-OPERATION,
      L_INT_TYPE  LIKE SMODISRC_KEY-INT_TYPE,
      L_XNAME     LIKE SMODILOG_DYNP-XNAME.

DATA: BEGIN OF FLOW_EXTEND_CALL_PBO OCCURS 0,
        LINE LIKE D022S-LINE,
      END OF FLOW_EXTEND_CALL_PBO,
      BEGIN OF FLOW_EXTEND_CALL_PAI OCCURS 0,
        LINE LIKE D022S-LINE,
      END OF FLOW_EXTEND_CALL_PAI,
      BEGIN OF BUFFER               OCCURS 0,
        LINE LIKE D022S-LINE,
      END OF BUFFER.

DATA: BEGIN OF L_FIELD_LIST OCCURS 0. "Feldliste intern
         INCLUDE STRUCTURE D021S.
DATA: END OF L_FIELD_LIST,
      L_FIELD LIKE D021S,
      MARKED,
      LENG TYPE I,
      VLENG TYPE I.


*-----------------------------------------------------------------------
* Dynpro Upload/Download
*-----------------------------------------------------------------------

CONSTANTS:
           STARS(64)          VALUE
'****************************************************************',
                                                        "#EC NOTEXT
           COMMENT1(64)       VALUE
'*   THIS FILE IS GENERATED BY THE SCREEN PAINTER.              *',
                                                        "#EC NOTEXT
           COMMENT2(64)       VALUE
'*   NEVER CHANGE IT MANUALLY, PLEASE !                         *',
                                                        "#EC NOTEXT
           DYNPRO_TEXT(8)     VALUE '%_DYNPRO',         "#EC NOTEXT
           HEADER_TEXT(8)     VALUE '%_HEADER',         "#EC NOTEXT
           PARAMS_TEXT(8)     VALUE '%_PARAMS',         "#EC NOTEXT
           DESCRIPT_TEXT(13)  VALUE '%_DESCRIPTION',    "#EC NOTEXT
           FIELDS_TEXT(8)     VALUE '%_FIELDS',         "#EC NOTEXT
           KREUZ(1)           VALUE 'x',                "#EC NOTEXT
           FLOWLOGIC_TEXT(11) VALUE '%_FLOWLOGIC'.      "#EC NOTEXT

DATA: BEGIN OF DYNP,
           PROG LIKE D020S-PROG,
           DNUM LIKE D020S-DNUM,
      END OF DYNP.
DATA  TITLE_TEXT(80).
DATA  MESS_TEXT(80).
DATA  MESS2_TEXT(80).
DATA  MESS3_TEXT(80).
DATA  HEADER_CHAR LIKE SCR_CHHEAD.
DATA  FIELDS_CHAR LIKE SCR_CHFLD OCCURS 0 WITH HEADER LINE.
DATA  FILENAME LIKE  RLGRAP-FILENAME.
DATA  DYNP_CHAR LIKE SCR_CHFLD OCCURS 0 WITH HEADER LINE.
DATA  RELEASE.
DATA  PROG_LEN     TYPE P.
DATA  PROG_LEN_AKT TYPE P.
DATA  STATUS.
DATA  CANCEL.
DATA  ANSWER TYPE C.

DATA  DYNP_RL(3).

DATA: BEGIN OF R OCCURS 0,       "Rahmeninformation
         LINE LIKE D021S-LINE,     "Zeile
         COLN LIKE D021S-COLN,     "Spalte
         LENG LIKE D021S-LENG,     "Laenge
         INPUT,                    "Input X=on
         INTEN,                    "Intensified X=on
      END OF R.
