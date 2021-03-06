FUNCTION Z_DYNPRO_DOWNLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(HEADER) LIKE  D020S STRUCTURE  D020S
*"     VALUE(DESCRIPT) LIKE  D020T-DTXT OPTIONAL
*"     VALUE(FILE) TYPE  RLGRAP-FILENAME
*"  TABLES
*"      FIELDS STRUCTURE  D021S
*"      FLOWLOGIC STRUCTURE  D022S
*"      PARAMS STRUCTURE  D023S OPTIONAL
*"----------------------------------------------------------------------

*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(HEADER) LIKE  D020S STRUCTURE  D020S OPTIONAL
*"     REFERENCE(DESCRIPT) LIKE  D020T-DTXT OPTIONAL
*"     REFERENCE(FILE) LIKE  RLGRAP-FILENAME OPTIONAL
*"  TABLES
*"      FIELDS STRUCTURE  D021S
*"      FLOWLOGIC STRUCTURE  D022S
*"      PARAMS STRUCTURE  D023S OPTIONAL
*"----------------------------------------------------------------------


 CALL FUNCTION 'RS_SCRP_HEADER_RAW_TO_CHAR'
       EXPORTING
            HEADER_INT  = HEADER
       IMPORTING
            HEADER_CHAR = HEADER_CHAR
       EXCEPTIONS
            OTHERS      = 1.

  REFRESH DYNP_CHAR.

* Comment
  DYNP_CHAR = STARS.    APPEND DYNP_CHAR.
  DYNP_CHAR = COMMENT1. APPEND DYNP_CHAR.
  DYNP_CHAR = COMMENT2. APPEND DYNP_CHAR.
  DYNP_CHAR = STARS.    APPEND DYNP_CHAR.

* Identification
  DYNP_CHAR = DYNPRO_TEXT.      APPEND DYNP_CHAR.          "  '%_DYNPRO'
  DYNP_CHAR = HEADER_CHAR-PROG. APPEND DYNP_CHAR.
  DYNP_CHAR = HEADER_CHAR-DNUM. APPEND DYNP_CHAR.
  DYNP_CHAR = SY-SAPRL.         APPEND DYNP_CHAR.
  DESCRIBE FIELD D020T-PROG LENGTH PROG_LEN in CHARACTER MODE.
  DYNP_CHAR(16) = PROG_LEN.      APPEND DYNP_CHAR.

* Header
  DYNP_CHAR = HEADER_TEXT.      APPEND DYNP_CHAR.     "  '%_HEADER'
  APPEND HEADER_CHAR TO DYNP_CHAR.

* Description
  DYNP_CHAR = DESCRIPT_TEXT.    APPEND DYNP_CHAR.     "  '%_DESCRIPTION'
  APPEND DESCRIPT TO DYNP_CHAR.

* Fieldlist
  DYNP_CHAR = FIELDS_TEXT.          "  '%_FIELDS'
  APPEND DYNP_CHAR.
  CALL FUNCTION 'RS_SCRP_FIELDS_RAW_TO_CHAR'
       TABLES
            FIELDS_INT  = FIELDS
            FIELDS_CHAR = FIELDS_CHAR
       EXCEPTIONS
            OTHERS      = 1.

  LOOP AT FIELDS_CHAR.
    APPEND FIELDS_CHAR TO DYNP_CHAR.
  ENDLOOP.

* Flowlogic
  DYNP_CHAR = FLOWLOGIC_TEXT.         "  '%_FLOWLOGIC'
  APPEND DYNP_CHAR.

  LOOP AT FLOWLOGIC.
    APPEND FLOWLOGIC TO DYNP_CHAR.
  ENDLOOP.
**  refresh flowlogic.                   "vjb 25.06.98


* Dynpro Parameters                      "vjb ab 4.6A (01.07.98)
  IF PARAMS IS REQUESTED.
    DYNP_CHAR = PARAMS_TEXT.
    APPEND DYNP_CHAR.

    LOOP AT PARAMS.
      APPEND PARAMS TO DYNP_CHAR.
    ENDLOOP.
  ENDIF.

   CALL FUNCTION 'WS_DOWNLOAD'
     EXPORTING
       FILENAME                      = FILE
       FILETYPE                      = 'ASC'
     TABLES
       data_tab                      =  DYNP_CHAR
     EXCEPTIONS
       OTHERS                        = 10 .
   IF sy-subrc <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

** Download
*  CALL FUNCTION 'DOWNLOAD'
*       EXPORTING
*            FILENAME            = FILE
*       IMPORTING
*            CANCEL              = CANCEL
*       TABLES
*            DATA_TAB            = DYNP_CHAR
*       EXCEPTIONS
*            OTHERS              = 6.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
*            RAISING NOT_EXECUTED.
*  ENDIF.

  IF CANCEL = KREUZ.
   MESSAGE S202(EU) RAISING NOT_EXECUTED.
  ENDIF.






ENDFUNCTION.
