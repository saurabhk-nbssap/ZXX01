FUNCTION FILE_READ_AND_GET_TAB .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       TABLES
*"              ITAB_RECEIVER OPTIONAL
*"       EXCEPTIONS
*"              UPLOAD_CANCELLED
*"              UPLOAD_ERROR
*"----------------------------------------------------------------------

  TRUX_DISPLAY-FILE_SERV   = 'PRS'.
  TRUX_DISPLAY-FILE_FORMAT = 'XLS'.

  CALL SCREEN 1000 STARTING AT  6  1
                   ENDING   AT 80 12.
  IF SY-SUBRC <> C_RC0.
    MESSAGE E052(ED) RAISING UPLOAD_CANCELLED.
  ELSE.
    CALL FUNCTION 'FILE_READ_AND_CONVERT_SAP_DATA'
         EXPORTING
              I_FILENAME     = TRUX_DISPLAY-FILE_NAME
              I_SERVERTYP    = TRUX_DISPLAY-FILE_SERV
              I_FILEFORMAT   = TRUX_DISPLAY-FILE_FORMAT
              I_LINE_HEADER  = TRUX_DISPLAY-FILE_HEADER
         TABLES
              I_TAB_RECEIVER = ITAB_RECEIVER
         EXCEPTIONS
              OTHERS         = C_RC8.
    CASE SY-SUBRC.
      WHEN C_RC0.
      WHEN OTHERS.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                RAISING UPLOAD_ERROR.
    ENDCASE.
  ENDIF.
ENDFUNCTION.
