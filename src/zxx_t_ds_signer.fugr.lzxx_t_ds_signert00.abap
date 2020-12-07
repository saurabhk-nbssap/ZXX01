*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.12.2020 at 13:47:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZXX_T_DS_SIGNER.................................*
DATA:  BEGIN OF STATUS_ZXX_T_DS_SIGNER               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZXX_T_DS_SIGNER               .
CONTROLS: TCTRL_ZXX_T_DS_SIGNER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZXX_T_DS_SIGNER               .
TABLES: ZXX_T_DS_SIGNER                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
