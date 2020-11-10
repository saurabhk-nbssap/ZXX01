*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10.11.2020 at 09:25:00
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZXX_T_CTB_TITLE.................................*
DATA:  BEGIN OF STATUS_ZXX_T_CTB_TITLE               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZXX_T_CTB_TITLE               .
CONTROLS: TCTRL_ZXX_T_CTB_TITLE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZXX_T_CTB_TITLE               .
TABLES: ZXX_T_CTB_TITLE                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
