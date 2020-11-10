*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10.11.2020 at 09:43:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZXX_V_CTB_TITLE.................................*
TABLES: ZXX_V_CTB_TITLE, *ZXX_V_CTB_TITLE. "view work areas
CONTROLS: TCTRL_ZXX_V_CTB_TITLE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZXX_V_CTB_TITLE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZXX_V_CTB_TITLE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZXX_V_CTB_TITLE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZXX_V_CTB_TITLE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZXX_V_CTB_TITLE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZXX_V_CTB_TITLE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZXX_V_CTB_TITLE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZXX_V_CTB_TITLE_TOTAL.

*.........table declarations:.................................*
TABLES: TSAD3                          .
TABLES: TSAD3T                         .
TABLES: ZXX_T_CTB_TITLE                .
