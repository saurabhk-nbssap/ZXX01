FUNCTION ZSUSR_USERS_GET_FROM_REMOTE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(REPORT_ID) LIKE  SY-REPID
*"  TABLES
*"      USERS STRUCTURE  USSELMODBE
*"      USERS_OUTTAB STRUCTURE  USOUTTAB
*"      PARAMS STRUCTURE  RSPARAMS
*"--------------------------------------------------------------------
*  10.03.2009 D034973   ( Note 1315887 )                         7.01 +
* lock state was not moved into the USERS_OUTTAB
* the structure USOUTTAB was changed:
*   ICONLOCKED  => ICON_LOCKED
*   LOCK_REASON => LOCK_REASON
*-----------------------------------------------------------------------


  DATA: gv_xcall TYPE c.

  TABLES t000.

*{   REPLACE        SBXK900172                                        1
*\  TYPES: BEGIN OF ys_outtab.
*\          INCLUDE STRUCTURE sim_rsusr200_alv.
*\    types:   trdat type  xuldate,      " hiden sort field
*\    color type  lvc_t_scol,   " for color
*\    end of ys_outtab.
  TYPES: ys_outtab type sim_rsusr200_alv.
*}   REPLACE

  CONSTANTS: gc_xcall(14)      TYPE c VALUE 'EXT_CALL_SUSR',
             gc_usr02_ret(10)  TYPE c VALUE 'X_RSUSR200'.

  RANGES: bname FOR usr02-bname.

  DATA: lt_color  TYPE lvc_t_scol,
        ls_color  TYPE lvc_s_scol.

  DATA:  gt_usr02  TYPE STANDARD TABLE OF usr02,
         gt_outtab TYPE STANDARD TABLE OF ys_outtab.


  FIELD-SYMBOLS: <lp_outtab> TYPE  ys_outtab.


  gv_xcall = 'X'.

  SELECT SINGLE logsys FROM t000 INTO t000-logsys
        WHERE mandt = sy-mandt.

  bname[] = users[].

  EXPORT gv_xcall TO MEMORY ID gc_xcall.
  EXPORT bname TO MEMORY ID gc_usr02_ret.

  SUBMIT (report_id) WITH SELECTION-TABLE params AND RETURN.
  IMPORT gt_usr02 gt_outtab FROM MEMORY ID gc_usr02_ret.

  sy-index = 1.

  LOOP AT gt_outtab ASSIGNING <lp_outtab>.
    lt_color = <lp_outtab>-color.
    IF lt_color[] IS NOT INITIAL.
      READ TABLE lt_color INDEX sy-index INTO ls_color.
      users_outtab-fname = ls_color-fname.
      users_outtab-nokeycol =  ls_color-nokeycol.
      users_outtab-col = ls_color-color-col.
      users_outtab-int = ls_color-color-int.
      users_outtab-inv = ls_color-color-inv.
    ELSE.
      CLEAR users_outtab-col.
    ENDIF.
    MOVE-CORRESPONDING <lp_outtab> TO users_outtab.
    users_outtab-system = t000-logsys.
    APPEND users_outtab.
  ENDLOOP.

ENDFUNCTION.
