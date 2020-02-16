class ZDOMPAY_UATCO_IDOMESTIC_PAY_SE definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_ACC_BALANCE
    importing
      !INPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV9
    exporting
      !OUTPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV8
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_RETURN_TRXN
    importing
      !INPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV7
    exporting
      !OUTPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV6
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_STATMENT
    importing
      !INPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV5
    exporting
      !OUTPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV4
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_TXN_RESPONSE_IN_XML
    importing
      !INPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV3
    exporting
      !OUTPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV2
    raising
      CX_AI_SYSTEM_FAULT .
  methods PROCESS_TXN_IN_XML
    importing
      !INPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERV1
    exporting
      !OUTPUT type ZDOMPAY_UATIDOMESTIC_PAY_SERVI
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZDOMPAY_UATCO_IDOMESTIC_PAY_SE IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZDOMPAY_UATCO_IDOMESTIC_PAY_SE'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method GET_ACC_BALANCE.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'GET_ACC_BALANCE'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method GET_RETURN_TRXN.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'GET_RETURN_TRXN'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method GET_STATMENT.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'GET_STATMENT'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method GET_TXN_RESPONSE_IN_XML.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'GET_TXN_RESPONSE_IN_XML'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.


  method PROCESS_TXN_IN_XML.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'PROCESS_TXN_IN_XML'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
