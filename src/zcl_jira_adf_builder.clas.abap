CLASS zcl_jira_adf_builder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA gs_node TYPE zif_jira_adf_types=>ts_node .

    METHODS constructor
      IMPORTING
        VALUE(iv_type)  TYPE string OPTIONAL
        VALUE(iv_text)  TYPE string OPTIONAL
        VALUE(is_attrs) TYPE zif_jira_adf_types=>ts_attrs OPTIONAL
        VALUE(it_marks) TYPE zif_jira_adf_types=>tt_marks OPTIONAL.

    METHODS get_node_data
      RETURNING
        VALUE(rs_node) TYPE zif_jira_adf_types=>ts_node .

    METHODS set_type
      IMPORTING
        VALUE(iv_type) TYPE string
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_jira_adf_builder .

    METHODS set_text
      IMPORTING
        VALUE(iv_text) TYPE string
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_jira_adf_builder .

    METHODS set_attrs
      IMPORTING
        VALUE(is_attrs) TYPE zif_jira_adf_types=>ts_attrs
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_jira_adf_builder .

    METHODS add_marks
      IMPORTING
        VALUE(is_marks) TYPE zif_jira_adf_types=>ts_marks
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_jira_adf_builder .

    METHODS add_content
      IMPORTING
        VALUE(io_child_node) TYPE REF TO zcl_jira_adf_builder
      RETURNING
        VALUE(ro_self)       TYPE REF TO zcl_jira_adf_builder .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JIRA_ADF_BUILDER IMPLEMENTATION.


  METHOD add_content.

    DATA: ls_child_node TYPE zif_jira_adf_types=>ts_node.

    FIELD-SYMBOLS:
     <lt_content> TYPE STANDARD TABLE.

    IF gs_node-content IS INITIAL.
      CREATE DATA gs_node-content TYPE zif_jira_adf_types=>tt_nodes.
    ENDIF.

    ASSIGN gs_node-content->* TO <lt_content>.

    ls_child_node-text = io_child_node->gs_node-text.
    ls_child_node-type = io_child_node->gs_node-type.
    ls_child_node-attrs = io_child_node->gs_node-attrs.
    ls_child_node-marks = io_child_node->gs_node-marks.
    ls_child_node-content = io_child_node->gs_node-content.

    APPEND ls_child_node TO <lt_content>.

    ro_self = me.

  ENDMETHOD.


  METHOD add_marks.

    APPEND is_marks TO gs_node-marks.
    ro_self = me.

  ENDMETHOD.


  METHOD set_type.

    gs_node-type = iv_type.
    ro_self = me.

  ENDMETHOD.


  METHOD get_node_data.

    rs_node = gs_node.

  ENDMETHOD.


  METHOD constructor.

    CLEAR: gs_node.
    gs_node-type = iv_type.
    gs_node-text = iv_text.
    gs_node-attrs = is_attrs.
    gs_node-marks = it_marks.

  ENDMETHOD.


  METHOD set_attrs.

    gs_node-attrs = is_attrs.
    ro_self = me.

  ENDMETHOD.


  METHOD set_text.

    gs_node-text = iv_text.
    ro_self = me.

  ENDMETHOD.
ENDCLASS.
