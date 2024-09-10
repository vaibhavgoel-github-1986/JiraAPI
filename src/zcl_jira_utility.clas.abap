CLASS zcl_jira_utility DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_jira_utils.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JIRA_UTILITY IMPLEMENTATION.


  METHOD zif_jira_utils~construct_query_string.

    DATA: lt_pairs TYPE TABLE OF string,
          lv_pair  TYPE string.

    FIELD-SYMBOLS: <lv_value> TYPE any.

    DATA(lt_components) = CAST cl_abap_structdescr(
                                cl_abap_typedescr=>describe_by_data(
                                 is_query_params ) )->get_components( ).

*- Convert to camelCase and create key-value Pairs
    LOOP AT lt_components INTO DATA(ls_component).
      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_query_params
        TO <lv_value>.
      IF <lv_value> IS ASSIGNED
     AND <lv_value> IS NOT INITIAL.
        lv_pair = me->zif_jira_utils~to_camel_case( ls_component-name ) && '=' && <lv_value>.
        APPEND lv_pair TO lt_pairs.
      ENDIF.
      UNASSIGN <lv_value>.
    ENDLOOP.

*- Construct the query string
    LOOP AT lt_pairs INTO lv_pair.
      IF rv_query_string IS INITIAL.
        rv_query_string = lv_pair.
      ELSE.
        CONCATENATE rv_query_string '&' lv_pair INTO rv_query_string.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_jira_utils~convert_to_jira_date_format.

    rv_jira_date = iv_date+0(4) && '-' && iv_date+4(2) && '-' && iv_date+6(2).

  ENDMETHOD.


  METHOD zif_jira_utils~to_camel_case.

    DATA: lt_parts TYPE TABLE OF char50,
          lv_part  TYPE string.

*- Split the input string by underscore
    SPLIT iv_string AT '_' INTO TABLE lt_parts.

*- Process each part to form camelCase
    LOOP AT lt_parts ASSIGNING FIELD-SYMBOL(<lv_parts>).
      IF sy-tabix = 1.
*-     Lower case the first part
        rv_string = to_lower( <lv_parts> ).
      ELSE.
*-     Capitalize the first letter of each subsequent part
        <lv_parts> = to_lower( <lv_parts> ).
        TRANSLATE <lv_parts>+0(1) TO UPPER CASE.
        CONCATENATE rv_string <lv_parts> INTO rv_string.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
