CLASS zcl_jira_issues DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_jira_issues .

    DATA:
      go_jira_http_client TYPE REF TO zif_jira_http_client,
      go_jira_utility     TYPE REF TO zif_jira_utils.

    METHODS:
      constructor
        IMPORTING
          VALUE(io_http_client)  TYPE REF TO zif_jira_http_client OPTIONAL
          VALUE(io_jira_utility) TYPE REF TO zif_jira_utils OPTIONAL.

  PROTECTED SECTION.

ENDCLASS.



CLASS ZCL_JIRA_ISSUES IMPLEMENTATION.


  METHOD zif_jira_issues~search_issue_by_jql.

    CLEAR: es_response.

    TRY.
*-     Create and make HTTP Client Call
        go_jira_http_client->call_api(
          EXPORTING
            iv_operation    = zif_jira_issues=>gc_search_by_sql
            iv_query_params = go_jira_utility->construct_query_string( is_query_params )
            iv_method       = if_http_entity=>co_request_method_get
           RECEIVING
            rv_json_response = DATA(lv_json_response) ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

    IF lv_json_response IS NOT INITIAL.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_json_response
          pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
          assoc_arrays     = abap_true
        CHANGING
          data             = es_response ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CLEAR:
     go_jira_http_client,
     go_jira_utility.

*- Dependency Injection
    IF io_http_client IS INITIAL.
      go_jira_http_client = NEW zcl_jira_http_client( ).
    ELSE.
      go_jira_http_client = io_http_client.
    ENDIF.

    IF io_jira_utility IS INITIAL.
      go_jira_utility = NEW zcl_jira_utility( ).
    ELSE.
      go_jira_utility = io_jira_utility.
    ENDIF.

  ENDMETHOD.


  METHOD zif_jira_issues~create_issue.

    DATA: lt_mappings TYPE /ui2/cl_json=>name_mappings.

    CLEAR: es_response.

*- Create Mappings for Custom Fields
    lt_mappings = VALUE #(
                   ( abap = to_upper( 'customfield_10043')
                     json = 'customfield_10043' )
                   ( abap = to_upper( 'customfield_10035')
                     json = 'customfield_10035' )
                   ( abap = to_upper( 'customfield_10040')
                     json = 'customfield_10040' )
                   ( abap = to_upper( 'customfield_10038')
                     json = 'customfield_10038' )
                   ( abap = to_upper( 'customfield_10026')
                     json = 'customfield_10026' ) ).

*- Serialize into JSON Body
    /ui2/cl_json=>serialize(
      EXPORTING
        data          = is_request
        compress      = abap_true
        name_mappings = lt_mappings
        pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
        assoc_arrays  = abap_true
      RECEIVING
        r_json = DATA(lv_body) ).

    TRY.
*-     Create and make HTTP Client Call
        go_jira_http_client->call_api(
          EXPORTING
            iv_operation    = zif_jira_issues=>gc_post_issue
            iv_method       = if_http_entity=>co_request_method_post
            iv_cdata        = lv_body
          RECEIVING
           rv_json_response = DATA(lv_json_response) ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

*- If No Exception, then De-serialize the Response
    /ui2/cl_json=>deserialize(
       EXPORTING
         json             = lv_json_response
         pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
         assoc_arrays     = abap_true
      CHANGING
        data             = es_response ).

  ENDMETHOD.
ENDCLASS.
