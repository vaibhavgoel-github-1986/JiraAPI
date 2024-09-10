CLASS zcl_jira_http_client DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_jira_http_client.

  PROTECTED SECTION.
    METHODS:
      parse_error_message
        IMPORTING
          VALUE(iv_json)      TYPE string
        RETURNING
          VALUE(rv_error_msg) TYPE char100.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_JIRA_HTTP_CLIENT IMPLEMENTATION.


  METHOD parse_error_message.

    TYPES: BEGIN OF lts_error_response,
             error_messages TYPE TABLE OF string WITH EMPTY KEY,
           END OF lts_error_response.

    DATA: ls_error_response TYPE lts_error_response.

    DATA: lt_components TYPE abap_component_tab.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lv_error_msg> TYPE string.

*- Checking for errorMessages Array first
    /ui2/cl_json=>deserialize(
      EXPORTING
        json         = iv_json
        pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
        assoc_arrays = abap_true
      CHANGING
        data         = ls_error_response ).
    IF ls_error_response-error_messages IS NOT INITIAL.
      rv_error_msg = ls_error_response-error_messages[ 1 ].
      RETURN.
    ENDIF.

*- Deserializing the Dynamic JSON
    /ui2/cl_json=>deserialize(
      EXPORTING
        json         = iv_json
        pretty_name  = /ui2/cl_json=>pretty_mode-camel_case
        assoc_arrays = abap_true
      CHANGING
        data         = lr_data ).

*- Format of Error Response is like this:
* {"errorMessages":[],"errors":{"priority":"The priority selected is invalid."}}
    ASSIGN lr_data->('ERRORS') TO FIELD-SYMBOL(<ls_ref_errors>).
    IF <ls_ref_errors> IS ASSIGNED.
      ASSIGN <ls_ref_errors>->* TO FIELD-SYMBOL(<ls_errors>).

*-   Since, the JSON is dynamic, the Property name can vary
      lt_components = CAST cl_abap_structdescr(
                       cl_abap_typedescr=>describe_by_data( <ls_errors> )
                        )->get_components( ).

      IF lt_components IS NOT INITIAL.
        DATA(lv_comp_name) = lt_components[ 1 ]-name.
        ASSIGN COMPONENT lv_comp_name OF STRUCTURE <ls_errors>
         TO FIELD-SYMBOL(<lv_ref_error_msg>).
        IF <lv_ref_error_msg> IS ASSIGNED.
          ASSIGN <lv_ref_error_msg>->* TO <lv_error_msg>.
          IF <lv_error_msg> IS ASSIGNED.
            lv_comp_name = to_lower( lv_comp_name ).
            rv_error_msg = |{ lv_comp_name }: { <lv_error_msg> }|.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_jira_http_client~call_api.

    DATA: lv_url TYPE string.

    DATA: lv_username TYPE string,
          lv_password TYPE string.

    DATA lo_http_client TYPE REF TO if_http_client .

    CONSTANTS: lc_value_1 TYPE string VALUE 'Content-Type'  ##NO_TEXT,
               lc_value_3 TYPE string VALUE 'Accept' ##NO_TEXT,
               lc_value_4 TYPE string VALUE '*/*' ##NO_TEXT.

*- Creating URL
    lv_url = |https://cisco-jira.atlassian.net{ iv_operation }|.
    IF iv_query_params IS NOT INITIAL.
      CONCATENATE lv_url iv_query_params INTO lv_url
       SEPARATED BY '?'.
    ENDIF.

*- Create the HTTP client
    TEST-SEAM create_http_client.
      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_url
        IMPORTING
          client             = lo_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
    END-TEST-SEAM.
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 400
          reason = 'Bad Request'
          msgv1  = 'HTTP Client Creation Failed'.
    ENDIF.

*- Get  User Name
    SELECT SINGLE low
     FROM tvarvc
     INTO lv_username
    WHERE name = 'Z_JIRA_NOTIFY_GEN_ID'
      AND type = 'P'.
    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 404
          reason = 'TVARVC Config Not Found'
          msgv1  = 'Gen User ID not found'.
    ENDIF.

*- Get API Key
    SELECT SINGLE low
     FROM tvarvc
     INTO lv_password
    WHERE name = 'Z_JIRA_NOTIFY_API_KEY'
      AND type = 'P'.
    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 404
          reason = 'TVARVC Config Not Found'
          msgv1  = 'API Key not found'.
    ENDIF.

    lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lo_http_client->request->set_method( iv_method ).

    lo_http_client->request->set_authorization(
      EXPORTING
        username  = lv_username
        password  = lv_password ).

    lo_http_client->request->set_header_fields(
      fields = VALUE #(
                 ( name  = 'Content-Type' value = 'application/json' )
                 ( name  = 'Accept' value = '*/*' ) ) ).

    IF iv_cdata IS NOT INITIAL.
      lo_http_client->request->set_cdata(
        EXPORTING
          data   = iv_cdata ).
    ENDIF.

*- Send the HTTP Request
    lo_http_client->send(
      EXPORTING
        timeout                    = 60
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2 ).
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 502
          reason = 'Bad Gateway'
          msgv1  = 'HTTP Send Request Failed'.
    ENDIF.

*- Read the Response
    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3 ).
    IF sy-subrc IS NOT INITIAL.
      lo_http_client->get_last_error(
        IMPORTING
          code           = DATA(lv_code)
          message        = DATA(lv_reason) ).

      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = lv_code
          reason = lv_reason
          msgv1  = 'HTTP Receive Request Failed'.
    ENDIF.

*- Get the HTTP return code
    lo_http_client->response->get_status(
       IMPORTING
         code   = lv_code
         reason = lv_reason ).

*- Response JSON Payload
    rv_json_response = lo_http_client->response->get_cdata( ).

*- Close Connection
    lo_http_client->close( ).

*- Raise Exception in case of Error
    IF lv_code > 299.
      DATA(lv_error_msg) = parse_error_message( rv_json_response ).

      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = lv_code
          reason = lv_reason
          msgv1  = lv_error_msg+0(50)
          msgv2  = lv_error_msg+50(50).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
