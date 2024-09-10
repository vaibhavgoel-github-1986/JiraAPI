INTERFACE zif_jira_http_client
  PUBLIC .

  METHODS:
    call_api
      IMPORTING
        VALUE(iv_operation)     TYPE string
        VALUE(iv_query_params)  TYPE string OPTIONAL
        VALUE(iv_method)        TYPE string DEFAULT if_http_entity=>co_request_method_get
        VALUE(iv_cdata)         TYPE string OPTIONAL
      RETURNING
        VALUE(rv_json_response) TYPE string
      RAISING
        zcx_jira_exceptions.

ENDINTERFACE.
