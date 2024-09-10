INTERFACE zif_jira_user_search
  PUBLIC .

  TYPES:
    BEGIN OF ts_query_params,
      query       TYPE string,
      account_id  TYPE string,
      start_at    TYPE i,
      max_results TYPE i,
    END OF ts_query_params,

    BEGIN OF ts_user_details,
      account_id    TYPE string,
      email_address TYPE string,
      display_name  TYPE string,
      active        TYPE abap_bool,
    END OF ts_user_details,

    tt_user_details TYPE STANDARD TABLE OF ts_user_details.

  CONSTANTS:
    gc_get_users TYPE string VALUE '/rest/api/3/user/search'.

  METHODS:
    find_users
      IMPORTING
        VALUE(is_query_params) TYPE ts_query_params
      EXPORTING
        et_user_details        TYPE tt_user_details
      RAISING
        zcx_jira_exceptions,

    get_jira_account_id
      IMPORTING
        VALUE(iv_bname)      TYPE xubname
      RETURNING
        VALUE(rv_account_id) TYPE zjira_acc_id
      RAISING
        zcx_jira_exceptions.

ENDINTERFACE.
