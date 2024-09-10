CLASS zcl_jira_user_search DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      zif_jira_user_search.

    DATA:
      go_jira_http_client TYPE REF TO zif_jira_http_client,
      go_jira_utility     TYPE REF TO zif_jira_utils.

    METHODS:
      constructor
        IMPORTING
          VALUE(io_http_client)  TYPE REF TO zif_jira_http_client OPTIONAL
          VALUE(io_jira_utility) TYPE REF TO zif_jira_utils OPTIONAL.

  PROTECTED SECTION.
    METHODS:
      get_db_account_id
        IMPORTING
          VALUE(iv_bname)      TYPE xubname
        RETURNING
          VALUE(rv_account_id) TYPE zjira_acc_id,

      modify_users_db
        IMPORTING
          VALUE(is_user_details) TYPE zdt_jira_users.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JIRA_USER_SEARCH IMPLEMENTATION.


  METHOD constructor.

    CLEAR: go_jira_http_client.

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


  METHOD zif_jira_user_search~find_users.

    CLEAR: et_user_details.

    TRY.
*-     Create and make HTTP Client Call
        go_jira_http_client->call_api(
          EXPORTING
            iv_operation    = zif_jira_user_search=>gc_get_users
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
          data             = et_user_details ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_jira_user_search~get_jira_account_id.

    DATA: ls_query_params TYPE zif_jira_user_search=>ts_query_params.

*- First check in the Persistence DB
    rv_account_id = get_db_account_id( to_upper( iv_bname ) ).
    IF rv_account_id IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
*-     Make the Jira API to Get User Account ID
        me->zif_jira_user_search~find_users(
          EXPORTING
            is_query_params = VALUE #( query = to_lower( iv_bname ) )
           IMPORTING
             et_user_details = DATA(lt_user_details) ).

        IF lt_user_details IS NOT INITIAL.
          DATA(ls_user_details) = lt_user_details[ 1 ].

          rv_account_id = ls_user_details-account_id.

*-       Update the Persistence Layer
          modify_users_db(
            VALUE #( bname       = to_upper( iv_bname )
                     account_id  = ls_user_details-account_id
                     user_active = ls_user_details-active ) ).
        ELSE.
          RAISE EXCEPTION TYPE zcx_jira_exceptions
            EXPORTING
              code   = 404
              reason = 'Not Found'
              msgv1  = |Jira Account not found for User ID: |
              msgv2  = to_upper( iv_bname ).
        ENDIF.

      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD get_db_account_id.

*- Fetch from Persistance DB
    SELECT SINGLE *
     FROM zdt_jira_users
      INTO @DATA(ls_user)
     WHERE bname = @iv_bname.
    IF sy-subrc IS INITIAL
   AND ls_user-user_active EQ abap_false.
*-  Then assign the ticket to Default User - Vaibhav Goel
      rv_account_id = get_db_account_id( to_upper( 'vaibhago' ) ).
    ELSE.
      rv_account_id = ls_user-account_id.
    ENDIF.

  ENDMETHOD.


  METHOD modify_users_db.

*- Update the DB
    MODIFY zdt_jira_users FROM is_user_details.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
