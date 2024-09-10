CLASS ltcl_jira_user_search DEFINITION DEFERRED.
CLASS zcl_jira_user_search DEFINITION LOCAL FRIENDS ltcl_jira_user_search.

CLASS ltcl_jira_user_search DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FRIENDS zcl_jira_issues.

  PRIVATE SECTION.
    DATA:
      go_cut         TYPE REF TO zcl_jira_user_search.      "Class Under Test

    DATA:
      go_mock_client TYPE REF TO zif_jira_http_client.      "Mock HTTP Client

    CLASS-DATA:
       go_osql_test TYPE REF TO if_osql_test_environment.   "SQL Test Double

    CLASS-METHODS:
      " Method to set up the test environment for the class
      class_setup,

      " Method to clean up the test environment after all tests in the class have run
      class_teardown.

    METHODS:
      " Method to prepare the CUT (Class Under Test) and clear test doubles before each test method
      setup,

      " Method to clear the CUT object after each test method
      teardown,

      " Method to test fetching account ID from the database when user is active
      test_get_active_user FOR TESTING,
      " Method to test fetching account ID from the database when user is inactive
      test_get_default_user FOR TESTING,

      " Method to test the User Search
      test_find_users FOR TESTING,
      test_find_users_exception FOR TESTING,

      " User Found in Database
      test_get_account_id_1 FOR TESTING,

      " User Not Found in Database but Found via API
      test_get_account_id_2 FOR TESTING.

    CLASS-METHODS:
      " Method to insert mock test data into the zdt_jira_users table
      insert_mock_test_data
        IMPORTING
          VALUE(iv_bname)       TYPE bname
          VALUE(iv_user_active) TYPE abap_bool
          VALUE(iv_account_id)  TYPE string.

ENDCLASS.

CLASS ltcl_jira_user_search IMPLEMENTATION.

  METHOD class_setup.
    " Initialize the oSQL test environment
    go_osql_test = cl_osql_test_environment=>create(
                    i_dependency_list = VALUE #( ( 'ZDT_JIRA_USERS' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up the oSQL test environment
    go_osql_test->destroy( ).
  ENDMETHOD.

  METHOD setup.

*- Creating Test Double for HTTP Client Class
    go_mock_client ?= cl_abap_testdouble=>create( object_name = 'ZIF_JIRA_HTTP_CLIENT' ).

    " Create an instance of the CUT class
    go_cut = NEW zcl_jira_user_search(
      io_http_client  = go_mock_client  "Injecting Test Double
    ).

  ENDMETHOD.

  METHOD teardown.
    " Clear the CUT object
    CLEAR: go_cut.

    " Clear any previous test doubles
    go_osql_test->clear_doubles( ).
  ENDMETHOD.

  METHOD test_get_active_user.
    " Test fetching account ID from the database when the user is active

    " Insert mock test data for an active user
    insert_mock_test_data(
      iv_bname = to_upper( 'john_doe' )
      iv_user_active = abap_true
      iv_account_id = 'account_123' ).

    " Call the method and get the account ID
    DATA(lv_account_id) = go_cut->get_db_account_id( to_upper( 'john_doe' ) ).

    " Assert that the account ID is as expected
    cl_abap_unit_assert=>assert_equals(
      act = lv_account_id
      exp = 'account_123'
      msg = 'Account ID for active user is not as expected' ).

  ENDMETHOD.

  METHOD test_get_default_user.
    " Test fetching account ID from the database when the user is inactive

    " Insert mock test data for an inactive user
    insert_mock_test_data(
      iv_bname = to_upper( 'jane_doe' )
      iv_user_active = abap_false
      iv_account_id = 'account_456' ).

    " Insert mock test data for the default user 'vaibhago'
    insert_mock_test_data(
      iv_bname = to_upper( 'vaibhago' )
      iv_user_active = abap_true
      iv_account_id = 'default_account' ).

    " Call the method and get the account ID
    DATA(lv_account_id) = go_cut->get_db_account_id( to_upper( 'jane_doe' ) ).

    " Assert that the account ID is as expected
    cl_abap_unit_assert=>assert_equals(
      act = lv_account_id
      exp = 'default_account'
      msg = 'Account ID for inactive user is not as expected' ).

  ENDMETHOD.

  METHOD insert_mock_test_data.

    " Insert mock data into the zdt_jira_users table

    DATA: lt_jira_users TYPE STANDARD TABLE OF zdt_jira_users,
          ls_jira_user  TYPE zdt_jira_users.

    " Prepare mock data
    ls_jira_user = VALUE #(
                     bname = iv_bname
                     user_active = iv_user_active
                     account_id = iv_account_id ).

    APPEND ls_jira_user TO lt_jira_users.

    " Insert mock data into the oSQL test environment
    go_osql_test->insert_test_data(
      EXPORTING
        i_data = lt_jira_users ).

  ENDMETHOD.

  METHOD test_find_users.

    DATA:
      lv_json_response TYPE string,
      ls_query_params  TYPE zif_jira_user_search=>ts_query_params.

    " Set up mock request data
    ls_query_params = VALUE #( query = 'test_user_id' ).

    " Configure the mock HTTP client to return a predefined JSON response
    lv_json_response = '[{"accountId":"test_id"}]'.

    cl_abap_testdouble=>configure_call( go_mock_client
        )->ignore_all_parameters( )->returning( lv_json_response ).

    TRY.
        go_mock_client->call_api(
          EXPORTING
            iv_operation     = zif_jira_user_search=>gc_get_users ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    " Call the create_issue method on the CUT
    TRY.
        go_cut->zif_jira_user_search~find_users(
          EXPORTING
            is_query_params = ls_query_params
          IMPORTING
            et_user_details = DATA(ls_user_details) ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>fail( msg = 'Exception raised during find_users call' ).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act  = ls_user_details[]
        msg = 'API call to find_users did not return any response' ).

    " Assert that the response is as expected
    cl_abap_unit_assert=>assert_equals(
      act = ls_user_details[ 1 ]-account_id
      exp = 'test_id'
      msg = 'find_users did not return the expected Account ID' ).

  ENDMETHOD.

  METHOD test_find_users_exception.

    DATA:
      lv_json_response TYPE string,
      ls_query_params  TYPE zif_jira_user_search=>ts_query_params.

    " Set up mock request data
    ls_query_params = VALUE #( query = 'test_user_id' ).

    DATA(lo_exp_exception) = NEW zcx_jira_exceptions(
      code     = 500
      reason   = 'Test Reason' ).

    cl_abap_testdouble=>configure_call( go_mock_client
        )->ignore_all_parameters( )->raise_exception( lo_exp_exception ).

    TRY.
        go_mock_client->call_api(
          EXPORTING
            iv_operation     = zif_jira_user_search=>gc_get_users ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    " Call the create_issue method on the CUT
    TRY.
        go_cut->zif_jira_user_search~find_users(
          EXPORTING
            is_query_params = ls_query_params
          IMPORTING
            et_user_details = DATA(ls_user_details) ).

        cl_abap_unit_assert=>assert_initial(
          EXPORTING
            act  = ls_user_details
            msg  = 'Exception was not raised' ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act  = lo_exception
            exp  = lo_exp_exception
            msg  = 'Exception was not raised during find_users call'  ).
    ENDTRY.

*    " Insert mock test data for default user
*    insert_mock_test_data(
*      iv_bname = to_upper( 'vaibhago' )
*      iv_user_active = abap_true
*      iv_account_id = 'account_123' ).
*
*    " Can cover for test: User Not Found in Database and API Call Fails
*    try.
*        data(lv_account_id) = go_cut->zif_jira_user_search~get_jira_account_id( 'some_userid'  ).
*      catch zcx_jira_exceptions.
*        "handle exception
*    endtry.
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act  = 'account_123'
*        exp  = 'account_123'
*        msg  = 'Exception was not raised during find_users call'  ).

  ENDMETHOD.

  METHOD test_get_account_id_1.

    " Testing User Found in Database

    " Insert mock test data for an active user
    insert_mock_test_data(
      iv_bname = to_upper( 'john_doe' )
      iv_user_active = abap_true
      iv_account_id = 'account_123' ).

    DATA(lv_account_id) =
      go_cut->zif_jira_user_search~get_jira_account_id( to_upper( 'john_doe' ) ).

    " Assert that the account ID is as expected
    cl_abap_unit_assert=>assert_equals(
      act = lv_account_id
      exp = 'account_123'
      msg = 'Account ID was not found in the DB' ).

  ENDMETHOD.

  METHOD test_get_account_id_2.

    " Testing User Not Found in Database but Found via API
    DATA:
      lv_json_response TYPE string,
      ls_query_params  TYPE zif_jira_user_search=>ts_query_params.

    " Set up mock request data
    ls_query_params = VALUE #( query = 'test_user_id' ).

    " Configure the mock HTTP client to return a predefined JSON response
    lv_json_response = '[{"accountId":"test_id"}]'.

    cl_abap_testdouble=>configure_call( go_mock_client
        )->ignore_all_parameters( )->returning( lv_json_response ).

    TRY.
        go_mock_client->call_api(
          EXPORTING
            iv_operation     = zif_jira_user_search=>gc_get_users ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    DATA(lv_account_id) =
      go_cut->zif_jira_user_search~get_jira_account_id( to_upper( 'john_doe' ) ).

    " Assert that the account ID is as expected
    cl_abap_unit_assert=>assert_equals(
      act = lv_account_id
      exp = 'test_id'
      msg = 'Account ID was not found in the DB' ).

  ENDMETHOD.

ENDCLASS.
