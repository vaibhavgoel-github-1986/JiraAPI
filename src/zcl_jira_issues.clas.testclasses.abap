CLASS ltcl_jira_issues DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      go_cut              TYPE REF TO zcl_jira_issues,        "Class Under Test
      go_mock_http_client TYPE REF TO zif_jira_http_client,   "Mock HTTP Client
      go_mock_utility     TYPE REF TO zif_jira_utils.         "Mock Utility

    METHODS:
      " Method to prepare the CUT (Class Under Test) and clear test doubles before each test method
      setup,

      " Method to clear the CUT object after each test method
      teardown,

      " Method to test the creation of a JIRA issue using the API
      test_create_issue FOR TESTING,
      test_create_issue_exception FOR TESTING,

      " Method to test the search of JIRA issues using JQL
      test_search_issue_by_jql FOR TESTING,
      test_search_issue_exception FOR TESTING.

    METHODS:
      " Method to create test doubles for the HTTP client and utility interfaces
      create_test_doubles.

ENDCLASS.

CLASS ltcl_jira_issues IMPLEMENTATION.

  METHOD setup.

    " Create test doubles for the HTTP client and utility interfaces
    create_test_doubles( ).

    " Create an instance of the CUT class with injected dependencies
    go_cut = NEW zcl_jira_issues(
                io_http_client  = go_mock_http_client
                io_jira_utility = go_mock_utility ).

  ENDMETHOD.

  METHOD teardown.

    " Clear the CUT object
    CLEAR:
      go_cut,
      go_mock_http_client,
      go_mock_utility.

  ENDMETHOD.

  METHOD test_create_issue.

    DATA: ls_request       TYPE zif_jira_issues=>ts_create_request,
          ls_response      TYPE zif_jira_issues=>ts_create_response,
          lv_json_response TYPE string.

    " Set up mock request data
    ls_request = VALUE #( fields-summary = 'This is a test issue.' ).

    " Configure the mock HTTP client to return a predefined JSON response
    lv_json_response = '{"id":"test_id","key":"test_key","self":"self_mock"}'.

    cl_abap_testdouble=>configure_call( go_mock_http_client
        )->ignore_all_parameters( )->returning( lv_json_response ).

    TRY.
        go_mock_http_client->call_api(
          EXPORTING
            iv_operation     = zif_jira_issues=>gc_post_issue ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    " Call the create_issue method on the CUT
    TRY.
        go_cut->zif_jira_issues~create_issue(
          EXPORTING
            is_request  = ls_request
          IMPORTING
            es_response = ls_response ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>fail( msg = 'Exception raised during create_issue call' ).
    ENDTRY.

    " Assert that the response is as expected
    cl_abap_unit_assert=>assert_equals(
      act = ls_response-key
      exp = 'test_key'
      msg = 'Create issue did not return the expected key' ).

  ENDMETHOD.

  METHOD test_create_issue_exception.

    DATA: ls_request TYPE zif_jira_issues=>ts_create_request.

    " Set up mock request data
    ls_request = VALUE #( fields-summary = 'This is a test issue.' ).

    DATA(lo_exp_exception) = NEW zcx_jira_exceptions(
      code     = 500
      reason   = 'Test Reason' ).

    cl_abap_testdouble=>configure_call( go_mock_http_client
        )->ignore_all_parameters( )->raise_exception( lo_exp_exception ).

    TRY.
        go_mock_http_client->call_api(
          EXPORTING
            iv_operation     = zif_jira_issues=>gc_post_issue ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    " Call the create_issue method on the CUT
    TRY.
        go_cut->zif_jira_issues~create_issue(
          EXPORTING
            is_request  = ls_request
          IMPORTING
            es_response = DATA(ls_response) ).

        cl_abap_unit_assert=>assert_initial(
          EXPORTING
            act  = ls_response
            msg  = 'Exception was not raised' ).

      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act  = lo_exception
            exp  = lo_exp_exception
            msg  = 'Exception was not raised during create_issue call'  ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_search_issue_by_jql.

    DATA: ls_query_params  TYPE zif_jira_issues=>ts_query_params,
          lv_json_response TYPE string.

    " Set up mock query parameters
    ls_query_params = VALUE #( jql = 'project = TEST AND status = Open' ).

    " Configure the mock HTTP client to return a predefined JSON response
    lv_json_response = '{"issues": [{"id": "test_id", "key": "test_key", "fields": {"summary": "Test Issue"}}]}'.

    cl_abap_testdouble=>configure_call( go_mock_http_client
      )->ignore_all_parameters( )->returning( lv_json_response ).

    TRY.
        go_mock_http_client->call_api(
          EXPORTING
            iv_operation     = 'SEARCH' ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    " Call the search_issue_by_jql method on the CUT
    TRY.
        go_cut->zif_jira_issues~search_issue_by_jql(
          EXPORTING
            is_query_params = ls_query_params
          IMPORTING
            es_response = DATA(ls_response) ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>fail( msg = 'Exception raised during search_issue_by_jql call' ).
    ENDTRY.

    " Assert that the response contains the expected issue key
    cl_abap_unit_assert=>assert_equals(
      act = ls_response-issues[ 1 ]-key
      exp = 'test_key'
      msg = 'Search issue did not return the expected key' ).

  ENDMETHOD.

  METHOD test_search_issue_exception.

    DATA: ls_query_params  TYPE zif_jira_issues=>ts_query_params.

    " Set up mock query parameters
    ls_query_params = VALUE #( jql = 'project = TEST AND status = Open' ).

    DATA(lo_exp_exception) = NEW zcx_jira_exceptions(
      code     = 500
      reason   = 'Test Reason' ).

    cl_abap_testdouble=>configure_call( go_mock_http_client
        )->ignore_all_parameters( )->raise_exception( lo_exp_exception ).

    TRY.
        go_mock_http_client->call_api(
          EXPORTING
            iv_operation     = zif_jira_issues=>gc_post_issue ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    " Call the create_issue method on the CUT
    TRY.
        go_cut->zif_jira_issues~search_issue_by_jql(
          EXPORTING
            is_query_params = ls_query_params
          IMPORTING
            es_response = DATA(ls_response) ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act  = lo_exception
            exp  = lo_exp_exception
            msg  = 'Exception was not raised during create_issue call'  ).
    ENDTRY.

  ENDMETHOD.

  METHOD create_test_doubles.

    " Create test doubles for the HTTP client and utility interfaces
    go_mock_http_client ?= cl_abap_testdouble=>create( object_name = 'ZIF_JIRA_HTTP_CLIENT' ).
    go_mock_utility     ?= cl_abap_testdouble=>create( object_name = 'ZIF_JIRA_UTILS' ).

  ENDMETHOD.

ENDCLASS.
