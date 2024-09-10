CLASS ltcl_jira_http_client DEFINITION DEFERRED.
CLASS zcl_jira_http_client DEFINITION LOCAL FRIENDS ltcl_jira_http_client.

CLASS ltcl_jira_http_client DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FRIENDS zcl_jira_http_client.

  PRIVATE SECTION.

    DATA:
      go_cut       TYPE REF TO zcl_jira_http_client.       "Class Under Test

    CLASS-DATA:
      go_lcl_mock_client TYPE REF TO lcl_mock_http_client.

    CLASS-DATA:
      go_osql_test TYPE REF TO if_osql_test_environment.  "SQL Test Double

    CLASS-METHODS:
      " Method to set up the test environment once before executing all the tests
      class_setup.

    CLASS-METHODS:
      " Method called once after the last tear down method
      class_teardown.

    METHODS:
      " Method is called before every test method
      setup,

      " Method is called after every test method
      teardown,

      " Method to test a simple GET API call
      test_simple_get_call FOR TESTING,

      " Method to test a simple POST API call
      test_simple_post_call FOR TESTING,

      " Method to test all the exception handling during API calls
      test_tvarvc_query_exception FOR TESTING,

      " Method to test all the exception handling during API calls
      test_send_exception FOR TESTING,

      " Method to test all the exception handling during API calls
      test_receive_exception FOR TESTING,

      " Method to test in case of Error Code
      test_error_exception FOR TESTING,

      " Method to test exception handling for create_http_client
      test_http_client_exception FOR TESTING,

      " Method to test parsing of error messages from a JSON array
      test_parse_msg_array FOR TESTING,

      " Method to test parsing of error messages from a JSON object
      test_parse_msg_errors FOR TESTING,

      " Method to test handling of JSON responses that do not contain error messages
      test_parse_msg_no_error FOR TESTING.

    METHODS:
      " Insert Mock Data to the oSQL Test Double
      insert_mock_test_data.

    CLASS-METHODS:
      " Create the oSQL Test Environment
      create_class_test_double
        RETURNING
          VALUE(ro_test_double) TYPE REF TO if_http_client.

ENDCLASS.

CLASS ltcl_jira_http_client IMPLEMENTATION.

  METHOD class_setup.

*- SQL Test Double, should be created only once
    go_osql_test = cl_osql_test_environment=>create(
                    i_dependency_list = VALUE #( ( 'TVARVC' ) ) ).

  ENDMETHOD.

  METHOD class_teardown.

    go_osql_test->destroy( ).

  ENDMETHOD.

  METHOD setup.

*- Creating Object for the CUT Class
    go_cut = NEW zcl_jira_http_client( ).

*- Mock Client
    go_lcl_mock_client = NEW lcl_mock_http_client( ).

*- Clear the Doubles
    go_osql_test->clear_doubles( ).

  ENDMETHOD.

  METHOD teardown.

    CLEAR:
     go_cut,
     go_lcl_mock_client.

  ENDMETHOD.

  METHOD test_simple_get_call.

*- Insert Mock Data for oSQL Test Double
    insert_mock_test_data( ).

*- Creating Test Doubles for Injection
    TEST-INJECTION create_http_client.
      lo_http_client = ltcl_jira_http_client=>create_class_test_double( ).

*-   Configure get_status
      cl_abap_testdouble=>configure_call( lo_http_client->response
      )->set_parameter(
        EXPORTING
          name  = 'CODE'
          value = 200 ).

      lo_http_client->response->get_status( ).

*-   Configure get_cdata
      cl_abap_testdouble=>configure_call( lo_http_client->response
      )->returning( '{some: response}'  ).

      lo_http_client->response->get_cdata( ).

    END-TEST-INJECTION.

    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation     = '/rest/api/3/issue/TEST-123'
          RECEIVING
            rv_json_response = DATA(lv_json_response)
        ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_json_response
        exp = '{some: response}'
        msg = 'Simple GET call not working as expected' ).

  ENDMETHOD.

  METHOD test_simple_post_call.

*- Insert Mock Data for oSQL Test Double
    insert_mock_test_data( ).

*- Creating Test Doubles for Injection
    TEST-INJECTION create_http_client.
      lo_http_client = ltcl_jira_http_client=>create_class_test_double( ).

*-   Configure get_status
      cl_abap_testdouble=>configure_call( lo_http_client->response
      )->set_parameter(
        EXPORTING
          name  = 'CODE'
          value = 201 ).

      lo_http_client->response->get_status( ).

*-   Configure get_cdata
      cl_abap_testdouble=>configure_call( lo_http_client->response
      )->returning( '{key: ISSUE_1001}'  ).

      lo_http_client->response->get_cdata( ).

    END-TEST-INJECTION.

    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation  = '/rest/api/3/issue'
            iv_method     = 'POST'
            iv_cdata      = '{fields: {priority: 4 } }'
          RECEIVING
            rv_json_response = DATA(lv_json_response)
        ).
      CATCH zcx_jira_exceptions.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_json_response
        exp = '{key: ISSUE_1001}'
        msg = 'Simple POST call not working as expected' ).

  ENDMETHOD.

  METHOD test_tvarvc_query_exception.

*- Since we just need to bypass Create Call
    TEST-INJECTION create_http_client.
      sy-subrc = 0.
    END-TEST-INJECTION.

*- For Get API Key Query Exception
    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation    = '/rest/api/3/issue/TEST-123'
            iv_query_params = 'some_params' ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lo_exception->code
            exp   = 404
            msg   = 'Get API Key not raising any exception' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_parse_msg_array.

    DATA: lv_json_response TYPE string.

    " Test case for errorMessages array
    lv_json_response = '{"errorMessages":["Issue not found"]}'.

    DATA(lv_error_message) = go_cut->parse_error_message( lv_json_response ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_error_message
      exp = 'Issue not found'
      msg = 'Error message extraction from array failed' ).

  ENDMETHOD.

  METHOD test_parse_msg_errors.

    DATA: lv_json_response TYPE string.

    " Test case for errors object
    lv_json_response = '{"errors":{"projectKey":"Project key is required."}}'.

    DATA(lv_error_message) = go_cut->parse_error_message( lv_json_response ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_error_message
      exp = 'Project key is required.'
      msg = 'Error message extraction from errors object failed' ).

  ENDMETHOD.

  METHOD test_parse_msg_no_error.

    DATA: lv_json_response TYPE string.

    " Test case for no error message in the response
    lv_json_response = '{"someOtherField":"value"}'.

    DATA(lv_error_message) = go_cut->parse_error_message(
         lv_json_response ).

    cl_abap_unit_assert=>assert_initial(
      act = lv_error_message
      msg = 'Error message should be initial when no error is found' ).

  ENDMETHOD.

  METHOD create_class_test_double.

*- Creating a Test Double for Injection
    ro_test_double ?= cl_abap_testdouble=>create(
                       object_name ='IF_HTTP_CLIENT' ).

*- Creating Test Doubles for Injection
    ro_test_double->request ?= cl_abap_testdouble=>create(
                   object_name = 'IF_HTTP_REQUEST' ).

    ro_test_double->response ?= cl_abap_testdouble=>create(
                   object_name = 'IF_HTTP_RESPONSE' ).

  ENDMETHOD.

  METHOD insert_mock_test_data.

    DATA: lt_tvarvc TYPE STANDARD TABLE OF tvarvc.

*- Mock Data
    lt_tvarvc = VALUE #( ( name = 'ZJIRA_API_KEY'
                           type = 'P' low = 'Dummy_Value' ) ).

    go_osql_test->insert_test_data(
      EXPORTING
        i_data = lt_tvarvc ).

  ENDMETHOD.

  METHOD test_receive_exception.

*- Now insert the mock data for TVARVC query to work
    insert_mock_test_data( ).

*- Injecting our Fake Implementation instead of OO Test Double
    TEST-INJECTION create_http_client.
      lo_http_client = ltcl_jira_http_client=>go_lcl_mock_client.

*-   Creating Test Doubles for Injection to avoid null reference issue
      lo_http_client->request ?= cl_abap_testdouble=>create(
                     object_name = 'IF_HTTP_REQUEST' ).

      lo_http_client->response ?= cl_abap_testdouble=>create(
                      object_name = 'IF_HTTP_RESPONSE' ).
    END-TEST-INJECTION.

*- Settings Parameters for SEND method to raise exception
    go_lcl_mock_client->gv_fail_receive = abap_true.

    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation     = '/rest/api/3/issue/TEST-123' ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lo_exception->code
            exp   = 501
            msg   = 'HTTP Send Request Failed' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_send_exception.

*- Now insert the mock data for TVARVC query to work
    insert_mock_test_data( ).

*- Injecting our Fake Implementation instead of OO Test Double
    TEST-INJECTION create_http_client.
      lo_http_client = ltcl_jira_http_client=>go_lcl_mock_client.

*-   Creating Test Doubles for Injection to avoid null reference issue
      lo_http_client->request ?= cl_abap_testdouble=>create(
                     object_name = 'IF_HTTP_REQUEST' ).

      lo_http_client->response ?= cl_abap_testdouble=>create(
                      object_name = 'IF_HTTP_RESPONSE' ).
    END-TEST-INJECTION.

*- Settings Parameters for SEND method to raise exception
    go_lcl_mock_client->gv_fail_send = abap_true.

    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation     = '/rest/api/3/issue/TEST-123' ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lo_exception->code
            exp   = 502
            msg   = 'HTTP Send Request Failed' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_error_exception.

*- Now insert the mock data for TVARVC query to work
    insert_mock_test_data( ).

*- Injecting our Fake Implementation instead of OO Test Double
    TEST-INJECTION create_http_client.
      lo_http_client = ltcl_jira_http_client=>go_lcl_mock_client.

*-   Creating Test Doubles for Injection to avoid null reference issue
      lo_http_client->request ?= cl_abap_testdouble=>create(
                     object_name = 'IF_HTTP_REQUEST' ).

      lo_http_client->response ?= cl_abap_testdouble=>create(
                      object_name = 'IF_HTTP_RESPONSE' ).

*-   Configure get_status
      cl_abap_testdouble=>configure_call( lo_http_client->response
      )->set_parameter(
        EXPORTING
          name  = 'CODE'
          value = 300 ).

      lo_http_client->response->get_status( ).
    END-TEST-INJECTION.

    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation     = '/rest/api/3/issue/TEST-123' ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lo_exception->code
            exp   = 300
            msg   = 'HTTP Receive Request Failed' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_http_client_exception.

*- Injecting our Fake Implementation instead of OO Test Double
    TEST-INJECTION create_http_client.
      sy-subrc = 4.
    END-TEST-INJECTION.

    TRY.
*-     Call the CUT Method
        go_cut->zif_jira_http_client~call_api(
          EXPORTING
            iv_operation     = '/rest/api/3/issue/TEST-123' ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lo_exception->code
            exp   = 400
            msg   = 'HTTP Receive Request Failed' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
