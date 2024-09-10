CLASS ltcl_jira_issue_notify DEFINITION DEFERRED.
CLASS zcl_jira_issue_notify DEFINITION LOCAL FRIENDS ltcl_jira_issue_notify.

CLASS ltcl_jira_issue_notify DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      go_cut TYPE REF TO zcl_jira_issue_notify.             "Class Under Test

    DATA:
      go_otd_jira_issues TYPE REF TO zif_jira_issues,      "OO Test Doubles
      go_otd_user_search TYPE REF TO zif_jira_user_search.

    CLASS-DATA:
       go_osql_test TYPE REF TO if_osql_test_environment.   "SQL Test Double

    CLASS-METHODS:
      " Method to set up the test environment for the class
      class_setup,

      " Method to clean up the test environment after all tests in the class have run
      class_teardown,

      " Insert mock TVARVC Data for oSQL Double
      insert_mock_test_data.

    METHODS:
      " Method to prepare the CUT (Class Under Test) and clear test doubles before each test method
      setup,

      " Method to clear the CUT object after each test method
      teardown.

    METHODS:
      test_create_panel_node FOR TESTING.

ENDCLASS.


CLASS ltcl_jira_issue_notify IMPLEMENTATION.

  METHOD class_setup.
    " Initialize the oSQL test environment
    go_osql_test = cl_osql_test_environment=>create(
                    i_dependency_list = VALUE #( ( 'TVARVC' ) ) ).

    " Insert Mock Data
    insert_mock_test_data( ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up the oSQL test environment
    go_osql_test->destroy( ).
  ENDMETHOD.

  METHOD setup.

*- Creating Test Double for HTTP Client Class
    go_otd_jira_issues ?= cl_abap_testdouble=>create( object_name = 'ZIF_JIRA_ISSUES' ).
    go_otd_user_search ?= cl_abap_testdouble=>create( object_name = 'ZIF_JIRA_USER_SEARCH' ).

    " Create an instance of the CUT class
    go_cut = NEW zcl_jira_issue_notify(
       io_issues_api      = go_otd_jira_issues
       io_user_search_api = go_otd_user_search
    ).

  ENDMETHOD.

  METHOD teardown.
    " Clear the CUT object
    CLEAR: go_cut.

    " Clear any previous test doubles
    go_osql_test->clear_doubles( ).
  ENDMETHOD.

  METHOD test_create_panel_node.

*- Calling CUT Method
    DATA(lo_panel) = go_cut->create_panel_node( ).

*- Checking for expected values
    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = lo_panel
        msg = 'Panel object not bound' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lo_panel->get_node_data( )-type
        exp = zif_jira_adf_types=>gc_nodes-panel
        msg  = 'Node type mismatch' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lo_panel->get_node_data( )-attrs-panel_type
        exp = zif_jira_adf_types=>gc_panel_types-error
        msg  = 'Panel type mismatch' ).

  ENDMETHOD.

  METHOD insert_mock_test_data.

    DATA: lt_data TYPE STANDARD TABLE OF tvarvc.

    lt_data = VALUE #(
      ( name = 'Z_JIRA_NOTIFY_ASSIGNEE'         type = 'P' low = 'test_id' )
      ( name = 'Z_JIRA_NOTIFY_ASSIGNEE_NONPROD' type = 'P' low = 'test_id_nonprod' )
      ( name = 'Z_JIRA_NOTIFY_COMP_ID'          type = 'P' low = 'comp_id' )
      ( name = 'Z_JIRA_NOTIFY_ISSUE_TYPE'       type = 'P' low = 'issue_type' )
      ( name = 'Z_JIRA_NOTIFY_LABEL'            type = 'P' low = 'Label_test' )
      ( name = 'Z_JIRA_NOTIFY_PARENT_ISSUE'     type = 'P' low = 'Parent_issue' )
      ( name = 'Z_JIRA_NOTIFY_PRIORITY'         type = 'P' low = '2' )
      ( name = 'Z_JIRA_NOTIFY_PRIORITY_NONPROD' type = 'P' low = '3' )
      ( name = 'Z_JIRA_NOTIFY_PROJECT_ID'       type = 'P' low = 'Test_10122' )
      ( name = 'Z_JIRA_NOTIFY_SCRUM_TEAM'       type = 'P' low = 'Test_25690' )
      ( name = 'Z_JIRA_NOTIFY_SEVERITY'         type = 'P' low = 'Test_10105' )
      ( name = 'Z_JIRA_NOTIFY_SEVERITY_NONPROD' type = 'P' low = 'Test_10107' )
      ( name = 'Z_JIRA_NOTIFY_STORY_POINTS'     type = 'P' low = '2.0' ) ).

    " Insert mock data into the oSQL test environment
    go_osql_test->insert_test_data(
      EXPORTING
        i_data = lt_data ).

  ENDMETHOD.

ENDCLASS.
