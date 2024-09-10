CLASS ltcl_jira_utility DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      go_cut TYPE REF TO zcl_jira_utility.  "Class Under Test

    METHODS: setup.
    METHODS: teardown.

    METHODS: test_valid_date FOR TESTING.
    METHODS: test_empty_query_params FOR TESTING.
    METHODS: test_simple_query_params FOR TESTING.
    METHODS: test_complex_query_params FOR TESTING.
    METHODS: test_to_camel_case FOR TESTING.
ENDCLASS.


CLASS ltcl_jira_utility IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT go_cut.
  ENDMETHOD.

  METHOD teardown.
    CLEAR go_cut.
  ENDMETHOD.

  METHOD test_valid_date.

    DATA:
      lv_input_date    TYPE sydatum,
      lv_expected_date TYPE string,
      lv_result_date   TYPE string.

    " Valid test case:
    lv_input_date = '20240520'.    " Example input date
    lv_expected_date = '2024-05-20'.  " Expected Jira format

    lv_result_date = go_cut->zif_jira_utils~convert_to_jira_date_format(
                       lv_input_date ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result_date
      exp = lv_expected_date
      msg = 'Conversion to Jira Date Format Failed'
    ).

  ENDMETHOD.

  METHOD test_empty_query_params.

    DATA: ls_query_params TYPE zif_jira_issues=>ts_query_params. " Empty structure

    DATA(lv_actual) = go_cut->zif_jira_utils~construct_query_string( ls_query_params ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_actual
      exp = ''
      msg = 'Empty query params should result in an empty string'
    ).
  ENDMETHOD.

  METHOD test_simple_query_params.

    DATA:
      ls_query_params TYPE zif_jira_issues=>ts_query_params,
      lv_expected     TYPE string.

    ls_query_params-fields = 'status'.
    ls_query_params-jql  = |key in ('OI098','OIU-988')|.
    lv_expected = |jql=key in ('OI098','OIU-988')&fields=status|.

    DATA(lv_actual) = go_cut->zif_jira_utils~construct_query_string( ls_query_params ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_actual
      exp = lv_expected
      msg = 'Simple query params conversion failed'
    ).

  ENDMETHOD.


  METHOD test_complex_query_params.

    TYPES:
      BEGIN OF lts_params,
        projectkey TYPE string,
        issuetype  TYPE string,
        status     TYPE string,
        summary    TYPE string,
      END OF lts_params.

    DATA:
      ls_query_params TYPE lts_params,
      lv_expected     TYPE string.

    ls_query_params-projectkey = 'TEST'.
    ls_query_params-issuetype  = 'Bug'.
    ls_query_params-status    = 'Open'.
    lv_expected = 'projectkey=TEST&issuetype=Bug&status=Open'.

    DATA(lv_actual) = go_cut->zif_jira_utils~construct_query_string( ls_query_params ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_actual
      exp = lv_expected
      msg = 'Complex query params conversion failed'
    ).
  ENDMETHOD.

  METHOD test_to_camel_case.
    DATA: lv_input_string    TYPE string,
          lv_expected_string TYPE string,
          lv_result_string   TYPE string.

    " Test cases for to_camel_case
    lv_input_string = 'project_key'.
    lv_expected_string = 'projectKey'.
    lv_result_string = go_cut->zif_jira_utils~to_camel_case( lv_input_string ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result_string
      exp = lv_expected_string
      msg = 'Conversion failed for simple input'
    ).

    lv_input_string = 'issue_type_name'.
    lv_expected_string = 'issueTypeName'.
    lv_result_string = go_cut->zif_jira_utils~to_camel_case( lv_input_string ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result_string
      exp = lv_expected_string
      msg = 'Conversion failed for multiple underscores'
    ).

    lv_input_string = 'UPPERCASENAME'.
    lv_expected_string = 'uppercasename'.
    lv_result_string = go_cut->zif_jira_utils~to_camel_case( lv_input_string ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result_string
      exp = lv_expected_string
      msg = 'Conversion failed for already camel case input'
    ).

    lv_input_string = 'issue_type_name_with_numbers123'.
    lv_expected_string = 'issueTypeNameWithNumbers123'.
    lv_result_string = go_cut->zif_jira_utils~to_camel_case( lv_input_string ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result_string
      exp = lv_expected_string
      msg = 'Conversion failed for input with numbers'
    ).

    lv_input_string = 'this_is_a_longer_string_with_many_parts'.
    lv_expected_string = 'thisIsALongerStringWithManyParts'.
    lv_result_string = go_cut->zif_jira_utils~to_camel_case( lv_input_string ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result_string
      exp = lv_expected_string
      msg = 'Conversion failed for a long input string'
    ).

  ENDMETHOD.

ENDCLASS.
