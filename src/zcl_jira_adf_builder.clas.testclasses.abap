CLASS ltcl_jira_adf_builder_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      go_cut TYPE REF TO zcl_jira_adf_builder.

    METHODS:
      " Set up and tear down methods
      setup,
      teardown,

      " Test methods for each public method in the class
      test_constructor FOR TESTING,
      test_set_type FOR TESTING,
      test_set_text FOR TESTING,
      test_set_attrs FOR TESTING,
      test_add_marks FOR TESTING,
      test_add_content FOR TESTING,
      test_get_node_data FOR TESTING.

ENDCLASS.

CLASS ltcl_jira_adf_builder_test IMPLEMENTATION.

  METHOD setup.
    " Create an instance of the class under test (CUT)
    go_cut = NEW zcl_jira_adf_builder( ).
  ENDMETHOD.

  METHOD teardown.
    " Clear the CUT instance
    CLEAR go_cut.
  ENDMETHOD.

  METHOD test_constructor.
    " Test the constructor method

    " Create a new instance with specific parameters
    DATA(lo_builder) = NEW zcl_jira_adf_builder(
      iv_type = 'paragraph'
      iv_text = 'Sample text'
      is_attrs = VALUE #( layout = 'layout' )
      it_marks = VALUE #( ( type = 'textColor'
                            attrs-color = zif_jira_adf_types=>gc_colors-orange ) ) ).

    " Get the node data and verify
    DATA(ls_node) = lo_builder->get_node_data( ).

    " Assertions
    cl_abap_unit_assert=>assert_equals(
      act = ls_node-type
      exp = 'paragraph'
      msg = 'Type is not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-text
      exp = 'Sample text'
      msg = 'Text is not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-attrs-layout
      exp = 'layout'
      msg = 'Attributes are not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-marks[ 1 ]-type
      exp = 'textColor'
      msg = 'Marks are not as expected' ).

  ENDMETHOD.

  METHOD test_set_type.
    " Test the set_type method

    " Set the type and get the returned object
    DATA(lo_self) = go_cut->set_type( 'heading' ).

    " Verify the type
    DATA(ls_node) = go_cut->get_node_data( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-type
      exp = 'heading'
      msg = 'Type is not as expected' ).

    " Verify the method returns self
    cl_abap_unit_assert=>assert_equals(
      act = lo_self
      exp = go_cut
      msg = 'Returned object is not self' ).
  ENDMETHOD.

  METHOD test_set_text.
    " Test the set_text method

    " Set the text and get the returned object
    DATA(lo_self) = go_cut->set_text( 'New text' ).

    " Verify the text
    DATA(ls_node) = go_cut->get_node_data( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-text
      exp = 'New text'
      msg = 'Text is not as expected' ).

    " Verify the method returns self
    cl_abap_unit_assert=>assert_equals(
      act = lo_self
      exp = go_cut
      msg = 'Returned object is not self' ).
  ENDMETHOD.

  METHOD test_set_attrs.
    " Test the set_attrs method

    " Set the attributes and get the returned object
    DATA(lo_self) = go_cut->set_attrs( VALUE #( layout = 'new_value' ) ).

    " Verify the attributes
    DATA(ls_node) = go_cut->get_node_data( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-attrs-layout
      exp = 'new_value'
      msg = 'Attributes are not as expected' ).

    " Verify the method returns self
    cl_abap_unit_assert=>assert_equals(
      act = lo_self
      exp = go_cut
      msg = 'Returned object is not self' ).
  ENDMETHOD.

  METHOD test_add_marks.
    " Test the add_marks method

    " Add marks and get the returned object
    DATA(lo_self) = go_cut->add_marks( VALUE #( type = zif_jira_adf_types=>gc_mark_types-strong  ) ).

    " Verify the marks
    DATA(ls_node) = go_cut->get_node_data( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-marks[ 1 ]-type
      exp = zif_jira_adf_types=>gc_mark_types-strong
      msg = 'Marks are not as expected' ).

    " Verify the method returns self
    cl_abap_unit_assert=>assert_equals(
      act = lo_self
      exp = go_cut
      msg = 'Returned object is not self' ).

  ENDMETHOD.

  METHOD test_add_content.
    " Test the add_content method

    " Create a child node
    DATA(lo_child) = NEW zcl_jira_adf_builder( iv_type = 'child_type' iv_text = 'Child text' ).

    " Add content and get the returned object
    DATA(lo_self) = go_cut->add_content( lo_child ).

    " Verify the content
    DATA(ls_node) = go_cut->get_node_data( ).
    FIELD-SYMBOLS: <lt_content> TYPE zif_jira_adf_types=>tt_nodes.
    ASSIGN ls_node-content->* TO <lt_content>.

    cl_abap_unit_assert=>assert_not_initial(
      act = <lt_content>
      msg = 'Content is not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = <lt_content>[ 1 ]-type
      exp = 'child_type'
      msg = 'Child type is not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = <lt_content>[ 1 ]-text
      exp = 'Child text'
      msg = 'Child text is not as expected' ).

    " Verify the method returns self
    cl_abap_unit_assert=>assert_equals(
      act = lo_self
      exp = go_cut
      msg = 'Returned object is not self' ).
  ENDMETHOD.

  METHOD test_get_node_data.
    " Test the get_node_data method

    " Set some properties
    go_cut->set_type( 'node_type' )->set_text( 'node_text'
      )->set_attrs( VALUE #( color = zif_jira_adf_types=>gc_colors-blue ) ).

    " Get the node data
    DATA(ls_node) = go_cut->get_node_data( ).

    " Verify the node data
    cl_abap_unit_assert=>assert_equals(
      act = ls_node-type
      exp = 'node_type'
      msg = 'Type is not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-text
      exp = 'node_text'
      msg = 'Text is not as expected' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_node-attrs-color
      exp = zif_jira_adf_types=>gc_colors-blue
      msg = 'Attributes are not as expected' ).

  ENDMETHOD.

ENDCLASS.
