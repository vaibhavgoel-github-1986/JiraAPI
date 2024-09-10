CLASS zcl_jira_issue_notify DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_jira_issue_notify.

    DATA:
      gt_config TYPE STANDARD TABLE OF tvarvc.

    DATA:
      go_issues_api      TYPE REF TO zif_jira_issues,
      go_user_search_api TYPE REF TO zif_jira_user_search.

    METHODS constructor
      IMPORTING
        VALUE(io_issues_api)      TYPE REF TO zif_jira_issues OPTIONAL
        VALUE(io_user_search_api) TYPE REF TO zif_jira_user_search OPTIONAL
      RAISING
        zcx_jira_exceptions .

  PROTECTED SECTION.
    METHODS:
      create_payload
        IMPORTING
          VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data
        EXPORTING
          es_create_request    TYPE zif_jira_issues=>ts_create_request
        RAISING
          zcx_jira_exceptions,

      create_panel_node
        IMPORTING
          VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data OPTIONAL
        RETURNING
          VALUE(ro_node)       TYPE REF TO zcl_jira_adf_builder,

      create_environment
        IMPORTING
          VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data OPTIONAL
        EXPORTING
          es_doc               TYPE zif_jira_adf_types=>ts_document,

      create_description_adf
        IMPORTING
          VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data OPTIONAL
        EXPORTING
          es_doc               TYPE zif_jira_adf_types=>ts_document,

      create_acceptance_adf
        IMPORTING
          VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data OPTIONAL
        EXPORTING
          es_doc               TYPE zif_jira_adf_types=>ts_document,

      create_text_node
        IMPORTING
          VALUE(iv_text)   TYPE string
          VALUE(iv_strong) TYPE abap_bool DEFAULT abap_false
          VALUE(iv_color)  TYPE string OPTIONAL
        RETURNING
          VALUE(ro_node)   TYPE REF TO zcl_jira_adf_builder,

      create_list_item
        IMPORTING
          VALUE(iv_label) TYPE string
          VALUE(iv_value) TYPE string
        RETURNING
          VALUE(ro_node)  TYPE REF TO zcl_jira_adf_builder.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_JIRA_ISSUE_NOTIFY IMPLEMENTATION.


  METHOD create_acceptance_adf.

    DATA: lt_nodes TYPE zif_jira_adf_types=>tt_nodes.

    CLEAR: es_doc.

    DATA(lo_text) = create_text_node( 'Please analyse the issue and update the RCA.' ).

    DATA(lo_paragraph1) = NEW zcl_jira_adf_builder(
       iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
       )->add_content( lo_text ).

    APPEND lo_paragraph1->get_node_data( ) TO lt_nodes.

*- Final ADF Document
    es_doc-version = 1.
    es_doc-type = zif_jira_adf_types=>gc_doc.
    es_doc-content = lt_nodes.

  ENDMETHOD.


  METHOD create_description_adf.

    DATA: lt_nodes TYPE zif_jira_adf_types=>tt_nodes.

    CLEAR: es_doc.

*- Error Panel
    APPEND create_panel_node( )->get_node_data( ) TO lt_nodes.

*- Bullet List for Notification Data
    DATA(lo_bullet_list) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-bulletlist ).

*- Adding Notification Data to the Bullet List
    IF is_notif_data-env IS NOT INITIAL.
      DATA(lo_list_item) = create_list_item(
                             iv_label = |Environment: |
                             iv_value = CONV #( is_notif_data-env ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-business_object IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Business Object: |
                             iv_value = CONV #( is_notif_data-business_object ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-source_system IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Source System: |
                             iv_value = CONV #( is_notif_data-source_system ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-target_system IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Target System: |
                             iv_value = CONV #( is_notif_data-target_system ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-api_name IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |BSL API Name: |
                             iv_value = is_notif_data-api_name ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-business_id IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Business ID: |
                             iv_value = CONV #( is_notif_data-business_id ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-corelation_id IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Correlation ID: |
                             iv_value = CONV #( is_notif_data-corelation_id ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-interface_id IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Interface ID: |
                             iv_value = CONV #( is_notif_data-interface_id ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-error_timestamp IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Error Timestamp: |
                             iv_value = is_notif_data-error_timestamp ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-code IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Error Code: |
                             iv_value = CONV #( is_notif_data-code ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-status IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Error Status: |
                             iv_value = CONV #( is_notif_data-status ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-error_message IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Error Message: |
                             iv_value = is_notif_data-error_message ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.

    IF is_notif_data-error_trace IS NOT INITIAL.
      lo_list_item = create_list_item(
                             iv_label = |Error Details: |
                             iv_value = CONV #( is_notif_data-error_trace ) ).
      lo_bullet_list->add_content( lo_list_item ).
      CLEAR: lo_list_item.
    ENDIF.


    APPEND lo_bullet_list->get_node_data( ) TO lt_nodes.

    DATA(lo_text) = create_text_node(
                      iv_text   = 'Please look into this issue ASAP.'
                      iv_strong = abap_false ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
      )->add_content( lo_text ).

    APPEND lo_paragraph->get_node_data( ) TO lt_nodes.

    CLEAR:
     lo_text,
     lo_paragraph.

    lo_text = create_text_node(
                  iv_text   = 'Thanks.'
                  iv_strong = abap_false ).

    lo_paragraph = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
      )->add_content( lo_text ).

    APPEND lo_paragraph->get_node_data( ) TO lt_nodes.

*- Final ADF Document
    es_doc-version = 1.
    es_doc-type = zif_jira_adf_types=>gc_doc.
    es_doc-content = lt_nodes.

  ENDMETHOD.


  METHOD create_list_item.

    DATA(lo_text_label) = create_text_node(
                          iv_text   = iv_label
                          iv_strong = abap_true ).

    DATA(lo_text_value) = create_text_node(
                           iv_text   = iv_value
                           iv_strong = abap_false ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
      )->add_content( lo_text_label
      )->add_content( lo_text_value ).

    DATA(lo_list_item) = NEW zcl_jira_adf_builder(
      iv_type  = zif_jira_adf_types=>gc_nodes-list_item
      )->add_content( lo_paragraph ).

    ro_node = lo_list_item.

  ENDMETHOD.


  METHOD create_payload.

    DATA:
      lv_msg    TYPE char255,
      ls_fields TYPE zif_jira_issues=>ts_fields.

    CLEAR:
     es_create_request.

    TRY.
*-     Priority, based on the System
        IF sy-sysid+0(1) = 'P'.
          ls_fields-priority-id = gt_config[ name = 'Z_JIRA_NOTIFY_PRIORITY' ]-low.
        ELSE.
          ls_fields-priority-id = gt_config[ name = 'Z_JIRA_NOTIFY_PRIORITY_NONPROD' ]-low.
        ENDIF.

*-     Assignee Account ID, based on the System
        IF sy-sysid+0(1) = 'P'.
          DATA(lv_assignee_id) = gt_config[ name = 'Z_JIRA_NOTIFY_ASSIGNEE' ]-low.
        ELSE.
          lv_assignee_id = gt_config[ name = 'Z_JIRA_NOTIFY_ASSIGNEE_NONPROD' ]-low.
        ENDIF.

        IF lv_assignee_id IS NOT INITIAL.
          TRY.
              ls_fields-assignee-account_id =
                go_user_search_api->get_jira_account_id( CONV #( lv_assignee_id ) ).
            CATCH zcx_jira_exceptions INTO DATA(lo_exception).
              RAISE EXCEPTION lo_exception.
          ENDTRY.
        ENDIF.

*-     Parent Issue ID
        ls_fields-parent-key = gt_config[ name = 'Z_JIRA_NOTIFY_PARENT_ISSUE' ]-low.

*-     Component ID
        ls_fields-components = VALUE #( (
           id = gt_config[ name = 'Z_JIRA_NOTIFY_COMP_ID' ]-low ) ).

*-     Summary Text (Title)
        ls_fields-summary = |SAP BRIM-{ sy-sysid }({ sy-mandt }): { is_notif_data-error_message } |.

*-     Severity ID, based on the System
        IF sy-sysid+0(1) = 'P'.
          ls_fields-customfield_10043-id = gt_config[ name = 'Z_JIRA_NOTIFY_SEVERITY' ]-low.
        ELSE.
          ls_fields-customfield_10043-id = gt_config[ name = 'Z_JIRA_NOTIFY_SEVERITY_NONPROD' ]-low.
        ENDIF.

*-     Scrum Team ID
        ls_fields-customfield_10035-id = gt_config[ name = 'Z_JIRA_NOTIFY_SCRUM_TEAM' ]-low.

*-     Issue Type
        ls_fields-issuetype-id =  gt_config[ name = 'Z_JIRA_NOTIFY_ISSUE_TYPE' ]-low.

*-     Project ID
        ls_fields-project-id =  gt_config[ name = 'Z_JIRA_NOTIFY_PROJECT_ID' ]-low.

*-     Story Points
        ls_fields-customfield_10026 = gt_config[ name = 'Z_JIRA_NOTIFY_STORY_POINTS' ]-low.

*-     Labels
        ls_fields-labels =  VALUE #( ( gt_config[ name = 'Z_JIRA_NOTIFY_LABEL' ]-low ) ).

      CATCH cx_sy_itab_line_not_found INTO DATA(lo_line_not_found).
        lv_msg =  lo_line_not_found->get_text( ).
        RAISE EXCEPTION TYPE zcx_jira_exceptions
          EXPORTING
            code   = 404
            reason = 'Not Found'
            msgv1  = lv_msg+0(50)
            msgv2  = lv_msg+50(50).
    ENDTRY.

*- Body Content
    create_description_adf(
      EXPORTING
        is_notif_data = is_notif_data
       IMPORTING
         es_doc    = ls_fields-description ).

*- Environment
    create_environment(
      IMPORTING
        es_doc        = ls_fields-environment ).

*- Acceptance Criteria
    create_acceptance_adf(
      IMPORTING
        es_doc    = ls_fields-customfield_10038 ).

*- Exporting Payload
    es_create_request-fields = ls_fields.

  ENDMETHOD.


  METHOD zif_jira_issue_notify~create_jira_issue.

    CLEAR:
     es_response.

    TRY.
*-     Create Payload Structure
        create_payload(
          EXPORTING
            is_notif_data     = is_notif_data
           IMPORTING
             es_create_request = DATA(ls_payload)
        ).
      CATCH zcx_jira_exceptions INTO DATA(lo_exception).
        RAISE EXCEPTION lo_exception.
    ENDTRY.

    TRY.
*-     Create Jira Issue
        go_issues_api->create_issue(
          EXPORTING
            is_request   = ls_payload
           IMPORTING
             es_response = es_response
        ).
      CATCH zcx_jira_exceptions INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD create_panel_node.

    DATA(lv_text) = |An error has occured in { sy-sysid }({ sy-mandt }) system|.

    DATA(lo_text) = create_text_node(
                      iv_text   = lv_text
                      iv_strong = abap_true
                      iv_color  = zif_jira_adf_types=>gc_colors-dark_red ).

    DATA(lo_paragraph) = NEW zcl_jira_adf_builder(
         iv_type = zif_jira_adf_types=>gc_nodes-paragraph
         )->add_content( lo_text ).

*- Warning Panel
    DATA(lo_panel) = NEW zcl_jira_adf_builder(
      iv_type = zif_jira_adf_types=>gc_nodes-panel
      is_attrs = VALUE #( panel_type = zif_jira_adf_types=>gc_panel_types-error )
       )->add_content( lo_paragraph ).

    ro_node = lo_panel.

  ENDMETHOD.


  METHOD constructor.

    CLEAR:
      gt_config,
      go_issues_api.

*- Dependency Injection
    IF io_issues_api IS INITIAL.
      go_issues_api = NEW zcl_jira_issues( ).
    ELSE.
      go_issues_api = io_issues_api.
    ENDIF.

    IF io_user_search_api IS INITIAL.
      go_user_search_api = NEW zcl_jira_user_search( ).
    ELSE.
      go_user_search_api = io_user_search_api.
    ENDIF.

*- Fetch Jira Config
    SELECT *
     FROM tvarvc
     INTO TABLE gt_config
     WHERE name LIKE 'Z_JIRA%'.
    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_jira_exceptions
        EXPORTING
          code   = 404
          reason = 'Not Found'
          msgv1  = 'Jira Config Not Found'.
    ENDIF.

  ENDMETHOD.


  METHOD create_environment.

    DATA: lt_nodes TYPE zif_jira_adf_types=>tt_nodes.

    CLEAR: es_doc.

    DATA(lo_text) = create_text_node( |{ sy-sysid }({ sy-mandt })| ).

    DATA(lo_paragraph1) = NEW zcl_jira_adf_builder(
       iv_type  = zif_jira_adf_types=>gc_nodes-paragraph
       )->add_content( lo_text ).

    APPEND lo_paragraph1->get_node_data( ) TO lt_nodes.

*- Final ADF Document
    es_doc-version = 1.
    es_doc-type = zif_jira_adf_types=>gc_doc.
    es_doc-content = lt_nodes.

  ENDMETHOD.


  METHOD create_text_node.

    DATA(lo_text) = NEW zcl_jira_adf_builder(
        iv_type  = zif_jira_adf_types=>gc_nodes-text
        iv_text  = iv_text ).

    IF iv_strong = abap_true.
      lo_text->add_marks(
            VALUE #( type = zif_jira_adf_types=>gc_mark_types-strong ) ).
    ENDIF.

    IF iv_color IS NOT INITIAL.
      lo_text->add_marks(
         VALUE #( type = zif_jira_adf_types=>gc_mark_types-text_color
                  attrs-color = iv_color ) ).
    ENDIF.

    ro_node = lo_text.

  ENDMETHOD.


  METHOD zif_jira_issue_notify~update_error_logs.

    DATA:
      lt_error_log    TYPE STANDARD TABLE OF zdt_outbound_log,
      ls_outbound_log TYPE zdt_outbound_log,
      lv_url          TYPE string,
      lv_json         TYPE string,
      ls_slg1_obj     TYPE bal_s_log,
      lt_slg1_log     TYPE balmi_t,
      ls_slg1_msg     TYPE balmi.

    CONSTANTS:
      lc_one      TYPE char1  VALUE '1',
      lc_slg1_obj TYPE string VALUE 'ZCISCO',
      lc_msgno    TYPE string VALUE '000',
      lc_msgid    TYPE string VALUE 'ZMC_LOG',
      lc_error    TYPE char1  VALUE 'E',
      lc_bus_type TYPE char10 VALUE 'JIRA_API'.

*- JSON Format
    lv_json = /ui2/cl_json=>serialize(
                EXPORTING
                 data = is_notif_data
                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

*- Outbound Error Logs
    ls_outbound_log-interface_id   = is_notif_data-interface_id.
    ls_outbound_log-e_date         = sy-datum.
    ls_outbound_log-e_time         = sy-uzeit.
    ls_outbound_log-counter        = lc_one.
    ls_outbound_log-bus_type       = lc_bus_type.
    ls_outbound_log-status         = lc_error.
    ls_outbound_log-message        = iv_error_msg.
    ls_outbound_log-data           = lv_json.
    ls_outbound_log-processed_by   = sy-uname.
    ls_outbound_log-processed_date = sy-datum.
    ls_outbound_log-processed_time = sy-uzeit.

    MODIFY zdt_outbound_log FROM ls_outbound_log.
    CLEAR ls_outbound_log.

*- Create SLG1 log
    ls_slg1_obj-object    = lc_slg1_obj.
    ls_slg1_obj-subobject = is_notif_data-interface_id.
    ls_slg1_obj-extnumber = is_notif_data-business_id.
    ls_slg1_msg-msgty = lc_error.
    ls_slg1_msg-msgid = lc_msgid.
    ls_slg1_msg-msgno = lc_msgno.
    ls_slg1_msg-msgv1 = iv_error_msg+0(50).
    ls_slg1_msg-msgv2 = iv_error_msg+50(50).
    APPEND ls_slg1_msg TO lt_slg1_log.
    CLEAR: ls_slg1_msg.

    CALL METHOD zcl_global_utilities=>create_slg1_log_from_itab
      EXPORTING
        is_bal_s_log = ls_slg1_obj
        it_tab_msg   = lt_slg1_log.

  ENDMETHOD.
ENDCLASS.
