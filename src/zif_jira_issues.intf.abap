INTERFACE zif_jira_issues

  PUBLIC .

  TYPES:
    BEGIN OF ts_id,
      id TYPE string,
    END OF ts_id,

    BEGIN OF ts_account_id,
      account_id    TYPE string,
      display_name  TYPE string,
      email_address TYPE string,
    END OF ts_account_id,

    tt_components TYPE STANDARD TABLE OF ts_id WITH EMPTY KEY,

    BEGIN OF ts_parent_issue,
      key TYPE string,
    END OF ts_parent_issue,

    tt_labels TYPE STANDARD TABLE OF string WITH EMPTY KEY,

    BEGIN OF ts_status,
      id   TYPE string,
      name TYPE string,
    END OF ts_status,

    BEGIN OF ts_progress,
      progress TYPE i,
      total    TYPE i,
    END OF ts_progress,

    BEGIN OF ts_fields,
      priority          TYPE ts_id,
      assignee          TYPE ts_account_id,
      parent            TYPE ts_parent_issue,
      status            TYPE ts_status,
      creator           TYPE ts_account_id,
      components        TYPE tt_components,
      created           TYPE timestamp,
      summary           TYPE string,
      customfield_10043 TYPE ts_id,                             "Severity
      customfield_10035 TYPE ts_id,                             "Scrum Team
      reporter          TYPE ts_account_id,
      issuetype         TYPE ts_id,
      project           TYPE ts_id,
      progress          TYPE ts_progress,
      customfield_10040 TYPE zif_jira_adf_types=>ts_document,   "Notes ADF Format
      customfield_10038 TYPE zif_jira_adf_types=>ts_document,   "Acceptance Criteria ADF Format
      environment       TYPE zif_jira_adf_types=>ts_document,   "Environment ADF Format
      customfield_10026 TYPE p LENGTH 3 DECIMALS 1,             "Story Points e.g. 5.0
      description       TYPE zif_jira_adf_types=>ts_document,   "Description Body
      labels            TYPE tt_labels,                         "Unit_Testing"
    END OF ts_fields,

    BEGIN OF ts_create_request,
      fields TYPE ts_fields,
    END OF ts_create_request.

  TYPES:
    BEGIN OF ts_query_params,
      jql         TYPE string,
      fields      TYPE string,
      properties  TYPE string,
      start_at    TYPE i,
      max_results TYPE i,
    END OF ts_query_params.

  TYPES:
    BEGIN OF ts_create_response,
      id   TYPE string,
      key  TYPE string,
      self TYPE string,
    END OF ts_create_response,

    BEGIN OF ts_issues,
      key    TYPE string,
      fields TYPE ts_fields,
    END OF ts_issues,

    BEGIN OF ts_search_response,
      start_at         TYPE i,
      max_results      TYPE i,
      total            TYPE i,
      issues           TYPE TABLE OF ts_issues WITH EMPTY KEY,
      warning_messages TYPE TABLE OF string WITH EMPTY KEY,
    END OF ts_search_response.

  CONSTANTS:
    BEGIN OF gc_issue_types,
      task     TYPE char5 VALUE '10003',
      sub_task TYPE char5 VALUE '10004',
      story    TYPE char5 VALUE '10001',
      bug      TYPE char5 VALUE '10005',
      epic     TYPE char5 VALUE '10000',
    END OF gc_issue_types,

    BEGIN OF gc_priorities,
      highest TYPE char1 VALUE '1',
      high    TYPE char1 VALUE '2',
      medium  TYPE char1 VALUE '3',
      low     TYPE char1 VALUE '4',
      lowest  TYPE char1 VALUE '5',
    END OF gc_priorities,

    BEGIN OF gc_severities,
      s1_critical_no_wrknd       TYPE char5 VALUE '10103',
      s2_critical_with_wrknd     TYPE char5 VALUE '10104',
      s3_non_critical_no_wrknd   TYPE char5 VALUE '10105',
      s4_non_critical_with_wrknd TYPE char5 VALUE '10106',
      s5_minor_defect            TYPE char5 VALUE '10107',
      s6_enhancement             TYPE char5 VALUE '10108',
    END OF gc_severities,

    BEGIN OF gc_user_story_status,
      pending_approval TYPE string VALUE '10008',
      ready_to_start   TYPE string VALUE '10009',
      accepted         TYPE string VALUE '10005',
      dev_complete     TYPE string VALUE '10015',
      in_progress      TYPE string VALUE '3',
      test_complete    TYPE string VALUE '10016',
      done             TYPE string VALUE '10001',
      cancelled        TYPE string VALUE '10045',
      error            TYPE string VALUE '99999',
    END OF gc_user_story_status,

    BEGIN OF gc_bug_status,
      to_do       TYPE string VALUE '10000',
      in_progress TYPE string VALUE '3',
      in_review   TYPE string VALUE '10003',
      done        TYPE string VALUE '10001',
      deferred    TYPE string VALUE '10042',
      cancelled   TYPE string VALUE '10045',
      closed      TYPE string VALUE '6',
      error       TYPE string VALUE '99999',
    END OF gc_bug_status.

*- Operations
  CONSTANTS:
    gc_post_issue    TYPE string VALUE '/rest/api/3/issue',
    gc_get_issue     TYPE string VALUE '/rest/api/3/issue/{issueIdOrKey}',
    gc_search_by_sql TYPE string VALUE '/rest/api/3/search'.

  METHODS:
    create_issue
      IMPORTING
        VALUE(is_request) TYPE ts_create_request
      EXPORTING
        es_response       TYPE ts_create_response
      RAISING
        zcx_jira_exceptions,

    search_issue_by_jql
      IMPORTING
        VALUE(is_query_params) TYPE ts_query_params
      EXPORTING
        es_response            TYPE ts_search_response
      RAISING
        zcx_jira_exceptions.

ENDINTERFACE.
