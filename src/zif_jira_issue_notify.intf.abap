INTERFACE zif_jira_issue_notify
  PUBLIC .

  METHODS:
    create_jira_issue
      IMPORTING
        VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data
      EXPORTING
        es_response          TYPE zif_jira_issues=>ts_create_response
      RAISING
        zcx_jira_exceptions,

    update_error_logs
      IMPORTING
        VALUE(iv_error_msg)  TYPE bapi_msg
        VALUE(is_notif_data) TYPE zcl_global_utilities=>gs_alert_notif_data.

ENDINTERFACE.
