INTERFACE zif_jira_utils
  PUBLIC .

  METHODS:
    convert_to_jira_date_format
      IMPORTING
        VALUE(iv_date)      TYPE sy-datum
      RETURNING
        VALUE(rv_jira_date) TYPE char15,

      construct_query_string
        IMPORTING
          VALUE(is_query_params) TYPE any
        RETURNING
          VALUE(rv_query_string) TYPE string,

      to_camel_case
        IMPORTING
          VALUE(iv_string) TYPE string
        RETURNING
          VALUE(rv_string) TYPE string.

ENDINTERFACE.
