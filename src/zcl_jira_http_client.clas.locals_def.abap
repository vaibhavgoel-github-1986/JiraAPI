CLASS lcl_mock_http_client DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      if_http_client.

    DATA:
      gv_fail_send,
      gv_fail_receive.

ENDCLASS.
