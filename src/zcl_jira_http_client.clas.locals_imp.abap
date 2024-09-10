
CLASS lcl_mock_http_client IMPLEMENTATION.

  METHOD if_http_client~send.
    IF gv_fail_send EQ abap_true.
      RAISE http_communication_failure.
    ENDIF.
  ENDMETHOD.


  METHOD if_http_client~receive.
    IF gv_fail_receive EQ abap_true.
      RAISE http_communication_failure.
    ENDIF.
  ENDMETHOD.

  METHOD if_http_client~get_last_error.
    code = 501.
  ENDMETHOD.

  METHOD if_http_client~close.
     "Fake Implementation
  ENDMETHOD.

ENDCLASS.
