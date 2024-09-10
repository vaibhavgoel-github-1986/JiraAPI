class ZCX_JIRA_EXCEPTIONS definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_JIRA_EXCEPTIONS,
      msgid type symsgid value 'ZMSG_JIRA',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'CODE',
      attr2 type scx_attrname value 'REASON',
      attr3 type scx_attrname value 'MSGV1',
      attr4 type scx_attrname value 'MSGV2',
    end of ZCX_JIRA_EXCEPTIONS .
  data CODE type I .
  data REASON type STRING .
  data MSGV1 type MSGV1 .
  data MSGV2 type MSGV2 .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !CODE type I optional
      !REASON type STRING optional
      !MSGV1 type MSGV1 optional
      !MSGV2 type MSGV2 optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_JIRA_EXCEPTIONS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->CODE = CODE .
me->REASON = REASON .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_JIRA_EXCEPTIONS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
