INTERFACE zif_jira_adf_types
  PUBLIC .

  TYPES:
    BEGIN OF ts_attrs,
      title                    TYPE string,
      color                    TYPE string,
      panel_type               TYPE string,
      is_number_column_enabled TYPE abap_bool,
      layout                   TYPE string,
      href                     TYPE string,
      order                    TYPE i,
      colwidth                 TYPE TABLE OF int4 WITH EMPTY KEY,
    END OF ts_attrs .

  TYPES:
    BEGIN OF ts_marks,
      type  TYPE string,
      attrs TYPE ts_attrs,
    END OF ts_marks .

  TYPES:
    tt_marks TYPE STANDARD TABLE OF ts_marks WITH EMPTY KEY .

  TYPES:
    BEGIN OF ts_content,
      type  TYPE string,
      text  TYPE string,
      attrs TYPE ts_attrs,
      marks TYPE tt_marks,
    END OF ts_content .

  TYPES:
    tt_content TYPE STANDARD TABLE OF ts_content WITH EMPTY KEY .

  TYPES:
    BEGIN OF ts_node,
      type    TYPE string,
      text    TYPE string,
      attrs   TYPE ts_attrs,
      marks   TYPE tt_marks,
      content TYPE REF TO data,  "Child Nodes
    END OF ts_node .

  TYPES:
    tt_nodes TYPE STANDARD TABLE OF ts_node WITH EMPTY KEY.

  TYPES:
    BEGIN OF ts_document,
      version TYPE i,
      type    TYPE char10,
      content TYPE tt_nodes,
    END OF ts_document.

  CONSTANTS:
    BEGIN OF gc_nodes,
      blockquote   TYPE string VALUE 'blockquote',
      bulletlist   TYPE string VALUE 'bulletList',
      codeblock    TYPE string VALUE 'codeblock',
      heading      TYPE string VALUE 'heading',
      panel        TYPE string VALUE 'panel',
      paragraph    TYPE string VALUE 'paragraph',
      table        TYPE string VALUE 'table',
      list_item    TYPE string VALUE 'listItem',
      media        TYPE string VALUE 'media',
      table_cell   TYPE string VALUE 'tableCell',
      table_header TYPE string VALUE 'tableHeader',
      table_row    TYPE string VALUE 'tableRow',
      text         TYPE string VALUE 'text',
      ordered_list TYPE string VALUE 'orderedList',
    END OF gc_nodes .

  CONSTANTS:
    BEGIN OF gc_mark_types,
      border     TYPE string VALUE 'border',
      code       TYPE string VALUE 'code',
      em         TYPE string VALUE 'em',
      link       TYPE string VALUE 'link',
      strike     TYPE string VALUE 'strike',
      strong     TYPE string VALUE 'strong',
      subsup     TYPE string VALUE 'subsup',
      text_color TYPE string VALUE 'textColor',
      underline  TYPE string VALUE 'underline',
    END OF gc_mark_types .

  CONSTANTS:
    BEGIN OF gc_panel_types,
      info    TYPE string VALUE 'info',
      note    TYPE string VALUE 'note',
      warning TYPE string VALUE 'warning',
      success TYPE string VALUE 'success',
      error   TYPE string VALUE 'error',
    END OF gc_panel_types .

  CONSTANTS:
    BEGIN OF gc_table_layouts,
      wide        TYPE string VALUE 'wide',
      full_width  TYPE string VALUE 'full-width',
      center      TYPE string VALUE 'center',
      align_end   TYPE string VALUE 'align-end',
      align_start TYPE string VALUE 'align-start',
      default     TYPE string VALUE 'default',
    END OF gc_table_layouts .

  CONSTANTS:
    BEGIN OF gc_colors,
      light_gray   TYPE string VALUE '#97a0af',
      white        TYPE string VALUE '#ffffff',
      dark_blue    TYPE string VALUE '#0747a6',
      blue         TYPE string VALUE '#4c9aff',
      light_blue   TYPE string VALUE '#b3d4ff',
      dark_green   TYPE string VALUE '#006644',
      green        TYPE string VALUE '#36b37e',
      light_green  TYPE string VALUE '#abf5d1',
      orange       TYPE string VALUE '#ff991f',
      yellow       TYPE string VALUE '#ffc400',
      light_yellow TYPE string VALUE '#fff0b3',
      dark_red     TYPE string VALUE '#bf2600',
      red          TYPE string VALUE '#ff5630',
      light_red    TYPE string VALUE '#ffbdad',
      dark_purple  TYPE string VALUE '#403294',
      purple       TYPE string VALUE '#6554c0',
      light_purple TYPE string VALUE '#eae6ff',
    END OF gc_colors .

  CONSTANTS:
    gc_doc TYPE char10 VALUE 'doc'.

ENDINTERFACE.
