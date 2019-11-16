"! <p class="shorttext synchronized" lang="en"></p>
"!
"! <p>The central point is ALV_TABLE_CREATE - It calls FB_TABLE_CREATE_STRING</p>
"!
"! <p>The other methods call ALV_TABLE_CREATE</p>
"! <ul>
"! <li>create_dynamic_table -&gt; LVC_TABLE_CREATE -&gt; ALV_TABLE_CREATE</li>
"! <li>LVC_TABLE_CREATE -&gt; ALV_TABLE_CREATE</li>
"! <li>reuse_alv_table_create -&gt; ALV_TABLE_CREATE</li>
"! </ul>
"!
CLASS zcl_alv_table_create_rttc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

  "! Wrapper of LVC_TABLE_CREATE (called with parameters 'ZCL_ALV_TABLE_CREATE_RTTI' and 'INTERNAL_RECEIVER')
    CLASS-METHODS create_dynamic_table
      IMPORTING
        VALUE(i_style_table) TYPE char01 OPTIONAL
        !it_fieldcatalog     TYPE lvc_t_fcat
        !i_length_in_byte    TYPE boolean OPTIONAL
      EXPORTING
        !ep_table            TYPE REF TO data
        !e_style_fname       TYPE lvc_fname
      EXCEPTIONS
        generate_subpool_dir_full .
    "! Wrapper of ALV_TABLE_CREATE (note: before calling it, it first calls REUSE_ALV_TRANSFER_DATA)
    CLASS-METHODS reuse_alv_table_create
      IMPORTING
        VALUE(it_fieldcat)        TYPE slis_t_fieldcat_alv
        VALUE(i_callback_program) TYPE sy-repid
        VALUE(i_formname)         TYPE char30
      EXCEPTIONS
        generate_subpool_dir_full .
    "! Wrapper of FB_TABLE_CREATE_STRING
    CLASS-METHODS alv_table_create
      IMPORTING
        VALUE(it_fieldcat)        TYPE kkblo_t_fieldcat
        VALUE(i_callback_program) TYPE sy-repid
        VALUE(i_formname)         TYPE char30
        VALUE(i_tabname)          TYPE kkblo_tabname DEFAULT '1'
        !i_style_table            TYPE c OPTIONAL
        !i_oo_class_reference     TYPE REF TO object OPTIONAL
        !i_oo_class_name          TYPE c OPTIONAL
        !i_oo_method              TYPE c OPTIONAL
        !i_length_in_byte         TYPE boolean OPTIONAL
      EXCEPTIONS
        generate_subpool_dir_full .
    "! Wrapper of ALV_TABLE_CREATE
    CLASS-METHODS lvc_table_create
      IMPORTING
        VALUE(it_fieldcat)        TYPE lvc_t_fcat
        VALUE(i_callback_program) TYPE sy-repid OPTIONAL
        VALUE(i_formname)         TYPE char30 OPTIONAL
        !i_style_table            TYPE c OPTIONAL
        !i_oo_class_reference     TYPE REF TO object OPTIONAL
        !i_oo_class_name          TYPE c OPTIONAL
        !i_oo_method              TYPE c OPTIONAL
        !i_length_in_byte         TYPE boolean OPTIONAL
      EXCEPTIONS
        generate_subpool_dir_full .
    "! creates the table via RTTI (note: the ALV catalog is also completed if needed)
    CLASS-METHODS fb_table_create_string
      IMPORTING
        !r_form           TYPE c
        !r_program        TYPE syrepid
        !r_oo_class       TYPE REF TO object
        !r_oo_class_name  TYPE c
        !r_oo_method      TYPE c
        !r_style_table    TYPE c
        !r_tabname        TYPE kkblo_tabname
        !r_length_in_byte TYPE boolean
      CHANGING
        !rt_fieldcat      TYPE kkblo_t_fieldcat
      EXCEPTIONS
        generate_subpool_dir_full .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mp_table TYPE REF TO data .

    CLASS-METHODS internal_receiver
      IMPORTING
        !ip_table TYPE REF TO data .
ENDCLASS.



CLASS zcl_alv_table_create_rttc IMPLEMENTATION.


  METHOD alv_table_create.

    fb_table_create_string(
      EXPORTING
        r_form           = i_formname
        r_program        = i_callback_program
        r_oo_class       = i_oo_class_reference
        r_oo_class_name  = i_oo_class_name
        r_oo_method      = i_oo_method
        r_style_table    = i_style_table
        r_tabname        = i_tabname
        r_length_in_byte = i_length_in_byte
      CHANGING
        rt_fieldcat      = it_fieldcat[]
      EXCEPTIONS
        generate_subpool_dir_full = 1
    ).
    CASE sy-subrc.
      WHEN 1. RAISE generate_subpool_dir_full.
    ENDCASE.

  ENDMETHOD.


  METHOD create_dynamic_table.

    DATA: l_oo_method(80) TYPE c.
    l_oo_method = 'INTERNAL_RECEIVER'.
*<<ML14.11

    lvc_table_create(
          EXPORTING
            it_fieldcat               = it_fieldcatalog
            i_style_table             = i_style_table
            i_length_in_byte          = i_length_in_byte
            i_oo_class_name           = 'ZCL_ALV_TABLE_CREATE_RTTI'
            i_oo_method               = l_oo_method
          EXCEPTIONS
            generate_subpool_dir_full = 1
          ).
    CASE sy-subrc.
      WHEN 1. RAISE generate_subpool_dir_full.
    ENDCASE.

    ep_table = mp_table.

    IF NOT i_style_table IS INITIAL.
      e_style_fname = 'XYZSTYLEZYX'.
    ENDIF.


  ENDMETHOD.


  METHOD fb_table_create_string.


    DATA: ls_fieldcat TYPE kkblo_fieldcat.
    DATA: l_string(60) TYPE c.
    DATA: l_name LIKE sy-repid.
    DATA: l_message(240) TYPE c,
          l_line         TYPE i,
          l_word(72)     TYPE c.

    DATA: l_form(30) TYPE c VALUE 'TABLE_CREATE'.
    DATA: l_length TYPE lvc_outlen.                         "Y9CK020977

    DATA:
      lt_comp              TYPE cl_abap_structdescr=>component_table,
      ls_comp              LIKE LINE OF lt_comp,
      lp_struc_descr       TYPE REF TO cl_abap_structdescr,
      lp_tab_descr         TYPE REF TO cl_abap_tabledescr,
      lt_fieldcatalog      TYPE lvc_t_fcat,
      lv_inttype           TYPE inttype,
      lv_length            TYPE i,
      lv_byte_length       TYPE i,
      lv_decimals          TYPE i,
      lv_fieldcat_complete TYPE flag.

    FIELD-SYMBOLS:
      <ls_fieldcat>       TYPE kkblo_fieldcat.

    CALL FUNCTION 'K_KKB_FIELDCAT_COMPLETE'
      EXPORTING
        i_tabname   = r_tabname
      CHANGING
        ct_fieldcat = rt_fieldcat[]
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*<<< YI3K081678 get invalid signs to check fieldnames
    DATA: l_space      TYPE string,
          l_fieldname  TYPE string,
          l_field_mess TYPE string,
          l_subrc      TYPE sy-subrc.

    l_space = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).
    CONCATENATE l_space ':' INTO l_space.
    CONCATENATE l_space ';' INTO l_space.
    CONCATENATE l_space '.' INTO l_space.
*>>> YI3K081678

    SORT rt_fieldcat BY col_pos ASCENDING.

    " Check if field catalog must be completed
    LOOP AT rt_fieldcat TRANSPORTING NO FIELDS
         WHERE inttype IS INITIAL.
      lv_fieldcat_complete =  abap_true.
      EXIT.
    ENDLOOP.

    LOOP AT rt_fieldcat INTO ls_fieldcat.

      l_fieldname = ls_fieldcat-fieldname.
      IF l_fieldname CA l_space.
        l_field_mess = l_fieldname.
        l_subrc = '1'.

      ELSE.

        ls_comp-name = ls_fieldcat-fieldname.

        IF NOT ls_fieldcat-ref_fieldname IS INITIAL.
          ls_comp-type ?= cl_abap_typedescr=>describe_by_name( ls_fieldcat-ref_tabname && '-' && ls_fieldcat-ref_fieldname ).
        ELSE.
          IF ls_fieldcat-datatype = 'CHAR'.                 "Y6AK044662

*  >>Y6AK037383
            IF r_length_in_byte EQ abap_true.
*          class cl_abap_char_utilities definition load.
              ls_fieldcat-intlen = ls_fieldcat-intlen / cl_abap_char_utilities=>charsize.
            ENDIF.
*  <<Y6AK037383

            IF ls_fieldcat-ddic_outputlen > 0.  ">>B20K8A0Q9G
              l_length = ls_fieldcat-ddic_outputlen.        "Y6AK044662
            ENDIF.

            IF ls_fieldcat-intlen > 0.
              IF ls_fieldcat-ddic_outputlen > ls_fieldcat-intlen.
                l_length = ls_fieldcat-ddic_outputlen.
              ELSE.
                l_length = ls_fieldcat-intlen.
              ENDIF.
            ENDIF.

            IF l_length = 0.
              l_length = ls_fieldcat-outputlen.
            ENDIF.  "<<B20K8A0Q9G

            ls_comp-type = cl_abap_elemdescr=>get_c( l_length + 0 ).

          ELSEIF ls_fieldcat-datatype = 'NUMC'.             "Y6AK044662

            IF r_length_in_byte EQ abap_true.
              CLASS cl_abap_char_utilities DEFINITION LOAD.
              ls_fieldcat-intlen = ls_fieldcat-intlen / cl_abap_char_utilities=>charsize.
            ENDIF.

            IF ls_fieldcat-ddic_outputlen > 0.
              l_length = ls_fieldcat-ddic_outputlen.
            ENDIF.

            IF ls_fieldcat-intlen > 0.
              IF ls_fieldcat-ddic_outputlen > ls_fieldcat-intlen.
                l_length = ls_fieldcat-ddic_outputlen.
              ELSE.
                l_length = ls_fieldcat-intlen.
              ENDIF.
            ENDIF.

            IF l_length = 0.
              l_length = ls_fieldcat-outputlen.
            ENDIF.  "<<B20K8A0Q9G

            ls_comp-type = cl_abap_elemdescr=>get_n( l_length + 0 ).

          ELSEIF ls_fieldcat-datatype = 'CURR'.

            ls_comp-type = cl_abap_elemdescr=>get_p(
                           p_length   = 8
                           p_decimals = 2 ).

          ELSEIF ls_fieldcat-datatype EQ 'INT1'
            OR ls_fieldcat-inttype EQ 'B'
            OR ls_fieldcat-inttype EQ 'b'.
            ls_fieldcat-inttype = 'I'.

            ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'INT1' ).

          ELSEIF ls_fieldcat-datatype EQ 'INT2'
            OR ls_fieldcat-inttype EQ 's'.
            ls_fieldcat-inttype = 'I'.

            IF NOT ls_fieldcat-intlen IS INITIAL AND
                   ls_fieldcat-inttype NE 'g'.              "B20K8A0MOD

              IF ls_fieldcat-inttype = 'F' AND
                 ls_fieldcat-intlen NE 8.
                ls_fieldcat-intlen = 8.
              ENDIF.

              IF ls_fieldcat-inttype = 'D' AND
                ls_fieldcat-intlen NE 8.
                ls_fieldcat-intlen = 8.
              ENDIF.
              IF ls_fieldcat-inttype = 'T' AND              "Y7AK018607
                ls_fieldcat-intlen NE 6.
                ls_fieldcat-intlen = 6.
              ENDIF.
              IF ls_fieldcat-inttype EQ 'P'.
                DATA: l_leng TYPE lvc_outlen.
                IF r_length_in_byte EQ abap_true.
                  l_leng = ls_fieldcat-intlen.
                ELSE.
*                if ls_fieldcat-no_sign is initial.
*                  l_leng =  ( ls_fieldcat-intlen / 2 ) + 1 .
*                else.
                  l_leng =  ( ls_fieldcat-intlen + 1 ) / 2.
*                endif.
                ENDIF.
                CONCATENATE ls_fieldcat-fieldname '(' l_leng ')'
                              INTO l_string.
              ELSE.
                CONCATENATE ls_fieldcat-fieldname '(' ls_fieldcat-intlen ')'
                              INTO l_string.
              ENDIF.
            ELSE.
              l_string = ls_fieldcat-fieldname.
            ENDIF.

            IF ls_fieldcat-inttype EQ 'g'.                  "B20K8A0MOD

              ls_comp-type = cl_abap_elemdescr=>get_string( ).
            ELSE.
              CASE ls_fieldcat-inttype.
                WHEN 'D'.
                  ls_comp-type = cl_abap_elemdescr=>get_d( ).
                WHEN 'F'.
                  ls_comp-type = cl_abap_elemdescr=>get_f( ).
                WHEN 'I'.
                  ls_comp-type = cl_abap_elemdescr=>get_i( ).
                WHEN 'P'.
                  ls_comp-type = cl_abap_elemdescr=>get_p(
                                 p_length   = l_leng + 0
                                 p_decimals = 2 ).
                WHEN 'T'.
                  ls_comp-type = cl_abap_elemdescr=>get_t( ).
                WHEN 'X'.
                  ls_comp-type = cl_abap_elemdescr=>get_x( ls_fieldcat-intlen + 0 ).
              ENDCASE.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_comp TO lt_comp.

    ENDLOOP.

*<<< YI3K081678 single (!) message incorrect fieldname
    IF l_subrc EQ '1'.
      MESSAGE i538(0k) WITH l_field_mess 'FIELDNAME'.
      CLEAR l_subrc.
    ENDIF.
*>>> YI3K081678

    IF r_style_table = 'X'.
      CLEAR ls_comp.
      ls_comp-name  = 'XYZSTYLEZYX'.
      ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'LVC_T_STYL' ).
      APPEND ls_comp TO lt_comp.
    ENDIF.

    TRY.
        lp_struc_descr = cl_abap_structdescr=>create(
                          p_components = lt_comp
                          p_strict     = ' ' ).

        lp_tab_descr = cl_abap_tabledescr=>create(
                          p_line_type  = lp_struc_descr
                          p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        DATA ep_table TYPE REF TO data.

        CREATE DATA ep_table TYPE HANDLE lp_tab_descr.

        DATA lx_root TYPE REF TO cx_root.
      CATCH cx_root INTO lx_root.
        l_message = lx_root->get_text( ).
        MESSAGE x000(0k) WITH l_message space space space.
    ENDTRY.

    IF r_oo_class_name IS INITIAL.
      FIELD-SYMBOLS <lt_gentab> TYPE STANDARD TABLE.
      ASSIGN ep_table->* TO <lt_gentab>.
      PERFORM (r_form) IN PROGRAM (r_program) TABLES <lt_gentab>.
    ELSEIF r_oo_class IS BOUND.
      CALL METHOD r_oo_class->(r_oo_method) EXPORTING ip_table = ep_table.
    ELSE.
      CALL METHOD zcl_alv_table_create_rttc=>(r_oo_method) EXPORTING ip_table = ep_table.
    ENDIF.


  ENDMETHOD.


  METHOD internal_receiver.

    mp_table = ip_table.

  ENDMETHOD.


  METHOD lvc_table_create.

    DATA: lt_fieldcat_lvc TYPE kkblo_t_fieldcat.

    CALL FUNCTION 'LVC_TRANSFER_TO_KKBLO'
      EXPORTING
        it_fieldcat_lvc   = it_fieldcat
      IMPORTING
        et_fieldcat_kkblo = lt_fieldcat_lvc.

    alv_table_create(
      EXPORTING
        it_fieldcat               = lt_fieldcat_lvc
        i_callback_program        = i_callback_program
        i_formname                = i_formname
        i_style_table             = i_style_table
        i_length_in_byte          = i_length_in_byte
        i_oo_class_reference      = i_oo_class_reference
        i_oo_class_name           = i_oo_class_name
        i_oo_method               = i_oo_method
        i_tabname                 = '1'
      EXCEPTIONS
        generate_subpool_dir_full = 1
      ).
    CASE sy-subrc.
      WHEN 1. RAISE generate_subpool_dir_full.
    ENDCASE.

  ENDMETHOD.


  METHOD reuse_alv_table_create.

    DATA: lt_fieldcat_lvc TYPE kkblo_t_fieldcat.

    CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
      EXPORTING
        it_fieldcat = it_fieldcat
      IMPORTING
        et_fieldcat = lt_fieldcat_lvc
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    alv_table_create(
      EXPORTING
        it_fieldcat               = lt_fieldcat_lvc
        i_callback_program        = i_callback_program
        i_formname                = i_formname
        i_tabname                 = '1'
      EXCEPTIONS
        generate_subpool_dir_full = 1
    ).
    CASE sy-subrc.
      WHEN 1. RAISE generate_subpool_dir_full.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
