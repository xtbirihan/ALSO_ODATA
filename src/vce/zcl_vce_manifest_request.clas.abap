CLASS zcl_vce_manifest_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_vce_api_client .
    INTERFACES zif_vce_manifest_model .

    ALIASES build_request_data
      FOR zif_vce_api_client~build_request_data .
    ALIASES get_response
      FOR zif_vce_api_client~get_response .
    ALIASES process_response
      FOR zif_vce_api_client~process_response .
    ALIASES send_get_request
      FOR zif_vce_api_client~send_get_request .
    ALIASES send_post_request
      FOR zif_vce_api_client~send_post_request .
    ALIASES ty_request
      FOR zif_vce_manifest_model~ty_request .

    TYPES:
      BEGIN OF ty_parameter,
        key   TYPE char255,
        value TYPE string, "fpcontent,
      END OF ty_parameter .
    TYPES:
      tt_parameter TYPE STANDARD TABLE OF ty_parameter WITH EMPTY KEY .
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure response data</p>
      BEGIN OF ty_response,
        error      TYPE char10,
        error_desc TYPE string,
        parameters TYPE tt_parameter,
        details    TYPE zif_vce_manifest_model=>tt_details_response,
      END OF ty_response .
    TYPES:
      BEGIN OF ty_build_parameters,
        tu_num        TYPE  /scwm/de_tu_num,
        tu_sr_act_num TYPE  /scwm/de_sr_act_num,
      END OF ty_build_parameters .

    DATA mo_log TYPE REF TO zcl_vce_log .

    METHODS constructor
      IMPORTING
        !iv_lgnum           TYPE /scwm/lgnum
        !io_log             TYPE REF TO zcl_vce_log OPTIONAL
        !iv_application_log TYPE balloghndl OPTIONAL
      RAISING
        zcx_core_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS mc_url TYPE string VALUE 'https://api.also.uat.vce.de/api/v1/ship' ##NO_TEXT.
    DATA mv_lgnum TYPE /scwm/lgnum .
    DATA mo_rest_client TYPE REF TO zcl_vce_rest_client .
    DATA ms_build_parameters TYPE ty_build_parameters .
    DATA ms_terminal TYPE zout_term_def .
    DATA mv_appl_log_handle TYPE balloghndl .

    METHODS convert_base_64_to_raw
      IMPORTING
        !iv_content        TYPE string
      RETURNING
        VALUE(rv_xcontent) TYPE xstring .
    METHODS handle_pdf
      IMPORTING
        !iv_key      TYPE string
        !iv_pdf_data TYPE fpcontent
      RAISING
        zcx_core_exception .
    METHODS add_message
      IMPORTING
        !iv_problemclass TYPE bal_s_msg-probclass DEFAULT '1' .
ENDCLASS.



CLASS ZCL_VCE_MANIFEST_REQUEST IMPLEMENTATION.


  METHOD add_message.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Add message/display message
**********************************************************************
    IF mo_log IS BOUND.
      mo_log->add_message( ).
    ELSEIF mv_appl_log_handle IS NOT INITIAL.
      cl_log_ppf=>add_message(
        EXPORTING
          ip_problemclass = iv_problemclass
          ip_handle       = mv_appl_log_handle ).
    ELSE.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE sy-msgty.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Create instance
**********************************************************************
    mv_lgnum = iv_lgnum.

    mo_rest_client = NEW zcl_vce_rest_client( iv_url = mc_url ).
    mo_log = io_log.
    mv_appl_log_handle = iv_application_log.
  ENDMETHOD.


  METHOD convert_base_64_to_raw.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& convert Base 64 to binary content
**********************************************************************
    xco_binary=>text_encoding->base64->if_xco_string_xstring_cnvrsn~apply(
      EXPORTING
        iv_string  = iv_content
      RECEIVING
        rv_xstring = rv_xcontent
    ).
  ENDMETHOD.


  METHOD handle_pdf.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Print PDF file and attach to the TU
**********************************************************************
    DATA:
          lv_terminal       TYPE /scwm/de_wc_terminal.
    IF ms_terminal-printer IS INITIAL.
      CALL FUNCTION 'TH_USER_INFO'
        EXPORTING
          client   = sy-mandt
          user     = sy-uname
        IMPORTING
          terminal = lv_terminal.

      IF lv_terminal IS NOT INITIAL.
        ms_terminal = zcl_crud_zout_term_def=>select_single_by_key( iv_terminal = lv_terminal ).
      ENDIF.
    ENDIF.

    DATA(ls_output_param) = VALUE sfpoutputparams( dest = ms_terminal-printer
      ).
    zcl_vce_request_utils=>print_pdf_file( iv_pdf_data     = iv_pdf_data
                                           is_outputparams = ls_output_param ).

    zcl_vce_request_utils=>attach_label_to_doc(
      iv_file_name = CONV #( |{ /scwm/if_api_attachments=>gc_obj_type_tua }{ ms_build_parameters-tu_sr_act_num }{ iv_key }.pdf| ) ##no_text
      iv_objkey    = |{ ms_build_parameters-tu_sr_act_num  ALPHA = OUT }|
      iv_objtyp    = /scwm/if_api_attachments=>gc_obj_type_tua
      iv_pdf_data  = iv_pdf_data
      iv_commit    = abap_true ).
  ENDMETHOD.


  METHOD zif_vce_api_client~build_request_data.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Read manifest data
**********************************************************************
    DATA:
      ls_request_data   TYPE ty_request,
      ls_inb_parameters TYPE ty_build_parameters,
      lv_terminal       TYPE /scwm/de_wc_terminal,
      lv_msg            TYPE string ##needed,
      lt_huident        TYPE  /scwm/tt_ident_int.

    CONSTANTS:
      c_true  TYPE string VALUE 'TRUE' ##needed,
      c_false TYPE string VALUE 'FALSE'.

    CLEAR es_data.

    MOVE-CORRESPONDING iv_input TO ls_inb_parameters.
    /scwm/cl_tm=>set_lgnum( mv_lgnum ).

    ms_build_parameters = ls_inb_parameters.
    TRY.
        DATA(lo_tu) = /scwm/cl_sr_bom=>get_instance( )->get_bo_tu_by_key( VALUE /scwm/s_tu_sr_act_num( tu_num = ls_inb_parameters-tu_num tu_sr_act_num = ls_inb_parameters-tu_sr_act_num ) ).
        IF lo_tu IS NOT BOUND.
          RETURN.
        ENDIF.

        lo_tu->get_data(
          EXPORTING
            iv_add_info = abap_true
          IMPORTING
            es_tuhdr    = DATA(ls_tu_data)                 " Transporteinheit
            et_tu_dlv   = DATA(lt_tudlv_hu)                 " Lieferungen und HU´s der Transporteinheiten
            et_seal     = DATA(lt_seal)                  " Tabelle von Siegeln einer Transporteinheit
            et_ident    = DATA(lt_tu_ident)
        ).


        CALL FUNCTION 'TH_USER_INFO'
          EXPORTING
            client   = sy-mandt
            user     = sy-uname
          IMPORTING
            terminal = lv_terminal.

        IF lv_terminal IS NOT INITIAL.
          DATA(ms_terminal) = zcl_crud_zout_term_def=>select_single_by_key( iv_terminal = lv_terminal ).
        ELSE.
          MESSAGE e037(zmc_out) INTO lv_msg WITH sy-uname.
          add_message( ).
          RETURN.
        ENDIF.

        IF ms_terminal-printer IS INITIAL.
          MESSAGE e038(zmc_out) INTO lv_msg WITH ms_terminal-terminal.
          add_message(  ).
          RETURN.
        ENDIF.

        "Carrier
        DATA(lt_map_vce_carr) = zcl_crud_ztout_map_carr=>select_multi_by_bupartner( iv_lgnum = mv_lgnum iv_partner = ls_tu_data-tsp ).
        IF lt_map_vce_carr IS NOT INITIAL.
          DATA(lv_vce_carrier) = lt_map_vce_carr[ 1 ]-carr.
        ENDIF.
        IF lt_seal IS NOT INITIAL.
          DATA(lv_seal) = lt_seal[ 1 ]-seal_num.
        ENDIF.


        DATA(lv_cmr_req) = xsdbool( ls_tu_data-zz_cmr IS NOT INITIAL AND ls_tu_data-zz_no_cmr_print EQ abap_false ).
        DATA(ls_manifest) = VALUE zif_vce_manifest_model=>manif_request(
                                   user = sy-uname
                                   parameters = VALUE #(
                                      ( key = zif_vce_manifest_model=>c_param_key-carrier value = lv_vce_carrier )
                                      ( key = zif_vce_manifest_model=>c_param_key-loadnumber value = ls_tu_data-tu_num )
                                      ( key = zif_vce_manifest_model=>c_param_key-cmr
                                        value = COND #( WHEN lv_cmr_req EQ abap_true  THEN 'TRUE' ELSE 'FALSE'   )
                                      )
                                   )
                           ).
        IF lv_cmr_req EQ abap_true.
          DATA(ls_security_shipment) = VALUE #( lt_tu_ident[ idart = zif_wme_c=>gs_idart-s ] OPTIONAL ).
          DATA(ls_free_text_bl13) = VALUE #( lt_tu_ident[ idart = zif_wme_c=>gs_idart-t ] OPTIONAL ).
          APPEND LINES OF VALUE zif_vce_api_model=>tt_parameter(
                                      ( key = zif_vce_manifest_model=>c_param_key-cmr_5_attached_documents
                                        value = |Frachtbrief{ ls_tu_data-tu_num  }|
                                      )
                                      ( key = zif_vce_manifest_model=>c_param_key-cmr_13_sender_instructions
                                        value =  |{ lv_seal }{ COND string( WHEN lv_seal IS NOT INITIAL THEN | | ) }{ ls_free_text_bl13-ident }| &
                                                 |{ COND string( WHEN ls_free_text_bl13-ident IS NOT INITIAL THEN | | ) }{ ls_security_shipment-ident } |
                                      )
            ) TO ls_manifest-parameters  ##no_text.
        ENDIF.
        "Select HU
        CALL FUNCTION '/SCWM/HU_SELECT_GEN'
          EXPORTING
            iv_lgnum   = mv_lgnum                 " Lagernummer/Lagerkomplex
            it_guid_hu = VALUE /scwm/tt_guid_hu( FOR tudlv IN lt_tudlv_hu WHERE ( top_hu IS NOT INITIAL )
                                      ( guid_hu = tudlv-top_hu ) )                " Tabelle mit HU-Guids
          IMPORTING
            et_ident   = lt_huident                 " Tabelle der Identifikationen einer HU
          EXCEPTIONS
            OTHERS     = 0.

        LOOP AT lt_huident INTO DATA(huident) GROUP BY ( guid_hu = huident-guid_hu ) REFERENCE INTO DATA(lr_grp_hu) ##into_ok.
          LOOP AT GROUP lr_grp_hu INTO DATA(ls_huident).
            IF ls_huident-idart EQ zif_wme_c=>gs_huidart-v.
              DATA(lv_tracking_id) = ls_huident-huident.
            ELSEIF ls_huident-idart EQ zif_wme_c=>gs_huidart-s.
              DATA(lv_sscc) = ls_huident-huident.
            ENDIF.
          ENDLOOP.
          IF lv_tracking_id IS NOT INITIAL.
            APPEND VALUE zif_vce_manifest_model=>ty_packages(
                 external_id = lv_sscc
                 tracking_no = lv_tracking_id
                 is_master_tracking_no = c_false
              ) TO ls_manifest-packages.
          ENDIF.
          CLEAR: lv_tracking_id, lv_sscc.
        ENDLOOP.

      CATCH /scwm/cx_sr_error. " Class for Exceptions in Shipping/Receiving.
        MESSAGE e043(zmc_out) WITH ls_inb_parameters-tu_num INTO lv_msg.
        add_message(  ).
        RETURN.
    ENDTRY.
    ls_request_data = ls_manifest.
*    ls_request_data-manifest = ls_manifest.
    es_data = ls_request_data.
  ENDMETHOD.


  METHOD zif_vce_api_client~get_response.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Get response and map json to internal structure
**********************************************************************
    DATA: ls_response TYPE ty_response.

    clear ev_status.
    DATA(lv_json) = iv_json.

    IF lv_json IS INITIAL.
      lv_json = mo_rest_client->get_api_response( )->get_string_data( ).

      ev_status = mo_rest_client->get_status( ).
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                CHANGING data = ls_response ).
    es_data   = ls_response.


  ENDMETHOD.


  METHOD zif_vce_api_client~process_response.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Process the response data. Get the PDF files, print it and attach
*& to the TU
**********************************************************************
    DATA: ls_response TYPE ty_response,
          lt_huhdr    TYPE /scwm/tt_huhdr_int,
          lv_msg      TYPE string ##needed.

    CONSTANTS:
      c_cmr  TYPE char255 VALUE 'CMR',
      c_adr  TYPE char255 VALUE 'ADR',
      c_true TYPE string VALUE 'TRUE'.

    get_response(
      EXPORTING
        iv_json   = iv_json
      IMPORTING
        ev_status = DATA(lv_status)
        es_data   = ls_response ) ##needed.

    IF ls_response-error EQ c_true.
      MESSAGE e044(zmc_out) INTO lv_msg.
      add_message( ).
      MESSAGE e398(00) INTO lv_msg ##mg_missing.
      cl_message_helper=>set_msg_vars_for_clike( ls_response-error_desc ).
      add_message( ).
      RETURN.
    ENDIF.

    IF ms_build_parameters-tu_num IS INITIAL.
      IF ls_response-details IS INITIAL.
        MESSAGE e045(zmc_out) INTO lv_msg.
        add_message( ).
        RETURN.
      ENDIF.

      READ TABLE ls_response-details REFERENCE INTO DATA(lr_detail) INDEX 1.

      CALL FUNCTION '/SCWM/HU_SELECT_GEN'
        EXPORTING
          iv_lgnum    = mv_lgnum                 " Lagernummer/Lagerkomplex
          ir_idart    = VALUE rseloption( ( sign = 'I' option = 'EQ' low = zif_wme_c=>gs_huidart-s ) )
          ir_addident = VALUE rseloption( ( sign = 'I' option = 'EQ' low = lr_detail->external_id ) )
        IMPORTING
          et_huhdr    = lt_huhdr                 " Tabellentype für HU-Köpfe in interner Struktur
        EXCEPTIONS
          OTHERS      = 0.
      IF lt_huhdr IS INITIAL.
        MESSAGE e046(zmc_out) WITH lr_detail->external_id INTO lv_msg.
        add_message( ).
        RETURN.
      ENDIF.

      DATA(lo_tudlv_manager) = /scwm/cl_sr_tudlv=>get_instance( ).

      lo_tudlv_manager->setup(
                          EXPORTING
                            it_guid_hu = VALUE #( ( guid_hu = lt_huhdr[ 1 ]-guid_hu_top ) ) ).

      lo_tudlv_manager->get_tu_by_hu(
                          EXPORTING
                            iv_guid_hu    = lt_huhdr[ 1 ]-guid_hu_top
                            iv_direct     = abap_false
                            iv_doccat     = wmegc_doccat_pdo
                          IMPORTING
                            et_tu_act_key = DATA(lt_tu_act_key) ).
      IF lt_tu_act_key IS INITIAL.
        MESSAGE e047(zmc_out) WITH lr_detail->external_id INTO lv_msg.
        RETURN.
      ENDIF.
      MOVE-CORRESPONDING lt_tu_act_key[ 1 ] TO ms_build_parameters.
    ENDIF.

    READ TABLE ls_response-parameters REFERENCE INTO DATA(lr_param)
         WITH KEY key = c_cmr.
    IF sy-subrc EQ 0 AND lr_param->value IS NOT INITIAL.
      TRY.

          handle_pdf( iv_key = CONV #( lr_param->key ) iv_pdf_data = convert_base_64_to_raw( lr_param->value ) ).
        CATCH zcx_core_exception.
          add_message( ).
          RETURN.
      ENDTRY.
    ENDIF.

    READ TABLE ls_response-parameters REFERENCE INTO lr_param
         WITH KEY key = c_adr.
    IF sy-subrc EQ 0 AND lr_param->value IS NOT INITIAL.
      TRY.
          handle_pdf( iv_key = CONV #( lr_param->key ) iv_pdf_data = convert_base_64_to_raw( lr_param->value ) ).
        CATCH zcx_core_exception.
          add_message( ).
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD zif_vce_api_client~send_get_request ##needed.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Not implemented
**********************************************************************
  ENDMETHOD.


  METHOD zif_vce_api_client~send_post_request.
**********************************************************************
*& Key           : LH-111123
*& Request No.   : GAP-12 – "VCE Carrier integration”
**********************************************************************
*& Description (short)
*& Send the request
**********************************************************************
    DATA: ls_request TYPE ty_request.

    ls_request = is_data.

    TRY.
        DATA(lv_json) = /ui2/cl_json=>serialize( data        = ls_request
                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        mo_rest_client->post( lv_json ).
      CATCH cx_rest_client_exception INTO DATA(lo_error) ##needed.
        MESSAGE e048(zmc_out) INTO DATA(lv_msg) ##needed.
        add_message( ).
    ENDTRY.

    mo_log->add_parameters( ls_request ).

  ENDMETHOD.
ENDCLASS.
