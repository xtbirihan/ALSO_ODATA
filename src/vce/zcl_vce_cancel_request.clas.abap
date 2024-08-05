class ZCL_VCE_CANCEL_REQUEST definition
  public
  final
  create public .

public section.

  interfaces ZIF_VCE_CANCEL_MODEL .
  interfaces ZIF_VCE_API_CLIENT .

  aliases BUILD_REQUEST_DATA
    for ZIF_VCE_API_CLIENT~BUILD_REQUEST_DATA .
  aliases GET_RESPONSE
    for ZIF_VCE_API_CLIENT~GET_RESPONSE .
  aliases PROCESS_RESPONSE
    for ZIF_VCE_API_CLIENT~PROCESS_RESPONSE .
  aliases SEND_GET_REQUEST
    for ZIF_VCE_API_CLIENT~SEND_GET_REQUEST .
  aliases SEND_POST_REQUEST
    for ZIF_VCE_API_CLIENT~SEND_POST_REQUEST .
  aliases TY_REQUEST
    for ZIF_VCE_CANCEL_MODEL~TY_REQUEST .
  aliases TY_RESPONSE
    for ZIF_VCE_CANCEL_MODEL~TY_RESPONSE .

  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/LGNUM
    raising
      ZCX_CORE_EXCEPTION .
protected section.
private section.

  data MO_REST_CLIENT type ref to ZCL_VCE_REST_CLIENT .
  data MO_LOG type ref to ZCL_VCE_LOG .
  data MT_HUIDENT type /SCDL/DL_DB_HUIDENT_TAB .
  constants MC_URL type STRING value 'https://b2b-test.also.com/restv2/VCE/ShipmentCalculation' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_VCE_CANCEL_REQUEST IMPLEMENTATION.


  METHOD constructor.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 11, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    mo_rest_client = NEW zcl_vce_rest_client( iv_url = mc_url ).

  ENDMETHOD.


  METHOD zif_vce_api_client~build_request_data.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 8, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    DATA: ls_canc_req TYPE zif_vce_cancel_model=>ty_request,
          lt_huhdr    TYPE  /scwm/tt_huhdr_int.

    DATA(lt_huid) = CONV /scwm/tt_guid_hu( iv_input ).

    " select HU items
    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        it_guid_hu = lt_huid
      IMPORTING
        et_huhdr   = lt_huhdr
      EXCEPTIONS
        not_found  = 1
        error      = 2
        OTHERS     = 3.
    IF sy-subrc <> 0 OR lines( lt_huhdr ) = 0.
      RETURN.
    ENDIF.

    zcl_crud_huident=>select_by_huid_huidtyp(
      EXPORTING
        it_huid_r     = VALUE rseloption( FOR <ls_huid> IN lt_huid ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <ls_huid>-guid_hu ) )
        it_huidtype_r = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = zif_wme_c=>gs_huidart-v ) )                 " Handling Unit Identification Type
      IMPORTING
        et_huident    = DATA(lt_huident)
    ).

    IF lt_huident IS INITIAL.
      RETURN.
    ENDIF.

    mt_huident = lt_huident.

    ls_canc_req = VALUE #(
        user = ''
        parameters = VALUE #(  )
        packages = VALUE #( FOR GROUPS OF <ls_huident> IN lt_huident
                          GROUP BY <ls_huident>-huid
                          ( external_id = '' "VALUE #( lt_huident[ huid = <ls_huident>-huid huidtype = zif_wme_c=>gs_huidart-s ]-huident OPTIONAL )
                            order_no = ''
                            reference = ''
                            tracking_no = lt_huident[ huid = <ls_huident>-huid
                                                      huidtype = zif_wme_c=>gs_huidart-v ]-huident ) ) ).

    es_data = ls_canc_req.
  ENDMETHOD.


  METHOD zif_vce_api_client~get_response.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 8, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
********************************************************************
    DATA ls_response TYPE ty_response.

    DATA(lv_json) = iv_json.

    IF lv_json IS INITIAL.
      lv_json = mo_rest_client->get_api_response( )->get_string_data( ).

      ev_status = mo_rest_client->get_status( ).
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json        = lv_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = ls_response ).
    es_data = ls_response.

    IF mo_log IS NOT BOUND.
      mo_log = NEW zcl_vce_log( iv_extnumber = 'CANCEL'
                                iv_altcode   = 'RESPONSE' ).
    ENDIF.

    mo_log->add_parameters( ls_response ).

    mo_log->save_log( ).

  ENDMETHOD.


  METHOD zif_vce_api_client~process_response.

    DATA: ls_response TYPE ty_response.

    get_response(
      EXPORTING
        iv_json   = iv_json
      IMPORTING
        ev_status = DATA(lv_status)
        es_data   = ls_response ).

    " save response to SLG1
    DATA(lv_extnumb) = VALUE balnrext( ).
    lv_extnumb = |{ zif_vce_api_model=>gs_msg_extid_key-cancel }-{ sy-datum }|.
    DATA(lo_msg) = NEW zcl_vce_log( iv_extnumber = lv_extnumb
                                    iv_altcode   = zif_vce_api_model=>gs_msg_altcode-receive ).

    IF ls_response-error <> abap_true OR iv_json IS NOT INITIAL.

      DATA(lv_suc) = zcl_crud_huident=>delete_by_huid_huidtyp(
        EXPORTING
          it_huidtype_r = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = zif_wme_c=>gs_huidart-s )
                                   ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = zif_wme_c=>gs_huidart-v ) )                 " SELECT-OPTIONS Table
          it_huid_r     = VALUE #( FOR <ls_package> IN ls_response-details
                                     ( VALUE #( mt_huident[ huident = <ls_package>-external_id ]-huid OPTIONAL ) ) )                 " SELECT-OPTIONS Table
      ).

    ELSE.

    ENDIF.

  ENDMETHOD.


  METHOD zif_vce_api_client~send_post_request.

    DATA: lt_request TYPE ty_request.

    lt_request = is_data.

    TRY.
        DATA(lv_json) = /ui2/cl_json=>serialize( data        = lt_request
                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

*        mo_rest_client->post( lv_json ).
      CATCH cx_rest_client_exception INTO DATA(lo_error).
    ENDTRY.

    DATA(lv_extnumb) = VALUE balnrext( ).
    lv_extnumb = |{ zif_vce_api_model=>gs_msg_extid_key-cancel }-{ sy-datum }|.
    DATA(lo_msg) = NEW zcl_vce_log( iv_extnumber = lv_extnumb
                                    iv_altcode   = zif_vce_api_model=>gs_msg_altcode-send ).

    lo_msg->add_parameters( lt_request ).

    lo_msg->save_log( abap_true ).

  ENDMETHOD.
ENDCLASS.
