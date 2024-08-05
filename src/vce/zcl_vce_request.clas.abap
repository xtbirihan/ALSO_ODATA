class ZCL_VCE_REQUEST definition
  public
  final
  create public .

public section.

  class-methods SEND_SHIP_REQUEST
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUIDENT type /SCWM/DE_HUIDENT
      !IV_PRINTER type RSPOPNAME optional .
  class-methods SEND_SHIP_CALC_REQUEST
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCID type /SCDL/DL_DOCID
      !IV_DOCNO type /SCDL/DL_DOCNO_INT .
  class-methods SEND_CANCEL_REQUEST
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_GUID_HU type /SCWM/TT_GUID_HU .
protected section.
private section.

  class-methods GET_PRINTER
    returning
      value(RV_PRINTER) type RSPOPNAME
    raising
      ZCX_CORE_EXCEPTION .
ENDCLASS.



CLASS ZCL_VCE_REQUEST IMPLEMENTATION.


  METHOD get_printer.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 6, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    DATA: lv_terminal TYPE /scwm/de_wc_terminal,
          lv_msg      TYPE string.

    "get printer device from custom table "ZOUT_TERM_DEF"
    CALL FUNCTION 'TH_USER_INFO'
      EXPORTING
        client   = sy-mandt
        user     = sy-uname
      IMPORTING
        terminal = lv_terminal.

    IF lv_terminal IS NOT INITIAL.
      DATA(ls_print_dev) = zcl_crud_zout_term_def=>select_single_by_key( lv_terminal ).
    ENDIF.

    IF ls_print_dev-printer IS INITIAL.
      MESSAGE e003(zmc_odata) WITH lv_terminal 'ZOUT_TERM_DEF' INTO lv_msg.
      RAISE EXCEPTION NEW ZCX_CORE_EXCEPTION( ).
    ENDIF.

  ENDMETHOD.


  METHOD send_cancel_request.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 11, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

*    DATA lv_qname TYPE trfcqnam.
*
*    lv_qname = |{ zif_wme_c=>gs_vce_queue-vce }{ zif_wme_c=>gs_vce_queue-cancel }{ sy-datum }|.
*
*    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
*      EXPORTING
*        qin_name   = lv_qname
*        no_execute = abap_true.
*
*    CALL FUNCTION 'Z_VCE_CANCEL_REQUEST' IN BACKGROUND TASK
*      EXPORTING
*        iv_lgnum   = iv_lgnum
*        it_guid_hu = it_guid_hu.

    DATA:
      lo_cancel       TYPE REF TO zcl_vce_cancel_request,
      ls_request_data TYPE zcl_vce_cancel_request=>ty_request,
      lv_response     TYPE /ui2/cl_json=>json.

    TRY.
        lo_cancel = NEW zcl_vce_cancel_request( iv_lgnum = iv_lgnum ).
      CATCH zcx_core_exception. " Superclass for CORE exception classes
        RETURN.
    ENDTRY.

    lo_cancel->build_request_data(
      EXPORTING
        iv_input = it_guid_hu
      IMPORTING
        es_data  = ls_request_data
    ).

    lo_cancel->send_post_request( EXPORTING is_data = ls_request_data
                                  IMPORTING ev_json = DATA(lv_json) ).

    CALL FUNCTION 'Z_VCE_INTERFACE' DESTINATION 'Z_WEBMETHODS'
      EXPORTING
        iv_http_request = lv_json
      IMPORTING
        ev_http_request = lv_json
        ev_result       = lv_response.

  ENDMETHOD.


  METHOD send_ship_calc_request.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 6, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    DATA lv_qname TYPE trfcqnam.

    lv_qname = |{ zif_wme_c=>gs_vce_queue-vce }{ zif_wme_c=>gs_vce_queue-ship_calc }{ sy-datum(2) }{ iv_docno ALPHA = OUT }|.

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
        qin_name   = lv_qname
        no_execute = abap_true.

    CALL FUNCTION 'Z_VCE_SHIPCALC_REQUEST' IN BACKGROUND TASK
      EXPORTING
        iv_lgnum = iv_lgnum
        iv_docid = iv_docid.

  ENDMETHOD.


  METHOD send_ship_request.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 5, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    DATA lv_qname TYPE trfcqnam.

    IF iv_printer IS INITIAL.
      TRY.
          DATA(lv_printer) = get_printer( ).
        CATCH ZCX_CORE_EXCEPTION INTO DATA(lo_exc).
          DATA(lv_msg) = lo_exc->get_text(  ).
          MESSAGE s174(hrpaydeal) DISPLAY LIKE wmegc_severity_err.
          RETURN.
      ENDTRY.
    ENDIF.

    lv_qname = |{ zif_wme_c=>gs_vce_queue-vce }{ zif_wme_c=>gs_vce_queue-ship }{ sy-datum }{ iv_huident ALPHA = OUT }|.

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
        qin_name   = lv_qname
        no_execute = abap_true.

    CALL FUNCTION 'Z_VCE_SHIP_REQUEST' IN BACKGROUND TASK
      EXPORTING
        iv_lgnum   = iv_lgnum
        iv_huident = iv_huident
        iv_printer = lv_printer.

  ENDMETHOD.
ENDCLASS.
