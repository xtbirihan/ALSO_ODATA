FUNCTION z_vce_ship_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_HUIDENT) TYPE  /SCWM/DE_HUIDENT
*"     VALUE(IV_PRINTER) TYPE  RSPOPNAME
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AAHMEDOV>-Nov 17, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  : Send & process ShipRequest
********************************************************************

  DATA:
    lo_ship_calc    TYPE REF TO zcl_vce_ship_request,
    lt_request_data TYPE zif_vce_ship_request_model=>tt_request.

  TRY.
      lo_ship_calc = NEW zcl_vce_ship_request( iv_lgnum   = iv_lgnum
                                               iv_printer = iv_printer ).
    CATCH zcx_core_exception. " Superclass for CORE exception classes
      RETURN.
  ENDTRY.

  lo_ship_calc->build_request_data(
    EXPORTING
      iv_input = iv_huident
    IMPORTING
      es_data  = lt_request_data ).
*
  lo_ship_calc->send_post_request( is_data = lt_request_data ).
*
  lo_ship_calc->process_response( ).

ENDFUNCTION.
