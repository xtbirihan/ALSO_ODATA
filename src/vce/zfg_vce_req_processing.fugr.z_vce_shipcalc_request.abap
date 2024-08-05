FUNCTION z_vce_shipcalc_request.
********************************************************************
*& Key          : <AAHMEDOV>-Nov 17, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  : Send & process ShipCalculationRequest
********************************************************************
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_DOCID) TYPE  /SCDL/DL_DOCID
*"----------------------------------------------------------------------

  DATA:
    lo_ship_calc    TYPE REF TO zcl_vce_shipment_calculation,
    ls_request_data TYPE zif_vce_shipment_calc_model=>ty_request.

  TRY.
      lo_ship_calc = NEW zcl_vce_shipment_calculation( iv_lgnum = iv_lgnum ).
    CATCH zcx_core_exception. " Superclass for CORE exception classes
      RETURN.
  ENDTRY.

  lo_ship_calc->build_request_data(
    EXPORTING
      iv_input = iv_docid
    IMPORTING
      es_data  = ls_request_data ).

  lo_ship_calc->send_post_request( is_data = ls_request_data ).

  lo_ship_calc->process_response( ).

ENDFUNCTION.
