FUNCTION z_vce_cancel_request.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IT_GUID_HU) TYPE  /SCWM/TT_GUID_HU
*"----------------------------------------------------------------------

  DATA:
    lo_cancel       TYPE REF TO zcl_vce_cancel_request,
    ls_request_data TYPE zcl_vce_cancel_request=>ty_request.

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

  lo_cancel->process_response( ).

ENDFUNCTION.
