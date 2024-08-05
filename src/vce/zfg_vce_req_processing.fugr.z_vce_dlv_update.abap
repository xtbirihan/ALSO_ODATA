FUNCTION z_vce_dlv_update.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DLV_UPDATE) TYPE  ZTT_VCE_DLV_UPD_FIELDS
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AAHMEDOV>-Nov 17, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  : Update delivery
*&
*&
********************************************************************

  DATA:
      lo_helper TYPE REF TO lcl_helper.

  TRY.
      lo_helper = NEW lcl_helper( iv_lgnum ).
      lo_helper->main( it_dlv_fields = it_dlv_update ).

    CATCH lcx_constructor_error.
      MESSAGE e490(/scwm/delivery).
    CATCH lcx_bom_error.
      MESSAGE e490(/scwm/delivery).
  ENDTRY.

ENDFUNCTION.
