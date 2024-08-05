interface ZIF_VCE_CANCEL_MODEL
  public .


  types:
    BEGIN OF ty_packages,
      external_id           TYPE char255,
      order_no              TYPE char10,
      reference             TYPE /scdl/dl_docno_int,
      tracking_no           TYPE /scdl/db_proch_o-docno,
      is_master_tracking_no TYPE boole_d,
    END OF ty_packages .
  types:
    tt_packages TYPE STANDARD TABLE OF ty_packages WITH EMPTY KEY .
  types:
    BEGIN OF manif_request,
      user       TYPE char10,
      parameters TYPE zif_vce_api_model=>tt_parameter,
      packages   TYPE tt_packages,
    END OF  manif_request .
  types:
    BEGIN OF ty_details_response,
      external_id TYPE char255,
      order_no    TYPE char10,
      reference   TYPE /scdl/dl_docno_int,
      tracking_no TYPE /scdl/db_proch_o-docno,
    END OF ty_details_response .
  types:
    tt_details_response TYPE STANDARD TABLE OF ty_details_response WITH EMPTY KEY .
*  TYPES:
*    BEGIN OF ty_request,
*      manifest TYPE manif_request,
*    END OF ty_request.
  types TY_REQUEST type MANIF_REQUEST .
  types:
*      manifest TYPE ,
*    END OF ty_request.
    BEGIN OF ty_response,
      error      TYPE char10,
      error_desc TYPE string,
      parameters TYPE zif_vce_api_model=>tt_parameter,
      details    TYPE tt_details_response,
    END OF ty_response .
endinterface.
