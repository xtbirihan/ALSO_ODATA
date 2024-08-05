INTERFACE zif_vce_manifest_model
  PUBLIC .

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Structure packages data</p>
    BEGIN OF ty_packages,
      external_id           TYPE char255,
      order_no              TYPE char10,
      reference             TYPE /scdl/dl_docno_int,
      tracking_no           TYPE /scdl/db_proch_o-docno,
      is_master_tracking_no TYPE boole_d,
    END OF ty_packages,
    "! <p class="shorttext synchronized" lang="en">Table type packages data</p>
    tt_packages TYPE STANDARD TABLE OF ty_packages WITH EMPTY KEY.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Structure request data</p>
    BEGIN OF manif_request,
      user       TYPE char10,
      parameters TYPE zif_vce_api_model=>tt_parameter,
      packages   TYPE tt_packages,
    END OF  manif_request.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Structure details response data</p>
    BEGIN OF ty_details_response,
      external_id TYPE char255,
      order_no    TYPE char10,
      reference   TYPE /scdl/dl_docno_int,
      tracking_no TYPE /scdl/db_proch_o-docno,
    END OF ty_details_response,
    "! <p class="shorttext synchronized" lang="en">Table type details response data</p>
    tt_details_response TYPE STANDARD TABLE OF ty_details_response WITH EMPTY KEY.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Structure request data</p>
*    BEGIN OF ty_request,
*      manifest TYPE manif_request,
*    END OF ty_request.

    ty_request TYPE manif_request.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Structure response data</p>
    BEGIN OF ty_response,
      error      TYPE char10,
      error_desc TYPE string,
      parameters TYPE zif_vce_api_model=>tt_parameter,
      details    TYPE tt_details_response,
    END OF ty_response.

  CONSTANTS:
    BEGIN OF c_param_key,
      carrier                    TYPE string VALUE 'CARRIER',
      loadnumber                 TYPE string VALUE 'Loadnumber',
      cmr                        TYPE string VALUE 'CMR',
      cmr_5_attached_documents   TYPE string VALUE 'CMR_5_attached_documents',
      cmr_13_sender_instructions TYPE string VALUE 'CMR_13_sender_instructions',
    END OF c_param_key.
ENDINTERFACE.
