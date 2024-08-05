INTERFACE zif_vce_shipment_calc_model
  PUBLIC .


  TYPES:
    BEGIN OF ty_bp,
      external_id TYPE /scdl/dl_partyno,
      name1       TYPE char40,
      name2       TYPE char40,
      name3       TYPE char40,
      street      TYPE char60,
      house_no    TYPE char10,
      post_code   TYPE char10,
      city        TYPE char40,
      country     TYPE char3,
      reference1  TYPE char10,
      reference2  TYPE char10,
    END OF ty_bp,

    BEGIN OF ty_seller,
      external_id   TYPE /scdl/db_proch_o-vkorg,
      name          TYPE /scdl/db_proch_o-zzvkorg_txt,
      profit_center TYPE /scdl/db_proci_o-zzvkbur,
      seller_group  TYPE /scdl/db_proci_o-zzvkgrp,
    END OF ty_seller,

    BEGIN OF ty_parcels,
      weight_in_kg         TYPE p LENGTH 8 DECIMALS 3,
      length_in_cm         TYPE p LENGTH 8 DECIMALS 3,
      width_in_cm          TYPE p LENGTH 8 DECIMALS 3,
      height_in_cm         TYPE p LENGTH 8 DECIMALS 3,
      volume_in_cbm        TYPE p LENGTH 8 DECIMALS 3,
      net_weight_in_kg     TYPE p LENGTH 8 DECIMALS 3,
      chargeable_weight    TYPE p LENGTH 8 DECIMALS 3,
      dimensional_weight   TYPE p LENGTH 8 DECIMALS 3,
      loading_meters       TYPE p LENGTH 8 DECIMALS 3,
      lead_time_in_seconds TYPE i,
    END OF ty_parcels,
    tt_parcels TYPE STANDARD TABLE OF ty_parcels WITH EMPTY KEY,

    BEGIN OF ty_pallets,
      weight_in_kg         TYPE p LENGTH 8 DECIMALS 3,
      length_in_cm         TYPE p LENGTH 8 DECIMALS 3,
      width_in_cm          TYPE p LENGTH 8 DECIMALS 3,
      height_in_cm         TYPE p LENGTH 8 DECIMALS 3,
      volume_in_cbm        TYPE p LENGTH 8 DECIMALS 3,
      net_weight_in_kg     TYPE p LENGTH 8 DECIMALS 3,
      chargeable_weight    TYPE p LENGTH 8 DECIMALS 3,
      dimensional_weight   TYPE p LENGTH 8 DECIMALS 3,
      loading_meters       TYPE p LENGTH 8 DECIMALS 3,
      pallet_type          TYPE string,
      lead_time_in_seconds TYPE i,
    END OF ty_pallets,
    tt_pallets TYPE STANDARD TABLE OF ty_pallets WITH EMPTY KEY,

    BEGIN OF ty_carrier_service_fields,
      handling_unit_type    TYPE char10,
      business_partner      TYPE char10,
      contract              TYPE char10,
      account               TYPE char10,
      carma_user_code       TYPE string,
      service_type          TYPE char10,
      additional_services   TYPE /scwm/tt_string_table,
      cut_off_time          TYPE bu_timestamp_t,
      shipping_cost_in_euro TYPE char10,
    END OF ty_carrier_service_fields,

    BEGIN OF ty_carrier_service,
      carrier_service TYPE ty_carrier_service_fields,
    END OF ty_carrier_service,

    tt_carrier_service TYPE STANDARD TABLE OF ty_carrier_service WITH EMPTY KEY,

    BEGIN OF ty_shipment_pickup,
      pickup_date        TYPE timestamp,
      preferred_services TYPE tt_carrier_service,
      backup_services    TYPE tt_carrier_service,
      services           TYPE tt_carrier_service,
    END OF ty_shipment_pickup,
    tt_shipment_pickup TYPE STANDARD TABLE OF ty_shipment_pickup WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_shipment_calculation,
      company                     TYPE char35,
      shipment_number             TYPE /scdl/db_proch_o-docno,
      shipping_date               TYPE timestamp,
*      lead_time_in_seconds      TYPE i,
      value_in_euro               TYPE p LENGTH 8 DECIMALS 3,
      order_reason                TYPE /scdl/incl_eew_dlv_head_str-zzaugru,
*      same_day_ship_out         TYPE boole_d,
      logistics_parameter         TYPE /scdl/incl_eew_dlv_head_str-zzlogcon,
      shipping_condition          TYPE /scdl/db_proci_o-service_level,
      shipping_methods            TYPE /scwm/tt_string_table,
      customer_shipping_condition TYPE char255,
      additional_services         TYPE /scwm/tt_string_table,
*      is_bulky                    TYPE boole_d,
*      is_fragile                  TYPE boole_d,
*      requires_two_man_handling   TYPE boole_d,
      contains_dangerous_goods    TYPE boole_d,
      parameters                  TYPE zif_vce_api_model=>tt_parameter,
      seller                      TYPE ty_seller,
      sold_to                     TYPE ty_bp,
      ship_from                   TYPE ty_bp,
      ship_to                     TYPE ty_bp,
      parcels                     TYPE tt_parcels,
      pallets                     TYPE tt_pallets,
    END OF ty_shipment_calculation.

*  TYPES:
*    BEGIN OF ty_request,
*      shipment_calculation TYPE ty_shipment_calculation,
*    END OF ty_request.

  TYPES:
      ty_request TYPE ty_shipment_calculation.

  TYPES:
    BEGIN OF ty_response,
      error           TYPE boole_d,
      error_desc      TYPE string,
      shipment_number TYPE /scdl/db_proch_o-docno,
      pickups         TYPE tt_shipment_pickup,
    END OF ty_response.

ENDINTERFACE.
