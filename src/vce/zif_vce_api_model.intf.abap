INTERFACE zif_vce_api_model
  PUBLIC .

  CONSTANTS:
    BEGIN OF gs_msg_extid_key,
      shipcalc(8) TYPE c VALUE 'SHIPCALC',
      shipreq(7)  TYPE c VALUE 'SHIPREQ',
      manifest(8) TYPE c VALUE 'MANIFEST',
      cancel(6)   TYPE c VALUE 'CANCEL',
    END OF gs_msg_extid_key,

    BEGIN OF gs_msg_altcode,
      send    TYPE baltcode VALUE 'SEND',
      receive TYPE baltcode VALUE 'RECEIVE',
    END OF gs_msg_altcode.
*  TYPES:
*    BEGIN OF ty_parcels,
*      weight_in_kg         TYPE p LENGTH 8 DECIMALS 3,
*      length_in_cm         TYPE p LENGTH 8 DECIMALS 3,
*      width_in_cm          TYPE p LENGTH 8 DECIMALS 3,
*      height_in_cm         TYPE p LENGTH 8 DECIMALS 3,
*      volume_in_cbm        TYPE p LENGTH 8 DECIMALS 3,
*      net_weight_in_kg     TYPE p LENGTH 8 DECIMALS 3,
*      chargeable_weight    TYPE p LENGTH 8 DECIMALS 3,
*      dimensional_weight   TYPE p LENGTH 8 DECIMALS 3,
*      loading_meters       TYPE p LENGTH 8 DECIMALS 3,
*      lead_time_in_seconds TYPE i,
*    END OF ty_parcels,
*    tt_parcels TYPE STANDARD TABLE OF ty_parcels WITH EMPTY KEY,
*
*    BEGIN OF ty_pallets,
*      weight_in_kg         TYPE p LENGTH 8 DECIMALS 3,
*      length_in_cm         TYPE p LENGTH 8 DECIMALS 3,
*      width_in_cm          TYPE p LENGTH 8 DECIMALS 3,
*      height_in_cm         TYPE p LENGTH 8 DECIMALS 3,
*      volume_in_cbm        TYPE p LENGTH 8 DECIMALS 3,
*      net_weight_in_kg     TYPE p LENGTH 8 DECIMALS 3,
*      chargeable_weight    TYPE p LENGTH 8 DECIMALS 3,
*      dimensional_weight   TYPE p LENGTH 8 DECIMALS 3,
*      loading_meters       TYPE p LENGTH 8 DECIMALS 3,
*      pallet_type          TYPE string,
*      lead_time_in_seconds TYPE i,
*    END OF ty_pallets,
*    tt_pallets TYPE STANDARD TABLE OF ty_pallets WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_parameter,
      key   TYPE char255,
      value TYPE char255,
    END OF ty_parameter,
    tt_parameter TYPE STANDARD TABLE OF ty_parameter WITH EMPTY KEY.

ENDINTERFACE.
