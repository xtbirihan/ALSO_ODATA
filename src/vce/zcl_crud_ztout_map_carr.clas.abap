CLASS zcl_crud_ztout_map_carr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_ztout_map_carr TYPE STANDARD TABLE OF ztout_map_carr WITH EMPTY KEY .

    CLASS-METHODS select_bupartner_by_vce_carr
      IMPORTING
        !iv_lgnum               TYPE /scwm/lgnum
        !iv_hutype              TYPE zde_vce_hutype
        !iv_vce_bupartner       TYPE zde_vce_carr
      RETURNING
        VALUE(rv_ewm_bupartner) TYPE /scdl/dl_partyno .
    CLASS-METHODS select_multi_by_bupartner
      IMPORTING
        !iv_lgnum     TYPE /scwm/lgnum
        !iv_partner   TYPE /scdl/dl_partyno
      RETURNING
        VALUE(rt_map) TYPE tt_ztout_map_carr .

    CLASS-METHODS select_multi_by_carr
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !it_carr_selopt TYPE rseloption
      RETURNING
        VALUE(rt_map)   TYPE tt_ztout_map_carr .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_MAP_CARR IMPLEMENTATION.


  METHOD select_bupartner_by_vce_carr.
********************************************************************
*& Key          : <AHMEDOV>-Nov 6, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_vce_bupartner IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE bupartner FROM ztout_map_carr
      INTO @rv_ewm_bupartner
     WHERE lgnum = @iv_lgnum
      AND carr = @iv_vce_bupartner
      AND hutype = @iv_hutype.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_map_carr' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_multi_by_bupartner.
    SELECT FROM ztout_map_carr
         FIELDS *
          WHERE lgnum EQ @iv_lgnum
            AND bupartner EQ @iv_partner
           INTO TABLE @rt_map.
  ENDMETHOD.


  METHOD select_multi_by_carr.
********************************************************************
*& Key          : BSUGAREV-Jan 11, 2024
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL OR lines( it_carr_selopt ) = 0.
      RETURN.
    ENDIF.

    SELECT * FROM ztout_map_carr
      INTO TABLE @rt_map
     WHERE lgnum = @iv_lgnum
       AND carr IN @it_carr_selopt.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_map_carr' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
