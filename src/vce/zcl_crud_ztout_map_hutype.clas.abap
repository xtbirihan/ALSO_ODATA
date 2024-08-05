CLASS zcl_crud_ztout_map_hutype DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES tt_ztout_map_hutype TYPE STANDARD TABLE OF ztout_map_hutype WITH EMPTY KEY.

    CLASS-METHODS select_by_lgnum_hutyp
      IMPORTING
        !iv_lgnum              TYPE /scwm/lgnum
        !iv_hutype             TYPE /scwm/de_hutyp
      RETURNING
        VALUE(rv_vce_packtype) TYPE zde_vce_packtype .

    CLASS-METHODS select_multi_by_hutyp
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
        !it_hutype       TYPE rseloption
      RETURNING
        VALUE(rt_result) TYPE tt_ztout_map_hutype .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_MAP_HUTYPE IMPLEMENTATION.


  METHOD select_by_lgnum_hutyp.
********************************************************************
*& Key          : <AHMEDOV>-Nov 8, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_hutype IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE vce_packtype FROM ztout_map_hutype
      INTO @rv_vce_packtype
     WHERE lgnum = @iv_lgnum
      AND hutype = @iv_hutype.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_map_hutype' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_multi_by_hutyp.
********************************************************************
*& Key          : <BSUGAREV>-Nov 14, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum IS INITIAL OR lines( it_hutype ) = 0.
      RETURN.
    ENDIF.

    SELECT * FROM ztout_map_hutype
      INTO TABLE @rt_result
     WHERE lgnum =  @iv_lgnum
      AND hutype IN @it_hutype.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_map_hutype' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
