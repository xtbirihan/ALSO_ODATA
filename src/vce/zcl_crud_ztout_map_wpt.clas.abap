class ZCL_CRUD_ZTOUT_MAP_WPT definition
  public
  final
  create public .

public section.

  types:
    tt_ztout_map_wpt TYPE STANDARD TABLE OF ztout_map_wpt WITH EMPTY KEY .

  class-methods SELECT_MULTI_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_RESULT) type TT_ZTOUT_MAP_WPT .
  class-methods SELECT_SINGLE_BY_KEY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUTYPE type ZDE_VCE_HUTYPE
    returning
      value(RS_RESULT) type ZTOUT_MAP_WPT .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_MAP_WPT IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
********************************************************************
*& Key          : <BSUGAREV>-Oct 13, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztout_map_wpt
      INTO TABLE @rt_result
     WHERE lgnum = @iv_lgnum.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_map_wpt' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Oct 13, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztout_map_wpt
      INTO @rs_result
     WHERE lgnum  = @iv_lgnum
       AND hutype = @iv_hutype.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_map_wpt' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
