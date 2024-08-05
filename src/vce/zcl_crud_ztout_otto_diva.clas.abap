class ZCL_CRUD_ZTOUT_OTTO_DIVA definition
  public
  final
  create public .

public section.

  types TY_OTTO_DIVA type ZTOUT_OTTO_DIVA .
  types:
    tt_otto_diva TYPE TABLE OF ty_otto_diva .

  class-methods SELECT_SINGLE_BY_DOCNO_ITEMNO
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_ITEMNO type /SCDL/DL_ITEMNO
      !IV_DOCNO type /SCDL/DL_DOCNO_INT
    returning
      value(RV_EXT_TR_NR) type ZDE_VCE_EX_TR_NR .
  class-methods UPDATE
    importing
      !IT_OTTO_DIVA type TT_OTTO_DIVA .
  class-methods INSERT
    importing
      !IT_OTTO_DIVA type TT_OTTO_DIVA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_OTTO_DIVA IMPLEMENTATION.


  METHOD insert.

    IF it_otto_diva IS INITIAL.
      RETURN.
    ENDIF.

    INSERT ztout_otto_diva
    FROM TABLE it_otto_diva ACCEPTING DUPLICATE KEYS.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_otto_diva' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_single_by_docno_itemno.
********************************************************************
*& Key          : <AAHMEDOV>-Nov 6, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_docno IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE ext_tracking_nr
      FROM ztout_otto_diva
      INTO @rv_ext_tr_nr
     WHERE lgnum EQ @iv_lgnum
      AND docno  EQ @iv_docno
      AND itemno  EQ @iv_itemno
      AND used EQ @abap_false.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_otto_diva' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD update.
********************************************************************
*& Key          : <AAHMEDOV>-Nov 6, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************

    IF it_otto_diva IS INITIAL .
      RETURN.
    ENDIF.

    MODIFY ztout_otto_diva
    FROM TABLE it_otto_diva.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_otto_diva' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
