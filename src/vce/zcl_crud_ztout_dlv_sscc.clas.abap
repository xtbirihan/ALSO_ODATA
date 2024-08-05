class ZCL_CRUD_ZTOUT_DLV_SSCC definition
  public
  final
  create public .

public section.

  types TY_DLV_SSCC type ZTOUT_DLV_SSCC .
  types:
    tt_dlv_sscc TYPE TABLE OF ty_dlv_sscc .

  class-methods SELECT_SINGLE_BY_DOCNO
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCDL/DL_DOCNO_INT
    returning
      value(RV_DLV_SSCC) type ZDE_VCE_SSCC .
  class-methods UPDATE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCDL/DL_DOCNO_INT
      !IV_DLV_SSCC type ZDE_VCE_SSCC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_DLV_SSCC IMPLEMENTATION.


  METHOD select_single_by_docno.
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

    SELECT SINGLE sscc
      FROM ztout_dlv_sscc
      INTO @rv_dlv_sscc
     WHERE lgnum EQ @iv_lgnum
      AND docno  EQ @iv_docno
      AND used EQ @abap_false.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_dlv_sscc' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD update.
********************************************************************
*& Key          : <AAHMEDOV>-Nov 6, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************

    DATA: ls_dlv_sscc TYPE zcl_crud_ztout_dlv_sscc=>ty_dlv_sscc.

    IF iv_docno IS INITIAL AND
      iv_dlv_sscc IS INITIAL.
      RETURN.
    ENDIF.

    ls_dlv_sscc = VALUE #( lgnum = iv_lgnum
                           docno = iv_docno
                           sscc = iv_dlv_sscc
                           used = abap_true ).

    MODIFY ztout_dlv_sscc
    FROM ls_dlv_sscc.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztout_dlv_sscc' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
