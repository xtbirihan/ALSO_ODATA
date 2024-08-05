class ZCL_DLV_DET_AT_SAVE definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_DLV_DET_AT_SAVE .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DLV_DET_AT_SAVE IMPLEMENTATION.


  METHOD /scwm/if_ex_dlv_det_at_save~check_header.

    DATA(lo_delivery) = /scwm/cl_dlv_management_prd=>get_instance( ).

    lo_delivery->get_warehouse(
      EXPORTING
        iv_docid = VALUE #( it_keys[ 1 ]-docid OPTIONAL )                " Document ID
      IMPORTING
        ev_whno  = DATA(lv_lgnum)                 " Warehouse Number/Warehouse Complex
    ).

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum  = lv_lgnum
                                               iv_devid  = zif_switch_const=>c_zvce_001 ) <> abap_true.
      RETURN.
    ENDIF.

    IF sy-uname = 'AAHMEDOV'.
      BREAK-POINT.
    ELSE.
      RETURN.
    ENDIF.

    IF sy-tcode EQ zif_param_const=>c_monitor.
      RETURN.
    ENDIF.

    ct_relevant_keys = it_keys.

  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_det_at_save~check_item.

    DATA(lo_delivery) = /scwm/cl_dlv_management_prd=>get_instance( ).

    lo_delivery->get_warehouse(
      EXPORTING
        iv_docid = VALUE #( it_keys[ 1 ]-docid OPTIONAL )                " Document ID
      IMPORTING
        ev_whno  = DATA(lv_lgnum)                 " Warehouse Number/Warehouse Complex
    ).

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum  = lv_lgnum
                                               iv_devid  = zif_switch_const=>c_zvce_001 ) <> abap_true.
      RETURN.
    ENDIF.

    IF sy-tcode EQ zif_param_const=>c_monitor.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_det_at_save~execute_header.

    DATA:
      lo_delivery TYPE REF TO /scwm/cl_dlv_management_prd,
      lo_message  TYPE REF TO /scwm/cl_dm_message_no,
      lt_message  TYPE /scdl/dm_message_tab,
      lx_delivery TYPE REF TO /scdl/cx_delivery,
      lv_qname    TYPE trfcqnam,
      lo_bom      TYPE REF TO /scdl/cl_bo_management,
      lo_bo       TYPE REF TO /scdl/if_bo,
      lt_item     TYPE /scdl/dl_item_tab,
      lo_item     TYPE REF TO /scdl/cl_dl_item_prd,
      ls_sapext   TYPE /scdl/dl_sap_prd_item_o_str,
      lv_lgnum    TYPE /scwm/lgnum,
      lo_header   TYPE REF TO /scdl/cl_dl_header.

    IF sy-uname = 'AAHMEDOV'.
      BREAK-POINT.
    ELSE.
      RETURN.
    ENDIF.

    IF iv_doccat <> /scdl/if_dl_c=>sc_doccat_out_prd.
      RETURN.
    ENDIF.

    lo_delivery = /scwm/cl_dlv_management_prd=>get_instance( ).

    lo_bom = /scdl/cl_bo_management=>get_instance( ).

    lo_bo = lo_bom->get_bo_by_id(
      iv_docid = VALUE #( it_relevant_keys[ 1 ]-docid OPTIONAL )             " Document ID
    ).

    IF lo_bo IS NOT BOUND.
      RETURN.
    ENDIF.

    lo_bo->get_item_tab(
      EXPORTING
        iv_docid = it_relevant_keys[ 1 ]-docid
      IMPORTING
        et_item  = lt_item ).

    lo_item ?= lt_item[ 1 ]-item.

    lo_item->get_sapext(
      IMPORTING
        es_sapext_o = ls_sapext                 " SAP Enhancements: Delivery Work Document Item, Outbound
    ).

    lv_lgnum = ls_sapext-/scwm/whno.

    lo_header = lo_bo->get_header( iv_docid = VALUE #( it_relevant_keys[ 1 ]-docid OPTIONAL ) ).

    lo_header->get_objchg(
      RECEIVING
        ev_objchg = DATA(lv_objchg)                  " Change Mode
    ).

    IF lv_objchg = /scdl/if_dl_object_c=>sc_objchg_create.

      zcl_vce_request=>send_ship_calc_request(
        iv_lgnum = lv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_docid = lo_header->get_docid( )                 " Document ID
        iv_docno = lo_header->get_docno( )                 " Document Number
      ).

    ENDIF.

  ENDMETHOD.


  method /SCWM/IF_EX_DLV_DET_AT_SAVE~EXECUTE_ITEM.

  endmethod.
ENDCLASS.
