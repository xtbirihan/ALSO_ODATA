*&---------------------------------------------------------------------*
*& Report zvce_build_json_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvce_build_json_file.

*DATA: gv_docno TYPE /scwm/dlv_docno_str-docno.

PARAMETERS: p_lgnum TYPE /scwm/lgnum DEFAULT 'DE50' OBLIGATORY.
PARAMETERS: p_docno TYPE /scwm/dlv_docno_str-docno.
PARAMETERS: p_huno TYPE /scwm/s_huhdr-huident.
PARAMETERS: p_tu TYPE /scwm/de_tu_num_ext.

SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
              r2 RADIOBUTTON GROUP rad1,
              r3 RADIOBUTTON GROUP rad1,
              r4 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK part2.

CLASS lcl_start DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lcl_start IMPLEMENTATION.
  METHOD main.
    DATA: lv_request_in_json       TYPE /ui2/cl_json=>json,
          ls_ship_calc_request     TYPE zif_vce_shipment_calc_model=>ty_request,
          lt_shipment_request      TYPE zif_vce_ship_request_model=>tt_request,
          ls_cancel_request_data   TYPE zcl_vce_cancel_request=>ty_request,
          ls_manifest_request_data TYPE zcl_vce_cancel_request=>ty_request.

*    IF p_lgnum IS INITIAL OR p_docno IS INITIAL.
*      RETURN.
*    ENDIF.

    TRY.
        NEW /scwm/cl_dlv_management_prd( )->query(
          EXPORTING
            it_docno        = VALUE #( ( docno = p_docno ) )
            is_read_options = VALUE #( keys_only = abap_true )
            is_include_data = VALUE #( )
          IMPORTING
            et_headers      = DATA(lt_dlv_headers) ).
      CATCH /scdl/cx_delivery.
    ENDTRY.

    IF lines( lt_dlv_headers ) = 0.
*      MESSAGE e452(/scmtms/ui_messages) WITH p_docno.
    ENDIF.

    DATA(ls_dlv_head) = VALUE #( lt_dlv_headers[ 1 ] OPTIONAL ).

    TRY.
        CASE abap_true.
          WHEN r1.

*            NEW zcl_ship_pmat_rough_algorithm( )->pack_deliv_by_default_pmats(
*              EXPORTING
*                iv_lgnum       = p_lgnum
*                iv_doccat      = 'PDO'
*                it_docno       = VALUE #( ( docno = ls_dlv_head-docno ) )
*              IMPORTING
*                et_pack_output = DATA(lt_result)
*                et_bapiret     = DATA(lt_bapiret) ).

            " Shipment calculation request
            DATA(lo_ship_cal) = NEW zcl_vce_shipment_calculation( p_lgnum ).

            lo_ship_cal->build_request_data(
              EXPORTING
                iv_input = ls_dlv_head-docid
              IMPORTING
                es_data  = ls_ship_calc_request ).

            lv_request_in_json = /ui2/cl_json=>serialize( data        = ls_ship_calc_request
                                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
          WHEN r2.
            " Ship request
            DATA(lo_ship_request) = NEW zcl_vce_ship_request( iv_lgnum   = p_lgnum
                                                              iv_printer = 'LOCL' ).

            lo_ship_request->build_request_data(
              EXPORTING
                iv_input = p_huno
              IMPORTING
                es_data  = lt_shipment_request ).

            lv_request_in_json = /ui2/cl_json=>serialize( data        = lt_shipment_request
                                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
          WHEN r3.

            DATA: lt_huhdr    TYPE  /scwm/tt_huhdr_int.
            " Cancel request
            DATA(lo_cancel_request) = NEW zcl_vce_cancel_request( iv_lgnum   = p_lgnum ).

            " select HU items
            CALL FUNCTION '/SCWM/HU_SELECT'
              EXPORTING
                it_huident = VALUE /scwm/tt_huident( ( lgnum = p_lgnum
                                                       huident = p_huno ) )
              IMPORTING
                et_huhdr   = lt_huhdr
              EXCEPTIONS
                not_found  = 1
                error      = 2
                OTHERS     = 3.
            IF sy-subrc <> 0
              OR lt_huhdr IS INITIAL.
              RETURN.
            ENDIF.
*
            lo_cancel_request->build_request_data(
              EXPORTING
                iv_input = VALUE /scwm/tt_guid_hu( ( guid_hu = lt_huhdr[ 1 ]-guid_hu ) )
              IMPORTING
                es_data  = ls_cancel_request_data
            ).

            lv_request_in_json = /ui2/cl_json=>serialize( data        = ls_cancel_request_data
                                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

          WHEN r4.
            SELECT FROM /scwm/tunit
                   FIELDS *
                   WHERE tu_num_ext EQ @p_tu
                   INTO TABLE @DATA(lt_tu).
            IF sy-subrc NE 0.
              RETURN.
            ENDIF.
            DATA(lv_tu) = lt_tu[ 1 ]-tu_num.
            SELECT FROM /scwm/tu_sr_act
                   FIELDS *
                   WHERE tu_num EQ @lv_tu
                   INTO TABLE @DATA(lt_tu_act).
            IF sy-subrc NE 0.
              RETURN.
            ENDIF.
            SORT lt_tu_act BY tu_sr_act_num DESCENDING.

            DATA(ls_param) = VALUE zcl_vce_manifest_request=>ty_build_parameters( tu_num = lt_tu_act[ 1 ]-tu_num tu_sr_act_num = lt_tu_act[ 1 ]-tu_sr_act_num ).
            DATA(lo_manifest_request) = NEW zcl_vce_manifest_request( iv_lgnum   = p_lgnum ).

            lo_manifest_request->build_request_data(
              EXPORTING
                iv_input = ls_param
              IMPORTING
                es_data  = ls_manifest_request_data
            ).

            lv_request_in_json = /ui2/cl_json=>serialize( data        = ls_manifest_request_data
                                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
        ENDCASE.

      CATCH zcx_core_exception.
    ENDTRY.

    IF sy-uname = 'BSUGAREV'.
*      CALL FUNCTION 'Z_VCE_INTERFACE'
*        DESTINATION 'Z_WEBMETHODS'
*        EXPORTING
*          iv_http_request = lv_request_in_json
**      IMPORTING
**         ev_http_request =
**         ev_result       =
*        .
    ENDIF.
*


    cl_demo_output=>display( data = lv_request_in_json ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  NEW lcl_start( )->main( ).
