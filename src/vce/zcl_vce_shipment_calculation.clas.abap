CLASS zcl_vce_shipment_calculation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_vce_api_client .
    INTERFACES zif_vce_shipment_calc_model.

    ALIASES ty_request
      FOR zif_vce_shipment_calc_model~ty_request.
    ALIASES ty_bp
      FOR zif_vce_shipment_calc_model~ty_bp.
    ALIASES ty_response
      FOR zif_vce_shipment_calc_model~ty_response.
    ALIASES build_request_data
      FOR zif_vce_api_client~build_request_data .
    ALIASES get_response
      FOR zif_vce_api_client~get_response .
    ALIASES process_response
      FOR zif_vce_api_client~process_response .
    ALIASES send_get_request
      FOR zif_vce_api_client~send_get_request .
    ALIASES send_post_request
      FOR zif_vce_api_client~send_post_request .

    METHODS constructor
      IMPORTING
        !iv_lgnum TYPE /scwm/lgnum
      RAISING
        zcx_core_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_request_data TYPE ty_request .
    CONSTANTS mc_url TYPE string VALUE 'https://b2b-test.also.com/restv2/VCE/ShipmentCalculation' ##NO_TEXT.
    DATA mv_lgnum TYPE /scwm/lgnum .
    DATA mo_rest_client TYPE REF TO zcl_vce_rest_client .
    DATA mo_log TYPE REF TO zcl_vce_log .
    DATA mv_num_of_pallets TYPE i .
    DATA mv_num_of_parcels TYPE i .

    METHODS calc_leadtime_in_sec
      IMPORTING
        !iv_hutype       TYPE zde_vce_hutype
      RETURNING
        VALUE(rv_result) TYPE i .
    METHODS get_shiping_date
      IMPORTING
        !is_dlv_head     TYPE /scwm/dlv_header_out_prd_str
      RETURNING
        VALUE(rv_result) TYPE /scdl/dl_tstto .
    METHODS get_bp_soldto
      IMPORTING
        !is_dlv_item     TYPE /scwm/dlv_item_out_prd_str
      RETURNING
        VALUE(rs_result) TYPE ty_bp .
    METHODS get_bp_shipto
      IMPORTING
        !is_dlv_head     TYPE /scwm/dlv_header_out_prd_str
      RETURNING
        VALUE(rs_result) TYPE ty_bp .
    METHODS get_parcels
      IMPORTING
        !is_parcels      TYPE zcl_ship_pmat_rough_algorithm=>ty_count_with_pmat_data
      RETURNING
        VALUE(rt_result) TYPE zif_vce_shipment_calc_model=>tt_parcels .
    METHODS get_pallets
      IMPORTING
        !it_pallets      TYPE zcl_ship_pmat_rough_algorithm=>ty_count_with_pmat_data
      RETURNING
        VALUE(rt_result) TYPE zif_vce_shipment_calc_model=>tt_pallets .
    METHODS get_bp_ship_from
      IMPORTING
        !iv_lgnum        TYPE /scwm/lgnum
      RETURNING
        VALUE(rs_result) TYPE ty_bp .
    METHODS get_shipping_method
      IMPORTING
        !is_dlv_head     TYPE /scwm/dlv_header_out_prd_str
        !is_dlv_item     TYPE /scwm/dlv_item_out_prd_str
      RETURNING
        VALUE(rt_result) TYPE /scwm/tt_string_table .
    METHODS get_additional_services
      IMPORTING
        !is_dlv_head     TYPE /scwm/dlv_header_out_prd_str
      RETURNING
        VALUE(rt_result) TYPE /scwm/tt_string_table .
    METHODS get_fixed_carrier
      IMPORTING
        !is_dlv_head     TYPE /scwm/dlv_header_out_prd_str
      RETURNING
        VALUE(rv_result) TYPE zde_fixed_carr .
    METHODS is_dangerous_goods_contained
      IMPORTING
        !it_dlv_items    TYPE /scwm/dlv_item_out_prd_tab
      RETURNING
        VALUE(rv_result) TYPE boole_d .
ENDCLASS.



CLASS ZCL_VCE_SHIPMENT_CALCULATION IMPLEMENTATION.


  METHOD calc_leadtime_in_sec.
********************************************************************
*& Key          : AAHMEDOV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: ls_tmplt_data TYPE /scwm/s_tmplt_data.

    DATA(ls_map_wpt) = zcl_crud_ztout_map_wpt=>select_single_by_key(
        iv_lgnum  = mv_lgnum
        iv_hutype = iv_hutype ).

    CALL FUNCTION '/SCWM/WAVE_TMPLT_READ_SINGLE'
      EXPORTING
        iv_lgnum      = mv_lgnum
        iv_tmplt      = ls_map_wpt-wavetmp
      IMPORTING
        es_tmplt_data = ls_tmplt_data
      EXCEPTIONS
        not_found     = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF sy-tcode <> zif_wme_c=>gs_tcodes-zpack_wc.
      "wave completion time - cutoff time
      rv_result = ls_tmplt_data-t_optn_data[ 1 ]-load_comp_tm - ls_tmplt_data-t_optn_data[ 1 ]-cutoff_tm.
    ELSE.
      "wave completion time - pick completion time
      rv_result = abs( ls_tmplt_data-t_optn_data[ 1 ]-load_comp_tm - ls_tmplt_data-t_optn_data[ 1 ]-pick_comp_tm ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    mv_lgnum = iv_lgnum.

    mo_rest_client = NEW zcl_vce_rest_client( iv_url = mc_url ).
  ENDMETHOD.


  METHOD get_additional_services.
********************************************************************
*& Key          : BSUGAREV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    IF is_dlv_head-eew-zzfixed_carr   IS NOT INITIAL OR
       is_dlv_head-eew-zzadd_service1 IS NOT INITIAL.

      rt_result = VALUE /scwm/tt_string_table(
                                    ( CONV string( is_dlv_head-eew-zzadd_service1 ) )
                                    ( CONV string( is_dlv_head-eew-zzadd_service2 ) )
                                    ( CONV string( is_dlv_head-eew-zzadd_service3 ) )
                                    ( CONV string( is_dlv_head-eew-zzadd_service4 ) )
                                    ( CONV string( is_dlv_head-eew-zzadd_service5 ) ) ).

    ELSE.
      DATA(lv_bp) = VALUE #( is_dlv_head-partyloc[ party_role = zif_wme_c=>gs_partyloc_c-sc_party_role_zcuspr ]-partyno OPTIONAL ).

      IF lv_bp IS INITIAL.
        lv_bp = VALUE #( is_dlv_head-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).
      ENDIF.

      DATA(ls_bp_gen_data) = zcl_vce_request_utils=>get_bp_master_data( lv_bp ).
      rt_result = VALUE /scwm/tt_string_table( ( CONV string( ls_bp_gen_data-zz_fix_carr_serv1 ) )
                                         ( CONV string( ls_bp_gen_data-zz_fix_carr_serv2 ) )
                                         ( CONV string( ls_bp_gen_data-zz_fix_carr_serv3 ) )
                                         ( CONV string( ls_bp_gen_data-zz_fix_carr_serv4 ) )
                                         ( CONV string( ls_bp_gen_data-zz_fix_carr_serv5 ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_bp_shipto.
********************************************************************
*& Key          : BSUGAREV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    rs_result-external_id = VALUE #( is_dlv_head-partyloc[ party_role  = /scdl/if_dl_partyloc_c=>sc_party_role_stprt ]-partyno OPTIONAL ).

    DATA(ls_address) = zcl_vce_request_utils=>get_bp( rs_result-external_id ).

    rs_result-city = ls_address-city1.
    rs_result-country = ls_address-country.
    rs_result-house_no = ls_address-house_num1.
    rs_result-name1 = ls_address-name1.
    rs_result-name2 = ls_address-name2.
    rs_result-name3 = ls_address-name3.
    rs_result-post_code = ls_address-post_code1.
    rs_result-reference1 = ''.
    rs_result-reference2 = ''.
    rs_result-street = ls_address-street.
  ENDMETHOD.


  METHOD get_bp_ship_from.
********************************************************************
*& Key          : <BSUGAREV>-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(ls_address) = zcl_vce_request_utils=>get_bp( CONV #( iv_lgnum ) ).

    rs_result-external_id = iv_lgnum.
    rs_result-city = ls_address-city1.
    rs_result-country = ls_address-country.
    rs_result-house_no = ls_address-house_num1.
    rs_result-name1 = ls_address-name1.
    rs_result-name2 = ls_address-name2.
    rs_result-name3 = ls_address-name3.
    rs_result-post_code = ls_address-post_code1.
    rs_result-reference1 = ''.
    rs_result-reference2 = ''.
    rs_result-street = ls_address-street.
  ENDMETHOD.


  METHOD get_bp_soldto.
********************************************************************
*& Key          : BSUGAREV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    rs_result-external_id = VALUE #( is_dlv_item-partyloc[ party_role  = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).

    DATA(ls_address) = zcl_vce_request_utils=>get_bp( rs_result-external_id ).

    rs_result-city = ls_address-city1.
    rs_result-country = ls_address-country.
    rs_result-house_no = ls_address-house_num1.
    rs_result-name1 = ls_address-name1.
    rs_result-name2 = ls_address-name2.
    rs_result-name3 = ls_address-name3.
    rs_result-post_code = ls_address-post_code1.
    rs_result-reference1 = ''.
    rs_result-reference2 = ''.
    rs_result-street = ls_address-street.
  ENDMETHOD.


  METHOD get_fixed_carrier.
********************************************************************
*& Key          : BSUGAREV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
********************************************************************
    DATA(lv_bp) = VALUE #( is_dlv_head-partyloc[ party_role = zif_wme_c=>gs_partyloc_c-sc_party_role_zcuspr ]-partyno OPTIONAL ).

    IF lv_bp IS INITIAL.
      lv_bp = VALUE #( is_dlv_head-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).
    ENDIF.

    rv_result = zcl_vce_request_utils=>get_bp_master_data( lv_bp )-zz_fixed_carr.
  ENDMETHOD.


  METHOD get_pallets.
********************************************************************
*& Key          : <BSUGAREV>-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_matnr_r TYPE rseloption.

    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    DATA(lv_leadtime_in_sec) = calc_leadtime_in_sec( zif_wme_c=>gs_vce_hutype-pallet ).

    TRY.
        DATA(lt_marm_hutyp) = zcl_crud_marm=>select_multi_range(
          it_matnr = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = it_pallets-pmat_details[ 1 ]-nested[ 1 ]-materials[ 1 ]-matnr ) )
          it_uom   = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = zif_wme_c=>gs_uom-pal ) ) ).

        DATA(lt_hutyp_map) = zcl_crud_ztout_map_hutype=>select_multi_by_hutyp(
          iv_lgnum  = mv_lgnum
          it_hutype = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = lt_marm_hutyp[ 1 ]-zz1_hutype_uom ) ) ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    LOOP AT it_pallets-pmat_details ASSIGNING FIELD-SYMBOL(<ls_pallet>).

      APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_res_pallet>).

      <ls_res_pallet>-lead_time_in_seconds = lv_leadtime_in_sec.

      <ls_res_pallet>-pallet_type = VALUE #( lt_hutyp_map[ 1 ]-vce_packtype OPTIONAL ).

      " Weight
      <ls_res_pallet>-weight_in_kg = <ls_pallet>-weight-quan.
      IF <ls_pallet>-weight-unit <> zif_wme_c=>gs_uom-kg.
        TRY.
            <ls_res_pallet>-weight_in_kg = lo_conv->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_pallet>-weight-unit
              iv_uom_to   = zif_wme_c=>gs_uom-kg
              iv_quan     = <ls_pallet>-weight-quan ).
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      " Dimensions
      <ls_res_pallet>-length_in_cm = <ls_pallet>-dist_dim-length.
      <ls_res_pallet>-width_in_cm  = <ls_pallet>-dist_dim-length.
      <ls_res_pallet>-height_in_cm = <ls_pallet>-dist_dim-length.

      IF <ls_pallet>-dist_dim-unit_lwh <> zif_wme_c=>gs_uom-cm.
        TRY.
            <ls_res_pallet>-length_in_cm = lo_conv->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_pallet>-dist_dim-unit_lwh
              iv_uom_to   = zif_wme_c=>gs_uom-cm
              iv_quan     = CONV #( <ls_pallet>-dist_dim-length ) ).
          CATCH /scmb/cx_md_access.
        ENDTRY.

        TRY.
            <ls_res_pallet>-width_in_cm = lo_conv->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_pallet>-dist_dim-unit_lwh
              iv_uom_to   = zif_wme_c=>gs_uom-cm
              iv_quan     = CONV #( <ls_pallet>-dist_dim-width ) ).
          CATCH /scmb/cx_md_access.
        ENDTRY.

        TRY.
            <ls_res_pallet>-height_in_cm = lo_conv->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_pallet>-dist_dim-unit_lwh
              iv_uom_to   = zif_wme_c=>gs_uom-cm
              iv_quan     = CONV #( <ls_pallet>-dist_dim-height ) ).
          CATCH /scmb/cx_md_access.
        ENDTRY.

      ENDIF.

      " Volume
      <ls_res_pallet>-volume_in_cbm = <ls_pallet>-volum-quan.
      IF <ls_pallet>-volum-unit <> zif_wme_c=>gs_uom-m3.
        TRY.
            <ls_res_pallet>-volume_in_cbm = lo_conv->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_pallet>-volum-unit
              iv_uom_to   = zif_wme_c=>gs_uom-m3
              iv_quan     = <ls_pallet>-volum-quan ).
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_parcels.
********************************************************************
*& Key          : <BSUGAREV>-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lo_converter) = /scmb/cl_md_access_mdl=>get_md_access( ).

    DATA(lv_leadtime_in_sec) = calc_leadtime_in_sec( iv_hutype = zif_wme_c=>gs_vce_hutype-parcel ).

    LOOP AT is_parcels-pmat_details ASSIGNING FIELD-SYMBOL(<ls_carton>).

      APPEND INITIAL LINE TO rt_result ASSIGNING FIELD-SYMBOL(<ls_parcel>).

      <ls_parcel>-lead_time_in_seconds = lv_leadtime_in_sec.

      " Weight
      <ls_parcel>-weight_in_kg = <ls_carton>-weight-quan.
      IF <ls_carton>-weight-unit <> zif_wme_c=>gs_uom-kg.
        TRY.
            <ls_parcel>-weight_in_kg = lo_converter->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_carton>-weight-unit
              iv_uom_to   = zif_wme_c=>gs_uom-kg
              iv_quan     = <ls_carton>-weight-quan ).
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

      " Dimensions
      <ls_parcel>-length_in_cm = <ls_carton>-dist_dim-length.
      <ls_parcel>-width_in_cm  = <ls_carton>-dist_dim-length.
      <ls_parcel>-height_in_cm = <ls_carton>-dist_dim-length.

      IF <ls_carton>-dist_dim-unit_lwh <> zif_wme_c=>gs_uom-cm.

        TRY.
            <ls_parcel>-length_in_cm = lo_converter->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_carton>-dist_dim-unit_lwh
              iv_uom_to   = zif_wme_c=>gs_uom-cm
              iv_quan     = CONV #( <ls_carton>-dist_dim-length ) ).
          CATCH /scmb/cx_md_access.
        ENDTRY.

        TRY.
            <ls_parcel>-width_in_cm = lo_converter->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_carton>-dist_dim-unit_lwh
              iv_uom_to   = zif_wme_c=>gs_uom-cm
              iv_quan     = CONV #( <ls_carton>-dist_dim-width ) ).
          CATCH /scmb/cx_md_access.
        ENDTRY.

        TRY.
            <ls_parcel>-height_in_cm = lo_converter->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_carton>-dist_dim-unit_lwh
              iv_uom_to   = zif_wme_c=>gs_uom-cm
              iv_quan     = CONV #( <ls_carton>-dist_dim-height ) ).
          CATCH /scmb/cx_md_access.
        ENDTRY.

      ENDIF.

      " Volume
      <ls_parcel>-volume_in_cbm = <ls_carton>-volum-quan.
      IF <ls_carton>-volum-unit <> zif_wme_c=>gs_uom-m3.
        TRY.
            <ls_parcel>-volume_in_cbm = lo_converter->prod_quan_conversion(
              iv_prodid   = VALUE #( )
              iv_uom_from = <ls_carton>-volum-unit
              iv_uom_to   = zif_wme_c=>gs_uom-m3
              iv_quan     = <ls_carton>-volum-quan ).
          CATCH /scmb/cx_md_access.
        ENDTRY.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_shiping_date.
********************************************************************
*& Key          : BSUGAREV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    rv_result = VALUE #( is_dlv_head-dates[ tsttype = /scdl/if_dl_date_c=>sc_tsttype_outyard ]-tstto OPTIONAL ).
  ENDMETHOD.


  METHOD get_shipping_method.
********************************************************************
*& Key          : BSUGAREV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    " if there is no party role "carrier" in the delivery, shipping method must be empty
    IF NOT line_exists( is_dlv_head-partyloc[ party_role = /scmb/if_tmdl_roles_c=>prt_carrier ] ).
      RETURN.
    ENDIF.

    " Delivery status must not be 1-(Picking not started)
    IF line_exists( is_dlv_head-status[ status_type  = /scdl/if_dl_status_c=>sc_t_picking
                                        status_value = /scdl/if_dl_status_c=>sc_v_not_started ] ).
      RETURN.
    ENDIF.

    DATA(lt_hutype) = zcl_crud_ztout_map_wpt=>select_multi_by_lgnum( mv_lgnum ).

    DATA(lv_hutyp) = VALUE #( lt_hutype[ lgnum = mv_lgnum procty = is_dlv_item-sapext-/scwm/procty ]-hutype OPTIONAL ).

    IF lv_hutyp IS NOT INITIAL.
      rt_result = VALUE #( ( CONV #( lv_hutyp ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD is_dangerous_goods_contained.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  : Check if there are dangerous goods in the delivery
*&
********************************************************************
    IF lines( zcl_vce_request_utils=>read_dangerous_goods( it_dlv_items ) ) > 0.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_vce_api_client~build_request_data.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  : Collect data for the JSON file
********************************************************************
    DATA: ls_request_data TYPE ty_request.

    DATA(lv_docid) = CONV /scdl/dl_docid( iv_input ).

    TRY.
        NEW /scwm/cl_dlv_management_prd( )->query(
          EXPORTING
            it_docid        = VALUE #( ( docid = lv_docid ) )
            iv_whno         = mv_lgnum
            is_read_options = VALUE #( data_retrival_only = abap_true mix_in_object_instances = abap_true )
            is_include_data = VALUE #( head_date     = abap_true
                                       head_partyloc = abap_true
                                       head_status   = abap_true
                                       item_partyloc = abap_true )
          IMPORTING
            et_headers      = DATA(lt_dlv_headers)
            et_items        = DATA(lt_dlv_items) ).
      CATCH /scdl/cx_delivery.
    ENDTRY.

    IF lt_dlv_headers IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_dlv_head) = VALUE #( lt_dlv_headers[ 1 ] OPTIONAL ).
    DATA(ls_dlv_item) = VALUE #( lt_dlv_items[ 1 ] OPTIONAL ).

    " call rough algorithm for calculating the packing of the document
    NEW zcl_ship_pmat_rough_algorithm( )->pack_deliv_by_default_pmats(
      EXPORTING
        iv_lgnum       = mv_lgnum
        iv_doccat      = /scdl/if_dl_doc_c=>sc_doccat_out_prd
        it_docno       = VALUE #( ( docno = ls_dlv_head-docno ) )
      IMPORTING
        et_pack_output = DATA(lt_rough_packing)
        et_bapiret     = DATA(lt_bapiret) ).

    IF lines( lt_rough_packing ) = 0.
      RETURN.
    ENDIF.

    ls_request_data = VALUE #(
      company  = 'COMLINE Computer'
      shipment_number = ls_dlv_head-docno
      shipping_date   = get_shiping_date( ls_dlv_head )
      value_in_euro = ls_dlv_head-eew-zzcomm_hdr_value
      order_reason  = ls_dlv_head-eew-zzaugru
      logistics_parameter = ls_dlv_head-eew-zzlogcon
      shipping_condition  = ls_dlv_item-delterm-service_level
      shipping_methods    = get_shipping_method( is_dlv_head = ls_dlv_head is_dlv_item = ls_dlv_item )
      customer_shipping_condition = get_fixed_carrier( is_dlv_head = ls_dlv_head )
      additional_services = get_additional_services( is_dlv_head = ls_dlv_head )
      contains_dangerous_goods = is_dangerous_goods_contained( lt_dlv_items )
      parameters = VALUE #( ( key = '' value = '' ) )
      seller = VALUE #( external_id   = ls_dlv_head-eew-zzvkorg
                        name          = ls_dlv_head-eew-zzvkorg_txt
                        profit_center = ls_dlv_item-eew-zzvkbur
                        seller_group  = ls_dlv_item-eew-zzcust_grp1 )
      sold_to = zcl_vce_request_utils=>get_bp_sold_to( ls_dlv_item )
      ship_from = zcl_vce_request_utils=>get_bp_ship_from( mv_lgnum )
      ship_to = zcl_vce_request_utils=>get_bp_ship_to( ls_dlv_head )
      parcels = get_parcels( VALUE #( lt_rough_packing[ docno = ls_dlv_head-docno ]-carton OPTIONAL ) )
      pallets = get_pallets( VALUE #( lt_rough_packing[ docno = ls_dlv_head-docno ]-pallet OPTIONAL ) )
    ).

    ms_request_data = ls_request_data.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zout_0001                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_procty_kep                 " Parameter ID for process
      IMPORTING
        et_range     = DATA(lt_procty_kep_r)                 " SELECT-OPTIONS Table
    ).

    IF ls_dlv_item-sapext-/scwm/procty IN lt_procty_kep_r.

      " find number of cartons
      mv_num_of_parcels = VALUE #( lt_rough_packing[ docno = ls_dlv_head-docno ]-carton-total OPTIONAL ).

    ELSE.

      zcl_param=>get_parameter(
        EXPORTING
          iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
          iv_process   = zif_param_const=>c_zout_0001                 " Process ID (Specification, Program, BAdI etc.)
          iv_parameter = zif_param_const=>c_procty_sped                 " Parameter ID for process
        IMPORTING
          et_range     = DATA(lt_procty_sped_r)                 " SELECT-OPTIONS Table
      ).

      " find number of pallets
      mv_num_of_pallets = VALUE #( lt_rough_packing[ docno = ls_dlv_head-docno ]-pallet-total OPTIONAL ).

    ENDIF.

    es_data = ls_request_data.
  ENDMETHOD.


  METHOD zif_vce_api_client~get_response.
********************************************************************
*& Key          : <BSUGAREV>-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA ls_response TYPE ty_response.

    DATA(lv_json) = iv_json.

    IF lv_json IS INITIAL.
      lv_json = mo_rest_client->get_api_response( )->get_string_data( ).

      ev_status = mo_rest_client->get_status( ).
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                CHANGING data = ls_response ).
    es_data = ls_response.

    IF mo_log IS NOT BOUND.
      mo_log = NEW zcl_vce_log( iv_extnumber = 'SHIP_CALCULATION'
                                iv_altcode   = 'RESPONSE' ).
    ENDIF.

    mo_log->add_parameters( ls_response ).

    mo_log->save_log( ).
  ENDMETHOD.


  METHOD zif_vce_api_client~process_response.
********************************************************************
*& Key          : AAHMEDOV-Dec 5, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: ls_response           TYPE ty_response,
          lt_vce_dlv_upd_fields TYPE ztt_vce_dlv_upd_fields,
          lt_insert             TYPE zcl_crud_ztout_ship_calc=>tt_ship_calc,
          lv_cnt                TYPE i VALUE 1,
          lv_add_serv           TYPE char255,
          lv_init_str           TYPE string.

    get_response(
      EXPORTING
        iv_json   = iv_json
      IMPORTING
        ev_status = DATA(lv_status)
        es_data   = ls_response ).

    " save response to SLG1
    DATA(lv_extnumb) = VALUE balnrext( ).
    lv_extnumb = |{ zif_vce_api_model=>gs_msg_extid_key-shipcalc }-{ ms_request_data-shipment_number }|.
    DATA(lo_msg) = NEW zcl_vce_log( iv_extnumber = lv_extnumb
                                    iv_altcode   = zif_vce_api_model=>gs_msg_altcode-receive ).

    lo_msg->add_parameters( ls_response ).

    lo_msg->save_log( abap_true ).

    IF ls_response-error <> abap_true.

      "update dlv with preferred carrier & cutofftime
      "save other carrier proposals in ZTOUT_SHIP_CALC

      " **********************************************************************
      "get values for updating the delivery

      "if preferred services table is empty,
      "take the values from the services table
      DATA(ls_sel_service) =  VALUE #( ls_response-pickups[ 1 ]-preferred_services[ 1 ] OPTIONAL ).
      IF ls_sel_service IS INITIAL.
        ls_sel_service = VALUE #( ls_response-pickups[ 1 ]-services[ 1 ] OPTIONAL ).
      ENDIF.

      IF ls_sel_service IS NOT INITIAL.
        DATA(lv_cutofftime) = VALUE #( ls_sel_service-carrier_service-cut_off_time[ 1 ] OPTIONAL ).
        DATA(lv_hutype) = ls_sel_service-carrier_service-handling_unit_type.
        DATA(lv_bupartner) = zcl_crud_ztout_map_carr=>select_bupartner_by_vce_carr( iv_lgnum         = mv_lgnum
                                                                                    iv_vce_bupartner = ls_sel_service-carrier_service-business_partner
                                                                                    iv_hutype        = CONV zde_vce_hutype( lv_hutype ) ).

        lt_vce_dlv_upd_fields = VALUE ztt_vce_dlv_upd_fields(
          ( docno = ls_response-shipment_number
            bupartner = lv_bupartner
            cutofftime = lv_cutofftime
            hutype = lv_hutype
            carma_user_code = ls_sel_service-carrier_service-carma_user_code
            servicetype = ls_sel_service-carrier_service-service_type
            add_services = REDUCE string( INIT txt = lv_init_str FOR <ser> IN ls_sel_service-carrier_service-additional_services
                                          NEXT txt = |{ txt }{ zif_wme_c=>gs_gen-comma }{ <ser> }| )
            pallet_num = mv_num_of_pallets
            parcel_num = mv_num_of_parcels ) ).
      ENDIF.

      TRY.

          " get values for updating ZTOUT_SHIP_CALC where IS_PREF = TRUE
          LOOP AT ls_response-pickups[ 1 ]-preferred_services ASSIGNING FIELD-SYMBOL(<ls_pref_service>).

            APPEND INITIAL LINE TO lt_insert ASSIGNING FIELD-SYMBOL(<ls_insert>).
            <ls_insert> = VALUE #( mandt = sy-mandt
                                   docno = ls_response-shipment_number
                                   is_pref = abap_true
                                   bupartner = <ls_pref_service>-carrier_service-business_partner
                                   cutofftime = VALUE #( <ls_pref_service>-carrier_service-cut_off_time[ 1 ] OPTIONAL )
                                   hutype = <ls_pref_service>-carrier_service-handling_unit_type
                                   servicetype = <ls_pref_service>-carrier_service-service_type
                                   carma_user_code = <ls_pref_service>-carrier_service-carma_user_code
                                   additionalservices = REDUCE string( INIT txt = lv_init_str FOR <ser> IN <ls_pref_service>-carrier_service-additional_services
                                                                       NEXT txt = |{ txt },{ <ser> }| )
                                   ship_cost_eur = <ls_pref_service>-carrier_service-shipping_cost_in_euro
                                   contract      = <ls_pref_service>-carrier_service-contract
                                   account       = <ls_pref_service>-carrier_service-account
                                   counter = lv_cnt ).

            lv_cnt += 1.
          ENDLOOP.
        CATCH cx_sy_itab_line_not_found.

      ENDTRY.

      TRY.

          " get values for updating ZTOUT_SHIP_CALC where IS_PREF = FALSE
          LOOP AT ls_response-pickups[ 1 ]-services ASSIGNING FIELD-SYMBOL(<ls_service>).

            APPEND INITIAL LINE TO lt_insert ASSIGNING <ls_insert>.
            <ls_insert> = VALUE #( mandt = sy-mandt
                                   docno = ls_response-shipment_number
                                   bupartner = <ls_service>-carrier_service-business_partner
                                   cutofftime = VALUE #( <ls_service>-carrier_service-cut_off_time[ 1 ] OPTIONAL )
                                   hutype = <ls_service>-carrier_service-handling_unit_type
                                   servicetype = <ls_service>-carrier_service-service_type
                                   carma_user_code = <ls_service>-carrier_service-carma_user_code
                                   additionalservices = REDUCE string( INIT txt = lv_init_str FOR <ser> IN <ls_service>-carrier_service-additional_services
                                                                       NEXT txt = |{ txt },{ <ser> }| )
                                   ship_cost_eur = <ls_service>-carrier_service-shipping_cost_in_euro
                                   contract      = <ls_service>-carrier_service-contract
                                   account       = <ls_service>-carrier_service-account
                                   counter = lv_cnt ).

            lv_cnt += 1.
          ENDLOOP.

        CATCH cx_sy_itab_line_not_found.

      ENDTRY.

      TRY.

          zcl_param=>get_parameter(
            EXPORTING
              iv_lgnum     = mv_lgnum                  " Warehouse Number/Warehouse Complex
              iv_process   = zif_param_const=>c_zvce_0002                 " Process ID (Specification, Program, BAdI etc.)
              iv_parameter = zif_param_const=>c_backup_srv_type                 " Parameter ID for process
            IMPORTING
              et_range     = DATA(lt_backup_srv_type_r)                 " SELECT-OPTIONS Table
          ).

          " get values for updating ZTOUT_SHIP_CALC where IS_BACKUP = TRUE
          LOOP AT ls_response-pickups[ 1 ]-backup_services ASSIGNING FIELD-SYMBOL(<ls_backup_service>).

            DATA(lv_srv_type) = to_upper( <ls_backup_service>-carrier_service-service_type ).

            CHECK lv_srv_type IN lt_backup_srv_type_r.

            APPEND INITIAL LINE TO lt_insert ASSIGNING <ls_insert>.
            <ls_insert> = VALUE #( mandt = sy-mandt
                                   docno = ls_response-shipment_number
                                   bupartner = <ls_backup_service>-carrier_service-business_partner
                                   is_backup = abap_true
                                   cutofftime = VALUE #( <ls_backup_service>-carrier_service-cut_off_time[ 1 ] OPTIONAL )
                                   hutype = <ls_backup_service>-carrier_service-handling_unit_type
                                   servicetype = <ls_backup_service>-carrier_service-service_type
                                   carma_user_code = <ls_backup_service>-carrier_service-carma_user_code
                                   additionalservices = REDUCE string( INIT txt = lv_init_str FOR <ser> IN <ls_service>-carrier_service-additional_services
                                                                       NEXT txt = |{ txt },{ <ser> }| )
                                   ship_cost_eur = <ls_backup_service>-carrier_service-shipping_cost_in_euro
                                   contract      = <ls_backup_service>-carrier_service-contract
                                   account       = <ls_backup_service>-carrier_service-account
                                   counter = lv_cnt ).

            lv_cnt += 1.
          ENDLOOP.

        CATCH cx_sy_itab_line_not_found.

      ENDTRY.

      " **********************************************************************
      zcl_crud_ztout_ship_calc=>delete(
        EXPORTING
          it_docno_r = VALUE rseloption( FOR <ls_docno> IN lt_insert
                                         ( sign   = wmegc_sign_inclusive
                                           option = wmegc_option_eq
                                           low    = <ls_docno>-docno ) )
        IMPORTING
          ev_suc     = DATA(lv_suc) ).

      zcl_crud_ztout_ship_calc=>modify( EXPORTING it_ship_calc = lt_insert
                                        IMPORTING ev_suc       = lv_suc ).
    ELSE.

      "change error description field of DLV
      lt_vce_dlv_upd_fields = VALUE ztt_vce_dlv_upd_fields( ( docno = ls_response-shipment_number
                                                              error_desc = ls_response-error_desc
                                                              pallet_num = mv_num_of_pallets
                                                              parcel_num = mv_num_of_parcels ) ).
    ENDIF.

    IF lt_vce_dlv_upd_fields IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'Z_VCE_DLV_UPDATE'
      EXPORTING
        iv_lgnum      = mv_lgnum
        it_dlv_update = lt_vce_dlv_upd_fields.

  ENDMETHOD.


  METHOD zif_vce_api_client~send_get_request.

  ENDMETHOD.


  METHOD zif_vce_api_client~send_post_request.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description  : Send request to 3th party
*&
*&
********************************************************************
    DATA: lt_request TYPE ty_request.

    lt_request = is_data.

    TRY.
        DATA(lv_json) = /ui2/cl_json=>serialize( data        = lt_request
                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        mo_rest_client->post( lv_json ).
      CATCH cx_rest_client_exception INTO DATA(lo_error).
    ENDTRY.

    " save sent data to SLG1
    DATA(lv_extnumb) = VALUE balnrext( ).
    lv_extnumb = |{ zif_vce_api_model=>gs_msg_extid_key-shipcalc }-{ ms_request_data-shipment_number }|.
    DATA(lo_msg) = NEW zcl_vce_log( iv_extnumber = lv_extnumb
                                    iv_altcode   = zif_vce_api_model=>gs_msg_altcode-send ).

    lo_msg->add_parameters( lt_request ).

    lo_msg->save_log( abap_true ).
  ENDMETHOD.
ENDCLASS.
