class ZCL_VCE_SHIP_REQUEST definition
  public
  final
  create public .

public section.

  interfaces ZIF_VCE_API_CLIENT .
  interfaces ZIF_VCE_SHIP_REQUEST_MODEL .

  aliases BUILD_REQUEST_DATA
    for ZIF_VCE_API_CLIENT~BUILD_REQUEST_DATA .
  aliases GET_RESPONSE
    for ZIF_VCE_API_CLIENT~GET_RESPONSE .
  aliases PROCESS_RESPONSE
    for ZIF_VCE_API_CLIENT~PROCESS_RESPONSE .
  aliases SEND_GET_REQUEST
    for ZIF_VCE_API_CLIENT~SEND_GET_REQUEST .
  aliases SEND_POST_REQUEST
    for ZIF_VCE_API_CLIENT~SEND_POST_REQUEST .
  aliases TT_PACKAGES_REQUEST
    for ZIF_VCE_SHIP_REQUEST_MODEL~TT_PACKAGES_REQUEST .
  aliases TT_PACKDETAILS
    for ZIF_VCE_SHIP_REQUEST_MODEL~TT_PACKDETAILS .
  aliases TT_REQUEST
    for ZIF_VCE_SHIP_REQUEST_MODEL~TT_REQUEST .
  aliases TY_BP
    for ZIF_VCE_SHIP_REQUEST_MODEL~TY_BP .
  aliases TY_RESPONSE
    for ZIF_VCE_SHIP_REQUEST_MODEL~TY_RESPONSE .

  methods SEND_REQUEST .
  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PRINTER type RSPOPNAME
    raising
      ZCX_CORE_EXCEPTION .
  class-methods POST_REQ_MOCKDATA
    returning
      value(RV_JSON) type /UI2/CL_JSON=>JSON .
  class-methods MARA_READ_MULTI
    importing
      !IT_MATNR type /SCWM/TT_MATNR_R
    returning
      value(RT_MARA) type MARA_TT .
  PROTECTED SECTION.
private section.

  constants MC_URL type STRING value 'https://api.also.uat.vce.de/api/v1/ship' ##NO_TEXT.
  data MV_LGNUM type /SCWM/LGNUM .
  data MO_REST_CLIENT type ref to ZCL_VCE_REST_CLIENT .
*    DATA mo_log TYPE REF TO zcl_vce_log .
  data MS_HUHDR type /SCWM/S_HUHDR_INT .
  data MV_PRINTER type RSPOPNAME .
  data MS_DLV_HEADER type /SCWM/DLV_HEADER_OUT_PRD_STR .
  data MS_DLV_ITEM type /SCWM/DLV_ITEM_OUT_PRD_STR .

  methods GET_RETURN_ADDRESS
    importing
      !IT_PARTYLOC_H type /SCDL/DL_PARTYLOC_TAB
      !IT_PARTYLOC_I type /SCDL/DL_PARTYLOC_TAB
    returning
      value(RS_RESULT) type TY_BP .
  methods ATTACH_HU_LABEL
    importing
      !IV_PDF_LABEL_DATA type FPCONTENT .
  methods UPDATE_TABLES
    importing
      !IS_RESPONSE type TY_RESPONSE .
  methods GET_PARAMETERS
    importing
      !IV_DOCNO type /SCDL/DL_DOCNO_INT
      !IV_HU_GUID type /SCWM/GUID_HU
      !IV_EX_TR_NR type CHAR20
      !IV_LAST_LOADING_UNIT type XFELD
    returning
      value(RT_PARAMETERS) type ZIF_VCE_API_MODEL=>TT_PARAMETER .
  methods PRINT_FILE
    importing
      !IS_RESPONSE type TY_RESPONSE .
  methods GET_COVER_ADDRESS
    importing
      !IV_IS_VIND type XFELD
      !IV_IS_VINV type XFELD
      !IT_PARTYLOC_I type /SCDL/DL_PARTYLOC_TAB optional
      !IT_PARTYLOC_H type /SCDL/DL_PARTYLOC_TAB
    returning
      value(RS_RESULT) type TY_BP .
  methods GET_SENDER_ADDRESS
    returning
      value(RS_RESULT) type TY_BP .
  methods GET_RECIEVER_ADDRESS
    importing
      !IV_PARTYNO type /SCDL/DL_PARTYNO
      !IS_ADDRESS type ADDR1_VAL
    returning
      value(RS_RESULT) type TY_BP .
  methods GET_SHIPREQ_PACKDETAILS
    importing
      !IS_DLV_HEAD type /SCWM/DLV_HEADER_OUT_PRD_STR
      !IT_DLV_ITEMS type /SCWM/DLV_ITEM_OUT_PRD_TAB
      !IT_HUITMS type /SCWM/TT_HUITM_INT
    returning
      value(RT_RESULT) type TT_PACKDETAILS .
  methods GET_ADDITIONAL_SERVICES
    importing
      !IT_SHIP_CALC type ZCL_CRUD_ZTOUT_SHIP_CALC=>TT_SHIP_CALC
    exporting
      !EV_GET_RET_ADDRESS type XFELD
    returning
      value(RT_RESULT) type /SCWM/TT_STRING_TABLE .
  methods GET_SERVICES
    importing
      !IT_SHIP_CALC type ZCL_CRUD_ZTOUT_SHIP_CALC=>TT_SHIP_CALC
    returning
      value(RT_RESULT) type /SCWM/TT_STRING_TABLE .
  methods GET_PACKAGES
    importing
      !IS_HUHDR type /SCWM/S_HUHDR_INT
      !IS_DLV_HDR type /SCWM/DLV_HEADER_OUT_PRD_STR
      !IT_PACKDETAILS type TT_PACKDETAILS
      !IS_DLV_ITM type /SCWM/DLV_ITEM_OUT_PRD_STR
      !IT_HUITM type /SCWM/TT_HUITM_INT
    returning
      value(RT_RESULT) type TT_PACKAGES_REQUEST .
ENDCLASS.



CLASS ZCL_VCE_SHIP_REQUEST IMPLEMENTATION.


  METHOD attach_hu_label.

    DATA: lo_attachment_ui TYPE REF TO cl_odata_cv_attachment_api,
          lv_object_key    TYPE objky VALUE '000D3A2E4C971EDE94D99A238784D07B',
          lv_file_name     TYPE string,
          lv_file_size     TYPE i,
          ls_media_content TYPE if_odata_cv_attachment_api=>ty_s_media_resource,
          ls_request       TYPE /scwm/if_api_attachments=>ys_file,
          lv_mime_type     TYPE skwf_mime VALUE 'application/pdf',
          lv_object_type   TYPE dokob VALUE 'HU',
          lv_xstring       TYPE  xstring.

    lo_attachment_ui = cl_odata_cv_attachment_api=>get_instance( ).

    IF ms_huhdr-guid_hu IS NOT INITIAL.
      lv_object_key = ms_huhdr-guid_hu.
    ENDIF.

*    CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
*      EXPORTING
*        input    = CONV string( VALUE #( is_response-packages[ 1 ]-zpl_label_data OPTIONAL ) )
*        unescape = abap_true
*      IMPORTING
*        output   = lv_xstring
*      EXCEPTIONS
*        failed   = 1
*        OTHERS   = 2.
*    IF sy-subrc <> 0.
**      implement suitable error handling here
*      RETURN.
*    ENDIF.

    ls_media_content-filename  = |HU1{ ms_huhdr-huident }.pdf|.
    ls_media_content-value     = CONV xstring( iv_pdf_label_data ).
    ls_media_content-mime_type = lv_mime_type.

    ls_request-object_key  = lv_object_key.
    ls_request-object_type = lv_object_type.
    ls_request-file        = ls_media_content.

    TRY.
        lo_attachment_ui->if_odata_cv_attachment_api~mass_upload_attachments(
          EXPORTING
            iv_objecttype      = lv_object_type                 " Linked SAP Object
            iv_objectkey       = lv_object_key                " Document management object key
            it_media_content   = VALUE #( ( ls_media_content ) )                " Media content details
            iv_auto_commit     = abap_true
          IMPORTING
            et_messages        = DATA(lt_messages)                 " Return parameter table
            et_failed_contents = DATA(lt_failed_contents)                 " Failed attachments - which cannot be loaded
        ).
      CATCH cx_odata_cv_base_exception. " ODATA CV base exception
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    mv_lgnum = iv_lgnum.

    mv_printer = iv_printer.

    mo_rest_client = NEW zcl_vce_rest_client( iv_url = mc_url ).

  ENDMETHOD.


  METHOD get_additional_services.
    DATA: lt_serv_tmp TYPE /scwm/tt_string_table.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zvce_0003                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_add_service                 " Parameter ID for process
      IMPORTING
        et_range     = DATA(lt_add_serv_r)                 " SELECT-OPTIONS Table
    ).

    LOOP AT it_ship_calc ASSIGNING FIELD-SYMBOL(<ls_ship_calc>).

      SPLIT <ls_ship_calc>-additionalservices AT zif_wme_c=>gs_gen-comma INTO TABLE lt_serv_tmp.

      LOOP AT lt_serv_tmp ASSIGNING FIELD-SYMBOL(<ls_serv_tmp>).

        CHECK <ls_serv_tmp> IS NOT INITIAL.

        IF <ls_serv_tmp> IN lt_add_serv_r.
          ev_get_ret_address = abap_true.
        ENDIF.

        CHECK NOT line_exists( rt_result[ <ls_serv_tmp> ] ).

        APPEND <ls_serv_tmp> TO rt_result.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_cover_address.
    "The cover adress will also be defaulted in the BP transaction with the sold-to address,
    "so in case the user in BP forgets to maintain the data we will at least have the sold-to address as cover address.

    DATA: ls_address TYPE addr1_val,
          lv_partyno TYPE /scdl/dl_partyno,
          name1      TYPE char70.

    DATA(lv_cust_no) = VALUE #( it_partyloc_h[ party_role = zif_wme_c=>gs_partyloc_c-sc_party_role_zcuspr ]-partyno OPTIONAL ).
    IF lv_cust_no IS INITIAL.
      lv_cust_no = VALUE #( it_partyloc_i[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).
    ENDIF.

    DATA(ls_customer_master) = zcl_vce_request_utils=>get_bp_master_data( iv_bp = lv_cust_no ).
    DATA(lv_use_def_dc_ra) = ls_customer_master-zz_use_dc_ra.

    IF lv_use_def_dc_ra EQ abap_true.

      IF iv_is_vind EQ abap_true.

        lv_partyno = VALUE #( it_partyloc_i[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL ).

      ELSEIF iv_is_vinv EQ abap_true.

        lv_partyno = VALUE #( it_partyloc_h[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt ]-partyno OPTIONAL ).

      ENDIF.

      ls_address = zcl_vce_request_utils=>get_bp(
        EXPORTING
          iv_bp = lv_partyno                 " Business Partner Number
      ).

      name1 = |{ ls_address-name1 } { COND string( WHEN iv_is_vind EQ abap_true
                                                             THEN TEXT-als ) }|.

    ELSE.

      "get sold-to address name
      DATA(ls_address_sotprt) = zcl_vce_request_utils=>get_bp(
        EXPORTING
          iv_bp = VALUE #( it_partyloc_i[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ]-partyno OPTIONAL )            " Business Partner Number
      ).

      name1 = |{ ls_address_sotprt-name1 } { TEXT-als }|.
      ls_address-street = ls_customer_master-zz_street_cv.
      ls_address-house_num1 = ls_customer_master-zz_house_num1_cv.
      ls_address-post_code1 = ls_customer_master-zz_post_code1_cv.
      ls_address-city1 = ls_customer_master-zz_city1_cv.
      ls_address-country = ls_customer_master-zz_country_cv.

    ENDIF.

    rs_result = VALUE #(
        external_id = ''
        name1     = name1
        name2     = ls_address-name2
        name3     = ls_address-name3
        name4     = ls_address-name4
        street    = ls_address-street
        house_no  = ls_address-house_num1
        post_code = ls_address-post_code1
        city      = ls_address-city1
        state     = ''
        country   = ls_address-country
*        phone     = ls_address-
        mail      = ''
        gln       = ''
        x_coord   = ''
        y_coord   = ''
        z_coord   = '' ).
  ENDMETHOD.


  METHOD get_packages.

    DATA: lv_price TYPE zde_commercial_value.

    NEW zcl_ship_pmat_rough_algorithm( )->pack_deliv_by_default_pmats(
      EXPORTING
        iv_lgnum       = mv_lgnum
        iv_doccat      = /scdl/if_dl_doc_c=>sc_doccat_out_prd
        it_docno       = VALUE #( ( docno = is_dlv_hdr-docno ) )
      IMPORTING
        et_pack_output = DATA(lt_rough_packing)
        et_bapiret     = DATA(lt_bapiret) ).

    IF lines( lt_rough_packing ) = 0.
      RETURN.
    ENDIF.

    " find number of cartons
    DATA(lv_num_cartons) = VALUE #( lt_rough_packing[ docno = is_dlv_hdr-docno ]-carton-total OPTIONAL ).

    " find number of pallets
    DATA(lv_num_pallet) = VALUE #( lt_rough_packing[ docno = is_dlv_hdr-docno ]-pallet-total OPTIONAL ).

    DATA(lv_prod_qty_hu) = REDUCE /scdl/dl_quantity( INIT l = 0
                                     FOR <hu_itm> IN it_huitm
                                     NEXT l = l + VALUE #( <hu_itm>-quan_t[ unit = is_dlv_itm-qty-uom ]-quan OPTIONAL ) ) .

    DATA(lv_prod_qty_itm) = is_dlv_itm-qty-qty.

    lv_price = is_dlv_itm-eew-zzcomm_value * CONV /scdl/dl_quantity( lv_prod_qty_hu / lv_prod_qty_itm ).

    rt_result = VALUE #(
          ( external_id = |{ is_huhdr-huident ALPHA = OUT }|
            reference   = ''
            reference2  = ''
            reference3  = ''
            length  = is_huhdr-max_length
            width   = is_huhdr-max_width
            height  = is_huhdr-max_height
            weight  = is_huhdr-g_weight
            content = 'ITC Goods'
            type    = zcl_crud_ztout_map_hutype=>select_by_lgnum_hutyp( iv_lgnum = mv_lgnum                 " Warehouse Number/Warehouse Complex
                                                                        iv_hutype = is_huhdr-letyp )                " Handling Unit Type
            cash_currency        = is_dlv_itm-eew-zzcomm_value_curr
            cash_value           = lv_price
            third_party_order_id = VALUE #( is_dlv_hdr-refdoc[ refdoccat = /scdl/if_dl_doc_c=>sc_doccat_out_poc ]-refdocno OPTIONAL )
            package_count        = ''
            package_count_total  = lv_num_cartons + lv_num_pallet
            parameters      = VALUE #( )
            package_details = it_packdetails ) ).
  ENDMETHOD.


  METHOD get_parameters.

    DATA: lt_huident TYPE /scdl/dl_db_huident_tab.

    IF iv_ex_tr_nr IS NOT INITIAL.
      "ONLY for OTTO Diva. Used as Hermes trackingnumber for Otto Diva label

      APPEND VALUE zif_vce_api_model=>ty_parameter( key = zif_wme_c=>gs_vce_params-ex_tr_nr
                                                   value = iv_ex_tr_nr ) TO rt_parameters.
    ELSE.
      "if the delivery is not an OTTO Diva delivery,
      "check for an SSCC number in case it is Amazon KEP

      DATA(lv_found) = zcl_crud_huident=>select_by_huid_huidtyp(
        EXPORTING
          it_huid_r     = VALUE rseloption( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = iv_hu_guid ) )
          it_huidtype_r = VALUE rseloption( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = zif_wme_c=>gs_huidart-s ) )                 " Handling Unit Identification Type
        IMPORTING
          et_huident    = lt_huident ).

      IF lv_found IS NOT INITIAL.

        APPEND VALUE zif_vce_api_model=>ty_parameter( key = zif_wme_c=>gs_vce_params-sscc
                                                      value = VALUE #( lt_huident[ 1 ]-huident OPTIONAL ) ) TO rt_parameters.

      ENDIF.

    ENDIF.

    IF iv_last_loading_unit EQ abap_true.

      APPEND VALUE zif_vce_api_model=>ty_parameter( key = zif_wme_c=>gs_vce_params-is_last_loading_unit
                                                    value = abap_true ) TO rt_parameters.

    ENDIF.

  ENDMETHOD.


  METHOD get_reciever_address.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************

    rs_result = VALUE #(
        external_id = iv_partyno
        name1       = is_address-name1
        name2       = is_address-name2
        name3       = is_address-name3
        name4       = is_address-name4
        street      = is_address-street
        house_no    = is_address-house_num1
        post_code   = is_address-post_code1
        city        = is_address-city1
        state       = is_address-region
        country     = is_address-country
        phone       = is_address-tel_number
        mail        = ''
        gln         = ''
        x_coord     = ''
        y_coord     = ''
        z_coord     = '' ).
  ENDMETHOD.


  METHOD get_return_address.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 4, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    DATA: ls_address_lgnum TYPE addr1_val,
          ls_address       TYPE addr1_val.

    DATA(lv_cust_no) = VALUE #( it_partyloc_h[ party_role = zif_wme_c=>gs_partyloc_c-sc_party_role_zcuspr ]-partyno OPTIONAL ).

    DATA(ls_customer_master) = zcl_vce_request_utils=>get_bp_master_data( iv_bp = lv_cust_no ).

    ls_address_lgnum = zcl_vce_request_utils=>get_bp( CONV #( mv_lgnum ) ).

    IF ls_customer_master-zz_street_re IS NOT INITIAL.

      "For the return address we cannot default ship_from in BP,
      "because the return address is warehouse dependent
      "and the the BP transaction is not.
      ls_address-name1 = ls_address_lgnum-name1.
      ls_address-street = ls_customer_master-zz_street_re.
      ls_address-house_num1 = ls_customer_master-zz_house_num1_re.
      ls_address-post_code1 = ls_customer_master-zz_post_code1_re.
      ls_address-city1 = ls_customer_master-zz_city1_re.
      ls_address-country = ls_customer_master-zz_country_re.

    ELSE.

      ls_address = CORRESPONDING #( ls_address_lgnum ).

    ENDIF.

    rs_result = VALUE #(
       external_id = ''
       name1     = ls_address-name1
       name2     = ls_address-name2
       name3     = ls_address-name3
       name4     = ls_address-name4
       street    = ls_address-street
       house_no  = ls_address-house_num1
       post_code = ls_address-post_code1
       city      = ls_address-city1
       state     = ''
       country   = ls_address-country
       mail      = ''
       gln       = ''
       x_coord   = ''
       y_coord   = ''
       z_coord   = '' ).

  ENDMETHOD.


  METHOD get_sender_address.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(ls_bp) = zcl_vce_request_utils=>get_bp( CONV char10( mv_lgnum ) ).

    rs_result = VALUE #(
        external_id = mv_lgnum
        name1     = ls_bp-name1
        name2     = ls_bp-name2
        name3     = ls_bp-name3
        name4     = ls_bp-name4
        street    = ls_bp-street
        house_no  = ls_bp-house_num1
        post_code = ls_bp-post_code1
        city      = ls_bp-city1
        state     = ls_bp-region
        country   = ls_bp-country
        phone     = ls_bp-tel_number
        mail      = ''
        gln       = ''
        x_coord   = ''
        y_coord   = ''
        z_coord   = '' ).
  ENDMETHOD.


  METHOD get_services.
    rt_result = VALUE #( FOR <s> IN it_ship_calc WHERE ( is_pref = abap_true )
                                               ( CONV #( <s>-servicetype ) ) ).
  ENDMETHOD.


  METHOD get_shipreq_packdetails.
    DATA(lt_danger_goods) =  zcl_vce_request_utils=>read_dangerous_goods( it_dlv_items ).

    DATA(lt_matnr_hmsign_map) = mara_read_multi( it_matnr = VALUE /scwm/tt_matnr_r( FOR <i> IN it_huitms
                                                 ( sign   = wmegc_sign_inclusive
                                                   option = wmegc_option_eq
                                                   low    = VALUE /scmb/mdl_product_no( it_dlv_items[ docid = <i>-qdocid
                                                                          itemid = <i>-qitmid ]-product-productno OPTIONAL ) ) ) ).

    DATA(lt_map_hmco) = zcl_crud_ztcross_map_hmco=>select_multi_by_key( it_hmsign_r = VALUE rseloption( FOR <hm> IN lt_matnr_hmsign_map
                                                                                                   ( sign   = wmegc_sign_inclusive
                                                                                                     option = wmegc_option_eq
                                                                                                     low    = <hm>-zz1_mhmsign01_prd ) ) ).

    rt_result = VALUE #( FOR <ls_huitm> IN it_huitms
                  ( line_number    = ''
                    article_number = ''
                    description    = <ls_huitm>-maktx
                    po_line_number = ''
                    length         = ''
                    width          = ''
                    height         = ''
                    weight         = <ls_huitm>-weight
                    quantity       = <ls_huitm>-quan
                    parameters     = VALUE #( )
                    customs_data    = VALUE #(
                      value       = is_dlv_head-eew-zzcomm_hdr_value
                      description = ''
                      currency    = is_dlv_head-eew-zzcomm_hdr_value_curr
                      code        = ''
                      quantity    = <ls_huitm>-quan
                      country_of_origin = VALUE #( it_dlv_items[ itemid = <ls_huitm>-qitmid ]-eew-zzherkl OPTIONAL ) )
                    dangerous_goods = VALUE #(
                      classification = VALUE #( lt_danger_goods[ matnr = VALUE #( it_dlv_items[ itemid = <ls_huitm>-qitmid ]-product-productno OPTIONAL ) ]-dgcl OPTIONAL )
                      number         = VALUE #( lt_danger_goods[ matnr = VALUE #( it_dlv_items[ itemid = <ls_huitm>-qitmid ]-product-productno OPTIONAL ) ]-dgnu OPTIONAL )
                      description    = VALUE #( lt_danger_goods[ matnr = VALUE #( it_dlv_items[ itemid = <ls_huitm>-qitmid ]-product-productno OPTIONAL ) ]-pdgnud OPTIONAL )
                      uom_value   = <ls_huitm>-weight
                      uom         = <ls_huitm>-unit_w
                      packages_quantity = <ls_huitm>-quan
                      package_type      = ''
                      package_group     = VALUE #( lt_map_hmco[ sign = VALUE #( lt_matnr_hmsign_map[ scm_matid_guid16 = <ls_huitm>-matid ]-zz1_mhmsign01_prd OPTIONAL ) ]-package_grp OPTIONAL )
                      secondary_danger  = VALUE #( lt_danger_goods[ matnr = VALUE #( it_dlv_items[ itemid = <ls_huitm>-qitmid ]-product-productno OPTIONAL ) ]-dgsc OPTIONAL )
                      nos_text          = ''
                      tunnel_code       = VALUE #( lt_map_hmco[ sign = VALUE #( lt_matnr_hmsign_map[ scm_matid_guid16 = <ls_huitm>-matid ]-zz1_mhmsign01_prd OPTIONAL ) ]-tunnelcode OPTIONAL )
                      transport_category = VALUE #( lt_danger_goods[ matnr = VALUE #( it_dlv_items[ itemid = <ls_huitm>-qitmid ]-product-productno OPTIONAL ) ]-mot OPTIONAL )
                      factor = ''
                      points = ''
                      notice = VALUE #( lt_map_hmco[ sign = VALUE #( lt_matnr_hmsign_map[ scm_matid_guid16 = <ls_huitm>-matid ]-zz1_mhmsign01_prd OPTIONAL ) ]-notice OPTIONAL )
                      type   = ''
                      regulation_set = VALUE #( lt_danger_goods[ matnr = it_dlv_items[ itemid = <ls_huitm>-qitmid ]-product-productno ]-lwdg OPTIONAL )
                      transport_mode = '' )
                  ) ).
  ENDMETHOD.


  METHOD mara_read_multi.
********************************************************************
*& Key          : <AAHMEDOV>-Feb 5, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  : read hazardous material sign
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    IF it_matnr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT scm_matid_guid16, zz1_mhmsign01_prd
      FROM mara
      WHERE matnr IN @it_matnr
      INTO CORRESPONDING FIELDS OF TABLE @rt_mara.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'mara' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD post_req_mockdata.
*    DATA: lt_request TYPE zif_vce_ship_request_model=>tt_ship_request.
*
*    lt_request = VALUE #( (
*      ship_request = VALUE #(
*        user = ''
*        reference       = ''
*        reference2     = ''
*        reference3     = ''
*        reference4     = ''
*        incoterms       = ''
*        comment         = ''
*        tpls            = VALUE #( ( CONV #( 'DHL' ) ) )
*        services        = VALUE #( ( CONV #( 'STANDARD') ) )
*        additional_services = VALUE #( ( CONV #( 'SATURDAY_DELIVERY' ) ) )
*        deliver_advice  = 'Please deliver to garage'
*        sender = VALUE #(
*          external_id = ''
*          name1       = ''
*          name2       = ''
*          name3       = ''
*          name4       = ''
*          street      = ''
*          house_no  = ''
*          post_code = ''
*          city      = ''
*          state     = ''
*          country   = ''
*          mail      = ''
*          phone     = ''
*          gln       = ''
*          x_coord   = ''
*          y_coord   = ''
*          z_coord   = '' )
*        receiver = VALUE #(
*          external_id = ''
*          name1       = ''
*          name2       = ''
*          name3       = ''
*          name4       = ''
*          street      = ''
*          house_no  = ''
*          post_code = ''
*          city      = ''
*          state     = ''
*          country   = ''
*          mail      = ''
*          phone     = ''
*          gln       = ''
*          x_coord   = ''
*          y_coord   = ''
*          z_coord   = '' )
*        cover_address = VALUE #(
*          external_id = ''
*          name1       = ''
*          name2       = ''
*          name3       = ''
*          name4       = ''
*          street      = ''
*          house_no  = ''
*          post_code = ''
*          city      = ''
*          state     = ''
*          country   = ''
*          mail      = ''
*          phone     = ''
*          gln       = ''
*          x_coord   = ''
*          y_coord   = ''
*          z_coord   = '' )
*        additional_address = VALUE #(
*          external_id = ''
*          name1       = ''
*          name2       = ''
*          name3       = ''
*          name4       = ''
*          street      = ''
*          house_no  = ''
*          post_code = ''
*          city      = ''
*          state     = ''
*          country   = ''
*          mail      = ''
*          phone     = ''
*          gln       = ''
*          x_coord   = ''
*          y_coord   = ''
*          z_coord   = '' )
*        parameters = VALUE #( ( key = 'externalGroupingKey'
*                                value = '6064126888' ) )
*        packages = VALUE #(
*          ( external_id = ''
*            reference   = ''
*            reference2  = ''
*            reference3  = ''
*            length  = ''
*            width   = ''
*            height  = ''
*            weight  = ''
*            content = ''
*            type    = ''
*            cash_currency        = ''
*            cash_value           = ''
*            third_party_order_id = ''
*            package_count        = ''
*            package_count_total  = ''
*            parameters      = VALUE #( ( key = 'externalGroupingKey'
*                                         value = '6064126888' ) )
*            package_details = VALUE #(
*              ( line_number    = ''
*                article_number = ''
*                description    = ''
*                po_line_number = ''
*                length         = ''
*                width          = ''
*                height         = ''
*                weight         = ''
*                quantity       = ''
*                parameters     = VALUE #( ( key = 'externalGroupingKey'
*                                            value = '6064126888' ) )
*                customs_data    = VALUE #(
*                  value       = '7'
*                  description = 'cust 7'
*                  currency    = 'EUR'
*                  code        = ''
*                  quantity    = '12423'
*                  country_of_origin = 'DE' )
*                dangerous_goods = VALUE #(
*                  classification = ''
*                  number      = ''
*                  description = ''
*                  uom_value   = ''
*                  uom         = ''
*                  packages_quantity = ''
*                  package_type      = ''
*                  package_group     = ''
*                  secondary_danger  = ''
*                  nos_text          = ''
*                  tunnel_code       = ''
*                  transport_category = ''
*                  factor = ''
*                  points = ''
*                  notice = ''
*                  type   = ''
*                  regulation_set = ''
*                  transport_mode = '' )
*              ) )
*         ) ) ) ) ).
*
*    rv_json = /ui2/cl_json=>serialize( data = lt_request
*                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
  ENDMETHOD.


  METHOD print_file.
********************************************************************
*& Key          : <BSUGAREV>-Nov 21, 2023
*& Request No.  :
********************************************************************
*& Description  : Print PDF form supplied by VCE
*&
*&
********************************************************************
*    DATA lv_name          TYPE string  .
*    DATA lv_bin_filesize  TYPE i.
*    DATA lv_buffer        TYPE xstring .
*    DATA lv_msg           TYPE string.
*    DATA lv_spoolid       TYPE rspoid.
*    DATA lt_stream        TYPE etxml_xline_tabtype.
*    DATA lv_handle        TYPE sytabix.
*    DATA lv_error         TYPE boole_d.
*    DATA: lv_terminal TYPE /scwm/de_wc_terminal.
*    DATA: lv_lifetime TYPE c VALUE 8,
*          lv_size     TYPE i.
*
*    CLEAR: lv_msg, lv_buffer, lv_name, lv_bin_filesize, lt_stream,
*           lv_handle, lv_spoolid.
*
*    CLEAR: lv_msg.
*
*    DATA(lv_pdf_data) = CONV fpcontent( is_response-packages[ 1 ]-zpl_label_data ).
*
*    lv_size = xstrlen( lv_pdf_data ).
*
*    CALL FUNCTION 'TH_USER_INFO'
*      EXPORTING
*        client   = sy-mandt
*        user     = sy-uname
*      IMPORTING
*        terminal = lv_terminal.
*
*    IF lv_terminal IS NOT INITIAL.
*      DATA(ls_print_dev) = zcl_crud_zout_term_def=>select_single_by_key( lv_terminal ).
*    ENDIF.
*
*    IF ls_print_dev IS INITIAL.
*      MESSAGE e004(zmc_odata) WITH lv_terminal 'zout_term_def' INTO lv_msg.
**      RAISE EXCEPTION new zcx_generalexception( ).
*    ENDIF.
*
*    attach_hu_label( iv_pdf_label_data = lv_pdf_data ).
*
*    CALL FUNCTION 'ADS_CREATE_PDF_SPOOLJOB'
*      EXPORTING
*        dest              = ls_print_dev-printer
*        pages             = 1
*        pdf_data          = lv_pdf_data
*        name              = 'PBFORM'
*        suffix1           = ''
*        suffix2           = 'BSUGAREV'
*        copies            = 1
**       PRIO              = ps_op-
*        immediate_print   = abap_true
*        auto_delete       = abap_false
*        titleline         = 'Test Lable Print from report'
**       receiver          = gs_outputparams-receiver
**       division          = gs_outputparams-division
**       authority         = gs_outputparams-authority
*        lifetime          = lv_lifetime
*      IMPORTING
*        spoolid           = lv_spoolid
*      EXCEPTIONS
*        no_data           = 1
*        not_pdf           = 2
*        wrong_devtype     = 3
*        operation_failed  = 4
*        cannot_write_file = 5
*        device_missing    = 6
*        no_such_device    = 7
*        OTHERS            = 8.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
  ENDMETHOD.


  METHOD send_request.

  ENDMETHOD.


  METHOD update_tables.

    IF lines( is_response-parameters ) < 1.
      RETURN.
    ENDIF.

*    IF line_exists( is_response-parameters[ key = zif_wme_c=>gs_vce_params-sscc ] ).
*
*      "set SSCC to used in ztout_dlv_sscc table
*      zcl_crud_ztout_dlv_sscc=>update(
*        EXPORTING
*          iv_lgnum    = mv_lgnum                 " Warehouse Number/Warehouse Complex
*          iv_docno    = VALUE #( is_response-packages[ 1 ]-order_no OPTIONAL )                " Document Number
*          iv_dlv_sscc = CONV #( is_response-parameters[ key = zif_wme_c=>gs_vce_params-sscc ]-value )               " SSCC Number
*      ).
*    ENDIF.


    IF line_exists( is_response-parameters[ key = zif_wme_c=>gs_vce_params-tr_nr ] ).
    "we should always recieve a tracking number in the response
    " Save tracking number in field “trackingNo” in a tracking number ID-type of the EWM HU
      zcl_crud_huident=>insert(
        EXPORTING
          it_huident = VALUE #( ( docid = ms_dlv_header-docid
                                  huid  = ms_huhdr-guid_hu
                                  huidtype = zif_wme_c=>gs_huidart-v
                                  huident = VALUE #( is_response-parameters[ key = zif_wme_c=>gs_vce_params-tr_nr ] OPTIONAL )
                                  counter = 1 ) ) ).

*      zcl_crud_ztout_otto_diva=>update( it_otto_diva = VALUE #( ( lgnum = mv_lgnum
*                                                                  docno = ms_dlv_header-docno
*                                                                  ext_tracking_nr = VALUE #( is_response-parameters[ key = zif_wme_c=>gs_vce_params-tr_nr ]-value OPTIONAL  )
*                                                                  itemno       = ms_dlv_item-itemno  ) ) ).

    ENDIF.

    IF line_exists( is_response-parameters[ key = zif_wme_c=>gs_vce_params-sscc ] ).
      "SSCC number can be received in the response and not be sent in the request, so we save it
      " Save SSCC in field “ExternalID” in a SSCC ID-type of the EWM HU.
      zcl_crud_huident=>insert(
        EXPORTING
          it_huident = VALUE #( ( docid = ms_dlv_header-docid
                                  huid  = ms_huhdr-guid_hu
                                  huidtype = zif_wme_c=>gs_huidart-s
                                  huident = ms_huhdr-huident
                                  counter = 1 ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_vce_api_client~build_request_data.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  :
********************************************************************
*& Description  : Build data for the JSON file
*&
*&
********************************************************************
    DATA: lt_huhdr           TYPE /scwm/tt_huhdr_int,
          lt_huitm           TYPE	/scwm/tt_huitm_int,
          lv_use_def_dc_ra   TYPE xfeld,
          lv_get_ret_address TYPE xfeld.

    DATA(lv_huident) = CONV /scwm/de_huident( iv_input ).
    lv_huident = |{ lv_huident ALPHA = IN }|.

    " select HU items
    CALL FUNCTION '/SCWM/HU_SELECT'
      EXPORTING
        it_huident = VALUE /scwm/tt_huident( ( lgnum = mv_lgnum
                                               huident = lv_huident ) )
      IMPORTING
        et_huhdr   = lt_huhdr
        et_huitm   = lt_huitm
      EXCEPTIONS
        not_found  = 1
        error      = 2
        OTHERS     = 3.
    IF sy-subrc <> 0 OR lines( lt_huitm ) = 0.
      RETURN.
    ENDIF.

    DATA(ls_huhdr) = lt_huhdr[ top = abap_true ].
    ms_huhdr = ls_huhdr.

    " use docids from HU items to select documents
    TRY.
        NEW /scwm/cl_dlv_management_prd( )->query(
          EXPORTING
            it_docid        = VALUE #( FOR <ls_huitm> IN lt_huitm
                                        ( docid	= <ls_huitm>-qdocid
                                          itemid = <ls_huitm>-qitmid
                                          doccat = <ls_huitm>-qdoccat ) )
            iv_whno         = mv_lgnum
            is_read_options = VALUE #( data_retrival_only = abap_true mix_in_object_instances = abap_true )
            is_include_data = VALUE #( head_date = abap_true
                                       head_partyloc = abap_true
                                       item_partyloc = abap_true
                                       head_refdoc = abap_true )
          IMPORTING
            et_headers      = DATA(lt_dlv_headers)
            et_items        = DATA(lt_dlv_items) ).
      CATCH /scdl/cx_delivery.
    ENDTRY.

    DATA(ls_dlv_hdr) = VALUE #( lt_dlv_headers[ 1 ] OPTIONAL ).
    ms_dlv_header = ls_dlv_hdr.
    DATA(ls_dlv_itm) = VALUE #( lt_dlv_items[ 1 ] OPTIONAL ).

    IF ls_dlv_hdr IS INITIAL.
      RETURN.
    ENDIF.

    zcl_crud_ztout_ship_calc=>select_by_docno(
      EXPORTING
        it_docno_r   = VALUE #( ( sign = wmegc_sign_inclusive
                                  option = wmegc_option_eq
                                  low = ls_dlv_hdr-docno ) )
      IMPORTING
        et_ship_calc = DATA(lt_ship_calc) ).

    DATA(ls_ship_calc) = VALUE #( lt_ship_calc[ 1 ] OPTIONAL ).
    IF ls_ship_calc IS INITIAL.
      RETURN.
    ENDIF.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zvce_0001                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_vind                 " Parameter ID for process
      IMPORTING
        et_range     = DATA(lt_vind_r)                " SELECT-OPTIONS Table
    ).

    DATA(lv_is_vind) = COND #( WHEN ls_dlv_hdr-eew-zzaugru IN lt_vind_r THEN abap_true ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zvce_0001                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_vinv                 " Parameter ID for process
      IMPORTING
        et_range     = DATA(lt_vinv_r)                " SELECT-OPTIONS Table
    ).

    DATA(lv_is_vinv) = COND #( WHEN ls_dlv_hdr-eew-zzaugru IN lt_vinv_r THEN abap_true ).

    TRY.
        DATA(lv_stprt) = ls_dlv_hdr-partyloc[ party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt ]-partyno.
        DATA(ls_reciever_address) = zcl_vce_request_utils=>get_bp( lv_stprt ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    IF ( ls_reciever_address IS NOT INITIAL
     AND ls_reciever_address-country <> zif_wme_c=>gs_gen-ger ).
      DATA(lt_shipreq_packdetails) = get_shipreq_packdetails(
        is_dlv_head  = ls_dlv_hdr
        it_dlv_items = lt_dlv_items
        it_huitms    = lt_huitm ).
    ENDIF.

    DATA(lt_request_data) = VALUE tt_request(
          ( user         = ls_ship_calc-carma_user_code
            reference    = |{ ls_ship_calc-docno ALPHA = OUT }|
            reference2   = ''
            reference3   = ''
            reference4   = ''
            incoterms    = ls_dlv_hdr-incoterms-inco1
            comment      = ''
            services            = get_services( lt_ship_calc )
            additional_services = get_additional_services( EXPORTING it_ship_calc = lt_ship_calc
                                                           IMPORTING ev_get_ret_address = lv_get_ret_address )
            deliver_advice = 'Please deliver to garage'
            sender         = get_sender_address( )
            receiver       = get_reciever_address( iv_partyno = lv_stprt
                                                   is_address = ls_reciever_address )
            cover_address  = get_cover_address( it_partyloc_h = ls_dlv_hdr-partyloc
                                                it_partyloc_i = ls_dlv_itm-partyloc
                                                iv_is_vind = lv_is_vind
                                                iv_is_vinv = lv_is_vinv )
           	return_address =  COND #( WHEN lv_get_ret_address EQ abap_true
                                      THEN get_return_address( it_partyloc_h = ls_dlv_hdr-partyloc
                                                               it_partyloc_i = ls_dlv_itm-partyloc ) )
            additional_address = VALUE #( )
            parameters = get_parameters( EXPORTING iv_docno = ls_ship_calc-docno
                                                   iv_ex_tr_nr = ls_dlv_itm-eew-zztp_exidv
                                                   iv_hu_guid = ms_huhdr-guid_hu
                                                   iv_last_loading_unit =
                                                       COND xfeld( WHEN line_exists( ls_dlv_hdr-status[ status_type = /scdl/if_dl_status_c=>sc_t_pack_shpm
                                                                                                        status_value = /scdl/if_dl_status_c=>sc_v_finished ] ) THEN abap_true  ) )
            packages   = get_packages( is_huhdr = ls_huhdr
                                       is_dlv_hdr = ls_dlv_hdr
                                       it_packdetails = lt_shipreq_packdetails
                                       it_huitm = lt_huitm
                                       is_dlv_itm = ls_dlv_itm ) ) ).
    es_data = lt_request_data.

  ENDMETHOD.


  METHOD zif_vce_api_client~get_response.
    DATA ls_response TYPE ty_response.

    DATA(lv_json) = iv_json.

    IF lv_json IS INITIAL.
      lv_json = mo_rest_client->get_api_response( )->get_string_data( ).

      ev_status = mo_rest_client->get_status( ).
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                CHANGING data = ls_response ).
    es_data   = ls_response.

*    IF mo_log IS NOT BOUND.
*      mo_log = NEW zcl_vce_log( iv_extnumber = 'SHIP_CALCULATION'
*                                iv_altcode   = 'RESPONSE' ).
*    ENDIF.
*
*    mo_log->add_parameters( ls_response ).
*
*    mo_log->save_log( ).
  ENDMETHOD.


  METHOD zif_vce_api_client~process_response.
    DATA: ls_response TYPE ty_response.

    get_response(
      EXPORTING
        iv_json   = iv_json
      IMPORTING
        ev_status = DATA(lv_status)
        es_data   = ls_response ).

    IF ls_response-error <> abap_true.

      DATA(ls_package) = VALUE #( ls_response-packages[ 1 ] OPTIONAL ).

      TRY.
          DATA(ls_output_param) = VALUE sfpoutputparams(
              dest     = mv_printer
              dataset  = 'PBFORM'
              suffix2  = sy-uname
              covtitle = ls_package-external_id
              copies   = 1
              reqimm   = abap_true
              reqdel   = abap_false
              lifetime = 8 ).

          zcl_vce_request_utils=>print_pdf_file( iv_pdf_data     = ls_package-zpl_label_data
                                                 is_outputparams = ls_output_param ).

          zcl_vce_request_utils=>attach_label_to_doc(
            iv_file_name = CONV #( |{ /scwm/if_api_attachments=>gc_obj_type_hu }{ ms_huhdr-huident }.pdf| )
            iv_objkey    = CONV #( ms_huhdr-guid_hu )
            iv_objtyp    = /scwm/if_api_attachments=>gc_obj_type_hu
            iv_pdf_data  = ls_package-zpl_label_data
            iv_commit    = abap_true ).

*          attach_hu_label( lv_pdf_data ).

        CATCH zcx_core_exception.
      ENDTRY.

      update_tables( ls_response ).

    ENDIF.

    DATA(lv_extnumb) = VALUE balnrext( ).

    lv_extnumb = |{ zif_vce_api_model=>gs_msg_extid_key-shipreq }-{ ms_huhdr-huident }|.
    DATA(lo_msg) = NEW zcl_vce_log( iv_extnumber = lv_extnumb
                                    iv_altcode   = zif_vce_api_model=>gs_msg_altcode-receive ).

    CLEAR: ls_response-packages, ls_response-parameters.
    lo_msg->add_parameters( ls_response ).

    lo_msg->save_log( abap_true ).
  ENDMETHOD.


  METHOD zif_vce_api_client~send_get_request.

  ENDMETHOD.


  METHOD zif_vce_api_client~send_post_request.
********************************************************************
*& Key          : <BSUGAREV>-Nov 17, 2023
*& Request No.  :
********************************************************************
*& Description  : Send request to 3th party
*&
*&
********************************************************************
    DATA: lt_request TYPE tt_request.

    lt_request = is_data.

    TRY.
        DATA(lv_json) = /ui2/cl_json=>serialize( data        = lt_request
                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        mo_rest_client->post( lv_json ).
      CATCH cx_rest_client_exception INTO DATA(lo_error).
    ENDTRY.

    DATA(lv_extnumb) = VALUE balnrext( ).
    lv_extnumb = |{ zif_vce_api_model=>gs_msg_extid_key-shipreq }-{ ms_huhdr-huident }|.
    DATA(lo_msg) = NEW zcl_vce_log( iv_extnumber = lv_extnumb
                                    iv_altcode   = zif_vce_api_model=>gs_msg_altcode-send ).

    lo_msg->add_parameters( lt_request ).

    lo_msg->save_log( abap_true ).
  ENDMETHOD.
ENDCLASS.
