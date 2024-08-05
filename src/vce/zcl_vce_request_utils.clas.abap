class ZCL_VCE_REQUEST_UTILS definition
  public
  final
  create public .

public section.

  class-methods GET_BP_SHIP_TO
    importing
      !IS_DLV_HEAD type /SCWM/DLV_HEADER_OUT_PRD_STR
    returning
      value(RS_RESULT) type ZIF_VCE_SHIPMENT_CALC_MODEL=>TY_BP .
  class-methods ATTACH_LABEL_TO_DOC
    importing
      !IV_FILE_NAME type FILEP
      !IV_OBJKEY type OBJKY
      !IV_OBJTYP type DOKOB
      !IV_PDF_DATA type FPCONTENT
      !IV_COMMIT type BOOLE_D default ABAP_FALSE .
  class-methods CHECK_HU_HAS_TRACKING_NUM
    importing
      !IT_HUID_R type RSELOPTION optional
      !IT_DOCID_R type RSELOPTION optional
      !IV_HUIDTYPE type /SCDL/DL_HUIDTYPE
    returning
      value(RV_FOUND) type XFELD .
  class-methods GET_BP
    importing
      !IV_BP type BU_PARTNER optional
    returning
      value(RS_ADDRESS) type ADDR1_VAL .
  class-methods GET_BP_MASTER_DATA
    importing
      !IV_BP type BU_PARTNER
    returning
      value(RS_BUT000) type BUT000 .
  class-methods PRINT_PDF_FILE
    importing
      !IV_PDF_DATA type FPCONTENT
      !IS_OUTPUTPARAMS type SFPOUTPUTPARAMS
    raising
      ZCX_CORE_EXCEPTION .
  class-methods READ_DANGEROUS_GOODS
    importing
      !IT_DLV_ITEMS type /SCWM/DLV_ITEM_OUT_PRD_TAB
    returning
      value(RT_RDGMDIOT) type EHS_RDGMDIOT_T .
  class-methods GET_BP_SHIP_FROM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RS_RESULT) type ZIF_VCE_SHIPMENT_CALC_MODEL=>TY_BP .
  class-methods GET_BP_SOLD_TO
    importing
      !IS_DLV_ITEM type /SCWM/DLV_ITEM_OUT_PRD_STR
    returning
      value(RS_RESULT) type ZIF_VCE_SHIPMENT_CALC_MODEL=>TY_BP .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_instance TYPE REF TO zcl_vce_request_utils .
ENDCLASS.



CLASS ZCL_VCE_REQUEST_UTILS IMPLEMENTATION.


  METHOD attach_label_to_doc.
********************************************************************
*& Key          : AAHMEDOV-Dec 1, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  : Attach
********************************************************************
    CONSTANTS:
      lc_mime_type     TYPE skwf_mime VALUE 'application/pdf'.

    DATA: lv_file_name     TYPE string,
          lv_file_size     TYPE i,
          ls_media_content TYPE if_odata_cv_attachment_api=>ty_s_media_resource,
          lv_xstring       TYPE  xstring.

    DATA(lo_attachment_ui) = cl_odata_cv_attachment_api=>get_instance( ).

    DATA(lt_med_content) = VALUE if_odata_cv_attachment_api=>ty_t_media_resource(
      ( filename  = iv_file_name
        value     = CONV xstring( iv_pdf_data )
        mime_type = lc_mime_type ) ).

    TRY.
        lo_attachment_ui->if_odata_cv_attachment_api~mass_upload_attachments(
          EXPORTING
            iv_objecttype      = iv_objtyp
            iv_objectkey       = iv_objkey
            it_media_content   = lt_med_content
            iv_auto_commit     = iv_commit
          IMPORTING
            et_messages        = DATA(lt_messages)
            et_failed_contents = DATA(lt_failed_contents) ).
      CATCH cx_odata_cv_base_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD check_hu_has_tracking_num.
********************************************************************
*& Key          : <AHMEDOV>-Oct 30, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************

    IF it_huid_r IS SUPPLIED.
      rv_found = zcl_crud_huident=>select_by_huid_huidtyp(
        EXPORTING
          it_huid_r     = it_huid_r               " Warehouse Number/Warehouse Complex
          it_huidtype_r = VALUE rseloption( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = iv_huidtype ) ) ).                " Handling Unit Identification Type

    ELSEIF it_docid_r IS SUPPLIED.
      rv_found = zcl_crud_huident=>select_by_docid_huidtyp(
        EXPORTING
          it_docid_r    = it_docid_r                 " SELECT-OPTIONS Table
          it_huidtype_r = VALUE rseloption( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = iv_huidtype ) ) ).               " Handling Unit Identification Type
    ENDIF.

  ENDMETHOD.


  METHOD get_bp.
    DATA: ls_address_num TYPE addr1_sel,
          lv_address_no  TYPE but020-addrnumber,
          lv_bp_no       TYPE bu_partner,
          lv_lgnum       TYPE /scwm/lgnum,
          ls_t300_md     TYPE /scwm/s_t300_md.

    CALL FUNCTION 'BUA_ADDRESS_GET'
      EXPORTING
        i_partner        = iv_bp
      IMPORTING
        e_addrnumber     = lv_address_no
      EXCEPTIONS
        no_address_found = 1
        internal_error   = 2
        wrong_parameters = 3
        date_invalid     = 4
        not_valid        = 5
        partner_blocked  = 6
        OTHERS           = 7.
    IF sy-subrc = 0.
      ls_address_num-addrnumber = lv_address_no.
    ENDIF.

    " BP not found, check if warehouse number is provided
    IF lv_address_no IS INITIAL.
      lv_lgnum = iv_bp.

      CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
        EXPORTING
          iv_lgnum   = lv_lgnum
        IMPORTING
          es_t300_md = ls_t300_md
        EXCEPTIONS
          not_found  = 1
          OTHERS     = 2.
      IF sy-subrc = 0 AND ls_t300_md IS NOT INITIAL.

        SELECT * FROM /scmb/toentity
          INTO TABLE @DATA(lt_toentity)
          WHERE scuguid = @ls_t300_md-scuguid.

        ASSIGN lt_toentity[ 1 ] TO FIELD-SYMBOL(<ls_toentity>).
        IF <ls_toentity> IS ASSIGNED.
          ls_address_num-addrnumber = <ls_toentity>-adresse.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_address_num IS NOT INITIAL.
      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          address_selection = ls_address_num
        IMPORTING
          address_value     = rs_address
        EXCEPTIONS
          parameter_error   = 01
          address_not_exist = 02
          version_not_exist = 03
          internal_error    = 04
          OTHERS            = 99.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_bp_master_data.
********************************************************************
*& Key          : <AHMEDOV>-Oct 30, 2023
*& Request No.  : GAP-012 - Outbound_VCE_carrier_software_integration
********************************************************************
*& Description
********************************************************************
    DATA: ls_but000 TYPE but000.

    CALL FUNCTION 'BKK_BUPA_PARTNER_DATA_GET'
      EXPORTING
        i_partner = iv_bp
      IMPORTING
        e_but000  = rs_but000.

  ENDMETHOD.


  METHOD get_bp_ship_from.
********************************************************************
*& Key          : AAHMEDOV-Dec 4, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
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


  METHOD get_bp_ship_to.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 4, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
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


  METHOD get_bp_sold_to.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 4, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
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


  METHOD print_pdf_file.
********************************************************************
*& Key          : <BSUGAREV>-Dec 1, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_msg     TYPE string,
          lv_spoolid TYPE rspoid.

    CALL FUNCTION 'ADS_CREATE_PDF_SPOOLJOB'
      EXPORTING
        dest              = is_outputparams-dest
        pages             = 1
        pdf_data          = iv_pdf_data
        name              = is_outputparams-dataset
        suffix1           = is_outputparams-suffix1
        suffix2           = is_outputparams-suffix2
        copies            = CONV rspocopies( is_outputparams-copies )
        immediate_print   = is_outputparams-reqimm
        auto_delete       = is_outputparams-reqdel
        titleline         = is_outputparams-covtitle
        receiver          = is_outputparams-receiver
        division          = is_outputparams-division
        authority         = is_outputparams-authority
        lifetime          = CONV char01( is_outputparams-lifetime )
      IMPORTING
        spoolid           = lv_spoolid
      EXCEPTIONS
        no_data           = 1
        not_pdf           = 2
        wrong_devtype     = 3
        operation_failed  = 4
        cannot_write_file = 5
        device_missing    = 6
        no_such_device    = 7
        OTHERS            = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
      RAISE EXCEPTION NEW ZCX_CORE_EXCEPTION( ).
    ENDIF.
  ENDMETHOD.


  METHOD: read_dangerous_goods.
    DATA: lt_selection_tab       TYPE TABLE OF rdgmdrng,
          lv_flg_changenum_coll  TYPE rcgrhbuf-chcoflg,
          lv_flg_lockfail        TYPE rcgrhbuf-valflg,
          ls_further_settings_md TYPE esp5_settings_type.

    lt_selection_tab = VALUE #(  sign   = wmegc_sign_inclusive
                                 ( option = wmegc_option_cp fieldname = zif_c_mdm_tool=>c_fieldnames-rvlid low  = zif_c_mdm_tool=>c_hazardous-validity_area )
                                 ( option = wmegc_option_cp fieldname = zif_c_mdm_tool=>c_fieldnames-lwdg  low  = zif_c_mdm_tool=>c_hazardous-validity_area )
                                 ( option = wmegc_option_ge fieldname = zif_c_mdm_tool=>c_fieldnames-valto low  = sy-datum )
                                 ( option = wmegc_option_le fieldname = zif_c_mdm_tool=>c_fieldnames-valfr low  = zif_c_mdm_tool=>c_hazardous-end_of_the_calendar ) ).

    lt_selection_tab = VALUE #( BASE lt_selection_tab FOR <l> IN it_dlv_items
                                ( sign = wmegc_sign_inclusive
                                  option = wmegc_option_eq
                                  fieldname = zif_c_mdm_tool=>c_fieldnames-matnr
                                  low = <l>-product-productno ) ).

    ls_further_settings_md = VALUE #( enq_deq_function_name     = zif_c_mdm_tool=>c_hazardous-enq_dec_logic
                                      select_function_name      = zif_c_mdm_tool=>c_hazardous-select_master
                                      flg_return_whole_interval = abap_true
                                      valfr                     = zif_c_mdm_tool=>c_hazardous-start_of_the_calendar
                                      valto                     = zif_c_mdm_tool=>c_hazardous-end_of_the_calendar
                                      flg_no_ch_doc_writing     = abap_true ).

*-------------------------------------------------------
* STEP: Read master data / fill IOTAB
*-------------------------------------------------------
    CALL FUNCTION 'DG10N_DGTMD_LOAD_BUF_AND_IOT'
      EXPORTING
        i_aennr            = VALUE eseaennr( ) "Change number
        i_valdat           = sy-datum
        i_do_locking       = VALUE eseboole( )
        i_further_settings = ls_further_settings_md
      IMPORTING
        e_flg_lockfail     = lv_flg_changenum_coll
        e_flg_chacoll      = lv_flg_lockfail
      TABLES
        e_iotab            = rt_rdgmdiot
        i_selection_tab    = lt_selection_tab
      EXCEPTIONS
        buffer_error       = 1
        no_record_found    = 2
        OTHERS             = 3.

* (EHS 3.2; RK.) We also sort the iotab according to materials and LWDG
* because that looks better and things are easier to find.
    SORT rt_rdgmdiot BY matnr lwdg valfr.
  ENDMETHOD.
ENDCLASS.
