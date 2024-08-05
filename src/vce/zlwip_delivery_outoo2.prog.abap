*&---------------------------------------------------------------------*
*& Include          /SCWM/LWIP_DELIVERY_OUTOO2
*&---------------------------------------------------------------------*

CLASS lcx_constructor_error DEFINITION INHERITING FROM cx_no_check. ENDCLASS.
CLASS lcx_bom_error         DEFINITION INHERITING FROM cx_no_check. ENDCLASS.

CLASS lcl_helper DEFINITION
    FINAL
    CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_lgnum TYPE /scwm/lgnum,

      assign_carrier IMPORTING it_data TYPE STANDARD TABLE,

      main
        IMPORTING it_dlv_fields TYPE ztt_vce_dlv_upd_fields.

  PRIVATE SECTION.

    CONSTANTS:
      mc_high TYPE i VALUE 3,
      mc_imp  TYPE i VALUE 2,
      mc_med  TYPE i VALUE 1.

    DATA:
      mo_attr_handler TYPE REF TO /scwm/cl_dlv_ah_adapter,
      mo_message_box  TYPE REF TO /scdl/cl_sp_message_box,
      mo_prd          TYPE REF TO /scwm/cl_dlv_management_prd,
      mo_log          TYPE REF TO /scwm/cl_dlv_log_mon_gen,
      mo_sp           TYPE REF TO /scdl/cl_sp_prd_out,
      mo_bom          TYPE REF TO /scdl/cl_bo_management,
      mv_lgnum        TYPE /scwm/lgnum.

    METHODS:

      get_docid_for_selected_records
        IMPORTING
          it_data         TYPE STANDARD TABLE
        RETURNING
          VALUE(rt_docid) TYPE /scwm/dlv_docid_item_tab,

      get_docno_for_selected_record
        IMPORTING
          it_data           TYPE STANDARD TABLE
        RETURNING
          VALUE(rt_headers) TYPE /scwm/dlv_header_out_prd_tab,

      ask_for_carrier_name
        RETURNING
          VALUE(rv_partyno) TYPE /scdl/dl_partyno,

      assign_carrier_for_dlvs
        IMPORTING
          it_docid   TYPE /scwm/dlv_docid_item_tab
          iv_partyno TYPE /scdl/dl_partyno
          iv_tstmp   TYPE timestamp,

      prevalidate_dlvs
        CHANGING
          ct_docid TYPE /scwm/dlv_docid_item_tab,

      load_bo_instances
        IMPORTING
          it_docid TYPE /scwm/dlv_docid_item_tab,

      valid_dlv
        IMPORTING
          io_bo           TYPE REF TO /scdl/if_bo
        RETURNING
          VALUE(rv_valid) TYPE abap_bool,

      lock_dlv
        IMPORTING
          iv_docid  TYPE /scdl/dl_docid
        EXPORTING
          ev_failed TYPE abap_bool,

      check_error
        IMPORTING
          it_return_codes         TYPE /scdl/t_sp_return_code
          iv_rejected             TYPE abap_bool
        RETURNING
          VALUE(rv_error_occured) TYPE abap_bool,

      add_message_to_log
        IMPORTING
          iv_docno       TYPE /scwm/sp_docno_int
          iv_probclass_i TYPE i
          it_msg         TYPE /scwm/tt_dlv_log_hu_msg_db,

      get_messages_from_sp
        EXPORTING
          ev_probclass_i TYPE i
        CHANGING
          ct_msg         TYPE /scwm/tt_dlv_log_hu_msg_db,

      query_dlvs
        IMPORTING
          it_docno   TYPE /scwm/dlv_docno_itemno_tab
        EXPORTING
          et_headers TYPE /scwm/dlv_header_out_prd_tab
          et_items   TYPE /scwm/dlv_item_out_prd_tab
          ev_failed  TYPE xfeld,

      update_dlvs
        IMPORTING
          it_headers    TYPE /scwm/dlv_header_out_prd_tab
          it_dlv_update TYPE ztt_vce_dlv_upd_fields,

      update_dlv
        IMPORTING
          io_bo                TYPE REF TO /scdl/if_bo
          is_dlv_update        TYPE zstr_vce_dlv_upd_fields
        EXPORTING
          ev_rollback_required TYPE        abap_bool,

      update_carr
        IMPORTING
          io_bo      TYPE REF TO /scdl/if_bo
          iv_partyno TYPE zde_bu_partner
        EXPORTING
          ev_failed  TYPE        abap_bool,

      update_cutofftime
        IMPORTING
          io_bo     TYPE REF TO /scdl/if_bo
          iv_tstmp  TYPE timestamp
        EXPORTING
          ev_failed TYPE        abap_bool,

      update_item_wpt
        IMPORTING
          io_bo     TYPE REF TO /scdl/if_bo
          iv_hutype TYPE zde_vce_hutype
        EXPORTING
          ev_failed TYPE        abap_bool,

      update_eew_fields
        IMPORTING
          io_bo         TYPE REF TO /scdl/if_bo
          is_dlv_update TYPE zstr_vce_dlv_upd_fields
        EXPORTING
          ev_failed     TYPE        abap_bool,

      update_error_desc
        IMPORTING
          io_bo         TYPE REF TO /scdl/if_bo
          iv_error_desc TYPE zde_errdesc
        EXPORTING
          ev_failed     TYPE        abap_bool,

      ins_or_upd_carr_for_dlv
        IMPORTING
          io_bo                TYPE REF TO /scdl/if_bo
          iv_partyno           TYPE        /scdl/dl_partyno
          iv_tstmp             TYPE timestamp
        EXPORTING
          ev_rollback_required TYPE abap_bool,

      insert_carr_for_dlv
        IMPORTING
          io_bo      TYPE REF TO /scdl/if_bo
          iv_partyno TYPE        /scdl/dl_partyno
          iv_tstmp   TYPE timestamp
        EXPORTING
          ev_failed  TYPE        abap_bool,

      update_carr_for_dlv
        IMPORTING
          io_bo            TYPE REF TO /scdl/if_bo
          iv_partyno       TYPE        /scdl/dl_partyno
          is_partyloc_orig TYPE        /scdl/dl_partyloc_str
          iv_tstmp         TYPE timestamp OPTIONAL
          is_date_orig     TYPE /scdl/dl_date_str
        EXPORTING
          ev_failed        TYPE        abap_bool,

      insert_tmstmp_for_dlv
        IMPORTING
          io_bo     TYPE REF TO /scdl/if_bo
          iv_tstmp  TYPE timestamp OPTIONAL
        EXPORTING
          ev_failed TYPE        abap_bool,

      update_tmstmp_for_dlv
        IMPORTING
          io_bo        TYPE REF TO /scdl/if_bo
          iv_tstmp     TYPE timestamp OPTIONAL
          is_date_orig TYPE /scdl/dl_date_str
        EXPORTING
          ev_failed    TYPE        abap_bool,

      execute_validation
        IMPORTING
          iv_docid  TYPE /scdl/dl_docid
        EXPORTING
          ev_failed TYPE abap_bool,

      reverse_action
        IMPORTING
          io_bo            TYPE REF TO /scdl/if_bo
          iv_num_of_carr   TYPE        i
          is_partyloc_orig TYPE        /scdl/dl_partyloc_str
          is_date_orig     TYPE /scdl/dl_date_str
        EXPORTING
          ev_failed        TYPE        abap_bool,

      delete_carr
        IMPORTING
          iv_docid   TYPE /scdl/dl_docid
          iv_counter TYPE i
        EXPORTING
          ev_failed  TYPE abap_bool,

      save_changes
        EXPORTING
          ev_failed TYPE abap_bool,

      display_log,

      cleanup.

ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD assign_carrier.
    DATA:
      lt_headers   TYPE /scwm/dlv_header_out_prd_tab,
      lt_docid     TYPE /scwm/dlv_docid_item_tab,
      ls_ship_calc TYPE zstr_mon_ship_calc,
      lv_tzone     TYPE  tznzone.

    lt_headers = get_docno_for_selected_record( it_data ).

    DATA(lt_ship_calc) = VALUE ztt_mon_ship_calc( FOR <ls_data> IN it_data
                                  ( CONV zstr_mon_ship_calc( <ls_data> ) ) ).

    lt_docid = VALUE /scwm/dlv_docid_item_tab( FOR <ls_header> IN lt_headers
                                                  ( doccat = <ls_header>-doccat
                                                    docid = <ls_header>-docid ) ).

    CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
      EXPORTING
        iv_lgnum        = mv_lgnum
      IMPORTING
        ev_tzone        = lv_tzone
      EXCEPTIONS
        interface_error = 1
        data_not_found  = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    LOOP AT lt_docid ASSIGNING FIELD-SYMBOL(<ls_docid>).

      ls_ship_calc = VALUE #( lt_ship_calc[ docno = lt_headers[ docid = <ls_docid>-docid ]-docno ] ).

      CONVERT DATE ls_ship_calc-cutoff_date
              TIME ls_ship_calc-cutoff_time
              INTO TIME STAMP DATA(lv_tmstmp) TIME ZONE lv_tzone.

      CHECK ls_ship_calc-bupartner IS NOT INITIAL
        AND lv_tmstmp IS NOT INITIAL.

      assign_carrier_for_dlvs(
        EXPORTING
          it_docid   = VALUE #( FOR <ls_d> IN lt_docid
                                  WHERE ( docid = <ls_docid>-docid )
                                  ( <ls_d> ) )
          iv_partyno = CONV #( ls_ship_calc-bupartner )
          iv_tstmp   = lv_tmstmp ).

    ENDLOOP.

    cleanup( ).

  ENDMETHOD.

  METHOD update_dlv.

    DATA:
      lo_header      TYPE REF TO /scdl/cl_dl_header,
*      lt_partyloc      TYPE        /scdl/dl_partyloc_tab,
*      lt_date          TYPE        /scdl/dl_date_tab,
*      lv_num_of_carr   TYPE        i,
*      lv_num_of_date   TYPE        i,
*      ls_partyloc_orig TYPE        /scdl/dl_partyloc_str,
*      ls_date_orig     TYPE        /scdl/dl_date_str,
      lt_msg         TYPE        /scwm/tt_dlv_log_hu_msg_db,
      lv_probclass_i TYPE        i,
*      ls_msg           TYPE        /scwm/s_dlv_log_hu_msg_db,
      lv_failed      TYPE        abap_bool.

    lo_header = io_bo->get_header( ).

*    DATA(lo_items) = io_bo->get_item(
**                       iv_itemid      =                                            " Item ID
**                       iv_itemno      =                                            " Item Number
**                       iv_objectstate = /scdl/if_dl_object_c=>sc_object_state_curr " Object State
*                     ).

*    is_dlv_update

*    DESCRIBE TABLE lt_partyloc LINES lv_num_of_carr.
*    DESCRIBE TABLE lt_date LINES lv_num_of_date.

*    CASE lv_num_of_carr.
*      WHEN 0.
*      WHEN 1.
*        READ TABLE lt_partyloc INTO ls_partyloc_orig INDEX 1.
*        READ TABLE lt_date INTO ls_date_orig INDEX 1.
*
*        IF lv_failed EQ abap_true.
**          MESSAGE
**           ID     '/SCWM/DELIVERY'
**           TYPE   /scwm/cl_dm_message_no=>sc_msgty_abort
**           NUMBER '492'
**           WITH   iv_partyno
**           INTO   ls_msg-message.
*
*          APPEND ls_msg TO lt_msg.
*
*          add_message_to_log(
*            EXPORTING
*              iv_docno       = io_bo->get_docno( )
*              iv_probclass_i = mc_high
*              it_msg         = lt_msg ).
*          RETURN.
*        ENDIF.
*
*      WHEN OTHERS.
**       Multiple carriers assigned: we don't deal with it
*        MESSAGE
*          ID     '/SCWM/DELIVERY'
*          TYPE   /scwm/cl_dm_message_no=>sc_msgty_abort
*          NUMBER '491'
**          WITH   iv_partyno
*          INTO   ls_msg-message.
*
*        APPEND ls_msg TO lt_msg.
*
*        add_message_to_log(
*          EXPORTING
*            iv_docno       = io_bo->get_docno( )
*            iv_probclass_i = mc_high
*            it_msg         = lt_msg ).
*        RETURN.
*    ENDCASE.

    "update pallet num and parcel num no matter
    "if the response has resulted in error or success
    update_eew_fields(
      EXPORTING
        io_bo         = io_bo
        is_dlv_update = is_dlv_update
      IMPORTING
        ev_failed     = lv_failed ).

    IF lv_failed <> abap_true.

      IF is_dlv_update-error_desc IS NOT INITIAL.

        update_error_desc(
          EXPORTING
            io_bo         = io_bo
            iv_error_desc = is_dlv_update-error_desc
          IMPORTING
            ev_failed     = lv_failed
        ).

      ELSE.

        IF is_dlv_update-bupartner IS NOT INITIAL.
          update_carr(
            EXPORTING
              io_bo      = io_bo
              iv_partyno = is_dlv_update-bupartner
            IMPORTING
              ev_failed  = lv_failed
          ).
        ENDIF.

        IF is_dlv_update-cutofftime IS NOT INITIAL.

          update_cutofftime(
            EXPORTING
              io_bo     = io_bo
              iv_tstmp  = is_dlv_update-cutofftime
            IMPORTING
              ev_failed = lv_failed
          ).

        ENDIF.
*
        IF is_dlv_update-hutype IS NOT INITIAL.

          update_item_wpt(
            EXPORTING
              io_bo     = io_bo
              iv_hutype = is_dlv_update-hutype
            IMPORTING
              ev_failed = lv_failed
          ).

        ENDIF.

      ENDIF.

    ENDIF.

    IF lv_failed EQ abap_true.
*      get_messages_from_sp(
*        IMPORTING
*          ev_probclass_i = lv_probclass_i
*        CHANGING
*          ct_msg         = lt_msg ).
*
*      add_message_to_log(
*        EXPORTING
*          iv_docno       = io_bo->get_docno( )
*          iv_probclass_i = lv_probclass_i
*          it_msg         = lt_msg ).

      ev_rollback_required = lv_failed.

      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD update_eew_fields.

    DATA:
      ls_inrecords    TYPE /scdl/s_sp_a_head_eew_prd,
      lt_outrecords   TYPE /scdl/t_sp_a_head_eew_prd,
      lt_return_codes	TYPE /scdl/t_sp_return_code,
      lv_rejected     TYPE abap_bool,
      lo_header       TYPE REF TO /scdl/cl_dl_header_prd.

    lo_header ?= io_bo->get_header( ).

    DATA(ls_eew) = lo_header->get_eew( ).

    MOVE-CORRESPONDING ls_eew TO ls_inrecords.

    ls_inrecords-zzpallet_num = COND #( WHEN is_dlv_update-pallet_num IS NOT INITIAL
                                        THEN is_dlv_update-pallet_num
                                        ELSE ls_inrecords-zzpallet_num ).
    ls_inrecords-zzparcel_num = COND #( WHEN is_dlv_update-parcel_num IS NOT INITIAL
                                        THEN is_dlv_update-parcel_num
                                        ELSE ls_inrecords-zzparcel_num ).
    ls_inrecords-zzservicetype = COND #( WHEN is_dlv_update-servicetype IS NOT INITIAL
                                         THEN is_dlv_update-servicetype
                                         ELSE ls_inrecords-zzservicetype ).
    ls_inrecords-zzcarma_ucode = COND #( WHEN is_dlv_update-carma_user_code IS NOT INITIAL
                                         THEN is_dlv_update-carma_user_code
                                         ELSE ls_inrecords-zzcarma_ucode ).
    DATA(lv_counter) = 0.
    SPLIT is_dlv_update-add_services AT zif_wme_c=>gs_gen-comma INTO TABLE DATA(lt_add_serv).
    LOOP AT lt_add_serv ASSIGNING FIELD-SYMBOL(<ls_add_serv>).
      CHECK <ls_add_serv> IS NOT INITIAL.

      lv_counter += 1.
      CASE lv_counter.
        WHEN 1.
          ls_inrecords-zzadd_service1 = <ls_add_serv>.
        WHEN 2.
          ls_inrecords-zzadd_service2 = <ls_add_serv>.
        WHEN 3.
          ls_inrecords-zzadd_service3 = <ls_add_serv>.
        WHEN 4.
          ls_inrecords-zzadd_service4 = <ls_add_serv>.
        WHEN 5.
          ls_inrecords-zzadd_service5 = <ls_add_serv>.
      ENDCASE.
    ENDLOOP.

    ls_inrecords-docid = lo_header->get_docid( ).

    mo_sp->update(
      EXPORTING
        aspect       = /scdl/if_sp_c=>sc_asp_head_eew_prd
        inrecords    = VALUE /scdl/t_sp_a_head_eew_prd( ( ls_inrecords ) )
      IMPORTING
        outrecords   = lt_outrecords
        return_codes = lt_return_codes
        rejected     = lv_rejected ).

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD update_carr.

    DATA:
      lt_sp_a_head_party     TYPE /scdl/t_sp_a_head_partyloc,
      lt_sp_a_head_party_out TYPE /scdl/t_sp_a_head_partyloc,
      ls_sp_a_head_party     TYPE /scdl/s_sp_a_head_partyloc,
*      lt_out                 TYPE /scdl/t_sp_a_item,
      lt_return_codes        TYPE /scdl/t_sp_return_code,
      lv_rejected            TYPE abap_bool.

    DATA:
      ls_relation_inkey    TYPE /scdl/s_sp_k_head,
*      ls_inrecord          TYPE /scdl/s_sp_a_head_partyloc,
      lt_inrecords         TYPE /scdl/t_sp_a_head_partyloc,
      lt_outrecords        TYPE /scdl/t_sp_a_head_partyloc,
      ls_relaion_outrecord TYPE /scdl/s_sp_a_head,
      lv_bupartner_name    TYPE bu_name1.

    DATA(lo_header) = io_bo->get_header( ).

    lo_header->get_partyloc(
      EXPORTING
        iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr
      RECEIVING
        et_partyloc   = DATA(lt_partyloc) ).

    CALL FUNCTION 'BKK_BUPA_PARTNER_NAME_GET'
      EXPORTING
        i_partner = CONV bu_partner( |{ iv_partyno ALPHA = IN }| )
      IMPORTING
        e_name1   = lv_bupartner_name.

    READ TABLE lt_partyloc INDEX 1 INTO DATA(ls_partyloc_orig).

    IF sy-subrc <> 0.
      "insert carrier

      APPEND VALUE #( docid      = io_bo->get_docid( )
                      party_role = /scmb/if_tmdl_roles_c=>prt_carrier
                      value_ind  = /scdl/if_dl_data_c=>sc_ind_manual
                      partyno    = iv_partyno
                      party_text = lv_bupartner_name ) TO lt_inrecords.

      ls_relation_inkey-docid = io_bo->get_docid( ).

      mo_sp->insert(
        EXPORTING
          inrecords          = lt_inrecords
          aspect             = /scdl/if_sp_c=>sc_asp_head_partyloc
          relation           = /scdl/if_sp_c=>sc_rel_head_to_partyloc
          relation_inkey     = ls_relation_inkey
        IMPORTING
          outrecords         = lt_outrecords
          relation_outrecord = ls_relaion_outrecord
          rejected           = lv_rejected
          return_codes       = lt_return_codes ).

    ELSE.
      "update carrier

      MOVE-CORRESPONDING ls_partyloc_orig TO ls_sp_a_head_party.
      ls_sp_a_head_party-docid     = io_bo->get_docid( ).
      ls_sp_a_head_party-partyno   = iv_partyno.
      ls_sp_a_head_party-party_text   = lv_bupartner_name.
      ls_sp_a_head_party-value_ind = /scdl/if_dl_data_c=>sc_ind_manual.

      APPEND ls_sp_a_head_party TO lt_sp_a_head_party.

      mo_sp->update(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_head_partyloc
          inrecords    = lt_sp_a_head_party
        IMPORTING
          outrecords   = lt_sp_a_head_party_out
          rejected     = lv_rejected
          return_codes = lt_return_codes ).

    ENDIF.

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD update_cutofftime.

    DATA:
      lt_return_codes       TYPE /scdl/t_sp_return_code,
      ls_relation_inkey     TYPE /scdl/s_sp_k_head,
*      ls_inrecord           TYPE /scdl/s_sp_a_head_date,
      lt_inrecords          TYPE /scdl/t_sp_a_head_date,
      lt_outrecords         TYPE /scdl/t_sp_a_head_date,
      ls_relation_outrecord TYPE /scdl/s_sp_a_head.

    DATA:
      lt_date               TYPE /scdl/dl_date_tab,
      lt_sp_a_head_date     TYPE /scdl/t_sp_a_head_date,
      lt_sp_a_head_date_out TYPE /scdl/t_sp_a_head_date,
      ls_sp_a_head_date     TYPE /scdl/s_sp_a_head_date,
      lv_tzone              TYPE tznzone,
      lv_rejected           TYPE abap_bool.

    DATA(lo_header) = io_bo->get_header( ).

    lo_header->get_date(
      EXPORTING
        iv_tsttype      = /scdl/if_dl_date_c=>sc_tsttype_outyard
        iv_tst_category = /scdl/if_dl_date_c=>sc_tstcat_plan                 " Date/Time Category
      RECEIVING
        et_date         = lt_date                 " Dates/Times Table
    ).

    READ TABLE lt_date INDEX 1 INTO DATA(ls_date_orig).

    IF sy-subrc <> 0.

      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum = mv_lgnum
        IMPORTING
          ev_tzone = lv_tzone.

      APPEND VALUE #( docid = io_bo->get_docid( )
                      date_indicator  = /scdl/if_dl_data_c=>sc_ind_manual
                      tsttype = /scdl/if_dl_date_c=>sc_tsttype_outyard
                      tst_category = /scdl/if_dl_date_c=>sc_tstcat_plan
                      tzone  = lv_tzone
                      tstfr = iv_tstmp
                      tstto =  iv_tstmp  ) TO lt_inrecords.

      ls_relation_inkey-docid = io_bo->get_docid( ).

      mo_sp->insert(
        EXPORTING
          inrecords          = lt_inrecords
          aspect             = /scdl/if_sp_c=>sc_asp_head_date
          relation           = /scdl/if_sp_c=>sc_rel_head_to_date
          relation_inkey     = ls_relation_inkey
        IMPORTING
          outrecords         = lt_outrecords
          relation_outrecord = ls_relation_outrecord
          rejected           = lv_rejected
          return_codes       = lt_return_codes ).

    ELSE.

      MOVE-CORRESPONDING ls_date_orig TO ls_sp_a_head_date.

      ls_sp_a_head_date-docid = io_bo->get_docid( ).
      ls_sp_a_head_date-date_indicator  = /scdl/if_dl_data_c=>sc_ind_manual.
      ls_sp_a_head_date-tstfr = iv_tstmp.
      ls_sp_a_head_date-tstto =  iv_tstmp.

      APPEND ls_sp_a_head_date TO lt_sp_a_head_date.

      mo_sp->update(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_head_date
          inrecords    = lt_sp_a_head_date
        IMPORTING
          outrecords   = lt_sp_a_head_date_out
          rejected     = lv_rejected
          return_codes = lt_return_codes ).

    ENDIF.

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).


  ENDMETHOD.

  METHOD update_item_wpt.

    DATA: lt_inrecords       TYPE  /scdl/t_sp_k_head,
          ls_inrecords_item  TYPE /scdl/s_sp_a_item_sapext_prdo,
          lt_inrecords_item  TYPE  /scdl/t_sp_a_item_sapext_prdo,
          lt_outrecords_item TYPE  /scdl/t_sp_a_item_sapext_prdo,
          lt_outrecords      TYPE /scdl/t_sp_a_item,
          lv_rejected        TYPE abap_bool,
          lt_return_codes    TYPE /scdl/t_sp_return_code,
          lt_items           TYPE /scdl/dl_item_tab,
          lo_item_prd        TYPE REF TO /scdl/cl_dl_item_prd.

    DATA(lo_header) = io_bo->get_header( ).

    APPEND VALUE #( docid = lo_header->get_docid( ) ) TO lt_inrecords.

    mo_sp->select_by_relation(
      EXPORTING
        relation     = /scdl/if_sp_c=>sc_rel_head_to_item
        inrecords    = lt_inrecords
        aspect       = /scdl/if_sp_c=>sc_asp_head
*       options      =
      IMPORTING
        outrecords   = lt_outrecords
        rejected     = lv_rejected
        return_codes = lt_return_codes
    ).

    io_bo->get_item_tab(
*        EXPORTING
*          iv_objectstate  = /scdl/if_dl_object_c=>sc_object_state_curr " Object State
*          iv_incl_deleted = abap_false                                 " Data Element for Domain BOOLE: TRUE (='X') and FALSE (=' ')
*          iv_docid        =                                            " In case the items in the inactive DO demand
      IMPORTING
        et_item = lt_items                                           " Item Table
*       et_item_sorted  =                                            " Item Table Sorted by Hierarchy Information
    ).

    zcl_crud_ztout_map_wpt=>select_single_by_key(
      EXPORTING
        iv_lgnum  = mv_lgnum
        iv_hutype = iv_hutype                 " VCE Proposal: Handling Unit Type
      RECEIVING
        rs_result = DATA(ls_map_wpt)
    ).

    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).

      lo_item_prd ?= <ls_item>-item.

      lo_item_prd->get_sapext(
        IMPORTING
          es_sapext_o = DATA(ls_sapext)                 " SAP Enhancements: Delivery Work Document Item, Outbound
      ).

      MOVE-CORRESPONDING ls_sapext TO ls_inrecords_item.

      ls_inrecords_item-docid = lo_header->get_docid( ).
      ls_inrecords_item-itemid = <ls_item>-itemid.
      ls_inrecords_item-/scwm/procty = ls_map_wpt-procty.

      APPEND ls_inrecords_item TO lt_inrecords_item.

    ENDLOOP.

    mo_sp->update(
      EXPORTING
        aspect       = /scdl/if_sp_c=>sc_asp_item_sapext_prdo
        inrecords    = lt_inrecords_item
      IMPORTING
        outrecords   = lt_outrecords_item
        rejected     = lv_rejected
        return_codes = lt_return_codes
    ).

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD update_error_desc.

    DATA:
      ls_inrecords    TYPE /scdl/s_sp_a_head_eew_prd,
      lt_outrecords   TYPE /scdl/t_sp_a_head_eew_prd,
      lt_return_codes	TYPE /scdl/t_sp_return_code,
      lv_rejected     TYPE abap_bool,
      lo_header       TYPE REF TO /scdl/cl_dl_header_prd.

    lo_header ?= io_bo->get_header( ).

    DATA(ls_eew) = lo_header->get_eew( ).

    MOVE-CORRESPONDING ls_eew TO ls_inrecords.

    ls_inrecords-zzerror_desc = iv_error_desc.
    ls_inrecords-docid = lo_header->get_docid( ).

    mo_sp->update(
      EXPORTING
        aspect       = /scdl/if_sp_c=>sc_asp_head_eew_prd
        inrecords    = VALUE /scdl/t_sp_a_head_eew_prd( ( ls_inrecords ) )
      IMPORTING
        outrecords   = lt_outrecords
        return_codes = lt_return_codes
        rejected     = lv_rejected
    ).

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD get_docid_for_selected_records.
    DATA:
      ls_docid TYPE /scwm/dlv_docid_item_str.

    FIELD-SYMBOLS:
      <lfs_wo>        TYPE any.

    LOOP AT it_data ASSIGNING <lfs_wo>.
      MOVE-CORRESPONDING <lfs_wo> TO ls_docid.
      COLLECT ls_docid INTO rt_docid.
    ENDLOOP.

    IF rt_docid IS INITIAL.
      MESSAGE s622(/scwm/delivery) DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD get_docno_for_selected_record.

    DATA:
      lt_docno TYPE /scwm/dlv_docno_itemno_tab.

    FIELD-SYMBOLS:
      <lfs_wo>        TYPE any.

    LOOP AT it_data ASSIGNING <lfs_wo>.
      APPEND INITIAL LINE TO lt_docno ASSIGNING FIELD-SYMBOL(<ls_docno>).
      MOVE-CORRESPONDING <lfs_wo> TO <ls_docno>.
    ENDLOOP.

    SORT lt_docno BY docno ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_docno.

    IF sy-subrc EQ 0.
      "there are adjacent duplicates
*      msg please select unique deliveries
      MESSAGE s673(/sehs/ba_misc1) DISPLAY LIKE wmegc_severity_war.
      RETURN.
    ENDIF.

    TRY.
        mo_prd->query(
          EXPORTING
            it_docno   = lt_docno                 " Key for Delivery
          IMPORTING
            et_headers = rt_headers              " Delivery Header (PRD) for Read Operations
        ).
      CATCH /scdl/cx_delivery. " Exception Class of Delivery
        MESSAGE s622(/scwm/delivery) DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD ask_for_carrier_name.

    DATA:
      ls_popup_param TYPE        /scmb/s_insert_parameters,
      lv_cancel      TYPE        abap_bool,
      ls_inparam     TYPE REF TO data,
      lo_msg_handler TYPE REF TO /scmb/cl_message_handler,
      ls_carr        TYPE        /scwm/s_ass_carr.

    FIELD-SYMBOLS:
      <lfs_inparam>  TYPE any.

    ls_popup_param-v_structure_name   = '/SCWM/S_ASS_CARR'.
    ls_popup_param-v_popup_title_text = TEXT-018 ##TEXT_POOL.
    APPEND 'PARTYNO' TO ls_popup_param-t_mandatory_fields.

    CALL FUNCTION '/SCMB/EES_DYNGEN'
      EXPORTING
        iv_structure        = ls_popup_param-v_structure_name
        iv_title_text       = ls_popup_param-v_popup_title_text
        it_mandatory_fields = ls_popup_param-t_mandatory_fields
        io_message_handler  = lo_msg_handler
      IMPORTING
        ev_cancel           = lv_cancel
        es_structure        = ls_inparam.

    CHECK lv_cancel EQ abap_false.
    ASSIGN: ls_inparam->* TO <lfs_inparam>.
    MOVE-CORRESPONDING <lfs_inparam> TO ls_carr.
    rv_partyno = ls_carr-partyno.

  ENDMETHOD.

  METHOD assign_carrier_for_dlvs.

    DATA:
      ls_docid             TYPE        /scwm/dlv_docid_item_str,
      lt_msg               TYPE        /scwm/tt_dlv_log_hu_msg_db,
      lv_probclass_i       TYPE        i,
      lo_bo                TYPE REF TO /scdl/if_bo,
      lv_rollback_required TYPE        abap_bool,
      lv_failed            TYPE        abap_bool,
      lv_save_required     TYPE        abap_bool,
      lv_dummymsg          TYPE        bapi_msg.

    LOOP AT it_docid INTO ls_docid.
      CLEAR: lv_failed, lt_msg, lv_probclass_i, lo_bo.
      mo_message_box->cleanup( ).

      lo_bo = mo_bom->get_bo_by_id( iv_docid = ls_docid-docid ).
      CHECK lo_bo IS BOUND.

      lock_dlv(
        EXPORTING
          iv_docid  = ls_docid-docid
        IMPORTING
          ev_failed = lv_failed ).

      IF lv_failed EQ abap_true.
        get_messages_from_sp(
          IMPORTING
            ev_probclass_i = lv_probclass_i
          CHANGING
            ct_msg         = lt_msg ).

        add_message_to_log(
          EXPORTING
            iv_docno       = lo_bo->get_docno( )
            iv_probclass_i = lv_probclass_i
            it_msg         = lt_msg ).
      ELSE.
        mo_message_box->cleanup( ).
        ins_or_upd_carr_for_dlv(
          EXPORTING
            io_bo                = lo_bo
            iv_partyno           = iv_partyno
            iv_tstmp             = iv_tstmp
          IMPORTING
            ev_rollback_required = lv_rollback_required ).

        IF lv_failed EQ abap_true.
          EXIT.
        ELSE.
          lv_save_required = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_rollback_required EQ abap_false.

      CHECK lv_save_required EQ abap_true..
      save_changes( IMPORTING ev_failed = lv_failed ).

      IF lv_failed EQ abap_false.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      /scwm/cl_tm=>cleanup( ).
    ELSE.
      ROLLBACK WORK.
      /scwm/cl_tm=>cleanup( ).
      FREE mo_log.
      CREATE OBJECT mo_log.

      MESSAGE
        ID     '/SCWM/DELIVERY'
        TYPE   /scwm/cl_dm_message_no=>sc_msgty_error
        NUMBER '192'
        INTO   lv_dummymsg.

      mo_log->add_message_log_general(
        EXPORTING
          iv_probclass = wmegc_log_vip
          it_msg       = lt_msg ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    TRY.
        CREATE OBJECT:
          mo_attr_handler,
          mo_message_box,
          mo_prd,
          mo_log,
          mo_sp
            EXPORTING
              io_attribute_handler = mo_attr_handler
              io_message_box       = mo_message_box
              iv_doccat            = /scdl/if_dl_doc_c=>sc_doccat_out_prd
              iv_mode              = /scdl/cl_sp=>sc_mode_classic.

        mo_bom = /scdl/cl_bo_management=>get_instance( ).
      CATCH cx_root.
        RAISE EXCEPTION TYPE lcx_constructor_error.
    ENDTRY.

    mv_lgnum = iv_lgnum.

  ENDMETHOD.

  METHOD load_bo_instances.

    DATA:
      ls_docid LIKE LINE OF it_docid.

    READ TABLE it_docid INTO ls_docid INDEX 1.

    TRY.
        mo_prd->query(
          EXPORTING
            iv_doccat = ls_docid-doccat
            it_docid  = it_docid ).
      CATCH /scdl/cx_delivery.
        RAISE EXCEPTION TYPE lcx_bom_error.
    ENDTRY.

  ENDMETHOD.

  METHOD prevalidate_dlvs.

* we filter out the documents which already have validation errors
* otherwise we might get errors during save at the last stage

    DATA:
      lt_docid TYPE        /scwm/dlv_docid_item_tab,
      ls_docid TYPE        /scwm/dlv_docid_item_str,
      lo_bo    TYPE REF TO /scdl/if_bo.

    load_bo_instances( it_docid = ct_docid ).

    LOOP AT ct_docid INTO ls_docid.
      lo_bo = mo_bom->get_bo_by_id( iv_docid = ls_docid-docid ).
      CHECK lo_bo IS BOUND.

      IF valid_dlv( lo_bo ) EQ abap_true.
        APPEND ls_docid TO lt_docid.
      ENDIF.
    ENDLOOP.

    /scwm/cl_tm=>cleanup( ).
    ct_docid = lt_docid.

  ENDMETHOD.

  METHOD valid_dlv.

* before starting the process, we check if the document is valid
* if it's not or validation fails, we skip this document

    DATA:
      lv_validation_fail TYPE abap_bool,
      lt_msg             TYPE /scwm/tt_dlv_log_hu_msg_db,
      lv_probclass_i     TYPE i,
      ls_msg             TYPE /scwm/s_dlv_log_hu_msg_db.

    rv_valid = abap_true.

    execute_validation(
      EXPORTING
        iv_docid  = io_bo->get_docid( )
      IMPORTING
        ev_failed = lv_validation_fail ).

    get_messages_from_sp(
      IMPORTING
        ev_probclass_i = lv_probclass_i
      CHANGING
        ct_msg         = lt_msg ).

    IF lv_validation_fail EQ abap_true OR lv_probclass_i EQ mc_high.

      get_messages_from_sp(
        IMPORTING
          ev_probclass_i = lv_probclass_i
        CHANGING
          ct_msg         = lt_msg ).

      MESSAGE
        ID     '/SCWM/DELIVERY'
        TYPE   /scwm/cl_dm_message_no=>sc_msgty_abort
        NUMBER '488'
        INTO   ls_msg-message.

      APPEND ls_msg TO lt_msg.

      add_message_to_log(
        EXPORTING
          iv_docno       = io_bo->get_docno( )
          iv_probclass_i = mc_high
          it_msg         = lt_msg ).

      rv_valid = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD lock_dlv.

    DATA:
      lt_sp_k_head    TYPE /scdl/t_sp_k_head,
      ls_sp_k_head    TYPE /scdl/s_sp_k_head,
      lt_return_codes TYPE /scdl/t_sp_return_code,
      lv_rejected     TYPE abap_bool.

    ls_sp_k_head-docid = iv_docid.
    APPEND ls_sp_k_head TO lt_sp_k_head.

    mo_sp->lock(
      EXPORTING
        inkeys       = lt_sp_k_head
        aspect       = /scdl/if_sp_c=>sc_asp_head
        lockmode     = /scdl/if_sp1_locking=>sc_exclusive_lock
      IMPORTING
        rejected     = lv_rejected
        return_codes = lt_return_codes ).

    ev_failed = check_error( it_return_codes = lt_return_codes iv_rejected = lv_rejected ).

  ENDMETHOD.

  METHOD check_error.
    READ TABLE it_return_codes TRANSPORTING NO FIELDS WITH KEY failed = abap_true.
    IF sy-subrc IS INITIAL OR iv_rejected EQ abap_true.
      rv_error_occured = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD main.

    DATA: lt_headers TYPE /scwm/dlv_header_out_prd_tab,
          lt_items   TYPE /scwm/dlv_item_out_prd_tab,
          lv_failed  TYPE xfeld.

    IF it_dlv_fields IS INITIAL.
      RETURN.
    ENDIF.

    query_dlvs(
      EXPORTING
        it_docno   = VALUE #( FOR <ls_docno> IN it_dlv_fields
                                          ( docno = <ls_docno>-docno ) )
      IMPORTING
        et_headers = lt_headers
        et_items   = lt_items
        ev_failed  = lv_failed
    ).

    IF lv_failed EQ abap_true.
      MESSAGE s036(zmc_out) WITH it_dlv_fields[ 1 ]-docno DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    update_dlvs(
      it_headers    = lt_headers
      it_dlv_update = it_dlv_fields
    ).

  ENDMETHOD.

  METHOD query_dlvs.

    TRY.
        mo_prd->query(
          EXPORTING
            it_docno   = it_docno                 " Key for Delivery
          IMPORTING
            et_headers = et_headers
            et_items   = et_items
        ).

        "check if at least one of the HUs for this DLV has a tracking number
        DATA(lv_hu_has_track_nr) = zcl_vce_request_utils=>check_hu_has_tracking_num(
          EXPORTING
            it_docid_r  = VALUE rseloption( FOR <ls_dlv_hdr> IN et_headers
                                             ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low    = <ls_dlv_hdr>-docid  ) )                 " SELECT-OPTIONS Table
            iv_huidtype = zif_wme_c=>gs_huidart-v                " Handling Unit Identification Type
        ).

        IF lv_hu_has_track_nr IS NOT INITIAL.
          ev_failed = abap_true.
          RETURN.
        ENDIF.

      CATCH /scdl/cx_delivery. " Exception Class of Delivery
    ENDTRY.

  ENDMETHOD.

  METHOD update_dlvs.

    DATA:
      lt_msg               TYPE        /scwm/tt_dlv_log_hu_msg_db,
      lv_probclass_i       TYPE        i,
      lo_bo                TYPE REF TO /scdl/if_bo,
      lv_rollback_required TYPE        abap_bool,
      lv_failed            TYPE        abap_bool,
      lv_save_required     TYPE        abap_bool,
      lv_dummymsg          TYPE        bapi_msg.

    LOOP AT it_headers ASSIGNING FIELD-SYMBOL(<ls_header>).

      CLEAR: lv_failed, lt_msg, lv_probclass_i, lo_bo.
      mo_message_box->cleanup( ).

      lo_bo = mo_bom->get_bo_by_id( iv_docid = <ls_header>-docid ).
      CHECK lo_bo IS BOUND.

      lock_dlv(
        EXPORTING
          iv_docid  = <ls_header>-docid
        IMPORTING
          ev_failed = lv_failed ).

      IF lv_failed EQ abap_true.
        get_messages_from_sp(
          IMPORTING
            ev_probclass_i = lv_probclass_i
          CHANGING
            ct_msg         = lt_msg ).

        add_message_to_log(
          EXPORTING
            iv_docno       = <ls_header>-docno
            iv_probclass_i = lv_probclass_i
            it_msg         = lt_msg ).
      ELSE.
        mo_message_box->cleanup( ).
        update_dlv(
          EXPORTING
            io_bo                = lo_bo
            is_dlv_update        = VALUE #( it_dlv_update[ docno = <ls_header>-docno ] )
          IMPORTING
            ev_rollback_required = lv_rollback_required
        ).

        IF lv_failed EQ abap_true.
          EXIT.
        ELSE.
          lv_save_required = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_rollback_required EQ abap_false.

      CHECK lv_save_required EQ abap_true.
      save_changes( IMPORTING ev_failed = lv_failed ).

      IF lv_failed EQ abap_false.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      /scwm/cl_tm=>cleanup( ).
    ELSE.
      ROLLBACK WORK.
      /scwm/cl_tm=>cleanup( ).
      FREE mo_log.
      CREATE OBJECT mo_log.

      MESSAGE
        ID     '/SCWM/DELIVERY'
        TYPE   /scwm/cl_dm_message_no=>sc_msgty_error
        NUMBER '192'
        INTO   lv_dummymsg.

      mo_log->add_message_log_general(
        EXPORTING
          iv_probclass = wmegc_log_vip
          it_msg       = lt_msg ).
    ENDIF.


  ENDMETHOD.

  METHOD ins_or_upd_carr_for_dlv.

    DATA:
      lo_header        TYPE REF TO /scdl/cl_dl_header,
      lt_partyloc      TYPE        /scdl/dl_partyloc_tab,
      lt_date          TYPE /scdl/dl_date_tab,
      lv_num_of_carr   TYPE        i,
      lv_num_of_date   TYPE        i,
      ls_partyloc_orig TYPE        /scdl/dl_partyloc_str,
      ls_date_orig     TYPE        /scdl/dl_date_str,
      lt_msg           TYPE        /scwm/tt_dlv_log_hu_msg_db,
      lv_probclass_i   TYPE        i,
      ls_msg           TYPE        /scwm/s_dlv_log_hu_msg_db,
      lv_failed        TYPE        abap_bool.

    lo_header = io_bo->get_header( ).
    lo_header->get_partyloc(
      EXPORTING
        iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr
      RECEIVING
        et_partyloc   = lt_partyloc ).

    lo_header->get_date(
      EXPORTING
        iv_tsttype      = /scdl/if_dl_date_c=>sc_tsttype_outyard
        iv_tst_category = /scdl/if_dl_date_c=>sc_tstcat_plan                 " Date/Time Category
      RECEIVING
        et_date         = lt_date                 " Dates/Times Table
    ).

    DESCRIBE TABLE lt_partyloc LINES lv_num_of_carr.
    DESCRIBE TABLE lt_date LINES lv_num_of_date.

    CASE lv_num_of_carr.
      WHEN 0.
        insert_carr_for_dlv(
          EXPORTING
            io_bo      = io_bo
            iv_partyno = iv_partyno
            iv_tstmp   = iv_tstmp
          IMPORTING
            ev_failed  = lv_failed ).
      WHEN 1.
        READ TABLE lt_partyloc INTO ls_partyloc_orig INDEX 1.
        READ TABLE lt_date INTO ls_date_orig INDEX 1.

        update_carr_for_dlv(
          EXPORTING
            io_bo            = io_bo
            iv_partyno       = iv_partyno
            is_partyloc_orig = ls_partyloc_orig
            iv_tstmp         = iv_tstmp
            is_date_orig     = ls_date_orig
          IMPORTING
            ev_failed        = lv_failed ).
        IF lv_failed EQ abap_true.
          MESSAGE
           ID     '/SCWM/DELIVERY'
           TYPE   /scwm/cl_dm_message_no=>sc_msgty_abort
           NUMBER '492'
           WITH   iv_partyno
           INTO   ls_msg-message.

          APPEND ls_msg TO lt_msg.

          add_message_to_log(
            EXPORTING
              iv_docno       = io_bo->get_docno( )
              iv_probclass_i = mc_high
              it_msg         = lt_msg ).
          RETURN.
        ENDIF.

      WHEN OTHERS.
*       Multiple carriers assigned: we don't deal with it
        MESSAGE
          ID     '/SCWM/DELIVERY'
          TYPE   /scwm/cl_dm_message_no=>sc_msgty_abort
          NUMBER '491'
*          WITH   iv_partyno
          INTO   ls_msg-message.

        APPEND ls_msg TO lt_msg.

        add_message_to_log(
          EXPORTING
            iv_docno       = io_bo->get_docno( )
            iv_probclass_i = mc_high
            it_msg         = lt_msg ).
        RETURN.
    ENDCASE.

    IF lv_failed EQ abap_true.
      get_messages_from_sp(
        IMPORTING
          ev_probclass_i = lv_probclass_i
        CHANGING
          ct_msg         = lt_msg ).

      add_message_to_log(
        EXPORTING
          iv_docno       = io_bo->get_docno( )
          iv_probclass_i = lv_probclass_i
          it_msg         = lt_msg ).

      RETURN.
    ENDIF.

*    execute_validation(
*      EXPORTING
*        iv_docid  = io_bo->get_docid( )
*      IMPORTING
*        ev_failed = ev_rollback_required ).
*
*    CHECK ev_rollback_required EQ abap_false.
*
*    get_messages_from_sp(
*      IMPORTING
*        ev_probclass_i = lv_probclass_i
*      CHANGING
*        ct_msg         = lt_msg ).
*
*    CASE lv_probclass_i.
*      WHEN mc_high.
**     There was a validation error -> we reverse our action
*        reverse_action(
*          EXPORTING
*            io_bo            = io_bo
*            iv_num_of_carr   = lv_num_of_carr
*            is_partyloc_orig = ls_partyloc_orig
*            is_date_orig     = ls_date_orig
*          IMPORTING
*            ev_failed        = ev_rollback_required ).
*
*        CHECK ev_rollback_required EQ abap_false.
*
*        MESSAGE
*          ID     '/SCWM/DELIVERY'
*          TYPE   /scwm/cl_dm_message_no=>sc_msgty_abort
*          NUMBER '492'
*          WITH   iv_partyno
*          INTO   ls_msg-message.
*
*        APPEND ls_msg TO lt_msg.
*
*      WHEN mc_imp
*        OR mc_med.
*        MESSAGE
*          ID     '/SCWM/DELIVERY'
*          TYPE   /scwm/cl_dm_message_no=>sc_msgty_success
*          NUMBER '493'
*          WITH   iv_partyno
*          INTO   ls_msg-message.
*
*        APPEND ls_msg TO lt_msg.
*    ENDCASE.
*
*    add_message_to_log(
*      EXPORTING
*        iv_docno       = io_bo->get_docno( )
*        iv_probclass_i = lv_probclass_i
*        it_msg         = lt_msg ).

  ENDMETHOD.

  METHOD insert_carr_for_dlv.

    DATA:
      lt_return_codes      TYPE /scdl/t_sp_return_code,
      lv_rejected          TYPE abap_bool,
      ls_relation_inkey    TYPE /scdl/s_sp_k_head,
      ls_inrecord          TYPE /scdl/s_sp_a_head_partyloc,
      lt_inrecords         TYPE /scdl/t_sp_a_head_partyloc,
      lt_outrecords        TYPE /scdl/t_sp_a_head_partyloc,
      ls_relaion_outrecord TYPE /scdl/s_sp_a_head.

    ls_inrecord-docid      = io_bo->get_docid( ).
    ls_inrecord-party_role = /scmb/if_tmdl_roles_c=>prt_carrier.
    ls_inrecord-value_ind  = /scdl/if_dl_data_c=>sc_ind_manual.
    ls_inrecord-partyno    = iv_partyno.
    APPEND ls_inrecord TO lt_inrecords.

    ls_relation_inkey-docid = io_bo->get_docid( ).

    mo_sp->insert(
      EXPORTING
        inrecords          = lt_inrecords
        aspect             = /scdl/if_sp_c=>sc_asp_head_partyloc
        relation           = /scdl/if_sp_c=>sc_rel_head_to_partyloc
        relation_inkey     = ls_relation_inkey
      IMPORTING
        outrecords         = lt_outrecords
        relation_outrecord = ls_relaion_outrecord
        rejected           = lv_rejected
        return_codes       = lt_return_codes ).

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).


    IF ev_failed EQ abap_false.

      insert_tmstmp_for_dlv(
        EXPORTING
          io_bo     = io_bo
          iv_tstmp  = iv_tstmp
        IMPORTING
          ev_failed = lv_rejected
      ).
    ENDIF.

  ENDMETHOD.

  METHOD insert_tmstmp_for_dlv.

    DATA:
      lt_return_codes      TYPE /scdl/t_sp_return_code,
      lv_rejected          TYPE abap_bool,
      ls_relation_inkey    TYPE /scdl/s_sp_k_head,
      ls_inrecord          TYPE /scdl/s_sp_a_head_date,
      lt_inrecords         TYPE /scdl/t_sp_a_head_date,
      lt_outrecords        TYPE /scdl/t_sp_a_head_date,
      ls_relaion_outrecord TYPE /scdl/s_sp_a_head.

    ls_inrecord-docid      = io_bo->get_docid( ).
    ls_inrecord-tsttype = /scdl/if_dl_date_c=>sc_tsttype_outyard.
    ls_inrecord-tst_category = /scdl/if_dl_date_c=>sc_tstcat_plan.
    ls_inrecord-date_indicator  = /scdl/if_dl_data_c=>sc_ind_manual.
    ls_inrecord-tstfr    = iv_tstmp.
    ls_inrecord-tstto    = iv_tstmp.

    APPEND ls_inrecord TO lt_inrecords.

    ls_relation_inkey-docid = io_bo->get_docid( ).

    mo_sp->insert(
      EXPORTING
        inrecords          = lt_inrecords
        aspect             = /scdl/if_sp_c=>sc_asp_head_date
        relation           = /scdl/if_sp_c=>sc_rel_head_to_date
        relation_inkey     = ls_relation_inkey
      IMPORTING
        outrecords         = lt_outrecords
        relation_outrecord = ls_relaion_outrecord
        rejected           = lv_rejected
        return_codes       = lt_return_codes ).

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD update_tmstmp_for_dlv.

    DATA:
      lt_return_codes TYPE /scdl/t_sp_return_code,
      lv_rejected     TYPE abap_bool.

    DATA:
      lt_sp_a_head_date     TYPE /scdl/t_sp_a_head_date,
      lt_sp_a_head_date_out TYPE /scdl/t_sp_a_head_date,
      ls_sp_a_head_date     TYPE /scdl/s_sp_a_head_date.

    DATA:
      lt_logfnames TYPE SORTED TABLE OF /scdl/dl_logfname_str WITH NON-UNIQUE KEY keyfield,
      lo_logfname  TYPE REF TO /scdl/cl_dl_logfname,
      ls_key       TYPE        /scdl/dl_logfname_key_str,
      lt_key       TYPE        /scdl/dl_logfname_key_tab,
      lv_structure TYPE        /scdl/dl_structure,
      lv_level     TYPE        /scdl/dl_level,
      ls_map       TYPE REF TO /scdl/dl_logfname_map_str,
      lt_map       TYPE        /scdl/dl_logfname_map_tab,
      lv_writable  TYPE        boole_d,
      lv_fieldname TYPE        fieldname,
      lx_delivery  TYPE REF TO /scdl/cx_delivery,
*      ls_msg       TYPE        /scwm/s_dlv_log_hu_msg_db,
*      lt_msg       TYPE        /scwm/tt_dlv_log_hu_msg_db,
      lo_header    TYPE REF TO /scdl/cl_dl_header.

    MOVE-CORRESPONDING is_date_orig TO ls_sp_a_head_date.

    ls_sp_a_head_date-docid = io_bo->get_docid( ).
    ls_sp_a_head_date-date_indicator  = /scdl/if_dl_data_c=>sc_ind_manual.
*    ls_sp_a_head_date-tsttype = /scdl/if_dl_date_c=>sc_tsttype_outyard.
*    ls_sp_a_head_date-tst_category = /scdl/if_dl_date_c=>sc_tstcat_plan.
*    ls_sp_a_head_date-tzone  = lv_tzone.

    IF iv_tstmp IS INITIAL.
      ls_sp_a_head_date-tstfr = is_date_orig-tstfr.
      ls_sp_a_head_date-tstto = is_date_orig-tstto.
    ELSE.
      ls_sp_a_head_date-tstfr = iv_tstmp.
      ls_sp_a_head_date-tstto =  iv_tstmp.
    ENDIF.

    APPEND ls_sp_a_head_date TO lt_sp_a_head_date.

    lo_header = io_bo->get_header( ).
    lt_logfnames = lo_header->get_changeable_fields( iv_exclude_key_combinations = abap_false ).
    lo_logfname = /scdl/cl_dl_logfname=>get_instance( ).
    lv_writable = abap_false.
    DELETE lt_logfnames WHERE keyfield = abap_false.
    ls_key-keyfield = 'TSTTYPE'.
    ls_key-keyvalue = 'TOUTYARD'.
    APPEND ls_key TO lt_key.
    ls_key-keyfield = 'TST_CATEGORY'.
    ls_key-keyvalue = 'PLAN'.
    APPEND ls_key TO lt_key.
    lv_structure = /scdl/if_sp_c=>sc_asp_head_date.
    lv_level = /scdl/if_dl_object_c=>sc_object_level_h.
    lv_fieldname = 'TSTTYPE'.
    IF lt_logfnames IS NOT INITIAL.
      TRY.
          lo_logfname->get_logfname_by_key(
            EXPORTING
              iv_structure = lv_structure
              iv_level     = lv_level
              iv_fieldname = lv_fieldname
              it_keys      = lt_key
            IMPORTING
              et_map_tab   = lt_map ).

          IF lt_map[] IS INITIAL.
            lv_writable = abap_false.
          ELSE.
* a key is changeable, only if all its key fields are changeable!
* One example: a date consists of 2 key fields: Date Type + Date Category.
* the plan delivery date is changeable, only if both
* date type TDELIVERY and date category PLAN are changeable
            lv_writable = abap_true. "assumption: the key is changeable
            LOOP AT lt_key INTO ls_key. "check every key field
              READ TABLE lt_map WITH KEY fieldname = ls_key-keyfield REFERENCE INTO ls_map.
              IF sy-subrc = 0. "logical field name found for the current key field
                READ TABLE lt_logfnames WITH KEY logfname = ls_map->logfname
                     TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0. "current key field is not changeable => key not changeable
                  lv_writable = abap_false.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        CATCH /scdl/cx_delivery INTO lx_delivery.
          lv_writable = abap_false.
      ENDTRY.
    ENDIF.
    IF lv_writable = abap_false.
      ev_failed = abap_true.
    ELSE.
      mo_sp->update(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_head_date
          inrecords    = lt_sp_a_head_date
        IMPORTING
          outrecords   = lt_sp_a_head_date_out
          rejected     = lv_rejected
          return_codes = lt_return_codes ).

      ev_failed = check_error(
        it_return_codes = lt_return_codes
        iv_rejected     = lv_rejected ).
    ENDIF.

  ENDMETHOD.



  METHOD update_carr_for_dlv.

    DATA:
      lt_sp_a_head_party     TYPE /scdl/t_sp_a_head_partyloc,
      lt_sp_a_head_party_out TYPE /scdl/t_sp_a_head_partyloc,
      ls_sp_a_head_party     TYPE /scdl/s_sp_a_head_partyloc,
*      lt_out                 TYPE /scdl/t_sp_a_item,
      lt_return_codes        TYPE /scdl/t_sp_return_code,
      lv_rejected            TYPE abap_bool.

    DATA:
      lt_logfnames TYPE SORTED TABLE OF /scdl/dl_logfname_str WITH NON-UNIQUE KEY keyfield,
      lo_logfname  TYPE REF TO /scdl/cl_dl_logfname,
      ls_key       TYPE        /scdl/dl_logfname_key_str,
      lt_key       TYPE        /scdl/dl_logfname_key_tab,
      lv_structure TYPE        /scdl/dl_structure,
      lv_level     TYPE        /scdl/dl_level,
      ls_map       TYPE REF TO /scdl/dl_logfname_map_str,
      lt_map       TYPE        /scdl/dl_logfname_map_tab,
      lv_writable  TYPE        boole_d,
      lv_fieldname TYPE        fieldname,
      lx_delivery  TYPE REF TO /scdl/cx_delivery,
*      ls_msg       TYPE        /scwm/s_dlv_log_hu_msg_db,
*      lt_msg       TYPE        /scwm/tt_dlv_log_hu_msg_db,
      lo_header    TYPE REF TO /scdl/cl_dl_header.

    MOVE-CORRESPONDING is_partyloc_orig TO ls_sp_a_head_party.
    ls_sp_a_head_party-docid     = io_bo->get_docid( ).
    ls_sp_a_head_party-partyno   = iv_partyno.
    ls_sp_a_head_party-value_ind = /scdl/if_dl_data_c=>sc_ind_manual.

    APPEND ls_sp_a_head_party TO lt_sp_a_head_party.

    lo_header = io_bo->get_header( ).
    lt_logfnames = lo_header->get_changeable_fields( iv_exclude_key_combinations = abap_false ).
    lo_logfname = /scdl/cl_dl_logfname=>get_instance( ).
    lv_writable = abap_false.
    DELETE lt_logfnames WHERE keyfield = abap_false.
    ls_key-keyfield = 'PARTY_ROLE'.
    ls_key-keyvalue = 'CARR'.
    APPEND ls_key TO lt_key.
    lv_structure = '/SCDL/S_SP_A_HEAD_PARTYLOC'.
    lv_level = /scdl/if_dl_object_c=>sc_object_level_h.
    lv_fieldname = 'PARTY_ROLE'.
    IF lt_logfnames IS NOT INITIAL.
      TRY.
          lo_logfname->get_logfname_by_key(
            EXPORTING
              iv_structure = lv_structure
              iv_level     = lv_level
              iv_fieldname = lv_fieldname
              it_keys      = lt_key
            IMPORTING
              et_map_tab   = lt_map ).

          IF lt_map[] IS INITIAL.
            lv_writable = abap_false.
          ELSE.
* a key is changeable, only if all its key fields are changeable!
* One example: a date consists of 2 key fields: Date Type + Date Category.
* the plan delivery date is changeable, only if both
* date type TDELIVERY and date category PLAN are changeable
            lv_writable = abap_true. "assumption: the key is changeable
            LOOP AT lt_key INTO ls_key. "check every key field
              READ TABLE lt_map WITH KEY fieldname = ls_key-keyfield REFERENCE INTO ls_map.
              IF sy-subrc = 0. "logical field name found for the current key field
                READ TABLE lt_logfnames WITH KEY logfname = ls_map->logfname
                     TRANSPORTING NO FIELDS.
                IF sy-subrc <> 0. "current key field is not changeable => key not changeable
                  lv_writable = abap_false.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        CATCH /scdl/cx_delivery INTO lx_delivery.
          lv_writable = abap_false.
      ENDTRY.
    ENDIF.
    IF lv_writable = abap_false.
      ev_failed = abap_true.
    ELSE.
      mo_sp->update(
        EXPORTING
          aspect       = /scdl/if_sp_c=>sc_asp_head_partyloc
          inrecords    = lt_sp_a_head_party
        IMPORTING
          outrecords   = lt_sp_a_head_party_out
          rejected     = lv_rejected
          return_codes = lt_return_codes ).

*      mo_sp->select_by_relation(
*        EXPORTING
*          relation     = /scdl/if_sp_c=>sc_rel_head_to_item
*          inrecords    = VALUE /scdl/t_sp_k_head( ( docid = lt_sp_a_head_party[ 1 ]-docid ) )
**         aspect       =
**         options      =
*        IMPORTING
*          outrecords   = lt_out
*          rejected     = DATA(lv_rej)
*          return_codes = DATA(lt_ret_codes)
*      ).

*      DATA(lt_items) = io_bo->get_item

      ev_failed = check_error(
        it_return_codes = lt_return_codes
        iv_rejected     = lv_rejected ).

      IF ev_failed <> abap_true.

        update_tmstmp_for_dlv(
          EXPORTING
            io_bo        = io_bo
            iv_tstmp     = iv_tstmp
            is_date_orig = is_date_orig
          IMPORTING
            ev_failed    = lv_rejected
        ).

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD execute_validation.

    DATA:
      ls_sp_action    TYPE /scdl/s_sp_act_action,
      lt_return_codes TYPE /scdl/t_sp_return_code,
      lv_rejected     TYPE abap_bool,
      lt_sp_k_head    TYPE /scdl/t_sp_k_head,
      ls_sp_k_head    TYPE /scdl/s_sp_k_head,
      lt_a_head       TYPE /scdl/t_sp_a_head.

    ls_sp_k_head-docid = iv_docid.
    APPEND ls_sp_k_head TO lt_sp_k_head.
    ls_sp_action-action_code = /scdl/if_bo_action_c=>sc_validate.

    mo_sp->execute(
      EXPORTING
        aspect       = /scdl/if_sp_c=>sc_asp_head
        inkeys       = lt_sp_k_head
        inparam      = ls_sp_action
        action       = /scdl/if_sp_c=>sc_act_execute_action
      IMPORTING
        outrecords   = lt_a_head
        rejected     = lv_rejected
        return_codes = lt_return_codes ).

*   If validation could not be executed, we rollback
    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD reverse_action.

    DATA:
      lv_probclass_i TYPE i,
      lt_msg         TYPE /scwm/tt_dlv_log_hu_msg_db.
*      ls_msg         TYPE /scwm/s_dlv_log_hu_msg_db.

    CASE iv_num_of_carr.
      WHEN 0.
*         We delete the assigned carrier
        delete_carr(
          EXPORTING
            iv_docid   = io_bo->get_docid( )
            iv_counter = 1 ).
      WHEN 1.
*         We reset the initial value
        update_carr_for_dlv(
          EXPORTING
            io_bo            = io_bo
            iv_partyno       = is_partyloc_orig-partyno
            is_partyloc_orig = is_partyloc_orig
            is_date_orig     = is_date_orig
          IMPORTING
            ev_failed        = ev_failed ).

        CHECK ev_failed EQ abap_false.

    ENDCASE.

    execute_validation(
      EXPORTING
        iv_docid  = io_bo->get_docid( )
      IMPORTING
        ev_failed = ev_failed ).

    get_messages_from_sp(
      IMPORTING
        ev_probclass_i = lv_probclass_i
      CHANGING
        ct_msg         = lt_msg ).

    IF lv_probclass_i EQ mc_high.
      ev_failed = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD delete_carr.

    DATA:
      lt_return_codes    TYPE /scdl/t_sp_return_code,
      lv_rejected        TYPE abap_bool,
      lt_sp_k_head_party TYPE /scdl/t_sp_k_head_partyloc,
      ls_sp_k_head_party TYPE /scdl/s_sp_k_head_partyloc.

    ls_sp_k_head_party-docid      = iv_docid.
    ls_sp_k_head_party-party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr.
    ls_sp_k_head_party-counter    = iv_counter.
    APPEND ls_sp_k_head_party TO lt_sp_k_head_party.

    mo_sp->delete(
      EXPORTING
        inkeys       = lt_sp_k_head_party
        aspect       = /scdl/if_sp_c=>sc_asp_head_partyloc
      IMPORTING
        rejected     = lv_rejected
        return_codes = lt_return_codes ).

    ev_failed = check_error(
      it_return_codes = lt_return_codes
      iv_rejected     = lv_rejected ).

  ENDMETHOD.

  METHOD get_messages_from_sp.

    DATA:
      lt_messages    TYPE /scdl/dm_message_tab,
      ls_message     TYPE /scdl/dm_message_str,
*      lv_probclass   TYPE balprobcl,
      lv_probclass_i TYPE i,
      ls_msg         TYPE /scwm/s_dlv_log_hu_msg_db.

    lt_messages = mo_message_box->get_messages( ).
    mo_message_box->cleanup( ).

    SORT lt_messages BY msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
    DELETE ADJACENT DUPLICATES FROM lt_messages
      COMPARING msgty msgid msgno msgv1 msgv2 msgv3 msgv4.

    LOOP AT lt_messages INTO ls_message.
      MESSAGE
        ID     ls_message-msgid
        TYPE   ls_message-msgty
        NUMBER ls_message-msgno
        WITH   ls_message-msgv1
               ls_message-msgv2
               ls_message-msgv3
               ls_message-msgv4
        INTO   ls_msg-message.

      APPEND ls_msg TO ct_msg.

* We skip consistency messages as user can correct them later
* the same is possible via /SCWM/PRDO
      IF ls_message-consistency_message = abap_true.
        lv_probclass_i = mc_med.
        CONTINUE.
      ENDIF.

* we need to decide the probclass for messages of particular ODO
* decided to select it according to the highest msgty of its messages
      CHECK lv_probclass_i NE mc_high.

      CASE ls_message-msgty.

        WHEN /scwm/cl_dm_message_no=>sc_msgty_error
        OR   /scwm/cl_dm_message_no=>sc_msgty_abort.
          IF lv_probclass_i < mc_high.
            lv_probclass_i = mc_high.
          ENDIF.

        WHEN /scwm/cl_dm_message_no=>sc_msgty_warning.
          IF lv_probclass_i < mc_imp.
            lv_probclass_i = mc_imp.
          ENDIF.

        WHEN /scwm/cl_dm_message_no=>sc_msgty_success
        OR   /scwm/cl_dm_message_no=>sc_msgty_info.
          IF lv_probclass_i < mc_med.
            lv_probclass_i = mc_med.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    ev_probclass_i = lv_probclass_i.

  ENDMETHOD.

  METHOD add_message_to_log.

    DATA:
      lv_dummymsg  TYPE bapi_msg,
      lv_msgty     TYPE symsgty,
      lv_probclass TYPE balprobcl.

    CASE iv_probclass_i.
      WHEN mc_high.
        lv_probclass = wmegc_log_vip.
        lv_msgty     = /scwm/cl_dm_message_no=>sc_msgty_error.
      WHEN mc_imp.
        lv_probclass = wmegc_log_imp.
        lv_msgty     = /scwm/cl_dm_message_no=>sc_msgty_warning.
      WHEN mc_med.
        lv_probclass = wmegc_log_med.
        lv_msgty     = /scwm/cl_dm_message_no=>sc_msgty_success.
    ENDCASE.

    MESSAGE
      ID     '/SCWM/DELIVERY'
      TYPE   lv_msgty
      NUMBER '498'
      WITH   iv_docno
      INTO   lv_dummymsg.

    mo_log->add_message_log_general(
      EXPORTING
        iv_probclass = lv_probclass
        it_msg       = it_msg ).

  ENDMETHOD.

  METHOD save_changes.

    DATA:
      lv_rejected TYPE abap_bool,
      lt_msg      TYPE /scwm/tt_dlv_log_hu_msg_db,
      ls_msg      TYPE /scwm/s_dlv_log_hu_msg_db,
      lv_dummymsg TYPE bapi_msg.

    mo_sp->save( IMPORTING rejected = lv_rejected ).
    IF lv_rejected EQ abap_true.
      get_messages_from_sp( CHANGING ct_msg = lt_msg ).

      MESSAGE
        ID     '/SCWM/DELIVERY'
        TYPE   /scwm/cl_dm_message_no=>sc_msgty_error
        NUMBER '192'
        INTO   lv_dummymsg.

      mo_log->add_message_log_general(
        EXPORTING
          iv_probclass = wmegc_log_vip
          it_msg       = lt_msg ).

      ev_failed = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD display_log.

    DATA:
      ls_display_prof TYPE bal_s_prof.

*   get display profile
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = ls_display_prof.

    ls_display_prof-disvariant-report = sy-repid.
    ls_display_prof-disvariant-handle = 'LOG'.
    ls_display_prof-show_all          = 'X'.
    ls_display_prof-use_grid          = 'X'.
    ls_display_prof-pop_adjst         = ' '.
    ls_display_prof-start_col         = '5'.
    ls_display_prof-start_row         = '5'.
    ls_display_prof-end_col           = '150'.
    ls_display_prof-end_row           = '35'.

    mo_log->display( is_display_profile = ls_display_prof ).

  ENDMETHOD.

  METHOD cleanup.

    FREE:
      mo_sp, mo_attr_handler, mo_message_box,
      mo_prd, mo_log, mo_sp, mo_bom.

  ENDMETHOD.

ENDCLASS.
