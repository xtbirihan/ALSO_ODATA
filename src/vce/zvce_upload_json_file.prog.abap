*&---------------------------------------------------------------------*
*& Report zvce_upload_json_file
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvce_upload_json_file.

PARAMETERS: p_lgnum TYPE /scwm/lgnum DEFAULT 'DE50' OBLIGATORY,
            p_file  TYPE localfile OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP rad1 DEFAULT 'X',
              r2 RADIOBUTTON GROUP rad1,
              r3 RADIOBUTTON GROUP rad1,
              r4 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK part2.

CLASS lcl_start DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS f4_file.

    METHODS main.

    METHODS upload_file
      IMPORTING
        iv_file TYPE localfile
      EXPORTING
        ev_json TYPE /ui2/cl_json=>json.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_start=>f4_file( ).

START-OF-SELECTION.
  NEW lcl_start( )->main( ).

CLASS lcl_start IMPLEMENTATION.

  METHOD main.
    DATA: ls_ship_calc_response TYPE zif_vce_shipment_calc_model=>ty_request,
          lt_shipment_response  TYPE zif_vce_ship_request_model=>tt_request,
          lv_status             TYPE i.

    upload_file(
      EXPORTING
        iv_file = p_file
      IMPORTING
        ev_json = DATA(lv_json) ).

    TRY.
        CASE abap_true.
          WHEN r1.
            " Shipment calculation request
            DATA(lo_ship_cal) = NEW zcl_vce_shipment_calculation( p_lgnum ).

            " now call function for EWM udates
            lo_ship_cal->process_response( iv_json = lv_json ).

          WHEN r2.
            " Ship request
            DATA(lo_ship_request) = NEW zcl_vce_ship_request( iv_lgnum   = p_lgnum
                                                              iv_printer = 'LOCL' ).

*            zcl_crud_zout_term_def=>select_single_by_key( ).
            " now call function for EWM updates
            lo_ship_request->process_response( iv_json = lv_json ).

          WHEN r3.
            " Cancel request
            " Shipment calculation request
            DATA(lo_cancel_req) = NEW zcl_vce_cancel_request( p_lgnum ).

            lo_cancel_req->process_response( iv_json = lv_json ).


          WHEN r4.
            " Manifest request

            DATA(lo_manifest_req) = NEW zcl_vce_manifest_request( p_lgnum ).

            lo_manifest_req->process_response( iv_json = lv_json ).

        ENDCASE.
      CATCH zcx_core_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD f4_file.
    DATA: lt_retfiletable  TYPE filetable,
          lv_retrc         TYPE sysubrc,
          lv_retuseraction TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        multiselection          = abap_false
      CHANGING
        file_table              = lt_retfiletable
        rc                      = lv_retrc
        user_action             = lv_retuseraction
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_retfiletable INTO DATA(ls_file) INDEX 1.
      IF sy-subrc = 0.
        p_file = ls_file-filename.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD upload_file.
    TYPES:
      BEGIN OF ty_file_data,
        line TYPE /ui2/cl_json=>json,
      END OF ty_file_data.

    DATA: lt_file_datas TYPE STANDARD TABLE OF ty_file_data WITH EMPTY KEY.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = CONV #( iv_file )
        filetype                = 'ASC'
      CHANGING
        data_tab                = lt_file_datas
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).

    IF lines( lt_file_datas ) = 0.
      RETURN.
    ENDIF.

*    if lines( lt_file_datas ) > 1 .
    DATA(ls_line) = VALUE string( ).
    LOOP AT lt_file_datas ASSIGNING FIELD-SYMBOL(<ls_line>).
      ls_line = <ls_line>-line.
      CONDENSE ls_line NO-GAPS.

      ev_json = |{ ev_json }{ ls_line }|.
    ENDLOOP.

    DATA(lv_init_txt) = VALUE string( ).
    ev_json = REDUCE #( INIT text = lv_init_txt FOR <line> IN lt_file_datas
                          NEXT text = text && <line>-line ).
*lw_output_h-sl_text = REDUCE #( INIT text = `` FOR <line> IN lt_tline NEXT text = text && <line>-tdline ).
*    else.
*    ev_json = lt_file_datas[ 1 ]-line.
*    ENDIF.


  ENDMETHOD.

ENDCLASS.
