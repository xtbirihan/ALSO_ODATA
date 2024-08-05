CLASS zcl_vce_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_sign_equal TYPE c VALUE '=' ##NO_TEXT.

    METHODS add_message .
    METHODS add_parameters
      IMPORTING
        !it_object TYPE any .
    "! <p class="shorttext synchronized" lang="en">Build message class</p>
    METHODS constructor
      IMPORTING
        !iv_extnumber TYPE bal_s_log-extnumber
        !iv_altcode   TYPE bal_s_log-altcode .
    METHODS save_log
      IMPORTING
        iv_commit TYPE boole_d OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_log TYPE REF TO /scwm/cl_log .
    DATA mv_msg TYPE string .

    METHODS add_data_from_structure
      IMPORTING
        !is_structure TYPE any
        !io_data_type TYPE REF TO cl_abap_typedescr .
    METHODS add_data_from_table
      IMPORTING
        !it_table     TYPE any
        !io_data_type TYPE REF TO cl_abap_typedescr .
ENDCLASS.



CLASS ZCL_VCE_LOG IMPLEMENTATION.


  METHOD add_data_from_structure.
********************************************************************
*& Key          : <BSUGAREV>-Sep 7, 2023
*& Request No.  :
********************************************************************
*& Description  : Add data from structures to the log object
*&
*&
********************************************************************
    FIELD-SYMBOLS: <lv_comp_value> TYPE any.

    IF is_structure IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_structure) = CAST cl_abap_structdescr( io_data_type ).

    DATA(lt_components) = lo_structure->get_components( ).

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_structure TO <lv_comp_value>.

      CASE <ls_component>-type->type_kind.

        WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
          " add message with the name of the nested structure
          MESSAGE i001(zmc_odata) WITH <ls_component>-name INTO mv_msg.
          add_message( ).

          add_data_from_structure( is_structure = <lv_comp_value>
                                   io_data_type = <ls_component>-type ).

        WHEN cl_abap_typedescr=>typekind_table.
          " add message with the name of the nested table
          MESSAGE i002(zmc_odata) WITH <ls_component>-name INTO mv_msg.
          add_message( ).

          add_data_from_table( it_table = <lv_comp_value>
                               io_data_type = <ls_component>-type ).
        WHEN OTHERS.
          IF <lv_comp_value> IS NOT ASSIGNED OR <lv_comp_value> IS INITIAL.
            CONTINUE.
          ENDIF.

          MESSAGE i000(zmc_odata) WITH space <ls_component>-name c_sign_equal <lv_comp_value> INTO mv_msg.
          add_message( ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_data_from_table.
********************************************************************
*& Key          : <BSUGAREV>-Sep 7, 2023
*& Request No.  :
********************************************************************
*& Description  : Add entries from tables types to the log object
*&
*&
********************************************************************
    DATA(lo_table) = CAST cl_abap_tabledescr( io_data_type ).

    DATA(lo_table_line_type) = CAST cl_abap_typedescr( lo_table->get_table_line_type( ) ).

    CASE lo_table_line_type->type_kind.

      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.

        LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_line>).
          add_data_from_structure( is_structure = <ls_line>
                                   io_data_type = lo_table_line_type ).
        ENDLOOP.

      WHEN OTHERS.

        LOOP AT it_table ASSIGNING FIELD-SYMBOL(<ls_line_el>).
          CHECK <ls_line_el> IS NOT INITIAL.

          MESSAGE i000(zmc_odata) WITH space <ls_line_el> space space INTO mv_msg.
          add_message( ).
        ENDLOOP.

    ENDCASE.

  ENDMETHOD.


  METHOD add_message.
********************************************************************
*& Key          : <BSUGAREV>-Sep 7, 2023
*& Request No.  :
********************************************************************
*& Description  : Add new message to the log
*&
*&
********************************************************************

    mo_log->add_message( ).
  ENDMETHOD.


  METHOD add_parameters.
********************************************************************
*& Key          : <BSUGAREV>-Sep 7, 2023
*& Request No.  :
********************************************************************
*& Description  : Starting point for iterating through elements of
*&    supplied data structure
*&
********************************************************************
    DATA(lo_data_type) = CAST cl_abap_typedescr( cl_abap_datadescr=>describe_by_data( it_object ) ).

    DATA(lv_name) =  lo_data_type->get_relative_name( ).

    CASE lo_data_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.

        add_data_from_structure( is_structure = it_object
                                 io_data_type = lo_data_type ).

      WHEN cl_abap_typedescr=>typekind_table.
        DATA(lo_table) = CAST cl_abap_tabledescr( lo_data_type ).

        DATA(lo_structure) = CAST cl_abap_structdescr( lo_table->get_table_line_type( ) ).

        add_data_from_table( it_table     = it_object
                             io_data_type = lo_data_type ).
    ENDCASE.

  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : <BSUGAREV>-Sep 5, 2023
*& Request No.  :
********************************************************************
*& Description  : Build message class
********************************************************************
    DATA ls_log         TYPE bal_s_log.

    CREATE OBJECT mo_log.

    ls_log-extnumber = 1.
    ls_log-object    = zif_wme_c=>gs_msgobj-zewm.
    ls_log-subobject = zif_wme_c=>gs_msgsubobj-zodata_vce.
    ls_log-extnumber = iv_extnumber.
    ls_log-altcode   = iv_altcode.

    CALL METHOD mo_log->create_log
      EXPORTING
        is_log       = ls_log
      IMPORTING
        ev_loghandle = DATA(lv_loghandle).

  ENDMETHOD.


  METHOD save_log.
********************************************************************
*& Key          : <BSUGAREV>-Sep 7, 2023
*& Request No.  :
********************************************************************
*& Description  : Save log to the DB
*&
*&
********************************************************************
    DATA(lt_bapiret) = mo_log->get_prot( ).

    IF lines( lt_bapiret ) > 0.

      mo_log->convert_bapiret2applog(
        EXPORTING
          it_bapiret   = lt_bapiret ).

      TRY.
          mo_log->save_applog2db( ).

          IF iv_commit = abap_true.
            COMMIT WORK.
          ENDIF.
        CATCH /scwm/cx_basics.
      ENDTRY.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
