CLASS zcl_vce_rest_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_core_exception.

    METHODS get.
    METHODS get_api_response
      RETURNING
        VALUE(ro_api_response) TYPE REF TO if_rest_entity .
    METHODS get_status
      RETURNING
        VALUE(rv_status) TYPE i .
    METHODS post
      IMPORTING
        !iv_api_request_data TYPE /ui2/cl_json=>json .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_http_client     TYPE REF TO if_http_client,
          mo_rest_client     TYPE REF TO cl_rest_http_client,
          mo_request_entity  TYPE REF TO if_rest_entity,
          mo_response_entity TYPE REF TO if_rest_entity.
    METHODS build_request_entity
      IMPORTING
        iv_api_request_data TYPE string.
ENDCLASS.



CLASS ZCL_VCE_REST_CLIENT IMPLEMENTATION.


  METHOD build_request_entity.
    mo_request_entity = mo_rest_client->if_rest_client~create_request_entity( ).

    mo_request_entity->set_string_data( iv_api_request_data ).

    mo_request_entity->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
  ENDMETHOD.


  METHOD constructor.
    IF iv_url IS NOT INITIAL.
      cl_http_client=>create_by_url(
        EXPORTING
            url                    = iv_url
*            proxy_host             =
*            proxy_service          =
          IMPORTING
            client                 = mo_http_client
          EXCEPTIONS
            argument_not_found     = 1
            plugin_not_active      = 2
            internal_error         = 3
            pse_not_found          = 4
            pse_not_distrib        = 5
            pse_errors             = 6
            OTHERS                 = 7 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_core_exception( ).
      ENDIF.

      mo_http_client->propertytype_logon_popup = mo_http_client->co_disabled.
      mo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.


      mo_http_client->authenticate( username = 'EWM'
                                    password = 'CWL9Z8z.JEqCj.>h' ).

*      mo_http_client->request->set_header_field( name  = if_http_form_fields_sap=>sap_client value = '100' ).

      mo_rest_client = NEW #( mo_http_client ).

*      mo_http_client->request->set_header_field(
*          name  = 'Authorization'
*          value = |Bearer Token { lv_token }| ).
*      mo_rest_client->if_rest_client~set_request_header(
*        EXPORTING
*          iv_name  = |Authorization|
*          iv_value = |Bearer { lv_token }| ).

    ENDIF.

  ENDMETHOD.


  METHOD get.
    mo_rest_client->if_rest_client~get( ).
  ENDMETHOD.


  METHOD get_api_response.
    mo_response_entity = mo_rest_client->if_rest_client~get_response_entity( ).

    ro_api_response = mo_response_entity.
  ENDMETHOD.


  METHOD get_status.
    rv_status = mo_rest_client->if_rest_client~get_status( ).
  ENDMETHOD.


  METHOD post.
    build_request_entity( iv_api_request_data ).

    mo_rest_client->if_rest_client~post( mo_request_entity ).
  ENDMETHOD.
ENDCLASS.
