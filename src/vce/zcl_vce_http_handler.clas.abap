class ZCL_VCE_HTTP_HANDLER definition
  public
  inheriting from CL_REST_HTTP_HANDLER
  final
  create public .

public section.

  methods IF_REST_APPLICATION~GET_ROOT_HANDLER
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_VCE_HTTP_HANDLER IMPLEMENTATION.


  METHOD IF_REST_APPLICATION~GET_ROOT_HANDLER.
    DATA(lo_router) = NEW cl_rest_router( ).

    SELECT * FROM ztodata_vce_http
      INTO TABLE @DATA(lt_vce_http).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    LOOP AT lt_vce_http ASSIGNING FIELD-SYMBOL(<ls_template>).
      lo_router->attach( iv_template      = |{ to_lower( <ls_template>-template ) }|
                         iv_handler_class = <ls_template>-handler_class ).
    ENDLOOP.

    ro_root_handler = lo_router.
  ENDMETHOD.
ENDCLASS.
