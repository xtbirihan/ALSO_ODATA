CLASS zcl_crud_ztout_ship_calc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ts_ship_calc TYPE ztout_ship_calc .
    TYPES:
      tt_ship_calc TYPE STANDARD TABLE OF ts_ship_calc WITH EMPTY KEY.

    CLASS-METHODS select_by_docno
      IMPORTING
        !it_docno_r   TYPE rseloption
      EXPORTING
        !et_ship_calc TYPE zcl_crud_ztout_ship_calc=>tt_ship_calc .
    CLASS-METHODS modify
      IMPORTING
        !it_ship_calc TYPE zcl_crud_ztout_ship_calc=>tt_ship_calc
      EXPORTING
        !ev_suc       TYPE xfeld .
    CLASS-METHODS delete
      IMPORTING
        !it_docno_r TYPE rseloption
      EXPORTING
        !ev_suc     TYPE xfeld .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTOUT_SHIP_CALC IMPLEMENTATION.


  METHOD delete.

    DELETE FROM ztout_ship_calc
    WHERE docno IN @it_docno_r.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_ship_calc' sy-datum sy-uzeit.
    ELSE.
      ev_suc = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD modify.

    MODIFY ztout_ship_calc FROM TABLE it_ship_calc.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_ship_calc' sy-datum sy-uzeit.
    ELSE.
      ev_suc = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD select_by_docno.

    BREAK-POINT ID zcg_db_crud.

    SELECT *
      FROM ztout_ship_calc
      WHERE   docno IN @it_docno_r
      INTO TABLE @et_ship_calc.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztout_ship_calc' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
