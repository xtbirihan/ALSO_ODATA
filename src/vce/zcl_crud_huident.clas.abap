class ZCL_CRUD_HUIDENT definition
  public
  final
  create public .

public section.

  class-methods SELECT_BY_DOCID_HUIDTYP
    importing
      !IT_DOCID_R type RSELOPTION
      !IT_HUIDTYPE_R type RSELOPTION
    returning
      value(RV_SUC) type XFELD .
  class-methods SELECT_BY_HUID_HUIDTYP
    importing
      !IT_HUID_R type RSELOPTION
      !IT_HUIDTYPE_R type RSELOPTION
    exporting
      !ET_HUIDENT type /SCDL/DL_DB_HUIDENT_TAB
    returning
      value(RV_SUC) type XFELD .
  class-methods INSERT
    importing
      !IT_HUIDENT type /SCDL/DL_DB_HUIDENT_TAB
    exporting
      !EV_RC type I .
  class-methods SELECT_BY_HUIDTYPE
    importing
      !IV_HUIDTYPE type /SCDL/DL_HUIDTYPE
    returning
      value(RT_HUIDENT) type /SCDL/DL_DB_HUIDENT_TAB .
  class-methods DELETE_BY_HUID_HUIDTYP
    importing
      !IT_HUIDTYPE_R type RSELOPTION
      !IT_HUID_R type RSELOPTION
    returning
      value(RV_SUC) type XFELD .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_HUIDENT IMPLEMENTATION.


  METHOD delete_by_huid_huidtyp.
********************************************************************
*& Key          : <AAHMEDOV>-Oct 29, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF it_huid_r IS INITIAL.
      RETURN.
    ENDIF.

    DELETE FROM /scdl/db_huident
     WHERE huid IN @it_huid_r
     AND huidtype IN @it_huidtype_r.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/scdl/db_huident' sy-datum sy-uzeit.
    ENDIF.

    rv_suc = COND #( WHEN sy-subrc EQ 0
                     THEN abap_true ).

  ENDMETHOD.


  METHOD insert.
********************************************************************
*& Key          : <AHMEDOV>-Oct 29, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF it_huident IS INITIAL.
      RETURN.
    ENDIF.

    INSERT /scdl/db_huident
    FROM TABLE it_huident ACCEPTING DUPLICATE KEYS.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/scdl/db_huident' sy-datum sy-uzeit.
    ENDIF.

    ev_rc = sy-subrc.

  ENDMETHOD.


  METHOD select_by_docid_huidtyp.
********************************************************************
*& Key          : <AHMEDOV>-Oct 29, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF it_docid_r IS INITIAL OR
      it_huidtype_r IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM /scdl/db_huident
      INTO TABLE @DATA(lt_huident)
     WHERE docid IN @it_docid_r
      AND huidtype IN @it_huidtype_r.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/scdl/db_huident' sy-datum sy-uzeit.
    ENDIF.

    rv_suc = COND #( WHEN sy-subrc EQ 0
                     THEN abap_true ).

  ENDMETHOD.


  METHOD select_by_huidtype.
********************************************************************
*& Key          : <AHMEDOV>-Dec 8, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF iv_huidtype IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM /scdl/db_huident
      INTO TABLE @DATA(lt_huident)
     WHERE huidtype = @iv_huidtype.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/scdl/db_huident' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_by_huid_huidtyp.
********************************************************************
*& Key          : <AHMEDOV>-Oct 29, 2023
*& Request No.  :
********************************************************************
*& Description
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF it_huid_r IS INITIAL OR
      it_huidtype_r IS INITIAL.
      RETURN.
    ENDIF.

    SELECT * FROM /scdl/db_huident
      INTO TABLE @DATA(lt_huident)
     WHERE huid IN @it_huid_r
      AND huidtype IN @it_huidtype_r.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/scdl/db_huident' sy-datum sy-uzeit.
    ELSE.
      IF et_huident IS REQUESTED.
        et_huident = lt_huident.
      ENDIF.
    ENDIF.

    rv_suc = COND #( WHEN sy-subrc EQ 0
                     THEN abap_true ).

  ENDMETHOD.
ENDCLASS.
