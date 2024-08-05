*&---------------------------------------------------------------------*
*& Include          ZFG_SHIP_CARR_MON_SEL
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : <AAHMEDOV>-140723
*& Request No.   : GAP 12 - PGAP-012_FS_Outbound_VCE_carrier_software_integration
**********************************************************************

TABLES: zstr_mon_ship_calc.

SELECTION-SCREEN: BEGIN OF SCREEN 110.
  SELECT-OPTIONS: so_docno FOR zstr_mon_ship_calc-docno.
SELECTION-SCREEN END OF SCREEN 110.
