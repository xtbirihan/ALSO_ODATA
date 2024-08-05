*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_MAP_HUTYPE..................................*
TABLES: ZMV_MAP_HUTYPE, *ZMV_MAP_HUTYPE. "view work areas
CONTROLS: TCTRL_ZMV_MAP_HUTYPE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_MAP_HUTYPE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_MAP_HUTYPE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_MAP_HUTYPE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_HUTYPE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_HUTYPE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_MAP_HUTYPE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_HUTYPE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_HUTYPE_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_MAP_HUTYPE               .
