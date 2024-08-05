*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_MAP_CARR....................................*
TABLES: ZMV_MAP_CARR, *ZMV_MAP_CARR. "view work areas
CONTROLS: TCTRL_ZMV_MAP_CARR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_MAP_CARR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_MAP_CARR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_MAP_CARR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_CARR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_CARR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_MAP_CARR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_CARR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_CARR_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_MAP_CARR                 .
