*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_MAP_WPT.....................................*
TABLES: ZMV_MAP_WPT, *ZMV_MAP_WPT. "view work areas
CONTROLS: TCTRL_ZMV_MAP_WPT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_MAP_WPT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_MAP_WPT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_MAP_WPT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_WPT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_WPT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_MAP_WPT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_WPT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_WPT_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_MAP_WPT                  .
