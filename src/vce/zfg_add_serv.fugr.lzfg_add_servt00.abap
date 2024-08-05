*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_ADD_SERV....................................*
TABLES: ZMV_ADD_SERV, *ZMV_ADD_SERV. "view work areas
CONTROLS: TCTRL_ZMV_ADD_SERV
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_ADD_SERV. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_ADD_SERV.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_ADD_SERV_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_ADD_SERV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_ADD_SERV_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_ADD_SERV_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_ADD_SERV.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_ADD_SERV_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_ADD_SERV                 .
