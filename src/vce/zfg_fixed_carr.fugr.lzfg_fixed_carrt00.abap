*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_FIXED_CARR..................................*
TABLES: ZMV_FIXED_CARR, *ZMV_FIXED_CARR. "view work areas
CONTROLS: TCTRL_ZMV_FIXED_CARR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_FIXED_CARR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_FIXED_CARR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_FIXED_CARR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_FIXED_CARR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_FIXED_CARR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_FIXED_CARR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_FIXED_CARR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_FIXED_CARR_TOTAL.

*.........table declarations:.................................*
TABLES: ZTOUT_FIXED_CARR               .
