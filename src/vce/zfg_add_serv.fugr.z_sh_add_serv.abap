FUNCTION z_sh_add_serv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  IF callcontrol-step <> if_ppmgco_cmn_constants=>gc_shlp_evt_presel1.
    RETURN.
  ENDIF.

  APPEND VALUE ddshselopt( shlpname = shlp-shlpname
                           shlpfield = zif_wme_c=>gs_fields-active
                           sign = wmegc_sign_inclusive
                           option = wmegc_option_eq
                           low = abap_true ) TO shlp-selopt.

ENDFUNCTION.
