/* xdr_regs.h - xdr header for 68k registers */

/*  Copyright 1992 Free Software Foundation, Inc.

    This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01a,05jun90,llk  extracted from xdr_regs.h.
*/

/* xdr structures are defined in reg.h (a bad place for them, i might add) */

bool_t xdr_regs();
bool_t xdr_ext_fp();
bool_t xdr_fp_status();
bool_t xdr_fpa_status();
bool_t xdr_fpa_long();
bool_t xdr_fpa_regs();
