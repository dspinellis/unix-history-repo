/* xdr_regs.c - xdr routines for 68k registers */

/* Copyright 1984, 1985, 1986, 1987, 1988, 1989, 1992 Free Software Foundation, Inc.

   This code was donated by Wind River Systems, Inc. */

/*
DESCRIPTION
This module contains the eXternal Data Representation (XDR) routines
for the GDB interface for VxWorks.
*/

#include "vxWorks.h"
#include <rpc/rpc.h>
#include "reg.h"
#include "xdr_regs.h"


#ifdef I80960
/*******************************************************************************
*
* xdr_regs - xdr routine for i960 registers
*/

bool_t xdr_regs (xdrs, objp)
    XDR *xdrs;
    struct regs *objp;

    {
    if (! xdr_opaque(xdrs, (char *) objp->r_lreg, 16 * sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) objp->r_greg, 16 * sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) &objp->r_pcw, sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) &objp->r_acw, sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) &objp->r_tcw, sizeof(int)))
	return(FALSE);

    return(TRUE);
    }

/*******************************************************************************
*
* xdr_fp_status - xdr routine for i960 floating point registers
*/

bool_t xdr_fp_status (xdrs, objp)
    XDR *xdrs;
    struct fp_status *objp;

    {
    unsigned int size = 4 * FP_REG_SIZE;

    /* We use xdr_bytes to indicate how many bytes of FP regs there are! */
    if (! xdr_bytes (xdrs, (char *) objp->fps_regs, &size, 4 * FP_REG_SIZE))
	return (FALSE);
    return (TRUE);
    }

/*******************************************************************************
*
* xdr_ext_fp - xdr for a single fp register
*/

bool_t xdr_ext_fp (xdrs, objp)
    XDR *xdrs;
    char *objp;

    {
    unsigned int size = FP_REG_SIZE;

    if (! xdr_bytes (xdrs, objp, &size, FP_REG_SIZE)) 
	return(FALSE);

    return(TRUE);
    }
#else	/* Must be 68K if it isn't i960 -- for now.  FIXME!  */

/*******************************************************************************
*
* xdr_regs - xdr routine for 68k registers
*/

bool_t xdr_regs (xdrs, objp)
    XDR *xdrs;
    struct regs *objp;

    {
    if (! xdr_opaque(xdrs, (char *) objp->r_dreg, 8 * sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) objp->r_areg, 8 * sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) &objp->r_sr, sizeof(int)))
	return(FALSE);
    if (! xdr_opaque(xdrs, (char *) &objp->r_pc, sizeof(int)))
	return(FALSE);

    return(TRUE);
    }

/*******************************************************************************
*
* xdr_ext_fp - xdr for a single fp register
*/

bool_t xdr_ext_fp (xdrs, objp)
    XDR *xdrs;
    ext_fp *objp;

    {
    if (! xdr_vector(xdrs, (char *) objp->fp, 3, sizeof(int), xdr_int)) 
	return(FALSE);

    return(TRUE);
    }
/*******************************************************************************
*
* xdr_fp_status - xdr routine for floating point registers
*/

bool_t xdr_fp_status (xdrs, objp)
    XDR *xdrs;
    struct fp_status *objp;

    {
    if (! xdr_vector (xdrs, (char *) objp->fps_regs, 8, 
		sizeof(ext_fp), xdr_ext_fp))
	return (FALSE);
    if (! xdr_int (xdrs, &objp->fps_control)) 
	return (FALSE);
    if (! xdr_int (xdrs, &objp->fps_status)) 
	return (FALSE);
    if (! xdr_int (xdrs, &objp->fps_iaddr)) 
	return (FALSE);
    if (! xdr_int (xdrs, &objp->fps_code)) 
	return (FALSE);
    if (! xdr_int (xdrs, &objp->fps_flags)) 
	return (FALSE);

    return (TRUE);
    }
/*******************************************************************************
*
* xdr_fpa_status - xdr for fpa status
*/

bool_t xdr_fpa_status (xdrs, objp)
    XDR *xdrs;
    struct fpa_status *objp;

    {
    if (! xdr_u_int (xdrs, &objp->fpas_state)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_imask)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_load_ptr)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_ierr)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_act_instr)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_nxt_instr)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_act_d1half)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_act_d2half)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_nxt_d1half)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_nxt_d2half)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_mode3_0)) 
	return (FALSE);
    if (! xdr_u_int (xdrs, &objp->fpas_wstatus)) 
	return (FALSE);

    return (TRUE);
    }
/*******************************************************************************
*
* xdr_fpa_long - xdr for fpa data register
*/

bool_t xdr_fpa_long (xdrs,objp)
    XDR *xdrs;
    fpa_long *objp;

    {
    if (! xdr_vector (xdrs, (char *) objp->fpl_data, 2, sizeof(int), xdr_int)) 
	return (FALSE);

    return (TRUE);
    }
/*******************************************************************************
*
* xdr_fpa_regs - xdr for fpa_regs 
*/

bool_t xdr_fpa_regs (xdrs, objp)
    XDR *xdrs;
    struct fpa_regs *objp;

    {
    if (! xdr_u_int (xdrs, &objp->fpar_flags)) 
	return (FALSE);
    if (! xdr_fpa_status (xdrs, &objp->fpar_status)) 
	return (FALSE);
    if (! xdr_vector (xdrs, (char *) objp->fpar_data, 
    		    FPA_NDATA_REGS, sizeof(fpa_long), xdr_fpa_long)) 
	return (FALSE);

    return (TRUE);
    }

#endif /* I80960 */

