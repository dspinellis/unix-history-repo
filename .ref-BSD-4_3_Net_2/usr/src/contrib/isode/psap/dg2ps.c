/* dg2ps.c - datagram-backed abstraction for PStreams */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/psap/RCS/dg2ps.c,v 7.3 91/02/22 09:35:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/psap/RCS/dg2ps.c,v 7.3 91/02/22 09:35:36 mrose Interim $
 *
 *
 * $Log:	dg2ps.c,v $
 * Revision 7.3  91/02/22  09:35:36  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/07  12:40:25  mrose
 * update
 * 
 * Revision 7.1  90/08/08  14:02:31  mrose
 * stuff
 * 
 * Revision 7.0  89/11/23  22:12:34  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <stdio.h>
#include "psap.h"


struct ps_dg {
    int	    ps_fd;
    int	    ps_maxsize;

    struct ps_inout {
	struct qbuf *pio_qb;
	char   *pio_ptr;
	int	pio_cnt;

	IFP	pio_fnx;
    }	    ps_input,
	    ps_output;

    IFP	    ps_check;
};


extern	IFP	set_check_fd ();

/*  */

static int  dg_prime (ps, waiting)
register PS	ps;
int	waiting;
{
    struct qbuf *qb;
    register struct ps_dg *pt = (struct ps_dg *) ps -> ps_addr;
    register struct ps_inout *pi = &pt -> ps_input;

    switch (waiting) {
	case 0:
	    if (pi -> pio_cnt > 0)
		return OK;
	    break;

	case 1:
	default:
	    return (pi -> pio_cnt > 0 ? DONE : OK);

	case -1:
	    if (pi -> pio_cnt <= 0)
		return OK;
	    break;
    }

    if (pi -> pio_qb != NULL) {
	qb_free (pi -> pio_qb);
	pi -> pio_qb = NULL;
    }
    pi -> pio_cnt = 0;

    if (waiting < 0)
	return ps_seterr (ps, PS_ERR_EXTRA, NOTOK);

    if ((*pi -> pio_fnx) (pt -> ps_fd, &qb) == NOTOK)
	return ps_seterr (ps, PS_ERR_IO, NOTOK);

    if (pi -> pio_qb = qb)
	pi -> pio_ptr = qb -> qb_data, pi -> pio_cnt = qb -> qb_len;
    else
	pi -> pio_ptr = NULL, pi -> pio_cnt = 0;

    return OK;
}


/* ARGSUSED */
    
static int  dg_read (ps, data, n, in_line)
register PS	ps;
PElementData data;
PElementLen n;
int	in_line;
{
    int	    cc;
    register struct ps_dg *pt = (struct ps_dg *) ps -> ps_addr;
    register struct ps_inout *pi = &pt -> ps_input;

    if ((cc = pi -> pio_cnt) <= 0)
	return 0;
    if (cc > n)
	cc = n;

    bcopy (pi -> pio_ptr, (char *) data, cc);
    pi -> pio_ptr += cc, pi -> pio_cnt -= cc;

    return cc;
}


/* ARGSUSED */

static int  dg_write (ps, data, n, in_line)
register PS	ps;
PElementData data;
PElementLen n;
int	in_line;
{
    register struct ps_dg *pt = (struct ps_dg *) ps -> ps_addr;
    register struct ps_inout *po = &pt -> ps_output;

    if (po -> pio_cnt < n)
	return 0;

    bcopy ((char *) data, po -> pio_ptr, n);
    po -> pio_ptr += n, po -> pio_cnt -= n;

    return n;
}


static int  dg_flush (ps)
register PS	ps;
{
    register struct ps_dg *pt = (struct ps_dg *) ps -> ps_addr;
    register struct ps_inout *po = &pt -> ps_output;
    register struct qbuf *qb = po -> pio_qb;

    qb -> qb_len = po -> pio_ptr - qb -> qb_data;
    if ((*po -> pio_fnx) (pt -> ps_fd, qb) != qb -> qb_len)
	return ps_seterr (ps, PS_ERR_IO, NOTOK);

    po -> pio_ptr = qb -> qb_data, po -> pio_cnt = pt -> ps_maxsize;

    return OK;
}


static int  dg_close (ps)
register PS	ps;
{
    register struct ps_dg *pt = (struct ps_dg *) ps -> ps_addr;

    if (pt == NULL)
	return OK;

    if (pt -> ps_input.pio_qb)
	qb_free (pt -> ps_input.pio_qb);
    if (pt -> ps_output.pio_qb)
	qb_free (pt -> ps_output.pio_qb);

    (void) set_check_fd (pt -> ps_fd, NULLIFP, NULLCP);

    free ((char *) pt);

    return OK;
}


static int  dg_check (fd, data)
int	fd;
caddr_t	data;
{
    int	    n;
    PS	    ps = (PS) data;
    register struct ps_dg *pt = (struct ps_dg *) ps -> ps_addr;

    if (pt -> ps_check && (n = (*pt -> ps_check) (fd)))
	return n;

    return (ps_prime (ps, 1) > 0 ? DONE : OK);
}

/*  */

int	dg_open (ps)
register PS	ps;
{
    ps -> ps_primeP = dg_prime;
    ps -> ps_readP = dg_read;
    ps -> ps_writeP = dg_write;
    ps -> ps_flushP = dg_flush;
    ps -> ps_closeP = dg_close;

    return OK;
}


int	dg_setup (ps, fd, size, rfx, wfx, cfx)
register PS	ps;
int	fd,
	size;
IFP	rfx,
	wfx,
	cfx;
{
    register struct ps_dg *pt;
    register struct ps_inout *po;
    register struct qbuf *qb;

    if ((pt = (struct ps_dg *) calloc (1, sizeof *pt)) == NULL)
	return ps_seterr (ps, PS_ERR_NMEM, NOTOK);
    ps -> ps_addr = (caddr_t) pt;

    pt -> ps_fd = fd;
    pt -> ps_maxsize = size;

    if ((qb = (struct qbuf *) malloc (sizeof *qb
				          + (unsigned) pt -> ps_maxsize))
	    == NULL)
	return ps_seterr (ps, PS_ERR_NMEM, NOTOK);
    qb -> qb_forw = qb -> qb_back = qb;
    qb -> qb_len = 0;
    qb -> qb_data = qb -> qb_base;

    po = &pt -> ps_output;
    po -> pio_qb = qb;
    po -> pio_ptr = qb -> qb_data, po -> pio_cnt = pt -> ps_maxsize;
    if ((pt -> ps_input.pio_fnx = rfx) == NULLIFP
	    || (po -> pio_fnx = wfx) == NULLIFP)
	return ps_seterr (ps, PS_ERR_XXX, NOTOK);

    pt -> ps_check = cfx;
    (void) set_check_fd (fd, dg_check, (caddr_t) ps);

    return OK;
}
