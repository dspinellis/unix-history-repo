/* states5.c - VTPM: FSM sector 5 states */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/states5.c,v 7.1 91/02/22 09:48:20 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/states5.c,v 7.1 91/02/22 09:48:20 mrose Interim $
 *
 *
 * $Log:	states5.c,v $
 * Revision 7.1  91/02/22  09:48:20  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:47  mrose
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


#include "vtpm.h"

#define	undefined(s1, e1) \
	adios (NULLCP, \
	      "undefined state/event: sector is 5, state is %s, event is %d", \
	       s1, e1)

int
s5_400B(event, pe)			/* sector 5, state 400B	*/
	int	event;
	PE	pe;
{
	switch (event) {
	case DLQ:
		return(a5_35(pe));
	case NDQ_ntr:
		return(a5_3(pe));
	case NDQ_tr:
		return(a5_2(pe));
	case UDQ:
		return(a5_34(pe));
	case HDQ:
		return(a5_106(pe));
	case VDATreq_h:
		return(a5_11(pe));
	case VDATreq_u:
		return(a5_28(pe));
	case RLQ:
		return(a5_38(pe));
	case BKQ:
		return(a5_32(pe));
	case VDATreq_n:
		return(a5_1(pe));
	case VDELreq:
		return(a5_9(pe));
	case VRELreq:
		return(a5_17(pe));
	case VBRKreq:
		return(a5_5(pe));
	default:
		undefined ("400B", event); /* NOTREACHED */
	}
}

/* ARGSUSED */
int
s5_402B(event, pe)
	int	event;
	PE	pe;
{
		undefined ("402B", event); /* NOTREACHED */
}

/* ARGSUSED */
int
s5_420B(event, pe)
	int	event;
	PE	pe;
{
		undefined ("420B", event); /* NOTREACHED */
}

/* ARGSUSED */
int
s5_422B(event, pe)			/* sector 5, state 422B	*/
	int	event;
	PE	pe;
{
		undefined ("422B", event); /* NOTREACHED */
}

/* ARGSUSED */
int
s5_40N(event, pe)
	int	event;
	PE	pe;
{
		undefined ("40N", event); /* NOTREACHED */
}

/* ARGSUSED */
int
s5_40T(event, pe)
	int	event;
	PE	pe;
{
		undefined ("40T", event); /* NOTREACHED */
}

/* ARGSUSED */
int
s5_42T(event, pe)
	int	event;
	PE	pe;
{
		undefined ("42T", event); /* NOTREACHED */
}

/* ARGSUSED */
int
s5_42N(event, pe)
	int	event;
	PE	pe;
{
		undefined ("42N", event); /* NOTREACHED */
}


s5_61(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case BKR:
		return(a5_31(pe));
	default:
		undefined ("61", event); /* NOTREACHED */
	}
}
int
s5_62(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case VBRKrsp:
		return(a5_6(pe));
	default:
		undefined ("62", event); /* NOTREACHED */
	}
}
