/* states1.c - VTPM: FSM sector 1 states */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/states1.c,v 7.1 91/02/22 09:48:19 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/states1.c,v 7.1 91/02/22 09:48:19 mrose Interim $
 *
 *
 * $Log:	states1.c,v $
 * Revision 7.1  91/02/22  09:48:19  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:46  mrose
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

#define	undefined(s1,e1) \
	adios (NULLCP, \
	      "undefined state/event: sector is 1, state is %s, event is %d", \
	       s1, e1)

int
s1_01(event, pe)			/* sector 1, state 01	*/
	int	event;
	PE	pe;
{
	switch (event) {
	case ASQ:
		return(a1_17(pe));
	case VASSreq:
		return(a1_2(pe));
	case APQ:
		return(a1_107(pe));
	case AUQ:
		return(a1_107(pe));
	case PAB:
		return(a1_107(pe));
	case VTAB:
		return(a1_107(pe));
	case VUABreq:
		return(a1_107(pe));
	default:
		undefined ("01", event); /* NOTREACHED */
	}
}

int
s1_02B(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case ASR:
		return(a1_15(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK);	/* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("02B", event); /* NOTREACHED */
	}
}

int
s1_02S(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case ASR:
		return(a1_16(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK);	/* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("02S", event); /* NOTREACHED */
	}
}

int
s1_03B(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case VASSrsp:
		return(a1_3(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("03B", event); /* NOTREACHED */
	}
}

int
s1_03S(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case VASSrsp:
		return(a1_4(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("03S", event); /* NOTREACHED */
	}
}

int
s1_10B(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case GTQ:
		return(a1_107(pe));
	case RLQ:
		return(a1_25(pe));
	case RTQ:
		return(a1_5(pe));
	case VRELreq:
		return(a1_7(pe));
	case SNQ:
		return(a1_29(pe));
	case SPQ:
		return(a1_30(pe));
	case VSNEGreq:
		return(a1_11(pe));
	case VSWPreq:
		return(a1_13(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("10B", event); /* NOTREACHED */
	}
}

int
s1_10N(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case GTQ:
		return(a1_19(pe));
	case RLQ:
		return(a1_26(pe));
	case SNQ:
		return(a1_28(pe));
	case SPQ:
		return(a1_30(pe));
	case VRQTreq:
		return(a1_10(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("10N", event); /* NOTREACHED */
	}
}

int
s1_10T(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case RTQ:
		return(a1_27(pe));
	case VGVTreq:
		return(a1_6(pe));
	case VRELreq:
		return(a1_8(pe));
	case VRQTreq:
		return(a1_19(pe));
	case VSNEGreq:
		return(a1_12(pe));
	case VSWPreq:
		return(a1_14(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("10T", event); /* NOTREACHED */
	}
}

int
s1_50B(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case DLQ:
		return(a1_21(pe));
	case GTQ:
		return(a1_7(pe));
	case NDQ_ntr:
		return(a1_0(pe));
	case NDQ_tr:
		return(a1_1(pe));
	case RLQ:
		return(a1_22(pe));
	case SNQ:
		return(a1_23(pe));
	case SPQ:
		return(a1_24(pe));
	case UDQ:
		return(a1_18(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("50B", event); /* NOTREACHED */
	}
}


int
s1_51Q(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case DLQ:
		return(a1_21(pe));
	case NDQ_ntr:
		return(a1_0(pe));
	case NDQ_tr:
		return(a1_1(pe));
	case RLR:
		return(a1_20(pe));
	case RTQ:
		return(a1_107(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("51Q", event); /* NOTREACHED */
	}
}

int
s1_51R(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case VRELrsp:
		return(a1_9(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("51R", event); /* NOTREACHED */
	}
}

int
s1_51N(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case VRELrsp:
		return(a1_9(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("51N", event); /* NOTREACHED */
	}
}

int
s1_51T(event, pe)
	int	event;
	PE	pe;
{
	switch (event) {
	case RLR:
		return(a1_20(pe));
	case RTQ:
		return(a1_0(pe));
	case UDQ:
		return(a1_18(pe));
	case APQ:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case AUQ:
		a1_101(pe);
		return(OK);	/* NOTREACHED	*/
	case PAB:
		a1_100(pe);
		return(OK); /*	NOTREACHED	*/
	case VTAB:
		a1_103(pe);
		return(OK); /* NOTREACHED */
	case VUABreq:
		a1_102(pe);
		return(OK);	/* NOTREACHED */
	default:
		undefined ("51T", event); /* NOTREACHED */
	}
}
