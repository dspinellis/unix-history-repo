/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/*
 * $Header: tp_meas.c,v 5.2 88/11/18 17:28:04 nhall Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_meas.c,v $
 * 
 * tp_meas.c : create a performance measurement event
 * in the circular buffer tp_Meas[]
 */

#ifndef lint
static char *rcsid = "$Header: tp_meas.c,v 5.2 88/11/18 17:28:04 nhall Exp $";
#endif lint

#include "types.h"
#include "time.h"
#include "../netiso/tp_meas.h"

extern struct timeval time;

#ifdef TP_PERF_MEAS
int		tp_Measn = 0;
struct tp_Meas tp_Meas[TPMEASN];

/*
 * NAME:	 tpmeas()
 *
 * CALLED FROM: tp_emit(), tp_soisdisconecting(), tp_soisdisconnected()
 *	tp0_stash(), tp_stash(), tp_send(), tp_goodack(), tp_usrreq()
 *
 * FUNCTION and ARGUMENTS:
 *  stashes a performance-measurement event for the given reference (ref)
 *  (kind) tells which kind of event, timev is the time to be stored
 *  with this event, (seq), (win), and (size) are integers that usually
 *  refer to the sequence number, window number (on send) and 
 *  size of tpdu or window.
 *
 * RETURNS:		Nada
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
void
tpmeas(ref, kind, timev, seq, win, size)
	u_int 	ref;
	u_int	kind;
	struct 	timeval *timev;
	u_int	seq, win, size;
{
	register struct tp_Meas *tpm;
	static int mseq;

	tpm = &tp_Meas[tp_Measn++];
	tp_Measn %= TPMEASN;

	tpm->tpm_kind = kind;
	tpm->tpm_tseq = mseq++;
	tpm->tpm_ref = ref;
	if(kind == TPtime_from_ll)
		bcopy((caddr_t)timev, (caddr_t)&tpm->tpm_time, sizeof(struct timeval));
	else
		bcopy( (caddr_t)&time, 
			(caddr_t)&tpm->tpm_time, sizeof(struct timeval) );
	tpm->tpm_seq = seq;
	tpm->tpm_window = win;
	tpm->tpm_size = size;
}

#endif TP_PERF_MEAS
