#include <stdio.h>
#define TPPT

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/types.h>
#include <sys/time.h>

#include <netiso/tp_param.h>
#include <netiso/tp_timer.h>
#include <netiso/tp_stat.h>
#include <netiso/tp_param.h>
#include <netiso/tp_ip.h>
#include <netiso/tp_pcb.h>
#include <netiso/tp_tpdu.h>
#include <netiso/argo_debug.h>
#include <netiso/tp_trace.h>

static tp_seq = 0;
u_char tp_traceflags[128];

main(argc, argv)
{
	
}

#ifdef notdef

tpprint(tp)
	register struct tp_Trace *tp;
{
/*tptrace(tpcb, event, arg, src, len, arg4, arg5)*/
	struct tp_pcb	*tpcb;
	u_int 			event, arg;
	u_int	 		src;
	u_int	 		len; 
	u_int	 		arg4;
	u_int	 		arg5;

	tp->tpt_event = event;
	tp->tpt_tseq = tp_seq++;
	tp->tpt_arg = arg;
	if(tpcb)
		tp->tpt_arg2 = tpcb->tp_lref;
	bcopy( (caddr_t)&time, (caddr_t)&tp->tpt_time, sizeof(struct timeval) );

	switch(tp->tpt_event) {

	case TPPTertpdu:
		bcopy((caddr_t)src, (caddr_t)&tp->tpt_ertpdu,
			(unsigned)MIN((int)len, sizeof(struct tp_Trace)));
		break;

	case TPPTusrreq:
	case TPPTmisc:

		printf(tp->tpt_str,
			tp->tpt_m2/* = src */,
			tp->tpt_m3/* = len */,
			tp->tpt_m4/* = arg4 */,
			tp->tpt_m1/* = arg5 */);

	case TPPTgotXack: 
	case TPPTXack: 
	case TPPTsendack: 
	case TPPTgotack: 
	case TPPTack: 
	case TPPTindicate: 
	default:
	case TPPTdriver: 
		tp->tpt_m2 = arg; 
		tp->tpt_m3 = src;
		tp->tpt_m4 = len;
		tp->tpt_m5 = arg4;
		tp->tpt_m1 = arg5; 
		break;
	case TPPTparam:
		bcopy((caddr_t)src, (caddr_t)&tp->tpt_param, sizeof(struct tp_param));
		break;
	case TPPTref:
		bcopy((caddr_t)src, (caddr_t)&tp->tpt_ref, sizeof(struct tp_ref));
		break;

	case TPPTtpduin:
	case TPPTtpduout:
		tp->tpt_arg2 = arg4;
		bcopy((caddr_t)src, (caddr_t)&tp->tpt_tpdu,
		      (unsigned)MIN((int)len, sizeof(struct tp_Trace)));
		break;
	}
}
#endif

char tpdu_types[][4] = {
"#0", "XPD", "XAK", "GR", "#4", "#5", "AK", "ER",
"DR", "#9", "#10", "#11", "DC", "CC", "CR", "DT"};

p_tpdu(tpdu, xtype)
struct tpdu *tpdu;
{
	struct tpdu t = *tpdu;
	int type = t.tpdu_type;

	printf("<(%d)%s:", t.tpdu_li, tpdu_types[type]);
	switch (type) {
	case DT_TPDU_type:
	case XPD_TPDU_type:
		printf("to %x cdt %d seq %d eot %d\>", t.tpdu_dref, t.tpdu_cdt
			xtype ? t.tpdu_DTseqX : t.tpdu_DTseq,
			xtype ? t.tpdu_DTeotX : t.tpdu_DTeot);
		break;
	}
}
