#ifndef lint
static char sccsid[] = "@(#)trpt.c	4.1 82/04/02";
#endif

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#define PRUREQUESTS
#include <sys/protosw.h>
#include <net/in.h>
#include <net/route.h>
#include <net/in_pcb.h>
#include <net/in_systm.h>
#include <net/if.h>
#include <net/ip.h>
#include <net/ip_var.h>
#include <net/tcp.h>
#define TCPSTATES
#include <net/tcp_fsm.h>
#include <net/tcp_seq.h>
#define	TCPTIMERS
#include <net/tcp_timer.h>
#include <net/tcp_var.h>
#include <net/tcpip.h>
#define	TANAMES
#include <net/tcp_debug.h>
#include <errno.h>

#include <nlist.h>

n_time	ntime;
int	sflag;
struct	nlist nl[] = {
	{ "_tcp_debug" },
	{ "_tcp_debx" },
	0
};
struct	tcp_debug tcp_debug[TCP_NDEBUG];
int	tcp_debx;

main(argc, argv)
	int argc;
	char **argv;
{
	int i;

	argc--, argv++;
again:
	if (argc > 0 && !strcmp(*argv, "-s")) {
		sflag++, argc--, argv++;
		goto again;
	}
	nlist(argc > 0 ? *argv : "/vmunix", nl);
	if (nl[0].n_value == 0) {
		printf("no namelist\n");
		exit(1);
	}
	close(0);
	open(argc > 1 ? argv[1] : "/dev/kmem", 0);
	if (argc > 1) {
		nl[0].n_value &= 0x7fffffff;
		nl[1].n_value &= 0x7fffffff;
	}
	lseek(0, nl[1].n_value, 0);
	read(0, &tcp_debx, sizeof (tcp_debx));
	printf("tcp_debx=%d\n", tcp_debx);
	lseek(0, nl[0].n_value, 0);
	read(0, tcp_debug, sizeof (tcp_debug));
	for (i = tcp_debx % TCP_NDEBUG; i < TCP_NDEBUG; i++) {
		struct tcp_debug *td = &tcp_debug[i];
		ntime = td->td_time;
#if vax
		ntime = ntohl(ntime);
#endif
		tcp_trace(td->td_act, td->td_ostate, td->td_tcb, &td->td_cb,
		    &td->td_ti, td->td_req);
	}
	for (i = 0; i < tcp_debx % TCP_NDEBUG; i++) {
		struct tcp_debug *td = &tcp_debug[i];
		ntime = td->td_time;
#if vax
		ntime = ntohl(ntime);
#endif
		tcp_trace(td->td_act, td->td_ostate, td->td_tcb, &td->td_cb,
		    &td->td_ti, td->td_req);
	}
	exit(0);
}

/*
 * Tcp debug routines
 */
tcp_trace(act, ostate, atp, tp, ti, req)
	short act, ostate;
	struct tcpcb *atp, *tp;
	struct tcpiphdr *ti;
	int req;
{
	tcp_seq seq, ack;
	int len, flags, win;
	char *cp;

	ptime(ntime);
	printf("%x %s:%s ", ((int)atp)&0xfffff,
	    tcpstates[ostate], tanames[act]);
	switch (act) {

	case TA_INPUT:
	case TA_OUTPUT:
		seq = ti->ti_seq;
		ack = ti->ti_ack;
		len = ti->ti_len;
		win = ti->ti_win;
#if vax
		if (act == TA_OUTPUT) {
			seq = ntohl(seq);
			ack = ntohl(ack);
			len = ntohs(len);
			win = ntohs(win);
		}
#endif
		if (act == TA_OUTPUT)
			len -= sizeof (struct tcphdr);
		if (len)
			printf("[%x..%x)", seq, seq+len);
		else
			printf("%x", seq);
		printf("@%x", ack);
		if (win)
			printf("(win=%d)", win);
		flags = ti->ti_flags;
		if (flags) {
			char *cp = "<";
#define pf(f) { if (ti->ti_flags&TH_/**/f) { printf("%s%s", cp, "f"); cp = ","; } }
			pf(SYN); pf(ACK); pf(FIN); pf(RST); pf(URG);
			printf(">");
		}
		break;

	case TA_USER:
		printf("%s", prurequests[req&0xff]);
		if (req >> 8)
			printf("<%s>", tcptimers[req>>8]);
		break;
	}
	printf(" -> %s", tcpstates[tp->t_state]);
	/* print out internal state of tp !?! */
	printf("\n");
	if (sflag) {
		printf("\trcv_nxt %x rcv_wnd %d snd_una %x snd_nxt %x snd_max %x\n",
		    tp->rcv_nxt, tp->rcv_wnd, tp->snd_una, tp->snd_nxt, tp->snd_max);
		printf("\tsnd_wl1 %x snd_wl2 %x snd_wnd %x\n", tp->snd_wl1, tp->snd_wl2, tp->snd_wnd);
	}
}

ptime(ms)
	int ms;
{

	printf("%03d ", (ms/10) % 1000);
}
