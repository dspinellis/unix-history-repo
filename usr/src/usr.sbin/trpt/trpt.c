#ifndef lint
static char sccsid[] = "@(#)trpt.c	4.5 83/01/18";
#endif

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#define PRUREQUESTS
#include <sys/protosw.h>

#include <net/route.h>
#include <net/if.h>

#include <netinet/in.h>
#include <netinet/in_pcb.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_var.h>
#include <netinet/tcp.h>
#define TCPSTATES
#include <netinet/tcp_fsm.h>
#include <netinet/tcp_seq.h>
#define	TCPTIMERS
#include <netinet/tcp_timer.h>
#include <netinet/tcp_var.h>
#include <netinet/tcpip.h>
#define	TANAMES
#include <netinet/tcp_debug.h>

#include <stdio.h>
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
	int i, mask = 0;
	caddr_t tcpcb = 0;
	char *system = "/vmunix", *core = "/dev/kmem";

	argc--, argv++;
again:
	if (argc > 0 && !strcmp(*argv, "-s")) {
		sflag++, argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-p")) {
		if (argc < 1) {
			fprintf(stderr, "-p: missing tcpcb address\n");
			exit(1);
		}
		argc--, argv++;
		sscanf(*argv, "%x", &tcpcb);
		argc--, argv++;
		goto again;
	}
	if (argc > 0) {
		system = *argv;
		argc--, argv++;
		mask++;
	}
	if (argc > 0) {
		core = *argv;
		argc--, argv++;
		mask++;
	}
	(void) nlist(system, nl);
	if (nl[0].n_value == 0) {
		fprintf(stderr, "trpt: %s: no namelist\n", system);
		exit(1);
	}
	(void) close(0);
	if (open(core, 0) < 0) {
		fprintf(stderr, "trpt: "); perror(core);
		exit(2);
	}
	if (mask) {
		nl[0].n_value &= 0x7fffffff;
		nl[1].n_value &= 0x7fffffff;
	}
	(void) lseek(0, nl[1].n_value, 0);
	if (read(0, &tcp_debx, sizeof (tcp_debx)) != sizeof (tcp_debx)) {
		fprintf(stderr, "trpt: "); perror("tcp_debx");
		exit(3);
	}
	printf("tcp_debx=%d\n", tcp_debx);
	(void) lseek(0, nl[0].n_value, 0);
	if (read(0, tcp_debug, sizeof (tcp_debug)) != sizeof (tcp_debug)) {
		fprintf(stderr, "trpt: "); perror("tcp_debug");
		exit(3);
	}
	for (i = tcp_debx % TCP_NDEBUG; i < TCP_NDEBUG; i++) {
		struct tcp_debug *td = &tcp_debug[i];

		if (tcpcb && td->td_tcb != tcpcb)
			continue;
		ntime = ntohl(td->td_time);
		tcp_trace(td->td_act, td->td_ostate, td->td_tcb, &td->td_cb,
		    &td->td_ti, td->td_req);
	}
	for (i = 0; i < tcp_debx % TCP_NDEBUG; i++) {
		struct tcp_debug *td = &tcp_debug[i];

		if (tcpcb && td->td_tcb != tcpcb)
			continue;
		ntime = ntohl(td->td_time);
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
	int len, flags, win, timer;
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
		if (act == TA_OUTPUT) {
			seq = ntohl(seq);
			ack = ntohl(ack);
			len = ntohs(len);
			win = ntohs(win);
		}
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
			pf(SYN); pf(ACK); pf(FIN); pf(RST); pf(PUSH); pf(URG);
			printf(">");
		}
		break;

	case TA_USER:
		timer = req >> 8;
		req &= 0xff;
		printf("%s", prurequests[req]);
		if (req == PRU_SLOWTIMO || req == PRU_FASTTIMO)
			printf("<%s>", tcptimers[timer]);
		break;
	}
	printf(" -> %s", tcpstates[tp->t_state]);
	/* print out internal state of tp !?! */
	printf("\n");
	if (sflag) {
		printf("\trcv_nxt %x rcv_wnd %d snd_una %x snd_nxt %x snd_max %x\n",
		    tp->rcv_nxt, tp->rcv_wnd, tp->snd_una, tp->snd_nxt,
		    tp->snd_max);
		printf("\tsnd_wl1 %x snd_wl2 %x snd_wnd %x\n", tp->snd_wl1,
		    tp->snd_wl2, tp->snd_wnd);
	}
}

ptime(ms)
	int ms;
{

	printf("%03d ", (ms/10) % 1000);
}
