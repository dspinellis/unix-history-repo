/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)trsp.c	6.2 (Berkeley) 7/1/88";
#endif /* not lint */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#define PRUREQUESTS
#include <sys/protosw.h>

#include <net/route.h>
#include <net/if.h>

#define TCPSTATES
#include <netinet/tcp_fsm.h>
#define	TCPTIMERS
#include <netinet/tcp_timer.h>

#include <netns/ns.h>
#include <netns/ns_pcb.h>
#include <netns/idp.h>
#include <netns/idp_var.h>
#include <netns/sp.h>
#include <netns/spidp.h>
#include <netns/spp_var.h>
#define SANAMES
#include <netns/spp_debug.h>

#include <stdio.h>
#include <errno.h>
#include <nlist.h>

unsigned long	ntime;
int	sflag;
int	tflag;
int	jflag;
int	aflag;
int	zflag;
int	numeric();
struct	nlist nl[] = {
	{ "_spp_debug" },
	{ "_spp_debx" },
	0
};
struct	spp_debug spp_debug[SPP_NDEBUG];
caddr_t	spp_pcbs[SPP_NDEBUG];
int	spp_debx;

main(argc, argv)
	int argc;
	char **argv;
{
	int i, mask = 0, npcbs = 0;
	char *system = "/vmunix", *core = "/dev/kmem";

	argc--, argv++;
again:
	if (argc > 0 && !strcmp(*argv, "-a")) {
		aflag++, argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-z")) {
		zflag++, argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-s")) {
		sflag++, argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-t")) {
		tflag++, argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-j")) {
		jflag++, argc--, argv++;
		goto again;
	}
	if (argc > 0 && !strcmp(*argv, "-p")) {
		argc--, argv++;
		if (argc < 1) {
			fprintf(stderr, "-p: missing sppcb address\n");
			exit(1);
		}
		if (npcbs >= SPP_NDEBUG) {
			fprintf(stderr, "-p: too many pcb's specified\n");
			exit(1);
		}
		sscanf(*argv, "%x", &spp_pcbs[npcbs++]);
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
		fprintf(stderr, "trsp: %s: no namelist\n", system);
		exit(1);
	}
	(void) close(0);
	if (open(core, 0) < 0) {
		fprintf(stderr, "trsp: "); perror(core);
		exit(2);
	}
	if (mask) {
		nl[0].n_value &= 0x7fffffff;
		nl[1].n_value &= 0x7fffffff;
	}
	(void) lseek(0, nl[1].n_value, 0);
	if (read(0, &spp_debx, sizeof (spp_debx)) != sizeof (spp_debx)) {
		fprintf(stderr, "trsp: "); perror("spp_debx");
		exit(3);
	}
	printf("spp_debx=%d\n", spp_debx);
	(void) lseek(0, nl[0].n_value, 0);
	if (read(0, spp_debug, sizeof (spp_debug)) != sizeof (spp_debug)) {
		fprintf(stderr, "trsp: "); perror("spp_debug");
		exit(3);
	}
	/*
	 * Here, we just want to clear out the old trace data and start over.
	 */
	if (zflag) {
		char *cp = (char *) spp_debug,
		     *cplim = cp + sizeof(spp_debug);
		(void) close(0);
		if (open(core, 2) < 0) {
			fprintf(stderr, "trsp: "); perror(core);
			exit(2);
		}
		while(cp < cplim) *cp++ = 0;
		(void) lseek(0, nl[0].n_value, 0);
		if (write(0, spp_debug, sizeof (spp_debug)) != sizeof (spp_debug)) {
			fprintf(stderr, "trsp: "); perror("spp_debug");
			exit(3);
		}
		(void) lseek(0, nl[1].n_value, 0);
		spp_debx = 0;
		if (write(0, &spp_debx, sizeof (spp_debx)) != sizeof (spp_debx)) {
			fprintf(stderr, "trsp: "); perror("spp_debx");
			exit(3);
		}
		exit(0);
	}
	/*
	 * If no control blocks have been specified, figure
	 * out how many distinct one we have and summarize
	 * them in spp_pcbs for sorting the trace records
	 * below.
	 */
	if (npcbs == 0) {
		for (i = 0; i < SPP_NDEBUG; i++) {
			register int j;
			register struct spp_debug *sd = &spp_debug[i];

			if (sd->sd_cb == 0)
				continue;
			for (j = 0; j < npcbs; j++)
				if (spp_pcbs[j] == sd->sd_cb)
					break;
			if (j >= npcbs)
				spp_pcbs[npcbs++] = sd->sd_cb;
		}
	}
	qsort(spp_pcbs, npcbs, sizeof (caddr_t), numeric);
	if (jflag) {
		char *cp = "";

		for (i = 0; i < npcbs; i++) {
			printf("%s%x", cp, spp_pcbs[i]);
			cp = ", ";
		}
		if (*cp)
			putchar('\n');
		exit(0);
	}
	for (i = 0; i < npcbs; i++) {
		printf("\n%x:\n", spp_pcbs[i]);
		dotrace(spp_pcbs[i]);
	}
	exit(0);
}

dotrace(sppcb)
	register caddr_t sppcb;
{
	register int i;
	register struct spp_debug *sd;

	for (i = spp_debx % SPP_NDEBUG; i < SPP_NDEBUG; i++) {
		sd = &spp_debug[i];
		if (sppcb && sd->sd_cb != sppcb)
			continue;
		ntime = ntohl(sd->sd_time);
		spp_trace(sd->sd_act, sd->sd_ostate, sd->sd_cb, &sd->sd_sp,
		    &sd->sd_si, sd->sd_req);
	}
	for (i = 0; i < spp_debx % SPP_NDEBUG; i++) {
		sd = &spp_debug[i];
		if (sppcb && sd->sd_cb != sppcb)
			continue;
		ntime = ntohl(sd->sd_time);
		spp_trace(sd->sd_act, sd->sd_ostate, sd->sd_cb, &sd->sd_sp,
		    &sd->sd_si, sd->sd_req);
	}
}

ptime(ms)
	int ms;
{

	printf("%03d ", (ms/10) % 1000);
}

numeric(c1, c2)
	caddr_t *c1, *c2;
{
	
	return (*c1 - *c2);
}

spp_trace(act, ostate, asp, sp, si, req)
	short act, ostate;
	struct sppcb *asp, *sp;
	struct spidp *si;
	int req;
{
	u_short seq, ack, len, alo;
	int flags, timer;
	char *cp;

	if(ostate >= TCP_NSTATES) ostate = 0;
	if(act > SA_DROP) act = SA_DROP;
	printf("\n");
	ptime(ntime);
	printf("%s:%s", tcpstates[ostate], sanames[act]);

	if (si != 0) {
		seq = si->si_seq;
		ack = si->si_ack;
		alo = si->si_alo;
		len = si->si_len;
		switch (act) {
		case SA_RESPOND:
		case SA_OUTPUT:
				seq = ntohs(seq);
				ack = ntohs(ack);
				alo = ntohs(alo);
				len = ntohs(len);
		case SA_INPUT:
		case SA_DROP:
			if (aflag) {
				printf("\n\tsna=");
				ns_printhost(&si->si_sna);
				printf("\tdna=");
				ns_printhost(&si->si_dna);
			}
			printf("\n\t");
#define p1(f)  { printf("%s = %x, ", "f", f); }
			p1(seq); p1(ack); p1(alo); p1(len);
			flags = si->si_cc;
			printf("flags=%x", flags);
			if (flags) {
				char *cp = "<";
#define pf(f) { if (flags&SP_/**/f) { printf("%s%s", cp, "f"); cp = ","; } }
				pf(SP); pf(SA); pf(OB); pf(EM);
				printf(">");
			}
			printf(", ");
#define p2(f)  { printf("%s = %x, ", "f", si->si_/**/f); }
			p2(sid);p2(did);p2(dt);
			printf("\n\tsna=");
			ns_printhost(&si->si_sna);
			printf("\tdna=");
			ns_printhost(&si->si_dna);
		}
	}
	if(act == SA_USER) {
		printf("\treq=%s", prurequests[req&0xff]);
		if ((req & 0xff) == PRU_SLOWTIMO)
			printf("<%s>", tcptimers[req>>8]);
	}
	printf(" -> %s", tcpstates[sp->s_state]);

	/* print out internal state of sp !?! */
	printf("\n");
	if (sp == 0)
		return;
#define p3(f)  { printf("%s = %x, ", "f", sp->s_/**/f); }
	if(sflag) {
		printf("\t"); p3(rack); p3(ralo); p3(snt); p3(flags);
#undef pf
#define pf(f) { if (flags&SF_/**/f) { printf("%s%s", cp, "f"); cp = ","; } }
		flags = sp->s_flags;
		if (flags || sp->s_oobflags) {
			char *cp = "<";
			pf(AK); pf(DELACK); pf(HI); pf(HO);
			flags = sp->s_oobflags;
			pf(SOOB); pf(IOOB);
			printf(">");
		}

	}
	/* print out timers? */
	if (tflag) {
		char *cp = "\t";
		register int i;

		printf("\n\tTIMERS: ");
		p3(idle); p3(force); p3(rtseq);
		for (i = 0; i < TCPT_NTIMERS; i++) {
			if (sp->s_timer[i] == 0)
				continue;
			printf("%s%s=%d", cp, tcptimers[i], sp->s_timer[i]);
			if (i == TCPT_REXMT)
				printf(" (s_rxtshft=%d)", sp->s_rxtshift);
			cp = ", ";
		}
		if (*cp != '\t')
			putchar('\n');
	}
}

ns_printhost(p)
register struct ns_addr *p;
{

	printf("<net:%x%x,host:%4.4x%4.4x%4.4x,port:%x>",
			p->x_net.s_net[0],
			p->x_net.s_net[1],
			p->x_host.s_host[0],
			p->x_host.s_host[1],
			p->x_host.s_host[2],
			p->x_port);

}

