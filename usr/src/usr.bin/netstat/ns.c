/*
 * Copyright (c) 1985,1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)ns.c	5.8 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <errno.h>
#include <nlist.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>

#include <net/route.h>
#include <net/if.h>

#include <netinet/tcp_fsm.h>

#include <netns/ns.h>
#include <netns/ns_pcb.h>
#include <netns/idp.h>
#include <netns/idp_var.h>
#include <netns/ns_error.h>
#include <netns/sp.h>
#include <netns/spidp.h>
#include <netns/spp_timer.h>
#include <netns/spp_var.h>
#define SANAMES
#include <netns/spp_debug.h>


struct	nspcb nspcb;
struct	sppcb sppcb;
struct	socket sockb;
extern	int kmem;
extern	int Aflag;
extern	int aflag;
extern	int nflag;
extern	char *plural();
char *ns_prpr();

static	int first = 1;

/*
 * Print a summary of connections related to a Network Systems
 * protocol.  For SPP, also give state of connection.
 * Listening processes (aflag) are suppressed unless the
 * -a (all) flag is specified.
 */

nsprotopr(off, name)
	off_t off;
	char *name;
{
	struct nspcb cb;
	register struct nspcb *prev, *next;
	int isspp;

	if (off == 0)
		return;
	isspp = strcmp(name, "spp") == 0;
	klseek(kmem, off, 0);
	read(kmem, (char *)&cb, sizeof (struct nspcb));
	nspcb = cb;
	prev = (struct nspcb *)off;
	if (nspcb.nsp_next == (struct nspcb *)off)
		return;
	for (;nspcb.nsp_next != (struct nspcb *)off; prev = next) {
		off_t ppcb;

		next = nspcb.nsp_next;
		klseek(kmem, (off_t)next, 0);
		read(kmem, (char *)&nspcb, sizeof (nspcb));
		if (nspcb.nsp_prev != prev) {
			printf("???\n");
			break;
		}
		if (!aflag && ns_nullhost(nspcb.nsp_faddr) ) {
			continue;
		}
		klseek(kmem, (off_t)nspcb.nsp_socket, 0);
		read(kmem, (char *)&sockb, sizeof (sockb));
		ppcb = (off_t) nspcb.nsp_pcb;
		if (ppcb) {
			if (isspp) {
				klseek(kmem, ppcb, 0);
				read(kmem, (char *)&sppcb, sizeof (sppcb));
			} else continue;
		} else
			if (isspp) continue;
		if (first) {
			printf("Active NS connections");
			if (aflag)
				printf(" (including servers)");
			putchar('\n');
			if (Aflag)
				printf("%-8.8s ", "PCB");
			printf(Aflag ?
				"%-5.5s %-6.6s %-6.6s  %-18.18s %-18.18s %s\n" :
				"%-5.5s %-6.6s %-6.6s  %-22.22s %-22.22s %s\n",
				"Proto", "Recv-Q", "Send-Q",
				"Local Address", "Foreign Address", "(state)");
			first = 0;
		}
		if (Aflag)
			printf("%8x ", ppcb);
		printf("%-5.5s %6d %6d ", name, sockb.so_rcv.sb_cc,
			sockb.so_snd.sb_cc);
		printf("  %-22.22s", ns_prpr(&nspcb.nsp_laddr));
		printf(" %-22.22s", ns_prpr(&nspcb.nsp_faddr));
		if (isspp) {
			extern char *tcpstates[];
			if (sppcb.s_state >= TCP_NSTATES)
				printf(" %d", sppcb.s_state);
			else
				printf(" %s", tcpstates[sppcb.s_state]);
		}
		putchar('\n');
		prev = next;
	}
}
#define ANY(x,y,z)  ((x) ? printf("\t%d %s%s%s -- %s\n",x,y,plural(x),z,"x") : 0)

/*
 * Dump SPP statistics structure.
 */
spp_stats(off, name)
	off_t off;
	char *name;
{
	struct spp_istat spp_istat;
#define sppstat spp_istat.newstats

	if (off == 0)
		return;
	klseek(kmem, off, 0);
	read(kmem, (char *)&spp_istat, sizeof (spp_istat));
	printf("%s:\n", name);
	ANY(spp_istat.nonucn, "connection", " dropped due to no new sockets ");
	ANY(spp_istat.gonawy, "connection", " terminated due to our end dying");
	ANY(spp_istat.nonucn, "connection", " dropped due to inability to connect");
	ANY(spp_istat.noconn, "connection", " dropped due to inability to connect");
	ANY(spp_istat.notme, "connection", " incompleted due to mismatched id's");
	ANY(spp_istat.wrncon, "connection", " dropped due to mismatched id's");
	ANY(spp_istat.bdreas, "packet", " dropped out of sequence");
	ANY(spp_istat.lstdup, "packet", " duplicating the highest packet");
	ANY(spp_istat.notyet, "packet", " refused as exceeding allocation");
	ANY(sppstat.spps_connattempt, "connection", " initiated");
	ANY(sppstat.spps_accepts, "connection", " accepted");
	ANY(sppstat.spps_connects, "connection", " established");
	ANY(sppstat.spps_drops, "connection", " dropped");
	ANY(sppstat.spps_conndrops, "embryonic connection", " dropped");
	ANY(sppstat.spps_closed, "connection", " closed (includes drops)");
	ANY(sppstat.spps_segstimed, "packet", " where we tried to get rtt");
	ANY(sppstat.spps_rttupdated, "time", " we got rtt");
	ANY(sppstat.spps_delack, "delayed ack", " sent");
	ANY(sppstat.spps_timeoutdrop, "connection", " dropped in rxmt timeout");
	ANY(sppstat.spps_rexmttimeo, "retransmit timeout", "");
	ANY(sppstat.spps_persisttimeo, "persist timeout", "");
	ANY(sppstat.spps_keeptimeo, "keepalive timeout", "");
	ANY(sppstat.spps_keepprobe, "keepalive probe", " sent");
	ANY(sppstat.spps_keepdrops, "connection", " dropped in keepalive");
	ANY(sppstat.spps_sndtotal, "total packet", " sent");
	ANY(sppstat.spps_sndpack, "data packet", " sent");
	ANY(sppstat.spps_sndbyte, "data byte", " sent");
	ANY(sppstat.spps_sndrexmitpack, "data packet", " retransmitted");
	ANY(sppstat.spps_sndrexmitbyte, "data byte", " retransmitted");
	ANY(sppstat.spps_sndacks, "ack-only packet", " sent");
	ANY(sppstat.spps_sndprobe, "window probe", " sent");
	ANY(sppstat.spps_sndurg, "packet", " sent with URG only");
	ANY(sppstat.spps_sndwinup, "window update-only packet", " sent");
	ANY(sppstat.spps_sndctrl, "control (SYN|FIN|RST) packet", " sent");
	ANY(sppstat.spps_sndvoid, "request", " to send a non-existant packet");
	ANY(sppstat.spps_rcvtotal, "total packet", " received");
	ANY(sppstat.spps_rcvpack, "packet", " received in sequence");
	ANY(sppstat.spps_rcvbyte, "byte", " received in sequence");
	ANY(sppstat.spps_rcvbadsum, "packet", " received with ccksum errs");
	ANY(sppstat.spps_rcvbadoff, "packet", " received with bad offset");
	ANY(sppstat.spps_rcvshort, "packet", " received too short");
	ANY(sppstat.spps_rcvduppack, "duplicate-only packet", " received");
	ANY(sppstat.spps_rcvdupbyte, "duplicate-only byte", " received");
	ANY(sppstat.spps_rcvpartduppack, "packet", " with some duplicate data");
	ANY(sppstat.spps_rcvpartdupbyte, "dup. byte", " in part-dup. packet");
	ANY(sppstat.spps_rcvoopack, "out-of-order packet", " received");
	ANY(sppstat.spps_rcvoobyte, "out-of-order byte", " received");
	ANY(sppstat.spps_rcvpackafterwin, "packet", " with data after window");
	ANY(sppstat.spps_rcvbyteafterwin, "byte", " rcvd after window");
	ANY(sppstat.spps_rcvafterclose, "packet", " rcvd after 'close'");
	ANY(sppstat.spps_rcvwinprobe, "rcvd window probe packet", "");
	ANY(sppstat.spps_rcvdupack, "rcvd duplicate ack", "");
	ANY(sppstat.spps_rcvacktoomuch, "rcvd ack", " for unsent data");
	ANY(sppstat.spps_rcvackpack, "rcvd ack packet", "");
	ANY(sppstat.spps_rcvackbyte, "byte", " acked by rcvd acks");
	ANY(sppstat.spps_rcvwinupd, "rcvd window update packet", "");
}
#undef ANY
#define ANY(x,y,z)  ((x) ? printf("\t%d %s%s%s\n",x,y,plural(x),z) : 0)

/*
 * Dump IDP statistics structure.
 */
idp_stats(off, name)
	off_t off;
	char *name;
{
	struct idpstat idpstat;

	if (off == 0)
		return;
	klseek(kmem, off, 0);
	read(kmem, (char *)&idpstat, sizeof (idpstat));
	printf("%s:\n", name);
	ANY(idpstat.idps_toosmall, "packet", " smaller than a header");
	ANY(idpstat.idps_tooshort, "packet", " smaller than advertised");
	ANY(idpstat.idps_badsum, "packet", " with bad checksums");
}

static	struct {
	u_short code;
	char *name;
	char *where;
} ns_errnames[] = {
	{0, "Unspecified Error", " at Destination"},
	{1, "Bad Checksum", " at Destination"},
	{2, "No Listener", " at Socket"},
	{3, "Packet", " Refused due to lack of space at Destination"},
	{01000, "Unspecified Error", " while gatewayed"},
	{01001, "Bad Checksum", " while gatewayed"},
	{01002, "Packet", " forwarded too many times"},
	{01003, "Packet", " too large to be forwarded"},
	{-1, 0, 0},
};

/*
 * Dump NS Error statistics structure.
 */
/*ARGSUSED*/
nserr_stats(off, name)
	off_t off;
	char *name;
{
	struct ns_errstat ns_errstat;
	register int j;
	register int histoprint = 1;
	int z;

	if (off == 0)
		return;
	klseek(kmem, off, 0);
	read(kmem, (char *)&ns_errstat, sizeof (ns_errstat));
	printf("NS error statistics:\n");
	ANY(ns_errstat.ns_es_error, "call", " to ns_error");
	ANY(ns_errstat.ns_es_oldshort, "error",
		" ignored due to insufficient addressing");
	ANY(ns_errstat.ns_es_oldns_err, "error request",
		" in response to error packets");
	ANY(ns_errstat.ns_es_tooshort, "error packet",
		" received incomplete");
	ANY(ns_errstat.ns_es_badcode, "error packet",
		" received of unknown type");
	for(j = 0; j < NS_ERR_MAX; j ++) {
		z = ns_errstat.ns_es_outhist[j];
		if (z && histoprint) {
			printf("Output Error Histogram:\n");
			histoprint = 0;
		}
		ns_erputil(z, ns_errstat.ns_es_codes[j]);

	}
	histoprint = 1;
	for(j = 0; j < NS_ERR_MAX; j ++) {
		z = ns_errstat.ns_es_inhist[j];
		if (z && histoprint) {
			printf("Input Error Histogram:\n");
			histoprint = 0;
		}
		ns_erputil(z, ns_errstat.ns_es_codes[j]);
	}
}
static
ns_erputil(z, c)
{
	int j;
	char codebuf[30];
	char *name, *where;
	for(j = 0;; j ++) {
		if ((name = ns_errnames[j].name) == 0)
			break;
		if (ns_errnames[j].code == c)
			break;
	}
	if (name == 0)  {
		if (c > 01000)
			where = "in transit";
		else
			where = "at destination";
		sprintf(codebuf, "Unknown XNS error code 0%o", c);
		name = codebuf;
	} else 
		where =  ns_errnames[j].where;
	ANY(z, name, where);
}
static struct sockaddr_ns ssns = {AF_NS};

char *ns_prpr(x)
struct ns_addr *x;
{
	extern char *ns_print();
	struct sockaddr_ns *sns = &ssns;
	sns->sns_addr = *x;
	return(ns_print(sns));
}
