/*-
 * Copyright (c) 1988, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pk_dump.c	7.2 (Berkeley) 5/9/91";
#endif /* not lint */

/*
 * This is a kernel debugging aid.
 * dumps out a cache of mbufs.
 */

#include <sys/param.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <net/if.h>
#include <netccitt/x25.h>
#include <netccitt/pk.h>
#include <netccitt/pk_var.h>

#include <errno.h>
#include <netdb.h>
#include <nlist.h>
#include <kvm.h>
#include <paths.h>
#include <stdio.h>
/* 
 *  This procedure decodes the X.25 level 3 packet returning a 
 *  code to be used in switchs or arrays.
 */

pk_decode (xp)
register struct x25_packet *xp;
{
	register int type;

	if (xp -> fmt_identifier != 1)
		return (INVALID_PACKET);
#ifdef ancient_history
	/* 
	 *  Make sure that the logical channel group number is 0.
	 *  This restriction may be removed at some later date.
	 */
	if (xp -> lc_group_number != 0)
		return (INVALID_PACKET);
#endif
	/* 
	 *  Test for data packet first.
	 */
	if (!(xp -> packet_type & DATA_PACKET_DESIGNATOR))
		return (DATA);

	/* 
	 *  Test if flow control packet (RR or RNR).
	 */
	if (!(xp -> packet_type & RR_OR_RNR_PACKET_DESIGNATOR))
		switch (xp -> packet_type & 0x1f) {
		case X25_RR:
			return (RR);
		case X25_RNR:
			return (RNR);
		case X25_REJECT:
			return (REJECT);
		}

	/* 
	 *  Determine the rest of the packet types.
	 */
	switch (xp -> packet_type) {
	case X25_CALL: 
		type = CALL;
		break;

	case X25_CALL_ACCEPTED: 
		type = CALL_ACCEPTED;
		break;

	case X25_CLEAR: 
		type = CLEAR;
		break;

	case X25_CLEAR_CONFIRM: 
		type = CLEAR_CONF;
		break;

	case X25_INTERRUPT: 
		type = INTERRUPT;
		break;

	case X25_INTERRUPT_CONFIRM: 
		type = INTERRUPT_CONF;
		break;

	case X25_RESET: 
		type = RESET;
		break;

	case X25_RESET_CONFIRM: 
		type = RESET_CONF;
		break;

	case X25_RESTART: 
		type = RESTART;
		break;

	case X25_RESTART_CONFIRM: 
		type = RESTART_CONF;
		break;

	case X25_DIAGNOSTIC:
		type = DIAG_TYPE;
		break;

	default: 
		type = INVALID_PACKET;
	}
	return (type);
}

char	*pk_state[] = {
	"Listen",	"Ready",	"Received-Call",
	"Sent-Call",	"Data-Transfer","Received-Clear",
	"Sent-Clear",
};

char   *pk_name[] = {
	"Call",		"Call-Conf",	"Clear",
	"Clear-Conf",	"Data",		"Intr",		"Intr-Conf",
	"Rr",		"Rnr",		"Reset",	"Reset-Conf",
	"Restart",	"Restart-Conf",	"Reject",	"Diagnostic",
	"Invalid"
};

int pk_lengths[] = {0, 0, 0,
0, 3, 5, 3,
3, 3, 5, 5,
5, 5, 5, 0,
0, 0};

pk_trace (m, dir)
register struct mbuf *m;
char *dir;
{
	register char *s;
	struct x25_packet *xp = mtod(m, struct x25_packet *);
	register int i, len = 0, cnt = 0;

	i = pk_decode (xp) / MAXSTATES;
	if ((len = pk_lengths[i]) || (len = m -> m_len))
		if (len > 5)
			len = 5;

	printf ("%s LCN=%d: %s (", dir, LCN(xp), pk_name[i]);
  
	for (s = (char *) xp, i = 0; i < len; ++i, ++s)
		printf ("%x ", (int) * s & 0xff);
	printf (")\n");
}

bprintf(fp, b, s)
	register FILE *fp;
	register int b;
	register u_char *s;
{
	register int i;
	int gotsome = 0;

	if (b == 0)
		return;
	while (i = *s++) {
		if (b & (1 << (i-1))) {
			if (gotsome == 0)
				i = '<';
			else
				i = ',';
			(void) putc(i, fp);
			gotsome = 1;
			for (; (i = *s) > 32; s++)
				(void) putc(i, fp);
		} else
			while (*s > 32)
				s++;
	}
	if (gotsome)
		(void) putc('>', fp);
}

int verbose = 0; /* so you can adb -w the binary */
int tflag = 0;
int Iflag = 0;
int Aflag = 0;
char *vmunix = _PATH_UNIX;
char *kmemf;
struct nlist nl[] = {
{"_pk_output_cache"},
{"_pk_input_cache"},
0
};

main(argc, argv)
	int argc;
	char **argv;
{

	if (kvm_openfiles(vmunix, kmemf, NULL) == -1) {
		fprintf(stderr, "netstat: kvm_openfiles: %s\n", kvm_geterr());
		exit(1);
	}
	if (kvm_nlist(nl) < 0 || nl[0].n_type == 0) {
		fprintf(stderr, "%s: no namelist\n", vmunix);
		exit(1);
	}
	mbuf_cache_dump(nl);
	mbuf_cache_dump(nl + 1);
}
struct mbuf_cache c;
struct mbuf **mbvec;
#define kget(p, d) \
	(kvm_read((void *)(p), &(d), sizeof (d)))

mbuf_cache_dump(nl)
struct nlist *nl;
{
	register struct mbuf *m;
	unsigned cache_size;
	int i;

	printf("Dumping %s:\n", nl->n_name);
	kget(nl->n_value, c);
	if (cache_size = c.mbc_size * sizeof(m))
		mbvec = (struct mbuf **)malloc(cache_size);
	if (mbvec == 0 || c.mbc_cache == 0)
		return;
	kvm_read(c.mbc_cache, mbvec, cache_size);
	for (i = c.mbc_num;;) {
		if (i == 0)
			i = c.mbc_size;
		i--;
		if (m = mbvec[i])
			mbuf_dump(m);
		if (i == c.mbc_num)
			break;
	}
}


mbuf_dump(m)
register struct mbuf *m;
{
	int virgin = 1;
	register struct x25_packet *xp;
	struct mbuf n;
	char extbuf[1024];

	putchar('\n');
	for (; m; m = n.m_next) {
		kget(m, n);
		printf("m %x", m);
		if (n.m_flags) {
			printf(" flags ");
			bprintf(stdout, n.m_flags,
			    "\1M_EXT\2M_PKTHDR\3M_EOR\4M_BCAST\5M_MCAST");
		}
		if (Aflag)
			printf(" chained %x", n.m_nextpkt);
		printf(" next %x len %d", n.m_next, n.m_len);
		if (n.m_flags & M_PKTHDR) {
			printf(" total %d", n.m_pkthdr.len);
			if (Iflag)
				printf(" rcvif %x", n.m_pkthdr.rcvif);
		}
		putchar('\n');
		if (n.m_flags & M_EXT) {
			kvm_read(n.m_ext.ext_buf, extbuf, sizeof(extbuf));
			n.m_data = extbuf + (n.m_data - n.m_ext.ext_buf);
		} else if (n.m_data <  m->m_dat + MLEN)
			n.m_data = n.m_dat + (n.m_data - m->m_dat);
		else {
			printf("mbuf screwup\n");
			continue;
		}
		if (virgin) {
			virgin = 0;
			pk_trace(&n, "  X.25: ");
		}
		dumpit("data: ",n.m_data, n.m_len);
	}
}

dumpit(what, where, n)
char *what; unsigned short *where; int n;
{
	unsigned short *s = where;
	unsigned short *z = where + (n+1)/2;
	int count = 0;

	if (verbose == 0)
		return;
	printf(what);
	while(s < z) {
		count++;
		printf("%x ",*s++);
		if ((count & 15) == 0)
			putchar('\n');
	}
	if (count & 15)
		putchar('\n');
	fflush(stdout);
}
