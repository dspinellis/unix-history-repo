/*
 * Copyright (c) 1983,1988 Regents of the University of California.
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
static char sccsid[] = "@(#)unix.c	5.5 (Berkeley) 2/7/88";
#endif not lint

/*
 * Display protocol blocks in the unix domain.
 */
#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/un.h>
#include <sys/unpcb.h>
#define	KERNEL
#include <sys/file.h>

int	Aflag;
int	kmem;
extern	char *calloc();

unixpr(nfileaddr, fileaddr, unixsw)
	off_t nfileaddr, fileaddr;
	struct protosw *unixsw;
{
	register struct file *fp;
	struct file *filep;
	struct socket sock, *so = &sock;

	if (nfileaddr == 0 || fileaddr == 0) {
		printf("nfile or file not in namelist.\n");
		return;
	}
	klseek(kmem, nfileaddr, L_SET);
	if (read(kmem, (char *)&nfile, sizeof (nfile)) != sizeof (nfile)) {
		printf("nfile: bad read.\n");
		return;
	}
	klseek(kmem, fileaddr, L_SET);
	if (read(kmem, (char *)&filep, sizeof (filep)) != sizeof (filep)) {
		printf("File table address, bad read.\n");
		return;
	}
	file = (struct file *)calloc(nfile, sizeof (struct file));
	if (file == (struct file *)0) {
		printf("Out of memory (file table).\n");
		return;
	}
	klseek(kmem, (off_t)filep, L_SET);
	if (read(kmem, (char *)file, nfile * sizeof (struct file)) !=
	    nfile * sizeof (struct file)) {
		printf("File table read error.\n");
		return;
	}
	fileNFILE = file + nfile;
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0 || fp->f_type != DTYPE_SOCKET)
			continue;
		klseek(kmem, (off_t)fp->f_data, L_SET);
		if (read(kmem, (char *)so, sizeof (*so)) != sizeof (*so))
			continue;
		/* kludge */
		if (so->so_proto >= unixsw && so->so_proto <= unixsw + 2)
			if (so->so_pcb)
				unixdomainpr(so, fp->f_data);
	}
	free((char *)file);
}

static	char *socktype[] =
    { "#0", "stream", "dgram", "raw", "rdm", "seqpacket" };

unixdomainpr(so, soaddr)
	register struct socket *so;
	caddr_t soaddr;
{
	struct unpcb unpcb, *unp = &unpcb;
	struct mbuf mbuf, *m;
	struct sockaddr_un *sa;
	static int first = 1;

	klseek(kmem, (off_t)so->so_pcb, L_SET);
	if (read(kmem, (char *)unp, sizeof (*unp)) != sizeof (*unp))
		return;
	if (unp->unp_addr) {
		m = &mbuf;
		klseek(kmem, (off_t)unp->unp_addr, L_SET);
		if (read(kmem, (char *)m, sizeof (*m)) != sizeof (*m))
			m = (struct mbuf *)0;
		sa = mtod(m, struct sockaddr_un *);
	} else
		m = (struct mbuf *)0;
	if (first) {
		printf("Active UNIX domain sockets\n");
		printf(
"%-8.8s %-6.6s %-6.6s %-6.6s %8.8s %8.8s %8.8s %8.8s Addr\n",
		    "Address", "Type", "Recv-Q", "Send-Q",
		    "Inode", "Conn", "Refs", "Nextref");
		first = 0;
	}
	printf("%8x %-6.6s %6d %6d %8x %8x %8x %8x",
	    soaddr, socktype[so->so_type], so->so_rcv.sb_cc, so->so_snd.sb_cc,
	    unp->unp_inode, unp->unp_conn,
	    unp->unp_refs, unp->unp_nextref);
	if (m)
		printf(" %.*s", m->m_len - sizeof(sa->sun_family),
		    sa->sun_path);
	putchar('\n');
}
