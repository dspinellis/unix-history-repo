/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)unix.c	5.1 (Berkeley) %G%";
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
	if (read(kmem, &nfile, sizeof (nfile)) != sizeof (nfile)) {
		printf("nfile: bad read.\n");
		return;
	}
	klseek(kmem, fileaddr, L_SET);
	if (read(kmem, &filep, sizeof (filep)) != sizeof (filep)) {
		printf("File table address, bad read.\n");
		return;
	}
	file = (struct file *)calloc(nfile, sizeof (struct file));
	if (file == (struct file *)0) {
		printf("Out of memory (file table).\n");
		return;
	}
	klseek(kmem, (off_t)filep, L_SET);
	if (read(kmem, file, nfile * sizeof (struct file)) !=
	    nfile * sizeof (struct file)) {
		printf("File table read error.\n");
		return;
	}
	fileNFILE = file + nfile;
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0 || fp->f_type != DTYPE_SOCKET)
			continue;
		klseek(kmem, fp->f_data, L_SET);
		if (read(kmem, so, sizeof (*so)) != sizeof (*so))
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
	static int first = 1;

	klseek(kmem, so->so_pcb, L_SET);
	if (read(kmem, unp, sizeof (*unp)) != sizeof (*unp))
		return;
	if (unp->unp_remaddr) {
		m = &mbuf;
		klseek(kmem, unp->unp_remaddr, L_SET);
		if (read(kmem, m, sizeof (*m)) != sizeof (*m))
			m = (struct mbuf *)0;
	} else
		m = (struct mbuf *)0;
	if (first) {
		printf(
"%-8.8s %-6.6s %-6.6s %-6.6s %8.8s %8.8s %8.8s %8.8s Remaddr\n",
		    "Address", "Type", "Recv-Q", "Send-Q",
		    "Inode", "Conn", "Refs", "Nextref");
		first = 0;
	}
	printf("%8x %-6.6s %6d %6d %8x %8x %8x %8x",
	    soaddr, socktype[so->so_type], so->so_rcv.sb_cc, so->so_snd.sb_cc,
	    unp->unp_inode, unp->unp_conn,
	    unp->unp_refs, unp->unp_nextref);
	if (m)
		printf(" %.*s", m->m_len, mtod(m, char *));
	putchar('\n');
}
