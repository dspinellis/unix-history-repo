/*-
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)unix.c	5.16 (Berkeley) %G%";
#endif /* not lint */

/*
 * Display protocol blocks in the unix domain.
 */
#include <kvm.h>
#include <sys/param.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/sysctl.h>
#include <sys/un.h>
#include <sys/unpcb.h>
#define KERNEL
struct uio;
struct proc;
#include <sys/file.h>

#include <netinet/in.h>

#include <stdio.h>
#include <stdlib.h>
#include "netstat.h"

static	void unixdomainpr __P((struct socket *, caddr_t));

static struct	file *file, *fileNFILE;
static int	nfiles;
extern	kvm_t *kvmd;

void
unixpr(off)
	u_long	off;
{
	register struct file *fp;
	struct socket sock, *so = &sock;
	char *filebuf;
	struct protosw *unixsw = (struct protosw *)off;

	filebuf = (char *)kvm_getfiles(kvmd, KERN_FILE, 0, &nfiles);
	if (filebuf == 0) {
		printf("Out of memory (file table).\n");
		return;
	}
	file = (struct file *)(filebuf + sizeof(fp));
	fileNFILE = file + nfiles;
	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_count == 0 || fp->f_type != DTYPE_SOCKET)
			continue;
		if (kread((u_long)fp->f_data, (char *)so, sizeof (*so)))
			continue;
		/* kludge */
		if (so->so_proto >= unixsw && so->so_proto <= unixsw + 2)
			if (so->so_pcb)
				unixdomainpr(so, fp->f_data);
	}
}

static	char *socktype[] =
    { "#0", "stream", "dgram", "raw", "rdm", "seqpacket" };

static void
unixdomainpr(so, soaddr)
	register struct socket *so;
	caddr_t soaddr;
{
	struct unpcb unpcb, *unp = &unpcb;
	struct mbuf mbuf, *m;
	struct sockaddr_un *sa;
	static int first = 1;

	if (kread((u_long)so->so_pcb, (char *)unp, sizeof (*unp)))
		return;
	if (unp->unp_addr) {
		m = &mbuf;
		if (kread((u_long)unp->unp_addr, (char *)m, sizeof (*m)))
			m = (struct mbuf *)0;
		sa = (struct sockaddr_un *)(m->m_dat);
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
	    unp->unp_vnode, unp->unp_conn,
	    unp->unp_refs, unp->unp_nextref);
	if (m)
		printf(" %.*s",
		    m->m_len - (int)(sizeof(*sa) - sizeof(sa->sun_path)),
		    sa->sun_path);
	putchar('\n');
}
