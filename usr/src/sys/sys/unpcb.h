/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)unpcb.h	6.2 (Berkeley) %G%
 */

/*
 * Protocol control block for an active
 * instance of a UNIX internal protocol.
 *
 * A socket may be associated with an inode in the
 * file system.  If so, the unp_inode pointer holds
 * a reference count to this inode, which should be irele'd
 * when the socket goes away.
 *
 * A socket may be connected to another socket, in which
 * case the control block of the socket to which it is connected
 * is given by unp_conn.
 *
 * A socket may be referenced by a number of sockets (e.g. several
 * sockets may be connected to a datagram socket.)  These sockets
 * are in a linked list starting with unp_refs, linked through
 * unp_nextref and null-terminated.  Note that a socket may be referenced
 * by a number of other sockets and may also reference a socket (not
 * necessarily one which is referencing it).  This generates
 * the need for unp_refs and unp_nextref to be separate fields.
 */
struct	unpcb {
	struct	socket *unp_socket;	/* pointer back to socket */
	struct	inode *unp_inode;	/* if associated with file */
	struct	unpcb *unp_conn;	/* control block of connected socket */
	struct	unpcb *unp_refs;	/* referencing socket linked list */
	struct 	unpcb *unp_nextref;	/* link in unp_refs list */
	struct	mbuf *unp_remaddr;	/* address of connected socket */
};

#define	sotounpcb(so)	((struct unpcb *)((so)->so_pcb))
