/*
 * $Id: nfs_start.c,v 5.2 90/06/23 22:19:48 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfs_start.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"
#include "amq.h"
#include <sys/signal.h>
#include <setjmp.h>
extern jmp_buf select_intr;
extern int select_intr_valid;

#ifdef HAS_TFS
/*
 * Use replacement for RPC/UDP transport
 * so that we do NFS gatewaying.
 */
#define	svcudp_create svcudp2_create
extern SVCXPRT *svcudp2_create P((int));
#endif /* HAS_TFS */

extern void nfs_program_2();
extern void amq_program_1();

unsigned short nfs_port;
SVCXPRT *nfsxprt;

extern int fwd_sock;
int max_fds = -1;

#define	MASKED_SIGS	(sigmask(SIGINT)|sigmask(SIGTERM)|sigmask(SIGCHLD)|sigmask(SIGHUP))

#ifdef DEBUG
/*
 * Check that we are not burning resources
 */
static void checkup(P_void)
{

static int max_fd = 0;
static char *max_mem = 0;

	int next_fd = dup(0);
	extern caddr_t sbrk P((int));
	caddr_t next_mem = sbrk(0);
	close(next_fd);

	/*if (max_fd < 0) {
		max_fd = next_fd;
	} else*/ if (max_fd < next_fd) {
		dlog("%d new fds allocated; total is %d",
			next_fd - max_fd, next_fd);
		max_fd = next_fd;
	}

	/*if (max_mem == 0) {
		max_mem = next_mem;
	} else*/ if (max_mem < next_mem) {
		dlog("%#x bytes of memory allocated; total is %#x (%d pages)",
			next_mem - max_mem,
			next_mem,
			((int)next_mem+getpagesize()-1)/getpagesize());
		max_mem = next_mem;
	}
}
#endif /* DEBUG */

static int do_select(smask, fds, fdp, tvp)
int smask;
int fds;
int *fdp;
struct timeval *tvp;
{
	int sig;
	int nsel;
	if (sig = setjmp(select_intr)) {
		select_intr_valid = 0;
		/* Got a signal */
		switch (sig) {
		case SIGINT:
		case SIGTERM:
			amd_state = Finishing;
			reschedule_timeout_mp();
			break;
		}
		nsel = -1;
		errno = EINTR;
	} else {
		select_intr_valid = 1;
		/*
		 * Invalidate the current clock value
		 */
		clock_valid = 0;
		/*
		 * Allow interrupts.  If a signal
		 * occurs, then it will cause a longjmp
		 * up above.
		 */
		(void) sigsetmask(smask);
		/*
		 * Wait for input
		 */
		nsel = select(fds, fdp, (int *) 0, (int *) 0,
				tvp->tv_sec ? tvp : (struct timeval *) 0);

	}

	(void) sigblock(MASKED_SIGS);

	/*
	 * Perhaps reload the cache?
	 */
	if (do_mapc_reload < clocktime()) {
		mapc_reload();
		do_mapc_reload = clocktime() + ONE_HOUR;
	}
	return nsel;
}

static serv_state run_rpc(P_void)
{
	int dtbsz = max_fds + 1;
	int smask = sigblock(MASKED_SIGS);

	next_softclock = clocktime();

	amd_state = Run;

	/*
	 * Keep on trucking while we are in Run mode.  This state
	 * is switched to Quit after all the file systems have
	 * been unmounted.
	 */
	while ((int)amd_state <= (int)Finishing) {
		struct timeval tvv;
		int nsel;
		time_t now;
#ifdef RPC_4
		fd_set readfds;
		readfds = svc_fdset;
		FD_SET(fwd_sock, &readfds);
#else
#ifdef FD_SET
		fd_set readfds;
		FD_ZERO(&readfds);
		readfds.fds_bits[0] = svc_fds;
		FD_SET(fwd_sock, &readfds);
#else
		int readfds = svc_fds | (1 << fwd_sock);
#endif /* FD_SET */
#endif /* RPC_4 */

#ifdef DEBUG
		checkup();
#endif /* DEBUG */

		/*
		 * If the full timeout code is not called,
		 * then recompute the time delta manually.
		 */
		now = clocktime();

		if (next_softclock <= now) {
			if (amd_state == Finishing)
				umount_exported();
			tvv.tv_sec = softclock();
		} else {
			tvv.tv_sec = next_softclock - now;
		}
		tvv.tv_usec = 0;

		if (amd_state == Finishing && last_used_map < 0) {
			flush_mntfs();
			amd_state = Quit;
			break;
		}

#ifdef DEBUG
		if (tvv.tv_sec)
			dlog("Select waits for %ds", tvv.tv_sec);
		else
			dlog("Select waits for Godot");
#endif /* DEBUG */

		nsel = do_select(smask, dtbsz, &readfds, &tvv);


		switch (nsel) {
		case -1:
			if (errno == EINTR) {
#ifdef DEBUG
				dlog("select interrupted");
#endif /* DEBUG */
				continue;
			}
			perror("select");
			break;

		case 0:
#ifdef DEBUG
			/*dlog("select returned 0");*/
#endif /* DEBUG */
			break;

		default:
#ifdef FD_SET
			if (FD_ISSET(fwd_sock, &readfds)) {
				FD_CLR(fwd_sock, &readfds);
				fwd_reply();
				--nsel;
			}
#else
			if (readfds & (1 << fwd_sock)) {
				readfds &= ~(1 << fwd_sock);
				fwd_reply();
				--nsel;
			}
#endif /* FD_SET */

			if (nsel) {
				/*
				 * Anything left must be a normal
				 * RPC request.
				 */
#ifdef RPC_4
				svc_getreqset(&readfds);
#else
#ifdef FD_SET
				svc_getreq(readfds.fds_bits[0]);
#else
				svc_getreq(readfds);
#endif /* FD_SET */
#endif /* RPC_4 */
			}
			break;
		}
	}

	(void) sigsetmask(smask);

	if (amd_state == Quit)
		amd_state = Done;

	return amd_state;
}

static int bindnfs_port(so)
int so;
{
	unsigned short port;
	int error = bind_resv_port(so, &port);
	if (error == 0)
		nfs_port = port;
	return error;
}

void unregister_amq(P_void)
{
#ifdef DEBUG
	Debug(D_AMQ)
#endif /* DEBUG */
	(void) pmap_unset(AMQ_PROGRAM, AMQ_VERSION);
}

int mount_automounter(ppid)
int ppid;
{
	int so = socket(AF_INET, SOCK_DGRAM, 0);
	SVCXPRT *amqp;
	int nmount;

	unregister_amq();

	if (so < 0 || bindnfs_port(so) < 0) {
		perror("Can't create privileged nfs port");
		return 1;
	}

	if ((nfsxprt = svcudp_create(so)) == NULL || 
			(amqp = svcudp_create(so)) == NULL) {
		plog(XLOG_FATAL, "cannot create rpc/udp service");
		return 2;
	}

	if (!svc_register(nfsxprt, NFS_PROGRAM, NFS_VERSION, nfs_program_2, 0)) {
		plog(XLOG_FATAL, "unable to register (NFS_PROGRAM, NFS_VERSION, 0)");
		return 3;
	}

#ifdef DEBUG
	Debug(D_AMQ)
#endif /* DEBUG */
	if (!svc_register(amqp, AMQ_PROGRAM, AMQ_VERSION, amq_program_1, IPPROTO_UDP)) {
		plog(XLOG_FATAL, "unable to register (AMQ_PROGRAM, AMQ_VERSION, udp)");
		return 3;
	}

	/*
	 * Start RPC forwarding
	 */
	if (fwd_init() != 0)
		return 3;

	/*
	 * One or other of so, fwd_sock
	 * must be the highest fd on
	 * which to select.
	 */
	if (so > max_fds)
		max_fds = so;
	if (fwd_sock > max_fds)
		max_fds = fwd_sock;

	/*
	 * Construct the root automount node
	 */
	make_root_node();

	/*
	 * Pick up the pieces from a previous run
	 * This is likely to (indirectly) need the rpc_fwd package
	 * so it *must* come after the call to fwd_init().
	 */
	if (restart_existing_mounts)
		restart();

	/*
	 * Mount the top-level auto-mountpoints
	 */
	nmount = mount_exported();

	/*
	 * Now safe to tell parent that we are up and running
	 */
	if (ppid)
		kill(ppid, SIGQUIT);

	if (nmount == 0) {
		plog(XLOG_FATAL, "No work to do - quitting");
		amd_state = Done;
		return 0;
	}

	/*
	 * Start timeout_mp rolling
	 */
	reschedule_timeout_mp();

	/*
	 * Start the server
	 */
	if (run_rpc() != Done) {
		plog(XLOG_FATAL, "run_rpc failed");
		amd_state = Done;
	}

	return 0;
}
