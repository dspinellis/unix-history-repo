/*
 * $Id: srvr_nfs.c,v 5.2 90/06/23 22:20:02 jsp Rel $
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
 *	@(#)srvr_nfs.c	5.1 (Berkeley) 6/29/90
 */

/*
 * NFS server modeling
 */

#include "am.h"
#include <netdb.h>
#include <rpc/pmap_prot.h>
#include "mount.h"

extern qelem nfs_srvr_list;
qelem nfs_srvr_list = { &nfs_srvr_list, &nfs_srvr_list };

typedef struct nfs_private {
	u_short np_mountd;	/* Mount daemon port number */
	char np_mountd_inval;	/* Port may be invalid */
	int np_ping;		/* Number of failed ping attempts */
	time_t np_ttl;		/* Time when server is thought dead */
	int np_xid;		/* RPC transaction id for pings */
	int np_error;		/* Error during portmap request */
} nfs_private;

static int np_xid;	/* For NFS pings */
#define	NPXID_ALLOC()	(++np_xid)
/*#define	NPXID_ALLOC()	((++np_xid&0x0fffffff) == 0 ? npxid_gc() : np_xid)*/

/*
 * Number of pings allowed to fail before host is declared down
 * - three-fifths of the allowed mount time...
#define	MAX_ALLOWED_PINGS	((((ALLOWED_MOUNT_TIME + 5 * AM_PINGER - 1) * 3) / 5) / AM_PINGER)
 */
#define	MAX_ALLOWED_PINGS	(3 + /* for luck ... */ 1)

/*
 * How often to ping when starting a new server
 */
#define	FAST_NFS_PING		3

#if (FAST_NFS_PING * MAX_ALLOWED_PINGS) >= ALLOWED_MOUNT_TIME
 #error: sanity check failed
/*
 you cannot do things this way...
 sufficient fast pings must be given the chance to fail
 within the allowed mount time
 */
#endif /* (FAST_NFS_PING * MAX_ALLOWED_PINGS) >= ALLOWED_MOUNT_TIME */

static int ping_len;
static char ping_buf[sizeof(struct rpc_msg) + 32];

/*
 * Startup the NFS ping
 */
static void start_ping()
{
	XDR ping_xdr;
	struct rpc_msg ping_msg;

	rpc_msg_init(&ping_msg, NFS_PROGRAM, NFS_VERSION, NFSPROC_NULL);

	/*
	 * Create an XDR endpoint
	 */
	xdrmem_create(&ping_xdr, ping_buf, sizeof(ping_buf), XDR_ENCODE);

	/*
	 * Create the NFS ping message
	 */
	if (!xdr_callmsg(&ping_xdr, &ping_msg)) {
		plog(XLOG_ERROR, "Couldn't create ping RPC message");
		going_down(3);
	}

	/*
	 * Find out how long it is
	 */
	ping_len = xdr_getpos(&ping_xdr);

	/*
	 * Destroy the XDR endpoint - we don't need it anymore
	 */
	xdr_destroy(&ping_xdr);
}


/*
 * Called when a portmap reply arrives
 */
static void got_portmap(pkt, len, sa, ia, idv, done)
voidp pkt;
int len;
struct sockaddr_in *sa, *ia;
voidp idv;
int done;
{
	fserver *fs2 = (fserver *) idv;
	fserver *fs = 0;
	ITER(fs, fserver, &nfs_srvr_list)
		if (fs == fs2)
			break;

	if (fs == fs2) {
		u_long port = 0;	/* XXX - should be short but protocol is naff */
		int error = done ? pickup_rpc_reply(pkt, len, (voidp) &port, xdr_u_long) : -1;
		nfs_private *np = (nfs_private *) fs->fs_private;
		if (!error && port) {
#ifdef DEBUG
			dlog("got port (%d) for mountd on %s", port, fs->fs_host);
#endif /* DEBUG */
			/*
			 * Grab the port number.  Portmap sends back
			 * an unsigned long in native ordering, so it
			 * needs converting to a unsigned short in
			 * network ordering.
			 */
			np->np_mountd = htons((u_short) port);
			np->np_mountd_inval = FALSE;
			np->np_error = 0;
		} else {
#ifdef DEBUG
			dlog("Error fetching port for mountd on %s", fs->fs_host);
#endif /* DEBUG */
			/*
			 * Almost certainly no mountd running on remote host
			 */
			np->np_error = error ? error : ETIMEDOUT;
		}
		if (fs->fs_flags & FSF_WANT)
			wakeup_srvr(fs);
	} else if (done) {
#ifdef DEBUG
		dlog("Got portmap for old port request");
#endif /* DEBUG */
	} else {
#ifdef DEBUG
		dlog("portmap request timed out");
#endif /* DEBUG */
	}
}

/*
 * Obtain portmap information
 */
static int call_portmap(fs, auth, prog, vers, prot)
fserver *fs;
AUTH *auth;
unsigned long prog, vers, prot;
{
	struct rpc_msg pmap_msg;
	int len;
	char iobuf[UDPMSGSIZE];
	int error;
	struct pmap pmap;

	rpc_msg_init(&pmap_msg, PMAPPROG, PMAPVERS, (unsigned long) 0);
	pmap.pm_prog = prog;
	pmap.pm_vers = vers;
	pmap.pm_prot = prot;
	pmap.pm_port = 0;
	len = make_rpc_packet(iobuf, sizeof(iobuf), PMAPPROC_GETPORT,
			&pmap_msg, (voidp) &pmap, xdr_pmap, auth);
	if (len > 0) {
		struct sockaddr_in sin;
		bzero((voidp) &sin, sizeof(sin));
		sin = *fs->fs_ip;
		sin.sin_port = htons(PMAPPORT);
		error = fwd_packet(RPC_XID_PORTMAP, (voidp) iobuf, len,
				&sin, &sin, (voidp) fs, got_portmap);
	} else {
		error = -len;
	}
	return error;
}

static void nfs_keepalive P((fserver*));

static void recompute_portmap P((fserver *fs));
static void recompute_portmap(fs)
fserver *fs;
{				
	if (!nfs_auth)
		nfs_auth = authunix_create_default();
	if (!nfs_auth) {
		nfs_private *np = (nfs_private *) fs->fs_private;
		np->np_error = ENOBUFS;
	} else {
		call_portmap(fs, nfs_auth, MOUNTPROG,
			MOUNTVERS, (unsigned long) IPPROTO_UDP);
	}
}

/*
 * This is called when we get a reply to an RPC ping.
 * The value of id was taken from the nfs_private
 * structure when the ping was transmitted.
 */
/*ARGSUSED*/
static void nfs_pinged(pkt, len, sp, tsp, idv, done)
voidp pkt;
int len;
struct sockaddr_in *sp, *tsp;
voidp idv;
int done;
{
	int xid = (int) idv;
	fserver *fs;
	int found_map = 0;

	if (!done)
		return;

	/*
	 * For each node...
	 */
	ITER(fs, fserver, &nfs_srvr_list) {
		nfs_private *np = (nfs_private *) fs->fs_private;
		if (np->np_xid == xid) {
			/*
			 * Reset the ping counter.
			 * Update the keepalive timer.
			 * Log what happened.
			 */
			if (fs->fs_flags & FSF_DOWN) {
				fs->fs_flags &= ~FSF_DOWN;
				if (fs->fs_flags & FSF_VALID) {
					srvrlog(fs, "is up");
				} else {
					srvrlog(fs, "ok");
					fs->fs_flags |= FSF_VALID;
				}

#ifdef notdef
				/* why ??? */
				if (fs->fs_flags & FSF_WANT)
					wakeup_srvr(fs);
#endif /* notdef */
			} else {
				if (fs->fs_flags & FSF_VALID) {
#ifdef DEBUG
					dlog("file server %s type nfs is still up", fs->fs_host);
#endif /* DEBUG */
				} else {
					srvrlog(fs, "ok");
					fs->fs_flags |= FSF_VALID;
				}
			}

			/*
			 * Adjust ping interval
			 */
			untimeout(fs->fs_cid);
			fs->fs_cid = timeout(fs->fs_pinger, nfs_keepalive, (voidp) fs);

			/*
			 * Update ttl for this server
			 */
			np->np_ttl = clocktime() +
				(MAX_ALLOWED_PINGS - 1) * FAST_NFS_PING + fs->fs_pinger - 1;

			/*
			 * New RPC xid...
			 */
			np->np_xid = NPXID_ALLOC();

			/*
			 * Failed pings is zero...
			 */
			np->np_ping = 0;

			/*
			 * Recompute portmap information if not known
			 */
			if (np->np_mountd_inval)
				recompute_portmap(fs);

			found_map++;
			break;
		}
	}

#ifdef DEBUG
	if (found_map == 0)
		dlog("Spurious ping packet");
#endif /* DEBUG */
}

/*
 * Called when no ping-reply received
 */
static void nfs_timed_out P((fserver *fs));
static void nfs_timed_out(fs)
fserver *fs;
{
	nfs_private *np = (nfs_private *) fs->fs_private;

	/*
	 * Not known to be up any longer
	 */
	if (FSRV_ISUP(fs)) {
		fs->fs_flags &= ~FSF_VALID;
		srvrlog(fs, "not responding");
	}

	/*
	 * Another ping has failed
	 */
	np->np_ping++;

	/*
	 * If ttl has expired then guess that it is dead
	 */
	if (np->np_ttl < clocktime()) {
		if ((fs->fs_flags & FSF_DOWN) == 0) {
			/*
			 * Server was up, but is now down.
			 */
			srvrlog(fs, "is down");
			fs->fs_flags |= FSF_DOWN|FSF_VALID;
			if (fs->fs_flags & FSF_WANT)
				wakeup_srvr(fs);
			/*
			 * Since the server is down, the portmap
			 * information may now be wrong, so it
			 * must be flushed from the local cache
			 */
			flush_nfs_fhandle_cache(fs);
			np->np_error = -1;
			/*
			 * Pretend just one ping has failed now
			 */
			np->np_ping = 1;
		} else {
			/*
			 * Known to be down
			 */
			fs->fs_flags |= FSF_VALID;
		}
	} else {
#ifdef DEBUG
		if (np->np_ping > 1)
			dlog("%d pings to %s failed - at most %d allowed", np->np_ping, fs->fs_host, MAX_ALLOWED_PINGS);
#endif /* DEBUG */
	}

	/*
	 * Run keepalive again
	 */
	nfs_keepalive(fs);
}

/*
 * Keep track of whether a server is alive
 */
static void nfs_keepalive P((fserver *fs));
static void nfs_keepalive(fs)
fserver *fs;
{
	int error;
	nfs_private *np = (nfs_private *) fs->fs_private;
	int fstimeo = -1;

	/*
	 * Send an NFS ping to this node
	 */

	if (ping_len == 0)
		start_ping();

	/*
	 * Queue the packet...
	 */
	error = fwd_packet(MK_RPC_XID(RPC_XID_NFSPING, np->np_xid), (voidp) ping_buf,
		ping_len, fs->fs_ip, (struct sockaddr_in *) 0, (voidp) np->np_xid, nfs_pinged);

	/*
	 * See if a hard error occured
	 */
	switch (error) {
	case ENETDOWN:
	case ENETUNREACH:
	case EHOSTDOWN:
	case EHOSTUNREACH:
		np->np_ping = MAX_ALLOWED_PINGS;	/* immediately down */
		np->np_ttl = (time_t) 0;
		/*
		 * This causes an immediate call to nfs_timed_out
		 * whenever the server was thought to be up.
		 * See +++ below.
		 */
		fstimeo = 0;
		break;

	case 0:
#ifdef DEBUG
		dlog("Sent NFS ping to %s", fs->fs_host);
#endif /* DEBUG */
		break;
	}

#ifdef DEBUG
	/*dlog("keepalive, ping = %d", np->np_ping);*/
#endif /* DEBUG */

	/*
	 * Back off the ping interval if we are not getting replies and
	 * the remote system is know to be down.
	 */
	switch (fs->fs_flags & (FSF_DOWN|FSF_VALID)) {
	case FSF_VALID:			/* Up */
		if (fstimeo < 0)	/* +++ see above */
			fstimeo = FAST_NFS_PING;
		break;

	case FSF_VALID|FSF_DOWN:	/* Down */
		fstimeo = fs->fs_pinger;
		break;

	default:			/* Unknown */
		fstimeo = FAST_NFS_PING;
		break;
	}

#ifdef DEBUG
	dlog("NFS timeout in %d seconds", fstimeo);
#endif /* DEBUG */

	fs->fs_cid = timeout(fstimeo, nfs_timed_out, (voidp) fs);
}

int nfs_srvr_port(fs, port, wchan)
fserver *fs;
u_short *port;
voidp wchan;
{
	int error = -1;
	if ((fs->fs_flags & FSF_VALID) == FSF_VALID) {
		if ((fs->fs_flags & FSF_DOWN) == 0) {
			nfs_private *np = (nfs_private *) fs->fs_private;
			if (np->np_error == 0) {
				*port = np->np_mountd;
				/*
				 * Now go get it again in case it changed
				 */
				np->np_mountd_inval = TRUE;
				error = 0;
			} else {
				if (np->np_error < 0)
					recompute_portmap(fs);
				error = np->np_error;
			}
		} else {
			error = EWOULDBLOCK;
		}
	}
	if (error < 0 && wchan && !(fs->fs_flags & FSF_WANT)) {
		/*
		 * If a wait channel is supplied, and no
		 * error has yet occured, then arrange
		 * that a wakeup is done on the wait channel,
		 * whenever a wakeup is done on this fs node.
		 * Wakeup's are done on the fs node whenever
		 * it changes state - thus causing control to
		 * come back here and new, better things to happen.
		 */
		fs->fs_flags |= FSF_WANT;
		sched_task(wakeup_task, wchan, (voidp) fs);
	}
	return error;
}

static void start_nfs_pings P((fserver *fs, int pingval));
static void start_nfs_pings(fs, pingval)
fserver *fs;
int pingval;
{
	if (!(fs->fs_flags & FSF_PINGING)) {
		fs->fs_flags |= FSF_PINGING;
		if (fs->fs_cid)
			untimeout(fs->fs_cid);
		if (pingval < 0) {
			srvrlog(fs, "wired up");
			fs->fs_flags |= FSF_VALID;
			fs->fs_flags &= ~FSF_DOWN;
		} else {
			nfs_keepalive(fs);
		}
	} else {
#ifdef DEBUG
		dlog("Already running pings to %s", fs->fs_host);
#endif /* DEBUG */
	}
}

/*
 * Find an nfs server for a host.
 */
fserver *find_nfs_srvr P((mntfs *mf));
fserver *find_nfs_srvr(mf)
mntfs *mf;
{
	fserver *fs;
	struct hostent *hp = 0;
	char *host = mf->mf_fo->opt_rhost;
	struct sockaddr_in *ip;
	nfs_private *np;
	int pingval;

	/*
	 * Get ping interval from mount options.
	 * Current only used to decide whether pings
	 * are required or not.  < 0 = no pings.
	 */
	{ struct mntent mnt;
	  mnt.mnt_opts = mf->mf_fo->opt_opts;
	  pingval = hasmntval(&mnt, "ping");
#ifdef HAS_TCP_NFS
	  /*
	   * Over TCP mount, don't bother to do pings.
	   * This is experimental - maybe you want to
	   * do pings anyway...
	   */
	  if (pingval == 0 && hasmntopt(&mnt, "tcp"))
		pingval = -1;
#endif /* HAS_TCP_NFS */
	}


top:
	/*
	 * Scan the list of known servers looking
	 * for one with the same name
	 */
	ITER(fs, fserver, &nfs_srvr_list) {
		if (STREQ(host, fs->fs_host)) {
			start_nfs_pings(fs, pingval);
			fs->fs_refc++;
			return fs;
		}
	}

	/*
	 * If the name is not known, it may be
	 * because it was an alternate name for
	 * the same machine.  So do a lookup and
	 * try again with the primary name if that
	 * is different.
	 * All that assuming it wasn't normalized
	 * earlier of course...
	 */
	if (hp == 0) {
		hp = gethostbyname(host);
		if (hp && !STREQ(host, hp->h_name) && !normalize_hosts) {
			host = hp->h_name;
			goto top;
		}
	}

	/*
	 * Get here if we can't find an entry
	 */
	if (hp) {
		switch (hp->h_addrtype) {
		case AF_INET:
			ip = ALLOC(sockaddr_in);
			bzero((voidp) ip, sizeof(*ip));
			ip->sin_family = AF_INET;
			ip->sin_addr = *(struct in_addr *) hp->h_addr;
			ip->sin_port = htons(NFS_PORT);
			break;

		default:
			ip = 0;
			break;
		}
	} else {
		ip = 0;
	}

	/*
	 * Allocate a new server
	 */
	fs = ALLOC(fserver);
	fs->fs_refc = 1;
	fs->fs_host = strdup(hp ? hp->h_name : "unknown_hostname");
	host_normalize(&fs->fs_host);
	fs->fs_ip = ip;
	fs->fs_cid = 0;
	if (ip) {
		fs->fs_flags = FSF_DOWN;	/* Starts off down */
	} else {
		fs->fs_flags = FSF_ERROR|FSF_VALID;
		mf->mf_flags |= MFF_ERROR;
		mf->mf_error = ENOENT;
	}
	fs->fs_type = "nfs";
	fs->fs_pinger = AM_PINGER;
	np = ALLOC(nfs_private);
	bzero((voidp) np, sizeof(*np));
	np->np_mountd_inval = TRUE;
	np->np_xid = NPXID_ALLOC();
	np->np_error = -1;
	/*
	 * Initially the server will be deemed dead after
	 * MAX_ALLOWED_PINGS of the fast variety have failed.
	 */
	np->np_ttl = clocktime() + MAX_ALLOWED_PINGS * FAST_NFS_PING - 1;
	fs->fs_private = (voidp) np;
	fs->fs_prfree = (void (*)()) free;

	if (!(fs->fs_flags & FSF_ERROR)) {
		/*
		 * Start of keepalive timer
		 */
		start_nfs_pings(fs, pingval);
	}

	/*
	 * Add to list of servers
	 */
	ins_que(&fs->fs_q, &nfs_srvr_list);

	return fs;
}
