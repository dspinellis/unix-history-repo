/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)srvr_afs.c	5.4 (Berkeley) %G%
 *
 * $Id: srvr_afs.c,v 5.2.2.1 1992/02/09 15:09:05 jsp beta $
 *
 */

/*
 * Automount FS server ("localhost") modeling
 */

#include "am.h"

extern qelem afs_srvr_list;
qelem afs_srvr_list = { &afs_srvr_list, &afs_srvr_list };

static fserver *localhost;

/*
 * Find an nfs server for the local host
 */
fserver *find_afs_srvr P((mntfs *));
fserver *find_afs_srvr(mf)
mntfs *mf;
{
	fserver *fs = localhost;

	if (!fs) {
		fs = ALLOC(fserver);
		fs->fs_refc = 0;
		fs->fs_host = strdup("localhost");
		fs->fs_ip = 0;
		fs->fs_cid = 0;
		fs->fs_pinger = 0;
		fs->fs_flags = FSF_VALID;
		fs->fs_type = "local";
		fs->fs_private = 0;
		fs->fs_prfree = 0;

		ins_que(&fs->fs_q, &afs_srvr_list);

		srvrlog(fs, "starts up");

		localhost = fs;
	}

	fs->fs_refc++;

	return fs;
}

/*------------------------------------------------------------------*/
		/* Generic routines follow */

/*
 * Wakeup anything waiting for this server
 */
void wakeup_srvr P((fserver *fs));
void wakeup_srvr(fs)
fserver *fs;
{
	fs->fs_flags &= ~FSF_WANT;
	wakeup((voidp) fs);
}

/*
 * Called when final ttl of server has expired
 */
static void timeout_srvr P((fserver *fs));
static void timeout_srvr(fs)
fserver *fs;
{
	/*
	 * If the reference count is still zero then
	 * we are free to remove this node
	 */
	if (fs->fs_refc == 0) {
#ifdef DEBUG
		dlog("Deleting file server %s", fs->fs_host);
#endif /* DEBUG */
		if (fs->fs_flags & FSF_WANT)
			wakeup_srvr(fs);

		/*
		 * Remove from queue.
		 */
		rem_que(&fs->fs_q);
		/*
		 * (Possibly) call the private free routine.
		 */
		if (fs->fs_private && fs->fs_prfree)
			(*fs->fs_prfree)(fs->fs_private);

		/*
		 * Free the net address
		 */
		if (fs->fs_ip)
			free((voidp) fs->fs_ip);

		/*
		 * Free the host name.
		 */
		free((voidp) fs->fs_host);

		/*
		 * Discard the fserver object.
		 */
		free((voidp) fs);
	}
}

/*
 * Free a file server
 */
void free_srvr P((fserver *fs));
void free_srvr(fs)
fserver *fs;
{
	if (--fs->fs_refc == 0) {
		/*
		 * The reference count is now zero,
		 * so arrange for this node to be
		 * removed in AM_TTL seconds if no
		 * other mntfs is referencing it.
		 */
		int ttl = (fs->fs_flags & (FSF_DOWN|FSF_ERROR)) ? 19 : AM_TTL;
#ifdef DEBUG
		dlog("Last hard reference to file server %s - will timeout in %ds", fs->fs_host, ttl);
#endif /* DEBUG */
		if (fs->fs_cid) {
			untimeout(fs->fs_cid);
			/*
			 * Turn off pinging - XXX
			 */
			fs->fs_flags &= ~FSF_PINGING;
		}
		/*
		 * Keep structure lying around for a while
		 */
		fs->fs_cid = timeout(ttl, timeout_srvr, (voidp) fs);
		/*
		 * Mark the fileserver down and invalid again
		 */
		fs->fs_flags &= ~FSF_VALID;
		fs->fs_flags |= FSF_DOWN;
	}
}

/*
 * Make a duplicate fserver reference
 */
fserver *dup_srvr P((fserver *fs));
fserver *dup_srvr(fs)
fserver *fs;
{
	fs->fs_refc++;
	return fs;
}

/*
 * Log state change
 */
void srvrlog P((fserver *fs, char *state));
void srvrlog(fs, state)
fserver *fs;
char *state;
{
	plog(XLOG_INFO, "file server %s type %s %s", fs->fs_host, fs->fs_type, state);
}
