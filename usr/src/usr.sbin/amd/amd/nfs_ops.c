/*
 * $Id: nfs_ops.c,v 5.2 90/06/23 22:19:45 jsp Rel $
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
 *	@(#)nfs_ops.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"

#ifdef HAS_NFS

#define NFS
#define NFSCLIENT
#ifdef NFS_3
typedef nfs_fh fhandle_t;
#endif /* NFS_3 */
#ifdef NFS_HDR
#include NFS_HDR
#endif /* NFS_HDR */
#include <sys/mount.h>
#include "mount.h"

/*
 * Network file system
 */

/*
 * Convert from nfsstat to UN*X error code
 */
#define unx_error(e)	((int)(e))

/*
 * The NFS layer maintains a cache of file handles.
 * This is *fundamental* to the implementation and
 * also allows quick remounting when a filesystem
 * is accessed soon after timing out.
 *
 * The NFS server layer knows to flush this cache
 * when a server goes down so avoiding stale handles.
 *
 * Each cache entry keeps a hard reference to
 * the corresponding server.  This ensures that
 * the server keepalive information is maintained.
 *
 * The copy of the sockaddr_in here is taken so
 * that the port can be twiddled to talk to mountd
 * instead of portmap or the NFS server as used
 * elsewhere.
 * The port# is flushed if a server goes down.
 * The IP address is never flushed - we assume
 * that the address of a mounted machine never
 * changes.  If it does, then you have other
 * problems...
 */
typedef struct fh_cache fh_cache;
struct fh_cache {
	qelem	fh_q;			/* List header */
	voidp	fh_wchan;		/* Wait channel */
	int	fh_error;		/* Valid data? */
	int	fh_id;			/* Unique id */
	int	fh_cid;			/* Callout id */
	struct fhstatus fh_handle;	/* Handle on filesystem */
	struct sockaddr_in fh_sin;	/* Address of mountd */
	fserver *fh_fs;			/* Server holding filesystem */
	char	*fh_path;		/* Filesystem on host */
};

/*
 * FH_TTL is the time a file handle will remain in the cache since
 * last being used.  If the file handle becomes invalid, then it
 * will be flushed anyway.
 */
#define	FH_TTL		(5 * 60)		/* five minutes */
#define	FH_TTL_ERROR	(30)			/* 30 seconds */

static int fh_id = 0;
#define	FHID_ALLOC()	(++fh_id)
extern qelem fh_head;
qelem fh_head = { &fh_head, &fh_head };

static int call_mountd P((fh_cache*, unsigned long, fwd_fun, voidp));

AUTH *nfs_auth;

static fh_cache *find_nfs_fhandle_cache P((voidp idv, int done));
static fh_cache *find_nfs_fhandle_cache(idv, done)
voidp idv;
int done;
{
	fh_cache *fp, *fp2 = 0;
	int id = (int) idv;

	ITER(fp, fh_cache, &fh_head) {
		if (fp->fh_id == id) {
			fp2 = fp;
			break;
		}
	}

#ifdef DEBUG
	if (fp2) {
		dlog("fh cache gives fp %#x, fs %s", fp2, fp2->fh_path);
	} else {
		dlog("fh cache search failed");
	}
#endif /* DEBUG */

	if (fp2 && !done) {
		fp2->fh_error = ETIMEDOUT;
		return 0;
	}

	return fp2;
}

/*
 * Called when a filehandle appears
 */
static void got_nfs_fh P((voidp pkt, int len, struct sockaddr_in *sa,
				struct sockaddr_in *ia, voidp idv, int done));
static void got_nfs_fh(pkt, len, sa, ia, idv, done)
voidp pkt;
int len;
struct sockaddr_in *sa, *ia;
voidp idv;
int done;
{
	fh_cache *fp = find_nfs_fhandle_cache(idv, done);
	if (fp) {
		fp->fh_error = pickup_rpc_reply(pkt, len, (voidp) &fp->fh_handle, xdr_fhstatus);
		if (!fp->fh_error) {
#ifdef DEBUG
			dlog("got filehandle for %s:%s", fp->fh_fs->fs_host, fp->fh_path);
#endif /* DEBUG */
			/*
			 * Wakeup anything sleeping on this filehandle
			 */
			if (fp->fh_wchan) {
#ifdef DEBUG
				dlog("Calling wakeup on %#x", fp->fh_wchan);
#endif /* DEBUG */
				wakeup(fp->fh_wchan);
			}
		}
	}
}

void flush_nfs_fhandle_cache P((fserver *fs));
void flush_nfs_fhandle_cache(fs)
fserver *fs;
{
	fh_cache *fp;
	ITER(fp, fh_cache, &fh_head) {
		if (fp->fh_fs == fs) {
			fp->fh_sin.sin_port = (u_short) 0;
			fp->fh_error = -1;
		}
	}
}

static void discard_fh P((fh_cache *fp));
static void discard_fh(fp)
fh_cache *fp;
{
	rem_que(&fp->fh_q);
#ifdef DEBUG
	dlog("Discarding filehandle for %s:%s", fp->fh_fs->fs_host, fp->fh_path);
#endif /* DEBUG */
	free_srvr(fp->fh_fs);
	free((voidp) fp->fh_path);
	free((voidp) fp);
}

/*
 * Determine the file handle for a node
 */
static int prime_nfs_fhandle_cache P((char *path, fserver *fs, struct fhstatus *fhbuf, voidp wchan));
static int prime_nfs_fhandle_cache(path, fs, fhbuf, wchan)
char *path;
fserver *fs;
struct fhstatus *fhbuf;
voidp wchan;
{
	fh_cache *fp, *fp_save = 0;
	int error;
	int reuse_id = FALSE;

#ifdef DEBUG
	dlog("Searching cache for %s:%s", fs->fs_host, path);
#endif /* DEBUG */

	/*
	 * First search the cache
	 */
	ITER(fp, fh_cache, &fh_head) {
		if (fs == fp->fh_fs && strcmp(path, fp->fh_path) == 0) {
			switch (fp->fh_error) {
			case 0:
				error = fp->fh_error = unx_error(fp->fh_handle.fhs_status);
				if (error == 0) {
					if (fhbuf)
						bcopy((voidp) &fp->fh_handle, (voidp) fhbuf,
							sizeof(fp->fh_handle));
					if (fp->fh_cid)
						untimeout(fp->fh_cid);
					fp->fh_cid = timeout(FH_TTL, discard_fh, (voidp) fp);
				} else if (error == EACCES) {
					/*
					 * Now decode the file handle return code.
					 */
					plog(XLOG_INFO, "Filehandle denied for \"%s:%s\"",
						fs->fs_host, path);
				} else {
					errno = error;	/* XXX */
					plog(XLOG_INFO, "Filehandle error for \"%s:%s\": %m",
						fs->fs_host, path);
				}

				/*
				 * The error was returned from the remote mount daemon.
				 * Policy: this error will be cached for now...
				 */
				return error;

			case -1:
				/*
				 * Still thinking about it, but we can re-use.
				 */
				fp_save = fp;
				reuse_id = TRUE;
				break;

			default:
				/*
				 * Return the error.
				 * Policy: make sure we recompute if required again
				 * in case this was caused by a network failure.
				 * This can thrash mountd's though...  If you find
				 * your mountd going slowly then:
				 * 1.  Add a fork() loop to main.
				 * 2.  Remove the call to innetgr() and don't use
				 *     netgroups, especially if you don't use YP.
				 */
				error = fp->fh_error;
				fp->fh_error = -1;
				return error;
			}
			break;
		}
	}

	/*
	 * Not in cache
	 */
	if (fp_save) {
		fp = fp_save;
		/*
		 * Re-use existing slot
		 */
		untimeout(fp->fh_cid);
		free_srvr(fp->fh_fs);
		free(fp->fh_path);
	} else {
		fp = ALLOC(fh_cache);
		bzero((voidp) fp, sizeof(*fp));
		ins_que(&fp->fh_q, &fh_head);
	}
	if (!reuse_id)
		fp->fh_id = FHID_ALLOC();
	fp->fh_wchan = wchan;
	fp->fh_error = -1;
	fp->fh_cid = timeout(FH_TTL, discard_fh, (voidp) fp);

	/*
	 * If the address has changed then don't try to re-use the
	 * port information
	 */
	if (fp->fh_sin.sin_addr.s_addr != fs->fs_ip->sin_addr.s_addr) {
		fp->fh_sin = *fs->fs_ip;
		fp->fh_sin.sin_port = 0;
	}
	fp->fh_fs = dup_srvr(fs);
	fp->fh_path = strdup(path);

	error = call_mountd(fp, MOUNTPROC_MNT, got_nfs_fh, wchan);
	if (error) {
		/*
		 * Local error - cache for a short period
		 * just to prevent thrashing.
		 */
		untimeout(fp->fh_cid);
		fp->fh_cid = timeout(error < 0 ? 2 * ALLOWED_MOUNT_TIME : FH_TTL_ERROR,
						discard_fh, (voidp) fp);
		fp->fh_error = error;
	} else {
		error = fp->fh_error;
	}
	return error;
}

static int call_mountd P((fh_cache *fp, u_long proc, fwd_fun f, voidp wchan));
static int call_mountd(fp, proc, f, wchan)
fh_cache *fp;
u_long proc;
fwd_fun f;
voidp wchan;
{
	struct rpc_msg mnt_msg;
	int len;
	char iobuf[8192];
	int error;

	if (!nfs_auth) {
		nfs_auth = authunix_create_default();
		if (!nfs_auth)
			return ENOBUFS;
	}

	if (fp->fh_sin.sin_port == 0) {
		u_short port;
		error = nfs_srvr_port(fp->fh_fs, &port, wchan);
		if (error)
			return error;
		fp->fh_sin.sin_port = port;
	}

	rpc_msg_init(&mnt_msg, MOUNTPROG, MOUNTVERS, (unsigned long) 0);
	len = make_rpc_packet(iobuf, sizeof(iobuf), proc,
			&mnt_msg, (voidp) &fp->fh_path, xdr_nfspath,  nfs_auth);

	if (len > 0) {
		error = fwd_packet(MK_RPC_XID(RPC_XID_MOUNTD, fp->fh_id),
			(voidp) iobuf, len, &fp->fh_sin, &fp->fh_sin, (voidp) fp->fh_id, f);
	} else {
		error = -len;
	}
	return error;
}

/*-------------------------------------------------------------------------*/

/*
 * NFS needs the local filesystem, remote filesystem
 * remote hostname.
 * Local filesystem defaults to remote and vice-versa.
 */
static int nfs_match(fo)
am_opts *fo;
{
	if (fo->opt_fs && !fo->opt_rfs)
		fo->opt_rfs = fo->opt_fs;
	if (!fo->opt_rfs) {
		plog(XLOG_USER, "nfs: no remote filesystem specified");
		return FALSE;
	}
	if (!fo->opt_rhost) {
		plog(XLOG_USER, "nfs: no remote host specified");
		return FALSE;
	}
	/*
	 * Determine magic cookie to put in mtab
	 */
	fo->fs_mtab = (char *) xrealloc(fo->fs_mtab, strlen(fo->opt_rhost) +
				strlen(fo->opt_rfs) + 2);
	sprintf(fo->fs_mtab, "%s:%s", fo->opt_rhost, fo->opt_rfs);
#ifdef DEBUG
	dlog("NFS: mounting remote server \"%s\", remote fs \"%s\" on \"%s\"",
		fo->opt_rhost, fo->opt_rfs, fo->opt_fs);
#endif /* DEBUG */

	return TRUE;
}

/*
 * Initialise am structure for nfs
 */
static int nfs_init(mf)
mntfs *mf;
{
	int error;
	char *colon = strchr(mf->mf_info, ':');
	if (colon == 0)
		return ENOENT;

	error = prime_nfs_fhandle_cache(colon+1, mf->mf_server, (struct fhstatus *) 0, (voidp) mf);

	return error;
}

mount_nfs_fh(fhp, dir, fs_name, opts, mf)
struct fhstatus *fhp;
char *dir;
char *fs_name;
char *opts;
mntfs *mf;
{
	struct nfs_args nfs_args;
	struct mntent mnt;
	int retry;
	char *colon;
	char *path;
	char host[MAXHOSTNAMELEN + MAXPATHLEN + 2];
	fserver *fs = mf->mf_server;
	int flags;
#ifdef notdef
	unsigned short port;
#endif /* notdef */

	MTYPE_TYPE type = MOUNT_TYPE_NFS;

	bzero((voidp) &nfs_args, sizeof(nfs_args));	/* Paranoid */

	/*
	 * Extract host name to give to kernel
	 */
	if (!(colon = strchr(fs_name, ':')))
		return ENOENT;
#ifndef NFS_ARGS_NEEDS_PATH
	*colon = '\0';
#endif
	strncpy(host, fs_name, sizeof(host));
#ifndef NFS_ARGS_NEEDS_PATH
	*colon = ':';
#endif /* NFS_ARGS_NEEDS_PATH */
	path = colon + 1;

	bzero((voidp) &nfs_args, sizeof(nfs_args));

	mnt.mnt_dir = dir;
	mnt.mnt_fsname = fs_name;
	mnt.mnt_type = MTAB_TYPE_NFS;
	mnt.mnt_opts = opts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;

	retry = hasmntval(&mnt, "retry");
	if (retry <= 0)
		retry = 1;	/* XXX */

/*again:*/

	/*
	 * set mount args
	 */
	NFS_FH_DREF(nfs_args.fh, (NFS_FH_TYPE) fhp->fhstatus_u.fhs_fhandle);

#ifdef ULTRIX_HACK
	nfs_args.optstr = mnt.mnt_opts;
#endif /* ULTRIX_HACK */

	nfs_args.hostname = host;
	nfs_args.flags |= NFSMNT_HOSTNAME;
#ifdef HOSTNAMESZ
	/*
	 * Most kernels have a name length restriction.
	 */
	if (strlen(host) >= HOSTNAMESZ)
		strcpy(host + HOSTNAMESZ - 3, "..");
#endif /* HOSTNAMESZ */

	if (nfs_args.rsize = hasmntval(&mnt, "rsize"))
		nfs_args.flags |= NFSMNT_RSIZE;

	if (nfs_args.wsize = hasmntval(&mnt, "wsize"))
		nfs_args.flags |= NFSMNT_WSIZE;

	if (nfs_args.timeo = hasmntval(&mnt, "timeo"))
		nfs_args.flags |= NFSMNT_TIMEO;

	if (nfs_args.retrans = hasmntval(&mnt, "retrans"))
		nfs_args.flags |= NFSMNT_RETRANS;

#ifdef NFSMNT_BIODS
	if (nfs_args.biods = hasmntval(&mnt, "biods"))
		nfs_args.flags |= NFSMNT_BIODS;

#endif /* NFSMNT_BIODS */

#ifdef notdef
/*
 * This isn't supported by the ping algorithm yet.
 * In any case, it is all done in nfs_init().
 */
 	if (port = hasmntval(&mnt, "port"))
		sin.sin_port = htons(port);
	else
		sin.sin_port = htons(NFS_PORT);	/* XXX should use portmapper */
#endif /* notdef */

	if (hasmntopt(&mnt, MNTOPT_SOFT) != NULL)
		nfs_args.flags |= NFSMNT_SOFT;

#ifdef MNTOPT_INTR
	if (hasmntopt(&mnt, MNTOPT_INTR) != NULL)
		nfs_args.flags |= NFSMNT_INT;
#endif /* MNTOPT_INTR */

#ifdef MNTOPT_NODEVS
	if (hasmntopt(&mnt, MNTOPT_NODEVS) != NULL)
		nfs_args.flags |= NFSMNT_NODEVS;
#endif /* MNTOPT_NODEVS */

#ifdef NFSMNT_PGTHRESH
	if (nfs_args.pg_thresh = hasmntval(&mnt, "pgthresh"))
		nfs_args.flags |= NFSMNT_PGTHRESH;
#endif /* NFSMNT_PGTHRESH */

	NFS_SA_DREF(nfs_args, fs->fs_ip);

	flags = compute_mount_flags(&mnt);

#ifdef HAS_TCP_NFS
	if (hasmntopt(&mnt, "tcp") != NULL)
		nfs_args.sotype = SOCK_STREAM;
#endif /* HAS_TCP_NFS */


#ifdef ULTRIX_HACK
	/*
	 * Ultrix passes the flags argument as part of the
	 * mount data structure, rather than using the
	 * flags argument to the system call.  This is
	 * confusing...
	 */
	if (!(nfs_args.flags & NFSMNT_PGTHRESH)) {
		nfs_args.pg_thresh = 64; /* 64k - XXX */
		nfs_args.flags |= NFSMNT_PGTHRESH;
	}
	nfs_args.gfs_flags = flags;
	flags &= M_RDONLY;
	if (flags & M_RDONLY)
		nfs_args.flags |= NFSMNT_RONLY;
#endif /* ULTRIX_HACK */

	return mount_fs(&mnt, flags, (caddr_t) &nfs_args, retry, type);
}

static mount_nfs(dir, fs_name, opts, mf)
char *dir;
char *fs_name;
char *opts;
mntfs *mf;
{
	int error;
	struct fhstatus fhs;
	char *colon;

	if (!(colon = strchr(fs_name, ':')))
		return ENOENT;

#ifdef DEBUG
	dlog("locating fhandle for %s", fs_name);
#endif /* DEBUG */
	error = prime_nfs_fhandle_cache(colon+1, mf->mf_server, &fhs, (voidp) 0);

	if (error)
		return error;

	return mount_nfs_fh(&fhs, dir, fs_name, opts, mf);
}

static int nfs_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	int error = mount_nfs(mf->mf_mount, mf->mf_info,
			mf->mf_fo->opt_opts, mf);

#ifdef DEBUG
	if (error) {
		errno = error;
		dlog("mount_nfs: %m");
	}
#endif /* DEBUG */
	return error;
}

static int nfs_umount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	int error = UMOUNT_FS(mf->mf_mount);
	if (error)
		return error;

	return 0;
}

static void nfs_umounted(mp)
am_node *mp;
{
#ifdef INFORM_MOUNTD
	/*
	 * Don't bother to inform remote mountd
	 * that we are finished.  Until a full
	 * track of filehandles is maintained
	 * the mountd unmount callback cannot
	 * be done correctly anyway...
	 */

	mntfs *mf = mp->am_mnt;
	fserver *fs;
	char *colon, *path;

	if (mf->mf_error || mf->mf_refc > 1)
		return;

	fs = mf->mf_server;

	/*
	 * Call the mount daemon on the server to
	 * announce that we are not using the fs any more.
	 *
	 * This is *wrong*.  The mountd should be called
	 * when the fhandle is flushed from the cache, and
	 * a reference held to the cached entry while the
	 * fs is mounted...
	 */
	colon = path = strchr(mf->mf_info, ':');
	if (fs && colon) {
		fh_cache f;
#ifdef DEBUG
		dlog("calling mountd for %s", mf->mf_info);
#endif /* DEBUG */
		*path++ = '\0';
		f.fh_path = path;
		f.fh_sin = *fs->fs_ip;
		f.fh_sin.sin_port = (u_short) 0;
		f.fh_fs = fs;
		f.fh_id = 0;
		f.fh_error = 0;
		(void) prime_nfs_fhandle_cache(colon+1, mf->mf_server, (struct fhstatus *) 0, (voidp) mf);
		(void) call_mountd(&f, MOUNTPROC_UMNT, (fwd_fun) 0, (voidp) 0);
		*colon = ':';
	}
#endif /* INFORM_MOUNTD */
}

/*
 * Network file system
 */
am_ops nfs_ops = {
	"nfs",
	nfs_match,
	nfs_init,
	nfs_mount,
	nfs_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* nfs_readlink */
	0, /* nfs_mounted */
	nfs_umounted,
	find_nfs_srvr,
	FS_MKMNT|FS_BACKGROUND|FS_AMQINFO
};

#endif /* HAS_NFS */
