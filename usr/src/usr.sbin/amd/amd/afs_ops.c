/*
 * $Id: afs_ops.c,v 5.2 90/06/23 22:19:14 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)afs_ops.c	5.1 (Berkeley) %G%
 */

#include "am.h"

#define NFS
#define NFSCLIENT

#include <sys/stat.h>
#ifdef NFS_3
typedef nfs_fh fhandle_t;
#endif /* NFS_3 */
#ifdef NFS_HDR
#include NFS_HDR
#endif /* NFS_HDR */
#include <sys/mount.h>
#include "mount.h"

/*
 * Automount file system
 */

/*
 * Interval between forced retries of a mount.
 */
#define RETRY_INTERVAL	2

/*
 * AFS needs nothing in particular.
 */
static int afs_match(fo)
am_opts *fo;
{
	char *p = fo->opt_rfs;
	if (!fo->opt_rfs) {
		plog(XLOG_USER, "auto: no mount point named (rfs:=)");
		return 0;
	}
	if (!fo->opt_fs) {
		plog(XLOG_USER, "auto: no map named (fs:=)");
		return 0;
	}
	/*
	 * Swap round fs:= and rfs:= options
	 * ... historical (jsp)
	 */
	fo->opt_rfs = fo->opt_fs;
	fo->opt_fs = p;
	/*
	 * fs_mtab turns out to be the name of the mount map
	 */
	fo->fs_mtab = strealloc(fo->fs_mtab, fo->opt_rfs ? fo->opt_rfs : ".");
	return 1;
}

static int afs_init(mf)
mntfs *mf;
{
	/*
	 * Fill in attribute fields
	 */
	mf->mf_fattr.type = NFDIR;
	mf->mf_fattr.mode = NFSMODE_DIR | 0555;
	mf->mf_fattr.nlink = 2;
	mf->mf_fattr.size = 512;

	return 0;
}

/*
 * Mount the an automounter directory.
 * The automounter is connected into the system
 * as a user-level NFS server.  mount_afs constructs
 * the necessary NFS parameters to be given to the
 * kernel so that it will talk back to us.
 */
static int mount_afs(dir, fs_name, opts)
char *dir;
char *fs_name;
char *opts;
{
	struct nfs_args nfs_args;
	struct mntent mnt;
	int retry;
	struct sockaddr_in sin;
	unsigned short port;
	int flags;
	extern nfs_fh *root_fh();
	nfs_fh *fhp;
	char fs_hostname[MAXHOSTNAMELEN+MAXPATHLEN+1];

	MTYPE_TYPE type = MOUNT_TYPE_NFS;

	bzero((voidp) &nfs_args, sizeof(nfs_args));	/* Paranoid */

	mnt.mnt_dir = dir;
	mnt.mnt_fsname = fs_name;
	mnt.mnt_type = MNTTYPE_AUTO;
	mnt.mnt_opts = opts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;

	retry = hasmntval(&mnt, "retry");
	if (retry <= 0)
		retry = 2;	/* XXX */

	/*
	 * get fhandle of remote path for automount point
	 */
	
	fhp = root_fh(fs_name);
	if (!fhp) {
		plog(XLOG_FATAL, "Can't find root file handle for %s", fs_name);
		return EINVAL;
	}

	NFS_FH_DREF(nfs_args.fh, (NFS_FH_TYPE) fhp);

	/*
	 * Create sockaddr to point to the local machine.  127.0.0.1
	 * is not used since that will not work in HP-UX clusters and
	 * this is no more expensive.
	 */
	bzero((voidp) &sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr = myipaddr;
	if (port = hasmntval(&mnt, "port")) {
		sin.sin_port = htons(port);
	} else {
		plog(XLOG_ERROR, "no port number specified for %s", fs_name);
		return EINVAL;
	}

	/*
	 * set mount args
	 */
	NFS_SA_DREF(nfs_args, &sin);

	/*
	 * Make a ``hostname'' string for the kernel
	 */
#ifndef HOSTNAMESZ
#define	SHORT_MOUNT_NAME
#endif /* HOSTNAMESZ */
#ifdef SHORT_MOUNT_NAME
	sprintf(fs_hostname, "amd:%d", mypid);
#else
	sprintf(fs_hostname, "pid%d@%s:%s", mypid, hostname, dir);
#endif /* SHORT_MOUNT_NAME */
	nfs_args.hostname = fs_hostname;
	nfs_args.flags |= NFSMNT_HOSTNAME;
#ifdef HOSTNAMESZ
	/*
	 * Most kernels have a name length restriction.
	 */
	if (strlen(fs_hostname) >= HOSTNAMESZ)
		strcpy(fs_hostname + HOSTNAMESZ - 3, "..");
#endif /* HOSTNAMESZ */

	/*
	 * Parse a subset of the standard nfs options.  The
	 * others are probably irrelevant for this application
	 */
	if (nfs_args.timeo = hasmntval(&mnt, "timeo"))
		nfs_args.flags |= NFSMNT_TIMEO;

	if (nfs_args.retrans = hasmntval(&mnt, "retrans"))
		nfs_args.flags |= NFSMNT_RETRANS;

#ifdef NFSMNT_BIODS
	if (nfs_args.biods = hasmntval(&mnt, "biods"))
		nfs_args.flags |= NFSMNT_BIODS;

#endif /* NFSMNT_BIODS */

#if defined(NFSMNT_ACREGMIN) && defined(NFSMNT_ACREGMAX)
	/*
	 * Don't cache attributes - they are changing under
	 * the kernel's feet...
	 */
	nfs_args.acregmin = nfs_args.acregmax = 1;
	nfs_args.flags |= NFSMNT_ACREGMIN|NFSMNT_ACREGMAX;
#endif /* defined(NFSMNT_ACREGMIN) && defined(NFSMNT_ACREGMAX) */
	/*
	 * These two are constructed internally by the calling routine
	 */
	if (hasmntopt(&mnt, MNTOPT_SOFT) != NULL)
		nfs_args.flags |= NFSMNT_SOFT;

#ifdef MNTOPT_INTR
	if (hasmntopt(&mnt, MNTOPT_INTR) != NULL)
		nfs_args.flags |= NFSMNT_INT;
#endif /* MNTOPT_INTR */

	flags = compute_mount_flags(&mnt);
#ifdef ULTRIX_HACK
	nfs_args.gfs_flags = flags;
	flags &= M_RDONLY;
	if (flags & M_RDONLY)
		nfs_args.flags |= NFSMNT_RONLY;
#endif /* ULTRIX_HACK */
	return mount_fs(&mnt, flags, (caddr_t) &nfs_args, retry, type);
}

static int afs_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	/*
	 * There are two cases to consider...
	 */
	if (mp->am_parent && mp->am_parent->am_parent) {
		/*
		 * If this am_node has a parent which is not the root node, in
		 * which case we are supplying a pseudo-directory, in which
		 * case no action is needed.  Pseudo-directories are used to
		 * provide some structure to the automounted directories instead
		 * of putting them all in the top-level automount directory.
		 */
		mp->am_parent->am_mnt->mf_fattr.nlink++;
		/*
		 * Info field of . means use parent's info field.
		 */
		if (mf->mf_info[0] == '.' && mf->mf_info[1] == '\0')
			mf->mf_info = strealloc(mf->mf_info, mp->am_parent->am_mnt->mf_info);
		/*
		 * Compute prefix:
		 *
		 * If there is an option prefix then use that else
		 * If the parent had a prefix then use that with name
		 *	of this node appended else
		 * Use the name of this node.
		 *
		 * That means if you want no prefix you must say so
		 * in the map.
		 */
		if (mf->mf_fo->opt_pref) {
			/*
			 * the prefix specified as an option
			 */
			mp->am_pref = strdup(mf->mf_fo->opt_pref);
		} else {
			/*
			 * else the parent's prefix
			 * followed by the name
			 * followed by /
			 */
			char *ppref = mp->am_parent->am_pref;
			if (ppref == 0)
				ppref = "";
			mp->am_pref = str3cat((char *) 0, ppref, mp->am_name, "/");
		}
	} else {
		/*
		 * Otherwise, we are mounting the automounter.  In which case
		 * we need to make sure the mount directory exists, construct
		 * the mount options and call the mount_afs routine.
		 */
		struct stat stb;
		char opts[256];
		int error;

		/*
		 * Top-level mount - so make
		 * sure the mount point exists
		 * and is a directory.
		 */
		error = mkdirs(mp->am_path, 0555);
		if (error)
			return error;
		mp->am_flags |= AMF_MKPATH;

		if (stat(mp->am_path, &stb) < 0) {
			return errno;
		} else if ((stb.st_mode & S_IFMT) != S_IFDIR) {
			plog(XLOG_WARNING, "%s is not a directory", mp->am_path);
			return ENOTDIR;
		}

		mf->mf_mount = strealloc(mf->mf_mount, mp->am_path);

		/*
		 * Construct some mount options
		 */
		sprintf(opts,
#ifdef MNTOPT_INTR
			"%s,%s,%s=%d,%s=%d,%s=%d,%sdirect",
			MNTOPT_INTR,
#else
			"%s,%s=%d,%s=%d,%s=%d,%sdirect",
#endif /* MNTOPT_INTR */
#ifdef AUTOMOUNT_RO
			MNTOPT_RO,	/* You don't really want this... */
#else
			"rw",
#endif /* AUTOMOUNT_RO */
			"port", nfs_port,
			"timeo", afs_timeo,
			"retrans", afs_retrans,
			mf->mf_ops == &afs_ops ? "in" : "");

		error = mount_afs(mp->am_path, mp->am_name, opts);
		if (error) {
			errno = error;
			plog(XLOG_FATAL, "mount_afs: %m");
			return error;
		}
		mp->am_name = pid_fsname;
	}

	/*
	 * Build a new map cache for this node, or re-use
	 * an existing cache for the same map.
	 */
	{ char *cache;
	  if (mf->mf_fo->opt_cache)
	  	cache = mf->mf_fo->opt_cache;
	  else
	  	cache = "none";
	  mf->mf_private = (voidp) mapc_find(mf->mf_info, cache);
	  mf->mf_prfree = mapc_free;
	}

	return 0;
}

/*
 * Unmount an automount node
 */
static int afs_umount(mp)
am_node *mp;
{
	int error;

	/*
	 * If this is a pseudo-directory then just adjust the link count
	 * in the parent, otherwise call the generic unmount routine
	 */
	if (!mp->am_parent) {
		error = 0;
	} else if (mp->am_parent && mp->am_parent->am_parent) {
		--mp->am_parent->am_mnt->mf_fattr.nlink;
		error = 0;
	} else {
		struct stat stb;
again:
		/*
		 * The lstat is needed if this mount is type=direct.
		 * When that happens, the kernel cache gets confused
		 * between the underlying type (dir) and the mounted
		 * type (link) and so needs to be re-synced before
		 * the unmount.  This is all because the unmount system
		 * call follows links and so can't actually unmount
		 * a link (stupid!).  It was noted that doing an ls -ld
		 * of the mount point to see why things were not working
		 * actually fixed the problem - so simulate an ls -ld here.
		 */
		if (lstat(mp->am_path, &stb) < 0) {
#ifdef DEBUG
			dlog("lstat(%s): %m", mp->am_path);
#endif /* DEBUG */
		}
		error = UMOUNT_FS(mp->am_path);
		if (error == EBUSY) {
			plog(XLOG_WARNING, "afs_unmount retrying %s in 1s", mp->am_path);
			sleep(1);	/* XXX */
			goto again;
		}
	}

	return error;
}

/*
 * Unmount an automount node
 */
static void afs_umounted(mp)
am_node *mp;
{
	/*
	 * If this is a pseudo-directory then just adjust the link count
	 * in the parent, otherwise call the generic unmount routine
	 */
	if (mp->am_parent && mp->am_parent->am_parent)
		--mp->am_parent->am_mnt->mf_fattr.nlink;
}

/*
 * Mounting a file system may take a significant period of time.  The
 * problem is that if this is done in the main process thread then
 * the entire automounter could be blocked, possibly hanging lots of
 * processes on the system.  Instead we use a continuation scheme to
 * allow mounts to be attempted in a sub-process.  When the sub-process
 * exits we pick up the exit status (by convention a UN*X error number)
 * and continue in a notifier.  The notifier gets handed a data structure
 * and can then determine whether the mount was successful or not.  If
 * not, it updates the data structure and tries again until there are no
 * more ways to try the mount, or some other permanent error occurs.
 * In the mean time no RPC reply is sent, even after the mount is succesful.
 * We rely on the RPC retry mechanism to resend the lookup request which
 * can then be handled.
 */


struct continuation {
	char **ivec;		/* Current mount info */
	am_node *mp;		/* Node we are trying to mount */
	char *key;		/* Map key */
	char *info;		/* Info string */
	char **xivec;		/* Saved strsplit vector */
	char *opts;		/* Mount options */
	am_opts fs_opts;	/* Filesystem options */
	char *def_opts;		/* Default options */
	int retry;		/* Try again? */
	int tried;		/* Have we tried any yet? */
	time_t start;		/* Time we started this mount */
	int callout;		/* Callout identifier */
};

/*
 * Discard an old continuation
 */
static void free_continuation(cp)
struct continuation *cp;
{
	if (cp->callout)
		untimeout(cp->callout);
	free((voidp) cp->key);
	free((voidp) cp->xivec);
	free((voidp) cp->info);
	free((voidp) cp->opts);
	free((voidp) cp->def_opts);
	free_opts(&cp->fs_opts);
	free((voidp) cp);
}

static int afs_bgmount P((struct continuation*, int));

/*
 * Discard the underlying mount point and replace
 * with a reference to an error filesystem.
 */
static void assign_error_mntfs(mp)
am_node *mp;
{
	if (mp->am_error > 0) {
		/*
		 * Save the old error code
		 */
		int error = mp->am_error;
		/*
		 * Discard the old filesystem
		 */
		free_mntfs(mp->am_mnt);
		/*
		 * Allocate a new error reference
		 */
		mp->am_mnt = new_mntfs();
		/*
		 * Put back the error code
		 */
		mp->am_mnt->mf_error = error;
		mp->am_mnt->mf_flags |= MFF_ERROR;
		/*
		 * Zero the error in the mount point
		 */
		mp->am_error = 0;
	}
}

/*
 * The continuation function.  This is called by
 * the task notifier when a background mount attempt
 * completes.
 */
static void afs_cont(rc, term, closure)
int rc;
int term;
voidp closure;
{
	struct continuation *cp = (struct continuation *) closure;
	mntfs *mf = cp->mp->am_mnt;

	/*
	 * Definitely not trying to mount at the moment
	 */
	mf->mf_flags &= ~MFF_MOUNTING;
	/*
	 * While we are mounting - try to avoid race conditions
	 */
	new_ttl(cp->mp);

	/*
	 * Wakeup anything waiting for this mount
	 */
	wakeup((voidp) mf);

	/*
	 * Check for termination signal or exit status...
	 */
	if (rc || term) {
		if (term) {
			/*
			 * Not sure what to do for an error code.
			 */
			mf->mf_error = EIO;	/* XXX ? */
			mf->mf_flags |= MFF_ERROR;
			plog(XLOG_ERROR, "mount for %s got signal %d", cp->mp->am_path, term);
		} else {
			/*
			 * Check for exit status...
			 */
			mf->mf_error = rc;
			mf->mf_flags |= MFF_ERROR;
			errno = rc;	/* XXX */
			plog(XLOG_ERROR, "%s: mount (afs_cont): %m", cp->mp->am_path);
		}

		/*
		 * If we get here then that attempt didn't work, so
		 * move the info vector pointer along by one and
		 * call the background mount routine again
		 */
		amd_stats.d_merr++;
		cp->ivec++;
		(void) afs_bgmount(cp, 0);
		assign_error_mntfs(cp->mp);
	} else {
		/*
		 * The mount worked.
		 */
		am_mounted(cp->mp);
		free_continuation(cp);
	}

	reschedule_timeout_mp();
}

/*
 * Retry a mount
 */
/*ARGSUSED*/
static void afs_retry(rc, term, closure)
int rc;
int term;
voidp closure;
{
	struct continuation *cp = (struct continuation *) closure;
	int error = 0;

#ifdef DEBUG
	dlog("Commencing retry for mount of %s", cp->mp->am_path);
#endif /* DEBUG */

	if ((cp->start + ALLOWED_MOUNT_TIME) < clocktime()) {
		/*
		 * The entire mount has timed out.
		 * Set the error code and skip past
		 * all the info vectors so that
		 * afs_bgmount will not have any more
		 * ways to try the mount, so causing
		 * an error.
		 */
		plog(XLOG_INFO, "mount of \"%s\" has timed out", cp->mp->am_path);
		error = ETIMEDOUT;
		new_ttl(cp->mp);
		while (*cp->ivec)
			cp->ivec++;
	}

	(void) afs_bgmount(cp, error);
	reschedule_timeout_mp();
}

/*
 * Try to mount a file system.  Can be called
 * directly or in a sub-process by run_task
 */
static int try_mount(mp)
am_node *mp;
{
	/*
	 * Mount it!
	 */
	int error;

	error = mount_node(mp);
#ifdef DEBUG
	if (error) {
		errno = error;
		dlog("afs call to mount_node failed: %m");
	}
#endif /* DEBUG */
	return error;
}

/*
 * Pick a file system to try mounting and
 * do that in the background if necessary
 *
For each location:
	if it is new -defaults then
		extract and process
		continue;
	fi
	if it is a cut then
		if a location has been tried then
			break;
		fi
		continue;
	fi
	parse mount location
	discard previous mount location if required
	find matching mounted filesystem
	if not applicable then
		this_error = No such file or directory
		continue
	fi
	if the filesystem failed to be mounted then
		this_error = error from filesystem
	elif the filesystem is mounting or unmounting then
		this_error = -1
	elif the fileserver is down then
		this_error = -1
	elif the filesystem is already mounted
		this_error = 0
		break
	fi
	if no error on this mount then
		this_error = initialise mount point
	fi
	if no error on this mount and mount is delayed then
		this_error = -1
	fi
	if this_error < 0 then
		retry = true
	fi
	if no error on this mount then
		make mount point if required
	fi
	if no error on this mount then
		if mount in background then
			run mount in background
			return -1
		else
			this_error = mount in foreground
		fi
	fi
	if an error occured on this mount then
		update stats
		save error in mount point
	fi
endfor
 */

static int afs_bgmount(cp, mpe)
struct continuation *cp;
int mpe;
{
	mntfs *mf = cp->mp->am_mnt;	/* Current mntfs */
	mntfs *mf_retry = 0;		/* First mntfs which needed retrying */
	int this_error = -1;		/* Per-mount error */
	int hard_error = -1;
	int mp_error = mpe;

	/*
	 * Try to mount each location.
	 * At the end:
	 * hard_error == 0 indicates something was mounted.
	 * hard_error > 0 indicates everything failed with a hard error
	 * hard_error < 0 indicates nothing could be mounted now
	 */
	for (; this_error && *cp->ivec; cp->ivec++) {
		am_ops *p;
		am_node *mp = cp->mp;
		char *link_dir;
		int dont_retry;

		if (hard_error < 0)
			hard_error = this_error;

		this_error = -1;

		if (**cp->ivec == '-') {
			/*
			 * Pick up new defaults
			 */
			if (cp->opts && *cp->opts)
				cp->def_opts = str3cat(cp->def_opts, cp->opts, ";", *cp->ivec+1);
			else
				cp->def_opts = strealloc(cp->def_opts, *cp->ivec+1);
#ifdef DEBUG
			dlog("Setting def_opts to \"%s\"", cp->def_opts);
#endif /* DEBUG */
			continue;
		}

		/*
		 * If a mount has been attempted, and we find
		 * a cut then don't try any more locations.
		 */
		if (strcmp(*cp->ivec, "/") == 0 || strcmp(*cp->ivec, "||") == 0) {
			if (cp->tried) {
#ifdef DEBUG
				dlog("Cut: not trying any more locations for %s",
					mp->am_path);
#endif /* DEBUG */
				break;
			}
			continue;
		}

#ifdef SUNOS4_COMPAT
		/*
		 * By default, you only get this bit on SunOS4.
		 * If you want this anyway, then define SUNOS4_COMPAT
		 * in the relevant "os-blah.h" file.
		 *
		 * We make the observation that if the local key line contains
		 * no '=' signs then either it is sick, or it is a SunOS4-style
		 * "host:fs[:link]" line.  In the latter case the am_opts field
		 * is also assumed to be in old-style, so you can't mix & match.
		 * You can use ${} expansions for the fs and link bits though...
		 *
		 * Actually, this doesn't really cover all the possibilities for
		 * the latest SunOS automounter and it is debatable whether there
		 * is any point bothering.
		 */
		if (strchr(*cp->ivec, '=') == 0)
			p = sunos4_match(&cp->fs_opts, *cp->ivec, cp->def_opts, mp->am_path, cp->key, mp->am_parent->am_mnt->mf_info);
		else
#endif /* SUNOS4_COMPAT */
			p = ops_match(&cp->fs_opts, *cp->ivec, cp->def_opts, mp->am_path, cp->key, mp->am_parent->am_mnt->mf_info);

		/*
		 * Find a mounted filesystem for this node.
		 */
		mp->am_mnt = mf = realloc_mntfs(mf, p, &cp->fs_opts, cp->fs_opts.opt_fs,
						cp->fs_opts.fs_mtab, cp->opts);

		p = mf->mf_ops;
#ifdef DEBUG
		dlog("Got a hit with %s", p->fs_type);
#endif /* DEBUG */
		/*
		 * Note whether this is a real mount attempt
		 */
		if (p == &efs_ops) {
			plog(XLOG_MAP, "Map entry %s for %s failed to match", *cp->ivec, mp->am_path);
			if (this_error <= 0)
				this_error = ENOENT;
			continue;
		} else {
			if (cp->fs_opts.fs_mtab) {
				plog(XLOG_MAP, "Trying mount of %s on %s fstype %s",
					cp->fs_opts.fs_mtab, mp->am_path, p->fs_type);
			}
			cp->tried = TRUE;
		}

		this_error = 0;
		dont_retry = FALSE;

		if (mp->am_link) {
			free(mp->am_link);
			mp->am_link = 0;
		}

		link_dir = mf->mf_fo->opt_sublink;

		if (link_dir && *link_dir) {
			if (*link_dir == '/') {
				mp->am_link = strdup(link_dir);
			} else {
				mp->am_link = str3cat((char *) 0,
					mf->mf_fo->opt_fs, "/", link_dir);
			}
		}

		if (mf->mf_error > 0) {
			this_error = mf->mf_error;
		} else if (mf->mf_flags & (MFF_MOUNTING|MFF_UNMOUNTING)) {
			/*
			 * Still mounting - retry later
			 */
#ifdef DEBUG
			dlog("Duplicate pending mount fstype %s", p->fs_type);
#endif /* DEBUG */
			this_error = -1;
		} else if (FSRV_ISDOWN(mf->mf_server)) {
			/*
			 * Would just mount from the same place
			 * as a hung mount - so give up
			 */
#ifdef DEBUG
			dlog("%s is already hung - giving up", mf->mf_mount);
#endif /* DEBUG */
			mp_error = EWOULDBLOCK;
			dont_retry = TRUE;
			this_error = -1;
		} else if (mf->mf_flags & MFF_MOUNTED) {
#ifdef DEBUG
			dlog("duplicate mount of \"%s\" ...", mf->mf_info);
#endif /* DEBUG */
			this_error = 0;
			new_ttl(mp);
			break;
		}

		/*
		 * Will usually need to play around with the mount nodes
		 * file attribute structure.  This must be done here.
		 */
		if (!this_error) {
			/*
			 * Fill in attribute fields
			 */
			mf->mf_fattr.type = NFLNK;
			mf->mf_fattr.mode = NFSMODE_LNK | 0777;
			mf->mf_fattr.nlink = 1;
			mf->mf_fattr.size = MAXPATHLEN / 4;	/* Conservative */
			mf->mf_fattr.fileid = mp->am_gen;

			if (p->fs_init)
				this_error = (*p->fs_init)(mf);
		}

		if (!this_error && mf->mf_fo->opt_delay) {
			/*
			 * If there is a delay timer on the mount
			 * then don't try to mount if the timer
			 * has not expired.
			 */
			int i = atoi(mf->mf_fo->opt_delay);
			if (i > 0 && (cp->start + i) < clocktime()) {
#ifdef DEBUG
				dlog("Mount of %s delayed by %ds", mf->mf_mount, i);
#endif /* DEBUG */
				this_error = -1;
			}
		}

		if (this_error < 0 && !dont_retry) {
			if (!mf_retry)
				mf_retry = dup_mntfs(mf);
			cp->retry = TRUE;
		}

		if (!this_error) {
			/*
			 * If the directory is not yet made and
			 * it needs to be made, then make it!
			 */
			 if (!(mf->mf_flags & MFF_MKMNT) &&
				 	p->fs_flags & FS_MKMNT) {
				this_error = mkdirs(mf->mf_mount, 0555);
				if (!this_error)
					mf->mf_flags |= MFF_MKMNT;
			}
		}

		if (!this_error)
		if (p->fs_flags & FS_MBACKGROUND) {
			mf->mf_flags |= MFF_MOUNTING;	/*XXX*/
#ifdef DEBUG
			dlog("backgrounding mount of \"%s\"", mf->mf_info);
#endif /* DEBUG */
			if (cp->callout) {
				untimeout(cp->callout);
				cp->callout = 0;
			}
			run_task(try_mount, (voidp) mp, afs_cont, (voidp) cp);
			if (mf_retry) free_mntfs(mf_retry);
			return -1;
		} else {
#ifdef DEBUG
			dlog("foreground mount of \"%s\" ...", mf->mf_info);
#endif /* DEBUG */
			this_error = try_mount(mp);
		}

		if (this_error >= 0) {
			if (this_error > 0) {
				amd_stats.d_merr++;
				if (mf != mf_retry) {
					mf->mf_error = this_error;
					mf->mf_flags |= MFF_ERROR;
				}
			}
			/*
			 * Wakeup anything waiting for this mount
			 */
			wakeup((voidp) mf);
		}
	}

	if (this_error && cp->retry) {
		free_mntfs(mf);
		mf = cp->mp->am_mnt = mf_retry;
		/*
		 * Not retrying again (so far)
		 */
		cp->retry = FALSE;
		cp->tried = FALSE;
		/*
		 * Start at the beginning.
		 * Rewind the location vector and
		 * reset the default options.
		 */
		cp->ivec = cp->xivec;
		cp->def_opts = strealloc(cp->def_opts, cp->opts);
		/*
		 * Arrange that afs_bgmount is called
		 * after anything else happens.
		 */
#ifdef DEBUG
		dlog("Arranging to retry mount of %s", cp->mp->am_path);
#endif /* DEBUG */
		sched_task(afs_retry, (voidp) cp, (voidp) mf);
		if (cp->callout)
			untimeout(cp->callout);
		cp->callout = timeout(RETRY_INTERVAL, wakeup, (voidp) mf);

		cp->mp->am_ttl = clocktime() + RETRY_INTERVAL;

		/*
		 * Not done yet - so don't return anything
		 */
		return -1;
	}

	/*
	 * Discard handle on duff filesystem.
	 * This should never happen since it
	 * should be caught by the case above.
	 */
	if (mf_retry) {
		plog(XLOG_ERROR, "discarding a retry mntfs for %s", mf_retry->mf_mount);
		free_mntfs(mf_retry);
	}

	if (hard_error < 0 || !this_error)
		hard_error = this_error;

	/*
	 * If we get here, then either the mount succeeded or
	 * there is no more mount information available.
	 */
	if (hard_error < 0 && mp_error)
		hard_error = cp->mp->am_error = mp_error;
	if (hard_error > 0) {
		/*
		 * Set a small(ish) timeout on an error node if
		 * the error was not a time out.
		 */
		switch (hard_error) {
		case ETIMEDOUT:
		case EWOULDBLOCK:
			cp->mp->am_timeo = 5;
			break;
		default:
			cp->mp->am_timeo = 17;
			break;
		}
		cp->mp->am_timeo_w = 0;
	}

	/*
	 * Make sure that the error value in the mntfs has a
	 * reasonable value.
	 */
	if (mf->mf_error < 0) {
		mf->mf_error = hard_error;
		if (hard_error)
			mf->mf_flags |= MFF_ERROR;
	}

	/*
	 * In any case we don't need the continuation any more
	 */
	free_continuation(cp);

	return hard_error;
}

/*
 * Automount interface to RPC lookup routine
 */
static am_node *afs_lookuppn(mp, fname, error_return, op)
am_node *mp;
char *fname;
int *error_return;
int op;
{
#define ereturn(x) { *error_return = x; return 0; }

	/*
	 * Find the corresponding entry and return
	 * the file handle for it.
	 */
	am_node *ap, *new_mp, *ap_hung;
	char *info;			/* Mount info - where to get the file system */
	char **ivec, **xivec;		/* Split version of info */
	char *opts;			/* Mount options */
	int error = 0;			/* Error so far */
	char path_name[MAXPATHLEN];	/* General path name buffer */
	char *pfname;			/* Path for database lookup */
	struct continuation *cp;	/* Continuation structure if we need to mount */
	int in_progress = 0;		/* # of (un)mount in progress */
	char *dflts;
	mntfs *mf;

#ifdef DEBUG
	dlog("in afs_lookuppn");
#endif /* DEBUG */

	/*
	 * If the server is shutting down
	 * then don't return information
	 * about the mount point.
	 */
	if (amd_state == Finishing) {
#ifdef DEBUG
		dlog("%s/%s mount ignored - going down",
			mp->am_path, fname);
#endif /* DEBUG */
		ereturn(ENOENT);
	}

	/*
	 * Handle special case of "." and ".."
	 */
	if (fname[0] == '.') {
		if (fname[1] == '\0')
			return mp;	/* "." is the current node */
		if (fname[1] == '.' && fname[2] == '\0') {
			if (mp->am_parent) {
#ifdef DEBUG
				dlog(".. in %s gives %s", mp->am_path, mp->am_parent->am_path);
#endif /* DEBUG */
				return mp->am_parent;	/* ".." is the parent node */
			}
			ereturn(ESTALE);
		}
	}

	/*
	 * Check for valid key name.
	 * If it is invalid then pretend it doesn't exist.
	 */
	if (!valid_key(fname)) {
		plog(XLOG_WARNING, "Key \"%s\" contains a disallowed character", fname);
		ereturn(ENOENT);
	}

	/*
	 * Expand key name.
	 * fname is now a private copy.
	 */
	fname = expand_key(fname);

	for (ap_hung = 0, ap = mp->am_child; ap; ap = ap->am_osib) {
		/*
		 * Otherwise search children of this node
		 */
		if (FSTREQ(ap->am_name, fname)) {
			mf = ap->am_mnt;
			if (ap->am_error) {
				error = ap->am_error;
				continue;
			}

			/*
			 * If the error code is undefined then it must be
			 * in progress.
			 */
			if (mf->mf_error < 0)
				goto in_progrss;

			/*
			 * Check for a hung node
			 */
			if (FSRV_ISDOWN(mf->mf_server)) {
				ap_hung = ap;
				continue;
			}

			/*
			 * If there was a previous error with this node
			 * then return that error code.
			 */
			if (mf->mf_flags & MFF_ERROR) {
				error = mf->mf_error;
				continue;
			}

			if (!(mf->mf_flags & MFF_MOUNTED) /*|| (mf->mf_flags & MFF_UNMOUNTING)*/) {
in_progrss:
				/*
				 * If the fs is not mounted or it is unmounting then there
				 * is a background (un)mount in progress.  In this case
				 * we just drop the RPC request (return nil) and
				 * wait for a retry, by which time the (un)mount may
				 * have completed.
				 */
#ifdef DEBUG
				dlog("ignoring mount of %s in %s -- in progress",
					fname, mf->mf_mount);
#endif /* DEBUG */
				in_progress++;
				continue;
			}

			/*
			 * Otherwise we have a hit: return the current mount point.
			 */
#ifdef DEBUG
			dlog("matched %s in %s", fname, ap->am_path);
#endif /* DEBUG */
			free(fname);
			return ap;
		}
	}

	if (in_progress) {
#ifdef DEBUG
		dlog("Waiting while %d mount(s) in progress", in_progress);
#endif /* DEBUG */
		free(fname);
		ereturn(-1);
	}

	/*
	 * If an error occured then return it.
	 */
	if (error) {
#ifdef DEBUG
		errno = error; /* XXX */
		dlog("Returning error: %m", error);
#endif /* DEBUG */
		free(fname);
		ereturn(error);
	}

	/*
	 * If doing a delete then don't create again!
	 */
	switch (op) {
	case VLOOK_DELETE:
		ereturn(ENOENT);
		break;

	case VLOOK_CREATE:
		break;

	default:
		plog(XLOG_FATAL, "Unknown op to afs_lookuppn: 0x%x", op);
		ereturn(EINVAL);
		break;
	}

	/*
	 * If the server is going down then just return,
	 * don't try to mount any more file systems
	 */
	if ((int)amd_state >= (int)Finishing) {
#ifdef DEBUG
		dlog("not found - server going down anyway");
#endif /* DEBUG */
		free(fname);
		ereturn(ENOENT);
	}

	/*
	 * If we get there then this is a reference to an,
	 * as yet, unknown name so we need to search the mount
	 * map for it.
	 */
	if (mp->am_pref) {
		sprintf(path_name, "%s%s", mp->am_pref, fname);
		pfname = path_name;
	} else {
		pfname = fname;
	}

	mf = mp->am_mnt;

#ifdef DEBUG
	dlog("will search map info in %s to find %s", mf->mf_info, pfname);
#endif /* DEBUG */
	/*
	 * Consult the oracle for some mount information.
	 * info is malloc'ed and belongs to this routine.
	 * It ends up being free'd in free_continuation().
	 *
	 * Note that this may return -1 indicating that information
	 * is not yet available.
	 */
	error = mapc_search((mnt_map*) mf->mf_private, pfname, &info);
	if (error) {
		plog(XLOG_MAP, "No map entry for %s", pfname);
		free(fname);
		ereturn(error);
	}

#ifdef DEBUG
	dlog("mount info is %s", info);
#endif /* DEBUG */

	/*
	 * Split info into an argument vector.
	 * The vector is malloc'ed and belongs to
	 * this routine.  It is free'd in free_continuation()
	 */
	xivec = ivec = strsplit(info, '\"');

	/*
	 * Default error code...
	 */
	if (ap_hung)
		error = EWOULDBLOCK;
	else
		error = ENOENT;

	/*
	 * Allocate a new map
	 */
	new_mp = exported_ap_alloc();
	if (new_mp == 0) {
		free((voidp) xivec);
		free((voidp) info);
		free((voidp) fname);
		ereturn(ENOSPC);
	}

	if (mf->mf_opts)
		opts = mf->mf_opts;
	else
		opts = "";

	opts = strdup(opts);

#ifdef DEBUG
	dlog("searching for /defaults entry");
#endif /* DEBUG */
	if (mapc_search((mnt_map*) mf->mf_private, "/defaults", &dflts) == 0) {
	  	char *dfl;
		char **rvec;
#ifdef DEBUG
		dlog("/defaults gave %s", dflts);
#endif /* DEBUG */
		if (*dflts == '-')
			dfl = dflts+1;
		else
			dfl = dflts;

		/*
		 * Chop the defaults up
		 */
		rvec = strsplit(dfl, '\"');
		/*
		 * Extract first value
		 */
		dfl = rvec[0];

		/*
		 * Log error if there were other values
		 */
		if (rvec[1]) {
#ifdef DEBUG
			dlog("/defaults chopped into %s", dfl);
#endif /* DEBUG */
			plog(XLOG_USER, "More than a single value for /defaults in %s", mf->mf_info);
		}

		/*
		 * Don't need info vector any more
		 */
		free((voidp) rvec);

		/*
		 * If there were any values at all...
		 */
		if (dfl) {
			/*
			 * Prepend to existing defaults if they exist,
			 * otherwise just use these defaults.
			 */
			if (*opts && *dfl) {
				char *nopts = (char *) xmalloc(strlen(opts)+strlen(dfl)+2);
				sprintf(nopts, "%s;%s", dfl, opts);
				free(opts);
				opts = nopts;
			} else if (*dfl) {
				opts = strealloc(opts, dfl);
			}
		}
		free(dflts);
	}

	/*
	 * Fill it in
	 */
	init_map(new_mp, fname);

	/*
	 * Put it in the table
	 */
	insert_am(new_mp, mp);

	/*
	 * Fill in some other fields,
	 * path and mount point
	 */
	new_mp->am_path = str3cat(new_mp->am_path, mp->am_path, *fname == '/' ? "" : "/", fname);

#ifdef DEBUG
	dlog("setting path to %s", new_mp->am_path);
#endif /* DEBUG */

	/*
	 * Take private copy of pfname
	 */
	pfname = strdup(pfname);

	/*
	 * Construct a continuation
	 */
	cp = ALLOC(continuation);
	cp->mp = new_mp;
	cp->xivec = xivec;
	cp->ivec = ivec;
	cp->info = info;
	cp->key = pfname;
	cp->opts = opts;
	cp->retry = FALSE;
	cp->tried = FALSE;
	cp->start = clocktime();
	cp->def_opts = strdup(opts);
	bzero((voidp) &cp->fs_opts, sizeof(cp->fs_opts));

	/*
	 * Try and mount the file system
	 * If this succeeds immediately (possible
	 * for a ufs file system) then return
	 * the attributes, otherwise just
	 * return an error.
	 */
	error = afs_bgmount(cp, error);
	reschedule_timeout_mp();
	if (!error) {
		free(fname);
		return new_mp;
	}

	assign_error_mntfs(cp->mp);

	free(fname);

	ereturn(error);
#undef ereturn
}

/*
 * Locate next node in sibling list which is mounted
 * and is not an error node.
 */
static am_node *next_nonerror_node(xp)
am_node *xp;
{
	mntfs *mf;

	/*
	 * Bug report (7/12/89) from Rein Tollevik <rein@ifi.uio.no>
	 * Fixes a race condition when mounting direct automounts.
	 * Also fixes a problem when doing a readdir on a directory
	 * containing hung automounts.
	 */
	while (xp &&
	       (!(mf = xp->am_mnt) ||			/* No mounted filesystem */
	        mf->mf_error != 0 ||			/* There was a mntfs error */
	        xp->am_error != 0 ||			/* There was a mount error */
	        !(mf->mf_flags & MFF_MOUNTED) ||	/* The fs is not mounted */
	        (mf->mf_server->fs_flags & FSF_DOWN))	/* The fs may be down */
		)
		xp = xp->am_osib;

	return xp;
}

static int afs_readdir(mp, cookie, dp, ep)
am_node *mp;
nfscookie cookie;
struct dirlist *dp;
struct entry *ep;
{
	unsigned int gen = *(unsigned int*) cookie;
	am_node *xp;

	dp->eof = FALSE;

	if (gen == 0) {
		/*
		 * In the default instance (which is used to
		 * start a search) we return "." and "..".
		 *
		 * This assumes that the count is big enough
		 * to allow both "." and ".." to be returned in
		 * a single packet.  If it isn't (which would
		 * be fairly unbelievable) then tough.
		 */
#ifdef DEBUG
		dlog("default search");
#endif /* DEBUG */
		xp = next_nonerror_node(mp->am_child);
		dp->entries = ep;

		/* construct "." */
		ep[0].fileid = mp->am_gen;
		ep[0].name = ".";
		ep[0].nextentry = &ep[1];
		*(unsigned int *) ep[0].cookie = 0;

		/* construct ".." */
		if (mp->am_parent)
			ep[1].fileid = mp->am_parent->am_gen;
		else
			ep[1].fileid = mp->am_gen;
		ep[1].name = "..";
		ep[1].nextentry = 0;
		*(unsigned int *) ep[1].cookie =
			xp ? xp->am_gen : ~(unsigned int)0;

		if (!xp) dp->eof = TRUE;
		return 0;
	}

#ifdef DEBUG
	dlog("real child");
#endif /* DEBUG */

	if (gen == ~(unsigned int)0) {
#ifdef DEBUG
		dlog("End of readdir in %s", mp->am_path);
#endif /* DEBUG */
		dp->eof = TRUE;
		dp->entries = 0;
		return 0;
	}

	xp = mp->am_child;
	while (xp && xp->am_gen != gen)
		xp = xp->am_osib;

	if (xp) {
		am_node *xp_next = next_nonerror_node(xp->am_osib);

		if (xp_next) {
			*(unsigned int *) ep->cookie = xp_next->am_gen;
		} else {
			*(unsigned int *) ep->cookie = ~(unsigned int)0;
			dp->eof = TRUE;
		}

		ep->fileid = xp->am_gen;
		ep->name = xp->am_name;

		ep->nextentry = 0;
		dp->entries = ep;

		return 0;
	}

	return ESTALE;

}

static am_node *dfs_readlink(mp, error_return)
am_node *mp;
int *error_return;
{
	am_node *xp;
	int rc = 0;

	xp = next_nonerror_node(mp->am_child);
	if (!xp)
		xp = afs_lookuppn(mp, mp->am_path+1, &rc, VLOOK_CREATE);

	if (xp) {
		new_ttl(xp);	/* (7/12/89) from Rein Tollevik */
		return xp;
	}
	if (amd_state == Finishing)
		rc = ENOENT;
	*error_return = rc;
	return 0;
}

/*
 * Ops structure
 */
am_ops afs_ops = {
	"auto",
	afs_match,
	afs_init,
	afs_mount,
	afs_umount,
	afs_lookuppn,
	afs_readdir,
	0, /* afs_readlink */
	0, /* afs_mounted */
	afs_umounted,
	find_afs_srvr,
	FS_NOTIMEOUT|FS_UBACKGROUND|FS_AMQINFO
};

am_ops dfs_ops = {
	"direct",
	afs_match,
	0, /* dfs_init */
	afs_mount,
	afs_umount,
	efs_lookuppn,
	efs_readdir,
	dfs_readlink,
	0, /* afs_mounted */
	afs_umounted,
	find_afs_srvr,
	FS_NOTIMEOUT|FS_UBACKGROUND|FS_AMQINFO
};
