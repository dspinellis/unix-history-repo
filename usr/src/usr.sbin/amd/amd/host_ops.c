/*
 * $Id: host_ops.c,v 5.2 90/06/23 22:19:26 jsp Rel $
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
 *	@(#)host_ops.c	5.1 (Berkeley) %G%
 */

#include "am.h"

#ifdef HAS_HOST

#include "mount.h"
#include <sys/stat.h>

/*
 * NFS host file system
 */

/*
 * Define HOST_RPC_UDP to use dgram instead of stream RPC.
 * Datagrams are generally much faster.
 */
#define	HOST_RPC_UDP

/*
 * Define HOST_MKDIRS to make Amd automatically try
 * to create the mount points.
 */
#define HOST_MKDIRS

/*
 * Execute needs the same as NFS plus a helper command
 */
static int host_match(fo)
am_opts *fo;
{
#ifdef HOST_EXEC
	if (!host_helper) {
		plog(XLOG_USER, "No host helper command given");
		return FALSE;
	}
#endif /* HOST_EXEC */

	/*
	 * Make sure rfs is specified to keep nfs_match happy...
	 */
	if (!fo->opt_rfs)
		fo->opt_rfs = "/";

	if (!(*nfs_ops.fs_match)(fo))
		return FALSE;

	return TRUE;
}

static int host_init(mf)
mntfs *mf;
{
	if (strchr(mf->mf_info, ':') == 0)
		return ENOENT;
	return 0;
}

/*
 * Two implementations:
 * HOST_EXEC gets you the external version.  The program specified with
 * the -h option is called.  The external program is not published...
 * roll your own.
 *
 * Otherwise you get the native version.  Faster but makes the program
 * bigger.
 */

#ifndef HOST_EXEC

static bool_t
xdr_pri_free(xdr_args, args_ptr)
xdrproc_t xdr_args;
caddr_t args_ptr;
{
	XDR xdr;
	xdr.x_op = XDR_FREE;
	return ((*xdr_args)(&xdr, args_ptr));
}

static int do_mount(fhp, dir, fs_name, opts, mf)
fhstatus *fhp;
char *dir;
char *fs_name;
char *opts;
mntfs *mf;
{
	struct stat stb;
#ifdef DEBUG
	dlog("host: mounting fs %s on %s\n", fs_name, dir);
#endif /* DEBUG */
#ifdef HOST_MKDIRS
	(void) mkdirs(dir, 0555);
#endif /* HOST_MKDIRS */
	if (stat(dir, &stb) < 0 || (stb.st_mode & S_IFMT) != S_IFDIR) {
		plog(XLOG_ERROR, "No mount point for %s - skipping", dir);
		return ENOENT;
	}

	return mount_nfs_fh(fhp, dir, fs_name, opts, mf);
}

static sortfun(a, b)
exports *a,*b;
{
	return strcmp((*a)->ex_dir, (*b)->ex_dir);
}

/*
 * Get filehandle
 */
static int fetch_fhandle(client, dir, fhp)
CLIENT *client;
char *dir;
fhstatus *fhp;
{
	struct timeval tv;
	enum clnt_stat clnt_stat;

	/*
	 * Pick a number, any number...
	 */
	tv.tv_sec = 10;
	tv.tv_usec = 0;

#ifdef DEBUG
	dlog("Fetching fhandle for %s", dir);
#endif /* DEBUG */
	/*
	 * Call the mount daemon on the remote host to
	 * get the filehandle.
	 */
	clnt_stat = clnt_call(client, MOUNTPROC_MNT, xdr_dirpath, &dir, xdr_fhstatus, fhp, tv);
	if (clnt_stat != RPC_SUCCESS) {
		extern char *clnt_sperrno();
		char *msg = clnt_sperrno(clnt_stat);
		plog(XLOG_ERROR, "mountd rpc failed: %s", msg);
		return EIO;
	}
	/*
	 * Check status of filehandle
	 */
	if (fhp->fhs_status) {
#ifdef DEBUG
		errno = fhp->fhs_status;
		dlog("fhandle fetch failed: %m");
#endif /* DEBUG */
		return fhp->fhs_status;
	}
	return 0;
}

/*
 * Mount the export tree from a host
 */
static int host_mount(mp)
am_node *mp;
{
	struct timeval tv2;
	CLIENT *client;
	enum clnt_stat clnt_stat;
	int n_export;
	int j;
	exports exlist = 0, ex;
	exports *ep = 0;
	fhstatus *fp = 0;
	mntfs *mf = mp->am_mnt;
	char *host = mf->mf_server->fs_host;
	int error = 0;
	struct sockaddr_in sin;
	int sock = RPC_ANYSOCK;
	int ok = FALSE;
	
#ifdef HOST_RPC_UDP
	struct timeval tv;
	tv.tv_sec = 10; tv.tv_usec = 0;
#endif /* HOST_RPC_UDP */

	/*
	 * Take a copy of the server address
	 */
	sin = *mf->mf_server->fs_ip;

	/*
	 * Zero out the port - make sure we recompute
	 */
	sin.sin_port = 0;
	/*
	 * Make a client end-point
	 */
#ifdef HOST_RPC_UDP
	if ((client = clntudp_create(&sin, MOUNTPROG, MOUNTVERS, tv, &sock)) == NULL)
#else
	if ((client = clnttcp_create(&sin, MOUNTPROG, MOUNTVERS, &sock, 0, 0)) == NULL)
#endif /* HOST_RPC_UDP */
	{
		plog(XLOG_ERROR, "Failed to make rpc connection to mountd on %s", host);
		error = EIO;
		goto out;
	}

	if (!nfs_auth) {
		nfs_auth = authunix_create_default();
		if (!nfs_auth) {
			error = ENOBUFS;
			goto out;
		}
	}

	client->cl_auth = nfs_auth;

#ifdef DEBUG
	dlog("Fetching export list from %s", host);
#endif /* DEBUG */

	/*
	 * Fetch the export list
	 */
	tv2.tv_sec = 10; tv2.tv_usec = 0;
	clnt_stat = clnt_call(client, MOUNTPROC_EXPORT, xdr_void, 0, xdr_exports, &exlist, tv2);
	if (clnt_stat != RPC_SUCCESS) {
		/*clnt_perror(client, "rpc");*/
		error = EIO;
		goto out;
	}

	/*
	 * Figure out how many exports were returned
	 */
	for (n_export = 0, ex = exlist; ex; ex = ex->ex_next) {
		/*printf("export %s\n", ex->ex_dir);*/
		n_export++;
	}
#ifdef DEBUG
	/*dlog("%d exports returned\n", n_export);*/
#endif /* DEBUG */

	/*
	 * Allocate an array of pointers into the list
	 * so that they can be sorted.
	 */
	ep = (exports *) xmalloc(n_export * sizeof(exports));
	for (j = 0, ex = exlist; ex; ex = ex->ex_next, j++)
		ep[j] = ex;

	/*
	 * Sort into order.
	 * This way the mounts are done in order down the tree,
	 * instead of any random order returned by the mount
	 * daemon (the protocol doesn't specify...).
	 */
	qsort(ep, n_export, sizeof(exports), sortfun);

	/*
	 * Allocate an array of filehandles
	 */
	fp = (fhstatus *) xmalloc(n_export * sizeof(fhstatus));

	/*
	 * Try to obtain filehandles for each directory.
	 * If a fetch fails then just zero out the array
	 * reference but discard the error.
	 */
	for (j = 0; j < n_export; j++) {
		if (error = fetch_fhandle(client, ep[j]->ex_dir, &fp[j]))
			ep[j] = 0;
	}

	/*
	 * Mount each filesystem for which we have a filehandle.
	 * If any of the mounts succeed then mark "ok" and return
	 * error code 0 at the end.  If they all fail then return
	 * the last error code.
	 */
	for (j = 0; j < n_export; j++) {
		ex = ep[j];
		if (ex) {
			char fs_name[MAXPATHLEN];
			char mntpt[MAXPATHLEN];
			sprintf(fs_name, "%s:%s", host, ex->ex_dir);
			if (strcmp(ex->ex_dir, "/") == 0)
				strcpy(mntpt, mf->mf_mount);
			else
				sprintf(mntpt, "%s%s", mf->mf_mount, ex->ex_dir);
			error = do_mount(&fp[j], mntpt, fs_name, mf->mf_fo->opt_opts, mf);
			if (!error)
				ok = TRUE;
		}
	}

	/*
	 * Clean up and exit
	 */
out:
	if (ep)
		free(ep);
	if (fp)
		free(fp);
	if (client)
		clnt_destroy(client);
	if (exlist)
		xdr_pri_free(xdr_exports, &exlist);
	if (ok)
		return 0;
	return error;
}

/*
 * Return true if pref is a directory prefix of dir.
 *
 * TODO:
 * Does not work if pref is "/".
 */
static int directory_prefix(pref, dir)
char *pref;
char *dir;
{
	int len = strlen(pref);
	if (strncmp(pref, dir, len) != 0)
		return FALSE;
	if (dir[len] == '/' || dir[len] == '\0')
		return TRUE;
	return FALSE;
}

/*
 * Unmount a mount tree
 */
static int host_umount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;
	mntlist *ml, *mprev;
	int xerror = 0;

	/*
	 * Read the mount list
	 */
	mntlist *mlist = read_mtab(mf->mf_mount);

	/*
	 * Unlock the mount list
	 */
	unlock_mntlist();

	/*
	 * Reverse list...
	 */
	ml = mlist;
	mprev = 0;
	while (ml) {
		mntlist *ml2 = ml->mnext;
		ml->mnext = mprev;
		mprev = ml;
		ml = ml2;
	}
	mlist = mprev;

	/*
	 * Unmount all filesystems...
	 */
	for (ml = mlist; ml; ml = ml->mnext) {
		char *dir = ml->mnt->mnt_dir;
		if (directory_prefix(mf->mf_mount, dir)) {
			int error;
#ifdef DEBUG
			dlog("host: unmounts %s", dir);
#endif /* DEBUG */
			/*
			 * Unmount "dir"
			 */
			error = UMOUNT_FS(dir);
			/*
			 * Keep track of errors
			 */
			if (error) {
				if (!xerror)
					xerror = error;
				if (error != EBUSY) {
					errno = error;
					plog("Tree unmount of %s failed: %m", ml->mnt->mnt_dir);
				}
			} else {
#ifdef HOST_MKDIRS
				(void) rmdirs(dir);
#endif /* HOST_MKDIRS */
			}
		}
	}

	/*
	 * Throw away mount list
	 */
	discard_mntlist(mlist);

	return xerror;
}

#else /* HOST_EXEC */

static int host_exec(op, host, fs, opts)
char *op;
char *host;
char *fs;
char *opts;
{
	int error;
	char *argv[7];

	/*
	 * Build arg vector
	 */
	argv[0] = host_helper;
	argv[1] = host_helper;
	argv[2] = op;
	argv[3] = host;
	argv[4] = fs;
	argv[5] = opts && *opts ? opts : "rw,default";
	argv[6] = 0;

	/*
	 * Put stdout to stderr
	 */
	(void) fclose(stdout);
	(void) dup(fileno(logfp));
	if (fileno(logfp) != fileno(stderr)) {
		(void) fclose(stderr);
		(void) dup(fileno(logfp));
	}
	/*
	 * Try the exec
	 */
#ifdef DEBUG
	Debug(D_FULL) {
		char **cp = argv;
		plog(XLOG_DEBUG, "executing (un)mount command...");
		while (*cp) {
	  		plog(XLOG_DEBUG, "arg[%d] = '%s'", cp-argv, *cp);
			cp++;
		}
	}
#endif /* DEBUG */
	if (argv[0] == 0 || argv[1] == 0) {
		errno = EINVAL;
		plog(XLOG_USER, "1st/2nd args missing to (un)mount program");
	} else {
		(void) execv(argv[0], argv+1);
	}
	/*
	 * Save error number
	 */
	error = errno;
	plog(XLOG_ERROR, "exec %s failed: %m", argv[0]);

	/*
	 * Return error
	 */
	return error;
}

static int host_mount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return host_exec("mount", mf->mf_server->fs_host, mf->mf_mount, mf->mf_fo->opt_opts);
}

static int host_umount(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	return host_exec("unmount", mf->mf_server->fs_host, mf->mf_mount, "xxx");
}

#endif /* HOST_EXEC */

/*
 * Ops structure
 */
am_ops host_ops = {
	"host",
	host_match,
	host_init,
	host_mount,
	host_umount,
	efs_lookuppn,
	efs_readdir,
	0, /* host_readlink */
	0, /* host_mounted */
	0, /* host_umounted */
	find_nfs_srvr,
	FS_MKMNT|FS_BACKGROUND|FS_AMQINFO
};

#endif /* HAS_HOST */
