/*
 * $Id: mount_fs.c,v 5.2 90/06/23 22:19:42 jsp Rel $
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
 *	@(#)mount_fs.c	5.1 (Berkeley) 6/29/90
 */

#include "am.h"
#ifdef NFS_3
typedef nfs_fh fhandle_t;
#endif /* NFS_3 */
#include <sys/mount.h>

/*
 * System Vr4 / SunOS 4.1 compatibility
 * - put dev= in the options list
 *
 * From: Brent Callaghan <brent@eng.sun.com>
 */
#define	MNTINFO_DEV	"dev"
#include <sys/stat.h>

/*
 * Standard mount flags
 */
#ifdef hpux
/*
 * HP-UX has an annoying feature of printing
 * error msgs on /dev/console
 */
#undef M_NOSUID
#endif /* hpux */

struct opt_tab mnt_flags[] = {
	{ "ro", M_RDONLY },
#ifdef M_CACHE
	{ "nocache", M_NOCACHE },
#endif /* M_CACHE */
#ifdef M_GRPID
	{ "grpid", M_GRPID },
#endif /* M_GRPID */
#ifdef M_MULTI
	{ "multi", M_MULTI },
#endif /* M_MULTI */
#ifdef M_NODEV
	{ "nodev", M_NODEV },
#endif /* M_NODEV */
#ifdef M_NOEXEC
	{ "noexec", M_NOEXEC },
#endif /* M_NOEXEC */
#ifdef M_NOSUB
	{ "nosub", M_NOSUB },
#endif /* M_NOSUB */
#ifdef M_NOSUID
	{ "nosuid", M_NOSUID },
#endif /* M_NOSUID */
#ifdef M_SYNC
	{ "sync", M_SYNC },
#endif /* M_SYNC */
	{ 0, 0 }
};

int compute_mount_flags(mnt)
struct mntent *mnt;
{
	struct opt_tab *opt;
	int flags;
#ifdef NFS_4
	flags = M_NEWTYPE;
#else
	flags = 0;
#endif /* NFS_4 */

	/*
	 * Crack basic mount options
	 */
	for (opt = mnt_flags; opt->opt; opt++)
		flags |= hasmntopt(mnt, opt->opt) ? opt->flag : 0;

	return flags;
}

int mount_fs(mnt, flags, mnt_data, retry, type)
struct mntent *mnt;
int flags;
caddr_t mnt_data;
int retry;
MTYPE_TYPE type;
{
	int error = 0;
	int automount = 0;
#ifdef MNTINFO_DEV
	struct stat stb;
	char *xopts = 0;
#endif /* MNTINFO_DEV */

#ifdef DEBUG
#ifdef NFS_4
	dlog("%s fstype %s (%s) flags %#x (%s)",
		mnt->mnt_dir, type, mnt->mnt_type, flags, mnt->mnt_opts);
#else
	dlog("%s fstype %d (%s) flags %#x (%s)",
		mnt->mnt_dir, type, mnt->mnt_type, flags, mnt->mnt_opts);
#endif /* NFS_4 */
#endif /* DEBUG */

	/*
	 * Fake some mount table entries for the automounter
	 */
	if (STREQ(mnt->mnt_type, MNTTYPE_AUTO)) {
		automount = 1;
		mnt->mnt_fsname = pid_fsname;
		/*
		 * Try it with the normal name
		 */
#ifdef notdef
		/*
		 * This is notdef'ed because some systems use
		 * the mount table in getwd() (esp. SunOS4) and
		 * if all the mount points are not marked it can
		 * cause major confusion.  This can probably
		 * be changed when no-one is running SunOS 4.0
		 * any more.
		 */
		mnt->mnt_type = MNTTYPE_IGNORE;
#endif /* notdef */
		mnt->mnt_type = MNTTYPE_NFS;
		/*
		 * Background the mount, so that the stat of the
		 * mountpoint is done in a background process.
		 */
		if (background())
			return 0;
	}

again:
	clock_valid = 0;
	error = MOUNT_TRAP(type, mnt, flags, mnt_data);
	if (error < 0)
		plog(XLOG_ERROR, "%s: mount: %m", mnt->mnt_dir);
	if (error < 0 && --retry > 0) {
		sleep(1);
		goto again;
	}
	if (error < 0) {
		if (automount)
			going_down(errno);
		return errno;
	}

#ifdef UPDATE_MTAB
#ifdef MNTINFO_DEV
	/*
	 * Add the extra dev= field to the mount table.
	 */
	if (lstat(mnt->mnt_dir, &stb) == 0) {
		char *zopts = (char *) xmalloc(strlen(mnt->mnt_opts) + 32);
		xopts = mnt->mnt_opts;
		if (sizeof(stb.st_dev) == 2) {
			/* SunOS 4.1 */
			sprintf(zopts, "%s,%s=%04lx", xopts, MNTINFO_DEV,
					(u_long) stb.st_dev & 0xffff);
		} else {
			/* System Vr4 */
			sprintf(zopts, "%s,%s=%08lx", xopts, MNTINFO_DEV,
					(u_long) stb.st_dev);
		}
		mnt->mnt_opts = zopts;
	}
#endif /* MNTINFO_DEV */

#ifdef hpux
	/*
	 * Yet another gratuitously incompatible change in HP-UX
	 */
	mnt->mnt_time = clocktime();
#endif /* hpux */
	write_mntent(mnt);
#ifdef MNTINFO_DEV
	if (xopts) {
		free(mnt->mnt_opts);
		mnt->mnt_opts = xopts;
	}
#endif /* MNTINFO_DEV */
#endif /* UPDATE_MTAB */

	/*
	 * Needed this way since mnt may contain a pointer
	 * to a local variable in this stack frame.
	 */
	if (automount)
		going_down(0);
	return 0;
}

#ifdef NEED_MNTOPT_PARSER
/*
 * Some systems don't provide these to the user,
 * but amd needs them, so...
 *
 * From: Piete Brooks <pb@cl.cam.ac.uk>
 */

#include <ctype.h>

static char *nextmntopt(p)
char **p;
{
	char *cp = *p;
	char *rp;
	/*
	 * Skip past white space
	 */
	while (*cp && isspace(*cp))
		cp++;
	/*
	 * Word starts here
	 */
	rp = cp;
	/*
	 * Scan to send of string or separator
	 */
	while (*cp && *cp != ',')
		cp++;
	/*
	 * If separator found the overwrite with nul char.
	 */
	if (*cp) {
		*cp = '\0';
		cp++;
	}
	/*
	 * Return value for next call
	 */
	*p = cp;
	return rp;
}

char *hasmntopt(mnt, opt)
struct mntent *mnt;
char *opt;
{
	char t[MNTMAXSTR];
	char *f;
	char *o = t;
	int l = strlen(opt);
	strcpy(t, mnt->mnt_opts);

	while (*(f = nextmntopt(&o)))
		if (strncmp(opt, f, l) == 0)
			return f - t + mnt->mnt_opts;

	return 0;
}
#endif /* NEED_MNTOPT_PARSER */

#ifdef MOUNT_AIX3

#include "aix3-nfs.h"

static int aix3_mkvp(p, gfstype, flags, object, stub, host, info, info_size, args)
char *p;
int gfstype;
int flags;
char *object;
char *stub;
char *host;
char *info;
int info_size;
char *args;
{
	struct vmount *vp = (struct vmount *) p;
	bzero((voidp) vp, sizeof(*vp));
	/*
	 * Fill in standard fields
	 */
	vp->vmt_revision = VMT_REVISION;
	vp->vmt_flags = flags;
	vp->vmt_gfstype = gfstype;

#define	VMT_ROUNDUP(len) (4 * ((len + 3) / 4))
#define VMT_ASSIGN(vp, idx, data, size) \
	vp->vmt_data[idx].vmt_off = p - (char *) vp; \
	vp->vmt_data[idx].vmt_size = size; \
	bcopy(data, p, size); \
	p += VMT_ROUNDUP(size);

	/*
	 * Fill in all variable length data
	 */
	p += sizeof(*vp);

	VMT_ASSIGN(vp, VMT_OBJECT, object, strlen(object) + 1);
	VMT_ASSIGN(vp, VMT_STUB, stub, strlen(stub) + 1);
	VMT_ASSIGN(vp, VMT_HOST, host, strlen(host) + 1);
	VMT_ASSIGN(vp, VMT_HOSTNAME, host, strlen(host) + 1);
	VMT_ASSIGN(vp, VMT_INFO, info, info_size);
	VMT_ASSIGN(vp, VMT_ARGS, args, strlen(args) + 1);

#undef VMT_ASSIGN
#undef VMT_ROUNDUP

	/*
	 * Return length
	 */
	return vp->vmt_length = p - (char *) vp;
}

/*
 * Map from conventional mount arguments
 * to AIX 3-style arguments.
 */
aix3_mount(fsname, dir, flags, type, data, args)
char *fsname;
char *dir;
int flags;
int type;
void *data;
char *args;
{
	char buf[4096];
	int size;

#ifdef DEBUG
	dlog("aix3_mount: fsname %s, dir %s, type %d", fsname, dir, type);
#endif /* DEBUG */

/* aix3_mkvp(p, gfstype, flags, object, stub, host, info, info_size, args) */

	switch (type) {

	case MOUNT_TYPE_NFS: {
		char *host = strdup(fsname);
		char *rfs = strchr(host, ':');
		*rfs++ = '\0';

		size = aix3_mkvp(buf, type, flags, rfs, dir, host, data, sizeof(struct nfs_args), args);
		free(host);

		} break;

	case MOUNT_TYPE_UFS:
		/* Need to open block device and extract log device info from sblk. */
		return EINVAL;

	default:
		return EINVAL;
	}
#ifdef DEBUG
	/*dlog("aix3_mkvp: flags %#x, size %d, args %s", flags, size, args);*/
#endif /* DEBUG */

	return vmount(buf, size);
}
#endif /* MOUNT_AIX3 */
