/*
 * $Id: util.c,v 5.2 90/06/23 22:20:06 jsp Rel $
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
 *	@(#)util.c	5.1 (Berkeley) %G%
 */

/*
 * Utils
 */

#include "am.h"
#ifdef HAS_SYSLOG
#include <syslog.h>
#endif /* HAS_SYSLOG */
#include <ctype.h>
#include <sys/stat.h>

#include <netdb.h>


INLINE
char *strnsave(str, len)
const char *str;
int len;
{
	char *sp = (char *) xmalloc(len+1);

	bcopy(str, sp, len);
	sp[len] = 0;

	return sp;
}

char *strdup(s)
const char *s;
{
	return strnsave(s, strlen(s));
}

/*
 * Concatenate three strings and store in buffer pointed to
 * by p, making p large enough to hold the strings
 */
char *str3cat(p, s1, s2, s3)
char *p;
char *s1;
char *s2;
char *s3;
{
	int l1 = strlen(s1);
	int l2 = strlen(s2);
	int l3 = strlen(s3);
	p = (char *) xrealloc(p, l1 + l2 + l3 + 1);
	bcopy(s1, p, l1);
	bcopy(s2, p + l1, l2);
	bcopy(s3, p + l1 + l2, l3 + 1);
	return p;
}

char *strealloc(p, s)
char *p;
char *s;
{
	int len = strlen(s) + 1;

	p = (char *) xrealloc((voidp) p, len);

	strcpy(p, s);
#ifdef DEBUG_MEM
	malloc_verify();
#endif /* DEBUG_MEM */
	return p;
}

voidp xrealloc(ptr, len)
voidp ptr;
int len;
{
#if defined(DEBUG) && defined(DEBUG_MEM)
	Debug(D_MEM) plog(XLOG_DEBUG, "Reallocated size %d; block %#x", len, ptr);
#endif /* defined(DEBUG) && defined(DEBUG_MEM) */

	if (ptr)
		ptr = (voidp) realloc(ptr, (unsigned) len);
	else
		ptr = (voidp) xmalloc((unsigned) len);

	if (!ptr) {
		plog(XLOG_FATAL, "Out of memory in realloc");
		going_down(1);
		abort();
	}
	return ptr;
}

char **strsplit(s, qc)
char *s;
int qc;
{
	char **ivec;
	int ic = 0;
	int done = 0;

	ivec = (char **) xmalloc((ic+1)*sizeof(char *));

	while (!done) {
		char *v;
		/*
		 * skip white space
		 */
		while (*s && isascii(*s) && isspace(*s))
			s++;

		/*
		 * End of string?
		 */
		if (!*s)
			break;

		/*
		 * remember start of string
		 */
		v = s;

		/*
		 * skip to white space
		 */
		while (*s && (!isascii(*s) || !isspace(*s))) {
			if (*s++ == qc) {
				/*
				 * Skip past string.
				 */
				s++;
				while (*s && *s != qc)
				 	s++;
				if (*s == qc)
					s++;
			}
		}

		if (!*s)
			done = 1;
		*s++ = '\0';

		/*
		 * save string in new ivec slot
		 */
		ivec[ic++] = v;
		ivec = (char **) xrealloc(ivec, (ic+1)*sizeof(char *));
#ifdef DEBUG
		Debug(D_STR)
			plog(XLOG_DEBUG, "strsplit saved \"%s\"", v);
#endif /* DEBUG */
	}

#ifdef DEBUG
	Debug(D_STR)
		plog(XLOG_DEBUG, "strsplit saved a total of %d strings", ic);
#endif /* DEBUG */

	ivec[ic] = 0;

	return ivec;
}

/*
 * Strip off the trailing part of a domain
 * to produce a short-form domain relative
 * to the local host domain.
 * Note that this has no effect if the domain
 * names do not have the same number of
 * components.  If that restriction proves
 * to be a problem then the loop needs recoding
 * to skip from right to left and do partial
 * matches along the way -- ie more expensive.
 */
static void domain_strip P((char *otherdom, char *localdom));
static void domain_strip(otherdom, localdom)
char *otherdom, *localdom;
{
        char *p1 = otherdom-1;
	char *p2 = localdom-1;

        do {
                if (p1 = strchr(p1+1, '.'))
                if (p2 = strchr(p2+1, '.'))
                if (strcmp(p1+1, p2+1) == 0) {
                        *p1 = '\0';
                        break;
                }
        } while (p1 && p2);
}

/*
 * Normalize a host name
 */
void host_normalize P((char **chp));
void host_normalize(chp)
char **chp;
{
	/*
	 * Normalize hosts is used to resolve host name aliases
	 * and replace them with the standard-form name.
	 * Invoked with "-n" command line option.
	 */
	if (normalize_hosts) {
		struct hostent *hp;
		clock_valid = 0;
		hp = gethostbyname(*chp);
		if (hp && hp->h_addrtype == AF_INET) {
#ifdef DEBUG
			dlog("Hostname %s normalized to %s", *chp, hp->h_name);
#endif /* DEBUG */
			*chp = strealloc(*chp, hp->h_name);
		}
	}
	domain_strip(*chp, hostd);
}

/*
 * Keys are not allowed to contain " ' ! or ; to avoid
 * problems with macro expansions.
 */
static char invalid_keys[] = "\"'!;@ \t\n";
int valid_key P((char *key));
int valid_key(key)
char *key;
{
	while (*key)
		if (strchr(invalid_keys, *key++))
			return FALSE;
	return TRUE;
}

void going_down P((int rc));
void going_down(rc)
int rc;
{
	if (foreground) {
		if (amd_state != Start) {
			if (amd_state != Done)
				return;
			unregister_amq();
		}
	}
	if (foreground) {
		plog(XLOG_INFO, "Finishing with status %d", rc);
	} else {
#ifdef DEBUG
		dlog("background process exiting with status %d", rc);
#endif /* DEBUG */
	}

	exit(rc);
}

#ifdef DEBUG_MEM
static int mem_bytes;
static int orig_mem_bytes;
static void checkup_mem(P_void)
{
extern struct mallinfo __mallinfo;
	if (mem_bytes != __mallinfo.uordbytes) {
		if (orig_mem_bytes == 0)
			mem_bytes = orig_mem_bytes = __mallinfo.uordbytes;
		else {
			fprintf(logfp, "%s[%d]: ", progname, mypid);
			if (mem_bytes < __mallinfo.uordbytes) {
				fprintf(logfp, "ALLOC: %d bytes",
					__mallinfo.uordbytes - mem_bytes);
			} else {
				fprintf(logfp, "FREE: %d bytes",
					mem_bytes - __mallinfo.uordbytes);
			}
			mem_bytes = __mallinfo.uordbytes;
			fprintf(logfp, ", making %d missing\n",
				mem_bytes - orig_mem_bytes);
		}
	}
	malloc_verify();
}
#endif /* DEBUG_MEM */

/*
 * Take a log format string and expand occurences of %m
 * with the current error code take from errno.
 */
INLINE
static void expand_error(f, e)
char *f;
char *e;
{
	extern int sys_nerr;
	extern char *sys_errlist[];
	char *p;
	int error = errno;

	for (p = f; *e = *p; e++, p++) {
		if (p[0] == '%' && p[1] == 'm') {
			char *errstr;
			if (error < 0 || error >= sys_nerr)
				errstr = 0;
			else
				errstr = sys_errlist[error];
			if (errstr)
				strcpy(e, errstr);
			else
				sprintf(e, "Error %d", error);
			e += strlen(e) - 1;
			p++;
		}
	}
}

/*
 * Output the time of day and hostname to the logfile
 */
static void show_time_host_and_name(lvl)
int lvl;
{
static time_t last_t = 0;
static char *last_ctime = 0;
	time_t t = clocktime();
	char *sev;
	extern char *ctime();

#if defined(DEBUG) && defined(PARANOID)
extern char **gargv;
#endif /* defined(DEBUG) && defined(PARANOID) */

	if (t != last_t) {
		last_ctime = ctime(&t);
		last_t = t;
	}

	switch (lvl) {
	case XLOG_FATAL:	sev = "fatal:"; break;
	case XLOG_ERROR: 	sev = "error:"; break;
	case XLOG_USER:		sev = "user: "; break;
	case XLOG_WARNING:	sev = "warn: "; break;
	case XLOG_INFO:		sev = "info: "; break;
	case XLOG_DEBUG:	sev = "debug:"; break;
	case XLOG_MAP:		sev = "map:  "; break;
	case XLOG_STATS:	sev = "stats:"; break;
	default:		sev = "hmm:  "; break;
	}
	fprintf(logfp, "%15.15s %s %s[%d]/%s ",
		last_ctime+4, hostname,
#if defined(DEBUG) && defined(PARANOID)
		gargv[0],
#else
		progname,
#endif /* defined(DEBUG) && defined(PARANOID) */
		mypid,
		sev);
}

#ifdef DEBUG
/*VARARGS1*/
void dplog(fmt, j,s,_,p,e,n,d,r,y)
char *fmt;
char *j, *s, *_, *p, *e, *n, *d, *r, *y;
{
	plog(XLOG_DEBUG, fmt, j,s,_,p,e,n,d,r,y);
}

#endif /* DEBUG */
/*VARARGS1*/
void plog(lvl, fmt, j,s,_,p,e,n,d,r,y)
int lvl;
char *fmt;
char *j, *s, *_, *p, *e, *n, *d, *r, *y;
{
	char msg[1024];
	char efmt[1024];
	char *ptr = msg;

	if (!(xlog_level & lvl))
		return;

#ifdef DEBUG_MEM
	checkup_mem();
#endif /* DEBUG_MEM */

	expand_error(fmt, efmt);
	sprintf(ptr, efmt, j,s,_,p,e,n,d,r,y);
	ptr += strlen(ptr);
	if (ptr[-1] == '\n')
		*--ptr  = '\0';
#ifdef HAS_SYSLOG
	if (syslogging) {
		switch(lvl) {	/* from mike <mcooper@usc.edu> */
		case XLOG_FATAL:	lvl = LOG_CRIT; break;
		case XLOG_ERROR: 	lvl = LOG_ERR; break;
		case XLOG_USER:		lvl = LOG_WARNING; break;
		case XLOG_WARNING:	lvl = LOG_WARNING; break;
		case XLOG_INFO:		lvl = LOG_INFO; break;
		case XLOG_DEBUG:	lvl = LOG_DEBUG; break;
		case XLOG_MAP:		lvl = LOG_DEBUG; break;
		case XLOG_STATS:	lvl = LOG_INFO; break;
		default:		lvl = LOG_ERR; break;
		}
		syslog(lvl, "%s", msg);
		return;
	}
#endif /* HAS_SYSLOG */

	*ptr++ = '\n';
	*ptr = '\0';

	/*
	 * Mimic syslog header
	 */
	show_time_host_and_name(lvl);
	fwrite(msg, ptr - msg, 1, logfp);
	fflush(logfp);
}

int bind_resv_port P((int so, u_short *pp));
int bind_resv_port(so, pp)
int so;
u_short *pp;
{
	struct sockaddr_in sin;
	int rc;
	unsigned short port;

	bzero((voidp) &sin, sizeof(sin));
	sin.sin_family = AF_INET;

	port = IPPORT_RESERVED;

	do {
		--port;
		sin.sin_port = htons(port);
		rc = bind(so, (struct sockaddr *) &sin, sizeof(sin));
	} while (rc < 0 && port > IPPORT_RESERVED/2);

	if (pp && rc == 0)
		*pp = port;
	return rc;
}

void forcibly_timeout_mp P((am_node *mp));
void forcibly_timeout_mp(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;
	/*
	 * Arrange to timeout this node
	 */
	if (mf && ((mp->am_flags & AMF_ROOT) ||
		(mf->mf_flags & (MFF_MOUNTING|MFF_UNMOUNTING)))) {
		if (!(mf->mf_flags & MFF_UNMOUNTING))
			plog(XLOG_WARNING, "ignoring timeout request for active node %s", mp->am_path);
	} else {
		plog(XLOG_INFO, "\"%s\" forcibly timed out", mp->am_path);
		mp->am_flags &= ~AMF_NOTIMEOUT;
		mp->am_ttl = clocktime();
		reschedule_timeout_mp();
	}
}

void am_mounted P((am_node *mp));
void am_mounted(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;
	int quoted;
	mf->mf_flags |= MFF_MOUNTED;
	mf->mf_error = 0;

	/*
	 * Patch up path for direct mounts
	 */
	if (mp->am_parent && mp->am_parent->am_mnt->mf_ops == &dfs_ops)
		mp->am_path = str3cat(mp->am_path, mp->am_parent->am_path, "/", ".");

	/*
	 * Check whether this mount should be cached permanently
	 */
	if (mf->mf_ops->fs_flags & FS_NOTIMEOUT) {
		mp->am_flags |= AMF_NOTIMEOUT;
	} else if (mf->mf_mount[1] == '\0' && mf->mf_mount[0] == '/') {
		mp->am_flags |= AMF_NOTIMEOUT;
	} else {
		struct mntent mnt;
		mnt.mnt_opts = mf->mf_fo->opt_opts;
		if (hasmntopt(&mnt, "nounmount"))
			mp->am_flags |= AMF_NOTIMEOUT;
		if ((mp->am_timeo = hasmntval(&mnt, "utimeout")) == 0)
			mp->am_timeo = am_timeo;
/*		if ((mf->mf_server->fs_pinger = hasmntval(&mnt, "ping")) == 0)
			mf->mf_server->fs_pinger = AM_PINGER;
*/
	}

	/*
	 * Do mounted callback
	 */
	if (mf->mf_ops->mounted)
		(*mf->mf_ops->mounted)(mf);

	/*
	 * If this node is a symlink then
	 * compute the length of the returned string.
	 */
	if (mf->mf_fattr.type == NFLNK)
		mf->mf_fattr.size = strlen(mp->am_link ? mp->am_link : mp->am_mnt->mf_mount);

	/*
	 * Record mount time
	 */
	mf->mf_fattr.mtime.seconds = mp->am_stats.s_mtime = clocktime();
	new_ttl(mp);
	/*
	 * Update mtime of parent node
	 */
	if (mp->am_parent && mp->am_parent->am_mnt)
		mp->am_parent->am_mnt->mf_fattr.mtime.seconds = mp->am_stats.s_mtime;

	/*
	 * Log message
	 */
	quoted = strchr(mf->mf_info, ' ') != 0;
	plog(XLOG_INFO, "%s%s%s mounted fstype %s on %s",
		quoted ? "\"" : "",
		mf->mf_info,
		quoted ? "\"" : "",
		mf->mf_ops->fs_type, mf->mf_mount);

	/*
	 * Update stats
	 */
	amd_stats.d_mok++;
}

int mount_node P((am_node *mp));
int mount_node(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;
	int error;

	mf->mf_flags |= MFF_MOUNTING;
	error = (*mf->mf_ops->mount_fs)(mp);
	mf = mp->am_mnt;
	mf->mf_flags &= ~MFF_MOUNTING;
	if (!error && !(mf->mf_ops->fs_flags & FS_MBACKGROUND)) {
		/* ...but see ifs_mount */
		am_mounted(mp);
	}

	return error;
}

void am_unmounted P((am_node *mp));
void am_unmounted(mp)
am_node *mp;
{
	mntfs *mf = mp->am_mnt;

	if (!foreground) /* firewall - should never happen */
		return;

#ifdef DEBUG
	/*dlog("in am_unmounted(), foreground = %d", foreground);*/
#endif /* DEBUG */

	/*
	 * Do unmounted callback
	 */
	if (mf->mf_ops->umounted)
		(*mf->mf_ops->umounted)(mp);

	/*
	 * Update mtime of parent node
	 */
	if (mp->am_parent && mp->am_parent->am_mnt)
		mp->am_parent->am_mnt->mf_fattr.mtime.seconds = clocktime();

	free_map(mp);
}


/*
 * Fork the automounter
 *
 * TODO: Need a better strategy for handling errors
 */
static int dofork(P_void);
INLINE
static int dofork()
{
	int pid;
top:
	pid = fork();

	if (pid < 0) {
		sleep(1);
		goto top;
	}

	if (pid == 0) {
		mypid = getpid();
		foreground = 0;
	}

	return pid;
}

int background(P_void);
int background()
{
	int pid = dofork();
	if (pid == 0) {
#ifdef DEBUG
		dlog("backgrounded");
#endif /* DEBUG */
		foreground = 0;
	}

	return pid;
}

int mkdirs P((char *path, int mode));
int mkdirs(path, mode)
char *path;
int mode;
{
	/*
	 * take a copy in case path is in readonly store
	 */
	char *p2 = strdup(path);
	char *sp = p2;
	struct stat stb;
	int error_so_far = 0;

	/*
	 * Skip through the string make the directories.
	 * Mostly ignore errors - the result is tested at the end.
	 *
	 * This assumes we are root so that we can do mkdir in a
	 * mode 555 directory...
	 */
	while (sp = strchr(sp+1, '/')) {
		*sp = '\0';
		if (mkdir(p2, mode) < 0) {
			error_so_far = errno;
		} else {
#ifdef DEBUG
			dlog("mkdir(%s)", p2);
#endif /* DEBUG */
		}
		*sp = '/';
	}

	if (mkdir(p2, mode) < 0) {
		error_so_far = errno;
	} else {
#ifdef DEBUG
		dlog("mkdir(%s)", p2);
#endif /* DEBUG */
	}

#ifdef SUNOS4_WORKAROUND
	/*
	 * Do a sync - if we do rmdirs() immediately
	 * and then the system crashes it leaves
	 * the filesystem in a state that fsck -p
	 * can't fix.  (Observed more than once on
	 * SunOS 4 ...)
	 *
	 * The problem was caused by a bug somewhere
	 * in the UFS code which has since been fixed
	 * (at least at Berkeley).
	 *
	 * Attempted workaround - XXX.
	 */
	sync();
#endif /* SUNOS4_WORKAROUND */

	free(p2);

	return stat(path, &stb) == 0 &&
		(stb.st_mode & S_IFMT) == S_IFDIR ? 0 : error_so_far;
}

void rmdirs P((char *dir));
void rmdirs(dir)
char *dir;
{
	char *xdp = strdup(dir);
	char *dp;

	do {
		struct stat stb;
		/*
		 * Try to find out whether this was
		 * created by amd.  Do this by checking
		 * for owner write permission.
		 */
		if (stat(xdp, &stb) == 0 && (stb.st_mode & 0200) == 0) {
			if (rmdir(xdp) < 0) {
				if (errno != ENOTEMPTY && errno != EBUSY && errno != EEXIST)
					plog(XLOG_ERROR, "rmdir(%s): %m", xdp);
				break;
			} else {
#ifdef DEBUG
				dlog("rmdir(%s)", xdp);
#endif /* DEBUG */
			}
		} else {
			break;
		}
		dp = strrchr(xdp, '/');
		if (dp)
			*dp = '\0';
	} while (dp && dp > xdp);
	free(xdp);
}

/*
 * Because the internal clock is only used for
 * timing out mounts, it does not have to be
 * particularly accurate, so long as it does not run
 * ahead of the real time.  So, to reduce the system
 * call overhead, repeated calls to gettimeofday()
 * are replaced by calls to the macro clocktime().
 * If the global time (clock_valid) is zero then
 * update_clocktime() is called to obtain the real time.
 * Before any system calls that are likely to block for a
 * significant time, the clock_valid value is set
 * so that the clock is recomputed next time it is
 * needed.
 */

time_t clock_valid = 0;
#ifndef clocktime
time_t clocktime(P_void)
{
	return time(&clock_valid);
}
#endif /* clocktime */

voidp xmalloc(len)
int len;
{
	voidp p;
	int retries = 600;

	do {
		p = (voidp) malloc((unsigned) len);
		if (p) {
#if defined(DEBUG) && defined(DEBUG_MEM)
			Debug(D_MEM) plog(XLOG_DEBUG, "Allocated size %d; block %#x", len, p);
#endif /* defined(DEBUG) && defined(DEBUG_MEM) */
			return p;
		}
		if (retries > 0) {
			plog(XLOG_ERROR, "Retrying memory allocation");
			sleep(1);
		}
	} while (--retries);

	plog(XLOG_FATAL, "Out of memory");
	going_down(1);

	abort();

	return 0;
}

#if defined(DEBUG) && defined(DEBUG_MEM)
xfree(f, l, p)
char *f;
int l;
voidp p;
{
	Debug(D_MEM) plog(XLOG_DEBUG, "Free in %s:%d: block %#x", f, l, p);
#undef free
	free(p);
}
#endif /* defined(DEBUG) && defined(DEBUG_MEM) */
