/*
 * This software is Copyright (c) 1986 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * funcs - functions used by many programs
 */

#ifdef SCCSID
static char	*SccsId = "@(#)funcs.c	2.36	10/7/87";
#endif /* SCCSID */

/*LINTLIBRARY*/

#include "params.h"
#include <errno.h>
#if defined(USG) || defined(BSD4_2) || defined(BSD4_1C)
#include <fcntl.h>
#endif /* !v7 */

extern char *Progname;

/*
 * News group matching.
 *
 * nglist is a list of newsgroups.
 * sublist is a list of subscriptions.
 * sublist may have "meta newsgroups" in it.
 * All fields are NGDELIM separated,
 * and there is an NGDELIM at the end of each argument.
 *
 * Currently implemented glitches:
 * sublist uses 'all' like shell uses '*', and '.' like shell '/'.
 * If subscription X matches Y, it also matches Y.anything.
 */
ngmatch(nglist, sublist)
register char *nglist, *sublist;
{
	register char *n, *s;
	register int rc;

	rc = FALSE;
	for (n = nglist; *n != '\0' && rc == FALSE;) {
		for (s = sublist; *s != '\0';) {
			if (*s != NEGCHAR)
				rc = rc || ptrncmp(s, n);
			else
				rc = rc && !ptrncmp(s+1, n);
			while (*s++ != NGDELIM && *s != '\0')
				;
		}
		while (*n++ != NGDELIM && *n != '\0')
			;
	}
	return rc;
}

/*
 * Compare two newsgroups for equality.
 * The first one may be a "meta" newsgroup.
 */
ptrncmp(ng1, ng2)
register char *ng1, *ng2;
{
	while (*ng1 != NGDELIM && *ng1 != '\0') {
		if (ng1[0]=='a' && ng1[1]=='l' && ng1[2]=='l') {
			ng1 += 3;
			while (*ng2 != NGDELIM && *ng2 != '.' && *ng2 != '\0')
				if (ptrncmp(ng1, ng2++))
					return(TRUE);
			return ptrncmp(ng1, ng2);
		} else if (*ng1++ != *ng2++)
			return FALSE;
	}
	return *ng2 == '.' || *ng2 == NGDELIM || *ng2 == '\0';
}

/*
 * Exec the shell.
 * This version resets uid, gid, and umask.
 * Called with fsubr(ushell, s, NULL)
 */
/* ARGSUSED */
ushell(s, dummy)
char *s, *dummy;
{
	(void) umask(savmask);
	(void) setgid(gid);
	(void) setuid(uid);
	xshell(s);
}

/*
 * Exec the shell.
 */

#ifdef lint
char	**environ;
#else /* !lint */
extern char	**environ;
#endif /* !lint */

xshell(s)
char *s;
{
	char *env[100], **envp;
	char a[BUFLEN + 2];
	extern char filename[];
	/* set $A */
	(void) sprintf(a, "A=%s", filename);
	env[0] = a;
	for (envp = env + 1 ; *environ != NULL && envp < env + 98 ; environ++)
		if ((*environ)[0] != 'A' || (*environ)[1] != '=')
			*envp++ = *environ;
	*envp = NULL;

	execle(SHELL, SHELL, "-c", s, (char *)0, env);
	xerror("No shell!");
}

/*
 * Fork and call a subroutine with two args.
 * Return pid without waiting.
 */
fsubr(f, s1, s2)
int (*f)();
char *s1, *s2;
{
	register int pid;

	/* this may NOT be a vfork */
	while ((pid = fork()) == -1)
		sleep((unsigned)1);
	if (pid == 0) {
		(*f)(s1, s2);
		exit(0);
	}
	return pid;
}

/*
 * Wait on a child process.
 */
fwait(pid)
register int pid;
{
	register int w;
	int status;
	int (*onhup)(), (*onint)();

	onint = signal(SIGINT, SIG_IGN);
	onhup = signal(SIGHUP, SIG_IGN);
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	(void) signal(SIGINT, onint);
	(void) signal(SIGHUP, onhup);
	return status;
}

/*
 * Strip trailing newlines, blanks, and tabs from 's'.
 * Return TRUE if newline was found, else FALSE.
 */
nstrip(s)
register char *s;
{
	register char *p;
	register int rc;

	rc = FALSE;
	p = s;
	while (*p)
		if (*p++ == '\n')
			rc = TRUE;
	while (--p >= s && (*p == '\n' || *p == ' ' || *p == '\t'));
	*++p = '\0';
	return rc;
}

/*
 * Local open routine.
 */
FILE *
xfopen(name, fmode)
register char *name, *fmode;
{
	register FILE *fp;
	char	*fname;
	extern int errno;

	if ((fp = fopen(name, fmode)) == NULL) {
#ifdef IHCC
		/*
		 * IHCC users only see the "filename" that was in trouble,
		 * not the whole path.  (for security!)
		 */
		fname = rindex(name, '/') + 1;
#else
		fname = name;
#endif
		xerror("Cannot open %s (%s): %s", fname, fmode, errmsg(errno));
	}
	/* kludge for setuid not being honored for root */
	if ((uid == 0) && (duid != 0) && ((*fmode == 'a') || (*fmode == 'w')))
		(void) chown(name, duid, dgid);
	return fp;
}

char *
errmsg(code)
int code;
{
	extern int sys_nerr;
	extern char *sys_errlist[];
	static char ebuf[6+5+1];

	if (code > sys_nerr) {
		(void) sprintf(ebuf, "Error %d", code);
		return ebuf;
	} else
		return sys_errlist[code];
}
/* From UC Berkeley @(#)strcasecmp.c	1.3 (Berkeley) 8/3/87 */

/*
 * This array is designed for mapping upper and lower case letter
 * together for a case independent comparison.  The mappings are
 * based upon ascii character sequences.
 */
char charmap[] = {
	'\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
	'\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
	'\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
	'\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
	'\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
	'\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
	'\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
	'\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
	'\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
	'\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
	'\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
	'\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
	'\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
	'\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
	'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
	'\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
	'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
	'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
	'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
	'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
	'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
	'\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
	'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
	'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
	'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
	'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

strcasecmp(s1, s2)
register char *s1, *s2;
{
	register char *cm = charmap;

	while (cm[*s1] == cm[*s2++])
		if (*s1++ == '\0')
			return 0;
	return cm[*s1] - cm[*--s2];
}

strncasecmp(s1, s2, n)
register char *s1, *s2;
register int n;
{
	register char *cm = charmap;

	while (--n >= 0 && cm[*s1] == cm[*s2++])
		if (*s1++ == '\0')
			return 0;
	return n < 0 ? 0 : cm[*s1] - cm[*--s2];
}

prefix(full, pref)
register char *full, *pref;
{
	register char *cm = charmap;

	while (*pref != '\0') {
		if (cm[*full++] != cm[*pref++])
			return FALSE;
	}
	return TRUE;
}

char *
dirname(ngname)
char *ngname;
{
	static char rbuf[BUFLEN];
	register char *p;

	(void) sprintf(rbuf, "%s/%s", SPOOL, ngname);

	for (p=rbuf+strlen(SPOOL); *p; p++)
		if (*p == '.')
			*p = '/';
	return rbuf;
}

/*
 * Return TRUE iff ngname is a valid newsgroup name
 */
validng(ngname)
char *ngname;
{
	register FILE *fp;
	register char *p, *q;
	char abuf[BUFLEN];

	fp = xfopen(ACTIVE, "r");
	while(fgets(abuf, BUFLEN, fp) != NULL) {
		p = abuf;
		q = ngname;
		while (*p++ == *q++)
			;
		if (*--q == '\0' && *--p == ' ') {
			(void) fclose(fp);
			return TRUE;
		}
	}
	(void) fclose(fp);
	return FALSE;
}

/* VARARGS1 */
xerror(message, arg1, arg2, arg3)
char *message;
long arg1, arg2, arg3;
{
	char buffer[LBUFLEN];

	fflush(stdout);
	(void) sprintf(buffer, message, arg1, arg2, arg3);
	logerr(buffer);
	xxit(1);
}

/* VARARGS1 */
log(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	_dolog(0, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
}

/* VARARGS1 */
logerr(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	_dolog(1, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
}

char *lfsuffix[] = {
	"log",
	"errlog",
	NULL,
};

/*
 * Log the given message, with printf strings and parameters allowed,
 * on the log file, if it can be written.  The date and an attempt at
 * figuring out the remote system name are also logged.
 */
/* VARARGS1 */
_dolog(which, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
	FILE *logfile;
	register char *p, *logtime;
	int i;
	char logfname[BUFLEN];		/* the log file */
	char rmtsys[BUFLEN];
	char msg[LBUFLEN];
	time_t t;

	(void) strcpy(rmtsys, header.path);
	p = index(rmtsys, '!');
	if (p == NULL)
		p = index(rmtsys, ':');
	if (p)
		*p = 0;
	else {
		p = rindex(rmtsys, '@');
		if (p)
			(void) strcpy(rmtsys, p+1);
		else
			(void) strcpy(rmtsys, "local");
	}

	(void) time(&t);
	logtime = ctime(&t);
	logtime[16] = 0;
	logtime += 4;


	(void) sprintf(msg, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);

	if (which)
		fprintf(stderr,"%s: %s\n", Progname, msg);

	for (i=0; i<=which;i++) {
		(void) sprintf(logfname, "%s/%s", LIB, lfsuffix[i]);

		if (access(logfname, 0) == 0 && (logfile = fopen(logfname, "a")) != NULL) {
#if defined(USG) || defined(BSD4_2) || defined(BSD4_1C)
			int flags;
			flags = fcntl(fileno(logfile), F_GETFL, 0);
			(void) fcntl(fileno(logfile), F_SETFL, flags|O_APPEND);
#else /* v7 */
			(void) lseek(fileno(logfile), 0L, 2);
#endif /* v7 */
			if (i)
				fprintf(logfile, "%s\t%s\t%s: %s\n", logtime,
					header.ident[0] ? header.ident : username, Progname, msg);
			else
				fprintf(logfile, "%s\t%s\t%s\n", logtime,
					rmtsys, msg);
			(void) fclose(logfile);
		}
	}
}
#ifdef VMS

/*
 * vmslink allows simulation of file linking under VMS.
 */
vmslink(infile,outfile)
char *infile, *outfile;
{
	FILE *fp;

	if (access(outfile,0) == 0) {
		errno = EEXIST;
		return -1;
	}

	fp = fopen(outfile, "w");
	if (fp == NULL) {
		errno = EACCES;
		return -1;
	}

	(void) fprintf(fp, "%s", infile);
	(void) fclose(fp);

	return 0;
}

/*
 * vmsdelete deletes all revisions of a file.  It attempts to
 * appear as unlink(2) under conventional Unix in other respects.
 */
vmsdelete(file)
char *file;
{
	int i;

	i = unlink(file);
	if (i != 0)
		return i;

	i = errno;
	while (unlink(file) == 0)
		;
	errno = i;

	return 0;
}

/*
 * Convert a Unix file to a VMS fixed record format file by
 * executing the 'unixtovms' command.
 */
unixtovms(file)
char *file;
{
	char buf[BUFLEN];
	sprintf(buf, "exec /etc/unixtovms %s", file);
	return system(buf);
}

/*
 * Convert a VMS fixed record format file to a Unix file by
 * executing the 'vmstounix' command.
 */
vmstounix(file)
char *file;
{
	char buf[BUFLEN];
	sprintf(buf,"exec /etc/vmstounix %s", file);
	return system(buf);
}
#endif /* VMS */

#if !defined(BSD4_2) && !defined(BSD4_1C)
/*
 * make a directory. Also make sure that the directory is owned
 * by the right userid
 */
mkdir(path, perm)
char *path;
int perm;
{
	int pid, status;
#ifdef USG
	char parent[200];
	char *p;
	struct stat sbuf;

	/*
	 * Make parent directory writable, because we will
	 * be creating a directory owned by the real user,
	 * rather than by news.
	 */
	(void) strcpy(parent, path);
	if (p = rindex(parent, '/')) {
		*p = '\0';
		if (stat(parent, &sbuf) == 0)
			(void) chmod(parent, 0777);
		else
			return -1;
	} else
		return -1;
#endif

	if (pid=vfork()) {
		status = fwait(pid);
#if defined(USG) && !defined(CHEAP)
		if (pid=vfork())
			(void) fwait(pid);
		else {
			setgid(gid);
			setuid(uid);
			if (chown(path, duid, dgid) == 0)
				(void) chmod(path, perm&(~N_UMASK));
			_exit(0);
		}
#endif /* USG && !CHEAP */
	} else {
		(void) setgid(dgid);
		if (setuid(duid) < 0)
			(void) umask(0);
		else
			(void) umask(perm&N_UMASK);
		(void) execlp("mkdir", "mkdir", path, (char *)NULL);
		perror(path);
		_exit(1);
	}
#ifdef USG
	(void) chmod(parent, sbuf.st_mode); /* Restore mode of parent */
#endif
	return status;
}
#endif /* !BSD4_2 && ! BSD4_1C */
#ifndef USG
char *
strpbrk(str, chars)
register char *str, *chars;
{
	register char *cp;

	do {
		cp = chars - 1;
		while (*++cp) {
			if (*str == *cp)
				return str;
		}
	} while (*str++);
	return NULL;
}
#endif /* !USG */

#ifdef FASCIST
/*
 *  This routine checks to see if the posting user is allowed to
 *  post to the given newsgroup.  If the username is not in the file
 *  $LIBDIR/authorized then the default in the symbol FASCIST is used.
 *
 *  Format of the call:
 *     fascist(user, newgroups)
 *
 *  Returns:
 *     FALSE, if authorized
 *     TRUE, if not
 *
 *  Format of the file "authorized" is:
 *    user:allowed groups  
 *
 *  Example:
 *    root:net.all,mod.all
 *    naughty_person:junk,net.politics
 *    operator:!net.all,general,test,mod.unix
 *
 *  An open environment could have FASCIST set to "all"
 *  and then individual entries could be made in the authorized file
 *  to prevent certain individuals from posting to such a wide
 *  area.
 *
 *  Note that a distribution of "all" does NOT mean to allow postings
 *  only to local groups -- "all" includes "all.all".  
 *  Use "all,!all.all" to get this behavior
 *
 *	Eugene Spafford		spaf@gatech	May 22, 1985
 */

fascist(user, newsgroups)
register char *user, *newsgroups;
{
	FILE *facfd;
	char facuser[BUFLEN], facgroups[BUFLEN], factemp[BUFLEN];
	register char  *facptr;

	/* First, open the necessary file...$LIBDIR/authorized and see if there
	 * is an entry for this user 
	 */

	(void) strncpy(facgroups, FASCIST, BUFLEN);
	sprintf(factemp, "%s/%s", LIB, "authorized");
	facfd = fopen(factemp, "r");

	if (facfd != NULL) { /* If no such file, we go with the global default */
		while (fscanf(facfd, "%[^:]:%s\n", facuser, factemp) != EOF)
		{
			if (feof(facfd))
				break;
			if (strncmp(facuser, user, BUFLEN) == 0) {
				(void) strcat(facgroups, ",");
				(void) strcat(facgroups, factemp);
				break;
			}
		}
		fclose (facfd);
	}
#ifdef DEBUG
	fprintf(stderr, "facgroups = %s\n", facgroups);
	fprintf(stderr, "newsgroups = %s\n", newsgroups);
#endif /* DEBUG */

	/* We step through the newsgroups being posted to and check each against
	 * the restriction list.  *ALL* posted groups must match the restriction
	 * list or we don't allow the posting.
	 */

	while (*newsgroups != '\0') {
		facptr = factemp;
		while (*newsgroups != '\0' && *newsgroups != NGDELIM)
			*facptr++ = *newsgroups++;
		*facptr = '\0';
		if (*newsgroups == NGDELIM)
			newsgroups++;

#ifdef DEBUG
		fprintf(stderr, "Checking newsgroup '%s'\n", factemp);
#endif

		if (ngmatch(factemp, facgroups) == FALSE)
			return TRUE;
	}

	/* must be okay -- return */
#ifdef DEBUG
	fprintf (stderr, "Newsgroups approved for this poster.\n");
#endif /* DEBUG */
	return FALSE;
}
#endif /* FASCIST */
