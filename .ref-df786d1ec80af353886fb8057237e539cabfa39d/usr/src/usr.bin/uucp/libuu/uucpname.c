#ifndef lint
static char sccsid[] = "@(#)uucpname.c	5.1 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	GETMYHNAME
#include <UNET/unetio.h>
#endif

#ifdef	UNAME
/* Use USG uname() library routine */
#include <sys/utsname.h>
#endif

#ifdef	CCWHOAMI
/* Compile in 'sysname' as found in standard(!) include file */
#include <whoami.h>
#endif

/*******
 *	uucpname(name)		get the uucp name
 *
 *	return code - none
 */

uucpname(name)
register char *name;
{
	register char *s, *d;

	/* HUGE KLUDGE HERE!  rti!trt
	 * Since some UNIX systems do not honor the set-user-id bit
	 * when the invoking user is root, we must change the uid here.
	 * So uucp files are created with the correct owner.
	 */
	if (geteuid() == 0 && getuid() == 0) {
		struct stat stbuf;
		stbuf.st_uid = 0;	/* In case the stat fails */
		stbuf.st_gid = 0;
		stat(UUCICO, &stbuf);	/* Assume uucico is correctly owned */
		setgid(stbuf.st_gid);
		setuid(stbuf.st_uid);
	}

	s = NULL;	/* system name unknown, so far */

#ifdef	GETHOSTNAME
	/* Use 4.1a library routine */
	if (s == NULL || *s == '\0') {
		char hostname[32];

		s = hostname;
		if(gethostname(hostname, sizeof(hostname)) == -1) {
			DEBUG(1, "gethostname", "FAILED");
			s = NULL;
		}
	}
#endif

#ifdef	UNAME
	/* Use USG library routine */
	if (s == NULL || *s == '\0') {
		struct utsname utsn;

		s = utsn.nodename;
		if (uname(&utsn) == -1) {
			DEBUG(1, "uname", "FAILED");
			s = NULL;
		}
	}
#endif

#ifdef	WHOAMI
	/* Use fake gethostname() routine */
	if (s == NULL || *s == '\0') {
		char fakehost[32];

		s = fakehost;
		if (fakegethostname(fakehost, sizeof(fakehost)) == -1) {
			DEBUG(1, "whoami search", "FAILED");
			s = NULL;
		}
	}
#endif

#ifdef	CCWHOAMI
	/* compile sysname right into uucp */
	if (s == NULL || *s == '\0') {
		s = sysname;
	}
#endif

#ifdef	UUNAME
	/* uucp name is stored in /etc/uucpname or /local/uucpname */
	if (s == NULL || *s == '\0') {
		FILE *uucpf;
		char stmp[10];

		s = stmp;
		if (((uucpf = fopen("/etc/uucpname", "r")) == NULL &&
		     (uucpf = fopen("/local/uucpname", "r")) == NULL) ||
			fgets(s, 8, uucpf) == NULL) {
				DEBUG(1, "uuname search", "FAILED");
				s = NULL;
		} else {
			for (d = stmp; *d && *d != '\n' && d < stmp + 8; d++)
				;
			*d = '\0';
		}
		if (uucpf != NULL)
			fclose(uucpf);
	}
#endif

#ifdef	GETMYHNAME
	/* Use 3Com's getmyhname() routine */
	if (s == NULL || *s == '\0') {
		if ((s == getmyhname()) == NULL)
			DEBUG(1, "getmyhname", "FAILED");
	}
#endif

#ifdef	MYNAME
	if (s == NULL || *s == '\0') {
		s = MYNAME;
	}
#endif

	if (s == NULL || *s == '\0') {
		/*
		 * As a last ditch thing, we *could* search Spool
		 * for D.<uucpname> and use that,
		 * but that is too much trouble, isn't it?
		 */
		logent("SYSTEM NAME", "CANNOT DETERMINE");
		s = "unknown";
	}

	/*
	 * copy uucpname back to caller-supplied buffer,
	 * truncating to 7 characters.
	 * Also set up subdirectory names, if subdirs are being used.
	 */
	d = name;
	while ((*d = *s++) && d < name + 7)
		d++;
	*(name + 7) = '\0';
	DEBUG(1, "My uucpname = %s\n", name);

#ifdef	UUDIR
	sprintf(DLocal, "D.%s", name);
	sprintf(DLocalX, "D.%sX", name);
#endif
}

#ifdef	WHOAMI
/*
 * simulate the 4.1a bsd system call by reading /usr/include/whoami.h
 * and looking for the #define sysname
 * CHANGE NOTICE (rti!trt): returns -1 on failure, 0 on success.
 */

#define	HDRFILE "/usr/include/whoami.h"

fakegethostname(name, len)
char *name;
int len;
{
	char buf[BUFSIZ];
	char bname[32];
	char hname[32];
	char nname[128];
	register char *p, *q, *nptr;
	int i;
	register FILE *fd;
	
	fd = fopen(HDRFILE, "r");
	if (fd == NULL)
		return(-1);
	
	hname[0] = 0;	/* rti!trt: was "hostunknown" */
	nname[0] = 0;
	nptr = nname;

	while (fgets(buf, sizeof buf, fd) != NULL) { /* each line in the file */
		if (sscanf(buf, "#define sysname \"%[^\"]\"", bname) == 1) {
			strcpy(hname, bname);
		} else if (sscanf(buf, "#define nickname \"%[^\"]\"", bname) == 1) {
			strcpy(nptr, bname);
			nptr += strlen(bname) + 1;
		} else if (sscanf(buf, "#define nickname%d \"%[^\"]\"", &i, bname) == 2) {
			strcpy(nptr, bname);
			nptr += strlen(bname) + 1;
		}
	}
	fclose(fd);
	if (hname[0] == 0)
		return(-1);	/* added by rti!trt */
	strncpy(name, hname, len);
	p = nname;
	i = strlen(hname) + 1;
	q = name + i;
	while (i < len && (p[0] != 0 || p[1] != 0)) {
		*q++ = *p++;
		i++;
	}
	*q++ = 0;
	return(0);
}
#endif
