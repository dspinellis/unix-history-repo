#ifndef lint
static char sccsid[] = "@(#)uucpname.c	5.5 (Berkeley) 10/9/85";
#endif

#include "uucp.h"
#include <sys/stat.h>

/*LINTLIBRARY*/

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
char Myfullname[64];

/*
 *	uucpname(name)		get the uucp name
 *
 *	return code - none
 */
uucpname(name)
register char *name;
{
	register char *s;

	/*
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

#ifdef GETHOSTNAME
	if (s == NULL || *s == '\0') {
#ifdef VMS
		int i = sizeof(Myfullname);
#endif VMS

		s = Myfullname;
#ifdef VMS
		if(gethostname(Myfullname, &i) == -1) {
#else !VMS
		if(gethostname(Myfullname, sizeof(Myfullname)) == -1) {
#endif !VMS
			DEBUG(1, "gethostname", _FAILED);
			s = NULL;
		}
	}
#endif GETHOSTNAME

#ifdef	UNAME
	/* Use USG library routine */
	if (s == NULL || *s == '\0') {
		struct utsname utsn;

		if (uname(&utsn) == -1) {
			DEBUG(1, "uname", _FAILED);
			s = NULL;
		} else {
			strncpy(Myfullname, utsn.nodename, sizeof Myfullname);
			s = Myfullname;
		}
	}
#endif

#ifdef	WHOAMI
	/* Use fake gethostname() routine */
	if (s == NULL || *s == '\0') {

		s = Myfullname;
		if (fakegethostname(Myfullname, sizeof(Myfullname)) == -1) {
			DEBUG(1, "whoami search", _FAILED);
			s = NULL;
		}
	}
#endif

#ifdef	CCWHOAMI
	/* compile sysname right into uucp */
	if (s == NULL || *s == '\0') {
		s = sysname;
		strncpy(Myfullname, s, sizeof Myfullname);
	}
#endif

#ifdef	UUNAME
	/* uucp name is stored in /etc/uucpname or /local/uucpname */
	if (s == NULL || *s == '\0') {
		FILE *uucpf;

		s = Myfullname;
		if (((uucpf = fopen("/etc/uucpname", "r")) == NULL &&
		     (uucpf = fopen("/local/uucpname", "r")) == NULL) ||
			fgets(Myfullname, sizeof Myfullname, uucpf) == NULL) {
				DEBUG(1, "uuname search", _FAILED);
				s = NULL;
		}
		if (s == Myfullname) {
			register char *p;
			p = index(s, '\n');
			if (p)
				*p = '\0';
  		}
		if (uucpf != NULL)
			fclose(uucpf);
	}
#endif

#ifdef	GETMYHNAME
	/* Use 3Com's getmyhname() routine */
	if (s == NULL || *s == '\0') {
		if ((s = getmyhname()) == NULL)
			DEBUG(1, "getmyhname", _FAILED);
		else
			strncpy(Myfullname, s, sizeof Myfullname);
	}
#endif

#ifdef	MYNAME
	if (s == NULL || *s == '\0') {
		s = MYNAME;
		strncpy(Myfullname, s, sizeof Myfullname);
	}
#endif

	if (s == NULL || *s == '\0') {
		/*
		 * As a last ditch thing, we *could* search Spool
		 * for D.<uucpname> and use that,
		 * but that is too much trouble, isn't it?
		 */
		logent("SYSTEM NAME", "CANNOT DETERMINE");
		strcpy(Myfullname, "unknown");
	}

	/*
	 * copy uucpname back to caller-supplied buffer,
	 * truncating to MAXBASENAME characters.
	 * Also set up subdirectory names
	 * Truncate names at '.' if found to handle cases like
	 * seismo.css.gov being returned by gethostname().
	 * uucp sites should not have '.' in their name anyway
	 */
	s = index(Myfullname, '.');
	if (s != NULL)
		*s = '\0';
	strncpy(name, Myfullname, MAXBASENAME);
	name[MAXBASENAME] = '\0';
	DEBUG(1, "My uucpname = %s\n", name);

	sprintf(DLocal, "D.%.*s", SYSNSIZE, name);
	sprintf(DLocalX, "D.%.*sX", SYSNSIZE, name);
}

#ifdef	WHOAMI
/*
 * simulate the 4.2 bsd system call by reading /usr/include/whoami.h
 * and looking for the #define sysname
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
	
	hname[0] = 0;
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
		return FAIL;
	strncpy(name, hname, len);
	p = nname;
	i = strlen(hname) + 1;
	q = name + i;
	while (i < len && (p[0] != 0 || p[1] != 0)) {
		*q++ = *p++;
		i++;
	}
	*q++ = 0;
	return SUCCESS;
}
#endif
