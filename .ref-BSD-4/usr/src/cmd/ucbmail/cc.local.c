#

/*
 * Mail -- a mail program
 *
 * Computer Center Unix
 *
 * Local routines that are installation dependent.
 */

#include "rcv.h"

/*
 * Locate the user's mailbox file (ie, the place where new, unread
 * mail is queued).  At Computer Center, it is in /usr/mail/name.
 */

findmail()
{
	register char *cp;

	cp = copy("/usr/mail/", mailname);
	copy(myname, cp);
}

/*
 * Get rid of the queued mail.
 * This is essentially "mail -n > /dev/null &"
 */

demail()
{
	register int p;

	if (uid == 0) {
		remove(mailname);
		return;
	}
	if ((p = fork()) != 0)
		return;
	for (p = 0; p < 15; p++)
		 close(p);
	open("/dev/null", 2);
	dup(0);
	dup(0);
	for (p = SIGHUP; p <= SIGQUIT; p++)
		signal(p, SIG_IGN);
	execl(MAIL, "mail", "-n", 0);
	perror(MAIL);
	exit(1);
}

/*
 * Get the value of an environment variable.
 */

char *
getenv(name)
	char name[];
{
	register int t;
	static char val[30];

	t = ttyn(2);
	hget(t);
	if (equal(name, "SHELL"))
		return("/bin/csh");
	if (!equal(name, "HOME"))
		return(NOSTR);
	copy(hgethome(), val);
	return(val);
}

/*
 * Mail file lock / unlock.
 * Not implemented in this version.
 */

lock(name)
	char name[];
{

	return(0);
}

unlock()
{
	return(0);
}

/*
 * Discover user login name.
 */

username(uid, namebuf)
	char namebuf[];
{

	return(getname(uid, namebuf));
}

/*
 * Unix routine to do an "fopen" on file descriptor
 * The mode has to be repeated because you can't query its
 * status
 */

FILE *
fdopen(fd, mode)
register char *mode;
{
	extern int errno;
	register FILE *iop;
	extern FILE *_lastbuf;

	for (iop = _iob; iop->_flag&(_IOREAD|_IOWRT); iop++)
		if (iop >= _lastbuf)
			return(NULL);
	iop->_cnt = 0;
	iop->_file = fd;
	if (*mode != 'r') {
		iop->_flag |= _IOWRT;
		if (*mode == 'a')
			lseek(fd, 0L, 2);
	} else
		iop->_flag |= _IOREAD;
	return(iop);
}
