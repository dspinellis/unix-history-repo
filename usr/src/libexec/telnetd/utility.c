/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)utility.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#define PRINTOPTIONS
#include "telnetd.h"

/*
 * utility functions performing io related tasks
 */

/*
 * ttloop
 *
 *	A small subroutine to flush the network output buffer, get some data
 * from the network, and pass it through the telnet state machine.  We
 * also flush the pty input buffer (by dropping its data) if it becomes
 * too full.
 */

void
ttloop()
{
    void netflush();

#ifdef DIAGNOSTICS
    if (diagnostic & TD_REPORT) {
	sprintf(nfrontp, "td: ttloop\r\n");
	nfrontp += strlen(nfrontp);
    }
#endif /* DIAGNOSTICS */
    if (nfrontp-nbackp) {
	netflush();
    }
    ncc = read(net, netibuf, sizeof netibuf);
    if (ncc < 0) {
	syslog(LOG_INFO, "ttloop:  read: %m\n");
	exit(1);
    } else if (ncc == 0) {
	syslog(LOG_INFO, "ttloop:  peer died: %m\n");
	exit(1);
    }
#ifdef DIAGNOSTICS
    if (diagnostic & TD_REPORT) {
	sprintf(nfrontp, "td: ttloop read %d chars\r\n", ncc);
	nfrontp += strlen(nfrontp);
    }
#endif /* DIAGNOSTICS */
    netip = netibuf;
    telrcv();			/* state machine */
    if (ncc > 0) {
	pfrontp = pbackp = ptyobuf;
	telrcv();
    }
}  /* end of ttloop */

/*
 * Check a descriptor to see if out of band data exists on it.
 */
stilloob(s)
int	s;		/* socket number */
{
    static struct timeval timeout = { 0 };
    fd_set	excepts;
    int value;

    do {
	FD_ZERO(&excepts);
	FD_SET(s, &excepts);
	value = select(s+1, (fd_set *)0, (fd_set *)0, &excepts, &timeout);
    } while ((value == -1) && (errno == EINTR));

    if (value < 0) {
	fatalperror(pty, "select");
    }
    if (FD_ISSET(s, &excepts)) {
	return 1;
    } else {
	return 0;
    }
}

ptyflush()
{
	int n;

	if ((n = pfrontp - pbackp) > 0) {
#ifdef DIAGNOSTICS
		if (diagnostic & (TD_REPORT | TD_PTYDATA)) {
			sprintf(nfrontp, "td: ptyflush %d chars\r\n", n);
			nfrontp += strlen(nfrontp);
		}
		if (diagnostic & TD_PTYDATA) {
			printdata("pd", pbackp, n);
		}
#endif /* DIAGNOSTICS */
		n = write(pty, pbackp, n);
	}
	if (n < 0)
		return;
	pbackp += n;
	if (pbackp == pfrontp)
		pbackp = pfrontp = ptyobuf;
}

/*
 * nextitem()
 *
 *	Return the address of the next "item" in the TELNET data
 * stream.  This will be the address of the next character if
 * the current address is a user data character, or it will
 * be the address of the character following the TELNET command
 * if the current address is a TELNET IAC ("I Am a Command")
 * character.
 */
char *
nextitem(current)
char	*current;
{
    if ((*current&0xff) != IAC) {
	return current+1;
    }
    switch (*(current+1)&0xff) {
    case DO:
    case DONT:
    case WILL:
    case WONT:
	return current+3;
    case SB:		/* loop forever looking for the SE */
	{
	    register char *look = current+2;

	    for (;;) {
		if ((*look++&0xff) == IAC) {
		    if ((*look++&0xff) == SE) {
			return look;
		    }
		}
	    }
	}
    default:
	return current+2;
    }
}  /* end of nextitem */


/*
 * netclear()
 *
 *	We are about to do a TELNET SYNCH operation.  Clear
 * the path to the network.
 *
 *	Things are a bit tricky since we may have sent the first
 * byte or so of a previous TELNET command into the network.
 * So, we have to scan the network buffer from the beginning
 * until we are up to where we want to be.
 *
 *	A side effect of what we do, just to keep things
 * simple, is to clear the urgent data pointer.  The principal
 * caller should be setting the urgent data pointer AFTER calling
 * us in any case.
 */
netclear()
{
    register char *thisitem, *next;
    char *good;
#define	wewant(p)	((nfrontp > p) && ((*p&0xff) == IAC) && \
				((*(p+1)&0xff) != EC) && ((*(p+1)&0xff) != EL))

    thisitem = netobuf;

    while ((next = nextitem(thisitem)) <= nbackp) {
	thisitem = next;
    }

    /* Now, thisitem is first before/at boundary. */

    good = netobuf;	/* where the good bytes go */

    while (nfrontp > thisitem) {
	if (wewant(thisitem)) {
	    int length;

	    next = thisitem;
	    do {
		next = nextitem(next);
	    } while (wewant(next) && (nfrontp > next));
	    length = next-thisitem;
	    bcopy(thisitem, good, length);
	    good += length;
	    thisitem = next;
	} else {
	    thisitem = nextitem(thisitem);
	}
    }

    nbackp = netobuf;
    nfrontp = good;		/* next byte to be sent */
    neturg = 0;
}  /* end of netclear */

/*
 *  netflush
 *		Send as much data as possible to the network,
 *	handling requests for urgent data.
 */
void
netflush()
{
    int n;
    extern int not42;

    if ((n = nfrontp - nbackp) > 0) {
#ifdef DIAGNOSTICS
	if (diagnostic & TD_REPORT) {
	    sprintf(nfrontp, "td: netflush %d chars\r\n", n);
	    n += strlen(nfrontp);  /* get count first */
	    nfrontp += strlen(nfrontp);  /* then move pointer */
	}
#endif /* DIAGNOSTICS */
	/*
	 * if no urgent data, or if the other side appears to be an
	 * old 4.2 client (and thus unable to survive TCP urgent data),
	 * write the entire buffer in non-OOB mode.
	 */
	if ((neturg == 0) || (not42 == 0)) {
	    n = write(net, nbackp, n);	/* normal write */
	} else {
	    n = neturg - nbackp;
	    /*
	     * In 4.2 (and 4.3) systems, there is some question about
	     * what byte in a sendOOB operation is the "OOB" data.
	     * To make ourselves compatible, we only send ONE byte
	     * out of band, the one WE THINK should be OOB (though
	     * we really have more the TCP philosophy of urgent data
	     * rather than the Unix philosophy of OOB data).
	     */
	    if (n > 1) {
		n = send(net, nbackp, n-1, 0);	/* send URGENT all by itself */
	    } else {
		n = send(net, nbackp, n, MSG_OOB);	/* URGENT data */
	    }
	}
    }
    if (n < 0) {
	if (errno == EWOULDBLOCK || errno == EINTR)
		return;
	cleanup();
    }
    nbackp += n;
    if (nbackp >= neturg) {
	neturg = 0;
    }
    if (nbackp == nfrontp) {
	nbackp = nfrontp = netobuf;
    }
    return;
}  /* end of netflush */


/*
 * writenet
 *
 * Just a handy little function to write a bit of raw data to the net.
 * It will force a transmit of the buffer if necessary
 *
 * arguments
 *    ptr - A pointer to a character string to write
 *    len - How many bytes to write
 */
writenet(ptr, len)
register char *ptr;
register int len;
{
	/* flush buffer if no room for new data) */
	if ((&netobuf[BUFSIZ] - nfrontp) < len) {
		/* if this fails, don't worry, buffer is a little big */
		netflush();
	}

	bcopy(ptr, nfrontp, len);
	nfrontp += len;

}  /* end of writenet */


/*
 * miscellaneous functions doing a variety of little jobs follow ...
 */


fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	(void) sprintf(buf, "telnetd: %s.\r\n", msg);
	(void) write(f, buf, (int)strlen(buf));
	sleep(1);	/*XXX*/
	exit(1);
}

fatalperror(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ], *strerror();

	(void) sprintf(buf, "%s: %s\r\n", msg, strerror(errno));
	fatal(f, buf);
}

char editedhost[32];

edithost(pat, host)
	register char *pat;
	register char *host;
{
	register char *res = editedhost;
	char *strncpy();

	if (!pat)
		pat = "";
	while (*pat) {
		switch (*pat) {

		case '#':
			if (*host)
				host++;
			break;

		case '@':
			if (*host)
				*res++ = *host++;
			break;

		default:
			*res++ = *pat;
			break;
		}
		if (res == &editedhost[sizeof editedhost - 1]) {
			*res = '\0';
			return;
		}
		pat++;
	}
	if (*host)
		(void) strncpy(res, host,
				sizeof editedhost - (res - editedhost) -1);
	else
		*res = '\0';
	editedhost[sizeof editedhost - 1] = '\0';
}

static char *putlocation;

putstr(s)
register char *s;
{

	while (*s)
		putchr(*s++);
}

putchr(cc)
{
	*putlocation++ = cc;
}

putf(cp, where)
register char *cp;
char *where;
{
	char *slash;
#ifndef	NO_GETTYTAB
	char datebuffer[60];
#endif	/* NO_GETTYTAB */
	extern char *rindex();

	putlocation = where;

	while (*cp) {
		if (*cp != '%') {
			putchr(*cp++);
			continue;
		}
		switch (*++cp) {

		case 't':
			slash = rindex(line, '/');
			if (slash == (char *) 0)
				putstr(line);
			else
				putstr(&slash[1]);
			break;

		case 'h':
			putstr(editedhost);
			break;

#ifndef	NO_GETTYTAB
		case 'd':
			get_date(datebuffer);
			putstr(datebuffer);
			break;
#endif	/* NO_GETTYTAB */

		case '%':
			putchr('%');
			break;
		}
		cp++;
	}
}

/*ARGSUSED*/
#ifdef	NO_GETTYTAB
getent(cp, name)
char *cp, *name;
{
	return(0);
}

/*ARGSUSED*/
char *
getstr(cp, cpp)
char *cp, **cpp;
{
	return(0);
}
#endif	/* NO_GETTYTAB */

#ifdef DIAGNOSTICS
/*
 * Print telnet options and commands in plain text, if possible.
 */
void
printoption(fmt, option)
register char *fmt;
register int option;
{
	if (TELOPT_OK(option))
		sprintf(nfrontp, "%s %s\r\n", fmt, TELOPT(option));
	else if (TELCMD_OK(option))
		sprintf(nfrontp, "%s %s\r\n", fmt, TELCMD(option));
	else
		sprintf(nfrontp, "%s %d\r\n", fmt, option);
	nfrontp += strlen(nfrontp);
	return;
}

char *slcnames[] = { SLC_NAMES };

void
printsub(dirp, pointer, length)
char	*dirp;
unsigned char	*pointer;	/* where suboption data sits */
int	length;			/* length of suboption data */
{
    register int i;

	if (dirp) {
	    sprintf(nfrontp, "%s suboption ", dirp);
	    nfrontp += strlen(nfrontp);
	    if (length >= 3) {
		register int j;

		i = pointer[length-2];
		j = pointer[length-1];

		if (i != IAC || j != SE) {
		    sprintf(nfrontp, "(terminated by ");
		    nfrontp += strlen(nfrontp);
		    if (TELOPT_OK(i))
			sprintf(nfrontp, "%s ", TELOPT(i));
		    else if (TELCMD_OK(i))
			sprintf(nfrontp, "%s ", TELCMD(i));
		    else
			sprintf(nfrontp, "%d ", i);
		    nfrontp += strlen(nfrontp);
		    if (TELOPT_OK(j))
			sprintf(nfrontp, "%s", TELOPT(j));
		    else if (TELCMD_OK(j))
			sprintf(nfrontp, "%s", TELCMD(j));
		    else
			sprintf(nfrontp, "%d", j);
		    nfrontp += strlen(nfrontp);
		    sprintf(nfrontp, ", not IAC SE!) ");
		    nfrontp += strlen(nfrontp);
		}
	    }
	    length -= 2;
	}
	if (length < 1) {
	    sprintf(nfrontp, "(Empty suboption???)");
	    nfrontp += strlen(nfrontp);
	    return;
	}
	switch (pointer[0]) {
	case TELOPT_TTYPE:
	    sprintf(nfrontp, "TERMINAL-TYPE ");
	    nfrontp += strlen(nfrontp);
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		sprintf(nfrontp, "IS \"%.*s\"", length-2, (char *)pointer+2);
		break;
	    case TELQUAL_SEND:
		sprintf(nfrontp, "SEND");
		break;
	    default:
		sprintf(nfrontp,
				"- unknown qualifier %d (0x%x).",
				pointer[1], pointer[1]);
	    }
	    nfrontp += strlen(nfrontp);
	    break;
	case TELOPT_TSPEED:
	    sprintf(nfrontp, "TERMINAL-SPEED");
	    nfrontp += strlen(nfrontp);
	    if (length < 2) {
		sprintf(nfrontp, " (empty suboption???)");
		nfrontp += strlen(nfrontp);
		break;
	    }
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		sprintf(nfrontp, " IS %.*s", length-2, (char *)pointer+2);
		nfrontp += strlen(nfrontp);
		break;
	    default:
		if (pointer[1] == 1)
		    sprintf(nfrontp, " SEND");
		else
		    sprintf(nfrontp, " %d (unknown)", pointer[1]);
		nfrontp += strlen(nfrontp);
		for (i = 2; i < length; i++) {
		    sprintf(nfrontp, " ?%d?", pointer[i]);
		    nfrontp += strlen(nfrontp);
		}
		break;
	    }
	    break;

	case TELOPT_LFLOW:
	    sprintf(nfrontp, "TOGGLE-FLOW-CONTROL");
	    nfrontp += strlen(nfrontp);
	    if (length < 2) {
		sprintf(nfrontp, " (empty suboption???)");
		nfrontp += strlen(nfrontp);
		break;
	    }
	    switch (pointer[1]) {
	    case 0:
		sprintf(nfrontp, " OFF"); break;
	    case 1:
		sprintf(nfrontp, " ON"); break;
	    default:
		sprintf(nfrontp, " %d (unknown)", pointer[1]);
	    }
	    nfrontp += strlen(nfrontp);
	    for (i = 2; i < length; i++) {
		sprintf(nfrontp, " ?%d?", pointer[i]);
		nfrontp += strlen(nfrontp);
	    }
	    break;

	case TELOPT_NAWS:
	    sprintf(nfrontp, "NAWS");
	    nfrontp += strlen(nfrontp);
	    if (length < 2) {
		sprintf(nfrontp, " (empty suboption???)");
		nfrontp += strlen(nfrontp);
		break;
	    }
	    if (length == 2) {
		sprintf(nfrontp, " ?%d?", pointer[1]);
		nfrontp += strlen(nfrontp);
		break;
	    }
	    sprintf(nfrontp, " %d %d (%d)",
		pointer[1], pointer[2],
		(int)((((unsigned int)pointer[1])<<8)|((unsigned int)pointer[2])));
	    nfrontp += strlen(nfrontp);
	    if (length == 4) {
		sprintf(nfrontp, " ?%d?", pointer[3]);
		nfrontp += strlen(nfrontp);
		break;
	    }
	    sprintf(nfrontp, " %d %d (%d)",
		pointer[3], pointer[4],
		(int)((((unsigned int)pointer[3])<<8)|((unsigned int)pointer[4])));
	    nfrontp += strlen(nfrontp);
	    for (i = 5; i < length; i++) {
		sprintf(nfrontp, " ?%d?", pointer[i]);
		nfrontp += strlen(nfrontp);
	    }
	    break;

	case TELOPT_LINEMODE:
	    sprintf(nfrontp, "LINEMODE ");
	    nfrontp += strlen(nfrontp);
	    if (length < 2) {
		sprintf(nfrontp, " (empty suboption???)");
		nfrontp += strlen(nfrontp);
		break;
	    }
	    switch (pointer[1]) {
	    case WILL:
		sprintf(nfrontp, "WILL ");
		goto common;
	    case WONT:
		sprintf(nfrontp, "WONT ");
		goto common;
	    case DO:
		sprintf(nfrontp, "DO ");
		goto common;
	    case DONT:
		sprintf(nfrontp, "DONT ");
	    common:
		nfrontp += strlen(nfrontp);
		if (length < 3) {
		    sprintf(nfrontp, "(no option???)");
		    nfrontp += strlen(nfrontp);
		    break;
		}
		switch (pointer[2]) {
		case LM_FORWARDMASK:
		    sprintf(nfrontp, "Forward Mask");
		    nfrontp += strlen(nfrontp);
		    for (i = 3; i < length; i++) {
			sprintf(nfrontp, " %x", pointer[i]);
			nfrontp += strlen(nfrontp);
		    }
		    break;
		default:
		    sprintf(nfrontp, "%d (unknown)", pointer[2]);
		    nfrontp += strlen(nfrontp);
		    for (i = 3; i < length; i++) {
			sprintf(nfrontp, " %d", pointer[i]);
			nfrontp += strlen(nfrontp);
		    }
		    break;
		}
		break;
		
	    case LM_SLC:
		sprintf(nfrontp, "SLC");
		nfrontp += strlen(nfrontp);
		for (i = 2; i < length - 2; i += 3) {
		    if (pointer[i+SLC_FUNC] <= NSLC)
			sprintf(nfrontp, " %s", slcnames[pointer[i+SLC_FUNC]]);
		    else
			sprintf(nfrontp, " %d", pointer[i+SLC_FUNC]);
		    nfrontp += strlen(nfrontp);
		    switch (pointer[i+SLC_FLAGS]&SLC_LEVELBITS) {
		    case SLC_NOSUPPORT:
			sprintf(nfrontp, " NOSUPPORT"); break;
		    case SLC_CANTCHANGE:
			sprintf(nfrontp, " CANTCHANGE"); break;
		    case SLC_VARIABLE:
			sprintf(nfrontp, " VARIABLE"); break;
		    case SLC_DEFAULT:
			sprintf(nfrontp, " DEFAULT"); break;
		    }
		    nfrontp += strlen(nfrontp);
		    sprintf(nfrontp, "%s%s%s",
			pointer[i+SLC_FLAGS]&SLC_ACK ? "|ACK" : "",
			pointer[i+SLC_FLAGS]&SLC_FLUSHIN ? "|FLUSHIN" : "",
			pointer[i+SLC_FLAGS]&SLC_FLUSHOUT ? "|FLUSHOUT" : "");
		    nfrontp += strlen(nfrontp);
		    if (pointer[i+SLC_FLAGS]& ~(SLC_ACK|SLC_FLUSHIN|
						SLC_FLUSHOUT| SLC_LEVELBITS)) {
			sprintf(nfrontp, "(0x%x)", pointer[i+SLC_FLAGS]);
			nfrontp += strlen(nfrontp);
		    }
		    sprintf(nfrontp, " %d;", pointer[i+SLC_VALUE]);
		    nfrontp += strlen(nfrontp);
		    if ((pointer[i+SLC_VALUE] == IAC) &&
			(pointer[i+SLC_VALUE+1] == IAC))
				i++;
		}
		for (; i < length; i++) {
		    sprintf(nfrontp, " ?%d?", pointer[i]);
		    nfrontp += strlen(nfrontp);
		}
		break;

	    case LM_MODE:
		sprintf(nfrontp, "MODE ");
		nfrontp += strlen(nfrontp);
		if (length < 3) {
		    sprintf(nfrontp, "(no mode???)");
		    nfrontp += strlen(nfrontp);
		    break;
		}
		{
		    char tbuf[32];
		    sprintf(tbuf, "%s%s%s%s%s",
			pointer[2]&MODE_EDIT ? "|EDIT" : "",
			pointer[2]&MODE_TRAPSIG ? "|TRAPSIG" : "",
			pointer[2]&MODE_SOFT_TAB ? "|SOFT_TAB" : "",
			pointer[2]&MODE_LIT_ECHO ? "|LIT_ECHO" : "",
			pointer[2]&MODE_ACK ? "|ACK" : "");
		    sprintf(nfrontp, "%s", tbuf[1] ? &tbuf[1] : "0");
		    nfrontp += strlen(nfrontp);
		}
		if (pointer[2]&~(MODE_EDIT|MODE_TRAPSIG|MODE_ACK)) {
		    sprintf(nfrontp, " (0x%x)", pointer[2]);
		    nfrontp += strlen(nfrontp);
		}
		for (i = 3; i < length; i++) {
		    sprintf(nfrontp, " ?0x%x?", pointer[i]);
		    nfrontp += strlen(nfrontp);
		}
		break;
	    default:
		sprintf(nfrontp, "%d (unknown)", pointer[1]);
		nfrontp += strlen(nfrontp);
		for (i = 2; i < length; i++) {
		    sprintf(nfrontp, " %d", pointer[i]);
		    nfrontp += strlen(nfrontp);
		}
	    }
	    break;

	case TELOPT_STATUS: {
	    register char *cp;
	    register int j, k;

	    sprintf(nfrontp, "STATUS");
	    nfrontp += strlen(nfrontp);

	    switch (pointer[1]) {
	    default:
		if (pointer[1] == TELQUAL_SEND)
		    sprintf(nfrontp, " SEND");
		else
		    sprintf(nfrontp, " %d (unknown)", pointer[1]);
		nfrontp += strlen(nfrontp);
		for (i = 2; i < length; i++) {
		    sprintf(nfrontp, " ?%d?", pointer[i]);
		    nfrontp += strlen(nfrontp);
		}
		break;
	    case TELQUAL_IS:
		sprintf(nfrontp, " IS\r\n");
		nfrontp += strlen(nfrontp);

		for (i = 2; i < length; i++) {
		    switch(pointer[i]) {
		    case DO:	cp = "DO"; goto common2;
		    case DONT:	cp = "DONT"; goto common2;
		    case WILL:	cp = "WILL"; goto common2;
		    case WONT:	cp = "WONT"; goto common2;
		    common2:
			i++;
			if (TELOPT_OK((int)pointer[i]))
			    sprintf(nfrontp, " %s %s", cp, TELOPT(pointer[i]));
			else
			    sprintf(nfrontp, " %s %d", cp, pointer[i]);
			nfrontp += strlen(nfrontp);

			sprintf(nfrontp, "\r\n");
			nfrontp += strlen(nfrontp);
			break;

		    case SB:
			sprintf(nfrontp, " SB ");
			nfrontp += strlen(nfrontp);
			i++;
			j = k = i;
			while (j < length) {
			    if (pointer[j] == SE) {
				if (j+1 == length)
				    break;
				if (pointer[j+1] == SE)
				    j++;
				else
				    break;
			    }
			    pointer[k++] = pointer[j++];
			}
			printsub(0, &pointer[i], k - i);
			if (i < length) {
			    sprintf(nfrontp, " SE");
			    nfrontp += strlen(nfrontp);
			    i = j;
			} else
			    i = j - 1;

			sprintf(nfrontp, "\r\n");
			nfrontp += strlen(nfrontp);

			break;
				
		    default:
			sprintf(nfrontp, " %d", pointer[i]);
			nfrontp += strlen(nfrontp);
			break;
		    }
		}
		break;
	    }
	    break;
	  }

	case TELOPT_XDISPLOC:
	    sprintf(nfrontp, "X-DISPLAY-LOCATION ");
	    nfrontp += strlen(nfrontp);
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		sprintf(nfrontp, "IS \"%.*s\"", length-2, (char *)pointer+2);
		break;
	    case TELQUAL_SEND:
		sprintf(nfrontp, "SEND");
		break;
	    default:
		sprintf(nfrontp, "- unknown qualifier %d (0x%x).",
				pointer[1], pointer[1]);
	    }
	    nfrontp += strlen(nfrontp);
	    break;

	case TELOPT_ENVIRON:
	    sprintf(nfrontp, "ENVIRON ");
	    nfrontp += strlen(nfrontp);
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		sprintf(nfrontp, "IS ");
		goto env_common;
	    case TELQUAL_SEND:
		sprintf(nfrontp, "SEND ");
		goto env_common;
	    case TELQUAL_INFO:
		sprintf(nfrontp, "INFO ");
	    env_common:
	    nfrontp += strlen(nfrontp);
		{
		    register int noquote = 2;
		    for (i = 2; i < length; i++ ) {
			switch (pointer[i]) {
			case ENV_VAR:
			    if (pointer[1] == TELQUAL_SEND)
				goto def_case;
			    sprintf(nfrontp, "\" VAR " + noquote);
			    nfrontp += strlen(nfrontp);
			    noquote = 2;
			    break;

			case ENV_VALUE:
			    sprintf(nfrontp, "\" VALUE " + noquote);
			    nfrontp += strlen(nfrontp);
			    noquote = 2;
			    break;

			case ENV_ESC:
			    sprintf(nfrontp, "\" ESC " + noquote);
			    nfrontp += strlen(nfrontp);
			    noquote = 2;
			    break;

			default:
			def_case:
			    if (isprint(pointer[i]) && pointer[i] != '"') {
				if (noquote) {
				    *nfrontp++ = '"';
				    noquote = 0;
				}
				*nfrontp++ = pointer[i];
			    } else {
				sprintf(nfrontp, "\" %03o " + noquote,
							pointer[i]);
				nfrontp += strlen(nfrontp);
				noquote = 2;
			    }
			    break;
			}
		    }
		    if (!noquote)
			*nfrontp++ = '"';
		    break;
		}
	    }
	    break;

	default:
	    sprintf(nfrontp, "Unknown option ");
	    nfrontp += strlen(nfrontp);
	    for (i = 0; i < length; i++) {
		sprintf(nfrontp, " %d", pointer[i]);
		nfrontp += strlen(nfrontp);
	    }
	    break;
	}
	sprintf(nfrontp, "\r\n");
	nfrontp += strlen(nfrontp);
}

/*
 * Dump a data buffer in hex and ascii to the output data stream.
 */
void
printdata(tag, ptr, cnt)
register char *tag;
register char *ptr;
register int cnt;
{
register int i;
char xbuf[30];

	while (cnt) {
		/* flush net output buffer if no room for new data) */
		if ((&netobuf[BUFSIZ] - nfrontp) < 80) {
			netflush();
		}

		/* add a line of output */
		sprintf(nfrontp, "%s: ", tag);
		nfrontp += strlen(nfrontp);
		for (i = 0; i < 20 && cnt; i++) {
			sprintf(nfrontp, "%02x", *ptr);
			nfrontp += strlen(nfrontp); 
			if (isprint(*ptr)) {
				xbuf[i] = *ptr;
			} else {
				xbuf[i] = '.';
			}
			if (i % 2) { 
				*nfrontp = ' ';
				nfrontp++;
			}
			cnt--;
			ptr++;
		}
		xbuf[i] = '\0';
		sprintf(nfrontp, " %s\r\n", xbuf );
		nfrontp += strlen(nfrontp);
	} 
}

#endif /* DIAGNOSTICS */

#ifdef	NO_STRERROR
char *
strerror(errno)
{
	extern char *sys_errlist[];

	return(sys_errlist[errno]);
}
#endif
