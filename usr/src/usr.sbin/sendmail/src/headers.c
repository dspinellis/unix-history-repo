/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)headers.c	5.12 (Berkeley) %G%";
#endif /* not lint */

# include <errno.h>
# include "sendmail.h"

/*
**  CHOMPHEADER -- process and save a header line.
**
**	Called by collect and by readcf to deal with header lines.
**
**	Parameters:
**		line -- header as a text line.
**		def -- if set, this is a default value.
**
**	Returns:
**		flags for this header.
**
**	Side Effects:
**		The header is saved on the header list.
**		Contents of 'line' are destroyed.
*/

chompheader(line, def)
	char *line;
	bool def;
{
	register char *p;
	register HDR *h;
	HDR **hp;
	char *fname;
	char *fvalue;
	struct hdrinfo *hi;
	bool cond = FALSE;
	BITMAP mopts;
	extern char *crackaddr();
	extern ADDRESS *sendto();

# ifdef DEBUG
	if (tTd(31, 6))
		printf("chompheader: %s\n", line);
# endif DEBUG

	/* strip off options */
	clrbitmap(mopts);
	p = line;
	if (*p == '?')
	{
		/* have some */
		register char *q = index(p + 1, *p);
		
		if (q != NULL)
		{
			*q++ = '\0';
			while (*++p != '\0')
				setbitn(*p, mopts);
			p = q;
		}
		else
			usrerr("chompheader: syntax error, line \"%s\"", line);
		cond = TRUE;
	}

	/* find canonical name */
	fname = p;
	p = index(p, ':');
	if (p == NULL)
	{
		syserr("chompheader: syntax error, line \"%s\"", line);
		return (0);
	}
	fvalue = &p[1];
	while (isspace(*--p))
		continue;
	*++p = '\0';
	makelower(fname);

	/* strip field value on front */
	if (*fvalue == ' ')
		fvalue++;

	/* see if it is a known type */
	for (hi = HdrInfo; hi->hi_field != NULL; hi++)
	{
		if (strcmp(hi->hi_field, fname) == 0)
			break;
	}

	/* see if this is a resent message */
	if (!def && bitset(H_RESENT, hi->hi_flags))
		CurEnv->e_flags |= EF_RESENT;

	/* if this means "end of header" quit now */
	if (bitset(H_EOH, hi->hi_flags))
		return (hi->hi_flags);

	/* drop explicit From: if same as what we would generate -- for MH */
	p = "resent-from";
	if (!bitset(EF_RESENT, CurEnv->e_flags))
		p += 7;
	if (!def && !QueueRun && strcmp(fname, p) == 0)
	{
		if (CurEnv->e_from.q_paddr != NULL &&
		    strcmp(fvalue, CurEnv->e_from.q_paddr) == 0)
			return (hi->hi_flags);
	}

	/* drop forged Sender: values */
	p = "resent-sender";
	if (!bitset(EF_RESENT, CurEnv->e_flags))
		p += 7;
	if (!def && !QueueRun && CurEnv->e_from.q_mailer == LocalMailer &&
	    bitset(H_VALID, hi->hi_flags))
	{
		return (hi->hi_flags);
	}

	/* delete default value for this header */
	for (hp = &CurEnv->e_header; (h = *hp) != NULL; hp = &h->h_link)
	{
		if (strcmp(fname, h->h_field) == 0 &&
		    bitset(H_DEFAULT, h->h_flags) &&
		    !bitset(H_FORCE, h->h_flags))
			h->h_value = NULL;
	}

	/* create a new node */
	h = (HDR *) xalloc(sizeof *h);
	h->h_field = newstr(fname);
	h->h_value = NULL;
	h->h_link = NULL;
	bcopy((char *) mopts, (char *) h->h_mflags, sizeof mopts);
	*hp = h;
	h->h_flags = hi->hi_flags;
	if (def)
		h->h_flags |= H_DEFAULT;
	if (cond)
		h->h_flags |= H_CHECK;
	if (h->h_value != NULL)
		free((char *) h->h_value);
		(void) sendto(h->h_value, 0, (ADDRESS *) NULL, 0);

	/* hack to see if this is a new format message */
	if (!def && bitset(H_RCPT|H_FROM, h->h_flags) &&
	    (index(fvalue, ',') != NULL || index(fvalue, '(') != NULL ||
	     index(fvalue, '<') != NULL || index(fvalue, ';') != NULL))
	{
		CurEnv->e_flags &= ~EF_OLDSTYLE;
	}

	return (h->h_flags);
}
/*
**  ADDHEADER -- add a header entry to the end of the queue.
**
**	This bypasses the special checking of chompheader.
**
**	Parameters:
**		field -- the name of the header field.
**		value -- the value of the field.  It must be lower-cased.
**		e -- the envelope to add them to.
**
**	Returns:
**		none.
**
**	Side Effects:
**		adds the field on the list of headers for this envelope.
*/

addheader(field, value, e)
	char *field;
	char *value;
	ENVELOPE *e;
{
	register HDR *h;
	register struct hdrinfo *hi;
	HDR **hp;

	/* find info struct */
	for (hi = HdrInfo; hi->hi_field != NULL; hi++)
	{
		if (strcmp(field, hi->hi_field) == 0)
			break;
	}

	/* find current place in list -- keep back pointer? */
	for (hp = &e->e_header; (h = *hp) != NULL; hp = &h->h_link)
	{
		if (strcmp(field, h->h_field) == 0)
			break;
	}

	/* allocate space for new header */
	h = (HDR *) xalloc(sizeof *h);
	h->h_field = field;
	h->h_value = newstr(value);
	h->h_link = *hp;
	h->h_flags = hi->hi_flags | H_DEFAULT;
	clrbitmap(h->h_mflags);
	*hp = h;
}
/*
**  HVALUE -- return value of a header.
**
**	Only "real" fields (i.e., ones that have not been supplied
**	as a default) are used.
**
**	Parameters:
**		field -- the field name.
**
**	Returns:
**		pointer to the value part.
**		NULL if not found.
**
**	Side Effects:
**		none.
*/

char *
hvalue(field)
	char *field;
{
	register HDR *h;

	for (h = CurEnv->e_header; h != NULL; h = h->h_link)
	{
		if (!bitset(H_DEFAULT, h->h_flags) && strcmp(h->h_field, field) == 0)
			return (h->h_value);
	}
	return (NULL);
}
/*
**  ISHEADER -- predicate telling if argument is a header.
**
**	A line is a header if it has a single word followed by
**	optional white space followed by a colon.
**
**	Parameters:
**		s -- string to check for possible headerness.
**
**	Returns:
**		TRUE if s is a header.
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
isheader(s)
	register char *s;
{
	while (*s > ' ' && *s != ':' && *s != '\0')
		s++;

	/* following technically violates RFC822 */
	while (isspace(*s))
		s++;

	return (*s == ':');
}
/*
**  EATHEADER -- run through the stored header and extract info.
**
**	Parameters:
**		e -- the envelope to process.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets a bunch of global variables from information
**			in the collected header.
**		Aborts the message if the hop count is exceeded.
*/

eatheader(e)
	register ENVELOPE *e;
{
	register HDR *h;
	register char *p;
	int hopcnt = 0;

#ifdef DEBUG
	if (tTd(32, 1))
		printf("----- collected header -----\n");
#endif DEBUG
	for (h = e->e_header; h != NULL; h = h->h_link)
	{
#ifdef DEBUG
		extern char *capitalize();

		if (tTd(32, 1))
			printf("%s: %s\n", capitalize(h->h_field), h->h_value);
#endif DEBUG
		/* count the number of times it has been processed */
		if (bitset(H_TRACE, h->h_flags))
			hopcnt++;

		/* send to this person if we so desire */
		if (GrabTo && bitset(H_RCPT, h->h_flags) &&
		    !bitset(H_DEFAULT, h->h_flags) &&
		    (!bitset(EF_RESENT, CurEnv->e_flags) || bitset(H_RESENT, h->h_flags)))
		{
			sendtolist(h->h_value, (ADDRESS *) NULL, &CurEnv->e_sendqueue);
		}

		/* log the message-id */
#ifdef LOG
		if (!QueueRun && LogLevel > 8 && h->h_value != NULL &&
		    strcmp(h->h_field, "message-id") == 0)
		{
			char buf[MAXNAME];

			p = h->h_value;
			if (bitset(H_DEFAULT, h->h_flags))
			{
				expand(p, buf, &buf[sizeof buf], e);
				p = buf;
			}
			syslog(LOG_INFO, "%s: message-id=%s", e->e_id, p);
		}
#endif LOG
	}
#ifdef DEBUG
	if (tTd(32, 1))
		printf("----------------------------\n");
#endif DEBUG

	/* store hop count */
	if (hopcnt > e->e_hopcount)
		e->e_hopcount = hopcnt;

	/* message priority */
	p = hvalue("precedence");
	if (p != NULL)
		e->e_class = priencode(p);
	if (!QueueRun)
		e->e_msgpriority = e->e_msgsize
				 - e->e_class * WkClassFact
				 + e->e_nrcpts * WkRecipFact;

	/* return receipt to */
	p = hvalue("return-receipt-to");
	if (p != NULL)
		e->e_receiptto = p;

	/* errors to */
	p = hvalue("errors-to");
	if (p != NULL)
		sendtolist(p, (ADDRESS *) NULL, &e->e_errorqueue);

	/* from person */
	if (OpMode == MD_ARPAFTP)
	{
		register struct hdrinfo *hi = HdrInfo;

		for (p = NULL; p == NULL && hi->hi_field != NULL; hi++)
		{
			if (bitset(H_FROM, hi->hi_flags))
				p = hvalue(hi->hi_field);
		}
		if (p != NULL)
			setsender(p);
	}

	/* full name of from person */
	p = hvalue("full-name");
	if (p != NULL)
		define('x', p, e);

	/* date message originated */
	p = hvalue("posted-date");
	if (p == NULL)
		p = hvalue("date");
	if (p != NULL)
	{
		define('a', p, e);
		/* we don't have a good way to do canonical conversion ....
		define('d', newstr(arpatounix(p)), e);
		.... so we will ignore the problem for the time being */
	}

	/*
	**  Log collection information.
	*/

# ifdef LOG
	if (!QueueRun && LogLevel > 1)
	{
		char hbuf[100], *name = hbuf;

		if (RealHostName == NULL)
			name = "local";
		else if (RealHostName[0] == '[')
			name = RealHostName;
		else
			(void)sprintf(hbuf, "%.90s (%s)", 
			    RealHostName, inet_ntoa(RealHostAddr.sin_addr));
		syslog(LOG_INFO,
		    "%s: from=%s, size=%ld, class=%d, received from %s\n",
		    CurEnv->e_id, CurEnv->e_from.q_paddr, CurEnv->e_msgsize,
		    CurEnv->e_class, name);
	}
# endif LOG
}
/*
**  PRIENCODE -- encode external priority names into internal values.
**
**	Parameters:
**		p -- priority in ascii.
**
**	Returns:
**		priority as a numeric level.
**
**	Side Effects:
**		none.
*/

priencode(p)
	char *p;
{
	register int i;

	for (i = 0; i < NumPriorities; i++)
	{
		if (!strcasecmp(p, Priorities[i].pri_name))
			return (Priorities[i].pri_val);
	}

	/* unknown priority */
	return (0);
}
/*
**  CRACKADDR -- parse an address and turn it into a macro
**
**	This doesn't actually parse the address -- it just extracts
**	it and replaces it with "$g".  The parse is totally ad hoc
**	and isn't even guaranteed to leave something syntactically
**	identical to what it started with.  However, it does leave
**	something semantically identical.
**
**	The process is kind of strange.  There are a number of
**	interesting cases:
**		1.  comment <address> comment	==> comment <$g> comment
**		2.  address			==> address
**		3.  address (comment)		==> $g (comment)
**		4.  (comment) address		==> (comment) $g
**	And then there are the hard cases....
**		5.  add (comment) ress		==> $g (comment)
**		6.  comment <address (comment)>	==> comment <$g (comment)>
**		7.    .... etc ....
**
**	Parameters:
**		addr -- the address to be cracked.
**
**	Returns:
**		a pointer to the new version.
**
**	Side Effects:
**		none.
**
**	Warning:
**		The return value is saved in local storage and should
**		be copied if it is to be reused.
*/

char *
crackaddr(addr)
	register char *addr;
{
	register char *p;
	register int i;
	static char buf[MAXNAME];
	char *rhs;
	bool gotaddr;
	register char *bp;

# ifdef DEBUG
	if (tTd(33, 1))
		printf("crackaddr(%s)\n", addr);
# endif DEBUG

	(void) strcpy(buf, "");
	rhs = NULL;

	/* strip leading spaces */
	while (*addr != '\0' && isspace(*addr))
		addr++;

	/*
	**  See if we have anything in angle brackets.  If so, that is
	**  the address part, and the rest is the comment.
	*/

	p = index(addr, '<');
	if (p != NULL)
	{
		/* copy the beginning of the addr field to the buffer */
		*p = '\0';
		(void) strcpy(buf, addr);
		(void) strcat(buf, "<");
		*p++ = '<';

		/* skip spaces */
		while (isspace(*p))
			p++;

		/* find the matching right angle bracket */
		addr = p;
		for (i = 0; *p != '\0'; p++)
		{
			switch (*p)
			{
			  case '<':
				i++;
				break;

			  case '>':
				i--;
				break;
			}
			if (i < 0)
				break;
		}

		/* p now points to the closing quote (or a null byte) */
		if (*p != '\0')
		{
			/* make rhs point to the extra stuff at the end */
			rhs = p;
			*p++ = '\0';
		}
	}

	/*
	**  Now parse the real address part.  "addr" points to the (null
	**  terminated) version of what we are inerested in; rhs points
	**  to the extra stuff at the end of the line, if any.
	*/

	p = addr;

	/* now strip out comments */
	bp = &buf[strlen(buf)];
	gotaddr = FALSE;
	for (; *p != '\0'; p++)
	{
		if (*p == '(')
		{
			/* copy to matching close paren */
			*bp++ = *p++;
			for (i = 0; *p != '\0'; p++)
			{
				*bp++ = *p;
				switch (*p)
				{
				  case '(':
					i++;
					break;

				  case ')':
					i--;
					break;
				}
				if (i < 0)
					break;
			}
			continue;
		}

		/*
		**  If this is the first "real" character we have seen,
		**  then we put the "$g" in the buffer now.
		*/

		if (isspace(*p))
			*bp++ = *p;
		else if (!gotaddr)
		{
			(void) strcpy(bp, "\001g");
			bp += 2;
			gotaddr = TRUE;
		}
	}

	/* hack, hack.... strip trailing blanks */
	do
	{
		*bp-- = '\0';
	} while (isspace(*bp));
	bp++;

	/* put any right hand side back on */
	if (rhs != NULL)
	{
		*rhs = '>';
		(void) strcpy(bp, rhs);
	}

# ifdef DEBUG
	if (tTd(33, 1))
		printf("crackaddr=>`%s'\n", buf);
# endif DEBUG

	return (buf);
}
/*
**  PUTHEADER -- put the header part of a message from the in-core copy
**
**	Parameters:
**		fp -- file to put it on.
**		m -- mailer to use.
**		e -- envelope to use.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

putheader(fp, m, e)
	register FILE *fp;
	register MAILER *m;
	register ENVELOPE *e;
{
	char buf[BUFSIZ];
	register HDR *h;
	extern char *arpadate();
	extern char *capitalize();
	char obuf[MAXLINE];

	for (h = e->e_header; h != NULL; h = h->h_link)
	{
		register char *p;
		extern bool bitintersect();

		if (bitset(H_CHECK|H_ACHECK, h->h_flags) &&
		    !bitintersect(h->h_mflags, m->m_flags))
			continue;

		/* handle Resent-... headers specially */
		if (bitset(H_RESENT, h->h_flags) && !bitset(EF_RESENT, e->e_flags))
			continue;

		p = h->h_value;
		if (bitset(H_DEFAULT, h->h_flags))
		{
			/* macro expand value if generated internally */
			expand(p, buf, &buf[sizeof buf], e);
			p = buf;
			if (p == NULL || *p == '\0')
				continue;
		}

		if (bitset(H_FROM|H_RCPT, h->h_flags))
		{
			/* address field */
			bool oldstyle = bitset(EF_OLDSTYLE, e->e_flags);

			if (bitset(H_FROM, h->h_flags))
				oldstyle = FALSE;
			commaize(h, p, fp, oldstyle, m);
		}
		else
		{
			/* vanilla header line */
			register char *nlp;

			(void) sprintf(obuf, "%s: ", capitalize(h->h_field));
			while ((nlp = index(p, '\n')) != NULL)
			{
				*nlp = '\0';
				(void) strcat(obuf, p);
				*nlp = '\n';
				putline(obuf, fp, m);
				p = ++nlp;
				obuf[0] = '\0';
			}
			(void) strcat(obuf, p);
			putline(obuf, fp, m);
		}
	}
}
/*
**  COMMAIZE -- output a header field, making a comma-translated list.
**
**	Parameters:
**		h -- the header field to output.
**		p -- the value to put in it.
**		fp -- file to put it to.
**		oldstyle -- TRUE if this is an old style header.
**		m -- a pointer to the mailer descriptor.  If NULL,
**			don't transform the name at all.
**
**	Returns:
**		none.
**
**	Side Effects:
**		outputs "p" to file "fp".
*/

commaize(h, p, fp, oldstyle, m)
	register HDR *h;
	register char *p;
	FILE *fp;
	bool oldstyle;
	register MAILER *m;
{
	register char *obp;
	int opos;
	bool firstone = TRUE;
	char obuf[MAXLINE + 3];

	/*
	**  Output the address list translated by the
	**  mailer and with commas.
	*/

# ifdef DEBUG
	if (tTd(14, 2))
		printf("commaize(%s: %s)\n", h->h_field, p);
# endif DEBUG

	obp = obuf;
	(void) sprintf(obp, "%s: ", capitalize(h->h_field));
	opos = strlen(h->h_field) + 2;
	obp += opos;

	/*
	**  Run through the list of values.
	*/

	while (*p != '\0')
	{
		register char *name;
		char savechar;
		extern char *remotename();
		extern char *DelimChar;		/* defined in prescan */

		/*
		**  Find the end of the name.  New style names
		**  end with a comma, old style names end with
		**  a space character.  However, spaces do not
		**  necessarily delimit an old-style name -- at
		**  signs mean keep going.
		*/

		/* find end of name */
		while (isspace(*p) || *p == ',')
			p++;
		name = p;
		for (;;)
		{
			char *oldp;
			char pvpbuf[PSBUFSIZE];
			extern bool isatword();
			extern char **prescan();

			(void) prescan(p, oldstyle ? ' ' : ',', pvpbuf);
			p = DelimChar;

			/* look to see if we have an at sign */
			oldp = p;
			while (*p != '\0' && isspace(*p))
				p++;

			if (*p != '@' && !isatword(p))
			{
				p = oldp;
				break;
			}
			p += *p == '@' ? 1 : 2;
			while (*p != '\0' && isspace(*p))
				p++;
		}
		/* at the end of one complete name */

		/* strip off trailing white space */
		while (p >= name && (isspace(*p) || *p == ',' || *p == '\0'))
			p--;
		if (++p == name)
			continue;
		savechar = *p;
		*p = '\0';

		/* translate the name to be relative */
		name = remotename(name, m, bitset(H_FROM, h->h_flags), FALSE);
		if (*name == '\0')
		{
			*p = savechar;
			continue;
		}

		/* output the name with nice formatting */
		opos += qstrlen(name);
		if (!firstone)
			opos += 2;
		if (opos > 78 && !firstone)
		{
			(void) strcpy(obp, ",\n");
			putline(obuf, fp, m);
			obp = obuf;
			(void) sprintf(obp, "        ");
			opos = strlen(obp);
			obp += opos;
			opos += qstrlen(name);
		}
		else if (!firstone)
		{
			(void) sprintf(obp, ", ");
			obp += 2;
		}

		/* strip off quote bits as we output */
		while (*name != '\0' && obp < &obuf[MAXLINE])
		{
			if (bitset(0200, *name))
				*obp++ = '\\';
			*obp++ = *name++ & ~0200;
		}
		firstone = FALSE;
		*p = savechar;
	}
	(void) strcpy(obp, "\n");
	putline(obuf, fp, m);
}
/*
**  ISATWORD -- tell if the word we are pointing to is "at".
**
**	Parameters:
**		p -- word to check.
**
**	Returns:
**		TRUE -- if p is the word at.
**		FALSE -- otherwise.
**
**	Side Effects:
**		none.
*/

bool
isatword(p)
	register char *p;
{
	extern char lower();

	if (lower(p[0]) == 'a' && lower(p[1]) == 't' &&
	    p[2] != '\0' && isspace(p[2]))
		return (TRUE);
	return (FALSE);
}
