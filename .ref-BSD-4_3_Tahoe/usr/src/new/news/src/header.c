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
 * header.c - header functions plus some other goodies
 */
/*LINTLIBRARY*/

#ifdef SCCSID
static char	*SccsId = "@(#)header.c	2.49	10/7/87";
#endif /* SCCSID */

#include <stdio.h>
#include "params.h"
#include "patchlevel.h"

char *hfgets();

char *news_version = NEWS_VERSION;

/*
 * Read header from file fp into *hp.  If wholething is FALSE,
 * it's an incremental read, otherwise start from scratch.
 * Return (FILE *) if header okay, else NULL.
 */
FILE *
hread(hp, fp, wholething)
register struct hbuf *hp;
FILE *fp;
int wholething;
{
#ifndef GENERICPATH
	register int	len;
#endif /* GENERICPATH */
	register int	i;
#ifdef OLD
	char *p;
#endif /* OLD */

	if (wholething) {
		for(i=0;i<NUNREC;i++)
			if (hp->unrec[i] != NULL)
				free(hp->unrec[i]);
			else
				break;
		bzero((char *)hp, sizeof (*hp));
		for (i=0;i<NUNREC;i++)
			hp->unrec[i] = NULL;
	}

	/* Check that it's a B news style header. */
	if (hfgets(bfr, PATHLEN, fp) != NULL && isalpha(bfr[0])
	    && index(bfr, ':'))
		if (frmread(fp, hp))
			goto strip;

	if (!nstrip(bfr+1))
		return NULL;

	/* It's not.  Try A news (begins with PROTO). */
	if (bfr[0] != PROTO)
		return NULL;
#ifndef OLD
	logerr("Can not process A news format article without OLD defined");
#else /* OLD */
	/* Read in an A news format article. */
	p = index(bfr+1, '.');
	if (p == NULL) {
		(void) strcpy(hp->ident, bfr+1);
		return NULL;
	}
	*p++ = '\0';
	(void) sprintf(hp->ident, "<%s@%s%s>", p, bfr+1, ".UUCP");

	/* Newsgroup List */
	if (hfgets(hp->nbuf, BUFLEN, fp) == NULL || !nstrip(hp->nbuf))
		return NULL;
	/* source path */
	if (hfgets(hp->path, PATHLEN, fp) == NULL || !nstrip(hp->path))
		return NULL;
	/* date */
	if (hfgets(hp->subdate, DATELEN, fp) == NULL || !nstrip(hp->subdate))
		return NULL;
	/* title */
	if (hfgets(hp->title, BUFLEN, fp) == NULL || !nstrip(hp->title))
		return NULL;
#endif /* OLD */

strip:	/* strip off sys! from front of path. */
#ifndef GENERICPATH
	if (strncmp(PATHSYSNAME, hp->path, (len = strlen(PATHSYSNAME))) == 0
		&& index(NETCHRS, hp->path[len]))
		(void) strcpy(hp->path, &(hp->path[len+1]));
#endif /* GENERICPATH */
	lcase(hp->nbuf);

	/* Intuit the From: line if only a path was given. */
	if (wholething) {
#ifdef OLD
		if (hp->from[0] == '\0')
			intuitfrom(hp);
		else
#endif /* OLD */
			fixfrom(hp);
	}

	return fp;
}


/*
 * Get header info from mail-format file.
 * Return non-zero on success.
 */
#define FROM 		1
#define NEWSGROUP 	2
#define TITLE 		3
#define SUBMIT		4
#define RECEIVE		5
#define EXPIRE		6
#define ARTICLEID	7
#define MESSAGEID	8
#define REPLYTO		9
#define FOLLOWID	10
#define CONTROL		11
#define SENDER		12
#define FOLLOWTO	13
#define PATH		14
#define POSTVERSION	15
#define RELAYVERSION	16
#define DISTRIBUTION	17
#define ORGANIZATION	18
#define NUMLINES	19
#define KEYWORDS	20
#define APPROVED	21
#define NFID		22
#define NFFROM		23
#define XREF		24
#define SUMMARY		25
#define XPATH		26
#define SUPERSEDES	27
#define OTHER		99

char *malloc();

frmread(fp, hp)
register FILE *fp;
register struct hbuf *hp;
{
	int	unreccnt = 0;
	register int	i;

	i = type(bfr);
	do {
		switch (i) {
		case PATH:
			getfield(hp->path, sizeof(hp->path));
			break;
		case FROM:
			getfield(hp->from, sizeof(hp->from));
			break;
		case NEWSGROUP:
			getfield(hp->nbuf, sizeof(hp->nbuf));
			break;
		case TITLE:
			getfield(hp->title, sizeof(hp->title));
			break;
		case SUBMIT:
			getfield(hp->subdate, sizeof(hp->subdate));
			break;
		case EXPIRE:
			getfield(hp->expdate, sizeof(hp->expdate));
			break;
#ifdef OLD
		case ARTICLEID:
			/* Believe Message-ID in preference to Article-ID */
			if (hp->ident[0] == '\0') {
				register char *p;
				char msgb[NAMELEN];
				getfield(msgb, sizeof msgb);
				p = index(msgb, '.');
				if (p == NULL) {
					(void) strcpy(hp->ident, msgb);
				} else {
					*p++ = '\0';
					(void) sprintf(hp->ident, "<%s@%s%s>", p, msgb, ".UUCP");
				}
			}
			break;
#endif /* OLD */
		case MESSAGEID:
			getfield(hp->ident, sizeof(hp->ident));
			break;
		case REPLYTO:
			getfield(hp->replyto, sizeof(hp->replyto));
			break;
		case FOLLOWID:
			getfield(hp->followid, sizeof(hp->followid));
			break;
		case SENDER:
			getfield(hp->sender, sizeof(hp->sender));
			break;
		case FOLLOWTO:
			getfield(hp->followto, sizeof(hp->followto));
			break;
		case CONTROL:
			getfield(hp->ctlmsg, sizeof(hp->ctlmsg));
			break;
		case DISTRIBUTION:
			getfield(hp->distribution, sizeof(hp->distribution));
			if (strcmp(hp->distribution, "net") == 0
				|| strcmp(hp->distribution, "world") == 0)
				hp->distribution[0] = '\0';
			break;
		case ORGANIZATION:
			getfield(hp->organization, sizeof(hp->organization));
			break;
		case NUMLINES:
			getfield(hp->numlines, sizeof(hp->numlines));
			hp->intnumlines = atoi(hp->numlines);
			break;
		case KEYWORDS:
			getfield(hp->keywords, sizeof(hp->keywords));
			break;
		case APPROVED:
			getfield(hp->approved, sizeof(hp->approved));
			break;
		case NFID:
			getfield(hp->nf_id, sizeof(hp->nf_id));
			break;
		case NFFROM:
			getfield(hp->nf_from, sizeof(hp->nf_from));
			break;
		case SUPERSEDES:
			getfield(hp->supersedes, sizeof(hp->supersedes));
			break;
		/* discard these lines */
		case XREF:
		case XPATH:
		case RELAYVERSION:
		case POSTVERSION:
		case RECEIVE:
			break;
		case SUMMARY:
			getfield(hp->summary, sizeof(hp->summary));
			break;
		case OTHER:
			if (unreccnt < NUNREC) {
				if ((hp->unrec[unreccnt] = malloc((unsigned)(strlen(bfr) + 1))) != NULL ) {
					(void) strcpy(hp->unrec[unreccnt], bfr);
					(void) nstrip(hp->unrec[unreccnt]);
					unreccnt++;
				} else
					xerror("frmread: out of memory");
			}
			break;
		}
	} while ((i = type(hfgets(bfr, LBUFLEN, fp))) > 0);

	if ((hp->from[0] || hp->path[0]) && hp->subdate[0] && hp->ident[0])
		return TRUE;
	return FALSE;
}

#ifdef OLD
/*
 * There was no From: line in the message (because it was generated by
 * an old news program).  Guess what it should have been and create it.
 */
intuitfrom(hp)
register struct hbuf *hp;
{
	char *tp;
	char *user, *host;
	char *tailpath(), *rindex();
	char *at, *dot;
	char pathbuf[PATHLEN];
	char fullname[BUFLEN];
	extern char *mydomain();

	tp = tailpath(hp);
	user = rindex(tp, '!');
	if (user == NULL)
		user = tp;
	else
		*user++ = '\0';

	/* Check for an existing Internet address on the end. */
	at = index(user, '@');
	if (at) {
		dot = index(at, '.');
		if (dot) {
			(void) strcpy(hp->from, user);
			return;
		}
		/* @ signs are illegal except for the biggie, so */
		*at = '%';
	}

	if (tp[0] == '.')
		host = index(tp, '!') + 1;
	else if (user == tp)
		host = FROMSYSNAME;
	else
		host = tp;

	tp = index(host, '@');
	if (tp != NULL)
		*tp = 0;
	if (index(host, '.') != NULL)
		(void) sprintf(hp->from, "%s@%s%s", user, host, mydomain());
	else
		(void) sprintf(hp->from, "%s@%s", user, host);

	skin(pathbuf, fullname, hp->path);	/* remove RFC822-style comments */
	if (fullname[0] != '\0') {
		strcat(hp->from, " (");
		(void) strcat(hp->from, fullname);
		strcat(hp->from, ")");
	}
	strcpy(hp->path, pathbuf);	/* and stick it back in */
}
#endif /* OLD */

/*
 * Canonicalize the "From:" line into the form
 *
 * From: <mail-address> (full-name)
 *
 * RFC822 doesn't require the comment to be at the end of the string
 * like that.
 */
fixfrom(hp)
register struct hbuf *hp;
{
	char frombuf[PATHLEN];
	char fullname[BUFLEN];

	skin(frombuf, fullname, hp->from);	/* remove RFC822-style comments */
	if (fullname[0] != '\0') {
		strcat(frombuf, " (");
		strcat(frombuf, fullname);
		strcat(frombuf, ")");
	}
	strcpy(hp->from, frombuf);	/* stick the canonicalized "from" back in */
}

skin(name, fullname, hfield)
char *name;
char *fullname;
char *hfield;
{
	register int c;
	register char *cp, *cp2;
	char *bufend;
	int gotlt, parenlev, lastsp;
	int seenfullname = FALSE;

	*fullname = '\0';	/* no full name yet */
	if (strpbrk(hfield, "(< ") == NULL) {		/* include ',' ?? */
		strcpy(name, hfield);
		return;
	}
	gotlt = 0;
	parenlev = 0;
	lastsp = 0;
	bufend = name;
	for (cp = hfield, cp2 = bufend; c = *cp++; ) {
		switch (c) {
		case '(':
			/*
			 * Start of a "comment".
			 * Ignore it, or save it in "fullname" if we haven't
			 * seen a comment yet.
			 */
			parenlev++;
			while ((c = *cp) != 0) {
				cp++;
				switch (c) {
				case '\\':
					if ((c = *cp) == 0)
						goto outcm;
					cp++;
					break;
				case '(':
					parenlev++;
					break;
				case ')':
					parenlev--;
					if (parenlev == 0)
						goto outcm;
					break;
				}
				if (!seenfullname)
					*fullname++ = c;
			}
		outcm:
			parenlev = 0;
			lastsp = 0;
			if (!seenfullname) {
				*fullname = '\0';
				seenfullname = TRUE;	/* only extract first comment */
			}
			break;

		case '"':
			/*
			 * Start of a "quoted-string".
			 * Copy it in its entirety.
			 */
			while ((c = *cp) != 0) {
				cp++;
				switch (c) {
				case '\\':
					if ((c = *cp) == 0)
						goto outqs;
					cp++;
					break;
				case '"':
					goto outqs;
				}
				*cp2++ = c;
			}
		outqs:
			lastsp = 0;
			break;

		case ' ':
			if (cp[0] == 'a' && cp[1] == 't' && cp[2] == ' ')
				cp += 3, *cp2++ = '@';
			else
			if (cp[0] == '@' && cp[1] == ' ')
				cp += 2, *cp2++ = '@';
			else
				lastsp = 1;
			break;

		case '<':
			if (!seenfullname) {
				*cp2 = '\0';
				strcpy(fullname, name);
				seenfullname = TRUE;
			}
			cp2 = bufend;
			gotlt++;
			lastsp = 0;
			break;

		case '>':
			if (gotlt) {
				gotlt = 0;
				/*
				 * this doesn't seem reasonable, what about (,)
				 * or "," ??
				 */
				while (*cp != ',' && *cp != 0)
					cp++;
				if (*cp == 0 )
					goto done;
				*cp2++ = ',';
				*cp2++ = ' ';
				bufend = cp2;
				break;
			}

			/* Fall into . . . */

		default:
			if (lastsp) {
				lastsp = 0;
				*cp2++ = ' ';
			}
			*cp2++ = c;
			break;
		}
	}
done:
	*cp2 = 0;
}


#ifdef OLD
char *
oident(ident)
char *ident;
{
	char lbuf[BUFLEN];
	static char oidbuf[BUFLEN];
	register char *p, *q;

	(void) strcpy(lbuf, ident);
	p = index(lbuf, '@');
	if (p == NULL)
		return ident;
	*p++ = '\0';
	q = index(p, '.');
	if (q == NULL)
		q = index(p, '>');
	if (q)
		*q++ = '\0';
	p[SNLN] = '\0';
	(void) sprintf(oidbuf, "%s.%s", p, lbuf+1);
	return oidbuf;
}
#endif /* OLD */

/*
 * Get the given field of a header (char * parm) from bfr, but only
 * if there's something actually there (after the colon).  Don't
 * bother if we already have an entry for this field.
 */
getfield(hpfield, size)
char	*hpfield;
int	size;
{
	register char	*ptr;

	if (hpfield[0])
		return;
	for (ptr = index(bfr, ':'); isspace(*++ptr); )
		;
	if (*ptr != '\0') {
		(void) strncpy(hpfield, ptr, size - 1);
		(void) nstrip(hpfield);
	}
}


#define its(type) (PREFIX(ptr, type))
type(ptr)
register char	*ptr;
{
	register char	*colon, *space;

	if (ptr == NULL)
		return FALSE;
	if (its("From: "))
		if (index(ptr, '@') || !index(ptr, '!'))
			return FROM;
		else
			return PATH;
	if (its("Path: "))
		return PATH;
	if (its("Newsgroups: "))
		return NEWSGROUP;
	if (its("Subject: "))
		return TITLE;
	if (its("Date: "))
		return SUBMIT;
	if (its("Date-Received: "))
		return RECEIVE;
#ifdef OLD
	if (its("Title: "))
		return TITLE;
	if (its("Posted: "))
		return SUBMIT;
#endif /* OLD */
	if (its("Received: "))
		return RECEIVE;
	if (its("Expires: "))
		return EXPIRE;
	if (its("Article-I.D.: "))
		return ARTICLEID;
	if (its("Message-ID: "))
		return MESSAGEID;
	if (its("Reply-To: "))
		return REPLYTO;
	if (its("References: "))
		return FOLLOWID;
	if (its("Control: "))
		return CONTROL;
	if (its("Sender: "))
		return SENDER;
	if (its("Followup-To: "))
		return FOLLOWTO;
	if (its("Distribution: "))
		return DISTRIBUTION;
	if (its("Organization: "))
		return ORGANIZATION;
	if (its("Lines: "))
		return NUMLINES;
	if (its("Summary: "))
		return SUMMARY;
	if (its("Keywords: "))
		return KEYWORDS;
	if (its("Approved: "))
		return APPROVED;
	if (its("Nf-ID: "))
		return NFID;
	if (its("Nf-From: "))
		return NFFROM;
	if (its("Supersedes: "))
		return SUPERSEDES;
	if (its("Xref: "))
		return XREF;
	if (its("Xpath: "))
		return XPATH;
	if (its("Posting-Version: "))
		return POSTVERSION;
	if (its("Relay-Version: "))
		return RELAYVERSION;
	if (!isalpha(*ptr))
		return FALSE;
	colon = index(ptr, ':');
	space = index(ptr, ' ');
	if (!colon || space && space < colon)
		return FALSE;
	return OTHER;
}

/*
 * Write header at 'hp' on stream 'fp' in B+ format.  Include received date
 * if wr is 1.  Leave off sysname if wr is 2.
 */
#ifndef DOXREFS
/*ARGSUSED*/
#endif /* !DOXREFS */
ihwrite(hp, fp, wr)
register struct hbuf *hp;
register FILE *fp;
int	wr;
{
	int iu;
	time_t t;
	time_t cgtdate();

	/*
	 * We're being tricky with Path/From because of upward compatibility
	 * issues.  The new version considers From and Path to be separate.
	 * The old one thinks they both mean "Path" but only believes the
	 * first one it sees, so will ignore the second.
	 */
	if (PREFIX(hp->path, PATHSYSNAME) &&
		index(NETCHRS, hp->path[strlen(PATHSYSNAME)]))
		fprintf(fp, "Path: %s\n", hp->path);
	else
		fprintf(fp, "Path: %s!%s\n", PATHSYSNAME, hp->path);
	if (hp->from[0])
		fprintf(fp, "From: %s\n", hp->from);

	fprintf(fp, "Newsgroups: %s\n", hp->nbuf);
	fprintf(fp, "Subject: %s\n", hp->title);
	if (*hp->summary)
		fprintf(fp, "Summary: %s\n", hp->summary);
	if (*hp->keywords)
		fprintf(fp, "Keywords: %s\n", hp->keywords);
	fprintf(fp, "Message-ID: %s\n", hp->ident);
	t = cgtdate(hp->subdate);
	fprintf(fp, "Date: %s\n", arpadate(&t));
#ifdef OLD
	fprintf(fp, "Article-I.D.: %s\n", oident(hp->ident));
	fprintf(fp, "Posted: %s", ctime(&t));
#endif /* OLD */
	if (*hp->expdate)
		fprintf(fp, "Expires: %s\n", hp->expdate);
	if (*hp->followid) {
		register char *dp, *cp;

		dp = cp = hp->followid;
		while (*cp != '\0')
			if (*cp == '<' && *(cp + 1) == '>')
				cp += 2;
			else
				*dp++ = *cp++;
		*dp = '\0';
		if (*hp->followid)
			fprintf(fp, "References: %s\n", hp->followid);
	}
	if (*hp->ctlmsg)
		fprintf(fp, "Control: %s\n", hp->ctlmsg);
	if (*hp->sender)
		fprintf(fp, "Sender: %s\n", hp->sender);
	if (*hp->replyto)
		fprintf(fp, "Reply-To: %s\n", hp->replyto);
	if (*hp->followto)
		fprintf(fp, "Followup-To: %s\n", hp->followto);
	if (*hp->distribution)
		fprintf(fp, "Distribution: %s\n", hp->distribution);
	if (*hp->organization)
		fprintf(fp, "Organization: %s\n", hp->organization);
	if (*hp->numlines)
		fprintf(fp, "Lines: %s\n", hp->numlines);
	if (*hp->approved)
		fprintf(fp, "Approved: %s\n", hp->approved);
	if (*hp->nf_id)
		fprintf(fp, "Nf-ID: %s\n", hp->nf_id);
	if (*hp->nf_from)
		fprintf(fp, "Nf-From: %s\n", hp->nf_from);
	if (*hp->supersedes)
		fprintf(fp, "Supersedes: %s\n", hp->supersedes);
#ifdef DOXREFS
	if ( wr ==1 && *hp->xref)
		fprintf(fp, "Xref: %s\n", hp->xref);
#endif /* DOXREFS */
	for (iu = 0; iu < NUNREC; iu++) {
		if (hp->unrec[iu])
			fprintf(fp, "%s\n", &hp->unrec[iu][0]);
	}
	putc('\n', fp);
}


#ifndef BSD4_2
/*
 * Set nc bytes, starting at cp, to zero.
 */
bzero(cp, nc)
register char	*cp;
register int	nc;
{
	if (nc > 0)
		do {
			*cp++ = 0;
		} while (--nc);
}
#endif /* !BSD4_2 */

/*
 * hfgets is like fgets, but deals with continuation lines.
 * It also ensures that even if a line that is too long is
 * received, the remainder of the line is thrown away
 * instead of treated like a second line.
 */
char *
hfgets(buf, len, fp)
char *buf;
int len;
FILE *fp;
{
	register int c;
	register int n = 0;
	register char *cp;

	cp = buf;
	while (n < len && (c = getc(fp)) != EOF) {
		if (c == '\n')
			break;
		if (isprint(c) || c == '\b' || c == ' ' || c == '\t') {
			*cp++ = c;
			n++;
		}
	}
	if (c == EOF && cp == buf)
		return NULL;
	*cp = '\0';

	if (c != '\n') {
		/* Line too long - part read didn't fit into a newline */
		while ((c = getc(fp)) != '\n' && c != EOF)
			;
	} else if (cp == buf) {
		/* Don't look for continuation of blank lines */
		*cp++ = '\n';
		*cp = '\0';
		return buf;
	}

	while ((c = getc(fp)) == ' ' || c == '\t') {	/* for each cont line */
		/* Continuation line. */
		if ((n += 2) < len) {
			*cp++ = '\n';
			*cp++ = c;
		}
		while ((c = getc(fp)) != '\n' && c != EOF)
			if ((isprint(c) || c == '\b' || c == ' ' || c == '\t')
				&& n++ < len)
				*cp++ = c;
	}
	if (n >= len - 1)
		cp = buf + len - 2;
	*cp++ = '\n';
	*cp = '\0';
	if (c != EOF)
		(void) ungetc(c, fp); /* push back first char of next header */
	return buf;
}
