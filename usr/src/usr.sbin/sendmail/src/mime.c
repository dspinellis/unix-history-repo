/*
 * Copyright (c) 1994 Eric P. Allman
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

# include "sendmail.h"
# include <string.h>

#ifndef lint
static char sccsid[] = "@(#)mime.c	8.10 (Berkeley) %G%";
#endif /* not lint */

/*
**  MIME support.
**
**	I am indebted to John Beck of Hewlett-Packard, who contributed
**	his code to me for inclusion.  As it turns out, I did not use
**	his code since he used a "minimum change" approach that used
**	several temp files, and I wanted a "minimum impact" approach
**	that would avoid copying.  However, looking over his code
**	helped me cement my understanding of the problem.
**
**	I also looked at, but did not directly use, Nathaniel
**	Borenstein's "code.c" module.  Again, it functioned as
**	a file-to-file translator, which did not fit within my
**	design bounds, but it was a useful base for understanding
**	the problem.
*/


/* character set for hex and base64 encoding */
char	Base16Code[] =	"0123456789ABCDEF";
char	Base64Code[] =	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/* types of MIME boundaries */
#define MBT_SYNTAX	0	/* syntax error */
#define MBT_NOTSEP	1	/* not a boundary */
#define MBT_INTERMED	2	/* intermediate boundary (no trailing --) */
#define MBT_FINAL	3	/* final boundary (trailing -- included) */

static int	MimeBoundaryType;	/* internal linkage */
/*
**  MIME8TO7 -- output 8 bit body in 7 bit format
**
**	The header has already been output -- this has to do the
**	8 to 7 bit conversion.  It would be easy if we didn't have
**	to deal with nested formats (multipart/xxx and message/rfc822).
**
**	We won't be called if we don't have to do a conversion, and
**	appropriate MIME-Version: and Content-Type: fields have been
**	output.  Any Content-Transfer-Encoding: field has not been
**	output, and we can add it here.
**
**	Parameters:
**		mci -- mailer connection information.
**		header -- the header for this body part.
**		e -- envelope.
**		boundary -- the message boundary -- NULL if we are
**			processing the outer portion.
**
**	Returns:
**		An indicator of what terminated the message part:
**		  MBT_FINAL -- the final boundary
**		  MBT_INTERMED -- an intermediate boundary
**		  MBT_NOTSEP -- an end of file
*/

int
mime8to7(mci, header, e, boundary)
	register MCI *mci;
	HDR *header; register ENVELOPE *e;
	char *boundary;
{
	register char *p;
	int linelen;
	int bt;
	off_t offset;
	size_t sectionsize, sectionhighbits;
	char bbuf[128];
	char buf[MAXLINE];

	if (tTd(43, 1))
	{
		printf("mime8to7: boundary=%s\n",
			boundary == NULL ? "<none>" : boundary);
	}
	p = hvalue("Content-Type", header);
	if (p != NULL && strncasecmp(p, "multipart/", 10) == 0)
	{
		register char *q;

		/* oh dear -- this part is hard */
		p = strstr(p, "boundary=");		/*XXX*/
		if (p == NULL)
		{
			syserr("mime8to7: Content-Type: %s missing boundary", p);
			p = "---";
		}
		else
			p += 9;
		if (*p == '"')
			q = strchr(p, '"');
		else
			q = strchr(p, ',');
		if (q == NULL)
			q = p + strlen(p);
		if (q - p > sizeof bbuf - 1)
		{
			syserr("mime8to7: multipart boundary \"%.*s\" too long",
				q - p, p);
			q = p + sizeof bbuf - 1;
		}
		strncpy(bbuf, p, q - p);
		bbuf[q - p] = '\0';
		if (tTd(43, 1))
		{
			printf("mime8to7: multipart boundary \"%s\"\n", bbuf);
		}

		/* skip the early "comment" prologue */
		bt = MBT_FINAL;
		while (fgets(buf, sizeof buf, e->e_dfp) != NULL)
		{
			bt = mimeboundary(buf, bbuf);
			if (bt != MBT_NOTSEP)
				break;
			putline(buf, mci);
		}
		while (bt != MBT_FINAL)
		{
			auto HDR *hdr = NULL;

			sprintf(buf, "--%s", bbuf);
			putline(buf, mci);
			collect(e->e_dfp, FALSE, FALSE, &hdr, e);
			putheader(mci, hdr, e, 0);
			bt = mime8to7(mci, hdr, e, bbuf);
		}
		sprintf(buf, "--%s--", bbuf);
		putline(buf, mci);

		/* skip the late "comment" epilogue */
		while (fgets(buf, sizeof buf, e->e_dfp) != NULL)
		{
			putline(buf, mci);
			bt = mimeboundary(buf, boundary);
			if (bt != MBT_NOTSEP)
				break;
		}
		return bt;
	}

	/*
	**  Non-compound body type
	**
	**	Compute the ratio of seven to eight bit characters;
	**	use that as a heuristic to decide how to do the
	**	encoding.
	*/

	sectionsize = sectionhighbits = 0;
	if (p == NULL || strcasecmp(p, "message/rfc822") != 0)
	{
		/* remember where we were */
		offset = ftell(e->e_dfp);
		if (offset == -1)
			syserr("mime8to7: cannot ftell on %s", e->e_df);

		/* do a scan of this body type to count character types */
		while (fgets(buf, sizeof buf, e->e_dfp) != NULL)
		{
			bt = mimeboundary(buf, boundary);
			if (bt != MBT_NOTSEP)
				break;
			for (p = buf; *p != '\0'; p++)
			{
				/* count bytes with the high bit set */
				sectionsize++;
				if (bitset(0200, *p))
					sectionhighbits++;
			}

			/*
			**  Heuristic: if 1/4 of the first 4K bytes are 8-bit,
			**  assume base64.  This heuristic avoids double-reading
			**  large graphics or video files.
			*/

			if (sectionsize >= 4096 &&
			    sectionhighbits > sectionsize / 4)
				break;
		}
		if (feof(e->e_dfp))
			bt = MBT_FINAL;

		/* return to the original offset for processing */
		/* XXX use relative seeks to handle >31 bit file sizes? */
		if (fseek(e->e_dfp, offset, SEEK_SET) < 0)
			syserr("mime8to7: cannot fseek on %s", e->e_df);
	}

	/*
	**  Heuristically determine encoding method.
	**	If more than 1/8 of the total characters have the
	**	eighth bit set, use base64; else use quoted-printable.
	*/

	if (tTd(43, 8))
	{
		printf("mime8to7: %ld high bits in %ld bytes\n",
			sectionhighbits, sectionsize);
	}
	if (sectionhighbits == 0)
	{
		/* no encoding necessary */
		p = hvalue("content-transfer-encoding", header);
		if (p != NULL)
		{
			sprintf(buf, "Content-Transfer-Encoding: %s", p);
			putline(buf, mci);
		}
		putline("", mci);
		mci->mci_flags &= ~MCIF_INHEADER;
		while (fgets(buf, sizeof buf, e->e_dfp) != NULL)
		{
			bt = mimeboundary(buf, boundary);
			if (bt != MBT_NOTSEP)
				break;
			if (buf[0] == 'F' &&
			    bitnset(M_ESCFROM, mci->mci_mailer->m_flags) &&
			    strncmp(buf, "From ", 5) == 0)
				(void) putc('>', mci->mci_out);
			putline(buf, mci);
		}
	}
	else if (sectionsize / 8 < sectionhighbits)
	{
		/* use base64 encoding */
		int c1, c2;

		putline("Content-Transfer-Encoding: base64", mci);
		putline("", mci);
		mci->mci_flags &= ~MCIF_INHEADER;
		linelen = 0;
		while ((c1 = mime_getchar(e->e_dfp, boundary)) != EOF)
		{
			if (linelen > 71)
			{
				fputs(mci->mci_mailer->m_eol, mci->mci_out);
				linelen = 0;
			}
			linelen += 4;
			fputc(Base64Code[c1 >> 2], mci->mci_out);
			c1 = (c1 & 0x03) << 4;
			c2 = mime_getchar(e->e_dfp, boundary);
			if (c2 == EOF)
			{
				fputc(Base64Code[c1], mci->mci_out);
				fputc('=', mci->mci_out);
				fputc('=', mci->mci_out);
				break;
			}
			c1 |= (c2 >> 4) & 0x0f;
			fputc(Base64Code[c1], mci->mci_out);
			c1 = (c2 & 0x0f) << 2;
			c2 = mime_getchar(e->e_dfp, boundary);
			if (c2 == EOF)
			{
				fputc(Base64Code[c1], mci->mci_out);
				fputc('=', mci->mci_out);
				break;
			}
			c1 |= (c2 >> 6) & 0x03;
			fputc(Base64Code[c1], mci->mci_out);
			fputc(Base64Code[c2 & 0x3f], mci->mci_out);
		}
	}
	else
	{
		/* use quoted-printable encoding */
		int c1, c2;
		int fromstate;

		putline("Content-Transfer-Encoding: quoted-printable", mci);
		putline("", mci);
		mci->mci_flags &= ~MCIF_INHEADER;
		linelen = fromstate = 0;
		c2 = '\n';
		while ((c1 = mime_getchar(e->e_dfp, boundary)) != EOF)
		{
			if (c1 == '\n')
			{
				if (c2 == ' ' || c2 == '\t')
				{
					fputc('=', mci->mci_out);
					fputc(Base16Code[(c2 >> 4) & 0x0f],
								mci->mci_out);
					fputc(Base16Code[c2 & 0x0f],
								mci->mci_out);
					fputs(mci->mci_mailer->m_eol,
								mci->mci_out);
				}
				fputs(mci->mci_mailer->m_eol, mci->mci_out);
				linelen = fromstate = 0;
				c2 = c1;
				continue;
			}
			if (c2 == ' ' && linelen == 4 && fromstate == 4 &&
			    bitnset(M_ESCFROM, mci->mci_mailer->m_flags))
			{
				fputs("=20", mci->mci_out);
				linelen += 3;
			}
			else if (c2 == ' ' || c2 == '\t')
			{
				fputc(c2, mci->mci_out);
				linelen++;
			}
			if (linelen > 72)
			{
				fputc('=', mci->mci_out);
				fputs(mci->mci_mailer->m_eol, mci->mci_out);
				linelen = fromstate = 0;
				c2 = '\n';
			}
			if (c2 == '\n' && c1 == '.' &&
				 bitnset(M_XDOT, mci->mci_mailer->m_flags))
			{
				fputc('.', mci->mci_out);
				linelen++;
			}
			if ((c1 < 0x20 && c1 != '\t') || c1 >= 0x7f || c1 == '=')
			{
				fputc('=', mci->mci_out);
				fputc(Base16Code[(c1 >> 4) & 0x0f], mci->mci_out);
				fputc(Base16Code[c1 & 0x0f], mci->mci_out);
				linelen += 3;
			}
			else if (c1 != ' ' && c1 != '\t')
			{
				if (linelen < 4 && c1 == "From"[linelen])
					fromstate++;
				fputc(c1, mci->mci_out);
				linelen++;
			}
			c2 = c1;
		}

		/* output any saved character */
		if (c2 == ' ' || c2 == '\t')
		{
			fputc('=', mci->mci_out);
			fputc(Base16Code[(c2 >> 4) & 0x0f], mci->mci_out);
			fputc(Base16Code[c2 & 0x0f], mci->mci_out);
			linelen += 3;
		}
	}
	if (linelen > 0)
		fputs(mci->mci_mailer->m_eol, mci->mci_out);
	return MimeBoundaryType;
}
/*
**  MIME_GETCHAR -- get a character for MIME processing
**
**	Treats boundaries as EOF.
**
**	Parameters:
**		fp -- the input file.
**		boundary -- the current MIME boundary.
**
**	Returns:
**		The next character in the input stream.
*/

int
mime_getchar(fp, boundary)
	register FILE *fp;
	char *boundary;
{
	int c;
	static char *bp = NULL;
	static int buflen = 0;
	static bool atbol = TRUE;	/* at beginning of line */
	static char buf[128];		/* need not be a full line */

	if (buflen > 0)
	{
		buflen--;
		return *bp++;
	}
	bp = buf;
	buflen = 0;
	c = fgetc(fp);
	if (c == '\n')
	{
		/* might be part of a MIME boundary */
		*bp++ = c;
		atbol = TRUE;
		c = fgetc(fp);
	}
	if (c != EOF)
		*bp++ = c;
	if (atbol && c == '-' && boundary != NULL)
	{
		/* check for a message boundary */
		c = fgetc(fp);
		if (c != '-')
		{
			if (c != EOF)
				*bp++ = c;
			buflen = bp - buf - 1;
			bp = buf;
			return *bp++;
		}

		/* got "--", now check for rest of separator */
		*bp++ = '-';
		while (bp < &buf[sizeof buf - 1] &&
		       (c = fgetc(fp)) != EOF && c != '\n')
		{
			*bp++ = c;
		}
		*bp = '\0';
		MimeBoundaryType = mimeboundary(buf, boundary);
		switch (MimeBoundaryType)
		{
		  case MBT_FINAL:
		  case MBT_INTERMED:
			/* we have a message boundary */
			buflen = 0;
			return EOF;
		}

		atbol = c == '\n';
		if (c != EOF)
			*bp++ = c;
	}

	buflen = bp - buf - 1;
	if (buflen < 0)
		return EOF;
	bp = buf;
	return *bp++;
}
/*
**  MIMEBOUNDARY -- determine if this line is a MIME boundary & its type
**
**	Parameters:
**		line -- the input line.
**		boundary -- the expected boundary.
**
**	Returns:
**		MBT_NOTSEP -- if this is not a separator line
**		MBT_INTERMED -- if this is an intermediate separator
**		MBT_FINAL -- if this is a final boundary
**		MBT_SYNTAX -- if this is a boundary for the wrong
**			enclosure -- i.e., a syntax error.
*/

int
mimeboundary(line, boundary)
	register char *line;
	char *boundary;
{
	int type;
	int i;

	if (line[0] != '-' || line[1] != '-' || boundary == NULL)
		return MBT_NOTSEP;
	if (tTd(43, 5))
		printf("mimeboundary: bound=\"%s\", line=\"%s\"... ",
			boundary, line);
	i = strlen(line);
	if (line[i - 1] == '\n')
		i--;
	while (line[i - 1] == ' ' || line[i - 1] == '\t')
		i--;
	if (i > 2 && strncmp(&line[i - 2], "--", 2) == 0)
	{
		type = MBT_FINAL;
		i -= 2;
	}
	else
		type = MBT_INTERMED;

	/* XXX should check for improper nesting here */
	if (strncmp(boundary, &line[2], i - 2) != 0 ||
	    strlen(boundary) != i - 2)
		type = MBT_NOTSEP;
	if (tTd(43, 5))
		printf("%d\n", type);
	return type;
}
/*
**  DEFCHARSET -- return default character set for message
**
**	The first choice for character set is for the mailer
**	corresponding to the envelope sender.  If neither that
**	nor the global configuration file has a default character
**	set defined, return "unknown-8bit" as recommended by
**	RFC 1428 section 3.
**
**	Parameters:
**		e -- the envelope for this message.
**
**	Returns:
**		The default character set for that mailer.
*/

char *
defcharset(e)
	register ENVELOPE *e;
{
	if (e != NULL && e->e_from.q_mailer != NULL &&
	    e->e_from.q_mailer->m_defcharset != NULL)
		return e->e_from.q_mailer->m_defcharset;
	if (DefaultCharSet != NULL)
		return DefaultCharSet;
	return "unknown-8bit";
}
