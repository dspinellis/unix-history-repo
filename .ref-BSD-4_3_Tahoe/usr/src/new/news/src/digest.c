/*
 * digest - process ARPANET digests
 *
 * digest(ifile, ofile, header)
 * FILE *ifile, *ofile;
 * struct header *header;
 *
 * returns:	TRUE	EOF reached, exit from readnews.
 *		FALSE	normal exit, continue reading news.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)digest.c	1.7	9/19/86";
#endif /* SCCSID */

#include "rparams.h"

struct art {
	long	a_hdr;
	long	a_bod;
	int	a_blen;
	int	a_hlen;
};

#define	loop		for(;;)
#define	getnum(p, n)	for (n=0; *p>='0' && *p<='9'; p++) n = n*10 + *p-'0'
#define	errchk(p)	if (*p) goto badopt

#define	MAXART		128

struct art	*arts;
int		lastart;

digest(ifp, ofp, h)
FILE *ifp, *ofp;
struct hbuf *h;
{
	register int	n, curart;
	struct art	artbuf[MAXART];
	int		printh, eod, nomore;
	char		cbuf[BUFLEN], *cmd;

	arts = artbuf;
	printh = TRUE;
	nomore = eod = FALSE;
	curart = 1;

	if (dscan(ifp))
		return FALSE;

	dprint(0, ifp, ofp);

	loop {
		if (nomore) break;
		if (curart < 1) {
			curart = 1;
			eod = nomore = FALSE;
		}
		if (curart > lastart) curart = lastart;
		if (eod) nomore = TRUE;
		if (printh && !nomore)
			(void) dhprint(curart, ifp, ofp);
	getcmd:
		loop {
			SigTrap = FALSE;
			fprintf(ofp, "Digest article %d of %d ", curart, lastart);
			if (curart==lastart && nomore)
				fprintf(ofp, "Last digest article ");
			fprintf(ofp, "(%d lines) More? [%s] ",
				arts[curart].a_blen, nomore?"snq":"ynq");
			(void) fflush(ofp);
			cmd = cbuf;
			if (fgets(cmd, BUFLEN, stdin))
				break;
			if (!SigTrap)
				return(TRUE);
			putc('\n', ofp);
		}
		(void) nstrip(cmd);
		while (*cmd==' ' || *cmd=='\t')
			cmd++;
		printh = TRUE;

		switch (*cmd++) {
		case '#':
			fprintf(ofp, "%d articles in digest\n", lastart);
			(void) fflush(ofp);
			printh = FALSE;
			break;

		case '$':
			curart = lastart;
			break;

		case '!':
			fwait(fsubr(ushell, cmd, (char *)NULL));
			fprintf(ofp, "!\n");
			printh = FALSE;
			break;

		case '\0':
			if (nomore) {
				putc('\n', ofp);
				return(FALSE);
			}
			cmd--;
		case 'y':
		case 'p':
			errchk(cmd);
			dprint(curart++, ifp, ofp);
			if (curart > lastart)
				eod = TRUE;
			break;

		case 'n':
			errchk(cmd);
			if (++curart > lastart) {
				putc('\n', ofp);
				return(FALSE);
			}
			break;

		case '+':
			getnum(cmd, n);
			errchk(cmd);
			if (nomore) {
				putc('\n', ofp);
				return(FALSE);
			}
			if (n)	curart += n;
			else {
				curart += 1;
				if (curart > lastart)
					eod = TRUE;
			}
			break;

		case '-':
			getnum(cmd, n);
			errchk(cmd);
			eod = nomore = FALSE;
			curart -= (n) ? n : 1;
			break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			cmd--;
			getnum(cmd, n);
			errchk(cmd);
			curart = n;
			eod = nomore = FALSE;
			break;

		case 'q':
		case 'x':
			putc('\n', ofp);
			return(FALSE);

		case '?':
			fprintf(ofp, "\nDigester options:\n\n");
			fprintf(ofp, "y\tyes, print article.\n");
			fprintf(ofp, "n\tno, go to next article.\n");
			fprintf(ofp, "q\texit from digester.\n");
			fprintf(ofp, "h\tprint article header.\n");
			fprintf(ofp, "s file\tsave article in file.\n");
			fprintf(ofp, "t\ttable of contents.\n");
			fprintf(ofp, "+[n]\tforward n articles (1).\n");
			fprintf(ofp, "-[n]\tback n articles (1).\n");
			fprintf(ofp, "\nh and s may be followed by '-'\n");
			(void) fflush(ofp);
			break;

		case 'h':
			n = curart;
			if (*cmd=='-') {
				cmd++;
				if (n > 1) n--;
			}
			errchk(cmd);
			(void) dhprint(n, ifp, ofp);
			nomore = printh = FALSE;
			if (n!=curart)
				putc('\n', ofp);
			break;

		case 's':
		case 'w':
			n = curart;
			if (*cmd=='-') {
				cmd++;
				if (n > 1) n--;
			}
			while (*cmd==' ' || *cmd=='\t')
				cmd++;
			dsaveart(n, ifp, ofp, cmd);
			nomore = printh = FALSE;
			if (n!=curart)
				putc('\n', ofp);
			break;

		case 'H':
			errchk(cmd);
			hprint(h, ofp, 1);
			eod = nomore = FALSE;
			break;

		case 'T':
		case 't':
			errchk(cmd);
			if (cmd[-1]=='T')
				hprint(h, ofp, 0);
			dprint(0, ifp, ofp);
			eod = nomore = FALSE;
			break;

		default:
	badopt:
			if (!nomore)
				fprintf(ofp, "y (yes), n (no), ");
			fprintf(ofp, "q (quit), s file (save), h (header), t (table of contents)\n");
			fprintf(ofp, "? for help\n");
			goto getcmd;
		}
	}
	putc('\n', ofp);
	return(FALSE);
}

dscan(ifp)
register FILE *ifp;
{
	char		scanbuf[BUFLEN];
	register int	n, len;
	register char	*s;
	register long	pos;
	short		wasblank, ishead;

	n = len = 0;
	wasblank = FALSE;
	s = scanbuf;
	arts[0].a_bod = arts[1].a_hdr = ftell(ifp);
	arts[0].a_hdr = 0L;
	arts[1].a_bod = -1L;

	loop {
		if (SigTrap)
			return(TRUE);
		pos = ftell(ifp);
		if (fgets(s, BUFLEN, ifp)==NULL)
			*s = '\0';
		if (wasblank && isheader(s)) {
			long lastpos;
			short is_blank;
			short nhlines;
			arts[n++].a_blen = len;
			len = 0;
			nhlines = 0;
			arts[n].a_hdr = pos;
			is_blank = FALSE;
			ishead = TRUE;
			do {
				lastpos = pos;
				wasblank = is_blank;
				nhlines++;
				pos = ftell(ifp);
				if (fgets(s, BUFLEN, ifp)==NULL)
					*s = '\0';
				else
					len++;
				is_blank = (*s=='\n') ? TRUE : FALSE;
				if (is_blank && nhlines==1)
					/* one liner--not a header */
					break;
				if (!ishead || (s[0] != ' ' && s[0] != '\t'))
					ishead = isheader(s);
			} while ((is_blank && !wasblank) || ishead);
			if ((!is_blank && !wasblank) || nhlines < 2) {
				/* oops! not a header... back off */
				arts[n].a_hdr = arts[n-1].a_bod;
				len += arts[--n].a_blen;
			} else {
				if (wasblank)
					pos = lastpos;
				arts[n].a_hlen = len;
				arts[n].a_bod = arts[n+1].a_hdr = pos;
				arts[n+1].a_bod = -1L;
				arts[n+1].a_hlen = 3;	/* average header len */
				len = 0;
			}
		}
		if (*s=='\0')
			break;
		wasblank = (*s=='\n') ? TRUE : FALSE;
		len++;
	}
	arts[n].a_blen = len;
	arts[n+1].a_hdr = pos;
	lastart = n;
	return FALSE;
}

dhprint(art, ifp, ofp)
register int art;
register FILE *ifp, *ofp;
{
	register char	c;
	register long	pos = arts[art].a_hdr;
	register long	epos = arts[art].a_bod;
	register int	nlines = 1;

	putc('\n', ofp);
	fseek(ifp, pos, 0);
	while (pos++ < epos && !SigTrap) {
		if ((c = getc(ifp))=='\n')
			nlines++;
		putc(c, ofp);
	}
	(void) fflush(ofp);
	SigTrap = FALSE;
	return nlines;
}

dprint(art, ifp, ofp)
int art;
FILE *ifp, *ofp;
{
#ifdef	PAGE
	register int	cnt;
	FILE		*pfp, *popen();

	if (art && arts[art].a_blen > 23-arts[art+1].a_hlen && *PAGER) {
		if (!index(PAGER, FMETA)) {
			if ((pfp = popen(PAGER, "w"))==NULL)
				(void) dprinta(art, ifp, ofp);
			else {
				cnt = dprinta(art, ifp, pfp) % 23;
				if (cnt > 23-arts[art+1].a_hlen)
					while (cnt++ < 24)
						putc('\n', pfp);
				(void) pclose(pfp);
			}
		} else
			pout(ofp);
	} else
#endif /* PAGE */
		(void) dprinta(art, ifp, ofp);
}

dprinta(art, ifp, ofp)
int art;
register FILE *ifp, *ofp;
{
	register char	c;
	register long	pos = arts[art].a_bod;
	register long	epos = arts[art+1].a_hdr;
	register int	nlines = 0;

	(void) fseek(ifp, pos, 0);
	while (pos++ < epos && !SigTrap) {
		if ((c = getc(ifp))=='\n')
			nlines++;
		putc(c, ofp);
	}
	(void) fflush(ofp);
	SigTrap = FALSE;
	return nlines;
}

dsaveart(art, ifp, ofp, name)
int art;
register FILE *ifp, *ofp;
register char *name;
{
	register FILE	*nfp;
	char		fname[BUFLEN];
	char		*strcat(), *strcpy(), *getenv();
	register char	*nb;

	while (*name==' ' || *name=='\t')
		name++;

	if (*name=='|') {
		fprintf(ofp, "don't know how to pipe yet.\n");
		(void) fflush(ofp);
		return;
	} else if (*name=='/')
		(void) strcpy(fname, name);
	else {
		if (nb = getenv("NEWSBOX"))
			(void) strcpy(fname, nb);
		else
			(void) strcpy(fname, userhome);
		(void) strcat(fname, "/");
		(void) strcat(fname, name);
	}

	fprintf(ofp, "Save digest article %d in \"%s\"", art, fname);
	(void) fflush(ofp);
	if ((nfp = fopen(fname, "a"))!=NULL) {
		int ln;
		ln = dhprint(art, ifp, nfp);
		ln += dprinta(art, ifp, nfp);
		fprintf(ofp, " [Appended] %d lines\n", ln);
		(void) fclose(nfp);
	} else
		fprintf(ofp, " cannot append to.\n");
}

isheader(s)
register char *s;
{
	if (isupper(*s) || islower(*s)) {
		while (*s && *s!=':' && !isspace(*s))
			s++;
		if (*s==':' && *++s==' ')
			return TRUE;
	}
	return FALSE;
}
