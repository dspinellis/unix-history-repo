/*
 * rfuncs - functions for readnews.
 */

static char	*SccsId = "@(#)rfuncs.c	2.9	3/7/83";

#include "rparams.h"

long nngsize;	/* The next upcoming value of ngsize. */

nextng() 
{
	long	curpos;
#ifdef DEBUG
	fprintf(stderr, "nextng()\n");
#endif
	curpos = ftell(actfp);

next:
#ifdef DEBUG
	fprintf(stderr, "next:\n");
#endif
	if (actdirect == BACKWARD) {
		if (back()) {
			fseek(actfp, curpos, 0);
			return 1;
		}
		if (back()) {
			fseek(actfp, curpos, 0);
			return 1;
		}
	}
	if (fgets(afline, BUFLEN, actfp) == NULL)
		return 1;
	sscanf(afline, "%s %ld", bfr, &nngsize);
#ifdef DEBUG
	fprintf(stderr, "bfr = '%s'\n", bfr);
#endif

	ngcat(bfr);
	if (!ngmatch(bfr, header.nbuf))
		goto next;
	ngdel(bfr);
	if (xflag)
		readmode = SPEC;
	else
		readmode = NEXT;
	if (selectng(bfr))
		goto next;
	return 0;
}


selectng(name)
char	*name;
{
	register char	*ptr, punct = ',';
	register int	cur = 0, i;
	register char	*p;
	int	next = 0;
	char	oldptr;
	long	findngsize();

	if (*groupdir)
		updaterc();
	last = 1;
	if (strcmp(name, bfr))
		ngsize = findngsize(name);
	else
		ngsize = nngsize;
#ifdef DEBUG
	fprintf(stderr, "selectng(%s) sets ngsize to %ld\n", name, ngsize);
#endif
	strcpy(groupdir, name);
	if (!xflag) {
		i = findrcline(name);
		if (i >= 0) {
			if (index(rcline[i], '!')) {
				groupdir[0] = 0;
				return 1;
			}
			sprintf(rcbuf, "%s,%ld", rcline[i], ngsize+1);
		}
		else
			sprintf(rcbuf, "ng: %ld", ngsize+1);
	}

	/*
	 * Fast check for common case: 1-###
	 */
	p = rcbuf;
	while (*p != ' ')
		p++;
	while (*p == ' ')
		p++;
	if (*p++ == '1' && *p++ == '-') {
		i = 0;
		while (isdigit(*p))
			i = 10 * i + *p++ - '0';
		if (*p == ',' && i >= ngsize) {
			groupdir[0] = 0;
			return 1;
		}
	}

/*
 * The key to understanding this piece of code is that a bit is set iff
 * that article has NOT been read.  Thus, we fill in the holes when
 * commas are found (e.g. 1-20,30-35 will result in filling in the 21-29
 * hold), and so we assume the newsrc file is properly ordered, the way
 * we write it out. 
 */
	cur = 0;
	/* Zero out the bitmap */
	p = &bitmap[ngsize/8+1];
	for (ptr = bitmap; ptr <= p; ptr)
		*ptr++ = 0;

	/* Decode the .newsrc line indicating what we have read. */
	for (ptr = rcbuf; *ptr && *ptr != ':'; ptr++)
		;
	while (*ptr) {
		while (!isdigit(*ptr) && *ptr)
			ptr++;
		if (!*ptr)
			break;
		sscanf(ptr, "%d", &next);
		if (punct == ',') {
			while (++cur < next) {
				set(cur);
			}
		}
		cur = next;
		while (!ispunct(*ptr) && *ptr)
			ptr++;
		punct = *ptr;
	}
	if (rflag)
		bit = ngsize+1;
	else
		bit = 0;
	nextbit();
	ngrp = 1;
	return 0;
}

/*
 * Figure out the number of the largest article in newsgroup ng,
 * and return that value.
 */
long
findngsize(ng)
char *ng;
{
	FILE *af;
	long s;
	char buf[100], n[100];

	af = xfopen(ACTIVE, "r");
	while (fgets(buf, sizeof buf, af)) {
		sscanf(buf, "%s %ld", n, &s);
		if (strcmp(n, ng) == 0) {
			fclose(af);
			return s;
		}
	}
	return 0;
}

#ifdef TMAIL
catchterm()
{
	unlink(infile);
	unlink(outfile);
	xxit(0);
}


/*
 * The -M (Mail) interface.  This code is a reasonably simple model for
 * writing other interfaces.  We write out all relavent articles to
 * a temp file, then invoke Mail with an option to have it tell us which
 * articles it read.  Finally we count those articles as really read.
 */
Mail()
{
	register FILE *fp = NULL, *ofp;
	struct hbuf h;
	register char	*ptr, *fname;
	int	news = 0;

	ofp = xfopen(mktemp(outfile), "w");
	if (aflag && *datebuf)
		if ((atime = cgtdate(datebuf)) == -1)
			xerror("Cannot parse date string");
	while (!nextng())
		while (bit <= ngsize) {
			sprintf(filename, "%s/%d", dirname(groupdir), bit);
			if (access(filename, 4)
			|| ((fp = fopen(filename, "r")) == NULL)
			|| (hread(&h, fp, TRUE) == NULL)
			|| !select(&h, FALSE)) {
#ifdef DEBUG
				fprintf(stderr, "Bad article '%s'\n", filename);
#endif
				if (fp != NULL) {
					fclose(fp);
					fp = NULL;
				}
				clear(bit);
				nextbit();
				continue;
			}
			fname = ptr = index(h.from, '(');
			if (fname) {
				while (ptr && ptr[-1] == ' ')
					ptr--;
				if (ptr)
					*ptr = 0;
				fname++;
				ptr = fname + strlen(fname) - 1;
				if (*ptr == ')')
					*ptr = 0;
			}
			h.subtime = cgtdate(h.subdate);
			fprintf(ofp, "From %s %s",
#ifdef INTERNET
			    h.from[0] ? h.from :
#endif
			    h.path, ctime(&h.subtime));
			if (fname)
				fprintf(ofp, "Full-Name: %s\n", fname);
			fprintf(ofp, "Newsgroups: %s\n", h.nbuf);
			fprintf(ofp, "Subject: %s\n", h.title);
			fprintf(ofp, "Article-ID: %s/%d\n\n", groupdir, bit);
			tprint(fp, ofp, TRUE);
			putc('\n', ofp);
			news = TRUE;
			fclose(fp);
			fp = NULL;
			nextbit();
		}
	updaterc();
	fclose(ofp);
	if (!news) {
		fprintf(stderr, "No news.\n");
		unlink(outfile);
		return;
	}
	signal(SIGHUP, catchterm);
	signal(SIGTERM, catchterm);
	sprintf(bfr, "%s -f %s -T %s", TMAIL, outfile, mktemp(infile));
	fwait(fsubr(ushell, bfr, (char *)NULL));
	ofp = xfopen(infile, "r");
	fseek(actfp, 0L, 0);
	while (fgets(afline, BUFLEN, actfp) != NULL) {
		last = 0;
		sscanf(afline, "%s %ld", bfr, &nngsize);
		ngcat(bfr);
		if (!ngmatch(bfr, header.nbuf))
			continue;
		ngdel(bfr);
		*groupdir = 0;
		if (selectng(bfr))
			continue;
		fseek(ofp, 0L, 0);
		while (fgets(groupdir, BUFLEN, ofp) != NULL) {
			nstrip(groupdir);
			ptr = index(groupdir, '/');
			*ptr = 0;
			if (strcmp(bfr, groupdir))
				continue;
			sscanf(++ptr, "%d", &last);
			clear(last);
		}
		if (last) {
			strcpy(groupdir, bfr);
			updaterc();
		}
	}
	unlink(infile);
	unlink(outfile);
}
#endif

updaterc()
{
	register int	cur = 1, next = 1, i;
	register char	*ptr;
	char	oldptr;

	sprintf(rcbuf, "%s%c ", groupdir, zapng ? '!' : ':');

	zapng = FALSE;
again:
	ptr = &rcbuf[strlen(rcbuf)];
	while (get(next))
		next++;
	cur = next;
	while (!(get(next)) && next <= ngsize)
		next++;
	if (cur == next) {
		next = 8193;
		goto skip;
	}
	if (cur + 1 == next)
		sprintf(ptr, "%d,", cur);
	else
		sprintf(ptr, "%d-%d,", cur, next - 1);
skip:
	if ((long) next > ngsize) {
		if (index(rcbuf, ',') != NULL)
			ngdel(rcbuf);
		else if (index(rcbuf, '!') == NULL)
			return;
		ptr = index(rcbuf, ' ');
		ptr--;
		oldptr = *ptr;
		ptr[0] = ':';
		ptr[1] = '\0';
		i = findrcline(groupdir);
		if (i >= 0) {
			ptr[0] = oldptr;
			ptr[1] = ' ';
			rcline[i] = realloc(rcline[i], strlen(rcbuf) + 1);
			if (rcline[i] == NULL)
				xerror("Cannot realloc");
			strcpy(rcline[i], rcbuf);
			return;
		}
		if (++line > LINES)
			xerror("Too many newsgroups\n");
		ptr[0] = oldptr;
		ptr[1] = ' ';
		if ((rcline[line] = malloc(strlen(rcbuf) + 1)) == NULL)
			xerror("Not enough memory");
		strcpy(rcline[line], rcbuf);
		return;
	}
	cur = next;
	goto again;
}


newrc(rcname)
{
	register FILE *fp;

	if (close(creat(rcname, 0666))) {
		sprintf(bfr, "Cannot create %s", newsrc);
		xerror(bfr);
	}

	if ((fp = fopen(USERS, "a")) != NULL) {
		fprintf(fp, "%s\n", username);
		fclose(fp);
		chmod(USERS, 0666);
	}
}


xerror(message)
char	*message;
{
	fflush(stdout);
	fprintf(stderr, "readnews: %s.\n", message);
	xxit(1);
}


nextbit() 
{
	last = bit;
	if (readmode == SPEC || xflag) {
		if (rflag)
			bit--;
		else
			bit++;
		return;
	}
	if (rflag)
		while (--bit, !get(bit) && bit > 0)
			;
	else
		while (++bit, !get(bit) && bit <= ngsize)
			;
}


xxit(status)
int	status;
{
	unlink(infile);
	unlink(outfile);
	exit(status);
}


/*
 * Return TRUE if the user has not ruled out this article.
 */
select(hp, insist)
register struct hbuf *hp;
int	insist;
{
	if (insist)
		return TRUE;
	if (tflag && !titmat(hp, header.title))
		return FALSE;
	if (aflag && cgtdate(hp->recdate) < atime)
		return FALSE;
	if (index(hp->nbuf, ',') && seenbefore(hp->ident))
		return FALSE;
	if (fflag && isfol(hp))
		return FALSE;
	return TRUE;
}


/*
 * Return TRUE if this article is a followup to something.
 */
isfol(hp)
register struct hbuf *hp;
{
	if (hp->followid[0])
		return TRUE;
	if (strncmp(hp->title, "Re:", 3) == 0)
		return TRUE;
	return FALSE;
}


/*
 * Given an article ID, return TRUE if we have already seen that article ID
 * in this readnews session.  This should only be called for articles
 * with commas in the newsgroup name, and prevents the same article, which
 * was submitted to multiple newsgroups, from being shown to the same
 * person more than once.  Bug: if the user quits after seeing the first
 * copy, he'll see it again next time in the other newsgroup.
 */
#define NART	100	/* max # articles on multiple newsgroups */
static int	nbef = 0;
static char	*histbuf[NART];
static char	nextabuf[BUFLEN];
seenbefore(artid)
char	*artid;
{
	register int	i;

	for (i = 0; i < nbef; i++)
		if (strcmp(histbuf[i], artid) == 0)
			return TRUE;
	if (nbef >= NART - 1) {
		return FALSE;
	}
	/* Remember the name, but don't record it as saved yet. */
	strcpy(nextabuf, artid);
	return FALSE;
}

/*
 * The current article has actually been looked at, so record it as such.
 */
itsbeenseen(artid)
char *artid;
{
	if (nextabuf[0] == '\0')
		return;
	if (strcmp(artid, nextabuf) == 0) {
		histbuf[nbef] = (char *) malloc(strlen(artid)+1);
		strcpy(histbuf[nbef++], artid);
	}
	nextabuf[0] = '\0';
}

back() 
{
	while (fseek(actfp, -2L, 1) != -1 && ftell(actfp) > 0L) {
		if (getc(actfp) == '\n')
			return 0;
	}
	if (ftell(actfp) == 0L)
		return 0;
	return 1;
}

/*
 * Copy from one header structure to another.
 * Really should just copy memory, if we had a memcpy.
 */
hbufcp(hbuf2, hbuf1)
register struct hbuf *hbuf1, *hbuf2;
{
	strcpy(hbuf2->path, hbuf1->path);
	strcpy(hbuf2->from, hbuf1->from);
	strcpy(hbuf2->replyto, hbuf1->replyto);
	strcpy(hbuf2->nbuf, hbuf1->nbuf);
	strcpy(hbuf2->title, hbuf1->title);
	strcpy(hbuf2->ident, hbuf1->ident);
	strcpy(hbuf2->subdate, hbuf1->subdate);
	strcpy(hbuf2->recdate, hbuf1->recdate);
	strcpy(hbuf2->expdate, hbuf1->expdate);
	hbuf2->subtime = hbuf1->subtime;
	hbuf2->rectime = hbuf1->rectime;
	hbuf2->exptime = hbuf1->exptime;
}


/*
 * Trap interrupts.
 */
onsig(n)
int	n;
{
	signal(n, onsig);
	sigtrap = n;
	if (rcreadok < 2) {
		fprintf(stderr, "Aborted early\n");
		exit(0);
	}
}

/*
 * finds the line in your .newsrc file (actually the in-core "rcline"
 * copy of it) and returns the index into the array where it was found.
 * -1 means it didn't find it.
 *
 * We play clever games here to make this faster.  It's inherently
 * quadratic - we spend lots of CPU time here because we search through
 * the whole .newsrc for each line.  The "prev" variable remembers where
 * the last match was found; we start the search there and loop around
 * to the beginning, in the hopes that the calls will be roughly in order.
 */
int
findrcline(name)
char *name;
{
	register char *p, *ptr;
	register int cur;
	register int i;
	register int top;
	static int prev = 0;

	top = line; i = prev;
loop:
	for (; i <= top; i++) {
		for (p = name, ptr = rcline[i]; (cur = *p++); ) {
			if (cur != *ptr++)
				goto contin2;
		}
		if (*ptr != ':' && *ptr != '!')
			continue;
		prev = i;
		return i;
contin2:
		;
	}
	if (i > line && line > prev-1) {
		i = 0;
		top = prev-1;
		goto loop;
	}
	return -1;
}
