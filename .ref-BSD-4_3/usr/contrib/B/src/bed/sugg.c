/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: sugg.c,v 2.4 84/10/26 12:10:51 guido Exp $";

/*
 * B editor -- New suggestion handling module.
 */

#include "feat.h"

#ifdef USERSUGG

#include "b.h"
#include "bobj.h"
#include "node.h"
#include "supr.h"
#include "gram.h"
#include "queu.h"

#include <ctype.h>

extern bool dflag;
extern bool edontop;

extern bool lefttorite;

#ifndef SUGGFILE
#define SUGGFILE ".Bed_sugg"
#endif

#define MAXNSUGG 1000

Hidden value sugg[MAXNSUGG];
Hidden int nsugg;
Hidden int nbuiltin;
Hidden bool suggchanges;
Hidden bool ignorefirstcall; /* Communication between killsugg and setsugg */

/*
 * Read the suggestion table from file.
 */

Visible Procedure
initsugg()
{
	char buffer[1000];
	register FILE *fp;
	register c;

	fp = fopen(SUGGFILE, "r");
	if (!fp) {
		if (dflag) {
			fprintf(stderr, "*** No suggestion file: ");
			perror(SUGGFILE);
		}
		return;
	}
	while (fgets(buffer, sizeof buffer, fp)) {
		if (!index(buffer, '\n')) { /* Skip long line */
			fprintf(stderr,
			    "*** Excessively long suggestion ignored\n");
			while ((c = getc(fp)) != '\n' && c != EOF)
				;
		}
		else
			addsugg(buffer, -1);
	}
	fclose(fp);
}


/*
 * Make sure a line looks like a suggestion, return No if not.
 * Replace the trailing newline or comment-sign by a zero byte.
 * ***** Should check more thoroughly. *****
 */

Hidden bool
checksugg(bp)
	string bp;
{
	if (!isascii(*bp) || !isupper(*bp))
		return No;
	while (*bp && *bp != '\n' && *bp != '\\')
		++bp;
	*bp = 0;
	return Yes;
}


/*
 * Procedure to add a suggestion to the suggestion table.
 */

Visible Procedure
addsugg(str, builtin)
	string str;
	int builtin;
{
	int i;
	int j;
	int len;
	int cmp;
	string suggi;
	int where = (builtin == -1) ? nsugg : nbuiltin;

	if (!checksugg(str))
		return;
	for (len = 0; str[len] && str[len] != ' '; ++len)
		;
	for (i = nsugg-1; i >= 0; --i) {
		suggi = Str(sugg[i]);
		cmp = strncmp(str, suggi, len);
		if (cmp < 0)
			continue;
		if (cmp > 0) {
			if (i >= where)
				where = i+1;
			continue;
		}
		if (suggi[len] && suggi[len] != ' ')
			continue; /* No match, just prefix */
		if (Strequ(str+len, suggi+len))
			return; /* Ignore exact duplicates */
		if (i < nbuiltin)
			return; /* Cannot replace built-in */
		/* Replacement */
		sugg[i] = mk_text(str); /* No use to release the old one... */
					/* ...its refcount is infinite */
		fix(sugg[i]);
		suggchanges = Yes;
		return;
	}
	/* Insertion */
	if (nsugg >= MAXNSUGG)
		return; /* Table overflow */
	if (builtin == Yes)
		++nbuiltin;
	for (j = nsugg; j > where; --j)
		sugg[j] = sugg[j-1];
	++nsugg;
	sugg[where] = mk_text(str);
	fix(sugg[where]);
	suggchanges = Yes;
}


/*
 * Procedure to delete a suggestion from the suggestion table.
 * Must supply the whole string as argument.
 */

Hidden Procedure
delsugg(str)
	string str;
{
	int i;

	for (i = 0; i < nsugg; ++i) {
		if (Strequ(str, Str(sugg[i]))) {
			--nsugg;
			for (; i < nsugg; ++i)
				sugg[i] = sugg[i+1];
			suggchanges = Yes;
			return;
		}
	}
}


/*
 * Return a suitable suggestion which matches str for len characters.
 * If len > 1, and all of str (even beyond len) equals some table
 * entry, the first matching entry after that is preferred; otherwise,
 * the first matching entry at all is returned.
 * Vnil is returned if no entry matches.
 */

Hidden node
nextsugg(str, len)
	string str;
	int len;
{
	bool found = !str[len];
	int first = -1;
	int i;

	for (i = 0; i < nsugg; ++i) {
		if (!Strnequ(str, Str(sugg[i]), len))
			continue;
		if (found)
			return (node) sugg[i];
		if (Strequ(str+len, Str(sugg[i])+len))
			found = Yes;
		if (first < 0)
			first = i;
	}
	if (first >= 0)
		return (node) sugg[first];
	return Nnil;
}


/*
 * Procedure to save the suggestion file if it has been changed.
 */

Visible Procedure
endsugg()
{
	FILE *fp;
	int i;

	if (!suggchanges)
		return;
	suggchanges = No;
	fp = fopen(SUGGFILE, "w");
	if (!fp) {
		if (dflag) {
			fprintf(stderr, "*** Can't rewrite ");
			perror(SUGGFILE);
		}
		return;
	}
	if (dflag)
		fprintf(stderr, "*** [Rewriting suggestion file]\n");
	for (i = nbuiltin; i < nsugg; ++i)
		fprintf(fp, "%s\n", Str(sugg[i]));
	if (fclose(fp) == EOF) {
		fprintf(stderr, "*** Can't finish writing ");
		perror(SUGGFILE);
		return;
	}
}


/*
 * Find a new suggestion or advance in the current one.
 * Interface styled like resuggest: string pointer is advanced here.
 */

Visible bool
newsugg(ep, pstr, alt_c)
	environ *ep;
	string *pstr;
	int alt_c;
{
	char buffer[1000];
	node n = tree(ep->focus);
	node nn;
	int sym = symbol(n);
	string str;
	string bp;
	bool end;

	Assert(pstr && *pstr);
	if (sym != Suggestion || ep->mode != VHOLE || ep->s1 != 2)
		return No;
	strncpy(buffer, Str((value)firstchild(n)), sizeof buffer);
	for (str = *pstr, bp = buffer+ep->s2, end = No;
			*str && bp < buffer + sizeof buffer; ++str, ++bp) {
		if (!*bp)
			end = Yes;
		*bp = *str;
	}
	if (end)
		*bp = 0;
	nn = (node)nextsugg(buffer, ep->s2 + 1);
	if (!nn) {
		if (!alt_c)
			return No;
		buffer[ep->s2] = alt_c;
		nn = (node)nextsugg(buffer, ep->s2 + 1);
		if (!nn)
			return No;
	}
	if (nn != firstchild(n)) {
		s_down(ep);
		replace(&ep->focus, nn);
		s_up(ep);
	}
	/* No need to release because its refcount is infinite anyway */
	++ep->s2;
	if (**pstr == ' ')
		accsugg(ep);
	++*pstr;
	return Yes;
}


/*
 * Kill suggestion -- only the part to the left of the focus is kept.
 */

Visible Procedure
killsugg(ep)
	environ *ep;
{
	queue q = Qnil;
	char buffer[1000];
	node n = tree(ep->focus);

	Assert(ep->mode == VHOLE && ep->s1 == 2 && symbol(n) == Suggestion);
	strncpy(buffer, Str((value)firstchild(n)), ep->s2);
	buffer[ep->s2] = 0;
	delfocus(&ep->focus);
	ep->mode = WHOLE;
	ignorefirstcall = Yes;
	ins_string(ep, buffer, &q, 0);
	qrelease(q);
	ignorefirstcall = No;
}


/*
 * Place an initial suggestion in a node.
 */

Visible bool
setsugg(pp, c, ep)
	path *pp;
	char c;
	environ *ep;
{
	char buf[2];
	node n;

	if (lefttorite)
		return No;
	if (ignorefirstcall) {
		ignorefirstcall = No;
		return No;
	}
	buf[0] = c;
	buf[1] = 0;
	n = (node)nextsugg(buf, 1);
	if (!n)
		return No;
	replace(pp, newnode(1, Suggestion, &n));
	ep->mode = VHOLE;
	ep->s1 = 2;
	ep->s2 = 1;
	return Yes;
}


/*
 * Accept a suggestion -- turn it into real nodes.
 */

Visible Procedure
accsugg(ep)
	environ *ep;
{
	node n = tree(ep->focus);
	int s2 = ep->s2;
	queue q = Qnil;
	environ env;

	Assert(symbol(n) == Suggestion && ep->mode == VHOLE && ep->s1 == 2);
	stringtoqueue(Str((value)firstchild(n)) + s2, &q);
	killsugg(ep);
	Ecopy(*ep, env);
	if (app_queue(ep, &q))
		Erelease(env);
	else {
		Erelease(*ep);
		Emove(env, *ep);
		qrelease(q);
	}
}


/*
 * Procedure called when a unit is read in.
 * It tries to update the suggestion database.
 * It also remembers the suggestion so that it can be removed by writesugg
 * if that finds the unit was deleted.
 */

Hidden char lastsugg[1000];

Visible Procedure
readsugg(p)
	path p;
{
	p = pathcopy(p);
	top(&p);
	getpattern(lastsugg, tree(p));
	pathrelease(p);
	addsugg(lastsugg, No);
}


/*
 * Procedure called when a unit is saved.
 * It tries to update the suggestion database.
 * If the unit appears empty, the last suggestion passed to readsugg
 * will be deleted.
 */

Visible Procedure
writesugg(p)
	path p;
{
	p = pathcopy(p);
	top(&p);
	if (width(tree(p)) == 0)
		delsugg(lastsugg);
	else {
		getpattern(lastsugg, tree(p));
		if (lastsugg[0])
			addsugg(lastsugg, No);
	}
	pathrelease(p);
}


/*
 * Procedure to find out the suggestion that fits the current unit.
 * Makes the buffer empty if not a HOW'TO unit.
 * ***** Won't work if B-grammar is severely changed! *****
 */

Hidden Procedure
getpattern(buffer, n)
	string buffer;
	node n;
{
	string *rp = noderepr(n);

	buffer[0] = 0;
	while (Fw_zero(rp[0])) {
		if (nchildren(n) == 0)
			return;
		n = firstchild(n);
		rp = noderepr(n);
	}
	if (!Strequ(rp[0], "HOW'TO ") || nchildren(n) < 1)
		return;
	subgetpattern(&buffer, firstchild(n));
	*buffer = 0;
}


/*
 * Refinement for getpattern to do the work.
 */

Hidden Procedure
subgetpattern(pbuf, n)
	string *pbuf;
	node n;
{
	string *rp;
	int i;
	int nch;

	rp = noderepr(n);
	nch = (Type(n) == Tex) ? 0 : nchildren(n);
	for (i = 0; i <= nch; ++i) {
		if (i > 0)
			subgetpattern(pbuf, child(n, i));
		if (Fw_positive(rp[i])) {
			if (islower(rp[i][0]))
				*(*pbuf)++ = '?';
			else {
				strcpy(*pbuf, rp[i]);
				*pbuf += strlen(*pbuf);
			}
		}
	}
}

#endif USERSUGG
