/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "yy.h"

/*
 * COMMENT PROCESSING CLUSTER
 *
 * The global organization of this cluster is as follows.
 * While parsing the program information is saved in the tree which
 * tells the source text coordinates (sequence numbers and columns)
 * bounding each production.  The comments from the source program
 * are also saved, with information about their source text position
 * and a classification as to their kind.
 *
 * When printing the reformatted program we flush out the comments
 * at various points using the information in the comments and the parse
 * tree to "resynchronize".  A number of special cases are recognized to
 * deal with the vagarities of producing a true "fixed point" so that
 * a prettyprinted program will re-prettyprint to itself.
 */

/*
 * Save sequence id's and column markers bounding a production
 * for later use in placing comments.  We save the sequence id
 * and column of the leftmost token and the following token, and
 * the sequence id of the last token in this reduction.
 * See putcm, putcml, and putcmp below for motivation.
 */
line2of(l)
	int l;
{

	return (lineNof(l, 2));
}

lineof(l)
	int l;
{

	return (lineNof(l, 1));
}

lineNof(l, i)
	int l, i;
{

	return(tree(6, l, yypw[i].Wseqid, yypw[i].Wcol, yyseqid, yycol, yypw[N].Wseqid));
}

/*
 * After a call to setline, Seqid is set to the sequence id
 * of the symbol which followed the reduction in which the
 * lineof call was embedded, Col to the associated column,
 * and LSeqid to the sequence id of the last symbol in the reduction
 * (Note that this is exact only if the last symbol was a terminal
 * this is always true when it matters.)
 */
int	Seqid, Col, LSeqid;

/*
 * Retrieve the information from a call to lineof before beginning the
 * output of a tree from a reduction.  First flush to the left margin
 * of the production, and then set so that later calls to putcm, putcml
 * and putcmp will deal with the right margin of this comment.
 *
 * The routine setinfo is called when the lineof has no embedded line
 * number to avoid trashing the current "line".
 *
 * The routine setinfo is often called after completing the output of
 * the text of a tree to restore Seqid, Col, and LSeqid which may have
 * been destroyed by the nested processing calls to setline.
 * In this case the only effect of the call to setinfo is to
 * modify the above three variables as a side effect.
 *
 * We return a word giving information about the comments which were
 * actually put out.  See putcm for details.
 */
setline(ip)
	int *ip;
{

	line = ip[0];
	return(setinfo(ip));
}

setinfo(ip)
	register int *ip;
{
	register int i;

	ip++;
	Seqid = *ip++;
	Col = *ip++;
	i = putcm();
	Seqid = *ip++;
	Col = *ip++;
	LSeqid = *ip++;
	return (i);
}

char	cmeof, incomm;

/*
 * Get the text of a comment from the input stream,
 * recording its type and linking it into the linked
 * list of comments headed by cmhp.
 */
getcm(cmdelim)
	char cmdelim;
{
	int cmjust, col;
	register struct comment *cp;
	register struct commline *kp;

	incomm = 1;
	if (cmdelim == '*' && yycol == 10 || cmdelim == '{' && yycol == 9)
		cmjust = CLMARG;
	else if (yytokcnt <= 1)
		cmjust = CALIGN;
	else if (yywhcnt < 2)
		cmjust = CTRAIL;
	else
		cmjust = CRMARG;
	col = yycol - (cmdelim == '{' ? 1 : 2);
	cp = tree5(NIL, cmdelim, NIL, cmjust, yyseqid);
	cmeof = 0;
	do {
		kp = getcmline(cmdelim);
		if (cp->cml == NIL) {
			kp->cml = kp;
			kp->cmcol = col;
		} else {
			kp->cml = cp->cml->cml;
			cp->cml->cml = kp;
			switch (cp->cmjust) {
				case CTRAIL:
				case CRMARG:
					cp->cmjust = CALIGN;
			}
		}
		cp->cml = kp;
	} while (!cmeof);
	newcomm(cp);
	incomm = 0;
}

/*
 * Chain the new comment at "cp" onto the linked list of comments.
 */
newcomm(cp)
	register struct comment *cp;
{

	if (cmhp == NIL)
		cp->cmnext = cp;
	else {
		cp->cmnext = cmhp->cmnext;
		cmhp->cmnext = cp;
	}
	cmhp = cp;
}


int	nilcml[3];

quickcomm(t)
	int t;
{

	if (incomm)
		return;
	newcomm(tree5(nilcml, NIL, NIL, t, yyseqid));
}

commincl(cp, ch)
	char *cp, ch;
{

	newcomm(tree5(nilcml, savestr(cp), ch, CINCLUD, yyseqid));
}

getcmline(cmdelim)
	char cmdelim;
{
	char lastc;
	register char *tp;
	register CHAR c;
	register struct commline *kp;

	c = getchar();
	kp = tree3(NIL, yycol, NIL);
	tp = token;
	lastc = 0;
	for (;;) {
		switch (c) {
			case '}':
				if (cmdelim == '{')
					goto endcm;
				break;
			case ')':
				if (cmdelim == '*' && lastc == '*') {
					--tp;
					goto endcm;
				}
				break;
			case '\n':
				goto done;
			case -1:
				yerror("Comment does not terminate - QUIT");
				pexit(ERRS);
		}
		lastc = c;
		*tp++ = c;
		c = getchar();
	}
endcm:
	cmeof++;
done:
	*tp = 0;
	kp->cmtext = copystr(token);
	return (kp);
}

/*
 * Flush through the line this token is on.
 * Ignore if next token on same line as this one.
 */
putcml()
{
	register int i, SSeqid, SCol;

	if (Seqid == LSeqid)
		return;
	SSeqid = Seqid, SCol = Col;
	Seqid = LSeqid, Col = 32767;
	i = putcm();
	Seqid = SSeqid, Col = SCol;
	return (i);
}

/*
 * Flush to the beginning of the line this token is on.
 * Ignore if this token is on the same line as the previous one
 * (effectively since all such already then flushed.)
 */
putcmp()
{
	register int i, SSeqid, SCol;

	SSeqid = Seqid, SCol = Col;
	Seqid = LSeqid, Col = 0;
	i = putcm();
	Seqid = SSeqid, Col = SCol;
	return (i);
}

/*
 * Put out the comments to the border indicated by Seqid and Col
 */
putcm()
{
	register struct comment *cp;
	register int i;

	cp = cmhp;
	if (cp == NIL)
		return (0);
	i = 0;
	cp = cp->cmnext;
	while (cp->cmseqid < Seqid || cp->cmseqid == Seqid && cp->cml->cmcol < Col) {
		putone(cp);
		i =| 1 << cp->cmjust;
		if (cp->cmnext == cp) {
			cmhp = NIL;
			break;
		}
		cp = cp->cmnext;
		cmhp->cmnext = cp;
	}
	return (i);
}

/*
 * Put out one comment.
 * Note that empty lines, form feeds and #include statements
 * are treated as comments are regurgitated here.
 */
putone(cp)
	register struct comment *cp;
{
	register struct commline *cml, *cmf;

	align(cp);
	switch (cp->cmjust) {
		case CINCLUD:
		     /* ppflush() */
			if (noinclude == 0) {
				putchar('\f');
				return;
			}
			printf("#include %c%s%c", cp->cml, cp->cmdelim, cp->cml);
			return;
	}
	if (stripcomm)
		return;
	switch (cp->cmjust) {
		case CFORM:
			ppop("\f");
			ppnl();
		case CNL:
		case CNLBL:
			return;
	}
	ppbra(cp->cmdelim == '{' ? "{" : "(*");
	cmf = cp->cml->cml;
	ppid(cmf->cmtext);
	for (cml = cmf->cml; cml != cmf; cml = cml->cml) {
		align(cp);
		oneline(cmf->cmcol, cml);
	}
	ppket(cp->cmdelim == '{' ? "}" : "*)");
}

/*
 * Do the preliminary horizontal and vertical
 * motions necessary before beginning a comment,
 * or between lines of a mult-line comment.
 */
align(cp)
	register struct comment *cp;
{

	switch (cp->cmjust) {
		case CNL:
			ppsnl();
			break;
		case CNLBL:
			ppsnlb();
			break;
		case CFORM:
		case CINCLUD:
			ppnl();
			break;
		case CLMARG:
			ppnl();
			if (profile)
				ppid("\t");
			break;
		case CALIGN:
			ppnl();
			indent();
			break;
		case CTRAIL:
			ppspac();
			break;
		case CRMARG:
		case CSRMARG:
			pptab();
			break;
	}
}

/*
 * One line of a multi-line comment
 * Deal with alignment and initial white space trimming.
 * The "margin" indicates where the first line of the
 * comment began... don't print stuff in this comment
 * which came before this.
 */
oneline(margin, cml)
	int margin;
	struct commline *cml;
{
	register char *tp;
	register int i;

	for (i = 8, tp = cml->cmtext; i < margin && *tp; tp++)
		switch (*tp) {
			case ' ':
				i++;
				continue;
			case '\t':
				i =+ 8;
				i =& ~7;
				if (i < margin)
					continue;
				ppop("\t");
			default:
				goto out;
		}
out:
	ppid(tp);
}

/*
 * Flush all comments
 */
flushcm()
{

	Seqid = 32767;
	return(putcm());
}

#define	BLANKS	((1 << CNL) | (1 << CNLBL) | (1 << CFORM))
noblank(i)
	int i;
{

	return ((i & BLANKS) == 0);
}

int	needform, neednlbl, neednl, needseqid;

needtree()
{
	register struct comment *cp;

	needform = neednlbl = neednl = 0;
	cp = cmhp;
	if (cp == NIL)
		return (0);
	do {
		switch (cp->cmjust) {
			case CNL:
				neednl++;
				goto seq;
			case CNLBL:
				neednlbl++;
				goto seq;
			case CFORM:
				needform++;
seq:
				needseqid = cp->cmseqid;
				break;
			default:
				neednl = neednlbl = needform = 0;
				return (1);
		}
		cp = cp->cmnext;
	} while (cp != cmhp);
	cmhp = NIL;
	return (0);
}

packtree()
{
	int save;

	save = yyseqid;
	yyseqid = needseqid;
	for (; needform > 0; needform--)
		commform();
	for (; neednl > 0; neednl--)
		commnl();
	for (; neednlbl > 0; neednlbl--)
		commnlbl();
	yyseqid = save;
}
