/*
 *
 * lex.c - lexical analysis
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#include "zsh.h"

/* lexical state */

static int xincmdpos,xincond,xincasepat,dbparens,xdbparens,xalstat;
static char *xhlastw;

static int xisfirstln, xisfirstch, xhistremmed, xhistdone,
	xspaceflag, xstophist, xlithist, xalstackind,xhlinesz;
static char *xhline, *xhptr;

/* save the lexical state */

/* is this a hack or what? */

void lexsave() /**/
{
	xincmdpos = incmdpos;
	xincond = incond;
	xincasepat = incasepat;
	xdbparens = dbparens;
	xalstat = alstat;
	xalstackind = alstackind;
	xisfirstln = isfirstln;
	xisfirstch = isfirstch;
	xhistremmed = histremmed;
	xhistdone = histdone;
	xspaceflag = spaceflag;
	xstophist = stophist;
	xlithist = lithist;
	xhline = hline;
	xhptr = hptr;
	xhlastw = hlastw;
	xhlinesz = hlinesz;
	inredir = 0;
}

/* restore lexical state */

void lexrestore() /**/
{
	incmdpos = xincmdpos;
	incond = xincond;
	incasepat = xincasepat;
	dbparens = xdbparens;
	alstat = xalstat;
	isfirstln = xisfirstln;
	isfirstch = xisfirstch;
	histremmed = xhistremmed;
	histdone = xhistdone;
	spaceflag = xspaceflag;
	stophist = xstophist;
	lithist = xlithist;
	hline = xhline;
	hptr = xhptr;
	hlastw = xhlastw;
	clearalstack();
	alstackind = xalstackind;
	hlinesz = xhlinesz;
	lexstop = errflag = 0;
}

void yylex() /**/
{
	if (tok == LEXERR) return;
	do
		tok = gettok();
	while (tok != ENDINPUT && exalias());
	if (tok != NEWLIN) isnewlin = 0;
	else isnewlin = (inbufct) ? -1 : 1;
	if (tok == SEMI || tok == NEWLIN) tok = SEPER;
}

void ctxtlex() /**/
{
static int oldpos;

	yylex();
	switch (tok) {
	case SEPER: case NEWLIN: case SEMI: case DSEMI: case AMPER:
	case INPAR: case INBRACE: case DBAR: case DAMPER: case BAR:
	case BARAMP: case INOUTPAR: case DO: case THEN: case ELIF:
	case ELSE: incmdpos = 1; break;
	case STRING: /* case ENVSTRING: */ case ENVARRAY: case OUTPAR:
	case CASE: incmdpos = 0; break;
	}
	if (IS_REDIROP(tok) || tok == FOR || tok == FOREACH || tok == SELECT) {
		inredir = 1;
		oldpos = incmdpos;
		incmdpos = 0;
	} else if (inredir) {
		incmdpos = oldpos;
		inredir = 0;
	}
}

#define LX1_BKSLASH 0
#define LX1_COMMENT 1
#define LX1_NEWLIN 2
#define LX1_SEMI 3
#define LX1_BANG 4
#define LX1_AMPER 5
#define LX1_BAR 6
#define LX1_INPAR 7
#define LX1_OUTPAR 8
#define LX1_INBRACE 9
#define LX1_OUTBRACE 10
#define LX1_INBRACK 11
#define LX1_OUTBRACK 12
#define LX1_INANG 13
#define LX1_OUTANG 14
#define LX1_OTHER 15

#define LX2_BREAK 0
#define LX2_OUTPAR 1
#define LX2_BAR 2
#define LX2_STRING 3
#define LX2_INBRACK 4
#define LX2_OUTBRACK 5
#define LX2_TILDE 6
#define LX2_INPAR 7
#define LX2_INBRACE 8
#define LX2_OUTBRACE 9
#define LX2_OUTANG 10
#define LX2_INANG 11
#define LX2_EQUALS 12
#define LX2_BKSLASH 13
#define LX2_QUOTE 14
#define LX2_DQUOTE 15
#define LX2_BQUOTE 16
#define LX2_OTHER 17

unsigned char lexact1[256],lexact2[256],lextok2[256];

void initlextabs() /**/
{
int t0;
static char *lx1 = "\\q\n;!&|(){}[]<>xx";
static char *lx2 = "x)|$[]~({}><=\\\'\"`x";

	for (t0 = 0; t0 != 256; t0++) {
		lexact1[t0] = LX1_OTHER;
		lexact2[t0] = LX2_OTHER;
		lextok2[t0] = t0;
	}
	for (t0 = 0; lx1[t0]; t0++)
		if (lx1[t0] != 'x')
			lexact1[lx1[t0]] = t0;
	for (t0 = 0; lx2[t0]; t0++)
		if (lx2[t0] != 'x')
			lexact2[lx2[t0]] = t0;
	lexact2[';'] = LX2_BREAK;
	lexact2['&'] = LX2_BREAK;
	lextok2[','] = Comma;
	lextok2['*'] = Star;
	lextok2['?'] = Quest;
	lextok2['{'] = Inbrace;
	lextok2['['] = Inbrack;
	lextok2['$'] = String;
}

/* initialize lexical state */

void lexinit() /**/
{
	incond = incasepat = nocorrect =
		dbparens = alstat = lexstop = 0;
	incmdpos = 1;
	tok = ENDINPUT;
	if (isset(EXTENDEDGLOB))
		{
		lextok2['#'] = Pound;
		lextok2['^'] = Hat;
		}
	else
		{
		lextok2['#'] = '#'; 
		lextok2['^'] = '^';
		}
}

int len = 0,bsiz = 256;
char *bptr;

/* add a char to the string buffer */

void add(c) /**/
int c;
{
	*bptr++ = c;
	if (bsiz == ++len)
		{
		int newbsiz;

		newbsiz = bsiz * 8;
		while (newbsiz < inbufct)
			newbsiz *= 2;
		bptr = len+(tokstr = hrealloc(tokstr,bsiz,newbsiz));
		bsiz = newbsiz;
		}
}

static void unadd()
{
	bptr--; len--;
}

int gettok() /**/
{
int bct = 0,pct = 0,brct = 0;
int c,d,intpos = 1;
int peekfd = -1,peek,ninbracks;

beginning:
	hlastw = NULL;
	tokstr = NULL;
	parbegin = -1;
	while (iblank(c = hgetc()) && !lexstop);
	isfirstln = 0;
	wordbeg = inbufct;
	hwbegin();
	hwaddc(c);
	if (dbparens)	/* handle ((...)) */
		{
		pct = 2;
		peek = STRING;
		len = dbparens = 0;
		bptr = tokstr = ncalloc(bsiz = 256);
		for (;;)
			{
			if (c == '(')
				pct++;
			else if (c == ')')
				pct--;
			else if (c == '\n')
				{
				zerr("parse error: )) expected",NULL,0);
				peek = LEXERR;
				return peek;
				}
			else if (c == '$')
				c = Qstring;
			if (pct >= 2)
				add(c);
			if (pct)
				c = hgetc();
			else
				break;
			}
		*bptr = '\0';
		return peek;
		}
	if (idigit(c))	/* handle 1< foo */
		{
		d = hgetc();
		hungetc(d);
		lexstop = 0;
		if (d == '>' || d == '<')
			{
			peekfd = c-'0';
			c = hgetc();
			}
		}

	/* chars in initial position in word */

	if (c == hashchar &&
			(isset(INTERACTIVECOMMENTS) ||
			(!zleparse && (!interact || unset(SHINSTDIN) || strin))))
		{
		/* changed hgetch to hgetc so comments appear in history */
		stophist = 1;
		while ((c = hgetc()) != '\n' && !lexstop);
		if (c == '\n') {
			hwaddc('\n');
			peek = NEWLIN;
		} else {
			peek = (errflag) ? LEXERR : ENDINPUT;
			errflag = 1;
		}
		return peek;
		}
	if (lexstop)
		return (errflag) ? LEXERR : ENDINPUT;
	switch (lexact1[(unsigned char) c])
		{
		case LX1_BKSLASH:
			d = hgetc();
			if (d == '\n')
				goto beginning;
			hungetc(d);
			break;
		case LX1_NEWLIN: return NEWLIN;
		case LX1_SEMI:
			d = hgetc();
			if (d != ';')
				{
				hungetc(d);
				return SEMI;
				}
			return DSEMI;
		case LX1_BANG:
			d = hgetc();
			hungetc(d);
			if (!inblank(d))
				break;
			if (incmdpos || incond)
				return BANG;
			break;
		case LX1_AMPER:
			d = hgetc();
			if (d != '&')
				{
				hungetc(d);
				return AMPER;
				}
			return DAMPER;
		case LX1_BAR:
			d = hgetc();
			if (d == '|')
				return DBAR;
			else if (d == '&')
				return BARAMP;
			hungetc(d);
			return BAR;
		case LX1_INPAR:
			d = hgetc();
			if (d == '(' && incmdpos)
				{
				tokstr = strdup("let");
				dbparens = 1;
				return STRING;
				}
			else if (d == ')')
				return INOUTPAR;
			hungetc(d);
			if (!(incond || incmdpos))
				break;
			return INPAR;
		case LX1_OUTPAR: return OUTPAR;
		case LX1_INBRACE: if (!incmdpos) break; return INBRACE;
		case LX1_OUTBRACE: return OUTBRACE;
		case LX1_INBRACK:
			if (!incmdpos)
				break;
			d = hgetc();
			if (d == '[')
				return DINBRACK;
			hungetc(d);
			break;
		case LX1_OUTBRACK:
			if (!incond)
				break;
			d = hgetc();
			if (d == ']')
				return DOUTBRACK;
			hungetc(d);
			break;
		case LX1_INANG:
			d = hgetc();
			if ((!incmdpos && d == '(') || incasepat) {
				hungetc(d);
				break;
			} else if (d == '<') {
				int e = hgetc();

				if (e == '(') {
					hungetc(e);
					hungetc(d);
					peek = INANG;
				} else if (e == '<')
					peek = TRINANG;
				else if (e == '-')
					peek = DINANGDASH;
				else {
					hungetc(e);
					peek = DINANG;
				}
			} else if (d == '&')
				peek = INANGAMP;
			else {
				peek = INANG;
				hungetc(d);
			}
			tokfd = peekfd;
			return peek;
		case LX1_OUTANG:
			d = hgetc();
			if (d == '(')
				{
				hungetc(d);
				break;
				}
			else if (d == '&')
				{
				d = hgetc();
				if (d == '!')
					peek = OUTANGAMPBANG;
				else
					{
					hungetc(d);
					peek = OUTANGAMP;
					}
				}
			else if (d == '!')
				peek = OUTANGBANG;
			else if (d == '>')
				{
				d = hgetc();
				if (d == '&')
					{
					d = hgetc();
					if (d == '!')
						peek = DOUTANGAMPBANG;
					else
						{
						hungetc(d);
						peek = DOUTANGAMP;
						}
					}
				else if (d == '!')
					peek = DOUTANGBANG;
				else if (d == '(')
					{
					hungetc(d);
					hungetc('>');
					peek = OUTANG;
					}
				else
					{
					hungetc(d);
					peek = DOUTANG;
					if (isset(NOCLOBBER)) hwaddc('!');
					}
				}
			else
				{
				hungetc(d);
				peek = OUTANG;
				if (isset(NOCLOBBER)) hwaddc('!');
				}
			tokfd = peekfd;
			return peek;
		}

	/* we've started a string, now get the rest of it, performing
		tokenization */

	peek = STRING;
	len = 0;
	bptr = tokstr = ncalloc(bsiz = 256);
	for(;;)
		{
		int act;
		int d;
		
		if (inblank(c))
			act = LX2_BREAK;
		else
			{
			act = lexact2[(unsigned char) c];
			c = lextok2[(unsigned char) c];
			}
		switch (act)
			{
			case LX2_BREAK: goto brk;
			case LX2_OUTPAR:
				if (!pct)
					goto brk;
				c = Outpar;
				pct--;
				break;
			case LX2_BAR:
				if (!pct && !incasepat)
					goto brk;
				c = Bar;
				break;
			case LX2_STRING:
				d = hgetc();
				if (d == '[')
					{
					add(String);
					add(Inbrack);
					ninbracks = 1;
					while (ninbracks && (c = hgetc()) && !lexstop) {
						if (c == '[') ninbracks++;
						else if (c == ']') ninbracks--;
						if (ninbracks) add(c);
					}
					c = Outbrack;
					}
				else if (d == '(')
					{
					add(String);
					if (skipcomm()) { peek = LEXERR; goto brk; }
					c = Outpar;
					}
				else
					hungetc(d);
				break;
			case LX2_INBRACK: brct++; break;
			case LX2_OUTBRACK:
				if (incond && !brct)
					goto brk;
				brct--;
				c = Outbrack;
				break;
			case LX2_TILDE: /* if (intpos) */ c = Tilde; break;
			case LX2_INPAR:
				d = hgetc();
				hungetc(d);
				if (d == ')' || (incmdpos && peek != ENVSTRING))
					goto brk;
				pct++;
				c = Inpar;
				break;
			case LX2_INBRACE: bct++; break;
			case LX2_OUTBRACE:
				if (!bct)
					goto brk;
				bct--;
				c = Outbrace;
				break;
			case LX2_OUTANG:
				d = hgetc();
				if (d != '(')
					{
					hungetc(d);
					goto brk;
					}
				add(Outang);
				if (skipcomm()) { peek = LEXERR; goto brk; }
				c = Outpar;
				break;
			case LX2_INANG:
				d = hgetc();
				if (!(idigit(d) || d == '-' || d == '>' || d == '(' || d == ')'))
					{
					hungetc(d);
					goto brk;
					}
				c = Inang;
				if (d == '(')
					{
					add(c);
					if (skipcomm()) { peek = LEXERR; goto brk; }
					c = Outpar;
					}
				else if (d == ')')
					hungetc(d);
				else
					{
					add(c);
					c = d;
					while (c != '>' && !lexstop)
						add(c),c = hgetc();
					c = Outang;
					}
				break;
			case LX2_EQUALS:
				if (intpos)
					{
					d = hgetc();
					if (d != '(')
						{
						hungetc(d);
						c = Equals;
						}
					else
						{
						add(Equals);
						if (skipcomm()) { peek = LEXERR; goto brk; }
						c = Outpar;
						}
					}
				else if (peek != ENVSTRING && incmdpos)
					{
					d = hgetc();
					if (d == '(' && incmdpos)
						{
						*bptr = '\0';
						return ENVARRAY;
						}
					hungetc(d);
					peek = ENVSTRING;
					intpos = 2;
					}
				break;
			case LX2_BKSLASH:
				c = hgetc();
				if (c == '\n')
					{
					c = hgetc();
					continue;
					}
				add(c);
				c = hgetc();
				continue;
			case LX2_QUOTE:
				add(Nularg);

				/* we add the Nularg to prevent this:

				echo $PA'TH'

				from printing the path. */

				for (;;) {
					while ((c = hgetc()) != '\'' && !lexstop) {
						if (isset(CSHJUNKIEQUOTES) && c == '\n') {
							if (bptr[-1] == '\\') unadd(); else break;
						}
						add(c);
					}
					if (c != '\'') {
						zerr("unmatched \'",NULL,0);
						peek = LEXERR;
						goto brk;
					}
					d = hgetc();
					if (d != '\'' || unset(RCQUOTES)) break;
					add(c);
				}
				hungetc(d);
				c = Nularg;
				break;
			case LX2_DQUOTE:
				add(Nularg);
				while ((c = hgetc()) != '\"' && !lexstop)
					if (c == '\\')
						{
						c = hgetc();
						if (c != '\n')
							{
							if (c != '$' && c != '\\' && c != '\"' && c != '`')
								add('\\');
							add(c);
							}
						}
					else {
						if (isset(CSHJUNKIEQUOTES) && c == '\n') {
							if (bptr[-1] == '\\') unadd(); else break;
						}
						if (c == '$') {
							d = hgetc();
							if (d == '(') {
								add(Qstring);
								if (skipcomm()) { peek = LEXERR; goto brk; }
								c = Outpar;
							} else if (d == '[') {
								add(String);
								add(Inbrack);
								while ((c = hgetc()) != ']' && !lexstop)
									add(c);
								c = Outbrack;
							} else {
								c = Qstring;
								hungetc(d);
							}
						} else if (c == '`')
							c = Qtick;
						add(c);
					}
				if (c != '\"') {
					zerr("unmatched \"",NULL,0);
					peek = LEXERR;
					goto brk;
				}
				c = Nularg;
				break;
			case LX2_BQUOTE:
				add(Tick);
				parbegin = inbufct;
				while ((c = hgetc()) != '`' && !lexstop)
					if (c == '\\')
						{
						c = hgetc();
						if (c != '\n')
							{
							if (c != '`' && c != '\\' && c != '$')
								add('\\');
							add(c);
							}
						}
					else {
						if (isset(CSHJUNKIEQUOTES) && c == '\n') {
							if (bptr[-1] == '\\') unadd(); else break;
						}
						add(c);
					}
				if (c != '`') {
					if (!zleparse) zerr("unmatched `",NULL,0);
					peek = LEXERR;
					goto brk;
				}
				c = Tick;
				parbegin = -1;
				break;
			}
		add(c);
		c = hgetc();
		if (intpos)
			intpos--;
		if (lexstop)
			break;
		}
brk:
	hungetc(c);
	*bptr = '\0';
	return peek;
}

/* expand aliases, perhaps */

int exalias() /**/
{
struct alias *an;
char *s,*t;

	s = yytext = hwadd();
	for (t = s; *t && *t != HISTSPACE; t++);
	if (!*t)
		t = NULL;
	else
		*t = '\0';
	if (interact && isset(SHINSTDIN) && !strin && !incasepat && tok == STRING &&
		(isset(CORRECTALL) || (isset(CORRECT) && incmdpos)) && !nocorrect)
			spckword(&tokstr,&s,&t,!incmdpos,1);
	if (zleparse && !alstackind) {
		int zp = zleparse;
		gotword(s);
		if (zp && !zleparse) {
			if (t) *t = HISTSPACE;
			return 0;
		}
	}
	an = gethnode(s,aliastab);
	if (t) *t = HISTSPACE;
	if (alstackind != MAXAL && an && !an->inuse)
		if (!(an->cmd && !incmdpos && alstat != ALSTAT_MORE)) {
			if (an->cmd < 0) {
				tok = DO-an->cmd-1;
				return 0;
			} else {
				an->inuse = 1;
				hungets(ALPOPS);
				hungets((alstack[alstackind++] = an)->text);
				alstat = 0;
				/* remove from history if it begins with space */
				if (isset(HISTIGNORESPACE) && an->text[0] == ' ') remhist();
				lexstop = 0;
				return 1;
			}
		}
	return 0;
}

/* skip (...) */

int skipcomm() /**/
{
int pct = 1,c;

	parbegin = inbufct;
	c = Inpar;
	do
		{
		add(c);
		c = hgetc();
		if (itok(c) || lexstop)
			break;
		else if (c == '(') pct++;
		else if (c == ')') pct--;
		else if (c == '\\')
			{
			add(c);
			c = hgetc();
			}
		else if (c == '\'')
			{
			add(c);
			while ((c = hgetc()) != '\'' && !lexstop)
				add(c);
			}
		else if (c == '\"')
			{
			add(c);
			while ((c = hgetc()) != '\"' && !lexstop)
				if (c == '\\')
					{
					add(c);
					add(hgetc());
					}
				else add(c);
			}
		else if (c == '`')
			{
			add(c);
			while ((c = hgetc()) != '`' && !lexstop)
				if (c == '\\') add(c), add(hgetc());
				else add(c);
			}
		}
	while(pct);
	if (!lexstop) parbegin = -1;
	return lexstop;
}

