/*
 *
 * parse.c - parser
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
#include "funcs.h"

#define YYERROR { tok = LEXERR; return NULL; }
#define YYERRORV { tok = LEXERR; return; }

#define make_list() allocnode(N_LIST)
#define make_sublist() allocnode(N_SUBLIST)
#define make_pline() allocnode(N_PLINE)
#define make_cmd() allocnode(N_CMD)
#define make_forcmd() allocnode(N_FOR)
#define make_casecmd() allocnode(N_CASE)
#define make_ifcmd() allocnode(N_IF)
#define make_whilecmd() allocnode(N_WHILE)
#define make_varnode() allocnode(N_VARASG)
#define make_cond() allocnode(N_COND)

/*
 * event	: ENDINPUT
 *			| SEPER
 *			| sublist [ SEPER | AMPER ]
 */
List parse_event() /**/
{
	tok = ENDINPUT;
	incmdpos = 1;
	yylex();
	return par_event();
}

List par_event() /**/
{
Sublist sl;
List l = NULL;

	while (tok == SEPER) {
		if (isnewlin > 0) return NULL;
		yylex();
	}
	if (tok == ENDINPUT) return NULL;
	if (sl = par_sublist())
		if (tok == ENDINPUT) {
			l = make_list();
			l->type = SYNC; l->left = sl;
		} else if (tok == SEPER) {
			l = make_list();
			l->type = SYNC; l->left = sl;
			if (isnewlin <= 0) yylex();
		} else if (tok == AMPER) {
			l = make_list();
			l->type = ASYNC; l->left = sl;
			yylex();
		} else
			l = NULL;
	if (!l) {
		if (errflag) { yyerror(); return NULL; }
		yyerror();
		errflag = 0;
		if (isnewlin <= 0) {
			int c;

			hwbegin();
			while ((c = hgetc()) != '\n' && !lexstop);
			if (c == '\n') hungetc('\n');
			hwaddc(HISTSPACE);
			hwadd();
		}
		errflag = 1;
		return NULL;
	} else {
		l->right = par_event();
	}
	return l;
}

List parse_list() /**/
{
	tok = ENDINPUT;
	incmdpos = 1;
	yylex();
	return par_list();
}

/*
 * list	: { SEPER } [ sublist [ { SEPER | AMPER } list ] ]
 */
List par_list() /**/
{
Sublist sl;
List l = NULL;

	while (tok == SEPER) yylex();
	if (sl = par_sublist())
		if (tok == SEPER || tok == AMPER) {
			l = make_list();
			l->left = sl;
			l->type = (tok == SEPER) ? SYNC : ASYNC;
			incmdpos = 1;
			while (tok == SEPER || tok == AMPER) yylex();
			l->right = par_list();
		} else {
			l = make_list();
			l->left = sl;
			l->type = SYNC;
		}
	return l;
}

List par_list1() /**/
{
Sublist sl;
List l = NULL;

	if (sl = par_sublist()) {
		l = make_list();
		l->type = SYNC;
		l->left = sl;
	}
	return l;
}

/*
 * sublist	: sublist2 [ ( DBAR | DAMPER ) { SEPER } sublist ]
 */
Sublist par_sublist() /**/
{
Sublist sl;

	if (sl = par_sublist2())
		if (tok == DBAR || tok == DAMPER) {
			int qtok = tok;
			yylex();
			while (tok == SEPER) yylex();
			sl->right = par_sublist();
			sl->type = (qtok == DBAR) ? ORNEXT : ANDNEXT;
		}
	return sl;
}

/*
 * sublist2	: [ COPROC | BANG ] pline
 */
Sublist par_sublist2() /**/
{
Sublist sl;
Pline p;

	sl = make_sublist();
	if (tok == COPROC) { sl->flags |= PFLAG_COPROC; yylex(); }
	else if (tok == BANG) { sl->flags |= PFLAG_NOT; yylex(); }
	if (!(p = par_pline()) && !sl->flags)
		return NULL;
	sl->left = p;
	return sl;
}

/*
 * pline	: cmd [ ( BAR | BARAMP ) { SEPER } pline ]
 */
Pline par_pline() /**/
{
Cmd c;
Pline p,p2;

	if (!(c = par_cmd()))
		return NULL;
	if (tok == BAR) {
		yylex();
		while (tok == SEPER) yylex();
		p2 = par_pline();
		p = make_pline();
		p->left = c; p->right = p2; p->type = PIPE;
		return p;
	} else if (tok == BARAMP) {
		struct redir *rdr = alloc(sizeof *rdr);
		rdr->type = MERGE; rdr->fd1 = 2; rdr->fd2 = 1;
		addnode(c->redir,rdr);

		yylex();
		p2 = par_pline();
		p = make_pline();
		p->left = c; p->right = p2; p->type = PIPE;
		return p;
	} else {
		p = make_pline();
		p->left = c; p->type = END;
		return p;
	}
}

/*
 * cmd	: { redir } ( for | case | if | while | repeat |
 *				subsh | funcdef | time | dinbrack | simple ) { redir }
 */
Cmd par_cmd() /**/
{
Cmd c;

	c = make_cmd();
	c->args = newlist();
	c->redir = newlist();
	c->vars = newlist();
	while (IS_REDIROP(tok))
		par_redir(c->redir);
	switch (tok) {
	case FOR: case FOREACH: case SELECT: par_for(c); break;
	case CASE: par_case(c); break;
	case IF: par_if(c); break;
	case WHILE: case UNTIL: par_while(c); break;
	case REPEAT: par_repeat(c); break;
	case INPAR: case INBRACE: par_subsh(c); break;
	case FUNC: par_funcdef(c); break;
	case TIME: par_time(c); break;
	case DINBRACK: par_dinbrack(c); break;
	default: if (!par_simple(c)) return NULL; break;
	}
	while (IS_REDIROP(tok))
		par_redir(c->redir);
	incmdpos = 1;
	incasepat = 0;
	incond = 0;
	return c;
}

/*
 * for	: ( FOR[EACH] | SELECT ) name ( "in" wordlist | INPAR wordlist OUTPAR )
				{ SEPER } ( DO list DONE | INBRACE list OUTBRACE |
				list ZEND | list1 )
 */
void par_for(c) /**/
Cmd c;
{
struct forcmd *f;
int csh = (tok == FOREACH || isset(CSHJUNKIELOOPS));

	f = make_forcmd();
	c->type = (tok == SELECT) ? CSELECT : CFOR;
	incmdpos = 0;
	yylex();
	if (tok != STRING) YYERRORV;
	f->name = tokstr;
	incmdpos = 1;
	yylex();
	if (tok == STRING && !strcmp(tokstr,"in")) {
		f->inflag = 1;
		incmdpos = 0;
		yylex();
		c->args = par_wordlist();
		if (tok != SEPER) YYERRORV;
	} else if (tok == INPAR) {
		f->inflag = 1;
		incmdpos = 0;
		yylex();
		c->args = par_nl_wordlist();
		if (tok != OUTPAR) YYERRORV;
		incmdpos = 1;
		yylex();
	}
	incmdpos = 1;
	while (tok == SEPER) yylex();
	if (tok == DO) {
		yylex();
		f->list = par_list();
		if (tok != DONE) YYERRORV;
		yylex();
	} else if (tok == INBRACE) {
		yylex();
		f->list = par_list();
		if (tok != OUTBRACE) YYERRORV;
		yylex();
	} else if (csh) {
		f->list = par_list();
		if (tok != ZEND) YYERRORV;
		yylex();
	} else if (isset(NOSHORTLOOPS)) {
		YYERRORV;
	} else
		f->list = par_list1();
	c->u.forcmd = f;
}

/*
 * case	: CASE STRING { SEPER } ( "in" | INBRACE )
				{ { SEPER } STRING { BAR STRING } OUTPAR list [ DSEMI ] }
				{ SEPER } ( "esac" | OUTBRACE )
 */
void par_case(c) /**/
Cmd c;
{
struct casecmd **ccp;
int brflag;

	c->type = CCASE;
	incmdpos = 0;
	yylex();
	if (tok != STRING) YYERRORV;
	addnode(c->args,tokstr);
	incmdpos = 1;
	yylex();
	while (tok == SEPER) yylex();
	if (!(tok == STRING && !strcmp(tokstr,"in")) && tok != INBRACE) YYERRORV;
	brflag = (tok == INBRACE);
	incasepat = 1;
	incmdpos = 0;
	yylex();
	ccp = &c->u.casecmd;
	for (;;) {
		char *str;
		struct casecmd *cc;

		*ccp = cc = make_casecmd();
		while (tok == SEPER) yylex();
		if (tok == OUTBRACE) {
			yylex();
			break;
		}
		if (tok != STRING) YYERRORV;
		if (!strcmp(tokstr,"esac")) {
			yylex();
			break;
		}
		str = tokstr;
		yylex();
		while (tok == BAR) {
			char *str2;
			int sl = strlen(str);

			yylex();
			if (tok != STRING) YYERRORV;
			str2 = alloc(sl+strlen(tokstr)+1);
			strcpy(str2,str);
			str2[sl] = Bar;
			strcpy(str2+sl+1,tokstr);
			str = str2;
			yylex();
		}
		if (tok != OUTPAR) YYERRORV;
		incasepat = 0;
		incmdpos = 1;
		yylex();
		cc->pat = str;
		cc->list = par_list();
		ccp = &cc->next;
		if ((tok == ESAC && !brflag) || (tok == OUTBRACE && brflag)) {
			yylex();
			break;
		}
		if (tok != DSEMI) YYERRORV;
		incasepat = 1;
		incmdpos = 0;
		yylex();
	}
	*ccp = NULL;
}

/*
 * if	: { ( IF | ELIF ) { SEPER } ( INPAR list OUTPAR | list )
			{ SEPER } ( THEN list | INBRACE list OUTBRACE | list1 ) }
			[ FI | ELSE list FI | ELSE { SEPER } INBRACE list OUTBRACE ]
			(you get the idea...?)
 */
void par_if(c) /**/
Cmd c;
{
struct ifcmd *i,**ip;
int xtok;

	c->type = CIF;
	ip = &c->u.ifcmd;
	for (;;) {
		xtok = tok;
		yylex();
		if (xtok == FI)
			break;
		while (tok == SEPER) yylex();
		if (xtok == ELSE)
			break;
		if (!(xtok == IF || xtok == ELIF)) YYERRORV;
		*ip = i = make_ifcmd();
		ip = &i->next;
		if (tok == INPAR) {
			yylex();
			i->ifl = par_list();
			if (tok != OUTPAR) YYERRORV;
			incmdpos = 1;
			yylex();
		} else {
			i->ifl = par_list();
			incmdpos = 1;
		}
		while (tok == SEPER) yylex();
		xtok = FI;
		if (tok == THEN) {
			yylex();
			i->thenl = par_list();
			incmdpos = 1;
		} else if (tok == INBRACE) {
			yylex();
			i->thenl = par_list();
			if (tok != OUTBRACE) YYERRORV;
			yylex();
			incmdpos = 1;
			if (tok == SEPER) break;
		} else if (isset(NOSHORTLOOPS)) {
			YYERRORV;
		} else {
			i->thenl = par_list1();
			incmdpos = 1;
			break;
		}
	}
	if (xtok == ELSE) {
		*ip = i = make_ifcmd();
		if (tok == INBRACE) {
			yylex();
			i->thenl = par_list();
			if (tok != OUTBRACE) YYERRORV;
			yylex();
		} else {
			i->thenl = par_list();
			if (tok != FI) YYERRORV;
			yylex();
		}
	}
}

/*
 * while	: ( WHILE | UNTIL ) ( INPAR list OUTPAR | list ) { SEPER }
				( DO list DONE | INBRACE list OUTBRACE | list ZEND )
 */
void par_while(c) /**/
Cmd c;
{
struct whilecmd *w;

	c->type = CWHILE;
	w = c->u.whilecmd = make_whilecmd();
	w->cond = (tok == UNTIL);
	yylex();
	if (tok == INPAR) {
		yylex();
		w->cont = par_list();
		if (tok != OUTPAR) YYERRORV;
		yylex();
	} else {
		w->cont = par_list();
	}
	incmdpos = 1;
	while (tok == SEPER) yylex();
	if (tok == DO) {
		yylex();
		w->loop = par_list();
		if (tok != DONE) YYERRORV;
		yylex();
	} else if (tok == INBRACE) {
		yylex();
		w->loop = par_list();
		if (tok != OUTBRACE) YYERRORV;
		yylex();
	} else if (isset(CSHJUNKIELOOPS)) {
		w->loop = par_list();
		if (tok != ZEND) YYERRORV;
		yylex();
	} else
		YYERRORV;
}

/*
 * repeat	: REPEAT STRING { SEPER } ( DO list DONE | list1 )
 */
void par_repeat(c) /**/
Cmd c;
{
	c->type = CREPEAT;
	incmdpos = 0;
	yylex();
	if (tok != STRING) YYERRORV;
	addnode(c->args,tokstr);
	incmdpos = 1;
	yylex();
	while (tok == SEPER) yylex();
	if (tok == DO) {
		yylex();
		c->u.list = par_list();
		if (tok != DONE) YYERRORV;
		yylex();
	} else {
		c->u.list = par_list1();
	}
}

/*
 * subsh	: ( INPAR | INBRACE ) list ( OUTPAR | OUTBRACE )
 */
void par_subsh(c) /**/
Cmd c;
{
	c->type = (tok == INPAR) ? SUBSH : CURSH;
	yylex();
	c->u.list = par_list();
	if (tok != ((c->type == SUBSH) ? OUTPAR : OUTBRACE)) YYERRORV;
	yylex();
}

/*
 * funcdef	: FUNCTION wordlist [ INOUTPAR ] { SEPER }
 *					( list1 | INBRACE list OUTBRACE )
 */
void par_funcdef(c) /**/
Cmd c;
{
	nocorrect = 1;
	incmdpos = 0;
	yylex();
	c->type = FUNCDEF;
	c->args = newlist();
	while (tok == STRING) {
		if (*tokstr == Inbrace && !tokstr[1]) { tok = INBRACE; break; }
		addnode(c->args,tokstr);
		yylex();
	}
	nocorrect = 0;
	incmdpos = 1;
	if (tok == INOUTPAR) yylex();
	while (tok == SEPER) yylex();
	if (tok == INBRACE) {
		yylex();
		c->u.list = par_list();
		if (tok != OUTBRACE) YYERRORV;
		yylex();
	} else if (isset(NOSHORTLOOPS)) {
		YYERRORV;
	} else
		c->u.list = par_list1();
}

/*
 * time	: TIME sublist2
 */
void par_time(c) /**/
Cmd c;
{
	yylex();
	c->type = ZCTIME;
	c->u.pline = par_sublist2();
}

/*
 * dinbrack	: DINBRACK cond DOUTBRACK
 */
void par_dinbrack(c) /**/
Cmd c;
{
	c->type = COND;
	incond = 1;
	incmdpos = 0;
	yylex();
	c->u.cond = par_cond();
	if (tok != DOUTBRACK) YYERRORV;
	incond = 0;
	incmdpos = 1;
	yylex();
}

/*
 * simple	: { COMMAND | EXEC | NOGLOB | NOCORRECT | DASH }
					{ STRING | ENVSTRING | ENVARRAY wordlist OUTPAR | redir }
					[ INOUTPAR { SEPER } ( list1 | INBRACE list OUTBRACE ) ]
 */
Cmd par_simple(c) /**/
Cmd c;
{
int isnull = 1;

	c->type = SIMPLE;
	for (;;) {
		if (tok == COMMAND) c->flags |= CFLAG_COMMAND;
		else if (tok == EXEC) c->flags |= CFLAG_EXEC;
		else if (tok == NOGLOB) c->flags |= CFLAG_NOGLOB;
		else if (tok == NOCORRECT) nocorrect = 1;
		else if (tok == DASH) c->flags = CFLAG_DASH;
		else break;
		yylex();
	}
	if (tok == AMPER) YYERROR;
	for (;;) {
		if (tok == STRING) {
			incmdpos = 0;
			addnode(c->args,tokstr);
			yylex();
		} else if (tok == ENVSTRING) {
			struct varasg *v = make_varnode();
			v->type = PMFLAG_s;
			equalsplit(v->name = tokstr,&v->str);
			addnode(c->vars,v);
			yylex();
		} else if (tok == ENVARRAY) {
			struct varasg *v = make_varnode();
			int oldcmdpos = incmdpos;
			v->type = PMFLAG_A;
			incmdpos = 0;
			v->name = tokstr;
			yylex();
			v->arr = par_nl_wordlist();
			if (tok != OUTPAR) YYERROR;
			incmdpos = oldcmdpos;
			yylex();
			addnode(c->vars,v);
		} else if (IS_REDIROP(tok)) {
			par_redir(c->redir);
		} else if (tok == INOUTPAR) {
			incmdpos = 1;
			yylex();
			while (tok == SEPER) yylex();
			if (tok == INBRACE) {
				yylex();
				c->u.list = par_list();
				if (tok != OUTBRACE) YYERROR;
				yylex();
			} else if (isset(NOSHORTLOOPS)) {
				YYERROR;
			} else
				c->u.list = par_list1();
			c->type = FUNCDEF;
		} else break;
		isnull = 0;
	}
	if (isnull && !full(c->redir)) return NULL;
	if (full(c->args)) {
		if (underscore)
			free(underscore);
		underscore = ztrdup(getdata(lastnode(c->args)));
		untokenize(underscore);
	}
	incmdpos = 1;
	return c;
}

/*
 * cond	: cond_1 { SEPER } [ DBAR { SEPER } cond ]
 */
Cond par_cond() /**/
{
Cond c,c2;

	c = par_cond_1();
	while (tok == SEPER) yylex();
	if (tok == DBAR) {
		yylex();
		while (tok == SEPER) yylex();
		c2 = make_cond();
		c2->left = c;
		c2->right = par_cond();
		c2->type = COND_OR;
		return c2;
	}
	return c;
}

/*
 * cond_1 : cond_2 { SEPER } [ DAMPER { SEPER } cond_1 ]
 */
Cond par_cond_1() /**/
{
Cond c,c2;

	c = par_cond_2();
	while (tok == SEPER) yylex();
	if (tok == DAMPER) {
		yylex();
		while (tok == SEPER) yylex();
		c2 = make_cond();
		c2->left = c;
		c2->right = par_cond_1();
		c2->type = COND_AND;
		return c2;
	}
	return c;
}

/*
 * cond_2	: BANG cond_2
				| INPAR { SEPER } cond_2 { SEPER } OUTPAR
				| STRING STRING STRING
				| STRING STRING
				| STRING ( INANG | OUTANG ) STRING
 */
Cond par_cond_2() /**/
{
Cond c,c2;
char *s1,*s2,*s3;
int xtok;

	if (tok == BANG) {
		yylex();
		c = par_cond_2();
		c2 = make_cond();
		c2->left = c;
		c2->type = COND_NOT;
		return c2;
	}
	if (tok == INPAR) {
		yylex();
		while (tok == SEPER) yylex();
		c = par_cond();
		while (tok == SEPER) yylex();
		if (tok != OUTPAR) YYERROR;
		yylex();
		return c;
	}
	if (tok != STRING) YYERROR;
	s1 = tokstr;
	yylex();
	xtok = tok;
	if (tok == INANG || tok == OUTANG) {
		yylex();
		if (tok != STRING) YYERROR;
		s3 = tokstr;
		yylex();
		c = make_cond();
		c->left = s1;
		c->right = s3;
		c->type = (xtok == INANG) ? COND_STRLT : COND_STRGTR;
		c->types[0] = c->types[1] = NT_STR;
		return c;
	}
	if (tok != STRING) YYERROR;
	s2 = tokstr;
	yylex();
	if (tok == STRING) {
		s3 = tokstr;
		yylex();
		return par_cond_triple(s1,s2,s3);
	} else
		return par_cond_double(s1,s2);
}

/*
 * redir	: ( OUTANG | ... | TRINANG ) STRING
 */
void par_redir(l) /**/
Lklist l;
{
char *toks;
struct redir *fn = allocnode(N_REDIR);
int mergerror = 0;
int oldcmdpos,oldnc;

	oldcmdpos = incmdpos;
	incmdpos = 0;
	oldnc = nocorrect;
	if (tok != INANG) nocorrect = 1;
	fn->type = redirtab[tok-OUTANG];
	fn->fd1 = tokfd;
	yylex();
	if (tok != STRING && tok != ENVSTRING) YYERRORV;
	toks = tokstr;
	incmdpos = oldcmdpos;
	nocorrect = oldnc;
	yylex();

/* assign default fd */

	if (fn->fd1 == -1)
		fn->fd1 = IS_READFD(fn->type) ? 0 : 1;

/* > >(...) or < <(...) */

	if ((*toks == Inang || *toks == Outang) && toks[1] == Inpar) {
		if ((fn->type & ~1) == WRITE) fn->type = OUTPIPE;
		else if (fn->type == READ) fn->type = INPIPE;
		else YYERRORV;
		fn->name = toks;

/* <<[-] name */

	} else if (fn->type == HEREDOC || fn->type == HEREDOCDASH) {
		char tbuf[256], *tlin;
		int tsiz = 0, l;
		/* Save the rest of the current line for later tokenization */
		if (!isnewlin) {
			while (hgets(tbuf, 256) != NULL) {
				l = strlen(tbuf);
				if (tsiz == 0) {
					tlin = ztrdup(tbuf); /* Test for failure? */
					tsiz = l;
				} else {
					tlin = realloc(tlin,tsiz+l+1); /* Test for failure? */
					strcpy(&tlin[tsiz], tbuf);
					tsiz += l;
				}
				if (tbuf[l-1] == '\n') break;
			}
		}
		/* Now grab the document */
		fn->name = gethere(toks,fn->type);
		fn->type = HERESTR;
		/* Put back the saved line to resume tokenizing */
		if (tsiz > 0) {
			hungets(tlin);
			free(tlin);
		}

/* >& name or >>& name */

	} else if (IS_ERROR_REDIR(fn->type) && getfdstr(toks) == FD_WORD) {
		mergerror = 1;
		fn->name = toks;
		fn->type = UN_ERROR_REDIR(fn->type);

/* >>& and >>&! are only valid with a name after them */

	} else if (fn->type == ERRAPP || fn->type == ERRAPPNOW) {
		YYERRORV;

/* >& # */

	} else if (fn->type == MERGE || fn->type == MERGEOUT) {
		fn->fd2 = getfdstr(toks);
		if (fn->fd2 == FD_CLOSE) fn->type = CLOSE;
		else if (fn->fd2 == FD_WORD) fn->fd2 = (fn->type==MERGEOUT) ? 1 : 0;
	} else
		fn->name = toks;
	addnode(l,fn);
	if (mergerror)
		{
		struct redir *fe = allocnode(N_REDIR);

		fe->fd1 = 2;
		fe->fd2 = fn->fd1;
		fe->type = MERGEOUT;
		addnode(l,fe);
		}
}

/*
 * wordlist	: { STRING }
 */
Lklist par_wordlist() /**/
{
Lklist l;

	l = newlist();
	while (tok == STRING) {
		addnode(l,tokstr);
		yylex();
	}
	return l;
}

/*
 * nl_wordlist	: { STRING | SEPER }
 */
Lklist par_nl_wordlist() /**/
{
Lklist l;

	l = newlist();
	while (tok == STRING || tok == SEPER) {
		if (tok != SEPER)
			addnode(l,tokstr);
		yylex();
	}
	return l;
}

/* get fd associated with str */

int getfdstr(s) /**/
char *s;
{
	if (s[1]) return FD_WORD;
	if (idigit(*s)) return *s-'0';
	if (*s == 'p') return FD_COPROC;
	if (*s == '-') return FD_CLOSE;
	return FD_WORD;
}

Cond par_cond_double(a,b) /**/
char *a;char *b;
{
Cond n = make_cond();

	if (a[0] != '-' || !a[1] || a[2])
		{
		zerr("parse error: condition expected: %s",a,0);
		return NULL;
		}
	n->left = b;
	n->type = a[1];
	n->types[0] = n->types[1] = NT_STR;
	return n;
}

Cond par_cond_triple(a,b,c) /**/
char *a;char *b;char *c;
{
Cond n = make_cond();
static char *condstrs[] = {
	"nt","ot","ef","eq","ne","lt","gt","le","ge",NULL
	};
int t0;

	if ((b[0] == Equals || b[0] == '=') && !b[1])
		n->type = COND_STREQ;
	else if (b[0] == '!' && b[1] == '=' && !b[2])
		n->type = COND_STRNEQ;
	else if (b[0] == '-')
		{
		for (t0 = 0; condstrs[t0]; t0++)
			if (!strcmp(condstrs[t0],b+1))
				break;
		if (condstrs[t0])
			n->type = t0+COND_NT;
		else
			zerr("unrecognized condition: %s",b,0);
		}
	else
		zerr("condition expected: %s",b,0);
	n->left = a;
	n->right = c;
	n->types[0] = n->types[1] = NT_STR;
	return n;
}

void yyerror() /**/
{
int t0;

	for (t0 = 0; t0 != 20; t0++)
		if (!yytext[t0] || yytext[t0] == '\n' || yytext[t0] == HISTSPACE)
			break;
	if (t0 == 20)
		zerr("parse error near `%l...'",yytext,20);
	else if (t0)
		zerr("parse error near `%l'",yytext,t0);
	else
		zerr("parse error",NULL,0);
}

