/*
 *
 * text.c - textual representations of syntax trees
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

static char *tptr,*tbuf,*tlim;
static int tsiz,tindent,tnewlins;

/* add a character to the text buffer */

void taddchr(c) /**/
int c;
{
	*tptr++ = c;
	if (tptr == tlim) {
		if (!tbuf) { tptr--; return; }
		tbuf = realloc(tbuf,tsiz *= 2);
		tlim = tbuf+tsiz;
		tptr = tbuf+tsiz/2;
	}
}

/* add a string to the text buffer */

void taddstr(s) /**/
char *s;
{
int sl = strlen(s);

	while (tptr+sl >= tlim) {
		int x = tptr-tbuf;

		if (!tbuf) return;
		tbuf = realloc(tbuf,tsiz *= 2);
		tlim = tbuf+tsiz;
		tptr = tbuf+x;
	}
	strcpy(tptr,s);
	tptr += sl;
}

/* add an integer to the text buffer */

void taddint(x) /**/
int x;
{
char buf[10];

	sprintf(buf,"%d",x);
	taddstr(buf);
}

/* add a newline, or something equivalent, to the text buffer */

void taddnl() /**/
{
int t0;

	if (tnewlins)
		{
		taddchr('\n');
		for (t0 = 0; t0 != tindent; t0++)
			taddchr('\t');
		}
	else
		taddstr("; ");
}

/* get a permanent textual representation of n */

char *getpermtext(n) /**/
struct node *n;
{
	tnewlins = 1;
	tbuf = zalloc(tsiz = 32);
	tptr = tbuf;
	tlim = tbuf+tsiz;
	tindent = 1;
	gettext2(n);
	*tptr = '\0';
	untokenize(tbuf);
	return tbuf;
}

/* get a representation of n in a job text buffer */

char *getjobtext(n) /**/
struct node *n;
{
static char jbuf[JOBTEXTSIZE];

	tnewlins = 0;
	tbuf = NULL;
	tptr = jbuf;
	tlim = tptr+JOBTEXTSIZE-1;
	tindent = 1;
	gettext2(n);
	*tptr = '\0';
	untokenize(jbuf);
	return jbuf;
}

#define gt2(X) gettext2((struct node *) (X))

/*
	"gettext2" or "type checking and how to avoid it"
	an epic function by Paul Falstad
*/

#define _Cond(X) ((Cond) (X))
#define _Cmd(X) ((Cmd) (X))
#define _Pline(X) ((Pline) (X))
#define _Sublist(X) ((Sublist) (X))
#define _List(X) ((List) (X))
#define _casecmd(X) ((struct casecmd *) (X))
#define _ifcmd(X) ((struct ifcmd *) (X))
#define _whilecmd(X) ((struct whilecmd *) (X))

void gettext2(n) /**/
struct node *n;
{
Cmd nn;
Cond nm;

	if (!n)
		return;
	switch (n->type)
		{
		case N_LIST:
			gt2(_List(n)->left);
			if (_List(n)->type == ASYNC)
				taddstr(" &");
			simplifyright(_List(n));
			if (_List(n)->right)
				{
				if (tnewlins)
					taddnl();
				else
					taddstr((_List(n)->type == ASYNC) ? " " : "; ");
				gt2(_List(n)->right);
				}
			break;
		case N_SUBLIST:
			if (_Sublist(n)->flags & PFLAG_NOT)
				taddstr("! ");
			if (_Sublist(n)->flags & PFLAG_COPROC)
				taddstr("coproc ");
			gt2(_Sublist(n)->left);
			if (_Sublist(n)->right)
				{
				taddstr((_Sublist(n)->type == ORNEXT) ? " || " : " && ");
				gt2(_Sublist(n)->right);
				}
			break;
		case N_PLINE:
			gt2(_Pline(n)->left);
			if (_Pline(n)->type == PIPE)
				{
				taddstr(" | ");
				gt2(_Pline(n)->right);
				}
			break;
		case N_CMD:
			nn = _Cmd(n);
			if (nn->flags & CFLAG_EXEC)
				taddstr("exec ");
			if (nn->flags & CFLAG_COMMAND)
				taddstr("command ");
			switch (nn->type)
				{
				case SIMPLE:
					getsimptext(nn);
					break;
				case SUBSH:
					taddstr("( ");
					tindent++;
					gt2(nn->u.list);
					tindent--;
					taddstr(" )");
					break;
				case ZCTIME:
					taddstr("time ");
					tindent++;
					gt2(nn->u.pline);
					tindent--;
					break;
				case FUNCDEF:
					taddlist(nn->args);
					taddstr(" () {");
					tindent++;
					taddnl();
					gt2(nn->u.list);
					tindent--;
					taddnl();
					taddstr("}");
					break;
				case CURSH:
					taddstr("{ ");
					tindent++;
					gt2(nn->u.list);
					tindent--;
					taddstr(" }");
					break;
				case CFOR:
				case CSELECT:
					taddstr((nn->type == CFOR) ? "for " : "select ");
					taddstr(nn->u.forcmd->name);
					if (nn->u.forcmd->inflag)
						{
						taddstr(" in ");
						taddlist(nn->args);
						}
					taddnl();
					taddstr("do");
					tindent++;
					taddnl();
					gt2(nn->u.forcmd->list);
					taddnl();
					tindent--;
					taddstr("done");
					break;
				case CIF:
					gt2(nn->u.ifcmd);
					taddstr("fi");
					break;
				case CCASE:
					taddstr("case ");
					taddlist(nn->args);
					taddstr(" in");
					tindent++;
					taddnl();
					gt2(nn->u.casecmd);
					tindent--;
					if (tnewlins)
						taddnl();
					else
						taddchr(' ');
					taddstr("esac");
					break;
				case COND:
					taddstr("[[ ");
					gt2(nn->u.cond);
					taddstr(" ]]");
					break;
				case CREPEAT:
					taddstr("repeat ");
					taddlist(nn->args);
					taddnl();
					taddstr("do");
					tindent++;
					taddnl();
					gt2(nn->u.list);
					tindent--;
					taddnl();
					taddstr("done");
					break;
				case CWHILE:
					gt2(nn->u.whilecmd);
					break;
				}
			getredirs(nn);
			break;
		case N_COND:
			nm = _Cond(n);
			switch (nm->type)
				{
				case COND_NOT:
					taddstr("! ");
					gt2(nm->left);
					break;
				case COND_AND:
					taddstr("( ");
					gt2(nm->left);
					taddstr(" && ");
					gt2(nm->right);
					taddstr(" )");
					break;
				case COND_OR:
					taddstr("( ");
					gt2(nm->left);
					taddstr(" || ");
					gt2(nm->right);
					taddstr(" )");
					break;
				default:
					{
					static char *c1[] = {
						" = "," != "," < "," > "," -nt "," -ot "," -ef "," -eq ",
						" -ne "," -lt "," -gt "," -le "," -ge "
						};
					if (nm->right)
						taddstr(nm->left);
					if (nm->type <= COND_GE)
						taddstr(c1[nm->type-COND_STREQ]);
					else
						{
						char c2[5];
						c2[0] = ' '; c2[1] = '-';
						c2[2] = nm->type;
						c2[3] = ' '; c2[4] = '\0';
						taddstr(c2);
						}
					taddstr((nm->right) ? nm->right : nm->left);
					}
					break;
				}
			break;
		case N_CASE:
			taddstr(_casecmd(n)->pat);
			taddstr(") ");
			tindent++;
			gt2(_casecmd(n)->list);
			tindent--;
			taddstr(";;");
			if (tnewlins)
				taddnl();
			else
				taddchr(' ');
			gt2(_casecmd(n)->next);
			break;
		case N_IF:
			if (_ifcmd(n)->ifl)
				{
				taddstr("if ");
				tindent++;
				gt2(_ifcmd(n)->ifl);
				tindent--;
				taddnl();
				taddstr("then");
				}
			else
				taddchr('e');
			tindent++;
			taddnl();
			gt2(_ifcmd(n)->thenl);
			tindent--;
			taddnl();
			if (_ifcmd(n)->next)
				{
				taddstr("els");
				gt2(_ifcmd(n)->next);
				}
			break;
		case N_WHILE:
			taddstr((_whilecmd(n)->cond) ? "until " : "while ");
			tindent++;
			gt2(_whilecmd(n)->cont);
			tindent--;
			taddnl();
			taddstr("do");
			tindent++;
			taddnl();
			gt2(_whilecmd(n)->loop);
			tindent--;
			taddnl();
			taddstr("done");
			break;
		}
}

void getsimptext(cmd) /**/
Cmd cmd;
{
Lknode n;

	for (n = firstnode(cmd->vars); n; incnode(n))
		{
		struct varasg *v = getdata(n);

		taddstr(v->name);
		taddchr('=');
		if ((v->type & PMTYPE) == PMFLAG_A)
			{
			taddchr('(');
			taddlist(v->arr);
			taddstr(") ");
			}
		else
			{
			taddstr(v->str);
			taddchr(' ');
			}
		}
	taddlist(cmd->args);
}

void getredirs(cmd) /**/
Cmd cmd;
{
Lknode n;
static char *fstr[] = {
	">",">!",">>",">>!",">&",">&!",">>&",">>&!","<","<<",
	"<<-","<<<","<&",">&-","..",".."
	};

	taddchr(' ');
	for (n = firstnode(cmd->redir); n; incnode(n))
		{
		struct redir *f = getdata(n);

		switch(f->type)
			{
			case WRITE: case WRITENOW: case APP: case APPNOW: case READ:
			case HERESTR:
				if (f->fd1 != ((f->type == READ) ? 0 : 1))
					taddchr('0'+f->fd1);
				taddstr(fstr[f->type]);
				taddchr(' ');
				taddstr(f->name);
				taddchr(' ');
				break;
			case MERGE: case MERGEOUT:
				if (f->fd1 != ((f->type == MERGEOUT) ? 1 : 0))
					taddchr('0'+f->fd1);
				taddstr(fstr[f->type]);
				taddchr(' ');
				taddint(f->fd2);
				taddchr(' ');
				break;
			case CLOSE:
				taddchr(f->fd1+'0');
				taddstr(">&- ");
				break;
			case INPIPE:
			case OUTPIPE:
				if (f->fd1 != ((f->type == INPIPE) ? 0 : 1))
					taddchr('0'+f->fd1);
				taddstr((f->type == INPIPE) ? "< " : "> ");
				taddstr(f->name);
				taddchr(' ');
				break;
			}
		}
	tptr--;
}

void taddlist(l) /**/
Lklist l;
{
Lknode n;

	for (n = firstnode(l); n; incnode(n))
		{
		taddstr(getdata(n));
		taddchr(' ');
		}
	tptr--;
}
