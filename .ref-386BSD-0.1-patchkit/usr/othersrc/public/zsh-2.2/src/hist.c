/*
 *
 * hist.c - history expansion
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

#define HEAPSIZE 4096

struct hp {
	Hp next;
	char *pool,*ptr;
	int free,histno;
};

static Hp hp_lit, hp_lex;
static Histent curhistent;

static int lastc;

/* add a character to the current history word */

void hwaddc(c) /**/
int c;
{
	if (hlastw && hline && (!(errflag || lexstop) || c == HISTSPACE)) {
		if (c == '!' && unset(NOBANGHIST)) hwaddc('\\');
		*hptr++ = c;
		if (hptr-hline >= hlinesz) {
			int ll,flag = 0,oldsiz = hlinesz;

			ll = hptr-hlastw;
			if (curhistent->lex == hline) flag = 1;
			hline = hp_realloc(&hp_lex,hline,oldsiz,hlinesz = oldsiz+16);
			if (flag) curhistent->lex = hline;
			hptr = hline+oldsiz;
			hlastw = hptr-ll;
		}
	}
}

#define habort() { errflag = lexstop = 1; return ' '; }

/* get a character after performing history substitution */

int hgetc() /**/
{
int c,ev,farg,larg,argc,marg = -1,cflag = 0,bflag = 0;
char buf[256],*ptr;
char *sline,*eline;

tailrec:
	c = hgetch();
	if (stophist || alstackind)
		{
		hwaddc(c);
		return c;
		}
	if (isfirstch && c == hatchar)
		{
		isfirstch = 0;
		hungetch(hatchar);
		hungets(":s");
		c = bangchar;
		goto hatskip;
		}
	if (c != ' ')
		isfirstch = 0;
	if (c == '\\') {
		int g = hgetch();
		
		if (g != bangchar)
			hungetch(g);
		else {
			hwaddc(bangchar);
			return bangchar;
		}
	}
	if (c != bangchar)
		{
		hwaddc(c);
		return c;
		}
hatskip:
	*hptr = '\0';
	if ((c = hgetch()) == '{')
		{
		bflag = cflag = 1;
		c = hgetch();
		}
	if (c == '\"')
		{
		stophist = 1;
		goto tailrec;
		}
	if (!cflag && inblank(c) || c == '=' || c == '(' || lexstop)
		{
		if (lexstop)
			lexstop = 0;
		else
			hungetch(c);
		hwaddc(bangchar);
		return bangchar;
		}
	cflag = 0;
	ptr = buf;

	/* get event number */

	if (c == '?')
		{
		for(;;)
			{
			c = hgetch();
			if (c == '?' || c == '\n' || lexstop)
				break;
			else
				*ptr++ = c;
			}
		if (c != '\n' && !lexstop)
			c = hgetch();
		*ptr = '\0';
		ev = hconsearch(hsubl = ztrdup(buf),&marg);
		if (ev == -1)
			{
			herrflush();
			zerr("no such event: %s",buf,0);
			habort();
			}
		}
	else
		{
		int t0;
 
		for (;;)
			{
			if (inblank(c) || c == ';' || c == ':' || c == '^' || c == '$' ||
					c == '*' || c == '%' || c == '}' || lexstop)
				break;
			if (ptr != buf) {
				if (c == '-') break;
				if ((idigit(buf[0]) || buf[0] == '-') && !idigit(c)) break;
			}
			*ptr++ = c;
			if (c == '#' || c == bangchar)
				{
				c = hgetch();
				break;
				}
			c = hgetch();
			}
		*ptr = 0;
		if (!*buf)
			ev = defev;
		else if (t0 = atoi(buf))
			ev = (t0 < 0) ? curhist+t0 : t0;
		else if (*buf == bangchar)
			ev = curhist-1;
		else if (*buf == '#')
			ev = curhist;
		else if ((ev = hcomsearch(buf)) == -1)
			{
			zerr("event not found: %s",buf,0);
			while (c != '\n' && !lexstop)
				c = hgetch();
			habort();
			}
		}

	/* get the event */

	if (!(eline = getevent(defev = ev)))
		habort();

	/* extract the relevant arguments */

	argc = getargc(eline);
	if (c == ':')
		{
		cflag = 1;
		c = hgetch();
		}
	if (c == '*')
		{
		farg = 1;
		larg = argc;
		cflag = 0;
		}
	else
		{
		hungetch(c);
		larg = farg = getargspec(argc,marg);
		if (larg == -2)
			habort();
		if (farg != -1)
			cflag = 0;
		c = hgetch();
		if (c == '*')
			{
			cflag = 0;
			larg = argc;
			}
		else if (c == '-')
			{
			cflag = 0;
			larg = getargspec(argc,marg);
			if (larg == -2)
				habort();
			if (larg == -1)
				larg = argc-1;
			}
		else
			hungetch(c);
		}
	if (farg == -1)
		farg = 0;
	if (larg == -1)
		larg = argc;
	if (!(sline = getargs(eline,farg,larg)))
		habort();

	/* do the modifiers */

	for(;;)
		{
		c = (cflag) ? ':' : hgetch();
		cflag = 0;
		if (c == ':')
			{
			int gbal = 0;
		
			if ((c = hgetch()) == 'g')
				{
				gbal = 1;
				c = hgetch();
				}
			switch(c)
				{
				case 'p':
					histdone = HISTFLAG_DONE|HISTFLAG_NOEXEC;
					break;
				case 'h':
					if (!remtpath(&sline))
						{
						herrflush();
						zerr("modifier failed: h",NULL,0);
						habort();
						}
					break;
				case 'e':
					if (!rembutext(&sline))
						{
						herrflush();
						zerr("modifier failed: e",NULL,0);
						habort();
						}
					break;
				case 'r':
					if (!remtext(&sline))
						{
						herrflush();
						zerr("modifier failed: r",NULL,0);
						habort();
						}
					break;
				case 't':
					if (!remlpaths(&sline))
						{
						herrflush();
						zerr("modifier failed: t",NULL,0);
						habort();
						}
					break;
				case 's':
					{
					int del;
					char *ptr1,*ptr2;
				
					del = hgetch();
					ptr1 = hdynread2(del);
					if (!ptr1)
						habort();
					ptr2 = hdynread2(del);
					if (strlen(ptr1))
						{
						if (hsubl)
							free(hsubl);
						hsubl = ptr1;
						}
					if (hsubr)
						free(hsubr);
					hsubr = ptr2;
					}
				case '&':
					if (hsubl && hsubr)
						subst(&sline,hsubl,hsubr,gbal);
					else
						{
						herrflush();
						zerr("no previous substitution with &",NULL,0);
						habort();
						}
					break;
				case 'q':
					quote(&sline);
					break;
				case 'x':
					quotebreak(&sline);
					break;
				case 'l':
					downcase(&sline);
					break;
				case 'u':
					upcase(&sline);
					break;
				default:
					herrflush();
					zerr("illegal modifier: %c",NULL,c);
				habort();
				break;
			}
		}
	else
		{
		if (c != '}' || !bflag)
			hungetch(c);
		if (c != '}' && bflag)
			{
			zerr("'}' expected",NULL,0);
			habort();
			}
		break;
		}
	}

	/* stuff the resulting string in the input queue and start over */

	lexstop = 0;
	if (alstackind != MAXAL)
		{
		hungets(HISTMARK);
		alstack[alstackind++] = NULL;
		}
	for (ptr = sline; *ptr; ptr++) {
		if (ptr[0] == '\\' && ptr[1] == '!') chuck(ptr);
	}
	hungets(sline);
	histdone |= HISTFLAG_DONE;
	if (isset(HISTVERIFY)) histdone |= HISTFLAG_NOEXEC|HISTFLAG_RECALL;
	goto tailrec;
}

/* reset the alias stack for lexrestore () */

void clearalstack() /**/
{
Alias ix;

	while (alstackind)
		{
		ix = alstack[--alstackind];
		ix->inuse = 0;
		}
}

/* get a character without history expansion */

int hgetch() /**/
{
unsigned char *line,*pmpt,*pmpt2 = NULL;
int plen;

start:
	if (inbufct)
		{
		inbufct--;
		if ((lastc = *inbufptr++) == ALPOP)
			{
			Alias ix;
			char *t;

			if (!alstackind)
				{
				zerr("alias stack underflow",NULL,0);
				errflag = lexstop = 1;
				return lastc = ' ';
				}
			ix = alstack[--alstackind];
			if (ix)
				{
				ix->inuse = 0;
				t = ix->text;
				if (*t && t[strlen(t)-1] == ' ')
					alstat = ALSTAT_MORE;
				else
					alstat = ALSTAT_JUNK;
				}
			goto start;
			}
		if (itok(lastc))
			goto start;
		return lastc;
		}
	if (strin || errflag)
		{
		lexstop = 1;
		return lastc = ' ';
		}
	if (interact && isset(SHINSTDIN))
		if (!isfirstln)
			pmpt = (unsigned char *)putprompt(prompt2,&plen);
		else
			{
			int foo;

			pmpt = (unsigned char *)putprompt(prompt,&plen);
			pmpt2 = (unsigned char *)((rprompt) ? putprompt(rprompt,&foo) : NULL);
			}
	if (!(interact && isset(SHINSTDIN) && SHTTY != -1 && isset(USEZLE))) {
		char *lbuf;
		if (interact && isset(SHINSTDIN))
			write(2,pmpt,strlen((char *) pmpt));
		line = (unsigned char *)fgets(lbuf = zalloc(256),256,bshin);
		if (!line) free(lbuf);
	} else
		line = zleread(pmpt,pmpt2,plen);
	if (!line) {
		lexstop = 1;
		return lastc = ' ';
	}
	if (errflag) {
		free(line);
		lexstop = errflag = 1;
		return lastc = ' ';
	}
	if (interact && isset(SHINSTDIN)) {
		char *s = curhistent->lit;
		curhistent->lit = hp_concat(s,(char*)line);
	}
	if (isfirstln) spaceflag = *line == ' ';
	if (isset(VERBOSE)) {
		fputs((char *) line,stderr);
		fflush(stderr);
	}
	if (*line && line[strlen((char *) line)-1] == '\n')
		{
		lineno++;
		if (interact && isset(SUNKEYBOARDHACK) && isset(SHINSTDIN) && 
				SHTTY != -1 && *line && line[1] &&
				line[strlen((char *) line)-2] == '`')
			{
			int ct;
			unsigned char *ptr;

			for (ct = 0, ptr = line; *ptr; ptr++)
				if (*ptr == '`')
					ct++;
			if (ct & 1)
				{
				ptr[-2] = '\n';
				ptr[-1] = '\0';
				}
			}
		}
	isfirstch = 1;
	hungets((char*)line);
	free(line);
	goto start;
}

/* Read one line of at most n-1 chars from the input queue */

char *hgets(buf, n) /**/
char *buf;int n;
{
int l;

	for (l = 0; l < n-1; l++)
		if ((buf[l] = hgetch()) == '\n' || lexstop)
			break;
	buf[l+(lexstop?0:1)] = 0;

	return (!lexstop || l) ? buf : NULL;
}

/* put a string in the input queue */

void hungets(str) /**/
char *str;
{
int slen = strlen(str);

/* shrink inbuf if it gets too big */

	if (!inbufct && inbufsz > 65536)
		{
		free(inbuf);
		inbuf = zalloc(inbufsz = 256);
		inbufptr = inbuf+inbufsz;
		inbufct = 0;
		}
	if (slen+inbufct > inbufsz)
		{
		char *x;

		while (slen+inbufct > inbufsz)
			inbufsz *= 4;
		x = zalloc(inbufsz);
		memcpy(x+inbufsz-inbufct,inbufptr,inbufct);
		inbufptr = x+inbufsz-inbufct;
		free(inbuf);
		inbuf = x;
		}
	memcpy(inbufptr -= slen,str,slen);
	inbufct += slen;
}

/* unget a char and remove it from hline */

void hungetc(c) /**/
int c;
{
	if (lexstop)
		return;
	if (hlastw) {
		if (hlastw == hptr)
			zerr("hungetc attempted at buffer start",NULL,0);
		else {
			hptr--;
			if (*hptr == '!' && unset(NOBANGHIST)) hptr--;
		}
	}
	hungetch(c);
}

void hungetch(c) /**/
int c;
{
	if (lexstop)
		return;
	if (inbufct == inbufsz)
		{
		hungets(" ");
		*inbufptr = c;
		}
	else
		{
		*--inbufptr = c;
		inbufct++;
		}
}

/* begin reading a string */

void strinbeg() /**/
{
	strin = 1;
	hbegin();
	lexinit();
}

/* done reading a string */

void strinend() /**/
{
	strin = 0;
	isfirstch = 1;
	histdone = 0;
	hend();
}

/* stuff a whole file into the input queue and print it */

int stuff(fn) /**/
char *fn;
{
FILE *in;
char *buf;
int len;

	if (!(in = fopen(fn,"r")))
		{
		zerr("can't open %s",fn,0);
		return 1;
		}
	fseek(in,0,2);
	len = ftell(in);
	fseek(in,0,0);
	buf = alloc(len+1);
	if (!(fread(buf,len,1,in)))
		{
		zerr("read error on %s",fn,0);
		fclose(in);
		free(buf);
		return 1;
		}
	fclose(in);
	buf[len] = '\0';
	fwrite(buf,len,1,stdout);
	hungets(buf);
	return 0;
}

/* flush input queue */

void hflush() /**/
{
	inbufptr += inbufct;
	inbufct = 0;
}

/* initialize the history mechanism */

void hbegin() /**/
{
	isfirstln = isfirstch = 1;
	histremmed = errflag = histdone = spaceflag = 0;
	stophist = isset(NOBANGHIST) || unset(SHINSTDIN);
	lithist = isset(HISTLIT);
	hline = hptr = hp_alloc(&hp_lex,hlinesz = 16);
	curhistent = gethistent(curhist);
	if (!curhistent->ftim) curhistent->ftim = time(NULL);
	if (interact && isset(SHINSTDIN) && !strin) {
		inittty();
		defev = curhist++;
		if (curhist-histsiz >= 0) gethistent(curhist-histsiz)->lex = NULL;
		if (curhist-lithistsiz >= 0) gethistent(curhist-lithistsiz)->lit = NULL;
		curhistent = gethistent(curhist);
		hp_purge(hp_lex,curhist-histsiz);
		hp_purge(hp_lit,curhist-lithistsiz);
		curhistent->lex = hline;
		*(curhistent->lit = hp_alloc(&hp_lit,1)) = '\0';
	} else
		histremmed = 1;
}

void inittty() /**/
{
	attachtty(mypgrp);
}

/* say we're done using the history mechanism */

int hend() /**/
{
int flag,save = 1;
Histent he;

	if (!hline)
		return 1;
	if (!interact || strin || unset(SHINSTDIN)) {
		hp_free(hp_lex,hline,hlinesz);
		return 1;
	}
	flag = histdone;
	histdone = 0;
	if (hptr < hline+2)
		save = 0;
	else {
		char *s,*t;

		s = curhistent->lit;
		if (*s && *(t = s+strlen(s)-1) == HISTSPACE) *t = '\0';
		hptr[-1] = '\0';
		if (hptr[-2] == '\n')
			if (hline[1]) {
				if (hptr[-3] == HISTSPACE) hptr[-3] = '\0';
			} else save = 0;
		he = gethistent(curhist-1);
		if (!strcmp(hline,"\n") ||
				(isset(HISTIGNOREDUPS) && he->lex && !strcmp(he->lex,hline)) ||
				(isset(HISTIGNORESPACE) && spaceflag))
			save = 0;
	}
	if (flag & (HISTFLAG_DONE|HISTFLAG_RECALL)) {
		char *ptr,*p;
		p = ptr = ztrdup(hline);
		for (;*p;p++) if (*p == HISTSPACE) *p = ' ';
		if ((flag & (HISTFLAG_DONE|HISTFLAG_RECALL)) == HISTFLAG_DONE) {
			fprintf(stderr,"%s\n",ptr);
			fflush(stderr);
		}
		if (flag & HISTFLAG_RECALL) {
			permalloc();
			pushnode(bufstack,ptr);
			lastalloc();
			save = 0;
		} else free(ptr);
	}
	curhistent->stim = time(NULL);
	curhistent->ftim = 0L;
	if (!save) remhist();
	if (hline && !curhistent->lex) hp_free(hp_lex,hline,hlinesz);
	hline = NULL;
	return !(flag & HISTFLAG_NOEXEC || errflag);
}

/* remove the current line from the history List */

void remhist() /**/
{
	if (!histremmed) { histremmed = 1; curhist--; }
}

/* begin a word */

void hwbegin() /**/
{
	hlastw = hptr;
}

/* add a word to the history List */

char *hwadd() /**/
{
char *ret = hlastw;

	if (hlastw && hline)
		{
		hwaddc(HISTSPACE);
		if (alstackind || strin)
			if (!(alstackind == 1 && !alstack[0]))
				hptr = hlastw;
		}
	if (alstat == ALSTAT_JUNK)
		alstat = 0;
	return ret;
}

/* get an argument specification */

int getargspec(argc,marg) /**/
int argc;int marg;
{
int c,ret = -1;
 
	if ((c = hgetch()) == '0')
		return 0;
	if (idigit(c))
		{
		ret = 0;
		while (idigit(c))
			{
			ret = ret*10+c-'0';
			c = hgetch();
			}
		hungetch(c);
		}
	else if (c == '^')
		ret = 1;
	else if (c == '$')
		ret = argc;
	else if (c == '%')
		{
		if (marg == -1)
			{
			herrflush();
			zerr("%% with no previous word matched",NULL,0);
			return -2;
			}
		ret = marg;
		}
	else
		hungetch(c);
	return ret;
}

/* do ?foo? search */

int hconsearch(str,marg) /**/
char *str;int *marg;
{
int t0,t1 = 0;
char *s,*hs;

	for (t0 = curhist-1; hs = quietgetevent(t0); t0--)
		if (s = ztrstr(hs,str)) {
			while (s != hs) if (*s-- == HISTSPACE) t1++;
			*marg = t1;
			return t0;
		}
	return -1;
}

/* do !foo search */

int hcomsearch(str) /**/
char *str;
{
int t0;
char *hs;

	for (t0 = curhist-1; hs = quietgetevent(t0); t0--)
		if (!strncmp(hs,str,strlen(str))) return t0;
	return -1;
}

/* various utilities for : modifiers */

int remtpath(junkptr) /**/
char **junkptr;
{
char *str = *junkptr,*cut;
 
	if (cut = strrchr(str,'/')) {
		if (str != cut) *cut = '\0';
		else str[1] = '\0';
		return 1;
	}
	return 0;
}
 
int remtext(junkptr) /**/
char **junkptr;
{
char *str = *junkptr,*cut;
 
	if ((cut = strrchr(str,'.')) && cut != str)
		{
		*cut = '\0';
		return 1;
		}
	return 0;
}
 
int rembutext(junkptr) /**/
char **junkptr;
{
char *str = *junkptr,*cut;
 
	if ((cut = strrchr(str,'.')) && cut != str)
		{
		*junkptr = strdup(cut+1);  /* .xx or xx? */
		return 1;
		}
	return 0;
}
 
int remlpaths(junkptr) /**/
char **junkptr;
{
char *str = *junkptr,*cut;
 
	if (cut = strrchr(str,'/'))
		{
		*cut = '\0';
		*junkptr = strdup(cut+1);
		return 1;
		}
	return 0;
}

int makeuppercase(junkptr) /**/
char **junkptr;
{
char *str = *junkptr;

	for (; *str; str++)
		*str = tuupper(*str);
	return 1;
}

int makelowercase(junkptr) /**/
char **junkptr;
{
char *str = *junkptr;

	for (; *str; str++)
		*str = tulower(*str);
	return 1;
}

void subst(strptr,in,out,gbal) /**/
char **strptr;char *in;char *out;int gbal;
{
char *str = *strptr,*cut,*sptr;
int off;

	while (cut = (char *) ztrstr(str,in)) {
		*cut = '\0';
		sptr = convamps(out,in);
		off = cut-*strptr+strlen(sptr);
		cut += strlen(in);
		*strptr = tricat(*strptr,sptr,cut);
		if (gbal) {
			str = (char *) *strptr+off;
			continue;
		}
		break;
	}
}
 
char *convamps(out,in) /**/
char *out;char *in;
{
char *ptr,*ret,*pp;
int slen,inlen = strlen(in);
 
	for (ptr = out, slen = 0; *ptr; ptr++,slen++)
		if (*ptr == '\\')
			ptr++;
		else if (*ptr == '&')
			slen += inlen-1;
	ret = pp = alloc(slen+1);
	for (ptr = out; *ptr; ptr++)
		if (*ptr == '\\')
			*pp++ = *++ptr;
		else if (*ptr == '&')
			{
			strcpy(pp,in);
			pp += inlen;
			}
		else
			*pp++ = *ptr;
	*pp = '\0';
	return ret;
}

char *makehstr(s) /**/
char *s;
{
char *t;

	t = s = strdup(s);
	for (; *t; t++)
		if (*t == HISTSPACE)
			*t = ' ';
	return s;
}

char *quietgetevent(ev) /**/
int ev;
{
Histent ent;

	if (ev < firsthist()) return NULL;
	ent = gethistent(ev);
	return (lithist) ? ent->lit : ent->lex;
}

char *getevent(ev) /**/
int ev;
{
char *ret;

	ret = quietgetevent(ev);
	if (!ret) {
		herrflush();
		zerr("no such event: %d",NULL,ev);
	}
	return ret;
}
 
int getargc(list) /**/
char *list;
{
int argc = 0;

	for (; *list; list++) if (*list == HISTSPACE) argc++;
	return argc;
}
 
char *getargs(elist,arg1,arg2) /**/
char *elist;int arg1;int arg2;
{
char *ret = elist,*retn;
int acnt = arg2-arg1+1;

	while (arg1--)
		while (*ret && *ret++ != HISTSPACE);
	if (!*ret)
		{
		herrflush();
		zerr("no such word in event",NULL,0);
		return NULL;
		}
	retn = ret = strdup(ret);
	while (acnt > 0)
		{
		while (*ret && *ret != HISTSPACE)
			ret++;
		if (*ret == HISTSPACE)
			*ret = ' ';
		else
			break;
		acnt--;
		}
	if (acnt > 1 && !*ret)
		{
		herrflush();
		zerr("no such word in event",NULL,0);
		return NULL;
		}
	*ret = '\0';
	return retn;
}

void upcase(x) /**/
char **x;
{
char *pp = *(char **) x;

	for (; *pp; pp++)
		*pp = tuupper(*pp);
}

void downcase(x) /**/
char **x;
{
char *pp = *(char **) x;

	for (; *pp; pp++)
		*pp = tulower(*pp);
}

int quote(tr) /**/
char **tr;
{
char *ptr,*rptr,**str = (char **) tr;
int len = 3;
 
	for (ptr = *str; *ptr; ptr++,len++)
		if (*ptr == '\'') len += 3;
	ptr = *str;
	*str = rptr = alloc(len);
	*rptr++ = '\'';
	for (; *ptr; ptr++)
		if (*ptr == '\'') {
			*rptr++ = '\''; *rptr++ = '\\'; *rptr++ = '\''; *rptr++ = '\'';
		} else
			*rptr++ = *ptr;
	*rptr++ = '\'';
	*rptr++ = 0;
	str[1] = NULL;
	return 0;
}
 
int quotebreak(tr) /**/
char **tr;
{
char *ptr,*rptr,**str = (char **) tr;
int len = 3;
 
	for (ptr = *str; *ptr; ptr++,len++)
		if (*ptr == '\'')
			len += 3;
		else if (inblank(*ptr))
			len += 2;
	ptr = *str;
	*str = rptr = alloc(len);
	*rptr++ = '\'';
	for (; *ptr; )
		if (*ptr == '\'') {
			*rptr++ = '\''; *rptr++ = '\\'; *rptr++ = '\''; *rptr++ = '\'';
			ptr++;
		} else if (inblank(*ptr)) {
			*rptr++ = '\''; *rptr++ = *ptr++; *rptr++ = '\'';
		} else
			*rptr++ = *ptr++;
	*rptr++ = '\'';
	*rptr++ = '\0';
	return 0;
}

void herrflush() /**/
{
	if (strin)
		hflush();
	else while (lastc != '\n' && !lexstop)
		hgetch();
}

/* read an arbitrary amount of data into a buffer until stop is found */

char *hdynread(stop) /**/
int stop;
{
int bsiz = 256,ct = 0,c;
char *buf = zalloc(bsiz),*ptr;
 
	ptr = buf;
	while ((c = hgetch()) != stop && c != '\n' && !lexstop)
		{
		if (c == '\\')
			c = hgetch();
		*ptr++ = c;
		if (++ct == bsiz)
			{
			buf = realloc(buf,bsiz *= 2);
			ptr = buf+ct;
			}
		}
	*ptr = 0;
	if (c == '\n')
		{
		hungetch('\n');
		zerr("delimiter expected",NULL,0);
		free(buf);
		return NULL;
		}
	return buf;
}
 
char *hdynread2(stop) /**/
int stop;
{
int bsiz = 256,ct = 0,c;
char *buf = zalloc(bsiz),*ptr;
 
	ptr = buf;
	while ((c = hgetch()) != stop && c != '\n' && !lexstop)
		{
		if (c == '\n')
			{
			hungetch(c);
			break;
			}
		if (c == '\\')
			c = hgetch();
		*ptr++ = c;
		if (++ct == bsiz)
			{
			buf = realloc(buf,bsiz *= 2);
			ptr = buf+ct;
			}
		}
	*ptr = 0;
	if (c == '\n')
		hungetch('\n');
	return buf;
}

void inithist() /**/
{
	hp_lit = zalloc(sizeof *hp_lit);
	hp_lit->next = NULL;
	hp_lit->ptr = hp_lit->pool = zalloc(HEAPSIZE);
	hp_lit->free = HEAPSIZE;
	hp_lex = zalloc(sizeof *hp_lex);
	hp_lex->next = NULL;
	hp_lex->ptr = hp_lex->pool = zalloc(HEAPSIZE);
	hp_lex->free = HEAPSIZE;
	histentct = (lithistsiz > histsiz) ? lithistsiz : histsiz;
	histentarr = zcalloc(histentct*sizeof *histentarr);
}

void resizehistents() /**/
{
int newentct,t0,t1,firstlit,firstlex;
Histent newarr;

	newentct = (lithistsiz > histsiz) ? lithistsiz : histsiz;
	newarr = zcalloc(newentct*sizeof *newarr);
	firstlex = curhist-histsiz+1;
	firstlit = curhist-lithistsiz+1;
	t0 = firsthist();
	if (t0 < curhist-newentct) t0 = curhist-newentct;
	t1 = t0 % newentct;
	for (; t0 <= curhist; t0++) {
		newarr[t1] = *gethistent(t0);
		if (t0 < firstlex) newarr[t1].lex = NULL;
		if (t0 < firstlit) newarr[t1].lit = NULL;
		t1++; if (t1 == newentct) t1 = 0;
	}
	free(histentarr);
	histentarr = newarr;
	histentct = newentct;
}

char *hp_alloc(hp,siz) /**/
Hp *hp; int siz;
{
char *ret;
Hp h = *hp;

	if (h->free >= siz) {
		ret = h->ptr;
		h->ptr += siz;
		h->free -= siz;
		return ret;
	}
#ifdef MEMDEBUG
	fprintf(stderr,"new heap (siz = %d, curhist = %d)\n",siz,curhist);
#endif
	permalloc();
	h = zalloc(sizeof *h);
	h->next = *hp;
	h->free = (siz > HEAPSIZE) ? siz : HEAPSIZE;
	h->ptr = h->pool = zalloc(h->free);
	h->histno = curhist;
	*hp = h;
	heapalloc();
	return hp_alloc(hp,siz);
}

char *hp_realloc(hp,ptr,oldsiz,newsiz) /**/
Hp *hp; char *ptr; int oldsiz; int newsiz;
{
Hp h = *hp;
int delta = newsiz-oldsiz;
char *ret;

	if (h->ptr-oldsiz == ptr && h->free >= delta) {
		h->free -= delta;
		h->ptr += delta;
		return ptr;
	}
#ifdef MEMDEBUG
	fprintf(stderr,"realloc copy\n");
#endif
	memcpy(ret = hp_alloc(hp,newsiz),ptr,oldsiz);
	return ret;
}

void hp_free(h,ptr,siz) /**/
Hp h; char *ptr; int siz;
{
	if (h->ptr-siz == ptr) {
		h->free += siz;
		h->ptr -= siz;
	}
}

char *hp_concat(old,new) /**/
char *old; char *new;
{
int oldlen,newlen;

	oldlen = strlen(old); newlen = strlen(new);
	old = hp_realloc(&hp_lit,old,oldlen+1,oldlen+newlen+1);
	strcpy(old+oldlen,new);
	return old;
}

void hp_purge(h,lim) /**/
Hp h; int lim;
{
Hp hlast;

	if (!h->next) return;
	while (h->next) { hlast = h; h = h->next; }
	if (h->histno <= lim || h->histno == 0) {
#ifdef MEMDEBUG
		fprintf(stderr,"purging %d\n",lim);
#endif
		free(h->pool);
		free(h);
		hlast->next = NULL;
	}
}

void readhistfile(s,err) /**/
char *s;int err;
{
char buf[1024];
FILE *in;
Histent ent;
time_t tim = time(NULL);

	if (!s) return;
	if (in = fopen(s,"r")) {
		while (fgets(buf,1024,in)) {
			int l = strlen(buf);
			char *pt = buf;

			while (l && buf[l-1] == '\n') {
				buf[l-1] = '\0';
				if (l > 1 && buf[l-2] == '\\') {
					buf[l-2] = '\n';
					fgets(buf+l-1,1024-(l-1),in);
					l = strlen(buf);
				} else break;
			}
			for (;*pt;pt++) if (*pt == ' ') *pt = HISTSPACE;

			ent = gethistent(++curhist);
			ent->lex = hp_alloc(&hp_lex,strlen(buf)+1);
			strcpy(ent->lex,buf);
			ent->lit = hp_alloc(&hp_lit,strlen(buf)+1);
			strcpy(ent->lit,buf);
			ent->ftim = ent->stim = tim;
		}
		fclose(in);
	} else if (err)
		zerr("can't read history file",s,0);
}

void savehistfile(s,err,app) /**/
char *s;int err;int app;
{
char *t;
FILE *out;
int ev,flag;

	if (!s || !interact) return;
	ev = curhist-savehist+1;
	flag = (app) ? O_APPEND : O_TRUNC;
	if (ev < firsthist()) ev = firsthist();
	if (out = fdopen(open(s,O_CREAT|O_WRONLY|flag,0600),"w")) {
		for (; ev <= curhist; ev++) {
			t = quietgetevent(ev);
			for (; *t; t++)
				if (*t == HISTSPACE) fputc(' ',out);
				else {
					if (*t == '\n') fputc('\\',out);
					fputc(*t,out);
				}
			fputc('\n',out);
		}
		fclose(out);
	} else if (err) zerr("can't write history file: %s",s,0);
}

int firsthist() /**/
{
int ev;
Histent ent;

	ev = curhist-histentct+1;
	if (ev < 1) ev = 1;
	do {
		ent = gethistent(ev);
		if ((lithist) ? ent->lit : ent->lex) break;
		ev++;
	} while (ev < curhist);
	return ev;
}

