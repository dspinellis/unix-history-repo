/*
 *
 * params.c - parameters
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
#include <pwd.h>

#define new(X) (X=(vptr)alloc(sizeof(*(X))))

static Param argvparam;

struct iparam {
	struct hashnode *next; int canfree; char *nam; /* hash data */
	void *value;
	int (*func1)(); /* set func */
	int (*func2)(); /* get func */
	int ct;				/* output base or field width */
	int flags;
	vptr data;			/* used by getfns */
	char *env;			/* location in environment, if exported */
	char *ename;		/* name of corresponding environment var */
	};

#define IFN(X) ((int (*)())(X))

/* put predefined params in hash table */

void setupparams() /**/
{
static struct iparam pinit[] = {
#define IPDEF1(A,B,C,D) {NULL,0,A,NULL,IFN(C),IFN(B),10,\
		PMFLAG_i|PMFLAG_SPECIAL|D,NULL,NULL,NULL}
	IPDEF1("#",poundgetfn,IFN(nullsetfn),PMFLAG_r),
	IPDEF1("ARGC",poundgetfn,IFN(nullsetfn),PMFLAG_r),
	IPDEF1("ERRNO",errnogetfn,IFN(nullsetfn),PMFLAG_r),
	IPDEF1("GID",gidgetfn,IFN(nullsetfn),PMFLAG_r),
	IPDEF1("HISTSIZE",histsizegetfn,histsizesetfn,0),
	IPDEF1("LITHISTSIZE",lithistsizegetfn,lithistsizesetfn,0),
	IPDEF1("RANDOM",randomgetfn,randomsetfn,0),
	IPDEF1("SECONDS",secondsgetfn,secondssetfn,0),
	IPDEF1("UID",uidgetfn,IFN(nullsetfn),PMFLAG_r),

#define IPDEF2(A,B,C,D) {NULL,0,A,NULL,IFN(C),IFN(B),0,\
		PMFLAG_SPECIAL|D,NULL,NULL,NULL}
	IPDEF2("-",dashgetfn,IFN(nullsetfn),PMFLAG_r),
	IPDEF2("HISTCHARS",histcharsgetfn,histcharssetfn,0),
	IPDEF2("HOME",homegetfn,homesetfn,0),
	IPDEF2("TERM",termgetfn,termsetfn,0),
	IPDEF2("WORDCHARS",wordcharsgetfn,wordcharssetfn,0),
	IPDEF2("IFS",ifsgetfn,ifssetfn,0),
	IPDEF2("_",underscoregetfn,IFN(nullsetfn),PMFLAG_r),

#define IPDEF3(A,B) {NULL,0,A,NULL,IFN(nullsetfn),IFN(strconstgetfn),0,PMFLAG_r|\
		PMFLAG_SPECIAL,(vptr)B,NULL,NULL}
	IPDEF3("HOSTTYPE",HOSTTYPE),
	IPDEF3("VERSION",VERSIONSTR),

#define IPDEF4(A,B) {NULL,0,A,NULL,IFN(nullsetfn),IFN(intvargetfn),10,\
		PMFLAG_r|PMFLAG_i|PMFLAG_SPECIAL,(vptr)B,NULL,NULL}
	IPDEF4("!",&lastpid),
	IPDEF4("$",&mypid),
	IPDEF4("?",&lastval),
	IPDEF4("status",&lastval),
	IPDEF4("LINENO",&lineno),
	IPDEF4("PPID",&ppid),

#define IPDEF5(A,B) {NULL,0,A,NULL,IFN(intvarsetfn),IFN(intvargetfn),10,\
		PMFLAG_i|PMFLAG_SPECIAL,(vptr)B,NULL,NULL}
	IPDEF5("BAUD",&baud),
	IPDEF5("COLUMNS",&columns),
	IPDEF5("DIRSTACKSIZE",&dirstacksize),
	IPDEF5("LINES",&lines),
	IPDEF5("LISTMAX",&listmax),
	IPDEF5("LOGCHECK",&logcheck),
	IPDEF5("MAILCHECK",&mailcheck),
	IPDEF5("OPTIND",&zoptind),
	IPDEF5("PERIOD",&period),
	IPDEF5("REPORTTIME",&reporttime),
	IPDEF5("SAVEHIST",&savehist),
	IPDEF5("SHLVL",&shlvl),
	IPDEF5("TMOUT",&tmout),

#define IPDEF6(A,B) {NULL,0,A,NULL,IFN(nullsetfn),IFN(strvargetfn),0,\
		PMFLAG_r|PMFLAG_SPECIAL,(vptr)B,NULL,NULL}
	IPDEF6("LOGNAME",&logname),
	IPDEF6("PWD",&pwd),
	IPDEF6("TTY",&ttystrname),
	IPDEF6("USERNAME",&username),

#define IPDEF7(A,B) {NULL,0,A,NULL,IFN(strvarsetfn),IFN(strvargetfn),0,\
		PMFLAG_SPECIAL,(vptr)B,NULL,NULL}
	IPDEF7("FCEDIT",&fceditparam),
	IPDEF7("HOST",&hostnam),
	IPDEF7("OLDPWD",&oldpwd),
	IPDEF7("OPTARG",&optarg),
	IPDEF7("MAIL",&mailfile),
	IPDEF7("NULLCMD",&nullcmd),
	IPDEF7("POSTEDIT",&postedit),
	IPDEF7("prompt",&prompt),
	IPDEF7("PROMPT",&prompt),
	IPDEF7("PROMPT2",&prompt2),
	IPDEF7("PROMPT3",&prompt3),
	IPDEF7("PROMPT4",&prompt4),
	IPDEF7("READNULLCMD",&readnullcmd),
	IPDEF7("RPROMPT",&rprompt),
	IPDEF7("PS1",&prompt),
	IPDEF7("PS2",&prompt2),
	IPDEF7("PS3",&prompt3),
	IPDEF7("PS4",&prompt4),
	IPDEF7("RPS1",&rprompt),
	IPDEF7("SPROMPT",&sprompt),
	IPDEF7("TIMEFMT",&timefmt),
	IPDEF7("TMPPREFIX",&tmpprefix),
	IPDEF7("WATCHFMT",&watchfmt),
	IPDEF7("0",&argzero),

#define IPDEF8(A,B,C) {NULL,0,A,NULL,IFN(colonarrsetfn),IFN(colonarrgetfn),0,\
		PMFLAG_SPECIAL,(vptr)C,NULL,B}
	IPDEF8("CDPATH","cdpath",&cdpath),
	IPDEF8("FIGNORE","fignore",&fignore),
	IPDEF8("FPATH","fpath",&fpath),
	IPDEF8("MAILPATH","mailpath",&mailpath),
	IPDEF8("MANPATH","manpath",&manpath),
	IPDEF8("WATCH","watch",&watch),
	IPDEF8("HOSTS","hosts",&hosts),
	IPDEF8("PATH",NULL,NULL),

#define IPDEF9(A,B,C) {NULL,0,A,NULL,IFN(arrvarsetfn),IFN(arrvargetfn),0,\
		PMFLAG_A|PMFLAG_SPECIAL|C,(vptr)B,NULL,NULL}
	IPDEF9("cdpath",&cdpath,0),
	IPDEF9("fignore",&fignore,0),
	IPDEF9("fpath",&fpath,0),
	IPDEF9("mailpath",&mailpath,0),
	IPDEF9("manpath",&manpath,0),
	IPDEF9("watch",&watch,0),
	IPDEF9("hosts",&hosts,0),
	IPDEF9("signals",&sigptr,PMFLAG_r),
	IPDEF9("argv",&pparams,0),
	IPDEF9("*",&pparams,0),
	IPDEF9("@",&pparams,0),

#define IPDEF10(A,C,D) {NULL,0,A,NULL,IFN(D),IFN(C),0,\
		PMFLAG_A|PMFLAG_SPECIAL,NULL,NULL,NULL}
	IPDEF10("path",pathgetfn,pathsetfn),
	IPDEF10("hostcmds",nullgetfn,hostcmdssetfn),
	IPDEF10("optcmds",nullgetfn,optcmdssetfn),
	IPDEF10("bindcmds",nullgetfn,bindcmdssetfn),
	IPDEF10("varcmds",nullgetfn,varcmdssetfn),
	{NULL,}
	};
struct iparam *ip;

	for (ip = pinit; ip->nam; ip++) addhperm(ip->nam,ip,paramtab,NULL);
	argvparam = gethnode("argv",paramtab);
}

static int unsetflag;

struct param *createparam(name,value,flags) /**/
char *name;vptr value;int flags;
{
struct param *pm;
char buf[20];

	pm = zcalloc(sizeof *pm);
	if (isset(ALLEXPORT))
		flags |= PMFLAG_x;
	pm->flags = flags;
	if ((flags & PMTYPE) == PMFLAG_s) {
		pm->u.str = value;
		pm->sets.cfn = strsetfn;
		pm->gets.cfn = strgetfn;
	} else if ((flags & PMTYPE) == PMFLAG_A) {
		pm->u.arr = value;
		pm->sets.afn = arrsetfn;
		pm->gets.afn = arrgetfn;
	} else {
		pm->u.val = (value) ? matheval(value) : 0;
		pm->sets.ifn = intsetfn;
		pm->gets.ifn = intgetfn;
		sprintf(buf,"%ld",pm->u.val);
		value = buf;
	}
	if (flags & PMFLAG_x)
		pm->env = addenv(name,value);
	addhnode(ztrdup(name),pm,paramtab,freepm);
	return pm;
}

int isident(s) /**/
char *s;
{
char *ss;

	for (ss = s; *ss; ss++) if (!iident(*ss)) break;
	if (!*ss || *ss == '[') return 1;
	if (*s == Quest)
		*s = '?';
	else if (*s == Pound)
		*s = '#';
	else if (*s == String || *s == Qstring)
		*s = '$';
	else if (*s == Star)
		*s = '*';
	if (*s == '#' || *s == '-' || *s == '?' || *s == '$' || *s == '_' ||
		 *s == '!' || *s == '@' || *s == '*')
		return 1;
	return 0;
}

Value getvalue(pptr,bracks) /**/
char **pptr;int bracks;
{
char *s = *pptr,*t = *pptr;
char sav;
Value v;

	if (idigit(*s)) while (idigit(*s)) s++;
	else if (iident(*s)) while (iident(*s)) s++;
	else if (*s == Quest) *s++ = '?';
	else if (*s == Pound) *s++ = '#';
	else if (*s == String) *s++ = '$';
	else if (*s == Qstring) *s++ = '$';
	else if (*s == Star) *s++ = '*';
	else if (*s == '#' || *s == '-' || *s == '?' || *s == '$' ||
				*s == '_' || *s == '!' || *s == '@' || *s == '*') s++;
	else return NULL;
	if (sav = *s) *s = '\0';
	if (idigit(*t) && *t != '0') {
		v = (Value) hcalloc(sizeof *v);
		v->pm = argvparam;
		v->a = v->b = atoi(t)-1;
		if (sav)
			*s = sav;
	} else {
		struct param *pm;
		int isvarat = !strcmp(t, "@");

		pm = gethnode(t,paramtab);
		if (sav)
			*s = sav;
		*pptr = s;
		if (!pm)
			return NULL;
		v = hcalloc(sizeof *v);
		if (pmtype(pm) == PMFLAG_A)
			v->isarr = isvarat ? -1 : 1;
		v->pm = pm;
		v->a = 0; v->b = -1;
		if (bracks && (*s == '[' || *s == Inbrack)) {
			int a,b;
			char *olds = s,*t;

			*s++ = '[';
			for (t = s; *t && *t != ']' && *t != Outbrack; t++)
				if (itok(*t))
					*t = ztokens[*t-Pound];
			if (*t == Outbrack)
				*t = ']';
			if ((s[0] == '*' || s[0] == '@')  && s[1] == ']') {
				if (v->isarr) v->isarr = (s[0] == '*') ? 1 : -1;
				v->a = 0;
				v->b = -1;
				s += 2;
			} else {
				a = mathevalarg(s,&s);
				if (a > 0) a--;
				if (*s == ',' || *s == Comma) {
					s++;
					b = mathevalarg(s,&s);
					if (b > 0) b--;
				} else
					b = a;
				if (*s == ']') {
					s++;
					if (v->isarr && a == b)
						v->isarr = 0;
					v->a = a;
					v->b = b;
				} else
					s = olds;
			}
		}
	}
	if (!bracks && *s)
		return NULL;
	*pptr = s;
	return v;
}

char *getstrvalue(v) /**/
Value v;
{
char *s,**ss;
static char buf[20];

	if (!v)
		return "";
	if (pmtype(v->pm) != PMFLAG_A) {
		if ((pmtype(v->pm) == PMFLAG_i))
			convbase(s = buf,v->pm->gets.ifn(v->pm),v->pm->ct);
		else
			s = v->pm->gets.cfn(v->pm);
		if (v->a == 0 && v->b == -1) return s;
		if (v->a < 0) v->a += strlen(s);
		if (v->b < 0) v->b += strlen(s);
		s = (v->a > strlen(s)) ? strdup("") : strdup(s+v->a);
		if (v->b < v->a) s[0] = '\0';
		else if (v->b-v->a < strlen(s)) s[v->b-v->a+1] = '\0';
		return s;
	}
	if (v->isarr) return spacejoin(v->pm->gets.afn(v->pm));

	ss = v->pm->gets.afn(v->pm);
	if (v->a < 0) v->a += arrlen(ss);
	s = (v->a >= arrlen(ss) || v->a < 0) ? "" : ss[v->a];
	return s;
}

char **getarrvalue(v) /**/
Value v;
{
char **s;
static char *nular[] = { "", NULL };

	if (!v)
		return arrdup(nular);
	s = v->pm->gets.afn(v->pm);
	if (v->a == 0 && v->b == -1) return s;
	if (v->a < 0) v->a += arrlen(s);
	if (v->b < 0) v->b += arrlen(s);
	if (v->a > arrlen(s) || v->a < 0)
		s = arrdup(nular);
	else
		s = arrdup(s)+v->a;
	if (v->b < v->a) s[0] = NULL;
	else if (v->b-v->a < arrlen(s)) s[v->b-v->a+1] = NULL;
	return s;
}

long getintvalue(v) /**/
Value v;
{
char **ss;

	if (!v || v->isarr)
		return 0;
	if (pmtype(v->pm) != PMFLAG_A) {
		if (pmtype(v->pm) == PMFLAG_i)
			return v->pm->gets.ifn(v->pm);
		return atol(v->pm->gets.cfn(v->pm));
	}
	ss = v->pm->gets.afn(v->pm);
	if (v->a < 0) v->a += arrlen(ss);
	if (v->a < 0 || v->a > arrlen(ss)) return 0;
	return atol(ss[v->a]);
}

void setstrvalue(v,val) /**/
Value v;char *val;
{
char *s;

	if (v->pm->flags & PMFLAG_r) {
		free(val);
		return;
	}
	if ((s = v->pm->env) && val)
		v->pm->env = replenv(v->pm->env,val);
	switch (pmtype(v->pm)) {
		case PMFLAG_s:
			if (v->a == 0 && v->b == -1)
				(v->pm->sets.cfn)(v->pm,val);
			else {
				char *z,*y,*x;

				z = strdup((v->pm->gets.cfn)(v->pm));
				if (v->a < 0) {
					v->a += strlen(z);
					if (v->a < 0) v->a = 0;
				}
				if (v->a > strlen(z)) v->a = strlen(z);
				if (v->b < 0) v->b += strlen(z);
				if (v->b < v->a) v->b = v->a;
				z[v->a] = '\0';
				y = z+v->b+1;
				x = zalloc(strlen(z)+strlen(y)+strlen(val)+1);
				strcpy(x,z);
				strcat(x,val);
				strcat(x,y);
				(v->pm->sets.cfn)(v->pm,x);
			}
			if (v->pm->flags & (PMFLAG_L|PMFLAG_R|PMFLAG_Z) && !v->pm->ct)
				v->pm->ct = strlen(val);
			break;
		case PMFLAG_i:
			(v->pm->sets.ifn)(v->pm,matheval(val));
			if (!v->pm->ct && lastbase != 1)
				v->pm->ct = lastbase;
			free(val);
			break;
		case PMFLAG_A:
			if (v->a != v->b)
				zerr("illegal array assignment",NULL,0);
			else {
				char **ss = (v->pm->gets.afn)(v->pm);
				int ac,ad,t0;

				ac = arrlen(ss);
				if (v->a < 0) {
					v->a += ac;
					if (v->a < 0) v->a = 0;
				}
				if (v->a >= ac) {
					char **st = ss;

					ad = v->a+1;
					ss = zalloc((ad+1)*sizeof *ss);
					memcpy(ss,st,(ad+1)*sizeof *ss);
					for (t0 = 0; t0 != ac; t0++)
						ss[t0] = ztrdup(ss[t0]);
					while (ac < ad)
						ss[ac++] = ztrdup("");
					ss[ac] = NULL;
				}
				if (ss[v->a]) free(ss[v->a]);
				ss[v->a] = val;
				(v->pm->sets.afn)(v->pm,ss);
			}
			break;
	}
}

void setintvalue(v,val) /**/
Value v;long val;
{
char buf[20];

	if (v->pm->flags & PMFLAG_r)
		return;
	if (v->pm->env) {
		sprintf(buf,"%ld",val);
		v->pm->env = replenv(v->pm->env,buf);
	}
	switch (pmtype(v->pm))
		{
		case PMFLAG_s:
			sprintf(buf,"%ld",val);
			(v->pm->sets.cfn)(v->pm,ztrdup(buf));
			break;
		case PMFLAG_i:
			(v->pm->sets.ifn)(v->pm,val);
			if (!v->pm->ct && lastbase != -1)
				v->pm->ct = lastbase;
			break;
		case PMFLAG_A:
			zerr("attempt to assign integer to array",NULL,0);
			break;
		}
}

void setintenv(s,val) /**/
char *s; long val;
{
Param pm;
char buf[20];

	if ((pm = gethnode(s,paramtab)) && pm->env) {
		sprintf(buf,"%ld",val);
		pm->env = replenv(pm->env,buf);
	}
}

void setarrvalue(v,val) /**/
Value v;char **val;
{
	if (v->pm->flags & PMFLAG_r)
		return;
	if (pmtype(v->pm) != PMFLAG_A)
		{
		zerr("attempt to assign array value to non-array",NULL,0);
		return;
		}
	(v->pm->sets.afn)(v->pm,val);
}

char *getsparamval(s,l) /**/
char *s;int l;
{
char sav,*t = s;
Value v;

	if (sav = t[l])
		t[l] = '\0';
	if (!(v = getvalue(&s,0)))
		return NULL;
	t[l] = sav;
	t = getstrvalue(v);
	return t;
}

long getiparam(s) /**/
char *s;
{
Value v;

	if (!(v = getvalue(&s,0)))
		return 0;
	return getintvalue(v);
}

char *getsparam(s) /**/
char *s;
{
Value v;

	if (!(v = getvalue(&s,0)))
		return NULL;
	return getstrvalue(v);
}

char **getaparam(s) /**/
char *s;
{
Value v;

	if (!(v = getvalue(&s,0))) return NULL;
	return getarrvalue(v);
}

Param setsparam(s,val) /**/
char *s;char *val;
{
Value v;
char *t = s;

	if (!isident(s)) {
		zerr("not an identifier: %s",s,0);
		free(val);
		return NULL;
	}
	if (!(v = getvalue(&s,1)) || *s)
		return createparam(t,val,PMFLAG_s);
	if ((v->pm->flags & PMTYPE) != PMFLAG_s &&
			!(v->pm->flags & PMFLAG_SPECIAL)) {
		unsetparam(s);
		return createparam(t,val,PMFLAG_s);
	}
	setstrvalue(v,val);
	return v->pm;
}

Param setaparam(s,val) /**/
char *s;char **val;
{
Value v;
char *t = s;

	if (!isident(s))
		{
		zerr("not an identifier: %s",s,0);
		return NULL;
		}
	if (!(v = getvalue(&s,1)) || *s)
		return createparam(t,val,PMFLAG_A);
	if ((v->pm->flags & PMTYPE) != PMFLAG_A &&
			!(v->pm->flags & PMFLAG_SPECIAL)) {
		unsetparam(s);
		return createparam(t,val,PMFLAG_A);
	}
	setarrvalue(v,val);
	return v->pm;
}

Param setiparam(s,val) /**/
char *s;long val;
{
Value v;
char *t = s;
Param pm;

	if (!isident(s))
		{
		zerr("not an identifier: %s",s,0);
		return NULL;
		}
	if (!(v = getvalue(&s,0)))
		{
		pm = createparam(t,NULL,PMFLAG_i);
		pm->u.val = val;
		return pm;
		}
	setintvalue(v,val);
	return v->pm;
}

void unsetparam(s) /**/
char *s;
{
Param pm;

	if (!(pm = gethnode(s,paramtab)))
		return;
	if (pm->flags & PMFLAG_r)
		return;
	unsetflag = 1;
	switch (pmtype(pm))
		{
		case 0:
			(pm->sets.cfn)(pm,ztrdup(""));
			break;
		case PMFLAG_i:
			(pm->sets.ifn)(pm,0);
			break;
		case PMFLAG_A:
			(pm->sets.afn)(pm,mkarray(NULL));
			break;
		}
	if (pmtype(pm) == PMFLAG_s && (pm->flags & PMFLAG_x)) {
		delenv(pm->env);
		free(pm->env);
	}
	if (!(pm->flags & PMFLAG_SPECIAL))
		freepm(remhnode(s,paramtab));
	unsetflag = 0;
}

void intsetfn(pm,x) /**/
Param pm;long x;
{
	pm->u.val = x;
}

long intgetfn(pm) /**/
Param pm;
{
	return pm->u.val;
}

void strsetfn(pm,x) /**/
Param pm;char *x;
{
	if (x) 
		{
		if (pm->u.str)
			free(pm->u.str);
		pm->u.str = x;
		}
}

char *strgetfn(pm) /**/
Param pm;
{
	return pm->u.str;
}

void nullsetfn(pm,x) /**/
Param pm; char *x;
{
	free(x);
}

void arrsetfn(pm,x) /**/
Param pm;char **x;
{
int ct;

	if (x)
		{
		if (pm->u.arr && pm->u.arr != x)
			freearray(pm->u.arr);
		pm->u.arr = x;
		for (ct = 0; *x; x++,ct++);
		pm->ct = ct;
		}
}

char **arrgetfn(pm) /**/
Param pm;
{
	return pm->u.arr;
}

void intvarsetfn(pm,x) /**/
Param pm;long x;
{
	*((long *) pm->data) = x;
}

long intvargetfn(pm) /**/
Param pm;
{
	return *((long *) pm->data);
}

void strvarsetfn(pm,x) /**/
Param pm;char *x;
{
char **q = ((char **) pm->data);

	if (*q) free(*q);
	*q = x;
}

void strvarnonullsetfn(pm,x) /**/
Param pm;char *x;
{
char **q = ((char **) pm->data);

	if (*q) free(*q);
	*q = (x) ? x : ztrdup("");
}

char *strvargetfn(pm) /**/
Param pm;
{
char *s;

	s = *((char **) pm->data);
	if (!s) return "";
	return s;
}

char *strconstgetfn(pm) /**/
Param pm;
{
	return (char *) pm->data;
}

void colonarrsetfn(pm,x) /**/
Param pm;char *x;
{
char **s,**t,*u,*up;

	s = colonsplit(x);
	free(x);
	if (pm->data != &fignore)
		for (t = s; *t; t++) {
			u = *t;
			if (*u == '~') *u = Tilde;
			if (*u == '=') *u = Equals;
			up = hcalloc(strlen(u)+1);
			strcpy(up,u);
			u = up;
			filesub(&u);
			if (!*u) u = ".";
			*t = ztrdup(u);
		}
	if (pm->data) {
		freearray(*((char ***) pm->data));
		*((char ***) pm->data) = s;
		if (pm->ename)
			arrfixenv(pm->ename,s);
	} else {
		freearray(path);
		path = s;
		newcmdnamtab();
		arrfixenv("PATH",s);
	}
}

char *colonarrgetfn(pm) /**/
Param pm;
{
	if ((char **) pm->data)
		return colonjoin(*(char ***) pm->data);
	else
		return colonjoin(path);
}

char **arrvargetfn(pm) /**/
Param pm;
{
	return *((char ***) pm->data);
}

void arrvarsetfn(pm,x) /**/
Param pm;char **x;
{
	if ((*(char ***) pm->data) != x)
		freearray(*(char ***) pm->data);
	*((char ***) pm->data) = x;
	if (pm->ename)
		arrfixenv(pm->ename,x);
}

char **pathgetfn(pm) /**/
Param pm;
{
	return path;
}

void pathsetfn(pm,x) /**/
Param pm;char **x;
{
	if (path != x) freearray(path);
	path = x;
	newcmdnamtab();
	arrfixenv("PATH",x);
}

void hostcmdssetfn(pm,x) /**/
Param pm;char **x;
{
	compctl_process(x,CC_HOSTS|CC_FILES,NULL);
	freearray(x);
}

void optcmdssetfn(pm,x) /**/
Param pm;char **x;
{
	compctl_process(x,CC_OPTIONS,NULL);
	freearray(x);
}

void bindcmdssetfn(pm,x) /**/
Param pm;char **x;
{
	compctl_process(x,CC_BINDINGS,NULL);
	freearray(x);
}

void varcmdssetfn(pm,x) /**/
Param pm;char **x;
{
	compctl_process(x,CC_VARS,NULL);
	freearray(x);
}

char **nullgetfn(pm) /**/
Param pm;
{
static char *nl = NULL; return &nl;
}

void unsettablesetfn(pm,x) /**/
Param pm;char *x;
{ ; }

long poundgetfn(pm) /**/
Param pm;
{
	return arrlen(pparams);
}

long randomgetfn(pm) /**/
Param pm;
{
	return rand() & 0x7fff;
}

void randomsetfn(pm,v) /**/
Param pm;long v;
{
	srand((unsigned int) v);
}

long secondsgetfn(pm) /**/
Param pm;
{
	return time(NULL)-shtimer;
}

void secondssetfn(pm,x) /**/
Param pm;long x;
{
	shtimer = time(NULL)-x;
}

long uidgetfn(pm) /**/
Param pm;
{
	return getuid();
}

long gidgetfn(pm) /**/
Param pm;
{
	return getegid();
}

char *usernamegetfn(pm) /**/
Param pm;
{
struct passwd *pwd;

	pwd = getpwuid(getuid());
	return pwd->pw_name;
}

char *hostgetfn(pm) /**/
Param pm;
{
static char hostnam[65];
static int got = 0;

	if (!got)
		{
		gethostname(hostnam,64);
		hostnam[64] = '\0';
		got = 1;
		}
	return hostnam;
}

char *ifsgetfn(pm) /**/
Param pm;
{
	return ifs;
}

void ifssetfn(pm,x) /**/
Param pm;char *x;
{
	if (x) { free(ifs); ifs = x; }
	inittyptab();
}

void histsizesetfn(pm,v) /**/
Param pm;long v;
{
	if ((histsiz = v) <= 2) histsiz = 2;
	resizehistents();
}

long histsizegetfn(pm) /**/
Param pm;
{
	return histsiz;
}

void lithistsizesetfn(pm,v) /**/
Param pm;long v;
{
	if ((lithistsiz = v) <= 2) lithistsiz = 2;
	resizehistents();
}

long lithistsizegetfn(pm) /**/
Param pm;
{
	return lithistsiz;
}

void mailchecksetfn(pm,x) /**/
Param pm;long x;
{
	mailcheck = (unsetflag) ? 600 : x;
}

void pathasetfn(pm,x) /**/
Param pm;char **x;
{
	freearray(path);
	path = x;
	newcmdnamtab();
}

char **pathagetfn(pm) /**/
Param pm;
{
	return path;
}

long errnogetfn(pm) /**/
Param pm;
{
	return errno;
}

char *dashgetfn(pm) /**/
Param pm;
{
static char buf[100];
char *val = buf;
int t0;

	for (val = buf, t0 = ' ';t0 <= 'z'; t0++)
		if (opts[t0] == OPT_SET)
			*val++ = t0;
	*val = '\0';
	return buf;
}

void histcharssetfn(pm,x) /**/
Param pm;char *x;
{
	if (x) {
		bangchar = x[0];
		hatchar = (bangchar) ? x[1] : '\0';
		hashchar = (hatchar) ? x[2] : '\0';
		free(x);
	}
}

char *histcharsgetfn(pm) /**/
Param pm;
{
static char buf[4];

	buf[0] = bangchar;
	buf[1] = hatchar;
	buf[2] = hashchar;
	buf[3] = '\0';
	return buf;
}

char *homegetfn(pm) /**/
Param pm;
{
	return home;
}

void homesetfn(pm,x) /**/
Param pm;char *x;
{
	free(home);
	if (isset(CHASELINKS) && (home = xsymlink(x))) free(x);
	else home = x;
}

char *wordcharsgetfn(pm) /**/
Param pm;
{
	return wordchars;
}

void wordcharssetfn(pm,x) /**/
Param pm;char *x;
{
	free(wordchars);
	if (x) wordchars = x;
	else wordchars = ztrdup(DEFWORDCHARS);
	inittyptab();
}

char *underscoregetfn(pm) /**/
Param pm;
{
char *s,*t;

	if (!(s = qgetevent(curhist-1)))
		return "";
	for (t = s+strlen(s); t > s; t--)
		if (*t == HISTSPACE)
			break;
	if (t != s)
		t++;
	return t;
}

char *termgetfn(pm) /**/
Param pm;
{
	return term;
}

void termsetfn(pm,x) /**/
Param pm;char *x;
{
	if (term) free(term);
	term = x;
	if (!interact || unset(USEZLE))
		return;
	if (tgetent(termbuf,term) != 1)
		{
		zerr("can't find termcap info for %s",term,0);
		errflag = 0;
		termok = 0;
		}
	else
		{
		char tbuf[1024],*pp;
		int t0;

		termok = 1;
		for (t0 = 0; t0 != TC_COUNT; t0++)
			{
			pp = tbuf;
			if (tcstr[t0])
				free(tcstr[t0]);
			if (!tgetstr(tccapnams[t0],&pp))
				tcstr[t0] = NULL, tclen[t0] = 0;
			else
				{
				tcstr[t0] = zalloc(tclen[t0] = pp-tbuf);
				memcpy(tcstr[t0],tbuf,tclen[t0]);
				}
			}

/* if there's no termcap entry for cursor left, use \b. */

		if (!tccan(TCLEFT))
			{
			tcstr[TCLEFT] = ztrdup("\b");
			tclen[TCLEFT] = 1;
			}

/* if there's no termcap entry for clear, use ^L. */

		if (!tccan(TCCLEARSCREEN))
			{
			tcstr[TCCLEARSCREEN] = ztrdup("\14");
			tclen[TCCLEARSCREEN] = 1;
			}

/* if the termcap entry for down is \n, don't use it. */

		if (tccan(TCDOWN) && tcstr[TCDOWN][0] == '\n')
			{
			tclen[TCDOWN] = 0;
			free(tcstr[TCDOWN]);
			tcstr[TCDOWN] = NULL;
			}

/* if there's no termcap entry for cursor up, forget it.
	Use single line mode. */

		if (!tccan(TCUP))
			termok = 0;
		}
}

void setparams() /**/
{
char **envp,**envp2,**envp3,*str;
char buf[50];
struct param *pm;
int ct;

	noerrs = 1;
	for (envp = environ, ct = 2; *envp; envp++,ct++);
	envp = environ;
	envp2 = envp3 = (char **) zalloc(sizeof(char *)*ct);
	for (; *envp; envp++)
		*envp2++ = ztrdup(*envp);
	*envp2 = NULL;
	envp = environ;
	environ = envp2 = envp3;
	for (; *envp; envp++,envp2++) {
		for (str = *envp; *str && *str != '='; str++);
		if (*str == '=') {
			char *iname;

			*str = '\0';
			if (isident(*envp))
				pm = setsparam(iname = *envp,ztrdup(str+1));
			if (pm) {
				pm->flags |= PMFLAG_x;
				pm->env = *envp2;
				if (pm->flags & PMFLAG_SPECIAL)
					pm->env = replenv(pm->env,getsparam(iname));
			}
			*str = '=';
		}
	}
	pm = gethnode("HOME",paramtab);
	if (!(pm->flags & PMFLAG_x)) {
		pm->flags |= PMFLAG_x;
		pm->env = addenv("HOME",home);
	}
	pm = gethnode("PWD",paramtab);
	if (!(pm->flags & PMFLAG_x)) {
		pm->flags |= PMFLAG_x;
		pm->env = addenv("PWD",pwd);
	}
	pm = gethnode("LOGNAME",paramtab);
	if (!(pm->flags & PMFLAG_x)) {
		pm->flags |= PMFLAG_x;
		pm->env = addenv("LOGNAME",logname);
	}
	pm = gethnode("SHLVL",paramtab);
	if (!(pm->flags & PMFLAG_x))
		pm->flags |= PMFLAG_x;
	sprintf(buf,"%d",++shlvl);
	pm->env = addenv("SHLVL",buf);
	noerrs = 0;
}

char *mkenvstr(x,y) /**/
char *x;char *y;
{
char *z;
int xl = strlen(x),yl = strlen(y);

	z = zalloc(xl+yl+2);
	strcpy(z,x);
	z[xl] = '=';
	strcpy(z+xl+1,y);
	z[xl+yl+1] = '\0';
	return z;
}

void arrfixenv(s,t) /**/
char *s;char **t;
{
char **ep;
int sl = strlen(s);

	for (ep = environ; *ep; ep++)
		if (!strncmp(*ep,s,sl) && (*ep)[sl] == '=') {
			char *u = colonjoin(t);
			replenv(*ep,u);
			break;
		}
}

char *replenv(e,value) /**/
char *e;char *value;
{
char **ep;

	for (ep = environ; *ep; ep++)
		if (*ep == e)
			{
			char *s = e;

			while (*s++ != '=');
			*s = '\0';
			*ep = zalloc(strlen(e)+strlen(value)+2);
			strcpy(*ep,e);
			strcat(*ep,value);
			free(e);
			return *ep;
			}
	return NULL;
}

char *addenv(name,value) /**/
char *name;char *value;
{
char **ep,**ep2,**ep3;
int envct;

	for (ep = environ; *ep; ep++)
		{
		char *s = *ep,*t = name;

		while (*s && *s == *t) s++,t++;
		if (*s == '=' && !*t)
			{
			free(*ep);
			return *ep = mkenvstr(name,value);
			}
		}
	envct = arrlen(environ);
	ep = ep2 = (char **) zalloc((sizeof (char *))*(envct+3));
	for (ep3 = environ; *ep2 = *ep3; ep3++,ep2++);
	*ep2 = mkenvstr(name,value);
	ep2[1] = NULL;
	free(environ);
	environ = ep;
	return *ep2;
}

void delenv(x) /**/
char *x;
{
char **ep;

	ep = environ;
	for (; *ep; ep++)
		if (*ep == x)
			break;
	if (*ep)
		for (; ep[0] = ep[1]; ep++);
}

void convbase(s,v,base) /**/
char *s;long v;int base;
{
int digs = 0;
long x;

	if (base <= 1)
		base = 10;
	x = v;
	if (x < 0)
		{
		x = -x;
		digs++;
		}
	for (; x; digs++)
		x /= base;
	if (!digs)
		digs = 1;
	s[digs--] = '\0';
	x = (v < 0) ? -v : v;
	while (digs >= 0)
		{
		int dig = x%base;
		s[digs--] = (dig < 10) ? '0'+dig : dig-10+'A';
		x /= base;
		}
	if (v < 0)
		s[0] = '-';
}


