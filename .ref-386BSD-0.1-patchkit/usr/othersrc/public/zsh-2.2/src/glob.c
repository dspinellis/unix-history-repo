/*
 *
 * glob.c - filename generation
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

#ifdef __hpux
#include <ndir.h>
#else
#ifdef SYSV
#define direct dirent
#else
#include <sys/dir.h>
#endif
#endif
#include <sys/errno.h>

#define exists(X) (access(X,0) == 0 || readlink(X,NULL,0) == 0)

static int mode;				/* != 0 if we are parsing glob patterns */
static int pathpos;			/* position in pathbuf */
static int matchsz;			/* size of matchbuf */
static int matchct;			/* number of matches found */
static char pathbuf[MAXPATHLEN];	/* pathname buffer */
static char **matchbuf;		/* array of matches */
static char **matchptr;		/* &matchbuf[matchct] */
static Comp exclude;			/* pattern to exclude */

/* max # of qualifiers */

#define QUALCT 16

static int (*qualfuncs[QUALCT])DCLPROTO((struct stat *,long));
static long qualdata[QUALCT];
static int qualsense[QUALCT];
static int qualct;
static int gf_nullglob,gf_markdirs,gf_noglobdots;

/* pathname component in filename patterns */

struct complist {
	Complist next;
	Comp comp;
	int closure;	/* 1 if this is a (foo/)# */
	};
struct comp {
	Comp left,right,next;
	char *str;
	int closure,last;
	};

void glob(list,np) /**/
Lklist list;Lknode *np;
{
Lknode node = prevnode(*np);
Lknode next = nextnode(*np);
int sl;			/* length of the pattern */
char *ostr;		/* the pattern before the parser chops it up */
Complist q;		/* pattern after parsing */
char *str = getdata(*np);	/* the pattern */

	sl = strlen(str);
	ostr = strdup(str);
	uremnode(list,*np);
	qualct = 0;
	gf_nullglob = isset(NULLGLOB);
	gf_markdirs = isset(MARKDIRS);
	gf_noglobdots = unset(GLOBDOTS);
	if (str[sl-1] == Outpar)	/* check for qualifiers */
		{
		char *s;
		int sense = 0;
		long data;
		int (*func) DCLPROTO((struct stat *,long));

		for (s = str+sl-2; s != str; s--)
			if (*s == Bar || *s == Outpar || *s == Inpar)
				break;
		if (*s == Inpar)
			{
			*s++ = '\0';
			func = NULL;
			while (*s != Outpar)
				{
				func = NULL;
				if (idigit(*s))
					{
					func = qualflags;
					data = 0;
					while (idigit(*s))
						data = data*010+(*s++-'0');
					}
				else switch ((int)(unsigned char)(*s++))
					{
					case (int)(unsigned char)Hat: case '^': sense = 1-sense; break;
#ifdef S_IFLNK
					case '@': func = qualmode; data = S_IFLNK; break;
#endif
#ifdef S_IFSOCK
					case '=': func = qualmode; data = S_IFSOCK; break;
#endif
#ifdef S_IFIFO
					case 'p': func = qualmode; data = S_IFIFO; break;
#endif
					case '/': func = qualmode; data = S_IFDIR; break;
					case '.': func = qualmode; data = S_IFREG; break;
					case '%': func = qualisdev; break;
					case (int)(unsigned char)Star:  func = qualiscom; break;
					case 'R': func = qualflags; data = 0004; break;
					case 'W': func = qualflags; data = 0002; break;
					case 'X': func = qualflags; data = 0001; break;
					case 'r': func = qualflags; data = 0400; break;
					case 'w': func = qualflags; data = 0200; break;
					case 'x': func = qualflags; data = 0100; break;
					case 's': func = qualflags; data = 04000; break;
					case 'S': func = qualflags; data = 02000; break;
					case 'd': func = qualdev; data = qgetnum(&s); break;
					case 'l': func = qualnlink; data = qgetnum(&s); break;
					case 'U': func = qualuid; data = geteuid(); break;
					case 'G': func = qualgid; data = getegid(); break;
					case 'u': func = qualuid; data = qgetnum(&s); break;
					case 'g': func = qualgid; data = qgetnum(&s); break;
					case 'M': gf_markdirs = !sense; break;
					case 'N': gf_nullglob = !sense; break;
					case 'D': gf_noglobdots = sense; break;
					default: zerr("unknown file attribute",NULL,0); return;
					}
				if (func)
					{
					if (qualct == QUALCT-1)
						{
						zerr("too many qualifiers",NULL,0);
						return;
						}
					qualfuncs[qualct] = func;
					qualsense[qualct] = sense;
					qualdata[qualct] = data;
					qualct++;
					}
				if (errflag)
					return;
				}
			}
		}
	else if ((str[sl-1] == '/') && !((str[sl-2] == Star)&&
				(str[sl-3] == Star)&&(str[sl-4] == Star)&&
				(str[sl-5]==Star)))		/* foo/ == foo(/) */
		{
		str[sl-1] = '\0';
		qualfuncs[0] = qualmode;
		qualdata[0] = S_IFDIR;
		qualsense[0] = 0;
		qualct = 1;
		}
	qualfuncs[qualct] = NULL;
	if (*str == '/')	/* pattern has absolute path */
		{
		str++;
		pathbuf[0] = '/';
		pathbuf[pathpos = 1] = '\0';
		}
	else		/* pattern is relative to pwd */
		pathbuf[pathpos = 0] = '\0';
	q = parsepat(str);
	if (!q || errflag)	/* if parsing failed */
		{
		if (isset(NOBADPATTERN))
			{
			insnode(list,node,ostr);
			return;
			}
		errflag = 0;
		zerr("bad pattern: %s",ostr,0);
		return;
		}
	matchptr = matchbuf = (char **) zalloc((matchsz = 16)*sizeof(char *));
	matchct = 0;
	scanner(q);		/* do the globbing */
	if (matchct)
		badcshglob |= 2;
	else if (!gf_nullglob)
		if (isset(CSHNULLGLOB)) {
			badcshglob |= 1;
		} else if (unset(NONOMATCH)) {
			zerr("no matches found: %s",ostr,0);
			free(matchbuf);
			return;
		} else {
			*matchptr++ = strdup(ostr);
			matchct = 1;
		}
	qsort(&matchbuf[0],matchct,sizeof(char *),notstrcmp);
	matchptr = matchbuf;
	while (matchct--)			/* insert matches in the arg list */
		insnode(list,node,*matchptr++);
	free(matchbuf);
	*np = (next) ? prevnode(next) : lastnode(list);
}

/* get number after qualifier */

long qgetnum(s) /**/
char **s;
{
long v = 0;

	if (!idigit(**s))
		{
		zerr("number expected",NULL,0);
		return 0;
		}
	while (idigit(**s))
		v = v*10+*(*s)++-'0';
	return v;
}

int notstrcmp(a,b) /**/
char **a;char **b;
{
char *c = *b,*d = *a;
int x1,x2;

	for (; *c == *d && *c; c++,d++);
	x1 = atoi(c); x2 = atoi(d);
	if (x1==x2 || unset(NUMERICGLOBSORT))
		return ((int) (unsigned char) *c-(int) (unsigned char) *d);
	return x1-x2;
}

int forstrcmp(a,b) /**/
char **a;char **b;
{
char *c = *b,*d = *a;

	for (; *c == *d && *c; c++,d++);
	return ((int) (unsigned char) *d-(int) (unsigned char) *c);
}

/* add a match to the list */

void insert(s) /**/
char *s;
{
struct stat buf;
int statted = 0;

	if (exclude && domatch(s,exclude,gf_noglobdots)) return;
	if (gf_markdirs && !lstat(s,&buf) && S_ISDIR(buf.st_mode)) {
		char *t;
		int ll = strlen(s);

		t = ncalloc(ll+2);
		strcpy(t,s);
		t[ll] = '/';
		t[ll+1] = '\0';
		s = t;
		statted = 1;
	}
	if (qualct)	{ /* do the (X) (^X) stuff */
		int (**fptr)DCLPROTO((struct stat *,long)) = qualfuncs;
		int *sptr = qualsense;
		long *lptr = qualdata;
		struct stat buf;

		if (statted || lstat(s,&buf) >= 0)
			while (*fptr) if (!(!!((*fptr++)(&buf,*lptr++)) ^ *sptr++)) return;
	}
	*matchptr++ = s;
	if (++matchct == matchsz) {
		matchbuf = (char **) realloc((char *) matchbuf,
			sizeof(char **)*(matchsz *= 2));
		matchptr = matchbuf+matchct;
	}
}

/* check to see if str is eligible for filename generation */

int haswilds(str) /**/
char *str;
{
	if ((*str == Inbrack || *str == Outbrack) && !str[1]) return 0;
	if (str[0] == '%') return 0;
	for (; *str; str++)
		if (*str == Pound || *str == Hat || *str == Star ||
				*str == Bar || *str == Inbrack || *str == Inang ||
				*str == Quest) return 1;
	return 0;
}

/* check to see if str is eligible for brace expansion */

int hasbraces(str) /**/
char *str;
{
int mb,bc,cmct1,cmct2;
char *lbr = NULL;

	if (str[0] == Inbrace && str[1] == Outbrace)
		return 0;
	if (isset(BRACECCL)) {
		for (mb = bc = 0; *str; ++str)
			if (*str == Inbrace) {
				if (++bc > mb)
					mb = bc;
			}
			else if (*str == Outbrace)
				if (--bc < 0)
					return(0);
		return(mb && bc == 0);
	}
	for (mb = bc = cmct1 = cmct2 = 0; *str; str++)
		{
		if (*str == Inbrace)
			{
			if (!bc)
				lbr = str;
			bc++;
			if (str[4] == Outbrace && str[2] == '-') /* {a-z} */
				{
				cmct1++;
				if (bc == 1)
					cmct2++;
				}
			}
		else if (*str == Outbrace)
			{
			bc--;
			if (!bc)
				{
				if (!cmct2)
					{
					*lbr = '{';
					*str = '}';
					}
				cmct2 = 0;
				}
			}
		else if (*str == Comma && bc)
			{
			cmct1++;
			if (bc == 1)
				cmct2++;
			}
		if (bc > mb)
			mb = bc;
		if (bc < 0)
			return 0;
		}
	return (mb && bc == 0 && cmct1);
}

/* expand stuff like >>*.c */

int xpandredir(fn,tab) /**/
struct redir *fn;Lklist tab;
{
Lklist fake;
char *nam;
struct redir *ff;
int ret = 0;

	fake = newlist();
	addnode(fake,fn->name);
	prefork(fake);
	if (!errflag)
		postfork(fake,1);
	if (errflag) return 0;
	if (full(fake) && !nextnode(firstnode(fake))) {
		fn->name = peekfirst(fake);
		untokenize(fn->name);
	} else
		while (nam = ugetnode(fake)) {
			ff = alloc(sizeof *ff);
			*ff = *fn;
			ff->name = nam;
			addnode(tab,ff);
			ret = 1;
		}
	return ret;
}

/* concatenate s1 and s2 in dynamically allocated buffer */

char *dyncat(s1,s2) /**/
char *s1;char *s2;
{
char *ptr;
 
	ptr = ncalloc(strlen(s1)+strlen(s2)+1);
	strcpy(ptr,s1);
	strcat(ptr,s2);
	return ptr;
}

/* concatenate s1, s2, and s3 in dynamically allocated buffer */

char *tricat(s1,s2,s3) /**/
char *s1;char *s2;char *s3;
{
char *ptr;

	ptr = zalloc(strlen(s1)+strlen(s2)+strlen(s3)+1);
	strcpy(ptr,s1);
	strcat(ptr,s2);
	strcat(ptr,s3);
	return ptr;
}

/* brace expansion */

void xpandbraces(list,np) /**/
Lklist list;Lknode *np;
{
Lknode node = (*np),last = prevnode(node);
char *str = getdata(node),*str3 = str,*str2;
int prev, bc, comma;

	for (; *str != Inbrace; str++);
	for (str2 = str, bc = comma = 0; *str2; ++str2)
		if (*str2 == Inbrace)
			++bc;
		else if (*str2 == Outbrace) {
			if (--bc == 0)
				break;
		}
		else if (bc == 1 && *str2 == Comma)
			++comma;
	if (!comma && !bc && isset(BRACECCL)) {			/* {a-mnop} */
		char	ccl[256], *p;
		unsigned char c1,c2,lastch;

		uremnode(list,node);
		memset(ccl, 0, sizeof(ccl) / sizeof(ccl[0]));
		for (p = str + 1, lastch = 0; p < str2; ) {
			if (itok(c1 = *p++))
				c1 = ztokens[c1 - (unsigned char)Pound];
			if (itok(c2 = *p))
				c2 = ztokens[c2 - (unsigned char)Pound];
			if (c1 == '-' && lastch && p < str2 && lastch <= c2) {
				while (lastch < c2)
					ccl[lastch++] = 1;
				lastch = 0;
			}
			else
				ccl[lastch = c1] = 1;
		}
		strcpy(str + 1, str2 + 1);
		for (p = ccl+255; p-- > ccl; )
			if (*p) {
				*str = p - ccl;
				insnode(list, last, strdup(str3));
			}
		*np = nextnode(last);
		return;
	}
 	if (str[2] == '-' && str[4] == Outbrace)	 /* {a-z} */
		{
		char c1,c2;

		uremnode(list,node);
		chuck(str);
		c1 = *str;
		chuck(str);
		chuck(str);
		c2 = *str;
		chuck(str);
		if (itok(c1))
			c1 = ztokens[c1-Pound];
		if (itok(c2))
			c2 = ztokens[c2-Pound];
		if (c1 < c2)
			for (; c2 >= c1; c2--)	/* {a-z} */
				{
				*str = c2;
				insnode(list,last,strdup(str3));
				}
		else
			for (; c2 <= c1; c2++)	/* {z-a} */
				{
				*str = c2;
				insnode(list,last,strdup(str3));
				}
		*np = nextnode(last);
		return;
		}
	prev = str-str3;
	str2 = getparen(str++);
	if (!str2)
		{
		zerr("how did you get this error?",NULL,0);
		return;
		}
	uremnode(list,node);
	node = last;
	for(;;)
		{
		char *zz,*str4;
		int cnt;
		
		for (str4 = str, cnt = 0; cnt || *str != Comma && *str !=
				Outbrace; str++)
			if (*str == Inbrace)
				cnt++;
			else if (*str == Outbrace)
				cnt--;
			else if (!*str)
				exit(10);
		zz = zalloc(prev+(str-str4)+strlen(str2)+1);
		ztrncpy(zz,str3,prev);
		strncat(zz,str4,str-str4);
		strcat(zz,str2);
		insnode(list,node,zz);
		incnode(node);
		if (*str != Outbrace)
			str++;
		else
			break;
		}
	*np = nextnode(last);
}

/* get closing paren, given pointer to opening paren */

char *getparen(str) /**/
char *str;
{
int cnt = 1;
char typein = *str++,typeout = typein+1;

	for (; *str && cnt; str++)
		if (*str == typein)
			cnt++;
		else if (*str == typeout)
			cnt--;
	if (!str && cnt)
		return NULL;
	return str;
}

/* check to see if a matches b (b is not a filename pattern) */

int matchpat(a,b) /**/
char *a;char *b;
{
Comp c;
int val,len;
char *b2;

	remnulargs(b);
	len = strlen(b);
	b2 = alloc(len+3);
	strcpy(b2+1,b);
	b2[0] = Inpar;
	b2[len+1] = Outpar;
	b2[len+2] = '\0';
	c = parsereg(b2);
	if (!c)
		{
		zerr("bad pattern: %s",b,0);
		return 0;
		}
	val = domatch(a,c,0);
	return val;
}

/* do the ${foo%%bar}, ${foo#bar} stuff */
/* please do not laugh at this code. */

void getmatch(sp,pat,dd) /**/
char **sp;char *pat;int dd;
{
Comp c;
char *t,*lng = NULL,cc,*s = *sp;

	remnulargs(pat);
	c = parsereg(pat);
	if (!c)
		{
		zerr("bad pattern: %s",pat,0);
		return;
		}
	if (!(dd & 2))
		{
		for (t = s; t==s || t[-1]; t++)
			{
			cc = *t;
			*t = '\0';
			if (domatch(s,c,0))
				{
				if (!(dd & 1))
					{
					*t = cc;
					t = strdup(t);
					*sp = t;
					return;
					}
				lng = t;
				}
			*t = cc;
			}
		if (lng)
			{
			t = strdup(lng);
			*sp = t;
			return;
			}
		}
	else
		{
		for (t = s+strlen(s); t >= s; t--)
			{
			if (domatch(t,c,0))
				{
				if (!(dd & 1))
					{
					cc = *t;
					*t = '\0';
					*sp = strdup(*sp);
					*t = cc;
					return;
					}
				lng = t;
				}
			}
		if (lng)
			{
			cc = *lng;
			*lng = '\0';
			*sp = strdup(*sp);
			*lng = cc;
			return;
			}
		}
}

/* add a component to pathbuf */

static int addpath(s)
char *s;
{
	if (strlen(s)+pathpos >= MAXPATHLEN) return 0;
	while (pathbuf[pathpos++] = *s++);
	pathbuf[pathpos-1] = '/';
	pathbuf[pathpos] = '\0';
	return 1;
}

char *getfullpath(s) /**/
char *s;
{
static char buf[MAXPATHLEN];

	strcpy(buf,pathbuf);
	strcat(buf,s);
	return buf;
}

/* do the globbing */

void scanner(q) /**/
Complist q;
{
Comp c;
int closure;

	if (closure = q->closure)	/* (foo/)# */
		if (q->closure == 2)		/* (foo/)## */
			q->closure = 1;
		else
			scanner(q->next);
	if (c = q->comp)
		{
		if (!(c->next || c->left) && !haswilds(c->str))
			if (q->next)
				{
				int oppos = pathpos;

				if (errflag)
					return;
				if (q->closure && !strcmp(c->str,".")) return;
				if (!addpath(c->str)) return;
				if (!closure || exists(pathbuf))
					scanner((q->closure) ? q : q->next);
				pathbuf[pathpos = oppos] = '\0';
				}
			else
				{
				char *s;

				if (exists(s = getfullpath(c->str)))
					insert(strdup(s));
				}
		else
			{
			char *fn;
			int dirs = !!q->next;
			struct direct *de;
			DIR *lock = opendir((*pathbuf) ? pathbuf : ".");
			 
			if (lock == NULL)
				return;
			readdir(lock); readdir(lock); 	/* skip . and .. */
			while (de = readdir(lock))
				{
				if (errflag)
					break;
				fn = &de->d_name[0];
				if (domatch(fn,c,gf_noglobdots))
					{
					int oppos = pathpos;

					if (dirs)
						{
						if (closure)
							{
							int type3;
							struct stat buf;

					 		if (lstat(getfullpath(fn),&buf) == -1)
								{
								if (errno != ENOENT && errno != EINTR &&
										errno != ENOTDIR)
									{
									zerr("%e: %s",fn,errno);
									errflag = 0;
									}
								continue;
								}
							type3 = buf.st_mode & S_IFMT;
							if (type3 != S_IFDIR)
								continue;
							}
						if (addpath(fn))
							scanner((q->closure) ? q : q->next); /* scan next level */
						pathbuf[pathpos = oppos] = '\0';
						}
					else insert(dyncat(pathbuf,fn));
					}
				}
			closedir(lock);
			}
		}
	else
		zerr("no idea how you got this error message.",NULL,0);
}

/* do the [..(foo)..] business */

int minimatch(pat,str) /**/
char **pat;char **str;
{
char *pt = *pat+1,*s = *str;
	
	for (; *pt != Outpar; s++,pt++)
		if ((*pt != Quest || !*s) && *pt != *s)
			{
			*pat = getparen(*pat)-1;
			return 0;
			}
	*str = s-1;
	return 1;
}

static char *pptr;
static Comp tail = 0;
static int first;

int domatch(str,c,fist) /**/
char *str;Comp c;int fist;
{
	pptr = str;
	first = fist;
	return doesmatch(c);
}

/* see if current pattern matches c */

int doesmatch(c) /**/
Comp c;
{
char *pat = c->str;

	if (c->closure == 1) {
		char *saves = pptr;

		if (first && *pptr == '.') return 0;
		if (doesmatch(c->next)) return 1;
		pptr = saves;
		first = 0;
	}
	for(;;)
		{
		if (!pat || !*pat)
			{
			char *saves;
			int savei;

			if (errflag)
				return 0;
			saves = pptr;
			savei = first;
			if (c->left || c->right)
				if (!doesmatch(c->left))
					if (c->right)
						{
						pptr = saves;
						first = savei;
						if (!doesmatch(c->right))
							return 0;
						}
					else
						return 0;
			if (c->closure)
				return doesmatch(c);
			if (!c->next)
				return (!c->last || !*pptr);
			return doesmatch(c->next);
			}
		if (first && *pptr == '.' && *pat != '.')
			return 0;
		if (*pat == Star)	/* final * is not expanded to ?#; returns success */
			{
			while (*pptr) pptr++;
			return 1;
			}
		first = 0;
		if (*pat == Quest && *pptr)
			{
			pptr++;
			pat++;
			continue;
			}
		if (*pat == Hat)
			return 1-doesmatch(c->next);
		if (*pat == Inbrack) {
			if (!*pptr) break;
			if (pat[1] == Hat || pat[1] == '^') {
				pat[1] = Hat;
				for (pat += 2; *pat != Outbrack && *pat; pat++)
					if (*pat == '-' && pat[-1] != Hat && pat[1] != Outbrack) {
						if (pat[-1] <= *pptr && pat[1] >= *pptr)
							break;
					} else if (*pptr == *pat) break;
				if (!*pat) {
					zerr("something is very wrong.",NULL,0);
					return 0;
				}
				if (*pat != Outbrack)
					break;
				pat++;
				pptr++;
				continue;
			} else {
				for (pat++; *pat != Outbrack && *pat; pat++)
					if (*pat == Inpar) {
						if (minimatch(&pat,&pptr))
							break;
					} else if (*pat == '-' && pat[-1] != Inbrack &&
							pat[1] != Outbrack) {
						if (pat[-1] <= *pptr && pat[1] >= *pptr)
							break;
					} else if (*pptr == *pat) break;
				if (!pat || !*pat) {
					zerr("oh dear.  that CAN'T be right.",NULL,0);
					return 0;
				}
				if (*pat == Outbrack)
					break;
				for (pptr++; *pat != Outbrack; pat++);
				pat++;
				continue;
			}
		}
		if (*pat == Inang)
			{
			int t1,t2,t3;
			char *ptr;

			if (*++pat == Outang)	/* handle <> case */
				{
				( void ) zstrtol(pptr,&ptr,10);
				if (ptr == pptr)
					break;
				pptr = ptr;
				pat++;
				}
			else
				{
				t1 = zstrtol(pptr,&ptr,10);
				if (ptr == pptr)
					break;
				pptr = ptr;
				t2 = zstrtol(pat,&ptr,10);
				if (*ptr != '-')
					exit(31);
				t3 = zstrtol(ptr+1,&pat,10);
				if (!t3)
					t3 = -1;
				if (*pat++ != Outang)
					exit(21);
				if (t1 < t2 || (t3 != -1 && t1 > t3))
					break;
				}
			continue;
			}
		if (*pptr == *pat)
			{
			pptr++;
			pat++;
			continue;
			}
		break;
		}
	return 0;
}

Complist parsepat(str) /**/
char *str;
{
char *s;

	exclude = NULL;
	if (isset(EXTENDEDGLOB)) {
		s = str+strlen(str);
		while (s-- > str) {
			if (*s == Tilde && s[1]) {
				*s++ = '\0';
				exclude = parsereg(s);
				if (!exclude) return NULL;
				break;
			}
		}
	}
	mode = 0;
	pptr = str;
	return parsecomplist();
}

Comp parsereg(str) /**/
char *str;
{
	mode = 1;
	pptr = str;
	return parsecompsw();
}

Complist parsecomplist() /**/
{
Comp c1;
Complist p1;

	if (pptr[0] == Star && pptr[1] == Star &&
			(pptr[2] == '/' ||
			(pptr[2] == Star && pptr[3] == Star && pptr[4] == '/'))) {
		pptr += 3;
		if (pptr[-1] == Star) pptr += 2;
		p1 = (Complist) alloc(sizeof *p1);
		p1->next = parsecomplist();
		p1->comp = (Comp) alloc(sizeof *p1->comp);
		p1->comp->last = 1;
		p1->comp->str = strdup("*");
		*p1->comp->str = Star;
		p1->closure = 1;
		return p1;
	}
	if (*pptr == Inpar)
		{
		char *str;
		int pars = 1;

		for (str = pptr+1; *str && pars; str++)
			if (*str == Inpar)
				pars++;
			else if (*str == Outpar)
				pars--;
		if (str[0] != Pound || str[-1] != Outpar || str[-2] != '/')
			goto kludge;
		pptr++;
		if (!(c1 = parsecompsw()))
			return NULL;
		if (pptr[0] == '/' && pptr[1] == Outpar && pptr[2] == Pound)
			{
			int pdflag = 0;

			pptr += 3;
			if (*pptr == Pound)
				{
				pdflag = 1;
				pptr++;
				}
			p1 = (Complist) alloc(sizeof *p1);
			p1->comp = c1;
			p1->closure = 1+pdflag;
			p1->next = parsecomplist();
			return (p1->comp) ? p1 : NULL;
			}
		}
	else
		{
kludge:
		if (!(c1 = parsecompsw()))
			return NULL;
		if (*pptr == '/' || !*pptr)
			{
			int ef = *pptr == '/';

			p1 = (Complist) alloc(sizeof *p1);
			p1->comp = c1;
			p1->closure = 0;
			p1->next = (*pptr == '/') ? (pptr++,parsecomplist()) : NULL;
			return (ef && !p1->next) ? NULL : p1;
			}
		}
	errflag = 1;
	return NULL;
}

Comp parsecomp() /**/
{
Comp c = (Comp) alloc(sizeof *c),c1,c2;
char *s = c->str = alloc(MAXPATHLEN*2),*ls = NULL;

	c->next = tail;

	while (*pptr && (mode || *pptr != '/') && *pptr != Bar &&
			*pptr != Outpar)
		{
		if (*pptr == Hat)
			{
			*s++ = Hat;
			*s++ = '\0';
			pptr++;
			if (!(c->next = parsecomp()))
				return NULL;
			return c;
			}
		if (*pptr == Star && pptr[1] && (mode || pptr[1] != '/'))
			{
			*s++ = '\0';
			pptr++;
			c1 = (Comp) alloc(sizeof *c1);
			*(c1->str = strdup("?")) = Quest;
			c1->closure = 1;
			if (!(c2 = parsecomp())) return NULL;
			c1->next = c2;
			c->next = c1;
			return c;
			}
		if (*pptr == Inpar)
			{
			int pars = 1;
			char *startp = pptr, *endp;
			Comp stail = tail;
			int dpnd = 0;

			for (pptr = pptr+1; *pptr && pars; pptr++)
				if (*pptr == Inpar)
					pars++;
				else if (*pptr == Outpar)
					pars--;
			if (pptr[-1] != Outpar)
				{
				errflag = 1;
				return NULL;
				}
			if (*pptr == Pound)
				{
				dpnd = 1;
				pptr++;
				if (*pptr == Pound)
					{
					pptr++;
					dpnd = 2;
					}
				}
			if (!(c1 = parsecomp())) return NULL;
			tail = c1;
			endp = pptr;
			pptr = startp;
			pptr++;
			*s++ = '\0';
			c->next = (Comp) alloc(sizeof *c);
			c->next->left = parsecompsw();
			c->next->closure = dpnd;
			c->next->next = (Comp) alloc(sizeof *c);
			pptr = endp;
			tail = stail;
			return c;
			}
		if (*pptr == Pound)
			{
			*s = '\0';
			pptr++;
			if (!ls)
				return NULL;
			if (*pptr == Pound) 
				{
				pptr++;
				c->next = c1 = (Comp) alloc(sizeof *c);
				c1->str = strdup(ls);
				}
			else
				c1 = c;
			c1->next = c2 = (Comp) alloc(sizeof *c);
			c2->str = strdup(ls);
			c2->closure = 1;
			c2->next = parsecomp();
			if (!c2->next)
				return NULL;
			*ls++ = '\0';
			return c;
			}
		ls = s;
		if (*pptr == Inang)
			{
			int dshct;

			dshct = (pptr[1] == Outang);
			*s++ = *pptr++;
			while (*pptr && (*s++ = *pptr++) != Outang)
				if (s[-1] == '-')
					dshct++;
				else if (!idigit(s[-1]))
					break;
			if (s[-1] != Outang || dshct != 1)
				return NULL;
			}
		else if (*pptr == Inbrack)
			{
			while (*pptr && (*s++ = *pptr++) != Outbrack);
			if (s[-1] != Outbrack)
				return NULL;
			}
		else if (itok(*pptr) && *pptr != Star && *pptr != Quest)
			*s++ = ztokens[*pptr++-Pound];
		else
			*s++ = *pptr++;
		}
	if (*pptr == '/' || !*pptr)
		c->last = 1;
	*s++ = '\0';
	return c;
}

Comp parsecompsw() /**/
{
Comp c1,c2,c3;

	c1 = parsecomp();
	if (!c1)
		return NULL;
	if (*pptr == Bar)
		{
		c2 = (Comp) alloc(sizeof *c2);
		pptr++;
		c3 = parsecompsw();
		if (!c3)
			return NULL;
		c2->str = strdup("");
		c2->left = c1;
		c2->right = c3;
		return c2;
		}
	return c1;
}

/* tokenize and see if ss matches tt */

int patmatch(ss,tt) /**/
char *ss;char *tt;
{
char *s = ss,*t;

	for (; *s; s++)
		if (*s == '\\')
			chuck(s);
		else
			for (t = ztokens; *t; t++)
				if (*t == *s)
					{
					*s = (t-ztokens)+Pound;
					break;
					}
	return matchpat(ss,tt);
}

/* remove unnecessary Nulargs */

void remnulargs(s) /**/
char *s;
{
int nl = *s;
char *t = s;

	while (*s)
		if (*s == Nularg)
			chuck(s);
		else
			s++;
	if (!*t && nl)
		{
		t[0] = Nularg;
		t[1] = '\0';
		}
}

/* qualifier functions */

int qualdev(buf,dv) /**/
struct stat *buf;long dv;
{
	return buf->st_dev == dv;
}

int qualnlink(buf,ct) /**/
struct stat *buf;long ct;
{
	return buf->st_nlink == ct;
}

int qualuid(buf,uid) /**/
struct stat *buf;long uid;
{
	return buf->st_uid == uid;
}

int qualgid(buf,gid) /**/
struct stat *buf;long gid;
{
	return buf->st_gid == gid;
}

int qualisdev(buf,junk) /**/
struct stat *buf;long junk;
{
	junk = buf->st_mode & S_IFMT;
	return junk == S_IFBLK || junk == S_IFCHR;
}

int qualmode(buf,mod) /**/
struct stat *buf;long mod;
{
	return (buf->st_mode & S_IFMT) == mod;
}

int qualflags(buf,mod) /**/
struct stat *buf;long mod;
{
	return buf->st_mode & mod;
}

int qualiscom(buf,mod) /**/
struct stat *buf;long mod;
{
	return (buf->st_mode & (S_IFMT|S_IEXEC)) == (S_IFREG|S_IEXEC);
}

