/*
 *
 * builtin.c - builtin commands
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
#include <sys/errno.h>

#define makecond() allocnode(N_COND)

/* builtin flags */

#define BINF_PLUSOPTS		1		/* +xyz legal */
#define BINF_R					2		/* this is r (fc -e -) */
#define BINF_PRINTOPTS		4
#define BINF_SETOPTS			8
#define BINF_FCOPTS		  16
#define BINF_TYPEOPT      32
#define BINF_TYPEOPTS	  (BINF_TYPEOPT|BINF_PLUSOPTS)
#define BINF_ECHOPTS      64

/* builtin funcs */

#define BIN_TYPESET 0
#define BIN_BG 1
#define BIN_FG 2
#define BIN_JOBS 3
#define BIN_WAIT 4
#define BIN_DISOWN 5
#define BIN_BREAK 6
#define BIN_CONTINUE 7
#define BIN_EXIT 8
#define BIN_RETURN 9
#define BIN_SHIFT 10
#define BIN_CD 11
#define BIN_POPD 12
#define BIN_PUSHD 13
#define BIN_PRINT 14
#define BIN_EVAL 15
#define BIN_SCHED 16
#define BIN_FC 17
#define BIN_PUSHLINE 18
#define BIN_LOGOUT 19
#define BIN_BUILTIN 20
#define BIN_TEST 21
#define BIN_BRACKET 22

struct bincmd {
	char *name;
	int (*handlerfunc) DCLPROTO((char *,char **,char *,int));
	int minargs;		/* min # of args */
	int maxargs;		/* max # of args, or -1 for no limit */
	int flags;			/* BINF_flags (see above) */
	int funcid;			/* xbins (see above) for overloaded handlerfuncs */
	char *optstr;		/* string of legal options */
	char *defopts;		/* options set by default for overloaded handlerfuncs */
	};

/* structure for foo=bar assignments */

struct asgment {
	struct asgment *next;
	char *name,*value;
	};

static char *auxdata;
static int auxlen;
static int showflag = 0,showflag2 = 0;

struct bincmd builtins[] = {
	"[",bin_test,0,-1,0,BIN_BRACKET,NULL,NULL,
	".",bin_dot,1,-1,0,0,NULL,NULL,
	":",bin_colon,0,-1,0,0,NULL,NULL,
	"alias",bin_alias,0,-1,0,0,"ga",NULL,
	"autoload",bin_typeset,0,-1,BINF_TYPEOPTS,0,"tx","fu",
	"bg",bin_fg,0,-1,0,BIN_BG,NULL,NULL,
	"bindkey",bin_bindkey,0,-1,0,0,"asvemdrl",NULL,
	"break",bin_break,0,1,0,BIN_BREAK,NULL,NULL,
	"builtin",NULL,0,0,0,BIN_BUILTIN,NULL,NULL,
	"bye",bin_break,0,1,0,BIN_EXIT,NULL,NULL,
	"cd",bin_cd,0,2,0,BIN_CD,NULL,NULL,
	"chdir",bin_cd,0,2,0,BIN_CD,NULL,NULL,
	"compctl",bin_compctl,0,-1,0,0,NULL,NULL,
	"continue",bin_break,0,1,0,BIN_CONTINUE,NULL,NULL,
	"declare",bin_typeset,0,-1,BINF_TYPEOPTS,0,"LRZfilrtux",NULL,
	"dirs",bin_dirs,0,-1,0,0,"v",NULL,
	"disable",bin_disable,1,-1,0,0,NULL,NULL,
	"disown",bin_fg,1,-1,0,BIN_DISOWN,NULL,NULL,
	"echo",bin_print,0,-1,BINF_PRINTOPTS|BINF_ECHOPTS,BIN_PRINT,"n","-",
	"echotc",bin_echotc,1,-1,0,0,NULL,NULL,
	"enable",bin_enable,1,-1,0,0,NULL,NULL,
	"eval",bin_eval,0,-1,0,BIN_EVAL,NULL,NULL,
	"exit",bin_break,0,1,0,BIN_EXIT,NULL,NULL,
	"export",bin_typeset,0,-1,BINF_TYPEOPTS,0,"LRZfilrtu","x",
	"false",bin_let,0,0,0,0,NULL,NULL,
	"fc",bin_fc,0,-1,BINF_FCOPTS,BIN_FC,"nlreRWAdD",NULL,
	"fg",bin_fg,0,-1,0,BIN_FG,NULL,NULL,
	"functions",bin_typeset,0,-1,BINF_TYPEOPTS,0,"tu","f",
	"getln",bin_read,0,-1,0,0,NULL,"zr",
	"getopts",bin_getopts,2,-1,0,0,NULL,NULL,
	"hash",bin_hash,2,2,0,0,"r",NULL,
	"history",bin_fc,0,-1,0,BIN_FC,"nrdD","l",
	"integer",bin_typeset,0,-1,BINF_TYPEOPTS,0,"LRZlrtux","i",
	"jobs",bin_fg,0,-1,0,BIN_JOBS,"lpZ",NULL,
	"kill",bin_kill,0,-1,0,0,NULL,NULL,
	"let",bin_let,1,-1,0,0,NULL,NULL,
	"limit",bin_limit,0,-1,0,0,"sh",NULL,
	"local",bin_typeset,0,-1,BINF_TYPEOPTS,0,"LRZfilrtux",NULL,
	"log",bin_log,0,0,0,0,NULL,NULL,
	"logout",bin_break,0,1,0,BIN_LOGOUT,NULL,NULL,
	"popd",bin_cd,0,2,0,BIN_POPD,NULL,NULL,
	"print",bin_print,0,-1,BINF_PRINTOPTS,BIN_PRINT,"RDPnrslzNu0123456789p-",NULL,
	"pushd",bin_cd,0,2,0,BIN_PUSHD,NULL,NULL,
	"pushln",bin_print,0,-1,BINF_PRINTOPTS,BIN_PRINT,NULL,"-nz",
	"pwd",bin_pwd,0,0,0,0,NULL,NULL,
	"r",bin_fc,0,-1,BINF_R,BIN_FC,"nrl",NULL,
	"read",bin_read,0,-1,0,0,"rzu0123456789p",NULL,
	"readonly",bin_typeset,0,-1,BINF_TYPEOPTS,0,"LRZfiltux","r",
	"rehash",bin_rehash,0,0,0,0,"f",NULL,
	"return",bin_break,0,1,0,BIN_RETURN,NULL,NULL,
	"sched",bin_sched,0,-1,0,0,NULL,NULL,
	"set",bin_set,0,-1,BINF_SETOPTS|BINF_PLUSOPTS,0,"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZaefghijklnosuvwxy",NULL,
	"setopt",bin_setopt,0,-1,BINF_PLUSOPTS,0,"0123456789BCDEFGHIJKLMNOPQRSTUVWXYZaefghijklmnosuvwxy",NULL,
	"shift",bin_break,0,1,0,BIN_SHIFT,NULL,NULL,
	"source",bin_dot,1,-1,0,0,NULL,NULL,
	"suspend",bin_suspend,0,0,0,0,"f",NULL,
	"test",bin_test,0,-1,0,BIN_TEST,NULL,NULL,
	"ttyctl",bin_ttyctl,0,0,0,0,"fu",NULL,
	"times",bin_times,0,0,0,0,NULL,NULL,
	"trap",bin_trap,0,-1,0,0,NULL,NULL,
	"true",bin_colon,0,0,0,0,NULL,NULL,
	"type",bin_whence,0,-1,0,0,"pfa","v",
	"typeset",bin_typeset,0,-1,BINF_TYPEOPTS,0,"LRZfilrtux",NULL,
	"ulimit",bin_ulimit,0,1,0,0,"HSacdfmnt",NULL,
	"umask",bin_umask,0,1,0,0,NULL,NULL,
	"unalias",bin_unalias,1,-1,0,0,NULL,NULL,
	"unfunction",bin_unhash,1,-1,0,0,NULL,NULL,
	"unhash",bin_unhash,1,-1,0,0,NULL,NULL,
	"unlimit",bin_unlimit,0,-1,0,0,"h",NULL,
	"unset",bin_unset,1,-1,0,0,NULL,NULL,
	"unsetopt",bin_setopt,0,-1,BINF_PLUSOPTS,1,"0123456789BCDEFGHIJKLMNOPQRSTUWXYZabefghijklmnosuvwxy",NULL,
	"vared",bin_vared,1,1,0,0,NULL,NULL,
	"wait",bin_fg,0,-1,0,BIN_WAIT,NULL,NULL,
	"whence",bin_whence,0,-1,0,0,"pvcfa",NULL,
	"which",bin_whence,0,-1,0,0,"pa","c",
	NULL,NULL,0,0,0,0,NULL,NULL
	};

/* print options */

static void prtopt()
{
struct option *opp;

	if (isset(KSHOPTIONPRINT)) {
		printf("Current option settings\n");
		for (opp = optns; opp->name; opp++)
			printf("%-20s%s\n", opp->name,
				(opts[opp->id] == OPT_SET) ? "on" : "off");
	} else
		for (opp = optns; opp->name; opp++)
			if (opts[opp->id] == OPT_SET)
				puts(opp->name);
}

/* add builtins to the command hash table */

void addbuiltins() /**/
{
struct cmdnam *c;
struct bincmd *b;
int t0;

	for (t0 = 0, b = builtins; b->name; b++,t0++)
		{
		c = (Cmdnam) zcalloc(sizeof *c);
		c->type = BUILTIN;
		c->u.binnum = t0;
		addhperm(b->name,c,cmdnamtab,freecmdnam);
		}
}

/* enable */

int bin_enable(name,argv,ops,whocares) /**/
char *name;char **argv;char *ops;int whocares;
{
struct cmdnam *c;
struct bincmd *b;
int t0,ret = 0;

	for (; *argv; argv++)
		{
		for (t0 = 0, b = builtins; b->name; b++,t0++)
			if (!strcmp(*argv,b->name))
				break;
		if (!b->name)
			{
			zerrnam(name,"no such builtin: %s",*argv,0);
			ret = 1;
			}
		else
			{
			c = (Cmdnam) zcalloc(sizeof *c);
			c->type = BUILTIN;
			c->u.binnum = t0;
			addhperm(b->name,c,cmdnamtab,freecmdnam);
			}
		}
	return ret;
}

/* :, true */

int bin_colon(name,argv,ops,whocares) /**/
char *name;char **argv;char *ops;int whocares;
{
	return 0;
}

/* break, bye, continue, exit, logout, return, shift */

int bin_break(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int num = -1;

	if (*argv)
		num = matheval(*argv);
	if ((func == BIN_BREAK || func == BIN_CONTINUE) && !loops)
		{
		if (func == BIN_CONTINUE)
			zerrnam(name,"not in loop",NULL,0);
		return 1;
		}
	switch (func)
		{
		case BIN_CONTINUE:
			contflag = 1;
		case BIN_BREAK:
			breaks = (num == -1) ? 1 : num;
			if (breaks > loops) breaks = loops;
			break;
		case BIN_LOGOUT:
			if (!islogin)
				{
				zerrnam(name,"not login shell",NULL,0);
				return 1;
				}
		case BIN_EXIT:
			zexit((num == -1) ? lastval : num);
			break;
		case BIN_RETURN:
			retflag = 1;
			return lastval = (num == -1) ? lastval : num;
		case BIN_SHIFT:
			{
			char **s;

			if (num == -1)
				num = 1;
			if (num > arrlen(pparams))
				num = arrlen(pparams);
			permalloc();
			s = arrdup(pparams+num);
			heapalloc();
			freearray(pparams);
			pparams = s;
			break;
			}
		}
	return 0;
}

/* bg, disown, fg, jobs, wait */

int bin_fg(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int job,lng,firstjob = -1,retval = 0;

	if (ops['Z']) { if (*argv) strcpy(hackzero,*argv); return 0; }
	lng = (ops['l']) ? 1 : (ops['p']) ? 2 : 0;
	if ((func == BIN_FG || func == BIN_BG) && !jobbing)
		{
		zerrnam(name,"no job control in this shell.",NULL,0);
		return 1;
		}
	if (unset(NOTIFY)) scanjobs();
	if (!(jobtab[curjob].stat & STAT_INUSE))
		{
		curjob = prevjob; setprevjob();
		if (!(jobtab[curjob].stat & STAT_INUSE))
			curjob = prevjob; setprevjob();
		}
	if (func == BIN_JOBS)
		stopmsg = 2;
	if (!*argv)
		if (func == BIN_FG || func == BIN_BG)
			{
			if (curjob == -1 || curjob == thisjob)
				{
				zerrnam(name,"no current job",NULL,0);
				return 1;
				}
			firstjob = curjob;
			}
		else if (func == BIN_JOBS)
			{
			for (job = 0; job != MAXJOB; job++)
				if (job != thisjob && jobtab[job].stat)
					printjob(job+jobtab,lng);
			return 0;
			}
		else
			{
			for (job = 0; job != MAXJOB; job++)
				if (job != thisjob && jobtab[job].stat)
					waitjob(job);
			return lastval;
			}
	for (; (firstjob != -1) || *argv; ( void ) (*argv && argv++))
		{
		int stopped,ocj = thisjob;

		if (func == BIN_WAIT && isanum(*argv)) {
			waitforpid((long) atoi(*argv));
			retval = lastval;
			thisjob = ocj;
			continue;
		}
		job = (*argv) ? getjob(*argv,name) : firstjob;
		firstjob = -1;
		if (job == -1)
			break;
		if (!(jobtab[job].stat & STAT_INUSE))
			{
			zerrnam(name,"no such job: %d",0,job);
			return 1;
			}
		switch (func)
			{
			case BIN_FG:
			case BIN_BG:
				if (stopped = (jobtab[job].stat & STAT_STOPPED))
					makerunning(jobtab+job);
				else if (func == BIN_BG)
					{
					zerrnam(name,"job already in background",NULL,0);
					thisjob = ocj;
					return 1;
					}
				if (curjob == job)
					{
					curjob = prevjob;
					prevjob = (func == BIN_BG) ? -1 : job;
					}
				if (prevjob == job)
					prevjob = -1;
				if (prevjob == -1)
					setprevjob();
				if (curjob == -1)
					{
					curjob = prevjob;
					setprevjob();
					}
				printjob(jobtab+job,(stopped) ? -1 : 0);
				if (func == BIN_FG)
					{
					thisjob = job;
					if (strcmp(jobtab[job].pwd,pwd))
						{
						printf("(pwd : ");
						printdir(jobtab[job].pwd);
						printf(")\n");
						}
					fflush(stdout);
					attachtty(jobtab[job].gleader);
					}
				if (stopped)
					killpg(jobtab[job].gleader,SIGCONT);
				if (func == BIN_FG)
					waitjobs();
				break;
			case BIN_JOBS:
				printjob(job+jobtab,lng);
				break;
			case BIN_WAIT:
				waitjob(job);
				retval = lastval;
				break;
			case BIN_DISOWN:
				{
				static struct job zero;
				jobtab[job] = zero;
				break;
				}
			}
		thisjob = ocj;
		}
	return retval;
}

/* false, let */

int bin_let(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
long val = 0;

	while (*argv)
		val = matheval(*argv++);
	return !val;
}

/* print the directory stack */

static void pdstack()
{
Lknode node;

	printdir(pwd);
	for (node = firstnode(dirstack); node; incnode(node))
		{
		putchar(' ');
		printdir(getdata(node));
		}
	putchar('\n');
}

/* exit the shell */

int zexit(val) /**/
int val;
{
	if (isset(MONITOR))
		if (!stopmsg) {
			checkjobs();
			if (stopmsg) {
				stopmsg = 2;
				return 1;
			}
		} else killrunjobs();
	savehistfile(getsparam("HISTFILE"),0,0);
	if (islogin && unset(NORCS))
		sourcehome(".zlogout");
	if (sigtrapped[SIGEXIT])
		dotrap(SIGEXIT);
	exit(val); return 0;
}

/* identify an option name */

int optlookup(s) /**/
char *s;
{
char *t;
struct option *o;

	t = s = strdup(s);
	while (*t)
		if (*t == '_')
			chuck(t);
		else {
			*t = tulower(*t);
			t++;
		}
	for (o = optns; o->name; o++)
		if (!strcmp(o->name,s))
			return o->id;
	return -1;
}

/* setopt, unsetopt */

int bin_setopt(nam,args,ops,isun) /**/
char *nam;char **args;char *ops;int isun;
{
struct option *opp;
int c;

	if (!ops['@'] && !*args) {
		if (!isun)
			prtopt();
		return 0;
	}
	for (opp = optns; opp->name; opp++)
		if (ops[opp->id] == 1+isun)
			opts[opp->id] = OPT_SET;
		else if (ops[opp->id] == 2-isun)
			opts[opp->id] = OPT_UNSET;
	while (*args) {
		c = optlookup(*args++);
		if (c != -1) {
			if (c == INTERACTIVE || c == MONITOR)
				zerrnam(nam,"can't change that option",NULL,0);
			else
				opts[c] = (isun) ? OPT_UNSET : OPT_SET;
		} else {
			zerrnam(nam,"no such option: %s",args[-1],0);
			return 1;
		}
	}
	return 0;
}

/* execute func on each member of the hash table ht */

void listhtable(ht,func) /**/
Hashtab ht;HFunc func;
{
int t0;
struct hashnode *hn;

	for (t0 = ht->hsize-1; t0 >= 0; t0--)
		for (hn = ht->nodes[t0]; hn; hn = hn->next)
			func(hn->nam,(char *) hn);
}

/* print a shell function (used with listhtable) */

void pshfunc(s,cc) /**/
char *s;Cmdnam cc;
{
char *t;

	if (cc->type != SHFUNC)
		return;
	if (showflag && (cc->flags & showflag2) != showflag2)
		return;
	if (cc->flags & PMFLAG_u)
		printf("undefined ");
	if (cc->flags & PMFLAG_t)
		printf("traced ");
	if (!cc->u.list || !showflag) {
		printf("%s ()\n",s);
		return;
	}
	t = getpermtext((vptr) (cc->u.list));
	printf("%s () {\n\t%s\n}\n",s,t);
	free(t);
}

void niceprint(s) /**/
char *s;
{
	niceprintf(s,stdout);
}

void niceprintf(s,f) /**/
char *s;FILE *f;
{
	for (; *s; s++)
		{
		if (isprint(*s))
			fputc(*s,f);
		else if (*s == '\n')
			{
			putc('\\',f);
			putc('n',f);
			}
		else
			{
			putc('^',f);
			fputc(*s | 0x40,f);
			}
		}
}

int bin_umask(nam,args,ops,func) /**/
char *nam;char **args;char *ops;int func;
{
int um;
char *s = *args;

	um = umask(0);
	umask(um);
	if (!s)
		{
		printf("%03o\n",um);
		return 0;
		}
	if (idigit(*s))
		{
		um = zstrtol(s,&s,8);
		if (*s)
			{
			zerrnam(nam,"bad umask",NULL,0);
			return 1;
			}
		}
	else
		{
		int whomask,op,mask;

		for (;;)
			{
			if (*s == 'u')
				s++, whomask = 0100;
			else if (*s == 'g')
				s++, whomask = 0010;
			else if (*s == 'o')
				s++, whomask = 0001;
			else
				whomask = 0111;
			op = *s++;
			if (!(op == '+' || op == '-' || op == '='))
				{
				zerrnam(nam,"bad symbolic mode operator: %c",NULL,op);
				return 1;
				}
			mask = whomask;
			if (*s == 'r')
				mask *= 04;
			else if (*s == 'w')
				mask *= 02;
			else if (*s != 'x')
				{
				zerrnam(nam,"bad symbolic mode permission: %c",NULL,*s);
				return 1;
				}
			if (op == '+')
				um |= mask;
			else if (op == '-')
				um &= ~mask;
			else /* op == '=' */
				um = (um & ~(whomask*07)) | mask;
			if (*++s == ',')
				s++;
			else
				break;
			}
		if (*s)
			{
			zerrnam(nam,"bad character in symbolic mode: %c",NULL,*s);
			return 1;
			}
		}
	umask(um);
	return 0;
}

/* type, whence, which */

int bin_whence(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
struct cmdnam *chn;
struct alias *a;
int retval = 0;
int csh = ops['c'],all = ops['a'];
int v = ops['v'] || csh;
char *cnam;

	for (; *argv; argv++) {
		if (!ops['p'] && (a = (Alias) gethnode(*argv,aliastab))) {
			if (a->cmd < 0)
				printf((csh) ? "%s: shell reserved word\n" :
					(v) ? "%s is a reserved word\n" : "%s\n",*argv);
			else if (!v)
				puts(a->text);
			else if (a->cmd)
				printf((csh) ? "%s: aliased to %s\n" :
					"%s is an alias for %s\n",*argv,a->text);
			else
				printf((csh) ? "%s: globally aliased to %s\n" :
					"%s is a global alias for %s\n",*argv,a->text);
			retval = 0;
			if (!all) continue;
		}
		if (!ops['p'] && (chn = (Cmdnam) gethnode(*argv,cmdnamtab)) &&
				(chn->type == SHFUNC || chn->type == BUILTIN)) {
			if (chn->type == SHFUNC) {
				if (csh || ops['f']) {
					showflag = 1; showflag2 = 0;
					pshfunc(*argv,chn);
				} else {
					printf((v) ? "%s is a function\n" : "%s\n",*argv);
				}
			} else
				printf((csh) ? "%s: shell built-in command\n" :
					(v) ? "%s is a shell builtin\n" : "%s\n",*argv);
			retval = 0;
			if (!all) continue;
		}
		if (all) {
			char **pp,buf[MAXPATHLEN],*z;
			for (pp = path; *pp; pp++) {
				z = buf;
				strucpy(&z,*pp);
				*z++ = '/';
				strcpy(z,*argv);
				if (iscom(buf)) {
					if (v && !csh) printf("%s is %s\n",*argv,buf);
					else puts(buf);
					retval = 0;
				}
			}
		} else if (!(cnam = findcmd(*argv))) {
			if (v) printf("%s not found\n",*argv);
			retval = 1;
			break;
		} else {
			if (v && !csh) printf("%s is %s\n",*argv,cnam);
			else puts(cnam);
			retval = 0;
		}
	}
	return retval;
}

/* cd, chdir, pushd, popd */

int bin_cd(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
char *dest;

	if (func == BIN_CD && isset(AUTOPUSHD))
		func = BIN_PUSHD;
	dest = cd_get_dest(nam,argv,ops,func);
	if (!dest) return 1;
	dest = cd_do_chdir(nam,dest);
	if (!dest) return 1;
	cd_new_pwd(func,dest);
	return 0;
}

char *cd_get_dest(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
char *dest;

	if (!argv[0])
		if (func == BIN_CD || (func == BIN_PUSHD && isset(PUSHDTOHOME)
				|| !full(dirstack)))
			dest = home;
		else
			dest = getnode(dirstack);
	else if (!argv[1]) {
		Lknode n;
		int dd;

		if (argv[0][1] && argv[0][0] == (isset(PUSHDMINUS) ? '-' : '+')) {
			dd = atoi(argv[0]+1)-1;
			if (dd < 0) {
				zerrnam(nam,"bad directory specification",NULL,0);
				return NULL;
			}
			for (n = firstnode(dirstack); n && dd; dd--, incnode(n));
			if (!n) {
				zerrnam(nam,"no such entry in dir stack",NULL,0);
				return NULL;
			}
			dest = remnode(dirstack,n);
		} else if (argv[0][1] && argv[0][0] == (isset(PUSHDMINUS) ? '+' : '-')) {
			dd = atoi(argv[0]+1);
			for (n = lastnode(dirstack); n != (Lknode) dirstack && dd;
					dd--, n = prevnode(n));
			if (n == (Lknode) dirstack) {
				zerrnam(nam,"no such entry in dir stack",NULL,0);
				return NULL;
			}
			dest = remnode(dirstack,n);
		} else {
			if (!strcmp(argv[0],"-")) printdircr(dest = oldpwd);
			else dest = argv[0];
		}
	} else {
		char *u;
		int len1,len2,len3;

		if (!(u = ztrstr(pwd,argv[0]))) {
			zerrnam(nam,"string not in pwd: %s",argv[0],0);
			return NULL;
		}
		len1 = strlen(argv[0]);
		len2 = strlen(argv[1]);
		len3 = u-pwd;
		dest = alloc(len3+len2+strlen(u+len1)+1);
		strncpy(dest,pwd,len3);
		strcpy(dest+len3,argv[1]);
		strcat(dest,u+len1);
		printdircr(dest);
	}
	return dest;
}

char *cd_do_chdir(cnam,dest) /**/
char *cnam; char *dest;
{
int hasdot = 0, eno = ENOENT;
char **pp,*ret;

	if (*dest == '/') {
		if (ret = cd_try_chdir(NULL,dest)) return ret;
		zerrnam(cnam,"%e: %s",dest,errno);
		return NULL;
	}
	for (pp = cdpath; *pp; pp++)
		if ((*pp)[0] == '.' && (*pp)[1] == '\0') hasdot = 1;
	if (!hasdot) {
		if (ret = cd_try_chdir(NULL,dest)) return ret;
		if (errno != ENOENT) eno = errno;
	}
	for (pp = cdpath; *pp; pp++) {
		if (ret = cd_try_chdir(*pp,dest)) {
			if (strcmp(*pp,".")) {
				printdircr(ret);
			}
			return ret;
		}
		if (errno != ENOENT) eno = errno;
	}
	if (isset(CDABLEVARS)) {
		char *s = getsparam(dest);
		if (s && *s == '/' && chdir(s) != -1) {
			printdircr(s);
			return s;
		}
		if (errno != ENOENT) eno = errno;
	}
	zerrnam(cnam,"%e: %s",dest,eno);
	return NULL;
}

char *cd_try_chdir(pfix,dest) /**/
char *pfix; char *dest;
{
static char buf[MAXPATHLEN], buf2[MAXPATHLEN];
char *s;
int dotsct;

	if (pfix) sprintf(buf,"%s/%s",(!strcmp("/",pfix)) ? "" : pfix,dest); 
	else strcpy(buf,dest);
	dotsct = fixdir(buf2,buf);
	if (buf2[0] == '/') return (chdir(buf2) == -1) ? NULL : buf2;
	if (!dotsct) {
		if (chdir((*buf2) ? buf2 : ".") == -1) return NULL;
		if (*buf2) sprintf(buf,"%s/%s",(!strcmp("/",pwd)) ? "" : pwd,buf2);
		else strcpy(buf,pwd);
		return buf;
	}
	strcpy(buf,pwd);
	s = buf+strlen(buf)-1;
	while (dotsct--) while (s != buf) if (*--s == '/') break;
	if (s == buf || *buf2) s++;
	strcpy(s,buf2);
	if (chdir(buf) != -1 || chdir(dest) != -1) return buf;
	return NULL;
}

int fixdir(d,s) /**/
char *d; char *s;
{
int ct = 0;
char *d0 = d;

#ifdef HAS_RFS
	if (*s == '/' && s[1] == '.' && s[2] == '.') {
		*d++ = '/'; *d++ = '.'; *d++ = '.';
		s += 3;
	}
#endif
	for (;;) {
		if (*s == '/') {
			*d++ = *s++;
			while (*s == '/') s++;
		}
		if (!*s) {
			while (d > d0+1 && d[-1] == '/') d--;
			*d = '\0';
			return ct;
		}
		if (s[0] == '.' && s[1] == '.' && (s[2] == '\0' || s[2] == '/')) {
			if (d > d0+1) {
				for (d--; d > d0+1 && d[-1] != '/'; d--);
			} else ct++;
			s += 2; if (*s) s++;
		} else if (s[0] == '.' && (s[1] == '/' || s[1] == '\0')) {
			s++; if (*s) s++;
		} else {
			while (*s != '/' && *s != '\0') *d++ = *s++;
		}
	}
}

void cd_new_pwd(func,s) /**/
int func; char *s;
{
Param pm;
List l;

	oldpwd = pwd;
	if (isset(CHASELINKS))
		pwd = findpwd(s);
	else
		pwd = ztrdup(s);
	if ((pm = gethnode("PWD", paramtab)) &&
		 (pm->flags & PMFLAG_x) && pm->env)
		pm->env = replenv(pm->env,pwd);
	if ((pm = gethnode("OLDPWD", paramtab)) &&
		 (pm->flags & PMFLAG_x) && pm->env)
		pm->env = replenv(pm->env,oldpwd);
	if (func == BIN_PUSHD) {
		permalloc();
		if (isset(PUSHDIGNOREDUPS)) {
			Lknode n;
			for (n = firstnode(dirstack); n; incnode(n))
				if (!strcmp(oldpwd,getdata(n))) {
					free(remnode(dirstack,n)); break;
				}
		}
		pushnode(dirstack,oldpwd);
		heapalloc();
	}
	if (unset(PUSHDSILENT) && func != BIN_CD && isset(INTERACTIVE))
		pdstack();
	if (l = getshfunc("chpwd")) {
		fflush(stdout); fflush(stderr);
		doshfuncnoval(dupstruct(l),NULL,0);
	}
	if (dirstacksize != -1 && countnodes(dirstack) >= dirstacksize) {
		if (dirstacksize < 2)
			dirstacksize = 2;
		else
			free(remnode(dirstack,lastnode(dirstack)));
	}
}

void convertwd(s,t,off) /**/
char *s; char *t; int off;
{
char *u,*start;

	*t++ = '/';
	start = t;
	while (off--) *t++ = *s++;
	for (;;) {
		while (*s == '/') s++;
		for (u = s; *u && *u != '/'; u++);
		if (!strncmp(s,".",u-s)) {
			;
		} else if (!strncmp(s,"..",u-s)) {
			while (t != start && *--t != '/');
		} else {
			if (t != start) *t++ = '/';
			struncpy(&t,s,u-s);
		}
		if (!*u) break;
		s = u;
	}
	*t = '\0';
}

int bin_rehash(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
	newcmdnamtab();
	if (ops['f']) fullhash();
	return 0;
}

int bin_hash(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
struct cmdnam *chn;

	chn = (Cmdnam) zcalloc(sizeof *chn);
	chn->type = EXCMD;
	chn->pcomp = NULL; /* this is probably a bug ! */
	chn->u.nam = ztrdup(argv[1]);
	addhnode(ztrdup(argv[0]),chn,cmdnamtab,freecmdnam);
	return 0;
}

/* != 0 if s is a prefix of t */

int prefix(s,t) /**/
char *s;char *t;
{
	while (*s && *t && *s == *t) s++,t++;
	return (!*s);
}

/* convert %%, %1, %foo, %?bar? to a job number */

int getjob(s,prog) /**/
char *s;char *prog;
{
int t0,retval;

	if (*s != '%')
		goto jump;
	s++;
	if (*s == '%' || *s == '+' || !*s)
		{
		if (curjob == -1)
			{
			zerrnam(prog,"no current job",NULL,0);
			retval = -1; goto done;
			}
		retval = curjob; goto done;
		}
	if (*s == '-')
		{
		if (prevjob == -1)
			{
			zerrnam(prog,"no previous job",NULL,0);
			retval = -1; goto done;
			}
		retval = prevjob; goto done;
		}
	if (idigit(*s))
		{
		t0 = atoi(s);
		if (t0 && t0 < MAXJOB && jobtab[t0].stat && t0 != thisjob)
			{ retval = t0; goto done; }
		zerrnam(prog,"no such job",NULL,0);
		retval = -1; goto done;
		}
	if (*s == '?')
		{
		struct process *pn;

		for (t0 = MAXJOB-1; t0 >= 0; t0--)
			if (jobtab[t0].stat && t0 != thisjob)
				for (pn = jobtab[t0].procs; pn; pn = pn->next)
					if (ztrstr(pn->text,s+1))
						{ retval = t0; goto done; }
		zerrnam(prog,"job not found: %s",s,0);
		retval = -1; goto done;
		}
jump:
	if ((t0 = findjobnam(s)) != -1)
		{ retval = t0; goto done; }
	zerrnam(prog,"job not found: %s",s,0);
	retval = -1;
done:
	return retval;
}

/* find a job named s */

int findjobnam(s) /**/
char *s;
{
int t0;

	for (t0 = MAXJOB-1; t0 >= 0; t0--)
		if (jobtab[t0].stat && jobtab[t0].procs && t0 != thisjob && 
				jobtab[t0].procs->text && prefix(s,jobtab[t0].procs->text))
			return t0;
	return -1;
}

int isanum(s) /**/
char *s;
{
	while (*s == '-' || idigit(*s)) s++;
	return *s == '\0';
}

int bin_kill(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
int sig = SIGTERM;
int retval = 0;

	if (*argv && **argv == '-') {
		if (idigit((*argv)[1]))
			sig = atoi(*argv+1);
		else {
			if ((*argv)[1] == 'l' && (*argv)[2] == '\0') {
				printf("%s",sigs[1]);
				for (sig = 2; sig != SIGCOUNT; sig++)
					printf(" %s",sigs[sig]);
				putchar('\n');
				return 0;
			}
			for (sig = 0; sig != SIGCOUNT; sig++)
				if (!strcmp(sigs[sig],*argv+1)) break;
			if (sig == SIGCOUNT) {
				zerrnam(nam,"unknown signal: SIG%s",*argv+1,0);
				zerrnam(nam,"type kill -l for a List of signals",NULL,0);
				return 1;
			}
		}
		argv++;
	}
	for (; *argv; argv++) {
		if (**argv == '%') {
			int p = getjob(*argv,"kill");

			if (p == -1) {
				retval = 1;
				continue;
			}
			if (killjb(jobtab+p,sig) == -1) {
				zerrnam("kill","kill failed: %e",NULL,errno);
				retval = 1;
				continue;
			}
			if (jobtab[p].stat & STAT_STOPPED) {
				if (sig == SIGCONT)
					jobtab[p].stat &= ~STAT_STOPPED;
				if (sig != SIGKILL && sig != SIGCONT && sig != SIGTSTP
						&& sig != SIGTTOU && sig != SIGTTIN && sig != SIGSTOP)
					killjb(jobtab+p,SIGCONT);
			}
		} else if (!isanum(*argv)) {
			zerrnam("kill","illegal pid: %s",*argv,0);
		} else if (kill(atoi(*argv),sig) == -1) {
				zerrnam("kill","kill failed: %e",NULL,errno);
				retval = 1;
		}
	}
	return 0;
}

static char *recs[] = {
	"cputime","filesize","datasize","stacksize","coredumpsize",
	"resident","descriptors"
	};

int bin_limit(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
#ifndef RLIM_INFINITY
	zerrnam(nam,"not available on this system",NULL,0);
	return 1;
#else
char *s;
int hard = ops['h'],t0,lim;
long val;

	if (ops['s'])
		{
		if (*argv)
			zerrnam(nam,"arguments after -s ignored",NULL,0);
		for (t0 = 0; t0 != RLIM_NLIMITS; t0++)
			if (setrlimit(t0,limits+t0) < 0)
				zerrnam(nam,"setrlimit failed: %e",NULL,errno);
		return 0;
		}
	if (!*argv)
		{
		showlimits(hard,-1);
		return 0;
		}
	while (s = *argv++)
		{
		for (lim = -1, t0 = 0; t0 != RLIM_NLIMITS; t0++)
			if (!strncmp(recs[t0],s,strlen(s)))
				{
				if (lim != -1)
					lim = -2;
				else
					lim = t0;
				}
		if (lim < 0)
			{
			zerrnam("limit",
				(lim == -2) ? "ambiguous resource specification: %s"
								: "no such resource: %s",s,0);
			return 1;
			}
		if (!(s = *argv++))
			{
			showlimits(hard,lim);
			return 0;
			}
		if (!lim)
			{
			val = zstrtol(s,&s,10);
			if (*s)
				if ((*s == 'h' || *s == 'H') && !s[1])
					val *= 3600L;
				else if ((*s == 'm' || *s == 'M') && !s[1])
					val *= 60L;
				else if (*s == ':')
					val = val*60+zstrtol(s+1,&s,10);
				else
					{
					zerrnam("limit","unknown scaling factor: %s",s,0);
					return 1;
					}
			}
#ifdef RLIMIT_NOFILE
		else if (lim == RLIMIT_NOFILE)
			val = zstrtol(s,&s,10);
#endif
		else
			{
			val = zstrtol(s,&s,10);
			if (!*s || ((*s == 'k' || *s == 'K') && !s[1]))
				val *= 1024L;
			else if ((*s == 'M' || *s == 'm') && !s[1])
				val *= 1024L*1024;
			else
				{
				zerrnam("limit","unknown scaling factor: %s",s,0);
				return 1;
				}
			}
		if (hard)
			if (val > limits[lim].rlim_max && geteuid())
				{
				zerrnam("limit","can't raise hard limits",NULL,0);
				return 1;
				}
			else
				{
				limits[lim].rlim_max = val;
				if (limits[lim].rlim_max < limits[lim].rlim_cur)
					limits[lim].rlim_cur = limits[lim].rlim_max;
				}
		else
			if (val > limits[lim].rlim_max)
				{
				zerrnam("limit","limit exceeds hard limit",NULL,0);
				return 1;
				}
			else
				limits[lim].rlim_cur = val;
		}
	return 0;
#endif
}

int bin_unlimit(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
#ifndef RLIM_INFINITY
	zerrnam(nam,"not available on this system",NULL,0);
	return 1;
#else
int hard = ops['h'],t0,lim;

	if (hard && geteuid())
		{
		zerrnam(nam,"can't remove hard limits",NULL,0);
		return 1;
		}
	if (!*argv)
		{
		for (t0 = 0; t0 != RLIM_NLIMITS; t0++)
			{
			if (hard)
				limits[t0].rlim_max = RLIM_INFINITY;
			else
				limits[t0].rlim_cur = limits[t0].rlim_max;
			}
		return 0;
		}
	for (; *argv; argv++)
		{
		for (lim = -1, t0 = 0; t0 != RLIM_NLIMITS; t0++)
			if (!strncmp(recs[t0],*argv,strlen(*argv)))
				{
				if (lim != -1)
					lim = -2;
				else
					lim = t0;
				}
		if (lim < 0)
			{
			zerrnam(nam,
				(lim == -2) ? "ambiguous resource specification: %s"
								: "no such resource: %s",*argv,0);
			return 1;
			}
		if (hard)
			limits[lim].rlim_max = RLIM_INFINITY;
		else
			limits[lim].rlim_cur = limits[lim].rlim_max;
		}
	return 0;
#endif
}

void showlimits(hard,lim) /**/
int hard;int lim;
{
int t0;
long val;

#ifdef RLIM_INFINITY
	for (t0 = 0; t0 != RLIM_NLIMITS; t0++)
		if (t0 == lim || lim == -1)
			{
			printf("%-16s",recs[t0]);
			val = (hard) ? limits[t0].rlim_max : limits[t0].rlim_cur;
			if (val == RLIM_INFINITY)
				printf("unlimited\n");
			else if (!t0)
				printf("%d:%02d:%02d\n",(int) (val/3600),
					(int) (val/60) % 60,(int) (val % 60));
#ifdef RLIMIT_NOFILE
			else if (t0 == RLIMIT_NOFILE)
				printf("%d\n",(int) val);
#endif
			else if (val >= 1024L*1024L)
				printf("%ldMb\n",val/(1024L*1024L));
			else
				printf("%ldKb\n",val/1024L);
			}
#endif
}

int bin_sched(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
char *s = *argv++;
time_t t;
long h,m;
struct tm *tm;
struct schedcmd *sch,*sch2,*schl;
int t0;

	if (s && *s == '-')
		{
		t0 = atoi(s+1);

		if (!t0)
			{
			zerrnam("sched","usage for delete: sched -<item#>.",NULL,0);
			return 1;
			}
		for (schl = (struct schedcmd *) &schedcmds, sch = schedcmds, t0--;
				sch && t0; sch = (schl = sch)->next, t0--);
		if (!sch)
			{
			zerrnam("sched","not that many entries",NULL,0);
			return 1;
			}
		schl->next = sch->next;
		free(sch->cmd);
		free(sch);
		return 0;
		}
	if (!s)
		{
		char tbuf[40];

		for (t0 = 1, sch = schedcmds; sch; sch = sch->next,t0++)
			{
			t = sch->time;
			tm = localtime(&t);
			ztrftime(tbuf,20,"%a %b %e %k:%M:%S",tm);
			printf("%3d %s %s\n",t0,tbuf,sch->cmd);
			}
		return 0;
		}
	else if (!*argv)
		{
		zerrnam("sched","not enough arguments",NULL,0);
		return 1;
		}
	if (*s == '+')
		{
		h = zstrtol(s+1,&s,10);
		if (*s != ':')
			{
			zerrnam("sched","bad time specifier",NULL,0);
			return 1;
			}
		m = zstrtol(s+1,&s,10);
		if (*s)
			{
			zerrnam("sched","bad time specifier",NULL,0);
			return 1;
			}
		t = time(NULL)+h*3600+m*60;
		}
	else
		{
		h = zstrtol(s,&s,10);
		if (*s != ':')
			{
			zerrnam("sched","bad time specifier",NULL,0);
			return 1;
			}
		m = zstrtol(s+1,&s,10);
		if (*s && *s != 'a' && *s != 'p')
			{
			zerrnam("sched","bad time specifier",NULL,0);
			return 1;
			}
		t = time(NULL);
		tm = localtime(&t);
		t -= tm->tm_sec+tm->tm_min*60+tm->tm_hour*3600;
		if (*s == 'p')
			h += 12;
		t += h*3600+m*60;
		if (t < time(NULL))
			t += 3600*24;
		}
	sch = zcalloc(sizeof *sch);
	sch->time = t;
	sch->cmd = ztrdup(spacejoin(argv));
	sch->next = NULL;
	for (sch2 = (struct schedcmd *) &schedcmds; sch2->next; sch2 = sch2->next);
	sch2->next = sch;
	return 0;
}

int bin_eval(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
char *s = ztrdup(spacejoin(argv));
List list;

	hungets(s);
	free(s);
	strinbeg();
	if (!(list = parse_list()))
		{
		hflush();
		strinend();
		return 1;
		}
	strinend();
	runlist(list);
	return lastval;
}

/* get the history event associated with s */

int fcgetcomm(s) /**/
char *s;
{
int cmd;

	if (cmd = atoi(s))
		{
		if (cmd < 0)
			cmd = curhist+cmd+1;
		return cmd;
		}
	cmd = hcomsearch(s);
	if (cmd == -1)
		zerrnam("fc","event not found: %s",s,0);
	return cmd;
}

/* perform old=new substituion */

int fcsubs(sp,sub) /**/
char **sp;struct asgment *sub;
{
char *s1,*s2,*s3,*s4,*s = *sp,*s5;
int subbed = 0;

	while (sub)
		{
		s1 = sub->name;
		s2 = sub->value;
		sub = sub->next;
		s5 = s;
		while (s3 = (char *) ztrstr(s5,s1))
			{
			s4 = alloc(1+(s3-s)+strlen(s2)+strlen(s3+strlen(s1)));
			ztrncpy(s4,s,s3-s);
			strcat(s4,s2);
			s5 = s4+strlen(s4);
			strcat(s4,s3+strlen(s1));
			s = s4;
			subbed = 1;
			}
		}
	*sp = s;
	return subbed;
}

/* print a series of history events to a file */

int fclist(f,n,r,D,d,first,last,subs) /**/
FILE *f;int n;int r;int D;int d;int first;int last;struct asgment *subs;
{
int done = 0;
char *s,*hs;
Histent ent;

	if (!subs) done = 1;
	for (;;) {
		hs = quietgetevent(first);
		if (!hs) {
			zerrnam("fc","no such event: %d",NULL,first);
			return 1;
		}
		s = makehstr(hs);
		done |= fcsubs(&s,subs);
		if (n) fprintf(f,"%5d  ",first);
		ent = NULL;
		if (d) {
			struct tm *ltm;
			
			if (!ent) ent = gethistent(first);
			ltm = localtime(&ent->stim);
			fprintf(f,"%2d:%02d  ",ltm->tm_hour,ltm->tm_min);
		}
		if (D) {
			long diff;

			if (!ent) ent = gethistent(first);
			diff = (ent->ftim) ? ent->ftim-ent->stim : 0;
			fprintf(f,"%d:%02d  ",diff/60,diff%60);
		}
		if (f == stdout) {
			niceprintf(s,f);
			putc('\n',f);
		} else fprintf(f,"%s\n",s);
		if (first == last) break;
		(r) ? first-- : first++;
	}
	if (f != stdout) fclose(f);
	if (!done) {
		zerrnam("fc","no substitutions performed",NULL,0);
		return 1;
	}
	return 0;
}

int fcedit(ename,fn) /**/
char *ename;char *fn;
{
	if (!strcmp(ename,"-"))
		return 1;
	return !zyztem(ename,fn);
}

/* fc, history, r */

int bin_fc(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
int first = -1,last = -1,retval,minflag = 0;
char *s;
struct asgment *asgf = NULL,*asgl = NULL;

	if (!interact) {
		zerrnam(nam,"not interactive shell",NULL,0);
		return 1;
	}
	if (!(ops['l'] && unset(HISTNOSTORE))) remhist();
	if (ops['R']) {
		readhistfile(*argv ? *argv : getsparam("HISTFILE"),1);
		return 0;
	}
	if (ops['W']) {
		savehistfile(*argv ? *argv : getsparam("HISTFILE"),1,0);
		return 0;
	}
	if (ops['A']) {
		savehistfile(*argv ? *argv : getsparam("HISTFILE"),1,1);
		return 0;
	}
	while (*argv && equalsplit(*argv,&s)) {
		struct asgment *a = (struct asgment *) alloc(sizeof *a);

		if (!asgf) asgf = asgl = a;
		else {
			asgl->next = a;
			asgl = a;
		}
		a->name = *argv;
		a->value = s;
		argv++;
	}
	if (*argv) {
		minflag = **argv == '-';
		first = fcgetcomm(*argv);
		if (first == -1) return 1;
		argv++;
	}
	if (*argv) {
		last = fcgetcomm(*argv);
		if (last == -1) return 1;
		argv++;
	}
	if (*argv) {
		zerrnam("fc","too many arguments",NULL,0);
		return 1;
	}
	if (first == -1) first = (ops['l']) ? curhist-16 : curhist;
	if (last == -1) last = (ops['l']) ? curhist : first;
	if (first < firsthist()) first = firsthist();
	if (last == -1) last = (minflag) ? curhist : first;
	if (ops['l'])
		retval = fclist(stdout,!ops['n'],ops['r'],ops['D'],ops['d'],
			first,last,asgf);
	else {
		FILE *out;
		char *fil = gettemp();

		out = fopen(fil,"w");
		if (!out)
			zerrnam("fc","can't open temp file: %e",NULL,errno);
		else {
			retval = 1;
			if (!fclist(out,0,ops['r'],0,0,first,last,asgf))
				if (fcedit(auxdata ? auxdata : fceditparam,fil))
					if (stuff(fil))
						zerrnam("fc","%e: %s",s,errno);
					else
						retval = 0;
		}
		unlink(fil);
	}
	return retval;
}

int bin_suspend(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
	if (islogin && !ops['f']) {
		zerrnam(name,"can't suspend login shell",NULL,0);
		return 1;
	}
	if (jobbing) {
		signal(SIGPIPE,SIG_DFL);
		signal(SIGTTIN,SIG_DFL);
		signal(SIGTSTP,SIG_DFL);
		signal(SIGTTOU,SIG_DFL);
	}
	kill(0,SIGTSTP);
	if (jobbing) {
		while (gettygrp() != mypgrp) {
			sleep(1);
			if (gettygrp() != mypgrp) kill(0,SIGTTIN);
		}
		signal(SIGTTOU,SIG_IGN);
		signal(SIGTSTP,SIG_IGN);
		signal(SIGTTIN,SIG_IGN);
		signal(SIGPIPE,SIG_IGN);
	}
	return 0;
}

int bin_alias(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
struct alias *an;
struct asgment *asg;
int incm = !(ops['a'] || ops['g']),ret = 0;

	showflag = !incm;
	if (!*argv)
		listhtable(aliastab,(HFunc) printalias);
	else while (asg = getasg(*argv++))
		{
		if (asg->value)
			addhnode(ztrdup(asg->name),mkanode(ztrdup(asg->value),incm),
				aliastab,freeanode);
		else if (an = (Alias) gethnode(asg->name,aliastab))
			printalias(asg->name,an);
		else
			ret = 1;
		}
	return ret;
}

/* print an alias; used with listhtable */

void printalias(s,a) /**/
char *s;struct alias *a;
{
	if (a->cmd >= 0 && !(showflag && a->cmd))
		printf("%s=%s\n",s,a->text);
}

/* print a param; used with listhtable */

void printparam(s,p) /**/
char *s;Param p;
{
	if (showflag > 0 && !(p->flags & showflag))
		return;
	if (!showflag)
		{
		int fgs = p->flags;

		if (fgs & PMFLAG_i) printf("integer ");
		if (fgs & PMFLAG_A) printf("array ");
		if (fgs & PMFLAG_L) printf("left justified %d ",p->ct);
		if (fgs & PMFLAG_R) printf("right justified %d ",p->ct);
		if (fgs & PMFLAG_Z) printf("zero filled %d ",p->ct);
		if (fgs & PMFLAG_l) printf("lowercase ");
		if (fgs & PMFLAG_u) printf("uppercase ");
		if (fgs & PMFLAG_r) printf("readonly ");
		if (fgs & PMFLAG_t) printf("tagged ");
		if (fgs & PMFLAG_x) printf("exported ");
		}
	if (showflag2)
		printf("%s\n",s);
	else
		{
		char *t,**u;

		printf("%s=",s);
		switch (p->flags & PMTYPE)
			{
			case PMFLAG_s:
				if (p->gets.cfn && (t = p->gets.cfn(p)))
					puts(t);
				else
					putchar('\n');
				break;
			case PMFLAG_i: printf("%ld\n",p->gets.ifn(p)); break;
			case PMFLAG_A:
				putchar('(');
				u = p->gets.afn(p);
				if (!*u)
					printf(")\n");
				else
					{
					while (u[1])
						printf("%s ",*u++);
					printf("%s)\n",*u);
					}
				break;
			}
		}
}

/* autoload, declare, export, functions, integer, local, readonly, typeset */

int bin_typeset(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int on = 0,off = 0,roff,bit = 1,retcode = 0;
char *optstr = "LRZilurtx";
struct param *pm;
struct asgment *asg;

	for (; *optstr; optstr++,bit <<= 1)
		if (ops[*optstr] == 1)
			on |= bit;
		else if (ops[*optstr] == 2)
			off |= bit;
	roff = off;
	if (ops['f']) {
		on &= PMFLAG_t|PMFLAG_u;
		off &= PMFLAG_t|PMFLAG_u;
		showflag = (ops['f'] == 1);
		if (ops['@'] && ((off & ~PMFLAG_t) || (on & ~(PMFLAG_u|PMFLAG_t)))) {
			zerrnam(name,"invalid option(s)",NULL,0);
			return 1;
		}
		showflag2 = 0;
		if (!*argv) {
			showflag2 = off|on;
			listhtable(cmdnamtab,(HFunc) pshfunc);
		} else for (; *argv; argv++) {
			Cmdnam cc;

			if ((cc = (Cmdnam) gethnode(*argv,cmdnamtab)) && cc->type == SHFUNC)
				if (on|off) cc->flags = (cc->flags | on) & (~off);
				else pshfunc(*argv,cc);
			else if (on & PMFLAG_u) {
				cc = (Cmdnam) zcalloc(sizeof *cc);
				cc->type = SHFUNC;
				cc->flags = on;
				addhnode(ztrdup(*argv),cc,cmdnamtab,freecmdnam);
			} else
				retcode = 1;
		}
		return retcode;
	}
	if (on & PMFLAG_L)
		off |= PMFLAG_R;
	if (on & PMFLAG_R)
		off |= PMFLAG_L;
	if (on & PMFLAG_u)
		off |= PMFLAG_l;
	if (on & PMFLAG_l)
		off |= PMFLAG_u;
	on &= ~off;
	showflag = showflag2 = 0;
	if (!*argv) {
		showflag = on|off;
		showflag2 = roff;
		listhtable(paramtab,(HFunc) printparam);
	} else while (asg = getasg(*argv++)) {
		if (asg->value && *asg->value == '~') {
			*asg->value = Tilde;
			singsub(&asg->value);
		}
		pm = (Param) gethnode(asg->name,paramtab);
		if (pm) {
			if (!on && !roff && !asg->value) {
				printparam(asg->name,pm);
				continue;
			}
			pm->flags = (pm->flags | on) & ~off;
			if ((on & (PMFLAG_L | PMFLAG_R | PMFLAG_Z | PMFLAG_i)) 
					&& (pmtype(pm) != PMFLAG_A))
				pm->ct = auxlen;
			if (pmtype(pm) != PMFLAG_A) {
				if (pm->flags & PMFLAG_x) {
					if (!pm->env)
						pm->env = addenv(asg->name,
							(asg->value) ? asg->value : getsparam(asg->name));
				} else if (pm->env) {
					delenv(pm->env);
					free(pm->env);
					pm->env = NULL;
				}
				if (asg->value)
					setsparam(asg->name,ztrdup(asg->value));
			}
		} else {
			if (locallist && !(on & PMFLAG_x)) {
				permalloc();
				addnode(locallist,ztrdup(asg->name));
				heapalloc();
			}
			createparam(ztrdup(asg->name),
				ztrdup((asg->value) ? asg->value : ""),on);
			pm = (Param) gethnode(asg->name,paramtab);
			pm->ct = auxlen;
		}
	}
	return 0;
}

/* convert s with escape sequences */

char *escsubst(s,nnl) /**/
char *s; int *nnl;
{
char *t = alloc(strlen(s)+1),*ret = t;

	for (; *s; s++)
		if (*s == '\\' && s[1])
			switch (*++s) {
				case 'b': *t++ = '\b'; break;
				case 'c': *nnl |= 1; break;
				case 'e': *t++ = '\033'; break;
				case 'f': *t++ = '\f'; break;
				case 'n': *t++ = '\n'; break;
				case 'r': *t++ = '\r'; break;
				case 't': *t++ = '\t'; break;
				case 'v': *t++ = '\v'; break;
				case '\\': *t++ = '\\'; break;
				case '0': *t++ = zstrtol(s,&s,8); s--; break;
				default: *t++ = '\\'; *t++ = *s; break;
			}
		else *t++ = *s;
	*t = '\0';
	return ret;
}

/* echo, print, pushln */

int bin_print(name,args,ops,func) /**/
char *name;char **args;char *ops;int func;
{
int nnl = 0, fd;
Histent ent;
FILE *fout = stdout;

	if (ops['z']) {
		permalloc();
		pushnode(bufstack,ztrdup(spacejoin(args)));
		heapalloc();
		return 0;
	}
	if (ops['s']) {
		permalloc();
		ent = gethistent(++curhist);
		ent->lex = ztrdup(join(args,HISTSPACE));
		ent->lit = ztrdup(join(args,' '));
		ent->stim = ent->ftim = time(NULL);
		heapalloc();
		return 0;
	}
	if (ops['R'])
		ops['r'] = 1;
	if (ops['u'] || ops['p']) {
		if (ops['u']) {
			for (fd = 0; fd < 10; fd++) if (ops[fd+'0']) break;
			if (fd == 10) fd = 0;
		} else fd = coprocout;
		if ((fd = dup(fd)) < 0) {
			zerrnam(name,"bad file number",NULL,0);
			return 1;
		}
		if ((fout = fdopen(fd,"w")) == 0) {
			zerrnam(name,"bad mode on fd",NULL,0);
			return 1;
		}
	}
	for (; *args; args++) {
		if (!ops['r']) *args = escsubst(*args,&nnl);
		if (ops['D']) fprintdir(*args,fout);
		else if (ops['P']) {
			int junk;
			fputs(putprompt(*args,&junk),fout);
		} else fputs(*args,fout);
		if (args[1]) fputc(ops['l'] ? '\n' : ops['0'] ? '\0' : ' ',fout);
	}
	if (!(ops['n'] || nnl)) fputc(ops['N'] ? '\0' : '\n',fout);
	if (fout != stdout) fclose(fout);
	return 0;
}

int bin_dirs(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
Lklist l;

	if (ops['v'])
		{
		Lknode node;
		int t0 = 1;

		printf("0\t");
		printdir(pwd);
		for (node = firstnode(dirstack); node; incnode(node))
			{
			printf("\n%d\t",t0++);
			printdir(getdata(node));
			}
		putchar('\n');
		return 0;
		}
	if (!*argv)
		{
		pdstack();
		return 0;
		}
	permalloc();
	l = newlist();
	if (!*argv)
		{
		heapalloc();
		return 0;
		}
	while (*argv)
		addnode(l,ztrdup(*argv++));
	freetable(dirstack,freestr);
	dirstack = l;
	heapalloc();
	return 0;
}

int bin_unalias(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int ret = 0;
vptr dat;

	while (*argv)
		{
		if (dat = remhnode(*argv++,aliastab))
			freeanode(dat);
		else
			ret = 1;
		}
	return ret;
}

int bin_disable(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
Cmdnam chn;

	while (*argv) {
		if (!strncmp(*argv,"TRAP",4))
			unsettrap(getsignum(*argv+4));
		chn = zalloc(sizeof *chn);
		chn->type = DISABLED;
		addhnode(ztrdup(*argv++),chn,cmdnamtab,freecmdnam);
	}
	return 0;
}

int bin_unhash(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
vptr dat;

	while (*argv) {
		if (!strncmp(*argv,"TRAP",4)) unsettrap(getsignum(*argv+4));
		if (dat = remhnode(*argv++,cmdnamtab)) freecmdnam(dat);
	}
	return 0;
}

int bin_unset(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int retval = 0;
char *s;

	while (s = *argv++)
		if (gethnode(s,paramtab))
			unsetparam(s);
		else
			retval = 1;
	return retval;
}

static char *zbuf;
static int readfd;

int zread() /**/
{
char cc;

	if (zbuf)
		return (*zbuf) ? *zbuf++ : EOF;
	if (read(readfd,&cc,1) != 1)
		return EOF;
	return cc;
}

int bin_read(name,args,ops,func) /**/
char *name;char **args;char *ops;int func;
{
char *reply,*pmpt;
int bsiz,c,gotnl = 0;
char *buf,*bptr;

	reply = (*args) ? *args++ : "REPLY";
	if (ops['u'] && !ops['p']) {
		for (readfd = 0; readfd < 10; ++readfd) if (ops[readfd+'0']) break;
		if (readfd == 10) readfd = 0;
	} else if (ops['p']) readfd = coprocin;
	else {
		attachtty((jobtab[thisjob].gleader) ? jobtab[thisjob].gleader : mypgrp);
		readfd = 0;
		if (isatty(0)) {
			for (pmpt = reply; *pmpt && *pmpt != '?'; pmpt++);
			if (*pmpt++) {
				write(2,pmpt,strlen(pmpt));
				pmpt[-1] = '\0';
			}
		}
#if 0
		else if (isset(SHINSTDIN) && unset(INTERACTIVE)) {
			if (isatty(1)) readfd = 1;
			else if (isatty(2)) readfd = 2;
		}
#endif
	}
	zbuf = (!ops['z']) ? NULL :
		(full(bufstack)) ? (char *) getnode(bufstack) : NULL;
	while (*args) {
		buf = bptr = zalloc(bsiz = 64);
redo:
		for(;;) {
			if (gotnl) break;
			c = zread();
			if (!ops['r'] && c == '\n' && bptr != buf && bptr[-1] == '\\') {
				bptr--;
				continue;
			}
			if (c == EOF || isep(c) || c == '\n') break;
			*bptr++ = c;
			if (bptr == buf+bsiz) {
				buf = realloc(buf,bsiz *= 2);
				bptr = buf+(bsiz/2);
			}
		}
		if (c == EOF) {
			if (readfd == coprocin) {
				close(coprocin);
				close(coprocout);
				coprocin = coprocout = -1;
			}
			return 1;
		}
		if (c == '\n') gotnl = 1;
		if (bptr == buf) goto redo;
		*bptr = '\0';
		setsparam(reply,buf);
		reply = *args++;
	}
	buf = bptr = zalloc(bsiz = 64);
	if (!gotnl)
		for (;;) {
			c = zread();
			if (!ops['r'] && c == '\n' && bptr != buf && bptr[-1] == '\\') {
				bptr--;
				continue;
			}
			if (c == EOF || (c == '\n' && !zbuf)) break;
			*bptr++ = c;
			if (bptr == buf+bsiz) {
				buf = realloc(buf,bsiz *= 2);
				bptr = buf+(bsiz/2);
			}
		}
	*bptr = '\0';
	setsparam(reply,buf);
	if (c == EOF) {
		if (readfd == coprocin) {
			close(coprocin);
			close(coprocout);
			coprocin = coprocout = -1;
		}
		return 1;
	}
	return 0;
}

int bin_vared(name,args,ops,func) /**/
char *name;char **args;char *ops;int func;
{
char *s;char *t;
struct param *pm;

	if (!(s = getsparam(args[0]))) {
		zerrnam(name,"no such variable: %s",args[0],0);
		return 1;
	}
	permalloc();
	pushnode(bufstack,ztrdup(s));
	heapalloc();
	t = (char *) zleread((unsigned char *)"> ",NULL,2);
	if (!t || errflag)
		return 1;
	if (t[strlen(t)-1] == '\n')
		t[strlen(t)-1] = '\0';
	pm = gethnode(args[0],paramtab);
	if (pmtype(pm) == PMFLAG_A)
		setaparam(args[0],spacesplit(t));
	else
		setsparam(args[0],t);
	return 0;
}

#define fset(X) (flags & X)

/* execute a builtin handler function after parsing the arguments */

int execbin(args,cnode) /**/
Lklist args;Cmdnam cnode;
{
struct bincmd *b;
char ops[128],*arg,*pp,*name,**argv,**oargv,*optstr;
int t0,flags,sense,argc = 0,op;
Lknode n;

	auxdata = NULL;
	auxlen = 0;
	for (t0 = 0; t0 != 128; t0++)
		ops[t0] = 0;
	name = ugetnode(args);
	b = builtins+cnode->u.binnum;

/* the 'builtin' builtin is handled specially */

	if (b->funcid == BIN_BUILTIN)
		{
		if (!(name = ugetnode(args)))
			{
			zerrnam("builtin","command name expected",NULL,0);
			return 1;
			}
		for (t0 = 0, b = builtins; b->name; b++,t0++)
			if (!strcmp(name,b->name))
				break;
		if (!b->name)
			{
			zerrnam("builtin","no such builtin: %s",name,0);
			return 1;
			}
		}
	flags = b->flags;
	arg = ugetnode(args);
	optstr = b->optstr;
	if (flags & BINF_ECHOPTS && arg && strcmp(arg,"-n"))
		optstr = NULL;
	if (optstr)
		while (arg &&
				((sense = *arg == '-') || fset(BINF_PLUSOPTS) && *arg == '+') &&
				(fset(BINF_PLUSOPTS) || !atoi(arg)))
			{
			pp = arg;
			if (arg[1] == '-')
				arg++;
			if (!arg[1])
				{
				ops['-'] = 1;
				if (!sense)
					ops['+'] = 1;
				}
			else
				ops['@'] = 1;
			op = -1;
			while (*++arg)
				if (strchr(b->optstr,op = *arg))
					ops[*arg] = (sense) ? 1 : 2;
				else
					break;
			if (*arg)
				{
				zerr("bad option: %c",NULL,*arg);
				return 1;
				}
			arg = ugetnode(args);
			if (fset(BINF_SETOPTS) && op == 'o')
				{
				int c;
				
				if (!arg)
					prtopt();
				else
					{
					c = optlookup(arg);
					if (c == -1)
						{
						zerr("bad option: %s",arg,0);
						return 1;
						}
					else
						{
						ops[c] = ops['o'];
						arg = ugetnode(args);
						}
					}
				}
			if ((fset(BINF_PRINTOPTS) && ops['R']) || ops['-'])
				break;
			if (fset(BINF_SETOPTS) && ops['A'])
				{
				auxdata = arg;
				arg = ugetnode(args);
				break;
				}
			if (fset(BINF_FCOPTS) && op == 'e')
				{
				auxdata = arg;
				arg = ugetnode(args);
				}
			if (fset(BINF_TYPEOPT) && (op == 'L' || op == 'R' ||
					op == 'Z' || op == 'i') && arg && idigit(*arg))
				{
				auxlen = atoi(arg);
				arg = ugetnode(args);
				}
			}
	if (fset(BINF_R))
		auxdata = "-";
	if (pp = b->defopts)
		while (*pp)
			ops[*pp++] = 1;
	if (arg)
		{
		argc = 1;
		n = firstnode(args);
		while (n)
			argc++,incnode(n);
		}
	oargv = argv = (char **) ncalloc(sizeof(char **) * (argc+1));
	if (*argv++ = arg)
		while (*argv++ = ugetnode(args));
	argv = oargv;
	if (errflag)
		return 1;
	if (argc < b->minargs || (argc > b->maxargs && b->maxargs != -1)) {
		zerrnam(name,(argc < b->minargs)
			? "not enough arguments" : "too many arguments",NULL,0);
		return 1;
	}
	if (isset(XTRACE)) {
		char **pp = argv;
		fprintf(stderr,"%s%s",(prompt4) ? prompt4 : "",name);
		while (*pp) fprintf(stderr," %s",*pp++);
		fputc('\n',stderr);
		fflush(stderr);
	}
	return (*(b->handlerfunc))(name,argv,ops,b->funcid);
}

struct asgment *getasg(s) /**/
char *s;
{
static struct asgment asg;

	if (!s)
		return NULL;
	if (*s == '=')
		{
		zerr("bad assignment",NULL,0);
		return NULL;
		}
	asg.name = s;
	for (; *s && *s != '='; s++);
	if (*s)
		{
		*s = '\0';
		asg.value = s+1;
		}
	else
		asg.value = NULL;
	return &asg;
}

/* ., source */

int bin_dot(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
char **old,*old0;
int ret;
char buf[MAXPATHLEN];
char *s,**t,*enam;

	if (!*argv)
		return 0;
	old = pparams;
	old0 = argzero;
	if (argv[1]) {
		permalloc();
		pparams = arrdup(argv+1);
		heapalloc();
	}
	enam = argzero = ztrdup(*argv);
	errno = ENOENT;
	ret = 1;
	for (s = argzero; *s; s++)
		if (*s == '/') {
			ret = source(argzero);
			break;
		}
	if (!*s) {
		for (t = path; *t; t++)
			if ((*t)[0] == '.' && !(*t)[1]) {
				ret = source(argzero);
				break;
			} else {
				sprintf(buf,"%s/%s",*t,argzero);
				if (access(buf,F_OK) == 0) {
					ret = source(enam = buf);
					break;
				}
			}
		if (!*t && access(argzero,F_OK) == 0)
			ret = source(enam = argzero);
	}
	if (argv[1]) {
		freearray(pparams);
		pparams = old;
	}
	if (ret) zerrnam(name,"%e: %s",enam,errno);
	free(argzero);
	argzero = old0;
	return ret;
}

int bin_set(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
struct option *opp;
char **x;

	if (((ops['+'] && ops['-']) || !ops['-']) && !ops['@'] && !*argv)
		{
		showflag = ~0;
		showflag2 = ops['+'];
		listhtable(paramtab,(HFunc) printparam);
		}
   for (opp = optns; opp->name; opp++)
      if (ops[opp->id] == 1)
         opts[opp->id] = OPT_SET;
      else if (ops[opp->id] == 2)
         opts[opp->id] = OPT_UNSET;
	if (!*argv && !ops['-'])
		return 0;
	permalloc();
	x = arrdup(argv);
	heapalloc();
	if (ops['A'])
		setaparam(auxdata,x);
	else {
		freearray(pparams);
		pparams = x;
	}
	return 0;
}

#define pttime(X) printf("%dm%ds",(X)/3600,(X)/60%60)

int bin_times(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
struct tms buf;

	if (times(&buf) == -1)
		return 1;
	pttime(buf.tms_utime);
	putchar(' ');
	pttime(buf.tms_stime);
	putchar('\n');
	pttime(buf.tms_cutime);
	putchar(' ');
	pttime(buf.tms_cstime);
	putchar('\n');
	return 0;
}

int bin_getopts(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
char *optstr = *argv++,*var = *argv++;
char **args = (*argv) ? argv : pparams;
static int optcind = 1,quiet;
char *str,optbuf[3],*opch = optbuf+1;

	if (zoptind < 1) zoptind = 1;
	optbuf[0] = '+'; optbuf[1] = optbuf[2] = '\0';
	if (optarg) free(optarg);
	optarg = ztrdup("");
	setsparam(var,ztrdup(""));
	if (*optstr == ':') {
		quiet = 1;
		optstr++;
	}
	if (zoptind > arrlen(args)) return 1;
	str = args[zoptind-1];
	if (*str != '+' && *str != '-' || optcind >= strlen(str) ||
			!strcmp("--",str)) {
		if (*str == '+' || *str == '-')
			zoptind++;
		optcind = 0;
		return 1;
	}
	if (!optcind)
		optcind = 1;
	*opch = str[optcind++];
	if (!args[zoptind-1][optcind]) {
		zoptind++;
		optcind = 0;
	}
	for (; *optstr; optstr++)
		if (*opch == *optstr)
			break;
	if (!*optstr) {
		setsparam(var,ztrdup("?"));
		if (quiet) {
			free(optarg); optarg = ztrdup(opch);
			return 0;
		}
		zerr("bad option: %c",NULL,*opch); errflag = 0;
		return 0;
	}
	setsparam(var,ztrdup(opch-(*str == '+')));
	if (optstr[1] == ':') {
		if (!args[zoptind-1]) {
			if (quiet) {
				free(optarg); optarg = ztrdup(opch);
				setsparam(var,ztrdup(":"));
				return 0;
			}
			setsparam(var,ztrdup("?"));
			zerr("argument expected after %c option",NULL,*opch); errflag = 0;
			return 0;
		}
		free(optarg);
		optarg = ztrdup(args[zoptind-1]+optcind);
		zoptind++;
		optcind = 0;
	}
	return 0;
}

/* get a signal number from a string */

int getsignum(s) /**/
char *s;
{
int x = atoi(s),t0;

	if (idigit(*s) && x >= 0 && x < VSIGCOUNT)
		return x;
	for (t0 = 0; t0 != VSIGCOUNT; t0++)
		if (!strcmp(s,sigs[t0]))
			return t0;
	return -1;
}

int bin_trap(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
List l;
char *arg;

	if (!*argv) {
		int t0;

		for (t0 = 0; t0 != VSIGCOUNT; t0++)
			if (sigtrapped[t0])
				if (!sigfuncs[t0])
					printf("TRAP%s () {}\n",sigs[t0]);
				else {
					char *s = getpermtext((vptr) sigfuncs[t0]);
					printf("TRAP%s () {\n\t%s\n}\n",sigs[t0],s);
					free(s);
				}
		return 0;
	}
	if (!strcmp(*argv,"-")) {
		int t0;

		argv++;
		if (!*argv)
			for (t0 = 0; t0 != VSIGCOUNT; t0++) unsettrap(t0);
		else
			while (*argv) unsettrap(getsignum(*argv++));
		return 0;
	}
	arg = *argv++;
	if (!*arg) l = NULL;
	else if (!(l = parselstring(arg))) {
		zerrnam(name,"couldn't parse trap command",NULL,0);
		popheap();
		return 1;
	}
	for (; *argv; argv++) {
		int sg = getsignum(*argv);
		if (sg == -1) {
			zerrnam(name,"undefined signal: %s",*argv,0);
			break;
		}
		settrap(sg,l);
	}
	if (l) popheap();
	return errflag;
}

void printulimit(lim,hard) /**/
int lim;int hard;
{
long t0;

#ifdef RLIM_INFINITY
	t0 = (hard) ? limits[lim].rlim_max : limits[lim].rlim_cur;
	switch (lim)
		{
		case RLIMIT_CPU: printf("cpu time (seconds)         "); break;
		case RLIMIT_FSIZE: printf("file size (blocks)         "); t0 /= 512; break;
		case RLIMIT_DATA: printf("data seg size (kbytes)     "); t0 /= 1024; break;
		case RLIMIT_STACK: printf("stack size (kbytes)        "); t0 /= 1024; break;
		case RLIMIT_CORE: printf("core file size (blocks)    "); t0 /= 512; break;
#ifdef RLIMIT_RSS
		case RLIMIT_RSS: printf("resident set size (kbytes) "); t0 /= 1024; break;
#endif
#ifdef RLIMIT_NOFILE
		case RLIMIT_NOFILE: printf("file descriptors           "); break;
#endif
		}
	printf("%ld\n",t0);
#endif
}

int bin_ulimit(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int res,hard;

#ifndef RLIM_INFINITY
	zerrnam(name,"not available on this system",NULL,0);
	return 1;
#else
	hard = ops['H'];
	if (ops['a'] || !ops['@'])
		res = -1;
	else if (ops['t'])
		res = RLIMIT_CPU;
	else if (ops['f'])
		res = RLIMIT_FSIZE;
	else if (ops['d'])
		res = RLIMIT_DATA;
	else if (ops['s'])
		res = RLIMIT_STACK;
	else if (ops['c'])
		res = RLIMIT_CORE;
#ifdef RLIMIT_RSS
	else if (ops['m'])
		res = RLIMIT_RSS;
#endif
#ifdef RLIMIT_NOFILE
	else if (ops['n'])
		res = RLIMIT_NOFILE;
#endif
	else
		{
		zerrnam(name,"no such limit",NULL,0);
		return 1;
		}
	if (res == -1)
		if (*argv)
			{
			zerrnam(name,"no arguments required after -a",NULL,0);
			return 1;
			}
		else
			{
			int t0;

			for (t0 = 0; t0 != RLIM_NLIMITS; t0++)
				printulimit(t0,hard);
			return 0;
			}
	if (!*argv)
		printulimit(res,hard);
	else if (strcmp(*argv,"unlimited"))
		{
		long t0;
		
		t0 = atol(*argv);
		switch(res)
			{
			case RLIMIT_FSIZE: case RLIMIT_CORE: t0 *= 512; break;
			case RLIMIT_DATA: case RLIMIT_STACK:
#ifdef RLIMIT_RSS
			case RLIMIT_RSS:
#endif
				t0 *= 1024; break;
			}
		if (hard)
			{
			if (t0 > limits[res].rlim_max && geteuid())
				{
				zerrnam(name,"can't raise hard limits",NULL,0);
				return 1;
				}
			limits[res].rlim_max = t0;
			}
		else
			{
			if (t0 > limits[res].rlim_max)
				{
				if (geteuid())
					{
					zerrnam(name,"value exceeds hard limit",NULL,0);
					return 1;
					}
				limits[res].rlim_max = limits[res].rlim_cur = t0;
				}
			else
				limits[res].rlim_cur = t0;
			}
		}
	else
		{
		if (hard)
			{
			if (geteuid())
				{
				zerrnam(name,"can't remove hard limits",NULL,0);
				return 1;
				}
			limits[res].rlim_max = RLIM_INFINITY;
			}
		else
			limits[res].rlim_cur = limits[res].rlim_max;
		}
	return 0;
#endif
}

int putraw(c) /**/
int c;
{
	putchar(c);
	return 0;
}

int bin_echotc(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
char *s,buf[2048],*t,*u;
int num,argct,t0;

	s = *argv++;
	if (!termok)
		return 1;
	if ((num = tgetnum(s)) != -1)
		{
		printf("%d\n",num);
		return 0;
		}
	u = buf;
	t = tgetstr(s,&u);
	if (!t || !*t)
		{
		zerrnam(name,"no such capability: %s",s,0);
		return 1;
		}
	for (argct = 0, u = t; *u; u++)
		if (*u == '%')
			{
			if (u++, (*u == 'd' || *u == '2' || *u == '3' || *u == '.' ||
					*u == '+'))
				argct++;
			}
	if (arrlen(argv) != argct)
		{
		zerrnam(name,(arrlen(argv) < argct) ? "not enough arguments" :
			"too many arguments",NULL,0);
		return 1;
		}
	if (!argct)
		tputs(t,1,putraw);
	else
		{
		t0 = (argv[1]) ? atoi(argv[1]) : atoi(*argv);
		tputs(tgoto(t,atoi(*argv),t0),t0,putraw);
		}
	return 0;
}

int bin_pwd(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
	printf("%s\n",pwd);
	return 0;
}

#define TEST_END 0
#define TEST_INPAR 1
#define TEST_OUTPAR 2
#define TEST_STR 3
#define TEST_AND 4
#define TEST_OR 5
#define TEST_NOT 6

static char **tsp;
static int *tip;

int bin_test(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
char **s;
int cnt,*arr,*ap;
Cond c;

	if (func == BIN_BRACKET)
		{
		for (s = argv; *s; s++);
		if (s == argv || strcmp(s[-1],"]"))
			{
			zerrnam(name,"']' expected",NULL,0);
			return 1;
			}
		s[-1] = NULL;
		}
	for (s = argv, cnt = 0; *s; s++,cnt++);
	ap = arr = alloc((cnt+1)*sizeof *arr);
	for (s = argv; *s; s++,ap++)
		if (!strcmp(*s,"("))
			*ap = TEST_INPAR;
		else if (!strcmp(*s,")"))
			*ap = TEST_OUTPAR;
		else if (!strcmp(*s,"-a"))
			*ap = TEST_AND;
		else if (!strcmp(*s,"-o"))
			*ap = TEST_OR;
		else if (!strcmp(*s,"!"))
			*ap = TEST_NOT;
		else
			*ap = TEST_STR;
	*ap = TEST_END;
	tsp = argv;
	tip = arr;
	c = partest(0);
	if (*tip != TEST_END || errflag)
		{
		zerrnam(name,"parse error",NULL,0);
		return 1;
		}
	return (c) ? !evalcond(c) : 1;
}

Cond partest(level) /**/
int level;
{
Cond a,b;

	switch (level)
		{
		case 0:
			a = partest(1);
			if (*tip == TEST_OR)
				{
				tip++,tsp++;
				b = makecond();
				b->left = a;
				b->right = partest(0);
				b->type = COND_OR;
				return b;
				}
			return a;
		case 1:
			a = partest(2);
			if (*tip == TEST_AND)
				{
				tip++,tsp++;
				b = makecond();
				b->left = a;
				b->right = partest(1);
				b->type = COND_AND;
				return b;
				}
			return a;
		case 2:
			if (*tip == TEST_NOT)
				{
				tip++,tsp++;
				b = makecond();
				b->left = partest(2);
				b->type = COND_NOT;
				return b;
				}
		case 3:
			if (*tip == TEST_INPAR)
				{
				tip++,tsp++;
				b = partest(0);
				if (*tip != TEST_OUTPAR)
					{
					zerrnam("test","parse error",NULL,0);
					return NULL;
					}
				tip++,tsp++;
				return b;
				}
			if (tip[0] != TEST_STR)
				{
				zerrnam("test","parse error",NULL,0);
				return NULL;
				}
			else if (tip[1] != TEST_STR)
				{
				b = makecond();
				if (!strcmp(*tsp,"-t"))
					{
					b->left = strdup("1");
					b->type = 't';
					}
				else
					{
					b->left = tsp[0];
					b->type = 'n';
					}
				tip++,tsp++;
				return b;
				}
			else if (tip[2] != TEST_STR)
				{
				b = par_cond_double(tsp[0],tsp[1]);
				tip += 2,tsp += 2;
				return b;
				}
			else
				{
				b = par_cond_triple(tsp[0],tsp[1],tsp[2]);
				tip += 3,tsp += 3;
				return b;
				}
		}
   return NULL;
}

int bin_compctl(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
int flags = 0;
Compctl cc = NULL;
char *usrkeys = NULL;

	for (; *argv && **argv == '-'; argv++)
		while (*++(*argv)) switch(**argv) {
		case 'c': flags |= CC_COMMPATH; break;
		case 'f': flags |= CC_FILES; break;
		case 'h': flags |= CC_HOSTS; break;
		case 'o': flags |= CC_OPTIONS; break;
		case 'v': flags |= CC_VARS; break;
		case 'b': flags |= CC_BINDINGS; break;
		case 'k':
			flags |= CC_USRKEYS;
			if ((*argv)[1]) { usrkeys = (*argv)+1; *argv = ""-1; }
			else if (!argv[1]) {
				zerrnam(name,"variable name expected after -k",NULL,0);
				return 1;
			} else { usrkeys = *++argv; *argv = ""-1; }
			break;
		case 'C': cc = &cc_compos; break;
		case 'D': cc = &cc_default; break;
		default: zerrnam(name,"bad option: %c",NULL,**argv); return 1;
		}
	if (cc) {
		cc->mask = flags;
		if (cc->keyvar) free(cc->keyvar);
		cc->keyvar  = ztrdup(usrkeys);
	}
	if (!*argv) {
		if (!cc) {
			showflag = flags;
			listhtable(compctltab,(HFunc) printcompctl);
			printcompctl("COMMAND",&cc_compos);
			printcompctl("DEFAULT",&cc_default);
		}
		return 0;
	}
	compctl_process(argv,flags,usrkeys);
	return 0;
}

void printcompctl(s,cc) /**/
char *s;Compctl cc;
{
char *css = "fchovb";

	if (cc->mask & showflag) {
		puts(s);
	} else if (!showflag) {
		int flags = cc->mask;
		printf("%s -",s);
		while (*css) {
			if (flags & 1) putchar(*css);
			css++; flags >>= 1;
		}
		if (flags & 1) printf("k %s",cc->keyvar);
		putchar('\n');
	}
}

void compctl_process(s,mask,uk) /**/
char **s;int mask;char *uk;
{
Compctl cc;

	for (;*s;s++) {
		cc = zalloc(sizeof *cc);
		cc->mask = mask; cc->keyvar = ztrdup(uk);
		addhnode(ztrdup(*s),cc,compctltab,freecompctl);
	}
}

int bin_ttyctl(name,argv,ops,func) /**/
char *name;char **argv;char *ops;int func;
{
	if (ops['f'] || !ops['@']) ttyfrozen = 1;
	else if (ops['u']) ttyfrozen = 0;
	return 0;
}

