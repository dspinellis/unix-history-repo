/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)test.c	1.1 */
/*
 * test expression
 * [ expression ]
 * Rewritten by David Korn
 */

#include	<sys/types.h>
#include	<sys/stat.h>
#include	"shtype.h"
#include	"defs.h"
#include	"test.h"
#include	"sym.h"

#define	tio(a,f)	(access(a,f)==0)
/* single char string compare */
#define c_eq(a,c)	(*a==c && *(a+1)==0)
/* two character string compare */
#define c2_eq(a,c1,c2)	(*a==c1 && *(a+1)==c2 && *(a+2)==0)

int ftype();
int testfn();

extern long aeval();
extern char *strchr();
extern void failed();

static char *nxtarg();
static time_t ftime_compare();
static int exp();
static int e3();
static int fsizep();

extern MSG	synmsg;
extern MSG	test_opts;

static int ap, ac;
static char **av;

int testfn(argn, com)
char *com[];
register int argn;
{
	register char *p = com[0];
	av = com;
	ap = 1;
	if(c_eq(p,'['))
	{
		p = com[--argn];
		if(!c_eq(p, ']'))
			failed(btest,  endmatch);
	}
	if(argn <= 1)
		return(1);
	ac = argn;
	return(!exp(0));
}

/*
 * evaluate a test expression.
 * flag is 0 on outer level
 * flag is 1 when in parenthesis
 * flag is 2 when evaluating -a 
 */

static exp(flag)
{
	register int r;
	register char *p;
	r = e3();
	while(ap < ac)
	{
		p = nxtarg(0);
		/* check for -o and -a */
		if(flag && c_eq(p,')'))
		{
			ap--;
			break;
		}
		if(*p=='-' && *(p+2)==0)
		{
			if(*++p == 'o')
			{
				if(flag==2)
				{
					ap--;
					break;
				}
				r |= exp(3);
				continue;
			}
			else if(*p == 'a')
			{
				r &= exp(2);
				continue;
			}
		}
		failed(btest,  synmsg);
	}
	return(r);
}

static char *nxtarg(mt)
{
	if(ap >= ac)
	{
		if(mt)
		{
			ap++;
			return(0);
		}
		failed(btest, argexp);
	}
	return(av[ap++]);
}


static e3()
{
	register char *a;
	register char *p2;
	register int p1;
	long int int1, int2;
	char *op;
	a=nxtarg(0);
	if(c_eq(a, '!'))
		return(!e3());
	if(c_eq(a, '('))
	{
		p1 = exp(1);
		p2 = nxtarg(0);
		if(!c_eq(p2, ')'))
			failed(btest,parexp);
		return(p1);
	}
	p2 = nxtarg(1);
	if(p2!=0 && (c_eq(p2,'=') || c2_eq(p2,'!','=')))
		goto skip;
	if(c2_eq(a,'-','t'))
	{
		if(p2 && isdigit(*p2))
			 return(*(p2+1)?0:isatty(*p2-'0'));
		else
		{
		/* test -t with no arguments */
			ap--;
			return(isatty(1));
		}
	}
	if((*a=='-' && *(a+2)==0) && strchr(test_opts,*(a+1)))
	{
		if(p2==0 || c_eq(p2,')') )
			failed(btest, argexp);
		switch(*(a+1))
		{
			case 'r':
				return(tio(p2, 4));
			case 'w':
				return(tio(p2, 2));
			case 'x':
				return(tio(p2, 1));
			case 'd':
				return(ftype(p2,S_IFMT,S_IFDIR));
			case 'c':
				return(ftype(p2,S_IFMT,S_IFCHR));
			case 'b':
				return(ftype(p2,S_IFMT,S_IFBLK));
			case 'f':
				return(ftype(p2,S_IFMT,S_IFREG));
			case 'u':
				return(ftype(p2,S_ISUID,S_ISUID));
			case 'g':
				return(ftype(p2,S_ISGID,S_ISGID));
			case 'k':
				return(ftype(p2,S_ISVTX,S_ISVTX));
			case 'L':
#ifdef S_IFLNK
				{
					struct stat statb;
					if(lstat(p2,&statb)<0)
						return(0);
					return((statb.st_mode&S_IFMT)==S_IFLNK);
				}
#else
				return(0);
#endif	/* S_IFLNK */
			case 'p':
#ifdef S_IFIFO
				return(ftype(p2,S_IFIFO,S_IFIFO));
#else
				return(0);
#endif	/* S_IFIFO */
			case 's':
				return(fsizep(p2));
			case 'n':
				return(*p2 != 0);
			case 'z':
				return(*p2 == 0);
		}
	}
	if(p2==0 || c_eq(p2,')'))
	{
		ap--;
		return(*a!=0);
	}
skip:
	p1 = syslook(p2,testops);
	op = p2;
	if((p1&TEST_BINOP)==0)
		p2 = nxtarg(0);
	if(p1==0)
		failed(op,badop);
	if(p1&TEST_ARITH)
	{
		int1 = aeval(a);
		int2 = aeval(p2);
	}
	switch(p1)
	{
		/* p1 must be one of the following values */
		case TEST_AND:
		case TEST_OR:
			ap--;
			return(*a!=0);
		case TEST_SEQ:
			return(eq(p2, a));
		case TEST_SNE:
			return(!eq(p2, a));
		case TEST_EF:
			return(eq_inode(p2,a));
		case TEST_NT:
			return(ftime_compare(a,p2)>0);
		case TEST_OT:
			return(ftime_compare(a,p2)<0);
		case TEST_EQ:
			return(int1==int2);
		case TEST_NE:
			return(int1!=int2);
		case TEST_GT:
			return(int1>int2);
		case TEST_LT:
			return(int1<int2);
		case TEST_GE:
			return(int1>=int2);
		case TEST_LE:
			return(int1<=int2);
	}
	/* NOTREACHED */
}

ftype(f,mask,field)
char *f;
int field;
{
	struct stat statb;
	if(stat(f,&statb)<0)
		return(0);
	return((statb.st_mode&mask)==field);
}

static fsizep(f)
char *f;
{
	struct stat statb;
	if(stat(f, &statb) <0)
		return(0);
	return(statb.st_size>0);
}

/*
 * returns the modification time of f1 - modification time of f2
 */

static time_t ftime_compare(file1,file2)
char *file1,*file2;
{
	struct stat statb1,statb2;
	if(stat(file1,&statb1)<0)
		statb1.st_mtime = 0;
	if(stat(file2,&statb2)<0)
		statb2.st_mtime = 0;
	return(statb1.st_mtime-statb2.st_mtime);
}

/*
 * return true if inode of two files are the same
 */

eq_inode(file1,file2)
char *file1,*file2;
{
	struct stat stat1,stat2;
	if(stat(file1,&stat1)>=0  && stat(file2,&stat2)>=0)
		if(stat1.st_dev == stat2.st_dev && stat1.st_ino == stat2.st_ino)
			return(1);
	return(0);
}

