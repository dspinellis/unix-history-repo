/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)test.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*
 *	test expression
 *	[ expression ]
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#define EQ(a,b)	((tmp=a)==0?0:(strcmp(tmp,b)==0))

#define DIR 1
#define FIL 2
int	ap;
int	ac;
char	**av;
char	*tmp;
char	*nxtarg();

main(argc, argv)
char *argv[];
{
	int status;

	ac = argc; av = argv; ap = 1;
	if(EQ(argv[0],"[")) {
		if(!EQ(argv[--ac],"]"))
			synbad("] missing","");
	}
	argv[ac] = 0;
	if (ac<=1) exit(1);
	status = (exp()?0:1);
	if (nxtarg(1)!=0)
		synbad("too many arguments","");
	exit(status);
}

char *nxtarg(mt) {

	if (ap>=ac) {
		if(mt) {
			ap++;
			return(0);
		}
		synbad("argument expected","");
	}
	return(av[ap++]);
}

exp() {
	int p1;

	p1 = e1();
	if (EQ(nxtarg(1), "-o")) return(p1 | exp());
	ap--;
	return(p1);
}

e1() {
	int p1;

	p1 = e2();
	if (EQ(nxtarg(1), "-a")) return (p1 & e1());
	ap--;
	return(p1);
}

e2() {
	if (EQ(nxtarg(0), "!"))
		return(!e3());
	ap--;
	return(e3());
}

e3() {
	int p1;
	register char *a;
	char *p2;
	int int1;

	a=nxtarg(0);
	if(EQ(a, "(")) {
		p1 = exp();
		if(!EQ(nxtarg(0), ")")) synbad(") expected","");
		return(p1);
	}

	if(EQ(a, "-r"))
		return(tio(nxtarg(0), 0));

	if(EQ(a, "-w"))
		return(tio(nxtarg(0), 1));

	if(EQ(a, "-d"))
		return(ftype(nxtarg(0))==DIR);

	if(EQ(a, "-f"))
		return(ftype(nxtarg(0))==FIL);

	if(EQ(a, "-s"))
		return(fsizep(nxtarg(0)));

	if(EQ(a, "-t"))
		if(ap>=ac)
			return(isatty(1));
		else
			return(isatty(atoi(nxtarg(0))));

	if(EQ(a, "-n"))
		return(!EQ(nxtarg(0), ""));
	if(EQ(a, "-z"))
		return(EQ(nxtarg(0), ""));

	p2 = nxtarg(1);
	if (p2==0)
		return(!EQ(a,""));
	if(EQ(p2, "="))
		return(EQ(nxtarg(0), a));

	if(EQ(p2, "!="))
		return(!EQ(nxtarg(0), a));

	if(EQ(a, "-l")) {
		int1=length(p2);
		p2=nxtarg(0);
	} else{	int1=atoi(a);
	}
	if(EQ(p2, "-eq"))
		return(int1==atoi(nxtarg(0)));
	if(EQ(p2, "-ne"))
		return(int1!=atoi(nxtarg(0)));
	if(EQ(p2, "-gt"))
		return(int1>atoi(nxtarg(0)));
	if(EQ(p2, "-lt"))
		return(int1<atoi(nxtarg(0)));
	if(EQ(p2, "-ge"))
		return(int1>=atoi(nxtarg(0)));
	if(EQ(p2, "-le"))
		return(int1<=atoi(nxtarg(0)));

	--ap;
	return(!EQ(a,""));
}

tio(a, f)
char *a;
int f;
{

	f = open(a, f);
	if (f>=0) {
		(void) close(f);
		return(1);
	}
	return(0);
}

ftype(f)
char *f;
{
	struct stat statb;

	if(stat(f,&statb)<0)
		return(0);
	if((statb.st_mode&S_IFMT)==S_IFDIR)
		return(DIR);
	return(FIL);
}

fsizep(f)
char *f;
{
	struct stat statb;
	if(stat(f,&statb)<0)
		return(0);
	return(statb.st_size>0);
}

synbad(s1,s2)
char *s1, *s2;
{
	(void) write(2, "test: ", 6);
	(void) write(2, s1, strlen(s1));
	(void) write(2, s2, strlen(s2));
	(void) write(2, "\n", 1);
	exit(255);
}

length(s)
	char *s;
{
	char *es=s;
	while(*es++);
	return(es-s-1);
}
