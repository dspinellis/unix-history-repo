/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>

/* funny -
 * funny accepts a line from the user and prints it out in
 * a mildly amusing manner.
 */

#define WORDLEN 200
char nl[] { 015, 0};

char	is3a;

main(ct,av)
	int ct;
	char **av;
{
	int cvec[2],size,order[WORDLEN],i,rubout();
	char *word,current[WORDLEN],myline[WORDLEN],*src;

	if (ct > 1 && av[1][0] == '-') {
		is3a++;
		ct--;
		av++;
	}
	time(cvec);
	srand(cvec[1]);
	if (ct<2)
	{
		printf("Input a line - ");
		size=read(0,myline,WORDLEN)-1;
		myline[size]=0;
		src=myline;
	}
	else src=av[1];
	signal(2,rubout);
	word=src;
	size=0;
	while (*word++) size++;
	if (size>=WORDLEN)
	{
		printf("%s\nLINE TOO LONG.\n",src);
		exit(9);
	}
	word=src;
	for (;;)
	{
		for (i=0;i<size;++i) current[i]=040;
		pick(size,order);
		for (i=0;i<size;++i) prch(order[i],word,current);
		printf("\n");
	}
}

pick(s,a)
	int s,*a;
{
	int i,j,r,this[WORDLEN];
	for (i=0;i<s;++i) this[i]=i;
	for (i=s-1;i>=0;--i)
	{
		r=rnd(i+1);
		a[i]=this[r];
		this[r]=this[i];
	}
}

rnd(i)
	int i;
{
	int r;
	r=rand();
	if (r<0) r= -r;
	return(r % i);
}

prch(who,from,to)
	int who;
	char *from,*to;
{
	int i;
	to[who]=from[who];
	if (is3a) {
		printf("\033=%c%c%c", ' '+23, ' '+who, to[who]);
		return;
	}
	for (i=0;i<=who;++i) write(1,to+i,1);
/*
	for (i=0;i<=who;++i) write(1,"\b",1);
*/
	write(1,nl,1);
}

rubout()
{
	char *cp,resp[100];
	signal(2,1);
	printf("\nHad enough?");
	read(0,resp,100);
	cp=resp;
	while (*cp != 012 && *cp != 'y' && *cp !='n') ++cp;
	if (*cp == 'n')
	{
		signal(2,rubout);
		return;
	}
	exit(9);
}
