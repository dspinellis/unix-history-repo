/*
 * Print horizontally as possible, thus saving paper
 */
#include <stdio.h>
int tab[8] = {8,7,6,5,4,3,2,1};
int next;	/*next output position on line*/
int slen;	/*length at start of line*/
int alen;	/*actual length*/
int elen;	/*length on current line*/
char buf[256];
getit()
{	register int i;
	register c;
	slen=alen=elen=0;
	for(i=0;;i++)
	{	buf[i]=c=getchar();
		switch(c)
		{
		case '\n':	if(i==0) continue;
		case EOF:
			alen=i;
			return(i);
		case '\t':
			elen+= tab[(next+elen)%8];
			slen += tab[slen%8];
			continue;
		default:
			elen++;
			slen++;
			continue;
		}
	}
}
putit(ntab)
{	register int i;
	for(i=0;i<ntab;i++) putchar('\t');
	for(i=0;i<alen;i++) putchar(buf[i]);
}
clean()
{
	putchar('\n');
}
main(argc,argv) char *argv[];
{	int len,ntab;
	int i;
	len=80;
	if(argc>1)
	{	i=atoi(argv[1]);
		if(i<0) i= -i;
		len=(i<1?1:i);
	}
	else len=80;
	for(;;)
	{	if(next==0) ntab=0;
		else if(tab[next%8]<1)
			{	ntab=2;
				next+= tab[next%8];
				next += tab[next%8];
			}
			else
			{	ntab=1;
				next += tab[next%8];
			}
		if(getit()<=0) {clean(); exit(0);}
		if(elen+next>=len)
		{	clean();
			next=slen;
			putit(0);
		}
		else
		{	next += elen;
			putit(ntab);
		}
	}
}
