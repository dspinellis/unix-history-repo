#ifndef lint
static char sccsid[] = "@(#)printable.c	4.1 (Berkeley) 1/1/83";
#endif

main(argc,argv)
	char *argv[];
{
char b[512], *p;
int f, c;
f = open(argv[1], 0);
if (f<0) return(1);
p = b + read(f, b, 512);
while (p>b)
	if ( (c= *--p) ==0 || (c&0200) ) return(1);
return(0);
}
