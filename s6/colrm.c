/*
COLRM removes unwanted columns from a file
	Jeff Schriebman  UC Berkeley 11-74
*/

struct buf {
	int fildes;
	int nleft;
	char *nextp;
	char buffer[512];
} buff[2];

main(argc,argv)
char **argv;
{
	int first;
	register ct,last;
	register char c;
	struct buf *ptrp,*ptrg;

	ptrp = &buff[0];
	ptrg = &buff[1];
	ptrp->fildes = 1;
	ptrg->fildes = 0;
	ptrp->nleft = 0;
	ptrg->nleft = 0;
	ptrp->nextp = ptrp->buffer;
	ptrg->nextp = ptrg->buffer;
	first = 20000;
	last  = -1;
	if (argc>1) {
		first = getn(*++argv);
		last = 20000;
	}
	if (argc>2)
		last = getn(*++argv);

start:
	ct = 0;
loop1:
	if ((c=getc(ptrg))<0)
		goto fin;
	ct++;
	if (c=='\n') {
		putc(c,ptrp);
		goto start;
	}
	if (ct<first) {
		putc(c,ptrp);
		goto loop1;
	}

/* Loop getting rid of characters */
	for (;ct<last;ct++) {
		if ((c=getc(ptrg))<0)
			goto fin;
		if (c=='\n') {
			putc(c,ptrp);
			goto start;
		}
	}

/* Output last of the line */
	while ((c=getc(ptrg))>0) {
		putc(c,ptrp);
		if (c=='\n')
			goto start;
	}
fin:
	fflush(ptrp);
}

getn(ap)
char *ap;
{
	register int n,c;
	register char *p;

	p = ap;
	n = 0;
	while ((c = *p++) >= '0' && c <= '9')
		n = n*10 + c - '0';
	return(n);
}
