# define VAX 1
struct {short hiword; short loword;}; /* stupid fp-11 */
fixl(p) register long *p;{
	register short t;
	t=p->hiword; p->hiword=p->loword; p->loword=t;
}

main(argc,args)
int argc ;
char **args ;
{
int ifd , ofd , nbyt , ttt;
register char *infile , *oufile , *ap ;
char * fbuf, zero;
struct {
	long magic;
	long tlen,dlen;
	long blen,slen,eadr,trl,drl;
} head;

char buf[512] ;
 
zero = '\0';
if (argc != 3) {
	usage :
		printf("usage : rmhead input output\n") ;
		exit(1) ;
	}
 
infile = args[1] ;
oufile = args[2] ;
 
if ((ifd = open(infile,0)) < 0) {
	ap = infile ;
	oerr :
		printf("%s : cannot open\n",ap) ;
		exit(1) ;
	}
 
if ((ofd = creat(oufile,0777)) < 0) {
	ap = oufile ;
	goto oerr ;
	}
 
read(ifd,&head,sizeof head);
#ifndef VAX
fixl(&head.tlen); fixl(&head.dlen);
#endif
if (seek(ifd,sizeof head,0) < 0) {
	printf("input seek error\n") ;
	unlnk :
		unlink(oufile) ;
			exit(1) ;
	}
 
fbuf=sbrk(4096);
while (head.tlen>0) {
	if (head.tlen>4096) nbyt=4096; else nbyt=head.tlen;
	read(ifd,fbuf,nbyt);
	write(ofd,fbuf,nbyt);
	head.tlen -= nbyt;
}
while (nbyt&0777) {write(ofd,&zero,1); ++nbyt;}
while (head.dlen>0) {
	if (head.dlen>4096) nbyt=4096; else nbyt=head.dlen;
	read(ifd,fbuf,nbyt);
	write(ofd,fbuf,nbyt);
	head.dlen -= nbyt;
}
return(0);
}
