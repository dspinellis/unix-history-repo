static char *sccsid = "@(#)fpipe.c	35.1 5/6/81";

#include "global.h"
#include <signal.h>
FILE *_dofpip(iodes)
int iodes;
{
	register FILE *p;

	for(p=_iob; (p->_flag&(_IOWRT|_IOREAD))!=0; p++)
		if (p >= _iob+_NFILE)
			return(NULL);
	p->_file = iodes;
	p->_cnt = 0;
	p->_base = p->_ptr = NULL;
	return(p);
}

FILE * fpipe(info)
FILE *info[2];
{
	register FILE *p;
	int descrips[2];

	if(0 > pipe(descrips)) return( (FILE *) -1);

	if(NULL==(p = _dofpip(descrips[0]))) return( (FILE *) -1);
	p->_flag = (_IONBF|_IOREAD);
	info[0] = p;

	if(NULL==(p = _dofpip(descrips[1]))) return( (FILE *) -1);
	p->_flag = _IOWRT;
	info[1] = p;
	return((FILE *) 2); /*indicate sucess*/
}
#ifndef VMS
/*C library -- write
  nwritten = write(file, buffer, count);
  nwritten == -1 means error
*/
write(file, buffer, count)
char *buffer;
{
	register lispval handy;
	int retval;
	if((file != 1) || (Vcntlw->a.clb == nil)) goto top;
	/* since ^w is non nil, we do not want to print to the terminal,
	   but we must be sure to return a correct value from the write
	   in case there is no write to ptport
	*/
	asm(" movl 12(ap),-4(fp) ") /* retval = count  */
	goto skipit;

top:

asm("	.set	write,4 ");
asm("	chmk	$write	");
asm("	bcc 	noerror ");
asm("	jmp 	cerror  ");
asm("noerror:		");
asm("	movl	r0,-4(fp)");

skipit:
    if(file==1) {
	handy = Vptport->a.clb;
	if(handy!=nil && TYPE(handy)==PORT && handy->p->_file!=1) {
		fflush(handy->p);
		file = handy->p->_file;
		goto top;
	}
    }
    return(retval);
}

/*
# C library -- read

# nread = read(file, buffer, count);
#
# nread ==0 means eof; nread == -1 means error
*/
read(file,buffer,count)
{
asm("	.set	read,3 ");
asm("	.set	eintr,4	 ");  /* from /usr/include/errno.h */
again:
asm("	chmk	$read ");
asm("	bcs 	error ");
asm("	ret ");
asm("error: ");
asm("	cmpl	r0,$eintr ");
asm("	jeql	intseen ");
asm("	jmp	cerror ");
asm(" intseen: ");
	if(sigintcnt > 0) sigcall(SIGINT);
	goto again;
}
#endif
