#ifndef lint
static char sccsid[] = "@(#)io.c	4.3 3/19/85";
#endif

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"


/* ========	input output and file copying ======== */

initf(fd)
	UFD		fd;
{
	REG FILE	f=standin;

	f->fdes=fd; f->fsiz=((flags&(oneflg|ttyflg))==0 ? BUFSIZ : 1);
	f->fnxt=f->fend=f->fbuf; f->feval=0; f->flin=1;
	f->feof=FALSE;
}

estabf(s)
	REG STRING	s;
{
	REG FILE	f;

	(f=standin)->fdes = -1;
	f->fend=length(s)+(f->fnxt=s);
	f->flin=1;
	return(f->feof=(s==0));
}

push(af)
	FILE		af;
{
	REG FILE	f;

	(f=af)->fstak=standin;
	f->feof=0; f->feval=0;
	standin=f;
}

pop()
{
	REG FILE	f;

	IF (f=standin)->fstak
	THEN	IF f->fdes>=0 THEN close(f->fdes) FI
		standin=f->fstak;
		return(TRUE);
	ELSE	return(FALSE);
	FI
}

chkpipe(pv)
	INT		*pv;
{
	IF pipe(pv)<0 ORF pv[INPIPE]<0 ORF pv[OTPIPE]<0
	THEN	error(piperr);
	FI
}

chkopen(idf)
	STRING		idf;
{
	REG INT		rc;

	IF (rc=open(idf,0))<0
	THEN	failed(idf,badopen);
	ELSE	return(rc);
	FI
}

rename(f1,f2)
	REG INT		f1, f2;
{
	IF f1!=f2
	THEN	dup2(f1, f2);
		close(f1);
		IF f2==0 THEN ioset|=1 FI
	FI
}

create(s)
	STRING		s;
{
	REG INT		rc;

	IF (rc=creat(s,0666))<0
	THEN	failed(s,badcreate);
	ELSE	return(rc);
	FI
}

tmpfil()
{
	itos(serial++); movstr(numbuf,tmpnam);
	return(create(tmpout));
}

/* set by trim */
BOOL		nosubst;

copy(ioparg)
	IOPTR		ioparg;
{
	CHAR		c, *ends;
	REG CHAR	*cline, *clinep;
	INT		fd;
	REG IOPTR	iop;

	IF iop=ioparg
	THEN	copy(iop->iolst);
		ends=mactrim(iop->ioname); IF nosubst THEN iop->iofile &= ~IODOC FI
		fd=tmpfil();
		iop->ioname=cpystak(tmpout);
		iop->iolst=iotemp; iotemp=iop;
		cline=locstak();

		LOOP	clinep=cline; chkpr(NL);
			WHILE (c = (nosubst ? readc() :  nextc(*ends)),  !eolchar(c)) DO *clinep++ = c OD
			*clinep=0;
			IF eof ORF eq(cline,ends) THEN break FI
			*clinep++=NL;
			write(fd,cline,clinep-cline);
		POOL
		close(fd);
	FI
}
