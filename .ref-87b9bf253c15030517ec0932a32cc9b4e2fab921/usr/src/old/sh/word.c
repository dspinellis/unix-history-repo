/*	word.c	4.1	82/05/07	*/

#
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Bell Telephone Laboratories
 *
 */

#include	"defs.h"
#include	"sym.h"


/* ========	character handling for command lines	========*/


word()
{
	REG CHAR	c, d;
	REG CHAR	*argp=locstak()+BYTESPERWORD;
	INT		alpha=1;

	wdnum=0; wdset=0;

	WHILE (c=nextc(0), space(c)) DONE

	IF c=='#'
	THEN	WHILE (c=readc()) ANDF c!=NL DONE
	FI

	IF !eofmeta(c)
	THEN	REP	IF c==LITERAL
			THEN	*argp++=(DQUOTE);
				WHILE (c=readc()) ANDF c!=LITERAL
				DO *argp++=(c|QUOTE); chkpr(c) OD
				*argp++=(DQUOTE);

			ELSE	*argp++=(c);
				IF c=='=' THEN wdset |= alpha FI
				IF !alphanum(c) THEN alpha=0 FI
				IF qotchar(c)
				THEN	d=c;
					WHILE (*argp++=(c=nextc(d))) ANDF c!=d
					DO chkpr(c) OD
				FI
			FI
		PER (c=nextc(0), !eofmeta(c)) DONE
		argp=endstak(argp);
		IF !letter(argp->argval[0]) THEN wdset=0 FI

		peekc=c|MARK;
		IF argp->argval[1]==0 ANDF (d=argp->argval[0], digit(d)) ANDF (c=='>' ORF c=='<')
		THEN	word(); wdnum=d-'0';
		ELSE	/*check for reserved words*/
			IF reserv==FALSE ORF (wdval=syslook(argp->argval,reserved))==0
			THEN	wdarg=argp; wdval=0;
			FI
		FI

	ELIF dipchar(c)
	THEN	IF (d=nextc(0))==c
		THEN	wdval = c|SYMREP;
		ELSE	peekc = d|MARK; wdval = c;
		FI
	ELSE	IF (wdval=c)==EOF
		THEN	wdval=EOFSYM;
		FI
		IF iopend ANDF eolchar(c)
		THEN	copy(iopend); iopend=0;
		FI
	FI
	reserv=FALSE;
	return(wdval);
}

nextc(quote)
	CHAR		quote;
{
	REG CHAR	c, d;
	IF (d=readc())==ESCAPE
	THEN	IF (c=readc())==NL
		THEN	chkpr(NL); d=nextc(quote);
		ELIF quote ANDF c!=quote ANDF !escchar(c)
		THEN	peekc=c|MARK;
		ELSE	d = c|QUOTE;
		FI
	FI
	return(d);
}

readc()
{
	REG CHAR	c;
	REG INT		len;
	REG FILE	f;

retry:
	IF peekc
	THEN	c=peekc; peekc=0;
	ELIF (f=standin, f->fnxt!=f->fend)
	THEN	IF (c = *f->fnxt++)==0
		THEN	IF f->feval
			THEN	IF estabf(*f->feval++)
				THEN	c=EOF;
				ELSE	c=SP;
				FI
			ELSE	goto retry; /* = c=readc(); */
			FI
		FI
		IF flags&readpr ANDF standin->fstak==0 THEN prc(c) FI
		IF c==NL THEN f->flin++ FI
	ELIF f->feof ORF f->fdes<0
	THEN	c=EOF; f->feof++;
	ELIF (len=readb())<=0
	THEN	close(f->fdes); f->fdes = -1; c=EOF; f->feof++;
	ELSE	f->fend = (f->fnxt = f->fbuf)+len;
		goto retry;
	FI
	return(c);
}

LOCAL	readb()
{
	REG FILE	f=standin;
	REG INT		len;

	REP	IF trapnote&SIGSET THEN newline(); sigchk() FI
	PER (len=read(f->fdes,f->fbuf,f->fsiz))<0 ANDF trapnote DONE
	return(len);
}
