#ifndef lint
static char sccsid[] = "@(#)macro.c	4.3 8/11/83";
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
#include	"sym.h"

LOCAL CHAR	quote;	/* used locally */
LOCAL CHAR	quoted;	/* used locally */



LOCAL STRING	copyto(endch)
	REG CHAR	endch;
{
	REG CHAR	c;

	WHILE (c=getch(endch))!=endch ANDF c
	DO pushstak(c|quote) OD
	zerostak();
	IF c!=endch THEN error(badsub) FI
}

LOCAL	skipto(endch)
	REG CHAR	endch;
{
	/* skip chars up to } */
	REG CHAR	c;
	WHILE (c=readc()) ANDF c!=endch
	DO	SWITCH c IN

		case SQUOTE:	skipto(SQUOTE); break;

		case DQUOTE:	skipto(DQUOTE); break;

		case DOLLAR:	IF readc()==BRACE
				THEN	skipto('}');
				FI
		ENDSW
	OD
	IF c!=endch THEN error(badsub) FI
}

LOCAL	getch(endch)
	CHAR		endch;
{
	REG CHAR	d;

retry:
	d=readc();
	IF !subchar(d)
	THEN	return(d);
	FI
	IF d==DOLLAR
	THEN	REG INT	c;
		IF (c=readc(), dolchar(c))
		THEN	NAMPTR		n=NIL;
			INT		dolg=0;
			BOOL		bra;
			REG STRING	argp, v;
			CHAR		idb[2];
			STRING		id=idb;

			IF bra=(c==BRACE) THEN c=readc() FI
			IF letter(c)
			THEN	argp=relstak();
				WHILE alphanum(c) DO pushstak(c); c=readc() OD
				zerostak();
				n=lookup(absstak(argp)); setstak(argp);
				v = n->namval; id = n->namid;
				peekc = c|MARK;;
			ELIF digchar(c)
			THEN	*id=c; idb[1]=0;
				IF astchar(c)
				THEN	dolg=1; c='1';
				FI
				c -= '0';
				v=((c==0) ? cmdadr : (c<=dolc) ? dolv[c] : (dolg=0));
			ELIF c=='$'
			THEN	v=pidadr;
			ELIF c=='!'
			THEN	v=pcsadr;
			ELIF c=='#'
			THEN	v=dolladr;
			ELIF c=='?'
			THEN	v=exitadr;
			ELIF c=='-'
			THEN	v=flagadr;
			ELIF bra THEN error(badsub);
			ELSE	goto retry;
			FI
			c = readc();
			IF !defchar(c) ANDF bra
			THEN	error(badsub);
			FI
			argp=0;
			IF bra
			THEN	IF c!='}'
				THEN	argp=relstak();
					IF (v==0)NEQ(setchar(c))
					THEN	copyto('}');
					ELSE	skipto('}');
					FI
					argp=absstak(argp);
				FI
			ELSE	peekc = c|MARK; c = 0;
			FI
			IF v
			THEN	IF c!='+'
				THEN	LOOP WHILE c = *v++
					     DO pushstak(c|quote); OD
					     IF dolg==0 ORF (++dolg>dolc)
					     THEN break;
					     ELSE v=dolv[dolg]; pushstak(SP|(*id=='*' ? quote : 0));
					     FI
					POOL
				FI
			ELIF argp
			THEN	IF c=='?'
				THEN	failed(id,*argp?argp:badparam);
				ELIF c=='='
				THEN	IF n
					THEN	assign(n,argp);
					ELSE	error(badsub);
					FI
				FI
			ELIF flags&setflg
			THEN	failed(id,badparam);
			FI
			goto retry;
		ELSE	peekc=c|MARK;
		FI
	ELIF d==endch
	THEN	return(d);
	ELIF d==SQUOTE
	THEN	comsubst(); goto retry;
	ELIF d==DQUOTE
	THEN	quoted++; quote^=QUOTE; goto retry;
	FI
	return(d);
}

STRING	macro(as)
	STRING		as;
{
	/* Strip "" and do $ substitution
	 * Leaves result on top of stack
	 */
	REG BOOL	savqu =quoted;
	REG CHAR	savq = quote;
	FILEHDR		fb;

	push(&fb); estabf(as);
	usestak();
	quote=0; quoted=0;
	copyto(0);
	pop();
	IF quoted ANDF (stakbot==staktop) THEN pushstak(QUOTE) FI
	quote=savq; quoted=savqu;
	return(fixstak());
}

LOCAL	comsubst()
{
	/* command substn */
	FILEBLK		cb;
	REG CHAR	d;
	REG STKPTR	savptr = fixstak();

	usestak();
	WHILE (d=readc())!=SQUOTE ANDF d
	DO pushstak(d) OD

	BEGIN
	   REG STRING	argc;
	   trim(argc=fixstak());
	   push(&cb); estabf(argc);
	END
	BEGIN
	   REG TREPTR	t = makefork(FPOU,cmd(EOFSYM,MTFLG|NLFLG));
	   INT		pv[2];

	   /* this is done like this so that the pipe
	    * is open only when needed
	    */
	   chkpipe(pv);
	   initf(pv[INPIPE]);
	   execute(t, 0, 0, pv);
	   close(pv[OTPIPE]);
	END
	tdystak(savptr); staktop=movstr(savptr,stakbot);
	WHILE d=readc() DO locstak(); pushstak(d|quote) OD
	await(0);
	WHILE stakbot!=staktop
	DO	IF (*--staktop&STRIP)!=NL
		THEN	++staktop; break;
		FI
	OD
	pop();
}

#define CPYSIZ	512

subst(in,ot)
	INT		in, ot;
{
	REG CHAR	c;
	FILEBLK		fb;
	REG INT		count=CPYSIZ;

	push(&fb); initf(in);
	/* DQUOTE used to stop it from quoting */
	WHILE c=(getch(DQUOTE)&STRIP)
	DO pushstak(c);
	   IF --count == 0
	   THEN	flush(ot); count=CPYSIZ;
	   FI
	OD
	flush(ot);
	pop();
}

LOCAL	flush(ot)
{
	write(ot,stakbot,staktop-stakbot);
	IF flags&execpr THEN write(output,stakbot,staktop-stakbot) FI
	staktop=stakbot;
}
