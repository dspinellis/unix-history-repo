/*	expand.c	4.1	82/05/07	*/

#
/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	Bell Telephone Laboratories
 *
 */

#include	"defs.h"
#include	<sys/param.h>
#include	<sys/stat.h>
#include	<dir.h>



/* globals (file name generation)
 *
 * "*" in params matches r.e ".*"
 * "?" in params matches r.e. "."
 * "[...]" in params matches character class
 * "[...a-z...]" in params matches a through z.
 *
 */

PROC VOID	addg();


INT	expand(as,rflg)
	STRING		as;
{
	INT		count;
	DIR		*dirf;
	BOOL		dir=0;
	STRING		rescan = 0;
	REG STRING	s, cs;
	ARGPTR		schain = gchain;
	struct direct	*dp;
	STATBUF		statb;

	IF trapnote&SIGSET THEN return(0); FI

	s=cs=as;

	/* check for meta chars */
	BEGIN
	   REG BOOL slash; slash=0;
	   WHILE !fngchar(*cs)
	   DO	IF *cs++==0
		THEN	IF rflg ANDF slash THEN break; ELSE return(0) FI
		ELIF *cs=='/'
		THEN	slash++;
		FI
	   OD
	END

	LOOP	IF cs==s
		THEN	s=nullstr;
			break;
		ELIF *--cs == '/'
		THEN	*cs=0;
			IF s==cs THEN s="/" FI
			break;
		FI
	POOL
	IF stat(s,&statb)>=0
	    ANDF (statb.st_mode&S_IFMT)==S_IFDIR
	    ANDF (dirf=opendir(s))>0
	THEN	dir++;
	FI
	count=0;
	IF *cs==0 THEN *cs++=0200 FI
	IF dir
	THEN	/* check for rescan */
		REG STRING rs; rs=cs;

		REP	IF *rs=='/' THEN rescan=rs; *rs=0; gchain=0 FI
		PER	*rs++ DONE

		WHILE (dp = readdir(dirf)) != NULL ANDF (trapnote&SIGSET) == 0
		DO	IF (*dp->d_name=='.' ANDF *cs!='.')
			THEN	continue;
			FI
			IF gmatch(dp->d_name, cs)
			THEN	addg(s,dp->d_name,rescan); count++;
			FI
		OD
		closedir(dirf);

		IF rescan
		THEN	REG ARGPTR	rchain;
			rchain=gchain; gchain=schain;
			IF count
			THEN	count=0;
				WHILE rchain
				DO	count += expand(rchain->argval,1);
					rchain=rchain->argnxt;
				OD
			FI
			*rescan='/';
		FI
	FI

	BEGIN
	   REG CHAR	c;
	   s=as;
	   WHILE c = *s
	   DO	*s++=(c&STRIP?c:'/') OD
	END
	return(count);
}

gmatch(s, p)
	REG STRING	s, p;
{
	REG INT		scc;
	CHAR		c;

	IF scc = *s++
	THEN	IF (scc &= STRIP)==0
		THEN	scc=0200;
		FI
	FI
	SWITCH c = *p++ IN

	    case '[':
		{BOOL ok; INT lc;
		ok=0; lc=077777;
		WHILE c = *p++
		DO	IF c==']'
			THEN	return(ok?gmatch(s,p):0);
			ELIF c==MINUS
			THEN	IF lc<=scc ANDF scc<=(*p++) THEN ok++ FI
			ELSE	IF scc==(lc=(c&STRIP)) THEN ok++ FI
			FI
		OD
		return(0);
		}

	    default:
		IF (c&STRIP)!=scc THEN return(0) FI

	    case '?':
		return(scc?gmatch(s,p):0);

	    case '*':
		IF *p==0 THEN return(1) FI
		--s;
		WHILE *s
		DO  IF gmatch(s++,p) THEN return(1) FI OD
		return(0);

	    case 0:
		return(scc==0);
	ENDSW
}

LOCAL VOID	addg(as1,as2,as3)
	STRING		as1, as2, as3;
{
	REG STRING	s1, s2;
	REG INT		c;

	s2 = locstak()+BYTESPERWORD;

	s1=as1;
	WHILE c = *s1++
	DO	IF (c &= STRIP)==0
		THEN	*s2++='/';
			break;
		FI
		*s2++=c;
	OD
	s1=as2;
	WHILE *s2 = *s1++ DO s2++ OD
	IF s1=as3
	THEN	*s2++='/';
		WHILE *s2++ = *++s1 DONE
	FI
	makearg(endstak(s2));
}

makearg(args)
	REG STRING	args;
{
	args->argnxt=gchain;
	gchain=args;
}

