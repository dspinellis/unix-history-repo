/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)rsnmle.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 *		name-list read
 */

#include "fio.h"
#include "lio.h"
#include "nmlio.h"
#include <ctype.h>

LOCAL char *nml_rd;

static int ch;
LOCAL nameflag;
LOCAL	char var_name[VL+1];

#define SP 1
#define B  2
#define AP 4
#define EX 8
#define INTG 16
#define RL 32
#define LGC 64
#define IRL 		(INTG | RL | LGC )
#define isblnk(x)	(ltab[x+1]&B)	/* space, tab, newline */
#define issep(x)	(ltab[x+1]&SP)	/* space, tab, newline, comma */
#define isapos(x)	(ltab[x+1]&AP)	/* apost., quote mark */
#define isexp(x)	(ltab[x+1]&EX)	/* d, e, D, E */
#define isint(x)	(ltab[x+1]&INTG)	/* 0-9, plus, minus */
#define isrl(x)		(ltab[x+1]&RL)	/* 0-9, plus,  minus, period */
#define islgc(x)	(ltab[x+1]&LGC)	/* 0-9, period, t, f, T, F */

#define GETC (ch=t_getc())
#define UNGETC() ungetc(ch,cf)

LOCAL char *lchar;
LOCAL double lx,ly;
LOCAL int ltype;
int t_getc(), ungetc();

LOCAL char ltab[128+1] =
{			0, 		/* offset one for EOF */
/*   0- 15 */ 0,0,0,0,0,0,0,0,0,SP|B,SP|B,0,0,0,0,0, /* TAB,NEWLINE */
/*  16- 31 */ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/*  32- 47 */ SP|B,0,AP,0,0,0,0,AP,0,0,0,RL|INTG,SP,RL|INTG,RL|LGC,0, /* space,",',comma,., */
/*  48- 63 */ IRL,IRL,IRL,IRL,IRL,IRL,IRL,IRL,IRL,IRL,0,0,0,0,0,0, /* digits */
/*  64- 79 */ 0,0,0,0,EX,EX,LGC,0,0,0,0,0,0,0,0,0,	/* D,E,F */
/*  80- 95 */ 0,0,0,0,LGC,0,0,0,0,0,0,0,0,0,0,0,	/* T */
/*  96-111 */ 0,0,0,0,EX,EX,LGC,0,0,0,0,0,0,0,0,0,	/* d,e,f */
/* 112-127 */ 0,0,0,0,LGC,0,0,0,0,0,0,0,0,0,0,0		/* t */
};

s_rsne(a) namelist_arglist *a;
{
	int n;
	struct namelistentry *entry;
	int nelem, vlen, vtype;
	char *nmlist_nm, *addr;

	nml_rd = "namelist read";
	reading = YES;
	formatted = NAMELIST;
	fmtbuf = "ext namelist io";
	if(n=c_le(a,READ)) return(n);
	getn = t_getc;
	ungetn = ungetc;
	leof = curunit->uend;
	if(curunit->uwrt && ! nowreading(curunit)) err(errflag, errno, nml_rd)

	/* look for " &namelistname " */
	nmlist_nm = a->namelist->namelistname;
	while(isblnk(GETC)) ;
	/* check for "&end" (like IBM) or "$end" (like DEC) */
	if(ch != '&' && ch != '$') goto rderr;
	/* save it - write out using the same character as used on input */
	namelistkey_ = ch;
	while( *nmlist_nm )
		if( GETC != *nmlist_nm++ ) 
			{
				nml_rd = "incorrect namelist name";
				goto rderr;
			}
	if(!isblnk(GETC)) goto rderr;
	while(isblnk(GETC)) ;
	if(leof) goto rderr;
	UNGETC();

	while( GETC != namelistkey_ )
	{
	    UNGETC();
	    /* get variable name */
	    if(!nameflag && rd_name(var_name)) goto rderr;

	    entry = a->namelist->names;
	    /* loop through namelist entries looking for this variable name */
	    while( entry->varname[0] != 0 )
	    {
		if( strcmp(entry->varname, var_name) == 0 ) goto got_name;
		entry++;
	    }
	    nml_rd = "incorrect variable name";
	    goto rderr;
got_name:
	    if( n = get_pars( entry, &addr, &nelem, &vlen, &vtype ))
							goto rderr_n;
	    while(isblnk(GETC)) ;
	    if(ch != '=') goto rderr;

	    nameflag = NO;
	    if(n = l_read( nelem, addr, vlen, vtype )) goto rderr_n;
	    while(isblnk(GETC));
	    if(ch == ',') while(isblnk(GETC));
	    UNGETC();
	    if(leof) goto rderr;
	}
	/* check for 'end' after '&' or '$'*/
	if(GETC!='e' || GETC!='n' || GETC!='d' )
		goto rderr;
	/* flush to next input record */
flush:
	while(GETC != '\n' && ch != EOF);
	return(ch == EOF ? EOF : OK);

rderr:
	if(leof)
		n = EOF;
	else
		n = F_ERNMLIST;
rderr_n:
	if(n == EOF ) err(endflag,EOF,nml_rd);
	/* flush after error in case restart I/O */
	if(ch != '\n')  while(GETC != '\n' && ch != EOF) ;
	err(errflag,n,nml_rd)
}

#define MAXSUBS 7

LOCAL
get_pars( entry, addr, nelem, vlen, vtype )
struct namelistentry *entry;
char	**addr;		/* beginning address to read into */
int	*nelem,		/* number of elements to read */
	*vlen,		/* length of elements */
	*vtype;		/* type of elements */
{
	int	offset, i, n,
		*dimptr,	/* points to dimensioning info */
		ndim,		/* number of dimensions */
		baseoffset,	/* offset of corner element */
		*span,		/* subscript span for each dimension */
		subs[MAXSUBS],	/* actual subscripts */
		subcnt = -1;	/* number of actual subscripts */


	/* get element size and base address */
	*vlen = entry->typelen;
	*addr = entry->varaddr;

	/* get type */
	switch ( *vtype = entry->type ) {
		case TYSHORT:
		case TYLONG:
		case TYREAL:
		case TYDREAL:
		case TYCOMPLEX:
		case TYDCOMPLEX:
		case TYLOGICAL:
		case TYCHAR:
			break;
		default:
		    fatal(F_ERSYS,"unknown type in rsnmle");
	}

	/* get number of elements */
	dimptr = entry->dimp;
	if( dimptr==NULL )
	{		/* scalar */
		*nelem = 1;
		return(OK);
	}

	if( GETC != '(' ) 
	{		/* entire array */
		*nelem = dimptr[1];
		UNGETC();
		return(OK);
	}

	/* get element length, number of dimensions, base, span vector */
	ndim = dimptr[0];
	if(ndim<=0 || ndim>MAXSUBS) fatal(F_ERSYS,"illegal dimensions");
	baseoffset = dimptr[2];
	span = dimptr+3;

	/* get subscripts from input data */
	while(ch!=')') {
		if( ++subcnt > MAXSUBS-1 ) return F_ERNMLIST;
		if(n=get_int(&subs[subcnt])) return n;
		GETC;
		if(leof) return EOF;
		if(ch != ',' && ch != ')') return F_ERNMLIST;
	}
	if( ++subcnt != ndim ) return F_ERNMLIST;
	
	offset = subs[ndim-1];
	for( i = ndim-2; i>=0; i-- )
		offset = subs[i] + span[i]*offset;
	offset -= baseoffset;
	*nelem = dimptr[1] - offset;
	if( offset < 0 || offset >= dimptr[1] )
		return F_ERNMLIST;
	*addr = *addr + (*vlen)*offset;
	return OK;
}

LOCAL
get_int(subval)
int *subval;
{
	int sign=0, value=0, cnt=0;

	/* look for sign */
	if(GETC == '-') sign = -1;
	else if(ch == '+') ;
	else UNGETC();
	if(ch == EOF) return(EOF);

	while(isdigit(GETC))
	{
		value = 10*value + ch-'0';
		cnt++;
	}
	UNGETC();
	if(ch == EOF) return EOF;
	if(cnt == 0 ) return F_ERNMLIST;
	if(sign== -1) value = -value;
	*subval = value;
	return OK;
}

LOCAL
rd_name(ptr)
char *ptr;
{
	/* read a variable name from the input stream */
	char *init = ptr-1;

	if(!isalpha(GETC)) {
		UNGETC();
		return(ERROR);
	}
	*ptr++ = ch;
	while(isalnum(GETC)) 
	{
		if(ptr-init > VL ) return(ERROR);
		*ptr++ = ch;
	}
	*ptr = '\0';
	UNGETC();
	return(OK);
}

LOCAL
t_getc()
{	int ch;
	static newline = YES;
rd:
	if(curunit->uend) {
		leof = EOF;
		return(EOF);
	}
	if((ch=getc(cf))!=EOF)
	{
		if(ch == '\n') newline = YES;
		else if(newline==YES) 
		{	/* skip first character on each line for namelist */
			newline = NO;
			goto rd;
		}
		return(ch);
	}
	if(feof(cf))
	{	curunit->uend = YES;
		leof = EOF;
	}
	else clearerr(cf);
	return(EOF);
}

LOCAL
l_read(number,ptr,len,type) ftnint number,type; flex *ptr; ftnlen len;
{	int i,n;
	double *yy;
	float *xx;

	lcount = 0;
	for(i=0;i<number;i++)
	{
		if(leof) return EOF;
		if(lcount==0)
		{
			ltype = NULL;
			if(i!=0)
			{	/* skip to comma */
				while(isblnk(GETC));
				if(leof) return(EOF);
				if(ch == namelistkey_) 
				{	UNGETC();
					return(OK);
				}
				if(ch != ',' ) return(F_ERNMLIST);
			}
			while(isblnk(GETC));
			if(leof) return(EOF);
			UNGETC();
			if(i!=0 && ch == namelistkey_) return(OK);

			switch((int)type)
			{
			case TYSHORT:
			case TYLONG:
				if(!isint(ch)) return(OK);
				ERRNM(l_R(1));
				break;
			case TYREAL:
			case TYDREAL:
				if(!isrl(ch)) return(OK);
				ERRNM(l_R(1));
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				if(!isdigit(ch) && ch!='(') return(OK);
				ERRNM(l_C());
				break;
			case TYLOGICAL:
				if(!islgc(ch)) return(OK);
				ERRNM(l_L());
				if(nameflag) return(OK);
				break;
			case TYCHAR:
				if(!isdigit(ch) && !isapos(ch)) return(OK);
				ERRNM(l_CHAR());
				break;
			}
		
			if(leof) return(EOF);
 			/* peek at next character -
				should be separator or namelistkey_ */
 			GETC; UNGETC();
			if(!issep(ch) && (ch != namelistkey_)) 
			return( leof?EOF:F_ERNMLIST );
		}
 
		if(!ltype) return(F_ERNMLIST);
		switch((int)type)
		{
		case TYSHORT:
			ptr->flshort=lx;
			break;
		case TYLOGICAL:
			if(len == sizeof(short))
				ptr->flshort = lx;
			else
				ptr->flint = lx;
			break;
		case TYLONG:
			ptr->flint=lx;
			break;
		case TYREAL:
			ptr->flreal=lx;
			break;
		case TYDREAL:
			ptr->fldouble=lx;
			break;
		case TYCOMPLEX:
			xx=(float *)ptr;
			*xx++ = ly;
			*xx = lx;
			break;
		case TYDCOMPLEX:
			yy=(double *)ptr;
			*yy++ = ly;
			*yy = lx;
			break;
		case TYCHAR:
			b_char(lchar,(char *)ptr,len);
			break;
		}
		if(lcount>0) lcount--;
		ptr = (flex *)((char *)ptr + len);
	}
	if(lcount>0) return F_ERNMLIST;
	return(OK);
}

LOCAL
get_repet()
{
	double lc;
	if(isdigit(GETC))
	{	UNGETC();
		rd_int(&lc);
		lcount = (int)lc;
		if(GETC!='*')
			if(leof) return(EOF);
			else return(F_ERREPT);
	}
	else
	{	lcount = 1;
		UNGETC();
	}
	return(OK);
}

LOCAL
l_R(flg) int flg;
{	double a,b,c,d;
	int da,db,dc,dd;
	int i,sign=0;
	a=b=c=d=0;
	da=db=dc=dd=0;

	if( flg )		/* real */
	{
		da=rd_int(&a);	/* repeat count ? */
		if(GETC=='*')
		{
			if (a <= 0.) return(F_ERNREP);
			lcount=(int)a;
			db=rd_int(&b);	/* whole part of number */
		}
		else
		{	UNGETC();
			db=da;
			b=a;
			lcount=1;
		}
	}
	else		   /* complex */
	{
		db=rd_int(&b);
	}

	if(GETC=='.' && isdigit(GETC))
	{	UNGETC();
		dc=rd_int(&c);	/* fractional part of number */
	}
	else
	{	UNGETC();
		dc=0;
		c=0.;
	}
	if(isexp(GETC))
		dd=rd_int(&d);	/* exponent */
	else if (ch == '+' || ch == '-')
	{	UNGETC();
		dd=rd_int(&d);
	}
	else
	{	UNGETC();
		dd=0;
	}
	if(db<0 || b<0)
	{	sign=1;
		b = -b;
	}
	for(i=0;i<dc;i++) c/=10.;
	b=b+c;
	if (dd > 0)
	{	for(i=0;i<d;i++) b *= 10.;
		for(i=0;i< -d;i++) b /= 10.;
	}
	lx=sign?-b:b;
	ltype=TYLONG;
	return(OK);
}

LOCAL
rd_int(x) double *x;
{	int sign=0,i=0;
	double y=0.0;
	if(GETC=='-') sign = -1;
	else if(ch=='+') sign=0;
	else UNGETC();
	while(isdigit(GETC))
	{	i++;
		y=10*y + ch-'0';
	}
	UNGETC();
	if(sign) y = -y;
	*x = y;
	return(y==0.0?sign:i); /* 0:[+]&&y==0, -1:-&&y==0, >0:#digits&&y!=0 */
}

LOCAL
l_C()
{	int n;
	if(n=get_repet()) return(n);		/* get repeat count */
	if(GETC!='(') err(errflag,F_ERNMLIST,"no (")
	while(isblnk(GETC));
	UNGETC();
	l_R(0);		/* get real part */
	ly = lx;
	while(isblnk(GETC));  /* get comma */
	if(leof) return(EOF);
	if(ch!=',') return(F_ERNMLIST);
	while(isblnk(GETC));
	UNGETC();
	if(leof) return(EOF);
	l_R(0);		/* get imag part */
	while(isblnk(GETC));
	if(ch!=')') err(errflag,F_ERNMLIST,"no )")
	ltype = TYCOMPLEX;
	return(OK);
}

LOCAL
l_L()
{
	int n, keychar=ch, scanned=NO;
	if(ch=='f' || ch=='F' || ch=='t' || ch=='T')
	{
		scanned=YES;
		if(rd_name(var_name))
			return(leof?EOF:F_ERNMLIST);
		while(isblnk(GETC));
		UNGETC();
		if(ch == '=' || ch == '(')
		{  	/* found a name, not a value */
			nameflag = YES;
			return(OK);
		}
	}
	else
	{
		if(n=get_repet()) return(n);		/* get repeat count */
		if(GETC=='.') GETC;
		keychar = ch;
	}
	switch(keychar)
	{
	case 't':
	case 'T':
		lx=1;
		break;
	case 'f':
	case 'F':
		lx=0;
		break;
	default:
		if(ch==EOF) return(EOF);
		else	err(errflag,F_ERNMLIST,"logical not T or F");
	}
	ltype=TYLOGICAL;
	if(scanned==NO)
	{
		while(!issep(GETC) && ch!=EOF) ;
		UNGETC();
	}
	if(ch == EOF ) return(EOF);
	return(OK);
}

#define BUFSIZE	128
LOCAL
l_CHAR()
{	int size,i,n;
	char quote,*p;
	if(n=get_repet()) return(n);		/* get repeat count */
	if(isapos(GETC)) quote=ch;
	else if(ch == EOF) return EOF;
	else return F_ERNMLIST;
	ltype=TYCHAR;
	if(lchar!=NULL) free(lchar);
	size=BUFSIZE-1;
	p=lchar=(char *)malloc(BUFSIZE);
	if(lchar==NULL) return (F_ERSPACE);
	for(i=0;;)
	{	while( GETC!=quote && ch!='\n' && ch!=EOF && ++i<size )
				*p++ = ch;
		if(i==size)
		{
		newone:
			size += BUFSIZE;
			lchar=(char *)realloc(lchar, size+1);
			if(lchar==NULL) return( F_ERSPACE );
			p=lchar+i-1;
			*p++ = ch;
		}
		else if(ch==EOF) return(EOF);
		else if(ch=='\n')
		{	if(*(p-1) == '\\') *(p-1) = ch;
		}
		else if(GETC==quote)
		{	if(++i<size) *p++ = ch;
			else goto newone;
		}
		else
		{	UNGETC();
			*p = '\0';
			return(OK);
		}
	}
}
