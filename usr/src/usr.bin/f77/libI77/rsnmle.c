/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rsnmle.c	1.1	%G%
 */

/*
 *		name-list read
 */

#include "fio.h"
#include "lio.h"
#include "nmlio.h"
#include <ctype.h>

LOCAL char nml_rd[] = "namelist read";

static int ch;

#define SP 1
#define B  2
#define AP 4
#define EX 8
#define isblnk(x)	(ltab[x+1]&B)	/* space, tab, newline */
#define issep(x)	(ltab[x+1]&SP)	/* space, tab, newline, comma */
#define isapos(x)	(ltab[x+1]&AP)	/* apost., quote mark */
#define isexp(x)	(ltab[x+1]&EX)	/* d, e, D, E */

#define GETC(x) (x=t_getc())
#define UNGETC() ungetc(ch,cf)

LOCAL char *lchar;
LOCAL double lx,ly;
LOCAL int ltype;
int t_getc(), ungetc();

LOCAL char ltab[128+1] =
{			0, 		/* offset one for EOF */
/*   0- 15 */	0,0,0,0,0,0,0,0,0,SP|B,SP|B,0,0,0,0,0, /* TAB,NEWLINE */
/*  16- 31 */	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/*  32- 47 */	SP|B,0,AP,0,0,0,0,AP,0,0,0,0,SP,0,0,0, /* space,",',comma */
/*  48- 63 */	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/*  64- 79 */	0,0,0,0,EX,EX,0,0,0,0,0,0,0,0,0,0,	/* D,E */
/*  80- 95 */	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
/*  96-111 */	0,0,0,0,EX,EX,0,0,0,0,0,0,0,0,0,0,	/* d,e */
/* 112-127 */	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

s_rsne(a) namelist_arglist *a;
{
	int n, first;
	struct namelistentry *entry;
	int nelem, vlen, vtype;
	char *nmlist_nm, *addr;
	char var_name[VL+1];

	reading = YES;
	formatted = NAMELIST;
	fmtbuf = "ext namelist io";
	if(n=c_le(a,READ)) return(n);
	l_first = YES;
	getn = t_getc;
	ungetn = ungetc;
	leof = curunit->uend;
	lcount = 0;
	ltype = NULL;
	if(curunit->uwrt && ! nowreading(curunit)) err(errflag, errno, nml_rd)

	/* look for " &namelistname " */
	nmlist_nm = a->namelist->namelistname;
	while(isblnk(GETC(ch))) ;
	/* check for "&end" (like IBM) or "$end" (like DEC) */
	if(ch != '&' && ch != '$') goto rderr;
	/* save it - write out using the same character as used on input */
	namelistkey_ = ch;
	while( *nmlist_nm )
		if( GETC(ch) != *nmlist_nm++ ) goto rderr;
	if(!isblnk(GETC(ch))) goto rderr;
	while(isblnk(GETC(ch))) ;
	if(leof) goto rderr;
	UNGETC();

	while( GETC(ch) != namelistkey_ )
	{
	    /* get variable name */
	    if(rd_name(var_name)) goto rderr;
	    entry = a->namelist->names;
	    /* loop through namelist entries looking for this variable name */
	    while( entry->varname[0] != 0 )
	    {
		if( strcmp(entry->varname, var_name) == 0 ) goto got_name;
		entry++;
	    }
	    goto rderr;
got_name:
	    if( n= get_pars( entry, &addr, &nelem, &vlen, &vtype ))
							goto rderr_n;
		/*debug*/printf("var=%s, nelem=%x,vlen=%x,vtype=%x\n",
		/*debug*/	var_name, nelem, vlen, vtype);
	    while(isblnk(GETC(ch))) ;
	    if(ch != '=') goto rderr;
	    if(n = l_read( nelem, addr, vlen, vtype )) 
		{
rderr_n:
			err(n<0?endflag:errflag,n,nml_rd)
		}
	    while(isblnk(GETC(ch)));
	    if(ch == ',') while(isblnk(GETC(ch)));
	    UNGETC();
	    if(leof) goto rderr;
	}
	printf("at end record looking for 'end'\n");
	/* check for 'end' after '&' or '$'*/
	if(GETC(ch)!='e' || GETC(ch)!='n' || GETC(ch)!='d' )
		goto rderr;
	/* flush to next input record */
flush:
	while(GETC(ch) != '\n' && ch != EOF);
	return(ch == EOF ? EOF : OK);

rderr:
	if(leof)
		err(endflag,EOF,nml_rd)
	else
		err(errflag,F_ERNMLIST,nml_rd)
	goto flush;
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
		    fatal(F_ERSYS,"unknown type in wsnmle");
	}

	/* get number of elements */
	dimptr = entry->dimp;
	if( dimptr==NULL )
	{		/* scalar */
		*nelem = 1;
		return(OK);
	}

	if( GETC(ch) != '(' ) 
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
		GETC(ch);
		if(leof) return EOF;
		if(ch != ',' && ch != ')') return F_ERNMLIST;
	}
	if( ++subcnt != ndim ) return F_ERNMLIST;
	
	offset = subs[ndim-1];
	for( i = ndim-2; i>=0; i-- )
		offset = subs[i] + span[i]*offset;
	offset -= baseoffset;
	*nelem = dimptr[1] - offset;
	printf("get_par: *nelem, dimptr[1], offset, baseoffset = %d %d %d %d\n",
			*nelem, dimptr[1], offset, baseoffset );
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
	if(GETC(ch) == '-') sign = -1;
	else if(ch == '+') ;
	else UNGETC();
	if(ch == EOF) return(EOF);

	while(isdigit(GETC(ch)))
	{
		value = 10*value + ch-'0';
		cnt++;
	}
	UNGETC();
	if(ch == 'EOF') return EOF;
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

	if(!isalpha(ch)) {
		UNGETC();
		return(ERROR);
	}
	*ptr++ = ch;
	while(isalnum(GETC(ch))) 
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
	for(i=0;i<number;i++)
	{
		if(leof) return EOF;
		if(l_first)
		{	l_first = NO;
			while(isblnk(GETC(ch)));	/* skip blanks */
			UNGETC();
		}
		else if(lcount==0)		/* repeat count == 0 ? */
		{	ERRNM(t_sep());  /* look for non-blank, allow 1 comma */
		}
		if(!lr_comm())
		{
			while(isblnk(GETC(ch)));
			UNGETC();
			if(ch == namelistkey_ ) return(OK);

			switch((int)type)
			{
			case TYSHORT:
			case TYLONG:
			case TYREAL:
			case TYDREAL:
				ERRNM(l_R(1));
				break;
			case TYCOMPLEX:
			case TYDCOMPLEX:
				ERRNM(l_C());
				break;
			case TYLOGICAL:
				ERRNM(l_L());
				break;
			case TYCHAR:
				ERRNM(l_CHAR());
				break;
			}
		}
		
 		/* peek at next character;should be separator or namelistkey_ */
 		GETC(ch); UNGETC();
		printf("l_read: peek at %c %x\n", ch, ch);
		if(!issep(ch) && (ch != namelistkey_)) 
			return( leof?EOF:F_ERNMLIST );
 
		if(ltype) switch((int)type)
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
lr_comm()
{	int ch;
	if(lcount) return(lcount);
	ltype=NULL;
	while(isblnk(GETC(ch)));
	UNGETC();
	if(ch==',')
	{	lcount=1;
		return(lcount);
	}
	return(OK);
}

LOCAL
get_repet()
{	char ch;
	double lc;
	if(isdigit(GETC(ch)))
	{	UNGETC();
		rd_int(&lc);
		lcount = (int)lc;
		if(GETC(ch)!='*')
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
	int i,ch,sign=0;
	a=b=c=d=0;
	da=db=dc=dd=0;

	if( flg )		/* real */
	{
		da=rd_int(&a);	/* repeat count ? */
		if(GETC(ch)=='*')
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

	if(GETC(ch)=='.' && isdigit(GETC(ch)))
	{	UNGETC();
		dc=rd_int(&c);	/* fractional part of number */
	}
	else
	{	UNGETC();
		dc=0;
		c=0.;
	}
	if(isexp(GETC(ch)))
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
{	int ch,sign=0,i=0;
	double y=0.0;
	if(GETC(ch)=='-') sign = -1;
	else if(ch=='+') sign=0;
	else UNGETC();
	while(isdigit(GETC(ch)))
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
{	int ch,n;
	if(n=get_repet()) return(n);		/* get repeat count */
	if(GETC(ch)!='(') err(errflag,F_ERNMLIST,"no (")
	while(isblnk(GETC(ch)));
	UNGETC();
	l_R(0);		/* get real part */
	ly = lx;
	if(t_sep()) return(EOF);
	l_R(0);		/* get imag part */
	while(isblnk(GETC(ch)));
	if(ch!=')') err(errflag,F_ERNMLIST,"no )")
	ltype = TYCOMPLEX;
	return(OK);
}

LOCAL
l_L()
{
	int ch,n;
	if(n=get_repet()) return(n);		/* get repeat count */
	if(GETC(ch)=='.') GETC(ch);
	switch(ch)
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
		if(issep(ch))
		{	UNGETC();
			lx=0;
			return(OK);
		}
		else if(ch==EOF) return(EOF);
		else	err(errflag,F_ERNMLIST,"logical not T or F");
	}
	ltype=TYLOGICAL;
	while(!issep(GETC(ch)) && ch!=EOF) ;
	UNGETC();
	if(ch == EOF ) return(EOF);
	return(OK);
}

#define BUFSIZE	128
LOCAL
l_CHAR()
{	int ch,size,i,n;
	char quote,*p;
	if(n=get_repet()) return(n);		/* get repeat count */
	if(isapos(GETC(ch))) quote=ch;
	else if(ch == EOF) return EOF;
	else return F_ERNMLIST;
	ltype=TYCHAR;
	if(lchar!=NULL) free(lchar);
	size=BUFSIZE-1;
	p=lchar=(char *)malloc(BUFSIZE);
	if(lchar==NULL) return (F_ERSPACE);
	for(i=0;;)
	{	while( GETC(ch)!=quote && ch!='\n' && ch!=EOF && ++i<size )
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
		else if(GETC(ch)==quote)
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

LOCAL
t_sep()
{
	int ch;
	while(isblnk(GETC(ch)));
	if(leof) return(EOF);
	if(issep(ch)) while(isblnk(GETC(ch)));
	if(leof) return(EOF);
	UNGETC();
	return(OK);
}
