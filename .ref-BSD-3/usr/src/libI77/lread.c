#include "fio.h"
#include "fmt.h"
#include "lio.h"
#include "ctype.h"
extern char *fmtbuf;
int (*lioproc)();

#define isblnk(x) (ltab[x+1]&B)
#define issep(x) (ltab[x+1]&SX)
#define isapos(x) (ltab[x+1]&AX)
#define isexp(x) (ltab[x+1]&EX)
#define issign(x) (ltab[x+1]&SG)
#define SX 1
#define B 2
#define AX 4
#define EX 8
#define SG 16
char ltab[128+1] = {	/* offset one for EOF */
	0,
	0,0,AX,0,0,0,0,0,0,0,SX,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	SX|B,0,AX,0,0,0,0,AX,0,0,0,SG,SX,SG,0,SX,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,EX,EX,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	AX,0,0,0,EX,EX,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

char l_comma, l_first;
t_getc()
{	int ch;
	if(curunit->uend) return(EOF);
	if((ch=getc(cf))!=EOF) return(ch);
	if(feof(cf)) curunit->uend = 1;
	return(EOF);
}
e_rsle()
{
	int ch;
	if(curunit->uend) return(0);
	while((ch=t_getc())!='\n' && ch!=EOF);
	return(0);
}

flag lquit;
int lcount,ltype;
char *lchar;
double lx,ly;
#define ERR(x) if(n=(x)) return(n)
#define GETC(x) (x=t_getc())

l_read(number,ptr,len,type) ftnint *number,type; flex *ptr; ftnlen len;
{	int i,n,ch;
	double *yy;
	float *xx;
	for(i=0;i<*number;i++)
	{
		if(lquit) return(0);
		if(curunit->uend) err(elist->ciend, EOF, "list in")
		if(lcount == 0) {
			ltype = NULL;
			if(!l_first) l_comma = 0;
			else l_first = 0;
			for(;;)  {
				GETC(ch);
				switch(ch) {
				case EOF:
					goto loopend;
				case ' ':
				case '\n':
					continue;
				case '/':
					lquit = 1;
					goto loopend;
				case ',':
					if(l_comma) {
						lcount = 1;
						ungetc(ch, cf);
						goto loopend;
					}
					l_comma = 1;
					continue;
				default:
					ungetc(ch, cf);
					goto rddata;
				}
			}
		}
	rddata:
		switch((int)type)
		{
		case TYSHORT:
		case TYLONG:
		case TYREAL:
		case TYDREAL:
			ERR(l_R());
			break;
		case TYCOMPLEX:
		case TYDCOMPLEX:
			ERR(l_C());
			break;
		case TYLOGICAL:
			ERR(l_L());
			break;
		case TYCHAR:
			ERR(l_CHAR());
			break;
		}
	loopend:
		if(lquit) return(0);
		if(feof(cf)) err(elist->ciend,(EOF),"list in")
		else if(ferror(cf))
		{	clearerr(cf);
			err(elist->cierr,errno,"list in")
		}
		if(ltype==NULL) goto bump;
		switch((int)type)
		{
		case TYSHORT:
			ptr->flshort=lx;
			break;
		case TYLOGICAL:
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
			*xx++ = lx;
			*xx = ly;
			break;
		case TYDCOMPLEX:
			yy=(double *)ptr;
			*yy++ = lx;
			*yy = ly;
			break;
		case TYCHAR:
			b_char(lchar,(char *)ptr,len);
			break;
		}
	bump:
		if(lcount>0) lcount--;
		ptr = (flex *)((char *)ptr + len);
	}
	return(0);
}
l_R()
{	double a,b,c,d;
	int i,ch,sign=0,da,db,dc;
	a=b=c=d=0;
	da=db=dc=0;
	if(lcount>0) return(0);
	ltype=NULL;
	da=rd_int(&a);
	if(da== -1) sign=da;
	if(GETC(ch)!='*')
	{	ungetc(ch,cf);
		db=1;
		b=a;
		a=1;
	}
	else
		db=rd_int(&b);
	if(GETC(ch)!='.')
	{	dc=c=0;
		ungetc(ch,cf);
	}
	else	dc=rd_int(&c);
	if(isexp(GETC(ch))) db=rd_int(&d);
	else if(issign(ch))
	{	ungetc(ch, cf);
		db = rd_int(&d);
	}
	else
	{	ungetc(ch,cf);
		d=0;
	}
	lcount=a;
	if(!db && !dc)
		return(0);
	if(db && b<0)
	{	sign=1;
		b = -b;
	}
	for(i=0;i<dc;i++) c/=10;
	b=b+c;
	for(i=0;i<d;i++) b *= 10;
	for(i=0;i< -d;i++) b /= 10;
	if(sign) b = -b;
	ltype=TYLONG;
	lx=b;
	return(0);
}
rd_int(x) double *x;
{	int ch,sign=0,i;
	double y;
	i=0;
	y=0;
	if(GETC(ch)=='-') sign = -1;
	else if(ch=='+') sign=0;
	else ungetc(ch,cf);
	while(isdigit(GETC(ch)))
	{	i++;
		y=10*y+ch-'0';
	}
	ungetc(ch,cf);
	if(sign) y = -y;
	*x = y;
	return(y!=0?i:sign);
}
l_C()
{	int ch;
	if(lcount>0) return(0);
	ltype=NULL;
	GETC(ch);
	if(ch!='(')
	{	if(fscanf(cf,"%d",&lcount)!=1)
			if(!feof(cf)) err(elist->cierr,112,"complex format")
			else err(elist->cierr,(EOF),"lread");
		if(GETC(ch)!='*')
		{	ungetc(ch,cf);
			if(!feof(cf)) err(elist->cierr,112,"no star")
			else err(elist->cierr,(EOF),"lread");
		}
		if(GETC(ch)!='(')
		{	ungetc(ch,cf);
			return(0);
		}
	}
	lcount = 1;
	ltype=TYLONG;
	fscanf(cf,"%lf",&lx);
	while(isblnk(GETC(ch)) || (ch == '\n'));
	if(ch!=',')
	{	ungetc(ch,cf);
		err(elist->cierr,112,"no comma");
	}
	while(isblnk(GETC(ch)));
	ungetc(ch,cf);
	fscanf(cf,"%lf",&ly);
	while(isblnk(GETC(ch)));
	if(ch!=')') err(elist->cierr,112,"no )");
	while(isblnk(GETC(ch)));
	if(ch != '\n') ungetc(ch,cf);
	return(0);
}
l_L()
{
	int ch;
	if(lcount>0) return(0);
	ltype=NULL;
	GETC(ch);
	if(isdigit(ch))
	{	ungetc(ch,cf);
		fscanf(cf,"%d",&lcount);
		if(GETC(ch)!='*')
			if(!feof(cf)) err(elist->cierr,112,"no star")
			else err(elist->cierr,(EOF),"lread");
	}
	else	ungetc(ch,cf);
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
		if(isblnk(ch) || issep(ch) || ch==EOF)
		{	ungetc(ch,cf);
			return(0);
		}
		else	err(elist->cierr,112,"logical");
	}
	ltype=TYLONG;
	lcount = 1;
	while(!issep(GETC(ch)) && ch!=EOF);
	ungetc(ch, cf);
	return(0);
}
#define BUFSIZE	128
l_CHAR()
{	int ch,size,i;
	char quote,*p;
	if(lcount>0) return(0);
	ltype=NULL;

	GETC(ch);
	if(isdigit(ch))
	{	ungetc(ch,cf);
		fscanf(cf,"%d",&lcount);
		if(GETC(ch)!='*') err(elist->cierr,112,"no star");
	}
	else	ungetc(ch,cf);
	if(GETC(ch)=='\'' || ch=='"') quote=ch;
	else if(isblnk(ch) || (issep(ch) && ch != '\n') || ch==EOF)
	{	ungetc(ch,cf);
		return(0);
	}
	else err(elist->cierr,112,"no quote");
	ltype=TYCHAR;
	if(lchar!=NULL) free(lchar);
	size=BUFSIZE;
	p=lchar=(char *)malloc(size);
	if(lchar==NULL) err(elist->cierr,113,"no space");
	for(i=0;;)
	{	while(GETC(ch)!=quote && ch!='\n'
			&& ch!=EOF && ++i<size) *p++ = ch;
		if(i==size)
		{
		newone:
			lchar=(char *)realloc(lchar, size += BUFSIZE);
			p=lchar+i-1;
			*p++ = ch;
		}
		else if(ch==EOF) return(EOF);
		else if(ch=='\n')
		{	if(*(p-1) != '\\') continue;
			i--;
			p--;
			if(++i<size) *p++ = ch;
			else goto newone;
		}
		else if(GETC(ch)==quote)
		{	if(++i<size) *p++ = ch;
			else goto newone;
		}
		else
		{	ungetc(ch,cf);
			*p++ = 0;
			return(0);
		}
	}
}
s_rsle(a) cilist *a;
{
	int n;
	if(!init) f_init();
	if(n=c_le(a,READ)) return(n);
	reading=1;
	external=1;
	formatted=1;
	l_first=1;
	l_comma = 1;
	lioproc = l_read;
	lquit = 0;
	lcount = 0;
	if(curunit->uwrt)
		return(nowreading(curunit));
	else	return(0);
}
c_le(a,flag) cilist *a;
{
	fmtbuf="list io";
	if(a->ciunit>=MXUNIT || a->ciunit<0)
		err(a->cierr,101,"stler");
	scale=recpos=0;
	elist=a;
	curunit = &units[a->ciunit];
	if(curunit->ufd==NULL && fk_open(flag,SEQ,FMT,a->ciunit))
		err(a->cierr,102,"lio");
	cf=curunit->ufd;
	if(!curunit->ufmt) err(a->cierr,103,"lio")
	return(0);
}
do_lio(type,number,ptr,len) ftnint *number,*type; flex *ptr; ftnlen len;
{
	return((*lioproc)(number,ptr,len,*type));
}
