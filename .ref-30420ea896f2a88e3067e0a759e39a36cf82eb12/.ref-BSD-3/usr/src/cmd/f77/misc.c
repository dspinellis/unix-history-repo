#include "defs"



cpn(n, a, b)
register int n;
register char *a, *b;
{
while(--n >= 0)
	*b++ = *a++;
}



eqn(n, a, b)
register int n;
register char *a, *b;
{
while(--n >= 0)
	if(*a++ != *b++)
		return(NO);
return(YES);
}







cmpstr(a, b, la, lb)	/* compare two strings */
register char *a, *b;
ftnint la, lb;
{
register char *aend, *bend;
aend = a + la;
bend = b + lb;


if(la <= lb)
	{
	while(a < aend)
		if(*a != *b)
			return( *a - *b );
		else
			{ ++a; ++b; }

	while(b < bend)
		if(*b != ' ')
			return(' ' - *b);
		else
			++b;
	}

else
	{
	while(b < bend)
		if(*a != *b)
			return( *a - *b );
		else
			{ ++a; ++b; }
	while(a < aend)
		if(*a != ' ')
			return(*a - ' ');
		else
			++a;
	}
return(0);
}





chainp hookup(x,y)
register chainp x, y;
{
register chainp p;

if(x == NULL)
	return(y);

for(p = x ; p->nextp ; p = p->nextp)
	;
p->nextp = y;
return(x);
}



struct Listblock *mklist(p)
chainp p;
{
register struct Listblock *q;

q = ALLOC(Listblock);
q->tag = TLIST;
q->listp = p;
return(q);
}


chainp mkchain(p,q)
register int p, q;
{
register chainp r;

if(chains)
	{
	r = chains;
	chains = chains->nextp;
	}
else
	r = ALLOC(Chain);

r->datap = p;
r->nextp = q;
return(r);
}



char * varstr(n, s)
register int n;
register char *s;
{
register int i;
static char name[XL+1];

for(i=0;  i<n && *s!=' ' && *s!='\0' ; ++i)
	name[i] = *s++;

name[i] = '\0';

return( name );
}




char * varunder(n, s)
register int n;
register char *s;
{
register int i;
static char name[XL+1];

for(i=0;  i<n && *s!=' ' && *s!='\0' ; ++i)
	name[i] = *s++;

#if TARGET != GCOS
name[i++] = '_';
#endif

name[i] = '\0';

return( name );
}





char * nounder(n, s)
register int n;
register char *s;
{
register int i;
static char name[XL+1];

for(i=0;  i<n && *s!=' ' && *s!='\0' ; ++s)
	if(*s != '_')
		name[i++] = *s;

name[i] = '\0';

return( name );
}



char *copyn(n, s)
register int n;
register char *s;
{
register char *p, *q;

p = q = ckalloc(n);
while(--n >= 0)
	*q++ = *s++;
return(p);
}



char *copys(s)
char *s;
{
return( copyn( strlen(s)+1 , s) );
}



ftnint convci(n, s)
register int n;
register char *s;
{
ftnint sum;
sum = 0;
while(n-- > 0)
	sum = 10*sum + (*s++ - '0');
return(sum);
}

char *convic(n)
ftnint n;
{
static char s[20];
register char *t;

s[19] = '\0';
t = s+19;

do	{
	*--t = '0' + n%10;
	n /= 10;
	} while(n > 0);

return(t);
}



double convcd(n, s)
int n;
register char *s;
{
double atof();
char v[100];
register char *t;
if(n > 90)
	{
	err("too many digits in floating constant");
	n = 90;
	}
for(t = v ; n-- > 0 ; s++)
	*t++ = (*s=='d' ? 'e' : *s);
*t = '\0';
return( atof(v) );
}



struct Nameblock *mkname(l, s)
int l;
register char *s;
{
struct Hashentry *hp;
int hash;
register struct Nameblock *q;
register int i;
char n[VL];

hash = 0;
for(i = 0 ; i<l && *s!='\0' ; ++i)
	{
	hash += *s;
	n[i] = *s++;
	}
hash %= MAXHASH;
while( i < VL )
	n[i++] = ' ';

hp = hashtab + hash;
while(q = hp->varp)
	if( hash==hp->hashval && eqn(VL,n,q->varname) )
		return(q);
	else if(++hp >= lasthash)
		hp = hashtab;

if(++nintnames >= MAXHASH-1)
	many("names", 'n');
hp->varp = q = ALLOC(Nameblock);
hp->hashval = hash;
q->tag = TNAME;
cpn(VL, n, q->varname);
return(q);
}



struct Labelblock *mklabel(l)
ftnint l;
{
register struct Labelblock *lp;

if(l <= 0)
	return(NULL);

for(lp = labeltab ; lp < highlabtab ; ++lp)
	if(lp->stateno == l)
		return(lp);

if(++highlabtab > labtabend)
	many("statement numbers", 's');

lp->stateno = l;
lp->labelno = newlabel();
lp->blklevel = 0;
lp->labused = NO;
lp->labdefined = NO;
lp->labinacc = NO;
lp->labtype = LABUNKNOWN;
return(lp);
}


newlabel()
{
return( ++lastlabno );
}


/* find or put a name in the external symbol table */

struct Extsym *mkext(s)
char *s;
{
int i;
register char *t;
char n[XL];
struct Extsym *p;

i = 0;
t = n;
while(i<XL && *s)
	*t++ = *s++;
while(t < n+XL)
	*t++ = ' ';

for(p = extsymtab ; p<nextext ; ++p)
	if(eqn(XL, n, p->extname))
		return( p );

if(nextext >= lastext)
	many("external symbols", 'x');

cpn(XL, n, nextext->extname);
nextext->extstg = STGUNKNOWN;
nextext->extsave = NO;
nextext->extp = 0;
nextext->extleng = 0;
nextext->maxleng = 0;
nextext->extinit = NO;
return( nextext++ );
}








struct Addrblock *builtin(t, s)
int t;
char *s;
{
register struct Extsym *p;
register struct Addrblock *q;

p = mkext(s);
if(p->extstg == STGUNKNOWN)
	p->extstg = STGEXT;
else if(p->extstg != STGEXT)
	{
	errstr("improper use of builtin %s", s);
	return(0);
	}

q = ALLOC(Addrblock);
q->tag = TADDR;
q->vtype = t;
q->vclass = CLPROC;
q->vstg = STGEXT;
q->memno = p - extsymtab;
return(q);
}



frchain(p)
register chainp *p;
{
register chainp q;

if(p==0 || *p==0)
	return;

for(q = *p; q->nextp ; q = q->nextp)
	;
q->nextp = chains;
chains = *p;
*p = 0;
}


ptr cpblock(n,p)
register int n;
register char * p;
{
register char *q;
ptr q0;

q = q0 = ckalloc(n);
while(n-- > 0)
	*q++ = *p++;
return(q0);
}



max(a,b)
int a,b;
{
return( a>b ? a : b);
}


ftnint lmax(a, b)
ftnint a, b;
{
return( a>b ? a : b);
}

ftnint lmin(a, b)
ftnint a, b;
{
return(a < b ? a : b);
}




maxtype(t1, t2)
int t1, t2;
{
int t;

t = max(t1, t2);
if(t==TYCOMPLEX && (t1==TYDREAL || t2==TYDREAL) )
	t = TYDCOMPLEX;
return(t);
}



/* return log base 2 of n if n a power of 2; otherwise -1 */
#if FAMILY == PCC
log2(n)
ftnint n;
{
int k;

/* trick based on binary representation */

if(n<=0 || (n & (n-1))!=0)
	return(-1);

for(k = 0 ;  n >>= 1  ; ++k)
	;
return(k);
}
#endif



frrpl()
{
struct Rplblock *rp;

while(rpllist)
	{
	rp = rpllist->nextp;
	free(rpllist);
	rpllist = rp;
	}
}


popstack(p)
register chainp *p;
{
register chainp q;

if(p==NULL || *p==NULL)
	fatal("popstack: stack empty");
q = (*p)->nextp;
free(*p);
*p = q;
}



struct Exprblock *callk(type, name, args)
int type;
char *name;
chainp args;
{
register struct Exprblock *p;

p = mkexpr(OPCALL, builtin(type,name), args);
p->vtype = type;
return(p);
}



struct Exprblock *call4(type, name, arg1, arg2, arg3, arg4)
int type;
char *name;
expptr arg1, arg2, arg3, arg4;
{
struct Listblock *args;
args = mklist( mkchain(arg1, mkchain(arg2, mkchain(arg3, mkchain(arg4, NULL)) ) ) );
return( callk(type, name, args) );
}




struct Exprblock *call3(type, name, arg1, arg2, arg3)
int type;
char *name;
expptr arg1, arg2, arg3;
{
struct Listblock *args;
args = mklist( mkchain(arg1, mkchain(arg2, mkchain(arg3, NULL) ) ) );
return( callk(type, name, args) );
}





struct Exprblock *call2(type, name, arg1, arg2)
int type;
char *name;
expptr arg1, arg2;
{
struct Listblock *args;

args = mklist( mkchain(arg1, mkchain(arg2, NULL) ) );
return( callk(type,name, args) );
}




struct Exprblock *call1(type, name, arg)
int type;
char *name;
expptr arg;
{
return( callk(type,name, mklist(mkchain(arg,0)) ));
}


struct Exprblock *call0(type, name)
int type;
char *name;
{
return( callk(type, name, NULL) );
}



struct Impldoblock *mkiodo(dospec, list)
chainp dospec, list;
{
register struct Impldoblock *q;

q = ALLOC(Impldoblock);
q->tag = TIMPLDO;
q->varnp = dospec;
q->datalist = list;
return(q);
}




ptr ckalloc(n)
register int n;
{
register ptr p;
ptr calloc();

if( p = calloc(1, (unsigned) n) )
	return(p);

fatal("out of memory");
/* NOTREACHED */
}





isaddr(p)
register expptr p;
{
if(p->headblock.tag == TADDR)
	return(YES);
if(p->headblock.tag == TEXPR)
	switch(p->exprblock.opcode)
		{
		case OPCOMMA:
			return( isaddr(p->exprblock.rightp) );

		case OPASSIGN:
		case OPPLUSEQ:
			return( isaddr(p->exprblock.leftp) );
		}
return(NO);
}





addressable(p)
register expptr p;
{
switch(p->headblock.tag)
	{
	case TCONST:
		return(YES);

	case TADDR:
		return( addressable(p->addrblock.memoffset) );

	default:
		return(NO);
	}
}



hextoi(c)
register int c;
{
register char *p;
static char p0[17] = "0123456789abcdef";

for(p = p0 ; *p ; ++p)
	if(*p == c)
		return( p-p0 );
return(16);
}
