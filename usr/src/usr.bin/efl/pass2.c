#include "defs"
#include <ctype.h>

static int indent;

char *verb[] = { " ", " ", "continue", "call ", "do ", "if ", "if ",
	"goto ", "return", "read ", "write ", "format ", "stop ",
	"data ", "equivalence ", "common ", "external ",
	"rewind", "backspace", "endfile",
	"subroutine ", "function ", "program", "blockdata", "end", CNULL };

extern char *ops[];
ptr getsii();

/* generate code */

pass2()
{
exnull();
if(comments) putcomment();
if(verbose)
	fprintf(diagfile, "    Pass 2\n");

dclsect = 0;
indent = 0;

namegen();
dclgen();
body(iefile);
datas();
body(icfile);

p2stmt(0);
p2key(FEND);
p2flush();
if(verbose)
	fprintf(diagfile, "    Pass 2 done\n");
}

datas()
{
register int c, n;
int n1;

rewii(idfile);
swii(idfile);

for( ; ; )
	{
	c = getic(&n1);
	n = n1;
	switch(c)
		{
		case ICEOF:
			return;
	
		case ICMARK:
			break;
	
		case ICBLANK:
			putblank(n);
			break;
	
		case ICNAME:
			if(*ftnames[n] == '\0')
				fatal1("no name for n=%d", n);
			p2stmt(0);
			p2key(FDATA);
			p2str( ftnames[n] );
			break;
	
		case ICOP:
			p2str( ops[n] );
			break;
	
		case ICCONST:
			p2str( getsii(n) );
			break;
	
		default:
			fatal1("datas: invalid intermediate tag %d", c);
		}
	}
}

body(fileadd)
struct fileblock **fileadd;
{
int n1;
register int n;
register int c;
int prevc;
int ifn;

rewii(fileadd);
swii(fileadd);

prevc = 0;
ifn = 0;

for(;;)
	{
	c = getic(&n1);
	n = n1;
	switch(c)
		{
		case ICEOF:
			return;

		case ICBEGIN:
			if(n != 0)
				{
				if(prevc)
					p2key(FCONTINUE);
				else	prevc = 1;
				p2stmt( stnos[n] );
				}
			else if(!prevc)  p2stmt(0);
			break;

		case ICKEYWORD:
			p2key(n);
			if(n != FIF2)
				break;
			getic(&ifn);
			if( indifs[ifn] )
				skipuntil(ICMARK) ;
			break;

		case ICOP:
			p2str( ops[n] );
			break;

		case ICNAME:
			if(*ftnames[n]=='\0')
				fatal1("no name for n=%d", n);
			p2str( ftnames[n] );
			break;

		case ICCOMMENT:
			if(prevc)
				p2key(FCONTINUE);
			p2com(n);
			break;

		case ICBLANK:
			putblank(n);
			break;

		case ICCONST:
			p2str( getsii(n) );
			break;

		case ICINDPTR:
			n = indifs[n];

		case ICLABEL:
			p2str(" ");
			p2int( stnos[n] );
			break;

		case ICMARK:
			if( indifs[ifn] )
				{
				p2str(" ");
				p2key(FGOTO);
				p2int( stnos[ indifs[ifn] ] );
				}
			else
				{
				skipuntil(ICINDENT);
				p2str(" ");
				}
			break;

		case ICINDENT:
			indent = n * INDENTSPACES;
			p2indent(indent);
			break;

		default:
			sprintf(msg, "Bad pass2 value %o,%o", c,n);
			fatal(msg);
			break;
		}
	if(c!=ICBEGIN && c!=ICINDENT)
		prevc = 0;
	}
}

putname(p)
register ptr p;
{
register int i;

if(p->vextbase)
	{
	putic(ICNAME, p->vextbase);
	return;
	}

for(i=0 ; i<NFTNTYPES ; ++i)
	if(p->vbase[i])
		{
		putic(ICNAME, p->vbase[i]);
		return;
		}
if(strlen(p->sthead->namep) <= XL)
	fatal1("no fortran slot for name %s", p->sthead->namep);
}



putconst(ty, p)
int ty;
char *p;
{
ptr mkchcon();

if(ty != TYCHAR)
	putsii(ICCONST,p);
else	/* change character constant to a variable */
	putname( mkchcon(p) );
}


putzcon(p)
register ptr p;
{
char buff[100];
sprintf(buff, "(%s,%s)", p->leftp, p->rightp);
putsii(ICCONST,buff);
}






putcomment()
{
register ptr p;

for(p = comments ; p ; p = p->nextp)
	{
	putsii(ICCOMMENT, p->datap);
	cfree(p->datap);
	}
frchain(&comments);
}


putblank(n)
int n;
{
while(n-- > 0)
	p2putc(' ');
}



skipuntil(k)
int k;
{
register int i;
int n;

while( (i = getic(&n))!=k && i!=ICEOF)
	if(i==ICCOMMENT || i==ICCONST)
		getsii(n);
}


p2int(n)	/* put an integer constant in the output */
int n;
{
p2str( convic(n) );
}




p2key(n)	/* print a keyword */
int n;
{
p2str( verb[n] );
}



p2str(s)	/* write a character string on the output */
char *s;
{
int n;

n = strlen(s);
if(nftnch==LINESPACES-1 && (n==1 || (n==2 && s[1]==' ')) )
	p2putc(s[0]);

else	{
	if( n<=LINESPACES && nftnch+n>LINESPACES-1 )
		p2line( min(LINESPACES-n , indent+INDENTSPACES) );

	while(*s)
		p2putc(*s++);
	}
}



p2stmt(n)	/* start a statement with label n */
int n;
{
if(n > 0)
	fprintf(codefile,"\n%4d  ", n);
else	fprintf(codefile,"\n      ");

nftnch = 0;
nftncont = 0;
}


p2com(n)		/* copy a comment */
int n;
{
register int k;
register char *q;

q = getsii(n);
if(q[0] == '%')	/* a literal escape line */
	{
	putc('\n', codefile);
	while(--n > 0)
		putc(*++q, codefile);
	}
else	 /* actually a comment line */
	{
	++q;
	--n;

	do	{
		k = (n>71 ? 71 : n);
		fprintf(codefile, "\n");
		putc( tailor.ftnsys==CRAY ? 'C' : 'c' , codefile);
		while(k-- > 0)
			putc(*q++, codefile);
		n -= 71;
		}
		   while(n > 0);
	}
}




p2flush()
{
if(nftnch > 0)
	{
	fprintf(codefile, "\n");
	nftnch = 0;
	}
}




p2putc(c)
char c;
{
if(nftnch >= LINESPACES)	/* end of line */
	p2line(0);
if(tailor.ftnsys == CRAY)
	putc( islower(c) ? toupper(c) : c , codefile);
else
	putc(c, codefile);
++nftnch;
}



p2line(in)
int in;
{
register char contchar;

if(++nftncont > 19)
	{
	execerr("too many continuation lines", CNULL);
	contchar = 'X';
	}
if(tailor.ftncontnu == 1)
	fprintf(codefile, "\n&");
else	{	/* standard column-6 continuation */
	if(nftncont < 20)
		contchar = "0123456789ABCDEFGHIJ" [nftncont];
	fprintf(codefile, "\n     %c", contchar);
	}

nftnch = 0;
if(in > 0)
	p2indent(in);
}



p2indent(n)
register int n;
{
while(n-- > 0)
	p2putc(' ');
}
