char *xxxvers = "\nDeroff Version 2.0    29 December 1979\n";


#include <stdio.h>

/* Deroff command -- strip troff, eqn, and Tbl sequences from
a file.  Has two flags argument, -w, to cause output one word per line
rather than in the original format.
-ms (or -m) causes -ms macro's to be interpreted so that just
sentences are output, -mm does same for -mm macro's,
-ml in addition to interpreting -ms macros also gets rid of
lists.
Deroff follows .so and .nx commands, removes contents of macro
definitions, equations (both .EQ ... .EN and $...$),
Tbl command sequences, and Troff backslash constructions.

All input is through the C macro; the most recently read character is in c.
*/

#define C ( (c=getc(infile)) == EOF ? eof() : ((c==ldelim)&&(filesp==files) ? skeqn() : c) )
#define C1 ( (c=getc(infile)) == EOF ? eof() :  c)
#define SKIP while(C != '\n') 
#define SKIP_TO_COM SKIP; SKIP; pc=c; while(C != '.' || pc != '\n' || C > 'Z')pc=c

#define YES 1
#define NO 0
#define MS 0
#define MM 1
#define ONE 1
#define TWO 2

#define NOCHAR -2
#define SPECIAL 0
#define APOS 1
#define DIGIT 2
#define LETTER 3

int wordflag = NO;
int msflag = NO;
int mac = MS;
int disp = 0;
int inmacro = NO;
int intable = NO;

char chars[128];  /* SPECIAL, APOS, DIGIT, or LETTER */

char line[512];
char *lp;

int c;
int pc;
int ldelim	= NOCHAR;
int rdelim	= NOCHAR;


int argc;
char **argv;

char fname[50];
FILE *files[15];
FILE **filesp;
FILE *infile;

char *calloc();



main(ac, av)
int ac;
char **av;
{
register int i;
register char *p;
static char onechar[2] = "X";
FILE *opn();

argc = ac - 1;
argv = av + 1;

while(argc>0 && argv[0][0]=='-' && argv[0][1]!='\0') 
	{
	for(p=argv[0]+1; *p; ++p) switch(*p)
		{
		case 'w':
			wordflag = YES;
			break;
		case 'm':
			msflag = YES;
			if(*(p+1) == 'm'){
				mac=MM;
				p++;
			}
			else if(*(p+1) == 's')
				p++;
			else if(*(p+1) == 'l'){
				disp=1;
				p++;
			}
			break;
		default:
			onechar[0] = *p;
			fatal("Invalid flag %s\n", onechar);
		}
	--argc;
	++argv;
	}

if(argc == 0)
	infile = stdin;
else	{
	infile = opn(argv[0]);
	--argc;
	++argv;
	}

files[0] = infile;
filesp = &files[0];

for(i='a'; i<='z' ; ++i)
	chars[i] = LETTER;
for(i='A'; i<='Z'; ++i)
	chars[i] = LETTER;
for(i='0'; i<='9'; ++i)
	chars[i] = DIGIT;
chars['\''] = APOS;
chars['&'] = APOS;

work();
}



skeqn()
{
while((c = getc(infile)) != rdelim)
	if(c == EOF)
		c = eof();
	else if(c == '"')
		while( (c = getc(infile)) != '"')
			if(c == EOF)
				c = eof();
			else if(c == '\\')
				if((c = getc(infile)) == EOF)
					c = eof();
if(msflag)return(c='x');
return(c = ' ');
}


FILE *opn(p)
register char *p;
{
FILE *fd;

if(p[0]=='-' && p[1]=='\0')
	fd = stdin;
else if( (fd = fopen(p, "r")) == NULL)
	fatal("Cannot open file %s\n", p);

return(fd);
}



eof()
{
if(infile != stdin)
	fclose(infile);
if(filesp > files)
	infile = *--filesp;
else if(argc > 0)
	{
	infile = opn(argv[0]);
	--argc;
	++argv;
	}
else
	exit(0);

return(C);
}



getfname()
{
register char *p;
struct chain { struct chain *nextp; char *datap; } *chainblock;
register struct chain *q;
static struct chain *namechain	= NULL;
char *copys();

while(C == ' ') ;

for(p = fname ; (*p=c)!= '\n' && c!=' ' && c!='\t' && c!='\\' ; ++p)
	C;
*p = '\0';
while(c != '\n')
	C;

/* see if this name has already been used */

for(q = namechain ; q; q = q->nextp)
	if( ! strcmp(fname, q->datap))
		{
		fname[0] = '\0';
		return;
		}

q = (struct chain *) calloc(1, sizeof(*chainblock));
q->nextp = namechain;
q->datap = copys(fname);
namechain = q;
}




fatal(s,p)
char *s, *p;
{
fprintf(stderr, "Deroff: ");
fprintf(stderr, s, p);
exit(1);
}

work()
{

for( ;; )
	{
	if(C == '.'  ||  c == '\'')
		comline();
	else
		regline(NO,TWO);
	}
}




regline(macline,const)
int macline;
int const;
{
line[0] = c;
lp = line;
for( ; ; )
	{
	if(c == '\\')
		{
		*lp = ' ';
		backsl();
		}
	if(c == '\n') break;
	if(intable && c=='T')
		{
		*++lp = C;
		if(c=='{' || c=='}')
			{
			lp[-1] = ' ';
			*lp = C;
			}
		}
	else	*++lp = C;
	}

*lp = '\0';

if(line[0] != '\0')
	if(wordflag)
		putwords(macline);
	else if(macline)
		putmac(line,const);
	else
		puts(line);
}




putmac(s,const)
register char *s;
int const;
{
register char *t;

while(*s)
	{
	while(*s==' ' || *s=='\t')
		putchar(*s++);
	for(t = s ; *t!=' ' && *t!='\t' && *t!='\0' ; ++t)
		;
	if(*s == '\"')s++;
	if(t>s+const && chars[ s[0] ]==LETTER && chars[ s[1] ]==LETTER)
		while(s < t)
			if(*s == '\"')s++;
			else
				putchar(*s++);
	else
		s = t;
	}
putchar('\n');
}



putwords(macline)	/* break into words for -w option */
int macline;
{
register char *p, *p1;
int i, nlet;


for(p1 = line ; ;)
	{
	/* skip initial specials ampersands and apostrophes */
	while( chars[*p1] < DIGIT)
		if(*p1++ == '\0') return;
	nlet = 0;
	for(p = p1 ; (i=chars[*p]) != SPECIAL ; ++p)
		if(i == LETTER) ++nlet;

	if( (!macline && nlet>1)   /* MDM definition of word */
	   || (macline && nlet>2 && chars[ p1[0] ]==LETTER && chars[ p1[1] ]==LETTER) )
		{
		/* delete trailing ampersands and apostrophes */
		while(p[-1]=='\'' || p[-1]=='&')
			 --p;
		while(p1 < p) putchar(*p1++);
		putchar('\n');
		}
	else
		p1 = p;
	}
}



comline()
{
register int c1, c2;

com:
while(C==' ' || c=='\t')
	;
comx:
if( (c1=c) == '\n')
	return;
c2 = C;
if(c1=='.' && c2!='.')
	inmacro = NO;
if(c2 == '\n')
	return;

if(c1=='E' && c2=='Q' && filesp==files)
	eqn();
else if(c1=='T' && (c2=='S' || c2=='C' || c2=='&') && filesp==files){
	if(msflag){ stbl(); }
	else tbl(); }
else if(c1=='T' && c2=='E')
	intable = NO;
else if(c1=='G' && c2 == 'R')
		sdis('G','E');
else if(!inmacro && c1=='d' && c2=='e')
	macro();
else if(!inmacro && c1=='i' && c2=='g')
	macro();
else if(!inmacro && c1=='a' && c2 == 'm')
	macro();
else if(c1=='s' && c2=='o')
	{
	getfname();
	if( fname[0] )
		infile = *++filesp = opn( fname );
	}
else if(c1=='n' && c2=='x')
	{
	getfname();
	if(fname[0] == '\0') exit(0);
	if(infile != stdin)
		fclose(infile);
	infile = *filesp = opn(fname);
	}
else if(c1=='h' && c2=='w')
	{ SKIP; }
else if(msflag && c1 == 'T' && c2 == 'L'){
	SKIP_TO_COM;
	goto comx; }
else if(msflag && c1=='N' && c2 == 'R')SKIP;
else if(msflag && c1 == 'A' && (c2 == 'U' || c2 == 'I')){
	if(mac==MM)SKIP;
	else {
		SKIP_TO_COM;
		goto comx; }
	}
else if(msflag && c1 == 'F' && c2 == 'S'){
	SKIP_TO_COM;
	goto comx; }
else if(msflag && c1 == 'S' && c2 == 'H'){
	SKIP_TO_COM;
	goto comx; }
else if(msflag && c1 == 'N' && c2 == 'H'){
	SKIP_TO_COM;
	goto comx; }
else if(msflag && c1 == 'O' && c2 == 'K'){
	SKIP_TO_COM;
	goto comx; }
else if(msflag && c1 == 'N' && c2 == 'D')
	SKIP;
else if(msflag && mac==MM && c1=='H' && (c2==' '||c2=='U'))
	SKIP;
else if(msflag && mac==MM && c2=='L'){
	if(disp || c1 == 'R')sdis('L','E');
	else{
		SKIP;
		putchar('.');
		}
	}
else if(msflag && (c1 == 'D' || c1 == 'N' || c1 == 'K') && c2 == 'S')
	{ sdis(c1,'E'); }		/* removed RS-RE */
else if(msflag && c1 == 'n' && c2 == 'f')
	sdis('f','i');
else if(msflag && c1 == 'c' && c2 == 'e')
	sce();
else
	{
	if(c1=='.' && c2=='.')
		while(C == '.')
			;
	++inmacro;
	if(c1 <= 'Z' && msflag)regline(YES,ONE);
	else regline(YES,TWO);
	--inmacro;
	}
}



macro()
{
if(msflag){
	do { SKIP; }
		while(C!='.' || C!='.' || C=='.');	/* look for  .. */
	if(c != '\n')SKIP;
	return;
}
SKIP;
inmacro = YES;
}




sdis(a1,a2)
char a1,a2;
{
	register int c1,c2;
	register int eqnf;
	eqnf=1;
	SKIP;
	while(1){
		while(C != '.')SKIP;
		if((c1=C) == '\n')continue;
		if((c2=C) == '\n')continue;
		if(c1==a1 && c2 == a2){
			SKIP;
			if(eqnf)putchar('.');
			putchar('\n');
			return;
		}
		else if(a1 == 'D' && c1 == 'E' && c2 == 'Q'){eqn(); eqnf=0;}
		else SKIP;
	}
}
tbl()
{
while(C != '.');
SKIP;
intable = YES;
}
stbl()
{
while(C != '.');
SKIP_TO_COM;
if(c != 'T' || C != 'E'){
	SKIP;
	pc=c;
	while(C != '.' || pc != '\n' || C != 'T' || C != 'E')pc=c;
}
}

eqn()
{
register int c1, c2;
register int dflg;
int last;

last=0;
dflg = 1;
SKIP;

for( ;;)
	{
	if(C == '.'  || c == '\'')
		{
		while(C==' ' || c=='\t')
			;
		if(c=='E' && C=='N')
			{
			SKIP;
			if(msflag && dflg){
				putchar('x');
				putchar(' ');
				if(last){putchar('.'); putchar(' '); }
			}
			return;
			}
		}
	else if(c == 'd')	/* look for delim */
		{
		if(C=='e' && C=='l')
		    if( C=='i' && C=='m')
			{
			while(C1 == ' ');
			if((c1=c)=='\n' || (c2=C1)=='\n'
			    || (c1=='o' && c2=='f' && C1=='f') )
				{
				ldelim = NOCHAR;
				rdelim = NOCHAR;
				}
			else	{
				ldelim = c1;
				rdelim = c2;
				}
			}
			dflg = 0;
		}

	if(c != '\n') while(C != '\n'){ if(c == '.')last=1; else last=0; }
	}
}



backsl()	/* skip over a complete backslash construction */
{
int bdelim;

sw:  switch(C)
	{
	case '"':
		SKIP;
		return;
	case 's':
		if(C == '\\') backsl();
		else	{
			while(C>='0' && c<='9') ;
			ungetc(c,infile);
			c = '0';
			}
		--lp;
		return;

	case 'f':
	case 'n':
	case '*':
		if(C != '(')
			return;

	case '(':
		if(C != '\n') C;
		return;

	case '$':
		C;	/* discard argument number */
		return;

	case 'b':
	case 'x':
	case 'v':
	case 'h':
	case 'w':
	case 'o':
	case 'l':
	case 'L':
		if( (bdelim=C) == '\n')
			return;
		while(C!='\n' && c!=bdelim)
			if(c == '\\') backsl();
		return;

	case '\\':
		if(inmacro)
			goto sw;
	default:
		return;
	}
}




char *copys(s)
register char *s;
{
register char *t, *t0;

if( (t0 = t = calloc( strlen(s)+1, sizeof(*t) ) ) == NULL)
	fatal("Cannot allocate memory", (char *) NULL);

while( *t++ = *s++ )
	;
return(t0);
}
sce(){
register char *ap;
register int n, i;
char a[10];
	for(ap=a;C != '\n';ap++){
		*ap = c;
		if(ap == &a[9]){
			SKIP;
			ap=a;
			break;
		}
	}
	if(ap != a)n = atoi(a);
	else n = 1;
	for(i=0;i<n;){
		if(C == '.'){
			if(C == 'c'){
				if(C == 'e'){
					while(C == ' ');
					if(c == '0')break;
					else SKIP;
				}
				else SKIP;
			}
			else SKIP;
		}
		else {
			SKIP;
			i++;
		}
	}
}
