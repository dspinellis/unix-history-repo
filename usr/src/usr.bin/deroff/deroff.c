#ifndef lint
static char sccsid[] = "@(#)deroff.c	4.2	(Berkeley)	82/11/06";
#endif not lint

char *xxxvers = "\nDeroff Version 1.02 (Bell Labs)    24 July 1978\n";


#include <stdio.h>

/* Deroff command -- strip troff, eqn, and Tbl sequences from
a file.  Has two flags argument, -w, to cause output one word per line
rather than in the original format.
-mm (or -ms) causes the corresponding macro's to be interpreted
so that just sentences are output
-ml  also gets rid of lists.
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
#define PUNCT 2
#define DIGIT 3
#define LETTER 4

int wordflag = NO;
int msflag = NO;
int mac = MM;
int disp = 0;
int parag = 0;
int inmacro = NO;
int intable = NO;

char chars[128];  /* SPECIAL, PUNCT, APOS, DIGIT, or LETTER */

char line[512];
char *lp;

int c;
int pc;
int ldelim	= NOCHAR;
int rdelim	= NOCHAR;


int argc;
char **argv;

extern int optind;
extern char*optarg;
char fname[50];
FILE *files[15];
FILE **filesp;
FILE *infile;

main(ac, av)
int ac;
char **av;
{
	register int i;
	int errflg = 0;
	register optchar;
	FILE *opn();

	argc = ac;
	argv = av;
	while ((optchar = getopt(argc, argv, "wpm:")) != EOF) switch(optchar) {
	case 'w':
		wordflag = YES;
		break;
	case 'm':
		msflag = YES;
		if (*optarg == 'm')
			mac = MM;
		else if (*optarg == 's')
			mac = MS;
		else if (*optarg == 'l')
			disp = 1;
		else errflg++;
		break;
	case 'p':
		parag=YES;
		break;
	case '?':
		errflg++;
	}
	if (errflg)
		fatal("usage: deroff [ -w ] [ -m (m s l) ] [ file ] ... \n", (char *) NULL);
	if ( optind == argc )
		infile = stdin;
	else
		infile = opn(argv[optind++]);
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
	chars['.'] = PUNCT;
	chars[','] = PUNCT;
	chars[';'] = PUNCT;
	chars['?'] = PUNCT;
	chars[':'] = PUNCT;
	work();
}
char *calloc();






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

	if( (fd = fopen(p, "r")) == NULL)
		fatal("Cannot open file %s\n", p);

	return(fd);
}



eof()
{
	if(infile != stdin)
		fclose(infile);
	if(filesp > files)
		infile = *--filesp;
	else if(optind < argc)
	{
		infile = opn(argv[optind++]);
	}
	else
		exit(0);

	return(C);
}



getfname()
{
	register char *p;
	struct chain { 
		struct chain *nextp; 
		char *datap; 
	} *chainblock;
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
	register found;
	int last;
	found = 0;

	while(*s)
	{
		while(*s==' ' || *s=='\t')
			putchar(*s++);
		for(t = s ; *t!=' ' && *t!='\t' && *t!='\0' ; ++t)
			;
		if(*s == '\"')s++;
		if(t>s+const && chars[ s[0] ]==LETTER && chars[ s[1] ]==LETTER){
			while(s < t)
				if(*s == '\"')s++;
				else
					putchar(*s++);
			last = *(t-1);
			found++;
		}
		else if(found && chars[ s[0] ] == PUNCT && s[1] == '\0')
			putchar(*s++);
		else{
			last = *(t-1);
			s = t;
		}
	}
	putchar('\n');
	if(msflag && chars[last] == PUNCT){
		putchar(last);
		putchar('\n');
	}
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
			while(p[-1]=='\'' || p[-1]=='&'|| chars[ p[-1] ] == PUNCT)
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
	if(msflag && c1 == '['){
		refer(c2);
		return;
	}
	if(parag && mac==MM && c1 == 'P' && c2 == '\n'){
		printf(".P\n");
		return;
	}
	if(c2 == '\n')
		return;

	if(c1 == '\\' && c2 == '\"')
		SKIP;
	else if(c1=='E' && c2=='Q' && filesp==files)
		eqn();
	else if(c1=='T' && (c2=='S' || c2=='C' || c2=='&') && filesp==files){
		if(msflag){ 
			stbl(); 
		}
		else tbl(); 
	}
	else if(c1=='T' && c2=='E')
		intable = NO;
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
	else if(c1 == 't' && c2 == 'm')
		SKIP;
	else if(c1=='h' && c2=='w')
		SKIP; 
	else if(msflag && c1 == 'T' && c2 == 'L'){
		SKIP_TO_COM;
		goto comx; 
	}
	else if(msflag && c1=='N' && c2 == 'R')SKIP;
	else if(parag && msflag && (c1 == 'P' || c1 == 'I' || c1 == 'L') && c2 == 'P'){
		printf(".%c%c",c1,c2);
		while(C != '\n')putchar(c);
		putchar('\n');
	}
	else if(parag && mac==MM && c1 == 'P' && c2 == ' '){
		printf(".%c%c",c1,c2);
		while(C != '\n')putchar(c);
		putchar('\n');
	}
	else if(msflag && c1 == 'A' && (c2 == 'U' || c2 == 'I')){
		if(mac==MM)SKIP;
		else {
			SKIP_TO_COM;
			goto comx; 
		}
	}
	else if(msflag && c1 == 'F' && c2 == 'S'){
		SKIP_TO_COM;
		goto comx; 
	}
	else if(msflag && (c1 == 'S' || c1 == 'N') && c2 == 'H'){
		if(parag){
			printf(".%c%c",c1,c2);
			while(C != '\n')putchar(c);
			putchar(c);
			putchar('!');
			while(1){
				while(C != '\n')putchar(c);
				putchar('\n');
				if(C == '.')goto com;
				putchar('!');
				putchar(c);
			}
		}
		else {
			SKIP_TO_COM;
			goto comx; 
		}
	}
	else if(c1 == 'U' && c2 == 'X'){
		if(wordflag)printf("UNIX\n");
		else printf("UNIX ");
	}
	else if(msflag && c1 == 'O' && c2 == 'K'){
		SKIP_TO_COM;
		goto comx; 
	}
	else if(msflag && c1 == 'N' && c2 == 'D')
		SKIP;
	else if(msflag && mac==MM && c1=='H' && (c2==' '||c2=='U')){
		if(parag){
			printf(".%c%c",c1,c2);
			while(C != '\n')putchar(c);
			putchar('\n');
		}
		else {
			SKIP;
		}
	}
	else if(msflag && mac==MM && c2=='L'){
		if(disp || c1 == 'R')sdis('L','E');
		else{
			SKIP;
			putchar('.');
		}
	}
	else if(!msflag &&c1 == 'P' && c2 == 'S'){
		inpic();
	}
	else if(msflag && (c1 == 'D' || c1 == 'N' || c1 == 'K'|| c1=='P') && c2 == 'S')
	{ 
		sdis(c1,'E'); 
	}		/* removed RS-RE */
	else if(msflag && (c1 == 'K' && c2 == 'F'))
	{ 
		sdis(c1,'E'); 
	}
	else if(msflag && c1 == 'n' && c2 == 'f')
		sdis('f','i');
	else if(msflag && c1 == 'c' && c2 == 'e')
		sce();
	else
	{
		if(c1=='.' && c2=='.'){
			if(msflag){
				SKIP;
				return;
			}
			while(C == '.')
				;
		}
		++inmacro;
		if(c1 <= 'Z' && msflag)regline(YES,ONE);
		else regline(YES,TWO);
		--inmacro;
	}
}



macro()
{
	if(msflag){
		do { 
			SKIP; 
		}		while(C!='.' || C!='.' || C=='.');	/* look for  .. */
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
	int lct;
	lct = 0;
	eqnf=1;
	SKIP;
	while(1){
		while(C != '.')
			if(c == '\n')continue;
			else SKIP;
		if((c1=C) == '\n')continue;
		if((c2=C) == '\n')continue;
		if(c1==a1 && c2 == a2){
			SKIP;
			if(lct != 0){
				lct--;
				continue;
			}
			if(eqnf)putchar('.');
			putchar('\n');
			return;
		}
		else if(a1 == 'L' && c2 == 'L'){
			lct++;
			SKIP;
		}
		else if(a1 == 'D' && c1 == 'E' && c2 == 'Q'){
			eqn(); 
			eqnf=0;
		}
		else if(a1 == 'f' && (c1 == 'P' || c2 == 'P')){
			SKIP;
			return;
		}
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
	char last;

	last=0;
	dflg = 1;
	SKIP;

	for( ;;)
	{
		if(C1 == '.'  || c == '\'')
		{
			while(C1==' ' || c=='\t')
				;
			if(c=='E' && C1=='N')
			{
				SKIP;
				if(msflag && dflg){
					putchar('x');
					putchar(' ');
					if(last){
						putchar(last); 
						putchar('\n'); 
					}
				}
				return;
			}
		}
		else if(c == 'd')	/* look for delim */
		{
			if(C1=='e' && C1=='l')
				if( C1=='i' && C1=='m')
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

		if(c != '\n') while(C1 != '\n'){ 
			if(chars[c] == PUNCT)last = c;
			else if(c != ' ')last = 0;
		}
	}
}


backsl()	/* skip over a complete backslash construction */
{
	int bdelim;

sw:  
	switch(C)
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
		if(msflag){
			if(C == 'e'){
				if(C == 'm'){
					*lp = '-';
					return;
				}
			}
			else if(c != '\n')C;
			return;
		}
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

	if( (t0 = t = calloc( (unsigned)(strlen(s)+1), sizeof(*t) ) ) == NULL)
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
					if(c == '0'){
						SKIP;
						break;
					}
					else SKIP;
				}
				else SKIP;
			}
			else if(c == 'P' || C == 'P'){
				if(c != '\n')SKIP;
				break;
			}
			else if(c != '\n')SKIP;
		}
		else {
			SKIP;
			i++;
		}
	}
}
refer(c1)
{
	register int c2;
	if(c1 != '\n')
		SKIP;
	while(1){
		if(C != '.')
			SKIP;
		else {
			if(C != ']')
				SKIP;
			else {
				while(C != '\n')
					c2=c;
				if(chars[c2] == PUNCT)putchar(c2);
				return;
			}
		}
	}
}
inpic(){
	register int c1;
	register char *p1;
	SKIP;
	p1 = line;
	c = '\n';
	while(1){
		c1 = c;
		if(C == '.' && c1 == '\n'){
			if(C != 'P'){
				if(c == '\n')continue;
				else { SKIP; c='\n'; continue;}
			}
			if(C != 'E'){
				if(c == '\n')continue;
				else { SKIP; c='\n';continue; }
			}
			SKIP;
			return;
		}
		else if(c == '\"'){
			while(C != '\"'){
				if(c == '\\'){
					if(C == '\"')continue;
					ungetc(c,infile);
					backsl();
				}
				else *p1++ = c;
			}
			*p1++ = ' ';
		}
		else if(c == '\n' && p1 != line){
			*p1 = '\0';
			if(wordflag)putwords(NO);
			else {
				puts(line);
				putchar('\n');
			}
			p1 = line;
		}
	}
}
