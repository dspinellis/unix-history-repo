# include "r.h"

char *keyword []{
	"do", "DO",	/* have to be first */
	"if", "IF",
	"else", "ELSE",
	"for", "FOR",
	"repeat", "REPEAT",
	"until", "UNTIL",
	"while", "WHILE",
	"break", "BREAK",
	"next", "NEXT",
	"define", "DEFINE",
	"include", "INCLUDE",
	0};

#include "y.tab.c"

int keytran[]{
	0, 0,
	XIF, XIF,
	XELSE, XELSE,
	XFOR, XFOR,
	REPEAT, REPEAT,
	UNTIL, UNTIL,
	XWHILE, XWHILE,
	XBREAK, XBREAK,
	NEXT, NEXT,
	XDEFINE, XDEFINE,
	XINCLUDE, XINCLUDE,
	0};

int	svargc;
char	**svargv;
int	infile	0;
int	fd	0;
int	ninclude	0;
int	filestack[10];
int	linect[10];

main(argc,argv) int argc; char **argv; {
	contfld = errorflag = 0;
	if(argc>1 && argv[1][0]=='-'){
		if(argv[1][1]=='6')
			contfld=6;
		argc--;
		argv++;
	}
	svargc = argc;
	svargv = argv;
	filestack[0] = infile = fd = ninclude = linect[0] = 0;
	if(--svargc>0)
		if( (fd = filestack[0] = copen(svargv[++infile],'r')) < 0 ) {
			error("can't open %s", svargv[infile]);
			cexit(1);
		}
	yyparse();
	cexit(errorflag);
}

int	peek	-1;
int	nextchar	'\n';

getc(){
	nextchar = (peek<0) ? gchar(): peek;
	peek = -1;
	return(nextchar);
}

int	gcp	0;
char	gcbuf[300];
int	apos	-1;

gchar(){
	extern int linect[], nnames;
	extern char *names[], *nameptr[];
	int c,i,atype,t;
	if( c=gcbuf[gcp++] )
		return(c);
   loop:
	for(gcp=0; (c=gcbuf[gcp]=cgetc(fd))!='\0' ; gcp++ ){
		if( gcbuf[0]== '%' ){
			while(putchar(cgetc(fd))!='\n');
			gcp = -1;
			++linect[ninclude];
			continue;
		}
		if( (atype=alphanum(c)) && apos < 0 ){
			apos = gcp;
			continue;
		}
		if( !atype )
			if( apos >= 0 ){
				gcbuf[gcp] = '\0';
				if( nnames>0 && (t=lookup(&gcbuf[apos],names))>=0){
					for(i=0;gcbuf[apos++]=nameptr[t][i];i++);
					gcp = apos-1;
				}
				apos = -1;
				gcbuf[gcp] = c;
			}
		if( c < ' ' && (c!='\n' && c!='\t') )	/* strip crap */
			c = gcbuf[gcp] = ' ';
		if( c=='#' ){
			gcbuf[gcp] = '\n';
			while( (c=cgetc(fd))!='\n' && c!='\0');
		}
		if( c=='"' || c=='\'' ){
			while( (gcbuf[++gcp]=t=cgetc(fd)) != c )
				if( t=='\n' ) {
					error("unbalanced quote");
					gcbuf[gcp] = c;
					gcbuf[++gcp] = c = '\n';
					goto newline;
				}
			continue;
		}
	newline:
		if( c=='\n' ){
			gcbuf[gcp+1] = '\0';
			gcp = 1;
			++linect[ninclude];
			return(gcbuf[0]);
		}
	}
	if(ninclude){
		cclose(filestack[ninclude--]);
		fd = filestack[ninclude];
		goto loop;
	}
	cclose(filestack[ninclude]);
	if(--svargc>0){
		if( (fd = filestack[ninclude] = copen(svargv[++infile],'r')) < 0) {
			error("can't open %s", svargv[infile]);
			cexit(1);
		}
		linect[0] = 0;
		goto loop;
	}
	return(0);
}

inclstat(){
	int i,c;
	char fname[100];
	while( (c=getc())==' ' || c=='\t' );
	peek = c;
	for(i=0; (fname[i]=c=getc())!='\n' && c!=';' && c!=' ' && c!='\t'; i++);
	fname[i] = '\0';
	if( (fd = copen(fname,'r')) < 0 ) {
		error("can't open %s", fname);
		cexit(1);
	}
	else {
		filestack[++ninclude] = fd;
		linect[ninclude] = 0;
	}
}

lookup(string,tbl) char *string; char *tbl[]; {
	register i,j, r;
	for(i=0; tbl[i]!=0; i++){ 
		for( j=0; (r=tbl[i][j])==string[j] && r!='\0'; j++);
		if( r == string[j] )
			return(i);
	}
	return( -1 );
}

char	str[200];
int	strp;
int	nstr;

yylex(){
	int c, type;
  top:
	while( (c=getc())==' ' || c=='\n' || c=='\t' );
	yylval = c;
	switch(c){

	case '\0':
		return('\0');
	case ';':
		return(SCOL);
	case'{':
		return(LCURL);
	case '}':
		return(RCURL);
	}
	peek = c;
	nstr = getstr(str);
	yylval = &str[0];
	if( alldigits(str) )
		return(DIGITS);
	type = lookup(str,keyword);
	if( keytran[type]==XDEFINE ) {
		defstat();
		goto top;
	} else if( keytran[type]==XINCLUDE ) {
		inclstat();
		goto top;
	} else if( type > 1 )
		return(keytran[type]);
	else if( type < 0 )
		return(XGOK);
	while( (c=getc())==' ' || c=='\t' || c=='\n' );
	peek = c;
	if( c>='a' && c<='z' || c>='A' && c<='Z' )
		return(NEWDO);
	else
		return(OLDDO);
}

getstr(s) char *s; {
	int  c, sp;
	for (sp=0; (c=s[sp++]=getc())!=' ' && c!='\t' && c!='\n' && c!='{' && c!='}'
		&& c!=';' && c!='(' && c!=')' ; )
			if( c=='\'' || c=='"' )
				while( (s[sp++]=getc())!=c );
	peek = c;
	s[--sp]='\0';
	return(sp);
}

alldigits(s) char *s; {
	int c;
	if( *s == '\0' )
		return(0);
	while( (c = *s++) != '\0' )
		if( c<'0' || c>'9' )
			return(0);
	return(1);
}

int	dbg	0;

yyerror(){;}

alphanum(c) int c; {
	if(c>='0' && c<='9') return(1);
	if(c>='a' && c<='z') return(1);
	if(c>='A' && c<='Z') return(1);
	return(0);
}

#define	MAXNAMES	100

char	*names[MAXNAMES];
char	*nameptr[MAXNAMES];
int	nnames	0;

defstat(){
	int c,i,index;
	extern int peek,nstr;
	extern char str[];
	char *getvec();
	while( (c=getc())==' ' || c=='\t' );
	peek = c;
	for(nstr=0; c=getc(); nstr++ ){
		if(c==' ' || c=='\t' || c=='\n') break;
		str[nstr] = c;
	}
	peek = c;
	str[nstr] = '\0';
	if( (index=lookup(str,names)) >= 0 )
		nameptr[index] = 0;
	else if( (index = nnames++)>=MAXNAMES-1 ){
		error("too many defined names");
		cexit(1);
	}
	names[index] = getvec(nstr+1);
	for( i=0; names[index][i]=str[i]; i++ );
	while( (c=getc())==' ' || c=='\t' );
	peek = c;
	for( i=0; (c=getc())!='\n' && c!='\0'; i++ )
		str[i] = c;
	str[i] = '\0';
	nameptr[index] = getvec(i+1);
	for( i=0; nameptr[index][i]=str[i]; i++ );
}

