#include "r.h"

char	scrat[500];

int	brkptr	-1;
int	brkstk[10];
int	forptr	0;
int	forstk[10];

repcode() {
	outcont(0);
	yyval = genlab();
	outcont(yyval);
	brkstk[++brkptr] = yyval+1;
	genlab();
	genlab();
}

untils(p1) int p1; {
	outnum(p1+1);
	outcode("\tif(.not.");
	balpar(scrat);
	outcode(scrat);
	outcode(")");
	outgoto(p1);
	outcont(p1+2);
	brkptr--;
}

ifcode(p1) int p1; {
	outcode("\tif(.not.");
	balpar(scrat);
	outcode(scrat);
	outcode(")");
	outgoto(yyval=genlab()); genlab();
}

whilecode(p1) int p1; {
	outcont(0);
	brkstk[++brkptr] = yyval = genlab(); genlab();
	outnum(yyval);
	outcode("\tif(.not.");
	balpar(scrat);
	outcode(scrat);
	outcode(")");
	outgoto(yyval+1);
}

whilestat(p1) int p1; {
	outgoto(p1);
	outcont(p1+1);
	brkptr--;
}

balpar(bp) char *bp; {
	int  i, c, lpar;
	extern int peek;
	while( (c=getc()) == ' ' || c == '\t' || c=='\n' );
	peek = c;
	if( c != '(' ){
		error("missing left paren");
		bp[0] = '\0';
		return(bp);
	}
	for( lpar=i=0; (bp[i++]=c=getc())!='\0'; ){
		if( c=='\'' || c=='"' )
			while( (bp[i++]=getc()) != c );
		if( i>=499 || c=='{' || c=='}' ){
			error("missing right parenthesis at %.20s", bp);
			break;
		}
		if( c=='(' )
			lpar++;
		else if( c==')' )
			lpar--;
		if( lpar == 0 )
			break;
	}
	bp[i] = '\0';
	return(bp);
}

int	labval	23000;

genlab(){
	return(++labval);
}

gokcode(p1) char *p1; {
	outcode("\t");
	outcode(p1);
	eatup(p1,scrat);
	outcode(scrat);
	outcode(0);
}

eatup(p1,bp) char *p1, *bp; {
	extern int peek;
	int i,c,lnb,lpar;
	lnb = '\n';
	while( c = *p1++ )
		if( c!=' ' )
			lnb = c;
	i = lpar = 0;
  more:
	for( ; (bp[i++]=c=getc())!=';' && c!='{' && c!='\n' && c!='}'; ){
		if( i>=499 ){
			error("statement too long at %.20s", bp);
			break;
		}
		if( c != ' ' && c != '\t' )
			lnb = c;
		if( c=='\'' || c=='"' )
			while( (bp[i++]=getc()) != c );
		if( c=='(' )
			lpar++;
		else if( c==')' ) {
			lpar--;
			if( lpar < 0 )
				error("missing left paren at %.20s",bp);
		}
	}
	if( c == '\n' ){
		if( lnb=='\n' || lnb=='+' || lnb=='-' || lnb=='*' || lnb=='('
			|| lnb=='/' || lnb==',' || lnb=='&'  || lnb=='|'
			|| lnb=='=' )
				goto more;
		c = ';';
	}
	if( c!=';' )
		peek = c;
	bp[i-1] = '\0';
	if( lpar > 0 )
		error("missing right paren at %.20s",bp);
	return(bp);
}

forcode(){
	extern int peek;
	int i,j,c;
	char *bp, *getvec();
	outcont(0);
	balpar(scrat);
	yyval = genlab(); genlab(); genlab();
	brkstk[++brkptr] = yyval+1;
	if( scrat[0] == '\0' ){
		forstk[forptr++] = bp = getvec(1);
		*bp = '\0';
		return;
	}
	scrat[0] = '\t';
	for( i=1; (c=scrat[i++])!=';' && c!='\0' ; )
		if( c=='\'' || c=='"' )
			while( scrat[i++] != c );
	scrat[i-1] = '\0';
	if( nonblank(scrat) ){
		outcode(scrat);
		outcode(0);
	}
	for( j=i; (c=scrat[i++])!=';' && c!='\0' ; )
		if( c=='\'' || c=='"' )
			while( scrat[i++] != c );
	scrat[i-1] = '\0';
	if( nonblank(&scrat[j]) ){
		outnum(yyval);
		outcode("\tif(.not.(");
		outcode(&scrat[j]);
		outcode("))");
		outgoto(yyval+2);
	}
	else
		outcont(yyval);
	for( j=0; scrat[i+1]!='\0'; )
		scrat[j++] = scrat[i++];
	scrat[j] = '\0';
	forstk[forptr++] = bp = getvec(j+1);
	for(i=0; *bp++ = scrat[i++]; );
}

forstat(p1) int p1; {
	char *bp, *q;
	int i;
	bp = forstk[--forptr];
	outnum(p1+1);
	if( nonblank(bp) ){
		outcode("\t");
		outcode(bp);
		outcode(0);
	}
	outgoto(p1);
	outcont(p1+2);
	for( q=bp; *q++; );
	relvec(bp, q-bp);
	brkptr--;
}

docode(new,p1) int new; char *p1; {
	outcode("\t");
	outcode(p1);
	eatup(p1,scrat);
	yyval = 0;
	if(new){
		yyval = genlab(); genlab();
		brkstk[++brkptr] = yyval;
		outnum(yyval);
	}
	outcode(scrat);
	outcode(0);
}

dostat(p1) int p1; {
	if( p1==0 )
		return;
	outcont(p1);
	outcont(p1+1);
	brkptr--;
}

breakcode(p1) int p1; {
	if(brkptr<0){
		error("illegal BREAK");
		return;
	}
	outgoto(brkstk[brkptr]+1);
}

nextcode(p1) int p1; {
	if(brkptr<0){
		error("illegal NEXT");
		return;
	}
	outgoto(brkstk[brkptr]);
}

nonblank(s) char *s; {
	int c;
	while( c = *s++ )
		if( c!=' ' && c!='\t' && c!='\n' )
			return(1);
	return(0);
}

error(s1, s2) char *s1, *s2; {
	extern int linect[],ninclude,infile;
	printf( 2, "error at line %d, file %d: ",linect[ninclude],infile);
	printf( 2, s1,s2);
	printf( 2, "\n");
	errorflag = 1;
}

errcode(p1) char *p1; {
	int c;
	extern int yychar;
	extern int linect[],ninclude,infile;
	printf( 2, "\nsyntax error, line %d, file %d\n", linect[ninclude],infile);
	while( (c=getc()) != ';' && c != '}' && c != '\n' && c != '\0' );
	yychar = -1;
	errorflag = 1;
}
