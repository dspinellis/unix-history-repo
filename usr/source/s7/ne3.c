# include "ne.h"

char *res[] {
	">=", "<=", "!=",
	"+-", "==", "cdot", "CDOT",
	"times", "TIMES",
	"SIGMA", "pi", "PI",
	"alpha", "beta", "gamma", "GAMMA", "delta", "epsilon", "omega",
	"DELTA", "LAMBDA", "PHI", "OMEGA",
	"lambda", "mu", "nu", "theta", "rho", "sigma", "tau", "phi",
	"INF", "INFINITY",
	"inf", "infinity",
	"partial", "PARTIAL",
	"zeta", "eta", "iota", "kappa", "xi", "omicron", "upsilon",
	"chi", "psi", "THETA", "XI", "UPSILON", "PSI",
	"del", "DEL",
	"nothing", "NOTHING",
	"approx", "APPROX",
	0};
char *restran[] {
	">\b_", "<\b_", "/\b=",
	"+\b_", "=\b_", "8.9", "8.9",
	"x", "x",
	"R", "J", "P",
	"A", "B", "\\e", "G", "D", "S", "C",
	"W", "E", "F", "Z",
	"L", "M", "@", "T", "K", "Y", "I", "U",
	"o", "o", "o", "o",
	"]", "]",
	"Q", "N", "i", "k", "X", "o", "u",
	"X", "V", "O", "X", "U", "H",
	"[", "[",
	"", "",
	"~\b=", "~\b=",
	0};

int	csp;
int	psp;
#define	CSSIZE	400
char	cs[420];

text(t,p1) int t; char *p1; {
	int i,j,c;
	int w;
	yyval = oalloc();
	ebase[yyval] = 0;
	eht[yyval] = 2;	/* ht in 1/2 spaces */
	if( t=='q' )
		j = p1;
	else if ( t == '~' )
		j = &"~";
	else if ( t == '^' )
		j = &"";
	else if ( t == '\t' )
		j = &"\\t";
	else if( (i=lookup(p1,res))>=0 )
		j = restran[i];
	else {
		for( csp=psp=0; (c=p1[psp++])!='\0'; ){
			trans(c,p1);
			if( csp>CSSIZE )
				error(FATAL,"converted token %.20s... too long",p1);
		}
		cs[csp] = '\0';
		j = cs;
	}
	ewid[yyval] = width(j);
	if(dbg)printf(".\t%ctext: S%d <- %s; b=%d,h=%d,w=%d\n",
		t, yyval, j, ebase[yyval], eht[yyval], ewid[yyval]);
	printf(".ds %d \"%s\n",yyval,j);
}

width(s) char *s; {
	int c,w;
	w = 0;
	while( (c = *s++) != '\0' ){
		if( c == '\b' || c == 033 )
			w--;
		else if ( c == '\\' && *s == '\\' );
		else if ( c == '\\' && *s == 'e' );
		else if ( c >= 040 )
			w++;
	}
	return(w);
}

trans(c,p1) int c; char *p1; {
	switch( c){
	case '>': case '<': case '=':
		if( p1[psp]=='=' ){	/* look ahead for == <= >= */
			cs[csp++] = c; cs[csp++] = '\b'; cs[csp++] = '_';
			psp++;
		} else 
			cs[csp++] = c;
		break;
	case '\\':	/* troff - pass 2 or 3 more chars */
		cs[csp++] = c; cs[csp++] = c = p1[psp++]; cs[csp++] = p1[psp++];
		if( c=='(' ) cs[csp++] = p1[psp++];
		break;
	default:
		cs[csp++] = c; break;
	}
}
