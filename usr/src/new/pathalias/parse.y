%{
/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)parse.y	8.2 (down!honey) 86/01/29";
#endif lint

#include "def.h"

/* I thank Paul Haahr and Greg Noel for helping to clean this up. */
%}

%union {
	node	*y_node;
	Cost	y_cost;
	char	y_net;
	char	*y_name;
	struct {
		node *ys_node;
		Cost ys_cost;
		char ys_net;
		char ys_dir;
	} y_s;
}

%type <y_s>	site
%type <y_node>	links aliases plist network nlist host Psite Site
%type <y_cost>	cost cexpr

%token <y_name>	SITE HOST
%token <y_cost>	COST
%token <y_net>	NET
%token NL PRIVATE

%left	'+' '-'
%left	'*' '/'

%%
map	:	/* empty */
	|	map		NL
	|	map links	NL
	|	map aliases	NL
	|	map network	NL
	|	map private	NL
	|	error		NL
	;

links	: host site cost {
		if (GATEWAYED($2.ys_node))
			addgateway($1, $2.ys_node, $3, $2.ys_net, $2.ys_dir);
		else
			addlink($1, $2.ys_node, $3, $2.ys_net, $2.ys_dir);
	  }
				
	| links ',' site cost {
		if (GATEWAYED($3.ys_node))
			addgateway($1, $3.ys_node, $4, $3.ys_net, $3.ys_dir);
		else
			addlink($1, $3.ys_node, $4, $3.ys_net, $3.ys_dir);
	  }
	| links ','	/* permit this benign error */
	;

aliases	: host '=' Site		{alias($1, $3);}
	| aliases ',' Site	{alias($1, $3);}
	| aliases ','	/* permit this benign error */
	;

network	: host '=' '{' nlist '}' cost	{fixnet($1, $4, $6, DEFNET, DEFDIR);}
	| host '=' NET '{' nlist '}' cost	{fixnet($1, $5, $7, $3, LRIGHT);}
	| host '=' '{' nlist '}' NET cost	{fixnet($1, $4, $7, $6, LLEFT);}
	;

private	: PRIVATE '{' plist '}' ;

host	: HOST		{$$ = addnode($1);}
	| PRIVATE	{$$ = addnode("private");}
	;

Site	: SITE	{$$ = addnode($1);} ;

site	: Site {
		$$.ys_node = $1;
		$$.ys_net = DEFNET;
		$$.ys_dir = DEFDIR;
	  }
	| NET Site {
		$$.ys_node = $2;
		$$.ys_net = $1;
		$$.ys_dir = LRIGHT;
	  }
	| Site NET {
		$$.ys_node = $1;
		$$.ys_net = $2;
		$$.ys_dir = LLEFT;
	  }
	;

Psite	: SITE	{$$ = addprivate($1);} ;

plist	: Psite		{$1->n_flag |= ISPRIVATE;}
	| plist ',' Psite	{$3->n_flag |= ISPRIVATE;}
	| plist ','	/* permit this benign error  */
	;

nlist	: Site
	| nlist ',' Site {
		if ($3->n_net == 0) {
			$3->n_net = $1;
			$$ = $3;
		}
	  }
	| nlist ','	/* permit this benign error */
	;
		
cost	: {$$ = DEFCOST;	/* empty -- cost is always optional */}
	| '(' {Scanstate = COSTING;} cexpr {Scanstate = OTHER;} ')'
		{$$ = $3;}
	;

cexpr	: COST
	| '(' cexpr ')'   {$$ = $2;}
	| cexpr '+' cexpr {$$ = $1 + $3;}
	| cexpr '-' cexpr {$$ = $1 - $3;}
	| cexpr '*' cexpr {$$ = $1 * $3;}
	| cexpr '/' cexpr {
		if ($3 == 0)
			yyerror("zero divisor\n");
		else
			$$ = $1 / $3;
	  }
	;
%%

yyerror(s)
char *s;
{
	/* a concession to bsd error(1) */
	if (Cfile)
		fprintf(stderr, "\"%s\", ", Cfile);
	else
		fprintf(stderr, "%s: ", ProgName);
	fprintf(stderr, "line %d: %s\n", Lineno, s);
}

/*
 * patch in the costs of getting on/off the network.
 *
 * for each network member on netlist, add links:
 *	network -> member	cost = 0;
 *	member -> network	cost = parameter.
 *
 * if network and member both require gateways, assume network
 * is a gateway to member (but not v.v., to avoid such travesties
 * as topaz!seismo.css.gov.edu.rutgers).
 *
 * note that members can have varying costs to a network, by suitable
 * multiple declarations.  this is a feechur, albeit a useless one.
 */
fixnet(network, nlist, cost, netchar, netdir)
register node	*network;
node	*nlist;
Cost	cost;
char	netchar, netdir;
{
	register node	*member, *nextnet;
	link	*l;

	network->n_flag |= NNET;

	/* now insert the links */
	for (member = nlist ; member; member = nextnet) {
		/* network -> member, cost is 0 */
		if (GATEWAYED(network) && GATEWAYED(member))
			(void) addgateway(network, member, (Cost) 0, netchar, netdir);
		else
			(void) addlink(network, member, (Cost) 0, netchar, netdir);

		/* member -> network, cost is parameter */
		(void) addlink(member, network, cost, netchar, netdir);
		nextnet = member->n_net;
		member->n_net = 0;	/* clear for later use */
	}
}

/* scanner */

#define LBRACE '{'
#define RBRACE '}'
#define LPAREN '('
#define RPAREN ')'
#define QUOTE '"'

Cost isacost();

yylex()
{
	register int	c;
	Cost	cost;
	char	errbuf[128];
	static char	buf[128];	/* for return to yacc part */

tailrecursion:
	if (feof(stdin) && yywrap())
		return(EOF);

	if ((c = getchar()) == EOF)
		goto tailrecursion;

	while (c == ' ' || c == '\t')
		c = getchar();

	if (c == '\n') {
		Lineno++;
		c = getchar();
		if (c == ' ' || c == '\t')
			goto tailrecursion;
		ungetc(c, stdin);
		Scanstate = NEWLINE;
		return(NL);
	}

	if (c == '#') {
		while ((c = getchar()) != '\n')
			if (c == EOF)
				goto tailrecursion;
		ungetc(c, stdin);
		goto tailrecursion;
	}

	ungetc(c, stdin);

	switch(Scanstate) {
	case COSTING:
		if (isdigit(c)) {
			cost = 0;
			for (c = getchar(); isdigit(c); c = getchar())
				cost = (cost * 10) + c - '0';

			ungetc(c, stdin);
			yylval.y_cost = cost;
			return(COST);
		}

		
		if (getword(buf) == 0) {
			if ((yylval.y_cost = isacost(buf)) == 0) {
				sprintf(errbuf, "unknown cost (%s), using default", buf);
				yyerror(errbuf);
				yylval.y_cost = DEFCOST;
			}
			return(COST);
		}

		return(getchar());	/* can't be EOF */

	case NEWLINE:
		Scanstate = OTHER;
		if (getword(buf) != 0)
			return(getchar());	/* can't be EOF */
		/* `private' (but not `"private"')? */
		if (c == 'p' && strcmp(buf, "private") == 0)
			return(PRIVATE);

		yylval.y_name = buf;
		return(HOST);
	}

	if (getword(buf) == 0) {
		yylval.y_name = buf;
		return(SITE);
	}

	c = getchar();	/* can't be EOF */

	if (index(Netchars, c)) {
		yylval.y_net = c;
		return(NET);
	}

	return(c);
}

/*
 * fill str with the next word in [0-9A-Za-z][-._0-9A-Za-z]+ or a quoted
 * string that contains no newline.  return -1 on failure or EOF, 0 o.w.
 */ 
getword(str)
register char	*str;
{
	register int	c;

	c = getchar();
	if (c == QUOTE) {
		for ( ; (*str = getchar()) != '"'; str++) {
			if (*str == '\n') {
				yyerror("newline in quoted string\n");
				ungetc('\n', stdin);
				return(-1);
			}
		}
		*str = 0;
		return(0);
	}

	/* host name must start with alphanumeric or `.' */
	if (!isalnum(c) && c != '.') {
		ungetc(c, stdin);
		return(-1);
	}

yymore:
	do {
		*str++ = c;
		c = getchar();
	} while (isalnum(c) || c == '.' || c == '_');

	if (c == '-' && Scanstate != COSTING)
		goto yymore;

	ungetc(c, stdin);
	*str = 0;
	return(0);
}

static struct ctable {
	char *cname;
	Cost cval;
} ctable[] = {
	/*
	 * this list is searched sequentially (with strcmps!).
	 * it is too long.  (they are ordered by frequency of
	 * appearance in a "typical" dataset.)
	 *
	 * adding a 0 cost token breaks isacost().  don't do it.
	 */
	{"DEMAND", 300},
	{"DAILY", 5000},
	{"DIRECT", 200},
	{"EVENING", 1800},
	{"LOCAL", 25},
	{"LOW", 5},	/* baud rate penalty */
	{"HOURLY", 500},
	{"POLLED", 5000},
	{"DEDICATED", 95},
	{"WEEKLY", 30000},
	{"DEAD", INF/2},
	{"HIGH", -5},	/* baud rate bonus */
	/* the remainder are reviled */
	{"ARPA", 100},
	{"DIALED", 300},
	{"A", 300},
	{"B", 500},
	{"C", 1800},
	{"D", 5000},
	{"E", 30000},
	{"F", INF/2},
	0
};

STATIC Cost
isacost(buf)
register char	*buf;
{
	register struct ctable	*ct;

	for (ct = ctable; ct->cname; ct++)
		if (strcmp(buf, ct->cname) == 0)
			return(ct->cval);

	return((Cost) 0);
}

yywrap()
{
	char	errbuf[100];

	fixprivate();	/* munge private host definitions */

	if (Ifiles == 0) 
		return(1);

	fclose(stdin);
	while (*Ifiles) {
		Lineno = 1;
		if (fopen((Cfile = *Ifiles++), "r"))
			return(0);
		sprintf(errbuf, "%s: %s", ProgName, Cfile);
		perror(errbuf);
	}
	return(1);
}
