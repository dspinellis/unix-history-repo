%{
/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)parse.y	9.11 91/06/01";
#endif /* lint */

#include "def.h"

/* scanner states (yylex, parse) */
#define OTHER		0
#define COSTING		1
#define NEWLINE		2
#define FILENAME	3

/* exports */
long Tcount;
extern void yyerror();

/* imports */
extern node *addnode(), *addprivate();
extern void fixprivate(), alias(), deadlink(), deletelink();
extern link *addlink();
extern int strcmp();
extern char *strsave();
extern int optind;
extern char *Cfile, *Netchars, **Argv;
extern int Lineno, Argc;
extern node *Home;

/* privates */
STATIC void fixnet(), adjust();
STATIC int yylex(), yywrap(), getword();
static int Scanstate = NEWLINE;	/* scanner (yylex) state */

/* flags for ys_flags */
#define TERMINAL 1
%}

%union {
	node	*y_node;
	Cost	y_cost;
	char	y_net;
	char	*y_name;
	struct {
		node *ys_node;
		Cost ys_cost;
		short ys_flag;
		char ys_net;
		char ys_dir;
	} y_s;
}

%type <y_s>	site asite
%type <y_node>	links aliases plist network nlist host nhost
%type <y_node>	usite delem dlist
%type <y_cost>	cost cexpr

%token <y_name>	SITE HOST STRING
%token <y_cost>	COST
%token <y_net>	NET
%token EOL PRIVATE DEAD DELETE FILETOK ADJUST

%left	'+' '-'
%left	'*' '/'

%%
map	:	/* empty */
	|	map		EOL
	|	map links	EOL
	|	map aliases	EOL
	|	map network	EOL
	|	map private	EOL
	|	map dead	EOL
	|	map delete	EOL
	|	map file	EOL
	|	map adjust	EOL
	|	error		EOL
	;

links	: host site cost {
		struct link *l;

		l = addlink($1, $2.ys_node, $3, $2.ys_net, $2.ys_dir);
		if (GATEWAYED($2.ys_node))
			l->l_flag |= LGATEWAY;
		if ($2.ys_flag & TERMINAL)
			l->l_flag |= LTERMINAL;
	  }			
	| links ',' site cost {
		struct link *l;

		l = addlink($1, $3.ys_node, $4, $3.ys_net, $3.ys_dir);
		if (GATEWAYED($3.ys_node))
			l->l_flag |= LGATEWAY;
		if ($3.ys_flag & TERMINAL)
			l->l_flag |= LTERMINAL;
	  }
	| links ','	/* benign error */
	;

host	: HOST		{$$ = addnode($1);}
	| PRIVATE	{$$ = addnode("private");}
	| DEAD		{$$ = addnode("dead");}
	| DELETE	{$$ = addnode("delete");}
	| FILETOK	{$$ = addnode("file");}
	| ADJUST	{$$ = addnode("adjust");}
	;

site	: asite {
		$$ = $1;
		$$.ys_net = DEFNET;
		$$.ys_dir = DEFDIR;
	  }
	| NET asite {
		$$ = $2;
		$$.ys_net = $1;
		$$.ys_dir = LRIGHT;
	  }
	| asite NET {
		$$ = $1;
		$$.ys_net = $2;
		$$.ys_dir = LLEFT;
	  }
	;

asite	: SITE {
		$$.ys_node = addnode($1);
		$$.ys_flag = 0;
	  }
	| '<' SITE '>' {
		Tcount++;
		$$.ys_node = addnode($2);
		$$.ys_flag = TERMINAL;
	  }
	;

aliases	: host '=' SITE	{alias($1, addnode($3));}
	| aliases ',' SITE	{alias($1, addnode($3));}
	| aliases ','	/* benign error */
	;

network	: nhost '{' nlist '}' cost	{fixnet($1, $3, $5, DEFNET, DEFDIR);}
	| nhost NET '{' nlist '}' cost	{fixnet($1, $4, $6, $2, LRIGHT);}
	| nhost '{' nlist '}' NET cost	{fixnet($1, $3, $6, $5, LLEFT);}
	;

nhost	: '='		{$$ = 0;	/* anonymous net */}
	| host '='	{$$ = $1;	/* named net */}
	;

nlist	: SITE		{$$ = addnode($1);}
	| nlist ',' SITE {
		node *n;

		n = addnode($3);
		if (n->n_net == 0) {
			n->n_net = $1;
			$$ = n;
		}
	  }
	| nlist ','	/* benign error */
	;
		
private	: PRIVATE '{' plist '}'			/* list of privates */
	| PRIVATE '{' '}'	{fixprivate();}	/* end scope of privates */
	;

plist	: SITE			{addprivate($1)->n_flag |= ISPRIVATE;}
	| plist ',' SITE	{addprivate($3)->n_flag |= ISPRIVATE;}
	| plist ','		/* benign error */
	;

dead	: DEAD '{' dlist '}';

dlist	: delem
	| dlist ',' delem
	| dlist ','		/* benign error */
	;

delem	: SITE			{deadlink(addnode($1), (node *) 0);}
	| usite NET usite	{deadlink($1, $3);}
	;

usite	: SITE	{$$ = addnode($1);} ;	/* necessary unit production */

delete	: DELETE '{' dellist '}';

dellist	: delelem
	| dellist ',' delelem
	| dellist ','		/* benign error */
	;

delelem	: SITE {
		node *n;

		n = addnode($1);
		deletelink(n, (node *) 0);
		n->n_flag |= ISPRIVATE;
		/* reset Home if it's deleted */
		if (n == Home)
			Home = addnode(Home->n_name);
	  }
	| usite NET usite	{deletelink($1, $3);}
	;

file	: FILETOK '{' {Scanstate = FILENAME;} STRING {Scanstate = OTHER;} '}' {
		Lineno = 0;
		Cfile = strsave($4);
	}

adjust	: ADJUST '{' adjlist '}' ;

adjlist	: adjelem
	| adjlist ',' adjelem
	| adjlist ','		/* benign error */
	;

adjelem	: usite cost	{adjust($1, $2);} ;

cost	: {$$ = DEFCOST;	/* empty -- cost is always optional */}
	| '(' {Scanstate = COSTING;} cexpr {Scanstate = OTHER;} ')'
		{$$ = $3;}
	;

cexpr	: COST
	| '-' cexpr	  {$$ = -$2;}
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

void
#ifdef YYDEBUG
/*VARARGS1*/
yyerror(fmt, arg)
	char *fmt, *arg;
#else
yyerror(s)
	char *s;
#endif
{
	/* a concession to bsd error(1) */
	fprintf(stderr, "\"%s\", ", Cfile);
#ifdef YYDEBUG
	fprintf(stderr, "line %d: ", Lineno);
	fprintf(stderr, fmt, arg);
	putc('\n', stderr);
#else
	fprintf(stderr, "line %d: %s\n", Lineno, s);
#endif
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
STATIC void
fixnet(network, nlist, cost, netchar, netdir)
	register node *network;
	node *nlist;
	Cost cost;
	char netchar, netdir;
{	register node *member, *nextnet;
	link *l;
	static int netanon = 0;
	char anon[25];

	if (network == 0) {
		sprintf(anon, "[unnamed net %d]", netanon++);
		network = addnode(anon);
	}
	network->n_flag |= NNET;

	/* insert the links */
	for (member = nlist ; member; member = nextnet) {

		/* network -> member, cost is 0 */
		l = addlink(network, member, (Cost) 0, netchar, netdir);
		if (GATEWAYED(network) && GATEWAYED(member))
			l->l_flag |= LGATEWAY;

		/* member -> network, cost is parameter */
		/* never ever ever crawl up from a domain*/
		if (!ISADOMAIN(network))
			(void) addlink(member, network, cost, netchar, netdir);

		nextnet = member->n_net;
		member->n_net = 0;	/* clear for later use */
	}
}

/* scanner */

#define QUOTE '"'
#define STR_EQ(s1, s2) (s1[2] == s2[2] && strcmp(s1, s2) == 0)
#define NLRETURN() {Scanstate = NEWLINE; return EOL;}

static struct ctable {
	char *cname;
	Cost cval;
} ctable[] = {
	/* ordered by frequency of appearance in a "typical" dataset */
	{"DIRECT", 200},
	{"DEMAND", 300},
	{"DAILY", 5000},
	{"HOURLY", 500},
	{"DEDICATED", 100},
	{"EVENING", 2000},
	{"LOCAL", 25},
	{"LOW", 5},	/* baud rate, quality penalty */
	{"DEAD", MILLION},
	{"POLLED", 5000},
	{"WEEKLY", 30000},
	{"HIGH", -5},	/* baud rate, quality bonus */
	{"FAST", -80},	/* high speed (>= 9.6 kbps) modem */
	/* deprecated */
	{"ARPA", 100},
	{"DIALED", 300},
	{0, 0}
};

STATIC int
yylex()
{	static char retbuf[128];	/* for return to yacc part */
	register int c;
	register char *buf = retbuf;
	register struct ctable *ct;
	register Cost cost;
	char errbuf[128];

	if (feof(stdin) && yywrap())
		return EOF;

	/* count lines, skip over space and comments */
	if ((c = getchar()) == EOF)
		NLRETURN();
    
continuation:
	while (c == ' ' || c == '\t')
		if ((c = getchar()) == EOF)
			NLRETURN();

	if (c == '#')
		while ((c = getchar()) != '\n')
			if (c == EOF)
				NLRETURN();

	/* scan token */
	if (c == '\n') {
		Lineno++;
		if ((c = getchar()) != EOF) {
			if (c == ' ' || c == '\t')
				goto continuation;
			ungetc(c, stdin);
		}
		NLRETURN();
	}

	switch(Scanstate) {
	case COSTING:
		if (isdigit(c)) {
			cost = c - '0';
			for (c = getchar(); isdigit(c); c = getchar())
				cost = (cost * 10) + c - '0';
			ungetc(c, stdin);
			yylval.y_cost = cost;
			return COST;
		}

		if (getword(buf, c) == 0) {
			for (ct = ctable; ct->cname; ct++)
				if (STR_EQ(buf, ct->cname)) {
					yylval.y_cost = ct->cval;
					return COST;
				}
			sprintf(errbuf, "unknown cost (%s), using default", buf);
			yyerror(errbuf);
			yylval.y_cost = DEFCOST;
			return COST;
		}

		return c;	/* pass the buck */

	case NEWLINE:
		Scanstate = OTHER;
		if (getword(buf, c) != 0)
			return c;
		/*
		 * special purpose tokens.
		 *
		 * the "switch" serves the dual-purpose of recognizing
		 * unquoted tokens only.
		 */
		switch(c) {
		case 'p':
			if (STR_EQ(buf, "private"))
				return PRIVATE;
			break;
		case 'd':
			if (STR_EQ(buf, "dead"))
				return DEAD;
			if (STR_EQ(buf, "delete"))
				return DELETE;
			break;
		case 'f':
			if (STR_EQ(buf, "file"))
				return FILETOK;
			break;
		case 'a':
			if (STR_EQ(buf, "adjust"))
				return ADJUST;
			break;
		}

		yylval.y_name = buf;
		return HOST;

	case FILENAME:
		while (c != EOF && isprint(c)) {
			if (c == ' ' || c == '\t' || c == '\n' || c == '}')
				break;
			*buf++ = c;
			c = getchar();
		}
		if (c != EOF)
			ungetc(c, stdin);
		*buf = 0;
		yylval.y_name = retbuf;
		return STRING;
	}

	if (getword(buf, c) == 0) {
		yylval.y_name = buf;
		return SITE;
	}

	if (index(Netchars, c)) {
		yylval.y_net = c;
		return NET;
	}

	return c;
}

/*
 * fill str with the next word in [0-9A-Za-z][-._0-9A-Za-z]+ or a quoted
 * string that contains no newline.  return -1 on failure or EOF, 0 o.w.
 */ 
STATIC int
getword(str, c)
	register char *str;
	register int c;
{
	if (c == QUOTE) {
		while ((c = getchar()) != QUOTE) {
			if (c == '\n') {
				yyerror("newline in quoted string\n");
				ungetc(c, stdin);
				return -1;
			}
			if (c == EOF) {
				yyerror("EOF in quoted string\n");
				return -1;
			}
			*str++ = c;
		}
		*str = 0;
		return 0;
	}

	/* host name must start with alphanumeric or `.' */
	if (!isalnum(c) && c != '.')
		return -1;

yymore:
	do {
		*str++ = c;
		c = getchar();
	} while (isalnum(c) || c == '.' || c == '_');

	if (c == '-' && Scanstate != COSTING)
		goto yymore;

	ungetc(c, stdin);
	*str = 0;
	return 0;
}

STATIC int
yywrap()
{	char errbuf[100];

	fixprivate();	/* munge private host definitions */
	Lineno = 1;
	while (optind < Argc) {
		if (freopen((Cfile = Argv[optind++]), "r", stdin) != 0)
			return 0;
		sprintf(errbuf, "%s: %s", Argv[0], Cfile);
		perror(errbuf);
	}
	freopen("/dev/null", "r", stdin);
	return -1;
}

STATIC void
adjust(n, cost)
	node *n;
	Cost cost;
{	link *l;

	n->n_cost += cost;	/* cumulative */

	/* hit existing links */
	for (l = n->n_link; l; l = l->l_next) {
		if ((l->l_cost += cost) < 0) {
			char buf[100];

			l->l_flag |= LDEAD;
			sprintf(buf, "link to %s deleted with negative cost",
							l->l_to->n_name);
			yyerror(buf);
		}
	}
}
