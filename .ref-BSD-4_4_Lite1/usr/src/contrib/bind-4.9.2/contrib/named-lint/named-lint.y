%{
#include <stdio.h>
#include <string.h>

#ifndef YYDEBUG
#define YYDEBUG 1
#endif

#define True 1
#define False 0
int	restofline=False;	/* eat till the \n */
int	justtokens=0;		/* >0 - want tokens not NUM NAMES... */
				/* <0 - want tokens and keywords */
				/* =0 - no tokens */
int lldebug=0;			/* !=0 lexical debugging */
int dup_ip=0;			/* check for duplicate ip addresses */
FILE *nfd;
FILE *cfd;

char *origin;

#define MAXNAME 256
char host[MAXNAME];

int numrr;			/* number of RRs for this host */
int numcname;			/* number of CNAME RRs for this host */

char *pgm;			/* name this program is invoked by */
char *inname;			/* name of the input file */

extern FILE *yyin;		/* defined in scanner */
extern char *yytext;		/* defined in scanner */
extern int yyleng;		/* defined in scanner */

struct inaddr {
  unsigned char b1, b2, b3, b4;
};
%}

%union {
  int i;
  char *c;
  struct inaddr ia;
}
%token INCLUDE ORIGIN
%token NUM NAME TOKEN RESTOFLINE
%token IN
%token A CNAME GID HINFO MB MG MINFO MR MX NS NULLRR
%token PTR RP SOA TXT UID UINFO UNSPEC WKS AFSDB
%token CAMPUS LOCATION MADDR PNAME OADDR
%token OPHONE EXTENSION ORGANIZATION OWNER PERSON ROOM

%expect 1

%%
input:	/* empty */
	| input line comment '\n'
	;

line:	/* empty */
	| nstmt		{ numrr++;
			  if (numcname != 0 && numrr > numcname)
			    yyerror("host has CNAME and other RRs"); 
			}
	| bstmt
	| error
	;

comment: /* empty */
	| cmnt
	;

cmnt:	';' restofline
	;

restofline: {restofline = True;} RESTOFLINE {restofline = False;}

dname:	NAME		{ $<c>$=$<c>1;}
	| '@'		{ $<c>$="@";}
	| '.'		{ $<c>$=".";}
	;

bstmt:	INCLUDE token
			{/* copy new filename */} 
		dname
			{ /* copy new origin */
			  /* indicate recursive parse */
			}
	| INCLUDE token
			{/* copy new filename */} 

	| ORIGIN dname
	;

nstmt:	dname {(void)strcpy(host, $<c>1);  numrr = numcname = 0; }
	     stmt	{(void)fprintf(nfd, "%s\n", host);}
	| stmt
	;

stmt:	NUM IN rr
	| NUM rr
	| IN rr
	| rr
	;
	
services:  /* empty */
	| services NAME
		{/* make sure the name is a service */}
	;

mline:	/* empty */
	| '\n'
	| cmnt '\n'
	;

rr:	  A addr cmnt
		{ 
		  if (dup_ip && iskeycmnt("noaddr", $<c>3))
		    (void)dup_check(host, origin, $<ia>2);
		}
	|  A addr
		{ if (dup_ip) (void)dup_check(host, origin, $<ia>2); }
	| CNAME NAME
		{ 
		  numcname++;
		  if (nstrcmp(host, $<c>2) == 0)
		      yyerror("host name and alias/CNAME are the same");
		}
	| GID NUM
	| HINFO token token
	| MB NAME
	| MG NAME
	| MINFO NAME
	| MR NAME
	| MX NUM NAME
	| NS NAME
	| NULLRR
	| PTR NAME
	| RP NAME NAME
	| AFSDB NUM NAME
	| SOA NAME NAME '(' mline
  		NUM mline NUM mline NUM mline NUM mline NUM mline ')'
	| TXT qtxtrr
	| UID NUM
	| UINFO '?'
	| WKS addr NAME {/* check for proper protocol */} services
	;

qtxtrr:	'"' txtrr '"'
	| txtrr
	;
  	
txtrr:	LOCATION baddr
	| MADDR token
	| PNAME restofline
	| OADDR baddr
	| OPHONE PHONENUM
	| ORGANIZATION whom
	| OWNER restofline
	| PERSON whom
	| NAME restofline
		{ /* unknown TXT RR */ }
	;

whom:	NAME
		{register char *p;
		 if ((p=strchr($<c>1, '.')) == NULL
		     || nstrcmp(p,".who.rutgers.edu") != 0)
		   yyerror("name doesn't end in .who.rutgers.edu");
	        }
	;

PHONENUM: phone EXTENSION NUM {if (yyleng!=4) yyerror("illegal extension");}
	| phone
	;

phone:	NUM {if (yyleng!=3) yyerror("illegal area code");}
	    '-' NUM {if (yyleng!=3) yyerror("illegal exchange");}
	    '-' NUM {if (yyleng!=4) yyerror("illegal phone number");}
	;

addr:	NUM '.' NUM '.' NUM '.' NUM
                { 
		  int i,e=0;
		  if ((i=atoi($<c>1)) > 255) { e++; goto addr_r;}
		  $<ia>$.b1 = i;
		  if ((i=atoi($<c>3)) > 255) { e++; goto addr_r;}
		  $<ia>$.b2 = i;
		  if ((i=atoi($<c>5)) > 255) { e++; goto addr_r;}
		  $<ia>$.b3 = i;
		  if ((i=atoi($<c>7)) > 255) { e++; goto addr_r;}
		  $<ia>$.b4 = i;
		addr_r:
		  if (e != 0)
                    yyerror("illegal internet addr");
		}
	;

baddr:	keytoks ROOM keytoks CAMPUS keytoks
	;

token:	{justtokens=1;} TOKEN {justtokens=0;}
	;

keytoks: {justtokens=-1;} toklst {justtokens=0;}
	;

toklst:	TOKEN
	| toklst TOKEN
	;
%%
main(argc, argv)
  int argc;
  char *argv[];
{
  extern char *optarg;
  extern int optind;
  int c, aerr, e;

  /* command to check for duplicate host names */
  static char ndup_cmd [] = "perl -e 'while (<>) {tr/A-Z/a-z/; $hosts{$_}++;'\
			     -e 'if ($hosts{$_} > 1) { ' \
			     -e 'print \"Duplicate host name $_\"; $v++; }' \
			     -e '} exit($v);' ";

  pgm = *argv;
  aerr = 0;
  while ((c=getopt(argc, argv, "o:iLYh?")) != -1)
    switch (c) {
      case 'L':	lldebug++; break;
      case 'Y':
#ifdef YYDEBUG
      		yydebug++;
#else !YYDEBUG
		fprintf(stderr, "%s: not compiled with YYDEBUG, -Y ignored\n",
			pgm);
#endif !YYDEBUG
		break;
      case 'i': dup_ip++; break;
      case 'o': origin = optarg; break;
      case 'h':
      case '?': aerr++;
    }
  if (aerr || ((optind+1) < argc)) {
    fprintf(stderr, "usage: %s [-L] [-Y] [-o origin] [-i] [file-name]\n", pgm);
    fprintf(stderr, "where:\tfile-name or stdinn is the input file\n");
    fprintf(stderr, "\t-i = duplicate IP address checking\n");
    fprintf(stderr, "\t-o = initialize the origin (ala $ORIGIN)\n");
    fprintf(stderr, "\t-L = Lex debugging\n");
    fprintf(stderr, "\t-Y = Yacc debugging\n");
    exit(-1);
  } else {
    if (optind >= argc) {
      inname = "<stdin>";
    } else {
      inname = argv[optind++];
      yyin = fopen(inname, "r");
      if (yyin == NULL) {
	fprintf("%s: Can't open file named: '%s'\n", pgm, inname);
	perror(pgm);
	exit(-1);
      }
    }
  }

  if ((nfd = popen(ndup_cmd, "w")) == NULL) {
    (void)fprintf(stderr, "%s: can't execute dup name checker\n", pgm);
    perror(pgm);
    exit(2);
  }

  if (dup_ip) {
#if 0
    static char dup_cmd[] = "sort -n +0 -1 +1 -2 +2 -3 +3 -4 \\\n\
      | awk ' $1==l1 && $2==l2 && $3==l3 && $4==l4 { \\\n\
      printf(\"duplicate IP address %d.%d.%d.%d, for: %s and %s\\n\",\\\n\
                l1, l2, l3, l4, l5, $5) } \\\n\
  	{ l1=$1 ; l2=$2 ; l3=$3 ; l4=$4 ; l5=$5 } ' \\\n\
	| tee /tmp/named-dup$$ \n\
	if [ -s /tmp/named-dup$$ ]; then \n\
	  stat=1 \n\
	else \n\
	  stat=0 \n\
        fi \n\
	rm -f /tmp/named-dup$$ \n\
 	exit $stat\n";
#endif
    static char dup_cmd[] = "/usr/local/bin/perl ./uniq-addr";
    if ((cfd = popen(dup_cmd, "w")) == NULL) {
      (void)fprintf(stderr, "%s: can't execute dup checker\n", pgm);
      perror(pgm);
      exit(2);
    }      
#if 0
    if ((cfd = fopen("/tmp/named-dup.raw","w")) == NULL) {
      (void)fprintf(stderr, "%s: can't open dup checker file: /tmp/named-dup.raw \n", pgm);
      perror(pgm);
      exit(2);
    }      
#endif
  }
  (void)yyparse();

  if ((e = pclose(nfd)) == -1)
    yynerrs++;
  else
    yynerrs += (e >> 8);
  

  if (dup_ip) {
#if 1
    if ((e=pclose(cfd)) == -1)
#else
    if ((e=fclose(cfd)) == -1)
#endif
      yynerrs++;
    else
      yynerrs += (e >> 8);	/* get the exit code not the whole status */
  }
  if (yynerrs != 0 )
    (void)printf("%s: %d errors\n",pgm, yynerrs);
  exit(yynerrs);
}

/* write out the record needed by the dup_ip code */
dup_check(host, origin, ia)
  char host[], origin[];
  struct inaddr ia;
{		    
  (void)fprintf(cfd, "%3d %3d %3d %3d %s%",
		ia.b1, ia.b2, ia.b3, ia.b4,
		host);
  if (origin!= NULL)
    (void)fprintf(cfd, ".%s\n", origin);
  else
    (void)fputc('\n', cfd);
}

/* compute the print size of a string 
 * if the string is printed starting at column off figure tab expansion and
 * count the size of the string
 */
int prtsize(str, off)
  char str[];
  int off;
{
  register char *s;
  register int c;

  for (s=str, c=off; *s!=NULL; c++, s++)
    if (*s == '\t')
      c = (((c/8)+1)*8) - 1;
  return (c);
}

extern int yylineno;
extern char linebuf[];

yyerror(s)
  char s[];
{
  register char c;
  register int i;

  (void)fprintf(stderr, "%s: %s on line %d, last recognized host name %s\n", 
		pgm, s, yylineno, host);
  i = prtsize(linebuf, strlen(pgm)+3); /* compute bogon pointer offset */
  /* reset the lexcial flags to the initial state */
  justtokens=0;
  restofline++;
  (void)yylex();		/* read the rest of the line */
  restofline=0;
  (void)fprintf(stderr, "%s: '%s'\n", pgm, linebuf);
  for (; i>0; i--)		/* pad out bogon pointer */
    (void)fputc(' ', stderr);
  (void)fprintf(stderr, "^\n");
}

#include <ctype.h>

/* check if the first word of a comment is the keyword */
iskeycmnt(key,cmnt)
  char key[], cmnt[];
{
  register char *c=cmnt+1;
  char *s,t;
  int r;

  while (*c == ' ' || *c == '\t') c++;
  s=c;
  while (isalpha(*c)) c++;
  t=*c; *c=NULL;
  r=nstrcmp(key, s);
  *c=t;
  return (r==0);
}


/* my lex replacement code */

#define iseol(c) ((c)=='\n')

FILE *yyin = {stdin};
#define input() fgetc(yyin)
#define unputc(c) ungetc(c, yyin)

yyinput() {return (input());}
yyunputc(c) char c; {return (unputc(c));}

#define MAXTOKEN 500
char linebuf[MAXTOKEN*10];	/* input line buffer */
char *eline;			/* end of input line */
char *yytext;			/* start of last 'token' */
int eollast = True;		/* say a \n last */
int yylineno=1;			/* line number */
int yyleng;			/* length of the last token (NUM only) */

yylex()
{
  char *c;

  if (lldebug)
    fprintf(stderr,"Entering yylex, justtokens=%d\n", justtokens);

  if (eollast && !restofline) {
    yytext = linebuf;
    eollast = False;
  } else
    yytext = eline;
  c = yytext;

  for(;;) {
    if (restofline) {
      if (!eollast) {
	while (! iseol(*c))
	  *++c=input();
	unputc(*c);
	*c = NULL;
	eline = c;
      }
      return (RESTOFLINE);
    }
    *c=input();
    if (*c == EOF)
      return (0);
    if (iseol(*c)) {
      *c = NULL;
      eollast++;
      yylineno++; 
      return ('\n');
    }
    if (isspace(*c)) {
      do *++c=input(); while (isspace(*c) && (! iseol(*c))) ;
      unputc(*c);
      *c = NULL;
      yytext = c;
      continue;			/* just ignore the spaces */
    }

    if (*c == '"')
      goto special;

    if (justtokens) {
      do *++c=input(); while (! isspace(*c) && ! iseol(*c) && *c != '"') ;
      unputc(*c);
      *c = NULL;
      eline = c;
      yylval.c = yytext;
      if (justtokens>0)
	return (TOKEN);
      /* else, check for a keyword */
      return (lookkey(yytext));
    }

    if (isdigit(*c)) {
      for (yyleng=0; isdigit(*c) ; yyleng++) 
	*++c = input();
      unputc(*c);
      *c = NULL;
      eline = c;
      yylval.c = yytext;
      return (NUM);
    }
    if (*c == '*') {
      *++c = input();
      if (*c == '.') {		/* an funny name? */
	*++c = input();
	if (! isalpha(*c)) {
	  unputc(*c--);
	  unputc(*c--);		/* put the '.' back also */
	}
      } else {
	unputc(*c--);
      }
    }
    if (isalpha(*c)) {
      do *++c = input(); while (isalnum(*c) || *c=='.' || *c=='-') ;
      if (*c == ':') {		/* maybe a TXT keyword? */
	int r;
	*++c = NULL;
	if ((r=lookkey(yytext)) != TOKEN) {
	  eline = c;		/* yes */
	  return (r);
	}
	c--;			/* no, unput ':', test for other keywords */
      }
      unputc(*c);
      *c = NULL;
      eline = c;
      yylval.c = yytext;
      return (lookup(yytext));
    }
    if (*c == '$') {
      do *++c = input(); while (isalnum(*c)) ;
      unputc(*c);
      *c = NULL;
      eline = c;
      if (nstrcmp(yytext, "$include") == 0)
	return (INCLUDE);
      if (nstrcmp(yytext, "$origin") == 0)
	return (ORIGIN);
      return (TOKEN);
    }
  special:
    eline = c;
    *++eline = NULL;
    return (*c);
  }
}

#define lc(c) (isupper(c)?tolower(c):(c))

/* case insensitive string compare (ala. strcmp(3)) */
nstrcmp(str1, str2)
  char str1[], str2[];
{
  register char *s, *t;
  for (s=str1, t=str2; *s!=NULL && *t!=NULL && lc(*s)==lc(*t); s++, t++) ;
  if (*s==NULL)
    if (*t==NULL) {
      return (0);
    } else {
      return (-1);
    }
  return (1);
}

/* symbol tables */

#define SYM struct sym
SYM {
  char *key;
  int token;
};

SYM dict[] = {
  {"a", A},
  {"cname", CNAME},
  {"gid", GID},
  {"hinfo", HINFO},
  {"in", IN},
  {"mb", MB},
  {"mg", MG},
  {"mr", MR},
  {"mx", MX},
  {"ns", NS},
  {"null", NULLRR},
  {"ptr", PTR},
  {"rp", RP},
  {"soa", SOA},
  {"txt", TXT},
  {"afsdb", AFSDB},
  {"uid", UID},
  {"uinfo", UINFO},
  {"unspec", UNSPEC},
  {"wks", WKS}
};
#define DICTSIZE (sizeof(dict)/sizeof(SYM))

SYM tdict[] = {
  {"campus:", CAMPUS},
  {"location:", LOCATION},
  {"mailaddr:", MADDR},
  {"name:", PNAME},
  {"officeaddr:", OADDR},
  {"extension:", EXTENSION},
  {"officephone:", OPHONE},
  {"organization:", ORGANIZATION},
  {"owner:", OWNER},
  {"person:", PERSON},
  {"room:", ROOM},
};
#define TDICTSIZE (sizeof(tdict)/sizeof(SYM))


int dict_compare(sym1, sym2)
  SYM *sym1, *sym2;
{
  return strcmp(sym1->key, sym2->key);
}

#ifdef NOPE
char *bsearch(key, base, nel, keysize, compar)
  char *key, *base;
  unsigned nel;
  int keysize;
  int (*compar)();
{
  register char *b;
  register i=nel;
  for (b=base, i=0; i<=nel; i++, b+=keysize)
    if ((*compar)(key, b) == 0)
      return (b);
}
#endif NOPE

lookkey(t)
  char t[];
{
  SYM *d, e;
  char name[MAXNAME];
  register char *f, *n;

  /* lowercase input */
  for (f=t, n=name; *f!=NULL; f++, n++)
    *n = (isupper(*f))?tolower(*f):*f;
  *n=NULL;
    
  e.key = name;

  d = (SYM *)bsearch((char *)(&e), (char *)tdict, TDICTSIZE,
		     sizeof(SYM), dict_compare);
  if (d != (SYM *)NULL)
    return (d->token);
  return (TOKEN);
}

lookup(t)
  char t[];
{
  SYM *d, e;
  char name[MAXNAME];
  register char *f, *n;

  /* lowercase input */
  for (f=t, n=name; *f!=NULL; f++, n++)
    *n = (isupper(*f))?tolower(*f):*f;
  *n=NULL;
    
  e.key = name;
  			 /* key          base   nel */
  d = (SYM *)bsearch((char *)(&e), (char *)dict, DICTSIZE,
		     /* sizeof(*key)     commpar */
		     sizeof(SYM), dict_compare);
  if (d != (SYM *)NULL)
    return(d->token);

  d = (SYM *)bsearch((char *)(&e), (char *)tdict, TDICTSIZE,
		     sizeof(SYM), dict_compare);
  if (d != (SYM *)NULL)
    return(d->token);
  return(NAME);
}

stophere()
{return;}
