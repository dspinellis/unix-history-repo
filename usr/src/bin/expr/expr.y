%{
/* Written by Pace Willisson (pace@blitz.com) 
 * and placed in the public domain
 */
#include <stdio.h>
#include <ctype.h>

char *malloc ();
char *calloc ();

struct val {
	char *sval;
	int ival;
	int iflag;
};

struct val *result;

struct val *op_or ();
struct val *op_and ();
struct val *op_eq ();
struct val *op_gt ();
struct val *op_lt ();
struct val *op_ge ();
struct val *op_le ();
struct val *op_ne ();
struct val *op_plus ();
struct val *op_minus ();
struct val *op_times ();
struct val *op_div ();
struct val *op_rem ();
struct val *op_colon ();

char **av;
%}

%union
{
	struct val *val;
}

%left <val> '|'
%left <val> '&'
%left <val> '=' '>' '<' GE LE NE
%left <val> '+' '-'
%left <val> '*' '/' '%'
%left <val> ':'
%left UNARY

%token <val> TOKEN
%type <val> start expr

%%

start: expr { result = $$; }

expr:	TOKEN
	| '(' expr ')' { $$ = $2; }
	| expr '|' expr { $$ = op_or ($1, $3); }
	| expr '&' expr { $$ = op_and ($1, $3); }
	| expr '=' expr { $$ = op_eq ($1, $3); }
	| expr '>' expr { $$ = op_gt ($1, $3); }
	| expr '<' expr { $$ = op_lt ($1, $3); }
	| expr GE expr  { $$ = op_ge ($1, $3); }
	| expr LE expr  { $$ = op_le ($1, $3); }
	| expr NE expr  { $$ = op_ne ($1, $3); }
	| expr '+' expr { $$ = op_plus ($1, $3); }
	| expr '-' expr { $$ = op_minus ($1, $3); }
	| expr '*' expr { $$ = op_times ($1, $3); }
	| expr '/' expr { $$ = op_div ($1, $3); }
	| expr '%' expr { $$ = op_rem ($1, $3); }
	| expr ':' expr { $$ = op_colon ($1, $3); }
	| '-' expr %prec UNARY { $$ = op_minus (NULL, $2); }
	;


%%

struct val *
make_val (sval)
char *sval;
{
	struct val *vp;
	char *p;

	if ((vp = (struct val *)calloc (1, sizeof *vp)) == NULL
	    || (vp->sval = malloc (strlen (sval) + 1)) == NULL) {
		fprintf (stderr, "out of memory\n");
		exit (2);
	}

	strcpy (vp->sval, sval);
	
	p = sval;

	if (*p == '-')
		p++;
	while (isdigit (*p))
		p++;
	if (*p == 0) {
		vp->iflag = 1;
		vp->ival = atoi (sval);
	}

	return (vp);
}

struct val *
make_integer (ival)
int ival;
{
	char buf[25];

	sprintf (buf, "%d", ival);
	return (make_val (buf));
}

int
yylex ()
{
	struct val *vp;
	char *p;

	if (*av == NULL)
		return (0);

	p = *av++;

	if (strlen (p) == 1) {
		if (strchr ("|&=<>+-*/%:", *p))
			return (*p);
	} else if (strlen (p) == 2 && p[1] == '=') {
		switch (*p) {
		case '>': return (GE);
		case '<': return (LE);
		case '!': return (NE);
		}
	}

	yylval.val = make_val (p);
	return (TOKEN);
}

int
is_zero_or_null (vp)
struct val *vp;
{
	if (vp->iflag && vp->ival == 0)
		return (1);

	if (*vp->sval == 0)
		return (1);

	return (0);
}

void
main (argc, argv)
int argc;
char **argv;
{
	av = argv + 1;

	yyparse ();

	if (result->iflag)
		printf ("%d\n", result->ival);
	else
		printf ("%s\n", result->sval);

	if (is_zero_or_null (result))
		exit (1);
	else
		exit (0);
}

int
yyerror (s)
char *s;
{
	fprintf (stderr, "syntax error\n");
	exit (2);
}

void
check_integers (a, b)
struct val *a, *b;
{
	if (!a->iflag || !b->iflag) {
		fprintf (stderr, "expr: non-numeric argument\n");
		exit (2);
	}
}

struct val *
op_or (a, b)
struct val *a, *b;
{
	if (is_zero_or_null (a))
		return (b);
	else
		return (a);
}
		
struct val *
op_and (a, b)
struct val *a, *b;
{
	if (is_zero_or_null (a) || is_zero_or_null (b))
		return (make_integer (0));
	else
		return (a);
}

struct val *
op_eq (a, b)
struct val *a, *b;
{
	if (a->iflag && b->iflag)
		return (make_integer (a->ival == b->ival));
	else
		return (make_integer (strcmp (a->sval, b->sval) == 0));
}

struct val *
op_gt (a, b)
struct val *a, *b;
{
	if (a->iflag && b->iflag)
		return (make_integer (a->ival > b->ival));
	else
		return (make_integer (strcmp (a->sval, b->sval) > 0));
}

struct val *
op_lt (a, b)
struct val *a, *b;
{
	if (a->iflag && b->iflag)
		return (make_integer (a->ival < b->ival));
	else
		return (make_integer (strcmp (a->sval, b->sval) < 0));
}

struct val *
op_ge (a, b)
struct val *a, *b;
{
	if (a->iflag && b->iflag)
		return (make_integer (a->ival >= b->ival));
	else
		return (make_integer (strcmp (a->sval, b->sval) >= 0));
}

struct val *
op_le (a, b)
struct val *a, *b;
{
	if (a->iflag && b->iflag)
		return (make_integer (a->ival <= b->ival));
	else
		return (make_integer (strcmp (a->sval, b->sval) <= 0));
}

struct val *
op_ne (a, b)
struct val *a, *b;
{
	if (a->iflag && b->iflag)
		return (make_integer (a->ival != b->ival));
	else
		return (make_integer (strcmp (a->sval, b->sval) != 0));
}

struct val *
op_plus (a, b)
struct val *a, *b;
{
	check_integers (a, b);

	return (make_integer (a->ival + b->ival));
}
	
struct val *
op_minus (a, b)
struct val *a, *b;
{
	check_integers (a, b);

	return (make_integer (a->ival - b->ival));
}
	
struct val *
op_times (a, b)
struct val *a, *b;
{
	check_integers (a, b);

	return (make_integer (a->ival * b->ival));
}
	
struct val *
op_div (a, b)
struct val *a, *b;
{
	check_integers (a, b);

	return (make_integer (a->ival / b->ival));
}
	
struct val *
op_rem (a, b)
struct val *a, *b;
{
	check_integers (a, b);

	return (make_integer (a->ival % b->ival));
}
	
#include <regexp.h>

struct val *
op_colon (a, b)
struct val *a, *b;
{
	regexp *rp;
	char *newexp;
	char *p;
	char *q;

	newexp = malloc (3 * strlen (b->sval));
	p = b->sval;
	q = newexp;

	*q++ = '^';
	while (*p) {
		if (*p == '\\') {
			p++;
			if (*p == '(' || *p == ')') {
				*q++ = *p++;
			} else {
				*q++ = '\\';
				*q++ = *p++;
			}
		} else if (*p == '(' || *p == ')') {
			*q++ = '\\';
			*q++ = *p++;
		} else {
			*q++ = *p++;
		}
	}
	*q = 0;
				
	if ((rp = regcomp (newexp)) == NULL)
		yyerror ("invalid regular expression");

	if (regexec (rp, a->sval)) {
		if (rp->startp[1]) {
			rp->endp[1][0] = 0;
			return (make_val (rp->startp[1]));
		} else {
			return (make_integer (rp->endp[0] - rp->startp[0]));
		}
	} else {
		return (make_integer (0));
	}
}
