# include	<symbol.h>
# include	<ingres.h>
# include	<tree.h>
# include	<aux.h>
# include	<sccs.h>

SCCSID(@(#)pr_tree.c	7.1	2/5/81)

/*
**  PR_TREE.C -- Query tree printing routines
**
**	Trace Flags:
**		52
*/




char	*pr_trim();
char	*resultres();

struct tab
{
	char	t_opcode;
	char	*t_string;
};


struct tab	Uop_tab[] =
{
	opPLUS,		"+ ",
	opMINUS,	"- ",
	opNOT,		"not[ERROR]",
	opATAN,		"atan",
	opCOS,		"cos",
	opGAMMA,	"gamma",
	opLOG,		"log",
	opASCII,	"ascii",
	opSIN,		"sin",
	opSQRT,		"sqrt",
	opABS,		"abs",
	opEXP,		"exp",
	opINT1,		"int1",
	opINT2,		"int2",
	opINT4,		"int4",
	opFLOAT4,	"float4",
	opFLOAT8,	"float8",
};
struct tab	Bop_tab[] =
{
	opADD,		"+",
	opSUB,		"-",
	opMUL,		"*",
	opDIV,		"/",
	opPOW,		"**",
	opEQ,		"=",
	opNE,		"!=",
	opLT,		"<",
	opLE,		"<=",
	opGT,		">",
	opGE,		">=",
	opMOD,		"%",
};

struct tab	Cop_tab[] =
{
	opDBA,		"dba",
	opUSERCODE,	"usercode",
	opDATE,		"date",
	opTIME,		"time",
};

struct tab	Aop_tab[] =
{
	opCOUNT,	"count",
	opCOUNTU,	"countu",
	opSUM,		"sum",
	opSUMU,		"sumu",
	opAVG,		"avg",
	opAVGU,		"avgu",
	opMIN,		"min",
	opMAX,		"max",
	opANY,		"any",
};


DESC		Attdes;
int		Tl_elm;
int		Dom_num;
char		*Resrel;
/* 
**  PR_TREE
**
**	tree:	tl_clause ROOT tl_clause
**
**	prints out a tree assuming a mdVIEW-like mode
**
*/

pr_tree(root)
QTREE	*root;
{

#	ifdef xZTR1
	if (tTf(52, -1))
		printf("pr_tree: root %u Resultvar %d Resrel %s\n",
			root, Qt.qt_resvar, Resrel);
#	endif

	printf("%s ", pr_trim(resultres(), MAXNAME));

	pr_dom_init();
	Tl_elm = 0;

	/* print target list */
	printf("(\n");
	pr_tl_clause(root->left, TRUE);
	putchar(')');

	/* print qualification */
	if (root->right->sym.type != QLEND)
	{
                printf("\nwhere ");
		pr_qual(root->right);
	}
	putchar('\n');
}
/*
**  PR_TL_CLAUSE
**
**	tl_clause:	TREE
**		|	tl_clause RESDOM expr
**	
** target_flag = "in a target list (as opposed to in a by list)"
*/

pr_tl_clause(t_l, target_flag)
QTREE		*t_l;
register bool	target_flag;
{

#	ifdef xZTR1
	if (tTf(52, 1))
		printf("tl_clause target %d Tl_elm %d\n", target_flag, Tl_elm);
#	endif

	if (t_l->sym.type != TREE)
	{
		pr_tl_clause(t_l->left, target_flag);
		if (Tl_elm)
		{
			printf(", ");
			if (target_flag)
				putchar('\n');
		}
		/* print out info on result variable */
		pr_resdom(t_l, target_flag);
		pr_expr(t_l->right);
		Tl_elm++;
	}
}
/*
**  PR_RESDOM
**
**	print out info on a result attribute.
**	this will be done only if the RESDOM node
**	is inside a target_list and if the Qt.qt_resvar >= 0.
**	Qt.qt_resvar == -1 inside a target list indicates that this is
**	a retrieve to terminal.
*/

pr_resdom(resdom, target_flag)
QTREE	*resdom;
int	target_flag;
{

#	ifdef xZTR1
	if (tTf(52, 2))
		printf("pr_resdom: target_flag %d\n", target_flag);
#	endif

	if (target_flag)
	{
		printf("\t");
		pr_attname(resultres(), resdom->sym.value.sym_resdom.resno);
		printf(" = ");
	}
}
/* 
**  PR_ATTNAME
**
**	give a relation name, and the attribute number of that
**	relation, looks in the attribute relation for the name of the
**	attribute.
*/

pr_attname(rel, attno)
char	*rel;
int	attno;
{
	TID			tid;
	struct attribute	key, tuple;
	register		i;

#	ifdef xZTR1
	if (tTf(52, 3))
		printf("pr_attname: rel %s attno %d\n",
		rel, attno);
#	endif

	if (attno == 0)
	{
		printf("tid");
		return;
	}
	opencatalog("attribute", 0);
	clearkeys(&Attdes);
	setkey(&Attdes, &key, rel, ATTRELID);
	setkey(&Attdes, &key, &attno, ATTID);
	i = getequal(&Attdes, &key, &tuple, &tid);
	if (i)
		syserr("pr_attname: bad getequal %d rel %s attno %d",
		i, rel, attno);
	printf("%s", pr_trim(tuple.attname, MAXNAME));
}
/*
**  PR_EXPR
**
**	expr:		VAR
**		|	expr BOP expr
**		|	expr UOP
**		|	AOP AGHEAD qual
**			  \
**			   expr
**		|	BYHEAD	  AGHEAD qual
**		        /    \
**		tl_clause     AOP
**				\
**				 expr
**		|	INT
**		|	FLOAT
**		| 	CHAR
*/

pr_expr(e)
register QTREE	*e;
{
	register int	op;
	register int	tl_elm;

	switch (e->sym.type)
	{
	  case VAR:
		pr_var(e);
		break;

	  case BOP:
		if (e->sym.value.sym_op.opno == opCONCAT)
		{
			printf("concat(");
			pr_expr(e->left);
			printf(", ");
			pr_expr(e->right);
			putchar(')');
		}
		else
		{
			putchar('(');
			pr_expr(e->left);
			pr_op(BOP, e->sym.value.sym_op.opno);
			pr_expr(e->right);
			putchar(')');
		}
		break;

	  case UOP:
		if ((op = e->sym.value.sym_op.opno) == opMINUS || op == opPLUS || op == opNOT)
		{
			pr_op(UOP, e->sym.value.sym_op.opno);
			pr_expr(e->left);
			putchar(')');
		}
		else
		{
			/* functional operators */
			pr_op(UOP, e->sym.value.sym_op.opno);
			pr_expr(e->left);
			putchar(')');
		}
		break;

	  case AGHEAD:
		if (e->left->sym.type == AOP)
		{
			/* simple aggregate */
			pr_op(AOP, e->left->sym.value.sym_op.opno);
			pr_expr(e->left->right);
			if (e->right->sym.type != QLEND)
			{
				printf("\where ");
				pr_qual(e->right);
			}
			putchar(')');
		}
		else
		{
			/* aggregate function */
			pr_op(AOP, e->left->right->sym.value.sym_op.opno);
			pr_expr(e->left->right->right);
			printf(" by ");
			/* avoid counting target list elements
			 * in determining wether to put out
			 * commas after list's elements
			 */
			tl_elm = Tl_elm;
			Tl_elm = 0;
			pr_tl_clause(e->left->left, FALSE);
			Tl_elm = tl_elm;
			if (e->right->sym.type != QLEND)
			{
				printf("\n\t\twhere ");
				pr_qual(e->right);
			}
			putchar(')');
		}
		break;
	  
	  case INT:
	  case FLOAT:
	  case CHAR:
		pr_const(e);
		break;

	  default:
		syserr("expr %d", e->sym.type);
	}
}
/*
**  PR_CONST -- print constant
*/

pr_const(c)
register QTREE	*c;
{
	register char	*cp;
	register int	i;
	char		ch;
	double		d;

	switch (c->sym.type)
	{
	  case INT:
		if (c->sym.len == 1)
			printf("%d", c->sym.value.sym_data.i1type);
		else if (c->sym.len == 2)
			printf("%d", c->sym.value.sym_data.i2type);
		else	/* i4 */
			printf("%D", c->sym.value.sym_data.i4type);
		break;

	  case FLOAT:
		if (c->sym.len == 4)
			d = c->sym.value.sym_data.f4type;
		else
			d = c->sym.value.sym_data.f8type;
		printf("%-10.3f", c->sym.value.sym_data.f8type);
		break;

	  case CHAR:
		printf("\"");
		cp = c->sym.value.sym_data.c0type;
		for (i = c->sym.len; i--; cp++)
		{
			if (any(*cp, "\"\\[]*?") == TRUE)
				putchar('\\');

			if (*cp >= ' ')
			{
				putchar(*cp);
				continue;
			}
			/* perform pattern matching character replacement */
			switch (*cp)
			{
			  case PAT_ANY:
				ch = '*';
				break;
			
			  case PAT_ONE:
				ch = '?';
				break;
			
			  case PAT_LBRAC:
				ch = '[';
				break;

			  case PAT_RBRAC:
				ch = ']';
				break;

			  default:
				ch = *cp;
			}
			putchar(ch);
		}
		putchar('"');
		break;

	  default:
		syserr("bad constant %d", c->sym.type);
	}
}
/*
**  PR_OP -- print out operator of a certain type
*/

pr_op(op_type, op_code)
int		op_type;
register int	op_code;
{
	register struct tab	*s;

	switch (op_type)
	{
	  case UOP:
		s = &Uop_tab[op_code];
		printf("%s(", s->t_string);
		break;

	  case BOP:
		s = &Bop_tab[op_code];
		printf(" %s ", s->t_string);
		break;

	  case AOP:
		s = &Aop_tab[op_code];
		printf("%s(", s->t_string);
		break;

	  case COP:
		s = &Cop_tab[op_code];
		printf("%s", s->t_string);
		break;
	}
	if (op_code != s->t_opcode)
		syserr("pr_op: op in wrong place type %d, code %d", op_type,
		s->t_opcode);
}
/*
**  PR_VAR
**
**	print a VAR node: that is, a var.attno pair
**	at present the var part is the relation name over which var
**	ranges.
*/

pr_var(var)
QTREE	*var;
{

#	ifdef xZTR1
	if (tTf(52, 4))
		printf("pr_var(var=%d)\n", var);
#	endif
	pr_rv(var->sym.value.sym_var.varno);
	putchar('.');
	pr_attname(Qt.qt_rangev[var->sym.value.sym_var.varno].rngvdesc->reldum.relid, var->sym.value.sym_var.attno);
}
/*
**  PR_QUAL
**
**	qual:		QLEND
**		|	q_clause AND qual
**
*/

pr_qual(q)
register QTREE	*q;
{

	pr_q_clause(q->left);
	if (q->right->sym.type != QLEND)
	{
		printf(" and ");
		pr_qual(q->right);
	}
}
/*
**  PR_Q_CLAUSE
**
**	q_clause:	q_clause OR q_clause
**		|	expr
*/

pr_q_clause(q)
QTREE	*q;
{

	if (q->sym.type == OR)
	{
		pr_q_clause(q->left);
		printf(" or ");
		pr_q_clause(q->right);
	}
	else
		pr_expr(q);
}
/*
**  PR_TRIM
*/

char *
pr_trim(s, l)
register char	*s;
register int	l;
{
	static char	buf[30];

	bmove(s, buf, l);
	for (s = buf; l && *s != ' ' && *s; --l)
		s++;
	*s = '\0';
	return (buf);
}
/*
**  PR_DOM_INIT
*/

pr_dom_init()
{
	Dom_num = 0;
}

pr_ddom()
{
	printf("d%d = ", Dom_num++);
}
/*
**  PR_RANGE -- print the range table
*/

pr_range()
{
	register int	i;

	for (i = 0; i <= MAXVAR; i++)
	{
		if (Qt.qt_rangev[i].rngvdesc != NULL)
		{
			printf("range of ");
			pr_rv(i);
			printf(" is %s\n", 
			  pr_trim(Qt.qt_rangev[i].rngvdesc->reldum.relid, MAXNAME));
		}
	}
}
/*
**  PR_RV -- print range variable
*/

pr_rv(re)
register int	re;
{
	register int	j;
	register char	ch;

	ch = Qt.qt_rangev[re].rngvdesc->reldum.relid[0];

#	ifdef xZTR1
	if (tTf(52, 6))
		printf("pr_rv(%d) ch '%c'\n", re, ch);
#	endif
	
	for (j = 0; j <= MAXVAR; j++)
	{
		if (Qt.qt_rangev[j].rngvdesc == NULL)
			continue;
		if (ch == Qt.qt_rangev[j].rngvdesc->reldum.relid[0])
			break;
	}
	if (j < re)
		printf("rv%d", re);
	else
		printf("%c", ch);
}
/*
**  RESULTRES
*/

char *
resultres()
{
	extern char	*Resrel;

#	ifdef xZTR1
	if (tTf(52, 5))
		printf("resultres: Resultvar %d, Resrel %s\n", Qt.qt_resvar, Resrel);
#	endif
	if (Qt.qt_resvar > 0)
		return (Qt.qt_rangev[Qt.qt_resvar].rngvdesc->reldum.relid);
	if (Resrel == 0)
		syserr("resultres: Resrel == 0");

	return (Resrel);
}

any(c, s)
register char	c;
register char	*s;
{

	while (*s != '\0')
		if (*s++ == c)
			return (TRUE);
	return (FALSE);
}
