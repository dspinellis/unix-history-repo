#include "awk.def"
#include "stdio.h"
#include "awk.h"
#define printit(n) printf(printname[n-FIRSTTOKEN]);
extern char *printname[];
extern cell fldtab[];

dump(a,b) node *a, *b;
{
#ifdef	DEBUG
	node *x;
	if(a==nullstat) return;
	for(x=a;x!=NULL;x=x->nnext)
	{
	if(x==b) printf(" *** ");
		if(x->ntype==NVALUE)
		{	switch(x->subtype)
			{
			default: runerror();
			case CCON: case CVAR:
				printf("%s",x->nobj->nval);
				continue;
			case CFLD:
				if(x->nobj->nval==0) printf("$%d",x->nobj - fldtab);
				else printf("$0");
				continue;
			}
		}
		else if(x->ntype==PASTAT2)
		{	pa2dump(x,b);
			continue;
		}
		switch(x->nobj)
		{
		default: runerror();
		case LE: case LT: case EQ:
		case NE: case GT: case GE:
		case MATCH: case NOTMATCH:
		case ADD: case MINUS: case MULT:
		case DIVIDE: case MOD: case ASGNOP:
		case BOR: case AND: case CAT:
			dump(x->narg[0], b);
			printit(x->nobj);
			if(x->nobj!=MATCH && x->nobj!=NOTMATCH)
				dump(x->narg[1]);
			else	printf("regex");
			break;
		case UMINUS: case FNCN: case INCR:
		case DECR: case INDIRECT:
			printit(x->nobj);
			dump(x->narg[0], b);
			break;
		case PRINT: case PRINTF: case SPRINTF:
		case SPLIT:
			printit(x->nobj);
			dump(x->narg[0], b);
			if(x->nobj==SPLIT || x->nobj==SPRINTF) break;
			if(x->narg[1]==0) break;
			printit((int)x->narg[1]);
			dump(x->narg[2], b);
			break;
		case IF: case WHILE:
			printit(x->nobj);
			dump(x->narg[0], b);
			printf(") ");
			dump(x->narg[1], b);
			if(x->narg[2]==NULL) break;
			printit(ELSE);
			dump(x->narg[2], b);
			break;
		case FOR:
			printit(x->nobj);
			dump(x->narg[0], b);
			putchar(';');
			dump(x->narg[1], b);
			putchar(';');
			dump(x->narg[2], b);
			printf(") ");
			dump(x->narg[3], b);
			break;
		case NEXT: case EXIT: case BREAK: case CONTINUE:
			printit(x->nobj);
			break;
		case PROGRAM:
			if(x->narg[0]!=NULL)
			{	printf("BEGIN {");
				dump(x->narg[0], b);
				printf("}\n");
			}
			dump(x->narg[1], b);
			if(x->narg[2]!=NULL)
			{	printf("END {");
				dump(x->narg[2], b);
				printf("}\n");
			}
			break;
		case PASTAT:
			dump(x->narg[0], b);
			printf("{");
			dump(x->narg[1], b);
			printf("}\n");
			break;
		case ARRAY:
			printf(x->nval);
			printf("[");
			dump(x->narg[1], b);
			printf("]");
			break;
		case SUBSTR:
			printit(x->nobj);
			dump(x->narg[0], b);
			putchar(',');
			dump(x->narg[1], b);
			if(x->narg[2]!=NULL)
			{	putchar(',');
				dump(x->narg[2], b);
			}
			putchar(')');
			break;
		}
		if(x->ntype == NSTAT) putchar('\n');
	}
#endif
}
pa2dump(a, b) node *a, *b;
{
#ifdef	DEBUG
	dump(a->narg[0], b);
	printf(", ");
	dump(a->narg[1], b);
	printf(" {");
	dump(a->narg[2], b);
	printf("}\n");
#endif
}
