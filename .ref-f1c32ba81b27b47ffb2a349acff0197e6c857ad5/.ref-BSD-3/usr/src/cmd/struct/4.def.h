#define YESTAB	TRUE
#define NOTAB	FALSE
#define TABOVER(n)	tabover(n,outfd)
#define OUTSTR(x)		fprintf(outfd,"%s",x)
#define OUTNUM(x)		fprintf(outfd,"%d",x)


extern LOGICAL *brace;
#define YESBRACE(v,i)	{ if (DEFINED(LCHILD(v,i))) brace[LCHILD(v,i)] = TRUE; }
#define NOBRACE(v,i)	{ if (DEFINED(LCHILD(v,i))) brace[LCHILD(v,i)] = FALSE; }
#define HASBRACE(v,i)	 ((DEFINED(LCHILD(v,i))) ? brace[LCHILD(v,i)] : TRUE)
