#include "defs"


warn1(s,t)
char *s, *t;
{
char buff[100];
warn( sprintf(buff, s, t) );
}


warn(s)
char *s;
{
if(nowarnflag)
	return;
fprintf(diagfile, "Warning on line %d of %s: %s\n", lineno, infname, s);
++nwarn;
}



err2(s,t,u)
char *s, *t, *u;
{
char buff[100];
err( sprintf(buff, s, t, u) );
}


err1(s,t)
char *s, *t;
{
char buff[100];
err( sprintf(buff, s, t) );
}


err(s)
char *s;
{
fprintf(diagfile, "Error on line %d of %s: %s\n", lineno, infname, s);
++nerr;
}


yyerror(s)
char *s;
{ err(s); }



dclerr(s, v)
char *s;
struct nameblock *v;
{
char buff[100];

if(v)
	err( sprintf(buff, "Declaration error for %s: %s", varstr(VL, v->varname), s) );
else
	err1("Declaration error %s", s);
}



execerr(s, n)
char *s, *n;
{
char buf1[100], buf2[100];

sprintf(buf1, "Execution error %s", s);
err( sprintf(buf2, buf1, n) );
}


fatal(t)
char *t;
{
fprintf(diagfile, "Compiler error line %d of %s: %s\n", lineno, infname, t);
if(debugflag)
	abort();
done(3);
exit(3);
}




fatal1(t,d)
char *t, *d;
{
char buff[100];
fatal( sprintf(buff, t, d) );
}
