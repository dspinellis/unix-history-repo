#ifndef lint
static char sccsid[] = "@(#)tt.c	4.2 8/11/83";
#endif

 /* tt.c: subroutines for drawing horizontal lines */
# include "t..c"
ctype(il, ic)
{
if (instead[il])
	return(0);
if (fullbot[il])
	return(0);
il = stynum[il];
return(style[il][ic]);
}
min(a,b)
{
return(a<b ? a : b);
}
fspan(i,c)
{
c++;
return(c<ncol && ctype(i,c)=='s');
}
lspan(i,c)
{
int k;
if (ctype(i,c) != 's') return(0);
c++;
if (c < ncol && ctype(i,c)== 's') 
	return(0);
for(k=0; ctype(i,--c) == 's'; k++);
return(k);
}
ctspan(i,c)
{
int k;
c++;
for(k=1; c<ncol && ctype(i,c)=='s'; k++)
	c++;
return(k);
}
tohcol(ic)
{
			if (ic==0)
				fprintf(tabout, "\\h'|0'");
			else
				fprintf(tabout, "\\h'(|\\n(%du+|\\n(%du)/2u'", ic+CLEFT, ic+CRIGHT-1);
}
allh(i)
{
/* return true if every element in line i is horizontal */
/* also at least one must be horizontl */
int c, one, k;
if (fullbot[i]) return(1);
for(one=c=0; c<ncol; c++)
	{
	k = thish(i,c);
	if (k==0) return(0);
	if (k==1) continue;
	one=1;
	}
return(one);
}
thish(i,c)
{
	int t;
	char *s;
	struct colstr *pc;
	if (c<0)return(0);
	if (i<0) return(0);
	t = ctype(i,c);
	if (t=='_' || t == '-')
		return('-');
	if (t=='=')return('=');
	if (t=='^') return(1);
	if (fullbot[i] )
		return(fullbot[i]);
	if (t=='s') return(thish(i,c-1));
	if (t==0) return(1);
	pc = &table[i][c];
	s = (t=='a' ? pc->rcol : pc->col);
	if (s==0 || (point(s) && *s==0))
		return(1);
	if (vspen(s)) return(1);
	if (t=barent( s))
		return(t);
	return(0);
}
