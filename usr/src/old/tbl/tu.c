/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tu.c	4.4 (Berkeley) %G%";
#endif /* not lint */

 /* tu.c: draws horizontal lines */
# include "t..c"
makeline(i,c,lintype)
{
int cr, type, shortl;
type = thish(i,c);
if (type==0) return;
cr=c;
shortl = (table[i][c].col[0]=='\\');
if (c>0 && !shortl && thish(i,c-1) == type)return;
if (shortl==0)
	for(cr=c; cr < ncol && (ctype(i,cr)=='s'||type==thish(i,cr)); cr++);
else
	for(cr=c+1; cr<ncol && ctype(i,cr)=='s'; cr++);
drawline(i, c, cr-1, lintype, 0, shortl);
}
fullwide(i, lintype)
{
int cr, cl;
if (!pr1403)
	fprintf(tabout, ".nr %d \\n(.v\n.vs \\n(.vu-\\n(.sp\n", SVS);
cr= 0;
while (cr<ncol)
	{
	cl=cr;
	while (i>0 && vspand(prev(i),cl,1))
		cl++;
	for(cr=cl; cr<ncol; cr++)
		if (i>0 && vspand(prev(i),cr,1))
			break;
	if (cl<ncol)
	drawline(i,cl,(cr<ncol?cr-1:cr),lintype,1,0);
	}
fprintf(tabout, "\n");
if (!pr1403)
	fprintf(tabout, ".vs \\n(%du\n", SVS);
}

drawline(i, cl, cr, lintype, noheight, shortl)
{
	char *exhr, *exhl, *lnch;
	int lcount, ln, linpos, oldpos, nodata;
lcount=0;
exhr=exhl= "";
switch(lintype)
	{
	case '-': lcount=1;break;
	case '=': lcount = pr1403? 1 : 2; break;
	case SHORTLINE: lcount=1; break;
	}
if (lcount<=0) return;
nodata = cr-cl>=ncol || noheight || allh(i);
	if (!nodata)
		fprintf(tabout, "\\v'-.5m'");
for(ln=oldpos=0; ln<lcount; ln++)
	{
	linpos = 2*ln - lcount +1;
	if (linpos != oldpos)
		fprintf(tabout, "\\v'%dp'", linpos-oldpos);
	oldpos=linpos;
	if (shortl==0)
	{
	tohcol(cl);
	if (lcount>1)
		{
		switch(interv(i,cl))
			{
			case TOP: exhl = ln==0 ? "1p" : "-1p"; break;
			case BOT: exhl = ln==1 ? "1p" : "-1p"; break;
			case THRU: exhl = "1p"; break;
			}
		if (exhl[0])
		fprintf(tabout, "\\h'%s'", exhl);
		}
	else if (lcount==1)
		{
		switch(interv(i,cl))
			{
			case TOP: case BOT: exhl = "-1p"; break;
			case THRU: exhl = "1p"; break;
			}
		if (exhl[0])
		fprintf(tabout, "\\h'%s'", exhl);
		}
	if (lcount>1)
		{
		switch(interv(i,cr+1))
			{
			case TOP: exhr = ln==0 ? "-1p" : "+1p"; break;
			case BOT: exhr = ln==1 ? "-1p" : "+1p"; break;
			case THRU: exhr = "-1p"; break;
			}
		}
	else if (lcount==1)
		{
		switch(interv(i,cr+1))
			{
			case TOP: case BOT: exhr = "+1p"; break;
			case THRU: exhr = "-1p"; break;
			}
		}
	}
	else
		fprintf(tabout, "\\h'|\\n(%du'", cl+CLEFT);
	fprintf(tabout, "\\s\\n(%d",LSIZE);
	if (linsize)
		fprintf(tabout, "\\v'-\\n(%dp/6u'", LSIZE);
	if (shortl)
		fprintf(tabout, "\\l'|\\n(%du'", cr+CRIGHT);
	else
	{
	lnch = "\\(ul";
	if (pr1403)
		lnch = lintype==2 ? "=" : "\\(ru";
	if (cr+1>=ncol)
		fprintf(tabout, "\\l'|\\n(TWu%s%s'", exhr,lnch);
	else
		fprintf(tabout, "\\l'(|\\n(%du+|\\n(%du)/2u%s%s'", cr+CRIGHT,
			cr+1+CLEFT, exhr, lnch);
	}
	if (linsize)
		fprintf(tabout, "\\v'\\n(%dp/6u'", LSIZE);
	fprintf(tabout, "\\s0");
	}
if (oldpos!=0)
	fprintf(tabout, "\\v'%dp'", -oldpos);
if (!nodata)
	fprintf(tabout, "\\v'+.5m'");
}
getstop()
{
int i,c,k,junk, stopp;
stopp=1;
for(i=0; i<MAXLIN; i++)
	linestop[i]=0;
for(i=0; i<nlin; i++)
	for(c=0; c<ncol; c++)
		{
		k = left(i,c,&junk);
		if (k>=0 && linestop[k]==0)
			linestop[k]= ++stopp;
		}
if (boxflg || allflg || dboxflg)
	linestop[0]=1;
}
left(i,c, lwidp)
	int *lwidp;
{
int kind, li, lj;
	/* returns -1 if no line to left */
	/* returns number of line where it starts */
	/* stores into lwid the kind of line */
*lwidp=0;
kind = lefdata(i,c);
if (kind==0) return(-1);
if (i+1<nlin)
if (lefdata(next(i),c)== kind) return(-1);
while (i>=0 && lefdata(i,c)==kind)
	i=prev(li=i);
if (prev(li)== -1) li=0;
*lwidp=kind;
for(lj= i+1; lj<li; lj++)
	if (instead[lj] && strcmp(instead[lj], ".TH")==0)
		return(li);
for(i= i+1; i<li; i++)
	if (fullbot[i])
		li=i;
return(li);
}
lefdata(i,c)
{
int ck;
if (i>=nlin) i=nlin-1;
if (ctype(i,c) == 's')
	{
	for(ck=c; ctype(i,ck)=='s'; ck--);
	if (thish(i,ck)==0)
		return(0);
	}
i =stynum[i];
i = lefline[i][c];
if (i>0) return(i);
if (dboxflg && c==0) return(2);
if (allflg)return(1);
if (boxflg && c==0) return(1);
return(0);
}
next(i)
{
while (i+1 <nlin)
	{
	i++;
	if (!fullbot[i] && !instead[i]) break;
	}
return(i);
}
prev(i)
{
while (--i >=0  && (fullbot[i] || instead[i]))
	;
return(i);
}
