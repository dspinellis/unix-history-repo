/*
 * inquire.c - f77 i/o inquire statement routine
 */

#include "fio.h"

f_inqu(a) inlist *a;
{	char *byfile;
	int i;
	unit *p;
	char buf[256], *s;
	long x_inode;

	elist = NO;
	lfname = a->infile;
	lunit = a->inunit;
	external = YES;
	p = NULL;
	if(byfile=a->infile)
	{
		g_char(a->infile,a->infilen,buf);
		if((x_inode=inode(buf))==-1)
		{	if(a->inex) *a->inex = NO;  /* doesn't exist */
			return(OK);
		}
		for(i=0;i<MXUNIT;i++)
			if(units[i].ufd && (units[i].uinode==x_inode))
			{
				p = &units[i];
				break;
			}
	}
	else
	{
		if (not_legal(lunit)) err(a->inerr,101,"inquire")
		else
			if (units[lunit].ufd)
			{	p= &units[lunit];
				lfname = p->ufnm;
			}
	}
	if(a->inex) *a->inex= ((byfile && x_inode) || (!byfile && p));
	if(a->inopen) *a->inopen=(p!=NULL);
	if(a->innum) *a->innum= (p?(p-units):-1);
	if(a->innamed) *a->innamed= (byfile || (p && p->ufnm));
	if(a->inname)
	{
		if(byfile) s = buf;
		else if(p && p->ufnm) s = p->ufnm;
		else s="";
		b_char(s,a->inname,a->innamlen);
	}
	if(a->inacc && p)
	{
		if(p->url) s = "direct";
		else	s = "sequential";
		b_char(s,a->inacc,a->inacclen);
	}
	if(a->inseq)
	{
		s= ((byfile && !p) || (p && !p->url))? "yes" : "no";
		b_char(s,a->inseq,a->inseqlen);
	}
	if(a->indir)
	{
		s= ((byfile && !p) || (p && p->useek && p->url))? "yes" : "no";
		b_char(s,a->indir,a->indirlen);
	}
	if(a->inform)
	{	if(p)
		{
#ifndef KOSHER
			if(p->uprnt) s = "print"; /*** NOT STANDARD FORTRAN ***/
			else
#endif
				s = p->ufmt?"formatted":"unformatted";
		}
		else s = "unknown";
		b_char(s,a->inform,a->informlen);
	}
	if(a->infmt)
	{
		if (p) s= p->ufmt? "yes" : "no";
		else s= "unknown";
		b_char(s,a->infmt,a->infmtlen);
	}
	if(a->inunf)
	{
		if (p) s= p->ufmt? "no" : "yes";
		else s= "unknown";
		b_char(s,a->inunf,a->inunflen);
	}
	if(a->inrecl && p) *a->inrecl=p->url;
	if(a->innrec && p && p->url)
		*a->innrec=(ftell(p->ufd)/p->url)+1;
	if(a->inblank && p && p->ufmt)
	{
		b_char(p->ublnk? "zero" : "blank",a->inblank,a->inblanklen);
	}
	return(OK);
}
