#include "fio.h"
f_clos(a) cllist *a;
{	unit *b;
	if(a->cunit >= MXUNIT) return(0);
	b= &units[a->cunit];
	if(b->ufd==NULL) return(0);
	b->uend=0;
	if(a->csta!=0)
		switch(*a->csta)
		{
		default:
		keep:
		case 'k':
			if(b->uwrt) t_runc(b);
			if(fclose(b->ufd) != 0) abort();
			if(b->ufnm!=0) free(b->ufnm);
			b->ufnm=NULL;
			b->ufd=NULL;
			return(0);
		case 'd':
		delete:
			fclose(b->ufd);
			if(b->ufnm!=0)
			{	unlink(b->ufnm); /*SYSDEP*/
				free(b->ufnm);
			}
			b->ufnm=NULL;
			b->ufd=NULL;
			return(0);
		}
	else if(b->uscrtch==1) goto delete;
	else goto keep;
}
f_exit()
{	int i;
	cllist xx;
	xx.cerr=1;
	xx.csta=NULL;
	for(i=0;i<MXUNIT;i++)
	{
		xx.cunit=i;
		f_clos(&xx);
	}
}
flush_()
{	int i;
	for(i=0;i<MXUNIT;i++)
		if(units[i].ufd != NULL) fflush(units[i].ufd);
}
