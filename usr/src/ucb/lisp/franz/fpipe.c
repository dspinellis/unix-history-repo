#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/fpipe.c,v 1.1 83/01/29 12:49:40 jkf Exp $";
#endif

/*					-[Sat Jan 29 12:44:16 1983 by jkf]-
 * 	fpipe.c				$Locker:  $
 * pipe creation
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include "global.h"
#include <signal.h>
FILE *_dofpip(iodes)
int iodes;
{
	register FILE *p;

	for(p=_iob; (p->_flag&(_IOWRT|_IOREAD))!=0; p++)
		if (p >= _iob+_NFILE)
			return(NULL);
	p->_file = iodes;
	p->_cnt = 0;
	p->_base = p->_ptr = NULL;
	return(p);
}

FILE * fpipe(info)
FILE *info[2];
{
	register FILE *p;
	int descrips[2];

	if(0 > pipe(descrips)) return( (FILE *) -1);

	if(NULL==(p = _dofpip(descrips[0]))) return( (FILE *) -1);
	p->_flag = (_IONBF|_IOREAD);
	info[0] = p;

	if(NULL==(p = _dofpip(descrips[1]))) return( (FILE *) -1);
	p->_flag = _IOWRT;
	info[1] = p;
	return((FILE *) 2); /*indicate sucess*/
}
