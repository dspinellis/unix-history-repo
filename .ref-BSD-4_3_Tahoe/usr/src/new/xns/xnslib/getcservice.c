/*	$Header: getcservice.c,v 2.0 85/11/21 07:22:09 jqj Exp $	*/

#include "courierdb.h"

struct courierdbent *
getcourierservice(prognum,vernum)
	register long unsigned prognum;
	register short unsigned vernum;
{
	register struct courierdbent *p;

	setcourierdbent(0);
	while (p = getcourierdbent()) {
		if (p->cr_programnumber == prognum && p->cr_version == vernum)
			break;
	}
	endcourierdbent();
	return (p);
}
