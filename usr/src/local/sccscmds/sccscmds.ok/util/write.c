# include "errno.h"
# include "../hdr/macros.h"
SCCSID(@(#)write	2.1);

/*
	Interface to write(II) (called syswrite) which handles
	all error conditions.
	Returns number of bytes written on success,
	returns fatal(<mesg>) on failure.
*/

write(fildes,buffer,nbytes)
char *buffer;
{
	register int n;

	if (nbytes>0 && (n=syswrite(fildes,buffer,nbytes))!=nbytes)
		n = xmsg("","write");
	return(n);
}
