#include <utmp.h>

static	char	UTMP[]	= "/etc/utmp";
static	struct	utmp ubuf;

char *
getlogin()
{
	register me, uf;
	register char *cp;

	if( !(me = ttyslot()) )
		return(0);
	if( (uf = open( UTMP, 0 )) < 0 )
		return(0);
	lseek( uf, (long)(me*sizeof(ubuf)), 0 );
	if (read(uf, (char *)&ubuf, sizeof(ubuf)) != sizeof(ubuf))
		return(0);
	close(uf);
	ubuf.ut_name[8] = ' ';
	for (cp=ubuf.ut_name; *cp++!=' ';)
		;
	*--cp = '\0';
	return( ubuf.ut_name );
}
