#include <stdio.h>
#include "connect.h"

struct connectdomain cdbuf;
main (argc, argv) char *argv[] ; {
	int fd, rv ;
	char optbuf[1024] ;
	int optlen ;

	strcpy(cdbuf.cd_address,"6428831") ;
	cdbuf.cd_alen = strlen (cdbuf.cd_address) +1 ;
	strcpy (optbuf,"OPTIONS") ;
	optlen = 7 ;

	fd = externalconnect (&cdbuf, optbuf, optlen, 2) ;

	printf("fd %d ", fd) ; 
	/*printf("rv %d\n", write (fd, "foo\n", 4) );*/
	rv = externalfinish (fd) ;
	printf("rv finish %d\n", rv );
	return (0);
}
