# include	"iodec.h"

/**
 **	flush and close all files and exit
 **/

cexit(rcode)
{
	flush();
	__exit(rcode);
}


exit(rcode)
{
	cexit(rcode);
}
