#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>

/*******
 *	anyread		check if anybody can read
 *	return 0 ok: FAIL not ok
 */

anyread(file)
char *file;
{
	struct stat s;

	if (stat(file, &s) != 0)
		return(FAIL);
	if (!(s.st_mode & ANYREAD))
		return(FAIL);
	return(0);
}
