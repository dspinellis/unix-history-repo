/*  $Revision: 1.1 $
**
**  Encapsulate dirent/direct differences.
*/

#if	defined(DIR_DIRENT)
#include <dirent.h>
typedef struct dirent	DIRENTRY;
#endif	/* defined(DIR_DIRENT) */

#if	defined(DIR_DIRECT)
#include <sys/dir.h>
typedef struct direct	DIRENTRY;
#endif	/* defined(DIR_DIRECT) */
