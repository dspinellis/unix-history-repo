/*  $Revision: 1.3 $
**
**  Get a config value from INN.
*/
#include "configdata.h"
#include <stdio.h>
#include <sys/types.h>
#include "libinn.h"
#include "clibrary.h"
#include "macros.h"


int
main(ac, av)
    int			ac;
    char		*av[];
{
    register char	*p;
    register char	*val;
    register BOOL	File;
    register int	i;

    /* Parse JCL. */
    File = FALSE;
    while ((i = getopt(ac, av, "f")) != EOF)
	switch (i) {
	default:
	    (void)fprintf(stderr, "Usage error.\n");
	    exit(1);
	    /* NOTREACHED */
	case 'f':
	    File = TRUE;
	    break;
	}
    ac -= optind;
    av += optind;

    /* Loop over parameters, each a config value. */
    while ((p = *av++) != NULL) {
	val = File ? GetFileConfigValue(p) : GetConfigValue(p);
	if (val == NULL)
	    (void)fprintf(stderr, "No value for %s parameter\n", p);
	else
	    (void)printf("%s\n", val);
    }

    exit(0);
    /* NOTREACHED */
}
