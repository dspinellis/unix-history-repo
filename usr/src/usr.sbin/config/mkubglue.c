/*
 * Make the uba interrupt file ubglue.s
 */
#include <stdio.h>
#include "config.h"
#include "y.tab.h"

ubglue()
{
    register FILE *fp;
    register struct device *dp, *mp;

    fp = fopen(path(ubglue.c), "w");
    for (dp = dtab ; dp != NULL; dp = dp->d_next)
    {
	mp = dp->d_conn;
	if (mp != NULL && mp != -1 && !eq(mp->d_name, "mba"))
	{
	    if (dp->d_vec1 != NULL)
		dump_vec(fp, dp->d_vec1, dp->d_unit);
	    if (dp->d_vec2 != NULL)
		dump_vec(fp, dp->d_vec2, dp->d_unit);
	}
    }
    fclose(fp);
}

/*
 * dump_vec
 *	Print an interrupt vector
 */

dump_vec(fp, vector, number)
register FILE *fp;
char *vector;
int number;
{
    char nbuf[80];
    register char *v = nbuf;

    sprintf(v, "%s%d", vector, number);
    fprintf(fp, "\t.globl\t_X%s\n\t.align\t2\n_X%s:\n\tpushr\t$0x3f\n", v, v);
    if (strncmp(vector, "dzx", 3) == 0)
	fprintf(fp, "\tmovl\t$%d,r0\n\tjbr\t_dzdma\n\n", number);
    else
    {
	fprintf(fp, "\tpushl\t$%d\n", number);
	fprintf(fp, "\tcalls\t$1,_%s\n\tpopr\t$0x3f\n\trei\n\n", vector);
    }
}
