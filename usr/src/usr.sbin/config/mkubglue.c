/*
 * Make the uba interrupt file ubglue.s
 *	mkubglue.c	1.5	82/06/16
 */
#include <stdio.h>
#include "config.h"
#include "y.tab.h"

ubglue()
{
    register FILE *fp;
    register struct device *dp, *mp;

    fp = fopen(path("ubglue.s"), "w");
    if (fp == NULL) {
	perror(path("ubglue.s"));
	exit(1);
    }
    for (dp = dtab ; dp != NULL; dp = dp->d_next)
    {
	mp = dp->d_conn;
	if (mp != NULL && mp != -1 && !eq(mp->d_name, "mba"))
	{
	    struct idlst *id, *id2;
	    for (id = dp->d_vec; id; id = id->id_next) {
		for (id2 = dp->d_vec; id2; id2 = id2->id_next) {
		    if (id2 == id) {
			dump_vec(fp, id->id, dp->d_unit);
			break;
		    }
		    if (!strcmp(id->id, id2->id))
			break;
		}
	    }
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
	fprintf(fp, "\tmovl\t$%d,r0\n\tjmp\t_dzdma\n\n", number);
    else
    {
	fprintf(fp, "\tpushl\t$%d\n", number);
	fprintf(fp, "\tcalls\t$1,_%s\n\tpopr\t$0x3f\n\trei\n\n", vector);
    }
}
