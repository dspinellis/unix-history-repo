/*	@(#)adddq.c	4.2	(Melbourne)	82/02/21	*/

#include <sys/types.h>
#include <sys/quota.h>

adddq(d1, f1, d2, f2)
register struct dquot *d1, *d2;
register char (*f1)[32], (*f2)[32];
{
	register	i, j;

	for (i = 0; i < 16; i++) {
		if (f1[i][0] == 0)
			break;
		for (j = 0; j < 16; j++) {
			if (f2[j][0] == 0)
				break;
			if (strcmp(f1[i], f2[j]))
				continue;
			addquota(&d1[i].dq_dqb, &d2[j].dq_dqb);
			goto cont;
		}
		/* there is no new quota on this filesys, delete it */
		for (j = i; j < 15; j++) {
			if (f1[j+1][0] == 0)
				break;
			d1[j] = d1[j+1];
			strcpy(f1[j], f1[j+1]);
		}
		f1[j][0] = 0;
		i--;
  cont:;
	}
}
