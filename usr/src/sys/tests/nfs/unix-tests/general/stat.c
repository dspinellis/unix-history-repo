/*	@(#)stat.c	1.2 90/01/03 NFS Rev 2 Testsuite	*/
/*	1.3 Lachman ONC Test Suite source	*/

#include <stdio.h>
#include <math.h>

/*
 *  crunch through time stat files.  This program will handle two
 *  formats: BSD
 *	        1.7 real         0.0 user         0.4 sys  
 *  and ATT
 *		real        1.4
 *		user        0.0
 *		sys         0.2
 *
 *  ATT format may break out minutes -- 2:03.2
 */
#define MAXINDEX 100
double	real[MAXINDEX];
double	user[MAXINDEX];
double	sys[MAXINDEX];
char	*Prog, *File;

main(argc, argv)
int	argc;
char	*argv[];
{
	FILE	*fp;
	int	i, n;
	char	c, *fmt;
	int	attfmt = 0;	/* set if using att time format */

	Prog = argv[0];
	if (argc == 1) {
		fprintf(stderr, "Usage: %s datafile\n", Prog);
		exit(1);
	}
	File = argv[1];
	fp = fopen(File, "r");
	if (fp == NULL) {
		fprintf(stderr, "%s: unable to open %s\n",
			Prog, File);
		exit(1);
	}
	if ((i = fgetc(fp)) == EOF) {
		fprintf(stderr, "%s: %s is empty\n",
			Prog, File);
		exit(1);
	}
	c = i & 0x7f;
	if (c == '\n' || c == '\r' || c == 'r')
		attfmt = 1;
	else
		fmt = "%F %*s %F %*s %F %*s";		/* BSD fmt */
	if (ungetc(c, fp) == EOF) {
		fprintf(stderr, "%s: can't push char back to %s\n",
                        Prog, File);
                exit(1); 
	}

	if (attfmt) {
		for (n = 0; getattfmt(fp, n, 1); n++) {
			getattfmt(fp, n, 2);
			getattfmt(fp, n, 3);
		}
	} else {
		n = 0;
		while(fscanf(fp, fmt, &real[n], &user[n], &sys[n]) == 3)
			n++;
	}
	if (n == 0) {
		fprintf(stderr, "%s: no data in %s\n",
                        Prog, File);
#ifdef SVR3
                exit(0);
#else
                exit(1);
#endif
        }
	stat(real, n);
	printf(" real");
	stat(user, n);
	printf(" user");
	stat(sys, n);
	printf(" sys\n");

	exit(0);
}

/*
 *  which:  1: real, 2:user, 3:sys
 *
 *  returns 0 if no more data, else 1
 */
getattfmt(fp, n, which)
FILE *fp;
int n, which;
{
	char	buf[BUFSIZ];
	char	*p;
	char	*fmt;
	double	*dp;
	int 	min, err = 0;

	if (n < 0 || n >= MAXINDEX) {
		fprintf(stderr, "%s: illegal index=%d in getattfmt\n",
			Prog, n);
		exit(1);
	}
	switch(which) {
	    case 1:		/* real */
		dp = &real[n];
		break;
	    case 2:		/* user */
		dp = &user[n];
		break;
	    case 3:		/* sys  */
		dp = &sys[n];
		break;
	    default:
		fprintf(stderr, "%s: illegal which=%d in getattfmt\n",
			Prog, which);
		exit(1);
	}

	while (fgets(buf, BUFSIZ, fp)) {
		/* null out newline */
		for (p = buf; *p && *p != '\n'; p++)
			;
		if (*p == '\n')
			*p = '\0';

		/* look for blank line and skip it */
		for (p = buf; *p && (*p == ' ' || *p == '\t'); p++)
			;
		if (*p == '\0')
			continue;

		min = 0;
		for (p = buf; *p && *p != ':'; p++)
			;
		if (*p == ':') {
			fmt = "%*s %d:%F";
			if (sscanf(buf, fmt, &min, dp) != 2)
				err = 1;
		} else {
			fmt = "%*s %F";
			if (sscanf(buf, fmt, dp) != 1)
				err = 1;
		}
		if (err) {
			fprintf(stderr, "%s: bad data format in %s (%s)\n",
				Prog, File, buf);
			exit(1);
		}
		if (min > 0)
			*dp += (double)(min * 60);
		return 1;
	}
	/* EOF */
	if (which == 1)
		return 0;
	else {
		fprintf(stderr, "%s: premature EOF in %s\n",
			Prog, File);
		exit(1);
	}
}
			
stat(array, n)
double	array[];
int	n;
{
	double	avg, sd;
	int	i;
	
	avg = 0;
	for (i = 0; i < n; i++)
		avg += array[i];
	avg = avg / (float) n;
	sd = 0;
	for (i = 0; i < n; i++)
		sd += (array[i] - avg)*(array[i] - avg);
	if (n > 1) {
		sd = sd / (float) (n - 1);
        	sd = sqrt(sd);	
	} else {
		sd = 0.0;
	}
	printf("\t%.1f (%.1f)", avg, sd);
}
