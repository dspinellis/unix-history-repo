#!/bin/sh
awk '
/^     1/ && READY == 0 {	printf ("    1   ");
		for (i = 2; i < NF; i++)
			printf ("          %2d        ", i);
		printf ("   Operation\n");
		next;
	}
/^---/	{ 	printf ("--------");
		for (i = 2; i <= NF; i++)
			printf ("   -----------------");
		printf ("\n");
		READY=1; next;
 	}
READY==1 {
		base=$1;
		printf ("%8.1f", base);
		for (i = 2; i < '$1'; i++) {
			if (base == 0)
				printf ("   %8.1f         ", $i);
			else {
				rate=$i/base;
				if (rate < .1)
					printf ("   %8.1f (%6.3f)", $i, rate);
				else if (rate < 1000)
					printf ("   %8.1f (%6.2f)", $i, rate);
				else
					printf ("   %8.1f (%6.0f)", $i, rate);
			}
		}
		printf ("   ");
		for (; i <= NF; i++)
		{
			printf ("%s ", $i);
		}
		printf ("\n");
		next;
	   }
	   { print $0; }
'
