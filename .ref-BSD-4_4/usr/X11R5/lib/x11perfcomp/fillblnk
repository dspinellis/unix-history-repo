#!/bin/sh
echo '#####EOF#####' | cat $2 - $1 |
awk -F: '\
$1 == "#####EOF#####"	{ filling = 1; currentItem = 1; lastItem = NR; next; }
filling != "1"	{ itemOrder[" " $1] = NR; name[NR] = $1; }
filling == "1"	{ rate[itemOrder[$2]] = $1; }
END	{
	for (i = 1; i < lastItem; i++) {
		if (rate[i] != "") {
			printf ("%s: %s\n", rate[i], name[i]);
		} else {
			printf (" 0 trep @ 0.0 msec (0.0/sec): %s\n", name[i]);
		}
	}
	}'
