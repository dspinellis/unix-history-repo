#include <stdio.h>
#include <sys/types.h>
#include <a.out.h>
#include <time.h>

struct	tm *localtime();

#define	DIV60(t)	((t+30)/60)    /* x/60 rounded */ 

struct	nlist	nl[] = {
#define X_BOOTIME	0
	{ "_bootime" },
#define X_AVENRUN	1
	{ "_avenrun" },
	{ 0 }
};

time_t	now, bootime;
double	avenrun[3];

int	kmem;

main(argc, argv)
	char **argv;
{
	char obuf[BUFSIZ];
	register int i, days;
	register time_t uptime;
	register struct tm *nowt;

	setbuf(stdout, obuf);
	nlist("/vmunix" , nl);
	if (nl[0].n_value == 0) {
		fprintf(stderr, "No namelist\n");
		exit(1);
	}
	kmem = open("/dev/kmem", 0);
	if (kmem < 0) {
		fprintf(stderr ,"No kmem\n");
		exit(1);
	}
	time(&now);
	nowt = localtime(&now);
	prtat(nowt);
	lseek(kmem, (long)nl[X_BOOTIME].n_value, 0);
	read(kmem, &bootime, sizeof (bootime));
	uptime = now - bootime;
	printf("  up");
	days = uptime / (60*60*24);
	if (days > 0) {
		printf(" %d day%s, ", days, days>1?"s":"");
		uptime %= (60*60*24);
	}
	prttime(DIV60(uptime), "");
	printf("\t\t");
	printf("load average:");
	lseek(kmem, (long)nl[X_AVENRUN].n_value, 0);
	read(kmem, avenrun, sizeof(avenrun));
	for (i = 0; i < 3; i++) {
		printf(" %.2f", avenrun[i]);
		if (i < 2)
			printf(",");
	}
	printf("\n");
}

prttime(tim, tail)
	time_t tim;
	char *tail;
{
	register int didhrs = 0;

	if (tim >= 60) {
		printf("%3d:", tim/60);
		didhrs++;
	} else {
		printf("    ");
	}
	tim %= 60;
	if (tim > 0 || didhrs) {
		printf(didhrs&&tim<10 ? "%02d" : "%2d", tim);
	} else {
		printf("  ");
	}
	printf("%s", tail);
}

/* prtat prints a 12 hour time given a pointer to a time of day */
prtat(p)
	struct tm *p;
{
	register int t, pm;

	t = p -> tm_hour;
	pm = (t > 11);
	if (t > 11)
		t -= 12;
	if (t == 0)
		t = 12;
	prttime(t*60 + p->tm_min, pm ? "pm" : "am");
}
