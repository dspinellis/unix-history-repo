#include <whoami.h>
/*
 * leave - reminds you when you have to leave.
 * leave prompts for input and goes away if you hit return.
 * it nags you like a mother hen.
 */
char origlogin[20], thislogin[20];
char *getlogin();
char *whenleave;
char *ctime();
char buff[100];

main(argc,argv) char **argv; {
	long when, tod, now, diff, hours, minutes;
	int *nv;
	int atoi();
	int *localtime();

	if (argc < 2) {
		printf("When do you have to leave? ");
		buff[read(0,buff,sizeof buff)] = 0;
	} else {
		strcpy(buff,argv[1]);
	}
	if (buff[0] == '\n') exit(0);
	if (buff[0] == '+') {
		diff = atoi(buff+1);
		doalarm(diff);
	}
	if (buff[0] < '0' || buff[0] > '9') {
		printf("usage: %s [hhmm]\n",argv[0]);
		exit(1);
	}
	strcpy(origlogin,getlogin());

	tod = atoi(buff);
	hours = tod / 100;
	if (hours > 12) hours -= 12;
	if (hours == 12) hours = 0;
	minutes = tod % 100;

	if (hours < 0 || hours > 12 || minutes < 0 || minutes > 59) {
		printf("usage: %s [hhmm]\n",argv[0]);
		exit(1);
	}

	setexit();	/* refigure time if killed */
	time(&now);
	nv = localtime(&now);
	when = 60*hours+minutes;
	if (nv[2] > 12) nv[2] -= 12;	/* do am/pm bit */
	now = 60*nv[2] + nv[1];
	diff = when - now;
	while (diff < 0)
		diff += 12*60;
	if (diff > 11*60) printf("That time has already passed!\n");
	doalarm(diff);
	exit(0);
}


doalarm(nmins) long nmins; {
	char *msg1, *msg2, *msg3, *msg4;
	register int i;
	int slp1, slp2, slp3, slp4;
	int seconds, gseconds;
	long daytime;

	seconds = 60 * nmins;
	if (seconds <= 0) seconds = 1;
	gseconds = seconds;

	msg1 = "You have to leave in 5 minutes";
	if (seconds <= 60*5) {
		slp1 = 0;
	} else {
		slp1 = seconds - 60*5;
		seconds = 60*5;
	}

	msg2 = "Just one more minute!";
	if (seconds <= 60) {
		slp2 = 0;
	} else {
		slp2 = seconds - 60;
		seconds = 60;
	}

	msg3 = "Time to leave!";
	slp3 = seconds;

	msg4 = "You're going to be late!";
	slp4 = 60;

	time(&daytime);
	daytime += gseconds;
	whenleave = ctime(&daytime);
	printf("Alarm set for %s\n",whenleave);
	if (fork()) exit(0);
	signal(2,1);
	signal(3,1);
	signal(15,1/*nag*/);

	if (slp1)
		bother(slp1,msg1);
	if (slp2)
		bother(slp2,msg2);
	bother(slp3,msg3);
	for (;;) {
		bother(slp4,msg4);
	}
}

bother(slp,msg) int slp; char *msg; {

	delay(slp);
	printf("\7\7\7");
	printf("%s\n",msg);
}

/*
 * delay is like sleep but does it in 100 sec pieces and
 * knows what zero means.
 */
delay(secs) int secs; {
	int n;

	while(secs>0) {
		n = 100;
		secs = secs - 100;
		if (secs < 0) {
			n = n + secs;
		}
		if (n > 0) sleep(n);
		strcpy(thislogin,getlogin());
		if (strcmp(origlogin, thislogin)) exit(0);
	}
}

#ifndef VAX
char *getlogin() {
#include <utmp.h>
	static struct utmp ubuf;
	int ufd;

	ufd = open("/etc/utmp",0);
	seek(ufd, ttyn(0)*sizeof(ubuf), 0);
	read(ufd, &ubuf, sizeof(ubuf));
	ubuf.ut_name[8] = 0;
	return(&ubuf.ut_name);
}
#endif
