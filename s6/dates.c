#
/*
 * dates [ date [ identifier [ description ] ] ]
 *
 * Bill Joy June 76
 *
 */
#define canmod()  (!(getuid() & 0377))
struct lc {
	int seconds;
	int minutes;
	int hours;
	int day;
	int month;
	int yearoff;
	int weekday;
	int yearday;
	int daylight;
};
struct {
	char uid;
	char gid;
};
struct ev
{
	int owner;
	int mm;
	int dd;
	int yy;
	int hh;
	int nn;
	char ident[30];
	char z1[1];
	char descr[30];
	char z2[1];
};
char **xargv;
struct lc *local;
struct ev event, newevent;
int tvec[2];
int equal;
int status;
int suser;
int onintr();

int days[13]
{
	0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

main(argc, argv)
char *argv[];
{
	int i, j, ef, ff;

	suser = getuid() == 0;
	xargv = argv;
	time(tvec);
	local = localtime(tvec);
	local->month++;	/* 1 to 12 scale !!! */
	if ((ef = open("/usr/lib/dates", 0)) < 0)
		error("can't open /usr/lib/dates");
	if (argc >= 2)
		getdate();
	if (argc <= 2)
	{
		while ((i = read(ef, &event, sizeof event)) == sizeof event)
			if (current(&event))
				printev(&event);
		if (i != 0)
			error("bad format in /usr/lib/dates");
	}
	else
	{
		signal(1, onintr);
		signal(2, onintr);
		signal(3, onintr);
		if (!canmod())
			error("sorry, you can't modify the dates file");
		else if (creat("/tmp/dates.lock", 0) < 0)
			error("file busy, try again");
		else if ((ff = creat("/tmp/dates", 0600)) < 0)
			ferror("can't create /tmp/dates");
		while ((i = read(ef, &event, sizeof event)) == sizeof event)
			if (current(&event))
				break;
			else
trynext:
				write(ff, &event, sizeof event);
		if (i == -1)
			ferror("error reading \"/usr/lib/dates\"");
		else if (i > 0 && i < sizeof event)
			ferror("bad format in \"/usr/lib/dates\"");
		else if (argc == 3 && !i)
			ferror("entry to be deleted does not exist");
		newevent.owner = getuid();
		newevent.mm = local->month;
		newevent.dd = local->day;
		newevent.yy = local->yearoff;
		newevent.hh = local->hours;
		newevent.nn = local->minutes;
		for (j = 0; j < sizeof event.ident; j++)
			if (!(newevent.ident[j] = argv[2][j]))
				while(j < sizeof event.ident)
					newevent.ident[j++] = ' ';
		if (argc == 3)
		{
			if (newevent.dd != event.dd || newevent.mm != event.mm
			  || newevent.yy != event.yy || newevent.hh != event.hh
			  || newevent.nn != event.nn)
				goto trynext;
			for (j = 0; j < sizeof event.descr; j++)
				if (newevent.ident[j] != event.ident[j])
					goto trynext;
			if (newevent.owner && event.owner != newevent.owner)
				ferror("sorry, you can't delete that");
			else
			{
				printf("deleted:\n");
				printev(&event);
			}
		}
		else
		{
			for (j = 0; j < sizeof event.descr; j++)
				if (!(newevent.descr[j] = argv[3][j]))
					break;
			write(ff, &newevent, sizeof event);
			if (i)
				write(ff, &event, sizeof event);
			printev(&newevent);
		}
		if (i)
		while ((i = read(ef, &event, sizeof event)) == sizeof event)
			write(ff, &event, sizeof event);
		signal(1, 1);
		signal(2, 1);
		signal(3, 1);
		if (sys("cp", "/tmp/dates", "/usr/lib/dates", 0))
			ferror("cp failed: contact system staff!");
		else
		{
			unlink("/tmp/dates");
			unlink("/tmp/dates.lock");
		}
	}
}

getdate()
{
	char *argp;

	argp = xargv[1];
	if (!getnum(&argp, &local->month))
		error("month expected but '%c' found", *argp);
	else if (local->month == 0)
		error("month cannot be zero");
	else if (local->month > 12)
		error("month cannot be > 12 (%d)", local->month);
	else if (*argp++ != '/')
		error("'/' separating month from day expected, found '%c'",
			*--argp);
	else if (!getnum(&argp, &local->day))
		error("day expected but '%c' found", *argp);
	else if (local->day == 0)
		error("day cannot be zero");
	else if (local->day > days[local->month])
		error("there are at most %d days in that month",
			days[local->month]);
	else
	{
		if (*argp == '/')
		{
			++argp;
			if (!getnum(&argp, &local->yearoff))
				error("year expected but '%c' found", *argp);
			else if (local->yearoff > 99)
			{
				if (local->yearoff > 1900)
					local->yearoff=- 1900;
				if (local->yearoff > 99)
					error("did this program survive to 2000 !?");
			}
		}
		if (*argp == '.')
		{
			++argp;
			local->minutes = 0;
			if (!getnum(&argp, &local->hours))
				error("hours expected but '%c' found", *argp);
			else if (local->hours > 24)
				error("there are only 24 hours in a day");
			else if (*argp++ != ':')
				argp--;
			else if (!getnum(&argp, &local->minutes))
				error("minutes expected but '%c' found", *argp);
			else if (local->minutes > 59)
				error("there are only 59 minutes in an hour");
			else if (local->hours == 24 && local->minutes)
				error("sorry sire, but for 24:xx, xx may %s",
					"only be 00");
		}
		else
		{
			local->hours = -1;
			local->minutes = 0;
		}
	}
	if (*argp != 0)
		error("extra characters: \"%s\" at end of date", argp);
	else
		return(1);
}

printev(aevent)
struct ev *aevent;
{
	register struct ev *aev;

	aev = aevent;
	if (suser)
		printf("%d	", aev->gid & 0377);
	printf("%2d/", aev->mm);
	if (aev->dd < 10)
		printf("0%1d/", aev->dd);
	else
		printf("%2d/", aev->dd);
	printf("%2d", aev->yy);
	if (aev->hh == -1)
		printf("         ");
	else
	{
		if (aev->hh < 10)
			printf(".0%1d:", aev->hh);
		else
			printf(".%2d:", aev->hh);
		if (aev->nn < 10)
			printf("0%1d   ", aev->nn);
		else
			printf("%2d   ", aev->nn);
	}
	printf("%s %s\n", aev->ident, aev->descr);
}

current(aevent)
struct event *aevent;
{
	register struct event *aev;

	aev = aevent;
	equal = 0;
	if (aev->yy != local->yearoff)
		return(aev->yy > local->yearoff);
	else if (aev->mm != local->month)
		return(aev->mm > local->month);
	else if (aev->dd != local->day)
		return(aev->dd > local->day);
	else if (aev->hh != local->hours)
		return(aev->hh > local->hours);
	else
	{
		equal = aev->nn == local->minutes;
		return(aev->nn >= local->minutes);
	}
}

ferror(a1, a2, a3, a4)
{
	unlink("/tmp/dates");
	unlink("/tmp/dates.lock");
	if (a1)
		error(a1, a2, a3, a4);
	exit(1);
}

onintr()
{
	ferror(0);
}

error(a1, a2, a3, a4)
{
	printf(a1, a2, a3, a4);
	putchar('\n');
	exit(1);
}

getnum(acp, anum)
char **acp;
int *anum;
{
	register char *cp;
	register num;

	num = 0;
	cp = *acp;
	if (*cp >= '0' && *cp <= '9')
	{
		do
			num = num * 10 + *cp++ - '0';
		while (*cp >= '0' && *cp <= '9');
		*acp = cp;
		*anum = num;
		return(1);
	}
	else
		return(0);
}

sys(name)
char *name;
{
	int i, k;
	register char *argp, *rp;
	char routine[25];

	if ((i = fork()) < 0)
		ferror("try again.");
	else if (i)
	{
		while ((k = wait(&status)) != i && k != -1)
			continue;
		if (k == -1)
			ferror("no children to wait for !!!\n");
		else
			if (status)
				ferror(0);
		return(status);
	}
	else
	{
		argp = "/usr/bin/";
		rp = routine;
		while(*rp++ = *argp++)
			continue;
		rp--;
		argp = name;
		while (*rp++ = *argp++)
			continue;
		execv(routine+4, &name);
		execv(routine, &name);
		ferror("can't find %s", name);
	}
}
