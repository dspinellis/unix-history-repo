#ifndef lint
static	char *sccsid = "@(#)sync.c	1.3 83/10/14";
#endif

#include "externs.h"

static char sync_buf[BUFSIZE];
static char *sync_bufp = sync_buf;
static char sync_lock[25];
static char sync_file[25];
static long sync_seek;
static FILE *sync_fp;
#define SF "/tmp/#sailsink.%d"
#define LF "/tmp/#saillock.%d"

/*VARARGS3*/
makesignal(from, fmt, ship, a, b, c)
struct ship *from;
char *fmt;
register struct ship *ship;
{
	char message[80];

	if (ship == 0)
		(void) sprintf(message, fmt, a, b, c);
	else
		(void) sprintf(message, fmt,
			ship->shipname, colours(ship),
			sterncolour(ship), a, b, c);
	Write(W_SIGNAL, from, 1, (int)message, 0, 0, 0);
}

sync_exists(game)
{
	char buf[sizeof sync_file];

	(void) sprintf(buf, SF, game);
	return access(buf, 0) >= 0;
}

sync_open()
{
	(void) sprintf(sync_lock, LF, game);
	(void) sprintf(sync_file, SF, game);
	if (access(sync_file, 0) < 0) {
		int omask;
#ifdef SETUID
		omask = umask(077);
#else
		omask = umask(011);
#endif
		sync_fp = fopen(sync_file, "w+");
		(void) umask(omask);
	} else
		sync_fp = fopen(sync_file, "r+");
	if (sync_fp == 0)
		return -1;
	sync_seek == 0;
}

sync_close(remove)
char remove;
{
	(void) fclose(sync_fp);
	if (remove)
		(void) unlink(sync_file);
}

Write(type, ship, isstr, a, b, c, d)
int type;
struct ship *ship;
char isstr;
int a, b, c, d;
{
	if (isstr)
		(void) sprintf(sync_bufp, "%d %d %d %s\n",
			type, ship-SHIP(0), isstr, a);
	else
		(void) sprintf(sync_bufp, "%d %d %d %d %d %d %d\n",
			type, ship-SHIP(0), isstr, a, b, c, d);
	while (*sync_bufp++)
		;
	sync_bufp--;
	if (sync_bufp >= &sync_buf[sizeof sync_buf])
		abort();
	sync_update(type, ship, a, b, c, d);
}

Sync()
{
	int (*sig1)(), (*sig2)();
	int type, shipnum, isstr, a, b, c, d;
	char buf[60];
	register char *p = buf;
	int n;

	sig1 = signal(SIGHUP, SIG_IGN);
	sig2 = signal(SIGINT, SIG_IGN);
	for (n = 0; link(sync_file, sync_lock) < 0 && n < 30; n++)
		sleep(2);
	(void) fseek(sync_fp, sync_seek, 0);
	while (fscanf(sync_fp, "%d%d%d", &type, &shipnum, &isstr) != EOF) {
		if (isstr) {
			for (p = buf;;) {
				switch (*p++ = getc(sync_fp)) {
				case '\n':
					p--;
				case EOF:
					break;
				default:
					continue;
				}
				break;
			}
			*p = 0;
			for (p = buf; *p == ' '; p++)
				;
			a = (int)p;
			b = c = d = 0;
		} else
			(void) fscanf(sync_fp, "%d%d%d%d", &a, &b, &c, &d);
		sync_update(type, SHIP(shipnum), a, b, c, d);
	}
	if (sync_bufp != sync_buf) {
		(void) fseek(sync_fp, 0L, 2);
		(void) fputs(sync_buf, sync_fp);
		(void) fflush(sync_fp);
		sync_bufp = sync_buf;
	}
	sync_seek = ftell(sync_fp);
	(void) unlink(sync_lock);
	(void) signal(SIGHUP, sig1);
	(void) signal(SIGINT, sig2);
}

sync_update(type, ship, a, b, c, d)
int type;
register struct ship *ship;
int a, b, c, d;
{
	switch (type) {
	case W_DBP: {
		register struct BP *p = &ship->file->DBP[a];
		p->turnsent = b;
		p->toship = SHIP(c);
		p->mensent = d;
		break;
		}
	case W_OBP: {
		register struct BP *p = &ship->file->OBP[a];
		p->turnsent = b;
		p->toship = SHIP(c);
		p->mensent = d;
		break;
		}
	case W_FOUL: {
		register struct snag *p = &ship->file->fouls[a];
		p->turnfoul = b;
		p->toship = SHIP(c);
		break;
		}
	case W_GRAP: {
		register struct snag *p = &ship->file->grapples[a];
		p->turnfoul = b;
		p->toship = SHIP(c);
		break;
		}
	case W_SIGNAL:
		if (isplayer) {
			(void) putchar('\7');
			Signal("%s (%c%c): %s", ship, a);
		}
		break;
	case W_CREW: {
		register struct shipspecs *s = ship->specs;
		s->crew1 = a;
		s->crew2 = b;
		s->crew3 = c;
		break;
		}
	case W_CAPTAIN:
		(void) strncpy(ship->file->captain, (char *)a,
			sizeof ship->file->captain - 1);
		ship->file->captain[sizeof ship->file->captain - 1] = 0;
		break;
	case W_CAPTURED:
		ship->file->captured = SHIP(a);
		break;
	case W_CLASS:
		ship->specs->class = a;
		break;
	case W_DRIFT:
		ship->file->drift = a;
		break;
	case W_EXPLODE:
		if ((ship->file->explode = a) == 2)
			ship->file->dir = 0;
		break;
	case W_FS:
		ship->file->FS = a;
		break;
	case W_GUNL: {
		register struct shipspecs *s = ship->specs;
		s->gunL = a;
		s->carL = b;
		break;
		}
	case W_GUNR: {
		register struct shipspecs *s = ship->specs;
		s->gunR = a;
		s->carR = b;
		break;
		}
	case W_HULL:
		ship->specs->hull = a;
		break;
	case W_LAST:
		(void) strncpy(ship->file->last, (char *)a,
			sizeof ship->file->last - 1);
		ship->file->last[sizeof ship->file->last - 1] = 0;
		break;
	case W_PCREW:
		ship->file->pcrew = a;
		break;
	case W_POINTS:
		ship->file->points = a;
		break;
	case W_QUAL:
		ship->specs->qual = a;
		break;
	case W_RIGG: {
		register struct shipspecs *s = ship->specs;
		s->rig1 = a;
		s->rig2 = b;
		s->rig3 = c;
		s->rig4 = d;
		break;
		}
	case W_RIG1:
		ship->specs->rig1 = a;
		break;
	case W_RIG2:
		ship->specs->rig2 = a;
		break;
	case W_RIG3:
		ship->specs->rig3 = a;
		break;
	case W_RIG4:
		ship->specs->rig4 = a;
		break;
	case W_SHIPCOL:
		ship->file->col = a;
		break;
	case W_SHIPDIR:
		ship->file->dir = a;
		break;
	case W_SHIPROW:
		ship->file->row = a;
		break;
	case W_SINK:
		if ((ship->file->sink = a) == 2)
			ship->file->dir = 0;
		break;
	case W_STRUCK:
		ship->file->struck = a;
		break;
	case W_TA:
		ship->specs->ta = a;
		break;
	case W_ALIVE:
		alive = 1;
		break;
	case W_TURN:
		turn = a;
		break;
	case W_WIND:
		winddir = a;
		windspeed = b;
		break;
	case W_BEGIN:
		(void) strcpy(ship->file->captain, "begin");
		people++;
		break;
	case W_END:
		(void) strcpy(ship->file->captain, "");
		people--;
		break;
	default:
		fprintf(stderr, "sync_update: unknown type %d\n", type);
		break;
	}
}
