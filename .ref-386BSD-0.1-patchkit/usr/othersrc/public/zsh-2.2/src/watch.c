/*
 *
 * watch.c - login/logout watching
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#include "zsh.h"
#include <utmp.h>

static int wtabsz;
struct utmp *wtab;

/* get the time of login/logout for WATCH */

time_t getlogtime(u,inout) /**/
struct utmp *u;int inout;
{
FILE *in;
struct utmp uu;
int first = 1;
int srchlimit = 50; /* max number of wtmp records to search */

	if (inout)
		return u->ut_time;
	if (!(in = fopen(WTMP_FILE,"r")))
		return time(NULL);
	fseek(in,0,2);
	do
		{
		if (fseek(in,((first) ? -1 : -2)*sizeof(struct utmp),1))
			{
			fclose(in);
			return time(NULL);
			}
		first = 0;
		if (!fread(&uu,sizeof(struct utmp),1,in))
			{
			fclose(in);
			return time(NULL);
			}
		if (uu.ut_time < lastwatch || !srchlimit--)
			{
			fclose(in);
			return time(NULL);
			}
		}
	while (memcmp(&uu,u,sizeof(struct utmp)));
	do
		if (!fread(&uu,sizeof(struct utmp),1,in))
			{
			fclose(in);
			return time(NULL);
			}
	while (strncmp(uu.ut_line,u->ut_line,8));
	fclose(in);
	return uu.ut_time;
}

/* print a login/logout event */

void watchlog2(inout,u,fmt) /**/
int inout;struct utmp *u;char *fmt;
{
char *p,buf[40],*bf;
int i;
time_t timet;
struct tm *tm = NULL;

	while (*fmt)
		if (*fmt != '%')
			putchar(*fmt++);
		else
			{
			fmt++;
			switch(*fmt++)
				{
				case 'n':
					printf("%.*s",8,u->ut_name);
					break;
				case 'a':
					printf("%s",(!inout) ? "logged off" : "logged on");
					break;
				case 'l':
					if (u->ut_line[0] == 't')
						printf("%.*s",5,u->ut_line+3);
					else
						printf("%.*s",8,u->ut_line);
					break;
#ifdef UTMP_HOST
				case 'm':
					for (p = u->ut_host,i = 16; i && *p;i--,p++)
						{
						if (*p == '.' && !idigit(p[1]))
							break;
						putchar(*p);
						}
					break;
				case 'M':
					printf("%.*s",16,u->ut_host);
					break;
#endif
				case 't':
				case '@':
					timet = getlogtime(u,inout);
					tm = localtime(&timet);
					ztrftime(buf,40,"%l:%M%p",tm);
					printf("%s",(*buf == ' ') ? buf+1 : buf);
					break;
				case 'T':
					timet = getlogtime(u,inout);
					tm = localtime(&timet);
					ztrftime(buf,40,"%k:%M",tm);
					printf("%s",buf);
					break;
				case 'w':
					timet = getlogtime(u,inout);
					tm = localtime(&timet);
					ztrftime(buf,40,"%a %e",tm);
					printf("%s",buf);
					break;
				case 'W':
					timet = getlogtime(u,inout);
					tm = localtime(&timet);
					ztrftime(buf,40,"%m/%d/%y",tm);
					printf("%s",buf);
					break;
				case 'D':
					timet = getlogtime(u,inout);
					tm = localtime(&timet);
					ztrftime(buf,40,"%y-%m-%d",tm);
					printf("%s",buf);
					break;
				case '%':
					putchar('%');
					break;
				case 'S':
					bf = buf;
					if (tgetstr("so",&bf))
						fputs(buf,stdout);
					break;
				case 's':
					bf = buf;
					if (tgetstr("se",&bf))
						fputs(buf,stdout);
					break;
				case 'B':
					bf = buf;
					if (tgetstr("md",&bf))
						fputs(buf,stdout);
					break;
				case 'b':
					bf = buf;
					if (tgetstr("me",&bf))
						fputs(buf,stdout);
					break;
				case 'U':
					bf = buf;
					if (tgetstr("us",&bf))
						fputs(buf,stdout);
					break;
				case 'u':
					bf = buf;
					if (tgetstr("ue",&bf))
						fputs(buf,stdout);
					break;
				default:
					putchar('%');
					putchar(fmt[-1]);
					break;
				}
			}
	putchar('\n');
}

/* check the List for login/logouts */

void watchlog(inout,u,w,fmt) /**/
int inout;struct utmp *u;char **w;char *fmt;
{
char *v,*vv,sav;
int bad;

	if (*w && !strcmp(*w,"all"))
		{
		watchlog2(inout,u,fmt);
		return;
		}
	for (; *w; w++)
		{
		bad = 0;
		v = *w;
		if (*v != '@' && *v != '%')
			{
			for (vv = v; *vv && *vv != '@' && *vv != '%'; vv++);
			sav = *vv;
			*vv = '\0';
			if (strncmp(u->ut_name,v,8))
				bad = 1;
			*vv = sav;
			v = vv;
			}
		for (;;)
			if (*v == '%')
				{
				for (vv = ++v; *vv && *vv != '@'; vv++);
				sav = *vv;
				*vv = '\0';
				if (strncmp(u->ut_line,v,8))
					bad = 1;
				*vv = sav;
				v = vv;
				}
#ifdef UTMP_HOST
			else if (*v == '@')
				{
				for (vv = ++v; *vv && *vv != '%'; vv++);
				sav = *vv;
				*vv = '\0';
				if (strncmp(u->ut_host,v,strlen(v)))
					bad = 1;
				*vv = sav;
				v = vv;
				}
#endif
			else
				break;
		if (!bad)
			{
			watchlog2(inout,u,fmt);
			return;
			}
		}
}

/* compare 2 utmp entries */

int ucmp(u,v) /**/
struct utmp *u;struct utmp *v;
{
	if (u->ut_time == v->ut_time)
		return strncmp(u->ut_line,v->ut_line,8);
	return u->ut_time - v->ut_time;
}

/* initialize the user List */

void readwtab() /**/
{
struct utmp *uptr;
int wtabmax = 32;
FILE *in;

	wtabsz = 0;
	if (!(in = fopen(UTMP_FILE,"r"))) return;
	uptr = wtab = (struct utmp *) zalloc(wtabmax*sizeof(struct utmp));
	while (fread(uptr,sizeof(struct utmp),1,in))
#ifdef USER_PROCESS
		if (uptr->ut_type == USER_PROCESS)
#else
		if (uptr->ut_name[0])
#endif
			{
			uptr++;
			if (++wtabsz == wtabmax)
				uptr = (wtab = (struct utmp *) realloc((vptr) wtab,(wtabmax*=2)*
					sizeof(struct utmp)))+wtabsz;
			}
	fclose(in);
	if (wtabsz)
		qsort(wtab,wtabsz,sizeof(struct utmp),ucmp);
}

/* check for login/logout events; executed before each prompt
	if WATCH is set */

void dowatch() /**/
{
char **s = watch;
char *fmt = (watchfmt) ? watchfmt : DEFWATCHFMT;
FILE *in;
int utabsz = 0,utabmax = wtabsz+4,uct,wct;
struct utmp *utab,*uptr,*wptr;

	holdintr();
	if (!fmt)
		fmt = "%n has %a %l from %m.";
	if (!wtab) {
		readwtab();
		noholdintr();
		return;
	}
	uptr = utab = (struct utmp *) zalloc(utabmax*sizeof(struct utmp));
	if (!(in = fopen(UTMP_FILE,"r"))) {
		free(utab);
		return;
	}
	while (fread(uptr,sizeof *uptr,1,in))
#ifdef USER_PROCESS
		if (uptr->ut_type == USER_PROCESS)
#else
		if (uptr->ut_name[0])
#endif
			{
			uptr++;
			if (++utabsz == utabmax)
				uptr = (utab = (struct utmp *) realloc((vptr) utab,(utabmax*=2)*
					sizeof(struct utmp)))+utabsz;
			}
	fclose(in);
	noholdintr();
	if (errflag) {
		free(utab);
		return;
	}
	if (utabsz)
		qsort(utab,utabsz,sizeof(struct utmp),ucmp);

	wct = wtabsz; uct = utabsz;
	uptr = utab; wptr = wtab;
	if (errflag) {
		free(utab);
		return;
	}
	while ((uct || wct) && !errflag)
		if (!uct || (wct && ucmp(uptr,wptr) > 0))
			wct--,watchlog(0,wptr++,s,fmt);
		else if (!wct || (uct && ucmp(uptr,wptr) < 0))
			uct--,watchlog(1,uptr++,s,fmt);
		else
			uptr++,wptr++,wct--,uct--;
	free(wtab);
	wtab = utab;
	wtabsz = utabsz;
	fflush(stdout);
}

int bin_log(nam,argv,ops,func) /**/
char *nam;char **argv;char *ops;int func;
{
	if (!watch)
		return 1;
	if (wtab)
		free(wtab);
	wtab = (struct utmp *) zalloc(1);
	wtabsz = 0;
	dowatch();
	return 0;
}

