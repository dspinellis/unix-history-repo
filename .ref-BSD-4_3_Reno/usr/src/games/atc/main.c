/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ed James.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.3 (Berkeley) 4/30/90";
#endif /* not lint */

#include "include.h"
#include "pathnames.h"

main(ac, av)
	char	*av[];
{
	int			seed;
	int			f_usage = 0, f_list = 0, f_showscore = 0;
	int			f_printpath = 0;
	char			*file = NULL;
	char			*name, *ptr;
#ifdef BSD
	struct itimerval	itv;
#endif
	extern int		update(), quit(), log_score();
	extern char		*default_game(), *okay_game();

	start_time = seed = time(0);

	name = *av++;
	while (*av) {
#ifndef SAVEDASH
		if (**av == '-') 
			*++*av;
		else
			break;
#endif
		ptr = *av++;
		while (*ptr) {
			switch (*ptr) {
			case '?':
			case 'u':
				f_usage++;
				break;
			case 'l':
				f_list++;
				break;
			case 's':
			case 't':
				f_showscore++;
				break;
			case 'p':
				f_printpath++;
				break;
			case 'r':
				seed = atoi(*av);
				av++;
				break;
			case 'f':
			case 'g':
				file = *av;
				av++;
				break;
			default: 
				fprintf(stderr, "Unknown option '%c'\n", *ptr,
					name);
				f_usage++;
				break;
			}
			ptr++;
		}
	}
	srandom(seed);

	if (f_usage)
		fprintf(stderr, 
		    "Usage: %s -[u?lstp] [-[gf] game_name] [-r random seed]\n",
			name);
	if (f_showscore)
		log_score(1);
	if (f_list)
		list_games();
	if (f_printpath) {
		char	buf[100];

		strcpy(buf, _PATH_GAMES);
		buf[strlen(buf) - 1] = '\0';
		puts(buf);
	}
		
	if (f_usage || f_showscore || f_list || f_printpath)
		exit(0);

	if (file == NULL)
		file = default_game();
	else
		file = okay_game(file);

	if (file == NULL || read_file(file) < 0)
		exit(1);

	init_gr();
	setup_screen(sp);

	addplane();

	signal(SIGINT, quit);
	signal(SIGQUIT, quit);
#ifdef BSD
	signal(SIGTSTP, SIG_IGN);
	signal(SIGSTOP, SIG_IGN);
#endif
	signal(SIGHUP, log_score);
	signal(SIGTERM, log_score);

#ifdef BSD
	ioctl(fileno(stdin), TIOCGETP, &tty_start);
	bcopy(&tty_start, &tty_new, sizeof(tty_new));
	tty_new.sg_flags |= CBREAK;
	tty_new.sg_flags &= ~ECHO;
	ioctl(fileno(stdin), TIOCSETP, &tty_new);
#endif

#ifdef SYSV
	ioctl(fileno(stdin), TCGETA, &tty_start);
	bcopy(&tty_start, &tty_new, sizeof(tty_new));
	tty_new.c_lflag &= ~ICANON;
	tty_new.c_lflag &= ~ECHO;
	tty_new.c_cc[VMIN] = 1;
	tty_new.c_cc[VTIME] = 0;
	ioctl(fileno(stdin), TCSETAW, &tty_new);
#endif

	signal(SIGALRM, update);

#ifdef BSD
	itv.it_value.tv_sec = 0;
	itv.it_value.tv_usec = 1;
	itv.it_interval.tv_sec = sp->update_secs;
	itv.it_interval.tv_usec = 0;
	setitimer(ITIMER_REAL, &itv, NULL);
#endif
#ifdef SYSV
	alarm(sp->update_secs);
#endif

	for (;;) {
		if (getcommand() != 1)
			planewin();
		else {
#ifdef BSD
			itv.it_value.tv_sec = 0;
			itv.it_value.tv_usec = 0;
			setitimer(ITIMER_REAL, &itv, NULL);
#endif
#ifdef SYSV
			alarm(0);
#endif

			update();

#ifdef BSD
			itv.it_value.tv_sec = sp->update_secs;
			itv.it_value.tv_usec = 0;
			itv.it_interval.tv_sec = sp->update_secs;
			itv.it_interval.tv_usec = 0;
			setitimer(ITIMER_REAL, &itv, NULL);
#endif
#ifdef SYSV
			alarm(sp->update_secs);
#endif
		}
	}
}

read_file(s)
	char	*s;
{
	extern FILE	*yyin;
	int		retval;

	file = s;
	yyin = fopen(s, "r");
	if (yyin == NULL) {
		perror(s);
		return (-1);
	}
	retval = yyparse();
	fclose(yyin);

	if (retval != 0)
		return (-1);
	else
		return (0);
}

char	*
default_game()
{
	FILE		*fp;
	static char	file[256];
	char		line[256], games[256];

	strcpy(games, _PATH_GAMES);
	strcat(games, GAMES);

	if ((fp = fopen(games, "r")) == NULL) {
		perror(games);
		return (NULL);
	}
	if (fgets(line, sizeof(line), fp) == NULL) {
		fprintf(stderr, "%s: no default game available\n", games);
		return (NULL);
	}
	fclose(fp);
	line[strlen(line) - 1] = '\0';
	strcpy(file, _PATH_GAMES);
	strcat(file, line);
	return (file);
}

char	*
okay_game(s)
	char	*s;
{
	FILE		*fp;
	static char	file[256];
	char		*ret = NULL, line[256], games[256];

	strcpy(games, _PATH_GAMES);
	strcat(games, GAMES);

	if ((fp = fopen(games, "r")) == NULL) {
		perror(games);
		return (NULL);
	}
	while (fgets(line, sizeof(line), fp) != NULL) {
		line[strlen(line) - 1] = '\0';
		if (strcmp(s, line) == 0) {
			strcpy(file, _PATH_GAMES);
			strcat(file, line);
			ret = file;
			break;
		}
	}
	fclose(fp);
	if (ret == NULL) {
		test_mode = 1;
		ret = s;
		fprintf(stderr, "%s: %s: game not found\n", games, s);
		fprintf(stderr, "Your score will not be logged.\n");
		sleep(2);	/* give the guy time to read it */
	}
	return (ret);
}

list_games()
{
	FILE		*fp;
	char		line[256], games[256];
	int		num_games = 0;

	strcpy(games, _PATH_GAMES);
	strcat(games, GAMES);

	if ((fp = fopen(games, "r")) == NULL) {
		perror(games);
		return (-1);
	}
	puts("available games:");
	while (fgets(line, sizeof(line), fp) != NULL) {
		printf("	%s", line);
		num_games++;
	}
	fclose(fp);
	if (num_games == 0) {
		fprintf(stderr, "%s: no games available\n", games);
		return (-1);
	}
	return (0);
}
