/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/ioctl.h>

#include <limits.h>
#include <regex.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * This is where all of the "global" variables are declared. They are
 * set for extern in the ed.h header file (so everyone can get them).
 */

int nn_max, nn_max_flag, Start_default, End_default, address_flag;
int zsnum, filename_flag, add_flag=0, join_flag=0;
int help_flag=0, gut_num=-1;
#ifdef STDIO
FILE *fhtmp;
int file_seek;
#endif

#ifdef DBI
DB *dbhtmp;
#endif

LINE *nn_max_start, *nn_max_end;

struct MARK mark_matrix[26]; /* in init set all to null */ 

char *text;
LINE **gut=NULL;
char *filename_current, *prompt_string=NULL, help_msg[130];
char *template=NULL;
int prompt_str_flg=0, start_up_flag=0, name_set=0;

LINE *top, *current, *bottom, *Start, *End; 
struct u_layer *u_stk;
struct d_layer *d_stk;
LINE *u_current, *u_top, *u_bottom;
int u_set;
regex_t RE_comp;
regmatch_t RE_match[RE_SEC];
int RE_sol=0, RE_flag=0;
char *RE_patt=NULL;

int ss; /* for the getc() */
int explain_flag=1, g_flag=0, GV_flag=0, printsfx=0, exit_code=0;
long change_flag=0L;
int line_length;
jmp_buf ctrl_position, ctrl_position2, ctrl_position3; /* For SIGnal handling. */
int sigint_flag, sighup_flag, sigspecial=0, sigspecial2=0, sigspecial3=0;

static void sigint_handler __P((int));
static void sighup_handler __P((int));

/*
 * Starts the whole show going. Set things up as the arguments spec
 * in the shell and set a couple of the global variables.
 *
 * Note naming viol'n with errnum for consistancy.
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	int l_num, errnum=0, l_err=0;
	char *l_fnametmp, *l_col, buf[2];
	struct winsize win;

	setbuffer(stdin, buf, 1);
	line_length = ((l_col = getenv("COLUMNS")) == NULL ? 0 : atoi(l_col));
	if ((line_length == 0 && isatty(STDOUT_FILENO) &&
		ioctl(STDOUT_FILENO, TIOCGWINSZ, &win) != -1))
		line_length = win.ws_col;
	if (line_length == 0)
		line_length = 78;
	line_length -= 3;	/* for the octal to break properly in 'l' */

	Start = End = NULL;
	top = bottom = NULL;
	current = NULL;
	nn_max_flag = 0;
	nn_max_start = nn_max_end = NULL;
	l_fnametmp = calloc(FILENAME_LEN, sizeof(char));
	if (l_fnametmp == NULL)
		ed_exit(4);
	text = calloc(NN_MAX_START + 2, sizeof(char));
	if (text == NULL)
		ed_exit(4);
	Start_default = End_default = 0;
	zsnum = 22;		/* for the 'z' command */
	help_msg[0] = '\0';
	u_stk = NULL;
	d_stk = NULL;
	u_current = u_top = u_bottom = NULL;
	u_set = 0;		/* for in d after a j */
	filename_flag = 0;
	filename_current = NULL;

	l_num = 1;
	for (;;) {
		/* Process the command line options */
		if (l_num >= argc)
			break;
		switch (argv[l_num][0]) {
		case '-':
			switch (argv[l_num][1]) {
			case '\0':	/* this is why 'getopt' not used */
			case 's':
				explain_flag = 0;
				break;
			case 'p':
				if (++l_num < argc) {
					prompt_string =
					    calloc(strlen(argv[l_num]),
					    sizeof(char));
					if (prompt_string == NULL)
						ed_exit(4);
					strcpy(prompt_string, argv[l_num]);
					prompt_str_flg = 1;
					break;
				}
				l_err = 1;
			case 'v':
#ifdef BSD
				(void)printf("ed: in BSD mode:\n");
#endif
#ifdef POSIX
				(void)printf("ed: in POSIX mode:\n");
#endif
				break;
			default:
				l_err++;
				ed_exit(l_err);
			}
			break;
		default:
			if (name_set)
				ed_exit(3);
			strcpy(l_fnametmp, argv[l_num]);
			filename_current = l_fnametmp;
			name_set = 1;
			if (prompt_str_flg)
				break;
			/* default ed prompt */
			prompt_string = (char *) calloc(3, sizeof(char));
			strcpy(prompt_string, "*");
			break;
		}
		l_num++;
	}

	start_up_flag = 1;
	cmd_loop(stdin, &errnum);
	/* NOTREACHED */
}

/*
 * The command loop. What the command is that the user has specified
 * is determined here. This is not just for commands coming from
 * the terminal but any standard i/o stream; see the global commands.
 * Some of the commands are handled within here (i.e. 'H') while most
 * are handled in their own functions (as called).
 */
void
cmd_loop(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	LINE *l_tempp;
	int l_last, l_jmp_flag;

	l_last = 0; /* value in l_last may be clobbered (reset to = 0) by longjump, but that's okay */

	if (g_flag == 0) {	/* big, BIG trouble if we don't check! think. */
		/* set the jump point for the signals */
		l_jmp_flag = setjmp(ctrl_position);
		signal(SIGINT, sigint_handler);
		signal(SIGHUP, sighup_handler);
		switch (l_jmp_flag) {
		case JMP_SET:
			break;
		/* Some general cleanup not specific to the jmp pt. */
		case INTERUPT:
			sigint_flag = 0;
			GV_flag = 0;	/* safest place to do these flags */
			g_flag = 0;
			(void)printf("\n?\n");
			break;
		case HANGUP:		/* shouldn't get here. */
			break;
		default:
			(void)fprintf(stderr, "Signal jump problem\n");
		}
		/* Only do this once! */
		if (start_up_flag) {
			start_up_flag = 0;
			/* simulate the 'e' at startup */
			e2(inputt, errnum);
			if (*errnum == 0)
				goto errmsg2;
		}
	}
	for (;;) {
		if (prompt_str_flg == 1)
			(void)printf("%s", prompt_string);
		ss = getc(inputt);
		*errnum = 0;
		l_tempp = Start = End = NULL;
		Start_default = End_default = 1;

		/*
		 * This isn't nice and alphabetical mainly because of
		 * restrictions with 'G' and 'V' (see ed(1)).
		 */
		for (;;) {
			switch (ss) {
			case 'd':
				d(inputt, errnum);
				break;
			case 'e':
			case 'E':
				e(inputt, errnum);
				if (*errnum == 0)
					goto errmsg2;
				break;
			case 'f':
				f(inputt, errnum);
				break;
			case 'a':
			case 'c':
			case 'i':
			case 'g':
			case 'G':
			case 'v':
			case 'V':
				if (GV_flag == 1) {
					(void)sprintf(help_msg,
					    "command `%c' illegal in G/V", ss);
					*errnum = -1;
					break;
				}
				switch (ss) {
				case 'a':
					a(inputt, errnum);
					break;
				case 'c':
					c(inputt, errnum);
					break;
				case 'i':
					i(inputt, errnum);
					break;
				default:
					g(inputt, errnum);
				}
				break;
			case 'h':
				if (rol(inputt, errnum))
					break;
				if (help_msg[0])
					(void)printf("%s\n", help_msg);
				*errnum = 1;
				break;
			case 'H':
				if (rol(inputt, errnum))
					break;
				if (help_flag == 0) {
					help_flag = 1;
					if (help_msg[0])
						(void)printf("%s\n",
						    help_msg);
				} else
					help_flag = 0;
				*errnum = 1;
				break;
			case 'j':
				j(inputt, errnum);
				break;
			case 'k':
				set_mark(inputt, errnum);
				break;
			case 'l':
				l(inputt, errnum);
				break;
			case 'm':
				m(inputt, errnum);
				break;
#ifdef POSIX
				/* In POSIX-land 'P' toggles the prompt. */
			case 'P':
				if (rol(inputt, errnum))
					break;
				prompt_str_flg = prompt_str_flg ? 0 : 1;
				*errnum = 1;
				break;
#endif
			case '\n':
				if (GV_flag == 1)
					return;
				/* For 'p' to consume. */
				ungetc(ss, inputt);
				if ((current == bottom) && (End == NULL)) {
					strcpy(help_msg, "at end of buffer");
					*errnum = -1;
					break;
				}
				current = current->below;
#ifdef BSD
				/* In BSD 'P'=='p'. */
			case 'P':
#endif
			case 'p':
				p(inputt, errnum, 0);
				break;
			case 'n':
				p(inputt, errnum, 1);
				break;
			/*
			 * An EOF means 'q' unless we're still in the middle
			 * of a global command, in which case it was just the
			 * end of the command list found.
			 */
			case EOF:
				clearerr(inputt);
				if (g_flag > 0)
					return;
				/*ss = 'q';*/
			case 'q':
			case 'Q':
				if ((!isatty(STDIN_FILENO)) && (ss == 'q'))
					ss = 'Q';
				q(inputt, errnum);
				break;
			case 'r':
				r(inputt, errnum);
				if (*errnum == 0)
					goto errmsg2;
				break;
			case 's':
				s(inputt, errnum);
				break;
			case 't':
				t(inputt, errnum);
				break;
			case 'u':
				u(inputt, errnum);
				break;
			case 'w':
			case 'W':
				w(inputt, errnum);
				break;
			case 'z':
				z(inputt, errnum);
				break;
			case '!':
				bang(inputt, errnum);
				break;
			case '=':
				equal(inputt, errnum);
				break;
			/*
			 * Control of address forms from here down.
			 *
			 * It's a head-game to understand why ";" and "," look
			 * as they do below, but a lot of it has to do with ";"
			 * and "," being special address pair forms themselves
			 * and the compatibility for address "chains".
			 */
			case ';':
				if (End_default == 1 && Start_default == 1) {
					Start = current;
					End = bottom;
					Start_default = End_default = 0;
				} else {
					Start = current = End;
					Start_default = 0;
					End_default = 1;
				}
				l_tempp = NULL;
				break;
			/*
			 * Note address ".,x" where x is a cmd is legal; not a
			 * bug - for backward compatability.
			 */
			case ',':
				if (End_default == 1 && Start_default == 1) {
					Start = top;
					End = bottom;
					Start_default = End_default = 0;
				} else {
					Start = End;
					Start_default = 0;
					End_default = 1;
				}
				l_tempp = NULL;
				break;
			case '%':
				if (End_default == 0) {
					strcpy(help_msg,
					    "'%' is an address pair");
					*errnum = -1;
					break;
				}
				Start = top;
				End = bottom;
				Start_default = End_default = 0;
				l_tempp = NULL;
				break;
			/*
			 * Within address_conv => l_last = '+', foobar, but
			 * historical and now POSIX...
			 */
			case ' ':
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '-':
			case '^':
			case '+':
			case '\'':
			case '$':
			case '?':
			case '/':
			case '.':
				ungetc(ss, inputt);
				if (Start_default == 0 && End_default == 0) {
					strcpy(help_msg,
					    "badly formed address");
					*errnum = -1;
					break;
				}
				ss = l_last;
				l_tempp = address_conv(l_tempp, inputt, errnum);
				if (*errnum < 0)
					break;
				End = l_tempp;
				End_default = 0;
				if (Start_default == 0)
					*errnum = address_check(Start, End);
				break;
			default:
				*errnum = -1;
				strcpy(help_msg, "unknown command");
				break;
			}	/* end-switch(ss) */

			/* Things came out okay with the last command. */
			if (*errnum > 0) {
				if (GV_flag == 1)
					return;
				/* Do the suffixes if there were any. */
				if (printsfx > 0) {
					Start = End = current;
					ungetc(ss, inputt);
					if (printsfx == 1)
						p(inputt, errnum, 0);
					else
						if (printsfx == 2)
							p(inputt, errnum, 1);
						else if (printsfx == 4)
							l(inputt, errnum);
					/* Unlikely it's needed, but... */
					if (*errnum < 0)
						goto errmsg;
				}
				break;
			}
			/* There was a problem with the last command. */
			else if (*errnum < 0) {
errmsg:				while (((ss = getc(inputt)) != '\n') &&
				    (ss != EOF));
				(void)printf("?\n");
errmsg2:			if (help_flag)
					(void)printf("%s\n", help_msg);
				exit_code = 4;
/* for people wanting scripts to carry on after a cmd error, then
 * define NOENDONSCRIPT on the compile line.
 */
#ifndef NOENDONSCRIPT
				if (!isatty(STDIN_FILENO)) {
					ss = 'Q';
					ungetc('\n', inputt);
					q(inputt, errnum);
				}
#endif
				break;
			}
			l_last = ss;
			ss = getc(inputt);
		}
	}
}

/*
 * Exits ed and prints an appropriate message about the command line
 * being malformed (see below).
 */
void
ed_exit(err)
	int err;
{
	switch (err) {
          case 1:
		(void)fprintf(stderr, "ed: illegal option\n");
		break;
          case 2:
		(void)fprintf(stderr, "ed: missing promptstring\n");
		break;
          case 3:
		(void)fprintf(stderr, "ed: too many filenames\n");
		break;
          case 4:
		(void)fprintf(stderr, "ed: out of memory error\n");
		break;
	  case 5:
		(void)fprintf(stderr, "ed: unable to create buffer\n");
		break;
          default:
		(void)fprintf(stderr, "ed: command line error\n");
		break;
        }
	(void)fprintf(stderr,
	    "ed: ed [ -s ] [ -p promptstring ] [ filename ]\n");
	exit(1);
}

/*
 * SIGINT is never turned off. We flag it happened and then pay attention
 * to it at certain logical locations in the code we don't do more here
 * cause some of our buffer pointer's may be in an inbetween state at the
 * time of the SIGINT. So we flag it happened, let the local fn handle it
 * and do a jump back to the cmd_loop
 */
static void
sigint_handler(signo)
	int signo;
{
	sigint_flag = 1;
	if (sigspecial3) {
		sigspecial3 = 0;
		SIGINT_ILACTION;
	}
	else
		if (sigspecial);
		else
			SIGINT_ACTION;
}

static void
sighup_handler(signo)
	int signo;
{
	sighup_flag = 1;
	undo();
	do_hup();
	/* NOTREACHED */

	SIGHUP_ACTION;
}
