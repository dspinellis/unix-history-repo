/*	interact.c	1.8	83/07/27	*/
#include <stdio.h>
#include "command.h"
#include <setjmp.h>
#include <signal.h>

#define errinp { printf("??\n"); break; }

jmp_buf	env;
int	firsttime = 0;

interact()

{

	int	i, intrpt();
	char	cmd[80], *rest, *index();

	for (;;) {
		if (firsttime++ == 0) {
			signal(SIGINT, intrpt, -1);
			(void) setjmp(env);
		}
		if (cmdinp(cmd) < 0)
			return (0);
		rest = index(cmd, ' ');
		if (rest)
			*rest++ = '\0';
		i = chkcmd(cmd);
#ifdef DEBUG
		printf("command: %s, ind: %d\n", cmd, i);
#endif

		switch (i) {
		default:
			errinp;
			break;
		case CMD_DIR:	
		case CMD_LS:
			dispdir();
			break;

		case CMD_RENAME:
			rename(rest);
			break;

		case CMD_OCOPY:
			copyc(rest, 0);
			break;

		case CMD_ICOPY:
			pip(rest, 0);
			break;

		case CMD_DELETE:
		case CMD_ERASE:
			delete(rest);
			break;

		case CMD_EXIT:
		case CMD_LOGOUT:
			return(0);

		case CMD_TYPE:
			copy(rest, stdout, 0);
			break;

		case CMD_HELP:
			help();
			break;

		case CMD_OCCOPY:
			copyc(rest, 1);
			break;

		case CMD_ICCOPY:
			pip(rest,1);
			break;

		case CMD_DUMP:
			dump(rest);
			break;

		}
	}
}

/*
 * handle interrupts (in interactive mode only),
 * just (long)jump back to command input mode
 */

intrpt()
{
	firsttime = 0;
	printf("\n");
	longjmp(env, 0);
}
