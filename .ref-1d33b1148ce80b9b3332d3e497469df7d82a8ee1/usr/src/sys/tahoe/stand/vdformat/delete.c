#ifndef lint
static char sccsid[] = "@(#)delete.c	1.1 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

/*
**
*/
static int	line = 0;


delete_help()
{
	indent();
	print("The following commands are available:\n");
	indent();
	print("STATus - Display formatter state.\n");
	print("QUIT   - Terminate current operation.\n");
	print("");
	print("Any line number between 0 and %d may be entered.\n", line-1);
	print("");
	exdent(2);
}


delete()
{
	register int	ctlr, drive;
	int		*table[(MAXCTLR * MAXDRIVE) + 50];
	int		list[(MAXCTLR * MAXDRIVE) + 50];
	int		tokens[(MAXCTLR * MAXDRIVE) + 50];
	int		*tok;

	indent();
	indent();
	line = 0;
	for(ctlr=0; ctlr<MAXCTLR; ctlr++)
		for(drive=0; drive<MAXDRIVE; drive++)
			if(ops_to_do[ctlr][drive].op) {
				table[line] = &ops_to_do[ctlr][drive].op;
				list[line] = line;
				print("%d) ", line++);
				display_operations(ctlr, drive);
			}
	list[line] = -1;
	exdent(1);
	if(line > 1) {
		for(;;) {
			print("Delete line? ");
			get_digit_list(tokens, list, delete_help);
			if(kill_processes == true) {
				kill_processes = false;
				break;
			}
			indent();
			tok = tokens;
			for(tok=tokens; *tok != -1; tok++) {
				if(*table[*tok] != 0) {
					print("Line %d has been deleted.\n",*tok);
					*table[*tok] = 0;
				}
			}
			exdent(1);
		}
	}
	else if(line == 1) {
		print("Line 0 deleted since it was the only line possible.\n");
		*table[0] = 0;
	}
	else {
		print("Nothing to delete.\n");
	}
	exdent(1);
}
