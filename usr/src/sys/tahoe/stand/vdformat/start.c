#ifndef lint
static char sccsid[] = "@(#)start.c	1.1 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"

/*
**
*/

start_commands()
{
	register int	ctlr, drive, cur_op;

	indent();
	for(ctlr=0; ctlr<MAXCTLR; ctlr++)
		for(drive=0; drive<MAXDRIVE; drive++) {
			for(cur_op=0; cur_op<NUMOPS; cur_op++) {
				if(ops_to_do[ctlr][drive].op & (1<<cur_op)) {
					cur.controller = ctlr;
					cur.drive = drive;
					if(!_setjmp(abort_environ)) {
						load_verify_patterns();
						spin_up_drive();
						(*operations[cur_op].routine)();
					}
					ops_to_do[ctlr][drive].op&=~(1<<cur_op);
				}
			}
		}
	exdent(1);
}

