#ifndef lint
static char sccsid[] = "@(#)vdfmt.c	1.7 (Berkeley/CCI) 6/24/90";
#endif

/*
**
*/

#include	"vdfmt.h"

main()
{
	exdent(-1);
	print("VDFORMAT            Berkeley Version 1.7 \n\n");

	for(;;) {
		determine_controller_types();
		print(
		  "\nType `Help' for help, `Start' to execute operations.\n\n");
		if(!_setjmp(reset_environ)) {
			init_environment();
			for(;;) {
				if(!_setjmp(quit_environ)) {
					reset_operation_tables();
					process_commands();
				}
				else
					report_unexecuted_ops();
			}
		}
	}
}


/*
**
*/

report_unexecuted_ops()
{
	register int	ctlr, drive;
	char *header = "The following operations will not be executed:\n";

	indent();
	for(ctlr=0; ctlr<MAXCTLR; ctlr++)
		for(drive=0; drive<MAXDRIVE; drive++)
			if(ops_to_do[ctlr][drive].op) {
				print(header);
				if(strlen(header)) {
					indent();
					header = "";
					print(header);
				}
				display_operations(ctlr, drive);
				ops_to_do[ctlr][drive].op = 0;
			}
	exdent(-1);
}


/*
**
*/
#define	VDBASE	0xffff2000	/* address of first controller */
#define	VDOFF	0x100		/* offset between controllers */

determine_controller_types()
{
	extern fmt_err	*smd_decode_position(), *smd_e_decode_position();
	extern bs_entry	*smd_code_position(), *smd_e_code_position();
	register int	ctlr, drive;
	register ctlr_info *ci;

	/* Identify which controllers are present and what type they are. */
	num_controllers = 0;
	for(ctlr = 0; ctlr < MAXCTLR; ctlr++) {
		ci = &c_info[ctlr];
		ci->addr = (struct vddevice *)(VDBASE+(ctlr*VDOFF));
		if(!badaddr(ci->addr, 2)) {
			printf("controller %d: ", ctlr);
			num_controllers++;
			ci->addr->vdreset = (unsigned)0xffffffff;
			DELAY(1000000);
			if(ci->addr->vdreset!=(unsigned)0xffffffff) {
				ci->alive = u_true;
				ci->type = VDTYPE_VDDC;
				ci->name = "VDDC";
				ci->decode_pos = smd_decode_position;
				ci->code_pos = smd_code_position;
				printf("vddc\n");
				DELAY(1000000);
			} else {
				ci->alive = u_true;
				ci->type = VDTYPE_SMDE;
				ci->name = "SMD-E";
				ci->addr->vdrstclr = 0;
				ci->decode_pos = smd_e_decode_position;
				ci->code_pos = smd_e_code_position;
				printf("smd-e\n");
				DELAY(3000000);
			}
		} else  {
			ci->alive = u_false;
			ci->type = -1;
		}
		for(drive=0; drive<MAXDRIVE; drive++)
			d_info[ctlr][drive].alive = u_unknown;
	}
	if(num_controllers == 0)
		_stop("vdfmt: I can't find any disk controllers.  Giving up!");
}


/*
**	Init_environment is used to reset everything to it's initial state.
** All previously stored drive information is lost when this command
** is executed.
*/

init_environment()
{
	register int	ctlr, drive;

	/* clear list of operations to do */
	for(ctlr=0; ctlr<MAXCTLR; ctlr++) {
		for(drive=0; drive<MAXDRIVE; drive++) {
			bzero((char *)&d_info[ctlr][drive],
			    sizeof(d_info[ctlr][drive]));
			d_info[ctlr][drive].alive = u_unknown;
			d_info[ctlr][drive].id = -1;
		}
	}
	/* Init pattern table pointers */
	pattern_address[0] = pattern_0;
	pattern_address[1] = pattern_1;
	pattern_address[2] = pattern_2;
	pattern_address[3] = pattern_3;
	pattern_address[4] = pattern_4;
	pattern_address[5] = pattern_5;
	pattern_address[6] = pattern_6;
	pattern_address[7] = pattern_7;
	pattern_address[8] = pattern_8;
	pattern_address[9] = pattern_9;
	pattern_address[10] = pattern_10;
	pattern_address[11] = pattern_11;
	pattern_address[12] = pattern_12;
	pattern_address[13] = pattern_13;
	pattern_address[14] = pattern_14;
	pattern_address[15] = pattern_15;
	/* Init operations command table */
	operations[0].routine = format;
	operations[0].op_name = "Format";
	operations[0].op_action = "Formatting";
	operations[1].routine = verify;
	operations[1].op_name = "Verify";
	operations[1].op_action = "Verification";
	operations[2].routine = relocate;
	operations[2].op_name = "Relocate";
	operations[2].op_action = "Relocation";
	operations[3].routine = info;
	operations[3].op_name = "Info";
	operations[3].op_action = "Information gathering";
	operations[4].routine = correct;
	operations[4].op_name = "Correct";
	operations[4].op_action = "Correction";
	operations[5].routine = profile;
	operations[5].op_name = "Profile";
	operations[5].op_action = "Profiling";
	operations[6].routine = exercise;
	operations[6].op_name = "Exercise";
	operations[6].op_action = "exercising";
	bad_map = &norm_bad_map;
}


/*
**	Reset_operation_tables reinitializes all the  tables that
**  control the sequence of formatter operations.
*/

reset_operation_tables()
{
	register int	ctlr, drive;

	/* clear list of operations to do */
	for(ctlr=0; ctlr<MAXCTLR; ctlr++) {
		for(drive=0; drive<MAXDRIVE; drive++) {
			ops_to_do[ctlr][drive].op = 0;
			ops_to_do[ctlr][drive].numpat = 1;
		}
	}
	kill_processes = false;
}
