#ifndef lint
static char sccsid[] = "@(#)vdfmt.c	1.4 (Berkeley/CCI) %G%";
#endif

/*
**
*/

#include	"vdfmt.h"

main()
{
	exdent(-1);
	print("VDFORMAT                   Version 3.0 \n\n");

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
	extern fmt_err	smd_decode_position(), smd_e_decode_position();
	extern bs_entry	smd_code_position(), smd_e_code_position();
	extern int	smd_cyl_skew(), smd_trk_skew();
	extern int	smd_e_cyl_skew(), smd_e_trk_skew();
	register int	ctlr, drive;

	/* Identify which controllers are present and what type they are. */
	num_controllers = 0;
	for(ctlr = 0; ctlr < MAXCTLR; ctlr++) {
		c_info[ctlr].addr = (struct vddevice *)(VDBASE+(ctlr*VDOFF));
		if(!badaddr(c_info[ctlr].addr, 2)) {
			printf("controller %d: ", ctlr);
			num_controllers++;
			c_info[ctlr].addr->vdreset = (unsigned)0xffffffff;
			DELAY(1000000);
			if(c_info[ctlr].addr->vdreset!=(unsigned)0xffffffff) {
				c_info[ctlr].alive = u_true;
				c_info[ctlr].type = VDTYPE_VDDC;
				c_info[ctlr].name = "VDDC";
				c_info[ctlr].decode_pos = smd_decode_position;
				c_info[ctlr].code_pos = smd_code_position;
				c_info[ctlr].cylinder_skew = smd_cyl_skew;
				c_info[ctlr].track_skew = smd_trk_skew;
				printf("vddc\n");
				DELAY(1000000);
			} else {
				c_info[ctlr].alive = u_true;
				c_info[ctlr].type = VDTYPE_SMDE;
				c_info[ctlr].name = "SMD-E";
				c_info[ctlr].addr->vdrstclr = 0;
				c_info[ctlr].decode_pos = smd_e_decode_position;
				c_info[ctlr].code_pos = smd_e_code_position;
				c_info[ctlr].cylinder_skew = smd_e_cyl_skew;
				c_info[ctlr].track_skew = smd_e_trk_skew;
				printf("smd-e\n");
				DELAY(3000000);
			}
		} else  {
			c_info[ctlr].alive = u_false;
			c_info[ctlr].type = -1;
		}
		for(drive=0; drive<MAXDRIVE; drive++) {
			d_info[ctlr][drive].alive = u_unknown;
			d_info[ctlr][drive].info = (struct vdconfig *)0;
		}
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
		for(drive=0; drive<MAXCTLR; drive++) {
			d_info[ctlr][drive].alive = u_unknown;
			d_info[ctlr][drive].info = (struct vdconfig *)0;
			d_info[ctlr][drive].id = -1;
			d_info[ctlr][drive].trk_size = 0;
			d_info[ctlr][drive].num_slip = 0;
			d_info[ctlr][drive].track_skew = 0;
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
	bad_map = (bs_map *)bs_map_space;
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
