#ifndef lint
static char sccsid[] = "@(#)proc_cmd.c	1.4 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

#define	RESET		1
#define	LIST		2
#define	DELETE		3
#define	FORMAT		4
#define	VERIFY		5
#define	RELOCATE	6
#define	CORRECT		7
#define	INFO		8
#define	PROFILE		9
#define	EXERCISE	10
#define	START		11
#define	EXIT		12

static cmd_text_element	commands[] = {
	{ RESET,     "RESET",	  "Reinitialize VDFORMAT, and start over" },
	{ EXIT,      "EXIT",	  "Terminate program" },
	{ LIST,	     "List",	  "List operations specified so far" },
	{ DELETE,    "Delete",	  "Delete specific operations" },
	{ FORMAT,    "Format",	  "Format and verify disk surface" },
	{ VERIFY,    "Verify",	  "Destructively verify disk surface" },
	{ RELOCATE,  "Relocate",  "Add known flaws to bad sector map" },
	{ CORRECT,   "Correct",	  "Correct erroneous relocations or drive ID" },
	{ INFO,	     "Info",	  "Display known disk information" },
	{ PROFILE,   "Profile",   "Display seek profile graph of disk" },
	{ EXERCISE,  "Exercise",  "Perform seek exercises on disk" },
	{ START,     "STARt",	  "Start operations" },
	{ 0,	     "",	  "" }
};


/*
**
*/

process_commands()
{
	int	tokens[20];
	int	*tok_ptr, count;
	int	op_mask = 0;
	char	*cptr;
	boolean	should_start = false;

	for(;;) {
		(void)_setjmp(abort_environ);
		cur.state = cmd;
		kill_processes = false;
		exdent(-1);
		op_mask = 0;
		printf("vdformat> ");
		count = get_text_cmd(commands, tokens);
		if(kill_processes == true)
			_longjmp(quit_environ, 1);
		tok_ptr = tokens;
		if((*tok_ptr == 0) || !count)
			continue;
		while(*tok_ptr) {
			switch (*tok_ptr) {
			case RESET :
				reset();
				break;
			case LIST :
				list();
				break;
			case DELETE :
				delete();
				break;
			case FORMAT :
				op_mask |= FORMAT_OP;
				break;
			case VERIFY :
				op_mask |= VERIFY_OP;
				break;
			case RELOCATE :
				op_mask |= RELOCATE_OP;
				break;
			case CORRECT :
				op_mask |= CORRECT_OP;
				break;
			case INFO :
				op_mask |= INFO_OP;
				break;
			case PROFILE :
				op_mask |= PROFILE_OP;
				break;
			case EXERCISE :
				op_mask |= EXERCISE_OP;
				break;
			case START :
				should_start = true;
				break;
			case EXIT:
				exit(0);
				/*NOTREACHED*/
			default:		/* ignore */
				break;
			}
		tok_ptr++;
		}
		if(op_mask) {
			get_drive_parameters(op_mask);
		}
		if(should_start) {
			start_commands();
			should_start = false;
		}
	}
}


/*
**
*/

static boolean	header_printed = false;

get_drive_parameters(op_mask)
int	op_mask;
{
	int	c_list[20], i, num_pat;

	indent();
	header_printed = false;
	get_ctlr_list(c_list, op_mask);
	if(kill_processes == true) {
		kill_processes = false;
		c_list[0]= -1;
	}
	for(i=0; c_list[i] != -1; i++) {
		int	d_list[40], j;

		indent();
		get_drive_list(c_list[i], d_list, op_mask);
		if(kill_processes == true) {
			kill_processes = false;
			break;
		}
		indent();
		if(op_mask & (FORMAT_OP | VERIFY_OP)) {
			num_pat = get_num_pat();
			if(kill_processes == true) {
				kill_processes = false;
				break;
			}
		}
		for(j=0; d_list[j] != -1; j++) {
			cur.controller = c_list[i];
			cur.drive = d_list[j];
			C_INFO = &c_info[cur.controller];
			D_INFO = &d_info[cur.controller][cur.drive];
			lab = &D_INFO->label;
			get_drive_type(cur.controller, cur.drive, op_mask);
			if(kill_processes == true) {
				kill_processes = false;
				break;
			}
			if(op_mask & ~INFO_OP) {
				indent();
				get_drive_id(c_list[i], d_list[j]);
				if(kill_processes == true) {
					kill_processes = false;
					break;
				}
				exdent(1);
			}
			ops_to_do[c_list[i]][d_list[j]].op |= op_mask;
			if(op_mask & (FORMAT_OP | VERIFY_OP))
				ops_to_do[c_list[i]][d_list[j]].numpat=num_pat;
		}
		exdent(1);
	}
	exdent(2);
}

/*
**
*/

get_ctlr_list(c_list, op_mask)
int	*c_list, op_mask;
{
	extern int	ctlr_help();
	register int	i, ctlr;
	int		table[MAXCTLR+10];

	i = 0;
	for(ctlr=0; ctlr<MAXCTLR; ctlr++)
		if(c_info[ctlr].alive == u_true)
			table[i++] = ctlr;
	table[i] = -1;
	/* If only one controller is possible don't ask */
	if(table[1] == -1) {
		*c_list++ = table[0];
		*c_list = -1;
		return;
	}
	for(;;) {
		header_printed = true;
		print("");  /* Force indent */
		print_op_list(op_mask);
		printf(" on which controllers? ");
		get_digit_list(c_list, table, ctlr_help);
		if(kill_processes == true)
			return;
		if(*c_list != -1)
			break;
	}
}


/*
**
*/

ctlr_help()
{
	register int	ctlr;

	indent();
	print("The following controllers are attached to the system:\n");
	indent();
	for(ctlr=0; ctlr<MAXCTLR; ctlr++)
		if(c_info[ctlr].alive == u_true) {
			print("Controller %d, which is a%s %s controller.\n",
			    ctlr, (c_info[ctlr].name[0] == 'S') ? "n" : "",
			    c_info[ctlr].name);
		}
	print("\n");
	exdent(2);
}

static int	max_drive = 0;

/*
**
*/

get_drive_list(ctlr, d_list, op_mask)
int	ctlr, *d_list, op_mask;
{
	extern int	drive_help();
	int		table[MAXDRIVE+10];
	int		i;

	max_drive = (c_info[ctlr].type == VDTYPE_VDDC) ? 4 : 16;
	for(i=0; i<max_drive; i++)
		table[i] = i;
	table[i] = -1;
	for(;;) {
		if(header_printed == true)
			print("Drives on controller %d? ", ctlr);
		else {
			header_printed = true;
			print("");  /* Force indent */
			print_op_list(op_mask);
			printf(" on which drives? ");
		}
		get_digit_list(d_list, table, drive_help);
		if(kill_processes == true)
			return;
		if(*d_list != -1)
			break;
	}
}

/*
**
*/

id_help()
{
	indent();
	print("The following commands are available:\n");
	indent();
	print("STATus - Display formatter state.\n");
	print("QUIT   - Terminate current operation.\n");
	print("");
	print("A module serial can be any number greater than zero.\n");
	exdent(2);
}


/*
**
*/

get_drive_id(ctlr, drive)
int	ctlr, drive;
{
	int	new_id;

	for(;;) {
		print("Module serial number for controller %d, drive %d? ",
		    ctlr, drive);
		if(d_info[ctlr][drive].id != -1)
			printf("(%d) ", d_info[ctlr][drive].id);
		new_id = get_digit_cmd(id_help);
		if(new_id > 0) {
			d_info[ctlr][drive].id = new_id;
			break;
		}
		else if(d_info[ctlr][drive].id != -1) 
			break;
	}
}


/*
**
*/

drive_help()
{
	indent();
	print("Drive numbers 0 through %d may be entered.\n", max_drive-1);
	exdent(1);
}


/*
**
*/

pat_help()
{
	indent();
	print("Between 0 and 16 patterns may be used while verifying.\n");
	exdent(1);
}


/*
**
*/

get_num_pat()
{
	int	table[17+10];
	int	results[17+10];
	int	i;

	for(i=0; i<=16; i++)
		table[i] = i;
	table[i] = -1;
	for(;;) {
		print("Number of patterns to use while verifying? ");
		get_digit_list(results, table, pat_help);
		if(kill_processes == true)
			return 0;
		if(results[0] != -1)
			break;
	}
	return results[0];
}

