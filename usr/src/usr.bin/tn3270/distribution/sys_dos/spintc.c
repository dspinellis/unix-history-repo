#define	LINT_ARGS

#include <stdio.h>
#include <dos.h>
#include <stdlib.h>

#include "../ntn3270/general.h"

#define	PSP_ENVIRONMENT		0x2c
#define	PSP_FCB1		0x5c
#define	PSP_FCB2		0x6c

#define	INTERRUPT_NUMBER	73

typedef struct {
    int
	environment,		/* Segment address of environment */
	cmd_ptr_offset,		/* Offset of command to execute */
	cmd_ptr_segment,	/* Segment where command lives */
	fcb1_ptr_offset,	/* Offset of FCB 1 */
	fcb1_ptr_segment,	/* Segment of FCB 1 */
	fcb2_ptr_offset,	/* Offset of FCB 2 */
	fcb2_ptr_segment;	/* Segment of FCB 2 */
} ExecList;

typedef struct {
    union REGS		regs;
    struct SREGS	sregs;
    int			int_no;	/* Which interrupt to wait on */
    int			done;	/* Are we done, or just took an interrupt? */
    int			rc;	/* return code */
} Spawn;


void
do_spawn(command, spawn)
char *command;
Spawn *spawn;
{
    ExecList mylist;
    char *comspec;
    void int_spawn();
    int int_offset, int_segment;

    /*
     * Get comspec.
     */
    comspec = getenv("COMSPEC");
    if (comspec == 0) {			/* Can't find where command.com is */
	fprintf(stderr, "Unable to find COMSPEC in the environment.");
	spawn->done = 1;
	spawn->rc = 99;	/* XXX */
	return;
    }

    /*
     * Now, hook up our interrupt routine.
     */

    spawn->regs.h.ah = 0x35;
    spawn->regs.h.al = spawn->int_no;
    intdosx(&spawn->regs, &spawn->regs, &spawn->sregs);

    /* Save old routine */
    int_offset = spawn->regs.x.bx;
    int_segment = spawn->sregs.es;

    spawn->regs.h.ah = 0x25;
    spawn->regs.h.al = spawn->int_no;
    spawn->regs.x.dx = (int) int_spawn;
    segread(&spawn->sregs);
    spawn->sregs.ds = spawn->sregs.cs;
    intdosx(&spawn->regs, &spawn->regs, &spawn->sregs);

    /*
     * Read in segment registers.
     */

    segread(&spawn->sregs);

    /*
     * Set up registers for the EXEC call.
     */

    spawn->regs.h.ah = 0x4b;
    spawn->regs.h.al = 0;
    spawn->regs.x.dx = (int) comspec;
    spawn->sregs.es = spawn->sregs.ds;		/* Superfluous, probably */
    spawn->regs.x.bx = (int) &mylist;

    /*
     * Set up EXEC parameter list.
     */

    ClearElement(mylist);
    mylist.cmd_ptr_offset = (int) command;
    mylist.cmd_ptr_segment = spawn->sregs.ds;
    mylist.fcb1_ptr_offset = PSP_FCB1;
    mylist.fcb1_ptr_segment = _psp;
    mylist.fcb2_ptr_offset = PSP_FCB2;
    mylist.fcb2_ptr_segment = _psp;
    mylist.environment = *((int far *)(((long)_psp<<16)|PSP_ENVIRONMENT));

    /*
     * Call to assembly language routine to actually set up for
     * the spawn.
     */

    start_spawn(spawn);
    spawn->done = 1;			/* XXX */

    if (spawn->done == 0) {
	return;				/* Not done yet */
    }

    if (spawn->regs.x.cflag) {
	fprintf(stderr, "0x%x return code from EXEC.\n", spawn->regs.x.ax);
	spawn->done = 1;
	spawn->rc = 99;
	return;
    }

    spawn->regs.h.ah = 0x4d;			/* Get return code */

    intdos(&spawn->regs, &spawn->regs);

    spawn->rc = spawn->regs.x.ax;

    /*
     * Restore old interrupt handler.
     */

    spawn->regs.h.ah = 0x25;
    spawn->regs.h.al = spawn->int_no;
    spawn->regs.x.dx = int_offset;
    spawn->sregs.ds = int_segment;
    intdosx(&spawn->regs, &spawn->regs, &spawn->sregs);
}

main(argc, argv, envp)
int	argc;				/* Number of passed arguments */
char	*argv[];			/* Arguments passed */
char	*envp[];			/* Inherited environment */
{
    Spawn spawned;
    static char command[256];

    ClearElement(spawned);
    spawned.int_no = INTERRUPT_NUMBER;
    if (argc == 1) {
	command[0] = 0;
    } else {
	char *cmdptr;
	int length;

	argc--;
	argv++;
	strcpy(command, " /c");
	cmdptr = command+strlen(command);
	while (argc) {
	    if ((cmdptr+strlen(*argv)) >= (command+sizeof command)) {
		fprintf(stderr, "Argument list too long at argument *%s*.\n",
			    *argv);
		return 0;
	    }
	    *cmdptr++ = ' ';		/* Blank separators */
	    strcpy(cmdptr, *argv);
	    cmdptr += strlen(cmdptr);
	    argc--;
	    argv++;
	}
	length = strlen(command)-1;
	if (length < 0) {
	    length = 0;
	}
	command[0] = length;
    }

    /*
     * do_spawn returns when either the command has finished, or when
     * the required interrupt comes in.  In the latter case, the appropriate
     * thing to do is to process the interrupt, and then return to
     * the interrupt issuer.
     */
    do_spawn(command, &spawned);
    if (spawned.done == 0) {
	/* Process request */
	switch (spawned.regs.h.ah) {
	case 1:			/* Add */
	    spawned.regs.x.cx += spawned.regs.x.dx;
	    break;
	case 2:			/* Subtract */
	    spawned.regs.x.cx -= spawned.regs.x.dx;
	    break;
	case 3:			/* Multiply */
	    spawned.regs.x.cx *= spawned.regs.x.dx;
	    break;
	case 4:			/* Divide */
	    spawned.regs.x.cx /= spawned.regs.x.dx;
	    break;
	default:
	    spawned.regs.h.al = -1;	/* Error */
	    spawned.regs.x.cflag = 1;
	    break;
	}
	continue_spawn(&spawned);
	/*NOTREACHED*/
	/* continue_spawn() causes an eventual return from do_spawn. */
    }
    if (spawned.rc != 0) {
	fprintf(stderr, "Process generated a return code of 0x%x.\n",
								spawned.rc);
    }
}
