/*
**  PROC.H -- process descriptors
**
**	Version:
**		@(#)proc.h	7.1	2/5/81
*/

# ifndef CM_MAXPROC


# define	CM_MAXPROC	10	/* maximum # of procs */

typedef struct
{
	char	pr_stat;	/* status byte for this proc, see below */
	char	pr_file;	/* file descriptor to get to this proc */
	char	pr_ninput;	/* new cm_input after writing to this proc */
}  proc_t;

# define	PR_BCAST	00001	/* write broadcasts on this pipe */
# define	PR_RADJCT	00002	/* adjacent on read pipe */
# define	PR_WADJCT	00004	/* adjacent on write pipe */


# endif CM_MAXPROC
