/*
**  RESP.H -- response blocks
**
**	Version:
**		@(#)resp.h	7.1	2/5/81
*/

struct resp
{
	short	resp_resp;	/* the function value */
	char	resp_active;	/* > 0 if in use */
	long	resp_time;	/* elapsed time */
	long	resp_tups;	/* count of tuples touched */
	long	resp_pread;	/* pages read */
	long	resp_pwrit;	/* pages written */
	/* PARM	resp_rval;	/* the module return value */
};

extern struct resp	Resp;	/* the current response */
