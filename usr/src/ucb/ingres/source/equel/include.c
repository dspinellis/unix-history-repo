# include	<stdio.h>
# include	"constants.h"
# include	"globals.h"
# include	<sccs.h>

SCCSID(@(#)include.c	7.1	2/5/81)



/*
**  TST_INCLUDE -- Test and process a "#include" line
**
**	Checks that a line is of the form '# include "<filename.q.h>"',
**	and calls include to change i/o files if so.
**
**	Returns:
**		1 -- if change of file was done
**		0 -- otherwise
*/


tst_include()
{
	register char	*lp, *tp, *chp;
	char		temp [MAXSTRING + 1];

	/* copy line read into temp ( because temp is same
	 * size as Linebuf, no overflow check is needed 
	 */
	for (tp = temp, lp = Line_buf; (*tp++ = *lp++) != '\n'; )
		;
	/* skip white space between "#" and "include" */
	for (tp = &temp [1]; *tp == ' ' || *tp == '\t'; tp++)
		;
	if (scompare("include", 7, tp, 7))
		return (0);
	/* skip "include", the white space till '"' */
	for (tp = &tp [7]; *tp == ' ' || *tp == '\t'; tp++)
		;
	if (*tp++ != '"')
		return (0);
	
	/* {tp points to X in a line "#<white>include<white>"X..." }
	 * make "lp" point at right end '"' (filename must not have
	 * escaped '"' in it
	 */
	for (lp = tp; *lp != '"' && *lp != '\n'; lp++)
		;
	
	if (*lp != '"')
		return (0);
	/* make sure filename is long enough to have the equel "include"
	 * suffix ".q.h"
	 */
	if (lp - tp < 4)
		return (0);
	if (scompare(".q.h", 4, &lp [-4], 4))
		return (0);	/* other include (non-equel) */

	/* "chp" points at 'q' in '.q.h' which will be changed to 'c' */
	chp = &lp [-3];

	/* check that rest of the line is white space */
	for (lp++; *lp == ' ' || *lp == '\t'; lp++)
		;
	if (*lp != '\n')
	{
		*lp = '\0';
		yysemerr("garbage after valid \"#include\"", temp);
		return (0);
	}
	*++lp = '\0';
	return (include(temp, tp, chp, &chp [3]));
}
/*
**  INCLUDE -- Change i/o files for a file inclusion
**	Saves status of current i/o files, puts out 
**	'#include"___.c.h"', and opens appropriate files.
**	Makes both files legal C files, closing quotes,
**	and reopeneing them in the new file, if necessary.
**
**	Parameters:
**		buf -- "#include..." line
**		start -- start of filename
**		chp -- *chp is 'q' in ".q.h" of filename
**		end -- ptr to last '"' after filename
**
**	Returns:
**		1 -- if i/o files changed
**		0 -- otherwise
**
**	Called By:
**		tst_include() -- [include.c] on seeing a pre-processor
**		line.
*/


include(buf, start, chp, end)
char	*buf;
char	*start;
char	*chp;
char	*end;
{
	char				in_q_flag;
	register struct inc_file	*i_f;
	char				*salloc();

	in_q_flag = In_quote;
	if (in_q_flag)
	{
		end_quote();
		equate_lines();
	}

	if (!(i_f = (struct inc_file *)nalloc(sizeof *i_f)))
	{
err1 :
		*++end = '\0';	/* overwrite before new-line at end */
		yysemerr("alloc error in #include processing", buf);
		if (in_q_flag)
			begin_quote();
		*end = '\n';
		return (0);
	}
	i_f->inc_yyline = yyline + 1;	/* next line that will be read is
					 * yyline + 1 because getch does not
					 * see the '\n' at the end of the
					 * "#include" line
					 */
	i_f->inc_lineout = Lineout + 1;	/* because the write of the "#include
					 * "... .c.h" file is done after the
					 * fixation of the value of Lineout.
					 */
	i_f->inc_fid = Input_file_name;
	i_f->inc_outfile = Out_file;
	i_f->inc_infile = In_file;
	i_f->inc_next = 0;

	*end = '\0';
	if (!(Input_file_name = salloc(start)))
	{
		xfree(i_f);
		goto err1;
	}

	if ((In_file = fopen(Input_file_name, "r")) == NULL)
	{
		printf("can't read included file \"%s\"\n", Input_file_name);
err3 :
		xfree(Input_file_name);
		Input_file_name = i_f->inc_fid;
		In_file = i_f->inc_infile;
		xfree(i_f);
		return (0);
	}
	*end = '"';
	*chp = 'c';

	/* write out "#include..c.h" line (make sure it's at beginning
	 * of line.
	 */
	equate_lines();
	if (Charcnt != 0)
		w_op("\n");
	w_op(buf);
	fflush(Out_file);
	*end = '\0';
	if ((Out_file = fopen(start, "w")) == NULL)
	{
		printf("can't write file \"%s\"\n", start);
		fclose(In_file);
		Out_file = i_f->inc_outfile;
		goto err3;
	}

	/* go to it !!! */
	Lineout = yyline = Newline = 1;
	Pre_proc_flg = 0;
	Line_pos = 0;
	if (in_q_flag)
		begin_quote();
	Inc_files = i_f;
	return (1);
}
/*
**  RESTOREF -- Restore previous file environment
**	Closes current files, and restores global variable
**	values for old files.
**
**	Returns:
**		0 -- if no old files (top level, no "#include"s)
**		1 -- otherwise
**
**	Called By:
**		equel() -- to close any files that may remain
**			open after a reset(III).
**		getch() -- upon receiving end-of-file
**			at some include level.
*/


restoref()
{
	register struct inc_file	*i_f;

	if (Inc_files)
	{
		fclose(Out_file);
		fclose(In_file);
		xfree(Input_file_name);

		/* restore previous environment */
		i_f = Inc_files;
		yyline = i_f->inc_yyline;
		Lineout = i_f->inc_lineout;
		Input_file_name = i_f->inc_fid;
		Out_file = i_f->inc_outfile;
		In_file = i_f->inc_infile;
		Line_pos = 0;
		Inc_files = i_f->inc_next;
		xfree(i_f);
		return (1);
	}
	return (0);
}
