/* File name wild card expansion for VMS.
   This file is part of the etags program.
   Copyright (C) 1987 Free Software Foundation, Inc. 3 Feb 1987
     
This program is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.
     
   Permission is granted to anyone to distribute verbatim copies
   of this program's source code as received, in any medium, provided that
   the copyright notice, the nonwarraty notice above
   and this permission notice are preserved,
   and that the distributor grants the recipient all rights
   for further redistribution as permitted by this notice,
   and informs him of these rights.
     
   Permission is granted to distribute modified versions of this
   program's source code, or of portions of it, under the above
   conditions, plus the conditions that all changed files carry
   prominent notices stating who last changed them and that the
   derived material, including anything packaged together with it and
   conceptually functioning as a modification of it rather than an
   application of it, is in its entirety subject to a permission
   notice identical to this one.
     
   Permission is granted to distribute this program (verbatim or
   as modified) in compiled or executable form, provided verbatim
   redistribution is permitted as stated above for source code, and
    A.  it is accompanied by the corresponding machine-readable
      source code, under the above conditions, or
    B.  it is accompanied by a written offer, with no time limit,
      to distribute the corresponding machine-readable source code,
      under the above conditions, to any one, in return for reimbursement
      of the cost of distribution.   Verbatim redistribution of the
      written offer must be permitted.  Or,
    C.  it is distributed by someone who received only the
      compiled or executable form, and is accompanied by a copy of the
      written offer of source code which he received along with it.
     
   Permission is granted to distribute this program (verbatim or as modified)
   in executable form as part of a larger system provided that the source
   code for this program, including any modifications used,
   is also distributed or offered as stated in the preceding paragraph.
     
In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */

#include	<stdio.h>
typedef	char	tbool;

/* This is a BUG!  ANY arbitrary limit is a BUG!
   Won't someone please fix this?  */
#define	MAX_FILE_SPEC_LEN	255
typedef struct	{
  short   curlen;
  char    body[MAX_FILE_SPEC_LEN + 1];
} vspec;
#define	EOS	'\0'
#define	NO	0
#define	YES	1
#define	NULL	0

/* v1.01 nmm 19-Aug-85 gfnames - return in successive calls the 
 name of each file specified by all the remaining args in the command-line
 expanding wild cards and 
 stepping over arguments when they have been processed completely
*/
char*
gfnames(pac, pav, p_error)
	int	*pac;
	char	**pav[];
	tbool	*p_error;
{
	static	vspec	filename = {MAX_FILE_SPEC_LEN, "\0"};
	short	fn_exp();

	while (1)
		if (*pac == 0)
			{
			*p_error = NO;
			return(NULL);
			}
		else switch(fn_exp(&filename, **pav))
			{
		case 1:
			*p_error = NO;
			return(filename.body);
			break;
		case 0:
			--*pac;
			++*pav;
			break;
		default:
			*p_error = YES;
			return(filename.body);
			break;
			}
				
}

/* v1.05 nmm 26-Jun-86 fn_exp - expand specification of list of file names
 returning in each successive call the next filename matching the input
 spec. The function expects that each in_spec passed
 to it will be processed to completion; in particular, up to and
 including the call following that in which the last matching name
 is returned, the function ignores the value of in_spec, and will
 only start processing a new spec with the following call. 
 If an error occurs, on return out_spec contains the value
 of in_spec when the error occurred.

 With each successive filename returned in out_spec, the
 function's return value is one. When there are no more matching
 names the function returns zero. If on the first call no file
 matches in_spec, or there is any other error, -1 is returned. 
*/

#include	<rmsdef.h>
#include	<descrip.h>
#define		OUTSIZE	MAX_FILE_SPEC_LEN
short
fn_exp(out, in)
	vspec	*out;
	char	*in;
{
	static	long	context = 0;
	static	struct	dsc$descriptor_s	o;
	static	struct	dsc$descriptor_s	i;
	static	tbool	pass1 = YES;
		long	status;
		short	retval;

	if (pass1)
		{
		pass1 = NO;
		o.dsc$a_pointer = out;
		o.dsc$w_length = (short)OUTSIZE;
		i.dsc$a_pointer = in;
		i.dsc$w_length = (short)strlen(in);
		i.dsc$b_dtype = DSC$K_DTYPE_T;
		i.dsc$b_class = DSC$K_CLASS_S;
		o.dsc$b_dtype = DSC$K_DTYPE_VT;
		o.dsc$b_class = DSC$K_CLASS_VS;
		}
	if ( (status = lib$find_file(&i, &o, &context, 0, 0)) == RMS$_NORMAL)
		{
		out->body[out->curlen] = EOS;
		return(1);
		}
	else if (status == RMS$_NMF)
		retval = 0;
	else
		{
		strcpy(out->body, in);
		retval = -1;
		}
	lib$find_file_end(&context);
	pass1 = YES;
	return(retval);
}	

#ifndef OLD  /* Newer versions of VMS do provide `system'.  */
system(cmd)
     char *cmd;
{
  fprintf(stderr, "system() function not implemented under VMS\n");
}
#endif

#define	VERSION_DELIM	';'
char *massage_name(s)
	char	*s;
{
	char	*start = s;	

	for ( ; *s; s++)
		if (*s == VERSION_DELIM)
			{
			*s = EOS;
			break;
			}
		else
			*s = tolower(*s);
	return(start);
}
