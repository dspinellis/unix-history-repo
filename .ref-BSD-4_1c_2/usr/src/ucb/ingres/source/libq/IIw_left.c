# include	<ingres.h>
# undef MAXNAME
# include	"../equel/constants.h"
# include	"IIglobals.h"
# include	<sccs.h>

SCCSID(@(#)IIw_left.c	7.2	10/27/81)

/*
**	IIw_left -- writes down a "tupret's" target list.
**		
**	Parameters:
**		string -- a char * to a string containing everything
**			inside the equivalent "retrieve" statement,
**			but instead of result domain names, the string
**			should have '%<ingres_type>', where <ingres_type>
**			is the ingres type of the resulting C variable.
**			To escape a '%' use 2 ("%%").
**			'String' is left unchanged after the call.
**		argv -- a vector of pointers to the 
**			corresponding C variables.
**
**	Usage:
**		argv [0] = &double_var;
**		argv [1] = &int_var;
**		IIw_left("%f8 = i.double, %i2=i.ifield", argv);
**
**	Required by:
**		parametrized retrieves without a target relation
**
**	Requires:
**		Uses the ret_sym array IIretsym, and the old equel
**		method for doing tuprets. NOTE that this does not
**		allow dynamic (before each tuple) resolution of 
**		the result C variables as does the new tupret method.
**
**	Error numbers:
**		1003 -- 1 parameter, the erroneous string.
**			"Bad format for a domain in a param retrieve
**			without a result relation"
*/


IIw_left(string, argv)
char	*string;
char	**argv;
{
	register char	*b_st, *e_st;
	register char	**av;
	int		type;
	char		*IIitos();

	if (IIdebug)
		printf("ent IIw_left : string \"%s\"\n",
		string);
	av = argv;
	for (b_st = e_st = string; *e_st; )
	{
		if (*e_st != '%')
		{
			e_st++;
			continue;
		}

		/* provide escape method */
		if (e_st [1] == '%')
		{
			e_st [1] = '\0';
			IIwrite(b_st);
			/* leave string intact */
			e_st [1] = '%';
			b_st = e_st = &e_st [2];
			continue;
		}
		*e_st = '\0';
		IIwrite(b_st);
		*e_st++ = '%';
		IIwrite(" RET_VAR ");

		switch (*e_st)
		{

		  case 'f' :
			switch (*++e_st)
			{

			  case '8' :
				type = opDOUBLE;
				break;

			  case '4' :
				type = opFLOAT;
				break;

			  default :
				goto error_label;
			}
			break;

		  case 'i' :
			switch (*++e_st)
			{

			  case '4' :
				type = opLONG;
				break;

			  case '2' :
				type = opSHORT;
				break;

			  default :
				goto error_label;
			}
			break;

		  case 'c' :
			type = opSTRING;
			break;
		}
		IIretrieve(*av++, type);
		b_st = ++e_st;
	}
	IIwrite(b_st);
	return;


error_label :
	IIerror(1003, 1, &string); 
	IIerrflag = 1003;
	/* make sure that part already written down will
	 * cause an error, and ignore that error
	 */
	IIwrite(",");
	IIo_print = IIprint_err;
	IIprint_err = IIno_err;
}
