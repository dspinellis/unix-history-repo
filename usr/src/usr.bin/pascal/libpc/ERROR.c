/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)ERROR.c 1.5 %G%";

#include	<stdio.h>
#include	<signal.h>
#include	"h00vars.h"
#include	"h01errs.h"

/*
 * Routine ERROR is called from the runtime library when a runtime error
 * occurs. Its arguments are the internal number of the error which occurred,
 * and an error specific piece of error data. The error file is constructed
 * from errdata by the makefile using the editor script make.ed1.
 */
long
ERROR(errnum, errdata)

	int	errnum;
	union cvt {
		long	longdat;
		char	*strngdat;
		double	dbldat;
	} errdata;
{
	PFLUSH();
	if (_entry[errnum].entryaddr != 0) {
		(*_entry[errnum].entryaddr)(errdata);
		return;
	}
	fputc('\n',stderr);
	SETRACE();
	switch (errnum) {
	case ECHR:
		fprintf(stderr, "Argument to chr of %D is out of range\n"
			,errdata.longdat);
		return(errdata.longdat);
	case EHALT:
		fputs("Call to procedure halt\n",stderr);
		PCEXIT(0);
	case ENILPTR:
		fputs("Pointer value out of legal range\n",stderr);
		return(0);
	case EPASTEOF:
		fprintf(stderr,"%s: Tried to read past end of file\n"
			,errdata.strngdat);
		return(0);
	case EREADIT:
		fprintf(stderr,"%s: Attempt to read, but open for writing\n"
			,errdata.strngdat);
		return(0);
	case EWRITEIT:
		fprintf(stderr,"%s: Attempt to write, but open for reading\n"
			,errdata.strngdat);
		return(0);
	case ECLOSE:
		fprintf(stderr,"%s: Close failed\n",errdata.strngdat);
		return(0);
	case ELLIMIT:
		fprintf(stderr,"%s: Line limit exceeded\n",errdata.strngdat);
		return(0);
	case ESQRT:
		fprintf(stderr,"Negative argument of %e to sqrt\n"
			,errdata.dbldat);
		return(errdata.dbldat);
	case EREFINAF:
		fprintf(stderr,"%s: ",errdata.strngdat);
	case ENOFILE:
		fputs("Reference to an inactive file\n",stderr);
		return(0);
	case EWRITE:
		fputs("Could not write to ",stderr);
		perror(errdata.strngdat);
		return(0);
	case EOPEN:
		fputs("Could not open ",stderr);
		perror(errdata.strngdat);
		return(0);
	case ECREATE:
		fputs("Could not create ",stderr);
		perror(errdata.strngdat);
		return(0);
	case EREMOVE:
		fputs("Could not remove ",stderr);
		perror(errdata.strngdat);
		return(0);
	case ESEEK:
		fputs("Could not reset ",stderr);
		perror(errdata.strngdat);
		return(0);
	case ENAMESIZE:
		fprintf(stderr,"%s: File name too long\n",errdata.strngdat);
		return(0);
	case ELN:
		fprintf(stderr,"Non-positive argument of %e to ln\n"
			,errdata.dbldat);
		return(errdata.dbldat);
	case EBADINUM:
		fprintf(stderr,"%s: Bad data found on integer read\n"
			,errdata.strngdat);
		return(0);
	case EBADFNUM:
		fprintf(stderr,"%s: Bad data found on real read\n"
			,errdata.strngdat);
		return(0);
	case ENUMNTFD:
		fprintf(stderr,
			"Unknown name \"%s\" found on enumerated type read\n",
			errdata.strngdat);
		return(0);
	case ENAMRNG:
		fprintf(stderr,
			"Enumerated type value of %D is out of range on output\n",
			errdata.longdat);
		return(errdata.longdat);
	case EFMTSIZE:
		fprintf(stderr,"Non-positive format width: %D\n",errdata.longdat);
		return(0);
	case EGOTO:
		fputs("Active frame not found in non-local goto\n", stderr);
		return(0);
	case ECASE:
		fprintf(stderr,"Label of %D not found in case\n"
			,errdata.longdat);
		return(errdata.longdat);
	case EOUTOFMEM:
		fputs("Ran out of memory\n",stderr);
		return(0);
	case ECTLWR:
		fprintf(stderr, "Range lower bound of %D out of set bounds\n",
			errdata.longdat);
		return(0);
	case ECTUPR:
		fprintf(stderr, "Range upper bound of %D out of set bounds\n",
			errdata.longdat);
		return(0);
	case ECTSNG:
		fprintf(stderr, "Value of %D out of set bounds\n",
			errdata.longdat);
		return(0);
	case ENARGS:
		if (errdata.longdat < 0)
			fprintf(stderr,
				"There were %D too few arguments to formal routine\n",
				-errdata.longdat);
		else
			fprintf(stderr,
				"There were %D too many arguments to formal routine\n",
				errdata.longdat);
		return(0);
	case EARGV:
		fprintf(stderr,"Argument to argv of %D is out of range\n"
			,errdata.longdat);
		return(errdata.longdat);
	case EPACK:
		fprintf(stderr,"i = %D: Bad i to pack(a,i,z)\n"
			,errdata.longdat);
		return(errdata.longdat);
	case EUNPACK:
		fprintf(stderr,"i = %D: Bad i to unpack(z,a,i)\n"
			,errdata.longdat);
		return(errdata.longdat);
	case ERANGE:
		fprintf(stderr,"Value of %D is out of range\n",errdata.longdat);
		return(errdata.longdat);
	case ESUBSC:
		fprintf(stderr,"Subscript value of %D is out of range\n"
			,errdata.longdat);
		return(errdata.longdat);
	case EASRT:
		fprintf(stderr,"Assertion failed: %s\n",errdata.strngdat);
		return(0);
	case ESTLIM:
		fprintf(stderr,
			"Statement count limit exceeded, %D statements executed\n",
			errdata.longdat);
		return(errdata.longdat);
	default:
		fputs("Panic: unknown error\n",stderr);
		return(0);
	}
}
