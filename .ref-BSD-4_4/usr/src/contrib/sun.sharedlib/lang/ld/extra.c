/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

#ident	"@(#)extra.c	1.2	69/12/31"

/* Copyright (c) 1991 by Sun Microsystems, Inc. */

#include <a.out.h>
#include <stdio.h>

/*
 * collect_extra_sections is called from load2() when it is
 * noticed that there is more stuff after the string table.
 * this is assumed to be the extra section stuff, which
 * is to be concatenated, each in its respective sections,
 * at the end of the output file. Because we don't know
 * how long the string table is going to turn out to be,
 * we cannot simply copy to the appropriate place in
 * that file. Instead, we use temp files, which have
 * to be copied at the end. Sad.
 */

#define MAX_EXTRA_SECTIONS	2
FILE * temp_file[MAX_EXTRA_SECTIONS] = { NULL, NULL };
char * temp_file_name[MAX_EXTRA_SECTIONS] = { NULL, NULL };
int    n_extra_sections = 0;
int    total_size[MAX_EXTRA_SECTIONS];

void
collect_extra_sections( iop, loc, offset, maxoff )
	void * iop;    /* opaque-to-me pointer to an IO block. */
	long   loc;    /* offset in containing file (for archives) */
	int    offset; /* where in the input file/element the string table ends */
	int	   maxoff; /* where the input file/element ends */
{
	struct extra_sections xtra;
	int    section_size[ MAX_EXTRA_SECTIONS];
	FILE * sect_fd;
	int    secno, l, seclength;
	char   buffer[512];

	/* establish IO contact with the extra sections */
	dseek1( iop, loc+offset, maxoff-offset );
	/* read the header */
	mget( (char *)&xtra, sizeof(xtra), iop);
	if (xtra.extra_magic != EXTRA_MAGIC)
		error(1, "bad secondary magic number");
	if ( xtra.extra_nsects >= MAX_EXTRA_SECTIONS )
		error(1, "too many extra sections");
	/* read the section size table */
	mget( (char *)section_size, xtra.extra_nsects*sizeof(int), iop );
	/* open the temp files, as necessary */
	while ( n_extra_sections < xtra.extra_nsects ){
		create_section_temp( n_extra_sections++ );
	}

	/* now shovel the stuff */
	for ( secno = 0 ; secno < xtra.extra_nsects ; secno += 1 ){
		seclength = 0;
		sect_fd   = temp_file[secno];
		while ( (l = section_size[secno] - seclength) > 0 ){
			if ( l > sizeof(buffer) ) l = sizeof(buffer);
			mget( buffer, l, iop );
			fwrite( buffer, l, 1, sect_fd );
			seclength += l;
		}
		total_size[secno] += seclength;
	}

}

void
write_extra_sections( strout )
	void * strout;     /* opaque-to-me pointer to an IO block. */
{
	struct extra_sections xtra;
	FILE * sect_fd;
	int    secno, l, seclength;
	char   buffer[512];

	if ( n_extra_sections == 0 ) return;

	/* setup and write the header */
	xtra.extra_magic = EXTRA_MAGIC;
	xtra.extra_nsects = n_extra_sections;
	bwrite( &xtra, sizeof(xtra), strout );
	/* write segment size table */
	bwrite( &total_size[0], n_extra_sections*sizeof(total_size[0]), strout );

	/* now shovel the stuff */
	for ( secno = 0 ; secno < xtra.extra_nsects ; secno += 1 ){
		seclength = 0;
		sect_fd   = temp_file[secno];
		rewind( sect_fd );
		while ( (l = total_size[secno] - seclength) > 0 ){
			if ( l > sizeof(buffer) ) l = sizeof(buffer);
			if ( fread( buffer, 1, l, sect_fd ) != l )
				error(1, "ioerror in write_extra_sections");
			bwrite( buffer, l, strout );
			seclength += l;
		}
	}
}


/*
 * junk routines for tmp file housekeeping.
 */

create_section_temp( n ) /* called from collect_extra_sections() as needed */
{
	char tmpprefix[23] ; /* random number */
	sprintf( tmpprefix, "ld.%d.", n );
	temp_file_name[n] = tempnam( NULL, tmpprefix );
	if ( (temp_file[n] = fopen( temp_file_name[n], "w+")) == NULL){
		error(1,"cannot create section temp files");
	}
}

void
delete_section_temps() /* called from delexit() */
{

	while (n_extra_sections > 0 ){
		fclose( temp_file[n_extra_sections-1] );
		if ( unlink( temp_file_name[n_extra_sections-1] ) != 0 ){
			error(1, "cannot remove section temp files");
		}
		free( temp_file_name[n_extra_sections-1] );
		n_extra_sections -= 1;
	}
}
