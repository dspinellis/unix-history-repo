/* xdr_ld.h - xdr for additional dbxWorks structures */

/*  Copyright 1992 Free Software Foundation, Inc.

    This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01a,05jun90,llk  extracted from xdr_dbx.h.
*/

#ifndef INCxdrldh
#define INCxdrldh

#define MAXSTRLEN 256
#define MAXTBLSZ 100

/*
 * structure used to pass back the information for a single file
 * loaded in VxWorks
 */
struct ldfile {
	char 	*name;
	int 	txt_addr;
	int 	data_addr;
	int 	bss_addr;
};
typedef struct ldfile ldfile;

/*
 * structure used to return a list of all files loaded over to 
 * VxWorks. (VX_STATE_INQ return)
 */
struct ldtabl {
	u_int tbl_size;
	ldfile *tbl_ent;
};
typedef struct ldtabl ldtabl;


bool_t xdr_ldfile();
bool_t xdr_ldtabl();

#endif	INCxdrldh
