/* mine.h */

/* 
 * $Header: /f/osi/pepsy/RCS/mine.h,v 7.2 91/02/22 09:49:16 mrose Interim $
 *
 *
 * $Log:	mine.h,v $
 * Revision 7.2  91/02/22  09:49:16  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/04  19:18:55  mrose
 * update
 * 
 * Revision 7.0  90/07/01  19:54:42  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#define TABLESIZE 29

typedef struct ID_TABLE {
	char	*h_value;
	char	*r_value;
	int	def_bit;
	int	def_value;
	int	count;
	struct ID_TABLE	*next;
	} id_entry;

typedef struct S_TABLE {
	char	*name;
	char	*type;
	struct S_TABLE	*parent;
	char	*field;
	int	defined;
	struct S_TABLE *next;
	} s_table;

extern	id_entry	*id_table[];

extern char *c_flags();
