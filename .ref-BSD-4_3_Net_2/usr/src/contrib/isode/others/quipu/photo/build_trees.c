/* build_trees.c - build decode trees */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/build_trees.c,v 7.3 91/02/22 09:29:00 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/build_trees.c,v 7.3 91/02/22 09:29:00 mrose Interim $
 *
 *
 * $Log:	build_trees.c,v $
 * Revision 7.3  91/02/22  09:29:00  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/15  13:42:36  mrose
 * KEJ
 * 
 * Revision 1.1  91/01/02  21:34:39  kej
 * Initial revision
 * 
 * Revision 7.0  89/11/23  22:01:33  mrose
 * Release 6.0
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



#include <stdio.h>
#include "quipu/photo.h"

int built = 0;
char * malloc ();

extern node * bl_tree_top;      /* pointers to the decode trees */
extern node * wt_tree_top;
extern node * two_tree_top;

/*
 *  Decoding tables
 */

static char *bl_make [] = {
"0000001111",
"000011001000",
"000011001001",
"000001011011",
"000000110011",
"000000110100",
"000000110101",
"0000001101100",
"0000001101101",
"0000001001010",
"0000001001011",
"0000001001100",
"0000001001101",
"0000001110010",
"0000001110011",
"0000001110100",
"0000001110101",
"0000001110110",
"0000001110111",
"0000001010010",
"0000001010011",
"0000001010100",
"0000001010101",
"0000001011010",
"0000001011011",
"0000001100100",
"0000001100101",
"00000001000",
"00000001100",
"00000001101",
"000000010010",
"000000010011",
"000000010100",
"000000010101",
"000000010110",
"000000010111",
"000000011100",
"000000011101",
"000000011110",
"000000011111",
NULL
};

static char *bl_term [] = {
"0000110111",
"010",
"11",
"10",
"011",
"0011",
"0010",
"00011",
"000101",
"000100",
"0000100",
"0000101",
"0000111",
"00000100",
"00000111",
"000011000",
"0000010111",
"0000011000",
"0000001000",
"00001100111",
"00001101000",
"00001101100",
"00000110111",
"00000101000",
"00000010111",
"00000011000",
"000011001010",
"000011001011",
"000011001100",
"000011001101",
"000001101000",
"000001101001",
"000001101010",
"000001101011",
"000011010010",
"000011010011",
"000011010100",
"000011010101",
"000011010110",
"000011010111",
"000001101100",
"000001101101",
"000011011010",
"000011011011",
"000001010100",
"000001010101",
"000001010110",
"000001010111",
"000001100100",
"000001100101",
"000001010010",
"000001010011",
"000000100100",
"000000110111",
"000000111000",
"000000100111",
"000000101000",
"000001011000",
"000001011001",
"000000101011",
"000000101100",
"000001011010",
"000001100110",
"000001100111",
NULL
};

static char *two_dim [] = {
"0001",
"001",
"0000010",
"000010",
"010",
"1",
"011",
"000011",
"0000011",
NULL
};

static char *wt_make [] = {
"11011",
"10010",
"010111",
"0110111",
"00110110",
"00110111",
"01100100",
"01100101",
"01101000",
"01100111",
"011001100",
"011001101",
"011010010",
"011010011",
"011010100",
"011010101",
"011010110",
"011010111",
"011011000",
"011011001",
"011011010",
"011011011",
"010011000",
"010011001",
"010011010",
"011000",
"010011011",
"00000001000",
"00000001100",
"00000001101",
"000000010010",
"000000010011",
"000000010100",
"000000010101",
"000000010110",
"000000010111",
"000000011100",
"000000011101",
"000000011110",
"000000011111",
NULL
};

static char *wt_term [] = {
"00110101",
"000111",
"0111",
"1000",
"1011",
"1100",
"1110",
"1111",
"10011",
"10100",
"00111",
"01000",
"001000",
"000011",
"110100",
"110101",
"101010",
"101011",
"0100111",
"0001100",
"0001000",
"0010111",
"0000011",
"0000100",
"0101000",
"0101011",
"0010011",
"0100100",
"0011000",
"00000010",
"00000011",
"00011010",
"00011011",
"00010010",
"00010011",
"00010100",
"00010101",
"00010110",
"00010111",
"00101000",
"00101001",
"00101010",
"00101011",
"00101100",
"00101101",
"00000100",
"00000101",
"00001010",
"00001011",
"01010010",
"01010011",
"01010100",
"01010101",
"00100100",
"00100101",
"01011000",
"01011001",
"01011010",
"01011011",
"01001010",
"01001011",
"00110010",
"00110011",
"00110100",
NULL
};

static char * uncompressed_1d = "000000001111";
static char * uncompressed_2d = "0000001111";

/* ROUTINE:     get_node                                                */
/*                                                                      */
/* SYNOPSIS:    creates a new node.                                     */
/*                                                                      */
/* DESCRIPTION: Uses malloc to allocate sufficient memory for a node to */
/*              be stored, and the sets all the pointer fields of the   */
/*              node to NULL, and initialises the other fields          */

node *
get_node ()

{
node * mem;

   mem = (node *) malloc (sizeof (node));
   mem->n_type = INTERNAL;              /* The most common node type */
   mem->zero = NULL;
   mem->one = NULL;

   return (mem);
}


/* ROUTINE:     build_trees.
/*
/* SYNOPSIS:    build the decode tree.
/*
/* DESCRIPTION: For each of the three trees, read the data in from a file
/* in string form, convert this into an integer, then add this integer to the
/* tree.
/* Also contained in the node is the value associated with the string read in,
/* so we need to count each string as it is read in.
*/

build_trees ()
{
   register i;
   char **  string;

   if (built)
	return (0);

   /* create the tops of the trees */

   bl_tree_top = get_node ();
   wt_tree_top = get_node ();
   two_tree_top = get_node ();

/* Add the white terminals to the white tree */

   i=0;
   for (string = wt_term; *string; ++string)
	add_tree (*string,i++,WT_TERM,wt_tree_top);

   /* Add the black terminals to the black tree */

   i=0;
   for (string = bl_term; *string; ++string)
	add_tree (*string,i++,BL_TERM,bl_tree_top);

   /* Add the white make codes to the white tree */

   i = 64;
   for (string = wt_make; *string; ++string) {
	add_tree (*string,i,MAKE,wt_tree_top);
	i += 64;
   }

   /* Add the black make up codes to the black tree */

   i = 64;
   for (string = bl_make; *string; ++string) {
	add_tree (*string,i,MAKE,bl_tree_top);
	i += 64;
   }

   /* make the two dimensional decode tree */

   i = 1;
   for (string = two_dim; *string; ++string)
	add_tree (*string,i++,MAKE,two_tree_top);

   /* put uncompressed mode entrance codewords on all three trees */

   add_tree (uncompressed_1d,-1,MAKE,bl_tree_top);
   add_tree (uncompressed_1d,-1,MAKE,wt_tree_top);
   add_tree (uncompressed_2d,-1,MAKE,two_tree_top);

   /* put end of line markers on all three trees */

   add_tree ("00000000000",EOLN,EOLN,bl_tree_top);
   add_tree ("00000000000",EOLN,EOLN,wt_tree_top);
   add_tree ("00000000000",EOLN,EOLN,two_tree_top);

   built = 1;
   return(0);
}

/* ROUTINE:     add_tree                                                */
/*                                                                      */
/* SYNOPSIS:    adds a run to the tree                                  */
/*                                                                      */

add_tree (string,run,mode,root)

char *  string;         /* string containing the bit sequence           */
int     run;            /* the run length associated with the sequence  */
char    mode;           /* the type of data we are entering             */
node *  root;           /* top of the tree string should be added to    */
{

char *   ptr;
node *   treeptr;
register i;

   ptr = string;
   treeptr = root;

   for ( i=0; i< strlen(string); i++,ptr++)
	if (*ptr == '0') {
	   if (treeptr->zero == NULL)
		treeptr->zero = get_node ();
	   treeptr = treeptr->zero;

	} else {
	      if (treeptr->one == NULL)
		   treeptr->one = get_node ();
	      treeptr = treeptr->one;
	}

   treeptr->n_type = mode;
   treeptr->value  = run;
}

