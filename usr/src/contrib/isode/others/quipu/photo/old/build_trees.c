/* build_trees.c - build decode trees */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/build_trees.c,v 7.1 90/09/24 15:36:37 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/build_trees.c,v 7.1 90/09/24 15:36:37 mrose Exp $
 *
 *
 * $Log:	build_trees.c,v $
 * Revision 7.1  90/09/24  15:36:37  mrose
 * update
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

static int built = 0;
char * malloc ();

extern node * bl_tree_top;      /* pointers to the decode trees */
extern node * wt_tree_top;
extern node * two_tree_top;

/* ROUTINE:     get_node                                                */
/*                                                                      */
/* SYNOPSIS:    creates a new node.                                     */
/*                                                                      */
/* DESCRIPTION: Uses malloc to allocate sufficient memory for a node to */
/*              be stored, and the sets all the pointer fields of the   */
/*              node to NULL, and initialises the other fields          */

static node *
get_node ()

{
node * mem;

   if ((mem = (node *) malloc (sizeof (node))) == NULL)
       return NULL;
   mem->n_type = INTERNAL;              /* The most common node type */
   mem->zero = NULL;
   mem->one = NULL;

   return (mem);
}


static char * file_path (faxdir,file)
char *faxdir, *file;
{
static char buf [200];
char * sprintf ();

	(void) sprintf (buf,"%s/%s",faxdir,file);
	return (buf);
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

build_trees (faxdir)
char * faxdir;
{
FILE *  fptr;
register i;
char    buffer [15];
char *  string = buffer;
char * file;

   if (built)
	return (0);

   /* create the tops of the trees */
   bl_tree_top = get_node ();
   wt_tree_top = get_node ();
   two_tree_top = get_node ();
   if (bl_tree_top == NULL || wt_tree_top == NULL || two_tree_top == NULL) {
       if (bl_tree_top)
	   free (bl_tree_top);
       if (wt_tree_top)
	   free (wt_tree_top);
       if (two_tree_top)
	   free (two_tree_top);
       (void) fprintf (stderr, "PHOTO: out of memory");
       return (-1);
   }

 /* Add the white terminals to the white tree */
   file = file_path (faxdir,"wt_term");
   if ((fptr = fopen (file,"r")) == NULL)
   {
	(void) fprintf(stderr,"Can't open data file %s\n",file);
	return(-1);
   }
   i=0;
   while ( fscanf(fptr,"%s",string) != EOF )
	add_tree (string,i++,WT_TERM,wt_tree_top);
   (void) fclose (fptr);

   /* Add the black terminals to the black tree */
   file = file_path (faxdir,"bl_term");
   if ((fptr = fopen (file,"r")) == NULL)
   {
	(void) fprintf(stderr,"Can't open data file %s\n",file);
	return(-1);
   }
   i=0;
   while ( fscanf(fptr,"%s",string) != EOF )
	add_tree (string,i++,BL_TERM,bl_tree_top);
   (void) fclose (fptr);

   /* Add the white make codes to the white tree */
   file = file_path (faxdir,"wt_make");
   if ((fptr = fopen (file,"r")) == NULL)
   {
	(void) fprintf(stderr,"Can't open data file %s\n",file);
	return(-1);
   }
   i = 64;
   while ( fscanf(fptr,"%s",string) != EOF ) {
	add_tree (string,i,MAKE,wt_tree_top);
	i += 64;
	}
   (void) fclose (fptr);

   /* Add the black make up codes to the black tree */
   file = file_path (faxdir,"bl_make");
   if ((fptr = fopen (file,"r")) == NULL)
   {
	(void) fprintf(stderr,"Can't open data file %s\n",file);
	return(-1);
   }
   i = 64;
   while ( fscanf(fptr,"%s",string) != EOF ) {
	add_tree (string,i,MAKE,bl_tree_top);
	i += 64;
	}
   (void) fclose (fptr);

   /* make the two dimensional decode tree */
   file = file_path (faxdir,"two_dim");
   if ((fptr = fopen (file,"r")) == NULL)
   {
	(void) fprintf(stderr,"Can't open data file %s\n",file);
	return(-1);
   }
   i = 1;
   while ( fscanf(fptr,"%s",string) != EOF )
	add_tree (string,i++,MAKE,two_tree_top);
   (void) fclose (fptr);

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

static add_tree (string,run,mode,root)

char *  string;         /* string containing the bit sequence           */
int     run;            /* the run length associated with the sequence  */
char    mode;           /* the type of data we are entering             */
node *  root;           /* top of the tree sting should be added to     */
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

