/* m_foil.c - foil search of .mh_profile */

#include "../h/mh.h"
#include <stdio.h>


void m_foil (path)
char   *path;
{
    register struct node *np;

    defpath = context = "/dev/null";

    if (path) {
	np = m_defs = (struct node *) malloc (sizeof *np);
	if (np == NULL)
	    adios (NULLCP, "unable to allocate profile storage");

	np -> n_name = getcpy ("Path");
	np -> n_field = getcpy (path);
	np -> n_context = 0;
	np -> n_next = NULL;

	if (mypath == NULL && (mypath = getenv ("HOME")) != NULL)
	    mypath = getcpy (mypath);		
    }
}
