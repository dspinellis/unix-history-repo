/*
 *	This is the VMS version of the UNIX nlist function.  It will open
 *	the specifed linker symbol table file (produced by the /SYMBOL_TABLE=
 *	linker option) and return the addresses of the specified global
 *	variables just like the UNIX nlist function.
 */

#include <aout.h>
/*
 * Define VMS object file (symbol table) global symbol stuff
 */
#define OBJ$C_GSD     1			/* GSD record type */
#define OBJ$C_GSD_PSC 0				/* PSECT definition */
#define OBJ$C_GSD_SYM 1				/* GLOBAL symbol */
#define OBJ$C_GSD_EPM 2				/* ENTRY mask */
#define OBJ$C_GSD_PRO 3				/* FORMAL procedure */

#define SPACE_SIZE 2048			/* Size of read Buffer */

nlist(name, list)
char *name;
struct nlist *list;
{
	register struct nlist *p;	/* Pointer to user's nlist list */
	register int f, i, nreq, offset;
	int read_size;			/* Size of last read request */
	char space[SPACE_SIZE];		/* Read buffer */
	char real_name[100];		/* Possibly changed symtab filename */
	char *cp;
	char code;

	/*
	 * Clear out the type and value parts of the user's nlist list
	 */
	for(p = list, nreq = 0; p->n_un.n_name[0] ; p++, nreq++) {
		p->n_type = 0;
		p->n_value = 0;
	}

	/*
	 * Open the symtab file.  If there is no extension, use ".stb"
	 */
	for(i=0,cp=name;*cp && (*cp != '.');i++) real_name[i] = *cp++;
	real_name[i] = '.';
	real_name[i+1] = 's';
	real_name[i+2] = 't';
	real_name[i+3] = 'b';
	real_name[i+4] =  0;
	f = open( *cp == '.' ? name : real_name, 0);
	if(f < 0)
		return(-1);

	/*
	 * Cycle through the symtab file decoding the object records.
	 * When you hit a global symbol definition, check it against
	 * the list of requested symbols.
	 */
	while (read_size=read(f,space,SPACE_SIZE)){
		if(space[0] != OBJ$C_GSD) continue;	/* looking for GSD */
		for (i=1;i<read_size;)
			switch(code=space[i]){
			
				case OBJ$C_GSD_PSC:	/* PSECT, ignore it */
					i += 9 + space[i+8];
					break;

				case OBJ$C_GSD_SYM:	/* GLOBAL SYMBOL */
					offset = 9;
					goto common;
				case OBJ$C_GSD_EPM:	/* ENTRY PT. MASK */
				case OBJ$C_GSD_PRO:	/* Proc decl. */
					offset = 11;
			common:	       {
					register int j,nchars;
					int *val;
					register char *cp;
					/*
					 * Get size of symbol string
					 */
					nchars = space[i+offset];
					/*
					 * Search the list for this string
					 */
					for(p = list;p->n_un.n_name[0]; p++) {
						cp = p->n_un.n_name;
						/*
						 * Strip off a leading '_'
						 */
						if (*cp == '_') cp++;
						for(j=0;j<nchars;j++)
						   /*
						    * Make the comparison
						    *  caseless
						    */
						   if( ( (*cp >= 'a') &&
							 (*cp <= 'z') ?
							*cp++ -'a'+'A' :
							*cp++)
						      != space[i+j+offset+1]) goto cont;
						/*
						 * GOT IT! Extract its value
						 */
						val = (int *)&space[i+5];
						p->n_value = *val;
						p->n_type = 0;
						if(--nreq == 0)
							goto alldone;
						break;
				cont:		;
					}
				       }
				       i += offset+1 + space[i+offset];
				       if (code == OBJ$C_GSD_PRO){
						/* Dump the PRO arguments */
						register int n;
						n = space[i+1];
						i += 2;
						while (n-- > 0)
							i += 2 + space[i+1];
				       }
				       break;


				default: printf("%d is an invalid GSD type\n",
						space[i]);
					 return(-1);
			}
	}
alldone:
	close(f);
	return(0);
}
