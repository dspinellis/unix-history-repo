/*
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <ctags.h>

extern char	searchar;		/* ex search character */

/*
 * getline --
 *	get the line the token of interest occurred on,
 *	prepare it for printing.
 */
getline()
{
	register long	saveftell;
	register int	c,
			cnt;
	register char	*cp;

	saveftell = ftell(inf);
	(void)fseek(inf,lineftell,L_SET);
	if (xflag)
		for (cp = lbuf;GETC(!=,'\n');*cp++ = c);
	/*
	 * do all processing here, so we don't step through the
	 * line more than once; means you don't call this routine
	 * unless you're sure you've got a keeper.
	 */
	else for (cnt = 0,cp = lbuf;GETC(!=,EOF) && cnt < ENDLINE;++cnt) {
		if (c == (int)'\\') {		/* backslashes */
			if (cnt > ENDLINE - 2)
				break;
			*cp++ = '\\'; *cp++ = '\\';
			++cnt;
		}
		else if (c == (int)searchar) {	/* search character */
			if (cnt > ENDLINE - 2)
				break;
			*cp++ = '\\'; *cp++ = c;
			++cnt;
		}
		else if (c == (int)'\n') {	/* end of keep */
			*cp++ = '$';		/* can find whole line */
			break;
		}
		else
			*cp++ = c;
	}
	*cp = EOS;
	(void)fseek(inf,saveftell,L_SET);
}

/*
 * put_entries --
 *	write out the tags
 */
put_entries(node)
	register NODE	*node;
{
	extern FILE	*outf;		/* ioptr for tags file */
	extern int	vflag;		/* -v: vgrind style output */

	if (node->left)
		put_entries(node->left);
	if (vflag)
		printf("%s %s %d\n",
		    node->entry,node->file,(node->lno + 63) / 64);
	else if (xflag)
		printf("%-16s%4d %-16s %s\n",
		    node->entry,node->lno,node->file,node->pat);
	else
		fprintf(outf,"%s\t%s\t%c^%s%c\n",
		    node->entry,node->file,searchar,node->pat,searchar);
	if (node->right)
		put_entries(node->right);
}

char *
savestr(str)
	char	*str;
{
	register u_int	len;
	register char	*space;
	char	*malloc();

	len = strlen(str) + 1;
	if (!(space = malloc((u_int)len))) {
		/*
		 * should probably free up the tree, here,
		 * we're just as likely to fail here as we
		 * are when getting the NODE structure
		 */
		fputs("ctags: no more space.\n",stderr);
		exit(1);
	}
	bcopy(str,space,len);
	return(space);
}
