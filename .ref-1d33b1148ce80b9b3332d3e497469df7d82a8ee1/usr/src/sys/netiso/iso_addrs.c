/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */

iso_maskmatch(sisoa, sisob, smask)
	struct sockaddr_iso	*sisoa, *sisob, smask;
{
	caddr_t a, b, c;
	int compare_len;

	if ( smask ) {
		compare_len = smask->isoa_len;
	} else if ((compare_len = isoaa->isoa_len) != isoab->isoa_len)
		return 0;

	return masked_cmp(&sisoa->siso_addr, &sisob->siso_addr, 
		&smask->siso_addr, compare_len);
}

int masked_cmp( a, b, c, len)
	register caddr_t a, b, c;
	int len;
{
	caddr_t ax=a, bx=b, cx=c;
	register int i;

	for( i=0; i< len; i++) {
		if( (*a)&(*c) != (*b)&(*c) )
			break;
		a++;
		b++;
		c++;
	}
	return (int) i;
}
