/*
 * Copyright (c) 1985 University of Alberta *
 */

#ifndef lint
static char *rcsid_dofile_c = "$Header: dofile.c,v 10.2 86/02/01 15:59:47 tony Rel $";
#endif

#include "site.h"
#include "imPdefs.h"
#include "imPcodes.h"
#include "impv.h"
#include <stdio.h>
#ifndef XWIND
#include <pixrect/pixrect_hs.h>
#endif

dofile()
{
	register int i, j, code;
	register struct glyph *gp;
	register unsigned char *p;
#ifdef COLOR
	register int k, *tp;
	int t, tmp[1000];
	unsigned char setcolor();
#endif COLOR
	short rwid, iwid;
	short hsize, vsize;
	MSKWORD	mw;

	while ((code = gc()) == '@')	/* skip document control stuff */
	    while ((code = gc()) != ')')
	    if (code == '"') while(gc() != '"');
	do{
		if(code & 0200) decode(code);
		switch(code){
		case ASP0:
			HPos += SpaceSize; 
			break;
		case ASP1:
			HPos += SpaceSize + 1; 	
			break;
		case AM:
			EXTSIGN(1); 
			HPos += V(1); 
			break;
		case AMM:
			HPos--; 
			break;
		case AMP:
			HPos++; 
			break;
		case ASRULE:
			EXTSIGN(3); 
			P_rule(); 
			break;
		case ABRULE:
			P_rule(); 
			break;
		case ASGLY:
			EXTSIGN(4); 
			EXTSIGN(6);
		case ABGLY:
			fam = (V(1)>>7)&0177;
			if(family[fam] == 0){
				family[fam] = (struct glyph *)malloc((unsigned)sizeof font0);
				for(i=127; i >= 0; i--)
					family[fam][i].bits = 0;
				fam_rot[fam] = (V(1)>>14) & 03;
			}
			gp = &family[fam][0177&V(1)];
			gp->advance = V(2);
			gp->left = V(4);
			gp->top = V(6);
			rwid = ( V(3) + 7) >> 3;
#ifdef COLOR
			if( slide )
			{
			    gp->width = (V(3) + 2) / 3;
			    gp->height = (V(5) + 2) / 3;
			    iwid = gp->height * gp->width;
			    gp->bits = (unsigned char *)malloc((unsigned)iwid);
			    for (p = gp->bits, i = iwid; i--;) *p++ = 0;
			    for (tp = tmp, i = (gp->width * 3); i--;) *tp++ = 0;

			    for (i = 0; i < V(5); i++)
			    {
				for (j = 0; j < rwid; j++)
				{
				    t = gc();
				    for (k = 8; k--;)
					tmp[j * 8 + (7 - k)]
						= (t & (1 << k)) ? 1 : 0;
				}
				tp = tmp;
				p = &gp->bits[i/3 * gp->width];
				for (j = gp->width; j--;)
				    *p++ += *tp++ + *tp++ + *tp++;
			    }
			    for (p = gp->bits, i = iwid; i--; p++)
				*p = (*p + 1) / 2;
			} else
#endif COLOR
			{
			    gp->width = (V(3) + 1) >> 1;
			    gp->height = (V(5) + 1) >> 1;
			    /*round size to nearest byte size */
			    iwid = ( gp->width + 7) >> 3;
			    gp->bits = (unsigned char *)malloc((unsigned)gp->height * iwid);
			    /* for the height of the glyph */
			    for (i=0; i<V(5); i++) {
				/* point at the bytes of glyph storage*/
				p = &gp->bits[(i>>1) * iwid];
				/* for two bytes at a time */
				for (j=0; j < rwid; j += 2, p++) {
					/* squeez the first byte 4 left */
					mw = map8_4[gc()]<<4;
					/* if not the last byte squeez */
					/* in another */
					if (j < (rwid - 1))
					     mw |= map8_4[gc()];
					/* or store byte if i odd */ 
					if(i&1) *p |= mw;
					/* store byte if even */
					else *p = mw;
				}
			    }
			}
			break;
		case ADELC:
		case ADELG:
			gp = &family[0177 & (V(1)>>7) ][0177 & V(1)];
			if(gp->bits) free((char *)gp->bits);
			gp->bits = 0;
			break;
		case ADELF:
			fam =0177 & V(1);
			if(family[fam] == 0) break;
			for(i = 127; i >= 0; i--){
				gp = &family[fam][i];
				if(gp->bits) free((char *)gp->bits);
				gp->bits = 0;
			}
			break;
		case AMARGIN:
			BeginOfLine = V(1); 	
			break;
		case ABSKIP:
			InterLine = V(1); 
			break;
		case AN:
			HPos = BeginOfLine;
			VPos += InterLine;
			break;
		case AEND:
			if( ppause()) return;
		case APAGE:
			HPos = VPos = 0; 
			break;
		case AF:
			CurFamily =0177&V(1); 
			break;
		case ASETSP:
			SpaceSize = V(1); 
			break;
		case AH:
			if( V(1) & 01) HPos += V(1) >> 1;
			else HPos = V(1) >> 1;
			break;
		case AV:
			if( V(1) & 01) VPos += V(1) >> 1;
			else VPos = V(1) >> 1;
			break;
		case ASET_HV_SYS:
			/*page orientation  not done*/
			set_hv_sys();
			break;
		case ASET_ABS_H:
			/*set abs major advance pos*/
			HPos = V(1);
			break;
		case ASET_ABS_V:
			/*set abs minor advance pos*/
			VPos = V(1);
			break;
		case ASET_REL_H:
			/*set rel major advance pos*/
			HPos += V(1);
			break;
		case ASET_REL_V:
			/*set rel minor advance pos*/
			VPos += V(1);
			break;
		case AROTMS:
			/*set advance directions not done*/
			advance_dir = v(1);
			break;
		case AMMOVE:
			/*add to main dir  not done*/
			if(orient == 0) HPos += V(1);
			break;
		case ASMOVE:
			/*add to main dir  not done*/
			if(orient == 0) VPos += V(1);
			break;
		case ACREATE_MAP:
			/*create font map not done*/
			/* get name  and size*/
			map_name = v(1);
			ntuples = v(2);
			/* read and throw away bytes */
			for (i=0;i<ntuples*4;i++) (void)gc();
#ifdef notdef
			/* get memory for ntuples*/
			map = (map_ptr *)malloc(ntuples * sizeof int);
			/* read in map */
			for(i=0;i<ntuples;i++) {
				get and store byte word byte
				    map = map;
			}
#endif
			break;
		case ACREATE_FAMILY:
			/* create family table not done */
			/* get family name and size*/
			fam_in = v(1);
			ntuples = v(2);
			for (i=0;i<ntuples;i++) {
				(void)gc();
				while(gc()!=NULL);
			}
#ifdef notdef
			/* get memory for ntuples*/
			/* read in family */
			for(i=0;i<ntuples;i++) {
				get and store byte string*
			}
#endif
			break;
		case AFORCE_GLY_DELETE:
			/*delete marked glyphs  */
			break;
		case ASET_PATH:
			/*get a line path */
			/*get vertexcount*/
			vertex_count = V(1);
			path_point = (struct path *)malloc
			    (vertex_count * 2 * (sizeof(short)));
#ifdef COLOR
			if (slide)
				for (i=0; i<vertex_count; i++) {
					(path_point+i)->hor = getint() / 3;
					(path_point+i)->vert = getint() / 3;
			 	}
			else
#endif COLOR
			{
				for (i=0; i<vertex_count; i++) {
					(path_point+i)->hor = (getint() >> 1);
					(path_point+i)->vert = (getint() >> 1);
				}
			}
			break;
		case ASET_TEXTURE:
			/*set texture for lines? */
			fam = (V(1) >> 7) & 0177;
			member = V(1) & 0177;
			break;
		case ASET_PEN:
			/*set pen diameter */
#ifdef COLOR
			if(slide)
				diameter = (v(1)+2) / 3;
			else
#endif COLOR
				diameter = (v(1)+1) > 1;
			break;
		case ADRAW_PATH:
			/* draw a path of bits or lines */
			operation = v(1);
			if (diameter < 4){ /* draw up to 4 || lines */
				for(j=0; j<(vertex_count-1); j++){
					for(i=0; i<diameter; i++){
						draw_path1((path_point+j)->hor, 
						(path_point+j)->vert, 
						(path_point+j+1)->hor,
						(path_point+j+1)->vert);
					}
				}
			}
			else { 
				/* draw a path wider than 4 lines */	 
				for(j=0; j<vertex_count; j++) (void)fflush(stdout);
			}
			break;
		case AFILL_PATH:
			/* fill in a polygon   not done*/
			operation = v(1);
			break;
		case ABIT_MAP:
			/* get a bit map  not done*/
			operation = v(1);
			hsize = v(2);
			vsize = v(3);
			Prnt_Bitmap(hsize,vsize);
			break;
		case ASET_MAGNIFICATION:
			magnification = v(1);
			break;
		case ASET_PUSH_MASK:
			/* set the state mask */
			push_mask = V(1);
			break;
		case APUSH:
			/*push a state onto the state stack */
			push_stack[pushed] = (struct state *)
			    malloc((unsigned)sizeof pstack);
			stap = push_stack[pushed];
			stap->push_mask =push_mask;
			if(push_mask& 0400) {
				stap->diameter = diameter;
				stap->texture = texture;
			}
			if(push_mask& 0200) stap->SpaceSize = SpaceSize;
			if(push_mask& 0100) stap->InterLine = InterLine;
			if(push_mask& 040) stap->BeginOfLine = BeginOfLine;
			if(push_mask& 020) stap->fam = fam;
			if(push_mask& 010) {
				stap->HPos = HPos;
				stap->VPos = VPos;
			}
			if(push_mask& 04) stap->advance_dir = advance_dir;
			if(push_mask& 02) {
				stap->horigin = horigin;
				stap->vorigin = vorigin;
			}
			if(push_mask& 01) stap->orient = orient;
			pushed++;
			break;
		case APOP:
			/*pop a state off the state stack */
			if(pushed >= 1) {
				pushed--;
				stap = push_stack[pushed];
				push_mask =  stap->push_mask;
				if(push_mask& 0400) {
					diameter = stap->diameter; 
					texture = stap->texture;
				}
				if(push_mask& 0200) SpaceSize = stap->SpaceSize;
				if(push_mask& 0100) InterLine = stap->InterLine;
				if(push_mask& 040) BeginOfLine = stap->BeginOfLine;
				if(push_mask& 020) fam = stap->fam;
				if(push_mask& 010) {
					HPos = stap->HPos;
					VPos = stap->VPos;
				}
				if(push_mask& 04) advance_dir = stap->advance_dir;
				if(push_mask& 02) {
					horigin = stap->horigin;
					vorigin = stap->vorigin;
				}
				if(push_mask& 01) orient = stap->orient;
				free((char *)push_stack[pushed]);
			}
			else fprintf(stderr, "Can not pop more states\n");
			break;
		case ADEFINE_MACRO:
			/* define a macro  */
			/* get the name v(1) and length V(2) of the macro*/
#ifdef COLOR
			if (v(1) == 255) 
			{
			    if (gc() == 0 )
	  		    {
				bc.red = (float)gc();
				bc.green = (float)gc();
				bc.blue = (float)gc();
				if( slide )	
				{
					backcolor = setcolor(0);
 					p = (unsigned char *)(mpr_d(pscreen)->md_image);
					for (i = scr_size; i--;)
						*p++ = backcolor;
				 }
			    }
			    else
			    {
				cc.red = (float)gc();
				cc.green = (float)gc();
				cc.blue = (float)gc();
			    }
		         }
			 else
#endif COLOR
			 {
			 macro[v(1)].length = V(2);
			 macro_length = V(2);
			 /* get space equal to length */
			 mp = macro[v(1)].pointer =
			     (unsigned char *)malloc((unsigned)macro_length);
			 /*read the macro into the got space*/
			 for(i=0;i<macro_length; i++) mp[i] = gc();
			 }
			break;
		case AEXECUTE_MACRO:
			/* execute a macro */
			macro_length = macro[v(1)].length;
			mp = macro[v(1)].pointer;			
			macro_on = TRUE;
			break;
		case ANOP:
			break;
		default:
			if(!(code&0200)) {
				gp = &family[CurFamily][code];
				Prnt_Glyph(&family[CurFamily][code]);
			} 
			else {
				(void)write(1,'$',1);
			}
		}
	}
	while (AEOF != (code=gc()) );
}

char r_mask[9] = { 
	0, 01, 03, 07, 017, 037, 077, 0177, 0377 };
char l_mask[9] = { 
	0377, 0376, 0374, 0370, 0360, 0340, 0300, 0200, 00 };
Prnt_Glyph(gp)
register struct glyph *gp;
{
	register int i, bit_width, skew;
	register unsigned char *sp, *base;
	register unsigned char *bp = gp->bits;
	short  x_bit_pos, y_bit_pos;
#ifdef COLOR
	short int j;
	unsigned char setcolor();
#endif COLOR

#ifdef COLOR
	if( slide )
	{
		x_bit_pos = (HPos - gp->left + 2) / 3;
		y_bit_pos = (VPos - gp->top + 2) / 3;
		base = ((unsigned char *)(mpr_d(pscreen)->md_image))
		   + y_bit_pos * scr_x + x_bit_pos;
	}
	else
#endif COLOR
	{
		x_bit_pos = (HPos - gp->left + 1) >> 1;
		y_bit_pos = (VPos - gp->top + 1) >> 1;
		if(((((HPos+1) >>1) + gp->width) > scr_x) ||
			((((VPos+1)>>1) + gp->height) > scr_y) || bp == 0)
		{
			big++;
			return;
		}
		if((x_bit_pos < 0) || (y_bit_pos < 0) )
		{
			(void)write(1,'-',1);
			little++;
			return;
	 	}
#ifdef XWIND
		base = pscreen
#else XWIND
		base = ((unsigned char *)(mpr_d(pscreen)->md_image))
#endif XWIND
		   + (y_bit_pos * wide) + (x_bit_pos >> 3);
	}
	/* skew is bits displacement of glyph from a byte edge*/
	skew = 8 - (x_bit_pos & 07);

#ifdef COLOR
	if (slide)
	{
		for (i = gp->height; i--; base += scr_x)
		{
			sp = base;
			for (j = gp->width; j--;)
				if( *bp != 0 )
					*sp++ = setcolor(*bp++);
				else
				{
					bp++;
					sp++;
				}
		}
	}
	else
#endif COLOR
	{
	    for(i = gp->height; i--; base += wide){
		sp = base;
		for(bit_width = gp->width; bit_width > 0;){
			if(skew == 8){
				*sp++ |= *bp++;
				bit_width -= 8;
			} 
			else {
				*sp++ |= (*bp >> (8 - skew)) & r_mask[skew];
				if((bit_width -= skew) <= 0){
					bp++; 
					break;
				}
				*sp |= (*bp++ << skew) & l_mask[skew];
				bit_width -= (8 - skew);
			}
		}
	    }
	}
	HPos += gp->advance;
}

char l_bits[8] = {
	0377, 0200, 0300, 0340, 0360, 0370, 0374, 0376};
P_rule()
{
	register int i, bit_width, skew;
	register unsigned char *sp, *base;
	register int bits_l;
	short  x_bit_pos, y_bit_pos;

	if(((V(1) + HPos) > 2048) || ((V(2) + VPos) > 2640)) {
		big++;
		return;
	}
	x_bit_pos = (HPos + 1) >> 1;
	y_bit_pos = (VPos + V(3) + 1) >> 1;
	if((x_bit_pos < 0) || (y_bit_pos < 0)){
		(void)write(1,'-',1);
		little++;
		return;
	}
#ifdef XWIND
	base = pscreen
#else
	base = ((unsigned char *)(mpr_d(pscreen)->md_image))
#endif
	    + (y_bit_pos * wide) + (x_bit_pos >> 3);
	skew = 8 - (x_bit_pos & 07);
	V(1) = (V(1) + 1) >> 1;
	V(2) = (V(2) + 1) >> 1;
	for(i = V(2); i--; base += wide){
		sp = base;
		for(bit_width = V(1); bit_width > 0;){
			bits_l = l_bits[bit_width>7 ? 0 : bit_width];
			if(skew == 8){
				*sp++ |= bits_l;
				bit_width -= 8;
			} 
			else {
				*sp++ |= (bits_l >> (8 - skew)) & r_mask[skew];
				if((bit_width -= skew) <= 0) break;
				*sp |= (bits_l << skew) & l_mask[skew];
				bit_width -= (8 - skew);
			}
		}
	}
}

set_hv_sys()
{
	register int norigin, naxes, norient;

	/*set a logical page orientation relative to the physical pagenot done*/
	norigin = (v(1)>>5 )& 03;
	naxes = (v(1)>>3)& 03;
	norient = v(1)&07;
	if(norient < 4) orient = (orient+norient)%4;
	else orient = norient - 4;
	set_axes(naxes, norigin);

}

set_axes(ax, or)
int ax, or;
{
	switch (ax){
	case 0: 
		break;
	case 1:
		hvangle = -hvangle;
		break;
	case 2:
		hvangle = 1;
		break;
	case 3:
		hvangle = -1;
		break;
	}
	set_origin(or);
}

set_origin(or)
int or;
{
	/*set the origin of the logical page relative to the physical*/
	switch (or){
	case 0: 
		break;
	case 1: 
		break;
	case 2:
		originlv =  0;
		switch (orient){
		case 0:
			originlh =  0;
			break;
		case 1:
			originlh =  MAXx;
			break;
		case 2:
		case 3:
			originlh =  MAXy;
			break;
		}
	case 3:
		if(orient == 1 ||orient == 3){
			originlh =  xpos;
			originlv =  ypos;
		}
		else{
			originlv =  xpos;
			originlh =  ypos;
		}
		break;
	}
}

draw_path1(x0, y0, x1, y1)	/* draw line from here to x0, y0, x1, y1 */
int x0, y0, x1, y1;
{
	int d, xd, yd, dx, dy, incr1, incr2;
	int i, numdots;
	int motincrx, motincry;

	xd = x1 - x0;
	yd = y1 - y0;
	dx = abs(xd);
	dy = abs(yd);
	/* sort between vertical, horizontal and in between */
	put1(x0, y0);
	if (xd == 0) {
		numdots = abs (yd);
		motincry = (yd<0)? -1 : 1;
		for (i = 0; i < numdots; i++) {
			y0 += motincry;
			put1(x0, y0);
		}
		return;
	}
	if (yd == 0) {
		numdots = abs (xd);
		motincrx = (xd<0)? -1 : 1;
		for (i = 0; i < numdots; i++) {
			x0 += motincrx;
			put1(x0, y0);
		}
		return;
	}
	if (abs (xd) > abs (yd)) { /* slope less than 1 */
		d = 2 * dy -dx;
		incr1 = 2 * dy;
		incr2 = 2 * (dy - dx);
		numdots = abs (xd);
		motincrx = (xd<0)? -1 : 1;
		motincry = (yd<0)? -1 : 1;
		for (i = 0; i < numdots; i++) {
			put1(x0, y0);
			x0 += motincrx;
			if(d < 0) d = d + incr1;
			else{
				y0 += motincry;
				d = d + incr2;
			}
		}
	}
	else { /* slope more than 1 */
		d = 2 * dx -dy;
		incr1 = 2 * dx;
		incr2 = 2 * (dx - dy);
		numdots = abs (yd);
		motincrx = (xd<0)? -1 : 1;
		motincry = (yd<0)? -1 : 1;
		for (i = 0; i < numdots; i++) {
			put1(x0, y0);
			y0 += motincry;
			if(d < 0) d = d + incr1;
			else{
				x0 += motincrx;
				d = d + incr2;
			}
		}
	}
}


/* set the value of a t byte (texture) */
t_byte(x, y)
int x, y;
{

	if(fam == 0 && member == 0)
	    return(r_mask[9]);
	/* else find the byte in the glyph mask */
	return( *(family[fam][member].bits + ((x & 017) >> 3) + 2 * (y & 017)) );
}

/* put a pixel onto a page image. */
put1(x,y)
int x, y;
{
	register int skew;
	short  x_bit_pos, y_bit_pos;
	register unsigned char *pbyte, tbyte;
#ifdef COLOR
	unsigned char setcolor();
#endif COLOR

	x_bit_pos = x;
	y_bit_pos = y;
	if((x_bit_pos > scr_x) || (y_bit_pos > scr_y)) {
		(void)write(1, "+pix", 4);
		return;
	}
	if((x_bit_pos < 0) || (y_bit_pos < 0) ) {
		(void)write(1,'-',1);
		return;
	}
#ifdef COLOR
	if( slide )
	{
		pbyte = ((unsigned char *)(mpr_d(pscreen)->md_image))
		   + (y_bit_pos * scr_x) + x_bit_pos;
	}
	else
#endif COLOR
	{
#ifdef XWIND
		pbyte = pscreen
#else
		pbyte = ((unsigned char *)(mpr_d(pscreen)->md_image))
#endif
		    +(y_bit_pos * wide) + (x_bit_pos >> 3);
	}
	/* skew is bits displacement of pixel from a byte edge*/
	skew = 8 - (x_bit_pos & 07);
#ifdef COLOR
	if(slide)
	{
		switch(operation) {
		case(0):
			*pbyte = backcolor;
			break;
		case(3):
		case(7):
		default:
			*pbyte = setcolor(5);
			break;
		}
	}
	else
#endif COLOR
	{
		switch(operation)
		{
		case(0): /* clear the bit */
			if (skew == 8) *pbyte &= r_mask[skew-1];
			else *pbyte &= (r_mask[skew-1] | l_mask[skew+1]);
			return;
		case(3): /* opaque with t bit */
			tbyte = t_byte(x, y);
			if (skew == 8) *pbyte &= r_mask[skew-1];
			else *pbyte &= (r_mask[skew-1] | l_mask[skew+1]);
			if (skew == 8) tbyte &= r_mask[skew-1];
			else tbyte &= (r_mask[skew-1] | l_mask[skew+1]);
			*pbyte &= tbyte;
			return;
		case(7):/* or with t bit */
			tbyte = t_byte(x, y);
			if (skew == 8) tbyte &= r_mask[skew-1];
			else tbyte &= (r_mask[skew-1] | l_mask[skew+1]);
			*pbyte |= tbyte;
			return;
		default: /* black the bit */
			if(skew == 8) *(pbyte-1) |=  1;
			else *pbyte |=  1 << skew;
			return;
		}
	}
}

#ifdef notdef
/* put a line onto a page image. */
putline(xstart,xend,y)
int xstart,xend,y;
{
	register int nbytes, xh, xs, i;
	unsigned char pbyte, tbyte, *scptr;

	/* find how many bytes are affected by the line*/
	nbytes=1;
	if (xstart/8 != xend/8) nbytes += xend/8 - xstart/8;
	switch(operation){
		case(0): /* clear the bytes */
		for(i=0; i< nbytes; i++){
			xh = xstart/8;
			xs = xstart%8;
#ifdef XWIND
			scptr = pscreen;
#else
			scptr = (unsigned char *)(mpr_d(pscreen)->md_image);
#endif
			pbyte = *scptr+y*wide+xh;
			if((xend+1)/8 > xh+1)
			    pbyte &= l_mask[xs];
			else
				pbyte &= (l_mask[xs] 
				    & r_mask[(xend+1)%8]);
			*(scptr+y*wide+xh) = pbyte; 
			xstart = xstart + 8 - xs;
		}
		return;

		case(15):/* black the bit */
		for(i=0; i< nbytes; i++){
			xh = xstart/8;
			xs = xstart%8;
			pbyte = *scptr+y*wide+xh;
			if((xend+1)/8 > xh+1)
			    pbyte &= r_mask[8 - xs];
			else
				pbyte &= (r_mask[8 - xs] 
				    & l_mask[8 - (xend+1)%8]);
			*(scptr+y*wide+xh) = pbyte; 
			xstart = xstart + 8 - xs;
		}
		return;

		case(3): /* opaque with t bit */
		for(i=0; i< nbytes; i++){
			xh = xstart >> 3;
			xs = xstart & 07;
			pbyte = *scptr+wide*y+xh;
			if((xend+1)/8 > xh+1)
			    pbyte &= l_mask[xs]
			    & t_byte(xstart, (xstart+8-xs), y);
			else
				pbyte &= (l_mask[xs] 
				    & r_mask[(xend+1)%8])
				    & t_byte(xstart, xend, y);
			*(scptr+y*wide+xh) = pbyte; 
			xstart = xstart + 8 - xs;
		}
		return;

		case(7):/* or with t bit */
		for(i=0; i< nbytes; i++){
			xh = xstart/8;
			xs = xstart%8;
			pbyte = *scptr+wide*y+xh;
			if((xend+1)/8 > xh+1)
			    pbyte |= 
			    t_byte(xstart, (xstart+8-xs), y);
			else
				pbyte |= 
				    t_byte(xstart, xend, y);
			*(scptr+y*wide+xh) = pbyte; 
			xstart = xstart + 8 - xs;
		}
		return;
	}
}
#endif
Prnt_Bitmap(hsize, vsize)
short int hsize, vsize;
{
	register short int i, j, k, l;
	register unsigned char *sp;
	unsigned char mw, *base, *basev, *baseb;
	short  x_bit_pos, y_bit_pos;

	/*
		 * get the bits and put them ???
		 */
	x_bit_pos = (HPos + 1) >> 1;
	y_bit_pos = (VPos + 1) >> 1;
	if((((HPos+hsize*4+1) >> 1)  > scr_x) || (((VPos+vsize*32+1) >> 1) > scr_y)){
		big++;
		return;
	}
	if((x_bit_pos < 0) || (y_bit_pos < 0) ) {
		(void)write(1,'-',1);
		little++;
		return;
	}
#ifdef XWIND
	base = pscreen
#else
	base = ((unsigned char *)(mpr_d(pscreen)->md_image))
#endif
	    +(y_bit_pos * wide) + (x_bit_pos >> 3);
	basev = base;
	if(magnification == 0) for(i=0;i<vsize;i++) {
		/* rows of cols of 32*32 blocks */
		baseb = basev;
		/* cols of 32*32 blocks */
		for(j=0;j<hsize;j++) {
			sp = baseb;
			/* block of 32*32 */
			for(k=0;k<32;k++) {
				/* row of 32 bits in 32*32 block*/
				/* point at the bytes of glyph storage*/
				/* for two bytes at a time */
				for (l=0; l < 2; l++, sp++) {
					/* squeez the first byte 4 left */
					mw = map8_4[gc()]<<4;
					mw |= map8_4[gc()];
					/* or store byte if k odd */ 
					if(k&1) *sp |= mw;
					/* store byte if even */
					else *sp = mw;
				}
				sp = baseb + wide * ((1+k)>>1);
				/* add 1 line to v pos if odd*/
			}
			baseb = basev + 2 * (j+1);
			/* add 16 bits to h pos */
		}
		basev = base + wide * 16 *(i+1);
		/* squeeze to 1/2 height*/
	}
	else if (magnification == 1) 
	    for (i=0; i<vsize; i++) {
		baseb = basev;
		for (j=0; j<hsize; j++) {
			sp = baseb;
			for (k=0; k<32; k++) {
				/* point at the bytes of glyph storage*/
				for (l=4; l; l--) *sp++ = gc();
				sp += 124;
			}
			baseb += 4;
		}
		basev = base + wide * 32 *i;
	}
}
