#ifndef lint
static char *sccsid ="opqpoly.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

extern float interalpha[INTERSIZE];
extern int internum;

void opqpoly (edgelist, linelist, inlines, outlines, both)
EDGEPTR edgelist;
LINEPTR linelist;
LINEPTR *inlines, *outlines, *both;
{
	LINEPTR linewalk, circlearc;
	LINENODE nuin, nuout, nuboth;
	LINEPTR inwalk, outwalk, bothwalk;
	LINEPTR forfreeing;

	inwalk = &nuin;
	inwalk->next = NULL;
	outwalk = &nuout;
	outwalk->next = NULL;
	bothwalk = &nuboth;
	bothwalk->next = NULL;
	linewalk = linelist;
	while (linewalk) {
		while (inwalk->next)
			inwalk = inwalk->next;
		while (outwalk->next)
			outwalk = outwalk->next;
		while (bothwalk->next)
			bothwalk = bothwalk->next;
		switch (linewalk->kind) {
		case LINE:
			polyline (
				edgelist,
				linewalk->x0,
				linewalk->y0,
				linewalk->x1,
				linewalk->y1,
				&inwalk->next,
				&outwalk->next,
				&bothwalk->next
			);
			forfreeing = linewalk;
			linewalk = linewalk->next;
			tryfree(forfreeing);
			break;
		case CIRCLE:
			circlearc = angularc (
				((CIRCPTR) linewalk)->x0,
				((CIRCPTR) linewalk)->y0,
				((CIRCPTR) linewalk)->r,
				0.0,
				2.0*PI
			);
			circlearc->next = linewalk->next;
			tryfree(linewalk);
			linewalk = circlearc;
			/* FALL THROUGH TO case arc */
		case ARC:
			polyarc (
				edgelist,
				((ARCPTR) linewalk)->x0,
				((ARCPTR) linewalk)->y0,
				((ARCPTR) linewalk)->radius,
				((ARCPTR) linewalk)->theta1,
				((ARCPTR) linewalk)->theta2,
				&inwalk->next,
				&outwalk->next,
				&bothwalk->next
			);
			forfreeing = linewalk;
			linewalk = linewalk->next;
			tryfree(forfreeing);
			break;
		case STRING:
/*
			fprintf (stderr, "ideal: can't opaque over strings\n");
*/
			bothwalk->next = linewalk;
			linewalk = linewalk->next;
			bothwalk->next->next = NULL;
			break;
		case SPLINE:
/*
			fprintf (stderr, "ideal: can't opaque over splines\n");
*/
			bothwalk->next = linewalk;
			linewalk = linewalk->next;
			bothwalk->next->next = NULL;
			break;
		default:
			impossible ("opqpoly");
			break;
		} /* switch */
	} /* while */
	*inlines = nuin.next;
	*outlines = nuout.next;
	*both = nuboth.next;
} /* opqpoly */

void polyline (edgelist, candx0,candy0, candx1,candy1, inlines, outlines, both)
EDGEPTR edgelist;
float candx0,candy0, candx1,candy1;
LINEPTR *inlines, *outlines, *both;
{
	OPQPTR interwalk;
	boolean inside, onedge;
	LINENODE nuin, nuout;
	LINEPTR inwalk, outwalk;
	LINEPTR linewalk;
	EDGEPTR prevedge, curedge;
	OPQPTR alphalist;
	float alpha, beta;
	float gamma[2], theta[2];
	boolean collinear;
	boolean X, Y, Z, W;
	int i;
	double dummy, rem;

	alphalist = (OPQPTR) NULL;
	inwalk = &nuin;
	inwalk->next = NULL;
	outwalk = &nuout;
	outwalk->next = NULL;
	curedge = edgelist;
	do {
		if (curedge->fax == NULL) {
			if (
				llinter (
					candx0, candy0,
					candx1, candy1,
					curedge->sx, curedge->sy,
					curedge->ex, curedge->ey,
					&alpha,
					&beta,
					&collinear
				)
			) {
				if (EPSILON < beta && beta < 1.0 - EPSILON)
					curedge->code[0] = SIMPLE;
				else if (fabs(beta) < EPSILON)
					curedge->code[0] = AT0;
				else if (fabs(1.0-beta) < EPSILON)
					curedge->code[0] = AT1;
				else
					curedge->code[0] = UNUSED;
				curedge->alpha[0] = alpha;
				curedge->code[1] = UNUSED;
			} else
				if (collinear) {
					if (fabs(candx1 - candx0) < EPSILON) {
						curedge->alpha[0] = (curedge->sy - candy0)/(candy1 - candy0);
						curedge->alpha[1] = (curedge->ey - candy0)/(candy1 - candy0);
					} else {
						curedge->alpha[0] = (curedge->sx - candx0)/(candx1 - candx0);
						curedge->alpha[1] = (curedge->ex - candx0)/(candx1 - candx0);
					}
					if (curedge->alpha[0] < curedge->alpha[1]) {
						curedge->code[0] = ON0;
						curedge->code[1] = ON1;
					} else {
						curedge->code[0] = ON1;
						curedge->code[1] = ON0;
					}
				} else
					curedge->code[0] = curedge->code[1] = UNUSED;
		} else if (curedge->fax->kind == ARC) {
			if (
				!lcinter (
					candx0, candy0,
					candx1, candy1,
					curedge->fax->x0, curedge->fax->y0,
					fabs(curedge->fax->radius),
					&gamma[0], &theta[0],
					&gamma[1], &theta[1]
				)
			) {
				curedge->code[0] = curedge->code[1] = UNUSED;
				dprintf "line outside circle\n");
			} else if (fabs(theta[0] - theta[1]) < EPSILON) {
				if (fabs(theta[0] - curedge->fax->theta1) < EPSILON) {
					curedge->alpha[0] = gamma[0];
					curedge->code[0] = curedge->flipped?AT1:AT0;
					dprintf "%d\n", curedge->code[0]);
				} else if (fabs(theta[0] - curedge->fax->theta2) < EPSILON) {
					curedge->alpha[0] = gamma[0];
					curedge->code[0] = curedge->flipped?AT0:AT1;
					dprintf "%d\n", curedge->code[0]);
				} else {
					curedge->code[0] = UNUSED;
					dprintf "line tangent\n");
				}
				curedge->code[1] = UNUSED;
			} else {
				for (i = 0; i < 2; i ++) {
					dprintf "disposition of %f\n", theta[i]);
					if (curedge->fax->theta2 < 2.0*PI) {
						if (theta[i] - curedge->fax->theta1 < -EPSILON
							|| curedge->fax->theta2 - theta[i] < -EPSILON) {
							curedge->code[i] = UNUSED;
							dprintf "intersection off arc\n");
							continue;
						}
					}
					if (curedge->fax->theta2 >= 2.0*PI) {
						if (theta[i] - curedge->fax->theta1 < -EPSILON
							&& curedge->fax->theta2 - theta[i] < 2.0*PI - EPSILON) {
							curedge->code[i] = UNUSED;
							dprintf "intersection off arc\n");
							continue;
						}
					}
					rem = modf(fabs(theta[i] - curedge->fax->theta1)/(2*PI), &dummy);
					dprintf "rem1 = %f\n", rem);
					if (rem < EPSILON || fabs(1.0-rem) < EPSILON) {
						curedge->alpha[i] = gamma[i];
						curedge->code[i] = curedge->flipped?AT1:AT0;
						dprintf "%d\n", curedge->code[i]);
						continue;
					}
					rem = modf(fabs(theta[i] - curedge->fax->theta2)/(2*PI), &dummy);
					dprintf "rem2 = %f\n", rem);
					if (rem < EPSILON || fabs(1.0-rem) < EPSILON) {
						curedge->alpha[i] = gamma[i];
						curedge->code[i] = curedge->flipped?AT0:AT1;
						dprintf "%d\n", curedge->code[i]);
						continue;
					}
					dprintf "simple\n");
					curedge->code[i] = SIMPLE;
					curedge->alpha[i] = gamma[i];
				}
			}
		} else
			impossible ("polyline(A)");
		curedge = curedge->next;
	} while (curedge != edgelist);
	if (dbg) {
		curedge = edgelist;
		do {
			fprintf (stderr, "s (%f,%f); e (%f,%f)\n",
				curedge->sx, curedge->sy,
				curedge->ex, curedge->ey
			);
			fprintf (stderr, "st (%f,%f); et (%f,%f)\n",
				curedge->stx, curedge->sty,
				curedge->etx, curedge->ety
			);
			for (i = 0; i < POSSINTER; i ++)
				fprintf (stderr, "%d %f\n",
					curedge->code[i],
					curedge->alpha[i]
				);
			curedge = curedge->next;
		} while (curedge != edgelist);
	}
	prevedge = edgelist;
	curedge = edgelist->next;
	do {
		for (i = 0; i < POSSINTER; i ++)
			switch (curedge->code[i]) {
			case UNUSED:
				break;
			case SIMPLE:
				opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
				break;
			case AT0:
				dprintf "vertex intersection at (%f,%f)\n", curedge->sx, curedge->sy);
				X = arecollinear(curedge->sx,curedge->sy,curedge->stx,curedge->sty,prevedge->etx,prevedge->ety);
				Y = between(curedge->stx,curedge->sty,curedge->sx,curedge->sy,prevedge->etx,prevedge->ety);
				Z = arecollinear(candx0,candy0,candx1,candy1,curedge->stx,curedge->sty);
				dprintf "X=%d Y=%d Z=%d\n", X, Y, Z);
				if (X && !Z) {
					if (Y)
						opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
					break;
				}
				if (X && Z) {
					if (
						llinter (
							prevedge->sx, prevedge->sy,
							curedge->ex, curedge->ey,
							candx0, candy0,
							candx1, candy1,
							&alpha,
							&beta,
							&collinear
						)
						&& (0.0 < alpha)
						&& (alpha < 1.0)
					)
						opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
					break;
				}
				if (!X) {
					if (
						llinter (
							prevedge->etx, prevedge->ety,
							curedge->stx, curedge->sty,
							candx0, candy0,
							candx1, candy1,
							&alpha,
							&beta,
							&collinear
						)
						&& (alpha > 0.0)
						&& (alpha < 1.0)
					)
					opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
					break;
				}
				impossible("polyline(II:AT0)");
				break;
			case AT1:
				/* should be taken care of by next AT0 */
				break;
			case ON0:
			case ON1:
				X = arecollinear(prevedge->etx,prevedge->ety,curedge->sx,curedge->sy,curedge->next->stx,curedge->next->sty);
				Y = llinter(
					prevedge->etx,prevedge->ety,
					curedge->next->stx,curedge->sty,
					candx0,candy0,
					candx1,candy1,
					&alpha,
					&beta,
					&collinear
				)
				&& (alpha > 0.0)
				&& (alpha < 1.0)
				;
				Z = llinter (
					prevedge->sx,prevedge->sy,
					curedge->next->ex,curedge->next->ey,
					candx0,candy0,
					candx1,candy1,
					&alpha,
					&beta,
					&collinear
				)
				&& (alpha > 0.0)
				&& (alpha < 1.0)
				;
				W = prevedge == curedge->next->next;
				dprintf "X=%d Y=%d, Z=%d, W=%d\n", X, Y, Z, W);
				if (!Y && W)
					opqinsert((curedge->code[i] == ON0)?EXT0:EXT1, curedge->alpha[i], &alphalist);
				else if (!X && Y)
					opqinsert((curedge->code[i] == ON0)?INFL0:INFL1, curedge->alpha[i], &alphalist);
				else if (X && Z)
					opqinsert((curedge->code[i] == ON0)?INFL0:INFL1, curedge->alpha[i], &alphalist);
				else
					opqinsert((curedge->code[i] == ON0)?EXT0:EXT1, curedge->alpha[i], &alphalist);
				break;
			case TANGENT:
			default:
				impossible ("polyline(B)");
				break;
			}
		prevedge = curedge;
		curedge = curedge->next;
	} while (prevedge != edgelist);
	opqinsert(INHERIT, 0.0, &alphalist);
	opqinsert(INHERIT, 1.0, &alphalist);
	if (dbg) {
		fprintf (stderr, "interalpha:\n");
		for (interwalk = alphalist;
			interwalk;
			interwalk = interwalk->next)
			fprintf (stderr, "%d %f, ", interwalk->code, interwalk->alpha);
		fprintf (stderr, "\n");
	}
	inside = onedge = FALSE;
	for (interwalk = alphalist; interwalk; interwalk = interwalk->next)
		switch (interwalk->code) {
		case SIMPLE:
			interwalk->code = (!inside)?INBEGIN:OUTBEGIN;
			inside = !inside;
			break;
		case EXT1:
			interwalk->code = inside?INBEGIN:OUTBEGIN;
			onedge = FALSE;
			break;
		case EXT0:
			interwalk->code = ONBEGIN;
			onedge = TRUE;
			break;
		case INFL1:
			interwalk->code = (!inside)?INBEGIN:OUTBEGIN;
			onedge = FALSE;
			inside = !inside;
			break;
		case INFL0:
			interwalk->code = ONBEGIN;
			onedge = TRUE;
			break;
		case INHERIT:
		case IGNORE:
			interwalk->code = onedge?ONBEGIN:(inside?INBEGIN:OUTBEGIN);
			break;
			break;
		default:
			impossible("polyline(C)");
			break;
		}
	if (dbg) {
		fprintf (stderr, "interalpha:\n");
		for (interwalk = alphalist;
			interwalk;
			interwalk = interwalk->next)
			fprintf (stderr, "%d %f, ", interwalk->code, interwalk->alpha);
		fprintf (stderr, "\n");
	}
	for (interwalk = alphalist; interwalk; interwalk = interwalk->next) {
		linewalk = linegen (
			candx0 + interwalk->alpha*(candx1 - candx0),
			candy0 + interwalk->alpha*(candy1 - candy0),
			candx0 + interwalk->next->alpha*(candx1 - candx0),
			candy0 + interwalk->next->alpha*(candy1 - candy0)
		);
		if (
			interwalk->alpha > -EPSILON
			&& interwalk->next
			&& interwalk->next->alpha < 1.0 + EPSILON
		)
			switch (interwalk->code) {
			case INBEGIN:
				inwalk->next = linewalk;
				inwalk = inwalk->next;
				break;
			case OUTBEGIN:
				outwalk->next = linewalk;	
				outwalk = outwalk->next;
				break;
			case ONBEGIN:
				tryfree(linewalk);
				break;
			default:
				impossible("polyline(D)");
				break;
			}
	}
	*inlines = nuin.next;
	*outlines = nuout.next;
	*both = NULL;
}

#define	xtanp(x,y,r,t)	x+r*cos(t)+sin(t)
#define	ytanp(x,y,r,t)	y+r*sin(t)-cos(t)
#define	xtane(x,y,r,t)	x+r*cos(t)-sin(t)
#define	ytane(x,y,r,t)	y+r*sin(t)+cos(t)

boolean ptinpoly (edgelist, x, y)
EDGEPTR edgelist;
float x, y;
{
	LINEPTR inlines, outlines, both;
	polyline (
		edgelist,
		x - 100*EPSILON, y - 100*EPSILON,
		x + 100*EPSILON, y + 100*EPSILON,
		&inlines, &outlines, &both
	);
	if (inlines) {
		if (outlines || both)
			impossible ("ptinpoly(A)");
		else {
			linefree(inlines);
			dprintf "ptinpoly: TRUE\n");
			return TRUE;
		}
	} else if (outlines) {
		if (inlines || both)
			impossible ("ptinpoly(B)");
		else {
			linefree(outlines);
			dprintf "ptinpoly: FALSE\n");
			return FALSE;
		}
	} else
		impossible ("ptinpoly(C)");
}

void polyarc (edgelist, x0,y0, radius, startang, endang, inlines, outlines, both)
EDGEPTR edgelist;
float x0, y0, radius, startang, endang;
LINEPTR *inlines, *outlines, *both;
{
	OPQPTR interwalk;
	boolean inside, onedge;
	LINENODE nuin, nuout;
	LINEPTR inwalk, outwalk;
	LINEPTR linewalk;
	EDGEPTR prevedge, curedge;
	OPQPTR alphalist;
	float alpha[2], beta[2], gamma[2], theta[2];
	boolean collinear;
	boolean X, Y, Z, W;
	float stx, sty, etx, ety;
	int i;
	double dummy, rem;

	alphalist = (OPQPTR) NULL;
	inwalk = &nuin;
	inwalk->next = NULL;
	outwalk = &nuout;
	outwalk->next = NULL;
	curedge = edgelist;
	do {
		if (curedge->fax == NULL) {
			if (
				lcinter (
					curedge->sx, curedge->sy,
					curedge->ex, curedge->ey,
					x0, y0,
					radius,
					&alpha[0], &theta[0],
					&alpha[1], &theta[1]
				)
			) {
				if (fabs(theta[0] - theta[1]) < EPSILON) {
					if (fabs(alpha[0]) < EPSILON)
						curedge->code[0] = AT0;
					else if (fabs(1.0-alpha[0]) < EPSILON)
						curedge->code[0] = AT1;
					else
						curedge->code[0] = TANGENT;
					curedge->alpha[0] = rprin(theta[0]);
					curedge->code[1] = UNUSED;
				} else {
					for (i = 0; i < 2; i ++) {
						if (EPSILON < alpha[i] && alpha[i] < 1.0 - EPSILON)
							curedge->code[i] = SIMPLE;
						else if (fabs(alpha[i]) < EPSILON)
							curedge->code[i] = AT0;
						else if (fabs(alpha[i] - 1.0) < EPSILON)
							curedge->code[i] = AT1;
						else
							curedge->code[i] = UNUSED;
						curedge->alpha[i] = rprin(theta[i]);
					}
				}
			}
		} else if (curedge->fax->kind == ARC) {
			if (!ccinter (
					x0, y0,
					radius,
					curedge->fax->x0, curedge->fax->y0,
					curedge->fax->radius,
					&gamma[0], &theta[0],
					&gamma[1], &theta[1]
				)
			) {
				if (fabs(x0 - curedge->fax->x0) < EPSILON
					&& fabs(y0 - curedge->fax->y0) < EPSILON
					&& fabs(fabs(radius) - fabs(curedge->fax->radius)) < EPSILON
				) {
					curedge->alpha[0] = rprin(curedge->fax->theta1);
					curedge->alpha[1] = rprin(curedge->fax->theta2);
					curedge->code[0] = ON0;
					curedge->code[1] = ON1;
				} else {
					curedge->code[0] = curedge->code[1] = UNUSED;
				}
			} else if (fabs(theta[0] - theta[1]) < EPSILON) {
				if (fabs(theta[0] - curedge->fax->theta1) < EPSILON)
					curedge->code[0] = curedge->flipped?AT1:AT0;
				else if (fabs(theta[0] - curedge->fax->theta2) < EPSILON)
					curedge->code[0] = curedge->flipped?AT0:AT1;
				else
					curedge->code[0] = TANGENT;
				curedge->alpha[0] = rprin(gamma[0]);
				curedge->code[1] = UNUSED;
			} else {
				for (i = 0; i < 2; i ++) {
					dprintf "disposition of %f\n", theta[i]);
					if (curedge->fax->theta2 < 2.0*PI) {
						if (theta[i] - curedge->fax->theta1 < -EPSILON
							|| curedge->fax->theta2 - theta[i] < -EPSILON) {
							curedge->code[i] = UNUSED;
							dprintf "intersection off arc\n");
							continue;
						}
					}
					if (curedge->fax->theta2 > 2.0*PI) {
						if (theta[i] - curedge->fax->theta1 < -EPSILON
							&& curedge->fax->theta2 - theta[i] < 2.0*PI - EPSILON) {
							curedge->code[i] = UNUSED;
							dprintf "intersection off arc\n");
							continue;
						}
					}
					rem = modf(fabs(theta[i] - curedge->fax->theta1)/(2.0*PI), &dummy);
					dprintf "rem1 = %f\n", rem);
					if (rem < EPSILON || fabs(1.0 - rem) < EPSILON) {
						curedge->alpha[i] = rprin(gamma[i]);
						curedge->code[i] = curedge->flipped?AT1:AT0;
						continue;
					}
					rem = modf(fabs(theta[i] - curedge->fax->theta2)/(2.0*PI), &dummy);
					dprintf "rem2 = %f\n", rem);
					if (rem < EPSILON || fabs(1.0 - rem) < EPSILON) {
						curedge->alpha[i] = rprin(gamma[i]);
						curedge->code[i] = curedge->flipped?AT0:AT1;
						continue;
					}
					dprintf "simple\n");
					curedge->code[i] = SIMPLE;
					curedge->alpha[i] = rprin(gamma[i]);
				}
			}
		} else {
			impossible ("polyarc(D)");
		}
		curedge = curedge->next;
	} while (curedge != edgelist);
	if (dbg) {
		curedge = edgelist;
		do {
			fprintf (stderr, "s (%f,%f); e (%f,%f)\n",
				curedge->sx, curedge->sy,
				curedge->ex, curedge->ey
			);
			fprintf (stderr, "st (%f,%f); et (%f,%f)\n",
				curedge->stx, curedge->sty,
				curedge->etx, curedge->ety
			);
			for (i = 0; i < POSSINTER; i ++)
				fprintf (stderr, "%d %f\n",
					curedge->code[i],
					curedge->alpha[i]
				);
			curedge = curedge->next;
		} while (curedge != edgelist);
	}
	prevedge = edgelist;
	curedge = edgelist->next;
	do {
		for (i = 0; i < POSSINTER; i ++) {
			stx = xtanp(x0,y0,radius,curedge->alpha[i]);
			sty = ytanp(x0,y0,radius,curedge->alpha[i]);
			etx = xtane(x0,y0,radius,curedge->alpha[i]);
			ety = ytane(x0,y0,radius,curedge->alpha[i]);
			switch (curedge->code[i]) {
			case UNUSED:
				break;
			case SIMPLE:
				opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
				break;
			case AT0:
				dprintf "vertex intersection at (%f,%f)\n", curedge->sx, curedge->sy);
				X = arecollinear(curedge->sx,curedge->sy,curedge->stx,curedge->sty,prevedge->etx,prevedge->ety);
				Y = between(curedge->stx,curedge->sty,curedge->sx,curedge->sy,prevedge->etx,prevedge->ety);
				Z = arecollinear(stx,sty,etx,ety,curedge->stx, curedge->sty);
				dprintf "X=%d Y=%d Z=%d\n", X, Y, Z);
				if (X && !Z) {
					if (Y)
						opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
					break;
				}
				if (X && Z) {
					if (
						llinter (
							prevedge->sx, prevedge->sy,
							curedge->ex, curedge->ey,
							stx, sty,
							etx, ety,
							&alpha[0],
							&alpha[1],
							&collinear
						)
						&& (0.0 < alpha[0])
						&& (alpha[0] < 1.0)
					)
						opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
					break;
				}
				if (!X) {
					if (
						llinter (
							prevedge->etx, prevedge->ety,
							curedge->stx, curedge->sty,
							stx, sty,
							etx, ety,
							&alpha[0],
							&alpha[1],
							&collinear
						)
						&& (alpha[0] > 0.0)
						&& (alpha[0] < 1.0)
					)
					opqinsert(SIMPLE, curedge->alpha[i], &alphalist);
					break;
				}
				impossible("polyline(II:AT0)");
				break;
			case AT1:
				/* should be taken care of by next AT0 */
				break;
			case ON0:
			case ON1:
				X = hypot(prevedge->etx - x0, prevedge->ety - y0) > fabs(radius);
				Y = hypot(curedge->next->stx - x0, curedge->next->sty - y0) > fabs(radius);
				dprintf "X=%d Y=%d\n", X, Y);
				Z = X && Y;
				W = !X && !Y;
				if (Z || W)
					opqinsert((curedge->code[i] == ON0)?EXT0:EXT1, curedge->alpha[i], &alphalist);
				else
					opqinsert((curedge->code[i] == ON0)?INFL0:INFL1, curedge->alpha[i], &alphalist);
				break;
			case TANGENT:
				opqinsert(IGNORE, curedge->alpha[i], &alphalist);
				break;
			default:
				impossible ("polyline(B)");
				break;
			}
		}
		prevedge = curedge;
		curedge = curedge->next;
	} while (prevedge != edgelist);
	opqinsert(INHERIT, rprin(startang), &alphalist);
	opqinsert(INHERIT, rprin(endang), &alphalist);
	if (dbg) {
		fprintf (stderr, "interalpha:\n");
		for (interwalk = alphalist;
			interwalk;
			interwalk = interwalk->next)
			fprintf (stderr, "%d %f, ", interwalk->code, interwalk->alpha);
		fprintf (stderr, "\n");
	}
	((OPQPTR) tail((NAMEPTR) alphalist))->next = alphalist;
	interwalk = alphalist;
	onedge = FALSE;
	do {
		switch (interwalk->code) {
		case EXT0:
			alpha[0] = interwalk->alpha;
			onedge = TRUE;
			break;
		case EXT1:
			alpha[1] = interwalk->alpha;
			onedge = TRUE;
			break;
		case INFL0:
			alpha[0] = interwalk->alpha;
			onedge = TRUE;
			break;
		case INFL1:
			alpha[1] = interwalk->alpha;
			onedge = TRUE;
			break;
		default:
			break;
		}
		interwalk = interwalk->next;
	} while (interwalk != alphalist);
	if (onedge) {
		rem = modf(fabs(alpha[0]-alpha[1])/(2.0*PI), &dummy);
		if (rem < EPSILON || fabs(1.0-rem) < EPSILON)
			return;
	}
	interwalk = alphalist;
	do {
		if (interwalk->code == EXT0 || interwalk->code == INFL0 || interwalk->code == INHERIT)
			interwalk = interwalk->next;
		else
			break;
	} while (interwalk != alphalist);
	inside = ptinpoly (
		edgelist,
		x0 + fabs(radius)*cos((interwalk->alpha + interwalk->next->alpha)/2.0),
		y0 + fabs(radius)*sin((interwalk->alpha + interwalk->next->alpha)/2.0)
	);
	dprintf "inside: %d\n", inside);
	alphalist = interwalk->next;
	interwalk = alphalist;
	onedge = FALSE;
	do {
		switch (interwalk->code) {
		case SIMPLE:
			interwalk->code = (!inside)?INBEGIN:OUTBEGIN;
			inside = !inside;
			break;
		case EXT1:
			interwalk->code = inside?INBEGIN:OUTBEGIN;
			onedge = FALSE;
			break;
		case EXT0:
			interwalk->code = ONBEGIN;
			onedge = TRUE;
			break;
		case INFL1:
			interwalk->code = (!inside)?INBEGIN:OUTBEGIN;
			inside = !inside;
			onedge = FALSE;
			break;
		case INFL0:
			interwalk->code = ONBEGIN;
			onedge = TRUE;
			break;
		case INHERIT:
		case IGNORE:
			interwalk->code = onedge?ONBEGIN:(inside?INBEGIN:OUTBEGIN);
			break;
		default:
			impossible("polyline(C)");
			break;
		}
		interwalk = interwalk->next;
	} while (interwalk != alphalist);
	while (alphalist->alpha < alphalist->next->alpha)
		alphalist = alphalist->next;
	alphalist = alphalist->next;
	if (dbg) {
		fprintf (stderr, "interalpha:\n");
		interwalk = alphalist;
		do {
			fprintf (stderr, "%d %f, ", interwalk->code, interwalk->alpha);
			interwalk = interwalk->next;
		} while (interwalk != alphalist);
		fprintf (stderr, "\n");
	}
	interwalk = alphalist;
	do {
		if (interwalk->alpha > interwalk->next->alpha)
			break;
		if (endang < 2.0*PI + EPSILON) {
			if (interwalk->alpha < startang - EPSILON || interwalk->alpha > endang + EPSILON) {
				dprintf "arc rejected (A)\n");
				interwalk = interwalk->next;
				continue;
			}
			if (interwalk->next->alpha < startang - EPSILON || interwalk->next->alpha > endang + EPSILON) {
				dprintf "arc rejected (B)\n");
				interwalk = interwalk->next;
				continue;
			}
		} else {
			if (interwalk->alpha < startang - EPSILON && interwalk->alpha > endang + EPSILON - 2.0*PI) {
				dprintf "arc rejected (C)\n");
				interwalk = interwalk->next;
				continue;
			}
			if (interwalk->next->alpha < startang - EPSILON && interwalk->next->alpha > endang + EPSILON - 2.0*PI) {
				dprintf "arc rejected (D)\n");
				interwalk = interwalk->next;
				continue;
			}
		}
		linewalk = angularc (
			x0, y0,
			radius,
			interwalk->alpha,
			interwalk->next->alpha
		);
		switch (interwalk->code) {
		case INBEGIN:
			inwalk->next = linewalk;
			inwalk = inwalk->next;
			break;
		case OUTBEGIN:
			outwalk->next = linewalk;	
			outwalk = outwalk->next;
			break;
		case ONBEGIN:
			tryfree(linewalk);
			break;
		default:
			impossible("polyline(D)");
			break;
		}
		interwalk = interwalk->next;
	} while (interwalk != alphalist);
	*inlines = nuin.next;
	*outlines = nuout.next;
	*both = NULL;
}
