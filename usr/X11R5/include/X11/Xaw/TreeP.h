/*
 * $XConsortium: TreeP.h,v 1.13 90/04/13 16:39:54 jim Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 * Copyright 1989 Prentice Hall
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose and without fee is hereby granted, provided that the above
 * copyright notice appear in all copies and that both the copyright notice
 * and this permission notice appear in supporting documentation.
 * 
 * M.I.T., Prentice Hall and the authors disclaim all warranties with regard
 * to this software, including all implied warranties of merchantability and
 * fitness.  In no event shall M.I.T., Prentice Hall or the authors be liable
 * for any special, indirect or cosequential damages or any damages whatsoever
 * resulting from loss of use, data or profits, whether in an action of
 * contract, negligence or other tortious action, arising out of or in
 * connection with the use or performance of this software.
 * 
 * Authors:  Jim Fulton, MIT X Consortium,
 *           based on a version by Douglas Young, Prentice Hall
 * 
 * This widget is based on the Tree widget described on pages 397-419 of
 * Douglas Young's book "The X Window System, Programming and Applications 
 * with Xt OSF/Motif Edition."  The layout code has been rewritten to use
 * additional blank space to make the structure of the graph easier to see
 * as well as to support vertical trees.
 */


#ifndef _XawTreeP_h
#define _XawTreeP_h

#include <X11/Xaw/Tree.h>

typedef struct _TreeClassPart {
    int ignore;
} TreeClassPart;

typedef struct _TreeClassRec {
    CoreClassPart core_class;
    CompositeClassPart composite_class;
    ConstraintClassPart constraint_class;
    TreeClassPart tree_class;
} TreeClassRec;

extern TreeClassRec treeClassRec;

typedef struct {
    /* fields available through resources */
    Dimension hpad;			/* hSpace/HSpace */
    Dimension vpad;			/* vSpace/VSpace */
    Dimension line_width;		/* lineWidth/LineWidth */
    Pixel foreground;			/* foreground/Foreground */
    XtGravity gravity;			/* gravity/Gravity */
    Boolean auto_reconfigure;		/* autoReconfigure/AutoReconfigure */
    /* private fields */
    GC gc;				/* used to draw lines */
    Widget tree_root;			/* hidden root off all children */
    Dimension *largest;			/* list of largest per depth */
    int n_largest;			/* number of elements in largest */
    Dimension maxwidth, maxheight;	/* for shrink wrapping */
} TreePart;


typedef struct _TreeRec {
    CorePart core;
    CompositePart composite;
    ConstraintPart constraint;
    TreePart tree;
}  TreeRec;


/*
 * structure attached to all children
 */
typedef struct _TreeConstraintsPart {
    /* resources */
    Widget parent;			/* treeParent/TreeParent */
    GC gc;				/* treeGC/TreeGC */
    /* private data */
    Widget *children;
    int n_children;
    int max_children;
    Dimension bbsubwidth, bbsubheight;	/* bounding box of sub tree */
    Dimension bbwidth, bbheight;	/* bounding box including node */
    Position x, y;
} TreeConstraintsPart;

typedef struct _TreeConstraintsRec {
   TreeConstraintsPart tree;
} TreeConstraintsRec, *TreeConstraints;


/*
 * useful macros
 */

#define TREE_CONSTRAINT(w) \
                   ((TreeConstraints)((w)->core.constraints))

#define TREE_INITIAL_DEPTH 10		/* for allocating largest array */
#define TREE_HORIZONTAL_DEFAULT_SPACING 20
#define TREE_VERTICAL_DEFAULT_SPACING 6

#endif /* _XawTreeP_h */



