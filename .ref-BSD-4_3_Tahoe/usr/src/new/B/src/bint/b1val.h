/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1val.h,v 1.4 85/08/22 16:42:19 timo Exp $
*/

#ifndef INTEGRATION

/* Private definitions for grabbing and ref count scheme */

value grab_tlt(); 	/* literal type, it; */

btreeptr grabbtreenode();	/* literal flag, it */
btreeptr copybtree();    	/* btreeptr pnode */
/* Procedure uniqlbtreenode(); */	/* btreeptr *pptr; literal it */
btreeptr ccopybtreenode();	/* btreeptr pnode; literal it */
btreeptr mknewroot();
    /* btreeptr ptr0, itemptr pitm0, btreeptr ptr1, literal it */
/* Procedure relbtree(); */		/* btreeptr pnode; literal it */
/* Procedure freebtreenode(); */	/* btreeptr pnode; */

#endif !INTEGRATION
