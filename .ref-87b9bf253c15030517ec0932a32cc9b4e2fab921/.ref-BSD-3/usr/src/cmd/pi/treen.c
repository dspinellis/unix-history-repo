/* Copyright (c) 1979 Regents of the University of California */
    /*
     *	is there some reason why these aren't #defined?
     */

tree1 ( arg1 )
    int		arg1;
    {
	tree ( 1 , arg1 );
    }

tree2 ( arg1 , arg2 )
    int		arg1 , arg2;
    {
	tree ( 2 , arg1 , arg2 );
    }

tree3 ( arg1 , arg2 , arg3 )
    int		arg1 , arg2 , arg3;
    {
	tree ( 3 , arg1 , arg2 , arg3 );
    }

tree4 ( arg1 , arg2 , arg3 , arg4 )
    int		arg1 , arg2 , arg3 , arg4;
    {
	tree ( 4 , arg1 , arg2 , arg3 , arg4 );
    }

tree5 ( arg1 , arg2 , arg3 , arg4 , arg5 )
    int		arg1 , arg2 , arg3 , arg4 , arg5;
    {
	tree ( 5 , arg1 , arg2 , arg3 , arg4 , arg5 );
    }

