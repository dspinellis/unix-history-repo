/* Copyright (c) 1979 Regents of the University of California */
    /*
     *	put[1234]
     *	these sort of replace the assembler code
     *	which used to mung the stack inserting 1, 2, 3, or 4 and then
     *	jmp ( not jsr ) to put.  these are more portable,
     *	but since they can only receive integer arguments,  calls
     *	to one of these with long or real arguments must be changed
     *	to call put directly.
     */

    /*
     *	is there some reason why these aren't #defined?
     */

put1 ( arg1 )
    int		arg1;
    {
	return ( put ( 1 , arg1 ) );
    }

put2 ( arg1 , arg2 )
    int		arg1 , arg2;
    {
	return ( put ( 2 , arg1 , arg2 ) );
    }

put3 ( arg1 , arg2 , arg3 )
    int		arg1 , arg2 , arg3;
    {
	return ( put ( 3 , arg1 , arg2 , arg3 ) );
    }

put4 ( arg1 , arg2 , arg3 , arg4 )
    int		arg1 , arg2 , arg3 , arg4;
    {
	return ( put ( 4 , arg1 , arg2 , arg3 , arg4 ) );
    }

