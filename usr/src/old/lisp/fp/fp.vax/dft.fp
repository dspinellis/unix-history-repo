# Discrete Fourier Transform
# Usage: dft : b
# Where "b" is the input vector

{pi  	%3.141592653589793}

{wN	1}
{p	2}
{r	2}
{B	1}

{realCDiv	&/ @ distr @ reverse}

{distMult &* @ distl}

{iota0	apndl @ [%0,
		 iota @ - @ [id,%1]
		]
}

{oddp	= @  [%1 , mod @ [id,%2]]}

{cAdd		&+ @ trans}
       
{reCxIp	!cAdd @ &&* @ &distl @ trans}

{cExp	[cos , sin]}

{N 	length @ 1}

{w 	cExp @  / @ [!* @  [%-2, pi, p],
		     wN
		    ]
}

{ws 	cExp @  + @ [pi,
		     / @ [!* @  [%-2, pi, p],
		            wN
		      	 ]
		    ]

}


{wFactors	&(oddp @ 3 ->
		       ws @ [1,* @ tl];
		       w  @ [1,* @ tl]) @
		&apndl @ 
		distl @
		[N,
		 distl @ [r, iota0 @ N]
		]
}





{dftPt	realCDiv  @  [N,
		     reCxIp @ [B, wFactors]
		     ]
}
  
{dft	&dftPt @ distl @ [id,iota0 @ length]}

{b %<1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.5>}

{d %<0.0, 0.5, 1.0, 1.0>}

{e %<
1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.5, 1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.5,
1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.5, 1.0, 2.0, 3.0, 4.0, 3.0, 2.0, 1.0, 0.5>}
