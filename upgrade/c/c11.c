333,334c333,334
< jmp	*L%d(r0)\n\
< .data\n\
---
> asl	r0\n\
> jmp	L%d(r0)\n\
338,347d337
< char	simpsw[] {"\
< mov	$L%d,r1\n\
< mov	r0,L%d\n\
< L%d:cmp	r0,(r1)+\n\
< jne	L%d\n\
< jmp	*L%d-L%d(r1)\n\
< .data\n\
< L%d:\
< "};
< 
353,359c343,344
< add	$L%d,r1\n\
< mov	r0,*(r1)+\n\
< mov	(r1)+,r1\n\
< L%d:cmp	r0,-(r1)\n\
< jne	L%d\n\
< jmp	*L%d-L%d(r1)\n\
< .data\n\
---
> asl	r1\n\
> jmp	L%d(r1)\n\
390c375
< 				printf("L%d\n", fp->swlab);
---
> 				printf("012707\nL%d\n", fp->swlab);
393c378
< 				printf("L%d\n", deflab);
---
> 				printf("012707\nL%d\n", deflab);
399,401d383
< 		i = isn++;
< 		j = isn++;
< 		printf(simpsw, i, j, isn, isn, j, i, i);
403,408c385,392
< 		for (; fp<=lp; fp++)
< 			printf("%o\n", fp->swval);
< 		printf("L%d:..\n", j);
< 		for (fp = afp; fp<=lp; fp++)
< 			printf("L%d\n", fp->swlab);
< 		printf("L%d\n", deflab);
---
> 		for (; fp<=lp; fp++) {
> 			if (fp->swval)
> 				printf("cmp	r0,$%o\n", fp->swval);
> 			else
> 				printf("tst	r0\n");
> 			printf("jeq	L%d\n", fp->swlab);
> 		}
> 		printf("jbr	L%d\n", deflab);
429c413
< 	printf(hashsw, tabs, isn, i, i, isn+tabs+1, isn+1, isn);
---
> 	printf(hashsw, tabs, i, i);
431,432c415,416
< 	for (i=0; i<=tabs; i++)
< 		printf("L%d\n", isn+i);
---
> 	for (i=0; i<tabs; i++)
> 		printf("012707\nL%d\n", isn+i);
434c418
< 		printf("L%d:..\n", isn++);
---
> 		printf("L%d:\n", isn++);
436,437c420,427
< 			if (lrem(0, swp->swval, tabs) == i)
< 				printf("%o\n", ldiv(0, swp->swval, tabs));
---
> 			if (lrem(0, swp->swval, tabs) == i) {
> 				if (ldiv(0, swp->swval, tabs))
> 					printf("cmp	r0,$%o\n", ldiv(0, swp->swval, tabs));
> 				else
> 					printf("tst	r0\n");
> 				printf("jeq	L%d\n", swp->swlab);
> 			}
> 		printf("jbr	L%d\n", deflab);
439,445d428
< 	printf("L%d:", isn++);
< 	for (i=0; i<tabs; i++) {
< 		printf("L%d\n", deflab);
< 		for (swp=fp; swp<=lp; swp++)
< 			if (lrem(0, swp->swval, tabs) == i)
< 				printf("L%d\n", swp->swlab);
< 	}
447c430
< 	printf(".text\n");
---
> 	;
