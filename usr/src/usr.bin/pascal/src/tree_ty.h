struct tnode
{
    int tag;
    union
    {
	struct FOR_NODE
	{
	    int		line_no; 	/* line number of for */
	    struct	tnode	*init_asg;	/* initialization */
	    struct	tnode	*term_expr;	/* termination expresssion */
	    struct	tnode	*for_stmnt;	/* for statement */
	} for_node;
	struct ASG_NODE
	{
	    int		line_no;	/* line number of := */
	    struct	tnode	*lhs_var;
	    struct	tnode	*rhs_expr;
	} asg_node;
	struct VAR_NODE
	{
	    int 	line_no;
	    char	*cptr;
	    struct	tnode	*qual;
	    struct	tnode	*fields;
	} var_node;
	struct EXPR_NODE
	{
	    int		const_tag;
	    struct	tnode	*lhs;
	    struct	tnode	*rhs;
	} expr_node;
    } tree_ele;
};
