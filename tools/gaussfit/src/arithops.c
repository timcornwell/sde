
#include <stdio.h>
#include <math.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "opcodes.p"
#include "machine.h"
#include "alloc.h"

void
Negate() {
	long in1, unode;
	
	reg[regn++] = in1 = unode = popval();   /* pop item on top of stack */
	trace4list(in1);
	while (unode > -1) {
		ValueOf(unode) = -ValueOf(unode);
		unode = NextOf(unode);
	}
	pushval(in1);
}
	

void
Add() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();   /* pop two items on top of stack */
	reg[regn++] = in1 = popval();  
	trace4list(in1);
	trace4list(in2);
	pushval(MergeNodeLists(in1,in2));
}

void
Subtract() {
	Negate();
	Add();
}

/* SingleCross creates a cross term node */

#define SingleCross(u, v, val)													\
	NextOf(rnode) = rnext = GetFreeRecord(nodes); /* allocate node */			\
	newnode = datumptr + rnext;													\
	*newnode = mtNode2;															\
	newnode->deriv[0] = datumptr[u].deriv[0];     /* copy 1st var */			\
	newnode->deriv[1] = datumptr[v].deriv[0];     /* copy 2nd var */			\
	newnode->value = val;   /* set expression value */							\
	rnode = rnext;			/* link node in chain */							\
/* end of macro SingleCross */


void
Multiply() {
	long unode, vnode, unode0, vnode0, rnode, rnext;
	long in1, in2, out1, out2;
	short type;
	DatumPtr newnode;
	double uval, vval;
	int compare, uok, vok;
	
	reg[regn++] = in1 = unode = popval();   /* pop two items on top of stack */
	reg[regn++] = in2 = vnode = popval();  
	trace4list(in1);
	trace4list(in2);
	uval  = ValueOf(unode);
	vval  = ValueOf(vnode);
	
	out1 = rnode = NewNode(0);			/* create 0th order term */
	ValueOf(rnode) = uval * vval;
	
	unode0 = unode = NextOf(unode);
	vnode0 = vnode = NextOf(vnode);
	/* traverse both lists in order, simultaneously */
	while (unode > -1 || vnode > -1) {
		compare = CompareNodes(unode, vnode);
		if (compare==0) { /* nodes of same variable in both lists */
			rnext = CopyRecord(nodes, unode);
			/*   u * dv/dx + v * du/dx   -or-   u * d2v/dx2 + v * d2u/dx2   */
			ValueOf(rnext) = vval * ValueOf(unode)
				+ uval * ValueOf(vnode);
			unode = NextOf(unode);
			vnode = NextOf(vnode);
		} else if (compare < 0) {
			rnext = CopyRecord(nodes, unode);
			/*   v * du/dx   -or-   v * d2u/dx2   */
			ValueOf(rnext) = vval * ValueOf(unode);
			unode = NextOf(unode);
		} else {
			rnext = CopyRecord(nodes, vnode);
			/*   u * dv/dx   -or-   u * d2v/dx2   */
			ValueOf(rnext) = uval * ValueOf(vnode);
			vnode = NextOf(vnode);
		}
		NextOf(rnode) = rnext;
		rnode = rnext;
	}
	
	/* compute 2nd order terms */
	
	if (do2ndDerivs) {
		out2 = rnode = NewNode(0);		/* create 0th order term (value = 0.0) */
		/*
		unode = unode0;
		vnode = vnode0;
		*/
		/* scan through both lists in order, simultaneously */
		while (unode0 > -1 && OrderOf(unode0) == 1 
			|| vnode0 > -1 && OrderOf(unode0) == 1) {
			compare = CompareNodes(unode0, vnode0);
			unode = unode0;
			vnode = vnode0;
			if (compare==0) {
				type = VarTypeOf(unode0, 0);
				if    (type == ObservationType && doObservations
					|| type == ParameterType   && doParams) {
					uval = ValueOf(unode0);
					vval = ValueOf(vnode0);
					
					/*   2 * du/dx * dv/dx   */
					SingleCross(unode0, unode, 2. * uval * vval);
					unode = NextOf(unode);
					vnode = NextOf(vnode);
					
					/*
						hang on to first var of pair and scan from cur pos
						in both lists in order simultaneously to combine with
						all other variables 
					*/
					while (unode > -1 && OrderOf(unode) == 1 
						|| vnode > -1 && OrderOf(vnode) == 1) {
						compare = CompareNodes(unode, vnode);
						if (compare==0) {
							/*   du/dx * dv/dy + dv/dx * du/dy   */
							type = VarTypeOf(unode, 0);
							if    (type == ObservationType && doObservations
								|| type == ParameterType   && doParams) {
								SingleCross(unode0, unode,
									uval * ValueOf(vnode) +
									vval * ValueOf(unode));
							}
							unode = NextOf(unode);
							vnode = NextOf(vnode);
						} else if (compare < 0) {
							/*   dv/dx * du/dy   */
							type = VarTypeOf(unode, 0);
							if    (type == ObservationType && doObservations
								|| type == ParameterType   && doParams) {
								SingleCross(vnode0, unode, vval*ValueOf(unode));
							}
							unode = NextOf(unode);
						} else {
							/*   du/dx * dv/dy   */
							type = VarTypeOf(vnode, 0);
							if    (type == ObservationType && doObservations
								|| type == ParameterType   && doParams) {
								SingleCross(unode0, vnode, uval*ValueOf(vnode));
							}
							vnode = NextOf(vnode);
						}
					}
				}
				unode0 = NextOf(unode0);
				vnode0 = NextOf(vnode0);
			} else if (compare<0) {
				/* current var in list u is not in list v */
				/* combine var in list u with terms in list v */
				type = VarTypeOf(unode0, 0);
				if    (type == ObservationType && doObservations
					|| type == ParameterType   && doParams) {
					uval = ValueOf(unode0);
					while (vnode > -1 && OrderOf(vnode) == 1) {
						type = VarTypeOf(vnode, 0);
						if    (type == ObservationType && doObservations
							|| type == ParameterType   && doParams) {
							/*   du/dx * dv/dy   */
							SingleCross(unode0, vnode, uval * ValueOf(vnode));
						}
						vnode = NextOf(vnode);
					}
				}
				unode0 = NextOf(unode0);
			} else {
				/* current var in list v is not in list u */
				/* combine var in list v with terms in list u */
				type = VarTypeOf(vnode0, 0);
				if    (type == ObservationType && doObservations
					|| type == ParameterType   && doParams) {
					vval = ValueOf(vnode0);
					while (unode > -1 && OrderOf(unode) == 1) {
						type = VarTypeOf(unode, 0);
						if    (type == ObservationType && doObservations
							|| type == ParameterType   && doParams) {
							/*   dv/dx * du/dy   */
							SingleCross(vnode0, unode, vval * ValueOf(unode));
						}
						unode = NextOf(unode);
					}
				}
				vnode0 = NextOf(vnode0);
			}
		}
		out1 = MergeNodeLists(out1, out2);
	}
	FreeNodeList(in1);
	FreeNodeList(in2);
	pushval(out1);
}

void
Divide() {
	long unode, vnode, unode0, vnode0, rnode, rnext;
	long in1, in2, out1, out2;
	DatumPtr newnode;
	double uval, vval;
	double vmin1, vmin2, uv2, uv3, vmin22;
	int compare, uok, vok;
	short type;
	
	reg[regn++] = in1 = vnode = popval();   /* pop two items on top of stack */
	reg[regn++] = in2 = unode = popval();  
	trace4list(in1);
	trace4list(in2);
	uval  = ValueOf(unode);
	vval  = ValueOf(vnode);
	vmin1 = 1.0 / vval;
	vmin2 = - vmin1 * vmin1;		/* -1/v2 */
	uv2   = uval * vmin2;			/* -u/v2 */
	if (do2ndDerivs) {
		vmin22 = vmin2 * 2.0;			/* -2/v2 */
		uv3   = -2.0 * vmin1 * uv2;		/* 2u/v3 */
	}
	
	out1 = rnode = NewNode(0);			/* create 0th order term */
	ValueOf(rnode) = uval * vmin1;
	
	unode0 = unode = NextOf(unode);
	vnode0 = vnode = NextOf(vnode);
	while (unode > -1 || vnode > -1) {
		compare = CompareNodes(unode, vnode);
		if (compare==0) {
			rnext = CopyRecord(nodes, unode);
			if (OrderOf(unode) == 1) {
				/*   1/v * du/dx - u/v2 * dv/dx   */
				ValueOf(rnext) = vmin1 * ValueOf(unode)
					+ uv2 * ValueOf(vnode);
			} else {
				/*   1/v * d2u/dx2 + 2u/v3 * d2v/dx2   */
				ValueOf(rnext) = vmin1 * ValueOf(unode)
					+ uv3 * ValueOf(vnode);
			}
			unode = NextOf(unode);
			vnode = NextOf(vnode);
		} else if (compare < 0) {
			rnext = CopyRecord(nodes, unode);
			/*   1/v * du/dx   -or-   1/v * d2u/dx2   */
			ValueOf(rnext) = vmin1 * ValueOf(unode);
			unode = NextOf(unode);
		} else {
			rnext = CopyRecord(nodes, vnode);
			if (OrderOf(vnode) == 1) {
				/*   u/v2 * dv/dx   */
				ValueOf(rnext) = uv2 * ValueOf(vnode);
			} else {
				/*   2u/v3 * d2v/dx2   */
				ValueOf(rnext) = uv3 * ValueOf(vnode);
			}
			vnode = NextOf(vnode);
		}
		NextOf(rnode) = rnext;
		rnode = rnext;
	}
	
	/* compute cross terms */
	
	if (do2ndDerivs) {
		out2 = rnode = NewNode(0);		/* create 0th order term (value = 0.0) */
		unode = unode0;
		vnode = vnode0;
		/* scan through both lists in order, simultaneously */
		while (unode0 > -1 && OrderOf(unode0) == 1 
			|| vnode0 > -1 && OrderOf(unode0) == 1) {
			compare = CompareNodes(unode0, vnode0);
			unode = unode0;
			vnode = vnode0;
			if (compare==0) {
			
				type = VarTypeOf(unode0, 0);
				if    (type == ObservationType && doObservations
					|| type == ParameterType   && doParams) {
					/* matched pair */
					/*   -1/v2 * du/dx * dv/dx   */
					SingleCross(unode0, unode, 
						vmin2 * ValueOf(unode0) * ValueOf(vnode0));
					unode = NextOf(unode);
					vnode = NextOf(vnode);
					
					/*
						hang on to first var of pair and scan from cur pos
						in both lists in order simultaneously to combine with
						all other vars 
					*/
					uval = vmin2 * ValueOf(unode0);
					vval = vmin2 * ValueOf(vnode0);
					while (unode > -1 && OrderOf(unode) == 1 
						|| vnode > -1 && OrderOf(vnode) == 1) {
						compare = CompareNodes(unode, vnode);
						if (compare==0) {
							/*   -1/v2 * du/dx * dv/dy + -1/v2 * dv/dx * du/dy   */
							type = VarTypeOf(unode, 0);
							if    (type == ObservationType && doObservations
								|| type == ParameterType   && doParams) {
								SingleCross(unode0, unode, 
									uval * ValueOf(vnode) +
									vval * ValueOf(unode) );
							}
							unode = NextOf(unode);
							vnode = NextOf(vnode);
						} else if (compare < 0) {
							/*   -1/v2 * dv/dx * du/dy   */
							type = VarTypeOf(unode, 0);
							if    (type == ObservationType && doObservations
								|| type == ParameterType   && doParams) {
								SingleCross(vnode0, unode, vval * ValueOf(unode));
							}
							unode = NextOf(unode);
						} else {
							/*   -1/v2 * du/dx * dv/dy   */
							type = VarTypeOf(vnode, 0);
							if    (type == ObservationType && doObservations
								|| type == ParameterType   && doParams) {
								SingleCross(unode0, vnode, uval * ValueOf(vnode));
							}
							vnode = NextOf(vnode);
						}
					}
				}
				unode0 = NextOf(unode0);
				vnode0 = NextOf(vnode0);
			} else if (compare<0) {
				/* current var in list u is not in list v */
				/* combine var in list u with terms in list v */
				type = VarTypeOf(unode0, 0);
				if    (type == ObservationType && doObservations
					|| type == ParameterType   && doParams) {
					uval = vmin2 * ValueOf(unode0);
					while (vnode > -1 && OrderOf(vnode) == 1) {
						/*   -1/v2 * du/dx * dv/dy   */
						type = VarTypeOf(vnode, 0);
						if    (type == ObservationType && doObservations
							|| type == ParameterType   && doParams) {
							SingleCross(unode0, vnode, uval * ValueOf(vnode));
						}
						vnode = NextOf(vnode);
					}
				}
				unode0 = NextOf(unode0);
			} else {
				/* current var in list v is not in list u */
				/* combine var in list v with terms in list u */
				type = VarTypeOf(vnode0, 0);
				if    (type == ObservationType && doObservations
					|| type == ParameterType   && doParams) {
					vval = vmin2 * ValueOf(vnode0);
					while (unode > -1 && OrderOf(unode) == 1) {
						/*   -1/v2 * dv/dx * du/dy   */
						type = VarTypeOf(unode, 0);
						if    (type == ObservationType && doObservations
							|| type == ParameterType   && doParams) {
							SingleCross(vnode0, unode, vval * ValueOf(unode));
						}
						unode = NextOf(unode);
					}
				}
				vnode0 = NextOf(vnode0);
			}
		}
		out1 = MergeNodeLists(out1, out2);
	}
	FreeNodeList(in1);
	FreeNodeList(in2);
	pushval(out1);
}

