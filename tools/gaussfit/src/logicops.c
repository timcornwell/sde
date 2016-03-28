
#include <stdio.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "protoplasm.h"
#include "opcodes.p"
#include "machine.h"
#include "alloc.h"



void
PushTrue() {
	long node;
	node = NewNode(0);
	datumptr[node].value = 1.0;
	pushval(node);
}

void
PushFalse() {
	long node;
	node = NewNode(0);
	datumptr[node].value = 0.0;
	pushval(node);
}

void
NotFn() {
	long in1, in2;
	
	reg[regn++] = in1 = popval();
	trace4list(in1);
	if (datumptr[in1].value == 0.0) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
}


void
AndFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value && datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
OrFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value || datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
ExclusiveOrFn() {
	long in1, in2;
	int val1, val2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	val1 = datumptr[in1].value != 0.0;
	val2 = datumptr[in2].value != 0.0;
	if ((val1 || val2) && !(val1 && val2)) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
EqualsFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value == datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
NotEqualsFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value != datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
LessThanFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value < datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
GreaterThanFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value > datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
LessOrEqualFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value <= datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

void
GreaterOrEqualFn() {
	long in1, in2;
	
	reg[regn++] = in2 = popval();
	reg[regn++] = in1 = popval();
	trace4list(in1);
	trace4list(in2);
	if (datumptr[in1].value >= datumptr[in2].value) PushTrue();
	else PushFalse();
	FreeNodeList(in1);
	FreeNodeList(in2);
}

