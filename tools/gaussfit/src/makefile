
oBJeCtso = gauss.o matrixman.o errhandle.o functs.o\
           arithops.o house.o files.o \
           logicops.o  opcodes.o  robust.o\
           simplefits.o simplermidas.o\
		   compile.o symtab.o ytab.o gfitmain.o\
		   minsum.o quicksort.o  util.o declare.o\
		   array.o datum.o export.o symboltable.o machine.o

gaussfit : $(oBJeCtso) lexyy.h symtab.h symboltable.h def.h robust.h\
		defines.h datum.h house.h simpledefs.h files.h\
		compile.h machine.h alloc.h array.h
	acc -g  $(oBJeCtso)  -o gaussfit -lm 

arithops.o: arithops.c array.h datum.h machine.h protoplasm.h alloc.h
	acc -g  -c   arithops.c
 
array.o: array.c array.h protoplasm.h alloc.h
	acc -g  -c   array.c
 
datum.o: datum.c array.h  symboltable.h datum.h protoplasm.h alloc.h
	acc -g  -c   datum.c
 
declare.o: declare.c array.h  symboltable.h declare.h datum.h\
		protoplasm.h alloc.h
	acc -g  -c   declare.c
 
errhandle.o: errhandle.c defines.h
	acc -g  -c   errhandle.c

export.o: export.c defines.h house.h files.h array.h datum.h symboltable.h protoplasm.h\
		alloc.h
	acc -g  -c   export.c

files.o: files.c defines.h alloc.h files.h strings.h array.h datum.h symboltable.h\
		protoplasm.h alloc.h
	acc -g  -c   files.c

functs.o: functs.c array.h datum.h  machine.h protoplasm.h alloc.h
	acc -g  -c   functs.c

logicops.o: logicops.c array.h datum.h  machine.h protoplasm.h alloc.h
	acc -g  -c   logicops.c

gauss.o: gauss.c array.h datum.h symboltable.h defines.h house.h files.h \
		protoplasm.h alloc.h
	acc -g  -c   gauss.c
 
gfitmain.o: gfitmain.c protoplasm.h alloc.h defines.h
	acc -g  -c   gfitmain.c

house.o: house.c alloc.h house.h files.h array.h datum.h symboltable.h defines.h\
		protoplasm.h
	acc -g  -c   house.c

list.o: list.c defines.h datum.h
	acc -g  -c   list.c

machine.o: machine.c array.h datum.h machine.h declare.h symboltable.h compile.h\
		protoplasm.h alloc.h functs.h
	acc -g  -c   machine.c

matrixman.o: matrixman.c defines.h array.h datum.h symboltable.h house.h files.h\
		robust.h protoplasm.h alloc.h
	acc -g  -c   matrixman.c

opcodes.o: opcodes.c array.h datum.h declare.h symboltable.h machine.h protoplasm.h\
		alloc.h
	acc -g  -c   opcodes.c

symboltable.o: symboltable.c array.h datum.h  symboltable.h protoplasm.h\
		alloc.h
	acc -g  -c   symboltable.c

quicksort.o: quicksort.c 
	acc -g  -c   quicksort.c
 
relops.o: relops.c defines.h datum.h
	acc -g  -c   relops.c

robust.o: robust.c defines.h house.h files.h
	acc -g  -c   robust.c
 
simplefits.o: simplefits.c defines.h protoplasm.h simpledefs.h strings.h
	acc -g  -c   simplefits.c

simplermidas.o: simplermidas.c defines.h strings.h protoplasm.h alloc.h house.h
	acc -g  -c   simplermidas.c

compile.o : compile.c symtab.h def.h compile.h array.h datum.h declare.h symboltable.h\
		machine.h protoplasm.h alloc.h
	acc -c -g    compile.c

symtab.o : symtab.c symtab.h def.h protoplasm.h alloc.h
	acc -c -g    symtab.c

minsum.o: minsum.c defines.h house.h
	acc -g  -c   minsum.c
 
ytab.o : ytab.c array.h alloc.h strings.h symtab.h def.h compile.h defines.h\
		lexyy.h datum.h symboltable.h machine.h protoplasm.h
	acc -c -g    ytab.c

util.o : util.c 
	acc -c -g    util.c
