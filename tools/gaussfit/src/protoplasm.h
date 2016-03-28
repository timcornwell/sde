#include <stdio.h>
#include "array.h"
#include "datum.h"
#include "simpledefs.h"
#include "symtab.h"

/* arithops.c */
void Negate(void);
void Add(void);
void Subtract(void);
void Multiply(void);
void Divide(void);

/* array.c */
char *SizeArray(ArrayPtr, long);
char *SizePool(PoolPtr, long);
ArrayPtr NewArray(long, long, long);
int InitArray(ArrayPtr, long, long, long);
PoolPtr NewPool(long, long, long);
int InitPool(PoolPtr, long, long, long);
char *GetRecordPtr(ArrayPtr, long);
int GetRecord(ArrayPtr, long, char *);
int GetNRecords(ArrayPtr, long, long, char *);
int PutRecord(ArrayPtr, long, char *);
int PutNRecords(ArrayPtr, long, int, char *);
int InsertRecord(ArrayPtr, long, char *);
int InsertNRecords(ArrayPtr, long, long, char *);
int DeleteRecord(ArrayPtr, long, char *);
int DeleteNRecords(ArrayPtr, long, long, char *);
long AddRecords(ArrayPtr, long);
long SubRecords(ArrayPtr, long);
int PushRecord(ArrayPtr, char *);
long PushStack(ArrayPtr);
int PopRecord(ArrayPtr, char *);
int DropNStack(ArrayPtr, long);
int FreeRecord(PoolPtr, long);
int FreePool(PoolPtr);
int DumpPool(FILE *, PoolPtr);
long GetFreeRecord(PoolPtr);
long CopyRecord(PoolPtr, long);

/* compile.c */
int compile(char *);
int init(char *);
int yyerror(char *);
char *itos(int);
int recover(void);
int tlist_token(struct tlist *, int);
char *tlist_symbol(struct tlist *, int);
int ftn_err(struct tlist *);
int recerror(char *, char *, char *);
int labgen(int *, int);
int process_token(char *, int);
int cleanup(void);
int DumpTlist(void);
int echo(char *);

/* datum.c */
char *SizeDatums(PoolPtr, long);
int InitDatumPool(void);
int FreeNodeList(long);
long NodeListLen(long);
long CopyNodeList(long);
long NewNode(int);
long MergeNodeLists(long, long);
int CompareIndices(DerivPtr, DerivPtr);
int CompareVars(DerivPtr, DerivPtr);
int CmpNodeVars(long, long, int, int);
int CompareNodes(long, long);
ArrayBlockPtr InitArrayBlock(ArrayBlockPtr, long, short *);
ArrayBlockPtr NewArrayBlock(short *);
int FreeArrayNodes(long);
long ArrayNodeLen(long);
int DumpNodeList(FILE *, long);
int DumpNode(FILE *, long);
void FreeDatumPool(void);

/* declare.c */
char *SizeDeclareStack(PoolPtr, long);
void InitDeclareStack(void);
void ResetDeclareStack(void);
void DropStackFrame(long);
void DecArrayRef(long);
int IncArrayRef(long);
int DefFunc(void);

/* errhandle.c */
int error(char *);
int fatalerror(char *, char *);
int fatalerror2(char *, int, char *);
int fatalerror2c(char *, char *, char *);
int warningerror(char *, char *, int, int);

/* export.c */
#include "house.h"
int saveexport(long);
int unsaveexport(void);
int doexport(void);
int export(void);
int export2(void);
int exportn(int, long[MATSIZE]);
int exportconstraint(void);
int sumconstraint(void);

/* files.c */
int saveenv(int);
int openenvtable(char *);
char *getenvstr(char *);
double getenvval(char *);
int putenvval(char *, double);
int getenvint(char *);
int datafileopen(char *);
int xparfileopen(void);
int xparopen(char *);
int getitemnumber(void);
int import(void);
double getdataval(char *);
int putresidual(char *, double);
double getresidual(char *);
double gettheobs(char *);
int getdataint(char *);
int putdataval(char *, double);
int BCompare(int, short[5 ]);
int searchmidas(int, int, int, short[5 ]);
int BinSearch(short[5 ]);
int savexpar(int);
int writexpar(void);
int initfindex(void);
int putxparval(int, short[5 ], double);
double getxparval(int, short[5 ]);
int fastindex(int, int, short[5 ]);
int makeindex(int, int, short[5 ]);
int FExchange(int, int);
int FCompare(int, int);
int qwiksort(int, int);
double getparamval(int);
int putparamval(int, double);
int saveparams(void);
int getxparnum(int);
FITS *getenvtabpntr(void);
int load_sigmas(char *, short[5 ], double);
int putsigma(char *, double);
int putindexsigma(char *, short[5 ], double);

/* functs.c */
long DxSquared(long);
void SqrtFn(void);
void SinFn(void);
void CosFn(void);
void LogFn(void);
void Log10Fn(void);
void ExpFn(void);
void TanFn(void);
void ArcSinFn(void);
void ArcCosFn(void);
void ArcTanFn(void);
void AbsFn(void);
void Power(void);

/* gauss.c */
double UpdateValues(void);
int gaussmain(char *, char *);
int Forming_Eqns(void);
int ShowResults(int);
int itlimit(FILE *);
int prolog(void);
int getenvvars(char *);
int Allocate_Param_Space(void);
int Old_into_Current_Params(void);
int Current_into_Old_Params(void);
int epilog(void);
int printold(void);
void checkvar(void);

/* gfitmain.c */
int main(int, char *[]);
int getfilename(char *, char *);

/* house.c */
int rowptr(int);
int colptr(int);
char *MemAlloc(char *, long);
char *Reallocate(char *, long, char *);
int freemem(char *, char *);
double LSB(void);
int CountVars(int);
int DumpColNames(void);
int GetDeltaValue(int, char **, short *, int *, double *);
int GetDeltaParams(int, char **, short *, int *, double *);
int getcolumn(int, char *, short *);
double getdeltas(int, char *, short *);
int PrintMatrix(void);
int PrinttheMatrix(int, int, int);
int PrintResults(void);
int InitHouse(char *, int, double);
int ReserveSpace(void);
int rowspace(void);
int colspace(void);
int matspace(void);
double *rowallocate(void);
double *rowreallocate(char *);
int filecheck(char *);
int printcopyright(FILE *);
int SumInit(int);
int SumAdd(int, char *, short[5 ], double);
int SwitchCol(int, int);
int Modulus(int, int);
int SortPivotColumn(int);
int ApplyConstraintTransform(int);
int ApplyHouseholderTransform(int);
int ApplyTransformation(int);
int CalculateModuli(void);
int CalculateColumnSum(void);
int CalculateRowSum(void);
double TypeModulus(int);
double NameModulus(int);
double CCModulus(int);
double CompareCol(int, int);
int FindPivotColumn(int);
double sign(double);
int SolveLinearSystem(void);
int SumIt(void);
int printiter_results(FILE *, double);
int CovarianceMtx(void);
int PrintCMatrix(FILE *, int, int, int, int, int, int, int);
int prSigma(int, int);
int prIndex(FILE *, short[5 ], int);
int prSIndex(FILE *, short[5 ], int, int);
int printdouble(FILE *, double, int);
int getindexsz(void);
int getmaxprmname(void);
int getdimnum(void);
int Solve1(void);
int SolveMatrix(void);
int triangularize1(void);
int triangularize(void);
int insertresidual(double);
int dump(char *);
int dump8(void);
int d8(int, int);
int printenvtoRes(FILE *);
int printdate(FILE *);
int Get_LastCol(void);
double Get_Sigma(void);
int ChiSquare(void);
int printmessage(FILE *);
int machine_prec(void);
int rmachar(int *);

/* logicops.c */
void PushTrue(void);
void PushFalse(void);
void NotFn(void);
void AndFn(void);
void OrFn(void);
void ExclusiveOrFn(void);
void EqualsFn(void);
void NotEqualsFn(void);
void LessThanFn(void);
void GreaterThanFn(void);
void LessOrEqualFn(void);
void GreaterOrEqualFn(void);

/* machine.c */
int corespace(void);
int runtimeerror(char *, char *);
int traceinstruction(FILE *);
int trace4list(long);
int dumpregisters(FILE *);
int printopcode(FILE *, long);
int setpc2label(char *);
int interpret(void);
int mycompile(char *);
int AnalyzeDatumPool(int);


/* matrixman.c */
int matxpose(MATRIX, MATRIX, int, int);
int matcopy(MATRIX, MATRIX, int, int);
int matadd(MATRIX, MATRIX, MATRIX, int, int);
int matsub(MATRIX, MATRIX, MATRIX, int, int);
int matmpy(MATRIX, MATRIX, MATRIX, int, int, int);
int matmpyd(MATRIX, MATRIX, MATRIX, int, int, int);
int matvecmpy(MATRIX, VECTOR, VECTOR, int, int);
int cholesky(MATRIX, int);
int mat_inv(MATRIX, int);
int ut_inv(MATRIX, int);
int matprint(MATRIX, int, int);
int matfprint(MATRIX, int, int);
int search(int *, char *[MATSIZE ], char *);
int covarname(char *, char *, char *);
double getsigma(char *, char *);
int banner(void);
int computewt(int, long[MATSIZE ], VECTOR, MATRIX);
int qcompar(double *, double *);
int initwobblefix(void);
int addmeandu(int, double);
int getvar_zero();
int getvar_nonzero();

/* minsum.c */
int cl1(int, int, int, int, int, int, double **, double *, double *, int *, int *, int *, double, double *, int *);
int initialize(void);
int phase1costs(void);
int xchrows(int, int);
int phase2costs(void);
int computemarginalcosts(void);
int enteringvector(void);
int sub290(void);
int sub300(void);
int sub320(void);
int sub330(void);
int sub360(void);
int bypassvertices(void);
int xchcolumns(void);
int dogaussjordan(void);
int results(void);
int leavingvector(void);
int testoptimality(void);
int simplex(void);
int initworkspace(void);

/* opcodes.c */
int initstacks(void);
long addconst(double);
int pushval(long);
long pushcopy(long);
long popval(void);
long topval(void);

/* quicksort.c */
int printvec(double *, int);
int quicksort(int, double *);
double median(double *, int);
double MAD(double *, int);

/* robust.c */
double rhoprim(double);
double integrand(double);
double simpson(int);
int getintegral(void);
double interpolate(double, int, double[], double);
double lininterp(double, double[]);
int getfaircon(double);
int gettrimcon(double);
int gettukeycon(double);
int gethubercon(double);
int getcon(void);
double ulength(double[], int, int);
double rhofn(double[], int, int);
double Weightfn(double[], int, int);
double Weightfnp(double[], int, int);
double psifn(double[], int, int);
double psipfn(double[], int, int, int);
double oldpsipfn(double[], int, int, int);

/* simplefits.c */
FITS *fitsopen(char *);
FITS *fitsclose(FITS *,int);
int fitsread(FILE *, FITS *);
int fitswrite(FILE *, FITS *);
char *getfitsstr(FITS *, char *);
int putfitsstr(FITS *, char *, char *);
double getfitsval(FITS *, char *);
int putfitsval(FITS *, char *, double);
int convertstr(char *, OBJECT *, int *);

/* simplemidas2.c */
MIDAS *midasopen(char *);
MIDAS *midaswrite(MIDAS *, char *, int);
int printchars(FILE *, MIDAS *, long, long);
int freethestructure(MIDAS *);
int midasread(FILE *, MIDAS *);
int addspace(MIDAS *);
int loaddouble(MIDAS *, int, int, char *);
int gettableheader(FILE *, MIDAS *);
int makespace(MIDAS *);
double *rowalloc(MIDAS *);
char **charalloc(MIDAS *);
double getmidasval(MIDAS *, char *, int);
char *getmidasstr(MIDAS *, char *, int);
int putmidasval(MIDAS *, char *, int, double);
int putmidasvalcol(MIDAS *, int, int, double);
int putmidasstr(MIDAS *, char *, int, char *);
int putmidasstrcol(MIDAS *, int, int, char *);
int rowrealloc(MIDAS *);
int colheadrealloc(MIDAS *);
int colrealloc(MIDAS *);
int copytable(MIDAS *, char *);
int getnumrows(MIDAS *);
int getnumcols(MIDAS *);
char *getcolname(int, MIDAS *);
int changecolname(MIDAS *, char *, char *);
int setundef(MIDAS *, char *, int, char *);
char *getword(char *, char *);
char *wordalloc(char *);
int getline(char[], int, FILE *);

/* symboltable.c */
char *SizeSymbolTable(PoolPtr, long);
int InitSymbolTable(void);
short AddSymbol(char *);
short findsymbol(char *, int);
int SymbolTablePostCompileCleanup(void);
char *SizeFileVarTable(PoolPtr, long);
int InitFileVarTable(void);
long AddFileVar(short[5 ]);
char *wordfill(char *);
int DumpSymbolTable(void);
int wipelocalindices(void);

/* symtab.c */
struct nlist *insert(char *, int);
struct nlist *lookup(char *);
int is_reserved(char *);
struct nlist *entry_alloc(void);
struct tlist *tlist_alloc(void);
int init_table(void);
int deltab(int);
struct tlist *init_token_list(void);
int dump_symtab(int);
int is_declaration_sysword(int);
int is_valid_lhs(int);

/* util.c */
int notzero(double);
#include <stdio.h>
void xprintf(FILE *file, char *format, ...);

/* ytab.c */
int dumptxtstak(void);
char *poptxt(void);
char *toptxt(void);
int pushtxt(char *);
int yywrap(void);
struct fstack *get_func(char *);
int parm_decl(int);
int yyparse(void);
