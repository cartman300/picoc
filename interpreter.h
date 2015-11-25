/* picoc main header file - this has all the main data structures and 
 * function prototypes. If you're just calling picoc you should look at the
 * external interface instead, in picoc.h */
 
#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "platform.h"


/* handy definitions */
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef min
#define min(x,y) (((x)<(y))?(x):(y))
#endif

#define MEM_ALIGN(x) (((x) + sizeof(ALIGN_TYPE) - 1) & ~(sizeof(ALIGN_TYPE)-1))

#define GETS_BUF_MAX 256

/* for debugging */
#define PRINT_SOURCE_POS ({ PrintSourceTextErrorLine(Parser->pc->CStdOut, Parser->FileName, Parser->SourceText, Parser->Line, Parser->CharacterPos); PlatformPrintf(Parser->pc->CStdOut, "\n"); })
#define PRINT_TYPE(typ) PlatformPrintf(Parser->pc->CStdOut, "%t\n", typ);

/* small processors use a simplified FILE * for stdio, otherwise use the system FILE * */
#ifdef BUILTIN_MINI_STDLIB
typedef struct OutputStream IOFILE;
#else
typedef FILE IOFILE;
#endif

/* coercion of numeric types to other numeric types */
#ifndef NO_FP
#define IS_FP(v) ((v)->Typ->Base == TypeFP)
#define FP_VAL(v) ((v)->Val->FP)
#else
#define IS_FP(v) 0
#define FP_VAL(v) 0
#endif

#define IS_POINTER_COERCIBLE(v, ap) ((ap) ? ((v)->Typ->Base == TypePointer) : 0)
#define POINTER_COERCE(v) ((int)(v)->Val->Pointer)

#define IS_INTEGER_NUMERIC_TYPE(t) ((t)->Base >= TypeInt && (t)->Base <= TypeUnsignedLong)
#define IS_INTEGER_NUMERIC(v) IS_INTEGER_NUMERIC_TYPE((v)->Typ)
#define IS_NUMERIC_COERCIBLE(v) (IS_INTEGER_NUMERIC(v) || IS_FP(v))
#define IS_NUMERIC_COERCIBLE_PLUS_POINTERS(v,ap) (IS_NUMERIC_COERCIBLE(v) || IS_POINTER_COERCIBLE(v,ap))


struct Table;
struct Picoc_Struct;

typedef struct Picoc_Struct Picoc;

/* lexical tokens */
enum LexToken
{
    /* 0x00 */ TokenNone, 
    /* 0x01 */ TokenComma,
    /* 0x02 */ TokenAssign, TokenAddAssign, TokenSubtractAssign, TokenMultiplyAssign, TokenDivideAssign, TokenModulusAssign,
    /* 0x08 */ TokenShiftLeftAssign, TokenShiftRightAssign, TokenArithmeticAndAssign, TokenArithmeticOrAssign, TokenArithmeticExorAssign,
    /* 0x0d */ TokenQuestionMark, TokenColon, 
    /* 0x0f */ TokenLogicalOr, 
    /* 0x10 */ TokenLogicalAnd, 
    /* 0x11 */ TokenArithmeticOr, 
    /* 0x12 */ TokenArithmeticExor, 
    /* 0x13 */ TokenAmpersand, 
    /* 0x14 */ TokenEqual, TokenNotEqual, 
    /* 0x16 */ TokenLessThan, TokenGreaterThan, TokenLessEqual, TokenGreaterEqual,
    /* 0x1a */ TokenShiftLeft, TokenShiftRight, 
    /* 0x1c */ TokenPlus, TokenMinus, 
    /* 0x1e */ TokenAsterisk, TokenSlash, TokenModulus,
    /* 0x21 */ TokenIncrement, TokenDecrement, TokenUnaryNot, TokenUnaryExor, TokenSizeof, TokenCast,
    /* 0x27 */ TokenLeftSquareBracket, TokenRightSquareBracket, TokenDot, TokenArrow, 
    /* 0x2b */ TokenOpenBracket, TokenCloseBracket,
    /* 0x2d */ TokenIdentifier, TokenIntegerConstant, TokenFPConstant, TokenStringConstant, TokenCharacterConstant,
    /* 0x32 */ TokenSemicolon, TokenEllipsis,
    /* 0x34 */ TokenLeftBrace, TokenRightBrace,
    /* 0x36 */ TokenIntType, TokenCharType, TokenFloatType, TokenDoubleType, TokenVoidType, TokenEnumType,
    /* 0x3c */ TokenLongType, TokenSignedType, TokenShortType, TokenStaticType, TokenAutoType, TokenRegisterType, TokenExternType, TokenStructType, TokenUnionType, TokenUnsignedType, TokenTypedef,
    /* 0x46 */ TokenContinue, TokenDo, TokenElse, TokenFor, TokenGoto, TokenIf, TokenWhile, TokenBreak, TokenSwitch, TokenCase, TokenDefault, TokenReturn,
    /* 0x52 */ TokenHashDefine, TokenHashInclude, TokenHashIf, TokenHashIfdef, TokenHashIfndef, TokenHashElse, TokenHashEndif,
    /* 0x59 */ TokenNew, TokenDelete,
    /* 0x5b */ TokenOpenMacroBracket,
    /* 0x5c */ TokenEOF, TokenEndOfLine, TokenEndOfFunction
};

/* used in dynamic memory allocation */
struct AllocNode
{
    unsigned int Size;
    struct AllocNode *NextFree;
};

/* whether we're running or skipping code */
enum RunMode
{
    RunModeRun,                 /* we're running code as we parse it */
    RunModeSkip,                /* skipping code, not running */
    RunModeReturn,              /* returning from a function */
    RunModeCaseSearch,          /* searching for a case label */
    RunModeBreak,               /* breaking out of a switch/while/do */
    RunModeContinue,            /* as above but repeat the loop */
    RunModeGoto                 /* searching for a goto label */
};

/* parser state - has all this detail so we can parse nested files */
struct ParseState
{
    Picoc *pc;                  /* the picoc instance this parser is a part of */
    const unsigned char *Pos;   /* the character position in the source text */
    char *FileName;             /* what file we're executing (registered string) */
    short int Line;             /* line number we're executing */
    short int CharacterPos;     /* character/column in the line we're executing */
    enum RunMode Mode;          /* whether to skip or run code */
    int SearchLabel;            /* what case label we're searching for */
    const char *SearchGotoLabel;/* what goto label we're searching for */
    const char *SourceText;     /* the entire source text */
    short int HashIfLevel;      /* how many "if"s we're nested down */
    short int HashIfEvaluateToLevel;    /* if we're not evaluating an if branch, what the last evaluated level was */
    char DebugMode;             /* debugging mode */
    int ScopeID;                /* for keeping track of local variables (free them after they go out of scope) */
};

/* values */
enum BaseType
{
    TypeVoid,                   /* no type */
    TypeInt,                    /* integer */
    TypeShort,                  /* short integer */
    TypeChar,                   /* a single character (signed) */
    TypeLong,                   /* long integer */
    TypeUnsignedInt,            /* unsigned integer */
    TypeUnsignedShort,          /* unsigned short integer */
    TypeUnsignedChar,           /* unsigned 8-bit number */ /* must be before unsigned long */
    TypeUnsignedLong,           /* unsigned long integer */
#ifndef NO_FP
    TypeFP,                     /* floating point */
#endif
    TypeFunction,               /* a function */
    TypeMacro,                  /* a macro */
    TypePointer,                /* a pointer */
    TypeArray,                  /* an array of a sub-type */
    TypeStruct,                 /* aggregate type */
    TypeUnion,                  /* merged type */
    TypeEnum,                   /* enumerated integer type */
    TypeGotoLabel,              /* a label we can "goto" */
    Type_Type                   /* a type for storing types */
};

/* data type */
struct ValueType
{
    enum BaseType Base;             /* what kind of type this is */
    int ArraySize;                  /* the size of an array type */
    int Sizeof;                     /* the storage required */
    int AlignBytes;                 /* the alignment boundary of this type */
    const char *Identifier;         /* the name of a struct or union */
    struct ValueType *FromType;     /* the type we're derived from (or NULL) */
    struct ValueType *DerivedTypeList;  /* first in a list of types derived from this one */
    struct ValueType *Next;         /* next item in the derived type list */
    struct Table *Members;          /* members of a struct or union */
    int OnHeap;                     /* true if allocated on the heap */
    int StaticQualifier;            /* true if it's a static */
};

/* function definition */
struct FuncDef
{
    struct ValueType *ReturnType;   /* the return value type */
    int NumParams;                  /* the number of parameters */
    int VarArgs;                    /* has a variable number of arguments after the explicitly specified ones */
    struct ValueType **ParamType;   /* array of parameter types */
    char **ParamName;               /* array of parameter names */
    void (*Intrinsic)();            /* intrinsic call address or NULL */
    struct ParseState Body;         /* lexical tokens of the function body if not intrinsic */
};

/* macro definition */
struct MacroDef
{
    int NumParams;                  /* the number of parameters */
    char **ParamName;               /* array of parameter names */
    struct ParseState Body;         /* lexical tokens of the function body if not intrinsic */
};

/* values */
union AnyValue
{
    char Character;
    short ShortInteger;
    int Integer;
    long LongInteger;
    unsigned short UnsignedShortInteger;
    unsigned int UnsignedInteger;
    unsigned long UnsignedLongInteger;
    unsigned char UnsignedCharacter;
    char *Identifier;
    char ArrayMem[2];               /* placeholder for where the data starts, doesn't point to it */
    struct ValueType *Typ;
    struct FuncDef FuncDef;
    struct MacroDef MacroDef;
#ifndef NO_FP
    double FP;
#endif
    void *Pointer;                  /* unsafe native pointers */
};

struct Value
{
    struct ValueType *Typ;          /* the type of this value */
    union AnyValue *Val;            /* pointer to the AnyValue which holds the actual content */
    struct Value *LValueFrom;       /* if an LValue, this is a Value our LValue is contained within (or NULL) */
    char ValOnHeap;                 /* this Value is on the heap */
    char ValOnStack;                /* the AnyValue is on the stack along with this Value */
    char AnyValOnHeap;              /* the AnyValue is separately allocated from the Value on the heap */
    char IsLValue;                  /* is modifiable and is allocated somewhere we can usefully modify it */
    int ScopeID;                    /* to know when it goes out of scope */
    char OutOfScope;
};

/* hash table data structure */
struct TableEntry
{
    struct TableEntry *Next;        /* next item in this hash chain */
    const char *DeclFileName;       /* where the variable was declared */
    unsigned short DeclLine;
    unsigned short DeclColumn;

    union TableEntryPayload
    {
        struct ValueEntry
        {
            char *Key;              /* points to the shared string table */
            struct Value *Val;      /* the value we're storing */
        } v;                        /* used for tables of values */
        
        char Key[1];                /* dummy size - used for the shared string table */
        
        struct BreakpointEntry      /* defines a breakpoint */
        {
            const char *FileName;
            short int Line;
            short int CharacterPos;
        } b;
        
    } p;
};
    
struct Table
{
    short Size;
    short OnHeap;
    struct TableEntry **HashTable;
};

/* stack frame for function calls */
struct StackFrame
{
    struct ParseState ReturnParser;         /* how we got here */
    const char *FuncName;                   /* the name of the function we're in */
    struct Value *ReturnValue;              /* copy the return value here */
    struct Value **Parameter;               /* array of parameter values */
    int NumParams;                          /* the number of parameters */
    struct Table LocalTable;                /* the local variables and parameters */
    struct TableEntry *LocalHashTable[LOCAL_TABLE_SIZE];
    struct StackFrame *PreviousStackFrame;  /* the next lower stack frame */
};

/* lexer state */
enum LexMode
{
    LexModeNormal,
    LexModeHashInclude,
    LexModeHashDefine,
    LexModeHashDefineSpace,
    LexModeHashDefineSpaceIdent
};

struct LexState
{
    const char *Pos;
    const char *End;
    const char *FileName;
    int Line;
    int CharacterPos;
    const char *SourceText;
    enum LexMode Mode;
    int EmitExtraNewlines;
};

/* library function definition */
struct LibraryFunction
{
    void (*Func)(struct ParseState *Parser, struct Value *, struct Value **, int);
    const char *Prototype;
};

/* output stream-type specific state information */
union OutputStreamInfo
{
    struct StringOutputStream
    {
        struct ParseState *Parser;
        char *WritePos;
    } Str;
};

/* stream-specific method for writing characters to the console */
typedef void CharWriter(unsigned char, union OutputStreamInfo *);

/* used when writing output to a string - eg. sprintf() */
struct OutputStream
{
    CharWriter *Putch;
    union OutputStreamInfo i;
};

/* possible results of parsing a statement */
enum ParseResult { ParseResultEOF, ParseResultError, ParseResultOk };

/* a chunk of heap-allocated tokens we'll cleanup when we're done */
struct CleanupTokenNode
{
    void *Tokens;
    const char *SourceText;
    struct CleanupTokenNode *Next;
};

/* linked list of lexical tokens used in interactive mode */
struct TokenLine
{
    struct TokenLine *Next;
    unsigned char *Tokens;
    int NumBytes;
};


/* a list of libraries we can include */
struct IncludeLibrary
{
    char *IncludeName;
    void (*SetupFunction)(Picoc *pc);
    struct LibraryFunction *FuncList;
    const char *SetupCSource;
    struct IncludeLibrary *NextLib;
};

#define FREELIST_BUCKETS 8                          /* freelists for 4, 8, 12 ... 32 byte allocs */
#define SPLIT_MEM_THRESHOLD 16                      /* don't split memory which is close in size */
#define BREAKPOINT_TABLE_SIZE 21


/* the entire state of the picoc system */
struct Picoc_Struct
{
    /* parser global data */
    struct Table GlobalTable;
    struct CleanupTokenNode *CleanupTokenList;
    struct TableEntry *GlobalHashTable[GLOBAL_TABLE_SIZE];
    
    /* lexer global data */
    struct TokenLine *InteractiveHead;
    struct TokenLine *InteractiveTail;
    struct TokenLine *InteractiveCurrentLine;
    int LexUseStatementPrompt;
    union AnyValue LexAnyValue;
    struct Value LexValue;
    struct Table ReservedWordTable;
    struct TableEntry *ReservedWordHashTable[RESERVED_WORD_TABLE_SIZE];

    /* the table of string literal values */
    struct Table StringLiteralTable;
    struct TableEntry *StringLiteralHashTable[STRING_LITERAL_TABLE_SIZE];
    
    /* the stack */
    struct StackFrame *TopStackFrame;

    /* the value passed to exit() */
    int PicocExitValue;

    /* a list of libraries we can include */
    struct IncludeLibrary *IncludeLibList;

    /* heap memory */
#ifdef USE_MALLOC_STACK
    unsigned char *HeapMemory;          /* stack memory since our heap is malloc()ed */
    void *HeapBottom;                   /* the bottom of the (downward-growing) heap */
    void *StackFrame;                   /* the current stack frame */
    void *HeapStackTop;                 /* the top of the stack */
#else
# ifdef SURVEYOR_HOST
    unsigned char *HeapMemory;          /* all memory - stack and heap */
    void *HeapBottom;                   /* the bottom of the (downward-growing) heap */
    void *StackFrame;                   /* the current stack frame */
    void *HeapStackTop;                 /* the top of the stack */
    void *HeapMemStart;
# else
    unsigned char HeapMemory[HEAP_SIZE];  /* all memory - stack and heap */
    void *HeapBottom;                   /* the bottom of the (downward-growing) heap */
    void *StackFrame;                   /* the current stack frame */
    void *HeapStackTop;                 /* the top of the stack */
# endif
#endif

    struct AllocNode *FreeListBucket[FREELIST_BUCKETS];      /* we keep a pool of freelist buckets to reduce fragmentation */
    struct AllocNode *FreeListBig;                           /* free memory which doesn't fit in a bucket */

    /* types */    
    struct ValueType UberType;
    struct ValueType IntType;
    struct ValueType ShortType;
    struct ValueType CharType;
    struct ValueType LongType;
    struct ValueType UnsignedIntType;
    struct ValueType UnsignedShortType;
    struct ValueType UnsignedLongType;
    struct ValueType UnsignedCharType;
    #ifndef NO_FP
    struct ValueType FPType;
    #endif
    struct ValueType VoidType;
    struct ValueType TypeType;
    struct ValueType FunctionType;
    struct ValueType MacroType;
    struct ValueType EnumType;
    struct ValueType GotoLabelType;
    struct ValueType *CharPtrType;
    struct ValueType *CharPtrPtrType;
    struct ValueType *CharArrayType;
    struct ValueType *VoidPtrType;

    /* debugger */
    struct Table BreakpointTable;
    struct TableEntry *BreakpointHashTable[BREAKPOINT_TABLE_SIZE];
    int BreakpointCount;
    int DebugManualBreak;
    
    /* C library */
    int BigEndian;
    int LittleEndian;

    IOFILE *CStdOut;
    IOFILE CStdOutBase;

    /* the picoc version string */
    const char *VersionString;
    
    /* exit longjump buffer */
#if defined(UNIX_HOST) || defined(WIN32)
    jmp_buf PicocExitBuf;
#endif
#ifdef SURVEYOR_HOST
    int PicocExitBuf[41];
#endif
    
    /* string table */
    struct Table StringTable;
    struct TableEntry *StringHashTable[STRING_TABLE_SIZE];
    char *StrEmpty;
};

/* table.c */
EXPORT void TableInit(Picoc *pc);
EXPORT char *TableStrRegister(Picoc *pc, const char *Str);
EXPORT char *TableStrRegister2(Picoc *pc, const char *Str, int Len);
EXPORT void TableInitTable(struct Table *Tbl, struct TableEntry **HashTable, int Size, int OnHeap);
EXPORT int TableSet(Picoc *pc, struct Table *Tbl, char *Key, struct Value *Val, const char *DeclFileName, int DeclLine, int DeclColumn);
EXPORT int TableGet(struct Table *Tbl, const char *Key, struct Value **Val, const char **DeclFileName, int *DeclLine, int *DeclColumn);
EXPORT struct Value *TableDelete(Picoc *pc, struct Table *Tbl, const char *Key);
EXPORT char *TableSetIdentifier(Picoc *pc, struct Table *Tbl, const char *Ident, int IdentLen);
EXPORT void TableStrFree(Picoc *pc);

/* lex.c */
EXPORT void LexInit(Picoc *pc);
EXPORT void LexCleanup(Picoc *pc);
EXPORT void *LexAnalyse(Picoc *pc, const char *FileName, const char *Source, int SourceLen, int *TokenLen);
EXPORT void LexInitParser(struct ParseState *Parser, Picoc *pc, const char *SourceText, void *TokenSource, char *FileName, int RunIt, int SetDebugMode);
EXPORT enum LexToken LexGetToken(struct ParseState *Parser, struct Value **Value, int IncPos);
EXPORT enum LexToken LexRawPeekToken(struct ParseState *Parser);
EXPORT void LexToEndOfLine(struct ParseState *Parser);
EXPORT void *LexCopyTokens(struct ParseState *StartParser, struct ParseState *EndParser);
EXPORT void LexInteractiveClear(Picoc *pc, struct ParseState *Parser);
EXPORT void LexInteractiveCompleted(Picoc *pc, struct ParseState *Parser);
EXPORT void LexInteractiveStatementPrompt(Picoc *pc);

/* parse.c */
/* the following are defined in picoc.h:
 * void PicocParse(const char *FileName, const char *Source, int SourceLen, int RunIt, int CleanupNow, int CleanupSource);
 * void PicocParseInteractive(); */
EXPORT void PicocParseInteractiveNoStartPrompt(Picoc *pc, int EnableDebugger);
EXPORT enum ParseResult ParseStatement(struct ParseState *Parser, int CheckTrailingSemicolon);
EXPORT struct Value *ParseFunctionDefinition(struct ParseState *Parser, struct ValueType *ReturnType, char *Identifier);
EXPORT void ParseCleanup(Picoc *pc);
EXPORT void ParserCopyPos(struct ParseState *To, struct ParseState *From);
EXPORT void ParserCopy(struct ParseState *To, struct ParseState *From);

/* expression.c */
EXPORT int ExpressionParse(struct ParseState *Parser, struct Value **Result);
EXPORT long ExpressionParseInt(struct ParseState *Parser);
EXPORT void ExpressionAssign(struct ParseState *Parser, struct Value *DestValue, struct Value *SourceValue, int Force, const char *FuncName, int ParamNo, int AllowPointerCoercion);
EXPORT long ExpressionCoerceInteger(struct Value *Val);
EXPORT unsigned long ExpressionCoerceUnsignedInteger(struct Value *Val);
#ifndef NO_FP
EXPORT double ExpressionCoerceFP(struct Value *Val);
#endif

/* type.c */
EXPORT void TypeInit(Picoc *pc);
EXPORT void TypeCleanup(Picoc *pc);
EXPORT int TypeSize(struct ValueType *Typ, int ArraySize, int Compact);
EXPORT int TypeSizeValue(struct Value *Val, int Compact);
EXPORT int TypeStackSizeValue(struct Value *Val);
EXPORT int TypeLastAccessibleOffset(Picoc *pc, struct Value *Val);
EXPORT int TypeParseFront(struct ParseState *Parser, struct ValueType **Typ, int *IsStatic);
EXPORT void TypeParseIdentPart(struct ParseState *Parser, struct ValueType *BasicTyp, struct ValueType **Typ, char **Identifier);
EXPORT void TypeParse(struct ParseState *Parser, struct ValueType **Typ, char **Identifier, int *IsStatic);
EXPORT struct ValueType *TypeGetMatching(Picoc *pc, struct ParseState *Parser, struct ValueType *ParentType, enum BaseType Base, int ArraySize, const char *Identifier, int AllowDuplicates);
EXPORT struct ValueType *TypeCreateOpaqueStruct(Picoc *pc, struct ParseState *Parser, const char *StructName, int Size);
EXPORT int TypeIsForwardDeclared(struct ParseState *Parser, struct ValueType *Typ);

/* heap.c */
EXPORT void HeapInit(Picoc *pc, int StackSize);
EXPORT void HeapCleanup(Picoc *pc);
EXPORT void *HeapAllocStack(Picoc *pc, int Size);
EXPORT int HeapPopStack(Picoc *pc, void *Addr, int Size);
EXPORT void HeapUnpopStack(Picoc *pc, int Size);
EXPORT void HeapPushStackFrame(Picoc *pc);
EXPORT int HeapPopStackFrame(Picoc *pc);
EXPORT void *HeapAllocMem(Picoc *pc, int Size);
EXPORT void HeapFreeMem(Picoc *pc, void *Mem);

/* variable.c */
EXPORT void VariableInit(Picoc *pc);
EXPORT void VariableCleanup(Picoc *pc);
EXPORT void VariableFree(Picoc *pc, struct Value *Val);
EXPORT void VariableTableCleanup(Picoc *pc, struct Table *HashTable);
EXPORT void *VariableAlloc(Picoc *pc, struct ParseState *Parser, int Size, int OnHeap);
EXPORT void VariableStackPop(struct ParseState *Parser, struct Value *Var);
EXPORT struct Value *VariableAllocValueAndData(Picoc *pc, struct ParseState *Parser, int DataSize, int IsLValue, struct Value *LValueFrom, int OnHeap);
EXPORT struct Value *VariableAllocValueAndCopy(Picoc *pc, struct ParseState *Parser, struct Value *FromValue, int OnHeap);
EXPORT struct Value *VariableAllocValueFromType(Picoc *pc, struct ParseState *Parser, struct ValueType *Typ, int IsLValue, struct Value *LValueFrom, int OnHeap);
EXPORT struct Value *VariableAllocValueFromExistingData(struct ParseState *Parser, struct ValueType *Typ, union AnyValue *FromValue, int IsLValue, struct Value *LValueFrom);
EXPORT struct Value *VariableAllocValueShared(struct ParseState *Parser, struct Value *FromValue);
EXPORT struct Value *VariableDefine(Picoc *pc, struct ParseState *Parser, char *Ident, struct Value *InitValue, struct ValueType *Typ, int MakeWritable);
EXPORT struct Value *VariableDefineButIgnoreIdentical(struct ParseState *Parser, char *Ident, struct ValueType *Typ, int IsStatic, int *FirstVisit);
EXPORT int VariableDefined(Picoc *pc, const char *Ident);
EXPORT int VariableDefinedAndOutOfScope(Picoc *pc, const char *Ident);
EXPORT void VariableRealloc(struct ParseState *Parser, struct Value *FromValue, int NewSize);
EXPORT void VariableGet(Picoc *pc, struct ParseState *Parser, const char *Ident, struct Value **LVal);
EXPORT void VariableDefinePlatformVar(Picoc *pc, struct ParseState *Parser, char *Ident, struct ValueType *Typ, union AnyValue *FromValue, int IsWritable);
EXPORT void VariableStackFrameAdd(struct ParseState *Parser, const char *FuncName, int NumParams);
EXPORT void VariableStackFramePop(struct ParseState *Parser);
EXPORT struct Value *VariableStringLiteralGet(Picoc *pc, char *Ident);
EXPORT void VariableStringLiteralDefine(Picoc *pc, char *Ident, struct Value *Val);
EXPORT void *VariableDereferencePointer(struct ParseState *Parser, struct Value *PointerValue, struct Value **DerefVal, int *DerefOffset, struct ValueType **DerefType, int *DerefIsLValue);
EXPORT int VariableScopeBegin(struct ParseState * Parser, int* PrevScopeID);
EXPORT void VariableScopeEnd(struct ParseState * Parser, int ScopeID, int PrevScopeID);

/* clibrary.c */
EXPORT void BasicIOInit(Picoc *pc);
EXPORT void LibraryInit(Picoc *pc);
EXPORT void LibraryAdd(Picoc *pc, struct Table *GlobalTable, const char *LibraryName, struct LibraryFunction *FuncList);
EXPORT void CLibraryInit(Picoc *pc);
EXPORT void PrintCh(char OutCh, IOFILE *Stream);
EXPORT void PrintSimpleInt(long Num, IOFILE *Stream);
EXPORT void PrintInt(long Num, int FieldWidth, int ZeroPad, int LeftJustify, IOFILE *Stream);
EXPORT void PrintStr(const char *Str, IOFILE *Stream);
EXPORT void PrintFP(double Num, IOFILE *Stream);
EXPORT void PrintType(struct ValueType *Typ, IOFILE *Stream);
EXPORT void LibPrintf(struct ParseState *Parser, struct Value *ReturnValue, struct Value **Param, int NumArgs);

/* platform.c */
/* the following are defined in picoc.h:
 * void PicocCallMain(int argc, char **argv);
 * int PicocPlatformSetExitPoint();
 * void PicocInitialise(int StackSize);
 * void PicocCleanup();
 * void PicocPlatformScanFile(const char *FileName);
 * extern int PicocExitValue; */
EXPORT void ProgramFail(struct ParseState *Parser, const char *Message, ...);
EXPORT void ProgramFailNoParser(Picoc *pc, const char *Message, ...);
EXPORT void AssignFail(struct ParseState *Parser, const char *Format, struct ValueType *Type1, struct ValueType *Type2, int Num1, int Num2, const char *FuncName, int ParamNo);
EXPORT void LexFail(Picoc *pc, struct LexState *Lexer, const char *Message, ...);
EXPORT void PlatformInit(Picoc *pc);
EXPORT void PlatformCleanup(Picoc *pc);
EXPORT char *PlatformGetLine(char *Buf, int MaxLen, const char *Prompt);
EXPORT int PlatformGetCharacter();
EXPORT void PlatformPutc(unsigned char OutCh, union OutputStreamInfo *);
EXPORT void PlatformPrintf(IOFILE *Stream, const char *Format, ...);
EXPORT void PlatformVPrintf(IOFILE *Stream, const char *Format, va_list Args);
EXPORT void PlatformExit(Picoc *pc, int ExitVal);
EXPORT char *PlatformMakeTempName(Picoc *pc, char *TempNameBuffer);
EXPORT void PlatformLibraryInit(Picoc *pc);

/* include.c */
EXPORT void IncludeInit(Picoc *pc);
EXPORT void IncludeCleanup(Picoc *pc);
EXPORT void IncludeRegister(Picoc *pc, const char *IncludeName, void (*SetupFunction)(Picoc *pc), struct LibraryFunction *FuncList, const char *SetupCSource);
EXPORT void IncludeFile(Picoc *pc, char *Filename);
/* the following is defined in picoc.h:
 * void PicocIncludeAllSystemHeaders(); */
 
/* debug.c */
EXPORT void DebugInit();
EXPORT void DebugCleanup();
EXPORT void DebugCheckStatement(struct ParseState *Parser);


/* stdio.c */
extern const char StdioDefs[];
extern struct LibraryFunction StdioFunctions[];
EXPORT void StdioSetupFunc(Picoc *pc);

/* math.c */
extern struct LibraryFunction MathFunctions[];
EXPORT void MathSetupFunc(Picoc *pc);

/* string.c */
extern struct LibraryFunction StringFunctions[];
EXPORT void StringSetupFunc(Picoc *pc);

/* stdlib.c */
extern struct LibraryFunction StdlibFunctions[];
EXPORT void StdlibSetupFunc(Picoc *pc);

/* time.c */
extern const char StdTimeDefs[];
extern struct LibraryFunction StdTimeFunctions[];
EXPORT void StdTimeSetupFunc(Picoc *pc);

/* errno.c */
EXPORT void StdErrnoSetupFunc(Picoc *pc);

/* ctype.c */
extern struct LibraryFunction StdCtypeFunctions[];

/* stdbool.c */
extern const char StdboolDefs[];
EXPORT void StdboolSetupFunc(Picoc *pc);

/* unistd.c */
extern const char UnistdDefs[];
extern struct LibraryFunction UnistdFunctions[];
EXPORT void UnistdSetupFunc(Picoc *pc);

#endif /* INTERPRETER_H */
