/* picoc external interface. This should be the only header you need to use if
 * you're using picoc as a library. Internal details are in interpreter.h */
#ifndef PICOC_H
#define PICOC_H

/* picoc version number */
#ifdef VER
#define PICOC_VERSION "v2.2 beta r" VER         /* VER is the subversion version number, obtained via the Makefile */
#else
#define PICOC_VERSION "v2.2"
#endif

/* handy definitions */
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#include "interpreter.h"


#if defined(UNIX_HOST) || defined(WIN32)
#include <setjmp.h>

/* this has to be a macro, otherwise errors will occur due to the stack being corrupt */
#define PicocPlatformSetExitPoint(pc) setjmp((pc)->PicocExitBuf)
#endif

#ifdef SURVEYOR_HOST
/* mark where to end the program for platforms which require this */
extern int PicocExitBuf[];

#define PicocPlatformSetExitPoint(pc) setjmp((pc)->PicocExitBuf)
#endif

/* parse.c */
EXPORT void PicocParse(Picoc *pc, const char *FileName, const char *Source, int SourceLen, int RunIt, int CleanupNow, int CleanupSource, int EnableDebugger);
EXPORT void PicocParseInteractive(Picoc *pc);

/* platform.c */
EXPORT void PicocCallMain(Picoc *pc, int argc, char **argv);
EXPORT void PicocInitialise(Picoc *pc, int StackSize);
EXPORT void PicocCleanup(Picoc *pc);
EXPORT void PicocPlatformScanFile(Picoc *pc, const char *FileName);

/* include.c */
EXPORT void PicocIncludeAllSystemHeaders(Picoc *pc);

#endif /* PICOC_H */
