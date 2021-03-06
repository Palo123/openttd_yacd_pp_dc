/* $Id: os_timer.cpp 17248 2009-08-21 20:21:05Z rubidium $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file os_timer.cpp OS/compiler dependant real time tick sampling. */

#include "stdafx.h"

#undef RDTSC_AVAILABLE

/* rdtsc for MSC_VER, uses simple inline assembly, or _rdtsc
 * from external win64.asm because VS2005 does not support inline assembly */
#if defined(_MSC_VER) && !defined(RDTSC_AVAILABLE) && !defined(WINCE)
#include <intrin.h>
uint64 ottd_rdtsc()
{
	return __rdtsc();
}
#define RDTSC_AVAILABLE
#endif

/* rdtsc for OS/2. Hopefully this works, who knows */
#if defined (__WATCOMC__) && !defined(RDTSC_AVAILABLE)
unsigned __int64 ottd_rdtsc();
# pragma aux ottd_rdtsc = 0x0F 0x31 value [edx eax] parm nomemory modify exact [edx eax] nomemory;
# define RDTSC_AVAILABLE
#endif

/* rdtsc for all other *nix-en (hopefully). Use GCC syntax */
#if (defined(__i386__) || defined(__x86_64__)) && !defined(__DJGPP__) && !defined(RDTSC_AVAILABLE)
uint64 ottd_rdtsc()
{
	uint32 high, low;
	__asm__ __volatile__ ("rdtsc" : "=a" (low), "=d" (high));
	return ((uint64)high << 32) | low;
}
# define RDTSC_AVAILABLE
#endif

/* rdtsc for PPC which has this not */
#if (defined(__POWERPC__) || defined(__powerpc__)) && !defined(RDTSC_AVAILABLE)
uint64 ottd_rdtsc()
{
	uint32 high = 0, high2 = 0, low;
	/* PPC does not have rdtsc, so we cheat by reading the two 32-bit time-counters
	 * it has, 'Move From Time Base (Upper)'. Since these are two reads, in the
	 * very unlikely event that the lower part overflows to the upper part while we
	 * read it; we double-check and reread the registers */
	asm volatile (
				  "mftbu %0\n"
				  "mftb %1\n"
				  "mftbu %2\n"
				  "cmpw %3,%4\n"
				  "bne- $-16\n"
				  : "=r" (high), "=r" (low), "=r" (high2)
				  : "0" (high), "2" (high2)
				  );
	return ((uint64)high << 32) | low;
}
# define RDTSC_AVAILABLE
#endif

/* In all other cases we have no support for rdtsc. No major issue,
 * you just won't be able to profile your code with TIC()/TOC() */
#if !defined(RDTSC_AVAILABLE)
/* MSVC (in case of WinCE) can't handle #warning */
# if !defined(_MSC_VER)
#warning "(non-fatal) No support for rdtsc(), you won't be able to profile with TIC/TOC"
# endif
uint64 ottd_rdtsc() {return 0;}
#endif
