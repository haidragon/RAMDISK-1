/*++

Copyright (c) 1990-2000 Microsoft Corporation, All Rights Reserved

Module Name:
    debug.h

Abstract: 

Author:
    Raju Ramanathan             02/22/2000

Enviroment:
    Kernel Mode Only

Revision History:

--*/

#ifndef _DEBUG_H_
#define _DEBUG_H_

#include <ntddk.h>

#define DBG_COMP_NONE       0x00000000
#define DBG_COMP_INIT       0x00000001
#define DBG_COMP_READ       0x00000002
#define DBG_COMP_WRITE      0x00000004
#define DBG_COMP_IOCTL      0x00000008
#define DBG_COMP_QUEUE      0x00000010
#define DBG_COMP_DPC        0x00000020
#define DBG_COMP_TIME       0x00000040
#define DBG_COMP_PNP        0x00000080
#define DBG_COMP_POWER      0x00000100
#define DBG_COMP_WMI        0x00000200
#define DBG_COMP_ALL        0xFFFFFFFF

#define DBG_LEVEL_ERROR     0x00000000
#define DBG_LEVEL_NOTIFY    0x00000000
#define DBG_LEVEL_WARN      0x00000001
#define DBG_LEVEL_INFO      0x00000002
#define DBG_LEVEL_VERBOSE   0x00000003

#if DBG

//
// use "ed ramdisk!DbgLevel" in WinDbg to modify these
//
extern  ULONG   DbgComp;
extern  ULONG   DbgLevel;

#ifndef DBG_HEADER
#define DBG_HEADER  "RAMDISK: "
#endif

#define DBGPRINT( Component, Level, Fmt )             \
    {                                                 \
        if ( ((DbgComp & Component) == Component) &&  \
            (Level <= DbgLevel) )                     \
            {                                         \
                KdPrint((DBG_HEADER));                \
                KdPrint(Fmt);                         \
            }                                         \
    }

#else
// No debug stuff in free build
    #define DBGPRINT
#endif  // DBG

#endif  // _DEBUG_H_

// EOF



