/*++

Copyright (c) 1990-2000 Microsoft Corporation, All Rights Reserved

Module Name:

    ramdisk.h

Abstract:

    This file includes data declarations for the Ram Disk driver for NT.

Author:

    Robert Nelson (RobertN) 10-Mar-1993.

Environment:

    Kernel mode only.

Notes:

Revision History:


  Raju Ramanathan (Rajuram)

  Converted the sample driver to Windows 2000   02/22/2000
  Code cleaning                                 04/19/2000

--*/


#ifndef _RAMDISK_H_
#define _RAMDISK_H_

#include <ntddk.h>
#include <ntdddisk.h>
#include "debug.h"

#define NT_DEVICE_NAME                  L"\\Device\\Ramdisk"
#define DOS_DEVICE_NAME                 L"\\DosDevices\\"

#define RAMDISK_TAG_GENERAL             '1maR'  // "Ram1" - generic tag
#define RAMDISK_TAG_DISK                '2maR'  // "Ram2" - disk memory tag
#define DOS_DEVNAME_LENGTH              (sizeof(DOS_DEVICE_NAME)+sizeof(WCHAR)*10)
#define DRIVE_LETTER_LENGTH             (sizeof(WCHAR)*10)

#define RAMDISK_DRIVER_EXTENSION_KEY    ((PVOID) DriverEntry)
#define RAMDISK_MEDIA_TYPE              0xF8
#define DIR_ENTRIES_PER_SECTOR          16

#define FLAG_LINK_CREATED               0x00000001

#define REMLOCK_TAG                     'lmaR'
#define REMLOCK_MAXIMUM                 1        // Max minutes system allows lock to be held
#define REMLOCK_HIGHWATER               10       // Max number of irps holding lock at one time

#if DBG
#define DEFAULT_BREAK_ON_ENTRY          0                   // No break
#define DEFAULT_DEBUG_LEVEL             (DBG_LEVEL_ERROR)   // Log only errors
#define DEFAULT_DEBUG_COMP              (DBG_COMP_ALL)
#endif

#define DEFAULT_DISK_SIZE               (1024*1024)     // 1 MB
#define DEFAULT_ROOT_DIR_ENTRIES        512
#define DEFAULT_SECTORS_PER_CLUSTER     2
#define DEFAULT_DRIVE_LETTER            L"Z:"

typedef enum  _DEVICE_STATE {
    STOPPED,                    // Dvice stopped
    WORKING,                    // Started and working
    PENDINGSTOP,                // Stop pending
    PENDINGREMOVE,              // Remove pending
    SURPRISEREMOVED,            // Surprise removed
    REMOVED,                    // Removed
    MAX_STATE                   // Unknown state -Some error
} DEVICE_STATE, *PDEVICE_STATE;

#if DBG
typedef struct _DEBUG_INFO {
    ULONG   BreakOnEntry;       // Break into debugger 
    ULONG   DebugLevel;         // Debug log level
    ULONG   DebugComp;          // Components to log debug message
} DEBUG_INFO, *PDEBUG_INFO;
#endif

typedef struct _DISK_INFO {
    ULONG   DiskSize;           // Ramdisk size in bytes
    ULONG   RootDirEntries;     // No. of root directory entries
    ULONG   SectorsPerCluster;  // Sectors per cluster
    UNICODE_STRING DriveLetter; // Drive letter to be used
} DISK_INFO, *PDISK_INFO;

typedef struct _RAMDISK_DRIVER_EXTENSION {
    UNICODE_STRING  RegistryPath;
	ULONG           DeviceInitialized;
} RAMDISK_DRIVER_EXTENSION, *PRAMDISK_DRIVER_EXTENSION;

typedef struct _DEVICE_EXTENSION {
    PDEVICE_OBJECT      DeviceObject;               // Back pointer to device object
    PDEVICE_OBJECT      LowerDeviceObject;          // Target device object
    PDEVICE_OBJECT      PhysicalDeviceObject;       // Physica device object
    DEVICE_STATE        DevState;                   // Current device state
    IO_REMOVE_LOCK      RemoveLock;                 // Remove lock to avoid abnormal device removal
    ULONG               Flags;                      // General device flag
    PUCHAR              DiskImage;                  // Pointer to beginning of disk image
    DISK_GEOMETRY       DiskGeometry;               // Drive parameters built by Ramdisk
    DISK_INFO           DiskRegInfo;                // Disk parameters from the registry
    UNICODE_STRING      SymbolicLink;               // Dos symbolic name; Drive letter
} DEVICE_EXTENSION, *PDEVICE_EXTENSION;


#pragma pack(1)

typedef struct  _BOOT_SECTOR
{
    UCHAR       bsJump[3];          // x86 jmp instruction, checked by FS
    CCHAR       bsOemName[8];       // OEM name of formatter
    USHORT      bsBytesPerSec;      // Bytes per Sector
    UCHAR       bsSecPerClus;       // Sectors per Cluster
    USHORT      bsResSectors;       // Reserved Sectors
    UCHAR       bsFATs;             // Number of FATs - we always use 1
    USHORT      bsRootDirEnts;      // Number of Root Dir Entries
    USHORT      bsSectors;          // Number of Sectors
    UCHAR       bsMedia;            // Media type - we use RAMDISK_MEDIA_TYPE
    USHORT      bsFATsecs;          // Number of FAT sectors
    USHORT      bsSecPerTrack;      // Sectors per Track - we use 32
    USHORT      bsHeads;            // Number of Heads - we use 2
    ULONG       bsHiddenSecs;       // Hidden Sectors - we set to 0
    ULONG       bsHugeSectors;      // Number of Sectors if > 32 MB size
    UCHAR       bsDriveNumber;      // Drive Number - not used
    UCHAR       bsReserved1;        // Reserved
    UCHAR       bsBootSignature;    // New Format Boot Signature - 0x29
    ULONG       bsVolumeID;         // VolumeID - set to 0x12345678
    CCHAR       bsLabel[11];        // Label - set to RamDisk
    CCHAR       bsFileSystemType[8];// File System Type - FAT12 or FAT16
    CCHAR       bsReserved2[448];   // Reserved
    UCHAR       bsSig2[2];          // Originial Boot Signature - 0x55, 0xAA
}   BOOT_SECTOR, *PBOOT_SECTOR;

typedef struct  _DIR_ENTRY
{
    UCHAR       deName[8];          // File Name
    UCHAR       deExtension[3];     // File Extension
    UCHAR       deAttributes;       // File Attributes
    UCHAR       deReserved;         // Reserved
    USHORT      deTime;             // File Time
    USHORT      deDate;             // File Date
    USHORT      deStartCluster;     // First Cluster of file
    ULONG       deFileSize;         // File Length
}   DIR_ENTRY, *PDIR_ENTRY;

#pragma pack()

//
// Directory Entry Attributes
//

#define DIR_ATTR_READONLY   0x01
#define DIR_ATTR_HIDDEN     0x02
#define DIR_ATTR_SYSTEM     0x04
#define DIR_ATTR_VOLUME     0x08
#define DIR_ATTR_DIRECTORY  0x10
#define DIR_ATTR_ARCHIVE    0x20


//
// MACROS
//
#define COMPLETE_REQUEST( _pIrp, _Status, _Information )    \
        {                                                   \
            ASSERT( _pIrp != NULL );                        \
            ASSERT( KeGetCurrentIrql() <= DISPATCH_LEVEL ); \
            _pIrp->IoStatus.Status = _Status;               \
            _pIrp->IoStatus.Information = _Information;     \
            IoCompleteRequest( _pIrp, IO_NO_INCREMENT );    \
        }


NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
    );

NTSTATUS
RamDiskCreateClose(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

VOID
RamDiskUnload(
    IN PDRIVER_OBJECT DriverObject
    );

NTSTATUS
RamDiskIOCtl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
RamDiskReadWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
RamDiskAddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT PhysicalDeviceObject
    );

NTSTATUS
RamDiskDispatchPnp(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
RamDiskDispatchPower(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
RamDiskDispatchSystemControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
RamDiskIoCompletionRoutine(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
    );

#if DBG
VOID
RamDiskQueryDebugRegParameters(
    IN PUNICODE_STRING RegistryPath
    );
#endif

VOID
RamDiskQueryDiskRegParameters(
    IN PUNICODE_STRING RegistryPath,
    IN PDISK_INFO DiskRegInfo
    );

NTSTATUS
RamDiskFormatDisk(
    IN PDEVICE_OBJECT DeviceObject
    );

VOID
RamDiskCleanUp( 
    IN PDEVICE_OBJECT DeviceObject
    );

VOID
RamDiskRemoveDevice(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

PSTR
GetPnpIrpName(
    IN UCHAR    PnpMinorFunction
    );

#endif    // _RAMDISK_H_




