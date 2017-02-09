/*++

Copyright (c) 1990-2000 Microsoft Corporation, All Rights Reserved

Module Name:

    Ramdisk.c

Abstract:

    This is the Ramdisk sample driver.

Author:

    Robert Nelson (RobertN) 10-Mar-1993.

Environment:

    Kernel mode only.

Notes:
    If the device is not ready, queue the I/O IRPs instead of rejecting

Revision History:
    Added the IOCTL_DISK_GET_PARTITION_INFO query to make it work with NTFS
    driver loaded (thanks Robert Vierthaler (RobertVi)).


  Raju Ramanathan (Rajuram)

  Converted the sample driver to Windows 2000   02/22/2000
  Code cleaning                                 04/19/2000

--*/


#include "ramdisk.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text( INIT, DriverEntry )
#pragma alloc_text( PAGE, RamDiskCreateClose) 
#pragma alloc_text( PAGE, RamDiskUnload)
#pragma alloc_text( PAGE, RamDiskQueryDiskRegParameters )
#pragma alloc_text( PAGE, RamDiskFormatDisk )
#pragma alloc_text( PAGE, RamDiskAddDevice )
#pragma alloc_text( PAGE, RamDiskDispatchPnp )
#pragma alloc_text( PAGE, RamDiskDispatchPower )
#pragma alloc_text( PAGE, RamDiskDispatchSystemControl )
#pragma alloc_text( PAGE, RamDiskRemoveDevice )
#pragma alloc_text( PAGE, GetPnpIrpName )
#if DBG
#pragma alloc_text( PAGE, RamDiskQueryDebugRegParameters )
#endif
#endif // ALLOC_PRAGMA

#if DBG
ULONG  BreakOnEntry = FALSE;
ULONG  DbgLevel = DBG_LEVEL_ERROR;
ULONG  DbgComp  = DBG_COMP_ALL;
// To disable selected components 
//ULONG  DbgComp  = DBG_COMP_ALL & ~DBG_COMP_PNP;
#endif    // DBG


NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
    )

/*++

Routine Description:

    Installable driver initialization entry point.
    This entry point is called directly by the I/O system.

Arguments:

    DriverObject - pointer to the driver object

    RegistryPath - pointer to a unicode string representing the path
                   to driver-specific key in the registry

Return Value:

    STATUS_SUCCESS if successful.

--*/
{
    PRAMDISK_DRIVER_EXTENSION   driverExtension;
    NTSTATUS                    status;

#if DBG
    CHAR VersionHerald[]    = "Windows 2000 Ramdisk Driver - Version %s built on %s\n";
    CHAR VersionNumber[]    = "1.0";
    CHAR VersionTimestamp[] = __DATE__ " " __TIME__;

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_NOTIFY, ( VersionHerald, VersionNumber, VersionTimestamp) );
#endif

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("DriverEntry - IN\n") );


    //
    // Create extension for the driverobject to store driver specific 
    // information. Device specific information should be stored in
    // Device Extension

    status = IoAllocateDriverObjectExtension(DriverObject,
                                             RAMDISK_DRIVER_EXTENSION_KEY,
                                             sizeof(RAMDISK_DRIVER_EXTENSION),
                                             &driverExtension);

    if(!NT_SUCCESS(status)) {
         DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, 
            ("Ramdisk driver extension could not be allocated %lx \n", status ) );
        return status;
    }

    //
    // Copy the registry path into the driver extension so we can use it later
    //

    driverExtension->RegistryPath.Length = RegistryPath->Length;
    driverExtension->RegistryPath.MaximumLength = RegistryPath->MaximumLength + 
                                                    sizeof(UNICODE_NULL);

    driverExtension->RegistryPath.Buffer =
        ExAllocatePoolWithTag(PagedPool,
                              driverExtension->RegistryPath.MaximumLength,
                              RAMDISK_TAG_GENERAL);

    if(driverExtension->RegistryPath.Buffer == NULL) {
        status = STATUS_INSUFFICIENT_RESOURCES;
        return status;
    }

    RtlCopyUnicodeString( &(driverExtension->RegistryPath), RegistryPath);
	driverExtension->DeviceInitialized = FALSE;

#if DBG
    // Query registry paramters

    RamDiskQueryDebugRegParameters( RegistryPath );

    // Break if required
    if ( BreakOnEntry ) {
        KdBreakPoint();
    }
#endif

    //
    // Create dispatch points for Create, Close, Unload, Pnp, Power & WMI
    //

    DriverObject->MajorFunction[IRP_MJ_CREATE]         = RamDiskCreateClose;
    DriverObject->MajorFunction[IRP_MJ_CLOSE]          = RamDiskCreateClose;
    DriverObject->MajorFunction[IRP_MJ_READ]           = RamDiskReadWrite;
    DriverObject->MajorFunction[IRP_MJ_WRITE]          = RamDiskReadWrite;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = RamDiskIOCtl;
    DriverObject->MajorFunction[IRP_MJ_PNP]            = RamDiskDispatchPnp;
    DriverObject->MajorFunction[IRP_MJ_POWER]          = RamDiskDispatchPower;
    DriverObject->MajorFunction[IRP_MJ_SYSTEM_CONTROL] = RamDiskDispatchSystemControl;
    DriverObject->DriverExtension->AddDevice           = RamDiskAddDevice;
    DriverObject->DriverUnload                         = RamDiskUnload;

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("DriverEntry - OUT.\n") );
    return STATUS_SUCCESS;
}   // End of DriverEntry()


NTSTATUS
RamDiskCreateClose(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    This routine is called by the I/O system to create or close a
    handle to the device that we control. 

Arguments:

    DeviceObject - a pointer to the object that represents the device
    Irp - a pointer to the I/O Request Packet for this request.

Return Value:

    STATUS_SUCCESS

--*/
{
    PIO_STACK_LOCATION  irpStack;
    NTSTATUS            status = STATUS_SUCCESS;

    PAGED_CODE();

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("CreateClose - IN\n") );

    irpStack = IoGetCurrentIrpStackLocation( Irp );

    switch ( irpStack->MajorFunction ) {

        case IRP_MJ_CREATE:
            DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("IRP_MJ_CREATE (%p)\n", Irp) );
            COMPLETE_REQUEST( Irp, status, 0 );
            break;

        case IRP_MJ_CLOSE:
            DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("IRP_MJ_CLOSE (%p)\n", Irp) );
            COMPLETE_REQUEST( Irp, status, 0 );
            break;

        default:
            status = STATUS_NOT_IMPLEMENTED;
            COMPLETE_REQUEST( Irp, status, 0 );
            ASSERTMSG("BUG: we should never get here", 0);
            break;

    } // switch

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("CreateClose - OUT\n") );
    return status;
}   // End of RamDiskCreateClose()

VOID
RamDiskUnload(
    IN PDRIVER_OBJECT DriverObject
    )
/*++

Routine Description:

    This routine is called when the driver is unloaded from the
    system. This released the memory allocated during DriverEntry

Arguments:

    DriverObject - a pointer to the driver object 

Return Value:

    None

--*/
{
    PRAMDISK_DRIVER_EXTENSION   driverExtension;

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("Driver Unload\n") );
    ASSERT(DriverObject->DeviceObject == NULL);
    driverExtension = IoGetDriverObjectExtension(DriverObject,
                                             RAMDISK_DRIVER_EXTENSION_KEY);
    ASSERT ( driverExtension != NULL );
    if ( driverExtension->RegistryPath.Buffer ) {
        ExFreePool( driverExtension->RegistryPath.Buffer );
    }
    return;
}   // End of RamDiskUnload()


NTSTATUS
RamDiskIOCtl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    This is the IOCTL handler for our driver. This is called whenever
    the driver on top sends any IOCTL requests

Arguments:

    DeviceObject - a pointer to the object that represents the device
    Irp - a pointer to the I/O Request Packet for this request.

Return Value:

    Status based on the request

--*/
{

    PIO_STACK_LOCATION  irpStack;
    NTSTATUS            status = STATUS_INVALID_DEVICE_REQUEST;
    ULONG               command;
    ULONG               information = 0;

    PDEVICE_EXTENSION    devExt = DeviceObject->DeviceExtension;

    DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_VERBOSE, ("IOCtl- IN \n" ) );

    status = IoAcquireRemoveLock(&devExt->RemoveLock, Irp);
    if (!NT_SUCCESS(status)) {
        DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_ERROR, ("Acquire RemoveLock failed\n" ) );
        COMPLETE_REQUEST( Irp, status, 0 );
        return status;
    }

    irpStack = IoGetCurrentIrpStackLocation(Irp);
    command = irpStack->Parameters.DeviceIoControl.IoControlCode;

    switch ( command )
    {

    case IOCTL_DISK_GET_PARTITION_INFO: {

        DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_INFO, ("IOCTL_DISK_GET_PARTITION_INFO \n" ) );
        if (irpStack->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof(PARTITION_INFORMATION)) {
            DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_INFO, ("Output buffer too small... \n" ) );
            status = STATUS_BUFFER_TOO_SMALL;       // Inform the caller we need bigger buffer
            information = sizeof(PARTITION_INFORMATION);
        } else {
            PPARTITION_INFORMATION outputBuffer;
            PBOOT_SECTOR    bootSector = (PBOOT_SECTOR) devExt->DiskImage;

            outputBuffer = ( PPARTITION_INFORMATION )Irp->AssociatedIrp.SystemBuffer;

            outputBuffer->PartitionType = 
                (bootSector->bsFileSystemType[4] == '6') ? PARTITION_FAT_16 : PARTITION_FAT_12;

            outputBuffer->BootIndicator       = FALSE;
            outputBuffer->RecognizedPartition = TRUE;
            outputBuffer->RewritePartition    = FALSE;
            outputBuffer->StartingOffset      = RtlConvertUlongToLargeInteger(0);
            outputBuffer->PartitionLength     = RtlConvertUlongToLargeInteger(devExt->DiskRegInfo.DiskSize);
            outputBuffer->HiddenSectors       = (ULONG) (1L);
            outputBuffer->PartitionNumber     = (ULONG) (-1L);

            status = STATUS_SUCCESS;
            information = sizeof( PARTITION_INFORMATION );
        }

        break;
    }                                        

    case IOCTL_DISK_GET_DRIVE_GEOMETRY: {

        DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_INFO, ("IOCTL_DISK_GET_DRIVE_GEOMETRY \n" ) );
        //
        // Return the drive geometry for the ram disk. Note that
        // we return values which were made up to suit the disk size.
        //

        if ( irpStack->Parameters.DeviceIoControl.OutputBufferLength <
            sizeof( DISK_GEOMETRY ) )
        {
            //
            // Instead of returning STATUS_INVALID_PARAMETER, we will return
            // STATUS_BUFFER_TOO_SMALL and the required buffer size. 
            // So that the called will send a bigger buffer
            //
            status = STATUS_BUFFER_TOO_SMALL;       // Inform the caller we need bigger buffer
            information = sizeof(PARTITION_INFORMATION);
        }
        else
        {
            PDISK_GEOMETRY outputBuffer;

            outputBuffer = ( PDISK_GEOMETRY ) Irp->AssociatedIrp.SystemBuffer;
            RtlCopyMemory( outputBuffer, &(devExt->DiskGeometry), sizeof(DISK_GEOMETRY) );
            status = STATUS_SUCCESS;
            information = sizeof( DISK_GEOMETRY );
        }
        break;
    }

    case IOCTL_DISK_CHECK_VERIFY: {

        DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_INFO, ("IOCTL_DISK_CHECK_VERIFY \n" ) );
        //
        // Return status success
        //
        status = STATUS_SUCCESS;
        break;
    }                                        

    case IOCTL_DISK_IS_WRITABLE: {

        DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_INFO, ("IOCTL_DISK_IS_WRITABLE \n" ) );
        //
        // Return status success
        //
        status = STATUS_SUCCESS;
        break;
    }                                        

    default: {
        //
        // Received not supported IOCTLs, fail them
        DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_INFO, ("Unknown IOCTL command %X\n", command ) );
        status = STATUS_INVALID_DEVICE_REQUEST;
        break;
    }

    }   // end switch

    COMPLETE_REQUEST( Irp, status, information );
    IoReleaseRemoveLock(&devExt->RemoveLock, Irp);

    DBGPRINT( DBG_COMP_IOCTL, DBG_LEVEL_VERBOSE, ("IOCtl- OUT \n" ) );
    return status;
}   // End of RamDiskIOCtl()



NTSTATUS
RamDiskReadWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++

Routine Description:

    This routine is called by the I/O system to read or write to a
    device that we control. It can also be called by
    RamDiskDispatchDeviceControl() to do a VERIFY.

Arguments:

    DeviceObject - a pointer to the object that represents the device
    that I/O is to be done on.

    Irp - a pointer to the I/O Request Packet for this request.

Return Value:

    Status based on the request

--*/

{
    PIO_STACK_LOCATION  irpStack;
    NTSTATUS            status;
    ULONG               information = 0;
    PUCHAR              currentAddress;

    PDEVICE_EXTENSION   devExt = DeviceObject->DeviceExtension;
    DBGPRINT( DBG_COMP_READ, DBG_LEVEL_VERBOSE, ("ReadWrite - IN \n" ) );

    if ( devExt->DevState != WORKING ) {
        //
        // Device is not yet started or being removed, reject any IO request
        // TODO: Queue the IRPs

        DBGPRINT( DBG_COMP_READ, DBG_LEVEL_WARN, ("Device not ready\n" ) );
        status = STATUS_INVALID_DEVICE_STATE;
        COMPLETE_REQUEST( Irp, status, information );
    }
    status = IoAcquireRemoveLock(&devExt->RemoveLock, Irp);
    if (!NT_SUCCESS(status)) {
        DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_ERROR, ("Acquire RemoveLock failed\n" ) );
        COMPLETE_REQUEST( Irp, status, 0 );
        return status;
    }

    irpStack = IoGetCurrentIrpStackLocation(Irp);

    //
    // Check for invalid parameters.  It is an error for the starting offset
    // + length to go past the end of the buffer, or for the length to
    // not be a proper multiple of the sector size.
    //
    // Others are possible, but we don't check them since we trust the
    // file system
    //

    if (RtlLargeIntegerGreaterThan(
            RtlLargeIntegerAdd( 
                irpStack->Parameters.Read.ByteOffset,
                RtlConvertUlongToLargeInteger(irpStack->Parameters.Read.Length)),
            RtlConvertUlongToLargeInteger(devExt->DiskRegInfo.DiskSize)) ||
        (irpStack->Parameters.Read.Length & (devExt->DiskGeometry.BytesPerSector - 1))) {
        //
        // Do not give an I/O boost for parameter errors.
        //
        DBGPRINT( DBG_COMP_READ, DBG_LEVEL_ERROR, 
            (
                "Error invalid parameter\n"
                "ByteOffset: %x\n"
                "Length: %d\n"
                "Operation: %x\n",
                irpStack->Parameters.Read.ByteOffset,
                irpStack->Parameters.Read.Length,
                irpStack->MajorFunction
            ));

        status = STATUS_INVALID_PARAMETER;
        COMPLETE_REQUEST( Irp, status, information );
        IoReleaseRemoveLock(&devExt->RemoveLock, Irp);
        return status;
    }

    //
    // Get a system-space pointer to the user's buffer.  A system
    // address must be used because we may already have left the
    // original caller's address space.
    //

    ASSERT ( Irp->MdlAddress != NULL );
    currentAddress = MmGetSystemAddressForMdlSafe( Irp->MdlAddress, NormalPagePriority );

    //
    // The mapping request can fail if system is very low on resources.
    // Check for NULL and return approriate error status if the mapping failed
    //

    if ( currentAddress == NULL ) {
        status = STATUS_INSUFFICIENT_RESOURCES;
        COMPLETE_REQUEST( Irp, status, information );
        IoReleaseRemoveLock(&devExt->RemoveLock, Irp);
        DBGPRINT( DBG_COMP_READ, DBG_LEVEL_ERROR, ("Unable to get the system-space virtual address\n" ) );
        return status;
    }

    DBGPRINT( DBG_COMP_READ, DBG_LEVEL_VERBOSE,
        (
            "Irp of Request: %x\n"
            "Vmem Address of Transfer: %x - %x\n"
            "Length of Transfer: %d\n"
            "Operation: %x\n"
            "Starting ByteOffset: %x\n",
            Irp,
            currentAddress,
            ((PUCHAR)currentAddress) + irpStack->Parameters.Read.Length,
            irpStack->Parameters.Read.Length,
            irpStack->MajorFunction,
            irpStack->Parameters.Read.ByteOffset.LowPart
        ));

    information = irpStack->Parameters.Read.Length;

    switch (irpStack->MajorFunction) {

    case IRP_MJ_READ:
        RtlMoveMemory(
            currentAddress,
            devExt->DiskImage + irpStack->Parameters.Read.ByteOffset.LowPart,
            irpStack->Parameters.Read.Length);
        break;

    case IRP_MJ_WRITE:
        RtlMoveMemory(
            devExt->DiskImage + irpStack->Parameters.Read.ByteOffset.LowPart,
            currentAddress, irpStack->Parameters.Read.Length);
        break;

    default:
        information = 0;
        break;
    }

    status = STATUS_SUCCESS;
    COMPLETE_REQUEST( Irp, status, information );
    IoReleaseRemoveLock(&devExt->RemoveLock, Irp);

    DBGPRINT( DBG_COMP_READ, DBG_LEVEL_VERBOSE, ("ReadWrite - OUT \n" ) );
    return status;
}   // End of RamDiskReadWrite()

#if DBG
                       
VOID
RamDiskQueryDebugRegParameters(
    IN PUNICODE_STRING RegistryPath
    )
/*++

Routine Description:

    This routine is called from the DriverEntry to get the debug
    parameters from the registry. If the registry query fails, then
    default values are used.

Arguments:

    RegistryPath    - Points the service path to get the registry parameters

Return Value:

    None

--*/
{

    RTL_QUERY_REGISTRY_TABLE    rtlQueryRegTbl[ 4 + 1 ];  // Need 1 for NULL
    NTSTATUS                    status;
    DEBUG_INFO                  defDebugRegInfo;

    PAGED_CODE();

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("QueryDebugRegParameters\n" ) );
    ASSERT( RegistryPath->Buffer != NULL );

    // Set the default values

    defDebugRegInfo.BreakOnEntry      = DEFAULT_BREAK_ON_ENTRY;
    defDebugRegInfo.DebugLevel        = DEFAULT_DEBUG_LEVEL;
    defDebugRegInfo.DebugComp         = DEFAULT_DEBUG_COMP;

    RtlZeroMemory( rtlQueryRegTbl, sizeof(rtlQueryRegTbl) );

    //
    // Setup the query table
    //

    rtlQueryRegTbl[0].Flags         = RTL_QUERY_REGISTRY_SUBKEY;
    rtlQueryRegTbl[0].Name          = L"Parameters";
    rtlQueryRegTbl[0].EntryContext  = NULL;
    rtlQueryRegTbl[0].DefaultType   = (ULONG)NULL;
    rtlQueryRegTbl[0].DefaultData   = NULL;
    rtlQueryRegTbl[0].DefaultLength = (ULONG)NULL;

    //
    // Debug paramters
    //

    rtlQueryRegTbl[1].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[1].Name          = L"BreakOnEntry";
    rtlQueryRegTbl[1].EntryContext  = &BreakOnEntry;
    rtlQueryRegTbl[1].DefaultType   = REG_DWORD;
    rtlQueryRegTbl[1].DefaultData   = &defDebugRegInfo.BreakOnEntry;
    rtlQueryRegTbl[1].DefaultLength = sizeof(ULONG);

    rtlQueryRegTbl[2].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[2].Name          = L"DebugLevel";
    rtlQueryRegTbl[2].EntryContext  = &DbgLevel;
    rtlQueryRegTbl[2].DefaultType   = REG_DWORD;
    rtlQueryRegTbl[2].DefaultData   = &defDebugRegInfo.DebugLevel;
    rtlQueryRegTbl[2].DefaultLength = sizeof(ULONG);

    rtlQueryRegTbl[3].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[3].Name          = L"DebugComp";
    rtlQueryRegTbl[3].EntryContext  = &DbgComp;
    rtlQueryRegTbl[3].DefaultType   = REG_DWORD;
    rtlQueryRegTbl[3].DefaultData   = &defDebugRegInfo.DebugComp;
    rtlQueryRegTbl[3].DefaultLength = sizeof(ULONG);

    status = RtlQueryRegistryValues(
                                    RTL_REGISTRY_ABSOLUTE | RTL_REGISTRY_OPTIONAL,    
                                    RegistryPath->Buffer,
                                    rtlQueryRegTbl,
                                    NULL,
                                    NULL
                                    );

    if ( !NT_SUCCESS( status ) ) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_WARN, ("RrlQueryRegistryValues returned 0x%x\n", status ) );
        BreakOnEntry = defDebugRegInfo.BreakOnEntry;
        DbgLevel     = defDebugRegInfo.DebugLevel;
        DbgComp      = defDebugRegInfo.DebugComp;
    }
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("BreakOnEntry = 0x%lx\n", BreakOnEntry) );
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("DebugLevel   = 0x%lx\n", DbgLevel) );
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("DebugComp    = 0x%lx\n", DbgComp) );
    return;
}   // End of RamDiskDebugQueryRegParameters()

#endif

VOID
RamDiskQueryDiskRegParameters(
    IN PUNICODE_STRING RegistryPath,
    IN PDISK_INFO DiskRegInfo
    )
/*++

Routine Description:

    This routine is called from the DriverEntry to get the debug
    parameters from the registry. If the registry query fails, then
    default values are used.


Arguments:

    RegistryPath    - Points the service path to get the registry parameters

Return Value:

    None

--*/
{

    RTL_QUERY_REGISTRY_TABLE    rtlQueryRegTbl[ 5+ 1 ];  // Need 1 for NULL
    NTSTATUS                    status;
    DISK_INFO                   defDiskRegInfo;

    PAGED_CODE();

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("QueryDiskRegParameters \n" ) );
    ASSERT( RegistryPath->Buffer != NULL );

    // Set the default values

    defDiskRegInfo.DiskSize          = DEFAULT_DISK_SIZE;
    defDiskRegInfo.RootDirEntries    = DEFAULT_ROOT_DIR_ENTRIES;
    defDiskRegInfo.SectorsPerCluster = DEFAULT_SECTORS_PER_CLUSTER;

    RtlInitUnicodeString( &defDiskRegInfo.DriveLetter, DEFAULT_DRIVE_LETTER );

    RtlZeroMemory( rtlQueryRegTbl, sizeof(rtlQueryRegTbl) );

    //
    // Setup the query table
    //

    rtlQueryRegTbl[0].Flags         = RTL_QUERY_REGISTRY_SUBKEY;
    rtlQueryRegTbl[0].Name          = L"Parameters";
    rtlQueryRegTbl[0].EntryContext  = NULL;
    rtlQueryRegTbl[0].DefaultType   = (ULONG)NULL;
    rtlQueryRegTbl[0].DefaultData   = NULL;
    rtlQueryRegTbl[0].DefaultLength = (ULONG)NULL;

    //
    // Disk paramters
    //

    rtlQueryRegTbl[1].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[1].Name          = L"DiskSize";
    rtlQueryRegTbl[1].EntryContext  = &DiskRegInfo->DiskSize;
    rtlQueryRegTbl[1].DefaultType   = REG_DWORD;
    rtlQueryRegTbl[1].DefaultData   = &defDiskRegInfo.DiskSize;
    rtlQueryRegTbl[1].DefaultLength = sizeof(ULONG);

    rtlQueryRegTbl[2].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[2].Name          = L"RootDirEntries";
    rtlQueryRegTbl[2].EntryContext  = &DiskRegInfo->RootDirEntries;
    rtlQueryRegTbl[2].DefaultType   = REG_DWORD;
    rtlQueryRegTbl[2].DefaultData   = &defDiskRegInfo.RootDirEntries;
    rtlQueryRegTbl[2].DefaultLength = sizeof(ULONG);

    rtlQueryRegTbl[3].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[3].Name          = L"SectorsPerCluster";
    rtlQueryRegTbl[3].EntryContext  = &DiskRegInfo->SectorsPerCluster;
    rtlQueryRegTbl[3].DefaultType   = REG_DWORD;
    rtlQueryRegTbl[3].DefaultData   = &defDiskRegInfo.SectorsPerCluster;
    rtlQueryRegTbl[3].DefaultLength = sizeof(ULONG);

    rtlQueryRegTbl[4].Flags         = RTL_QUERY_REGISTRY_DIRECT;
    rtlQueryRegTbl[4].Name          = L"DriveLetter";
    rtlQueryRegTbl[4].EntryContext  = &DiskRegInfo->DriveLetter;
    rtlQueryRegTbl[4].DefaultType   = REG_SZ;
    rtlQueryRegTbl[4].DefaultData   = defDiskRegInfo.DriveLetter.Buffer;
    rtlQueryRegTbl[4].DefaultLength = 0;


    status = RtlQueryRegistryValues(
                                    RTL_REGISTRY_ABSOLUTE | RTL_REGISTRY_OPTIONAL,    
                                    RegistryPath->Buffer,
                                    rtlQueryRegTbl,
                                    NULL,
                                    NULL
                                    );

    if ( !NT_SUCCESS( status ) ) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_WARN, ("RrlQueryRegistryValues returned 0x%x\n", status ) );
        DiskRegInfo->DiskSize          = defDiskRegInfo.DiskSize;
        DiskRegInfo->RootDirEntries    = defDiskRegInfo.RootDirEntries;
        DiskRegInfo->SectorsPerCluster = defDiskRegInfo.SectorsPerCluster;
        RtlCopyUnicodeString( &DiskRegInfo->DriveLetter, &defDiskRegInfo.DriveLetter );
    }

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("DiskSize          = 0x%lx\n", DiskRegInfo->DiskSize) );
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("RootDirEntries    = 0x%lx\n", DiskRegInfo->RootDirEntries) );
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("SectorsPerCluster = 0x%lx\n", DiskRegInfo->SectorsPerCluster) );
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("DriveLetter       = %wZ\n",   &(DiskRegInfo->DriveLetter)) );
    return;
}   // End of RamDiskQueryRegParameters()


NTSTATUS
RamDiskFormatDisk(
    IN PDEVICE_OBJECT DeviceObject
    )

/*++

Routine Description:

    This routine formats the new disk.


Arguments:

    DeviceObject - Supplies a pointer to the device object that represents
                   the device whose capacity is to be read.

Return Value:

    status is returned.

--*/
{

    PDEVICE_EXTENSION   devExt = DeviceObject->DeviceExtension;
    PBOOT_SECTOR        bootSector = (PBOOT_SECTOR) devExt->DiskImage;
    PUCHAR              firstFatSector;
    ULONG               rootDirEntries;
    ULONG               sectorsPerCluster;
    USHORT              fatType;        // Type FAT 12 or 16
    USHORT              fatEntries;     // Number of cluster entries in FAT
    USHORT              fatSectorCnt;   // Number of sectors for FAT
    PDIR_ENTRY          rootDir;        // Pointer to first entry in root dir
    NTSTATUS            status = STATUS_SUCCESS;

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("FormatDisk\n" ) );

    PAGED_CODE();
    ASSERT( sizeof(BOOT_SECTOR) == 512);
    ASSERT( devExt->DiskImage != NULL );

    RtlZeroMemory( devExt->DiskImage, devExt->DiskRegInfo.DiskSize );

    devExt->DiskGeometry.BytesPerSector = 512;
    devExt->DiskGeometry.SectorsPerTrack = 32;     // Using Ramdisk value
    devExt->DiskGeometry.TracksPerCylinder = 2;    // Using Ramdisk value
    
    //
    // Calculate number of cylinders.
    //

    devExt->DiskGeometry.Cylinders.QuadPart = devExt->DiskRegInfo.DiskSize / 512 / 32 / 2;

    //
    // Our media type is RAMDISK_MEDIA_TYPE
    //

    devExt->DiskGeometry.MediaType = RAMDISK_MEDIA_TYPE; 

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, 
        ("Cylinders: %ld\n TracksPerCylinder: %ld\n SectorsPerTrack: %ld\n BytesPerSector: %ld\n", 
        devExt->DiskGeometry.Cylinders.QuadPart, devExt->DiskGeometry.TracksPerCylinder,
        devExt->DiskGeometry.SectorsPerTrack, devExt->DiskGeometry.BytesPerSector ) );

    rootDirEntries = devExt->DiskRegInfo.RootDirEntries;
    sectorsPerCluster = devExt->DiskRegInfo.SectorsPerCluster;

    //
    // Round Root Directory entries up if necessary
    //
    if (rootDirEntries & (DIR_ENTRIES_PER_SECTOR - 1)) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_WARN, ("Adjusting RootDirEntries \n" ) );

        rootDirEntries =
            (rootDirEntries + ( DIR_ENTRIES_PER_SECTOR - 1 )) &
                ~ ( DIR_ENTRIES_PER_SECTOR - 1 );
    }

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, 
        ("Root dir entries: %ld\n Sectors/cluster: %ld\n", 
        rootDirEntries, sectorsPerCluster ) );
    //
    // We need to have the 0xeb and 0x90 since this is one of the
    // checks the file system recognizer uses
    //
    bootSector->bsJump[0] = 0xeb;
    bootSector->bsJump[1] = 0x3c;
    bootSector->bsJump[2] = 0x90;

    strncpy(bootSector->bsOemName, "RajuRam ", 8);
    bootSector->bsBytesPerSec = (SHORT)devExt->DiskGeometry.BytesPerSector;
    bootSector->bsResSectors  = 1;
    bootSector->bsFATs        = 1;
    bootSector->bsRootDirEnts = (USHORT)rootDirEntries;

    bootSector->bsSectors     = (USHORT)( devExt->DiskRegInfo.DiskSize / devExt->DiskGeometry.BytesPerSector );
    bootSector->bsMedia       = (UCHAR) devExt->DiskGeometry.MediaType;
    bootSector->bsSecPerClus  = (UCHAR)sectorsPerCluster;

    //
    // Calculate number of sectors required for FAT
    //
    fatEntries =
        (bootSector->bsSectors - bootSector->bsResSectors -
            bootSector->bsRootDirEnts / DIR_ENTRIES_PER_SECTOR) /
                bootSector->bsSecPerClus + 2;

    //
    // Choose between 12 and 16 bit FAT based on number of clusters we
    // need to map
    //
    if (fatEntries > 4087) {
        fatType =  16;
        fatSectorCnt = (fatEntries * 2 + 511) / 512;
        fatEntries -= fatSectorCnt;
        fatSectorCnt = (fatEntries * 2 + 511) / 512;
    }
    else {
        fatType =  12;
        fatSectorCnt = (((fatEntries * 3 + 1) / 2) + 511) / 512;
        fatEntries -= fatSectorCnt;
        fatSectorCnt = (((fatEntries * 3 + 1) / 2) + 511) / 512;
    }

    bootSector->bsFATsecs       = fatSectorCnt;
    bootSector->bsSecPerTrack   = (USHORT)devExt->DiskGeometry.SectorsPerTrack;
    bootSector->bsHeads         = (USHORT)devExt->DiskGeometry.TracksPerCylinder;
    bootSector->bsBootSignature = 0x29;
    bootSector->bsVolumeID      = 0x12345678;
    strncpy(bootSector->bsLabel, "RamDisk    ", 11);
    strncpy(bootSector->bsFileSystemType, "FAT1?   ", 8);
    bootSector->bsFileSystemType[4] = ( fatType == 16 ) ? '6' : '2';
    
    bootSector->bsSig2[0] = 0x55;
    bootSector->bsSig2[1] = 0xAA;
    
    //
    // The FAT is located immediately following the boot sector.
    //
    firstFatSector    = (PUCHAR)(bootSector + 1);
    firstFatSector[0] = (UCHAR) devExt->DiskGeometry.MediaType;
    firstFatSector[1] = 0xFF;
    firstFatSector[2] = 0xFF;

    if (fatType == 16) {
        firstFatSector[3] = 0xFF;
    }

    //
    // The Root Directory follows the FAT
    //
    rootDir = (PDIR_ENTRY)(bootSector + 1 + fatSectorCnt);
    strcpy(rootDir->deName, "MS-RAMDR");
    strcpy(rootDir->deExtension, "IVE");
    rootDir->deAttributes = DIR_ATTR_VOLUME;

    return status;
} // end RamDiskFormatDisk()



