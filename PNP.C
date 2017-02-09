/*++

Copyright (c) 1990-2000 Microsoft Corporation, All Rights Reserved

Module Name:
    pnp.c

Abstract: 
    This is the code for handling PnP, Power and WMI IRPs

Author:
    Raju Ramanathan        02/22/2000

Enviroment:
    Kernel Mode Only

Revision History:

      Code cleaning   04/19/2000

--*/

#include <stdio.h>
#include "ntddk.h"
#include "debug.h"
#include "ramdisk.h"

PCSTR StateTable[] ={
    { "STOPPED"         },
    { "WORKING"         },
    { "PENDINGSTOP"     },
    { "PENDINGREMOVE"   },
    { "SURPRISEREMOVED" },
    { "REMOVED"         },
    { "UNKNOWN"         }
};

NTSTATUS
RamDiskAddDevice( 
    IN PDRIVER_OBJECT DriverObject, 
    IN PDEVICE_OBJECT PhysicalDeviceObject
    )
/*++ 
Routine Description:

    AddDevice routine to create the device object and symbolic link
    
Arguments:

    DriverObject            - Supplies the driver object

    PhysicalDeviceObject    - Supplies the physical device object
    
Return Value:

    NTSTATUS
    
--*/    
{

    PRAMDISK_DRIVER_EXTENSION   driverExtension;
    PDEVICE_OBJECT              functionDeviceObject;
    PDEVICE_EXTENSION           devExt;
    UNICODE_STRING              uniDeviceName;
    UNICODE_STRING              uniWin32Name;
    NTSTATUS                    status = STATUS_SUCCESS;
    
    PAGED_CODE();

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("AddDevice - IN. DriverObject=(%p) Pdo=(%p) \n",
        DriverObject, PhysicalDeviceObject ) );

    // Get the Driver object extension 

    driverExtension = IoGetDriverObjectExtension(DriverObject,
                                                 RAMDISK_DRIVER_EXTENSION_KEY);

    ASSERT ( driverExtension != NULL );

	//
	// We are capable of handling only one device. If the we get AddDevice request 
	// for the next device, reject it
	//

	if ( driverExtension->DeviceInitialized == TRUE ) {

        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("Device exists\n") );
		return STATUS_DEVICE_ALREADY_ATTACHED;
	}

    //
    // Create counted string version of our device name.
    //

    RtlInitUnicodeString( &uniDeviceName, NT_DEVICE_NAME );

    //
    // Create the device object
    //

    status = IoCreateDevice(
                DriverObject,
                sizeof(DEVICE_EXTENSION),
                &uniDeviceName,
                FILE_DEVICE_VIRTUAL_DISK,
                (FILE_DEVICE_SECURE_OPEN),
                FALSE,                 // This isn't an exclusive device
                &functionDeviceObject
                );

    if (!NT_SUCCESS(status)) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("IoCreateDevice error: 0x%x\n", status) );
        return status;        
    }
    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_INFO, ("FDO created successfully (%p)\n",
        functionDeviceObject) );
    devExt = functionDeviceObject->DeviceExtension;

    RtlZeroMemory( devExt, sizeof(DEVICE_EXTENSION) );

    // Allocate buffer for storing the drive letter

    devExt->DiskRegInfo.DriveLetter.Buffer =  ExAllocatePoolWithTag( 
                            PagedPool, 
                            DRIVE_LETTER_LENGTH,
                            RAMDISK_TAG_GENERAL);
    if ( devExt->DiskRegInfo.DriveLetter.Buffer == NULL ) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("Can't allocate memory for drive letter\n") );
        RamDiskCleanUp( functionDeviceObject );
        return STATUS_INSUFFICIENT_RESOURCES;
    }
    devExt->DiskRegInfo.DriveLetter.MaximumLength = DRIVE_LETTER_LENGTH;

    // 
    // Get the disk parameters from the registry
    //

    RamDiskQueryDiskRegParameters( &driverExtension->RegistryPath, &devExt->DiskRegInfo  );

    devExt->PhysicalDeviceObject = PhysicalDeviceObject;  // Save PDO pointer
    devExt->DeviceObject = functionDeviceObject;          // Save device object pointer
    devExt->DevState = STOPPED;                           // Device starts in Stopped state
    IoInitializeRemoveLock ( &devExt->RemoveLock, 
                            REMLOCK_TAG, 
                            REMLOCK_MAXIMUM, 
                            REMLOCK_HIGHWATER);

    // Set device flags

    functionDeviceObject->Flags |= DO_POWER_PAGABLE;
    functionDeviceObject->Flags |= DO_DIRECT_IO;

    //
    // Allocate the memory for disk image.
    //
    
    devExt->DiskImage = ExAllocatePoolWithTag( 
                            NonPagedPool, 
                            devExt->DiskRegInfo.DiskSize,
                            RAMDISK_TAG_DISK);
    
    if ( devExt->DiskImage == NULL )
    {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("Can't allocate memory for disk image\n") );
        RamDiskCleanUp( functionDeviceObject );
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    // Format the disk

    RamDiskFormatDisk( functionDeviceObject );

    // Create symbolic link, which is the drive letter for the ramdisk

    devExt->SymbolicLink.Buffer = ExAllocatePoolWithTag( 
                            PagedPool, 
                            DOS_DEVNAME_LENGTH,
                            RAMDISK_TAG_GENERAL);

    if ( devExt->SymbolicLink.Buffer == NULL ) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("Can't allocate memory for symbolic link\n") );
        RamDiskCleanUp( functionDeviceObject );
        return STATUS_INSUFFICIENT_RESOURCES;
    }
    RtlInitUnicodeString( &uniWin32Name, DOS_DEVICE_NAME );

    devExt->SymbolicLink.MaximumLength = DOS_DEVNAME_LENGTH;
    devExt->SymbolicLink.Length = uniWin32Name.Length;

    RtlCopyUnicodeString( &(devExt->SymbolicLink), &uniWin32Name );
    RtlAppendUnicodeStringToString( &(devExt->SymbolicLink), &(devExt->DiskRegInfo.DriveLetter) );

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_NOTIFY, ("Creating drive letter = %wZ\n",&(devExt->SymbolicLink) ) );

    // Create a drive letter from our device name to a name in the Win32 namespace.
    
    status = IoCreateSymbolicLink( &devExt->SymbolicLink, &uniDeviceName );

    if (!NT_SUCCESS(status)) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("IoCreateSymbolicLink error: 0x%x\n", status) );
        RamDiskCleanUp( functionDeviceObject );
        return status;
    }
    devExt->Flags |= FLAG_LINK_CREATED;

    devExt->LowerDeviceObject = 
        IoAttachDeviceToDeviceStack( functionDeviceObject, PhysicalDeviceObject );
    if ( devExt->LowerDeviceObject == NULL ) {
        DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_ERROR, ("IoAttachDeviceToDeviceStack error\n") );
        RamDiskCleanUp( functionDeviceObject );
        return STATUS_NO_SUCH_DEVICE;      
    }

    driverExtension->DeviceInitialized = TRUE;

    // Clear DO_DEVICE_INITIALIZING flag

    functionDeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

    DBGPRINT( DBG_COMP_INIT, DBG_LEVEL_VERBOSE, ("AddDevice - OUT. Fdo=(%p) LowerDevice=(%p)\n",
        functionDeviceObject, devExt->LowerDeviceObject ) );

    return status;
}  // End of RamDiskAddDevice()

NTSTATUS
RamDiskDispatchPnp(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++ 
Routine Description:

    Dispatch routine for Plug and Play IRPs
    
Arguments:

    DeviceObject    - Supplies the device object.
    
    Irp             - Supplies the I/O request packet. 

Return Value:

    NTSTATUS
    
--*/    
{
    PIO_STACK_LOCATION  irpStack;
    PDEVICE_EXTENSION   devExt;
    KEVENT              event;
    NTSTATUS            status = STATUS_SUCCESS;
    BOOLEAN             lockHeld = TRUE;


    PAGED_CODE();

    irpStack = IoGetCurrentIrpStackLocation( Irp );
    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

    ASSERT( devExt->DevState < MAX_STATE );

    DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_INFO, ("DispatchPnP - IN. Fdo=(%p) Irp=(%p) %s Device State=%s\n",
        DeviceObject, Irp, GetPnpIrpName(irpStack->MinorFunction), StateTable[devExt->DevState] ) );

    status = IoAcquireRemoveLock(&devExt->RemoveLock, Irp);
    if (!NT_SUCCESS(status)) {
        DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_ERROR, ("Acquire RemoveLock failed\n" ) );
        COMPLETE_REQUEST( Irp, status, 0 );
        return status;
    }

    switch (irpStack->MinorFunction) {
    
    case IRP_MN_START_DEVICE: {
        KeInitializeEvent( &event, NotificationEvent, FALSE);
        IoCopyCurrentIrpStackLocationToNext( Irp );
        IoSetCompletionRoutine( Irp, (PIO_COMPLETION_ROUTINE) RamDiskIoCompletionRoutine, 
                                (PVOID) &event, TRUE, TRUE, TRUE );
        status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        if (status == STATUS_PENDING) {
            KeWaitForSingleObject( &event, Executive, KernelMode, FALSE, NULL );
        }
        if ( NT_SUCCESS(status) ) {
        //
        // Device started successfully by the lower driver
        //
            devExt->DevState = WORKING;
        }
        COMPLETE_REQUEST( Irp, status, 0 );
        break;
    }

    case IRP_MN_QUERY_STOP_DEVICE: {
        devExt->DevState = PENDINGSTOP;
        Irp->IoStatus.Status = STATUS_SUCCESS;
        IoSkipCurrentIrpStackLocation( Irp );
        status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        break;
    }

    case IRP_MN_CANCEL_STOP_DEVICE: {
        //
        // Before sending the IRP down make sure we have received 
        // a IRP_MN_QUERY_STOP_DEVICE. We may get Cancel Stop 
        // without receiving a Query Stop earlier, if the 
        // driver on top fails a Query Stop and passes down the
        // Cancel Stop.
        //

        if ( devExt->DevState == PENDINGSTOP ) {
            devExt->DevState = WORKING;
            Irp->IoStatus.Status = STATUS_SUCCESS;
            IoSkipCurrentIrpStackLocation( Irp );
            status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        } else {
            //
            // A spurious Cancel Stop request. Just complete it
            //
            status = STATUS_SUCCESS;
            COMPLETE_REQUEST( Irp, status, 0 );
        }
        break;
    }

    case IRP_MN_STOP_DEVICE: {
        devExt->DevState = STOPPED;
        Irp->IoStatus.Status = STATUS_SUCCESS;
        IoSkipCurrentIrpStackLocation( Irp );
        status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        break;
    }


    case IRP_MN_QUERY_REMOVE_DEVICE: {
        devExt->DevState = PENDINGREMOVE;
        Irp->IoStatus.Status = STATUS_SUCCESS;
        IoSkipCurrentIrpStackLocation( Irp );
        status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        break;
    }

    case IRP_MN_CANCEL_REMOVE_DEVICE: {
        //
        // Before sending the IRP down make sure we have received 
        // a IRP_MN_QUERY_REMOVE_DEVICE. We may get Cancel Remove 
        // without receiving a Query Remove earlier, if the 
        // driver on top fails a Query Remove and passes down the
        // Cancel Remove.
        //

        if ( devExt->DevState == PENDINGREMOVE ) {
            devExt->DevState = WORKING;
            Irp->IoStatus.Status = STATUS_SUCCESS;
            IoSkipCurrentIrpStackLocation( Irp );
            status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        } else {
            //
            // A spurious Cancel Remove request. Just complete it
            //
            status = STATUS_SUCCESS;
            COMPLETE_REQUEST( Irp, status, 0 );
        }
        break;
    }

    case IRP_MN_SURPRISE_REMOVAL: {
        devExt->DevState = SURPRISEREMOVED;
        Irp->IoStatus.Status = STATUS_SUCCESS;
        IoSkipCurrentIrpStackLocation( Irp );
        status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        break;
    }

    case IRP_MN_REMOVE_DEVICE: {
        RamDiskRemoveDevice( DeviceObject, Irp );
        //
        // Remove Lock released by RamDiskRemoveDevice
        //
        lockHeld = FALSE;

        break;
    }

    default: {
        IoSkipCurrentIrpStackLocation( Irp );
        status = IoCallDriver( devExt->LowerDeviceObject, Irp );
        break;
    } // default

    } // switch

    //
    // Device Extenion is gone if the current IRP is IRP_MN_REMOVE_DEVICE
    //
    if ( lockHeld == TRUE ) {
        IoReleaseRemoveLock(&devExt->RemoveLock, Irp);
        DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_INFO, ("DispatchPnP - OUT. Device State=%s\n",
              StateTable[devExt->DevState] ) );
    }

    return status;
}  // End of RamDiskDispatchPnp()

NTSTATUS
RamDiskDispatchPower(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++ 
Routine Description:

    Dispatch routine for Power management IRPs
    
Arguments:

    DeviceObject    - Supplies the device object.
    
    Irp             - Supplies the I/O request packet. 

Return Value:

    NTSTATUS
    
--*/    
{
    PDEVICE_EXTENSION   devExt;
    NTSTATUS            status = STATUS_SUCCESS;

    PAGED_CODE();

    DBGPRINT( DBG_COMP_POWER, DBG_LEVEL_VERBOSE, ("DispatchPower - IN. Fdo=(%p) Irp=(%p)\n",
                                                        DeviceObject, Irp ) );
    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

    //
    // If the device has been removed, the driver should not pass
    // the IRP down to the next lower driver.
    //

    if ( devExt->DevState == REMOVED) {
        PoStartNextPowerIrp( Irp );
        status = STATUS_DELETE_PENDING;
        COMPLETE_REQUEST( Irp, status, 0 );
        return status;
    }

    PoStartNextPowerIrp( Irp );
    IoSkipCurrentIrpStackLocation( Irp );

    DBGPRINT( DBG_COMP_POWER, DBG_LEVEL_VERBOSE, ("DispatchPower - OUT.\n" ) );

    return PoCallDriver( devExt->LowerDeviceObject, Irp );

}  // End of RamDiskDispatchPower()

NTSTATUS
RamDiskDispatchSystemControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
/*++ 
Routine Description:

    Dispatch routine for WMI IRPs. It just forwards the 
    IRPs to the lower driver.
    
Arguments:

    DeviceObject    - Supplies the device object.
    
    Irp             - Supplies the I/O request packet. 

Return Value:

    NTSTATUS
    
--*/    
{
    PDEVICE_EXTENSION   devExt;

    PAGED_CODE();

    DBGPRINT( DBG_COMP_WMI, DBG_LEVEL_VERBOSE, ("DispatchSystemControl - IN. Fdo=(%p) Irp=(%p)\n",
        DeviceObject, Irp ) );

    devExt = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    IoSkipCurrentIrpStackLocation( Irp );
    DBGPRINT( DBG_COMP_WMI, DBG_LEVEL_VERBOSE, ("DispatchSystemControl - OUT.\n" ) );
    return IoCallDriver( devExt->LowerDeviceObject, Irp );

}  // End of RamDiskDispatchSystemControl()


NTSTATUS
RamDiskIoCompletionRoutine(
    IN PDEVICE_OBJECT  DeviceObject,
    IN PIRP            Irp,
    IN PKEVENT         Event
    )
/*++ 
Routine Description:

    Io completion routine
    
Arguments:

    DeviceObject    - Supplies the device object.
    
    Irp             - Supplies the I/O request packet. 

    Event           - Supplies the Event to be set.

Return Value:

    NTSTATUS
    
--*/    
{

    KeSetEvent( Event, 0, FALSE );
    return STATUS_MORE_PROCESSING_REQUIRED;

}  // End of RamDiskIoCompletionRoutine()


VOID
RamDiskCleanUp( 
    IN PDEVICE_OBJECT DeviceObject
    )
/*++

Routine Description:

    This routine does the required cleaning like deleting the symbolic link
    releasjing the memory etc.

Arguments:

    DeviceObject - Supplies a pointer to the device object that represents
        the device whose capacity is to be read.

Return Value:

    None.

--*/
{
      
    PDEVICE_EXTENSION   devExt = DeviceObject->DeviceExtension;

    PAGED_CODE();
    DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_VERBOSE, ("RamDiskCleanUp\n" ) );
    
    if ( devExt->Flags & FLAG_LINK_CREATED ) {
        IoDeleteSymbolicLink( &devExt->SymbolicLink );
    }
    if ( devExt->SymbolicLink.Buffer ) {
        ExFreePool( devExt->SymbolicLink.Buffer );
    }
    if ( devExt->DiskRegInfo.DriveLetter.Buffer ) {
        ExFreePool( devExt->DiskRegInfo.DriveLetter.Buffer );
    }
    if ( devExt->DiskImage ) {
        ExFreePool( devExt->DiskImage );
    }
    if ( devExt->LowerDeviceObject ) {
        IoDetachDevice( devExt->LowerDeviceObject );
    }
    IoDeleteDevice( DeviceObject );

    return;
}

VOID
RamDiskRemoveDevice(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )

/*++

Routine Description:

    This routine releases the remove lock and calls RamDiskCleanUp
    to do the cleaning

Arguments:

    DeviceObject - Supplies a pointer to the device object that represents
        the device whose capacity is to be read.

Return Value:

    None.

--*/
{
    PDEVICE_EXTENSION           devExt = DeviceObject->DeviceExtension;
    PRAMDISK_DRIVER_EXTENSION   driverExtension;
    NTSTATUS                    status;

    PAGED_CODE();
    DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_VERBOSE, ("RemoveDevice\n" ) );

    Irp->IoStatus.Status = STATUS_SUCCESS;
    IoSkipCurrentIrpStackLocation( Irp );
    status = IoCallDriver( devExt->LowerDeviceObject, Irp );

    //
    // Set the device status to REMOVED and wait for other drivers 
    // to release the lock, then delete the device object
    //
    devExt->DevState = REMOVED;
    IoReleaseRemoveLockAndWait(&devExt->RemoveLock, Irp);

    driverExtension = IoGetDriverObjectExtension(DeviceObject->DriverObject,
                                             RAMDISK_DRIVER_EXTENSION_KEY);
    ASSERT ( driverExtension != NULL );
	driverExtension->DeviceInitialized = FALSE;

    RamDiskCleanUp( DeviceObject );

    DBGPRINT( DBG_COMP_PNP, DBG_LEVEL_NOTIFY, ("Device Removed succesfully\n" ) );

    return;
} // end RamDiskRemoveDevice()


PSTR
GetPnpIrpName(
    IN UCHAR    PnpMinorFunction
    )
/*++ 
Routine Description:

    This function returns the minor function string for
    the given id.
    
Arguments:

    PnpMinorFunction    - Supplies the Minor function Irp id.
    
Return Value:

    PSTR    - Function name string
    
--*/    
{
    static char    functionName[80];

    PAGED_CODE();

    switch ( PnpMinorFunction ) {

        case IRP_MN_START_DEVICE:                 // 0x00
            return "IRP_MN_START_DEVICE";
            break;

        case IRP_MN_QUERY_REMOVE_DEVICE:          // 0x01
            return "IRP_MN_QUERY_REMOVE_DEVICE";
            break;

        case IRP_MN_REMOVE_DEVICE:                // 0x02
            return "IRP_MN_REMOVE_DEVICE";
            break;

        case IRP_MN_CANCEL_REMOVE_DEVICE:         // 0x03
            return "IRP_MN_CANCEL_REMOVE_DEVICE";
            break;

        case IRP_MN_STOP_DEVICE:                  // 0x04
            return "IRP_MN_STOP_DEVICE";
            break;

        case IRP_MN_QUERY_STOP_DEVICE:            // 0x05
            return "IRP_MN_QUERY_STOP_DEVICE";
            break;

        case IRP_MN_CANCEL_STOP_DEVICE:           // 0x06
            return "IRP_MN_CANCEL_STOP_DEVICE";
            break;

        case IRP_MN_QUERY_DEVICE_RELATIONS:       // 0x07
            return "IRP_MN_QUERY_DEVICE_RELATIONS";
            break;

        case IRP_MN_QUERY_INTERFACE:              // 0x08
            return "IRP_MN_QUERY_INTERFACE";
            break;

        case IRP_MN_QUERY_CAPABILITIES:           // 0x09
            return "IRP_MN_QUERY_CAPABILITIES";
            break;

        case IRP_MN_QUERY_RESOURCES:              // 0x0A
            return "IRP_MN_QUERY_RESOURCES";
            break;

        case IRP_MN_QUERY_RESOURCE_REQUIREMENTS:  // 0x0B
            return "IRP_MN_QUERY_RESOURCE_REQUIREMENTS";
            break;

        case IRP_MN_QUERY_DEVICE_TEXT:            // 0x0C
            return "IRP_MN_QUERY_DEVICE_TEXT";
            break;

        case IRP_MN_FILTER_RESOURCE_REQUIREMENTS: // 0x0D
            return "IRP_MN_FILTER_RESOURCE_REQUIREMENTS";
            break;

        case IRP_MN_READ_CONFIG:                  // 0x0F
            return "IRP_MN_READ_CONFIG";
            break;

        case IRP_MN_WRITE_CONFIG:                 // 0x10
            return "IRP_MN_WRITE_CONFIG";
            break;

        case IRP_MN_EJECT:                        // 0x11
            return "IRP_MN_EJECT";
            break;

        case IRP_MN_SET_LOCK:                     // 0x12
            return "IRP_MN_SET_LOCK";
            break;

        case IRP_MN_QUERY_ID:                     // 0x13
            return "IRP_MN_QUERY_ID";
            break;

        case IRP_MN_QUERY_PNP_DEVICE_STATE:       // 0x14
            return "IRP_MN_QUERY_PNP_DEVICE_STATE";
            break;

        case IRP_MN_QUERY_BUS_INFORMATION:        // 0x15
            return "IRP_MN_QUERY_BUS_INFORMATION";
            break;

        case IRP_MN_DEVICE_USAGE_NOTIFICATION:    // 0x16
            return "IRP_MN_DEVICE_USAGE_NOTIFICATION";
            break;

        case IRP_MN_SURPRISE_REMOVAL:             // 0x17
            return "IRP_MN_SURPRISE_REMOVAL";
            break;

        case IRP_MN_QUERY_LEGACY_BUS_INFORMATION: // 0x18
            return "IRP_MN_QUERY_LEGACY_BUS_INFORMATION";
            break;

        default:
            sprintf( functionName, "Unknown IRP(0x%x)", PnpMinorFunction );
            return functionName;
            break;

    }    // switch
}  // End of GetPnpIrpName()





