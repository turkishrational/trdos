; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.0) - DEFINITIONS : trdosk0.s
; ----------------------------------------------------------------------------
; Last Update: 29/02/2016
; ----------------------------------------------------------------------------
; Beginning: 04/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ****************************************************************************
; TRDOS2.ASM (c) 2004-2011 Erdogan TAN [ 17/01/2004 ] Last Update: 09/11/2011
;
; Masterboot / Partition Table at Beginning+1BEh
ptBootable       equ 0
ptBeginHead      equ 1
ptBeginSector    equ 2
ptBeginCylinder  equ 3
ptFileSystemID   equ 4
ptEndHead        equ 5
ptEndSector      equ 6
ptEndCylinder    equ 7
ptStartSector    equ 8
ptSectors        equ 12

; Boot Sector Parameters at 7C00h
DataArea1     equ -4
DataArea2     equ -2
BootStart     equ 0h
OemName       equ 03h
BytesPerSec   equ 0Bh
SecPerClust   equ 0Dh
ResSectors    equ 0Eh
FATs          equ 10h
RootDirEnts   equ 11h
Sectors       equ 13h
Media         equ 15h
FATSecs       equ 16h
SecPerTrack   equ 18h
Heads         equ 1Ah 
Hidden1       equ 1Ch
Hidden2       equ 1Eh
HugeSec1      equ 20h
HugeSec2      equ 22h
DriveNumber   equ 24h
Reserved1     equ 25h
bootsignature equ 26h                 
VolumeID      equ 27h
VolumeLabel   equ 2Bh
FileSysType   equ 36h          
Reserved2     equ 3Eh                           ; Starting cluster of P2000

; FAT32 BPB Structure
FAT32_FAT_Size equ 36
FAT32_RootFClust equ 44
FAT32_FSInfoSec equ 48
FAT32_DrvNum equ 64
FAT32_BootSig equ 66
FAT32_VolID equ 67
FAT32_VolLab equ 71
FAT32_FilSysType equ 82

; BIOS Disk Parameters
DPDiskNumber  equ 0h
DPDType       equ 1h
DPReturn      equ 2h
DPHeads       equ 3h
DPCylinders   equ 4h
DPSecPerTrack equ 6h
DPDisks       equ 7h
DPTableOff    equ 8h
DPTableSeg    equ 0Ah
DPNumOfSecs   equ 0Ch

; BIOS INT 13h Extensions (LBA extensions)
; Just After DP Data (DPDiskNumber+)
DAP_PacketSize equ 10h  ; If extensions present, this byte will be >=10h
DAP_Reserved1 equ 11h   ; Reserved Byte 
DAP_NumOfBlocks equ 12h ; Value of this byte must be 0 to 127
DAP_Reserved2 equ 13h   ; Reserved Byte
DAP_Destination equ 14h ; Address of Transfer Buffer as SEGMENT:OFFSET
DAP_LBA_Address equ 18h ; LBA=(C1*H0+H1)*S0+S1-1
                        ; C1= Selected Cylinder Number
                        ; H0= Number Of Heads (Maximum Head Number + 1)
                        ; H1= Selected Head Number
                        ; S0= Maximum Sector Number
                        ; S1= Selected Sector Number
                        ; QUAD WORD
; DAP_Flat_Destination equ 20h ; 64 bit address, if value in 4h is FFFF:FFFFh
                             ; QUAD WORD (Also, value in 0h must be 18h) 
                             ; TR-DOS will not use 64 bit Flat Address

; INT 13h Function 48h "Get Enhanced Disk Drive Parameters"
; Just After DP Data (DPDiskNumber+)
GetDParams_48h equ 20h ; Word. Data Length, must be 26 (1Ah) for short data.
GDP_48h_InfoFlag equ 22h ; Word
; Bit 1 = 1 -> The geometry returned in bytes 4-15 is valid.
GDP_48h_NumOfPCyls equ 24h ; Double Word. Number physical cylinders.
GDP_48h_NumOfPHeads equ 28h ; Double Word. Number of physical heads.
GDP_48h_NumOfPSpT equ 2Ch ; Double word. Num of physical sectors per track.
GDP_48h_LBA_Sectors equ 30h ; 8 bytes. Number of physical/LBA sectors.
GDP_48h_BytesPerSec equ 38h ; Word. Number of bytes in a sector.

; TR-DOS Standalone Program Extensions to the DiskParams Block
; Just After DP Data (DPDiskNumber+)
TRDP_CurrentSector equ 3Ah  ; DX:AX (LBA)
TRDP_SectorCount equ 3Eh    ; CX (or Counter)


; DOS Logical Disks
LD_Name equ 0
LD_DiskType equ 1
LD_PhyDrvNo equ 2
LD_FATType equ 3
LD_FSType equ 4
LD_LBAYes equ 5
LD_BPB equ 6
LD_FATBegin equ 96
LD_ROOTBegin equ 100
LD_DATABegin equ 104
LD_StartSector equ 108
LD_TotalSectors equ 112
LD_FreeSectors equ 116
LD_Clusters equ 120
LD_PartitionEntry equ 124
LD_DParamEntry equ 125
LD_MediaChanged equ 126
LD_CDirLevel equ 127
LD_CurrentDirectory equ 128

; Singlix FS Extensions to DOS Logical Disks
; 03/01/2010 (LD_BPB compatibility for CHS r/w)

LD_FS_Name equ 0
LD_FS_DiskType equ 1
LD_FS_PhyDrvNo equ 2
LD_FS_FATType equ 3
LD_FS_FSType equ 4
LD_FS_LBAYes equ 5
LD_FS_BPB equ 6
LD_FS_MediaAttrib equ 6
LD_FS_VersionMajor equ 7
LD_FS_RootDirD equ 8
LD_FS_MATLocation equ 12
LD_FS_Reserved1 equ 16 ;1 reserved byte
LD_FS_BytesPerSec equ 17 ; LD_BPB + 0Bh
LD_FS_Reserved2 equ 19 ;2 reserved byte
LD_FS_DATLocation equ 20
LD_FS_DATSectors equ 24
LD_FS_Reserved3 equ 28 ;3 reserved word
LD_FS_SecPerTrack equ 30 ; LD_BPB + 18h
LD_FS_NumHeads equ 32    ; LD_BPB + 1Ah
LD_FS_UnDelDirD equ 34
LD_FS_Reserved4 equ 38 ;4 reserved word
LD_FS_VolumeSerial equ 40
LD_FS_VolumeName equ 44
LD_FS_BeginSector equ 108
LD_FS_VolumeSize equ 112
LD_FS_FreeSectors equ 116
LD_FS_FirstFreeSector equ 120
LD_FS_PartitionEntry equ 124
LD_FS_DParamEntry equ 125
LD_FS_MediaChanged equ 126
LD_FS_CDirLevel equ 127
LD_FS_CDIR_Converted equ 128

; Valid FAT Types
FS_FAT12 equ 1
FS_FAT16_CHS equ 2
FS_FAT32_CHS equ 3
FS_FAT16_LBA equ 4
FS_FAT32_LBA equ 5

; Cursor Location
CCCpointer equ  0450h   ; BIOS data, current cursor column
; FAT Clusters EOC sign
FAT12EOC equ 0FFFh
FAT16EOC equ 0FFFFh
;FAT32EOC equ 0FFFFFFFh ; It is not direct usable for 8086 code
; BAD Cluster
FAT12BADC equ 0FF7h
FAT16BADC equ 0FFF7h
;FAT32BADC equ 0FFFFFF7h ; It is not direct usable for 8086 code
; MS-DOS FAT16 FS (Maximum Possible) Last Cluster Number= 0FFF6h 

; TRFS

bs_FS_JmpBoot equ 0 ; jmp short bsBootCode
                ; db 0EBh, db 3Fh, db 90h
bs_FS_Identifier equ 3  ; db 'FS', db 0
bs_FS_BytesPerSec equ 6 ; dw 512
bs_FS_MediaAttrib equ 8 ; db 3
bs_FS_PartitionID equ 9 ; db 0A1h
bs_FS_VersionMaj equ 10 ; db 01h
bs_FS_VersionMin equ 11 ; db 0
bs_FS_BeginSector equ 12   ; dd 0 
bs_FS_VolumeSize equ 16 ; dd 2880
bs_FS_StartupFD equ 20 ; dd 0
bs_FS_MATLocation equ 24 ; dd 1
bs_FS_RootDirD equ 28 ; dd 8
bs_FS_SystemConfFD equ 32 ; dd 0
bs_FS_SwapFD equ 36 ; dd 0
bs_FS_UnDelDirD equ 40 ; dd 0
bs_FS_DriveNumber equ 44 ; db 0
bs_FS_LBA_Ready equ 45 ; db 0
bs_FS_MagicWord equ 46 
bs_FS_SecPerTrack equ 46 ; db 0A1h
bs_FS_Heads equ 47 ; db 01h 
bs_FS_OperationSys equ 48 ; db "TR-SINGLIX v1.0b"
bs_FS_Terminator equ 64 ; db 0
bs_FS_BootCode equ 65 

FS_MAT_DATLocation equ 12
FS_MAT_DATScount equ 16
FS_MAT_FreeSectors equ 20
FS_MAT_FirstFreeSector equ 24
FS_RDT_VolumeSerialNo equ 28
FS_RDT_VolumeName equ 64

; FAT12 + FAT16 + FAT32
BS_JmpBoot equ 0
BS_OEMName equ 3
BPB_BytsPerSec equ 11
BPB_SecPerClust equ 13
BPB_RsvdSecCnt equ 14
BPB_NumFATs equ 16
BPB_RootEntCnt equ 17
BPB_TotalSec16 equ 19
BPB_Media equ 21
BPB_FATSz16 equ 22
BPB_SecPerTrk equ 24
BPB_NumHeads equ 26
BPB_HiddSec equ 28
BPB_TotalSec32 equ 32

; FAT12 and FAT16 only
BS_DrvNum equ 36
BS_Reserved1 equ 37
BS_BootSig equ 38
BS_VolID equ 39
BS_VolLab equ 43
BS_FilSysType equ 54 ; 8 bytes
BS_BootCode equ 62

; FAT32 only
BPB_FATSz32 equ 36 ; FAT32, 4 bytes
BPB_ExtFlags equ 40 ; FAT32, 2 bytes
BPB_FSVer equ 42 ; FAT32, 2 bytes
BPB_RootClus equ 44 ; FAT32, 4 bytes
BPB_FSInfo equ 48 ; FAT 32, 2 bytes 
BPB_BkBootSec equ 50 ; FAT32, 2 bytes
BPB_Reserved equ 52 ; FAT32, 12 bytes
BS_FAT32_DrvNum equ 64 ; FAT32, 1 byte
BS_FAT32_Reserved1 equ 65 ; FAT32, 1 byte
BS_FAT32_BootSig equ 66 ; FAT32, 1 byte
BS_FAT32_VolID equ 67 ; FAT32, 4 bytes
BS_FAT32_VolLab equ 71 ; FAT32, 11 bytes
BS_FAT32_FilSysType equ 82 ; FAT32, 8 bytes
BS_FAT32_BootCode equ 90

; 29/02/2016
;(FAT32 Free Cluster Count & First Free Cluster values)
;[BPB_Reserved] = Free Cluster Count (offset 52)
;[BPB_Reserved+4] = First Free Cluster (offset 56)

BS_Validation equ 510

; 15/02/2016
; FILE.ASM - 09/10/2011
; Directory Entry Structure
; 29/10/2009 (According to Microsoft FAT32 File System Specification)
DirEntry_Name equ 0
DirEntry_Attr equ 11
DirEntry_NTRes equ 12
DirEntry_CrtTimeTenth equ 13
DirEntry_CrtTime equ 14
DirEntry_CrtDate equ 16
DirEntry_LastAccDate equ 18
DirEntry_FstClusHI equ 20
DirEntry_WrtTime equ 22
DirEntry_WrtDate equ 24
DirEntry_FstClusLO equ 26
DirEntry_FileSize equ 28