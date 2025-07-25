; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - UNINITIALIZED DATA : trdoskx.s
; ----------------------------------------------------------------------------
; Last Update: 25/07/2025 (Previous: 01/09/2024 - Kernel v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 04/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ****************************************************************************
; DRV_INIT.ASM [26/09/2009] Last Update: 07/08/2011
; MAINPROG.ASM [17/01/2004] Last Update: 09/11/2011
; DIR.ASM      [17/01/2004] Last Update: 09/10/2011
; CMD_INTR.ASM [29/01/2005] Last update: 09/11/2011
; DRV_FAT.ASM  [07/07/2009] Last update: 21/08/2011

alignb 4

; MAINPROG.ASM
MainProgCfg_FileSize:   resd 1 ; 14/04/2016
MainProgCfg_LineOffset: resd 1 ; 14/04/2016

Current_VolSerial: resd 1

Current_Dir_FCluster: resd 1

Current_Dir_Level: resb 1
Current_FATType: resb 1
Current_Drv: resb 1
Current_Dir_Drv:   resb 1 ; '?'
                   resb 1 ; ':'
Current_Dir_Root:  resb 1 ; '/'
;Current_Directory: resb 90
; 14/05/2025 - TRDOS 386 v2.0.10
Current_Directory: resb 103 ; 8 sub dir levels (7*13 + 12)
End_Of_Current_Dir_Str: resb 1
Current_Dir_StrLen: resb 1

CursorColumn: 	resb 1
CmdArgStart:    resb 1

; 03/02/2016
Remark:		resb 78

CommandBuffer: 	resb 80

TextBuffer:	resb 256
; 02/07/2025 - TRDOS 386 v2.0.10
RunPathBuffer:	resb 388

MasterBootBuff:
MasterBootCode: resb 1BEh
PartitionTable: resb 64
MBIDCode: resw 1

PTable_Buffer:
PTable_hd0: resb 64
PTable_hd1: resb 64
PTable_hd2: resb 64
PTable_hd3: resb 64
; 15/07/2020
;PTable_ep0: resb 64
;PTable_ep1: resb 64
;PTable_ep2: resb 64
;PTable_ep3: resb 64

; 22/05/2024
HD_LBA_yes: resd 1
; 13/08/2020
scount: resb 1 ; 16/05/2016 (diskio.s, 'int33h:')
; 22/05/2024
	;resb 1
	;resb 1
	;resb 1
;HD_LBA_yes: resb 1
PP_Counter: resb 1
EP_Counter: resb 1
; 13/08/2020
LD_Counter: resb 1

; 30/08/2020
MBR_EP_StartSector: resd 1
		; Extd partition start sector as in MBR
EP_StartSector: resd 1 ; next extd partition start sector
	; 15/07/2020
                ;resd 1
                ;resd 1

; 20/07/2020
DOSBootSectorBuff: resb 512
; 15/07/2020
;DOSBootSectorBuff: resb 446 ; 1BEh
;MiniPartitionTable: resb 64 ;  40h
;MiniPartitionMagic: resw  1 ;  02h

FAT_BuffDescriptor:
FAT_CurrentCluster: resd 1
FAT_BuffValidData: resb 1
FAT_BuffDrvName: resb 1
FAT_BuffOffset: resw 1
FAT_BuffSector: resd 1

FAT_ClusterCounter: resd 1
LastCluster: resd 1

; 16/05/2016
;; 18/03/2016 (TRDOS v2.0)
;ClusterBuffer_Valid: resb 1

; 29/07/2022
;resb 1
; 02/12/2023
P_TIMER: resb 1 ; diskette change check (2 seconds)

Dir_BuffDescriptor:
DirBuff_DRV: resb 1
DirBuff_FATType: resb 1
DirBuff_ValidData: resb 1
DirBuff_CurrentEntry: resw 1
DirBuff_LastEntry: resw 1
DirBuff_Cluster: resd 1
DirBuffer_Size: resw 1
;DirBuff_EntryCounter: resw 1

; 01/02/2016
; these are on (real mode) segment 8000h and later
; FAT_Buffer:	resb 1536 ; 3 sectors
; Dir_Buffer:	resb 512*32
; Logical_DOSDisks:  resb 6656 ; 26 * 256 bytes

; 18/01/2016

FreeClusterCount: resd 1
; 04/05/2025
FirstFreeCluster: resd 1

VolSize_Unit1:   resd 1
VolSize_Unit2:   resd 1

Vol_Tot_Sec_Str_Start:	    resd 1
Vol_Tot_Sec_Str: 	    resb 10
Vol_Tot_Sec_Str_End:	    resb 1
resb 1
Vol_Free_Sectors_Str_Start: resd 1
Vol_Free_Sectors_Str:	    resb 10
Vol_Free_Sectors_Str_End:   resb 1

; 10/02/2016
RUN_CDRV: resb 1 ; CMD_INTR.ASM  ; 09/11/2011

; 24/01/2016
PATH_Array:     resb 128 ; DIR.ASM ; 09/10/2011

; 22/06/2025
; 06/02/2016
;CCD_DriveDT:	resd 1 ; DIR.ASM ; (word)
; 24/06/2025
Current_LDRVT:	resd 1

; 14/06/2025
;CCD_Level:	resb 1 ; DIR.ASM
;Last_Dir_Level: resb 1 ; DIR.ASM
;
CDLF_FNAddress:	resd 1 ; DIR.ASM (word)
CDLF_AttributesMask: resw 1 ; DIR.ASM
CDLF_DEType:	resw 1 ; DIR.ASM
; 29/05/2025 - TRDOS 386 v2.0.10
DirEntry_Counter: resd 1 ; directory entry index number
;
CD_COMMAND:	resb 1 ; DIR.ASM

alignb 4

; 29/01/2016
Program_Exit:	resb 1 ; CMD_INTR.ASM  ; 09/11/2011

;alignb 4
; 23/02/2016
disk_rw_op:	resb 1 ; 0 = disk read, 1 = disk write
;disk_rw_spt:	resb 1 ; sectors per track (<= 63) /// (<256)
; 31/01/2016
retry_count: 	resb 1 ; DISK_IO.ASM ; 20/07/2011 (CHS_RetryCount)
disk_rw_err: 	resb 1 ; DISK_IO.ASM ; (Disk_IO_err_code)
sector_count:	resd 1 ; DISK_IO.ASM ; (Disk_RW_SectorCount)

; 06/02/2016 (long name)
FDE_AttrMask:	   resw 1 ; DIR.ASM
;AmbiguousFileName: resw 1 ; DIR.ASM
; 18/05/2025 - TRDOS 386 v2.0.10
AmbiguousFileName: resb 1
PreviousAttr:	   resb 1 ; DIR.ASM
; 01/06/2025
LFN_level:	 resb 1   ; 25/05/2025
LongNameFound:	 resb 1	  ; DIR.ASM
LFN_EntryLength: resb 1   ; DIR.ASM
LFN_CheckSum:	 resb 1   ; DIR.ASM
;LongFileName:	 resb 132 ; DIR.ASM
; 25/05/2025 - TRDOS 386 v2.0.10
LongFileName:	 resb 260 ; 130 UNICODE (2-byte) chars

; 16/06/2025
Current_DDT:
; 13/06/2025 - TRDOS 386 v2.0.10
DIR_FCluster:	resd 1
 
;PATH_Array_Ptr: resw 1 ; DIR.ASM
PATH_CDLevel:	 resb 1 ; DIR.ASM
; 14/06/2025
;PATH_Level:	 resb 1 ; DIR.ASM

; 07/02/2016
Dir_File_Name:	resb 13 ; DIR.ASM ; 09/10/2011

; 18/05/2025
; 10/02/2016
;Dir_Entry_Name: resb 13 ; DIR.ASM

alignb 2

AttributesMask: resw 1 ; CMD_INTR.ASM ; 09/11/2011

; 10/02/2016 (128 bytes -> 126 bytes)
; 08/02/2016
;FFF Structure (128 bytes) ; DIR.ASM ; 09/10/2011
FINDFILE_BUFFER:	; 11/05/2025

FindFile_Drv:		  resb 1
;FindFile_Directory:	  resb 65
; 15/05/2025
FindFile_Directory:	  resb 104 ; 7*13 + 12 + zero
FindFile_Name:		  resb 13
; 17/05/2025
FindFile_AttributesMask:  resw 1
FindFile_DirEntry:	  resb 32
FindFile_DirFirstCluster: resd 1
FindFile_DirCluster:	  resd 1
FindFile_DirSector:	  resd 1
FindFile_DirEntryNumber:  resb 1
FindFile_DirSectorCount:  resb 1
FindFile_MatchCounter:	  resw 1
;FindFile_Reserved1:	  resw 1 ; 06/03/2016
FindFile_LastEntryNumber: resw 1 ; 29/05/2025
FindFile_LongNameEntryLength:
FindFile_LongNameYes: 	  resb 1 ; Sign for longname procedures
;FindFile_Reserved2:	  resb 1 ; 17/05/2025
;FindFile_DirBuffer:	  resd 1 ; 19/05/2025
FindFile_DirEntryName:	  resb 13 ; 18/05/2025
;FindFile_Reserved3:	  resb 1 ; 18/05/2025
; 184 bytes ; 19/05/2025
; 25/05/2025
FindFile_LongName:	  resb 129 ; ASCIIZ (max. 128+NUL)
; 01/07/2025
FindFile_Attributes:	  resb 1
FindFile_Size:		  resd 1
FindFile_FirstCluster:	  resd 1
;FindFile_LmDateTime:	  resd 1
FindFile_FATType:	  resb 1

; 26/06/2025
Path_FileSystem:	  resb 1
Path_Drv:		  resb 1
Path_Directory:		  resb 256 ; ASCIIZ (max. 256+NUL)
Path_FileName:		  resb 129
Path_LongFlag:		  resb 1 ; Flag for using Long Path

First_Path_Pos: resd 1	; DIR.ASM ; 09/10/2011
Last_Slash_Pos: resd 1	; DIR.ASM

; 10/02/2016
File_Count:     resw 1 	; DIR.ASM ; 09/10/2011
Dir_Count:      resw 1
Total_FSize:    resd 1
TFS_Dec_Begin:  resd 1
                resb 10
TFS_Dec_End:    resb 1

PrintDir_RowCounter: resb 1

alignb 4
; 28/06/2025
Show_Buffer:	resd 1
; 15/02/2015 ('show' command variables)
Show_FDT:	resd 1
; 26/06/2025
;Show_LDDDT:	resd 1
Show_Cluster:	resd 1
Show_FileSize:	resd 1
Show_FilePointer: resd 1
;Show_ClusterPointer: resw 1
;Show_ClusterSize: resw 1
; 28/06/2025
Show_FileSector: resd 1
Show_ClusterSize: resd 1
Show_SectorCount: resd 1
Show_RowCount:	resb 1

alignb 4
; 21/02/2016
DelFile_FNPointer:	resd 1 ; ; CMD_INTR.ASM (word) ; 09/11/2011
; 27/02/2016
; DIR.ASM (09/10/2011)
rmdir_FCluster:	; 17/07/2025
DelFile_FCluster:	resd 1
rmdir_EntryNumber: ; 18/07/2025
DelFile_EntryNumber:	resd 1 ; 21/07/2025
rmdir_LNEL: ; 16/07/2025
DelFile_LNEL:		resb 1
			resb 1

; DIR.ASM
rmdir_DirName_Offset:	; 16/07/2025
mkdir_DirName_Offset: 	resd 1
mkdir_FFCluster:	resd 1
mkdir_LastDirCluster:	resd 1
mkdir_FreeSectors:	resd 1
mkdir_attrib:		resw 1
mkdir_SecPerClust:	resb 1
mkdir_add_new_cluster:	resb 1
mkdir_Name:		resb 13
			resw 1 ; 01/03/2016
; 14/07/2025 - TRDOS 386 v2.0.10
mkdir_phydrv:		resb 1
mkdir_dirsector:	resd 1
mkdir_datetime:		resd 1
mkdir_entrypos:		resd 1

; 27/02/2016
RmDir_MultiClusters:	resb 1
RmDir_DirEntryOffset:	resd 1 ; 01/03/2016 (word -> dword)
RmDir_ParentDirCluster: resd 1
RmDir_DirLastCluster:   resd 1
RmDir_PreviousCluster:  resd 1

delfile_dir_fcluster:	; 21/07/2025
; 17/07/2025
rmdir_dir_fcluster:	resd 1
delfile_drv:		; 21/07/2025
rmdir_drv:		resb 1

; 16/07/2025
LMDT_Flag:	resb 1  ; Last Modification Date & Time update flag
; 22/02/2016
UPDLMDT_CDirLevel:	resb 1
UPDLMDT_CDirFCluster:	resd 1

alignb 4
; DRV_FAT.ASM ; 21/08/2011
gffc_next_free_cluster:  resd 1
; 12/07/2025
FCS_START:	
gffc_first_free_cluster: resd 1
FCS_END:
gffc_last_free_cluster:  resd 1

; 29/04/2016
Cluster_Index: ; resd 1
; 22/02/2016
ClusterValue:	resd 1

; 04/03/2016
Attributes:	resb 1
;;CFS_error:  resb 1 ;; 01/03/2016
; 13/06/2025 (negative attributes)
NegAttribs:	resb 1

CFS_OPType: resb 1
CFS_Drv:    resb 1
CFS_CC:	    resd 1
CFS_FAT32FSINFOSEC: resd 1
CFS_FAT32FC: resd 1

; 27/02/2016
;alignb 4
glc_prevcluster: resd 1 ; DRV_FAT.ASM (21/08/2011)
; 22/10/2016
glc_index:	 resd 1 ; Last Cluster Index (22/10/2016)

; DIR.ASM
DLN_EntryNumber: resw 1
DLN_40h:	 resb 1
; 28/02/2016
TCC_FATErr:	 resb 1 ; DRV_FAT.ASM

alignb 4
; DIR.ASM (09/10/2011)
LCDE_EntryIndex: resw 1 ; LCDE_EntryOffset
LCDE_ClusterSN:  resw 1
LCDE_Cluster: 	 resd 1
LCDE_ByteOffset: resd 1

;alignb4
; 06/03/2016 (word -> dword)
; CMD_INTR.ASM (01/08/2010)
SourceFilePath:	     resd 1
DestinationFilePath: resd 1

;alignb 4
; 25/07/2025 - TRDOS 386 v2.0.10
; 06/03/2016
; FILE.ASM (09/10/2011)
;Source File Structure (same with 'Find File' Structure)
SourceFile_Drv:		  resb 1
SourceFile_Directory:	  resb 104 ; 7*13 + 12 + zero
SourceFile_Name:	  resb 13
SourceFile_AttributesMask: resw 1
SourceFile_DirEntry:	  resb 32
SourceFile_DirFirstCluster: resd 1
SourceFile_DirCluster:	  resd 1
SourceFile_DirSector:	  resd 1
SourceFile_DirEntryNumber: resb 1
SourceFile_DirSectorCount: resb 1
SourceFile_MatchCounter:  resw 1
;SourceFile_Reserved1:	  resw 1
SourceFile_LastEntryNumber: resw 1
SourceFile_LongNameEntryLength:
SourceFile_LongNameYes:	  resb 1 ; Sign for longname procedures
SourceFile_DirEntryName:  resb 13
; 184 bytes
;SourceFile_LongName:	  resb 129 ; ASCIIZ (max. 128+NUL)
; 01/07/2025
;SourceFile_Attributes:	  resb 1
;SourceFile_Size:	  resd 1
;SourceFile_FirstCluster: resd 1
;;SourceFile_LmDateTime:  resd 1
;SourceFile_FATType:	  resb 1

; 25/07/2025 - TRDOS 386 v2.0.10
;Destination File Structure (same with 'Find File' Structure)
DestinationFile_Drv:		resb 1
DestinationFile_Directory:	resb 104 ; 7*13 + 12 + zero
DestinationFile_Name:		resb 13
DestinationFile_AttributesMask:	resw 1
DestinationFile_DirEntry:	resb 32
DestinationFile_DirFirstCluster: resd 1
DestinationFile_DirCluster:	resd 1
DestinationFile_DirSector:	resd 1
DestinationFile_DirEntryNumber:	resb 1
DestinationFile_DirSectorCount:	resb 1
DestinationFile_MatchCounter:	resw 1
;DestinationFile_Reserved1:	resw 1
DestinationFile_LastEntryNumber: resw 1
DestinationFile_LongNameEntryLength:
DestinationFile_LongNameYes:	resb 1 ; Sign for longname procedures
DestinationFile_DirEntryName:	resb 13
; 184 bytes
;DestinationFile_LongName:	resb 129 ; ASCIIZ (max. 128+NUL)
; 01/07/2025
;DestinationFile_Attributes:	resb 1
;DestinationFile_Size:		resd 1
;DestinationFile_FirstCluster:	resd 1
;;DestinationFile_LmDateTime:	resd 1
;DestinationFile_FATType:	resb 1

; 25/07/2025
SourceFile_SecPerClust: resb 1
DestinationFile_SecPerClust: resb 1 

; 10/03/2016
; FILE.ASM
move_cmd_phase:	   resb 1
msftdf_sf_df_drv:  resb 1
msftdf_drv_offset: resd 1

; 11/03/2016
; DRV_FAT.ASM (21/08/2011)
FAT_anc_LCluster:  resd 1
FAT_anc_FFCluster: resd 1

;alignb 4

; 14/03/2016
; TRDOS 386 = TRDOS v2.0 feature only !
; 'allocate_memory_block' in 'memory.s'
mem_ipg_count:	resd 1 ; page count (for contiguous allocation)
mem_pg_count:	resd 1 ; page count (for count down)
mem_aperture:	resd 1 ; contiguous free pages (current)
mem_max_aperture: resd 1 ; maximum value of contiguous free pages
mem_pg_pos:	resd 1 ; mem. position (page #) of current aperture
mem_max_pg_pos: resd 1 ; mem. position (page #) of max. aperture

; 15/03/2016
; FILE.ASM ('copy_source_file_to_destination_file')
copy_cmd_phase:       resb 1
csftdf_rw_err:	      resb 1
DestinationFileFound: resb 1
csftdf_cdrv: 	      resb 1
csftdf_filesize:      resd 1
; TRDOS386 (TRDOS v2.0)
csftdf_sf_mem_addr:   resd 1
csftdf_sf_mem_bsize:  resd 1
;

csftdf_sf_cluster:    resd 1 ; 16/03/2016
csftdf_df_cluster:    resd 1
; 16/03/2016
csftdf_r_size:        resd 1
csftdf_w_size:        resd 1
csftdf_sf_rbytes:     resd 1
csftdf_df_wbytes:     resd 1
csftdf_percentage:    resb 1
; 17/03/2016
csftdf_videopage:     resb 1
csftdf_cursorpos:     resw 1
csftdf_sf_drv_dt:     resd 1
csftdf_df_drv_dt:     resd 1
; 01/09/2024
; 29/08/2024
;csftdf_df_dclust:    resd 1
;csftdf_df_dindex:    resd 1

; 21/03/2016
; 20/03/2016
; FILE.ASM
createfile_Name_Offset:  resd 1
createfile_FreeSectors:  resd 1
createfile_size:         resd 1
createfile_FFCluster:    resd 1 ; 11/03/2016
createfile_LastDirCluster: resd 1
createfile_Cluster:      resd 1
createfile_PCluster:     resd 1
createfile_attrib:	 resb 1
createfile_SecPerClust:  resb 1
createfile_DirIndex:     resw 1
createfile_CCount:	 resd 1
createfile_BytesPerSec:	 resw 1 ; 23/03/2016
createfile_wfc:	         resb 1
createfile_UpdatePDir:	 resb 1 ; 31/03/2016

;alignb 4

; 11/04/2016
env_var_length:	resw 1

alignb 4

; 25/04/2016
readi.valid:	resb 1 ; valid data (>0 = valid for readi)
readi.drv:	resb 1 ; drive number (0, 1,2,3,4..)
readi.spc:	resb 1 ; sectors per cluster for 'readi' drive
readi.s_index:  resb 1 ; sector index in current cluster (buffer)
readi.sector:	resd 1 ; current disk sector
readi.bpc:	resw 1 ; bytes per cluster - 1
readi.offset:	resw 1 ; byte offset in cluster buffer
readi.cluster:  resd 1 ; current cluster number
readi.c_index:	resd 1 ; cluster index of the current cluster (0,1,2,3..)
readi.fclust:	resd 1 ; first cluster of the current cluster
; 01/07/2025
;readi.fs_index: resd 1 ; sector index in disk/file section (for Singlix FS)
;readi.buffer:	resd 1 ; readi sector buffer address

;alignb 4

writei.valid:	resb 1 ; valid data (>0 = valid for writei)
writei.drv:	resb 1 ; drive number (0, 1,2,3,4..)
writei.spc:	resb 1 ; sectors per cluster for 'writei' drive
writei.s_index: resb 1 ; sector index in current cluster (buffer)
writei.sector:	resd 1 ; current disk sector
writei.bpc:	resw 1 ; bytes per cluster - 1
writei.offset:	resw 1 ; byte offset in cluster buffer
writei.cluster: resd 1 ; current cluster number
writei.c_index:	resd 1 ; cluster index of the current cluster (0,1,2,3..)
writei.fclust:  resd 1 ; first cluster of the current cluster
writei.fs_index: resd 1 ; sector index in disk/file section (for Singlix FS)
;writei.buffer:	resd 1 ; writei sector buffer address
writei.lclust:	resd 1 ; writei last cluster (mget_w) ; 23/10/2016
writei.l_index:	resd 1 ; writei last cluster index (mget_w) ; 23/10/2016
writei.ofn:	resb 1 ; open file number (to be written) ; 23/10/2016

alignb 4

; 29/04/2016
Run_CDirFC:	resd 1
;Run_Auto_Path:	resb 1
; 02/07/2025
Run_Path_Length:
		resd 1
Run_Auto_Path:	resd 1
Run_Manual_Path: 
		resb 1 ; 0 -> auto path sequence needed
EXE_ID:		resb 1
EXE_dot:	resb 1
		resb 1 ; 02/07/2025

; 06/05/2016
mainprog_return_addr: resd 1
last_error:	resd 1  ; this will be used to return error code to MainProg
			; 'lasterror' keyword will be used later to get the
			; last error code/number/status.
; 12/05/2016
video_eax:	resd 1  ; eax return value of video function

; 01/06/2016
user_buffer:	resd 1  ; 'diskio.s' (INT 33h, Function 08h, floppy disk type)

; 21/05/2016 - TRDOS 386 ('swap/switch', 'rswap', [u.pri])
priority:	resb 1  ; running priority level of process (0,1,2)
			; (run queue which is process comes from)
; 22/05/2016 - TRDOS 386 ('set_run_sequence', 'rtc_int', 'u_timer')
p_change:	resb 1  ; process change status (for timer events)
; 23/05/2016 - TRDOS 386 ('clock')
multi_tasking:	resb 1   ; Multi Tasking status (0 = disabled, >0 = enabled)
			; (EBX will return with user buffer addr or disk type)
; 07/06/2016
timer_events:	resb 1  ; number of (active) timer events, <= 16

; 24/06/2016
w_str_cmd:	resb 1	; WRITE_STRING command (0,1,2,3) ; video.s
p_crt_mode:	resb 1  ; previous video mode (=3 or 0), backup mark/sign
; 26/06/2016
p_crt_page:	resb 1  ; previous active page (for 'set_mode')
; 04/07/2016
noclearmem:	resb 1  ; if set, 'SET MODE' (INT 31h) function (AH = 4)
			; will not clear the video memory
			; (usable for graphics modes only)
alignb 2
CRT_LEN:	resw 1  ; length of regen buffer in bytes
cursor_pposn:	resw 8  ; cursor positions backup

; 10/07/2016 ('VGA_FONT_SETUP', INT 43H address for x86 real mode bios)
VGA_INT43H:	resd 1	; 0 = default (not configured by user)
			; 0FFFFFFFFh = user defined fonts
			; address:
			; 	vgafont8
			; 	vgafont16
			; 	vgafont14

; 25/07/2016
VGA_MTYPE:	resb 1  ; 0=CTEXT,1=MTEXT,2=CGA,3=PLANAR1,4=PLANAR4,5=LINEAR

; 23/10/2016
setfmod:	resb 1	; update last modification date&time sign (if >0)
			; (it is Open File Number + 1, if > 0)
; 27/08/2024
setfclust:	resb 1  ; first cluster of file
			; (is used by update lmdt proc)
alignb 4

; 16/10/2016
FFF_UBuffer:	resd 1  ; User's buffer address for FFF & FNF system calls
; 18/05/2025 - TRDOS 386 v2.0.10
FFF_mpid:	resw 1
; 20/05/2025
FFF_FATtype:	resb 1
; 15/10/2016
FFF_Valid:	resb 1  ; Find First File Structure validation byte
			; 0  = invalid (Find Next File can't use FFF struct)
			; >0 = valid, return type for FFF and Find Next File
			; 24 = basic parameters, 24 bytes
			; 128 = entire FFF structure/table, 128 bytes
; 16/10/2016 (FFF_Attrib: resw 1)
FFF_Attrib:	resb 1	; Find First File attributes for Find Next File (LB)
FFF_RType:	resb 1  ; FFF return type (0 = Basic, >0 = complete) (HB)
; 16/10/2016 - 05/10/2016 (Set Working Path)
SWP_inv_fname:	resb 1	; Set Working Path - Invalid File Name
SWP_Mode:	resw 1	; Set Working Path - Mode
SWP_DRV:	resb 1	; Set Working Path - Drive
SWP_DRV_chg:	resb 1	; Set Working Path - Drive Change

; 27/02/2017
fpready:	resb 1	; '80387 fpu is ready' flag

; 17/04/2021
; (DEVICE parameters is disabled as temporary)

; 08/10/2016
;device_name:	resb 9  ; capitalized (and zero padded) device name
			; (example: "TTY0",0,0,0,0,0")

; 11/07/2025 - TRDOS 386 v2.0.10
current_file:	resb 1	; open file number (SFT number)
; 14/07/2025
FAILERR:	resb 1	; ('BUFWRITE' error return)

alignb 4

; 08/10/2016
; 07/10/2016
; Table of kernel devices (which do not use installable device drivers)
; has been coded into KERNEL (trdosk9.s)
; 07/10/2016
; 8 installable device drivers available to install (NUMIDEV)
;IDEV_PGDIR: resd NUMIDEV
			; Page directories of installable device drivers
			;
			; Note: Virtual start address is always 400000h
			; (end of the 1st 4MB). [org 400000h]
			; Segments: KCODE, KDATA
			; Method: call 400000h (after changing page dir)
			; Query code located at the start (400000h).
			; Query code returns with
			;   eax = device type and driver version
			;         AL = Device Type minor
			;         AH = Device Type major
			;         Byte 16-23 : Version minor
			;	  Byte 24-31 : Version major - 1
			;		       (0:0 -> 1.0)
			;   ebx = initialization code address
			;   ecx = configuration table address
			;   edx = description table address
			;   esi = device (default) name address (ASCIIZ)
			;	 (name has "/DEV/" prefix)
			;   edi = dispatch table address
			;        (for calling kernel-device functions)
			;   ebp = address table address
			; Initialization code returns with
			;   eax = open code address
			;   ecx = close code address
			;   ebx = read code address
			;   edx = write code address
			;   esi = IOCTL code address
			;   edi = dispatch table address
			;   ebp = address table address
			; Address Table:
			;    Offset 0  : open code address
			;    Offset 4  : read code address
			;    Offset 8  : write code address
			;    Offset 12 : close code address
			;    Offset 16 : IOCTL code address
			;    Offset 20 : initialization code address
			;    Offset 24 : description table address
			;    Offset 28 : configuration table address
			;    Offset 32 : device name address
			;    Offset 36 : dispatch table address
			;          (for calling kernel-device functions)

;IDEV_NAME:  resb 8*NUMIDEV
			  ; 8 byte names of installable device drivers

;IDEV_TYPE:  resb NUMIDEV ; Driver type of installable device drivers
;IDEV_FLAGS: resb NUMIDEV ; Device access parameters for installable
                          ; device drivers (These values are set while
			  ; the device driver is being loaded.)
;IDEV_OADDR: resd NUMIDEV ; open function addr for installable dev driver
;IDEV_CADDR: resd NUMIDEV ; close function addr for installable dev driver
;IDEV_RADDR: resd NUMIDEV ; read function addr for installable dev driver
;IDEV_WADDR: resd NUMIDEV ; write function addr for installable dev driver

; 08/10/2016
; 07/10/2016
; Device Open and Access parameters
;DEV_ACCESS:	resb NUMOFDEVICES    ; bit 0 = accessable by normal users
				     ; bit 1 = read access permission
				     ; bit 2 = write access permission
				     ; bit 3 = IOCTL permission to users
				     ; bit 4 = block device if it is set
				     ; bit 5 = 16 bit or 1024 byte data
				     ; bit 6 = 32 bit or 2048 byte data
				     ; bit 7 = installable device driver
;DEV_R_OWNER:	resb NUMOFDEVICES    ; Reading owner no (u.uid) of devices
;DEV_R_OPENCOUNT: resb NUMOFDEVICES  ; Reading open count
;DEV_W_OWNER:	resb NUMOFDEVICES    ; Writing owner no (u.uid) of devices
;DEV_W_OPENCOUNT: resb NUMOFDEVICES  ; Writing open count
;DEV_DRIVER:	resb NUMOFDEVICES    ; device driver number (1 to 7Fh)
				     ; *if bit 7 is set (80 to FFh)
				     ; *if it is installable device driver
				     ; *index (0 to 7Fh)
				     ; otherwise it is kernel device index
;DEV_OPENMODE:	resb NUMOFDEVICES    ; 1 = read mode
				     ; 2 = write mode
				     ; 3 = read & write
				     ; 0 = not open (free)
;DEV_NAME_PTR:	resd NUMOFDEVICES    ; pointers to name addresses of drivers
				     ; Address base: KDEV_NAME+
				     ; or IDEV_NAME+
;DEV_R_POINTER:	resd NUMOFDEVICES    ; reading pointer, writing pointer
;DEV_W_POINTER:	resd NUMOFDEVICES    ; sector number if block device
				     ; character offset if char device
alignb 4

; 06/10/2016
; Open File Parameters
OF_FCLUSTER:	resd OPENFILES  ; First clusters of open files
OF_DRIVE:	resb OPENFILES  ; Logical DOS drive numbers of open files
OF_MODE:	resb OPENFILES  ; Open mode (1 = read, 2 = write, 3 = r&w)
OF_STATUS:	resb OPENFILES  ; (bit 0 = read, bit 1 = write)
OF_OPENCOUNT:	resb OPENFILES  ; Open counts of open files
OF_POINTER:	resd OPENFILES	; File seek/read/write pointer
OF_SIZE:	resd OPENFILES	; File sizes of open files (in bytes)
OF_DIRFCLUSTER:	resd OPENFILES  ; Directory First Clusters of open files
OF_DIRCLUSTER:	resd OPENFILES  ; Directory (Entry) Clusters of open files
OF_VOLUMEID:	resd OPENFILES  ; Vol ID for removable drives of open files
OF_CCLUSTER:	resd OPENFILES  ; Current clusters of open files
OF_CCINDEX:	resd OPENFILES  ; Cluster index numbers of current clusters
; 24/10/2016
OF_DIRENTRY:	resw OPENFILES  ; Directory entry index no. in dir cluster
				; Sector index = entry index / 16
; 21/04/2025 - TRDOS 386 v2.0.10
OF_DIRSECTOR:	resd OPENFILES	; Directory Sector Numbers of open files
OF_DIRPOS:	resb OPENFILES	; Directory entry index in directory sector
; 24/04/2025
OF_NAME:	resb OPENFILES*12 ; File name in directory entry format
OF_ATTRIB:	resb OPENFILES	; File attributes
OF_DATETIME:	resd OPENFILES	; Last modification time (LW) and date (hw)
; 11/07/2025
OF_LCLUSTER:	resd OPENFILES  ; Last clusters of open files

;alignb 2

DTA:		;resd 24	; Find First File data transfer area
		resb 24		; 29/07/2022

; 19/12/2016
tcallback:	resb 1		; Timer callback method flag for 'systimer'
trtc:		resb 1		; Timer interrupt type flag for 'systimer'
; 20/02/2017
no_page_swap:	resb 1		; Swap lock for Signal Response Byte pages
;;15/01/2017
; 02/01/2017
;;intflg:	resb 1		; software interrupt in progress signal
				; (for timer interrupt)
alignb 4
; 13/04/2017
;DEV_INTR:	resb NUMOFDEVICES ; Device Interrupt (IRQ) number + 1
				; (0= not available, 1= IRQ 0, 16= IRQ 15)
DEV_INT_HNDLR:	resd 16		; Device Interrupt Handler addr, if > 0

;alignb 4

; 26/02/2017 ; IRQ Callback parameters ('syscalbac')
;Index: ; 0 to 8
;	0 = IRQ3, 1 = IRQ4, 2 = IRQ5, 3 = IRQ7
;	4 = IRQ9, 5 = IRQ10, 6 = IRQ11, 7 = IRQ12, 8 = IRQ13  
IRQ.owner:	resb 9		; owner, 0 = free, >0 = [u.uno]
IRQ.dev:	resb 9		; 0 = default/kernel, >0 = device number
IRQ.method:	resb 9 		; 0 = Signal Response Byte, 1 = Callback
IRQ.srb:	resb 9 		; Signal Response/Return Byte value
IRQ.addr:	resd 9		; Rignal Response Byte address (physical)
				; or Callback service address (virtual)
; 28/02/2017
IRQ_cr3:	resd 1		; for saving cr3 register in IRQ handler
IRQnum:		resb 1		; IRQ number for IRQ handler (trdosk8.s)

; 10/04/2017
; 03/04/2017
; UNINITIALIZED AUDIO DATA
alignb 4
audio_pci:	resb 1
audio_device:	resb 1
;audio_mode:	resb 1
audio_intr:	resb 1
audio_busy:	resb 1  ; Busy flag for audio irq ; 21/04/2017
;audio_reserved: resb 1
; 20/11/2023
NAMBAR:		resw 1	; Native Audio Mixer Base Address
NABMBAR:	; 02/10/2023 (NABMBAR = audio_io_base)
audio_io_base:	resw 1 	; Base I/O address of audio device
audio_dev_id:	resd 1	; BUS/DEV/FN ; 00000000BBBBBBBBDDDDDFFF00000000
audio_vendor:	resd 1
audio_stats_cmd: resd 1
;
audio_buffer:	resd 1	; virtual address of user's audio buffer
audio_p_buffer:	resd 1	; Physical address of user's audio buffer
audio_buff_size: resd 1 ; user's audio buffer size (half buffer size)
audio_dma_buff: resd 1  ; dma buffer address
audio_dmabuff_size: resd 1 ; dma buffer size (2 * half buffer size)
; 05/06/2024
dma_hbuff_size: resd 1 ; dma half buffer size
;
audio_flag:	resb 1  ; dma buffer flag (1st half = 0, 2nd half = 1)
audio_user:	resb 1	; user number of the owner
audio_cb_mode:	resb 1	; 0 = signal response byte method
			; 1 = callback method
			; 2 = s.r.b. method with auto increment
audio_srb:	resb 1	; signal response byte value
audio_cb_addr:	resd 1  ; callback service address or s.r.b. address
			; (s.r.b. addr is physical, cbs addr is virtual)

audio_bps:	resb 1  ; selected mode: 8 bit, 16 bit
audio_stmo:	resb 1	; selected mode: mono /stereo
audio_freq: 	resw 1	; sampling rate
; 20/11/2023
VRA:		resb 1
audio_mode:	resb 1

; 21/04/2017
audio_play_cmd: resb 1  ; Play/Stop command (1 = play, 0 = stop)
; 21/11/2023
;audio_civ: ; 28/05/2017 ; Current Buffer Index (AC'97)
; 23/05/2024
LVI:			; AC'97 Last Valid Buffer Index
audio_flag_eol:	resb 1  ; End of Link status (vt8233, EOL/FLAG)

audio_master_volume:
audio_master_volume_l: resb 1 ; sound volume (lineout) left channel
audio_master_volume_r: resb 1 ; sound volume (lineout) right channel

; 24/05/2024
audio_pcmo_volume:
audio_pcmo_volume_l: resb 1 ; PCM out volume left channel
audio_pcmo_volume_r: resb 1 ; PCM out volume right channel

; 02/06/2024
;alignb 4

; 20/11/2023
; 28/05/2017
; AC'97 Audio Controller Base Adress Registers
;NAMBAR:	resw 1	; Native Audio Mixer Base Address
;NABMBAR:	resw 1	; Native Audio Bus Mastering Base Address

; 02/06/2024
alignb 8

; 21/04/2017
audio_bdl_buff:	resd 32*8 ; VT8233 (AC97) BDL Buffer Size
; 12/05/2017
base_addr:	resd 1	; 'direct_memory_access' (memory.s)

; 01/06/2024
reset:		resb 1	; AC97 init

; 28/08/2017
; 20/08/2017
dma_user:	resb 1	; user number for sysdma
dma_channel:	resb 1	; dma channel for sysdma
dma_mode:	resb 1  ; dma mode for sysdma

dma_addr:	resd 1	; dma buffer physical addr for sysdma
dma_size:	resd 1  ; dma buffer size (in bytes) for sysdma
dma_start:	resd 1  ; dma start address for sysdma
dma_count:	resd 1  ; dma count (in bytes) for sysdma

; 04/12/2023
%if 0

alignb 65536
; 09/08/2017
; 12/05/2017
sb16_dma_buffer: resb 65536 ; DMA buffer for sb16 audio playing.

%endif

; 17/04/2024 - TRDOS 386 v2.0.10
BufferQueue:	resd 1	; (MSDOS -> BufferQueue)
DirtyBufferCount:
		resd 1	; (MSDOS -> DirtyBufferCount)
; 05/06/2025
;buf_prev_off:	resd 1

; 03/05/2025
LastBuffer:	resd 1 	; (MSDOS -> LastBuffer)
FIRST_BUFF_ADDR:
		resd 1	; (MSDOS -> FIRST_BUFF_ADDR)
CL0FATENTRY	resd 1	; (MSDOS -> CL0FATENTRY)

CurrentBuffer:	resd 1	; (MSDOS -> CURCUF)
CLUSNUM:	resd 1	; (MSDOS -> CLUSNUM)
ClusSec:	resd 1	; (MSDOS -> CLUSSEC)
ClusSave:	resd 1	; (MSDOS -> CLUSSAVE)
DIRSTART:	resd 1	; (MSDOS -> DIRSTART)
DIRSEC:		resd 1	; (MSDOS -> DIRSEC)
pre_read:	resb 1	; (MSDOS -> PREREAD)
ClusSplit:	resb 1	; (MSDOS -> CLUSSPLIT)
CLUSFAC:	resb 1	; (MSDOS -> CLUSFAC)
SECCLUSPOS:	resb 1	; (MSDOS -> SECCLUSPOS)
NXTCLUSNUM:	resd 1	; (MSDOS -> NXTCLUSNUM)
SRCH_CLUSTER:	resd 1	; (MSDOS -> SRCH_CLUSTER)
LASTENT:	resd 1	; (MSDOS -> LASTENT)
; 02/06/2025
WFP_START:	resd 1	; (MSDOS -> WFP_START)
THISCDS:	resd 1	; (MSDOS -> THISCDS)
CURR_DIR_END:	resd 1	; (MSDOS -> CURR_DIR_END)
ATTRIB:		resb 1	; (MSDOS -> ATTRIB)
SATTRIB:	resb 1	; (MSDOS -> SATTRIB)
NAME1: 		resb 12	; (MSDOS -> NAME1)
NoSetDir:	resb 1	; (MSDOS -> NoSetDir)
CREATING:	resb 1	; (MSDOS -> CREATING)
DELALL:		resb 1	; (MSDOS -> DELALL)
VOLID:		resb 1	; (MSDOS -> VOLID)
		resw 1
ENTLAST:	resd 1	; (MSDOS -> ENTLAST)
ENTFREE:	resd 1	; (MSDOS -> ENTFREE)
ENTLAST_PREV:	resd 1	; (PCDOS 7.1 -> ? -LFN search-)
LNE_COUNT:	resb 1	; (PCDOS 7.1 -> ? -LFN entry count-)

; 11/07/2025 - TRDOS 386 v2.0.10
; (ADD_NEW_CLUSTERS)
FATBYT:		resd 1
;FCS_START:	resd 1
;FCS_END:	resd 1	; 12/07/2025
LASTCLUSTER:	resd 1
FREECLUSTER:	resd 1
NEXTCLUSTER:	resd 1
CLUSTERS:	resd 1
CLUSTCOUNT:	resd 1

; 02/06/2025 - TRDOS 386 v2.0.10
; get_direntry parameters
GDE_BINDEX:	resd 1
GDE_INDEX:	resd 1
;GDE_SPC:	resd 1
GDE_SINDEX:	resd 1
GDE_CINDEX:	resd 1
GDE_SKIP:	resd 1
GDE_DRVT:	resd 1
GDE_FCLUST:	resd 1
GDE_CCLUST:	resd 1

; 03/06/2025
; long name search parameters
LFN_Pos:	resd 1
LDIR_Ord:	resb 1
		resb 1
		resw 1

; 02/06/2025 - TRDOS 386 v2.0.10
; Windows/DOS long to short name conv. parms
order_number:	resd 1

; 02/06/2025 - TRDOS 386 v2.0.10
; Singlix FS file name search parameters
;FS_DDT_Buffer:	resd 1
;FS_SectorIndex: ; 01/07/2025
FS_Dir_Index:	resd 1
FS_CurrentDirectory:
		resd 1
; 17/06/2025
FS_Consequtive: resd 1
FS_CurrentSector:
		resd 1
; 18/06/2025
FS_Dir_Buffer:	resd 1
; 19/06/2025
FS_Current_DDT: resd 1

; 25/05/2025 - TRDOS 386 v2.0.10
; Singlix FS file name conversion parameters

FDT_Number:	; 02/06/2025
fdt_number:	resd 1
; 17/06/2025
;f_name_limit:	resd 1
;f_base_start:	resd 1
f_target:	resd 1
f_base_count:	resd 1 ; 1 byte is used
f_ext_start:	resd 1
f_ext_count:	resd 1 ; 1 byte is used
;f_name_count:	resd 1
formal_size:	resd 1
lossy_conversion: ; 28/05/2025
insert_fdtnum:	resb 1
target_name:	resb 13
;temp_name:	resb 65
temp_name:	resb 129
conv_ucase:	resb 1
