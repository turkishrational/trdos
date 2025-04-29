; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - File System Procs : trdosk5s
; ----------------------------------------------------------------------------
; Last Update: 29/04/2025 (Previous: 31/08/2024, v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; DRV_FAT.ASM (21/08/2011)
; ****************************************************************************
; DRV_FAT.ASM (c) 2005-2011 Erdogan TAN [ 07/07/2009 ] Last Update: 21/08/2011

get_next_cluster:
	; 07/08/2022
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 23/03/2016
	; 01/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 05/07/2011
	; 07/07/2009
	; 2005
	; INPUT ->
	;	EAX = Cluster Number (32 bit)
	;	ESI = Logical DOS Drive Parameters Table
	; OUTPUT ->
	;	cf = 0 -> No Error, EAX valid
	;	cf = 1 & EAX = 0 -> End Of Cluster Chain
	;	cf = 1 & EAX > 0 -> Error
	;	ECX = Current/Previous cluster (if CF = 0)
	;	EAX = Next Cluster Number (32 bit)
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

	mov	[FAT_CurrentCluster], eax
check_next_cluster_fat_type:
	sub	edx, edx ; 0
	cmp     byte [esi+LD_FATType], 2
	jb	short get_FAT12_next_cluster
	;ja	get_FAT32_next_cluster
	; 25/07/2022
	jna	short get_FAT16_next_cluster
	jmp	get_FAT32_next_cluster

get_FAT16_next_cluster:
	mov	ebx, 300h ;768
	div	ebx
	; EAX = Count of 3 FAT sectors
	; EDX = Cluster Offset (< 768)
	;shl	dx, 1 ; Multiply by 2
	; 25/07/2022
	shl	edx, 1
	mov	ebx, edx ; Byte Offset
	add	ebx, FAT_Buffer
	mov	dx, 3
	mul	edx
	; EAX = FAT Sector (<= 256)
	; EDX = 0
	mov	cl, [esi+LD_Name]
	;cmp	byte [FAT_BuffValidData], 0
        cmp	[FAT_BuffValidData], dl ; 0
	jna	short load_FAT_sectors0
	cmp	cl, [FAT_BuffDrvName]
        jne	short load_FAT_sectors0
	cmp	eax, [FAT_BuffSector]
        jne	short load_FAT_sectors1
	;movzx	eax, word [ebx]
	mov	ax, [ebx]
	; 01/02/2016
	; DRV_FAT.ASM (21/08/2011) had a FATal bug here !
	; (cmp ah, 0Fh) ! (ax >= FF7h)
	; (how can i do a such mistake!?)
	;cmp	al, 0F7h
	;jb	short loc_pass_gnc_FAT16_eoc_check
	;cmp	ah, 0FFh
	;jb	short loc_pass_gnc_FAT16_eoc_check
	cmp	ax, 0FFF7h
	jb	short loc_pass_gnc_FAT16_eoc_check
	; ax >= FFF7h (cluster 0002h to FFF6h is valid, in use)
	jmp	short loc_pass_gnc_FAT16_eoc_check_xor_eax

get_FAT12_next_cluster:
	mov	ebx, 400h ;1024
	div	ebx
	; EAX = Count of 3 FAT sectors
	; EDX = Cluster Offset (< 1024)
	; 25/07/2022
	;push	ax
	push	eax
	;mov	ax, 3
	mov	al, 3
	;mul	dx    	; Multiply by 3
	mul	edx
	;shr	ax, 1	; Divide by 2
        shr	eax, 1
	;mov	bx, ax 	; Byte Offset
	mov	ebx, eax
	add	ebx, FAT_Buffer
	pop	eax
	;pop	ax
	;mov	dx, 3
	mov	dl, 3
	mul	edx
	; EAX = FAT Sector (<= 12)
	; EDX = 0
	mov	cl, [esi+LD_Name]
	;cmp	byte [FAT_BuffValidData], 0
	cmp	[FAT_BuffValidData], dl ; 0
	jna	short load_FAT_sectors0
	cmp	cl, [FAT_BuffDrvName]
	jne	short load_FAT_sectors0
	cmp	eax, [FAT_BuffSector]
	jne	short load_FAT_sectors1
	mov	eax, [FAT_CurrentCluster]
	;shr	ax, 1
	; 25/07/2022
	shr	eax, 1
	;movzx	eax, word [ebx]
	mov	ax, [ebx]
	jnc	short get_FAT12_nc_even
	;shr	ax, 4
	shr	eax, 4
loc_gnc_fat12_eoc_check:
	;cmp	al, 0F7h
	;jb	short loc_pass_gnc_FAT16_eoc_check
	;cmp	ah, 0Fh
	;jb	short loc_pass_gnc_FAT16_eoc_check
	cmp	ax, 0FF7h
	jb	short loc_pass_gnc_FAT16_eoc_check
	; ax >= FF7h (cluster 0002h to FF6h is valid, in use)

loc_pass_gnc_FAT16_eoc_check_xor_eax:
	xor	eax, eax ; 0
loc_pass_gnc_FAT16_eoc_check:
loc_pass_gnc_FAT32_eoc_check:
	mov	ecx, [FAT_CurrentCluster]
	cmc
	retn

get_FAT12_nc_even:
	and	ah, 0Fh
	jmp	short loc_gnc_fat12_eoc_check

load_FAT_sectors0:
	mov	[FAT_BuffDrvName], cl
load_FAT_sectors1:
	; 25/07/2022
	;sub	edx, edx
	; edx = 0
	mov	[FAT_BuffSector], eax
	mov	ebx, eax
        add     eax, [esi+LD_FATBegin]
	mov	dl, 2
	;cmp	byte [esi+LD_FATType], 2
        cmp	[esi+LD_FATType], dl ; 2
	ja      short load_FAT_sectors3
	movzx	ecx, word [esi+LD_BPB+BPB_FATSz16]
	jmp	short load_FAT_sectors4

	; 07/08/2022
get_FAT32_next_cluster:
	mov	ebx, 180h ;384
	div	ebx
	; EAX = Count of 3 FAT sectors
	; EDX = Cluster Offset (< 384)
	;shl	dx, 2	; Multiply by 4
	; 25/07/2022
	shl	edx, 2
	mov	ebx, edx ; Byte Offset
	add	ebx, FAT_Buffer
	mov	dx, 3
	mul	edx
        ; EAX = FAT Sector (<= 2097152) ; (FFFFFF7h * 4) / 512
	; 	for 32KB cluster size:
	;	EAX <= 1024 = (4GB / 32KB) * 4) / 512
	; EDX = 0
	mov	cl, [esi+LD_Name]
	;cmp	byte [FAT_BuffValidData], 0
	cmp	[FAT_BuffValidData], dl ; 0
	jna	short load_FAT_sectors0
	cmp	cl, [FAT_BuffDrvName]
	jne	short load_FAT_sectors0
	cmp	eax, [FAT_BuffSector] ; 0, 3, 6, 9 ...
	jne	short load_FAT_sectors1
	mov	eax, [ebx]
 	and	eax, 0FFFFFFFh ; 28 bit Cluster
	cmp	eax, 0FFFFFF7h
	jb	short loc_pass_gnc_FAT32_eoc_check
	; eax >= FFFFFF7h (cluster 0002h to FFFFFF6h is valid)
	jmp	short loc_pass_gnc_FAT16_eoc_check_xor_eax

load_FAT_sectors3:
	mov	ecx, [esi+LD_BPB+BPB_FATSz32]
load_FAT_sectors4:
	sub	ecx, ebx ; [FAT_BuffSector]
	; 25/07/2022
	inc	dl
	; edx = 3
        ;cmp	ecx, 3
        cmp	ecx, edx ; 3
	jna     short load_FAT_sectors5
	;mov	ecx, 3
	; 25/07/2022
	mov	ecx, edx ; 3
load_FAT_sectors5:
	mov	ebx, FAT_Buffer
	call	disk_read
	jnc	short load_FAT_sectors_ok
	; 15/10/2016 (15h -> 17)
	; 23/03/2016 (15h)
	mov	eax, 17 ; Drive not ready or read error
	;mov	byte [FAT_BuffValidData], 0
	; 25/07/2022
	mov	byte [FAT_BuffValidData], ah ; 0
	retn
load_FAT_sectors_ok:
	mov	byte [FAT_BuffValidData], 1
	mov	eax, [FAT_CurrentCluster]
        jmp     check_next_cluster_fat_type

load_FAT_root_directory:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 23/10/2016
	; 15/10/2016
	; 07/02/2016
	; 02/02/2016
	; 01/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 21/05/2011
	; 22/08/2009
	;
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table
	; OUTPUT ->
	;	cf = 1 -> Root directory could not be loaded
	;	    EAX > 0 -> Error number
	;	cf = 0 -> EAX = 0
	;	ECX = Directory buffer size in sectors (CL)
	;	EBX = Directory buffer address
	; 	NOTE: DirBuffer_Size is in bytes ! (word)
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

	; NOTE: Only for FAT12 and FAT16 file systems !
	; (FAT32 fs root dir must be loaded as sub directory)

	mov	bl, [esi+LD_Name]
	mov	bh, [esi+LD_FATType]

	;mov	[DirBuff_DRV], bl
	;mov	[DirBuff_FATType], bh
	mov	[DirBuff_DRV], bx

	;cmp	bh, 2
	;ja	short load_FAT32_root_dir0 ; FAT32 root dir

load_FAT_root_dir0: ; 23/10/2016
	movzx	edx, word [esi+LD_BPB+RootDirEnts]

	;or	dx, dx ; 0 for FAT32 file systems
	;jz	short load_FAT32_root_dir0 ; FAT32 root dir
	
	; 25/07/2022
	mov	eax, edx
	cmp	dx, 512 ; Number of Root Dir Entries
	je	short lrd_mov_ecx_32
	;mov	eax, edx ; 25/07/2022
	; 23/10/2016
	mov	ecx, eax
	add	cx, 15 ; round up 
	;shr	cx, 4  ; 16 entries per sector (512/32)
	; 25/07/2022
	shr	ecx, 4
	; ecx = Root directory size in sectors
	;;shl	ax, 5 ; Root directory size in bytes
	; 25/07/2022
	;shl	eax, 5
	;;dec	dx    ; Last entry number of root dir
	;dec	edx
	; cx = Dir Buffer sector count
	jmp	short lrd_check_dir_buffer

lrd_mov_ecx_32:
	;mov	ecx, 32
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 32
	;dec	dx ; 511
	;mov	ax, 32*512 

lrd_check_dir_buffer:
	; 25/07/2022
	dec	edx ; root dir entries - 1
	shl	eax, 5 ; * 32
	;
	sub	ebx, ebx ; 0
	mov	[DirBuff_ValidData], bl ; 0
	mov	[DirBuff_LastEntry], dx
	mov	[DirBuff_Cluster], ebx ; 0
	mov	[DirBuffer_Size], ax

	mov	eax, [esi+LD_ROOTBegin]
read_directory:
	mov	ebx, Directory_Buffer
	push	ecx ; Directory buffer sector count
	push	ebx
	call	disk_read
	pop	ebx
	jc	short load_DirBuff_error

validate_DirBuff_and_return:
	pop	ecx ; Number of loaded sectors
	mov	byte [DirBuff_ValidData], 1
	xor	eax, eax ; 0 = no error
	retn

load_DirBuff_error:
	mov	eax, ecx ; remaining sectors
	pop	ecx ; sector count
	sub	ecx, eax ; Number of loaded sectors
	; 15/10/2016 (15h -> 17)
	;mov	eax, 17 ; DRV NOT READY OR READ ERROR !
	; 25/07/2022
	;sub	eax, eax
	mov	al, 17
	stc
        retn

load_FAT32_root_directory:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/02/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table
	; OUTPUT ->
	;	cf = 1 -> Root directory could not be loaded
	;	    EAX > 0 -> Error number
	;	cf = 0 -> EAX = 0
	;	ECX = Directory buffer size in sectors (CL)
	;	EBX = Directory buffer address
	; 	NOTE: DirBuffer_Size is in bytes ! (word)
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

	mov	bl, [esi+LD_Name]
	mov	bh, [esi+LD_FATType]

	;mov	[DirBuff_DRV], bl
	;mov	[DirBuff_FATType], bh
	mov	[DirBuff_DRV], bx

load_FAT32_root_dir0:
	mov	eax, [esi+LD_BPB+FAT32_RootFClust]
	jmp	short load_FAT_sub_dir0
	
load_FAT_sub_directory:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 01/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 05/07/2011
	; 23/08/2009
	;
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table
	;	EAX = Cluster Number
	; OUTPUT ->
	;	cf = 1 -> Sub directory could not be loaded
	;	    EAX > 0 -> Error number
	;	cf = 0 -> EAX = 0
	;	ECX = Directory buffer size in sectors (CL)
	;	EBX = Directory buffer address
	;
	; 	NOTE: DirBuffer_Size is in bytes ! (word)
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

	mov	bl, [esi+LD_Name]
	mov	bh, [esi+LD_FATType]

	;mov	[DirBuff_DRV], bl
	;mov	[DirBuff_FATType], bh
	mov	[DirBuff_DRV], bx

load_FAT_sub_dir0:
	movzx	ecx, byte [esi+LD_BPB+SecPerClust]

	mov	[DirBuff_ValidData], ch ; 0
	mov	[DirBuff_Cluster], eax

	movzx	eax, word [esi+LD_BPB+BytesPerSec]
	mul	ecx
	shr	eax, 5 ; directory entry count (dir size / 32)
	;dec	ax ; last entry
	; 25/07/2022
	dec	eax
	mov	[DirBuff_LastEntry], ax

	mov	eax, [DirBuff_Cluster]
	sub	eax, 2
	mul	ecx
	add	eax, [esi+LD_DATABegin]
	; ecx = sectors per cluster (dir buffer size <= 128 sectors)
	jmp	short read_directory

; DRV_FS.ASM

; 25/07/2022 (TRDOS 386 Kernel v2.0.5)

load_current_FS_directory:
	;retn
load_FS_root_directory:
	;retn
load_FS_sub_directory:
	retn

read_cluster:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 18/03/2016
	; 16/03/2016
	; 17/02/2016
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; INPUT ->
	;	EAX = Cluster Number (Sector index for SINGLIX FS)
	;	ESI = Logical DOS Drive Description Table address
	;	EBX = Cluster (File R/W) Buffer address (max. 64KB)
	;	Only for SINGLIX FS:
	;	EDX = File Number (The 1st FDT address)
	; OUTPUT ->
	;	cf = 1 -> Cluster can not be loaded at the buffer
	;	    EAX > 0 -> Error number
	;	cf = 0 -> Cluster has been loaded at the buffer
	;
	; (Modified registers: EAX, ECX, EBX, EDX)
	
	movzx	ecx, byte [esi+LD_BPB+BPB_SecPerClust]
	; CL = 1 = [esi+LD_FS_Reserved2] ; SectPerClust for Singlix FS

read_file_sectors: ; 16/03/2016
	;cmp	byte [esi+LD_FATType], 0
	; 25/07/2022
	cmp	[esi+LD_FATType], ch ; 0
	jna	short read_fs_cluster

read_fat_file_sectors: ; 18/03/2016
	sub	eax, 2 ; Beginning cluster number is always 2
	movzx	edx, byte [esi+LD_BPB+BPB_SecPerClust] ; 18/03/2016 
	mul	edx
	add	eax, [esi+LD_DATABegin] ; absolute address of the cluster

	; EAX = Disk sector address
	; ECX = Sector count
	; EBX = Buffer address
	; (EDX = 0)
	; ESI = Logical DOS drive description table address

	call	disk_read
	jnc	short rclust_retn
	
	; 15/10/2016 (15h -> 17)
	mov	eax, 17 ; Drive not ready or read error !
	retn

rclust_retn:
	sub	eax, eax ; 0
	retn

read_fs_cluster:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	; Singlix FS

	; EAX = Cluster number is sector index number of the file (eax)
	
	; EDX = File number is the first File Descriptor Table address
	;	of the file. (Absolute address of the FDT).

	; eax = sector index (0 for the first sector)
	; edx = FDT0 address
		; 64 KB buffer = 128 sectors (limit) 
	;mov	ecx, 128 ; maximum count of sectors (before eof)
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 128
	;call	read_fs_sectors
	;retn
	;jmp	short read_fs_sectors

read_fs_sectors:
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	stc
	retn

get_first_free_cluster:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/03/2016
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 26/10/2010 (DRV_FAT.ASM, 'proc_get_first_free_cluster')
	; 10/07/2010
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table address
	; OUTPUT ->
	;	cf = 1 -> Error code in AL (EAX)
	;	cf = 0 -> 
	;	  EAX = Cluster number
	;	  If EAX = FFFFFFFFh -> no free space
	;	If the drive has FAT32 fs:
	;	  EBX = FAT32 FSI sector buffer address (if > 0)
	;
	; (Modified registers: eax, ebx, ecx, edx)
	;

	mov	eax, [esi+LD_Clusters]
	inc	eax ; add eax, 1
	mov	[gffc_last_free_cluster], eax

	xor	ebx, ebx ; 0 ; 02/03/2016

	cmp	byte [esi+LD_FATType], 2
	jna	short loc_gffc_get_first_fat_free_cluster0

loc_gffc_get_first_fat32_free_cluster:
	; 02/03/2016
	call	get_fat32_fsinfo_sector_parms
	jc	short loc_gffc_get_first_fat_free_cluster0

loc_gffc_check_fsinfo_parms:
	;;mov	ebx, DOSBootSectorBuff
	;cmp	dword [ebx], 41615252h
	;jne	short loc_gffc_fat32_fsinfo_err
	;cmp	dword [ebx+484], 61417272h
	;jne	short loc_gffc_fat32_fsinfo_err
	;mov	eax, [ebx+492] ; FSI_Next_Free
	;EAX = First free cluster
	;(from FAT32 FSInfo sector)
	mov	eax, edx ; FSI_Next_Free (First Free Cluster)
	cmp	eax, 0FFFFFFFFh ; invalid (unknown) !
	jb	short loc_gffc_get_first_fat_free_cluster1

	; Start from the 1st cluster of the FAT(32) file system
loc_gffc_get_first_fat_free_cluster0:
	;mov	eax, 2
	; 25/07/2022
	sub	eax, eax
	mov	al, 2
	;xor	edx, edx

loc_gffc_get_first_fat_free_cluster1:
	push	ebx ; 02/03/2016

loc_gffc_get_first_fat_free_cluster2:
	mov	[gffc_first_free_cluster], eax
	mov	[gffc_next_free_cluster], eax

	; EBX = FAT32 FSINFO sector buffer address
	; (EBX = 0, if the drive has not got FAT32 fs or
	; FAT32 FSINFO sector buffer is invalid.)

loc_gffc_get_first_fat_free_cluster3:
	call	get_next_cluster
	jnc	short loc_gffc_get_first_fat_free_cluster4
	or	eax, eax
	jz	short loc_gffc_first_free_fat_cluster_next
	pop	ebx ; 02/03/2016
	cmc 	; stc
	retn

loc_gffc_get_first_fat_free_cluster4:
	and	eax, eax ; next cluster value
	jnz	short loc_gffc_first_free_fat_cluster_next
	mov	eax, ecx ; current (previous cluster) value
	jmp	short loc_gffc_check_for_set

loc_gffc_first_free_fat_cluster_next:
	mov	eax, [gffc_next_free_cluster]
	cmp	eax, [gffc_last_free_cluster]
	jnb	short retn_stc_from_get_first_free_cluster
pass_gffc_last_cluster_eax_check:
	inc	eax ; add eax, 1
	mov	[gffc_next_free_cluster], eax
	jmp	short loc_gffc_get_first_fat_free_cluster3

retn_stc_from_get_first_free_cluster:
	mov	eax, [gffc_first_free_cluster]
	cmp	eax, 2
	ja	short loc_gffc_check_previous_clusters
	sub	eax, eax
	dec	eax ; FFFFFFFFh

loc_gffc_check_for_set:
	; 02/03/2016
	pop	ebx

	; EBX = FAT32 FSINFO sector buffer address
	; (EBX = 0, if the drive has not got FAT32 fs or
	; FAT32 FSINFO sector buffer is invalid.)

	or	ebx, ebx
	jnz	short loc_gffc_set_ffree_fat32_cluster

	;cmp	byte [esi+LD_FATType], 3
	;jnb	short loc_gffc_set_ffree_fat32_cluster

	;xor	ebx, ebx ; 0

loc_gffc_retn:
	retn

loc_gffc_check_previous_clusters:
	dec	eax ; sub eax, 1
	mov	[gffc_last_free_cluster], eax
	;mov	eax, 2
	; 25/07/2022
	xor	eax, eax
	mov	al, 2
	; eax = 2
	;xor	edx, edx
	jmp	short loc_gffc_get_first_fat_free_cluster2

loc_gffc_set_ffree_fat32_cluster:
	;call	set_first_free_cluster
	;retn
	;jmp	short set_first_free_cluster

set_first_free_cluster:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 23/03/2016
	; 02/03/2016
	; 29/02/2016
	; 26/02/2016
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 21/08/2011 (DRV_FAT.ASM, 'proc_set_first_free_cluster')
	; 11/07/2010
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table address
	;	EAX = First free cluster
	;	EBX = FSINFO sector buffer address
	;  	;;If EBX > 0, it is FSINFO sector buffer address
	;	;;EBX = 0, if FSINFO sector is not loaded
	; OUTPUT->
	;	ESI = Logical DOS Drive Description Table address
	;  	If EBX > 0, it is FSINFO sector buffer address
	;	EBX = 0, if FSINFO sector could not be loaded
	; 	CF = 1 -> Error code in AL (EAX)
	;	CF = 0 -> first free cluster is successfully updated

	;cmp	byte [esi+LD_FATType], 3
	;jb	short loc_sffc_invalid_drive

	; Save First Free Cluster value for 'update_cluster'
	mov	[esi+LD_BPB+BPB_Reserved+4], eax ; First free Cluster

	;or	ebx, ebx
	;jnz	short loc_sffc_read_fsinfo_sector

	cmp     dword [ebx], 41615252h
	jne	short loc_sffc_read_fsinfo_sector
	cmp	dword [ebx+484], 61417272h
	jne	short loc_sffc_read_fsinfo_sector

	cmp	eax, [ebx+492]  ; FSI_Next_Free
	je	short loc_sffc_retn

loc_sffc_write_fsinfo_sector:
	; EBX = FSINFO sector buffer
	; [CFS_FAT32FSINFOSEC] is set in 'get_fat32_fsinfo_sector_parms'
	mov	[ebx+492], eax
	mov	eax, [CFS_FAT32FSINFOSEC]
	;mov	ecx, 1
	; 25/07/2022
	xor	ecx, ecx
	inc	cl
	; ecx = 1
	push	ebx
	call	disk_write
	jc      short loc_sffc_read_fsinfo_sector_err1
	pop	ebx

	mov	eax, [ebx+492] ; First (Next) Free Cluster

loc_sffc_retn:
	retn

;loc_sffc_invalid_drive:
;	mov	eax, 0Fh ; MSDOS Error : Invalid drive
;	push	edx

loc_sffc_read_fsinfo_sector_err1:
	; 25/07/2022
	;mov	ebx, 0
	; 15/10/2016 (1Dh -> 18)
	; 23/03/2016 (1Dh)
	;mov	eax, 18 ; Drive not ready or write error
	xor	eax, eax
	mov	ebx, eax ; 0
	mov	al, 18
	stc
loc_sffc_read_fsinfo_sector_err2:
	pop	edx
	retn
	
loc_sffc_read_fsinfo_sector:
	push	eax

	call	get_fat32_fsinfo_sector_parms
	jc	short loc_sffc_read_fsinfo_sector_err2

	pop	eax
	; EDX = First (Next) Free Cluster value from FSINFO sector
	; EAX = First Free Cluster value from 'get_next_cluster'
	; (edx = old value)
	cmp	eax, edx ; First free Cluster (eax = new value)
	jne	short loc_sffc_write_fsinfo_sector

	retn

update_cluster:
	; 31/08/2024
	; 29/08/2024
	; 26/08/2024
	; 24/08/2024 (TRDOS 386 Kernel v2.0.9)
	; 07/08/2022
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 23/10/2016
	; 23/03/2016
	; 02/03/2016
	; 01/03/2016
	; 29/02/2016
	; 27/02/2016
	; 26/02/2016
	; 22/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 11/08/2011
	; 09/02/2005
	; INPUT ->
	;	EAX = Cluster Number
	;	ECX = New Cluster Value
	;	ESI = Logical Dos Drive Parameters Table
	;
	;	/// dword [FAT_ClusterCounter] ///
	;
	; OUTPUT ->
	;	cf = 0 -> No Error, EAX is valid
	;	cf = 1 & EAX = 0 -> End Of Cluster Chain
	; 	cf = 1 & EAX > 0 -> Error
	;		(ECX -> any value)
	; 	EAX = Next Cluster
	;	ECX = New Cluster Value
	;
	;	/// [FAT_ClusterCounter] is updated,
	;	/// decreased when a free cluster is assigned,
	;	/// increased if an assigned cluster is freed.
	;
	;
	; (Modified registers: EAX, EBX, -ECX-, EDX)
	
	mov	[FAT_CurrentCluster], eax
	mov	[ClusterValue], ecx

loc_update_cluster_check_fat_buffer:
	mov	bl, [esi+LD_Name]
	cmp	[FAT_BuffDrvName], bl
	je	short loc_update_cluster_check_fat_type
	cmp	byte [FAT_BuffValidData], 2
	;je	loc_uc_save_fat_buffer
	; 25/07/2022
	jne	short loc_uc_reset_fat_buffer_validation
	jmp	loc_uc_save_fat_buffer

loc_uc_reset_fat_buffer_validation:
	mov	byte [FAT_BuffValidData], 0

loc_uc_check_fat_type_reset_drvname:
	mov	[FAT_BuffDrvName], bl

loc_update_cluster_check_fat_type:
	sub	edx, edx ; 26/02/2016
	mov	bl, [esi+LD_FATType]
	cmp	eax, 2
        jb	short update_cluster_inv_data

	;;;
	; 24/08/2024
	; edx = 0 ; 24/08/2024
	mov	ecx, [esi+LD_Clusters]
	inc	ecx
	mov	[LastCluster], ecx
	;
	cmp	eax, ecx
	;ja	short return_uc_fat32_stc ; 25/07/2022
	; edx = 0 ; (must be -1 or > 0 after here)
	ja	short return_uc_fat_stc
	;;;

	cmp	bl, 2
        ;ja	update_fat32_cluster
	; 25/07/2022
	jna	short loc_uc_check_fat_type_1
	jmp	update_fat32_cluster

; 24/08/2024
;loc_uc_check_fat_type_1:
;	;cmp	bl, 1
;	;jb	short update_cluster_inv_data
;	mov	ecx, [esi+LD_Clusters]
;	inc	ecx  
;	mov	[LastCluster], ecx
;	cmp	eax, ecx ; dword [LastCluster]
;	;ja	return_uc_fat_stc
;	; 25/07/2022
;	jna	short loc_uc_check_fat_type_2
;	; 24/08/2024
;	; edx = 0 ; (must be -1 or > 0 after here)
;	jmp	return_uc_fat_stc

	; 25/07/2022
update_cluster_inv_data:
	;;mov	eax, 0Dh
	;mov	al, 0Dh  ; Invalid Data
	; 31/08/2024
	mov	al, ERR_INV_DATA ; 29 ; Invalid Data
	retn

	; 24/08/2024
loc_uc_check_fat_type_1:
loc_uc_check_fat_type_2:
	; TRDOS v1 has a FATal bug here !
		; or bl, bl ; cmp bl, 0
		; jz short update_fat12_cluster
	; !! It would destroy FAT12 floppy disk fs here !!
	; ('A:' disks of TRDOS v1 operating system project
	; had 'singlix fs', so, I could not differ this mistake
	; on a drive 'A:')
	cmp	bl, 1 ; correct comparison is this !
	;jna	update_fat12_cluster
	; 25/07/2022
	ja	short update_fat16_cluster
	jmp	update_fat12_cluster

	; 24/08/2024
;return_uc_fat16_stc:
	; 25/07/2022
return_uc_fat_stc:
	; 24/08/2024
	; edx = 0
	dec	edx
	; edx = -1 ; 0FFFFFFFFh
	; ecx > 0
return_uc_fat16_stc:	; 24/08/2024
loc_fat_buffer_stc_0:	; 24/08/2024
	; 01/03/2016
	xor	eax, eax
	stc
	jmp	short loc_fat_buffer_stc_1

update_fat16_cluster:
pass_uc_fat16_errc:
	;sub	edx, edx
	; edx = 0
	mov	ebx, 300h ; 768
	div	ebx
	; EAX = Count of 3 FAT sectors
	; DX = Cluster index in FAT buffer
	;mov	bx, dx  
	; 25/07/2022
	mov	ebx, edx
	;shl	bx, 1 ; Multiply by 2
	; 25/07/2022
	shl	ebx, 1
	mov	dx, 3
	mul	edx
	; EAX = FAT Sector
	; EDX = 0
	; EBX = Byte offset in FAT buffer
	mov	cl, [FAT_BuffValidData]
	cmp	cl, 2
	jne	short loc_uc_check_fat16_buff_sector_load

loc_uc_check_fat16_buff_sector_save:
	cmp	eax, [FAT_BuffSector]
	;jne	short loc_uc_save_fat_buffer
	;jmp	short loc_update_fat16_cell
	; 07/08/2022
	je	short loc_update_fat16_cell
	;jmp	loc_uc_save_fat_buffer

	; 07/08/2022
loc_uc_save_fat_buffer:
	; byte [FAT_BuffValidData] = 2
	call	save_fat_buffer
        jc	short loc_fat_sectors_rw_error2
	;mov	byte [FAT_BuffValidData], 1
	mov	eax, [FAT_CurrentCluster]
	;mov	ecx, [ClusterValue]
	;jmp	short loc_update_cluster_check_fat_buffer
	mov	bl, [esi+LD_Name] ; 01/03/2016
        jmp	loc_uc_reset_fat_buffer_validation

loc_uc_check_fat16_buff_sector_load:
	cmp	cl, 1 ; byte [FAT_BuffValidData]
        jne	short loc_uc_load_fat_sectors
	cmp	eax, [FAT_BuffSector]
        jne	short loc_uc_load_fat_sectors

loc_update_fat16_cell:
loc_update_fat16_buffer:
	add	ebx, FAT_Buffer ; 26/02/2016
	;movzx	eax, word [ebx]
	mov	ax, [ebx]
	; 01/03/2016
	mov	edx, eax ; old value of the cluster
	mov	[FAT_CurrentCluster], eax
	mov	ecx, [ClusterValue] ; 32 bits
	mov	[ebx], cx ; 16 bits !

	mov	byte [FAT_BuffValidData], 2
	
	cmp	ax, 2
	jb	short return_uc_fat16_stc
	cmp	eax, [LastCluster]
	ja	short return_uc_fat16_stc

loc_fat_buffer_updated:
	; 01/03/2016
	clc
loc_fat_buffer_stc_1:
	pushf
	and	ecx, ecx
	jnz	short loc_fat_buffer_updated_1

	; 01/03/2016 
	; new value of the cluster = 0 (free)
	; increase free(d) cluster count
	inc	dword [FAT_ClusterCounter]

loc_fat_buffer_updated_1: ; new value of the cluster > 0
	or	edx, edx ; 02/03/2016
	jnz	short loc_fat_buffer_updated_2
	; old value of the cluster = 0 (it was free cluster)
	; decrease free(d) cluster count
	dec	dword [FAT_ClusterCounter] ; it may be negative number

loc_fat_buffer_updated_2:
	popf
	retn

	; 25/07/2022
loc_fat_sectors_rw_error1:
	;mov	byte [FAT_BuffValidData], 0
	; 23/10/2016 (15h -> 17)
	; 23/03/2016
	mov	eax, 17 ; Drive not ready or read error
	mov	[FAT_BuffValidData], ah ; 0

loc_fat_sectors_rw_error2:
	;mov	eax, error code
	;mov	edx, 0
	mov	ecx, [ClusterValue]
	retn

	; 25/07/2022
loc_uc_load_fat_sectors:
	mov	[FAT_BuffSector], eax

load_uc_fat_sectors_zero:
	add	eax, [esi+LD_FATBegin]
	mov	ebx, FAT_Buffer
	mov	ecx, 3
	call	disk_read
	jc	short loc_fat_sectors_rw_error1

        mov     byte [FAT_BuffValidData], 1
	mov 	eax, [FAT_CurrentCluster]
	mov	ecx, [ClusterValue]
        jmp     loc_update_cluster_check_fat_type

update_fat12_cluster:
pass_uc_fat12_errc:
	;sub	edx, edx
	mov	ebx, 400h ; 1024
	div	ebx
	; EAX = Count of 3 FAT sectors
	; DX = Cluster index in FAT buffer
	;mov	cx, 3
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 3
	; ecx = 3
	;mov	bx, ax
	mov	ebx, eax
	;mov	ax, cx ; 3
	mov	eax, ecx
	;mul	dx     ; Multiply by 3
	mul	edx
	;shr	ax, 1  ; Divide by 2
	shr	eax, 1
	;xchg	bx, ax
	xchg	ebx, eax
	; EAX = Count of 3 FAT sectors
	; EBX = Byte Offset in FAT buffer
	;mul	cx  ; 3 * AX
	mul	ecx ; 3 * EAX
	; EAX = FAT Beginning Sector
	; EDX = 0
	mov	cl, [FAT_BuffValidData]
	; TRDOS v1 has a FATal bug here ! 
	; (it does not have 'cmp cl, 2' instruction here !
	;  while 'jne' is existing !)
	cmp	cl, 2 ; 2 = dirty buffer (must be written to disk)
	jne	short loc_uc_check_fat12_buff_sector_load

loc_uc_check_fat12_buff_sector_save:
	cmp	eax, [FAT_BuffSector]
        ;jne	short loc_uc_save_fat_buffer
	;jmp	short loc_update_fat12_cell
	; 07/08/2022
	je	short loc_update_fat12_cell
	jmp	loc_uc_save_fat_buffer

loc_uc_check_fat12_buff_sector_load:
	cmp	cl, 1 ; byte ptr [FAT_BuffValidData]
        jne     short loc_uc_load_fat_sectors
	cmp	eax, [FAT_BuffSector]
	jne	short loc_uc_load_fat_sectors
	; 07/08/2022
	;je	short loc_update_fat12_cell
	;jmp	loc_uc_load_fat_sectors

loc_update_fat12_cell:
	add	ebx, FAT_Buffer ; 26/02/2016
	;mov	cx, [FAT_CurrentCluster]
	; 25/07/2022
	mov	ecx, [FAT_CurrentCluster]
	;shr	cx, 1
	; 25/07/2022
	shr	ecx, 1
	mov	ax, [ebx]
	;mov	dx, ax
	mov	edx, eax ; 25/07/2022
	; 24/08/2024 (*)
	mov	ecx, [ClusterValue] ; 32 bits
	jnc	short uc_fat12_nc_even

	; 26/08/2024
	; ODD cluster number
	; eax = current value (before updated)
	; ecx = new value of the cluster
 	; of the cluster
	; low 4 bit of al is high 4 bit
	; of the previous cluster
	;	(it must not be overwritten)

	;and	ax, 0Fh
	; 25/07/2022
	and	al, 0Fh
	; 24/08/2024 (*)
	;mov	ecx, [ClusterValue] ; 32 bits
	;shl	cx, 4
	shl	ecx, 4
	;or	cx, ax
	or	cl, al ; 25/07/2022
	;mov	ax, dx
	mov	eax, edx
	mov	[ebx], cx  ; 16 bits !
	;shr	ax, 4 ; al(bit4..7)+ah(bit0..7)
	; 25/07/2022
	shr	eax, 4

update_fat12_buffer:
	;;;
	; 24/08/2024
	mov	ecx, [ClusterValue]
	;;;
	mov	[FAT_CurrentCluster], eax
	mov	edx, eax ; 01/03/2016
	mov	byte [FAT_BuffValidData], 2
	cmp	ax, 2
        jb      short return_uc_fat12_stc
	cmp	eax, [LastCluster]
        ja      short return_uc_fat12_stc
        jmp     loc_fat_buffer_updated

uc_fat12_nc_even:
	; 26/08/2024
	; EVEN cluster number
	; eax = current value (before updated)
	; ecx = new value of the cluster
 	; of the cluster
	; high 4 bit of ah is low 4 bit
	; of the next cluster (it must not be overwritten)

	and	ax, 0F000h
	; 24/08/2024 (*)
	;mov	ecx, [ClusterValue] ; 32 bits
	and	ch, 0Fh
	;or	cx, ax
	; 25/07/2022
	or	ecx, eax
	;mov	ax, dx
	mov	eax, edx
	mov	[ebx], cx ; 16 bits !
	and	ah, 0Fh ; al(bit0..7)+ah(bit0..3)
	jmp	short update_fat12_buffer

update_fat32_cluster:
	; edx = 0 ; 24/08/2024
	;mov	ecx, [esi+LD_Clusters]
	;inc	ecx
	;mov	[LastCluster], ecx
	; 24/08/2024
	; ecx = [LastCluster]

	; 24/08/2024
	;cmp	eax, ecx
	;;ja	short return_uc_fat32_stc ; 25/07/2022
	;; 24/08/2024
	;; edx = 0 ; (must be -1 or > 0 after here)
	;ja	short return_uc_fat_stc

pass_uc_fat32_errc:
	;sub	edx, edx
	mov	ebx, 180h ; 384
	div	ebx
	; EAX = Count of 3 FAT sectors
	; DX = Cluster index in FAT buffer
	mov	ebx, edx
	shl	ebx, 2 ; Multiply by 4
	;mov	edx, 3
	; 25/07/2022
	;xor	dh, dh
	;mov	dl, 3
	mov	dx, 3
	mul	edx
	; EBX = Cluster Offset in FAT buffer
	; EAX = FAT Sector
	; EDX = 0
	mov	cl, [FAT_BuffValidData]
	cmp	cl, 2
	jne	short loc_uc_check_fat32_buff_sector_load

loc_uc_check_fat32_buff_sector_save:
	cmp	eax, [FAT_BuffSector]
	;jne	loc_uc_save_fat_buffer
	;jmp	short loc_update_fat32_cell
	; 25/07/2022
	je	short loc_update_fat32_cell
	jmp	loc_uc_save_fat_buffer

return_uc_fat12_stc:
return_uc_fat32_stc:
	; 24/08/2024
	jmp	loc_fat_buffer_stc_0 ; (*)
	; 25/07/2022
	;sub	eax, eax
	;stc
	;jmp	loc_fat_buffer_stc_1 ; (*)

loc_uc_check_fat32_buff_sector_load:
	cmp	cl, 1 ; byte [FAT_BuffValidData]
	;jne	loc_uc_load_fat_sectors
	; 25/07/2022
	jne	short loc_uc_load_fat_sects
	cmp	eax, [FAT_BuffSector]
	;jne	loc_uc_load_fat_sectors
	; 25/07/2022
	je	short loc_update_fat32_cell
loc_uc_load_fat_sects:
	jmp	loc_uc_load_fat_sectors

loc_update_fat32_cell:
loc_update_fat32_buffer:
	add	ebx, FAT_Buffer ; 26/02/2016
	mov	eax, [ebx]
	and	eax, 0FFFFFFFh ; 28 bit cluster value
	
	mov	edx, [FAT_CurrentCluster] ; 01/03/2016

	mov 	[FAT_CurrentCluster], eax
	mov	ecx, [ClusterValue]
	;;;
	; 29/08/2024
	and	ecx, 0FFFFFFFh ; 28 bit cluster value
	;;; 
	mov	[ebx], ecx ; 29/02/2016

	mov	byte [FAT_BuffValidData], 2

	; 01/03/2016
	and	eax, eax ; was it free cluster ?
	jnz	short loc_upd_fat32_c0

	;or	ecx, ecx ; it will be left free ?!
	;jz	short loc_upd_fat32_c3

	cmp	edx, [esi+LD_BPB+BPB_Reserved+4] ; First free cluster
	jne	short loc_upd_fat32_c3

	cmp	edx, [LastCluster]
	jb	short loc_upd_fat32_c0

	;mov	edx, 2 ; rewind !
	; 25/07/2022
	sub	edx, edx
	mov	dl, 2
	jmp	short loc_upd_fat32_c2

loc_upd_fat32_c0:
	inc	dword [esi+LD_BPB+BPB_Reserved+4] ; set it to next cluster
	jmp	short loc_upd_fat32_c3

loc_upd_fat32_c1:
	or	ecx, ecx ; will it be free cluster ?
	jnz	short loc_upd_fat32_c3

	cmp	edx, [esi+LD_BPB+BPB_Reserved+4] ; First free cluster
	jnb	short loc_upd_fat32_c3

loc_upd_fat32_c2:
	mov	[esi+LD_BPB+BPB_Reserved+4], edx

loc_upd_fat32_c3:
	mov	edx, eax

loc_upd_fat32_c4:
	cmp	eax, 2
	jb	short return_uc_fat32_stc ; 25/07/2022 

pass_uc_fat32_c_zero_check_2:
	cmp	eax, [LastCluster]
	ja	short return_uc_fat32_stc ; 25/07/2022
	
	jmp     loc_fat_buffer_updated

save_fat_buffer:
	; 31/08/2024 (TRDOS 386 v2.0.9)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 01/03/2016
	; 22/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 11/08/2011
	; 09/02/2005
	; INPUT ->
	;	None
	; OUTPUT ->
	;	cf = 0 -> OK.
	;	cf = 1 -> error code in AL (EAX)
	;
	;	EBX = FAT_Buffer address
	;
	; (EAX, EDX, ECX, EBX will be modified)

	;cmp	byte [FAT_BuffValidData], 2
	;je	short loc_save_fat_buff

;loc_save_fat_buffer_retn:
;	xor	eax, eax
;	retn

loc_save_fat_buff:
	xor	edx, edx
	mov	dh, [FAT_BuffDrvName]
	cmp	dh, 'A'
	jb	short loc_save_fat_buffer_inv_data_retn
	sub	dh, 'A'
	push	esi ; *
        mov     esi, Logical_DOSDisks
	add	esi, edx

	mov	dl, [esi+LD_FATType]
	and	dl, dl
	jz	short loc_save_fat_buffer_inv_data_pop_retn

	mov	eax, [FAT_BuffSector]
	cmp	dl, 2
	ja	short loc_save_fat32_buff

loc_save_fat_12_16_buff:
	; 01/03/2016
	; TRDOS v1 has a FATal bug here!
	; Correct code: mov dx, word ptr [FAT_BuffSector]+2
	; (DX:AX in TRDOS v1 -> EAX in TRDOS v2)
	;
	movzx	ecx, word [esi+LD_BPB+FATSecs]
	sub	ecx, eax
	; TRDOS v1 has a bug here... ('pop esi' was forgotten!)
	;jna	short loc_save_fat_buffer_inv_data_retn ; wrong addr!
	jna	short loc_save_fat_buffer_inv_data_pop_retn ; correct addr.
	; 25/07/2022
	;jmp	short loc_save_fat_buffer_check_rs3

loc_save_fat_buffer_check_rs3:
	; 25/07/2022
	sub	ebx, ebx
	mov	bl, 3
	;cmp	ecx, 3
	cmp	ecx, ebx ; 3
	jna	short loc_save_fat_buff_continue
	mov	ecx, ebx ; mov ecx, 3
loc_save_fat_buff_continue:
	mov	ebx, FAT_Buffer
	add	eax, [esi+LD_FATBegin]
	push	ecx
	call	disk_write
	pop	ecx
	jc	short loc_save_FAT_buff_write_err

	cmp	byte [esi+LD_FATType], 2
	jna	short loc_calc_2nd_fat12_16_addr

loc_calc_2nd_fat32_addr:
	mov	eax, [esi+LD_BPB+FAT32_FAT_Size]
	jmp	short loc_calc_2nd_fat_addr

	; 25/07/2022
loc_save_fat32_buff:
	mov	ecx, [esi+LD_BPB+FAT32_FAT_Size]
	sub	ecx, eax
	ja	short loc_save_fat_buffer_check_rs3

loc_save_fat_buffer_inv_data_pop_retn:
	pop	esi ; *
loc_save_fat_buffer_inv_data_retn:
	;mov	eax, 0Dh ; Invalid DATA
	; 25/07/2022
	sub	eax, eax
	;mov	al, 0Dh  ; Invalid Data
	; 31/08/2024
	mov	al, ERR_INV_DATA ; 29 ; Invalid Data
	stc	; cf = 1
	retn

loc_calc_2nd_fat12_16_addr:
	movzx	eax, word [esi+LD_BPB+FATSecs]

loc_calc_2nd_fat_addr:
	add	eax, [esi+LD_FATBegin]
	add	eax, [FAT_BuffSector]
	mov	ebx, FAT_Buffer
	; ecx = 1 to 3
	call	disk_write
	jc	short loc_save_FAT_buff_write_err
 	; Valid buffer (1 = valid but do not save)
	mov	byte [FAT_BuffValidData], 1

loc_save_FAT_buff_write_err:
	pop	esi ; *
	mov	ebx, FAT_Buffer
	; 15/10/2016 (1Dh -> 18)
	; 23/03/2016 (1Dh)
	mov	eax, 18 ; Drive not ready or write error
	retn

calculate_fat_freespace:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 23/03/2016
	; 02/03/2016
	; 01/03/2016
	; 29/02/2016
	; 22/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 30/04/2011
	; 03/04/2010
	; 2005
	; INPUT ->
	;	EAX = Cluster count to be added or subtracted
	; 	If BH = FFh, ESI = TR-DOS Logical Drive Description Table
	; 	If BH < FFh, BH = TR-DOS Logical Drive Number
	; 	BL: 
	;	0 = Calculate, 1 = Add, 2 = Subtract, 3 = Get (Not Set/Calc)
	; OUTPUT ->
	;	EAX = Free Space in sectors
	;	ESI = Logical Dos Drive Description Table address
	;	BH = Logical Dos Drive Number (same with input value of BH)
	;	BL = Type of operation (same with input value of BL)
	;	ECX = 0 -> valid
	;	ECX > 0 -> error or invalid
	;	If EAX = FFFFFFFFh, it is 're-calculation needed'
	;			          sign due to r/w error
	;
	; (Modifed registers: eax, ebx, ecx, edx, esi)
	;

	mov	[CFS_OPType], bx
	mov	[CFS_CC], eax

	cmp	bh, 0FFh
	je	short pass_calculate_freespace_get_drive_dt_offset

loc_calculate_freespace_get_drive_dt_offset:
	xor	eax, eax
        mov     ah, bh
	mov	esi, Logical_DOSDisks
        add     esi, eax

pass_calculate_freespace_get_drive_dt_offset:
	or	bl, bl
	jz	short loc_reset_fcc

loc_get_free_sectors:
	mov	eax, [esi+LD_FreeSectors]

	;xor	ecx, ecx
	;dec	ecx ; 0FFFFFFFFh
	;cmp	eax, ecx ; 29/02/2016
	;je	short loc_get_free_sectors_retn ; recalculation is needed!

	; 23/03/2016
	mov	ecx, [esi+LD_TotalSectors]
	cmp	ecx, eax ; Total sectors must be greater than Free sectors !
	ja	short loc_get_free_sectors_check_optype

	xor	eax, eax
	dec	eax ; 0FFFFFFFFh  ; recalculation is needed!
	mov	[esi+LD_FreeSectors], eax ; reset (for recalculation)

loc_get_free_sectors_retn:
	retn

loc_get_free_sectors_check_optype:
	cmp	bl, 3
	jb	short loc_set_fcc_1 ; 25/07/2022

	sub	ecx, ecx ; 0

	retn

loc_set_fcc_1:
	cmp	byte [esi+LD_FATType], 2
	;ja	loc_update_FAT32_fs_info_fcc
	; 25/07/2022
	jna	short loc_set_fcc_2
	jmp	loc_update_FAT32_fs_info_fcc

loc_set_fcc_2:
	;mov	eax, [esi+LD_FreeSectors]
	movzx	ecx, byte [esi+LD_BPB+SecPerClust]
	sub	edx, edx
	div	ecx
	;or	dx, dx
	;	; DX -> Remain sectors < SecPerClust
	;	; DX > 0 -> invalid free sector count
	;jnz	short loc_reset_fcc

;pass_set_fcc_div32:
	mov	[FreeClusterCount], eax
        jmp     loc_set_free_sectors_FAT12_FAT16

loc_reset_fcc:
	xor	eax, eax
	mov	[FreeClusterCount], eax ; 0
	mov	edx, [esi+LD_Clusters]
	inc	edx
	mov	[LastCluster], edx

	cmp	byte [esi+LD_FATType], 2
	jna	short loc_count_free_fat_clusters_0

	dec	eax ; FFFFFFFFh
	mov	[CFS_FAT32FC], eax

	; 29/02/2016
	mov	[esi+LD_BPB+BPB_Reserved], eax ; reset
	mov	[esi+LD_BPB+BPB_Reserved+4], eax ; reset

	;mov 	eax, 2
	; 25/07/2022
	inc	eax ; eax = 0
	mov	al, 2

loc_count_fc_next_cluster_0:
	push	eax
	call	get_next_cluster
	jnc	short loc_check_fat32_ff_cluster
	or	eax, eax
	jz	short pass_inc_cfs_fcc_0

loc_put_fcc_unknown_sign:
	pop	eax
	; "Free count is Unknown" sign
	;mov	dword [FreeClusterCount], 0FFFFFFFFh

	; 29/02/2016
	; Save Free Cluster Count value in FAT32 'BPB_Reserved' area
	;mov	[esi+LD_BPB+BPB_Reserved], 0FFFFFFFFh ; unknown!
	mov	edx, [CFS_FAT32FC] ; First Free Cluster
	; Save First Free Cluster value in FAT32 'BPB_Reserved+4' area
	mov	[esi+LD_BPB+BPB_Reserved+4], edx

        jmp     loc_put_fcc_invalid_sign

loc_check_fat32_ff_cluster:
	or	eax, eax
	jnz	short pass_inc_cfs_fcc_0
	pop	eax
	mov	[CFS_FAT32FC], eax
	;mov	dword [FreeClusterCount], 1
	inc	dword [FreeClusterCount]
	jmp	short pass_inc_cfs_fcc_1

pass_inc_cfs_fcc_0:
	pop	eax

pass_inc_cfs_fcc_0c:
	inc	eax ; add eax, 1
	cmp	eax, [LastCluster]
	jna 	short loc_count_fc_next_cluster_0
	jmp	short loc_update_FAT32_fs_info_fcc

loc_count_free_fat_clusters_0:
	;mov	eax, 2
	mov	al, 2

loc_count_fc_next_cluster:
	push	eax
	call	get_next_cluster
	jc	short loc_count_fcc_stc

loc_count_free_clusters_1:
	and	eax, eax
	jnz	short pass_inc_cfs_fcc

	inc	dword [FreeClusterCount]
	jmp	short pass_inc_cfs_fcc

loc_count_fcc_stc:
	or	eax, eax
	jnz	short loc_put_fcc_unknown_sign ; 29/02/2016

pass_inc_cfs_fcc:
	pop	eax

pass_inc_cfs_fcc_1:
	inc	eax ; add eax, 1
	cmp	eax, [LastCluster]
	jna	short loc_count_fc_next_cluster

loc_set_free_sectors:
	cmp	byte [esi+LD_FATType], 2
	ja	short loc_update_FAT32_fs_info_fcc

loc_set_free_sectors_FAT12_FAT16:
	cmp	byte [CFS_OPType], 0
	jna	short pass_FAT_add_sub_fcc
	mov	eax, [CFS_CC]
	cmp	byte [CFS_OPType], 1
	ja	short pass_FAT_add_fcc
	add 	[FreeClusterCount], eax
	jmp	short pass_FAT_add_sub_fcc

pass_FAT_add_fcc:
	sub	[FreeClusterCount], eax

pass_FAT_add_sub_fcc:
	movzx	eax, byte [esi+LD_BPB+SecPerClust]
	mov	edx, [FreeClusterCount]
	mul	edx

	xor	ecx, ecx 
	jmp	short loc_cfs_retn_params

loc_put_fcc_invalid_sign:
       	sub	eax, eax ; 0
	dec	eax ; FFFFFFFFh
loc_fat32_ffc_recalc_needed:
	mov	ecx, eax

loc_cfs_retn_params:
	mov 	[esi+LD_FreeSectors], eax
	movzx	ebx, word [CFS_OPType]
	retn

loc_update_FAT32_fs_info_fcc:
loc_check_fcc_FSINFO_op:
	; 29/02/2016
	; EAX = Free cluster count (before this update) ; value from disk
	; EDX = First Free Cluster (before this update) ; value from disk
	cmp	byte [CFS_OPType], 1
	jb	short loc_cfs_FAT32_get_rcalc_parms ; 0 = recalculated
	je	short loc_check_fcc_FSINFO_op1 ; 1 = add
loc_check_fcc_FSINFO_op2: ; subtract
	neg	dword [CFS_CC] ; prepare to subtract ; 2 = sub (add negative)
loc_check_fcc_FSINFO_op1:
	; 01/03/2016
	xor	edx, edx ; 0
	dec	edx ; 0FFFFFFFFh
	mov	eax, [esi+LD_BPB+BPB_Reserved]
	cmp	eax, edx
	jnb	short loc_put_fcc_invalid_sign
        add     eax, [CFS_CC] ; free cluster count on disk + current count
	jc	short loc_put_fcc_invalid_sign

	mov	[FreeClusterCount], eax
	jmp	short loc_cfs_write_FSINFO_sector

loc_cfs_FAT32_get_rcalc_parms:
	mov	edx, [CFS_FAT32FC]
	mov	eax, [FreeClusterCount]
	mov	[esi+LD_BPB+BPB_Reserved+4], edx ; First Free Cluster
loc_cfs_write_FSINFO_sector:
	mov	[esi+LD_BPB+BPB_Reserved], eax ; Free cluster count
	; 01/03/2016
	call	set_fat32_fsinfo_sector_parms
        jc      short loc_put_fcc_invalid_sign

loc_set_FAT32_free_sectors:
	; 29/02/2016
	;mov	eax, [FreeClusterCount]
	;mov	ecx, eax
	;cmp	eax, 0FFFFFFFFh ; Invalid !
	;je	short loc_cfs_retn_params
	;
	mov	ecx, [FreeClusterCount]
	movzx	eax, byte [esi+LD_BPB+SecPerClust]
	mul	ecx
	; 29/02/2016
	xor	ecx, ecx ; 0
	or	edx, edx ; 0 ?
        jnz	short loc_put_fcc_invalid_sign ; 25/07/2022
	cmp	[esi+LD_TotalSectors], eax ; Volume size in sectors
        jna     short loc_put_fcc_invalid_sign
	;
loc_set_FAT32_free_sectors_ok:
	xor	edx, edx ; 0
        jmp     short loc_cfs_retn_params
	;

get_last_cluster:
	; 22/10/2016
	; 27/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 12/06/2010 (DRV_FAT.ASM, 'proc_get_last_custer')
	; 06/06/2010
	; INPUT ->
	;	EAX = First Cluster Number
	; 	ESI = Logical Dos Drive Parameters Table
	; OUTPUT ->
	;	cf = 0 -> No Error, EAX is valid
	;	cf = 1 -> EAX > 0 -> Error
	;	EAX = Last Cluster Number
	;       ECX = Previous Cluster -just before the last cluster-
	;       ; 22/10/2016
	;	[glc_index] = cluster index number of the last cluster	
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

	mov	ecx, eax

	mov	dword [glc_index], 0FFFFFFFFh ; 22/10/2016	

loc_glc_get_next_cluster_1:
	mov	[glc_prevcluster], ecx
 	; 22/10/2016
	inc	dword [glc_index]

loc_glc_get_next_cluster_2:
	call	get_next_cluster
	; ecx = current/previous cluster 
	; eax = next/last cluster
	jnc	short loc_glc_get_next_cluster_1

	or	eax, eax
	jnz	short loc_glc_stc_retn

	; ecx = previous cluster
        mov	eax, ecx

	; previous cluster becomes last cluster (ecx -> eax)
	; previous of previous cluster becomes previous cluster (ecx)

loc_glc_prev_cluster_retn:
	mov	ecx, [glc_prevcluster] 
	retn

loc_glc_stc_retn:
	cmc	;stc
        jmp	short loc_glc_prev_cluster_retn

truncate_cluster_chain:
	; 31/08/2024 - TRDOS 386 v2.0.9
	; 01/03/2016
	; 28/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 22/01/2011 (DRV_FAT.ASM, 'proc_truncate_cluster_chain')
	; 11/09/2010
	; INPUT ->
	;	ESI = Logical dos drive description table address
	;	EAX = First cluster to be truncated/unlinked 
	; OUTPUT ->
	;	ESI = Logical dos drive description table address
	; 	ECX = Count of truncated/removed clusters
	; 	CF = 0 -> EAX = Free sectors
	; 	CF = 1 -> Error code in EAX (AL)

	; NOTE: This procedure does not update lm date&time !
	 
loc_truncate_cc:	
	xor	ecx, ecx ; mov ecx, 0
	;mov	byte [FAT_BuffValidData], 0
	mov	[FAT_ClusterCounter], ecx ; 0 ; reset

	;;;
	; 31/08/2024
	and	eax, eax ; 0
	jz	short loc_tcc_unlink_zero_cluster  ; zero
	;cmp	eax, 0FFFFFF7h
	cmp	eax, 0FFFFFFFh ; 28 bit cluster number limit (EOF)
	jb	short loc_tcc_unlink_clusters

	; (possible FAT32) EOF signature...
	;	not a valid cluster number
	;	
	; NOTE: update_cluster returns EOF (if eax > [LastCluster])
	;	instead of invalid data error

	mov	eax, ERR_INV_DATA  ; invalid cluster number
	stc
loc_tcc_unlink_zero_cluster:	; nothing to do
	retn
	;;;

loc_tcc_unlink_clusters:
	call	update_cluster
	; EAX = Next Cluster
	; ECX = Cluster Value
	; Note:
	; Returns count of unlinked clusters in
	; dword ptr FAT_ClusterCounter
	jnc	short loc_tcc_unlink_clusters

	; error or EOF (end of cluster chain) ; 31/08/2024

pass_tcc_unlink_clusters:
	mov	byte [TCC_FATErr], al
	cmp	byte [FAT_BuffValidData], 2
	jne	short loc_tcc_calculate_FAT_freespace
	call	save_fat_buffer
	jnc	short loc_tcc_calculate_FAT_freespace
	mov	byte [TCC_FATErr], al ; Error
	;mov	byte [FAT_BuffValidData], 0

	; 01/03/2016
	jmp	short loc_tcc_recalculate_FAT_freespace

loc_tcc_calculate_FAT_freespace:
	mov	eax, [FAT_ClusterCounter] ; signed (+-) number
	mov	bx, 0FF01h ; BH = FFh -> ESI = Dos drv desc. table
			   ; BL = 1 -> add cluster(s)
	call	calculate_fat_freespace
	and	ecx, ecx ; cx = 0 -> valid free sector count
	jz	short pass_truncate_cc_recalc_FAT_freespace

loc_tcc_recalculate_FAT_freespace:
	mov	bx, 0FF00h ; recalculate !
	call	calculate_fat_freespace
              
loc_tcc_calculate_FAT_freespace_err:
pass_truncate_cc_recalc_FAT_freespace:
	mov	ecx, [FAT_ClusterCounter]

	cmp	byte [TCC_FATErr], 0
	jna	short loc_tcc_unlink_clusters_retn

loc_tcc_unlink_clusters_error:
	movzx	eax, byte [TCC_FATErr]
	stc
loc_tcc_unlink_clusters_retn:
	retn

set_fat32_fsinfo_sector_parms:
	; 15/10/2016
	; 23/03/2016
	; 29/02/2016 (TRDOS 386 = TRDOS v2.0)
	; INPUT ->
	;	ESI = Logical dos drive description table address
	;	[esi+LD_BPB+BPB_Reserved] = Free Cluster Count
	;	[esi+LD_BPB+BPB_Reserved+4] = First Free Cluster 
	; OUTPUT ->
	;	ESI = Logical dos drive description table address
	; 	CF = 0 -> OK..
	; 	CF = 1 -> Error code in EAX (AL)
	;
	; (Modified registers: EAX, EBX, ECX, EDX)

	call	get_fat32_fsinfo_sector_parms
	jc	short update_fat32_fsinfo_sector_retn

	mov	eax, [esi+LD_BPB+BPB_Reserved] ; Free Cluster Count
	mov	edx, [esi+LD_BPB+BPB_Reserved+4] ; First free Cluster	

        ;mov	ebx, DOSBootSectorBuff
	mov	[ebx+488], eax
	mov	[ebx+492], edx	

	mov	eax, [CFS_FAT32FSINFOSEC]
	mov	ecx, 1
	call	disk_write
	;jnc	short update_fat32_fsinfo_sector_retn

	; 15/10/2016 (1Dh -> 18)
	; 23/03/2016 (1Dh)
	;mov	eax, 18 ; Drive not ready or write error

update_fat32_fsinfo_sector_retn:
	retn

get_fat32_fsinfo_sector_parms:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 23/03/2016
	; 01/03/2016
	; 29/02/2016 (TRDOS 386 = TRDOS v2.0)
	; INPUT ->
	;	ESI = Logical dos drive description table address
	; OUTPUT ->
	;	ESI = Logical dos drive description table address
	;	EBX = FSINFO sector buffer address (DOSBootSectorBuff)	
	;	CF = 0 -> OK..
	;	   EAX = FsInfo sector address
	;	   ECX = Free cluster count
	;	   EDX = First free cluster 	
	;	CF = 1 -> Error code in AL (EAX)
	;	   EBX = 0
	;	
	;	[CFS_FAT32FSINFOSEC] = FAT32 FSINFO sector address
        ;
	; (Modified registers: EAX, EBX, ECX, EDX)

	movzx	eax, word [esi+LD_BPB+FAT32_FSInfoSec]
	add	eax, [esi+LD_StartSector]
	mov	[CFS_FAT32FSINFOSEC], eax
	
        mov     ebx, DOSBootSectorBuff
	;mov	ecx, 1
	; 25/07/2022
	sub	ecx, ecx
	inc	cl
	; ecx = 1
	call	disk_read
	jc	short loc_read_FAT32_fsinfo_sec_err

	mov	ebx, DOSBootSectorBuff

	cmp	dword [ebx], 41615252h
	jne	short loc_read_FAT32_fsinfo_sec_stc

	cmp	dword [ebx+484], 61417272h
	jne	short loc_read_FAT32_fsinfo_sec_stc

	mov	eax, [CFS_FAT32FSINFOSEC]
	mov	ecx, [ebx+488] ; free cluster count
	mov	edx, [ebx+492] ; first (next) free cluster	

	retn

loc_read_FAT32_fsinfo_sec_stc: 
	; 15/10/2016 (0Bh -> 28)
	;mov	eax, 28 ; Invalid format!
	; 25/07/2022
	mov	bl, 28
	jmp	short loc_read_FAT32_fsinfo_sec_stc_retn

loc_read_FAT32_fsinfo_sec_err:
	; 15/10/2016 (15h -> 17)
	; 23/03/2016 (15h)
	;mov	eax, 17 ; Drive not ready or read error
	; 25/07/2022
	mov	bl, 17
loc_read_FAT32_fsinfo_sec_stc_retn:
	; 25/07/2022
	sub	eax, eax
	mov	al, bl ; error code
	; eax = error code
	sub	ebx, ebx ; 0
	stc
	retn

add_new_cluster:
	; 30/08/2024
	; 27/08/2024
	; 25/08/2024 - TRDOS 386 v2.0.9
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 16/05/2016
	; 18/03/2016, 24/03/2016
	; 11/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 30/07/2011 (DRV_FAT.ASM)
	; 11/09/2010
	; INPUT ->
	;	ESI = Logical dos drv desc. table address
	;	EAX = Last cluster
	; OUTPUT ->
	;	ESI = Logical dos drv desc. table address
	;	EAX = New Last cluster (next cluster)
	;	cf = 1 -> error code in EAX (AL)
	;	cf = 1 -> EBX = sectors per cluster
	;	ECX = Free sectors
	;;;	25/07/2022
	;	(EBX = sectors per cluster -not used-)
	;	EDX = 0 (if cf = 0)
	; NOTE:
	; This procedure does not update lm date&time !
	;    ; 30/08/2024	
	; and doesn't update 1st clust and file size fields !
	;
	; (Modified registers: EAX, EBX, ECX, EDX, EDI)

	mov	[FAT_anc_LCluster], eax

	call	get_first_free_cluster
	jc	short loc_add_new_cluster_retn
	; EAX >= 2 and EAX < FFFFFFFFh is valid

	;mov	edx, eax
	;
	;inc	edx
	;;jnz	short loc_add_new_cluster_check_ffc_eax
	;jnz	short loc_add_new_cluster_save_fcc

	; 27/08/2024
	inc	eax ; (*)
	jnz	short loc_add_new_cluster_save_fcc

loc_add_new_cluster_no_disk_space_retn:
	;mov	eax, 27h ; MSDOS err => insufficient disk space
	; 27/08/2024
	; eax = 0
	; 25/07/2022
	;xor	eax, eax
	mov	al, 27h
loc_add_new_cluster_stc_retn:
	stc
loc_add_new_cluster_retn:
	; 25/07/2022
	;movzx	ebx, byte [esi+LD_BPB+SecPerClust]
	mov	ecx, [esi+LD_FreeSectors]
	;xor	edx, edx
	;stc
	retn

loc_anc_invalid_format_stc_retn:
	; 27/08/2024
	;stc
loc_add_new_cluster_invalid_format_retn:
	; 15/10/2016 (0Bh -> 28)
	;mov	eax, 28 ; Invalid format
	;jmp	short loc_add_new_cluster_retn
	; 25/07/2022
	sub	eax, eax
	mov	al, 28
	jmp	short loc_add_new_cluster_stc_retn

;loc_add_new_cluster_check_ffc_eax:
;	cmp	eax, 2
;	jb	short loc_add_new_cluster_invalid_format_retn

loc_add_new_cluster_save_fcc:
	;;;
	; 27/08/2024
	dec	eax ; (*)
	;;;  
	mov	[FAT_anc_FFCluster], eax

; 27/08/2024 (TRDOS 386 v2.0.9)
%if 0
	sub	eax, 2
	movzx   ebx, byte [esi+LD_BPB+SecPerClust]
	mul	ebx
	or	edx, edx
	jnz	short loc_anc_invalid_format_stc_retn

loc_add_new_cluster_allocate_cluster:
	; 18/03/2016
	xchg	edx, eax ; eax = 0
	; 16/05/2016
	;cmp	[ClusterBuffer_Valid], al ; 0
	;jna	short loc_anc_clear_cluster_buffer
	;; 'copy' command, 
	;; writing destination file clust after reading source file clust
	;mov	[ClusterBuffer_Valid], al ; 0 ; reset
	;jmp	short loc_add_new_cluster_write_nc_to_disk

loc_anc_clear_cluster_buffer:
	; 11/03/2016
	; Clear buffer
	mov	edi, Cluster_Buffer ; 70000h (for current TRDOS 386 version)
	mov	ecx, ebx ; sector count
	shl	ecx, 7 ; 1 sector = 512 bytes -> 128 double words
	;xor	eax, eax ; 0
	rep	stosd

loc_add_new_cluster_write_nc_to_disk:
	; 11/03/2016
	;xchg	eax, edx ; edx = 0, eax = sector offset
	mov	eax, edx
        add     eax, [esi+LD_DATABegin]
	jc	short loc_add_new_cluster_invalid_format_retn
		
	mov	ecx, ebx ; ECX = sectors per cluster (<256)
	mov	ebx, Cluster_Buffer
	call	disk_write
	jnc	short loc_add_new_cluster_update_fat_nlc
	
	; 15/10/2016 (1Dh -> 18)
	;mov	eax, 18 ; Write Error
	; 25/07/2022
	xor	eax, eax
	mov	al, 18
	jmp	short loc_add_new_cluster_stc_retn

loc_add_new_cluster_update_fat_nlc:
	mov	eax, [FAT_anc_FFCluster]
%endif
	; 30/08/2024
	; eax = [FAT_anc_FFCluster] ; first free cluster
	xor	ecx, ecx ; 0
	mov	[FAT_ClusterCounter], ecx ; 0 ; reset
	dec	ecx ; -1 ; 0FFFFFFFFh
	test	[FAT_anc_LCluster], ecx	; 0 ?
	jz	short loc_add_new_cluster_update_fat_fc ; yes

	; 27/08/2024
	; eax = (first free) cluster to be added as last cluster
	;;;
	;xor	ecx, ecx

loc_add_new_cluster_update_fat_nlc: ; 30/08/2024
	;mov	[FAT_ClusterCounter], ecx ; 0 ; reset
	;dec	ecx ; 0FFFFFFFFh ; last cluster
	call	update_cluster
	jnc	short loc_add_new_cluster_update_fat_plc
	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	jnz	short loc_add_new_cluster_stc_retn

loc_add_new_cluster_update_fat_plc:
	mov	eax, [FAT_anc_LCluster]
	mov	ecx, [FAT_anc_FFCluster]
loc_add_new_cluster_update_fat_fc: ; 30/08/2024
	call	update_cluster
	jnc	short loc_add_new_cluster_save_fat_buffer

	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	jz	short loc_add_new_cluster_save_fat_buffer

loc_anc_save_fat_buffer_err_retn:
	;cmp	byte [FAT_ClusterCounter], 1
	;jb	short loc_add_new_cluster_retn

	mov	bx, 0FF00h ; recalculate free space (BL = 0)
			   ; (BH = FFh -> Use ESI as Drv Param. Tbl.)
	push	eax
	call	calculate_fat_freespace
	pop	eax
        jmp     loc_add_new_cluster_stc_retn

loc_add_new_cluster_save_fat_buffer:
	;cmp	byte [FAT_BuffValidData], 2
	;jne	short loc_add_new_cluster_calc_FAT_freespace
	;Byte [FAT_BuffValidData] = 2 
	call	save_fat_buffer
	jc	short loc_anc_save_fat_buffer_err_retn

loc_add_new_cluster_calc_FAT_freespace:
	;mov	eax, 1 ; Only one Cluster
	mov	eax, [FAT_ClusterCounter]
	mov	bx, 0FF01h ; BH = FFh -> ESI -> Dos drv desc. table
		; BL = 1 -> add cluster(s)
	;mov	bl, 01h ; 27/08/2024
	; NOTE: EAX value will be added to Free Cluster Count
	; (Free Cluster Count is decreased when EAX value is negative)
        call    calculate_fat_freespace
	; ECX = 0 -> no error, ECX > 0 -> error or invalid return
	and	ecx, ecx ; ECX = 0 -> valid free sector count
	jz	short loc_add_new_cluster_return_cluster_number

loc_add_new_cluster_recalc_FAT_freespace:
	mov	bx, 0FF00h  ; recalculate free space
        call    calculate_fat_freespace
	; cf = 0
loc_add_new_cluster_return_cluster_number:
	mov	ecx, eax ; Free sector count
	mov	eax, [FAT_anc_FFCluster]
	;mov	edi, Cluster_Buffer
	; 25/07/2022 (EBX is not used by callers of this sprocedure)
	;movzx	ebx, byte [esi+LD_BPB+SecPerClust]
	xor	edx, edx ; 0
        retn

write_cluster:
	; 31/08/2024 - TRDOS 386 v2.0.9
	; 15/10/2016
	; 21/03/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; INPUT ->
	;	EAX = Cluster Number (Sector index for SINGLIX FS)
	;	ESI = Logical DOS Drive Description Table address
	;	EBX = Cluster (File R/W) Buffer address (max. 64KB)
	;	Only for SINGLIX FS:
	;	EDX = File Number (The 1st FDT address) 
	; OUTPUT ->
	;	cf = 1 -> Cluster can not be written onto disk
	;	    EAX > 0 -> Error number
	;	cf = 0 -> Cluster has been written successfully
	;
	; (Modified registers: EAX, ECX, EBX, EDX)
	
	movzx	ecx, byte [esi+LD_BPB+BPB_SecPerClust] 
	; CL = 1 = [esi+LD_FS_Reserved2] ; SectPerClust for Singlix FS

write_file_sectors: ; 16/03/2016
	cmp	byte [esi+LD_FATType], 0
	jna	short write_fs_cluster

write_fat_file_sectors:
	; 31/08/2024
	; ecx = sector count (may be different than sectors per cluster)
	sub	eax, 2 ; Beginning cluster number is always 2
	movzx	edx, byte [esi+LD_BPB+BPB_SecPerClust] ; 18/03/2016 
	mul	edx
	add	eax, [esi+LD_DATABegin] ; absolute address of the cluster

	; EAX = Disk sector address
	; ECX = Sector count
	; EBX = Buffer address
	; (EDX = 0)
	; ESI = Logical DOS drive description table address	

	call	disk_write
	jnc	short wclust_retn
	
	; 15/10/2016 (1Dh -> 18)
	mov	eax, 18 ; Drive not ready or write error !
	retn

wclust_retn:
	sub	eax, eax ; 0
	retn

write_fs_cluster:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 21/03/2016 (TRDOS 386 = TRDOS v2.0)
	; Singlix FS
	
	; EAX = Cluster number is sector index number of the file (eax)
	
	; EDX = File number is the first File Descriptor Table address 
	;	of the file. (Absolute address of the FDT).
	
	; eax = sector index (0 for the first sector)
	; edx = FDT0 address
		; 64 KB buffer = 128 sectors (limit) 
	;mov	ecx, 128 ; maximum count of sectors (before eof) 
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 128
	;call	write_fs_sectors
	;retn
	;jmp	short write_fs_sectors

write_fs_sectors:
	; 21/03/2016 (TRDOS 386 = TRDOS v2.0)
	stc
	retn

get_cluster_by_index:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/04/2016 (TRDOS 386 = TRDOS v2.0)
	; INPUT ->
	; 	EAX = Beginning cluster
	; 	EDX = Sector index in disk/file section
	;	      (Only for SINGLIX file system!)
	; 	ECX = Cluster sequence number after the beginning cluster
	; 	ESI = Logical DOS Drive Description Table address
	; OUTPUT ->
	;	EAX = Cluster number 
	;	cf = 1 -> Error code in AL (EAX)
	;
	;(Modified registers: EAX, ECX, EBX, EDX)
	;	
	cmp	byte [esi+LD_FATType], 1
        jb      short get_fs_section_by_index 

	cmp	ecx, [esi+LD_Clusters]
	jb	short gcbi_1
gcbi_0:
	;stc
	;mov	eax, 23h ; Cluster not available ! 
			 ; MSDOS error code: FCB unavailable
	; 25/07/2022
	sub	eax, eax
gcbi_4:
	mov	al, 23h
	stc
	retn
gcbi_1:
	push	ecx
	call	get_next_cluster
	pop	ecx
	jc	short gcbi_3
	loop	gcbi_1
gcbi_2:
	retn
gcbi_3:
	or	eax, eax
	;jz	short gcbi_0
	; 25/07/2022
	jz	short gcbi_4
	cmc 	; stc
	retn

get_fs_section_by_index:
	; 29/04/2016 (TRDOS 386 = TRDOS v2.0)
	; INPUT ->
	; 	EAX = Beginning FDT number/address
	; 	EDX = Sector index in disk/file section
	; 	ECX = Sector sequence number after the beginning FDT
	; 	ESI = Logical DOS Drive Description Table address
	; OUTPUT ->
	; 	EAX = FDT number/address
	; 	EDX = Sector index of the section (0,1,2,3,4...)
	;	cf = 1 -> Error code in AL (EAX)
	;
	;(Modified registers: EAX, ECX, EBX, EDX)
	;
	mov	eax, 0FFFFFFFFh
	retn

get_last_section:
	; 22/10/2016 (TRDOS 386 = TRDOS v2.0)	
	; INPUT ->
	; 	EAX = (The 1st) FDT number/address
	; 	ESI = Logical DOS Drive Description Table address
	; OUTPUT ->
	; 	EAX = FDT number/address of the last section
	; 	EDX = Last sector of the section (0,1,2,3,4...)
	;	[glc_index] = sector index number of the last sector
	;		      (for file, not for the last section)  	
	;		   	
	;	cf = 1 -> Error code in AL (EAX)
	;
	;(Modified registers: EAX, ECX, EBX, EDX)
	;
	mov	eax, 0
	mov	edx, 0
	retn

; --------------------------------------------------------------------

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

update_directory_entry:
	; 27/04/2025
	; 24/04/2025
	; 21/04/2025
	; (MSDOS -> DirFromSFT)
	;
	; INPUT:
	;	EBX = System File Number (MSDOS -> SFT entry number)
	; OUTPUT:
	;	EDI = Directory Entry Location
	;	ESI = Directory Buffer Header address
	;
	; Modified registers:
	;	eax, edx, ecx, esi, edi, ebp
	;

	mov	eax, [ebx+OF_DIRSECTOR] ; (physical sector number)
	xor	edx, edx
	mov	dh, [ebx+OF_DRIVE]	; (logical drive number)
	
	; convert logical drv sector number to physical disk sector number
	;add	eax, [edx+LD_StartSector] ; ! physical address !
	mov	cl, [edx+LD_PhyDrvNo]

	; ref: Retro DOS v5 ibmdos7.s - GETBUFFR ; (MSDOS)
	call	GetBuffer
	jc	short ude_1

	; esi = [CurrentBuffer]
	or	byte [esi+BUFFINFO.buf_flags], buf_isDIR
	
	xor	eax, eax
	mov	al, dir_entry.size ; 32
	mul	byte [ebx+OF_DIRPOS]
	lea	edi, [esi+BUFINSIZ]	; buffer data address
	add	edi, eax

	; 24/04/2025
	; (MSDOS -> LOCAL_CLOSE)
	; ebx = system file number (MSDOS -> SFT number)
	; edx = logical dos drive description table address
	; edi = directory entry offset (in the directory buffer) 	

	push	edx

	call	convert_current_date_time ; (MSDOS -> DATE16)
	; ax = Time in dos dir entry format (HHHHHMMMMMMSSSSS)
	; dx = Date in dos dir entry format (YYYYYYYMMMMDDDDD)
	; modified registers: eax, edx, ecx

	mov	[edi+DirEntry_LastAccDate], dx

	mov	eax, 12	; name size + 1 ; (MSDOS -> SF_ENTRY.sf_name)
	mul	ebx

	pop	edx
	
	lea	esi, [eax+OF_NAME]
	push	edi
	call	MetaCompare	; (MSDOS -> MetaCompare)
	pop	edi
	jz	short ude_2

	mov	eax, ERR_NOT_FOUND ; file not found error !

	stc
ude_1:
	; eax = error code
	retn
ude_2:
	mov	al, [ebx+OF_ATTRIB]
	or	al, attr_archive	; set archive
	mov	[edi+DirEntry_Attr], al

	push	ebx
	
	shl	ebx, 2
	mov	eax, [ebx+OF_FCLUSTER]
	mov	[edi+DirEntry_FstClusLO], ax
	shr	eax, 16
	mov	[edi+DirEntry_FstClusHI], ax		

	mov	eax, [ebx+OF_DATETIME] ; lw = time, hw = date
	mov	[edi+DirEntry_CrtTime], eax ; hw = DirEntry_CrtDate

	mov	eax, [ebx+OF_SIZE]
	mov	[edi+DirEntry_FileSize], eax

	mov	esi, [CurrentBuffer] 

	test	byte [esi+BUFFINFO.buf_flags], buf_dirty
	jnz	short ude_3		; already dirty buffer

	;call	INC_DIRTY_COUNT
	inc	dword [DirtyBufferCount]

	or	byte [esi+BUFFINFO.buf_flags], buf_dirty
ude_3:
	mov	al, [edx+LD_PhyDrvNo]
	; (MSDOS -> FLUSHBUF)
	call	FlushBuffers
	jnc	short ude_4

	pop	ebx
	mov	eax, ERR_DEV_ACCESS ; (MSDOS -> error_access_denied)
	retn
ude_4:
	call	update_fat32_fsinfo

	pop	ebx

	retn

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

GetFatBuffer:
	; 28/04/2025
	; 21/04/2025
	; (MSDOS -> GETBUFFRB) - Ref: Retro DOS v5 - ibmdos7.s
	mov	ch, 1		; fat buffer flag
				; sector must be pre-read
	jmp	short getb_x

GetBuffer_npr:
	; 28/04/2025
	; 21/04/2025
	; (MSDOS -> GETBUFFR) - Ref: Retro DOS v5 - ibmdos7.s
	
	mov	ch, 80h		; bit 7 is 1 = no pre-read
				; not fat buffer (bit 0)
	jmp	short getb_x

GetBuffer:
	; 28/04/2025
	; 24/04/2025
	; 21/04/2025
	; (MSDOS -> GETBUFFR) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;	eax = Physical sector number (LBA)
	;	 cl = Physical Disk Number
	;	edx = Logical DOS Drive Table address
	; OUTPUT:
	;	esi = Directory Buffer Header address
	;	 cl = Physical Disk Number
	;	edx = Logical DOS Drive Table address
	;
	; Modified registers:
	;	eax, ecx, esi, edi, ebp
	;

	xor	ch, ch	; 0	; not fat buffer
				; sector must be pre-read
getb_x:
	mov	[pre_read], ch	; bit 7 is 'no pre-read' flag
				; bit 0 is fat buffer flag

	mov	esi, [LastBuffer] ; is Last Buffer address valid?

	mov	ebp, -1		; 0FFFFFFFFh

	cmp	esi, ebp ; -1
	je	short getb_1	; no

	cmp	eax, [esi+BUFFINFO.buf_sector] ; is same disk sector ?
	jne	short getb_1	; no

	cmp	cl, [esi+BUFFINFO.buf_ID] ; is same (phy) disk number ?
	jne	short getb_1	; no

getb_0:
	mov	[CurrentBuffer], esi
	retn

getb_1:
	; get Q Head
	call	GetCurrentHead	; (MSDOS -> GETCURHEAD)
getb_2:
	cmp	eax, [esi+BUFFINFO.buf_sector]	; (phy) sector num
	jne	short getb_7
	
	cmp	cl, [esi+BUFFINFO.buf_ID]	; (phy) drive num
	jne	short getb_7

getb_3:
	xor 	ebp, ebp ; 0

	mov	al, 1			; default

	test	ch, al ; 1		; FAT sector ?
	jz	short getb_6		; no

	mov	bp, [edx+LD_BPB+BPB_FATSz16] ; FAT size in sectors

	cmp	[edx+LD_FATType], 2	; FAT32 ?
	jna	short getb_4		; no

	test	byte [edx+LD_BPB+BPB_ExtFlags], 80h
	jnz	short getb_5	; bit 7 = 1 means only one FAT is active
getb_4:
	mov	al, [edx+LD_BPB+BPB_NumFATs]
getb_5:
	mov	ebp, [edx+LD_BPB+BPB_FATSz32]
getb_6:
	mov	[esi+BUFFINFO.buf_wrtcnt], al
	mov	[esi+BUFFINFO.buf_wrtcntinc], ebp

	; 28/04/2025
	mov	[esi+BUFFINFO.buf_DPB], edx ; current ldrvt address

	; ref: Retro DOS v5 ibmdos7.s - PLACEBUF ; (MSDOS)
	call	PlaceBuffer

	mov	[LastBuffer], esi
	; 24/04/2025
	clc
	jmp	short getb_0

getb_7:
	cmp	[esi+BUFFINFO.buf_ID], 0FFh ; -1 ; Free buffer ?
	jne	short getb_8		; no

	mov	ebp, esi		; save buffer address
getb_8: 
	;mov	esi, [esi+BUFFINFO.buf_next]
	mov	esi, [esi]
	cmp	esi, [FIRST_BUFF_ADDR]	; back at the front again?
	jne	short getb_2		; no, continue looking

	cmp	ebp, -1		; 0FFFFFFFFh ; invalid (not free buf)
	je	short getb_9
	mov	esi, ebp ; restore free buffer (header offset) address
	; 28/04/2025
	mov	[esi+BUFFINFO.buf_DPB], edx ; current ldrvt address
	jmp	short getb_10
getb_9:
	; The requested sector is not available in the buffers.
	;
	; esi = the first buffer in the Queue.
	;
	; Flush the first buffer & read in the new sector into it.

	; 28/04/2025
	push	ebx ; *
	push	edx ; **
	push	esi ; ***

	mov	ebp, eax ; save disk sector number

	call	BUFWRITE		; write out the dirty buffer
	jc	short getb_11
getb_10:
	mov	ch, [pre_read]	; bit 7 = no pre-read flag
				; bit 0 = fat buffer flag
	test	ch, 80h			; read in new sector ?
	jnz	short getb_13		; no, done

	; 28/04/2025
	lea	ebx, [esi+BUFINSIZ]	; buffer data address
	mov	esi, edx
	
	and	ch, ch			; fat sector ?
	jz	short getb_12		; no

	; input: eax = phy sector, ebx = buffer, esi = ldrv table
	call	FATSECRD
	; modified registers: (eax), ecx, edx
	jc	short getb_11
	mov	ch, buf_isFAT		; set buf_flags
	jmp	short getb_13

getb_11:
	; 28/04/2025
	pop	esi ; ***
	pop	edx ; **
	pop	ebx ; *
	; eax = error code (if CF = 1)
	retn

getb_12:
	; 28/04/2025 - TRDOS 386 v2.0.10
	; read 1 disk sector ('trdosk7.s')
	;   --- mov ecx, 1
	;   --- call disk_read
	; input: eax = phy sector, ebx = buffer, esi = ldrv table 
	call	DREAD
	; modified registers: eax, ebx, ecx, edx
	jc	short getb_11
	mov	ch, 0			; set buf_flags to no type
getb_13:
	; 28/04/2025
	pop	esi ; ***
	pop	edx ; **
	pop	ebx ; *

	mov	eax, ebp ; restore disk sector number

	; eax = disk sector
	; ch = buf_flags
	; esi = buffer address
	; edx = logical dos drive table address
	
	mov	[esi+BUFFINFO.buf_sector], eax
	mov	cl, [edx+LD_PhyDrvNo]	; physical drive number
	mov	[esi+BUFFINFO.buf_ID], cx ; set ID and flags
	mov	ch, [pre_read]	; bit 0 -> fat buffer flag
				; bit 7 -> no pre-read flag
	jmp	short getb_3				

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

GetCurrentHead:
	; 21/04/2025
	; (MSDOS -> GETCURHEAD) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;	none
	; OUTPUT:
	;	esi = the first buffer address in Queue
	;	FIRST_BUFF_ADDR = esi
	;	LASTBUFFER = -1
	;
	; Modified registers:
	;	esi
		
	mov	esi, [BufferQueue]	; pointer to the first buffer
	mov	dword [LastBuffer], -1	; invalidate last buffer
	mov	[FIRST_BUFF_ADDR], esi	; save first buffer address
	retn

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

PlaceBuffer:
	; 21/04/2025
	; (MSDOS -> PLACEBUF) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;	esi = buffer (header) address
	; OUTPUT:
	;	Remove buffer from queue 
	;	and re-insert it in proper place.
	;
	; Modified registers:
	;	eax, ecx, edi

	;mov	eax, [esi+BUFFINFO.buf_next]
	mov	eax, [esi]
	mov	edi, [BufferQueue]

	cmp	eax, edi		; Buf = last?
	je	short pb_ret		; Yes, special case

	cmp	esi, edi		; Buf = first?
	jne	short pb_nf		; not first

	mov	[BufferQueue], eax	; Override
	
	; continue with repositioning
	jmp	short pb_ret
pb_nf:
	; (remove buffer [esi] from chain)
	mov	ecx, [esi+BUFFINFO.buf_prev] ; previous buffer
	;mov	[ecx+BUFFINFO.buf_next], eax ; next buffer
	mov	[ecx], eax
	mov	[eax+BUFFINFO.buf_prev], ecx

	; (locate buffer [esi] again)
	mov	eax, [edi+BUFFINFO.buf_prev]
	;mov	[eax+BUFFINFO.buf_next], esi
	mov	[eax], esi
	mov	[edi+BUFFINFO.buf_prev], esi
	mov	[esi+BUFFINFO.buf_prev], eax
	;mov	[esi+BUFFINFO.buf_next], edi
	mov	[esi], edi
pb_ret:
	cmp	byte [esi+BUFFINFO.buf_ID], -1 ; is a free buffer ?
	jne	short pb_x		; no, skip
	mov	[BufferQueue], esi	; yes, make it LRU.
pb_x:
	retn


; --------------------------------------------------------------------

; 24/04/2025 - TRDOS 386 v2.0.10

MetaCompare:
	; 24/04/2025
	; (MSDOS -> MetaCompare) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;	esi = 11 chars DirEntry format name with possible '?'
	;	edi = 11 chars DirEntry format name, no '?'
	;			(Directory Entry)	
	; OUTPUT:
	;	Zero if match else NZ
	;
	; Modified registers:
	;	ecx, esi, edi

MetaCompare:
	mov	ecx, 11
WildCrd:
	repe	cmpsb
	jz	short MetaRet 	; most of the time we will fail.

	cmp	byte [esi-1],"?"
	je	short WildCrd
MetaRet:
 	retn			; Zero set, Match


; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

UNPACK:
	; 27/04/2025
	; (MSDOS -> UNPACK) - Ref: Retro DOS v5 - ibmdos7.s
	; Unpack FAT entries (get next cluster)
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	; OUTPUT:
	;	eax = Content of FAT for given cluster
	;	      (next/new cluster number)
	;	esi = Start of the buffer
	;	ebx = buffer offset (not used)
	;
	;	Note: if EAX input is 0
	;		EAX = [CL0FATENTRY]
	;	if ZF = 1, EAX output = 0 (Free Cluster)
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	and	eax, eax
	jnz	short up_1

	mov	eax, [CL0FATENTRY]
	or	eax, eax
	retn

up_0:
	mov	dword [edx+LD_FreeSectors], -1 ; invalid free sectors
	mov	eax, ERR_CLUSTER ; 'cluster not available !'
	stc
	retn

up_1:
	mov	ecx, [edx+LD_Clusters] ; Last Cluster number - 1
	inc	ecx	; Last Cluster number
	cmp	eax, ecx
	ja	short up_0  ; (MSDOS -> 'HURTFAT:')

	;call	MAPCLUSTER
	;; if cf = 1 -> eax  = error code
	;; if eax = 0 -> zf = 1
	;retn

	;jmp short MAPCLUSTER

; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

MAPCLUSTER:
	; 27/04/2025
	; (MSDOS -> MAPCLUSTER) - Ref: Retro DOS v5 - ibmdos7.s
	; Buffer a FAT sector
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	; OUTPUT:
	;	eax = Content of FAT for given cluster
	;	      (next/new cluster number)
	;	esi = Start of the buffer
	;	ebx = buffer offset (not used)
	;
	;	If CF = 0 and ZF = 1 (EAX = 0) -> Free Cluster
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	mov	[ClusNum], eax	; save current cluster number

	cmp	byte [edx+LD_FATType], 2
	ja	short mapcl_2	; FAT32
	
	mov	byte [ClusSplit], 0
	mov	ebx, eax

	je	short mapcl_1	; FAT16

	; FAT12
	shr	eax, 1
mapcl_1:
	add	eax, ebx  ; eax = 2 * cluster number (for FAT16)
			  ;     = 1.5 * cluster number (for FAT12)
	jmp	short mapcl_3
mapcl_2:	
	shl	eax, 2
	; eax = 4 * cluster number (for FAT32)
mapcl_3:
	; 512 bytes per sector
	; 9 bit shift to right

	mov	ebx, eax
	shr	eax, 9
	and	ebx, 511 ; 1FFh

	; eax = FAT sector (index) number
	; ebx = cluster number offset in the FAT sector

	add	eax, [edx+LD_FATBegin]
	; eax = physical sector number of the FAT sector
	
	mov	[ClusSec], eax

	mov	cl, [edx+LD_PhyDrvNo]

	; edx = logical dos drive table address
	; ebx = cluster number offset (in the buffer)
	;  cl = physical drive number

	call	GetFatBuffer
	jc	short mapcl_5	; eax = error code

	;mov	esi, [CurrentBuffer]
	lea	edi, [esi+BUFINSIZ]
	add	edi, ebx
	cmp	ebx, 511
	jne	short mapcl_4

	; FAT12, cluster split
	
	mov	al, [edi]	
	inc	byte [ClusSplit]
	mov	[ClusSave], al

	;mov	eax, [esi+BUFFINFO.buf_sector]
	;mov	[ClusSec], eax

	mov	eax, [ClusSec]
	;mov	cl, [edx+LD_PhyDrvNo]
	inc	eax

	call	GetFatBuffer
	jc	short mapcl_5	; eax = error code

	mov	al, [ClusSave]
	mov	ah, [esi+BUFINSIZ]

	jmp	short mapcl_7

mapcl_4:
	mov	eax, [edi]

	cmp	byte [edx+LD_FATType], 2
	ja	short mapcl_6	; FAT32
	jb	short mapcl_7	; FAT12
	; FAT16
	and	eax, 0FFFFh
mapcl_5:
	retn

mapcl_6:
	; FAT32
	and	eax, 0FFFFFFFh ; 28bit
	retn

mapcl_7:
	test	byte [ClusNum], 1 ; odd ? 
	jz	short mapcl_8 ; no, even	

	; FAT12, high 12 bit
	shr	eax, 4
mapcl_8:
	; FAT12
	; low 12 bit
	and	eax, 0FFFh
	retn

; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

SETDIRSRCH:
	; 27/04/2025
	; (MSDOS -> SETDIRSRCH) - Ref: Retro DOS v5 - ibmdos7.s
	; Set up a directory search
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	; OUTPUT:
	;	[DIRSTART] = eax
	;	[CLUSFAC], [CLUSNUM], [SECCLUSPOS], [DIRSEC] set
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	or	eax, eax
	jnz	short sds_fat32

	cmp	byte [edx+LD_FATType], 2
	jna	short SETROOTSRCH2

SETROOTSRCH_FAT32:
	mov	eax, [edx+LD_BPB+FAT32_RootFClust]
	
	mov	ecx, [edx+LD_Clusters]  ; Last Cluster - 1
	inc	ecx		; Last Cluster
	cmp	eax, ecx
	ja	short sds_fat32

	cmp	eax, 2
	jnb	short sds_fat32

sds_error:
	mov	eax, ERR_CLUSTER ; 'cluster not available !'
	stc
	retn

sds_fat32:
	mov	[DIRSTART], eax
	mov	cl, [edx+LD_BPB+BPB_SecPerClust]
	mov	[CLUSFAC], cl
	
	call	UNPACK
	jnc	short sdc_unp_ok
	retn

; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

SETROOTSRCH:
	; 27/04/2025
	; (MSDOS -> SETROOTSRCH) - Ref: Retro DOS v5 - ibmdos7.s
	; Set up a directory search (root directory)
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	; OUTPUT:
	;	[DIRSTART] = eax
	;	[CLUSFAC], [CLUSNUM], [SECCLUSPOS], [DIRSEC] set
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	xor	eax, eax
SETROOTSRCH2:
	cmp	byte [edx+LD_FATType], 2
	ja	short SETROOTSRCH_FAT32

SETROOTSRCH_FAT:
	mov	[DIRSTART], eax ; 0
	mov	[SECCLUSPOS], al ; 0

	inc	eax
	mov	[SRCH_CLUSTER], eax ; 1	
	dec	eax
	dec	eax
	mov	[CLUSNUM], eax ; -1
	
	mov	eax, [edx+LD_DATABegin]
	mov	ebx, [edx+LD_ROOTBegin]

	sub	eax, ebx
	mov	[CLUSFAC], al ; number of root dir sectors

	jmp	short SETROOTSRCH3
	
SETROOTSRCH3:
	mov	[DIRSEC], ebx ; physical sector number of the dir
	clc
	retn

sdc_unp_ok:
	; eax =	Contents of FAT for given cluster (from UNPACK)
	; [CLUSNUM] = directory cluster number (from UNPACK)

	xchg	eax, [CLUSNUM]
	mov	[SRCH_CLUSTER], eax
			; Directory start cluster number
			; for searching/locating (directory entry)
	xor	ebx, ebx
	mov	byte [SECCLUSPOS], bl ; 0

	; eax = cluster number
	; edx = logical dos disk table address
	call	FIGREC
	; eax = physical sector number
	;  cl = physical drive/disk number (not used here)
	;
	; CF = 0	
 
	mov	[DIRSEC], eax
	;clc
	retn

; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

FIGREC:
	; 27/04/2025
	; (MSDOS -> FIGREC) - Ref: Retro DOS v5 - ibmdos7.s
	; Convert cluster number to physical disk sector number
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	;	ebx = Sector position in cluster
	; OUTPUT:
	;	eax = physical sector number
	;	 cl = physical drive/disk number
	;		(needed for GetBuffer procedure)
	;
	; Modified registers:
	;	eax, ecx

	mov	cl, [edx+LD_BPB+BPB_SecPerClust]
	sub	eax, 2
figrec_1:
	shr	cl, 1
	jz	short figrec_2
	shl	eax, 1
	jmp	short figrec_1

figrec_2:
	add	eax, ebx
	add	eax, [edx+LD_DATABegin]
	mov	cl, [edx+LD_PhyDrvNo]
	retn

; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

DIRREAD:
	; 27/04/2025
	; (MSDOS -> DIRREAD) - Ref: Retro DOS v5 - ibmdos7.s
	; Read a directory sector (into [CurrentBuffer]).
	;
	; INPUT:
	;	eax = directory block (index) number
	;		(relative to the first block of directory)
	;	edx = Logical DOS Drive Description Table address
	;	[DIRSEC] = First physical sector of 1st cluster of dir
	;	[CLUSNUM] = Next cluster
	;	[CLUSFAC] = Sectors/Cluster ; not used
	; OUTPUT:
	;	[NXTCLUSNUM] = Next cluster (after the one skipped to)
	;	[SECCLUSPOS] Set
	;	[CURBUF] points to Buffer with dir sector
	;	Carry set if error
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	xor	ecx, ecx

	cmp	[DIRSTART], ecx ; 0
	jnz	short drd_subdir
	
	xchg	eax, ecx
	jmp	short drd_doread2

drd_subdir:
	mov	cl, [edx+LD_BPB+BPB_SecPerClust]
	mov	ebx, ecx
	dec	ebx	; cluster mask
	and	ebx, eax
	; ebx = sector position in the cluster
	
drd_sd_shift:
	shr	cl, 1
	jz	short drd_doread1
	shr	eax, 1
	jmp	short drd_sd_shift

drd_doread1:		
	mov	ecx, eax
drd_doread2:
	; ecx = number of clusters to skip
	; ebx = position in cluster

	mov	[SECCLUSPOS], bl
	mov	eax, [CLUSNUM]
	mov	[NXTCLUSNUM], eax
	jcxz	drd_fcluster

drd_skipcl:
	push	ecx
	call	UNPACK
	pop	ecx
	jc	short drd_retn
	call	IsEOF
	jae	short drd_skipped
	loop	drd_skipcl
drd_skipped:
	mov	[NXTCLUSNUM], eax

	movzx	ebx, byte [SECCLUSPOS]
	mov	eax, [ClusNum]

	call	FIGREC
	; eax = physical sector number
	;  cl = physical drive/disk number	
drd_getbuf:
	call	GetBuffer ; pre-read
	jc	short drd_retn

;set_buf_as_dir:
	;mov	esi, [CurrentBuffer]
	or	byte [esi+BUFFINFO.buf_flags], buf_isDIR
drd_retn:
	retn

drd_fcluster:
	mov	eax, [DIRSEC]
	add	eax, ebx
	mov	cl, [edx+LD_PhyDrvNo]
	jmp	short drd_getbuf

 --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

IsEOF:
	; 27/04/2025
	; (MSDOS -> IsEOF) - Ref: Retro DOS v5 - ibmdos7.s
	; check the cluster value for eof
	;
	; INPUT:
	;	eax = Cluster value
	;	edx = Logical DOS Drive Description Table address
	; OUTPUT:
	;	jae -> eof (jb -> not eof)
	;
	; Modified registers:
	;	none

	cmp	byte [edx+LD_FATType], 2
	ja	short IsEOF_FAT32
	jb	short IsEOF_FAT12

IsEOF_FAT16:
	cmp	eax, 0FFF8h
	retn	

IsEOF_FAT12:
	;cmp	eax, 0FF0h	; media byte: F0h
	;je	short IsEOF_other
	cmp	eax, 0FF8h
;IsEOF_other:
	retn	

IsEOF_FAT32:
	cmp	eax, 0FFFFFF8h
	retn	

 --------------------------------------------------------------------

; 28/04/2025 - TRDOS 386 v2.0.10

FATSECRD:
	; 28/04/2025
	; (MSDOS -> FATSECRD) - Ref: Retro DOS v5 - ibmdos7.s
	; Read a FAT sector
	;
	; INPUT:
	;	EAX = Physical sector number
	;	EBX = Buffer (Transfer) address
	;	ESI = Logical DOS Drive Description Table address
	; OUTPUT:
	;	Calls to disk bios (disk_read)
	;
	;	if cf = 1 -> eax = error code
	;
	; Modified registers:
	;	EAX, ECX, EDX

	xor	ecx, ecx

	mov	cl, [esi+LD_BPB+BPB_NumFATs] ; FAT count

	cmp	byte [esi+LD_FATType], 2
	jna	short fatsecrd_1 ; not FAT32

	test	[esi+LD_BPB+BPB_ExtFlags], 80h
	jz	short fatsecrd_1
	mov	cl, 1 ; only one FAT is active
fatsecrd_1:
	push	eax ; *
	push	ebx ; **
	push	ecx ; ***
	
	;call	DSKREAD ; MSDOS (RetroDOS v5)
	call	DREAD	; TRDOS 386 v2.0.10 (read one sector)
		; mov ecx, 1
		; call disk_read
	; modified registers: eax, ebx, ecx, edx

	pop	ecx ; ***
	pop	ebx ; **
	pop	eax ; *
	jnc	short fatsecrd_2

	loop	fatsecrd_3

	; 28/04/2025
	;mov	edx, esi ; LDRVT address for failed disk read

	mov	eax, ERR_DRV_READ ; 'disk read error !'
	stc
fatsecrd_2:
	retn 

fatsecrd_3:
	cmp	byte [esi+LD_FATType], 2
	jna	short fatsecrd_4 ; not FAT32

	add	eax, [esi+LD_BPB+BPB_FATSz32] ; FAT sectors
	jmp	short fatsecrd_1

fatsecrd_4:
	movzx	edx, word [esi+LD_BPB+BPB_FATSz16] ; FAT sectors
	add	eax, edx
	jmp	short fatsecrd_1

 --------------------------------------------------------------------

; 28/04/2025 - TRDOS 386 v2.0.10

BUFWRITE:
	; 28/04/2025
	; (MSDOS -> BUFWRITE) - Ref: Retro DOS v5 - ibmdos7.s
	; Write out a buffer if dirty
	;
	; INPUT:
	; 	ESI = Buffer header address
	; OUTPUT:
	;	Buffer marked free
	;
	;	if cf = 1 -> eax = error code
	;		  -> edx = LDRVT addr for failed drive
	;
	; Modified registers:
	;	EAX, EBX, ECX, EDX

	xor	ecx, ecx
	mov	cl, 0FFh ; -1
	xchg	ecx, [esi+BUFFINFO.buf_ID]
	cmp	cl, 0FFh
	je	short bufwrt_4 ; Buffer is clean, carry clear
	
	test	ch, buf_dirty
	jz	short bufwrt_4 ; Buffer is clean, carry clear.

	;call	DEC_DIRTY_COUNT
	dec	dword [DirtyBufferCount]
	jns	short bufwrt_1
	inc	dword [DirtyBufferCount] ; reset (must be 0)
bufwrt_1:
	mov	eax, [esi+BUFFINFO.buf_sector]
	;movzx	ecx, [esi+BUFFINFO.buf_wrtcnt] ; write count
	shr	ecx, 16 ; ecx = write count (for FAT buffer)
	xor	edx, edx ; 0
bufwrt_2:
	lea	ebx, [esi+BUFINSIZ]	; Point at buffer

	push	eax
	push	edx
	push	ecx
	push	esi	; buffer header address

	mov	esi, [esi+BUFFINFO.buf_DPB] ; LDRVT address

	; eax = physical disk sector
	; ebx = buffer (transfer) address 
	; esi = logical dos drive description table address

	call 	DWRITE	; TRDOS 286 v2.0.10
		; mov ecx, 1
		; call disk_write
	pop	esi
	pop	ecx ; write count (remain)
	pop	edx
	pop	eax
	jc	short bufwrt_3

	inc	edx	
bufwrt_3:
	add	eax, [esi+BUFFINFO.buf_wrtcntinc]

	loop	bufwrt_2
	
	or	edx, edx
	jnz	short bufwrt_4 ; At least one write succeed

	; 28/04/2025
	; return LDRVT address for failed disk
	mov	edx, [esi+BUFFINFO.buf_DPB]
	
	mov	eax, ERR_DRV_WRITE ; 'disk write error !'
	stc
bufwrt_4:
	retn

 --------------------------------------------------------------------

; 29/04/2025 - TRDOS 386 v2.0.10

FlushBuffers:
	; 29/04/2025
	; (MSDOS -> FLUSHBUF) - Ref: Retro DOS v5 - ibmdos7.s
	; Write out all dirty buffers for disk and flag them as clean
	;
	; INPUT:
	;	AL = Physical drive/disk number
	;	   = -1 for all drives
	;	AH = bit 0 = 0 -> invalidate all buffers
	;	   = bit 0 = 1 -> do not invalidate
	; OUTPUT:
	;	none
	;
	;	if cf = 1 -> edx = LDRVT addr for failed drive
	;
	; Modified registers:
	;	EBX, ECX, EDX, ESI

	; Note: if AL = -1 all of disk buffers will be invalidated
	;	after writing
	;	Otherwise, no_invalidate flag (AH) will be checked 	

	;call	GETCURHEAD ; (MSDOS)
	call	GetCurrentHead

	cmp	al, -1
	je	short scan_buf_queue

	test	ah, 1
	jz	short scan_buf_queue	; invalidate all buffers

	cmp	dword [DirtyBufferCount], 0
	je	short end_scan
	
scan_buf_queue:
	call	CHECKFLUSH
	;jc	short dont_free_the_buf ; already invalidated
	; 29/04/2025
	jc	short flushbuf_err

	cmp	al, -1
	je	short free_the_buf	; invalidate all buffers

	test	ah, 1
	jnz	short dont_free_the_buf ; do not invalidate

	; 29/04/2025
	; invalidate buffers only for the requested physical disk
	cmp	al, [esi+BUFFINFO.buf_ID]
	jne	short dont_free_the_buf	; not same disk/drive

free_the_buf:
	; free the buffer (invalidate) 
	mov	dword [esi+BUFFINFO.buf_ID], 0FFh

dont_free_the_buf:
	mov	esi, [esi]
	;mov	esi, [esi+BUFFINFO.buf_next] ; .buf_link
	cmp	esi, [FIRST_BUFF_ADDR]
	jne	short scan_buf_queue

end_scan:
	retn

	; 29/04/2025
flushbuf_err:
	; set disk (buffer) write error flag (bit 7)
	; edx = logical dos drive table address (from CHECKFLUSH)
	or	[edx+LD_MediaChanged], 80h
	jmp	short dont_free_the_buf ; already invalidated

 --------------------------------------------------------------------

; 29/04/2025 - TRDOS 386 v2.0.10

CHECKFLUSH:
	; 29/04/2025
	; (MSDOS -> CHECKFLUSH) - Ref: Retro DOS v5 - ibmdos7.s
	; Write out a buffer if it is dirty
	;
	; INPUT:
	;	AL = Physical drive/disk number
	;	   = -1 for all drives (do not check drive number)
	;      ESI = Buffer header address
	;
	; OUTPUT:
	;	none
	;
	;	if cf = 1 -> edx = LDRVT addr for failed drive
	;
	; Modified registers:
	;	EBX, ECX, EDX

CHECKFLUSH:
	push	eax
	mov	ah, -1
	cmp	[esi+BUFFINFO.buf_ID], ah ; -1
	je	short chk_flush_2	; Skip free buffer, cf = 0
	cmp	al, ah	; cmp al, -1
	je	short chk_flush_1	; do this buffer

	cmp	al, [esi+BUFFINFO.buf_ID]
	clc				; cf = 0
	jne	short chk_flush_2	; Buffer not for this drive/disk
chk_flush_1:
	test	byte [esi+BUFFINFO.buf_flags], buf_dirty
	jz	short chk_flush_2	; Buffer not dirty, cf = 0

	push	dword [esi+BUFFINFO.buf_ID]
	call	BUFWRITE
	pop	eax
	jc	short chk_flush_2	; Leave buffer marked free (lost).
	
	and	ah, ~buf_dirty		; Buffer is clean, cf = 0
	mov	[esi+BUFFINFO.buf_ID], eax
chk_flush_2:
	pop	eax
	retn