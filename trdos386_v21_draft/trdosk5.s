; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - File System Procs : trdosk5s
; ----------------------------------------------------------------------------
; Last Update: 06/01/2026 (Previous: 31/08/2024, v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; DRV_FAT.ASM (21/08/2011)
; ****************************************************************************
; DRV_FAT.ASM (c) 2005-2011 Erdogan TAN [ 07/07/2009 ] Last Update: 21/08/2011

get_next_cluster:
	; 04/05/2025 (TRDOS 386 Kernel v2.0.10)
	;	    -PCDOS/MSDOS style FAT fs handling-
	;	    ((Ref: RetroDOS v5, 'ibmdos7.s'))
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
	; 	EDX = ESI = Logical DOS Drive Parameters Table
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

; 04/05/2025 - TRDOS 386 v2.0.10
%if 0
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
%else
	; 04/05/2025
	mov	edx, esi
	push	esi
	; edx = Logical DOS Drive Description Table address
	; eax = Cluster Number (28bit for FAT32 fs)
	call	MAPCLUSTER
	; Modified registers: eax, ecx, ebx, esi, edi, ebp
	pop	esi
	mov	ecx, [CLUSNUM] ; current cluster number
	jc	short gnc_@

	; edx = LDRVT address
	; eax = next cluster number
	call	IsEOF
	jc	short gnc_ok ; cf = 1
gnc_eof:
	; end of cluster chain
	xor	eax, eax
	; cf = 0
gnc_ok:
	cmc ; cf = 1 -> cf = 0
	    ; cf = 0 -> cf = 1
gnc_@:
	; if cf = 0 -> 
	;    eax = next cluster number ; eax = 0 -> free
	; if cf = 1 -> eax = error code ; eax = 0 -> eof
	; ecx = current (previous) cluster number

	retn
%endif

load_FAT_root_directory:
	; 13/05/2025
	; 07/05/2025 (TRDOS 386 Kernel v2.0.10)
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
	;	cf = 0 -> ;; EAX = 0
	;	ECX = Directory buffer size in sectors (CL)
	;	; 07/05/2025
	;	EAX = Pysical addr of the 1st root dir sector
	;	EBX = Directory buffer (header) address
	;	; 13/05/2025
	;	[DIRSEC] = Physical sector number = eax
	;		   of the 1st sector of root dir
	;	[CLUSNUM] = root dir cluster number
	;		 (= 0 for FAT fs)
	;	[LASTENT] = last dir entry num in root dir
	;	[CurrentBuffer] = ebx
	;	CL = physical drive number
	;
	; 	;;NOTE: DirBuffer_Size is in bytes ! (word)
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

	; NOTE: Only for FAT12 and FAT16 file systems !
	; (FAT32 fs root dir must be loaded as sub directory)

; 07/05/2025
%if 0
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
%else
	; 07/05/2025 - TRDOS 386 v2.0.10
 	mov	eax, [esi+LD_ROOTBegin]
	mov	[DIRSEC], eax
	; 13/05/2025
	xor	ecx, ecx
	mov	[CLUSNUM], ecx ; 0 ; root directory
	mov	cx, [esi+LD_BPB+RootDirEnts]
	; ecx = 512 or 224 or 112
	dec	ecx
	mov	[LASTENT], ecx ; last entry in root directory

load_FAT_root_directory_ns:
	; eax = root directory sector
load_FAT32_root_directory_ns: ; ecx = next cluster
	; 13/05/2025
	; 08/05/2025
	mov	cl, [esi+LD_PhyDrvNo]
	mov	edx, esi
	mov	ebx, eax
	push	esi
	push	edi
	push	ebp
	call	GETBUFFER
	pop	ebp
	pop	edi
	pop	esi
	jc	short load_fat_root_dir_err
			; cf = 1 ; eax = error code
	; cf = 0
	; [CurrentBuffer] = allocated directory buffer addr

	; 08/05/2025
	; ecx = root directory sectors (FAT)
	; ecx = next cluster number (FAT32)

	mov	eax, ebx ; (physical disk sector)
			 ; eax = [esi+LD_ROOTBegin]
	mov	ebx, [CurrentBuffer]

	or	byte [ebx+BUFFINFO.buf_flags], buf_isDIR
load_fat_root_dir_err:
	retn
%endif

load_FAT32_root_directory:
	; 13/05/2025
	; 08/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/02/2016 (TRDOS 386 = TRDOS v2.0)
	;
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table
	; OUTPUT ->
	;	cf = 1 -> Root directory could not be loaded
	;	    EAX > 0 -> Error number
	;	;;cf = 0 -> EAX = 0
	;	;;ECX = Directory buffer size in sectors (CL)
	;	;EBX = Directory buffer address
	;
	;	; 08/05/2025
	;	EAX = Physical sector number (dir sector)
	;	EBX = Directory buffer (header) address
	;	ESI = Logical dos drive desc. table (LDRVT)
	;	; 13/05/2025
	;	[DIRSEC] = Physical sector number = eax
	;		   of the 1st sector of the directory
	;	[CLUSNUM] = (input) cluster number
	;	[LASTENT] = last dir entry num in the cluster
	;	[CurrentBuffer] = ebx
	;	CL = physical drive number
	;
	; 	;NOTE: DirBuffer_Size is in bytes ! (word)
	;
	; (Modified registers: EAX, ECX, EBX, EDX) ; 08/05/2025

; 08/05/2025
%if 0
	mov	bl, [esi+LD_Name]
	mov	bh, [esi+LD_FATType]

	;mov	[DirBuff_DRV], bl
	;mov	[DirBuff_FATType], bh
	mov	[DirBuff_DRV], bx
%endif

load_FAT32_root_dir0:
	mov	eax, [esi+LD_BPB+FAT32_RootFClust]

	; 08/05/2025 - - TRDOS 386 v2.0.10
	;jmp	short load_FAT_sub_dir0

load_FAT32_sub_directory:

load_FAT_sub_directory:
	; 08/06/2025
	; 13/05/2025
	; 08/05/2025 (TRDOS 386 Kernel v2.0.10)
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
	;	;;cf = 0 -> EAX = 0
	;	;;ECX = Directory buffer size in sectors (CL)
	;	;EBX = Directory buffer address
	;
	;	; 08/05/2025
	;	EAX = Physical sector number (dir sector)
	;	EBX = Directory buffer (header) address
	;	ESI = Logical dos drive desc. table (LDRVT)
	;	; 13/05/2025
	;	[DIRSEC] = Physical sector number = eax
	;		   of the 1st sector of the directory
	;	[CLUSNUM] = (input) cluster number
	;	[LASTENT] = last dir entry num in the cluster
	;	[CurrentBuffer] = ebx
	;	CL = physical drive number
	;
	; 	;NOTE: DirBuffer_Size is in bytes ! (word)
	;
	; (Modified registers: EAX, ECX, EBX, EDX) ; 08/05/2025

; 08/05/2025
%if 0
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

%else
	; 13/05/2025
 	mov	ecx, [esi+LD_Clusters]  ; Last Cluster - 1
	inc	ecx		; Last Cluster
	cmp	ecx, eax
	jb	short cna_error

	cmp	eax, 2
	jnb	short load_fat_subdir_@

cna_error:
	mov	eax, ERR_CLUSTER ; 'cluster not available !'
	;stc
	retn

load_fat_subdir_@:
	mov	[CLUSNUM], eax

	push	eax
	movzx	ecx, byte [esi+LD_BPB+SecPerClust]
	movzx	eax, word [esi+LD_BPB+BytesPerSec]
	mul	ecx
	shr	eax, 5 ; directory entry count (dir size / 32)
	dec	eax
	mov	[LASTENT], eax ; last entry in cluster
	pop	eax

	push	ebp
	push	esi
	push	edi

	; 13/05/2025
	mov	edx, esi
	; 08/06/2025
	;xor	ebx, ebx ; 0

	; edx = Logical DOS Drive Description Table address
	; eax = Cluster Number (28bit for FAT32 fs)
	;; ebx = Sector position in cluster

	call	FIGREC

	mov	[DIRSEC], eax

	; eax = physical sector number
	;  cl = physical drive/disk number

	call	GETBUFFER ; pre-read
	jc	short load_subdir_err ; eax = error code
	; modified regs: eax, ecx, esi, edi, ebp
	
	; esi = Directory Buffer Header address
	;  cl = Physical Disk Number
	; edx = Logical DOS Drive Table address
	; [CurrentBuffer] = esi
	; eax = directory sector

	or	byte [esi+BUFFINFO.buf_flags], buf_isDIR

	lea	ebx, [esi+BUFINSIZ]
	mov	eax, [DIRSEC] ; physical sector number
	
load_subdir_err:
	pop	edi
	pop	esi
	pop	ebp
	retn
%endif

; DRV_FS.ASM

; 25/07/2022 (TRDOS 386 Kernel v2.0.5)

load_current_FS_directory:
	;retn
load_FS_root_directory:
	;retn
load_FS_sub_directory:
	retn

; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif

	; 07/12/2025 - TRDOS 386 Kernel v2.0.10
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
	;	ECX = Sector count (<= sectors per cluster)
	; OUTPUT ->
	;	cf = 1 -> Cluster can not be loaded at the buffer
	;	    EAX > 0 -> Error number
	;	cf = 0 -> Cluster has been loaded at the buffer
	;
	; (Modified registers: EAX, ECX, EBX, EDX)

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

; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif

	;;;;
	; 08/07/2025 - TRDOS 386 v2.0.10
get_first_free_cluster_@:
	; set start cluster number
	; for 'get_first_free_cluster'
	; INPUT:
	;   ESI = Logical DOS Drv Description Table addr
	; OUTPUT:
	;   get_first_free_cluster output

	cmp	byte [esi+LD_FATType], 2
	jna	short get_fat_first_free_cluster ; FAT fs

get_fat32_first_free_cluster:
	; FAT32 fs
	mov	eax, [esi+LD_BPB+FAT32_FirstFreeClust]
	jmp	short gfatffc_1

get_fat_first_free_cluster:
	; FAT16 or FAT12 fs
	mov	eax, [esi+LD_BPB+FAT_FirstFreeClust]
gfatffc_1:
	or	eax, eax
	jz	short gfatffc_2

	cmp	eax, -1 ; invalid
	jb	short get_first_free_cluster ; valid

	sub	eax, eax
gfatffc_2:
	; reset to the default
	mov	al, 2
	;;;;

get_next_free_cluster:	; 04/05/2025
get_first_free_cluster:
	; 04/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/03/2016
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 26/10/2010 (DRV_FAT.ASM, 'proc_get_first_free_cluster')
	; 10/07/2010
	; INPUT ->
	;	ESI = Logical DOS Drive Description Table address
	;	EAX = start cluster number ; 04/05/2025
	; OUTPUT ->
	;	cf = 1 -> Error code in AL (EAX)
	;	cf = 0 ->
	;	  EAX = Cluster number
	;	  If EAX = FFFFFFFFh -> no free space
	;	;;If the drive has FAT32 fs:
	;	;;   EBX = FAT32 FSI sector buffer address (if > 0)
	;
	; (Modified registers: eax, ebx, ecx, edx)
	;

	; 04/05/2025
	mov	ebx, [esi+LD_Clusters]
	inc	ebx ; add ebx, 1
	mov	[gffc_last_free_cluster], ebx

; 04/05/2025 - TRDOS 386 v2.0.10
%if 0
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

%else
	; 04/05/2025
	; eax = start cluster number
	; ebx = last cluster number

	cmp	eax, ebx ; [gffc_last_free_cluster]
	jna	short gffc_1

gffc_0:
	mov	eax, 2 ; 1st cluster
gffc_1:
	mov	[gffc_first_free_cluster], eax

gffc_2:
	; eax = current (next) cluster number
	call	get_next_cluster
	jnc	short gffc_3

	or	eax, eax
	jz	short gffc_7 ; eof

	; error
	cmc 	; stc
	retn
gffc_3:
	and	eax, eax ; next cluster value
	mov	eax, ecx
	jnz	short gffc_4

; 08/07/2025
%if 0
	;;;;
	; 08/07/2025
	; update First Free Cluster field of the LDRVT
	; for fast search
	cmp	byte [esi+LD_FATType], 2
	jna	short gffc_8 ; FAT fs
	; FAT32 fs
	mov	[esi+LD_BPB+FAT32_FirstFreeClust], eax
	retn
gffc_8:
	; FAT16 or FAT12 fs
	mov	[esi+LD_BPB+FAT_FirstFreeClust], eax
	;;;;
%endif
	; eax = first free cluster
	retn
gffc_4:
	cmp	eax, [gffc_last_free_cluster]
	jnb	short gffc_5

	inc	eax ; add eax, 1
	jmp	short gffc_2
gffc_5:
	mov	eax, [gffc_first_free_cluster]
	cmp	eax, 2
	ja	short gffc_6
	sub	eax, eax
	dec	eax ; FFFFFFFFh ; -1
	retn
gffc_6:
	; scan previous FAT part (2 to start-1)
	mov	eax, [gffc_first_free_cluster]
	dec	eax
	mov	[gffc_last_free_cluster], eax
	jmp	short gffc_0
gffc_7:
	mov	eax, ecx
	jmp	short gffc_4

%endif

; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif

; 03/01/2026
; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif

; 03/01/2026
; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif

; 03/01/2026
; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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
	;mov	[esi+LD_BPB+BPB_Reserved], eax ; reset
	;mov	[esi+LD_BPB+BPB_Reserved+4], eax ; reset
	; 09/05/2025
	mov	[esi+LD_BPB+FAT_FreeClusters], eax ; reset
	mov	[esi+LD_BPB+FAT_FirstFreeClust], eax ; reset

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

%endif

; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 1

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

; 21/07/2025 - TRDOS 386 v2.0.10
%if 0
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
%else
	; 21/07/2025 - temporary !
truncate_cluster_chain:
	mov	eax, ERR_MISC ; 27
	stc
	retn
%endif
%endif ; 07/12/2025

; 03/01/2026
; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif

; 01/01/2026
; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif ; 07/12/2025

; 07/12/2025 - TRDOS 3865 v2.0.10 (v2.1)
%if 0

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

%endif ; 07/12/2025

get_cluster_by_index:
	; 01/07/2025 (TRDOS 386 Kernel v2.0.10)
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
	;	    = File Sector Number (Singlix File System)
	;	cf = 1 -> Error code in AL (EAX)
	;
	;(Modified registers: EAX, ECX, EBX, EDX)
	;
	cmp	byte [esi+LD_FATType], 1
        ;jb	short get_fs_section_by_index
	; 01/07/2025
	jb      short get_fs_sector_by_index

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

get_fs_sector_by_index:
	; 01/07/2025 - TRDOS 386 v2.0.10
	; Get Singlix FS File Sector By Index
	; (section = extent)
	; 'get_fs_section_by_index'
	; 29/04/2016 (TRDOS 386 = TRDOS v2.0)
	; INPUT ->
	; 	EAX = Beginning FDT number/address
	; 	ECX = Sector sequence number after the beginning FDT
	; 	ESI = Logical DOS Drive Description Table address
	; OUTPUT ->
	; 	EAX = sector number (not physical)
	; 	EDX = ESI = LDRVT address
	;       EBX = Sector sequence/index number (input)
	;	ECX = Remaining sectors in the section
	;
	;	cf = 1 -> Error code in AL (EAX)
	;
	; Modified registers: EAX, ECX, EBX, EDX

	; 01/07/2025
	push	ebp
	push	edi
	push	esi

	;mov	[FS_SectorIndex], ecx
	mov	ebx, ecx
	mov	[FDT_Number], eax
	mov	edx, esi
	add	eax, [esi+LD_FS_BeginSector]
	mov	cl, [edx+LD_FS_PhyDrvNo]
		; physical (rombios) drive number
	call	GETBUFFER
	jc	short get_fs_sbi_6

	add	esi, BUFINSIZ
	;mov	ebx, [FS_SectorIndex]

	; ebx = sector sequence/index number
	; edx = LDRVT address
	; esi = FDT buffer address

	call	get_fs_sector

	; esi = FDT buffer address
	; ebx = sector index number
	; edx = LDRVT address
	; EAX = sector address (!not physical!)
	; ecx = remaining sectors in the extent
	;	(remaining consecutive sectors)

get_fs_sbi_6:
	pop	esi
	pop	edi
	pop	ebp
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
	; 06/01/2026
	; 02/01/2026
	; 03/05/2025
	; 29/04/2025
	; 27/04/2025
	; 24/04/2025
	; 21/04/2025
	; (MSDOS -> DirFromSFT)
	;
	; INPUT:
	;	EBX = System File Number (MSDOS -> SFT entry number)
	; OUTPUT:
	;  (if cf = 0)
	;	EDI = Directory Entry Location
	;	ESI = LDRVT Address ; 06/01/2026
	;
	; Modified registers:
	;	eax, edx, ecx, esi, edi, ebp
	;

	; 02/01/2026
	shl	ebx, 2

	mov	eax, [ebx+OF_DIRSECTOR] ; (physical sector number)
	; 02/01/2026
	shr	ebx, 2
	xor	edx, edx
	mov	dh, [ebx+OF_DRIVE]	; (logical drive number)
	add	edx, Logical_DOSDisks

	; convert logical drv sector num to physical disk sector num
	;add	eax, [edx+LD_StartSector] ; ! physical address !
	mov	cl, [edx+LD_PhyDrvNo]

	; ref: Retro DOS v5 ibmdos7.s - GETBUFFR ; (MSDOS)
	call	GETBUFFER
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
	call	METACOMPARE	; (MSDOS -> MetaCompare)
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

	;mov	[edi+DirEntry_CrtTime], eax ; hw = DirEntry_CrtDate
	; 02/01/2026
	; set last modification date & time
	mov	[edi+DirEntry_WrtTime], eax ; hw = DirEntry_WrtDate

	mov	eax, [ebx+OF_SIZE]
	mov	[edi+DirEntry_FileSize], eax

	mov	esi, [CurrentBuffer]

	test	byte [esi+BUFFINFO.buf_flags], buf_dirty
	jnz	short ude_3		; already dirty buffer

	;call	INC_DIRTY_COUNT
	inc	dword [DirtyBufferCount]

	or	byte [esi+BUFFINFO.buf_flags], buf_dirty
ude_3:
	; 03/05/2025
	push	edx ; LDRVT address

	mov	al, [edx+LD_PhyDrvNo]
	; 29/04/2025
	mov	ah, 1	; bit 0 = 1 -> do not invalidate buffers
	; (MSDOS -> FLUSHBUF)
	call	FLUSHBUFFERS
	; 29/04/2025
	;jnc	short ude_4
	;
	; 29/04/2025
	; Note:
	; (FAT file system buffer flush never returns with error)
	; If there is a buffer write error, it is marked on the LDRVT.
	; LD_MediaChanged bit 7 (buffer write error flag) will be set.
	;
	;pop	ebx
	;mov	eax, ERR_DEV_ACCESS ; (MSDOS -> error_access_denied)
	;retn
ude_4:
	; 03/05/2025
	pop	esi ; LDRVT address

	call	update_fat32_fsinfo

	pop	ebx

	retn

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

GETFATBUFFER:
	; 01/01/2026
	; 09/11/2025
	; 19/10/2025
	; 22/07/2025
	; 05/06/2025
	; 28/04/2025
	; 21/04/2025
	; (MSDOS -> GETBUFFRB) - Ref: Retro DOS v5 - ibmdos7.s
	mov	ch, 1		; fat buffer flag
				; sector must be pre-read
	jmp	short getb_x

GETBUFFER_NPR:
	; 01/01/2026
	; 09/11/2025
	; 19/10/2025
	; 22/07/2025
	; 05/06/2025
	; 28/04/2025
	; 21/04/2025
	; (MSDOS -> GETBUFFR) - Ref: Retro DOS v5 - ibmdos7.s

	mov	ch, 80h		; bit 7 is 1 = no pre-read
				; not fat buffer (bit 0)
	jmp	short getb_x

GETBUFFER:
	; 01/01/2026
	; 09/11/2025
	; 19/10/2025
	; 22/07/2025
	; 05/06/2025
	; 28/04/2025
	; 24/04/2025
	; 21/04/2025
	; (MSDOS -> GETBUFFR) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;	eax = Physical sector number (LBA)
	;	 cl = Physical Disk Number
	;	edx = Logical DOS Drive Table address
	; OUTPUT: ; 22/07/2025
	;	esi = Buffer Header address
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
	call	GETCURRENTHEAD	; (MSDOS -> GETCURHEAD)
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

	; 19/10/2025
	cmp	byte [edx+LD_FATType], 2 ; FAT32 ?
	ja	short getb_4		; yes

	; FAT12 or FAT16
	mov	bp, [edx+LD_BPB+BPB_FATSz16] ; FAT size in sectors
	jmp	short getb_5
getb_4:
	mov	ebp, [edx+LD_BPB+BPB_FATSz32]

	test	byte [edx+LD_BPB+BPB_ExtFlags], 80h
	jnz	short getb_6	; bit 7 = 1 means only one FAT is active
getb_5:
	mov	al, [edx+LD_BPB+BPB_NumFATs]
getb_6:
	mov	[esi+BUFFINFO.buf_wrtcnt], al
	mov	[esi+BUFFINFO.buf_wrtcntinc], ebp

	; 28/04/2025
	mov	[esi+BUFFINFO.buf_DPB], edx ; current ldrvt address

	; ref: Retro DOS v5 ibmdos7.s - PLACEBUF ; (MSDOS)
	call	PLACEBUFFER

	mov	[LastBuffer], esi
	; 24/04/2025
	clc
	jmp	short getb_0

getb_7:
	; eax = physical sector number
	cmp	byte [esi+BUFFINFO.buf_ID], 0FFh ; -1 ; Free buffer ?
	jne	short getb_8		; no

	mov	ebp, esi		; save buffer address
getb_8: 
	;mov	esi, [esi+BUFFINFO.buf_next]
	mov	esi, [esi]
	cmp	esi, [FIRST_BUFF_ADDR]	; back at the front again?
	jne	short getb_2		; no, continue looking

	; 05/06/2025
	; 28/04/2025
	push	ebx ; *
	push	edx ; **

	cmp	ebp, -1		; 0FFFFFFFFh ; invalid (not free buf)
	je	short getb_9

	mov	esi, ebp ; restore free buffer (header offset) address
	; 09/11/2025
	; 28/04/2025
	;mov	[esi+BUFFINFO.buf_DPB], edx ; current ldrvt address
	jmp	short getb_10

getb_9:
	; The requested sector is not available in the buffers.
	;
	; esi = the first buffer in the Queue.
	;
	; Flush the first buffer & read in the new sector into it.

	mov	ebp, eax	; save sector number

	call	BUFWRITE	; write out the dirty buffer
	;jc	short getb_11
	; 05/06/2025
	jc	short getb_12	; eax = ERR_DRV_WRITE

	mov	eax, ebp	; restore sector number
getb_10:
	; 05/06/2025
	push	esi ; ***

	; eax = physical sector number
	mov	ch, [pre_read]	; bit 7 = no pre-read flag
				; bit 0 = fat buffer flag
	test	ch, 80h			; read in new sector ?
	;jnz	short getb_14		; no, done
	; 05/06/2025
	jnz	short getb_15

	; 28/04/2025
	lea	ebx, [esi+BUFINSIZ]	; buffer data address
	mov	esi, edx

	; 05/06/2025
	push	eax ; **** ; save disk sector number

	and	ch, ch			; fat sector ?
	jz	short getb_13		; no

	; input: eax = phy sector, ebx = buffer, esi = ldrv table
	call	FATSECRD
	; modified registers: (eax), ecx, edx
	jc	short getb_11
	mov	ch, buf_isFAT		; set buf_flags
	jmp	short getb_14

getb_11:
	; 01/01/2026
	mov	eax, ERR_DRV_READ ; 'disk read error !'

	; 05/06/2025
	pop	ebp ; **** ; disk sector number
	; 28/04/2025
	pop	esi ; ***
getb_12:
	pop	edx ; **
	pop	ebx ; *
	; eax = error code (if CF = 1)
	retn

getb_13:
	; 28/04/2025 - TRDOS 386 v2.0.10
	; read 1 disk sector ('trdosk7.s')
	;   --- mov ecx, 1
	;   --- call disk_read
	; input: eax = phy sector, ebx = buffer, esi = ldrv table,
	call	DREAD
	; modified registers: eax, ebx, ecx, edx
	jc	short getb_11
	mov	ch, 0			; set buf_flags to no type
getb_14:
	; 05/06/2025
	pop	eax ; **** ; restore disk sector number
getb_15:
	; 28/04/2025
	pop	esi ; ***
	pop	edx ; **
	pop	ebx ; *

	; eax = disk sector
	; ch = buf_flags
	; esi = buffer address
	; edx = logical dos drive table address

	mov	[esi+BUFFINFO.buf_sector], eax
	mov	cl, [edx+LD_PhyDrvNo]	; physical drive number
	mov	[esi+BUFFINFO.buf_ID], cx ; set ID and flags
	mov	ch, [pre_read]	; bit 0 -> fat buffer flag
				; bit 7 -> no pre-read flag
	; 09/11/2025
	mov	[esi+BUFFINFO.buf_DPB], edx ; current ldrvt address

	jmp	getb_3

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

GETCURRENTHEAD:
	; 21/04/2025
	; (MSDOS -> GETCURHEAD) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;	none
	; OUTPUT:
	;	esi = the first buffer address in Queue
	;	FIRST_BUFF_ADDR = esi
	;	LastBuffer = -1
	;
	; Modified registers:
	;	esi

	mov	esi, [BufferQueue]	; pointer to the first buffer
	mov	dword [LastBuffer], -1	; invalidate last buffer
	mov	[FIRST_BUFF_ADDR], esi	; save first buffer address
	retn

; --------------------------------------------------------------------

; 21/04/2025 - TRDOS 386 v2.0.10

PLACEBUFFER:
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

METACOMPARE:
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

	mov	ecx, 11
WildCrd:
	repe	cmpsb
	jz	short MetaRet 	; most of the time we will fail.

	cmp	byte [esi-1], "?"
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
	;	esi = Start of the buffer (buffer header address)
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
	; 25/11/2025
	; 22/07/2025
	; 10/07/2025
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
	;	esi = Start of the (FAT) buffer
	;	ebx = buffer offset (not used)
	;	22/07/2025
	;	[ClusSave] = cluster data (16 bit or 32 bit)
	;	10/07/2025
	;	[CLUSNUM] = eax input
	;	edi = (FAT) buffer (data/cluster) address
	;	[ClusSave], [ClusSec], [ClusSplit]
	;
	;	If CF = 0 and ZF = 1 (EAX = 0) -> Free Cluster
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	mov	[CLUSNUM], eax	; save current cluster number

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

	mov	cl, [edx+LD_PhyDrvNo]

	add	eax, [edx+LD_FATBegin]
	; eax = physical sector number of the FAT sector

	mov	[ClusSec], eax

	; edx = logical dos drive table address
	; ebx = cluster number offset (in the buffer)
	;  cl = physical drive number

	call	GETFATBUFFER
	jc	short mapcl_5	; eax = error code

	;mov	esi, [CurrentBuffer]
	lea	edi, [esi+BUFINSIZ]
	add	edi, ebx

	cmp	ebx, 511
	jne	short mapcl_4

	; FAT12, cluster split

	mov	al, [edi]
	mov	[ClusSave], al

	inc	byte [ClusSplit]

	;mov	eax, [esi+BUFFINFO.buf_sector]
	;mov	[ClusSec], eax

	mov	eax, [ClusSec]
	; 22/07/2025
	mov	cl, [edx+LD_PhyDrvNo]
	inc	eax

	call	GETFATBUFFER
	jc	short mapcl_5	; eax = error code

	;mov	al, [ClusSave]
	; 10/07/2025
	;mov	ah, [esi+BUFINSIZ]
	lea	edi, [esi+BUFINSIZ]
	;mov	ah, [edi]
	; 25/11/2025
	mov	eax, [edi]
	shl	eax, 8
	mov	al, [ClusSave]
	mov	[ClusSave], eax
	; 25/11/2025
	;jmp	short mapcl_7

mapcl_7:
	test	byte [CLUSNUM], 1 ; odd ?
	jz	short mapcl_8 ; no, even

	; FAT12, high 12 bit
	shr	eax, 4
mapcl_8:
	; FAT12
	; low 12 bit
	and	eax, 0FFFh
	retn

mapcl_4:
	mov	eax, [edi]
	; 22/07/2025
	mov	[ClusSave], eax
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
	; 08/05/2025
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

	; 08/05/2025
	;jmp	short SETROOTSRCH3
	
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
	; 17/05/2025
	; 27/04/2025
	; (MSDOS -> FIGREC) - Ref: Retro DOS v5 - ibmdos7.s
	; Convert cluster number to physical disk sector number
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	;	; 17/05/2025
	;	;ebx = Sector position in cluster
	; OUTPUT:
	;	eax = physical sector number
	;	 cl = physical drive/disk number
	;		(needed for GETBUFFER procedure)
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
	; 17/05/2025
	;add	eax, ebx
	add	eax, [edx+LD_DATABegin]
	mov	cl, [edx+LD_PhyDrvNo]
	retn

; --------------------------------------------------------------------

; 27/04/2025 - TRDOS 386 v2.0.10

DIRREAD:
	; 17/07/2025
	; 01/06/2025
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

	; 01/06/2025
	mov	ebx, eax
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

	;movzx	ebx, byte [SECCLUSPOS]
	mov	eax, [CLUSNUM]

	call	FIGREC

	; 17/07/2025
	movzx	ebx, byte [SECCLUSPOS]
	add	eax, ebx

	; eax = physical sector number
	;  cl = physical drive/disk number
drd_getbuf:
	call	GETBUFFER ; pre-read
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

; --------------------------------------------------------------------

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

; --------------------------------------------------------------------

; 28/04/2025 - TRDOS 386 v2.0.10

FATSECRD:
	; 01/01/2026
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
	;;;;	if cf = 1 -> eax = error code
	;
	; Modified registers:
	;	EAX, ECX, EDX

	xor	ecx, ecx

	mov	cl, [esi+LD_BPB+BPB_NumFATs] ; FAT count

	cmp	byte [esi+LD_FATType], 2
	jna	short fatsecrd_1 ; not FAT32

	test	byte [esi+LD_BPB+BPB_ExtFlags], 80h
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

	; 01/01/2026
	;mov	eax, ERR_DRV_READ ; 'disk read error !'
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

; --------------------------------------------------------------------

; 28/04/2025 - TRDOS 386 v2.0.10

BUFWRITE:
	; 19/07/2025
	; 14/07/2025
	; 28/04/2025
	; (MSDOS -> BUFWRITE) - Ref: Retro DOS v5 - ibmdos7.s
	; Write out a buffer if dirty
	;
	; INPUT:
	; 	ESI = Buffer header address
	; OUTPUT:
	;	Buffer marked free
	;	EDX = LDRVT address ; 19/07/2025
	;
	;	if cf = 1 -> eax = [FAILERR] = error code
	;		     edx = LDRVT addr for failed drive
	;;	if cf = 0 -> [FAILERR] = 0 ; 14/07/2025

	; Modified registers:
	;	EAX, EBX, ECX, EDX

	xor	ecx, ecx
	; 14/07/2025
	mov	[FAILERR], cl
	mov	cl, 0FFh ; -1
	xchg	ecx, [esi+BUFFINFO.buf_ID]
	cmp	cl, 0FFh
	je	short bufwrt_4 ; Buffer is free, carry clear.

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
	;jnz	short bufwrt_4 ; At least one write succeed

	; 28/04/2025
	; return LDRVT address ; 19/07/2025
	mov	edx, [esi+BUFFINFO.buf_DPB]

	; 19/07/2025
	jnz	short bufwrt_4 ; At least one write succeed

	mov	eax, ERR_DRV_WRITE ; 'disk write error !'
	; 14/07/2025
	mov	[FAILERR], al
	stc
bufwrt_4:
	retn

; --------------------------------------------------------------------

; 29/04/2025 - TRDOS 386 v2.0.10

FLUSHBUFFERS:
	; 14/07/2025
	; 13/07/2025
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
	;	(if cf = 1 -> edx = LDRVT addr for failed drive)
	;
	; Modified registers:
	;	EBX, ECX, EDX, ESI

	; Note: if AL = -1 all of disk buffers will be invalidated
	;	after writing
	;	Otherwise, no_invalidate flag (AH) will be checked

	;call	GETCURHEAD ; (MSDOS)
	call	GETCURRENTHEAD

	; 13/07/2025
	push	eax
	call	BUFWRITE
	pop	eax

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
	;;;
	; 14/07/2025
	cmp	byte [FAILERR], 1
	cmc
	;;;
	retn

	; 29/04/2025
flushbuf_err:
	; set disk (buffer) write error flag (bit 7)
	; edx = logical dos drive table address (from CHECKFLUSH)
	or	byte [edx+LD_MediaChanged], 80h
	jmp	short dont_free_the_buf ; already invalidated

; --------------------------------------------------------------------

; 29/04/2025 - TRDOS 386 v2.0.10

CHECKFLUSH:
	; 14/07/2025	
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
	;	if cf = 1 -> [FAILERR] = error code ; 14/07/2025
	;	             edx = LDRVT addr for failed drive
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

; --------------------------------------------------------------------

; 03/05/2025 - TRDOS 386 v2.0.10

update_fat32_fsinfo:
	; 01/12/2025
	; 04/05/2025
	; 03/05/2025
	; Ref: Retro DOS v5 - ibmdos7.s
	; (PCDOS 7.1, Retrodos v5 -> update_fat32_fsinfo)
	;
	; Update FAT32 fs info sector data
	;
	; INPUT:
	;	ESI = Logical DOS Drive Description Table address
	;
	;	[esi+LD_BPB+FAT32_FreeClusters] = free cluster count
	;	[esi+LD_BPB+FAT32_FirstFreeCluster] = 1st free clust
	;
	; OUTPUT:
	;	if cf = 0 and EAX = 0 ->
	;		FSINFO sector is not valid or not updated
	;
	;	if cf = 0 and EAX > 0 ->
	;		eax = physical address of the FSINFO sector
	;		which is suscessfully written/updated
	;	   (Or, if eax > 0,
	;		it means fsinfo is valid and not modified)
	;
	;	if cf = 1 -> EAX = (disk io/rw) error code
	;
	; Modified registers:
	;	EAX, EBX, ECX, EDX

	cmp	byte [esi+LD_FATType], 3 ; FAT32 fs ?
	jne	short u_fat32_fsi_2 ; no, nothing to do

	; 01/12/2025
	; check modification status of FSINFO sector data
	cmp	byte [esi+LD_BPB+BS_FAT32_Reserved1], 0
	jna	short u_fat32_fsi_2 ; FSINFO data is valid
			; and free count or ffc is not changed
	; reset FSINFO sector modification flag
	mov	byte [esi+LD_BPB+BS_FAT32_Reserved1], 0 ; reset

; 04/05/2025
%if 0
	push	esi
	call	GETCURRENTHEAD	; (MSDOS -> GETCURHEAD)
	call	BUFWRITE
	jnc	short u_fat32_fsi_1
	pop	esi
	retn

u_fat32_fsi_1:
	lea	ebx, [esi+BUFINSIZ]
	pop	esi
	movzx	eax, word [esi+LD_BPB+FAT32_FSInfoSec]
	add	eax, [esi+LD_StartSector]

	mov	[CFS_FAT32FSINFOSEC], eax

	push	ebx
	call	DREAD	; read fs info sector
	pop	ebx
	jc	short u_fat32_fsi_3

	;cmp	dword [ebx+FSINFO.LeadSig], 41615252h
	cmp	dword [ebx], 41615252h ; 'RRaA' ; (NASM syntax)
	jne	short u_fat32_fsi_2

	;cmp	dword [ebx+484], 61417272h
	cmp	dword [ebx+FSINFO.StrucSig], 61417272h ; 'rrAa'
	je	short u_fat32_fsi_4

u_fat32_fsi_2:
	xor	eax, eax
	; eax = 0 -> no error

u_fat32_fsi_3:
	retn
	
u_fat32_fsi_4:
	;cmp	dword [ebx+508], 0AA550000h
	cmp	dword [ebx+FSINFO.TrailSig], 0AA550000h
	jne	short u_fat32_fsi_2

%else
	call	read_fat32_fsinfo
	jnc	short u_fat32_fsi_1
	retn

u_fat32_fsi_2:
	xor	eax, eax
	; eax = 0 -> no error

u_fat32_fsi_3:
	retn

%endif

u_fat32_fsi_1:
	mov	eax, [esi+LD_BPB+FAT32_FreeClusters]
			; note: this reserved field is used to store free
 			;	cluster count but this field must be zero
			;	on the disk (FAT32 volume boot sector)
	; set free cluster count
	mov	[ebx+FSINFO.Free_Count], eax ; BPB_Reserved ; 52
	mov	eax, [esi+LD_BPB+FAT32_FirstFreeClust]
			; note: this reserved field is used to store
 			;	the first free cluster number
			;	but this field must be zero on the disk
			;		 (FAT32 volume boot sector)
	; set first free cluster number to search
	mov	[ebx+FSINFO.Nxt_Free], eax ; BPB_Reserved+4 ; 56

	mov	eax, [CFS_FAT32FSINFOSEC]

	;push	ebx
	call	DWRITE
	;pop	ebx
	jc	short u_fat32_fsi_3

	mov	eax, [CFS_FAT32FSINFOSEC]

	; flag/set the FSINFO sector data is valid and not-modified yet
	; (modification procedure will reset it to zero again)

	; 01/12/2025
	;mov	[esi+LD_BPB+FAT32_fsinfo_sector], eax

	retn

; --------------------------------------------------------------------

; 04/05/2025 - TRDOS 386 v2.0.10

read_fat32_fsinfo:
	; 28/11/2025
	; 04/05/2025
	;
	; Read FAT32 fs info sector
	;
	; INPUT:
	;	ESI = Logical DOS Drive Description Table address
	;
	; OUTPUT:
	;	EBX = buffer (data) address
	;	[CFS_FAT32FSINFOSEC] = FSINFO sector address
	;
	;	if cf = 1 -> EAX = (disk io/rw) error code
	;
	; Modified registers:
	;	EAX, EBX, ECX, EDX

	push	esi
	call	GETCURRENTHEAD	; (MSDOS -> GETCURHEAD)
	call	BUFWRITE
	jnc	short r_fat32_fsi_1
	pop	esi
	retn

r_fat32_fsi_1:
	lea	ebx, [esi+BUFINSIZ]
	pop	esi
	movzx	eax, word [esi+LD_BPB+FAT32_FSInfoSec]
	add	eax, [esi+LD_StartSector]

	mov	[CFS_FAT32FSINFOSEC], eax

	push	ebx
	call	DREAD	; read fs info sector
	pop	ebx
	jc	short r_fat32_fsi_3

	;cmp	dword [ebx+FSINFO.LeadSig], 41615252h
	cmp	dword [ebx], 41615252h ; 'RRaA' ; (NASM syntax)
	jne	short r_fat32_fsi_2

	;cmp	dword [ebx+484], 61417272h
	cmp	dword [ebx+FSINFO.StrucSig], 61417272h ; 'rrAa'
	jne	short r_fat32_fsi_2

	;cmp	dword [ebx+508], 0AA550000h
	cmp	dword [ebx+FSINFO.TrailSig], 0AA550000h
	je	short r_fat32_fsi_3

r_fat32_fsi_2:
	mov	eax, -1 ; invalid data/sector
	stc

r_fat32_fsi_3:
	retn

; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

DOS_SEARCH_FIRST:
	; 02/06/2025
	; 12/05/2025
	; 11/05/2025
	; (MSDOS -> DOS_SEARCH_FIRST) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Initiate a search for the given file spec
	;
	; INPUT:
	;	[WFP_START] Points to WFP string 
	;	   ("d:/" must be first 3 chars, NUL terminated)
	;	[CURR_DIR_END] Points to end of Current dir part of string
	;	   ( = -1 if current dir not involved, else
	;	   Points to first char after last "/" of current dir part)
	;	[THISCDS] Points to CDS being used
	;	   (Low word = -1 if NUL CDS (Net direct request))
	;	[SATTRIB] is attribute of search,
	;		  determines what files can be found
	;	FINDFILE_BUFFER is FFF destination buffer
	;
	; OUTPUT:
	;	FINDFILE_BUFFER is filled
	;
	;	12/05/2025 - temporary
	;	CARRY CLEAR
	;	The 57 bytes of FINDFILE_BUFFER are filled in as follows:
	;
	;	  Drive Byte (A=1, B=2, ...)
	;	  11 byte search name with Meta chars in it
	;	  Search Attribute Byte, attribute of search
	;	  DWORD LastEnt value
	;	  DWORD DirStart
	;	  4 byte pad
	;	  32 bytes of the directory entry found
	;
	;	if cf = 1 -> EAX = error code
	;
	; Modified registers:
	;	EAX, EBX, ECX, EDX, ESI, EDI, EBP

	mov	byte [NoSetDir], 1	; if we find a dir, don't change to it
	call	GETPATH
	; ebx = offset NAME1
	jnc	short found_entry
	jnz	short bad_path3	
	or	cl, cl
	jz	short bad_path3
find_no_more:
	mov	eax, ERR_NO_MORE_FILES	; 12
BadBye:
	stc
	retn

bad_path3:
	mov	eax, ERR_PATH_NOT_FOUND	; 3
	jmp	short BadBye

found_entry:
	; EBX points to start of entry in [CurrentBuffer]
	; ESI points to start of dir_first field in [CurrentBuffer]

	mov	edi, FINDFILE_BUFFER
	; 02/06/2025
	mov	esi, [WFP_START]		; get pointer to beginning
	mov	al, [esi]
	sub	al, 'A'-1		; logical drive
	stosb		; find_buf.drive
	mov	esi, NAME1
	mov	ecx, 11
	rep	movsb	; find_buf.name

	mov	al, [ATTRIB]
	stosb		; find_buf.sattr
	mov	eax, [LASTENT]
	stosd		; find_buf.LastEnt
	mov	eax, [DIRSTART]
	stosd		; find_buf.DirStart

	mov	eax, -1
	stosd		; find_buf.NETID

	cmp	[CurrentBuffer], eax ; -1
	jne	short OkStore

	; The user has specified the root directory itself,
	; rather than some contents of it. We can't "find" that.

	; Cause DOS_SEARCH_NEXT to fail by stuffing a -1 at Lastent
	mov	[edi-12], eax ; -1 ; find_buf.LastEnt
	jmp	short find_no_more

OkStore:
	; ebx = offset NAME1 (from GETPATH)
	mov	esi, ebx	; ESI -> start of entry
	mov	ecx, 32/4
	rep	movsd		; find_buf.DirEntry
	retn

; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

GETPATH:
	; 05/06/2025
	; 11/05/2025
	; (MSDOS -> GETPATH) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Parse a WFP (crack the path)
	;
	; INPUT:
	;	[WFP_START] Points to WFP string 
	;	   ("d:/" must be first 3 chars, NUL terminated)
	;	[CURR_DIR_END] Points to end of Current dir part of string
	;	   ( = -1 if current dir not involved, else
	;	   Points to first char after last "/" of current dir part)
	;	[THISCDS] Points to CDS being used
	;	   (Low word = -1 if NUL CDS (Net direct request))
	;	[SATTRIB] is attribute of search,
	;		  determines what files can be found
	;	[NoSetDir] set
	;
	; OUTPUT:
	;	[ATTRIB] = [SATTRIB]
	;	EDX points to Logical DOS Drive Description Table
	;
	;	if cf = 1 -> ERROR
	;	   ESI points to path element causing failure
	;	   if zf = 1
	;	      [DIRSTART],[DIRSEC],[CLUSNUM] and [CLUSFAC]
	;	        are set up to start a search on the last directory
	;	      CL is zero if there is a bad name in the path
	;	      CL is non-zero if the name was simply not found
	;		 [ENTFREE] may have free spot in directory
	;		 [NAME1] is the name.
	;		 CL = 81H if '*'s or '?' in NAME1, 80H otherwise
	;	   if zf = 0
	;	      File in middle of path or bad name in path
	;			or attribute mismatch
	;			or path too long or malformed path
	;	if cf = 0
	;	   [CurrentBuffer] = -1 if root directory
	;	   [CurrentBuffer] contains directory record with match
	;	   EBX points to start of entry in [CurrentBuffer]
	;	   ESI points to start of dir_first field in [CurrentBuffer]
	;	   [NAME1] has name looked for
	;	   If last element is a directory zero flag is set and:
	;	      [DIRSTART],[SECCLUSPOS],[DIRSEC],[CLUSNUM] & [CLUSFAC]
	;	        are set up to start a search on it.
	;	      unless [NoSetDir] is non zero in which case
	;		the return is like that for a file
	;				(except for zero flag)
	;	   If last element is a file zero flag is reset and:
	;	      [DIRSEC],[CLUSNUM],[CLUSFAC],[NXTCLUSNUM],[SECCLUSPOS],
	;	      [LASTENT], [ENTLAST] are set to continue search of last
	;	      directory for further matches on NAME1 via the NEXTENT
	;	      entry point in FindEntry (or GETENT entry in GETENTRY in
	;	      which case [NXTCLUSNUM] & [SECCLUSPOS] need not be valid)
	;
	; Modified registers:
	;	EAX, EBX, ECX, EDX, ESI, EDI, EBP
	;

	mov	word [CREATING], 0E500h ; Not Creating, not DEL *.*

	; Same as GetPath only CREATING and DELALL already set

	;entry	GetPathNoSet
GetPathNoSet:
	; (MSDOS -> [CURBUF])
	mov	eax, -1
	mov	[CurrentBuffer], eax	; -1 ; initial setting
	xor	edx, edx
	mov	dh, [Current_Drv]
	; 05/06/2025
	add	edx, Logical_DOSDisks
	; edx = LDRVT address		; (MSDOS -> [THISDPB])
CrackIt:
	mov	byte [ATTRIB],attr_directory+attr_system+attr_hidden
				; Attributes to search through Dirs
	mov	edi, [THISCDS]
	mov	ebx, [edi+curdir.ID]
	mov	esi, [CURR_DIR_END]

	; EAX = -1
	; EBX = Cluster number of current directory.
	;	This number is -1 if the media has been uncertainly changed.
	; ESI = Path offset to end of current directory text.
	;	This may be -1 if no current directory part has been used.

	cmp	esi, eax		; if Current directory is not part
	je	short NO_CURR_D		; then we must crack from root

	cmp	ebx, eax	; is the current directory cluster valid ?
	jne	short short Got_Search_Cluster ; yes

	; no, crack from the root
NO_CURR_D:
	mov	esi, [WFP_START]
	add	esi, 3			; skip 'd:/'
	;xor	edx, edx
	;mov	dh, [Current_Drv]
	;add	edx, Logical_DOSDisks
	; edx = LDRVT address		; (MSDOS -> [THISDPB])
	jmp	short ROOTPATH

	; crack from the current directory part
Got_Search_Cluster:
	;xor	edx, edx
	;mov	dh, [Current_Drv]
	;add	edx, Logical_DOSDisks
	; edx = LDRVT address		; (MSDOS -> [THISDPB])
	call	SETDIRSRCH
	jnc	short FINDPATH
SetFErr:
	xor	cl, cl			; set zero
	stc
	retn

; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

ROOTPATH:
	; 11/05/2025
	; (MSDOS -> ROOTPATH) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Search from root for path
	;
	; INPUT:
	;	Same as FINDPATH but,
	;	ESI Points to asciiz string of path which is assumed
	;	    to start at the root (no leading '/').
	;
	; OUTPUT:
	;	Same as FINDPATH but:
	;	If root directory specified,
	;	   [CURBUF] and [NAME1] are NOT set,
	;	   and [NoSetDir] is ignored.
	;
	; Modified registers:
	;	EAX, EBX, ECX, ESI, EDI, EBP
	;

ROOTPATH:
	push	esi
	call	SETROOTSRCH
	pop	esi
	cmp	byte [esi], 0
	ja	short FINDPATH

	; Root dir specified
	
	mov	al, [SATTRIB]
	mov	[ATTRIB], al

	retn

; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

FINDPATH:
	; 11/05/2025
	; (MSDOS -> FINDPATH) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Parse path name - Search from path
	;
	; INPUT:
	;	[ATTRIB] Set to get through directories
	;	[SATTRIB] Set to find last element
	;	EDX Points to Logical Dos Drive Description Table
	;	ESI Points to asciz string of path (no leading '/').
	;	[SECCLUSPOS] = 0
	;	[DIRSEC] = Phys sec # of first sector of directory
	;	[CLUSNUM] = Cluster # of next cluster
	;	[CLUSFAC] = Sectors per cluster
	;	[NoSetDir] set
	;	[CURR_DIR_END] Points to end of Current dir part of string
	;	   ( = -1 if current dir not involved, else
	;	   Points to 1st char after last "/" of current dir part)
	;	[THISCDS] Points to CDS being used
	;	[CREATING] and [DELALL] set
	;
	; OUTPUT:
	;	ID field of [THISCDS] updated appropriately
	;	[ATTRIB] = [SATTRIB]
	;	EDX points to Logical DOS Drive Description Table
	;
	;	if cf = 1 -> ERROR
	;	   ESI points to path element causing failure
	;	   if zf = 1
	;	      [DIRSTART],[DIRSEC],[CLUSNUM] and [CLUSFAC]
	;	        are set up to start a search on the last directory
	;	      CL is zero if there is a bad name in the path
	;	      CL is non-zero if the name was simply not found
	;		 [ENTFREE] may have free spot in directory
	;		 [NAME1] is the name.
	;		 CL = 81H if '*'s or '?' in NAME1, 80H otherwise
	;	   if zf = 0
	;	      File in middle of path or bad name in path
	;			or attribute mismatch
	;			or path too long or malformed path
	;	if cf = 0
	;	   [CurrentBuffer] = -1 if root directory
	;	   [CurrentBuffer] contains directory record with match
	;	   EBX points to start of entry in [CurrentBuffer]
	;	   ESI points to start of dir_first field in [CurrentBuffer]
	;	   [NAME1] has name looked for
	;	   If last element is a directory zero flag is set and:
	;	      [DIRSTART],[SECCLUSPOS],[DIRSEC],[CLUSNUM] & [CLUSFAC]
	;	        are set up to start a search on it.
	;	      unless [NoSetDir] is non zero in which case
	;		the return is like that for a file
	;				(except for zero flag)
	;	   If last element is a file zero flag is reset and:
	;	      [DIRSEC],[CLUSNUM],[CLUSFAC],[NXTCLUSNUM],[SECCLUSPOS],
	;	      [LASTENT], [ENTLAST] are set to continue search of last
	;	      directory for further matches on NAME1 via the NEXTENT
	;	      entry point in FindEntry (or GETENT entry in GETENTRY in
	;	      which case [NXTCLUSNUM] & [SECCLUSPOS] need not be valid)
	;
	; Modified registers:
	;	EAX, EBX, ECX, ESI, EDI, EBP
	;

	push	esi ; *
	cmp	dword [CURR_DIR_END], -1
	je	short NOIDS		; No current dir part
	cmp	esi, [CURR_DIR_END]
	jne	short NOIDS		; Not to current dir end yet
	mov	ecx, [DIRSTART]		; Get start clus of dir being searched
	mov	edi, [THISCDS]
	mov	[edi+curdir.ID], ecx	; Set current directory cluster
NOIDS:
	; Parse the name offset of ESI into NAME1.
	; AL = 1 if there was a meta character in the string.
	; ECX, EDI are destroyed.
	
	xor	ecx, ecx
	mov	edi, NAME1
	push	edi
	mov	al, ' ' ; 20h ; space
	mov	cl, 11
	rep	stosb
	pop	edi
GetNam:
	lodsb
	cmp	al, '.'	; 2Eh
	je	short _SetExt
	or	al, al
	jz	short _GetDone
	cmp	al, '/'
	je	short _GetDone
	cmp	al, '?'
	jne	short StoNam
	or	cl, 1
StoNam: 
	stosb
	jmp	short GetNam
_SetExt:
	mov	edi, NAME1+8
GetExt:
	lodsb
	or	al, al
	jz	short _GetDone
	cmp	al, '/'
	je	short _GetDone
	cmp	al, '?'
	jne	short StoExt
	or	cl, 1
StoExt: 
	stosb
	jmp	short GetExt
_GetDone:
	dec	esi
	or	cl, 80h
	pop	edi ; *			; Start of this element
	cmp	esi, edi
	jne	short Find_File
	jmp	_BadPath		; NUL parse (two delims most likely)

Find_File:
	push	esi ; *			; Start of next element
	push	edi ; **		; Start of this element
	push	ecx ; ***

	call	FINDENTRY
DIR_FOUND:
	pop	ecx ; ***
	pop	esi ; **
	jnc	short LOAD_BUF
	jmp	BADPATHPOP

LOAD_BUF:
	mov	edi, [CurrentBuffer]	; (MSDOS ->[CURBUF])
	test	byte [ebx+dir_entry.dir_attr], attr_directory
	jnz	short GO_NEXT
FILEINPATH_j:
	jmp	short FILEINPATH	; Error or end of path

	; 03/03/2025
_SETRET:
	retn

	; if not a directory, then check for end of string
GO_NEXT:
	cmp	byte [NoSetDir], 0
	jz	short SetDir
	mov	ecx, edi		; Save pointer to entry

	pop	edi ; *			; Start of next element

	cmp	byte [edi], 0
	jz	short _SETRET

NEXT_ONE:
	push	edi ; *		; Put start of next element back on stack
	mov	edi, ecx		; Get back pointer to entry
SetDir:
	xor	eax, eax
	cmp	byte [edx+LD_FATType], 3
	jb	short SetDir2 ; not FAT32
	mov	ax, [esi-6]		; dir_entry.dir_fclus_hi
	shl	eax, 16
SetDir2:
	mov	ax, [esi] 		; dir_entry.dir_first

DO_NORMAL:
	sub	ebx, edi	; Offset into sector of start of entry
	sub	esi, edi	; Offset into sector of dir_first
	push	ebx ; ++++
	push	eax ; +++
	push	esi ; ++
	push	ecx ; +

	mov	ebx, [edi+BUFFINFO.buf_sector]
	push	ebx

	; eax = cluster number
	; edx = LDRVT address

	call	SETDIRSRCH	; This uses UNPACK which might blow
				; the entry sector buffer
	pop	ebx
	jc	short SKIP_GETB

	; eax = [DIRSEC] = physical sector number
	;  cl = physical drive/disk number (not used here) 

	call	GETBUFFER ; pre-read

	; esi = [CurrentBuffer] Points to the Buffer for the sector
	; (MSDOS -> [CURBUF])
	; edx = Logical DOS Drive Table address	

SKIP_GETB:
	pop	ecx ; +
	pop	esi ; ++
	pop	eax ; +++
	pop	ebx ; ++++
	jnc	short SET_THE_BUF
	pop	esi ; *			; Start of next element
	jmp	short _BadPath

SET_THE_BUF:
	mov	edi, [CurrentBuffer]	; (MSDOS -> [CURBUF])
	add	esi, edi		; Get the offsets back
	add	ebx, edi
	pop	edi ; *			; Start of next element

	mov	al, [edi]
	or	al, al
	jz	short _SETRET		; At end
	inc	edi			; Skip over "/"
	mov	esi, edi		; Point with ESI

	; 11/05/2025
	; !!! (al = '/' there is no other possibility) !!!
	;call	PATHCHRCMP ; (MSDOS)
	cmp	al, '/'
	jnz	short find_bad_name	; oops
	jmp	FINDPATH		; Next element

find_bad_name:
	dec	esi			; Undo above INC to get failure point
_BadPath:
	xor	cl, cl			; Set zero
	jmp	short BADPRET

FILEINPATH:
	pop	edi ; *			; Start of next element

	mov	al, [edi]
	or	al, al
	jnz 	short BADPRET_X		; Path too long

	inc	al			; Reset zero
	; zf = 0
	retn

BADPATHPOP:
	pop	esi ; * 		; Start of next element
	mov	al, [esi]
	or	al, al	; zero if bad element is last, non-zero if path too long
BADPRET_X:
	mov	esi, edi		; Start of bad element
BADPRET:
	mov	al, [SATTRIB]
	mov	[ATTRIB], al		; Make sure return correct
	stc
	retn

; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

SEARCH:
FINDENTRY:
	; 02/06/2025
	; 11/05/2025
	; (MSDOS -> FINDENTRY) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Look for a directory entry
	;	(find file name in disk directory)
	;
	; INPUT:
	;	;;;[THISDPB] = Base of drive parameters (MSDOS)
	;	EDX Points to Logical Dos Drive Description Table
	;	[SECCLUSPOS] = 0
	;	[DIRSEC] = Starting directory sector number (physical)
	;	[CLUSNUM] = Next cluster of directory
	;	[CLUSFAC] = Sectors per cluster
	;	[NAME1] = Name to look for
	;
	; OUTPUT:
	;	If cf = 1 -> name not found
	;	if cf = 0
	;		Zero set if attributes match
	;			 (always except when creating)
	;	;;;[THISDPB] = Base of drive parameters (MSDOS)
	;	EDX Points to Logical Dos Drive Description Table
	;	EBX = Pointer into directory buffer
	;	ESI = Pointer to 1st Cluster field in directory entry
	;	;[CURBUF] has directory record with match (MSDOS)
	;	[CurrentBuffer] has directory sector with match
	;	[NAME1] has file name
	;	[LASTENT] is entry number of the entry
	;
	; Modified registers:
	;	EAX, EBX, ECX, ESI, EDI, EBP
	;

	call	STARTSRCH
	mov	al, [ATTRIB]
	and	al, ~attr_ignore	; Ignore useless bits
	cmp	al, attr_volume_id	; Looking for vol ID only ?
	jne	short NotVolSrch	; No
	call	SETROOTSRCH		; Yes force search of root
NotVolSrch:	
	call	GETENTRY
	jc	short SETESRET

	; EBX = Pointer to next directory entry in CURBUF
	; ECX = Pointer to first byte after end of CURBUF
	; [LASTENT] = New directory entry number
	; [NXTCLUSNUM],[SECCLUSPOS] set
	; EDX = LDRVT address (MSDOS -> [THISDPB])
SRCH:
	;mov	word [LNE_COUNT], 0	; reset long name entry count
	mov	byte [LNE_COUNT], 0	; reset long name entry count
SRCH2:
	mov	ah, [ebx] ; [ebx+dir_entry.dir_name] ; mov ah, [ebx+0]
	or	ah, ah
	jz	short findentry_free
	mov	al, [ebx+dir_entry.dir_attr] ; [ebx+0Bh]
	push	eax
	call	check_longname
	pop	eax
	jz	short NEXTENT2

	cmp	ah, [DELALL]		; Free entry?
	je	short findentry_free

	test	byte [ebx+dir_entry.dir_attr], attr_volume_id
					; Volume ID file?
	jz	short CHKFNAM 		; NO

	inc	byte [VOLID]
CHKFNAM:
	mov	edi, ebx  ; no possible '?' ; directory entry
	mov	esi, NAME1 ; possible '?'
	call	METACOMPARE
	jz	short findentry_found
NEXTENT:
	;mov	edx, [THISDPB]
	; edx = Logical DOS Drive Description Table address
	call	NEXTENTRY
	jnc	short SRCH
SETESRET:
	push	dword [ENTLAST]
	pop	dword [ENTLAST_PREV]	; previous ENTLAST
	jc	short SETESRETN
	;mov	word [LNE_COUNT], 0	; reset long name entry count
	mov	byte [LNE_COUNT], 0	; reset long name entry count
SETESRETN:
	retn

NEXTENT2:
	;inc	word [LNE_COUNT]	; Long Name entry count
	inc	byte [LNE_COUNT]
NEXTENT3:
	;mov	edx, [THISDPB]
	; edx = Logical DOS Drive Description Table address
	call	NEXTENTRY
	jnc	short SRCH2
	jmp	short SETESRET

findentry_free:
	mov	ecx, [LASTENT]
	cmp	ecx, [ENTFREE]
	jae	short TSTALL
	mov	[ENTFREE], ecx
TSTALL:
	cmp	ah, [DELALL]		; At end of directory?
	je	short NEXTENT3		; No - continue search
	mov	[ENTLAST], ecx
	stc
	jmp	short SETESRET

findentry_found:
	; We have a file with a matching name.
	; We must now consider the attributes:
	
	; ATTRIB     Action
	; ------     ------
	; Volume_ID  Is Volume_ID in test?
	; Otherwise  If no create then Is ATTRIB+extra superset of test?
	;	     If create then Is ATTRIB equal to test?

	mov	ch, [esi]		; Attributes of file
	mov	ah, [ATTRIB]		; Attributes of search
	;and	ah, 9Eh
	and	ah, ~attr_ignore
	;lea	esi, [esi+15]
	lea	esi, [esi+dir_entry.dir_first-dir_entry.dir_attr]
					; point to first cluster field
	;test	ch, 8
	test	ch, attr_volume_id	; Volume ID file?
	jz	short check_one_volume_id ; Nope check other attributes
	;test	ah, 8
	test	ah, attr_volume_id	; Can we find Volume ID?
	jz	short NEXTENT 		; Nope
	xor	ah, ah			; Set zero flag
	; zf = 1
	jmp	short SETESRET

check_one_volume_id:
	;cmp	ah, 8
	cmp	ah, attr_volume_id	; Looking only for Volume ID?
	je	short NEXTENT		; Yes, continue search
	call	MATCHATTRIBUTES
	jz	short SETESRET
	test	byte [CREATING], -1	; Pass back mismatch if creating
	;jz	short NEXTENT		; Otherwise continue searching
	;; zf = 0
	;jmp	short SETESRET
	; 02/06/2025
	jnz	short SETESRET
	; zf = 1
	jmp	NEXTENT

; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

STARTSRCH:
	; 11/05/2025
	; (MSDOS -> STARTSRCH) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Initiate a directory search
	;	(Set up a search for GETENTRY and NEXTENTRY)
	;
	; INPUT:
	;	;;;[THISDPB] = Base of drive parameters (MSDOS)
	;	;EDX Points to Logical Dos Drive Description Table
	; OUTPUT:
	;	;;;ES:BP = Drive parameters (MSDOS)
	;	;EDX Points to Logical Dos Drive Description Table
	;
	;	Sets up LASTENT=VOLID=0, ENTFREE=ENTLAST=-1
	;
	; Modified registers:
	;	EAX
	;

	;;;;les	bp, [THISDPB]

	xor	eax, eax ; 0
	mov	[LASTENT], eax
	mov	[VOLID], al ; 0	; No volume ID found
	dec	eax ; -1
	mov	[ENTFREE], eax
	mov	[ENTLAST], eax
	retn


; --------------------------------------------------------------------

; 11/05/2025 - TRDOS 386 v2.0.10

GETENTRY:
	; 11/05/2025
	; (MSDOS -> GETENTRY) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; Locates directory entry in preparation for search
	; (GETENT provides entry for passing desired entry in EAX)
	;
	; INPUT:
	;	[LASTENT] has directory entry
	;	EDX Points to Logical Dos Drive Description Table
	;	[DIRSEC],[CLUSNUM],[CLUSFAC],[ENTLAST] set
	;					 for DIR involved
	; OUTPUT:
	;	EBX = Pointer to next directory entry in CURBUF
	;	ECX = Pointer to first byte after end of CURBUF
	;	[LASTENT] = New directory entry number
	;	[NXTCLUSNUM],[SECCLUSPOS] set via DIRREAD
	;
	;	Carry set if error
	;
	; Modified registers:
	;	EAX, EBX, ECX, ESI, EDI, EBP
	;

;GETENTRY:
	mov	eax, [LASTENT]
	; 11/05/2025
	jmp	short GETENT_@
GETENT:
	mov	[LASTENT], eax
GETENT_@:
	; Convert the entry number in EAX into a byte offset
	;	from the beginning of the directory
	shl	eax, 5 ; * 32
	mov	ebx, edx
	xor	edx, edx ; 0
	;mov	ecx, 512
	movzx	ecx, word [ebx+LD_BPB+BytesPerSec]
	div	ecx
	xchg	ebx, edx

	push	ecx ; bytes per sector	
	push	ebx ; byte offset to dir entry

	; eax = directory block (index) number
	;	(relative to the first block of directory)
	; edx = Logical DOS Drive Description Table address
	; [DIRSEC] = First physical sector of 1st cluster of dir
	; [CLUSNUM] = Next cluster
	; [CLUSFAC] = Sectors/Cluster ; not used
	
	call	DIRREAD
	pop	ebx ; byte offset to dir entry
	pop	ecx ; bytes per sector
	jc	short getentry_retn

	; [NXTCLUSNUM] = Next cluster (after the one skipped to)
	; [SECCLUSPOS] Set
	; [CURBUF] points to Buffer with dir sector

SETENTRY:
	mov	eax, [CurrentBuffer] ; (MSDOS -> [CURBUF])
	add	eax, BUFINSIZ
	add	ebx, eax
	add	ecx, eax
	;clc
getentry_retn:
	retn

; --------------------------------------------------------------------

; 12/05/2025 - TRDOS 386 v2.0.10

MATCHATTRIBUTES:
	; 12/05/2025
	; (MSDOS -> MATCHATTRIBUTES) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; the final check for attribute matching
	;
	; INPUT:
	;	[ATTRIB] = attribute to search for
	;	CH = found attribute
	; OUTPUT:
	;	JZ <match>
	;	JNZ <nomatch>
	;
	; Modified registers: EAX
	;

	mov	al, [ATTRIB]	; searchset
	not	al		; searchset'
	and	al, ch		; searchset' and foundset
	;and	al, 16h
	and	al, attr_all	; searchset' & foundset & important
	retn

; --------------------------------------------------------------------

; 12/05/2025 - TRDOS 386 v2.0.10

check_longname:
	; 12/05/2025
	; (PCDOS 7.1 -> ?) - Ref: Retro DOS v5 - ibmdos7.s
	; ((ref: FAT32 File System Specification)) (Microsoft)
	;
	; check directry entry attribute 
	; if the entry is longname sub component of not
	;
	; INPUT:
	;	AL = attribute to search for
	;	AH = first character of DirEntry_Name
	;			(LDIR_Ord)
	; OUTPUT:
	;	JZ <match>
	;	JNZ <nomatch>
	;
	; Modified registers: EAX (AL)
	;

	cmp	ah, 0E5h
	jne	short chk_lname_@
	and	ah, ah
	; zf = 0
	retn
chk_lname_@:
	;and	al, 0Fh
	and	al, ATTR_LONGNAME_MASK ; 3Fh
	;cmp	al, 0Fh
	cmp	al, ATTR_LONGNAME ; 0Fh
	retn

; --------------------------------------------------------------------

; 02/06/2025 - TRDOS 386 v2.0.10
; temporary !!!! 02/06/2025

NEXTENTRY:
	stc
	retn

; --------------------------------------------------------------------

; 10/07/2025 - TRDOS 386 v2.0.10

PACK:
	; 03/12/2025
	; 26/11/2025
	; 25/11/2025
	; 22/07/2025
	; 10/07/2025
	; (MSDOS -> PACK) - Ref: Retro DOS v5 - ibmdos7.s
	; Pack FAT entries (ALLOCATE)
	;
	; INPUT:
	;	edx = Logical DOS Drive Description Table address
	;	eax = Cluster Number (28bit for FAT32 fs)
	;	ebx = Data
	; OUTPUT:
	;	The data is stored in the FAT at the given cluster.
	;
	;	if cf = 1, eax = error code
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	or	eax, eax
	jnz	short pack_1

	mov	[CL0FATENTRY], ebx
pack_0:
	retn
pack_1:
	push	ebx
	call	MAPCLUSTER
	pop	ebx
	jc	short pack_0

	; EAX = content of FAT for given cluster
	; EDI = buffer data (cluster pos) address
	; ESI = buffer header address
	; [CLUSNUM] = EAX input
	; 22/07/2025
	; [ClusSave] = 16 bit or 32 bit cluster data
	; [ClusSplit], [ClusSec]

	cmp	byte [edx+LD_FATType], 2
	ja	short pack_6 ; FAT32
	je	short pack_7 ; FAT16

	; 22/07/2025
	; Note: EAX contains 12 bit next cluster number
	;       but we need all of the 16 bit cluster data
	;	(it is in [ClusSave])
	;mov	ax, [ClusSave]
	; 25/11/2025
	mov	eax, [ClusSave]
	; 26/11/2025
	and	ebx, 0FFFh

	; FAT12
	test	byte [CLUSNUM], 1 ; odd ?
	jz	short pack_2	; no, even

	; move data to upper 12 bits
	shl	ebx, 4  ; shl bx, 4
	; leave in original low 4 bits
	and	eax, 0Fh
	;jmp	short PACKIN
	jmp	short pack_3

	; FAT12
pack_2:
	; leave upper 4 bits original
	and	eax, 0F000h
	; store only 12 bits
	;and	ebx, 0FFFh
	;jmp	short PACKIN ; pack_3

pack_3:	; (MSDOS -> PACKIN)
	or	ebx, eax  ; or bx, ax

	cmp	byte [ClusSplit], 0
	jz	short pack_7

	mov	[edi], bh
	;mov	[ClusSave], bl	; (*)

	;;mov	esi, [CurrentBuffer]
	;mov	eax, [esi+BUFFINFO.buf_sector]
	;dec	eax
	mov	eax, [ClusSec]
	mov	cl, [edx+LD_PhyDrvNo]
	;pop	ebx
	call	GETFATBUFFER
	;pop	ebx
	jc	short pack_5

	test	byte [esi+BUFFINFO.buf_flags], buf_dirty
				; if already dirty
	jnz	short pack_4	; don't increment dirty count

	;call	INC_DIRTY_COUNT
	;inc	word [DirtyBufferCount]
	inc	dword [DirtyBufferCount]

	;or	byte [esi+9],40h
	or	byte [esi+BUFFINFO.buf_flags], buf_dirty
pack_4:
	lea	edi, [esi+BUFINSIZ+511]
	;mov	bl, [ClusSave]	; (*)
	mov	[edi], bl
pack_5:
	retn
pack_6:
	; 22/07/2025
	; Note: EAX contains 28 bit next cluster number
	;       32 bit cluster data is in [ClusSave]
	mov	eax, [ClusSave]
	;;;
	and	eax, 0F0000000h
	; 03/12/2025
	and	ebx, 00FFFFFFFh
	or	ebx, eax
	mov	[edi], ebx
	jmp	short pack_8
pack_7:
	mov	[edi], bx
pack_8:
	;mov	esi, [CurrentBuffer]
	test	byte [esi+BUFFINFO.buf_flags], buf_dirty
				; if already dirty
	jnz	short pack_9	; don't increment dirty count

	;call	INC_DIRTY_COUNT
	;inc	word [DirtyBufferCount]
	inc	dword [DirtyBufferCount]

	;or	byte [esi+9], 40h
	or	byte [esi+BUFFINFO.buf_flags], buf_dirty
pack_9:
	retn

; --------------------------------------------------------------------

; 11/07/2025 - TRDOS 386 v2.0.10

ADD_NEW_CLUSTER:
	; 21/11/2025
	; 19/11/2025
	;mov	ecx, 1
;ADD_NEW_CLUSTERS:
	; 17/07/2025
	; 14/07/2025
	; 12/07/2025
	; 11/07/2025
	; (MSDOS -> ALLOCATE) - Ref: Retro DOS v5 - ibmdos7.s
	; Allocate disk clusters
	;
	; ALLOCATE is called to allocate disk clusters.
	; The new clusters are FAT-chained
	;  onto the end of the existing file.
	;
	; INPUT:
	;     edx = Logical DOS Drive Description Table address
	;     eax = Last cluster of file (0 if null file)
	;     19/11/2025
	;;;   ecx = Number of clusters to allocate
	;     [current_file] = file (SFT) number
	;
	; OUTPUT:
	;  If cf = 0
	;     eax = First cluster allocated
	;     FAT is fully updated
	;     ; 14/07/2025
	;     ebx = eax input
	;;;;  OF_FCLUST field of [current_file]
	;;;;		is set if file was null
	;
	;  If cf = 1 and eax = 39 ; ERR_DISK_SPACE (disk full)
	;     ecx = max. no. of clusters that could be added to file
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

;ALLOCATE:
	; edx = Logical DOS Drive Description Table address
	; eax = Last cluster of file (0 if null file)
	;; ecx = Number of clusters to allocate
	; 19/11/2025
	;mov 	[NEXTCLUSTER], eax
	mov	[LASTCLUSTER], eax
	;mov	[CLUSTERS], ecx
	;mov	[CLUSTCOUNT], ecx

	; 19/11/2025
	;xor	eax, eax ; 0
	;call	UNPACK
	;; eax = [CL0FATENTRY]
	;mov	[FATBYT], eax	; save correct cluster 0 value
	;;jc	short figrec_retn ; abort if error

	cmp	byte [edx+LD_FATType], 2
	jna	short adc_1 ; FAT fs

	; FAT32 fs
	mov	eax, [edx+LD_BPB+FAT32_FirstFreeClust]
	jmp	short adc_2

adc_1:
	; FAT16 or FAT12 fs
	mov	eax, [edx+LD_BPB+FAT_FirstFreeClust]
adc_2:
	; 12/07/2025
	;;;
	mov	ecx, [edx+LD_Clusters]
	inc	ecx
	mov	[FCS_END], ecx ; Last Cluster (search end)
	;;;

	;and	eax, eax
	;jz	short adc_4
	; 19/11/2025
	cmp	eax, 2
	jb	short adc_4

	mov	[FCS_START], eax

	cmp	eax, -1 ; invalid
	jb	short adc_7 ; FINDFRE
adc_3:
	xor	eax, eax
adc_4:
	; reset to the default
	mov	al, 2
	mov	[FCS_START], eax
adc_5:
	cmp	byte [edx+LD_FATType], 2
	jna	short adc_6
	mov	[edx+LD_BPB+FAT32_FirstFreeClust], eax
	jmp	short adc_7  ; FINDFRE
adc_6:
	mov	[edx+LD_BPB+FAT_FirstFreeClust], eax

adc_7:	; (MSDOS -> FINDFRE)
	; eax = first free cluster candidate to be checked
	; edx = LDRVT address
	mov	[FREECLUSTER], eax
	call	UNPACK
	jc	short adc_10
	; 14/07/2025
	mov	eax, [FREECLUSTER]
	jz	short adc_11 ; free cluster (eax = 0)

	;mov	eax, [FREECLUSTER] ; 14/07/2025
adc_8:
	;inc	eax  ; next cluster to be checked
	;mov	ecx, [edx+LD_Clusters] ; last cluster - 1
	;inc	ecx  ; disk's last cluster
	;cmp	eax, ecx
	;jna	short adc_5
	; 12/07/2025
	;cmp	eax, [edx+LD_Clusters]
	;ja	short adc_9
	;inc	eax
	;jmp	short adc_5
	inc	eax
	cmp	eax, [FCS_END]
	jna	short adc_5

; We're at the end of the disk, and not satisfied.
; See if we've scanned ALL of the disk...

	; 12/07/2025
	mov	ecx, [FCS_START]
	cmp	ecx, 2
	ja	short adc_9
	jmp	adc_17  ; disk full !
adc_9:
	dec	ecx
	mov	[FCS_END], ecx
	jmp	short adc_3
adc_10:
	retn
adc_11:
	; 21/11/2025
	;; free cluster (eax = 0)
	;;mov	eax, [FREECLUSTER] ; 14/07/2025
	;; eax = cluster number
	;;mov	ebx, 1
	;sub	ebx, ebx
	;inc	ebx 	; 1 ; mark this free guy as "1"
	;call	PACK	; set special "temporary" mark
	;jc	short adc_10

	cmp	byte [edx+LD_FATType], 2
	jna	short adc_12 ; not FAT32

	; FAT32 fs
	lea	ebx, [edx+LD_BPB+FAT32_FreeClusters]
	jmp	short adc_13

adc_12:
	; FAT16 or FAT12 fs
	lea	ebx, [edx+LD_BPB+FAT_FreeClusters]
adc_13:
; 21/11/2025
%if 0	
	mov	eax, [ebx]
	inc	eax
	;jz	short NO_ALLOC ; Free count not valid
	jz	short adc_14
	dec	eax

	dec	eax
	; Reduce free count by 1
	mov	[ebx], eax
	movzx	eax, byte [edx+LD_BPB+SecPerClust]
	; 17/07/2025
	sub	[edx+LD_FreeSectors], eax
%else
	; 21/11/2025
	mov	ecx, [ebx]
	inc	ecx
	jz	short adc_14 ; Free count not valid
	dec	ecx

	dec	ecx
	; Reduce free count by 1
	mov	[ebx], ecx
	movzx	ecx, byte [edx+LD_BPB+SecPerClust]
	sub	[edx+LD_FreeSectors], ecx
%endif

adc_14: ; (MSDOS -> NO_ALLOC)
	; 19/11/2025
	;mov	ebx, [FREECLUSTER]
	;mov	eax, [NEXTCLUSTER]
	;call	PACK
	;jc	short adc_10

	;dec	dword [CLUSTCOUNT]
	;jz	short adc_15
	;
	;mov	eax, [FREECLUSTER]
	;mov	[NEXTCLUSTER], eax
	;jmp	short adc_8

; We've successfully extended the file. Clean up and exit

adc_15:
	;mov	eax, [FREECLUSTER] ; (new) last cluster
	; 21/11/2025
	; eax = [FREECLUSTER]
	mov	ebx, -1 ; 0FFFFFFFFh
	call	PACK	; mark last cluster EOF
	jc	short adc_10

	; 19/11/2025
	;mov	eax, [LASTCLUSTER]
	;call	UNPACK		; Get first cluster allocated for return
	;jc	short adc_10
	;mov	[NEXTCLUSTER], eax
	;call	RESTFATBYT	; Restore correct cluster 0 value
	;jc	short adc_10

	; 21/11/2025
	mov	eax, [LASTCLUSTER] ; cluster number
	and	eax, eax
	jz	short adc_18
	mov	ebx, [FREECLUSTER] ; content (next cluster)
	call	PACK
	jnc	short adc_18
	mov	eax, [FREECLUSTER]
	sub	ebx, ebx ; 0
	call	PACK	; mark cluster free
	; FAT write error !
	mov	eax, ERR_MISC ; 27 ; miscellaneous/other errors
	stc
	retn
adc_18:
	mov	ebx, [LASTCLUSTER]
	;mov	eax, [NEXTCLUSTER]
	; 19/11/2025
	mov	eax, [FREECLUSTER]
		; EAX = first cluster allocated
; 14/07/2025
%if 0
	or 	ebx, ebx
	jnz	short adc_16	; we were extending an existing file
		; EBX = 0

; We were doing the first allocation for a new file.
; Update the SFT cluster info

dofastk:
	movzx	esi, byte [current_file]
	shl	esi, 2 ; * 4
	mov	[esi+OF_FCLUSTER], eax ; first cluster
	mov	[esi+OF_LCLUSTER], eax ; last cluster
%endif

adc_16:
	retn

; Sorry, we've gone over the whole disk, with insufficient luck. Lets give
; the space back to the free list and tell the caller how much he could have
; had. We have to make sure we remove the "special mark" we put on the last
; cluster we were able to allocate, so it doesn't become orphaned.

adc_17:
	; 19/11/2025
	;mov	eax, [LASTCLUSTER] ; EAX = last cluster of file
	;mov	ebx, -1 ; 0FFFFFFFFh ; cluster content (data)
	;			; last cluster sign
	;call	RELBLKS         ; give back any clusters just alloced
	;call	RESTFATBYT	; Alloc failed.
	;mov	ecx, [CLUSTERS] ; Number of clusters to allocate
	;sub	ecx, [CLUSTCOUNT]
		; ECX = max. no. of clusters that could be added to file
Disk_Full_Return:
        ; MSDOS 6.0
	;mov	byte [DISK_FULL], 1 ; indicating disk full
	; 11/07/2025
	mov	eax, ERR_DISK_SPACE ; 39 ; 'out of volume !'
	stc
        retn

; --------------------------------------------------------------------

; 11/07/2025 - TRDOS 386 v2.0.10

; 19/11/2025
%if 0
RESTFATBYT:
	; 11/07/2025
	; (MSDOS -> RESTFATBYT) - Ref: Retro DOS v5 - ibmdos7.s
	;
	; INPUT:
	;     edx = Logical DOS Drive Description Table address
	;     [FATBYT] = Cluster 0 value
	;
	; OUTPUT:
	;  If cf = 0
	;     NONE (cluster 0 will be updated)
	;
	;  If cf = 1 and eax = error code
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

	;xor	eax, eax ; cluster 0
	;mov	ebx, [FATBYT]
	;call	PACK
	;retn
	;jmp	PACK
	; 11/07/2025
	mov	eax, [FATBYT]
	mov	[CL0FATENTRY], eax
	retn
%endif

; --------------------------------------------------------------------

; 11/07/2025 - TRDOS 386 v2.0.10

RELEASE:
	; (MSDOS -> RELEASE)
RELEASE_nc:	; 21/07/2025
       	xor	ebx, ebx
RELBLKS:
	; 01/01/2026
	; 16/12/2025
	; 13/12/2025
	; 01/12/2025
	; 19/10/2025
	; 17/07/2025
	; 11/07/2025
	; (MSDOS -> RELBLKS) - Ref: Retro DOS v5 - ibmdos7.s
	; Deassign (Deallocate) disk space
	;
	; Frees cluster chain starting with EAX
	;
	; INPUT:
	;     edx = Logical dos drive parameters table address
	;     eax = Cluster in file
	; OUTPUT:
	;  If cf = 0
	;     FAT is updated
	;  If cf = 1 -> eax = error code
	;
	; Modified registers:
	;	eax, ecx, ebx, esi, edi, ebp

; Enter here with EBX=0FFFFFFFFh to put an end-of-file mark
; in the first cluster and free the rest in the chain.

	push	ebx
	push	eax
	call	UNPACK

	pop	ecx
	pop	ebx
        jbe	short rblks_4 ; jna short rblks_4

	mov	[NEXTCLUSTER], eax

	; eax = Content of FAT
	;	for given cluster (next cluster)
	; ebx = -1 -> put eof mark
	;     = 0 -> release (set as free cluster)

	mov	eax, ecx

	push	ebx
	call	PACK
	pop	ebx
	jc	short rblks_4

	or	ebx, ebx
	jnz	short rblks_3 ; Was putting EOF mark

	; 01/01/2026
	xor	ecx, ecx

	cmp	byte [edx+LD_FATType], 2
	jna	short rblks_1 ; not FAT32

	; FAT32 fs
	lea	ebx, [edx+LD_BPB+FAT32_FreeClusters]
	; 01/01/2026
	lea	esi, [edx+LD_BPB+FAT32_FirstFreeClust]
	jmp	short rblks_2

rblks_1:
	; FAT16 or FAT12 fs
	lea	ebx, [edx+LD_BPB+FAT_FreeClusters]
	; 01/01/2026
	lea	esi, [edx+LD_BPB+FAT_FirstFreeClust]

	; 19/10/2025
	; (clear carry flag is needed here for FAT12 fs)
	clc
rblks_2:
	mov	eax, [ebx]
	inc	eax ; -1 -> 0
	;;jz	short NO_DEALLOC ; Free count not valid
	;jz	short rblks_3
	; 01/01/2026
	jz	short rblks_5

	; Increase free count by 1

	mov	[ebx], eax
	movzx	eax, byte [edx+LD_BPB+SecPerClust]
	; 17/07/2025
	add	[edx+LD_FreeSectors], eax

	; 01/01/2026
	inc	ecx
rblks_5:
	mov	eax, [esi]
	inc	eax ; -1 -> 0
	jz	short rblks_6 ; First free cluster not valid

	cmp	eax, [NEXTCLUSTER]
	jna	short rblks_6
	mov	ecx, [NEXTCLUSTER]
	mov	[esi], ecx
	jmp	short rblks_7
rblks_6:
	and	ecx, ecx
	jz	short rblks_3 ; FSINFO not modified
rblks_7:
	; 01/12/2025
	cmp	byte [edx+LD_FATType], 2
	jna	short rblks_3 ; not FAT32
	; 01/12/2025 (Set FSINFO modified flag)
	mov	byte [edx+LD_BPB+BS_FAT32_Reserved1],-1

rblks_3: ; (MSDOS -> NO_DEALLOC)
	mov	eax, [NEXTCLUSTER]
; 16/12/2025
; 13/12/2025
%if 0
	; check for 1
	; is last cluster of incomplete chain
	dec	eax
	jz	short rblks_4
	inc	eax
%endif
	call	IsEOF
        ;jb	short RELEASE
	; 21/07/2025
        jb	short RELEASE_nc
rblks_4:
	retn

; --------------------------------------------------------------------

; 12/07/2025 - TRDOS 386 v2.0.10

;CL0FATENTRY:	dd -1 ;  0FFFFFFFFh

; --------------------------------------------------------------------

; 14/07/2025 - TRDOS 386 v2.0.10

SETDOTENT:
	; 14/07/2025
	; (MSDOS -> SETDOTENT) - Ref: Retro DOS v5 - ibmdos7.s
	; set up a . or .. directory entry for a directory
	;
	; INPUT:
	;     edi = points to the beginning of a directory entry
	;     eax contains ".   " or "..  "
	;     [mkdir_datetime] = date (hw) and time (lw)
	;			 in MSDOS directory entry format
	;     ecx = first cluster
	; OUTPUT:
	;     edi = next directory entry position
	;     eax = 0
	;
	; Modified registers: eax, edi

	; Fill in name field
	stosd	; char 1,2,3,4	; 0 to 3
	mov	eax, 20202020h
	stosd	; char 5,6,7,8	; 4 to 7
	; and Set up attribute
	mov	al, 10h ; attr_directory
	ror	eax, 8
	stosd	; char 9,10,11	; 8 to 10
		; and attr_directory ; 11

	sub	eax, eax
	stosw		; NTReserved, 12
			; CrtTimeTenth, 13

	; Set up last creation time & date
	mov	eax, [mkdir_datetime]
	push	eax
	stosd		; CrtTime, 14
			; CrtDate, 16

	; Set up last access date
	; and first cluster field (hw)
	push	ecx
	ror	ecx, 16
	mov	ax, cx
	ror	eax, 16
	stosd		; LastAccDate, 18
			; FClusterHw, 20
	pop	ecx
	pop	eax
	; Set up last modification time & date
	stosd		; WrtTime, 22
			; WrtDate, 24
	; Set up first cluster field (lw)
	mov	ax, cx
	stosw		; FClusterLw, 26
	; Set up file size (dword) to zero
	xor	eax, eax ; 0
	stosd		; FileSize, 28

	retn

; --------------------------------------------------------------------
