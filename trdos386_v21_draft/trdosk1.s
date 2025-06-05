; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - SYS INIT : trdosk1.s
; ----------------------------------------------------------------------------
; Last Update: 05/06/2025 (Previous: 26/09/2024, v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 04/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ****************************************************************************
; TRDOS2.ASM (c) 2004-2011 Erdogan TAN [ 17/01/2004 ] Last Update: 09/11/2011
;

sys_init:
	; 08/05/2025
	; 17/04/2025 (TRDOS 386 v2.0.10)
	; 26/09/2024 (TRDOS 386 v2.0.9)
	; 18/04/2021 (TRDOS 386 v2.0.4)
	; 20/01/2018  (v2.0.1)
	; 23/01/2017  (v2.0.0)
	; 07/05/2016
	; 02/05/2016
	; 24/04/2016
	; 14/04/2016
	; 13/04/2016
	; 30/03/2016
	; 24/01/2016
	; 06/01/2016
	; 04/01/2016 (TRDOS 386 v2.0 - Beginning)

	; 23/01/2017 - reset timer frequency (to 18.2Hz)
	mov	al, 00110110b ; 36h
 	out	43h, al
	xor	eax, eax  ; sub	al, al ; 0
	out	40h, al ; LB
	out	40h, al ; HB
 	;
	; 30/03/2016
	; Clear Logical DOS Disk Description Tables Area
	;xor	eax, eax
	mov	edi, Logical_DOSDisks
	mov	ecx, 6656/4 ; 26*256 = 6656 bytes
	rep	stosd ; 1664 times 4 bytes

	mov	eax, '?:/'
	mov	[Current_Dir_Drv], eax

	; 08/05/2025
	; 17/04/2025 - TRDOS 386 v2.0.10
	call	set_buffers

	; Logical DRV INIT (only for hard disks)
	call 	ldrv_init  ; trdosk2.s

	; When floppy_drv_init call is disabled
	; media changed sign is needed
	; for proper drive initialization

	mov 	esi, Logical_DOSDisks
	mov 	al, 1 ; Initialization sign (invalid_fd_parameter)
	add 	esi, LD_MediaChanged ; Media Change Status = 1 (init needed)
	mov 	[esi], al ; A:
	; Temporary - 26/09/2024
	;;mov	dword [esi+LD_Clusters], -1 ; *
	;dec	dword [esi+LD_Clusters]
	add 	esi, 100h 
	mov 	[esi], al ; B:
	;;mov	dword [esi+LD_Clusters], -1 ; *
	;dec	dword [esi+LD_Clusters]

_current_drive_bootdisk:
	mov 	dl, [boot_drv] ; physical drive number
	cmp 	dl, 0FFh
	je 	short _last_dos_diskno_check
_boot_drive_check:
	cmp 	dl, 80h
	jb 	short _current_drive_a
	sub 	dl, 7Eh ; C = 2 , D = 3
	jmp 	short _current_drive_a

_last_dos_diskno_check:
	mov 	dl, [Last_DOS_DiskNo]
	cmp 	dl, 2
	ja 	short _current_drive_c
	je 	short _current_drive_a
	xor 	dl, dl ; A:
	jmp 	short _current_drive_a

_current_drive_c:
	mov 	dl, 2 ; C:

_current_drive_a:
	mov	[drv], dl
        mov     esi, msg_CRLF_temp
	call 	print_msg

	mov	dl, [drv]
_default_drive_c:
	call 	change_current_drive
	jnc 	short _start_mainprog

_drv_not_ready_error: 
	mov 	esi, msgl_drv_not_ready
	call 	print_msg
        ;jmp	_end_of_mainprog

	; 20/01/2018
	mov	dl, 2
	cmp	[drv], dl
	jnb	short _end_of_mainprog
	mov	[drv], dl
	jmp	short _default_drive_c

_start_mainprog:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 18/04/2021 (TRDOS 386 v2.0.4 - Beginning)
	; 07/01/2017
	; 07/05/2016
	; 02/05/2016
	; 24/04/2016 (TRDOS 386 v2)
	; Retro UNIX 386 v1, 'sys_init' (u0.s)
	; 23/06/2015

	; 02/05/2016
	; 24/04/2016
	;mov	ax, 1
	; 18/04/2021 - TRDOS 386 v2.0.4
	xor	eax, eax
	inc	al  ; eax = 1
	mov	[u.uno], al
	mov	[mpid], ax
	mov	[p.pid], ax
	mov	[p.stat], al
	mov	byte [u.quant], time_count  ; 07/01/2017
	;
	mov	eax, [k_page_dir]
	mov	[u.pgdir], eax ; reset
	;
	call	allocate_page
	;jc	panic
	; 25/07/2022
	jnc	short _start_mainprog_1
	jmp	panic

_start_mainprog_1:
	mov	[u.upage], eax ; user structure page
	mov	[p.upage], eax
	call	clear_page
	;
	; 24/08/2015
	dec 	byte [sysflg] ; FFh = ready for system call
			      ; 0 = executing a system call
	; 13/04/2016
	; Clear Environment Variables Page/Area
	mov	edi, Env_Page ; 93000h
	mov	ecx, Env_Page_Size / 4	; 512/4  (4096/4)
	xor	eax, eax
	rep	stosd

	; 14/04/2016
 	call	mainprog_startup_configuration

        call    dos_prompt

_end_of_mainprog:
        mov     esi, msg_CRLF_temp
	call 	print_msg
	mov 	esi, mainprog_Version
	call 	print_msg
	; 24/01/2016
	sub	ah, ah
	call	int16h ; call getch
	jmp	cpu_reset

infinitiveloop: jmp short infinitiveloop

print_msg:
	; 13/05/2016
	; 04/01/2016
	; 01/07/2015
	; 13/03/2015 (Retro UNIX 386 v1)
	; 07/03/2014 (Retro UNIX 8086 v1)
	; (Modified registers: EAX, EBX, ECX, EDX, ESI, EDI)
	;
	mov	bh, [ACTIVE_PAGE] ; 04/01/2016 (ptty)
	;mov	bl, 07h ; Black background, light gray forecolor

	lodsb
pmsg1:
	push 	esi
	;mov	bh, [ACTIVE_PAGE] ; 04/01/2016 (ptty)
	mov	bl, 07h ; Black background, light gray forecolor
	call 	_write_tty
	pop	esi
	lodsb
	and 	al, al
	jnz 	short pmsg1
	retn

clear_screen:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 06/12/2020 
	; 03/12/2020 (TRDOS 386 v2.0.3)
	; 13/05/2016
	; 30/01/2016
	; 24/01/2016
	; 04/01/2016
	movzx	ebx, byte [ACTIVE_PAGE] ; video page number (0 to 7)
	; 25/07/2022
	;sub	al, al
	; al = 0
	mov 	ah, [ebx+vmode] ; default = 03h (80x25 text)
	cmp	ah, 4
	jb	short cls1
	cmp	ah, 7
	jne	short vga_clear
cls1:
	;mov	bh, bl
	;mov	bl, 7
	cmp	ah, [CRT_MODE] ; current video mode ? 
	je	short cls2 ; yes (current video mode = 3)
	;;call	set_mode_3 ; set video mode to 3 (& clear screen)
	;;retn
	; 06/12/2020
	;cmp	byte [pmi32], 0
	; 25/07/2022
	cmp	[pmi32], bh ; 0
	ja	short vga_clear
	jmp	set_mode_3
cls2:
	mov	bh, bl ; video page (0 to 7)
	mov	bl, 07h ; attribute to be used on blanked line
	; 25/07/2022
	;sub 	al, al ; 0 = entire window
	;xor 	cx, cx
	; 25/07/2022
	; al = 0
	xor	ecx, ecx
	mov 	dx, 184Fh
	call	_scroll_up ; 24/01/2016
	;
	;mov	bh, [ACTIVE_PAGE] ; video page number (0 to 7)
	;xor 	dx, dx
	; 25/07/2022
	xor	edx, edx
	;call	_set_cpos ; 24/01/2016 
	;;retn
	; 03/12/2020
	jmp	_set_cpos ; returns to the caller of this proc
;cls3:
;	retn

	; 06/12/2020
vga_clear:
	; 03/12/2020
	; set mode by using _int10h
	; (also clears screen)
	;mov	al, ah
	;sub	ah, ah  ; set current video mode
	; 25/07/2022
	xchg	ah, al
	; ah = 0
	; al = video mode
	;call	_int10h ; simulates int 10h in TRDOS 386 kernel
	;jmp	short cls3
	jmp	_int10h ; returns to the caller of this proc

panic:
	; 13/05/2016 (TRDOS 386 = TRDOS v2)
	; 13/03/2015 (Retro UNIX 386 v1)
	; 07/03/2014 (Retro UNIX 8086 v1)
	mov 	esi, panic_msg
	call 	print_msg
key_to_reboot:
        ; 24/01/2016
        sub     ah, ah
        call    int16h	; call getch
        ; wait for a character from the current tty
	;
	mov	al, 0Ah
	mov	bh, [ptty] ; [ACTIVE_PAGE]
	mov	bl, 07h ; Black background, 
			; light gray forecolor
	call 	_write_tty
	jmp	cpu_reset

ctrlbrk:
	; 21/08/2024 (TRDOS 386 Kernel v2.0.9)
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 12/11/2015
	; 13/03/2015 (Retro UNIX 386 v1)
	; 06/12/2013 (Retro UNIX 8086 v1)
	;
	; INT 1Bh (control+break) handler
	;
      	; Retro Unix 8086 v1 feature only!
      	;

	; 25/07/2022
	push	edx
	xor	edx, edx

; 21/08/2024 - TRDOS 386 v2.0.9
%if 0
	;cmp 	word [u.intr], 0
	cmp	[u.intr], dx ; 0
	jna 	short cbrk4
cbrk0:
	; 12/11/2015
	; 06/12/2013
	;cmp 	word [u.quit], 0
	cmp	[u.quit], dx ; 0 ; 25/07/2022
	jz	short cbrk4
%endif
	; 20/09/2013
	;push 	ax
	push	eax ; 25/07/2022
	mov	al, [ptty]

; 21/08/2024 - TRDOS 386 v2.0.9
%if 1
	cmp	al, [u.ttyn]
	jne	short cbrk3
%else
	; 12/11/2015
	;
	; ctrl+break (EOT, CTRL+D) from serial port
	; or ctrl+break from console (pseudo) tty
	; (!redirection!)

	cmp	al, 8 ; serial port tty nums > 7
        jb      short cbrk1 ; console (pseudo) tty

	; Serial port interrupt handler sets [ptty]
	; to the port's tty number (as temporary).
	;
	; If active process is using a stdin or
	; stdout redirection (by the shell),
        ; console tty keyboard must be available
	; to terminate running process,
	; in order to prevent a deadlock.

	; 25/07/2022
	;push	edx
	;movzx	edx, byte [u.uno]
	mov	dl, [u.uno]
	cmp     al, [edx+p.ttyc-1] ; console tty (rw)
	;pop	edx
	je	short cbrk2
cbrk1:
	inc 	al  ; [u.ttyp] : 1 based tty number
	; 06/12/2013
	cmp	al, [u.ttyp]   ; recent open tty (r)
	je	short cbrk2
        cmp     al, [u.ttyp+1] ; recent open tty (w)
	jne	short cbrk3
cbrk2:
	;; 06/12/2013
	;mov	ax, [u.quit]
	;and	ax, ax
	;jz	short cbrk3
%endif
	;xor	ax, ax ; 0
	;dec	ax
	; 0FFFFh = 'ctrl+brk' keystroke
	; 25/07/2022
	;xor	eax, eax ; 0
	;dec	eax ; -1 ; 0FFFFFFFFh
	;mov	[u.quit], ax
	; 21/08/2024
	; set CTRL+BREAK flag (even if it is not activated)
	; (u.intr is it's activation flag, 0 = disabled))
	mov	word [u.quit], -1 ; 0FFFFh
cbrk3:
	;pop	ax
	pop	eax ; 25/07/2022
cbrk4:
	; 25/07/2022
	pop	edx
	retn

; 31/12/2017
; TRDOS 386 - 30/12/2017
%define get_rtc_date RTC_40
%define get_rtc_time RTC_20
%define	set_rtc_date RTC_50
%define set_rtc_time RTC_30
get_rtc_date_time:
; Retro UNIX 8086 v1 - UNIX.ASM (01/09/2014)
;epoch:
	; 18/04/2021 (TRDOS 386 v2.0.3)
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 09/04/2013 (Retro UNIX 8086 v1 - UNIX.ASM)
	; 'epoch' procedure prototype: 
	; 	            UNIXCOPY.ASM, 10/03/2013
	; 14/11/2012
	; unixboot.asm (boot file configuration)
	; version of "epoch" procedure in "unixproc.asm"
	; 21/7/2012
	; 15/7/2012
	; 14/7/2012
	; Erdogan Tan - RETRO UNIX v0.1
	; compute current date and time as UNIX Epoch/Time
	; UNIX Epoch: seconds since 1/1/1970 00:00:00
	;
        ;  ((Modified registers: EAX, EDX, ECX, EBX))
	;

	; 18/04/2021
	; INPUT:
	;	none (real time clock)
	; OUTPUT:
	;	eax = unix epoch time value
	;	    (seconds since 1/1/1970 00:00:00)

	call 	get_rtc_time		; Return Current Time
        ;xchg 	ch, cl ; 18/04/2021
        mov 	[hour], cx    ; BCD, cl = minute, ch = hour
        ;xchg 	dh, dl ; 18/04/2021
	;mov 	[second], dx  ; BCD, dh = second, dl = dse
	; 18/04/2021
	mov	[second], dh  ; second
	;
        call 	get_rtc_date		; Return Current Date
        ;xchg 	ch, cl ; 18/04/2021
        mov 	[year], cx    ; BCD, cl = year, ch = century
        ;xchg 	dh, dl ; 18/04/2021
        mov 	[month], dx   ; BCD, dl = day, dh = month
	;
	;mov 	al, [hour]    ; Hour
        ; 18/04/2021
	mov	al, [hour+1]  ; Hour
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	;mov 	[hour], al
	;mov 	al, [hour+1]  ; Minute
	xchg	al, [hour]    ; [hour] = hour, al = minute
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	[minute], al
	mov 	al, [second]  ; Second
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	mov 	[second], al
	mov 	ax, [year]    ; Year (century)
	; 18/04/2021
	;push 	eax ; puhs ax
	;mov	al, ah ; century ; 18/04/2021
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	;mov 	ah, 100
	;mul 	ah
	;mov 	[year], ax
	; 18/04/2021
	; ax = al = year (0 to 99)
	xchg	ax, [year]    ; [year+1] = century -> ah
	;pop	eax ; pop ax
	mov	al, ah  ; century
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	; 18/04/2021
	mov	ah, 100	      ; 100*(century byte of year)
	mul	ah
	;
	add 	[year], ax
	;mov 	al, [month]   ; Month
	; 18/04/2021
	mov	al, [month+1] ; Month
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
	;mov 	[month], al
        ;mov	al, [month+1] ; Day
	; 18/04/2021
	xchg	al, [month]   ; [month] = month, al = day
	   	; AL <-- BCD number
        db 	0D4h, 10h		; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
        aad 	; AX= AH*10+AL
        mov     [day], al

	retn	; 30/12/2017

epoch:
	call	get_rtc_date_time ; TRDOS 386 - 30/12/2017

convert_to_epoch:
	; 25/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 31/12/2017 (TRDOS 386 = TRDOS v2.0)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit modification)
	; 09/04/2013 (Retro UNIX 8086 v1)
	;
	; ((Modified registers: EAX, EDX, EBX))
	;
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998
	sub 	eax, eax
	mov 	ax, [year]
	sub 	ax, 1970
	mov 	edx, 365
	mul 	edx
	xor 	ebx, ebx
	mov 	bl, [month]
	dec 	bl
	shl 	bl, 1
	;sub	edx, edx
	mov 	dx, [EBX+DMonth]
        mov     bl, [day]
	dec 	bl
	add 	eax, edx
	add 	eax, ebx
			; EAX = days since 1/1/1970
	mov 	dx, [year]
	sub 	dx, 1969
	;shr 	dx, 1
	;shr 	dx, 1
	; 25/07/2022
	shr	edx, 2
		; (year-1969)/4
	add 	eax, edx
			; + leap days since 1/1/1970
	cmp 	byte [month], 2	; if past february
	jna 	short cte1
	mov 	dx, [year]
	and 	dx, 3 ; year mod 4
	jnz 	short cte1
			; and if leap year
	;add 	eax, 1 	; add this year's leap day (february 29)
	; 25/07/2022
	inc	eax
cte1: 			; compute seconds since 1/1/1970
	mov 	edx, 24
	mul	edx
	mov 	dl, [hour]
	add 	eax, edx
		; EAX = hours since 1/1/1970 00:00:00
	;mov	ebx, 60
	mov	bl, 60
	mul	ebx
	mov 	dl, [minute]
	add 	eax, edx
		; EAX = minutes since 1/1/1970 00:00:00
	;mov 	ebx, 60
	mul	ebx
	mov 	dl, [second]
	add 	eax, edx
 		; EAX -> seconds since 1/1/1970 00:00:00
	retn

;set_date_time:
convert_from_epoch:
	; 25/07/2022 (v2.0.5)
	; 18/04/2021 (v2.0.4)
	; 31/12/2017 (v2.0.0)
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 20/06/2013 (Retro UNIX 8086 v1)
	; 'convert_from_epoch' procedure prototype:
	; 	            UNIXCOPY.ASM, 10/03/2013
	;
	; ((Modified registers: EAX, EDX, ECX, EBX))
	;
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998
	;
	; INPUT:
	; EAX = Unix (Epoch) Time
	;
	xor 	edx, edx
	;mov 	ecx, 60
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 60
	div	ecx
	;mov 	[imin], eax   ; whole minutes
			  ; since 1/1/1970
	mov 	[second], dx  ; leftover seconds
	sub 	edx, edx
	div	ecx
	;mov 	[ihrs], eax   ; whole hours
	;		      ; since 1/1/1970
	mov 	[minute], dx  ; leftover minutes
	xor	edx, edx
	;mov 	cx, 24
	mov 	cl, 24
	div	ecx
	;mov 	[iday], ax   ; whole days
			     ; since 1/1/1970
	mov 	[hour], dx   ; leftover hours
	add 	eax, 365+366 ; whole day since
			     ; 1/1/1968
	;mov 	[iday], ax
	push 	eax
	sub	edx, edx
	;mov 	ecx, (4*365)+1 ; 4 years = 1461 days
	; 25/07/2022
	mov	cx, (4*365)+1
	div	ecx
	pop 	ecx
	;mov 	[lday], ax   ; count of quadyrs (4 years)
	;push 	dx
	; 18/04/2021
	push	edx
	;mov 	[qday], dx   ; days since quadyr began
	cmp 	dx, 31+29    ; if past feb 29 then
	cmc		     ; add this quadyr's leap day
	adc 	eax, 0	     ; to # of qadyrs (leap days)
	;mov 	[lday], ax   ; since 1968
	;mov 	cx, [iday]
	xchg 	ecx, eax     ; ECX = lday, EAX = iday
	sub 	eax, ecx     ; iday - lday
	;mov 	ecx, 365
	; 25/07/2022
	mov	cx, 365
	xor	edx, edx
	; EAX = iday-lday, EDX = 0
	div	ecx
	;mov 	[iyrs], ax   ; whole years since 1968
	;jday = iday - (iyrs*365) - lday
	;mov	[jday], dx   ; days since 1/1 of current year
	;add	eax, 1968
	add 	ax, 1968     ; compute year
	mov 	[year], ax
	;mov 	cx, dx
	; 25/07/2022
	mov	ecx, edx
	;;mov 	dx, [qday]
	;pop 	dx
	; 18/04/2021
	pop	edx
	cmp 	dx, 365	     ; if qday <= 365 and qday >= 60
	ja 	short cfe1   ; jday = jday +1
	cmp 	dx, 60       ; if past 2/29 and leap year then
        cmc		     ; add a leap day to the # of whole
	;adc 	cx, 0        ; days since 1/1 of current year
	; 25/07/2022
	adc	ecx, 0
cfe1:
	;mov 	[jday], cx
	;mov 	bx, 12       ; estimate month
	; 18/04/2021
	sub	ebx, ebx
	mov	bl, 12
	mov 	dx, 366      ; mday, max. days since 1/1 is 365
	and 	ax, 11b      ; year mod 4 (and dx, 3)
cfe2:	; Month calculation  ; 0 to 11  (11 to 0)
	;cmp 	cx, dx       ; mday = # of days passed from 1/1
	; 25/07/2022
	cmp	ecx, edx
	jnb 	short cfe3
	;dec 	bx           ; month = month - 1
	;shl 	bx, 1
	; 18/04/2021
	dec	bl
	shl	bl, 1 
	mov 	dx, [EBX+DMonth] ; # elapsed days at 1st of month
	; 18/04/2021
	;shr 	bx, 1        ; bx = month - 1 (0 to 11)
	shr	bl, 1
	;cmp	bx, 1        ; if month > 2 and year mod 4  = 0	
	cmp	bl, 1
	jna 	short cfe2   ; then mday = mday + 1
	jna 	short cfe2   ; then mday = mday + 1
	or 	al, al       ; if past 2/29 and leap year then
	jnz 	short cfe2   ; add leap day (to mday)
	;inc 	dx           ; mday = mday + 1
	; 25/07/2022
	inc	edx
	jmp 	short cfe2
cfe3:
	;inc 	bx	     ; -> bx = month, 1 to 12
	; 18/04/2021
	inc	bl
	mov 	[month], bx
	;sub 	cx, dx	     ; day = jday - mday + 1
	; 25/07/2022
	sub	ecx, edx
	;inc 	cx
	; 18/04/2021
	inc	cl
	;mov 	[day], cx
	mov	[day], cl

	; eax, ebx, ecx, edx is changed at return
	; output ->
	; [year], [month], [day], [hour], [minute], [second]

	retn	; 31/12/2017 (TRDOS 386)

set_rtc_date_time:
	; 31/12/2017 (v2.0.0)
	; 30/12/2017 (TRDOS 386)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 20/06/2013 (Retro UNIX 8086 v1)
	call	set_date_bcd
	; Set real-time clock date
	call	set_rtc_date ; RTC_50
	; Set real-time clock time
	call	set_time_bcd
	jmp	set_rtc_time ; RTC_30

; 31/12/2017
set_date_bcd:
        mov     al, [year+1]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	ch, al ; century (BCD)
	mov 	al, [year]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	cl, al ; year (BCD)
        mov 	al, [month]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	dh, al ; month (BCD)
	mov 	al, [day]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h, 10h    ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	; 18/04/2021
	mov 	dl, al ; day (BCD)
	retn	; 30/12/2017

; 31/12/2017
set_time_bcd:
        ; Read real-time clock time 
	; (get day light saving time bit status)
 	cli
	call	UPD_IPR 		; CHECK FOR UPDATE IN PROCESS
	; cf = 1 -> al = 0
        jc      short stime1
	mov	al, CMOS_REG_B		; ADDRESS ALARM REGISTER
	call	CMOS_READ		; READ CURRENT VALUE OF DSE BIT
stime1:
	sti
	and	al, 00000001b		; MASK FOR VALID DSE BIT
	mov	dl, al			; SET [DL] TO ZERO FOR NO DSE BIT
	; DL = 1 or 0 (day light saving time)
	
	mov 	al, [hour]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h,10h     ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	ch, al ; hour (BCD)
        mov     al, [minute]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h,10h     ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	cl, al       ; minute (BCD)
        mov     al, [second]
	aam 	; ah = al / 10, al = al mod 10
	db 	0D5h,10h     ; Undocumented inst. AAD
			     ; AL = AH * 10h + AL
	mov 	dh, al	     ; second (BCD)
	retn	; 30/12/2017

; ----------------------------------------------------------------------

	; 05/06/2025
	; 03/05/2025
	; 17/04/2025 - TRDOS 386 v2.0.10
	; ref: Retro DOS v5.0 (PCDOS 7.1)
set_buffers:
	; input: none
	; output: buffers Queue established
	mov	edi, BUFFERS
	mov	[BufferQueue], edi		; head of Buff Q

	; 05/06/2025
	mov	[FIRST_BUFF_ADDR], edi
	mov	ecx, -1
	mov	[LastBuffer], ecx ; -1
	;xor	ecx, ecx ; 0
	inc	ecx ; 0
	mov	[DirtyBufferCount], ecx ; 0	; set dirty_count to 0.
	mov	[buf_prev_off], ecx ; 0

	;mov	ecx, nbuf			; number of buffers
	mov	cl, nbuf
	mov	edx, BUFINSIZ + 512 ; 536	; space for one buffer
nxt_buff:
	call	set_buffer_info 		; set buf_link,buf_id...
	loop	nxt_buff
	retn

	; 17/04/2025 - TRDOS 386 v2.0.10
	; ref: Retro DOS v5.0 (PCDOS 7.1)
set_buffer_info:
	; function: set buf_link,buf_id,buf_sector
	mov	ebx, [buf_prev_off]
	mov	[edi+BUFFINFO.buf_prev], ebx
	mov	eax, edi
	add	eax, edx
	;mov	[edi+BUFFINFO.buf_next], eax
	mov	[edi], eax
	mov	[buf_prev_off], edi
	mov	edi, eax
	mov	dword [edi+BUFFINFO.buf_ID], 00FFh  ; new buffer free
	mov	dword [edi+BUFFINFO.buf_sector], 0
	retn
