; ****************************************************************************
; dow.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'dow.prg')
; ----------------------------------------------------------------------------
; DOW.PRG ! "Day of Week" - 'systime' TEST program for TRDOS 386 !
;
; 07/05/2026
;
; [ Last Modification: 07/05/2026 ]
;
; ****************************************************************************
; nasm dow4.asm -l dow4.txt -o DOW4.PRG

; 30/04/2026
; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45
_stdio	equ 46

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
    ; 03/09/2015
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.
    %if %0 >= 2
        mov ebx, %2
        %if %0 >= 3
            mov ecx, %3
            %if %0 = 4
               mov edx, %4
            %endif
        %endif
    %endif
    mov eax, %1
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)
%endmacro

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & date)
	jb	short sysdate ; use sistem date
	lodsd ; program file name address
	lodsd ; epoch string address

	mov	byte [flag], 1

	; EAX = arg2 ; epoch (text)
	mov	esi, eax
	call	str_to_epoch
	jnc	short p_date

	mov	esi, usage
	call	print_msg

	jmp	terminate

sysdate:
	; Get Date&Time in Unix/Epoch format
	sys	_time, 0

p_date:
	; EAX = Unix (Epoch) Time Ticks/Seconds
	push	eax
	call	dword_to_hex
	mov	esi, epoch_txt
	call	print_msg
	pop	eax

	call	convert_from_epoch

	push	dword [day]
	push	dword [month]
	push	dword [year]

	call	DayOfWeek

	add	esp, 12

	mov	[dow], eax

	cmp	byte [flag], 0
	ja	short skip_header

	mov	esi, header
	call	print_msg

skip_header:
	mov	esi, newline
	call	print_msg

	mov	edi, txt_date

	mov	eax, [day]
	call	bin_to_str_2

	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [month]
	call	bin_to_str_2

	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [year]
	call	bin_to_str_4

	mov	byte [edi],0

	mov	esi, txt_date
	call	print_msg

	mov	esi, _space
	call	print_msg

	mov	ebx, [dow]
	shl	ebx, 2	; * 4
	add	ebx, day_names
	mov	esi, [ebx]
	call	print_msg

	mov	esi, newline
	call	print_msg

	mov	edi, txt_time

	mov	eax, [hour]
	call	bin_to_str_2

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [minute]
	call	bin_to_str_2

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [second]
	call	bin_to_str_2

	mov	byte [edi],0

	mov	esi, txt_time
	call	print_msg

	mov	esi, newline
	call	print_msg

terminate:
	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

;-----------------------------------------------------------------

	; 06/05/2026
bin_to_str_4:
	mov	esi, 4
	jmp	short bin_to_str
bin_to_str_2:
	mov	esi, 2
bin_to_str:
	xor	ecx, ecx ; 0
	mov	ebp, esp
	mov	ebx, 10
bin2str_div:
	xor	edx, edx
	div	ebx
	add	dl, '0'
	push	edx
	inc	ecx
	;cmp	eax, 0
	;ja	short bin2str_div
	and	eax, eax
	jnz	short bin2str_div

	mov	ebx, esi
	add	esi, edi
	sub	ebx, ecx
	jng	short skip_zero_prefix
	; eax = 0
	add	al,'0'
zero_prefix:
	stosb
	dec	ebx
	jnz	short zero_prefix

skip_zero_prefix:
pop_next:
	pop	eax
	cmp	edi, esi
	jnb	short skip_stosb
	stosb
skip_stosb:
	cmp	esp, ebp
	jb	short pop_next

bin2str_ok:
	retn

;-----------------------------------------------------------------

print_msg:
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
print_msg_@:
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------

	; ref: TRDOS 386 v2.0.11 Kernel - 'trdosk1.s'
convert_from_epoch:
	; 07/05/2026 (v2.0.11)
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
	;;;;
	; 07/05/2026
	; ref: Google AI
	; 2100 Leap Year Correction (Patch)
	; ((The year 2100 is not a leap year
	; because it is not divisible by 400.))
	cmp	ax, 2100
	jb	short cfe_fix_skip
	inc	edx
cfe_fix_skip:
	;;;;
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

;-----------------------------------------------------------------

	; 07/05/2026 - Erdogan Tan
str_to_epoch:
	; esi = unix epoch -numeric- string (digits)
	xor	eax, eax
	mov	ecx, 10	; max. 10 digits
get_digit:
	lodsb
	cmp	al, '0'
	jb	short chk_eol
	cmp	al, '9'
	ja	short fail
	sub	al, '0'
	push	eax
	mov	eax, 10
	mul	dword [epoch]
	mov	[epoch], eax
	pop	eax
	add	dword [epoch], eax
	jc	short overflow	
	loop	get_digit

	lodsb
chk_eol:
	and	al, al
	jnz	short fail

	mov	eax, [epoch]
	retn
fail:
	stc
overflow:
	retn

;-----------------------------------------------------------------

dword_to_hex:
	; eax = binary number
	mov	ebx, eax
	mov	ecx, 8
	mov	edi, epoch_hex
dd2hex:
	rol	ebx, 4
	mov	dl, bl
	and	edx, 15
	add	edx, hex_digits
	mov	al, [edx]
	stosb
	loop	dd2hex	
	retn

hex_digits:
	db '0123456789ABCDEF'

;-----------------------------------------------------------------

DMonth:
	dw 0
	dw 31
	dw 59
	dw 90
	dw 120
	dw 151
	dw 181
	dw 212
	dw 243
	dw 273
	dw 304
	dw 334

;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

; Work out what the day of the week is for a given date
; Speed optimized x86 assembly (PIII) by Lingo
;
; C Algorithm by Tomohiko Sakamoto
; int dayofweek(int d, int m, int y) {
;   static int t[] = { 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 };
;   y -= m < 3;
;   return ( y + y/4 - y/100 + y/400 + t[m-1] + d) % 7;
; }
; year  = [0, 9999+]
; month = [1, 12]
; day   = [1, 31]

;DayOfWeekL  PROTO year:DWORD, month:DWORD, day:DWORD

DayOfWeek:
  mov  ecx, [esp+8]               ; ecx=month
  mov  edx, [esp+4]               ; edx=year
  cmp  ecx, 3                     ; test if month is < 3
  sbb  edx, 0                     ; if month < 3 then dec edx->years
  mov  ecx, [monthdata + ecx*4-4] ; ecx-> monthdata[m-1]
  mov  eax, edx                   ; eax=edx=years
  imul edx, 0A3D8h                ; dividing edx=years by 100 == imul edx, 0A3D8h / shr edx, 22
  add  ecx, [esp+12]              ; add day; ecx= monthdata[m-1] + day
  add  ecx, eax                   ; ecx-> monthdata[m-1] + day + years
  shr  eax, 2                     ; eax= years/4
  add  eax, ecx                   ; eax= monthdata[m-1] + day + years + years/4
  ;mov ecx, [esp]                 ; ecx->return address
  ;mov [esp+12], ecx              ; ecx->return address to right place
  shr  edx, 22                    ; end of dividing ; edx=years/100
  sub  eax, edx                   ; eax= monthdata[m-1] +day + years + years/4 - years/100
  shr  edx, 2                     ; edx=years/400
  add  eax, edx                   ; eax= monthdata[m-1] +day + years + years/4 - years/100 + years/400
  mov  edx, 24924925h             ; dividing eax by 7 == mul edx, eax ; 24924925h=dword (1/7 * 2^35)
  mov  ecx, eax                   ; ecx= monthdata[m-1]  + day + years + years/4 - years/100 + years/400
  mul  edx                        ; dividing eax by 7 ->result in edx
  mov  eax, ecx                   ; eax= monthdata[m-1] + day + years + years/4 - years/100 + years/400
  lea  ecx, [edx+edx*2]           ; multiplying edx by 7 ->
  lea  edx, [ecx+edx*4]           ; ->result in edx
  ;add  esp, 3*4                  ; clearing the stack from 3 dword parameters
  sub  eax, edx                   ; eax= (monthdata[m-1] + day + years + years/4 - years/100 + years/400) % 7
  retn                            ; faster return then return 3*4

;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

monthdata: dd 0,3,2,5,0,3,5,1,4,6,2,4

;-----------------------------------------------------------------

usage:
	db "TRDOS 386 v2 'systime' system call test program", 0Dh, 0Ah
	db  "by Erdogan Tan [May 2026]", 0Dh, 0Ah
	db  0Dh, 0Ah
	db  "Usage: dow unix_epoch_time_digits", 0Dh, 0Ah, 0 

epoch_txt:
	db  0Dh, 0Ah
	db  "Epoch: "
epoch_hex:
	dd  0
	dd  0
	db "h"
	db 0Dh, 0Ah, 0				

	 db 0
header:	 db 0Dh, 0Ah
	 db "Current Date&Time: "
newline: db 0Dh,0Ah,0
_space:	 db " ",0

_sunday:   db "SUNDAY",0
_monday:   db "MONDAY",0
_tuesday:  db "TUESDAY",0
_wednesday: db "WEDNESDAY",0
_thursday: db "THURSDAY",0
_friday:   db "FRIDAY",0
_saturday: db "SATURDAY",0

flag:	db 0	; epoch input flag

align 4

day_names:
	dd _sunday
	dd _monday
	dd _tuesday
	dd _wednesday
	dd _thursday
	dd _friday
	dd _saturday

year:	dd 0
month:	dd 0
day:	dd 0
hour:	dd 0
minute: dd 0
second: dd 0
dow:	dd 0

epoch:	dd 0

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

epoch_str:
	resb 10

txt_date:
	resb 12
txt_time:
	resb 10
