; ****************************************************************************
; calendar.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'calendar.prg')
; ----------------------------------------------------------------------------
; ! TEST program !
;
; 04/05/2026 - Erdogan Tan
;
; ****************************************************************************
; ref: "armando.asm", by Armando Aguirre, 2017 (GitHub)
	;Project 1:Calender
	;Spring 2017
	;(Tasm syntax)

	; Nasm syntax conversion: Erdogan Tan - May 2026

	bits 32
	org 0x0
start:
	mov	ah,01h
	mov	cx,2607h ; hide our cursor
	int	31h	 ; int 10h

	; Map video buffer (0B8000h) to user memory (same addr)
	;sys	_video, 0400h
	mov	ebx, 0400h
	mov	eax, 31 ; _video
	int	40h	; TRDOS 386 system call
	
axz:	; our loop random name to start
	cmp	dword [fort],1
	je	axt
	cmp	dword [port],1
	je	axt
 	jmp	clean ;we clean our screen first	

artl2:	; we will now draw the dates of the month	
	jmp drawmonth ; we draw our month
	; cleaning my screen
clean:
	mov	ecx,4000
	mov	eax,7520h
	mov	edi,0B8000h
	rep	stosw

	jmp	axd

axt:	; now we draw our first month name
genames: 
	mov	ebx,[month]
	cmp	bl,6
	je	name6
	ja	genames@
	cmp	bl,1
	je	name1
	cmp	bl,2
	je	name2
	cmp	bl,3
	je	name3
	cmp	bl,4
	je	name4
	jmp	name5
genames@:
	cmp	bl,7
	je	name7
	cmp	bl,8
	je	name8
	cmp	bl,9
	je	name9
	cmp	bl,10
	je	name10
	cmp	bl,11
	je	name11
	jmp	name12

bgenames:
artl:	; first we will figure out how many dates the month has
aac:	
	jmp	figdays ; another jmp that needed jmp help,

axd:	; we draw our skeleton edges,year,and line number and those stuff
skel:
	mov	ebx,0B8000h
tp:
	mov	word [ebx],75C4h ; need word bcs byte is not big
	mov	byte [ebx+1],70h 
	mov	word [ebx+3840],75C4h
	mov	byte [ebx+3841],70h
	add	ebx,2	; increase our value
	cmp	ebx,0B80A0h ; 160 is the last value of our screen
	jne	tp
	mov	word [0B8000h],75DAh ; left corner
	mov	byte [0B8001h],70h 
	mov	word [0B809Eh],75BFh ; right corner
	mov	byte [0B809Fh],70h
skelpart2: ; here we are to part 2
	mov	ebx,0B8000h
op:
	mov	word [ebx],75B3h
	mov	byte [ebx+1],70h 
	mov	word [ebx+158],75B3h
	mov	byte [ebx+159],70h 
	add	ebx,160
	cmp	ebx,0B8F00h ; 0B8000h+3840
	jne	op
	mov	esi,0B82B6h ; 0B8000h+694	
	mov	edi,0B82EEh ; 0B8000h+750
	sub	ebx,ebx
upok:	;drawing the lines
ok:
	mov	byte [esi],'_'
	mov	byte [esi+1],71h
	mov	byte [esi+2],'_'
	mov	byte [esi+3],71h
	add	esi,8
	cmp	esi,edi
	jne	ok
	sub	esi,56
	add	esi,320
	add	edi,320
	inc	ebx
	cmp	ebx,6
	jne	upok	
	mov	esi,0B8176h ; 0B8000h+374
	sub	ebx,ebx ; printing our name days mo tu..etc
pwq:	
	mov	al,[ebx+namedays]
	mov	[esi],ax
	mov	byte [esi+1],71h
	add	esi,2
	inc	ebx
	cmp	ebx,26
	jne	pwq
	; printing the year
	; set our string index
	mov	ebx,[y]
	mov	esi,0B80F6h ; 0B8000h+246
	mov	al,[ebx+txt]
	mov	[esi],ax
	mov	byte [esi+1],70h
	mov	ebx,[y1]
	add	esi,2
	mov	al,[ebx+txt]
	mov	[esi],ax
	mov	byte [esi+1],70h
	mov	ebx,[y2]
	add	esi,2
	mov	al,[ebx+txt]
	mov	[esi],ax
	mov	byte [esi+1],70h
	mov	ebx,[y3]
        add	esi,2
	mov	al,[ebx+txt]
	mov	[esi],ax
	mov	byte [esi+1],70h
	sub	ebx,ebx ; string index
	mov	esi,0B8AC4h ; 0B000h+2756
prk:
	mov	al,[ebx+howto] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],72h
	mov	al,[ebx+howto2]
	mov	[esi+320],ax
	mov	byte [esi+321],72h
	inc	ebx
	add	esi,2
	cmp	ebx,38
	jne	prk
	jmp	axt
	; --skel ends here
figdays: ; we will now figure it out and store the value in daysmth
	mov	ebx,[month] ; copy our month
	cmp	ebx,6
	je	set30
	ja	figdays@
	cmp	ebx,1
	je	set31
	cmp	ebx,2
	je	set28
	cmp	ebx,3
	je	set31
	cmp	ebx,4
	je	set30
	jmp	set31
figdays@:
	cmp	ebx,7
	je	set31
	cmp	ebx,8
	je	set31
	cmp	ebx,9
	je	set30
	cmp	ebx,10
	je	set31
	cmp	ebx,11
	je	set30
	jmp	set31
set30:
	mov	dword [daysmth],30
	cmp	dword [pre],1
	je	prevmonth2
	jmp	artl2
set31:
	mov	dword [daysmth],31
	cmp	dword [pre],1
	je	prevmonth2
	jmp	artl2
set28:
	cmp	dword [leap],4
	je	set29
	mov	dword [daysmth],28
	cmp	dword [pre],1
	je	prevmonth2
	jmp	artl2
set29:
	;mov	dword [leap],0
	mov	dword [daysmth],29
	cmp	dword [pre],1
	je	prevmonth2
	jmp	artl2
name1:	; name for january
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr1:
	mov	al,[ebx+jantxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,7
	jne	pr1
	jmp	artl
prevmonth2:
	jmp	prevmonth3
name2:	; name for february
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr2:
	mov	al,[ebx+febtxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,8
	jne	pr2
	jmp	artl
name3:	; name for march
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr3:	
	mov	al,[ebx+marchtxt] 
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,5
	jne	pr3
	jmp	artl
name4:	; name for april
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr:
	mov	al,[ebx+apriltxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,5
	jne	pr
	jmp	artl	
name5:	; name for may
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr5:
	mov	al,[ebx+maytxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,3
	jne	pr5
	jmp	artl
name6:	; name for june
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr6:
	mov	al,[ebx+junetxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,4
	jne	pr6
	jmp	artl
name7:	; name for july
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr7:
	mov	al,[ebx+julytxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,4
	jne	pr7
	jmp	artl
name8:	; name for august
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr8:
	mov	al,[ebx+augtxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,6
	jne	pr8
	jmp	artl
name9:	; name for september
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr9:
	mov	al,[ebx+septxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,9
	jne	pr9
	jmp	artl
name10:	; name for october
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr10:
	mov	al,[ebx+octtxt] ;mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,7
	jne	pr10
	jmp	artl
name11:	; name for november
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr11:
	mov	al,[ebx+novtxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,8
	jne	pr11
	jmp	artl
name12:	; name for december
	sub	ebx,ebx ; ebx=0
	mov	esi,0B80DEh ; 0B8000h+222
pr12:
	mov	al,[ebx+dectxt] ; mov ah,4ch maybe not needed
	mov	[esi],ax
	mov	byte [esi+1],71h
	inc	ebx
	add	esi,2
	cmp	ebx,8
	jne	pr12
	jmp	artl
sfa:
	jmp	axz
	; lets draw the days with the given year
drawmonth:
	sub	ebx,ebx ; index of our string ebx=0
	sub	ecx,ecx ; ecx=0 to count the days
	sub	edx,edx ; to keep track of what day to start next
	mov	esi,0B82B6h ; 0B8000h+694 ; the start of the month
	add	esi,[dayweek] ; we also need to add the day it adds
	mov	edi,[dayweek]
	mov	dword [prevdays],edi ; to save info for prev month
	mov	edi,0B82EEh  ; 0B8000h+750 ; the end of the month
zwrup:	; when skipping years I dont want it to draw it screen 
zwr:
	jmp	rdname
mid845:
	inc	ebx
	jmp	abstract
rdname:
	cmp	dword [fort],1
	je	mid845
	jmp	rdname1
mid784:
	inc	ebx
	jmp	abstract
rdname1:
	cmp	dword [port],1
	je	mid784
	mov	al,[ebx+daysstext]
	mov	[esi],ax
	mov	word [esi+1],71h
	inc	ebx ; 1 3
	mov	al,[ebx+daysstext]
	mov	[esi+2],ax
	mov	byte [esi+3],71h
abstract:
	add	esi,8
	add	edx,8
	mov	[dayweek],edx
	inc	ebx ; 2 4
	add	ecx,1
	cmp	ecx,[daysmth] ; this counts the amount of days used
	jge	input
	cmp	esi,edi
	jne	zwr
	sub	esi,56
	add	esi,320
	add	edi,320
	sub	edx,edx
	mov	[dayweek],edx
	cmp	ecx,[daysmth]
	jle	zwrup
	;jmp	input

input:	; we now read keys
	; before that let me check that our days is !> 56
	mov	ecx,56
	cmp	ecx,[dayweek]
	jle	res
input2:
	cmp	dword [fort],1 ; if forward is true we keep looping
	je	loop10
	cmp	dword [port],1
	je	loop24
	; getchar (wait)
	xor	ah,ah
	int	32h
	cmp	al, 27 ; esc
	je	endz
	cmp	al,'q' ; if q we quit
	je	endz
	cmp	al,'n' ; for next month
	je	nextmonth ; to change to next month
	cmp	al,'p' ; to change everything
	je	pastmonth
	cmp	al,'f' ; to forward ten years
	je	forward
	cmp	al,'r'
	je	reverse
	jmp	input ; if anything else is press
		; pastweek

mid1245:
	cmp	dword [y2],9
	je	input
	jmp	continue456

nextmonth:
ert:	cmp	dword [y],2
	je	mid1245
continue456:
	mov	ebx,[month]
	inc	ebx
	cmp	ebx,13
	je	th2
	mov	[month],ebx
	jmp	sfa
th2:
	inc	dword [y3]
	inc	dword [leap] ; increase our leap year
	jmp	end23
mid23:	
	mov	dword [leap],1
end23:
	cmp	dword [leap],5
	je	mid23
	cmp	dword [y3],10
	je	th3
	mov	dword [month],1
	jmp	sfa
th3:
	mov	dword [y3],0
	inc	dword [y2]
	cmp	dword [y2],10
	je	th9
	mov	dword [month],1
	jmp	sfa
th9:
	mov	dword [y2],0
	inc	dword [y1]
	cmp	dword [y1],10
	je	th78
	mov	dword [month],1
	jmp	sfa
th78:
	mov	dword [y1],0
	inc	dword [y]
	mov	dword [month],1
	jmp	sfa

res:	; to reset the days >56
	sub	ecx,ecx
	mov	[dayweek],ecx
	jmp	input2 ; end for reset

mid12456:
	cmp	dword [y2],1
	je	input
	jmp	continue4567

pastmonth: ; to go back to the prev month
ert34:	; if year decreases too much we do nothing
	cmp	dword [y],1
	je	mid12456
continue4567:
	mov	ebx,[month]
	;sub	ebx,1
	;cmp	ebx,0
	;je	th4
	dec	ebx
	jz	th4
	mov	[month],ebx
	jmp	prevmonth1	
th4:
	mov	dword [month],12
	;sub	dword [y3],1
	dec	dword [y3]
	jmp	chan
midc:
	mov	dword [y3],9
	;sub	dword [y2],1
	dec	dword [y2]
	jmp	ch23
midc23:
	mov	dword [y2],9
	mov	dword [y1],9
	;sub	dword [y],1
	dec	dword [y]
ch23:
	cmp	dword [y2],-1
	je	midc23
chan:
	cmp	dword [y3],-1
	je	midc
	;sub	dword [leap],1
	;jmp	change2
	dec	dword [leap]
	jg	short prevmonth1
mid2:
	mov	word [leap],4
change2:
	;cmp	word [leap],0
	;jle	mid2
prevmonth1:
	; lets figure out the amount of days the prev month had
	mov	dword [pre],1
	jmp	figdays
prevmonth3:
	mov	ebx,[daysmth] ; now let's calculate when this month should start
	mov	ecx,[prevdays] ; previous info save
	;cmp	ecx,0
	;je	premonthpt2
	jecxz	premonthpt2
	sub	edi,edi   ; info save
	jmp	loop01
middle01:
	sub	ecx,8
	inc	edi
loop01:
	cmp	ecx,0 ; let's check the previous days
	jne	middle01
		; our edi should equal to 4 or 2
	mov	ecx,7
	sub	ecx,edi ; ecx=3 ecx=5
	mov	dword [prevdays],ecx ; save info prevdays=3 prevdays=5
premonthpt2:
	jmp	loop0
middle:
	sub	ebx,7
loop0:
	cmp	ebx,7
	jg	middle
	mov	ecx,7	; our ebx should be equal to 3
	add	ebx,[prevdays]
	mov	dword [prevdays],0
	jmp	change
mid:
	sub	ebx,7
change:
	cmp	ebx,7
	jg	mid
	sub	ecx,ebx ; our ecx=4 now contains the date it will start
	sub	ebx,ebx
	sub	edi,edi
	jmp	loop1 ; to add our 8
middle1:
	add	edi,8
	inc	ebx
loop1:
	cmp	ecx,ebx
	jne	middle1
	          
	mov	[dayweek],edi

	mov	dword [pre],0 ; reseting the condition
	jmp	sfa	; printing it

forward: ; to go 3 years ahead i basically just loop my code 3 times
	; if the limit is near forward is diable
	jmp	ths
midw:
	cmp	dword [y2],8
	je	midws
	jmp	cont2
midws:
	cmp	dword [y3],7
	je	input
	jmp	cont2
ths:
	cmp	dword [y],2
	je	midw
cont2:
	mov	dword [fort],1
	mov	dword [temp],36
	jmp	loop10
middle10:
	;sub	dword [temp],1
	dec	dword [temp]
	jmp	nextmonth
loop10:
	cmp	dword [temp],1
	jne	middle10
	mov	dword [fort],0
	jmp	nextmonth
reverse:
	; to go 3 years in the past
	mov	dword [port],1
	mov	dword [temp],36
	jmp	loop24
middle24:
	;sub	dword [temp],1
	dec	dword [temp]
	jmp	pastmonth
loop24:
	cmp	dword [temp],1
	jne	middle24
	mov	dword [port],0
	jmp	pastmonth
endz:
	; set mode to 03h (clear screen)
	mov	eax, 3
	int	31h	
	
	; terminate the program
	;sys	_exit, 0
	;mov	ebx, 0
	mov	eax, 1 ; _exit
	int	40h	; TRDOS 386 system call

jantxt:	   db "January"
febtxt:    db "February"
marchtxt:  db "March"
apriltxt:  db "April"
maytxt:	   db "May"
junetxt:   db "June"
julytxt:   db "July"
augtxt:	   db "August"
septxt:    db "September"
octtxt:    db "October"
novtxt:    db "November"
dectxt:    db "December"
daysstext: db "01020304050607080910111213141516171819202122232425262728293031" ; size is 61
txt:	   db "0123456789" ; to print my year
howto:	   db  "press n:next month or  q:quit         " ; size 39 
howto2:	   db "press f:skip 3 years r:go back 3 years"
namedays:  db "Su  Mo  Tu  We  Th  Fr  Sa  Su" ; size is 26

month:	  dd 1 ; to keep track of our month
year:	  dd 2026 ; to keep track of our year ; january 1, 2026 = thursday
dayweek:  dd 4*8 ; so we will know what day we should begin our month with
daysmth:  dd 0 ; for the days of the month
prevdays: dd 0 ; to save info of the prev month
fort:	  dd 0 ; if 0 forward not on if 1 true
port:	  dd 0 ; if 0 reverse not on if 1 true
temp:	  dd 0 ; to save for forward 10 year or less
pre:	  dd 0 ; like a condition if it's 1 we will used info to calculate prev month
y:	  dd 2 ; this numbers is for my year
y1:	  dd 0
y2:	  dd 2
y3:	  dd 6
pastweek: dd 0 ; this is the variable for the previous month
leap:	  dd 2 ; this is for my leap years if it's 4 then it's a leap year

;bss_start:

;ABSOLUTE bss_start

;saveScr: resb 25*160

;bss_end:
