;Armando Aguirre
;Project 1:Calender
;Spring 2017
;(Tasm syntax)

; Nasm syntax conversion: Erdogan Tan - May 2026

bits 16
org 0x100

	start:
	realstart:

	mov ah,01h
	mov cx,2607h ;hide our cursor
	int 10h
	axz:  	;our loop random name to start
	cmp word [fort],1
	je axt
	cmp word [port],1
	je axt
 	jmp clean ;we clean our screen first	
	axd:      ;we draw our skeleton edges,year,and line number and those stuff
	jmp skel
	axt:      ;now we draw our first month name
	jmp genames
	artl:  ;first we will figure out how many dates the month has
	jmp aac	
	artl2: ;we will now draw the dates of the month	
	jmp drawmonth ;we draw our month
	;cleaning my screen
	clean:
	mov cx,4000
	mov ax,0B800h
	mov es,ax
	mov ax,7520h
	sub bx,bx ;bx=0
	f: mov [es:bx],ax
	add bx,2
	loop f

	jmp axd
	;name jmp because it could not reach the orinila jmp
	genames: mov bx,[month]
		cmp bx,1
		je cjump
		cmp bx,2
		je cjump1
		cmp bx,3
		je cjump2
		cmp bx,4
		je cjump3
		cmp bx,5
		je cjump4
		cmp bx,6
		je cjump5
	        cmp bx,7
		je cjump6
		cmp bx,8
		je cjump7
		cmp bx,9
		je cjump8
		cmp bx,10
		je cjump9
		cmp bx,11
		je cjump10
		cmp bx,12
		je cjump11
	bgenames: jmp artl
	aac: jmp figdays ;another jmp that needed jmp help
	skel: sub bx,bx  ;bx=0
	tp: mov word [es:bx],75c4h ;need word bcs byte is not big
	mov byte [es:bx+1],70h 
	mov word [es:bx+3840],75c4h
	mov byte [es:bx+3841],70h
	add bx,2  ;increase our value
	cmp bx,160 ;160 is the last value of our screen
	jne tp
	mov word [es:0],75DAh ;left corner
	mov byte [es:1],70h 
	mov word [es:158],75BFh  ; right corner
	mov byte [es:159],70h
	jmp skelpart2	;skipping to another part
	cjump:	jmp name1
	cjump1: ;all the name to our months that jmp to the actual code
		jmp name2
	cjump2:
		jmp name3
	cjump3:
		jmp name4
	cjump4:
		jmp name5
	cjump5:
		jmp name6
	cjump6:
		jmp name7
	cjump7:
		jmp name8
	cjump8:
		jmp name9
	cjump9:
		jmp name10
	cjump10:
		jmp name11
	cjump11:
		jmp name12
	cjump100: jmp bgenames
	skelpart2: ;here we are to part 2
	mov bx,160
	op: mov word [es:bx],75B3h
	mov byte [es:bx+1],70h 
	mov word [es:bx+158],75B3h
	mov byte [es:bx+159],70h 
	add bx,160
	cmp bx,3840
	jne op 
	mov si,694	
	mov di,750
	sub bx,bx
	upok: ;drawing the lines
	ok:mov word [es:si],'_'
	mov word [es:si+1],71h
	mov word [es:si+2],'_'
	mov word [es:si+3],71h
	add si,8
	cmp si,di
	jne ok
	sub si,56
	add si,320
	add di,320
	inc bx
	cmp bx,6
	jne upok	
	mov si,374
	sub bx,bx ;printing our name days mo tu..ect
	pwq: mov al,[cs:bx+namedays]
	mov [es:si],ax
	mov byte [es:si+1],71h
	add si,2
	inc bx
	cmp bx,26
	jne pwq
             ;printing the year
	sub bx,bx ;set our string index
	add bx,[y]
	mov si,246 ;
	mov al,[cs:bx+txt]
	mov [es:si],ax
	mov byte [es:si+1],70h
	sub bx,bx
	add bx,[y1]
	add si,2
	mov al,[cs:bx+txt]
	mov [es:si],ax
	mov byte [es:si+1],70h
	sub bx,bx
	add bx,[y2]
	add si,2
	mov al,[cs:bx+txt]
	mov [es:si],ax
	mov byte [es:si+1],70h
	sub bx,bx
	add bx,[y3]
        add si,2
	mov al,[cs:bx+txt]
	mov [es:si],ax
	mov byte [es:si+1],70h
	sub bx,bx ;string index
	mov si,2756
	prk: mov al,[cs:bx+howto] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],72h
	mov al,[cs:bx+howto2]
	mov [es:si+320],ax
	mov byte [es:si+321],72h
	inc bx
	add si,2
	cmp bx,38
	jne prk
	jmp axt
	;--skel ends here
	figdays: ;we will now figure it out and store the value in daysmth
	mov bx,[month] ;copy our month
	cmp bx,1
	je set31
	cmp bx,2
	je set28
	cmp bx,3
	je set31
	cmp bx,4
	je set30
	cmp bx,5
	je set31
	cmp bx,6
	je set30
	cmp bx,7
	je set31
	cmp bx,8
	je set31
	cmp bx,9
	je set30
	cmp bx,10
	je set31
	cmp bx,11
	je set30
	cmp bx,12
	je set31
	set30:
	mov word [daysmth],30
	cmp word [pre],1
	je prevmonth2
	jmp artl2
	set31:
	mov word [daysmth],31
	cmp word [pre],1
	je prevmonth2
	jmp artl2
	set28:
	cmp word [leap],4
	je set29
	mov word [daysmth],28
	cmp word [pre],1
	je prevmonth2
	jmp artl2
	set29:
	;mov word [leap],0
	mov word [daysmth],29
	cmp word [pre],1
	je prevmonth2
	jmp artl2
	name1: ;name for january
	sub bx,bx ;bx=0
	mov si,222
	pr1: mov al,[cs:bx+jantxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,7
	jne pr1
	jmp artl
	prevmonth2:jmp prevmonth3
	name2: ;name for february
	sub bx,bx ;bx=0	
	mov si,222
	pr2: mov al,[cs:bx+febtxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,8
	jne pr2
	jmp artl
	name3: ;name for march
	sub bx,bx ;bx=0	
	mov si,222
	pr3: mov al,[cs:bx+marchtxt] 
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,5
	jne pr3
	jmp artl
	name4: ;name for april
	sub bx,bx ;bx=0	
	mov si,222
	pr: mov al,[cs:bx+apriltxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,5
	jne pr
	jmp artl	
	name5: ;name for may
	sub bx,bx ;bx=0	
	mov si,222
	pr5: mov al,[cs:bx+maytxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,3
	jne pr5
	jmp artl
	name6: ;name for june
	sub bx,bx ;bx=0	
	mov si,222
	pr6: mov al,[cs:bx+junetxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,4
	jne pr6
	jmp artl
	name7: ;name for july
	sub bx,bx ;bx=0	
	mov si,222
	pr7: mov al,[cs:bx+julytxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,4
	jne pr7
	jmp artl
	name8: ;name for august
	sub bx,bx ;bx=0	
	mov si,222
	pr8: mov al,[cs:bx+augtxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,6
	jne pr8
	jmp artl
	name9: ;name for september
	sub bx,bx ;bx=0	
	mov si,222
	pr9: mov al,[cs:bx+septxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,9
	jne pr9
	jmp artl
	name10: ;name for october
	sub bx,bx ;bx=0	
	mov si,222
	pr10: mov al,[cs:bx+octtxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,7
	jne pr10
	jmp artl
	name11: ;name for november
	sub bx,bx ;bx=0	
	mov si,222
	pr11: mov al,[cs:bx+novtxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,8
	jne pr11
	jmp artl
	name12: ;name for december
	sub bx,bx ;bx=0	
	mov si,222
	pr12: mov al,[cs:bx+dectxt] ;mov ah,4ch maybe not needed
	mov [es:si],ax
	mov byte [es:si+1],71h
	inc bx
	add si,2
	cmp bx,8
	jne pr12
	jmp artl
	sfa:
	jmp axz
	;lets draw the days with the given year
	drawmonth:
	sub bx,bx ;index of our string bx=0
	sub cx,cx ;cx=0 to count the days
	sub dx,dx ;to keep track of what day to start next
	mov si,694 ;the start of the month
	add si,[dayweek]   ;we also need to add the day it adds
	mov di,[dayweek]
	mov word [prevdays],di	;to save info for prev month
	mov di,750 ;the end of the month
	zwrup: ;when skipping years I dont want it to draw it screen 
	zwr:
	jmp rdname
	mid845: inc bx
		jmp abstract
	rdname: cmp word [fort],1
		je mid845
	jmp rdname1
	mid784: inc bx
		jmp abstract
	rdname1: cmp word [port],1
		 je mid784
	mov al,[cs:bx+daysstext]
	mov [es:si],ax
	mov word [es:si+1],71h
	inc bx ;1 3
	mov al,[cs:bx+daysstext]
	mov [es:si+2],ax
	mov byte [es:si+3],71h
	abstract:
	add si,8
	add dx,8
	mov [dayweek],dx
	inc bx ;2 4
	add cx,1
	cmp cx,[daysmth] ;this counts the amount of days ued
	jge input
	cmp si,di
	jne zwr
	sub si,56
	add si,320
	add di,320
	sub dx,dx
	mov [dayweek],dx
	cmp cx,[daysmth]
	jle zwrup
	jmp input

	input:	;we now read keys
		;before that let me check that our days is !> 56
	mov cx,56
	cmp cx,[dayweek]
	jle res10
	input2:
	cmp word [fort],1 ;if forward is true we keep looping
	je loop100
	cmp word [port],1
	je loop242
	mov ah,07 ;we wait for input
	int 21h
	mov bh,al
		cmp bh,'q' ;if q we quit
		je endz2
	mov bh,al
		cmp bh,'n' ;for next month
		je nextmonth
	mov bh,al
		cmp bh,'p' ;to change everything
		je pastmonth278
	mov bh,al
		cmp bh,'f' ;to forward ten years
		je forward378
	mov bh,al
		cmp bh,'r'
		je reverse278
	jmp input ;if anything else is press
		;pastweek
	loop100: jmp loop10
	loop242:jmp loop24
	res10: jmp res
	endz2: jmp endz
	pastmonth278: jmp pastmonth2
	forward378: jmp forward3
	reverse278: jmp reverse2
	nextmonth: ;to change to next month
	jmp ert ;if year exceeds we do nothing
	mid1245: cmp word [y2],9
		 je input
		 jmp continue456
	ert: cmp word [y],2
	je mid1245
	continue456:
	mov bx,[month]
	inc bx
	cmp bx,13
	je th2
	mov [month],bx
	jmp sfa
	th2:
	inc word [y3]
	inc word [leap] ;increase our leap year
	jmp end23
	mid23:	mov word [leap],1
	end23:	cmp word [leap],5
		je mid23
	cmp word [y3],10
	je th3
	mov word [month],1
	jmp sfa
	th3:
	mov word [y3],0
	inc word [y2]
	cmp word [y2],10
	je th9
	mov word [month],1
	jmp sfa
	th9: mov word [y2],0
	inc word [y1]
	cmp word [y1],10
	je th78
	mov word [month],1
	jmp sfa
	th78: mov word [y1],0
	inc word [y]
	mov word [month],1
	jmp sfa
	input78: jmp input
	forward3: jmp forward
	pastmonth2: jmp pastmonth
	res: ;to reset the days >56
	sub cx,cx
	mov [dayweek],cx
	jmp input2 ;end for reset
	reverse2: jmp reverse
	pastmonth: ;to go back to the prev month
		jmp ert34;if year decreses too much we do nothing
	mid12456:cmp word [y2],1
		je input78
		jmp continue4567
	ert34:cmp word [y],1
	je mid12456
	continue4567:
	mov bx,[month]
	sub bx,1
	cmp bx,0
	je th4
	mov [month],bx
	jmp prevmonth1	
	th4:
	mov word [month],12
	sub word [y3],1
	jmp chan
	midc: mov word [y3],9
	      sub word [y2],1
		jmp ch23
		midc23: mov word [y2],9
			mov word [y1],9
			sub word [y],1
		ch23:cmp word [y2],-1
		je midc23
	chan:	cmp word [y3],-1
		je midc
	sub word [leap],1
	jmp change2
	mid2: mov word [leap],4
	change2: cmp word [leap],0
		jle mid2
	jmp prevmonth1
	prevmonth1: ;lets figure out the amount of days the prev month had
	mov word [pre],1
	jmp figdays
	prevmonth3:
	mov bx,[daysmth]  ;now let's calculate when this month should start
	mov cx,[prevdays] ;previous info save
	cmp cx,0
	je premonthpt2
	sub di,di   ;info save
	jmp loop01
	input789: jmp input78
	middle01: sub cx,8
		  inc di
	loop01: cmp cx,0 ;let's check the previous days
		jne middle01
			;our di shoud equal di=4 di=2
	mov cx,7
	sub cx,di ;cx 3 cx=5
	mov word [prevdays],cx ;save info prevdays=3 prevdays=3
	premonthpt2:
	jmp loop0
	middle: sub bx,7
	loop0:	cmp bx,7
		jg middle
	mov cx,7	;our bx should equal 3 bx=3
	add bx,[prevdays] 
	mov word [prevdays],0
	jmp change
	mid:
		sub bx,7
	change: cmp bx,7
		jg mid
	sub cx,bx ;our cx=4 now contains the date it will start
	sub bx,bx
	sub di,di
	jmp loop1 ;to add our 8
	middle1: add di,8
		 inc bx
	loop1:	cmp cx,bx
		jne middle1
	          
	mov [dayweek],di

	mov word [pre],0 ;resseting the condition
	jmp sfa		 ;printing it

	forward: ;to go 3 years ahead i basically just loop my code 3 times
	;if the limit is near forward is diable
	jmp ths
	midw:	cmp word [y2],8
		je midws
		jmp cont2
		midws:	cmp word [y3],7
			je input789
			jmp cont2
	ths:	cmp word [y],2
		je midw
	cont2:
	mov word [fort],1
	mov word [temp],36
	jmp loop10
	middle10:
		sub word [temp],1
		jmp nextmonth
	loop10: cmp word [temp],1
		jne middle10
	mov word [fort],0
	jmp nextmonth
	reverse: ;to go 3 years in the past
	mov word [port],1
	mov word [temp],36
	jmp loop24
	middle24: sub word [temp],1
		jmp pastmonth
	loop24: cmp word [temp],1
		jne middle24
	mov word [port],0
	jmp pastmonth
endz:
int 16h
mov ah,4ch ;to terminate dos
int 21h ;to terminate the program

month dw 1 ;to keep track of our month
year dw 2017 ;to keep track of our year
dayweek dw 0 ;so we will know what day we should begin our month with
daysmth dw 0 ; for the days of the month
prevdays dw 0 ;to save info of the prev month
fort  dw 0   ;if 0 forward not on if 1 true
port dw 0    ;if 0 reverse not on if 1 true
temp dw 0 ;to save for forwar 10 year or less
pre dw 0 ;like a condition if it's 1 we will used info to calculate prev month
y  dw 2 ;this numbers is for my year
y1 dw 0
y2 dw 1
y3 dw 7
pastweek dw 0 ;this is the variable for the previous month
leap dw 1 ;this is for my leap years if it's 4 then it's a leap year

jantxt:	db "January"
febtxt: db "February"
marchtxt: db "March"
apriltxt: db "April"
maytxt:	db "May"
junetxt: db "June"
julytxt: db "July"
augtxt:	db "August"
septxt: db "September"
octtxt: db "October"
novtxt: db "November"
dectxt: db "December"
daysstext: db "01020304050607080910111213141516171819202122232425262728293031" ;size is 61
txt: db "0123456789" ;to print my year
howto: db  "press n:next month or  q:quit         " ;size 39 
howto2: db "press f:skip 3 years r:go back 3 years"
namedays: db "Su  Mo  Tu  We  Th  Fr  Sa  Su" ;size is 26

bss_start:

ABSOLUTE bss_start

saveScr: resb 25*160

bss_end:


