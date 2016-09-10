org 100h 		; ¿É±àÒë³ÉCOMÎÄ¼ş
; ===================================================================
; ÃüÁîĞĞÖ÷³ÌĞò¿ªÊ¼
;--------------------------------------------------------------------	
	; Í¨¹ıAXÖĞ×ª£¬½«CSµÄÖµ¸³¸øDS¡¢ESºÍSS
	mov ax, cs
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 100h - 4	; ÖÃÕ»¶¥Ö¸ÕëSP=100h-4
	mov ax,12h    ;640*480 mode
	int 10h       ;ÉèÖÃ640*480/16É«ÏÔÊ¾Ä£Ê½
	; ³õÊ¼»¯ÄÚ²¿ÃüÁîÀı³ÌÈë¿ÚµØÖ·
	mov word [cmdaddr], ver		; VER ÏÔÊ¾°æÈ¨ĞÅÏ¢
	mov word [cmdaddr + 2], cls	; CLS ÇåÆÁ
	mov word [cmdaddr + 4], toa	; A:  ÇĞ»»µ½AÅÌ
	mov word [cmdaddr + 6], tob	; B:  ÇĞ»»µ½BÅÌ
	mov word [cmdaddr + 8], toc	; C:  ÇĞ»»µ½CÅÌ
	mov word [cmdaddr + 10], dir; DIR ÏÔÊ¾ÎÄ¼şÄ¿Â¼ÁĞ±í
	mov word [cmdaddr + 12], ls; LS  ÏÔÊ¾ÎÄ¼şÄ¿Â¼ÁĞ±í
	mov word [cmdaddr + 14], help; HELP ÏÔÊ¾°ïÖú
	mov word [cmdaddr + 16],cdToDir ;Ä¿Â¼Ìø×ª
	mov word [cmdaddr + 18], _dt; dt ÏÔÊ¾Ê±¼ä
	mov word [cmdaddr + 20], _dc; dc ÏÔÊ¾Ê±¼ä	
	mov word [cmdaddr + 22], rename
	mov word [cmdaddr + 24], mkdir
	mov word [cmdaddr + 26], ReadMemmory
	mov word [cmdaddr + 28], HZK16_test
	mov word [cmdaddr + 30], SignIn
	mov word [cmdaddr + 32], Restart
	mov word [cmdaddr + 34], ChangePassword
	mov word [cmdaddr + 36], ChangeUserName
	; ÉèÖÃÖĞ¶ÏÏòÁ¿£¨21h£©
	xor ax, ax		; AX = 0
	mov fs, ax		; FS = 0
	mov word[fs:21h*4], int21h ; ÉèÖÃ21hºÅÖĞ¶ÏÏòÁ¿µÄÆ«ÒÆµØÖ·
	mov ax,cs 
	mov [fs:21h*4+2], ax ; ÉèÖÃ21hºÅÖĞ¶ÏÏòÁ¿µÄ¶ÎµØÖ·=CS
	call Store_dc
	call getdiskparam	; »ñÈ¡´ÅÅÌ²ÎÊıH&S£¨ÓÃÓÚReadSecºÍlsÀı³Ì£©
	call cls		; ÇåÆÁ
	call initialDisk ;³õÊ¼»¯lsÓÃµ½µÄÉÈÇøĞÅÏ¢
	call int213dh
	call cls
	call ver		; ÏÔÊ¾°æÈ¨ĞÅÏ¢
	;call waitforkey_chin
	call cls		; ÇåÆÁ
	call SignIn
	call cls		; ÇåÆÁ
	;call ver		; ÏÔÊ¾°æÈ¨ĞÅÏ¢
again: ; ÃüÁîĞĞÑ­»·
	call BackToCmd
	;call ver0		; ÏÔÊ¾°æÈ¨ĞÅÏ¢
	call prompt		; ÏÔÊ¾ÌáÊ¾´®
	call getstrln	; »ñÈ¡¼üÅÌÊäÈëµÄÃüÁî´®ĞĞ
	call dtlen		; È·¶¨ÃüÁî´®³¤¶È
	call tocap		; ×ª»»³É´óĞ´×ÖÄ¸
	call newstr		; ¹¹ÔìĞÂ´®
	call Shut_dc
	call iscmd		; ÅĞ¶ÏÊÇ·ñÎªÄÚ²¿ÃüÁî£¬Èç¹ûÊÇ£¬ÔòÖ´ĞĞÖ®£¬·ñÔò£º
	call newline	; »Ø³µ»»ĞĞ
	call exec		; Ö´ĞĞÍâ²¿ÃüÁî£¨COMÎÄ¼ş£©
	jmp again		; ¼ÌĞøÑ­»·
	
;--------------------------------------------------------------------
; ¶¨Òå±äÁ¿¡¢Êı×é¡¢»º³åÇøºÍ×Ö·û´®

drvno db 0 ; ´ÅÅÌÇı¶¯Æ÷ºÅ£º0=ÈíÅÌA¡¢1=ÈíÅÌB¡¢80h=Ó²ÅÌC
i dw 0 ; Ñ­»·±äÁ¿
n dw 0 ; ÃüÁî´®³¤¶È

N equ 19	; ÄÚ²¿ÃüÁî×ÜÊı
cslen equ 8 ; ÃüÁî´®×î´ó³¤¶È

cmdstr: ; ÄÚ²¿ÃüÁî´®Êı×é£¨Í³Ò»´®³¤Îª8£¬²»×ã²¹¿Õ¸ñ·û£©
	db 'VER     '
	db 'CLS     '
	db 'A:      '
	db 'B:      '
	db 'C:      '
	db 'DIR     '
	db 'LS      '
	db 'HELP    '
	db 'CD      '
	db 'DT      '
	db 'DC      '
	db 'RENAME  '
	db 'MKDIR   '
	db 'READMEM '
	db 'CHINESE '
	db 'LOCK    '
	db 'RESTART '
	db 'CPASS   '
	db 'CUSER   '
cmdHelpStr:  ;ÄÚ²¿ÃüÁî½âÊÍÍ³Ò»´®³¤30
	db 'Show OS verion&OEM Info.      '
	db 'Clear screen.                 '
	db 'Switch to disk A (floppy).    '
	db 'Switch to disk B (floppy).    '
	db 'Switch to disk C (hard disk). '
	db 'Display the current directory.'
	db 'Lists all of the files.       '
	db 'Show help Info.               '
	db 'Change current directory.     '
	db 'Display time                  '
	db 'Display time(EN/US format).   '
	db 'Rename file name.                    '
	db 'Make Directory.             '
	db 'Read 16 byte memory in x:x.   '
	db 'Display Chinese Chararcter.   '
	db 'Lock screen                   '
cmdHelpStr_chin:  ;ÄÚ²¿ÃüÁî½âÊÍÍ³Ò»´®³¤30
	db 'ÏÔÊ¾ÏµÍ³°æ±¾                  '
	db 'ÇåÆÁ                          '
	db '¸Ä±äÄ¿Â¼ÖÁÏµÍ³ÅÌ¸ùÄ¿Â¼        '
	db '¸Ä±äÄ¿Â¼ÖÁ¶şºÅÈíÅÌ¸ùÄ¿Â¼      '
	db '¸Ä±äÄ¿Â¼ÖÁÓ²ÅÌ¸ùÄ¿Â¼          '
	db 'ÏÔÊ¾µ±Ç°Ä¿Â¼                  '
	db 'ÏÔÊ¾ËùÓĞµ±Ç°Ä¿Â¼ÎÄ¼şÌõÄ¿      '
	db 'ÏÔÊ¾°ïÖúĞÅÏ¢                  '
	db '¸Ä±äµ±Ç°Ä¿Â¼                  '
	db 'ÏÔÊ¾µ±Ç°Ê±¼ä                  '
	db 'ÊµÊ±Ê±¼ä                      '
	db 'ÖØÃüÃûÎÄ¼ş                    '
	db '´´½¨ÎÄ¼ş¼Ğ                    '
	db '¶ÁÈ¡ÄÚ´æ                      '
	db 'ºº×ÖÑİÊ¾                      '
	db 'ËøÆÁ                          '
	db 'ÖØÆô                          '
	db 'ĞŞ¸ÄÃÜÂë                      '
	db 'ĞŞ¸ÄÓÃ»§Ãû                    '
cmdaddr: ; ÄÚ²¿ÃüÁîÀı³ÌÈë¿ÚµØÖ·Êı×é
	resw N

fnbuf: ; COMÎÄ¼şÃû´®£¨8+3=11×Ö·û£©
	db '12345678COM'

Dirbuf: ; Ä¿Â¼Ãû´®£¨8+3=11×Ö·û£©
	db '           '
buflen: equ 80 ; »º³åÇø³¤¶È=80

buf: resb buflen ; ÃüÁîĞĞ»º³åÇø
 
str1: ; ×Ö·û´®1£¨°æÈ¨ĞÅÏ¢´®£©
	db 'BigBoom-OS 2.0  (C) 2016 Big Firecrackers'
str1len equ $ - str1 ; °æÈ¨´®³¤

str2: ; ×Ö·û´®2Êı×é£¨ÃüÁîĞĞÌáÊ¾´®£©
	db 'A:/$'
	resb 80   ;×ÓÄ¿Â¼»º³åÇø
str2len: dw 4 ; ÌáÊ¾´®³¤

str3: ; ×Ö·û´®3£¨³ö´íĞÅÏ¢´®£©
	db 'Wrong command!'
str3len equ $ - str3 ; ´íÎóÃüÁî´®³¤

str3_chin: ; ×Ö·û´®3£¨³ö´íĞÅÏ¢´®£©
	db '²»ÊÇÄÚ²¿»òÍâ²¿ÃüÁî£¬Ò²²»ÊÇ¿ÉÔËĞĞµÄ³ÌĞò'
str3len_chin equ ($ - str3_chin)/2 ; ´íÎóÃüÁî´®³¤

str4: ; ×Ö·û´®4£¨´®Ì«³¤ĞÅÏ¢´®£©
	db 'Too long!'
str4len equ $ - str4 ; Ì«³¤´®³¤
str4_chin: ; ×Ö·û´®3£¨³ö´íĞÅÏ¢´®£©
	db 'ÃüÁîÌ«³¤'
str4len_chin equ ($ - str4_chin)/2 ; ´íÎóÃüÁî´®³¤

str10: ; ×Ö·û´®5£¨³ö´íĞÅÏ¢´®£©
	db 'No such file or directory!'
str10len equ $ - str10 ; ´íÎóÃüÁî´®³¤
str10_chin: ; ×Ö·û´®3£¨³ö´íĞÅÏ¢´®£©
	db 'Ã»ÓĞÄÇ¸öÎÄ¼ş»òÄ¿Â¼'
str10len_chin equ ($ - str10_chin)/2 ; ´íÎóÃüÁî´®³¤

str11_chin: ; ×Ö·û´®4£¨³ö´íĞÅÏ¢´®£©
	db 'ÎÄ¼ş¼ĞÒÑ´æÔÚ'
str11len_chin equ ($ - str11_chin)/2 ; ÖØ¸´ÌõÄ¿
; -------------------------------------------------------------------
; ÃüÁîĞĞÖ÷³ÌĞò½áÊø
; ===================================================================
; Ğ¡ĞÍ¸¨ÖúÀı³Ì¿ªÊ¼
; ¶¯»­=================================================================================================================
head1 db	'   ;59935,  ,,:;iirrii:,,.    ...    '
head2 db	'  3#@@@@@#3srr;::,,,,:,:irs;5XHBHXS. '
head3 db	' i@##@#@@8:               .:3@@@@@@H,'
head4 db	' s@###@Hi                    ;B@#@#@h'
head5 db	'  8@@@&.                      :M@@@@r'
head6 db	'   rG&.      .,:rh3Sr;i,;5991. iMMA1 '
head7 db	'    i;       ,13GS3M&h;1BM33h   1r   '
head8 db	'   ;1         ..:ii;.  .iii.    ;S   '
head9 db	'  .3,             .      .      .S   '
head10 db	'.iH3          .isr,.;srrs:r;     93  '
head11 db	'G#@G          ,1r. .:hShi.::     8@G '
head12 db	'A##@S          .;srii;,i1;      .B@@G'
head13 db	'A##@@Ah,       .;, ::iirs.     sA@#@@'
head14 db	'A#@@@@@MGs,          s3Sr  :r3A@@#@@#'
head15 db	'A######@@@M&X31riis1ss158&H#@@@@#####'

squ db 0dbh,0dbh,0dbh,0dbh,0dbh,0dbh,0dbh,0dbh

eng1 db 'This is purple.  ' ;01h
eng2 db 'This is green.   ' ;0ah
eng3 db 'This is gray.    ' ;03h,0ch,0dh,0eh,09h,0fh
eng4 db 'This is red.     '		;0ch
eng5 db 'This is pink.    '	;0dh
eng6 db 'This is yellow.  ';0eh
eng7 db 'This is blue.    ';09h
eng8 db 'This is white.   '	;0fh



chi1 db 'ÕâÊÇ×ÏÉ«'
chi2 db 'ÕâÊÇÂÌÉ«'
chi3 db 'ÕâÊÇ»ÒÉ«'
chi4 db 'ÕâÊÇºìÉ«'
chi5 db 'ÕâÊÇ·ÛÉ«'	;0dh	
chi6 db 'ÕâÊÇ»ÆÉ«'    ;0eh
chi7 db 'ÕâÊÇÀ¶É«'
chi8 db 'ÕâÊÇ°×É«'
chin_len EQU ($-chi8)/2


huanchong db 0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h

ba db 0fh
color_char1 resb 1  ;ÏÔÊ¾ÑÕÉ«
color_ db 01h,02h,0ah,0ch,0dh,0eh,09h,0fh
colornumber db 0
start_c db 59
start:	
	pusha
	push ds
	push es
	mov ax,1000h
	mov es,ax
	mov ds,ax
	
	inc byte [start_c]
	mov al,[start_c]
	cmp al,60
	jnz _3
	mov byte[start_c],0
	mov ax,0
	mov al,[colornumber]
	call show
	
	mov ax,0
	mov al,[colornumber]
	inc ax
	mov [colornumber],al
	cmp ax,8
	jnz _3
	mov ax,0
	mov [colornumber],al
_3:
	pop es
	pop ds
	popa
	ret
	
	
	;mov ah,4ch
	;int 21h
	
show:
	pusha
	push ax
	mov di,color_
	mov cx,ax
ad:
	cmp cx,0
	jz .1
	inc di 
	dec cx
	jmp ad
.1:
	pop ax
	push ax
	mov bx,[di]
	;mov bl,04h
	mov [color_char1],bl
	
	; mov dh,29	;ĞĞ
	; mov dl, 30 		; µÚ10ÁĞ
	; call displayhc
	
	;call displayhc
	
	
	mov bx,7  ;ĞĞ
	mov dl,20 		; µÚ0ÁĞ
	call displayhead
	
	mov bx,11	;ĞĞ
	mov dl, 60 		; µÚ40ÁĞ
	call displaysqu
	pop ax
	push ax
	mov si,eng1
	mov di,chi1
.11:	cmp ax,0
	jz .2
	add si,17
	add di,chin_len
	add di,chin_len
	dec ax
	jmp .11
.2:	
	mov dh,23	;ĞĞ
	mov dl, 30 		; µÚ10ÁĞ
	call displayeng
	
	mov di,8
	mov dx,400
	mov cx,270
.33:	
	push cx
	mov al,0fh
	mov [ba],al
	call hc
	dec di
	pop cx
	add cx,10
	cmp di,0
	jnz .33
	
	mov cx,270
	mov ax,10
	mov bl,[colornumber]
	mul bl
	add cx,ax
	mov dx,400
	mov al,[color_char1]
	mov [ba],al
	call hc
	; mov cx,340
	; mov dx,400
	; mov al,0fh
	; mov [ba],al
	; call hc
	
	
	mov bp,di
	mov dh,22
	mov dl,26
	;call displaychi
	
	
	pop ax
	popa
	ret
	
	
displaychi:
	pusha
	mov ah,42h
	mov cx,chin_len
	
	mov bl,[color_char1]
	int 21h
	popa
	ret
displayeng:	
	pusha
	mov bp, si 	; BP=´®µØÖ·
	mov cx, 17	; ´®³¤
	call display
	popa 
	ret	
	
	
displaysqu:
	pusha
	mov si,squ
	mov cx,10
.1:
	mov bp, si 	; BP=´®µØÖ·
	mov dh,bl
	push cx
	mov cx, 8	; ´®³¤
	
	call display
	pop cx
	
	inc bx
	loop .1
	popa
	ret
	
displayhead:
	pusha
	mov si,head1
	mov cx,15
.1:
	mov bp, si 	; BP=´®µØÖ·
	mov dh,bl
	push cx
	mov cx, 37	; ´®³¤
	
	call display
	pop cx
	add si,37
	inc bx
	loop .1
	popa
	ret
	
displayhc:
	pusha
	push dx
	push cx
	mov dh,49
	mov dl,20
	mov cx,10
	call display
	pop cx
	pop dx
	popa
	ret
	
hc:
	pusha
	; push cx
	; push bx
	; push dx
	; push ax
	
	push dx
	push cx
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	
	
	inc cx
	;mov cx,51
	;mov dx,20
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	inc cx
	;mov cx,52
	;mov dx,20
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	
	pop cx
	push cx
	;mov cx,50
	inc dx
	;mov dx,21
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	inc cx
	;mov cx,51
	;mov dx,21
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	inc cx
	;mov cx,52
	;mov dx,21
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	
	pop cx
	;mov cx,50
	inc dx
	;mov dx,22
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	inc cx
	;mov cx,51
	;mov dx,22
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	inc cx
	;mov cx,52
	;mov dx,22
	mov bh,0
	mov ah,0ch
	mov al,[ba]
	int 10h
	pop dx
	; pop ax
	; pop dx
	; pop bx
	; pop cx
	popa
	ret
	
display:	
	pusha
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, [color_char1] 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	
	;mov dl, 0 		; µÚ0ÁĞ
	;mov bp, head1 	; BP=´®µØÖ·
	;mov cx, 37	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	popa
	ret 
display1:	
	pusha
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	
	;mov dl, 0 		; µÚ0ÁĞ
	;mov bp, head1 	; BP=´®µØÖ·
	;mov cx, 37	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	popa
	ret 
; =======================================================================¶¯»­½áÊø==============================================================================
; ============================AH=3Dh==========================================
cc dw 0
; ============================AH=3Dh=======================================END
int21h: ; int 21hÖĞ¶Ï´¦ÀíÀı³Ì
	cmp ah,4ch
	jnz .1
; ============================AH=4ch==========================================
	mov al, 20h		; AL = EOI
	out 20h, al		; ·¢ËÍEOIµ½Ö÷8529A
	out 0A0h, al	; ·¢ËÍEOIµ½´Ó8529A
	
	; ³õÊ¼»¯¶Î¼Ä´æÆ÷ºÍÕ»Ö¸Õë
	mov ax, cs 		; Í¨¹ıAXÖĞ×ª,  ½«CSµÄÖµ´«ËÍ¸øDS¡¢ESºÍSS
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 100h - 4; ÖÃÕ»¶¥Ö¸ÕëSP=100h-4
	
	mov ah,0fh      ;¶ÁÈ¡ÏÔÊ¾Ä£Ê½
	cmp al,12h      ;ÊÇ·ñÊÇ600*480 Í¼ĞÎÄ£Ê½£¿
	jz .out
	;mov ax,12h      ;Ìø»ØÍ¼ĞÎÄ£Ê½
	;mov bh,0
	;mov bl,0fh
	;int 10h
	;mov ax,0bh     	;ÉèÖÃ±³¾°ÑÕÉ«
	;mov bh,0
	;mov bl,00h		;ºÚÉ«±³¾°
	;int 10h
.out
	jmp again		; ÖØĞÂ¿ªÊ¼ÃüÁîĞĞÑ­»·
; ============================AH=4ch=======================================END
.1:
	cmp ah,3Dh
	jnz AH_3FH
; ============================AH=3Dh==========================================
	; ´ò¿ªHZK16×Ö¿âÎÄ¼ş£¬½«Æä¼ÓÔØµ½58000hÎ»ÖÃ£¬ÓÃÓÚÏÔÊ¾ºº×Ö
    ; ÏÂÃæÔÚAÅÌ¸ùÄ¿Â¼ÖĞÑ°ÕÒ *.BIN
	iret
; ============================AH=3Dh=======================================END
AH_3FH:
	cmp ah,3fh
	jnz AH_42H
; ============================AH=3Fh==========================================
	;¶Á×Ö¿âÎÄ¼ş  Ä¬ÈÏ32×Ö½Ú
	;Èë¿Ú£º CX=¶ÁÈ¡×Ö½ÚÊı   DS£ºDX=Êı¾İ»º³åÇøµØÖ·   ³ö¿Ú£ºÎŞ
	pusha
	push ax
	
	mov al,bh
	call hex2ascii
	pop ax
	mov al,bl
	call hex2ascii
	;mov ax,0b800h
	;mov gs,ax
	
	;mov bh,00001111b
	;mov bl,al
	;mov [gs:(12*80+25)*2],bx
	popa
	iret
; ============================AH=3Fh=======================================END
AH_42H:
	cmp ah,42h
	jnz AH_NULL
; ============================AH=42h==========================================
	;Ô­dosÖĞ¶Ï;ÒÆ¶¯ÒÑ¶ÁÈ¡ÎÄ¼şµÄÖ¸Õë  
	;;Èë¿Ú£º CX£ºDXÎ»ÒÆÁ¿
	;ĞÂcmdÖĞ¶Ï;INT21H AH=42h ÔÚÖ¸¶¨Î»ÖÃÏÔÊ¾ºº×Ö ´®µØÖ·=BP ´®³¤=CX  ĞĞºÅDH ÁĞºÅDL
	pusha
	push es
	push ds       ;±£´æÔ­¼Ä´æÆ÷ĞÅÏ¢£¬ÓÈÆäÊÇes dsµÄµØÖ·
	mov ax,1000h
	mov es,ax
	mov ds,ax
	
	mov [color_char],bl   ;ÉèÖÃÑÕÉ«
	;mov es,ax    ?»Ö¸´es ds ÎªcmdµÄ¶ÎµØÖ· 
	;mov ds,ax
	xor ax,ax     ;ÏñËØµã*16=ĞĞÁĞºÅ
	mov al,dh
	mov bl,16
	mul bl
	mov [line_char],ax
	xor ax,ax
	mov al,dl
	mov bl,16
	mul bl
	
	mov [col_char],ax
	mov [disp_data_len],cx
	push ax
	mov ax,1000h
	mov es,ax
	mov ds,ax
	;mov al,ch
	;call hex2ascii
	;mov al,cl
	;call hex2ascii
	;xor ah,ah
	;int 16h
	pop ax
	shl cx,1      ;CX*2==×Ö·û´®×Ö½ÚÊı
	
	;ds=1000h di=disp_data
	mov di,disp_data
	mov si,bp
	;es=µ÷ÓÃ³ÌĞò¶Î SI=BP²»±ä
	pop ds        ;µ¯³ö³ÌĞòËùÔÚ¶Î
	repe movsb
;.movsb
	;mov al,[ds:si]
	;mov [es:di],al
	;inc si
	;inc di
	;push ax
	;call hex2ascii
	;pop ax
	;loop .movsb
	
	push ds
	mov ax,1000h  ;ÔÙ»Ö¸´es
	mov ds,ax    
	call HZK16_test
	pop ds
	pop es
	popa
	
	iret
DispStr_Chinese:;ÔÚµ±Ç°Î»ÖÃÏÔÊ¾ºº×Ö ´®µØÖ·=BP ´®³¤=CX  
	pusha
	push es
	push ds       ;±£´æÔ­¼Ä´æÆ÷ĞÅÏ¢£¬ÓÈÆäÊÇes dsµÄµØÖ·
	mov ax,1000h
	mov es,ax
	mov ds,ax
	xor ax,ax     ;ÏñËØµã*16=ĞĞÁĞºÅ
	
	push cx
	mov ah,3      ;»ñÈ¡µ±Ç°¹â±êÎ»ÖÃ
	mov bh,0
	int 10h
	pop cx
	mov al,dh
	mov bl,16
	mul bl
	mov [line_char],ax
	xor ax,ax
	mov al,dl
	mov bl,16
	mul bl
	mov [col_char],ax
	mov [disp_data_len],cx
	mov byte[color_char],0fh
	shl cx,1      ;CX*2==×Ö·û´®×Ö½ÚÊı

	mov di,disp_data
	mov si,bp
	;es=µ÷ÓÃ³ÌĞò¶Î SI=BP²»±ä
	repe movsb
	call HZK16_test
	pop ds
	pop es
	popa
	ret
; ============================AH=42h=======================================END
AH_NULL:
	
	iret

; -------------------------------------------------------------------
getdiskparam: ; »ñÈ¡´ÅÅÌ²ÎÊıH/S
	call ReadPBootSec		; µ÷ÓÃ¶ÁÈë´ÅÅÌ·ÖÇøÒıµ¼ÉÈÇøÀı³Ì
	mov ax, [Sector + 18h]	; AX = Ã¿´ÅµÀÉÈÇøÊı
	mov [secspt], ax		; secspt = AX = Ã¿´ÅµÀÉÈÇøÊı
	mov ax, [Sector + 1Ah]	; AX = ´ÅÍ·Êı
	mov [heads], ax			; heads = AX = ´ÅÍ·Êı
	ret						; ´ÓÀı³Ì·µ»Ø
	
; -------------------------------------------------------------------
newline: ; »»ĞĞ£¨ÏÔÊ¾»Ø³µ·ûºÍ»»ĞĞ·û£©
	; ÏÔÊ¾»Ø³µ·ûCR£¨ÖÃµ±Ç°ÁĞºÅ=0£©
	mov ah, 0Eh 	; ¹¦ÄÜºÅ
	mov al, 0Dh 	; ÉèÖÃALÎª»Ø³µ·ûCR£¨ASCIIÂëÎª0DH£©
	mov bl, 0fh 	; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾»»ĞĞ·û£¨µ±Ç°ĞĞºÅ++£©
	mov ah, 0Eh 	; ¹¦ÄÜºÅ
	mov al, 0Ah 	; ÉèÖÃALÎª»»ĞĞ·ûLF£¨ASCIIÂëÎª0AH£©
	mov bl, 0fh 	; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø

; -------------------------------------------------------------------
space: ; ÏÔÊ¾¿Õ¸ñ·û
	mov ah, 0Eh 	; ¹¦ÄÜºÅ
	mov al, 20h 	; ÉèÖÃALÎª¿Õ¸ñ·ûSP£¨ASCIIÂëÎª20H£©
	mov bl, 0fh 	    ; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret			; ´ÓÀı³Ì·µ»Ø
	
; -------------------------------------------------------------------
showwrong: ; ÏÔÊ¾³ö´íĞÅÏ¢
	;call newline 	; »Ø³µ»»ĞĞ
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾³ö´íĞÅÏ¢´®
	;mov ah, 13h 	; ¹¦ÄÜºÅ
	;mov al, 1 		; ¹â±ê·Åµ½´®Î²
	;mov bl, 0fh 	; ÁÁ°×
	;mov bh, 0 		; µÚ0Ò³
	;mov dl, 0 		; µÚ0ÁĞ
	;mov bp, str3 	; BP=´®µØÖ·
	;mov cx, str3len	; ´®³¤
	;int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	;mov cx,buflen
	mov bp,buf
	push bp    ;±£´æbp
	push si
	mov si,0
.1:  
	cmp byte[bp],20h
	jz .2
	cmp byte[bp],0
	jz .2
	inc si
	inc bp
	jmp .1
.2:
	mov cx,si       ;¼ÆËãÃüÁî´®³¤
	pop si
	pop bp
	call DispStr
	mov ah, 0Eh 	; ¹¦ÄÜºÅ
	mov al, ':' 	; ÉèÖÃALÎª¿Õ¸ñ·ûSP£¨ASCIIÂëÎª20H£©
	mov bl, 0fh 	    ; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	mov ah, 2		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	add dl,1
	shr dl,1
	mov bp,str3_chin
	mov cx,str3len_chin
	mov bl,0fh
	;add dh,5
	;pop dx
	;mov dl,0
	;mov dh,25
	;call DispStr_Chinese
	mov ah,42h
	int 21h
	ret				; ´ÓÀı³Ì·µ»Ø
;--------------------------------------------------------------------
showError1: ;ÏÔÊ¾³ö´íĞÅÏ¢ ÌáÊ¾´®³¤=cx ,ÌáÊ¾´®Æ«ÒÆ=bp
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾³ö´íĞÅÏ¢´®
	
	mov ah, 42h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str10_chin	; BP=´®µØÖ·
	mov cx, str10len_chin	; ´®³¤
	int 21h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
;--------------------------------------------------------------------
showError2: ;ÏÔÊ¾³ö´íĞÅÏ¢ ÌáÊ¾´®³¤=cx ,ÌáÊ¾´®Æ«ÒÆ=bp
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾³ö´íĞÅÏ¢´®
	
	mov ah, 42h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str11_chin	; BP=´®µØÖ·
	mov cx, str11len_chin	; ´®³¤
	int 21h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------
showtoolong: ; ÏÔÊ¾Ì«³¤ĞÅÏ¢
	call newline 	; »Ø³µ»»ĞĞ
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾Ì«³¤ĞÅÏ¢´®
	mov ah, 42h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str4_chin	; BP=´®µØÖ·
	mov cx, str4len_chin	; ´®³¤
	int 21h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
Store_dc:
	pusha
	mov bx, 0x70	; BX = 70h£¨ÖĞ¶ÏºÅ£©
	shl bx, 2		; BX << 2£¨BX *= 4£© 
	cli				; ¹Ø±ÕÖĞ¶Ï£¬·ÀÖ¹¸Ä¶¯ÆÚ¼ä·¢ÉúĞÂµÄ0x70ºÅÖĞ¶Ï
	; ÉèÖÃ70hºÅÖĞ¶ÏµÄĞÂÏòÁ¿
	push es			; ±£´æESÈëÕ»
	xor ax, ax		; AX = 0
	mov es, ax		; ES = AX = 0
	mov ax,[es:bx+2]
	mov [Address_70h],ax
	mov ax,[es:bx]
	mov [Address_70h_offset],ax
	pop es
	popa
	sti
	ret
Address_70h dw 0
Address_70h_offset dw 0
Shut_dc:
	pusha
	mov bx, 0x70	; BX = 70h£¨ÖĞ¶ÏºÅ£©
	shl bx, 2		; BX << 2£¨BX *= 4£© 
	cli				; ¹Ø±ÕÖĞ¶Ï£¬·ÀÖ¹¸Ä¶¯ÆÚ¼ä·¢ÉúĞÂµÄ0x70ºÅÖĞ¶Ï
	; ÉèÖÃ70hºÅÖĞ¶ÏµÄĞÂÏòÁ¿
	push es			; ±£´æESÈëÕ»
	xor ax, ax		; AX = 0
	mov es, ax		; ES = AX = 0
	mov ax,[Address_70h]
	mov [es:bx+2],ax
	mov ax,[Address_70h_offset]
	mov [es:bx],ax
	pop es
	popa
	sti
	ret
BackToCmd:      ;ÉèÖÃ¹â±êÑÕÉ«
	pusha 
	;mov ax,0bh  
	;mov bh,00
	;mov bl,0h
	;int 10h
	popa
	call _dc
	ret
;--------------------------------------------------------------------
; Ğ¡ĞÍ¸¨ÖúÀı³Ì½áÊø
; ===================================================================

	
; ===================================================================
; ÄÚ²¿ÃüÁîÀı³Ì¿ªÊ¼
;-------------------------------------------------------------------------------
; ÎÄ¼şÃû×Ö·û´®
FileName_HZK:		db	"HZK16          " ; ×Ö¿âÎÄ¼şÃû
BaseOfFile_HZK	dw	3000h; ×Ö¿âÎÄ¼ş±»¼ÓÔØµ½µÄÎ»ÖÃ ----  ¶ÎµØÖ·
OffsetOfFile_HZK  equ 0h
Current_Base_HZK  dw 0
Current_Offset_HZK  dw 0
Original_Base_HZK dw 3000h
count db 0
BaseOfBuf_HZK		equ 8800h	; ÓÃÓÚ²éÕÒÎÄ¼şÌõÄ¿µÄ»º³åÇø ---- »ùµØÖ·
OffsetOfBuf_HZK	equ	0		; ÓÃÓÚ²éÕÒÎÄ¼şÌõÄ¿µÄ»º³åÇø ---- Æ«ÒÆµØÖ·
int21Name1 db 0dh,0ah,'Loading HZK16  '
int21Name1Len equ $-int21Name1
int21Name2 db 'Reading Word'
int21Name2Len equ $-int21Name2
int21Name3 db 'Setting Point'
int21Name3Len equ $-int21Name3
int21Name4 db 'Load Finish'
int21Name4Len equ $-int21Name4
int21Name5 db 'No file'
int21Name5Len equ $-int21Name5
GS_TEMP dw 0
; ====================================================================
int213dh:
	push es		; ±£»¤ES

; ÈíÇı¸´Î»
	xor	ah, ah	; ¹¦ÄÜºÅah=0£¨¸´Î»´ÅÅÌÇı¶¯Æ÷£©
	xor	dl, dl	; dl=0£¨ÈíÇıA£¬ÈíÇıBÎª1¡¢Ó²ÅÌºÍUÅÌÎª80h£©
	int	13h		; ´ÅÅÌÖĞ¶Ï
	
; ÏÂÃæÔÚ´ÅÅÌÄ¿Â¼ÖĞÑ°ÕÒ ×Ö¿âÎÄ¼ş
	;ÅĞ¶ÏÊÇ¸ùÄ¿Â¼»òÕß×ÓÄ¿Â¼
	push ax
	mov ax,[SectorNoOfCurrentDirectory] 	; ¸ø±íÊ¾µ±Ç°ÉÈÇøºÅµÄ
	mov	word [wSectorNo], ax
						; ±äÁ¿wSectorNo¸³³õÖµÎªµ±Ç°Ä¿Â¼ÇøµÄÊ×ÉÈÇøºÅ
	mov ax, [CurrentDirSectors]	; Ê£ÓàÉÈÇøÊı
	mov word [wRootDirSizeForLoop],ax
										; ³õÊ¼»¯Îªµ±Ç°Ä¿Â¼ËùÕ¼ÉÈÇøÊı£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
	pop ax
LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK:
	cmp	word [wRootDirSizeForLoop], 0 ; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	LABEL_NOT_FOUND_HZK	; Èô¶ÁÍêÔò±íÊ¾Î´ÕÒµ½×Ö¿âÎÄ¼ş
	dec	word [wRootDirSizeForLoop]	; µİ¼õ±äÁ¿wRootDirSizeForLoopµÄÖµ
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸öÄ¿Â¼ÉÈÇøµ½×°ÔØÇø
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı

	mov	si, FileName_HZK		; DS:SI -> ×Ö¿âÎÄ¼ş
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; Çå³ıDF±êÖ¾Î»
						; ÖÃ±È½Ï×Ö·û´®Ê±µÄ·½ÏòÎª×ó/ÉÏ[Ë÷ÒıÔö¼Ó]
	mov	dx, 10h			; Ñ­»·´ÎÊı=16£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
LABEL_SEARCH_FOR_COM_FILE_HZK:
	cmp	dx, 0			; Ñ­»·´ÎÊı¿ØÖÆ
	jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR_HZK ; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø
	dec	dx				; µİ¼õÑ­»·´ÎÊıÖµ			  ¾ÍÌøµ½ÏÂÒ»ÉÈÇø
	mov	cx, 11			; ³õÊ¼Ñ­»·´ÎÊıÎª11
LABEL_CMP_FILENAME_HZK:
	repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND_HZK ; Èç¹û±È½ÏÁË11¸ö×Ö·û¶¼ÏàµÈ£¬±íÊ¾ÕÒµ½
LABEL_DIFFERENT_HZK:
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
						; FFE0h = 1111111111100000£¨µÍ5Î»=32=Ä¿Â¼ÌõÄ¿´óĞ¡£©
	add	di, 20h			; DI += 20h ÏÂÒ»¸öÄ¿Â¼ÌõÄ¿
	mov	si, FileName_HZK		; SIÖ¸Ïò×°ÔØÎÄ¼şÃû´®µÄÆğÊ¼µØÖ·
	jmp	LABEL_SEARCH_FOR_COM_FILE_HZK; ×ªµ½Ñ­»·¿ªÊ¼´¦

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR_HZK:             ;ssssss
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	LABEL_NOT_FOUND_HZK ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc	word [wSectorNo]	; ¶ÔÓÚ¸ùÄ¿Â¼£¬µİÔöµ±Ç°ÉÈÇøºÅ
	
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK

LABEL_NOT_FOUND_HZK:
	pop es			; »Ö¸´ES
	;call showwrong	; ÏÔÊ¾×Ö·û´®
	;jmp $
	ret

; ÏÂÃæ½«×Ö¿âÎÄ¼ş¼ÓÔØµ½ÄÚ´æ
LABEL_FILENAME_FOUND_HZK:	; ÕÒµ½ ×Ö¿âÎÄ¼şºó±ãÀ´µ½ÕâÀï¼ÌĞø
	; ¼ÆËãÎÄ¼şµÄÆğÊ¼ÉÈÇøºÅ
	mov	ax, [CurrentDirSectors]	; AX=µ±Ç°Ä¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
	and	di, 0FFE0h		; DI -> µ±Ç°ÌõÄ¿µÄ¿ªÊ¼µØÖ·
	add	di, 1Ah			; DI -> ÎÄ¼şµÄÊ×ÉÈÇøºÅÔÚÌõÄ¿ÖĞµÄÆ«ÒÆµØÖ·
	mov cx, word [es:di] ; CX=ÎÄ¼şµÄÊ×ÉÈÇøºÅ
	push cx				; ±£´æ´ËÉÈÇøÔÚFATÖĞµÄĞòºÅ
	add	cx, RootDirSectors			; CX=ÎÄ¼şµÄÏà¶ÔÆğÊ¼ÉÈÇøºÅ+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı +¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
	;ÖØÒªµÄÊÂÇéËµÒ»Íò±é=_=,ÕÒÕâ¸öbugÓÃÁË¼¸Ğ¡Ê±   Ô­´úÂëadd	cx,ax   ÏÖÔÚ×ÓÄ¿Â¼ax²¢²»ÊÇ¸ùÄ¿Â¼Ê×ÉÈÇøºÅ
	add	cx, DeltaSectorNo ; CL <- COMÎÄ¼şµÄÆğÊ¼ÉÈÇøºÅ(0-based)
	mov	ax, [BaseOfFile_HZK]      ;+1C
	mov	es, ax			; ES <- BaseOfLoader£¨COM³ÌĞò»ùÖ·=4000h£©
	mov	bx, OffsetOfFile_HZK ; BX <- OffsetOfLoader£¨COM³ÌĞòÆ«ÒÆµØÖ·=100h£©
	mov	ax, cx			; AX <- ÆğÊ¼ÉÈÇøºÅ
LABEL_GOON_LOADING_FILE_HZK:
	push bx				; ±£´æ×Ö¿â³ÌĞòÆ«ÒÆµØÖ·
	mov	cl, 1			; 1¸öÉÈÇø
	call ReadSec		; ¶ÁÉÈÇø

	; ¼ÆËãÎÄ¼şµÄÏÂÒ»ÉÈÇøºÅ
	pop bx				; È¡³ö×Ö¿â³ÌĞòÆ«ÒÆµØÖ·
	pop	ax				; È¡³ö´ËÉÈÇøÔÚFATÖĞµÄĞòºÅ
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	cmp	ax, 0FF8h		; ÊÇ·ñÊÇÎÄ¼ş×îºó´Ø
	jae	LABEL_FILE_LOADED_HZK ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	push ax				; ±£´æÉÈÇøÔÚFATÖĞµÄĞòºÅ
	mov	dx, RootDirSectors	; DX = ¸ùÄ¿Â¼ÉÈÇøÊı
	add	ax, dx			; ÉÈÇøĞòºÅ + ¸ùÄ¿Â¼ÉÈÇøÊı
	add	ax, DeltaSectorNo ; AX = Òª¶ÁµÄÊı¾İÉÈÇøµØÖ·
	;add	bx, [BPB_BytsPerSec] ; BX+512Ö¸Ïò×Ö¿âµÄÏÂÒ»¸öÉÈÇøµØÖ·
	mov bx,0
nextPara:
	push ax
	mov ax,es
	add ax,20h
	mov es,ax
	pop ax
nextParaEnd:
    pusha
	call start
	popa
	jmp	LABEL_GOON_LOADING_FILE_HZK

; ÏÂÃæÌø×ªÖ´ĞĞCOM³ÌĞò
LABEL_FILE_LOADED_HZK:
	pop es
	;add sp,2
	;jmp	BaseOfLoader:OffsetOfLoader	; ÕâÒ»¾äÌø×ªµ½ÒÑ¼ÓÔØµ½ÄÚ´æÖĞµÄ
	ret
;------------------------------------------------------------------------------------------------
;21ºÅÖĞ¶Ï ¹¦ÄÜºÅah=42h
line_char DW 2    ;ÔÚÆÁÄ»ÉÏµÚ¼¸ĞĞÏÔÊ¾
col_char DW 2    ;ÔÚÆÁÄ»ÉÏµÚ¼¸ÁĞÏÔÊ¾
color_char db 0FH  ;ÏÔÊ¾ÑÕÉ«    LRGB
HZK16_test:
  pusha
  jmp install
  disp_data1 DB  'ÁÎÎ¬Ã÷'  
  disp_data resb 1024   ;ºº×Ö»º³åÇø£¬´æ·ÅÒªÏÔÊ¾µÄºº×Ö
  disp_data_len dw 0   ;ºº×Ö×Ö·û´®³¤
  ;chars EQU ($-disp_data)/2
  ;DISP_DATA_END EQU THIS BYTE
  zi_buffer resb 1280  ;Ò»ĞĞ¿ÉÏÔÊ¾40¸öºº×Ö,40*32b=1280byte
  OriginalBase dw 3000h
install:
  mov ax,1000h
  mov es,ax
  mov ds,ax
  
  mov si,disp_data
  mov di,zi_buffer
  mov cx,[disp_data_len]
  cld
ins2:
  push cx
  mov ah,[si]
  inc si
  mov al,[si]
  inc si  
  
  call get_dots    ;¶Á³öºº×ÖµãÕë
  pop cx
  loop ins2
  call disp_cc    ;ÏÔÊ¾µ½ÆÁÄ»
;sloop0:
  ;mov ah,01
  ;int 16h
  ;cmp al,'s'
  ;jz out_
  ;jmp sloop0
out_:
  ;mov ax,3
  ;int 10h
  mov ax,1000h
  mov es,ax
  mov ds,ax
  pusha
  mov ah,3
  mov bh,0
  int 10h
  mov ax,[col_char]
  mov bl,8     ;×ª»¯Îª×ÖÄ¸ÁĞºÅ
  div bl
  add ax,[disp_data_len]
  add ax,[disp_data_len]
  mov dl,al
  mov ah,2
  mov bh,0
  int 10h
  popa
  
  popa
  ret
  
get_dots:
  pusha
  push es
  push ds
  sub ax,0a1a1h   ;ºº×ÖµÄÄÚÂë´Ó A1Çø¿ªÊ¼
  cwd  ;ax?©å??°dxï¼Œax
  mov dl,al    ;ËùÒÔ¾ø¶Ô¿ªÊ¼ÇøÊÇÄÚÂë-A1
  mov al,ah    ;µãÕóÔÚ×Ö¿âÖĞµÄÎ»ÖÃÎª 
  cbw
  mov bl,94    ;£¨£¨ºº×ÖÂë1-A1£©* 94 + ºº×ÖÂë2 - A1£©* 32
  mul bl
  add ax,dx
  mov bx,32
  mul bx  ;   dxï¼Œax
  mov cx,dx  ;cxï¿?
  mov dx,ax  ;dxï¿?
  ;mov ax,4200h    ;ÒÆ¶¯¶ÁĞ´Ö¸Õëµ½µãÕóÊı¾İÎ»ÖÃ
  call Int21h
  pop ds
  pop es
  popa
  add di,32
  ret

disp_cc:     ;6666
  mov cx,[disp_data_len]
  mov si,zi_buffer
  mov bx,[col_char]
  sub bx,16   ;BX = column
dh_lop0:
  add bx,16    ;every char column+20
  push cx
  mov cx,16      ;l6 lines/char
  mov dx,[line_char]      ;DX = start line
dh_lop1:
  push bx
  push cx
  lodsb         ;16 dots/line
  mov ah,al
  lodsb
  mov cx,16
dh_lop2:
  shl ax,1
  push ax
  push bx
  push cx
  jc db_color
  xor al,al          ;back color is 0
  jmp short db_draw
db_color:
  mov al,[color_char]
db_draw:
  mov ah,0ch
  mov cx,bx
  xor bh,bh
  int 10h
  pop cx
  pop bx
  pop ax
  inc bx   ;inc column
  loop dh_lop2
  inc dx   ;next line
  pop cx
  pop bx
  loop dh_lop1
  pop cx
  loop dh_lop0
  
  ;mov cx,5
  ;mov si,0
;.21
  ;push cx
  ;mov cx,es
  ;mov dx,zi_buffer
  ;add dx,si
  ;add si,16
  ;call ReadMemmory_CHINESE
  ;call space
  ;call space
  ;call space
  ;call space
  ;call space
  ;call space
  ;pop cx
  ;loop .21
  ;mov cx,4000h
  ;mov dx,0b040h
  ;call ReadMemmory_CHINESE
  ;mov ah,0
  ;int 16h
  ret
Int21h:
	pusha
	push ds
	push es

	;mov ax,ds
	;mov es,ax    ;ES  DIÎªÄ¬ÈÏÖµ
	
	;mov ax,[OriginalBase]
	push bx
	push cx
	mov ax,0
	mov bx,0
	shrd bx,cx,4
	add ax,bx
	add ax,[OriginalBase]
	mov ds,ax    ;DS=3000h+CX*1000h
	
	pop cx
	pop bx
	;push ax
	;mov al,ah
	;call hex2ascii
	;pop ax
	;call hex2ascii
	
	mov si,dx	 ;SI=DX

	;push ax
	;mov al,dh
	;call hex2ascii
	;mov al,dl
	;call hex2ascii
	;pop ax
	
	mov bx,0
	cld
.1:
	mov al,[ds:si]
	;mov al,0fah
	;call hex2ascii
	mov [es:di],al
	inc si
	inc di
	inc bx
	cmp bx,32
	jz Int21h_Out
	jmp .1
Int21h_Out:
	pop es
	pop ds
	popa
	ret
; -------------------------------------------------------------------
ReadMemmory_CHINESE:   ;ÏÔÊ¾ CX:DX´¦16×Ö½ÚµÄÄÚ´æĞÅÏ¢
	pusha
	push ds
	mov ax,cx
	mov ds,ax
	mov si,dx
	mov di,0
.1:
	lodsb
	;mov al,0fah
	call hex2ascii
	inc di
	cmp di,16
	jz ReadMemmory_Out_CHINESE
	call space
	jmp .1
ReadMemmory_Out_CHINESE:
	pop ds
	popa
	ret
; -------------------------------------------------------------------
;-------------------------------------------------------------------------------
hex2ascii:  ;16½øÖÆ×ªASCII£¨ÊäÈë£ºAL = BCDÂë£¬Êä³ö£ºAX = ASCII£©
	pusha
	push ds
	push es
	push ax
	mov ax,1000h
	mov es,ax
	mov ds,ax
	pop ax
	mov dx,ax
	mov dh,dl
	push dx
	;------»ñÈ¡¸ßËÄÎ»---------;
	mov bx,0
	;shld bx,dx,4
	mov bx,dx
	shr bx,4
	and bl,0fh
	mov al,bl
	call ShowChar_HZK
	;------»ñÈ¡µÍËÄÎ»---------;
	mov bx,0
	mov bx,dx
	and bl,0fh
	mov al,bl
	call ShowChar_HZK
	
	pop dx
	
	POP ES
	POP ds
	popa
	ret
;-------------------------------------------------------------------------------
Restart: ;ÖØÆô²Ù×÷ÏµÍ³
	int 19h
	ret
; -------------------------------------------------------------------	
; ÏÔÊ¾µ¥¸öÊ®Áù½øÖÆ×Ö·ûº¯Êı
ShowChar_HZK: ; ÏÔÊ¾Ò»¸öÊ®Áù½øÖÆÊı×Ö·û£º0~9¡¢A~F£¨ÒÔALÎª´«µİ²ÎÊı£©
	cmp al, 10		; AL < 10 ?
	jl .1			; AL < 10£ºÌø×ªµ½.1
	add al, 7		; AL >= 10£ºÏÔÊ¾×ÖÄ¸£¨ = ÊıÖµ += 37h£©
.1: ; Êı×Ö
	add al, 30h		; Êı×Ö×Ö·û = ÊıÖµ+=30h
	mov ah, 0Eh		; ¹¦ÄÜºÅ£¨ÒÔµç´«·½Ê½ÏÔÊ¾µ¥¸ö×Ö·û£©
	mov bl, 0fh 	; ¶ÔÎÄ±¾·½Ê½ÖÃ0
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------
;-------------------------------------------------------------------------------
new_int_0x70_dc: ; 70hºÅĞÂÖĞ¶Ï´¦Àí³ÌĞòÈë¿Ú
	; ±£´æ½«ÒªÊ¹ÓÃµÄ¼Ä´æÆ÷ÒÔÃâ±»ÆÆ»µ
	push ax
	push bx
	push es
	
  wait0: ; ÅĞ¶ÏÊÇ·ñ¿É¶ÁÈÕÆÚÓëÊ±¼äĞÅÏ¢	
	; ´Ë¶Î´úÂë¶ÔÓÚ¸üĞÂÖÜÆÚ½áÊøÖĞ¶ÏÀ´ËµÊÇ²»±ØÒªµÄ
	mov al, 0x0a	; Ö¸¶¨¼Ä´æÆ÷A
	or al, 0x80		; ×è¶ÏNMI¡£µ±È»£¬Í¨³£ÊÇ²»±ØÒªµÄ	   
	out 0x70, al	; Êä³öALµ½¶Ë¿Ú70h£¬Ñ¡Ôñ¼Ä´æÆ÷A
	in al, 0x71		; ¶Á¼Ä´æÆ÷A
	test al, 0x80	; ²âÊÔµÚ7Î» = 0£¿ 
	jnz wait0		; ¡Ù 0Ê±£¨ÈÕÆÚÓëÊ±¼äÔÚ¸üĞÂÖĞ£©ĞèµÈ´ı

	; »ñÈ¡µ±Ç°µÄÊ±¼äĞÅÏ¢
	; »ñÈ¡ÃëĞÅÏ¢
	xor al, al		; AL = 0
	out 0x70, al	; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 0x71		; ¶ÁRTCµ±Ç°Ê±¼ä(Ãë)
	push ax			; ½«»ñÈ¡µÄÊı¾İALÑ¹Õ»±£´æ
	; »ñÈ¡·ÖĞÅÏ¢
	mov al, 2		; AL = 2
	out 0x70, al	; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 0x71		; ¶ÁRTCµ±Ç°Ê±¼ä(·Ö)
	push ax			; ½«»ñÈ¡µÄÊı¾İALÑ¹Õ»±£´æ
	; »ñÈ¡Ê±ĞÅÏ¢
	mov al, 4		; AL = 4
	out 0x70, al	; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 0x71		; ¶ÁRTCµ±Ç°Ê±¼ä(Ê±)
	push ax			; ½«»ñÈ¡µÄÊı¾İALÑ¹Õ»±£´æ
	; ¶ÁÈ¡¼Ä´æÆ÷C
	mov al, 0x0c	; Ö¸¶¨¼Ä´æÆ÷C£¬ÇÒ¿ª·ÅNMI 
	out 0x70, al	; Êä³öALµ½¶Ë¿Ú70h£¬Ñ¡Ôñ¼Ä´æÆ÷C
	in al, 0x71		; ¶ÁRTCµÄ¼Ä´æÆ÷C£¬·ñÔòÖ»·¢ÉúÒ»´ÎÖĞ¶Ï
					; ´Ë´¦²»¿¼ÂÇÄÖÖÓºÍÖÜÆÚĞÔÖĞ¶ÏµÄÇé¿ö 
	
	; ÔÚÆÁÄ»ÓÒÉÏ½ÇÏÔÊ¾Ê±¼äĞÅÏ¢
	; ÖÃES = ÏÔ´æ»ùÖ·
	mov ax,0x1000	; AX = B800h£¨²ÊÉ«ÎÄ±¾ÆÁÄ»ÏÔ´æµÄÆğÊ¼µØÖ· >> 4£©
	mov es,ax		; ES = AX = B800h£¨ES = ÏÔ´æ»ùÖ·£©
	; ÉèÖÃÊ±¼ä´®µÄÆğÊ¼Î»ÖÃ
	mov bx, (0*80 + 72)*2; ´ÓÆÁÄ»ÉÏµÄµÚ0ĞĞ72ÁĞ¿ªÊ¼ÏÔÊ¾
	mov ah,3
	mov bh,0
	int 10h
	mov [es:TEMP_DX],dx         ;±£´æ¹â±êÎ»ÖÃ
	mov ah,2
	mov dh,0
	mov dl,72    
	mov bh,0
	int 10h  		;ÉèÖÃ¹â±êÎ»ÖÃÎª0 72
	; ÏÔÊ¾Ê±
	pop ax			; ´ÓÕ»ÖĞµ¯³öÊ±
	call bcd2ascii	; µ÷ÓÃBCD×ªASCIIÀı³Ì
	; ÏÔÊ¾Á½Î»Ğ¡Ê±Êı×Ö
	;mov [es:bx], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 2], al
	call ShowChar_dt
	; ÏÔÊ¾·Ö¸ô·û':'
	mov al,':'
	;mov [es:bx + 4], al
	call ShowChar_dt
	; ÏÔÊ¾·Ö
	pop ax			; ´ÓÕ»ÖĞµ¯³ö·Ö
	call bcd2ascii	; µ÷ÓÃBCD×ªASCIIÀı³Ì
	; ÏÔÊ¾Á½Î»·ÖÖÓÊı×Ö
	;mov [es:bx + 6], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 8], al
	call ShowChar_dt
	; ÏÔÊ¾·Ö¸ô·û':'
	mov al,':'
	;mov [es:bx + 10], al
	call ShowChar_dt
	; ÏÔÊ¾Ãë
	pop ax			; ´ÓÕ»ÖĞµ¯³öÃë
	call bcd2ascii	; µ÷ÓÃBCD×ªASCIIÀı³Ì
	; ÏÔÊ¾Á½Î»Ğ¡Ê±Êı×Ö
	;mov [es:bx + 12], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 14], al
	call ShowChar_dt
	
	mov dx,[es:TEMP_DX]          ;»Ö¸´¹â±êÎ»ÖÃ
	mov ah,2
	mov bh,0
	int 10h
	; ·¢ËÍEOI¸ø8259A
	mov al, 0x20	;ÖĞ¶Ï½áÊøÃüÁîEOI 
	out 0xa0, al	;Ïò´ÓÆ¬·¢ËÍ 
	out 0x20, al	;ÏòÖ÷Æ¬·¢ËÍ 

	; »Ö¸´±£´æµÄ¼Ä´æÆ÷Öµ
	pop es
	pop bx
	pop ax

	iret			; ´ÓÖĞ¶Ï·µ»Ø
TEMP_DX DW 0
; -------------------------------------------------------------------
_dt:
	pusha 
	; »ñÈ¡ÄêĞÅÏ¢
	mov al, 9			; ÄêµÄÆ«ÒÆµØÖ·Îª9
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÄêĞÅÏ¢
	; ÏÔÊ¾ÄêĞÅÏ¢
	call ShowBCD	; ÏÔÊ¾BCDÊ®½øÖÆÊı
	; ÏÔÊ¾¾äµã·Ö¸ô·û
	;mov al, '.'			; AL = '.'
	;call ShowChar_dt		; ÏÔÊ¾×Ö·û
	mov bx,0
	add bx,date_str
	mov bp,bx
	mov cx,1
	push ax
	push cx
	mov ah,3
	mov bh,0
	int 10h
	pop cx
	pop ax
	mov dl,1
	;call DispStr_Chinese		; ÏÔÊ¾×Ö·û´®
	push ax
	mov bl,0fh          ;ÁÁ°×É«
	mov ah,42h
	int 21h
	pop ax
	; »ñÈ¡ÔÂĞÅÏ¢
	mov al, 8			; ÔÂµÄÆ«ÒÆµØÖ·Îª8
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÔÂĞÅÏ¢
	; ÏÔÊ¾ÔÂĞÅÏ¢
	call ShowBCD	; ÏÔÊ¾BCDÊ®½øÖÆÊı
	; ÏÔÊ¾¾äµã·Ö¸ô·û
	;mov al, '.'			; AL = '.'
	;call ShowChar_dt		; ÏÔÊ¾×Ö·û
	mov bx,2
	add bx,date_str
	mov bp,bx
	mov cx,1
	push ax
	push cx
	mov ah,3
	mov bh,0
	int 10h
	pop cx
	pop ax
	mov dl,3
	;call DispStr_Chinese		; ÏÔÊ¾×Ö·û´®
	push ax
	mov bl,0fh          ;ÁÁ°×É«
	mov ah,42h
	int 21h
	pop ax
	
	; »ñÈ¡ÈÕĞÅÏ¢
	mov al, 7			; ÈÕµÄÆ«ÒÆµØÖ·Îª7
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÈÕĞÅÏ¢
	; ÏÔÊ¾ÈÕĞÅÏ¢
	call ShowBCD	; ÏÔÊ¾BCDÊ®½øÖÆÊı
	mov bx,4
	add bx,date_str
	mov bp,bx
	mov cx,1
	push ax
	push cx
	mov ah,3
	mov bh,0
	int 10h
	pop cx
	pop ax
	mov dl,5
	;call DispStr_Chinese		; ÏÔÊ¾×Ö·û´®
	push ax
	mov bl,0fh          ;ÁÁ°×É«
	mov ah,42h
	int 21h
	pop ax
	; ÏÔÊ¾¿Õ¸ñ·Ö¸ô·û
	mov al, ' '			; AL = ' '
	call ShowChar_dt		; ÏÔÊ¾×Ö·û
	
	; »ñÈ¡ĞÇÆÚĞÅÏ¢
	mov al, 6			; ĞÇÆÚµÄÆ«ÒÆµØÖ·Îª6
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëĞÇÆÚĞÅÏ¢
	; ÏÔÊ¾ĞÇÆÚĞÅÏ¢
	dec al			; AL --
	mov bl, 6			; BL = 3
	mul bl			; AX = AL * BL
	add ax, weekstrs_chin	; AX += weekstrs
	mov bp, ax		; BP = AX Ö¸Ïò¶ÔÓ¦ĞÇÆÚ´®
	mov cx, 3			; ´®³¤ CX = 3
	push ax
	push cx
	mov ah,3
	mov bh,0
	int 10h
	pop cx
	pop ax
	mov dl,7
	;call DispStr_Chinese		; ÏÔÊ¾×Ö·û´®
	push ax
	mov bl,0fh          ;ÁÁ°×É«
	mov ah,42h
	int 21h
	pop ax
	; ÏÔÊ¾¿Õ¸ñ·Ö¸ô·û
	mov al, ' '			; AL = ' '
	call ShowChar_dt		; ÏÔÊ¾×Ö·û
	
	; »ñÈ¡Ê±ĞÅÏ¢
	mov al, 4			; Ê±µÄÆ«ÒÆµØÖ·Îª4
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÊ±ĞÅÏ¢
	; ÏÔÊ¾Ê±ĞÅÏ¢
	call ShowBCD	; ÏÔÊ¾BCDÊ®½øÖÆÊı
	; ÏÔÊ¾Ã°ºÅ·Ö¸ô·û
	mov al, ':'			; AL = ':'
	call ShowChar_dt		; ÏÔÊ¾×Ö·û

	; »ñÈ¡·ÖĞÅÏ¢
	mov al, 2			; ·ÖµÄÆ«ÒÆµØÖ·Îª2
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈë·ÖĞÅÏ¢
	; ÏÔÊ¾·ÖĞÅÏ¢
	call ShowBCD	; ÏÔÊ¾BCDÊ®½øÖÆÊı
	; ÏÔÊ¾Ã°ºÅ·Ö¸ô·û
	mov al, ':'			; AL = ':'
	call ShowChar_dt		; ÏÔÊ¾×Ö·û

	; »ñÈ¡ÃëĞÅÏ¢
	mov al, 0			; ÃëµÄÆ«ÒÆµØÖ·Îª0
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÃëĞÅÏ¢
	; ÏÔÊ¾ÃëĞÅÏ¢
	call ShowBCD	; ÏÔÊ¾BCDÊ®½øÖÆÊı
	; ÉèÖÃ¹â±êÎ»ÖÃ
	mov ah, 2		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	mov dl, 0		; ÁĞºÅ
	int 10h			; ÏÔÊ¾ÖĞ¶Ï
	; ÍË»ØDOS
	popa	
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©  ;»Ö¸´ĞĞºÅ·½±ãnewlineÊ¹ÓÃ
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	ret
date_str:
	db 'Äê'
	db 'ÔÂ'
	db 'ÈÕ'
weekstrs: ; ¶¨ÒåĞÇÆÚ´®Êı×é
	db 'Sun'
	db 'Mon'
	db 'Tue'
	db 'Wed'
	db 'Thu'
	db 'Fri'
	db 'Sat'
weekstrs_chin:
	db 'ĞÇÆÚÈÕ'
	db 'ĞÇÆÚÒ»'
	db 'ĞÇÆÚ¶ş'
	db 'ĞÇÆÚÈı'
	db 'ĞÇÆÚËÄ'
	db 'ĞÇÆÚÎå'
	db 'ĞÇÆÚÁù'
; -------------------------------------------------------------------
_dc:
	pusha
	; ÉèÖÃ70hĞÂÖĞ¶ÏÏòÁ¿
	; ¼ÆËã70hºÅÖĞ¶ÏÔÚIVTÖĞµÄÆ«ÒÆ
	mov bx, 0x70	; BX = 70h£¨ÖĞ¶ÏºÅ£©
	shl bx, 2		; BX << 2£¨BX *= 4£© 
	cli				; ¹Ø±ÕÖĞ¶Ï£¬·ÀÖ¹¸Ä¶¯ÆÚ¼ä·¢ÉúĞÂµÄ0x70ºÅÖĞ¶Ï
	; ÉèÖÃ70hºÅÖĞ¶ÏµÄĞÂÏòÁ¿
	push es			; ±£´æESÈëÕ»
	xor ax, ax		; AX = 0
	mov es, ax		; ES = AX = 0
	mov word [es:bx], new_int_0x70_dc ; Æ«ÒÆµØÖ·
	mov word [es:bx + 2], cs ; ¶ÎµØÖ·
	pop es			; ´ÓÕ»ÖĞ»Ö¸´ES

	; ÉèÖÃRTC×´Ì¬¼Ä´æÆ÷B
	mov al, 0x0b	; Ö¸¶¨RTC¼Ä´æÆ÷B
	or al, 0x80		; ×è¶ÏNMI 
	out 0x70, al	; Ñ¡Ôñ¼Ä´æÆ÷B
	mov al, 0x12	; ½ûÖ¹ÖÜÆÚĞÔºÍÄÖÖÓÖĞ¶Ï£¬Ö»¿ª·Å¸üĞÂ½áÊøºóÖĞ¶Ï£¬²ÉÓÃBCDÂëºÍ24Ğ¡Ê±ÖÆ  00010010
	out 0x71, al	; ÉèÖÃ¼Ä´æÆ÷B 
	; ¶ÁÈ¡RTC×´Ì¬¼Ä´æÆ÷C
	mov al, 0x0c	; Ö¸¶¨RTC¼Ä´æÆ÷C£¬¿ª·ÅNMI
	out 0x70, al	; Ñ¡Ôñ¼Ä´æÆ÷C
	in al, 0x71		; ¶ÁRTC¼Ä´æÆ÷C£¬¸´Î»Î´¾öµÄÖĞ¶Ï×´Ì¬

	; ´ò¿ª´Ó8259AµÄIRQ0£¨RTC£©ÖĞ¶Ï
	in al, 0xa1		; ¶Á´Ó8259AµÄIMR¼Ä´æÆ÷ 
	and al, 0xfe	; Çå³ıbit0£¨´ËÎ»Á¬½ÓRTC£©
	out 0xa1, al	; Ğ´»Ø´Ë¼Ä´æÆ÷ 

	sti				; ÖØĞÂ¿ª·ÅÖĞ¶Ï 
	
	;jmp $			; Èç¹ûÔÚDOSÏÂÔËĞĞ£¬ĞèÓÃ´ËËÀÑ­»·´úÌæÏÂÃæµÄÍË³öDOSÖĞ¶Ï

	; ÍË»ØDOS
	popa
	
	ret
;--------------------------------------------------------------------
hex2bcd:  ;al ascii×ªÎªÊ®Áù½øÖÆ   alÎªÆä16½øÖÆÊıÖµ
	cmp al, 3ah		; AL ÊÇÊı×Ö ?
	jl .1			; AL < 10£ºÌø×ªµ½.1
	sub al, 7		; AL >= 10£ºÏÔÊ¾×ÖÄ¸£¨ = ÊıÖµ += 37h£©
.1: ; Êı×Ö
	sub al, 30h		; 
	ret
; -------------------------------------------------------------------
ReadMemmory:   ;ÏÔÊ¾ X:X´¦16×Ö½ÚµÄÄÚ´æĞÅÏ¢
	mov bp,buf
	add bp,8   ;Ìø¹ıREADMEM   8¸ö×Ö·û
	mov cx,9
	push bp
	;call DispStr
	pop bp
	mov si,0
	mov cx,0   ;½«Ã°ºÅÇ°4¸öasciiÂë×ªÎªÊıÖµ·ÅÈëcx
.1:  
	mov ax,0
	mov al,byte[bp]
	inc si
	inc bp
	call hex2bcd
	mov ah,al
	shl ah,4       ;½«×Ö·ûÒÆ¶¯ÖÁ¸ßÎ»
	shld cx,ax,4   ;Ã¿´ÎÒÆ¶¯ËÄÎ»,8140  8 1 4 0
	cmp si,4
	jz .2
	jmp .1
.2:
	mov si,0   ;½«Ã°ºÅºó4¸öasciiÂë×ªÎªÊıÖµ·ÅÈëdx
	mov dx,0
	inc bp
.3:
	mov ax,0
	mov al,byte[bp]
	inc si
	inc bp
	call hex2bcd
	mov ah,al
	shl ah,4
	shld dx,ax,4   ;Ã¿´ÎÒÆ¶¯ËÄÎ»,8140  8 1 4 0
	cmp si,4
	jz .4
	jmp .3
.4:
	;call space
	;mov al,ch
	;call show_hex2ascii
	;mov al,cl
	;call show_hex2ascii
	;call space
	;mov al,dh
	;call show_hex2ascii
	;mov al,dl
	;call show_hex2ascii
	;call space
;-------------------------------------------------------------------1
;ÏÔÊ¾ CX:DX´¦16×Ö½ÚµÄÄÚ´æĞÅÏ¢
	pusha
	push ds
	push es
	mov ax,cx
	mov ds,ax
	mov si,dx
	mov di,0
	cld
.11:
	lodsb
	;mov al,0fah
	call show_hex2ascii
	inc di
	cmp di,15
	jz ReadMemmory_Out
	call space
	jmp .11
ReadMemmory_Out:
	pop es
	pop ds
	popa
	; ¶ÁÈ¡¹â±êÎ»ÖÃ
	mov ah, 3		; ¹¦ÄÜºÅ
	int 10h
	; ÉèÖÃ¹â±êÎ»ÖÃ
	mov ah, 2		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h			; ÏÔÊ¾ÖĞ¶Ï
	add sp,2
	jmp again	
; -------------------------------------------------------------------
ShowChar_dt: ; ÏÔÊ¾µ¥¸ö×Ö·û£¨ÒÔALÎª´«µİ²ÎÊı£©
	mov ah, 0Eh		; ¹¦ÄÜºÅ£¨ÒÔµç´«·½Ê½ÏÔÊ¾µ¥¸ö×Ö·û£©
	mov bl, 0fh 		; ÁÁ°××Ö
	int 10h 			; µ÷ÓÃ10HºÅÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
;-------------------------------------------------------------------------------
show_hex2ascii:  ;ÏÔÊ¾16½øÖÆ×ªASCII£¨ÊäÈë£ºAL = BCDÂë£¬Êä³ö£ºAX = ASCII£©
	pusha
	mov dx,ax
	mov dh,dl
	push dx
	;------»ñÈ¡¸ßËÄÎ»---------;
	mov bx,0
	;shld bx,dx,4
	mov bx,dx
	shr bx,4
	and bl,0fh
	mov al,bl
	call ShowChar
	;------»ñÈ¡µÍËÄÎ»---------;
	mov bx,0
	mov bx,dx
	and bl,0fh
	mov al,bl
	call ShowChar
	
	pop dx
	popa
	ret
;-------------------------------------------------------------------------------
bcd2ascii: ;BCDÂë×ªASCII£¨ÊäÈë£ºAL = BCDÂë£¬Êä³ö£ºAX = ASCII£©
	mov ah, al		; AH = AL£¨·Ö²ğ³ÉÁ½¸öÊı×Ö£©
	and al, 0x0f	; AL & 0Fh£¨È¡BCDµÄµÍ4Î»Êı¾İ£©
	add al, 0x30	; AL += 30h£¨×ª»»³ÉASCII£©
	shr ah, 4		; AH >> 4£¨È¡BCDµÄ¸ß4Î»Êı¾İ£©
	add ah, 0x30	; AH += 30h£¨×ª»»³ÉASCII£©
	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------	
ShowBCD: ; ÏÔÊ¾µ¥×Ö½ÚBCDÊ®½øÖÆÊı£¨ÒÔALÎª´«µİ²ÎÊı£©
	push ax			; ±£´æAL½øÕ»
	shr al, 4			; AL >> 4 £¨¸ßÎ»Êı×Ö£©
	add al, 30h		; Êı×Ö×Ö·û = ÊıÖµ+=30h
	call ShowChar_dt		; ÏÔÊ¾×Ö·û
	pop ax			; ´ÓÕ»ÖĞ»Ö¸´AL
	and al, 0Fh		; È¡ALµÄµÍ4Î»
	add al, 30h		; Êı×Ö×Ö·û = ÊıÖµ+=30h
	call ShowChar_dt		; ÏÔÊ¾×Ö·û
	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------
ver: ; ÏÔÊ¾°æÈ¨ĞÅÏ¢
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾°æÈ¨×Ö·û´® 'MyOS 1.x  (C) 2016 CANNON OS'
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str1 	; BP=´®µØÖ·
	mov cx, str1len	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø

; -------------------------------------------------------------------
DispStr_HZK: ; ÏÔÊ¾×Ö·û´®
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï

	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø

; -------------------------------------------------------------------
ver0: ; ÏÔÊ¾°æÈ¨ĞÅÏ¢
	pusha
	push dx
	mov dh,0
	; ÏÔÊ¾°æÈ¨×Ö·û´® 'MyOS 1.x  (C) 2016 CANNON OS'
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str1 	; BP=´®µØÖ·
	mov cx, str1len	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	pop dx
	
	mov ah,02h
	mov bh,0
	int 10h
	popa
	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------
cls: ; ÇåÆÁ
	mov	ah, 6		; ¹¦ÄÜºÅ
	mov	al, 0		; ¹ö¶¯µÄÎÄ±¾ĞĞÊı£¨0=Õû¸ö´°¿Ú£©
	mov bh, 00h		; ÉèÖÃ²åÈë¿ÕĞĞµÄ×Ö·ûÑÕÉ«ÎªºÚµ×ÁÁ°××Ö
	mov cx, 0		; ´°¿Ú×óÉÏ½ÇµÄĞĞºÅ=CH¡¢ÁĞºÅ=CL
	mov dh, 30		; ´°¿ÚÓÒÏÂ½ÇµÄĞĞºÅ
	mov dl, 79		; ´°¿ÚÓÒÏÂ½ÇµÄÁĞºÅ
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÉèÖÃ¹â±êÎ»ÖÃ
	mov ah, 2		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	mov dh, 0		; ĞĞºÅ
	mov dl, 0		; ÁĞºÅ
	int 10h			; ÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
	
; -------------------------------------------------------------------
diskok: ; ÅĞ¶ÏÇĞ»»µ½µÄÄ¿±ê´ÅÅÌÊÇ·ñ´æÔÚ£¨ÊäÈë²ÎÊıÎªDL=´ÅÅÌµÄÇı¶¯Æ÷ºÅ£©
	; ÀûÓÃ´ÅÅÌµÄ0ºÅÖĞ¶ÏÅĞ¶Ï´ÅÅÌÊÇ·ñ´æÔÚ
	mov ah, 0		; ¹¦ÄÜºÅ=0£º´ÅÅÌ¸´Î»£¨³ö´íÖÃCF±êÖ¾Î»£©
	int 13h			; µ÷ÓÃ13HºÅ´ÅÅÌÖĞ¶Ï
	jc .1			; CF=1 ´ÅÅÌ²»´æÔÚ£¬ÇĞ»»´ÅÅÌÊ§°Ü
	; ´ÅÅÌ´æÔÚÊ±£¬·µ»ØÇĞ»»´ÅÅÌÀı³Ì
	ret				; ´ÓÀı³Ì·µ»Ø
	
.1: ; ´ÅÅÌ²»´æÔÚÊ±£¬ÏÔÊ¾³ö´íĞÅÏ¢ºó£¬ÍË³öÑ­»·£¬ÖØĞÂ¿ªÊ¼
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾´ÅÅÌ²»´æÔÚµÄĞÅÏ¢ "Disk not exist!"
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str5 	; BP=´®µØÖ·
	mov cx, str5len	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÍË³öÑ­»·£¬ÖØĞÂ¿ªÊ¼
	add sp, 4		; µ¯³öÁ½´ÎcallµÄ·µ»ØµØÖ·
	jmp again		; ÖØĞÂ¿ªÊ¼
	
str5: ; ×Ö·û´®5£¨´ÅÅÌ²»´æÔÚĞÅÏ¢´®£©
	db 'Disk not exist!'
str5len equ $ - str5 ; ´ÅÅÌ²»´æÔÚ´®³¤
; -------------------------------------------------------------------
toa: ; ¸ÄÎªAÅÌ
	mov dl, 0		; ÈíÅÌAµÄÇı¶¯Æ÷ºÅ=0
	call diskok		; Èç¹û´ÅÅÌ²»´æÔÚ£¬¾Í²»ÇĞ»»´ÅÅÌ£¬·ñÔò¼ÌĞø£º
	mov byte [str2], 'A' ; ĞŞ¸ÄÌáÊ¾´®Ê××ÖÄ¸ÎªA
	mov byte [drvno], 0 ; ÉèÖÃÇı¶¯Æ÷ºÅÎª0
	call getdiskparam	; »ñÈ¡´ÅÅÌ²ÎÊıH&S£¨ÓÃÓÚReadSecºÍlsÀı³Ì£©
	add sp, 2		; µ¯³öcallµÄ·µ»ØµØÖ·

	;ĞŞ¸ÄÄ¿Â¼ÌáÊ¾´®
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	inc dh
	; ÉèÖÃ¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï

    call initialDisk
	
	jmp again		; ÖØĞÂ¿ªÊ¼
	
; -------------------------------------------------------------------
tob: ; ¸ÄÎªBÅÌ
	mov dl, 1		; ÈíÅÌBµÄÇı¶¯Æ÷ºÅ=1
	call diskok		; Èç¹û´ÅÅÌ²»´æÔÚ£¬¾Í²»ÇĞ»»´ÅÅÌ£¬·ñÔò¼ÌĞø£º
	mov byte [str2], 'B' ; ĞŞ¸ÄÌáÊ¾´®Ê××ÖÄ¸ÎªB
	mov byte [drvno], 1 ; ÉèÖÃÇı¶¯Æ÷ºÅÎª1
	call getdiskparam	; »ñÈ¡´ÅÅÌ²ÎÊıH&S£¨ÓÃÓÚReadSecºÍlsÀı³Ì£©
	add sp, 2		; µ¯³öcallµÄ·µ»ØµØÖ·
	;ĞŞ¸ÄÄ¿Â¼ÌáÊ¾´®
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	inc dh
	; ÉèÖÃ¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	
	call initialDisk
	
	jmp again		; ÖØĞÂ¿ªÊ¼

; -------------------------------------------------------------------
toc: ; ¸ÄÎªCÅÌ
	mov dl, 80h		; Ó²ÅÌCµÄÇı¶¯Æ÷ºÅ=80h
	call diskok		; Èç¹û´ÅÅÌ²»´æÔÚ£¬¾Í²»ÇĞ»»´ÅÅÌ£¬·ñÔò¼ÌĞø£º
	mov byte [str2], 'C' ; ĞŞ¸ÄÌáÊ¾´®Ê××ÖÄ¸ÎªC
	mov byte [drvno], 80h ; ÉèÖÃÇı¶¯Æ÷ºÅÎª80h
	call getdiskparam	; »ñÈ¡´ÅÅÌ²ÎÊıH&S£¨ÓÃÓÚReadSecºÍlsÀı³Ì£©
	add sp, 2		; µ¯³öcallµÄ·µ»ØµØÖ·
	;ĞŞ¸ÄÄ¿Â¼ÌáÊ¾´®
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	inc dh
	; ÉèÖÃ¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	
	call initialDisk
	
	jmp again		; ÖØĞÂ¿ªÊ¼

;---------------------------------------------------------------------
initialDisk:;        ;³õÊ¼»¯lsÓÃµ½µÄ²ÎÊı
	pusha
	call ReadPBootSec
	; nsecÎª¸ùÄ¿Â¼ÇøÊ£ÓàÉÈÇøÊı£¬³õÊ¼»¯Îª¸ùÄ¿Â¼ÉÈÇøÊı£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
	; ¼ÆËã¸ùÄ¿Â¼ÉÈÇøÊı£¨ = ×î´ó¸ùÄ¿Â¼ÏîÊı / 32£©
	mov ax, [Sector + 11h]	; AX = ×î´ó¸ùÄ¿Â¼ÏîÊı
	shr ax, 4				; AXÓÒÒÆ4Î»£¨~ /32£© = ¸ùÄ¿Â¼ÉÈÇøÊı
	mov word [nsec], ax		; nsec = AX = ¸ùÄ¿Â¼ÉÈÇøÊı

	; isecÎªµ±Ç°ÉÈÇøºÅ£¬¸³³õÖµÎª¸ùÄ¿Â¼ÇøµÄÊ×ÉÈÇøºÅ£¬ÔÚÑ­»·ÖĞ»áÖğ¸öÔö¼Ó
	; ¼ÆËã¸ùÄ¿Â¼Ê×ÉÈÇøºÅ£¨= ±£ÁôÉÈÇøÊı + FATÊı * FATÕ¼ÉÈÇøÊı£©
	movzx ax, byte [Sector + 10h] ; AX = FATÊı
	mul word [Sector + 16h]	; AX *= FATÕ¼ÉÈÇøÊı
	add ax, [Sector + 0Eh]	; AX += ±£ÁôÉÈÇøÊı
	mov [isec],ax			; isec = AX = ¸ùÄ¿Â¼Ê×ÉÈÇøºÅ
	popa
	ret
;--------------------------------------------------------------------
dir: ; ÏÔÊ¾¸ùÄ¿Â¼ÎÄ¼ş
	call showbpb	; ÏÔÊ¾´ÅÅÌĞÅÏ¢
	call ls			; ÏÔÊ¾´ÅÅÌÎÄ¼şĞÅÏ¢ÁĞ±í
	ret				; ´ÓÀı³Ì·µ»Ø
;--------------------------------------------------------------------
; ¶¨Òå±äÁ¿£¨´ÅÅÌ²ÎÊı£©
CurrentDirSectors	dw	14		; µ±Ç°Ä¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
SectorNoOfCurrentDirectory	dw	19	; µ±Ç°Ä¿Â¼ÇøµÄÊ×ÉÈÇøºÅ
SectorNoOfLastDirectory	dw	19	; ÉÏÒ»Ä¿Â¼ÇøµÄÊ×ÉÈÇøºÅ
iCurrentDirSectors  dw 0       ;´ı¼ÆËãµÄÏÂÒ»Ä¿Â¼ËùÕ¼ÉÈÇøÊı
Dir_len dw 0					;ÒªcdµÄÄ¿Â¼³¤¶È
isBackBool dw 0					;ÊÇ·ñÊÇÌø»ØÉÏÒ»¼¶..=2 »òÕß Í¬¼¶.=1
NumOfSign dw 0					;Ä¿Â¼·Ö¸ô·ûµÄ¸öÊı
cdToDir:   ;ÌøÖÁ×ÓÄ¿Â¼    X:/$×ÓÄ¿Â¼            ´óÖÂµÄË¼Â·ÊÇ:ÔÚµ±Ç°ÉÈÇøÕÒ×ÓÄ¿Â¼ÌõÄ¿£¬ÕÒµ½Ôò¼ÌĞø²é¿´ÌõÄ¿ÖĞÉÈÇøºÅ£¬ÔÙÈ¥ÕÒ£Æ£Á£Ô±í¼ÆËã³öÏÂÒ»¼¶Ä¿Â¼µÄ´óĞ¡£¬ĞŞ¸Äµ±Ç°ÉÈÇø±äÁ¿SectorNoOfCurrentDirectory¡¡ÒÔ¼°CurrentDirSectors
	push ax
	mov ax,[SectorNoOfCurrentDirectory]      ;±£´æ¼ÆËãÏÂÒ»ÉÈÇøÇ°µÄÉÈÇøºÅ
	mov [SectorNoOfLastDirectory],ax
	pop ax
	pusha
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äDirbuf
	mov cx, 11	; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøbufµÄ³¤¶È£¨buflen=80£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, Dirbuf		; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,3   ;Ìø¹ıcd  Èı¸ö×Ö·û
	cmp byte[bp],'\'
	jz backToRoot
	push bp    ;±£´æbp
	mov si,0
.1:  
	cmp byte[bp],20h
	jz .2
	cmp byte[bp],0
	jz .2
	inc si
	inc bp
	jmp .1
.2:
	pop bp
	cmp si,11
	jg dir_out
	cmp si,0
	jz cd_error
	mov [Dir_len],si
	mov di,Dirbuf
	cld
	mov cx,si
	mov si,bp
	rep movsb
	stosb
	popa
	
	call tocap_Dirbuf
	mov bp,Dirbuf      ;»ñµÃÒªÌø×ªÄ¿Â¼µÄÄ¿Â¼Ãû
	mov cx,11
	;call DispStr
	;ÅĞ¶ÏÊÇ·ñÎª. »òÕß ..
	mov bp,Dirbuf
	mov si,0
.isBack:
	cmp byte[bp],'.'
	jnz .isBackEnds    ;²»ÊÇµãÍË³öÀ´
	inc si
	inc bp
	jmp .isBack
.isBackEnds:
	;cmp si,2           ;Ò»¸öÊÇÍ¬¼¶Ä¿Â¼£¬Á½¸ö·µ»ØÉÏÒ»¼¶
	mov [isBackBool],si ;±£´æÅĞ¶Ï½á¹û£¬ĞŞ¸ÄÌáÊ¾´®Ê±ÓÃµ½
	cmp word[isBackBool],1   ;Ò»¸öµã»¹ÊÇÍ¬Ò»¼¶Ä¿Â¼£¬ÎŞĞè¸Ä±äÄ¿Â¼ÌáÊ¾´®
	jz cd_ends
	cmp word[isBackBool],2  ; Á½¸öµã´ú±íÉÏÒ»¼¶Ä¿Â¼
	jnz .cd_start
	pusha               ;ÉèÖÃĞÂµÄÄ¿Â¼ ÌáÊ¾´®
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	;add di,[str2len]
	dec di      ;¼õÈ¥$·ûºÅ
	;¼ÆËã/µÄ¸öÊı£¬ÒÔÅĞ¶ÏÊÇ·ñÎªÒ»¼¶Ä¿Â¼
	mov cx,[str2len]
	mov word [NumOfSign],0     ;³õÊ¼»¯¼ÆÊıÆ÷
.11
	cmp byte[di],'/'
	jnz .11.1
	inc word[NumOfSign]
.11.1:
	inc di
	loop .11
	popa                       ;ÏÈpop³öÀ´ºÃ½øĞĞÅĞ¶ÏÌø×ª
	push ax
	mov ax,ds
	mov es,ax
	pop ax
	cmp word [NumOfSign],2     ;´ú±íÉÏÒ»¼¶ÒÑ¾­ÊÇ¸ùÄ¿Â¼
	jz backToRoot
.cd_start:
;-------------------------------------------------------------------1
	push es		; ±£»¤ES

; ÈíÇı¸´Î»
	xor	ah, ah	; ¹¦ÄÜºÅah=0£¨¸´Î»´ÅÅÌÇı¶¯Æ÷£©
	xor	dl, dl	; dl=0£¨ÈíÇıA£¬ÈíÇıBÎª1¡¢Ó²ÅÌºÍUÅÌÎª80h£©
	int	13h		; ´ÅÅÌÖĞ¶Ï
	
; ÏÂÃæÔÚµ±Ç°Ä¿Â¼ÖĞÑ°ÕÒ×ÓÄ¿Â¼
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; ¸ø±íÊ¾µ±Ç°ÉÈÇøºÅµÄ
						; ±äÁ¿wSectorNo¸³³õÖµÎª¸ùÄ¿Â¼ÇøµÄÊ×ÉÈÇøºÅ£¨=19£©
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ¸ùÄ¿Â¼ÇøÊ£ÓàÉÈÇøÊı
										; ³õÊ¼»¯Îª14£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
LABEL_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	VOL_NOT_FOUND	; Èô¶ÁÍêÔò±íÊ¾Î´ÕÒµ½Ä¿Â¼Ïî
	dec	word [wRootDirSizeForLoop]	; µİ¼õ±äÁ¿wRootDirSizeForLoopµÄÖµ
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸ö¸ùÄ¿Â¼ÉÈÇøµ½×°ÔØÇø
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı

	mov	si, Dirbuf		; DS:SI -> Ä¿Â¼Ïî
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; Çå³ıDF±êÖ¾Î»
						; ÖÃ±È½Ï×Ö·û´®Ê±µÄ·½ÏòÎª×ó/ÉÏ[Ë÷ÒıÔö¼Ó]
	mov	dx, 10h			; Ñ­»·´ÎÊı=16£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
VOL_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; Ñ­»·´ÎÊı¿ØÖÆ
	jz LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR ; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø
	dec	dx				; µİ¼õÑ­»·´ÎÊıÖµ			  ¾ÍÌøµ½ÏÂÒ»ÉÈÇø
	mov	cx,11 	; ³õÊ¼Ñ­»·´ÎÊıÎª11
VOL_CMP_FILENAME:
	repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jz	LABEL_VOL_FOUND ; Èç¹û±È½ÏÁË11¸ö×Ö·û¶¼ÏàµÈ£¬±íÊ¾ÕÒµ½
VOL_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
						; FFE0h = 1111111111100000£¨µÍ5Î»=32=Ä¿Â¼ÌõÄ¿´óĞ¡£©
	add	di, 20h			; DI += 20h ÏÂÒ»¸öÄ¿Â¼ÌõÄ¿
	mov	si, Dirbuf		; SIÖ¸Ïò×°ÔØÎÄ¼şÃû´®µÄÆğÊ¼µØÖ·
	jmp	VOL_SEARCH_FOR_VOL_FILE; ×ªµ½Ñ­»·¿ªÊ¼´¦

LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR: ;¶ÔÓÚ×ÓÄ¿Â¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRÒª×Ô¼ºËã³öÀ´(Ö±½ÓÊ¹ÓÃtoDirÖĞµÄËã·¨)  Óë¸ùÄ¿Â¼Ëã·¨²»Í¬
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	exit_cd ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	LABEL_SEARCH_IN_Current_DIR_BEGIN		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc	word [wSectorNo]	; ¶ÔÓÚ¸ùÄ¿Â¼£¬µİÔöµ±Ç°ÉÈÇøºÅ
	jmp	LABEL_SEARCH_IN_Current_DIR_BEGIN
exit_cd:			;Ã»ÓĞ×ÓÄ¿Â¼µ¼ÖÂ Ìø×ªÊ§°ÜÖ±½ÓÍË³ö
	pop es			; »Ö¸´ES
	call showError1	; ÏÔÊ¾×Ö·û´®
	jmp cd_ends
VOL_NOT_FOUND:     
	pop es			; »Ö¸´ES
	call showError1	; ÏÔÊ¾×Ö·û´®
	jmp dir_out
;------------------------------------------------------+
LABEL_VOL_FOUND:           ;aaa
	mov word [iCurrentDirSectors],0  ;Õ¼ÓÃÉÈÇøÊı ¼ÆÊıÆ÷ÇåÁã
	; ¼ÆËãÎÄ¼şµÄÆğÊ¼ÉÈÇøºÅ
	mov	ax, [CurrentDirSectors]	; AX=µ±Ç°Ä¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
	and	di, 0FFE0h		; DI -> µ±Ç°ÌõÄ¿µÄ¿ªÊ¼µØÖ·
	add	di, 1Ah			; DI -> ×ÓÄ¿Â¼µÄÊ×ÉÈÇøºÅÔÚÌõÄ¿ÖĞµÄÆ«ÒÆµØÖ·
	mov cx, word [es:di] ; CX=×ÓÄ¿Â¼µÄÊ×ÉÈÇøºÅ
	mov word[SectorNoOfCurrentDirectory],cx ;ĞŞ¸Äµ±Ç°Ä¿Â¼µÄÊ×ÉÈÇøºÅ
	pop es
	
	
	mov word [nsec],1
	mov word [isec],cx
	add word [isec],1fh
	;jmp VOL_FILE_LOADED
	pusha
	push cx				; ±£´æ´ËÉÈÇøÔÚFATÖĞµÄĞòºÅ
	add	cx, ax			; CX=ÎÄ¼şµÄÏà¶ÔÆğÊ¼ÉÈÇøºÅ+µ±Ç°Ä¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
	add	cx, DeltaSectorNo ; CL <- Ä¿Â¼ÏîµÄÆğÊ¼ÉÈÇøºÅ(0-based)
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨COM³ÌĞò»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨COM³ÌĞòÆ«ÒÆµØÖ·=100h£©
	mov	ax, cx			; AX <- ÆğÊ¼ÉÈÇøºÅ	
VOL_GOON_SETTING_PARAM: ;ÉèÖÃµ±Ç°Ä¿Â¼µÄÉÈÇøÊıºÍÊ×ÉÈÇøºÅ
	push bx				; ±£´æCOM³ÌĞòÆ«ÒÆµØÖ·
	mov	cl, 1			; 1¸öÉÈÇø
	call ReadSec		; ¶ÁÉÈÇø
	inc word [iCurrentDirSectors]
	; ¼ÆËã×ÓÄ¿Â¼ËùÕ¼µÄÉÈÇøÊı
	pop bx				; È¡³öÄ¿Â¼Æ«ÒÆµØÖ·
	pop	ax				; È¡³ö´ËÉÈÇøÔÚFATÖĞµÄĞòºÅ
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	cmp	ax, 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	VOL_FILE_LOADED ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	push ax				; ±£´æÉÈÇøÔÚFATÖĞµÄĞòºÅ
	mov	dx, [CurrentDirSectors] ; DX = µ±Ç°Ä¿Â¼ÉÈÇøÊı
	add	ax, dx			; ÉÈÇøĞòºÅ + µ±Ç°Ä¿Â¼ÉÈÇøÊı
	add	ax, DeltaSectorNo ; AX = Òª¶ÁµÄÊı¾İÉÈÇøµØÖ·
	add	bx, [BPB_BytsPerSec] ; BX+512Ö¸Ïò×ÓÄ¿Â¼ÌõÄ¿ÇøµÄÏÂÒ»¸öÉÈÇøµØÖ·
	
	jmp VOL_GOON_SETTING_PARAM
VOL_FILE_LOADED:
	;jmp nextdir
	mov ax,[iCurrentDirSectors]
    mov word [CurrentDirSectors],ax
	push cx
	mov cx,[iCurrentDirSectors]
	pusha
	; ÏÔÊ¾¸ß4Î»
	mov al, cl		; AL=ID¸ßÎ»×Ö½Ú
	and al, 0F0h	; È¡³ö¸ß4Î»
	shr al, 4		; AL >> 4
	;call ShowChar	; µ÷ÓÃÏÔÊ¾×Ö·ûº¯Êı
	; ÏÔÊ¾µÍ4Î»
	mov al, cl		; AL=ID¸ßÎ»×Ö½Ú
	and al, 0Fh		; È¡³öµÍ4Î»
	;call ShowChar	; µ÷ÓÃÏÔÊ¾×Ö·ûº¯Êı
	popa
	pop cx
nextdir:
	popa
	cmp word[isBackBool],2  ; Á½¸öµã´ú±íÉÏÒ»¼¶Ä¿Â¼
	jz .upDir
	pusha               ;ÉèÖÃĞÂµÄÄ¿Â¼ ÌáÊ¾´®
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	add di,[str2len]
	dec di      ;¼õÈ¥$·ûºÅ
	add [str2len],si
	inc word [str2len]
	cld
	mov cx,si
	mov si,Dirbuf
	rep movsb
	mov al,'/'
	stosb
	mov al,'$'
	stosb
	popa
	jmp dir_out
.upDir:                 ;·µ»ØÉÏ¼¶Ä¿Â¼µÄÅĞ¶Ï,×¢ÒâµÄÊÇÒªÅĞ¶ÏÉÏÒ»¼¶ÊÇ²»ÊÇ¸ùÄ¿Â¼
	;ÊÇ·ñÊÇ·µ»Ø¸ùÄ¿Â¼ÔÚÇ°ÃæÅĞ¶Ï
	;µ½´ïÕâÒ»²½µÄ,ÌáÊ¾´®Òª¼õÉÙÒ»¼¶
	pusha
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	add di,[str2len]
	std                         ;µ¹×ÅÇå¿Õ
	mov al,20h                  ;Çåµô/$
	stosb
	mov al,20h                 
	stosb
	dec di
.2
	cmp byte[di],'/'
	jz .2.1                     ;Óöµ½µÚÒ»¸ö/ÍË³öÀ´
	mov al,20h                  ;·ñÔòÇå¿Õ
	stosb
	dec word [str2len]
	jmp .2
.2.1:
	cld
	inc di
	mov al,'$'                  ;Ìí¼Ó$
	stosb
	dec word [str2len]
	popa
	jmp dir_out
backToRoot:
	call getdiskparam	; »ñÈ¡´ÅÅÌ²ÎÊıH&S£¨ÓÃÓÚReadSecºÍlsÀı³Ì£©
	mov word [CurrentDirSectors],RootDirSectors
	mov word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	;mov byte [drvno], 0 ; ÉèÖÃÇı¶¯Æ÷ºÅÎª0
	
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	call initialDisk
dir_out:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;Çå¿ÕÄ¿Â¼Dirbuf
	rep stosb
	mov word[Dir_len],0
	

	mov ax,[CurrentDirSectors]
	mov [nsec],ax
	mov ax,[SectorNoOfCurrentDirectory]
	pusha
	push ax
	;call space
	pop ax
	mov cx,ax
	; ÏÔÊ¾¸ß4Î»
	mov al, cl		; AL=ID¸ßÎ»×Ö½Ú
	and al, 0F0h	; È¡³ö¸ß4Î»
	shr al, 4		; AL >> 4
	;call ShowChar	; µ÷ÓÃÏÔÊ¾×Ö·ûº¯Êı
	; ÏÔÊ¾µÍ4Î»
	mov al, cl		; AL=ID¸ßÎ»×Ö½Ú
	and al, 0Fh		; È¡³öµÍ4Î»
	;call ShowChar	; µ÷ÓÃÏÔÊ¾×Ö·ûº¯Êı
	popa
	;call judgeRootTODir
	;ÅĞ¶ÏÊÇ·ñ´ÓÊÇ¸ùÄ¿Â¼µ½×ÓÄ¿Â¼µÄÇĞ»»
	cmp word [SectorNoOfLastDirectory],SectorNoOfRootDirectory
	jz .1     ;ÉÏ´ÎÄ¿Â¼ÊÇ¸ùÄ¿Â¼
	jmp .2
.1	cmp word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .2.2       ;´Ó¸ùÄ¿Â¼ÌøÖÁ×ÓÄ¿Â¼+1fh
	add word[SectorNoOfCurrentDirectory],1fh       ;ÎªÊ²Ã´¼Ó1fh£¿ ×ÓÄ¿Â¼ÎÄ¼şÌõÄ¿ÖĞµÄÉÈÇøºÅÊÇÏà¶ÔÓÚÊı¾İÇøÆğÊ¼Î»ÖÃµÄ£¬µÃµ½ÎïÀíÉÈÇøºÅµÄ·½·¨ÊÇ+1fh£¨3e00h£©  ÎªÊ²Ã´ÊÇÕâÃ´¶à=_= 
	;Òª¶ÁµÄÊı¾İÉÈÇøµØÖ· = ÉÈÇøĞòºÅ + ¸ùÄ¿Â¼ÉÈÇøÊı + DeltaSectorNo           ¸ùÄ¿Â¼ÉÈÇøÊı + DeltaSectorNo = 17+14 =1fh
	jmp .2.2 
.2:	;Ô­±¾ÔÚ×ÓÄ¿Â¼
	cmp word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .2.2     ;ÏÖÔÚÊÇ¸ùÄ¿Â¼²»+1fh
	add word[SectorNoOfCurrentDirectory],1fh 
.2.2:	
	mov ax,[SectorNoOfCurrentDirectory]
	mov [isec],ax
	jmp cd_ends
cd_error:
	call showError1	; ÏÔÊ¾×Ö·û´®
cd_ends:	
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	inc dh
	; ÉèÖÃ¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	add sp,2
	
	jmp again
;--------------------------------------------------------------------
judgeRootTODir:   
	
	ret
;--------------------------------------------------------------------	
FileName_rename_len dw 0
FileSuffixes_len dw 0
FileName_renameTooLongStr db 'ÎÄ¼şÃûÌ«³¤£¬ÎŞ·¨ĞŞ¸ÄÄãÔìÂğ'
FileName_renameTooLong_len  equ ($ - FileName_renameTooLongStr)/2
RenameFile_DI dw 0
IsNotDir dw 1
rename:
	mov word [IsNotDir],1
	pusha
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äDirbuf
	mov cx, 11	; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøbufµÄ³¤¶È£¨buflen=80£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, Dirbuf		; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	mov word [Dir_len],11 ;ÎÄ¼şÌõÄ¿->ÎÄ¼şÃûÎª11×Ö½Ú
	
	mov cx,buflen
	mov bp,buf
	add bp,7  ;Ìø¹ırename  6¸ö×Ö·û
	;ÔÚ´Ë¿É¼ì²âÊäÈëºÏ·¨ĞÔ
	push bp    ;±£´æbp
	mov si,0
.1:  
	cmp byte[bp],20h
	jz .2.0
	cmp byte[bp],0
	jz .2
	cmp byte[bp],'.'
	jz .2
	inc si
	inc bp
	jmp .1
.2.0:
	mov word[IsNotDir],0
	;mov ax,0fah
	;call hex2ascii
.2:
	pop bp
	cmp si,11
	jg rename_tolong
	mov [FileName_rename_len],si
	;mov ax,si
	;call hex2ascii
	;xor ah,ah
	;int 16h
	
	cmp word[IsNotDir],1     ;×ÓÄ¿Â¼Ìø¹ıÎÄ¼şºó×º¼ì²â
	jnz .4.1
	push bp
	mov [FileName_rename_len],si
	add bp,si            ;spÖ¸Ïòºó×º
	add bp,[IsNotDir]             ;¶ÔÓÚÎÄ¼şÌø¹ı. ¶ÔÓÚ×ÓÄ¿Â¼²»±ä
	mov si,0
.3:  
	cmp byte[bp],20h
	jz .4
	cmp byte[bp],0
	jz .4
	inc si
	inc bp
	jmp .3
.4
	pop bp
	cmp si,4
	jg rename_tolong
	mov [FileSuffixes_len],si

	;mov ax,si
	;call hex2ascii
	;xor ah,ah
	;int 16h
	
	
	mov si,[FileSuffixes_len]
	mov di,Dirbuf+8 ;Æ«ÒÆµ½ÎÄ¼şºó×ºÃû
	cld
	mov cx,si
	mov si,bp
	add si,[FileName_rename_len]	  ;¶¨Î»µ½bufÀïµÄÎÄ¼şºó×º
	inc si
	rep movsb
	stosb
.4.1:	
	mov si,[FileName_rename_len]
	mov di,Dirbuf
	cld
	mov cx,si
	mov si,bp
	rep movsb
	
	popa
	
	call tocap_Dirbuf
	;mov bp,Dirbuf
	;mov cx,11
	;call DispStr

;	cmp word[FileSuffixes_len],0
;	jz .clearsuffix
;	jmp .next_10
;.clearsuffix:
;	mov cx,11
;	sub cx,[FileName_rename_len]
;	mov al,20h
;	mov di,Dirbuf            ;Çå¿ÕÄ¿Â¼Dirbuf ºó×º(Æ¥ÅäÄ¿Â¼ÓĞĞ©bug,Ö»ºÃÇåµôºó×º)
;	add di,[FileName_rename_len]
;.next_10:
	;xor ah,ah
	;int 16h
;-------------------------------------------------------------------1
	push es		; ±£»¤ES

; ÈíÇı¸´Î»
	xor	ah, ah	; ¹¦ÄÜºÅah=0£¨¸´Î»´ÅÅÌÇı¶¯Æ÷£©
	xor	dl, dl	; dl=0£¨ÈíÇıA£¬ÈíÇıBÎª1¡¢Ó²ÅÌºÍUÅÌÎª80h£©
	int	13h		; ´ÅÅÌÖĞ¶Ï
	
; ÏÂÃæÔÚµ±Ç°Ä¿Â¼ÖĞÑ°ÕÒ×ÓÄ¿Â¼
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; ¸ø±íÊ¾µ±Ç°ÉÈÇøºÅµÄ
						; ±äÁ¿wSectorNo¸³³õÖµÎª¸ùÄ¿Â¼ÇøµÄÊ×ÉÈÇøºÅ£¨=19£©
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ¸ùÄ¿Â¼ÇøÊ£ÓàÉÈÇøÊı
										; ³õÊ¼»¯Îª14£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
RENAME_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	RENAME_NOT_FOUND	; Èô¶ÁÍêÔò±íÊ¾Î´ÕÒµ½Ä¿Â¼Ïî
	dec	word [wRootDirSizeForLoop]	; µİ¼õ±äÁ¿wRootDirSizeForLoopµÄÖµ
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸ö¸ùÄ¿Â¼ÉÈÇøµ½×°ÔØÇø
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı

	mov	si, Dirbuf		; DS:SI -> Ä¿Â¼Ïî
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; Çå³ıDF±êÖ¾Î»
						; ÖÃ±È½Ï×Ö·û´®Ê±µÄ·½ÏòÎª×ó/ÉÏ[Ë÷ÒıÔö¼Ó]
	mov	dx, 10h			; Ñ­»·´ÎÊı=16£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
RENAME_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; Ñ­»·´ÎÊı¿ØÖÆ
	jz RENAME_GOTO_NEXT_SECTOR_IN_Current_DIR ; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø
	dec	dx				; µİ¼õÑ­»·´ÎÊıÖµ			  ¾ÍÌøµ½ÏÂÒ»ÉÈÇø
	mov	cx,[Dir_len] 	; ³õÊ¼Ñ­»·´ÎÊıÎª11
RENAME_CMP_FILENAME:
	repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jz	RENAME_VOL_FOUND ; Èç¹û±È½ÏÁË11¸ö×Ö·û¶¼ÏàµÈ£¬±íÊ¾ÕÒµ½
RENAME_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
						; FFE0h = 1111111111100000£¨µÍ5Î»=32=Ä¿Â¼ÌõÄ¿´óĞ¡£©
	add	di, 20h			; DI += 20h ÏÂÒ»¸öÄ¿Â¼ÌõÄ¿
	mov	si, Dirbuf		; SIÖ¸Ïò×°ÔØÎÄ¼şÃû´®µÄÆğÊ¼µØÖ·
	jmp	RENAME_SEARCH_FOR_VOL_FILE; ×ªµ½Ñ­»·¿ªÊ¼´¦

RENAME_GOTO_NEXT_SECTOR_IN_Current_DIR: ;¶ÔÓÚ×ÓÄ¿Â¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRÒª×Ô¼ºËã³öÀ´(Ö±½ÓÊ¹ÓÃtoDirÖĞµÄËã·¨)  Óë¸ùÄ¿Â¼Ëã·¨²»Í¬
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	exit_rename ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	RENAME_SEARCH_IN_Current_DIR_BEGIN		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc	word [wSectorNo]	; ¶ÔÓÚ¸ùÄ¿Â¼£¬µİÔöµ±Ç°ÉÈÇøºÅ
	jmp	RENAME_SEARCH_IN_Current_DIR_BEGIN

RENAME_NOT_FOUND:
	pop es			; »Ö¸´ES
exit_rename:
	call showError1	; ÏÔÊ¾×Ö·û´®
	jmp out_rename
;------------------------------------------------------+
RENAME_VOL_FOUND
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
	mov [RenameFile_DI],di
	;¼òµ¥µÄdebug
	
	pop es
	;pusha
	;mov cx,11
	;mov bp,di
	;call DispStr
	;popa
	
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;Çå¿ÕÄ¿Â¼Dirbuf
	rep stosb
	
	mov bp,buf
	add bp,7  ;Ìø¹ırename  6¸ö×Ö·û

.0:  
	cmp byte[bp],20h
	jz .10
	cmp byte[bp],0
	jz .10
	inc si
	inc bp
	jmp .0
.10
	inc bp
	;ÔÚ´Ë¿É¼ì²âÊäÈëºÏ·¨ĞÔ
	push bp    ;±£´æbp
	mov si,0
.1:  
	cmp byte[bp],20h
	jz .2
	cmp byte[bp],0
	jz .2
	cmp byte[bp],'.'
	jz .2
	inc si
	inc bp
	jmp .1
.2:
	pop bp
	cmp si,11
	jg rename_tolong
	mov [FileName_rename_len],si
	;mov ax,si
	;call hex2ascii
	
	cmp word[IsNotDir],1     ;×ÓÄ¿Â¼Ìø¹ıÎÄ¼şºó×º¼ì²â
	jnz .4.1
	push bp
	mov [FileName_rename_len],si
	add bp,si            ;spÖ¸Ïòºó×º
	add bp,[IsNotDir]
	mov si,0
.3:  
	cmp byte[bp],20h
	jz .4
	cmp byte[bp],0
	jz .4
	inc si
	inc bp
	jmp .3
.4
	pop bp
	cmp si,4
	jg rename_tolong
	mov [FileSuffixes_len],si

	;mov ax,si
	;call hex2ascii
	
	mov si,[FileSuffixes_len]
	mov di,Dirbuf+8 ;Æ«ÒÆµ½ÎÄ¼şºó×ºÃû
	cld
	mov cx,si
	mov si,bp
	add si,[FileName_rename_len]	  ;¶¨Î»µ½bufÀïµÄÎÄ¼şºó×º
	inc si
	rep movsb
	stosb
.4.1:	
	mov si,[FileName_rename_len]
	mov di,Dirbuf
	cld
	mov cx,si
	mov si,bp
	rep movsb
	
	call tocap_Dirbuf
	;mov bp,Dirbuf
	;mov cx,11
	;call DispStr
	
	;xor ah,ah
	;int 16h
	push es
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov di,[RenameFile_DI]
	
	mov si,Dirbuf
	mov cx,11
	repe movsb			; Ğ´»º³åÇø£¬ĞŞ¸Ä¶ÔÓ¦ÎÄ¼şÌõÄ¿
	
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call WriteSec		; µ÷ÓÃĞ´ÉÈÇøº¯Êı
	
	pop es
	;Dirbuf
	jmp out_rename
rename_tolong:;´®Ì«³¤
	mov cx,FileName_renameTooLong_len
	mov bp,FileName_renameTooLongStr
	call DispStr_Chinese
out_rename:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;Çå¿ÕÄ¿Â¼Dirbuf
	rep stosb
	mov word[Dir_len],0
	
	add sp,2
	jmp again
;--------------------------------------------------------------------	
mkdir:
	;mov cx,buflen
	;mov bp,buf
	;call DispStr
	;ÔÚFAT±íÖĞÕÒÎ´Ê¹ÓÃµÄÏîÄ¿£¬ÔÙÓÃĞ´ÉÈÇøÖĞ¶ÏĞ´×ÓÄ¿Â¼ÉÈÇøbuf£¬²¢ÔÚµ±Ç°Ä¿Â¼Ìí¼ÓÒ»¸ö×ÓÄ¿Â¼Ïî
		push ax
	mov ax,[SectorNoOfCurrentDirectory]      ;±£´æ¼ÆËãÏÂÒ»ÉÈÇøÇ°µÄÉÈÇøºÅ
	mov [SectorNoOfLastDirectory],ax
	pop ax
	pusha
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äDirbuf
	mov cx, 11	; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøbufµÄ³¤¶È£¨buflen=80£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, Dirbuf		; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,6   ;Ìø¹ımkdir   Áù¸ö×Ö·û
	push bp    ;±£´æbp
	mov si,0
.1:  
	cmp byte[bp],20h
	jz .2
	cmp byte[bp],0
	jz .2
	inc si
	inc bp
	jmp .1
.2:
	pop bp
	cmp si,11
	jg mk_out
	cmp si,0
	jz mk_error1
	mov [Dir_len],si
	mov di,Dirbuf
	cld
	mov cx,si
	mov si,bp
	rep movsb
	stosb
	popa
	
	call tocap_Dirbuf
	mov bp,Dirbuf      ;»ñµÃÒªÌø×ªÄ¿Â¼µÄÄ¿Â¼Ãû
	mov cx,11
	call DispStr
	
	.cd_start:
;-------------------------------------------------------------------1
	push es		; ±£»¤ES

; ÈíÇı¸´Î»
	xor	ah, ah	; ¹¦ÄÜºÅah=0£¨¸´Î»´ÅÅÌÇı¶¯Æ÷£©
	xor	dl, dl	; dl=0£¨ÈíÇıA£¬ÈíÇıBÎª1¡¢Ó²ÅÌºÍUÅÌÎª80h£©
	int	13h		; ´ÅÅÌÖĞ¶Ï
	
; ÏÂÃæÔÚµ±Ç°Ä¿Â¼ÖĞÑ°ÕÒ×ÓÄ¿Â¼
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; ¸ø±íÊ¾µ±Ç°ÉÈÇøºÅµÄ
						; ±äÁ¿wSectorNo¸³³õÖµÎª¸ùÄ¿Â¼ÇøµÄÊ×ÉÈÇøºÅ£¨=19£©
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ¸ùÄ¿Â¼ÇøÊ£ÓàÉÈÇøÊı
										; ³õÊ¼»¯Îª14£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
MKDIR_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	MKDIR_NOT_FOUND	; Èô¶ÁÍêÔò±íÊ¾Î´ÕÒµ½Ä¿Â¼Ïî
	dec	word [wRootDirSizeForLoop]	; µİ¼õ±äÁ¿wRootDirSizeForLoopµÄÖµ
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸ö¸ùÄ¿Â¼ÉÈÇøµ½×°ÔØÇø
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı

	mov	si, Dirbuf		; DS:SI -> Ä¿Â¼Ïî
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; Çå³ıDF±êÖ¾Î»
						; ÖÃ±È½Ï×Ö·û´®Ê±µÄ·½ÏòÎª×ó/ÉÏ[Ë÷ÒıÔö¼Ó]
	mov	dx, 10h			; Ñ­»·´ÎÊı=16£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
MKDIR_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; Ñ­»·´ÎÊı¿ØÖÆ
	jz MKDIR_GOTO_NEXT_SECTOR_IN_Current_DIR ; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø
	dec	dx				; µİ¼õÑ­»·´ÎÊıÖµ			  ¾ÍÌøµ½ÏÂÒ»ÉÈÇø
	mov	cx,11 	; ³õÊ¼Ñ­»·´ÎÊıÎª11
MKDIR_CMP_FILENAME:
	repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jz	MKDIR_VOL_FOUND ; Èç¹û±È½ÏÁË11¸ö×Ö·û¶¼ÏàµÈ£¬±íÊ¾ÕÒµ½
MKDIR_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
						; FFE0h = 1111111111100000£¨µÍ5Î»=32=Ä¿Â¼ÌõÄ¿´óĞ¡£©
	add	di, 20h			; DI += 20h ÏÂÒ»¸öÄ¿Â¼ÌõÄ¿
	mov	si, Dirbuf		; SIÖ¸Ïò×°ÔØÎÄ¼şÃû´®µÄÆğÊ¼µØÖ·
	jmp	MKDIR_SEARCH_FOR_VOL_FILE; ×ªµ½Ñ­»·¿ªÊ¼´¦

MKDIR_GOTO_NEXT_SECTOR_IN_Current_DIR: ;¶ÔÓÚ×ÓÄ¿Â¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRÒª×Ô¼ºËã³öÀ´(Ö±½ÓÊ¹ÓÃtoDirÖĞµÄËã·¨)  Óë¸ùÄ¿Â¼Ëã·¨²»Í¬
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	exit_mk ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	MKDIR_SEARCH_IN_Current_DIR_BEGIN		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc	word [wSectorNo]	; ¶ÔÓÚ¸ùÄ¿Â¼£¬µİÔöµ±Ç°ÉÈÇøºÅ
	jmp	MKDIR_SEARCH_IN_Current_DIR_BEGIN
exit_mk:			;Ã»ÓĞ×ÓÄ¿Â¼µ¼ÖÂ Ìø×ªÊ§°ÜÖ±½ÓÍË³ö
MKDIR_NOT_FOUND:    ; Ã»ÓĞÕÒµ½£¬Ôò¿ÉÒÔÌí¼ÓÌõÄ¿
	pop es			; »Ö¸´ES
	
	call CreateDir  ;µ÷ÓÃÉú³ÉÄ¿Â¼º¯Êı
	
	jmp mk_out
;------------------------------------------------------+
MKDIR_VOL_FOUND:     ; ÕÒµ½ÁË,ÔòÎŞ·¨´´½¨×ÓÄ¿Â¼
	pop es			; »Ö¸´ES
	call showError2	; ÏÔÊ¾×Ö·û´®
	jmp mk_out
mk_error1:
	call showError1	; ÏÔÊ¾×Ö·û´®
mk_out:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;Çå¿ÕÄ¿Â¼Dirbuf
	rep stosb
	mov word[Dir_len],0
mk_ends:	
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	inc dh
	; ÉèÖÃ¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	add sp,2
	
	jmp again
; ------------------------------------------------------------------
; ´´½¨Ä¿Â¼Ïîº¯Êı,Ä¿Â¼Ãû´æÔÚ DirBufÀïÃæ£¬º¯ÊıµÄÈÎÎñÊÇÔÚµ±Ç°Ä¿Â¼ÉÈÇøÖĞÕÒµ½¿ÕÏĞÌõÄ¿,²¢ÕÒµ½¿ÕÏĞµÄ512BÉÈÇø´æ·Å³õÊ¼»¯µÄÉÈÇøÊı¾İ
DefaultDirBuf db 10h
			resb 10	;10B±£Áô
			resb 6;db 39h,0a1h,0c2h,48h,50h,0    ;×îºóĞ´ÈëÊ±¼ä¡¢ÈÕÆÚ¡¢¿ªÊ¼´ØºÅ
			resb 4	;4B´óĞ¡ÎªÁã
CreateDir:
	;-------------------------------------------------------------------1
	push es		; ±£»¤ES

; ÈíÇı¸´Î»
	xor	ah, ah	; ¹¦ÄÜºÅah=0£¨¸´Î»´ÅÅÌÇı¶¯Æ÷£©
	xor	dl, dl	; dl=0£¨ÈíÇıA£¬ÈíÇıBÎª1¡¢Ó²ÅÌºÍUÅÌÎª80h£©
	int	13h		; ´ÅÅÌÖĞ¶Ï
	
; ÏÂÃæÔÚµ±Ç°Ä¿Â¼ÖĞÑ°ÕÒ¿ÕÌõÄ¿
	mov ax,[SectorNoOfCurrentDirectory]
	mov	word [wSectorNo], ax 	; ¸ø±íÊ¾µ±Ç°ÉÈÇøºÅµÄ
						; ±äÁ¿wSectorNo¸³³õÖµÎª¸ùÄ¿Â¼ÇøµÄÊ×ÉÈÇøºÅ£¨=19£©
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ¸ùÄ¿Â¼ÇøÊ£ÓàÉÈÇøÊı
										; ³õÊ¼»¯Îª14£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
CreateDir_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	CreateDir_NOT_FOUND	; Èô¶ÁÍêÔò±íÊ¾Î´ÕÒµ½Ä¿Â¼Ïî
	dec	word [wRootDirSizeForLoop]	; µİ¼õ±äÁ¿wRootDirSizeForLoopµÄÖµ
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸ö¸ùÄ¿Â¼ÉÈÇøµ½×°ÔØÇø
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı

	;mov	si, MKbuf		; DS:SI -> Ä¿Â¼Ïî
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; Çå³ıDF±êÖ¾Î»
						; ÖÃ±È½Ï×Ö·û´®Ê±µÄ·½ÏòÎª×ó/ÉÏ[Ë÷ÒıÔö¼Ó]
	mov	dx, 10h			; Ñ­»·´ÎÊı=1£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
CreateDir_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; Ñ­»·´ÎÊı¿ØÖÆ
	jz CreateDir_GOTO_NEXT_SECTOR_IN_Current_DIR ; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø
	dec	dx				; µİ¼õÑ­»·´ÎÊıÖµ			  ¾ÍÌøµ½ÏÂÒ»ÉÈÇø
CreateDir_CMP_FILENAME:
	;repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	;cmp	cx, 0
	;ÅĞ¶ÏµÚÒ»¸ö×Ö½ÚµÄÖµ
	cmp byte[es:di],0
	jz actually_found
	cmp byte[es:di],05
	jz actually_found
	cmp byte[es:di],0e5h
	jz actually_found
	
	;jz	CreateDir_VOL_FOUND ; Èç¹û±È½ÏÁË11¸ö×Ö·û¶¼ÏàµÈ£¬±íÊ¾ÕÒµ½
CreateDir_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
						; FFE0h = 1111111111100000£¨µÍ5Î»=32=Ä¿Â¼ÌõÄ¿´óĞ¡£©
	add	di, 20h			; DI += 20h ÏÂÒ»¸öÄ¿Â¼ÌõÄ¿
	;mov	si, MKbuf		; SIÖ¸Ïò×°ÔØÎÄ¼şÃû´®µÄÆğÊ¼µØÖ·
	jmp	CreateDir_SEARCH_FOR_VOL_FILE; ×ªµ½Ñ­»·¿ªÊ¼´¦

CreateDir_GOTO_NEXT_SECTOR_IN_Current_DIR: ;¶ÔÓÚ×ÓÄ¿Â¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRÒª×Ô¼ºËã³öÀ´(Ö±½ÓÊ¹ÓÃtoDirÖĞµÄËã·¨)  Óë¸ùÄ¿Â¼Ëã·¨²»Í¬
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	CreateDir_NOT_FOUND ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	CreateDir_SEARCH_IN_Current_DIR_BEGIN		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc	word [wSectorNo]	; ¶ÔÓÚ¸ùÄ¿Â¼£¬µİÔöµ±Ç°ÉÈÇøºÅ
	jmp	CreateDir_SEARCH_IN_Current_DIR_BEGIN
actually_found:          ;·¢ÏÖ¿ÕÓà¿é
	;ÉèÖÃÎÄ¼şÃû
	;ÉèÖÃÊôĞÔ(Ä¬ÈÏ)×ÓÄ¿Â¼=10h
	;ÉèÖÃÊ±¼ä(ÔİÊ±²»×÷¿¼ÂÇ)
	;ÎÄ¼ş´óĞ¡Ä¬ÈÏÈ«0
	pop es

	push es
	cld
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
	mov al,0fbh
	;call hex2ascii
	
	mov ax,di
	push ax
	mov al,ah
	;call hex2ascii
	pop ax
	;call hex2ascii
	mov si,Dirbuf
	mov cx,11
	repe movsb			; Ğ´»º³åÇø£¬ĞŞ¸Ä¶ÔÓ¦ÎÄ¼şÌõÄ¿  ÎÄ¼şÃû
	
	;¼ÆËã³öÊ±¼ä¡¢ÈÕÆÚ
	call CountTimeDate
	
	;fat±íÖĞ±éÀúµÃµ½¿ÕÏĞÇø,·ÖÅä¸ø×ÓÄ¿Â¼
	;ÉèÖÃfat±íÏîÎª×îºó´Ø  getEmptyFatEntry  setEmptyFatEntry
	call getEmptyFatEntry
	push ax
	;call hex2ascii
	;xor ah,ah
	;int 16h
	pop ax
	push ax
	mov [InitialDirSector],ax    ;±£´æ¿ÕÏĞµÄÉÈÇøºÅ£¬ÏÂÃæ¿ªÊ¼Ğ´ÉÈÇø
	call setEmptyFatEntry
	pop ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+1ah-11],ax  ;Ğ´Èë¶ÔÓ¦fatÉÈÇø
	pop di
	
	;=======================================
	;³õÊ¼»¯×ÓÄ¿Â¼ÉÈÇø
	push di
	mov di,InitialDirBuf
	mov word [ds:di+1ah],ax  ;Ğ´Èë¶ÔÓ¦fatÉÈÇø(¿ªÊ¼´ØºÅ).
	mov ax,[SectorNoOfCurrentDirectory]
	cmp ax,19                ;¸ùÄ¿Â¼Óë×ÓÄ¿Â¼ÉÈÇøº¬Òå²»Í¬£¬±È½ÏÄÔ²Ğ£¬cdToDirºÍmkDir¾ùÒªÅĞ¶Ï
	jnz .noNeedToAdd_1fh
	add ax,1fh
	jmp MKjudgeEnds
.noNeedToAdd_1fh:
	sub ax,1fh
MKjudgeEnds:
	mov word [ds:di+1ah+32],ax  ;Ğ´Èëµ±Ç°Ä¿Â¼´ØºÅ ffffg..
	pop di
	call CountTimeDate_Initial
	call InitialDir
	;=======================================
	;Ğ´Ä¿Â¼ÌõÄ¿
	mov si,DefaultDirBuf
	mov cx,21
	repe movsb			; Ğ´»º³åÇø£¬ĞŞ¸Ä¶ÔÓ¦ÎÄ¼şÌõÄ¿  Ä¬ÈÏÊôĞÔ
	
	;mov al,0cbh
	;call hex2ascii
	;xor ah,ah
	;int 16h
	
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call WriteSec		; µ÷ÓÃĞ´ÉÈÇøº¯Êı
	jmp create_ends
CreateDir_NOT_FOUND:	
	pop es
	;mov al,0fah
	;call hex2ascii
	push es
create_ends:
	pop es
	
	ret
; -------------------------------------------------------------------
InitialDirSector dw 0
InitialDirBuf  db 2Eh,20h,20h,20h,20h,20h,20h,20h,20h,20h,20h,10h,00h,00h,00h,00h
			   resb 16
			   db 2Eh,2Eh,20h,20h,20h,20h,20h,20h,20h,20h,20h,10h,00h,00h,00h,00h
			   resb 16
			   resb 512-64
InitialDir: ;¸ù¾İÉÈÇøºÅĞ´Èë
	push es
	pusha
	mov ax,ds
	mov es,ax
	mov bx,InitialDirBuf
	mov cl,1
	mov ax,[InitialDirSector]
	add ax,1fh
	call WriteSec
	popa
	pop es
	ret
; -------------------------------------------------------------------
CountTimeDate_Initial:
	push dx
	push di
	;=============ÉèÖÃÊ±¼ä================Ê±¼ä=Ğ¡Ê±*2048+·ÖÖÓ*32+Ãë/2
	mov dx,0
	; »ñÈ¡Ê±ĞÅÏ¢
	mov al, 4			; Ê±µÄÆ«ÒÆµØÖ·Îª4
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÊ±ĞÅÏ¢
	call bcd2hex
	
	mov dl,al
	shl dx,11          ;X2048 Ê±¼ä¹«Ê½ Ê±¼ä=Ğ¡Ê±*2048+·ÖÖÓ*32+Ãë/2
	
	; »ñÈ¡·ÖĞÅÏ¢
	mov al, 2			; ·ÖµÄÆ«ÒÆµØÖ·Îª2
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈë·ÖĞÅÏ¢
	call bcd2hex
	
	mov ah,0   
	shl ax,5           ;X32
	add dx,ax
	
	; »ñÈ¡ÃëĞÅÏ¢
	mov al, 0			; ÃëµÄÆ«ÒÆµØÖ·Îª0
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÃëĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	shr ax,1           ;/2
	add dx,ax
	
	push di
	mov di,InitialDirBuf
	mov word [ds:di+16h],dx  ;Ğ´Èëµ±Ç°Ê±¼ä.
	mov word [ds:di+16h+32],dx  ;Ğ´Èëµ±Ç°Ê±¼ä..
	pop di
	
	;==============ÉèÖÃÈÕÆÚ===============ÈÕÆÚ=(Äê·İ-1980)*512+ÔÂ·İ*32+ÈÕ
	mov dx,0
	; »ñÈ¡ÄêĞÅÏ¢
	mov al, 9			; ÄêµÄÆ«ÒÆµØÖ·Îª9
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÄêĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	add ax,2000-1980
	shl ax,9
	add dx,ax
	
	; »ñÈ¡ÔÂĞÅÏ¢
	mov al, 8			; ÔÂµÄÆ«ÒÆµØÖ·Îª8
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÔÂĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	shl ax,5
	add dx,ax
	
	; »ñÈ¡ÈÕĞÅÏ¢
	mov al, 7			; ÈÕµÄÆ«ÒÆµØÖ·Îª7
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÈÕĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	add dx,ax
	
	push di
	mov di,InitialDirBuf
	mov word [ds:di+18h],dx  ;Ğ´Èëµ±Ç°ÈÕÆÚ.
	mov word [ds:di+18h+32],dx  ;Ğ´Èëµ±Ç°ÈÕÆÚ..
	pop di
	
	pop di
	pop dx
	ret
; -------------------------------------------------------------------
CountTimeDate:
	push dx
	push di
	;=============ÉèÖÃÊ±¼ä================Ê±¼ä=Ğ¡Ê±*2048+·ÖÖÓ*32+Ãë/2
	mov dx,0
	; »ñÈ¡Ê±ĞÅÏ¢
	mov al, 4			; Ê±µÄÆ«ÒÆµØÖ·Îª4
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÊ±ĞÅÏ¢
	call bcd2hex
	
	mov dl,al
	shl dx,11          ;X2048 Ê±¼ä¹«Ê½ Ê±¼ä=Ğ¡Ê±*2048+·ÖÖÓ*32+Ãë/2
	
	; »ñÈ¡·ÖĞÅÏ¢
	mov al, 2			; ·ÖµÄÆ«ÒÆµØÖ·Îª2
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈë·ÖĞÅÏ¢
	call bcd2hex
	
	mov ah,0   
	shl ax,5           ;X32
	add dx,ax
	
	; »ñÈ¡ÃëĞÅÏ¢
	mov al, 0			; ÃëµÄÆ«ÒÆµØÖ·Îª0
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÃëĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	shr ax,1           ;/2
	add dx,ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+16h-11],dx  ;Ğ´Èëµ±Ç°Ê±¼ä
	pop di
	
	;==============ÉèÖÃÈÕÆÚ===============ÈÕÆÚ=(Äê·İ-1980)*512+ÔÂ·İ*32+ÈÕ
	mov dx,0
	; »ñÈ¡ÄêĞÅÏ¢
	mov al, 9			; ÄêµÄÆ«ÒÆµØÖ·Îª9
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÄêĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	add ax,2000-1980
	shl ax,9
	add dx,ax
	
	; »ñÈ¡ÔÂĞÅÏ¢
	mov al, 8			; ÔÂµÄÆ«ÒÆµØÖ·Îª8
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÔÂĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	shl ax,5
	add dx,ax
	
	; »ñÈ¡ÈÕĞÅÏ¢
	mov al, 7			; ÈÕµÄÆ«ÒÆµØÖ·Îª7
	out 70h, al		; Ö¸¶¨´æ´¢µ¥ÔªµØÖ·
	in al, 71h			; ¶ÁÈëÈÕĞÅÏ¢
	call bcd2hex
	
	mov ah,0
	add dx,ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+18h-11],dx  ;Ğ´Èëµ±Ç°ÈÕÆÚ
	pop di
	
	pop di
	pop dx
	ret
; ------------------------------------------------------------------
bcd2hex:  ;×éºÏÊ®½øÖÆbcdÂë ×ª 16½øÖÆ  Èë¿Ú£ºal  ³ö¿ÚAL
	push dx
	push bx
	mov dx,0
	mov dl,al
	and dl,0fh     ;½ØÈ¡µÍËÄÎ»  ¸öÎ»©
	and al,0f0h    ;½ØÈ¡¸ß4Î»   Ê®Î»
	shr al,4 	   ;ÒÆÖÁµÍÎ»
	mov ah,0
	mov bl,10
	mul bl         ;³Ë10
	add al,dl      ;¼Ó¸öÎ»
	
	mov ah,0
	pop bx
	pop dx
	ret
; ------------------------------------------------------------------
scrollscreen:      ;¹ö¶¯ÆÁÄ» al=ĞĞºÅ
	pusha
	mov	ah, 6			; ¹¦ÄÜºÅ
	mov bh,11110000b		; ÉèÖÃ±³¾°É«ÎªºÚÉ«
	mov ch, 0			; CH=ĞĞºÅ¡¢CL=ÁĞºÅ
	mov cl, 0			; ´°¿Ú×óÉÏ½ÇµÄĞĞÁĞºÅ¶¼Îª0
	mov dh, 29		; ´°¿ÚÓÒÏÂ½ÇµÄĞĞºÅ£¬ÎÄ±¾ÆÁÄ»25ĞĞ£¬ĞĞºÅ=0~24
	mov dl, 79		; ´°¿ÚÓÒÏÂ½ÇµÄÁĞºÅ£¬ÎÄ±¾ÆÁÄ»80ÁĞ£¬ÁĞºÅ=0~79
	int 10h			; ÏÔÊ¾ÖĞ¶Ï
	popa
	ret	
; -------------------------------------------------------------------
helpStr:
	db 'You can use the following inner command:'
helpStrLen equ $-helpStr
help: ; ÏÔÊ¾°ïÖúĞÅÏ¢
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	
	cmp dh,28
	jl .0
	mov al,2
	call scrollscreen
	mov dh,26
.0:
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, helpStr 	; BP=´®µØÖ·
	mov cx, helpStrLen	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï

	inc dh          ;ĞĞºÅ+1
	inc dh          ;ĞĞºÅ+1
	
	; Ñ­»·ÏÔÊ¾ÌáÊ¾´®
	push si
	push di
	mov cx,N       ;ÃüÁî´®¸öÊı
	mov si,0
	mov di,0
.helps:
	push cx
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 2 		; µÚ2ÁĞ
	
	mov bp, cmdstr 	; BP=´®µØÖ·
	add bp,si
	mov cx, 8	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	
	mov ah, 42h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 10 		; µÚ11ÁĞ
	mov bp, cmdHelpStr_chin 	; BP=´®µØÖ·
	add bp,di
	mov cx, 15	; ´®³¤
	
	int 21h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	
	add si,8
	add di,30
	inc dh
	
	cmp dh,28
	jl .1
	mov al,1
	call scrollscreen
	mov dh,27
.1:
	pop cx
	loop .helps
	pop di
	pop si
	
	inc dh
	call newline
	ret				; ´ÓÀı³Ì·µ»Ø
	
; -------------------------------------------------------------------
; ÄÚ²¿ÃüÁîÀı³Ì½áÊø
; ===================================================================


; ===================================================================
; ÃüÁîĞĞÖ÷Ñ­»·Àı³Ì¿ªÊ¼
; -------------------------------------------------------------------
prompt: ; ÏÔÊ¾ÃüÁîĞĞÏµÍ³ÌáÊ¾´®Àı³Ì
	call newline	; »Ø³µ»»ĞĞ
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	; ÏÔÊ¾ÌáÊ¾´®
	mov ah, 13h 	; ¹¦ÄÜºÅ
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bl, 0fh 	; ÁÁ°×
	mov bh, 0 		; µÚ0Ò³
	mov dl, 0 		; µÚ0ÁĞ
	mov bp, str2 	; BP=´®µØÖ·
	mov cx, [str2len]	; ´®³¤
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
blank db 20h
; -------------------------------------------------------------------
getstrln: ; »ñÈ¡¼üÅÌÊäÈëµÄÃüÁî´®ĞĞ
	cld				; Çå³ı·½Ïò±êÖ¾Î»£¨Ê¹É¨Ãè×Ö·û´®·½ÏòÎª´Ó´®Ê×µ½´®Î²£©
	
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äbuf
	mov cx, buflen	; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøbufµÄ³¤¶È£¨buflen=80£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, buf		; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äfnbufµÄÇ°8¸ö×Ö½Ú
	mov cx, cslen	; Ñ­»·´ÎÊıCX=ÃüÁî´®×î´óµÄ³¤¶È£¨cslen=8£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, fnbuf	; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	mov si, 0		; µ±Ç°×Ö·ûÆ«ÒÆÎ»ÖÃ SI = 0
keyin: ; ½ÓÊÜ¼üÅÌÊäÈë
	; ¶Á°´¼ü£¨·µ»ØµÄ°´¼üASCIIÂëÔÚALÖĞ£©
	mov ah, 0 		; ¹¦ÄÜºÅ
	int 16h 		; µ÷ÓÃ16HºÅÖĞ¶Ï
	; ¶Ô»Ø³µ·û£¨0DH£©½áÊøÊäÈë
	cmp al, 0dh 	; ±È½ÏALÖĞµÄ¼üÈë×Ö·ûÓë»Ø³µ·û£¨ASCIIÂëÎª0DH£©
	je return 		; ÏàµÈÌø×ªµ½´ÓÀı³Ì·µ»Ø
	cmp al, 08h
	je backspace
	; ±£´æ°´¼ü×Ö·ûµ½buf
	mov [buf + si], al; buf[SI]=AL
	inc si			; SI++
	; Ì«³¤Ê±Ìø³ö
	cmp si, buflen	; SI >= 80 ?
	jae goout		; >= Ê±Ìø×ª
	jmp next_k
	
backspace:
	cmp si,0        ;Ã»ÓĞÊäÈëµÄ×Ö·ûÌø×ª¼ÌĞøÊäÈë
	je keyin
	
	dec si
	mov byte [buf + si], 20h; ÌîÈë¿Õ¸ñ
	
	; ÏÔÊ¾×Ö·û´®Àı³Ì£¨ĞèÏÈÖÃ´®³¤CXºÍ´®µØÖ·BP£©
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	pusha
	mov cx,1       ; ´®³¤1
	mov bp,blank   ; ´®µØÖ·
	push cx			; ±£»¤CX£¨½øÕ»£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	pop cx			; »Ö¸´CX£¨³öÕ»£©
	;10	2	ÖÃ¹â±êÎ»ÖÃ	BH=Ò³ºÅ
    ;DH,DL=ĞĞ,ÁĞ
	
	dec dl          ; ÍË¸ñ
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	;dec dl          ; ÔÙÍËÒ»¸ñ
	; ÔÚµ±Ç°Î»ÖÃÏÔÊ¾×Ö·û´®£¨´®³¤CXºÍ´®µØÖ·BPÒÑÔ¤ÏÈÉèÖÃºÃÁË£©
	mov ah, 13h		; BIOSÖĞ¶ÏµÄ¹¦ÄÜºÅ£¨ÏÔÊ¾×Ö·û´®£©
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bh, 0 		; Ò³ºÅ=0
	mov bl, 0fh		; ×Ö·ûÑÕÉ«=²»ÉÁ£¨0£©ºÚµ×£¨000£©ÁÁ°××Ö£¨1111£©
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	
	;10	2	ÖÃ¹â±êÎ»ÖÃ	BH=Ò³ºÅ
    ;DH,DL=ĞĞ,ÁĞ
	
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	
	popa
	jmp keyin
	
	; ÏÔÊ¾ALÖĞµÄ¼üÈë×Ö·û
next_k:
	mov ah, 0eh 	; ¹¦ÄÜºÅ
	mov bl, 0fh 	; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	jmp keyin		; Ñ­»·¶Á´æÏÔ°´¼ü
return:
	ret 			; ´ÓÀı³Ì·µ»Ø

goout: ; ¼üÈëµÄ×Ö·ûÊı³¬¹ı»º³åÇø³¤¶ÈÊ±Ìø×ªµ½´Ë
	call showtoolong; ÏÔÊ¾´®Ì«³¤³ö´íĞÅÏ¢
	add sp, 2		; µ¯³öCALLÊ±Ñ¹Õ»µÄ·µ»ØµØÖ·
	jmp again		; ÖØĞÂ¿ªÊ¼Ö÷Ñ­»·
	
; -------------------------------------------------------------------
dtlen: ; È·¶¨ÃüÁî´®³¤¶È
	mov cx, buflen	; CX = ÊäÈë»º³åÇø³¤¶È£¨80£©
	mov al, 20h		; AL = ¿Õ¸ñ·û
	mov di, buf		; DIÖ¸Ïòbuf
	; ÔÚbufÖĞÕÒµ½µÚÒ»¸ö¿Õ¸ñ·ûºóÍ£Ö¹£º
	repne scasb		; CX>0 && [di]¡ÙAL Ê±DI++¼ÌĞøÉ¨Ãè£¬·ñÔòÍË³öÑ­»·
	jcxz toolong	; CX=0ÔòÃ»ÕÒµ½¿Õ¸ñ·û£¬´®³¤n = buflen >> cslen (= 8)
	; ¼ÆËã n = ÊäÈë»º³åÇø³¤¶È - CX - 1
	mov word [n], buflen ; n = buflen
	sub [n], cx		; n - CX
	dec word [n]	; n--
	je zlen 		; n=0£ºÖØĞÂ¿ªÊ¼ÃüÁîĞĞÑ­»·
	cmp word [n], cslen ; n > 8 ?
	ja toolong		; ÃüÁî´®³¤³¬¹ı8Ê±Ìø×ª
	ret 			; ´ÓÀı³Ì·µ»Ø

toolong: ; ÃüÁî´®Ì«³¤£¨±¨´íÍË³ö£©
	call showwrong	; ÏÔÊ¾³ö´íĞÅÏ¢
zlen: ; n=0Ê±ÖØĞÂ¿ªÊ¼
	add sp, 2		; µ¯³öcallÑ¹Õ»µÄ·µ»ØµØÖ·
	jmp again		; ÖØĞÂ¿ªÊ¼

; -------------------------------------------------------------------
tocap: ; ×ª»»³É´óĞ´×ÖÄ¸
	mov cx, [n]		; Ñ­»·´ÎÊı CX = n
	mov bx, 0		; ×Ö·ûÆ«ÒÆÖµ BX = 0£¨³õÖµÎª0£©
next: ; Ñ­»·¿ªÊ¼
	cmp byte [buf + bx], 61h	; ×Ö·ûÓë×ÖÄ¸a£¨61h£©±È½Ï
	jb notll					; ×Ö·û < 61h Ìø×ª
	cmp byte [buf + bx], 7ah	; ×Ö·ûÓë×ÖÄ¸z£¨7Ah£©±È½Ï
	ja notll					; ×Ö·û > 7Ah Ìø×ª
	sub byte [buf + bx], 20h	; Ğ¡Ğ´×ÖÄ¸ - 20h = ´óĞ´×ÖÄ¸
notll: ; ²»ÊÇĞ¡Ğ´×ÖÄ¸
	inc bx			; µİÔöÆ«ÒÆÖµ
	loop next		; ¼ÌĞøÑ­»·
	ret 			; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------
tocap_Dirbuf: ; ×ª»»³É´óĞ´×ÖÄ¸
	mov cx, [Dir_len]		; Ñ­»·´ÎÊı CX = n
	mov bx, 0		; ×Ö·ûÆ«ÒÆÖµ BX = 0£¨³õÖµÎª0£©
.next: ; Ñ­»·¿ªÊ¼
	cmp byte [Dirbuf + bx], 61h	; ×Ö·ûÓë×ÖÄ¸a£¨61h£©±È½Ï
	jb .notll					; ×Ö·û < 61h Ìø×ª
	cmp byte [Dirbuf + bx], 7ah	; ×Ö·ûÓë×ÖÄ¸z£¨7Ah£©±È½Ï
	ja .notll					; ×Ö·û > 7Ah Ìø×ª
	sub byte [Dirbuf + bx], 20h	; Ğ¡Ğ´×ÖÄ¸ - 20h = ´óĞ´×ÖÄ¸
.notll: ; ²»ÊÇĞ¡Ğ´×ÖÄ¸
	inc bx			; µİÔöÆ«ÒÆÖµ
	loop .next		; ¼ÌĞøÑ­»·
	ret 			; ´ÓÀı³Ì·µ»Ø

; -------------------------------------------------------------------
newstr:	; ¹¹ÔìĞÂ´®£¨ÃüÁî´® --> COMÎÄ¼şÃû£©
	mov si, buf		; Ô´´®ÆğÊ¼µØÖ·
	mov di, fnbuf	; Ä¿µÄ´®ÆğÊ¼µØÖ·
	mov cx, [n]		; Ñ­»·´ÎÊı CX = n
	; ½«ÊäÈë»º³åÇøbufÖĞµÄÃüÁî´®¸´ÖÆµ½ÎÄ¼şÃû»º³åÇøfnbuf£º
	rep movsb		; CX > 0Ê± [ES:DI] = [DS:SI]¡¢CX--£¬CX = 0Ê±ÍË³öÑ­»·
	ret 			; ´ÓÀı³Ì·µ»Ø

; -------------------------------------------------------------------
iscmd: ; ÅĞ¶ÏÊÇ·ñÎªÄÚ²¿ÃüÁî
	mov word [i], 0	; ÍâÑ­»·±äÁ¿/ÄÚ²¿ÃüÁîµÄĞòºÅi=0£¨³õÖµÎª0£©
	mov dx, cmdstr	; ÃüÁî´®µÄ³õÊ¼ÆğÊ¼µØÖ·
	
.1: ; ÍâÑ­»·
	mov si, fnbuf	; Ô´´®ÆğÊ¼µØÖ·
	mov di, dx		; Ä¿µÄ´®ÆğÊ¼µØÖ·
	mov cx, cslen 	; ÄÚÑ­»·´ÎÊı
	; ÖØ¸´±È½ÏÁ½×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	repe cmpsb		; CX>0 && [DS:SI]==[ES:DI]Ê±£¬CX--¡¢SI++¡¢DI++£¬¼ÌĞøÑ­»·£»·ñÔòÍË³ö
	jcxz docmd		; CX=0£¬±íÊ¾Á½´®ÏàµÈ£¬ÎªµÚBX¸öÄÚ²¿ÃüÁî´®£¬Ìø×ªÖ´ĞĞ¸ÃÃüÁî
	inc word [i]	; CX¡Ù0£¬±íÊ¾Á½´®²»µÈ£¬i++
	cmp word [i], N	; i=N£¨ÄÚ²¿ÃüÁî×ÜÊı£©£¿
	je .2			; ²»ÊÇÄÚ²¿ÃüÁî£¬ÍË³öÑ­»·
	add dx, cslen	; DX + 8 =ÏÂÒ»ÃüÁî´®µÄÆğÊ¼µØÖ·
	jmp .1			; ¼ÌĞøÍâÑ­»·
.2: ; ·µ»Ø
	;call showwrong	; ÏÔÊ¾³ö´íĞÅÏ¢
	ret 			; ´ÓÀı³Ì·µ»Ø
	
docmd: ; Ö´ĞĞÄÚ²¿ÃüÁî
	add sp, 2		; µ¯³öcall iscmdÊ±Ñ¹Õ»µÄ·µ»ØµØÖ·
	call newline	; »Ø³µ»»ĞĞ
	mov bx, [i]		; BX = ÄÚ²¿ÃüÁîµÄĞòºÅi
	shl bx, 1		; Æ«ÒÆµØÖ· = ÄÚ²¿ÃüÁîµÄĞòºÅ*2
	call [cmdaddr + bx] ; µ÷ÓÃµÚi¸öÄÚ²¿ÃüÁî
	jmp again		; Ìø×ªµ½ÃüÁîĞĞÑ­»·
	
;--------------------------------------------------------------------
exec: ; Ö´ĞĞÍâ²¿ÃüÁî£¨COMÎÄ¼ş£©

; ¶¨Òå³£Á¿£¨COMÎÄ¼ş¼ÓÔØÎ»ÖÃºÍ´ÅÅÌ²ÎÊı£©
BaseOfLoader	equ	2000h	; COMÎÄ¼ş±»¼ÓÔØµ½µÄÎ»ÖÃ ----  ¶ÎµØÖ·
OffsetOfLoader	equ	100h	; COMÎÄ¼ş±»¼ÓÔØµ½µÄÎ»ÖÃ ---- Æ«ÒÆµØÖ·
RootDirSectors	equ	14		; ¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
SectorNoOfRootDirectory	equ	19	; ¸ùÄ¿Â¼ÇøµÄÊ×ÉÈÇøºÅ
SectorNoOfFAT1	equ	1		; FAT#1µÄÊ×ÉÈÇøºÅ = BPB_RsvdSecCnt
DeltaSectorNo	equ	17		; DeltaSectorNo = BPB_RsvdSecCnt + 
							; (BPB_NumFATs * FATSz) - 2 = 1 + (2*9) -2 = 17
							; ÎÄ¼şµÄ¿ªÊ¼ÉÈÇøºÅ = Ä¿Â¼ÌõÄ¿ÖĞµÄ¿ªÊ¼ÉÈÇøºÅ 
							; + ¸ùÄ¿Â¼Õ¼ÓÃÉÈÇøÊıÄ¿ + DeltaSectorNo
	call Shut_dc
	push es		; ±£»¤ES
; ÈíÇı¸´Î»
	xor	ah, ah	; ¹¦ÄÜºÅah=0£¨¸´Î»´ÅÅÌÇı¶¯Æ÷£©
	xor	dl, dl	; dl=0£¨ÈíÇıA£¬ÈíÇıBÎª1¡¢Ó²ÅÌºÍUÅÌÎª80h£©
	int	13h		; ´ÅÅÌÖĞ¶Ï
	
; ÏÂÃæÔÚ´ÅÅÌÄ¿Â¼ÖĞÑ°ÕÒ COMÎÄ¼ş
	;ÅĞ¶ÏÊÇ¸ùÄ¿Â¼»òÕß×ÓÄ¿Â¼
	push ax
	mov ax,[SectorNoOfCurrentDirectory] 	; ¸ø±íÊ¾µ±Ç°ÉÈÇøºÅµÄ
	mov	word [wSectorNo], ax
						; ±äÁ¿wSectorNo¸³³õÖµÎªµ±Ç°Ä¿Â¼ÇøµÄÊ×ÉÈÇøºÅ
	mov ax, [CurrentDirSectors]	; Ê£ÓàÉÈÇøÊı
	mov word [wRootDirSizeForLoop],ax
										; ³õÊ¼»¯Îªµ±Ç°Ä¿Â¼ËùÕ¼ÉÈÇøÊı£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
	pop ax
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	LABEL_NOT_FOUND	; Èô¶ÁÍêÔò±íÊ¾Î´ÕÒµ½COMÎÄ¼ş
	dec	word [wRootDirSizeForLoop]	; µİ¼õ±äÁ¿wRootDirSizeForLoopµÄÖµ
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸öÄ¿Â¼ÉÈÇøµ½×°ÔØÇø
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨4000h£©
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader£¨100h£©
	mov	ax, [wSectorNo]	; AX <- ¸ùÄ¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov	cl, 1			; Ö»¶ÁÒ»¸öÉÈÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı

	mov	si, fnbuf		; DS:SI -> COMÎÄ¼ş
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; Çå³ıDF±êÖ¾Î»
						; ÖÃ±È½Ï×Ö·û´®Ê±µÄ·½ÏòÎª×ó/ÉÏ[Ë÷ÒıÔö¼Ó]
	mov	dx, 10h			; Ñ­»·´ÎÊı=16£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
LABEL_SEARCH_FOR_COM_FILE:
	cmp	dx, 0			; Ñ­»·´ÎÊı¿ØÖÆ
	jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR ; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø
	dec	dx				; µİ¼õÑ­»·´ÎÊıÖµ			  ¾ÍÌøµ½ÏÂÒ»ÉÈÇø
	mov	cx, 11			; ³õÊ¼Ñ­»·´ÎÊıÎª11
LABEL_CMP_FILENAME:
	repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND ; Èç¹û±È½ÏÁË11¸ö×Ö·û¶¼ÏàµÈ£¬±íÊ¾ÕÒµ½
LABEL_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0ÎªÁËÈÃËüÖ¸Ïò±¾ÌõÄ¿¿ªÍ·£¨µÍ5Î»ÇåÁã£©
						; FFE0h = 1111111111100000£¨µÍ5Î»=32=Ä¿Â¼ÌõÄ¿´óĞ¡£©
	add	di, 20h			; DI += 20h ÏÂÒ»¸öÄ¿Â¼ÌõÄ¿
	mov	si, fnbuf		; SIÖ¸Ïò×°ÔØÎÄ¼şÃû´®µÄÆğÊ¼µØÖ·
	jmp	LABEL_SEARCH_FOR_COM_FILE; ×ªµ½Ñ­»·¿ªÊ¼´¦

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:             ;ssssss
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	LABEL_NOT_FOUND ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc	word [wSectorNo]	; ¶ÔÓÚ¸ùÄ¿Â¼£¬µİÔöµ±Ç°ÉÈÇøºÅ
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NOT_FOUND:
	pop es			; »Ö¸´ES
	call showwrong	; ÏÔÊ¾×Ö·û´®
	ret

; ÏÂÃæ½«COMÎÄ¼ş¼ÓÔØµ½ÄÚ´æ
LABEL_FILENAME_FOUND:	; ÕÒµ½ COMÎÄ¼şºó±ãÀ´µ½ÕâÀï¼ÌĞø
	; ¼ÆËãÎÄ¼şµÄÆğÊ¼ÉÈÇøºÅ
	mov	ax, [CurrentDirSectors]	; AX=µ±Ç°Ä¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
	and	di, 0FFE0h		; DI -> µ±Ç°ÌõÄ¿µÄ¿ªÊ¼µØÖ·
	add	di, 1Ah			; DI -> ÎÄ¼şµÄÊ×ÉÈÇøºÅÔÚÌõÄ¿ÖĞµÄÆ«ÒÆµØÖ·
	mov cx, word [es:di] ; CX=ÎÄ¼şµÄÊ×ÉÈÇøºÅ
	push cx				; ±£´æ´ËÉÈÇøÔÚFATÖĞµÄĞòºÅ
	add	cx, RootDirSectors			; CX=ÎÄ¼şµÄÏà¶ÔÆğÊ¼ÉÈÇøºÅ+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı +¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı+¸ùÄ¿Â¼Õ¼ÓÃµÄÉÈÇøÊı
	;ÖØÒªµÄÊÂÇéËµÒ»Íò±é=_=,ÕÒÕâ¸öbugÓÃÁË¼¸Ğ¡Ê±   Ô­´úÂëadd	cx,ax   ÏÖÔÚ×ÓÄ¿Â¼ax²¢²»ÊÇ¸ùÄ¿Â¼Ê×ÉÈÇøºÅ
	add	cx, DeltaSectorNo ; CL <- COMÎÄ¼şµÄÆğÊ¼ÉÈÇøºÅ(0-based)
	mov	ax, BaseOfLoader      ;+1C
	mov	es, ax			; ES <- BaseOfLoader£¨COM³ÌĞò»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨COM³ÌĞòÆ«ÒÆµØÖ·=100h£©
	mov	ax, cx			; AX <- ÆğÊ¼ÉÈÇøºÅ

LABEL_GOON_LOADING_FILE:
	push bx				; ±£´æCOM³ÌĞòÆ«ÒÆµØÖ·
	mov	cl, 1			; 1¸öÉÈÇø
	call ReadSec		; ¶ÁÉÈÇø

	; ¼ÆËãÎÄ¼şµÄÏÂÒ»ÉÈÇøºÅ
	pop bx				; È¡³öCOM³ÌĞòÆ«ÒÆµØÖ·
	pop	ax				; È¡³ö´ËÉÈÇøÔÚFATÖĞµÄĞòºÅ
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	cmp	ax, 0FF8h		; ÊÇ·ñÊÇÎÄ¼ş×îºó´Ø
	jae	LABEL_FILE_LOADED ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	push ax				; ±£´æÉÈÇøÔÚFATÖĞµÄĞòºÅ
	mov	dx, RootDirSectors	; DX = ¸ùÄ¿Â¼ÉÈÇøÊı
	add	ax, dx			; ÉÈÇøĞòºÅ + ¸ùÄ¿Â¼ÉÈÇøÊı
	add	ax, DeltaSectorNo ; AX = Òª¶ÁµÄÊı¾İÉÈÇøµØÖ·
	add	bx, [BPB_BytsPerSec] ; BX+512Ö¸ÏòCOM³ÌĞòÇøµÄÏÂÒ»¸öÉÈÇøµØÖ·
	jmp	LABEL_GOON_LOADING_FILE

; ÏÂÃæÌø×ªÖ´ĞĞCOM³ÌĞò
LABEL_FILE_LOADED:
	pop es
	add sp, 2			; µ¯³öcallÖ¸ÁîÑ¹Õ»µÄ·µ»ØµØÖ·ºÍ±£´æµÄES
	jmp	BaseOfLoader:OffsetOfLoader	; ÕâÒ»¾äÌø×ªµ½ÒÑ¼ÓÔØµ½ÄÚ´æÖĞµÄ
						; COMÎÄ¼şµÄ¿ªÊ¼´¦£¬¿ªÊ¼Ö´ĞĞ COMÎÄ¼şµÄ´úÂë¡£
						; £¨COM³ÌĞòÍ¨¹ıµ÷ÓÃ21hÖĞ¶Ï·µ»ØÃüÁîĞĞ³ÌĞò£©

; ±äÁ¿
BPB_BytsPerSec	DW 512	; Ã¿ÉÈÇø×Ö½ÚÊı
BPB_SecPerTrk	DW 18	; Ã¿´ÅµÀÉÈÇøÊı

wRootDirSizeForLoop	dw	RootDirSectors	; ¸ùÄ¿Â¼ÇøÊ£ÓàÉÈÇøÊı
										; ³õÊ¼»¯Îª14£¬ÔÚÑ­»·ÖĞ»áµİ¼õÖÁÁã
wSectorNo		dw	0	; µ±Ç°ÉÈÇøºÅ£¬³õÊ¼»¯Îª0£¬ÔÚÑ­»·ÖĞ»áµİÔö
bOdd			db	0	; ÆæÊı»¹ÊÇÅ¼ÊıFATÏî
; -------------------------------------------------------------------
; ÃüÁîĞĞÖ÷Ñ­»·Àı³Ì½áÊø
; ===================================================================


; ===================================================================
; ´óĞÍ¸¨ÖúÀı³Ì¿ªÊ¼
;--------------------------------------------------------------------

;--------------------------------------------------------------------
; Àı³ÌÃû£ºGetFATEntry
;--------------------------------------------------------------------
; ×÷ÓÃ£ºÕÒµ½ĞòºÅÎªAXµÄÉÈÇøÔÚFATÖĞµÄÌõÄ¿£¬½á¹û·ÅÔÚAXÖĞ¡£ĞèÒª×¢ÒâµÄ
;     ÊÇ£¬ÖĞ¼äĞèÒª¶ÁFATµÄÉÈÇøµ½ES:BX´¦£¬ËùÒÔº¯ÊıÒ»¿ªÊ¼±£´æÁËESºÍBX
GetFATEntry:
	push es			; ±£´æES¡¢BXºÍAX£¨ÈëÕ»£©
	push bx
	push ax
; ÉèÖÃ¶ÁÈëµÄFATÉÈÇøĞ´ÈëµÄ»ùµØÖ·
	mov ax, BaseOfLoader	; AX=4000h
	sub	ax, 100h	; ÔÚBaseOfLoaderºóÃæÁô³ö4K¿Õ¼äÓÃÓÚ´æ·ÅFAT
	mov	es, ax		; ES=8F00h
; ÅĞ¶ÏFATÏîµÄÆæÅ¼
	pop	ax			; È¡³öFATÏîĞòºÅ£¨³öÕ»£©
	mov	byte [bOdd], 0; ³õÊ¼»¯ÆæÅ¼±äÁ¿ÖµÎª0£¨Å¼£©
	mov	bx, 3		; AX*1.5 = (AX*3)/2
	mul	bx			; DX:AX = AX * 3£¨AX*BX µÄ½á¹ûÖµ·ÅÈëDX:AXÖĞ£©
	mov	bx, 2		; BX = 2£¨³ıÊı£©
	xor	dx, dx		; DX=0	
	div	bx			; DX:AX / 2 => AX <- ÉÌ¡¢DX <- ÓàÊı
	cmp	dx, 0		; ÓàÊı = 0£¨Å¼Êı£©£¿
	jz LABEL_EVEN	; Å¼ÊıÌø×ª
	mov	byte [bOdd], 1	; ÆæÊı
LABEL_EVEN:		; Å¼Êı
	; ÏÖÔÚAXÖĞÊÇFATÏîÔÚFATÖĞµÄÆ«ÒÆÁ¿£¬ÏÂÃæÀ´
	; ¼ÆËãFATÏîÔÚÄÄ¸öÉÈÇøÖĞ(FATÕ¼ÓÃ²»Ö¹Ò»¸öÉÈÇø)
	xor	dx, dx		; DX=0	
	mov	bx, [BPB_BytsPerSec]	; BX=512
	div	bx			; DX:AX / 512
		  			; AX <- ÉÌ (FATÏîËùÔÚµÄÉÈÇøÏà¶ÔÓÚFATµÄÉÈÇøºÅ)
		  			; DX <- ÓàÊı (FATÏîÔÚÉÈÇøÄÚµÄÆ«ÒÆ)
	push dx			; ±£´æÓàÊı£¨ÈëÕ»£©
	mov bx, 0 		; BX <- 0 ÓÚÊÇ£¬ES:BX = 8F00h:0
	add	ax, SectorNoOfFAT1 ; ´Ë¾äÖ®ºóµÄAX¾ÍÊÇFATÏîËùÔÚµÄÉÈÇøºÅ
	mov	cl, 2			; ¶ÁÈ¡FATÏîËùÔÚµÄÉÈÇø£¬Ò»´Î¶ÁÁ½¸ö£¬±ÜÃâÔÚ±ß½ç
	call	ReadSec	; ·¢Éú´íÎó, ÒòÎªÒ»¸ö FATÏî¿ÉÄÜ¿çÔ½Á½¸öÉÈÇø
	pop	dx			; DX= FATÏîÔÚÉÈÇøÄÚµÄÆ«ÒÆ£¨³öÕ»£©
	add	bx, dx		; BX= FATÏîÔÚÉÈÇøÄÚµÄÆ«ÒÆ
	mov	ax, [es:bx]	; AX= FATÏîÖµ
	cmp	byte [bOdd], 1	; ÊÇ·ñÎªÆæÊıÏî£¿
	jnz	LABEL_EVEN_2	; Å¼ÊıÌø×ª
	shr	ax, 4			; ÆæÊı£ºÓÒÒÆ4Î»£¨È¡¸ß12Î»£©
LABEL_EVEN_2:		; Å¼Êı
	and	ax, 0FFFh	; È¡µÍ12Î»
LABEL_GET_FAT_ENRY_OK:
	pop	bx			; »Ö¸´ES¡¢BX£¨³öÕ»£©
	pop	es
	ret
;--------------------------------------------------------------------
;ÉèÖÃfat±íÏîÎª×îºó´Ø  getEmptyFatEntry  setEmptyFatEntry
getEmptyAX dw 2
getEmptyFatEntry:
;--------------------------------------------------------------------
; ×÷ÓÃ£ºÕÒµ½¿ÕÏĞ´ØºÅ
	push es
	mov ax,2
	mov word [getEmptyAX],2
.1
	call GetFATEntry
	cmp	ax, 0		; ÊÇ·ñÊÇ¿ÕÏĞ´Ø
	jz	.FindEmpty ;µÈÓÚÁãÈÏÎªÊÇ¿ÕÏĞ´Ø
	;call hex2ascii
	inc word[getEmptyAX]
	mov ax,[getEmptyAX]
	jmp .1
.FindEmpty:
	mov ax,[getEmptyAX]
	pop es
	ret
;--------------------------------------------------------------------
setEmptyAX dw 0
setEmptyFatEntry:
;--------------------------------------------------------------------
; ×÷ÓÃ  ÉèÖÃax¶ÔÓ¦µÄ´ØºÅ±»Õ¼ÓÃ
	push es			; ±£´æES¡¢BXºÍAX£¨ÈëÕ»£©
	push bx
	push ax
; ÉèÖÃ¶ÁÈëµÄFATÉÈÇøĞ´ÈëµÄ»ùµØÖ·
	mov ax, BaseOfLoader	; AX=4000h
	sub	ax, 100h	; ÔÚBaseOfLoaderºóÃæÁô³ö4K¿Õ¼äÓÃÓÚ´æ·ÅFAT
	mov	es, ax		; ES=8F00h
; ÅĞ¶ÏFATÏîµÄÆæÅ¼
	pop	ax			; È¡³öFATÏîĞòºÅ£¨³öÕ»£©
	mov	byte [bOdd], 0; ³õÊ¼»¯ÆæÅ¼±äÁ¿ÖµÎª0£¨Å¼£©
	mov	bx, 3		; AX*1.5 = (AX*3)/2
	mul	bx			; DX:AX = AX * 3£¨AX*BX µÄ½á¹ûÖµ·ÅÈëDX:AXÖĞ£©
	mov	bx, 2		; BX = 2£¨³ıÊı£©
	xor	dx, dx		; DX=0	
	div	bx			; DX:AX / 2 => AX <- ÉÌ¡¢DX <- ÓàÊı
	cmp	dx, 0		; ÓàÊı = 0£¨Å¼Êı£©£¿
	jz setEmpty_EVEN	; Å¼ÊıÌø×ª
	mov	byte [bOdd], 1	; ÆæÊı
setEmpty_EVEN:		; Å¼Êı
	; ÏÖÔÚAXÖĞÊÇFATÏîÔÚFATÖĞµÄÆ«ÒÆÁ¿£¬ÏÂÃæÀ´
	; ¼ÆËãFATÏîÔÚÄÄ¸öÉÈÇøÖĞ(FATÕ¼ÓÃ²»Ö¹Ò»¸öÉÈÇø)
	xor	dx, dx		; DX=0	
	mov	bx, [BPB_BytsPerSec]	; BX=512
	div	bx			; DX:AX / 512
		  			; AX <- ÉÌ (FATÏîËùÔÚµÄÉÈÇøÏà¶ÔÓÚFATµÄÉÈÇøºÅ)
		  			; DX <- ÓàÊı (FATÏîÔÚÉÈÇøÄÚµÄÆ«ÒÆ)
	push dx			; ±£´æÓàÊı£¨ÈëÕ»£©
	mov bx, 0 		; BX <- 0 ÓÚÊÇ£¬ES:BX = 8F00h:0
	add	ax, SectorNoOfFAT1 ; ´Ë¾äÖ®ºóµÄAX¾ÍÊÇFATÏîËùÔÚµÄÉÈÇøºÅ
	mov [setEmptyAX],ax
	
	mov	cl, 2			; ¶ÁÈ¡FATÏîËùÔÚµÄÉÈÇø£¬Ò»´Î¶ÁÁ½¸ö£¬±ÜÃâÔÚ±ß½ç
	call	ReadSec	; ·¢Éú´íÎó, ÒòÎªÒ»¸ö FATÏî¿ÉÄÜ¿çÔ½Á½¸öÉÈÇø
	pop	dx			; DX= FATÏîÔÚÉÈÇøÄÚµÄÆ«ÒÆ£¨³öÕ»£©
	add	bx, dx		; BX= FATÏîÔÚÉÈÇøÄÚµÄÆ«ÒÆ
	mov	ax, [es:bx]	; AX= FATÏîÖµ
	cmp	byte [bOdd], 1	; ÊÇ·ñÎªÆæÊıÏî£¿
	jnz	setEmpty_EVEN_2	; Å¼ÊıÌø×ª
	
	pusha
	or	ax, 0FFF0h
	;shrd bx,ax, 4			; ÆæÊı£ºÓÒÒÆ4Î»£¨È¡¸ß12Î»£©
	;mov dx,0FFFH
	;shld dx,bx,4            ;bxÖĞ±£´æµÄÊı¾İ·Å»Ødx
	mov	[es:bx],ax
	;Ğ´FAT±í
	mov ax,[setEmptyAX]
	mov cl,2
	mov bx,0
	call WriteSec	
	popa
	jmp setEmpty_GET_FAT_ENRY_OK
setEmpty_EVEN_2:		; Å¼Êı
	;and	ax, 0FFFh	; È¡µÍ12Î»
	pusha
	or	ax, 0FFFh
	;shld bx,ax, 4			; Å¼Êı£º×óÒÆ4Î»
	;mov dx,0FFF0H
	;shrd dx,bx,4            ;bxÖĞ±£´æµÄÊı¾İ·Å»Ødx
	mov	[es:bx],ax
	;Ğ´FAT±í
	mov ax,[setEmptyAX]
	mov cl,2
	mov bx,0
	call WriteSec	
	popa
setEmpty_GET_FAT_ENRY_OK:
	pop	bx			; »Ö¸´ES¡¢BX£¨³öÕ»£©
	pop	es
	ret
;--------------------------------------------------------------------
; Àı³ÌÃû£ºshowbpb
;--------------------------------------------------------------------
; ×÷ÓÃ£º; ÏÔÊ¾´ÅÅÌµÄBPBĞÅÏ¢
showbpb:
	call ReadPBootSec	; µ÷ÓÃ¶ÁÈë´ÅÅÌ·ÖÇøÒıµ¼ÉÈÇøÀı³Ì

	mov word [lns], 0	; µ±Ç°ÒÑÏÔÊ¾ĞĞÊı£¬³õÊ¼»¯Îª0
	
	; ÏÔÊ¾OEM´®---------------------------------------------
	mov cx, OEMMsgLen	; CX=´®³¤
	mov bp, OEMMsg		; BP="OEM:"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
	mov cx, 8			; CX=´®³¤=8
	mov bp, Sector + 3	; BP=BPBÖĞµÄOEM´®
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call newline		; »Ø³µ»»ĞĞ
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1

	; ÏÔÊ¾½éÖÊ´®---------------------------------------------
	mov cx, MediaMsgLen	; CX=´®³¤
	mov bp, MediaMsg	; BP="Media:"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
	cmp byte [Sector + 15h], 0F0h ; ½éÖÊÃèÊö·û > F0h ?
	jg HD				; > ÎªÓ²ÅÌ
	; ÈíÅÌ
	mov cx, FDMsgLen	; CX=ÈíÅÌµÄ´®³¤
	mov bp, FDMsg		; BP="Floppy Disk"
	jmp DStr			; Ìø×ªµ½ÏÔÊ¾´®
HD: ; Ó²ÅÌ
	mov cx, HDMsgLen	; Ó²ÅÌµÄ´®³¤=9
	mov bp, HDMsg		; BP="Hard Disk"
DStr: ; ÏÔÊ¾´®
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call newline		; »Ø³µ»»ĞĞ
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1
	
	; ÏÔÊ¾´ÅÅÌÈİÁ¿ --------------------------------------------------------
	; ÏÔÊ¾¡°Size:¡±´®
	mov cx, SizeMsgLen	; CX=´®³¤
	mov bp, SizeMsg		; BP="Size:"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û

	; »ñÈ¡Ê®½øÖÆÊı×Ö´®
	mov ax, [Sector + 13h] ; AX=×ÜÉÈÇøÊı
	shr ax, 1			; ÉÈÇøÊı/2 = KBÖµ
	call GetDigStr		; ÒÔAXÎª´«µİ²ÎÊı£¬BP(´®µØÖ·)ºÍCX(×Ö·û¸öÊı)Îª·µ»ØÖµ
	; ÏÔÊ¾Êı×Ö´®
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	
	; ÏÔÊ¾¡°KB¡±´®
	add dl, cl			; ÁĞºÅDL += Ê®½øÖÆÊı×Ö´®µÄ×Ö·û¸öÊı
	inc dl				; DL++£¨¿ÕÒ»¸ñ£©
	mov cx, KBMsgLen	; CX=´®³¤
	mov bp, KBMsg		; BP="KB"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call newline		; »Ø³µ»»ĞĞ
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1
	
	; ÏÔÊ¾ÎÄ¼şÏµÍ³ÀàĞÍ´®---------------------------------------------
	mov cx, FSMsgLen	; CX=´®³¤
	mov bp, FSMsg		; BP="File System:"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
	mov cx, 8			; CX=´®³¤=8
	mov bp, Sector + 36h ; BP=EBPBÖĞµÄÎÄ¼şÏµÍ³ÀàĞÍ´®
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call newline		; »Ø³µ»»ĞĞ
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1
	
	; ÏÔÊ¾BPBÖĞµÄ¾í±ê´®---------------------------------------------
	mov cx, VolMsgLen	; CX=´®³¤
	mov bp, VolMsg		; BP="Vol:"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
	mov cx, 11			; CX=´®³¤=11
	mov bp, Sector + 2Bh ; BP=EBPBÖĞµÄÎÄ¼şÏµÍ³ÀàĞÍ´®
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call newline		; »Ø³µ»»ĞĞ
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1
	
	; ÏÔÊ¾ID£¨ĞòÁĞºÅ£©---------------------------------------------
	mov cx, IDMsgLen	; CX=´®³¤
	mov bp, IDMsg		; BP="Vol:"
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
	call showid			; ÏÔÊ¾ID´®
	call newline		; »Ø³µ»»ĞĞ	
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1

	call newline		; »Ø³µ»»ĞĞ
	inc word [lns]		; lns++ ÒÑÏÔÊ¾ĞĞÊı+1

	ret					; ÖÕÖ¹³ÌĞò£¬·µ»Ø
	
; ¶¨Òå×Ö·û´®³£Á¿¼°Æä³¤¶ÈÖµ·ûºÅ³£Á¿£º	
OEMMsg db "OEM:"
OEMMsgLen equ $ - OEMMsg
MediaMsg db "Media:"
MediaMsgLen equ $ - MediaMsg
FDMsg db "Floppy Disk"
FDMsgLen equ $ - FDMsg
HDMsg db "Hard Disk"
HDMsgLen equ $ - HDMsg
SizeMsg db "Size:"
SizeMsgLen equ $ - SizeMsg
KBMsg db "KB"
KBMsgLen equ $ - KBMsg
FSMsg db "File System:"
FSMsgLen equ $ - FSMsg
VolMsg db "Vol:"
VolMsgLen equ $ - VolMsg
IDMsg db "ID:"
IDMsgLen equ $ - IDMsg

; -------------------------------------------------------------------	
showid: ; ÏÔÊ¾4BÕûÊıIDÖµµÄÊ®Áù½øÖÆ´®

	mov edx, [Sector + 27h] ; EDX = ID
	bswap edx		; ×Ö½Ú·´Ğò

	mov cx, 4		; Ñ­»·´ÎÊı
.1: ; ÏÔÊ¾µ¥¸ö×Ö½Ú
	; ÏÔÊ¾¸ß4Î»
	mov al, dl		; AL=ID¸ßÎ»×Ö½Ú
	and al, 0F0h	; È¡³ö¸ß4Î»
	shr al, 4		; AL >> 4
	call ShowChar	; µ÷ÓÃÏÔÊ¾×Ö·ûº¯Êı
	; ÏÔÊ¾µÍ4Î»
	mov al, dl		; AL=ID¸ßÎ»×Ö½Ú
	and al, 0Fh		; È¡³öµÍ4Î»
	call ShowChar	; µ÷ÓÃÏÔÊ¾×Ö·ûº¯Êı
	; ÏÂÒ»¸ö×Ö½Ú
	shr edx, 8		; EDX >> 8
	cmp cx, 3		; CX = 3 ?
	jne .2			; £¡= ¼ÌĞøÑ­»·
	; ÏÔÊ¾¼õºÅ·û'-'
	mov al,'-'		; AL = ¿Õ¸ñ·û
	mov ah,0Eh 		; ¹¦ÄÜºÅ£¨ÒÔµç´«·½Ê½ÏÔÊ¾µ¥¸ö×Ö·û£©
	mov bl,0fh 		; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
.2:
	loop .1			; Ñ­»·

	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------	

; -------------------------------------------------------------------	
; ÏÔÊ¾µ¥¸öÊ®Áù½øÖÆ×Ö·ûº¯Êı
ShowChar: ; ÏÔÊ¾Ò»¸öÊ®Áù½øÖÆÊı×Ö·û£º0~9¡¢A~F£¨ÒÔALÎª´«µİ²ÎÊı£©
	cmp al, 10		; AL < 10 ?
	jl .1			; AL < 10£ºÌø×ªµ½.1
	add al, 7		; AL >= 10£ºÏÔÊ¾×ÖÄ¸£¨ = ÊıÖµ += 37h£©
.1: ; Êı×Ö
	add al, 30h		; Êı×Ö×Ö·û = ÊıÖµ+=30h
	mov ah, 0Eh		; ¹¦ÄÜºÅ£¨ÒÔµç´«·½Ê½ÏÔÊ¾µ¥¸ö×Ö·û£©
	mov bl, 0fh 	; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
; -------------------------------------------------------------------	

; --------------------------------------------------------------------
ReadPBootSec: ; ¶ÁÈë´ÅÅÌµÄ·ÖÇøÒıµ¼ÉÈÇøµ½Sector´¦
	mov bx, Sector 	; ES:BX=¶ÁÈëÊı¾İµ½ÄÚ´æÖĞµÄ´æ´¢µØÖ·
	mov ah, 2 		; ¹¦ÄÜºÅ
	mov al, 1 		; Òª¶ÁÈëµÄÉÈÇøÊı
	mov dl, [drvno]	; ´ÅÅÌÇı¶¯Æ÷ºÅ£º0=ÈíÅÌA¡¢1=ÈíÅÌB¡¢80h=Ó²ÅÌC¡¢81h=Ó²ÅÌD
	mov dh, 0 		; ´ÅÍ·ºÅ
	mov ch, 0 		; ÖùÃæºÅ£¨ÈíÅÌ=0¡¢Ó²ÅÌ=1£©
	cmp byte[drvno], 1 ; Çı¶¯Æ÷ºÅ > 1 ? 
	jbe	.1			; <= 1 Ê±ÎªÈíÅÌ£¬ÖùÃæºÅCH=0
	mov ch, 1		; > 1 Ê±ÎªÓ²ÅÌ£¬ÖùÃæºÅCH=1
.1:
	mov cl, 1 		; ÆğÊ¼ÉÈÇøºÅ£¨±àºÅ´Ó1¿ªÊ¼£©
	int 13H 		; µ÷ÓÃ13HºÅÖĞ¶Ï
	ret 			; ´ÓÀı³Ì·µ»Ø
; ¶¨Òå»º³åÇø£¬ÓÃÓÚ´æ·Å´Ó´ÅÅÌ¶ÁÈëµÄÉÈÇø
Sector:
	resb 512

; --------------------------------------------------------------------
DispStr: ; ÏÔÊ¾×Ö·û´®Àı³Ì£¨ĞèÏÈÖÃ´®³¤CXºÍ´®µØÖ·BP£©
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	push cx			; ±£»¤CX£¨½øÕ»£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	pop cx			; »Ö¸´CX£¨³öÕ»£©

	; ÔÚµ±Ç°Î»ÖÃÏÔÊ¾×Ö·û´®£¨´®³¤CXºÍ´®µØÖ·BPÒÑÔ¤ÏÈÉèÖÃºÃÁË£©
	mov ah, 13h		; BIOSÖĞ¶ÏµÄ¹¦ÄÜºÅ£¨ÏÔÊ¾×Ö·û´®£©
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bh, 0 		; Ò³ºÅ=0
	mov bl, 0fh		; ×Ö·ûÑÕÉ«=²»ÉÁ£¨0£©ºÚµ×£¨000£©ÁÁ°××Ö£¨1111£©
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	ret				; ´ÓÀı³Ì·µ»Ø
	
; --------------------------------------------------------------------
; »ñÈ¡×ÖÊı¾İÖµÊ®½øÖÆ´®Àı³Ì
dn equ 5 ; ×î´óÎ»Êı
GetDigStr: ; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	mov cx, 1		; Î»Êı=1£¨³õÖµ£©
	mov bp, sbuf	; BP = sbuf + dn - 1 = sbufµÄµ±Ç°Î»ÖÃ
	add bp, dn - 1
	mov bx,10		; ³ıÊı=10
DLoop: ; Ñ­»·¿ªÊ¼´¦
	mov dx, 0		; DX=0, DX:AX / BX -> ÉÌAX¡¢ÓàDX
	div bx
	add dl, 30h		; ÓàÊı + 30h = ¶ÔÓ¦µÄÊı×Ö·ûASCIIÂë
	mov [bp], dl	; sbuf[BP] = DL
	cmp ax, 0		; ÉÌAX = 0 ?
	je OutLoop		; = 0 Ìø³öÑ­»·
	inc cx			; Î»ÊıCX++
	dec bp			; Êı×Ö·ûµÄµ±Ç°Î»ÖÃBP--
	jmp DLoop		; ¼ÌĞøÑ­»·
OutLoop: ; ÍË³öÑ­»·
	ret				; ´ÓÀı³Ì·µ»Ø

sbuf: resb dn ; ÓÃÓÚ´æ·ÅÊ®½øÖÆÊı×Ö´®µÄ»º³åÇø£¬´óĞ¡ = ³£Á¿dn£¨=5£©


;--------------------------------------------------------------------
; Àı³ÌÃû£ºReadSec
;--------------------------------------------------------------------
; ×÷ÓÃ£º´ÓµÚ AX¸öÉÈÇø¿ªÊ¼£¬½«CL¸öÉÈÇø¶ÁÈëES:BXÖĞ
; ĞèÊ¹ÓÃ´ÅÅÌ²ÎÊısecspt(Ã¿´ÅµÀÉÈÇøÊı£©ºÍheads(´ÅÍ·Êı£©
ReadSec:
	; ---------------------------------------------------------------
	; ÔõÑùÓÉÉÈÇøºÅÇóÉÈÇøÔÚ´ÅÅÌÖĞµÄÎ»ÖÃ (ÉÈÇøºÅ->ÖùÃæºÅ¡¢ÆğÊ¼ÉÈÇø¡¢´ÅÍ·ºÅ)
	; ---------------------------------------------------------------
	; ÉèÉÈÇøºÅÎª x£¨= AX£©
	;                             ©° ÖùÃæºÅC = y / ´ÅÍ·Êı
	;         x            ©° ÉÌ y ©È
	;   -------------- 	=> ©È      ©¸ ´ÅÍ·ºÅH = y % ´ÅÍ·Êı
	;    Ã¿´ÅµÀÉÈÇøÊı      ©¦
	;                      ©¸ Óà z => ÆğÊ¼ÉÈÇøºÅS = z + 1
	push es
	push cx			; ±£´æÒª¶ÁµÄÉÈÇøÊıCL
	push bx			; ±£´æBX
	mov	bl, [secspt]; BL(= ´ÅµÀÉÈÇøÊı£©Îª³ıÊı
	div	bl			; AX/BL£¬ÉÌyÔÚALÖĞ¡¢ÓàÊızÔÚAHÖĞ
	inc	ah			; z ++£¨Òò´ÅÅÌµÄÆğÊ¼ÉÈÇøºÅÎª1£©£¬AH = ÆğÊ¼ÉÈÇøºÅ
	mov	cl, ah		; CL <- ÆğÊ¼ÉÈÇøºÅS
	mov	ah, 0		; AX <- y
	mov bl, [heads]	; BL(= ´ÅÍ·Êı£©Îª³ıÊı
	div	bl			; AX/BL£¬ÉÌÔÚALÖĞ¡¢ÓàÊıÔÚAHÖĞ
	mov	ch, al		; CH <- ÖùÃæºÅC
	mov	dh, ah		; DH <- ´ÅÍ·ºÅH
	; ÖÁ´Ë£¬"ÖùÃæºÅ¡¢ÆğÊ¼ÉÈÇø¡¢´ÅÍ·ºÅ"ÒÑÈ«²¿µÃµ½
	pop	bx			; »Ö¸´BX
	pop ax			; AL = »Ö¸´µÄÒª¶ÁµÄÉÈÇøÊıCL
	mov	dl, [drvno]	; Çı¶¯Æ÷ºÅ
.1: ; Ê¹ÓÃ´ÅÅÌÖĞ¶Ï¶ÁÈëÉÈÇø
	mov	ah, 2		; ¹¦ÄÜºÅ£¨¶ÁÉÈÇø£©
	int	13h			; ´ÅÅÌÖĞ¶Ï
	jc .1			; Èç¹û¶ÁÈ¡´íÎó£¬CF»á±»ÖÃÎª1£¬ÕâÊ±¾Í²»Í£µØ¶Á£¬Ö±µ½ÕıÈ·ÎªÖ¹
	pop es
	ret
;--------------------------------------------------------------------
; Àı³ÌÃû£ºWriteSec
;--------------------------------------------------------------------
; ×÷ÓÃ£º´ÓµÚ AX¸öÉÈÇø¿ªÊ¼£¬½«ES:BXÖĞ Ğ´µ½CL¸öÉÈÇøÖĞ
; ĞèÊ¹ÓÃ´ÅÅÌ²ÎÊısecspt(Ã¿´ÅµÀÉÈÇøÊı£©ºÍheads(´ÅÍ·Êı£©
WriteSec:
	; ---------------------------------------------------------------
	; ÔõÑùÓÉÉÈÇøºÅÇóÉÈÇøÔÚ´ÅÅÌÖĞµÄÎ»ÖÃ (ÉÈÇøºÅ->ÖùÃæºÅ¡¢ÆğÊ¼ÉÈÇø¡¢´ÅÍ·ºÅ)
	; ---------------------------------------------------------------
	; ÉèÉÈÇøºÅÎª x£¨= AX£©
	;                             ©° ÖùÃæºÅC = y / ´ÅÍ·Êı
	;         x            ©° ÉÌ y ©È
	;   -------------- 	=> ©È      ©¸ ´ÅÍ·ºÅH = y % ´ÅÍ·Êı
	;    Ã¿´ÅµÀÉÈÇøÊı      ©¦
	;                      ©¸ Óà z => ÆğÊ¼ÉÈÇøºÅS = z + 1
	push es
	push cx			; ±£´æÒª¶ÁµÄÉÈÇøÊıCL
	push bx			; ±£´æBX
	mov	bl, [secspt]; BL(= ´ÅµÀÉÈÇøÊı£©Îª³ıÊı
	div	bl			; AX/BL£¬ÉÌyÔÚALÖĞ¡¢ÓàÊızÔÚAHÖĞ
	inc	ah			; z ++£¨Òò´ÅÅÌµÄÆğÊ¼ÉÈÇøºÅÎª1£©£¬AH = ÆğÊ¼ÉÈÇøºÅ
	mov	cl, ah		; CL <- ÆğÊ¼ÉÈÇøºÅS
	mov	ah, 0		; AX <- y
	mov bl, [heads]	; BL(= ´ÅÍ·Êı£©Îª³ıÊı
	div	bl			; AX/BL£¬ÉÌÔÚALÖĞ¡¢ÓàÊıÔÚAHÖĞ
	mov	ch, al		; CH <- ÖùÃæºÅC
	mov	dh, ah		; DH <- ´ÅÍ·ºÅH
	; ÖÁ´Ë£¬"ÖùÃæºÅ¡¢ÆğÊ¼ÉÈÇø¡¢´ÅÍ·ºÅ"ÒÑÈ«²¿µÃµ½
	pop	bx			; »Ö¸´BX
	pop ax			; AL = »Ö¸´µÄÒª¶ÁµÄÉÈÇøÊıCL
	mov	dl, [drvno]	; Çı¶¯Æ÷ºÅ
.1: ; Ê¹ÓÃ´ÅÅÌÖĞ¶Ï¶ÁÈëÉÈÇø
	mov	ah, 3		; ¹¦ÄÜºÅ Ğ´ÉÈÇø
	int	13h			; ´ÅÅÌÖĞ¶Ï
	jc .1			; Èç¹û¶ÁÈ¡´íÎó£¬CF»á±»ÖÃÎª1£¬ÕâÊ±¾Í²»Í£µØ¶Á£¬Ö±µ½ÕıÈ·ÎªÖ¹
	pop es
	ret
;--------------------------------------------------------------------
;--------------------------------------------------------------------
; Àı³ÌÃû£ºls        
;×ÓÄ¿Â¼×÷³öĞŞ¸Ä(×ÓÄ¿Â¼ºÍÎÄ¼şÒ»Ñù£¬¼«ÓĞ¿ÉÄÜ²»Á¬Ğø£¬Ô­À´µÄËã·¨²»¿É¿¿£¬ÒªÈ¥²éfatÏîÔÙ¼ÆËãÏÂÒ»ÉÈÇøÎ»ÖÃ)
;--------------------------------------------------------------------
; ×÷ÓÃ£º; ÏÔÊ¾´ÅÅÌ¸ùÄ¿Â¼ÎÄ¼şĞÅÏ¢ÁĞ±í
; ĞèÊ¹ÓÃ´ÅÅÌ²ÎÊısecspt(Ã¿´ÅµÀÉÈÇøÊı£©ºÍheads(´ÅÍ·Êı£©
ls: 
	;mov word[nsec],1
	;mov word[isec],1fh
	;add word[isec], 47h			
	
	;call getdiskparam	; »ñÈ¡´ÅÅÌ²ÎÊıH&S
	; »ñÈ¡´ÅÅÌ²ÎÊıH/S
	;mov ax, [Sector + 18h]	; AX = Ã¿´ÅµÀÉÈÇøÊı
	;mov [secspt], ax		; secspt = AX = Ã¿´ÅµÀÉÈÇøÊı
	;mov ax, [Sector + 1Ah]	; AX = ´ÅÍ·Êı
	;mov [heads], ax			; heads = AX = ´ÅÍ·Êı
	; ¶ÔÓ²ÅÌisecĞè¼ÓµÚ1¸öÖùÃæµÄÉÈÇøÊı
	mov ax,[isec]
	mov [isec_ls],ax
	mov ax,[nsec]
	mov [nsec_ls],ax
	cmp byte [drvno], 80h	; Çı¶¯Æ÷ºÅ=80h£¨Ó²ÅÌC£©£¿
	je hdc					; = 80h Ìø×ª
	jmp begain				; ÈíÅÌ
hdc: ; Ó²ÅÌC
	; ¼ÆËã·ÖÇøÇ°µÄÉÈÇøÊı£¨¼ÙÉè = 1¸öÖùÃæÉÈÇøÊı£©= Ã¿´ÅµÀÉÈÇøÊı * ´ÅÍ·Êı
	mov ax, [secspt] 		; AX = Ã¿´ÅµÀÉÈÇøÊı
	mul word [heads]		; AX *= ´ÅÍ·Êı = 1¸öÖùÃæÉÈÇøÊı
	add [isec_ls], ax			; isec += 1¸öÖùÃæÉÈÇøÊı = Ó²ÅÌ¸ùÄ¿Â¼Ê×ÉÈÇøºÅ

begain: 
	; ÏÂÃæÔÚ´ÅÅÌµ±Ç°Ä¿Â¼ÖĞÑ°ÕÒÎÄ¼şÄ¿Â¼ÌõÄ¿
searchrdir: ; ËÑË÷µ±Ç°Ä¿Â¼Ñ­»·£¨Öğ¸ö¶ÁÈë¸ùÄ¿Â¼ÉÈÇø£©
	cmp	word [nsec_ls], 0	; ÅĞ¶Ï¸ùÄ¿Â¼ÇøÊÇ·ñÒÑ¶ÁÍê
	jz	exit			; Èô¶ÁÍêÔòÍË³ö
	dec	word [nsec_ls]		; nsec--
	; µ÷ÓÃ¶ÁÉÈÇøº¯Êı¶ÁÈëÒ»¸öÄ¿Â¼ÉÈÇøµ½»º³åÇø
	mov	bx, Sector		; BX = Sector
	mov	ax, [isec_ls]		; AX <- µ±Ç°Ä¿Â¼ÖĞµÄµ±Ç°ÉÈÇøºÅ
	mov cl, 1			; ¶ÁÒ»¸öÉÈÇøµ½»º³åÇø
	call ReadSec		; µ÷ÓÃ¶ÁÉÈÇøº¯Êı
	
	mov	di, Sector		; ES:DI -> Sector	
	mov	word [i], 10h	; Ñ­»·´ÎÊı=16£¨Ã¿¸öÉÈÇøÓĞ16¸öÎÄ¼şÌõÄ¿£º512/32=16£©
searchfi: ; ËÑË÷ÎÄ¼şÏîÑ­»·£¨ÔÚµ±Ç°ÉÈÇøÖĞÖğ¸ö¼ì²éÎÄ¼şÄ¿Â¼Ïî£©
	cmp	word [i], 0		; Ñ­»·´ÎÊı¿ØÖÆ
	jz nextsec 			; ÈôÒÑ¶ÁÍêÒ»ÉÈÇø£¬Ìøµ½ÏÂÒ»ÉÈÇø
	dec	word [i]		; µİ¼õÑ­»·´ÎÊıÖµ
	; ÅĞ¶ÏÊÇ·ñÎªÎÄ¼şÌõÄ¿£¨0¿ªÊ¼µÄÎª¿ÕÏî¡¢E5h¿ªÊ¼µÄÎªÒÑÉ¾Ïî¡¢ÊôĞÔµÍ4Î»È«1µÄ
	; Îª³¤ÎÄ¼şÃûÏî»òÏµÍ³Õ¼ÓÃÏî¡¢¾í±êÏîµÄÊôĞÔ3ºÅÎ»Îª1£©
	cmp	byte [di], 0	; ÎÄ¼şÃûµÄÊ××ÖÄ¸=0£¿
	jz	notfi			; Îª¿ÕÄ¿Â¼Ïî
	cmp	byte [di], 0E5h	; ÎÄ¼şÃûµÄÊ××ÖÄ¸=E5£¿
	jz	notfi 			; ÎªÒÑÉ¾³ıÄ¿Â¼Ïî
	cmp	byte [di + 11], 0Fh; ÎÄ¼şÊôĞÔ=0Fh£¿
	jz	notfi 			; Îª³¤ÎÄ¼şÃûÄ¿Â¼Ïî

	; ÏÔÊ¾ÎÄ¼şÃû´®
	inc word [lns]		; µ±Ç°ÆÁÄ»ÉÏµÄÎÄ¼şÌõÄ¿Êılns++
	Inc word [FileNum]
	; ÅĞ¶ÏÊÇ·ñµ½ÁËÆÁÄ»µ×²¿
	cmp word [lns], 30	; ĞĞÊı = 30 £¿
	jb .1				; < 24 ¼ÌĞø
	mov word [lns], 1	; ÖØĞÂÉèÒÑÏÔÊ¾ĞĞÊıÎª1
	call waitforkey		; °´ÈÎÒâ¼ü¼ÌĞø
.1: ; ¼ÌĞø
	; ÏÔÊ¾ÎÄ¼şÌõÄ¿ĞÅÏ¢£¨ÎÄ¼şÃû¡¢´óĞ¡¡¢Ê±¼ä£©
	; ÏÔÊ¾ÎÄ¼şÃû´®
	mov bp, di			; BP=ÎÄ¼şÃû×Ö·û´®µÄÆğÊ¼µØÖ·
	mov cx, 11			; ÎÄ¼şÃû´®³¤8+3=11
	;ÅĞ¶ÏÊÇ·ñÎªÖĞÎÄ
	push ax
	mov al,[di + 02h]
	cmp al,80h
	ja .Chin            ; ÎŞ·ûºÅ´óĞ¡¿ØÖÆ
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
	jmp .DispEnd
.Chin
	mov cx,4            ;Ö»ÄÜÏÔÊ¾ËÄ¸öºº×Ö
	call DispStr_Chinese		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	mov cx,4            ;Ö»ÄÜÏÔÊ¾ËÄ¸öºº×Ö
	add bp,8
	mov cx,3
	call DispStr		; µ÷ÓÃÏÔÊ¾×Ö·û´®Àı³Ì
	call space			; ²åÈë¿Õ¸ñ·û
.DispEnd
	pop ax
	; ¶Ô¾í±êÏî£¬²»ÏÔÊ¾ÎÄ¼ş´óĞ¡£¬ÏÔÊ¾±êÊ¶´®"<VOL>"
	mov al, [di + 0Bh]	; AL=ÎÄ¼şÊôĞÔ
	and al, 8h			; AL & 8£¨¾í±êÎ»£©
	jz .1.1	 			; ²»Îª¾í±ê
	; Îª¾í±ê£¬ÏÔÊ¾×Ö·û´®"<VOL>"
	mov bp, volbuf		; ´®µØÖ·
	mov cx, fsbuflen + btbuflen + 1	; ´®³¤=ÎÄ¼ş´óĞ¡µÄ´®³¤
	call DispStr		; ÏÔÊ¾×Ö·û´®
	jmp .3				; Ìø¹ıÏÔÊ¾ÎÄ¼ş´óĞ¡´®
.1.1:	
	; ¶Ô×ÓÄ¿Â¼Ïî£¬²»ÏÔÊ¾ÎÄ¼ş´óĞ¡£¬ÏÔÊ¾±êÊ¶´®"<DIR>"
	cmp byte [di + 0Bh], 10h ; Îª×ÓÄ¿Â¼£¿
	jne .2				; ÏÔÊ¾ÎÄ¼ş´óĞ¡
	; ÏÔÊ¾×Ö·û´®"<DIR>"
	mov bp, dsbuf		; ´®µØÖ·
	mov cx, fsbuflen + btbuflen + 1	; ´®³¤=ÎÄ¼ş´óĞ¡µÄ´®³¤
	call DispStr		; ÏÔÊ¾×Ö·û´®
	jmp .3				; Ìø¹ıÏÔÊ¾ÎÄ¼ş´óĞ¡´®
	
.2: ; ¼ÆËã²¢ÏÔÊ¾ÎÄ¼ş´óĞ¡Ê®½øÖÆ´®
	;push eax
	;push ebx
	;mov ebx,[FileSize] 
	;add eax,ebx ;ÎÄ¼ş×Ü´óĞ¡++
	;mov [FileSize],eax
	;pop ebx
	push eax
	mov eax, [di + 1Ch]; EAX = ÎÄ¼ş´óĞ¡
	add [FileSize],eax ;ÎÄ¼ş´óĞ¡++
	pop eax
	call getsizestr		; »ñÈ¡ÎÄ¼ş´óĞ¡Ê®½øÖÆ´®
	mov bp, fsbuf		; ´®µØÖ·
	mov cx, fsbuflen	; ´®³¤
	; ÏÔÊ¾ÎÄ¼ş´óĞ¡×Ö·û´®
	call DispStr		; ÏÔÊ¾×Ö·û´®
	call space			; ²åÈë¿Õ¸ñ·û
	; ÏÔÊ¾×Ö½Ú×Ö·û´®£¨ÎÄ¼ş´óĞ¡µ¥Î»£©"Byte"
	mov bp, btbuf		; ´®µØÖ·
	mov cx, btbuflen	; ´®³¤
	call DispStr		; ÏÔÊ¾×Ö·û´®

.3: ; ²åÈëÈô¸É¿Õ¸ñ·Ö¸ô·û
	call space			; ²åÈë¿Õ¸ñ·û
	call space			; ²åÈë¿Õ¸ñ·û
	call space			; ²åÈë¿Õ¸ñ·û
	
	; ÏÔÊ¾Ê±¼ä£¨ÄêÔÂÈÕÊ±·ÖÃë£¬¸ñÊ½Îª£ºyyyy.mm.dd  hh:mm:ss£©
	; ÏÔÊ¾ÈÕÆÚ£¨Äê.ÔÂ.ÈÕ£©
	mov ax, [di + 18h]	; AX = ÈÕÆÚ£¨µÍ5Î»ÎªÈÕ¡¢ÖĞ4Î»ÎªÔÂ¡¢¸ß7Î»ÎªÄê-1980£©
	push ax				; ±£´æAX½øÕ»
	; ÏÔÊ¾Äê£¨¸ß7Î»ÎªÄê-1980£©
	shr ax, 9			; AX >> 9£¬AX = Äê - 1980
	add ax, 1980		; AX + 1980 = Äê
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	call DispStr		; ÏÔÊ¾Äê×Ö·û´®
	; ÏÔÊ¾ÔÂ£¨ÖĞ4Î»ÎªÔÂ£©
	pop ax				; µ¯³öAX = ÈÕÆÚ
	push ax				; ±£´æAX½øÕ»
	shr ax, 5			; AX >> 5
	and ax, 0Fh			; AX & 1111 b = ÔÂ
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	cmp cx, 1			; ´®³¤ > 1 £¿
	ja .4				; > 1£ºÌø×ª
	; = 1£º²¹³ä×Ö·û'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ¼ÓÇ°µ¼'0'
	inc cx				; ´®³¤CX++
.4: ; Ìí¼Ó¾äµã·Ö¸ô·û'.'
	dec bp				; BP--
	mov byte [bp], '.'	; ¼Ó¾äµã·û'.'
	inc cx				; ´®³¤CX++
	call DispStr		; ÏÔÊ¾ÔÂ×Ö·û´®
	; ÏÔÊ¾ÈÕ£¨µÍ5Î»ÎªÈÕ£©
	pop ax				; µ¯³öAX = ÈÕÆÚ
	and ax, 1Fh			; AX & 1 1111 b = ÈÕ
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	cmp cx, 1			; ´®³¤ > 1 £¿
	ja .5				; > 1£ºÌø×ª
	; = 1£º²¹³ä×Ö·û'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ¼ÓÇ°µ¼'0'
	inc cx				; ´®³¤CX++
.5: ; Ìí¼Ó¾äµã·Ö¸ô·û'.'
	dec bp				; BP--
	mov byte [bp], '.'	; ¼Ó¾äµã·û'.'
	inc cx				; ´®³¤CX++
	call DispStr		; ÏÔÊ¾ÈÕ×Ö·û´®
	call space			; ²åÈë¿Õ¸ñ·û
	call space			; ²åÈë¿Õ¸ñ·û

	; ÏÔÊ¾Ê±¼ä£¨Ê±:·Ö:Ãë£©	
	mov ax, [di + 16h]	; AX = Ê±¼ä£¨µÍ5Î»ÎªÃë/2¡¢ÖĞ6Î»Îª·Ö¡¢¸ß5Î»ÎªÊ±£©
	push ax				; ±£´æAX½øÕ»
	; ÏÔÊ¾Ê±£¨¸ß5Î»ÎªÊ±£©
	shr ax, 11			; AX >> 11£¬AX = Ê±
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	cmp cx, 1			; ´®³¤ > 1 £¿
	ja .6				; > 1£ºÌø×ª
	; = 1£º²¹³ä×Ö·û'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ¼ÓÇ°µ¼'0'
	inc cx				; ´®³¤CX++
.6:	
	call DispStr		; ÏÔÊ¾Ê±×Ö·û´®
	; ÏÔÊ¾·Ö£¨ÖĞ6Î»Îª·Ö£©
	pop ax				; µ¯³öAX = Ê±¼ä
	push ax				; ±£´æAX½øÕ»
	shr ax, 5			; AX >> 5
	and ax, 3Fh			; AX & 11 1111 b = ·Ö
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	cmp cx, 1			; ´®³¤ > 1 £¿
	ja .7				; > 1£ºÌø×ª
	; = 1£º²¹³ä×Ö·û'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ¼ÓÇ°µ¼'0'
	inc cx				; ´®³¤CX++
.7: ; Ìí¼ÓÃ°ºÅ·Ö¸ô·û':'
	dec bp				; BP--
	mov byte [bp], ':'	; ¼ÓÇ°µ¼':'
	inc cx				; ´®³¤CX++
	call DispStr		; ÏÔÊ¾ÔÂ×Ö·û´®
	; ÏÔÊ¾Ãë£¨µÍ5Î»ÎªÃë/2£©
	pop ax				; µ¯³öAX = Ê±¼ä
	and ax, 1Fh			; AX & 1 1111 b = Ãë/2
	shl ax, 1			; AX << 1£¬AX*2 = Ãë
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	cmp cx, 1			; ´®³¤ > 1 £¿
	ja .8				; > 1£ºÌø×ª
	; = 1£º²¹³ä×Ö·û'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ¼ÓÇ°µ¼'0'
	inc cx				; ´®³¤CX++
.8: ; Ìí¼ÓÃ°ºÅ·Ö¸ô·û':'
	dec bp				; BP--
	mov byte [bp], ':'	; ¼ÓÇ°µ¼':'
	inc cx				; ´®³¤CX++
	call DispStr		; ÏÔÊ¾ÈÕ×Ö·û´®
	
	call newline		; »Ø³µ»»ĞĞ
	
notfi:
	add	di, 20h			; DI += 20h Ö¸ÏòÏÂÒ»¸öÄ¿Â¼ÌõÄ¿¿ªÊ¼´¦
	jmp	searchfi		; ×ªµ½Ñ­»·¿ªÊ¼´¦

nextsec:   ;¶ÔÓÚ×ÓÄ¿Â¼nextsecÒª×Ô¼ºËã³öÀ´(Ö±½ÓÊ¹ÓÃtoDirÖĞµÄËã·¨)  Óë¸ùÄ¿Â¼Ëã·¨²»Í¬
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader£¨»º³åÇø»ùÖ·=4000h£©
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader£¨»º³åÇøÆ«ÒÆµØÖ·=100h£©
	mov ax,[isec_ls]
	sub ax,1fh
	call GetFATEntry	; »ñÈ¡FATÏîÖĞµÄÏÂÒ»´ØºÅ
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; ÊÇ·ñÊÇÄ¿Â¼µÄ×îºó´Ø
	jae	exit ; ¡İFF8hÊ±Ìø×ª£¬·ñÔò¶ÁÏÂÒ»¸ö´Ø
	
	push ax
	mov ax,[temp_ax]
	mov	word [isec_ls],ax
	add	word [isec_ls],1fh 	; ĞŞ¸Ä³É¼´½«·ÃÎÊµÄÉÈÇøºÅ  
	pop ax
	jmp	searchrdir		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
.root:
	inc word [isec_ls]  ;¶ÔÓÚ¸ùÄ¿Â¼£¬Ö»Ğè×ÔÔö
	jmp searchrdir		; ¼ÌĞøËÑË÷Ä¿Â¼Ñ­»·
exit: ; ÖÕÖ¹³ÌĞò£¬·µ»Ø
.9:
	call newline
	mov bp,fileNumberBuf1
	mov cx,fileNumberBufLen1
	call DispStr
	
	call space			; ²åÈë¿Õ¸ñ·û
	
	mov ax,[FileNum]
	call GetDigStr 		; ÒÔAXÎª´«µİ²ÎÊı£¬[´®µØÖ·]BPºÍ[×Ö·û¸öÊı]CXÎª·µ»ØÖµ
	call DispStr
	
	call space			; ²åÈë¿Õ¸ñ·û
	
	mov bp,fileNumberBuf2
	mov cx,fileNumberBufLen2
	call DispStr
	
	mov word[FileNum],0     ;ÏÔÊ¾ÍêÇåÁã¼ÆÊıÆ÷
.10:
	push di
	mov di,FileSize-1ch
	call getsizestr		; »ñÈ¡ÎÄ¼ş´óĞ¡Ê®½øÖÆ´®
	pop di
	
	mov bp, fsbuf		; ´®µØÖ·
	mov cx, fsbuflen	; ´®³¤
	; ÏÔÊ¾ÎÄ¼ş´óĞ¡×Ö·û´®
	call DispStr		; ÏÔÊ¾×Ö·û´®
	call space			; ²åÈë¿Õ¸ñ·û
	; ÏÔÊ¾×Ö½Ú×Ö·û´®£¨ÎÄ¼ş´óĞ¡µ¥Î»£©"Byte"
	mov bp, btbuf		; ´®µØÖ·
	mov cx, btbuflen	; ´®³¤
	call DispStr		; ÏÔÊ¾×Ö·û´®
	mov dword[FileSize],0     ;ÏÔÊ¾ÍêÇåÁã¼ÆÊıÆ÷
	ret
temp_ax dw 0
isec_ls dw 0;µ±Ç°ÉÈÇø£¨ÓÃÓÚls£©	
nsec_ls dw 0;Ê£ÓàÉÈÇøÊı£¨ÓÃÓÚls£©
isec dw 0	; µ±Ç°ÉÈÇøºÅ
nsec dw 0	; Ê£ÓàÉÈÇøÊı
lns dw 0	; ¶¨ÒåĞĞÊı£¬³õÖµÎª0
FileNum dw 0	; ÎÄ¼ş¸öÊı£¬³õÖµÎª0
FileSize dd 0   ; ÎÄ¼ş×Ü´óĞ¡£¬³õÖµÎª0
secspt dw 0	; Ã¿´ÅµÀÉÈÇøÊı
heads dw 0	; ´ÅÍ·Êı

fsbuf db '0,987,654,321' ; ÎÄ¼ş´óĞ¡´®
fsbuflen equ $ - fsbuf ; ´®³¤
dsbuf db  '            <DIR>          ' ; ×ÓÄ¿Â¼±êÊ¶´®
;dsbuflen equ $ - dsbuf ; ´®³¤
volbuf db '            <VOL>          ' ; ¾í±ê±êÊ¶´®
;volbuflen equ $ - volbuf
btbuf db 'Byte' ; ×Ö½Ú×Ö·û´®
btbuflen equ $ - btbuf ; ´®³¤
fileNumberBuf1 db '  ALL'
fileNumberBufLen1 equ $-fileNumberBuf1
fileNumberBuf2 db 'files.'
fileNumberBufLen2 equ $-fileNumberBuf2
;--------------------------------------------------------------------
getsizestr: ; »ñÈ¡ÎÄ¼ş´óĞ¡Ê®½øÖÆ´®
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äfsbuf
	push di			; ±£´æDIµ½Õ»
	mov cx, fsbuflen; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøfsbufµÄ³¤¶È
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, fsbuf	; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	pop di			; ´ÓÕ»»Ö¸´DI

	; ¼ÆËãÎÄ¼ş´óĞ¡Ê®½øÖÆ´®
	mov cx, 0		; µ±Ç°·Ö¶ÎÊı×Ö¸öÊı£¨³õÊ¼»¯Îª0£©
	mov bp, fsbuf	; BP = fsbuf + fsbuflen - 1 = fsbufµÄµ±Ç°Î»ÖÃ
	add bp, fsbuflen - 1 ; BP = ´®Î²
	mov ebx,10		; ³ıÊı=10
	mov eax, [di + 1Ch]; EAX = ÎÄ¼ş´óĞ¡
	
.1: ; Ñ­»·¿ªÊ¼´¦
	mov edx, 0		; EDX = 0
	div ebx			; EDX:EAX / EBX -> ÉÌEAX¡¢ÓàEDX
	add dl, 30h		; ÓàÊı + 30h = ¶ÔÓ¦µÄÊı×Ö·ûASCIIÂë
	mov [bp], dl	; fsbuf[BP] = DL
	cmp eax, 0		; ÉÌEAX = 0 ?
	je .2			; = 0 Ìø³öÑ­»·
	dec bp			; Êı×Ö·ûµÄµ±Ç°Î»ÖÃBP--
	inc cx			; µ±Ç°·Ö¶ÎÊı×Ö¸öÊı++
	cmp cx, 3		; CX == 3 £¿
	jne .1			; ¡Ù ¼ÌĞøÑ­»·
	; Ìí¼Ó¶ººÅ·Ö¸ô·û
	mov byte [bp], ',' ; ²åÈë¶ººÅ·Ö¸ô·û¡°,¡±
	dec bp			; Êı×Ö·ûµÄµ±Ç°Î»ÖÃBP--
	mov cx, 0		; ÖØĞÂÖÃCX=0
	jmp .1			; ¼ÌĞøÑ­»·
.2: ; ÍË³öÑ­»·
	ret				; ´ÓÀı³Ì·µ»Ø

;--------------------------------------------------------------------
waitforkey: ; °´ÈÎÒâ¼ü¼ÌĞø
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; ÏÔÊ¾ÌáÊ¾´®
	mov ah, 13h 	; BIOSÖĞ¶ÏµÄ¹¦ÄÜºÅ£¨ÏÔÊ¾×Ö·û´®£©
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bh, 0 		; Ò³ºÅ=0
	mov bl, 0fh 	; ×Ö·ûÑÕÉ«=²»ÉÁ£¨0£©ºÚµ×£¨000£©ÁÁ°××Ö£¨1111£©
	mov bp, pkinstr	; BP=´®µØÖ·
	mov cx, pkinstrlen; CX=´®³¤
	mov dl, 0		; ÁĞºÅ=0
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; µÈ´ıÓÃ»§°´¼ü
	mov ah, 0		; ¹¦ÄÜºÅ£¨½ÓÊÜ¼üÅÌ×Ö·ûÊäÈë£©
	int 16h			; µ÷ÓÃ16h¼üÅÌÖĞ¶Ï
	
	call newline	; »Ø³µ»»ĞĞ
	ret				; ´ÓÀı³Ì·µ»Ø

pkinstr db 'Press any key to continue!' ; ÌáÊ¾ÓÃ»§¼üÈëµÄ´®
pkinstrlen equ $ - pkinstr ; ´®³¤

;--------------------------------------------------------------------
waitforkey_chin: ; °´ÈÎÒâ¼ü¼ÌĞø
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	inc dh
	; ÏÔÊ¾ÌáÊ¾´®
	mov ah, 42h 	; BIOSÖĞ¶ÏµÄ¹¦ÄÜºÅ£¨ÏÔÊ¾×Ö·û´®£©
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bh, 0 		; Ò³ºÅ=0
	mov bl, 0fh 	; ×Ö·ûÑÕÉ«=²»ÉÁ£¨0£©ºÚµ×£¨000£©ÁÁ°××Ö£¨1111£©
	mov bp, pkinstr_chin	; BP=´®µØÖ·
	mov cx, pkinstrlen_chin; CX=´®³¤
	mov dl, 0		; ÁĞºÅ=0
	int 21h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	; µÈ´ıÓÃ»§°´¼ü
	mov ah, 0		; ¹¦ÄÜºÅ£¨½ÓÊÜ¼üÅÌ×Ö·ûÊäÈë£©
	int 16h			; µ÷ÓÃ16h¼üÅÌÖĞ¶Ï
	
	call newline	; »Ø³µ»»ĞĞ
	ret				; ´ÓÀı³Ì·µ»Ø

pkinstr_chin db '°´ÈÎÒâ¼ü¼ÌĞø'
pkinstrlen_chin equ ($ - pkinstr_chin)/2 ; ´®³¤
;--------------------------------------------------------------------
;----------------------------------------------------------------------------
; º¯ÊıÃû£ºReadSector
;----------------------------------------------------------------------------
; ×÷ÓÃ£º´ÓµÚ AX¸öÉÈÇø¿ªÊ¼£¬½«CL¸öÉÈÇø¶ÁÈëES:BXÖĞ
ReadSector:
	; -----------------------------------------------------------------------
	; ÔõÑùÓÉÉÈÇøºÅÇóÉÈÇøÔÚ´ÅÅÌÖĞµÄÎ»ÖÃ (ÉÈÇøºÅ->ÖùÃæºÅ¡¢ÆğÊ¼ÉÈÇø¡¢´ÅÍ·ºÅ)
	; -----------------------------------------------------------------------
	; ÉèÉÈÇøºÅÎª x
	;                           ©° ÖùÃæºÅ = y >> 1
	;       x           ©° ÉÌ y ©È
	;   -------------- 	=> ©È      ©¸ ´ÅÍ·ºÅ = y & 1
	;  Ã¿´ÅµÀÉÈÇøÊı     ©¦
	;                   ©¸ Óà z => ÆğÊ¼ÉÈÇøºÅ = z + 1
	push bp
	mov	bp, sp
	sub	sp, 2 		; ±Ù³öÁ½¸ö×Ö½ÚµÄ¶ÑÕ»ÇøÓò±£´æÒª¶ÁµÄÉÈÇøÊı: byte [bp-2]
	mov	byte [bp-2], cl
	push bx			; ±£´æBX
	mov	bl, [BPB_SecPerTrk]	; BLÎª³ıÊı
	div	bl			; AX/BL£¬ÉÌyÔÚALÖĞ¡¢ÓàÊızÔÚAHÖĞ
	inc	ah			; z ++£¨Òò´ÅÅÌµÄÆğÊ¼ÉÈÇøºÅÎª1£©
	mov	cl, ah		; CL <- ÆğÊ¼ÉÈÇøºÅ
	mov	dh, al		; DH <- y
	shr	al, 1			; y >> 1 £¨µÈ¼ÛÓÚy/BPB_NumHeads£¬ÈíÅÌÓĞ2¸ö´ÅÍ·£©
	mov	ch, al		; CH <- ÖùÃæºÅ
	and	dh, 1		; DH & 1 = ´ÅÍ·ºÅ
	pop	bx			; »Ö¸´BX
	; ÖÁ´Ë£¬"ÖùÃæºÅ¡¢ÆğÊ¼ÉÈÇø¡¢´ÅÍ·ºÅ"ÒÑÈ«²¿µÃµ½
	mov	dl, 0; Çı¶¯Æ÷ºÅ£¨0±íÊ¾ÈíÅÌA£©
.GoOnReading:
	mov	ah, 2			; ¶ÁÉÈÇø
	mov	al, byte [bp-2]	; ¶ÁAL¸öÉÈÇø
	int	13h			; ´ÅÅÌÖĞ¶Ï
	jc	.GoOnReading; Èç¹û¶ÁÈ¡´íÎó£¬CF»á±»ÖÃÎª1£¬
					; ÕâÊ±¾Í²»Í£µØ¶Á£¬Ö±µ½ÕıÈ·ÎªÖ¹
	add	sp, 2			; Õ»Ö¸Õë+2
	pop	bp

	ret
;----------------------------------------------------------------------------
; ´óĞÍ¸¨ÖúÀı³Ì½áÊø
; -------------------------------------------------------------------
getstrln0: ; »ñÈ¡¼üÅÌÊäÈëµÄÃüÁî´®ĞĞ
	cld				; Çå³ı·½Ïò±êÖ¾Î»£¨Ê¹É¨Ãè×Ö·û´®·½ÏòÎª´Ó´®Ê×µ½´®Î²£©
	
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äbuf
	mov cx, buflen	; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøbufµÄ³¤¶È£¨buflen=80£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, buf		; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äfnbufµÄÇ°8¸ö×Ö½Ú
	mov cx, cslen	; Ñ­»·´ÎÊıCX=ÃüÁî´®×î´óµÄ³¤¶È£¨cslen=8£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, fnbuf	; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	mov si, 0		; µ±Ç°×Ö·ûÆ«ÒÆÎ»ÖÃ SI = 0
keyin0: ; ½ÓÊÜ¼üÅÌÊäÈë
	; ¶Á°´¼ü£¨·µ»ØµÄ°´¼üASCIIÂëÔÚALÖĞ£©
	mov ah, 0 		; ¹¦ÄÜºÅ
	int 16h 		; µ÷ÓÃ16HºÅÖĞ¶Ï
	; ¶Ô»Ø³µ·û£¨0DH£©½áÊøÊäÈë
	cmp al, 0dh 	; ±È½ÏALÖĞµÄ¼üÈë×Ö·ûÓë»Ø³µ·û£¨ASCIIÂëÎª0DH£©
	je return0 		; ÏàµÈÌø×ªµ½´ÓÀı³Ì·µ»Ø
	cmp al, 08h
	je backspace0
	; ±£´æ°´¼ü×Ö·ûµ½buf
	mov [buf + si], al; buf[SI]=AL
	inc si			; SI++
	; Ì«³¤Ê±Ìø³ö
	cmp si, 21	; SI >= 80 ?
	jae goout0		; >= Ê±Ìø×ª
	jmp next_k0
	
backspace0:
	cmp si,0        ;Ã»ÓĞÊäÈëµÄ×Ö·ûÌø×ª¼ÌĞøÊäÈë
	je keyin0
	
	dec si
	mov byte [buf + si], 20h; ÌîÈë¿Õ¸ñ
	
	; ÏÔÊ¾×Ö·û´®Àı³Ì£¨ĞèÏÈÖÃ´®³¤CXºÍ´®µØÖ·BP£©
	; »ñÈ¡µ±Ç°¹â±êÎ»ÖÃ£¨·µ»ØµÄĞĞÁĞºÅ·Ö±ğÔÚDHºÍDLÖĞ£©
	pusha
	mov cx,1       ; ´®³¤1
	mov bp,blank   ; ´®µØÖ·
	push cx			; ±£»¤CX£¨½øÕ»£©
	mov ah, 3		; ¹¦ÄÜºÅ
	mov bh, 0		; µÚ0Ò³
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	pop cx			; »Ö¸´CX£¨³öÕ»£©
	;10	2	ÖÃ¹â±êÎ»ÖÃ	BH=Ò³ºÅ
    ;DH,DL=ĞĞ,ÁĞ
	
	dec dl          ; ÍË¸ñ
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	;dec dl          ; ÔÙÍËÒ»¸ñ
	; ÔÚµ±Ç°Î»ÖÃÏÔÊ¾×Ö·û´®£¨´®³¤CXºÍ´®µØÖ·BPÒÑÔ¤ÏÈÉèÖÃºÃÁË£©
	mov ah, 13h		; BIOSÖĞ¶ÏµÄ¹¦ÄÜºÅ£¨ÏÔÊ¾×Ö·û´®£©
	mov al, 1 		; ¹â±ê·Åµ½´®Î²
	mov bh, 0 		; Ò³ºÅ=0
	mov bl, 0fh		; ×Ö·ûÑÕÉ«=²»ÉÁ£¨0£©ºÚµ×£¨000£©ÁÁ°××Ö£¨1111£©
	int 10h 		; µ÷ÓÃ10HºÅÏÔÊ¾ÖĞ¶Ï
	
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx	
	popa
	jmp keyin0
	; ÏÔÊ¾ALÖĞµÄ¼üÈë×Ö·û
return0:
	ret 			; ´ÓÀı³Ì·µ»Ø
	
next_k0
	pusha
	mov al,'*'
	mov ah, 0eh 	; ¹¦ÄÜºÅ
	mov bh,0
	mov bl, 0fh 	; ÁÁ°××Ö
	int 10h 		; µ÷ÓÃ10HºÅÖĞ¶Ï
	popa
	jmp keyin0		; Ñ­»·¶Á´æÏÔ°´¼ü
	
goout0:
	
	mov dh,16
	mov dl,17
	mov ah,2
	mov bh,0
	int 10h
	
	mov bl,0fh
	mov bp,longPass
	mov cx,longPasslen
	call DispStr_Chinese
	
	add sp,2
	jmp again_pass
;
SignIn:
	call cls
	call drawSignUp
	
	mov dh,0
	mov dl,0
	mov ah,2
	mov bh,0
	int 10h

	mov bl,0fh
	mov bp,OSver_str1
	mov cx,OSver_str1_len
	call DispStr_Chinese         ;ÏÔÊ¾´óÅÚ
	
	
	mov dh,25
	mov dl,2
	mov ah,2
	mov bh,0
	int 10h

	mov bl,0fh
	mov bp,OSver_str1
	mov cx,OSver_str1_len
	call DispStr_Chinese         ;ÏÔÊ¾´óÅÚ
	
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	mov bh,0
	inc dl
	int 10h
	
	mov bp,OSver_str2
	mov cx,OSver_str2_len
	call DispStr 				 ;ÏÔÊ¾OSĞÅÏ¢
	
	mov dh,13
	mov dl,9
	mov ah,2
	mov bh,0
	int 10h
	mov bp,UserNameBuf
	mov cx,[UserNameBufLen]
	call DispStr

	mov dh,16
	mov dl,5
	mov ah,2
	mov bh,0
	int 10h
	mov bp,keyinPass
	mov cx,keyinPasslen
	call DispStr_Chinese          ;ÏÔÊ¾ÇëÊäÈëÃÜÂë
	xor ah,ah
	int 16h
again_pass	
	mov dh,16
	mov dl,10
	mov ah,2
	mov bh,0
	int 10h
	mov bp,spaceStr
	mov cx,21
	call DispStr
	mov dh,16
	mov dl,10
	mov ah,2
	mov bh,0
	int 10h
	call getstrln0
	call CopyPassword
	;Ğ£ÑéÃÜÂë
	mov si,passwordBuf
	mov di,passwordStr
	mov	cx, 16			; ³õÊ¼Ñ­»·´ÎÊıÎª4 pinÂë
	repe cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jnz .wrong
	jmp .out
.wrong
	mov dh,16
	mov dl,17
	mov ah,2
	mov bh,0
	int 10h
	mov bl,0fh
	mov bp,wrongPass
	mov cx,wrongPasslen
	call DispStr_Chinese
	jmp again_pass
.out:	
	mov dh,16
	mov dl,17
	mov ah,2
	mov bh,0
	int 10h
	mov bl,0fh
	mov bp,welcomePass
	mov cx,welcomePasslen
	call DispStr_Chinese
	call cls
	ret
keyinPass db 'ÇëÊäÈëÃÜÂë'
keyinPasslen equ ($-keyinPass)/2

longPass db 'ÃÜÂëÌ«³¤'
longPasslen equ ($-longPass)/2
welcomePass db '»¶Ó­Ê¹ÓÃ'
welcomePasslen equ ($-welcomePass)/2	
wrongPass db 'ÃÜÂë´íÎó'
wrongPasslen equ ($-wrongPass)/2
spaceStr:    db '                        '
passwordBuf: db '                        '
passwordStr: db '1997                    '
; ------------------------------------------------------------------
CopyPassword:	    ; ¹¹ÔìĞÂ´®£¨ÃüÁî´® --> ÃÜÂë£©
	mov si, buf		; Ô´´®ÆğÊ¼µØÖ·
	mov di, passwordBuf	; Ä¿µÄ´®ÆğÊ¼µØÖ·
	mov cx, 21		; Ñ­»·´ÎÊı CX = n
	; ½«ÊäÈë»º³åÇøbufÖĞµÄÃüÁî´®¸´ÖÆµ½ÎÄ¼şÃû»º³åÇøfnbuf£º
	rep movsb		; CX > 0Ê± [ES:DI] = [DS:SI]¡¢CX--£¬CX = 0Ê±ÍË³öÑ­»·
	
	ret 			; ´ÓÀı³Ì·µ»Ø
	
passwordStr_temp1 db '                        '
passwordStr_temp2 db '                        '
; ===============================================================================
;-------------------------------------------------------------------------------
; ¸Ä±äÓÃ»§ÃÜÂë
;ÌáÊ¾´®
PleaseEnterPassStr db 'ÇëÊäÈëÃÜÂë'
PleaseEnterPassStrLen equ ($-PleaseEnterPassStr)/2
PleaseEnterPassStr0 db 'ÔÙ´ÎÊäÈëÃÜÂëÒÔÈ·ÈÏ'
PleaseEnterPassStrLen0 equ ($-PleaseEnterPassStr0)/2
PleaseEnterPassStr1 db 'Á½´ÎÃÜÂë²»Ò»ÖÂ£¬ÇëÖØĞÂÊäÈë'
PleaseEnterPassStrLen1 equ ($-PleaseEnterPassStr1)/2
ChangePassword:
;¿ÉÄÜ±ä¶¯µÄÊı¾İ
;passwordStr: db '1997                    '
;UserNameBuf db 'Liao Weiming                         '
;UserNameBufLen dw 13
again_setpass
	mov di,passwordStr_temp1
	mov cx,16
	mov al,20h
	rep lodsb 
	mov di,passwordStr_temp2
	mov cx,16
	mov al,20h
	rep lodsb 
	mov bp,PleaseEnterPassStr
	mov cx,PleaseEnterPassStrLen
	call DispStr_Chinese          ;ÏÔÊ¾ÇëÊäÈëÃÜÂë1
	call newline
	;»ñÈ¡ÃÜÂë1
	call getstrln0
	call CopyPassword
	;¸´ÖÆµ½passwordStr_temp1
	mov si,passwordBuf
	mov di,passwordStr_temp1
	mov cx,16
	rep movsb
	
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	mov bh,0
	inc dh
	mov dl,0
	int 10h
	
	mov bp,PleaseEnterPassStr0
	mov cx,PleaseEnterPassStrLen0
	call DispStr_Chinese          ;ÏÔÊ¾ÇëÊäÈëÃÜÂë2
	call newline
	;»ñÈ¡ÃÜÂë2
	call getstrln0
	call CopyPassword
	;¸´ÖÆµ½passwordStr_temp2
	mov si,passwordBuf
	mov di,passwordStr_temp2
	mov cx,16
	rep movsb
	
	
	;Ğ£ÑéÃÜÂë
	mov si,passwordStr_temp1
	mov di,passwordStr_temp2
	mov	cx, 16			; ³õÊ¼Ñ­»·´ÎÊıÎª4 pinÂë
	rep cmpsb			; ÖØ¸´±È½Ï×Ö·û´®ÖĞµÄ×Ö·û£¬CX--£¬Ö±µ½²»ÏàµÈ»òCX=0
	cmp	cx, 0
	jnz .wrong
	jmp .out
.wrong
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	mov bh,0
	inc dh
	mov dl,0
	int 10h
	mov bp,PleaseEnterPassStr1
	mov cx,PleaseEnterPassStrLen1
	call DispStr_Chinese          ;ÏÔÊ¾´íÎóĞÅÏ¢
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	mov bh,0
	inc dh
	mov dl,0
	int 10h
	jmp again_setpass
.out
	mov si,passwordStr_temp1
	mov di,passwordStr
	mov	cx, 16			; ÉèÖÃĞÂÃÜÂë
	rep movsb
	
	add sp,2
	call SignIn
	jmp again
;-------------------------------------------------------------------------------
; ¸Ä±äÓÃ»§Ãû
;ÌáÊ¾´®
PleaseEnterUserNameStr db 'ÇëÊäÈëĞÂµÄÓÃ»§Ãû'
PleaseEnterUserNameStrLen equ ($-PleaseEnterUserNameStr)/2
ChangeUserName:
	pusha
	; ÓÃ¿Õ¸ñ·û£¨20h£©Ìî³äUserNameBuf
	mov cx, 16	; Ñ­»·´ÎÊıCX=ÃüÁîĞĞ»º³åÇøbufµÄ³¤¶È£¨buflen=80£©
	mov al, 20h		; AL=ÒªÌî³äµÄ¿Õ¸ñ·ûASCIIÂë
	mov di, UserNameBuf		; ES:DI=×Ö·û´®µÄÆğÊ¼µØÖ·
	rep stosb		; CX>0Ê±½«AL´æ´¢µ½[ES:DI]£¬CX--¡¢DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,5   ;Ìø¹ıcuser  Îå¸ö×Ö·û
	;cmp byte[bp],' '
	;jz ChangeUserEnd
	

	mov di,UserNameBuf
	cld
	mov cx,16
	mov si,bp
	rep movsb
	stosb
ChangeUserEnd:
	popa
	add sp,2
	jmp again
;--------------------------------------------------------------------

; ===================================================================
;Í¼ĞÎ¸¨Öúº¯Êı
a_x dw 0
a_y dw 0
b_x dw 0
b_y dw 0
c_x dw 0
c_y dw 0
OSver_str1 db '´óÅÚ'
OSver_str1_len equ ($ - OSver_str1)/2
OSver_str2 db 'OS 12.1'
OSver_str2_len equ $ - OSver_str2
UserNameBuf db 'Liao Weiming                         '
UserNameBufLen dw 16

drawSignUp:
	mov bx,0
	mov dx,20*32-1
	mov ax,17
	mov cx,0
	call drawLine

	call _dc      ;ÏÔÊ¾ÊµÊ±Ê±ÖÓ
	mov word[a_x],2*32
	mov word[a_y],(15-9)*32
	mov word[b_x],8*32
	mov word[b_y],(15-9)*32
	mov word[c_x],2*32
	mov word[c_y],(15-6)*32
	call drawRectangle
	
	
	mov word[a_x],2*32+8
	mov word[a_y],(15-7)*32-8
	mov word[b_x],8*32-8
	mov word[b_y],(15-7)*32-8
	mov word[c_x],2*32+8
	mov word[c_y],(15-6)*32-8
	call drawRectangle
	
    ret
drawRectangle: ;»­¾ØĞÎ ÏÈºópushÈı¸ö½Ç
	mov bx,[a_x]
	mov dx,[b_x]
	mov ax,[a_y]
	mov cx,0
	call drawLine
	
	mov bx,[a_x]
	mov dx,[b_x]
	mov ax,[c_y]
	mov cx,0
	call drawLine
	
	mov dx,[c_y]
	mov bx,[a_y]
	mov ax,[a_x]
	mov cx,1
	call drawLine
	
	mov dx,[c_y]
	mov bx,[a_y]
	mov ax,[b_x]
	mov cx,1
	call drawLine
	ret
drawLine:      ;ÆğÊ¼×ø±ê BX  ½áÊø×ø±ê DX  ²»±ä×ø±ê AX  CX»®Ïß·½Ïò 0Ë®Æ½ 1ÊúÖ±
	pusha
	cmp cx,0
	jz drawRow
drawCol:
	sub dx,bx
	mov cx,dx
.1:
	push cx
	mov cx,ax
	mov dx,bx
	call drawPixel
	inc bx
	pop cx
	loop .1
	jmp out__
drawRow:
	sub dx,bx
	mov cx,dx
.2:
	push cx
	mov dx,ax
	mov cx,bx
	call drawPixel
	inc bx
	pop cx
	loop .2
out__:
	popa
	ret
	
drawPixel:     ;X=CX   Y=DX
	pusha
	mov ax,0c0fh
	mov bh,0
	int 10h
	popa
	ret
; ===================================================================