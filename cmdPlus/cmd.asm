org 100h 		; �ɱ����COM�ļ�
; ===================================================================
; ������������ʼ
;--------------------------------------------------------------------	
	; ͨ��AX��ת����CS��ֵ����DS��ES��SS
	mov ax, cs
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 100h - 4	; ��ջ��ָ��SP=100h-4
	mov ax,12h    ;640*480 mode
	int 10h       ;����640*480/16ɫ��ʾģʽ
	; ��ʼ���ڲ�����������ڵ�ַ
	mov word [cmdaddr], ver		; VER ��ʾ��Ȩ��Ϣ
	mov word [cmdaddr + 2], cls	; CLS ����
	mov word [cmdaddr + 4], toa	; A:  �л���A��
	mov word [cmdaddr + 6], tob	; B:  �л���B��
	mov word [cmdaddr + 8], toc	; C:  �л���C��
	mov word [cmdaddr + 10], dir; DIR ��ʾ�ļ�Ŀ¼�б�
	mov word [cmdaddr + 12], ls; LS  ��ʾ�ļ�Ŀ¼�б�
	mov word [cmdaddr + 14], help; HELP ��ʾ����
	mov word [cmdaddr + 16],cdToDir ;Ŀ¼��ת
	mov word [cmdaddr + 18], _dt; dt ��ʾʱ��
	mov word [cmdaddr + 20], _dc; dc ��ʾʱ��	
	mov word [cmdaddr + 22], rename
	mov word [cmdaddr + 24], mkdir
	mov word [cmdaddr + 26], ReadMemmory
	mov word [cmdaddr + 28], HZK16_test
	mov word [cmdaddr + 30], SignIn
	mov word [cmdaddr + 32], Restart
	mov word [cmdaddr + 34], ChangePassword
	mov word [cmdaddr + 36], ChangeUserName
	; �����ж�������21h��
	xor ax, ax		; AX = 0
	mov fs, ax		; FS = 0
	mov word[fs:21h*4], int21h ; ����21h���ж�������ƫ�Ƶ�ַ
	mov ax,cs 
	mov [fs:21h*4+2], ax ; ����21h���ж������Ķε�ַ=CS
	call Store_dc
	call getdiskparam	; ��ȡ���̲���H&S������ReadSec��ls���̣�
	call cls		; ����
	call initialDisk ;��ʼ��ls�õ���������Ϣ
	call int213dh
	call cls
	call ver		; ��ʾ��Ȩ��Ϣ
	;call waitforkey_chin
	call cls		; ����
	call SignIn
	call cls		; ����
	;call ver		; ��ʾ��Ȩ��Ϣ
again: ; ������ѭ��
	call BackToCmd
	;call ver0		; ��ʾ��Ȩ��Ϣ
	call prompt		; ��ʾ��ʾ��
	call getstrln	; ��ȡ��������������
	call dtlen		; ȷ���������
	call tocap		; ת���ɴ�д��ĸ
	call newstr		; �����´�
	call Shut_dc
	call iscmd		; �ж��Ƿ�Ϊ�ڲ��������ǣ���ִ��֮������
	call newline	; �س�����
	call exec		; ִ���ⲿ���COM�ļ���
	jmp again		; ����ѭ��
	
;--------------------------------------------------------------------
; ������������顢���������ַ���

drvno db 0 ; �����������ţ�0=����A��1=����B��80h=Ӳ��C
i dw 0 ; ѭ������
n dw 0 ; �������

N equ 19	; �ڲ���������
cslen equ 8 ; �����󳤶�

cmdstr: ; �ڲ�������飨ͳһ����Ϊ8�����㲹�ո����
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
cmdHelpStr:  ;�ڲ��������ͳһ����30
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
cmdHelpStr_chin:  ;�ڲ��������ͳһ����30
	db '��ʾϵͳ�汾                  '
	db '����                          '
	db '�ı�Ŀ¼��ϵͳ�̸�Ŀ¼        '
	db '�ı�Ŀ¼���������̸�Ŀ¼      '
	db '�ı�Ŀ¼��Ӳ�̸�Ŀ¼          '
	db '��ʾ��ǰĿ¼                  '
	db '��ʾ���е�ǰĿ¼�ļ���Ŀ      '
	db '��ʾ������Ϣ                  '
	db '�ı䵱ǰĿ¼                  '
	db '��ʾ��ǰʱ��                  '
	db 'ʵʱʱ��                      '
	db '�������ļ�                    '
	db '�����ļ���                    '
	db '��ȡ�ڴ�                      '
	db '������ʾ                      '
	db '����                          '
	db '����                          '
	db '�޸�����                      '
	db '�޸��û���                    '
cmdaddr: ; �ڲ�����������ڵ�ַ����
	resw N

fnbuf: ; COM�ļ�������8+3=11�ַ���
	db '12345678COM'

Dirbuf: ; Ŀ¼������8+3=11�ַ���
	db '           '
buflen: equ 80 ; ����������=80

buf: resb buflen ; �����л�����
 
str1: ; �ַ���1����Ȩ��Ϣ����
	db 'BigBoom-OS 2.0  (C) 2016 Big Firecrackers'
str1len equ $ - str1 ; ��Ȩ����

str2: ; �ַ���2���飨��������ʾ����
	db 'A:/$'
	resb 80   ;��Ŀ¼������
str2len: dw 4 ; ��ʾ����

str3: ; �ַ���3��������Ϣ����
	db 'Wrong command!'
str3len equ $ - str3 ; ���������

str3_chin: ; �ַ���3��������Ϣ����
	db '�����ڲ����ⲿ���Ҳ���ǿ����еĳ���'
str3len_chin equ ($ - str3_chin)/2 ; ���������

str4: ; �ַ���4����̫����Ϣ����
	db 'Too long!'
str4len equ $ - str4 ; ̫������
str4_chin: ; �ַ���3��������Ϣ����
	db '����̫��'
str4len_chin equ ($ - str4_chin)/2 ; ���������

str10: ; �ַ���5��������Ϣ����
	db 'No such file or directory!'
str10len equ $ - str10 ; ���������
str10_chin: ; �ַ���3��������Ϣ����
	db 'û���Ǹ��ļ���Ŀ¼'
str10len_chin equ ($ - str10_chin)/2 ; ���������

str11_chin: ; �ַ���4��������Ϣ����
	db '�ļ����Ѵ���'
str11len_chin equ ($ - str11_chin)/2 ; �ظ���Ŀ
; -------------------------------------------------------------------
; ���������������
; ===================================================================
; С�͸������̿�ʼ
; ����=================================================================================================================
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



chi1 db '������ɫ'
chi2 db '������ɫ'
chi3 db '���ǻ�ɫ'
chi4 db '���Ǻ�ɫ'
chi5 db '���Ƿ�ɫ'	;0dh	
chi6 db '���ǻ�ɫ'    ;0eh
chi7 db '������ɫ'
chi8 db '���ǰ�ɫ'
chin_len EQU ($-chi8)/2


huanchong db 0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h

ba db 0fh
color_char1 resb 1  ;��ʾ��ɫ
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
	
	; mov dh,29	;��
	; mov dl, 30 		; ��10��
	; call displayhc
	
	;call displayhc
	
	
	mov bx,7  ;��
	mov dl,20 		; ��0��
	call displayhead
	
	mov bx,11	;��
	mov dl, 60 		; ��40��
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
	mov dh,23	;��
	mov dl, 30 		; ��10��
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
	mov bp, si 	; BP=����ַ
	mov cx, 17	; ����
	call display
	popa 
	ret	
	
	
displaysqu:
	pusha
	mov si,squ
	mov cx,10
.1:
	mov bp, si 	; BP=����ַ
	mov dh,bl
	push cx
	mov cx, 8	; ����
	
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
	mov bp, si 	; BP=����ַ
	mov dh,bl
	push cx
	mov cx, 37	; ����
	
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
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, [color_char1] 	; ����
	mov bh, 0 		; ��0ҳ
	
	;mov dl, 0 		; ��0��
	;mov bp, head1 	; BP=����ַ
	;mov cx, 37	; ����
	int 10h 		; ����10H����ʾ�ж�
	popa
	ret 
display1:	
	pusha
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	
	;mov dl, 0 		; ��0��
	;mov bp, head1 	; BP=����ַ
	;mov cx, 37	; ����
	int 10h 		; ����10H����ʾ�ж�
	popa
	ret 
; =======================================================================��������==============================================================================
; ============================AH=3Dh==========================================
cc dw 0
; ============================AH=3Dh=======================================END
int21h: ; int 21h�жϴ�������
	cmp ah,4ch
	jnz .1
; ============================AH=4ch==========================================
	mov al, 20h		; AL = EOI
	out 20h, al		; ����EOI����8529A
	out 0A0h, al	; ����EOI����8529A
	
	; ��ʼ���μĴ�����ջָ��
	mov ax, cs 		; ͨ��AX��ת,  ��CS��ֵ���͸�DS��ES��SS
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 100h - 4; ��ջ��ָ��SP=100h-4
	
	mov ah,0fh      ;��ȡ��ʾģʽ
	cmp al,12h      ;�Ƿ���600*480 ͼ��ģʽ��
	jz .out
	;mov ax,12h      ;����ͼ��ģʽ
	;mov bh,0
	;mov bl,0fh
	;int 10h
	;mov ax,0bh     	;���ñ�����ɫ
	;mov bh,0
	;mov bl,00h		;��ɫ����
	;int 10h
.out
	jmp again		; ���¿�ʼ������ѭ��
; ============================AH=4ch=======================================END
.1:
	cmp ah,3Dh
	jnz AH_3FH
; ============================AH=3Dh==========================================
	; ��HZK16�ֿ��ļ���������ص�58000hλ�ã�������ʾ����
    ; ������A�̸�Ŀ¼��Ѱ�� *.BIN
	iret
; ============================AH=3Dh=======================================END
AH_3FH:
	cmp ah,3fh
	jnz AH_42H
; ============================AH=3Fh==========================================
	;���ֿ��ļ�  Ĭ��32�ֽ�
	;��ڣ� CX=��ȡ�ֽ���   DS��DX=���ݻ�������ַ   ���ڣ���
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
	;ԭdos�ж�;�ƶ��Ѷ�ȡ�ļ���ָ��  
	;;��ڣ� CX��DXλ����
	;��cmd�ж�;INT21H AH=42h ��ָ��λ����ʾ���� ����ַ=BP ����=CX  �к�DH �к�DL
	pusha
	push es
	push ds       ;����ԭ�Ĵ�����Ϣ��������es ds�ĵ�ַ
	mov ax,1000h
	mov es,ax
	mov ds,ax
	
	mov [color_char],bl   ;������ɫ
	;mov es,ax    ?�ָ�es ds Ϊcmd�Ķε�ַ 
	;mov ds,ax
	xor ax,ax     ;���ص�*16=���к�
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
	shl cx,1      ;CX*2==�ַ����ֽ���
	
	;ds=1000h di=disp_data
	mov di,disp_data
	mov si,bp
	;es=���ó���� SI=BP����
	pop ds        ;�����������ڶ�
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
	mov ax,1000h  ;�ٻָ�es
	mov ds,ax    
	call HZK16_test
	pop ds
	pop es
	popa
	
	iret
DispStr_Chinese:;�ڵ�ǰλ����ʾ���� ����ַ=BP ����=CX  
	pusha
	push es
	push ds       ;����ԭ�Ĵ�����Ϣ��������es ds�ĵ�ַ
	mov ax,1000h
	mov es,ax
	mov ds,ax
	xor ax,ax     ;���ص�*16=���к�
	
	push cx
	mov ah,3      ;��ȡ��ǰ���λ��
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
	shl cx,1      ;CX*2==�ַ����ֽ���

	mov di,disp_data
	mov si,bp
	;es=���ó���� SI=BP����
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
getdiskparam: ; ��ȡ���̲���H/S
	call ReadPBootSec		; ���ö�����̷���������������
	mov ax, [Sector + 18h]	; AX = ÿ�ŵ�������
	mov [secspt], ax		; secspt = AX = ÿ�ŵ�������
	mov ax, [Sector + 1Ah]	; AX = ��ͷ��
	mov [heads], ax			; heads = AX = ��ͷ��
	ret						; �����̷���
	
; -------------------------------------------------------------------
newline: ; ���У���ʾ�س����ͻ��з���
	; ��ʾ�س���CR���õ�ǰ�к�=0��
	mov ah, 0Eh 	; ���ܺ�
	mov al, 0Dh 	; ����ALΪ�س���CR��ASCII��Ϊ0DH��
	mov bl, 0fh 	; ������
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ���з�����ǰ�к�++��
	mov ah, 0Eh 	; ���ܺ�
	mov al, 0Ah 	; ����ALΪ���з�LF��ASCII��Ϊ0AH��
	mov bl, 0fh 	; ������
	int 10h 		; ����10H����ʾ�ж�
	ret				; �����̷���

; -------------------------------------------------------------------
space: ; ��ʾ�ո��
	mov ah, 0Eh 	; ���ܺ�
	mov al, 20h 	; ����ALΪ�ո��SP��ASCII��Ϊ20H��
	mov bl, 0fh 	    ; ������
	int 10h 		; ����10H����ʾ�ж�
	ret			; �����̷���
	
; -------------------------------------------------------------------
showwrong: ; ��ʾ������Ϣ
	;call newline 	; �س�����
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ������Ϣ��
	;mov ah, 13h 	; ���ܺ�
	;mov al, 1 		; ���ŵ���β
	;mov bl, 0fh 	; ����
	;mov bh, 0 		; ��0ҳ
	;mov dl, 0 		; ��0��
	;mov bp, str3 	; BP=����ַ
	;mov cx, str3len	; ����
	;int 10h 		; ����10H����ʾ�ж�
	;mov cx,buflen
	mov bp,buf
	push bp    ;����bp
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
	mov cx,si       ;���������
	pop si
	pop bp
	call DispStr
	mov ah, 0Eh 	; ���ܺ�
	mov al, ':' 	; ����ALΪ�ո��SP��ASCII��Ϊ20H��
	mov bl, 0fh 	    ; ������
	int 10h 		; ����10H����ʾ�ж�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	mov ah, 2		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
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
	ret				; �����̷���
;--------------------------------------------------------------------
showError1: ;��ʾ������Ϣ ��ʾ����=cx ,��ʾ��ƫ��=bp
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ������Ϣ��
	
	mov ah, 42h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str10_chin	; BP=����ַ
	mov cx, str10len_chin	; ����
	int 21h 		; ����10H����ʾ�ж�
	ret				; �����̷���
;--------------------------------------------------------------------
showError2: ;��ʾ������Ϣ ��ʾ����=cx ,��ʾ��ƫ��=bp
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ������Ϣ��
	
	mov ah, 42h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str11_chin	; BP=����ַ
	mov cx, str11len_chin	; ����
	int 21h 		; ����10H����ʾ�ж�
	ret				; �����̷���
; -------------------------------------------------------------------
showtoolong: ; ��ʾ̫����Ϣ
	call newline 	; �س�����
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ̫����Ϣ��
	mov ah, 42h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str4_chin	; BP=����ַ
	mov cx, str4len_chin	; ����
	int 21h 		; ����10H����ʾ�ж�
	ret				; �����̷���
Store_dc:
	pusha
	mov bx, 0x70	; BX = 70h���жϺţ�
	shl bx, 2		; BX << 2��BX *= 4�� 
	cli				; �ر��жϣ���ֹ�Ķ��ڼ䷢���µ�0x70���ж�
	; ����70h���жϵ�������
	push es			; ����ES��ջ
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
	mov bx, 0x70	; BX = 70h���жϺţ�
	shl bx, 2		; BX << 2��BX *= 4�� 
	cli				; �ر��жϣ���ֹ�Ķ��ڼ䷢���µ�0x70���ж�
	; ����70h���жϵ�������
	push es			; ����ES��ջ
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
BackToCmd:      ;���ù����ɫ
	pusha 
	;mov ax,0bh  
	;mov bh,00
	;mov bl,0h
	;int 10h
	popa
	call _dc
	ret
;--------------------------------------------------------------------
; С�͸������̽���
; ===================================================================

	
; ===================================================================
; �ڲ��������̿�ʼ
;-------------------------------------------------------------------------------
; �ļ����ַ���
FileName_HZK:		db	"HZK16          " ; �ֿ��ļ���
BaseOfFile_HZK	dw	3000h; �ֿ��ļ������ص���λ�� ----  �ε�ַ
OffsetOfFile_HZK  equ 0h
Current_Base_HZK  dw 0
Current_Offset_HZK  dw 0
Original_Base_HZK dw 3000h
count db 0
BaseOfBuf_HZK		equ 8800h	; ���ڲ����ļ���Ŀ�Ļ����� ---- ����ַ
OffsetOfBuf_HZK	equ	0		; ���ڲ����ļ���Ŀ�Ļ����� ---- ƫ�Ƶ�ַ
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
	push es		; ����ES

; ������λ
	xor	ah, ah	; ���ܺ�ah=0����λ������������
	xor	dl, dl	; dl=0������A������BΪ1��Ӳ�̺�U��Ϊ80h��
	int	13h		; �����ж�
	
; �����ڴ���Ŀ¼��Ѱ�� �ֿ��ļ�
	;�ж��Ǹ�Ŀ¼������Ŀ¼
	push ax
	mov ax,[SectorNoOfCurrentDirectory] 	; ����ʾ��ǰ�����ŵ�
	mov	word [wSectorNo], ax
						; ����wSectorNo����ֵΪ��ǰĿ¼������������
	mov ax, [CurrentDirSectors]	; ʣ��������
	mov word [wRootDirSizeForLoop],ax
										; ��ʼ��Ϊ��ǰĿ¼��ռ����������ѭ���л�ݼ�����
	pop ax
LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK:
	cmp	word [wRootDirSizeForLoop], 0 ; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	LABEL_NOT_FOUND_HZK	; ���������ʾδ�ҵ��ֿ��ļ�
	dec	word [wRootDirSizeForLoop]	; �ݼ�����wRootDirSizeForLoop��ֵ
	; ���ö�������������һ��Ŀ¼������װ����
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call ReadSec		; ���ö���������

	mov	si, FileName_HZK		; DS:SI -> �ֿ��ļ�
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; ���DF��־λ
						; �ñȽ��ַ���ʱ�ķ���Ϊ��/��[��������]
	mov	dx, 10h			; ѭ������=16��ÿ��������16���ļ���Ŀ��512/32=16��
LABEL_SEARCH_FOR_COM_FILE_HZK:
	cmp	dx, 0			; ѭ����������
	jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR_HZK ; ���Ѷ���һ����
	dec	dx				; �ݼ�ѭ������ֵ			  ��������һ����
	mov	cx, 11			; ��ʼѭ������Ϊ11
LABEL_CMP_FILENAME_HZK:
	repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND_HZK ; ����Ƚ���11���ַ�����ȣ���ʾ�ҵ�
LABEL_DIFFERENT_HZK:
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
						; FFE0h = 1111111111100000����5λ=32=Ŀ¼��Ŀ��С��
	add	di, 20h			; DI += 20h ��һ��Ŀ¼��Ŀ
	mov	si, FileName_HZK		; SIָ��װ���ļ���������ʼ��ַ
	jmp	LABEL_SEARCH_FOR_COM_FILE_HZK; ת��ѭ����ʼ��

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR_HZK:             ;ssssss
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	LABEL_NOT_FOUND_HZK ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK		; ��������Ŀ¼ѭ��
.root:
	inc	word [wSectorNo]	; ���ڸ�Ŀ¼��������ǰ������
	
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK

LABEL_NOT_FOUND_HZK:
	pop es			; �ָ�ES
	;call showwrong	; ��ʾ�ַ���
	;jmp $
	ret

; ���潫�ֿ��ļ����ص��ڴ�
LABEL_FILENAME_FOUND_HZK:	; �ҵ� �ֿ��ļ���������������
	; �����ļ�����ʼ������
	mov	ax, [CurrentDirSectors]	; AX=��ǰĿ¼ռ�õ�������
	and	di, 0FFE0h		; DI -> ��ǰ��Ŀ�Ŀ�ʼ��ַ
	add	di, 1Ah			; DI -> �ļ���������������Ŀ�е�ƫ�Ƶ�ַ
	mov cx, word [es:di] ; CX=�ļ�����������
	push cx				; �����������FAT�е����
	add	cx, RootDirSectors			; CX=�ļ��������ʼ������+��Ŀ¼ռ�õ������� +��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������
	;��Ҫ������˵һ���=_=,�����bug���˼�Сʱ   ԭ����add	cx,ax   ������Ŀ¼ax�����Ǹ�Ŀ¼��������
	add	cx, DeltaSectorNo ; CL <- COM�ļ�����ʼ������(0-based)
	mov	ax, [BaseOfFile_HZK]      ;+1C
	mov	es, ax			; ES <- BaseOfLoader��COM�����ַ=4000h��
	mov	bx, OffsetOfFile_HZK ; BX <- OffsetOfLoader��COM����ƫ�Ƶ�ַ=100h��
	mov	ax, cx			; AX <- ��ʼ������
LABEL_GOON_LOADING_FILE_HZK:
	push bx				; �����ֿ����ƫ�Ƶ�ַ
	mov	cl, 1			; 1������
	call ReadSec		; ������

	; �����ļ�����һ������
	pop bx				; ȡ���ֿ����ƫ�Ƶ�ַ
	pop	ax				; ȡ����������FAT�е����
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	cmp	ax, 0FF8h		; �Ƿ����ļ�����
	jae	LABEL_FILE_LOADED_HZK ; ��FF8hʱ��ת���������һ����
	push ax				; ����������FAT�е����
	mov	dx, RootDirSectors	; DX = ��Ŀ¼������
	add	ax, dx			; ������� + ��Ŀ¼������
	add	ax, DeltaSectorNo ; AX = Ҫ��������������ַ
	;add	bx, [BPB_BytsPerSec] ; BX+512ָ���ֿ����һ��������ַ
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

; ������תִ��COM����
LABEL_FILE_LOADED_HZK:
	pop es
	;add sp,2
	;jmp	BaseOfLoader:OffsetOfLoader	; ��һ����ת���Ѽ��ص��ڴ��е�
	ret
;------------------------------------------------------------------------------------------------
;21���ж� ���ܺ�ah=42h
line_char DW 2    ;����Ļ�ϵڼ�����ʾ
col_char DW 2    ;����Ļ�ϵڼ�����ʾ
color_char db 0FH  ;��ʾ��ɫ    LRGB
HZK16_test:
  pusha
  jmp install
  disp_data1 DB  '��ά��'  
  disp_data resb 1024   ;���ֻ����������Ҫ��ʾ�ĺ���
  disp_data_len dw 0   ;�����ַ�����
  ;chars EQU ($-disp_data)/2
  ;DISP_DATA_END EQU THIS BYTE
  zi_buffer resb 1280  ;һ�п���ʾ40������,40*32b=1280byte
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
  
  call get_dots    ;�������ֵ���
  pop cx
  loop ins2
  call disp_cc    ;��ʾ����Ļ
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
  mov bl,8     ;ת��Ϊ��ĸ�к�
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
  sub ax,0a1a1h   ;���ֵ������ A1����ʼ
  cwd  ;ax?��??�dx，ax
  mov dl,al    ;���Ծ��Կ�ʼ��������-A1
  mov al,ah    ;�������ֿ��е�λ��Ϊ 
  cbw
  mov bl,94    ;����������1-A1��* 94 + ������2 - A1��* 32
  mul bl
  add ax,dx
  mov bx,32
  mul bx  ;   dx，ax
  mov cx,dx  ;cx�?
  mov dx,ax  ;dx�?
  ;mov ax,4200h    ;�ƶ���дָ�뵽��������λ��
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
	;mov es,ax    ;ES  DIΪĬ��ֵ
	
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
ReadMemmory_CHINESE:   ;��ʾ CX:DX��16�ֽڵ��ڴ���Ϣ
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
hex2ascii:  ;16����תASCII�����룺AL = BCD�룬�����AX = ASCII��
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
	;------��ȡ����λ---------;
	mov bx,0
	;shld bx,dx,4
	mov bx,dx
	shr bx,4
	and bl,0fh
	mov al,bl
	call ShowChar_HZK
	;------��ȡ����λ---------;
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
Restart: ;��������ϵͳ
	int 19h
	ret
; -------------------------------------------------------------------	
; ��ʾ����ʮ�������ַ�����
ShowChar_HZK: ; ��ʾһ��ʮ���������ַ���0~9��A~F����ALΪ���ݲ�����
	cmp al, 10		; AL < 10 ?
	jl .1			; AL < 10����ת��.1
	add al, 7		; AL >= 10����ʾ��ĸ�� = ��ֵ += 37h��
.1: ; ����
	add al, 30h		; �����ַ� = ��ֵ+=30h
	mov ah, 0Eh		; ���ܺţ��Ե紫��ʽ��ʾ�����ַ���
	mov bl, 0fh 	; ���ı���ʽ��0
	int 10h 		; ����10H���ж�
	ret				; �����̷���
; -------------------------------------------------------------------
;-------------------------------------------------------------------------------
new_int_0x70_dc: ; 70h�����жϴ���������
	; ���潫Ҫʹ�õļĴ������ⱻ�ƻ�
	push ax
	push bx
	push es
	
  wait0: ; �ж��Ƿ�ɶ�������ʱ����Ϣ	
	; �˶δ�����ڸ������ڽ����ж���˵�ǲ���Ҫ��
	mov al, 0x0a	; ָ���Ĵ���A
	or al, 0x80		; ���NMI����Ȼ��ͨ���ǲ���Ҫ��	   
	out 0x70, al	; ���AL���˿�70h��ѡ��Ĵ���A
	in al, 0x71		; ���Ĵ���A
	test al, 0x80	; ���Ե�7λ = 0�� 
	jnz wait0		; �� 0ʱ��������ʱ���ڸ����У���ȴ�

	; ��ȡ��ǰ��ʱ����Ϣ
	; ��ȡ����Ϣ
	xor al, al		; AL = 0
	out 0x70, al	; ָ���洢��Ԫ��ַ
	in al, 0x71		; ��RTC��ǰʱ��(��)
	push ax			; ����ȡ������ALѹջ����
	; ��ȡ����Ϣ
	mov al, 2		; AL = 2
	out 0x70, al	; ָ���洢��Ԫ��ַ
	in al, 0x71		; ��RTC��ǰʱ��(��)
	push ax			; ����ȡ������ALѹջ����
	; ��ȡʱ��Ϣ
	mov al, 4		; AL = 4
	out 0x70, al	; ָ���洢��Ԫ��ַ
	in al, 0x71		; ��RTC��ǰʱ��(ʱ)
	push ax			; ����ȡ������ALѹջ����
	; ��ȡ�Ĵ���C
	mov al, 0x0c	; ָ���Ĵ���C���ҿ���NMI 
	out 0x70, al	; ���AL���˿�70h��ѡ��Ĵ���C
	in al, 0x71		; ��RTC�ļĴ���C������ֻ����һ���ж�
					; �˴����������Ӻ��������жϵ���� 
	
	; ����Ļ���Ͻ���ʾʱ����Ϣ
	; ��ES = �Դ��ַ
	mov ax,0x1000	; AX = B800h����ɫ�ı���Ļ�Դ����ʼ��ַ >> 4��
	mov es,ax		; ES = AX = B800h��ES = �Դ��ַ��
	; ����ʱ�䴮����ʼλ��
	mov bx, (0*80 + 72)*2; ����Ļ�ϵĵ�0��72�п�ʼ��ʾ
	mov ah,3
	mov bh,0
	int 10h
	mov [es:TEMP_DX],dx         ;������λ��
	mov ah,2
	mov dh,0
	mov dl,72    
	mov bh,0
	int 10h  		;���ù��λ��Ϊ0 72
	; ��ʾʱ
	pop ax			; ��ջ�е���ʱ
	call bcd2ascii	; ����BCDתASCII����
	; ��ʾ��λСʱ����
	;mov [es:bx], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 2], al
	call ShowChar_dt
	; ��ʾ�ָ���':'
	mov al,':'
	;mov [es:bx + 4], al
	call ShowChar_dt
	; ��ʾ��
	pop ax			; ��ջ�е�����
	call bcd2ascii	; ����BCDתASCII����
	; ��ʾ��λ��������
	;mov [es:bx + 6], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 8], al
	call ShowChar_dt
	; ��ʾ�ָ���':'
	mov al,':'
	;mov [es:bx + 10], al
	call ShowChar_dt
	; ��ʾ��
	pop ax			; ��ջ�е�����
	call bcd2ascii	; ����BCDתASCII����
	; ��ʾ��λСʱ����
	;mov [es:bx + 12], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 14], al
	call ShowChar_dt
	
	mov dx,[es:TEMP_DX]          ;�ָ����λ��
	mov ah,2
	mov bh,0
	int 10h
	; ����EOI��8259A
	mov al, 0x20	;�жϽ�������EOI 
	out 0xa0, al	;���Ƭ���� 
	out 0x20, al	;����Ƭ���� 

	; �ָ�����ļĴ���ֵ
	pop es
	pop bx
	pop ax

	iret			; ���жϷ���
TEMP_DX DW 0
; -------------------------------------------------------------------
_dt:
	pusha 
	; ��ȡ����Ϣ
	mov al, 9			; ���ƫ�Ƶ�ַΪ9
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	; ��ʾ����Ϣ
	call ShowBCD	; ��ʾBCDʮ������
	; ��ʾ���ָ���
	;mov al, '.'			; AL = '.'
	;call ShowChar_dt		; ��ʾ�ַ�
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
	;call DispStr_Chinese		; ��ʾ�ַ���
	push ax
	mov bl,0fh          ;����ɫ
	mov ah,42h
	int 21h
	pop ax
	; ��ȡ����Ϣ
	mov al, 8			; �µ�ƫ�Ƶ�ַΪ8
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	; ��ʾ����Ϣ
	call ShowBCD	; ��ʾBCDʮ������
	; ��ʾ���ָ���
	;mov al, '.'			; AL = '.'
	;call ShowChar_dt		; ��ʾ�ַ�
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
	;call DispStr_Chinese		; ��ʾ�ַ���
	push ax
	mov bl,0fh          ;����ɫ
	mov ah,42h
	int 21h
	pop ax
	
	; ��ȡ����Ϣ
	mov al, 7			; �յ�ƫ�Ƶ�ַΪ7
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	; ��ʾ����Ϣ
	call ShowBCD	; ��ʾBCDʮ������
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
	;call DispStr_Chinese		; ��ʾ�ַ���
	push ax
	mov bl,0fh          ;����ɫ
	mov ah,42h
	int 21h
	pop ax
	; ��ʾ�ո�ָ���
	mov al, ' '			; AL = ' '
	call ShowChar_dt		; ��ʾ�ַ�
	
	; ��ȡ������Ϣ
	mov al, 6			; ���ڵ�ƫ�Ƶ�ַΪ6
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ����������Ϣ
	; ��ʾ������Ϣ
	dec al			; AL --
	mov bl, 6			; BL = 3
	mul bl			; AX = AL * BL
	add ax, weekstrs_chin	; AX += weekstrs
	mov bp, ax		; BP = AX ָ���Ӧ���ڴ�
	mov cx, 3			; ���� CX = 3
	push ax
	push cx
	mov ah,3
	mov bh,0
	int 10h
	pop cx
	pop ax
	mov dl,7
	;call DispStr_Chinese		; ��ʾ�ַ���
	push ax
	mov bl,0fh          ;����ɫ
	mov ah,42h
	int 21h
	pop ax
	; ��ʾ�ո�ָ���
	mov al, ' '			; AL = ' '
	call ShowChar_dt		; ��ʾ�ַ�
	
	; ��ȡʱ��Ϣ
	mov al, 4			; ʱ��ƫ�Ƶ�ַΪ4
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ����ʱ��Ϣ
	; ��ʾʱ��Ϣ
	call ShowBCD	; ��ʾBCDʮ������
	; ��ʾð�ŷָ���
	mov al, ':'			; AL = ':'
	call ShowChar_dt		; ��ʾ�ַ�

	; ��ȡ����Ϣ
	mov al, 2			; �ֵ�ƫ�Ƶ�ַΪ2
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; �������Ϣ
	; ��ʾ����Ϣ
	call ShowBCD	; ��ʾBCDʮ������
	; ��ʾð�ŷָ���
	mov al, ':'			; AL = ':'
	call ShowChar_dt		; ��ʾ�ַ�

	; ��ȡ����Ϣ
	mov al, 0			; ���ƫ�Ƶ�ַΪ0
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	; ��ʾ����Ϣ
	call ShowBCD	; ��ʾBCDʮ������
	; ���ù��λ��
	mov ah, 2		; ���ܺ�
	mov bh, 0		; ��0ҳ
	mov dl, 0		; �к�
	int 10h			; ��ʾ�ж�
	; �˻�DOS
	popa	
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�  ;�ָ��кŷ���newlineʹ��
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	ret
date_str:
	db '��'
	db '��'
	db '��'
weekstrs: ; �������ڴ�����
	db 'Sun'
	db 'Mon'
	db 'Tue'
	db 'Wed'
	db 'Thu'
	db 'Fri'
	db 'Sat'
weekstrs_chin:
	db '������'
	db '����һ'
	db '���ڶ�'
	db '������'
	db '������'
	db '������'
	db '������'
; -------------------------------------------------------------------
_dc:
	pusha
	; ����70h���ж�����
	; ����70h���ж���IVT�е�ƫ��
	mov bx, 0x70	; BX = 70h���жϺţ�
	shl bx, 2		; BX << 2��BX *= 4�� 
	cli				; �ر��жϣ���ֹ�Ķ��ڼ䷢���µ�0x70���ж�
	; ����70h���жϵ�������
	push es			; ����ES��ջ
	xor ax, ax		; AX = 0
	mov es, ax		; ES = AX = 0
	mov word [es:bx], new_int_0x70_dc ; ƫ�Ƶ�ַ
	mov word [es:bx + 2], cs ; �ε�ַ
	pop es			; ��ջ�лָ�ES

	; ����RTC״̬�Ĵ���B
	mov al, 0x0b	; ָ��RTC�Ĵ���B
	or al, 0x80		; ���NMI 
	out 0x70, al	; ѡ��Ĵ���B
	mov al, 0x12	; ��ֹ�����Ժ������жϣ�ֻ���Ÿ��½������жϣ�����BCD���24Сʱ��  00010010
	out 0x71, al	; ���üĴ���B 
	; ��ȡRTC״̬�Ĵ���C
	mov al, 0x0c	; ָ��RTC�Ĵ���C������NMI
	out 0x70, al	; ѡ��Ĵ���C
	in al, 0x71		; ��RTC�Ĵ���C����λδ�����ж�״̬

	; �򿪴�8259A��IRQ0��RTC���ж�
	in al, 0xa1		; ����8259A��IMR�Ĵ��� 
	and al, 0xfe	; ���bit0����λ����RTC��
	out 0xa1, al	; д�ش˼Ĵ��� 

	sti				; ���¿����ж� 
	
	;jmp $			; �����DOS�����У����ô���ѭ������������˳�DOS�ж�

	; �˻�DOS
	popa
	
	ret
;--------------------------------------------------------------------
hex2bcd:  ;al asciiתΪʮ������   alΪ��16������ֵ
	cmp al, 3ah		; AL ������ ?
	jl .1			; AL < 10����ת��.1
	sub al, 7		; AL >= 10����ʾ��ĸ�� = ��ֵ += 37h��
.1: ; ����
	sub al, 30h		; 
	ret
; -------------------------------------------------------------------
ReadMemmory:   ;��ʾ X:X��16�ֽڵ��ڴ���Ϣ
	mov bp,buf
	add bp,8   ;����READMEM   8���ַ�
	mov cx,9
	push bp
	;call DispStr
	pop bp
	mov si,0
	mov cx,0   ;��ð��ǰ4��ascii��תΪ��ֵ����cx
.1:  
	mov ax,0
	mov al,byte[bp]
	inc si
	inc bp
	call hex2bcd
	mov ah,al
	shl ah,4       ;���ַ��ƶ�����λ
	shld cx,ax,4   ;ÿ���ƶ���λ,8140  8 1 4 0
	cmp si,4
	jz .2
	jmp .1
.2:
	mov si,0   ;��ð�ź�4��ascii��תΪ��ֵ����dx
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
	shld dx,ax,4   ;ÿ���ƶ���λ,8140  8 1 4 0
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
;��ʾ CX:DX��16�ֽڵ��ڴ���Ϣ
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
	; ��ȡ���λ��
	mov ah, 3		; ���ܺ�
	int 10h
	; ���ù��λ��
	mov ah, 2		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h			; ��ʾ�ж�
	add sp,2
	jmp again	
; -------------------------------------------------------------------
ShowChar_dt: ; ��ʾ�����ַ�����ALΪ���ݲ�����
	mov ah, 0Eh		; ���ܺţ��Ե紫��ʽ��ʾ�����ַ���
	mov bl, 0fh 		; ������
	int 10h 			; ����10H���ж�
	ret				; �����̷���
;-------------------------------------------------------------------------------
show_hex2ascii:  ;��ʾ16����תASCII�����룺AL = BCD�룬�����AX = ASCII��
	pusha
	mov dx,ax
	mov dh,dl
	push dx
	;------��ȡ����λ---------;
	mov bx,0
	;shld bx,dx,4
	mov bx,dx
	shr bx,4
	and bl,0fh
	mov al,bl
	call ShowChar
	;------��ȡ����λ---------;
	mov bx,0
	mov bx,dx
	and bl,0fh
	mov al,bl
	call ShowChar
	
	pop dx
	popa
	ret
;-------------------------------------------------------------------------------
bcd2ascii: ;BCD��תASCII�����룺AL = BCD�룬�����AX = ASCII��
	mov ah, al		; AH = AL���ֲ���������֣�
	and al, 0x0f	; AL & 0Fh��ȡBCD�ĵ�4λ���ݣ�
	add al, 0x30	; AL += 30h��ת����ASCII��
	shr ah, 4		; AH >> 4��ȡBCD�ĸ�4λ���ݣ�
	add ah, 0x30	; AH += 30h��ת����ASCII��
	ret				; �����̷���
; -------------------------------------------------------------------	
ShowBCD: ; ��ʾ���ֽ�BCDʮ����������ALΪ���ݲ�����
	push ax			; ����AL��ջ
	shr al, 4			; AL >> 4 ����λ���֣�
	add al, 30h		; �����ַ� = ��ֵ+=30h
	call ShowChar_dt		; ��ʾ�ַ�
	pop ax			; ��ջ�лָ�AL
	and al, 0Fh		; ȡAL�ĵ�4λ
	add al, 30h		; �����ַ� = ��ֵ+=30h
	call ShowChar_dt		; ��ʾ�ַ�
	ret				; �����̷���
; -------------------------------------------------------------------
ver: ; ��ʾ��Ȩ��Ϣ
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ��Ȩ�ַ��� 'MyOS 1.x  (C) 2016 CANNON OS'
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str1 	; BP=����ַ
	mov cx, str1len	; ����
	int 10h 		; ����10H����ʾ�ж�
	ret				; �����̷���

; -------------------------------------------------------------------
DispStr_HZK: ; ��ʾ�ַ���
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�

	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	int 10h 		; ����10H����ʾ�ж�
	ret				; �����̷���

; -------------------------------------------------------------------
ver0: ; ��ʾ��Ȩ��Ϣ
	pusha
	push dx
	mov dh,0
	; ��ʾ��Ȩ�ַ��� 'MyOS 1.x  (C) 2016 CANNON OS'
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str1 	; BP=����ַ
	mov cx, str1len	; ����
	int 10h 		; ����10H����ʾ�ж�
	pop dx
	
	mov ah,02h
	mov bh,0
	int 10h
	popa
	ret				; �����̷���
; -------------------------------------------------------------------
cls: ; ����
	mov	ah, 6		; ���ܺ�
	mov	al, 0		; �������ı�������0=�������ڣ�
	mov bh, 00h		; ���ò�����е��ַ���ɫΪ�ڵ�������
	mov cx, 0		; �������Ͻǵ��к�=CH���к�=CL
	mov dh, 30		; �������½ǵ��к�
	mov dl, 79		; �������½ǵ��к�
	int 10h 		; ����10H����ʾ�ж�
	; ���ù��λ��
	mov ah, 2		; ���ܺ�
	mov bh, 0		; ��0ҳ
	mov dh, 0		; �к�
	mov dl, 0		; �к�
	int 10h			; ��ʾ�ж�
	ret				; �����̷���
	
; -------------------------------------------------------------------
diskok: ; �ж��л�����Ŀ������Ƿ���ڣ��������ΪDL=���̵��������ţ�
	; ���ô��̵�0���ж��жϴ����Ƿ����
	mov ah, 0		; ���ܺ�=0�����̸�λ��������CF��־λ��
	int 13h			; ����13H�Ŵ����ж�
	jc .1			; CF=1 ���̲����ڣ��л�����ʧ��
	; ���̴���ʱ�������л���������
	ret				; �����̷���
	
.1: ; ���̲�����ʱ����ʾ������Ϣ���˳�ѭ�������¿�ʼ
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ���̲����ڵ���Ϣ "Disk not exist!"
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str5 	; BP=����ַ
	mov cx, str5len	; ����
	int 10h 		; ����10H����ʾ�ж�
	; �˳�ѭ�������¿�ʼ
	add sp, 4		; ��������call�ķ��ص�ַ
	jmp again		; ���¿�ʼ
	
str5: ; �ַ���5�����̲�������Ϣ����
	db 'Disk not exist!'
str5len equ $ - str5 ; ���̲����ڴ���
; -------------------------------------------------------------------
toa: ; ��ΪA��
	mov dl, 0		; ����A����������=0
	call diskok		; ������̲����ڣ��Ͳ��л����̣����������
	mov byte [str2], 'A' ; �޸���ʾ������ĸΪA
	mov byte [drvno], 0 ; ������������Ϊ0
	call getdiskparam	; ��ȡ���̲���H&S������ReadSec��ls���̣�
	add sp, 2		; ����call�ķ��ص�ַ

	;�޸�Ŀ¼��ʾ��
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	inc dh
	; ���ù��λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�

    call initialDisk
	
	jmp again		; ���¿�ʼ
	
; -------------------------------------------------------------------
tob: ; ��ΪB��
	mov dl, 1		; ����B����������=1
	call diskok		; ������̲����ڣ��Ͳ��л����̣����������
	mov byte [str2], 'B' ; �޸���ʾ������ĸΪB
	mov byte [drvno], 1 ; ������������Ϊ1
	call getdiskparam	; ��ȡ���̲���H&S������ReadSec��ls���̣�
	add sp, 2		; ����call�ķ��ص�ַ
	;�޸�Ŀ¼��ʾ��
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	inc dh
	; ���ù��λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	
	call initialDisk
	
	jmp again		; ���¿�ʼ

; -------------------------------------------------------------------
toc: ; ��ΪC��
	mov dl, 80h		; Ӳ��C����������=80h
	call diskok		; ������̲����ڣ��Ͳ��л����̣����������
	mov byte [str2], 'C' ; �޸���ʾ������ĸΪC
	mov byte [drvno], 80h ; ������������Ϊ80h
	call getdiskparam	; ��ȡ���̲���H&S������ReadSec��ls���̣�
	add sp, 2		; ����call�ķ��ص�ַ
	;�޸�Ŀ¼��ʾ��
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	inc dh
	; ���ù��λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	
	call initialDisk
	
	jmp again		; ���¿�ʼ

;---------------------------------------------------------------------
initialDisk:;        ;��ʼ��ls�õ��Ĳ���
	pusha
	call ReadPBootSec
	; nsecΪ��Ŀ¼��ʣ������������ʼ��Ϊ��Ŀ¼����������ѭ���л�ݼ�����
	; �����Ŀ¼�������� = ����Ŀ¼���� / 32��
	mov ax, [Sector + 11h]	; AX = ����Ŀ¼����
	shr ax, 4				; AX����4λ��~ /32�� = ��Ŀ¼������
	mov word [nsec], ax		; nsec = AX = ��Ŀ¼������

	; isecΪ��ǰ�����ţ�����ֵΪ��Ŀ¼�����������ţ���ѭ���л��������
	; �����Ŀ¼�������ţ�= ���������� + FAT�� * FATռ��������
	movzx ax, byte [Sector + 10h] ; AX = FAT��
	mul word [Sector + 16h]	; AX *= FATռ������
	add ax, [Sector + 0Eh]	; AX += ����������
	mov [isec],ax			; isec = AX = ��Ŀ¼��������
	popa
	ret
;--------------------------------------------------------------------
dir: ; ��ʾ��Ŀ¼�ļ�
	call showbpb	; ��ʾ������Ϣ
	call ls			; ��ʾ�����ļ���Ϣ�б�
	ret				; �����̷���
;--------------------------------------------------------------------
; ������������̲�����
CurrentDirSectors	dw	14		; ��ǰĿ¼ռ�õ�������
SectorNoOfCurrentDirectory	dw	19	; ��ǰĿ¼������������
SectorNoOfLastDirectory	dw	19	; ��һĿ¼������������
iCurrentDirSectors  dw 0       ;���������һĿ¼��ռ������
Dir_len dw 0					;Ҫcd��Ŀ¼����
isBackBool dw 0					;�Ƿ���������һ��..=2 ���� ͬ��.=1
NumOfSign dw 0					;Ŀ¼�ָ����ĸ���
cdToDir:   ;������Ŀ¼    X:/$��Ŀ¼            ���µ�˼·��:�ڵ�ǰ��������Ŀ¼��Ŀ���ҵ�������鿴��Ŀ�������ţ���ȥ�ңƣ��Ա�������һ��Ŀ¼�Ĵ�С���޸ĵ�ǰ��������SectorNoOfCurrentDirectory���Լ�CurrentDirSectors
	push ax
	mov ax,[SectorNoOfCurrentDirectory]      ;���������һ����ǰ��������
	mov [SectorNoOfLastDirectory],ax
	pop ax
	pusha
	; �ÿո����20h�����Dirbuf
	mov cx, 11	; ѭ������CX=�����л�����buf�ĳ��ȣ�buflen=80��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, Dirbuf		; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,3   ;����cd  �����ַ�
	cmp byte[bp],'\'
	jz backToRoot
	push bp    ;����bp
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
	mov bp,Dirbuf      ;���Ҫ��תĿ¼��Ŀ¼��
	mov cx,11
	;call DispStr
	;�ж��Ƿ�Ϊ. ���� ..
	mov bp,Dirbuf
	mov si,0
.isBack:
	cmp byte[bp],'.'
	jnz .isBackEnds    ;���ǵ��˳���
	inc si
	inc bp
	jmp .isBack
.isBackEnds:
	;cmp si,2           ;һ����ͬ��Ŀ¼������������һ��
	mov [isBackBool],si ;�����жϽ�����޸���ʾ��ʱ�õ�
	cmp word[isBackBool],1   ;һ���㻹��ͬһ��Ŀ¼������ı�Ŀ¼��ʾ��
	jz cd_ends
	cmp word[isBackBool],2  ; �����������һ��Ŀ¼
	jnz .cd_start
	pusha               ;�����µ�Ŀ¼ ��ʾ��
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	;add di,[str2len]
	dec di      ;��ȥ$����
	;����/�ĸ��������ж��Ƿ�Ϊһ��Ŀ¼
	mov cx,[str2len]
	mov word [NumOfSign],0     ;��ʼ��������
.11
	cmp byte[di],'/'
	jnz .11.1
	inc word[NumOfSign]
.11.1:
	inc di
	loop .11
	popa                       ;��pop�����ý����ж���ת
	push ax
	mov ax,ds
	mov es,ax
	pop ax
	cmp word [NumOfSign],2     ;������һ���Ѿ��Ǹ�Ŀ¼
	jz backToRoot
.cd_start:
;-------------------------------------------------------------------1
	push es		; ����ES

; ������λ
	xor	ah, ah	; ���ܺ�ah=0����λ������������
	xor	dl, dl	; dl=0������A������BΪ1��Ӳ�̺�U��Ϊ80h��
	int	13h		; �����ж�
	
; �����ڵ�ǰĿ¼��Ѱ����Ŀ¼
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; ����ʾ��ǰ�����ŵ�
						; ����wSectorNo����ֵΪ��Ŀ¼�����������ţ�=19��
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ��Ŀ¼��ʣ��������
										; ��ʼ��Ϊ14����ѭ���л�ݼ�����
LABEL_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	VOL_NOT_FOUND	; ���������ʾδ�ҵ�Ŀ¼��
	dec	word [wRootDirSizeForLoop]	; �ݼ�����wRootDirSizeForLoop��ֵ
	; ���ö�������������һ����Ŀ¼������װ����
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call ReadSec		; ���ö���������

	mov	si, Dirbuf		; DS:SI -> Ŀ¼��
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; ���DF��־λ
						; �ñȽ��ַ���ʱ�ķ���Ϊ��/��[��������]
	mov	dx, 10h			; ѭ������=16��ÿ��������16���ļ���Ŀ��512/32=16��
VOL_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; ѭ����������
	jz LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR ; ���Ѷ���һ����
	dec	dx				; �ݼ�ѭ������ֵ			  ��������һ����
	mov	cx,11 	; ��ʼѭ������Ϊ11
VOL_CMP_FILENAME:
	repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	cmp	cx, 0
	jz	LABEL_VOL_FOUND ; ����Ƚ���11���ַ�����ȣ���ʾ�ҵ�
VOL_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
						; FFE0h = 1111111111100000����5λ=32=Ŀ¼��Ŀ��С��
	add	di, 20h			; DI += 20h ��һ��Ŀ¼��Ŀ
	mov	si, Dirbuf		; SIָ��װ���ļ���������ʼ��ַ
	jmp	VOL_SEARCH_FOR_VOL_FILE; ת��ѭ����ʼ��

LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR: ;������Ŀ¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRҪ�Լ������(ֱ��ʹ��toDir�е��㷨)  ���Ŀ¼�㷨��ͬ
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	exit_cd ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	LABEL_SEARCH_IN_Current_DIR_BEGIN		; ��������Ŀ¼ѭ��
.root:
	inc	word [wSectorNo]	; ���ڸ�Ŀ¼��������ǰ������
	jmp	LABEL_SEARCH_IN_Current_DIR_BEGIN
exit_cd:			;û����Ŀ¼���� ��תʧ��ֱ���˳�
	pop es			; �ָ�ES
	call showError1	; ��ʾ�ַ���
	jmp cd_ends
VOL_NOT_FOUND:     
	pop es			; �ָ�ES
	call showError1	; ��ʾ�ַ���
	jmp dir_out
;------------------------------------------------------+
LABEL_VOL_FOUND:           ;aaa
	mov word [iCurrentDirSectors],0  ;ռ�������� ����������
	; �����ļ�����ʼ������
	mov	ax, [CurrentDirSectors]	; AX=��ǰĿ¼ռ�õ�������
	and	di, 0FFE0h		; DI -> ��ǰ��Ŀ�Ŀ�ʼ��ַ
	add	di, 1Ah			; DI -> ��Ŀ¼��������������Ŀ�е�ƫ�Ƶ�ַ
	mov cx, word [es:di] ; CX=��Ŀ¼����������
	mov word[SectorNoOfCurrentDirectory],cx ;�޸ĵ�ǰĿ¼����������
	pop es
	
	
	mov word [nsec],1
	mov word [isec],cx
	add word [isec],1fh
	;jmp VOL_FILE_LOADED
	pusha
	push cx				; �����������FAT�е����
	add	cx, ax			; CX=�ļ��������ʼ������+��ǰĿ¼ռ�õ�������
	add	cx, DeltaSectorNo ; CL <- Ŀ¼�����ʼ������(0-based)
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��COM�����ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��COM����ƫ�Ƶ�ַ=100h��
	mov	ax, cx			; AX <- ��ʼ������	
VOL_GOON_SETTING_PARAM: ;���õ�ǰĿ¼������������������
	push bx				; ����COM����ƫ�Ƶ�ַ
	mov	cl, 1			; 1������
	call ReadSec		; ������
	inc word [iCurrentDirSectors]
	; ������Ŀ¼��ռ��������
	pop bx				; ȡ��Ŀ¼ƫ�Ƶ�ַ
	pop	ax				; ȡ����������FAT�е����
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	cmp	ax, 0FF8h		; �Ƿ���Ŀ¼������
	jae	VOL_FILE_LOADED ; ��FF8hʱ��ת���������һ����
	push ax				; ����������FAT�е����
	mov	dx, [CurrentDirSectors] ; DX = ��ǰĿ¼������
	add	ax, dx			; ������� + ��ǰĿ¼������
	add	ax, DeltaSectorNo ; AX = Ҫ��������������ַ
	add	bx, [BPB_BytsPerSec] ; BX+512ָ����Ŀ¼��Ŀ������һ��������ַ
	
	jmp VOL_GOON_SETTING_PARAM
VOL_FILE_LOADED:
	;jmp nextdir
	mov ax,[iCurrentDirSectors]
    mov word [CurrentDirSectors],ax
	push cx
	mov cx,[iCurrentDirSectors]
	pusha
	; ��ʾ��4λ
	mov al, cl		; AL=ID��λ�ֽ�
	and al, 0F0h	; ȡ����4λ
	shr al, 4		; AL >> 4
	;call ShowChar	; ������ʾ�ַ�����
	; ��ʾ��4λ
	mov al, cl		; AL=ID��λ�ֽ�
	and al, 0Fh		; ȡ����4λ
	;call ShowChar	; ������ʾ�ַ�����
	popa
	pop cx
nextdir:
	popa
	cmp word[isBackBool],2  ; �����������һ��Ŀ¼
	jz .upDir
	pusha               ;�����µ�Ŀ¼ ��ʾ��
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	add di,[str2len]
	dec di      ;��ȥ$����
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
.upDir:                 ;�����ϼ�Ŀ¼���ж�,ע�����Ҫ�ж���һ���ǲ��Ǹ�Ŀ¼
	;�Ƿ��Ƿ��ظ�Ŀ¼��ǰ���ж�
	;������һ����,��ʾ��Ҫ����һ��
	pusha
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	add di,[str2len]
	std                         ;�������
	mov al,20h                  ;���/$
	stosb
	mov al,20h                 
	stosb
	dec di
.2
	cmp byte[di],'/'
	jz .2.1                     ;������һ��/�˳���
	mov al,20h                  ;�������
	stosb
	dec word [str2len]
	jmp .2
.2.1:
	cld
	inc di
	mov al,'$'                  ;���$
	stosb
	dec word [str2len]
	popa
	jmp dir_out
backToRoot:
	call getdiskparam	; ��ȡ���̲���H&S������ReadSec��ls���̣�
	mov word [CurrentDirSectors],RootDirSectors
	mov word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	;mov byte [drvno], 0 ; ������������Ϊ0
	
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	call initialDisk
dir_out:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;���Ŀ¼Dirbuf
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
	; ��ʾ��4λ
	mov al, cl		; AL=ID��λ�ֽ�
	and al, 0F0h	; ȡ����4λ
	shr al, 4		; AL >> 4
	;call ShowChar	; ������ʾ�ַ�����
	; ��ʾ��4λ
	mov al, cl		; AL=ID��λ�ֽ�
	and al, 0Fh		; ȡ����4λ
	;call ShowChar	; ������ʾ�ַ�����
	popa
	;call judgeRootTODir
	;�ж��Ƿ���Ǹ�Ŀ¼����Ŀ¼���л�
	cmp word [SectorNoOfLastDirectory],SectorNoOfRootDirectory
	jz .1     ;�ϴ�Ŀ¼�Ǹ�Ŀ¼
	jmp .2
.1	cmp word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .2.2       ;�Ӹ�Ŀ¼������Ŀ¼+1fh
	add word[SectorNoOfCurrentDirectory],1fh       ;Ϊʲô��1fh�� ��Ŀ¼�ļ���Ŀ�е����������������������ʼλ�õģ��õ����������ŵķ�����+1fh��3e00h��  Ϊʲô����ô��=_= 
	;Ҫ��������������ַ = ������� + ��Ŀ¼������ + DeltaSectorNo           ��Ŀ¼������ + DeltaSectorNo = 17+14 =1fh
	jmp .2.2 
.2:	;ԭ������Ŀ¼
	cmp word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .2.2     ;�����Ǹ�Ŀ¼��+1fh
	add word[SectorNoOfCurrentDirectory],1fh 
.2.2:	
	mov ax,[SectorNoOfCurrentDirectory]
	mov [isec],ax
	jmp cd_ends
cd_error:
	call showError1	; ��ʾ�ַ���
cd_ends:	
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	inc dh
	; ���ù��λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	add sp,2
	
	jmp again
;--------------------------------------------------------------------
judgeRootTODir:   
	
	ret
;--------------------------------------------------------------------	
FileName_rename_len dw 0
FileSuffixes_len dw 0
FileName_renameTooLongStr db '�ļ���̫�����޷��޸�������'
FileName_renameTooLong_len  equ ($ - FileName_renameTooLongStr)/2
RenameFile_DI dw 0
IsNotDir dw 1
rename:
	mov word [IsNotDir],1
	pusha
	; �ÿո����20h�����Dirbuf
	mov cx, 11	; ѭ������CX=�����л�����buf�ĳ��ȣ�buflen=80��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, Dirbuf		; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	mov word [Dir_len],11 ;�ļ���Ŀ->�ļ���Ϊ11�ֽ�
	
	mov cx,buflen
	mov bp,buf
	add bp,7  ;����rename  6���ַ�
	;�ڴ˿ɼ������Ϸ���
	push bp    ;����bp
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
	
	cmp word[IsNotDir],1     ;��Ŀ¼�����ļ���׺���
	jnz .4.1
	push bp
	mov [FileName_rename_len],si
	add bp,si            ;spָ���׺
	add bp,[IsNotDir]             ;�����ļ�����. ������Ŀ¼����
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
	mov di,Dirbuf+8 ;ƫ�Ƶ��ļ���׺��
	cld
	mov cx,si
	mov si,bp
	add si,[FileName_rename_len]	  ;��λ��buf����ļ���׺
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
;	mov di,Dirbuf            ;���Ŀ¼Dirbuf ��׺(ƥ��Ŀ¼��Щbug,ֻ�������׺)
;	add di,[FileName_rename_len]
;.next_10:
	;xor ah,ah
	;int 16h
;-------------------------------------------------------------------1
	push es		; ����ES

; ������λ
	xor	ah, ah	; ���ܺ�ah=0����λ������������
	xor	dl, dl	; dl=0������A������BΪ1��Ӳ�̺�U��Ϊ80h��
	int	13h		; �����ж�
	
; �����ڵ�ǰĿ¼��Ѱ����Ŀ¼
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; ����ʾ��ǰ�����ŵ�
						; ����wSectorNo����ֵΪ��Ŀ¼�����������ţ�=19��
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ��Ŀ¼��ʣ��������
										; ��ʼ��Ϊ14����ѭ���л�ݼ�����
RENAME_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	RENAME_NOT_FOUND	; ���������ʾδ�ҵ�Ŀ¼��
	dec	word [wRootDirSizeForLoop]	; �ݼ�����wRootDirSizeForLoop��ֵ
	; ���ö�������������һ����Ŀ¼������װ����
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call ReadSec		; ���ö���������

	mov	si, Dirbuf		; DS:SI -> Ŀ¼��
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; ���DF��־λ
						; �ñȽ��ַ���ʱ�ķ���Ϊ��/��[��������]
	mov	dx, 10h			; ѭ������=16��ÿ��������16���ļ���Ŀ��512/32=16��
RENAME_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; ѭ����������
	jz RENAME_GOTO_NEXT_SECTOR_IN_Current_DIR ; ���Ѷ���һ����
	dec	dx				; �ݼ�ѭ������ֵ			  ��������һ����
	mov	cx,[Dir_len] 	; ��ʼѭ������Ϊ11
RENAME_CMP_FILENAME:
	repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	cmp	cx, 0
	jz	RENAME_VOL_FOUND ; ����Ƚ���11���ַ�����ȣ���ʾ�ҵ�
RENAME_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
						; FFE0h = 1111111111100000����5λ=32=Ŀ¼��Ŀ��С��
	add	di, 20h			; DI += 20h ��һ��Ŀ¼��Ŀ
	mov	si, Dirbuf		; SIָ��װ���ļ���������ʼ��ַ
	jmp	RENAME_SEARCH_FOR_VOL_FILE; ת��ѭ����ʼ��

RENAME_GOTO_NEXT_SECTOR_IN_Current_DIR: ;������Ŀ¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRҪ�Լ������(ֱ��ʹ��toDir�е��㷨)  ���Ŀ¼�㷨��ͬ
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	exit_rename ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	RENAME_SEARCH_IN_Current_DIR_BEGIN		; ��������Ŀ¼ѭ��
.root:
	inc	word [wSectorNo]	; ���ڸ�Ŀ¼��������ǰ������
	jmp	RENAME_SEARCH_IN_Current_DIR_BEGIN

RENAME_NOT_FOUND:
	pop es			; �ָ�ES
exit_rename:
	call showError1	; ��ʾ�ַ���
	jmp out_rename
;------------------------------------------------------+
RENAME_VOL_FOUND
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
	mov [RenameFile_DI],di
	;�򵥵�debug
	
	pop es
	;pusha
	;mov cx,11
	;mov bp,di
	;call DispStr
	;popa
	
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;���Ŀ¼Dirbuf
	rep stosb
	
	mov bp,buf
	add bp,7  ;����rename  6���ַ�

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
	;�ڴ˿ɼ������Ϸ���
	push bp    ;����bp
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
	
	cmp word[IsNotDir],1     ;��Ŀ¼�����ļ���׺���
	jnz .4.1
	push bp
	mov [FileName_rename_len],si
	add bp,si            ;spָ���׺
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
	mov di,Dirbuf+8 ;ƫ�Ƶ��ļ���׺��
	cld
	mov cx,si
	mov si,bp
	add si,[FileName_rename_len]	  ;��λ��buf����ļ���׺
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
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov di,[RenameFile_DI]
	
	mov si,Dirbuf
	mov cx,11
	repe movsb			; д���������޸Ķ�Ӧ�ļ���Ŀ
	
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call WriteSec		; ����д��������
	
	pop es
	;Dirbuf
	jmp out_rename
rename_tolong:;��̫��
	mov cx,FileName_renameTooLong_len
	mov bp,FileName_renameTooLongStr
	call DispStr_Chinese
out_rename:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;���Ŀ¼Dirbuf
	rep stosb
	mov word[Dir_len],0
	
	add sp,2
	jmp again
;--------------------------------------------------------------------	
mkdir:
	;mov cx,buflen
	;mov bp,buf
	;call DispStr
	;��FAT������δʹ�õ���Ŀ������д�����ж�д��Ŀ¼����buf�����ڵ�ǰĿ¼���һ����Ŀ¼��
		push ax
	mov ax,[SectorNoOfCurrentDirectory]      ;���������һ����ǰ��������
	mov [SectorNoOfLastDirectory],ax
	pop ax
	pusha
	; �ÿո����20h�����Dirbuf
	mov cx, 11	; ѭ������CX=�����л�����buf�ĳ��ȣ�buflen=80��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, Dirbuf		; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,6   ;����mkdir   �����ַ�
	push bp    ;����bp
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
	mov bp,Dirbuf      ;���Ҫ��תĿ¼��Ŀ¼��
	mov cx,11
	call DispStr
	
	.cd_start:
;-------------------------------------------------------------------1
	push es		; ����ES

; ������λ
	xor	ah, ah	; ���ܺ�ah=0����λ������������
	xor	dl, dl	; dl=0������A������BΪ1��Ӳ�̺�U��Ϊ80h��
	int	13h		; �����ж�
	
; �����ڵ�ǰĿ¼��Ѱ����Ŀ¼
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; ����ʾ��ǰ�����ŵ�
						; ����wSectorNo����ֵΪ��Ŀ¼�����������ţ�=19��
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ��Ŀ¼��ʣ��������
										; ��ʼ��Ϊ14����ѭ���л�ݼ�����
MKDIR_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	MKDIR_NOT_FOUND	; ���������ʾδ�ҵ�Ŀ¼��
	dec	word [wRootDirSizeForLoop]	; �ݼ�����wRootDirSizeForLoop��ֵ
	; ���ö�������������һ����Ŀ¼������װ����
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call ReadSec		; ���ö���������

	mov	si, Dirbuf		; DS:SI -> Ŀ¼��
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; ���DF��־λ
						; �ñȽ��ַ���ʱ�ķ���Ϊ��/��[��������]
	mov	dx, 10h			; ѭ������=16��ÿ��������16���ļ���Ŀ��512/32=16��
MKDIR_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; ѭ����������
	jz MKDIR_GOTO_NEXT_SECTOR_IN_Current_DIR ; ���Ѷ���һ����
	dec	dx				; �ݼ�ѭ������ֵ			  ��������һ����
	mov	cx,11 	; ��ʼѭ������Ϊ11
MKDIR_CMP_FILENAME:
	repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	cmp	cx, 0
	jz	MKDIR_VOL_FOUND ; ����Ƚ���11���ַ�����ȣ���ʾ�ҵ�
MKDIR_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
						; FFE0h = 1111111111100000����5λ=32=Ŀ¼��Ŀ��С��
	add	di, 20h			; DI += 20h ��һ��Ŀ¼��Ŀ
	mov	si, Dirbuf		; SIָ��װ���ļ���������ʼ��ַ
	jmp	MKDIR_SEARCH_FOR_VOL_FILE; ת��ѭ����ʼ��

MKDIR_GOTO_NEXT_SECTOR_IN_Current_DIR: ;������Ŀ¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRҪ�Լ������(ֱ��ʹ��toDir�е��㷨)  ���Ŀ¼�㷨��ͬ
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	exit_mk ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	MKDIR_SEARCH_IN_Current_DIR_BEGIN		; ��������Ŀ¼ѭ��
.root:
	inc	word [wSectorNo]	; ���ڸ�Ŀ¼��������ǰ������
	jmp	MKDIR_SEARCH_IN_Current_DIR_BEGIN
exit_mk:			;û����Ŀ¼���� ��תʧ��ֱ���˳�
MKDIR_NOT_FOUND:    ; û���ҵ�������������Ŀ
	pop es			; �ָ�ES
	
	call CreateDir  ;��������Ŀ¼����
	
	jmp mk_out
;------------------------------------------------------+
MKDIR_VOL_FOUND:     ; �ҵ���,���޷�������Ŀ¼
	pop es			; �ָ�ES
	call showError2	; ��ʾ�ַ���
	jmp mk_out
mk_error1:
	call showError1	; ��ʾ�ַ���
mk_out:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;���Ŀ¼Dirbuf
	rep stosb
	mov word[Dir_len],0
mk_ends:	
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	inc dh
	; ���ù��λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	add sp,2
	
	jmp again
; ------------------------------------------------------------------
; ����Ŀ¼���,Ŀ¼������ DirBuf���棬�������������ڵ�ǰĿ¼�������ҵ�������Ŀ,���ҵ����е�512B������ų�ʼ������������
DefaultDirBuf db 10h
			resb 10	;10B����
			resb 6;db 39h,0a1h,0c2h,48h,50h,0    ;���д��ʱ�䡢���ڡ���ʼ�غ�
			resb 4	;4B��СΪ��
CreateDir:
	;-------------------------------------------------------------------1
	push es		; ����ES

; ������λ
	xor	ah, ah	; ���ܺ�ah=0����λ������������
	xor	dl, dl	; dl=0������A������BΪ1��Ӳ�̺�U��Ϊ80h��
	int	13h		; �����ж�
	
; �����ڵ�ǰĿ¼��Ѱ�ҿ���Ŀ
	mov ax,[SectorNoOfCurrentDirectory]
	mov	word [wSectorNo], ax 	; ����ʾ��ǰ�����ŵ�
						; ����wSectorNo����ֵΪ��Ŀ¼�����������ţ�=19��
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; ��Ŀ¼��ʣ��������
										; ��ʼ��Ϊ14����ѭ���л�ݼ�����
CreateDir_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	CreateDir_NOT_FOUND	; ���������ʾδ�ҵ�Ŀ¼��
	dec	word [wRootDirSizeForLoop]	; �ݼ�����wRootDirSizeForLoop��ֵ
	; ���ö�������������һ����Ŀ¼������װ����
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call ReadSec		; ���ö���������

	;mov	si, MKbuf		; DS:SI -> Ŀ¼��
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; ���DF��־λ
						; �ñȽ��ַ���ʱ�ķ���Ϊ��/��[��������]
	mov	dx, 10h			; ѭ������=1��ÿ��������16���ļ���Ŀ��512/32=16��
CreateDir_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; ѭ����������
	jz CreateDir_GOTO_NEXT_SECTOR_IN_Current_DIR ; ���Ѷ���һ����
	dec	dx				; �ݼ�ѭ������ֵ			  ��������һ����
CreateDir_CMP_FILENAME:
	;repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	;cmp	cx, 0
	;�жϵ�һ���ֽڵ�ֵ
	cmp byte[es:di],0
	jz actually_found
	cmp byte[es:di],05
	jz actually_found
	cmp byte[es:di],0e5h
	jz actually_found
	
	;jz	CreateDir_VOL_FOUND ; ����Ƚ���11���ַ�����ȣ���ʾ�ҵ�
CreateDir_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
						; FFE0h = 1111111111100000����5λ=32=Ŀ¼��Ŀ��С��
	add	di, 20h			; DI += 20h ��һ��Ŀ¼��Ŀ
	;mov	si, MKbuf		; SIָ��װ���ļ���������ʼ��ַ
	jmp	CreateDir_SEARCH_FOR_VOL_FILE; ת��ѭ����ʼ��

CreateDir_GOTO_NEXT_SECTOR_IN_Current_DIR: ;������Ŀ¼LABEL_GOTO_NEXT_SECTOR_IN_Current_DIRҪ�Լ������(ֱ��ʹ��toDir�е��㷨)  ���Ŀ¼�㷨��ͬ
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	CreateDir_NOT_FOUND ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	CreateDir_SEARCH_IN_Current_DIR_BEGIN		; ��������Ŀ¼ѭ��
.root:
	inc	word [wSectorNo]	; ���ڸ�Ŀ¼��������ǰ������
	jmp	CreateDir_SEARCH_IN_Current_DIR_BEGIN
actually_found:          ;���ֿ����
	;�����ļ���
	;��������(Ĭ��)��Ŀ¼=10h
	;����ʱ��(��ʱ��������)
	;�ļ���СĬ��ȫ0
	pop es

	push es
	cld
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
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
	repe movsb			; д���������޸Ķ�Ӧ�ļ���Ŀ  �ļ���
	
	;�����ʱ�䡢����
	call CountTimeDate
	
	;fat���б����õ�������,�������Ŀ¼
	;����fat����Ϊ����  getEmptyFatEntry  setEmptyFatEntry
	call getEmptyFatEntry
	push ax
	;call hex2ascii
	;xor ah,ah
	;int 16h
	pop ax
	push ax
	mov [InitialDirSector],ax    ;������е������ţ����濪ʼд����
	call setEmptyFatEntry
	pop ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+1ah-11],ax  ;д���Ӧfat����
	pop di
	
	;=======================================
	;��ʼ����Ŀ¼����
	push di
	mov di,InitialDirBuf
	mov word [ds:di+1ah],ax  ;д���Ӧfat����(��ʼ�غ�).
	mov ax,[SectorNoOfCurrentDirectory]
	cmp ax,19                ;��Ŀ¼����Ŀ¼�������岻ͬ���Ƚ��ԲУ�cdToDir��mkDir��Ҫ�ж�
	jnz .noNeedToAdd_1fh
	add ax,1fh
	jmp MKjudgeEnds
.noNeedToAdd_1fh:
	sub ax,1fh
MKjudgeEnds:
	mov word [ds:di+1ah+32],ax  ;д�뵱ǰĿ¼�غ� ffffg..
	pop di
	call CountTimeDate_Initial
	call InitialDir
	;=======================================
	;дĿ¼��Ŀ
	mov si,DefaultDirBuf
	mov cx,21
	repe movsb			; д���������޸Ķ�Ӧ�ļ���Ŀ  Ĭ������
	
	;mov al,0cbh
	;call hex2ascii
	;xor ah,ah
	;int 16h
	
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call WriteSec		; ����д��������
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
InitialDir: ;����������д��
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
	;=============����ʱ��================ʱ��=Сʱ*2048+����*32+��/2
	mov dx,0
	; ��ȡʱ��Ϣ
	mov al, 4			; ʱ��ƫ�Ƶ�ַΪ4
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ����ʱ��Ϣ
	call bcd2hex
	
	mov dl,al
	shl dx,11          ;X2048 ʱ�乫ʽ ʱ��=Сʱ*2048+����*32+��/2
	
	; ��ȡ����Ϣ
	mov al, 2			; �ֵ�ƫ�Ƶ�ַΪ2
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; �������Ϣ
	call bcd2hex
	
	mov ah,0   
	shl ax,5           ;X32
	add dx,ax
	
	; ��ȡ����Ϣ
	mov al, 0			; ���ƫ�Ƶ�ַΪ0
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	shr ax,1           ;/2
	add dx,ax
	
	push di
	mov di,InitialDirBuf
	mov word [ds:di+16h],dx  ;д�뵱ǰʱ��.
	mov word [ds:di+16h+32],dx  ;д�뵱ǰʱ��..
	pop di
	
	;==============��������===============����=(���-1980)*512+�·�*32+��
	mov dx,0
	; ��ȡ����Ϣ
	mov al, 9			; ���ƫ�Ƶ�ַΪ9
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	add ax,2000-1980
	shl ax,9
	add dx,ax
	
	; ��ȡ����Ϣ
	mov al, 8			; �µ�ƫ�Ƶ�ַΪ8
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	shl ax,5
	add dx,ax
	
	; ��ȡ����Ϣ
	mov al, 7			; �յ�ƫ�Ƶ�ַΪ7
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	add dx,ax
	
	push di
	mov di,InitialDirBuf
	mov word [ds:di+18h],dx  ;д�뵱ǰ����.
	mov word [ds:di+18h+32],dx  ;д�뵱ǰ����..
	pop di
	
	pop di
	pop dx
	ret
; -------------------------------------------------------------------
CountTimeDate:
	push dx
	push di
	;=============����ʱ��================ʱ��=Сʱ*2048+����*32+��/2
	mov dx,0
	; ��ȡʱ��Ϣ
	mov al, 4			; ʱ��ƫ�Ƶ�ַΪ4
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ����ʱ��Ϣ
	call bcd2hex
	
	mov dl,al
	shl dx,11          ;X2048 ʱ�乫ʽ ʱ��=Сʱ*2048+����*32+��/2
	
	; ��ȡ����Ϣ
	mov al, 2			; �ֵ�ƫ�Ƶ�ַΪ2
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; �������Ϣ
	call bcd2hex
	
	mov ah,0   
	shl ax,5           ;X32
	add dx,ax
	
	; ��ȡ����Ϣ
	mov al, 0			; ���ƫ�Ƶ�ַΪ0
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	shr ax,1           ;/2
	add dx,ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+16h-11],dx  ;д�뵱ǰʱ��
	pop di
	
	;==============��������===============����=(���-1980)*512+�·�*32+��
	mov dx,0
	; ��ȡ����Ϣ
	mov al, 9			; ���ƫ�Ƶ�ַΪ9
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	add ax,2000-1980
	shl ax,9
	add dx,ax
	
	; ��ȡ����Ϣ
	mov al, 8			; �µ�ƫ�Ƶ�ַΪ8
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	shl ax,5
	add dx,ax
	
	; ��ȡ����Ϣ
	mov al, 7			; �յ�ƫ�Ƶ�ַΪ7
	out 70h, al		; ָ���洢��Ԫ��ַ
	in al, 71h			; ��������Ϣ
	call bcd2hex
	
	mov ah,0
	add dx,ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+18h-11],dx  ;д�뵱ǰ����
	pop di
	
	pop di
	pop dx
	ret
; ------------------------------------------------------------------
bcd2hex:  ;���ʮ����bcd�� ת 16����  ��ڣ�al  ����AL
	push dx
	push bx
	mov dx,0
	mov dl,al
	and dl,0fh     ;��ȡ����λ  ��λ�
	and al,0f0h    ;��ȡ��4λ   ʮλ
	shr al,4 	   ;������λ
	mov ah,0
	mov bl,10
	mul bl         ;��10
	add al,dl      ;�Ӹ�λ
	
	mov ah,0
	pop bx
	pop dx
	ret
; ------------------------------------------------------------------
scrollscreen:      ;������Ļ al=�к�
	pusha
	mov	ah, 6			; ���ܺ�
	mov bh,11110000b		; ���ñ���ɫΪ��ɫ
	mov ch, 0			; CH=�кš�CL=�к�
	mov cl, 0			; �������Ͻǵ����кŶ�Ϊ0
	mov dh, 29		; �������½ǵ��кţ��ı���Ļ25�У��к�=0~24
	mov dl, 79		; �������½ǵ��кţ��ı���Ļ80�У��к�=0~79
	int 10h			; ��ʾ�ж�
	popa
	ret	
; -------------------------------------------------------------------
helpStr:
	db 'You can use the following inner command:'
helpStrLen equ $-helpStr
help: ; ��ʾ������Ϣ
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	
	cmp dh,28
	jl .0
	mov al,2
	call scrollscreen
	mov dh,26
.0:
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, helpStr 	; BP=����ַ
	mov cx, helpStrLen	; ����
	int 10h 		; ����10H���ж�

	inc dh          ;�к�+1
	inc dh          ;�к�+1
	
	; ѭ����ʾ��ʾ��
	push si
	push di
	mov cx,N       ;�������
	mov si,0
	mov di,0
.helps:
	push cx
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 2 		; ��2��
	
	mov bp, cmdstr 	; BP=����ַ
	add bp,si
	mov cx, 8	; ����
	int 10h 		; ����10H���ж�
	
	mov ah, 42h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 10 		; ��11��
	mov bp, cmdHelpStr_chin 	; BP=����ַ
	add bp,di
	mov cx, 15	; ����
	
	int 21h 		; ����10H���ж�
	
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
	ret				; �����̷���
	
; -------------------------------------------------------------------
; �ڲ��������̽���
; ===================================================================


; ===================================================================
; ��������ѭ�����̿�ʼ
; -------------------------------------------------------------------
prompt: ; ��ʾ������ϵͳ��ʾ������
	call newline	; �س�����
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H���ж�
	; ��ʾ��ʾ��
	mov ah, 13h 	; ���ܺ�
	mov al, 1 		; ���ŵ���β
	mov bl, 0fh 	; ����
	mov bh, 0 		; ��0ҳ
	mov dl, 0 		; ��0��
	mov bp, str2 	; BP=����ַ
	mov cx, [str2len]	; ����
	int 10h 		; ����10H���ж�
	ret				; �����̷���
blank db 20h
; -------------------------------------------------------------------
getstrln: ; ��ȡ��������������
	cld				; ��������־λ��ʹɨ���ַ�������Ϊ�Ӵ��׵���β��
	
	; �ÿո����20h�����buf
	mov cx, buflen	; ѭ������CX=�����л�����buf�ĳ��ȣ�buflen=80��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, buf		; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	; �ÿո����20h�����fnbuf��ǰ8���ֽ�
	mov cx, cslen	; ѭ������CX=������ĳ��ȣ�cslen=8��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, fnbuf	; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	mov si, 0		; ��ǰ�ַ�ƫ��λ�� SI = 0
keyin: ; ���ܼ�������
	; �����������صİ���ASCII����AL�У�
	mov ah, 0 		; ���ܺ�
	int 16h 		; ����16H���ж�
	; �Իس�����0DH����������
	cmp al, 0dh 	; �Ƚ�AL�еļ����ַ���س�����ASCII��Ϊ0DH��
	je return 		; �����ת�������̷���
	cmp al, 08h
	je backspace
	; ���水���ַ���buf
	mov [buf + si], al; buf[SI]=AL
	inc si			; SI++
	; ̫��ʱ����
	cmp si, buflen	; SI >= 80 ?
	jae goout		; >= ʱ��ת
	jmp next_k
	
backspace:
	cmp si,0        ;û��������ַ���ת��������
	je keyin
	
	dec si
	mov byte [buf + si], 20h; ����ո�
	
	; ��ʾ�ַ������̣������ô���CX�ʹ���ַBP��
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	pusha
	mov cx,1       ; ����1
	mov bp,blank   ; ����ַ
	push cx			; ����CX����ջ��
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	pop cx			; �ָ�CX����ջ��
	;10	2	�ù��λ��	BH=ҳ��
    ;DH,DL=��,��
	
	dec dl          ; �˸�
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	;dec dl          ; ����һ��
	; �ڵ�ǰλ����ʾ�ַ���������CX�ʹ���ַBP��Ԥ�����ú��ˣ�
	mov ah, 13h		; BIOS�жϵĹ��ܺţ���ʾ�ַ�����
	mov al, 1 		; ���ŵ���β
	mov bh, 0 		; ҳ��=0
	mov bl, 0fh		; �ַ���ɫ=������0���ڵף�000�������֣�1111��
	int 10h 		; ����10H����ʾ�ж�
	
	;10	2	�ù��λ��	BH=ҳ��
    ;DH,DL=��,��
	
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	
	popa
	jmp keyin
	
	; ��ʾAL�еļ����ַ�
next_k:
	mov ah, 0eh 	; ���ܺ�
	mov bl, 0fh 	; ������
	int 10h 		; ����10H���ж�
	jmp keyin		; ѭ�������԰���
return:
	ret 			; �����̷���

goout: ; ������ַ�����������������ʱ��ת����
	call showtoolong; ��ʾ��̫��������Ϣ
	add sp, 2		; ����CALLʱѹջ�ķ��ص�ַ
	jmp again		; ���¿�ʼ��ѭ��
	
; -------------------------------------------------------------------
dtlen: ; ȷ���������
	mov cx, buflen	; CX = ���뻺�������ȣ�80��
	mov al, 20h		; AL = �ո��
	mov di, buf		; DIָ��buf
	; ��buf���ҵ���һ���ո����ֹͣ��
	repne scasb		; CX>0 && [di]��AL ʱDI++����ɨ�裬�����˳�ѭ��
	jcxz toolong	; CX=0��û�ҵ��ո��������n = buflen >> cslen (= 8)
	; ���� n = ���뻺�������� - CX - 1
	mov word [n], buflen ; n = buflen
	sub [n], cx		; n - CX
	dec word [n]	; n--
	je zlen 		; n=0�����¿�ʼ������ѭ��
	cmp word [n], cslen ; n > 8 ?
	ja toolong		; ���������8ʱ��ת
	ret 			; �����̷���

toolong: ; ���̫���������˳���
	call showwrong	; ��ʾ������Ϣ
zlen: ; n=0ʱ���¿�ʼ
	add sp, 2		; ����callѹջ�ķ��ص�ַ
	jmp again		; ���¿�ʼ

; -------------------------------------------------------------------
tocap: ; ת���ɴ�д��ĸ
	mov cx, [n]		; ѭ������ CX = n
	mov bx, 0		; �ַ�ƫ��ֵ BX = 0����ֵΪ0��
next: ; ѭ����ʼ
	cmp byte [buf + bx], 61h	; �ַ�����ĸa��61h���Ƚ�
	jb notll					; �ַ� < 61h ��ת
	cmp byte [buf + bx], 7ah	; �ַ�����ĸz��7Ah���Ƚ�
	ja notll					; �ַ� > 7Ah ��ת
	sub byte [buf + bx], 20h	; Сд��ĸ - 20h = ��д��ĸ
notll: ; ����Сд��ĸ
	inc bx			; ����ƫ��ֵ
	loop next		; ����ѭ��
	ret 			; �����̷���
; -------------------------------------------------------------------
tocap_Dirbuf: ; ת���ɴ�д��ĸ
	mov cx, [Dir_len]		; ѭ������ CX = n
	mov bx, 0		; �ַ�ƫ��ֵ BX = 0����ֵΪ0��
.next: ; ѭ����ʼ
	cmp byte [Dirbuf + bx], 61h	; �ַ�����ĸa��61h���Ƚ�
	jb .notll					; �ַ� < 61h ��ת
	cmp byte [Dirbuf + bx], 7ah	; �ַ�����ĸz��7Ah���Ƚ�
	ja .notll					; �ַ� > 7Ah ��ת
	sub byte [Dirbuf + bx], 20h	; Сд��ĸ - 20h = ��д��ĸ
.notll: ; ����Сд��ĸ
	inc bx			; ����ƫ��ֵ
	loop .next		; ����ѭ��
	ret 			; �����̷���

; -------------------------------------------------------------------
newstr:	; �����´������ --> COM�ļ�����
	mov si, buf		; Դ����ʼ��ַ
	mov di, fnbuf	; Ŀ�Ĵ���ʼ��ַ
	mov cx, [n]		; ѭ������ CX = n
	; �����뻺����buf�е�������Ƶ��ļ���������fnbuf��
	rep movsb		; CX > 0ʱ [ES:DI] = [DS:SI]��CX--��CX = 0ʱ�˳�ѭ��
	ret 			; �����̷���

; -------------------------------------------------------------------
iscmd: ; �ж��Ƿ�Ϊ�ڲ�����
	mov word [i], 0	; ��ѭ������/�ڲ���������i=0����ֵΪ0��
	mov dx, cmdstr	; ����ĳ�ʼ��ʼ��ַ
	
.1: ; ��ѭ��
	mov si, fnbuf	; Դ����ʼ��ַ
	mov di, dx		; Ŀ�Ĵ���ʼ��ַ
	mov cx, cslen 	; ��ѭ������
	; �ظ��Ƚ����ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	repe cmpsb		; CX>0 && [DS:SI]==[ES:DI]ʱ��CX--��SI++��DI++������ѭ���������˳�
	jcxz docmd		; CX=0����ʾ������ȣ�Ϊ��BX���ڲ��������תִ�и�����
	inc word [i]	; CX��0����ʾ�������ȣ�i++
	cmp word [i], N	; i=N���ڲ�������������
	je .2			; �����ڲ�����˳�ѭ��
	add dx, cslen	; DX + 8 =��һ�������ʼ��ַ
	jmp .1			; ������ѭ��
.2: ; ����
	;call showwrong	; ��ʾ������Ϣ
	ret 			; �����̷���
	
docmd: ; ִ���ڲ�����
	add sp, 2		; ����call iscmdʱѹջ�ķ��ص�ַ
	call newline	; �س�����
	mov bx, [i]		; BX = �ڲ���������i
	shl bx, 1		; ƫ�Ƶ�ַ = �ڲ���������*2
	call [cmdaddr + bx] ; ���õ�i���ڲ�����
	jmp again		; ��ת��������ѭ��
	
;--------------------------------------------------------------------
exec: ; ִ���ⲿ���COM�ļ���

; ���峣����COM�ļ�����λ�úʹ��̲�����
BaseOfLoader	equ	2000h	; COM�ļ������ص���λ�� ----  �ε�ַ
OffsetOfLoader	equ	100h	; COM�ļ������ص���λ�� ---- ƫ�Ƶ�ַ
RootDirSectors	equ	14		; ��Ŀ¼ռ�õ�������
SectorNoOfRootDirectory	equ	19	; ��Ŀ¼������������
SectorNoOfFAT1	equ	1		; FAT#1���������� = BPB_RsvdSecCnt
DeltaSectorNo	equ	17		; DeltaSectorNo = BPB_RsvdSecCnt + 
							; (BPB_NumFATs * FATSz) - 2 = 1 + (2*9) -2 = 17
							; �ļ��Ŀ�ʼ������ = Ŀ¼��Ŀ�еĿ�ʼ������ 
							; + ��Ŀ¼ռ��������Ŀ + DeltaSectorNo
	call Shut_dc
	push es		; ����ES
; ������λ
	xor	ah, ah	; ���ܺ�ah=0����λ������������
	xor	dl, dl	; dl=0������A������BΪ1��Ӳ�̺�U��Ϊ80h��
	int	13h		; �����ж�
	
; �����ڴ���Ŀ¼��Ѱ�� COM�ļ�
	;�ж��Ǹ�Ŀ¼������Ŀ¼
	push ax
	mov ax,[SectorNoOfCurrentDirectory] 	; ����ʾ��ǰ�����ŵ�
	mov	word [wSectorNo], ax
						; ����wSectorNo����ֵΪ��ǰĿ¼������������
	mov ax, [CurrentDirSectors]	; ʣ��������
	mov word [wRootDirSizeForLoop],ax
										; ��ʼ��Ϊ��ǰĿ¼��ռ����������ѭ���л�ݼ�����
	pop ax
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	LABEL_NOT_FOUND	; ���������ʾδ�ҵ�COM�ļ�
	dec	word [wRootDirSizeForLoop]	; �ݼ�����wRootDirSizeForLoop��ֵ
	; ���ö�������������һ��Ŀ¼������װ����
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader��4000h��
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader��100h��
	mov	ax, [wSectorNo]	; AX <- ��Ŀ¼�еĵ�ǰ������
	mov	cl, 1			; ֻ��һ������
	call ReadSec		; ���ö���������

	mov	si, fnbuf		; DS:SI -> COM�ļ�
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; ���DF��־λ
						; �ñȽ��ַ���ʱ�ķ���Ϊ��/��[��������]
	mov	dx, 10h			; ѭ������=16��ÿ��������16���ļ���Ŀ��512/32=16��
LABEL_SEARCH_FOR_COM_FILE:
	cmp	dx, 0			; ѭ����������
	jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR ; ���Ѷ���һ����
	dec	dx				; �ݼ�ѭ������ֵ			  ��������һ����
	mov	cx, 11			; ��ʼѭ������Ϊ11
LABEL_CMP_FILENAME:
	repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND ; ����Ƚ���11���ַ�����ȣ���ʾ�ҵ�
LABEL_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0Ϊ������ָ����Ŀ��ͷ����5λ���㣩
						; FFE0h = 1111111111100000����5λ=32=Ŀ¼��Ŀ��С��
	add	di, 20h			; DI += 20h ��һ��Ŀ¼��Ŀ
	mov	si, fnbuf		; SIָ��װ���ļ���������ʼ��ַ
	jmp	LABEL_SEARCH_FOR_COM_FILE; ת��ѭ����ʼ��

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:             ;ssssss
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	LABEL_NOT_FOUND ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN		; ��������Ŀ¼ѭ��
.root:
	inc	word [wSectorNo]	; ���ڸ�Ŀ¼��������ǰ������
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NOT_FOUND:
	pop es			; �ָ�ES
	call showwrong	; ��ʾ�ַ���
	ret

; ���潫COM�ļ����ص��ڴ�
LABEL_FILENAME_FOUND:	; �ҵ� COM�ļ���������������
	; �����ļ�����ʼ������
	mov	ax, [CurrentDirSectors]	; AX=��ǰĿ¼ռ�õ�������
	and	di, 0FFE0h		; DI -> ��ǰ��Ŀ�Ŀ�ʼ��ַ
	add	di, 1Ah			; DI -> �ļ���������������Ŀ�е�ƫ�Ƶ�ַ
	mov cx, word [es:di] ; CX=�ļ�����������
	push cx				; �����������FAT�е����
	add	cx, RootDirSectors			; CX=�ļ��������ʼ������+��Ŀ¼ռ�õ������� +��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������+��Ŀ¼ռ�õ�������
	;��Ҫ������˵һ���=_=,�����bug���˼�Сʱ   ԭ����add	cx,ax   ������Ŀ¼ax�����Ǹ�Ŀ¼��������
	add	cx, DeltaSectorNo ; CL <- COM�ļ�����ʼ������(0-based)
	mov	ax, BaseOfLoader      ;+1C
	mov	es, ax			; ES <- BaseOfLoader��COM�����ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��COM����ƫ�Ƶ�ַ=100h��
	mov	ax, cx			; AX <- ��ʼ������

LABEL_GOON_LOADING_FILE:
	push bx				; ����COM����ƫ�Ƶ�ַ
	mov	cl, 1			; 1������
	call ReadSec		; ������

	; �����ļ�����һ������
	pop bx				; ȡ��COM����ƫ�Ƶ�ַ
	pop	ax				; ȡ����������FAT�е����
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	cmp	ax, 0FF8h		; �Ƿ����ļ�����
	jae	LABEL_FILE_LOADED ; ��FF8hʱ��ת���������һ����
	push ax				; ����������FAT�е����
	mov	dx, RootDirSectors	; DX = ��Ŀ¼������
	add	ax, dx			; ������� + ��Ŀ¼������
	add	ax, DeltaSectorNo ; AX = Ҫ��������������ַ
	add	bx, [BPB_BytsPerSec] ; BX+512ָ��COM����������һ��������ַ
	jmp	LABEL_GOON_LOADING_FILE

; ������תִ��COM����
LABEL_FILE_LOADED:
	pop es
	add sp, 2			; ����callָ��ѹջ�ķ��ص�ַ�ͱ����ES
	jmp	BaseOfLoader:OffsetOfLoader	; ��һ����ת���Ѽ��ص��ڴ��е�
						; COM�ļ��Ŀ�ʼ������ʼִ�� COM�ļ��Ĵ��롣
						; ��COM����ͨ������21h�жϷ��������г���

; ����
BPB_BytsPerSec	DW 512	; ÿ�����ֽ���
BPB_SecPerTrk	DW 18	; ÿ�ŵ�������

wRootDirSizeForLoop	dw	RootDirSectors	; ��Ŀ¼��ʣ��������
										; ��ʼ��Ϊ14����ѭ���л�ݼ�����
wSectorNo		dw	0	; ��ǰ�����ţ���ʼ��Ϊ0����ѭ���л����
bOdd			db	0	; ��������ż��FAT��
; -------------------------------------------------------------------
; ��������ѭ�����̽���
; ===================================================================


; ===================================================================
; ���͸������̿�ʼ
;--------------------------------------------------------------------

;--------------------------------------------------------------------
; ��������GetFATEntry
;--------------------------------------------------------------------
; ���ã��ҵ����ΪAX��������FAT�е���Ŀ���������AX�С���Ҫע���
;     �ǣ��м���Ҫ��FAT��������ES:BX�������Ժ���һ��ʼ������ES��BX
GetFATEntry:
	push es			; ����ES��BX��AX����ջ��
	push bx
	push ax
; ���ö����FAT����д��Ļ���ַ
	mov ax, BaseOfLoader	; AX=4000h
	sub	ax, 100h	; ��BaseOfLoader��������4K�ռ����ڴ��FAT
	mov	es, ax		; ES=8F00h
; �ж�FAT�����ż
	pop	ax			; ȡ��FAT����ţ���ջ��
	mov	byte [bOdd], 0; ��ʼ����ż����ֵΪ0��ż��
	mov	bx, 3		; AX*1.5 = (AX*3)/2
	mul	bx			; DX:AX = AX * 3��AX*BX �Ľ��ֵ����DX:AX�У�
	mov	bx, 2		; BX = 2��������
	xor	dx, dx		; DX=0	
	div	bx			; DX:AX / 2 => AX <- �̡�DX <- ����
	cmp	dx, 0		; ���� = 0��ż������
	jz LABEL_EVEN	; ż����ת
	mov	byte [bOdd], 1	; ����
LABEL_EVEN:		; ż��
	; ����AX����FAT����FAT�е�ƫ������������
	; ����FAT�����ĸ�������(FATռ�ò�ֹһ������)
	xor	dx, dx		; DX=0	
	mov	bx, [BPB_BytsPerSec]	; BX=512
	div	bx			; DX:AX / 512
		  			; AX <- �� (FAT�����ڵ����������FAT��������)
		  			; DX <- ���� (FAT���������ڵ�ƫ��)
	push dx			; ������������ջ��
	mov bx, 0 		; BX <- 0 ���ǣ�ES:BX = 8F00h:0
	add	ax, SectorNoOfFAT1 ; �˾�֮���AX����FAT�����ڵ�������
	mov	cl, 2			; ��ȡFAT�����ڵ�������һ�ζ������������ڱ߽�
	call	ReadSec	; ��������, ��Ϊһ�� FAT����ܿ�Խ��������
	pop	dx			; DX= FAT���������ڵ�ƫ�ƣ���ջ��
	add	bx, dx		; BX= FAT���������ڵ�ƫ��
	mov	ax, [es:bx]	; AX= FAT��ֵ
	cmp	byte [bOdd], 1	; �Ƿ�Ϊ�����
	jnz	LABEL_EVEN_2	; ż����ת
	shr	ax, 4			; ����������4λ��ȡ��12λ��
LABEL_EVEN_2:		; ż��
	and	ax, 0FFFh	; ȡ��12λ
LABEL_GET_FAT_ENRY_OK:
	pop	bx			; �ָ�ES��BX����ջ��
	pop	es
	ret
;--------------------------------------------------------------------
;����fat����Ϊ����  getEmptyFatEntry  setEmptyFatEntry
getEmptyAX dw 2
getEmptyFatEntry:
;--------------------------------------------------------------------
; ���ã��ҵ����дغ�
	push es
	mov ax,2
	mov word [getEmptyAX],2
.1
	call GetFATEntry
	cmp	ax, 0		; �Ƿ��ǿ��д�
	jz	.FindEmpty ;��������Ϊ�ǿ��д�
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
; ����  ����ax��Ӧ�Ĵغű�ռ��
	push es			; ����ES��BX��AX����ջ��
	push bx
	push ax
; ���ö����FAT����д��Ļ���ַ
	mov ax, BaseOfLoader	; AX=4000h
	sub	ax, 100h	; ��BaseOfLoader��������4K�ռ����ڴ��FAT
	mov	es, ax		; ES=8F00h
; �ж�FAT�����ż
	pop	ax			; ȡ��FAT����ţ���ջ��
	mov	byte [bOdd], 0; ��ʼ����ż����ֵΪ0��ż��
	mov	bx, 3		; AX*1.5 = (AX*3)/2
	mul	bx			; DX:AX = AX * 3��AX*BX �Ľ��ֵ����DX:AX�У�
	mov	bx, 2		; BX = 2��������
	xor	dx, dx		; DX=0	
	div	bx			; DX:AX / 2 => AX <- �̡�DX <- ����
	cmp	dx, 0		; ���� = 0��ż������
	jz setEmpty_EVEN	; ż����ת
	mov	byte [bOdd], 1	; ����
setEmpty_EVEN:		; ż��
	; ����AX����FAT����FAT�е�ƫ������������
	; ����FAT�����ĸ�������(FATռ�ò�ֹһ������)
	xor	dx, dx		; DX=0	
	mov	bx, [BPB_BytsPerSec]	; BX=512
	div	bx			; DX:AX / 512
		  			; AX <- �� (FAT�����ڵ����������FAT��������)
		  			; DX <- ���� (FAT���������ڵ�ƫ��)
	push dx			; ������������ջ��
	mov bx, 0 		; BX <- 0 ���ǣ�ES:BX = 8F00h:0
	add	ax, SectorNoOfFAT1 ; �˾�֮���AX����FAT�����ڵ�������
	mov [setEmptyAX],ax
	
	mov	cl, 2			; ��ȡFAT�����ڵ�������һ�ζ������������ڱ߽�
	call	ReadSec	; ��������, ��Ϊһ�� FAT����ܿ�Խ��������
	pop	dx			; DX= FAT���������ڵ�ƫ�ƣ���ջ��
	add	bx, dx		; BX= FAT���������ڵ�ƫ��
	mov	ax, [es:bx]	; AX= FAT��ֵ
	cmp	byte [bOdd], 1	; �Ƿ�Ϊ�����
	jnz	setEmpty_EVEN_2	; ż����ת
	
	pusha
	or	ax, 0FFF0h
	;shrd bx,ax, 4			; ����������4λ��ȡ��12λ��
	;mov dx,0FFFH
	;shld dx,bx,4            ;bx�б�������ݷŻ�dx
	mov	[es:bx],ax
	;дFAT��
	mov ax,[setEmptyAX]
	mov cl,2
	mov bx,0
	call WriteSec	
	popa
	jmp setEmpty_GET_FAT_ENRY_OK
setEmpty_EVEN_2:		; ż��
	;and	ax, 0FFFh	; ȡ��12λ
	pusha
	or	ax, 0FFFh
	;shld bx,ax, 4			; ż��������4λ
	;mov dx,0FFF0H
	;shrd dx,bx,4            ;bx�б�������ݷŻ�dx
	mov	[es:bx],ax
	;дFAT��
	mov ax,[setEmptyAX]
	mov cl,2
	mov bx,0
	call WriteSec	
	popa
setEmpty_GET_FAT_ENRY_OK:
	pop	bx			; �ָ�ES��BX����ջ��
	pop	es
	ret
;--------------------------------------------------------------------
; ��������showbpb
;--------------------------------------------------------------------
; ���ã�; ��ʾ���̵�BPB��Ϣ
showbpb:
	call ReadPBootSec	; ���ö�����̷���������������

	mov word [lns], 0	; ��ǰ����ʾ��������ʼ��Ϊ0
	
	; ��ʾOEM��---------------------------------------------
	mov cx, OEMMsgLen	; CX=����
	mov bp, OEMMsg		; BP="OEM:"
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
	mov cx, 8			; CX=����=8
	mov bp, Sector + 3	; BP=BPB�е�OEM��
	call DispStr		; ������ʾ�ַ�������
	call newline		; �س�����
	inc word [lns]		; lns++ ����ʾ����+1

	; ��ʾ���ʴ�---------------------------------------------
	mov cx, MediaMsgLen	; CX=����
	mov bp, MediaMsg	; BP="Media:"
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
	cmp byte [Sector + 15h], 0F0h ; ���������� > F0h ?
	jg HD				; > ΪӲ��
	; ����
	mov cx, FDMsgLen	; CX=���̵Ĵ���
	mov bp, FDMsg		; BP="Floppy Disk"
	jmp DStr			; ��ת����ʾ��
HD: ; Ӳ��
	mov cx, HDMsgLen	; Ӳ�̵Ĵ���=9
	mov bp, HDMsg		; BP="Hard Disk"
DStr: ; ��ʾ��
	call DispStr		; ������ʾ�ַ�������
	call newline		; �س�����
	inc word [lns]		; lns++ ����ʾ����+1
	
	; ��ʾ�������� --------------------------------------------------------
	; ��ʾ��Size:����
	mov cx, SizeMsgLen	; CX=����
	mov bp, SizeMsg		; BP="Size:"
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��

	; ��ȡʮ�������ִ�
	mov ax, [Sector + 13h] ; AX=��������
	shr ax, 1			; ������/2 = KBֵ
	call GetDigStr		; ��AXΪ���ݲ�����BP(����ַ)��CX(�ַ�����)Ϊ����ֵ
	; ��ʾ���ִ�
	call DispStr		; ������ʾ�ַ�������
	
	; ��ʾ��KB����
	add dl, cl			; �к�DL += ʮ�������ִ����ַ�����
	inc dl				; DL++����һ��
	mov cx, KBMsgLen	; CX=����
	mov bp, KBMsg		; BP="KB"
	call DispStr		; ������ʾ�ַ�������
	call newline		; �س�����
	inc word [lns]		; lns++ ����ʾ����+1
	
	; ��ʾ�ļ�ϵͳ���ʹ�---------------------------------------------
	mov cx, FSMsgLen	; CX=����
	mov bp, FSMsg		; BP="File System:"
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
	mov cx, 8			; CX=����=8
	mov bp, Sector + 36h ; BP=EBPB�е��ļ�ϵͳ���ʹ�
	call DispStr		; ������ʾ�ַ�������
	call newline		; �س�����
	inc word [lns]		; lns++ ����ʾ����+1
	
	; ��ʾBPB�еľ�괮---------------------------------------------
	mov cx, VolMsgLen	; CX=����
	mov bp, VolMsg		; BP="Vol:"
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
	mov cx, 11			; CX=����=11
	mov bp, Sector + 2Bh ; BP=EBPB�е��ļ�ϵͳ���ʹ�
	call DispStr		; ������ʾ�ַ�������
	call newline		; �س�����
	inc word [lns]		; lns++ ����ʾ����+1
	
	; ��ʾID�����кţ�---------------------------------------------
	mov cx, IDMsgLen	; CX=����
	mov bp, IDMsg		; BP="Vol:"
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
	call showid			; ��ʾID��
	call newline		; �س�����	
	inc word [lns]		; lns++ ����ʾ����+1

	call newline		; �س�����
	inc word [lns]		; lns++ ����ʾ����+1

	ret					; ��ֹ���򣬷���
	
; �����ַ����������䳤��ֵ���ų�����	
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
showid: ; ��ʾ4B����IDֵ��ʮ�����ƴ�

	mov edx, [Sector + 27h] ; EDX = ID
	bswap edx		; �ֽڷ���

	mov cx, 4		; ѭ������
.1: ; ��ʾ�����ֽ�
	; ��ʾ��4λ
	mov al, dl		; AL=ID��λ�ֽ�
	and al, 0F0h	; ȡ����4λ
	shr al, 4		; AL >> 4
	call ShowChar	; ������ʾ�ַ�����
	; ��ʾ��4λ
	mov al, dl		; AL=ID��λ�ֽ�
	and al, 0Fh		; ȡ����4λ
	call ShowChar	; ������ʾ�ַ�����
	; ��һ���ֽ�
	shr edx, 8		; EDX >> 8
	cmp cx, 3		; CX = 3 ?
	jne .2			; ��= ����ѭ��
	; ��ʾ���ŷ�'-'
	mov al,'-'		; AL = �ո��
	mov ah,0Eh 		; ���ܺţ��Ե紫��ʽ��ʾ�����ַ���
	mov bl,0fh 		; ������
	int 10h 		; ����10H���ж�
.2:
	loop .1			; ѭ��

	ret				; �����̷���
; -------------------------------------------------------------------	

; -------------------------------------------------------------------	
; ��ʾ����ʮ�������ַ�����
ShowChar: ; ��ʾһ��ʮ���������ַ���0~9��A~F����ALΪ���ݲ�����
	cmp al, 10		; AL < 10 ?
	jl .1			; AL < 10����ת��.1
	add al, 7		; AL >= 10����ʾ��ĸ�� = ��ֵ += 37h��
.1: ; ����
	add al, 30h		; �����ַ� = ��ֵ+=30h
	mov ah, 0Eh		; ���ܺţ��Ե紫��ʽ��ʾ�����ַ���
	mov bl, 0fh 	; ������
	int 10h 		; ����10H���ж�
	ret				; �����̷���
; -------------------------------------------------------------------	

; --------------------------------------------------------------------
ReadPBootSec: ; ������̵ķ�������������Sector��
	mov bx, Sector 	; ES:BX=�������ݵ��ڴ��еĴ洢��ַ
	mov ah, 2 		; ���ܺ�
	mov al, 1 		; Ҫ�����������
	mov dl, [drvno]	; �����������ţ�0=����A��1=����B��80h=Ӳ��C��81h=Ӳ��D
	mov dh, 0 		; ��ͷ��
	mov ch, 0 		; ����ţ�����=0��Ӳ��=1��
	cmp byte[drvno], 1 ; �������� > 1 ? 
	jbe	.1			; <= 1 ʱΪ���̣������CH=0
	mov ch, 1		; > 1 ʱΪӲ�̣������CH=1
.1:
	mov cl, 1 		; ��ʼ�����ţ���Ŵ�1��ʼ��
	int 13H 		; ����13H���ж�
	ret 			; �����̷���
; ���建���������ڴ�ŴӴ��̶��������
Sector:
	resb 512

; --------------------------------------------------------------------
DispStr: ; ��ʾ�ַ������̣������ô���CX�ʹ���ַBP��
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	push cx			; ����CX����ջ��
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	pop cx			; �ָ�CX����ջ��

	; �ڵ�ǰλ����ʾ�ַ���������CX�ʹ���ַBP��Ԥ�����ú��ˣ�
	mov ah, 13h		; BIOS�жϵĹ��ܺţ���ʾ�ַ�����
	mov al, 1 		; ���ŵ���β
	mov bh, 0 		; ҳ��=0
	mov bl, 0fh		; �ַ���ɫ=������0���ڵף�000�������֣�1111��
	int 10h 		; ����10H����ʾ�ж�
	ret				; �����̷���
	
; --------------------------------------------------------------------
; ��ȡ������ֵʮ���ƴ�����
dn equ 5 ; ���λ��
GetDigStr: ; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	mov cx, 1		; λ��=1����ֵ��
	mov bp, sbuf	; BP = sbuf + dn - 1 = sbuf�ĵ�ǰλ��
	add bp, dn - 1
	mov bx,10		; ����=10
DLoop: ; ѭ����ʼ��
	mov dx, 0		; DX=0, DX:AX / BX -> ��AX����DX
	div bx
	add dl, 30h		; ���� + 30h = ��Ӧ�����ַ�ASCII��
	mov [bp], dl	; sbuf[BP] = DL
	cmp ax, 0		; ��AX = 0 ?
	je OutLoop		; = 0 ����ѭ��
	inc cx			; λ��CX++
	dec bp			; ���ַ��ĵ�ǰλ��BP--
	jmp DLoop		; ����ѭ��
OutLoop: ; �˳�ѭ��
	ret				; �����̷���

sbuf: resb dn ; ���ڴ��ʮ�������ִ��Ļ���������С = ����dn��=5��


;--------------------------------------------------------------------
; ��������ReadSec
;--------------------------------------------------------------------
; ���ã��ӵ� AX��������ʼ����CL����������ES:BX��
; ��ʹ�ô��̲���secspt(ÿ�ŵ�����������heads(��ͷ����
ReadSec:
	; ---------------------------------------------------------------
	; �������������������ڴ����е�λ�� (������->����š���ʼ��������ͷ��)
	; ---------------------------------------------------------------
	; ��������Ϊ x��= AX��
	;                             �� �����C = y / ��ͷ��
	;         x            �� �� y ��
	;   -------------- 	=> ��      �� ��ͷ��H = y % ��ͷ��
	;    ÿ�ŵ�������      ��
	;                      �� �� z => ��ʼ������S = z + 1
	push es
	push cx			; ����Ҫ����������CL
	push bx			; ����BX
	mov	bl, [secspt]; BL(= �ŵ���������Ϊ����
	div	bl			; AX/BL����y��AL�С�����z��AH��
	inc	ah			; z ++������̵���ʼ������Ϊ1����AH = ��ʼ������
	mov	cl, ah		; CL <- ��ʼ������S
	mov	ah, 0		; AX <- y
	mov bl, [heads]	; BL(= ��ͷ����Ϊ����
	div	bl			; AX/BL������AL�С�������AH��
	mov	ch, al		; CH <- �����C
	mov	dh, ah		; DH <- ��ͷ��H
	; ���ˣ�"����š���ʼ��������ͷ��"��ȫ���õ�
	pop	bx			; �ָ�BX
	pop ax			; AL = �ָ���Ҫ����������CL
	mov	dl, [drvno]	; ��������
.1: ; ʹ�ô����ж϶�������
	mov	ah, 2		; ���ܺţ���������
	int	13h			; �����ж�
	jc .1			; �����ȡ����CF�ᱻ��Ϊ1����ʱ�Ͳ�ͣ�ض���ֱ����ȷΪֹ
	pop es
	ret
;--------------------------------------------------------------------
; ��������WriteSec
;--------------------------------------------------------------------
; ���ã��ӵ� AX��������ʼ����ES:BX�� д��CL��������
; ��ʹ�ô��̲���secspt(ÿ�ŵ�����������heads(��ͷ����
WriteSec:
	; ---------------------------------------------------------------
	; �������������������ڴ����е�λ�� (������->����š���ʼ��������ͷ��)
	; ---------------------------------------------------------------
	; ��������Ϊ x��= AX��
	;                             �� �����C = y / ��ͷ��
	;         x            �� �� y ��
	;   -------------- 	=> ��      �� ��ͷ��H = y % ��ͷ��
	;    ÿ�ŵ�������      ��
	;                      �� �� z => ��ʼ������S = z + 1
	push es
	push cx			; ����Ҫ����������CL
	push bx			; ����BX
	mov	bl, [secspt]; BL(= �ŵ���������Ϊ����
	div	bl			; AX/BL����y��AL�С�����z��AH��
	inc	ah			; z ++������̵���ʼ������Ϊ1����AH = ��ʼ������
	mov	cl, ah		; CL <- ��ʼ������S
	mov	ah, 0		; AX <- y
	mov bl, [heads]	; BL(= ��ͷ����Ϊ����
	div	bl			; AX/BL������AL�С�������AH��
	mov	ch, al		; CH <- �����C
	mov	dh, ah		; DH <- ��ͷ��H
	; ���ˣ�"����š���ʼ��������ͷ��"��ȫ���õ�
	pop	bx			; �ָ�BX
	pop ax			; AL = �ָ���Ҫ����������CL
	mov	dl, [drvno]	; ��������
.1: ; ʹ�ô����ж϶�������
	mov	ah, 3		; ���ܺ� д����
	int	13h			; �����ж�
	jc .1			; �����ȡ����CF�ᱻ��Ϊ1����ʱ�Ͳ�ͣ�ض���ֱ����ȷΪֹ
	pop es
	ret
;--------------------------------------------------------------------
;--------------------------------------------------------------------
; ��������ls        
;��Ŀ¼�����޸�(��Ŀ¼���ļ�һ�������п��ܲ�������ԭ�����㷨���ɿ���Ҫȥ��fat���ټ�����һ����λ��)
;--------------------------------------------------------------------
; ���ã�; ��ʾ���̸�Ŀ¼�ļ���Ϣ�б�
; ��ʹ�ô��̲���secspt(ÿ�ŵ�����������heads(��ͷ����
ls: 
	;mov word[nsec],1
	;mov word[isec],1fh
	;add word[isec], 47h			
	
	;call getdiskparam	; ��ȡ���̲���H&S
	; ��ȡ���̲���H/S
	;mov ax, [Sector + 18h]	; AX = ÿ�ŵ�������
	;mov [secspt], ax		; secspt = AX = ÿ�ŵ�������
	;mov ax, [Sector + 1Ah]	; AX = ��ͷ��
	;mov [heads], ax			; heads = AX = ��ͷ��
	; ��Ӳ��isec��ӵ�1�������������
	mov ax,[isec]
	mov [isec_ls],ax
	mov ax,[nsec]
	mov [nsec_ls],ax
	cmp byte [drvno], 80h	; ��������=80h��Ӳ��C����
	je hdc					; = 80h ��ת
	jmp begain				; ����
hdc: ; Ӳ��C
	; �������ǰ�������������� = 1��������������= ÿ�ŵ������� * ��ͷ��
	mov ax, [secspt] 		; AX = ÿ�ŵ�������
	mul word [heads]		; AX *= ��ͷ�� = 1������������
	add [isec_ls], ax			; isec += 1������������ = Ӳ�̸�Ŀ¼��������

begain: 
	; �����ڴ��̵�ǰĿ¼��Ѱ���ļ�Ŀ¼��Ŀ
searchrdir: ; ������ǰĿ¼ѭ������������Ŀ¼������
	cmp	word [nsec_ls], 0	; �жϸ�Ŀ¼���Ƿ��Ѷ���
	jz	exit			; ���������˳�
	dec	word [nsec_ls]		; nsec--
	; ���ö�������������һ��Ŀ¼������������
	mov	bx, Sector		; BX = Sector
	mov	ax, [isec_ls]		; AX <- ��ǰĿ¼�еĵ�ǰ������
	mov cl, 1			; ��һ��������������
	call ReadSec		; ���ö���������
	
	mov	di, Sector		; ES:DI -> Sector	
	mov	word [i], 10h	; ѭ������=16��ÿ��������16���ļ���Ŀ��512/32=16��
searchfi: ; �����ļ���ѭ�����ڵ�ǰ�������������ļ�Ŀ¼�
	cmp	word [i], 0		; ѭ����������
	jz nextsec 			; ���Ѷ���һ������������һ����
	dec	word [i]		; �ݼ�ѭ������ֵ
	; �ж��Ƿ�Ϊ�ļ���Ŀ��0��ʼ��Ϊ���E5h��ʼ��Ϊ��ɾ����Ե�4λȫ1��
	; Ϊ���ļ������ϵͳռ�������������3��λΪ1��
	cmp	byte [di], 0	; �ļ���������ĸ=0��
	jz	notfi			; Ϊ��Ŀ¼��
	cmp	byte [di], 0E5h	; �ļ���������ĸ=E5��
	jz	notfi 			; Ϊ��ɾ��Ŀ¼��
	cmp	byte [di + 11], 0Fh; �ļ�����=0Fh��
	jz	notfi 			; Ϊ���ļ���Ŀ¼��

	; ��ʾ�ļ�����
	inc word [lns]		; ��ǰ��Ļ�ϵ��ļ���Ŀ��lns++
	Inc word [FileNum]
	; �ж��Ƿ�����Ļ�ײ�
	cmp word [lns], 30	; ���� = 30 ��
	jb .1				; < 24 ����
	mov word [lns], 1	; ����������ʾ����Ϊ1
	call waitforkey		; �����������
.1: ; ����
	; ��ʾ�ļ���Ŀ��Ϣ���ļ�������С��ʱ�䣩
	; ��ʾ�ļ�����
	mov bp, di			; BP=�ļ����ַ�������ʼ��ַ
	mov cx, 11			; �ļ�������8+3=11
	;�ж��Ƿ�Ϊ����
	push ax
	mov al,[di + 02h]
	cmp al,80h
	ja .Chin            ; �޷��Ŵ�С����
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
	jmp .DispEnd
.Chin
	mov cx,4            ;ֻ����ʾ�ĸ�����
	call DispStr_Chinese		; ������ʾ�ַ�������
	mov cx,4            ;ֻ����ʾ�ĸ�����
	add bp,8
	mov cx,3
	call DispStr		; ������ʾ�ַ�������
	call space			; ����ո��
.DispEnd
	pop ax
	; �Ծ�������ʾ�ļ���С����ʾ��ʶ��"<VOL>"
	mov al, [di + 0Bh]	; AL=�ļ�����
	and al, 8h			; AL & 8�����λ��
	jz .1.1	 			; ��Ϊ���
	; Ϊ��꣬��ʾ�ַ���"<VOL>"
	mov bp, volbuf		; ����ַ
	mov cx, fsbuflen + btbuflen + 1	; ����=�ļ���С�Ĵ���
	call DispStr		; ��ʾ�ַ���
	jmp .3				; ������ʾ�ļ���С��
.1.1:	
	; ����Ŀ¼�����ʾ�ļ���С����ʾ��ʶ��"<DIR>"
	cmp byte [di + 0Bh], 10h ; Ϊ��Ŀ¼��
	jne .2				; ��ʾ�ļ���С
	; ��ʾ�ַ���"<DIR>"
	mov bp, dsbuf		; ����ַ
	mov cx, fsbuflen + btbuflen + 1	; ����=�ļ���С�Ĵ���
	call DispStr		; ��ʾ�ַ���
	jmp .3				; ������ʾ�ļ���С��
	
.2: ; ���㲢��ʾ�ļ���Сʮ���ƴ�
	;push eax
	;push ebx
	;mov ebx,[FileSize] 
	;add eax,ebx ;�ļ��ܴ�С++
	;mov [FileSize],eax
	;pop ebx
	push eax
	mov eax, [di + 1Ch]; EAX = �ļ���С
	add [FileSize],eax ;�ļ���С++
	pop eax
	call getsizestr		; ��ȡ�ļ���Сʮ���ƴ�
	mov bp, fsbuf		; ����ַ
	mov cx, fsbuflen	; ����
	; ��ʾ�ļ���С�ַ���
	call DispStr		; ��ʾ�ַ���
	call space			; ����ո��
	; ��ʾ�ֽ��ַ������ļ���С��λ��"Byte"
	mov bp, btbuf		; ����ַ
	mov cx, btbuflen	; ����
	call DispStr		; ��ʾ�ַ���

.3: ; �������ɿո�ָ���
	call space			; ����ո��
	call space			; ����ո��
	call space			; ����ո��
	
	; ��ʾʱ�䣨������ʱ���룬��ʽΪ��yyyy.mm.dd  hh:mm:ss��
	; ��ʾ���ڣ���.��.�գ�
	mov ax, [di + 18h]	; AX = ���ڣ���5λΪ�ա���4λΪ�¡���7λΪ��-1980��
	push ax				; ����AX��ջ
	; ��ʾ�꣨��7λΪ��-1980��
	shr ax, 9			; AX >> 9��AX = �� - 1980
	add ax, 1980		; AX + 1980 = ��
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	call DispStr		; ��ʾ���ַ���
	; ��ʾ�£���4λΪ�£�
	pop ax				; ����AX = ����
	push ax				; ����AX��ջ
	shr ax, 5			; AX >> 5
	and ax, 0Fh			; AX & 1111 b = ��
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	cmp cx, 1			; ���� > 1 ��
	ja .4				; > 1����ת
	; = 1�������ַ�'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ��ǰ��'0'
	inc cx				; ����CX++
.4: ; ��Ӿ��ָ���'.'
	dec bp				; BP--
	mov byte [bp], '.'	; �Ӿ���'.'
	inc cx				; ����CX++
	call DispStr		; ��ʾ���ַ���
	; ��ʾ�գ���5λΪ�գ�
	pop ax				; ����AX = ����
	and ax, 1Fh			; AX & 1 1111 b = ��
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	cmp cx, 1			; ���� > 1 ��
	ja .5				; > 1����ת
	; = 1�������ַ�'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ��ǰ��'0'
	inc cx				; ����CX++
.5: ; ��Ӿ��ָ���'.'
	dec bp				; BP--
	mov byte [bp], '.'	; �Ӿ���'.'
	inc cx				; ����CX++
	call DispStr		; ��ʾ���ַ���
	call space			; ����ո��
	call space			; ����ո��

	; ��ʾʱ�䣨ʱ:��:�룩	
	mov ax, [di + 16h]	; AX = ʱ�䣨��5λΪ��/2����6λΪ�֡���5λΪʱ��
	push ax				; ����AX��ջ
	; ��ʾʱ����5λΪʱ��
	shr ax, 11			; AX >> 11��AX = ʱ
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	cmp cx, 1			; ���� > 1 ��
	ja .6				; > 1����ת
	; = 1�������ַ�'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ��ǰ��'0'
	inc cx				; ����CX++
.6:	
	call DispStr		; ��ʾʱ�ַ���
	; ��ʾ�֣���6λΪ�֣�
	pop ax				; ����AX = ʱ��
	push ax				; ����AX��ջ
	shr ax, 5			; AX >> 5
	and ax, 3Fh			; AX & 11 1111 b = ��
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	cmp cx, 1			; ���� > 1 ��
	ja .7				; > 1����ת
	; = 1�������ַ�'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ��ǰ��'0'
	inc cx				; ����CX++
.7: ; ���ð�ŷָ���':'
	dec bp				; BP--
	mov byte [bp], ':'	; ��ǰ��':'
	inc cx				; ����CX++
	call DispStr		; ��ʾ���ַ���
	; ��ʾ�루��5λΪ��/2��
	pop ax				; ����AX = ʱ��
	and ax, 1Fh			; AX & 1 1111 b = ��/2
	shl ax, 1			; AX << 1��AX*2 = ��
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	cmp cx, 1			; ���� > 1 ��
	ja .8				; > 1����ת
	; = 1�������ַ�'0'
	dec bp				; BP--
	mov byte [bp], '0'	; ��ǰ��'0'
	inc cx				; ����CX++
.8: ; ���ð�ŷָ���':'
	dec bp				; BP--
	mov byte [bp], ':'	; ��ǰ��':'
	inc cx				; ����CX++
	call DispStr		; ��ʾ���ַ���
	
	call newline		; �س�����
	
notfi:
	add	di, 20h			; DI += 20h ָ����һ��Ŀ¼��Ŀ��ʼ��
	jmp	searchfi		; ת��ѭ����ʼ��

nextsec:   ;������Ŀ¼nextsecҪ�Լ������(ֱ��ʹ��toDir�е��㷨)  ���Ŀ¼�㷨��ͬ
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader����������ַ=4000h��
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader��������ƫ�Ƶ�ַ=100h��
	mov ax,[isec_ls]
	sub ax,1fh
	call GetFATEntry	; ��ȡFAT���е���һ�غ�
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; �Ƿ���Ŀ¼������
	jae	exit ; ��FF8hʱ��ת���������һ����
	
	push ax
	mov ax,[temp_ax]
	mov	word [isec_ls],ax
	add	word [isec_ls],1fh 	; �޸ĳɼ������ʵ�������  
	pop ax
	jmp	searchrdir		; ��������Ŀ¼ѭ��
.root:
	inc word [isec_ls]  ;���ڸ�Ŀ¼��ֻ������
	jmp searchrdir		; ��������Ŀ¼ѭ��
exit: ; ��ֹ���򣬷���
.9:
	call newline
	mov bp,fileNumberBuf1
	mov cx,fileNumberBufLen1
	call DispStr
	
	call space			; ����ո��
	
	mov ax,[FileNum]
	call GetDigStr 		; ��AXΪ���ݲ�����[����ַ]BP��[�ַ�����]CXΪ����ֵ
	call DispStr
	
	call space			; ����ո��
	
	mov bp,fileNumberBuf2
	mov cx,fileNumberBufLen2
	call DispStr
	
	mov word[FileNum],0     ;��ʾ�����������
.10:
	push di
	mov di,FileSize-1ch
	call getsizestr		; ��ȡ�ļ���Сʮ���ƴ�
	pop di
	
	mov bp, fsbuf		; ����ַ
	mov cx, fsbuflen	; ����
	; ��ʾ�ļ���С�ַ���
	call DispStr		; ��ʾ�ַ���
	call space			; ����ո��
	; ��ʾ�ֽ��ַ������ļ���С��λ��"Byte"
	mov bp, btbuf		; ����ַ
	mov cx, btbuflen	; ����
	call DispStr		; ��ʾ�ַ���
	mov dword[FileSize],0     ;��ʾ�����������
	ret
temp_ax dw 0
isec_ls dw 0;��ǰ����������ls��	
nsec_ls dw 0;ʣ��������������ls��
isec dw 0	; ��ǰ������
nsec dw 0	; ʣ��������
lns dw 0	; ������������ֵΪ0
FileNum dw 0	; �ļ���������ֵΪ0
FileSize dd 0   ; �ļ��ܴ�С����ֵΪ0
secspt dw 0	; ÿ�ŵ�������
heads dw 0	; ��ͷ��

fsbuf db '0,987,654,321' ; �ļ���С��
fsbuflen equ $ - fsbuf ; ����
dsbuf db  '            <DIR>          ' ; ��Ŀ¼��ʶ��
;dsbuflen equ $ - dsbuf ; ����
volbuf db '            <VOL>          ' ; ����ʶ��
;volbuflen equ $ - volbuf
btbuf db 'Byte' ; �ֽ��ַ���
btbuflen equ $ - btbuf ; ����
fileNumberBuf1 db '  ALL'
fileNumberBufLen1 equ $-fileNumberBuf1
fileNumberBuf2 db 'files.'
fileNumberBufLen2 equ $-fileNumberBuf2
;--------------------------------------------------------------------
getsizestr: ; ��ȡ�ļ���Сʮ���ƴ�
	; �ÿո����20h�����fsbuf
	push di			; ����DI��ջ
	mov cx, fsbuflen; ѭ������CX=�����л�����fsbuf�ĳ���
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, fsbuf	; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	pop di			; ��ջ�ָ�DI

	; �����ļ���Сʮ���ƴ�
	mov cx, 0		; ��ǰ�ֶ����ָ�������ʼ��Ϊ0��
	mov bp, fsbuf	; BP = fsbuf + fsbuflen - 1 = fsbuf�ĵ�ǰλ��
	add bp, fsbuflen - 1 ; BP = ��β
	mov ebx,10		; ����=10
	mov eax, [di + 1Ch]; EAX = �ļ���С
	
.1: ; ѭ����ʼ��
	mov edx, 0		; EDX = 0
	div ebx			; EDX:EAX / EBX -> ��EAX����EDX
	add dl, 30h		; ���� + 30h = ��Ӧ�����ַ�ASCII��
	mov [bp], dl	; fsbuf[BP] = DL
	cmp eax, 0		; ��EAX = 0 ?
	je .2			; = 0 ����ѭ��
	dec bp			; ���ַ��ĵ�ǰλ��BP--
	inc cx			; ��ǰ�ֶ����ָ���++
	cmp cx, 3		; CX == 3 ��
	jne .1			; �� ����ѭ��
	; ��Ӷ��ŷָ���
	mov byte [bp], ',' ; ���붺�ŷָ�����,��
	dec bp			; ���ַ��ĵ�ǰλ��BP--
	mov cx, 0		; ������CX=0
	jmp .1			; ����ѭ��
.2: ; �˳�ѭ��
	ret				; �����̷���

;--------------------------------------------------------------------
waitforkey: ; �����������
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	; ��ʾ��ʾ��
	mov ah, 13h 	; BIOS�жϵĹ��ܺţ���ʾ�ַ�����
	mov al, 1 		; ���ŵ���β
	mov bh, 0 		; ҳ��=0
	mov bl, 0fh 	; �ַ���ɫ=������0���ڵף�000�������֣�1111��
	mov bp, pkinstr	; BP=����ַ
	mov cx, pkinstrlen; CX=����
	mov dl, 0		; �к�=0
	int 10h 		; ����10H����ʾ�ж�
	; �ȴ��û�����
	mov ah, 0		; ���ܺţ����ܼ����ַ����룩
	int 16h			; ����16h�����ж�
	
	call newline	; �س�����
	ret				; �����̷���

pkinstr db 'Press any key to continue!' ; ��ʾ�û�����Ĵ�
pkinstrlen equ $ - pkinstr ; ����

;--------------------------------------------------------------------
waitforkey_chin: ; �����������
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	inc dh
	; ��ʾ��ʾ��
	mov ah, 42h 	; BIOS�жϵĹ��ܺţ���ʾ�ַ�����
	mov al, 1 		; ���ŵ���β
	mov bh, 0 		; ҳ��=0
	mov bl, 0fh 	; �ַ���ɫ=������0���ڵף�000�������֣�1111��
	mov bp, pkinstr_chin	; BP=����ַ
	mov cx, pkinstrlen_chin; CX=����
	mov dl, 0		; �к�=0
	int 21h 		; ����10H����ʾ�ж�
	; �ȴ��û�����
	mov ah, 0		; ���ܺţ����ܼ����ַ����룩
	int 16h			; ����16h�����ж�
	
	call newline	; �س�����
	ret				; �����̷���

pkinstr_chin db '�����������'
pkinstrlen_chin equ ($ - pkinstr_chin)/2 ; ����
;--------------------------------------------------------------------
;----------------------------------------------------------------------------
; ��������ReadSector
;----------------------------------------------------------------------------
; ���ã��ӵ� AX��������ʼ����CL����������ES:BX��
ReadSector:
	; -----------------------------------------------------------------------
	; �������������������ڴ����е�λ�� (������->����š���ʼ��������ͷ��)
	; -----------------------------------------------------------------------
	; ��������Ϊ x
	;                           �� ����� = y >> 1
	;       x           �� �� y ��
	;   -------------- 	=> ��      �� ��ͷ�� = y & 1
	;  ÿ�ŵ�������     ��
	;                   �� �� z => ��ʼ������ = z + 1
	push bp
	mov	bp, sp
	sub	sp, 2 		; �ٳ������ֽڵĶ�ջ���򱣴�Ҫ����������: byte [bp-2]
	mov	byte [bp-2], cl
	push bx			; ����BX
	mov	bl, [BPB_SecPerTrk]	; BLΪ����
	div	bl			; AX/BL����y��AL�С�����z��AH��
	inc	ah			; z ++������̵���ʼ������Ϊ1��
	mov	cl, ah		; CL <- ��ʼ������
	mov	dh, al		; DH <- y
	shr	al, 1			; y >> 1 ���ȼ���y/BPB_NumHeads��������2����ͷ��
	mov	ch, al		; CH <- �����
	and	dh, 1		; DH & 1 = ��ͷ��
	pop	bx			; �ָ�BX
	; ���ˣ�"����š���ʼ��������ͷ��"��ȫ���õ�
	mov	dl, 0; �������ţ�0��ʾ����A��
.GoOnReading:
	mov	ah, 2			; ������
	mov	al, byte [bp-2]	; ��AL������
	int	13h			; �����ж�
	jc	.GoOnReading; �����ȡ����CF�ᱻ��Ϊ1��
					; ��ʱ�Ͳ�ͣ�ض���ֱ����ȷΪֹ
	add	sp, 2			; ջָ��+2
	pop	bp

	ret
;----------------------------------------------------------------------------
; ���͸������̽���
; -------------------------------------------------------------------
getstrln0: ; ��ȡ��������������
	cld				; ��������־λ��ʹɨ���ַ�������Ϊ�Ӵ��׵���β��
	
	; �ÿո����20h�����buf
	mov cx, buflen	; ѭ������CX=�����л�����buf�ĳ��ȣ�buflen=80��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, buf		; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	; �ÿո����20h�����fnbuf��ǰ8���ֽ�
	mov cx, cslen	; ѭ������CX=������ĳ��ȣ�cslen=8��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, fnbuf	; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	mov si, 0		; ��ǰ�ַ�ƫ��λ�� SI = 0
keyin0: ; ���ܼ�������
	; �����������صİ���ASCII����AL�У�
	mov ah, 0 		; ���ܺ�
	int 16h 		; ����16H���ж�
	; �Իس�����0DH����������
	cmp al, 0dh 	; �Ƚ�AL�еļ����ַ���س�����ASCII��Ϊ0DH��
	je return0 		; �����ת�������̷���
	cmp al, 08h
	je backspace0
	; ���水���ַ���buf
	mov [buf + si], al; buf[SI]=AL
	inc si			; SI++
	; ̫��ʱ����
	cmp si, 21	; SI >= 80 ?
	jae goout0		; >= ʱ��ת
	jmp next_k0
	
backspace0:
	cmp si,0        ;û��������ַ���ת��������
	je keyin0
	
	dec si
	mov byte [buf + si], 20h; ����ո�
	
	; ��ʾ�ַ������̣������ô���CX�ʹ���ַBP��
	; ��ȡ��ǰ���λ�ã����ص����кŷֱ���DH��DL�У�
	pusha
	mov cx,1       ; ����1
	mov bp,blank   ; ����ַ
	push cx			; ����CX����ջ��
	mov ah, 3		; ���ܺ�
	mov bh, 0		; ��0ҳ
	int 10h 		; ����10H����ʾ�ж�
	pop cx			; �ָ�CX����ջ��
	;10	2	�ù��λ��	BH=ҳ��
    ;DH,DL=��,��
	
	dec dl          ; �˸�
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	;dec dl          ; ����һ��
	; �ڵ�ǰλ����ʾ�ַ���������CX�ʹ���ַBP��Ԥ�����ú��ˣ�
	mov ah, 13h		; BIOS�жϵĹ��ܺţ���ʾ�ַ�����
	mov al, 1 		; ���ŵ���β
	mov bh, 0 		; ҳ��=0
	mov bl, 0fh		; �ַ���ɫ=������0���ڵף�000�������֣�1111��
	int 10h 		; ����10H����ʾ�ж�
	
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx	
	popa
	jmp keyin0
	; ��ʾAL�еļ����ַ�
return0:
	ret 			; �����̷���
	
next_k0
	pusha
	mov al,'*'
	mov ah, 0eh 	; ���ܺ�
	mov bh,0
	mov bl, 0fh 	; ������
	int 10h 		; ����10H���ж�
	popa
	jmp keyin0		; ѭ�������԰���
	
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
	call DispStr_Chinese         ;��ʾ����
	
	
	mov dh,25
	mov dl,2
	mov ah,2
	mov bh,0
	int 10h

	mov bl,0fh
	mov bp,OSver_str1
	mov cx,OSver_str1_len
	call DispStr_Chinese         ;��ʾ����
	
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	mov bh,0
	inc dl
	int 10h
	
	mov bp,OSver_str2
	mov cx,OSver_str2_len
	call DispStr 				 ;��ʾOS��Ϣ
	
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
	call DispStr_Chinese          ;��ʾ����������
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
	;У������
	mov si,passwordBuf
	mov di,passwordStr
	mov	cx, 16			; ��ʼѭ������Ϊ4 pin��
	repe cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
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
keyinPass db '����������'
keyinPasslen equ ($-keyinPass)/2

longPass db '����̫��'
longPasslen equ ($-longPass)/2
welcomePass db '��ӭʹ��'
welcomePasslen equ ($-welcomePass)/2	
wrongPass db '�������'
wrongPasslen equ ($-wrongPass)/2
spaceStr:    db '                        '
passwordBuf: db '                        '
passwordStr: db '1997                    '
; ------------------------------------------------------------------
CopyPassword:	    ; �����´������ --> ���룩
	mov si, buf		; Դ����ʼ��ַ
	mov di, passwordBuf	; Ŀ�Ĵ���ʼ��ַ
	mov cx, 21		; ѭ������ CX = n
	; �����뻺����buf�е�������Ƶ��ļ���������fnbuf��
	rep movsb		; CX > 0ʱ [ES:DI] = [DS:SI]��CX--��CX = 0ʱ�˳�ѭ��
	
	ret 			; �����̷���
	
passwordStr_temp1 db '                        '
passwordStr_temp2 db '                        '
; ===============================================================================
;-------------------------------------------------------------------------------
; �ı��û�����
;��ʾ��
PleaseEnterPassStr db '����������'
PleaseEnterPassStrLen equ ($-PleaseEnterPassStr)/2
PleaseEnterPassStr0 db '�ٴ�����������ȷ��'
PleaseEnterPassStrLen0 equ ($-PleaseEnterPassStr0)/2
PleaseEnterPassStr1 db '�������벻һ�£�����������'
PleaseEnterPassStrLen1 equ ($-PleaseEnterPassStr1)/2
ChangePassword:
;���ܱ䶯������
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
	call DispStr_Chinese          ;��ʾ����������1
	call newline
	;��ȡ����1
	call getstrln0
	call CopyPassword
	;���Ƶ�passwordStr_temp1
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
	call DispStr_Chinese          ;��ʾ����������2
	call newline
	;��ȡ����2
	call getstrln0
	call CopyPassword
	;���Ƶ�passwordStr_temp2
	mov si,passwordBuf
	mov di,passwordStr_temp2
	mov cx,16
	rep movsb
	
	
	;У������
	mov si,passwordStr_temp1
	mov di,passwordStr_temp2
	mov	cx, 16			; ��ʼѭ������Ϊ4 pin��
	rep cmpsb			; �ظ��Ƚ��ַ����е��ַ���CX--��ֱ������Ȼ�CX=0
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
	call DispStr_Chinese          ;��ʾ������Ϣ
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
	mov	cx, 16			; ����������
	rep movsb
	
	add sp,2
	call SignIn
	jmp again
;-------------------------------------------------------------------------------
; �ı��û���
;��ʾ��
PleaseEnterUserNameStr db '�������µ��û���'
PleaseEnterUserNameStrLen equ ($-PleaseEnterUserNameStr)/2
ChangeUserName:
	pusha
	; �ÿո����20h�����UserNameBuf
	mov cx, 16	; ѭ������CX=�����л�����buf�ĳ��ȣ�buflen=80��
	mov al, 20h		; AL=Ҫ���Ŀո��ASCII��
	mov di, UserNameBuf		; ES:DI=�ַ�������ʼ��ַ
	rep stosb		; CX>0ʱ��AL�洢��[ES:DI]��CX--��DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,5   ;����cuser  ����ַ�
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
;ͼ�θ�������
a_x dw 0
a_y dw 0
b_x dw 0
b_y dw 0
c_x dw 0
c_y dw 0
OSver_str1 db '����'
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

	call _dc      ;��ʾʵʱʱ��
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
drawRectangle: ;������ �Ⱥ�push������
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
drawLine:      ;��ʼ���� BX  �������� DX  �������� AX  CX���߷��� 0ˮƽ 1��ֱ
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