org 100h 		; 可编译成COM文件
; ===================================================================
; 命令行主程序开始
;--------------------------------------------------------------------	
	; 通过AX中转，将CS的值赋给DS、ES和SS
	mov ax, cs
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 100h - 4	; 置栈顶指针SP=100h-4
	mov ax,12h    ;640*480 mode
	int 10h       ;设置640*480/16色显示模式
	; 初始化内部命令例程入口地址
	mov word [cmdaddr], ver		; VER 显示版权信息
	mov word [cmdaddr + 2], cls	; CLS 清屏
	mov word [cmdaddr + 4], toa	; A:  切换到A盘
	mov word [cmdaddr + 6], tob	; B:  切换到B盘
	mov word [cmdaddr + 8], toc	; C:  切换到C盘
	mov word [cmdaddr + 10], dir; DIR 显示文件目录列表
	mov word [cmdaddr + 12], ls; LS  显示文件目录列表
	mov word [cmdaddr + 14], help; HELP 显示帮助
	mov word [cmdaddr + 16],cdToDir ;目录跳转
	mov word [cmdaddr + 18], _dt; dt 显示时间
	mov word [cmdaddr + 20], _dc; dc 显示时间	
	mov word [cmdaddr + 22], rename
	mov word [cmdaddr + 24], mkdir
	mov word [cmdaddr + 26], ReadMemmory
	mov word [cmdaddr + 28], HZK16_test
	mov word [cmdaddr + 30], SignIn
	mov word [cmdaddr + 32], Restart
	mov word [cmdaddr + 34], ChangePassword
	mov word [cmdaddr + 36], ChangeUserName
	; 设置中断向量（21h）
	xor ax, ax		; AX = 0
	mov fs, ax		; FS = 0
	mov word[fs:21h*4], int21h ; 设置21h号中断向量的偏移地址
	mov ax,cs 
	mov [fs:21h*4+2], ax ; 设置21h号中断向量的段地址=CS
	call Store_dc
	call getdiskparam	; 获取磁盘参数H&S（用于ReadSec和ls例程）
	call cls		; 清屏
	call initialDisk ;初始化ls用到的扇区信息
	call int213dh
	call cls
	call ver		; 显示版权信息
	;call waitforkey_chin
	call cls		; 清屏
	call SignIn
	call cls		; 清屏
	;call ver		; 显示版权信息
again: ; 命令行循环
	call BackToCmd
	;call ver0		; 显示版权信息
	call prompt		; 显示提示串
	call getstrln	; 获取键盘输入的命令串行
	call dtlen		; 确定命令串长度
	call tocap		; 转换成大写字母
	call newstr		; 构造新串
	call Shut_dc
	call iscmd		; 判断是否为内部命令，如果是，则执行之，否则：
	call newline	; 回车换行
	call exec		; 执行外部命令（COM文件）
	jmp again		; 继续循环
	
;--------------------------------------------------------------------
; 定义变量、数组、缓冲区和字符串

drvno db 0 ; 磁盘驱动器号：0=软盘A、1=软盘B、80h=硬盘C
i dw 0 ; 循环变量
n dw 0 ; 命令串长度

N equ 19	; 内部命令总数
cslen equ 8 ; 命令串最大长度

cmdstr: ; 内部命令串数组（统一串长为8，不足补空格符）
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
cmdHelpStr:  ;内部命令解释统一串长30
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
cmdHelpStr_chin:  ;内部命令解释统一串长30
	db '显示系统版本                  '
	db '清屏                          '
	db '改变目录至系统盘根目录        '
	db '改变目录至二号软盘根目录      '
	db '改变目录至硬盘根目录          '
	db '显示当前目录                  '
	db '显示所有当前目录文件条目      '
	db '显示帮助信息                  '
	db '改变当前目录                  '
	db '显示当前时间                  '
	db '实时时间                      '
	db '重命名文件                    '
	db '创建文件夹                    '
	db '读取内存                      '
	db '汉字演示                      '
	db '锁屏                          '
	db '重启                          '
	db '修改密码                      '
	db '修改用户名                    '
cmdaddr: ; 内部命令例程入口地址数组
	resw N

fnbuf: ; COM文件名串（8+3=11字符）
	db '12345678COM'

Dirbuf: ; 目录名串（8+3=11字符）
	db '           '
buflen: equ 80 ; 缓冲区长度=80

buf: resb buflen ; 命令行缓冲区
 
str1: ; 字符串1（版权信息串）
	db 'BigBoom-OS 2.0  (C) 2016 Big Firecrackers'
str1len equ $ - str1 ; 版权串长

str2: ; 字符串2数组（命令行提示串）
	db 'A:/$'
	resb 80   ;子目录缓冲区
str2len: dw 4 ; 提示串长

str3: ; 字符串3（出错信息串）
	db 'Wrong command!'
str3len equ $ - str3 ; 错误命令串长

str3_chin: ; 字符串3（出错信息串）
	db '不是内部或外部命令，也不是可运行的程序'
str3len_chin equ ($ - str3_chin)/2 ; 错误命令串长

str4: ; 字符串4（串太长信息串）
	db 'Too long!'
str4len equ $ - str4 ; 太长串长
str4_chin: ; 字符串3（出错信息串）
	db '命令太长'
str4len_chin equ ($ - str4_chin)/2 ; 错误命令串长

str10: ; 字符串5（出错信息串）
	db 'No such file or directory!'
str10len equ $ - str10 ; 错误命令串长
str10_chin: ; 字符串3（出错信息串）
	db '没有那个文件或目录'
str10len_chin equ ($ - str10_chin)/2 ; 错误命令串长

str11_chin: ; 字符串4（出错信息串）
	db '文件夹已存在'
str11len_chin equ ($ - str11_chin)/2 ; 重复条目
; -------------------------------------------------------------------
; 命令行主程序结束
; ===================================================================
; 小型辅助例程开始
; 动画=================================================================================================================
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



chi1 db '这是紫色'
chi2 db '这是绿色'
chi3 db '这是灰色'
chi4 db '这是红色'
chi5 db '这是粉色'	;0dh	
chi6 db '这是黄色'    ;0eh
chi7 db '这是蓝色'
chi8 db '这是白色'
chin_len EQU ($-chi8)/2


huanchong db 0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h,0f9h

ba db 0fh
color_char1 resb 1  ;显示颜色
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
	
	; mov dh,29	;行
	; mov dl, 30 		; 第10列
	; call displayhc
	
	;call displayhc
	
	
	mov bx,7  ;行
	mov dl,20 		; 第0列
	call displayhead
	
	mov bx,11	;行
	mov dl, 60 		; 第40列
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
	mov dh,23	;行
	mov dl, 30 		; 第10列
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
	mov bp, si 	; BP=串地址
	mov cx, 17	; 串长
	call display
	popa 
	ret	
	
	
displaysqu:
	pusha
	mov si,squ
	mov cx,10
.1:
	mov bp, si 	; BP=串地址
	mov dh,bl
	push cx
	mov cx, 8	; 串长
	
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
	mov bp, si 	; BP=串地址
	mov dh,bl
	push cx
	mov cx, 37	; 串长
	
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
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, [color_char1] 	; 亮白
	mov bh, 0 		; 第0页
	
	;mov dl, 0 		; 第0列
	;mov bp, head1 	; BP=串地址
	;mov cx, 37	; 串长
	int 10h 		; 调用10H号显示中断
	popa
	ret 
display1:	
	pusha
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	
	;mov dl, 0 		; 第0列
	;mov bp, head1 	; BP=串地址
	;mov cx, 37	; 串长
	int 10h 		; 调用10H号显示中断
	popa
	ret 
; =======================================================================动画结束==============================================================================
; ============================AH=3Dh==========================================
cc dw 0
; ============================AH=3Dh=======================================END
int21h: ; int 21h中断处理例程
	cmp ah,4ch
	jnz .1
; ============================AH=4ch==========================================
	mov al, 20h		; AL = EOI
	out 20h, al		; 发送EOI到主8529A
	out 0A0h, al	; 发送EOI到从8529A
	
	; 初始化段寄存器和栈指针
	mov ax, cs 		; 通过AX中转,  将CS的值传送给DS、ES和SS
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 100h - 4; 置栈顶指针SP=100h-4
	
	mov ah,0fh      ;读取显示模式
	cmp al,12h      ;是否是600*480 图形模式？
	jz .out
	;mov ax,12h      ;跳回图形模式
	;mov bh,0
	;mov bl,0fh
	;int 10h
	;mov ax,0bh     	;设置背景颜色
	;mov bh,0
	;mov bl,00h		;黑色背景
	;int 10h
.out
	jmp again		; 重新开始命令行循环
; ============================AH=4ch=======================================END
.1:
	cmp ah,3Dh
	jnz AH_3FH
; ============================AH=3Dh==========================================
	; 打开HZK16字库文件，将其加载到58000h位置，用于显示汉字
    ; 下面在A盘根目录中寻找 *.BIN
	iret
; ============================AH=3Dh=======================================END
AH_3FH:
	cmp ah,3fh
	jnz AH_42H
; ============================AH=3Fh==========================================
	;读字库文件  默认32字节
	;入口： CX=读取字节数   DS：DX=数据缓冲区地址   出口：无
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
	;原dos中断;移动已读取文件的指针  
	;;入口： CX：DX位移量
	;新cmd中断;INT21H AH=42h 在指定位置显示汉字 串地址=BP 串长=CX  行号DH 列号DL
	pusha
	push es
	push ds       ;保存原寄存器信息，尤其是es ds的地址
	mov ax,1000h
	mov es,ax
	mov ds,ax
	
	mov [color_char],bl   ;设置颜色
	;mov es,ax    ?恢复es ds 为cmd的段地址 
	;mov ds,ax
	xor ax,ax     ;像素点*16=行列号
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
	shl cx,1      ;CX*2==字符串字节数
	
	;ds=1000h di=disp_data
	mov di,disp_data
	mov si,bp
	;es=调用程序段 SI=BP不变
	pop ds        ;弹出程序所在段
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
	mov ax,1000h  ;再恢复es
	mov ds,ax    
	call HZK16_test
	pop ds
	pop es
	popa
	
	iret
DispStr_Chinese:;在当前位置显示汉字 串地址=BP 串长=CX  
	pusha
	push es
	push ds       ;保存原寄存器信息，尤其是es ds的地址
	mov ax,1000h
	mov es,ax
	mov ds,ax
	xor ax,ax     ;像素点*16=行列号
	
	push cx
	mov ah,3      ;获取当前光标位置
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
	shl cx,1      ;CX*2==字符串字节数

	mov di,disp_data
	mov si,bp
	;es=调用程序段 SI=BP不变
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
getdiskparam: ; 获取磁盘参数H/S
	call ReadPBootSec		; 调用读入磁盘分区引导扇区例程
	mov ax, [Sector + 18h]	; AX = 每磁道扇区数
	mov [secspt], ax		; secspt = AX = 每磁道扇区数
	mov ax, [Sector + 1Ah]	; AX = 磁头数
	mov [heads], ax			; heads = AX = 磁头数
	ret						; 从例程返回
	
; -------------------------------------------------------------------
newline: ; 换行（显示回车符和换行符）
	; 显示回车符CR（置当前列号=0）
	mov ah, 0Eh 	; 功能号
	mov al, 0Dh 	; 设置AL为回车符CR（ASCII码为0DH）
	mov bl, 0fh 	; 亮白字
	int 10h 		; 调用10H号显示中断
	; 显示换行符（当前行号++）
	mov ah, 0Eh 	; 功能号
	mov al, 0Ah 	; 设置AL为换行符LF（ASCII码为0AH）
	mov bl, 0fh 	; 亮白字
	int 10h 		; 调用10H号显示中断
	ret				; 从例程返回

; -------------------------------------------------------------------
space: ; 显示空格符
	mov ah, 0Eh 	; 功能号
	mov al, 20h 	; 设置AL为空格符SP（ASCII码为20H）
	mov bl, 0fh 	    ; 亮白字
	int 10h 		; 调用10H号显示中断
	ret			; 从例程返回
	
; -------------------------------------------------------------------
showwrong: ; 显示出错信息
	;call newline 	; 回车换行
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示出错信息串
	;mov ah, 13h 	; 功能号
	;mov al, 1 		; 光标放到串尾
	;mov bl, 0fh 	; 亮白
	;mov bh, 0 		; 第0页
	;mov dl, 0 		; 第0列
	;mov bp, str3 	; BP=串地址
	;mov cx, str3len	; 串长
	;int 10h 		; 调用10H号显示中断
	;mov cx,buflen
	mov bp,buf
	push bp    ;保存bp
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
	mov cx,si       ;计算命令串长
	pop si
	pop bp
	call DispStr
	mov ah, 0Eh 	; 功能号
	mov al, ':' 	; 设置AL为空格符SP（ASCII码为20H）
	mov bl, 0fh 	    ; 亮白字
	int 10h 		; 调用10H号显示中断
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	mov ah, 2		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
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
	ret				; 从例程返回
;--------------------------------------------------------------------
showError1: ;显示出错信息 提示串长=cx ,提示串偏移=bp
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示出错信息串
	
	mov ah, 42h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str10_chin	; BP=串地址
	mov cx, str10len_chin	; 串长
	int 21h 		; 调用10H号显示中断
	ret				; 从例程返回
;--------------------------------------------------------------------
showError2: ;显示出错信息 提示串长=cx ,提示串偏移=bp
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示出错信息串
	
	mov ah, 42h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str11_chin	; BP=串地址
	mov cx, str11len_chin	; 串长
	int 21h 		; 调用10H号显示中断
	ret				; 从例程返回
; -------------------------------------------------------------------
showtoolong: ; 显示太长信息
	call newline 	; 回车换行
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示太长信息串
	mov ah, 42h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str4_chin	; BP=串地址
	mov cx, str4len_chin	; 串长
	int 21h 		; 调用10H号显示中断
	ret				; 从例程返回
Store_dc:
	pusha
	mov bx, 0x70	; BX = 70h（中断号）
	shl bx, 2		; BX << 2（BX *= 4） 
	cli				; 关闭中断，防止改动期间发生新的0x70号中断
	; 设置70h号中断的新向量
	push es			; 保存ES入栈
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
	mov bx, 0x70	; BX = 70h（中断号）
	shl bx, 2		; BX << 2（BX *= 4） 
	cli				; 关闭中断，防止改动期间发生新的0x70号中断
	; 设置70h号中断的新向量
	push es			; 保存ES入栈
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
BackToCmd:      ;设置光标颜色
	pusha 
	;mov ax,0bh  
	;mov bh,00
	;mov bl,0h
	;int 10h
	popa
	call _dc
	ret
;--------------------------------------------------------------------
; 小型辅助例程结束
; ===================================================================

	
; ===================================================================
; 内部命令例程开始
;-------------------------------------------------------------------------------
; 文件名字符串
FileName_HZK:		db	"HZK16          " ; 字库文件名
BaseOfFile_HZK	dw	3000h; 字库文件被加载到的位置 ----  段地址
OffsetOfFile_HZK  equ 0h
Current_Base_HZK  dw 0
Current_Offset_HZK  dw 0
Original_Base_HZK dw 3000h
count db 0
BaseOfBuf_HZK		equ 8800h	; 用于查找文件条目的缓冲区 ---- 基地址
OffsetOfBuf_HZK	equ	0		; 用于查找文件条目的缓冲区 ---- 偏移地址
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
	push es		; 保护ES

; 软驱复位
	xor	ah, ah	; 功能号ah=0（复位磁盘驱动器）
	xor	dl, dl	; dl=0（软驱A，软驱B为1、硬盘和U盘为80h）
	int	13h		; 磁盘中断
	
; 下面在磁盘目录中寻找 字库文件
	;判断是根目录或者子目录
	push ax
	mov ax,[SectorNoOfCurrentDirectory] 	; 给表示当前扇区号的
	mov	word [wSectorNo], ax
						; 变量wSectorNo赋初值为当前目录区的首扇区号
	mov ax, [CurrentDirSectors]	; 剩余扇区数
	mov word [wRootDirSizeForLoop],ax
										; 初始化为当前目录所占扇区数，在循环中会递减至零
	pop ax
LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK:
	cmp	word [wRootDirSizeForLoop], 0 ; 判断根目录区是否已读完
	jz	LABEL_NOT_FOUND_HZK	; 若读完则表示未找到字库文件
	dec	word [wRootDirSizeForLoop]	; 递减变量wRootDirSizeForLoop的值
	; 调用读扇区函数读入一个目录扇区到装载区
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call ReadSec		; 调用读扇区函数

	mov	si, FileName_HZK		; DS:SI -> 字库文件
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; 清除DF标志位
						; 置比较字符串时的方向为左/上[索引增加]
	mov	dx, 10h			; 循环次数=16（每个扇区有16个文件条目：512/32=16）
LABEL_SEARCH_FOR_COM_FILE_HZK:
	cmp	dx, 0			; 循环次数控制
	jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR_HZK ; 若已读完一扇区
	dec	dx				; 递减循环次数值			  就跳到下一扇区
	mov	cx, 11			; 初始循环次数为11
LABEL_CMP_FILENAME_HZK:
	repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND_HZK ; 如果比较了11个字符都相等，表示找到
LABEL_DIFFERENT_HZK:
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
						; FFE0h = 1111111111100000（低5位=32=目录条目大小）
	add	di, 20h			; DI += 20h 下一个目录条目
	mov	si, FileName_HZK		; SI指向装载文件名串的起始地址
	jmp	LABEL_SEARCH_FOR_COM_FILE_HZK; 转到循环开始处

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR_HZK:             ;ssssss
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	LABEL_NOT_FOUND_HZK ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK		; 继续搜索目录循环
.root:
	inc	word [wSectorNo]	; 对于根目录，递增当前扇区号
	
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN_HZK

LABEL_NOT_FOUND_HZK:
	pop es			; 恢复ES
	;call showwrong	; 显示字符串
	;jmp $
	ret

; 下面将字库文件加载到内存
LABEL_FILENAME_FOUND_HZK:	; 找到 字库文件后便来到这里继续
	; 计算文件的起始扇区号
	mov	ax, [CurrentDirSectors]	; AX=当前目录占用的扇区数
	and	di, 0FFE0h		; DI -> 当前条目的开始地址
	add	di, 1Ah			; DI -> 文件的首扇区号在条目中的偏移地址
	mov cx, word [es:di] ; CX=文件的首扇区号
	push cx				; 保存此扇区在FAT中的序号
	add	cx, RootDirSectors			; CX=文件的相对起始扇区号+根目录占用的扇区数 +根目录占用的扇区数+根目录占用的扇区数+根目录占用的扇区数+根目录占用的扇区数+根目录占用的扇区数
	;重要的事情说一万遍=_=,找这个bug用了几小时   原代码add	cx,ax   现在子目录ax并不是根目录首扇区号
	add	cx, DeltaSectorNo ; CL <- COM文件的起始扇区号(0-based)
	mov	ax, [BaseOfFile_HZK]      ;+1C
	mov	es, ax			; ES <- BaseOfLoader（COM程序基址=4000h）
	mov	bx, OffsetOfFile_HZK ; BX <- OffsetOfLoader（COM程序偏移地址=100h）
	mov	ax, cx			; AX <- 起始扇区号
LABEL_GOON_LOADING_FILE_HZK:
	push bx				; 保存字库程序偏移地址
	mov	cl, 1			; 1个扇区
	call ReadSec		; 读扇区

	; 计算文件的下一扇区号
	pop bx				; 取出字库程序偏移地址
	pop	ax				; 取出此扇区在FAT中的序号
	call GetFATEntry	; 获取FAT项中的下一簇号
	cmp	ax, 0FF8h		; 是否是文件最后簇
	jae	LABEL_FILE_LOADED_HZK ; ≥FF8h时跳转，否则读下一个簇
	push ax				; 保存扇区在FAT中的序号
	mov	dx, RootDirSectors	; DX = 根目录扇区数
	add	ax, dx			; 扇区序号 + 根目录扇区数
	add	ax, DeltaSectorNo ; AX = 要读的数据扇区地址
	;add	bx, [BPB_BytsPerSec] ; BX+512指向字库的下一个扇区地址
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

; 下面跳转执行COM程序
LABEL_FILE_LOADED_HZK:
	pop es
	;add sp,2
	;jmp	BaseOfLoader:OffsetOfLoader	; 这一句跳转到已加载到内存中的
	ret
;------------------------------------------------------------------------------------------------
;21号中断 功能号ah=42h
line_char DW 2    ;在屏幕上第几行显示
col_char DW 2    ;在屏幕上第几列显示
color_char db 0FH  ;显示颜色    LRGB
HZK16_test:
  pusha
  jmp install
  disp_data1 DB  '廖维明'  
  disp_data resb 1024   ;汉字缓冲区，存放要显示的汉字
  disp_data_len dw 0   ;汉字字符串长
  ;chars EQU ($-disp_data)/2
  ;DISP_DATA_END EQU THIS BYTE
  zi_buffer resb 1280  ;一行可显示40个汉字,40*32b=1280byte
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
  
  call get_dots    ;读出汉字点针
  pop cx
  loop ins2
  call disp_cc    ;显示到屏幕
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
  mov bl,8     ;转化为字母列号
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
  sub ax,0a1a1h   ;汉字的内码从 A1区开始
  cwd  ;ax?╁??癲x锛宎x
  mov dl,al    ;所以绝对开始区是内码-A1
  mov al,ah    ;点阵在字库中的位置为 
  cbw
  mov bl,94    ;（（汉字码1-A1）* 94 + 汉字码2 - A1）* 32
  mul bl
  add ax,dx
  mov bx,32
  mul bx  ;   dx锛宎x
  mov cx,dx  ;cx锟?
  mov dx,ax  ;dx锟?
  ;mov ax,4200h    ;移动读写指针到点阵数据位置
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
	;mov es,ax    ;ES  DI为默认值
	
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
ReadMemmory_CHINESE:   ;显示 CX:DX处16字节的内存信息
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
hex2ascii:  ;16进制转ASCII（输入：AL = BCD码，输出：AX = ASCII）
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
	;------获取高四位---------;
	mov bx,0
	;shld bx,dx,4
	mov bx,dx
	shr bx,4
	and bl,0fh
	mov al,bl
	call ShowChar_HZK
	;------获取低四位---------;
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
Restart: ;重启操作系统
	int 19h
	ret
; -------------------------------------------------------------------	
; 显示单个十六进制字符函数
ShowChar_HZK: ; 显示一个十六进制数字符：0~9、A~F（以AL为传递参数）
	cmp al, 10		; AL < 10 ?
	jl .1			; AL < 10：跳转到.1
	add al, 7		; AL >= 10：显示字母（ = 数值 += 37h）
.1: ; 数字
	add al, 30h		; 数字字符 = 数值+=30h
	mov ah, 0Eh		; 功能号（以电传方式显示单个字符）
	mov bl, 0fh 	; 对文本方式置0
	int 10h 		; 调用10H号中断
	ret				; 从例程返回
; -------------------------------------------------------------------
;-------------------------------------------------------------------------------
new_int_0x70_dc: ; 70h号新中断处理程序入口
	; 保存将要使用的寄存器以免被破坏
	push ax
	push bx
	push es
	
  wait0: ; 判断是否可读日期与时间信息	
	; 此段代码对于更新周期结束中断来说是不必要的
	mov al, 0x0a	; 指定寄存器A
	or al, 0x80		; 阻断NMI。当然，通常是不必要的	   
	out 0x70, al	; 输出AL到端口70h，选择寄存器A
	in al, 0x71		; 读寄存器A
	test al, 0x80	; 测试第7位 = 0？ 
	jnz wait0		; ≠ 0时（日期与时间在更新中）需等待

	; 获取当前的时间信息
	; 获取秒信息
	xor al, al		; AL = 0
	out 0x70, al	; 指定存储单元地址
	in al, 0x71		; 读RTC当前时间(秒)
	push ax			; 将获取的数据AL压栈保存
	; 获取分信息
	mov al, 2		; AL = 2
	out 0x70, al	; 指定存储单元地址
	in al, 0x71		; 读RTC当前时间(分)
	push ax			; 将获取的数据AL压栈保存
	; 获取时信息
	mov al, 4		; AL = 4
	out 0x70, al	; 指定存储单元地址
	in al, 0x71		; 读RTC当前时间(时)
	push ax			; 将获取的数据AL压栈保存
	; 读取寄存器C
	mov al, 0x0c	; 指定寄存器C，且开放NMI 
	out 0x70, al	; 输出AL到端口70h，选择寄存器C
	in al, 0x71		; 读RTC的寄存器C，否则只发生一次中断
					; 此处不考虑闹钟和周期性中断的情况 
	
	; 在屏幕右上角显示时间信息
	; 置ES = 显存基址
	mov ax,0x1000	; AX = B800h（彩色文本屏幕显存的起始地址 >> 4）
	mov es,ax		; ES = AX = B800h（ES = 显存基址）
	; 设置时间串的起始位置
	mov bx, (0*80 + 72)*2; 从屏幕上的第0行72列开始显示
	mov ah,3
	mov bh,0
	int 10h
	mov [es:TEMP_DX],dx         ;保存光标位置
	mov ah,2
	mov dh,0
	mov dl,72    
	mov bh,0
	int 10h  		;设置光标位置为0 72
	; 显示时
	pop ax			; 从栈中弹出时
	call bcd2ascii	; 调用BCD转ASCII例程
	; 显示两位小时数字
	;mov [es:bx], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 2], al
	call ShowChar_dt
	; 显示分隔符':'
	mov al,':'
	;mov [es:bx + 4], al
	call ShowChar_dt
	; 显示分
	pop ax			; 从栈中弹出分
	call bcd2ascii	; 调用BCD转ASCII例程
	; 显示两位分钟数字
	;mov [es:bx + 6], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 8], al
	call ShowChar_dt
	; 显示分隔符':'
	mov al,':'
	;mov [es:bx + 10], al
	call ShowChar_dt
	; 显示秒
	pop ax			; 从栈中弹出秒
	call bcd2ascii	; 调用BCD转ASCII例程
	; 显示两位小时数字
	;mov [es:bx + 12], ah
	push ax
	mov al,ah
	call ShowChar_dt
	pop ax
	;mov [es:bx + 14], al
	call ShowChar_dt
	
	mov dx,[es:TEMP_DX]          ;恢复光标位置
	mov ah,2
	mov bh,0
	int 10h
	; 发送EOI给8259A
	mov al, 0x20	;中断结束命令EOI 
	out 0xa0, al	;向从片发送 
	out 0x20, al	;向主片发送 

	; 恢复保存的寄存器值
	pop es
	pop bx
	pop ax

	iret			; 从中断返回
TEMP_DX DW 0
; -------------------------------------------------------------------
_dt:
	pusha 
	; 获取年信息
	mov al, 9			; 年的偏移地址为9
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入年信息
	; 显示年信息
	call ShowBCD	; 显示BCD十进制数
	; 显示句点分隔符
	;mov al, '.'			; AL = '.'
	;call ShowChar_dt		; 显示字符
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
	;call DispStr_Chinese		; 显示字符串
	push ax
	mov bl,0fh          ;亮白色
	mov ah,42h
	int 21h
	pop ax
	; 获取月信息
	mov al, 8			; 月的偏移地址为8
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入月信息
	; 显示月信息
	call ShowBCD	; 显示BCD十进制数
	; 显示句点分隔符
	;mov al, '.'			; AL = '.'
	;call ShowChar_dt		; 显示字符
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
	;call DispStr_Chinese		; 显示字符串
	push ax
	mov bl,0fh          ;亮白色
	mov ah,42h
	int 21h
	pop ax
	
	; 获取日信息
	mov al, 7			; 日的偏移地址为7
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入日信息
	; 显示日信息
	call ShowBCD	; 显示BCD十进制数
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
	;call DispStr_Chinese		; 显示字符串
	push ax
	mov bl,0fh          ;亮白色
	mov ah,42h
	int 21h
	pop ax
	; 显示空格分隔符
	mov al, ' '			; AL = ' '
	call ShowChar_dt		; 显示字符
	
	; 获取星期信息
	mov al, 6			; 星期的偏移地址为6
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入星期信息
	; 显示星期信息
	dec al			; AL --
	mov bl, 6			; BL = 3
	mul bl			; AX = AL * BL
	add ax, weekstrs_chin	; AX += weekstrs
	mov bp, ax		; BP = AX 指向对应星期串
	mov cx, 3			; 串长 CX = 3
	push ax
	push cx
	mov ah,3
	mov bh,0
	int 10h
	pop cx
	pop ax
	mov dl,7
	;call DispStr_Chinese		; 显示字符串
	push ax
	mov bl,0fh          ;亮白色
	mov ah,42h
	int 21h
	pop ax
	; 显示空格分隔符
	mov al, ' '			; AL = ' '
	call ShowChar_dt		; 显示字符
	
	; 获取时信息
	mov al, 4			; 时的偏移地址为4
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入时信息
	; 显示时信息
	call ShowBCD	; 显示BCD十进制数
	; 显示冒号分隔符
	mov al, ':'			; AL = ':'
	call ShowChar_dt		; 显示字符

	; 获取分信息
	mov al, 2			; 分的偏移地址为2
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入分信息
	; 显示分信息
	call ShowBCD	; 显示BCD十进制数
	; 显示冒号分隔符
	mov al, ':'			; AL = ':'
	call ShowChar_dt		; 显示字符

	; 获取秒信息
	mov al, 0			; 秒的偏移地址为0
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入秒信息
	; 显示秒信息
	call ShowBCD	; 显示BCD十进制数
	; 设置光标位置
	mov ah, 2		; 功能号
	mov bh, 0		; 第0页
	mov dl, 0		; 列号
	int 10h			; 显示中断
	; 退回DOS
	popa	
	; 获取当前光标位置（返回的行列号分别在DH和DL中）  ;恢复行号方便newline使用
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	ret
date_str:
	db '年'
	db '月'
	db '日'
weekstrs: ; 定义星期串数组
	db 'Sun'
	db 'Mon'
	db 'Tue'
	db 'Wed'
	db 'Thu'
	db 'Fri'
	db 'Sat'
weekstrs_chin:
	db '星期日'
	db '星期一'
	db '星期二'
	db '星期三'
	db '星期四'
	db '星期五'
	db '星期六'
; -------------------------------------------------------------------
_dc:
	pusha
	; 设置70h新中断向量
	; 计算70h号中断在IVT中的偏移
	mov bx, 0x70	; BX = 70h（中断号）
	shl bx, 2		; BX << 2（BX *= 4） 
	cli				; 关闭中断，防止改动期间发生新的0x70号中断
	; 设置70h号中断的新向量
	push es			; 保存ES入栈
	xor ax, ax		; AX = 0
	mov es, ax		; ES = AX = 0
	mov word [es:bx], new_int_0x70_dc ; 偏移地址
	mov word [es:bx + 2], cs ; 段地址
	pop es			; 从栈中恢复ES

	; 设置RTC状态寄存器B
	mov al, 0x0b	; 指定RTC寄存器B
	or al, 0x80		; 阻断NMI 
	out 0x70, al	; 选择寄存器B
	mov al, 0x12	; 禁止周期性和闹钟中断，只开放更新结束后中断，采用BCD码和24小时制  00010010
	out 0x71, al	; 设置寄存器B 
	; 读取RTC状态寄存器C
	mov al, 0x0c	; 指定RTC寄存器C，开放NMI
	out 0x70, al	; 选择寄存器C
	in al, 0x71		; 读RTC寄存器C，复位未决的中断状态

	; 打开从8259A的IRQ0（RTC）中断
	in al, 0xa1		; 读从8259A的IMR寄存器 
	and al, 0xfe	; 清除bit0（此位连接RTC）
	out 0xa1, al	; 写回此寄存器 

	sti				; 重新开放中断 
	
	;jmp $			; 如果在DOS下运行，需用此死循环代替下面的退出DOS中断

	; 退回DOS
	popa
	
	ret
;--------------------------------------------------------------------
hex2bcd:  ;al ascii转为十六进制   al为其16进制数值
	cmp al, 3ah		; AL 是数字 ?
	jl .1			; AL < 10：跳转到.1
	sub al, 7		; AL >= 10：显示字母（ = 数值 += 37h）
.1: ; 数字
	sub al, 30h		; 
	ret
; -------------------------------------------------------------------
ReadMemmory:   ;显示 X:X处16字节的内存信息
	mov bp,buf
	add bp,8   ;跳过READMEM   8个字符
	mov cx,9
	push bp
	;call DispStr
	pop bp
	mov si,0
	mov cx,0   ;将冒号前4个ascii码转为数值放入cx
.1:  
	mov ax,0
	mov al,byte[bp]
	inc si
	inc bp
	call hex2bcd
	mov ah,al
	shl ah,4       ;将字符移动至高位
	shld cx,ax,4   ;每次移动四位,8140  8 1 4 0
	cmp si,4
	jz .2
	jmp .1
.2:
	mov si,0   ;将冒号后4个ascii码转为数值放入dx
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
	shld dx,ax,4   ;每次移动四位,8140  8 1 4 0
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
;显示 CX:DX处16字节的内存信息
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
	; 读取光标位置
	mov ah, 3		; 功能号
	int 10h
	; 设置光标位置
	mov ah, 2		; 功能号
	mov bh, 0		; 第0页
	int 10h			; 显示中断
	add sp,2
	jmp again	
; -------------------------------------------------------------------
ShowChar_dt: ; 显示单个字符（以AL为传递参数）
	mov ah, 0Eh		; 功能号（以电传方式显示单个字符）
	mov bl, 0fh 		; 亮白字
	int 10h 			; 调用10H号中断
	ret				; 从例程返回
;-------------------------------------------------------------------------------
show_hex2ascii:  ;显示16进制转ASCII（输入：AL = BCD码，输出：AX = ASCII）
	pusha
	mov dx,ax
	mov dh,dl
	push dx
	;------获取高四位---------;
	mov bx,0
	;shld bx,dx,4
	mov bx,dx
	shr bx,4
	and bl,0fh
	mov al,bl
	call ShowChar
	;------获取低四位---------;
	mov bx,0
	mov bx,dx
	and bl,0fh
	mov al,bl
	call ShowChar
	
	pop dx
	popa
	ret
;-------------------------------------------------------------------------------
bcd2ascii: ;BCD码转ASCII（输入：AL = BCD码，输出：AX = ASCII）
	mov ah, al		; AH = AL（分拆成两个数字）
	and al, 0x0f	; AL & 0Fh（取BCD的低4位数据）
	add al, 0x30	; AL += 30h（转换成ASCII）
	shr ah, 4		; AH >> 4（取BCD的高4位数据）
	add ah, 0x30	; AH += 30h（转换成ASCII）
	ret				; 从例程返回
; -------------------------------------------------------------------	
ShowBCD: ; 显示单字节BCD十进制数（以AL为传递参数）
	push ax			; 保存AL进栈
	shr al, 4			; AL >> 4 （高位数字）
	add al, 30h		; 数字字符 = 数值+=30h
	call ShowChar_dt		; 显示字符
	pop ax			; 从栈中恢复AL
	and al, 0Fh		; 取AL的低4位
	add al, 30h		; 数字字符 = 数值+=30h
	call ShowChar_dt		; 显示字符
	ret				; 从例程返回
; -------------------------------------------------------------------
ver: ; 显示版权信息
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示版权字符串 'MyOS 1.x  (C) 2016 CANNON OS'
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str1 	; BP=串地址
	mov cx, str1len	; 串长
	int 10h 		; 调用10H号显示中断
	ret				; 从例程返回

; -------------------------------------------------------------------
DispStr_HZK: ; 显示字符串
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断

	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	int 10h 		; 调用10H号显示中断
	ret				; 从例程返回

; -------------------------------------------------------------------
ver0: ; 显示版权信息
	pusha
	push dx
	mov dh,0
	; 显示版权字符串 'MyOS 1.x  (C) 2016 CANNON OS'
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str1 	; BP=串地址
	mov cx, str1len	; 串长
	int 10h 		; 调用10H号显示中断
	pop dx
	
	mov ah,02h
	mov bh,0
	int 10h
	popa
	ret				; 从例程返回
; -------------------------------------------------------------------
cls: ; 清屏
	mov	ah, 6		; 功能号
	mov	al, 0		; 滚动的文本行数（0=整个窗口）
	mov bh, 00h		; 设置插入空行的字符颜色为黑底亮白字
	mov cx, 0		; 窗口左上角的行号=CH、列号=CL
	mov dh, 30		; 窗口右下角的行号
	mov dl, 79		; 窗口右下角的列号
	int 10h 		; 调用10H号显示中断
	; 设置光标位置
	mov ah, 2		; 功能号
	mov bh, 0		; 第0页
	mov dh, 0		; 行号
	mov dl, 0		; 列号
	int 10h			; 显示中断
	ret				; 从例程返回
	
; -------------------------------------------------------------------
diskok: ; 判断切换到的目标磁盘是否存在（输入参数为DL=磁盘的驱动器号）
	; 利用磁盘的0号中断判断磁盘是否存在
	mov ah, 0		; 功能号=0：磁盘复位（出错置CF标志位）
	int 13h			; 调用13H号磁盘中断
	jc .1			; CF=1 磁盘不存在，切换磁盘失败
	; 磁盘存在时，返回切换磁盘例程
	ret				; 从例程返回
	
.1: ; 磁盘不存在时，显示出错信息后，退出循环，重新开始
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示磁盘不存在的信息 "Disk not exist!"
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str5 	; BP=串地址
	mov cx, str5len	; 串长
	int 10h 		; 调用10H号显示中断
	; 退出循环，重新开始
	add sp, 4		; 弹出两次call的返回地址
	jmp again		; 重新开始
	
str5: ; 字符串5（磁盘不存在信息串）
	db 'Disk not exist!'
str5len equ $ - str5 ; 磁盘不存在串长
; -------------------------------------------------------------------
toa: ; 改为A盘
	mov dl, 0		; 软盘A的驱动器号=0
	call diskok		; 如果磁盘不存在，就不切换磁盘，否则继续：
	mov byte [str2], 'A' ; 修改提示串首字母为A
	mov byte [drvno], 0 ; 设置驱动器号为0
	call getdiskparam	; 获取磁盘参数H&S（用于ReadSec和ls例程）
	add sp, 2		; 弹出call的返回地址

	;修改目录提示串
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	inc dh
	; 设置光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断

    call initialDisk
	
	jmp again		; 重新开始
	
; -------------------------------------------------------------------
tob: ; 改为B盘
	mov dl, 1		; 软盘B的驱动器号=1
	call diskok		; 如果磁盘不存在，就不切换磁盘，否则继续：
	mov byte [str2], 'B' ; 修改提示串首字母为B
	mov byte [drvno], 1 ; 设置驱动器号为1
	call getdiskparam	; 获取磁盘参数H&S（用于ReadSec和ls例程）
	add sp, 2		; 弹出call的返回地址
	;修改目录提示串
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	inc dh
	; 设置光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	
	call initialDisk
	
	jmp again		; 重新开始

; -------------------------------------------------------------------
toc: ; 改为C盘
	mov dl, 80h		; 硬盘C的驱动器号=80h
	call diskok		; 如果磁盘不存在，就不切换磁盘，否则继续：
	mov byte [str2], 'C' ; 修改提示串首字母为C
	mov byte [drvno], 80h ; 设置驱动器号为80h
	call getdiskparam	; 获取磁盘参数H&S（用于ReadSec和ls例程）
	add sp, 2		; 弹出call的返回地址
	;修改目录提示串
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	inc dh
	; 设置光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	
	call initialDisk
	
	jmp again		; 重新开始

;---------------------------------------------------------------------
initialDisk:;        ;初始化ls用到的参数
	pusha
	call ReadPBootSec
	; nsec为根目录区剩余扇区数，初始化为根目录扇区数，在循环中会递减至零
	; 计算根目录扇区数（ = 最大根目录项数 / 32）
	mov ax, [Sector + 11h]	; AX = 最大根目录项数
	shr ax, 4				; AX右移4位（~ /32） = 根目录扇区数
	mov word [nsec], ax		; nsec = AX = 根目录扇区数

	; isec为当前扇区号，赋初值为根目录区的首扇区号，在循环中会逐个增加
	; 计算根目录首扇区号（= 保留扇区数 + FAT数 * FAT占扇区数）
	movzx ax, byte [Sector + 10h] ; AX = FAT数
	mul word [Sector + 16h]	; AX *= FAT占扇区数
	add ax, [Sector + 0Eh]	; AX += 保留扇区数
	mov [isec],ax			; isec = AX = 根目录首扇区号
	popa
	ret
;--------------------------------------------------------------------
dir: ; 显示根目录文件
	call showbpb	; 显示磁盘信息
	call ls			; 显示磁盘文件信息列表
	ret				; 从例程返回
;--------------------------------------------------------------------
; 定义变量（磁盘参数）
CurrentDirSectors	dw	14		; 当前目录占用的扇区数
SectorNoOfCurrentDirectory	dw	19	; 当前目录区的首扇区号
SectorNoOfLastDirectory	dw	19	; 上一目录区的首扇区号
iCurrentDirSectors  dw 0       ;待计算的下一目录所占扇区数
Dir_len dw 0					;要cd的目录长度
isBackBool dw 0					;是否是跳回上一级..=2 或者 同级.=1
NumOfSign dw 0					;目录分隔符的个数
cdToDir:   ;跳至子目录    X:/$子目录            大致的思路是:在当前扇区找子目录条目，找到则继续查看条目中扇区号，再去找ＦＡＴ表计算出下一级目录的大小，修改当前扇区变量SectorNoOfCurrentDirectory　以及CurrentDirSectors
	push ax
	mov ax,[SectorNoOfCurrentDirectory]      ;保存计算下一扇区前的扇区号
	mov [SectorNoOfLastDirectory],ax
	pop ax
	pusha
	; 用空格符（20h）填充Dirbuf
	mov cx, 11	; 循环次数CX=命令行缓冲区buf的长度（buflen=80）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, Dirbuf		; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,3   ;跳过cd  三个字符
	cmp byte[bp],'\'
	jz backToRoot
	push bp    ;保存bp
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
	mov bp,Dirbuf      ;获得要跳转目录的目录名
	mov cx,11
	;call DispStr
	;判断是否为. 或者 ..
	mov bp,Dirbuf
	mov si,0
.isBack:
	cmp byte[bp],'.'
	jnz .isBackEnds    ;不是点退出来
	inc si
	inc bp
	jmp .isBack
.isBackEnds:
	;cmp si,2           ;一个是同级目录，两个返回上一级
	mov [isBackBool],si ;保存判断结果，修改提示串时用到
	cmp word[isBackBool],1   ;一个点还是同一级目录，无需改变目录提示串
	jz cd_ends
	cmp word[isBackBool],2  ; 两个点代表上一级目录
	jnz .cd_start
	pusha               ;设置新的目录 提示串
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	;add di,[str2len]
	dec di      ;减去$符号
	;计算/的个数，以判断是否为一级目录
	mov cx,[str2len]
	mov word [NumOfSign],0     ;初始化计数器
.11
	cmp byte[di],'/'
	jnz .11.1
	inc word[NumOfSign]
.11.1:
	inc di
	loop .11
	popa                       ;先pop出来好进行判断跳转
	push ax
	mov ax,ds
	mov es,ax
	pop ax
	cmp word [NumOfSign],2     ;代表上一级已经是根目录
	jz backToRoot
.cd_start:
;-------------------------------------------------------------------1
	push es		; 保护ES

; 软驱复位
	xor	ah, ah	; 功能号ah=0（复位磁盘驱动器）
	xor	dl, dl	; dl=0（软驱A，软驱B为1、硬盘和U盘为80h）
	int	13h		; 磁盘中断
	
; 下面在当前目录中寻找子目录
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; 给表示当前扇区号的
						; 变量wSectorNo赋初值为根目录区的首扇区号（=19）
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; 根目录区剩余扇区数
										; 初始化为14，在循环中会递减至零
LABEL_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; 判断根目录区是否已读完
	jz	VOL_NOT_FOUND	; 若读完则表示未找到目录项
	dec	word [wRootDirSizeForLoop]	; 递减变量wRootDirSizeForLoop的值
	; 调用读扇区函数读入一个根目录扇区到装载区
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call ReadSec		; 调用读扇区函数

	mov	si, Dirbuf		; DS:SI -> 目录项
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; 清除DF标志位
						; 置比较字符串时的方向为左/上[索引增加]
	mov	dx, 10h			; 循环次数=16（每个扇区有16个文件条目：512/32=16）
VOL_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; 循环次数控制
	jz LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR ; 若已读完一扇区
	dec	dx				; 递减循环次数值			  就跳到下一扇区
	mov	cx,11 	; 初始循环次数为11
VOL_CMP_FILENAME:
	repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
	cmp	cx, 0
	jz	LABEL_VOL_FOUND ; 如果比较了11个字符都相等，表示找到
VOL_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
						; FFE0h = 1111111111100000（低5位=32=目录条目大小）
	add	di, 20h			; DI += 20h 下一个目录条目
	mov	si, Dirbuf		; SI指向装载文件名串的起始地址
	jmp	VOL_SEARCH_FOR_VOL_FILE; 转到循环开始处

LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR: ;对于子目录LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR要自己算出来(直接使用toDir中的算法)  与根目录算法不同
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	exit_cd ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	LABEL_SEARCH_IN_Current_DIR_BEGIN		; 继续搜索目录循环
.root:
	inc	word [wSectorNo]	; 对于根目录，递增当前扇区号
	jmp	LABEL_SEARCH_IN_Current_DIR_BEGIN
exit_cd:			;没有子目录导致 跳转失败直接退出
	pop es			; 恢复ES
	call showError1	; 显示字符串
	jmp cd_ends
VOL_NOT_FOUND:     
	pop es			; 恢复ES
	call showError1	; 显示字符串
	jmp dir_out
;------------------------------------------------------+
LABEL_VOL_FOUND:           ;aaa
	mov word [iCurrentDirSectors],0  ;占用扇区数 计数器清零
	; 计算文件的起始扇区号
	mov	ax, [CurrentDirSectors]	; AX=当前目录占用的扇区数
	and	di, 0FFE0h		; DI -> 当前条目的开始地址
	add	di, 1Ah			; DI -> 子目录的首扇区号在条目中的偏移地址
	mov cx, word [es:di] ; CX=子目录的首扇区号
	mov word[SectorNoOfCurrentDirectory],cx ;修改当前目录的首扇区号
	pop es
	
	
	mov word [nsec],1
	mov word [isec],cx
	add word [isec],1fh
	;jmp VOL_FILE_LOADED
	pusha
	push cx				; 保存此扇区在FAT中的序号
	add	cx, ax			; CX=文件的相对起始扇区号+当前目录占用的扇区数
	add	cx, DeltaSectorNo ; CL <- 目录项的起始扇区号(0-based)
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（COM程序基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（COM程序偏移地址=100h）
	mov	ax, cx			; AX <- 起始扇区号	
VOL_GOON_SETTING_PARAM: ;设置当前目录的扇区数和首扇区号
	push bx				; 保存COM程序偏移地址
	mov	cl, 1			; 1个扇区
	call ReadSec		; 读扇区
	inc word [iCurrentDirSectors]
	; 计算子目录所占的扇区数
	pop bx				; 取出目录偏移地址
	pop	ax				; 取出此扇区在FAT中的序号
	call GetFATEntry	; 获取FAT项中的下一簇号
	cmp	ax, 0FF8h		; 是否是目录的最后簇
	jae	VOL_FILE_LOADED ; ≥FF8h时跳转，否则读下一个簇
	push ax				; 保存扇区在FAT中的序号
	mov	dx, [CurrentDirSectors] ; DX = 当前目录扇区数
	add	ax, dx			; 扇区序号 + 当前目录扇区数
	add	ax, DeltaSectorNo ; AX = 要读的数据扇区地址
	add	bx, [BPB_BytsPerSec] ; BX+512指向子目录条目区的下一个扇区地址
	
	jmp VOL_GOON_SETTING_PARAM
VOL_FILE_LOADED:
	;jmp nextdir
	mov ax,[iCurrentDirSectors]
    mov word [CurrentDirSectors],ax
	push cx
	mov cx,[iCurrentDirSectors]
	pusha
	; 显示高4位
	mov al, cl		; AL=ID高位字节
	and al, 0F0h	; 取出高4位
	shr al, 4		; AL >> 4
	;call ShowChar	; 调用显示字符函数
	; 显示低4位
	mov al, cl		; AL=ID高位字节
	and al, 0Fh		; 取出低4位
	;call ShowChar	; 调用显示字符函数
	popa
	pop cx
nextdir:
	popa
	cmp word[isBackBool],2  ; 两个点代表上一级目录
	jz .upDir
	pusha               ;设置新的目录 提示串
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	add di,[str2len]
	dec di      ;减去$符号
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
.upDir:                 ;返回上级目录的判断,注意的是要判断上一级是不是根目录
	;是否是返回根目录在前面判断
	;到达这一步的,提示串要减少一级
	pusha
	mov ax,ds
	mov es,ax
	mov si,[Dir_len]
	mov di,str2
	add di,[str2len]
	std                         ;倒着清空
	mov al,20h                  ;清掉/$
	stosb
	mov al,20h                 
	stosb
	dec di
.2
	cmp byte[di],'/'
	jz .2.1                     ;遇到第一个/退出来
	mov al,20h                  ;否则清空
	stosb
	dec word [str2len]
	jmp .2
.2.1:
	cld
	inc di
	mov al,'$'                  ;添加$
	stosb
	dec word [str2len]
	popa
	jmp dir_out
backToRoot:
	call getdiskparam	; 获取磁盘参数H&S（用于ReadSec和ls例程）
	mov word [CurrentDirSectors],RootDirSectors
	mov word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	;mov byte [drvno], 0 ; 设置驱动器号为0
	
	mov di,str2
	add di,3
	mov al,'$'
	stosb
	mov byte[str2len],4
	call initialDisk
dir_out:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;清空目录Dirbuf
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
	; 显示高4位
	mov al, cl		; AL=ID高位字节
	and al, 0F0h	; 取出高4位
	shr al, 4		; AL >> 4
	;call ShowChar	; 调用显示字符函数
	; 显示低4位
	mov al, cl		; AL=ID高位字节
	and al, 0Fh		; 取出低4位
	;call ShowChar	; 调用显示字符函数
	popa
	;call judgeRootTODir
	;判断是否从是根目录到子目录的切换
	cmp word [SectorNoOfLastDirectory],SectorNoOfRootDirectory
	jz .1     ;上次目录是根目录
	jmp .2
.1	cmp word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .2.2       ;从根目录跳至子目录+1fh
	add word[SectorNoOfCurrentDirectory],1fh       ;为什么加1fh？ 子目录文件条目中的扇区号是相对于数据区起始位置的，得到物理扇区号的方法是+1fh（3e00h）  为什么是这么多=_= 
	;要读的数据扇区地址 = 扇区序号 + 根目录扇区数 + DeltaSectorNo           根目录扇区数 + DeltaSectorNo = 17+14 =1fh
	jmp .2.2 
.2:	;原本在子目录
	cmp word [SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .2.2     ;现在是根目录不+1fh
	add word[SectorNoOfCurrentDirectory],1fh 
.2.2:	
	mov ax,[SectorNoOfCurrentDirectory]
	mov [isec],ax
	jmp cd_ends
cd_error:
	call showError1	; 显示字符串
cd_ends:	
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	inc dh
	; 设置光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	add sp,2
	
	jmp again
;--------------------------------------------------------------------
judgeRootTODir:   
	
	ret
;--------------------------------------------------------------------	
FileName_rename_len dw 0
FileSuffixes_len dw 0
FileName_renameTooLongStr db '文件名太长，无法修改你造吗'
FileName_renameTooLong_len  equ ($ - FileName_renameTooLongStr)/2
RenameFile_DI dw 0
IsNotDir dw 1
rename:
	mov word [IsNotDir],1
	pusha
	; 用空格符（20h）填充Dirbuf
	mov cx, 11	; 循环次数CX=命令行缓冲区buf的长度（buflen=80）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, Dirbuf		; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	mov word [Dir_len],11 ;文件条目->文件名为11字节
	
	mov cx,buflen
	mov bp,buf
	add bp,7  ;跳过rename  6个字符
	;在此可检测输入合法性
	push bp    ;保存bp
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
	
	cmp word[IsNotDir],1     ;子目录跳过文件后缀检测
	jnz .4.1
	push bp
	mov [FileName_rename_len],si
	add bp,si            ;sp指向后缀
	add bp,[IsNotDir]             ;对于文件跳过. 对于子目录不变
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
	mov di,Dirbuf+8 ;偏移到文件后缀名
	cld
	mov cx,si
	mov si,bp
	add si,[FileName_rename_len]	  ;定位到buf里的文件后缀
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
;	mov di,Dirbuf            ;清空目录Dirbuf 后缀(匹配目录有些bug,只好清掉后缀)
;	add di,[FileName_rename_len]
;.next_10:
	;xor ah,ah
	;int 16h
;-------------------------------------------------------------------1
	push es		; 保护ES

; 软驱复位
	xor	ah, ah	; 功能号ah=0（复位磁盘驱动器）
	xor	dl, dl	; dl=0（软驱A，软驱B为1、硬盘和U盘为80h）
	int	13h		; 磁盘中断
	
; 下面在当前目录中寻找子目录
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; 给表示当前扇区号的
						; 变量wSectorNo赋初值为根目录区的首扇区号（=19）
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; 根目录区剩余扇区数
										; 初始化为14，在循环中会递减至零
RENAME_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; 判断根目录区是否已读完
	jz	RENAME_NOT_FOUND	; 若读完则表示未找到目录项
	dec	word [wRootDirSizeForLoop]	; 递减变量wRootDirSizeForLoop的值
	; 调用读扇区函数读入一个根目录扇区到装载区
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call ReadSec		; 调用读扇区函数

	mov	si, Dirbuf		; DS:SI -> 目录项
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; 清除DF标志位
						; 置比较字符串时的方向为左/上[索引增加]
	mov	dx, 10h			; 循环次数=16（每个扇区有16个文件条目：512/32=16）
RENAME_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; 循环次数控制
	jz RENAME_GOTO_NEXT_SECTOR_IN_Current_DIR ; 若已读完一扇区
	dec	dx				; 递减循环次数值			  就跳到下一扇区
	mov	cx,[Dir_len] 	; 初始循环次数为11
RENAME_CMP_FILENAME:
	repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
	cmp	cx, 0
	jz	RENAME_VOL_FOUND ; 如果比较了11个字符都相等，表示找到
RENAME_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
						; FFE0h = 1111111111100000（低5位=32=目录条目大小）
	add	di, 20h			; DI += 20h 下一个目录条目
	mov	si, Dirbuf		; SI指向装载文件名串的起始地址
	jmp	RENAME_SEARCH_FOR_VOL_FILE; 转到循环开始处

RENAME_GOTO_NEXT_SECTOR_IN_Current_DIR: ;对于子目录LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR要自己算出来(直接使用toDir中的算法)  与根目录算法不同
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	exit_rename ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	RENAME_SEARCH_IN_Current_DIR_BEGIN		; 继续搜索目录循环
.root:
	inc	word [wSectorNo]	; 对于根目录，递增当前扇区号
	jmp	RENAME_SEARCH_IN_Current_DIR_BEGIN

RENAME_NOT_FOUND:
	pop es			; 恢复ES
exit_rename:
	call showError1	; 显示字符串
	jmp out_rename
;------------------------------------------------------+
RENAME_VOL_FOUND
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
	mov [RenameFile_DI],di
	;简单的debug
	
	pop es
	;pusha
	;mov cx,11
	;mov bp,di
	;call DispStr
	;popa
	
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;清空目录Dirbuf
	rep stosb
	
	mov bp,buf
	add bp,7  ;跳过rename  6个字符

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
	;在此可检测输入合法性
	push bp    ;保存bp
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
	
	cmp word[IsNotDir],1     ;子目录跳过文件后缀检测
	jnz .4.1
	push bp
	mov [FileName_rename_len],si
	add bp,si            ;sp指向后缀
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
	mov di,Dirbuf+8 ;偏移到文件后缀名
	cld
	mov cx,si
	mov si,bp
	add si,[FileName_rename_len]	  ;定位到buf里的文件后缀
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
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov di,[RenameFile_DI]
	
	mov si,Dirbuf
	mov cx,11
	repe movsb			; 写缓冲区，修改对应文件条目
	
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call WriteSec		; 调用写扇区函数
	
	pop es
	;Dirbuf
	jmp out_rename
rename_tolong:;串太长
	mov cx,FileName_renameTooLong_len
	mov bp,FileName_renameTooLongStr
	call DispStr_Chinese
out_rename:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;清空目录Dirbuf
	rep stosb
	mov word[Dir_len],0
	
	add sp,2
	jmp again
;--------------------------------------------------------------------	
mkdir:
	;mov cx,buflen
	;mov bp,buf
	;call DispStr
	;在FAT表中找未使用的项目，再用写扇区中断写子目录扇区buf，并在当前目录添加一个子目录项
		push ax
	mov ax,[SectorNoOfCurrentDirectory]      ;保存计算下一扇区前的扇区号
	mov [SectorNoOfLastDirectory],ax
	pop ax
	pusha
	; 用空格符（20h）填充Dirbuf
	mov cx, 11	; 循环次数CX=命令行缓冲区buf的长度（buflen=80）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, Dirbuf		; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,6   ;跳过mkdir   六个字符
	push bp    ;保存bp
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
	mov bp,Dirbuf      ;获得要跳转目录的目录名
	mov cx,11
	call DispStr
	
	.cd_start:
;-------------------------------------------------------------------1
	push es		; 保护ES

; 软驱复位
	xor	ah, ah	; 功能号ah=0（复位磁盘驱动器）
	xor	dl, dl	; dl=0（软驱A，软驱B为1、硬盘和U盘为80h）
	int	13h		; 磁盘中断
	
; 下面在当前目录中寻找子目录
	mov ax,[SectorNoOfCurrentDirectory]
	;cmp ax,SectorNoOfRootDirectory
	;jz .1.1
	;add ax,1fh
;.1.1
	mov	word [wSectorNo], ax 	; 给表示当前扇区号的
						; 变量wSectorNo赋初值为根目录区的首扇区号（=19）
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; 根目录区剩余扇区数
										; 初始化为14，在循环中会递减至零
MKDIR_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; 判断根目录区是否已读完
	jz	MKDIR_NOT_FOUND	; 若读完则表示未找到目录项
	dec	word [wRootDirSizeForLoop]	; 递减变量wRootDirSizeForLoop的值
	; 调用读扇区函数读入一个根目录扇区到装载区
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call ReadSec		; 调用读扇区函数

	mov	si, Dirbuf		; DS:SI -> 目录项
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; 清除DF标志位
						; 置比较字符串时的方向为左/上[索引增加]
	mov	dx, 10h			; 循环次数=16（每个扇区有16个文件条目：512/32=16）
MKDIR_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; 循环次数控制
	jz MKDIR_GOTO_NEXT_SECTOR_IN_Current_DIR ; 若已读完一扇区
	dec	dx				; 递减循环次数值			  就跳到下一扇区
	mov	cx,11 	; 初始循环次数为11
MKDIR_CMP_FILENAME:
	repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
	cmp	cx, 0
	jz	MKDIR_VOL_FOUND ; 如果比较了11个字符都相等，表示找到
MKDIR_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
						; FFE0h = 1111111111100000（低5位=32=目录条目大小）
	add	di, 20h			; DI += 20h 下一个目录条目
	mov	si, Dirbuf		; SI指向装载文件名串的起始地址
	jmp	MKDIR_SEARCH_FOR_VOL_FILE; 转到循环开始处

MKDIR_GOTO_NEXT_SECTOR_IN_Current_DIR: ;对于子目录LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR要自己算出来(直接使用toDir中的算法)  与根目录算法不同
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	exit_mk ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	MKDIR_SEARCH_IN_Current_DIR_BEGIN		; 继续搜索目录循环
.root:
	inc	word [wSectorNo]	; 对于根目录，递增当前扇区号
	jmp	MKDIR_SEARCH_IN_Current_DIR_BEGIN
exit_mk:			;没有子目录导致 跳转失败直接退出
MKDIR_NOT_FOUND:    ; 没有找到，则可以添加条目
	pop es			; 恢复ES
	
	call CreateDir  ;调用生成目录函数
	
	jmp mk_out
;------------------------------------------------------+
MKDIR_VOL_FOUND:     ; 找到了,则无法创建子目录
	pop es			; 恢复ES
	call showError2	; 显示字符串
	jmp mk_out
mk_error1:
	call showError1	; 显示字符串
mk_out:
	mov cx,11
	mov al,20h
	mov di,Dirbuf            ;清空目录Dirbuf
	rep stosb
	mov word[Dir_len],0
mk_ends:	
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	inc dh
	; 设置光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	add sp,2
	
	jmp again
; ------------------------------------------------------------------
; 创建目录项函数,目录名存在 DirBuf里面，函数的任务是在当前目录扇区中找到空闲条目,并找到空闲的512B扇区存放初始化的扇区数据
DefaultDirBuf db 10h
			resb 10	;10B保留
			resb 6;db 39h,0a1h,0c2h,48h,50h,0    ;最后写入时间、日期、开始簇号
			resb 4	;4B大小为零
CreateDir:
	;-------------------------------------------------------------------1
	push es		; 保护ES

; 软驱复位
	xor	ah, ah	; 功能号ah=0（复位磁盘驱动器）
	xor	dl, dl	; dl=0（软驱A，软驱B为1、硬盘和U盘为80h）
	int	13h		; 磁盘中断
	
; 下面在当前目录中寻找空条目
	mov ax,[SectorNoOfCurrentDirectory]
	mov	word [wSectorNo], ax 	; 给表示当前扇区号的
						; 变量wSectorNo赋初值为根目录区的首扇区号（=19）
	mov ax,[CurrentDirSectors]
	mov word [wRootDirSizeForLoop], ax	; 根目录区剩余扇区数
										; 初始化为14，在循环中会递减至零
CreateDir_SEARCH_IN_Current_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; 判断根目录区是否已读完
	jz	CreateDir_NOT_FOUND	; 若读完则表示未找到目录项
	dec	word [wRootDirSizeForLoop]	; 递减变量wRootDirSizeForLoop的值
	; 调用读扇区函数读入一个根目录扇区到装载区
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call ReadSec		; 调用读扇区函数

	;mov	si, MKbuf		; DS:SI -> 目录项
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; 清除DF标志位
						; 置比较字符串时的方向为左/上[索引增加]
	mov	dx, 10h			; 循环次数=1（每个扇区有16个文件条目：512/32=16）
CreateDir_SEARCH_FOR_VOL_FILE:
	cmp	dx, 0			; 循环次数控制
	jz CreateDir_GOTO_NEXT_SECTOR_IN_Current_DIR ; 若已读完一扇区
	dec	dx				; 递减循环次数值			  就跳到下一扇区
CreateDir_CMP_FILENAME:
	;repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
	;cmp	cx, 0
	;判断第一个字节的值
	cmp byte[es:di],0
	jz actually_found
	cmp byte[es:di],05
	jz actually_found
	cmp byte[es:di],0e5h
	jz actually_found
	
	;jz	CreateDir_VOL_FOUND ; 如果比较了11个字符都相等，表示找到
CreateDir_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
						; FFE0h = 1111111111100000（低5位=32=目录条目大小）
	add	di, 20h			; DI += 20h 下一个目录条目
	;mov	si, MKbuf		; SI指向装载文件名串的起始地址
	jmp	CreateDir_SEARCH_FOR_VOL_FILE; 转到循环开始处

CreateDir_GOTO_NEXT_SECTOR_IN_Current_DIR: ;对于子目录LABEL_GOTO_NEXT_SECTOR_IN_Current_DIR要自己算出来(直接使用toDir中的算法)  与根目录算法不同
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	CreateDir_NOT_FOUND ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	CreateDir_SEARCH_IN_Current_DIR_BEGIN		; 继续搜索目录循环
.root:
	inc	word [wSectorNo]	; 对于根目录，递增当前扇区号
	jmp	CreateDir_SEARCH_IN_Current_DIR_BEGIN
actually_found:          ;发现空余块
	;设置文件名
	;设置属性(默认)子目录=10h
	;设置时间(暂时不作考虑)
	;文件大小默认全0
	pop es

	push es
	cld
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
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
	repe movsb			; 写缓冲区，修改对应文件条目  文件名
	
	;计算出时间、日期
	call CountTimeDate
	
	;fat表中遍历得到空闲区,分配给子目录
	;设置fat表项为最后簇  getEmptyFatEntry  setEmptyFatEntry
	call getEmptyFatEntry
	push ax
	;call hex2ascii
	;xor ah,ah
	;int 16h
	pop ax
	push ax
	mov [InitialDirSector],ax    ;保存空闲的扇区号，下面开始写扇区
	call setEmptyFatEntry
	pop ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+1ah-11],ax  ;写入对应fat扇区
	pop di
	
	;=======================================
	;初始化子目录扇区
	push di
	mov di,InitialDirBuf
	mov word [ds:di+1ah],ax  ;写入对应fat扇区(开始簇号).
	mov ax,[SectorNoOfCurrentDirectory]
	cmp ax,19                ;根目录与子目录扇区含义不同，比较脑残，cdToDir和mkDir均要判断
	jnz .noNeedToAdd_1fh
	add ax,1fh
	jmp MKjudgeEnds
.noNeedToAdd_1fh:
	sub ax,1fh
MKjudgeEnds:
	mov word [ds:di+1ah+32],ax  ;写入当前目录簇号 ffffg..
	pop di
	call CountTimeDate_Initial
	call InitialDir
	;=======================================
	;写目录条目
	mov si,DefaultDirBuf
	mov cx,21
	repe movsb			; 写缓冲区，修改对应文件条目  默认属性
	
	;mov al,0cbh
	;call hex2ascii
	;xor ah,ah
	;int 16h
	
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call WriteSec		; 调用写扇区函数
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
InitialDir: ;根据扇区号写入
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
	;=============设置时间================时间=小时*2048+分钟*32+秒/2
	mov dx,0
	; 获取时信息
	mov al, 4			; 时的偏移地址为4
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入时信息
	call bcd2hex
	
	mov dl,al
	shl dx,11          ;X2048 时间公式 时间=小时*2048+分钟*32+秒/2
	
	; 获取分信息
	mov al, 2			; 分的偏移地址为2
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入分信息
	call bcd2hex
	
	mov ah,0   
	shl ax,5           ;X32
	add dx,ax
	
	; 获取秒信息
	mov al, 0			; 秒的偏移地址为0
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入秒信息
	call bcd2hex
	
	mov ah,0
	shr ax,1           ;/2
	add dx,ax
	
	push di
	mov di,InitialDirBuf
	mov word [ds:di+16h],dx  ;写入当前时间.
	mov word [ds:di+16h+32],dx  ;写入当前时间..
	pop di
	
	;==============设置日期===============日期=(年份-1980)*512+月份*32+日
	mov dx,0
	; 获取年信息
	mov al, 9			; 年的偏移地址为9
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入年信息
	call bcd2hex
	
	mov ah,0
	add ax,2000-1980
	shl ax,9
	add dx,ax
	
	; 获取月信息
	mov al, 8			; 月的偏移地址为8
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入月信息
	call bcd2hex
	
	mov ah,0
	shl ax,5
	add dx,ax
	
	; 获取日信息
	mov al, 7			; 日的偏移地址为7
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入日信息
	call bcd2hex
	
	mov ah,0
	add dx,ax
	
	push di
	mov di,InitialDirBuf
	mov word [ds:di+18h],dx  ;写入当前日期.
	mov word [ds:di+18h+32],dx  ;写入当前日期..
	pop di
	
	pop di
	pop dx
	ret
; -------------------------------------------------------------------
CountTimeDate:
	push dx
	push di
	;=============设置时间================时间=小时*2048+分钟*32+秒/2
	mov dx,0
	; 获取时信息
	mov al, 4			; 时的偏移地址为4
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入时信息
	call bcd2hex
	
	mov dl,al
	shl dx,11          ;X2048 时间公式 时间=小时*2048+分钟*32+秒/2
	
	; 获取分信息
	mov al, 2			; 分的偏移地址为2
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入分信息
	call bcd2hex
	
	mov ah,0   
	shl ax,5           ;X32
	add dx,ax
	
	; 获取秒信息
	mov al, 0			; 秒的偏移地址为0
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入秒信息
	call bcd2hex
	
	mov ah,0
	shr ax,1           ;/2
	add dx,ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+16h-11],dx  ;写入当前时间
	pop di
	
	;==============设置日期===============日期=(年份-1980)*512+月份*32+日
	mov dx,0
	; 获取年信息
	mov al, 9			; 年的偏移地址为9
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入年信息
	call bcd2hex
	
	mov ah,0
	add ax,2000-1980
	shl ax,9
	add dx,ax
	
	; 获取月信息
	mov al, 8			; 月的偏移地址为8
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入月信息
	call bcd2hex
	
	mov ah,0
	shl ax,5
	add dx,ax
	
	; 获取日信息
	mov al, 7			; 日的偏移地址为7
	out 70h, al		; 指定存储单元地址
	in al, 71h			; 读入日信息
	call bcd2hex
	
	mov ah,0
	add dx,ax
	
	push di
	mov di,DefaultDirBuf
	mov word [ds:di+18h-11],dx  ;写入当前日期
	pop di
	
	pop di
	pop dx
	ret
; ------------------------------------------------------------------
bcd2hex:  ;组合十进制bcd码 转 16进制  入口：al  出口AL
	push dx
	push bx
	mov dx,0
	mov dl,al
	and dl,0fh     ;截取低四位  个位�
	and al,0f0h    ;截取高4位   十位
	shr al,4 	   ;移至低位
	mov ah,0
	mov bl,10
	mul bl         ;乘10
	add al,dl      ;加个位
	
	mov ah,0
	pop bx
	pop dx
	ret
; ------------------------------------------------------------------
scrollscreen:      ;滚动屏幕 al=行号
	pusha
	mov	ah, 6			; 功能号
	mov bh,11110000b		; 设置背景色为黑色
	mov ch, 0			; CH=行号、CL=列号
	mov cl, 0			; 窗口左上角的行列号都为0
	mov dh, 29		; 窗口右下角的行号，文本屏幕25行，行号=0~24
	mov dl, 79		; 窗口右下角的列号，文本屏幕80列，列号=0~79
	int 10h			; 显示中断
	popa
	ret	
; -------------------------------------------------------------------
helpStr:
	db 'You can use the following inner command:'
helpStrLen equ $-helpStr
help: ; 显示帮助信息
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	
	cmp dh,28
	jl .0
	mov al,2
	call scrollscreen
	mov dh,26
.0:
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, helpStr 	; BP=串地址
	mov cx, helpStrLen	; 串长
	int 10h 		; 调用10H号中断

	inc dh          ;行号+1
	inc dh          ;行号+1
	
	; 循环显示提示串
	push si
	push di
	mov cx,N       ;命令串个数
	mov si,0
	mov di,0
.helps:
	push cx
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 2 		; 第2列
	
	mov bp, cmdstr 	; BP=串地址
	add bp,si
	mov cx, 8	; 串长
	int 10h 		; 调用10H号中断
	
	mov ah, 42h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 10 		; 第11列
	mov bp, cmdHelpStr_chin 	; BP=串地址
	add bp,di
	mov cx, 15	; 串长
	
	int 21h 		; 调用10H号中断
	
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
	ret				; 从例程返回
	
; -------------------------------------------------------------------
; 内部命令例程结束
; ===================================================================


; ===================================================================
; 命令行主循环例程开始
; -------------------------------------------------------------------
prompt: ; 显示命令行系统提示串例程
	call newline	; 回车换行
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号中断
	; 显示提示串
	mov ah, 13h 	; 功能号
	mov al, 1 		; 光标放到串尾
	mov bl, 0fh 	; 亮白
	mov bh, 0 		; 第0页
	mov dl, 0 		; 第0列
	mov bp, str2 	; BP=串地址
	mov cx, [str2len]	; 串长
	int 10h 		; 调用10H号中断
	ret				; 从例程返回
blank db 20h
; -------------------------------------------------------------------
getstrln: ; 获取键盘输入的命令串行
	cld				; 清除方向标志位（使扫描字符串方向为从串首到串尾）
	
	; 用空格符（20h）填充buf
	mov cx, buflen	; 循环次数CX=命令行缓冲区buf的长度（buflen=80）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, buf		; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	; 用空格符（20h）填充fnbuf的前8个字节
	mov cx, cslen	; 循环次数CX=命令串最大的长度（cslen=8）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, fnbuf	; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	mov si, 0		; 当前字符偏移位置 SI = 0
keyin: ; 接受键盘输入
	; 读按键（返回的按键ASCII码在AL中）
	mov ah, 0 		; 功能号
	int 16h 		; 调用16H号中断
	; 对回车符（0DH）结束输入
	cmp al, 0dh 	; 比较AL中的键入字符与回车符（ASCII码为0DH）
	je return 		; 相等跳转到从例程返回
	cmp al, 08h
	je backspace
	; 保存按键字符到buf
	mov [buf + si], al; buf[SI]=AL
	inc si			; SI++
	; 太长时跳出
	cmp si, buflen	; SI >= 80 ?
	jae goout		; >= 时跳转
	jmp next_k
	
backspace:
	cmp si,0        ;没有输入的字符跳转继续输入
	je keyin
	
	dec si
	mov byte [buf + si], 20h; 填入空格
	
	; 显示字符串例程（需先置串长CX和串地址BP）
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	pusha
	mov cx,1       ; 串长1
	mov bp,blank   ; 串地址
	push cx			; 保护CX（进栈）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	pop cx			; 恢复CX（出栈）
	;10	2	置光标位置	BH=页号
    ;DH,DL=行,列
	
	dec dl          ; 退格
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	;dec dl          ; 再退一格
	; 在当前位置显示字符串（串长CX和串地址BP已预先设置好了）
	mov ah, 13h		; BIOS中断的功能号（显示字符串）
	mov al, 1 		; 光标放到串尾
	mov bh, 0 		; 页号=0
	mov bl, 0fh		; 字符颜色=不闪（0）黑底（000）亮白字（1111）
	int 10h 		; 调用10H号显示中断
	
	;10	2	置光标位置	BH=页号
    ;DH,DL=行,列
	
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	
	popa
	jmp keyin
	
	; 显示AL中的键入字符
next_k:
	mov ah, 0eh 	; 功能号
	mov bl, 0fh 	; 亮白字
	int 10h 		; 调用10H号中断
	jmp keyin		; 循环读存显按键
return:
	ret 			; 从例程返回

goout: ; 键入的字符数超过缓冲区长度时跳转到此
	call showtoolong; 显示串太长出错信息
	add sp, 2		; 弹出CALL时压栈的返回地址
	jmp again		; 重新开始主循环
	
; -------------------------------------------------------------------
dtlen: ; 确定命令串长度
	mov cx, buflen	; CX = 输入缓冲区长度（80）
	mov al, 20h		; AL = 空格符
	mov di, buf		; DI指向buf
	; 在buf中找到第一个空格符后停止：
	repne scasb		; CX>0 && [di]≠AL 时DI++继续扫描，否则退出循环
	jcxz toolong	; CX=0则没找到空格符，串长n = buflen >> cslen (= 8)
	; 计算 n = 输入缓冲区长度 - CX - 1
	mov word [n], buflen ; n = buflen
	sub [n], cx		; n - CX
	dec word [n]	; n--
	je zlen 		; n=0：重新开始命令行循环
	cmp word [n], cslen ; n > 8 ?
	ja toolong		; 命令串长超过8时跳转
	ret 			; 从例程返回

toolong: ; 命令串太长（报错退出）
	call showwrong	; 显示出错信息
zlen: ; n=0时重新开始
	add sp, 2		; 弹出call压栈的返回地址
	jmp again		; 重新开始

; -------------------------------------------------------------------
tocap: ; 转换成大写字母
	mov cx, [n]		; 循环次数 CX = n
	mov bx, 0		; 字符偏移值 BX = 0（初值为0）
next: ; 循环开始
	cmp byte [buf + bx], 61h	; 字符与字母a（61h）比较
	jb notll					; 字符 < 61h 跳转
	cmp byte [buf + bx], 7ah	; 字符与字母z（7Ah）比较
	ja notll					; 字符 > 7Ah 跳转
	sub byte [buf + bx], 20h	; 小写字母 - 20h = 大写字母
notll: ; 不是小写字母
	inc bx			; 递增偏移值
	loop next		; 继续循环
	ret 			; 从例程返回
; -------------------------------------------------------------------
tocap_Dirbuf: ; 转换成大写字母
	mov cx, [Dir_len]		; 循环次数 CX = n
	mov bx, 0		; 字符偏移值 BX = 0（初值为0）
.next: ; 循环开始
	cmp byte [Dirbuf + bx], 61h	; 字符与字母a（61h）比较
	jb .notll					; 字符 < 61h 跳转
	cmp byte [Dirbuf + bx], 7ah	; 字符与字母z（7Ah）比较
	ja .notll					; 字符 > 7Ah 跳转
	sub byte [Dirbuf + bx], 20h	; 小写字母 - 20h = 大写字母
.notll: ; 不是小写字母
	inc bx			; 递增偏移值
	loop .next		; 继续循环
	ret 			; 从例程返回

; -------------------------------------------------------------------
newstr:	; 构造新串（命令串 --> COM文件名）
	mov si, buf		; 源串起始地址
	mov di, fnbuf	; 目的串起始地址
	mov cx, [n]		; 循环次数 CX = n
	; 将输入缓冲区buf中的命令串复制到文件名缓冲区fnbuf：
	rep movsb		; CX > 0时 [ES:DI] = [DS:SI]、CX--，CX = 0时退出循环
	ret 			; 从例程返回

; -------------------------------------------------------------------
iscmd: ; 判断是否为内部命令
	mov word [i], 0	; 外循环变量/内部命令的序号i=0（初值为0）
	mov dx, cmdstr	; 命令串的初始起始地址
	
.1: ; 外循环
	mov si, fnbuf	; 源串起始地址
	mov di, dx		; 目的串起始地址
	mov cx, cslen 	; 内循环次数
	; 重复比较两字符串中的字符，CX--，直到不相等或CX=0
	repe cmpsb		; CX>0 && [DS:SI]==[ES:DI]时，CX--、SI++、DI++，继续循环；否则退出
	jcxz docmd		; CX=0，表示两串相等，为第BX个内部命令串，跳转执行该命令
	inc word [i]	; CX≠0，表示两串不等，i++
	cmp word [i], N	; i=N（内部命令总数）？
	je .2			; 不是内部命令，退出循环
	add dx, cslen	; DX + 8 =下一命令串的起始地址
	jmp .1			; 继续外循环
.2: ; 返回
	;call showwrong	; 显示出错信息
	ret 			; 从例程返回
	
docmd: ; 执行内部命令
	add sp, 2		; 弹出call iscmd时压栈的返回地址
	call newline	; 回车换行
	mov bx, [i]		; BX = 内部命令的序号i
	shl bx, 1		; 偏移地址 = 内部命令的序号*2
	call [cmdaddr + bx] ; 调用第i个内部命令
	jmp again		; 跳转到命令行循环
	
;--------------------------------------------------------------------
exec: ; 执行外部命令（COM文件）

; 定义常量（COM文件加载位置和磁盘参数）
BaseOfLoader	equ	2000h	; COM文件被加载到的位置 ----  段地址
OffsetOfLoader	equ	100h	; COM文件被加载到的位置 ---- 偏移地址
RootDirSectors	equ	14		; 根目录占用的扇区数
SectorNoOfRootDirectory	equ	19	; 根目录区的首扇区号
SectorNoOfFAT1	equ	1		; FAT#1的首扇区号 = BPB_RsvdSecCnt
DeltaSectorNo	equ	17		; DeltaSectorNo = BPB_RsvdSecCnt + 
							; (BPB_NumFATs * FATSz) - 2 = 1 + (2*9) -2 = 17
							; 文件的开始扇区号 = 目录条目中的开始扇区号 
							; + 根目录占用扇区数目 + DeltaSectorNo
	call Shut_dc
	push es		; 保护ES
; 软驱复位
	xor	ah, ah	; 功能号ah=0（复位磁盘驱动器）
	xor	dl, dl	; dl=0（软驱A，软驱B为1、硬盘和U盘为80h）
	int	13h		; 磁盘中断
	
; 下面在磁盘目录中寻找 COM文件
	;判断是根目录或者子目录
	push ax
	mov ax,[SectorNoOfCurrentDirectory] 	; 给表示当前扇区号的
	mov	word [wSectorNo], ax
						; 变量wSectorNo赋初值为当前目录区的首扇区号
	mov ax, [CurrentDirSectors]	; 剩余扇区数
	mov word [wRootDirSizeForLoop],ax
										; 初始化为当前目录所占扇区数，在循环中会递减至零
	pop ax
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0 ; 判断根目录区是否已读完
	jz	LABEL_NOT_FOUND	; 若读完则表示未找到COM文件
	dec	word [wRootDirSizeForLoop]	; 递减变量wRootDirSizeForLoop的值
	; 调用读扇区函数读入一个目录扇区到装载区
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（4000h）
	mov	bx, OffsetOfLoader	; BX <- OffsetOfLoader（100h）
	mov	ax, [wSectorNo]	; AX <- 根目录中的当前扇区号
	mov	cl, 1			; 只读一个扇区
	call ReadSec		; 调用读扇区函数

	mov	si, fnbuf		; DS:SI -> COM文件
	mov	di, OffsetOfLoader ; ES:DI -> BaseOfLoader:0100
	cld					; 清除DF标志位
						; 置比较字符串时的方向为左/上[索引增加]
	mov	dx, 10h			; 循环次数=16（每个扇区有16个文件条目：512/32=16）
LABEL_SEARCH_FOR_COM_FILE:
	cmp	dx, 0			; 循环次数控制
	jz LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR ; 若已读完一扇区
	dec	dx				; 递减循环次数值			  就跳到下一扇区
	mov	cx, 11			; 初始循环次数为11
LABEL_CMP_FILENAME:
	repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND ; 如果比较了11个字符都相等，表示找到
LABEL_DIFFERENT:
	and	di, 0FFE0h		; DI &= E0为了让它指向本条目开头（低5位清零）
						; FFE0h = 1111111111100000（低5位=32=目录条目大小）
	add	di, 20h			; DI += 20h 下一个目录条目
	mov	si, fnbuf		; SI指向装载文件名串的起始地址
	jmp	LABEL_SEARCH_FOR_COM_FILE; 转到循环开始处

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:             ;ssssss
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[wSectorNo]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	LABEL_NOT_FOUND ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [wSectorNo],ax
	add	word [wSectorNo],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN		; 继续搜索目录循环
.root:
	inc	word [wSectorNo]	; 对于根目录，递增当前扇区号
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NOT_FOUND:
	pop es			; 恢复ES
	call showwrong	; 显示字符串
	ret

; 下面将COM文件加载到内存
LABEL_FILENAME_FOUND:	; 找到 COM文件后便来到这里继续
	; 计算文件的起始扇区号
	mov	ax, [CurrentDirSectors]	; AX=当前目录占用的扇区数
	and	di, 0FFE0h		; DI -> 当前条目的开始地址
	add	di, 1Ah			; DI -> 文件的首扇区号在条目中的偏移地址
	mov cx, word [es:di] ; CX=文件的首扇区号
	push cx				; 保存此扇区在FAT中的序号
	add	cx, RootDirSectors			; CX=文件的相对起始扇区号+根目录占用的扇区数 +根目录占用的扇区数+根目录占用的扇区数+根目录占用的扇区数+根目录占用的扇区数+根目录占用的扇区数
	;重要的事情说一万遍=_=,找这个bug用了几小时   原代码add	cx,ax   现在子目录ax并不是根目录首扇区号
	add	cx, DeltaSectorNo ; CL <- COM文件的起始扇区号(0-based)
	mov	ax, BaseOfLoader      ;+1C
	mov	es, ax			; ES <- BaseOfLoader（COM程序基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（COM程序偏移地址=100h）
	mov	ax, cx			; AX <- 起始扇区号

LABEL_GOON_LOADING_FILE:
	push bx				; 保存COM程序偏移地址
	mov	cl, 1			; 1个扇区
	call ReadSec		; 读扇区

	; 计算文件的下一扇区号
	pop bx				; 取出COM程序偏移地址
	pop	ax				; 取出此扇区在FAT中的序号
	call GetFATEntry	; 获取FAT项中的下一簇号
	cmp	ax, 0FF8h		; 是否是文件最后簇
	jae	LABEL_FILE_LOADED ; ≥FF8h时跳转，否则读下一个簇
	push ax				; 保存扇区在FAT中的序号
	mov	dx, RootDirSectors	; DX = 根目录扇区数
	add	ax, dx			; 扇区序号 + 根目录扇区数
	add	ax, DeltaSectorNo ; AX = 要读的数据扇区地址
	add	bx, [BPB_BytsPerSec] ; BX+512指向COM程序区的下一个扇区地址
	jmp	LABEL_GOON_LOADING_FILE

; 下面跳转执行COM程序
LABEL_FILE_LOADED:
	pop es
	add sp, 2			; 弹出call指令压栈的返回地址和保存的ES
	jmp	BaseOfLoader:OffsetOfLoader	; 这一句跳转到已加载到内存中的
						; COM文件的开始处，开始执行 COM文件的代码。
						; （COM程序通过调用21h中断返回命令行程序）

; 变量
BPB_BytsPerSec	DW 512	; 每扇区字节数
BPB_SecPerTrk	DW 18	; 每磁道扇区数

wRootDirSizeForLoop	dw	RootDirSectors	; 根目录区剩余扇区数
										; 初始化为14，在循环中会递减至零
wSectorNo		dw	0	; 当前扇区号，初始化为0，在循环中会递增
bOdd			db	0	; 奇数还是偶数FAT项
; -------------------------------------------------------------------
; 命令行主循环例程结束
; ===================================================================


; ===================================================================
; 大型辅助例程开始
;--------------------------------------------------------------------

;--------------------------------------------------------------------
; 例程名：GetFATEntry
;--------------------------------------------------------------------
; 作用：找到序号为AX的扇区在FAT中的条目，结果放在AX中。需要注意的
;     是，中间需要读FAT的扇区到ES:BX处，所以函数一开始保存了ES和BX
GetFATEntry:
	push es			; 保存ES、BX和AX（入栈）
	push bx
	push ax
; 设置读入的FAT扇区写入的基地址
	mov ax, BaseOfLoader	; AX=4000h
	sub	ax, 100h	; 在BaseOfLoader后面留出4K空间用于存放FAT
	mov	es, ax		; ES=8F00h
; 判断FAT项的奇偶
	pop	ax			; 取出FAT项序号（出栈）
	mov	byte [bOdd], 0; 初始化奇偶变量值为0（偶）
	mov	bx, 3		; AX*1.5 = (AX*3)/2
	mul	bx			; DX:AX = AX * 3（AX*BX 的结果值放入DX:AX中）
	mov	bx, 2		; BX = 2（除数）
	xor	dx, dx		; DX=0	
	div	bx			; DX:AX / 2 => AX <- 商、DX <- 余数
	cmp	dx, 0		; 余数 = 0（偶数）？
	jz LABEL_EVEN	; 偶数跳转
	mov	byte [bOdd], 1	; 奇数
LABEL_EVEN:		; 偶数
	; 现在AX中是FAT项在FAT中的偏移量，下面来
	; 计算FAT项在哪个扇区中(FAT占用不止一个扇区)
	xor	dx, dx		; DX=0	
	mov	bx, [BPB_BytsPerSec]	; BX=512
	div	bx			; DX:AX / 512
		  			; AX <- 商 (FAT项所在的扇区相对于FAT的扇区号)
		  			; DX <- 余数 (FAT项在扇区内的偏移)
	push dx			; 保存余数（入栈）
	mov bx, 0 		; BX <- 0 于是，ES:BX = 8F00h:0
	add	ax, SectorNoOfFAT1 ; 此句之后的AX就是FAT项所在的扇区号
	mov	cl, 2			; 读取FAT项所在的扇区，一次读两个，避免在边界
	call	ReadSec	; 发生错误, 因为一个 FAT项可能跨越两个扇区
	pop	dx			; DX= FAT项在扇区内的偏移（出栈）
	add	bx, dx		; BX= FAT项在扇区内的偏移
	mov	ax, [es:bx]	; AX= FAT项值
	cmp	byte [bOdd], 1	; 是否为奇数项？
	jnz	LABEL_EVEN_2	; 偶数跳转
	shr	ax, 4			; 奇数：右移4位（取高12位）
LABEL_EVEN_2:		; 偶数
	and	ax, 0FFFh	; 取低12位
LABEL_GET_FAT_ENRY_OK:
	pop	bx			; 恢复ES、BX（出栈）
	pop	es
	ret
;--------------------------------------------------------------------
;设置fat表项为最后簇  getEmptyFatEntry  setEmptyFatEntry
getEmptyAX dw 2
getEmptyFatEntry:
;--------------------------------------------------------------------
; 作用：找到空闲簇号
	push es
	mov ax,2
	mov word [getEmptyAX],2
.1
	call GetFATEntry
	cmp	ax, 0		; 是否是空闲簇
	jz	.FindEmpty ;等于零认为是空闲簇
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
; 作用  设置ax对应的簇号被占用
	push es			; 保存ES、BX和AX（入栈）
	push bx
	push ax
; 设置读入的FAT扇区写入的基地址
	mov ax, BaseOfLoader	; AX=4000h
	sub	ax, 100h	; 在BaseOfLoader后面留出4K空间用于存放FAT
	mov	es, ax		; ES=8F00h
; 判断FAT项的奇偶
	pop	ax			; 取出FAT项序号（出栈）
	mov	byte [bOdd], 0; 初始化奇偶变量值为0（偶）
	mov	bx, 3		; AX*1.5 = (AX*3)/2
	mul	bx			; DX:AX = AX * 3（AX*BX 的结果值放入DX:AX中）
	mov	bx, 2		; BX = 2（除数）
	xor	dx, dx		; DX=0	
	div	bx			; DX:AX / 2 => AX <- 商、DX <- 余数
	cmp	dx, 0		; 余数 = 0（偶数）？
	jz setEmpty_EVEN	; 偶数跳转
	mov	byte [bOdd], 1	; 奇数
setEmpty_EVEN:		; 偶数
	; 现在AX中是FAT项在FAT中的偏移量，下面来
	; 计算FAT项在哪个扇区中(FAT占用不止一个扇区)
	xor	dx, dx		; DX=0	
	mov	bx, [BPB_BytsPerSec]	; BX=512
	div	bx			; DX:AX / 512
		  			; AX <- 商 (FAT项所在的扇区相对于FAT的扇区号)
		  			; DX <- 余数 (FAT项在扇区内的偏移)
	push dx			; 保存余数（入栈）
	mov bx, 0 		; BX <- 0 于是，ES:BX = 8F00h:0
	add	ax, SectorNoOfFAT1 ; 此句之后的AX就是FAT项所在的扇区号
	mov [setEmptyAX],ax
	
	mov	cl, 2			; 读取FAT项所在的扇区，一次读两个，避免在边界
	call	ReadSec	; 发生错误, 因为一个 FAT项可能跨越两个扇区
	pop	dx			; DX= FAT项在扇区内的偏移（出栈）
	add	bx, dx		; BX= FAT项在扇区内的偏移
	mov	ax, [es:bx]	; AX= FAT项值
	cmp	byte [bOdd], 1	; 是否为奇数项？
	jnz	setEmpty_EVEN_2	; 偶数跳转
	
	pusha
	or	ax, 0FFF0h
	;shrd bx,ax, 4			; 奇数：右移4位（取高12位）
	;mov dx,0FFFH
	;shld dx,bx,4            ;bx中保存的数据放回dx
	mov	[es:bx],ax
	;写FAT表
	mov ax,[setEmptyAX]
	mov cl,2
	mov bx,0
	call WriteSec	
	popa
	jmp setEmpty_GET_FAT_ENRY_OK
setEmpty_EVEN_2:		; 偶数
	;and	ax, 0FFFh	; 取低12位
	pusha
	or	ax, 0FFFh
	;shld bx,ax, 4			; 偶数：左移4位
	;mov dx,0FFF0H
	;shrd dx,bx,4            ;bx中保存的数据放回dx
	mov	[es:bx],ax
	;写FAT表
	mov ax,[setEmptyAX]
	mov cl,2
	mov bx,0
	call WriteSec	
	popa
setEmpty_GET_FAT_ENRY_OK:
	pop	bx			; 恢复ES、BX（出栈）
	pop	es
	ret
;--------------------------------------------------------------------
; 例程名：showbpb
;--------------------------------------------------------------------
; 作用：; 显示磁盘的BPB信息
showbpb:
	call ReadPBootSec	; 调用读入磁盘分区引导扇区例程

	mov word [lns], 0	; 当前已显示行数，初始化为0
	
	; 显示OEM串---------------------------------------------
	mov cx, OEMMsgLen	; CX=串长
	mov bp, OEMMsg		; BP="OEM:"
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
	mov cx, 8			; CX=串长=8
	mov bp, Sector + 3	; BP=BPB中的OEM串
	call DispStr		; 调用显示字符串例程
	call newline		; 回车换行
	inc word [lns]		; lns++ 已显示行数+1

	; 显示介质串---------------------------------------------
	mov cx, MediaMsgLen	; CX=串长
	mov bp, MediaMsg	; BP="Media:"
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
	cmp byte [Sector + 15h], 0F0h ; 介质描述符 > F0h ?
	jg HD				; > 为硬盘
	; 软盘
	mov cx, FDMsgLen	; CX=软盘的串长
	mov bp, FDMsg		; BP="Floppy Disk"
	jmp DStr			; 跳转到显示串
HD: ; 硬盘
	mov cx, HDMsgLen	; 硬盘的串长=9
	mov bp, HDMsg		; BP="Hard Disk"
DStr: ; 显示串
	call DispStr		; 调用显示字符串例程
	call newline		; 回车换行
	inc word [lns]		; lns++ 已显示行数+1
	
	; 显示磁盘容量 --------------------------------------------------------
	; 显示“Size:”串
	mov cx, SizeMsgLen	; CX=串长
	mov bp, SizeMsg		; BP="Size:"
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符

	; 获取十进制数字串
	mov ax, [Sector + 13h] ; AX=总扇区数
	shr ax, 1			; 扇区数/2 = KB值
	call GetDigStr		; 以AX为传递参数，BP(串地址)和CX(字符个数)为返回值
	; 显示数字串
	call DispStr		; 调用显示字符串例程
	
	; 显示“KB”串
	add dl, cl			; 列号DL += 十进制数字串的字符个数
	inc dl				; DL++（空一格）
	mov cx, KBMsgLen	; CX=串长
	mov bp, KBMsg		; BP="KB"
	call DispStr		; 调用显示字符串例程
	call newline		; 回车换行
	inc word [lns]		; lns++ 已显示行数+1
	
	; 显示文件系统类型串---------------------------------------------
	mov cx, FSMsgLen	; CX=串长
	mov bp, FSMsg		; BP="File System:"
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
	mov cx, 8			; CX=串长=8
	mov bp, Sector + 36h ; BP=EBPB中的文件系统类型串
	call DispStr		; 调用显示字符串例程
	call newline		; 回车换行
	inc word [lns]		; lns++ 已显示行数+1
	
	; 显示BPB中的卷标串---------------------------------------------
	mov cx, VolMsgLen	; CX=串长
	mov bp, VolMsg		; BP="Vol:"
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
	mov cx, 11			; CX=串长=11
	mov bp, Sector + 2Bh ; BP=EBPB中的文件系统类型串
	call DispStr		; 调用显示字符串例程
	call newline		; 回车换行
	inc word [lns]		; lns++ 已显示行数+1
	
	; 显示ID（序列号）---------------------------------------------
	mov cx, IDMsgLen	; CX=串长
	mov bp, IDMsg		; BP="Vol:"
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
	call showid			; 显示ID串
	call newline		; 回车换行	
	inc word [lns]		; lns++ 已显示行数+1

	call newline		; 回车换行
	inc word [lns]		; lns++ 已显示行数+1

	ret					; 终止程序，返回
	
; 定义字符串常量及其长度值符号常量：	
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
showid: ; 显示4B整数ID值的十六进制串

	mov edx, [Sector + 27h] ; EDX = ID
	bswap edx		; 字节反序

	mov cx, 4		; 循环次数
.1: ; 显示单个字节
	; 显示高4位
	mov al, dl		; AL=ID高位字节
	and al, 0F0h	; 取出高4位
	shr al, 4		; AL >> 4
	call ShowChar	; 调用显示字符函数
	; 显示低4位
	mov al, dl		; AL=ID高位字节
	and al, 0Fh		; 取出低4位
	call ShowChar	; 调用显示字符函数
	; 下一个字节
	shr edx, 8		; EDX >> 8
	cmp cx, 3		; CX = 3 ?
	jne .2			; ！= 继续循环
	; 显示减号符'-'
	mov al,'-'		; AL = 空格符
	mov ah,0Eh 		; 功能号（以电传方式显示单个字符）
	mov bl,0fh 		; 亮白字
	int 10h 		; 调用10H号中断
.2:
	loop .1			; 循环

	ret				; 从例程返回
; -------------------------------------------------------------------	

; -------------------------------------------------------------------	
; 显示单个十六进制字符函数
ShowChar: ; 显示一个十六进制数字符：0~9、A~F（以AL为传递参数）
	cmp al, 10		; AL < 10 ?
	jl .1			; AL < 10：跳转到.1
	add al, 7		; AL >= 10：显示字母（ = 数值 += 37h）
.1: ; 数字
	add al, 30h		; 数字字符 = 数值+=30h
	mov ah, 0Eh		; 功能号（以电传方式显示单个字符）
	mov bl, 0fh 	; 亮白字
	int 10h 		; 调用10H号中断
	ret				; 从例程返回
; -------------------------------------------------------------------	

; --------------------------------------------------------------------
ReadPBootSec: ; 读入磁盘的分区引导扇区到Sector处
	mov bx, Sector 	; ES:BX=读入数据到内存中的存储地址
	mov ah, 2 		; 功能号
	mov al, 1 		; 要读入的扇区数
	mov dl, [drvno]	; 磁盘驱动器号：0=软盘A、1=软盘B、80h=硬盘C、81h=硬盘D
	mov dh, 0 		; 磁头号
	mov ch, 0 		; 柱面号（软盘=0、硬盘=1）
	cmp byte[drvno], 1 ; 驱动器号 > 1 ? 
	jbe	.1			; <= 1 时为软盘，柱面号CH=0
	mov ch, 1		; > 1 时为硬盘，柱面号CH=1
.1:
	mov cl, 1 		; 起始扇区号（编号从1开始）
	int 13H 		; 调用13H号中断
	ret 			; 从例程返回
; 定义缓冲区，用于存放从磁盘读入的扇区
Sector:
	resb 512

; --------------------------------------------------------------------
DispStr: ; 显示字符串例程（需先置串长CX和串地址BP）
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	push cx			; 保护CX（进栈）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	pop cx			; 恢复CX（出栈）

	; 在当前位置显示字符串（串长CX和串地址BP已预先设置好了）
	mov ah, 13h		; BIOS中断的功能号（显示字符串）
	mov al, 1 		; 光标放到串尾
	mov bh, 0 		; 页号=0
	mov bl, 0fh		; 字符颜色=不闪（0）黑底（000）亮白字（1111）
	int 10h 		; 调用10H号显示中断
	ret				; 从例程返回
	
; --------------------------------------------------------------------
; 获取字数据值十进制串例程
dn equ 5 ; 最大位数
GetDigStr: ; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	mov cx, 1		; 位数=1（初值）
	mov bp, sbuf	; BP = sbuf + dn - 1 = sbuf的当前位置
	add bp, dn - 1
	mov bx,10		; 除数=10
DLoop: ; 循环开始处
	mov dx, 0		; DX=0, DX:AX / BX -> 商AX、余DX
	div bx
	add dl, 30h		; 余数 + 30h = 对应的数字符ASCII码
	mov [bp], dl	; sbuf[BP] = DL
	cmp ax, 0		; 商AX = 0 ?
	je OutLoop		; = 0 跳出循环
	inc cx			; 位数CX++
	dec bp			; 数字符的当前位置BP--
	jmp DLoop		; 继续循环
OutLoop: ; 退出循环
	ret				; 从例程返回

sbuf: resb dn ; 用于存放十进制数字串的缓冲区，大小 = 常量dn（=5）


;--------------------------------------------------------------------
; 例程名：ReadSec
;--------------------------------------------------------------------
; 作用：从第 AX个扇区开始，将CL个扇区读入ES:BX中
; 需使用磁盘参数secspt(每磁道扇区数）和heads(磁头数）
ReadSec:
	; ---------------------------------------------------------------
	; 怎样由扇区号求扇区在磁盘中的位置 (扇区号->柱面号、起始扇区、磁头号)
	; ---------------------------------------------------------------
	; 设扇区号为 x（= AX）
	;                             ┌ 柱面号C = y / 磁头数
	;         x            ┌ 商 y ┤
	;   -------------- 	=> ┤      └ 磁头号H = y % 磁头数
	;    每磁道扇区数      │
	;                      └ 余 z => 起始扇区号S = z + 1
	push es
	push cx			; 保存要读的扇区数CL
	push bx			; 保存BX
	mov	bl, [secspt]; BL(= 磁道扇区数）为除数
	div	bl			; AX/BL，商y在AL中、余数z在AH中
	inc	ah			; z ++（因磁盘的起始扇区号为1），AH = 起始扇区号
	mov	cl, ah		; CL <- 起始扇区号S
	mov	ah, 0		; AX <- y
	mov bl, [heads]	; BL(= 磁头数）为除数
	div	bl			; AX/BL，商在AL中、余数在AH中
	mov	ch, al		; CH <- 柱面号C
	mov	dh, ah		; DH <- 磁头号H
	; 至此，"柱面号、起始扇区、磁头号"已全部得到
	pop	bx			; 恢复BX
	pop ax			; AL = 恢复的要读的扇区数CL
	mov	dl, [drvno]	; 驱动器号
.1: ; 使用磁盘中断读入扇区
	mov	ah, 2		; 功能号（读扇区）
	int	13h			; 磁盘中断
	jc .1			; 如果读取错误，CF会被置为1，这时就不停地读，直到正确为止
	pop es
	ret
;--------------------------------------------------------------------
; 例程名：WriteSec
;--------------------------------------------------------------------
; 作用：从第 AX个扇区开始，将ES:BX中 写到CL个扇区中
; 需使用磁盘参数secspt(每磁道扇区数）和heads(磁头数）
WriteSec:
	; ---------------------------------------------------------------
	; 怎样由扇区号求扇区在磁盘中的位置 (扇区号->柱面号、起始扇区、磁头号)
	; ---------------------------------------------------------------
	; 设扇区号为 x（= AX）
	;                             ┌ 柱面号C = y / 磁头数
	;         x            ┌ 商 y ┤
	;   -------------- 	=> ┤      └ 磁头号H = y % 磁头数
	;    每磁道扇区数      │
	;                      └ 余 z => 起始扇区号S = z + 1
	push es
	push cx			; 保存要读的扇区数CL
	push bx			; 保存BX
	mov	bl, [secspt]; BL(= 磁道扇区数）为除数
	div	bl			; AX/BL，商y在AL中、余数z在AH中
	inc	ah			; z ++（因磁盘的起始扇区号为1），AH = 起始扇区号
	mov	cl, ah		; CL <- 起始扇区号S
	mov	ah, 0		; AX <- y
	mov bl, [heads]	; BL(= 磁头数）为除数
	div	bl			; AX/BL，商在AL中、余数在AH中
	mov	ch, al		; CH <- 柱面号C
	mov	dh, ah		; DH <- 磁头号H
	; 至此，"柱面号、起始扇区、磁头号"已全部得到
	pop	bx			; 恢复BX
	pop ax			; AL = 恢复的要读的扇区数CL
	mov	dl, [drvno]	; 驱动器号
.1: ; 使用磁盘中断读入扇区
	mov	ah, 3		; 功能号 写扇区
	int	13h			; 磁盘中断
	jc .1			; 如果读取错误，CF会被置为1，这时就不停地读，直到正确为止
	pop es
	ret
;--------------------------------------------------------------------
;--------------------------------------------------------------------
; 例程名：ls        
;子目录作出修改(子目录和文件一样，极有可能不连续，原来的算法不可靠，要去查fat项再计算下一扇区位置)
;--------------------------------------------------------------------
; 作用：; 显示磁盘根目录文件信息列表
; 需使用磁盘参数secspt(每磁道扇区数）和heads(磁头数）
ls: 
	;mov word[nsec],1
	;mov word[isec],1fh
	;add word[isec], 47h			
	
	;call getdiskparam	; 获取磁盘参数H&S
	; 获取磁盘参数H/S
	;mov ax, [Sector + 18h]	; AX = 每磁道扇区数
	;mov [secspt], ax		; secspt = AX = 每磁道扇区数
	;mov ax, [Sector + 1Ah]	; AX = 磁头数
	;mov [heads], ax			; heads = AX = 磁头数
	; 对硬盘isec需加第1个柱面的扇区数
	mov ax,[isec]
	mov [isec_ls],ax
	mov ax,[nsec]
	mov [nsec_ls],ax
	cmp byte [drvno], 80h	; 驱动器号=80h（硬盘C）？
	je hdc					; = 80h 跳转
	jmp begain				; 软盘
hdc: ; 硬盘C
	; 计算分区前的扇区数（假设 = 1个柱面扇区数）= 每磁道扇区数 * 磁头数
	mov ax, [secspt] 		; AX = 每磁道扇区数
	mul word [heads]		; AX *= 磁头数 = 1个柱面扇区数
	add [isec_ls], ax			; isec += 1个柱面扇区数 = 硬盘根目录首扇区号

begain: 
	; 下面在磁盘当前目录中寻找文件目录条目
searchrdir: ; 搜索当前目录循环（逐个读入根目录扇区）
	cmp	word [nsec_ls], 0	; 判断根目录区是否已读完
	jz	exit			; 若读完则退出
	dec	word [nsec_ls]		; nsec--
	; 调用读扇区函数读入一个目录扇区到缓冲区
	mov	bx, Sector		; BX = Sector
	mov	ax, [isec_ls]		; AX <- 当前目录中的当前扇区号
	mov cl, 1			; 读一个扇区到缓冲区
	call ReadSec		; 调用读扇区函数
	
	mov	di, Sector		; ES:DI -> Sector	
	mov	word [i], 10h	; 循环次数=16（每个扇区有16个文件条目：512/32=16）
searchfi: ; 搜索文件项循环（在当前扇区中逐个检查文件目录项）
	cmp	word [i], 0		; 循环次数控制
	jz nextsec 			; 若已读完一扇区，跳到下一扇区
	dec	word [i]		; 递减循环次数值
	; 判断是否为文件条目（0开始的为空项、E5h开始的为已删项、属性低4位全1的
	; 为长文件名项或系统占用项、卷标项的属性3号位为1）
	cmp	byte [di], 0	; 文件名的首字母=0？
	jz	notfi			; 为空目录项
	cmp	byte [di], 0E5h	; 文件名的首字母=E5？
	jz	notfi 			; 为已删除目录项
	cmp	byte [di + 11], 0Fh; 文件属性=0Fh？
	jz	notfi 			; 为长文件名目录项

	; 显示文件名串
	inc word [lns]		; 当前屏幕上的文件条目数lns++
	Inc word [FileNum]
	; 判断是否到了屏幕底部
	cmp word [lns], 30	; 行数 = 30 ？
	jb .1				; < 24 继续
	mov word [lns], 1	; 重新设已显示行数为1
	call waitforkey		; 按任意键继续
.1: ; 继续
	; 显示文件条目信息（文件名、大小、时间）
	; 显示文件名串
	mov bp, di			; BP=文件名字符串的起始地址
	mov cx, 11			; 文件名串长8+3=11
	;判断是否为中文
	push ax
	mov al,[di + 02h]
	cmp al,80h
	ja .Chin            ; 无符号大小控制
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
	jmp .DispEnd
.Chin
	mov cx,4            ;只能显示四个汉字
	call DispStr_Chinese		; 调用显示字符串例程
	mov cx,4            ;只能显示四个汉字
	add bp,8
	mov cx,3
	call DispStr		; 调用显示字符串例程
	call space			; 插入空格符
.DispEnd
	pop ax
	; 对卷标项，不显示文件大小，显示标识串"<VOL>"
	mov al, [di + 0Bh]	; AL=文件属性
	and al, 8h			; AL & 8（卷标位）
	jz .1.1	 			; 不为卷标
	; 为卷标，显示字符串"<VOL>"
	mov bp, volbuf		; 串地址
	mov cx, fsbuflen + btbuflen + 1	; 串长=文件大小的串长
	call DispStr		; 显示字符串
	jmp .3				; 跳过显示文件大小串
.1.1:	
	; 对子目录项，不显示文件大小，显示标识串"<DIR>"
	cmp byte [di + 0Bh], 10h ; 为子目录？
	jne .2				; 显示文件大小
	; 显示字符串"<DIR>"
	mov bp, dsbuf		; 串地址
	mov cx, fsbuflen + btbuflen + 1	; 串长=文件大小的串长
	call DispStr		; 显示字符串
	jmp .3				; 跳过显示文件大小串
	
.2: ; 计算并显示文件大小十进制串
	;push eax
	;push ebx
	;mov ebx,[FileSize] 
	;add eax,ebx ;文件总大小++
	;mov [FileSize],eax
	;pop ebx
	push eax
	mov eax, [di + 1Ch]; EAX = 文件大小
	add [FileSize],eax ;文件大小++
	pop eax
	call getsizestr		; 获取文件大小十进制串
	mov bp, fsbuf		; 串地址
	mov cx, fsbuflen	; 串长
	; 显示文件大小字符串
	call DispStr		; 显示字符串
	call space			; 插入空格符
	; 显示字节字符串（文件大小单位）"Byte"
	mov bp, btbuf		; 串地址
	mov cx, btbuflen	; 串长
	call DispStr		; 显示字符串

.3: ; 插入若干空格分隔符
	call space			; 插入空格符
	call space			; 插入空格符
	call space			; 插入空格符
	
	; 显示时间（年月日时分秒，格式为：yyyy.mm.dd  hh:mm:ss）
	; 显示日期（年.月.日）
	mov ax, [di + 18h]	; AX = 日期（低5位为日、中4位为月、高7位为年-1980）
	push ax				; 保存AX进栈
	; 显示年（高7位为年-1980）
	shr ax, 9			; AX >> 9，AX = 年 - 1980
	add ax, 1980		; AX + 1980 = 年
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	call DispStr		; 显示年字符串
	; 显示月（中4位为月）
	pop ax				; 弹出AX = 日期
	push ax				; 保存AX进栈
	shr ax, 5			; AX >> 5
	and ax, 0Fh			; AX & 1111 b = 月
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	cmp cx, 1			; 串长 > 1 ？
	ja .4				; > 1：跳转
	; = 1：补充字符'0'
	dec bp				; BP--
	mov byte [bp], '0'	; 加前导'0'
	inc cx				; 串长CX++
.4: ; 添加句点分隔符'.'
	dec bp				; BP--
	mov byte [bp], '.'	; 加句点符'.'
	inc cx				; 串长CX++
	call DispStr		; 显示月字符串
	; 显示日（低5位为日）
	pop ax				; 弹出AX = 日期
	and ax, 1Fh			; AX & 1 1111 b = 日
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	cmp cx, 1			; 串长 > 1 ？
	ja .5				; > 1：跳转
	; = 1：补充字符'0'
	dec bp				; BP--
	mov byte [bp], '0'	; 加前导'0'
	inc cx				; 串长CX++
.5: ; 添加句点分隔符'.'
	dec bp				; BP--
	mov byte [bp], '.'	; 加句点符'.'
	inc cx				; 串长CX++
	call DispStr		; 显示日字符串
	call space			; 插入空格符
	call space			; 插入空格符

	; 显示时间（时:分:秒）	
	mov ax, [di + 16h]	; AX = 时间（低5位为秒/2、中6位为分、高5位为时）
	push ax				; 保存AX进栈
	; 显示时（高5位为时）
	shr ax, 11			; AX >> 11，AX = 时
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	cmp cx, 1			; 串长 > 1 ？
	ja .6				; > 1：跳转
	; = 1：补充字符'0'
	dec bp				; BP--
	mov byte [bp], '0'	; 加前导'0'
	inc cx				; 串长CX++
.6:	
	call DispStr		; 显示时字符串
	; 显示分（中6位为分）
	pop ax				; 弹出AX = 时间
	push ax				; 保存AX进栈
	shr ax, 5			; AX >> 5
	and ax, 3Fh			; AX & 11 1111 b = 分
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	cmp cx, 1			; 串长 > 1 ？
	ja .7				; > 1：跳转
	; = 1：补充字符'0'
	dec bp				; BP--
	mov byte [bp], '0'	; 加前导'0'
	inc cx				; 串长CX++
.7: ; 添加冒号分隔符':'
	dec bp				; BP--
	mov byte [bp], ':'	; 加前导':'
	inc cx				; 串长CX++
	call DispStr		; 显示月字符串
	; 显示秒（低5位为秒/2）
	pop ax				; 弹出AX = 时间
	and ax, 1Fh			; AX & 1 1111 b = 秒/2
	shl ax, 1			; AX << 1，AX*2 = 秒
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	cmp cx, 1			; 串长 > 1 ？
	ja .8				; > 1：跳转
	; = 1：补充字符'0'
	dec bp				; BP--
	mov byte [bp], '0'	; 加前导'0'
	inc cx				; 串长CX++
.8: ; 添加冒号分隔符':'
	dec bp				; BP--
	mov byte [bp], ':'	; 加前导':'
	inc cx				; 串长CX++
	call DispStr		; 显示日字符串
	
	call newline		; 回车换行
	
notfi:
	add	di, 20h			; DI += 20h 指向下一个目录条目开始处
	jmp	searchfi		; 转到循环开始处

nextsec:   ;对于子目录nextsec要自己算出来(直接使用toDir中的算法)  与根目录算法不同
	cmp word[SectorNoOfCurrentDirectory],SectorNoOfRootDirectory
	jz .root
	pusha
	push es
	push ds
	mov	ax, BaseOfLoader
	mov	es, ax			; ES <- BaseOfLoader（缓冲区基址=4000h）
	mov	bx, OffsetOfLoader ; BX <- OffsetOfLoader（缓冲区偏移地址=100h）
	mov ax,[isec_ls]
	sub ax,1fh
	call GetFATEntry	; 获取FAT项中的下一簇号
	mov [temp_ax],ax
	pop ds
	pop es
	popa
	
	cmp	word [temp_ax], 0FF8h		; 是否是目录的最后簇
	jae	exit ; ≥FF8h时跳转，否则读下一个簇
	
	push ax
	mov ax,[temp_ax]
	mov	word [isec_ls],ax
	add	word [isec_ls],1fh 	; 修改成即将访问的扇区号  
	pop ax
	jmp	searchrdir		; 继续搜索目录循环
.root:
	inc word [isec_ls]  ;对于根目录，只需自增
	jmp searchrdir		; 继续搜索目录循环
exit: ; 终止程序，返回
.9:
	call newline
	mov bp,fileNumberBuf1
	mov cx,fileNumberBufLen1
	call DispStr
	
	call space			; 插入空格符
	
	mov ax,[FileNum]
	call GetDigStr 		; 以AX为传递参数，[串地址]BP和[字符个数]CX为返回值
	call DispStr
	
	call space			; 插入空格符
	
	mov bp,fileNumberBuf2
	mov cx,fileNumberBufLen2
	call DispStr
	
	mov word[FileNum],0     ;显示完清零计数器
.10:
	push di
	mov di,FileSize-1ch
	call getsizestr		; 获取文件大小十进制串
	pop di
	
	mov bp, fsbuf		; 串地址
	mov cx, fsbuflen	; 串长
	; 显示文件大小字符串
	call DispStr		; 显示字符串
	call space			; 插入空格符
	; 显示字节字符串（文件大小单位）"Byte"
	mov bp, btbuf		; 串地址
	mov cx, btbuflen	; 串长
	call DispStr		; 显示字符串
	mov dword[FileSize],0     ;显示完清零计数器
	ret
temp_ax dw 0
isec_ls dw 0;当前扇区（用于ls）	
nsec_ls dw 0;剩余扇区数（用于ls）
isec dw 0	; 当前扇区号
nsec dw 0	; 剩余扇区数
lns dw 0	; 定义行数，初值为0
FileNum dw 0	; 文件个数，初值为0
FileSize dd 0   ; 文件总大小，初值为0
secspt dw 0	; 每磁道扇区数
heads dw 0	; 磁头数

fsbuf db '0,987,654,321' ; 文件大小串
fsbuflen equ $ - fsbuf ; 串长
dsbuf db  '            <DIR>          ' ; 子目录标识串
;dsbuflen equ $ - dsbuf ; 串长
volbuf db '            <VOL>          ' ; 卷标标识串
;volbuflen equ $ - volbuf
btbuf db 'Byte' ; 字节字符串
btbuflen equ $ - btbuf ; 串长
fileNumberBuf1 db '  ALL'
fileNumberBufLen1 equ $-fileNumberBuf1
fileNumberBuf2 db 'files.'
fileNumberBufLen2 equ $-fileNumberBuf2
;--------------------------------------------------------------------
getsizestr: ; 获取文件大小十进制串
	; 用空格符（20h）填充fsbuf
	push di			; 保存DI到栈
	mov cx, fsbuflen; 循环次数CX=命令行缓冲区fsbuf的长度
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, fsbuf	; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	pop di			; 从栈恢复DI

	; 计算文件大小十进制串
	mov cx, 0		; 当前分段数字个数（初始化为0）
	mov bp, fsbuf	; BP = fsbuf + fsbuflen - 1 = fsbuf的当前位置
	add bp, fsbuflen - 1 ; BP = 串尾
	mov ebx,10		; 除数=10
	mov eax, [di + 1Ch]; EAX = 文件大小
	
.1: ; 循环开始处
	mov edx, 0		; EDX = 0
	div ebx			; EDX:EAX / EBX -> 商EAX、余EDX
	add dl, 30h		; 余数 + 30h = 对应的数字符ASCII码
	mov [bp], dl	; fsbuf[BP] = DL
	cmp eax, 0		; 商EAX = 0 ?
	je .2			; = 0 跳出循环
	dec bp			; 数字符的当前位置BP--
	inc cx			; 当前分段数字个数++
	cmp cx, 3		; CX == 3 ？
	jne .1			; ≠ 继续循环
	; 添加逗号分隔符
	mov byte [bp], ',' ; 插入逗号分隔符“,”
	dec bp			; 数字符的当前位置BP--
	mov cx, 0		; 重新置CX=0
	jmp .1			; 继续循环
.2: ; 退出循环
	ret				; 从例程返回

;--------------------------------------------------------------------
waitforkey: ; 按任意键继续
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	; 显示提示串
	mov ah, 13h 	; BIOS中断的功能号（显示字符串）
	mov al, 1 		; 光标放到串尾
	mov bh, 0 		; 页号=0
	mov bl, 0fh 	; 字符颜色=不闪（0）黑底（000）亮白字（1111）
	mov bp, pkinstr	; BP=串地址
	mov cx, pkinstrlen; CX=串长
	mov dl, 0		; 列号=0
	int 10h 		; 调用10H号显示中断
	; 等待用户按键
	mov ah, 0		; 功能号（接受键盘字符输入）
	int 16h			; 调用16h键盘中断
	
	call newline	; 回车换行
	ret				; 从例程返回

pkinstr db 'Press any key to continue!' ; 提示用户键入的串
pkinstrlen equ $ - pkinstr ; 串长

;--------------------------------------------------------------------
waitforkey_chin: ; 按任意键继续
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	inc dh
	; 显示提示串
	mov ah, 42h 	; BIOS中断的功能号（显示字符串）
	mov al, 1 		; 光标放到串尾
	mov bh, 0 		; 页号=0
	mov bl, 0fh 	; 字符颜色=不闪（0）黑底（000）亮白字（1111）
	mov bp, pkinstr_chin	; BP=串地址
	mov cx, pkinstrlen_chin; CX=串长
	mov dl, 0		; 列号=0
	int 21h 		; 调用10H号显示中断
	; 等待用户按键
	mov ah, 0		; 功能号（接受键盘字符输入）
	int 16h			; 调用16h键盘中断
	
	call newline	; 回车换行
	ret				; 从例程返回

pkinstr_chin db '按任意键继续'
pkinstrlen_chin equ ($ - pkinstr_chin)/2 ; 串长
;--------------------------------------------------------------------
;----------------------------------------------------------------------------
; 函数名：ReadSector
;----------------------------------------------------------------------------
; 作用：从第 AX个扇区开始，将CL个扇区读入ES:BX中
ReadSector:
	; -----------------------------------------------------------------------
	; 怎样由扇区号求扇区在磁盘中的位置 (扇区号->柱面号、起始扇区、磁头号)
	; -----------------------------------------------------------------------
	; 设扇区号为 x
	;                           ┌ 柱面号 = y >> 1
	;       x           ┌ 商 y ┤
	;   -------------- 	=> ┤      └ 磁头号 = y & 1
	;  每磁道扇区数     │
	;                   └ 余 z => 起始扇区号 = z + 1
	push bp
	mov	bp, sp
	sub	sp, 2 		; 辟出两个字节的堆栈区域保存要读的扇区数: byte [bp-2]
	mov	byte [bp-2], cl
	push bx			; 保存BX
	mov	bl, [BPB_SecPerTrk]	; BL为除数
	div	bl			; AX/BL，商y在AL中、余数z在AH中
	inc	ah			; z ++（因磁盘的起始扇区号为1）
	mov	cl, ah		; CL <- 起始扇区号
	mov	dh, al		; DH <- y
	shr	al, 1			; y >> 1 （等价于y/BPB_NumHeads，软盘有2个磁头）
	mov	ch, al		; CH <- 柱面号
	and	dh, 1		; DH & 1 = 磁头号
	pop	bx			; 恢复BX
	; 至此，"柱面号、起始扇区、磁头号"已全部得到
	mov	dl, 0; 驱动器号（0表示软盘A）
.GoOnReading:
	mov	ah, 2			; 读扇区
	mov	al, byte [bp-2]	; 读AL个扇区
	int	13h			; 磁盘中断
	jc	.GoOnReading; 如果读取错误，CF会被置为1，
					; 这时就不停地读，直到正确为止
	add	sp, 2			; 栈指针+2
	pop	bp

	ret
;----------------------------------------------------------------------------
; 大型辅助例程结束
; -------------------------------------------------------------------
getstrln0: ; 获取键盘输入的命令串行
	cld				; 清除方向标志位（使扫描字符串方向为从串首到串尾）
	
	; 用空格符（20h）填充buf
	mov cx, buflen	; 循环次数CX=命令行缓冲区buf的长度（buflen=80）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, buf		; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	; 用空格符（20h）填充fnbuf的前8个字节
	mov cx, cslen	; 循环次数CX=命令串最大的长度（cslen=8）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, fnbuf	; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	mov si, 0		; 当前字符偏移位置 SI = 0
keyin0: ; 接受键盘输入
	; 读按键（返回的按键ASCII码在AL中）
	mov ah, 0 		; 功能号
	int 16h 		; 调用16H号中断
	; 对回车符（0DH）结束输入
	cmp al, 0dh 	; 比较AL中的键入字符与回车符（ASCII码为0DH）
	je return0 		; 相等跳转到从例程返回
	cmp al, 08h
	je backspace0
	; 保存按键字符到buf
	mov [buf + si], al; buf[SI]=AL
	inc si			; SI++
	; 太长时跳出
	cmp si, 21	; SI >= 80 ?
	jae goout0		; >= 时跳转
	jmp next_k0
	
backspace0:
	cmp si,0        ;没有输入的字符跳转继续输入
	je keyin0
	
	dec si
	mov byte [buf + si], 20h; 填入空格
	
	; 显示字符串例程（需先置串长CX和串地址BP）
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	pusha
	mov cx,1       ; 串长1
	mov bp,blank   ; 串地址
	push cx			; 保护CX（进栈）
	mov ah, 3		; 功能号
	mov bh, 0		; 第0页
	int 10h 		; 调用10H号显示中断
	pop cx			; 恢复CX（出栈）
	;10	2	置光标位置	BH=页号
    ;DH,DL=行,列
	
	dec dl          ; 退格
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx
	;dec dl          ; 再退一格
	; 在当前位置显示字符串（串长CX和串地址BP已预先设置好了）
	mov ah, 13h		; BIOS中断的功能号（显示字符串）
	mov al, 1 		; 光标放到串尾
	mov bh, 0 		; 页号=0
	mov bl, 0fh		; 字符颜色=不闪（0）黑底（000）亮白字（1111）
	int 10h 		; 调用10H号显示中断
	
	push dx
	mov ah,2
	mov bh,0
	int 10h
	pop dx	
	popa
	jmp keyin0
	; 显示AL中的键入字符
return0:
	ret 			; 从例程返回
	
next_k0
	pusha
	mov al,'*'
	mov ah, 0eh 	; 功能号
	mov bh,0
	mov bl, 0fh 	; 亮白字
	int 10h 		; 调用10H号中断
	popa
	jmp keyin0		; 循环读存显按键
	
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
	call DispStr_Chinese         ;显示大炮
	
	
	mov dh,25
	mov dl,2
	mov ah,2
	mov bh,0
	int 10h

	mov bl,0fh
	mov bp,OSver_str1
	mov cx,OSver_str1_len
	call DispStr_Chinese         ;显示大炮
	
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	mov bh,0
	inc dl
	int 10h
	
	mov bp,OSver_str2
	mov cx,OSver_str2_len
	call DispStr 				 ;显示OS信息
	
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
	call DispStr_Chinese          ;显示请输入密码
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
	;校验密码
	mov si,passwordBuf
	mov di,passwordStr
	mov	cx, 16			; 初始循环次数为4 pin码
	repe cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
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
keyinPass db '请输入密码'
keyinPasslen equ ($-keyinPass)/2

longPass db '密码太长'
longPasslen equ ($-longPass)/2
welcomePass db '欢迎使用'
welcomePasslen equ ($-welcomePass)/2	
wrongPass db '密码错误'
wrongPasslen equ ($-wrongPass)/2
spaceStr:    db '                        '
passwordBuf: db '                        '
passwordStr: db '1997                    '
; ------------------------------------------------------------------
CopyPassword:	    ; 构造新串（命令串 --> 密码）
	mov si, buf		; 源串起始地址
	mov di, passwordBuf	; 目的串起始地址
	mov cx, 21		; 循环次数 CX = n
	; 将输入缓冲区buf中的命令串复制到文件名缓冲区fnbuf：
	rep movsb		; CX > 0时 [ES:DI] = [DS:SI]、CX--，CX = 0时退出循环
	
	ret 			; 从例程返回
	
passwordStr_temp1 db '                        '
passwordStr_temp2 db '                        '
; ===============================================================================
;-------------------------------------------------------------------------------
; 改变用户密码
;提示串
PleaseEnterPassStr db '请输入密码'
PleaseEnterPassStrLen equ ($-PleaseEnterPassStr)/2
PleaseEnterPassStr0 db '再次输入密码以确认'
PleaseEnterPassStrLen0 equ ($-PleaseEnterPassStr0)/2
PleaseEnterPassStr1 db '两次密码不一致，请重新输入'
PleaseEnterPassStrLen1 equ ($-PleaseEnterPassStr1)/2
ChangePassword:
;可能变动的数据
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
	call DispStr_Chinese          ;显示请输入密码1
	call newline
	;获取密码1
	call getstrln0
	call CopyPassword
	;复制到passwordStr_temp1
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
	call DispStr_Chinese          ;显示请输入密码2
	call newline
	;获取密码2
	call getstrln0
	call CopyPassword
	;复制到passwordStr_temp2
	mov si,passwordBuf
	mov di,passwordStr_temp2
	mov cx,16
	rep movsb
	
	
	;校验密码
	mov si,passwordStr_temp1
	mov di,passwordStr_temp2
	mov	cx, 16			; 初始循环次数为4 pin码
	rep cmpsb			; 重复比较字符串中的字符，CX--，直到不相等或CX=0
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
	call DispStr_Chinese          ;显示错误信息
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
	mov	cx, 16			; 设置新密码
	rep movsb
	
	add sp,2
	call SignIn
	jmp again
;-------------------------------------------------------------------------------
; 改变用户名
;提示串
PleaseEnterUserNameStr db '请输入新的用户名'
PleaseEnterUserNameStrLen equ ($-PleaseEnterUserNameStr)/2
ChangeUserName:
	pusha
	; 用空格符（20h）填充UserNameBuf
	mov cx, 16	; 循环次数CX=命令行缓冲区buf的长度（buflen=80）
	mov al, 20h		; AL=要填充的空格符ASCII码
	mov di, UserNameBuf		; ES:DI=字符串的起始地址
	rep stosb		; CX>0时将AL存储到[ES:DI]，CX--、DI++
	
	mov cx,buflen
	mov bp,buf
	add bp,5   ;跳过cuser  五个字符
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
;图形辅助函数
a_x dw 0
a_y dw 0
b_x dw 0
b_y dw 0
c_x dw 0
c_y dw 0
OSver_str1 db '大炮'
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

	call _dc      ;显示实时时钟
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
drawRectangle: ;画矩形 先后push三个角
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
drawLine:      ;起始坐标 BX  结束坐标 DX  不变坐标 AX  CX划线方向 0水平 1竖直
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