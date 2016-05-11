org 100h
;---------------------------------
;put_one_point宏,
;利用int10h,AH= 0ch的写点（像素点）
;---------------------------------
%MACRO put_one_point 3
;x,y,color        
    push    ax
    push    cx
    push    dx
     
        mov ah,0Ch
        mov al,%3
        mov cx,%1
        mov dx,%2
        int 10h
        pop    dx
        pop    cx
        pop    ax
%ENDMACRO
;----------------------------------
 ; --------------------------------------------------------------------
%MACRO DispStr 4 ; 显示字符串例程（需先置串长CX和串地址BP）  x,y,length,offset
	; 获取当前光标位置（返回的行列号分别在DH和DL中）
	;push cx			; 保护CX（进栈）
	;mov ah, 3		; 功能号
	;mov bh, 0		; 第0页
	;int 10h 		; 调用10H号显示中断
	;pop cx			; 恢复CX（出栈）

	; 在指定位置显示字符串（串长CX和串地址BP已预先设置好了）
	mov dh,%1
	mov dl,%2
	mov cx,%3
	mov bp,%4
	mov ah, 13h		; BIOS中断的功能号（显示字符串）
	mov al, 1 		; 光标放到串尾
	mov bh, 0 		; 页号=0
	mov bl, 0fh		; 字符颜色=不闪（0）黑底（000）亮白字（1111）
	int 10h 		; 调用10H号显示中断
%ENDMACRO	
; --------------------------------------------------------------------
	mov ax, cs
	mov ds, ax
	mov es, ax
	mov ss, ax
	;jmp next
    ;;设置显示模式640*480*16色图形模式
    mov    ax,11h
    int    10h
 
    ;设置调色板
    mov    ah,0bh
    mov    bh,0
    mov    bl,1
    int    10h
    
	call drawTEST
next:
	call helloWorld
	call waitESC
	mov ah,4ch
	int 21h
helloWorld:
	DispStr 10,10,strlen,strhello
	ret
waitESC:
.1
	mov ah,00h
	int 16h
	cmp al,1bh
	jnz .1
	ret
drawTEST:
	mov word [m_x],100
	mov word [m_y],100
	mov cx,100
;loop1:                    ;斜线
    ;inc word [m_x]
	;inc word [m_y]
    ;put_one_point    [m_x],[m_y],[m_color]
    ;loop    loop1
	
	;mov cx,100
	mov cx,100
loop1:                    ;-线
    inc word [m_x]
    put_one_point    [m_x],[m_y],[m_color]
    loop    loop1
	
	mov cx,100
loop2:                    ;|线
	inc word [m_y]
    put_one_point    [m_x],[m_y],[m_color]
    loop    loop2
	
	mov cx,100
loop3:                    ;-线
    inc word [m_x]
    put_one_point    [m_x],[m_y],[m_color]
    loop    loop3
	
	mov cx,100
loop4:                    ;|线
	dec word [m_y]
    put_one_point    [m_x],[m_y],[m_color]
    loop    loop4
	
	mov word [m_x],400
	mov word [m_y],300
	mov cx,100
.1:                    ;斜线
    inc word [m_x]
	inc word [m_y]
    put_one_point    [m_x],[m_y],[m_color]
    loop    .1
	
	mov word [m_x],500
	mov word [m_y],300
	mov cx,100
.2:                    ;斜线
    dec word [m_x]
	inc word [m_y]
    put_one_point    [m_x],[m_y],[m_color]
    loop    .2	
	ret
;------------------------------
;以下代码段中用到的变量
;所要实现的功能为在第100列上显示一条长为400像素点的直线
;但实际在该位置只显示了300像素点，另外的长100像素点的直线出现在屏幕的右上
;------------------------------
m_x    dw    0    ;直线的起始列
m_y    dw    0    ;直线的起始行
m_color    db    3
strhello db 'Hello LWM!'
strlen equ $ - strhello
;-----------------------------
