.model tiny
.code 
.386
org 100h    
    
     
            
main:     

    jmp installer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defaultINT9         dd  0 

finishWritingMessage    db  13, 10, 'Finish file writing.', '$'

cmdMaxLength        equ 126                       
filePath            db  cmdMaxLength + 1 dup(0)  
filePathLength      db  ? 
fileID              dw  ?
filePosition        dw  2 dup(0)
bufferChar          db  ? 

printMessage macro string
    push cs
    pop ds
    mov ah, 9
    lea dx, string 
    int 21h 
endm

handlerINT9 proc far
    pushf
    call cs:defaultINT9    
    
    pusha
    push ds
    push es  
    
    push cs
    pop ds
    
    in al, 60h      
    
    cmp al, 80h
    jae endHandler
    
    cmp al, 1
    jne workWithFile
    
    printMessage finishWritingMessage
    
    cli
    mov dx, word ptr [offset defaultINT9] 
    mov ax, word ptr [offset defaultINT9 + 2]
    mov ds, ax
    mov ah, 25h
    mov al, 09h 
    int 21h
    sti   

    mov ah, 49h
	push cs
	pop es
    int 21h
    jmp endHandler
    
    workWithFile:
    mov  byte ptr bufferChar, al
    call saveInFile
    
    endHandler: 
    mov al, 20h
    out 20h, al       
    pop es
    pop ds
    popa   
         
    iret
handlerINT9 endp

openFile proc
    mov ah, 3dh
    mov al, 00000001b   ;write mode
    mov cl, 00000000b
    mov dx, offset filePath
    int 21h
    mov fileID, ax  
    jnc openFileSuccessfully
    
    jmp endHandler    
    
    openFileSuccessfully:  
    ret
openFile endp

closeFile proc
    mov ah, 3eh
    mov bx, fileID
    int 21h
    jnc closeFileSuccessfully      
    
    jmp endHandler
    
    closeFileSuccessfully:
    ret
closeFile endp

setPointer proc 
    ;si = offset shift  
    mov ah, 42h  
    mov al, 0
    mov bx, fileID
    mov cx, [si]
    mov dx, [si + 2]
    int 21h        
    jnc setPointerSuccessfully    
    
    jmp endHandler
    
    setPointerSuccessfully: 
    ret
setPointer endp  

incIndex proc
    ;si = offset index      
        cmp word ptr [si + 2], 0ffffh
        jne incSmallPart
    incBigPart:
        mov ax, [si]
        add ax, 1
        mov word ptr [si], ax
        mov ax, 0
        mov word ptr [si + 2], ax
        ret
    incSmallPart:
        mov ax, [si + 2]
        add ax, 1
        mov word ptr [si + 2], ax
        ret    
incIndex endp

writeFile proc
    ;dx = offset buffer, cl = length       
    mov ah, 40h
    mov bx, fileID
    xor ch, ch
    int 21h 
    jnc writeFileSuccessfully  
    
    jmp endHandler
    
    writeFileSuccessfully: 
    ret
writeFile endp

saveInFile proc
    call openFile     
    
    mov si, offset filePosition 
    call setPointer  
    
    mov cl, 1
    mov dx, offset bufferChar
    call writeFile           
    
    mov si, offset filePosition 
    call incIndex 
    
    call closeFile   
    ret    
saveInFile endp 

endResidentPart:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

emptyCmdErrorMessage          db  'Error. Cmd is empty.', 13, 10, '$'
excessCharsErrorMessage       db  'Error. File path contains excess characters.', 13, 10, '$'
filePathErrorMessage          db  'Error. File is not found.', 13, 10, '$'
fileExtensionMessage          db  'Error. File must be TXT.', 13, 10, '$'

fileIsFoundMessage            db  'File is found.', 13, 10, '$'
setHandlerMessage             db  'Set new int9 handler.', 13, 10, '$'
storeResPartMessage           db  'Store resident part.', 13, 10, '$'
   
copyFilePath proc 
    push cs
    pop es         
	
	fillFilePathLength:
    	xor cx, cx        
    	mov si, 80h
    	mov cl, ds:[si]
    	dec cl
    	mov es:filePathLength, cl
	
	checkEmptyCmd:                
    	cmp cl, 0ffh
    	jne fillFilePath
    	mov ds, ax 
    	printMessage emptyCmdErrorMessage
        jmp exit
	
	fillFilePath:
    	add si, 2
    	mov di, offset filePath
    	rep movsb 
    	  
    push cs
    pop ds 
    ret
copyFilePath endp
        
findFile macro 
    mov ah, 4eh
	mov cx, 0			
	mov dx, offset filePath
	int 21h
endm 
      
checkFilePath proc 
	checkExcessCharsInFilePath: 
	    mov si, offset filePath
        xor ax, ax
        mov al, filePathLength       
        add si, ax
	    dec si 
	    mov bl, byte ptr [si]
        mov byte ptr [si], 0
        mov byte ptr [si + 1], 0
        findFile 	
    	jc tryFindFile                      
    	                                        
    	printMessage excessCharsErrorMessage
        jmp exit
                  
    tryFindFile:
        mov byte ptr [si], bl
        findFile
        jnc isTxtFile 
        
        printMessage filePathErrorMessage
        jmp exit 
        
    isTxtFile:
        cmp bl, 't'
        je fileIsFound
        
        printMessage fileExtensionMessage
        jmp exit
            
    fileIsFound:         
        printMessage fileIsFoundMessage 
        ret
checkFilePath endp 
    
installer:                      
    call copyFilePath 
    call checkFilePath
    
    printMessage setHandlerMessage
    cli 
    mov ah, 35h
    mov al, 09h
    int 21h    
    mov word ptr [offset defaultINT9], bx
    mov word ptr [offset defaultINT9 + 2], es
    mov ah, 25h
    mov al, 09h 
    mov dx, offset handlerINT9
    int 21h        
    sti
    
    printMessage storeResPartMessage
    mov ah, 31h
    mov al, 00h
    mov dx, (endResidentPart - main + 100h) / 16 + 1
    int 21h 
    
    exit:
    mov ax, 4c00h
    int 21h
end main







;;;;;;;;;;;;;;;;;;      video + path + cmd
;.model tiny
;.code 
;.386
;org 100h    
;    
;     
;            
;main:     
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;defaultINT9         dd  0   
;
;aa dw ?;;;;;;;;;;;;  
;
;cmdMaxLength        equ 126                       
;filePath            db  50 dup(0);;try not 0 !!!!!!!!!!!!   
;filePathLength      db  ? 
;fileID              dw  ?
;filePosition        dw  2 dup(0)
;
;bufferChar          db  ?
;  
;handlerINT9 proc far
;    pushf
;    call cs:defaultINT9
;    pusha
;    push ds
;    push es
;    push cs
;    pop ds
;    
;    mov ax, 0B800h
;    mov es, ax        
;    
;;    mov ah, 0
;;    int 16h 
;    in al, 60h      
;    
;    cmp al, 80h
;    jae nexttt
;    
;    
;    
;    mov bx, 0111010001100101b
;    mov bl, al   
;    mov si, aa    
;    mov word ptr es:[si], bx
;    add si, 4
;    mov  word ptr aa, si
;    
;;       mov al, 20h
;;    out 20h, al 
;    nexttt:
;    pop es
;    pop ds
;    popa     
;    
;    iret
;handlerINT9 endp
;
;endResidentPart:
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   
;printMessage macro string
;    mov ah, 9
;    lea dx, string 
;    int 21h 
;;    mov ah, 2
;;    mov dl, '+'
;;    int 21h 
;endm
;
;copyFilePath proc  
;        
;    mov ah, 2
;    mov dl, 'a'   ;;;;;;;;;;;;;
;    int 21h 
;    
;    push cs
;    pop es           
;	;mov es, ax     
;	
;	fillFilePathLength:
;    	xor cx, cx        
;    	mov si, 80h
;    	mov cl, ds:[si]
;    	dec cl
;    	mov es:filePathLength, cl
;	
;	checkEmptyCmd:                
;    	cmp cl, 0ffh
;    	jne fillFilePath
;    	mov ds, ax 
;    	;printMessage emptyCmdErrorMessage
;        jmp exit
;	
;	fillFilePath:
;    	add si, 2
;    	mov di, offset filePath
;    	rep movsb                      
;    
;    ;mov ds, ax     
;    push cs
;    pop ds 
;    ret
;copyFilePath endp
;        
;findFile macro 
;    mov ah, 4eh
;	mov cx, 0			
;	mov dx, offset filePath
;	int 21h
;endm 
;      
;checkFilePath proc  
;    mov ah, 2
;    mov dl, 'b'   ;;;;;;;;;;;;;
;    int 21h
;             
;	checkExcessCharsInFilePath: 
;	    mov si, offset filePath
;        xor ax, ax
;        mov al, filePathLength       
;        add si, ax
;	    dec si 
;	    mov bl, byte ptr [si]
;        mov byte ptr [si], 0
;        mov byte ptr [si + 1], 0
;        findFile 	
;    	jc tryFindFile 
;    	
;    	push ax
;    	push dx                                        
;    	mov ah, 2
;    mov dl, 'c'   ;;;;;;;;;;;;;
;    int 21h        
;        pop dx
;        pop ax                                 
;    	                                        
;    	;printMessage excessCharsErrorMessage
;        jmp exit
;                  
;    tryFindFile:
;        mov byte ptr [si], bl
;        findFile
;        jnc isTxtFile   
;        
;        push ax
;        push dx                                        
;    	mov ah, 2
;    mov dl, 'd'   ;;;;;;;;;;;;;
;    int 21h        
;        pop dx
;        pop ax 
;        
;        ;printMessage filePathErrorMessage
;        jmp exit 
;        
;    isTxtFile:
;        cmp bl, 't'
;        je fileIsFound
;        
;        push ax
;        push dx                                        
;    	mov ah, 2
;    mov dl, 'e'   ;;;;;;;;;;;;;
;    int 21h        
;        pop dx
;        pop ax 
;        
;        ;printMessage filePathErrorMessage
;        jmp exit
;            
;    fileIsFound:         
;        ;printMessage fileIsFoundMessage 
;        ret
;checkFilePath endp 
;
;installer:
;
;    ;mov ax, @data               
;    call copyFilePath 
;    call checkFilePath
;    
;    mov ah, 2
;    mov dl, '+'   ;;;;;;;;;;;;;
;    int 21h 
;    
;    cli
;
;    mov ah, 35h
;    mov al, 09h
;    int 21h    
;    mov word ptr [offset defaultINT9], bx
;    mov word ptr [offset defaultINT9 + 2], es
;    
;    mov ah, 25h
;    mov al, 09h 
;    mov dx, offset handlerINT9
;    int 21h 
;    
;    sti
;    
;    
;    
;    ;;;;;;;;;;;;;;;;;;;;
;    mov ax, 0B800h
;    mov es, ax
;    mov ax, 0101100101100101b
;    mov word ptr es:[2520], ax;;;;;;;;;;; 
;    mov word ptr es:[2524], ax;;;;;;;;;;;      
;    mov  word ptr aa, 2720  
;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    
;    mov ah, 31h
;    mov al, 00h
;    mov dx, (endResidentPart - main + 100h) / 16 + 1
;    int 21h 
;    
;    exit:
;    mov ax, 4c00h
;    int 21h
;end main






;;;;;;;;;;;;;;;;;;;;;       cmd + videomem + 60h
;.model tiny
;.code 
;.386
;org 100h    
;    
;     
;            
;main:     
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;defaultINT9         dd  0
;;st db "qwertyuggg$"
;
;
;;bufferChar          db  ?   
;aa dw ?  
;
;;aaaa db 65
;  
;handlerINT9 proc far
;    pushf
;    call cs:defaultINT9
;    pusha
;    push ds
;    push es
;    push cs
;    pop ds
;    
;    mov ax, 0B800h
;    mov es, ax        
;    
;;    mov ah, 0
;;    int 16h 
;    in al, 60h      
;    
;    cmp al, 80h
;    jae nexttt
;    
;    
;    
;    mov bx, 0111010001100101b
;    mov bl, al   
;    mov si, aa    
;    mov word ptr es:[si], bx
;    add si, 4
;    mov  word ptr aa, si
;    
;;       mov al, 20h
;;    out 20h, al 
;    nexttt:
;    pop es
;    pop ds
;    popa     
;    
;    iret
;handlerINT9 endp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;installer:  
;    cli
;
;    mov ah, 35h
;    mov al, 09h
;    int 21h    
;    mov word ptr [offset defaultINT9], bx
;    mov word ptr [offset defaultINT9 + 2], es
;    
;    mov ah, 25h
;    mov al, 09h 
;    mov dx, offset handlerINT9
;    int 21h 
;    
;    sti
;    
;;    push cs
;;    pop ds
;;    mov ah, 9;;;;;;;;;;;;
;;    mov dx, offset st
;;    int 21h
;    
;    mov ax, 0B800h
;    mov es, ax
;    mov ax, 0101100101100101b
;    mov word ptr es:[2520], ax;;;;;;;;;;; 
;    mov word ptr es:[2524], ax;;;;;;;;;;;      
;    mov  word ptr aa, 2720
;    ;mov byte ptr aaaa, 67
;    
;    mov ah, 31h
;    mov al, 00h
;    mov dx, (installer - main + 100h) / 16 + 1
;    int 21h  
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101000101100101b
;;    mov word ptr es:[2530], ax;;;;;;;;;;; 
;;    mov word ptr es:[2534], ax;;;;;;;;;;;
;
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;    
;end main





;;;;;;;;;;;;;;;;;;;;;;      q
;.model tiny
;.code 
;org 100h    
;    
;                        
;                        
;            
;main:     
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;aa dw ?
;defaultINT9         dd  0
;
;                    
;handlerINT9 proc
;    mov ah, 2
;    mov dl, '*'
;    int 21h
;         
;    pushf
;    call cs:defaultINT9  
;    
;    
;    
;    push    ds       
;    push    es
;	push    ax
;	push    bx
;    push    cx             
;    push    dx
;	push    di
;
;	push cs
;	pop ds
;             
;             
;             
;;    mov ax, 0B800h
;;    mov es, ax
;    ;mov si, aa 
;;;    
;;    mov ah, 0
;;   int 16h 
;   
;   
;    ;mov cx, aa 
;    ;mov bx, 0111010001100101b
;    ;mov bl, cl 
;    ;mov word ptr es:[si], bx
;    ;mov bl, cl 
;    ;mov word ptr es:[si + 2], bx
;    
;    mov ax, aa    
;    add ax, 1
;    mov  word ptr aa, ax
;;    
;;    
;;    
;;;    
;;;    
;;;    mov ah, 2
;;;    mov dl, al
;;;    ;add dl, 20
;;;    int 21h
;;    
;;    
;;    
;;    mov ax, aa    
;    cmp ax, 8
;    jb nextt
;;            
;;    mov bx, 0101010001100101b
;;    mov word ptr es:[si + 2], bx   
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    
;    
;    cli
;    mov dx, word ptr [offset defaultINT9] 
;    mov ax, word ptr [offset defaultINT9 + 2]
;    mov ds, ax
;    mov ah, 25h
;    mov al, 09h 
;    int 21h
;    sti   
;
;;mov ah, 4Ah
;;	mov bx, 0
;;    int 21h
;
;    mov ah, 49h
;	push cs
;	pop es
;    int 21h
;    
;;    mov ah, 2
;;    mov dl, '+'
;;    int 21h
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;;    iret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;    nextt:
;    	pop     di
;	pop     dx
;	pop     cx
;	pop     bx
;	pop     ax
;	pop     es
;	pop     ds	     
;	
;   mov al, 20h
;    out 20h, al 
;
;	iret
;handlerINT9 endp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;installer:
;    cli
;
;    mov ah, 35h
;    mov al, 09h
;    int 21h    
;    mov word ptr [offset defaultINT9], bx
;    mov word ptr [offset defaultINT9 + 2], es
;    
;    push ds
;    pop es
;    
;    mov ah, 25h
;    mov al, 09h 
;    mov dx, offset handlerINT9
;    int 21h 
;    
;    sti
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101100101100101b
;;    mov word ptr es:[2520], ax;;;;;;;;;;; 
;;    mov word ptr es:[2524], ax;;;;;;;;;;;      
;;    mov  word ptr aa, 2720
;    
;    mov  word ptr aa, 0
;    
;    mov ah, 31h
;    mov al, 00h
;    mov dx, (installer - main + 100h) / 16 + 1
;    int 21h  
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101000101100101b
;;    mov word ptr es:[2530], ax;;;;;;;;;;; 
;;    mov word ptr es:[2534], ax;;;;;;;;;;;
;
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;
;    
;end main




;;;;;;;;;;;;;;;;;;;;;;;      cmd works + return
;.model tiny
;.code 
;org 100h    
;    
;                        
;                        
;            
;main:     
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;aa dw ?
;defaultINT9         dd  0
;
;                    
;handlerINT9 proc
;    mov ah, 2
;    mov dl, '*'
;    int 21h
;         
;    pushf
;    call cs:defaultINT9  
;    
;    
;    
;    push    ds       
;    push    es
;	push    ax
;	push    bx
;    push    cx             
;    push    dx
;	push    di
;
;	push cs
;	pop ds
;             
;             
;             
;;    mov ax, 0B800h
;;    mov es, ax
;    ;mov si, aa 
;;;    
;;    mov ah, 0
;;   int 16h 
;   
;   
;    ;mov cx, aa 
;    ;mov bx, 0111010001100101b
;    ;mov bl, cl 
;    ;mov word ptr es:[si], bx
;    ;mov bl, cl 
;    ;mov word ptr es:[si + 2], bx
;    
;    mov ax, aa    
;    add ax, 1
;    mov  word ptr aa, ax
;;    
;;    
;;    
;;;    
;;;    
;;;    mov ah, 2
;;;    mov dl, al
;;;    ;add dl, 20
;;;    int 21h
;;    
;;    
;;    
;;    mov ax, aa    
;    cmp ax, 8
;    jb nextt
;;            
;;    mov bx, 0101010001100101b
;;    mov word ptr es:[si + 2], bx   
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    
;    
;    cli
;    mov dx, word ptr [offset defaultINT9] 
;    mov ax, word ptr [offset defaultINT9 + 2]
;    mov ds, ax
;    mov ah, 25h
;    mov al, 09h 
;    int 21h
;    sti   
;
;;mov ah, 4Ah
;;	mov bx, 0
;;    int 21h
;
;    mov ah, 49h
;	push cs
;	pop es
;    int 21h
;    
;;    mov ah, 2
;;    mov dl, '+'
;;    int 21h
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;;    iret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;    nextt:
;    	pop     di
;	pop     dx
;	pop     cx
;	pop     bx
;	pop     ax
;	pop     es
;	pop     ds	     
;	
;   mov al, 20h
;    out 20h, al 
;
;	iret
;handlerINT9 endp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;installer:  
;    cli
;
;    mov ah, 35h
;    mov al, 09h
;    int 21h    
;    mov word ptr [offset defaultINT9], bx
;    mov word ptr [offset defaultINT9 + 2], es
;    
;    push ds
;    pop es
;    
;    mov ah, 25h
;    mov al, 09h 
;    mov dx, offset handlerINT9
;    int 21h 
;    
;    sti
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101100101100101b
;;    mov word ptr es:[2520], ax;;;;;;;;;;; 
;;    mov word ptr es:[2524], ax;;;;;;;;;;;      
;;    mov  word ptr aa, 2720
;    
;    mov  word ptr aa, 0
;    
;    mov ah, 31h
;    mov al, 00h
;    mov dx, (installer - main + 100h) / 16 + 1
;    int 21h  
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101000101100101b
;;    mov word ptr es:[2530], ax;;;;;;;;;;; 
;;    mov word ptr es:[2534], ax;;;;;;;;;;;
;
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;    
;end main
;










;;;;;;;;;;;;;           return default
;.model tiny
;.code 
;.386
;org 100h    
;    
;     
;            
;main:     
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;defaultINT9         dd  0
;;bufferChar          db  ?   
;aa dw ?
;  
;handlerINT9 proc far
;    pushf
;    call cs:defaultINT9
;    pusha
;    push ds
;    push es
;    push cs
;    pop ds
;    
;    mov ax, 0B800h
;    mov es, ax
;    mov si, aa 
;    
;    mov ah, 0
;    int 16h       
;    
;    mov bx, 0111010001100101b
;    mov bl, al 
;    mov word ptr es:[si], bx
;    mov bl, ah 
;    mov word ptr es:[si + 2], bx
;    
;    add si, 8
;    mov  word ptr aa, si
;    
;    cmp al, 'q'
;    jne next
;    
;;        mov ah, 2
;;    mov dl, '+'
;;    int 21h 
;    mov bx, 0101010001100101b
;    mov word ptr es:[si + 2], bx
;;;    
;;    mov dx, [offset defaultINT9] 
;;    mov ax, [offset defaultINT9 + 2]
;;    mov ds, ax
;;    mov ah, 25h
;;    mov al, 09h 
;;    int 21h
;;
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;;    iret
;    
;    next:
;    pop es
;    pop ds
;    popa    
;    iret
;handlerINT9 endp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;installer:  
;;    cli
;;
;;    mov ah, 35h
;;    mov al, 09h
;;    int 21h    
;;    mov word ptr [offset defaultINT9], bx
;;    mov word ptr [offset defaultINT9 + 2], es
;;    
;;    mov ah, 25h
;;    mov al, 09h 
;;    mov dx, offset handlerINT9
;;    int 21h 
;;    
;;    sti
;
;
;    ;cli
;    ;push ds
;    
;    mov ah, 35h
;    mov al, 09h
;    int 21h    
;    mov word ptr [offset defaultINT9], bx
;    mov word ptr [offset defaultINT9 + 2], es
;    
;;    mov ah, 25h
;;    mov al, 09h 
;;    mov dx, offset handlerINT9
;;    int 21h  
;    
;    ;pop ds
;    
;    mov dx, word ptr [offset defaultINT9] 
;    mov ax, word ptr [offset defaultINT9 + 2]
;    mov ds, ax
;;    mov dx, bx
;;    mov ax, es
;;    mov ds, ax
;    
;    
;    mov ah, 25h
;    mov al, 09h 
;    int 21h
;    
;    ;sti
;    
;    mov ax, 0B800h
;    mov es, ax
;    mov ax, 0101100101100101b
;    mov word ptr es:[2520], ax;;;;;;;;;;; 
;    mov word ptr es:[2524], ax;;;;;;;;;;;      
;    mov  word ptr aa, 2720
;    
;    
;    mov ah, 31h
;    mov al, 00h
;    mov dx, (installer - main + 100h) / 16 + 1
;    int 21h  
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101000101100101b
;;    mov word ptr es:[2530], ax;;;;;;;;;;; 
;;    mov word ptr es:[2534], ax;;;;;;;;;;;
;
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;    
;end main

          
          
          
          
          
          
;;;;;;;;;;;;;;;;;;   new int 9 + out key
;.model tiny
;.code 
;.386
;org 100h    
;    
;     
;            
;main:     
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;defaultINT9         dd  0
;;bufferChar          db  ?   
;aa dw ?
;  
;handlerINT9 proc far
;    pushf
;    call cs:defaultINT9
;    pusha
;    push ds
;    push es
;    push cs
;    pop ds
;    
;    mov ax, 0B800h
;    mov es, ax
;     
;    
;    mov ah, 0
;    int 16h       
;    
;    mov bx, 0111010001100101b
;    mov bl, al   
;    mov si, aa    
;    mov word ptr es:[si], bx
;    add si, 4
;    mov  word ptr aa, si
;    
;
;    pop es
;    pop ds
;    popa    
;    iret
;handlerINT9 endp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;installer:  
;    cli
;
;    mov ah, 35h
;    mov al, 09h
;    int 21h    
;    mov word ptr [offset defaultINT9], bx
;    mov word ptr [offset defaultINT9 + 2], es
;    
;    mov ah, 25h
;    mov al, 09h 
;    mov dx, offset handlerINT9
;    int 21h 
;    
;    sti
;    
;    mov ax, 0B800h
;    mov es, ax
;    mov ax, 0101100101100101b
;    mov word ptr es:[2520], ax;;;;;;;;;;; 
;    mov word ptr es:[2524], ax;;;;;;;;;;;      
;    mov  word ptr aa, 2720
;    
;    
;    mov ah, 31h
;    mov al, 00h
;    mov dx, (installer - main + 100h) / 16 + 1
;    int 21h  
;    
;;    mov ax, 0B800h
;;    mov es, ax
;;    mov ax, 0101000101100101b
;;    mov word ptr es:[2530], ax;;;;;;;;;;; 
;;    mov word ptr es:[2534], ax;;;;;;;;;;;
;
;;exit:                        
;;    mov ax, 4c00h
;;    int 21h
;    
;end main
 










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            separat code (resident/installer)
;.model tiny
;.code 
;org 100h    
;    
;     
;            
;main:     
;
;;mov ah, 2
;;    mov dl, '0'   ;;;;;;;;;;;;;
;;    int 21h 
;
;    jmp installer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;delete!!!!!!!!!!!!!!!!!!!!!
;
;openFileErrorMessage          db  'Open file error.', 13, 10, '$'
;closeFileErrorMessage         db  'Close file error.', 13, 10, '$'
;movePointerErrorMessage       db  'Move file pointer error.', 13, 10, '$'
;writeFileErrorMessage         db  'Write file error.', 13, 10, '$'
;    
;
;cmdMaxLength        equ 126                       
;filePath            db  cmdMaxLength + 1 dup(0)   
;filePathLength      db  ? 
;fileID              dw  ?
;filePosition        dw  2 dup(0)
;
;bufferChar          db  '3'    
;
;  
;printMessage macro string
;    mov ah, 9
;    lea dx, string 
;    int 21h 
;;    mov ah, 2
;;    mov dl, '+'
;;    int 21h 
;endm   
;
;openFile proc
;    mov ah, 3dh
;    mov al, 00000001b   ;read and write mode
;    mov cl, 00000000b
;    mov dx, offset filePath
;    int 21h
;    mov fileID, ax  
;    jnc openFileSuccessfully
;            
;    printMessage openFileErrorMessage
;    jmp exit    
;    
;    openFileSuccessfully:  
;    ret
;openFile endp
;
;closeFile proc
;    mov ah, 3eh
;    mov bx, fileID
;    int 21h
;    jnc closeFileSuccessfully 
;       
;    printMessage closeFileErrorMessage 
;    jmp exit
;    
;    closeFileSuccessfully:
;    ret
;closeFile endp
;
;setPointer proc 
;    ;si = offset shift  
;    mov ah, 42h  
;    mov al, 0
;    mov bx, fileID
;    mov cx, [si]
;    mov dx, [si + 2]
;    int 21h        
;    jnc setPointerSuccessfully          
;        
;    printMessage movePointerErrorMessage
;    jmp exit
;    
;    setPointerSuccessfully: 
;    ret
;setPointer endp  
;
;incIndex proc
;    ;si = offset index      
;        cmp [si + 2], 0ffffh
;        jne incSmallPart
;    incBigPart:
;        mov ax, [si]
;        add ax, 1
;        mov word ptr [si], ax
;        mov ax, 0
;        mov word ptr [si + 2], ax
;        ret
;    incSmallPart:
;        mov ax, [si + 2]
;        add ax, 1
;        mov word ptr [si + 2], ax
;        ret    
;incIndex endp
;
;writeFile proc
;    ;dx = offset buffer, cl = length       
;    mov ah, 40h
;    mov bx, fileID
;    xor ch, ch
;    int 21h 
;    jnc writeFileSuccessfully
;    
;    printMessage writeFileErrorMessage
;    jmp exit
;    
;    writeFileSuccessfully: 
;    ret
;writeFile endp
;
;
;saveInFile proc
;    call openFile     
;    
;    mov si, offset filePosition 
;    call setPointer  
;    
;    mov cl, 1
;    mov dx, offset bufferChar
;    call writeFile           
;    
;    mov si, offset filePosition 
;    call incIndex  
;
;mov ah, 2
;    mov dl, 'Q'   ;;;;;;;;;;;;;
;    int 21h
;    
;    call closeFile   
;    ret    
;saveInFile endp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;installer: 
;           
;             
;           
;   
;;
;;    mov ah, 2
;;    mov dl, '2'   ;;;;;;;;;;;;;
;;    int 21h 
;    
;    mov ax, @data               
;    call copyFilePath   
;                  
;    mov ah, '&'
;    mov bufferChar, ah 
;    mov ah, 2 
;    mov dl, bufferChar   ;;;;;;;;;;;;;
;    int 21h  
;    
;    mov ax, 0
;    mov word ptr [offset filePosition], ax 
;    mov word ptr [offset filePosition + 2], ax                         
;
;;    mov ah, 2
;;    mov dl, filePathLength   ;;;;;;;;;;;;;   
;;    add dl, 20
;;    int 21h 
;;        mov ah, 9
;;    lea dx, filePath  ;;;;;;;;;;;;;
;;    int 21h 
;    
;    call checkFilePath  
;        
;        mov ah, 2
;    mov dl, '2'   ;;;;;;;;;;;;;
;    int 21h 
;        
;    call saveInFile
;    call saveInFile
;    call saveInFile
;    call saveInFile 
;    call saveInFile
;    
;    mov ah, 2
;    mov dl, '3'   ;;;;;;;;;;;;;
;    int 21h 
;    
;exit:
;
;mov ah, 2
;    mov dl, '4'   ;;;;;;;;;;;;;
;    int 21h 
;                  
;    mov ax, 4c00h
;    int 21h
;    
;;    ;;;;;;;;;;;;;;;;
;;emptyCmdErrorMessage          db  'Error. Cmd is empty.', 13, 10, '$'
;;excessCharsErrorMessage       db  'Error. File path contains excess characters.', 13, 10, '$'
;;filePathErrorMessage          db  'Error. File is not found.', 13, 10, '$'
;;
;;fileIsFoundMessage            db  'File is found.', 13, 10, '$'
;
;    ;;;;;;;;;;;;;;;;
;emptyCmdErrorMessage          db  'q$'
;excessCharsErrorMessage       db  'q$'
;filePathErrorMessage          db  'q$'
;
;fileIsFoundMessage            db  'q$'
;         
;       
;
;         
;copyFilePath proc  
;        
;    mov ah, 2
;    mov dl, 'a'   ;;;;;;;;;;;;;
;    int 21h 
;             
;	mov es, ax     
;	
;	fillFilePathLength:
;    	xor cx, cx        
;    	mov si, 80h
;    	mov cl, ds:[si]
;    	dec cl
;    	mov es:filePathLength, cl
;	
;	checkEmptyCmd:
;    	cmp cl, 0ffh
;    	jne fillFilePath
;    	mov ds, ax 
;    	printMessage emptyCmdErrorMessage
;        jmp exit
;	
;	fillFilePath:
;    	add si, 2
;    	mov di, offset filePath
;    	rep movsb                      
;    
;    mov ds, ax    
;    ret
;copyFilePath endp
;        
;;        mov ah, 2
;;    mov dl, 'a'   ;;;;;;;;;;;;;
;;    int 21h
;        
;findFile macro 
;    mov ah, 4eh
;	mov cx, 0			
;	mov dx, offset filePath
;	int 21h
;endm 
;      
;;      mov ah, 2
;;    mov dl, 'b'   ;;;;;;;;;;;;;
;;    int 21h
;      
;checkFilePath proc  
;    mov ah, 2
;    mov dl, 'b'   ;;;;;;;;;;;;;
;    int 21h
;             
;	checkExcessCharsInFilePath: 
;	    mov si, offset filePath
;        xor ax, ax
;        mov al, filePathLength       
;        add si, ax
;	    dec si 
;	    mov bl, byte ptr [si]
;        mov byte ptr [si], 0
;        mov byte ptr [si + 1], 0
;        findFile 	
;    	jc tryFindFile 
;    	
;    	push ax
;    	push dx                                        
;    	mov ah, 2
;    mov dl, 'c'   ;;;;;;;;;;;;;
;    int 21h        
;        pop dx
;        pop ax                                 
;    	                                        
;    	printMessage excessCharsErrorMessage
;        jmp exit
;                  
;    tryFindFile:
;        mov byte ptr [si], bl
;        findFile
;        jnc fileIsFound   
;        
;        mov ah, 2
;    mov dl, 'd'   ;;;;;;;;;;;;;
;    int 21h 
;        
;        printMessage filePathErrorMessage
;        jmp exit
;        
;    fileIsFound:         
;        ;printMessage fileIsFoundMessage 
;        ;printMessage openFileErrorMessage;;;;;;;;;;;;
;        ret
;checkFilePath endp    
;    ;;;;;;;;;;;;;;;;;;;
;end main    







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    write file
;.model tiny
;.code 
;org 100h    
;    
;printMessage macro string
;    mov ah, 9
;    lea dx, string 
;    int 21h 
;endm
;
;findFile macro 
;    mov ah, 4eh
;	mov cx, 0			
;	mov dx, offset filePath
;	int 21h
;endm      
;            
;main:
;    jmp installer
;    
;    mov ax, @data  
;    call copyFilePath
;    call checkFilePath  
;    
;    call saveInFile
;    
;exit:                  
;    mov ax, 4c00h
;    int 21h
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; 
;emptyCmdErrorMessage          db  'Error. Cmd is empty.', 13, 10, '$'
;excessCharsErrorMessage       db  'Error. File path contains excess characters.', 13, 10, '$'
;filePathErrorMessage          db  'Error. File is not found.', 13, 10, '$'
;
;fileIsFoundMessage            db  'File is found.', 13, 10, '$'
;
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;openFileErrorMessage          db  'Open file error.', 13, 10, '$'
;closeFileErrorMessage         db  'Close file error.', 13, 10, '$'
;movePointerErrorMessage       db  'Move file pointer error.', 13, 10, '$'
;writeFileErrorMessage         db  'Write file error.', 13, 10, '$'
;    
;
;cmdMaxLength        equ 126     
;filePath            db  cmdMaxLength + 1 dup(0)  
;filePathLength      db  ? 
;fileID              dw  ?
;filePosition        dw  2 dup(0)
;
;bufferChar          db  '3'
;  
;copyFilePath proc           
;	mov es, ax     
;	
;	fillFilePathLength:
;    	xor cx, cx        
;    	mov si, 80h
;    	mov cl, ds:[si]
;    	dec cl
;    	mov es:filePathLength, cl
;	
;	checkEmptyCmd:
;    	cmp cl, 0ffh
;    	jne fillFilePath
;    	mov ds, ax 
;    	printMessage emptyCmdErrorMessage
;        jmp exit
;	
;	fillFilePath:
;    	add si, 2
;    	mov di, offset filePath
;    	rep movsb                      
;    
;    mov ds, ax    
;    ret
;copyFilePath endp
;
;checkFilePath proc           
;	checkExcessCharsInFilePath: 
;	    mov si, offset filePath
;        xor ax, ax
;        mov al, filePathLength       
;        add si, ax
;	    dec si 
;	    mov bl, byte ptr [si]
;        mov byte ptr [si], 0
;        findFile 	
;    	jc tryFindFile 
;    	
;    	printMessage excessCharsErrorMessage
;        jmp exit
;                  
;    tryFindFile:
;        mov byte ptr [si], bl
;        findFile
;        jnc fileIsFound   
;        
;        printMessage filePathErrorMessage
;        jmp exit
;        
;    fileIsFound:         
;        printMessage fileIsFoundMessage
;        ret
;checkFilePath endp 
;
;openFile proc
;    mov ah, 3dh
;    mov al, 00000001b   ;read and write mode
;    mov cl, 00000000b
;    mov dx, offset filePath
;    int 21h
;    mov fileID, ax  
;    jnc openFileSuccessfully
;            
;    printMessage openFileErrorMessage
;    jmp exit    
;    
;    openFileSuccessfully:  
;    ret
;openFile endp
;
;closeFile proc
;    mov ah, 3eh
;    mov bx, fileID
;    int 21h
;    jnc closeFileSuccessfully 
;       
;    printMessage closeFileErrorMessage 
;    jmp exit
;    
;    closeFileSuccessfully:
;    ret
;closeFile endp
;
;setPointer proc 
;    ;si = offset shift  
;    mov ah, 42h  
;    mov al, 0
;    mov bx, fileID
;    mov cx, [si]
;    mov dx, [si + 2]
;    int 21h        
;    jnc setPointerSuccessfully          
;        
;    printMessage movePointerErrorMessage
;    jmp exit
;    
;    setPointerSuccessfully: 
;    ret
;setPointer endp  
;
;incIndex proc
;    ;si = offset index      
;        cmp [si + 2], 0ffffh
;        jne incSmallPart
;    incBigPart:
;        mov ax, [si]
;        add ax, 1
;        mov word ptr [si], ax
;        mov ax, 0
;        mov word ptr [si + 2], ax
;        ret
;    incSmallPart:
;        mov ax, [si + 2]
;        add ax, 1
;        mov word ptr [si + 2], ax
;        ret    
;incIndex endp
;
;writeFile proc
;    ;dx = offset buffer, cl = length       
;    mov ah, 40h
;    mov bx, fileID
;    xor ch, ch
;    int 21h 
;    jnc writeFileSuccessfully
;    
;    printMessage writeFileErrorMessage
;    jmp exit
;    
;    writeFileSuccessfully: 
;    ret
;writeFile endp
;
;
;saveInFile proc
;    call openFile     
;    
;    mov si, offset filePosition 
;    call setPointer  
;    
;    mov cl, 1
;    mov dx, offset bufferChar
;    call writeFile           
;    
;    mov si, offset filePosition 
;    call incIndex  
;    
;    call closeFile   
;    ret    
;saveInFile endp
;
;installer:
;
;end main