;cmd    D:\emu8086\MyBuild\lw5.exe D:\emu8086\MySource\lw5\file.txt hac    

.model small
.stack 100h

.data           

    minCmdLengthErrorMessage      db  'Error. Cmd is not entered.', '$'
    oneArgumentInCmdErrorMessage  db  'Error. Cmd contains one argument.', '$'
    excessCharInCmdErrorMessage   db  'Error. Cmd contains excess characters.', '$'
    deletedWordLengthErrorMessage db  'Error. Big deleted word length', '$'
    openFileErrorMessage          db  'Open file error.', '$'
    closeFileErrorMessage         db  'Close file error.', '$'
    movePointerErrorMessage       db  'Move pointer error.', '$'
    readErrorMessage              db  'Read word error.', '$'
    writeErrorMessage             db  'Write word error.', '$'
    
    
    maxCmdLength        equ 126                                                       
    ;cmdLength           db  32 + 1 + 3
;    cmd                 db  'D:\emu8086\MySource\lw5\file.txt hac';maxCmdLength dup(?)
    
    cmdLength           db  ?
    cmd                 db  maxCmdLength dup(?)
     
    filePath            db  maxCmdLength + 1 dup(?)    
    fileID              dw  ?
    
    deletedWordMaxLength    equ 50
    deletedWord         db  deletedWordMaxLength dup(?)   
    deletedWordLength   db  ?
    bufferWord          db  deletedWordMaxLength dup(?)
    bufferChar          db  ?   
    
    currentLoopIndex    dw  2 dup(0) 
    previousLoopIndex   dw  2 dup(0)
    wordBeginIndex      dw  2 dup(?)
    wordEndIndex        dw  2 dup(?)
    lastFileIndex       dw  2 dup(?)
    
    
.code 
   
copyCmd macro           
	mov es, ax     
	
	xor cx, cx        
	mov si, 80h
	mov cl, ds:[si]
	dec cl
	mov es:cmdLength, cl
	add si, 2
	mov di, offset cmd
	rep movsb
    mov byte ptr es:[di], '$'      
    
    mov ds, ax    
endm   

printMessage macro string
;    push ax 
;    push dx 
         
    mov ah, 9
    lea dx, string 
    int 21h     
    
;    pop dx
;    pop ax  
endm

initializeBeginningData proc 
    checkMinCmdLength:
        cmp cmdLength, 0
        jg checkSpaceInCmd           
        printMessage minCmdLengthErrorMessage
        jmp finishMain 
             
    checkSpaceInCmd:
        mov al, ' '
        mov di, offset cmd
        xor cx, cx 
        mov cl, cmdLength                
        repne scasb
        
        cmp cx, 0
        jne fillFilePath         
        printMessage oneArgumentInCmdErrorMessage
        jmp finishMain
    
    fillFilePath:
        xor ax, ax 
        mov al, cmdLength
        inc cx
        sub ax, cx
        mov cx, ax 
        mov si, offset cmd
        mov di, offset filePath         
        rep movsb
        mov [di], 0
        
    checkExcessSpaceInCmd:
        inc si
        mov di, si 
        xor cx, cx 
        mov cl, cmdLength
        sub cx, ax
        mov bx, cx               
        mov al, ' '
        repne scasb
        
        cmp cx, 0
        je checkExcessTabInCmd
        printMessage excessCharInCmdErrorMessage
        jmp finishMain
        
    checkExcessTabInCmd:
        mov di, si 
        mov cx, bx               
        mov al, 9
        repne scasb
        
        cmp cx, 0
        je checkMaxDeletedWordLength
        printMessage excessCharInCmdErrorMessage
        jmp finishMain    
    
    checkMaxDeletedWordLength: 
        dec bx 
        cmp bx, deletedWordMaxLength
        jle fillDeletedWord
        printMessage deletedWordLengthErrorMessage
        jmp finishMain
        
    fillDeletedWord: 
        mov di, offset deletedWord                       
        mov cx, bx
        mov byte ptr deletedWordLength, bl
        rep movsb 
        
    ret    
initializeBeginningData endp

openFile proc  
    mov ah, 3dh
    mov al, 01000011b   ;read and write
    mov cl, 00000000b
    mov dx, offset filePath
    int 21h 
    mov fileID, ax
    jnc openFileSuccessfully
    
    printMessage openFileErrorMessage
    jmp finishMain
    
    openFileSuccessfully:
    ret
openFile endp

closeFile proc  
    mov ah, 3eh
    mov es, ax 
    mov bx, fileID
    int 21h
    jnc closeFileSuccessfully
    
    printMessage closeFileErrorMessage 
    jmp finishMain
    
    closeFileSuccessfully:
    ret
closeFile endp

getLastFileIndex proc 
    mov ah, 42h  
    mov al, 2
    mov bx, fileID
    mov cx, 0ffffh
    mov dx, 0ffffh
    int 21h         
    jnc initializeFileLengthSuccessfully
    
    printMessage movePointerErrorMessage
    jmp finishMain
    
    initializeFileLengthSuccessfully: 
    mov [lastFileIndex], dx
    mov [lastFileIndex + 2], ax 
    ret
getLastFileIndex endp

readFile proc
    ;dx = offset buffer, cl = length
    mov ah, 3fh
    mov bx, fileID 
    xor ch, ch
    int 21h
    jnc checkCharsCountAfterReading
    
    printMessage readErrorMessage
    jmp finishMain
    
    checkCharsCountAfterReading:
    cmp ax, cx
    je readFileSuccessfully
    
    jmp finishMain    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    readFileSuccessfully: 
    ret
readFile endp

writeFile proc
    ;dx = offset buffer, cl = length       
    mov ah, 40h
    mov bx, fileID
    xor ch, ch
    int 21h 
    jnc writeFileSuccessfully
    
    printMessage writeErrorMessage
    jmp finishMain
    
    writeFileSuccessfully: 
    ret
writeFile endp

compareCharWithSeparator proc 
    compareCharWithSpace:
        cmp bufferChar, 32
        jne compareCharWithTab
        ret             
    compareCharWithTab:
        cmp bufferChar, 9
        jne compareCharWithCret
        ret        
    compareCharWithCret:
        cmp bufferChar, 13
        jne compareCharWithNewl
        ret       
    compareCharWithNewl:
        cmp bufferChar, 10
        ret
compareCharWithSeparator endp    

movePointer proc 
    ;ah = shift, al = beg position
    mov bx, fileID
    xor cx, cx
    xor dx, dx
    mov dl, ah
    mov ah, 42h
    int 21h        
    jnc movePointerSuccessfully          
        
    printMessage movePointerErrorMessage
    jmp finishMain
    
    movePointerSuccessfully: 
    ret
movePointer endp

setPointer proc 
    ;si = offset shift  
    mov ah, 42h  
    mov al, 0
    mov bx, fileID
    mov cx, [si]
    mov dx, [si + 2]
    int 21h        
    jnc setPointerSuccessfully          
        
    printMessage movePointerErrorMessage
    jmp finishMain
    
    setPointerSuccessfully: 
    ret
setPointer endp

getPointer proc 
    mov ah, 0    
    mov al, 1
    call movePointer
    mov [si], dx
    mov [si + 2], ax 
    ret
getPointer endp

;compareWordEndAndFileLength proc 
;;    mov ah, 2
;;            mov dl, '_'      ;;;;;;;;;;;
;;            int 21h
;;     mov si, offset wordEndIndex    ;;;;;;;;;
;;                call pri  ;;;;;; 
;;                mov si, offset fileLength    ;;;;;;;;;
;;                call pri  ;;;;;;  
;;                
;;                mov ah, 2
;;            mov dl, '_'      ;;;;;;;;;;;
;;            int 21h
;                
;    
;    compsreBigParts:    
;        mov ax, [wordEndIndex]
;        cmp ax, [fileLength]
;        je compsreSmallParts
;        ret
;    compsreSmallParts:
;        mov ax, [wordEndIndex + 2]
;        cmp ax, [fileLength + 2]    
;        ret
;compareWordEndAndFileLength endp

incIndex proc      
        cmp [si + 2], 0ffffh
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

pri proc      
            mov dx,[si]
            mov ax,[si + 2]
            ;;
               
            mov cx, dx
            mov bx, ax
            mov ah, 2
            mov dl, ch 
            int 21h 
            mov dl, cl 
            int 21h 
            mov dl, bh 
            int 21h 
            mov dl, bl 
            int 21h
            mov dl, '|' 
            int 21h
            
        ret    
pri endp

;isBeginOfFile proc
;    checkBigPartOfCurrentLoopIndex:    
;        cmp [currentLoopIndex], 0
;        je checkSmallPartOfCurrentLoopIndex
;        ret
;    checkSmallPartOfCurrentLoopIndex:
;        cmp [currentLoopIndex + 2], 0
;        ret      
;isBeginOfFile endp
   
compareIndexes proc
    compsreBigParts:    
        mov ax, [si]
        cmp ax, [di]
        je compsreSmallParts
        ret
    compsreSmallParts:
        mov ax, [si + 2]
        cmp ax, [di + 2]    
        ret      
compareIndexes endp

main: 
;    mov ax, @data;;;;;;;;
;    mov ds, ax  ;;;;;;;;;
;    mov es, ax  ;;;;;;;;;
    mov ax, @data
    copyCmd
                    
    call initializeBeginningData
    
    call openFile
    call getLastFileIndex 
    
    mov ah, 0    
    mov al, 0
    call movePointer
;    mov ah, 2
;    mov dl, 'L'    ;;;;;;;;;;;;;;;
;    int 21h 
    fileMainLoop: 
;    mov ah, 2
;            mov dl, 'i'    ;;;;;;;;;;;;;;;
;            int 21h 
    
        saveMainLoopIndex:
            mov si, offset currentLoopIndex
            call getPointer
            
        saveWordBeginIndex:
            mov si, offset wordBeginIndex
            call getPointer    
            
        readWordAndCompare:
            mov cl, deletedWordLength
            mov dx, offset bufferWord 
            call readFile
            mov si, offset deletedWord 
            mov di, offset bufferWord
            xor cx, cx 
            mov cl, deletedWordLength
            repe cmpsb 
            jne nextIteration    
  ;                    
;                   mov ah, 2
;            mov dl, '_'    ;;;;;;;;;;;;;;;
;            int 21h     
            
        saveWordEndIndex:
            mov si, offset wordEndIndex
            call getPointer
                           
        checkEndOfFile:
            mov si, offset lastFileIndex
            call incIndex
            mov si, offset wordEndIndex 
            mov di, offset lastFileIndex
            call compareIndexes
            call getLastFileIndex
            jne checkRightChar
                 ;
;                    mov ah, 2
;            mov dl, 'E'    ;;;;;;;;;;;;;;;
;            int 21h
              
;            mov si, offset wordBeginIndex
;            call setPointer
;            mov cl, 0 
;            call writeFile
            jmp checkBeginOfFile
            
        checkRightChar:
            ;  mov ah, 2
;            mov dl, 'R'    ;;;;;;;;;;;;;;;
;            int 21h
            
            
            mov si, offset wordEndIndex
            call setPointer 
            mov cl, 1
            mov dx, offset bufferChar 
            call readFile          
            call compareCharWithSeparator    
            jne nextIteration
        
        checkBeginOfFile:
            mov si, offset currentLoopIndex 
            mov di, offset previousLoopIndex
            call compareIndexes  
             ;     mov ah, 2
;            mov dl, 'B'    ;;;;;;;;;;;;;;;
;            int 21h
            je deleteWord  
                        
        checkLeftChar:  
     ;         mov ah, 2
;            mov dl, 'L'    ;;;;;;;;;;;;;;;
;            int 21h             
            mov si, offset previousLoopIndex
            call setPointer
            ;;
;            mov si, offset previousLoopIndex
;            mov bx, [si+ 2]
;            mov ah, 2  
;            
;            mov dl, bh    ;;;;;;;;;;;;;;;
;            int 21h 
;            mov ah, 2
;            mov dl, bl    ;;;;;;;;;;;;;;;
;            int 21h
;            mov ah, 2
;            mov dl, '_'    ;;;;;;;;;;;;;;;
;            int 21h
            ;;
            mov cl, 1
            mov dx, offset bufferChar 
            call readFile
            call compareCharWithSeparator    
            jne nextIteration 
            
        deleteWord:     
                  mov ah, 2
                mov dl, 'D'    ;;;;;;;;;;;;;;;
                int 21h  
;            mov si, offset wordBeginIndex
;            call pri
;            mov si, offset wordEndIndex
;            call pri
;                        mov ah, 2
;            mov dl, '_'    ;;;;;;;;;;;;;;;
;            int 21h 
            mov si, offset lastFileIndex
            call incIndex
            mov si, offset wordEndIndex 
            mov di, offset lastFileIndex
            call compareIndexes
            call getLastFileIndex
            jne deleteWordLoop 
            
   ;         mov ah, 2
;            mov dl, '!'    ;;;;;;;;;;;;;;;
;            int 21h 
            
            mov si, offset wordBeginIndex
            call setPointer
            mov cl, 0 
            call writeFile
            jmp finishMain


            deleteWordLoop: 
                mov si, offset wordEndIndex
                call setPointer             
                mov cl, 1
                mov dx, offset bufferChar 
                call readFile   
                
                mov si, offset wordBeginIndex
                call setPointer                  
                mov cl, 1
                mov dx, offset bufferChar 
                call writeFile  
                
                mov si, offset wordEndIndex 
                mov di, offset lastFileIndex
                call compareIndexes
                
             ;   mov si, offset wordBeginIndex
;                call pri;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                
                je cutFile          
                mov si, offset wordBeginIndex
                call incIndex         
                mov si, offset wordEndIndex
                call incIndex
                
                
                
            jmp deleteWordLoop
            
            cutFile:
;                mov ah, 2
;            mov dl, 'C'    ;;;;;;;;;;;;;;;
;            int 21h  
;                mov si, offset wordBeginIndex
;                call pri
               
;               
;                mov si, offset wordBeginIndex
;                call setPointer
;                mov ah, 1    
;            mov al, 1
;            call movePointer
                mov cl, 0 
                call writeFile   
                call getLastFileIndex
                            
        nextIteration:                    
            mov si, offset currentLoopIndex
            call setPointer      
            savePreviousIndex:
                mov si, offset previousLoopIndex
                call getPointer           
            mov ah, 1    
            mov al, 1
            call movePointer

    jmp fileMainLoop       
            
            
    ;mov ah, 2
;    mov dl, '=' 
;    int 21h
    
    
                             
    finishMain:
;    mov ah, 2
;            mov dl, 'X'    ;;;;;;;;;;;;;;;
;            int 21h 
        call closeFile                  
        mov ax, 4c00h
        int 21h
end main
       

;;;;;;;;;;;;;;;;;;;;;       delete (not full)
;main: 
;    mov ax, @data;;;;;;;;
;    mov ds, ax  ;;;;;;;;;
;    mov es, ax  ;;;;;;;;;
;;    mov ax, @data
;;    copyCmd
;                    
;    call initializeBeginningData
;    
;    call openFile
;    call initializeFileLength
;    
;    fileMainLoop: 
;        saveMainLoopIndex:
;            mov ah, 0    
;            mov al, 1
;            call movePointer
;            mov [currentLoopIndex], dx
;            mov [currentLoopIndex + 2], ax
;            ;;;
;               
; ;           mov cx, dx
;;            mov bx, ax
;;            mov ah, 2
;;            mov dl, ch 
;;            int 21h 
;;            mov dl, cl 
;;            int 21h 
;;            mov dl, bh 
;;            int 21h 
;;            mov dl, bl 
;;            int 21h
;;            mov dl, '|' 
;;            int 21h   
;               
;               ;;;
;        readWordAndCompare:
;            mov cl, deletedWordLength
;            mov dx, offset bufferWord 
;            call readFile
;            mov si, offset deletedWord 
;            mov di, offset bufferWord
;            xor cx, cx 
;            mov cl, deletedWordLength + 1
;            repe cmpsb   
;            cmp cx, 0
;            jne nextIteration 
;            
;        checkRightChar:  
;            mov cl, 1
;            mov dx, offset bufferChar 
;            call readFile          
;            call compareCharWithSeparator    
;            jne nextIteration
;                        
;        checkLeftChar: 
;            mov si, offset previousLoopIndex
;            call setPointer
;            ;;
;;            mov si, offset previousLoopIndex
;;            mov bx, [si+ 2]
;;            mov ah, 2  
;;            
;;            mov dl, bh    ;;;;;;;;;;;;;;;
;;            int 21h 
;;            mov ah, 2
;;            mov dl, bl    ;;;;;;;;;;;;;;;
;;            int 21h
;;            mov ah, 2
;;            mov dl, '_'    ;;;;;;;;;;;;;;;
;;            int 21h
;            ;;
;            mov cl, 1
;            mov dx, offset bufferChar 
;            call readFile
;            call compareCharWithSeparator    
;            jne nextIteration 
;            
;        deleteWord:
;            mov ah, 0    
;            mov al, 1
;            call movePointer
;            mov [wordBeginIndex], dx
;            mov [wordBeginIndex + 2], ax
;            
;            mov ah, deletedWordLength    
;            mov al, 1
;            call movePointer
;            mov [wordEndIndex], dx
;            mov [wordEndIndex + 2], ax    
;            
;            deleteWordLoop: 
;                mov si, offset wordEndIndex
;                call setPointer
;            
;                mov cl, 1
;                mov dx, offset bufferChar 
;                call readFile   
;                
;                mov si, offset wordBeginIndex
;                call setPointer 
;                
;                mov cl, 1
;                mov dx, offset bufferChar 
;                call writeFile  
;                
;                call compareWordEndAndFileLength
;                je cutFile
;                
;                mov si, offset wordBeginIndex
;                call incIndex              
;                
;                mov si, offset wordEndIndex
;                call incIndex
;                
;            jmp deleteWordLoop
;            
;            cutFile:
;                mov cl, 0 
;                call writeFile   
;                call initializeFileLength
;                            
;        nextIteration:                    
;            mov si, offset currentLoopIndex
;            call setPointer      
;            savePreviousIndex:
;                mov ah, 0    
;                mov al, 1
;                call movePointer
;                mov [previousLoopIndex], dx
;                mov [previousLoopIndex + 2], ax            
;            mov ah, 1    
;            mov al, 1
;            call movePointer
;
;    jmp fileMainLoop       
;            
;            
;    ;mov ah, 2
;;    mov dl, '=' 
;;    int 21h
;    
;    
;                             
;    finishMain:
;        call closeFile                  
;        mov ax, 4c00h
;        int 21h
;end main
;       
       
       
       
       
       
;;;;;;;;;;;;;;;;;;;;;;;   cmd
;.model small
;.stack 100h
;
;.data       
;    
;  maxCmdLength      equ 126   
;  cmdLength         db  ?
;  cmd               db maxCmdLength dup(?) 
;  
;.code 
;   
;copyCmd macro           
;	mov es, ax     
;	
;	xor cx, cx        
;	mov si, 80h
;	mov cl, ds:[si]
;	dec cl
;	mov es:cmdLength, cl
;	add si, 2
;	mov di, offset cmd
;	rep movsb
;    mov byte ptr es:[di], '$'      
;    
;    mov ds, ax    
;endm    
;
;start: 
;     mov ax, @data
;    copyCmd
;	
;                      
;                
;    mov ah, 2
;    mov dl, cmdLength 
;    int 21h
;    mov ah, 2
;    mov dx, '=' 
;    int 21h
;    mov ah, 09h
;    lea dx, cmd 
;    int 21h     
;    mov ah, 2
;    mov dx, '=' 
;    int 21h               
;                      
;    mov ax, 4c00h
;    int 21h
;end start                                                   
