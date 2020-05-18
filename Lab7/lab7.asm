.286
.model  small
.stack  100h

empty_segment SEGMENT    
empty_segment ENDS

.data
    startMessage      db "Console parameter: ", '$'
    iterationsMsg     db "Iterations count: ", '$'
    fileMsg           db "Filename: ", '$'
    stringMsg         db "String number: ", '$'
    negativeExit      db "Enter correct number!", '$'
    allocatingError   db "Allocating error!", '$'
    startupError      db "Startup error!", '$'
    badFileMessage    db "Cannot open file", 0dh, 0ah, '$'
    badArguments      db "Bad arguments passed!", 0dh, 0ah, '$'
    fileError         db "Error while opening file!", '$'

    partSize          equ 256
    wasPreviousLetter dw 0
    realPartSize      dw 256
    descriptor        dw 0
    pointerPosition   dd 0
    path              db 256 dup('$')
    tempVariable      dw 0
    isEndl            db 0
    command_line      db 0
    ;epb               dw 0         ; 2 bytes - seg_env (enviroment segement address (if 0 = copy from parent))         ; address of FCB structure (37 bytes for file description)
    spacePos          dw 0
    saveSS            dw 0      ; for restoring
    saveSP            dw 0      ; for restoring
    saveES            dw 0      ; for restoring
    saveDS            dw 0      ; for restoring
    base              dw 10
    iterations        dw 0
    stringNumber      dw 0
    parsingStep       dw 1
    endl              db 13, 10, '$'
    endlCounter       dw 0
    
    tempString        db 256 dup('$')
    fileName          db 256 dup(0)
    dtaBuffer         db 128 dup(0)
    applicationName   db 256 dup(0)
    part              db partSize dup('$')
    
    ; === Exec Parameter Block (EPB) для функции 4Bh ===
    env dw 0            ;Сегмент среды (окружения DOS) для загружаемой программы
    cmd_off           dw ? 
    cmd_seg           dw ?
    fcb1              dd ?         ; address of FCB structure (37 bytes for file description)
    fcb2              dd ?
    Len dw $-env  ;Длина EPB

    progPath          db "lab1.exe", 0
    epb dw 0
	    dw offset command_line,0
	    dw 005Ch,0,006Ch,0  

    dsize=$-startMessage       ;размер сегмента данных
.code

;///////////////////////////////
print_str:     
    push bp
    mov bp, sp   
    pusha 
    
    mov dx, [ss:bp+4+0]     
    mov ax, 0900h
    int 21h 
    
    mov dx, offset endl
    mov ax, 0900h
    int 21h  
    
    popa
    pop bp      
ret 

puts:
    mov     ah, 9
    int     21h
    ret
ret

exit:
    mov     ax, 4c00h
    int     21h
ret
;///////////////////////////////

;///////////////////////////////
toInteger:  
    pusha        
    
    xor di, di
    lea di, path 
    
    xor bx, bx     
    xor ax, ax   
    xor cx, cx
    xor dx, dx

    mov bx, spacePos
    
    skipSpacesInteger:
        cmp [di + bx], byte ptr ' '
        jne unskippingInteger
        inc bx
        jmp skipSpacesInteger
    
    unskippingInteger:

    cmp [di + bx], byte ptr '-'
        jne atoi_loop

    jmp atoi_error

    atoi_loop:        
        cmp [di + bx], byte ptr '0'    
        jb atoi_error  
        cmp [di + bx], byte ptr '9'    
        ja atoi_error
                             
        mul base 
        mov dl, [di + bx] 
        jo atoi_error 
        sub ax, '0'  
        jo atoi_error
        add ax, dx 
    
        inc bx 
        cmp [di + bx], byte ptr ' '
    jne atoi_loop  
       
    jmp atoi_end 
    
    atoi_error:
        lea dx, negativeExit
        call puts
        call exit

    atoi_end: 
        mov tempVariable, ax 
        mov spacePos, bx
        inc parsingStep

    popa
ret

toString:
    pusha
    xor di, di
    lea di, tempString
    mov ax, tempVariable
    xor bx, bx
    mov bx, di

    xor cx, cx
    mov cx, 256

    setZeroString:
        mov [di], byte ptr '$'
        loop setZeroString
    
    lea di, tempString

    itoa_loop:
        xor dx, dx
        div base  
        add dl, '0'
        mov [di], dl
        inc di                   
        cmp ax, 0
    ja itoa_loop

    dec di
    xor si, si
    mov si, bx 

    reverse_mini:
        xor dx, dx
        xor cx, cx  
        
        mov dl, byte ptr [si]  
        mov cl, byte ptr [di]
        mov [si], cl
        mov [di], dl
        
        inc si
        dec di
        
        cmp si, di
        je reverse_mini
    popa
ret

;///////////////////////////////

;///////////////////////////////
getFilename:
    pusha

    lea di, path 
    
    xor bx, bx     
    xor ax, ax   

    mov bx, spacePos

    skipSpacesString:
        cmp [di + bx], byte ptr ' '
        jne unskippingString
        inc bx
        jmp skipSpacesString
    
    unskippingString:

    lea si, fileName

    copyFilename:
        xor ax, ax
        mov al, [di + bx] 
        mov [si], al
    
        inc bx
        inc si

        cmp [di + bx], byte ptr '$'
        jne copyFilename

    ;mov [si], byte ptr 0
    mov spacePos, bx

    popa
ret

getStringNumber:
    pusha

    xor ax, ax
    call toInteger
    mov ax, tempVariable
    mov stringNumber, ax

    popa
ret

allocate_memory:
  push ax
  push bx

  mov bx, ((csize/16)+1)+256/16+((dsize/16)+1)+256/16;

  mov ah, 4Ah
  int 21h

  jc allocate_memory_error
  jmp allocate_memory_end

  allocate_memory_error:
    lea     dx, allocatingError
    call    puts
    call    exit

  allocate_memory_end:
  pop bx
  pop ax
  ret
endp

getIterations:
    pusha

    xor ax, ax
    call toInteger
    mov ax, tempVariable
    mov iterations, ax

    cmp iterations, 255
    jg badRange
    
    popa
ret

badRange:
    call exit
ret

loadAndRun:
    mov ax, 4B00h
    lea dx, applicationName
    lea bx, env
    int 21h
ret

getApplicationName:
    pusha
    xor     ax, ax
    
    mov     dx, offset fileName
    mov     ah, 3Dh
    mov     al, 00h
    int     21h
    mov     descriptor, ax

    mov bx, ax
    jnc read_file_part
    jmp file_error;

    read_file_part:    
    
        mov ah, 42h
        mov cx, word ptr [offset pointerPosition]
        mov dx, word ptr [offset pointerPosition + 2]
        mov al, 0  
        mov bx, descriptor
        int 21h

        mov cx, partSize
        lea dx, part
        mov ah, 3Fh
        mov bx, descriptor
        int 21h
        mov realPartSize, ax
        
        call searchApplicationName
        call memset

        cmp realPartSize, partSize
        jb close_file
        
        mov cx, word ptr [offset pointerPosition]
        mov dx, word ptr [offset pointerPosition + 2]
        add dx, ax
        adc cx, 0
        mov word ptr [offset pointerPosition], cx
        mov word ptr [offset pointerPosition + 2], dx

        jmp read_file_part
             
    
    close_file:
        exit_from_file:
            mov     ah, 3Eh
            mov     bx, descriptor
            int     21h
            popa
ret

file_error:
    cmp ax, 03h
    jne cont

    lea dx, startMessage
    call puts

    cont:
    lea dx, fileError
    call puts

    call exit
ret

searchApplicationName:
    pusha
    xor si, si

    part_parsing:
        call checkEndl 

        mov ax, stringNumber
        cmp endlCounter, ax
        je parseApplicationName

        cmp isEndl, 0
        je  increment

        inc endlCounter
        inc si

        increment:
            inc si
            mov isEndl, 0
            cmp si, realPartSize
            jb part_parsing

    call exit

    parseApplicationName:
        lea di, applicationName

        copyApplicationName:
            xor ax, ax
            mov al, [part + si]
            mov [di], al
            
            inc si
            inc di

            cmp [part + si], 0dh
            je exitFromParsing

            cmp si, realPartSize
            je exitFromParsing
            
            jmp copyApplicationName
        
    exitFromParsing:

    popa
ret

checkEndl:
    mov al, [part + si]
    xor ah,ah

    mov bl, [part + si + 1]
    xor bh,bh

    cmp al, 0dh
    jne exit_from_endl_check

    cmp bl, 0ah
    jne exit_from_endl_check

    mov isEndl, 1
    
    exit_from_endl_check:
ret

memset:
    pusha
    xor si, si
    lea si, part
    mov cx, partSize

    set_end_cycle:
        mov byte ptr [si], '$'
        inc si
        loop set_end_cycle
    
    popa
ret

bad_arguments:
        lea     dx, badArguments
        call    puts
        call    exit
ret

start proc
    call allocate_memory

    mov ax, @data        ;move data segment address in DS and ES
    mov ds, ax

    mov bl, es:[80h] 
    add bx, 80h      ;args line last    
    mov si, 82h      ;args line start
    mov di, offset path
    
    cmp si, bx
    ja bad_arguments

    get_path:
        mov     al, es:[si]
        mov     [di], al

        cmp     BYTE PTR es:[si], byte ptr ' '
        jne     getNextCharacter

        ;cmp     wasPreviousLetter, 0
        ;je      getNextCharacter
        
        cmp     parsingStep, 1
        jne     stepTwo
        call    getIterations
        jmp     getNextCharacter

        stepTwo:
            call    getStringNumber
            jmp     getNextCharacter

        stepThree:
            call    getFilename
            jmp     main

        ;setPreviousLetter:
            ;mov wasPreviousLetter, 1

        getNextCharacter:
            inc     di
            inc     si
            cmp     si, bx
            jg      stepThree
    jbe get_path

    main:
        lea     dx, startMessage
        call    puts

        lea     ax, path 
        push    ax
        call    print_str  
        pop     ax
        
        dec     stringNumber
        call    getApplicationName

        xor cx, cx
        mov cx, iterations
        
        startApps:
            call    loadAndRun
            loop    startApps

        call exit
endp

csize=$-print_str

end start