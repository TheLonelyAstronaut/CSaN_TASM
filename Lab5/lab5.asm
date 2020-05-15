.286
.model  small
.stack  100h

.data
    startMessage    db "Console parameter: ", '$'
    resultMessage   db "Result number: ", '$'
    partSize        equ 256
    base            dw 10
    realPartSize    dw 256
    badArguments    db "Bad argument passed!", 0dh, 0ah, '$'
    badFileMessage  db "Cannot open file", 0dh, 0ah, '$'
    allOkMessage    db "All is OK!", 0dh, 0ah, '$'
    pointerPosition dd 0
    wasPreviousEndl db 1
    isEndl          db 0
    path            db 256 dup(0)
    part            db partSize dup('$')
    descriptor      dw 0
    resultNumber    dw 0
    resultString    db partSize dup('$')
    endl            db 10, 13, '$'

.code

start proc
    mov     ax, @data        ;move data segment address in DS and ES
    mov     ds, ax
    
    mov     bl, es:[80h] 
    add     bx, 80h      ;args line last    
    mov     si, 82h      ;args line start
    mov     di, offset path
    
    cmp si, bx
    ja bad_arguments

    get_path:
        cmp     BYTE PTR es:[si], '$' 
        je      main
              
        mov     al, es:[si]
        mov     [di], al      
              
        inc     di
        inc     si
        cmp     si, bx
    jbe get_path


    main:
        lea     dx, startMessage
        call    puts

        lea     ax, path 
        push    ax
        call    print_str  
        pop     ax
        
        call    file_tasks

        lea     dx, resultMessage
        call    puts

        call itoa

        lea dx, resultString
        call puts

        call    exit
        

    bad_arguments:
        lea     dx, badArguments
        call    puts
        call    exit

    file_error:
        lea     dx, badFileMessage 
        call    puts
        call    exit
endp

file_tasks:
    mov     dx, offset path
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
        jc close_file
        
        call calculateEmptyStrings
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

calculateEmptyStrings:
    pusha
    xor si, si


    part_parsing:
        call checkEndl

        ;xor ax, ax
        ;mov al, isEndl 
        ;mov resultNumber, ax
        ;call itoa
        ;lea ax, resultString
        ;push ax
        ;call print_str
        ;pop ax     

        cmp isEndl, 0
        je check_previous
        
        add si, 2
        mov isEndl, 0
        mov wasPreviousEndl, 1

        cmp si, realPartSize
        jb part_parsing
        jmp exit_loop
        
        check_previous:
            inc si
            cmp wasPreviousEndl, 1
            je increment

            mov wasPreviousEndl, 0
            cmp si, realPartSize
            jb part_parsing
            jmp exit_loop

        increment:
            inc resultNumber
            mov isEndl, 0
            mov wasPreviousEndl, 0
            cmp si, realPartSize
            jb part_parsing
        
        exit_loop:
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

itoa:
    pusha
    xor di, di
    lea di, resultString
    mov ax, resultNumber
    xor bx, bx
    mov bx, di
    
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
        jb reverse_mini
    popa
ret

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

end start