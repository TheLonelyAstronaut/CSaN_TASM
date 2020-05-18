.286
.model      small
.stack      100h

.data
    startMessage      db "Console parameter: ", '$'
    iterationsMsg     db "Iterations count: ", '$'
    fileMsg           db "Filename: ", '$'
    applicationError  db "Application start error!", '$'
    stringMsg         db "String number: ", '$'
    negativeExit      db "Enter correct number!", '$'
    allocatingError   db "Allocating error!", '$'
    startupError      db "Startup error!", '$'
    badFileMessage    db "Cannot open file", 0dh, 0ah, '$'
    badArguments      db "Bad arguments passed!", 0dh, 0ah, '$'
    fileError         db "Error while opening file!", '$'
    badFileName       db "Bad file name!", '$'

    partSize          equ 256
    wasPreviousLetter dw 0
    realPartSize      dw 256
    descriptor        dw 0
    pointerPosition   dd 0
    path              db 256 dup('$')
    tempVariable      dw 0
    isEndl            db 0
    spacePos          dw 0
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
    env               dw 0
    cmd_off           dw ? 
    cmd_seg           dw ?
    fcb1              dd ?         ; address of FCB structure (37 bytes for file description)
    fcb2              dd ?
    Len               dw $ - env  ;Длина EPB  

    dsize=$-startMessage       ;размер сегмента данных
.code

;///////////////////////////////

printString proc  
    push    bp
    mov     bp, sp   
    pusha 
    
    mov     dx, [ss:bp+4+0]     
    mov     ax, 0900h
    int     21h 
    
    mov     dx, offset endl
    mov     ax, 0900h
    int     21h  
    
    popa
    pop     bp      
    ret 
endp

puts proc
    mov     ah, 9
    int     21h
    ret
endp

badFileNameCall proc
    lea     dx, badFileName
    call    puts
    call    exit
endp

exit proc
    mov     ax, 4c00h
    int     21h
endp

badRange:
    lea     dx, negativeExit
    call    puts
    call    exit
ret

;///////////////////////////////

toInteger proc
    pusha        
    
    xor     di, di
    lea     di, path 

    xor     bx, bx     
    xor     ax, ax   
    xor     cx, cx
    xor     dx, dx

    mov     bx, spacePos
    
    skipSpacesInteger:
        cmp     [di + bx], byte ptr ' '
        jne     unskippingInteger
        inc     bx
        jmp     skipSpacesInteger
    
    unskippingInteger:

    cmp     [di + bx], byte ptr '-'
    jne     atoiLoop

    jmp     atoiError

    atoiLoop:        
        cmp     [di + bx], byte ptr '0'    
        jb      atoiError  
        cmp     [di + bx], byte ptr '9'    
        ja      atoiError
                             
        mul     base 
        mov     dl, [di + bx] 
        jo      atoiError 
        sub     ax, '0'  
        jo      atoiError
        add     ax, dx 
    
        inc     bx 
        cmp     [di + bx], byte ptr ' '

    jne     atoiLoop  
       
    jmp     atoiEnd 
    
    atoiError:
        jmp     badRange

    atoiEnd: 
        mov     tempVariable, ax 
        mov     spacePos, bx
        inc     parsingStep

    cmp     tempVariable, 255
    jg      badRange

    cmp     tempVariable, 0
    je      badRange

    popa
    ret
endp

toString proc
    pusha
    xor     di, di
    lea     di, tempString
    mov     ax, tempVariable
    xor     bx, bx
    mov     bx, di

    xor     cx, cx
    mov     cx, 256

    setZeroString:
        mov     [di], byte ptr '$'
        loop    setZeroString
    
    lea     di, tempString

    itoaLoop:
        xor     dx, dx
        div     base  
        add     dl, '0'
        mov     [di], dl
        inc     di                   
        cmp     ax, 0
        ja      itoaLoop

    dec     di
    xor     si, si
    mov     si, bx 

    reverseMini:
        xor     dx, dx
        xor     cx, cx  

        mov     dl, byte ptr [si]  
        mov     cl, byte ptr [di]
        mov     [si], cl
        mov     [di], dl

        inc     si
        dec     di

        cmp     si, di
        je      reverseMini
    popa
    ret
endp

;///////////////////////////////

applicationStartError:
    lea     dx, applicationError
    call    puts
    call    exit
ret

;///////////////////////////////

allocateMemory proc
    push    ax
    push    bx 

    mov     bx, ((csize/16)+1)+256/16+((dsize/16)+1)+256/16
    mov     ah, 4Ah
    int     21h 

    jc      allocateMemoryError
    jmp     allocateMemoryEnd 

    allocateMemoryError:
        lea     dx, allocatingError
        call    puts
        call    exit  
      
    allocateMemoryEnd:
        pop     bx
        pop     ax
        ret
endp

getIterations proc
    pusha

    xor     ax, ax
    call    toInteger
    mov     ax, tempVariable
    mov     iterations, ax
    
    popa
    ret
endp

loadAndRun proc
    mov     ax, 4B00h
    lea     dx, applicationName
    lea     bx, env
    int     21h

    jb      applicationStartError
    
    ret
endp

;///////////////////////////////

fileErrorCall:
    lea     dx, fileError
    call    puts
    call    exit
ret

;///////////////////////////////

getFilename proc
    pusha

    lea     di, path 

    xor     bx, bx     
    xor     ax, ax   

    mov     bx, spacePos

    skipSpacesString:
        cmp     [di + bx], byte ptr ' '
        jne     unskippingString
        inc     bx
        jmp     skipSpacesString
    
    unskippingString:

    lea si, fileName

    copyFilename:
        xor     ax, ax
        mov     al, [di + bx] 
        mov     [si], al
    
        inc     bx
        inc     si

        cmp     [di + bx], byte ptr '$'
        jne     copyFilename

    mov     spacePos, bx

    popa
    ret
endp

getStringNumber proc
    pusha

    xor     ax, ax
    call    toInteger
    mov     ax, tempVariable
    mov     stringNumber, ax

    popa
    ret
endp

getApplicationName proc
    pusha
    xor     ax, ax
    
    mov     dx, offset fileName
    mov     ah, 3Dh
    mov     al, 00h
    int     21h
    mov     descriptor, ax

    mov     bx, ax
    jnc     readFilePart
    jmp     fileErrorCall;

    readFilePart:    
    
        mov     ah, 42h
        mov     cx, word ptr [offset pointerPosition]
        mov     dx, word ptr [offset pointerPosition + 2]
        mov     al, 0  
        mov     bx, descriptor
        int     21h

        mov     cx, partSize
        lea     dx, part
        mov     ah, 3Fh
        mov     bx, descriptor
        int     21h
        mov     realPartSize, ax

        call    searchApplicationName
        call    memset

        cmp     realPartSize, partSize
        jb      closeFile

        mov     bx, stringNumber
        cmp     endlCounter, bx
        je      closeFile
        
        mov     cx, word ptr [offset pointerPosition]
        mov     dx, word ptr [offset pointerPosition + 2]
        add     dx, ax
        adc     cx, 0
        mov     word ptr [offset pointerPosition], cx
        mov     word ptr [offset pointerPosition + 2], dx

        jmp     readFilePart
             
    
    closeFile:
        exitFromFile:
            mov     ah, 3Eh
            mov     bx, descriptor
            int     21h
            popa
    
    ret
endp

searchApplicationName proc
    pusha
    xor     si, si

    partParsing:
        call    checkEndl 

        mov     ax, stringNumber
        cmp     endlCounter, ax
        je      parseApplicationName

        cmp     isEndl, 0
        je      increment

        inc     endlCounter
        jmp     partParsingCycle

        increment:
            inc     si
        
        partParsingCycle:
            mov     isEndl, 0
            cmp     si, realPartSize
            jb      partParsing

    
    popa
    ret

    parseApplicationName:
        cmp     isEndl, 1
        jne     parseStart   
        
        call    badFileNameCall

        parseStart:
            lea     di, applicationName

            copyApplicationName:
                xor     ax, ax
                mov     al, [part + si]
                mov     [di], al

                inc     si
                inc     di

                cmp     [part + si], 0dh
                je      exitFromParsing

                cmp     [part + si], 0ah
                je      exitFromParsing

                cmp     si, realPartSize
                je      exitFromParsing

                jmp     copyApplicationName
        
    exitFromParsing:

    popa
    ret
endp

checkEndl proc
    mov     al, [part + si]
    xor     ah,ah

    cmp     al, 0dh
    je      checkNextSymbol

    cmp     al, 0ah
    jne     exitFromEndlCheck

    inc     si
    call    setIsEndl
    
    exitFromEndlCheck:
    ret
endp

checkNextSymbol:
    call    setIsEndl
    mov     bl, [part + si + 1]
    xor     bh,bh

    cmp     bl, 0ah
    jne     exitFromCheck

    inc     si

    exitFromCheck:
        inc     si
ret

setIsEndl proc
    mov     isEndl, 1
    ret
endp

memset proc
    pusha
    xor     si, si
    lea     si, part
    mov     cx, partSize

    setEndCycle:
        mov     byte ptr [si], '$'
        inc     si
        loop    setEndCycle
    
    popa
    ret
endp

;///////////////////////////////

badArgumentsCall:
    lea     dx, badArguments
    call    puts
    call    exit
ret

;///////////////////////////////

start proc
    call    allocateMemory

    mov     ax, @data        ;move data segment address in DS
    mov     ds, ax

    mov     bl, es:[80h] 
    add     bx, 80h      ;args line last    
    mov     si, 82h      ;args line start
    mov     di, offset path

    cmp     si, bx
    ja      badArgumentsCall

    getPath:
        mov     al, es:[si]
        mov     [di], al

        cmp     BYTE PTR es:[si], byte ptr ' '
        jne     getNextCharacter
        
        cmp     wasPreviousLetter, 0
        je      skipCurrentSymbol

        mov     wasPreviousLetter, 0
        cmp     parsingStep, 1
        jne     stepTwo
        call    getIterations
        jmp     skipCurrentSymbol

        stepTwo:
            call    getStringNumber
            jmp     skipCurrentSymbol

        stepThree:
            call    getFilename
            jmp     main

        getNextCharacter:
            mov     wasPreviousLetter, 1
        
        skipCurrentSymbol:
            inc     di
            inc     si
            cmp     si, bx
            jg      stepThree
    jbe getPath

    main:
        lea     dx, startMessage
        call    puts

        lea     ax, path 
        push    ax
        call    printString  
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

csize = $ - printString

end start