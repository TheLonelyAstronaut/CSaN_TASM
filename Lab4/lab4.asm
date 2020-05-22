.286
.model small
.stack 100h

.data
    NEW_ITEM            dw 1 ;flag for creating new item
    GAME_OVER           dw 0 
    BLACK_SYMBOL        db 35h,0      ;black character on black background
	BLUE_SYMBOL         db 38h,11h     ;blue character on blue background
	GRAY_SYMBOL         db 30h,77h     ;gray character in gray background, attribute 0111 0111
	ITEM_CHAR           db 23h
	RED_ATTRIBUTE       db 44h
	GREEN_ATTRIBUTE     db 22h
	BLUE_ATTRIBUTE      db 33h
    TEMP_SYMBOL         dw 0    
    ITEM_ROTATE         dw 0
    ITEM_HEIGHT         dw 0
    ITEM_WIDTH          dw 0
    ITEM_X              dw 40
    ITEM_Y              dw 1
	ELAPSED_TIME        dw 0
	BLOCK_ROTATE        db 1
    RED_SYMBOL          db 23h, 44h
    stringScore         db "00000"
    score               dw 0
    delay               dw 4
    decrementSpeed      db 0
    scoreAttribute      equ 00000111b
    base                dw 10
    randomNumber        dw 0
    currentBlockModel   dw 3
.code

setInitState proc
    RIGHT_KEY       equ 4Dh
    LEFT_KEY        equ 4Bh
    UP_KEY          equ 48h
    DOWN_KEY        equ 50h

    SCREEN_WIDTH    equ 0A0h       ;screen width in bytes (dec: 80 x 2 = 160) 
    SCREEN_HEIGHT   equ 19h       ;screen height in characters (dec: 25)

    FACTOR          equ 2 

    LEFT_LIMIT      equ 31
    RIGHT_LIMIT     equ 50
    DOWN_LIMIT      equ 24

    mov     ax, @data ;move data segment address in DS
    mov     ds, ax

    mov     ah,00h ;set 80x25 text colorized videomode
	mov     al,3
	int     10h

	mov     ax,0B800h ;set pointer to the start of the videomemory
	mov     es,ax

    ret
endp

calculateScreenOffset proc
    push    cx
    push    bx

    mov     cl, SCREEN_WIDTH ;ScreenWidth = 80x2=160
    mul     cl ; ax = y * ScreenWidth
    mov     dx, ax ; dx = `y` * 80x2  

    mov     ax, bx
    mov     bx, FACTOR
    push    dx 
    mul     bx ; ax = `x` * 2
    pop     dx
    add     dx, ax ; dx contain offset

    pop     bx
    pop     cx

    ret
endp

getRandomNumber proc 
    pusha

    mov     ah, 00h  ; interrupts to get system time        
    int     1ah      ; CX:DX now hold number of clock ticks since midnight      	
    mov     ax, dx
    xor     dx, dx
    mov     cx, 3    
    div     cx       ; here dx contains the remainder of the division - from 0 to 2
    mov     randomNumber, dx	
    popa
    
    ret
endp

printRectangle proc
    push     bp
    mov      bp, sp

    ;===================
    ; [bp + 2] = call ret adress
	; [bp + 4] = symbol
	; [bp + 6] = height
	; [bp + 8] = width
	; [bp + 10] = y
    ; [bp + 12] = x
    ;===================

    push     ax
	push     bx  
	push     cx
	push     dx
    push     di

    mov     ax, [bp + 10]
    mov     bx, [bp + 12]
    call    calculateScreenOffset

    mov     di, dx

	mov     ax, [bp + 4] ; ax = ascii char + attribute
	mov     cx, [bp + 6] ; cx = height

	printRectangleLoop:  
		push    cx
		mov     cx, [bp + 8] ; cx = width

		push    di
		rep     stosw
		pop     di

		add     di, SCREEN_WIDTH

		pop     cx
	    loop    printRectangleLoop

	pop     di
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	pop     bp
    
    ret
endp

clearRectangle proc
    push    bp
    mov     bp, sp

    ;===================
    ; [bp + 2] = call ret adress
    ; [bp + 4] = height
    ; [bp + 6] = width
    ; [bp + 8] = y
    ; [bp + 10] = x
    ;===================

    push    ax
    push    bx  
    push    cx
    push    dx
    push    di

    mov     ax, [bp + 8]
    mov     bx, [bp + 10]
    call    calculateScreenOffset ; ax = `y` & bx = 'x' => dx = calculated offset
    mov     di, dx

    mov     ax, word ptr BLACK_SYMBOL; ax = ascii char + attribute
    mov     cx, [bp + 4] ; cx = height

    clearRectangleLoop:  
        push    cx
        mov     cx, [bp + 6] ; cx = width
    
        push    di
        rep     stosw
        pop     di

        add     di, SCREEN_WIDTH

        pop     cx
        loop    clearRectangleLoop

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
endp

printRectangleHelper macro x, y, width, height, symbol
    push    x                 ;X coordinate
    push    y                 ;Y coordinate
    push    width             ;width of the rectangle
    push    height            ;height of the rectangle
    push    word ptr symbol   ;char with attribute

    call    printRectangle

    pop     dx
    pop     dx
    pop     dx
    pop     dx
    pop     dx
endm

clearRectangleHelper macro x, y, width, height
    push    x                 ;X coordinate
    push    y                 ;Y coordinate
    push    width             ;width of the rectangle
    push    height            ;height of the rectangle

    call    clearRectangle

    pop     dx
    pop     dx
    pop     dx
    pop     dx
endm

printLayout proc
    printRectangleHelper    30, 0, 21, 1, GRAY_SYMBOL ; top
    printRectangleHelper    30, 0, 1, DOWN_LIMIT, GRAY_SYMBOL ; left
    printRectangleHelper    50, 0, 1, DOWN_LIMIT, GRAY_SYMBOL ; right
    printRectangleHelper    30, DOWN_LIMIT, 21, 1, GRAY_SYMBOL ; bottom

    ret
endp

toString proc
    pusha
    xor     di, di
    lea     di, stringScore
    mov     ax, score
    xor     bx, bx
    mov     bx, di

    xor     cx, cx
    mov     cx, 5

    setZeroString:
        mov     [di], byte ptr '0'
        loop    setZeroString
    
    lea     di, stringScore

    itoaLoop:
        xor     dx, dx
        div     base  
        add     dl, byte ptr '0'
        mov     [di], dl
        inc     di                   
        cmp     ax, 0
        ja      itoaLoop

    lea     di, stringScore
    add     di, 4
    lea     si, stringScore 

    reverseMini:
        xor     dx, dx
        xor     cx, cx  

        mov     dl, byte ptr [si]  
        ;mov     cl, byte ptr [di]
        mov     [di], dl
        mov     [si], byte ptr '0'

        inc     si
        dec     di

        cmp     si, di
        jb      reverseMini
    
    popa
    ret
endp

checkForCompletedRows proc
    pusha
    
    mov     bp, DOWN_LIMIT
    dec     bp
    mov     al, ITEM_CHAR
    
    mov     cx, bp

    checkForCompletedRowsLoop:
        push    cx
        mov     cx, 20
        dec     cx

        push    ax
        mov     ax, bp
        mov     bx, 31
        call    calculateScreenOffset ; ax = `y` & bx = 'x' => dx = calculated offset
        pop     ax

        mov     bx, dx

        checkCurrentRowLoop:
            cmp     byte ptr es:[bx], al
            jne     checkNextRow
            add     bx, 2
            loop    checkCurrentRowLoop

        mov     bx, dx

        clearRectangleHelper 31, bp, 20, 1
        

        inc     score
        inc     decrementSpeed
        push    ds
        push    es
        pop     ds

        push    bp

        ;cmp     DELAY, 1
        ;je      shiftUncomplitedRows

        shiftUncomplitedRows:
            mov     di, bx ; di on cleaning row
            sub     bx, SCREEN_WIDTH
            mov     si, bx ; si on previous row

            mov     cx, 20
            rep     movsw ; shift down string

            dec     bp
            cmp     bp, 1
            ja      shiftUncomplitedRows
        
        pop     bp	
        pop     ds

        checkNextRow:
            dec     bp ; y = y-1
            pop     cx
        
        loop checkForCompletedRowsLoop

    popa
    ret
endp

printScore proc
    push    bx
    push    si
    push    di

    call    toString

    mov     di, offset stringScore
	push    cx
    mov     cx, 134
    mov     si, cx
    add     cx, 10

    nextRenderIteration:
		xor     bx, bx
		mov     bl, byte ptr [di]
		mov     bh, scoreAttribute
		mov     word ptr es:[si], bx
		inc     di
		add     si, 2
		cmp     si,cx
		jne     nextRenderIteration
    
    pop     cx
    pop     di
    pop     si
    pop     bx
    
    ret
endp

checkSide proc
    pusha

    xor     ax, ax
    xor     bx, bx
    xor     cx, cx

    mov     al, byte ptr ITEM_Y ; y
    mov     bl, byte ptr ITEM_X ; x
    
    call    calculateScreenOffset ; ax = `y` & bx = 'x' => dx = calculated offset

    mov     bx, dx
    sub     bx, 2

    mov     al, byte ptr ITEM_WIDTH
    mov     cl, 2
    mul     cl
    add     ax, 2

    mov     cl, byte ptr ITEM_HEIGHT
    mov     dl, byte ptr ITEM_CHAR

    checkSideLoop:
        cmp     byte ptr es:[bx], dl
        je      disableSideMove

        add     bx, ax

        cmp     byte ptr es:[bx], dl
        je      disableSideMove

        add     bx, SCREEN_WIDTH
        sub     bx, ax
        loop    checkSideLoop

    mov     dx, 0
    jmp     endChecking

    disableSideMove:
        mov     dx, 1

    endChecking:
        popa
    ret
endp

isRotatingAllowed proc
    push    cx
    xor     cx, cx

    cmp     byte ptr ds:[ITEM_ROTATE], 0
    je      initialRotate

    cmp     byte ptr ds:[ITEM_ROTATE], 1
    je      firstStepRotate

    cmp     byte ptr ds:[ITEM_ROTATE], 2
    je      secondStepRotate
    ;cmp     byte ptr ds:[ITEM_ROTATE], 3
    jmp     far ptr     thirdStepRotate

    initialRotate:
        mov     bl, byte ptr ITEM_WIDTH	    ;bl = h2
	    sub     bl, byte ptr ITEM_HEIGHT	;bl = h2 - h1
	    mov     cl, bl 						;cl = h2 - h1 (iterations count)
    
	    mov     al, byte ptr ITEM_Y		    ; y
	    mov     bl, byte ptr ITEM_X     	; x
	    call    calculateScreenOffset 	    ; ax = `y` & bx = 'x' => dx = calculated offset
	    mov     bx, dx

	    mov     dl, byte ptr ITEM_CHAR
        mov     ax, word ptr GRAY_SYMBOL
        
        initialRotateLoop:
            sub     bx, SCREEN_WIDTH 			; go up
		    cmp     byte ptr es:[bx], dl
		    je      farJmpToBlockRotating
		    cmp     word ptr es:[bx], ax
		    je      farJmpToBlockRotating
	        loop    initialRotateLoop
        
        jmp     nonBlockRotating

        farJmpToBlockRotating:
            jmp     far ptr blockRotating

    firstStepRotate:
        mov     bl, byte ptr ITEM_HEIGHT 	;bl = w2
	    sub     bl, byte ptr ITEM_WIDTH 	;bl = w2 - w1
	    mov     cl, bl						;cl = w2 - w1 (iterations count)

	    mov     al, byte ptr ITEM_Y		; y
	    mov     bl, byte ptr ITEM_X 	; x
	    call    calculateScreenOffset	; ax = `y` & bx = 'x' => dx = calculated offset
	    mov     bx, dx

	    push    cx
	    mov     cl, byte ptr ITEM_HEIGHT
        dec     cl

        firstStepRotateDown:
            add     bx, SCREEN_WIDTH ;go down
	        loop    firstStepRotateDown
        
        pop     cx

        mov     dl, byte ptr ITEM_CHAR
        mov     ax, word ptr GRAY_SYMBOL
        
        firstStateRotateLoop:
            sub     bx, 2 						; go left
		    cmp     byte ptr es:[bx], dl
		    je      blockRotating
		    cmp     word ptr es:[bx], ax
		    je      blockRotating
            loop    firstStateRotateLoop
        
        jmp     nonBlockRotating

    secondStepRotate:
        mov     bl, byte ptr ITEM_WIDTH	;bl = h2
	    sub     bl, byte ptr ITEM_HEIGHT 	;bl = h2 - h1
	    mov     cl, bl							;cl = h2 - h1 (iterations count)

	    mov     bl, byte ptr ITEM_X		; x
	    mov     al, byte ptr ITEM_WIDTH
	    dec     al
	    mov     dl, 2
	    mul     dl

	    add     bx, ax
	    xor     ax, ax
	    mov     al, byte ptr ITEM_Y 		; y
	    call    calculateScreenOffset 			; ax = `y` & bx = 'x' => dx = calculated offset
	    mov     bx, dx

	    mov     dl, byte ptr ITEM_CHAR
        mov     ax, word ptr GRAY_SYMBOL
        
	    secondStepRotateLoop:
	    	sub     bx, SCREEN_WIDTH 			; go down
	    	cmp     byte ptr es:[bx], dl
	    	je      blockRotating
	    	cmp     word ptr es:[bx], ax
	    	je      blockRotating
            loop    secondStepRotateLoop
            
	    jmp     nonBlockRotating

    thirdStepRotate:
        mov     bl, byte ptr ITEM_HEIGHT 	;bl = w2
	    sub     bl, byte ptr ITEM_WIDTH 	;bl = w2 - w1
	    mov     cl, bl							;cl = w2 - w1 (iterations count)

	    mov     al, byte ptr ITEM_Y 		; y
	    mov     bl, byte ptr ITEM_X 		; x
	    call    calculateScreenOffset 				; ax = `y` & bx = 'x' => dx = calculated offset
	    mov     bx, dx

	    mov     dl, byte ptr ITEM_CHAR
        mov     ax, word ptr GRAY_SYMBOL
        
	    thirdStepRotateLoop:
	    	add     bx, 2 						; go right
	    	cmp     byte ptr es:[bx], dl
	    	je      blockRotating
	    	cmp     word ptr es:[bx], ax
	    	je      blockRotating
            loop    thirdStepRotateLoop
        
	    jmp     nonBlockRotating

    blockRotating:
	    mov     byte ptr ds:[BLOCK_ROTATE], 1
	    jmp     endCheckRotating

	nonBlockRotating:
	    mov     byte ptr ds:[BLOCK_ROTATE], 0

	endCheckRotating:
	    pop     cx
        ret
endp

rotateItem proc
    pusha

    xor     ax, ax
	xor     bx, bx
	xor     dx, dx

    call    isRotatingAllowed
    cmp     byte ptr BLOCK_ROTATE, 1
	jne     continueRotating

    jmp     far ptr endRotateItem

    continueRotating:

    clearRectangleHelper ITEM_X, ITEM_Y, ITEM_WIDTH, ITEM_HEIGHT
    
    cmp     byte ptr ITEM_ROTATE, 0
    je      rotateInitial

    cmp     byte ptr ITEM_ROTATE, 1
    je      firstRotate

    cmp     byte ptr ITEM_ROTATE, 2
    je      secondRotate

    cmp     byte ptr ITEM_ROTATE, 3
    je      thirdRotate
    
    rotateInitial:
        mov     bl, byte ptr ITEM_WIDTH 	;bl = h2
	    sub     bl, byte ptr ITEM_HEIGHT	;bl = h2 - h1
	    sub     byte ptr ds:ITEM_Y, bl		;bl = y2 = y1 + (h2 - h1)
    
	    ; swap width and height
	    mov     al, byte ptr ITEM_WIDTH
	    xchg    al, byte ptr ITEM_HEIGHT
	    mov     byte ptr ITEM_WIDTH, al
    
	    ;set current rotate state
	    inc     ITEM_ROTATE		;set 1
        jmp     endRotateItem
        
    firstRotate:
        mov     bl, byte ptr ITEM_HEIGHT 	;bl = w2
	    sub     bl, byte ptr ITEM_WIDTH 	;bl = w2 - w1
	    sub     byte ptr ITEM_X, bl 		;x2 = x1 - (w2 - w1)

	    mov     bl, byte ptr ITEM_HEIGHT	;bl = h1
	    sub     bl, byte ptr ITEM_WIDTH 	;bl = h1 - h2
	    add     byte ptr ITEM_Y, bl

        ; swap width and height
	    mov     al, byte ptr ITEM_WIDTH
	    xchg    al, byte ptr ITEM_HEIGHT
	    mov     byte ptr ITEM_WIDTH, al
	
	    ;set current rotate state
	    inc     ITEM_ROTATE		;set 2
		jmp     endRotateItem
	
    secondRotate:
        mov     bl, byte ptr ITEM_WIDTH 	;bl = w1
	    sub     bl, byte ptr ITEM_HEIGHT 	;bl = w1 - w2
	    add     byte ptr ITEM_X, bl 		;x2 = x1 + (w1 - w2)

	    ; swap width and height
	    mov     al, byte ptr ITEM_WIDTH
	    xchg    al, byte ptr ITEM_HEIGHT
	    mov     byte ptr ITEM_WIDTH, al
    
	    ;set current rotate state
	    inc     ITEM_ROTATE		;set 3
		jmp     endRotateItem

    thirdRotate:
        ; swap width and height 
	    mov     al, byte ptr ITEM_WIDTH
	    xchg    al, byte ptr ITEM_HEIGHT
	    mov     byte ptr ITEM_WIDTH, al

	    ;set current rotate state
	    mov     byte ptr ITEM_ROTATE, 0	;set 0

    endRotateItem:
        popa
        ret
endp

moveItem proc
    pusha

    call    checkCurrentBlockPositionForBorders
    cmp     dx, 1
    je      createNewItem

    clearRectangleHelper ITEM_X, ITEM_Y, ITEM_WIDTH, ITEM_HEIGHT

    cmp     ah, LEFT_KEY
	je      moveLeft
	
	cmp     ah, RIGHT_KEY
	je      moveRight

	jmp     moveDown

	moveLeft:
		cmp     ITEM_X, LEFT_LIMIT
		jbe     moveDown

		call    checkSide
		cmp     dx, 1
		je      moveDown

		sub     ITEM_X, 1
		jmp     moveDown

    moveRight:
		xor     dx, dx
		xor     bx, bx

		mov     dl, byte ptr ITEM_X
		mov     bl, byte ptr ITEM_WIDTH
		add     dx, bx

		cmp     dx, RIGHT_LIMIT
		jae     moveDown

		call    checkSide
		cmp     dx, 1
		je      moveDown

		add     ITEM_X, 1
		jmp     moveDown		

    createNewItem:
        pop     dx
	    pop     bx
	    pop     ax

	    mov     ITEM_ROTATE, 0
	    mov     NEW_ITEM, 1
        call    checkForCompletedRows
        jmp     exitFromMoving

    moveDown:
		add     ITEM_Y, 1
        printRectangleHelper    ITEM_X, ITEM_Y, ITEM_WIDTH, ITEM_HEIGHT, TEMP_SYMBOL
        
    exitFromMoving:

    popa
    ret
endp

perfomAction proc
    cmp     ah, LEFT_KEY ;compare scan code
    je      moveItemMark

    cmp     ah, RIGHT_KEY
    je      moveItemMark

    cmp     ah, UP_KEY
    je      rotateItemMark

    jne     moveItemMark

    rotateItemMark:
		call    rotateItem
		printRectangleHelper    ITEM_X, ITEM_Y, ITEM_WIDTH, ITEM_HEIGHT, TEMP_SYMBOL
	
    moveItemMark:
		call    moveItem
		jmp     endAction

    endAction:
    
    ret
endp

checkPlayerInput proc
    push    ax
    
    mov     ah, 1 	;check for key pressed
    int     16h 	;keyboard interrupt

    jz      endCheckInput 	;if zf=1 - key not pressed

    mov     ah, 0 	;get key
    int     16h 	;read key
    ;mov     byte ptr score,  ah

    endCheckInput:

    call    perfomAction
    pop     ax
    
    ret
endp

chooseItemModelByRandomValue proc
    push    dx
    xor     dx, dx

    cmp     byte ptr randomNumber, 0
    je      setFirstModel

    cmp     byte ptr randomNumber, 1
    je      setSecondModel

    cmp     byte ptr randomNumber, 2
    je      setThirdModel

    setFirstModel:	
        mov     dl, ITEM_CHAR
        mov     dh, RED_ATTRIBUTE
        mov     TEMP_SYMBOL, dx

        mov     ITEM_WIDTH, 3
        mov     ITEM_HEIGHT, 1
    jmp     endSelectModel

    setSecondModel:	
        mov     dl, ITEM_CHAR
        mov     dh, GREEN_ATTRIBUTE
        mov     TEMP_SYMBOL, dx

        mov     ITEM_WIDTH, 3
        mov     ITEM_HEIGHT, 2
    jmp     endSelectModel

    setThirdModel:	
        mov     dl, ITEM_CHAR
        mov     dh, BLUE_ATTRIBUTE
        mov     TEMP_SYMBOL, dx

        mov     ITEM_WIDTH, 2
        mov     ITEM_HEIGHT, 1
    jmp     endSelectModel

    endSelectModel:
        pop     dx
        ret
endp

delayToSkipTiks proc
    pusha    

    xor     cx,cx
    xor     dx, dx
    xor     ax, ax
    mov     ah, 86h
    mov     cx, 4
    int     15h    

    popa
    ret
endp

checkCurrentBlockPositionForBorders proc ;set `dx` to 1, if there are borders near item
    push    ax
    push    bx
    push    cx

    xor     ax, ax
    xor     bx, bx
    xor     cx, cx

    mov     ax, ITEM_Y
    mov     bx, ITEM_X
    call    calculateScreenOffset ; ax = `y` & bx = 'x' => dx = calculated offset

    mov     bx, dx
    xor     dx, dx

    mov     cx, ITEM_HEIGHT

	checkForBordersHeight:
		add     bx, SCREEN_WIDTH
	    loop    checkForBordersHeight

    mov     cx, ITEM_WIDTH

	checkForBordersWidth:
		mov     ax, word ptr GRAY_SYMBOL
        cmp     ax, word ptr es:[bx]
        
		je      CheckForBordersSuccess

		mov     al, ITEM_CHAR
        cmp     al, byte ptr es:[bx]
    
		je      CheckForBordersSuccess

		add     bx, 2
        loop    checkForBordersWidth
    
	jmp     EndCheckForBorders

	CheckForBordersSuccess:
	    mov     dx, 1
	
	EndCheckForBorders:
	    pop     cx
	    pop     bx
	    pop     ax

    ret
endp

createItem proc
    call    delayToSkipTiks
    call    getRandomNumber
    call    chooseItemModelByRandomValue
    
    mov     ITEM_X, 40
    mov     ITEM_Y, 1

    push    dx
	call    checkCurrentBlockPositionForBorders
	cmp     dx, 1
	je      createItemError
	pop     dx

    printRectangleHelper    ITEM_X, ITEM_Y, ITEM_WIDTH, ITEM_HEIGHT, TEMP_SYMBOL

    mov     NEW_ITEM, 0 ;disable drawing new item
    ret

    createItemError:
        pop     dx
	    mov     NEW_ITEM, 0 ;disable drawing new item
        mov     GAME_OVER, 1 ;set GAME_OVER
    
    ret
endp

game proc
	mov     ah, 0 ;get ticks count
    int     1Ah ;cx - higher byte, dx - lower one

    call    printScore

    cmp     decrementSpeed, 1
    jne     frameCalculatings

    cmp     delay, 1
    je      frameCalculatings

    dec     delay
    dec     decrementSpeed

    frameCalculatings:
        xor     cx, cx
	    cmp     dx, ELAPSED_TIME
	    jb      skipIteration
    
	    add     dx, delay
	    mov     ELAPSED_TIME, dx
    
	    cmp     NEW_ITEM, 0
	    je      skipDrawing
        
	    call    createItem
    
	    cmp     GAME_OVER, 1
	    je      gameOver

    skipDrawing:
        call    checkPlayerInput
    
    skipIteration:
    ret
endp

exit proc
    mov ah, 0h
	mov al, 3h
	int 10h
    
    mov     ax, 4c00h
    int     21h
endp

start proc
    call    setInitState  
    call    printLayout

    gameLoop:
        call    game
        jmp     gameLoop
    
    gameOver:
        call    exit
endp

end     start