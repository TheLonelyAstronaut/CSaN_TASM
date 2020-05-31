.model  tiny                
.code  
.386                    
org     100h 

start:
    jmp     main    
 
oldInterruptionHandler      dd 0
pointerPosition             dd 0
lastPointerPosition         dd 0
fileDescriptor              dw 0
flagDown                    db ?          
flagUp                      db ?
readBuffer                  db 128 dup(?)
commandLineParsingError     db "Error while opening file!", 10, 13, '$' 
fileName                    db 128 dup(0) 

newInterruptionHandler proc
    pushf  ;//Saving flags
    call    cs:oldInterruptionHandler ;//Calling old interruption handler
    pusha  ;//Saving registers
    
    push    cs                           
    pop     ds
    
    mov     ah,11h ;//Getting pressed key
    int     16h

    cmp     ah, 50h ;//Down arrow                   
    je      downMove 

    cmp     ah, 48h ;//Up arrow
    je      upMove    

    cmp     ah, 01h ;//Esc key         
    je      escKey

    popa ;//If there are no pressed keys in the keyboard buffer - return
    iret

    downMove:
        mov     ax, 0C00h ;//Clear stdin buffer
        int     21h

        mov     flagDown, 0
        mov     flagUp, 0

        mov     ah, 3Dh ;//Open file			        
	    mov     al, 0			     
	    lea     dx, fileName       
	    mov     cx, 0			        
	    int     21h

        mov     fileDescriptor, ax ;//Saving file descriptor

        mov     al, 0 ;//Set new read pointer position            
        mov     bx, fileDescriptor
	    mov     ah, 42h           
	    mov     cx, word ptr [offset lastPointerPosition]
	    mov     dx, word ptr [offset lastPointerPosition + 2]		 
	    int     21h

        mov     ah, 3Fh ;//Read from file to check data               
	    mov     bx, fileDescriptor         
	    mov     cx, 1            
	    lea     dx, readBuffer                 
        int     21h
        
        cmp     ax, 0 ;//Empty file
        je      endReadDown

        mov     ax, 0b800h
        mov     es, ax
        xor     di,di 
        mov     cx,2000 ;//80x25

        clearScreenDownMove: ;//Clear screen
            mov     al, ' ' 
            mov     es:[di], al
            add     di, 2
        loop clearScreenDownMove


        mov     al, 0 ;//Set new read pointer position          
        mov     bx, fileDescriptor
	    mov     ah, 42h            
	    mov     cx, 0
	    mov     cx, word ptr [offset pointerPosition]
	    mov     dx, word ptr [offset pointerPosition + 2]			 
	    int     21h       

        mov     ax, 0b800h ;//Pointer to videobuff
        mov     es, ax
        mov     di, 0

        readAndWriteSymbolsDown:
            mov     ah, 3Fh ;//Read symbol from file                 
            mov     bx, fileDescriptor            
            mov     cx, 1            
            lea     dx, readBuffer                  
            int     21h 

            cmp     ax,0 ;//If endfile
            je      endReadDown

            cmp     readBuffer[0], 0Dh ;//If endline
            jne     notEndStringDown

            addSpacesInEndStringDown:
                cmp     di,4000 ;//If screen end
                jge     endReadDown

                mov     al,' '  
                mov     es:[di], al
                inc     di 
                inc     di   
                mov     bx, 160
                mov     ax, di  
                xor     dx, dx
                div     bx   

                cmp     dx, 0
                jne     checkEndDown 
                cmp     flagDown, 0
                jne     checkEndDown    

                mov     al, 1                 
                mov     bx, fileDescriptor
	            mov     ah, 42h             
	            mov     cx, 0
	            mov     dx, 0		 
	            int     21h    
                mov     word ptr [offset pointerPosition], dx
                mov     word ptr [offset pointerPosition + 2], ax 
	            mov     flagDown, 1      
        
            checkEndDown: 
                cmp     di, 4000    
                jge     endReadDown

                mov     ax, di 
                xor     dx, dx   
                mov     bx, 160
                div     bx 
                cmp     dx, 0
                jne     addSpacesInEndStringDown
                jmp     readAndWriteSymbolsDown  
                
            notEndStringDown:
                cmp     readBuffer[0], 0Ah ;//If new line
                jne     printDown
                jmp     readAndWriteSymbolsDown
            
            printDown:
                mov     al, readBuffer[0]
                mov     es:[di], al
                add     di, 2   

                mov     bx, 160
                mov     ax, di  
                xor     dx, dx
                div     bx
                cmp     dx,0  
                jne     checkEndScreen   
                cmp     flagDown, 0
                jne     checkEndScreen   

                mov     flagDown, 1 
                mov     al, 1               
                mov     bx, fileDescriptor
                mov     ah, 42h            
                mov     cx, 0
                mov     dx, 0		 
                int     21h    
                mov     word ptr [offset pointerPosition], dx
                mov     word ptr [offset pointerPosition + 2], ax  
                
            checkEndScreen:
                cmp     di, 4000
                jge     endReadDown 
            
        jmp readAndWriteSymbolsDown   

        endReadDown: 
            mov     al, 1                
            mov     bx, fileDescriptor
            mov     ah, 42h             
            mov     cx, 0
            mov     dx, 0		 
            int     21h    
            mov     word ptr [offset lastPointerPosition], dx
            mov     word ptr [offset lastPointerPosition + 2], ax  

        jmp endHandler
            
    upMove:
        mov     ax,0C00h 
        int     21h   

        xor     si,si
        add     si, word ptr [offset lastPointerPosition]
        add     si, word ptr [offset lastPointerPosition + 2]    
        cmp     si, 0
        je      endHandler

        cmp     word ptr [offset pointerPosition], 0
        jne     openFileToRead 
        cmp     word ptr [offset pointerPosition + 2], 1
        je      endHandler

        openFileToRead:
            mov     ah, 3Dh			      
	        mov     al, 0			 
	        lea     dx, fileName     
	        mov     cx, 0			        
            int     21h 
            
            mov     flagDown, 0
	        mov     flagUp, 0                
	        mov     fileDescriptor, ax

            mov     al, 0                 
            mov     bx, fileDescriptor
	        mov     ah, 42h             
	        mov     cx, word ptr [offset pointerPosition]
	        mov     dx, word ptr [offset pointerPosition + 2]	 		 
	        int     21h                                       

            mov     cx, 160

            moveBack:
                push    cx     

                mov     al, 1                
                mov     bx, fileDescriptor
                mov     ah, 42h            
                mov     cx, -1
                mov     dx, -2		 
                int     21h   

                add     ax, dx                        
                cmp     ax, 0 
                je      popCxWithoutLoop

                readNext:   
                    mov     ah, 3Fh                   
	                mov     bx, fileDescriptor                 
	                mov     cx, 1         
	                lea     dx, readBuffer               
	                int     21h        

	                cmp     readBuffer[0], 0Ah
	                jne     popCxWithLoop  	
	                inc     flagUp
	                cmp     flagUp, 2
	                je      popCxWithoutLoop 
	                pop     cx
	                inc     cx 
	                loop    moveBack     
	                jmp     pushCx            

	                cmp     readBuffer[0], 0Dh
	                jne     popCxWithLoop     
	                pop     cx
	                inc     cx
	        loop moveBack 
            
            jmp pushCx 
            
            popCxWithLoop:
            	pop     cx
                loop    moveBack
                
            pushCx:   
                push    cx
                
            popCxWithoutLoop:  
                pop     cx

            mov     ax, 0b800h
            mov     es, ax    
            xor     di, di 
                
            mov     flagDown, 0    
            mov     di, 0

            readAndWriteSymbolsUp:
                mov     ah, 3Fh                 
                mov     bx, fileDescriptor            
                mov     cx, 1      
                lea     dx, readBuffer            
                int     21h  
                  
                cmp     ax, 0  
                je      endReadUp                 
                cmp     readBuffer[0], 0Dh  
                jne     notEndStringUp

                addSpacesInEndStringUp:
                    cmp     di, 4000
                    jge     endReadUp       

                    mov     al, ' '  
                    dec     cx
                    mov     es:[di], al
                    inc     di 
                    inc     di 
                    mov     bx, 160
                    mov     ax, di  
                    xor     dx, dx
                    div     bx  

                    cmp     dx, 0
                    jne     checkEndUp 
                    cmp     flagDown, 0
                    jne     checkEndUp

                    mov     flagDown, 1 
                    mov     al, 1                 
                    mov     bx, fileDescriptor
                    mov     ah, 42h            
                    mov     cx, 0
                    mov     dx, 0		 
                    int     21h    
                    mov     word ptr [offset pointerPosition], dx
                    mov     word ptr [offset pointerPosition + 2], ax

                checkEndUp:
                    cmp     di, 4000
                    jge     endReadUp

                    xor     dx, dx
                    mov     ax, di    
                    mov     bx, 160
                    div     bx  
                    cmp     dx,0
                    jne     addSpacesInEndStringUp

            jmp  readAndWriteSymbolsUp  

            notEndStringUp:
                cmp     readBuffer[0], 0Ah
                jne     printUp
                jmp     readAndWriteSymbolsUp

            printUp: 
                mov     al, readBuffer[0]
                mov     es:[di], al
                add     di, 2

                mov     bx, 160
                mov     ax, di  
                xor     dx, dx
                div     bx     

                cmp     dx, 0 
                jne     checkScreenEndUp   
                cmp     flagDown, 0
                jne     checkScreenEndUp      

                mov     flagDown, 1 
                mov     al, 1 
                mov     bx, fileDescriptor
                mov     ah, 42h              
                mov     cx, 0
                mov     dx, 0		 
                int     21h    
                mov     word ptr [offset pointerPosition], dx
                mov     word ptr [offset pointerPosition + 2], ax

                checkScreenEndUp:
                    cmp     di, 4000	  
                    jge     endReadUp
                    jmp     readAndWriteSymbolsUp
                
            endReadUp:   
                mov     al, 1                
                mov     bx, fileDescriptor
                mov     ah, 42h             
                mov     cx, 0
                mov     dx, 0		 
                int     21h    
                mov     word ptr [offset lastPointerPosition], dx
                mov     word ptr [offset lastPointerPosition + 2], ax 

    endHandler:
        mov     ah, 3Eh            
	    mov     bx, fileDescriptor          
        int     21h
        popa 
        iret
    
    escKey:
        mov     ax, 2509h
        mov     dx, word ptr cs:[oldInterruptionHandler]
        mov     ds, word ptr cs:[oldInterruptionHandler+2]
        int     21h 
    
	    popa	                          
    
        mov     es, cs:2ch 
        mov     ah, 49h 
        int     21h    

	    iret
endp

getFileFromCommandLine proc
    mov     bl, es:[80h] 
    add     bx, 80h      ;//Args line last    
    mov     si, 82h      ;//Args line start
    mov     di, offset fileName

    cmp     si, bx
    ja      parsingError

    getPath: ;//Parsing commandline args
        cmp     byte ptr es:[si], '$' 
        je      main

        mov     al, es:[si]
        mov     [di], al      

        inc     di
        inc     si
        cmp     si, bx
        jbe     getPath
    
    add     di, 2
    mov     [di], byte ptr '$'

    mov     ah, 3Dh	;//Trying to open file		      
	mov     al, 0			 
	lea     dx, fileName     
	mov     cx, 0			        
	int     21h                       
	
    jc      parsingError 
 
	mov     bx,ax
    mov     ah, 3Eh                    
 	int     21h

    ret
endp

main proc
    call    getFileFromCommandLine ;//Get filename from cammandline with file existing validation

    lea     dx, filename ;//Print filename
    mov     ah,9
    int     21h

    cli ;//Deny all interruptions

    mov     ah, 35h ;//Get old keyboard interruption
	mov     al, 09h                   
	int     21h

    mov     word ptr oldInterruptionHandler, bx ;//Old interruption handler offset
	mov     word ptr oldInterruptionHandler + 2, es ;//Segment address of the old interruption handler

    push    ds			                  
    pop     es
    
    mov     ah, 25h ;//Set new keyboard interruption                      
	mov     al, 09h                      
	mov     dx, offset newInterruptionHandler            
	int     21h

    sti ;//Allow all interruptions
    
    mov     ah, 31h ;//Set to resident mode       
	mov     al, 0                                                              
	mov     dx, (main - start + 10Fh) / 16                                  
	int     21h 

    ret

    parsingError: ;//Error while parsing commandline or while file opening
        lea     dx, commandLineParsingError
	    mov     ah,9
	    int     21h     
	    int     20h   
    ret
endp

end     start