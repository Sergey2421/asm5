.model tiny
.code
org 100h     
Begin: 
    PUSHA  
    print welcome
    call get_file_name     
    opening_file:  
    call open_file
    read_data:    
        mov cx,400
        mov dx, offset buffer 
        mov ah, 3Fh
        int 21h
        CMP ax, 400
        je not_close 
        inc flag_close
        not_close:
        push cx   
        mov cx, ax
         push ax
         push si
        mov si, offset buffer  
        loooop:
            mov ax, [si]
                           
            CMP al, 10 
            JE stra

            JMP not_str 
            stra:
            inc count_length 
            not_str: 
            inc si
        loop loooop 
        pop si
        pop ax
        pop cx
        CMP flag_close , 1
        je close_file  
    jmp short read_data
    close_file:
        inc count_length 
        mov ax, count_length
        mov number, ax
        call   output_int
        mov ah, 3Eh
        int 21h
        int 20h
exit:            
    CMP ax, 02h
    JNE next1
	mov ah,9
	mov dx,offset new_line 
	int 21h
    mov ah,9
    mov dx,offset error1
    int 21h
next1:
    CMP ax,03h
    JNE next2
	mov ah,9
	mov dx,offset new_line 
	int 21h
    mov ah,9
    mov dx,offset error2
    int 21h
next2:
    CMP ax,04h
    JNE next3
	mov ah,9
	mov dx,offset new_line 
	int 21h
    mov ah,9
    mov dx,offset error3
    int 21h
next3:
    CMP ax,05h
    JNE next4
	mov ah,9
	mov dx,offset new_line 
	int 21h
    mov ah,9
    mov dx,offset error4
    int 21h 
next4:
    CMP ax,0Ch
    JNE end_exit
	mov ah,9
	mov dx,offset new_line 
	int 21h
    mov ah,9
    mov dx,offset error5
    int 21h

end_exit:

  popa
    int 20h
;//////////////////////////////////////////PRINT///////////////////////////        
 macro print str
	mov ah,9
	mov dx,offset new_line 
	int 21h
  mov ah,9
  mov dx,offset str
  int 21h
  mov ah,9
	mov dx,offset new_line 
	int 21h
endm  
       
;//////////////////////////////////////////GET_FILE///////////////////////////           
get_file_name proc  
    xor cx, cx
    mov cl, es:[80h]  ;this adress contains size of cmd 
    cmp cl, 0 
    je empty_cmd
    mov di, 82h       ;beginning of cmd
    lea si, file_name ;start of address
get_symbols:
    mov al, es:[di]    
    cmp al, 0Dh       ;compare with end  
    je opening_file 
    cmp al, ' '
    je too_many_args
    mov [si], al;ds:[si]=es:[di]      file name=param from cmd 
    inc di            
    inc si          
jmp get_symbols     
    empty_cmd:
    print empty_cmd_message
    jmp exit 
    too_many_args:
    print too_many_message
    jmp exit 
    mov [si], 0  
    
ret
get_file_name endp 
;//////////////////////////////////////////OPEN_FILE///////////////////////////  
open_file proc   
    
    mov dx, offset file_name   ;address ASCIZ-string with name
    mov ah, 3Dh             ;DOS 3Dh
    mov al, 00h             ;00 - only reading 0 000 00 0
    int 21h                 ;open file 
    jc exit                 ;if error - exit
    mov bx, ax              ;bx - identefy file
    ;mov di, 01              ;di identify STDOUT (screen)   
    print open_message         
ret
open_file endp             
          
;//////////////////////////////////////////OUTPUT_INT///////////////////////////    
output_int proc near USES cx,dx,ax 
    pusha
     mov sign_number,0
     CMP number, 0
     JE number_zero
     JL out_negative                  ;number <0 
     JMP out_NOT_negative             ;number >0
      
      out_negative:   
            mov sign_number,1  ;number negative
            mov ax, number
            NEG ax
            mov number, ax 
      out_NOT_negative:
            xor ax,ax
            mov cx, 5 
            xor si,si   
            xor dx,dx
            xor bx,bx 
            CMP  sign_number,1
            JE out_neg
            JMP output
      out_neg: 
            mov symbol_out , '-'
            xor ax,ax
            mov ah,9
            mov dx,offset symbol_out 
            int 21h 
    output:   
           xor dx,dx
           xor ax,ax
           CMP number, 0
           JE end_output 
           mov AX,number 
           mov del, 10  
    
           IDIV del  
           mov symbol_out , dl
           mov number, ax 

           add symbol_out, 30h 
           xor bx,bx
           mov bx, offset output_mass
           dec bx ;sub bx
           xor ax,ax
           mov al,symbol_out 
           mov si, cx
           mov [bx][si] ,al
    loop output   
    JMP end_output
    number_zero:
           mov symbol_out, '0'
           xor ax,ax                                                                                                                                   
           mov ah,9
           mov dx,offset symbol_out  
           int 21h
           mov ah,9
           mov dx,offset new_number 
           int 21h 
           JMP END
    end_output:   
           xor bx,bx
           mov bx, offset output_mass
           mov [bx+5] ,'$'
           xor ax,ax                                                                                                                                   
           mov ah,9
           mov dx,offset output_mass
           add dx, cx     ;offset due to the number of elements(0 in begin str)
           int 21h     
           mov ah,9
           mov dx,offset new_number 
           int 21h 
    END:        
	popa               
    ret
output_int endp   

    welcome db 'Hello! Program start!$'
    buffer db 401 DUP ('$') 
    file_name_size equ 70
    file_name db file_name_size dup('$')
    argc dw 0 
    error1 db 'ERROR1:FILE NOT FOUND$' 
    error2 db 'ERROR2:PATH NOT FOUND$'  
    error3 db 'ERROR3:TOO MANY OPEN FILE$' 
    error4 db 'ERROR4:ACCESS IS DENIED$' 
    error5 db 'ERROR5:INVALID ACCESS MODE$'  
    open_message db 'File was opened$'
    empty_cmd_message db 'cmd is empty...Nothing to handle$'   
    too_many_message db 'Too many args were entered$'
    new_line db 13,10,'$'  
    count dw 0  
    number dw 0
    count_length dw 0
    sign_number dw 0       
    del dw 1 
    buf dw 0 
    flag_close db 0       
    new_number db 9,32, '$' 
    str db 'strings in file!'
    end_program db 'Program end!$'  
    output_mass db '0', '0', '0', '0', '0', '$', '$'
    symbol_out db  ?,'$'  
end Begin