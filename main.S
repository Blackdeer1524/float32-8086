assume DS:data,CS:code

data segment
    prompt_msg db 'Enter two decimal numbers (e.g., 15.123 20.456): $'
data ends

code segment 
start:
    mov ax, data
    mov ds, ax
    
    mov ah, 09h
    lea dx, prompt_msg
    int 21h

    mov ah, 4Ch
    int 21h
code ends.