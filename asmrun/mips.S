/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         */
/*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>              */
/*                                                                     */
/*  Copyright 2013 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* Asm part of the runtime system, Mips processor */

/* Allocation */

        .text

	.globl	caml_system__code_begin
caml_system__code_begin:

        .globl  caml_call_gc
        .ent    caml_call_gc

caml_call_gc:
        .set    noreorder
        .cpload $25
        .set    reorder
    /* Record return address and adjust it to point back to
       the beginning of the allocation sequence */
        sw      $31, caml_last_return_address
        subu    $31, $31, 16
    /* Don't request any allocation, will be redone at return */
        li      $24, 0
        b       $110

$110:
    /* Record lowest stack address */
        sw      $sp, caml_bottom_of_stack
    /* Reserve stack space for the registers and the call to C */
        subu    $sp, $sp, 0x110
    /* 0x110 = 32*4 (int regs) + 32*4 (float regs) + 16 (call) */
    /* Save actual retaddr, $gp, requested size. */
        sw      $31, 0x10C($sp)
        sw      $24, 0x108($sp)
        .cprestore 0x104
    /* Save pointer to register array */
        addu    $24, $sp, 0x90
        sw      $24, caml_gc_regs
    /* Save current allocation pointer for debugging purposes */
        sw      $22, caml_young_ptr
    /* Save the exception handler (if e.g. a sighandler raises) */
        sw      $30, caml_exception_pointer
    /* Save all regs used by the code generator on the stack */
        sw      $2, 2 * 4($24)
        sw      $3, 3 * 4($24)
        sw      $4, 4 * 4($24)
        sw      $5, 5 * 4($24)
        sw      $6, 6 * 4($24)
        sw      $7, 7 * 4($24)
        sw      $8, 8 * 4($24)
        sw      $9, 9 * 4($24)
        sw      $10, 10 * 4($24)
        sw      $11, 11 * 4($24)
        sw      $12, 12 * 4($24)
        sw      $13, 13 * 4($24)
        sw      $14, 14 * 4($24)
        sw      $15, 15 * 4($24)
        sw      $16, 16 * 4($24)
        sw      $17, 17 * 4($24)
        sw      $18, 18 * 4($24)
        sw      $19, 19 * 4($24)
        sw      $20, 20 * 4($24)
        sw      $21, 21 * 4($24)
/*        s.d     $f0, 16 + 0 * 4($sp)
        s.d     $f2, 16 + 2 * 4($sp)
        s.d     $f4, 16 + 4 * 4($sp)
        s.d     $f6, 16 + 6 * 4($sp)
        s.d     $f8, 16 + 8 * 4($sp)
        s.d     $f12, 16 + 12 * 4($sp)
        s.d     $f14, 16 + 14 * 4($sp)
        s.d     $f16, 16 + 16 * 4($sp)
        s.d     $f18, 16 + 18 * 4($sp)
        s.d     $f20, 16 + 20 * 4($sp)
        s.d     $f22, 16 + 22 * 4($sp)
        s.d     $f24, 16 + 24 * 4($sp)
        s.d     $f26, 16 + 26 * 4($sp)
        s.d     $f28, 16 + 28 * 4($sp)
        s.d     $f30, 16 + 30 * 4($sp) */
        /* Call the garbage collector */
        la	$25, caml_garbage_collection
        jal     $25
    /* Restore all regs used by the code generator */
        addu    $24, $sp, 0x90
        lw      $2, 2 * 4($24)
        lw      $3, 3 * 4($24)
        lw      $4, 4 * 4($24)
        lw      $5, 5 * 4($24)
        lw      $6, 6 * 4($24)
        lw      $7, 7 * 4($24)
        lw      $8, 8 * 4($24)
        lw      $9, 9 * 4($24)
        lw      $10, 10 * 4($24)
        lw      $11, 11 * 4($24)
        lw      $12, 12 * 4($24)
        lw      $13, 13 * 4($24)
        lw      $14, 14 * 4($24)
        lw      $15, 15 * 4($24)
        lw      $16, 16 * 4($24)
        lw      $17, 17 * 4($24)
        lw      $18, 18 * 4($24)
        lw      $19, 19 * 4($24)
        lw      $20, 20 * 4($24)
        lw      $21, 21 * 4($24)
     /*   l.d     $f0, 16 + 0 * 4($sp)
        l.d     $f2, 16 + 2 * 4($sp)
        l.d     $f4, 16 + 4 * 4($sp)
        l.d     $f6, 16 + 6 * 4($sp)
        l.d     $f8, 16 + 8 * 4($sp)
        l.d     $f12, 16 + 12 * 4($sp)
        l.d     $f14, 16 + 14 * 4($sp)
        l.d     $f16, 16 + 16 * 4($sp)
        l.d     $f18, 16 + 18 * 4($sp)
        l.d     $f20, 16 + 20 * 4($sp)
        l.d     $f22, 16 + 22 * 4($sp)
        l.d     $f24, 16 + 24 * 4($sp)
        l.d     $f26, 16 + 26 * 4($sp)
        l.d     $f28, 16 + 28 * 4($sp)
        l.d     $f30, 16 + 30 * 4($sp) */
    /* Reload new allocation pointer and allocation limit */
        lw      $22, caml_young_ptr
        lw      $23, caml_young_limit
    /* Allocate space for the block */
        lw      $24, 0x108($sp)
        subu    $22, $22, $24
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Return to caller */
        lw      $31, 0x10C($sp)
        addu    $sp, $sp, 0x110
        j       $31

        .end    caml_call_gc

/* Call a C function from Caml */

        .globl  caml_c_call
        .ent    caml_c_call

caml_c_call:
    /* Function to call is in $24 */
    /* Slightly optimized form of the above when referencing
       global variables is expensive */
        .set    noreorder
        .cpload $25
        .set    reorder
        la      $16, caml_last_return_address
        la      $17, caml_young_ptr
        la      $18, caml_young_limit
        sw      $31, 0($16)     /* caml_last_return_address */
        sw      $sp, caml_bottom_of_stack
        sw      $22, 0($17)     /* young_ptr */
        sw      $30, caml_exception_pointer
        move    $25, $24
        jal     $25
        lw      $31, 0($16)     /* caml_last_return_address */
        lw      $22, 0($17)     /* young_ptr */
        lw      $23, 0($18)     /* young_limit */
        sw      $0, 0($16)      /* caml_last_return_address */
        j       $31
        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .globl  stray_exn_handler
        .ent    caml_start_program
caml_start_program:
        .set    noreorder
        .cpload $25
        .set    reorder
        la      $24, caml_program

    /* Code shared with caml_callback* */
$103:
    /* Save return address */
        subu    $sp, $sp, 96
        sw	$gp, 92($sp)
        sw      $31, 88($sp)
    /* Save all callee-save registers */
        sw      $16, 0($sp)
        sw      $17, 4($sp)
        sw      $18, 8($sp)
        sw      $19, 12($sp)
        sw      $20, 16($sp)
        sw      $21, 20($sp)
        sw      $22, 24($sp)
        sw      $23, 28($sp)
        sw      $30, 32($sp)
/*        s.d     $f20, 40($sp)
        s.d     $f22, 48($sp)
        s.d     $f24, 56($sp)
        s.d     $f26, 64($sp)
        s.d     $f28, 72($sp)
        s.d     $f30, 80($sp) */
    /* Set up a callback link on the stack. */
        subu    $sp, $sp, 16
        lw      $2, caml_bottom_of_stack
        sw      $2, 0($sp)
        lw      $3, caml_last_return_address
        sw      $3, 4($sp)
        lw      $4, caml_gc_regs
        sw      $4, 8($sp)
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        subu    $sp, $sp, 8
        lw      $30, caml_exception_pointer
        sw      $30, 0($sp)
        la      $2, $105
        sw      $2, 4($sp)
        move    $30, $sp
    /* Reload allocation pointers */
        lw      $22, caml_young_ptr
        lw      $23, caml_young_limit
    /* Say that we are back into Caml code */
        sw      $0, caml_last_return_address
    /* Call the Caml code */
        move    $25, $24
$104:   jal     $25
    /* Reload $gp based on return address */
/*        .set    noreorder
        .cpload $31
        .set    reorder */
        lw	$gp, 116($sp)
    /* Pop the trap frame, restoring caml_exception_pointer */
        lw      $24, 0($sp)
        sw      $24, caml_exception_pointer
        addu    $sp, $sp, 8
.Lreturn_result:
    /* Pop the callback link, restoring the global variables */
        lw      $24, 0($sp)
        sw      $24, caml_bottom_of_stack
        lw      $25, 4($sp)
        sw      $25, caml_last_return_address
        lw      $24, 8($sp)
        sw      $24, caml_gc_regs
        addu    $sp, $sp, 16
    /* Update allocation pointer */
        sw      $22, caml_young_ptr
    /* Reload callee-save registers and return */
        lw      $31, 88($sp)
        lw      $16, 0($sp)
        lw      $17, 4($sp)
        lw      $18, 8($sp)
        lw      $19, 12($sp)
        lw      $20, 16($sp)
        lw      $21, 20($sp)
        lw      $22, 24($sp)
        lw      $23, 28($sp)
        lw      $30, 32($sp)
/*        l.d     $f20, 40($sp)
        l.d     $f22, 48($sp)
        l.d     $f24, 56($sp)
        l.d     $f26, 64($sp)
        l.d     $f28, 72($sp)
        l.d     $f30, 80($sp) */
        addu    $sp, $sp, 96
        j       $31

    /* The trap handler: re-raise the exception through mlraise,
       so that local C roots are cleaned up correctly. */
$105:
    /* Reload $gp based on trap address (still in $25) */
        .set    noreorder
        .cpload $25
        .set    reorder
        sw	$30, caml_exception_pointer
        or	$2, $2, 2
        b	.Lreturn_result
/*        sw      $22, caml_young_ptr
        sw      $30, caml_exception_pointer
        lw      $24, 0($sp)
        sw      $24, caml_bottom_of_stack
        lw      $25, 4($sp)
        sw      $25, caml_last_return_address
        lw      $24, 8($sp)
        sw      $24, caml_gc_regs
        addu    $sp, $sp, 16
        move    $4, $2
        jal     mlraise         */

        .end    caml_start_program

        /* Raise an exception from OCaml */
        .globl	caml_raise_exn
caml_raise_exn:
        .set	noreorder
        .cpload	$25
        .set	reorder
        /* Test if backtrace is active */
        lw	$24, caml_backtrace_active
        beqz	$24, 1f
        /* Preserve exception bucket in callee-save register $16 */
        move	$16, $2
        /* Stash the backtrace */
        move	$4, $2
        move	$5, $31
        move	$6, $sp
        move	$7, $30
        subu	$sp, $sp, 24
        sw      $gp, 16($sp)
        la	$25, caml_stash_backtrace
        jal	$25
        lw	$gp, 16($sp)
        addu	$sp, $sp, 24
        /* Restore exception bucket */
        move	$2, $16
1:      /* Cut stack at current trap handler */
        move	$sp, $30
        /* Pop previous handler and addr of trap, and jump to it */
        lw	$30, 0($sp)
        lw	$25, 4($sp)
        addu	$sp, $sp, 8
        j	$25
        .type	caml_raise_exn, @function
        .size	caml_raise_exn, .-caml_raise_exn

/* Raise an exception from C */

        .globl  caml_raise_exception
caml_raise_exception:
        .set    noreorder
        .cpload $25
        .set    reorder
        move    $2, $4
        /* Reload trap ptr, alloc ptr and alloc limit */
        lw	$30, caml_exception_pointer
        lw      $22, caml_young_ptr
        lw      $23, caml_young_limit
        /* Test if backtrace is active */
        lw	$24, caml_backtrace_active
        beqz	$24, 1f
        /* Preserve exception bucket in callee-save register $18 */
        move	$18, $2
        lw	$5, caml_last_return_address
        lw	$6, caml_bottom_of_stack
        move	$7, $30
        subu	$sp, $sp, 16
        la	$25, caml_stash_backtrace
        jal	$25
        addu	$sp, $sp, 16
        /* Restore exception bucket */
        move	$2, $18
1:      /* Cut stack at curren trap handler */
        move	$sp, $30
        /* Pop previous handler and addr of trap, and jump to it */
        lw      $30, 0($sp)
        lw      $25, 4($sp)
        addu    $sp, $sp, 8
        j       $25
        .type   caml_raise_exception, @function
        .size   caml_raise_exception, .-caml_raise_exception

/* Callback from C to Caml */

        .globl  caml_callback_exn
        .ent    caml_callback_exn
caml_callback_exn:
        .set    noreorder
        .cpload $25
        .set    reorder
    /* Initial shuffling of arguments */
        move    $9, $4          /* closure */
        move    $8, $5          /* argument */
        lw      $24, 0($4)      /* code pointer */
        b       $103
        .end    caml_callback_exn

        .globl  caml_callback2_exn
        .ent    caml_callback2_exn
caml_callback2_exn:
        .set    noreorder
        .cpload $25
        .set    reorder
    /* Initial shuffling of arguments */
        move    $10, $4                 /* closure */
        move    $8, $5                  /* first argument */
        move    $9, $6                  /* second argument */
        la      $24, caml_apply2        /* code pointer */
        b       $103

        .end    caml_callback2_exn

        .globl  caml_callback3_exn
        .ent    caml_callback3_exn
caml_callback3_exn:
        .set    noreorder
        .cpload $25
        .set    reorder
    /* Initial shuffling of arguments */
        move    $11, $4                 /* closure */
        move    $8, $5                  /* first argument */
        move    $9, $6                  /* second argument */
        move    $10, $7                 /* third argument */
        la      $24, caml_apply3        /* code pointer */
        b       $103

        .end    caml_callback3_exn

        .globl  caml_ml_array_bound_error
        .ent	caml_ml_array_bound_error
caml_ml_array_bound_error:
        .set	noreorder
        .cpload	$25
        .set	reorder
        la	$24, caml_array_bound_error
        la	$25, caml_c_call
        j	$25

        .end	caml_ml_array_bound_error

        .globl	caml_system__code_end
caml_system__code_end:

        .rdata
        .globl  caml_system__frametable
caml_system__frametable:
        .word   1               /* one descriptor */
        .word   $104 + 8        /* return address into callback */
        .half   -1              /* negative frame size => use callback link */
        .half   0               /* no roots here */
