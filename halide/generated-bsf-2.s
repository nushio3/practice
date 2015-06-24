	.file	"generated-bsf-2.c"
	.section	.rodata.cst4,"aM",@progbits,4
	.align	4
.LCPI0_0:
	.long	1056964608              # float 0.5
.LCPI0_1:
	.long	1048576000              # float 0.25
	.text
	.globl	main_compute
	.align	16, 0x90
	.type	main_compute,@function
main_compute:                           # @main_compute
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r15
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r14
.Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%r13
.Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%r12
.Ltmp11:
	.cfi_def_cfa_offset 48
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 56
	subq	$808, %rsp              # imm = 0x328
.Ltmp13:
	.cfi_def_cfa_offset 864
.Ltmp14:
	.cfi_offset %rbx, -56
.Ltmp15:
	.cfi_offset %r12, -48
.Ltmp16:
	.cfi_offset %r13, -40
.Ltmp17:
	.cfi_offset %r14, -32
.Ltmp18:
	.cfi_offset %r15, -24
.Ltmp19:
	.cfi_offset %rbp, -16
	movq	8(%rdi), %rax
	xorb	%cl, %cl
	testq	%rax, %rax
	movb	$0, %dl
	jne	.LBB0_2
# BB#1:
	cmpq	$0, (%rdi)
	sete	%dl
.LBB0_2:
	movb	%dl, 736(%rsp)          # 1-byte Spill
	movq	%rax, 496(%rsp)         # 8-byte Spill
	movl	64(%rdi), %eax
	movl	%eax, 752(%rsp)         # 4-byte Spill
	movslq	36(%rdi), %rax
	movq	%rax, 504(%rsp)         # 8-byte Spill
	movl	32(%rdi), %eax
	movl	%eax, 768(%rsp)         # 4-byte Spill
	movl	48(%rdi), %eax
	movl	%eax, 800(%rsp)         # 4-byte Spill
	movslq	20(%rdi), %rax
	movq	%rax, 792(%rsp)         # 8-byte Spill
	movslq	16(%rdi), %rax
	movq	%rax, 784(%rsp)         # 8-byte Spill
	movl	52(%rdi), %eax
	movq	%rdi, 760(%rsp)         # 8-byte Spill
	movl	%eax, 516(%rsp)         # 4-byte Spill
	movq	8(%rsi), %rax
	movq	%rax, 472(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB0_4
# BB#3:
	cmpq	$0, (%rsi)
	sete	%cl
.LBB0_4:
	movb	%cl, 740(%rsp)          # 1-byte Spill
	movslq	16(%rsi), %r10
	movq	%r10, 776(%rsp)         # 8-byte Spill
	leal	511(%r10), %ecx
	sarl	$9, %ecx
	movl	%ecx, 24(%rsp)          # 4-byte Spill
	movslq	20(%rsi), %rdx
	movq	%rdx, 720(%rsp)         # 8-byte Spill
	leal	511(%rdx), %eax
	movq	%rdx, %r8
	sarl	$9, %eax
	imull	%ecx, %eax
	movl	%eax, 28(%rsp)          # 4-byte Spill
	leal	-1(%rax), %r14d
	movl	%r14d, %eax
	cltd
	idivl	%ecx
	subl	%edx, %r14d
	sarl	$31, %edx
	movl	%ecx, %eax
	negl	%eax
	movl	%eax, 20(%rsp)          # 4-byte Spill
	andl	%eax, %edx
	movl	32(%rsi), %eax
	movl	%eax, 744(%rsp)         # 4-byte Spill
	movl	48(%rsi), %edi
	movl	%edi, 356(%rsp)         # 4-byte Spill
	addl	%edx, %r14d
	leal	1(%rdi), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %r13d            # imm = 0x1FFF
	cmovgel	%r13d, %eax
	xorl	%r9d, %r9d
	testl	%eax, %eax
	cmovsl	%r9d, %eax
	cmpl	%eax, %edi
	cmovlel	%edi, %eax
	leal	-1(%rdi), %ebp
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%r13d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r9d, %ebp
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	leal	(%r10,%rdi), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovll	%edx, %eax
	movl	%edx, %r15d
	testl	%eax, %eax
	cmovsl	%r9d, %eax
	leal	-1(%r10,%rdi), %edx
	movl	%edx, 732(%rsp)         # 4-byte Spill
	cmpl	%eax, %edx
	cmovgel	%edx, %eax
	leal	-2(%r10,%rdi), %r11d
	cmpl	$8192, %r11d            # imm = 0x2000
	cmovgel	%r13d, %r11d
	testl	%r11d, %r11d
	cmovsl	%r9d, %r11d
	cmpl	%r11d, %eax
	cmovgel	%eax, %r11d
	movl	52(%rsi), %ebx
	movl	%ebx, 276(%rsp)         # 4-byte Spill
	leal	(%r8,%rbx), %edx
	movl	%edx, 804(%rsp)         # 4-byte Spill
	cmpl	$8192, %edx             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovll	%edx, %eax
	testl	%eax, %eax
	cmovsl	%r9d, %eax
	leal	-1(%r8,%rbx), %edx
	movl	%edx, 728(%rsp)         # 4-byte Spill
	cmpl	%eax, %edx
	cmovgel	%edx, %eax
	leal	-2(%r8,%rbx), %r9d
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%r13d, %r9d
	testl	%r9d, %r9d
	movl	$0, %edx
	cmovsl	%edx, %r9d
	cmpl	%r9d, %eax
	cmovgel	%eax, %r9d
	leal	1(%rbx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%edx, %eax
	xorl	%r12d, %r12d
	cmpl	%eax, %ebx
	cmovlel	%ebx, %eax
	movq	%rsi, 712(%rsp)         # 8-byte Spill
	leal	-1(%rbx), %r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%r13d, %r8d
	testl	%r8d, %r8d
	cmovsl	%r12d, %r8d
	cmpl	%r8d, %eax
	cmovlel	%eax, %r8d
	movl	%r14d, %eax
	cltd
	idivl	%ecx
	leal	-1(%r10), %edx
	andl	$-512, %edx             # imm = 0xFFFFFFFFFFFFFE00
	leal	512(%rdi,%rdx), %edx
	leal	-512(%r10,%rdi), %ecx
	movl	%ecx, 16(%rsp)          # 4-byte Spill
	cmpl	%ecx, %edi
	movl	%ecx, %esi
	cmovlel	%edi, %esi
	cmpl	%r15d, %edx
	cmovlel	%edx, %r15d
	movl	%r15d, 708(%rsp)        # 4-byte Spill
	leal	1(%rbp), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r12d, %edx
	cmpl	%edx, %ebp
	cmovlel	%ebp, %edx
	decl	%ebp
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%r13d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r12d, %ebp
	cmpl	%ebp, %edx
	cmovlel	%edx, %ebp
	leal	1(%r11), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r12d, %edx
	cmpl	%edx, %r11d
	cmovgel	%r11d, %edx
	decl	%r11d
	cmpl	$8192, %r11d            # imm = 0x2000
	cmovgel	%r13d, %r11d
	testl	%r11d, %r11d
	cmovsl	%r12d, %r11d
	cmpl	%r11d, %edx
	cmovgel	%edx, %r11d
	leal	1(%r9), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r12d, %edx
	cmpl	%edx, %r9d
	cmovgel	%r9d, %edx
	decl	%r9d
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%r13d, %r9d
	testl	%r9d, %r9d
	cmovsl	%r12d, %r9d
	cmpl	%r9d, %edx
	cmovgel	%edx, %r9d
	leal	1(%r8), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r12d, %edx
	cmpl	%edx, %r8d
	cmovlel	%r8d, %edx
	decl	%r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%r13d, %r8d
	testl	%r8d, %r8d
	cmovsl	%r12d, %r8d
	cmpl	%r8d, %edx
	cmovlel	%edx, %r8d
	movl	%eax, %edx
	shll	$9, %edx
	movl	%eax, %ecx
	sarl	$31, %ecx
	andl	%edx, %ecx
	movl	%ecx, 704(%rsp)         # 4-byte Spill
	testl	%eax, %eax
	cmovlel	%r12d, %edx
	leal	512(%rbx,%rdx), %eax
	movl	804(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %eax
	cmovlel	%eax, %ecx
	movl	%ecx, 804(%rsp)         # 4-byte Spill
	leal	1(%rbp), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	movl	$0, %ecx
	cmovsl	%ecx, %eax
	cmpl	%eax, %ebp
	cmovlel	%ebp, %eax
	decl	%ebp
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%r13d, %ebp
	testl	%ebp, %ebp
	cmovsl	%ecx, %ebp
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	leal	1(%r11), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%ecx, %eax
	xorl	%edi, %edi
	cmpl	%eax, %r11d
	cmovgel	%r11d, %eax
	decl	%r11d
	cmpl	$8192, %r11d            # imm = 0x2000
	cmovgel	%r13d, %r11d
	testl	%r11d, %r11d
	cmovsl	%edi, %r11d
	cmpl	%r11d, %eax
	cmovgel	%eax, %r11d
	movl	%r11d, %eax
	subl	%ebp, %eax
	andl	$-4, %eax
	leal	3(%rbp,%rax), %ebx
	cmpl	%r11d, %ebx
	cmovgl	%r11d, %ebx
	addl	$-3, %r11d
	cmpl	%r11d, %ebp
	cmovlel	%ebp, %r11d
	leal	1(%r9), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%edi, %eax
	cmpl	%eax, %r9d
	cmovgel	%r9d, %eax
	decl	%r9d
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%r13d, %r9d
	testl	%r9d, %r9d
	cmovsl	%edi, %r9d
	cmpl	%r9d, %eax
	cmovgel	%eax, %r9d
	leal	1(%r8), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%edi, %eax
	cmpl	%eax, %r8d
	cmovlel	%r8d, %eax
	decl	%r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%r13d, %r8d
	testl	%r8d, %r8d
	cmovsl	%edi, %r8d
	cmpl	%r8d, %eax
	cmovlel	%eax, %r8d
	leal	1(%rbx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%edi, %eax
	cmpl	%eax, %ebx
	cmovgel	%ebx, %eax
	decl	%ebx
	leal	1(%r11), %r10d
	cmpl	$8192, %r10d            # imm = 0x2000
	cmovgel	%r13d, %r10d
	leal	1(%r9), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r13d, %edx
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%r13d, %ebx
	leal	-1(%r11), %r15d
	cmpl	$8192, %r15d            # imm = 0x2000
	cmovgel	%r13d, %r15d
	leal	1(%r8), %r14d
	cmpl	$8192, %r14d            # imm = 0x2000
	cmovgel	%r13d, %r14d
	leal	-1(%r8), %r12d
	cmpl	$8192, %r12d            # imm = 0x2000
	cmovgel	%r13d, %r12d
	leal	-1(%r9), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovll	%ecx, %r13d
	testl	%r10d, %r10d
	cmovsl	%edi, %r10d
	cmpl	%r10d, %r11d
	cmovlel	%r11d, %r10d
	testl	%edx, %edx
	movl	$0, %ecx
	cmovsl	%ecx, %edx
	cmpl	%edx, %r9d
	cmovgel	%r9d, %edx
	movl	%esi, %ebp
	testl	%r14d, %r14d
	cmovsl	%ecx, %r14d
	cmpl	%r14d, %r8d
	cmovlel	%r8d, %r14d
	movl	804(%rsp), %r11d        # 4-byte Reload
	movl	708(%rsp), %r8d         # 4-byte Reload
	movq	712(%rsp), %rsi         # 8-byte Reload
	movb	740(%rsp), %r9b         # 1-byte Reload
	testl	%r15d, %r15d
	cmovsl	%ecx, %r15d
	testl	%r13d, %r13d
	cmovsl	%ecx, %r13d
	testl	%r12d, %r12d
	cmovsl	%ecx, %r12d
	xorl	%ecx, %ecx
	testl	%ebx, %ebx
	cmovsl	%ecx, %ebx
	cmpl	%ebx, %eax
	cmovgel	%eax, %ebx
	cmpl	%r15d, %r10d
	cmovlel	%r10d, %r15d
	movq	720(%rsp), %r10         # 8-byte Reload
	cmpl	%r12d, %r14d
	cmovlel	%r14d, %r12d
	movl	704(%rsp), %edi         # 4-byte Reload
	cmpl	%r13d, %edx
	cmovgel	%edx, %r13d
	movl	276(%rsp), %ecx         # 4-byte Reload
	leal	-512(%r10,%rcx), %eax
	movl	%eax, 12(%rsp)          # 4-byte Spill
	addl	%ecx, %edi
	cmpl	%eax, %edi
	cmovgl	%eax, %edi
	testb	%r9b, %r9b
	movl	64(%rsi), %edx
	movslq	36(%rsi), %rax
	movq	%rax, 264(%rsp)         # 8-byte Spill
	je	.LBB0_6
# BB#5:
	movl	%r8d, %eax
	subl	%ebp, %eax
	movl	%ebp, 48(%rsi)
	movl	%edi, 52(%rsi)
	movl	$0, 56(%rsi)
	movl	$0, 60(%rsi)
	movl	%eax, 16(%rsi)
	movl	%r11d, %ecx
	subl	%edi, %ecx
	movl	%ecx, 20(%rsi)
	movl	$0, 24(%rsi)
	movl	$0, 28(%rsi)
	movl	$1, 32(%rsi)
	movl	%eax, 36(%rsi)
	movq	$0, 40(%rsi)
.LBB0_6:
	movl	%ebp, %r14d
	movl	%r8d, %ebp
	movb	736(%rsp), %r8b         # 1-byte Reload
	testb	%r8b, %r8b
	movl	800(%rsp), %r11d        # 4-byte Reload
	movq	760(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_8
# BB#7:                                 # %.thread
	subl	%r15d, %ebx
	movl	%r15d, 48(%rcx)
	movl	%r12d, 52(%rcx)
	movl	$0, 56(%rcx)
	movl	$0, 60(%rcx)
	incl	%ebx
	movl	%ebx, 16(%rcx)
	movl	$1, %eax
	subl	%r12d, %eax
	addl	%r13d, %eax
	movl	%eax, 20(%rcx)
	xorl	%eax, %eax
	movl	$0, 24(%rcx)
	movl	$0, 28(%rcx)
	movl	$1, 32(%rcx)
	movl	%ebx, 36(%rcx)
	movq	$0, 40(%rcx)
	jmp	.LBB0_119
.LBB0_8:
	xorl	%eax, %eax
	orb	%r8b, %r9b
	movq	776(%rsp), %rsi         # 8-byte Reload
	jne	.LBB0_119
# BB#9:
	cmpl	$4, %edx
	movl	356(%rsp), %ecx         # 4-byte Reload
	jne	.LBB0_10
# BB#12:
	movl	752(%rsp), %edx         # 4-byte Reload
	cmpl	$4, %edx
	jne	.LBB0_13
# BB#14:
	movl	%r14d, %edx
	cmpl	%edx, %ecx
	jle	.LBB0_17
# BB#15:
	xorl	%edi, %edi
	movl	$.L.str2, %esi
	jmp	.LBB0_16
.LBB0_10:
	xorl	%edi, %edi
	movl	$.L.str, %esi
	jmp	.LBB0_11
.LBB0_13:
	xorl	%edi, %edi
	movl	$.L.str1, %esi
.LBB0_11:
	xorb	%al, %al
	callq	halide_printf
	movl	$-1, %eax
	jmp	.LBB0_119
.LBB0_17:
	movl	%ecx, %edx
	movl	%ebp, %ecx
	subl	%esi, %ecx
	movq	%rsi, %r14
	cmpl	%edx, %ecx
	movl	%edi, %r8d
	jle	.LBB0_19
# BB#18:
	decl	%ebp
	xorl	%edi, %edi
	movl	$.L.str3, %esi
	movl	%ebp, %edx
	movl	732(%rsp), %ecx         # 4-byte Reload
	jmp	.LBB0_16
.LBB0_19:
	movl	276(%rsp), %ecx         # 4-byte Reload
	cmpl	%r8d, %ecx
	movq	784(%rsp), %rsi         # 8-byte Reload
	jle	.LBB0_21
# BB#20:
	xorl	%edi, %edi
	movl	$.L.str4, %esi
	movl	%r8d, %edx
	jmp	.LBB0_16
.LBB0_21:
	movl	%edx, %r8d
	movl	804(%rsp), %edx         # 4-byte Reload
	movl	%edx, %edi
	subl	%r10d, %edi
	cmpl	%ecx, %edi
	jle	.LBB0_23
# BB#22:
	decl	%edx
	xorl	%edi, %edi
	movl	$.L.str5, %esi
	movl	728(%rsp), %ecx         # 4-byte Reload
	jmp	.LBB0_16
.LBB0_23:
	cmpl	%r15d, %r11d
	jle	.LBB0_25
# BB#24:
	xorl	%edi, %edi
	movl	$.L.str6, %esi
	movl	%r15d, %edx
	movl	%r11d, %ecx
	jmp	.LBB0_16
.LBB0_25:
	movl	%ecx, %r9d
	movl	%ebx, %ecx
	subl	%esi, %ecx
	cmpl	%r11d, %ecx
	movq	792(%rsp), %rdi         # 8-byte Reload
	jge	.LBB0_26
# BB#27:
	movl	516(%rsp), %ecx         # 4-byte Reload
	cmpl	%r12d, %ecx
	jle	.LBB0_29
# BB#28:
	xorl	%edi, %edi
	movl	$.L.str8, %esi
	movl	%r12d, %edx
	jmp	.LBB0_16
.LBB0_26:
	leal	-1(%r11,%rsi), %ecx
	xorl	%edi, %edi
	movl	$.L.str7, %esi
	movl	%ebx, %edx
	jmp	.LBB0_16
.LBB0_29:
	movl	%ecx, %edx
	movl	%r13d, %ecx
	subl	%edi, %ecx
	cmpl	%edx, %ecx
	movl	%edx, %ecx
	jge	.LBB0_30
# BB#31:
	cmpl	$1, 744(%rsp)           # 4-byte Folded Reload
	jne	.LBB0_32
# BB#34:
	movl	%ecx, %ebp
	cmpl	$1, 768(%rsp)           # 4-byte Folded Reload
	jne	.LBB0_35
# BB#36:
	movq	264(%rsp), %rdx         # 8-byte Reload
	imulq	%r10, %rdx
	movl	$2147483648, %ecx       # imm = 0x80000000
	cmpq	%rcx, %rdx
	jge	.LBB0_37
# BB#38:
	movq	%r10, %rdx
	imulq	%r14, %rdx
	cmpq	%rcx, %rdx
	jge	.LBB0_39
# BB#40:
	movq	504(%rsp), %rcx         # 8-byte Reload
	imulq	%rdi, %rcx
	movl	$2147483648, %edx       # imm = 0x80000000
	cmpq	%rdx, %rcx
	jge	.LBB0_41
# BB#42:
	imulq	%rsi, %rdi
	cmpq	$2147483647, %rdi       # imm = 0x7FFFFFFF
	jg	.LBB0_120
# BB#43:                                # %.preheader1257
	cmpl	$0, 28(%rsp)            # 4-byte Folded Reload
	movq	%r10, %rcx
	jle	.LBB0_119
# BB#44:                                # %.lr.ph1260
	movl	$511, %ebx              # imm = 0x1FF
	movl	$511, %r10d             # imm = 0x1FF
	subl	%r14d, %r10d
	subl	%ecx, %ebx
	subl	%r9d, %ebx
	movl	%ebx, 8(%rsp)           # 4-byte Spill
	subl	%r8d, %r10d
	movl	%r10d, 4(%rsp)          # 4-byte Spill
	movl	%ebp, %esi
	movq	504(%rsp), %rax         # 8-byte Reload
	imull	%eax, %esi
	addl	%r11d, %esi
	movl	%esi, 516(%rsp)         # 4-byte Spill
	xorl	%r13d, %r13d
	movl	$8191, %r11d            # imm = 0x1FFF
	xorl	%ebp, %ebp
.LBB0_45:                               # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_46 Depth 2
                                        #       Child Loop BB0_47 Depth 3
                                        #         Child Loop BB0_68 Depth 4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_71 Depth 6
                                        #               Child Loop BB0_73 Depth 7
                                        #             Child Loop BB0_77 Depth 6
                                        #               Child Loop BB0_80 Depth 7
                                        #             Child Loop BB0_85 Depth 6
                                        #               Child Loop BB0_87 Depth 7
                                        #             Child Loop BB0_91 Depth 6
                                        #               Child Loop BB0_94 Depth 7
                                        #             Child Loop BB0_99 Depth 6
                                        #               Child Loop BB0_101 Depth 7
                                        #             Child Loop BB0_105 Depth 6
                                        #               Child Loop BB0_108 Depth 7
                                        #             Child Loop BB0_113 Depth 6
	movl	%ebp, 32(%rsp)          # 4-byte Spill
	movl	%ebp, %eax
	cltd
	movl	24(%rsp), %esi          # 4-byte Reload
	idivl	%esi
	movl	%edx, %edi
	movl	%ebp, %eax
	subl	%edi, %eax
	movl	%edi, %ecx
	sarl	$31, %ecx
	movl	%ecx, %edx
	andl	20(%rsp), %edx          # 4-byte Folded Reload
	addl	%edx, %eax
	cltd
	idivl	%esi
	andl	%esi, %ecx
	addl	%edi, %ecx
	shll	$9, %ecx
	shll	$9, %eax
	addl	%r9d, %eax
	addl	%r8d, %ecx
	movl	16(%rsp), %edx          # 4-byte Reload
	cmpl	%edx, %ecx
	cmovlel	%ecx, %edx
	movl	%edx, 140(%rsp)         # 4-byte Spill
	notl	%ecx
	movl	12(%rsp), %edx          # 4-byte Reload
	cmpl	%edx, %eax
	cmovlel	%eax, %edx
	movl	%edx, 96(%rsp)          # 4-byte Spill
	notl	%eax
	cmpl	%ebx, %eax
	cmovll	%ebx, %eax
	cmpl	%r10d, %ecx
	cmovll	%r10d, %ecx
	movl	%ecx, 100(%rsp)         # 4-byte Spill
	movl	$1, %edx
	subl	%ecx, %edx
	movl	%edx, 136(%rsp)         # 4-byte Spill
	movl	$3, %edx
	subl	%ecx, %edx
	movl	%edx, 132(%rsp)         # 4-byte Spill
	movl	$26, %edx
	subl	%ecx, %edx
	movl	%edx, 92(%rsp)          # 4-byte Spill
	movl	$30, %edx
	subl	%ecx, %edx
	movl	%edx, 88(%rsp)          # 4-byte Spill
	movl	$2, %edx
	subl	%ecx, %edx
	movl	%edx, 212(%rsp)         # 4-byte Spill
	movl	$-2, %edx
	subl	%ecx, %edx
	movl	%edx, 84(%rsp)          # 4-byte Spill
	movl	$-2, %r8d
	subl	%eax, %r8d
	movl	%ecx, %esi
	negl	%esi
	movl	%esi, 128(%rsp)         # 4-byte Spill
	leal	-3(%rcx), %esi
	movl	%esi, 80(%rsp)          # 4-byte Spill
	leal	-26(%rcx), %esi
	movl	%esi, 76(%rsp)          # 4-byte Spill
	leal	-29(%rcx), %esi
	movl	%esi, 72(%rsp)          # 4-byte Spill
	leal	-27(%rcx), %esi
	movl	%esi, 68(%rsp)          # 4-byte Spill
	leal	-28(%rcx), %esi
	movl	%esi, 64(%rsp)          # 4-byte Spill
	leal	-32(%rcx), %esi
	movl	%esi, 60(%rsp)          # 4-byte Spill
	leal	-30(%rcx), %esi
	movl	%esi, 56(%rsp)          # 4-byte Spill
	leal	-2(%rcx), %esi
	movl	%esi, 52(%rsp)          # 4-byte Spill
	leal	2(%rcx), %esi
	movl	%esi, 48(%rsp)          # 4-byte Spill
	leal	1(%rcx), %esi
	movl	%esi, 44(%rsp)          # 4-byte Spill
	leal	-1(%rcx), %esi
	movl	%esi, 40(%rsp)          # 4-byte Spill
	leal	-4(%rcx), %ecx
	movl	%ecx, 36(%rsp)          # 4-byte Spill
	leal	2(%rax), %ebp
	leal	1(%rax), %edi
	leal	-1(%rax), %edx
	xorl	%ecx, %ecx
	movl	%r11d, %r10d
.LBB0_46:                               #   Parent Loop BB0_45 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_47 Depth 3
                                        #         Child Loop BB0_68 Depth 4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_71 Depth 6
                                        #               Child Loop BB0_73 Depth 7
                                        #             Child Loop BB0_77 Depth 6
                                        #               Child Loop BB0_80 Depth 7
                                        #             Child Loop BB0_85 Depth 6
                                        #               Child Loop BB0_87 Depth 7
                                        #             Child Loop BB0_91 Depth 6
                                        #               Child Loop BB0_94 Depth 7
                                        #             Child Loop BB0_99 Depth 6
                                        #               Child Loop BB0_101 Depth 7
                                        #             Child Loop BB0_105 Depth 6
                                        #               Child Loop BB0_108 Depth 7
                                        #             Child Loop BB0_113 Depth 6
	movl	%ecx, 104(%rsp)         # 4-byte Spill
	movl	%edx, 108(%rsp)         # 4-byte Spill
	movl	%edi, 112(%rsp)         # 4-byte Spill
	movl	%ebp, 116(%rsp)         # 4-byte Spill
	movl	%r8d, 120(%rsp)         # 4-byte Spill
	movl	%eax, 124(%rsp)         # 4-byte Spill
	cmpl	$-8193, %eax            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r12d           # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%eax, %r12d
	notl	%r12d
	movl	%ecx, %esi
	shll	$5, %esi
	cmpl	%r12d, %r8d
	movl	96(%rsp), %ebx          # 4-byte Reload
	leal	-1(%rsi,%rbx), %r11d
	leal	1(%rsi,%rbx), %ecx
	cmovgel	%r8d, %r12d
	cmpl	$-8193, %ebp            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r8d            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%ebp, %r8d
	cmpl	$-8193, %edx            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %ebp            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%edx, %ebp
	cmpl	$-8193, %edi            # imm = 0xFFFFFFFFFFFFDFFF
	leal	-2(%rsi,%rbx), %edx
	movl	$-8192, %r14d           # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%edi, %r14d
	leal	32(%rsi,%rbx), %r15d
	leal	30(%rsi,%rbx), %edi
	movl	%edi, 804(%rsp)         # 4-byte Spill
	leal	31(%rsi,%rbx), %r9d
	leal	(%rsi,%rbx), %ebx
	movl	%ebx, 292(%rsp)         # 4-byte Spill
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%r10d, %ecx
	xorl	%edi, %edi
	testl	%ecx, %ecx
	cmovsl	%edi, %ecx
	cmpl	%ecx, %ebx
	cmovlel	%ebx, %ecx
	cmpl	$8192, %ebx             # imm = 0x2000
	movl	$8191, %esi             # imm = 0x1FFF
	cmovll	%ebx, %esi
	testl	%esi, %esi
	cmovsl	%edi, %esi
	cmpl	%esi, %r11d
	cmovgel	%r11d, %esi
	cmpl	$8192, %r11d            # imm = 0x2000
	cmovgel	%r10d, %r11d
	testl	%r11d, %r11d
	cmovsl	%edi, %r11d
	cmpl	%r11d, %ecx
	cmovlel	%ecx, %r11d
	notl	%r8d
	cmpl	%r8d, %r12d
	cmovgel	%r12d, %r8d
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r10d, %edx
	testl	%edx, %edx
	movl	$0, %ebx
	cmovsl	%ebx, %edx
	cmpl	%edx, %esi
	cmovgel	%esi, %edx
	movl	%ebp, %ecx
	xorl	$-1, %ecx
	notl	%ebp
	cmovsl	%ebx, %ebp
	notl	%ebp
	cmpl	$8192, %r15d            # imm = 0x2000
	cmovgel	%r10d, %r15d
	testl	%r15d, %r15d
	cmovsl	%ebx, %r15d
	cmpl	%ebp, %eax
	cmovgel	%eax, %ebp
	leal	1(%r11), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r10d, %edi
	testl	%edi, %edi
	cmovsl	%ebx, %edi
	testl	%r8d, %r8d
	cmovsl	%ebx, %r8d
	cmpl	%r15d, %r9d
	cmovgel	%r9d, %r15d
	movl	%r14d, %ecx
	xorl	$-1, %ecx
	notl	%r14d
	cmovsl	%ebx, %r14d
	notl	%r14d
	movl	804(%rsp), %eax         # 4-byte Reload
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r10d, %eax
	testl	%eax, %eax
	cmovsl	%ebx, %eax
	xorl	%ebx, %ebx
	cmpl	%eax, %r15d
	cmovgel	%r15d, %eax
	movl	%eax, %r12d
	cmpl	%r14d, %ebp
	cmovgel	%ebp, %r14d
	leal	1(%rdx), %eax
	movl	$-2, %ecx
	subl	%r8d, %ecx
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %esi             # imm = 0x1FFF
	cmovll	%eax, %esi
	testl	%esi, %esi
	cmovsl	%ebx, %esi
	cmpl	%edi, %r11d
	cmovlel	%r11d, %edi
	cmpl	%eax, %r11d
	cmovlel	%r11d, %eax
	movl	%eax, 520(%rsp)         # 4-byte Spill
	movl	%eax, %ebp
	decl	%r11d
	cmpl	$8192, %r11d            # imm = 0x2000
	cmovgel	%r10d, %r11d
	cmpl	%esi, %edx
	cmovgel	%edx, %esi
	decl	%edx
	testl	%r11d, %r11d
	cmovsl	%ebx, %r11d
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r10d, %edx
	testl	%edx, %edx
	cmovsl	%ebx, %edx
	cmpl	%ecx, %r14d
	cmovgel	%r14d, %ecx
	movl	%ecx, 288(%rsp)         # 4-byte Spill
	leal	1(%r12), %eax
	movl	%eax, 804(%rsp)         # 4-byte Spill
	cmpl	%r11d, %edi
	cmovlel	%edi, %r11d
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %ecx             # imm = 0x1FFF
	cmovll	%eax, %ecx
	testl	%ecx, %ecx
	cmovsl	%ebx, %ecx
	cmpl	%ecx, %r12d
	cmovgel	%r12d, %ecx
	cmpl	%edx, %esi
	cmovgel	%esi, %edx
	leal	-1(%r12), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%r10d, %eax
	testl	%eax, %eax
	cmovsl	%ebx, %eax
	cmpl	%r9d, %r12d
	cmovgel	%r12d, %r9d
	incl	%r9d
	cmpl	%eax, %ecx
	cmovgel	%ecx, %eax
	movl	292(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %ebp
	movl	%ecx, %ebx
	cmovlel	%ebp, %ebx
	movl	%ebx, 284(%rsp)         # 4-byte Spill
	subl	%ebx, %r9d
	movslq	%r9d, %rsi
	movq	%rsi, %rcx
	shlq	$5, %rcx
	shlq	$7, %rsi
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	cmpq	$2147483647, %rsi       # imm = 0x7FFFFFFF
	seta	%bl
	cmpq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	setg	%cl
	orb	%bl, %cl
	movb	%cl, 199(%rsp)          # 1-byte Spill
	leal	1(%r11), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%r10d, %ecx
	testl	%ecx, %ecx
	movl	$0, %esi
	cmovsl	%esi, %ecx
	cmpl	%ecx, %r11d
	cmovlel	%r11d, %ecx
	leal	-1(%r11), %r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%r10d, %r8d
	testl	%r8d, %r8d
	cmovsl	%esi, %r8d
	xorl	%esi, %esi
	cmpl	%r8d, %ecx
	cmovlel	%ecx, %r8d
	leal	1(%rdx), %ecx
	cmpl	%ecx, %r11d
	cmovgl	%ecx, %r11d
	movl	%r11d, 576(%rsp)        # 4-byte Spill
	leal	1(%rbp), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r10d, %edi
	testl	%edi, %edi
	cmovsl	%esi, %edi
	cmpl	%ebp, %r11d
	movl	%ebp, %esi
	movl	%ebp, %ebx
	cmovlel	%r11d, %esi
	cmpl	%edi, %esi
	cmovgl	%edi, %esi
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%r10d, %ecx
	testl	%ecx, %ecx
	movl	$0, %edi
	cmovsl	%edi, %ecx
	cmpl	%ecx, %edx
	cmovgel	%edx, %ecx
	decl	%edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r10d, %edx
	testl	%edx, %edx
	cmovsl	%edi, %edx
	cmpl	%edx, %ecx
	cmovgel	%ecx, %edx
	leal	1(%rax), %ebp
	movl	%ebp, 800(%rsp)         # 4-byte Spill
	cmpl	$8192, %ebp             # imm = 0x2000
	movl	$8191, %ecx             # imm = 0x1FFF
	cmovll	%ebp, %ecx
	testl	%ecx, %ecx
	cmovsl	%edi, %ecx
	xorl	%edi, %edi
	cmpl	%ecx, %eax
	cmovgel	%eax, %ecx
	leal	-1(%rax), %ebp
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%r10d, %ebp
	testl	%ebp, %ebp
	cmovsl	%edi, %ebp
	cmpl	%ebp, %ecx
	cmovgel	%ecx, %ebp
	leal	-1(%rbx), %r9d
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%r10d, %r9d
	testl	%r9d, %r9d
	cmovsl	%edi, %r9d
	cmpl	%r9d, %esi
	cmovlel	%esi, %r9d
	movl	%r9d, 568(%rsp)         # 4-byte Spill
	leal	1(%r8), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%r10d, %ecx
	testl	%ecx, %ecx
	movl	$0, %esi
	cmovsl	%esi, %ecx
	cmpl	%ecx, %r8d
	cmovlel	%r8d, %ecx
	leal	-1(%r8), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r10d, %edi
	testl	%edi, %edi
	cmovsl	%esi, %edi
	cmpl	%edi, %ecx
	cmovlel	%ecx, %edi
	leal	1(%rdx), %ecx
	cmpl	%ecx, %r8d
	cmovgl	%ecx, %r8d
	movl	%r8d, 572(%rsp)         # 4-byte Spill
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%r10d, %ecx
	testl	%ecx, %ecx
	cmovsl	%esi, %ecx
	cmpl	%ecx, %edx
	cmovgel	%edx, %ecx
	decl	%edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r10d, %edx
	testl	%edx, %edx
	cmovsl	%esi, %edx
	cmpl	%edx, %ecx
	cmovgel	%ecx, %edx
	movl	$8191, %r14d            # imm = 0x1FFF
	leal	1(%rbp), %r15d
	cmpl	$8192, %r15d            # imm = 0x2000
	movl	$8191, %ecx             # imm = 0x1FFF
	cmovll	%r15d, %ecx
	testl	%ecx, %ecx
	cmovsl	%esi, %ecx
	cmpl	%ecx, %ebp
	cmovgel	%ebp, %ecx
	leal	-1(%rbp), %ebx
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%r14d, %ebx
	testl	%ebx, %ebx
	cmovsl	%esi, %ebx
	xorl	%r10d, %r10d
	cmpl	%ebx, %ecx
	cmovgel	%ecx, %ebx
	leal	1(%r11), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%r14d, %esi
	testl	%esi, %esi
	cmovsl	%r10d, %esi
	cmpl	%r11d, %r8d
	movl	%r11d, %ecx
	cmovlel	%r8d, %ecx
	cmpl	%esi, %ecx
	cmovgl	%esi, %ecx
	incl	%edx
	cmpl	%edx, %edi
	cmovlel	%edi, %edx
	leal	-1(%r11), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovgel	%edi, %esi
	leal	1(%r8), %r14d
	cmpl	$8192, %r14d            # imm = 0x2000
	cmovgel	%edi, %r14d
	movl	$8191, %r10d            # imm = 0x1FFF
	leal	-1(%r8), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r10d, %edi
	cmpl	%r12d, %eax
	cmovgel	%eax, %r12d
	cmpl	%eax, %ebp
	cmovgel	%ebp, %eax
	cmpl	%ebp, %ebx
	cmovll	%ebp, %ebx
	testl	%esi, %esi
	movl	$0, %ebp
	cmovsl	%ebp, %esi
	cmpl	%esi, %ecx
	cmovlel	%ecx, %esi
	movl	%esi, 564(%rsp)         # 4-byte Spill
	movl	288(%rsp), %ecx         # 4-byte Reload
	leal	1(%rcx), %ecx
	movl	%ecx, 280(%rsp)         # 4-byte Spill
	movl	804(%rsp), %ecx         # 4-byte Reload
	subl	520(%rsp), %ecx         # 4-byte Folded Reload
	movslq	%ecx, %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movl	800(%rsp), %ebp         # 4-byte Reload
	subl	%r11d, %ebp
	incl	%r12d
	subl	%r9d, %r12d
	movslq	%r12d, %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movslq	%ebp, %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	testl	%edi, %edi
	movl	$0, %ebp
	cmovsl	%ebp, %edi
	subl	%r8d, %r15d
	incl	%eax
	cmpl	%r8d, %edx
	cmovgl	%r8d, %edx
	testl	%r14d, %r14d
	movslq	%r15d, %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	cmovsl	%ebp, %r14d
	movl	$0, 316(%rsp)           # 4-byte Folded Spill
	cmpl	%r14d, %edx
	cmovlel	%edx, %r14d
	cmpl	%edi, %r14d
	cmovgl	%edi, %r14d
	movl	%r14d, 560(%rsp)        # 4-byte Spill
	incl	%ebx
	subl	%esi, %eax
	movslq	%eax, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	subl	%r14d, %ebx
	movslq	%ebx, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movl	80(%rsp), %eax          # 4-byte Reload
	movl	%eax, 312(%rsp)         # 4-byte Spill
	movl	76(%rsp), %r12d         # 4-byte Reload
	movl	92(%rsp), %eax          # 4-byte Reload
	movl	%eax, 252(%rsp)         # 4-byte Spill
	movl	72(%rsp), %eax          # 4-byte Reload
	movl	%eax, 248(%rsp)         # 4-byte Spill
	movl	68(%rsp), %r14d         # 4-byte Reload
	movl	64(%rsp), %eax          # 4-byte Reload
	movl	%eax, 244(%rsp)         # 4-byte Spill
	movl	60(%rsp), %eax          # 4-byte Reload
	movl	56(%rsp), %r15d         # 4-byte Reload
	movl	88(%rsp), %esi          # 4-byte Reload
	movl	52(%rsp), %ebp          # 4-byte Reload
	movl	212(%rsp), %r11d        # 4-byte Reload
	movl	48(%rsp), %edi          # 4-byte Reload
	movl	84(%rsp), %ecx          # 4-byte Reload
	movl	44(%rsp), %ebx          # 4-byte Reload
	movl	40(%rsp), %edx          # 4-byte Reload
	movl	100(%rsp), %r9d         # 4-byte Reload
	movl	36(%rsp), %r8d          # 4-byte Reload
	xorl	%r10d, %r10d
.LBB0_47:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_68 Depth 4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_71 Depth 6
                                        #               Child Loop BB0_73 Depth 7
                                        #             Child Loop BB0_77 Depth 6
                                        #               Child Loop BB0_80 Depth 7
                                        #             Child Loop BB0_85 Depth 6
                                        #               Child Loop BB0_87 Depth 7
                                        #             Child Loop BB0_91 Depth 6
                                        #               Child Loop BB0_94 Depth 7
                                        #             Child Loop BB0_99 Depth 6
                                        #               Child Loop BB0_101 Depth 7
                                        #             Child Loop BB0_105 Depth 6
                                        #               Child Loop BB0_108 Depth 7
                                        #             Child Loop BB0_113 Depth 6
	movl	%ebx, 340(%rsp)         # 4-byte Spill
	movl	%eax, 256(%rsp)         # 4-byte Spill
	movl	%esi, 260(%rsp)         # 4-byte Spill
	movl	%r10d, 216(%rsp)        # 4-byte Spill
	movl	%r8d, 324(%rsp)         # 4-byte Spill
	movl	%r9d, 328(%rsp)         # 4-byte Spill
	movl	%edx, 332(%rsp)         # 4-byte Spill
	movl	%ecx, 220(%rsp)         # 4-byte Spill
	movl	%edi, 224(%rsp)         # 4-byte Spill
	movl	%r11d, 228(%rsp)        # 4-byte Spill
	movl	%ebp, 336(%rsp)         # 4-byte Spill
	movl	%r15d, 232(%rsp)        # 4-byte Spill
	movl	%r14d, 236(%rsp)        # 4-byte Spill
	movl	%r12d, 240(%rsp)        # 4-byte Spill
	cmpl	$-8193, %r9d            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %esi            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r9d, %esi
	cmpl	$-8193, %edx            # imm = 0xFFFFFFFFFFFFDFFF
	movl	%ecx, %ebx
	movl	%edi, %ecx
	movl	$-8192, %edi            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%edx, %edi
	cmpl	$-8193, %ecx            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %eax            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%ecx, %eax
	notl	%esi
	cmpl	%esi, %ebx
	cmovgel	%ebx, %esi
	cmpl	$-8193, %r12d           # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %ecx            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r12d, %ecx
	notl	%eax
	movl	%edi, %r12d
	notl	%r12d
	xorl	$-1, %edi
	cmovsl	%r13d, %r12d
	cmpl	$-8193, %r14d           # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %edx            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r14d, %edx
	cmpl	%eax, %esi
	cmovgel	%esi, %eax
	notl	%ecx
	cmpl	$-8193, %ebp            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %esi            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%ebp, %esi
	notl	%esi
	movl	252(%rsp), %edi         # 4-byte Reload
	cmpl	%ecx, %edi
	cmovgel	%edi, %ecx
	cmpl	$-8193, %r8d            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r14d           # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r8d, %r14d
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%esi, %r11d
	cmovgel	%r11d, %esi
	movl	%r10d, %ebp
	shll	$5, %ebp
	movl	244(%rsp), %edi         # 4-byte Reload
	cmpl	$-8193, %edi            # imm = 0xFFFFFFFFFFFFDFFF
	notl	%r12d
	movl	$-8192, %ebx            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%edi, %ebx
	movl	%edi, %r8d
	movl	%edx, %edi
	xorl	$-1, %edi
	notl	%edx
	notl	%ebx
	notl	%r14d
	cmovsl	%r13d, %edx
	notl	%edx
	cmpl	%edx, %r8d
	cmovgel	%r8d, %edx
	cmpl	%r12d, %r9d
	cmovgel	%r9d, %r12d
	movl	$-2, %edi
	subl	%eax, %edi
	movl	%edi, 792(%rsp)         # 4-byte Spill
	cmpl	%r14d, %esi
	cmovgel	%esi, %r14d
	movl	%r14d, 784(%rsp)        # 4-byte Spill
	cmpl	%ebx, %ecx
	cmovgel	%ecx, %ebx
	movl	%ebx, 800(%rsp)         # 4-byte Spill
	movl	248(%rsp), %eax         # 4-byte Reload
	cmpl	$-8193, %eax            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %ecx            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%eax, %ecx
	movl	340(%rsp), %eax         # 4-byte Reload
	cmpl	$-8193, %eax            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r11d           # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%eax, %r11d
	movl	256(%rsp), %esi         # 4-byte Reload
	cmpl	$-8193, %esi            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %eax            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%esi, %eax
	movl	%eax, 760(%rsp)         # 4-byte Spill
	cmpl	$-8193, %r15d           # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %eax            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r15d, %eax
	movl	%eax, 752(%rsp)         # 4-byte Spill
	movl	140(%rsp), %edi         # 4-byte Reload
	leal	27(%rbp,%rdi), %r10d
	leal	28(%rbp,%rdi), %ebx
	leal	3(%rbp,%rdi), %eax
	movl	%eax, 776(%rsp)         # 4-byte Spill
	leal	4(%rbp,%rdi), %r14d
	leal	29(%rbp,%rdi), %esi
	leal	2(%rbp,%rdi), %eax
	movl	%eax, 804(%rsp)         # 4-byte Spill
	leal	26(%rbp,%rdi), %r8d
	leal	-2(%rbp,%rdi), %r9d
	leal	-1(%rbp,%rdi), %eax
	movl	%eax, 744(%rsp)         # 4-byte Spill
	leal	31(%rbp,%rdi), %eax
	movl	%eax, 524(%rsp)         # 4-byte Spill
	leal	32(%rbp,%rdi), %eax
	movl	%eax, 732(%rsp)         # 4-byte Spill
	leal	30(%rbp,%rdi), %eax
	movl	%eax, 740(%rsp)         # 4-byte Spill
	leal	1(%rbp,%rdi), %r15d
	movl	%r15d, 736(%rsp)        # 4-byte Spill
	leal	(%rbp,%rdi), %ebp
	movl	%ebp, 360(%rsp)         # 4-byte Spill
	movl	%r11d, %edi
	xorl	$-1, %edi
	notl	%r11d
	cmovsl	%r13d, %r11d
	notl	%r11d
	cmpl	%r11d, %r12d
	cmovgel	%r12d, %r11d
	movl	%r11d, %eax
	movl	%ecx, %edi
	xorl	$-1, %edi
	notl	%ecx
	cmovsl	%r13d, %ecx
	notl	%ecx
	cmpl	%ecx, %edx
	cmovgel	%edx, %ecx
	cmpl	$8192, %r14d            # imm = 0x2000
	movl	$8191, %edx             # imm = 0x1FFF
	cmovgel	%edx, %r14d
	testl	%r14d, %r14d
	cmovsl	%r13d, %r14d
	movl	776(%rsp), %edi         # 4-byte Reload
	cmpl	%r14d, %edi
	cmovgel	%edi, %r14d
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%edx, %esi
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	cmpl	%esi, %ebx
	cmovlel	%ebx, %esi
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%edx, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%ebx, %r10d
	cmovgel	%r10d, %ebx
	cmpl	$8192, %r10d            # imm = 0x2000
	cmovgel	%edx, %r10d
	testl	%r10d, %r10d
	cmovsl	%r13d, %r10d
	cmpl	%r10d, %esi
	cmovlel	%esi, %r10d
	movl	%r10d, 712(%rsp)        # 4-byte Spill
	movl	792(%rsp), %edx         # 4-byte Reload
	cmpl	%edx, %eax
	cmovll	%edx, %eax
	movl	784(%rsp), %edx         # 4-byte Reload
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	movl	$2, %esi
	subl	%edx, %esi
	cmpl	$8192, %ebp             # imm = 0x2000
	movl	$8191, %edx             # imm = 0x1FFF
	cmovll	%ebp, %edx
	movl	%ebp, %r12d
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	movl	744(%rsp), %edi         # 4-byte Reload
	cmpl	%edx, %edi
	cmovgel	%edi, %edx
	movl	%edi, %r15d
	cmpl	$8192, %r9d             # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovgel	%edi, %r9d
	testl	%r9d, %r9d
	cmovsl	%r13d, %r9d
	cmpl	%r9d, %edx
	cmovgel	%edx, %r9d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%edi, %r8d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%r8d, %ebx
	cmovgel	%ebx, %r8d
	movl	804(%rsp), %edx         # 4-byte Reload
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%edi, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %r14d
	cmovgel	%r14d, %edx
	movl	%edx, 804(%rsp)         # 4-byte Spill
	movl	%edx, %r14d
	movl	800(%rsp), %edx         # 4-byte Reload
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	incl	%edx
	notl	%ecx
	cmpl	%edx, %ecx
	cmovll	%edx, %ecx
	movl	%ecx, 728(%rsp)         # 4-byte Spill
	movl	752(%rsp), %ecx         # 4-byte Reload
	notl	%ecx
	movl	%ecx, 752(%rsp)         # 4-byte Spill
	movl	260(%rsp), %edx         # 4-byte Reload
	cmpl	%ecx, %edx
	cmovgel	%edx, %ecx
	movl	760(%rsp), %edx         # 4-byte Reload
	notl	%edx
	movl	%edx, 760(%rsp)         # 4-byte Spill
	cmpl	%edx, %ecx
	cmovll	%edx, %ecx
	movl	732(%rsp), %ebp         # 4-byte Reload
	cmpl	$8192, %ebp             # imm = 0x2000
	movl	$8191, %edx             # imm = 0x1FFF
	cmovgel	%edx, %ebp
	testl	%ebp, %ebp
	cmovsl	%r13d, %ebp
	movl	%ebp, 732(%rsp)         # 4-byte Spill
	movl	524(%rsp), %edi         # 4-byte Reload
	cmpl	%ebp, %edi
	cmovgel	%edi, %ebp
	movl	740(%rsp), %edi         # 4-byte Reload
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%edx, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	movl	%edi, 740(%rsp)         # 4-byte Spill
	cmpl	%edi, %ebp
	cmovll	%edi, %ebp
	movl	%ebp, 768(%rsp)         # 4-byte Spill
	movl	736(%rsp), %edi         # 4-byte Reload
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%edx, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	movl	%edi, 736(%rsp)         # 4-byte Spill
	cmpl	%edi, %r12d
	movl	%edi, %ebx
	cmovlel	%r12d, %ebx
	movl	%r15d, %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%edx, %edi
	movl	$8191, %r11d            # imm = 0x1FFF
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	movl	%edi, 744(%rsp)         # 4-byte Spill
	cmpl	%edi, %ebx
	cmovgl	%edi, %ebx
	movl	%ebx, 708(%rsp)         # 4-byte Spill
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	notl	%ecx
	movl	%ecx, 720(%rsp)         # 4-byte Spill
	leal	1(%r10), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r11d, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %r10d
	cmovlel	%r10d, %edx
	leal	-1(%r10), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r11d, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%edi, %edx
	cmovlel	%edx, %edi
	movl	%edi, 784(%rsp)         # 4-byte Spill
	cmpl	%esi, %eax
	cmovgel	%eax, %esi
	movl	%esi, 320(%rsp)         # 4-byte Spill
	leal	1(%rbx), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r11d, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %ebx
	cmovlel	%ebx, %edx
	leal	-1(%rbx), %ebx
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%r11d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%ebx, %edx
	cmovlel	%edx, %ebx
	leal	1(%r9), %ecx
	movl	%ecx, 704(%rsp)         # 4-byte Spill
	cmpl	$8192, %ecx             # imm = 0x2000
	movl	$8191, %edx             # imm = 0x1FFF
	cmovll	%ecx, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %r9d
	cmovgel	%r9d, %edx
	decl	%r9d
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%r11d, %r9d
	testl	%r9d, %r9d
	cmovsl	%r13d, %r9d
	cmpl	%r9d, %edx
	cmovgel	%edx, %r9d
	leal	1(%r8), %ecx
	movl	%ecx, 700(%rsp)         # 4-byte Spill
	cmpl	$8192, %ecx             # imm = 0x2000
	movl	$8191, %edx             # imm = 0x1FFF
	cmovll	%ecx, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %r8d
	cmovgel	%r8d, %edx
	decl	%r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%r11d, %r8d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%r8d, %edx
	cmovgel	%edx, %r8d
	leal	1(%rbp), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r11d, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %ebp
	cmovgel	%ebp, %edx
	leal	-1(%rbp), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%r11d, %ecx
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	cmpl	%ecx, %edx
	cmovgel	%edx, %ecx
	leal	1(%r14), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r11d, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %r14d
	cmovgel	%r14d, %edx
	leal	-1(%r14), %ebp
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%r11d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r13d, %ebp
	leal	1(%rdi), %esi
	cmpl	%ebp, %edx
	cmovgel	%edx, %ebp
	movl	%ebp, 800(%rsp)         # 4-byte Spill
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%r11d, %esi
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	leal	-1(%rdi), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r11d, %edx
	cmpl	%esi, %edi
	cmovlel	%edi, %esi
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	leal	1(%rbx), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r11d, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%edi, %ebx
	cmovlel	%ebx, %edi
	cmpl	%edx, %esi
	cmovlel	%esi, %edx
	leal	-1(%rbx), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%r11d, %esi
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	cmpl	%esi, %edi
	cmovlel	%edi, %esi
	leal	1(%r9), %r12d
	cmpl	$8192, %r12d            # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovll	%r12d, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%edi, %r9d
	cmovgel	%r9d, %edi
	decl	%r9d
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%r11d, %r9d
	testl	%r9d, %r9d
	cmovsl	%r13d, %r9d
	cmpl	%r9d, %edi
	cmovgel	%edi, %r9d
	xorl	%eax, %eax
	leal	1(%r8), %r13d
	cmpl	$8192, %r13d            # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovll	%r13d, %edi
	testl	%edi, %edi
	cmovsl	%eax, %edi
	cmpl	%edi, %r8d
	cmovgel	%r8d, %edi
	decl	%r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%r11d, %r8d
	testl	%r8d, %r8d
	cmovsl	%eax, %r8d
	cmpl	%r8d, %edi
	cmovgel	%edi, %r8d
	leal	1(%rbp), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%r11d, %edi
	incl	%r8d
	cmpl	%r8d, %edx
	cmovgel	%edx, %r8d
	leal	1(%rcx), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%r11d, %edx
	incl	%r9d
	testl	%edx, %edx
	cmovsl	%eax, %edx
	testl	%edi, %edi
	cmovsl	%eax, %edi
	cmpl	%r9d, %esi
	cmovlel	%esi, %r9d
	leal	-1(%rbp), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%r11d, %esi
	cmpl	%edi, %ebp
	cmovgel	%ebp, %edi
	testl	%esi, %esi
	cmovsl	%eax, %esi
	cmpl	%edx, %ecx
	cmovgel	%ecx, %edx
	leal	-1(%rcx), %r15d
	movl	%ecx, %ebp
	cmpl	$8192, %r15d            # imm = 0x2000
	cmovgel	%r11d, %r15d
	testl	%r15d, %r15d
	cmovsl	%eax, %r15d
	cmpl	%r15d, %edx
	cmovgel	%edx, %r15d
	movl	%r15d, %edx
	subl	%r9d, %edx
	andl	$-4, %edx
	cmpl	%esi, %edi
	leal	3(%r8,%rdx), %ecx
	cmovgel	%edi, %esi
	addl	$-3, %esi
	cmpl	%r15d, %ecx
	cmovlel	%ecx, %r15d
	cmpl	%esi, %r9d
	cmovlel	%r9d, %esi
	movl	%esi, 792(%rsp)         # 4-byte Spill
	movl	%r15d, %eax
	subl	%esi, %eax
	incl	%eax
	movl	%eax, 596(%rsp)         # 4-byte Spill
	movslq	%eax, %rax
	imulq	144(%rsp), %rax         # 8-byte Folded Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rsi
	jg	.LBB0_49
# BB#48:                                #   in Loop: Header=BB0_47 Depth=3
	movq	%rsi, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_50
.LBB0_49:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movq	%rsi, %r14
	movl	$.L.str16, %esi
	xorb	%al, %al
	callq	halide_printf
	movq	%r14, %rsi
.LBB0_50:                               #   in Loop: Header=BB0_47 Depth=3
	movl	784(%rsp), %eax         # 4-byte Reload
	cmpl	%r13d, %eax
	cmovgel	%eax, %r13d
	cmpl	%r12d, %ebx
	cmovlel	%ebx, %r12d
	movl	%ebp, %r14d
	movl	%r14d, %eax
	subl	%r12d, %eax
	andl	$-4, %eax
	movl	800(%rsp), %ecx         # 4-byte Reload
	addl	$-3, %ecx
	cmpl	%ecx, %r12d
	cmovlel	%r12d, %ecx
	movl	%ecx, 800(%rsp)         # 4-byte Spill
	leal	3(%r13,%rax), %eax
	cmpl	%r14d, %eax
	cmovlel	%eax, %r14d
	movl	%r14d, 776(%rsp)        # 4-byte Spill
	leal	1(%r14), %ebp
	leal	-1(%r14), %r12d
	movl	792(%rsp), %eax         # 4-byte Reload
	cmpl	%ecx, %eax
	movl	%ecx, %edx
	cmovlel	%eax, %edx
	movl	%edx, 784(%rsp)         # 4-byte Spill
	leal	1(%rcx), %ebx
	xorl	%edi, %edi
	callq	halide_malloc
	movq	%rax, 608(%rsp)         # 8-byte Spill
	cmpl	$8192, %ebp             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovgel	%eax, %ebp
	testl	%ebp, %ebp
	movl	$0, %r13d
	cmovsl	%r13d, %ebp
	cmpl	%r14d, %r15d
	cmovll	%r14d, %r15d
	cmpl	%ebp, %r15d
	cmovll	%ebp, %r15d
	cmpl	$8192, %r12d            # imm = 0x2000
	cmovgel	%eax, %r12d
	testl	%r12d, %r12d
	cmovsl	%r13d, %r12d
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%eax, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%r12d, %r15d
	cmovgel	%r15d, %r12d
	movl	784(%rsp), %ecx         # 4-byte Reload
	cmpl	%ebx, %ecx
	cmovlel	%ecx, %ebx
	movl	800(%rsp), %r14d        # 4-byte Reload
	leal	-1(%r14), %ebp
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%eax, %ebp
	testl	%ebp, %ebp
	cmovsl	%r13d, %ebp
	cmpl	%ebp, %ebx
	cmovlel	%ebx, %ebp
	subl	%ebp, %r12d
	incl	%r12d
	movl	%r12d, 592(%rsp)        # 4-byte Spill
	movslq	%r12d, %rax
	imulq	160(%rsp), %rax         # 8-byte Folded Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rbx
	jg	.LBB0_52
# BB#51:                                #   in Loop: Header=BB0_47 Depth=3
	movq	%rbx, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_53
.LBB0_52:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movl	$.L.str17, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_53:                               #   in Loop: Header=BB0_47 Depth=3
	movl	%ebp, 784(%rsp)         # 4-byte Spill
	xorl	%edi, %edi
	movq	%rbx, %rsi
	callq	halide_malloc
	movq	%rax, 600(%rsp)         # 8-byte Spill
	movl	776(%rsp), %eax         # 4-byte Reload
	subl	%r14d, %eax
	movl	%r14d, %r12d
	incl	%eax
	movl	%eax, 588(%rsp)         # 4-byte Spill
	movslq	%eax, %rax
	imulq	152(%rsp), %rax         # 8-byte Folded Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rsi
	movl	804(%rsp), %ebp         # 4-byte Reload
	jg	.LBB0_55
# BB#54:                                #   in Loop: Header=BB0_47 Depth=3
	movq	%rsi, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_56
.LBB0_55:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movq	%rsi, %rbx
	movl	$.L.str18, %esi
	xorb	%al, %al
	callq	halide_printf
	movq	%rbx, %rsi
.LBB0_56:                               #   in Loop: Header=BB0_47 Depth=3
	movl	712(%rsp), %eax         # 4-byte Reload
	movl	700(%rsp), %edx         # 4-byte Reload
	cmpl	%edx, %eax
	cmovgel	%eax, %edx
	movl	708(%rsp), %eax         # 4-byte Reload
	movl	704(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %eax
	cmovlel	%eax, %ecx
	movl	768(%rsp), %r14d        # 4-byte Reload
	movl	%r14d, %eax
	subl	%ecx, %eax
	andl	$-4, %eax
	movl	%eax, 712(%rsp)         # 4-byte Spill
	addl	$-3, %ebp
	cmpl	%ebp, %ecx
	cmovlel	%ecx, %ebp
	movl	%ebp, 804(%rsp)         # 4-byte Spill
	leal	3(%rdx,%rax), %eax
	cmpl	%r14d, %eax
	cmovlel	%eax, %r14d
	movl	%r14d, 768(%rsp)        # 4-byte Spill
	leal	1(%r14), %ebx
	leal	-1(%r14), %r15d
	cmpl	%ebp, %r12d
	movl	%r12d, %eax
	movl	804(%rsp), %r12d        # 4-byte Reload
	cmovlel	%eax, %r12d
	movl	804(%rsp), %eax         # 4-byte Reload
	leal	1(%rax), %ebp
	xorl	%edi, %edi
	callq	halide_malloc
	movq	%rax, 680(%rsp)         # 8-byte Spill
	cmpl	$8192, %ebx             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovgel	%eax, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	movl	776(%rsp), %ecx         # 4-byte Reload
	cmpl	%r14d, %ecx
	cmovll	%r14d, %ecx
	cmpl	%ebx, %ecx
	cmovll	%ebx, %ecx
	cmpl	$8192, %r15d            # imm = 0x2000
	cmovgel	%eax, %r15d
	testl	%r15d, %r15d
	cmovsl	%r13d, %r15d
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%eax, %ebp
	testl	%ebp, %ebp
	cmovsl	%r13d, %ebp
	cmpl	%r15d, %ecx
	cmovgel	%ecx, %r15d
	cmpl	%ebp, %r12d
	cmovlel	%r12d, %ebp
	movl	804(%rsp), %r14d        # 4-byte Reload
	leal	-1(%r14), %ebx
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%eax, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%ebx, %ebp
	cmovlel	%ebp, %ebx
	subl	%ebx, %r15d
	incl	%r15d
	movl	%r15d, 584(%rsp)        # 4-byte Spill
	movslq	%r15d, %rax
	imulq	168(%rsp), %rax         # 8-byte Folded Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rbp
	jg	.LBB0_58
# BB#57:                                #   in Loop: Header=BB0_47 Depth=3
	movq	%rbp, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_59
.LBB0_58:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movl	$.L.str19, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_59:                               #   in Loop: Header=BB0_47 Depth=3
	movl	%ebx, 776(%rsp)         # 4-byte Spill
	xorl	%edi, %edi
	movq	%rbp, %rsi
	callq	halide_malloc
	movq	%rax, 624(%rsp)         # 8-byte Spill
	movl	768(%rsp), %eax         # 4-byte Reload
	subl	%r14d, %eax
	incl	%eax
	movl	%eax, 580(%rsp)         # 4-byte Spill
	movslq	%eax, %rax
	imulq	176(%rsp), %rax         # 8-byte Folded Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %r15
	jg	.LBB0_61
# BB#60:                                #   in Loop: Header=BB0_47 Depth=3
	movq	%r15, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_62
.LBB0_61:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movl	$.L.str20, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_62:                               #   in Loop: Header=BB0_47 Depth=3
	movl	524(%rsp), %ebp         # 4-byte Reload
	movl	768(%rsp), %eax         # 4-byte Reload
	cmpl	%ebp, %eax
	cmovgel	%eax, %ebp
	movl	732(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebp
	cmovll	%eax, %ebp
	movl	360(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r14d
	movl	%eax, %ebx
	cmovlel	%r14d, %ebx
	movl	736(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	cmovgl	%eax, %ebx
	movl	744(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	cmovgl	%eax, %ebx
	movl	740(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebp
	cmovll	%eax, %ebp
	xorl	%edi, %edi
	movq	%r15, %rsi
	callq	halide_malloc
	movq	%rax, 616(%rsp)         # 8-byte Spill
	subl	%ebx, %ebp
	incl	%ebp
	movl	%ebp, 524(%rsp)         # 4-byte Spill
	movslq	%ebp, %rax
	imulq	184(%rsp), %rax         # 8-byte Folded Reload
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rbp
	jg	.LBB0_64
# BB#63:                                #   in Loop: Header=BB0_47 Depth=3
	movq	%rbp, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_65
.LBB0_64:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movl	$.L.str21, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_65:                               #   in Loop: Header=BB0_47 Depth=3
	movl	%ebx, 704(%rsp)         # 4-byte Spill
	xorl	%edi, %edi
	movq	%rbp, %rsi
	callq	halide_malloc
	movq	%rax, 672(%rsp)         # 8-byte Spill
	cmpb	$0, 199(%rsp)           # 1-byte Folded Reload
	je	.LBB0_67
# BB#66:                                #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movl	$.L.str22, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_67:                               #   in Loop: Header=BB0_47 Depth=3
	movl	$-4, %ebx
	subl	712(%rsp), %ebx         # 4-byte Folded Reload
	subl	728(%rsp), %ebx         # 4-byte Folded Reload
	movl	720(%rsp), %eax         # 4-byte Reload
	cmpl	%ebx, %eax
	cmovgel	%eax, %ebx
	xorl	%r12d, %r12d
	movl	128(%rsp), %eax         # 4-byte Reload
	movl	320(%rsp), %ebp         # 4-byte Reload
	leal	(%rax,%rbp), %eax
	movl	%eax, 308(%rsp)         # 4-byte Spill
	notl	%ebx
	xorl	%edi, %edi
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	halide_malloc
	movq	%rax, 432(%rsp)         # 8-byte Spill
	movl	136(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rbp), %eax
	movl	%eax, 304(%rsp)         # 4-byte Spill
	movl	212(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rbp), %eax
	movl	%eax, 300(%rsp)         # 4-byte Spill
	movl	132(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rbp), %eax
	movl	%eax, 296(%rsp)         # 4-byte Spill
	movl	260(%rsp), %eax         # 4-byte Reload
	cmpl	%ebx, %eax
	cmovgel	%eax, %ebx
	movl	752(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	cmovll	%eax, %ebx
	movl	760(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	cmovll	%eax, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	leal	2(%rbp,%rbx), %eax
	movl	%eax, 544(%rsp)         # 4-byte Spill
	movl	$8191, %ebp             # imm = 0x1FFF
	vmovss	.LCPI0_0(%rip), %xmm3
	vmovss	.LCPI0_1(%rip), %xmm4
.LBB0_68:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_71 Depth 6
                                        #               Child Loop BB0_73 Depth 7
                                        #             Child Loop BB0_77 Depth 6
                                        #               Child Loop BB0_80 Depth 7
                                        #             Child Loop BB0_85 Depth 6
                                        #               Child Loop BB0_87 Depth 7
                                        #             Child Loop BB0_91 Depth 6
                                        #               Child Loop BB0_94 Depth 7
                                        #             Child Loop BB0_99 Depth 6
                                        #               Child Loop BB0_101 Depth 7
                                        #             Child Loop BB0_105 Depth 6
                                        #               Child Loop BB0_108 Depth 7
                                        #             Child Loop BB0_113 Depth 6
	movq	%r12, 344(%rsp)         # 8-byte Spill
	movl	292(%rsp), %ebx         # 4-byte Reload
	leal	(%rbx,%r12), %r14d
	cmpl	$8192, %r14d            # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovll	%r14d, %edi
	leal	1(%rbx,%r12), %r11d
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	$8192, %r11d            # imm = 0x2000
	cmovgel	%ebp, %r11d
	leal	-1(%rbx,%r12), %edx
	testl	%r11d, %r11d
	cmovsl	%r13d, %r11d
	movl	%r14d, %eax
	movl	284(%rsp), %r10d        # 4-byte Reload
	subl	%r10d, %eax
	movl	%r14d, %esi
	subl	276(%rsp), %esi         # 4-byte Folded Reload
	cmpl	%edi, %edx
	cmovgel	%edx, %edi
	leal	-2(%rbx,%r12), %ebx
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%ebp, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%ebx, %edi
	cmovgel	%edi, %ebx
	shll	$5, %eax
	movl	%eax, 768(%rsp)         # 4-byte Spill
	cmpl	%r11d, %r14d
	movl	%r11d, %r15d
	cmovlel	%r14d, %r15d
	cmovll	%r11d, %r14d
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%ebp, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %r14d
	cmovll	%edx, %r14d
	cmpl	%edx, %r15d
	cmovgl	%edx, %r15d
	subl	%r10d, %edx
	movq	264(%rsp), %rdi         # 8-byte Reload
                                        # kill: EDI<def> EDI<kill> RDI<kill>
	imull	%esi, %edi
	movl	%edi, 396(%rsp)         # 4-byte Spill
	leal	1(%rbx), %eax
	movl	%eax, 752(%rsp)         # 4-byte Spill
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovll	%eax, %edi
	leal	1(%r15), %esi
	leal	-1(%r15), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%ebp, %ecx
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%ebp, %esi
	leal	-1(%r14), %eax
	movl	%eax, 760(%rsp)         # 4-byte Spill
	leal	1(%r14), %eax
	movl	%eax, 636(%rsp)         # 4-byte Spill
	subl	%r10d, %r11d
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%edi, %ebx
	cmovgel	%ebx, %edi
	decl	%ebx
	cmpl	$8192, %ebx             # imm = 0x2000
	cmovgel	%ebp, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%ebx, %edi
	cmovgel	%edi, %ebx
	leal	1(%rbx), %r10d
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	cmpl	%esi, %r15d
	cmovlel	%r15d, %esi
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	cmpl	$8192, %r10d            # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovll	%r10d, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	leal	-1(%rbx), %r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%ebp, %r8d
	cmpl	%edi, %ebx
	cmovgel	%ebx, %edi
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%ecx, %esi
	cmovlel	%esi, %ecx
	leal	1(%rcx), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%ebp, %esi
	leal	-1(%rcx), %r9d
	cmpl	%r8d, %edi
	cmovgel	%edi, %r8d
	leal	1(%r8), %ebx
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	cmpl	%esi, %ecx
	cmovlel	%ecx, %esi
	cmpl	$8192, %r9d             # imm = 0x2000
	cmovgel	%ebp, %r9d
	testl	%r9d, %r9d
	cmovsl	%r13d, %r9d
	cmpl	$8192, %ebx             # imm = 0x2000
	movl	$8191, %edi             # imm = 0x1FFF
	cmovll	%ebx, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%r9d, %esi
	cmovlel	%esi, %r9d
	leal	1(%r9), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%ebp, %esi
	cmpl	%edi, %r8d
	cmovgel	%r8d, %edi
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	decl	%r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%ebp, %r8d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%r8d, %edi
	cmovgel	%edi, %r8d
	cmpl	%esi, %r9d
	cmovlel	%r9d, %esi
	leal	-1(%r9), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%ebp, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%edi, %esi
	cmovlel	%esi, %edi
	incl	%r8d
	testl	%r12d, %r12d
	movslq	768(%rsp), %rsi         # 4-byte Folded Reload
	movq	%rsi, 384(%rsp)         # 8-byte Spill
	cmovnel	752(%rsp), %r15d        # 4-byte Folded Reload
	movl	%r15d, 492(%rsp)        # 4-byte Spill
	cmovnel	%r10d, %ecx
	movl	%ecx, 488(%rsp)         # 4-byte Spill
	cmovnel	%ebx, %r9d
	movl	%r9d, 484(%rsp)         # 4-byte Spill
	cmovel	%edi, %r8d
	movl	%r8d, 448(%rsp)         # 4-byte Spill
	shll	$5, %r11d
	movslq	%r11d, %rcx
	movq	%rcx, 376(%rsp)         # 8-byte Spill
	shll	$5, %edx
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %ecx             # imm = 0x1FFF
	cmovll	%eax, %ecx
	movl	760(%rsp), %eax         # 4-byte Reload
	cmpl	$8192, %eax             # imm = 0x2000
	movslq	%edx, %rdx
	movq	%rdx, 368(%rsp)         # 8-byte Spill
	cmovgel	%ebp, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	movl	280(%rsp), %edx         # 4-byte Reload
	leal	(%rdx,%r15), %esi
	movl	288(%rsp), %edx         # 4-byte Reload
	leal	(%rdx,%r15), %edx
	cmpl	%ecx, %r14d
	cmovgel	%r14d, %ecx
	shll	$5, %edx
	imull	544(%rsp), %esi         # 4-byte Folded Reload
	cmpl	%eax, %ecx
	cmovll	%eax, %ecx
	leal	1(%rcx), %edi
	movl	%edi, 644(%rsp)         # 4-byte Spill
	movl	300(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rsi), %eax
	movl	%eax, 536(%rsp)         # 4-byte Spill
	movl	304(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rsi), %eax
	movl	%eax, 532(%rsp)         # 4-byte Spill
	movl	320(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rsi), %eax
	movl	%eax, 364(%rsp)         # 4-byte Spill
	movl	308(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rsi), %eax
	movl	%eax, 528(%rsp)         # 4-byte Spill
	addl	296(%rsp), %esi         # 4-byte Folded Reload
	movl	%esi, 540(%rsp)         # 4-byte Spill
	addl	$35, %edx
	movl	%edx, 444(%rsp)         # 4-byte Spill
	cmpl	$8192, %edi             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovll	%edi, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%eax, %ecx
	cmovgel	%ecx, %eax
	leal	-1(%rcx), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%ebp, %ecx
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	cmpl	%ecx, %eax
	cmovgel	%eax, %ecx
	leal	1(%rcx), %edx
	movl	%edx, 640(%rsp)         # 4-byte Spill
	cmpl	$8192, %edx             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovll	%edx, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%eax, %ecx
	cmovgel	%ecx, %eax
	decl	%ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%ebp, %ecx
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	cmpl	%ecx, %eax
	cmovgel	%eax, %ecx
	incl	%ecx
	movl	%ecx, 552(%rsp)         # 4-byte Spill
	movl	316(%rsp), %eax         # 4-byte Reload
	movl	%eax, 440(%rsp)         # 4-byte Spill
	movl	332(%rsp), %r12d        # 4-byte Reload
	movl	340(%rsp), %r11d        # 4-byte Reload
	movl	336(%rsp), %ebx         # 4-byte Reload
	movl	328(%rsp), %r10d        # 4-byte Reload
	movl	312(%rsp), %r9d         # 4-byte Reload
	movl	324(%rsp), %edx         # 4-byte Reload
	xorl	%eax, %eax
.LBB0_69:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB0_71 Depth 6
                                        #               Child Loop BB0_73 Depth 7
                                        #             Child Loop BB0_77 Depth 6
                                        #               Child Loop BB0_80 Depth 7
                                        #             Child Loop BB0_85 Depth 6
                                        #               Child Loop BB0_87 Depth 7
                                        #             Child Loop BB0_91 Depth 6
                                        #               Child Loop BB0_94 Depth 7
                                        #             Child Loop BB0_99 Depth 6
                                        #               Child Loop BB0_101 Depth 7
                                        #             Child Loop BB0_105 Depth 6
                                        #               Child Loop BB0_108 Depth 7
                                        #             Child Loop BB0_113 Depth 6
	movq	%rax, 456(%rsp)         # 8-byte Spill
	movl	%edx, 468(%rsp)         # 4-byte Spill
	movl	%r9d, 412(%rsp)         # 4-byte Spill
	movl	%r10d, 416(%rsp)        # 4-byte Spill
	movl	%ebx, 420(%rsp)         # 4-byte Spill
	movl	%r11d, 424(%rsp)        # 4-byte Spill
	movl	%r12d, 428(%rsp)        # 4-byte Spill
	leaq	(,%rax,4), %rdi
	movq	%rdi, 400(%rsp)         # 8-byte Spill
	movl	360(%rsp), %esi         # 4-byte Reload
	leal	(%rsi,%rdi), %eax
	movl	%eax, 452(%rsp)         # 4-byte Spill
	leal	4(%rsi,%rdi), %eax
	leal	-2(%rsi,%rdi), %r8d
	leal	-1(%rsi,%rdi), %r15d
	leal	1(%rsi,%rdi), %ecx
	leal	3(%rsi,%rdi), %r14d
	movl	%r14d, 768(%rsp)        # 4-byte Spill
	leal	2(%rsi,%rdi), %edi
	cmpl	$-8193, %r9d            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %esi            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r9d, %esi
	movl	%esi, 700(%rsp)         # 4-byte Spill
	cmpl	$-8193, %edx            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r9d            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%edx, %r9d
	cmpl	$-8193, %r12d           # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %edx            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r12d, %edx
	movl	%edx, 696(%rsp)         # 4-byte Spill
	cmpl	$-8193, %r11d           # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %edx            # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r11d, %edx
	movl	%edx, 692(%rsp)         # 4-byte Spill
	cmpl	$-8193, %ebx            # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r11d           # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%ebx, %r11d
	cmpl	$-8193, %r10d           # imm = 0xFFFFFFFFFFFFDFFF
	movl	$-8192, %r12d           # imm = 0xFFFFFFFFFFFFE000
	cmovgl	%r10d, %r12d
	movl	452(%rsp), %edx         # 4-byte Reload
	cmpl	$8192, %edx             # imm = 0x2000
	movl	$8191, %esi             # imm = 0x1FFF
	cmovll	%edx, %esi
	movl	%edx, %ebx
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	cmpl	%esi, %r15d
	cmovgel	%r15d, %esi
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%ebp, %r8d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%r8d, %esi
	cmovgel	%esi, %r8d
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%ebp, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	movl	768(%rsp), %edx         # 4-byte Reload
	cmpl	%eax, %edx
	cmovgel	%edx, %eax
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%ebp, %ecx
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	cmpl	%ecx, %ebx
	cmovlel	%ebx, %ecx
	cmpl	$8192, %r15d            # imm = 0x2000
	cmovgel	%ebp, %r15d
	testl	%r15d, %r15d
	cmovsl	%r13d, %r15d
	cmpl	%r15d, %ecx
	cmovlel	%ecx, %r15d
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%ebp, %edi
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	cmpl	%edi, %eax
	cmovgel	%eax, %edi
	movl	%edi, 712(%rsp)         # 4-byte Spill
	leal	1(%r8), %ecx
	movl	%ecx, 768(%rsp)         # 4-byte Spill
	cmpl	$8192, %ecx             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovll	%ecx, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%eax, %r8d
	cmovgel	%r8d, %eax
	decl	%r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%ebp, %r8d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%r8d, %eax
	cmovgel	%eax, %r8d
	leal	1(%r15), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%ebp, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%eax, %r15d
	cmovlel	%r15d, %eax
	leal	-1(%r15), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%ebp, %ecx
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	cmpl	%ecx, %eax
	cmovlel	%eax, %ecx
	leal	1(%rdi), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%ebp, %eax
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%eax, %edi
	cmovgel	%edi, %eax
	leal	-1(%rdi), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%ebp, %edx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	cmpl	%edx, %eax
	cmovgel	%eax, %edx
	movl	%edx, 708(%rsp)         # 4-byte Spill
	leal	1(%r8), %r10d
	cmpl	$8192, %r10d            # imm = 0x2000
	movl	$8191, %ebx             # imm = 0x1FFF
	cmovll	%r10d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r13d, %ebx
	cmpl	%ebx, %r8d
	cmovgel	%r8d, %ebx
	decl	%r8d
	leal	1(%rcx), %esi
	cmpl	$8192, %esi             # imm = 0x2000
	cmovgel	%ebp, %esi
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%ebp, %r8d
	leal	-1(%rcx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%ebp, %eax
	leal	-1(%rdx), %edi
	cmpl	$8192, %edi             # imm = 0x2000
	cmovgel	%ebp, %edi
	leal	1(%rdx), %r14d
	cmpl	$8192, %r14d            # imm = 0x2000
	cmovgel	%ebp, %r14d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	cmpl	%r8d, %ebx
	cmovgel	%ebx, %r8d
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	cmpl	%esi, %ecx
	cmovlel	%ecx, %esi
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	cmpl	%eax, %esi
	cmovlel	%esi, %eax
	movl	%r9d, %esi
	xorl	$-1, %esi
	notl	%r9d
	cmovsl	%r13d, %r9d
	movl	%r9d, 668(%rsp)         # 4-byte Spill
	movl	700(%rsp), %ebx         # 4-byte Reload
	movl	%ebx, %esi
	xorl	$-1, %esi
	notl	%ebx
	cmovsl	%r13d, %ebx
	movl	%ebx, 700(%rsp)         # 4-byte Spill
	movl	%r12d, %esi
	xorl	$-1, %esi
	notl	%r12d
	cmovsl	%r13d, %r12d
	movl	%r12d, 660(%rsp)        # 4-byte Spill
	movl	%r11d, %esi
	xorl	$-1, %esi
	notl	%r11d
	cmovsl	%r13d, %r11d
	movl	%r11d, 664(%rsp)        # 4-byte Spill
	movl	692(%rsp), %ebx         # 4-byte Reload
	movl	%ebx, %esi
	xorl	$-1, %esi
	notl	%ebx
	cmovsl	%r13d, %ebx
	movl	%ebx, 692(%rsp)         # 4-byte Spill
	movl	696(%rsp), %ebx         # 4-byte Reload
	movl	%ebx, %esi
	xorl	$-1, %esi
	notl	%ebx
	cmovsl	%r13d, %ebx
	movl	%ebx, 696(%rsp)         # 4-byte Spill
	testl	%r14d, %r14d
	cmovsl	%r13d, %r14d
	testl	%edi, %edi
	cmovsl	%r13d, %edi
	incl	%r8d
	cmpl	%r14d, %edx
	cmovgel	%edx, %r14d
	movq	456(%rsp), %rdx         # 8-byte Reload
	testl	%edx, %edx
	cmovnel	768(%rsp), %r15d        # 4-byte Folded Reload
	movl	%r15d, 656(%rsp)        # 4-byte Spill
	cmovnel	%r10d, %ecx
	movl	%ecx, 652(%rsp)         # 4-byte Spill
	cmovel	%eax, %r8d
	movl	%r8d, 648(%rsp)         # 4-byte Spill
	cmpl	%edi, %r14d
	cmovll	%edi, %r14d
	movl	%r14d, 768(%rsp)        # 4-byte Spill
	movl	552(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 448(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_75
# BB#70:                                # %.lr.ph1210
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	768(%rsp), %eax         # 4-byte Reload
	leal	4(%rax), %ecx
	subl	648(%rsp), %ecx         # 4-byte Folded Reload
	sarl	$2, %ecx
	movl	%ecx, 728(%rsp)         # 4-byte Spill
	leal	-3(%rax), %eax
	movl	%eax, 760(%rsp)         # 4-byte Spill
	movl	448(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, 732(%rsp)         # 4-byte Spill
.LBB0_71:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_73 Depth 7
	cmpl	$0, 728(%rsp)           # 4-byte Folded Reload
	movq	496(%rsp), %r15         # 8-byte Reload
	movl	792(%rsp), %edi         # 4-byte Reload
	movq	608(%rsp), %r14         # 8-byte Reload
	movl	%eax, %edx
	jle	.LBB0_74
# BB#72:                                # %.lr.ph
                                        #   in Loop: Header=BB0_71 Depth=6
	movl	732(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, %eax
	subl	560(%rsp), %eax         # 4-byte Folded Reload
	imull	596(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 736(%rsp)         # 4-byte Spill
	movq	504(%rsp), %rax         # 8-byte Reload
	movl	%eax, %r11d
	imull	%ecx, %r11d
	subl	516(%rsp), %r11d        # 4-byte Folded Reload
	movl	648(%rsp), %r10d        # 4-byte Reload
	movl	728(%rsp), %ebx         # 4-byte Reload
.LBB0_73:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        #             Parent Loop BB0_71 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	cmpl	%edx, %r10d
	movl	760(%rsp), %r9d         # 4-byte Reload
	cmovlel	%r10d, %r9d
	leal	-1(%r9), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%ebp, %edx
	leal	1(%r9), %r12d
	leal	3(%r9), %eax
	leal	2(%r9), %ecx
	leal	4(%r9), %r8d
	cmpl	$8192, %r8d             # imm = 0x2000
	cmovgel	%ebp, %r8d
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%ebp, %ecx
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%ebp, %eax
	cmpl	$8192, %r9d             # imm = 0x2000
	movl	$8191, %esi             # imm = 0x1FFF
	cmovll	%r9d, %esi
	cmpl	$8192, %r12d            # imm = 0x2000
	cmovgel	%ebp, %r12d
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	addl	%r11d, %edx
	movslq	%edx, %rbp
	movl	%r9d, %edx
	subl	%edi, %edx
	testl	%r12d, %r12d
	vmovss	(%r15,%rbp,4), %xmm0
	cmovsl	%r13d, %r12d
	leal	(%r11,%r9), %ebp
	testl	%esi, %esi
	movslq	%ebp, %rbp
	cmovsl	%r13d, %esi
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	vmovss	(%r15,%rbp,4), %xmm1
	movl	736(%rsp), %ebp         # 4-byte Reload
	movl	%ebx, %edi
	leal	2(%rdx,%rbp), %ebx
	movl	%ebx, 752(%rsp)         # 4-byte Spill
	movl	%edi, %ebx
	leal	3(%rdx,%rbp), %edi
	movl	%edi, 740(%rsp)         # 4-byte Spill
	leal	1(%rdx,%rbp), %edi
	movl	%edi, 744(%rsp)         # 4-byte Spill
	leal	(%rdx,%rbp), %edx
	movslq	%edx, %rdx
	addl	%r11d, %r12d
	movslq	%r12d, %r12
	vmulss	(%r15,%r12,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r14,%rdx,4)
	addl	%r11d, %r8d
	addl	%r11d, %ecx
	addl	%r11d, %eax
	movq	%r14, %r13
	leal	1(%r11,%r9), %r14d
	leal	2(%r11,%r9), %ebp
	leal	3(%r11,%r9), %edx
	addl	%r11d, %esi
	movslq	%esi, %rsi
	vmovss	(%r15,%rsi,4), %xmm0
	addl	$4, %r10d
	movslq	%r14d, %rsi
	movq	%r13, %r14
	vmovss	(%r15,%rsi,4), %xmm1
	decl	%ebx
	movslq	740(%rsp), %r9          # 4-byte Folded Reload
	movslq	%edx, %rdi
	movslq	%eax, %rdx
	movslq	%ecx, %rax
	movslq	%r8d, %r8
	movslq	752(%rsp), %rsi         # 4-byte Folded Reload
	movslq	%ebp, %rbp
	movslq	744(%rsp), %rcx         # 4-byte Folded Reload
	movl	$0, %r13d
	vmulss	(%r15,%rax,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r14,%rcx,4)
	vmulss	(%r15,%rdx,4), %xmm4, %xmm1
	vmovss	(%r15,%rbp,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r15,%r12,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r14,%rsi,4)
	vmulss	(%r15,%r8,4), %xmm4, %xmm1
	vmovss	(%r15,%rdi,4), %xmm0
	movl	792(%rsp), %edi         # 4-byte Reload
	movl	$8191, %ebp             # imm = 0x1FFF
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r15,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r14,%r9,4)
	movl	760(%rsp), %edx         # 4-byte Reload
	jne	.LBB0_73
.LBB0_74:                               # %._crit_edge
                                        #   in Loop: Header=BB0_71 Depth=6
	movl	732(%rsp), %eax         # 4-byte Reload
	incl	%eax
	movl	%eax, 732(%rsp)         # 4-byte Spill
	cmpl	552(%rsp), %eax         # 4-byte Folded Reload
	movl	%edx, %eax
	jl	.LBB0_71
.LBB0_75:                               # %.loopexit
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	640(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 484(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_89
# BB#76:                                # %.lr.ph1217
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	768(%rsp), %edi         # 4-byte Reload
	leal	4(%rdi), %edx
	subl	648(%rsp), %edx         # 4-byte Folded Reload
	sarl	$2, %edx
	movl	%edx, 548(%rsp)         # 4-byte Spill
	addl	$-3, %edi
	movl	%edi, 768(%rsp)         # 4-byte Spill
	movl	484(%rsp), %eax         # 4-byte Reload
	movl	$8191, %esi             # imm = 0x1FFF
	movl	700(%rsp), %ecx         # 4-byte Reload
.LBB0_77:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_80 Depth 7
	testl	%edx, %edx
	jle	.LBB0_78
# BB#79:                                # %.lr.ph1213
                                        #   in Loop: Header=BB0_77 Depth=6
	leal	1(%rax), %ecx
	movl	%ecx, 556(%rsp)         # 4-byte Spill
	cmpl	$8192, %ecx             # imm = 0x2000
	movl	$8191, %r12d            # imm = 0x1FFF
	cmovll	%ecx, %r12d
	testl	%r12d, %r12d
	cmovsl	%r13d, %r12d
	leal	-1(%rax), %ebp
	movl	560(%rsp), %ecx         # 4-byte Reload
	subl	%ecx, %r12d
	cmpl	$8192, %ebp             # imm = 0x2000
	cmovgel	%esi, %ebp
	testl	%ebp, %ebp
	cmovsl	%r13d, %ebp
	subl	%ecx, %ebp
	movl	%eax, %esi
	subl	%ecx, %esi
	movl	596(%rsp), %ecx         # 4-byte Reload
	imull	%ecx, %ebp
	movl	%ebp, 728(%rsp)         # 4-byte Spill
	imull	%ecx, %r12d
	imull	%ecx, %esi
	movl	%esi, 720(%rsp)         # 4-byte Spill
	subl	572(%rsp), %eax         # 4-byte Folded Reload
	imull	592(%rsp), %eax         # 4-byte Folded Reload
	movl	648(%rsp), %r9d         # 4-byte Reload
	movl	%edx, %r15d
	movq	608(%rsp), %r10         # 8-byte Reload
	movq	600(%rsp), %r14         # 8-byte Reload
.LBB0_80:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        #             Parent Loop BB0_77 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	cmpl	%edi, %r9d
	movl	768(%rsp), %edi         # 4-byte Reload
	cmovlel	%r9d, %edi
	movl	%edi, %esi
	subl	792(%rsp), %esi         # 4-byte Folded Reload
	movl	728(%rsp), %ebx         # 4-byte Reload
	leal	(%rsi,%rbx), %edx
	leal	(%rsi,%r12), %ebp
	movslq	%ebp, %r8
	movslq	%edx, %rbp
	subl	784(%rsp), %edi         # 4-byte Folded Reload
	leal	(%rdi,%rax), %edx
	movslq	%edx, %rdx
	vmovss	(%r10,%rbp,4), %xmm0
	vmulss	(%r10,%r8,4), %xmm4, %xmm1
	movl	720(%rsp), %ecx         # 4-byte Reload
	leal	(%rsi,%rcx), %ebp
	movslq	%ebp, %rbp
	vmovss	(%r10,%rbp,4), %xmm2
	vfmadd213ss	%xmm1, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm0
	vmovss	%xmm0, (%r14,%rdx,4)
	leal	1(%rsi,%r12), %edx
	movslq	%edx, %rdx
	leal	2(%rdi,%rax), %ebp
	movl	%ebp, 740(%rsp)         # 4-byte Spill
	leal	3(%rsi,%rbx), %ebp
	movl	%ebp, 752(%rsp)         # 4-byte Spill
	vmulss	(%r10,%rdx,4), %xmm4, %xmm0
	leal	3(%rdi,%rax), %r13d
	leal	1(%rdi,%rax), %edx
	movl	%edx, 736(%rsp)         # 4-byte Spill
	leal	1(%rsi,%rbx), %ebp
	leal	2(%rsi,%rbx), %edx
	movl	%edx, 732(%rsp)         # 4-byte Spill
	leal	3(%rsi,%rcx), %edi
	leal	1(%rsi,%rcx), %edx
	leal	2(%rsi,%rcx), %ecx
	leal	3(%rsi,%r12), %ebx
	leal	2(%rsi,%r12), %r8d
	movslq	%ebp, %rsi
	vmovss	(%r10,%rsi,4), %xmm1
	addl	$4, %r9d
	movslq	%edx, %r11
	movslq	%r13d, %rdx
	movq	%rdx, 760(%rsp)         # 8-byte Spill
	movslq	752(%rsp), %rdx         # 4-byte Folded Reload
	movq	%rdx, 752(%rsp)         # 8-byte Spill
	movslq	%edi, %rdx
	movq	%rdx, 744(%rsp)         # 8-byte Spill
	movslq	%ebx, %r13
	movslq	740(%rsp), %rdx         # 4-byte Folded Reload
	movslq	732(%rsp), %rsi         # 4-byte Folded Reload
	movslq	%ecx, %rbp
	movslq	%r8d, %rdi
	movslq	736(%rsp), %rbx         # 4-byte Folded Reload
	vmovss	(%r10,%r11,4), %xmm2
	decl	%r15d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%r14,%rbx,4)
	vmulss	(%r10,%rdi,4), %xmm4, %xmm1
	movl	768(%rsp), %edi         # 4-byte Reload
	vmovss	(%r10,%rbp,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r10,%rsi,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r14,%rdx,4)
	vmulss	(%r10,%r13,4), %xmm4, %xmm1
	movq	744(%rsp), %rcx         # 8-byte Reload
	vmovss	(%r10,%rcx,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	752(%rsp), %rcx         # 8-byte Reload
	vmovss	(%r10,%rcx,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	760(%rsp), %rcx         # 8-byte Reload
	vmovss	%xmm1, (%r14,%rcx,4)
	jne	.LBB0_80
# BB#81:                                #   in Loop: Header=BB0_77 Depth=6
	movl	556(%rsp), %eax         # 4-byte Reload
	xorl	%r13d, %r13d
	movl	$8191, %esi             # imm = 0x1FFF
	movl	700(%rsp), %ecx         # 4-byte Reload
	movl	548(%rsp), %edx         # 4-byte Reload
	jmp	.LBB0_82
.LBB0_78:                               # %._crit_edge1278
                                        #   in Loop: Header=BB0_77 Depth=6
	incl	%eax
.LBB0_82:                               # %._crit_edge1214
                                        #   in Loop: Header=BB0_77 Depth=6
	cmpl	640(%rsp), %eax         # 4-byte Folded Reload
	jl	.LBB0_77
# BB#83:                                # %.loopexit1215
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	%ecx, 700(%rsp)         # 4-byte Spill
	movl	640(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 484(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_89
# BB#84:                                # %.lr.ph1224
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	708(%rsp), %eax         # 4-byte Reload
	leal	4(%rax), %ecx
	subl	652(%rsp), %ecx         # 4-byte Folded Reload
	sarl	$2, %ecx
	movl	%ecx, 720(%rsp)         # 4-byte Spill
	leal	-3(%rax), %eax
	movl	%eax, 768(%rsp)         # 4-byte Spill
	movl	484(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, 728(%rsp)         # 4-byte Spill
.LBB0_85:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_87 Depth 7
	cmpl	$0, 720(%rsp)           # 4-byte Folded Reload
	movl	$8191, %esi             # imm = 0x1FFF
	movl	800(%rsp), %ebp         # 4-byte Reload
	movq	600(%rsp), %r11         # 8-byte Reload
	movq	680(%rsp), %r10         # 8-byte Reload
	movl	%eax, %edx
	jle	.LBB0_88
# BB#86:                                # %.lr.ph1220
                                        #   in Loop: Header=BB0_85 Depth=6
	movl	728(%rsp), %eax         # 4-byte Reload
	movl	%eax, %ecx
	subl	564(%rsp), %ecx         # 4-byte Folded Reload
	imull	588(%rsp), %ecx         # 4-byte Folded Reload
	movl	%ecx, 736(%rsp)         # 4-byte Spill
	subl	572(%rsp), %eax         # 4-byte Folded Reload
	imull	592(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 732(%rsp)         # 4-byte Spill
	movl	%eax, %r12d
	subl	784(%rsp), %r12d        # 4-byte Folded Reload
	movl	652(%rsp), %ebx         # 4-byte Reload
	movl	720(%rsp), %r15d        # 4-byte Reload
.LBB0_87:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        #             Parent Loop BB0_85 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	cmpl	%edx, %ebx
	movl	768(%rsp), %edx         # 4-byte Reload
	cmovlel	%ebx, %edx
	leal	-1(%rdx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%esi, %eax
	movl	%edx, %edi
	subl	784(%rsp), %edi         # 4-byte Folded Reload
	movl	732(%rsp), %r9d         # 4-byte Reload
	leal	(%rdi,%r9), %ecx
	movslq	%ecx, %rcx
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	addl	%r12d, %eax
	movslq	%eax, %rax
	movl	%edx, %r14d
	subl	%ebp, %r14d
	leal	1(%rdx), %r13d
	cmpl	$8192, %r13d            # imm = 0x2000
	vmovss	(%r11,%rax,4), %xmm0
	cmovgel	%esi, %r13d
	leal	4(%rdx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	vmovss	(%r11,%rcx,4), %xmm1
	cmovgel	%esi, %eax
	leal	3(%rdx), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%esi, %ecx
	cmpl	$8192, %edx             # imm = 0x2000
	movl	$8191, %r8d             # imm = 0x1FFF
	cmovll	%edx, %r8d
	leal	2(%rdx), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%esi, %edx
	leal	3(%rdi,%r9), %esi
	movl	%esi, 744(%rsp)         # 4-byte Spill
	leal	1(%rdi,%r9), %ebp
	leal	2(%rdi,%r9), %esi
	movl	%esi, 760(%rsp)         # 4-byte Spill
	movl	736(%rsp), %edi         # 4-byte Reload
	leal	3(%r14,%rdi), %r9d
	leal	1(%r14,%rdi), %esi
	movl	%esi, 752(%rsp)         # 4-byte Spill
	leal	2(%r14,%rdi), %esi
	movl	%esi, 740(%rsp)         # 4-byte Spill
	leal	(%r14,%rdi), %edi
	movslq	%edi, %rdi
	testl	%r13d, %r13d
	movl	$0, %esi
	cmovsl	%esi, %r13d
	addl	%r12d, %r13d
	movslq	%r13d, %r14
	xorl	%r13d, %r13d
	vmulss	(%r11,%r14,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r10,%rdi,4)
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	addl	%r12d, %eax
	addl	%r12d, %edx
	addl	%r12d, %ecx
	addl	%r12d, %r8d
	movslq	%r8d, %rdi
	vmovss	(%r11,%rdi,4), %xmm0
	addl	$4, %ebx
	movslq	%ebp, %rdi
	vmovss	(%r11,%rdi,4), %xmm1
	decl	%r15d
	movslq	%r9d, %r8
	movslq	744(%rsp), %r9          # 4-byte Folded Reload
	movslq	%ecx, %rbp
	movslq	%edx, %rcx
	movslq	%eax, %rax
	movslq	740(%rsp), %rdx         # 4-byte Folded Reload
	movslq	760(%rsp), %rdi         # 4-byte Folded Reload
	movslq	752(%rsp), %rsi         # 4-byte Folded Reload
	vmulss	(%r11,%rcx,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r10,%rsi,4)
	movl	$8191, %esi             # imm = 0x1FFF
	vmulss	(%r11,%rbp,4), %xmm4, %xmm1
	vmovss	(%r11,%rdi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r11,%r14,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r10,%rdx,4)
	movl	768(%rsp), %edx         # 4-byte Reload
	vmulss	(%r11,%rax,4), %xmm4, %xmm1
	vmovss	(%r11,%r9,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r11,%rcx,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r10,%r8,4)
	movl	800(%rsp), %ebp         # 4-byte Reload
	jne	.LBB0_87
.LBB0_88:                               # %._crit_edge1221
                                        #   in Loop: Header=BB0_85 Depth=6
	movl	728(%rsp), %eax         # 4-byte Reload
	incl	%eax
	movl	%eax, 728(%rsp)         # 4-byte Spill
	cmpl	640(%rsp), %eax         # 4-byte Folded Reload
	movl	%edx, %eax
	jl	.LBB0_85
.LBB0_89:                               # %.loopexit1222
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	644(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 488(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_103
# BB#90:                                # %.lr.ph1232
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	708(%rsp), %eax         # 4-byte Reload
	leal	4(%rax), %esi
	subl	652(%rsp), %esi         # 4-byte Folded Reload
	sarl	$2, %esi
	movl	%esi, 648(%rsp)         # 4-byte Spill
	addl	$-3, %eax
	movl	%eax, 708(%rsp)         # 4-byte Spill
	movl	488(%rsp), %r8d         # 4-byte Reload
.LBB0_91:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_94 Depth 7
	testl	%esi, %esi
	movq	680(%rsp), %rdi         # 8-byte Reload
	jle	.LBB0_92
# BB#93:                                # %.lr.ph1227
                                        #   in Loop: Header=BB0_91 Depth=6
	leal	1(%r8), %eax
	movl	%eax, 720(%rsp)         # 4-byte Spill
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %r12d            # imm = 0x1FFF
	cmovll	%eax, %r12d
	testl	%r12d, %r12d
	cmovsl	%r13d, %r12d
	leal	-1(%r8), %ebp
	movl	564(%rsp), %ecx         # 4-byte Reload
	subl	%ecx, %r12d
	cmpl	$8192, %ebp             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovgel	%eax, %ebp
	testl	%ebp, %ebp
	cmovsl	%r13d, %ebp
	subl	%ecx, %ebp
	movl	%r8d, %ebx
	subl	%ecx, %ebx
	movl	588(%rsp), %eax         # 4-byte Reload
	imull	%eax, %ebp
	movl	%ebp, 736(%rsp)         # 4-byte Spill
	imull	%eax, %r12d
	imull	%eax, %ebx
	movl	%ebx, 728(%rsp)         # 4-byte Spill
	subl	576(%rsp), %r8d         # 4-byte Folded Reload
	imull	584(%rsp), %r8d         # 4-byte Folded Reload
	movl	%r8d, 732(%rsp)         # 4-byte Spill
	movl	652(%rsp), %r14d        # 4-byte Reload
	movl	%esi, %r15d
	movq	624(%rsp), %r13         # 8-byte Reload
.LBB0_94:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        #             Parent Loop BB0_91 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	movl	708(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r14d
	movl	%eax, %edx
	cmovlel	%r14d, %edx
	movl	%edx, %ebp
	subl	800(%rsp), %ebp         # 4-byte Folded Reload
	movl	736(%rsp), %eax         # 4-byte Reload
	leal	(%rbp,%rax), %ecx
	leal	(%rbp,%r12), %esi
	movslq	%esi, %r9
	movslq	%ecx, %rsi
	subl	776(%rsp), %edx         # 4-byte Folded Reload
	leal	(%rdx,%r8), %ecx
	movslq	%ecx, %rcx
	vmovss	(%rdi,%rsi,4), %xmm0
	vmulss	(%rdi,%r9,4), %xmm4, %xmm1
	movl	728(%rsp), %ebx         # 4-byte Reload
	leal	(%rbp,%rbx), %esi
	movslq	%esi, %rsi
	vmovss	(%rdi,%rsi,4), %xmm2
	vfmadd213ss	%xmm1, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm0
	vmovss	%xmm0, (%r13,%rcx,4)
	leal	1(%rbp,%r12), %ecx
	movslq	%ecx, %rcx
	leal	2(%rdx,%r8), %esi
	movl	%esi, 744(%rsp)         # 4-byte Spill
	leal	3(%rbp,%rax), %esi
	movl	%esi, 760(%rsp)         # 4-byte Spill
	vmulss	(%rdi,%rcx,4), %xmm4, %xmm0
	leal	3(%rdx,%r8), %ecx
	movl	%ecx, 768(%rsp)         # 4-byte Spill
	leal	1(%rdx,%r8), %ecx
	movl	%ecx, 740(%rsp)         # 4-byte Spill
	leal	1(%rbp,%rax), %esi
	leal	2(%rbp,%rax), %r8d
	leal	3(%rbp,%rbx), %edx
	leal	1(%rbp,%rbx), %ecx
	leal	2(%rbp,%rbx), %eax
	leal	3(%rbp,%r12), %r11d
	leal	2(%rbp,%r12), %r10d
	movslq	%esi, %rsi
	vmovss	(%rdi,%rsi,4), %xmm1
	addl	$4, %r14d
	movslq	%ecx, %r9
	movslq	768(%rsp), %rcx         # 4-byte Folded Reload
	movq	%rcx, 768(%rsp)         # 8-byte Spill
	movslq	760(%rsp), %rcx         # 4-byte Folded Reload
	movq	%rcx, 760(%rsp)         # 8-byte Spill
	movslq	%edx, %rcx
	movq	%rcx, 752(%rsp)         # 8-byte Spill
	movslq	%r11d, %rbx
	movslq	744(%rsp), %r11         # 4-byte Folded Reload
	movslq	%r8d, %rbp
	movl	732(%rsp), %r8d         # 4-byte Reload
	movslq	%eax, %rsi
	movslq	%r10d, %rdx
	movslq	740(%rsp), %rcx         # 4-byte Folded Reload
	vmovss	(%rdi,%r9,4), %xmm2
	decl	%r15d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%r13,%rcx,4)
	vmulss	(%rdi,%rdx,4), %xmm4, %xmm1
	vmovss	(%rdi,%rsi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rdi,%rbp,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r13,%r11,4)
	vmulss	(%rdi,%rbx,4), %xmm4, %xmm1
	movq	752(%rsp), %rax         # 8-byte Reload
	vmovss	(%rdi,%rax,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	760(%rsp), %rax         # 8-byte Reload
	vmovss	(%rdi,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	768(%rsp), %rax         # 8-byte Reload
	vmovss	%xmm1, (%r13,%rax,4)
	jne	.LBB0_94
# BB#95:                                #   in Loop: Header=BB0_91 Depth=6
	movl	720(%rsp), %r8d         # 4-byte Reload
	xorl	%r13d, %r13d
	movl	648(%rsp), %esi         # 4-byte Reload
	jmp	.LBB0_96
.LBB0_92:                               # %._crit_edge1277
                                        #   in Loop: Header=BB0_91 Depth=6
	incl	%r8d
.LBB0_96:                               # %._crit_edge1228
                                        #   in Loop: Header=BB0_91 Depth=6
	movq	%rdi, 680(%rsp)         # 8-byte Spill
	cmpl	644(%rsp), %r8d         # 4-byte Folded Reload
	jl	.LBB0_91
# BB#97:                                # %.loopexit1229
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	644(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 488(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_103
# BB#98:                                # %.lr.ph1240
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	712(%rsp), %eax         # 4-byte Reload
	leal	4(%rax), %ecx
	subl	656(%rsp), %ecx         # 4-byte Folded Reload
	sarl	$2, %ecx
	movl	%ecx, 720(%rsp)         # 4-byte Spill
	leal	-3(%rax), %eax
	movl	%eax, 768(%rsp)         # 4-byte Spill
	movl	488(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, 728(%rsp)         # 4-byte Spill
.LBB0_99:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_101 Depth 7
	cmpl	$0, 720(%rsp)           # 4-byte Folded Reload
	movl	$8191, %edi             # imm = 0x1FFF
	movl	804(%rsp), %ebp         # 4-byte Reload
	movq	624(%rsp), %r11         # 8-byte Reload
	movq	616(%rsp), %r10         # 8-byte Reload
	movl	%eax, %edx
	jle	.LBB0_102
# BB#100:                               # %.lr.ph1236
                                        #   in Loop: Header=BB0_99 Depth=6
	movl	728(%rsp), %eax         # 4-byte Reload
	movl	%eax, %ecx
	subl	568(%rsp), %ecx         # 4-byte Folded Reload
	imull	580(%rsp), %ecx         # 4-byte Folded Reload
	movl	%ecx, 736(%rsp)         # 4-byte Spill
	subl	576(%rsp), %eax         # 4-byte Folded Reload
	imull	584(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 732(%rsp)         # 4-byte Spill
	movl	%eax, %r12d
	subl	776(%rsp), %r12d        # 4-byte Folded Reload
	movl	656(%rsp), %ebx         # 4-byte Reload
	movl	720(%rsp), %r15d        # 4-byte Reload
.LBB0_101:                              #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        #             Parent Loop BB0_99 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	cmpl	%edx, %ebx
	movl	768(%rsp), %edx         # 4-byte Reload
	cmovlel	%ebx, %edx
	leal	-1(%rdx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	cmovgel	%edi, %eax
	movl	%edx, %esi
	subl	776(%rsp), %esi         # 4-byte Folded Reload
	movl	732(%rsp), %r9d         # 4-byte Reload
	leal	(%rsi,%r9), %ecx
	movslq	%ecx, %rcx
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	addl	%r12d, %eax
	movslq	%eax, %rax
	movl	%edx, %r14d
	subl	%ebp, %r14d
	leal	1(%rdx), %r13d
	cmpl	$8192, %r13d            # imm = 0x2000
	vmovss	(%r11,%rax,4), %xmm0
	cmovgel	%edi, %r13d
	leal	4(%rdx), %eax
	cmpl	$8192, %eax             # imm = 0x2000
	vmovss	(%r11,%rcx,4), %xmm1
	cmovgel	%edi, %eax
	leal	3(%rdx), %ecx
	cmpl	$8192, %ecx             # imm = 0x2000
	cmovgel	%edi, %ecx
	cmpl	$8192, %edx             # imm = 0x2000
	movl	$8191, %r8d             # imm = 0x1FFF
	cmovll	%edx, %r8d
	leal	2(%rdx), %edx
	cmpl	$8192, %edx             # imm = 0x2000
	cmovgel	%edi, %edx
	leal	3(%rsi,%r9), %edi
	movl	%edi, 744(%rsp)         # 4-byte Spill
	leal	1(%rsi,%r9), %ebp
	leal	2(%rsi,%r9), %esi
	movl	%esi, 760(%rsp)         # 4-byte Spill
	movl	736(%rsp), %esi         # 4-byte Reload
	leal	3(%r14,%rsi), %r9d
	leal	1(%r14,%rsi), %edi
	movl	%edi, 752(%rsp)         # 4-byte Spill
	leal	2(%r14,%rsi), %edi
	movl	%edi, 740(%rsp)         # 4-byte Spill
	leal	(%r14,%rsi), %esi
	movslq	%esi, %rsi
	testl	%r13d, %r13d
	movl	$0, %edi
	cmovsl	%edi, %r13d
	addl	%r12d, %r13d
	movslq	%r13d, %r14
	xorl	%r13d, %r13d
	vmulss	(%r11,%r14,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r10,%rsi,4)
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	testl	%ecx, %ecx
	cmovsl	%r13d, %ecx
	testl	%edx, %edx
	cmovsl	%r13d, %edx
	testl	%eax, %eax
	cmovsl	%r13d, %eax
	addl	%r12d, %eax
	addl	%r12d, %edx
	addl	%r12d, %ecx
	addl	%r12d, %r8d
	movslq	%r8d, %rsi
	vmovss	(%r11,%rsi,4), %xmm0
	addl	$4, %ebx
	movslq	%ebp, %rsi
	vmovss	(%r11,%rsi,4), %xmm1
	decl	%r15d
	movslq	%r9d, %r8
	movslq	744(%rsp), %r9          # 4-byte Folded Reload
	movslq	%ecx, %rbp
	movslq	%edx, %rcx
	movslq	%eax, %rax
	movslq	740(%rsp), %rdx         # 4-byte Folded Reload
	movslq	760(%rsp), %rsi         # 4-byte Folded Reload
	movslq	752(%rsp), %rdi         # 4-byte Folded Reload
	vmulss	(%r11,%rcx,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r10,%rdi,4)
	movl	$8191, %edi             # imm = 0x1FFF
	vmulss	(%r11,%rbp,4), %xmm4, %xmm1
	vmovss	(%r11,%rsi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r11,%r14,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r10,%rdx,4)
	movl	768(%rsp), %edx         # 4-byte Reload
	vmulss	(%r11,%rax,4), %xmm4, %xmm1
	vmovss	(%r11,%r9,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r11,%rcx,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r10,%r8,4)
	movl	804(%rsp), %ebp         # 4-byte Reload
	jne	.LBB0_101
.LBB0_102:                              # %._crit_edge1237
                                        #   in Loop: Header=BB0_99 Depth=6
	movl	728(%rsp), %eax         # 4-byte Reload
	incl	%eax
	movl	%eax, 728(%rsp)         # 4-byte Spill
	cmpl	644(%rsp), %eax         # 4-byte Folded Reload
	movl	%edx, %eax
	jl	.LBB0_99
.LBB0_103:                              # %.loopexit1238
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	636(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 492(%rsp)         # 4-byte Folded Reload
	movq	432(%rsp), %r9          # 8-byte Reload
	movl	700(%rsp), %ecx         # 4-byte Reload
	movl	668(%rsp), %edx         # 4-byte Reload
	movl	696(%rsp), %esi         # 4-byte Reload
	movl	692(%rsp), %ebp         # 4-byte Reload
	movl	664(%rsp), %r8d         # 4-byte Reload
	movl	660(%rsp), %r10d        # 4-byte Reload
	jge	.LBB0_114
# BB#104:                               # %.lr.ph1248
                                        #   in Loop: Header=BB0_69 Depth=5
	incl	%esi
	movl	%esi, 696(%rsp)         # 4-byte Spill
	incl	%ebp
	movl	%ebp, 692(%rsp)         # 4-byte Spill
	incl	%r8d
	movl	%r8d, 664(%rsp)         # 4-byte Spill
	incl	%r10d
	movl	%r10d, 660(%rsp)        # 4-byte Spill
	incl	%ecx
	movl	%ecx, 700(%rsp)         # 4-byte Spill
	incl	%edx
	movl	%edx, 668(%rsp)         # 4-byte Spill
	movl	712(%rsp), %ecx         # 4-byte Reload
	leal	4(%rcx), %r9d
	subl	656(%rsp), %r9d         # 4-byte Folded Reload
	sarl	$2, %r9d
	movl	%r9d, 708(%rsp)         # 4-byte Spill
	addl	$-3, %ecx
	movl	%ecx, 712(%rsp)         # 4-byte Spill
	movl	492(%rsp), %r12d        # 4-byte Reload
.LBB0_105:                              #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_108 Depth 7
	testl	%r9d, %r9d
	jle	.LBB0_106
# BB#107:                               # %.lr.ph1243
                                        #   in Loop: Header=BB0_105 Depth=6
	leal	1(%r12), %eax
	movl	%eax, 720(%rsp)         # 4-byte Spill
	cmpl	$8192, %eax             # imm = 0x2000
	movl	$8191, %r8d             # imm = 0x1FFF
	cmovll	%eax, %r8d
	testl	%r8d, %r8d
	cmovsl	%r13d, %r8d
	leal	-1(%r12), %esi
	movl	568(%rsp), %edx         # 4-byte Reload
	subl	%edx, %r8d
	cmpl	$8192, %esi             # imm = 0x2000
	movl	$8191, %eax             # imm = 0x1FFF
	cmovgel	%eax, %esi
	testl	%esi, %esi
	cmovsl	%r13d, %esi
	subl	%edx, %esi
	movl	%r12d, %ebp
	subl	%edx, %ebp
	movl	580(%rsp), %eax         # 4-byte Reload
	imull	%eax, %esi
	movl	%esi, 732(%rsp)         # 4-byte Spill
	imull	%eax, %r8d
	movl	%r8d, %edi
	imull	%eax, %ebp
	movl	%ebp, 728(%rsp)         # 4-byte Spill
	subl	520(%rsp), %r12d        # 4-byte Folded Reload
	imull	524(%rsp), %r12d        # 4-byte Folded Reload
	movl	656(%rsp), %r14d        # 4-byte Reload
	movl	%r9d, %r15d
	movq	616(%rsp), %r11         # 8-byte Reload
	movq	672(%rsp), %rax         # 8-byte Reload
.LBB0_108:                              #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        #             Parent Loop BB0_105 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	cmpl	%ecx, %r14d
	movl	%ecx, %edx
	cmovlel	%r14d, %edx
	movl	%edx, %ebp
	subl	804(%rsp), %ebp         # 4-byte Folded Reload
	movl	732(%rsp), %r9d         # 4-byte Reload
	leal	(%rbp,%r9), %ecx
	leal	(%rbp,%rdi), %esi
	movslq	%esi, %r8
	movslq	%ecx, %rsi
	subl	704(%rsp), %edx         # 4-byte Folded Reload
	leal	(%rdx,%r12), %ecx
	movslq	%ecx, %rcx
	vmovss	(%r11,%rsi,4), %xmm0
	vmulss	(%r11,%r8,4), %xmm4, %xmm1
	movl	728(%rsp), %ebx         # 4-byte Reload
	leal	(%rbp,%rbx), %esi
	movslq	%esi, %rsi
	vmovss	(%r11,%rsi,4), %xmm2
	vfmadd213ss	%xmm1, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm0
	vmovss	%xmm0, (%rax,%rcx,4)
	leal	1(%rbp,%rdi), %ecx
	movslq	%ecx, %rcx
	leal	2(%rdx,%r12), %esi
	movl	%esi, 744(%rsp)         # 4-byte Spill
	leal	3(%rbp,%r9), %esi
	movl	%esi, 760(%rsp)         # 4-byte Spill
	vmulss	(%r11,%rcx,4), %xmm4, %xmm0
	leal	3(%rdx,%r12), %r13d
	leal	1(%rdx,%r12), %ecx
	movl	%ecx, 740(%rsp)         # 4-byte Spill
	leal	1(%rbp,%r9), %esi
	leal	2(%rbp,%r9), %ecx
	movl	%ecx, 736(%rsp)         # 4-byte Spill
	leal	3(%rbp,%rbx), %edx
	leal	1(%rbp,%rbx), %ecx
	leal	2(%rbp,%rbx), %ebx
	leal	3(%rbp,%rdi), %r8d
	leal	2(%rbp,%rdi), %r10d
	movslq	%esi, %rsi
	vmovss	(%r11,%rsi,4), %xmm1
	addl	$4, %r14d
	movslq	%ecx, %r9
	movslq	%r13d, %rcx
	movq	%rcx, 768(%rsp)         # 8-byte Spill
	movslq	760(%rsp), %rcx         # 4-byte Folded Reload
	movq	%rcx, 760(%rsp)         # 8-byte Spill
	movslq	%edx, %rcx
	movq	%rcx, 752(%rsp)         # 8-byte Spill
	movslq	%r8d, %r8
	movslq	744(%rsp), %r13         # 4-byte Folded Reload
	movslq	736(%rsp), %rbp         # 4-byte Folded Reload
	movslq	%ebx, %rsi
	movslq	%r10d, %rdx
	movslq	740(%rsp), %rcx         # 4-byte Folded Reload
	vmovss	(%r11,%r9,4), %xmm2
	decl	%r15d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%rax,%rcx,4)
	movl	712(%rsp), %ecx         # 4-byte Reload
	vmulss	(%r11,%rdx,4), %xmm4, %xmm1
	vmovss	(%r11,%rsi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r11,%rbp,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rax,%r13,4)
	vmulss	(%r11,%r8,4), %xmm4, %xmm1
	movq	752(%rsp), %rdx         # 8-byte Reload
	vmovss	(%r11,%rdx,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	760(%rsp), %rdx         # 8-byte Reload
	vmovss	(%r11,%rdx,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	768(%rsp), %rdx         # 8-byte Reload
	vmovss	%xmm1, (%rax,%rdx,4)
	jne	.LBB0_108
# BB#109:                               #   in Loop: Header=BB0_105 Depth=6
	movq	%rax, 672(%rsp)         # 8-byte Spill
	movl	720(%rsp), %r12d        # 4-byte Reload
	xorl	%r13d, %r13d
	movl	636(%rsp), %eax         # 4-byte Reload
	movl	668(%rsp), %edx         # 4-byte Reload
	movl	696(%rsp), %esi         # 4-byte Reload
	movl	692(%rsp), %ebp         # 4-byte Reload
	movl	664(%rsp), %r8d         # 4-byte Reload
	movl	660(%rsp), %r10d        # 4-byte Reload
	movl	708(%rsp), %r9d         # 4-byte Reload
	jmp	.LBB0_110
.LBB0_106:                              # %._crit_edge1276
                                        #   in Loop: Header=BB0_105 Depth=6
	incl	%r12d
.LBB0_110:                              # %._crit_edge1244
                                        #   in Loop: Header=BB0_105 Depth=6
	cmpl	%eax, %r12d
	jl	.LBB0_105
# BB#111:                               # %.loopexit1245
                                        #   in Loop: Header=BB0_69 Depth=5
	cmpl	%eax, 492(%rsp)         # 4-byte Folded Reload
	movq	432(%rsp), %r9          # 8-byte Reload
	jge	.LBB0_114
# BB#112:                               # %.preheader.preheader
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	364(%rsp), %eax         # 4-byte Reload
	addl	%eax, %esi
	movl	%esi, 696(%rsp)         # 4-byte Spill
	addl	%eax, %ebp
	movl	%ebp, 692(%rsp)         # 4-byte Spill
	addl	%eax, %r8d
	movl	%r8d, 664(%rsp)         # 4-byte Spill
	addl	%eax, %r10d
	movl	%r10d, 660(%rsp)        # 4-byte Spill
	addl	%eax, 700(%rsp)         # 4-byte Folded Spill
	addl	%eax, %edx
	movl	%edx, 668(%rsp)         # 4-byte Spill
	xorl	%eax, %eax
	movl	492(%rsp), %r12d        # 4-byte Reload
	movl	444(%rsp), %edx         # 4-byte Reload
	movl	440(%rsp), %esi         # 4-byte Reload
.LBB0_113:                              # %.preheader
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_46 Depth=2
                                        #       Parent Loop BB0_47 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movl	696(%rsp), %ecx         # 4-byte Reload
	leal	(%rcx,%rax), %edi
	movslq	%edi, %rdi
	movq	%rdi, 768(%rsp)         # 8-byte Spill
	movq	672(%rsp), %rcx         # 8-byte Reload
	vmulss	(%rcx,%rdi,4), %xmm4, %xmm1
	movl	528(%rsp), %edi         # 4-byte Reload
	leal	(%rdi,%rsi), %edi
	movslq	%edi, %rdi
	vmovss	(%rcx,%rdi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	leal	(%rbp,%rax), %edi
	movslq	%edi, %rdi
	vmovss	(%rcx,%rdi,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movl	532(%rsp), %edi         # 4-byte Reload
	leal	(%rdi,%rsi), %edi
	leal	-3(%rdx), %ebp
	movslq	%ebp, %rbp
	vmovss	%xmm1, (%r9,%rbp,4)
	movslq	%edi, %rbx
	leal	(%r10,%rax), %r10d
	leal	(%r8,%rax), %edi
	vmovss	(%rcx,%rbx,4), %xmm0
	movl	668(%rsp), %ebp         # 4-byte Reload
	leal	(%rbp,%rax), %ebp
	movl	%ebp, 752(%rsp)         # 4-byte Spill
	movl	700(%rsp), %ebp         # 4-byte Reload
	leal	(%rbp,%rax), %r11d
	movl	544(%rsp), %ebx         # 4-byte Reload
	addl	%ebx, %eax
	movl	536(%rsp), %ebp         # 4-byte Reload
	leal	(%rbp,%rsi), %r15d
	movl	540(%rsp), %ebp         # 4-byte Reload
	leal	(%rbp,%rsi), %r8d
	leal	(%rsi,%rbx), %esi
	movslq	%edi, %r14
	movslq	%r10d, %rdi
	vmovss	(%rcx,%rdi,4), %xmm1
	vmulss	(%rcx,%r14,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm0
	movslq	%r11d, %r10
	incl	%r12d
	movslq	%r8d, %rdi
	movq	%rdi, 760(%rsp)         # 8-byte Spill
	movslq	752(%rsp), %r11         # 4-byte Folded Reload
	movslq	%r15d, %r15
	leal	-2(%rdx), %edi
	movslq	%edi, %rdi
	movslq	%edx, %rbx
	leal	32(%rdx), %edx
	leal	-1(%rbx), %ebp
	movslq	%ebp, %rbp
	cmpl	636(%rsp), %r12d        # 4-byte Folded Reload
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r9,%rdi,4)
	vmulss	(%rcx,%r10,4), %xmm4, %xmm1
	vmovss	(%rcx,%r15,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	768(%rsp), %rdi         # 8-byte Reload
	vmovss	(%rcx,%rdi,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r9,%rbp,4)
	vmulss	(%rcx,%r11,4), %xmm4, %xmm1
	movl	660(%rsp), %r10d        # 4-byte Reload
	movl	664(%rsp), %r8d         # 4-byte Reload
	movl	692(%rsp), %ebp         # 4-byte Reload
	movq	760(%rsp), %rdi         # 8-byte Reload
	vmovss	(%rcx,%rdi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rcx,%r14,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r9,%rbx,4)
	jl	.LBB0_113
.LBB0_114:                              # %._crit_edge1252
                                        #   in Loop: Header=BB0_69 Depth=5
	movq	376(%rsp), %rax         # 8-byte Reload
	movq	400(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rax), %rax
	vmulss	(%r9,%rax,4), %xmm4, %xmm0
	movq	384(%rsp), %rcx         # 8-byte Reload
	leaq	(%rsi,%rcx), %rdi
	vmovss	(%r9,%rdi,4), %xmm1
	vfmadd213ss	%xmm0, %xmm3, %xmm1
	addq	368(%rsp), %rsi         # 8-byte Folded Reload
	vmovss	(%r9,%rsi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	movslq	%eax, %rcx
	movq	%rcx, 752(%rsp)         # 8-byte Spill
	movl	452(%rsp), %ebp         # 4-byte Reload
	subl	356(%rsp), %ebp         # 4-byte Folded Reload
	movl	396(%rsp), %ebx         # 4-byte Reload
	leal	(%rbp,%rbx), %edx
	movslq	%edx, %rdx
	movq	472(%rsp), %rax         # 8-byte Reload
	vmovss	%xmm0, (%rax,%rdx,4)
	movslq	%edi, %r15
	movq	%rcx, %rdi
	movq	%rcx, %rdx
	orq	$1, %rdi
	movslq	%esi, %rax
	movq	%rax, %rsi
	orq	$1, %rsi
	vmulss	(%r9,%rdi,4), %xmm4, %xmm2
	movq	%r15, %rdi
	orq	$1, %rdi
	vmovss	(%r9,%rdi,4), %xmm0
	addl	$4, 444(%rsp)           # 4-byte Folded Spill
	addl	$4, 440(%rsp)           # 4-byte Folded Spill
	movl	428(%rsp), %r12d        # 4-byte Reload
	addl	$-4, %r12d
	leal	1(%rbp,%rbx), %ecx
	vmovss	(%r9,%rsi,4), %xmm1
	leal	2(%rbp,%rbx), %esi
	movl	%esi, 760(%rsp)         # 4-byte Spill
	leal	3(%rbp,%rbx), %esi
	movl	%esi, 768(%rsp)         # 4-byte Spill
	movl	424(%rsp), %r11d        # 4-byte Reload
	addl	$-4, %r11d
	vfmadd213ss	%xmm2, %xmm3, %xmm0
	movl	420(%rsp), %ebx         # 4-byte Reload
	addl	$-4, %ebx
	movl	416(%rsp), %r10d        # 4-byte Reload
	addl	$-4, %r10d
	movq	%r9, %rdi
	movslq	%ecx, %r9
	movl	412(%rsp), %r8d         # 4-byte Reload
	addl	$-4, %r8d
	addl	$-4, 468(%rsp)          # 4-byte Folded Spill
	movq	456(%rsp), %r14         # 8-byte Reload
	incq	%r14
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	leaq	(,%rdx,4), %rsi
	leaq	(,%r15,4), %rcx
	leaq	(,%rax,4), %rbp
	movq	472(%rsp), %rdx         # 8-byte Reload
	vmovss	%xmm1, (%rdx,%r9,4)
	movslq	768(%rsp), %r9          # 4-byte Folded Reload
	movq	%r9, 768(%rsp)          # 8-byte Spill
	orq	$12, %rbp
	orq	$12, %rcx
	orq	$12, %rsi
	movslq	760(%rsp), %r9          # 4-byte Folded Reload
	orq	$2, %rax
	orq	$2, %r15
	movq	752(%rsp), %rdx         # 8-byte Reload
	orq	$2, %rdx
	cmpl	$8, %r14d
	vmulss	(%rdi,%rdx,4), %xmm4, %xmm1
	vmovss	(%rdi,%r15,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rdi,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	472(%rsp), %rax         # 8-byte Reload
	vmovss	%xmm1, (%rax,%r9,4)
	movl	%r8d, %r9d
	vmulss	(%rdi,%rsi), %xmm4, %xmm1
	vmovss	(%rdi,%rcx), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rdi,%rbp), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	768(%rsp), %rcx         # 8-byte Reload
	vmovss	%xmm1, (%rax,%rcx,4)
	movq	%r14, %rax
	movl	468(%rsp), %edx         # 4-byte Reload
	movl	$8191, %ebp             # imm = 0x1FFF
	movq	%rdi, %r14
	jne	.LBB0_69
# BB#115:                               #   in Loop: Header=BB0_68 Depth=4
	movq	344(%rsp), %r12         # 8-byte Reload
	incq	%r12
	cmpl	$32, %r12d
	movq	624(%rsp), %r15         # 8-byte Reload
	movq	616(%rsp), %rbx         # 8-byte Reload
	jne	.LBB0_68
# BB#116:                               #   in Loop: Header=BB0_47 Depth=3
	xorl	%edi, %edi
	movq	608(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	600(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	680(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	%r15, %rsi
	callq	halide_free
	xorl	%edi, %edi
	movq	%rbx, %rsi
	callq	halide_free
	xorl	%edi, %edi
	movq	672(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	%r14, %rsi
	callq	halide_free
	addl	$32, 316(%rsp)          # 4-byte Folded Spill
	addl	$-32, 312(%rsp)         # 4-byte Folded Spill
	movl	240(%rsp), %r12d        # 4-byte Reload
	addl	$-32, %r12d
	addl	$32, 252(%rsp)          # 4-byte Folded Spill
	addl	$-32, 248(%rsp)         # 4-byte Folded Spill
	movl	236(%rsp), %r14d        # 4-byte Reload
	addl	$-32, %r14d
	addl	$-32, 244(%rsp)         # 4-byte Folded Spill
	movl	256(%rsp), %eax         # 4-byte Reload
	addl	$-32, %eax
	movl	232(%rsp), %r15d        # 4-byte Reload
	addl	$-32, %r15d
	movl	260(%rsp), %esi         # 4-byte Reload
	addl	$32, %esi
	movl	336(%rsp), %ebp         # 4-byte Reload
	addl	$-32, %ebp
	movl	228(%rsp), %r11d        # 4-byte Reload
	addl	$32, %r11d
	movl	224(%rsp), %edi         # 4-byte Reload
	addl	$-32, %edi
	movl	220(%rsp), %ecx         # 4-byte Reload
	addl	$32, %ecx
	movl	340(%rsp), %ebx         # 4-byte Reload
	addl	$-32, %ebx
	movl	332(%rsp), %edx         # 4-byte Reload
	addl	$-32, %edx
	movl	328(%rsp), %r9d         # 4-byte Reload
	addl	$-32, %r9d
	movl	324(%rsp), %r8d         # 4-byte Reload
	addl	$-32, %r8d
	movl	216(%rsp), %r10d        # 4-byte Reload
	incl	%r10d
	cmpl	$16, %r10d
	jne	.LBB0_47
# BB#117:                               #   in Loop: Header=BB0_46 Depth=2
	movl	116(%rsp), %ebp         # 4-byte Reload
	addl	$-32, %ebp
	movl	120(%rsp), %r8d         # 4-byte Reload
	addl	$32, %r8d
	movl	112(%rsp), %edi         # 4-byte Reload
	addl	$-32, %edi
	movl	108(%rsp), %edx         # 4-byte Reload
	addl	$-32, %edx
	movl	124(%rsp), %eax         # 4-byte Reload
	addl	$-32, %eax
	movl	104(%rsp), %ecx         # 4-byte Reload
	incl	%ecx
	cmpl	$16, %ecx
	movl	$8191, %r10d            # imm = 0x1FFF
	jne	.LBB0_46
# BB#118:                               #   in Loop: Header=BB0_45 Depth=1
	movl	32(%rsp), %ebp          # 4-byte Reload
	incl	%ebp
	xorl	%eax, %eax
	cmpl	28(%rsp), %ebp          # 4-byte Folded Reload
	movl	356(%rsp), %r8d         # 4-byte Reload
	movl	276(%rsp), %r9d         # 4-byte Reload
	movl	8(%rsp), %ebx           # 4-byte Reload
	movl	4(%rsp), %r10d          # 4-byte Reload
	movl	$8191, %r11d            # imm = 0x1FFF
	jne	.LBB0_45
	jmp	.LBB0_119
.LBB0_30:
	leal	-1(%rcx,%rdi), %ecx
	xorl	%edi, %edi
	movl	$.L.str9, %esi
	movl	%r13d, %edx
.LBB0_16:
	xorb	%al, %al
	callq	halide_printf
	movl	$-1, %eax
.LBB0_119:                              # %.loopexit1258
	addq	$808, %rsp              # imm = 0x328
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
.LBB0_32:
	xorl	%edi, %edi
	movl	$.L.str10, %esi
	jmp	.LBB0_33
.LBB0_35:
	xorl	%edi, %edi
	movl	$.L.str11, %esi
	jmp	.LBB0_33
.LBB0_37:
	xorl	%edi, %edi
	movl	$.L.str12, %esi
	jmp	.LBB0_33
.LBB0_39:
	xorl	%edi, %edi
	movl	$.L.str13, %esi
	jmp	.LBB0_33
.LBB0_41:
	xorl	%edi, %edi
	movl	$.L.str14, %esi
	jmp	.LBB0_33
.LBB0_120:
	xorl	%edi, %edi
	movl	$.L.str15, %esi
.LBB0_33:
	xorb	%al, %al
	callq	halide_printf
	movl	$-1, %eax
	jmp	.LBB0_119
.Ltmp20:
	.size	main_compute, .Ltmp20-main_compute
	.cfi_endproc

	.section	.text.startup,"ax",@progbits
	.align	16, 0x90
	.type	_GLOBAL__I_a,@function
_GLOBAL__I_a:                           # @_GLOBAL__I_a
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp22:
	.cfi_def_cfa_offset 16
	movl	$_ZStL8__ioinit, %edi
	callq	_ZNSt8ios_base4InitC1Ev
	movl	$_ZNSt8ios_base4InitD1Ev, %edi
	movl	$_ZStL8__ioinit, %esi
	movl	$__dso_handle, %edx
	popq	%rax
	jmp	__cxa_atexit            # TAILCALL
.Ltmp23:
	.size	_GLOBAL__I_a, .Ltmp23-_GLOBAL__I_a
	.cfi_endproc

	.type	_ZStL8__ioinit,@object  # @_ZStL8__ioinit
	.local	_ZStL8__ioinit
	.comm	_ZStL8__ioinit,1,1
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	 "Output buffer f7 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n"
	.size	.L.str, 95

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	 "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n"
	.size	.L.str1, 97

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	 "Output buffer f7 is accessed at %d, which is before the min (%d) in dimension 0\n"
	.size	.L.str2, 81

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	 "Output buffer f7 is accessed at %d, which is beyond the max (%d) in dimension 0\n"
	.size	.L.str3, 81

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	 "Output buffer f7 is accessed at %d, which is before the min (%d) in dimension 1\n"
	.size	.L.str4, 81

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	 "Output buffer f7 is accessed at %d, which is beyond the max (%d) in dimension 1\n"
	.size	.L.str5, 81

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	 "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n"
	.size	.L.str6, 83

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	 "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n"
	.size	.L.str7, 83

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	 "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n"
	.size	.L.str8, 83

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	 "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n"
	.size	.L.str9, 83

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	 "Static constraint violated: f7.stride.0 == 1\n"
	.size	.L.str10, 46

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	 "Static constraint violated: inPar.stride.0 == 1\n"
	.size	.L.str11, 49

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	 "Total allocation for buffer f7 exceeds 2^31 - 1\n"
	.size	.L.str12, 49

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	 "Product of extents for buffer f7 exceeds 2^31 - 1\n"
	.size	.L.str13, 51

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	 "Total allocation for buffer inPar exceeds 2^31 - 1\n"
	.size	.L.str14, 52

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	 "Product of extents for buffer inPar exceeds 2^31 - 1\n"
	.size	.L.str15, 54

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	 "32-bit signed overflow computing size of allocation f0\n"
	.size	.L.str16, 56

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	 "32-bit signed overflow computing size of allocation f4\n"
	.size	.L.str17, 56

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	 "32-bit signed overflow computing size of allocation f1\n"
	.size	.L.str18, 56

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	 "32-bit signed overflow computing size of allocation f5\n"
	.size	.L.str19, 56

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	 "32-bit signed overflow computing size of allocation f2\n"
	.size	.L.str20, 56

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	 "32-bit signed overflow computing size of allocation f6\n"
	.size	.L.str21, 56

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	 "32-bit signed overflow computing size of allocation f3\n"
	.size	.L.str22, 56

	.section	.init_array,"aw",@init_array
	.align	8
	.quad	_GLOBAL__I_a

	.section	".note.GNU-stack","",@progbits
