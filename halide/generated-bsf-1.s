	.file	"generated-bsf-1.c"
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
	subq	$472, %rsp              # imm = 0x1D8
.Ltmp13:
	.cfi_def_cfa_offset 528
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
	movb	%dl, 380(%rsp)          # 1-byte Spill
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movl	64(%rdi), %eax
	movl	%eax, 400(%rsp)         # 4-byte Spill
	movslq	36(%rdi), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movl	32(%rdi), %eax
	movl	%eax, 420(%rsp)         # 4-byte Spill
	movl	48(%rdi), %eax
	movl	%eax, 448(%rsp)         # 4-byte Spill
	movslq	20(%rdi), %rax
	movq	%rax, 440(%rsp)         # 8-byte Spill
	movslq	16(%rdi), %rax
	movq	%rax, 424(%rsp)         # 8-byte Spill
	movl	52(%rdi), %eax
	movq	%rdi, 408(%rsp)         # 8-byte Spill
	movl	%eax, 152(%rsp)         # 4-byte Spill
	movq	8(%rsi), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB0_4
# BB#3:
	cmpq	$0, (%rsi)
	sete	%cl
.LBB0_4:
	movb	%cl, 384(%rsp)          # 1-byte Spill
	movslq	16(%rsi), %r10
	movq	%r10, 432(%rsp)         # 8-byte Spill
	leal	63(%r10), %ecx
	sarl	$6, %ecx
	movl	%ecx, 44(%rsp)          # 4-byte Spill
	movslq	20(%rsi), %rdx
	movq	%rdx, 456(%rsp)         # 8-byte Spill
	leal	63(%rdx), %eax
	movq	%rdx, %r8
	sarl	$6, %eax
	imull	%ecx, %eax
	movl	%eax, 52(%rsp)          # 4-byte Spill
	leal	-1(%rax), %r12d
	movl	%r12d, %eax
	cltd
	idivl	%ecx
	subl	%edx, %r12d
	sarl	$31, %edx
	movl	%ecx, %eax
	negl	%eax
	movl	%eax, 40(%rsp)          # 4-byte Spill
	andl	%eax, %edx
	movl	32(%rsi), %eax
	movl	%eax, 392(%rsp)         # 4-byte Spill
	movl	48(%rsi), %edi
	movl	%edi, 48(%rsp)          # 4-byte Spill
	addl	%edx, %r12d
	leal	1(%rdi), %eax
	cmpl	$2048, %eax             # imm = 0x800
	movl	$2047, %r13d            # imm = 0x7FF
	cmovgel	%r13d, %eax
	xorl	%r14d, %r14d
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %edi
	cmovlel	%edi, %eax
	leal	-1(%rdi), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r13d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r14d, %ebp
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	leal	(%r10,%rdi), %edx
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovll	%edx, %eax
	movl	%edx, %r15d
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	leal	-1(%r10,%rdi), %edx
	movl	%edx, 376(%rsp)         # 4-byte Spill
	cmpl	%eax, %edx
	cmovgel	%edx, %eax
	leal	-2(%r10,%rdi), %r11d
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r13d, %r11d
	testl	%r11d, %r11d
	cmovsl	%r14d, %r11d
	cmpl	%r11d, %eax
	cmovgel	%eax, %r11d
	movl	52(%rsi), %ebx
	leal	(%r8,%rbx), %edx
	movl	%edx, 464(%rsp)         # 4-byte Spill
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovll	%edx, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	leal	-1(%r8,%rbx), %edx
	movl	%edx, 372(%rsp)         # 4-byte Spill
	cmpl	%eax, %edx
	cmovgel	%edx, %eax
	movl	%ebx, %edx
	leal	-2(%r8,%rdx), %r9d
	cmpl	$2048, %r9d             # imm = 0x800
	cmovgel	%r13d, %r9d
	testl	%r9d, %r9d
	cmovsl	%r14d, %r9d
	cmpl	%r9d, %eax
	cmovgel	%eax, %r9d
	leal	1(%rdx), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %edx
	cmovlel	%edx, %eax
	movq	%rsi, 360(%rsp)         # 8-byte Spill
	leal	-1(%rdx), %r8d
	movl	%edx, %esi
	cmpl	$2048, %r8d             # imm = 0x800
	cmovgel	%r13d, %r8d
	testl	%r8d, %r8d
	cmovsl	%r14d, %r8d
	cmpl	%r8d, %eax
	cmovlel	%eax, %r8d
	movl	%r12d, %eax
	cltd
	idivl	%ecx
	leal	-1(%r10), %edx
	andl	$-64, %edx
	leal	64(%rdi,%rdx), %edx
	leal	-64(%r10,%rdi), %ecx
	movl	%ecx, 32(%rsp)          # 4-byte Spill
	cmpl	%ecx, %edi
	cmovlel	%edi, %ecx
	movl	%ecx, %edi
	cmpl	%r15d, %edx
	cmovlel	%edx, %r15d
	movl	%r15d, 452(%rsp)        # 4-byte Spill
	leal	1(%rbp), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r14d, %edx
	cmpl	%edx, %ebp
	cmovlel	%ebp, %edx
	decl	%ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r13d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r14d, %ebp
	cmpl	%ebp, %edx
	cmovlel	%edx, %ebp
	leal	1(%r11), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r14d, %edx
	cmpl	%edx, %r11d
	cmovgel	%r11d, %edx
	decl	%r11d
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r13d, %r11d
	testl	%r11d, %r11d
	cmovsl	%r14d, %r11d
	cmpl	%r11d, %edx
	cmovgel	%edx, %r11d
	leal	1(%r9), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r14d, %edx
	cmpl	%edx, %r9d
	cmovgel	%r9d, %edx
	decl	%r9d
	cmpl	$2048, %r9d             # imm = 0x800
	cmovgel	%r13d, %r9d
	testl	%r9d, %r9d
	cmovsl	%r14d, %r9d
	cmpl	%r9d, %edx
	cmovgel	%edx, %r9d
	leal	1(%r8), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r13d, %edx
	testl	%edx, %edx
	cmovsl	%r14d, %edx
	cmpl	%edx, %r8d
	cmovlel	%r8d, %edx
	decl	%r8d
	cmpl	$2048, %r8d             # imm = 0x800
	cmovgel	%r13d, %r8d
	testl	%r8d, %r8d
	cmovsl	%r14d, %r8d
	cmpl	%r8d, %edx
	cmovlel	%edx, %r8d
	movl	%eax, %edx
	shll	$6, %edx
	movl	%eax, %r15d
	sarl	$31, %r15d
	andl	%edx, %r15d
	testl	%eax, %eax
	cmovlel	%r14d, %edx
	leal	64(%rsi,%rdx), %eax
	movl	464(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %eax
	cmovlel	%eax, %ecx
	movl	%ecx, 464(%rsp)         # 4-byte Spill
	leal	1(%rbp), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %ebp
	cmovlel	%ebp, %eax
	decl	%ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r13d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r14d, %ebp
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	leal	1(%r11), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %r11d
	cmovgel	%r11d, %eax
	decl	%r11d
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r13d, %r11d
	testl	%r11d, %r11d
	cmovsl	%r14d, %r11d
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
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %r9d
	cmovgel	%r9d, %eax
	decl	%r9d
	cmpl	$2048, %r9d             # imm = 0x800
	cmovgel	%r13d, %r9d
	testl	%r9d, %r9d
	cmovsl	%r14d, %r9d
	cmpl	%r9d, %eax
	cmovgel	%eax, %r9d
	leal	1(%r8), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %r8d
	cmovlel	%r8d, %eax
	decl	%r8d
	cmpl	$2048, %r8d             # imm = 0x800
	cmovgel	%r13d, %r8d
	testl	%r8d, %r8d
	cmovsl	%r14d, %r8d
	cmpl	%r8d, %eax
	cmovlel	%eax, %r8d
	leal	1(%rbx), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r13d, %eax
	testl	%eax, %eax
	cmovsl	%r14d, %eax
	cmpl	%eax, %ebx
	cmovgel	%ebx, %eax
	decl	%ebx
	leal	1(%r11), %r10d
	cmpl	$2048, %r10d            # imm = 0x800
	cmovgel	%r13d, %r10d
	leal	1(%r9), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r13d, %edx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r13d, %ebx
	leal	-1(%r11), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r13d, %ebp
	leal	1(%r8), %r14d
	cmpl	$2048, %r14d            # imm = 0x800
	cmovgel	%r13d, %r14d
	leal	-1(%r8), %r12d
	cmpl	$2048, %r12d            # imm = 0x800
	cmovgel	%r13d, %r12d
	leal	-1(%r9), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovll	%ecx, %r13d
	testl	%r10d, %r10d
	movl	$0, %ecx
	cmovsl	%ecx, %r10d
	cmpl	%r10d, %r11d
	cmovlel	%r11d, %r10d
	testl	%edx, %edx
	cmovsl	%ecx, %edx
	cmpl	%edx, %r9d
	cmovgel	%r9d, %edx
	movl	%esi, %r9d
	testl	%r14d, %r14d
	cmovsl	%ecx, %r14d
	cmpl	%r14d, %r8d
	cmovlel	%r8d, %r14d
	movq	360(%rsp), %rsi         # 8-byte Reload
	movb	384(%rsp), %r11b        # 1-byte Reload
	testl	%ebp, %ebp
	cmovsl	%ecx, %ebp
	testl	%r13d, %r13d
	cmovsl	%ecx, %r13d
	testl	%r12d, %r12d
	cmovsl	%ecx, %r12d
	xorl	%ecx, %ecx
	testl	%ebx, %ebx
	cmovsl	%ecx, %ebx
	cmpl	%ebx, %eax
	cmovgel	%eax, %ebx
	cmpl	%ebp, %r10d
	cmovlel	%r10d, %ebp
	movl	464(%rsp), %r8d         # 4-byte Reload
	cmpl	%r12d, %r14d
	cmovlel	%r14d, %r12d
	cmpl	%r13d, %edx
	cmovgel	%edx, %r13d
	movq	456(%rsp), %rax         # 8-byte Reload
	leal	-64(%rax,%r9), %eax
	movl	%eax, 28(%rsp)          # 4-byte Spill
	addl	%r9d, %r15d
	cmpl	%eax, %r15d
	cmovgl	%eax, %r15d
	testb	%r11b, %r11b
	movl	64(%rsi), %edx
	movslq	36(%rsi), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	je	.LBB0_6
# BB#5:
	movl	452(%rsp), %eax         # 4-byte Reload
	subl	%edi, %eax
	movl	%edi, 48(%rsi)
	movl	%r15d, 52(%rsi)
	movl	$0, 56(%rsi)
	movl	$0, 60(%rsi)
	movl	%eax, 16(%rsi)
	movl	%r8d, %ecx
	subl	%r15d, %ecx
	movl	%ecx, 20(%rsi)
	movl	$0, 24(%rsi)
	movl	$0, 28(%rsi)
	movl	$1, 32(%rsi)
	movl	%eax, 36(%rsi)
	movq	$0, 40(%rsi)
.LBB0_6:
	movl	%edi, %r10d
	movl	%r8d, %edi
	movl	452(%rsp), %esi         # 4-byte Reload
	movb	380(%rsp), %r8b         # 1-byte Reload
	testb	%r8b, %r8b
	movl	448(%rsp), %r14d        # 4-byte Reload
	movq	408(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_8
# BB#7:                                 # %.thread
	subl	%ebp, %ebx
	movl	%ebp, 48(%rcx)
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
	jmp	.LBB0_114
.LBB0_8:
	xorl	%eax, %eax
	orb	%r8b, %r11b
	movq	432(%rsp), %r11         # 8-byte Reload
	jne	.LBB0_114
# BB#9:
	cmpl	$4, %edx
	movl	%r10d, %ecx
	jne	.LBB0_10
# BB#12:
	movl	400(%rsp), %edx         # 4-byte Reload
	cmpl	$4, %edx
	jne	.LBB0_13
# BB#14:
	movl	48(%rsp), %r8d          # 4-byte Reload
	cmpl	%ecx, %r8d
	movl	%esi, %edx
	jle	.LBB0_16
# BB#15:
	xorl	%edi, %edi
	movl	$.L.str2, %esi
	movl	%ecx, %edx
	movl	%r8d, %ecx
	jmp	.LBB0_28
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
	jmp	.LBB0_114
.LBB0_16:
	movl	%edx, %ecx
	subl	%r11d, %ecx
	cmpl	%r8d, %ecx
	jle	.LBB0_18
# BB#17:
	decl	%edx
	xorl	%edi, %edi
	movl	$.L.str3, %esi
	movl	376(%rsp), %ecx         # 4-byte Reload
	jmp	.LBB0_28
.LBB0_18:
	cmpl	%r15d, %r9d
	jle	.LBB0_20
# BB#19:
	xorl	%edi, %edi
	movl	$.L.str4, %esi
	movl	%r15d, %edx
	movl	%r9d, %ecx
	jmp	.LBB0_28
.LBB0_20:
	movl	%edi, %edx
	movl	%edx, %ecx
	movq	456(%rsp), %r10         # 8-byte Reload
	subl	%r10d, %ecx
	cmpl	%r9d, %ecx
	jle	.LBB0_22
# BB#21:
	decl	%edx
	xorl	%edi, %edi
	movl	$.L.str5, %esi
	movl	372(%rsp), %ecx         # 4-byte Reload
	jmp	.LBB0_28
.LBB0_22:
	cmpl	%ebp, %r14d
	jle	.LBB0_24
# BB#23:
	xorl	%edi, %edi
	movl	$.L.str6, %esi
	movl	%ebp, %edx
	movl	%r14d, %ecx
	jmp	.LBB0_28
.LBB0_24:
	movl	%ebx, %ecx
	movq	424(%rsp), %rsi         # 8-byte Reload
	subl	%esi, %ecx
	cmpl	%r14d, %ecx
	movq	440(%rsp), %rdi         # 8-byte Reload
	jge	.LBB0_25
# BB#26:
	movq	%r10, %rbp
	movl	152(%rsp), %ecx         # 4-byte Reload
	cmpl	%r12d, %ecx
	jle	.LBB0_29
# BB#27:
	xorl	%edi, %edi
	movl	$.L.str8, %esi
	movl	%r12d, %edx
	jmp	.LBB0_28
.LBB0_25:
	leal	-1(%r14,%rsi), %ecx
	xorl	%edi, %edi
	movl	$.L.str7, %esi
	movl	%ebx, %edx
	jmp	.LBB0_28
.LBB0_29:
	movl	%ecx, %edx
	movl	%r13d, %ecx
	subl	%edi, %ecx
	cmpl	%edx, %ecx
	movl	%edx, %ecx
	movq	%rbp, %rbx
	jge	.LBB0_30
# BB#31:
	cmpl	$1, 392(%rsp)           # 4-byte Folded Reload
	movl	%r8d, %r10d
	jne	.LBB0_32
# BB#34:
	movl	%ecx, %r8d
	cmpl	$1, 420(%rsp)           # 4-byte Folded Reload
	movl	%r9d, %ebp
	movl	%ebp, 36(%rsp)          # 4-byte Spill
	jne	.LBB0_35
# BB#36:
	movq	64(%rsp), %rdx          # 8-byte Reload
	imulq	%rbx, %rdx
	movl	$2147483648, %ecx       # imm = 0x80000000
	cmpq	%rcx, %rdx
	jge	.LBB0_37
# BB#38:
	movq	%rbx, %rdx
	imulq	%r11, %rdx
	cmpq	%rcx, %rdx
	jge	.LBB0_39
# BB#40:
	movq	144(%rsp), %rcx         # 8-byte Reload
	imulq	%rdi, %rcx
	movl	$2147483648, %edx       # imm = 0x80000000
	cmpq	%rdx, %rcx
	jge	.LBB0_41
# BB#42:
	imulq	%rsi, %rdi
	cmpq	$2147483647, %rdi       # imm = 0x7FFFFFFF
	jg	.LBB0_115
# BB#43:                                # %.preheader801
	cmpl	$0, 52(%rsp)            # 4-byte Folded Reload
	jle	.LBB0_114
# BB#44:                                # %.lr.ph804
	movl	$63, %ecx
	movl	%r8d, %esi
	movq	144(%rsp), %rax         # 8-byte Reload
	imull	%eax, %esi
	movl	$63, %eax
	subl	%r11d, %eax
	subl	%ebx, %ecx
	subl	%ebp, %ecx
	movl	%ecx, 24(%rsp)          # 4-byte Spill
	subl	%r10d, %eax
	movl	%eax, 60(%rsp)          # 4-byte Spill
	addl	%r14d, %esi
	movl	%esi, 152(%rsp)         # 4-byte Spill
	movl	%ebp, %eax
	notl	%eax
	movl	%eax, 20(%rsp)          # 4-byte Spill
	movl	%r10d, %eax
	negl	%eax
	movl	%eax, 16(%rsp)          # 4-byte Spill
	xorl	%r8d, %r8d
	movl	$2047, %r9d             # imm = 0x7FF
	xorl	%ebx, %ebx
.LBB0_45:                               # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_67 Depth 2
                                        #       Child Loop BB0_68 Depth 3
                                        #         Child Loop BB0_70 Depth 4
                                        #       Child Loop BB0_73 Depth 3
                                        #         Child Loop BB0_76 Depth 4
                                        #       Child Loop BB0_80 Depth 3
                                        #         Child Loop BB0_82 Depth 4
                                        #       Child Loop BB0_85 Depth 3
                                        #         Child Loop BB0_88 Depth 4
                                        #       Child Loop BB0_92 Depth 3
                                        #         Child Loop BB0_94 Depth 4
                                        #       Child Loop BB0_98 Depth 3
                                        #         Child Loop BB0_101 Depth 4
                                        #       Child Loop BB0_106 Depth 3
                                        #         Child Loop BB0_107 Depth 4
                                        #       Child Loop BB0_111 Depth 3
	movl	%ebx, 56(%rsp)          # 4-byte Spill
	movl	%ebx, %eax
	cltd
	movl	44(%rsp), %esi          # 4-byte Reload
	idivl	%esi
	movl	%edx, %edi
	movl	%ebx, %eax
	subl	%edi, %eax
	movl	%edi, %ecx
	sarl	$31, %ecx
	movl	%ecx, %edx
	andl	40(%rsp), %edx          # 4-byte Folded Reload
	addl	%edx, %eax
	cltd
	idivl	%esi
	shll	$6, %eax
	addl	%ebp, %eax
	movl	%eax, 392(%rsp)         # 4-byte Spill
	andl	%esi, %ecx
	addl	%edi, %ecx
	shll	$6, %ecx
	addl	%r10d, %ecx
	movl	%ecx, 400(%rsp)         # 4-byte Spill
	movl	28(%rsp), %edx          # 4-byte Reload
	cmpl	%edx, %eax
	movl	%edx, %edi
	cmovlel	%eax, %edi
	movl	%edi, 72(%rsp)          # 4-byte Spill
	cmpl	$2048, %edi             # imm = 0x800
	movl	$2047, %edx             # imm = 0x7FF
	cmovll	%edi, %edx
	leal	64(%rdi), %ebp
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r9d, %ebp
	leal	-2(%rdi), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r9d, %eax
	leal	62(%rdi), %r10d
	testl	%ebp, %ebp
	cmovsl	%r8d, %ebp
	cmpl	$2048, %r10d            # imm = 0x800
	cmovgel	%r9d, %r10d
	leal	-1(%rdi), %r11d
	cmpl	%edx, %r11d
	cmovgel	%r11d, %edx
	testl	%eax, %eax
	cmovsl	%r8d, %eax
	movl	32(%rsp), %esi          # 4-byte Reload
	cmpl	%esi, %ecx
	cmovlel	%ecx, %esi
	movl	%esi, 360(%rsp)         # 4-byte Spill
	leal	1(%rdi), %ebx
	leal	63(%rdi), %ecx
	movl	%ecx, 376(%rsp)         # 4-byte Spill
	cmpl	%ebp, %ecx
	cmovgel	%ecx, %ebp
	testl	%r10d, %r10d
	cmovsl	%r8d, %r10d
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r9d, %ebx
	leal	1(%rsi), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r9d, %ecx
	cmpl	%eax, %edx
	cmovgel	%edx, %eax
	leal	-1(%rsi), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r9d, %edx
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	movl	%edx, 344(%rsp)         # 4-byte Spill
	testl	%ecx, %ecx
	cmovsl	%r8d, %ecx
	movl	%ecx, 356(%rsp)         # 4-byte Spill
	testl	%ebx, %ebx
	cmovsl	%r8d, %ebx
	movl	%ebx, 372(%rsp)         # 4-byte Spill
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r9d, %r11d
	cmpl	%r10d, %ebp
	cmovgel	%ebp, %r10d
	movl	%r10d, 384(%rsp)        # 4-byte Spill
	leal	64(%rsi), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r9d, %ebp
	cmpl	%ebx, %edi
	movl	%ebx, %r15d
	cmovlel	%edi, %r15d
	testl	%r11d, %r11d
	cmovsl	%r8d, %r11d
	movl	%r11d, 380(%rsp)        # 4-byte Spill
	cmpl	%ecx, %esi
	movl	%ecx, %r14d
	cmovlel	%esi, %r14d
	cmpl	%edx, %r14d
	cmovgl	%edx, %r14d
	movl	%r14d, 408(%rsp)        # 4-byte Spill
	leal	62(%rsi), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r9d, %edx
	testl	%ebp, %ebp
	cmovsl	%r8d, %ebp
	movl	%ebp, 336(%rsp)         # 4-byte Spill
	leal	63(%rsi), %ecx
	movl	%ecx, 228(%rsp)         # 4-byte Spill
	cmpl	%ebp, %ecx
	movl	%ebp, %ebx
	cmovgel	%ecx, %ebx
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	movl	%edx, 332(%rsp)         # 4-byte Spill
	cmpl	%r11d, %r15d
	cmovgl	%r11d, %r15d
	movl	%r15d, 328(%rsp)        # 4-byte Spill
	leal	1(%rax), %ecx
	movl	%ecx, 236(%rsp)         # 4-byte Spill
	cmpl	$2048, %ecx             # imm = 0x800
	movl	$2047, %edi             # imm = 0x7FF
	cmovll	%ecx, %edi
	testl	%edi, %edi
	cmovsl	%r8d, %edi
	cmpl	%edx, %ebx
	cmovll	%edx, %ebx
	movl	%ebx, 456(%rsp)         # 4-byte Spill
	cmpl	%edi, %eax
	cmovgel	%eax, %edi
	decl	%eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r9d, %eax
	leal	1(%rbx), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r9d, %edx
	leal	1(%r14), %esi
	cmpl	$2048, %esi             # imm = 0x800
	cmovgel	%r9d, %esi
	leal	1(%r15), %r11d
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r9d, %r11d
	testl	%r11d, %r11d
	cmovsl	%r8d, %r11d
	testl	%esi, %esi
	cmovsl	%r8d, %esi
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	testl	%eax, %eax
	cmovsl	%r8d, %eax
	cmpl	%eax, %edi
	cmovgel	%edi, %eax
	leal	-1(%rbx), %r13d
	cmpl	$2048, %r13d            # imm = 0x800
	cmovgel	%r9d, %r13d
	cmpl	%edx, %ebx
	cmovgel	%ebx, %edx
	leal	-1(%r15), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r9d, %ecx
	cmpl	%esi, %r14d
	cmovlel	%r14d, %esi
	leal	-1(%r14), %r14d
	cmpl	$2048, %r14d            # imm = 0x800
	cmovgel	%r9d, %r14d
	cmpl	%r11d, %r15d
	cmovlel	%r15d, %r11d
	testl	%ecx, %ecx
	cmovsl	%r8d, %ecx
	testl	%r13d, %r13d
	cmovsl	%r8d, %r13d
	leal	1(%r10), %ebp
	movl	%ebp, 312(%rsp)         # 4-byte Spill
	cmpl	$2048, %ebp             # imm = 0x800
	movl	$2047, %edi             # imm = 0x7FF
	cmovll	%ebp, %edi
	testl	%edi, %edi
	cmovsl	%r8d, %edi
	testl	%r14d, %r14d
	cmovsl	%r8d, %r14d
	cmpl	%r14d, %esi
	cmovlel	%esi, %r14d
	leal	-1(%r10), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r9d, %ebx
	cmpl	%edi, %r10d
	cmovgel	%r10d, %edi
	testl	%ebx, %ebx
	cmovsl	%r8d, %ebx
	leal	1(%rax), %ebp
	movl	%ebp, 232(%rsp)         # 4-byte Spill
	cmpl	%r13d, %edx
	cmovgel	%edx, %r13d
	cmpl	$2048, %ebp             # imm = 0x800
	movl	$2047, %esi             # imm = 0x7FF
	cmovll	%ebp, %esi
	testl	%esi, %esi
	cmovsl	%r8d, %esi
	cmpl	%ecx, %r11d
	cmovlel	%r11d, %ecx
	movl	%ecx, 300(%rsp)         # 4-byte Spill
	leal	1(%rcx), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r9d, %edx
	leal	-1(%rax), %r11d
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r9d, %r11d
	cmpl	%esi, %eax
	cmovgel	%eax, %esi
	testl	%r11d, %r11d
	cmovsl	%r8d, %r11d
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	leal	1(%r14), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r9d, %eax
	cmpl	%ebx, %edi
	cmovgel	%edi, %ebx
	movl	%ebx, 464(%rsp)         # 4-byte Spill
	leal	-1(%rcx), %r10d
	cmpl	$2048, %r10d            # imm = 0x800
	cmovgel	%r9d, %r10d
	cmpl	%edx, %ecx
	cmovlel	%ecx, %edx
	testl	%r10d, %r10d
	cmovsl	%r8d, %r10d
	testl	%eax, %eax
	cmovsl	%r8d, %eax
	cmpl	%r11d, %esi
	cmovgel	%esi, %r11d
	leal	1(%rbx), %edi
	movl	%edi, 304(%rsp)         # 4-byte Spill
	cmpl	$2048, %edi             # imm = 0x800
	movl	$2047, %esi             # imm = 0x7FF
	cmovll	%edi, %esi
	testl	%esi, %esi
	cmovsl	%r8d, %esi
	cmpl	%esi, %ebx
	cmovgel	%ebx, %esi
	leal	-1(%rbx), %r15d
	cmpl	$2048, %r15d            # imm = 0x800
	cmovgel	%r9d, %r15d
	cmpl	%eax, %r14d
	cmovlel	%r14d, %eax
	testl	%r15d, %r15d
	cmovsl	%r8d, %r15d
	cmpl	%r10d, %edx
	cmovlel	%edx, %r10d
	leal	1(%r13), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r9d, %edx
	leal	-1(%r14), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r9d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r8d, %ebp
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	leal	1(%r11), %edi
	cmpl	%r15d, %esi
	cmovgel	%esi, %r15d
	movl	%r15d, 272(%rsp)        # 4-byte Spill
	cmpl	$2048, %edi             # imm = 0x800
	movl	$2047, %esi             # imm = 0x7FF
	cmovll	%edi, %esi
	testl	%esi, %esi
	cmovsl	%r8d, %esi
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	leal	-1(%r11), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r9d, %eax
	cmpl	%esi, %r11d
	cmovgel	%r11d, %esi
	leal	1(%r10), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r9d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r8d, %ebx
	testl	%eax, %eax
	cmovsl	%r8d, %eax
	cmpl	%eax, %esi
	cmovgel	%esi, %eax
	leal	-1(%r10), %esi
	cmpl	$2048, %esi             # imm = 0x800
	cmovgel	%r9d, %esi
	cmpl	%ebx, %r10d
	cmovlel	%r10d, %ebx
	testl	%esi, %esi
	cmovsl	%r8d, %esi
	cmpl	%edx, %r13d
	cmovgel	%r13d, %edx
	leal	-1(%r13), %r12d
	cmpl	$2048, %r12d            # imm = 0x800
	cmovgel	%r9d, %r12d
	testl	%r12d, %r12d
	cmovsl	%r8d, %r12d
	cmpl	%r12d, %edx
	cmovgel	%edx, %r12d
	movl	%r12d, %ecx
	subl	%ebp, %ecx
	movl	%ecx, 296(%rsp)         # 4-byte Spill
	movl	%ecx, %edx
	andl	$-4, %edx
	cmpl	%edi, %r10d
	cmovgl	%edi, %r10d
	movl	%r10d, 224(%rsp)        # 4-byte Spill
	leal	-1(%r15), %ecx
	cmpl	%esi, %ebx
	leal	3(%rbp,%rdx), %edx
	cmovlel	%ebx, %esi
	incl	%eax
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r9d, %ecx
	leal	-3(%r12), %edi
	movl	%edi, 452(%rsp)         # 4-byte Spill
	cmpl	%edi, %ebp
	cmovgl	%edi, %ebp
	leal	1(%r10), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r9d, %ebx
	cmpl	%r12d, %edx
	cmovlel	%edx, %r12d
	leal	-1(%r10), %r11d
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%r9d, %r11d
	leal	1(%r15), %edx
	movl	%edx, 420(%rsp)         # 4-byte Spill
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %edi             # imm = 0x7FF
	cmovll	%edx, %edi
	testl	%edi, %edi
	cmovsl	%r8d, %edi
	cmpl	%edi, %r15d
	cmovgel	%r15d, %edi
	cmpl	%eax, %esi
	cmovlel	%esi, %eax
	movl	%r12d, %esi
	subl	%ebp, %esi
	incl	%esi
	movl	%esi, 216(%rsp)         # 4-byte Spill
	testl	%ecx, %ecx
	cmovsl	%r8d, %ecx
	cmpl	%r10d, %eax
	cmovgl	%r10d, %eax
	testl	%ebx, %ebx
	cmovsl	%r8d, %ebx
	cmpl	%ecx, %edi
	movslq	%esi, %rsi
	cmovgel	%edi, %ecx
	cmpl	%r15d, %ecx
	cmovll	%r15d, %ecx
	incl	%ecx
	testl	%r11d, %r11d
	cmovsl	%r8d, %r11d
	cmpl	%ebx, %eax
	cmovlel	%eax, %ebx
	cmpl	%r11d, %ebx
	cmovgl	%r11d, %ebx
	movl	%ebx, 220(%rsp)         # 4-byte Spill
	subl	%ebx, %ecx
	movslq	%ecx, %rax
	imulq	%rsi, %rax
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rsi
	jg	.LBB0_47
# BB#46:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%rsi, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_48
.LBB0_47:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movq	%rsi, %rbx
	movl	$.L.str16, %esi
	xorb	%al, %al
	callq	halide_printf
	movq	%rbx, %rsi
.LBB0_48:                               #   in Loop: Header=BB0_45 Depth=1
	movl	%r13d, %eax
	subl	%r14d, %eax
	movl	%eax, 324(%rsp)         # 4-byte Spill
	movl	%eax, %r15d
	andl	$-4, %r15d
	addl	%r14d, %r15d
	leal	-3(%r13), %eax
	movl	%r13d, 448(%rsp)        # 4-byte Spill
	movl	%eax, 424(%rsp)         # 4-byte Spill
	cmpl	%eax, %r15d
	movl	%eax, %ebx
	cmovlel	%r15d, %ebx
	cmpl	%eax, %r14d
	cmovgl	%eax, %r14d
	movl	%r14d, 440(%rsp)        # 4-byte Spill
	leal	1(%r14), %r13d
	cmpl	$2048, %r13d            # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovgel	%eax, %r13d
	movl	%r14d, %ecx
	leal	4(%rbx), %r14d
	leal	3(%rbx), %edx
	cmpl	%edx, %r12d
	cmovgel	%r12d, %edx
	movl	%edx, %r12d
	cmpl	$2048, %r14d            # imm = 0x800
	cmovgel	%eax, %r14d
	testl	%r13d, %r13d
	movl	$0, %eax
	cmovsl	%eax, %r13d
	movl	%ebp, %eax
	movl	%eax, 432(%rsp)         # 4-byte Spill
	cmpl	%ecx, %eax
	movl	%ecx, %ebp
	cmovlel	%eax, %ebp
	xorl	%edi, %edi
	callq	halide_malloc
	movq	%rax, 288(%rsp)         # 8-byte Spill
	movl	420(%rsp), %eax         # 4-byte Reload
	subl	224(%rsp), %eax         # 4-byte Folded Reload
	movslq	%eax, %rax
	testl	%r14d, %r14d
	movl	$0, %ecx
	cmovsl	%ecx, %r14d
	cmpl	%r14d, %r12d
	cmovgel	%r12d, %r14d
	addl	$2, %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	movl	$2047, %esi             # imm = 0x7FF
	cmovgel	%esi, %ebx
	testl	%ebx, %ebx
	cmovsl	%ecx, %ebx
	movl	$0, %edx
	cmpl	%ebx, %r14d
	cmovgel	%r14d, %ebx
	cmpl	%r13d, %ebp
	cmovgl	%r13d, %ebp
	movl	440(%rsp), %ecx         # 4-byte Reload
	leal	-1(%rcx), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%esi, %ecx
	testl	%ecx, %ecx
	cmovsl	%edx, %ecx
	cmpl	%ecx, %ebp
	cmovlel	%ebp, %ecx
	movl	%ecx, 420(%rsp)         # 4-byte Spill
	subl	%ecx, %ebx
	incl	%ebx
	movl	%ebx, 212(%rsp)         # 4-byte Spill
	movslq	%ebx, %rcx
	imulq	%rax, %rcx
	cmpq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	leaq	(,%rcx,4), %r14
	jg	.LBB0_50
# BB#49:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%r14, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_51
.LBB0_50:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movl	$.L.str17, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_51:                               #   in Loop: Header=BB0_45 Depth=1
	movl	232(%rsp), %ebp         # 4-byte Reload
	movl	300(%rsp), %eax         # 4-byte Reload
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	movl	%ebp, 232(%rsp)         # 4-byte Spill
	leal	-1(%rbp), %r12d
	leal	1(%rbp), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovgel	%eax, %ebx
	cmpl	$2048, %r12d            # imm = 0x800
	cmovgel	%eax, %r12d
	xorl	%edi, %edi
	movq	%r14, %rsi
	callq	halide_malloc
	movq	%rax, 280(%rsp)         # 8-byte Spill
	testl	%r12d, %r12d
	movl	$0, %eax
	cmovsl	%eax, %r12d
	testl	%ebx, %ebx
	cmovsl	%eax, %ebx
	movl	224(%rsp), %eax         # 4-byte Reload
	cmpl	%ebp, %eax
	cmovlel	%eax, %ebp
	movl	464(%rsp), %eax         # 4-byte Reload
	movl	272(%rsp), %edx         # 4-byte Reload
	cmpl	%eax, %edx
	cmovll	%eax, %edx
	addl	$3, %r15d
	movl	448(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r15d
	cmovgl	%eax, %r15d
	movl	%r15d, %eax
	subl	440(%rsp), %eax         # 4-byte Folded Reload
	incl	%eax
	movl	%eax, 204(%rsp)         # 4-byte Spill
	incl	%edx
	cmpl	%ebx, %ebp
	cmovgl	%ebx, %ebp
	cmpl	%r12d, %ebp
	cmovgl	%r12d, %ebp
	movl	%ebp, 208(%rsp)         # 4-byte Spill
	subl	%ebp, %edx
	movslq	%eax, %rcx
	movslq	%edx, %rax
	imulq	%rcx, %rax
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	leaq	(,%rax,4), %rsi
	movl	456(%rsp), %ebx         # 4-byte Reload
	jg	.LBB0_53
# BB#52:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%rsi, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_54
.LBB0_53:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movq	%rsi, %rbp
	movl	$.L.str18, %esi
	xorb	%al, %al
	callq	halide_printf
	movq	%rbp, %rsi
.LBB0_54:                               #   in Loop: Header=BB0_45 Depth=1
	movl	%ebx, %eax
	movl	408(%rsp), %r14d        # 4-byte Reload
	subl	%r14d, %eax
	movl	%eax, 300(%rsp)         # 4-byte Spill
	andl	$-4, %eax
	movl	%eax, 260(%rsp)         # 4-byte Spill
	leal	3(%r14,%rax), %r13d
	cmpl	%ebx, %r13d
	cmovgl	%ebx, %r13d
	addl	$-3, %ebx
	movl	%ebx, 456(%rsp)         # 4-byte Spill
	cmpl	%ebx, %r14d
	cmovgl	%ebx, %r14d
	leal	1(%r14), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovgel	%eax, %ebx
	leal	1(%r13), %ebp
	cmpl	%r13d, %r15d
	cmovll	%r13d, %r15d
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%eax, %ebp
	testl	%ebx, %ebx
	movl	$0, %eax
	cmovsl	%eax, %ebx
	movl	440(%rsp), %eax         # 4-byte Reload
	cmpl	%r14d, %eax
	movl	%r14d, %r12d
	cmovlel	%eax, %r12d
	xorl	%edi, %edi
	callq	halide_malloc
	movq	%rax, 272(%rsp)         # 8-byte Spill
	movl	304(%rsp), %eax         # 4-byte Reload
	subl	232(%rsp), %eax         # 4-byte Folded Reload
	movslq	%eax, %rax
	testl	%ebp, %ebp
	movl	$0, %edx
	cmovsl	%edx, %ebp
	cmpl	%ebp, %r15d
	cmovgel	%r15d, %ebp
	leal	-1(%r13), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	movl	$2047, %edi             # imm = 0x7FF
	cmovgel	%edi, %ecx
	testl	%ecx, %ecx
	cmovsl	%edx, %ecx
	movl	$0, %esi
	cmpl	%ecx, %ebp
	cmovgel	%ebp, %ecx
	cmpl	%ebx, %r12d
	cmovgl	%ebx, %r12d
	leal	-1(%r14), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%edi, %edx
	testl	%edx, %edx
	cmovsl	%esi, %edx
	cmpl	%edx, %r12d
	cmovlel	%r12d, %edx
	movl	%edx, 448(%rsp)         # 4-byte Spill
	subl	%edx, %ecx
	incl	%ecx
	movl	%ecx, 200(%rsp)         # 4-byte Spill
	movslq	%ecx, %rcx
	imulq	%rax, %rcx
	cmpq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	leaq	(,%rcx,4), %r15
	jg	.LBB0_56
# BB#55:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%r15, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_57
.LBB0_56:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movl	$.L.str19, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_57:                               #   in Loop: Header=BB0_45 Depth=1
	movl	236(%rsp), %ecx         # 4-byte Reload
	movl	328(%rsp), %eax         # 4-byte Reload
	cmpl	%ecx, %eax
	cmovlel	%eax, %ecx
	movl	%ecx, 236(%rsp)         # 4-byte Spill
	leal	1(%rcx), %ebx
	leal	-1(%rcx), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovgel	%eax, %ebp
	movl	232(%rsp), %edx         # 4-byte Reload
	cmpl	%ecx, %edx
	movl	%ecx, %r12d
	cmovlel	%edx, %r12d
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%eax, %ebx
	xorl	%edi, %edi
	movq	%r15, %rsi
	callq	halide_malloc
	movq	%rax, 264(%rsp)         # 8-byte Spill
	testl	%ebx, %ebx
	movl	$0, %eax
	cmovsl	%eax, %ebx
	cmpl	%ebx, %r12d
	cmovlel	%r12d, %ebx
	testl	%ebp, %ebp
	cmovsl	%eax, %ebp
	movl	384(%rsp), %r15d        # 4-byte Reload
	movl	464(%rsp), %ecx         # 4-byte Reload
	cmpl	%r15d, %ecx
	cmovll	%r15d, %ecx
	movl	%r13d, %eax
	subl	%r14d, %eax
	incl	%eax
	movl	%eax, 192(%rsp)         # 4-byte Spill
	movslq	%eax, %rax
	incl	%ecx
	cmpl	%ebp, %ebx
	cmovlel	%ebx, %ebp
	movl	%ebp, 196(%rsp)         # 4-byte Spill
	subl	%ebp, %ecx
	movslq	%ecx, %rcx
	imulq	%rax, %rcx
	cmpq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	leaq	(,%rcx,4), %r12
	jg	.LBB0_59
# BB#58:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%r12, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_60
.LBB0_59:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movl	$.L.str20, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_60:                               #   in Loop: Header=BB0_45 Depth=1
	movl	360(%rsp), %ebx         # 4-byte Reload
	cmpl	%ebx, %r14d
	cmovlel	%r14d, %ebx
	movl	356(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	cmovgl	%eax, %ebx
	movl	228(%rsp), %ebp         # 4-byte Reload
	cmpl	%ebp, %r13d
	cmovgel	%r13d, %ebp
	movl	344(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	cmovgl	%eax, %ebx
	movl	336(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebp
	cmovll	%eax, %ebp
	movl	332(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebp
	cmovll	%eax, %ebp
	subl	%ebx, %ebp
	incl	%ebp
	movl	%ebp, 228(%rsp)         # 4-byte Spill
	xorl	%edi, %edi
	movq	%r12, %rsi
	callq	halide_malloc
	movq	%rax, 464(%rsp)         # 8-byte Spill
	movl	312(%rsp), %eax         # 4-byte Reload
	subl	236(%rsp), %eax         # 4-byte Folded Reload
	movslq	%eax, %rax
	movslq	%ebp, %rcx
	imulq	%rax, %rcx
	cmpq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	leaq	(,%rcx,4), %r13
	jg	.LBB0_62
# BB#61:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%r13, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_63
.LBB0_62:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movl	$.L.str21, %esi
	xorb	%al, %al
	callq	halide_printf
.LBB0_63:                               #   in Loop: Header=BB0_45 Depth=1
	movl	%r14d, 408(%rsp)        # 4-byte Spill
	movl	%ebx, 360(%rsp)         # 4-byte Spill
	movl	72(%rsp), %eax          # 4-byte Reload
	movl	236(%rsp), %ecx         # 4-byte Reload
	cmpl	%eax, %ecx
	cmovlel	%ecx, %eax
	movl	372(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %eax
	cmovgl	%ecx, %eax
	movl	380(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %eax
	cmovgl	%ecx, %eax
	movl	376(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %r15d
	cmovgel	%r15d, %ecx
	incl	%ecx
	subl	%eax, %ecx
	movslq	%ecx, %rbx
	xorl	%edi, %edi
	movq	%r13, %rsi
	callq	halide_malloc
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%rbx, %r10
	shlq	$8, %r10
	shlq	$6, %rbx
	cmpq	$2147483647, %rbx       # imm = 0x7FFFFFFF
	jg	.LBB0_65
# BB#64:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%r10, %rax
	shrq	$31, %rax
	testq	%rax, %rax
	je	.LBB0_66
.LBB0_65:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movl	$.L.str22, %esi
	xorb	%al, %al
	movq	%r10, %rbx
	callq	halide_printf
	movq	%rbx, %r10
.LBB0_66:                               #   in Loop: Header=BB0_45 Depth=1
	movl	392(%rsp), %r12d        # 4-byte Reload
	notl	%r12d
	movl	24(%rsp), %r9d          # 4-byte Reload
	cmpl	%r9d, %r12d
	movl	%r9d, %ecx
	cmovgel	%r12d, %ecx
	movl	%ecx, 380(%rsp)         # 4-byte Spill
	cmpl	$-2049, %ecx            # imm = 0xFFFFFFFFFFFFF7FF
	movl	$-2048, %eax            # imm = 0xFFFFFFFFFFFFF800
	cmovgl	%ecx, %eax
	movl	20(%rsp), %ebx          # 4-byte Reload
	subl	%ecx, %ebx
	leal	1(%rcx), %edx
	movl	%edx, 108(%rsp)         # 4-byte Spill
	movl	%ecx, %esi
	movl	%edx, %ecx
	cmpl	$-2049, %ecx            # imm = 0xFFFFFFFFFFFFF7FF
	movl	$-2048, %edx            # imm = 0xFFFFFFFFFFFFF800
	cmovgl	%ecx, %edx
	movq	64(%rsp), %rcx          # 8-byte Reload
	imull	%ecx, %ebx
	movl	%edx, %ecx
	notl	%ecx
	xorl	$-1, %edx
	movl	$0, %edx
	cmovsl	%edx, %ecx
	movl	$0, %r15d
	notl	%eax
	leal	2(%rsi), %edx
	cmpl	$-2049, %edx            # imm = 0xFFFFFFFFFFFFF7FF
	movl	$-2048, %r8d            # imm = 0xFFFFFFFFFFFFF800
	cmovlel	%r8d, %edx
	notl	%edx
	cmpl	%edx, %eax
	cmovgel	%eax, %edx
	notl	%ecx
	leal	-1(%rsi), %ebp
	movl	%ebp, 104(%rsp)         # 4-byte Spill
	movl	%esi, %edi
	cmpl	$-2049, %ebp            # imm = 0xFFFFFFFFFFFFF7FF
	movl	$-2048, %eax            # imm = 0xFFFFFFFFFFFFF800
	cmovgl	%ebp, %eax
	movl	%eax, %esi
	xorl	$-1, %esi
	notl	%eax
	cmovsl	%r15d, %eax
	notl	%eax
	cmpl	%ecx, %eax
	cmovll	%ecx, %eax
	movl	$-2, %ecx
	subl	%edi, %ecx
	cmpl	%ecx, %edx
	cmovgel	%edx, %ecx
	addl	16(%rsp), %ebx          # 4-byte Folded Reload
	movl	%ebx, 116(%rsp)         # 4-byte Spill
	movl	400(%rsp), %ebx         # 4-byte Reload
	notl	%ebx
	movl	%ebx, 400(%rsp)         # 4-byte Spill
	movl	60(%rsp), %edx          # 4-byte Reload
	cmpl	%edx, %ebx
	movl	%edx, %edi
	movl	%edx, %r13d
	cmovgel	%ebx, %edi
	movl	%edi, 332(%rsp)         # 4-byte Spill
	movl	%ebx, %r11d
	leal	1(%rdi), %esi
	movl	%edi, %ebx
	cmpl	$-2049, %esi            # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %esi
	movl	%esi, %edx
	xorl	$-1, %edx
	notl	%esi
	cmovsl	%r15d, %esi
	notl	%esi
	movl	%esi, 392(%rsp)         # 4-byte Spill
	movl	%esi, %edi
	leal	-1(%rbx), %esi
	cmpl	$-2049, %esi            # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %esi
	movl	%esi, %edx
	xorl	$-1, %edx
	notl	%esi
	cmovsl	%r15d, %esi
	notl	%esi
	movl	%esi, 384(%rsp)         # 4-byte Spill
	cmpl	%edi, %esi
	movl	%edi, %r14d
	cmovgel	%esi, %r14d
	cmpl	%r11d, %r14d
	cmovll	%r11d, %r14d
	cmpl	%r13d, %r14d
	cmovll	%r13d, %r14d
	leal	-3(%r14), %edi
	subl	260(%rsp), %edi         # 4-byte Folded Reload
	cmpl	%r12d, %eax
	cmovll	%r12d, %eax
	movl	%ebx, %edx
	leal	-64(%rdx), %ebx
	cmpl	$-2049, %ebx            # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %ebx
	notl	%ebx
	leal	-62(%rdx), %esi
	cmpl	$-2049, %esi            # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %esi
	notl	%esi
	movl	$62, %ebp
	subl	%edx, %ebp
	movl	%ebp, 376(%rsp)         # 4-byte Spill
	cmpl	%ebx, %esi
	movl	%ebx, %edx
	cmovgel	%esi, %edx
	cmpl	%ebp, %edx
	cmovll	%ebp, %edx
	testl	%edx, %edx
	cmovsl	%r15d, %edx
	movl	$2, %ebp
	subl	%edx, %ebp
	movl	%ebp, 76(%rsp)          # 4-byte Spill
	notl	%edx
	cmpl	%edi, %edx
	cmovll	%edi, %edx
	testl	%ecx, %ecx
	cmovsl	%r15d, %ecx
	movl	$-2, %r15d
	subl	%ecx, %r15d
	cmpl	%r15d, %eax
	cmovgel	%eax, %r15d
	cmpl	%r9d, %r15d
	cmovll	%r9d, %r15d
	notl	%edx
	cmpl	%esi, %edx
	cmovll	%esi, %edx
	cmpl	%ebx, %edx
	cmovll	%ebx, %edx
	movl	%edx, %ebx
	leal	1(%r14), %ecx
	cmpl	$-2049, %ecx            # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %ecx
	movl	%ecx, %eax
	xorl	$-1, %eax
	notl	%ecx
	movl	$0, %edx
	cmovsl	%edx, %ecx
	leal	-1(%r14), %eax
	cmpl	$-2049, %eax            # imm = 0xFFFFFFFFFFFFF7FF
	notl	%ecx
	movl	%ecx, 372(%rsp)         # 4-byte Spill
	movl	%ecx, %esi
	cmovlel	%r8d, %eax
	movl	%eax, %ecx
	xorl	$-1, %ecx
	notl	%eax
	cmovsl	%edx, %eax
	movl	$0, %edx
	notl	%eax
	movl	384(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %eax
	movl	%ecx, %r12d
	cmovgel	%eax, %r12d
	cmpl	%esi, %r12d
	cmovll	%esi, %r12d
	movl	392(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, %r12d
	cmovll	%ecx, %r12d
	cmpl	%r11d, %r12d
	cmovll	%r11d, %r12d
	cmpl	%r13d, %r12d
	cmovll	%r13d, %r12d
	leal	-1(%r12), %esi
	cmpl	$-2049, %esi            # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %esi
	leal	1(%r12), %r13d
	cmpl	$-2049, %r13d           # imm = 0xFFFFFFFFFFFFF7FF
	cmovlel	%r8d, %r13d
	movl	%esi, %ecx
	xorl	$-1, %ecx
	notl	%esi
	cmovsl	%edx, %esi
	notl	%esi
	cmpl	%esi, %eax
	cmovgel	%eax, %esi
	movl	%esi, %ebp
	xorl	%edi, %edi
	movq	%r10, %rsi
	callq	halide_malloc
	xorl	%r9d, %r9d
	movl	104(%rsp), %edi         # 4-byte Reload
	movl	108(%rsp), %r11d        # 4-byte Reload
	movq	%rax, 336(%rsp)         # 8-byte Spill
	movl	296(%rsp), %eax         # 4-byte Reload
	addl	$4, %eax
	sarl	$2, %eax
	movl	%eax, 296(%rsp)         # 4-byte Spill
	movl	324(%rsp), %eax         # 4-byte Reload
	addl	$4, %eax
	sarl	$2, %eax
	movl	%eax, 324(%rsp)         # 4-byte Spill
	movl	300(%rsp), %eax         # 4-byte Reload
	addl	$4, %eax
	sarl	$2, %eax
	movl	%eax, 300(%rsp)         # 4-byte Spill
	movl	332(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, %eax
	notl	%eax
	movl	%eax, 328(%rsp)         # 4-byte Spill
	subl	%ecx, 116(%rsp)         # 4-byte Folded Spill
	movl	%ecx, %eax
	negl	%eax
	movl	%eax, 332(%rsp)         # 4-byte Spill
	movl	76(%rsp), %eax          # 4-byte Reload
	cmpl	%eax, %r14d
	cmovgel	%r14d, %eax
	movl	%eax, 76(%rsp)          # 4-byte Spill
	movl	%eax, %ecx
	notl	%r14d
	movl	%r14d, 188(%rsp)        # 4-byte Spill
	movl	%r15d, %eax
	subl	380(%rsp), %eax         # 4-byte Folded Reload
	incl	%r15d
	movl	%r15d, 100(%rsp)        # 4-byte Spill
	shll	$6, %eax
	movl	%eax, 96(%rsp)          # 4-byte Spill
	movl	%r13d, %eax
	xorl	$-1, %eax
	notl	%r13d
	movl	$0, %edx
	cmovsl	%edx, %r13d
	movl	376(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebx
	notl	%r12d
	movl	%r12d, 184(%rsp)        # 4-byte Spill
	cmovll	%eax, %ebx
	testl	%ebx, %ebx
	cmovsl	%edx, %ebx
	leal	2(%rcx,%rbx), %eax
	movl	%eax, 156(%rsp)         # 4-byte Spill
	movl	384(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebp
	cmovll	%eax, %ebp
	movl	372(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %ebp
	cmovll	%eax, %ebp
	notl	%r13d
	cmpl	%r13d, %ebp
	cmovgel	%ebp, %r13d
	movl	392(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r13d
	cmovll	%eax, %r13d
	movl	400(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r13d
	cmovll	%eax, %r13d
	movl	60(%rsp), %eax          # 4-byte Reload
	cmpl	%eax, %r13d
	cmovll	%eax, %r13d
	notl	%r13d
	movl	%r13d, 180(%rsp)        # 4-byte Spill
	vmovss	.LCPI0_0(%rip), %xmm3
	vmovss	.LCPI0_1(%rip), %xmm4
.LBB0_67:                               #   Parent Loop BB0_45 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_68 Depth 3
                                        #         Child Loop BB0_70 Depth 4
                                        #       Child Loop BB0_73 Depth 3
                                        #         Child Loop BB0_76 Depth 4
                                        #       Child Loop BB0_80 Depth 3
                                        #         Child Loop BB0_82 Depth 4
                                        #       Child Loop BB0_85 Depth 3
                                        #         Child Loop BB0_88 Depth 4
                                        #       Child Loop BB0_92 Depth 3
                                        #         Child Loop BB0_94 Depth 4
                                        #       Child Loop BB0_98 Depth 3
                                        #         Child Loop BB0_101 Depth 4
                                        #       Child Loop BB0_106 Depth 3
                                        #         Child Loop BB0_107 Depth 4
                                        #       Child Loop BB0_111 Depth 3
	movq	%r9, 88(%rsp)           # 8-byte Spill
	movl	%edi, 104(%rsp)         # 4-byte Spill
	movl	%r11d, 108(%rsp)        # 4-byte Spill
	movl	72(%rsp), %esi          # 4-byte Reload
	leal	(%r9,%rsi), %eax
	leal	-2(%r9,%rsi), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	movl	$2047, %r8d             # imm = 0x7FF
	cmovgel	%r8d, %ecx
	cmpl	$2048, %eax             # imm = 0x800
	movl	$2047, %ebp             # imm = 0x7FF
	cmovll	%eax, %ebp
	testl	%ebp, %ebp
	movl	$0, %r15d
	cmovsl	%r15d, %ebp
	testl	%ecx, %ecx
	cmovsl	%r15d, %ecx
	cmpl	$-2049, %edi            # imm = 0xFFFFFFFFFFFFF7FF
	movl	$-2048, %edx            # imm = 0xFFFFFFFFFFFFF800
	cmovgl	%edi, %edx
	leal	1(%r9,%rsi), %ebx
	leal	-1(%r9,%rsi), %r12d
	cmpl	%ebp, %r12d
	cmovgel	%r12d, %ebp
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r8d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r15d, %ebx
	cmpl	$-2049, %r11d           # imm = 0xFFFFFFFFFFFFF7FF
	movl	%edx, %r10d
	notl	%r10d
	movl	$-2048, %esi            # imm = 0xFFFFFFFFFFFFF800
	cmovgl	%r11d, %esi
	cmpl	%ecx, %ebp
	movl	%esi, %r11d
	notl	%r11d
	cmovll	%ecx, %ebp
	xorl	$-1, %esi
	cmovsl	%r15d, %r11d
	xorl	$-1, %edx
	cmovsl	%r15d, %r10d
	cmpl	%ebx, %eax
	movl	%ebx, %ecx
	cmovlel	%eax, %ecx
	cmovgel	%eax, %ebx
	cmpl	$2048, %r12d            # imm = 0x800
	cmovgel	%r8d, %r12d
	testl	%r12d, %r12d
	cmovsl	%r15d, %r12d
	cmpl	%r12d, %ebx
	cmovll	%r12d, %ebx
	leal	1(%rbp), %edx
	movl	%edx, 400(%rsp)         # 4-byte Spill
	movl	100(%rsp), %eax         # 4-byte Reload
	addl	%eax, %r10d
	movl	%r10d, 84(%rsp)         # 4-byte Spill
	addl	%eax, %r11d
	movl	%r11d, 80(%rsp)         # 4-byte Spill
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovll	%edx, %eax
	cmpl	%r12d, %ecx
	cmovlel	%ecx, %r12d
	leal	1(%r12), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r8d, %ecx
	testl	%ecx, %ecx
	cmovsl	%r15d, %ecx
	testl	%eax, %eax
	cmovsl	%r15d, %eax
	cmpl	%eax, %ebp
	cmovgel	%ebp, %eax
	leal	-1(%r12), %r13d
	cmpl	$2048, %r13d            # imm = 0x800
	cmovgel	%r8d, %r13d
	decl	%ebp
	cmpl	%ecx, %r12d
	cmovlel	%r12d, %ecx
	testl	%r13d, %r13d
	cmovsl	%r15d, %r13d
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r8d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r15d, %ebp
	cmpl	%ebp, %eax
	cmovgel	%eax, %ebp
	leal	1(%rbx), %edx
	movl	%edx, 260(%rsp)         # 4-byte Spill
	cmpl	%r13d, %ecx
	cmovlel	%ecx, %r13d
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovll	%edx, %eax
	testl	%eax, %eax
	cmovsl	%r15d, %eax
	cmpl	%eax, %ebx
	cmovgel	%ebx, %eax
	decl	%ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r8d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r15d, %ebx
	cmpl	%ebx, %eax
	cmovgel	%eax, %ebx
	leal	1(%rbp), %r14d
	cmpl	$2048, %r14d            # imm = 0x800
	movl	$2047, %ecx             # imm = 0x7FF
	cmovll	%r14d, %ecx
	testl	%ecx, %ecx
	cmovsl	%r15d, %ecx
	cmpl	%ecx, %ebp
	cmovgel	%ebp, %ecx
	decl	%ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r8d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r15d, %ebp
	cmpl	%ebp, %ecx
	cmovgel	%ecx, %ebp
	leal	1(%r13), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r8d, %ecx
	testl	%ecx, %ecx
	cmovsl	%r15d, %ecx
	cmpl	%ecx, %r13d
	cmovlel	%r13d, %ecx
	leal	-1(%r13), %eax
	cmpl	$2048, %eax             # imm = 0x800
	cmovgel	%r8d, %eax
	testl	%eax, %eax
	cmovsl	%r15d, %eax
	cmpl	%eax, %ecx
	cmovlel	%ecx, %eax
	leal	1(%rbx), %edx
	movl	%edx, 248(%rsp)         # 4-byte Spill
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %ecx             # imm = 0x7FF
	cmovll	%edx, %ecx
	testl	%ecx, %ecx
	cmovsl	%r15d, %ecx
	cmpl	%ecx, %ebx
	cmovgel	%ebx, %ecx
	decl	%ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r8d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r15d, %ebx
	cmpl	%ebx, %ecx
	cmovgel	%ecx, %ebx
	leal	1(%rbp), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	movl	$2047, %edx             # imm = 0x7FF
	cmovll	%ecx, %edx
	testl	%edx, %edx
	cmovsl	%r15d, %edx
	cmpl	%edx, %ebp
	cmovgel	%ebp, %edx
	decl	%ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%r8d, %ebp
	testl	%ebp, %ebp
	cmovsl	%r15d, %ebp
	cmpl	%ebp, %edx
	cmovgel	%edx, %ebp
	leal	1(%rax), %esi
	cmpl	$2048, %esi             # imm = 0x800
	cmovgel	%r8d, %esi
	testl	%esi, %esi
	cmovsl	%r15d, %esi
	cmpl	%esi, %eax
	cmovlel	%eax, %esi
	leal	-1(%rax), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r8d, %edx
	testl	%edx, %edx
	cmovsl	%r15d, %edx
	cmpl	%edx, %esi
	cmovlel	%esi, %edx
	movl	%r11d, %esi
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	movl	%r10d, %esi
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	incl	%ebp
	leal	1(%rbx), %edi
	movl	%edi, 240(%rsp)         # 4-byte Spill
	cmpl	$2048, %edi             # imm = 0x800
	movl	$2047, %esi             # imm = 0x7FF
	cmovll	%edi, %esi
	testl	%esi, %esi
	cmovsl	%r15d, %esi
	testl	%r9d, %r9d
	cmovnel	400(%rsp), %r12d        # 4-byte Folded Reload
	movl	%r12d, 112(%rsp)        # 4-byte Spill
	cmovnel	%r14d, %r13d
	movl	%r13d, 312(%rsp)        # 4-byte Spill
	cmovnel	%ecx, %eax
	movl	%eax, 304(%rsp)         # 4-byte Spill
	cmovel	%edx, %ebp
	movl	%ebp, 372(%rsp)         # 4-byte Spill
	cmpl	%esi, %ebx
	cmovgel	%ebx, %esi
	decl	%ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r8d, %ebx
	testl	%ebx, %ebx
	cmovsl	%r15d, %ebx
	cmpl	%ebx, %esi
	cmovgel	%esi, %ebx
	incl	%ebx
	movl	%ebx, 356(%rsp)         # 4-byte Spill
	cmpl	%ebx, %ebp
	jge	.LBB0_72
.LBB0_68:                               # %.lr.ph756
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_70 Depth 4
	cmpl	$0, 296(%rsp)           # 4-byte Folded Reload
	movq	136(%rsp), %r13         # 8-byte Reload
	movl	$0, %edi
	movl	$2047, %edx             # imm = 0x7FF
	movl	432(%rsp), %r15d        # 4-byte Reload
	movl	452(%rsp), %esi         # 4-byte Reload
	movq	288(%rsp), %r12         # 8-byte Reload
	jle	.LBB0_71
# BB#69:                                # %.lr.ph
                                        #   in Loop: Header=BB0_68 Depth=3
	movl	372(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, %eax
	subl	220(%rsp), %eax         # 4-byte Folded Reload
	imull	216(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 380(%rsp)         # 4-byte Spill
	movq	144(%rsp), %rax         # 8-byte Reload
                                        # kill: EAX<def> EAX<kill> RAX<kill>
	imull	%ecx, %eax
	subl	152(%rsp), %eax         # 4-byte Folded Reload
	movl	180(%rsp), %r14d        # 4-byte Reload
	movl	296(%rsp), %ecx         # 4-byte Reload
	movl	%ecx, 376(%rsp)         # 4-byte Spill
.LBB0_70:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_68 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpl	%esi, %r14d
	movl	452(%rsp), %r10d        # 4-byte Reload
	cmovlel	%r14d, %r10d
	leal	-1(%r10), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%edx, %ecx
	leal	1(%r10), %r11d
	leal	3(%r10), %ebp
	leal	2(%r10), %esi
	leal	4(%r10), %r8d
	cmpl	$2048, %r8d             # imm = 0x800
	cmovgel	%edx, %r8d
	cmpl	$2048, %esi             # imm = 0x800
	cmovgel	%edx, %esi
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%edx, %ebp
	cmpl	$2048, %r10d            # imm = 0x800
	movl	$2047, %ebx             # imm = 0x7FF
	cmovll	%r10d, %ebx
	cmpl	$2048, %r11d            # imm = 0x800
	cmovgel	%edx, %r11d
	testl	%ecx, %ecx
	cmovsl	%edi, %ecx
	addl	%eax, %ecx
	movslq	%ecx, %rcx
	movl	%r10d, %edx
	subl	%r15d, %edx
	testl	%r11d, %r11d
	vmovss	(%r13,%rcx,4), %xmm0
	cmovsl	%edi, %r11d
	leal	(%rax,%r10), %ecx
	testl	%ebx, %ebx
	movslq	%ecx, %rcx
	cmovsl	%edi, %ebx
	testl	%ebp, %ebp
	cmovsl	%edi, %ebp
	testl	%esi, %esi
	cmovsl	%edi, %esi
	testl	%r8d, %r8d
	cmovsl	%edi, %r8d
	vmovss	(%r13,%rcx,4), %xmm1
	movl	380(%rsp), %ecx         # 4-byte Reload
	leal	2(%rdx,%rcx), %edi
	movl	%edi, 400(%rsp)         # 4-byte Spill
	leal	3(%rdx,%rcx), %edi
	movl	%edi, 384(%rsp)         # 4-byte Spill
	leal	1(%rdx,%rcx), %edi
	movl	%edi, 392(%rsp)         # 4-byte Spill
	leal	(%rdx,%rcx), %ecx
	movslq	%ecx, %rcx
	addl	%eax, %r11d
	movslq	%r11d, %r15
	vmulss	(%r13,%r15,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r12,%rcx,4)
	addl	%eax, %r8d
	addl	%eax, %esi
	addl	%eax, %ebp
	leal	1(%rax,%r10), %ecx
	movq	%r12, %r9
	leal	2(%rax,%r10), %r12d
	leal	3(%rax,%r10), %edx
	addl	%eax, %ebx
	movslq	%ebx, %rbx
	vmovss	(%r13,%rbx,4), %xmm0
	addl	$4, %r14d
	movslq	%ecx, %rcx
	vmovss	(%r13,%rcx,4), %xmm1
	decl	376(%rsp)               # 4-byte Folded Spill
	movslq	384(%rsp), %r10         # 4-byte Folded Reload
	movl	$0, %edi
	movslq	%edx, %r11
	movslq	%ebp, %rdx
	movslq	%esi, %rsi
	movslq	%r8d, %r8
	movslq	400(%rsp), %rbx         # 4-byte Folded Reload
	movslq	%r12d, %rcx
	movq	%r9, %r12
	movslq	392(%rsp), %rbp         # 4-byte Folded Reload
	vmulss	(%r13,%rsi,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%r12,%rbp,4)
	vmulss	(%r13,%rdx,4), %xmm4, %xmm1
	movl	$2047, %edx             # imm = 0x7FF
	vmovss	(%r13,%rcx,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r13,%r15,4), %xmm1
	movl	432(%rsp), %r15d        # 4-byte Reload
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r12,%rbx,4)
	vmulss	(%r13,%r8,4), %xmm4, %xmm1
	vmovss	(%r13,%r11,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r13,%rsi,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r12,%r10,4)
	movl	452(%rsp), %esi         # 4-byte Reload
	jne	.LBB0_70
.LBB0_71:                               # %._crit_edge
                                        #   in Loop: Header=BB0_68 Depth=3
	movl	372(%rsp), %eax         # 4-byte Reload
	incl	%eax
	movl	%eax, 372(%rsp)         # 4-byte Spill
	cmpl	356(%rsp), %eax         # 4-byte Folded Reload
	jl	.LBB0_68
.LBB0_72:                               # %.loopexit
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	304(%rsp), %eax         # 4-byte Reload
	cmpl	240(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, %esi
	movl	324(%rsp), %eax         # 4-byte Reload
	movl	420(%rsp), %r10d        # 4-byte Reload
	jge	.LBB0_84
.LBB0_73:                               # %.lr.ph763
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_76 Depth 4
	movl	296(%rsp), %r9d         # 4-byte Reload
	testl	%r9d, %r9d
	movq	288(%rsp), %rdi         # 8-byte Reload
	movq	280(%rsp), %r8          # 8-byte Reload
	jle	.LBB0_74
# BB#75:                                # %.lr.ph759
                                        #   in Loop: Header=BB0_73 Depth=3
	leal	1(%rsi), %eax
	movl	%eax, 176(%rsp)         # 4-byte Spill
	cmpl	$2048, %eax             # imm = 0x800
	movl	$2047, %edx             # imm = 0x7FF
	cmovll	%eax, %edx
	testl	%edx, %edx
	movl	$0, %eax
	cmovsl	%eax, %edx
	leal	-1(%rsi), %ebx
	movl	220(%rsp), %ebp         # 4-byte Reload
	subl	%ebp, %edx
	cmpl	$2048, %ebx             # imm = 0x800
	movl	$2047, %ecx             # imm = 0x7FF
	cmovgel	%ecx, %ebx
	testl	%ebx, %ebx
	cmovsl	%eax, %ebx
	subl	%ebp, %ebx
	movl	%esi, %ecx
	subl	%ebp, %ecx
	movl	216(%rsp), %eax         # 4-byte Reload
	imull	%eax, %ebx
	movl	%ebx, 356(%rsp)         # 4-byte Spill
	imull	%eax, %edx
	movl	%edx, 372(%rsp)         # 4-byte Spill
	imull	%eax, %ecx
	movl	%ecx, 344(%rsp)         # 4-byte Spill
	subl	224(%rsp), %esi         # 4-byte Folded Reload
	imull	212(%rsp), %esi         # 4-byte Folded Reload
	movl	180(%rsp), %r12d        # 4-byte Reload
	movl	%r9d, %r11d
.LBB0_76:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_73 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	452(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r12d
	movl	%eax, %ebx
	cmovlel	%r12d, %ebx
	movl	%ebx, %ebp
	subl	432(%rsp), %ebp         # 4-byte Folded Reload
	movl	356(%rsp), %r15d        # 4-byte Reload
	leal	(%rbp,%r15), %eax
	movl	372(%rsp), %r14d        # 4-byte Reload
	leal	(%rbp,%r14), %edx
	movslq	%edx, %r9
	movslq	%eax, %rdx
	subl	%r10d, %ebx
	leal	(%rbx,%rsi), %eax
	movslq	%eax, %rax
	vmovss	(%rdi,%rdx,4), %xmm0
	vmulss	(%rdi,%r9,4), %xmm4, %xmm1
	movl	344(%rsp), %ecx         # 4-byte Reload
	leal	(%rbp,%rcx), %edx
	movslq	%edx, %rdx
	vmovss	(%rdi,%rdx,4), %xmm2
	vfmadd213ss	%xmm1, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm0
	vmovss	%xmm0, (%r8,%rax,4)
	leal	1(%rbp,%r14), %eax
	movslq	%eax, %rdx
	leal	2(%rbx,%rsi), %eax
	movl	%eax, 384(%rsp)         # 4-byte Spill
	leal	3(%rbp,%r15), %eax
	movl	%eax, 400(%rsp)         # 4-byte Spill
	vmulss	(%rdi,%rdx,4), %xmm4, %xmm0
	leal	3(%rbx,%rsi), %edx
	leal	1(%rbx,%rsi), %eax
	movl	%eax, 380(%rsp)         # 4-byte Spill
	leal	1(%rbp,%r15), %ebx
	leal	2(%rbp,%r15), %eax
	movl	%eax, 376(%rsp)         # 4-byte Spill
	leal	3(%rbp,%rcx), %r10d
	leal	1(%rbp,%rcx), %eax
	leal	2(%rbp,%rcx), %r9d
	leal	3(%rbp,%r14), %r15d
	movq	%r8, %r13
	leal	2(%rbp,%r14), %r8d
	movslq	%ebx, %rbx
	vmovss	(%rdi,%rbx,4), %xmm1
	addl	$4, %r12d
	movslq	%eax, %r14
	movslq	%edx, %rdx
	movslq	400(%rsp), %rax         # 4-byte Folded Reload
	movq	%rax, 400(%rsp)         # 8-byte Spill
	movslq	%r10d, %rax
	movq	%rax, 392(%rsp)         # 8-byte Spill
	movslq	%r15d, %r15
	movslq	384(%rsp), %rcx         # 4-byte Folded Reload
	movslq	376(%rsp), %r10         # 4-byte Folded Reload
	movslq	%r9d, %rax
	movslq	%r8d, %rbp
	movq	%r13, %r8
	movslq	380(%rsp), %rbx         # 4-byte Folded Reload
	vmovss	(%rdi,%r14,4), %xmm2
	decl	%r11d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%rbx,4)
	vmulss	(%rdi,%rbp,4), %xmm4, %xmm1
	vmovss	(%rdi,%rax,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rdi,%r10,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%rcx,4)
	vmulss	(%rdi,%r15,4), %xmm4, %xmm1
	movq	392(%rsp), %rax         # 8-byte Reload
	vmovss	(%rdi,%rax,4), %xmm0
	movl	420(%rsp), %r10d        # 4-byte Reload
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	400(%rsp), %rax         # 8-byte Reload
	vmovss	(%rdi,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%rdx,4)
	jne	.LBB0_76
# BB#77:                                #   in Loop: Header=BB0_73 Depth=3
	movl	176(%rsp), %esi         # 4-byte Reload
	movl	324(%rsp), %eax         # 4-byte Reload
	jmp	.LBB0_78
.LBB0_74:                               # %._crit_edge824
                                        #   in Loop: Header=BB0_73 Depth=3
	incl	%esi
.LBB0_78:                               # %._crit_edge760
                                        #   in Loop: Header=BB0_73 Depth=3
	movq	%r8, 280(%rsp)          # 8-byte Spill
	movq	%rdi, 288(%rsp)         # 8-byte Spill
	cmpl	240(%rsp), %esi         # 4-byte Folded Reload
	jl	.LBB0_73
# BB#79:                                # %.loopexit761
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	240(%rsp), %ecx         # 4-byte Reload
	cmpl	%ecx, 304(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_84
.LBB0_80:                               # %.lr.ph770
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_82 Depth 4
	testl	%eax, %eax
	movl	$0, %edx
	movl	$2047, %ecx             # imm = 0x7FF
	movl	440(%rsp), %ebx         # 4-byte Reload
	movl	424(%rsp), %edi         # 4-byte Reload
	movq	280(%rsp), %r8          # 8-byte Reload
	movq	272(%rsp), %rsi         # 8-byte Reload
	jle	.LBB0_83
# BB#81:                                # %.lr.ph766
                                        #   in Loop: Header=BB0_80 Depth=3
	movl	304(%rsp), %eax         # 4-byte Reload
	movl	%eax, %ebp
	subl	208(%rsp), %ebp         # 4-byte Folded Reload
	imull	204(%rsp), %ebp         # 4-byte Folded Reload
	movl	%ebp, 372(%rsp)         # 4-byte Spill
	subl	224(%rsp), %eax         # 4-byte Folded Reload
	imull	212(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 356(%rsp)         # 4-byte Spill
	movl	%eax, %r9d
	subl	%r10d, %r9d
	movl	184(%rsp), %r11d        # 4-byte Reload
	movl	324(%rsp), %eax         # 4-byte Reload
.LBB0_82:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_80 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	%eax, 400(%rsp)         # 4-byte Spill
	cmpl	%edi, %r11d
	movl	%edi, %eax
	cmovlel	%r11d, %eax
	leal	-1(%rax), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	cmovgel	%ecx, %ebp
	movl	%eax, %edi
	subl	%r10d, %edi
	movl	$2047, %r13d            # imm = 0x7FF
	movl	356(%rsp), %r10d        # 4-byte Reload
	leal	(%rdi,%r10), %ecx
	movslq	%ecx, %rcx
	testl	%ebp, %ebp
	cmovsl	%edx, %ebp
	addl	%r9d, %ebp
	movslq	%ebp, %rdx
	movl	%eax, %r15d
	subl	%ebx, %r15d
	leal	1(%rax), %r14d
	cmpl	$2048, %r14d            # imm = 0x800
	vmovss	(%r8,%rdx,4), %xmm0
	cmovgel	%r13d, %r14d
	leal	4(%rax), %ebp
	cmpl	$2048, %ebp             # imm = 0x800
	vmovss	(%r8,%rcx,4), %xmm1
	cmovgel	%r13d, %ebp
	leal	3(%rax), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r13d, %ebx
	cmpl	$2048, %eax             # imm = 0x800
	movl	$2047, %r12d            # imm = 0x7FF
	cmovll	%eax, %r12d
	leal	2(%rax), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r13d, %ecx
	leal	3(%rdi,%r10), %eax
	movl	%eax, 380(%rsp)         # 4-byte Spill
	leal	1(%rdi,%r10), %r13d
	leal	2(%rdi,%r10), %eax
	movl	%eax, 392(%rsp)         # 4-byte Spill
	movl	372(%rsp), %edx         # 4-byte Reload
	leal	3(%r15,%rdx), %r10d
	leal	1(%r15,%rdx), %eax
	movl	%eax, 384(%rsp)         # 4-byte Spill
	leal	2(%r15,%rdx), %eax
	movl	%eax, 376(%rsp)         # 4-byte Spill
	leal	(%r15,%rdx), %edx
	movslq	%edx, %rdi
	testl	%r14d, %r14d
	movl	$0, %edx
	cmovsl	%edx, %r14d
	addl	%r9d, %r14d
	movslq	%r14d, %r14
	vmulss	(%r8,%r14,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%rsi,%rdi,4)
	testl	%r12d, %r12d
	movl	$0, %edx
	cmovsl	%edx, %r12d
	testl	%ebx, %ebx
	movl	$0, %edx
	cmovsl	%edx, %ebx
	testl	%ecx, %ecx
	movl	$0, %edx
	cmovsl	%edx, %ecx
	testl	%ebp, %ebp
	movl	$0, %edx
	cmovsl	%edx, %ebp
	addl	%r9d, %ebp
	addl	%r9d, %ecx
	addl	%r9d, %ebx
	addl	%r9d, %r12d
	movslq	%r12d, %rdx
	movl	400(%rsp), %eax         # 4-byte Reload
	vmovss	(%r8,%rdx,4), %xmm0
	addl	$4, %r11d
	movslq	%r13d, %rdx
	vmovss	(%r8,%rdx,4), %xmm1
	decl	%eax
	movslq	%r10d, %r10
	movslq	380(%rsp), %r15         # 4-byte Folded Reload
	movslq	%ebx, %rbx
	movslq	%ecx, %rcx
	movslq	%ebp, %r12
	movslq	376(%rsp), %rbp         # 4-byte Folded Reload
	movslq	392(%rsp), %rdx         # 4-byte Folded Reload
	movslq	384(%rsp), %rdi         # 4-byte Folded Reload
	vmulss	(%r8,%rcx,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	vmovss	%xmm0, (%rsi,%rdi,4)
	movl	424(%rsp), %edi         # 4-byte Reload
	vmulss	(%r8,%rbx,4), %xmm4, %xmm1
	vmovss	(%r8,%rdx,4), %xmm0
	movl	$0, %edx
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r8,%r14,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rsi,%rbp,4)
	vmulss	(%r8,%r12,4), %xmm4, %xmm1
	movl	440(%rsp), %ebx         # 4-byte Reload
	vmovss	(%r8,%r15,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r8,%rcx,4), %xmm1
	movl	$2047, %ecx             # imm = 0x7FF
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rsi,%r10,4)
	movl	420(%rsp), %r10d        # 4-byte Reload
	jne	.LBB0_82
.LBB0_83:                               # %._crit_edge767
                                        #   in Loop: Header=BB0_80 Depth=3
	movl	304(%rsp), %eax         # 4-byte Reload
	incl	%eax
	movl	%eax, 304(%rsp)         # 4-byte Spill
	cmpl	240(%rsp), %eax         # 4-byte Folded Reload
	movl	324(%rsp), %eax         # 4-byte Reload
	jl	.LBB0_80
.LBB0_84:                               # %.loopexit768
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	312(%rsp), %eax         # 4-byte Reload
	cmpl	248(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, %edi
	movl	448(%rsp), %ebx         # 4-byte Reload
	jge	.LBB0_96
.LBB0_85:                               # %.lr.ph778
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_88 Depth 4
	movl	324(%rsp), %r9d         # 4-byte Reload
	testl	%r9d, %r9d
	movq	272(%rsp), %rbp         # 8-byte Reload
	movq	264(%rsp), %r8          # 8-byte Reload
	jle	.LBB0_86
# BB#87:                                # %.lr.ph773
                                        #   in Loop: Header=BB0_85 Depth=3
	leal	1(%rdi), %eax
	movl	%eax, 304(%rsp)         # 4-byte Spill
	cmpl	$2048, %eax             # imm = 0x800
	movl	$2047, %r12d            # imm = 0x7FF
	cmovll	%eax, %r12d
	testl	%r12d, %r12d
	movl	$0, %eax
	cmovsl	%eax, %r12d
	leal	-1(%rdi), %edx
	movl	208(%rsp), %esi         # 4-byte Reload
	subl	%esi, %r12d
	cmpl	$2048, %edx             # imm = 0x800
	movl	$2047, %ecx             # imm = 0x7FF
	cmovgel	%ecx, %edx
	testl	%edx, %edx
	cmovsl	%eax, %edx
	subl	%esi, %edx
	movl	%edi, %ecx
	subl	%esi, %ecx
	movl	204(%rsp), %eax         # 4-byte Reload
	imull	%eax, %edx
	movl	%edx, 356(%rsp)         # 4-byte Spill
	imull	%eax, %r12d
	imull	%eax, %ecx
	movl	%ecx, 344(%rsp)         # 4-byte Spill
	subl	232(%rsp), %edi         # 4-byte Folded Reload
	imull	200(%rsp), %edi         # 4-byte Folded Reload
	movl	184(%rsp), %r10d        # 4-byte Reload
	movl	%r9d, %r15d
.LBB0_88:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_85 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	424(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r10d
	cmovlel	%r10d, %eax
	movl	%eax, %ebx
	subl	440(%rsp), %ebx         # 4-byte Folded Reload
	movl	356(%rsp), %r11d        # 4-byte Reload
	leal	(%rbx,%r11), %edx
	leal	(%rbx,%r12), %esi
	movslq	%esi, %r9
	movslq	%edx, %rsi
	subl	448(%rsp), %eax         # 4-byte Folded Reload
	leal	(%rax,%rdi), %edx
	movslq	%edx, %rdx
	vmovss	(%rbp,%rsi,4), %xmm0
	vmulss	(%rbp,%r9,4), %xmm4, %xmm1
	movl	344(%rsp), %ecx         # 4-byte Reload
	leal	(%rbx,%rcx), %esi
	movslq	%esi, %rsi
	vmovss	(%rbp,%rsi,4), %xmm2
	vfmadd213ss	%xmm1, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm0
	vmovss	%xmm0, (%r8,%rdx,4)
	leal	1(%rbx,%r12), %edx
	movslq	%edx, %rdx
	leal	2(%rax,%rdi), %esi
	movl	%esi, 380(%rsp)         # 4-byte Spill
	leal	3(%rbx,%r11), %esi
	movl	%esi, 392(%rsp)         # 4-byte Spill
	vmulss	(%rbp,%rdx,4), %xmm4, %xmm0
	leal	3(%rax,%rdi), %edx
	leal	1(%rax,%rdi), %eax
	movl	%eax, 376(%rsp)         # 4-byte Spill
	leal	1(%rbx,%r11), %esi
	leal	2(%rbx,%r11), %eax
	movl	%eax, 372(%rsp)         # 4-byte Spill
	leal	3(%rbx,%rcx), %r13d
	leal	1(%rbx,%rcx), %eax
	leal	2(%rbx,%rcx), %r9d
	leal	3(%rbx,%r12), %r11d
	leal	2(%rbx,%r12), %ecx
	movslq	%esi, %rsi
	vmovss	(%rbp,%rsi,4), %xmm1
	addl	$4, %r10d
	movslq	%eax, %r14
	movslq	%edx, %rax
	movq	%rax, 400(%rsp)         # 8-byte Spill
	movslq	392(%rsp), %rax         # 4-byte Folded Reload
	movq	%rax, 392(%rsp)         # 8-byte Spill
	movslq	%r13d, %rax
	movq	%rax, 384(%rsp)         # 8-byte Spill
	movslq	%r11d, %r11
	movslq	380(%rsp), %r13         # 4-byte Folded Reload
	movslq	372(%rsp), %rax         # 4-byte Folded Reload
	movslq	%r9d, %rdx
	movslq	%ecx, %rbx
	movslq	376(%rsp), %rsi         # 4-byte Folded Reload
	vmovss	(%rbp,%r14,4), %xmm2
	decl	%r15d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%rsi,4)
	vmulss	(%rbp,%rbx,4), %xmm4, %xmm1
	vmovss	(%rbp,%rdx,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rbp,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%r13,4)
	vmulss	(%rbp,%r11,4), %xmm4, %xmm1
	movq	384(%rsp), %rax         # 8-byte Reload
	vmovss	(%rbp,%rax,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	392(%rsp), %rax         # 8-byte Reload
	vmovss	(%rbp,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	400(%rsp), %rax         # 8-byte Reload
	vmovss	%xmm1, (%r8,%rax,4)
	jne	.LBB0_88
# BB#89:                                #   in Loop: Header=BB0_85 Depth=3
	movl	304(%rsp), %edi         # 4-byte Reload
	movl	448(%rsp), %ebx         # 4-byte Reload
	jmp	.LBB0_90
.LBB0_86:                               # %._crit_edge823
                                        #   in Loop: Header=BB0_85 Depth=3
	incl	%edi
.LBB0_90:                               # %._crit_edge774
                                        #   in Loop: Header=BB0_85 Depth=3
	movq	%r8, 264(%rsp)          # 8-byte Spill
	movq	%rbp, 272(%rsp)         # 8-byte Spill
	cmpl	248(%rsp), %edi         # 4-byte Folded Reload
	jl	.LBB0_85
# BB#91:                                # %.loopexit775
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	248(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, 312(%rsp)         # 4-byte Folded Reload
	jge	.LBB0_96
.LBB0_92:                               # %.lr.ph786
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_94 Depth 4
	cmpl	$0, 300(%rsp)           # 4-byte Folded Reload
	movl	$0, %esi
	movl	$2047, %r12d            # imm = 0x7FF
	movl	456(%rsp), %ecx         # 4-byte Reload
	movq	264(%rsp), %r13         # 8-byte Reload
	jle	.LBB0_95
# BB#93:                                # %.lr.ph782
                                        #   in Loop: Header=BB0_92 Depth=3
	movl	312(%rsp), %eax         # 4-byte Reload
	movl	%eax, %edx
	subl	196(%rsp), %edx         # 4-byte Folded Reload
	imull	192(%rsp), %edx         # 4-byte Folded Reload
	movl	%edx, 376(%rsp)         # 4-byte Spill
	subl	232(%rsp), %eax         # 4-byte Folded Reload
	imull	200(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 372(%rsp)         # 4-byte Spill
	movl	%eax, %r9d
	subl	%ebx, %r9d
	movl	188(%rsp), %r15d        # 4-byte Reload
	movl	300(%rsp), %r11d        # 4-byte Reload
.LBB0_94:                               #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_92 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpl	%ecx, %r15d
	movl	456(%rsp), %ecx         # 4-byte Reload
	cmovlel	%r15d, %ecx
	leal	-1(%rcx), %edx
	cmpl	$2048, %edx             # imm = 0x800
	cmovgel	%r12d, %edx
	movl	%ecx, %edi
	subl	%ebx, %edi
	movl	372(%rsp), %r10d        # 4-byte Reload
	leal	(%rdi,%r10), %eax
	movslq	%eax, %rax
	testl	%edx, %edx
	cmovsl	%esi, %edx
	addl	%r9d, %edx
	movslq	%edx, %rbp
	movl	$0, %r8d
	movl	%ecx, %esi
	subl	408(%rsp), %esi         # 4-byte Folded Reload
	leal	1(%rcx), %edx
	cmpl	$2048, %edx             # imm = 0x800
	vmovss	(%r13,%rbp,4), %xmm0
	cmovgel	%r12d, %edx
	leal	4(%rcx), %r14d
	cmpl	$2048, %r14d            # imm = 0x800
	vmovss	(%r13,%rax,4), %xmm1
	cmovgel	%r12d, %r14d
	leal	3(%rcx), %ebx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%r12d, %ebx
	cmpl	$2048, %ecx             # imm = 0x800
	movl	$2047, %eax             # imm = 0x7FF
	cmovll	%ecx, %eax
	leal	2(%rcx), %ecx
	cmpl	$2048, %ecx             # imm = 0x800
	cmovgel	%r12d, %ecx
	leal	3(%rdi,%r10), %ebp
	movl	%ebp, 384(%rsp)         # 4-byte Spill
	leal	1(%rdi,%r10), %ebp
	movl	%ebp, 380(%rsp)         # 4-byte Spill
	leal	2(%rdi,%r10), %edi
	movl	%edi, 400(%rsp)         # 4-byte Spill
	movl	376(%rsp), %edi         # 4-byte Reload
	leal	3(%rsi,%rdi), %r10d
	leal	1(%rsi,%rdi), %ebp
	movl	%ebp, 392(%rsp)         # 4-byte Spill
	leal	2(%rsi,%rdi), %ebp
	leal	(%rsi,%rdi), %esi
	movslq	%esi, %rsi
	testl	%edx, %edx
	cmovsl	%r8d, %edx
	addl	%r9d, %edx
	movslq	%edx, %r12
	vmulss	(%r13,%r12,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	movq	464(%rsp), %rdx         # 8-byte Reload
	vmovss	%xmm0, (%rdx,%rsi,4)
	testl	%eax, %eax
	cmovsl	%r8d, %eax
	testl	%ebx, %ebx
	cmovsl	%r8d, %ebx
	testl	%ecx, %ecx
	cmovsl	%r8d, %ecx
	testl	%r14d, %r14d
	cmovsl	%r8d, %r14d
	addl	%r9d, %r14d
	addl	%r9d, %ecx
	addl	%r9d, %ebx
	addl	%r9d, %eax
	movslq	%eax, %rax
	vmovss	(%r13,%rax,4), %xmm0
	addl	$4, %r15d
	movslq	380(%rsp), %rax         # 4-byte Folded Reload
	vmovss	(%r13,%rax,4), %xmm1
	decl	%r11d
	movslq	%r10d, %r10
	movslq	384(%rsp), %r8          # 4-byte Folded Reload
	movslq	%ebx, %rbx
	movslq	%ecx, %rcx
	movslq	%r14d, %rsi
	movslq	%ebp, %rdi
	movslq	400(%rsp), %rbp         # 4-byte Folded Reload
	movslq	392(%rsp), %rax         # 4-byte Folded Reload
	vmulss	(%r13,%rcx,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm1
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	movq	464(%rsp), %rdx         # 8-byte Reload
	vmovss	%xmm0, (%rdx,%rax,4)
	vmulss	(%r13,%rbx,4), %xmm4, %xmm1
	vmovss	(%r13,%rbp,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r13,%r12,4), %xmm1
	movl	$2047, %r12d            # imm = 0x7FF
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	464(%rsp), %rax         # 8-byte Reload
	vmovss	%xmm1, (%rax,%rdi,4)
	vmulss	(%r13,%rsi,4), %xmm4, %xmm1
	movl	$0, %esi
	vmovss	(%r13,%r8,4), %xmm0
	movq	464(%rsp), %rax         # 8-byte Reload
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r13,%rcx,4), %xmm1
	movl	456(%rsp), %ecx         # 4-byte Reload
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rax,%r10,4)
	movl	448(%rsp), %ebx         # 4-byte Reload
	jne	.LBB0_94
.LBB0_95:                               # %._crit_edge783
                                        #   in Loop: Header=BB0_92 Depth=3
	movl	312(%rsp), %eax         # 4-byte Reload
	incl	%eax
	movl	%eax, 312(%rsp)         # 4-byte Spill
	cmpl	248(%rsp), %eax         # 4-byte Folded Reload
	jl	.LBB0_92
.LBB0_96:                               # %.loopexit784
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	80(%rsp), %edx          # 4-byte Reload
	shll	$6, %edx
	movl	84(%rsp), %eax          # 4-byte Reload
	shll	$6, %eax
	movslq	96(%rsp), %r13          # 4-byte Folded Reload
	shlq	$38, 120(%rsp)          # 8-byte Folded Spill
	shlq	$38, 128(%rsp)          # 8-byte Folded Spill
	movslq	%eax, %rax
	movq	336(%rsp), %rcx         # 8-byte Reload
	leaq	(%rcx,%rax,4), %rsi
	movq	%rsi, 312(%rsp)         # 8-byte Spill
	movslq	%edx, %rax
	leaq	(%rcx,%rax,4), %rdi
	movq	%rdi, 304(%rsp)         # 8-byte Spill
	leaq	(%rcx,%r13,4), %rbx
	movq	%rbx, 248(%rsp)         # 8-byte Spill
	shlq	$32, %r13
	movq	%r13, 240(%rsp)         # 8-byte Spill
	movl	112(%rsp), %eax         # 4-byte Reload
	cmpl	260(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, %ebp
	movq	160(%rsp), %r9          # 8-byte Reload
	movl	$0, %eax
	movabsq	$17179869184, %r8       # imm = 0x400000000
	movl	300(%rsp), %edx         # 4-byte Reload
	movq	464(%rsp), %r15         # 8-byte Reload
	jge	.LBB0_97
.LBB0_98:                               # %.lr.ph794
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_101 Depth 4
	testl	%edx, %edx
	jle	.LBB0_99
# BB#100:                               # %.lr.ph789
                                        #   in Loop: Header=BB0_98 Depth=3
	leal	1(%rbp), %ecx
	movl	%ecx, 176(%rsp)         # 4-byte Spill
	cmpl	$2048, %ecx             # imm = 0x800
	movl	$2047, %r12d            # imm = 0x7FF
	cmovll	%ecx, %r12d
	testl	%r12d, %r12d
	cmovsl	%eax, %r12d
	leal	-1(%rbp), %edi
	movl	196(%rsp), %esi         # 4-byte Reload
	subl	%esi, %r12d
	cmpl	$2048, %edi             # imm = 0x800
	movl	$2047, %ecx             # imm = 0x7FF
	cmovgel	%ecx, %edi
	testl	%edi, %edi
	cmovsl	%eax, %edi
	subl	%esi, %edi
	movl	%ebp, %ecx
	subl	%esi, %ecx
	movl	192(%rsp), %eax         # 4-byte Reload
	imull	%eax, %edi
	movl	%edi, 372(%rsp)         # 4-byte Spill
	imull	%eax, %r12d
	imull	%eax, %ecx
	movl	%ecx, 356(%rsp)         # 4-byte Spill
	subl	236(%rsp), %ebp         # 4-byte Folded Reload
	imull	228(%rsp), %ebp         # 4-byte Folded Reload
	movl	%ebp, 344(%rsp)         # 4-byte Spill
	movl	188(%rsp), %r10d        # 4-byte Reload
	movl	%edx, %r13d
	movq	168(%rsp), %r9          # 8-byte Reload
.LBB0_101:                              #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_98 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	456(%rsp), %eax         # 4-byte Reload
	cmpl	%eax, %r10d
	cmovlel	%r10d, %eax
	movl	%eax, %ebx
	subl	408(%rsp), %ebx         # 4-byte Folded Reload
	movl	372(%rsp), %edi         # 4-byte Reload
	leal	(%rbx,%rdi), %edx
	leal	(%rbx,%r12), %esi
	movslq	%esi, %r8
	movslq	%edx, %rsi
	subl	360(%rsp), %eax         # 4-byte Folded Reload
	leal	(%rax,%rbp), %edx
	movslq	%edx, %rdx
	vmovss	(%r15,%rsi,4), %xmm0
	vmulss	(%r15,%r8,4), %xmm4, %xmm1
	movl	356(%rsp), %ecx         # 4-byte Reload
	leal	(%rbx,%rcx), %esi
	movslq	%esi, %rsi
	vmovss	(%r15,%rsi,4), %xmm2
	vfmadd213ss	%xmm1, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm0
	movq	%r9, %r8
	vmovss	%xmm0, (%r8,%rdx,4)
	leal	1(%rbx,%r12), %edx
	movslq	%edx, %rdx
	leal	2(%rax,%rbp), %esi
	movl	%esi, 384(%rsp)         # 4-byte Spill
	leal	3(%rbx,%rdi), %esi
	movl	%esi, 392(%rsp)         # 4-byte Spill
	vmulss	(%r15,%rdx,4), %xmm4, %xmm0
	leal	3(%rax,%rbp), %edx
	leal	1(%rax,%rbp), %eax
	movl	%eax, 380(%rsp)         # 4-byte Spill
	leal	1(%rbx,%rdi), %eax
	leal	2(%rbx,%rdi), %esi
	movl	%esi, 376(%rsp)         # 4-byte Spill
	leal	3(%rbx,%rcx), %r9d
	leal	1(%rbx,%rcx), %esi
	leal	2(%rbx,%rcx), %ebp
	leal	3(%rbx,%r12), %r11d
	leal	2(%rbx,%r12), %ecx
	movslq	%eax, %rdi
	vmovss	(%r15,%rdi,4), %xmm1
	addl	$4, %r10d
	movslq	%esi, %r14
	movslq	%edx, %rax
	movq	%rax, 400(%rsp)         # 8-byte Spill
	movslq	392(%rsp), %rax         # 4-byte Folded Reload
	movq	%rax, 392(%rsp)         # 8-byte Spill
	movslq	%r9d, %r9
	movslq	%r11d, %r11
	movslq	384(%rsp), %rdx         # 4-byte Folded Reload
	movslq	376(%rsp), %rax         # 4-byte Folded Reload
	movslq	%ebp, %rbx
	movl	344(%rsp), %ebp         # 4-byte Reload
	movslq	%ecx, %rdi
	movslq	380(%rsp), %rsi         # 4-byte Folded Reload
	vmovss	(%r15,%r14,4), %xmm2
	decl	%r13d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%rsi,4)
	vmulss	(%r15,%rdi,4), %xmm4, %xmm1
	vmovss	(%r15,%rbx,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r15,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r8,%rdx,4)
	vmulss	(%r15,%r11,4), %xmm4, %xmm1
	vmovss	(%r15,%r9,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movq	392(%rsp), %rax         # 8-byte Reload
	vmovss	(%r15,%rax,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	movq	400(%rsp), %rax         # 8-byte Reload
	vmovss	%xmm1, (%r8,%rax,4)
	movq	%r8, %r9
	jne	.LBB0_101
# BB#102:                               #   in Loop: Header=BB0_98 Depth=3
	movl	176(%rsp), %ebp         # 4-byte Reload
	movq	160(%rsp), %r9          # 8-byte Reload
	xorl	%eax, %eax
	movabsq	$17179869184, %r8       # imm = 0x400000000
	movl	300(%rsp), %edx         # 4-byte Reload
	movq	240(%rsp), %r13         # 8-byte Reload
	movq	312(%rsp), %rsi         # 8-byte Reload
	movq	304(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_103
.LBB0_99:                               # %._crit_edge822
                                        #   in Loop: Header=BB0_98 Depth=3
	incl	%ebp
.LBB0_103:                              # %._crit_edge790
                                        #   in Loop: Header=BB0_98 Depth=3
	cmpl	260(%rsp), %ebp         # 4-byte Folded Reload
	jl	.LBB0_98
# BB#104:                               # %.loopexit791
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	112(%rsp), %ecx         # 4-byte Reload
	cmpl	260(%rsp), %ecx         # 4-byte Folded Reload
	jge	.LBB0_97
# BB#105:                               # %.preheader.lr.ph
                                        #   in Loop: Header=BB0_67 Depth=2
	movq	%rbx, 248(%rsp)         # 8-byte Spill
	movq	%rdi, 304(%rsp)         # 8-byte Spill
	movq	%rsi, 312(%rsp)         # 8-byte Spill
	movl	100(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%rcx), %edx
	movl	156(%rsp), %esi         # 4-byte Reload
	imull	%edx, %esi
	addl	76(%rsp), %esi          # 4-byte Folded Reload
	shll	$6, %edx
	movslq	%ecx, %rdi
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	336(%rsp), %rax         # 8-byte Reload
.LBB0_106:                              # %.preheader
                                        #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_107 Depth 4
	movq	%rdi, 344(%rsp)         # 8-byte Spill
	movl	%esi, 356(%rsp)         # 4-byte Spill
	movl	%edx, 372(%rsp)         # 4-byte Spill
	movslq	%edx, %rcx
	leaq	(%rax,%rcx,4), %r10
	movl	%edi, %eax
	subl	236(%rsp), %eax         # 4-byte Folded Reload
	imull	228(%rsp), %eax         # 4-byte Folded Reload
	subl	360(%rsp), %eax         # 4-byte Folded Reload
	movl	%eax, 376(%rsp)         # 4-byte Spill
	shlq	$32, %rcx
	xorl	%r9d, %r9d
	movl	%esi, %r13d
.LBB0_107:                              #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        #       Parent Loop BB0_106 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	328(%rsp), %eax         # 4-byte Reload
	leal	(%rax,%r9), %r14d
	leal	-1(%rax,%r9), %edx
	cmpl	$2047, %edx             # imm = 0x7FF
	movl	$2047, %edi             # imm = 0x7FF
	cmovgel	%edi, %edx
	cmpl	$2047, %r14d            # imm = 0x7FF
	cmovgel	%edi, %r14d
	movl	332(%rsp), %ebp         # 4-byte Reload
	leal	1(%rbp,%r9), %esi
	leal	2(%rbp,%r9), %r8d
	leal	3(%rbp,%r9), %r12d
	leal	(%rbp,%r9), %ebx
	testl	%edx, %edx
	movl	$0, %eax
	cmovsl	%eax, %edx
	cmpl	$2048, %ebx             # imm = 0x800
	cmovgel	%edi, %ebx
	cmpl	$2048, %r12d            # imm = 0x800
	cmovgel	%edi, %r12d
	cmpl	$2048, %r8d             # imm = 0x800
	cmovgel	%edi, %r8d
	cmpl	$2048, %esi             # imm = 0x800
	cmovgel	%edi, %esi
	testl	%r14d, %r14d
	cmovsl	%eax, %r14d
	testl	%esi, %esi
	cmovsl	%eax, %esi
	testl	%r8d, %r8d
	cmovsl	%eax, %r8d
	testl	%r12d, %r12d
	cmovsl	%eax, %r12d
	movl	376(%rsp), %edi         # 4-byte Reload
	addl	%edi, %edx
	testl	%ebx, %ebx
	cmovsl	%eax, %ebx
	leal	(%rbp,%r13), %eax
	movslq	%eax, %r11
	addl	%edi, %ebx
	movslq	%edx, %rdx
	vmovss	(%r15,%r11,4), %xmm0
	addl	%edi, %r12d
	addl	%edi, %r8d
	addl	%edi, %esi
	addl	%edi, %r14d
	leal	3(%rbp,%r13), %eax
	movl	%eax, 384(%rsp)         # 4-byte Spill
	leal	2(%rbp,%r13), %eax
	movl	%eax, 380(%rsp)         # 4-byte Spill
	leal	1(%rbp,%r13), %eax
	addl	$4, %r13d
	vmovss	(%r15,%rdx,4), %xmm1
	movslq	%ebx, %rbx
	vmulss	(%r15,%rbx,4), %xmm4, %xmm2
	vfmadd213ss	%xmm2, %xmm3, %xmm0
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r10)
	addq	$16, %r10
	movq	%r10, 400(%rsp)         # 8-byte Spill
	addl	$4, %r9d
	movabsq	$17179869184, %rdx      # imm = 0x400000000
	leaq	(%rcx,%rdx), %rdx
	movq	%rdx, 392(%rsp)         # 8-byte Spill
	sarq	$32, %rcx
	shlq	$2, %rcx
	movq	%rcx, %r10
	orq	$4, %r10
	movq	%rcx, %r11
	orq	$8, %r11
	orq	$12, %rcx
	cmpl	$64, %r9d
	movslq	%esi, %rsi
	movl	%r13d, %edi
	movslq	%eax, %r13
	movslq	%r8d, %rbp
	movslq	%r12d, %r8
	movslq	380(%rsp), %rdx         # 4-byte Folded Reload
	vmovss	(%r15,%r13,4), %xmm0
	movl	%edi, %r13d
	movslq	384(%rsp), %rdi         # 4-byte Folded Reload
	vmulss	(%r15,%rsi,4), %xmm4, %xmm1
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	movslq	%r14d, %rax
	vmovss	(%r15,%rax,4), %xmm1
	movq	336(%rsp), %rax         # 8-byte Reload
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rax,%r10)
	movq	400(%rsp), %r10         # 8-byte Reload
	vmulss	(%r15,%rbp,4), %xmm4, %xmm1
	vmovss	(%r15,%rdx,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r15,%rbx,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rax,%r11)
	vmulss	(%r15,%r8,4), %xmm4, %xmm1
	vmovss	(%r15,%rdi,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%r15,%rsi,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%rax,%rcx)
	movq	392(%rsp), %rcx         # 8-byte Reload
	jne	.LBB0_107
# BB#108:                               #   in Loop: Header=BB0_106 Depth=3
	movl	356(%rsp), %esi         # 4-byte Reload
	addl	156(%rsp), %esi         # 4-byte Folded Reload
	movl	372(%rsp), %edx         # 4-byte Reload
	addl	$64, %edx
	movq	344(%rsp), %rdi         # 8-byte Reload
	incq	%rdi
	cmpl	260(%rsp), %edi         # 4-byte Folded Reload
	movq	240(%rsp), %r13         # 8-byte Reload
	jl	.LBB0_106
# BB#109:                               #   in Loop: Header=BB0_67 Depth=2
	movq	%r15, 168(%rsp)         # 8-byte Spill
	movl	116(%rsp), %r11d        # 4-byte Reload
	movq	160(%rsp), %r9          # 8-byte Reload
	movabsq	$17179869184, %r8       # imm = 0x400000000
	movq	464(%rsp), %r15         # 8-byte Reload
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	312(%rsp), %rsi         # 8-byte Reload
	movq	304(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_110
.LBB0_97:                               #   in Loop: Header=BB0_67 Depth=2
	movl	116(%rsp), %r11d        # 4-byte Reload
	movq	336(%rsp), %rax         # 8-byte Reload
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	120(%rsp), %r12         # 8-byte Reload
.LBB0_110:                              #   in Loop: Header=BB0_67 Depth=2
	xorl	%r10d, %r10d
.LBB0_111:                              #   Parent Loop BB0_45 Depth=1
                                        #     Parent Loop BB0_67 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	vmulss	(%rsi,%r10,4), %xmm4, %xmm1
	vmovss	(%rbx,%r10,4), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rdi,%r10,4), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	leal	-1(%r11), %ecx
	movslq	%ecx, %rcx
	vmovss	%xmm1, (%r9,%rcx,4)
	movq	%r12, %rcx
	sarq	$32, %rcx
	shlq	$2, %rcx
	movq	%rcx, %rdx
	orq	$4, %rdx
	vmulss	(%rax,%rdx), %xmm4, %xmm0
	movq	%r13, %rdx
	sarq	$32, %rdx
	shlq	$2, %rdx
	movq	%rdx, %rsi
	orq	$4, %rsi
	vmovss	(%rax,%rsi), %xmm1
	vfmadd213ss	%xmm0, %xmm3, %xmm1
	movq	%r14, %rsi
	sarq	$32, %rsi
	shlq	$2, %rsi
	movq	%rsi, %rdi
	orq	$4, %rdi
	vmovss	(%rax,%rdi), %xmm0
	vfmadd213ss	%xmm1, %xmm4, %xmm0
	addq	%r8, %r13
	addq	%r8, %r12
	movslq	%r11d, %rbx
	addq	%r8, %r14
	addq	$4, %r10
	leal	4(%rbx), %r11d
	vmovss	%xmm0, (%r9,%rbx,4)
	leal	2(%rbx), %edi
	incl	%ebx
	movslq	%edi, %r8
	movq	%rcx, %rdi
	orq	$8, %rdi
	vmulss	(%rax,%rdi), %xmm4, %xmm0
	movq	%rsi, %rbp
	orq	$12, %rbp
	movq	%rdx, %rdi
	orq	$12, %rdi
	orq	$12, %rcx
	movslq	%ebx, %rbx
	orq	$8, %rsi
	vmovss	(%rax,%rsi), %xmm1
	orq	$8, %rdx
	vmovss	(%rax,%rdx), %xmm2
	cmpl	$64, %r10d
	vfmadd213ss	%xmm0, %xmm3, %xmm2
	vfmadd213ss	%xmm2, %xmm4, %xmm1
	vmovss	%xmm1, (%r9,%rbx,4)
	vmulss	(%rax,%rcx), %xmm4, %xmm1
	vmovss	(%rax,%rdi), %xmm0
	vfmadd213ss	%xmm1, %xmm3, %xmm0
	vmovss	(%rax,%rbp), %xmm1
	vfmadd213ss	%xmm0, %xmm4, %xmm1
	vmovss	%xmm1, (%r9,%r8,4)
	movabsq	$17179869184, %r8       # imm = 0x400000000
	movq	248(%rsp), %rbx         # 8-byte Reload
	movq	304(%rsp), %rdi         # 8-byte Reload
	movq	312(%rsp), %rsi         # 8-byte Reload
	jne	.LBB0_111
# BB#112:                               #   in Loop: Header=BB0_67 Depth=2
	movq	%rax, 336(%rsp)         # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	addl	%eax, 116(%rsp)         # 4-byte Folded Spill
	addl	$64, 96(%rsp)           # 4-byte Folded Spill
	movl	104(%rsp), %edi         # 4-byte Reload
	decl	%edi
	movl	108(%rsp), %r11d        # 4-byte Reload
	decl	%r11d
	movq	88(%rsp), %r9           # 8-byte Reload
	incq	%r9
	cmpl	$64, %r9d
	jne	.LBB0_67
# BB#113:                               #   in Loop: Header=BB0_45 Depth=1
	xorl	%edi, %edi
	movq	288(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	280(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	272(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	264(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	%r15, %rsi
	callq	halide_free
	xorl	%edi, %edi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	xorl	%edi, %edi
	movq	336(%rsp), %rsi         # 8-byte Reload
	callq	halide_free
	movl	56(%rsp), %ebx          # 4-byte Reload
	incl	%ebx
	xorl	%eax, %eax
	cmpl	52(%rsp), %ebx          # 4-byte Folded Reload
	movl	48(%rsp), %r10d         # 4-byte Reload
	movl	36(%rsp), %ebp          # 4-byte Reload
	movl	$0, %r8d
	movl	$2047, %r9d             # imm = 0x7FF
	jne	.LBB0_45
	jmp	.LBB0_114
.LBB0_30:
	leal	-1(%rcx,%rdi), %ecx
	xorl	%edi, %edi
	movl	$.L.str9, %esi
	movl	%r13d, %edx
.LBB0_28:
	xorb	%al, %al
	callq	halide_printf
	movl	$-1, %eax
.LBB0_114:                              # %.loopexit802
	addq	$472, %rsp              # imm = 0x1D8
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
.LBB0_115:
	xorl	%edi, %edi
	movl	$.L.str15, %esi
.LBB0_33:
	xorb	%al, %al
	callq	halide_printf
	movl	$-1, %eax
	jmp	.LBB0_114
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
