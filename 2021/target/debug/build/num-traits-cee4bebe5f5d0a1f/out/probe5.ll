; ModuleID = 'probe5.34344da6fcc24e5d-cgu.0'
source_filename = "probe5.34344da6fcc24e5d-cgu.0"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; core::f64::<impl f64>::is_subnormal
; Function Attrs: inlinehint nonlazybind uwtable
define internal zeroext i1 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$12is_subnormal17h37befde07d28364dE"(double %self) unnamed_addr #0 {
start:
  %_2 = alloca i8, align 1
; call core::f64::<impl f64>::classify
  %0 = call i8 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$8classify17h4b86ee7d8f7974a5E"(double %self), !range !2
  store i8 %0, ptr %_2, align 1
  %1 = load i8, ptr %_2, align 1, !range !2, !noundef !3
  %_3 = zext i8 %1 to i64
  %2 = icmp eq i64 %_3, 3
  ret i1 %2
}

; probe5::probe
; Function Attrs: nonlazybind uwtable
define void @_ZN6probe55probe17h60e6f74554da1bf3E() unnamed_addr #1 {
start:
; call core::f64::<impl f64>::is_subnormal
  %_1 = call zeroext i1 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$12is_subnormal17h37befde07d28364dE"(double 1.000000e+00)
  ret void
}

; core::f64::<impl f64>::classify
; Function Attrs: nonlazybind uwtable
declare i8 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$8classify17h4b86ee7d8f7974a5E"(double) unnamed_addr #1

attributes #0 = { inlinehint nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }
attributes #1 = { nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 2, !"RtLibUseGOT", i32 1}
!2 = !{i8 0, i8 5}
!3 = !{}
