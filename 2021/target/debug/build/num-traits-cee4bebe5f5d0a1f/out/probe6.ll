; ModuleID = 'probe6.4cfde88763a71e9a-cgu.0'
source_filename = "probe6.4cfde88763a71e9a-cgu.0"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; core::num::<impl u32>::to_ne_bytes
; Function Attrs: inlinehint nonlazybind uwtable
define internal i32 @"_ZN4core3num21_$LT$impl$u20$u32$GT$11to_ne_bytes17hd05115008fbc8e7cE"(i32 %self) unnamed_addr #0 {
start:
  %0 = alloca [4 x i8], align 1
  store i32 %self, ptr %0, align 1
  %1 = load i32, ptr %0, align 1
  ret i32 %1
}

; probe6::probe
; Function Attrs: nonlazybind uwtable
define void @_ZN6probe65probe17hcfb2f442e3dd438dE() unnamed_addr #1 {
start:
  %0 = alloca i32, align 4
  %_1 = alloca [4 x i8], align 1
; call core::num::<impl u32>::to_ne_bytes
  %1 = call i32 @"_ZN4core3num21_$LT$impl$u20$u32$GT$11to_ne_bytes17hd05115008fbc8e7cE"(i32 1)
  store i32 %1, ptr %0, align 4
  call void @llvm.memcpy.p0.p0.i64(ptr align 1 %_1, ptr align 4 %0, i64 4, i1 false)
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #2

attributes #0 = { inlinehint nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }
attributes #1 = { nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }
attributes #2 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 2, !"RtLibUseGOT", i32 1}
