; ModuleID = 'CMAT'

declare i32 @printf(i8*, ...)

define i1 @foobar() {
entry:
  %b = alloca i1
  store i1 true, i1* %b
  %b1 = load i1* %b
  ret i1 %b1
}

define void @bar() {
entry:
  %k = alloca i32
  %0 = bitcast i32* %k to i8*
  tail call void @free(i8* %0)
  ret void
}

define float @foo() {
entry:
  %j = alloca float
  store float 0x3FF19999A0000000, float* %j
  %j1 = load float* %j
  ret float %j1
}

define i32 @main() {
entry:
  %i = alloca i32
  %f = alloca float
  %b = alloca i1
  store i32 0, i32* %i
  %foo_result = call float @foo()
  store float %foo_result, float* %f
  call void @bar()
  %foobar_result = call i1 @foobar()
  store i1 %foobar_result, i1* %b
  %i1 = load i32* %i
  ret i32 %i1
}

declare void @free(i8*)
