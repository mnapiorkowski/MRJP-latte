@nl  = internal constant [2 x i8] c"\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@dnl = internal constant [4 x i8] c"%d\0A\00"
@snl = internal constant [4 x i8] c"%s\0A\00"
@err = internal constant [14 x i8] c"runtime error\00"

@stdin = external global i8*

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @getline(i8**, i32*, i8*)
declare i32 @getchar()
declare i8* @malloc(i32)
declare void @free(i8*)
declare void @memcpy(i8*, i8*, i32)
declare i8* @memset(i8*, i32, i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare void @exit(i32)

%_array = type {i8*, i32}

define void @printInt(i32 %x) {
  %dnl = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %dnl, i32 %x)
  ret void
}

define void @printString(i8* %s) {
  %is_null = icmp eq i8* %s, null
  br i1 %is_null, label %Null, label %NotNull
Null:
  %nl = getelementptr [2 x i8], [2 x i8]* @nl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %nl)
	ret void
NotNull:
  %snl = getelementptr [4 x i8], [4 x i8]* @snl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %snl, i8* %s)
	ret void
}

define void @error() {
  %err = getelementptr [14 x i8], [14 x i8]* @err, i32 0, i32 0
  call void @printString(i8* %err)
  call void @exit(i32 1)
  ret void
}

define i32 @readInt() {
  %res = alloca i32
  store i32 0, i32* %res
  %d = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %d, i32* %res)
  call i32 @getchar()
	%res_val = load i32, i32* %res
	ret i32 %res_val
}

define i8* @readString() {
  %buff_ptr = alloca i8*
  store i8* null, i8** %buff_ptr
  %size_ptr = alloca i32
  store i32 0, i32* %size_ptr
  %stdin = load i8*, i8** @stdin
  %len = call i32 @getline(i8** %buff_ptr, i32* %size_ptr, i8* %stdin)
  %buff = load i8*, i8** %buff_ptr
  %cmp = icmp sgt i32 %len, 0
  br i1 %cmp, label %GetlineOk, label %GetlineError
GetlineOk:
  %len_m_1 = sub i32 %len, 1
  %line = call i8* @malloc(i32 %len)
  call void @memcpy(i8* %line, i8* %buff, i32 %len_m_1)
  %last_char_ptr = getelementptr i8, i8* %line, i32 %len_m_1
  store i8 0, i8* %last_char_ptr
  br label %End
GetlineError:
  call void @free(i8* %buff)
  call void @error()
  ret i8* null
End:
  call void @free(i8* %buff)
  ret i8* %line
}

define i8* @_concatStrings(i8* %a, i8* %b) {
  %is_a_null = icmp eq i8* %a, null
  br i1 %is_a_null, label %ANull, label %ANotNull
ANull:
  ret i8* %b
ANotNull:
  %is_b_null = icmp eq i8* %b, null
  br i1 %is_b_null, label %BNull, label %BNotNull
BNull:
  ret i8* %a
BNotNull:
  %len_a = call i32 @strlen(i8* %a)
  %len_b = call i32 @strlen(i8* %b)
  %len_a_b = add i32 %len_a, %len_b
  %len = add i32 %len_a_b, 1
  %buff = call i8* @malloc(i32 %len)
  call i8* @strcpy(i8* %buff, i8* %a)
  call i8* @strcat(i8* %buff, i8* %b)
  ret i8* %buff
}

define void @_clearNElems(i8* %buff, i32 %num, i32 %elem_size) {
  %size = mul i32 %num, %elem_size
  call i8* @memset(i8* %buff, i32 0, i32 %size)
  ret void
}

define %_array* @_mallocArrayType() {
  %sizeptr = getelementptr %_array, %_array* null, i32 1
  %size = ptrtoint %_array* %sizeptr to i32
  %ptr = call i8* @malloc(i32 %size)
  %res = bitcast i8* %ptr to %_array*
  ret %_array* %res
}
