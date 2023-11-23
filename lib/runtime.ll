@dnl = internal constant [4 x i8] c"%d\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@err = internal constant [14 x i8] c"runtime error\00"

@stdin = external global i8*

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare i32 @getline(i8**, i32*, i8*)
declare i8* @malloc(i32)
declare void @free(i8*)
declare void @memcpy(i8*, i8*, i32)
declare i32 @strlen(i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare void @exit(i32)

define void @printInt(i32 %x) {
entry:
  %dnl = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %dnl, i32 %x)
  ret void
}

define void @printString(i8* %s) {
entry: 
  call i32 @puts(i8* %s)
	ret void
}

define void @error() {
entry:
  %err = getelementptr [14 x i8], [14 x i8]* @err, i32 0, i32 0
  call i32 @puts(i8* %err)
  call void @exit(i32 1)
  ret void
}

define i32 @readInt() {
entry:	
  %res = alloca i32
  store i32 0, i32* %res
  %d = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %d, i32* %res)
	%res_val = load i32, i32* %res
	ret i32 %res_val
}

define i8* @readString() {
entry:
  %buff_ptr = alloca i8*
  store i8* null, i8** %buff_ptr
  %size_ptr = alloca i32
  store i32 0, i32* %size_ptr
  %stdin = load i8*, i8** @stdin
  %len = call i32 @getline(i8** %buff_ptr, i32* %size_ptr, i8* %stdin)
  %buff = load i8*, i8** %buff_ptr
  %cmp = icmp sgt i32 %len, 0
  br i1 %cmp, label %getline_ok, label %getline_error

getline_ok:
  %len_m_1 = sub i32 %len, 1
  %line = call i8* @malloc(i32 %len)
  call void @memcpy(i8* %line, i8* %buff, i32 %len_m_1)

  %last_char_ptr = getelementptr i8, i8* %line, i32 %len_m_1
  store i8 0, i8* %last_char_ptr
  br label %end

getline_error:
  call void @free(i8* %buff)
  call void @error()
  ret i8* null

end:
  call void @free(i8* %buff)
  ret i8* %line
}

define i8* @concatStrings(i8* %a, i8* %b) {
  %len_a = call i32 @strlen(i8* %a)
  %len_b = call i32 @strlen(i8* %b)
  %len_a_b = add i32 %len_a, %len_b
  %len = add i32 %len_a_b, 1
  %buff = call i8* @malloc(i32 %len)
  call i8* @strcpy(i8* %buff, i8* %a)
  call i8* @strcat(i8* %buff, i8* %b)
  ret i8* %buff
}
