.class public Main
.super java/lang/Object

; Constructor
.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

; Main
.method public static main([Ljava/lang/String;)V
  .limit locals 2
  .limit stack 2
  ;invokestatic runtime/readString()Ljava/lang/String;
  ;astore_1
  ; Read str[0]
  ldc "str length: "
  invokestatic runtime/printString(Ljava/lang/String;)V
  aload_0
  arraylength
  invokestatic runtime/printInt(I)V
  aload_0
  iconst_0
  aaload
  invokestatic runtime/printString(Ljava/lang/String;)V
  return
.end method