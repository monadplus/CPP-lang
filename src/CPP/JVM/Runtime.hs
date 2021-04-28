{-# LANGUAGE QuasiQuotes #-}
-- | 

module CPP.JVM.Runtime (runtimeJ) where

import Data.ByteString.Char8 (ByteString)
import Data.String.QQ

-- | 'ByteString' representation of the jasmin Runtime class.
--
-- This bytestring can be compiled with *jasmin* executable when written into a file.
runtimeJ :: ByteString
runtimeJ = [s|
.class Runtime
.super java/lang/Object

.method <init>()V
  .limit stack 1
  .limit locals 1
  .line 3
  0: aload_0
  1: invokespecial java/lang/Object/<init>()V
  4: return
.end method

.method public static printBool(Z)V
  .limit stack 2
  .limit locals 1
  .line 8
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: iload_0
  4: invokevirtual java/io/PrintStream/println(Z)V
  .line 9
  7: return
.end method

.method public static printInt(I)V
  .limit stack 2
  .limit locals 1
  .line 11
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: iload_0
  4: invokevirtual java/io/PrintStream/println(I)V
  .line 12
  7: return
.end method

.method public static printDouble(D)V
  .limit stack 3
  .limit locals 2
  .line 14
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: dload_0
  4: invokevirtual java/io/PrintStream/println(D)V
  .line 15
  7: return
.end method

.method public static printString(Ljava/lang/String;)V
  .limit stack 2
  .limit locals 1
  .line 17
  0: getstatic java/lang/System/out Ljava/io/PrintStream;
  3: aload_0
  4: invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  .line 18
  7: return
.end method

.method public static readInt()I
  .limit stack 3
  .limit locals 2
  .line 21
  0: new java/util/Scanner
  3: dup
  4: getstatic java/lang/System/in Ljava/io/InputStream;
  7: invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
  10: astore_0
  .line 22
  11: aload_0
  12: invokevirtual java/util/Scanner/nextInt()I
  15: istore_1
  .line 23
  16: aload_0
  17: invokevirtual java/util/Scanner/close()V
  .line 24
  20: iload_1
  21: ireturn
.end method

.method public static readDouble()D
  .limit stack 3
  .limit locals 3
  .line 27
  0: new java/util/Scanner
  3: dup
  4: getstatic java/lang/System/in Ljava/io/InputStream;
  7: invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
  10: astore_0
  .line 28
  11: aload_0
  12: invokevirtual java/util/Scanner/nextDouble()D
  15: dstore_1
  .line 29
  16: aload_0
  17: invokevirtual java/util/Scanner/close()V
  .line 30
  20: dload_1
  21: dreturn
.end method

.method public static readString()Ljava/lang/String;
  .limit stack 3
  .limit locals 2
  .line 33
  0: new java/util/Scanner
  3: dup
  4: getstatic java/lang/System/in Ljava/io/InputStream;
  7: invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
  10: astore_0
  .line 34
  11: aload_0
  12: invokevirtual java/util/Scanner/nextLine()Ljava/lang/String;
  15: astore_1
  .line 35
  16: aload_0
  17: invokevirtual java/util/Scanner/close()V
  .line 36
  20: aload_1
  21: areturn
.end method

.method public static i2s(I)Ljava/lang/String;
  .limit stack 1
  .limit locals 1
  .line 40
  0: iload_0
  1: invokestatic java/lang/String/valueOf(I)Ljava/lang/String;
  4: areturn
.end method

.method public static d2s(D)Ljava/lang/String;
  .limit stack 2
  .limit locals 2
  .line 43
  0: dload_0
  1: invokestatic java/lang/String/valueOf(D)Ljava/lang/String;
  4: areturn
.end method

.method public static sadd(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
  .limit stack 2
  .limit locals 2
  .line 46
  0: new java/lang/StringBuilder
  3: dup
  4: invokespecial java/lang/StringBuilder/<init>()V
  7: aload_0
  8: invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
  11: aload_1
  12: invokevirtual java/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;
  15: invokevirtual java/lang/StringBuilder/toString()Ljava/lang/String;
  18: areturn
.end method

.method public static seq(Ljava/lang/String;Ljava/lang/String;)Z
  .limit stack 2
  .limit locals 2
  .line 49
  0: aload_0
  1: aload_1
  2: invokevirtual java/lang/String/equals(Ljava/lang/Object;)Z
  5: ireturn
.end method

.method public static sne(Ljava/lang/String;Ljava/lang/String;)Z
  .limit stack 2
  .limit locals 2
  .line 52
  0: aload_0
  1: aload_1
  2: invokevirtual java/lang/String/equals(Ljava/lang/Object;)Z
  5: ifne Label12
  8: iconst_1
  9: goto Label13
Label12:
  12: iconst_0
Label13:
  13: ireturn
  ; same_frame (frameNumber = 0)
  ; frame_type = 12, offset_delta = 12
  ; frame bytes: 12
  .stack
    offset 12
    .end stack
  ; same_locals_1_stack_item_frame (frameNumber = 1)
  ; frame_type = 64, offset_delta = 0
  ; frame bytes: 64 1
  .stack
    offset 13
    stack Integer
    .end stack
.end method
|]
