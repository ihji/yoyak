package com.simplytyped.yoyak.android

import com.simplytyped.yoyak.il.CommonIL.Type.{IntegerType, CommonTypes}
import com.simplytyped.yoyak.il.CommonIL.{ClassName, MethodSig}

object AndroidAPIs {
  val internet = Set(
    MethodSig(ClassName("java.net.Socket"),"<init>",List(CommonTypes.String,IntegerType))
  )
}
