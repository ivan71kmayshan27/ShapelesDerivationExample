package com.example.derivation.genericComparators


object Demo extends AllComparators with App{
  //if something breaks here add "-Xlog-implicits" to compiler options and look what is missing
  sealed trait All
  case class Foo(s: String, i: Int, b: Boolean) extends All
  case class Bar(l: Long, f: Foo) extends All
  case class Baz(foos: List[Foo], bars: List[Bar]) extends All

  case class DoNotDerive(d: Double) extends Product with Serializable

  implicit val genericComparerDND: GenericComparer[DoNotDerive] = new GenericComparer[DoNotDerive] {
    override def apply(t1: DoNotDerive, t2: DoNotDerive)(implicit path: AdtPath, result: ComparisonResult)
    : ComparisonResult = {
      if( Math.abs( t1.d - t2.d) > 10E-5D){
        result && ComparisonResult.Success append FailEntry( path.downManual("DoNotDerive"), t1,t2)
      } else {
        result
      }
    }

  }

  case class DeriveWithDoNotDerive(i: Long, doNotDerive: DoNotDerive)

  implicitly[GenericComparer[Foo]]
  implicitly[GenericComparer[Bar]]
  implicitly[GenericComparer[Baz]]
  implicitly[GenericComparer[DeriveWithDoNotDerive]]

  val `10E-7` = 10E-7D
  val `10E-4` = 10E-4D

  val a = DeriveWithDoNotDerive(1, DoNotDerive(10 + `10E-7`))
  val b = DeriveWithDoNotDerive(1, DoNotDerive(10 - `10E-7`))

  val aa = DeriveWithDoNotDerive(1, DoNotDerive(10 + `10E-4`))
  val bb = DeriveWithDoNotDerive(1, DoNotDerive(10 - `10E-4`))

  val res = GenericComparer.compare[DeriveWithDoNotDerive](a,b)
  val res2 = GenericComparer.compare[DeriveWithDoNotDerive](aa,bb)

  println(res)
  println(res2)

  sealed trait Color

  sealed trait PredefColor extends Color
  case object Red extends PredefColor
  case object Green extends PredefColor
  case object Blue extends PredefColor

  case class RGB(r: Int, g: Int, b: Int) extends Color
  case class RGBA(r: Int, g: Int, b: Int, a: Int) extends Color
  case class HSV(h: Int, s: Int, v: Int) extends Color

  implicit class ToRgbOps(val color: PredefColor) extends AnyVal {
    def toRgb: RGB = color match {
      case Red => RGB(255, 0, 0)
      case Green => RGB(0, 255, 0)
      case Blue => RGB(0, 0, 255)
    }
  }


  val someColor: Color = RGBA(1,2,3,1)
  val someColor2: Color = RGB(1,2,3)

  val res3 = GenericComparer.compare(someColor, someColor2)
  println(res3)

  val someColor3: Color = RGBA(2,3,4,5)
  val someColor4: Color = RGBA(2,3,4,1)

  val res4 = GenericComparer.compare(someColor3, someColor4)
  println(res4)


}
