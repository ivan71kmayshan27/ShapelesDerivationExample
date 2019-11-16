Short example which demonstrates how simply typeclass instance derivation could be done with the help of shapeless. 

For this particular example, structural comparison operation derivation is done for sealed trait family. 

I.e. for any sealed trait family like: 

```scala
   sealed trait Color
   
     sealed trait PredefColor extends Color
     case object Red extends PredefColor
     case object Green extends PredefColor
     case object Blue extends PredefColor
   
     case class RGB(r: Int, g: Int, b: Int) extends Color
     case class RGBA(r: Int, g: Int, b: Int, a: Int) extends Color
     case class HSV(h: Int, s: Int, v: Int) extends Color
```

We derive 

```scala 
val comparer = GenericComparer[Color]
```

Which allows to determine structural difference between two objects belonging to the same family.

Done by Ivan Kamyshan, 2019.