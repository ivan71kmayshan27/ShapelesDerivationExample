package com.example.derivation

import cats.data.Chain

package object genericComparators {

  sealed trait ComparisonResult {
    def append(fe: FailEntry[_]): ComparisonResult

    def &&(that: ComparisonResult): ComparisonResult

    def fails: Chain[FailEntry[_]]

  }

  object ComparisonResult {

    case object Success extends ComparisonResult {
      override def append(fe: FailEntry[_]): ComparisonResult = Failure(Chain(fe))

      override def &&(that: ComparisonResult): ComparisonResult = that

      override def fails: Chain[FailEntry[_]] = Chain.empty[FailEntry[_]]

    }

    case class Failure(fails: Chain[FailEntry[_]]) extends ComparisonResult {
      override def append(fe: FailEntry[_]): ComparisonResult = Failure(fails :+ fe)

      override def &&(that: ComparisonResult): ComparisonResult = Failure(this.fails ++ that.fails)

    }

  }

  case class FailEntry[T](path: AdtPath, left: T, right: T)

  object AdtPath {
    val root = AdtPath(Chain.empty[PathElement])
  }

  sealed trait PathElement

  case object Root extends PathElement

  case class DownGeneric(generic: String) extends PathElement
  case class DownField(field: Symbol) extends PathElement
  case class DownCoproductElement(coproductType: Symbol) extends PathElement
  case class Primitive(field: String) extends PathElement
  case class DownIterable(index: Long) extends PathElement
  case class DownManual(tag: String) extends PathElement

  case class AdtPath(steps: Chain[PathElement], last: PathElement = Root) {
    def downHlist(fieldName: Symbol): AdtPath =
      last match {
        case DownField(_) => AdtPath(steps, DownField(fieldName))
        case Primitive(_) => throw new RuntimeException(s"should not never happen")
        case _ => AdtPath(steps :+ last, DownField(fieldName))
      }

    def downCoproduct(element: Symbol): AdtPath =
      last match {
        case DownCoproductElement(_) => AdtPath(steps, DownCoproductElement(element))
        case Primitive(_) => throw new RuntimeException(s"should not never happen")
        case _ => AdtPath(steps :+ last, DownCoproductElement(element))
      }

    def downGeneric(className: String): AdtPath =
      last match {
        case Primitive(_) => throw new RuntimeException(s"should not never happen")
        case _ => AdtPath(steps :+ last, DownGeneric(className))
      }

    def downIterable(index: Long): AdtPath = last match {
      case DownIterable(_) => AdtPath(steps, DownIterable(index))
      case Primitive(_) =>throw new RuntimeException(s"should not never happen")
      case _ => AdtPath(steps :+ last, DownIterable(index))
    }

    def downManual(manual: String): AdtPath = AdtPath( steps :+ last, DownManual(manual))

    def primitive(primitiveTag: String): AdtPath = AdtPath( steps :+ last, Primitive(primitiveTag))
  }
}
