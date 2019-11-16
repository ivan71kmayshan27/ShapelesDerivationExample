package com.example.derivation.genericComparators

import shapeless.labelled.FieldType
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

import scala.reflect.ClassTag

trait GenericComparatorsLowestPriority {

  implicit val cnilCompare: GenericComparer[CNil] = GenericComparer.instance[CNil] { (a, b) =>
    a.impossible
  }{"neverhappen"}

  implicit val hnilCompare: GenericComparer[HNil] = GenericComparer.instance[HNil] { (_, _) => true }{"hnil"}

  //case class compare
  implicit def hconsCompareKeyed[Key <: Symbol, Head, Tail <: HList](
    implicit key: Witness.Aux[Key],
    compareHeads: GenericComparer[Head],
    compareTails: Lazy[GenericComparer[Tail]]): GenericComparer[FieldType[Key, Head] :: Tail] =
    new GenericComparer[FieldType[Key, Head] :: Tail] {
      override def apply(
        t1: FieldType[Key, Head] :: Tail,
        t2: FieldType[Key, Head] :: Tail)(
        implicit path: AdtPath, result: ComparisonResult
      ): ComparisonResult = {
        val newPath = path.downHlist(key.value)

        compareHeads.apply(t1.head, t2.head)(newPath, result) &&
          compareTails.value.apply(t1.tail, t2.tail)(newPath, result)
      }
    }

  //sealed trait family compare
  implicit def cconsCompareKeyed[Key <: Symbol, Head, Tail <: Coproduct](
    implicit key: Witness.Aux[Key],
    compareHeads: GenericComparer[Head],
    compareTails: Lazy[GenericComparer[Tail]]
  ): GenericComparer[FieldType[Key, Head] :+: Tail] =
    new GenericComparer[FieldType[Key, Head] :+: Tail] {
      override def apply(
        t1: FieldType[Key, Head] :+: Tail,
        t2: FieldType[Key, Head] :+: Tail)(
        implicit path: AdtPath, result: ComparisonResult): ComparisonResult = {
        val newPath = path.downCoproduct(key.value)
        (t1, t2) match {
          case (Inl(a), Inl(b)) =>
            compareHeads.apply(a, b)(newPath, result)
          case (Inl(_), Inr(_)) | (Inr(_), Inl(_)) =>
            result.append(FailEntry(newPath, Coproduct.unsafeGet(t1), Coproduct.unsafeGet(t2)))
          case (Inr(tail1), Inr(tail2)) =>
            compareTails.value.apply(tail1, tail2)
        }
      }
    }

  //decompose sealed trait family
  implicit def lgenCompare[T, Repr](
    implicit
    ctag: ClassTag[T],
    lgen: LabelledGeneric.Aux[T, Repr],
    reprCompare: Lazy[GenericComparer[Repr]]): GenericComparer[T] = new GenericComparer[T] {
    override def apply(t1: T, t2: T)(implicit path: AdtPath, result: ComparisonResult): ComparisonResult = {
      reprCompare.value.apply(lgen.to(t1), lgen.to(t2))(path.downGeneric(ctag.runtimeClass.getSimpleName), result)
    }
  }

  //util method to compare any iterable
  //TODO: make least-edit distance diff here
  def compareIterables[T: GenericComparer]: GenericComparer[Iterable[T]] =
    new GenericComparer[Iterable[T]] {
      override def apply(t1: Iterable[T], t2: Iterable[T])(implicit path: AdtPath, result: ComparisonResult)
      : ComparisonResult = {
        if (t1.size == t2.size) {
          (t1 zip t2).foldLeft((result, 0)) { case ((res, i), (el1, el2)) =>
            (implicitly[GenericComparer[T]].apply(el1, el2)(path.downIterable(i), res), i + 1)
          }._1
        }
        else result.append(FailEntry(path.downIterable(-1), t1, t2))
      }
    }
}
