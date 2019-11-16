package com.example.derivation.genericComparators

import java.time.Instant
import java.util.UUID

import cats.data.NonEmptyList

trait AllComparators extends GenericComparatorsLowestPriority {

  //Primitive comparers
  implicit val scompare: GenericComparer[String] = GenericComparer.instance[String] { _ == _ } {"string"}
  implicit val icompare: GenericComparer[Int] = GenericComparer.instance[Int] { _ == _ } {"int"}
  implicit val lcompare: GenericComparer[Long] = GenericComparer.instance[Long] { _ == _ } {"long"}
  implicit val bcompare: GenericComparer[Byte] = GenericComparer.instance[Byte] { _ == _ } {"byte"}
  implicit val boolcompare: GenericComparer[Boolean] = GenericComparer.instance[Boolean] { _ == _ } {"bool"}
  implicit val ccompare: GenericComparer[Char] = GenericComparer.instance[Char] { _ == _ } {"char"}
  implicit val shortcompare: GenericComparer[Short] = GenericComparer.instance[Short] { _ == _ } {"short"}
  implicit val bicompare: GenericComparer[BigInt] = GenericComparer.instance[BigInt] { _ == _ } {"bigInt"}
  implicit val dcompare: GenericComparer[Double] = GenericComparer.instance[Double] { _ == _ } {"double"}


  implicit def nelComparator[T: GenericComparer]: GenericComparer[NonEmptyList[T]] =
    new GenericComparer[NonEmptyList[T]] {
    override def apply(t1: NonEmptyList[T], t2: NonEmptyList[T])(implicit path: AdtPath, result: ComparisonResult)
    : ComparisonResult = compareIterables[T].apply(t1.toList, t2.toList)
  }

  implicit val uuidcompare: GenericComparer[UUID] = GenericComparer.instance[UUID] { _ == _ } {"uuid"}
  implicit val instantcompare: GenericComparer[Instant] = GenericComparer.instance[Instant] { _ == _ } {"instant"}

  implicit def arraycompare[T : GenericComparer]: GenericComparer[Array[T]] = new GenericComparer[Array[T]] {
    override def apply(t1: Array[T], t2: Array[T])(implicit path: AdtPath, result: ComparisonResult)
    : ComparisonResult = compareIterables[T].apply(t1,t2)
  }

  implicit def listCompare[T: GenericComparer]: GenericComparer[List[T]] = new GenericComparer[List[T]] {
    override def apply(t1: List[T], t2: List[T])(implicit path: AdtPath, result: ComparisonResult)
    : ComparisonResult = compareIterables[T].apply(t1, t2)
  }

}
