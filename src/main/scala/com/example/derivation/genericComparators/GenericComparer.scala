package com.example.derivation.genericComparators

object GenericComparer {
  def compare[T](first: T, second: T)(
    implicit compare: GenericComparer[T]): ComparisonResult =
    compare.apply(first, second)(AdtPath.root, ComparisonResult.Success)

  def instance[U](f: (U, U) => Boolean)(tag: String = ""): GenericComparer[U] = new GenericComparer[U] {
    override def apply(t1: U, t2: U)(implicit path: AdtPath, result: ComparisonResult): ComparisonResult =
      if (f(t1, t2)) result
      else result.append(FailEntry(path, t1, t2))
  }
}

trait GenericComparer[T] {
  def apply(t1: T, t2: T)(implicit path: AdtPath, result: ComparisonResult): ComparisonResult
}