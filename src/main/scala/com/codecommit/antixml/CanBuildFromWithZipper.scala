package com.codecommit.antixml

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Vector
import scala.collection.mutable.Builder

trait CanBuildFromWithZipper[-From, -Elem, To <: Traversable[_]] extends CanBuildFrom[From, Elem, To] {
  def apply(from: From): Builder[Elem, To] = apply(from, Vector())
  def apply(): Builder[Elem, To] = apply(Vector())
  
  def apply(from: From, map: Vector[(Int, Int, Group[Node] => Node)]): Builder[Elem, To]
  def apply(map: Vector[(Int, Int, Group[Node] => Node)]): Builder[Elem, To]
  
  def concat[Elem2 <: Elem](left: Traversable[Elem2], right: Traversable[Elem2])(implicit cbf: CanBuildFrom[Traversable[_], Elem2, To]): To =
    left ++ right
}

object CanBuildFromWithZipper {
  implicit def identityCanBuildFrom[From, Elem, To <: Traversable[_]](implicit cbf: CanBuildFrom[From, Elem, To]): CanBuildFromWithZipper[From, Elem, To] = new CanBuildFromWithZipper[From, Elem, To] {
    def apply(from: From, map: Vector[(Int, Int, Group[Node] => Node)]) = cbf.apply(from)
    def apply(map: Vector[(Int, Int, Group[Node] => Node)]) = cbf.apply()
  }
}
