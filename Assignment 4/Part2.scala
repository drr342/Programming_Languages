import scala.collection.mutable.ListBuffer

abstract class Tree[+T]
case class Node[T] (label: T, left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T] (label: T) extends Tree[T]

trait Addable[T] {
  def +(t: T): T
}

class A (_a: Int) extends Addable[A] with Ordered[A] {
  var value: Int = _a
  override def +(other: A): A = new A(this.value + other.value)
  override def toString: String = "A(" + this.value + ")"
  override def compare(that: A): Int = Integer.signum(this.value - that.value)
}

class B (_b: Int) extends A(_b) {
  override def toString: String = "B(" + this.value + ")"
}

class C (_c: Int) extends B(_c) {
  override def toString: String = "C(" + this.value + ")"
}

object Part2 {

  def getValues[T](tree: Tree[T]): ListBuffer[T] = {
    var list: ListBuffer[T] = new ListBuffer[T]()
    tree match {
      case Leaf(label) => list += label
      case Node(label, left, right) => (list += label) ++ getValues(left) ++ getValues(right)
    }
  }

  def inOrder[T <: Ordered[T]](tree: Tree[T]): List[T] = getValues(tree).toList.sorted

  def treeSum[T <: Addable[T]](tree: Tree[T]): T = getValues(tree).toList.reduce(_ + _)

  def treeMap[T, V] (f: T => V, tree: Tree[T]): Tree[V] = {
    tree match {
      case Leaf(label) => Leaf(f(label))
      case Node(label, left, right) => Node(f(label), treeMap(f, left), treeMap(f, right))
    }
  }

  def BTreeMap (f: B => B, tree: Tree[B]): Tree[B] = treeMap(f, tree)

  def test() {
    def faa(a:A):A = new A(a.value+10)
    def fab(a:A):B = new B(a.value+20)
    def fba(b:B):A = new A(b.value+30)
    def fbb(b:B):B = new B(b.value+40)
    def fbc(b:B):C = new C(b.value+50)
    def fcb(c:C):B = new B(c.value+60)
    def fcc(c:C):C = new C(c.value+70)
    def fac(a:A):C = new C(a.value+80)
    def fca(c:C):A = new A(c.value+90)

    val myBTree: Tree[B] = Node(new B(4),Node(new B(2),Leaf(new B(1)),Leaf(new B(3))),
      Node(new B(6), Leaf(new B(5)), Leaf(new B(7))))

    val myATree: Tree[A] = myBTree

    println("inOrder = " + inOrder(myATree))
    println("Sum = " + treeSum(myATree))

    //println(BTreeMap(faa,myBTree)) // Expected: B => B, actual: A => A (Contravariant in the output NOT allowed)
    println(BTreeMap(fab,myBTree))   // Expected: B => B, actual: A => B (Contravariant input, output as expected = OK)
    //println(BTreeMap(fba,myBTree)) // Expected: B => B, actual: B => A (Contravariant in the output NOT allowed)
    println(BTreeMap(fbb,myBTree))   // Expected: B => B, actual: B => B (Invariant = OK)
    println(BTreeMap(fbc,myBTree))   // Expected: B => B, actual: B => C (Input as expected, covariant output = OK)
    //println(BTreeMap(fcb,myBTree)) // Expected: B => B, actual: C => B (Covariant in the input NOT allowed)
    //println(BTreeMap(fcc,myBTree)) // Expected: B => B, actual: C => C (Covariant in the input NOT allowed)
    println(BTreeMap(fac,myBTree))   // Expected: B => B, actual: A => C (Contravariant input, covariant output = OK)
    //println(BTreeMap(fca,myBTree)) // Expected: B => B, actual: C => A (Convariant input and contravariant output NOT allowed)

    println(treeMap(faa,myATree))    // in: A, f: A => A (f input as expected = OK)
    println(treeMap(fab,myATree))    // in: A, f: A => B (f input as expected = OK)
    //println(treeMap(fba,myATree))  // in: A, f: B => A (f contravariant input NOT allowed)
    //println(treeMap(fbb,myATree))  // in: A, f: B => B (f contravariant input NOT allowed)
    //println(treeMap(fbc,myATree))  // in: A, f: B => C (f contravariant input NOT allowed)
    //println(treeMap(fcb,myATree))  // in: A, f: C => B (f contravariant input NOT allowed)
    //println(treeMap(fcc,myATree))  // in: A, f: C => C (f contravariant input NOT allowed)
    println(treeMap(fac,myATree))    // in: A, f: A => C (f input as expected OK)
    //println(treeMap(fca,myATree))  // in: A, f: C => A (f contravariant input NOT allowed)
  }

  def main(args: Array[String]) {
    test()
  }

}
