package fintech.homework03


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]
  def sub(path: Seq[K]): PrefixTree[K, V]
  def get: V
}

class PrefixTreeImpl[K, +V](val value: Option[V], val prefixTree: Map[K, PrefixTree[K, V]]) extends PrefixTree[K, V]{

  override def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = {
    if (path.isEmpty)
      new PrefixTreeImpl[K, U](Option(value), Map())
    else if (prefixTree.contains(path.head))
      new PrefixTreeImpl[K, U](Option.empty,  Map(path.head -> prefixTree(path.head).put(path.drop(1), value)))
    else
      new PrefixTreeImpl[K, U](this.value,  prefixTree + (path.head -> put(path.drop(1), value)))
  }

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
    if (path.length > 1) {
      prefixTree(path.head).sub(path.drop(1))
    } else {
      prefixTree(path.head)
    }
  }

  override def get: V = value.get
}
