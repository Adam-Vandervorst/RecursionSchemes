import be.adamv.recursionschemes.{*, given}
import examples.file_system.{*, given}
import org.scalatest.funsuite.AnyFunSuite

class SystemTest extends AnyFunSuite:
  import System.*

  val di1 = Dir[Int]("numbers_1_2", Seq(1, 2))
  val di2 = Dir[Int]("even_under_10", Seq(2, 4, 6, 8))
  val ds1 = Dir[String]("numbers_1_2", Seq("1", "2"))

  val ff1 = Fix(File("foo", "txt", "42"))
  val ff2 = Fix(File("bar", "txt", "something"))
  val ff3 = Fix(File("baz", "txt", "else"))
  val ff4 = Fix(File("qux", "csv", "1,2;3,4"))
  val ff5 = Fix(File("qux", "bin", "1,2;3,4".strHash))

  val fd1 = Fix(Dir("nothing", Seq()))
  val fd2 = Fix(Dir("fb", Seq(ff1, ff2)))
  val fd3 = Fix(Dir("mixed", Seq(fd1, fd2, ff3)))
  val fd4 = Fix(Dir("other", Seq(ff4, ff5)))
  val fd5 = Fix(Dir("both", Seq(fd3, fd4)))

  val fl1 = Fix(Link(retrieve_path("root/both/other/cux.csv")))
  val fl2 = Fix(Link(retrieve_path("root/both/mixed/nothing")))
  val fl3 = Fix(Link(retrieve_path("root/refs")))

  val fd6 = Fix(Dir("refs", Seq(fl1, fl2, fl3)))
  val fd7 = Fix(Dir("root", Seq(fd5, fd6)))


  test("functor map"){
    assert(di1.map(_.toString) == ds1)
  }

  test("traversable map"){
    given Fix[System] = ff1

    assert(summon[Traversable[System]].map(di1)(_.toString) == ds1)
  }

  test("traversable sequence"){
    given Fix[System] = ff1

    assert(di1.map(Some(_).filter(_ % 2 == 0)).sequence.isEmpty)
    assert(di2.map(Some(_).filter(_ % 2 == 0)).sequence.contains(di2))
  }

  test("files"){
    given Fix[System] = fd5

//    println(files(fd1))
//    println(files(fd2))
//    println(files(ff3))
//    println(files(fd3))
//    println(files(fd4))
//    println(files(fd5))
  }

  test("search"){
    given Fix[System] = fd5

    println(search("ba")(fd5))
    println(search("fb")(fd5))
    println(search("nothing")(fd5))
    println(search("both")(fd5))
    println(search("qux")(fd5))
    println(search("xxx")(fd5))
  }

//  test("retrieve depth_1"){
//    assert(retrieve_path("baz.txt", ff1).isEmpty)
//    assert(retrieve_path("foo.txt", ff1).contains(ff1))
//  }

  test("retrieve depth_n"){
    given Fix[System] = fd3

//    println(retrieve_path("mixed/fb/foo.txt", fd3))
//    println(retrieve_path("mixed/nothing", fd3))

//    println(retrieve_path("mixed/baz.txt", fd3))
  }

