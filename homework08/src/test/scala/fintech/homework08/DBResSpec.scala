package fintech.homework08

import org.scalatest.{FlatSpec, Matchers}

class DBResSpec extends FlatSpec with Matchers {

  val uri = "jdbc:h2:~/dbres"

  "Method map of DBRes" should "be work correct" in {
    DBRes.update("CREATE TABLE people(name VARCHAR(256), birthday DATE)", List.empty)
      .map(_ => DBRes.select("SELECT name FROM people", List.empty)(x => x.getString("count"))).isInstanceOf[DBRes[List[String]]] shouldBe true
  }

  "Method flatMap of DBRes" should "be work correct" in {
    DBRes.update("CREATE TABLE people(name VARCHAR(256), birthday DATE)", List.empty)
        .flatMap(_ => DBRes.select("SELECT name FROM people", List.empty)(x => x.getString("count"))
        ).execute(uri) shouldBe List()
  }

}
