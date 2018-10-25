package fintech.homework05

import java.time.Instant
import java.util.UUID

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {

  "Storage method save and get" should "be work correct" in {
    val storage = InMemoryTweetStorage
    val uuid = UUID.randomUUID().toString
    val tweet = Tweet(
      uuid,
      "me",
      "Test text #test",
      Seq("#test"),
      Option(Instant.now()),
      0)
    storage.get(uuid) shouldBe Option.empty
    storage.save(tweet) shouldBe tweet
    storage.get(uuid).get shouldBe tweet
  }
}
