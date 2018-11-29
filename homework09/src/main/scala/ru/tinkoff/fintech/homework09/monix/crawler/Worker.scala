package ru.tinkoff.fintech.homework09.monix.crawler
import monix.eval.{Fiber, Task}

trait Worker {
  def http: Http
  def parseLinks: Parsr

  def worker(workerQueue: MQueue[Url], crawlerQueue: MQueue[CrawlerMessage]): Task[Fiber[Unit]] = {
    crawlerQueue.take.flatMap{
      case Start(url) =>
          http.get(url).flatMap(body => {
            workerQueue.offer(url)
            crawlerQueue.offer(CrawlResult(url, body))
          }
        ).start
    }
  }
}
