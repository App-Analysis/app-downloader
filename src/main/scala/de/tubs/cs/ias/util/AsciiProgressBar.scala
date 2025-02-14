package de.tubs.cs.ias.util


trait ProgressBar {

  def step() : Unit

  def getCurrent : Long

  def close() : Unit

}

class DummyProgressBar(val name : String,val length : Long) extends ProgressBar {

  private var current : Long = 0

  override def step() : Unit = synchronized {
      current += 1
  }

  override def getCurrent: Long = synchronized{ this.current }

  override def close(): Unit = {}

}

object AsciiProgressBar {

  def create(name: String, length: Long): ProgressBar = {
    /*
    new ProgressBarBuilder()
      .setStyle(ProgressBarStyle.ASCII)
      .setTaskName(name)
      .setInitialMax(length)
      .build()
     */
    new DummyProgressBar(name, length)
  }

}
