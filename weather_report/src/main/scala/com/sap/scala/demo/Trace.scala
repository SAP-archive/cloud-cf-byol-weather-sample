package com.sap.scala.demo

/***********************************************************************************************************************
  * Debugging trace utility
  *
  * Author : Chris Whealy, SAP(UK) Ltd
  *
  * Generally used by creating partial functions from the various flow... functions
  *
  * For example, a consumer of the trace functions would typically create the following values and partial functions
  * within an object or class:
  *
  *   private val libName     = "Your object or class name"
  *   private val traceActive = true
  *
  *   private val trace     = Trace.flow(traceActive)(libName)(_: String)(_: scala.Option[Boolean])
  *   private val traceInfo = Trace.flowInfo(traceActive)(libName)(_: String)(_: String)
  *
  *   private val enter = scala.Option(true)
  *   private val exit  = scala.Option(false)
  *
  * These are then used with a class or object to:
  *   1) mark the entry and exit of function calls
  *   2) trace the flow of data during function execution:
  *
  *   def someFn(p1: String, p2: Int): Unit = {
  *     private val me = "someFn"
  *
  *     trace(me, enter)
  *
  *     // Do stuff...
  *     traceInfo(me, s"Some variable = ${some.value}")
  *
  *     trace(me, exit)
  *   }
  *
  *  In the above example, if the second parameter passed to trace is None, then a double-headed pointer is printed to
  *  indicate a entry and immediate exit from a function.  This is used in situations where a function contains only a
  *  small number of expressions
  *
  *  The flowMsg function is used to indicate that the receive method of an Akka Actor has received a particular message
  *  The output printed to the console uses the standard Erlang syntax for sending a message  "fn ! msg"
  *
  */
object Trace {
  private val enterArrow = "-->"
  private val exitArrow  = "<--"
  private val inOutArrow = "<->"

  def flow(active: Boolean)(lib: String)(fn: String)(isStart: Option[Boolean] = None): Unit = {
    val ptrStr: String = isStart match {
      case Some(b) => if (b) enterArrow else exitArrow
      case None    => inOutArrow
    }

    wtc(active, s"$ptrStr $lib.$fn")
  }

  def flowMsg   (active: Boolean)(lib: String)(fn: String)(msg: String):              Unit = wtc(active, s"    $lib.$fn ! $msg")
  def flowInfo  (active: Boolean)(lib: String)(fn: String)(info: String):             Unit = wtc(active, s"    $lib.$fn: $info")
  def flowValues(active: Boolean)(lib: String)(fn: String)(data: Map[String,String]): Unit = wtc(active, s"    $lib.$fn: ${data.mkString(", ")}")

  //--------------------------------------------------------------------------------------------------------------------
  // Private functions
  // wtc = write to console
  private def wtc(active: Boolean, str: String): Unit = if (active) println(s"${java.time.LocalTime.now()} $str")

}
