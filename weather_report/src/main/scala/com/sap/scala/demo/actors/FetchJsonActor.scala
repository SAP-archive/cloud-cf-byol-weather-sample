package com.sap.scala.demo.actors

import akka.actor.Actor
import com.sap.scala.demo.{MessageBox, Trace, Utils}

/***********************************************************************************************************************
  * Actor to handle fetching some JSON data from the server
  */
class FetchJsonActor extends Actor {
  private val className = "FetchJsonActor"
  private val fnName    = "receive"

  private val traceActive = false
  private val traceMsg    = Trace.flowMsg(traceActive)(className)(fnName)(_: String)

  /*********************************************************************************************************************
    * Wait for messages
    */
  def receive = {
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Fetch a JSON resource from some backend server and return the data to the designated response handler
    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    case MessageBox.FetchJsonMsg(responseHandler, url) =>
      traceMsg(s"FetchJsonMsg('$url')")

      val req = Utils.buildXhrRequest(url)

      req.onload = Utils.jsonResponseHandler(responseHandler, req)
      req.send()
  }
}
