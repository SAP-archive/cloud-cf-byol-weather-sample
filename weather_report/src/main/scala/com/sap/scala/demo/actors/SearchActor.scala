package com.sap.scala.demo.actors

import akka.actor.Actor

import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.html.Input

import scala.scalajs.js.timers.{SetTimeoutHandle, clearTimeout, setTimeout}
import com.sap.scala.demo._
import java.time.Instant



/***********************************************************************************************************************
  * Actor to handle the input field and associated datalist of countries
  */
class SearchActor extends Actor {
  private val className = "SearchActor"
  private val fnName    = "receive"

  private val traceActive = true
  private val traceMsg    = Trace.flowMsg(traceActive)(className)(fnName)(_: String)

  private var prevEvt: Instant = null
  private var thisEvt: Instant = null

  /*********************************************************************************************************************
    * Wait for messages
    */
  def receive = {
    /*******************************************************************************************************************
      * Initialise actor - Create search input fields
      */
    case MessageBox.Initialise =>
      traceMsg("Initialise")
      DOMUtils.buildSearchInputFields(self)

    /*******************************************************************************************************************
      * Client event
      * The user has typed something into the search field
      */
    case evt: Event =>
      traceMsg(s"dom.Event ${evt.`type`} detected from ${evt.target.toLocaleString}")

      delay()(handleInput, 1000)
  }

  /*********************************************************************************************************************
    * Send user input to backend
    */
  private def handleInput(): Unit = {
    traceMsg("handleInput")

    val thisSearchStr = DOMUtils.elementById[Input](MessageBox.searchInput).value

    // What are the search parameters?
    // We don't actually care about the value of the "contains" radio button, because its a Boolean that always has
    // the opposite value of the "starts_with" radio button
    var whole_word  = DOMUtils.elementById[Input](MessageBox.searchCheckBox).checked
    var starts_with = DOMUtils.elementById[Input](MessageBox.searchRadio1).checked

    // Has the user entered more than 2 characters?
    if (thisSearchStr != null &&
        thisSearchStr != StageManager.placeholder &&
        thisSearchStr.size > 2) {
      // Yup, so invoke search
      StageManager.fetchJsonActor ! MessageBox.FetchJsonMsg(
        StageManager.searchResultsActor,
        Utils.buildSearchUrl(thisSearchStr, whole_word, starts_with)
      )
    }
    // Nope, so did the user blank out the country input field?
    else if (thisSearchStr.size == 0 ||
             thisSearchStr == StageManager.placeholder) {
      // Yup, so reset the map back to the world view and clear the old weather report
      StageManager.searchResultsActor ! MessageBox.ClearList
      StageManager.weatherReportActor ! MessageBox.ClearList
      StageManager.mapActor           ! MessageBox.RepositionMapMsg("0.0", "0.0")
    }
  }

  private def delay = () => {
    var handle: SetTimeoutHandle = setTimeout(0) {}

    (fn: Function0[Unit], ms: Double) => {
      traceMsg(s"Inner function. Handle = ${handle}")
      clearTimeout(handle)
      handle = setTimeout(ms) {
        fn()
      }
    }
  }
}
