package com.sap.scala.demo

import scala.scalajs.js
import java.lang.{Long => JLong}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Class initialiser extracts weather data from JSON response
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
class WeatherReportBuilder(data: js.Dynamic) {
  private val className = "WeatherReportBuilder"
  private val fnName    = "constructor"

  private val traceActive = false
  private val trace       = Trace.flow(traceActive)(className)(fnName)(_: scala.Option[Boolean])
  private val traceInfo   = Trace.flowInfo(traceActive)(className)(fnName)(_: String)

  private val enter = scala.Option(true)
  private val exit  = scala.Option(false)

  trace(enter)

  // From the weather JSON object obtains:
  // - City coordinates
  // - Basic atmospheric conditions:
  // - Wind speed and optional direction (given as a heading in degrees)
  // - Country, sunrise and sunset times.
  val coord      = new Coord(data.coord)
  traceInfo(s"Coordinates         = (${coord.lat},${coord.lng})")

  val main       = new WeatherMain(data.main)
  traceInfo(s"Humidity            = ${main.humidity}")
  traceInfo(s"Temperature         = ${main.temp}")
  traceInfo(s"Temperature (max)   = ${main.temp_max}")
  traceInfo(s"Temperature (min)   = ${main.temp_min}")
  traceInfo(s"Air pressure        = ${main.airPressure}")
  traceInfo(s"Air pressure (sea)  = ${main.sea_level}")
  traceInfo(s"Air pressure (grnd) = ${main.grnd_level}")

  val wind       = new Wind(data.wind)
  traceInfo(s"Wind heading        = ${wind.heading}")
  traceInfo(s"Wind speed          = ${wind.speed}")

  val weatherSys = new WeatherSys(data.sys)
  traceInfo(s"Country             = ${weatherSys.country}")
  traceInfo(s"Sunrise             = ${weatherSys.sunrise}")
  traceInfo(s"Sunset              = ${weatherSys.sunset}")

  // Multiple weather conditions might be supplied for one location
  val weatherConditions = data.weather.map {
    (weatherItem: js.Dynamic) => new WeatherCond(weatherItem)
  }.asInstanceOf[js.Array[WeatherCond]]
    .toSeq

  weatherConditions.map {
    (c: WeatherCond) => {
      traceInfo(s"Description         = ${c.desc}")
      traceInfo(s"Icon                = ${c.icon}")
      traceInfo(s"Main                = ${c.main}")
      traceInfo(s"Weather id          = ${c.weatherId}")
    }
  }

  // Optional visibility information. If this value is not available, assume maximum visibility
  val visibility = if (data.visibility.toString == "undefined") 10000 else data.visibility.asInstanceOf[Int]
  traceInfo(s"Visibility          = $visibility")

  // Percentage cloud cover
  val clouds = data.clouds.all.asInstanceOf[Int]
  traceInfo(s"Cloud cover         = $clouds")

  // Optional 3 hour rain and snow fall information
  val rain = if (data.rain == null || data.rain.toString == "undefined") 0.0 else data.rain.`3h`.asInstanceOf[Double]
  val snow = if (data.snow == null || data.snow.toString == "undefined") 0.0 else data.snow.`3h`.asInstanceOf[Double]
  traceInfo(s"Rain                = $rain")
  traceInfo(s"Snow                = $snow")

  // UTC timestamp showing when measurements were taken
  // City id, name and HTTP response code
  val measuredAt = JLong.parseLong(data.dt.toString)
  val cityId     = data.id.asInstanceOf[Int]
  val cityName   = data.name.asInstanceOf[String]
  traceInfo(s"measuredAt          = $measuredAt")
  traceInfo(s"cityId              = $cityId")
  traceInfo(s"cityName            = $cityName")

  val returnCode = if (data.cod.toString == "undefined") -1 else data.cod.asInstanceOf[Int]
  traceInfo(s"Return code         = $returnCode")

  trace(exit)
}
