package com.sap.scala.demo

import com.felstar.scalajs.leaflet.L
import scala.scalajs.js

class GeoBox(geoBox: js.Dynamic) {
  val north = geoBox.North.asInstanceOf[Double]
  val south = geoBox.South.asInstanceOf[Double]
  val east  = geoBox.East.asInstanceOf[Double]
  val west  = geoBox.West.asInstanceOf[Double]

  val centre = (north - (north - south)/2, east - (east - west)/2)
  val center = centre

  val bounds = L.latLngBounds(L.latLng(north, east), L.latLng(south, west))
}
