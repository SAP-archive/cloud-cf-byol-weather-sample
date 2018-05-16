# Weather Demo: Cloud Foundry's "Bring Your own Language" Concept

## Architecture

The Weather Demo is written in two parts:

* A server-side part that acts as a search engine to supply town/city information
* A client-side part that acts as the user interface for displaying a weather report for any town/city in the world returned from the server

![Solution Diagram](./docs/Weather%20Demo%20Solution%20Diagram.png "Weather Demo Solution Diagram")

### GeoServer

The server-side part of this demo is known as GeoServer.  It has been written in [Erlang](http://www.erlang.org) and deployed to Cloud Foundry using a Cloud Foundry [Community Buildpack](https://github.com/ChrisWhealy/cf-buildpack-erlang)

Detailed documentation for GeoServer can be found [here](./geo_server/README.md).

### Weather Report

The client-side part is written in Scala and compiled to JavaScript using the [Scala.js](https://www.scala-js.org) cross-compiler.  Since the cross-compilation process creates a static JavaScript file, this part of the demo can be deployed to Cloud Foundry using the standard Staticfile buildpack.

Detailed documentation for Weather Report can be found [here](./weather_report/README.md).

## Using Multi-Language Repository

First of all, clone this repository into some suitable directory on your local machine.

    $ git clone https://github.wdf.sap.corp/coolapps/WeatherDemo.git

In your local clone directory, you will now have two subdirectories: `./geo_server` and `./weather_report`.  The former contains the server-side part of the app and the latter, the client-side part.

Since these two parts of the application have been written in different languages and are designed to fulfil different roles, each part has its own specific deployment instructions.  Please refer to the README documents in these respective subdirectories to find the relevant deployment instructions.


Authors
-------

**Chris Whealy**

+ https://github.com/ChrisWhealy


Copyright and License
---------------------

Copyright (c) 2013-2018 SAP SE

Except as provided below, this software is licensed under the Apache License, Version 2.0 (the "License"); you may not use this software except in compliance with the License. You may obtain a copy of the License at:

[http://www.apache.org/licenses/LICENSE-2.0] (http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
