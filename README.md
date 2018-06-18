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

# Cloud Foundry's "Bring Your Own Language" Concept

Java is most certainly one of the most popular development languages at the moment; however, Cloud Foundry offers developers the opportunity to create their own applications using almost ***any*** programming language.

The only factors constraining your choice of language are:

* Has your chosen language been implemented to run on Windows or Linux?
* Does your chosen language have a Cloud Foundry buildpack?

If the answer to the first question is "No", then unfortunately all bets are off and you must choose a different language.

If the answer to the second question is "No", then although you might have to do a little more work, this is certainly not a show-stopper.

## Cloud Foundry Buildpacks

Before developing an application on your local machine, you must first install the set of tools appropriate for compiling your source code and creating an executable application.  A buildpack is nothing more than a set of shell scripts that repeat this process in the Cloud Foundry runtime environment in order to compile and execute your application.


### Buildpacks Built-in To Cloud Foundry

Cloud Foundry provides built-in support for the following languages:

* [Java](https://github.com/cloudfoundry/java-buildpack)
* [Ruby](https://github.com/cloudfoundry/ruby-buildpack)
* [PHP](https://github.com/cloudfoundry/php-buildpack)
* [Go](https://github.com/cloudfoundry/go-buildpack)
* [Python](https://github.com/cloudfoundry/python-buildpack)

and runtime environments:

* [NodeJS (Server-side JavaScript)](https://github.com/cloudfoundry/nodejs-buildpack)
* [Web servers delivering static content (Via an NGINX Web server)](https://github.com/cloudfoundry/staticfile-buildpack)
* [Arbitrary binary Web servers](https://github.com/cloudfoundry/binary-buildpack)
* [.NET Core Applications](https://github.com/cloudfoundry/dotnet-core-buildpack)

### Community-Developed Buildpacks

If however, your favourite language is not listed here, then there is a range of community developed buildpacks from which to choose.  These include buildpacks for languages such as:

* [Clojure](https://github.com/mstine/heroku-buildpack-clojure)
* [Scala](https://github.com/heroku/heroku-buildpack-scala)
* [Haskell](https://github.com/BrianMMcClain/heroku-buildpack-haskell)
* [Erlang](https://github.com/ChrisWhealy/cf-buildpack-erlang)
* [Elixir](https://github.com/HashNuke/heroku-buildpack-elixir)
* [Rust](https://github.com/emk/heroku-buildpack-rust)
* [Swift](https://github.com/cloudfoundry-community/swift-buildpack)

There is even a [Generic Language](https://github.com/oetiker/sourcey-buildpack) buildpack that, using a system of plugins, can compile and build a wide range of languages.

The full list of community-developed buildpacks is listed on the [Cloud Foundry Community Buildpack](https://github.com/cloudfoundry-community/cf-docs-contrib/wiki/Buildpacks) Wiki on GitHub.com.

### Do-It-Yourself Buildpacks

You might however find yourself in the situation that the existing community buildpack for your chosen language has become obsolete or is broken.

This is the situation I encountered when writing in Erlang.  The existing community buildpack had not been updated in about 4 years, and was no longer able build my app using the latest version of Erlang's build tool.

Nonetheless, by following Cloud Foundry's [buildpack documentation](https://docs.cloudfoundry.org/buildpacks/understand-buildpacks.html), I was able adapt the obsolete buildpack quite easily in order to get it back into working order.

My [Erlang buildpack](https://github.com/ChrisWhealy/cf-buildpack-erlang) is now listed on the [Cloud Foundry Community Buildpack page](https://github.com/cloudfoundry-community/cf-docs-contrib/wiki/Buildpacks).




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
