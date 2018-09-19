<!-- *********************************************************************** -->
<a name="cf_buildpacks"></a>
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

This is the situation I encountered when writing in Erlang.  The existing community buildpack had not been updated in about 4 years, and was no longer able build my app using the latest version of Erlang's build tool (Rebar3).

Nonetheless, by following Cloud Foundry's [buildpack documentation](https://docs.cloudfoundry.org/buildpacks/understand-buildpacks.html), I was able adapt the obsolete buildpack quite easily in order to get it back into working order.

My [Erlang buildpack](https://github.com/ChrisWhealy/cf-buildpack-erlang) is now listed on the [Cloud Foundry Community Buildpack page](https://github.com/cloudfoundry-community/cf-docs-contrib/wiki/Buildpacks).
