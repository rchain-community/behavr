## Building from source

We follow sbt conventions.
[IntelliJ IDEA][1] from Jetbrains works as a build tool.

[1]: https://www.jetbrains.com/idea/download/?fromIDE=#section=linux

### Testing

ref [Testing Scala in IntelliJ with ScalaTest ](https://docs.scala-lang.org/getting-started-intellij-track/testing-scala-in-intellij-with-scalatest.html)

### Adding a library

_probably old hat to most IDE users, but..._

Alt-Enter, "Add sbt dependency"

ref [Adding a library to the sbt project][al]

[al]: https://www.jetbrains.com/help/idea/sbt-support.html#add_library_scala_file

### Keyboard locks up

widely reported. known issue. work-around: restart ibus
https://youtrack.jetbrains.com/issue/IDEA-78860

### Road not taken: gradle

I tried the [scala plugin for gradle][sg] but ran into
enough rough edges to conclude that it's not a well-trodden path
and go back to sbt.

[sg]: https://docs.gradle.org/current/userguide/scala_plugin.html


## Translating algebraic datatypes from ocaml to Scala

See [Introduction to Algebraic Types in Scala][2] by Rob Norris / @tpolecat.

[2]: http://tpolecat.github.io/presentations/algebraic_types.html
