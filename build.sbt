val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mads",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.scalameta" %% "munit" % "0.7.26" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test
    )
  )
