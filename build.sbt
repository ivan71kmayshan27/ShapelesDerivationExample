name := "DerivationEqExample"

version := "0.1"

scalaVersion := "2.12.10"
scalacOptions ++= Seq("-Ypartial-unification"/*, "-Xlog-implicits"*/)
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-core" % "2.0.0"
)