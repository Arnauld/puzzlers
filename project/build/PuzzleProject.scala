import sbt._

class PuzzleProject(info: ProjectInfo) extends DefaultProject(info) {

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * dependencies
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  object Version {
    val log4j = "1.2.15"
    val slf4j = "1.6.1"
    val scala = "2.8.1"
    //
    val junit = "4.8.1"
    val scala_specs = "1.6.6"
    val mockito = "1.8.5"
  }
  // Use ivy directly to use exclude behavior
  override def ivyXML =
    <dependencies>
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- Test -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="junit" name="junit" rev={Version.junit} conf="test->default"/>
        <dependency org="org.scala-tools.testing" name={"specs_" + Version.scala} rev={Version.scala_specs} conf="test->default"/>
        <dependency org="org.mockito" name="mockito-all" rev={Version.mockito}/>

      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
      <!-- logs -->
      <!-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -->
        <dependency org="org.slf4j" name="slf4j-api" rev={Version.slf4j}/>
        <dependency org="org.slf4j" name="slf4j-log4j12" rev={Version.slf4j}/>
      <dependency org="log4j" name="log4j" rev={Version.log4j}>
          <exclude name="mail"/>
          <exclude name="jms"/>
          <exclude name="jmxtools"/>
          <exclude name="jmxri"/>
      </dependency>
    </dependencies>
}
