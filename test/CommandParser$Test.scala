import org.scalatest._
import org.scalatest.matchers._

/**
 * Created by jmasramon on 05/11/14.
 */
class CommandParser$Test extends FlatSpec with Matchers{

  "A CommandParser object" should "parse properly with default constructor" in {
    val (opcode, keyValuePairs) = CommandParser("React(generation=0,time=0,view=__W_W_W__,energy=100)")
    expect("React")(opcode)
    expect("0")(keyValuePairs("generation"))
    expect("0")(keyValuePairs("time"))
    expect("__W_W_W__")(keyValuePairs("view"))
    expect("100")(keyValuePairs("energy"))
  }


}
