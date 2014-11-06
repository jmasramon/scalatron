import org.scalatest.matchers.{ShouldMatchers, Matchers}
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import scala.Array

/**
 * Created by jmasramon on 05/11/14.
 */
class ControlFunction$Test extends FlatSpec  with BeforeAndAfterAll with ShouldMatchers{
  val (directionValue, nearestEnemyMaster, nearestEnemySlave) = ControlFunction.analyzeViewAsMaster(View("WWWWWWWW_b_S_WW___B_WWp_M__WW__s__WW__m_PWWWWWWWW"))

  //  WWWWWWW  (0,0) .. (6,0)
  //  W_b_S_W   b -200  S 0
  //  W___B_W           B 600
  //  Wp_M__W   p 0 (3,3)
  //  W__s__W      s -100
  //  W__m_PW      m 0  P 300
  //  WWWWWWW  (0,6) .. (6,6)

  // directionValue === Array(0.0, 600.0, 0.0, -200.0, 0.0, 0.0, -100.0, 300.0)
  //  -200   0  600
  //     0   M  0
  //     0 -100 300



  "A Control function object" should "analyze a view as a master to decide what to do" in {
    expect(8)(directionValue.length)
    directionValue should have length (8)
    //directionValue should be === Array(0.0, 600.0, 0.0, -200.0, 0.0, 0.0, -100.0, 300.0)
    //directionValue should be === Array(0.0, 600.0, 0.0, -200.0, 0.0, 0.0, -2500.0, 300.0)
    directionValue should be === Array(0.0, 600.0, 0.0, -200.0, 0.0, 0.0, -1250.0, 250.0)
    expect(Some(XY(0,1)))(nearestEnemySlave)
    expect(Some(XY(0,2)))(nearestEnemyMaster)
  }



}
