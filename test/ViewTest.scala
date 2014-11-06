import org.scalatest._
import org.scalatest.matchers._

/**
 * Created by jmasramon on 05/11/14.
 *
 * View just transposes from linear string format (index) to matrix format (x,y)
 */
class ViewTest extends FlatSpec with BeforeAndAfterAll with Matchers{
  val view = View("WWWWWWWW_____WW_____WW__M__WW_____WW____PWWWWWWWW")
//  WWWWWWW  (0,0) .. (6,0)
//  W_____W
//  W_____W
//  W__M__W       (3,3)
//  W_____W
//  W____PW
//  WWWWWWW  (0,6) .. (6,6)

  "A View object" should "initialize from an view string" in {
    expect(7)(view.size)
    expect(XY(3,3))(view.center)
   }

  it should "get the right index from absolute position" in {
    expect(0)(view.indexFromAbsPos(XY(0,0)))
    expect(6)(view.indexFromAbsPos(XY(6,0)))
    expect(8)(view.indexFromAbsPos(XY(1,1)))
    expect(24)(view.indexFromAbsPos(XY(3,3)))
    expect(42)(view.indexFromAbsPos(XY(0,6)))
    expect(48)(view.indexFromAbsPos(XY(6,6)))
  }

  it should "get the right cell content from absolute position" in {
    expect('W')(view.cellAtAbsPos(XY(0,0)))
    expect('W')(view.cellAtAbsPos(XY(6,0)))
    expect('_')(view.cellAtAbsPos(XY(1,1)))
    expect('M')(view.cellAtAbsPos(XY(3,3)))
    expect('W')(view.cellAtAbsPos(XY(0,6)))
    expect('W')(view.cellAtAbsPos(XY(6,6)))
  }

  it should "get the right index from relative position" in {
    expect(0)(view.indexFromRelPos(XY(-3,-3)))
    expect(6)(view.indexFromRelPos(XY(3,-3)))
    expect(24)(view.indexFromRelPos(XY(0,0)))
    expect(42)(view.indexFromRelPos(XY(-3,3)))
    expect(48)(view.indexFromRelPos(XY(3,3)))
  }

  it should "get the right cell content from relative position" in {
    expect('W')(view.cellAtRelPos(XY(-3,-3)))
    expect('W')(view.cellAtRelPos(XY(3,-3)))
    expect('M')(view.cellAtRelPos(XY(0,0)))
    expect('W')(view.cellAtRelPos(XY(-3,3)))
    expect('W')(view.cellAtRelPos(XY(3,3)))
  }

  it should "give the offset to nearest element searched" in {
    //expect(Some("2:2"))(view.offsetToNearest('P'))
    view.offsetToNearest('P') match {
      case Some(delta: XY) =>
        expect(2)(delta.x)
        expect(2)(delta.y)
      case None =>
        fail()
    }
    expect(None)(view.offsetToNearest('H'))

    view.offsetToNearest('W') match {
      case Some(delta: XY) =>
        expect(0)(delta.x)
        expect(-3)(delta.y)
      case None =>
        fail()
    }

  }

}
