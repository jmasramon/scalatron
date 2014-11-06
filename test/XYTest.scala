import org.scalatest._



/**
 * Created by jmasramon on 05/11/14.
 */
class XYTest extends FlatSpec with BeforeAndAfterAll{
  val xy = XY(2, 3)

  override def beforeAll(){
    println("Hola lola")
  }

//  afterAll() {
//  }

  ignore should "wordOccurrences: abcd" in {
    assert(true === true)
  }


  "A XY object" should "initialize properly with default constructor" in {
    val xy = XY(2, 3)
    assert(xy.x == 2)
    assert(xy.y == 3)
  }

  it should "initialize properly with string constructor" in {
    val xy = XY("4:5")
    assert(xy.x == 4)
    assert(xy.y == 5)
  }

  it should "give proper directions" in {
    assert(XY.fromDirection45(Direction45.Right) == XY.Right)
    assert(XY.fromDirection90(Direction90.Up) == XY.Up)
    assert(XY.Up == XY(0,-1))
  }

  it should "identify properly some special positons" in {
    assert(xy.isNonNegative)
    assert(xy.isNonZero)
    val xyZero = XY(0,0)
    assert(xyZero.isZero)
  }

  it should "add to each axis" in {
    //assert(xy.addToX(1) == XY(2,2))
    expect(XY(3,3))(xy.addToX(1))
    expect(XY(2,4))(xy.addToY(1))

  }

  it should "operate correctly" in {
    expect(XY(3,4))(xy + XY(1,1))
    expect(XY(0,0))(xy - XY(2,3))
    expect(XY(4,6))(xy * 2)
  }

  it should "give the proper length (vector module)" in {
    expect(3.605551275463989)(xy.length)
  }

  it should "calculate the proper distance " in {
    expect(2.23606797749979)(xy.distanceTo(XY(1,1)))
  }

  it should "calculate the step distance (number of movs) to  0,0 (relative position of master)" in {
    expect(3)(xy.stepCount)
    expect(xy.stepCount)(xy.stepsTo(XY(0,0)))
  }

  it should "give the orientation of a vector as a signum" in {
    expect(XY(1,1))(xy.signum)
    expect(XY(-1,1))(XY(-2,3).signum)
    expect(XY(1,-1))(XY(3,-4).signum)
    expect(XY(-1,-1))(XY(-1,-7).signum)
    expect(XY(1,1))(XY(5,2).signum)
    expect(XY(0,1))(XY(0,6).signum)
    expect(XY(1,0))(XY(4,0).signum)
    expect(XY(0,-1))(XY(0,-8).signum)
    expect(XY(-1,0))(XY(-9,0).signum)
  }

  it should "map each signum to a 45 direction" in {
    expect(Direction45.DownRight)(XY(1,1).toDirection45)
    expect(Direction45.LeftDown)(XY(-1,1).toDirection45)
    expect(Direction45.RightUp)(XY(1,-1).toDirection45)
    expect(Direction45.UpLeft)(XY(-1,-1).toDirection45)
    expect(Direction45.Down)(XY(0,1).toDirection45)
    expect(Direction45.Right)(XY(1,0).toDirection45)
    expect(Direction45.Up)(XY(0,-1).toDirection45)
    expect(Direction45.Left)(XY(-1,0).toDirection45)
  }

  it should "know how to retate a direction45 clockwise" in {
    expect(Direction45.RightUp)(XY.rotateClockwise(Direction45.Right))
    expect(Direction45.Right)(XY.rotateClockwise(Direction45.DownRight))
  }

}
