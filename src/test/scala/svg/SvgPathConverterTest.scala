package svg

/**
 * Created by calim on 04.11.14.
 */
object SvgPathConverterTest {
  val input =
    """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
      |<svg>
      |<path d="M0 0 0 1 1 1H 2 3 4 V 2 3 4 S1 1 5 5C 1 1 1 1 6 6 Z"></path>
      |<path d="m 0 0 0 1 1 1 h1 1 1 v 1 1 1s 1 1 1 1 c1 1 1 1 1 1z"></path>
      |<path d="M0 0 0 1 1 1H 2 3 4 V 2 3 4 S1 1 5 5C 1 1 1 1 6 6 Zm 0 0 0 1 1 1 h1 1 1 v 1 1 1s 1 1 1 1 c1 1 1 1 1 1z"></path>
      |<path d="m 0 0 0 1 1 1 h1 1 1 v 1 1 1s 1 1 1 1 c1 1 1 1 1 1zM0 0 0 1 1 1H 2 3 4 V 2 3 4 S1 1 5 5C 1 1 1 1 6 6 Z"></path>
      |<path d="m 0 0 0 1 1 1 h1 1 1 v 1 1 1s 1 1 1 1 c1 1 1 1 1 1zm 0 0 0 1 1 1 h1 1 1 v 1 1 1s 1 1 1 1 c1 1 1 1 1 1z"></path>
      |<path d="M0 0 0 1 1 1H 2 3 4 V 2 3 4 S1 1 5 5C 1 1 1 1 6 6 ZM0 0 0 1 1 1H 2 3 4 V 2 3 4 S1 1 5 5C 1 1 1 1 6 6 Z"></path>
      |</svg>""".stripMargin

  val expected =
    """<svg>
      |<g><polyline points="0.0,0.0 0.0,1.0 1.0,1.0 2.0,1.0 3.0,1.0 4.0,1.0 4.0,2.0 4.0,3.0 4.0,4.0 5.0,5.0 6.0,6.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/></g>
      |<g><polyline points="0.0,0.0 0.0,1.0 1.0,2.0 2.0,2.0 3.0,2.0 4.0,2.0 4.0,3.0 4.0,4.0 4.0,5.0 5.0,6.0 6.0,7.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/></g>
      |<g><polyline points="0.0,0.0 0.0,1.0 1.0,1.0 2.0,1.0 3.0,1.0 4.0,1.0 4.0,2.0 4.0,3.0 4.0,4.0 5.0,5.0 6.0,6.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/><polyline points="6.0,6.0 6.0,7.0 7.0,8.0 8.0,8.0 9.0,8.0 10.0,8.0 10.0,9.0 10.0,10.0 10.0,11.0 11.0,12.0 12.0,13.0 6.0,6.0" style="fill:none;stroke:black;stroke-width:0.1"/></g>
      |<g><polyline points="0.0,0.0 0.0,1.0 1.0,2.0 2.0,2.0 3.0,2.0 4.0,2.0 4.0,3.0 4.0,4.0 4.0,5.0 5.0,6.0 6.0,7.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/><polyline points="0.0,0.0 0.0,1.0 1.0,1.0 2.0,1.0 3.0,1.0 4.0,1.0 4.0,2.0 4.0,3.0 4.0,4.0 5.0,5.0 6.0,6.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/></g>
      |<g><polyline points="0.0,0.0 0.0,1.0 1.0,2.0 2.0,2.0 3.0,2.0 4.0,2.0 4.0,3.0 4.0,4.0 4.0,5.0 5.0,6.0 6.0,7.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/><polyline points="6.0,7.0 6.0,8.0 7.0,9.0 8.0,9.0 9.0,9.0 10.0,9.0 10.0,10.0 10.0,11.0 10.0,12.0 11.0,13.0 12.0,14.0 6.0,7.0" style="fill:none;stroke:black;stroke-width:0.1"/></g>
      |<g><polyline points="0.0,0.0 0.0,1.0 1.0,1.0 2.0,1.0 3.0,1.0 4.0,1.0 4.0,2.0 4.0,3.0 4.0,4.0 5.0,5.0 6.0,6.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/><polyline points="0.0,0.0 0.0,1.0 1.0,1.0 2.0,1.0 3.0,1.0 4.0,1.0 4.0,2.0 4.0,3.0 4.0,4.0 5.0,5.0 6.0,6.0 0.0,0.0" style="fill:none;stroke:black;stroke-width:0.1"/></g>
      |</svg>""".stripMargin
}

import SvgPathConverterTest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class SvgPathConverterTest extends FlatSpec with Matchers {
  "The converter" should "handle all commands in a path" in {
    val output = SvgPathConverter( 1f, allowDiagonals = true ).update( scala.xml.XML.loadString( input ) ).toString()
    output should be ( expected )
  }

  "Linking points" should "be anisotropic" in {
    val converter1 = SvgPathConverter( 1f, allowDiagonals = false )
    val A = RoundedPoint( 0.0f, 0.0f )
    val B = RoundedPoint( 1.0f, 1.0f )
    val C = RoundedPoint( -1.0f, 1.0f )
    converter1.link( Seq( A ), B ) should be ( converter1.link( Seq( B ), A ).reverse )
    converter1.link( Seq( A ), C ) should be ( converter1.link( Seq( C ), A ).reverse )
  }
}
