package svg

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

/**
 * Created by markus on 01/11/2014.
 */
object SvgPathConverter {

  val gridSize = 5f
  val lineWidth = 0.5f
  val allowDiagonals = false
  val keepDots = true

  val floatPattern = "-{0,1}[0-9]+(?:\\.[0-9]*)*(?:e-{0,1}[0-9]+)*"
  val floatRegExp = s"($floatPattern)".r
  val pathPartPattern = s"([AaMmLlHhVvCcSsQqTt])(?:[\\s,]*$floatPattern[\\s,]*)+".r

  val world =
//    scala.xml.XML.loadFile( "www/BlankMap-Equirectangular.svg" ) // recommended settings: gridSize ~ 1, lineWidth 0.1
  scala.xml.XML.loadFile( "www/BlankMap-World-alt.svg" ) // recommended settings: gridSize ~ 5, lineWidth 0.5

  def main( args: Array[String] ) {
    val now = System.currentTimeMillis
    scala.xml.XML.save( "www/test_custom.svg", update( pathToLine )( world ) )
    println( System.currentTimeMillis - now )
  }

  def evenListOfFloatsToRoundedPoints( floats: Seq[String] ): Seq[RoundedPoint] = {
    floats.grouped(2).map { case groupOf2 =>
      RoundedPoint(groupOf2(0).toFloat, groupOf2(1).toFloat)
    }.toSeq
  }

  def relativeToAbsolute( points: Seq[RoundedPoint],
                          from: RoundedPoint ): Seq[RoundedPoint] = {
    points.foldLeft( ( Seq.empty[RoundedPoint], from ) ) { case ( ( res, pos ), pt ) =>
      val newPos = RoundedPoint(pt.x + pos.x, pt.y + pos.y)
      ( res :+ newPos, newPos )
    }._1
  }

  def commandToPoints( command: String,
                       floats: Seq[String],
                       position: RoundedPoint ): Seq[RoundedPoint] = command match {
    case "M" | "L" | "T" =>
      if ( floats.size % 2 != 0 ) throw new Error( s"Expected even number of floats after M command, was: $floats" )
      evenListOfFloatsToRoundedPoints( floats )

    case "m" | "l" | "t" =>
      if ( floats.size % 2 != 0 ) throw new Error( s"Expected even number of floats after m command, was: $floats" )
      relativeToAbsolute( evenListOfFloatsToRoundedPoints( floats ),
                          position )

    case "H" =>
      evenListOfFloatsToRoundedPoints( floats.flatMap( f => Seq( f, position.y.toString ) ) )

    case "h" =>
      floats.foldLeft( Seq.empty[RoundedPoint] ) { case ( seq, floatStr ) =>
        if ( seq.isEmpty ) Seq( RoundedPoint( position.x + floatStr.toFloat, position.y ) )
        else seq :+ RoundedPoint( seq.last.x + floatStr.toFloat, seq.last.y )
      }

    case "V" =>
      evenListOfFloatsToRoundedPoints( floats.flatMap( f => Seq( position.x.toString, f ) ) )

    case "v" =>
      floats.foldLeft( Seq.empty[RoundedPoint] ) { case ( seq, floatStr ) =>
        if ( seq.isEmpty ) Seq( RoundedPoint( position.x, position.y + floatStr.toFloat ) )
        else seq :+ RoundedPoint( seq.last.x, seq.last.y + floatStr.toFloat )
      }

    case "S" | "Q" =>
      if ( floats.size % 4 != 0 ) throw new Error( s"Expected groups of 4 floats after S/Q command, was: $floats" )
      val everyThirdAndFourthPaired = floats.grouped( 4 ).map ( groupOf4 => Seq( groupOf4( 2 ), groupOf4( 3 ) ) )
      evenListOfFloatsToRoundedPoints( everyThirdAndFourthPaired.flatten.toSeq )

    case "s" | "q" =>
      if ( floats.size % 4 != 0 ) throw new Error( s"Expected groups of 4 floats after s/q command, was: $floats" )
      val everyThirdAndFourthPaired = floats.grouped( 4 ).map ( groupOf4 => Seq( groupOf4( 2 ), groupOf4( 3 ) ) )
      relativeToAbsolute( evenListOfFloatsToRoundedPoints( everyThirdAndFourthPaired.flatten.toSeq ),
                          position )

    case "C" =>
      if ( floats.size % 6 != 0 ) throw new Error( s"Expected egroups of 6 floats after C command, was: $floats" )
      val everyFifthAndSixthPaired = floats.grouped( 6 ).map ( groupOf6 => Seq( groupOf6( 4 ), groupOf6( 5 ) ) )
      evenListOfFloatsToRoundedPoints( everyFifthAndSixthPaired.flatten.toSeq )

    case "c" =>
      if ( floats.size % 6 != 0 ) throw new Error( s"Expected groups of 6 floats after c command, was: $floats" )
      val everyFifthAndSixthPaired = floats.grouped( 6 ).map ( groupOf6 => Seq( groupOf6( 4 ), groupOf6( 5 ) ) )
      relativeToAbsolute( evenListOfFloatsToRoundedPoints( everyFifthAndSixthPaired.flatten.toSeq ),
                          position )

    case "A" =>
      if ( floats.size % 7 != 0 ) throw new Error( s"Expected egroups of 7 floats after A command, was: $floats" )
      val everySixthAndSeventhPaired = floats.grouped( 7 ).map ( groupOf7 => Seq( groupOf7( 5 ), groupOf7( 6 ) ) )
      evenListOfFloatsToRoundedPoints( everySixthAndSeventhPaired.flatten.toSeq )

    case "a" =>
      if ( floats.size % 7 != 0 ) throw new Error( s"Expected groups of 7 floats after a command, was: $floats" )
      val everySixthAndSeventhPaired = floats.grouped( 7 ).map ( groupOf7 => Seq( groupOf7( 5 ), groupOf7( 6 ) ) )
      relativeToAbsolute( evenListOfFloatsToRoundedPoints( everySixthAndSeventhPaired.flatten.toSeq ),
                          position )
  }

  def parseSinglePath( startPosition: RoundedPoint = RoundedPoint( 0, 0 ) )
                     ( path: String ): Seq[RoundedPoint] = {
    // get each pair of floats in a path part
    pathPartPattern.findAllMatchIn( path ).foldLeft ( Seq.empty[RoundedPoint] ) { case ( seq, m ) =>
      val position = seq.lastOption.getOrElse( startPosition )
      val command = m.group( 1 )
      val floats = floatRegExp.findAllIn( m.group( 0 ) ).toSeq
      try {
        seq ++ commandToPoints(command, floats, position)
      } catch {
        case e: Error =>
          e.printStackTrace()
          println( s"Faulty command: ${m.group( 0 )}" )
          seq
      }
    }
  }

  def alignOnGrid( gridSize: Float )( f: Float ): Float = {
    math.round( f / gridSize ) * gridSize
  }

  // prerequisite: pt is on the grid already
  def neighbours( pt: RoundedPoint ): Set[RoundedPoint] = {
    if ( allowDiagonals ) ( for ( i <- -1 to 1; j <- -1 to 1 if !( j == 0 && i == 0 ) ) yield RoundedPoint( pt.x + i*gridSize, pt.y + j*gridSize ) ).toSet
    else Set( RoundedPoint( pt.x+gridSize, pt.y ), RoundedPoint( pt.x-gridSize, pt.y ), RoundedPoint( pt.x, pt.y+gridSize ), RoundedPoint( pt.x, pt.y-gridSize ) )
  }

  // link a line to a point following grid points
  @tailrec
  def link( line: Seq[RoundedPoint], to: RoundedPoint ): Seq[RoundedPoint] = {
    if ( line.isEmpty || neighbours( to ).contains( line.last ) ) line :+ to
    else {
      val closest = neighbours( line.last ).foldLeft( ( line.last, Float.MaxValue ) ) { case ( ( oldClosest, oldD ), p ) =>
        val dist = ( p.x - to.x ) * ( p.x - to.x ) + ( p.y - to.y ) * ( p.y - to.y )
        if ( oldD < dist ) ( oldClosest, oldD )
        else ( p, dist )
      }._1
      link( line :+ closest, to )
    }
  }

  @tailrec
  def collapseEnds( loop: Seq[RoundedPoint] ): Seq[RoundedPoint] = {
    if ( loop.size < 2 || loop.tail.head != loop( loop.size-2 ) ) loop
    else collapseEnds( loop.tail.take( loop.size-2 ) )
  }

  def snapOnGrid( points: Seq[RoundedPoint] ): Seq[RoundedPoint] = {
    points.map { p => RoundedPoint( alignOnGrid( gridSize )( p.x ), alignOnGrid( gridSize )( p.y ) ) }
  }

  def removeDuplicate( points: Seq[RoundedPoint] ): Seq[RoundedPoint] = {
    points.foldLeft( Seq.empty[RoundedPoint] ) { case ( line, point ) =>
      if ( line.isEmpty || line.last != point ) line :+ point
      else line
    }
  }

  def forceConsecutiveness( points: Seq[RoundedPoint] ): Seq[RoundedPoint] = {
    points.foldLeft( Seq.empty[RoundedPoint] ) ( link )
  }

  def prune( points: Seq[RoundedPoint] ): Seq[RoundedPoint] = {
    // if not already, make the point list loop explicitly
    val loop = if ( points.nonEmpty && points.head != points.last ) points :+ points.head else points
    // find 0 area branches in the loop
    val collapsed = loop.foldLeft( List.empty[RoundedPoint] ) {
      case ( Nil, p ) =>
        List( p )
      case ( h1 :: h2 :: tail, p ) if p != h2 =>
        p :: h1 :: h2 :: tail
      case ( h1 :: h2 :: tail, p ) if p == h2 =>
        h2 :: tail
      case ( l, p)  =>
        p :: l
    }
    // find 0 area branch centered on head/last of the loop
    collapseEnds(collapsed.reverse)
  }

  def parsePath( path: String ): Seq[Seq[RoundedPoint]] = {
    // a path can contained several close subpaths
    val loops = path.split( "[zZ]" ).filter( _.nonEmpty ).toSeq
    val ( lines, dots ) = ( if ( loops.isEmpty ) Seq( path ) else loops )
    .foldLeft( Seq.empty[Seq[RoundedPoint]]) { case ( seq, pathPart ) =>
      // keep track of last position to handle relative path definitions
      val pos = seq.lastOption.flatMap( _.lastOption ).getOrElse( RoundedPoint( 0, 0 ) )
      seq :+ parseSinglePath( pos )( pathPart )
    }
    // align points on grid
    .map ( snapOnGrid )
    // remove duplicate points
    .map ( removeDuplicate )
    // 2 consecutive points in a line must be neighbours on the grid
    .map ( forceConsecutiveness )
    // remove 0 area branches
    .map ( prune )
    .filter( _.nonEmpty )
    .partition( _.size > 1 )

    if (keepDots)
      // if a dot is on a border, remove it
      lines ++ dots.toSet.filter{ d => !lines.exists( _.contains( d( 0 ) ) ) }
    else
      lines
  }

  def createTag( points: Seq[RoundedPoint] ): Node = {
    if ( points.size == 1 )
      <rect x={( points(0).x - gridSize / 4 ).toString} y={( points( 0 ).y - gridSize / 4 ).toString} width={( gridSize / 2 ).toString} height={( gridSize / 2 ).toString} style="fill:none;stroke:black;stroke-width:0.12"/>
    else
      <polyline points={points.map( p => s"${p.x},${p.y}" ).mkString( " " )} style={s"fill:none;stroke:black;stroke-width:${lineWidth}"}/>
  }

  def pathToLine: PartialFunction[Node, Node] = {
    case path @ <path></path> =>
      val lines = parsePath( ( path \ "@d" ).text )
      val attributes = path.attributes.remove( "d" ).remove( "style" )
      if ( lines.size == 1 ) {
        val points = lines( 0 )
        val r = createTag( points )
        <g>{new Elem( r.prefix, r.label, r.attributes.append( attributes ), r.scope, true )}</g>
      } else {
        val child = lines map { points =>
          createTag( points )
        }
        new Elem( path.prefix, "g", attributes, path.scope, true, child: _* )
      }
  }

  def update( fct: PartialFunction[Node, Node] )( node : Node ) : Node = {
    def updateElements( seq : Seq[Node] ) : Seq[Node] =
      for( subNode <- seq ) yield update( fct )( subNode )

    def updateNode( n: Node ): Node = n match {
      case Elem( prefix, label, attribs, scope, children @ _* ) =>
        new Elem( prefix, label, attribs, scope, true, updateElements(children) : _* )
      case other => other  // preserve text
    }

    fct.applyOrElse( node, updateNode )
  }
}
