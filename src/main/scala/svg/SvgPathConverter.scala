package svg

import scala.annotation.tailrec
import scala.xml.{Elem, Node}

/**
 * Created by markus on 01/11/2014.
 */
object SvgPathConverter {

  val gridSize = 1f
  val allowDiagonals = true
  val keepDots = true

  val floatSepPattern = "[\\s,]+"
  val floatsPattern = s"(-{0,1}[0-9]+(?:\\.[0-9]*)$floatSepPattern-{0,1}[0-9]+(?:\\.[0-9]*))".r

  val world =
    scala.xml.XML.loadFile( "www/BlankMap-Equirectangular.svg" )
//  scala.xml.XML.loadFile( "www/BlankMap-World-alt.svg" )
//  scala.xml.XML.loadFile( "www/test.svg" )

  def main( args: Array[String] ) {
    val now = System.currentTimeMillis
    scala.xml.XML.save( "www/test_custom.svg", update( pathToLine )( world ) )
    println( System.currentTimeMillis - now )
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
    if ( line.isEmpty || neighbours( to ).contains( line.head ) ) to +: line
    else {
      val closest = neighbours( line.head ).foldLeft( ( line.head, Float.MaxValue ) ) { case ( ( oldClosest, oldD ), p ) =>
        val dist = ( p.x - to.x ) * ( p.x - to.x ) + ( p.y - to.y ) * ( p.y - to.y )
        if ( oldD < dist ) ( oldClosest, oldD )
        else ( p, dist )
      }._1
      link( closest +: line, to )
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
      if ( line.isEmpty || line.head != point ) point +: line
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
    collapseEnds(collapsed)
  }

  def parseSinglePath( path: String ): Seq[RoundedPoint] = {
    // get each pair of floats in a path part
    floatsPattern.findAllMatchIn( path ).map { m =>

      // parse them and fit them onto a grid
      val floats = m.group( 1 ).split( floatSepPattern ).map( _.toFloat ) // .map( alignOnGrid( gridSize ) )
      RoundedPoint( floats( 0 ), floats( 1 ) )
    }.toList
  }

  def parsePath( path: String ): Seq[Seq[RoundedPoint]] = {
    // a path can contained several close subpaths
    val loops = path.split( "[zZ][mM]" ).toSeq
    val ( lines, dots ) = (if ( loops.isEmpty ) Seq( path ) else loops )
    .map ( parseSinglePath )
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
      <polyline points={points.map( p => s"${p.x},${p.y}" ).mkString( " " )} style="fill:none;stroke:black;stroke-width:0.1"/>
  }

  def pathToLine: PartialFunction[Node, Node] = {
    case path @ <path></path> =>
      val lines = parsePath( ( path \ "@d" ).text )
      val attributes = path.attributes.remove( "d" )
      if ( lines.size == 1 ) {
        val points = lines( 0 )
        val r = createTag( points )
        <g>{new Elem( r.prefix, r.label, r.attributes.append( attributes ), r.scope, true )}</g>
      } else {
        val child = lines map { points =>
          createTag( points )
        }
        new Elem( path.prefix, "g", path.attributes.remove("d"), path.scope, true, child: _* )
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
